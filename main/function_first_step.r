library(dplyr); library(foreach); library(bspec); library(reticulate); library(signal)
score <- function(s_file, s_c3a2, s_loc, s_roc, s_emg) {
  # load header
  rec_header <- edfReader::readEdfHeader(s_file)
  
  # change epoch length (in seconds) if necessary
  epochl <- 20
  # change sliding windows length used for computing power spectra
  windowl <- 5
  
  # sampling rate for main derivation
  fs <- rec_header$sHeaders$sRate[rec_header$sHeaders$label==s_c3a2]
  # downsampling to 128 Hz. dn is downsampling factor
  dn <- fs %/% 128
  
  # read corresponding EDF file
  eeg_recording <- rec_header %>% edfReader::readEdfSignals()
  
  # Downsampling to frequency close to 128Hz
  eeg_downsampled <- foreach(c=c(s_c3a2, s_loc, s_roc, s_emg), .combine=cbind) %do% decimate(eeg_recording[[c]][['signal']], dn) %>% 
  as_tibble() %>% rename_all(~c(s_c3a2, s_loc, s_roc, s_emg)) %>% mutate_all(round,3)
#-------------------------------------------------------------------------------------------------------------
# notch filter function
notch_f <-  function(x) {
  notch_buttf <- signal::butter(2, W=c(49/64, 51/64), type="stop") # filtering out 50HZ signal with a stopband butterworth filter first order
  signal::filter(notch_buttf, x)
}
# high pass filter function
high_passf <-  function(x) {
  high_buttf <- signal::butter(1, W=2/64, type="high") # filtering out 50HZ signal with a stopband butterworth filter first order. Check filter with freqz(buttf)
  signal::filter(high_buttf, x)
}
# welch power spectral density function using hanning sliding window (length set by windowl)
pwelch_spectra <- function(c3a2_signal, window_length, downsampled_frequency) {
  t_welch <- welchPSD(ts(c3a2_signal, frequency = downsampled_frequency), windowfun = hannwindow, seglength = window_length)
  t_welch$power
}
#-------------------------------------------------------------------------------------------------------------
# apply filters
eeg_downsampled_filtered <- eeg_downsampled %>% # choose and apply filters
  mutate(across(everything(), notch_f)) # %>% mutate(across(C3_A2:C4_A1, high_passf))

# computing numbers of epochs
n_epochs <- floor((nrow(eeg_downsampled_filtered) / (fs/dn)) / epochl)

eeg_nested <- eeg_downsampled_filtered[1:(n_epochs*epochl*(fs/dn)),] %>%
  mutate(epoch = rep(1:n_epochs, each = (fs/dn) * epochl)) %>% 
  tidyr::nest(data = -epoch) %>%
  mutate(epoch = glue::glue('epoch_{epoch}')) %>%
  tidyr::unnest_wider(data)

# extracting C3:A2 signal to compute spectra
c3_a2_wider <- eeg_nested %>%
  dplyr::select(epoch, all_of(s_c3a2)) %>% 
  tidyr::pivot_wider(names_from = epoch, values_from = all_of(s_c3a2)) %>%
  summarize(across(starts_with('epoch'), unlist))

# add frequency column
t_fs <- welchPSD(ts(c3_a2_wider$epoch_1, frequency = (fs/dn)), windowfun = hannwindow, seglength = windowl)

# computing spectra in parallel
c3_a2_spectra_wider <- c3_a2_wider %>% 
  summarize(across(starts_with('epoch'), ~pwelch_spectra(., windowl, (fs/dn)))) %>% bind_cols('freq'=t_fs$frequency) 
c3_a2_spectra <- c3_a2_spectra_wider %>%
  tidyr::pivot_longer(!freq, names_to='epoch', values_to = 'power') %>%
  mutate(epoch = readr::parse_number(epoch))

#  compute PEMG 
emg_wider <- eeg_nested %>%
  dplyr::select(epoch, all_of(s_emg)) %>%
  tidyr::pivot_wider(names_from = epoch, values_from = all_of(s_emg)) %>%
  summarize(across(starts_with('epoch'), unlist))

emg_spectra <- emg_wider %>% summarize(across(starts_with('epoch'), ~pwelch_spectra(., windowl, (fs/dn)))) %>% bind_cols('freq'=t_fs$frequency) %>%
  tidyr::pivot_longer(!freq, names_to='epoch', values_to = 'power') %>%
  mutate(epoch = readr::parse_number(epoch))

PEMG <- emg_spectra %>% dplyr::filter(freq>15, freq<30) %>% dplyr::select(-freq) %>%
  group_by(epoch) %>%
  mutate(power = log(power+1), power = power-quantile(power, probs = 0.1), 
         power = runmed(power, 5), power = power/mean(power[1:10]), power = if_else(power>1, 1, power), 
         power = if_else(power < -1, -1, power), power = power/max(power)) %>%
  ungroup() %>%
  group_by(epoch) %>%
  summarize(pemg = mean(power))
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# save as matfile
R.matlab::writeMat(paste0(substr(s_file, 0, nchar(s_file)-4), '1.mat'),
                   Data = list(windowl=windowl, 
                               stages = t(rep(0, ncol(c3_a2_wider))),
                               channel.names = rec_header$sHeaders$label,
                               fs = (fs/dn),
                               epochl = epochl,
                               C3A2 = c3_a2_wider %>% data.matrix(),
                               Pspec = c3_a2_spectra_wider %>% dplyr::select(-freq) %>% data.matrix(),
                               PEMG = PEMG$pemg,
                               # Left ocular signal
                               LOC = eeg_nested %>% dplyr::select(epoch, all_of(s_loc)) %>%
                                 tidyr::pivot_wider(names_from = epoch, values_from = all_of(s_loc)) %>% 
                                 summarize(across(starts_with('epoch'), unlist)) %>%
                                 data.matrix(),
                               # Right ocular signal
                               ROC =  eeg_nested %>% dplyr::select(epoch, all_of(s_roc)) %>%
                                 tidyr::pivot_wider(names_from = epoch, values_from = all_of(s_roc)) %>% 
                                 summarize(across(starts_with('epoch'), unlist)) %>%
                                 data.matrix(),
                               # electromyogram signal
                               EMG = emg_wider %>% data.matrix(),
                               max.sample = nrow(eeg_downsampled_filtered),
                               maxep = n_epochs
                   )
)
######################################################################################
# copy mat file to sleep-scoring mat folder and execute python script
file.copy(paste0(substr(s_file, 0, nchar(s_file)-4), '1.mat'), '../mat/', overwrite = TRUE )
# load reticulate and exectute scoring python script
# search python
reticulate::py_available()
reticulate::py_config()
reticulate::py_run_file("predict.py")
}

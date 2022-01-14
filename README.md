# Automatic Human Sleep Stage Scoring Using Deep Neural Networks

This code is implementing the algorithm we used in our paper "Automatic Human Sleep Stage Scoring Using Deep Neural Networks".

Please cite our paper:

"Automatic Human Sleep Stage Scoring Using Deep Neural Networks"
Alexander Malafeev, Dmitry Laptev, Stefan Bauer, Ximena Omlin, Aleksandra Wierzbicka, Adam Wichniak, Wojciech Jernajczyk, Robert Riener, Joachim Buhmann and Peter Achermann
Front. Neurosci., 06 November 2018, https://doi.org/10.3389/fnins.2018.00781

## Getting Started

 The code is not exactly the same, but with slight improvements.
Since we can not publish the training data due to privacy reasons we provide only the needed code to train your own model using your data. We also provide trained models and the code needed to apply it for edf files. 

### Prerequisites

You will need a GPU to train the network, but application of already trained model is possible using  a CPU. 


### Scoring

After you have converted your data you can either train your own network using script train.py
or use one of the networks we trained.

To use pretrained network you can run the script predict.py. You should set the variable n_channels.
It can be 1, 2 or 3. The network will use 1 EEG channels, 1 EEG + 2 EOG channels, or 1 EEG + 2 EOG + 1 EMG correspondingly.

If your recording is longer than 8 hours I suggest to use a script predict_split.py. It splits the recording into segments (by default 1000 epochs long) to make sure there would be enough memory.

The .mat files with the results will be stored in the folder ./score_sleep/pred/
After the recordings are scored you can plot the result by using the matlab script plot_res.m located in the folder score sleep. Script will product the hypnograms and store them in the folder ./score_sleep/plot.

### Training 

If you want to train your own network you should use the script train.py. 
In the script you should set variables data_dir and f_set.
data_dir is the directory with the data for training. f_set is the .mat file containing the list of files in training, validation and test sets. You can generate this file using one of the scripts in the tools folder: create_file_sets1.m or create_file_sets2.m. The difference between them is that the first one just splits the files randomly. The second one takes into account that different recordings can be recorded from the same person.

You also need to set n_channels in the same way as for scoring.



## Author

* **Alexander Malafeev** 

## License

This project is licensed under the MIT license - see the [LICENSE.md](LICENSE.md) file for details



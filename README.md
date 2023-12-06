# Data and Code for 'Mental Compression: A Data-Driven, Feature Competition Account'

## Instructions
* For topic modelling, install 'gsl' (If you use homebrew, execute `brew install gsl`)
* Steps below are only required if you want to run clustering code:
  * Create a virtual Python environment
  * Install requirements: `pip install -r requirements.txt`
  * Run `python -m spacy download en_core_web_sm`
<br><br>
* This step is only needed if you want to re-run sentiment analysis (not required for main analyses): 
  * Setup sentiment.ai (For more information: https://benwiseman.github.io/sentiment.ai/#Installation__Setup)
  * Set calculate_sentiment to TRUE.

### For All Analyses Comparing Different Studies:
* Run `between_experiment_analyses/analysis.R`

### Study 1 - Satisfaction with Personal Experiences:
* Run `e1_satisfaction_w_personal_exp/analysis/analysis.R`

### Study 1b:
* Run `e1b_naturally_evaluative/analysis/analysis.R`

### Study 1c:
* Run `e1c_process_awareness/analysis/analysis.R`

### Study 2 - Hiring Based on Interviews:
* Run `e2_hiring/analysis/analysis.R`

### Study 3 - Meaningfulness of a Life:
* Run `e3_meaningfulness_of_a_life/analysis/analysis.R`

### Study 4 - Direct Experiences:
* Run `e4_direct_experiences/analysis/analysis.R`

    #### Other Analysis Files in E4
  * For exclusions, converting timeseries data to lines, plotting each participant line, calculating features of each line (e.g., num of peaks, derivative): Run `e4_direct_experiences/analysis/main.ipynb`. To run the .ipynb files, create a Python environment and run `pip install -r requirements.txt`
  * For clustering, run `e4_direct_experiences/analysis/TimeSeriesClustering.ipynb`
  * For re-generating the data file containing the features, run `e4_direct_experiences/analysis/calculate_predictors.R`, `e4_direct_experiences/analysis/main.ipynb`, and then `e4_direct_experiences/analysis/TimeSeriesClustering.ipynb`  

------------

## Troubleshooting
* If you get `Error in hash() : argument "x" is missing, with no default`, try unloading the library and loading it again, i.e., run `pacman::p_unload('hash')` and then `pacman::p_load('hash')`
* sentiment.ai setup probably won't work on an ARM based macbook, I will need to find a workaround for that.
* If you run into this error while setting up sentiment.ai: `‘~/.virtualenvs/r-sentiment-ai/bin/python’ was not built with a shared library.
reticulate can only bind to copies of Python built with ‘--enable-shared’.`, install pyenv and run `$ env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.8.10`

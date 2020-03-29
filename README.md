# Model
This model matches patient data using a logit regression over penalty functions determined by a mix of qualitatively derived functions as well as commonly used text-similarity algorithmns. To prevent overfitting, final covariate selection for the regression is performed by an elastic net regularization process.

The model goes through several steps:
1. We determine functions to penalize differences in characteristics between two patient IDs. We consider the possibility of misspellings, transcription errors, as well as other reasons there could be different data between two identical patients. For example, one such characteristic we used was the minimum of the difference between the digit sum of the date-of-births and the difference between the numeric values of the date-of-births. This takes into account the fact that there is little difference between 12/31/1999 and 1/1/2000, but also little difference between 12/31/1999 and 12/13/1999.
2. Other penalty functions are generated with commonly-used text-matching algorithms (e.g., soundex and Levenshtein distance) as well as a spelling-correction algorithmn (Hunspell).
3. Additional penalty terms are added for indication of NA/missing values.
4. A logit regression is used to determine the coefficient weights on each penalty term. Covariate selection is performed through an elastic net regularization process, with the hyperparameters selected through 10-fold cross-validation.
5. The model can then be used to match new testing data or calculated the in-sample fit on the training data.


# Usage
1. If you don't have R — install it!
2. Once you've opened R, you will need to install these packages which you can do using the install.packages() function: tidyverse, hunspell, stringr, stringdist, glmnet.
3. Run the below code, making changes to the `_PATH` variables as indicated further below.

```R
### Load libraries
library(tidyverse)
library(hunspell)
library(stringr)
library(stringdist)
library(glmnet)

# Load functions
source('https://raw.githubusercontent.com/cye131/OfficeAllyPatientMatch/master/R/testModel.R')
source('https://raw.githubusercontent.com/cye131/OfficeAllyPatientMatch/master/R/trainModel.R')

# Train model
model = trainModel(
  INPUT_TRAINING_CSV_PATH = 'https://raw.githubusercontent.com/OfficeAllyGit/LAHack-2020/master/Challenge%201%20-%20Patient%20Match/Patient%20Matching%20Data.csv',
  OUTPUT_MODEL_RDS_PATH = 'C:/Users/Charles/Documents/model.rds',
  OUTPUT_TRAINING_CSV_PATH  = 'C:/Users/Charles/Documents/train-output.csv'
  )

# Test model
testModel(
  INPUT_TESTING_CSV_PATH = 'C:/Users/Charles/Downloads/test.csv',
  MODEL = model,
  OUTPUT_TESTING_CSV_PATH = 'C:/Users/Charles/Documents/test-output.csv'
)
```
For model training, change the argument of `OUTPUT_MODEL_RDS_PATH` to a local file path (ending with extension .rds) to output the trained model. Change the argument of `OUTPUT_TRAINING_CSV_PATH` to a local file path (ending with extension .csv) to output the fitted values of the trained model. The `INPUT_TRAINING_CSV_PATH` can be edited if the model is to be trained on different data than those provided.

For model testing, change the argument of `INPUT_TESTING_CSV_PATH` to a URL or CSV file path (ending with extension .csv) containing the test dataset. Additionally, change the argument of `OUTPUT_TESTING_CSV_PATH` to a local file path (ending with extension .csv) where the predicted values of the testing model will be located.

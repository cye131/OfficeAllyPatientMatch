# Usage
1. If you don't have R — install it!
2. Once you've opened R, you will need to install these packages which you can do using the install.packages() function: tidyverse, hunspell, stringr, stringdist, glmnet.
3. Run the below code, making changes to the `PATH` variables as indicated further below.

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
For model training, change the argument of `OUTPUT_MODEL_RDS_PATH` to a local file path (ending with extension .rds) to output the trained model. Change the argument of `OUTPUT_TRAINING_CSV_PATH` to a local file path (ending with extension .csv) to output the fitted values of the trained model.

For model testing, change the argument of `INPUT_TESTING_CSV_PATH` to a URL or CSV file path (ending with extension .csv) containing the test dataset. Additionally, change the argument of `OUTPUT_TESTING_CSV_PATH` to a local file path (ending with extension .csv) where the predicted values of the testing model will be located.

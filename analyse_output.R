library(data.table)

total_n_bins = 60
bins_in_testPeriod = 12
bins_in_runin_testPeriod = 12

interestDrug = "Metformin_"

# load drug data for all IDs from test set
drugData <- read.csv("./pythonOutput/X_test_drugs.csv", header = FALSE)
# trim to runinData only
runinDrugData <- drugData[, 1:(ncol(drugData) - bins_in_testPeriod)]
# extract runin_testPeriod only
testExtract <- runinDrugData[, ((ncol(runinDrugData) - bins_in_runin_testPeriod) + 1): ncol(runinDrugData)]

# load lookup to interpret drug data
drugLookup <- read.csv("./pythonOutput/lookup.csv")
drugLookupDT <- data.table(drugLookup)

# number for metformin alone
interestNumber <- drugLookupDT[vectorWords == interestDrug]$vectorNumbers

# flags for same drug throughout final year and interest drug
report_sameDrug <- rep(0, nrow(testExtract))
report_drugOfInterest <- rep(0, nrow(testExtract))

for (i in seq(1, nrow(testExtract), 1)) {
  
  if(i%%400 == 0){print(i)}
  
  rowValues <- as.numeric(testExtract[i, ])
  report_sameDrug[i] <- ifelse(sum(diff(rowValues)) == 0, 1, 0)
  report_drugOfInterest[i] <- ifelse(rowValues[1] == interestNumber, 1, 0)
  
}

report_interestDrug_noChange <- ifelse(report_sameDrug == 1 & report_drugOfInterest == 1, 1, 0)

## load in predictions
therapyArray <- read.csv("./pythonOutput/therapyArray.csv", header = FALSE)
vectorWordsAsCharacter <- as.character(drugLookupDT$vectorWords)

for (j in seq(1, nrow(therapyArray), 1)) {
  therapyArray$drugNames[j] <- vectorWordsAsCharacter[drugLookupDT$vectorNumbers == therapyArray$V1[j]]
}

# generate analysis frame
# predictions, starting hba1c / sbp etc
analysisFrame <- as.data.frame(matrix(0, nrow = nrow(drugData), ncol = nrow(therapyArray)))
colnames(analysisFrame) <- therapyArray$drugNames

# first add drug combinations as will be different between experiments
for (ii in seq(1, nrow(therapyArray), 1)) {
  
  drugExperiment_n <- (ii - 1) # because from python start at 0
  
  pathToRead <- paste("./pythonOutput/y_pred_asNumber_combinationNumber_",drugExperiment_n, ".csv", sep = "")
  
  analysisFrame[, ii] <- read.csv(pathToRead, header = FALSE)
  
}

# then add consistent parameters
bmiData <- read.csv("./pythonOutput/decoded_Xtest_bmi.csv", header = FALSE)
hba1cData <- read.csv("./pythonOutput/decoded_Xtest_hba1c.csv", header = FALSE)
sbpData <- read.csv("./pythonOutput/decoded_Xtest_sbp.csv", header = FALSE)
# ageData <- read.csv("./pythonOutput/X_test_age.csv", header = FALSE)

analysisFrame$bmi <- bmiData[, (total_n_bins - bins_in_testPeriod)]
analysisFrame$hba1c <- hba1cData[, (total_n_bins - bins_in_testPeriod)]
analysisFrame$sbp <- sbpData[, (total_n_bins - bins_in_testPeriod)]

###
# subset analysisFrame to IDs of interest (ie on the interest drug during runin)
interestAnalysisFrame <- analysisFrame[report_interestDrug_noChange == 1, ]




# find IDs where runin test period all the interest drug





# find IDs on metformin alone in last year of runin

# find predictions for each of the 2nd line agents

# find highest prediction

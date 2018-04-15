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
for (j in seq(1, nrow(therapyArray), 1)) {
  therapyArray$drugNames[j] <- drugLookupDT[vectorNumbers == therapyArray$V1[j]]$vectorWords
}

pred0 <- read.csv("./pythonOutput/y_pred_asNumber_combinationNumber_0.csv", header = FALSE)
pred1 <- read.csv("./pythonOutput/y_pred_asNumber_combinationNumber_1.csv", header = FALSE)




# find IDs where runin test period all the interest drug





# find IDs on metformin alone in last year of runin

# find predictions for each of the 2nd line agents

# find highest prediction

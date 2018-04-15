library(data.table)

total_n_bins = 60
bins_in_testPeriod = 12
bins_in_runin_testPeriod = 12

interestDrug = "Metformin_"

# load drug data for all IDs from test set
drugData <- read.csv("./pythonOutput/X_test_drugs.csv", header = FALSE)
# trim to runinData only
runinDrugData <- drug_data[, 1:(ncol(drugData) - bins_in_testPeriod)]
# extract runin_testPeriod only
testExtract <- runinDrugData[, ((ncol(runinDrugData) - bins_in_runin_testPeriod) + 1): ncol(runinDrugData)]

# load lookup to interpret drug data
drugLookup <- read.csv("./pythonOutput/lookup.csv")
drugLookupDT <- data.table(drugLookup)

# number for metformin alone
interestNumber <- drugLookupDT[vectorWords == interestDrug]$vectorNumbers




# find IDs on metformin alone in last year of runin

# find predictions for each of the 2nd line agents

# find highest prediction

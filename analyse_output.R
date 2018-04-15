library(data.table)

total_n_bins = 60
bins_in_testPeriod = 12

# load drug data for all IDs from test set
drugData <- read.csv("./pythonOutput/X_test_drugs.csv", header = FALSE)
# trim to runinData only

# load lookup to interpret drug data
drugLookup <- read.csv("./pythonOutput/lookup.csv")
drugLookupDT <- data.table(drugLookup)

# number for metformin alone
interestNumber <- drugLookupDT[vectorWords == "Metformin_"]$vectorNumbers




# find IDs on metformin alone in last year of runin

# find predictions for each of the 2nd line agents

# find highest prediction

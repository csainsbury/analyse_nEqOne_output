library(data.table)

total_n_bins = 60
bins_in_testPeriod = 12
bins_in_runin_testPeriod = 36

interestDrug = "Metformin_"

# load number of loops
nLoops <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/numberOfLoops.csv", header = FALSE)
nLoops_integer <- nLoops[1, 1] - 1
#
nLoops_integer = 1

# load drug data for all IDs from test set
drugData <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/X_test_drugs.csv", header = FALSE)
# trim to runinData only
runinDrugData <- drugData[, 1:(ncol(drugData) - bins_in_testPeriod)]
# extract runin_testPeriod only
testExtract <- runinDrugData[, ((ncol(runinDrugData) - bins_in_runin_testPeriod) + 1): ncol(runinDrugData)]

# load lookup to interpret drug data
drugLookup <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/lookup.csv")
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
therapyArray <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/therapyArray.csv", header = FALSE)
vectorWordsAsCharacter <- as.character(drugLookupDT$vectorWords)

for (j in seq(1, nrow(therapyArray), 1)) {
  therapyArray$drugNames[j] <- vectorWordsAsCharacter[drugLookupDT$vectorNumbers == therapyArray$V1[j]]
}

## loop through for each iteration of the analysis
## generate a 3D matrix of row - ID, col - drug combinations, z - iterations
## then will need to average through the z axis to give final result

analysisArray_3d <- array(0, c(nrow(drugData), nrow(therapyArray), nLoops_integer))

for (iter in seq(1, nLoops_integer, 1)) {
# generate analysis frame for a single iteration
# predictions, starting hba1c / sbp etc
analysisArray_2d <- array(0, c(nrow(drugData), nrow(therapyArray)))
#colnames(analysisFrame_2d) <- therapyArray$drugNames

    # first add drug combinations as will be different between experiments
    for (ii in seq(1, nrow(therapyArray), 1)) {
      
      drugExperiment_n <- (ii - 1) # because from python start at 0
      
      pathToRead <- paste("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/y_pred_asNumber_combinationNumber_",drugExperiment_n, "_runN_", iter,".csv", sep = "")
      
      analysisArray_2d[, ii] <- read.csv(pathToRead, header = FALSE)[,1]
      
    }

# build the 3d array
analysisArray_3d[, , iter] = analysisArray_2d

}

## plot the relative probabilities
for (p in seq(1, 50, 1)) {
  if(p == 1) {plot(analysisArray_3d[p,1,], cex = 0, ylim = c(0, 1)); lines(analysisArray_3d[p,1,])}
  if(p > 1) {points(analysisArray_3d[p,1,], cex = 0); lines(analysisArray_3d[p,1,], col = p)}
}

# average the output
averagedOutput_mean <- array(0, c(nrow(drugData), nrow(therapyArray)))
averagedOutput_median <- array(0, c(nrow(drugData), nrow(therapyArray)))

for (r in seq(1, nrow(analysisArray_2d), 1)) {
  for (c in seq(1, ncol(analysisArray_2d), 1)) {
    averagedOutput_mean[r, c] <- mean(analysisArray_3d[r, c, ])
    averagedOutput_median[r, c] <- median(analysisArray_3d[r, c, ])
  }
}

# then add consistent parameters
bmiData <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/decoded_Xtest_bmi.csv", header = FALSE)
hba1cData <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/decoded_Xtest_hba1c.csv", header = FALSE)
sbpData <- read.csv("~/R/_workingDirectory/analyse_nEqOne_output/pythonOutput/decoded_Xtest_sbp.csv", header = FALSE)
# ageData <- read.csv("./pythonOutput/X_test_age.csv", header = FALSE)

# choose mean or median
#analysisFrame = as.data.frame(averagedOutput_mean)
#
analysisFrame = as.data.frame(averagedOutput_median)
colnames(analysisFrame) <- therapyArray$drugNames


analysisFrame$bmi <- bmiData[, (total_n_bins - bins_in_testPeriod)]
analysisFrame$hba1c <- hba1cData[, (total_n_bins - bins_in_testPeriod)]
analysisFrame$sbp <- sbpData[, (total_n_bins - bins_in_testPeriod)]

###
# subset analysisFrame to IDs of interest (ie on the interest drug during runin)
interestAnalysisFrame <- analysisFrame[report_interestDrug_noChange == 1, ]
interestAnalysisFrame$maxNames = names(interestAnalysisFrame[, 1:7])[apply(interestAnalysisFrame[, 1:7], 1, which.max)]

plotFrame <- subset(interestAnalysisFrame, sbp > 0 & bmi > 0)
plot(plotFrame$hba1c, plotFrame$sbp / plotFrame$bmi, ylim = c(2, 8), col = ifelse(plotFrame$maxNames == "DPP4_Metformin_", 2, 
                                                                                  ifelse(plotFrame$maxNames == "humanBDmixInsulin_Metformin_", 3, 
                                                                                         ifelse(plotFrame$maxNames == "Metformin_", 4, 
                                                                                                ifelse(plotFrame$maxNames == "Metformin_SGLT2_", 5, 
                                                                                                       ifelse(plotFrame$maxNames == "GLP1_Metformin_", 6, 1))))), pch = 16, cex = 2, xlim = c(30, 130))
abline(lm(plotFrame$sbp / plotFrame$bmi ~ plotFrame$hba1c))

print(table(plotFrame$maxNames))
# observations:
# GLP1 favoured. need to add SU alone and an insulin option - basal and basal bolus


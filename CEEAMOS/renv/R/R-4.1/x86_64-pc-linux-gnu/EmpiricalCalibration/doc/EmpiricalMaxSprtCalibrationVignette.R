## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library(EmpiricalCalibration)
cvs <- readRDS("cvs.rds")
allLlrs <- readRDS("allLlrs.rds")
set.seed(123)

## -----------------------------------------------------------------------------
maxSprtSimulationData <- simulateMaxSprtData()
head(maxSprtSimulationData)

## ----warning=FALSE------------------------------------------------------------
library(Cyclops)
library(survival)

dataOutcome51 <- maxSprtSimulationData[maxSprtSimulationData$outcomeId == 51, ]
dataOutcome51LookT50 <- dataOutcome51[dataOutcome51$lookTime == 50, ]

cyclopsData <- createCyclopsData(Surv(time, outcome) ~ exposure , 
                                 modelType = "cox", 
                                 data = dataOutcome51LookT50)
fit <- fitCyclopsModel(cyclopsData)
coef(fit)

## -----------------------------------------------------------------------------
# Maximum log likelihood:
fit$log_likelihood

## -----------------------------------------------------------------------------
llNull <- getCyclopsProfileLogLikelihood(object = fit,
                                         parm = "exposureTRUE",
                                         x = 0)$value
llNull

## -----------------------------------------------------------------------------
if (fit$return_flag == "ILLCONDITIONED" || coef(fit) < 0) {
  llr <- 0
} else {
  llr <- fit$log_likelihood - llNull
}
llr

## -----------------------------------------------------------------------------
outcomesPerLook <- aggregate(outcome ~ lookTime, dataOutcome51, sum)
# Need incremental outcomes per look:
outcomesPerLook <- outcomesPerLook$outcome[order(outcomesPerLook$lookTime)]
outcomesPerLook[2:10] <- outcomesPerLook[2:10] - outcomesPerLook[1:9]

exposedTime <- sum(dataOutcome51$time[dataOutcome51$exposure == TRUE & 
                                        dataOutcome51$lookTime == 100])
unexposedTime <- sum(dataOutcome51$time[dataOutcome51$exposure == FALSE & 
                                          dataOutcome51$lookTime == 100])
cv <- computeCvBinomial(groupSizes = outcomesPerLook,
                        z = unexposedTime / exposedTime,
                        minimumEvents = 1,
                        alpha = 0.05)
cv

## -----------------------------------------------------------------------------
llr > cv

## -----------------------------------------------------------------------------
llProfileOutcome51LookT50 <- getCyclopsProfileLogLikelihood(object = fit,
                                                            parm = "exposureTRUE",
                                                            bounds = log(c(0.1, 10)))
head(llProfileOutcome51LookT50)

## -----------------------------------------------------------------------------
negativeControlProfilesLookT50 <- list()
dataLookT50 <- maxSprtSimulationData[maxSprtSimulationData$lookTime == 50, ]
for (i in 1:50) {
  dataOutcomeIlookT50 <- dataLookT50[dataLookT50$outcomeId == i, ]
  cyclopsData <- createCyclopsData(Surv(time, outcome) ~ exposure , 
                                   modelType = "cox", 
                                   data = dataOutcomeIlookT50)
  fit <- fitCyclopsModel(cyclopsData)
  llProfile <- getCyclopsProfileLogLikelihood(object = fit,
                                              parm = "exposureTRUE",
                                              bounds = log(c(0.1, 10)))
  negativeControlProfilesLookT50[[i]] <- llProfile
}

## -----------------------------------------------------------------------------
nullLookT50 <- fitNullNonNormalLl(negativeControlProfilesLookT50)
nullLookT50

## -----------------------------------------------------------------------------
calibratedLlr <- calibrateLlr(null = nullLookT50,
                              likelihoodApproximation = llProfileOutcome51LookT50)
calibratedLlr

## -----------------------------------------------------------------------------
calibratedLlr > cv

## ----eval=FALSE---------------------------------------------------------------
#  allLlrs <- data.frame()
#  for (t in unique(maxSprtSimulationData$lookTime)) {
#  
#    # Compute likelihood profiles for all negative controls:
#    negativeControlProfilesLookTt <- list()
#    dataLookTt <- maxSprtSimulationData[maxSprtSimulationData$lookTime == t, ]
#    for (i in 1:50) {
#      dataOutcomeIlookTt <- dataLookTt[dataLookTt$outcomeId == i, ]
#      cyclopsData <- createCyclopsData(Surv(time, outcome) ~ exposure ,
#                                       modelType = "cox",
#                                       data = dataOutcomeIlookTt)
#      fit <- fitCyclopsModel(cyclopsData)
#      llProfile <- getCyclopsProfileLogLikelihood(object = fit,
#                                                  parm = "exposureTRUE",
#                                                  bounds = log(c(0.1, 10)))
#      negativeControlProfilesLookTt[[i]] <- llProfile
#    }
#  
#    # Fit null distribution:
#    nullLookTt <- fitNullNonNormalLl(negativeControlProfilesLookTt)
#  
#    # Compute calibrated and uncalibrated LLRs for all negative controls:
#    llrsLookTt <- c()
#    calibratedLlrsLookTt <- c()
#    for (i in 1:50) {
#      dataOutcomeIlookTt <- dataLookTt[dataLookTt$outcomeId == i, ]
#      cyclopsData <- createCyclopsData(Surv(time, outcome) ~ exposure ,
#                                       modelType = "cox",
#                                       data = dataOutcomeIlookTt)
#      fit <- fitCyclopsModel(cyclopsData)
#      llProfile <- getCyclopsProfileLogLikelihood(object = fit,
#                                                  parm = "exposureTRUE",
#                                                  bounds = log(c(0.1, 10)))
#  
#      # Calibrated LLR:
#      calibrateLlr <- calibrateLlr(null = nullLookTt,
#                   likelihoodApproximation = llProfile)
#      calibratedLlrsLookTt[i] <- calibrateLlr
#  
#      # Uncalibrated LLR:
#      llNull <- getCyclopsProfileLogLikelihood(object = fit,
#                                               parm = "exposureTRUE",
#                                               x = 0)$value
#      if (fit$return_flag == "ILLCONDITIONED" || coef(fit) < 0) {
#        llr <- 0
#      } else {
#        llr <- fit$log_likelihood - llNull
#      }
#      llrsLookTt[i] <- llr
#    }
#  
#    # Store in a data frame:
#    allLlrs <- rbind(allLlrs,
#                     data.frame(t = t,
#                                outcomeId = 1:50,
#                                llr = llrsLookTt,
#                                calibrateLlr = calibratedLlrsLookTt))
#  
#  }

## ----eval=FALSE---------------------------------------------------------------
#  cvs <- c()
#  for (i in 1:50) {
#    dataOutcomeI <- maxSprtSimulationData[maxSprtSimulationData$outcomeId == i, ]
#    outcomesPerLook <- aggregate(outcome ~ lookTime, dataOutcomeI, sum)
#    # Need incremental outcomes per look:
#    outcomesPerLook <- outcomesPerLook$outcome[order(outcomesPerLook$lookTime)]
#    outcomesPerLook[2:10] <- outcomesPerLook[2:10] - outcomesPerLook[1:9]
#  
#    exposedTime <- sum(dataOutcomeI$time[dataOutcomeI$exposure == TRUE &
#                                           dataOutcomeI$lookTime == 100])
#    unexposedTime <- sum(dataOutcomeI$time[dataOutcomeI$exposure == FALSE &
#                                              dataOutcomeI$lookTime == 100])
#    cv <- computeCvBinomial(groupSizes = outcomesPerLook,
#                            z = unexposedTime / exposedTime,
#                            minimumEvents = 1,
#                            alpha = 0.05)
#    cvs[i] <- cv
#  }
#  

## -----------------------------------------------------------------------------
signals <- c()
calibratedSignals <- c()
for (i in 1:50) {
  cv <- cvs[i]
  signals[i] <- any(allLlrs$llr[allLlrs$outcomeId == i] > cv)
  calibratedSignals[i] <- any(allLlrs$calibrateLlr[allLlrs$outcomeId == i] > cv)
}
# Type 1 error when not calibrated (should be 0.05):
mean(signals)

# Type 2 error when calibrated (should be 0.05):
mean(calibratedSignals)


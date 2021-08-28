# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CEEAMOS
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create the analyses details
#'
#' @details
#' This function creates files specifying the analyses that will be performed.
#'
#' @param workFolder        Name of local folder to place results; make sure to use forward slashes
#'                            (/)
#'
#' @export
createAnalysesDetails <- function(workFolder) {
    maxCohortSizeForFitting = 50000 # limit max cohort size
    defaultPrior <- Cyclops::createPrior("laplace",
                                         exclude = c(0),
                                         useCrossValidation = TRUE)
    
    defaultControl <- Cyclops::createControl(cvType = "auto",
                                             startingVariance = 0.01,
                                             noiseLevel = "quiet",
                                             tolerance  = 2e-07,
                                             maxIterations = 2500,
                                             cvRepetitions = 10,
                                             seed = 1234)
    excludedCovariateConceptIds <- c(1308842,
                                     1342439,
                                     1317640,
                                     1334456,
                                     1331235,
                                     1373225,
                                     40226742,
                                     1310756,
                                     907013,
                                     1367500,
                                     1308216,
                                     1347384,
                                     978555,
                                     974166,
                                     1363749,
                                     1346686,
                                     1341927,
                                     1395058,
                                     1340128,
                                     1351557,
                                     1335471,
                                     40235485)
    defaultCovariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                           useDemographicsAge = TRUE, 
                                                                           useDemographicsAgeGroup = TRUE,
                                                                           useDemographicsRace = TRUE, 
                                                                           useDemographicsEthnicity = TRUE,
                                                                           useDemographicsIndexYear = TRUE, 
                                                                           useDemographicsIndexMonth = TRUE,
                                                                           useConditionOccurrenceAnyTimePrior = FALSE,
                                                                           useConditionGroupEraAnyTimePrior = TRUE,
                                                                           useConditionGroupEraLongTerm = TRUE,
                                                                           useConditionGroupEraShortTerm = TRUE,
                                                                           useDrugGroupEraLongTerm = TRUE,
                                                                           useDrugGroupEraShortTerm = TRUE,
                                                                           useDrugGroupEraOverlapping = TRUE,
                                                                           useProcedureOccurrenceLongTerm = TRUE,
                                                                           useMeasurementLongTerm = TRUE,
                                                                           useMeasurementShortTerm = TRUE,
                                                                           useMeasurementRangeGroupLongTerm = TRUE,
                                                                           useObservationLongTerm = TRUE,
                                                                           useCharlsonIndex = TRUE, 
                                                                           useVisitCountLongTerm = TRUE, 
                                                                           longTermStartDays = -365,
                                                                           mediumTermStartDays = -180, 
                                                                           shortTermStartDays = -30, 
                                                                           endDays = 0,
                                                                           excludedCovariateConceptIds = excludedCovariateConceptIds, 
                                                                           addDescendantsToExclude = TRUE,
                                                                           includedCovariateIds = c()
    )

    getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                                     firstExposureOnly = FALSE,
                                                                     removeDuplicateSubjects = 'keep first',
                                                                     restrictToCommonPeriod = TRUE,
                                                                     maxCohortSize = 0,
                                                                     excludeDrugsFromCovariates = FALSE,
                                                                     covariateSettings = defaultCovariateSettings)
    
    OnTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                 firstExposureOnly = FALSE,
                                                                 washoutPeriod = 0,
                                                                 removeDuplicateSubjects = 'keep first',
                                                                 minDaysAtRisk = 1,
                                                                 riskWindowStart = 1,
                                                                 addExposureDaysToStart = FALSE,
                                                                 riskWindowEnd = 0,
                                                                 addExposureDaysToEnd = TRUE,
                                                                 censorAtNewRiskWindow = FALSE)
    
    ITT <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                         firstExposureOnly = FALSE,
                                                         washoutPeriod = 0,
                                                         removeDuplicateSubjects = 'keep first',
                                                         minDaysAtRisk = 1,
                                                         riskWindowStart = 1,
                                                         addExposureDaysToStart = FALSE,
                                                         riskWindowEnd = 9999,
                                                         addExposureDaysToEnd = FALSE,
                                                         censorAtNewRiskWindow = FALSE)
    
    createPsArgs1 <- CohortMethod::createCreatePsArgs(control = defaultControl,
                                                      errorOnHighCorrelation = FALSE,
                                                      #excludeCovariateIds = excludeCovariateIds,
                                                      stopOnError = FALSE)
    
    #trimByPsArgs<- CohortMethod::createTrimByPsArgs(trimFraction = 0.05)
    
    oneToOneMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1,
                                                               caliper = 0.2,
                                                               caliperScale = "standardized logit")
    
    variableRatioMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 100,
                                                                    caliper = 0.2,
                                                                    caliperScale = "standardized logit")
    
    stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
    
    #without matching arg
    fitOutcomeModelArgs0 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                    modelType = "cox",
                                                                    stratified = FALSE,
                                                                    prior = defaultPrior,
                                                                    control = defaultControl)
    #conditioning
    fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                    modelType = "cox",
                                                                    stratified = TRUE,
                                                                    prior = defaultPrior,
                                                                    control = defaultControl)
    #Without conditioning
    fitOutcomeModelArgs2 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                    modelType = "cox",
                                                                    stratified = FALSE,
                                                                    prior = defaultPrior,
                                                                    control = defaultControl)
    
    ###########################################
    a1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                         description = "On-treatment outcome, one-to-one matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         matchOnPs = TRUE,
                                         matchOnPsArgs = oneToOneMatchOnPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs2)
    
    a2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                         description = "On-treatment outcome, variable-ratio matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         matchOnPs = TRUE,
                                         matchOnPsArgs = variableRatioMatchOnPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    
    a3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                         description = "On-treatment outcome, stratification",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = TRUE,
                                         stratifyByPsArgs = stratifyByPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                         description = "On-treatment outcome, without matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = FALSE,
                                         createPsArgs = NULL,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = FALSE,
                                         stratifyByPsArgs = NULL,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    ###########################################
    
    a11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                          description = "ITT, one-to-one matching",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = oneToOneMatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs2)
    
    a12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                          description = "ITT, variable-ratio matching",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = variableRatioMatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    
    a13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                          description = "ITT, stratification",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          #trimByPs = TRUE,
                                          #trimByPsArgs = trimByPsArgs,
                                          stratifyByPs = TRUE,
                                          stratifyByPsArgs = stratifyByPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a14 <- CohortMethod::createCmAnalysis(analysisId = 14,
                                          description = "ITT, without matching",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = FALSE,
                                          createPsArgs = NULL,
                                          #trimByPs = TRUE,
                                          #trimByPsArgs = trimByPsArgs,
                                          stratifyByPs = FALSE,
                                          stratifyByPsArgs = NULL,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    cmAnalysisList <- list(a1, a2, a3, a4,
                           a11, a12, a13, a14)
    
    CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(workFolder, "cmAnalysisList.json"))
    
}

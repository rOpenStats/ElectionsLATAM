library(testthat)
library(ElectionsLATAM)
library(lgr)
library(dplyr)
library(readr)

log.levels <- lgr::get_log_levels()

lgr$set_threshold(log.levels["debug"])
lgr::threshold("debug", lgr::get_logger("EcologicalInferenceStrategyCalvoEtAl"))



test_check("ElectionsLATAM")

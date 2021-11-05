# ============================= #
#           Padgett
# ============================= #
# Data Created: 10/15/2018    
# By: R. Noah Padgett           
# ============================= #
# Project for SERA 2019       
# Conditions 1-9
# ============================= #
library(MplusAutomation)
createModels("mcfa_create_script_datagen.txt")
createModels("createmodels_Spec_CL1_CL2.txt")
createModels("createmodels_Spec_WL1_CL2.txt")
createModels("createmodels_Spec_CL1_WL2.txt")
createModels("createmodels_Spec_WL1_WL2.txt")

runModels(recursive = T)


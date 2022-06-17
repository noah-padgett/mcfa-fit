# ============================= #
# Padgett ML-CFA Study Models
# ============================= #
# Data Created: 10/15/2018    
# By: R. Noah Padgett           
# ============================= #   
# Conditions 1-72
# ============================= #
library(MplusAutomation)
createModels("mcfa_create_script_datagen.txt")
createModels("createmodels_Spec_CL1_CL2_MLR.txt")
createModels("createmodels_Spec_WL1_CL2_MLR.txt")
createModels("createmodels_Spec_CL1_WL2_MLR.txt")
createModels("createmodels_Spec_WL1_WL2_MLR.txt")

createModels("createmodels_Spec_CL1_CL2_WLSMV.txt")
createModels("createmodels_Spec_WL1_CL2_WLSMV.txt")
createModels("createmodels_Spec_CL1_WL2_WLSMV.txt")
createModels("createmodels_Spec_WL1_WL2_WLSMV.txt")

createModels("createmodels_Spec_CL1_CL2_ULSMV.txt")
createModels("createmodels_Spec_WL1_CL2_ULSMV.txt")
createModels("createmodels_Spec_CL1_WL2_ULSMV.txt")
createModels("createmodels_Spec_WL1_WL2_ULSMV.txt")

# the WLSM was not analyzed
# createModels("createmodels_Spec_CL1_CL2_WLSM.txt")
# createModels("createmodels_Spec_WL1_CL2_WLSM.txt")
# createModels("createmodels_Spec_CL1_WL2_WLSM.txt")
# createModels("createmodels_Spec_WL1_WL2_WLSM.txt")

runModels(recursive = T)


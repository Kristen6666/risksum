library(here)

data1 <- read.csv("COVID1-BaselineRisksumCleaning_DATA_.csv")

## New diagnosis
# NEWDXDATA
new_diagnosis_vars <- paste0("newdxco___", c(1:19, 21))  # Variables indicating new diagnoses
no_diagnosis_var <- "newdxco___20"  # Variable indicating no new diagnosis
unknown_refuse_vars <- paste0("newdxco___", c(97, 98))  # Variables indicating refusal or "don't know" responses

# Ensure all values for new diagnosis variables are numeric, and replace "." with NA
data1[new_diagnosis_vars] <- lapply(data1[new_diagnosis_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))
data1[[no_diagnosis_var]] <- ifelse(data1[[no_diagnosis_var]] == ".", NA, as.numeric(data1[[no_diagnosis_var]]))
data1[unknown_refuse_vars] <- lapply(data1[unknown_refuse_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))

# Calculate NEWDXDATA
data1$NEWDXDATA <- with(data1, ifelse(rowSums(data1[new_diagnosis_vars] == 1, na.rm = TRUE) > 0, 1,  # Set to 1 if any new diagnoses
                                      ifelse(rowSums(data1[new_diagnosis_vars] == 0, na.rm = TRUE) == length(new_diagnosis_vars) & 
                                               rowSums(data1[unknown_refuse_vars] == 1, na.rm = TRUE) > 0, 2,  # Set to 2 if no new diagnosis but "don't know" or refuse
                                             ifelse(data1[[no_diagnosis_var]] == 1, 3, NA))))  # Set to 3 if no new diagnosis, otherwise NA

# TOTNEWDX
totnewdx_vars <- paste0("newdxco___", c(1:18, 19, 21))  # Variables for calculating total new diagnoses

# Ensure all values for new diagnosis variables are numeric, and replace "." with NA
data1[totnewdx_vars] <- lapply(data1[totnewdx_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))

# Calculate TOTNEWDX
# Count how many values are 1 in totnewdx_vars
data1$TOTNEWDX <- rowSums(data1[totnewdx_vars] == 1, na.rm = TRUE)

# If no new diagnosis was reported (i.e., count is 0), set TOTNEWDX to NA
data1$TOTNEWDX[data1$TOTNEWDX == 0] <- NA

## Cancer treatment
# Define variables related to cancer treatment
cancer_treatment_vars <- paste0("cancertxco___", 1:5)      # Variables indicating types of cancer treatment
no_treatment_var <- "cancertxco___6"                       # Variable indicating no treatment
unknown_refuse_vars <- paste0("cancertxco___", c(97, 98))  # Variables indicating refusal or "don't know" responses

# Ensure all values for cancer treatment variables are numeric, and replace "." with NA
data1[cancer_treatment_vars] <- lapply(data1[cancer_treatment_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))
data1[[no_treatment_var]] <- ifelse(data1[[no_treatment_var]] == ".", NA, as.numeric(data1[[no_treatment_var]]))
data1[unknown_refuse_vars] <- lapply(data1[unknown_refuse_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))

# Calculate CANCERDATA
data1$CANCERDATA <- with(data1, ifelse(rowSums(data1[cancer_treatment_vars] == 1, na.rm = TRUE) > 0, 1,  # Set to 1 if there was cancer treatment
                                       ifelse(rowSums(data1[cancer_treatment_vars] == 0, na.rm = TRUE) == length(cancer_treatment_vars) & 
                                                rowSums(data1[unknown_refuse_vars] == 1, na.rm = TRUE) > 0, 2,  # Set to 2 if no treatment but "don't know" or refuse
                                              ifelse(data1[[no_treatment_var]] == 1, 3, NA))))  # Set to 3 if no treatment, otherwise NA


## Comorbidity
comorbidity_vars <- paste0("priorcomoco___", 1:20)  # Variables used to calculate TOTCOMO
no_comorbidity_var <- "priorcomoco___21"  # Variable indicating no comorbidities
refuse_var <- "priorcomoco___98"  # Variable indicating refusal to answer

# Ensure all comorbidity variables are numeric, and replace "." with NA
data1[comorbidity_vars] <- lapply(data1[comorbidity_vars], function(x) ifelse(x == ".", NA, as.numeric(x)))
data1[[no_comorbidity_var]] <- ifelse(data1[[no_comorbidity_var]] == ".", NA, as.numeric(data1[[no_comorbidity_var]]))
data1[[refuse_var]] <- ifelse(data1[[refuse_var]] == ".", NA, as.numeric(data1[[refuse_var]]))

# Calculate TOTCOMO
# Count how many values are 1 in comorbidity_vars
data1$TOTCOMO <- rowSums(data1[comorbidity_vars] == 1, na.rm = TRUE)

# If no comorbidity was reported (i.e., count is 0), set TOTCOMO to NA
data1$TOTCOMO[data1$TOTCOMO == 0] <- NA

# Calculate COMODATA
data1$COMODATA <- with(data1, ifelse(rowSums(data1[comorbidity_vars] == 1, na.rm = TRUE) > 0, 1,  # Set to 1 if there are comorbidities
                                     ifelse(rowSums(data1[comorbidity_vars] == 0, na.rm = TRUE) == length(comorbidity_vars) & 
                                              data1[[refuse_var]] == 1, 2,  # Set to 2 if no comorbidities and refusal to answer
                                            ifelse(data1[[no_comorbidity_var]] == 1, 3, NA))))  # Set to 3 if no comorbidities, otherwise NA

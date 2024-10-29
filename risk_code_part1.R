
library(here)


data <- read.csv("COVID1-BaselineRisksumCleaning_DATA_.csv")

# The U.S. participants: if the weight was not in the 70-350 pound range, it was set as missing and the effective weight was rounded up
data$weightdm <- ifelse(data$weightdm < 70 | data$weightdm > 350, NA, round(data$weightdm))

# The international participants: if the weight is not in the 32-158 kg range, it is set as missing and the effective weight is rounded up
data$wtkilo <- ifelse(data$wtkilo < 32 | data$wtkilo > 158, NA, round(data$wtkilo))

# Convert the code for 'heightdm' to the corresponding inch height (for US participants)
height_mapping <- c(48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 
                    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 
                    80, 81, 82, 83)
data$heightdm <- ifelse(data$heightdm %in% 1:36, height_mapping[data$heightdm], NA)



# Calculate the BMI of the US participants (weight in pounds, height in inches)
data$BMI_US <- ifelse(!is.na(data$weightdm) & !is.na(data$heightdm),
                      round((data$weightdm * 703) / (data$heightdm^2), 1),
                      NA)

data$htmeters[data$htmeters == "."] <- NA
data$htmeters <- suppressWarnings(as.numeric(data$htmeters))
# Calculate the BMI of the international participants(weight in kilograms, height in centimeters)
data$BMI_Intl <- ifelse(!is.na(data$wtkilo) & !is.na(data$htmeters),
                        round(data$wtkilo / ((data$htmeters / 100)^2), 1),
                        NA)

# BMI of U.S. and international participants was combined into the same column rdbmi
data$rdbmi <- ifelse(!is.na(data$BMI_US), data$BMI_US, data$BMI_Intl)

# Delete the temporary BMI column
data <- subset(data, select = -c(BMI_US, BMI_Intl))


## MObility and IADL

# 1. mobind
# If quartmilelm is missing but steps10lm is not missing, mobind = 1
# If quartmilelm is not missing and steps10lm is not missing, mobind = 2
# If quartmilelm and steps10lm are both missing, mobind = 0
data$mobind <- ifelse(is.na(data$quartmilelm) & !is.na(data$steps10lm), 1,
                      ifelse(!is.na(data$quartmilelm) & !is.na(data$steps10lm), 2, 0))

# 2. mobdis
# If mobind = 2 and quartmilelm or steps10lm has a mobility disability, the mobdis is calculated depending on the situation
data$mobdis <- ifelse(data$mobind == 2,
                      ifelse(data$quartmilelm == 1 & data$steps10lm == 0, 1,  # 1/4 mile mobility disability
                             ifelse(data$quartmilelm == 0 & data$steps10lm == 1, 2,  # 10 steps mobility disability
                                    ifelse(data$quartmilelm == 1 & data$steps10lm == 1, 3, 0))),  # both
                      NA)

# 3. mobdiff
# Determine the specific difficulty of mobility disability based on mobind and mobdis
#data$mobdiff <- ifelse(data$mobind == 2,
#                       ifelse(data$diff10stepslm == 1 & data$diffquartlm == 1, 0,  # no mobility disability
#                             ifelse(data$diff10stepslm == 2 | data$diffquartlm == 2, 1,  # some mobility difficulty
#                                     ifelse(data$diffquartlm == 4, 2,  # Can't go a quarter mile
#                                          ifelse(data$diff10stepslm == 4, 3,  # Unable to climb 10 steps
#                                                   ifelse(data$quartmilelm == 4 & data$steps10lm == 4, 4, NA))))),  # Unable to complete simultaneously
#                       NA)

# 4. iadlind
# If both ltworklm and hvworklm are missing, iadlind = 0
# If ltworklm is not missing and hvworklm is missing or vice versa, iadlind = 1
# If neither ltworklm nor hvworklm is missing, iadlind = 2
data$iadlind <- ifelse(is.na(data$ltworklm) & is.na(data$hvworklm), 0,
                       ifelse(!is.na(data$ltworklm) & is.na(data$hvworklm) | 
                                is.na(data$ltworklm) & !is.na(data$hvworklm), 1, 2))

# 5. iadldis
# If iadlind = 2 and light or heavy housework fails, calculate iadldis based on the situation
data$iadldis <- ifelse(data$iadlind == 2,
                       ifelse(data$ltworklm == 1 & data$hvworklm == 0, 1,  # light housework disability
                              ifelse(data$ltworklm == 0 & data$hvworklm == 1, 2,  # heavy housework disability
                                     ifelse(data$ltworklm == 1 & data$hvworklm == 1, 3, 0))),  # both
                       NA)

# 6. iadldiff
# Determine the specific difficulty of IADL disability based on iadlind and iadldis
#data$iadldiff <- ifelse(data$iadlind == 2,
#                        ifelse(data$iadldis == 0, 0,  # 没有IADL disability
#                               ifelse(data$ltworklm == 1 & data$hvworklm == 0, 1,  # some light housework difficulty
#                                      ifelse(data$ltworklm == 2 | data$hvworklm == 2, 2,  # cannot handle light housework
#                                             ifelse(data$hvworklm == 3, 3,  # cannot do heavy housework
#                                                    ifelse(data$ltworklm == 3 & data$hvworklm == 3, 4, NA))))),  # Unable to complete simultaneously
#                        NA)


data$prescore <- with(data, 
                      ifelse(!is.na(prestrenexer) & !is.na(premodexer) & !is.na(premildexer),
                             (prestrenexer * 9) + (premodexer * 5) + (premildexer * 3), 
                             NA))

# Calculate posscore (post-CoVID exercise score)
# Use vigorous exercise, moderate exercise, light exercise count
data$posscore <- with(data, 
                      ifelse(!is.na(posstrenexer) & !is.na(posmodexer) & !is.na(posmildexer),
                             (posstrenexer * 9) + (posmodexer * 5) + (posmildexer * 3), 
                             NA))


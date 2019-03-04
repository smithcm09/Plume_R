library(readxl)
PlumeDerivedStatistics <- read_excel("Dropbox/Plume Paper/PlumeDerivedStatistics.xlsx", 
                                     col_types = c("text", "blank", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "blank", "blank", "blank", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "blank", "numeric", 
                                                   "numeric", "numeric"), skip = 2)
View(PlumeDerivedStatistics)

#Set Up Factor Variables
attach(PlumeDerivedStatistics)
CRF_factor <- as.factor(CRF_factor)
summary(CRF_factor)
is.factor(CRF_factor)

Factor_ltgAll <- as.factor(Factor_ltgAll)
summary(Factor_ltgAll)
is.factor(Factor_ltgAll)

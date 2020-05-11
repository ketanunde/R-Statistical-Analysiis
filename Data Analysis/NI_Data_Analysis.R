library(dplyr)
# Loading the dataset into csv file
complete_data <- read.csv("Complete_NI_Investment_data.csv", header = TRUE)
head(complete_data, 10)
# the Structure for the data frame
str(complete_data)
# Displaying the column names
colnames(complete_data)
# Removing some more columns
complete_data <- select(complete_data, c(business_name, 
                                         financial_year_of_offer,
                                         total_assistance_offered_by_invest_ni,
                                         total_investment_includes_investment_ni_assistance,
                                         jobs_to_be_created_assisted,
                                         country_of_ownership,
                                         SIC_sector))
complete_data
str(complete_data)          
# Converting the variable SIC_sector from facotr to character for the changes sector names
# changing the abbrevation for the sector name 
# for that the variable is need to change from factor to character 
# an after the change again changes from character to factor as it has 
# easy differentiate the SIC_sector with factors.
complete_data$SIC_sector <- as.character(complete_data$SIC_sector)

attach(complete_data)
complete_data$SIC_sector[complete_data$SIC_sector == "Accommodation & Food Service Activities"] <- "ACCOMODATION&FOODSERVICE"
complete_data$SIC_sector[complete_data$SIC_sector == "Admin & Support Service Activities"] <- "ADMIN&SUPPORTSERVICE"
complete_data$SIC_sector[complete_data$SIC_sector == "Agriculture, Forestry & Fishing"] <- "AGRI&FORESTRY&FISHING" 
complete_data$SIC_sector[complete_data$SIC_sector == "Arts, Entertainment & Recreation"] <- "ART&ENTER&RECREATION"
complete_data$SIC_sector[complete_data$SIC_sector == "Construction"] <- "CONST"
complete_data$SIC_sector[complete_data$SIC_sector == "Education"] <- "EDUC" 
complete_data$SIC_sector[complete_data$SIC_sector == "Electricity, Gas, Steam & Air Conditioning Supply"] <- "ELECTRICITYGAS&AIR"
complete_data$SIC_sector[complete_data$SIC_sector == "Financial & Insurance Activities"] <- "FINANCIAL&INSURANCE"
complete_data$SIC_sector[complete_data$SIC_sector == "Human & Social Work Activities"] <- "HUMAN&SOCIALWORK"
complete_data$SIC_sector[complete_data$SIC_sector == "Information & Communication"] <- "INFO&COMMU"
complete_data$SIC_sector[complete_data$SIC_sector == "Manufacturing"] <- "MANUFAC"
complete_data$SIC_sector[complete_data$SIC_sector == "Mining & Quarrying"] <- "MINING&QUERRY"
complete_data$SIC_sector[complete_data$SIC_sector == "Other Service Activities"] <- "OTHERSERVICE"
complete_data$SIC_sector[complete_data$SIC_sector == "Professional, Scientific & Technical Activities"] <- "PROFESSIONAL&SCI&TECH"
complete_data$SIC_sector[complete_data$SIC_sector == "Public Administration & Defence; Compulsory Social Security"] <- "PUBLICADMIN&DEFENCE"
complete_data$SIC_sector[complete_data$SIC_sector == "Real Estate Activities"] <- "REALESTATE"
complete_data$SIC_sector[complete_data$SIC_sector == "Transportation & Storage"] <- "TRANSPORT&STORAGE" 
complete_data$SIC_sector[complete_data$SIC_sector == "Water Supply, Sewerage, Waste Management & Remediation Activities"] <- "WATERSUPPLY&SEWERGE"
complete_data$SIC_sector[complete_data$SIC_sector == "Wholesale & Retail Trade & Repair of Vehicles"] <- "WHOLESALE&RETAIL"
complete_data$SIC_sector[complete_data$SIC_sector == "X Unknown"] <- "UNKNOWN"
detach(complete_data)    



str(complete_data)


Analysis_NI_Investment <- complete_data
Analysis_NI_Investment
# Table () is use to cross-classifying the factors based on the their levels
table(complete_data$SIC_sector)

############### For analysis we have to convert some of the variables in Numeric from factor
# so it is get easy while doing the noramlization and applying several test
# to find out the P-value.

# Converting the Total invetstment variable into numerical
Analysis_NI_Investment$SIC_sector <- as.factor(Analysis_NI_Investment$SIC_sector)
Analysis_NI_Investment$jobs_to_be_created_assisted <- as.factor(Analysis_NI_Investment$jobs_to_be_created_assisted)
Analysis_NI_Investment$jobs_to_be_created_assisted <- as.numeric(Analysis_NI_Investment$jobs_to_be_created_assisted)
# converted the variable from character to factor for identifying how it is chatagorized variable or 
# it is a continious variable
# Some vaiables are converted into numerical from character because they have the information 
# like the price or values which is numeric data.
Analysis_NI_Investment$total_assistance_offered_by_invest_ni <- as.numeric(Analysis_NI_Investment$total_assistance_offered_by_invest_ni)
Analysis_NI_Investment$total_investment_includes_investment_ni_assistance <- as.numeric(Analysis_NI_Investment$total_investment_includes_investment_ni_assistance)
Analysis_NI_Investment$financial_year_of_offer <- as.factor(Analysis_NI_Investment$financial_year_of_offer)

str(Analysis_NI_Investment)

############################### Histogram Representation Of Data###############################
# Parametric Test 
# The jobs Variable is around the Country and SIC sector is not symmetrically divided 
# So it is not normally distributed 

library("lattice")
a <- histogram(~jobs_to_be_created_assisted | country_of_ownership, data = complete_data)
a
b <- histogram(~total_investment_includes_investment_ni_assistance | financial_year_of_offer, data = complete_data)
b
c <- histogram(data = complete_data, ~total_investment_includes_investment_ni_assistance | SIC_sector)
c
###################################################################################################

library(ggplot2)
qplot(x =SIC_sector , y = jobs_to_be_created_assisted, data = complete_data)


# Jobs against the countries 
qplot(data = complete_data, country_of_ownership, jobs_to_be_created_assisted, geom = "line")


##################################Normality Test####################################
# Shapiro test for normality
# this test only take sample size of 5000 at a time 
# so we divide the data into two parts and then apply the test on it

normality_test <- shapiro.test(Analysis_NI_Investment$jobs_to_be_created_assisted[0:5000])
normality_test
normality_test$p.value

normality_test1 <- shapiro.test(complete_data$jobs_to_be_created_assisted[5001:10000])
normality_test1


with(Analysis_NI_Investment, tapply(jobs_to_be_created_assisted, country_of_ownership, shapiro.test[0:5000]))
# KS test for checking the normality
ks.test(Analysis_NI_Investment$jobs_to_be_created_assisted, "pnorm")


############################################################################################

data_numeric_var <- sapply(Analysis_NI_Investment, is.numeric)
data_numeric_var


data_file_adjusted <- complete_data[, data_numeric_var]
pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)


# Sience THe p value is less than 0.5 the it rejects the Null Hypothesis.
chisq.test(complete_data$jobs_to_be_created_assisted, correct = FALSE)

# Welch Two Sample T-Test
# which use two variable to define the p-value
t.test(Analysis_NI_Investment$jobs_to_be_created_assisted ~ Analysis_NI_Investment$total_investment_includes_investment_ni_assistance)

# With One Sample T-Test
# One tailed hypothesis testing
t.test(complete_data$jobs_to_be_created_assisted)



install.packages("lsr")
library(lsr)
cd <- cohensD(Analysis_NI_Investment$jobs_to_be_created_assisted, Analysis_NI_Investment$total_investment_includes_investment_ni_assistance,
        method = "raw")
cd



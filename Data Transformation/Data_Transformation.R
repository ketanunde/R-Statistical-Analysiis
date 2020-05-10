# Cleanning the data and preparing it for analysis
# Loading the Source file 
data_file <- read.csv("csv-open-data-file-updated-nov19.csv")
data_file
# Class which is the "data frame"
class(data_file)
# Showing the structure of the Data frame
str(data_file)

# To deal with any NA values we ehave to calculate the NA first 
# there are missing values but it does not display here as it was not filled with any value yet 
sum(is.na(data_file))

# There are 20 columns are avaiable in this source.
# From which some we have to remove because they are not in 
# any use while dealing the data.
colnames(data_file)

# Summary displays the Total content of each variables from the data frame
summary(data_file)

# With The table() function on Sectors to find out how many sectors are there
# showing how many sectors are there in this data 
# to find which is amongst them have been giving lot profit
table(data_file$SIC.Sector)

# To find if there is any missing values in data frame
sum(complete.cases(data_file))

###################### Changing the Column names #######################

# Changing the names of the Columns
# Some Columns are having extra characters and extra space in name so, 
# replace with "_" for better understanding
str(data_file)

attach(data_file)
colnames(data_file)[which(names(data_file) == "Business.Name")] <- "business_name"
colnames(data_file)[which(names(data_file) == "Financial.Year.Offer.Made")] <- "financial_year_of_offer"
colnames(data_file)[which(names(data_file) == "Approval.Date")] <- "approval_date"
colnames(data_file)[which(names(data_file) == "Total.Assistance.Offered.by.Invest.NI....")] <- "total_assistance_offered_by_invest_ni"
colnames(data_file)[which(names(data_file) == "Total.Investment..Includes.Invest.NI.Assistance...")] <- "total_investment_includes_investment_ni_assistance"
colnames(data_file)[which(names(data_file) == "Conditions.of.Offer")] <- "condition_of_offer"
colnames(data_file)[which(names(data_file) == "Jobs.to.be.Created..Assisted.")] <- "jobs_to_be_created_assisted"
colnames(data_file)[which(names(data_file) == "Country.of.Ownership")] <- "country_of_ownership"
colnames(data_file)[which(names(data_file) == "Ownership.when.the.offer.was.made")] <- "ownership_when_the_offer_made"
colnames(data_file)[which(names(data_file) == "New...Existing")] <- "new_or_existing"
colnames(data_file)[which(names(data_file) == "Constituency.in.which.business.was.located.when.offer.was.made")] <- "constituency_in_which_business_was_located_when_offer_made"
colnames(data_file)[which(names(data_file) == "Council.in.which.business.was.located.when.offer.was.made")] <- "council_in_which_business_located_when_offer_made"
colnames(data_file)[which(names(data_file) == "SIC.Code.5.Digit")] <- "SIC_code_5_digit"
colnames(data_file)[which(names(data_file) == "SIC.Sector")] <- "SIC_sector"
colnames(data_file)[which(names(data_file) == "Current.Address.Line.1")] <- "current_address_line_1"
colnames(data_file)[which(names(data_file) == "Current.Address.Line.2")] <- "current_address_line_2"
colnames(data_file)[which(names(data_file) == "Current.Address.Line.3")] <- "current_address_line_3"
colnames(data_file)[which(names(data_file) == "Current.Address.Line.4")] <- "current_address_line_4"
colnames(data_file)[which(names(data_file) == "Current.Address.Line.5")] <- "current_address_line_5"
colnames(data_file)[which(names(data_file) == "Current.Postcode")] <- "current_postcode"
detach(data_file)

# Number of rows in data frame
nrow(data_file)

# Structure before dealing with missing data
str(data_file)



# before doing the data removal we have to drop information who doesnt have business_name or sic_sector
# if there is any

data_file <- subset(data_file, !is.na(data_file$business_name))
data_file <- subset(data_file, !is.na(data_file$SIC_sector))
nrow(data_file)

########################## Dealing With Misssing Data#########################################

# Missing Data will be replace with NA first in some columns
# After inserting NA in missing values we have to decide on 
# how many NA are present in the data

# Remove the missing values with NA
# THe total number of NA in the Data frame are 2838

data_file$financial_year_of_offer[data_file$financial_year_of_offer == ""] <-- NA
data_file$total_assistance_offered_by_invest_ni[data_file$total_assistance_offered_by_invest_ni == ""] <-- NA
data_file$SIC_code_5_digit[data_file$SIC_code_5_digit == ""] <-- NA
data_file$current_address_line_1[data_file$current_address_line_1 == ""] <-- NA
data_file$current_address_line_2[data_file$current_address_line_2 == ""] <-- NA
data_file$current_address_line_3[data_file$current_address_line_3 == ""] <-- NA
data_file$current_address_line_4[data_file$current_address_line_4 == ""] <-- NA
data_file$current_address_line_5[data_file$current_address_line_5 == ""] <-- NA
data_file$current_postcode[data_file$current_postcode == ""] <-- NA

# The NA values from the data count
sum(is.na(data_file))
# There is 20040 records having NA values 
# Count for the numbers in which no NA present
sum(!is.na(data_file))
# So there are 177140 records are not having NA values 

# Data with having missing values incomplete data
sum(!complete.cases(data_file))
# the number of incomplete cases are 8771, which having NA in it.
missing_values_data <- data_file[!complete.cases(data_file),]
missing_values_data

# This is complte data which do not have any missing values
sum(complete.cases(data_file))
data_without_na <- data_file[complete.cases(data_file),]
data_without_na

# It is the complete data but it was just the 20% of whole data so we nedd more data from it.
# Thsts why need to remove some columns and have look how data is spread accross.
# Using VIM and MICE libraries the missing data will be displayed in 
# graphical format on graph so it is easy to decide which variable to have more NA inside.

# VIM library use for the displaying and dealing the Missing values

library(mice)
md.pattern(data_file)

# With the sortVars Attribute it gives the number of count in Each variable 
# who having the missing information or NA
# SO it shows that the vary lagre number of values are missing in address variable only
# so to remove those address column is an solution for dealing with those missing values

library(VIM)
missing_values <- aggr(data_file, prop = FALSE, numbers = TRUE, sortVars = TRUE)
# It is shown that some of the main variable which is required for analysis are having
# no missing values.
# so there is no reason to delete some data from that variable.
# Most of the data missing was from the address fields variable.
# and there no need for these variable further so better to delete them 
# to avoide any false result.
############################# Removing The Columns####################################

# Remove the other variables which do not having any of use
# keep variables which is use for analysis


# Structure before deleting some columns
str(data_file)


library(dplyr)
data_file_updated <- select(data_file, c(business_name, 
                                         financial_year_of_offer,
                                         total_assistance_offered_by_invest_ni,
                                         total_investment_includes_investment_ni_assistance,
                                         jobs_to_be_created_assisted,
                                         country_of_ownership,
                                         council_in_which_business_located_when_offer_made,
                                         SIC_code_5_digit,
                                         SIC_sector))
data_file_updated

# structure after dealing wih missing data and removing some columns
str(data_file_updated)
nrow(data_file_updated)
# There are 9859 records which is clean for analysis


############################ Creating the separate csv file of cleaned data######################

# After watching the data and also the Removing the missing data 
# now its clearly shown that there is no more missing data in the data frame 
# and also the NA are available only in Address and Postcode Column
# So the data is now clean and ready for the Written into csv file.

write.csv(data_file_updated, "Complete_NI_Investment_data.csv")

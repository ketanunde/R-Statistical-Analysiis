model_data <- read.csv("Data Analysis/Complete_NI_Investment_data.csv")
model_data


str(model_data)
# Converting the variable from Factor to Numericals for the analysis
model_data$total_investment_includes_investment_ni_assistance <- as.numeric(model_data$total_investment_includes_investment_ni_assistance)
model_data$SIC_sector <- as.numeric(model_data$SIC_sector)
model_data$jobs_to_be_created_assisted <- as.numeric(model_data$jobs_to_be_created_assisted)
model_data$country_of_ownership <- as.numeric(model_data$country_of_ownership)
str(model_data)

# Plot to check if their is any two variables are co-ordinated.
# Correlation plot
# plot(model_data[,])
colnames(model_data)
# Check the Correlation
cor(model_data[c(5, 6, 7, 10)])
# here there is no Multicollinerity issue, as all correlation value are less than 9 

scatter.smooth(y = model_data$jobs_to_be_created_assisted,
               x = model_data$SIC_sector,
               main = "Jobs~Sector")

# Model 1
# the model includes Jobs 
fit <- lm(jobs_to_be_created_assisted ~ country_of_ownership+SIC_sector+total_investment_includes_investment_ni_assistance, data = model_data)
fit

# More stars in the result that means more statistically significant that variable could be
# with our vaiable 
# More stars indicated that more stable varible for the model

summary(fit)


# The confidence interval of the model coefficient can be extracted as follow
confint(fit)

p1 <- predict(fit, model_data)
p1

plot(fit)
abline(fit)
attach(model_data)
plot(SIC_sector, country_of_ownership)
plot(jobs_to_be_created_assisted, SIC_sector)
plot(jobs_to_be_created_assisted, country_of_ownership)

# With this plot this show that there is some relation between 
# Country and investment assistance 
# So build the model on the basis of that 
# to get the best results

coplot(jobs_to_be_created_assisted~country_of_ownership|total_investment_includes_investment_ni_assistance, 
         panel= panel.smooth, model_data)
detach(model_data)

# Checking the Model Details
resid <- fit$residuals
hist(resid)
qqnorm(resid)
qqline(resid)


# For Reducing the Model :- 
# This might not be the best model so to check whether goes wrong we take some test and then 
# decide where and which variable to reduce or add in the model
library(MASS)
stepAIC(fit)

# Model Transformation 
# Model 2 with change lookning from the corplot and qqnorm plot
# Model Multiple Linear Regression
# The main variable for the model using is Jobs which we will predict
# The supplimentory variables are sic_secotrs, total investment and country




# The Model Plot which also represents the plots like
# Residual Vs Fitted
# Normal Q-Q
# Scale-Location 

plot(f1)

# Prediction of model 2
predict(f1)

# Confidence Intervals of model 2
confint(f1)
# 
stepAIC(f1)
AIC(fit)
AIC(f1)
# Histogram of model 2 with residual analysis
# Because of the low AIC and all significant variables used.
reside <- f1$residuals
hist(reside)



anova(fit, f1)

# Splitting data into Training And Testing Datasets
smp_size <- floor(0.70 * nrow(model_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)

train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]

sample <- sample(model_data, size= round(0.7 * model_data))

fit1 <- lm(jobs_to_be_created_assisted~ country_of_ownership*total_investment_includes_investment_ni_assistance*SIC_sector, data = train)
fit1
summary(fit1)
confint(fit1)

# Prediction Based on Sample data
predict(fit1, data.frame("country_of_ownership"= 26, "total_investment_includes_investment_ni_assistance"=5362,
                         "SIC_sector"= 2,"total_investment_includes_investment_ni_assistance"= 353522))

predicted_jobs <- predict(fit1, test)

actual_prediction <- data.frame(cbind(actuals= test$jobs_to_be_created_assisted, predicted= predicted_jobs))
head(actual_prediction, 10)


plot(actual_prediction)



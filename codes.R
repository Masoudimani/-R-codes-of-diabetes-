#colorectal

 library(haven)
 my <- read_sav("C:/Users/MI3/Desktop/MA T4/colorectal/Untitled3.sav")
 
 View(my)

numeric_cols <- sapply(my, is.numeric)
my[numeric_cols] <- lapply(my[numeric_cols], as.factor)

numeric_cols <- sapply(funf, is.numeric)
funf[numeric_cols] <- lapply(funf[numeric_cols], as.factor)
str(funf)
str(my)
my$Colorectal
my$asprin
my$diabetes  
my$agecat
my$stage
my$gradeofdysplasia
cleaned_data <- na.omit(cleaned_data)
mylogit <- glm(Colorectal ~  agecat + gender + asprin + methformin + insulin + 
                 sulfonylurea + diabetes + cigarette + alcohol  , data = my, family = "binomial")
mylogit <- glm(Colorectal ~  agecat + gender + asprin + methformin + insulin + 
                 sulfonylurea + diabetes + cigarette + alcohol  , data = my, family = "binomial")
names(my)

mylogit <- glm(casecontrol ~  Gender + asprin + metformin + Insulin + hypertension + losartan + agecat  + Cigarettes
               , data = my, family = "binomial")
names(my)

mylogit <- glm(casecontrol ~hypcat
               , data = my, family = "binomial")
summary(mylogit)

ci <- exp(confint(mylogit, level = 0.95));ci
di <- exp(coef(mylogit));di

dd = table(my$hypertension,my$gradeofdysplasia);dd
col_perc <- prop.table(table_data, dd = 2) * 100;col_perc

my$hypcat <- na.omit(my$hypcat)
my$d
my$gradeofdysplasia

weights

mylogit$coefficients

library(caret)

# Calculate variable importance
importance <- varImp(mylogit)
 print(mylogit)
 varImp(mylogit  )

?varImp











m2 <- glm(casecontrol ~  agecat + Gender + asprin + metformin + Insulin + 
                 Sulfonylurea + diabetes + Cigarettes + alcohol , data = funf, family = "binomial")

summary(mylogit)
summary(m2)
# Calculate leverage values
leverage1 <- hatvalues(m2)

# Find observations with high leverage values
high_leverage1 <- which(leverage1 > 2 * mean(leverage1))

cleaned_data1 <- funf[-high_leverage1, ]
length(cleaned_data1)
length(cleaned_data)
m2_cleand <- glm(casecontrol ~  agecat + Gender + asprin + metformin + Insulin + 
            Sulfonylurea + diabetes + Cigarettes + alcohol , data = cleaned_data1, family = "binomial")
summary(m2_cleand)
# Calculate leverage values
leverage <- hatvalues(mylogit)



# Find observations with high leverage values
high_leverage <- which(leverage > 2 * mean(leverage))

cleaned_data <- my[-high_leverage, ]
length(cleaned_data)
# Fit the logistic regression model again with the cleaned data
model_cleaned <- glm(Colorectal ~ agecat + gender + asprin + methformin + insulin + 
                       sulfonylurea + diabetes + insulin + cigarette + alcohol , data = cleaned_data, family = "binomial")
summary(model_cleaned)
write.csv(cleaned_data, "cleaned_data.csv")
install.packages("writexl")
writexl::write_xlsx(cleaned_data, "cleaned_data.csv")

# Get the confidence intervals for the odds ratios
ci <- exp(confint(model_cleaned, level = 0.95));ci
di <- exp(coef(model_cleaned));di
# Print the confidence intervals
print(ci)

summary(model_cleaned)


influential <- cooks.distance(mylogit)

# Find observations with high Cook's distance values
high_influential <- which(influential > 4 / length(my))

# Remove observations with high Cook's distance values
cleaned_data <- my[-high_influential, ]
length(cleaned_data)

# Fit the logistic regression model again with the cleaned data
model_cleaned1 <- glm(colorectal ~ agecat + gender + asprin + methformin + insulin + 
                        sulfonylurea + diabetes + insulin , data = cleaned_data, family = "binomial")

summary(model_cleaned1)

m1 <- glm(cleaned_data$diabetes ~ cleaned_data$distantmetastasis  , data = cleaned_data, family = "binomial")
m1 <- glm(cleaned_data$diabetes ~ cleaned_data$stage  , data = cleaned_data, family = "binomial")

m1 <- glm(cleaned_data$diabetes ~ cleaned_data$tumorlocation  , data = cleaned_data, family = "binomial")
summary(m1)
ci <- exp(confint(m1, level = 0.95));ci
di <- exp(coef(m1));di

library(MASS)
names(my)
model <- polr(gradeofdysplasia ~  diabetes ,Hess=TRUE, data = my)
summary(model)
ci <- exp(confint(model, level = 0.95));ci
di <- exp(coef(model));di
table(my$gradeofdysplasia,my$diabetes)
summary(model2)


coef_x1 <- coef(summary(model))["diabetes", "Estimate"]
se_x1 <- coef(summary(model))["x1", "Std. Error"]

# Calculate Cohen's d for x1
cohen_d_x1 <- coef_x1 / se_x1


library(nnet)
names(my)
model <- multinom(tumorlocation ~ diabetes, data = my)
summary(model)

ci <- exp(confint(model, level = 0.95));ci
di <- exp(coef(model));di

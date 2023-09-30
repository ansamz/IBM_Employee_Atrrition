#Import libraries
library(dplyr)
library(multcomp)

data <- read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(data)
str(data)
colnames(data)

#convert the attrition to 0 and 1 except for yes and no in order to do a linear model
data <- data %>%
  mutate(Attrition = ifelse(Attrition == "No",0,1))

levels(data$Department)
unique(data$Department)

# Define the levels 
data$Department <- factor(data$Department, levels = c("Sales", "Research & Development", "Human Resources"))
levels(data$Department)

data$Gender <- factor(data$Gender, levels = c("Female", "Male"))

lm.ibm <- lm(formula = Attrition ~ YearsAtCompany + Education + Gender + Department, data = data)
coef(lm.ibm)

summary(lm.ibm)

# Intercept: when all predictor variables are zero the attrition value is 0.278149.
# now here what we are trying to predict is yes 1 or no zero, can we say that because it's closer to zero then it's a no?

# YearsAtCompany: A one-unit increase in YearsAtCompany is associated with a decrease in the log-odds of Attrition by approximately -0.0081.
# while the rest of the factors have not changed

# Education: Education doesn't have a significant effect on Attrition, as the p-value (0.368913) is greater than 0.05.

# GenderMale: Being male (GenderMale = 1) is associated with an increase in the log-odds of Attrition by approximately 0.0202 
# but this effect is not statistically significant (p-value = 0.29788).

# DepartmentResearch & Development: Belonging to the Research & Development department is associated with a decrease in the log-odds of Attrition by approximately -0.0722. 
# This effect is statistically significant (p-value = 0.000549).

# DepartmentHuman Resources: Belonging to the Human Resources department doesn't have a significant effect on Attrition, as the p-value (0.712443) is greater than 0.05.

# Model Summary: The model has a low R-squared value (Multiple R-squared = 0.02745), indicating that the predictors explain only a small portion of the variability in Attrition.

# Overall Model Significance: The F-statistic (8.264) is associated with a low p-value (1.035e-07), indicating that at least one predictor variable in the model is significantly related to Attrition.


# I think linear model is not the best approach for this dataset and I would prefer to go with logistic  regression since we are trying to classify 0 or 1

# # To test these two predictors we can use the drop1() function.
drop1(lm.ibm, test = "F")

# The results show that removing "YearsAtCompany" from the model would result in a significant decrease in model fit, as indicated by the low p-value (2.076e-07).


glht.1 <- glht(lm.ibm, linfct = mcp(Department = "Tukey"))
summary(glht.1)
 
# Research & Development employees have a significantly lower Attrition rate compared to Sales employees (p-value = 0.00149).
# Attrition rates in the Human Resources department are not significantly different from those in the Sales department (p-value = 0.92391).
# There's no significant difference in Attrition rates between the Human Resources and Research & Development departments (p-value = 0.46948).

glht.2 <- glht(lm.ibm, linfct = mcp(Gender = "Tukey"))
summary(glht.2)

# There's no significant difference in Attrition rates between males and females (p-value = 0.298).


# Logostoc regression:

logistic_model <- glm(formula = Attrition ~ YearsAtCompany + Education + Gender + Department, family = binomial, data = data)

# Step 4: Analyze the model
summary(logistic_model)


# The intercept represents the estimated log-odds of Attrition when all predictor variables are zero. In this case, it's approximately -0.73338. 
# The negative sign suggests a lower likelihood of attrition when all other predictors are zero.

# YearsAtCompany: A one-unit increase in "YearsAtCompany" is associated with a decrease in the log-odds of Attrition by approximately -0.08177. 
# This means that as employees spend more years at the company, the log-odds of attrition decrease.

# Education doesn't have a significant effect on Attrition, as indicated by the p-value (0.381539), which is greater than 0.05.

# GenderMale: Being male (GenderMale = 1) is associated with an increase in the log-odds of Attrition by approximately 0.15118, 
# but this effect is not statistically significant (p-value = 0.310601).

# DepartmentResearch & Development: Belonging to the Research & Development department is associated with a decrease in the log-odds of Attrition by approximately -0.52866. 
# This effect is statistically significant (p-value = 0.000504).

# DepartmentHuman Resources: Belonging to the Human Resources department doesn't have a significant effect on Attrition, as indicated by the p-value (0.715088).
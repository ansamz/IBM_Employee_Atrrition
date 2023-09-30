# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(tidyr)
library(cowplot)
library(forcats)
library(plotrix)

# Load the dataset (replace 'dataset.csv' with your actual file path)
data <- read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
dim(data)

str(data)
head(data)

# Summary statistics
summary(data)

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values
# no missing values yay

# Distribution of the target variable (Attrition)
ggplot(data, aes(x = Attrition)) +
  geom_bar(fill = "#7393B3") +
  labs(title = "Attrition Distribution")
#we can see from the plot that what we are trying to predict is imbalanced


# Box plot for numeric features
# here the features have a wide range of values and a lot of them are diminished 

numeric_features <- data %>%
  select_if(is.numeric)
numeric_features %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  labs(title = "Numeric Features")

# that's why I applied log to see them better
data2 <- data %>%
  mutate(across(where(is.numeric), ~log(.)))
numeric_features2 <- data2 %>%
  select_if(is.numeric)
numeric_features2 %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "#7393B3") +
  labs(title = "Numeric Features")


# Correlation heatmap for numeric features
correlation_matrix <- cor(numeric_features)
corrplot(correlation_matrix, method = "color")


###################################################################
# Let's look closely at the distribution of the Age of our employees
# Why? Unlike the older generation, millenials tend to switch workplaces more and thus that could
# be an explanation of why we have the current levels of attrition
options(repr.plot.width=8, repr.plot.height=6) 

dat_text <- data.frame(
  label = c("Mean = 37.33 \n Years Old", "Mean = 36.65 \n Years Old"),
  Gender   = c("Female", "Male")
)

gender.dist <- data %>%
  select(Gender, Age) %>%
  filter(Gender == 'Male' | Gender == 'Female') %>%
  filter(!is.na(Age)) %>%
  group_by(Gender) %>%
  ggplot(aes(x = Age)) +
  geom_density(aes(fill = Gender), alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Gender) +
  theme_minimal() +
  geom_vline(aes(xintercept = mean(Age)),
             color = "red", linetype = "dashed", linewidth = 1) +  # Use 'linewidth' here
  labs(title = "Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#F781F3", "#819FF7")) +
  geom_text(
    data = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust = -0.1,
    vjust = -1
  )


overall.dist <- data %>% select(Gender, Age) %>% filter(!is.na(Age)) %>% 
  ggplot(data=data, mapping=aes(x=Age)) + geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) +  theme_minimal() + labs(x="Overall Age") + 
  annotate("text", label = "Mean = 36.92 Years Old", x = 50, y = 0.03, color = "black")


plot_grid(gender.dist, overall.dist, nrow=2)


##############################
# Monthly Income by Gender
p <- ggplot(data, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot() + 
  scale_fill_manual(values=c("#F5A9F2", "#5882FA")) + scale_color_manual(values=c("#FE2EF7", "#5858FA")) +
  coord_flip() + labs(title="Are there any Gender Disparities in Income?")

p


#########################
#Attrition by Educational Level:
options(repr.plot.width=8, repr.plot.height=4) 

# Give names for the different education levels.
data$Educational_Levels <-  ifelse(data$Education == 1, "Without College D.",
                                 ifelse(data$Education == 2 , "College D.",
                                        ifelse(data$Education == 3, "Bachelors D.",
                                               ifelse(data$Education == 4, "Masters D.", "Phd D."))))

# I want to know in terms of proportions if we are loosing key talent here.
# Create the plot
edu.level <- data %>%
  select(Educational_Levels, Attrition) %>%
  group_by(Educational_Levels, Attrition) %>%
  summarize(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = fct_reorder(Educational_Levels, n), y = n, fill = Attrition, color = Attrition)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Attrition) +
  coord_flip() +
  scale_fill_manual(values = c("#2EF688", "#F63A2E")) +
  scale_color_manual(values = c("#09C873", "#DD1509")) +
  geom_label(aes(label = n, fill = Attrition), colour = "white", fontface = "italic") +
  labs(x = "", y = "Number of Employees", title = "Attrition by Educational Level") +
  theme_minimal() +  # Use the built-in "theme_minimal"
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14))

edu.level


#Attrition by Job Role
# The Funnel with the Attrition Rates by Job Role
options(repr.plot.width=10, repr.plot.height=6) 
attr.job <- data %>%
  select(JobRole, Attrition) %>%
  group_by(JobRole, Attrition) %>%
  summarize(amount = n(), .groups = 'drop') %>%
  mutate(pct = round(prop.table(amount), 2) * 100) %>%
  arrange(pct) %>%
  ungroup()

nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole) 
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), main = "Attrition by Job Role", 
                       gap=10, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(9)))

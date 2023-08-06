#Required libraries
library(dplyr)
library(glmnet)
library(ggplot2)
library(DescTools)
library(corrplot)
library(pROC)
library(aod)
library(countrycode)
library(gridExtra)
library(caret)

df <- read.csv("Life Expectancy Data.csv")
str(df)
#2 categorical variables - country and status
#Data Cleaning
#the column thinness..1.19.years is rate of thinness among people aged 10-19. Hence, renaming the column
df <- df %>%
  rename("thinness.10.19.years" = "thinness..1.19.years")
colnames(df)

#summary of df
summary(df)

#checking for null values
check_null <- function(df){
  sapply(df, function(x) sum(is.na(x)))
}

check_null(df)
         
#percentage of missing values
(colSums(is.na(df))/nrow(df))*100
#We can see that there are missing values, but the no. of missing values is not large enough to remove
#the columns. Hence, impute the missing values

#the target variable has 10 missing values, so will remove the rows with missing values. For the rest, I perform imputation
nrow(df)
df <- df[!is.na(df$Life.expectancy),]
nrow(df)

check_null(df)
#since it is time-series data it is better to impute missing values based on the values from the same year.
cols_to_impute <- c("Alcohol", "Hepatitis.B", "BMI", "Polio", "Total.expenditure", "Diphtheria", 
                    "GDP", "Population", "thinness.10.19.years", "thinness.5.9.years", 
                    "Income.composition.of.resources", "Schooling") 
par(mfrow=c(3,4))
# loop through the columns and create a histogram for each
for (i in 1:length(cols_to_impute)) {
  hist(df[,cols_to_impute[i]], main=cols_to_impute[i])
}

#will impute using median because the median is not sensitive to outliers and most of the columns' distributions are skewed
df_imputed <- df %>%
  group_by(Year) %>%
  mutate(across(cols_to_impute, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

check_null(df_imputed)

#converting df_imputed from tibble to dataframe
df_imputed <- data.frame(df_imputed)

#again check the distribution to see if the distributions of the columns are not changed drastically
for (i in 1:length(cols_to_impute)) {
  hist(df_imputed[,cols_to_impute[i]], main=cols_to_impute[i])
}
par(mfrow=c(1,1))

#Outlier Detection
#Subset only numeric columns
numeric_cols <- sapply(df_imputed, is.numeric)
df_numeric <- df_imputed[, numeric_cols]
colnames(df_numeric)
df_numeric <- df_numeric[,-1]#removing year column

#Boxplot
#Close any open graphics devices
dev.off()
#Open a new graphics device
dev.new()

#Create a list of ggplot objects for each column
plots_list <- lapply(names(df_numeric), function(col){
  ggplot(df_numeric, aes(x = "", y = df_numeric[[col]])) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col))
})

#Create a grid of plots with 4 columns per row
grid.arrange(grobs = plots_list, ncol = 4)

#Infant.Deaths represent no. of infant deaths per 1,000 population. 
#That is why the number beyond 1000 is unrealistic. 
#removing rows where values are greater than 1000 for measles, infant death, 
#and under.five.deaths columns
df_imputed <- df_imputed[df_imputed$infant.deaths <= 1000 & 
                           df_imputed$Measles <= 1000 & 
                           df_imputed$under.five.deaths <= 1000, ]

summary(df_imputed[,c("infant.deaths","Measles","under.five.deaths")])

#The BMI values are very unrealistic because the value plus 40 is considered 
#extreme obesity. The median is over 40 and some countries have an average of 
#around 60 which is not possible. Hence, I will delete this entire column as it has majorly redundant data.
df_imputed <- df_imputed[, -11]
colnames(df_imputed)

#Treating the rest of the outliers using Winsorize method
cols <- c("Life.expectancy","Adult.Mortality", "Alcohol", 
             "Measles", "under.five.deaths","Total.expenditure",
             "thinness.10.19.years", "thinness.5.9.years",
             "Schooling", "Income.composition.of.resources")

df_imputed[cols] <- lapply(df_imputed[cols], Winsorize, 
                              probs = c(0.05,0.95))

df_imputed$Diphtheria <- Winsorize(df_imputed$Diphtheria, probs = c(0.11,0.89))

df_imputed$Polio <- Winsorize(df_imputed$Polio, probs = c(0.13, 0.87))

df_imputed[c("HIV.AIDS", "Hepatitis.B")] <- lapply(df_imputed[c("HIV.AIDS", 
                                                                "Hepatitis.B")],
                                                   Winsorize, probs=c(0.2, 0.8))

df_imputed[c("GDP", "percentage.expenditure", "Population")] <- lapply(
  df_imputed[c("GDP", "percentage.expenditure", "Population")], Winsorize, 
  probs=c(0.15, 0.85))

#again plotting the boxplot
# Close any open graphics devices
dev.off()
# Open a new graphics device
dev.new()

# Creating a list of ggplot objects for each column
plots_list <- lapply(names(df_imputed)[sapply(df_imputed, is.numeric)], 
                     function(col){
  ggplot(df_imputed, aes(x = "", y = df_imputed[[col]])) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col))
})

# Creating a grid of plots with 4 columns per row
grid.arrange(grobs = plots_list, ncol = 4)

#EDA & Visualizations
#Let's observe the distribution of life expectancy
hist(df_imputed$Life.expectancy, main = "Distribution of Life Expectancy",
     xlab = "Life Expectancy", ylim = c(0,400))
#the distribution is left-skewed

#Top 10 countries based on life expectancy
#a new dataframe with the top 10 countries by life expectancy
df_top10 <- df_imputed %>% 
  select(Country, Life.expectancy) %>% 
  group_by(Country) %>% 
  summarise(Life.expectancy = median(Life.expectancy, na.rm = TRUE)) %>% 
  top_n(10, Life.expectancy) %>% 
  arrange(Life.expectancy)

#scatter plot
ggplot(df_top10, aes(x = Country, y = Life.expectancy)) +
  geom_point(size = 3, color = "dodgerblue") +
  ggtitle("Top 10 Countries with the Highest Life Expectancy") +
  labs(x = "Country", y = "Life Expectancy") +
  scale_x_discrete(limits = df_top10$Country)

#Which country had the highest life expectancy each year
# Group the data by year and find the country with the highest life expectancy for each year
highest_life_exp <- df_imputed %>% 
  group_by(Year) %>% 
  slice(which.max(Life.expectancy)) 

# Create a bar plot of the country with the highest life expectancy for each year
ggplot(highest_life_exp, aes(x = Year, y = Life.expectancy, fill = Country)) +
  geom_col(position = "dodge") +
  labs(title = "Country with the highest life expectancy by year", 
       x = "Year", y = "Life Expectancy", fill = "Country")
#Iceland had the highest life expectancy in 2000 & 2002, Switzerland in 2001,
#Australia in 2003, 2004, 2012, 2013, 2014, & 2015, Italy in the year 2005, Canada in 2006 & 2007,
#Austria in 2008, 2010, & 2011

#Pie chart to find the distribution of status among the countries represented
# Creating a table with the counts of each status
status_table <- table(df_imputed$Status)

# Creating a dataframe with the counts and percentages
df_status <- data.frame(Status = names(status_table),
                        Count = as.numeric(status_table),
                        Percentage = round((as.numeric(status_table)/sum(status_table)*100),2))

#pie chart
ggplot(df_status, aes(x = "", y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))
#We can observe that 80.19% of the countries are developing countries and only a few are developed countries (19.81%)

#Which countries, developed or developing, has more life expectancy
# Categorize the countries into developing and developed
df_sub <- df_imputed %>%
  mutate(country_status = ifelse(Status == "Developed", "Developed", "Developing"))

# Calculate the average life expectancy by year and country status
df_avg_life <- df_sub %>%
  group_by(Year, country_status) %>%
  summarise(avg_life_exp = mean(Life.expectancy, na.rm = TRUE))

# Plot the average life expectancy by year and country status
ggplot(df_avg_life, aes(x = Year, y = avg_life_exp, color = country_status)) +
  geom_line() +
  ggtitle("Average Life Expectancy of Developing and Developed Countries") +
  xlab("Year") +
  ylab("Average Life Expectancy")
#We can see from the graph that developed countries have more life expectancy than in developing countries.

#correlation matrix
corrplot(cor(df_imputed[,-c(1,3)]),method="number", number.cex = 0.5)
corrplot(cor(df_imputed[,-c(1,3)]))
#We can observe that Life expectancy is highly correlated to Adults.Mortality, 
#HIV/AIDS, Income composition of resources, schooling, thinness 10.19.years, 
#thinness 5.9.years
#won't pass year as input it's irrelevant and also checking it's correlation
cor(df_imputed$Year, df_imputed$Life.expectancy) 
#no correlation so remove year column
new_df <- df_imputed[,-2]
write.csv(new_df, file = "after_cleaning_data.csv")
         
################### Feature Selection Using Lasso Regression #############################
y <- new_df$Life.expectancy
# Scale the data - because lasso is sensitive to the scale of the features
num_cols <- sapply(new_df, is.numeric)
new_df_num <- new_df[, num_cols]
scaled_df <- as.data.frame(scale(new_df_num[, -1]))

# Perform Lasso feature selection using 10-fold cross-validation
set.seed(0)
lasso_model <- cv.glmnet(as.matrix(scaled_df), y, alpha = 1, nfolds = 10)
lambda_opt <- lasso_model$lambda.min #optimized lambda value
lasso_fit <- glmnet(as.matrix(scaled_df), y, alpha = 1, lambda = lambda_opt)
# Get coefficient values and ranks
coef_vals <- coef(lasso_fit)[-1]
names(coef_vals) <-  colnames(scaled_df)
coef_ranks <- order(abs(coef_vals), decreasing = TRUE)
# Select top 7 features
selected_Features <- names(coef_vals)[coef_ranks][1:7]
selected_Features

new_df <- new_df[c("Country", "Status", selected_Features, "Life.expectancy")]
head(new_df)

########################## Hypothesis testing #####################################
colnames(new_df)

#1. What is the impact of schooling on the lifespan of humans?
plot(new_df$Schooling, new_df$Life.expectancy, 
     main = "Scatterplot of Schooling vs Life expectancy",
     xlab = "Schooling (in years)",
     ylab = "Life Expectancy(in years)")
#Form -> linear, direction -> positively associated, strength -> strongly correlated
#numeric measure of strength -> correlation coefficient
cor(new_df$Schooling, new_df$Life.expectancy) 
#76.86% -> strongly correlated

#Simple Linear Regression
#H0: There is no linear association between no. of years of schooling and lifespan 
#of humans (ß1=0)
#Ha: There is a linear association between no. of years of schooling and lifespan 
#of humans(ß1 != 0)
#alpha = 0.05
#t-test
n <- nrow(new_df)
#t_critical value
t_critical <- qt(0.05/2, df=n-2, lower.tail = FALSE)
t_critical
#Decision Rule: If |t| >= t_critical -> reject H0. Otherwise, fail to reject H0
model1 <- lm(new_df$Life.expectancy ~ new_df$Schooling, data=new_df)
summary(model1)
pt(58.87, df=n-2, lower.tail = F)*2
#t=58.87 
#since t > critical value, we reject H0. 
#We have significant evidence at the alpha=0.05 level that there is a linear 
#association between no. of years of schooling and lifespan of humans
abline(model1)
#In this case, the estimated value of 2.3547 suggests that on average, 
#for each additional year of schooling, the life expectancy increases by 
#approximately 2.35 years the expected value of life expectancy is 41.18 when a person has no years of schooling
#computing R^2 value
r_squared <- (cor(new_df$Schooling, new_df$Life.expectancy)**2)
r_squared
#59.07% of the variability in the life expectancy of humans can be explained by the no. 
#of years of schooling

#2. Is there a significant relationship between adult mortality rate and life 
#expectancy?
plot(new_df$Adult.Mortality, new_df$Life.expectancy,
     main = "Scatterplot of Adult Mortality vs Life Expectancy",
     xlab = "Adult Mortality (per 1000 population)",
     ylab = "Life Expectancy (years)")
#Form->linear, direction->negatively associated, strength->strong correlation
#Correlation coefficient
cor(new_df$Adult.Mortality, new_df$Life.expectancy)
#-0.707 -> strong negative correlation between adult mortality rate and life 
#expectancy

#H0:There is no linear association between adult mortality rate (per 1000 population)
#and life expectancy(in years)
#Ha: There is a linear association between adult mortality rate (per 1000 population)
#and life expectancy(in years)
#alpha=0.05
#F-test 
#critical value
qf(0.05, df1 = 1, df2 = n-2, lower.tail = FALSE)
#critical value = 3.8453
#Decision Rule: if F >= 3.8453 -> reject H0. Otherwise, fail to reject H0.
model2 <- lm(new_df$Life.expectancy ~ new_df$Adult.Mortality, data=new_df)
anova(model2)
#F=2404
#Since F>critical value, we reject H0.
#That is, we have significant evidence at alpha=0.05 level that there is a linear
#association between adult mortality rate (per 1000 population) and life expectancy
#(in years)
abline(model2)
#R-squared
r_squared <- (cor(new_df$Adult.Mortality, new_df$Life.expectancy)**2)
r_squared
#About 50% of the variability in the life expectancy of humans can be explained by the 
#adult mortality rate
summary(model2)
#For a one-unit increase in adult mortality rate, the predicted value of life 
#expectancy decreases by 0.05678 years

#Multiple Linear Regression
#3. How do various health indicators (HIV/AIDS, Diphtheria, adult mortality, under
#five deaths, and thinness 5-9years) affect life expectancy?
pairs(new_df[,-c(1,2)])
#from the pair plot we can say that HIV_AIDS and Diphtheria have no association with life expectancy. Hence, we won't include them
#To address this question we have to perform multiple linear regression
model3 <- lm(new_df$Life.expectancy ~ 
               new_df$Adult.Mortality +
               new_df$thinness.5.9.years +
               new_df$under.five.deaths, data = new_df)
summary(model3)

#H0: ßam = ßth = ßun = 0 (adult mortality rate, rate of thinness (5-9 years of age),
# and under-five deaths) are not significant predictors
#Ha: at least one of the slope coefficients is different than 0; that is,
#adult mortality rate and/or rate of thinness and/or under-five deaths are
#significant predictors/is a significant predictor of life expectancy)
#alpha=0.05
#Global F-test
df1<-3
df2<-n-3-1
#critical_value
qf(0.05, df1 = df1, df2 = df2, lower.tail = FALSE)
#Decision Rule:Reject H0, if F>= 2.60861. Otherwise, fail to reject H0
#F=1570 - from the summary table
#since F > critical we reject H0. That is, there is significance
#evidence at alpha=0.05 level that at least one of the predictors 
#(adult mortality, thinness rate, or under-five deaths) 
#is linearly associated with life expectancy
#Individual t-test
#For Adult mortality rate:
#H0: ßam = 0 (after controlling for thinness.5.9.years and under.five.deaths)
#Ha: ßam != 0 (after controlling for thinness.5.9.years and under.five.deaths)
#alpha=0.05
qt(0.05/2, df=n-3-1, lower.tail = F)
#Decision Rule: Reject H0 if |t| >= 1.96095, otherwise fail to reject H0
t <- -0.03830/0.00110 
t
#Since, |t| > critical, we reject H0. That is, we have significant evidence at
#alpha=0.05 level that there is a linear association between adult mortality rate
#and life expectancy after controlling for thinness.5.9.years and under.five.deaths

#Similarly, for thinness rate:
#H0: ßth = 0 (after controlling for adult.mortality and under.five.deaths)
#Ha: ßth!= 0 (after controlling for adult.mortality and under.five.deaths)
#alpha=0.05
qt(0.05/2, df=n-3-1, lower.tail = F)
#Decision Rule: Reject H0 if |t| >= 1.96095, otherwise fail to reject H0
t <- -0.78083/0.03586 #from the summary table or -21.8 directly from the table
t
#Since, |t| > critical, we reject H0. That is, we have significant evidence at
#alpha=0.05 level that there is a linear association between thinness among
#children aged 5-9 years and life expectancy after controlling for
#adult.mortality and under.five.deaths

#Similarly, for under five deaths:
#H0: ßun = 0 (after controlling for adult.mortality and thinness.5.9.years)
#Ha: ßun!= 0 (after controlling for adult.mortality and thinness.5.9.years)
#alpha=0.05
qt(0.05/2, df=n-3-1, lower.tail = F)
#Decision Rule: Reject H0 if |t| >= 1.96095, otherwise fail to reject H0
t <- -0.10007/0.00512 #from the summary table or -19.6 directly from the table
t
#Since, |t| > critical, we reject H0. That is, we have significant evidence at
#alpha=0.05 level that there is a linear association between the number of deaths of 
#children under five years of age per 1000 population and life expectancy after 
#controlling for adult.mortality and thinness.5.9.years

#interpretation
#For every one-unit increase in the adult mortality rate, the life 
#expectancy decreases by an average of 0.038 years, holding all other 
#predictors constant.

#For every one-unit increase in the percentage of thinness among children aged
#5-9 years, the life expectancy decreases by an average of 0.7808 years,
#holding all other predictors constant.

#For every one-unit increase in the under-five deaths, the life expectancy 
#decreases by an average of 0.1001 years, holding all other predictors 
#constant.

#adj.R^2 = 66.2% of the variation in the life expectancy is explained 
#by the multiple regression model.

####To perform one-way ANOVA
###introducing a continent variable
unique(new_df$Country)

# Vector of country names
countries <- unique(new_df$Country)

# Add continent codes to data frame
new_df <- new_df %>% 
  mutate(continent = countrycode(sourcevar = Country, origin = "country.name", destination = "continent"))

unique(new_df$continent)

#visualize
new_df %>% count(continent)
df_counts <- new_df %>%
  group_by(continent) %>%
  count()
ggplot(df_counts, aes(x = continent, y = n)) +
  geom_bar(stat = "identity")
#most of the countries belong to Africa continent

#numerical summary of life expectancy for each continent (group)
aggregate(new_df$Life.expectancy, by=list(new_df$continent), summary)

# Create a boxplot of life expectancy by continent
ggplot(new_df, aes(x = continent, y = Life.expectancy)) + 
  geom_boxplot() +
  labs(title = "Life Expectancy by Continent", x = "Continent", y = "Life Expectancy")

#check if the continent is a factor variable
is.factor(new_df$continent)
new_df$continent <- factor(new_df$continent)
str(new_df)

#4. Is there a significant difference in the mean life expectancy among different continents?
#H0: there is no significant difference in the mean life expectancy 
#among different continents (µafrica = µamer = µasia = µeur = µoce)
#Ha: There is a significant difference in the mean life expectancy among
#at least two of the continents (µi != µj for some i and j)
#alpha=0.05
#Global test
#Decision Rule: Reject H0 if F>=F_critical. Otherwise, fail to reject H0
k = 5
n = nrow(new_df)
df1 = k-1
df2 = n-k
#critical value
qf(0.05, df1 = df1, df2 = df2, lower.tail = FALSE)
model4 <- aov(new_df$Life.expectancy ~ new_df$continent, data = new_df)
summary(model4)
#F=780 > critical value. Hence, reject H0. That is, the model is significant.
#We have significant evidence at the α=0.05 level that there is a difference in 
#life expectancy between different continents

#Tukeys test - pairwise t-test
TukeyHSD(model4)

#We have significant evidence at alpha=0.05 that the mean life expectancies
#are different between America and Africa, Asia and Africa,
#Europe-Africa, Oceania-Africa, Asia-Americas, Europe-Americas, 
#Oceania-Americas, Europe-Asia, and Oceania-Europe groups 
#We don't have significant evidence at alpha=0.05 that the mean life expectancy
#are different between Oceania-Asia groups as p>0.05

#Creating dummy variables and re-doing one-way ANOVA #Oceania as a reference group
new_df$Americas <- ifelse(new_df$continent == "Americas", 1, 0)
new_df$Africa <- ifelse(new_df$continent == "Africa", 1, 0)
new_df$Europe <- ifelse(new_df$continent == "Europe", 1, 0)
new_df$Asia <- ifelse(new_df$continent == "Asia", 1, 0)
#one-way anova using lm function
model5 <- lm(new_df$Life.expectancy ~ new_df$Americas  + new_df$Africa +
               new_df$Europe + new_df$Asia, data = new_df)
summary(model5)
# We can observe the same results as before. The f-value and p-value are the same as 
#before.While the Oceania-America, Oceania-Africa, and Oceania-Europe groups are significant, Oceania-Asia is not.

#interpretation of the beta estimates
#ß0=71.571 -> mean life expectancy in Oceania(reference group)
#ß1=1.919 -> mean difference in life expectancy between Americas and Oceania groups
#ß2=11.112 -> mean difference in life expectancy between Africa and Oceania groups
#ß3=5.534 -> mean difference in life expectancy between Europe-Oceania groups
#ß4=0.514 -> mean difference in life expectancy between Asia-Oceania groups

###################################################################################
#Logistic Regression
min(new_df$Life.expectancy)
max(new_df$Life.expectancy)
#to perform logistic regression, I will convert life expectancy column to
#a binary dependent variable. A new variable called "class" is created
#which takes value 1 if life expectancy is > 70 (long-life) else 0
df_log <- new_df[,-c(12,13,14,15)] #removing dummy variables created
df_log$class <- ifelse(new_df$Life.expectancy > 70, 1, 0)

#5.Exploring the relationship between no. of deaths of children under five years 
#of age per 1000 population and the likelihood of having a long-life
colnames(df_log)
#split the dataset into train and test
set.seed(0) # for reproducibility
#split the dataset into 80/20 train-test split
train_index <- createDataPartition(df_log$class, p = 0.8, list = FALSE)
train <- df_log[train_index, ]
test <- df_log[-train_index, ]

# Fit model on a training set
model6 <- glm(class ~ under.five.deaths, data = train, 
             family = binomial)
# Summary of model
summary(model6)
# Evaluate performance on the test set
#c-statistics
predictions <- predict(model6, newdata = test, type = "response")
# Convert the probabilities to binary predictions
test_results <- ifelse(predictions > 0.5, 1, 0)
#test_results <- data.frame(actual = test$class, predicted = round(predictions))
confusionMatrix(factor(test_results), factor(test$class))
#accuracy = 71.3%
#ROC curve
roc_curve <- roc(test$class, test_results)
auc(roc_curve)
plot(roc_curve)
#AUC is 0.665

#Hypothesis testing
#H0: ß1=0 (there is no association between and no. of deaths of children under five
#years of age likelihood of having a long life)
#Ha: ß1!=0 (there is an association between no. of deaths of children under five
#years of age likelihood of having a long life)
#alpha=0.05
#z-test
#critical_value
qnorm(0.05/2, lower.tail = FALSE)
#Decision Rule: if |z| >=1.96, reject H0. Otherwise, Fail to reject H0
z<- -0.05614/0.00368 #from the table
z  #-15.2554
#since |z|>critical value and p-value (from the table) < 0.05, we reject H0. 
#We have significant evidence at alpha=0.05 level that there is an association 
#between the number of deaths of children under five years of age and the likelihood of 
#having a long-life odds ratio
exp(-0.05614)
#For every one-unit increase in the number of deaths of children under five years of
#age, the odds of having a long-life decrease by a factor of 0.9454 

#6.Interested in predicting whether the person will live long or not from 
#status, adult mortality rate, and income composition of 
#resources (changing status to 0s and 1s as it's categorical)
df_log$Status <- ifelse(df_log$Status == "Developed", 1, 0)
# Set the seed for reproducibility
set.seed(0)
# Create the training and testing sets with 80/20 ratio
trainIndex <- createDataPartition(df_log$class, p = 0.8, list = FALSE)
train <- df_log[trainIndex, ]
test <- df_log[-trainIndex, ]
model7 <- glm(class ~ Status + Adult.Mortality + 
                Income.composition.of.resources, data = train, family = binomial)
summary(model7)
# Make predictions on the test set
test_results <- predict(model7, newdata = test, type = "response")
# Convert the probabilities to binary predictions
test_results <- ifelse(test_results > 0.5, 1, 0)
confusionMatrix(factor(test_results), factor(test$class))
#accuracy 85.4%
#ROC curve
roc_curve <- roc(test$class, test_results)
auc(roc_curve)
plot(roc_curve)
#AUC is 0.839
#H0: ß1 = ß2 = ß3 = 0
#Ha: there is at least one ßi!=0
#overall test:
wald.test(b=coef(model7), Sigma=vcov(model7), Terms=2:4)
#p-value < 0.05, hence reject H0. That means there is at least one ßi=0.
#Test for each parameter
#For Status
#H0: There is no association between status and life expectancy level, after
#controlling for adult mortality rate, and income composition
#Ha: There is an association between status and life expectancy level, after
#controlling for adult mortality rate, and income composition
#alpha=0.05
qnorm(0.05/2, lower.tail = FALSE)
z_st <- 3.274609/1.010547 
z_st #3.24
#z_st>1.96 - reject H0. There is an association between status and life expectancy 
#level, after controlling for adult mortality rate, and income composition
#odds ratio
exp(3.274609)
#After controlling for adult mortality rate and income composition, people living 
#in developed countries are 26.4329 times more likely to have a long life compared
#to people living in developing countries.

#For adult mortality rate
#H0: There is no association between adult mortality rate and life expectancy level,
#after controlling for health expenditure, status, and income composition
#Ha: There is an association between adult mortality rate and life expectancy level,
#after controlling for health expenditure, status, and income composition
#alpha=0.05
z_am <- (-0.013015)/0.000982
z_am
#|z_am| > critical value, reject H0. There is an association between adult mortality
#rate and life expectancy level, after controlling for status and income composition
exp(-0.013015)
#For every one-unit increase in adult mortality rate, the odds of having a long life
#decrease by a factor of 0.9871.

#For income composition
#H0: There is no association between income composition and life expectancy level, after
#controlling for health expenditure, status, and adult mortality rate
#Ha: There is an association between  income composition and life expectancy level, after
#controlling for health expenditure, status, and adult mortality rate
#alpha=0.05
z_in <- 11.053993/0.678411
z_in
#z_in>1.96 - reject H0. There is an association between income composition and life expectancy level, after
#controlling for status, and adult mortality rate
#odds ratio
exp(11.053993)

#We can observe that the multiple logistic regression model fits the data better
#because it has a higher AUC and accuracy compared to the simple logistic regression
write.csv(df_log, file = "logistic_data.csv")

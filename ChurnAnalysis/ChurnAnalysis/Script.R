library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(gridExtra)
library(corrplot)
library(caret)
library(e1071)

#loading data 

df = read.csv("D:\\Emman\\Documents\\R Projects\\Datasources\\customerchurn.csv", na.strings = c("", NA))

#check the first 5 rows of the data

head(df)

#df$MultipleLines = as.factor(ifelse(as.character(df$MultipleLines) == "No phone service", "No", as.character(df$MultipleLines))) %>% factor(,levels = c("No", "Yes"), labels = c(0,1))


summary(df)

str(df)


#lets check for null values
#it appears with 11 records of variables with null values we can impute those values.   
colSums(is.na(df))

#lets describe the data 
#it appears that for monthly charges there a mean of 2283.30 and and a standard deviation of 2266.77 
#the min value is 18.50 and the max value is 8684.80 this indicates variation in the data.  
#We will use outlier detection and treament
summary(df)

#data visualization
c1 = ggplot(df, aes(MonthlyCharges)) +
    geom_histogram(binwidth = 10, color = "black", fill = "blue") +
    ggtitle("Distribution of Monthly Charges") +
    xlab("Monthly Charges") +
    ylab("Frequency")

c2 = ggplot(df, aes(TotalCharges)) +
    geom_histogram(binwidth = 500, color = "black", fill = "lightgreen") +
    ggtitle("Distribution of Total Charges") +
    xlab("Total Charges") +
    ylab("Frequency")

grid.arrange(c1, c2, ncol = 2)


a1 = df %>% select(InternetService, MonthlyCharges) %>% filter(!InternetService == 'No') %>%
    ggplot() +
    geom_boxplot(aes(InternetService, MonthlyCharges, fill = InternetService))

a2 = df %>% select(Contract, MonthlyCharges) %>%
    ggplot() +
    geom_boxplot(aes(Contract, MonthlyCharges, fill = Contract))


grid.arrange(a1, a2, ncol = 2)


b1 = ggplot(df, aes(factor(Contract), fill = Contract)) +
    geom_bar()

b2 = ggplot(df, aes(factor(tenure), fill = tenure)) +
    geom_bar() +
    ggtitle("Distribution of Customer Tenure in Months") +
    xlab("Tenure in Months")

grid.arrange(b1, b2, ncol = 2)


ggplot(df, aes(x = TotalCharges, y = tenure, color = gender)) +
    geom_point()

#CLeaning the data 
#remove customer id from equation data preparation
#df$OnlineSecurity = as.factor(ifelse( as.character(df$OnlineSecurity) == "No internet service", "No",as.character(df$OnlineSecurity)))

for (i in names(df)) {
    if (is.factor(df[, i]) == TRUE) {
        df[, i] = as.factor(ifelse(as.character(df[, i]) == "No internet service" | as.character(df[, i]) == "No phone service", "No", as.character(df[, i])))
        
    }
    if (is.factor(df[, i]) & nlevels(df[, i]) == 2) {
        if (i == "gender") {
            df[, i] = factor(df[, i], levels = c("Female", "Male"), labels = c(0, 1))
        } else {
            df[, i] = factor(df[, i], levels = c("No", "Yes"), labels = c(0, 1))
        }
    }
}

df$TotalCharges = ifelse(is.na(df$TotalCharges), mean(df$TotalCharges, na.rm = TRUE), df$TotalCharges)

df$InternetService = factor(df$InternetService,
                            levels = c("DSL", "Fiber optic", "No"),
                            labels = c(1, 2, 3))

df$Contract = factor(df$Contract,
                     levels = c("Month-to-month", "One year", "Two year"),
                     labels = c(1,2,3))


for (i in colnames(df)) {
    if (is.numeric(df[, i]) & i != "SeniorCitizen") {
        df[, i] = scale(df[, i])
    }
}

summary(df)

str(df)

final = df %>% select(-customerID, -PaymentMethod)

#split train to test
split = sample.split(final$Churn, SplitRatio = 0.80)
train = subset(final, split == TRUE)
test = subset(final, split == FALSE)

head(train)
head(test)

#model building

formula = "Churn ~ ."

model1 = glm(formula, data = train, family = binomial(link = "logit"))

abc = step(model1, direction = 'both')
summary(model1)

abc$formula

formula2 ="Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService + 
    OnlineSecurity + DeviceProtection + TechSupport + StreamingTV + 
    StreamingMovies + Contract + PaperlessBilling + MonthlyCharges + 
    TotalCharges"

model2 = glm(formula2, data = train, family = binomial(link = "logit"))
zyx = step(model2, direction = "both")
summary(model2)

formula3 = "Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService +
    OnlineSecurity + StreamingTV + StreamingMovies + Contract + PaperlessBilling + MonthlyCharges +
    TotalCharges"


model3 = glm(formula3, data = train, family = binomial(link = "logit"))
zyx = step(model3, direction = "both")
summary(model3)


fitted.results = predict(model3, newdata = test, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy', 1 - misClasificError))

confusionMatrix(factor(fitted.results), factor(test$Churn))

library(rpart.plot)
library(rpart)
tree = rpart(formula3, method = "class", data = train)

printcp(tree)

rpart.plot(tree)

a = predict(tree, test, type = "class")

confusionMatrix(factor(test$Churn), factor(a))

summary(tree)

head(a)

fitted.results <- predict(tree, newdata = test, type = "class")
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy', 1 - misClasificError))

confusionMatrix(fitted.results, test$Churn)

install.packages("ROCR")

library(ROCR)

p = predict(model3, newdata = test, type = "response")
pred = prediction(p, test$Churn)


prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#anova anlyiss on contrct and monthly Charges 
#H0 = The means are equal between contracts
#Ha = The Means are different between the contracts.   
anova = aov(MonthlyCharges ~ Contract, df)

summary(anova)

#Pairwise
t = TukeyHSD(anova)

plot(t)

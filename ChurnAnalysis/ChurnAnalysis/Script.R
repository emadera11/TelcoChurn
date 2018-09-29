library(dplyr)
library(ggplot2)
library(psych)
library(mlbench)
library(Hmisc)
library(corrplot)
library(earth)
library(gridExtra)
library(knitr)
library(car)
library(e1071)
library(caret)
library(caTools)

install.packages("mlbench")
install.packages("gridExtra")

install.packages('kableExtra')

update.packages("kableExtra")

#loading data 

df = read.csv("D:\\Emman\\Documents\\R Projects\\Datasources\\customerchurn.csv", na.strings = c("", NA))

#check the first 5 rows of the data

head(df)

str(df)


#lets check for null values
#it appears with 11 records of variables with null values we can impute those values.   
colSums(is.na(df))

#lets describe the data 
#it appears that for monthly charges there a mean of 2283.30 and and a standard deviation of 2266.77 
#the min value is 18.50 and the max value is 8684.80 this indicates variation in the data.  
#We will use outlier detection and treament
summary(df)
psych::describe(df)


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


#remove customer id from equation data preparation
#df$OnlineSecurity = as.factor(ifelse( as.character(df$OnlineSecurity) == "No internet service", "No",as.character(df$OnlineSecurity)))

for (i in names(df)) {
    if (is.factor(df[, i]) == TRUE) {
        df[, i] = as.factor(ifelse(as.character(df[, i]) == "No internet service", "No", as.character(df[, i])))

    }
}


b = df %>% select(-customerID)
b$TotalCharges[is.na(b$TotalCharges)] = mean(b$TotalCharges, na.rm = TRUE)


indx <- sapply(b, is.factor)
b[indx] <- lapply(b[indx], function(x) as.numeric((x)))

b$Churn = ifelse(b$Churn == 1, 0, 1)

head(b)

c = cor(b)
corrplot(c, method = 'circle')

#split train to test
dt = sort(sample(nrow(b), nrow(b) * 0.8))
train = b[dt,]
test = b[-dt,]

head(train)


library(MASS)
#model building
model = glm(Churn ~ ., data = train, family = binomial(link = "logit"))
stepAIC(model, direction = 'both')
summary(model)

anova(model)

vif(model)

model2 = glm(Churn ~ SeniorCitizen + tenure + PhoneService +
    OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + Contract + PaperlessBilling + MonthlyCharges +
    TotalCharges, data = train, family = binomial(link = "logit"))

summary(model2)

anova(model2)

vif(model2)

predict()

fitted.results <- predict(model2, newdata = test, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy', 1 - misClasificError))

confusionMatrix(test$Churn, fitted.results)

library(rpart.plot)
library(rpart)
tree = rpart(Churn ~ SeniorCitizen + tenure + PhoneService +
    OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + Contract + PaperlessBilling + MonthlyCharges +
    TotalCharges, method = "class", data = train)

printcp(tree)

rpart.plot(tree)

a = predict(tree, test, type = "class")

confusionMatrix(test$Churn, a)

head(test$Churn)

summary(tree)



model1 = glm(Churn ~ tenure + PhoneService + OnlineSecurity + OnlineBackup + TechSupport + Contract + PaperlessBilling +
    MonthlyCharges + TotalCharges, data = train, family = binomial(link = "logit"))

summary(model1)

anova(model1)

plot(model1)
plot(anova(model1))


fitted.results <- predict(model1, newdata = test, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy', 1 - misClasificError))

confusionMatrix(fitted.results, test$Churn)


library(ROCR)

p = predict(model1, newdata = test, type = "response")
pred = prediction(p, test$Churn)


prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


df %>% select(Contract) %>% head

cmd = df %>%
    select(Contract, MonthlyCharges) %>%
    data.frame()

head(cmd)

#anova anlyiss on contrct and monthly Charges 
#H0 = The means are equal between contracts
#Ha = The Means are different between the contracts.   
anova = aov(MonthlyCharges ~ Contract, cmd)

summary(anova)

#Pairwise
TukeyHSD(anova)
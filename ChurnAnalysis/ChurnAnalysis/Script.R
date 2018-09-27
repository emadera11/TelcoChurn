library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(knitr)


df = read.csv("customerchurn.csv", na.strings = c("", "NA"))

head(df)
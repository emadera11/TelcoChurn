library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(knitr)
library(devtools)

install_github("haozhu233/kableExtra")


df = read.csv("customerchurn.csv", na.strings = c("", "NA"))

head(df)

install.packages("kableExtra")

update.packages("kabelExtra")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("psych")
install.packages("tidyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")

install.packages("devtools")
install.packages("knitr")
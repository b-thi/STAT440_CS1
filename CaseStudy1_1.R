# STAT 440 - Case Study 1 - Fall 2017#
# Matthew Reyers, Brad Smallwood, Barinder Thind #
# Code: September 12th, 2017

# Loading relevant packages
library(rJava)
library(xlsx)
library(tidyverse)

##### Dr. Davis's Code #####

# Reading in data initially
data = read.xlsx("ssc_case_study_2_inflammatory_bowel_disease.xlsx",
                 sheetIndex=1 ,as.is=TRUE)

# Doesn't look very nice
head(data)

# Creating variable names
names(data)[1] = "GeneID"
names(data)[2] = "Variable"
names(data)[c(-1,-2)] = paste0("Patient",1:(ncol(data)-2)) # 127 patients

# Looking at data again
data[,1:2]

# Cleaning up data (Stuff Jack did)
data$allvars = ""
data$allvars[1:4] = as.character(data[1:4,2])
data$allvars[-(1:4)] = as.character(data[-(1:4),1])
data[,2] = data$allvars
data[,1] = NULL
data$allvars = NULL
data$Patient127 = NULL

# Converting to "long format"
library(data.table)
data2 = transpose(data)
head(data2)

names(data2) = as.character(data2[1,])
data2 = data2[-1,]

# Looking at structure
str(data2)

# Everything is a character, so converting over appropriately:
for(k in c(1,3,4))
{
  data2[,k] = as.factor(data2[,k])
}

data2[,2] = as.numeric(data2[,2])
for(k in 5:ncol(data2))
{
  data2[,k] = as.numeric(data2[,k])
}

# Fixing misspellings
misspelled = which(data2$Ethnicity == "cacuasian")
data2$Ethnicity[misspelled] = "caucasian"
misspelled = which(data2$Group == "Ulcerative")
data2$Group[misspelled] = "Ulcerative Colitis"

## Check our work
table(data2$Group)
table(data2$Ethnicity)

# Writing cleaned .csv
write.csv(data2,"Case Study IBD Cleaned.csv")

# Reading in data again, but the clean one
cs1_data <- read.csv("Case Study IBD Cleaned.csv", as.is = T, header = T)

# Looking at data
head(cs1_data)
head(data2) # Why are these two not the same? The x's in front?

# colnames(cs1_data) = colnames(data2)

# Now we have our cleaned data set: let's use data2 as in class

##### End of Dr. Davis's Code #####

# Looking at individual variables

## Group
summary(data2$Group)

# There are 59 patients with Crohn's diseas, 41 who don't have any and
# 26 with ulcerative colitis

## Sex
summary(data2$Sex)

# There are 74 females and 52 males

## Age
summary(data2$Age)

# The maximum age is 73 and the minimum age is 20 – the median age is 44.

## Ethnicity
summary(data2$Ethnicity)

# What to do with asian, hispanic, and indian observations? Genetic
# differences may suggest that throwing out these observations makes
# sense as they might skew results. 

# Let's look at the first gene expression as a function
expression_1 <- as.data.frame(data2$`200006_at`)
colnames(expression_1) <- "200006_at"

head(expression_1)
ggplot(data = expression_1, aes(x = time(expression_1$`200006_at`), 
                                y = expression_1$`200006_at`)) + 
  geom_point() + geom_smooth(method = "lm")

# Doesn't seem to be much to look at here, let's make plots for every one
# of the gene expressions and see how they look





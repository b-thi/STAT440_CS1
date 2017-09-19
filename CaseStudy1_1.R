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

####### Splitting Data into Training/Test ########

data2_train1 <- data2[1:(nrow(data2)/1.5),]
data2_train2 <- cbind(data2_train1[,1:4], 
                      apply(data2_train1[,5:313], 2, function(x) ((x - mean(x))/sd(x))))

data2_test1 <- data2[((nrow(data2)/1.5) + 1):nrow(data2),]
data2_test2 <- cbind(data2_test1[,1:4],
  apply(data2_test1[,5:313], 2, function(x) ((x - mean(x))/sd(x))))

# Standardizing
data2_train <- apply(data2_train1[,5:313], 2, function(x) ((x - mean(x))/sd(x)))
data2_test <- apply(data2_test1[,5:313], 2, function(x) ((x - mean(x))/sd(x)))

#########################################

#### Principal Component Analysis ####

# # pca_train2 <- princomp(scaled.data2_train)
# # Need more obs than vars for princomp?? q
# 
# # Doing pca with prcomp
# pca_train2 <- prcomp(scaled.data2_train)
# nrow(scaled.data2_train)
# ncol(scaled.data2_train)
# 
# # Extracting out principal components
# loadings_train <- as.data.frame(pca_train2$rotation[,1:2])
# 
# # Looking at data
# head(loadings_train)
# nrow(loadings_train)
# 
# # Combining with previous data
# data2_train_comb <- cbind(data2_train[,1:4], 
#                           loadings_train)
# # What happened above? different dimensions?

# 1. Standardize the data
# scaled.data2_train <- apply(data2_train[,5:313], 2, function(x) (x - mean(x)))
# head(scaled.data2_train)

# Loading library
library(pcaMethods)

# Performing PCA
pca_train <- pca(data2_train, nPcs = 42)
pca_train1 <- pca(data2_train2, nPcs = 42)

summary(pca_train)
summary(pca_train1)

scores_pca <- as.data.frame(scores(pca_train))
scores_pca1 <- as.data.frame(scores(pca_train1))

# Combining Data
data2_train_comb2 <- cbind(data2_train1[,1:4], scores_pca)
data2_train_comb22 <- cbind(data2_train1[,1:4], scores_pca1)

# Looking at data
data2_train_comb2
data2_train_comb22

########## - Finished this Dimension Reduction ##########

##### Attempting random forest on above PCA #####
library(randomForest)
library(nnet)

# Removing empty factors
data2_train_comb2$Group <- droplevels(data2_train_comb2$Group)
data2_train_comb2$Ethnicity <- droplevels(data2_train_comb2$Ethnicity)

data2_train_comb22$Group <- droplevels(data2_train_comb22$Group)
data2_train_comb22$Ethnicity <- droplevels(data2_train_comb22$Ethnicity)

# Running random forest
rf_1 <- randomForest(data2_train_comb22$Group ~ ., 
                     data = data2_train_comb22, ntree = 10000,
                     mtry = 10)

# Looking at randomforest
rf_1

# Predicted
predicted_1 <- as.vector(predict(rf_1, data2_train_comb22))
actual_values_train <- as.vector(data2_train_comb22$Group)

# Looking at predictions
predicted_1 == actual_values_train

# Looking at it from test set
pca_test <- pca(data2_test, nPcs = 42)
summary(pca_test)
scores_pca2 <- as.data.frame(scores(pca_test))

pca_test1 <- pca(data2_test2, nPcs = 42)
summary(pca_test1)
scores_pca22 <- as.data.frame(scores(pca_test))


# Combining Data
data2_test_comb2 <- cbind(data2_test1[,1:4], scores_pca2)
data2_test_comb22 <- cbind(data2_test2[,1:4], scores_pca22)

# Getting rid of useless levels
data2_test_comb22$Group <- droplevels(data2_test_comb22$Group)
data2_test_comb22$Ethnicity <- droplevels(data2_test_comb22$Ethnicity)

str(data2_test_comb22$Group)
str(data2_train_comb22$Group)

str(data2_test_comb22$Ethnicity)
str(data2_train_comb22$Ethnicity)

# Predicting again
predicted_2 <- as.vector(predict(rf_1, data2_test_comb22))

# New factors present error - need to get rid of useless obs
summary(data2_test_comb22$Ethnicity)
summary(data2_train_comb22$Ethnicity)

# Getting rid of hispanic observation
data2_test_comb33 <- data2_test_comb22 %>% 
                      filter(Ethnicity != "asian")

# Looking at new summary
summary(data2_test_comb33$Ethnicity)

# Dropping levels again
data2_test_comb33$Ethnicity <- droplevels(data2_test_comb33$Ethnicity)

# Attempting to predict again
predicted_2 <- as.vector(predict(rf_1, data2_test_comb33))

colnames(data2_test_comb33) == colnames(data2_train_comb22)

# Fail again, let's look at other variables
summary(data2_test_comb22$Group)
summary(data2_train_comb22$Group)

summary(data2_test_comb22$Ethnicity)
summary(data2_train_comb22$Ethnicity)

# Fixing error again
levels(data2_test_comb33$Ethnicity) <- levels(data2_train_comb22$Ethnicity)

# Looking again at factors
summary(data2_test_comb33$Ethnicity)
summary(data2_train_comb22$Ethnicity)

# Now predicting AGAIn
predicted_3 <- as.vector(predict(rf_1, data2_test_comb33))

# Now, let's check our predictions
actual_values_test <- as.vector(data2_test_comb33$Group)
nrow(data2_test_comb33)
# Comparing
sum(predicted_3 == actual_values_test)

# Fail - 14/41 correct lol

# Let's re-run by editing our training data a bit
data2_train_comb33 <- data2_train_comb22 %>% 
  filter(Ethnicity == c("black", "caucasian"))

levels(data2_test_comb33$Ethnicity) <- levels(data2_train_comb33$Ethnicity)

data2_test_comb44 <- data2_test_comb33 %>% 
  filter(Ethnicity != c("hispanic", "indian"))

summary(data2_train_comb33$Ethnicity)
summary(data2_test_comb44$Ethnicity)

data2_train_comb33$Ethnicity <- droplevels(data2_train_comb33$Ethnicity)
data2_test_comb44$Ethnicity <- droplevels(data2_test_comb44$Ethnicity)

summary(data2_train_comb33$Ethnicity)
summary(data2_test_comb44$Ethnicity)

# New random forest
# Running random forest
rf_2 <- randomForest(data2_train_comb33$Group ~ ., 
                     data = data2_train_comb33, ntree = 10000,
                     mtry = 10)

# Predicted again
predicted_1_fix <- as.vector(predict(rf_2, data2_train_comb33))
actual_values_train <- as.vector(data2_train_comb33$Group)

# Looking at predictions
predicted_1_fix == actual_values_train

# Now trying with test data
predicted_2_fix <- as.vector(predict(rf_2, data2_test_comb44))
actual_values_test <- as.vector(data2_test_comb44$Group)

# Comparing
predicted_2_fix == actual_values_test
sum(predicted_2_fix == actual_values_test)
length(actual_values_test)

####### Multinomial Model ########
fit_mnom <- multinom(data2_train_comb33$Group ~ ., 
                     data = data2_train_comb33)

pred_multinom <- predict(fit_mnom, type="probs", 
                         newdata = data2_test_comb44)

pred_multinom_rounded <- round(pred_multinom, 0)
pred_multinom_round_t <- t(pred_multinom_rounded)
pred_multinom_round_t

data2_test_comb44$Group

# Running another multinomial model
fit_mnom2 <- multinom(data2_train1$Group ~ ., 
                      data = data2_train1)

pred_multinom2 <- round(predict(fit_mnom2, type="probs", 
                         newdata = data2_test1))

t(pred_multinom2)
data2_test1$Group

##### Method is not the best ######

#### Multiple Factor Analysis ####

# Loading package
library(FactoMineR)

# Comparing with Wine Data
str(wine) # 21 obs with 31 variables
str(scaled.data2) # 126 obs of 313 variables

# Doing MFA initially

# Wine
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n","s", "s", "s", "s", "s"),
           ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
           num.group.sup=c(1,6))

res2 <- MFA(scaled.data2, group = c(1, 1, 2, 309), 
            type = c("n", "s", "n", "s"), 
            ncp = 5,
            name.group = c("Group_1", "Age", "Ethnicity/Sex", 
                           "Gene Expressions"),
            num.group.sup = 1)

str(scaled.data2)



#### Random Forest for this data ####
library(randomForest)
library(nnet)

# scaled.data2 <- scaled.data2 %>% 
#   filter(Group != "Ulcerative")

scaled.data2$Group <- droplevels(scaled.data2$Group)
scaled.data2$Ethnicity <- droplevels(scaled.data2$Ethnicity)

str(scaled.data2$Group)
str(scaled.data2$Ethnicity)


fit1 <- multinom(scaled.data2$Group ~ ., data = scaled.data2)
summary(fit1)

m1 <- randomForest(cs1_data$Group ~ ., data = cs1_data, ntree = 100 )

fit2 <- glm(scaled.data2$Group ~ ., data = scaled.data2)
fit3 <- lm(scaled.data2$Group ~ ., data = scaled.data2)


# Looking at correlations
heatmap(cs1_data, "lol")

heatmap(data2_train_comb3, "train")

t(cor(cs1_data[,6:313]))

corr.df <- cor(data2_train_comb33[,5:46])

get_lower_tri <- function(data){
  data[upper.tri(data)] <- NA
  return(data)
}

melt.corr.df <- melt(get_lower_tri(corr.df), na.rm = T)

corr.d2 <- cor(data2_train2[,5:nrow(data2_train2)])
melt.corr.df2 <- melt(get_lower_tri(corr.d2))
melt.corr.df2

ordered_corr <- melt.corr.df2 %>% 
                  arrange(-value)


####### Neural Net ########
library(tidyverse)
library(nnet)

### Practice

# Loading in data
seeds<- read.csv("seeds.csv", header=T)

# Looking at data
seeds <- seeds[,1:8]
str(seeds)

colnames(seeds) <- c("area", "perimeter", "compactness",
                     "kernal_length", "kernel_width", "asymmetry", 
                     "length_kernel_groove", "Class")

# Training data set
seedstrain<- sample(1:210,147)

# Test data set
seedstest <- setdiff(1:210,seedstrain)

# Normalizing
ideal <- class.ind(seeds$Class)

# nnet
seedsANN = nnet(seeds[seedstrain,-8], ideal[seedstrain,], size=10, softmax=TRUE)

# Predicting training set
predict(seedsANN, seeds[seedstrain,-8], type="class")

# Predicting test set
table(predict(seedsANN, seeds[seedstest,-8], type="class"),seeds[seedstest,]$Class)

test_pred <- as.integer(predict(seedsANN, seeds[seedstest, -8], type = "class"))

seeds[seedstest, 8]

sum(test_pred == seeds[seedstest, 8])

### Redoing now with our gene expression data ###

# Ideal
ideal2 <- class.ind(data2_train_comb33$Group)

gene_ANN <- nnet(data2_train_comb33[, -c(1:4)], ideal2, size = 10, softmax = T)

# 100% success rate
train_pred2 <- predict(gene_ANN, data2_train_comb33[,-c(1:4)], type="class")
sum(train_pred2 == data2_train_comb33$Group)

test_pred2 <- predict(gene_ANN, data2_test_comb44[, -c(1:4)], type = "class")

sum(test_pred2 == data2_test_comb44$Group)
nrow(data2_test_comb44)

##### Trying again but with original data #####
# Ideal
ideal2 <- class.ind(data2_train2$Group)

gene_ANN <- nnet(data2_train2[, -c(1:4)], ideal2, size = 10, softmax = T, MaxNWts = 3200)

# 100% success rate
train_pred2 <- predict(gene_ANN, data2_train2[,-c(1:4)], type="class")
sum(train_pred2 == data2_train2$Group)

test_pred2 <- predict(gene_ANN, data2_test2[, -c(1:4)], type = "class")

sum(test_pred2 == data2_test2$Group)/nrow(data2_test2)

####### Cross-Validating #######


######### Testing again ##########


######## LASSSO -> Neural Net ########

# first testing dr. davis's code
library(glmnet)

x = matrix(rnorm())

x=matrix(rnorm(100*20),100,20)
y=rnorm(100)

fit1=glmnet(x,y)

print(fit1)

plot(fit1)

x2=matrix(rnorm(100*20), 100, 20)
y2=1*x2[,1] + 2*x2[,2] + 3*x2[,3] + rnorm(100)
fit2=glmnet(x2,y2)
print(fit2)
plot(fit2)

### Extract these lambdas
lambs = print(fit2)[,3]

### Print coefficients for the first 10
coef(fit2,s=lambs[1:10])

###### Trying for the gene expression data ######
x_1 <- data2_train_comb3[,5:46] # Extracting out principal components
y_1 <- data2_train_comb3[,1] # Extracting out the predicted values

lasso_fit <- glmnet(x_1, y_1)



# Using AIC and BIC to dimension reduce #
lm1 <- lm(data2_train_comb3$Group ~ ., data = data2_train_comb3)

lm1AIC <- step(lm1, scope = list(lower =~ 1, upper =~ .), direction = "backward",
               k = log(nrow(data2_train_comb3)), data = data2_train_comb3)

#' ---
#' title: 'OYJ036_analytics'
#' author: "OYJ"
#' date: '20210625'
#' output:
#'   html_document:
#'     number_sections: yes 
#' css: styleknitrBDA.css
#' sansfont: LiberationSans
#' ---
#' 

rm(list=ls())

library(knitr)
knitr::opts_chunk$set(
  comment=NA,   echo = TRUE, message=FALSE, error=FALSE, hightlight=TRUE, 
  include=TRUE, prompt=FALSE, warning=FALSE, fig.width=6, fig.height=6)
library(tidyverse)  
library(data.table) # fread
library(dataMaid)   # code book
library(caret)    # for data pre-processing; classification and regression training, including confusionMatrix
library(pROC)
library(ROCR)     # prediction, performance
library(rpart)      # more than two labels 
library(rpart.plot) # Enhanced tree plots


#
source("C:\\Rworking\\ZDist_files\\classificationTree\\source_dataMining.R")
hd  <- function(dat) {head(as.data.frame(dat))}
jtable <- function(dat) {a=table(dat, exclude=NULL); print(a); sum(a)}  
ptable <- function(dat) {a=table(dat, exclude=NULL); print(round(prop.table(a), digits=2)); sum(a)} 
pmiss  <- function(dat) {apply(dat, 2, function(x) sum(is.na(x))/length(x)*100)} 
cat_levels <- function(dat) {sapply(dat[, sapply(dat, is.factor)], nlevels)}

# calculates MSE (Root Mean Squared Error) for a regression decision tree
compute_rmseFUN <- function(ft_model, dat) {
  yhat <- predict(ft_model, newdata = dat)
  actual <- dat$Y 
  SSE  <- mean((yhat-actual)^2) %>% sqrt() # sqrt(SSE)
  return(SSE)
}



# data read
Mz <- fread("OYJ036_cleansing.csv")
dim(Mz)
str(Mz)



# Descriptive Analytics (use M7 data, not MZ for getting MicroPM column(continuous data))
# Mz is for Predictive analytics and M7 is for Descriptive Analytics
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(ggExtra)

G1 <- ggplot(data = M7, aes(x = MicroPM,  y = Temp)) + geom_point()
G2 <- ggplot(data = M7, aes(x = MicroPM,  y = RelHumidity)) + geom_point()
G3 <- ggplot(data = M7, aes(x = MicroPM,  y = WindDirec)) + geom_point()
G4 <- ggplot(data = M7, aes(x = MicroPM,  y = WindSpeed)) + geom_point()
G5 <- ggplot(data = M7, aes(x = MicroPM,  y = GustDirec)) + geom_point()
G6 <- ggplot(data = M7, aes(x = MicroPM,  y = GustSpeed)) + geom_point()
G7 <- ggplot(data = M7, aes(x = MicroPM,  y = Illumination)) + geom_point()
G8 <- ggplot(data = M7, aes(x = MicroPM,  y = UVrays)) + geom_point()
G9 <- ggplot(data = M7, aes(x = MicroPM,  y = WaterEva)) + geom_point()

grid.arrange(G1, G2, G3, G4, G5, G6, G7, G8, G9, nrow=2)


theme_set(theme_bw(base_size=16))
D1 <- M7%>%ggplot(aes(x=MicroPM, fill=Temp)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Temperature")
D2 <- M7%>%ggplot(aes(x=MicroPM, fill=RelHumidity)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Relative Humidity")
D3 <- M7%>%ggplot(aes(x=MicroPM, fill=WindDirec)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Wind Direction")
D4 <- M7%>%ggplot(aes(x=MicroPM, fill=WindSpeed)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Wind Speed")
D5 <- M7%>%ggplot(aes(x=MicroPM, fill=GustDirec)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Gust Direction")
D6 <- M7%>%ggplot(aes(x=MicroPM, fill=GustSpeed)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Gust Speed")
D7 <- M7%>%ggplot(aes(x=MicroPM, fill=Illumination)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Illumination")
D8 <- M7%>%ggplot(aes(x=MicroPM, fill=UVrays)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "UVrays")
D9 <- M7%>%ggplot(aes(x=MicroPM, fill=WaterEva)) +
  geom_density(alpha = 0.3) +
  labs(subtitle = "Water Evaporation")

grid.arrange(D1, D2, D3, D4, D5, D6, D7, D8, D9, nrow=2)



# Predictive Analytics
# pre-processing 
nzv <- nearZeroVar(Mz[, 1:9], saveMetrics = TRUE)
nzv

# re-sampling due to the class imbalance 
library(ROSE)
table(Mz$Y)
prop.table(table(Mz$Y))

M <- ROSE(Y ~., data = Mz, seed = 1)$data
table(M$Y)



# splitting
set.seed(123)

train_RowNo <- sample(1:nrow(M), nrow(M)*0.8, replace=FALSE)

train_data <- M[train_RowNo,  ]
dim(train_data)

test_data <- M[-train_RowNo, ]
dim(test_data)



# Modeling1: random forest
library(randomForest)

my_trControl1 <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 3)

model_rf <- caret::train(Y ~., method = "rf", data = train_data, trControl = my_trControl1)
model_rf

test_rf <- test_rocFUN(model_rf, test_data)



# Modeling2: r partition
model_rpart <- caret::train(Y ~., method = "rpart", data = train_data)
model_rpart

test_rpart <- test_rocFUN(model_rpart, test_data)



# Modeling3: Support Vector Machine with Linear
model_svm <- caret::train(as.factor(Y) ~ ., method = "svmLinear", data = train_data) 
model_svm

test_svm <- test_rocFUN(model_svm, test_data)
Y.colNumber <- which(names(test_data) == "Y")
predict.Y <- predict(model_svm, test_data[, -Y.colNumber]) 
print(  confusionMatrix(predict.Y,  as.factor(test_data$Y)))



# Modeling4: Ida(Linear Discriminant Analysis)
model_lda <- caret::train(as.factor(Y) ~ ., method = "lda", data = train_data)
model_lda

test_lda <- test_rocFUN(model_lda, test_data)



# Compare Models
model_list <- resamples(list(rpart=model_rpart, rf=model_rf, lda=model_lda))
summary(model_list)
bwplot(model_list, metric = "Accuracy")
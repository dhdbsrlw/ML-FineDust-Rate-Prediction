#' ---
#' title: 'OYJ036_cleansing'
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


#
source("C:\\Rworking\\ZDist_files\\classificationTree\\source_dataMining.R")
hd  <- function(dat) {head(as.data.frame(dat))}
jtable <- function(dat) {a=table(dat, exclude=NULL); print(a); sum(a)}  
ptable <- function(dat) {a=table(dat, exclude=NULL); print(round(prop.table(a), digits=2)); sum(a)} 
pmiss  <- function(dat) {apply(dat, 2, function(x) sum(is.na(x))/length(x)*100)} 
cat_levels <- function(dat) {sapply(dat[, sapply(dat, is.factor)], nlevels)}



# Calculates MSE (Root Mean Squared Error) for a regression decision tree
compute_rmseFUN <- function(ft_model, dat) {
  yhat <- predict(ft_model, newdata = dat)
  actual <- dat$Y 
  SSE  <- mean((yhat-actual)^2) %>% sqrt() # sqrt(SSE)
  return(SSE)
}


# After adding English column names to the original data file

# data read
M_org <- fread("OYJ036_org.csv", skip = 1, encoding = "UTF-8")
dim(M_org)
str(M_org)



# data cleansing
M1 <- select(M_org, -(Noise:MaxVib_z)) #irrelevant columns
M2 <- select(M1, -(MicroPMrev:Date)) #irrelevant & duplicate columns
M3 <- select(M2, -SerialNum) #irrelevant columns
dim(M3)


# remove unique columns
del_idx <- apply(M3, 2, function(x) ifelse(length(unique(x)) == 1, TRUE, FALSE))
sum(del_idx)
col <- names(M3)[!del_idx]
M4 <- M3 %>% dplyr::select(all_of(col))
dim(M4)


# remove all-rows-different columns
del_idx2 <- apply(M4, 2, function(x) ifelse(length(unique(x)) == nrow(M4), TRUE, FALSE))
sum(del_idx2)
col <- names(M4)[!del_idx2]
M5 <- M4 %>% select(all_of(col))
dim(M5)


# imputation (N/A)
library(mice)
Mz_imp <- M5
pmiss(Mz_imp)

N_na <- sum(is.na(Mz_imp))
tot_element <- nrow(Mz_imp) * ncol(Mz_imp)
na_percent <- (N_na)/(tot_element) * 100
cat("NA % = ", na_percent, "%\n")

if (na_percent < 0.1) {
  M6 <- na.omit(Mz_imp)
} else {
  md.pattern(Mz_imp)
  imp <- mice(Mz_imp, m = 1) 
  densityplot(imp)
  M6 <- complete(imp, 1)
}

head(M6)
dim(M6)


# mutate
M7 <- dplyr::mutate(M6, Y = ifelse( M6$MicroPM <= 35, "Good", "Bad"))
Mz <- select(M7, -(MicroPM:PM))
head(Mz)
dim(Mz)



# codebook
# makeCodebook(Mz, ouput = "html", replace = TRUE)

# write
#fwrite(Mz, "OYJ036_cleansing.csv")
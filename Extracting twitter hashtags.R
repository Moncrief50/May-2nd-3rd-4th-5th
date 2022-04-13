getwd()
setwd("/Users/robertmoncrief/Desktop/Twitter Data")
library(stringr)
library(plyr)
library(tm)
library(corpus)
library(cld3)
library(stylo)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(usethis)
library(Rcpp)
library(RcppEigen)
library(rstan)
library(stringi)

#reading in all the files, I rbinded them together on a different window and saved them as csv files
May2all <- data.frame(read.csv("/Users/robertmoncrief/Desktop/Twitter Data/May2all.csv"))
May3all <- data.frame(read.csv("/Users/robertmoncrief/Desktop/Twitter Data/May3all.csv"))
May4all <- data.frame(read.csv("/Users/robertmoncrief/Desktop/Twitter Data/May4all.csv"))
May5all <- data.frame(read.csv("/Users/robertmoncrief/Desktop/Twitter Data/May5all.csv"))

#rbinded all the csv files together to make one big dataframe
May2345 <- rbind(May2all, May3all, May4all, May5all)

May2345 <- subset(May2345, detect_language(May2345$text) == "en") #Gets rid of everything except english.

May2all <- subset(May2all, detect_language(May2all$text) == "en") 
May3all <- subset(May3all, detect_language(May3all$text) == "en")
May4all <- subset(May4all, detect_language(May4all$text) == "en")
May5all <- subset(May5all, detect_language(May5all$text) == "en")

stopwords = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19", 
"covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "?", "COV...",
"pandemic,", "'", "COVID-19")


#Deletes all stop words
May2345$text <- stri_replace_all_regex(May2345$text,
                                           pattern = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
                                                       "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
                                                       "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related"),
                                           replacement = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                                           vectorize = FALSE)

May2all$text <- stri_replace_all_regex(May2all$text,
                                       pattern = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
                                                   "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
                                                   "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related"),
                                       replacement = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                                       vectorize = FALSE)

May3all$text <- stri_replace_all_regex(May3all$text,
                                       pattern = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
                                                   "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
                                                   "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related"),
                                       replacement = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                                       vectorize = FALSE)

May4all$text <- stri_replace_all_regex(May4all$text,
                                       pattern = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
                                                   "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
                                                   "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related"),
                                       replacement = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                                       vectorize = FALSE)

May5all$text <- stri_replace_all_regex(May5all$text,
                                       pattern = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
                                                   "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
                                                   "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related"),
                                       replacement = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                                       vectorize = FALSE)


#Extracts hashtags from text 
hashtags <- str_extract_all(May2345$text, "#\\S+")
hashtags <- unlist(hashtags)

hashtags2 <- str_extract_all(May2all$text, "#\\S+")
hashtags2 <- unlist(hashtags2)

hashtags3 <- str_extract_all(May3all$text, "#\\S+")
hashtags3 <- unlist(hashtags3)
hashtags3

hashtags4 <- str_extract_all(May4all$text, "#\\S+")
hashtags4 <- unlist(hashtags4)
hashtags4

hashtags5 <- str_extract_all(May5all$text, "#\\S+")
hashtgas5 <- unlist(hashtags5)

#delete all punctuation
hashtags <- gsub("[[:punct:] ]+", "", hashtags)

hashtags2 <- gsub("[[:punct:] ]+", "", hashtags2)

hashtags3 <- gsub("[[:punct:] ]+", "", hashtags3)

hashtags4 <- gsub("[[:punct:] ]+", "", hashtags4)

hashtags5 <- gsub("[[:punct:] ]+", "", hashtags5)

#This makes hashtags into a viewable table by decreasing order 
Hcount <- table(hashtagstest)
Hcount <- sort(Hcount, decreasing = TRUE)

Hcount2 <- table(hashtags2)
Hcount2 <- sort(Hcount2, decreasing = TRUE)

Hcount3 <- table(hashtags3)
Hcount3 <- sort(Hcount3, decreasing = TRUE)

Hcount4 <- table(hashtags4)
Hcount4 <- sort(Hcount4, decreasing = TRUE)

Hcount5 <- table(hashtags5)
Hcount5 <- sort(Hcount5, decreasing = TRUE)

#This counts unique users
UniqueUsers <- count(May2345, user_id_str)
duplicated(UniqueUsers) #checks to see if there is any duplicated users

UniqueUsers2 <- count(May2all, user_id_str)
duplicated(UniqueUsers2)

UniqueUsers3 <- count(May3all, user_id_str)
duplicated(UniqueUsers3)

UniqueUsers4 <- count(May4all, user_id_str)
duplicated(UniqueUsers4)

UniqueUsers5 <- count(May5all, user_id_str)
duplicated(UniqueUsers5)

#Top 300 most frequent hashtags / unique users
b <- Hcount[2:300] / 1786193
b <- sort(b, decreasing = TRUE)

b2 <- Hcount2[2:300] / 487719
b2 <- sort(b2, decreasing = TRUE)

b3 <-Hcount3[2:301] / 415262
b3 <- sort(b3, decreasing = TRUE)
  
b4 <-Hcount4[3:302] / 818878
b4 <- sort(b4, decreasing = TRUE)
  
b5 <-Hcount5[3:302] / 706013
b5 <- sort(b5, decreasing = TRUE)

#Final Data frame
May2345Final <- data.frame(b)

May2ndFinal <- data.frame(b2)

May3rdFinal <- data.frame(b3)

May4thFinal <- data.frame(b4)

May5thFinal <- data.frame(b5)

write.csv(May2345Final, "/Users/robertmoncrief/Desktop/Twitter Data/May2345Final.csv")
write.csv(May2ndFinal, "/Users/robertmoncrief/Desktop/Twitter Data/May2ndFinal.csv")
write.csv(May3rdFinal, "/Users/robertmoncrief/Desktop/Twitter Data/May3rdFinal.csv")
write.csv(May4thFinal, "/Users/robertmoncrief/Desktop/Twitter Data/May4thFinal.csv")
write.csv(May5thFinal, "/Users/robertmoncrief/Desktop/Twitter Data/May5thFinal.csv")



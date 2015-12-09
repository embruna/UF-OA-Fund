library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)


setwd("/Users/emiliobruna/Dropbox/SHARED FOLDERS/UF Open Access Fund")

#Clear out everything from the environment
rm(list=ls())

######################################################
######################################################
### DATA ENTRY AND CLEANUP
######################################################
######################################################
#Step 1: load the CSV file and save as dataframes

OAF<-read.csv("UFOAFData_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE) #need to save strings so tha you can error correct
#Make some corrections to the data
OAF$College[OAF$Last.Name == "Emery"]  <- "FLMNH" #K. Emery  is in FLMNH, College was incorrectly ccoded as CLAS
OAF$Department[OAF$Last.Name == "Rey"]  <- "EntNemat" #FMEL is the lab in the department
OAF$Journal.Publication[OAF$Journal.Publication == "Frontiers in Plant Proteomics" & OAF$First.Name == "Annalisa"]<- "Frontiers in Plant Science"  #Was published in a diferent journal than in database
str(OAF)
OAF <- data.frame(OAF, stringsAsFactors=FALSE)  #Convert strings back to factors to be able to plot
#JOURNAL STATS
Journal.Count<-n_distinct(OAF$Journal.Publication)
Journal.Table<-as.data.frame(count(OAF,Journal.Publication))
Journal.Table <- Journal.Table[order(-Journal.Table$n),] 
PLOS.ONE.Percent<-(max(Journal.Table$n) / sum(Journal.Table$n))*100

#COLLEGE STATS
College.Count<-n_distinct(OAF$College)
College.Table<-as.data.frame(count(OAF,College))
College.Table <- College.Table[order(-College.Table$n),] 
College.Table$percentage <- College.Table$n/sum(College.Table$n)*100

#DEPARTMENT STATS
Department.Count<-n_distinct(OAF$Department)
Department.Table<-as.data.frame(count(OAF,Department))
Department.Table <- Department.Table[order(-Department.Table$n),] 
Department.Table$percentage <- Department.Table$n/sum(Department.Table$n)*100

#PI STATS
PI.Count<-n_distinct(OAF$Last.Name)
PI.Table<-as.data.frame(count(OAF,Last.Name))
PI.Table <- PI.Table[order(-PI.Table$n),] 
PI.Table$percentage <- PI.Table$n/sum(PI.Table$n)*100

OAF <- transform(OAF, Journal.Publication = reorder(Journal.Publication))
jrnl.fig <- ggplot(OAF, aes(factor(Journal.Publication)))  #Factor converts the string to a factor, allowing you to count them for the plot
jrnl.fig<-jrnl.fig + geom_bar()  # By default, uses stat="bin", which gives the count in each category
jrnl.fig


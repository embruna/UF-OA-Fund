library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(here)

# setwd("/Users/emiliobruna/Dropbox/SHARED FOLDERS/UF Open Access Fund")

#Clear out everything from the environment
# rm(list=ls())

######################################################
######################################################
### DATA ENTRY AND CLEANUP
######################################################
######################################################
#Step 1: load the CSV file and save as dataframes

OAF <- read.csv("UFOAFData_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE) #need to save strings so tha you can error correct (ERS: this is now default behavior as of a recent R version)
#Make some corrections to the data
OAF$College[OAF$Last.Name == "Emery"]  <- "FLMNH" #K. Emery  is in FLMNH, College was incorrectly ccoded as CLAS
OAF$Department[OAF$Last.Name == "Rey"]  <- "EntNemat" #FMEL is the lab in the department
OAF$Journal.Publication[OAF$Journal.Publication == "Frontiers in Plant Proteomics" & OAF$First.Name == "Annalisa"]<- "Frontiers in Plant Science"  #Was published in a diferent journal than in database
str(OAF)
OAF <- data.frame(OAF, stringsAsFactors=FALSE)  #Convert strings back to factors to be able to plot
#ERS: I don't think that's what this does...


#JOURNAL STATS
Journal.Count <- n_distinct(OAF$Journal.Publication) #count how many different journals
Journal.Table <- as.data.frame(count(OAF,Journal.Publication)) #creata a dataframe with the count of how many in each journal and and rename it
Journal.Table <- Journal.Table[order(-Journal.Table$n),] #sort this from highest to lowest
Journal.Table$Percent <- Journal.Table$n/sum(Journal.Table$n)*100 #add a column with the percentage
names(Journal.Table)[names(Journal.Table)=="Journal.Publication"] <- "Journal" #rename the column so it looks nicer in the table
names(Journal.Table)[names(Journal.Table)=="n"] <- "N" #rename the column so it looks nicer in the table
write.csv(Journal.Table, file = here("output", "Articles_by_Journal.csv"), row.names = F) #export it as a csv file


#COLLEGE STATS
# Same as above, but for colleges
College.Count<-n_distinct(OAF$College)
College.Table<-as.data.frame(count(OAF,College))
College.Table <- College.Table[order(-College.Table$n),] 
College.Table$Percent <- College.Table$n/sum(College.Table$n)*100
names(College.Table)[names(College.Table)=="n"] <- "N" #rename the column so it looks nicer in the table
write.csv(College.Table, file=here("output", "Articles_by_College.csv"), row.names = F) #export it as a csv file


#DEPARTMENT STATS
# Same but for departments
Department.Count<-n_distinct(OAF$Department)
Department.Table<-as.data.frame(count(OAF,Department))
Department.Table <- Department.Table[order(-Department.Table$n),] 
Department.Table$Percent <- Department.Table$n/sum(Department.Table$n)*100
names(Department.Table)[names(Department.Table)=="n"] <- "N" #rename the column so it looks nicer in the table
write.csv(Department.Table, file = here("output", "Articles_by_Department.csv"), row.names = F) #export it as a csv file


#PI STATS
#Same but for PIs
PI.Count<-n_distinct(OAF$Last.Name)
PI.Table<-as.data.frame(count(OAF,Last.Name, First.Name, College, Department))
PI.Table <- PI.Table[order(-PI.Table$n),] 
PI.Table$Percent <- PI.Table$n/sum(PI.Table$n)*100
names(PI.Table)[names(PI.Table)=="n"] <- "N" #rename the column so it looks nicer in the table
write.csv(PI.Table, file = here("output", "Articles_by_PI.csv"), row.names = F) #export it as a csv file





#FIGURES

###BY JOURNAL (plos one throws it, need to cut bar and pool least common)

# ERS: This line errors currently.  Probably needs some love from the `forcats` package.
# OAF <- transform(OAF, Journal.Publication = reorder(Journal.Publication))


jrnl.fig <- ggplot(OAF, aes(factor(Journal.Publication)))  #Factor converts the string to a factor, allowing you to count them for the plot
jrnl.fig <- jrnl.fig +
  geom_bar() + # By default, uses stat="bin", which gives the count in each category
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #ERS: not sure this helps much
jrnl.fig


### BY COLLEGE
# first reorder the dataframe so that it will sort the colleges from most to least common. if not, it will plot colleges in the order they appear in original dataframe
# see http://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
OAF <- within(OAF, College <- factor(College, levels=names(sort(table(College),decreasing=TRUE)))) 
## plot
College.fig<-ggplot(OAF,aes(x=College))+geom_bar()+    #histogram in ggplot2
  xlab("College") +                                              #change the X and Y labels
  ylab("N") +
  scale_y_continuous(breaks=seq(0, 65, 5))+                      #change the y axis to go from 0-60 by 5
  ggtitle("OA Fund Publications by College")                     #add a title

 College.fig <-College.fig + theme_classic()+theme(axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),    #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                              axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                              axis.text=element_text(colour="black", size = 16),                        #changes size, color, of labels on axes
                                              plot.title = element_text(colour="black", size = 22, vjust = -1),         #changes size, font, location of title
                                              axis.text.x=element_text(angle = -45, hjust = 0))                         #sets angle of labels on the axes to diagonal
College.fig
ggsave(here("output", "Figure-Publications_by_College.pdf"))

### BY YEAR
# change year to ordered factor
OAF <- within(OAF, Publication.Year <- factor(Publication.Year, levels=names(sort(table(Publication.Year),decreasing=TRUE)))) 
## plot
OAF$Publication.Year<-factor(OAF$Publication.Year)
OAF$Publication.Year<-ordered(OAF$Publication.Year, levels = c("2010", "2011", "2012", "2013", "2014"))

Year.fig <-
  ggplot(OAF, aes(x = Publication.Year)) +
  geom_bar() +    #histogram in ggplot2
  xlab("Year") +                                              #change the X and Y labels
  ylab("N") +
  scale_y_continuous(breaks = seq(0, 105, 15)) +                      #change the y axis to go from 0-60 by 5
  ggtitle("OA Fund Articles: Year of Publication")                     #add a title

Year.fig <-Year.fig + theme_classic()+theme(axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),    #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                  axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                  axis.text=element_text(colour="black", size = 16),                        #changes size, color, of labels on axes
                                                  plot.title = element_text(colour="black", size = 22, vjust = -1),         #changes size, font, location of title
                                                  axis.text.x=element_text(angle = -45, hjust = 0))                         #sets angle of labels on the axes to diagonal
Year.fig
ggsave(here("output", "Figure-Publications_by_Year.pdf"))


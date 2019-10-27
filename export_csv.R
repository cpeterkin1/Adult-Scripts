#Import packages
#install.packages("rlang")
#install.packages("tidyverse",dependencies=TRUE)
#install.packages("ggplot2",dependencies=TRUE)
library(ggplot2)
library(plyr)

#Import the data from a url
#theURL<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
#adult.data<-read.table(file=theURL,header=FALSE,sep=",",strip.white=TRUE,stringsAsFactors=TRUE,col.names=c("age","workclass","fnlwgt","education","educationnum","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","nativecountry","income"))
#dim(adult.data)

#Import the data from desktop
setwd("C:/Users/Chris/Desktop/R Scripts/Adult/Adult Data")
adult.data<-read.table("adult.data",sep=",",header=FALSE,na.strings=" ?")

#omit missing values
adult.data<-na.omit(adult.data)

#re-enumerate number of rows
row.names(adult.data)<-1:nrow(adult.data)

#column names
colnames(adult.data)<-c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","income")

#view data for accuracy
str(adult.data,vec.len=2,strict.width="no",width=30)

#factor levels function
levels_factors<-function(mydata) {
  col_names<-names(mydata)
  for (i in 1:length(col_names)) {
    if (is.factor(mydata[, col_names[i]])) {
      message(noquote(paste("Covariate ","*",col_names[i],"*"," with factor levels:",sep="")))
      print(levels(mydata[,col_names[i]]))
    }
  }
}

#view factor levels
levels_factors(adult.data)

#box plot for hours_per_week
ggplot(aes(x=factor(0),y=hours_per_week),data=adult.data)+
  geom_boxplot()+
  stat_summary(fun.y=mean,geom='point',shape=19,color="red",cex=2)+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(breaks=seq(0,100,5))+
  xlab(label="")+
  ylab(label="Working hour per week")+
  ggtitle("Box Plot of Working Hours per Week")

#categorize hours per week
adult.data$hours_w[adult.data$hours_per_week<40]<-" less_than_40"
adult.data$hours_w[adult.data$hours_per_week>=40 & adult.data$hours_per_week<=45]<-" between_40_and_45"
adult.data$hours_w[adult.data$hours_per_week>45 & adult.data$hours_per_week<=60]<-" between_45_and_60"
adult.data$hours_w[adult.data$hours_per_week>60 & adult.data$hours_per_week<=80]<-" between_60_and_80"
adult.data$hours_w[adult.data$hours_per_week>80]<-" more_than_80"

#make factor variable - hours per week
adult.data$hours_w<-factor(adult.data$hours_w,ordered=FALSE,levels=c(" less_than_40"," between_40_and_45"," between_45_and_60"," between_60_and_80"," more_than_80"))

#check results
summary(adult.data$hours_w)

#as percentage
for(i in 1:length(summary(adult.data$hours_w))) {
  print(round(100*summary(adult.data$hours_w)[i]/sum(!is.na(adult.data$hours_w)),2))
}

#make factor variable - native region
Asia_East <- c(" Cambodia", " China", " Hong", " Laos", " Thailand",
               " Japan", " Taiwan", " Vietnam")
Asia_Central <- c(" India", " Iran")
Central_America <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", 
                     " Puerto-Rico",  " Dominican-Republic", " El-Salvador", 
                     " Haiti", " Honduras", " Mexico", " Trinadad&Tobago")
South_America <- c(" Ecuador", " Peru", " Columbia")
Europe_West <- c(" England", " Germany", " Holand-Netherlands", " Ireland", 
                 " France", " Greece", " Italy", " Portugal", " Scotland")
Europe_East <- c(" Poland", " Yugoslavia", " Hungary")

#mutate data
adult.data <- mutate(adult.data, native_region = ifelse(native_country %in% Asia_East, " East-Asia",
                                                ifelse(native_country %in% Asia_Central, " Central-Asia",
                                                ifelse(native_country %in% Central_America, " Central-America",
                                                ifelse(native_country %in% South_America, " South-America",
                                                ifelse(native_country %in% Europe_West, " Europe-West",
                                                ifelse(native_country %in% Europe_East, " Europe-East",
                                               ifelse(native_country == " United-States", " United-States", " Outlying-US" ))))))))

#import
adult.data$native_region <- factor(adult.data$native_region, ordered = FALSE)

#capital gain and loss
adult.data <- mutate(adult.data, 
                   cap_gain = ifelse(adult.data$capital_gain < 3464, " Low",
                                     ifelse(adult.data$capital_gain >= 3464 & 
                                              adult.data$capital_gain <= 14080, " Medium", " High")))


adult.data$cap_gain <- factor(adult.data$cap_gain,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))

adult.data <- mutate(adult.data, 
                   cap_loss = ifelse(adult.data$capital_loss < 1672, " Low",
                                     ifelse(adult.data$capital_loss >= 1672 & 
                                              adult.data$capital_loss <= 1977, " Medium", " High")))


adult.data$cap_loss <- factor(adult.data$cap_loss,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))

#workclass
adult.data$workclass <- droplevels(adult.data$workclass)

#test data
adult.test <- read.table("adult.test",
                      sep = ",", 
                      header = FALSE, 
                      skip = 1, 
                      na.strings = " ?")

colnames(adult.test) <- c("age", "workclass", "fnlwgt", "education",
                       "education_num", "marital_status", "occupation",
                       "relationship", "race", "sex", "capital_gain",
                       "capital_loss", "hours_per_week",
                       "native_country", "income")

#clean NA
adult.test <- na.omit(adult.test)

row.names(adult.test) <- 1:nrow(adult.test)

#clean test data
levels(adult.test$income)[1] <- " <=50K"
levels(adult.test$income)[2] <- " >50K"

#hours per week
adult.test$hours_w[adult.test$hours_per_week < 40] <- " less_than_40"
adult.test$hours_w[adult.test$hours_per_week >= 40 & 
                  adult.test$hours_per_week <= 45] <- " between_40_and_45"
adult.test$hours_w[adult.test$hours_per_week > 45 &
                  adult.test$hours_per_week <= 60  ] <- " between_45_and_60"
adult.test$hours_w[adult.test$hours_per_week > 60 &
                  adult.test$hours_per_week <= 80  ] <- " between_60_and_80"
adult.test$hours_w[adult.test$hours_per_week > 80] <- " more_than_80"

#native region
adult.test <- mutate(adult.test, 
                  native_region = ifelse(native_country %in% Asia_East, " East-Asia",
                                  ifelse(native_country %in% Asia_Central, " Central-Asia",
                                  ifelse(native_country %in% Central_America, " Central-America",
                                 ifelse(native_country %in% South_America, " South-America",
                                ifelse(native_country %in% Europe_West, " Europe-West",
                               ifelse(native_country %in% Europe_East, " Europe-East",
                              ifelse(native_country == " United-States", " United-States", " Outlying-US" ))))))))


adult.test$native_region <- factor(adult.test$native_region, ordered = FALSE)

#capital gain and loss
adult.test <- mutate(adult.test, 
                  cap_gain = ifelse(adult.test$capital_gain < 3464, " Low",
                                    ifelse(adult.test$capital_gain >= 3464 & 
                                             adult.test$capital_gain <= 14080, " Medium", " High")))

adult.test$cap_gain <- factor(adult.test$cap_gain,
                           ordered = FALSE,
                           levels = c(" Low", " Medium", " High"))
adult.test<- mutate(adult.test, 
                 cap_loss = ifelse(adult.test$capital_loss < 1672, " Low",
                                   ifelse(adult.test$capital_loss >= 1672 & 
                                            adult.test$capital_loss <= 1977, " Medium", " High")))


adult.test$cap_loss <- factor(adult.test$cap_loss,
                           ordered = FALSE,
                           levels = c(" Low", " Medium", " High"))


adult.test$hours_w <- factor(adult.test$hours_w,
                          ordered = FALSE,
                          levels = c(" less_than_40", 
                                     " between_40_and_45", 
                                     " between_45_and_60",
                                     " between_60_and_80",
                                     " more_than_80"))

adult.test$workclass <- droplevels(adult.test$workclass)

#export data
write.csv(adult.data, "adult_df.csv", row.names = FALSE)

write.csv(adult.test, "test_df.csv", row.names = FALSE)
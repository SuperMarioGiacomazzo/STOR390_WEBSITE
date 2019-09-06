setwd("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
Section=read_excel("Roster.xlsx")[,1]

#Function to Divide Sections into Groups
Group.select.func<-function(data,random.seed){
  student.names=data$Name
  set.seed(random.seed)
  random.order=sample(1:length(student.names),replace=F)
  group=tibble(Order=random.order,Name=student.names) %>%
          arrange(Order) %>%
          mutate(Group=rep(1:(length(student.names)%/%5+(length(student.names)%%5!=0)),
                           each=5,length=length(student.names))) %>%
          select(-Order) %>%
          arrange(Group)
  return(group)
}

#######################################
#Selecting Groups for Game-Day Speeches
#######################################

#GS.1=Group.select.func(Section,216)
#write_csv(GS.1,path=str_c(getwd(),"/GS1 Group Assignments.csv"))

#GS.2=Group.select.func(Section,480)
#write_csv(GS.2,path=str_c(getwd(),"/GS2 Group Assignments.csv"))

GS.3=Group.select.func(Section,440)
write_csv(GS.2,path=str_c(getwd(),"/GS3 Group Assignments.csv"))
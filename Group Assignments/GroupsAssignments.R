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

#Selecting Groups for Game-Day Speeches
GDS.1=Group.select.func(Section,216)
write_csv(GDS.1,path=str_c(getwd(),"/GDS1 Group Assignments.csv"))


#Selecting Groups for Playoffs (Round 1)

#Selecting Groups for Playoffs (Round 2)

#Save Datasets
write_csv(Final.Section,path=str_c(getwd(),"/STOR390 Group Assignments.csv"))

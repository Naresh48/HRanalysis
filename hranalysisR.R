data<-read.csv(choose.files())
View(data)

library('dplyr')

get_mode=function(v){
  uniq=unique(v)
  as.character(uniq[which.max(tabulate(match(v,uniq)))])
}

nikit=function(y){
  uniq=unique(y)
  tabulate(match(y,uniq))
}

#which gender has more monthly income
data%>%
  group_by(Gender)%>%
  summarise(maxincome=max(MonthlyIncome,na.rm = TRUE))
#ans.male

#which marital status prefers business travel
data%>%
  group_by(MaritalStatus)%>%
  filter(BusinessTravel=="Travel_Frequently")%>%
  summarise(x=nikit(BusinessTravel))
#married persons prefers travel

#which education field helps you to earn more
data%>%
  group_by(EducationField)%>%
  summarise(c=max(DailyRate,na.rm = TRUE))
#ans.Marketing

#which gender prefers working after the job time
data%>%
  group_by(Gender)%>%
  filter(OverTime=="Yes")%>%
  summarise(z=nikit(OverTime))
#ans.male

#which department are more satisfied with thier job
data%>%
  group_by(Department)%>%
  summarise(b=get_mode(JobSatisfaction))
#ans.sales

#which department has high daily rate
data%>%
  group_by(Department)%>%
  summarise(d=max(DailyRate))
#as.sales 

anova_var <- aov(data$Age~data$JobRole,data)
summary(anova_var)

anova_p<-anova(anova_var)$'Pr(>F)'[1]
anova_p

if(anova_p<0.05){
  print("there is relation between age and job role")
}else{
  print("there is no relation between age and job role")
}


chi<-chisq.test(data$Gender,data$JobRole)
chi_p<-chi$p.value
chi
chi_p

if(chi_p<0.05){
  print("there is relation between job role and gender")
}else{
  print("there is no relation between job role and gender")
}

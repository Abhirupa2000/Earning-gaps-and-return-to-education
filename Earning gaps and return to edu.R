library(tidyverse)
library(stargazer)

load("D:/Books, Notes/APU/SEM 1/Quantitative and Research methodology/Working Directory/IHDS.Rdata")


IHDS1$ID13<-factor(IHDS1$ID13,levels=c(1,2,3,4,5,6),labels=c("Brahmin","General","OBC","SC","ST","Others"))
IHDS1$RO3<-factor(IHDS1$RO3,levels=c(1,2),labels=c("Male","Female"))
IHDS1$ID11<-factor(IHDS1$ID11,levels=c(1,2,3,4,5,6,7,8,9),labels=c("Hindu","Muslim","Christian","Sikh","Budhhist","Jain","Tribals", "Others" ,"None"))

# Mean of WSEARN by gender, caste, religion, and type of primary occupation
WSEARN.mean.gender <- IHDS1 %>% 
  group_by(RO3) %>% 
  summarise(mean.earnings = weighted.mean(WSEARN, WT, na.rm=T))

stargazer(as.data.frame(WSEARN.mean.gender),
          type="html",
          title="Average Annual Earnings Per person by Gender(in Rs.)",
          summary=FALSE,
          covariate.labels=c("S.No.", "Gender", "Average Earnings"),
          digits=1,
          notes=c("Source: IHDS-2, 2011-12 "),
          out=c("mean earnings.gender.html"))

WSEARN.mean.caste <- IHDS1 %>% 
  group_by(ID13) %>% 
  summarise(mean.earnings = weighted.mean(WSEARN, WT, na.rm=T)) %>% 
  na.omit()

stargazer(as.data.frame(WSEARN.mean.caste),
          type="html",
          title="Average Annual Earnings Per person by Caste (in Rs.)",
          summary=FALSE,
          covariate.labels=c("S.No.", "Caste", "Average Earnings"),
          digits=1,
          notes=c("Source: IHDS-2, 2011-12 "),
          out=c("mean earnings.caste.html"))


WSEARN.mean.religion <- IHDS1 %>% 
  group_by(ID11) %>% 
  summarise(mean.earnings = weighted.mean(WSEARN, WT, na.rm=T)) %>% 
  na.omit()

stargazer(as.data.frame(WSEARN.mean.religion),
          type="html",
          title="Average Annual Earnings Per person by Religion (in Rs.)",
          summary=FALSE,
          covariate.labels=c("S.No.", "Religion", "Average Earnings"),
          digits=1,
          notes=c("Source: IHDS-2, 2011-12 "),
          out=c("mean earnings.religion.html"))

WSEARN.mean.occ <- IHDS1 %>% 
  group_by(RO7) %>% 
  summarise(mean.earnings = weighted.mean(WSEARN, WT, na.rm=T)) %>% 
  na.omit()

stargazer(as.data.frame(WSEARN.mean.occ),
          type="html",
          title="Average Annual Earnings Per person by Primary Occupation (in Rs.)",
          summary=FALSE,
          covariate.labels=c("S.No.", "Primary Occupation", "Average Earnings"),
          digits=1,
          notes=c("Source: IHDS-2, 2011-12 "),
          out=c("mean earnings.occ.html"))

# Regression 1: Regressing log earnings on a gender dummy with survey weights to find the aggregate earnings gap.

log.earnings <- IHDS1 %>% 
  mutate(log.WSEARN = log(WSEARN))

IHDS1 <- IHDS1 %>% 
  mutate(gender.dum= ifelse(RO3==1,0, ifelse(RO3==2,1,NA)))

reg1 <- lm(log(WSEARN) ~ gender.dum, data=IHDS1, weights=WT)
stargazer(reg1, type='text')  


# Adding age, age squared, and education to see the change in coefficient. 

reg2 <- lm(log(WSEARN) ~ gender.dum + RO5 + I(RO5^2) + ED6, data=IHDS1, weights=WT)
stargazer(reg2, type='text')  


# Adding religion, caste, and primary activity status one at a time to see which one makes the biggest difference to the gender coefficient

reg1.rel <- lm(log(WSEARN) ~ gender.dum + RO5 + I(RO5^2) + ED6 + ID11, data=IHDS1, weights=WT)
stargazer(reg1.rel, type='text') 

reg1.cas <- lm(log(WSEARN) ~ gender.dum + RO5 + I(RO5^2) + ED6 + ID11 + ID13, data=IHDS1, weights=WT)
stargazer(reg1.cas, type='text') 

reg1.occ <- lm(log(WSEARN) ~ gender.dum + RO5 + I(RO5^2) + ED6 + ID11 + ID13 + RO7, data=IHDS1, weights=WT)
stargazer(reg1.occ, type='text') 

stargazer(reg1.rel, reg1.cas, reg1.occ, type = 'html', out = c("table05.allregs.html"))


# Regression 2: Estimating returns to education using a Mincerian earnings equation

IHDS2 <- IHDS1 %>%  filter(INCNONAG>0)


# return to education for casual wage

reg.wage <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), data=IHDS2, weights=WT)
stargazer(reg.wage, type='text') 


# return to education for salary

IHDS3 <- IHDS1 %>%  filter(INCSALARY>0)

reg.salary <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), data=IHDS3, weights=WT)
stargazer(reg.salary, type='text')


# regression by gender: Female and casual wage

IHDS.f.noag <- IHDS1 %>%  filter(RO3==2) %>% filter(INCNONAG>0)
IHDS.f.sal <- IHDS1 %>%  filter(RO3==2) %>% filter(INCSALARY>0)

IHDS.m.noag <- IHDS1 %>%  filter(RO3==1) %>% filter(INCNONAG>0)
IHDS.m.sal <- IHDS1 %>%  filter(RO3==1) %>% filter(INCSALARY>0)


# female and non agri
reg.f.noag <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), data=IHDS.f.noag, weights=WT)
stargazer(reg.f.noag, type = 'text')


# female and salary
reg.f.sal <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), data=IHDS.f.sal, weights=WT)
stargazer(reg.f.sal, type = 'text')


# male and non agri
reg.m.noag <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), data=IHDS.m.noag, weights=WT)
stargazer(reg.m.noag, type = 'text')


# male and salary
reg.m.sal <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), data=IHDS.m.sal, weights=WT)
stargazer(reg.m.sal, type = 'text')



# regression education: stratifying the regression by level of education
# calculate returns to an additional year of schooling for those who have more than 10 years of education

IHDS.noag.edu <- IHDS1 %>%  filter(ED6>10, INCNONAG>0)
IHDS.sal.edu <- IHDS1 %>%  filter(ED6>10, INCSALARY>0)

reg.noag.edu <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), data = IHDS.noag.edu, weights=WT )
reg.sal.edu <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), data = IHDS.sal.edu, weights=WT)

stargazer(reg.noag.edu,reg.sal.edu, type = 'text')
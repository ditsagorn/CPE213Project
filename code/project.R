library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(lattice)
library(rpart)
student <- read.csv("StudentsPerformance.csv")
pre <- student %>% 
  select(gender, parental.level.of.education,test.preparation.course:writing.score)
pre


#------------------------------------------------------------------------#
pre_math <- student %>% 
  select(gender,parental.level.of.education,test.preparation.course,math.score)%>%
  group_by(parental.level.of.education)%>%
  summarise(avg_math = mean(math.score))
pre_math

ggplot(pre,aes(math.score))+geom_histogram(color = "black",fill = "blue")+ggtitle("Math score")
ggplot(pre_math)+geom_col(mapping = aes(parental.level.of.education,avg_math,fill = parental.level.of.education))+ggtitle("Pre Math")

#------------------------------------------------------------------------#

test_math<- student %>%
  select(gender,test.preparation.course,math.score)%>%
  group_by(test.preparation.course)%>%
  summarise(avg_math = mean(math.score))
ggplot(test_math)+geom_col(mapping=aes(test.preparation.course,avg_math,fill = test.preparation.course))+ggtitle("Prepare Math")



#------------------------------------------------------------------------#
pre_read <- student %>% 
  select(gender,parental.level.of.education,test.preparation.course,reading.score)%>%
  group_by(parental.level.of.education)%>%
  summarise(avg_read = mean(reading.score))
pre_read

ggplot(pre,aes(reading.score))+geom_histogram(color = "black",fill = "blue")+ggtitle("Reading score")
ggplot(pre_read)+geom_col(mapping = aes(parental.level.of.education,avg_read,fill = parental.level.of.education))+ggtitle("Pre Read")
#------------------------------------------------------------------------#

test_read<- student %>%
  select(gender,test.preparation.course,reading.score)%>%
  group_by(test.preparation.course)%>%
  summarise(avg_read = mean(reading.score))
ggplot(test_read)+geom_col(mapping=aes(test.preparation.course,avg_read,fill = test.preparation.course))+ggtitle("Prepare Read")


#------------------------------------------------------------------------#

pre_write <- student %>% 
  select(gender,parental.level.of.education,test.preparation.course,writing.score)%>%
  group_by(parental.level.of.education)%>%
  summarise(avg_write = mean(writing.score))
pre_write
ggplot(pre,aes(writing.score))+geom_histogram(color = "black",fill = "blue")+ggtitle("Writing score")
ggplot(pre_write)+geom_col(mapping = aes(parental.level.of.education,avg_write,fill = parental.level.of.education))+ggtitle("Pre Write")
#------------------------------------------------------------------------#

test_write <- student %>%
  select(gender,test.preparation.course,writing.score)%>%
  group_by(test.preparation.course)%>%
  summarise(avg_write = mean(writing.score))
ggplot(test_write)+geom_col(mapping=aes(test.preparation.course,avg_write,fill = test.preparation.course))+ggtitle("Prepare Write")

#------------------------------------------------------------------------#
#Model implement#
ggplot(student,aes(math.score,writing.score))+geom_point()+geom_smooth(method = "lm")+ggtitle("Writing and Math")
lm_WM <- lm(writing.score~ math.score,data = student)
summary(lm_WM)
cor(student$math.score,student$writing.score)
#------------------------------------------------------------------------#

ggplot(student,aes(math.score,reading.score))+geom_point()+geom_smooth(method = "lm")+ggtitle("reading and Math")
lm_RM <- lm(reading.score~ math.score,data = student)
summary(lm_RM)
cor(student$math.score,student$reading.score)
#------------------------------------------------------------------------#

ggplot(student,aes(writing.score,reading.score))+geom_point()+geom_smooth(method = "lm")+ggtitle("reading and writing")
lm_RW <- lm(reading.score~ writing.score,data = student)
summary(lm_RW)
cor(student$writing.score,student$reading.score)
#-----------------------------------------------------------------------#
#ADD#
lm_ADD1 <- lm(writing.score~ math.score+reading.score,data = student)
summary(lm_ADD1)
MR1 <- student$math.score+student$reading.score
ggplot(student,aes(MR1,writing.score))+geom_point()+geom_smooth(method = "lm")
#-----------------------------------------------------------------------#
lm_ADD2 <- lm(reading.score~ math.score+writing.score,data = student)
summary(lm_ADD2)
MW1 <- student$math.score+student$writing.score
ggplot(student,aes(MW1,writing.score))+geom_point()+geom_smooth(method = "lm")
#-----------------------------------------------------------------------#
#MUL#
lm_MUL1 <- lm(writing.score~ math.score*reading.score,data = student)
summary(lm_MUL1)
MR2 <- student$math.score*student$reading.score
ggplot(student,aes(MR2,writing.score))+geom_point()+geom_smooth(method = "lm")
#-----------------------------------------------------------------------#
lm_MUL2 <- lm(reading.score~ math.score*writing.score,data = student)
summary(lm_MUL2)
MW2 <- student$math.score*student$writing.score
ggplot(student,aes(MW2,writing.score))+geom_point()+geom_smooth(method = "lm")
#-----------------------------------------------------------------------#
#Evulation#
ST <- factor(student$test.preparation.course)
md <- glm(ST ~ math.score+writing.score+reading.score,data = student, family= "binomial")
std_t <- predict(md,student,type = "response")
std_a <- factor(ifelse(std_t > 0.4, 'completed','none'))
confusionMatrix(std_a, ST , positive = 'completed',mode = "prec_recall")


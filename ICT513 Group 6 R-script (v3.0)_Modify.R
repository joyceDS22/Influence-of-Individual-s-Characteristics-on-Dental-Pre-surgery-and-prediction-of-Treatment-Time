
setwd("E:/Master/ICT_513_Data Analytics_May'23/Project_Group 2-3/Dataset_UC")

dental <- read.csv("dentaldat.csv") 

## Load the libraries

library(jtools)
library(ggplot2)
library(tidyverse)
library(leaps)
library(car)

## Data -Pre Processing 

dental.eda <- dental %>% 
  mutate(age_group = case_when(Age_start_treatment <= 14~"Children",
                               Age_start_treatment >=15 & Age_start_treatment <= 29~"Young Adults", 
                               Age_start_treatment >= 30 & Age_start_treatment <=59~"Middle Aged Adults"))

## Descriptive Statistics of pre-surgery / Post / Treatment time

glimpse(dental.eda)

## Pre-surgery Time

m.pre <- mean(dental.eda$Pre_surgery_time,dental.eda = T) # 496.405 


std.pre <-sqrt(var(dental.eda$Pre_surgery_time,na.rm = T))# 222.00


# 3.1.1 > Distribution and variability of response variable (Pre-Surgery / Post Surgey / Total Treatment Time)

# Histogram of pre-surgery

hist(dental.eda$Pre_surgery_time,density = 20, breaks = 20, prob =T,
     xlab = "Pre-Surgey Time (Days)", 
     main = "Histogram of Pre-Surgery Time (Days)",col = "gold")

curve(dnorm(x, mean=m.pre, sd=std.pre), 
      col="gray1", lwd=0.5, add=TRUE, yaxt="n")    


## Histogram of Log (pre-Surgey time)

m.pre.log <- mean(d.2.new$log_PreS) # 6.106


std.pre.log <-sqrt(var(d.2.new$log_PreS))# 0.464 

hist(d.2.new$log_PreS,density = 20, breaks = 20, prob =T,
     xlab = "Log(Pre-Surgey Time) (Days)", 
     main = "Histogram of Log(Pre-Surgery Time) (Days)",col = "orange")

curve(dnorm(x, mean=m.pre.log, sd=std.pre.log), 
      col="gray1", lwd=0.5, add=TRUE, yaxt="n") 


## Post-Surgery Time

m.post <- mean(dental.eda$Post_surgery_time,dental.eda = T) # 189.18 


std.post <-sqrt(var(dental.eda$Post_surgery_time,na.rm = T))# 131.83


# Histogram of Post-surgery

hist(dental.eda$Post_surgery_time,density = 20, breaks = 20, prob =T,
     xlab = "Post-Surgey Time (Days)", 
     main = "Histogram of Post -Surgey Time (Days)",col = "red")

curve(dnorm(x, mean=m.post, sd=std.post), 
      col="black", lwd=0.5, add=TRUE, yaxt="n")

## Treatment Time

m.treat <- mean(dental.eda$Treatment_time,dental.eda = T) # 685.5897


std.treat <-sqrt(var(dental.eda$Treatment_time,na.rm = T))# 273.4494

# Histogram of Treatment Time

hist(dental.eda$Treatment_time,density = 20, breaks = 20, prob =T,
     xlab = "Treatment Time (Days)", 
     main = "Histogram of Treatment Time (Days)",col = "darkorchid")

curve(dnorm(x, mean=m.treat, sd=std.treat), 
      col="black", lwd=0.5, add=TRUE, yaxt="n")




## 3.1.2 Demographic (Sex.Age grp) X Pre-Surgey Time

## GG Boxplot > Age.group X Pre-Surgey Time

gg.age <- ggplot(dental.eda,aes(x = Pre_surgery_time,color = age_group)) +
  geom_boxplot()+
  labs(title="BoxPlot of Dental Pre-Surgey Time on Age Grp",
       x="Pre-Surgey time in (Days)", 
       y = "len") 

## Boxplot > sex X Pre-Surgey Time
gg.sex <- ggplot(dental.eda,aes(x = Pre_surgery_time,color = Sex)) +
  geom_boxplot()+
  labs(title="BoxPlot of Pre-Surgey Time on Sex",
       x="Pre-Surgey time in (Days)", 
       y = "len") 

## 3.1.3  (Surgical Corrections & Hospital ) X Pre-Surgey Time

## Box Plot > Skeletal Ap X pre -surgey time 
gg.Skeletal <-ggplot(data=dental.eda,aes(x=factor(Skeletal_AP),y=Pre_surgery_time,
                                         group=factor(Skeletal_AP)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Skeletal_AP)))+
  labs(title="BoxPlot of Pre-Surgey Time on Skeletal Ap",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > Vertical Ap X pre -surgey time 
gg.Vertical <-ggplot(data=dental.eda,aes(x=factor(Vertical),y=Pre_surgery_time,
                                         group=factor(Vertical)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Vertical)))+
  labs(title="BoxPlot of Pre-Surgey Time on Vertical",
       x="Dental Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > Transverse X pre -surgey time 
gg.Transverse <-ggplot(data=dental.eda,aes(x=factor(Transverse),y=Pre_surgery_time,
                                           group=factor(Transverse)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Transverse)))+
  labs(title="BoxPlot of Pre-Surgey Time on Transverse",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > Maxilla X pre -surgey time 
gg.Maxilla <-ggplot(data=dental.eda,aes(x=factor(Max),y=Pre_surgery_time,
                                        group=factor(Max)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Max)))+
  labs(title="BoxPlot of Pre-Surgey Time on Maxilla",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > 	Mandible  X Treatment Time
gg.Mandible <-ggplot(data=dental.eda,aes(x=factor(Mand),y=Pre_surgery_time,
                                         group=factor(Mand)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Mand)))+
  labs(title="BoxPlot of Pre-Surgey Time on Mandible",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > 	Single_Bimax   X Pre_surgery_time
gg.Single_Bimax <-ggplot(data=dental.eda,aes(x=factor(Single_Bimax),y=Pre_surgery_time,
                                             group=factor(Single_Bimax)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Single_Bimax)))+
  labs(title="BoxPlot of Pre-Surgey Time on Single Bimax",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > 	Exo  X Pre_surgery_time
gg.Exo <-ggplot(data=dental.eda,aes(x=factor(Exo),y=Pre_surgery_time,
                                    group=factor(Exo)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Exo)))+
  labs(title="BoxPlot of Pre-Surgey Time on Exo",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Box Plot > 	Hospital  X Pre_surgery_time

gg.hos <-ggplot(data=dental.eda,aes(x=factor(Hospital),y=Pre_surgery_time,
                                    group=factor(Hospital)))+
  geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Exo)))+
  labs(title="BoxPlot of Pre-Surgey Time on Hospital",
       x="Pre-Surgey Time in (Days)", 
       y = "len")

## Lm before PCA 

d.2 <- dental.eda

dental.lm.before <- lm (log(Pre_surgery_time)  ~ factor (age_group) + factor(Vertical) + factor(Transverse) + 
                          factor(Max) + factor(Mand) + factor(Single_Bimax)+factor(Exo)+factor (Skeletal_AP)+ factor(Other)+factor(Sex),data = d.2)

summary(dental.lm.before)



# 3.1.4 >  Diagnostics plot : Before PCA

qqnorm(dental.lm.before$residuals, 
       main = "Model Before PCA :
       Normal QQ Plot of Residual Fit")


qqline(dental.lm.before$residuals,col = "red")



# 3.2.2 Principal Component Analysis --------------------------------------------

d.1 <-dental

## ---  Convert Sex --- 
d.1$Sex <- ifelse(d.1$Sex=="M",0,1) ## M = 0 , F = 1

## ---  Convert Skeletal_AP --- 
d.1$Skeletal_AP <- ordered(d.1$Skeletal_AP,levels = c ("Cl I","Cl II","Cl III")) ## Cl I : 1 , Cl II : 2 , Cl III : 3 
d.1$Skeletal_AP<- as.numeric(d.1$Skeletal_AP) 

## --- Convert Vertical  --- 
d.1$Vertical<- ordered(d.1$Vertical,levels = c ("normo","hyper","hypo")) ## Normal : 1 , hyper : 2 , hypo : 3
d.1$Vertical<- as.numeric(d.1$Vertical) 

## --- Convert Transverse  --- 
d.1$Transverse<- ordered(d.1$Transverse,levels = c ("normal","narrow")) ## Normal : 1 ,narrow : 2
d.1$Transverse<- as.numeric(d.1$Transverse) 

## --- Convert Maxilla  --- 
d.1$Max<- ordered(d.1$Max,levels = c ("none","adv","setback")) ## none : 1, Adv : 2 , Setback : 3
d.1$Max<- as.numeric(d.1$Max) 

## --- Convert Mandible  --- 
d.1$Mand<- ordered(d.1$Mand,levels = c ("none","adv","setback")) ## none : 1, Adv : 2 , Setback : 3
d.1$Mand<- as.numeric(d.1$Mand) 

## --- Convert Single_Bi Max  --- 
d.1$Single_Bimax <- ordered(d.1$Single_Bimax,levels = c ("S","B")) ## S : 1, B : 2 
d.1$Single_Bimax<- as.numeric(d.1$Single_Bimax) 

## --- Convert Exo  --- 
d.1$Exo <- ifelse(d.1$Exo=="no",0,1) ## no = 0 , yes = 1

## --- Convert Other   --- 
d.1$Other <- ifelse(d.1$Other=="no",0,1)## no = 0 , yes = 1

## --- Convert Hospital   --- 
d.1$Hospital <- ifelse(d.1$Hospital=="Private",1,2)## Private : 1 , Public : 2


## Correlation Matrix

d.1.corr <- subset(d.1, select=-c(Number))

d.1.corr.new <- d.1.corr %>% mutate(log_Pre = log(d.1.corr$Pre_surgery_time))

glimpse(d.1.corr)

library(GGally)

ggcorr(d.1.corr.new,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

library(Hmisc)
library(broom)
library(DT)

d.1.corr.new %>% 
  as.matrix(.) %>% 
  rcorr(.) %>% 
  tidy(.) %>% 
  rename(variable_1 = column1,
         variable_2 = column2,
         corr = estimate) %>% 
  mutate(abs_corr = abs(corr)
  ) %>% 
  datatable(options = list(scrollX = T),
  ) %>% 
  formatRound(columns = c("corr", "p.value", "abs_corr"), 
              digits = 3)




# Covariance Matrix for all possible variable
d.1.cov <- cov(cbind(Age =d.1$Age_start_treatment,
                     Sex = d.1$Sex,
                     Skeletal_AP = d.1$Skeletal_AP,
                     Vertical = d.1$Vertical,
                     Transverse = d.1$Transverse,
                     Max = d.1$Max,
                     Mand =d.1$Mand, 
                     Single_Bimax = d.1$Single_Bimax,
                     Exo = d.1$Exo,
                     Other = d.1$Other,  ### NA
                     Hospital = d.1$Hospital,
                     Treatment_time = d.1$Treatment_time,
                     Pre_surgery_time = d.1$Pre_surgery_time,
                     Post_surgery_time = d.1$Post_surgery_time))


as.data.frame(d.1.cov)



## 3.2.1 > PCA > Pre-Surgey Time

library(psych)

pca <-prcomp( ~ Age_start_treatment + Sex + Skeletal_AP + Vertical+Transverse+Max+Mand+Single_Bimax+Exo
                    +Hospital+log(Pre_surgery_time),data = d.1,scale =TRUE,center =TRUE)

summary(pca)

## Scree plot to decide how many Component to use

library(factoextra)

fviz_screeplot(pca, addlabels = TRUE, choice = "eigenvalue")

fviz_screeplot(pca, addlabels = TRUE, choice = "variance",
               barfill = "lightgreen",
               xlab = "Prinicpal Component",
               main = "Scree Plot of Variance Accounted by Principal Component")



# 3.2.2 > Contributions of variables to PC1 & 2
library(factoextra)

fviz_contrib(pca, choice = "var", axes = 1:2,top = 10)

#### 3.2.3 > Fit Multiple Linear Regression after PCA

dental.lm.1 <- lm (log(Pre_surgery_time)  ~ factor(age_group) + factor(Vertical) + factor(Transverse) + 
                     factor(Max) + factor(Mand) + factor(Single_Bimax)+factor(Exo)+ factor(Skeletal_AP)+factor(Hospital),data = d.2)

summary(dental.lm.1)


## 3.2.3 > collinearity Test
library(car)

vif(dental.lm.1)


## Final LM Model after Collinearity Test

dental.lm.final <- lm (log(Pre_surgery_time)  ~ factor(age_group) + factor(Vertical) + factor(Transverse) + 
                     +factor(Exo)+ factor(Skeletal_AP)+factor(Hospital),data = d.2)


summ(dental.lm.final,digits =3,confint=TRUE)



## 3.2.4.1 > Diagnostics plot  >  LM FINAL 


qqnorm(dental.lm.final$residuals, 
       main = "Normal QQ Plot of Residual Fit")
       
qqline(dental.lm.final$residuals,col = "red")


# LM FINAL > Homoscedasticity and Independence check

plot(dental.lm.final$fitted.values,dental.lm.final$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Scatterplott of Residuals Vs Fitted Value")

abline(h=0,col = "red2")



# ## 3.2.5 >  Compare the characteristic of individual profile in relation to treatment time

denatl.2 <- dental

# Box plot of variable Hospital
ggplot(data=denatl.2, aes(x=factor(Hospital),y=Treatment_time,group=factor(Hospital)))+geom_boxplot(outlier.colour="red",outlier.shape=4, outlier.size=2,aes(fill=factor(Hospital)))

#Box plot of variable Exo
ggplot(data=denatl.2, aes(x=factor(Exo),y=Treatment_time,group=factor(Exo)))+geom_boxplot(outlier.colour="green", outlier.shape=4,outlier.size=2,aes(fill=factor(Exo)))

## # Compare characteristics

dental_compare <- dental.eda

# Convert Treatment_time to categorical variable; "short" <= 730 days, "long" > 730 days

t = 730

dental_compare$Treatment_time[dental_compare$Treatment_time > t] <- "long"

dental_compare$Treatment_time[dental_compare$Treatment_time <= t] <- "short"

dental_compare$Treatment_time <- as.factor(dental_compare$Treatment_time)

# Remove empty entries and variables that are not used

dental_compare.rm <- subset(dental_compare, select=-c(Number))

plot(dental_compare.rm$Treatment_time, dental_compare.rm$Age_start_treatment, xlab = "Treatment Time", ylab = "Patient age at start of treatment", main = "Treatment time vs. Patient age at start of treatment")

plot(dental_compare.rm$Treatment_time, dental_compare.rm$Pre_surgery_time, xlab = "Treatment Time", ylab = "Pre-surgery time", main = "Treatment time vs. Pre-surgery time")

count1 <- table(dental_compare.rm$Treatment_time, dental_compare.rm$Hospital)
spineplot(count1, main = "Treatment Time against Hospital", xlab = "Treatment Time", ylab = "Hospital", col=c("blue","green"))

count2 <- table(dental_compare.rm$Treatment_time, dental_compare.rm$Exo)

spineplot(count2, main = "Treatment Time against Extraction", xlab = "Treatment Time", ylab = "Extraction", col=c("blue","green"))



## 3.2.6 > Predict treatment time in relation to the characteristic of individual's dental profile


dental_compare.p <- dental.eda

glimpse(dental_compare.p)

# Remove empty entries and variables that are not used

dental_compare.p.rm <- subset(dental_compare.p, select=-c(Number,Age_start_treatment,Other,Pre_surgery_time,Post_surgery_time))


# Convert Treatment_time to categorical variable; "short" <= 730 days, "long" > 730 days

t = 730

dental_compare.p.rm$Treatment_time[dental_compare.p.rm$Treatment_time > t] <- "long"

dental_compare.p.rm$Treatment_time[dental_compare.p.rm$Treatment_time <= t] <- "short"

dental_compare.p.rm[sapply(dental_compare.p.rm, is.character)] <- lapply(dental_compare.p.rm[sapply(dental_compare.p.rm, is.character)], 
                                       as.factor)


## Decision Tree

library(rpart)

set.seed(123)

n <- nrow(dental_compare.p.rm)

n_train <- round(.8 * n) # Split 0.70



train_indicise <- sample(1:n, n_train)

# Training / Test Split ---------------------------------------------------

# Train



dental_compare.p.rm_train <- dental_compare.p.rm[train_indicise,]

# Test
dental_compare.p.rm_test = dental_compare.p.rm[-train_indicise,]


dental_compare_model <- rpart(formula = Treatment_time ~.,
                    data = dental_compare.p.rm_train,
                    method = "class",
                    control = rpart.control(cp =0.02),
                    parms = list(split = "information"))
# Prediction

dental_compare_model_pred <- predict(object = dental_compare_model,
                     newdata = dental_compare.p.rm_test,
                     type = "class")

# Plotting the tree

rpart.plot::prp(dental_compare_model, extra = 1, faclen=0,nn = T,
                main = "Decision Tree of Predicting Dental Duration (Short and Long)",
                box.palette = "auto")

                


## Confusion Matrix

library(caret)


confusionMatrix(data = dental_compare_model_pred,
                reference = dental_compare.p.rm_test$Treatment_time)


save.image("Grp6_Projet_Code.RData")

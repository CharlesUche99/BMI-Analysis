setwd("D:/Desktop/Applied Econs/Courses/Semester 1/EC951 Economic Data Analysis/Assessment")

library(haven)

#read dataset into a pandas column called frame
frame <- read_dta("hse2011.dta")

#Obtain descriptive statistics on the dependent variable BMI
summary(frame$bmival)


colSums(is.na(frame))


#Create Dummy Variable for Male
frame$Male <- with(frame, Sex==1)
frame$Female <- with(frame, Sex==2)

#Create Dummy variable for marital status
frame$Single <- with(frame, marstatc==1)
frame$Married <- with(frame, marstatc==2)
frame$White <- with(frame, Origin==1 )
frame$African <- with(frame, Origin==14 )
frame$Chinese <- with(frame, Origin==12 )
frame$Drinks <- with(frame, dnnow==1 )


#Rename columns
names(frame)[names(frame) == "totalwu"] <- "AC"
names(frame)[names(frame) == "porfv"] <- "FC"
names(frame)[names(frame) == "eqvinc"] <- "EI"
names(frame)[names(frame) == "topqual3"] <- "EQ"


#Fit our regression model
health.lm <- lm(bmival ~ Male + Age + totinc + HHSize + White + African + Chinese +omdiaval + AC + FC +EQ, data=frame)
summary(health.lm)
AIC(health.lm)


# correlation for subset of numeric variables
frame_subset <- frame[,c("totinc", "HHSize", "omdiaval", "AC")]
res <- cor(frame_subset, use = "complete.obs")
round(res, 2)


#We now want to test for heteroscedasticity:
#install.packages("lmtest")
library(lmtest)
#Estimating the Bruesch-Pagan Heteroskedasticity test
#DOn't forget that our null hypothesis here is that H0 = Homoskedasticity. If p<0.05, we will reject H0 and conclude that there
#is hoeteroskedasticity in the model.
bptest(health.lm)


#Logged all variables so we can try to remove heteroskedasticity detected in the model.
health_logged.lm <- lm(log1p(bmival) ~ Male + Age + log1p(totinc) + HHSize + White + African + Chinese +log1p(omdiaval) + log1p(AC) + log1p(FC) +EQ, data=frame)
summary(health_logged.lm)
bptest(health_logged.lm)
AIC(health_logged.lm)
bptest(health_logged.lm)

#If you have detected heteroscedasticity, you can use robust standard errors:
#install.packages("sandwich")
library(sandwich)
coeftest(health_logged.lm, vcov = vcovHC(health_logged.lm, type="HC1"))


#Perform the Ramsey Reset Test
resettest(health_logged.lm)

summary(frame$Male)
           
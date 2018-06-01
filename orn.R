

setwd("G:/analytics/ISB CBA/Residency/Residency5&6/B9MT3SA3/Session-1")
orn <- read.csv("Ornst.csv")
#View(orn)
str(orn)
#FirmNo, Assets, and Interlcks are Int type
#Sector and Nation are factor with 10 and 4 levels respectively.

#lets understand the data and try to listen the data loud and clear
#Now we will do Univariate analysis
#lets check missing values if any
colSums(is.na(orn))
#yoho we are lucky enough to have no missing data but dissappointed as
# there will be no learning, anyways lets proceede.
summary(orn)
#takeon from the summary
#firmNo is of no use, as it is just a serial number
#Sector and Nation are categorical in nature
#Assets do have outliers
#Interlcks also have outliers
#will see the impact of outliers later on.

#check skewness and Kurtosis
library(moments)
skewness(orn$Assets)
kurtosis(orn$Assets)
#leptokurtic distribution means more peaked distribution then the normal
#distribution
library(ggplot2)
ggplot(data = orn, aes(Assets))+geom_histogram()
#Assets is highly right skewed
ggplot(data = orn, aes(Sector,Assets))+geom_boxplot()
#except bank all sector have outliers in them
ggplot(data = orn, aes(Nation, Assets))+geom_boxplot()
#except UK, all nation have outliers in them.

#lets investigate response/dependent variable
skewness(orn$Interlcks)
#skewness is 2.535699, means +ve skew that is right skewed
kurtosis(orn$Interlcks)
#same leptokurtic distribution

ggplot(data = orn, aes(orn$Interlcks))+geom_histogram()
#shows right skewed data, more observation cluttered around 0,so there are
#organization which does not have interlocks directorates and they are more
# in our data.

ggplot(data = orn, aes(Sector,Interlcks))+geom_boxplot()
#except bank all sector have outliers in them
ggplot(data = orn, aes(Nation, Interlcks))+geom_boxplot()
#except UK and others, all nation have outliers in them ie more number of
# interlocks than the average interlocks across the Nation.

#lets check the density of interlocks
stripchart(orn$Interlcks,method = "stack",offset = 0.5)


# library(ggplot2)
# ggplot(data = orn,aes(orn$Interlcks))+geom_histogram()+geom_density()
# ggplot(data = orn,aes(orn$Assets,orn$Interlcks))+geom_dotplot()
# #there are more number of interlocks which have assets less than 50000
# ggplot(data = orn,aes(orn$Interlcks,orn$Assets))+geom_dotplot()
ggplot(data = orn,aes(orn$Interlcks,orn$Assets))+geom_point()
# ggplot(data = orn,aes(orn$Interlcks,orn$Assets))
# qplot(orn$Sector, orn$Interlcks)
#As the data speak that the variable of interest is skewed towards right, so data suggest
# I am not normal so you cant treat me normally.


model1 <- glm(Interlcks~.,data = orn[,-1],family = "poisson")
summary(model1)
anova(model1)
# Interpretations and Conclusions
# #The predictors Nation and Sector are categorical. Nation is
# handled with "Canada" as baseline category and Sector is
# handled with "AGR" as baseline category. R handles
# lexicographic category variable by taking lexicographically
# lowest category as baseline.
# # The dot plot of interlocks indicates a non-Normal skewed
# distribution and Poisson model seems appropriate.
# # All the predictors are significant and can be interpreted in the
# context. For instance, for the same level of assets in the same
# sector, the estimated expected number in interlocKs is
# e(-????0:8259 ) 0.4378 times lower in a US controlled firm than a
# Canadian controlled firm.
# # Similarly, holding nation and sector at given levels and
# increasing assets by 1 billion dollars (the unit of assets)
# multiplies the estimated expected number of interlocks by
# e(0.02085 )1.021, an increase of just over 2%.
# #The residual deviance is too high with negligible p-value. It
#indicates that the model gives a poor fit to the data.

#all the attributes has low se and all are significant, that is betas are
#not zeros and can't be dropped as per current model.
# Residual deviance is better than the null deviance and the gap is good, thus
# the model is good
# A model fit statistic R2 = 1- (Residual Deviance of full model)/(Residual
# deviance of the null model) = 1 ???? (1887.4)/3737.0  0.495, shows that
# the model accounts for nearly half the deviance in number of interlocks

with(model1,cbind(res.deviance=deviance,df = df.residual,
                  p = pchisq(deviance,df.residual,lower.tail = FALSE)))
log(model1$coefficients)
qqnorm(rstandard(model1))
qqline(rstandard(model1))
plot(model1)

# Interpretation of Diagnostic Plots
# * From the first plot we see that the residuals are showing
# increasing variation (wider spread) as fitted values increase.
# * Normal Q-Q Plot shows lack of normality of residuals, a sign
# of poor fit
# * Scale-Location Plot shows unequal dispersion and Poisson
# distribution's variance = mean property is possibly violated
# * Residuals vs Leverage Plot shows quite a few outliers
# * On the whole, the diagnostic plots also show that the Poisson
# Regression model is not a particularly good fit. Large
# deviation or dispersion seems to be the common issue
# indicated by the plots
# library(MASS)
# stresid <- studres(model1) 
# hist(stresid, freq=FALSE, main="Distribution of Studentized Residuals") 
# xfit<-seq(min(stresid),max(stresid),length=40) 
# yfit<-dnorm(xfit) 
# lines(xfit, yfit)
# library(car) 
# residualPlot(model1, id.n=5)
# 
# residualPlots(model1, id.n=5)
# vif(model1)
# step1 <-stepAIC(model1,direction = "both")
# step1$anova

#now we will test for dispersion
library(AER)
dispersiontest(model1,trafo = 1)
#The data suffers from overdispersion and needs an alternative model.

#building 2nd model i.e negative binomial regression

library(foreign)
library(ggplot2)
library(MASS)

#removing index
orn2 <- orn[,-1]
model2 <- glm.nb(Interlcks~., data = orn2)
summary(model2)
anova(model2)

with(model2,cbind(res.deviance=deviance,df = df.residual,
                  p = pchisq(deviance,df.residual,lower.tail = FALSE)))
log(model2$coefficients)
qqnorm(rstandard(model2))
qqline(rstandard(model2))
plot(model1)

#Interpretations and Conlusions
# # The residual deviance has significantly reduced as compared
# to Poisson regression though the p-vaue for goodness of fit
# test is still small (0.005)
# # The diagnostic plots are more satisfactory that those of
# Poisson regression. The Q-Q plot shows the anticipated
# baehaviour.
# # The overall analysis of Ornstein's data is significantly better
# using negative binomial regression.
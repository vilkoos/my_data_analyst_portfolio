
####################################################################
#
#  data requisition and transformation
#
#####################################################################

# download the file loansData.rda from: 
# https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda
# put this file in your working directory.
# this was done on 12 feb 2013

load("loansData.rda")

dat <- na.omit (loansData) # omit the two cases that have missing values

# make - if posible colums - numeric
dat[, 3] <- as.numeric(sub('%','',dat$Interest.Rate))
dat[, 4] <- as.numeric(sub(' months','',dat$Loan.Length))
dat[, 6] <- as.numeric(sub('%','',dat$Debt.To.Income.Ratio))
dat[,10] <- as.numeric(sub('-.*$','',dat$FICO.Range))
dat[,14] <- sub(' years','',dat$Employment.Length)
dat[,14] <- sub(' year' ,'',dat$Employment.Length)
dat[,14] <- sub('< 1','0',dat$Employment.Length)
dat[,14] <- sub('\\+' ,'',dat$Employment.Length)
dat[,14] <- as.numeric(dat[,14])

#------- explore the data -----------------
# some examples, there were many more  
iRate <- dat[, 3]
boxplot(iRate)

employed <- dat[,14]
table(employed)
sum(is.na(employed))

#----remove NA/suspect rows ---------------------
dat <- na.omit(dat)                 # remove 77 cases with NA in Employment.Length
dat <- subset(dat,dat[,9] <= 20000) # remove 16 cases with very high monthly incomes

#-----create dataframe rat------------
# dataframe rat contains the nine explantory vars and the interest rate 
# (all these vars are rational)
rat      <- dat[,9:14]
rat[,7 ] <- dat[,1   ]
rat[,8 ] <- dat[,4   ]
rat[,9 ] <- dat[,6   ]
rat[,10] <- dat[,3   ]
names(rat)[ 7] <- "Amount.Requested"
names(rat)[ 8] <- "Loan.Length"
names(rat)[ 9] <- "Debt.To.Income.Ratio"
names(rat)[10] <- "Interest.Rate"

#------ explore the relationships between vars in rat ---------  
library("car")
scatterplotMatrix(rat[1:10])

#---scale the variables in rat -----------
ratSc <- as.data.frame(scale(rat))

####################################################################
#
#  fit the lm's on the scaled data
#     select the best model to use
#
#####################################################################

# fit model using all nine vars 
lmTot <- lm(ratSc[,10] ~ ratSc[,1] + ratSc[,2] + ratSc[,3] +ratSc[,4] + ratSc[,5] + ratSc[,6] +ratSc[,7] + ratSc[,8] + ratSc[,9] )
summary(lmTot)

#----- result ----------------------------------------------
"Residuals:
     Min       1Q   Median       3Q      Max 
-2.28505 -0.33078 -0.04626  0.29113  2.38393 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.093e-16  1.008e-02   0.000 1.000000    
ratSc[, 1]  -3.125e-02  1.296e-02  -2.411 0.015971 *  
ratSc[, 2]  -7.292e-01  1.043e-02 -69.944  < 2e-16 ***
ratSc[, 3]  -3.884e-02  1.164e-02  -3.337 0.000859 ***
ratSc[, 4]  -1.362e-02  1.175e-02  -1.160 0.246320    
ratSc[, 5]   1.043e-01  1.022e-02  10.205  < 2e-16 ***
ratSc[, 6]   2.974e-03  1.026e-02   0.290 0.772007    
ratSc[, 7]   2.881e-01  1.274e-02  22.602  < 2e-16 ***
ratSc[, 8]   3.199e-01  1.114e-02  28.712  < 2e-16 ***
ratSc[, 9]  -7.368e-03  1.190e-02  -0.619 0.536003    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.4941 on 2395 degrees of freedom
Multiple R-squared: 0.7567,	Adjusted R-squared: 0.7558 
F-statistic: 827.8 on 9 and 2395 DF,  p-value: < 2.2e-16 
"

# fit model using the selected vars
lmDef <- lm(ratSc$Interest.Rate ~ ratSc$FICO.Range + ratSc$Loan.Length + ratSc$Amount.Requested)
summary(lmDef)
anova(lmDef)

#------- result -------------------------------------------
"Residuals:
     Min       1Q   Median       3Q      Max 
-2.35051 -0.34811 -0.03082  0.30334  2.47607 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)             8.101e-17  1.032e-02    0.00        1    
ratSc$FICO.Range       -7.343e-01  1.036e-02  -70.85   <2e-16 ***
ratSc$Loan.Length       3.305e-01  1.133e-02   29.18   <2e-16 ***
ratSc$Amount.Requested  2.553e-01  1.136e-02   22.46   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.5063 on 2401 degrees of freedom
Multiple R-squared: 0.7439,	Adjusted R-squared: 0.7436 
F-statistic:  2325 on 3 and 2401 DF,  p-value: < 2.2e-16 

> anova(lmDef)
Analysis of Variance Table

Response: ratSc$Interest.Rate
                         Df  Sum Sq Mean Sq F value    Pr(>F)    
ratSc$FICO.Range          1 1204.34 1204.34 4697.40 < 2.2e-16 ***
ratSc$Loan.Length         1  454.70  454.70 1773.50 < 2.2e-16 ***
ratSc$Amount.Requested    1  129.39  129.39  504.66 < 2.2e-16 ***
Residuals              2401  615.58    0.26                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
"

#----- some unreported interesting variants of lmDef -------------

lm1 <- lm(ratSc$Interest.Rate ~ ratSc$FICO.Range) # !!! most variance is explained here!!!
summary(lm1)

lm2 <- lm(ratSc$Interest.Rate ~ ratSc$Loan.Length)
summary(lm2)

lm2a <- lm(ratSc$Interest.Rate ~ ratSc$Loan.Length + ratSc$Loan.Length*ratSc$FICO.Range)
summary(lm2a)

lm3 <- lm(ratSc$Interest.Rate ~ ratSc$Amount.Requested)
summary(lm3)

lm3a <- lm(ratSc$Interest.Rate ~ ratSc$Amount.Requested + ratSc$Amount.Requested*ratSc$FICO.Range)
summary(lm3a)

####################################################################
#
#  fit the selected lm on the un-scaled data
#     express the selected model in standard units (so un-scale)
#
#####################################################################

lmNice <- lm(rat$Interest.Rate ~ rat$FICO.Range + rat$Loan.Length + rat$Amount.Requested)
summary(lmNice)
anova(lmNice)
confint(lmNice)

#--------- result ---------------
"Residuals:
    Min      1Q  Median      3Q     Max 
-9.7678 -1.4466 -0.1281  1.2606 10.2896 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)           6.746e+01  8.883e-01   75.94   <2e-16 ***
rat$FICO.Range       -8.754e-02  1.235e-03  -70.85   <2e-16 ***
rat$Loan.Length       1.378e-01  4.724e-03   29.18   <2e-16 ***
rat$Amount.Requested  1.375e-04  6.122e-06   22.46   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 2.104 on 2401 degrees of freedom
Multiple R-squared: 0.7439,	Adjusted R-squared: 0.7436 
F-statistic:  2325 on 3 and 2401 DF,  p-value: < 2.2e-16 

> anova(lmNice)
Analysis of Variance Table

Response: rat$Interest.Rate
                       Df  Sum Sq Mean Sq F value    Pr(>F)    
rat$FICO.Range          1 20797.7 20797.7 4697.40 < 2.2e-16 ***
rat$Loan.Length         1  7852.1  7852.1 1773.50 < 2.2e-16 ***
rat$Amount.Requested    1  2234.4  2234.4  504.66 < 2.2e-16 ***
Residuals            2401 10630.4     4.4                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

> confint(lmNice)
                             2.5 %        97.5 %
(Intercept)          65.7138482967 69.1975773621
rat$FICO.Range       -0.0899601909 -0.0851147213
rat$Loan.Length       0.1285762538  0.1471052453
rat$Amount.Requested  0.0001255187  0.0001495277
"

####################################################################
#
#  script for figure 1
#
#####################################################################

par(mfrow=c(1,2))
#-----------------------
plot(dat$FICO.Range,dat$Interest.Rate, 
     pch=19, cex=0.5, col="blue", 
     xlab="FICO rate (in fico points)", ylab= "interest rate (100 * interest %)", 
     main = "(1a) interest by FICO rate")
abline(lsfit(dat[,10],dat[,3]),lwd=4, col="red")
#-----------------------
hist(dat$Interest.Rate, breaks=30, col="blue",
     xlab="interest rate (100 * interest %)", 
     main="(1b) distribution of interest rates")





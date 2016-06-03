# Exploring language effects in cross-cultural survey research: 
# Does the language of administration affect answers about politics? 
# Diana Zavala-Rojas

# 'Satisfaction and need of change in politics
# and the economy' (Model 2)

### To access the results published in the article use Ctrl+F and  find "Table 5" and "Table 6",
### To see the results published in the Appendix 3. Global fit indexes, use 
### Ctrl+F and  find "Appendix 3"

# setwd("~/Thesis/PAPER 3/paper3/NewModel")
# setwd("~/")
source("DataCleaningAndDescriptives.R")
# d<-read.csv("r123export2.csv", header=T,sep = ",")

library(lavaan)
library(semTools)


## Baseline model - No restrictions ####

ms0.1<- '
satD =~ 1*eco.d1 + gov.d1 + dem.d1 
c.satD =~ c.eco.d1 + 1*c.gov.d1 + c.dem.d1 

satO =~ 1*eco.d2 + gov.d2 + dem.d2 
c.satO =~ c.eco.d2 + 1*c.gov.d2 + c.dem.d2 

eco.d1~0*1
gov.d1~1
dem.d1~1

c.eco.d1~1
c.gov.d1~0*1 
c.dem.d1~1

satD~1
c.satD~1


eco.d2~0*1
gov.d2~1
dem.d2~1

c.eco.d2~1
c.gov.d2~0*1 
c.dem.d2~1

satO~1
c.satO~1

# Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

'    
ms.0 <-   lavaan(ms0.1, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.lv.x=T,
               meanstructure=T,
               estimator = "ML") 

FSatis<- fitMeasures(ms.0, c("df","chisq","pvalue", "rmsea",
                              "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, baseline model

FSatis


Jrule<-miPowerFit(ms.0, stdLoad=0.1, intcept=0.05)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

# summary(ms.0, standardized=T)

### Baseline + correlated error terms ###
ms0<- '
satD =~ 1*eco.d1 + gov.d1 + dem.d1 
c.satD =~ c.eco.d1 + 1*c.gov.d1 + c.dem.d1 

eco.d1~0*1
gov.d1~1
dem.d1~1

c.eco.d1~1
c.gov.d1~0*1 
c.dem.d1~1

satD~1
c.satD~1

satO =~ 1*eco.d2 + gov.d2 + dem.d2 
c.satO =~ c.eco.d2 + 1*c.gov.d2 + c.dem.d2 

eco.d2~0*1
gov.d2~1
dem.d2~1

c.eco.d2~1
c.gov.d2~0*1 
c.dem.d2~1

satO~1
c.satO~1

# Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2
'


ms<-   lavaan(ms0, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.lv.x=T,
               meanstructure=T,
               estimator = "ML") 

FSatis<- fitMeasures(ms, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, baseline model + correlated errors

FSatis


Jrule<-miPowerFit(ms, stdLoad=0.1, intcept=0.05)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

# summary(ms, standardized=T)
anova(ms.0,ms)

### Restricting factor Loadings ###
m1<- '
satD =~ 1*eco.d1 + c(e)* gov.d1 + c(f)*dem.d1 
c.satD =~ c(g)*c.eco.d1 + 1*c.gov.d1 + c(h)*c.dem.d1 

satO =~ 1*eco.d2 + c(e)*gov.d2 + c(f)*dem.d2 
c.satO =~ c(g)*c.eco.d2 + 1*c.gov.d2 + c(h)*c.dem.d2 

eco.d1~0*1
gov.d1~1
dem.d1~1

c.eco.d1~1
c.gov.d1~0*1 
c.dem.d1~1

satD~1
c.satD~1


eco.d2~0*1
gov.d2~1
dem.d2~1

c.eco.d2~1
c.gov.d2~0*1 
c.dem.d2~1

satO~1
c.satO~1

#Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2
'    


ms1 <-   lavaan(m1, 
                data=d1, 
                auto.fix.first=F,
                auto.var=T,
                meanstructure=T,
                estimator = "ML") 

FSatis<- fitMeasures(ms1, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, factor loadings invariance

FSatis


# summary(ms1, standardized=T)
anova(ms,ms1)

Jrule<-miPowerFit(ms1, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

# Invariance of intercepts

m2<- '
satD =~ 1*eco.d1 + c(e)* gov.d1 + c(f)*dem.d1 
c.satD =~ c(g)*c.eco.d1 + 1*c.gov.d1 + c(h)*c.dem.d1 

satO =~ 1*eco.d2 + c(e)*gov.d2 + c(f)*dem.d2 
c.satO =~ c(g)*c.eco.d2 + 1*c.gov.d2 + c(h)*c.dem.d2 

eco.d1~0*1
gov.d1~c(i)*1
dem.d1~c(j)*1

c.eco.d1~c(k)*1
c.gov.d1~0*1 
c.dem.d1~c(l)*1

eco.d2~0*1
gov.d2~c(i)*1
dem.d2~c(j)*1

c.eco.d2~c(k)*1 
c.gov.d2~0*1 
c.dem.d2~c(l)*1

satD~1
c.satD~1

satO~1
c.satO~1

#Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2
'    
ms2 <-   lavaan(m2, 
                data=d1, 
                auto.fix.first=F,
                auto.var=T,
                estimator = "ML") 

FSatis<- fitMeasures(ms2, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, invariance of intercepts

FSatis


summary(ms2, standardized=T)
anova(ms1,ms2)

Jrule<-miPowerFit(ms2, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

### Latent means test ###
m3<- '
satD =~ 1*eco.d1 + c(e)* gov.d1 + c(f)*dem.d1 
c.satD =~ c(g)*c.eco.d1 + 1*c.gov.d1 + c(h)*c.dem.d1 

satO =~ 1*eco.d2 + c(e)*gov.d2 + c(f)*dem.d2 
c.satO =~ c(g)*c.eco.d2 + 1*c.gov.d2 + c(h)*c.dem.d2 

eco.d1~0*1
gov.d1~c(i)*1
dem.d1~c(j)*1

c.eco.d1~c(k)*1
c.gov.d1~0*1 
c.dem.d1~c(l)*1

eco.d2~0*1
gov.d2~c(i)*1
dem.d2~c(j)*1

c.eco.d2~c(k)*1 
c.gov.d2~0*1 
c.dem.d2~c(l)*1


satD~c(m)*1
c.satD~c(n)*1

satO~c(m)*1
c.satO~c(n)*1

#Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2
'    
ms3 <-   lavaan(m3, 
                data=d1, 
                auto.fix.first=F,
                auto.var=T,
                estimator = "ML") 

FSatis<- fitMeasures(ms3, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, latent means differences test

FSatis


# summary(ms3, standardized=T)
anova(ms2,ms3)

Jrule<-miPowerFit(ms3, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

#### Latent mean test freeing the 'sat' latent mean ######

m4<- '
satD =~ 1*eco.d1 + c(e)* gov.d1 + c(f)*dem.d1 
c.satD =~ c(g)*c.eco.d1 + 1*c.gov.d1 + c(h)*c.dem.d1 

satO =~ 1*eco.d2 + c(e)*gov.d2 + c(f)*dem.d2 
c.satO =~ c(g)*c.eco.d2 + 1*c.gov.d2 + c(h)*c.dem.d2 

eco.d1~0*1
gov.d1~c(i)*1
dem.d1~c(j)*1

eco.d2~0*1
gov.d2~c(i)*1
dem.d2~c(j)*1

c.eco.d1~c(k)*1
c.gov.d1~0*1 
c.dem.d1~c(l)*1

c.eco.d2~c(k)*1 
c.gov.d2~0*1 
c.dem.d2~c(l)*1

satD~1
c.satD~c(m)*1

satO~1
c.satO~c(m)*1

#Testing factor covariances
c.satO~~satO
c.satD~~satD

# Testing latent correlations = 1
c.satO ~~  c.satD
satO ~~ satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2
'    
ms4 <-   lavaan(m4, 
                data=d1, 
                auto.fix.first=F,
                auto.var=T,
                estimator = "ML") 

FSatis<- fitMeasures(ms4, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, Latent mean test freeing the 'sat' latent mean

FSatis

# summary(ms4, standardized=T)
anova(ms2,ms4)

Jrule<-miPowerFit(ms4, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)


#### Latent correlations across the same concept in different languages = 1 ####

m6<- '
satD =~ 1*eco.d1 + c(e)* gov.d1 + c(f)*dem.d1 
c.satD =~ c(g)*c.eco.d1 + 1*c.gov.d1 + c(h)*c.dem.d1 

satO =~ 1*eco.d2 + c(e)*gov.d2 + c(f)*dem.d2 
c.satO =~ c(g)*c.eco.d2 + 1*c.gov.d2 + c(h)*c.dem.d2 

eco.d1~0*1
gov.d1~c(i)*1
dem.d1~c(j)*1

c.eco.d1~c(k)*1
c.gov.d1~0*1 
c.dem.d1~c(l)*1

eco.d2~0*1
gov.d2~c(i)*1
dem.d2~c(j)*1

c.eco.d2~c(k)*1 
c.gov.d2~0*1 
c.dem.d2~c(l)*1

satD~1
c.satD~1

satO~1
c.satO~1

#Testing factor covariances
c.satO~~c(m)*satO
c.satD~~c(m)*satD

# Testing latent correlations = 1
c.satO ~~  1*c.satD
satO ~~ 1*satD

# The remaining covariances
c.satO~~satD
c.satD~~satO

# Covariances (panel)
dem.d1 ~~ dem.d2 
c.eco.d1 ~~ c.eco.d2
eco.d1 ~~ eco.d2
c.dem.d1 ~~ c.dem.d2
c.gov.d1~~c.gov.d2
gov.d1~~gov.d2

# Extra covariances
dem.d1~~c(a)*c.dem.d1
dem.d2~~c(a)*c.dem.d2
c.eco.d1 ~~ c(b)*eco.d1
eco.d2 ~~ c(b)*c.eco.d2
gov.d1~~c(d)*c.gov.d1
gov.d2~~c(d)*c.gov.d2

#Variances observed variables
eco.d1~~eco.d1
gov.d1~~gov.d1
dem.d1~~dem.d1

c.eco.d1~~c.eco.d1
c.gov.d1~~c.gov.d1 
c.dem.d1~~c.dem.d1

eco.d2~~eco.d2
gov.d2~~gov.d2
dem.d2~~dem.d2

c.eco.d2~~c.eco.d2
c.gov.d2~~c.gov.d2 
c.dem.d2~~c.dem.d2

#Variances latent variables

satD~~1*satD
c.satD~~1*c.satD

satO~~1*satO
c.satO~~1*c.satO

'    

ms6 <-   lavaan(m6, 
                data=d1, 
                auto.fix.first=F,
                auto.var=T,
                estimator = "ML") 

# inspect(ms6,"cov.lv")

FSatis<- fitMeasures(ms6, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, latent correlations test

FSatis

# summary(ms6, standardized=T)
anova(ms2,ms6)


Jrule<-miPowerFit(ms6, stdLoad=0.1, intcept=0.055,cor=0.05)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

summary(ms2, standardized=T)

b0<-anova(ms,ms.0) # Table 5. baseline , correlated errors 
b1<-anova(ms,ms1) # Table 5.loadings
b2<-anova(ms1,ms2) # Table 5.intercepts
b6<-anova(ms2,ms6) # Table 6.correlations equal to 1
b3<-anova(ms2,ms3) # Table 6.mean difference
b4<-anova(ms2,ms4) # Table 6.mean difference "sat" only,  not passed.

###Table 5 and Table 6 in the paper
b<-rbind(b0,b1[2,],b2[2,],b6[2,],b3[2,],b4[2,])
b

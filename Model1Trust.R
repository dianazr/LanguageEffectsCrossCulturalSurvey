# Exploring language effects in cross-cultural survey research: 
# Does the language of administration affect answers about politics? 
# Diana Zavala-Rojas

# 'Trust and need of change in institutions' (Model 1)

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

mt0.1<-'
c.trustO=~  c.prl.d2+ 1*c.pol.d2 + c.par.d2 
c.trustD=~  c.prl.d1+ 1*c.pol.d1 + c.par.d1 

instD =~ prl.d1 + 1*pol.d1 + par.d1
instO =~ prl.d2 + 1*pol.d2 + par.d2

prl.d2 ~ 1
pol.d2 ~ 0*1 
par.d2 ~ 1


c.prl.d2 ~ 1
c.pol.d2 ~ 0*1 
c.par.d2 ~ 1

instO ~1
c.trustO ~1

prl.d1 ~ 1
pol.d1 ~ 0*1 
par.d1 ~ 1


c.prl.d1 ~ 1
c.pol.d1 ~ 0*1 
c.par.d1 ~ 1

instD ~ 1
c.trustD ~ 1

#Testing factor covariances
c.trustO~~instO
c.trustD~~instD

# Testing latent correlations = 1
c.trustO~~c.trustD
instO~~instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
# pol.d1 ~~ c(z)*c.pol.d1
# pol.d2 ~~ c(z)*c.pol.d2
# c.pol.d2 ~~ pol.d1
# c.pol.d1 ~~ pol.d2
'


mtr0.1 <- lavaan(mt0.1, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr0.1, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, baseline model

FTrust

# summary(mtr0.1, standardized=T)

Jrule<-miPowerFit(mtr0.1, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

### Baseline + correlated error terms ###
mt0<-'
c.trustO=~  c.prl.d2+ 1*c.pol.d2 + c.par.d2 
c.trustD=~  c.prl.d1+ 1*c.pol.d1 + c.par.d1 

instD =~ prl.d1 + 1*pol.d1 + par.d1
instO =~ prl.d2 + 1*pol.d2 + par.d2

prl.d2 ~ 1
pol.d2 ~ 0*1 
par.d2 ~ 1


c.prl.d2 ~ 1
c.pol.d2 ~ 0*1 
c.par.d2 ~ 1

instO ~1
c.trustO ~1

prl.d1 ~ 1
pol.d1 ~ 0*1 
par.d1 ~ 1


c.prl.d1 ~ 1
c.pol.d1 ~ 0*1 
c.par.d1 ~ 1

instD ~ 1
c.trustD ~ 1

# Testing factor covariances
c.trustO~~instO
c.trustD~~instD

# Testing latent correlations = 1
c.trustO~~c.trustD
instO~~instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
pol.d1 ~~ c(z)*c.pol.d1
pol.d2 ~~ c(z)*c.pol.d2
par.d1 ~~ c(w)*c.par.d1
par.d2 ~~ c(w)*c.par.d2

'


mtr0 <- lavaan(mt0, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr0, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, baseline model + correlated errors

FTrust

# summary(mtr0, standardized=T)

Jrule<-miPowerFit(mtr0, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

anova(mtr0.1,mtr0)

#### Restricting factor loadings

mt1<-'
instO =~ c(a)*prl.d2 + 1*pol.d2 + c(d)*par.d2
c.trustO=~ c(b)* c.prl.d2+ 1*c.pol.d2 + c(e)*c.par.d2 

instD =~ c(a)*prl.d1 + 1*pol.d1 + c(d)*par.d1
c.trustD=~  c(b)*c.prl.d1+ 1*c.pol.d1 + c(e)*c.par.d1 

prl.d2 ~ 1
pol.d2 ~ 0*1 
par.d2 ~ 1

instO ~1
c.trustO ~1

c.prl.d2 ~ 1
c.pol.d2 ~ 0*1 
c.par.d2 ~ 1


prl.d1 ~ 1
pol.d1 ~ 0*1 
par.d1 ~ 1

instD ~ 1
c.trustD ~ 1

c.prl.d1 ~ 1
c.pol.d1 ~ 0*1 
c.par.d1 ~ 1

#Testing factor covariances 
c.trustO~~instO
c.trustD~~instD

# Testing latent correlations = 1
c.trustO~~c.trustD
instO~~instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
pol.d1 ~~ c(z)*c.pol.d1
pol.d2 ~~ c(z)*c.pol.d2
par.d1 ~~ c(w)*c.par.d1
par.d2 ~~ c(w)*c.par.d2

'


mtr1 <- lavaan(mt1, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr1, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, factor loadings invariance

FTrust

# summary(mtr1, standardized=T)
anova(mtr0,mtr1)

Jrule<-miPowerFit(mtr1, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)


#### Invariance of intercepts

mt2<-#model for trust in institutions traits
        '
instO =~ c(a)*prl.d2 + 1*pol.d2 + c(d)*par.d2
c.trustO=~ c(b)* c.prl.d2+ 1*c.pol.d2 + c(e)*c.par.d2 

instD =~ c(a)*prl.d1 + 1*pol.d1 + c(d)*par.d1
c.trustD=~  c(b)*c.prl.d1+ 1*c.pol.d1 + c(e)*c.par.d1 

prl.d2 ~ c(f)*1
pol.d2 ~ 0*1 
par.d2 ~ c(g)*1

instO ~1
c.trustO ~1

instD ~ 1
c.trustD ~ 1

c.prl.d2 ~ c(h)*1
c.pol.d2 ~ 0*1 
c.par.d2 ~ c(i)*1

prl.d1 ~ c(f)*1
pol.d1 ~ 0*1 
par.d1 ~ c(g)*1

c.prl.d1 ~ c(h)*1
c.pol.d1 ~ 0*1 
c.par.d1 ~ c(i)*1

#Testing factor covariances
c.trustO~~instO
c.trustD~~instD

# Testing latent correlations = 1
c.trustO~~c.trustD
instO~~instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
pol.d1 ~~ c(z)*c.pol.d1
pol.d2 ~~ c(z)*c.pol.d2
par.d1 ~~ c(w)*c.par.d1
par.d2 ~~ c(w)*c.par.d2

'


mtr2 <- lavaan(mt2, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr2, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, invariance of intercepts

FTrust

# summary(mtr2, standardized=T)

Jrule<-miPowerFit(mtr2, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

anova(mtr1,mtr2)


### Latent means difference

mt3<-#model for trust in institutions traits
        '
instO =~ c(a)*prl.d2 + 1*pol.d2 + c(d)*par.d2
c.trustO=~ c(b)* c.prl.d2+ 1*c.pol.d2 + c(e)*c.par.d2 

instD =~ c(a)*prl.d1 + 1*pol.d1 + c(d)*par.d1
c.trustD=~  c(b)*c.prl.d1+ 1*c.pol.d1 + c(e)*c.par.d1 

prl.d2 ~ c(f)*1
pol.d2 ~ 0*1 
par.d2 ~ c(g)*1

c.prl.d2 ~ c(h)*1
c.pol.d2 ~ 0*1 
c.par.d2 ~ c(i)*1

prl.d1 ~ c(f)*1
pol.d1 ~ 0*1 
par.d1 ~ c(g)*1

c.prl.d1 ~ c(h)*1
c.pol.d1 ~ 0*1 
c.par.d1 ~ c(i)*1

instD ~ c(j)*1
c.trustD ~ c(k)*1
instO ~ c(j)*1
c.trustO ~c(k)*1

#Testing factor covariances
c.trustO~~instO
c.trustD~~instD

# Testing latent correlations = 1
c.trustO~~c.trustD
instO~~instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
pol.d1 ~~ c(z)*c.pol.d1
pol.d2 ~~ c(z)*c.pol.d2
par.d1 ~~ c(w)*c.par.d1
par.d2 ~~ c(w)*c.par.d2

'
mtr3 <- lavaan(mt3, 
               data=d1, 
               auto.fix.first=F,
               auto.var=T,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr3, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, latent means differences test

FTrust

# summary(mtr3, standardized=T)

Jrule<-miPowerFit(mtr3, stdLoad=0.1, intcept=0.055)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

anova(mtr2,mtr3)


### MEan difference passed####


#### Latent correlations across the same concept in different languages = 1 ####
#### by restricting model mt2
####

mt5<-#model for trust in institutions traits
        '
instO =~ c(a)*prl.d2 + 1*pol.d2 + c(d)*par.d2
c.trustO=~ c(b)* c.prl.d2+ 1*c.pol.d2 + c(e)*c.par.d2 

instD =~ c(a)*prl.d1 + 1*pol.d1 + c(d)*par.d1
c.trustD=~  c(b)*c.prl.d1+ 1*c.pol.d1 + c(e)*c.par.d1 

prl.d2 ~ c(f)*1
pol.d2 ~ 0*1 
par.d2 ~ c(g)*1

instO ~1
c.trustO ~1

instD ~ 1
c.trustD ~ 1

c.prl.d2 ~ c(h)*1
c.pol.d2 ~ 0*1 
c.par.d2 ~ c(i)*1

prl.d1 ~ c(f)*1
pol.d1 ~ 0*1 
par.d1 ~ c(g)*1

c.prl.d1 ~ c(h)*1
c.pol.d1 ~ 0*1 
c.par.d1 ~ c(i)*1

#Testing factor covariances
c.trustO~~ instO
c.trustD~~ instD

# Testing latent correlations = 1 
c.trustO~~ 1*c.trustD
instO~~ 1*instD

# The remaining covariances
c.trustO~~instD
c.trustD~~instO

# Covariances (panel)
pol.d1 ~~ pol.d2
par.d1 ~~ par.d2
prl.d1 ~~ prl.d2
c.pol.d2 ~~ c.pol.d1 
c.prl.d2 ~~ c.prl.d1
c.par.d2 ~~ c.par.d1

# Extra covariances
pol.d1 ~~ c(z)*c.pol.d1
pol.d2 ~~ c(z)*c.pol.d2
par.d1 ~~ c(w)*c.par.d1
par.d2 ~~ c(w)*c.par.d2

#Variances of latent variables
c.trustO ~~ 1*c.trustO
c.trustD ~~ 1*c.trustD
instD ~~ 1*instD
instO ~~ 1*instO

#Variances of observed variables
prl.d1~~ prl.d1
pol.d1 ~~pol.d1
par.d1 ~~ par.d1

c.prl.d1 ~~c.prl.d1
c.pol.d1 ~~c.pol.d1
c.par.d1 ~~c.par.d1

prl.d2~~ prl.d2
pol.d2 ~~pol.d2
par.d2 ~~ par.d2

c.prl.d2 ~~c.prl.d2
c.pol.d2 ~~c.pol.d2
c.par.d2 ~~c.par.d2

'


mtr5 <- lavaan(mt5, 
               data=d1, 
               auto.fix.first=F,
               auto.var=F,
               auto.cov.y=F,
               estimator = "ML") 

FTrust<- fitMeasures(mtr5, c("df","chisq","pvalue", "rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper", "cfi","srmr"))

## Appendix 3. Global fit indices, latent correlations test

FTrust

# summary(mtr5, standardized=T)

inspect(mtr5,"cov.lv")

Jrule<-miPowerFit(mtr5,stdLoad=0.1, intcept=0.055, cor=0.05)
Jrule$what<-paste(Jrule$lhs,Jrule$op,Jrule$rhs)
table(Jrule$what,Jrule$decision,Jrule$group)

anova(mtr2,mtr5)

#### LRT results ####
b0<-anova(mtr0.1,mtr0) # Table 5. baseline , correlated errors 
b1<-anova(mtr0,mtr1) # Table 5.loadings
b2<-anova(mtr1,mtr2) # Table 5.intercepts
b3<-anova(mtr2,mtr5) # Table 6.correlations equal 1
b4<-anova(mtr2,mtr3) # Table 6.mean difference


###Table 5 and Table 6 in the paper
b<-rbind(b0,b1[2,],b2[2,],b3[2,],b4[2,])
b

# Exploring language effects in cross-cultural survey research: 
# Does the language of administration affect answers about politics? 
# Diana Zavala-Rojas

# setwd("~/Thesis/PAPER 3/paper3/NewModel")
library(dplyr)
library(plyr)

d<-read.csv("r123export2.csv", header=T,sep = ",")


# Cleaning the data

table(d$lang.r2)

d$lang.r2 <- revalue(d$lang.r2, c("Papiamento: spoken on Curaçao"="Papiamento", 
                                  "Papiamento: spoken on Aruba"="Papiamento"))
table(d$lang.r2)

d1<- d[d$lang.r2=="Arabic" | d$lang.r2=="English" | d$lang.r2=="German" |
               d$lang.r2=="Turkish" | d$lang.r2=="Papiamento",]

##Table 2 ###

table(d1$lang.r2)
d1$c.dem.d1<-d1$c.dem..d1
d1$c.dem..d1<-NULL


##Table 3 ###

table(d1$lang.r2)

# Self-reported proficiency in Dutch 
vars <- select(d1, lang.r2, write.d,read.d,speak.d,listen.d) 

vars %>% 
        group_by(lang.r2) %>%
        summarise_each(funs(mean, "mean", mean(., na.rm = TRUE)))

vars %>% 
        group_by(lang.r2) %>%
        summarise_each(funs(sd, "sd", sd(., na.rm = TRUE)))

# Self-reported proficiency in second language

vars <- select(d1, lang.r2, write2,read2,speak2,listen2) 

vars %>% 
        group_by(lang.r2) %>%
        summarise_each(funs(mean, "mean", mean(., na.rm = TRUE)))

vars %>% 
        group_by(lang.r2) %>%
        summarise_each(funs(sd, "sd", sd(., na.rm = TRUE)))

ref<-citation(package = "dplyr", lib.loc = NULL)

## S3 method for class 'citation':
toBibtex(ref)


#Table 4 ##
#Arabic language in second wave
# lang.job,lang.friends,lang.home,lang.mother,lang.father)
table(d1$lang.r2=="Arabic",d1$lang.job)/sum(d1$lang.r2=="Arabic")
table(d1$lang.r2=="Arabic",d1$lang.friends)/sum(d1$lang.r2=="Arabic")
table(d1$lang.r2=="Arabic",d1$lang.home)/sum(d1$lang.r2=="Arabic")

#Table reports Arabic + Berber
table(d1$lang.r2=="Arabic",d1$lang.mother)/sum(d1$lang.r2=="Arabic")

#English language in second wave
# lang.job,lang.friends,lang.home,lang.mother,lang.father)
table(d1$lang.r2=="English",d1$lang.job)/sum(d1$lang.r2=="English")
table(d1$lang.r2=="English",d1$lang.friends)/sum(d1$lang.r2=="English")
table(d1$lang.r2=="English",d1$lang.home)/sum(d1$lang.r2=="English")
table(d1$lang.r2=="English",d1$lang.mother)/sum(d1$lang.r2=="English")

# German language in second wave
# lang.job,lang.friends,lang.home,lang.mother,lang.father)
table(d1$lang.r2=="German",d1$lang.job)/sum(d1$lang.r2=="German")
table(d1$lang.r2=="German",d1$lang.friends)/sum(d1$lang.r2=="German")
table(d1$lang.r2=="German",d1$lang.home)/sum(d1$lang.r2=="German")
table(d1$lang.r2=="German",d1$lang.mother)/sum(d1$lang.r2=="German")

# Turkish language in second wave
# lang.job,lang.friends,lang.home,lang.mother,lang.father)
table(d1$lang.r2=="Turkish",d1$lang.job)/sum(d1$lang.r2=="Turkish")
table(d1$lang.r2=="Turkish",d1$lang.friends)/sum(d1$lang.r2=="Turkish")
table(d1$lang.r2=="Turkish",d1$lang.home)/sum(d1$lang.r2=="Turkish")
table(d1$lang.r2=="Turkish",d1$lang.mother)/sum(d1$lang.r2=="Turkish")

# Papiamento language in second wave
# lang.job,lang.friends,lang.home,lang.mother,lang.father)
table(d1$lang.r2=="Papiamento",d1$lang.job)/sum(d1$lang.r2=="Papiamento")
table(d1$lang.r2=="Papiamento",d1$lang.friends)/sum(d1$lang.r2=="Papiamento")
table(d1$lang.r2=="Papiamento",d1$lang.home)/sum(d1$lang.r2=="Papiamento")
table(d1$lang.r2=="Papiamento",d1$lang.mother)/sum(d1$lang.r2=="Papiamento")


### Simulation page @ ##
ref<-citation(package = "MASS", lib.loc = NULL)
## S3 method for class 'citation':
toBibtex(ref)

library(MASS)
mu1 <- c(3.607,3.866,6.980,6.807)
mu2 <- c(3.607,3.607,6.980,6.980)
mu2 <- c(4.607,3.607,5.980,6.980)

Sigma <- matrix(c(3.121,2.314,-2.131,-1.804,
                  2.314,2.758,-1.680,-1.648,
                  -2.131,-1.680,3.119,2.186,
                  -1.804,-1.648,2.186,3.083), 
                nrow=4, ncol=4) 

raw1 <- mvrnorm(n=1000, mu=mu1, Sigma=Sigma,empirical=T)
raw1<-data.frame(raw1)
cov(raw1); cor(raw1)

raw2 <- mvrnorm(n=1000, mu=mu2, Sigma=Sigma,empirical=T)
raw2<-data.frame(raw2)
cov(raw2); cor(raw2)

Nm<- c("satD","satO","c.satD","c.satO")
colnames(raw1) <- Nm
colnames(raw2) <- Nm
attach(raw1)
par(mfrow=c(2,2))
plot(raw1$satD,raw1$satO,  
     xlab="Satisfaction Dutch", ylab="Satisfaction Other", 
     pch=20, cex=0.5,col=c("blue"),
     xlim=c(0, 10), ylim=c(0, 10)
)

points(raw2$satD,raw2$satO, 
       pch=20, cex=0.5,col=c("red"))
abline(lm(raw1$satO~raw1$satD), col="blue", lwd=1.8) # regression line (y~x)
abline(lm(raw2$satO~raw2$satD), col="red", lwd=1.8) # regression line (y~x)
abline(0,1, col="black", lwd=1.8) # regression line (y~x) 

plot(c.satD,c.satO, main="Correlation of need of change",
     xlab="Need of change Dutch", ylab="Need of change Other", 
     pch=20, cex=0.5,col=c("blue","red"),
     xlim=c(0, 10), ylim=c(0, 10)
     )

abline(lm(satO~satD), col="red", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen",lwd=1.5) # regression line (y~x) 
detach(raw1) 
attach(raw2)
plot(satD,satO, main="Correlation of satisfaction", 
     xlab="Satisfaction Dutch", ylab="Satisfaction Other", 
     pch=20, cex=0.5,col=c("blue","red"),
          xlim=c(0, 10), ylim=c(0, 10)
)
abline(lm(satD~satO), col="black", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen", lwd=1.5) # regression line (y~x) 

plot(c.satD,c.satO, main="Correlation of need of change",
     xlab="Need of change Dutch", ylab="Need of change Other", 
     pch=20, cex=0.5,col=c("blue","red"),
          xlim=c(0, 10), ylim=c(0, 10)
)

abline(lm(satO~satD), col="red", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen",lwd=1.5) # regression line (y~x) 
detach(raw2)


mu1 <- c(5.645,5.645,5.831,5.831)
mu2 <- c(4.645,5.645,4.831,5.831)


Sigma <- matrix(c(1.813,1.373,-1.311,-1.114,
                  1.373,1.729,-1.135,-1.023,
                  -1.311,-1.135,2.215,1.353,
                  -1.114,-1.023,1.353,1.196), 
                nrow=4, ncol=4) 

raw1 <- mvrnorm(n=1000, mu=mu1, Sigma=Sigma,empirical=T)
raw1<-data.frame(raw1)
cov(raw1); cor(raw1)

raw2 <- mvrnorm(n=1000, mu=mu2, Sigma=Sigma,empirical=T)
raw2<-data.frame(raw2)
cov(raw2); cor(raw2)

Nm<- c("TrustD","TrustO","changeD","changeO")
colnames(raw1) <- Nm
colnames(raw2) <- Nm


plot(raw1$TrustD,raw1$TrustO,  
     xlab="Trust Dutch", ylab="Trust Other", 
     pch=20, cex=0.5,col=c("blue"),
     xlim=c(0, 10), ylim=c(0, 10)
)

points(raw2$TrustD,raw2$TrustO, 
       pch=20, cex=0.5,col=c("red"))
abline(lm(raw1$TrustO~raw1$TrustD), col="blue", lwd=1.8) # regression line (y~x)
abline(lm(raw2$TrustO~raw2$TrustD), col="red", lwd=1.8) # regression line (y~x)
abline(0,1, col="black", lwd=1.8) # regression line (y~x) 

plot(changeD,changeO, main="Correlation of need of change",
     xlab="Need of change Dutch", ylab="Need of change Other", 
     pch=20, cex=0.5,col=c("blue","red"),
     xlim=c(0, 10), ylim=c(0, 10)
)

abline(lm(satO~satD), col="red", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen",lwd=1.5) # regression line (y~x) 
detach(raw1) 
attach(raw2)
plot(satD,satO, main="Correlation of satisfaction", 
     xlab="Satisfaction Dutch", ylab="Satisfaction Other", 
     pch=20, cex=0.5,col=c("blue","red"),
     xlim=c(0, 10), ylim=c(0, 10)
)
abline(lm(satD~satO), col="black", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen", lwd=1.5) # regression line (y~x) 

plot(c.satD,c.satO, main="Correlation of need of change",
     xlab="Need of change Dutch", ylab="Need of change Other", 
     pch=20, cex=0.5,col=c("blue","red"),
     xlim=c(0, 10), ylim=c(0, 10)
)

abline(lm(satO~satD), col="red", lwd=1.5) # regression line (y~x)
abline(0,1, col="darkgreen",lwd=1.5) # regression line (y~x) 
detach(raw2)

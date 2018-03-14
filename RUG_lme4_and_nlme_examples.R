library(lme4)
??lmer
## linear mixed models
data(sleepstudy)

??sleepstudy
str(sleepstudy)
head(sleepstudy)

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1) 
plot(fm1) #look at residuals!

fm1_ML <- update(fm1,REML=FALSE)
summary(fm1_ML) 
plot(fm1_ML) #look at residuals!

library(nlme)
??lme
fm2 <- lme(Reaction ~ Days, data = sleepstudy, random = ~ Days | Subject)
summary(fm2)
fm2_ML <- update(fm2,method= "ML")
summary(fm2_ML)
plot(fm2_ML) #look at residuals!

#adding weights
str(sleepstudy)
sub<-sleepstudy[sample(nrow(sleepstudy), 100), ]
head(sub)

library("plyr")
ns<-ddply(sub,c("Subject"),summarize, n=length(Days) )
sub$n<-ns[match(sub$Subject,ns$Subject),2]
head(sub)
(fm1.w <- lmer(Reaction ~ Days +(Days | Subject),weights=n, sub))
summary(fm1.w)
plot(fm1.w) #look at residuals!

fm2.w <- lme(Reaction ~ Days+ offset(n), data = sub, random = ~ Days | Subject,weights=~n)
summary(fm2.w)
plot(fm2.w) #look at residuals!

(fm1.o <- lmer(Reaction ~ Days +  offset(n)+(Days | Subject),data= sub))
summary(fm1.o)

fm2.o <- lme(Reaction ~ Days+ offset(n), data = sub, random = ~ Days | Subject)
summary(fm2.o)


#offset
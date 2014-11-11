
test <-subset(bci.mainstem, !is.na(bci.mainstem$sg100c_avg) & !is.na(bci.mainstem$dead.next.census & !is.na(bci.mainstem$rel.dbh)) & !is.na(bci.mainstem$dbh.gr))
test$dead.next.census[is.na(test$dead.next.census)]<-0
library(survival)
test <-subset(bci.mainstem, !is.na(bci.mainstem$sg100c_avg) & !is.na(bci.mainstem$dead.next.census & !is.na(rel.dbh)) & !is.na(dbh.gr))
test$sizeclass <- 0
test$sizeclass[test$rel.dbh > 0.33]<- 1
test$sizeclass[test$rel.dbh > 0.63]<- 2
test$dead.next.census[is.na(test$dead.next.census)]<-0
lfit <- survfit(Surv(rel.dbh, dead.next.census) ~ 1, data=test)


plot(lfit, xlab="time", ylab="Survival Probability")
lines(lfit, col='pink')



for(i in unique(test$sp)) {
  lfit[[i]] <- survfit(Surv(rel.dbh, dead.next.census) ~ 1, data= subset(bci.mainstem, 
                                                                       bci.mainstem$sp== i & !is.na(bci.mainstem$sg100c_avg) & 
                                                                         !is.na(bci.mainstem$dead.next.census & !is.na(rel.dbh)) & !is.na(dbh.gr)))
}

name <- unique(test$sp)
par(mfrow=c(4,4), mar=c(2,2,1,1), oma=c(2.5,2.5,0,0))
for(i in 1:16){
plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)

par(mfrow=c(4,4), mar=c(2,2,1,1), oma=c(2.5,2.5,0,0))

for(i in 17:32){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)


for(i in 33:48){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)

for(i in 49:64){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)

for(i in 65:80){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)

for(i in 81:96){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)


for(i in 97:112){
  plot(lfit[[name[i]]], main = name[i])
}
mtext(side = 1, text = 'Relative DBH', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)



dev.off()

critval <- 1.96 ## approx 95% CI
upr <- te$fit + (critval * te$se.fit)
lwr <- te$fit - (critval * te$se.fit)

plot(te$fitted.values ~ test$dbh[test$sp==name[70]], ylim=c(0,1), xlab='DBH (mm)', ylab='Prob of dying within 5 years')

te <-glm(dead.next.census ~ dbh  + I(dbh^2), data = test[test$sp=="hybapr",], family = "binomial")

plot.new()
abline(te)

spname <- c('alsebl','des2pa','faraoc','gar2in','hybapr','mourmy','tri2tu')
par(mfrow=c(3,3))
for(i in spname){
mod1 <- glm(dead.next.census ~ rel.dbh  + I(rel.dbh^2), data = test[test$sp=='tri2tu',], family = binomial(link="logit"))
plotdat <- data.frame(rel.dbh= seq(0,1,0.01))
preddat <- predict(mod1, newdata=plotdat, se.fit=TRUE)
with(test[test$sp=='tri2tu',], plot(rel.dbh, dead.next.census, type="n", 
                 ylim=c(0, 1), ylab="Probability of dying", xlab="DBH", main='tri2tu'))
with(preddat, lines(seq(0,1,0.01), exp(fit)/(1+exp(fit)), col="blue"), lwd=2)
with(preddat, lines(seq(0,1,0.01), exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, lwd=2))
with(preddat, lines(seq(0,1,0.01), exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, lwd=2))

}

#KM Curves

bci.mainstem$dead.next.census[is.na(bci.mainstem$dead.next.census)]<-0
bci.mainstem <- bci.mainstem[!is.na(bci.mainstem$end),]
bci.mainstem <- bci.mainstem %.%

group_by(sp) %.% 
  mutate(rel.dbh = dbh/max(dbh)) 
bci.mainstem$sizeclass <- 0
bci.mainstem$sizeclass[bci.mainstem$rel.dbh < 0.21] <- 0
bci.mainstem$sizeclass[bci.mainstem$rel.dbh > 0.20] <- 1
bci.mainstem$sizeclass[bci.mainstem$rel.dbh > 0.40] <- 2

lfit <- list()
coxph <- list()
for(i in spname) {
  lfit[[i]] <- survfit(Surv(time = julian, time2 = end, event = dead.next.census) ~ sizeclass, data= subset(bci.mainstem, 
                                                                         bci.mainstem$sp== i))
  
  coxph[[i]] <- aareg(Surv(time = julian, time2 = end, event = dead.next.census) ~ rel.dbh, data= subset(bci.mainstem, 
                                                                                                           bci.mainstem$sp== i))
}

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)

par(mfrow=c(3,3), mar=c(2,2,2,1), oma=c(2.5,2.5,0.1,0.1))
for(i in spname){
  plot(lfit[[i]], main = i, col=c('blue','black','red'))
}
mtext(side = 1, text = 'Days', outer=TRUE, line = 1)
mtext(side = 2, text = 'Survival Probability', outer=TRUE, line=1)

legend(x = 2000, y = 0.4, legend = c('dbh (0-20%)', 'dbh (21-40)', 'dbh (> 40%)'), lty=1, col=c('blue', 'black', 'red'), bty = 'n')


test <- subset(bci.mainstem, 
       bci.mainstem$sp== 'hybapr')

te<-aalen(Surv(time = julian, time2 = end, event = dead.next.census) ~ rel.dbh + dbh.gr, id = bci.mainstem$treeid[bci.mainstem$sp== 'alsebl'],data= subset(bci.mainstem, 
                                                                                         bci.mainstem$sp== 'alsebl'))


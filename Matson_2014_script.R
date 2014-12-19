library(ROCR)
library(MASS)
library(ggplot2)
library(pROC)

# Read in data
raw.clin.data <- read.csv('clin-data.csv', na.string=c('NA', '<NA>'), stringsAsFactors=FALSE)
clin.data <- raw.clin.data

# Pre-process data
names(clin.data) <- c('id', 'dz', 'severe.dz', 'new.bio', 'old.bio1', 'old.bio2', 'old.bio3', 'old.2.3.ratio', 'smoke.qen', 'smoke.qey', 'race')

clin.data$new.bio <- (clin.data$new.bio-mean(clin.data$new.bio))/sd(clin.data$new.bio)

numeric.strings.bio1 <- clin.data$old.bio1[clin.data$old.bio1 != '+' & clin.data$old.bio1 != '-' & !is.na(clin.data$old.bio1)]
clin.data$old.bio1[clin.data$old.bio1=='+'] <- 50
clin.data$old.bio1[clin.data$old.bio1=='-'] <- 0
clin.data$old.bio1 <- as.numeric(clin.data$old.bio1)
clin.data$old.bio1 <- (clin.data$old.bio1-mean(clin.data$old.bio1, na.rm=T))/sd(clin.data$old.bio1, na.rm=T)

numeric.strings.bio2 <- clin.data$old.bio2[clin.data$old.bio2 != '+' & clin.data$old.bio2 != '-' & !is.na(clin.data$old.bio2)]
clin.data$old.bio2[clin.data$old.bio2=='+'] <- 10000
clin.data$old.bio2[clin.data$old.bio2=='-'] <- 0
clin.data$old.bio2 <- as.numeric(clin.data$old.bio2)
clin.data$old.bio2 <- (clin.data$old.bio2-mean(clin.data$old.bio2, na.rm=T))/sd(clin.data$old.bio2, na.rm=T)

numeric.strings.bio3 <- clin.data$old.bio3[clin.data$old.bio3 != '+' & clin.data$old.bio3 != '-' & !is.na(clin.data$old.bio3)]
clin.data$old.bio3[clin.data$old.bio3=='+'] <- 2000
clin.data$old.bio3[clin.data$old.bio3=='-'] <- 0
clin.data$old.bio3 <- as.numeric(clin.data$old.bio3)
clin.data$old.bio3 <- (clin.data$old.bio3-mean(clin.data$old.bio3, na.rm=T))/sd(clin.data$old.bio3, na.rm=T)

clin.data$dz <- as.factor(clin.data$dz)
clin.data$severe.dz <- as.factor(clin.data$severe.dz)
clin.data$smoke.qen <- as.factor(clin.data$smoke.qen)
clin.data$smoke.qen <- as.numeric(clin.data$smoke.qen)-1
clin.data$smoke.qey <- as.factor(clin.data$smoke.qey)
clin.data$race <- factor(clin.data$race, levels=c("W", "A", "B"))
clin.data$mild <- clin.data$dz=='Prex' & clin.data$severe.dz==0
clin.data <- clin.data[!clin.data$mild,]
clin.data <- clin.data[!clin.data$smoke.qen,]



# FIGURE 2
par(mar=c(1,1,1,1))
fit.1 <- glm(severe.dz ~ old.bio1, data=clin.data, family=binomial, na.action=na.omit)
ps.1 <- predict(fit.1, type="response")
r1 <- roc(response=as.numeric(clin.data$severe.dz), predictor=ps.1, plot=T, smooth=F, col="black", lty=1, n=1, legacy.axes=T)

fit.2 <- glm(severe.dz ~ old.bio3, data=clin.data, family=binomial, na.action=na.omit)
ps.2 <- predict(fit.2, type="response")
r2 <- roc(response=as.numeric(clin.data$severe.dz), predictor=ps.2, plot=T, smooth=F, col=1, lty=2, add=T)

fit.3 <- glm(severe.dz ~ new.bio, data=clin.data, family=binomial, na.action=na.omit)
ps.3 <- predict(fit.3, type="response")
r3 <- roc(response=as.numeric(clin.data$severe.dz), predictor=ps.3, plot=T, smooth=F, col=1, lty=3, add=T)

fit.4 <- glm(severe.dz ~ old.bio1 + new.bio, data=clin.data, family=binomial, na.action=na.omit)
ps.4 <- predict(fit.4, type="response")
r4 <- roc(response=as.numeric(clin.data$severe.dz), predictor=ps.4, plot=T, smooth=F, col=1, lty=4, add=T)

fit.5 <- glm(severe.dz ~ old.bio3 + new.bio, data=clin.data, family=binomial, na.action=na.omit)
ps.5 <- predict(fit.5, type="response")
r5 <- roc(response=as.numeric(clin.data$severe.dz), predictor=ps.5, plot=T, smooth=F, col=1, lty=5, add=T)  

y.start <- 0.19
y.spacing <- 0.04
x.start <- 0.15
text(x=x.start, y=y.start-1*y.spacing, adj=1, "ENG", col="black")
text(x=x.start, y=y.start-2*y.spacing, adj=1, "PlGF", col="black")
text(x=x.start, y=y.start-3*y.spacing, adj=1, "MR-proADM", col="black")
text(x=x.start, y=y.start-4*y.spacing, adj=1, "ENG + MR-proADM", col="black")
text(x=x.start, y=y.start-5*y.spacing, adj=1, "PlGF + MR-proADM", col="black")

x.start <- x.start-0.1
text(x=x.start, y=y.start+0*y.spacing, adj=0, "AUC")
text(x=x.start, y=y.start-1*y.spacing, adj=0, round(r1$auc[1],2), col="black")
text(x=x.start, y=y.start-2*y.spacing, adj=0, round(r2$auc[1],2), col="black")
text(x=x.start, y=y.start-3*y.spacing, adj=0, round(r3$auc[1],2), col="black")
text(x=x.start, y=y.start-4*y.spacing, adj=0, sprintf("%.2f", round(r4$auc[1],2)), col="black")
text(x=x.start, y=y.start-5*y.spacing, adj=0, round(r5$auc[1],2), col="black")

y.vals <- seq(y.start-1*y.spacing, y.start-5*y.spacing, by=-y.spacing)
segments(x0=0.14, x1=0.06, y0=y.vals, y1=y.vals, col=1, lty=1:5, lwd=2)
setwd(...)

# load libraries
source("Libraries.R")






##########################################
########## COMPARE JOINT MODELS ##########
##########################################

#load model
load(file="joint5.rda")
load(file="joint5a.rda")
load(file="joint5b.rda")


## read in data
long_val <- read.csv("long_val.csv.gz")
surv_val <- read.csv("surv_val.csv.gz")







development <- long_val
validation <- surv_val	
	
	
	

## MODEL: joint5 ##
model <- joint5

fit04 <- NULL
for(start in 1:11){

	id <- unique(development[which(development$months==start),"id"])
	dat <- development[which(development$id %in% id & development$months==start),]
	end <- eval(start + 1)
	
	set.seed(1234)
	fit01 <- survfitJM(model, newdata = dat, idVar = "id", survTimes=end, simulate = FALSE)
	
	fit02 <- data.frame(matrix(unlist(fit01$summaries),  ncol=2, byrow=TRUE))

	nms <- data.frame(matrix(names(fit01$summaries), ncol=1))
	fit03 <- cbind(nms, fit02)
	colnames(fit03) <- c("id","times","predSurv")

	fit04 <- rbind(fit04,fit03)
	
	rm(id,dat,end,fit01,fit02,nms,fit03)
	}
	
fit05 <- merge(fit04, validation, by="id", all.x=TRUE)
fit05$s2 <- fit05$s
fit05$s <- ifelse(fit05$times==fit05$month, fit05$s2, 0)				
fit05 <- fit05[order(fit05$predSurv),]	
	
	
	

## MODEL: joint5b - cumulative ##
model <- joint5a

fit04 <- NULL
for(start in 1:11){

	id <- unique(development[which(development$months==start),"id"])
	dat <- development[which(development$id %in% id & development$months==start),]
	end <- eval(start + 1)
	
	set.seed(1234)
	fit01 <- survfitJM(model, newdata = dat, idVar = "id", survTimes=end, simulate = FALSE)
	
	fit02 <- data.frame(matrix(unlist(fit01$summaries),  ncol=2, byrow=TRUE))

	nms <- data.frame(matrix(names(fit01$summaries), ncol=1))
	fit03 <- cbind(nms, fit02)
	colnames(fit03) <- c("id","times","predSurv")

	fit04 <- rbind(fit04,fit03)
	
	rm(id,dat,end,fit01,fit02,nms,fit03)
	}	

fit06 <- merge(fit04, validation, by="id", all.x=TRUE)
fit06$s2 <- fit06$s
fit06$s <- ifelse(fit06$times==fit06$month, fit06$s2, 0)				
fit06 <- fit06[order(fit06$predSurv),]	
	
	
	

## MODEL: joint5a - slope ##
model <- joint5b

fit04 <- NULL
for(start in 1:11){

	id <- unique(development[which(development$months==start),"id"])
	dat <- development[which(development$id %in% id & development$months==start),]
	end <- eval(start + 1)
	
	set.seed(1234)
	fit01 <- survfitJM(model, newdata = dat, idVar = "id", survTimes=end, simulate = FALSE)
	
	fit02 <- data.frame(matrix(unlist(fit01$summaries),  ncol=2, byrow=TRUE))

	nms <- data.frame(matrix(names(fit01$summaries), ncol=1))
	fit03 <- cbind(nms, fit02)
	colnames(fit03) <- c("id","times","predSurv")

	fit04 <- rbind(fit04,fit03)
	
	rm(id,dat,end,fit01,fit02,nms,fit03)
	}	

fit07 <- merge(fit04, validation, by="id", all.x=TRUE)
fit07$s2 <- fit07$s
fit07$s <- ifelse(fit07$times==fit07$month, fit07$s2, 0)				
fit07 <- fit07[order(fit07$predSurv),]	
	
	
	
	

## ROC CURVE OF THREE JOINT MODELS TO COMPARE ##
png('C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/images/roc_joints5.png',width=6, height=5, units='in', res=300)
	roc00 <- plot(roc(s ~ predSurv, data=fit05, direction=">"), print.auc = TRUE, col = "blue", print.auc.y = .5)
	roc00 <- plot(roc(s ~ predSurv, data=fit06, direction=">"), print.auc = TRUE, col = "forestgreen", print.auc.y = .4, add = TRUE)
	roc00 <- plot(roc(s ~ predSurv, data=fit07, direction=">"), print.auc = TRUE, col = "red", print.auc.y = .3, add = TRUE)
	legend(1.1,1, c("Joint Model","Slope", "Cumulative"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("blue","forestgreen","red")) 
dev.off()
















######################################################
########## COMPARE JOINT AND SURVIVAL MODEL ##########
######################################################

load(file="joint5.rda")
load(file="survFit04.rda")



## read in data
# long_dev <- read.csv("long_dev.csv.gz")
# surv_dev <- read.csv("surv_dev.csv.gz")
long_val <- read.csv("long_val.csv.gz")
surv_val <- read.csv("surv_val.csv.gz")
# dat_val_may <- read.csv("dat_val_may.csv.gz")
	
	
	
	

## JOINT MODEL ##
model <- joint5
development <- long_val
validation <- surv_val

fit04 <- NULL
for(start in 1:11){

	id <- unique(development[which(development$months==start),"id"])			
	dat <- development[which(development$id %in% id & development$months==start),]	
	end <- eval(start + 1)	
	
	set.seed(1234)
	fit01 <- survfitJM(model, newdata = dat, idVar = "id", survTimes=end, simulate = FALSE)
	
	fit02 <- data.frame(matrix(unlist(fit01$summaries),  ncol=2, byrow=TRUE))

	nms <- data.frame(matrix(names(fit01$summaries), ncol=1))
	fit03 <- cbind(nms, fit02)
	colnames(fit03) <- c("id","times","predSurv")

	fit04 <- rbind(fit04,fit03)
	
	rm(id,dat,end,fit01,fit02,nms,fit03)
	}			

fit05 <- merge(fit04, validation, by="id", all.x=TRUE)
fit05$s2 <- fit05$s
fit05$s <- ifelse(fit05$times==fit05$month, fit05$s2, 0)		
fit05 <- fit05[order(fit05$predSurv),]	
	
	
	
	

## SURVIVAL MODEL ##
model <- survFit04
development <- long_val
validation <- surv_val

surv04 <- NULL
for(start in 1:11){

	id <- unique(development[which(development$months==start),"id"])
	dat <- development[which(development$id %in% id & development$months==start),]
	end <- eval(start + 1)
	
	set.seed(1234)
	fit01 <- survfit(model, newdata = dat, indiviual=TRUE, start.time=start)
	
	fit02 <- as.data.frame(matrix(summary(fit01, times=end)$surv, ncol=1))

	nms <- data.frame(matrix(dat$id, ncol=1))
	fit03 <- cbind(nms, end, fit02)
	colnames(fit03) <- c("id","times","predSurv")

	surv04 <- rbind(surv04,fit03)
	
	rm(id,dat,end,fit01,fit02,nms,fit03)
	}

fit06 <- merge(surv04, validation, by="id", all.x=TRUE)
fit06$s2 <- fit06$s
fit06$s <- ifelse(fit06$times==fit06$month, fit06$s2, 0)		
fit06 <- fit06[order(fit06$predSurv),]	




	
## SIMPLE GRAPH ##
# png('roc_compare5.png',width=6, height=5, units='in', res=300)
	# roc00 <- plot(roc(s ~ predSurv, data=fit05, direction=">"), print.auc = TRUE, col = "blue")
	# roc00 <- plot(roc(s ~ predSurv, data=fit06, direction=">"), print.auc = TRUE, col = "forestgreen", print.auc.y = .4, add = TRUE)
	# legend(1.1,1, c("Joint Model","Survival Model"), lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","forestgreen")) 
# dev.off()	



## GRAPH THAT COMPARES SURVIVAL AND JOINT - ADDING CONFIDENCE INTERVALS ##
#roc curves
roc1 <- roc(s ~ predSurv, data=fit05, direction=">", ci=TRUE)
roc2 <- roc(s ~ predSurv, data=fit06, direction=">", ci=TRUE)
#confidence intervals
ci1 <- ci.sp(roc1, sensitivities=seq(0, 1, .01), boot.n=100, conf.level=0.95)
ci2 <- ci.sp(roc2, sensitivities=seq(0, 1, .01), boot.n=100, conf.level=0.95)

png('roc_compare5.png',width=6, height=5, units='in', res=300)
	plot(roc1, grid=TRUE)
		
	plot(ci1, type="shape", col="lightcyan2", no.roc=TRUE)
	plot(roc1, print.auc = TRUE, col="blue", add=TRUE, ci=TRUE)
	plot(ci2, type="shape", col="honeydew2", add=TRUE, no.roc=TRUE)
	plot(roc2, print.auc = TRUE, col = "forestgreen", print.auc.y = .4, add = TRUE, ci=TRUE)
	
	legend(1.1,1, c("Joint Model","Survival Model"), lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","forestgreen")) 
dev.off()	



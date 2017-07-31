setwd(...)

# load libraries
source("Libraries.R")



#load joint model
load(file="joint5.rda")
load(file="joint5a.rda")
load(file="joint5b.rda")






## read in data
long_dev <- read.csv("long_dev.csv.gz")
surv_dev <- read.csv("surv_dev.csv.gz")

long_val <- read.csv("long_val.csv.gz")
surv_val <- read.csv("surv_val.csv.gz")

dat_val_may <- read.csv("dat_val_may.csv.gz")











##predict one month out

#pick model
model <- joint5
# model <- joint5a
# model <- joint5b


#pick data pair
development <- long_dev
validation <- surv_dev
# development <- long_val
# validation <- surv_val
# development <- dat_val_may[which(dat_val_may$dates=="2016-04-01" & is.na(dat_val_may$pct_WL_mo)==0),]
# validation <- dat_val_may[which(dat_val_may$dates=="2016-05-01"),]


	fit04 <- NULL
		for(start in 1:11){#last month / start
		# start<-3

		
			id <- unique(development[which(development$months==start),"id"])
			
			dat <- development[which(development$id %in% id & development$months==start),]	## predicts start month to end month
			
			end <- eval(start + 1)			## predict one month out
			#end <- 12						## predict in month 12
			#end <- eval(start + 1):12		## predict one month out through month 12
			
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

summaryBy(predSurv~s, fit05, FUN=c(mean,length))

summary(fit05$predSurv)


### ROC ###
roc01 <- roc(s ~ predSurv, data=fit05, direction=">", plot=TRUE)

roc01$auc	# probability that the classifier will rank a randomly chosen positive instance higher htan a randomly chosen negative instance
# roc01$direction


P <- sum(fit05$s)
N <- length(fit05$s) - sum(fit05$s)
T <- length(fit05$s)
P/T

TP <- roc01$sensitivities*P
TN <- roc01$specificities*N

acc <- (TP+TN)/T
# plot(roc01$thresholds, acc)
# abline(v=0.77)


#pick threshold 
fit05$pred <- with(fit05, ifelse(predSurv>=0.81, 0, 1)) 
conf <- with(fit05, round((table(s, pred)/T)*100,2))
conf
# conf[2,1] #minimize - false negative
# conf[1,2] #minimize - false positive
conf[1,1]+conf[2,2] #maximize - accuracy
conf[2,2] #true positive
# conf[1,1] #true negative

sum(fit05$pred)
sum(fit05$s)




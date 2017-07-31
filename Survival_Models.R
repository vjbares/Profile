setwd(...)

# load libraries
source("Libraries.R")

## read in data - last month is february 2016
surv_dev <- read.csv("surv_dev.csv.gz")
surv_val <- read.csv("surv_val.csv.gz")























# PLOTS #

survival01 <- survfit(Surv(month, s) ~ 1, data=surv_dev)
#KM
plot(survival01)


library(GGally)
# nicer looking graph of KM
png('surv.png',width=8, height=5, units='in', res=300)
	ggsurv(survival01, CI=FALSE, plot.cens=FALSE, surv.col="blue", size.est=1) +
	xlab("Months") +	
	ylab("Survival Probability") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = seq(0,1,0.1)) +
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right"
	)
dev.off()














summaryBy(month ~ s, surv_dev, FUN=c(mean,length))


### VARIABLE SELECTION ###

#each variable in the model - look at performance
AIC(coxph(Surv(month, s) ~ gender , data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ marital_status2, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ first_weight, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ start_bmi, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ med_total, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ med_indicator, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ start_month, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ start_season, data = surv_dev, x = TRUE) )



#correlation between survival and variable - pick the month
cor.test(surv_dev[which(surv_dev$month==5),"age"], surv_dev[which(surv_dev$month==5),"s"], method="spearman")$estimate
		




correlation.function <- function(vari){
	for(mon in 4:12){
		mon3<-mon-3

		if(mon==4){
			corr_data <- as.data.frame(matrix(nrow=8, ncol=4))
			colnames(corr_data) <- c("variable", "month", "pearsons", "pvalue")
			
			corr_data[mon3,1] <- vari
			corr_data[mon3,2] <- mon
			corr_data[mon3,3] <- cor.test(surv_dev[which(surv_dev$month==mon),vari], surv_dev[which(surv_dev$month==mon),"s"], method="pearson")$estimate
			corr_data[mon3,4] <- cor.test(surv_dev[which(surv_dev$month==mon),vari], surv_dev[which(surv_dev$month==mon),"s"], method="pearson")$p.value
		}
		else{
			corr_data[mon3,1] <- vari
			corr_data[mon3,2] <- mon
			corr_data[mon3,3] <- cor.test(surv_dev[which(surv_dev$month==mon),vari], surv_dev[which(surv_dev$month==mon),"s"], method="pearson")$estimate
			corr_data[mon3,4] <- cor.test(surv_dev[which(surv_dev$month==mon),vari], surv_dev[which(surv_dev$month==mon),"s"], method="pearson")$p.value
		}
	}
return(corr_data)
}

corr_age <- correlation.function("age")
corr_first_weight <- correlation.function("first_weight")
corr_start_bmi <- correlation.function("start_bmi")
corr_med_total <- correlation.function("med_total")
corr_med_indicator <- correlation.function("med_indicator")
corr_start_month <- correlation.function("start_month")


#combine all data 
corr_all <- rbind(corr_age,corr_first_weight,corr_start_bmi,corr_med_total,corr_med_indicator,corr_start_month)
corr_all

#print by month
print(corr_all[which(corr_all$month==4),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==5),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==6),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==7),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==8),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==9),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==10),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==11),c("variable","pearsons","pvalue")], row.names=FALSE)
print(corr_all[which(corr_all$month==12),c("variable","pearsons","pvalue")], row.names=FALSE)


#graphs
correlation.plot <- function(dat, titl){
	ggplot(dat, aes(x=month, y=pearsons)) +
		geom_line(size=3) +	
		geom_point(size=3, shape=21, fill="white") +
		xlab("Months in Program") +	
		ylab("Pearsons Correlation Coefficient") + 
		ggtitle(titl) + 
		scale_x_continuous(breaks = seq(0, 12, 1)) +
		scale_y_continuous(breaks = seq(-1,1,0.2), limit = c(-1,1)) +
		theme(
			axis.text = element_text(size = 16),
			axis.title = element_text(size = 18),
			panel.grid.major = element_line(colour = "white"),
			panel.grid.minor = element_blank(),
			panel.background = element_rect(fill = "grey90"),
			legend.text = element_text(size = 16),
			legend.title = element_text(size = 16),
			legend.position="right",
			plot.title = element_text(size = 20)
		)
}


	correlation.plot(corr_age, "age")
	correlation.plot(corr_first_weight, "first_weight")
	correlation.plot(corr_start_bmi, "start_bmi")
	correlation.plot(corr_med_total, "med_total")
	correlation.plot(corr_med_indicator, "med_indicator")
	correlation.plot(corr_start_month, "start_month")


	
	
##  PLOTS  ##
plot(dat_long_dev[which(dat_long_dev$month==5),c(3,5:11,20,21)])		
#all months together
plot(surv_dev[,c(3,5:11,20,21)])		




































#survival model
# gender, age, marital_status, first_weight, start_bmi, med_total, med_indicator,
#     med_blood_pressure, med_antidepressant, med_cholesterol, med_sleep, med_diabetes,
#     med_thyroid, med_vitamin, med_birthcontrol, start_month, start_season

summary(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )
#start month is not significant

summary(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_bmi + gender, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_bmi + gender, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + med_total + start_bmi + gender, data = surv_dev, x = TRUE) )
#try taking med_total out

summary(coxph(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, x = TRUE) )
#AIC went up slightly after removing med_total

#trying only two vars
summary(coxph(Surv(month, s) ~ age + marital_status2, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2, data = surv_dev, x = TRUE) )

#add gender
summary(coxph(Surv(month, s) ~ age + marital_status2 + gender, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + gender, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + gender, data = surv_dev, x = TRUE) )

#add med_total
summary(coxph(Surv(month, s) ~ age + marital_status2 + gender + med_total, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + gender + med_total, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + gender + med_total, data = surv_dev, x = TRUE) )

#start bmi instead of med total
summary(coxph(Surv(month, s) ~ age + marital_status2 + gender + start_bmi, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + gender + start_bmi, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + gender + start_bmi, data = surv_dev, x = TRUE) )

#
summary(coxph(Surv(month, s) ~ age + med_total + gender + start_bmi, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + med_total + gender + start_bmi, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + med_total + gender + start_bmi, data = surv_dev, x = TRUE) )

#
summary(coxph(Surv(month, s) ~ age + gender + start_bmi, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + gender + start_bmi, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + gender + start_bmi, data = surv_dev, x = TRUE) )

#
summary(coxph(Surv(month, s) ~ age + marital_status2 + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )
AIC(coxph(Surv(month, s) ~ age + marital_status2 + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )
Anova(coxph(Surv(month, s) ~ age + marital_status2 + start_month + start_bmi + gender, data = surv_dev, x = TRUE) )






# #alternatively use step() 
# surv01 <- coxph(Surv(month, s) ~ age + marital_status2 + start_month + start_bmi + gender, data = surv_dev, x = TRUE)
# results.step <- step(surv01)
# summary(surv01)
# Anova(surv01)
# #same as results from above


survFit01 <- coxph(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, x = TRUE) 
summary(survFit01)
survFit01$coef
AIC(survFit01)
Anova(survFit01)


survFit02 <- survreg(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, dist='weibull', x = TRUE) 
summary(survFit02)
survFit02$coef
AIC(survFit02)
Anova(survFit02)


survFit03 <- survreg(Surv(month, s) ~ age + marital_status2 + start_bmi + gender, data = surv_dev, dist='exponential', x = TRUE) 
summary(survFit03)
survFit03$coef
AIC(survFit03)
Anova(survFit03)


survFit04 <- coxph(Surv(month, s) ~ age + marital_status2 + gender, data = surv_dev, x = TRUE) 
summary(survFit04)
survFit04$coef
AIC(survFit04)
Anova(survFit04)






## save model
save(survFit04, file="survFit04.rda")












































## ACCURACY ##

## read in data
long_dev <- read.csv("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/data/long_dev.csv.gz")
surv_dev <- read.csv("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/data/surv_dev.csv.gz")

long_val <- read.csv("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/data/long_val.csv.gz")
surv_val <- read.csv("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/data/surv_val.csv.gz")

dat_val_may <- read.csv("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/data/dat_val_may.csv.gz")






# PREDICT ONE MONTH OUT

model <- survFit04

development <- long_dev
validation <- surv_dev

development <- long_val
validation <- surv_val

development <- dat_val_may[which(dat_val_may$dates=="2016-04-01" & is.na(dat_val_may$pct_WL_mo)==0),]
validation <- dat_val_may[which(dat_val_may$dates=="2016-05-01"),]

	fit04 <- NULL
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

			fit04 <- rbind(fit04,fit03)
			
			rm(id,dat,end,fit01,fit02,nms,fit03)
			}
		
		fit05 <- merge(fit04, validation, by="id", all.x=TRUE)
		fit05$s2 <- fit05$s
		fit05$s <- ifelse(fit05$times==fit05$month, fit05$s2, 0)
		
		fit05 <- fit05[order(fit05$predSurv),]
		# fit05$grp <- ntile(fit05$predSurv, 10)
		
		summaryBy(predSurv~s, fit05, FUN=c(mean,length))
		
		summary(fit05$predSurv)


		### ROC ###
		roc01 <- roc(s ~ predSurv, data=fit05, direction=">", plot=TRUE)
		roc01

P <- sum(fit05$s)
N <- length(fit05$s) - sum(fit05$s)
T <- length(fit05$s)
P/T

TP <- roc01$sensitivities*P
TN <- roc01$specificities*N

acc <- (TP+TN)/T
# plot(roc01$thresholds, acc)
# abline(v=0.83)

fit05$pred <- with(fit05, ifelse(predSurv>=0.84, 0, 1)) 
conf <- with(fit05, round((table(s, pred)/T)*100,2))
conf
# conf[2,1] #minimize - false negative
# conf[1,2] #minimize - false positive
conf[1,1]+conf[2,2] #maximize - accuracy
conf[2,2] #true positive
# conf[1,1] #true negative

sum(fit05$pred)
sum(fit05$s)



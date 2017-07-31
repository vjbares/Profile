setwd(...)

# load libraries
source("Libraries.R")

## read in data
dev <- read.csv("long_dev.csv.gz")
val <- read.csv("long_val.csv.gz")
dat_val_may <- read.csv("dat_val_may.csv.gz")

	
	
	
	
	
	
	
	
	
	
	

# # PLOT OF AVERAGE monthly WEIGHT LOSS OVER TIME #
mo_WL_time <- summaryBy(pct_WL_mo ~ months, data = dev, FUN=mean)
colnames(mo_WL_time)[2] <- "pct_WL_mo"

png("mo_WL_time_dev.png",width=7, height=4, units='in', res=300)
	ggplot(mo_WL_time, aes(x=months, y=pct_WL_mo)) +
		geom_line(size=2) +	
		geom_point(size=2, shape=21, fill="white") +
		xlab("Months in Program") +	
		ylab("Average Monthly Weight Loss") + 
		scale_x_continuous(breaks = seq(0, 12, 1)) +
		scale_y_continuous(breaks = c(-15:15)) +
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


mo_WL_time$nl_time <- log(mo_WL_time$months)

png("mo_WL_time2nl_dev.png",width=7, height=4, units='in', res=300)
	ggplot(mo_WL_time, aes(x=nl_time, y=pct_WL_mo)) +
		geom_line(size=2) +	
		geom_point(size=2, shape=21, fill="white") +
		xlab("log(Months in Program)") +	
		ylab("Average Monthly Weight Loss") + 
		scale_x_continuous(breaks = seq(0, 12, 1)) +
		scale_y_continuous(breaks = c(-15:15)) +
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





































##### APPLY THE MODEL #####

## FUNCTION TO APPLY MODEL TO DEVELOPMENT DATA ##
ApplyModel.Plot <- function(model, printimage="yes"){ 
		
	#apply the model
	pred <- as.data.frame(predict(eval(parse(text=model)), newdata=val, level=0, returnData=TRUE))
	colnames(pred) <- "predict.fixed"	
	
	#combine data with prediction
	newpred <- cbind(val,pred)	

	#remove missing actual values - pct_WL_mo
	newpred2 <- newpred[which(is.na(newpred$pct_WL_mo)==0),]
		
	newpred3 <- newpred2	
	# #remove a combined top/bottom 1% of pct_WL_mo to remove outliers
	# newpred3 <-  newpred2[newpred2$pct_WL_mo < quantile(newpred2$pct_WL_mo, 0.995) & newpred2$pct_WL_mo > quantile(newpred2$pct_WL_mo, 0.005), ]
	
	#calculate the difference between actual and predicted - using fixed effects
	newpred3$sig.fixed <- (newpred3$pct_WL_mo - newpred3$predict.fixed)^2
			
	#create a RMSE value - fixed effects
	err.fixed <- sqrt(sum(newpred3$sig.fixed) / nrow(newpred3))
		
	if(printimage=="yes"){	
	#plot predictions against actual
		graph1 <- 
			ggplot(newpred3, aes(x=pct_WL_mo, y=predict.fixed)) + 
				geom_point(color="blue", size=2, shape = 1) +
				geom_abline(intercept=0, slope=1) +
				xlab("Actual") +	
				ylab("Predicted") +				
				ggtitle(bquote(atop(.("Predictions for Validation"), atop(italic(.(paste0("RMSE = ",err.fixed))), "")))) +					
				scale_x_continuous(breaks = seq(-50, 50, 5), limits=c(-10,10)) +
				scale_y_continuous(breaks=seq(-50, 50, 5), limits=c(-10,10)) +
				theme(
					axis.text = element_text(size = 16),
					axis.title = element_text(size = 18),
					panel.grid.major = element_line(colour = "white"),
					panel.grid.minor = element_blank(),
					panel.background = element_rect(fill = "grey90"),
					legend.text = element_text(size = 16),
					legend.title = element_text(size = 16),
					legend.position="right",
					plot.title = element_text(size=18, face="bold", hjust=0.5))
		
				
		fixed <- summaryBy(pct_WL_mo + predict.fixed ~ months, data=newpred3, FUN=function(x)mean(x, na.rm=TRUE))
		colnames(fixed) <- c("Months","Actual","Predicted")
		fixed2 <- melt(fixed, id="Months") 

		graph2 <- 			
			ggplot(fixed2, aes(x=Months, y=value, colour=variable)) + 
				geom_line(size=2, aes(color=variable, group=variable)) +	
				geom_point(size=2, shape=21, fill="white") +
				xlab("Months") +	
				ylab("Percentage of Weight Loss") +
				ggtitle("Predictions for Validation") +	
				scale_x_continuous(breaks = seq(0, 12, 1)) +
				scale_y_continuous(breaks=seq(-5, 5, 1), limits=c(-4,2)) +
				theme(
					axis.text = element_text(size = 16),
					axis.title = element_text(size = 18),
					panel.grid.major = element_line(colour = "white"),
					panel.grid.minor = element_blank(),
					panel.background = element_rect(fill = "grey90"),
					legend.text = element_text(size = 16),
					legend.title = element_blank(),
					legend.position="bottom",
					plot.title = element_text(size=18, face="bold", hjust=0.5))

		png(paste0("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/images/longitudinal/",model,".png"), width=20, height=8, units='in', res=300)	
			grid.arrange(graph1, graph2, ncol=2)	
		dev.off()	
		
	 }	else{
		 print("no graph")}
				
	return(err.fixed)
}













## FUNCTION TO APPLY MODEL TO DEVELOPMENT, VALIDATION, AND MAY DATA ##
ApplyModel.Plot_all <- function(model, printimage="yes"){ 
	
		
	#apply the model to development
	pred_d <- as.data.frame(predict(eval(parse(text=model)), newdata=dev, level=0, returnData=TRUE))
	colnames(pred_d) <- "predict.fixed"	
	
	#combine data with prediction
	newpred_d <- cbind(dev,pred_d)	

	#remove missing actual values - pct_WL_mo
	newpred2_d <- newpred_d[which(is.na(newpred_d$pct_WL_mo)==0),] #doesn't do anything
		
	#calculate the difference between actual and predicted - using fixed effects
	newpred2_d$sig.fixed <- (newpred2_d$pct_WL_mo - newpred2_d$predict.fixed)^2	
		
	#create a RMSE value - fixed effects
	err.fixed_d <- sqrt(sum(newpred2_d$sig.fixed) / nrow(newpred2_d))

		
	#apply the model to validation
	pred <- as.data.frame(predict(eval(parse(text=model)), newdata=val, level=0, returnData=TRUE))
	colnames(pred) <- "predict.fixed"	
	
	#combine data with prediction
	newpred <- cbind(val,pred)	

	#remove missing actual values - pct_WL_mo
	newpred2 <- newpred[which(is.na(newpred$pct_WL_mo)==0),]
	
	#calculate the difference between actual and predicted - using fixed effects
	newpred2$sig.fixed <- (newpred2$pct_WL_mo - newpred2$predict.fixed)^2
			
	#create a RMSE value - fixed effects
	err.fixed <- sqrt(sum(newpred2$sig.fixed) / nrow(newpred2))
	
	
	
	#apply the model - only to the  month of may
	pred.may <- as.data.frame(predict(eval(parse(text=model)), newdata=dat_val_may[which(as.Date(dat_val_may$dates)=="2016-05-01"),], level=0, returnData=TRUE))
	colnames(pred.may) <- "predict.fixed"
	
		#combine data with prediction
	newpred.may <- cbind(dat_val_may[which(as.Date(dat_val_may$dates)=="2016-05-01"),],pred.may)

	#remove missing actual values - pct_WL_mo
	newpred.may2 <- newpred.may[which(is.na(newpred.may$pct_WL_mo)==0) ,]
			
	#calculate the difference between actual and predicted - using fixed effects
	newpred.may2$sig.fixed <- (newpred.may2$pct_WL_mo - newpred.may2$predict.fixed)^2	
			
	#create a RMSE value - fixed effects
	err.may.fixed <- sqrt(sum(newpred.may2$sig.fixed) / nrow(newpred.may2))
	
	

	if(printimage=="yes"){
	## DEVELOPMENT ##
		fixed_d <- summaryBy(pct_WL_mo + predict.fixed ~ months, data=newpred2_d, FUN=function(x)mean(x, na.rm=TRUE))
		colnames(fixed_d) <- c("Months","Actual","Predicted")
		fixed2_d <- melt(fixed_d, id="Months") 
		
		graph1 <- 
			ggplot(fixed2_d, aes(x=Months, y=value, colour=variable)) + 
				geom_line(size=2, aes(color=variable, group=variable)) +	
				geom_point(size=2, shape=21, fill="white") +
				xlab("Months") +	
				ylab("Percentage of Weight Loss") +
				ggtitle("Predictions for Development") +	
				scale_x_continuous(breaks = seq(0, 12, 1)) +
				scale_y_continuous(breaks=seq(-5, 5, 1), limits=c(-4,2)) +
				theme(
					axis.text = element_text(size = 16),
					axis.title = element_text(size = 18),
					panel.grid.major = element_line(colour = "white"),
					panel.grid.minor = element_blank(),
					panel.background = element_rect(fill = "grey90"),
					legend.text = element_text(size = 16),
					legend.title = element_blank(),
					legend.position="bottom",
					plot.title = element_text(size=18, face="bold", hjust=0.5))
		
		
		## VALIDATION ##		
		fixed <- summaryBy(pct_WL_mo + predict.fixed ~ months, data=newpred2, FUN=function(x)mean(x, na.rm=TRUE))
		colnames(fixed) <- c("Months","Actual","Predicted")
		fixed2 <- melt(fixed, id="Months") 

		graph2 <- 			
			ggplot(fixed2, aes(x=Months, y=value, colour=variable)) + 
				geom_line(size=2, aes(color=variable, group=variable)) +	
				geom_point(size=2, shape=21, fill="white") +
				xlab("Months") +	
				ylab("Percentage of Weight Loss") +
				ggtitle("Predictions for Validation") +	
				scale_x_continuous(breaks = seq(0, 12, 1)) +
				scale_y_continuous(breaks=seq(-5, 5, 1), limits=c(-4,2)) +
				theme(
					axis.text = element_text(size = 16),
					axis.title = element_text(size = 18),
					panel.grid.major = element_line(colour = "white"),
					panel.grid.minor = element_blank(),
					panel.background = element_rect(fill = "grey90"),
					legend.text = element_text(size = 16),
					legend.title = element_blank(),
					legend.position="bottom",
					plot.title = element_text(size=18, face="bold", hjust=0.5))

		
		## MAY ##		
		fixed.may <- summaryBy(pct_WL_mo + predict.fixed ~ months, data=newpred.may2, FUN=function(x)mean(x, na.rm=TRUE))
		colnames(fixed.may) <- c("Months","Actual","Predicted")
		fixed.may2 <- melt(fixed.may, id="Months") 

		graph3 <- 			
			ggplot(fixed.may2, aes(x=Months, y=value, colour=variable)) + 
				geom_line(size=2, aes(color=variable, group=variable)) +	
				geom_point(size=2, shape=21, fill="white") +
				xlab("Months") +	
				ylab("Percentage of Weight Loss") +
				ggtitle("Predictions for May") +	
				scale_x_continuous(breaks = seq(0, 12, 1)) +
				scale_y_continuous(breaks=seq(-5, 5, 1), limits=c(-4,2)) +
				theme(
					axis.text = element_text(size = 16),
					axis.title = element_text(size = 18),
					panel.grid.major = element_line(colour = "white"),
					panel.grid.minor = element_blank(),
					panel.background = element_rect(fill = "grey90"),
					legend.text = element_text(size = 16),
					legend.title = element_blank(),
					legend.position="bottom",
					plot.title = element_text(size=18, face="bold", hjust=0.5))
		
		#all six graphs together	
		png(paste0("C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/images/longitudinal/",model,"_all.png"), width=20, height=8, units='in', res=300)	
			grid.arrange(graph1, graph2, graph3, ncol=3)	
		dev.off()	
		
	 }	else{
		 print("no graph")}
		
	return(list(err.fixed_d,err.fixed,err.may.fixed))
}














































# random intercept
lme.int <- lme(fixed = pct_WL_mo ~ months, random = ~ 1 | id, data = dev, control=lmeControl(opt = "optim"))
summary(lme.int) # percentage of weight loss increases over time (more negative)
#accuracy of model
ApplyModel.Plot("lme.int")

# random intercept + random slope
lme.slp <- lme(fixed = pct_WL_mo ~ months, random = ~ months | id, data = dev, control=lmeControl(opt = "optim"))
summary(lme.slp) # percentage of weight loss increases over time (more negative)
#accuracy of model
ApplyModel.Plot("lme.slp")





































#natural log of months
lme.nl <- lme(fixed = pct_WL_mo ~ log(months), random = ~ log(months) | id, data = dev, control=lmeControl(opt = "optim"))
summary(lme.nl)
#accuracy of model
ApplyModel.Plot("lme.nl")





#### SPLINES ####

lme.sp <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)), random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp)
#accuracy of model
ApplyModel.Plot("lme.sp")

## look at each variable separately (with nl months) ##

lme.sp1 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + age, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp1)
#accuracy of model
ApplyModel.Plot("lme.sp1", printimage="no")


lme.sp2 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + first_weight, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp2)
#accuracy of model
ApplyModel.Plot("lme.sp2")


lme.sp2a <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) * first_weight, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp2a)
#accuracy of model
ApplyModel.Plot("lme.sp2a")


lme.sp3 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev)
summary(lme.sp3)
#accuracy of model
ApplyModel.Plot("lme.sp3")


lme.sp3a <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) * start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev)
summary(lme.sp3a)
#accuracy of model
ApplyModel.Plot("lme.sp3a")


lme.sp4 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + med_total, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp4)
#accuracy of model
ApplyModel.Plot("lme.sp4", printimage="no")


lme.sp5 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + med_indicator, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp5)
#accuracy of model
ApplyModel.Plot("lme.sp5", printimage="no")


lme.sp6 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_weight, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp6)
#accuracy of model
ApplyModel.Plot("lme.sp6", printimage="no")


lme.sp7 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_pct_WL_cum, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp7)
#accuracy of model
ApplyModel.Plot("lme.sp7")


lme.sp8 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_pct_WL_mo, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp8)
#accuracy of model
ApplyModel.Plot("lme.sp8", printimage="no")


lme.sp9 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_WL, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev,)
summary(lme.sp9)
#accuracy of model
ApplyModel.Plot("lme.sp9", printimage="no")


lme.sp10 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp10)
#accuracy of model
ApplyModel.Plot("lme.sp10", printimage="no")


lme.sp11 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_MonthlyRecordings, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp11)
#accuracy of model
ApplyModel.Plot("lme.sp11", printimage="no")


lme.sp12 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp12)
#accuracy of model
ApplyModel.Plot("lme.sp12")
ApplyModel.Plot_all("lme.sp12")


lme.sp12a <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12))*prev_note_count, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp12a)
#accuracy of model
ApplyModel.Plot("lme.sp12a")


lme.sp13 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_AllActivityN, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp13)
#accuracy of model
ApplyModel.Plot("lme.sp13", printimage="no")


lme.sp14 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_Food_Logged, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp14)
#accuracy of model
ApplyModel.Plot("lme.sp14", printimage="no")


lme.sp15 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count_total, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp15)
#accuracy of model
ApplyModel.Plot("lme.sp15", printimage="no")


lme.sp18 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + gender, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp18)
#accuracy of model
ApplyModel.Plot("lme.sp18", printimage="no")


lme.sp20 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_plan_grp, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp20)
#accuracy of model
ApplyModel.Plot("lme.sp20")


lme.sp21 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + marital_status2, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp21)
#accuracy of model
ApplyModel.Plot("lme.sp21", printimage="no")


lme.sp22 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + date_month, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp22)
#accuracy of model
ApplyModel.Plot("lme.sp22", printimage="no")


lme.sp23 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + date_season, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp23)
#accuracy of model
ApplyModel.Plot("lme.sp23", printimage="no")


##add variables
lme.sp24 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp24)
#accuracy of model
ApplyModel.Plot("lme.sp24")


##add variables
lme.sp25 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + date_season, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp25)
#accuracy of model
ApplyModel.Plot("lme.sp25")


lme.sp26 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + prev_MonthlyRecordings, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp26)
#accuracy of model
ApplyModel.Plot("lme.sp26")


lme.sp27 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp27)
#accuracy of model
ApplyModel.Plot("lme.sp27")
ApplyModel.Plot_all("lme.sp27")
Anova(lme.sp27)


lme.sp28 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + gender, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp28)
#accuracy of model
ApplyModel.Plot("lme.sp28")


lme.sp29 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) * start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp29)
#accuracy of model
ApplyModel.Plot("lme.sp29")


lme.sp30 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) * gender, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp30)
#accuracy of model
ApplyModel.Plot("lme.sp30")


lme.sp31 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + prev_MonthlyRecordings + start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp31)
#accuracy of model
ApplyModel.Plot("lme.sp31")
ApplyModel.Plot_all("lme.sp31")
Anova(lme.sp31)


lme.sp32 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_plan_grp + start_bmi + gender, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp32)
#accuracy of model
ApplyModel.Plot("lme.sp32")


lme.sp33 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_MonthlyRecordings + start_bmi + gender, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp33)
#accuracy of model
ApplyModel.Plot("lme.sp33")


lme.sp34 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp34)
#accuracy of model
ApplyModel.Plot("lme.sp34")
ApplyModel.Plot_all("lme.sp34")
Anova(lme.sp34)


lme.sp35 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_MonthlyRecordings + start_bmi, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp35)
#accuracy of model
ApplyModel.Plot("lme.sp35")
ApplyModel.Plot_all("lme.sp35")
Anova(lme.sp35)


lme.sp36 <- lme(fixed = pct_WL_mo ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)) + prev_note_count + prev_MonthlyRecordings, random = list(id = pdDiag(form= ~ bs(months, degree=2, knots=c(3,6), Boundary.knots=c(0,12)))), data = dev, control=lmeControl(opt = "optim"))
summary(lme.sp36)
#accuracy of model
ApplyModel.Plot("lme.sp36")
ApplyModel.Plot_all("lme.sp36")
Anova(lme.sp36)








vif(lme.sp31)
vif(lme.sp35)
vif(lme.sp34)







model1 <- lme.sp12
#save model
save(model1, file="model1.rda")



model2 <- lme.sp35
#save model
save(model2, file="model2.rda")



model3 <- lme.sp34
#save model
save(model3, file="model3.rda")





anova(model1, model3)
anova(model1, model2)
anova(model2, model3)

vif(model1)
vif(model2)
vif(model3)






















##### GRAPH WITH ALL MODELS IN IT FOR VALIDATION #####
	
#apply the models to validation
pred1 <- as.data.frame(predict(model1, newdata=val, level=0, returnData=TRUE))
colnames(pred1) <- "predict1"	

pred2 <- as.data.frame(predict(model3, newdata=val, level=0, returnData=TRUE)) #model3 is now model 2
colnames(pred2) <- "predict2"	

pred3 <- as.data.frame(predict(model2, newdata=val, level=0, returnData=TRUE))
colnames(pred3) <- "predict3"	


#combine data with prediction
newpred <- cbind(val,pred1,pred2,pred3)	

#remove missing actual values - pct_WL_mo
newpred2 <- newpred[which(is.na(newpred$pct_WL_mo)==0),]

fixed <- summaryBy(pct_WL_mo + predict1 + predict2 + predict3 ~ months, data=newpred2, FUN=function(x)mean(x, na.rm=TRUE))
colnames(fixed) <- c("Months","Actual","Model 1", "Model 2", "Model 3")
	
fixed2 <- melt(fixed, id="Months") 
	
png('C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/images/lme123.png',width=6, height=5, units='in', res=300)
	ggplot(fixed2, aes(x=Months, y=value, colour=variable)) + 
		geom_line(size=1.5, aes(color=variable, group=variable)) +	
		geom_point(size=1.5, shape=21, fill="white") +
		xlab("Months") +	
		ylab("Percentage of Weight Loss") +
		ggtitle("Predictions for Validation") +	
		scale_x_continuous(breaks = seq(0, 12, 1)) +
		scale_y_continuous(breaks=seq(-5, 5, 1), limits=c(-4,2)) +
		theme(
			axis.text = element_text(size = 16),
			axis.title = element_text(size = 18),
			panel.grid.major = element_line(colour = "white"),
			panel.grid.minor = element_blank(),
			panel.background = element_rect(fill = "grey90"),
			legend.text = element_text(size = 16),
			legend.title = element_blank(),
			legend.position="bottom",
			plot.title = element_text(size=18, face="bold", hjust=0.5))
dev.off()		
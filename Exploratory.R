setwd(...)

# load libraries
source("Libraries.R")


#########################
## WEIGHT MEASUREMENTS ##
#########################

device_weight <- read.csv("device_weight.csv.gz")

device_weight$weight_hour <- as.character(substr(device_weight$date_recorded, 12, 13))
device_weight$yearmo <- substr(device_weight$date_recorded, 1, 7)
device_weight$date_recorded <- substr(device_weight$date_recorded, 1, 10)

#remove duplicates and outliers
clean_weight <- sqldf("SELECT user_id, weight, yearmo, date_recorded FROM device_weight WHERE weight>100 AND weight<1000 AND user_id != 0")
clean_weight <-  clean_weight[!duplicated(clean_weight), ]

#remove duplicates and outliers and include hour of measurement - so no duplicates within an hour
clean_weight2 <- sqldf("SELECT user_id, weight, yearmo, date_recorded, weight_hour FROM device_weight WHERE weight>100 AND weight<1000 AND user_id != 0")
clean_weight2 <-  clean_weight2[!duplicated(clean_weight2), ]

#add a variable that indicates the number of days in the program for each measurements - remove daily duplicates - ONLY AFTER MAY 2014 MEASUREMENTS
clean_weight3 <- sqldf("SELECT a.user_id, a.weight, substr(a.date_recorded,1,10) AS date_recorded, substr(b.first_date,1,10) AS first_date, b.weight AS first_weight
						FROM device_weight a LEFT JOIN(SELECT user_id, weight, min(date_recorded) as first_date 
														FROM device_weight
														WHERE weight>100 AND weight<1000 AND user_id != 0 AND date_recorded>='2014-05-01'
														GROUP BY user_id) b 
											ON a.user_id=b.user_id
						WHERE a.weight>100 AND a.weight<1000 AND a.user_id != 0 AND date_recorded>='2014-05-01'")
clean_weight3 <-  clean_weight3[!duplicated(clean_weight3), ]
clean_weight3$date_recorded <- as.Date(clean_weight3$date_recorded)
clean_weight3$first_date <- as.Date(clean_weight3$first_date)
clean_weight3$days <- as.numeric( substr(clean_weight3$date_recorded - clean_weight3$first_date, 1, nchar(clean_weight3$date_recorded - clean_weight3$first_date)) )
clean_weight3$pct_WL_cum <- ((clean_weight3$weight - clean_weight3$first_weight) / clean_weight3$first_weight)*100
clean_weight4 <- clean_weight3[which(abs(clean_weight3$pct_WL_cum)<60),] #remove any outliers based on pct wl


## NUMBERS ##

#percentage of recordings between 150 and 250 pounds
nrow(clean_weight)
nrow(clean_weight[which(clean_weight$weight>=150 & clean_weight$weight<=250),])
round((nrow(clean_weight[which(clean_weight$weight>=150 & clean_weight$weight<=250),]) / nrow(clean_weight)) * 100)
# number of obs between 150 and 200 pounds
nrow(clean_weight[which(clean_weight$weight>=150 & clean_weight$weight<=200),]) 

#hourly recordings
table(clean_weight2[which(clean_weight2$date_recorded>='2014-05-01'),"weight_hour"])
nrow(clean_weight2)
(nrow(clean_weight2[which(clean_weight2$date_recorded>='2014-05-01' & clean_weight2$weight_hour=="06")),]) / nrow(clean_weight2)) * 100
(nrow(clean_weight2[which(clean_weight2$date_recorded>='2014-05-01' & clean_weight2$weight_hour %in% c("05","06","07","08","09")),]) / nrow(clean_weight2)) * 100


#histogram of all weight measurements -  between 100 and 1000
png('all_weight.png',width=6, height=4, units='in', res=300)
ggplot(data=clean_weight[which(clean_weight$date_recorded>='2014-05-01'),], aes(weight)) +
	geom_histogram(color="black", fill="blue", breaks = seq(100, 600, 50)) +
	xlab("Weight") +	
	ylab("Number of Measurements") +
	scale_x_continuous(breaks = seq(100, 600, 50)) +
	scale_y_continuous(breaks=seq(0,1000000,100000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()


#histogram of the hour of weight measurement - ALL MEASUREMENTS between 100 and 1000
png('time_weight.png',width=10, height=4, units='in', res=300)
ggplot(data=clean_weight2[which(clean_weight2$date_recorded>='2014-05-01'),], aes(weight_hour)) +
	geom_bar(color="black", fill="blue", aes(y=..count../sum(..count..))) +
	xlab("Hour") +	
	ylab("Proportion of Measurements") +
	scale_y_continuous(breaks=seq(0,1,.05)) +
	theme(axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()



#histogram of the month of weight measurements - starting in May 2014
png('date_weight.png',width=8, height=4, units='in', res=300)
ggplot(data=clean_weight[which(clean_weight$date_recorded>='2014-05-01'),], aes(yearmo)) +
	geom_bar(stat="count", color="black", fill="blue") +
	xlab("Date") +	
	ylab("Number of Measurements") +
	scale_y_continuous(breaks=seq(0,200000,10000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()


##scatterplot of weight measurements by days in program
#all measurements
ggplot(clean_weight4[which(clean_weight4$days>0),] , aes(x=days, y=weight)) +
	stat_binhex()
	
#the first year
ggplot(clean_weight4[which(clean_weight4$days>0 & clean_weight4$days<=365),] , aes(x=days, y=weight)) +
	stat_binhex(bins=50, geom="hex")
	
#first six months - weight
png('weight_meas_sixmo_days.png',width=8, height=6, units='in', res=300)
ggplot(clean_weight4[which(clean_weight4$days>0 & clean_weight4$days<=183),] , aes(x=days, y=weight)) +
	stat_binhex(bins=50, geom="hex") +
	scale_fill_gradientn(colours=c("lightskyblue","black"),name = "Frequency",na.value=NA) +
	xlab("Days in Program") +	
	ylab("Weight") +
	ggtitle("Weight Measurement by Days in Program \n for the first six months") +
	scale_x_continuous(breaks = seq(0, 183, 30)) +
	scale_y_continuous(breaks=seq(100,600,100)) +
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
dev.off()

#first six months - cum WL
nrow(clean_weight4[which(clean_weight4$days>0 & clean_weight4$days<=183),])
png('cumWL_meas_sixmo_days.png',width=8, height=6, units='in', res=300)
ggplot(clean_weight4[which(clean_weight4$days>0 & clean_weight4$days<=183),] , aes(x=days, y=pct_WL_cum)) +
	stat_binhex(bins=50, geom="hex") +
	scale_fill_gradientn(colours=c("plum","black"),name = "Frequency",na.value=NA) +
	xlab("Days in Program") +	
	ylab("Cumulative Weight Loss") +
	ggtitle("Cumulative Weight Loss by Days in Program \n for the first six months") +
	scale_x_continuous(breaks = seq(0, 183, 30)) +
	scale_y_continuous(breaks=seq(-60,60,15)) +
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right",
		plot.title = element_text(size=18, face="bold",hjust = 0.5))
dev.off()



## COACH MEETINGS ##

userinfo_notes <- read.csv("userinfo_notes.csv.gz")
notes01 <- sqldf("SELECT userinfo_id, created_by, for_date
						, date(for_date,'start of month') AS note_month
					FROM userinfo_notes
					WHERE is_deleted=0 AND is_alert=0 
							AND for_date >= '2014-05-01' AND for_date <= date('now')")
notes02 <- notes01[!duplicated(notes01),]
notes03 <- sqldf("	SELECT userinfo_id, note_month, count(*) AS note_count
					FROM notes02
					GROUP BY userinfo_id, note_month")	
notes03$note_month <- as.Date(notes03$note_month)


#histogram of monthly coach meetings - only if they have any
png('mo_meetings_distr.png',width=6, height=4, units='in', res=300)
ggplot(data=notes03, aes(note_count)) +
	geom_bar(color="black", fill="purple") +
	xlab("Monthly Coach Meetings") +	
	ylab("Number of Measurements") +
	scale_x_continuous(breaks = seq(1,10,1)) +
	scale_y_continuous(breaks=seq(0,150000,10000), label = comma) +
	theme(axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()




## HIP AND WAIST MEASUREMENTS ##

device_circ <- read.csv("device_circ.csv.gz")							
device_circ <- sqldf( "SELECT user_id, part, measurement, from_device
							, substr(date_recorded, 1, 10) AS date_recorded
							, date(date_recorded,'start of month') AS circ_month
						FROM device_circ
						WHERE date_recorded >= '2014-05-01' AND date_recorded <= date('now')")
	
device_circ01 <- device_circ[!duplicated(device_circ),]
	
#histogram of the month of weight measurements - starting in May 2014
png('date_circ.png',width=8, height=4, units='in', res=300)
ggplot(data=device_circ01, aes(substr(circ_month,1,7))) +
	geom_bar(stat="count", color="black", fill="green") +
	xlab("Date") +	
	ylab("Number of Measurements") +
	scale_y_continuous(breaks=seq(0,30000,2500), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()



#only waist measurements
waist <- device_circ[which(device_circ$part=="waist" ),c(1,3,5,6)] #& device_circ$measurement <= 100 & device_circ$measurement >= 20
#only hip measurements
hip <- device_circ[which(device_circ$part=="hip"),c(1,3,5,6)] # & device_circ$measurement <= 100 & device_circ$measurement >= 20
#remove duplicates 
waist01 <- waist[!duplicated(waist),]
hip01 <- hip[!duplicated(hip),]


#histogram of all waist measurements 
png('all_waist.png',width=7, height=4, units='in', res=300)
ggplot(data=waist01, aes(measurement)) +
	geom_histogram(color="black", fill="green", breaks = seq(0, 100, 5)) +
	xlab("Measurement") +	
	ylab("Number of Measurements") +
	scale_x_continuous(breaks = seq(0, 100, 5)) +
	scale_y_continuous(breaks=seq(0,20000,2500), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#histogram of all hip measurements 
png('all_hip.png',width=7, height=4, units='in', res=300)
ggplot(data=hip01, aes(measurement)) +
	geom_histogram(color="black", fill="green", breaks = seq(0, 100, 5)) +
	xlab("Measurement") +	
	ylab("Number of Measurements") +
	scale_x_continuous(breaks = seq(0, 100, 5)) +
	scale_y_continuous(breaks=seq(0,30000,2500), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()




## ACTIVITY ##

activity <- read.csv("activity.csv.gz")
activity_intensity <- read.csv("activity_intensity.csv")
activity_type <- read.csv("activity_intensity.csv")

activity01 <- sqldf("	SELECT a.user_id
								, substr(a.date_time, 1, 10) AS date								
								, date(a.date_time,'start of month') AS activity_month
								, a.duration
								, b.name AS activity_intensity
								, c.name AS activity_type
								, (c.name||'_'||b.name) as activity_group 
							FROM activity a LEFT JOIN activity_intensity b ON a.activity_intensity_id=b.id
											LEFT JOIN activity_type c ON a.activity_type_id=c.id
							WHERE a.is_deleted==0 AND date >= '2014-05-01' AND date <= date('now')" )
activity02 <- activity01[!duplicated(activity01),]

table(activity02$activity_intensity)
table(activity02$activity_type)
table(activity02$activity_group)
round((table(activity02$activity_type, activity02$activity_intensity) / nrow(activity02)) *100)



#how many members have  activity
act01 <- summaryBy(AllActivityN ~ id, data=data09, FUN=sum)
head(act01)
table(act01$AllActivityN.sum)
1 - (nrow(act01[which(act01$AllActivityN.sum==0),]) / nrow(act01))
	
#histogram of the month of activities recorded - starting in May 2014
png('date_activity.png',width=8, height=4, units='in', res=300)
ggplot(data=activity02, aes(substr(activity_month,1,7))) +
	geom_bar(stat="count", color="black", fill="orange") +
	xlab("Date") +	
	ylab("Number of Activities") +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()




## total minutes of activity
activity03 <- cast(activity02, user_id + activity_month ~ activity_intensity, value="duration", c(sum,length), margins="grand_col")
colnames(activity03) <- c("user_id", "activity_month", "ExtraActive.TotalMinutes", "ExtraActive.N", "LightActivity.TotalMinutes", "LightActivity.N"
									, "Moderate.TotalMinutes", "Moderate.N", "Sedentary.TotalMinutes", "Sedentary.N", "VeryActive.TotalMinutes", "VeryActive.N"
									, "AllActivityTotalMinutes", "AllActivityN")
						

#percent of activities that are more than 1000 minutes per month
(nrow(activity03[which(activity03$AllActivityTotalMinutes>1000),]) / nrow(activity03)) *100
(1-(nrow(activity03[which(activity03$AllActivityTotalMinutes>1000),]) / nrow(activity03))) *100
#total monthly activity minutes per month - truncated at 1000 minutes
png('activity_month_totalminutes.png',width=8, height=4, units='in', res=300)
ggplot(data=activity03, aes(AllActivityTotalMinutes)) +
	geom_histogram(color="black", fill="orange", breaks = seq(0, 1000, 60)) +
	xlab("Minutes of Monthly Activity") +	
	ylab("Count") +
	scale_x_continuous(breaks = seq(0, 1000, 60)) +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()


#total number of activities per month
png('activity_month_total.png',width=8, height=4, units='in', res=300)
ggplot(data=activity03, aes(AllActivityN)) +
	geom_histogram(color="black", fill="orange", breaks = seq(0, 100, 5)) +
	xlab("Number of Monthly Activities") +	
	ylab("Count") +
	scale_x_continuous(breaks = seq(0, 100, 5)) +
	scale_y_continuous(breaks=seq(0,100000,2000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#histogram of the duration of each activity
act02 <- summaryBy(user_id ~ duration, data=activity02, FUN=length)
png('activity_duration.png',width=8, height=4, units='in', res=300)
ggplot(data=act02, aes(x=duration, y=user_id.length)) +
	geom_bar(stat="identity", color="black", fill="orange") +
	xlab("Duration") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,25000,2000), label = comma) +
	scale_x_continuous(breaks=seq(5,60,5)) +
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#percentage
(act02[which(act02$duration==30),"user_id.length"] / sum(act02[,"user_id.length"])) *100
(sum(act02[which(act02$duration<=30),"user_id.length"]) / sum(act02[,"user_id.length"])) *100








## FOOD ##

food <- read.csv("food_tag_log.csv.gz")
food01 <- food[!duplicated(food),] #no duplicates
food02 <- food01[,1:5]
food02$month <- as.Date(paste0(substr(as.Date(food02$date),1,7),'-01'), format = "%Y-%m-%d")

#histogram of the month of food items logged - starting in May 2014
png('date_food.png',width=8, height=4, units='in', res=300)
ggplot(data=food02[which(food02$month>='2014-05-01'),], aes(substr(month,1,7))) +
	geom_bar(stat="count", color="black", fill="hotpink") +
	xlab("Date") +	
	ylab("Number of Food Items") +
	scale_y_continuous(breaks=seq(0,200000,25000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#monthly food logged
food03 <- summaryBy(date ~ user_id + month, data=food02, FUN=c(length))
colnames(food03)[3] <- "Food_Logged"


#daily food logged since May 1, 2014
food04 <- summaryBy(date ~ user_id + date, data=food02[which(food02$month>='2014-05-01'),], FUN=c(length))
colnames(food04)[3] <- "Food_Logged_daily"

food05 <- summaryBy(user_id ~ Food_Logged_daily, data=food04, FUN=length)

#histogram of the daily food items logged - starting in May 2014
png('food_daily.png',width=8, height=4, units='in', res=300)
ggplot(data=food05, aes(x=Food_Logged_daily, y=user_id.length)) +
	geom_bar(stat="identity", color="black", fill="hotpink") +
	xlab("Daily Food Items Logged") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,80000,10000), label = comma) +
	scale_x_continuous(breaks=seq(1,40,2)) +
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		#panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()






## MEDICATION ##

meds <- read.csv("user_medications.csv.gz")
meds01 <- meds

meds01$med_blood_pressure <- ifelse( grepl("blood",meds01$medication, ignore.case=TRUE) & grepl("pressure",meds01$medication, ignore.case=TRUE) | grepl("hypertension",meds01$medication, ignore.case=TRUE) | grepl("lisinopril",meds01$medication, ignore.case=TRUE) | grepl("bp",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_antidepressant <- ifelse( grepl("depress",meds01$medication, ignore.case=TRUE) | grepl("anxi",meds01$medication, ignore.case=TRUE) | grepl("zoloft",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_cholesterol <- ifelse( grepl("cholesterol",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_sleep <- ifelse( grepl("sleep",meds01$medication, ignore.case=TRUE) | grepl("insomnia",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_diabetes <- ifelse( grepl("diabet",meds01$medication, ignore.case=TRUE) | grepl("metformin",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_thyroid <- ifelse( grepl("thyroid",meds01$medication, ignore.case=TRUE) | grepl("synthroid",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_acid_reflux <- ifelse( grepl("reflux",meds01$medication, ignore.case=TRUE) | grepl("heartburn",meds01$medication, ignore.case=TRUE) | grepl("indigest",meds01$medication, ignore.case=TRUE) | grepl("gerd",meds01$medication, ignore.case=TRUE) | grepl("acid",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_vitamin <- ifelse( grepl("vitamin",meds01$medication, ignore.case=TRUE) | grepl("vit",meds01$medication, ignore.case=TRUE) | grepl("calcium",meds01$medication, ignore.case=TRUE) | grepl("fish",meds01$medication, ignore.case=TRUE), 1, 0)
meds01$med_diuretics <- ifelse( grepl("diuretics",meds01$medication, ignore.case=TRUE) ,1, 0)
meds01$med_allergies <- ifelse( grepl("allergies",meds01$medication, ignore.case=TRUE) ,1, 0)
meds01$med_birthcontrol <- ifelse( grepl("birth",meds01$medication, ignore.case=TRUE) ,1, 0)
meds01$med_asthma <- ifelse( grepl("asthma",meds01$medication, ignore.case=TRUE) ,1, 0)
meds01$med_aspirin <- ifelse( grepl("aspirin",meds01$medication, ignore.case=TRUE) ,1, 0)
meds01$med_bloodthinner <- ifelse( grepl("coumadin",meds01$medication, ignore.case=TRUE) | grepl("warfarin",meds01$medication, ignore.case=TRUE),1, 0)

meds02 <- summaryBy(med_blood_pressure + med_antidepressant + med_cholesterol + med_sleep + med_diabetes + med_thyroid + med_acid_reflux + med_vitamin + med_diuretics + med_allergies + med_birthcontrol + med_asthma + med_aspirin + med_bloodthinner ~ user_id, data=meds01, FUN=c(sum))
colnames(meds02)[2:15] <- c("med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner")

							
tmp <- as.data.frame(colSums(meds02[,2:15]))
tmp$Medication <- c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners")						
colnames(tmp)[1] <- "N"				
tmp <- tmp[order(-tmp$N),]	
rownames(tmp) <- NULL		



#histogram of medication use
png('medication_cnt.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp, aes(x=Medication, y=N)) +
	geom_bar(stat="identity", color="black", fill="turquoise2") +
	xlab("Medications") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,20000,2500), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()



							
							
## MEAL PLANS ##

plans <- read.csv("plans.csv.gz")		

plans01 <- plans[order(plans$user_id, plans$id),]
plans01$date_started <- as.Date(plans01$date_started, format="%Y-%m-%d")
plans01$date_ended <- as.Date(plans01$date_ended, format="%Y-%m-%d")

plans02 <- slide(data=plans01, Var="date_started", slideBy=1, NewVar="lag_date_started", GroupVar="user_id")
plans02$date_ended <-  as.Date( ifelse( is.na(plans02$date_ended) & !is.na(plans02$lag_date_started), plans02$lag_date_started,
									ifelse( is.na(plans02$date_ended) & is.na(plans02$lag_date_started), as.Date(max(as.Date(plans02$date_started),na.rm=TRUE) +1), plans02$date_ended)),  origin = "1970-01-01")
plans02$date_started <- as.character(as.Date(as.character(plans02$date_started), format = "%Y-%m-%d"))
plans02$current_plan <- ifelse(plans02$date_ended>=as.Date(max(as.Date(plans02$date_started),na.rm=TRUE) +1),1,0)

plans03 <- sqldf("SELECT user_id, template_name, calories, date_started, date_ended
						, date(date_started,'start of month') AS month_started
						, date(date_started,'start of month','+1 month','-1 day') AS month_started1 
						, current_plan
					FROM plans02
					WHERE user_id != 0 AND is_template==0 AND template_name IS NOT NULL AND date(date_started,'start of month') >= '2014-05-01'")
plans03$plan_grp <- ifelse( grepl("Teen",plans03$template_name, ignore.case=TRUE), "Teen", 
						ifelse( grepl("Sustain",plans03$template_name, ignore.case=TRUE), "Sustain",
							ifelse( grepl("Jump",plans03$template_name, ignore.case=TRUE), "Jump Start", 
								ifelse( grepl("Mom Protocol",plans03$template_name, ignore.case=TRUE), "Mom Protocol", 
									ifelse( grepl("Reboot Adapt",plans03$template_name, ignore.case=TRUE), "Reboot Adapt",
										ifelse( grepl("Reboot Reduce",plans03$template_name, ignore.case=TRUE), "Reboot Reduce",
											ifelse( grepl("Balance",plans03$template_name, ignore.case=TRUE), "Balance", "Other"																	
													)
																		
												)											
											)												
										)
									)									 
								) 
							)
plans03$date_started <- as.Date(plans03$date_started)
plans03$plan_days <- substr((plans03$date_ended - plans03$date_started), 1, nchar(plans03$date_ended - plans03$date_started))
plans03$plan_days <- as.numeric(plans03$plan_days)

plans04 <- plans03[which(plans03$plan_days > 0),]

plans05 <- plans04[!duplicated(plans04),]
plans05$month_started <- as.Date(plans05$month_started)   #first day of the month
plans05$month_started1 <- as.Date(plans05$month_started1) #last day of the month

dates <- as.data.frame(seq(from=as.Date('2014-05-01'), to=as.Date(as.yearmon(seq(as.Date(Sys.Date()), by="-1 months", length=2)[2])), by="month"), optional=TRUE)
colnames(dates)<-"dates"
dates$dates <- as.character(as.Date(dates$dates, format="%Y-%m-%d"))
dates2 <- sqldf("SELECT dates, date(dates,'start of month','+1 month','-1 day') AS dates_end 
					FROM dates")
dates2$dates <- as.Date(dates2$dates)         #current month
dates2$dates_end <- as.Date(dates2$dates_end) #last day of the current month

#every plan has an associated month to it
plans06 <- sqldf("SELECT a.*, b.dates, b.dates_end
					FROM plans05 a, dates2 b
					WHERE a.month_started<=b.dates AND a.date_ended>=b.dates")
plans06$diff <- ifelse( plans06$date_started>plans06$dates, plans06$month_started1-plans06$date_started, 
					ifelse( plans06$dates_end<plans06$date_ended, plans06$dates_end-plans06$dates, plans06$date_ended-plans06$dates))

plans07 <- plans06[,c(1:9,11,13)]
plans07$dates <- as.character(plans07$dates)

plans08 <- sqldf("SELECT b.*
				FROM (SELECT user_id, dates, max(diff) AS maxdiff
					FROM plans07
					GROUP BY user_id, dates) a LEFT JOIN plans07 b ON a.user_id=b.user_id AND a.dates=b.dates AND a.maxdiff=b.diff")

plans09 <- rbind(plans08, plans07[which(plans07$current_plan==1),])
plans09 <- plans09[order(plans09$user_id, plans09$dates, -plans09$current_plan),]

plans10 <- plans09[!duplicated(plans09[, c("user_id","dates")]),] 
plans10$dates <- as.Date(plans10$dates)

##

##remove monthly plans to determine the length of time in each meal plan
#create new groups
levels(plans10$template_name)
plans10$plan_grp2 <- ifelse( grepl("Teen Balance Adapt",plans10$template_name, ignore.case=TRUE), "Teen Balance Adapt", 
						ifelse( grepl("Teen Balance Reduce",plans10$template_name, ignore.case=TRUE), "Teen Balance Reduce", 
						ifelse( grepl("Teen Sustain",plans10$template_name, ignore.case=TRUE), "Teen Sustain", 
						ifelse( grepl("Teen Recharge",plans10$template_name, ignore.case=TRUE), "Teen Recharge", 						
						ifelse( grepl("Sustain",plans10$template_name, ignore.case=TRUE), "Sustain",
						ifelse( grepl("Reboot Sustain",plans10$template_name, ignore.case=TRUE), "Reboot Sustain",						
						ifelse( grepl("Reboot Reduce",plans10$template_name, ignore.case=TRUE), "Reboot Reduce",
						ifelse( grepl("Reboot Adapt",plans10$template_name, ignore.case=TRUE), "Reboot Adapt",
						ifelse( grepl("Performance",plans10$template_name, ignore.case=TRUE), "Performance",
						ifelse( grepl("NuStart",plans10$template_name, ignore.case=TRUE), "NuStart",
						ifelse( grepl("Mom Protocol",plans10$template_name, ignore.case=TRUE), "Mom Protocol",
						ifelse( grepl("Jump",plans10$template_name, ignore.case=TRUE), "Jump",
						ifelse( grepl("Balance",plans10$template_name, ignore.case=TRUE), "Balance",									
						ifelse( grepl("Research Protocol",plans10$template_name, ignore.case=TRUE), "Research Protocol",									
						ifelse( grepl("Empty Template",plans10$template_name, ignore.case=TRUE), "Not Specified", "Other"											
						)))))))))))))))


#select columns
plans11a <- plans10[,c("user_id","plan_grp2","date_started","date_ended")]
#remove duplicates
plans12a <- plans11a[!duplicated(plans11a),] 
#add number of days in plan
plans12a$days <- plans12a$date_ended-plans12a$date_started
#summary
plans13a <- summaryBy(days ~ plan_grp2, plans12a, FUN=c(length,mean,min,max))
plans13a <- plans13a[order(-plans13a$days.mean),]
plans13a



png('mealplans_num.png',width=6, height=4, units='in', res=300)
ggplot(plans13a, aes(x=plan_grp2, y=days.length)) + 
	geom_bar(stat="identity", color="black", fill="seagreen2") + 	
	xlab("Meal Plan Group") +	
	ylab("Number of Plans") +
	scale_y_continuous(breaks=seq(0,50000,10000), label=comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
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

png('mealplans_days.png',width=6, height=4, units='in', res=300)
ggplot(plans13a, aes(x=plan_grp2, y=days.mean)) + 
	geom_bar(stat="identity", color="black", fill="seagreen2") + 	
	xlab("Meal Plan Group") +	
	ylab("Avg Days in Plan") +
	scale_y_continuous(breaks=seq(0,300,50), label=comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
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



















#######################
#### COMBINED DATA ####
#######################

## ONE ROW PER PERSON PER MONTH (FROM THEIR STARTING DATE)

data08 <- read.csv("data08.csv.gz")
data09 <- read.csv("data09.csv.gz") #data08 but removes some of the outliers and their full account

#######################
#######################
#######################



#distribution of the number of monthly coach meetings by the month in program
#stacked graph
tab <- data09[,c('id','dates','months','note_count')]
tab$meet_cat <- ifelse(tab$note_count>=5, '5+', tab$note_count)
meet <- as.data.frame(matrix(nrow=12,ncol=7))
for(i in 1:12){
	meet[i,1] <- i 
	meet[i,2:7] <- t( with(tab[which(tab$months==i),], matrix( round((table(meet_cat) / nrow(tab[which(tab$months==i),])) *100,0), ncol=1, dimnames=list(sort(unique(tab$meet_cat)), c("percent")))))
}
colnames(meet) <- c("Month","0","1","2","3","4","5+")
#long dataset
meet1 <- melt(meet, id="Month")
colnames(meet1)[c(2,3)] <- c("Meetings","Distribution")

#create graph
png('meetings_mo_distr_stacked.png',width=6, height=4, units='in', res=300)
ggplot(meet1, aes(x=Month, y=Distribution, fill=Meetings)) + 
	geom_bar(stat="identity") +
	coord_cartesian(xlim=c(1:12)) +  
	scale_x_continuous(breaks=seq(1, 12, 1)) + 	
	guides(fill = guide_legend(reverse=TRUE)) +
	scale_color_discrete(name="Meetings") +
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




#histogram of starting age
round((nrow(data09[which(data09$months==0 & (data09$age>=30 & data09$age<=60)),]) / nrow(data09[which(data09$months==0),])) *100, 1)

png('age_distr.png',width=6, height=4, units='in', res=300)
ggplot(data=data09[which(data09$months==0),], aes(age)) +
	geom_histogram(color="black", fill="red", aes(y=..count../sum(..count..)), breaks = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90)) +
	xlab("Starting Age") +	
	ylab("Proportion of Measurements") +
	scale_x_continuous(breaks = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90)) +
	scale_y_continuous(breaks=seq(0,1,0.05)) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()


#histogram of marital status
table(data09[which(data09$months==0),"marital_status"]) / nrow(data09[which(data09$months==0),])

png('marital_distr.png',width=6, height=4, units='in', res=300)
ggplot(data=data09[which(data09$months==0),], aes(marital_status)) +
	geom_bar(color="black", fill="red", aes(y=..count../sum(..count..))) +
	xlab("Marital Status") +	
	ylab("Proportion of Measurements") +
	scale_y_continuous(breaks=seq(0,1,0.05)) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()


# ##pie chart of marital status
marry <- data.frame(table(data09[which(data09$months==0),"marital_status"]))
marry1 <- marry[2:6,]
cols <- c("orange2","chartreuse3","royalblue2","darkorchid3","red2")
percentlabels <- round(100*(marry1[,2]/sum(marry1[,2])),1)
pielabels <- paste(percentlabels, "%", sep="")
png('marital_distr_pie.png',width=4.5, height=4, units='in', res=300)
par(mar=c(0,0,1,1)+0.1) #b,l,t,r
	pie(marry1[,2], 
			labels=pielabels,
			col=cols,
			cex=0.8)
	legend("topright", c("Divorced","Married","Partner","Single","Widowed"), cex=0.8, fill=cols)		
dev.off()

##distr of marital status by gender
mg1 <- data09[which(data09$months==0 & data09$marital_status != "" & data09$gender != ""),]
png('marital_gender_distr.png',width=6, height=4, units='in', res=300)
ggplot(data=mg1, aes(marital_status, fill=gender)) +
	geom_bar(color="black") +
	xlab("Marital Status") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,20000,5000), labels=comma) +
	scale_fill_manual(name="Gender", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#two pie charge one for male one for female - marital distribution
marry2 <- data.frame(table(data09[which(data09$months==0 & data09$marital_status != "" & data09$gender != ""),c("gender","marital_status")]))
marry2m <- marry2[which(marry2$gender=="male"),c("marital_status","Freq")]
marry2m <- marry2m[2:6,]
marry2f <- marry2[which(marry2$gender=="female"),c("marital_status","Freq")] 
marry2f <- marry2f[2:6,]

cols <- c("orange2","chartreuse3","royalblue2","darkorchid3","red2")
percentlabels_m <- round(100*(marry2m[,2]/sum(marry2m[,2])),1)
pielabels_m <- paste(percentlabels_m, "%", sep="")
percentlabels_f <- round(100*(marry2f[,2]/sum(marry2f[,2])),1)
pielabels_f <- paste(percentlabels_f, "%", sep="")

png('marital_gender_distr_pie.png',width=8, height=4, units='in', res=300)
par(mfrow=c(1,2),mar=c(0,0,2.5,0)+0.1)
	pie(marry2m[,2], 
			labels=pielabels_m,
			col=cols,
			cex=0.8,
			main="Males")			
	pie(marry2f[,2], 
			labels=pielabels_f,
			col=cols,
			cex=0.8,
			main="Females")
	legend(0.55,1.15, c("Divorced","Married","Partner","Single","Widowed"), cex=0.8, fill=cols, bty = "n")	
dev.off()




#histogram of starting bmi
data09$bmi_cat <- with(data09,
			ifelse(bmi >= 40, "e_obeseIII", 
				ifelse(bmi >= 35, "d_obeseII",
					ifelse(bmi >= 30, "c_obeseI",
							ifelse(bmi >= 25 & bmi < 30, "b_overweight", "a_healthy_under")))))
png('start_bmi_distr.png',width=5, height=6, units='in', res=300)
ggplot(data=data09[which(data09$months==0),], aes(bmi_cat)) +
	geom_bar(color="black", fill="red", aes(y=..count../sum(..count..))) +
	xlab("Starting BMI Category") +	
	ylab("Proportion of Measurements") +
	scale_y_continuous(breaks=seq(0,1,0.05)) +
	scale_x_discrete(labels = c("a_healthy_under"="Healthy/Underweight", "b_overweight"="Overweight", "c_obeseI"="Obese I", "d_obeseII"="Obese II", "e_obeseIII"="Obese III")) + 
	theme(axis.text.x = element_text(angle = 55, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()



#sex
tmp <- as.data.frame(round((table(data09[which(data09$months==0),"gender"]) / nrow(data09[which(data09$months==0),])) , 4))
tmp$label <- c("Missing","Female","Male")
tmp

png('gender_distr.png',width=4, height=5, units='in', res=300)
ggplot(data=tmp, aes(y=Freq, x=label)) +
	geom_bar(stat="identity", color="black", fill="red") +
	xlab("Sex") +	
	ylab("Proportion") +
	scale_y_continuous(breaks=seq(0,1,0.1)) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()



## Average cumulative weight loss over time
time_WL <- summaryBy(pct_WL_cum ~ months, data = data09[which(data09$months>0),], FUN=c(length, function(x)mean(x, na.rm=TRUE)))
colnames(time_WL)[c(2,3)] <- c("N","pct_WL_cum")
time_WL <- rbind(data.frame(months=0,N=0,pct_WL_cum=0),time_WL)

png('time_WL.png',width=6, height=5, units='in', res=300)
ggplot(time_WL[which(time_WL$months<=12),], aes(x=months, y=pct_WL_cum)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 24, 1)) +
	scale_y_continuous(breaks = c(-13:0), limits=c(-13,0))+
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90")
	)	   
dev.off()


















## More Graphs ##


## obese distribution between gender over time - stacked graph
data09$bmi_cat <- with(data09,
					ifelse(bmi >= 30, "Obese",
						ifelse(bmi >= 25 & bmi < 30, "Overweight", 
							ifelse(bmi >= 18.5 & bmi < 25, "Healthy", "Underweight"))))
							
tmp1 <- data09[which(data09$bmi_cat=="Obese"),c('id','months','gender')]							
tmp2 <- as.data.frame(matrix(nrow=12,ncol=4))
for(i in 1:12){
	tmp2[i,1] <- i 
	tmp2[i,2:4] <- t( with(tmp1[which(tmp1$months==i),], matrix( round((table(gender) / nrow(tmp1[which(tmp1$months==i),])) *100,0), ncol=1, dimnames=list(sort(unique(tmp1$gender)), c("percent")))))
}
colnames(tmp2) <- c("Month","Missing","Female","Male")
tmp3 <- tmp2[,c("Month","Female","Male")]
#long dataset
tmp4 <- melt(tmp3, id="Month")
colnames(tmp4)[c(2,3)] <- c("Gender","Distribution")

#create graph
png('obese_gender_mo_distr_stacked.png',width=6, height=4, units='in', res=300)
ggplot(tmp4, aes(x=Month, y=Distribution, fill=Gender)) + 
	geom_bar(stat="identity") +
	coord_cartesian(xlim=c(1:12)) +  
	scale_x_continuous(breaks=seq(1, 12, 1)) + 	
	guides(fill = guide_legend(reverse=TRUE)) +
	scale_color_discrete(name="Gender") +
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

## healthy distribution between gender over time - stacked graph - No changes						
tmp1 <- data09[which(data09$bmi_cat=="Healthy"),c('id','months','gender')]							
tmp2 <- as.data.frame(matrix(nrow=12,ncol=4))
for(i in 1:12){
	tmp2[i,1] <- i 
	tmp2[i,2:4] <- t( with(tmp1[which(tmp1$months==i),], matrix( round((table(gender) / nrow(tmp1[which(tmp1$months==i),])) *100,0), ncol=1, dimnames=list(sort(unique(tmp1$gender)), c("percent")))))
}
colnames(tmp2) <- c("Month","Missing","Female","Male")

## overweight distribution between gender over time - stacked graph	- No changes				
tmp1 <- data09[which(data09$bmi_cat=="Overweight"),c('id','months','gender')]							
tmp2 <- as.data.frame(matrix(nrow=12,ncol=4))
for(i in 1:12){
	tmp2[i,1] <- i 
	tmp2[i,2:4] <- t( with(tmp1[which(tmp1$months==i),], matrix( round((table(gender) / nrow(tmp1[which(tmp1$months==i),])) *100,0), ncol=1, dimnames=list(sort(unique(tmp1$gender)), c("percent")))))
}
colnames(tmp2) <- c("Month","Missing","Female","Male")




















## MEDICATIONS AND WEIGHT LOSS OVER TIME ##

## blood pressure
BP_time_WL <- summaryBy(pct_WL_cum ~ med_blood_pressure + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(BP_time_WL)[3] <- "pct_WL_cum"
BP_time_WL <- rbind(data.frame(med_blood_pressure=0,months=0,pct_WL_cum=0),BP_time_WL)
BP_time_WL <- rbind(data.frame(med_blood_pressure=1,months=0,pct_WL_cum=0),BP_time_WL)
BP_time_WL <- BP_time_WL[order(BP_time_WL$med_blood_pressure, BP_time_WL$months),]
BP_time_WL$med_blood_pressure <- as.factor(BP_time_WL$med_blood_pressure)

png('BP_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(BP_time_WL, aes(x=months, y=pct_WL_cum, color=med_blood_pressure)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On blood\npressure\nmedication", labels=c("No","Yes")) +
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

## antidepressant
depress_time_WL <- summaryBy(pct_WL_cum ~ med_antidepressant + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(depress_time_WL)[3] <- "pct_WL_cum"
depress_time_WL <- rbind(data.frame(med_antidepressant=0,months=0,pct_WL_cum=0),depress_time_WL)
depress_time_WL <- rbind(data.frame(med_antidepressant=1,months=0,pct_WL_cum=0),depress_time_WL)
depress_time_WL <- depress_time_WL[order(depress_time_WL$med_antidepressant, depress_time_WL$months),]
depress_time_WL$med_antidepressant <- as.factor(depress_time_WL$med_antidepressant)

png('depress_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(depress_time_WL, aes(x=months, y=pct_WL_cum, color=med_antidepressant)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On anti-\ndepressant\nmedication", labels=c("No","Yes")) +
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

## cholesterol
chol_time_WL <- summaryBy(pct_WL_cum ~ med_cholesterol + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(chol_time_WL)[3] <- "pct_WL_cum"
chol_time_WL <- rbind(data.frame(med_cholesterol=0,months=0,pct_WL_cum=0),chol_time_WL)
chol_time_WL <- rbind(data.frame(med_cholesterol=1,months=0,pct_WL_cum=0),chol_time_WL)
chol_time_WL <- chol_time_WL[order(chol_time_WL$med_cholesterol, chol_time_WL$months),]
chol_time_WL$med_cholesterol <- as.factor(chol_time_WL$med_cholesterol)

png('chol_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(chol_time_WL, aes(x=months, y=pct_WL_cum, color=med_cholesterol)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On chol-\nesterol\nmedication", labels=c("No","Yes")) +
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

## sleep
sleep_time_WL <- summaryBy(pct_WL_cum ~ med_sleep + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(sleep_time_WL)[3] <- "pct_WL_cum"
sleep_time_WL <- rbind(data.frame(med_sleep=0,months=0,pct_WL_cum=0),sleep_time_WL)
sleep_time_WL <- rbind(data.frame(med_sleep=1,months=0,pct_WL_cum=0),sleep_time_WL)
sleep_time_WL <- sleep_time_WL[order(sleep_time_WL$med_sleep, sleep_time_WL$months),]
sleep_time_WL$med_sleep <- as.factor(sleep_time_WL$med_sleep)

png('sleep_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(sleep_time_WL, aes(x=months, y=pct_WL_cum, color=med_sleep)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On sleep\nmedication", labels=c("No","Yes")) +
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

## diabetes
diab_time_WL <- summaryBy(pct_WL_cum ~ med_diabetes + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(diab_time_WL)[3] <- "pct_WL_cum"
diab_time_WL <- rbind(data.frame(med_diabetes=0,months=0,pct_WL_cum=0),diab_time_WL)
diab_time_WL <- rbind(data.frame(med_diabetes=1,months=0,pct_WL_cum=0),diab_time_WL)
diab_time_WL <- diab_time_WL[order(diab_time_WL$med_diabetes, diab_time_WL$months),]
diab_time_WL$med_diabetes <- as.factor(diab_time_WL$med_diabetes)

png('diab_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(diab_time_WL, aes(x=months, y=pct_WL_cum, color=med_diabetes)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On diabetes\nmedication", labels=c("No","Yes")) +
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

## thyroid
thyr_time_WL <- summaryBy(pct_WL_cum ~ med_thyroid + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(thyr_time_WL)[3] <- "pct_WL_cum"
thyr_time_WL <- rbind(data.frame(med_thyroid=0,months=0,pct_WL_cum=0),thyr_time_WL)
thyr_time_WL <- rbind(data.frame(med_thyroid=1,months=0,pct_WL_cum=0),thyr_time_WL)
thyr_time_WL <- thyr_time_WL[order(thyr_time_WL$med_thyroid, thyr_time_WL$months),]
thyr_time_WL$med_thyroid <- as.factor(thyr_time_WL$med_thyroid)

png('thyr_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(thyr_time_WL, aes(x=months, y=pct_WL_cum, color=med_thyroid)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On thyroid\nmedication", labels=c("No","Yes")) +
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

## acid reflux
acid_time_WL <- summaryBy(pct_WL_cum ~ med_acid_reflux + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(acid_time_WL)[3] <- "pct_WL_cum"
acid_time_WL <- rbind(data.frame(med_acid_reflux=0,months=0,pct_WL_cum=0),acid_time_WL)
acid_time_WL <- rbind(data.frame(med_acid_reflux=1,months=0,pct_WL_cum=0),acid_time_WL)
acid_time_WL <- acid_time_WL[order(acid_time_WL$med_acid_reflux, acid_time_WL$months),]
acid_time_WL$med_acid_reflux <- as.factor(acid_time_WL$med_acid_reflux)

png('acid_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(acid_time_WL, aes(x=months, y=pct_WL_cum, color=med_acid_reflux)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On acid reflux\nmedication", labels=c("No","Yes")) +
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

## vitamin
vit_time_WL <- summaryBy(pct_WL_cum ~ med_vitamin + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(vit_time_WL)[3] <- "pct_WL_cum"
vit_time_WL <- rbind(data.frame(med_vitamin=0,months=0,pct_WL_cum=0),vit_time_WL)
vit_time_WL <- rbind(data.frame(med_vitamin=1,months=0,pct_WL_cum=0),vit_time_WL)
vit_time_WL <- vit_time_WL[order(vit_time_WL$med_vitamin, vit_time_WL$months),]
vit_time_WL$med_vitamin <- as.factor(vit_time_WL$med_vitamin)

png('vit_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(vit_time_WL, aes(x=months, y=pct_WL_cum, color=med_vitamin)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="Taking\nvitamins", labels=c("No","Yes")) +
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

## diuretics
diur_time_WL <- summaryBy(pct_WL_cum ~ med_diuretics + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(diur_time_WL)[3] <- "pct_WL_cum"
diur_time_WL <- rbind(data.frame(med_diuretics=0,months=0,pct_WL_cum=0),diur_time_WL)
diur_time_WL <- rbind(data.frame(med_diuretics=1,months=0,pct_WL_cum=0),diur_time_WL)
diur_time_WL <- diur_time_WL[order(diur_time_WL$med_diuretics, diur_time_WL$months),]
diur_time_WL$med_diuretics <- as.factor(diur_time_WL$med_diuretics)

png('diur_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(diur_time_WL, aes(x=months, y=pct_WL_cum, color=med_diuretics)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On diuretics\nmedication", labels=c("No","Yes")) +
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

## allergies
aller_time_WL <- summaryBy(pct_WL_cum ~ med_allergies + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(aller_time_WL)[3] <- "pct_WL_cum"
aller_time_WL <- rbind(data.frame(med_allergies=0,months=0,pct_WL_cum=0),aller_time_WL)
aller_time_WL <- rbind(data.frame(med_allergies=1,months=0,pct_WL_cum=0),aller_time_WL)
aller_time_WL <- aller_time_WL[order(aller_time_WL$med_allergies, aller_time_WL$months),]
aller_time_WL$med_allergies <- as.factor(aller_time_WL$med_allergies)

png('aller_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(aller_time_WL, aes(x=months, y=pct_WL_cum, color=med_allergies)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On allergy\nmedication", labels=c("No","Yes")) +
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

## birth control
BC_time_WL <- summaryBy(pct_WL_cum ~ med_birthcontrol + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(BC_time_WL)[3] <- "pct_WL_cum"
BC_time_WL <- rbind(data.frame(med_birthcontrol=0,months=0,pct_WL_cum=0),BC_time_WL)
BC_time_WL <- rbind(data.frame(med_birthcontrol=1,months=0,pct_WL_cum=0),BC_time_WL)
BC_time_WL <- BC_time_WL[order(BC_time_WL$med_birthcontrol, BC_time_WL$months),]
BC_time_WL$med_birthcontrol <- as.factor(BC_time_WL$med_birthcontrol)

png('BC_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(BC_time_WL, aes(x=months, y=pct_WL_cum, color=med_birthcontrol)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On birth\ncontrol", labels=c("No","Yes")) +
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

## asthma
asthma_time_WL <- summaryBy(pct_WL_cum ~ med_asthma + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(asthma_time_WL)[3] <- "pct_WL_cum"
asthma_time_WL <- rbind(data.frame(med_asthma=0,months=0,pct_WL_cum=0),asthma_time_WL)
asthma_time_WL <- rbind(data.frame(med_asthma=1,months=0,pct_WL_cum=0),asthma_time_WL)
asthma_time_WL <- asthma_time_WL[order(asthma_time_WL$med_asthma, asthma_time_WL$months),]
asthma_time_WL$med_asthma <- as.factor(asthma_time_WL$med_asthma)

png('asthma_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(asthma_time_WL, aes(x=months, y=pct_WL_cum, color=med_asthma)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-14:0), limits=c(-14,0)) +
	scale_color_discrete(name="On asthma\nmedication", labels=c("No","Yes")) +
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

## aspirin
asp_time_WL <- summaryBy(pct_WL_cum ~ med_aspirin + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(asp_time_WL)[3] <- "pct_WL_cum"
asp_time_WL <- rbind(data.frame(med_aspirin=0,months=0,pct_WL_cum=0),asp_time_WL)
asp_time_WL <- rbind(data.frame(med_aspirin=1,months=0,pct_WL_cum=0),asp_time_WL)
asp_time_WL <- asp_time_WL[order(asp_time_WL$med_aspirin, asp_time_WL$months),]
asp_time_WL$med_aspirin <- as.factor(asp_time_WL$med_aspirin)

png('asp_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(asp_time_WL, aes(x=months, y=pct_WL_cum, color=med_aspirin)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0), limits=c(-15,0)) +
	scale_color_discrete(name="Taking\naspirin", labels=c("No","Yes")) +
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

## blood thinner
BT_time_WL <- summaryBy(pct_WL_cum ~ med_bloodthinner + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(BT_time_WL)[3] <- "pct_WL_cum"
BT_time_WL <- rbind(data.frame(med_bloodthinner=0,months=0,pct_WL_cum=0),BT_time_WL)
BT_time_WL <- rbind(data.frame(med_bloodthinner=1,months=0,pct_WL_cum=0),BT_time_WL)
BT_time_WL <- BT_time_WL[order(BT_time_WL$med_bloodthinner, BT_time_WL$months),]
BT_time_WL$med_bloodthinner <- as.factor(BT_time_WL$med_bloodthinner)

png('BT_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(BT_time_WL, aes(x=months, y=pct_WL_cum, color=med_bloodthinner)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0), limits=c(-15,0)) +
	scale_color_discrete(name="On blood\nthinner\nmedication", labels=c("No","Yes")) +
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


















## coach meetings over time - vitamins
vit_time_meet <- summaryBy(note_count_total ~ med_vitamin + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(vit_time_meet)[3] <- "note_count_total"
vit_time_meet <- rbind(data.frame(med_vitamin=0,months=0,note_count_total=0),vit_time_meet)
vit_time_meet <- rbind(data.frame(med_vitamin=1,months=0,note_count_total=0),vit_time_meet)
vit_time_meet <- vit_time_meet[order(vit_time_meet$med_vitamin, vit_time_meet$months),]
vit_time_meet$med_vitamin <- as.factor(vit_time_meet$med_vitamin)

png('vit_time_meet.png',width=7, height=5, units='in', res=300)
ggplot(vit_time_meet, aes(x=months, y=note_count_total, color=med_vitamin)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(0:15)) +
	scale_color_discrete(name="Taking\nvitamins", labels=c("No","Yes")) +
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


## coach meetings over time - cholesterol
med_time_meet <- summaryBy(note_count_total ~ med_cholesterol + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(med_time_meet)[3] <- "note_count_total"
med_time_meet <- rbind(data.frame(med_cholesterol=0,months=0,note_count_total=0),med_time_meet)
med_time_meet <- rbind(data.frame(med_cholesterol=1,months=0,note_count_total=0),med_time_meet)
med_time_meet <- med_time_meet[order(med_time_meet$med_cholesterol, med_time_meet$months),]
med_time_meet$med_cholesterol <- as.factor(med_time_meet$med_cholesterol)

ggplot(med_time_meet, aes(x=months, y=note_count_total, color=med_cholesterol)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(0:15)) +
	scale_color_discrete(name="On medication", labels=c("No","Yes")) +
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



## cumulative coach meetings over time by medication use indicator
med_time_meet <- summaryBy(note_count_total ~ med_indicator + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(med_time_meet)[3] <- "note_count_total"
med_time_meet <- rbind(data.frame(med_indicator=0,months=0,note_count_total=0),med_time_meet)
med_time_meet <- rbind(data.frame(med_indicator=1,months=0,note_count_total=0),med_time_meet)
med_time_meet <- med_time_meet[order(med_time_meet$med_indicator, med_time_meet$months),]
med_time_meet$med_indicator <- as.factor(med_time_meet$med_indicator)

png('any_time_meet.png',width=7, height=5, units='in', res=300)
ggplot(med_time_meet, aes(x=months, y=note_count_total, color=med_indicator)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(0:15)) +
	scale_color_discrete(name="On\nmedication", labels=c("No","Yes")) +
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


## cumulative weight loss over time by medication use indicator
med_time_WL <- summaryBy(pct_WL_cum ~ med_indicator + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(med_time_WL)[3] <- "pct_WL_cum"
med_time_WL <- rbind(data.frame(med_indicator=0,months=0,pct_WL_cum=0),med_time_WL)
med_time_WL <- rbind(data.frame(med_indicator=1,months=0,pct_WL_cum=0),med_time_WL)
med_time_WL <- med_time_WL[order(med_time_WL$med_indicator, med_time_WL$months),]
med_time_WL$med_indicator <- as.factor(med_time_WL$med_indicator)

png('any_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(med_time_WL, aes(x=months, y=pct_WL_cum, color=med_indicator)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) +
	scale_color_discrete(name="On\nmedication", labels=c("No","Yes")) +
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




## cumulative coach meetings by gender over time
gender_time_meet <- summaryBy(note_count_total ~ gender + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(gender_time_meet)[3] <- "note_count_total"
gender_time_meet <- gender_time_meet[which(gender_time_meet$gender != ""),]
gender_time_meet <- rbind(data.frame(gender="female",months=0,note_count_total=0),gender_time_meet)
gender_time_meet <- rbind(data.frame(gender="male",months=0,note_count_total=0),gender_time_meet)
gender_time_meet <- gender_time_meet[order(gender_time_meet$gender, gender_time_meet$months),]

png('gender_time_meet.png',width=7, height=5, units='in', res=300)
ggplot(gender_time_meet, aes(x=months, y=note_count_total, color=gender)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(0:15)) +
	scale_color_discrete(name="Sex", labels=c("Male","Female")) +
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




## weight loss by gender over time
gender_time_WL <- summaryBy(pct_WL_cum ~ gender + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(gender_time_WL)[3] <- "pct_WL_cum"
gender_time_WL <- gender_time_WL[which(gender_time_WL$gender != ""),]
gender_time_WL <- rbind(data.frame(gender="female",months=0,pct_WL_cum=0),gender_time_WL)
gender_time_WL <- rbind(data.frame(gender="male",months=0,pct_WL_cum=0),gender_time_WL)
gender_time_WL <- gender_time_WL[order(gender_time_WL$gender, gender_time_WL$months),]

png('gender_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(gender_time_WL, aes(x=months, y=pct_WL_cum, color=gender)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) +
	scale_color_discrete(name="Sex", labels=c("Male","Female")) +
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


## 


## histogram of starting age by gender
png('age_gender_distr.png',width=7, height=4, units='in', res=300)
ggplot(data=data09[which(data09$months==0 & data09$gender != ""),], aes(age, fill=gender)) +
	geom_histogram(breaks = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90), color="black") +
	xlab("Starting Age") +	
	ylab("Count") +
	scale_x_continuous(breaks = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90)) +
	scale_fill_manual(name="Gender", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right")
dev.off()
		

		

## medication histogram
tmp1 <- data09[which(data09$months==0),]
tmp1$age_grp <- cut(tmp1$age, breaks=c(18,30,40,50,60,90), right=FALSE)
tmp2 <- tmp1[,c("id","gender","age_grp","med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")]
tmp2$med_none <- 1 - tmp2$med_indicator
tmp3 <- melt(tmp2, id=c("id","gender","age_grp"))
tmp4 <- summaryBy(value ~ variable, tmp3, FUN=sum)
tmp4$Medication <- c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None")
colnames(tmp4)[2] <- "Count"

png('med_distr_combined_data.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp4[1:14,], aes(x=Medication, y=Count)) +
	geom_bar(stat="identity", color="black", fill="turquoise2") +
	xlab("Medications") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

	
## gender and medication histogram
tmp5 <- summaryBy(value ~ gender + variable, tmp3, FUN=sum)
tmp5 <- tmp5[which(tmp5$gender != ""),]

tmp5$Medication <- rep(c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None"), 2)
colnames(tmp5)[c(1,3)] <- c("Gender","Count")

#by actual number and stacked
png('med_gender_distr.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp5[which(!(tmp5$Medication %in% c("Indicator","None"))),], aes(x=Medication, y=Count, fill=Gender)) +
	geom_bar(stat="identity", color="black") +
	xlab("Medications") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	scale_fill_manual(name="Sex", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()
round((tmp5[which((tmp5$Medication=="Blood Pressure" & tmp5$Gender=="female")),"Count"] / sum(tmp5[which((tmp5$Medication=="Blood Pressure")),"Count"])) *100, 1)
round((tmp5[which((tmp5$Medication=="Blood Pressure" & tmp5$Gender=="male")),"Count"] / sum(tmp5[which((tmp5$Medication=="Blood Pressure")),"Count"])) *100, 1)

#actual number and side by side
png('med_gender_distr2.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp5[which(!(tmp5$Medication %in% c("Indicator","None"))),], aes(x=Medication, y=Count, fill=Gender)) +
	geom_bar(stat="identity", color="black", position="dodge") +
	xlab("Medications") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	scale_fill_manual(name="Sex", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#total distribution and side by side
tmp5a <- tmp5[which(!(tmp5$Medication %in% c("Indicator","None"))),]
tmp5a$pct <- tmp5a$Count/sum(tmp5a$Count) 

png('med_gender_distr3.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp5a, aes(x=Medication, y=pct, fill=Gender)) +
	geom_bar(stat="identity", color="black", position="dodge") +
	xlab("Medications") +	
	ylab("Distribution") +
	scale_y_continuous(breaks=seq(0,1,.05)) +
	scale_fill_manual(name="Sex", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#distribution by gender and side by side
tmp5b <- tmp5[which(!(tmp5$Medication %in% c("Indicator","None"))),]
tmp5b$pct <- ifelse(tmp5b$Gender=="female", tmp5b$Count/sum(tmp5b[which(tmp5b$Gender=="female"),]$Count),  tmp5b$Count/sum(tmp5b[which(tmp5b$Gender=="male"),]$Count))

png('med_gender_distr4.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp5b, aes(x=Medication, y=pct, fill=Gender)) +
	geom_bar(stat="identity", color="black", position="dodge") +
	xlab("Medications") +	
	ylab("Distribution") +
	scale_y_continuous(breaks=seq(0,1,.05)) +
	scale_fill_manual(name="Sex", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()

#blood pressure
round(tmp5b[which((tmp5b$Medication=="Blood Pressure" & tmp5b$Gender=="female")),"pct"] *100, 1)
round(tmp5b[which((tmp5b$Medication=="Blood Pressure" & tmp5b$Gender=="male")),"pct"] *100, 1)

#antidepressant
round(tmp5b[which((tmp5b$Medication=="Antidepressant" & tmp5b$Gender=="female")),"pct"] *100, 1)
round(tmp5b[which((tmp5b$Medication=="Antidepressant" & tmp5b$Gender=="male")),"pct"] *100, 1)

	
## age and medication histogram
tmp6 <- summaryBy(value ~ age_grp + variable, tmp3, FUN=sum)
tmp6 <- tmp6[which(tmp6$age_grp != ""),]
tmp6$Medication <- rep(c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None"), 5)
colnames(tmp6)[c(1,3)] <- c("AgeGroup","Count")
tmp6 <- tmp6[order(tmp6$AgeGroup, decreasing=TRUE),]

png('med_age_distr.png',width=8, height=4, units='in', res=300)
ggplot(data=tmp6[which(!(tmp6$Medication %in% c("Indicator","None"))),], aes(x=Medication, y=Count, fill=AgeGroup)) +
	geom_bar(stat="identity", color="black") +
	xlab("Medications") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,10000,1000), label = comma) +
	scale_fill_manual(name="Age Group", values=c("firebrick1","darkorange1","forestgreen","dodgerblue3","darkorchid3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()





#### weight loss over time by medication use
dat1 <- data09[which(data09$months>0 & data09$months<=12),]
dat2 <- dat1[,c("id","months","pct_WL_cum","med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")]
dat2$med_none <- 1 - dat2$med_indicator
dat3 <- melt(dat2, id=c("id","months","pct_WL_cum"))
dat4 <- dat3[which(dat3$value==1),]
dat5 <- dat4[,c("id","months","pct_WL_cum","variable")]
dat6 <- summaryBy(pct_WL_cum ~ months + variable, dat5, FUN=function(x)mean(x, na.rm=TRUE))
dat6$Medication <- rep(c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None"), 12)
colnames(dat6)[3] <- "pct_WL_cum"

dat7 <- dat6[,c("months","pct_WL_cum","Medication")]
dat7 <- rbind(data.frame(cbind(months=rep(0,16), 
					pct_WL_cum=rep(0,16), 
					Medication=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
									"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None")),stringsAsFactors=FALSE)
				,dat7)
dat8 <- dat7
dat8$months <- as.integer(dat8$months)
dat8$pct_WL_cum <- as.numeric(dat8$pct_WL_cum)

dat9 <- dat8[which(dat8$Medication %in% c("Acid Reflux","Antidepressant","Blood Pressure","Cholesterol","Sleep","Diabetes","Vitamins","None")),]	

png('med_time_WL_some.png',width=8, height=5, units='in', res=300)
ggplot(dat9, aes(x=months, y=pct_WL_cum, color=Medication)) +
	geom_line(size=1, aes(color=Medication, group=Medication)) +	
	geom_point(size=1, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) +
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

png('med_time_WL_all.png',width=8, height=5, units='in', res=300)
ggplot(dat8, aes(x=months, y=pct_WL_cum, color=Medication)) +
	geom_line(size=1, aes(color=Medication, group=Medication)) +	
	geom_point(size=1, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) +
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





#### coach meetings over time by medication use
dat1a <- data09[which(data09$months>0 & data09$months<=12),]
dat2a <- dat1a[,c("id","months","note_count_total","med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")]
dat2a$med_none <- 1 - dat2a$med_indicator
dat3a<- melt(dat2a, id=c("id","months","note_count_total"))
dat4a <- dat3a[which(dat3a$value==1),]
dat5a <- dat4a[,c("id","months","note_count_total","variable")]
dat6a <- summaryBy(note_count_total ~ months + variable, dat5a, FUN=function(x)mean(x, na.rm=TRUE))
dat6a$Medication <- rep(c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
							"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None"), 12)
colnames(dat6a)[3] <- "note_count_total"

dat7a <- dat6a[,c("months","note_count_total","Medication")]
dat7a <- rbind(data.frame(cbind(months=rep(0,16), 
					note_count_total=rep(0,16), 
					Medication=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins",
									"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Indicator","None")),stringsAsFactors=FALSE)
				,dat7a)
dat8a <- dat7a
dat8a$months <- as.integer(dat8a$months)
dat8a$note_count_total <- as.numeric(dat8a$note_count_total)

png('med_time_meet_all.png',width=8, height=5, units='in', res=300)
ggplot(dat8a, aes(x=months, y=note_count_total, color=Medication)) +
	geom_line(size=1, aes(color=Medication, group=Medication)) +	
	geom_point(size=1, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(0:15)) +
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




## WL over time by starting BMI classification
data09$start_bmi_cat <- with(data09,
					ifelse(start_bmi >= 30, "4Obese",
						ifelse(start_bmi >= 25 & start_bmi < 30, "3Overweight", 
							ifelse(start_bmi >= 18.5 & start_bmi < 25, "2Healthy", "1Underweight"))))
WL_time_bmi <- summaryBy(pct_WL_cum ~ start_bmi_cat + months, data = data09[which(data09$months>0 & data09$months<=12 & data09$start_bmi_cat != "1Underweight"),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(WL_time_bmi)[3] <- "pct_WL_cum"
WL_time_bmi <- rbind(data.frame(start_bmi_cat="2Healthy",months=0,pct_WL_cum=0),WL_time_bmi)
WL_time_bmi <- rbind(data.frame(start_bmi_cat="3Overweight",months=0,pct_WL_cum=0),WL_time_bmi)
WL_time_bmi <- rbind(data.frame(start_bmi_cat="4Obese",months=0,pct_WL_cum=0),WL_time_bmi)
WL_time_bmi <- WL_time_bmi[order(WL_time_bmi$start_bmi_cat, decreasing=TRUE),]

png('WL_time_bmi.png',width=8, height=5, units='in', res=300)
ggplot(WL_time_bmi, aes(x=months, y=pct_WL_cum, color=start_bmi_cat)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) + 
	scale_color_discrete(name="Starting BMI\nClassification", labels=c("Obese","Overweight","Healthy")) +
	guides(col = guide_legend(reverse = TRUE)) +
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







































## OTHER GRAPHS ##

## box plot of 6 month weight loss of different medications
a1 <- data09[which(data09$months==6),]
a2 <- a1[,c("id","pct_WL_cum","med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")]
a2$med_none <- 1 - a2$med_indicator
a3 <- melt(a2, id=c("id","pct_WL_cum"))
a4 <- a3[which(a3$value==1),]
a5 <- a4[which(a4$variable != "med_indicator" & is.na(a4$pct_WL_cum)==0 ),c("id","pct_WL_cum","variable")]
colnames(a5)[3] <- "Medication"

png('WL6_med_box.png',width=7, height=6, units='in', res=300)
ggplot(a5, aes(Medication, pct_WL_cum)) +
	geom_boxplot() +
	xlab("Medications") +	
	ylab("Average Weight Loss at 6 Months") + 
	scale_x_discrete(labels=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins", "Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","None")) +
	scale_y_continuous(breaks = seq(-60,60,10)) + 
	theme(axis.text.x = element_text(angle = 55, hjust=1),
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

#same as above but condense a little
png('WL6_med_box2.png',width=7, height=6, units='in', res=300)
ggplot(a5, aes(Medication, pct_WL_cum)) +
	geom_boxplot() +
	xlab("Medications") +	
	ylab("Average Weight Loss at 6 Months") + 
	scale_x_discrete(labels=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins", "Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","None")) +
	scale_y_continuous(breaks = seq(-60,60,10), limits=c(-40,10)) + 
	theme(axis.text.x = element_text(angle = 55, hjust=1),
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



## box plot of 12 month weight loss of different medications
b1 <- data09[which(data09$months==12),]
b2 <- b1[,c("id","pct_WL_cum","med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")]
b2$med_none <- 1 - b2$med_indicator
b3 <- melt(b2, id=c("id","pct_WL_cum"))
b4 <- b3[which(b3$value==1),]
b5 <- b4[which(b4$variable != "med_indicator" & is.na(b4$pct_WL_cum)==0 ),c("id","pct_WL_cum","variable")]
colnames(b5)[3] <- "Medication"

png('WL12_med_box.png',width=7, height=6, units='in', res=300)
ggplot(b5, aes(Medication, pct_WL_cum)) +
	geom_boxplot() +
	xlab("Medications") +	
	ylab("Average Weight Loss at 12 Months") + 
	scale_x_discrete(labels=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins", "Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","None")) +
	scale_y_continuous(breaks = seq(-60,60,10)) + 
	theme(axis.text.x = element_text(angle = 55, hjust=1),
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

#same as above but condense a little
png('WL12_med_box2.png',width=7, height=6, units='in', res=300)
ggplot(b5, aes(Medication, pct_WL_cum)) +
	geom_boxplot() +
	xlab("Medications") +	
	ylab("Average Weight Loss at 12 Months") + 
	scale_x_discrete(labels=c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins", "Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","None")) +
	scale_y_continuous(breaks = seq(-60,60,10), limits=c(-40,20)) + 
	theme(axis.text.x = element_text(angle = 55, hjust=1),
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







## MOSAIC PLOTS
data09$start_bmi_cat <- with(data09,
					ifelse(start_bmi >= 40, "Obese III", 
						ifelse(start_bmi >= 35, "Obese II",
							ifelse(start_bmi >= 30, "Obese I",
								ifelse(start_bmi >= 25 & start_bmi < 30, "Overweight", 
									ifelse(start_bmi >= 18.5 & start_bmi < 25, "Healthy", "Underweight"))))))
data09$age_grp <- cut(data09$age, breaks=c(18,30,40,50,60,90), right=FALSE)	
data09$sex <- ifelse(data09$gender=="female", "F", ifelse(data09$gender=="male", "M", ""))
									
## mosaic plot of gender and starting bmi category
tmp1 <- data09[which(data09$months==0 & data09$gender != "" & data09$start_bmi_cat != "Underweight"),]
tmp1$gender <- droplevels(tmp1$gender)
tmp2 <- structable(tmp1$sex ~ tmp1$start_bmi_cat)
tmp3 <- tmp2[c(1,5,2,3,4),]

png('bmi_sex.png',width=10, height=5, units='in', res=300)
	mosaicplot(tmp3, xlab="Starting BMI", ylab="Sex", main="", color=c("purple2","chartreuse3"), cex.axis=1)
dev.off()



## mosaic plot of age group and starting bmi category 
tmp4 <- structable(tmp1$age_grp ~ tmp1$start_bmi_cat)
tmp5 <- tmp4[c(1,5,2,3,4),]

png('bmi_age.png',width=7, height=6, units='in', res=300)
	mosaicplot(tmp5, xlab="Starting BMI", ylab="Age Group", main="", color=c("red","orange","green","blue","purple"))
dev.off()



## mosaic plot of age group and sex 
tmp6 <- structable(tmp1$sex ~ tmp1$age_grp)
png('sex_age.png',width=7, height=6, units='in', res=300)
	mosaicplot(tmp6, xlab="Age Group", ylab="Sex", main="", color=c("mediumorchid3","aquamarine3"))
dev.off()

## histogram of starting age by gender
png('age_gender_distr2.png',width=6, height=4, units='in', res=300)
ggplot(data=data09[which(data09$months==0 & data09$gender != ""),], aes(age, fill=gender)) +
	geom_histogram(breaks = c(18,30,40,50,60,90), color="black") +
	xlab("Starting Age") +	
	ylab("Count") +
	scale_x_continuous(breaks = c(18,30,40,50,60,90)) +
	scale_fill_manual(name="Gender", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right")
dev.off()








##

## weight loss by gender over time
marital_time_WL <- summaryBy(pct_WL_cum ~ marital_status + months, data = data09[which(data09$months>0 & data09$months<=12),], FUN=function(x)mean(x, na.rm=TRUE))
colnames(marital_time_WL)[3] <- "pct_WL_cum"
marital_time_WL <- marital_time_WL[which(marital_time_WL$marital_status != ""),]
marital_time_WL <- rbind(data.frame(marital_status="divorced",months=0,pct_WL_cum=0),marital_time_WL)
marital_time_WL <- rbind(data.frame(marital_status="married",months=0,pct_WL_cum=0),marital_time_WL)
marital_time_WL <- rbind(data.frame(marital_status="partner",months=0,pct_WL_cum=0),marital_time_WL)
marital_time_WL <- rbind(data.frame(marital_status="single",months=0,pct_WL_cum=0),marital_time_WL)
marital_time_WL <- rbind(data.frame(marital_status="widowed",months=0,pct_WL_cum=0),marital_time_WL)
marital_time_WL <- marital_time_WL[order(marital_time_WL$marital_status, marital_time_WL$months),]

png('marital_time_WL.png',width=7, height=5, units='in', res=300)
ggplot(marital_time_WL, aes(x=months, y=pct_WL_cum, color=marital_status)) +
	geom_line(size=3) +	
	geom_point(size=3, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-15:0)) +
	scale_color_discrete(name="Marital Status", labels=c("Widowed","Single","Partner","Married","Divorced")) +
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







#distribution by gender and side by side
marry_gender1 <- data09[which(data09$months==0 & data09$gender != "" & data09$marital_status != ""),]
marry_gender2 <- marry_gender1[,c("id","gender","marital_status")]
marry_gender3 <- melt(marry_gender2, id=c("id","gender","marital_status"))
marry_gender5 <- as.data.frame(table(marry_gender3$gender,marry_gender3$marital_status))
colnames(marry_gender5) <- c("Gender","MaritalStatus","Count")
marry_gender5$pct <- ifelse(marry_gender5$Gender=="female", marry_gender5$Count/sum(marry_gender5[which(marry_gender5$Gender=="female"),]$Count),  marry_gender5$Count/sum(marry_gender5[which(marry_gender5$Gender=="male"),]$Count))
marry_gender5 <- marry_gender5[c(5,6,8,9,11,12,14,15,17,18),]

png('marital_gender_distr2.png',width=6, height=4, units='in', res=300)
ggplot(data=marry_gender5, aes(x=MaritalStatus, y=pct, fill=Gender)) +
	geom_bar(stat="identity", color="black", position="dodge") +
	xlab("Marital Status") +	
	ylab("Distribution") +
	scale_y_continuous(breaks=seq(0,1,.1)) +
	scale_fill_manual(name="Sex", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"))
dev.off()





## histogram of starting age by gender
data09$age_grp <- cut(data09$age, breaks=c(18,30,40,50,60,90), right=FALSE)	
age_gender1 <- data09[which(data09$months==0 & data09$gender != ""),]
age_gender2 <- age_gender1[,c("id","gender","age_grp")]
age_gender3 <- melt(age_gender2, id=c("id","gender","age_grp"))
age_gender5 <- as.data.frame(table(age_gender3$gender,age_gender3$age_grp))
colnames(age_gender5) <- c("Gender","AgeGroup","Count")
age_gender5$pct <- ifelse(age_gender5$Gender=="female", age_gender5$Count/sum(age_gender5[which(age_gender5$Gender=="female"),]$Count),  age_gender5$Count/sum(age_gender5[which(age_gender5$Gender=="male"),]$Count))
age_gender5 <- age_gender5[c(2,3,5,6,8,9,11,12,14,15),]

png('age_gender_distr3.png',width=6, height=4, units='in', res=300)
ggplot(data=age_gender5, aes(x=AgeGroup, y=pct, fill=Gender)) +
	geom_bar(stat="identity", color="black", position="dodge") +
	xlab("Starting Age") +	
	ylab("Distribution") +
	scale_y_continuous(breaks=seq(0,1,.05)) +
	scale_fill_manual(name="Gender", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right")
dev.off()


## histogram of starting age by gender
data09$age_grp <- cut(data09$age, breaks=c(18,30,40,50,60,90), right=FALSE)	
age_gender1 <- data09[which(data09$months==0 & data09$gender != ""),]
age_gender6 <- summaryBy(id ~ gender + age_grp, data=age_gender1, FUN=length)
age_gender7 <- age_gender6[c(1:5,7:11),]

png('age_gender_distr4.png',width=6, height=4, units='in', res=300)
ggplot(data=age_gender7, aes(x=age_grp, y=id.length, fill=gender)) +
	geom_bar(stat="identity", color="black") +
	xlab("Starting Age") +	
	ylab("Count") +
	scale_y_continuous(breaks=seq(0,10000,1000)) +
	scale_fill_manual(name="Gender", labels=c("Female","Male"), values=c("mediumorchid3","aquamarine3")) +
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right")
dev.off()




## boxplot gender vs age
png('gender_age.png',width=3, height=4, units='in', res=300)
ggplot(data09[which(data09$months==0 & is.na(data09$age)==0 & data09$gender!=""),], aes(gender, age)) +
	geom_boxplot() +
	xlab("Sex") +	
	ylab("Starting Age") + 
	scale_x_discrete(labels=c("Female", "Male")) +
	scale_y_continuous(breaks = seq(0,90,10)) + 
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

t.test(data09[which(data09$months==0 & is.na(data09$age)==0 & data09$gender=="female"),"age"], data09[which(data09$months==0 & is.na(data09$age)==0 & data09$gender=="male"),"age"])


## boxplot gender vs starting BMI
png('gender_bmi.png',width=4, height=4, units='in', res=300)
ggplot(data09[which(data09$months==0 & data09$gender!=""),], aes(gender, start_bmi)) +
	geom_boxplot() +
	xlab("Sex") +	
	ylab("Starting BMI") + 
	scale_x_discrete(labels=c("Female", "Male")) +
	scale_y_continuous(breaks = seq(0,200,10)) + 
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


## scatter age vs BMI
png('age_bmi.png',width=6, height=4, units='in', res=300)
ggplot(data09[which(data09$months==0 & is.na(data09$age)==0),], aes(age, start_bmi)) +
	geom_point(color="blue") +
	xlab("Age") +	
	ylab("Starting BMI") + 
	scale_x_continuous(breaks = seq(0,100,10)) +
	scale_y_continuous(breaks = seq(0,200,10)) + 
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		# panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right"
	)
dev.off()


## boxplot age grp vs BMI	
png('agegrp_bmi.png',width=4, height=4, units='in', res=300)
ggplot(data09[which(data09$months==0 & is.na(data09$age)==0),], aes(age_grp, start_bmi)) +
	geom_boxplot() +
	xlab("Age Group") +	
	ylab("Starting BMI") + 
	scale_x_discrete(labels=c("[18,30)","[30,40)","[40,50)","[50,60)","[60,90)")) +
	scale_y_continuous(breaks = seq(0,200,10)) + 
	theme(axis.text.x = element_text(angle = 45, hjust=1),
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		# panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right"
	)
dev.off()



## scatter cum WL by cum coach meetings at month 12
png('WL_meetings12.png',width=6, height=4, units='in', res=300)
ggplot(data09[which(data09$months==12 & is.na(data09$age)==0),], aes(note_count_total, pct_WL_cum)) +
	geom_point(color="blue") +
	xlab("Cumulative Coach Meetings") + 
	ylab("Cumulative Weight Loss") +	
	scale_x_continuous(breaks = seq(0,100,10)) +
	scale_y_continuous(breaks = seq(-60,60,10)) + 
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		# panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right"
	)
dev.off()


#scatter cum WL by cum coach meetings at month 12
png('WL_meetings12_2.png',width=8, height=6, units='in', res=300)
ggplot(data09[which(data09$months==12 & is.na(data09$age)==0),], aes(note_count_total, pct_WL_cum)) +
	stat_binhex(bins=50, geom="hex") +
	scale_fill_gradientn(colours=c("lightskyblue","black"),name = "Frequency",na.value=NA) +
		geom_smooth(method='lm', se=FALSE, color="black") +
	xlab("Cumulative Coach Meetings") + 
	ylab("Cumulative Weight Loss") +	
	scale_x_continuous(breaks = seq(0,100,10)) +
	scale_y_continuous(breaks = seq(-60,60,10)) + 
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="right",
		plot.title = element_text(size=18, face="bold"))
dev.off()







## MEAL PLAN GRAPHS ##
table(data09[which(data09$months==0),"gender"]) / nrow(data09[which(data09$months==0),])

#meal plans distribution over time
dat01 <- data09[which(data09$months>0 & data09$months<=12),]
dat02 <- dat01[,c("id","months","pct_WL_cum","plan_grp")]
dat03 <- melt(dat02, id=c("id","months","pct_WL_cum"))
dat04 <- dat03[,c("id","months","pct_WL_cum","value")]
dat05 <- summaryBy(pct_WL_cum ~ months + value, dat04, FUN=length)
colnames(dat05)[2] <- "MealPlan"
colnames(dat05)[3] <- "N"

dat05$months <- as.integer(dat05$months)
dat05$N <- as.integer(dat05$N)
		
dat06 <- dat05[which(dat05$MealPlan %in% c("Balance","Reboot Adapt","Reboot Reduce","Sustain")),]
dat06$MealPlan <- as.character(dat06$MealPlan)
dat06$MealPlan[dat06$MealPlan == "Reboot Adapt"] <- "Adapt"
dat06$MealPlan[dat06$MealPlan == "Reboot Reduce"] <- "Reduce"
dat06$MealPlan <- as.factor(dat06$MealPlan)


png('mealplan_distr.png',width=8, height=5, units='in', res=300)
ggplot(dat06, aes(x=months, y=N, color=MealPlan)) +
	geom_line(size=2, aes(color=MealPlan, group=MealPlan)) +	
	geom_point(size=2, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Number of Members") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = seq(0,30000,2000)) +
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

#create percentages
N <- summaryBy(N ~ months, dat06, FUN=sum)
dat07 <- merge(dat06, N, by="months")
dat07$pct <- round((dat07$N/dat07$N.sum)*100, 1)


png('mealplan_distr_pct.png',width=8, height=5, units='in', res=300)
ggplot(dat07, aes(x=months, y=pct, color=MealPlan)) +
	geom_line(size=2, aes(color=MealPlan, group=MealPlan)) +	
	geom_point(size=2, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Distribution of Members") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = seq(0,100,10)) +
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



#wl by meal plan over time
dat1 <- data09[which(data09$months>0 & data09$months<=12),]
dat2 <- dat1[,c("id","months","pct_WL_cum","plan_grp")]
dat3 <- melt(dat2, id=c("id","months","pct_WL_cum"))
dat4 <- dat3[,c("id","months","pct_WL_cum","value")]
dat5 <- summaryBy(pct_WL_cum ~ months + value, dat4, FUN=function(x)mean(x, na.rm=TRUE))
colnames(dat5)[2] <- "MealPlan"
colnames(dat5)[3] <- "pct_WL_cum"

dat5$months <- as.integer(dat5$months)
dat5$pct_WL_cum <- as.numeric(dat5$pct_WL_cum)

dat6 <- dat5[which(dat5$MealPlan %in% c("Balance","Reboot Adapt","Reboot Reduce","Sustain")),]
dat6$MealPlan <- as.character(dat6$MealPlan)
dat6$MealPlan[dat6$MealPlan == "Reboot Adapt"] <- "Adapt"
dat6$MealPlan[dat6$MealPlan == "Reboot Reduce"] <- "Reduce"
dat6$MealPlan <- as.factor(dat6$MealPlan)

zero <- as.data.frame(t(matrix(c(0,"Balance",0,0,"Adapt",0,0,"Reduce",0,0,"Sustain",0),ncol=4)))
colnames(zero) <- c("months","MealPlan","pct_WL_cum")
zero$months <- as.integer(zero$months)
zero$pct_WL_cum <- as.numeric(zero$pct_WL_cum)
zero$months <-0
zero$pct_WL_cum <-0

dat7 <- rbind(zero, dat6)

png('mealplan_WL_time.png',width=8, height=5, units='in', res=300)
ggplot(dat7, aes(x=months, y=pct_WL_cum, color=MealPlan)) +
	geom_line(size=2, aes(color=MealPlan, group=MealPlan)) +	
	geom_point(size=2, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Cumulative Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = c(-20:0)) +
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





#average coach meetings by meal plan over time
dat1a <- data09[which(data09$months>0 & data09$months<=12),]
dat2a <- dat1a[,c("id","months","note_count","plan_grp")]
dat3a <- melt(dat2a, id=c("id","months","note_count"))
dat4a <- dat3a[,c("id","months","note_count","value")]
dat5a <- summaryBy(note_count ~ months + value, dat4a, FUN=function(x)mean(x, na.rm=TRUE))
colnames(dat5a)[2] <- "MealPlan"
colnames(dat5a)[3] <- "note_count"

dat5a$months <- as.integer(dat5a$months)
dat5a$note_count <- as.numeric(dat5a$note_count)

dat6a <- dat5a[which(dat5a$MealPlan %in% c("Balance","Reboot Adapt","Reboot Reduce","Sustain")),]
dat6a$MealPlan <- as.character(dat6a$MealPlan)
dat6a$MealPlan[dat6a$MealPlan == "Reboot Adapt"] <- "Adapt"
dat6a$MealPlan[dat6a$MealPlan == "Reboot Reduce"] <- "Reduce"
dat6a$MealPlan <- as.factor(dat6a$MealPlan)

png('mealplan_meetings_time.png',width=8, height=5, units='in', res=300)
ggplot(dat6a, aes(x=months, y=note_count, color=MealPlan)) +
	geom_line(size=2, aes(color=MealPlan, group=MealPlan)) +	
	geom_point(size=2, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Average Monthly Coach Meetings") + 
	scale_x_continuous(breaks = seq(0, 12, 1)) +
	scale_y_continuous(breaks = seq(0,3,0.2)) +
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











































##### TESTS #####

### LOOK AT GENDER DIFFERENCES ###
## function that inputs variable to generate table of means and p-value for gender and each time point - input variable of interest
ttest.func <- 
	function(variable){
		tab <- matrix(ncol=12,nrow=3)
		for(i in 1:12){
		tab[,i] <- 
		rbind(
				t.test(data09[which(data09$gender=="female" & data09$months==i),variable], 
							data09[which(data09$gender=="male" & data09$months==i),variable])$estimate[[1]][1],
				t.test(data09[which(data09$gender=="female" & data09$months==i),variable], 
							data09[which(data09$gender=="male" & data09$months==i),variable])$estimate[[2]][1],
				t.test(data09[which(data09$gender=="female" & data09$months==i),variable], 
							data09[which(data09$gender=="male" & data09$months==i),variable])$p.value
				)
		}
		rownames(tab) <- c("Female","Male","P-Value")
		colnames(tab) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
		return(tab)
	}
	
## cumulative weight loss percentage
ttest.func("pct_WL_cum")
## actual weight loss
ttest.func("weight")
## number of monthly weight recordings - All
ttest.func("MonthlyRecordings")
## monthly coach meetings
ttest.func("note_count")
## waist measurements
ttest.func("waist.median")
## number of waist measurements - only if there is a measurement
ttest.func("waist.N")
## hip measurements
ttest.func("hip.median")
## number of hip measurements - only if there is a measurement
ttest.func("hip.N")
## WHR
ttest.func("WHR")
## total minutes of activity
ttest.func("AllActivityTotalMinutes")
## number of recorded activities - All
ttest.func("AllActivityN")
## number of food items logged - All
ttest.func("Food_Logged")
## expected calories
ttest.func("calories")
## expected protein
ttest.func("protein")
## expected carbs
ttest.func("carbs")
## expected fiber
ttest.func("fiber")
## BMI
ttest.func("bmi")
## cumulative number of coach meetings
ttest.func("note_count_total")

## actual weight lost - need to move the months back one since this is previous weight loss
actual_WL <- matrix(ncol=12,nrow=3)
		for(j in 1:12){
		i <- (j+1)
		actual_WL[,j] <- 
		rbind(
				t.test(data09[which(data09$gender=="female" & data09$months==i),"prev_WL"], 
							data09[which(data09$gender=="male" & data09$months==i),"prev_WL"])$estimate[[1]][1],
				t.test(data09[which(data09$gender=="female" & data09$months==i),"prev_WL"], 
							data09[which(data09$gender=="male" & data09$months==i),"prev_WL"])$estimate[[2]][1],
				t.test(data09[which(data09$gender=="female" & data09$months==i),"prev_WL"], 
							data09[which(data09$gender=="male" & data09$months==i),"prev_WL"])$p.value
				)
		}
		rownames(actual_WL) <- c("Female","Male","P-Value")
		colnames(actual_WL) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
		actual_WL


## starting age
rbind(
	t.test(data09[which(data09$gender=="female" & data09$months==0),"age"], 
				data09[which(data09$gender=="male" & data09$months==0),"age"])$estimate[[1]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"age"], 
				data09[which(data09$gender=="male" & data09$months==0),"age"])$estimate[[2]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"age"], 
				data09[which(data09$gender=="male" & data09$months==0),"age"])$p.value
	)
## starting weight
rbind(
	t.test(data09[which(data09$gender=="female" & data09$months==0),"first_weight"], 
				data09[which(data09$gender=="male" & data09$months==0),"first_weight"])$estimate[[1]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"first_weight"], 
				data09[which(data09$gender=="male" & data09$months==0),"first_weight"])$estimate[[2]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"first_weight"], 
				data09[which(data09$gender=="male" & data09$months==0),"first_weight"])$p.value
	)
## starting BMI
rbind(
	t.test(data09[which(data09$gender=="female" & data09$months==0),"start_bmi"], 
				data09[which(data09$gender=="male" & data09$months==0),"start_bmi"])$estimate[[1]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"start_bmi"], 
				data09[which(data09$gender=="male" & data09$months==0),"start_bmi"])$estimate[[2]][1],
	t.test(data09[which(data09$gender=="female" & data09$months==0),"start_bmi"], 
				data09[which(data09$gender=="male" & data09$months==0),"start_bmi"])$p.value
	)

	

## relationship between WHR obesity classfication and BMI obesity classification - who falls into both?
table(data09[which(data09$months==1),"prev_WHR_obese"], data09[which(data09$months==1),"prev_bmi_cat"])
table(data09[which(data09$months==1),"prev_bmi_cat"])






data09$start_bmi_cat <- with(data09,
					ifelse(start_bmi >= 40, "Obese III", 
						ifelse(start_bmi >= 35, "Obese II",
							ifelse(start_bmi >= 30, "Obese I",
								ifelse(start_bmi >= 25 & start_bmi < 30, "Overweight", 
									ifelse(start_bmi >= 18.5 & start_bmi < 25, "Healthy", "Underweight"))))))
data09$start_bmi_cat2 <- with(data09,
					ifelse(start_bmi >= 40, "Obese III", 
						ifelse(start_bmi >= 35, "Obese II",
							ifelse(start_bmi >= 30, "Obese I",
								ifelse(start_bmi >= 25 & start_bmi < 30, "Overweight", "Healthy/Underweight")))))
data09$age_grp <- cut(data09$age, breaks=c(18,30,40,50,60,90), right=FALSE)	
data09$age_grp2 <- cut(data09$age, breaks=c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90), right=FALSE)	
data09$sex <- ifelse(data09$gender=="female", "F", ifelse(data09$gender=="male", "M", ""))	



## ANOVA ##
## anova gender vs age
summary(lm(age ~ gender, data=data09[which(data09$months==0 & is.na(data09$age)==0 & data09$gender!=""),]))

## anova gender vs BMI
summary(lm(start_bmi ~ gender, data=data09[which(data09$months==0 & data09$gender!=""),]))

## anova age group vs BMI
summary(lm(start_bmi ~ age_grp, data=data09[which(data09$months==0),]))
anova(lm(start_bmi ~ age_grp, data=data09[which(data09$months==0),]))[1,4] #fstatistic
anova(lm(start_bmi ~ age_grp, data=data09[which(data09$months==0),]))[1,5] #pvalue

## anova marital status vs WL/month
tab <- matrix(ncol=2,nrow=12)
for(i in 1:12){
tab[i,] <- 
cbind(
		anova(lm(pct_WL_cum ~ marital_status, data=data09[which(data09$months==i & data09$marital_status!=""),]))[1,4],
		anova(lm(pct_WL_cum ~ marital_status, data=data09[which(data09$months==i & data09$marital_status!=""),]))[1,5]
		)
}
colnames(tab) <- c("F-Statistic","P-Value")
rownames(tab) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
tab

## anova medication vs WL @6mo
med.list <- c("med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
			"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")
tab1 <- matrix(ncol=2,nrow=length(med.list))
for(i in 1:length(med.list)){
tab1[i,] <- 
cbind(
		anova(lm(paste0("pct_WL_cum ~ ",med.list[i]), data=data09[which(data09$months==6),]))[1,4],
		anova(lm(paste0("pct_WL_cum ~ ",med.list[i]), data=data09[which(data09$months==6),]))[1,5]
		)
}
colnames(tab1) <- c("F-Statistic","P-Value")
rownames(tab1) <- med.list
tab1

## anova medication vs WL @12mo
tab2 <- matrix(ncol=2,nrow=length(med.list))
for(i in 1:length(med.list)){
tab2[i,] <- 
cbind(
		anova(lm(paste0("pct_WL_cum ~ ",med.list[i]), data=data09[which(data09$months==12),]))[1,4],
		anova(lm(paste0("pct_WL_cum ~ ",med.list[i]), data=data09[which(data09$months==12),]))[1,5]
		)
}
colnames(tab2) <- c("F-Statistic","P-Value")
rownames(tab2) <- med.list
tab2








## function that inputs variable to generate table of means and p-value for gender and each time point - input variable of interest
ttest.med.func <- 
	function(variable){
		tab <- matrix(ncol=12,nrow=3)
		for(i in 1:12){
		tab[,i] <- 
		rbind(
				t.test(data09[which(data09$med_indicator==1 & data09$months==i),variable], 
							data09[which(data09$med_indicator==0 & data09$months==i),variable])$estimate[[1]][1],
				t.test(data09[which(data09$med_indicator==1 & data09$months==i),variable], 
							data09[which(data09$med_indicator==0 & data09$months==i),variable])$estimate[[2]][1],
				t.test(data09[which(data09$med_indicator==1 & data09$months==i),variable], 
							data09[which(data09$med_indicator==0 & data09$months==i),variable])$p.value
				)
		}
		rownames(tab) <- c("Med Use","No Med","P-Value")
		colnames(tab) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
		return(tab)
	}	

ttest.anymed.func <- 
	function(variable,mon){
		tab <- matrix(ncol=3,nrow=length(med.list))
		for(i in 1:length(med.list)){
		medication <- med.list[i]
		dat0 <- data09[which(data09[,medication]==0 & data09$months==mon),]
		dat1 <- data09[which(data09[,medication]==1 & data09$months==mon),]
		tab[i,] <- 
		cbind(
				t.test(dat1[,variable], dat0[,variable])$estimate[[1]][1],
				t.test(dat1[,variable], dat0[,variable])$estimate[[2]][1],
				t.test(dat1[,variable], dat0[,variable])$p.value
				)
		}
		colnames(tab) <- c("Yes","No","P-Value")
		rownames(tab) <- med.list.name
		return(tab)
	}		
	
# gender and cumulative weight loss by month
ttest.func("pct_WL_cum")
# gender and cumulative coach meetings by month
ttest.func("note_count_total")
# t-test starting age for gender
t.test(data09[which(data09$gender=="female" & data09$months==0),"age"], data09[which(data09$gender=="male" & data09$months==0),"age"])
# t-test starting bmi for gender
t.test(data09[which(data09$gender=="female" & data09$months==0),"start_bmi"], data09[which(data09$gender=="male" & data09$months==0),"start_bmi"])



# medication use and weight loss by month
ttest.med.func("pct_WL_cum")
# medication use and cumulative coach meetings by month
ttest.med.func("note_count_total")

# antidepressant use and weight loss by month
tab <- matrix(ncol=12,nrow=3)
for(i in 1:12){
tab[,i] <- 
rbind(
		t.test(data09[which(data09$med_antidepressant==1 & data09$months==i),"pct_WL_cum"], 
					data09[which(data09$med_antidepressant==0 & data09$months==i),"pct_WL_cum"])$estimate[[1]][1],
		t.test(data09[which(data09$med_antidepressant==1 & data09$months==i),"pct_WL_cum"], 
					data09[which(data09$med_antidepressant==0 & data09$months==i),"pct_WL_cum"])$estimate[[2]][1],
		t.test(data09[which(data09$med_antidepressant==1 & data09$months==i),"pct_WL_cum"], 
					data09[which(data09$med_antidepressant==0 & data09$months==i),"pct_WL_cum"])$p.value
		)
		}
rownames(tab) <- c("Antidepressant","No Antidepressant","P-Value")
colnames(tab) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
tab


# t-test at month 6 for all meds
med.list <- c("med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
				"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner", "med_indicator")
med.list.name <- c("Blood Pressure", "Antidepressant", "Cholesterol", "Sleep", "Diabetes", "Thyroid", "Acid Reflux", "Vitamins", 
				"Diuretics", "Allergies", "Birth Control", "Asthma", "Aspirin", "Blood Thinners","Any") 
ttest.anymed.func("pct_WL_cum",6)

# t-test at month 12 for all meds
ttest.anymed.func("pct_WL_cum",12)
		

		
		
		
		
		
		
		
		
		


## CHI-SQUARED TESTS ##

# gender and marital status
tbl1 <- table(data09[which(data09$months==0 & data09$gender!="" & data09$marital_status!=""),]$gender, data09[which(data09$months==0 & data09$gender!="" & data09$marital_status!=""),]$marital_status)
tbl2 <- tbl1[c(2,3),c(2,3,4,5,6)]
chisq.test(tbl2)

# gender and marital status
tbl3 <- table(data09[which(data09$months==0 & data09$gender!=""),]$gender, data09[which(data09$months==0 & data09$gender!=""),]$age_grp2)
tbl3 <- table(data09[which(data09$months==0 & data09$gender!=""),]$gender, data09[which(data09$months==0 & data09$gender!=""),]$age_grp)
tbl4 <- tbl3[c(2,3),]
chisq.test(tbl4)

# gender and starting BMI group
tbl5 <- table(data09[which(data09$months==0 & data09$gender!=""),]$gender, data09[which(data09$months==0 & data09$gender!=""),]$start_bmi_cat)
tbl5 <- table(data09[which(data09$months==0 & data09$gender!=""),]$gender, data09[which(data09$months==0 & data09$gender!=""),]$start_bmi_cat2)
tbl6 <- tbl5[c(2,3),]
chisq.test(tbl6)

# age group and starting BMI group
tbl7 <- table(data09[which(data09$months==0),]$age_grp, data09[which(data09$months==0),]$start_bmi_cat2)
chisq.test(tbl7)







## PEARSON'S CORRELATION ##
# correlation between age and bmi
cor.test(data09[which(data09$months==0),]$age, data09[which(data09$months==0),]$start_bmi, na.action="na.omit", method="pearson")$estimate #Pearson's correlation coefficient	
cor.test(data09[which(data09$months==0),]$age, data09[which(data09$months==0),]$start_bmi, na.action="na.omit", method="pearson")$statistic #t-statistic
cor.test(data09[which(data09$months==0),]$age, data09[which(data09$months==0),]$start_bmi, na.action="na.omit", method="pearson")$p.value #P-value 	



























































##### made up graph for slope and cumulative of wl line
mo <- c(1:6,1:6)
ID <- c(rep(1,6),rep(2,6))
wl1 <- c(-0.75,-0.8,-0.85,-0.9,-0.95,-1) 
wl2 <- c(-3.5,-3,-2.5,-2,-1.5,-1) 
wl <- c(wl1,wl2)

d1 <- as.data.frame(cbind(mo,ID,wl))
d1$ID <- as.factor(d1$ID)
d1

#slope
png('slope.png',width=8, height=5, units='in', res=300)
ggplot(d1, aes(x=mo, y=wl, color=ID)) +
	geom_line(size=2, aes(color=ID, group=ID)) +	
	geom_point(size=2, shape=21, fill="white") +
	xlab("Months in Program") +	
	ylab("Monthly Percentage Weight Loss") + 
	scale_x_continuous(breaks = seq(0, 6, 1)) +
	scale_y_continuous(breaks = seq(-5,0,0.5), limit=c(-4,0)) +
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


#cumulative
d2 <- ddply(d1, .(ID), transform, wl_cum = cumsum(wl))
wl_all <- c(d2$wl, d2$wl_cum)

d3 <- as.data.frame(cbind(rep(1:6,4), c(rep(1,6),rep(2,6),rep(3,6),rep(4,6)), wl_all))
colnames(d3)[1:2] <- c("mo","ID")
d3$ID <- as.factor(d3$ID)

png('C:/Users/Valerie/Google Drive/SDSU/Dissertation/Profile/JointModel/images/cum.png',width=8, height=5, units='in', res=300)
ggplot(d3, aes(x=mo, y=wl_all, color=ID)) +
	geom_line(size=2, aes(color=ID, group=ID)) +	
	geom_point(size=2, shape=21, fill="white") +
	scale_x_continuous(breaks = seq(0, 6, 1)) +
	scale_y_continuous(breaks = seq(-35,0,5)) + 
	xlab("Months in Program") +	
	ylab("Percentage Weight Loss") + 
	scale_color_manual(values=c("#F8766D", "#00BFC4","#FBB4AE", "#99D8C9"), name="", labels=c("ID=1 - Monthly","ID=2 - Monthly","ID=1 - Cumulative","ID=2 - Cumulative")) +
	theme(
		axis.text = element_text(size = 16),
		axis.title = element_text(size = 18),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "grey90"),
		legend.text = element_text(size = 16),
		legend.title = element_text(size = 16),
		legend.position="bottom"
	)
dev.off()




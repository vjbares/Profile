setwd(...)


###########################################################################################

# load libraries
source("Libraries.R")


###########################################################################################

## to read data in:

userinfo <- read.csv("userinfo.csv.gz")

userinfo_notes <- read.csv("userinfo_notes.csv.gz")

device_weight <- read.csv("device_weight.csv.gz")

device_circ <- read.csv("device_circ.csv.gz")							

activity <- read.csv("activity.csv.gz")
activity_intensity <- read.csv("activity_intensity.csv")
activity_type <- read.csv("activity_intensity.csv")

food <- read.csv("food_tag_log.csv.gz")

meds <- read.csv("user_medications.csv.gz")

plans <- read.csv("plans.csv.gz")		

					
###########################################################################################

## Create a monthly row for every month after their start date
dates <- as.data.frame(seq(from=as.Date('2014-05-01'), to=as.Date('2016-05-01'), by="month"), optional=TRUE)
colnames(dates)<-"dates"


###########################################################################################

# WEIGHT RECORDINGS

device_weight$yearmo <- substr(device_weight$date_recorded, 1, 7)
device_weight$date_recorded <- substr(device_weight$date_recorded, 1, 10)

## grab only weights that are between 100 and 1000 
clean_weight <- sqldf("SELECT user_id, weight, yearmo, date_recorded FROM device_weight WHERE weight>100 AND weight<1000 AND user_id != 0")
##  also take out daily duplicates
clean_weight <-  clean_weight[!duplicated(clean_weight), ]


# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################

# ### CHECK IF EACH MEMBERS MONTHLY WEIGHT MEASUREMENTS ARE NORMALLY DISTRIBUTED ### 

# #number of repeated rows of the same weight for the same person in the same month
# repe <- ddply(clean_weight,.(user_id,yearmo,weight),nrow)
# #only those that have three or more that are the same since the shapiro.test will only look at three or more anyway
# repe3 <- repe[which(repe$V1>=3),]
# #number of weight measurements for each person in that month
# repe1 <- ddply(clean_weight,.(user_id,yearmo),nrow)
# #combine these two
# repe2 <- merge(x=repe3, y=repe1, by=c("user_id","yearmo"), all.x=TRUE, all.y=FALSE) 
# repe2 <- repe2[order(repe2$user_id,repe2$yearmo),]
# #create variable that will tell me if all of the measurements in that month are repeated or not
# repe2$z <- repe2$V1.x / repe2$V1.y
# #only those that have repeated 100%
# repe4 <- repe2[which(repe2$z>=1),]
# repe4
# nrow(repe4) #remove these 74
# #remove the rows that were discovered above
# clean_weight2 <- clean_weight[-which(clean_weight$user_id %in% repe4$user_id & clean_weight$yearmo %in% repe4$yearmo),]

# # count by id and month
# temp1a <- summaryBy(weight ~ user_id + yearmo, data=clean_weight2, FUN=c(length))
# colnames(temp1a)[3] <- "N"

# #shapiro.test only takes those that have three or more observations
# temp2a <- temp1a[which(temp1a$N>=3),]

# #use apply function to run shapiro-wilks test of normality for each member's monthly weight measurements - TOOK 3+ HOURS TO RUN
# temp3 <- sapply(1:nrow(temp2a), 
					# function(i) shapiro.test(clean_weight2[which(clean_weight2[,"user_id"] == temp2a[i,"user_id"] & clean_weight2[,"yearmo"] == temp2a[i,"yearmo"]),"weight"])$p.value
					# )					

# #percentage of individuals that monthly weight measurements are normally distributed - %86
# length(temp3[temp3>0.05]) / length(temp3)
# hist(temp3, breaks=20)
# abline(v=0.05,col="red")
# length(temp3[temp3<=0.05]) / length(temp3) #14% of the cases we reject the null hypothesis that the distributions are normal - we fail to reject normality assumption 86% of the time
# summary(temp3)


# #CHECKS
# #normal
# temp3[10004]
# temp2a[10004,]
# #
# clean_weight2[which(clean_weight2$user_id==4749 & clean_weight2$yearmo=='2016-01'),]
# hist(clean_weight2[which(clean_weight2$user_id==4749 & clean_weight2$yearmo=='2016-01'),'weight'])
# #
# clean_weight2[which(clean_weight2$user_id==4751 & clean_weight2$yearmo=='2013-06'),]
# hist(clean_weight2[which(clean_weight2$user_id==4751 & clean_weight2$yearmo=='2013-06'),'weight'])
# ##
# #not normal
# temp2a[57,]
# temp3[57]
# clean_weight2[which(clean_weight2$user_id==2499 & clean_weight2$yearmo=='2016-05'),]
# hist(clean_weight2[which(clean_weight2$user_id==2499 & clean_weight2$yearmo=='2016-05'),'weight'])


# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################







## GRAB THE MEDIAN WEIGHT FOR EACH MEMBER BY MONTH ##
med <- summaryBy(weight ~ user_id + yearmo, data = clean_weight, FUN = c(median,length))
med$date <- as.Date(paste0(med$yearmo,"-01"))
med <- med[,c(1,3,4,5)]
colnames(med)[c(2,3)] <- c("weight","MonthlyRecordings")

## Get starting date by first recorded weight
	## grab the first recorded weight for each user id (subquery) and join it 
	##  onto the userinfo table which is subset by the WHERE criterion			
first_weight <- sqldf("	SELECT a.id, a.zip, a.email, a.employer, a.occupation
								, a.birthdate, a.marital_status, a.gender, a.height, a.goal_weight, a.or_coach_id
								, a.homestore
								, b.weight as first_weight, b.first_date
								, date(b.first_date,'start of month') AS start
						FROM userinfo a 
							INNER JOIN (SELECT user_id, weight, min(date_recorded) as first_date 
										FROM clean_weight
										GROUP BY user_id) b 
								ON a.id=b.user_id 
						WHERE a.is_verified=1 AND a.is_coach=0 AND a.is_deleted=0 
								AND b.first_date >= '2014-05-01'")
								
first_weight$start_date <- as.Date(first_weight$start)

# each user from the "first_weight" table has a row for every month after they start
# contains all static data					
members <- sqldf("SELECT a.*, b.dates
					FROM first_weight a, dates b
					WHERE  a.start_date <= b.dates")


## append weight recordings for each month if there is one
weight01 <- sqldf("SELECT a.*, b.weight, b.MonthlyRecordings
					FROM members a LEFT JOIN med b ON a.id=b.user_id AND a.dates=b.date")

## create percentage weight loss					
weight01$pct_WL_cum <- ((weight01$weight - weight01$first_weight) / weight01$first_weight)*100

## order by id then dates
weight02 <- weight01[order(weight01$id, weight01$dates),]
## create column that contains the previous recorded weight
weight02 <- slide(data=weight02, Var="weight", slideBy=-1, NewVar="prev_weight", GroupVar="id")


## add months, age, monthly pct weight loss variables
weight02$months <- as.integer(round((as.yearmon(weight02$dates) - as.yearmon(weight02$start_date)) * 12,0))
weight02$age <- floor(as.yearmon(weight02$dates) - as.yearmon(weight02$birthdate))
weight02$pct_WL_nocum <- with(weight02, ifelse(months==0, weight02$pct_WL_cum, ((weight - prev_weight) / first_weight)*100))
weight02$pct_WL_mo <- with(weight02, ifelse(months==0, pct_WL_cum, ((weight - prev_weight) / prev_weight)*100))

weight02$MonthlyRecordings <- ifelse(is.na(weight02$MonthlyRecordings)==1, 0, weight02$MonthlyRecordings)

data01 <- weight02

rm(clean_weight,med,first_weight,weight01,weight02)


###########################################################################################


# NOTES/ COACH MEETINGS

## subset data
###  is_deleted==0
###  is_alert==0
###  removed for_date==0000-00-00 
###  Only dates after May 2014 and before "today"
#### took the actual notes out, not really relevant for what I'm doing
notes01 <- sqldf("SELECT userinfo_id, created_by, for_date
						, date(for_date,'start of month') AS note_month
					FROM userinfo_notes
					WHERE is_deleted=0 AND is_alert=0 
							AND for_date >= '2014-05-01' AND for_date <= date('now')")

## remove duplicate rows from notes01
notes02 <- notes01[!duplicated(notes01),]

## obtain number of monthly notes recorded from userinfo_notes table
notes03 <- sqldf("	SELECT userinfo_id, note_month, count(*) AS note_count
					FROM notes02
					GROUP BY userinfo_id, note_month")	
					
notes03$note_month <- as.Date(notes03$note_month)

## append number of monthly notes to monthly weight recordings table
## remove rows that exist before their start month
data02 <- sqldf("SELECT a.*, b.note_month
						, CASE WHEN b.note_count IS NULL then 0 ELSE b.note_count END AS note_count
						FROM data01 a LEFT JOIN notes03 b ON a.id=b.userinfo_id AND a.dates=b.note_month")

rm(notes01,notes02,notes03)		


###########################################################################################

## waist and hip data

#create a month column and truncate time
device_circ <- sqldf( "SELECT user_id, part, measurement, from_device
							, substr(date_recorded, 1, 10) AS date_recorded
							, date(date_recorded,'start of month') AS circ_month
						FROM device_circ
						WHERE date_recorded >= '2014-05-01' AND date_recorded <= date('now')")

						
#only thigh measurements
thigh <- device_circ[which(device_circ$part=="thigh"),c(1,3,5,6)]
#only hip measurements
hip <- device_circ[which(device_circ$part=="hip"),c(1,3,5,6)]
#only waist measurements
waist <- device_circ[which(device_circ$part=="waist"),c(1,3,5,6)]
#only bicep measurements
bicep <- device_circ[which(device_circ$part=="bicep"),c(1,3,5,6)]
#only neck measurements
neck <- device_circ[which(device_circ$part=="neck"),c(1,3,5,6)]

#remove duplicates 
thigh01 <- thigh[!duplicated(thigh),]
hip01 <- hip[!duplicated(hip),]
waist01 <- waist[!duplicated(waist),]
bicep01 <- bicep[!duplicated(bicep),]
neck01 <- neck[!duplicated(neck),]

## GRAB THE MEDIAN WAIST and HIP FOR EACH MEMBER BY MONTH ##
thigh02 <- summaryBy(measurement ~ user_id + circ_month, data = thigh01, FUN = c(median,length), var.names='thigh', fun.names=c('median','N'))
hip02 <- summaryBy(measurement ~ user_id + circ_month, data = hip01, FUN = c(median,length), var.names='hip', fun.names=c('median','N'))
waist02 <- summaryBy(measurement ~ user_id + circ_month, data = waist01, FUN = c(median,length), var.names='waist', fun.names=c('median','N'))
bicep02 <- summaryBy(measurement ~ user_id + circ_month, data = bicep01, FUN = c(median,length), var.names='bicep', fun.names=c('median','N'))
neck02 <- summaryBy(measurement ~ user_id + circ_month, data = neck01, FUN = c(median,length), var.names='neck', fun.names=c('median','N'))

## Combine hip and waist data by user_id
# circ01 <- merge(waist02, hip02, by=c("user_id","circ_month"), all.x=TRUE, all.y=TRUE)
## Combine all data by user_id
circ01 <- merge(merge(merge(merge(thigh02, hip02, by=c("user_id","circ_month"), all.x=TRUE, all.y=TRUE), waist02, by=c("user_id","circ_month"), all.x=TRUE, all.y=TRUE), bicep02, by=c("user_id","circ_month"), all.x=TRUE, all.y=TRUE), neck02, by=c("user_id","circ_month"), all.x=TRUE, all.y=TRUE)

## add waist to hip ratio
circ01$WHR <- circ01$waist.median / circ01$hip.median

## append waist and hip data to full data set
data03 <- merge(data02, circ01, by.x=c("id","dates"), by.y=c("user_id","circ_month"), all.x=TRUE )

## add obesity according to WHR ( from WHO standards )
data03$WHR_obese <- ifelse(data03$gender=="male" & data03$WHR > 0.9, 1, ifelse(data03$gender=="female" & data03$WHR > 0.85, 1, 0) )

rm(hip,hip01,hip02,waist,waist01,waist02,circ01)


###########################################################################################

## activity data

#combine data
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

#remove duplicates 
activity02 <- activity01[!duplicated(activity01),]

## total minutes of activity
activity03 <- cast(activity02, user_id + activity_month ~ activity_intensity, value="duration", c(sum,length), margins="grand_col")
colnames(activity03) <- c("user_id", "activity_month", "ExtraActive.TotalMinutes", "ExtraActive.N", "LightActivity.TotalMinutes", "LightActivity.N"
									, "Moderate.TotalMinutes", "Moderate.N", "Sedentary.TotalMinutes", "Sedentary.N", "VeryActive.TotalMinutes", "VeryActive.N"
									, "AllActivityTotalMinutes", "AllActivityN")
						
## append activity data to full data set
data04 <-  merge(data03, activity03[,c("user_id","activity_month","AllActivityTotalMinutes","AllActivityN")], by.x=c("id","dates"), by.y=c("user_id","activity_month"), all.x=TRUE )
data04$AllActivityTotalMinutes <- ifelse(is.na(data04$AllActivityTotalMinutes)==1, 0, data04$AllActivityTotalMinutes)
data04$AllActivityN <- ifelse(is.na(data04$AllActivityN)==1, 0, data04$AllActivityN)

rm(activity01,activity02,activity03)


###########################################################################################

## FOOD DATA

#remove duplicates 
food01 <- food[!duplicated(food),] #no duplicates

# remove date_added and _modified
food02 <- food01[,1:5]
food02$month <- as.character(as.Date(paste0(substr(as.Date(food02$date),1,7),'-01'), format = "%Y-%m-%d"))

#count number of logs per month
food03 <- summaryBy(date ~ user_id + month, data=food02, FUN=c(length))
colnames(food03)[3] <- "Food_Logged"

## append food data to full data set
data05 <-  merge(data04, food03, by.x=c("id","dates"), by.y=c("user_id","month"), all.x=TRUE )
data05$Food_Logged <- ifelse(is.na(data05$Food_Logged)==1, 0, data05$Food_Logged)

rm(food01,food02,food03)


###########################################################################################

## add prescription data

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

meds02 <- summaryBy(med_blood_pressure + med_antidepressant + med_cholesterol + med_sleep + med_diabetes + med_thyroid + med_acid_reflux + med_vitamin + med_diuretics + med_allergies + med_birthcontrol + med_asthma + med_aspirin + med_bloodthinner ~ user_id, data=meds01, FUN=sum)
colnames(meds02)[2:15] <- c("med_blood_pressure", "med_antidepressant", "med_cholesterol", "med_sleep", "med_diabetes", "med_thyroid", "med_acid_reflux", "med_vitamin",
							"med_diuretics", "med_allergies", "med_birthcontrol", "med_asthma", "med_aspirin", "med_bloodthinner")

## append med to full data set
data06 <-  merge(data05, meds02, by.x=c("id"), by.y=c("user_id"), all.x=TRUE )

data06$med_blood_pressure <- ifelse(is.na(data06$med_blood_pressure)==1, 0, ifelse(data06$med_blood_pressure>0, 1, 0))
data06$med_antidepressant <- ifelse(is.na(data06$med_antidepressant)==1, 0, ifelse(data06$med_antidepressant>0, 1, 0))
data06$med_cholesterol <- ifelse(is.na(data06$med_cholesterol)==1, 0, ifelse(data06$med_cholesterol>0, 1, 0))
data06$med_sleep <- ifelse(is.na(data06$med_sleep)==1, 0, ifelse(data06$med_sleep>0, 1, 0))
data06$med_diabetes <- ifelse(is.na(data06$med_diabetes)==1, 0, ifelse(data06$med_diabetes>0, 1, 0))
data06$med_thyroid <- ifelse(is.na(data06$med_thyroid)==1, 0, ifelse(data06$med_thyroid>0, 1, 0))
data06$med_acid_reflux <- ifelse(is.na(data06$med_acid_reflux)==1, 0, ifelse(data06$med_acid_reflux>0, 1, 0))
data06$med_vitamin <- ifelse(is.na(data06$med_vitamin)==1, 0, ifelse(data06$med_vitamin>0, 1, 0))
data06$med_diuretics <- ifelse(is.na(data06$med_diuretics)==1, 0, ifelse(data06$med_diuretics>0, 1, 0))
data06$med_allergies <- ifelse(is.na(data06$med_allergies)==1, 0, ifelse(data06$med_allergies>0, 1, 0))
data06$med_birthcontrol <- ifelse(is.na(data06$med_birthcontrol)==1, 0, ifelse(data06$med_birthcontrol>0, 1, 0))
data06$med_asthma <- ifelse(is.na(data06$med_asthma)==1, 0, ifelse(data06$med_asthma>0, 1, 0))
data06$med_aspirin <- ifelse(is.na(data06$med_aspirin)==1, 0, ifelse(data06$med_aspirin>0, 1, 0))
data06$med_bloodthinner <- ifelse(is.na(data06$med_bloodthinner)==1, 0, ifelse(data06$med_bloodthinner>0, 1, 0))

rm(meds01,meds02)


###########################################################################################

## meal plan data

plans01 <- plans[order(plans$user_id, plans$id),]

plans01$date_started <- as.Date(plans01$date_started, format="%Y-%m-%d")
plans01$date_ended <- as.Date(plans01$date_ended, format="%Y-%m-%d")

plans01 <- plans01[order(plans01$user_id, plans01$date_started),]
plans02 <- slide(data=plans01, Var="date_started", slideBy=1, NewVar="lag_date_started", GroupVar="user_id")

#create new end date based on the start date of the next meal plan 
plans02$date_ended <-  as.Date( ifelse( is.na(plans02$date_ended) & !is.na(plans02$lag_date_started), plans02$lag_date_started,
									ifelse( is.na(plans02$date_ended) & is.na(plans02$lag_date_started), as.Date(max(as.Date(plans02$date_started),na.rm=TRUE) +1), plans02$date_ended)),  origin = "1970-01-01")
#format start date
plans02$date_started <- as.character(as.Date(as.character(plans02$date_started), format = "%Y-%m-%d"))

#remove user_id=1, only those plans with an actual name, and after May 2014
#remove some variables
#create a "month" variable for the month that plan start and create variable with the last day of that month
plans03 <- sqldf("SELECT user_id, template_name
						, calories, protien AS protein, carbs, fiber, activity_level AS plan_activity_level
						, date_started, date_ended
						, date(date_started,'start of month') AS month_started
						, date(date_started,'start of month','+1 month','-1 day') AS month_end 
					FROM plans02
					WHERE user_id != 0 AND is_template==0 AND template_name IS NOT NULL AND date(date_started,'start of month') >= '2014-05-01'")

#group meal plans into groups
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

#format variable as a date							
plans03$date_started <- as.Date(plans03$date_started)

#number of days the plan was used based on start and end dates
plans03$plan_days <- substr((plans03$date_ended - plans03$date_started), 1, nchar(plans03$date_ended - plans03$date_started))
plans03$plan_days <- as.numeric(plans03$plan_days)

#only grab the plans that were used -  more than 0 days
plans04 <- plans03[which(plans03$plan_days > 0),] #60584 0's removed

#remove duplicate rows - 139 rows removed
plans05 <- plans04[!duplicated(plans04),]

#format date variables
plans05$month_started <- as.Date(plans05$month_started)   #first day of the month
plans05$month_end <- as.Date(plans05$month_end) #last day of the month

#dates data from earlier - all start of the month dates from may 2014 to may 2016 - format dates as character for joining purposes
dates$dates <- as.character(as.Date(dates$dates, format="%Y-%m-%d"))

#add an end of the month variable to dates data
dates2 <- sqldf("SELECT dates, date(dates,'start of month','+1 month','-1 day') AS dates_end FROM dates")

#format variables - seems unnecessary to have above step then?
dates2$dates <- as.Date(dates2$dates)         #current month
dates2$dates_end <- as.Date(dates2$dates_end) #last day of the current month

#every plan has an associated month to it
#if a plan was used during a month there is a row added for that month
#several rows added to this data
plans06 <- sqldf("SELECT a.*, b.dates, b.dates_end
					FROM plans05 a, dates2 b
					WHERE a.month_started<=b.dates AND a.date_ended>=b.dates")
# now month_started == dates AND month_end == dates_end					

#(1)if the plans start date is before the start of the current month & the end date is after the end of the current month then  
	#diff= (last day of the starting month) - (the first day of the month)
	#diff= the number of days in the current month (30 or 31 days)

#(2)if the plans start date is before the start of the current month & the plans end date is within the current month then 
	#diff= (last day of the plan) - (first day of the current month)
	#diff= number of days from teh start of the month until plan ended

#(3) if the plan started in the current month & the end date is after the end of the current month then
	#diff= (last day of the current month) - (the plans end date) 
	#diff= the number of days betwen when the plan started and the end of the month

#(4)otherwise (the plan started and ended in the same month)
	#diff= (the plans end date) - (the plans start date)
	#diff= the nuember of days between the start and end date of the plan
# plans06$diff <- ifelse( plans06$date_started>plans06$dates, plans06$month_end-plans06$date_started, 
					# ifelse( plans06$dates_end<plans06$date_ended, plans06$dates_end-plans06$dates, plans06$date_ended-plans06$dates))

plans06$diff <- ifelse( plans06$date_started<plans06$dates & plans06$date_ended>plans06$dates_end, (plans06$month_end-plans06$month_started)+1, #(1)
					ifelse( plans06$date_started<plans06$dates & plans06$date_ended<=plans06$dates_end, (plans06$date_ended-plans06$dates)+1, #(2)
						ifelse( plans06$date_started>=plans06$dates & plans06$date_ended>plans06$dates_end, (plans06$month_end-plans06$date_started)+1, #(3)
							ifelse( plans06$date_started>=plans06$dates & plans06$date_ended<=plans06$dates_end, (plans06$date_ended-plans06$date_started)+1 #(4)
								, NA)))) 

#remove variables and format 					
plans07 <- plans06[,-which(names(plans06) %in% c("plan_days","dates_end","month_started","month_end"
													,"template_name","calories","protein","carbs","fiber","plan_activity_level"))]
plans07$dates <- as.character(plans07$dates)

#combine plan groups by member by month and add the number of days in that plan group
plans07a <- summaryBy(diff ~ user_id + plan_grp + dates, data=plans07, FUN=sum)
plans07a <- plans07a[order(plans07a$user_id, plans07a$dates),]
colnames(plans07a)[4] <- "diff"

#if multiple plans per month choose the one that was used the most number of days
plans08 <- sqldf("SELECT b.*
				FROM (SELECT user_id, dates, max(diff) AS maxdiff
						FROM plans07a
						GROUP BY user_id, dates) a LEFT JOIN plans07a b ON a.user_id=b.user_id AND a.dates=b.dates AND a.maxdiff=b.diff")

#look for duplicates - don't have starting date so can't order by that - random choice if tied?
plans09 <- plans08[!duplicated(plans08[, c("user_id","dates")]),]

## append plans10 to full data set
plans09$dates <- as.Date(plans09$dates)
data07 <- sqldf("SELECT a.*, b.plan_grp
					FROM data06 a LEFT JOIN plans09 b ON a.id==b.user_id AND a.dates=b.dates")
					
rm(plans01,plans02,plans03,plans04,plans05,plans06,plans07,plans08,plans09)




###########################################################################################
###########################################################################################

## CLEAN UP DATA 

data08 <- data07

#default birthdate is 1/1/1970 so make this NA since we don't know their actual birthdate
data08$age <- ifelse(data08$birthdate=='1970-01-01', NA, data08$age)

## CREATE "VALID" VARIABLE
	# cumulative percentage of weight loss is less than 60 or is missing
		# Some of the first recorded weights are incorrect so their cumulative is incorrect for each month
	# age between 18 and 90 or is missing
	# monthly weight loss is less than 15 or is missing
	# not mom protocol or teen meal plans - trying to gain weight
data08$valid <- ifelse((abs(data08$pct_WL_cum)<60 | is.na(data08$pct_WL_cum)==1) & 
						((data08$age>=18 & data08$age<=90) | is.na(data08$age)==1) &
						(abs(data08$pct_WL_mo) < 15 | is.na(data08$pct_WL_mo)==1) &
						(data08$plan_grp != "Mom Protocol" & data08$plan_grp != "Teen" | is.na(data08$plan_grp)==1), 1, 0)

# CREATE "ACTIVE" VARIABLE
#### INCLUDES THEIR STARTING MONTH AND ONLY WHERE THEY HAVE A RECORDED WEIGHT THAT MONTH AND THE PREVIOUS MONTH
data08$active <- ifelse(data08$months==0 | (is.na(data08$weight)==0 & is.na(data08$prev_weight)==0), 1, 0)
#active in that month
data08$active_mo <- with(data08, ifelse(MonthlyRecordings==0 & note_count==0 & AllActivityN==0 & Food_Logged==0 , 0, 1))


## CREATE bmi VARIABLES
data08$bmi <- (data08$weight / (data08$height^2) ) * 703
data08$start_bmi <- (data08$first_weight / (data08$height^2) ) * 703


# create cumulative coach meetings
data08 <- data.table(data08)
data08 <- data08[order(data08$id, data08$months),]
data08 <- data08[, note_count_total := cumsum(note_count), by="id"]
data08 <- as.data.frame(data08)

data08$success10pct <- with(data08, ifelse(pct_WL_cum<=(-10), 1, 0))


# create previous variables 
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="pct_WL_cum", slideBy=-1, NewVar="prev_pct_WL_cum", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="pct_WL_mo", slideBy=-1, NewVar="prev_pct_WL_mo", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="plan_grp", slideBy=-1, NewVar="prev_plan_grp", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="MonthlyRecordings", slideBy=-1, NewVar="prev_MonthlyRecordings", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="note_count", slideBy=-1, NewVar="prev_note_count", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="bmi", slideBy=-1, NewVar="prev_bmi", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="AllActivityN", slideBy=-1, NewVar="prev_AllActivityN", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="Food_Logged", slideBy=-1, NewVar="prev_Food_Logged", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="note_count_total", slideBy=-1, NewVar="prev_note_count_total", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="waist.median", slideBy=-1, NewVar="prev_waist_median", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="waist.N", slideBy=-1, NewVar="prev_waist_N", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="hip.median", slideBy=-1, NewVar="prev_hip_median", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="hip.N", slideBy=-1, NewVar="prev_hip_N", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="WHR", slideBy=-1, NewVar="prev_WHR", GroupVar="id")
#
data08 <- data08[order(data08$id, data08$months),]
data08 <- slide(data=data08, Var="WHR_obese", slideBy=-1, NewVar="prev_WHR_obese", GroupVar="id")
#


#previous BMI categories
data08$prev_bmi_cat <- with(data08,
			ifelse(prev_bmi >= 40, "obeseIII", 
				ifelse(prev_bmi >= 35, "obeseII",
					ifelse(prev_bmi >= 30, "obeseI",
							ifelse(prev_bmi >= 25 & prev_bmi < 30, "overweight", "healthy_under")))))
data08$prev_bmi_cat2 <- with(data08,
					ifelse(prev_bmi >= 30, "obese",
							ifelse(prev_bmi >= 25 & prev_bmi < 30, "overweight", "healthy_under")))

							
#weight loss last month
data08$prev_WL <- (data08$first_weight - data08$prev_weight)	




						
# some of the previous variables have NA's - these can be converted to 0's 
data08[,c("prev_MonthlyRecordings", "prev_note_count", "prev_AllActivityN", "prev_Food_Logged", "prev_waist_N", "prev_hip_N")] <- apply(data08[,c("prev_MonthlyRecordings", "prev_note_count", "prev_AllActivityN", "prev_Food_Logged", "prev_waist_N", "prev_hip_N")], 2, function(x){replace(x, is.na(x), 0)})



###########################################################################################
## ADD ADDITIONAL MEDICAL VARIABLE ##

med_cnt <- summaryBy(medication ~ user_id, meds01, FUN=length)
colnames(med_cnt)[2] <- "med_total"

data08 <- merge(data08, med_cnt, by.x=c("id"), by.y=c("user_id"), all.x=TRUE )

data08$med_indicator <- ifelse(is.na(data08$med_total)==0, 1, 0)


###########################################################################################


write.csv(data08, "data08.csv", row.names=FALSE)
system("gzip data08.csv")


###########################################################################################

## REMOVE ALL ROWS OF ANY IDs THAT EVER HAD AN INVALID ROW ##

#only those IDs with VALID IDs
id_invalid <- unique(data08[which(data08$valid==0),"id"])

#no outliers, out of age range, etc.
temp1 <- data08[-which(data08$id %in% id_invalid),]

#create data09 to save
data09 <- temp1[order(temp1$id, temp1$months),]



###########################################################################################


write.csv(data09, "data09.csv", row.names=FALSE)
system("gzip data09.csv")


###########################################################################################


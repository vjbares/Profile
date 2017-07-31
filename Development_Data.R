
##### development data will be month 1 june 2014 to may 2015 -- through month 12 to april 2016 #####
##### hold out data will be  May of 2016    #####
setwd(...)

# load libraries
source("libraries.R")

# read data in
data09 <- read.csv("data09.csv.gz")


#only those IDs from 6/2014 through 5/2015
uid <- unique(data09[which(as.Date(data09$dates)>="2014-06-01" & as.Date(data09$dates)<="2015-05-01" & data09$months==1),]$id)
tmp_data09 <- data09[which(data09$id %in% uid & as.Date(data09$dates)<="2016-04-01"),]


# table(as.Date(data09$dates))
#grab everyone from month 1-12 and only those that start as overweight or obese 
#complete profiles
#already only valid IDs
temp0 <- tmp_data09[which(tmp_data09$months>0 & tmp_data09$months<=12 & tmp_data09$start_bmi>=25 & tmp_data09$gender!="" & is.na(tmp_data09$age)==0),] 	


table(temp0$months, temp0$dates)



#add and remove variables
temp0a <- temp0[, c("id","dates","months"
						,"gender","age","marital_status"
						,"first_weight","first_date","start_bmi","weight","note_count","note_count_total","MonthlyRecordings","plan_grp"
						,"med_total","med_indicator","med_blood_pressure","med_antidepressant","med_cholesterol","med_sleep","med_diabetes"
						,"med_thyroid","med_vitamin","med_birthcontrol"
						,"pct_WL_cum", "pct_WL_mo"
						,"prev_weight","prev_pct_WL_cum","prev_pct_WL_mo","prev_WL","prev_bmi","prev_MonthlyRecordings","prev_note_count"
						,"prev_AllActivityN","prev_Food_Logged","prev_note_count_total","prev_waist_N","prev_hip_N"
						,"prev_bmi_cat","prev_bmi_cat2","prev_plan_grp")]

#fix marital status
temp0a$marital_status <- as.character(temp0a$marital_status)
temp0a$marital_status[temp0a$marital_status=="" | is.na(temp0a$marital_status)==1] <- "missing"
temp0a$marital_status <- as.factor(temp0a$marital_status)

#other marital status
temp0a$marital_status2 <- with(temp0a, ifelse(marital_status %in% c("married","partner"),"relationship", ifelse(marital_status %in% c("divorced","single","widowed"),"single","missing")))
temp0a$marital_status2 <- as.factor(temp0a$marital_status2)



#fix med_total
temp0a$med_total <- ifelse(is.na(temp0a$med_total)==1, 0, temp0a$med_total)

#fix prev bmi cat
temp0a$prev_bmi_cat <- as.character(temp0a$prev_bmi_cat)
temp0a$prev_bmi_cat <- ifelse(is.na(temp0a$prev_bmi_cat)==1, "missing", temp0a$prev_bmi_cat)

#fix prev bmi cat2
temp0a$prev_bmi_cat2 <- as.character(temp0a$prev_bmi_cat2)
temp0a$prev_bmi_cat2 <- ifelse(is.na(temp0a$prev_bmi_cat2)==1, "missing", temp0a$prev_bmi_cat2)
 
#add starting month
temp0a$start_month <- month(as.Date(temp0a$first_date))

#add starting season
temp0a$start_season <- with(temp0a, ifelse(start_month %in% c(3,4,5), "Spring"
									, ifelse(start_month %in% c(6,7,8), "Summer"
										, ifelse(start_month %in% c(9,10,11), "Fall"
											, ifelse(start_month %in% c(12,1,2), "Winter", "Missing")))))

#add month of year
temp0a$date_month <- month(as.Date(temp0a$dates))

#add seasonal variable
temp0a$date_season <- with(temp0a, ifelse(date_month %in% c(3,4,5), "Spring"
									, ifelse(date_month %in% c(6,7,8), "Summer"
										, ifelse(date_month %in% c(9,10,11), "Fall"
											, ifelse(date_month %in% c(12,1,2), "Winter", "Missing")))))

#fix gender so there is only two categories
temp0a$gender <- as.factor(as.character(temp0a$gender))


temp0a$prev_plan_grp <- as.factor(ifelse(is.na(temp0a$prev_plan_grp)==1, "Other", as.character(temp0a$prev_plan_grp)))







#average of the previous three months
avg.prev.1 <- function (x) if (length(x) < 2) rep(NA,length(x)) else rollapplyr(x,list(-(1:1)),mean,fill=NA)
avg.prev.2 <- function (x) if (length(x) < 3) rep(NA,length(x)) else rollapplyr(x,list(-(2:1)),mean,fill=NA)
avg.prev.3 <- function (x) if (length(x) < 4) rep(NA,length(x)) else rollapplyr(x,list(-(3:1)),mean,fill=NA)

#rolling average (3 months) of coach meetings
temp0a1 <- temp0a %>% group_by(id) %>% arrange(months) %>% mutate(avg1_note_count=avg.prev.1(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)
temp0a1 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg2_note_count=avg.prev.2(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)
temp0a1 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg3_note_count=avg.prev.3(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)

temp0a1$avg3_note_count <- round(ifelse(temp0a1$months==2, temp0a1$avg1_note_count, 
									ifelse(temp0a1$months==3, temp0a1$avg2_note_count,
										ifelse(temp0a1$months>=4, temp0a1$avg3_note_count, 0))),2)



#rolling average (3 months) of coach meetings
temp0a2 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg1_recording=avg.prev.1(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)
temp0a2 <- temp0a2 %>% group_by(id) %>% arrange(months) %>% mutate(avg2_recording=avg.prev.2(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)
temp0a2 <- temp0a2 %>% group_by(id) %>% arrange(months) %>% mutate(avg3_recording=avg.prev.3(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)

temp0a2$avg3_recording <- round(ifelse(temp0a2$months==2, temp0a2$avg1_recording, 
									ifelse(temp0a2$months==3, temp0a2$avg2_recording,
										ifelse(temp0a2$months>=4, temp0a2$avg3_recording, 0))),2)

temp0a3 <- temp0a2[, -which(names(temp0a2) %in% c("avg1_note_count","avg2_note_count","avg1_recording","avg2_recording"))]







# FIX MISSING #

# function to carry last observation forward
na.locf.na <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...) 

#prev pct wl mo - assume no weight loss last month if missing
temp0b <- temp0a3
temp0b$prev_pct_WL_mo <- ifelse(is.na(temp0b$prev_pct_WL_mo)==1, 0, temp0b$prev_pct_WL_mo)

#prev wl - assume no weight loss last month if missing
temp0c <- temp0b
temp0c$prev_WL <- ifelse(is.na(temp0c$prev_WL)==1, 0, temp0c$prev_WL)

#prev weight - carry last observation forward
temp0d <- transform(temp0c, prev_weight = ave(prev_weight, id, FUN = na.locf.na))

#prev bmi - carry last observation forward
temp0e <- transform(temp0d, prev_bmi = ave(prev_bmi, id, FUN = na.locf.na))

#prev pct wl cum - carry last observation forward
temp0f <- transform(temp0e, prev_pct_WL_cum = ave(prev_pct_WL_cum, id, FUN = na.locf.na))



##   censor if end of dev data - feb 2016  ##
temp0f$censor <- with(temp0f, ifelse(as.Date(temp0f$dates)=="2016-04-01",1,0)) 



#missing if no weight recording and no coach meeting that month
temp0f$miss <- with(temp0f, ifelse(is.na(weight)==1 & note_count==0, 1, 0)) #is.na(weight)==1 does not play a part since later i delete all these anyway

#cumulative sum of dropping out indicator
temp1 <- ddply(temp0f, .(id), transform, s = cumsum(miss))



#eliminate rows after the member has dropped out 
temp2 <- temp1[which( (temp1$s==1 & temp1$miss==1) | (temp1$s==0 & temp1$miss==0) ),]

#not censored if missed
temp2$censor <- ifelse(temp2$censor==1 & temp2$s==1, 0, temp2$censor)


#############################################################################################################
##### temp2 data is all observations up to a miss.  All observations AFTER the miss are deleted.        #####
##### miss and s are the same right now                                                                 #####
##### new members from may 2014  through april 2015 and all observations through february 2016          #####
##### graphs and numbers should be taken from this data frame.                                          #####
##### survival data only contains one row per observation and the longitudinal data removes the missing #####
#############################################################################################################








#create short data - only contains one row per subject

#grab maximum month which will either be dead or alive or alive/censored
maxmonth2  <- summaryBy(months ~ id, data=temp2, FUN=max)

#combine maxmonth2 to with data and only keep the  maximum month row - inner join
temp3 <- merge(temp2, maxmonth2, by.x=c("id","months"), by.y=c("id","months.max"))

#make sure ordered
temp4 <- temp3[order(temp3$id, temp3$months),]

#rename months to month
temp4$month <- temp4$months

#drop vars - other variables were from the last month's observations
temp5 <- temp4[,c("id","month","s","censor")]

## add in baseline covariates
temp6 <- merge(temp5, data09[which(data09$months==0),c("id","dates","gender","age","marital_status","first_weight","first_date","start_bmi","med_total","med_indicator"
															,"med_blood_pressure","med_antidepressant","med_cholesterol","med_sleep","med_diabetes"
															,"med_thyroid","med_vitamin","med_birthcontrol")]
							, by.x=c("id"), by.y=c("id"), all.x=TRUE)
							

#fix marital status
temp6$marital_status <- as.character(temp6$marital_status)
temp6$marital_status[temp6$marital_status=="" | is.na(temp6$marital_status)==1] <- "missing"
temp6$marital_status <- as.factor(temp6$marital_status)

#other marital status
temp6$marital_status2 <- with(temp6, ifelse(marital_status %in% c("married","partner"),"relationship", ifelse(marital_status %in% c("divorced","single","widowed"),"single","missing")))
temp6$marital_status2 <- as.factor(temp6$marital_status2)


#fix med_total
temp6$med_total <- ifelse(is.na(temp6$med_total)==1, 0, temp6$med_total)

#add starting month
temp6$start_month <- month(as.Date(temp6$first_date))

#add starting season
temp6$start_season <- with(temp6, ifelse(start_month %in% c(3,4,5), "Spring"
									, ifelse(start_month %in% c(6,7,8), "Summer"
										, ifelse(start_month %in% c(9,10,11), "Fall"
											, ifelse(start_month %in% c(12,1,2), "Winter", "Missing")))))

#fix gender so there is only two categories
temp6$gender <- as.factor(as.character(temp6$gender))



## found on help page to add a really small number to time... - no idea
temp6$month <- temp6$month + 1.8*10^(-15)














## back to longitudinal data

#need to remove missing target
temp7 <- temp2[which(is.na(temp2$pct_WL_mo)==0),]

#merge temp0a and months from temp13 to get the maximum month before dropping out
temp8 <- merge(temp7[,colnames(temp7) != "censor"], temp6[,c("id","month","s")], by.x=c("id"), by.y=c("id"), all.x=TRUE)

#make sure it's ordered
temp9 <- temp8[order(temp8$id, temp8$months),]


#rename a few variables
colnames(temp9)[colnames(temp9)=="s.x"] <- "s"
colnames(temp9)[colnames(temp9)=="s.y"] <- "event"



#make sure ID is a factor
temp9$id <- as.factor(temp9$id)



































####################################################################################################
####################################################################################################
####################################################################################################

#save development data

#grab unique IDs from longitudinal data
ids <- unique(temp9$id)


#LONGITUDINAL DATA
dat_long <- temp9
write.csv(dat_long, "dat_long_dev.csv", row.names=FALSE)
system("gzip dat_long_dev.csv")



#SURVIVAL DATA
dat_surv <- temp6[which(temp6$id %in% ids),]
write.csv(dat_surv, "dat_surv_dev.csv", row.names=FALSE)
system("gzip dat_surv_dev.csv")




##################################
##development & validation
id <- unique(dat_long$id)
d <- round(length(id)*.7,0)

set.seed(5839)
id_dev <- sample(id, replace=FALSE, size=d)
id_val <- id[!(id %in% id_dev)]
# length(id_dev) + length(id_val) - length(id)


long_dev <- dat_long[which(dat_long$id %in% id_dev),]
long_val <- dat_long[which(dat_long$id %in% id_val),]


surv_dev <- dat_surv[which(dat_surv$id %in% id_dev),]
surv_val <- dat_surv[which(dat_surv$id %in% id_val),]


length(unique(long_dev$id))
length(surv_dev$id)

length(unique(long_val$id))
length(surv_val$id)



#longitudinal development
write.csv(long_dev, "long_dev.csv", row.names=FALSE)
system("gzip long_dev.csv")

#longitudinal validation
write.csv(long_val, "long_val.csv", row.names=FALSE)
system("gzip long_val.csv")



#survival development
write.csv(surv_dev, "surv_dev.csv", row.names=FALSE)
system("gzip surv_dev.csv")

#survival validation
write.csv(surv_val, "surv_val.csv", row.names=FALSE)
system("gzip surv_val.csv")













































## HOLD OUT DATA ##

#only those IDs from 2015-05-01 through 2016-05-01
#if dates are older than this, the members won't reach month 12 in may and they don't matter anyway
uid <- unique(data09[which(as.Date(data09$dates)>=as.Date("2015-05-01") & as.Date(data09$dates)<=as.Date("2016-05-01") & data09$months==1),]$id)
tmp_data09 <- data09[which(data09$id %in% uid & as.Date(data09$dates)<=as.Date("2016-05-01")),]

#grab everyone from month 1-12 and only those that start as overweight or obese 
#complete profiles
#already only valid IDs
temp0 <- tmp_data09[which(tmp_data09$months>=0 & tmp_data09$months<=12 & tmp_data09$start_bmi>=25 & tmp_data09$gender!="" & is.na(tmp_data09$age)==0),] 	

#add and remove variables
temp0a <- temp0[, c("id","dates","months"
						,"gender","age","marital_status"
						,"first_weight","first_date","start_bmi","weight","note_count","note_count_total","MonthlyRecordings","plan_grp"
						,"med_total","med_indicator","med_blood_pressure","med_antidepressant","med_cholesterol","med_sleep","med_diabetes"
						,"med_thyroid","med_vitamin","med_birthcontrol"
						,"pct_WL_cum", "pct_WL_mo"
						,"prev_weight","prev_pct_WL_cum","prev_pct_WL_mo","prev_WL","prev_bmi","prev_MonthlyRecordings","prev_note_count"
						,"prev_AllActivityN","prev_Food_Logged","prev_note_count_total","prev_waist_N","prev_hip_N"
						,"prev_bmi_cat","prev_bmi_cat2","prev_plan_grp")]

#fix marital status
temp0a$marital_status <- as.character(temp0a$marital_status)
temp0a$marital_status[temp0a$marital_status=="" | is.na(temp0a$marital_status)==1] <- "missing"
temp0a$marital_status <- as.factor(temp0a$marital_status)

#other marital status
temp0a$marital_status2 <- with(temp0a, ifelse(marital_status %in% c("married","partner"),"relationship", ifelse(marital_status %in% c("divorced","single","widowed"),"single","missing")))
temp0a$marital_status2 <- as.factor(temp0a$marital_status2)

#fix med_total
temp0a$med_total <- ifelse(is.na(temp0a$med_total)==1, 0, temp0a$med_total)

#fix prev bmi cat
temp0a$prev_bmi_cat <- as.character(temp0a$prev_bmi_cat)
temp0a$prev_bmi_cat <- ifelse(is.na(temp0a$prev_bmi_cat)==1, "missing", temp0a$prev_bmi_cat)

#fix prev bmi cat2
temp0a$prev_bmi_cat2 <- as.character(temp0a$prev_bmi_cat2)
temp0a$prev_bmi_cat2 <- ifelse(is.na(temp0a$prev_bmi_cat2)==1, "missing", temp0a$prev_bmi_cat2)
 
#add starting month
temp0a$start_month <- month(as.Date(temp0a$first_date))

#add starting season
temp0a$start_season <- with(temp0a, ifelse(start_month %in% c(3,4,5), "Spring"
									, ifelse(start_month %in% c(6,7,8), "Summer"
										, ifelse(start_month %in% c(9,10,11), "Fall"
											, ifelse(start_month %in% c(12,1,2), "Winter", "Missing")))))

#add month of year
temp0a$date_month <- month(as.Date(temp0a$dates))

#add seasonal variable
temp0a$date_season <- with(temp0a, ifelse(date_month %in% c(3,4,5), "Spring"
									, ifelse(date_month %in% c(6,7,8), "Summer"
										, ifelse(date_month %in% c(9,10,11), "Fall"
											, ifelse(date_month %in% c(12,1,2), "Winter", "Missing")))))

#fix gender so there is only two categories
temp0a$gender <- as.factor(as.character(temp0a$gender))

temp0a$prev_plan_grp <- as.factor(ifelse(is.na(temp0a$prev_plan_grp)==1, "Other", as.character(temp0a$prev_plan_grp)))

#average of the previous three months
avg.prev.1 <- function (x) if (length(x) < 2) rep(NA,length(x)) else rollapplyr(x,list(-(1:1)),mean,fill=NA)
avg.prev.2 <- function (x) if (length(x) < 3) rep(NA,length(x)) else rollapplyr(x,list(-(2:1)),mean,fill=NA)
avg.prev.3 <- function (x) if (length(x) < 4) rep(NA,length(x)) else rollapplyr(x,list(-(3:1)),mean,fill=NA)

#rolling average (3 months) of coach meetings
temp0a1 <- temp0a %>% group_by(id) %>% arrange(months) %>% mutate(avg1_note_count=avg.prev.1(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)
temp0a1 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg2_note_count=avg.prev.2(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)
temp0a1 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg3_note_count=avg.prev.3(note_count)) %>% ungroup() %>% arrange(id,months)
temp0a1 <- as.data.frame(temp0a1)

temp0a1$avg3_note_count <- round(ifelse(temp0a1$months==2, temp0a1$avg1_note_count, 
									ifelse(temp0a1$months==3, temp0a1$avg2_note_count,
										ifelse(temp0a1$months>=4, temp0a1$avg3_note_count, 0))),2)

#rolling average (3 months) of coach meetings
temp0a2 <- temp0a1 %>% group_by(id) %>% arrange(months) %>% mutate(avg1_recording=avg.prev.1(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)
temp0a2 <- temp0a2 %>% group_by(id) %>% arrange(months) %>% mutate(avg2_recording=avg.prev.2(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)
temp0a2 <- temp0a2 %>% group_by(id) %>% arrange(months) %>% mutate(avg3_recording=avg.prev.3(MonthlyRecordings)) %>% ungroup() %>% arrange(id,months)
temp0a2 <- as.data.frame(temp0a2)

temp0a2$avg3_recording <- round(ifelse(temp0a2$months==2, temp0a2$avg1_recording, 
									ifelse(temp0a2$months==3, temp0a2$avg2_recording,
										ifelse(temp0a2$months>=4, temp0a2$avg3_recording, 0))),2)

temp0a3 <- temp0a2[, -which(names(temp0a2) %in% c("avg1_note_count","avg2_note_count","avg1_recording","avg2_recording"))]


# FIX MISSING #
# function to carry last observation forward
na.locf.na <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...) 

#prev pct wl mo - assume no weight loss last month if missing
temp0b <- temp0a3
temp0b$prev_pct_WL_mo <- ifelse(is.na(temp0b$prev_pct_WL_mo)==1, 0, temp0b$prev_pct_WL_mo)

#prev wl - assume no weight loss last month if missing
temp0c <- temp0b
temp0c$prev_WL <- ifelse(is.na(temp0c$prev_WL)==1, 0, temp0c$prev_WL)

#prev weight - carry last observation forward
temp0d <- transform(temp0c, prev_weight = ave(prev_weight, id, FUN = na.locf.na))

#prev bmi - carry last observation forward
temp0e <- transform(temp0d, prev_bmi = ave(prev_bmi, id, FUN = na.locf.na))

#prev pct wl cum - carry last observation forward
temp0f <- transform(temp0e, prev_pct_WL_cum = ave(prev_pct_WL_cum, id, FUN = na.locf.na))


#missing if no weight recording and no coach meeting that month
temp0f$miss <- with(temp0f, ifelse(is.na(weight)==1 & note_count==0, 1, 0))

# #only include those that are not missing
# temp1 <- temp0f[which(temp0f$miss==0),]
temp1 <- ddply(temp0f, .(id), transform, s = cumsum(miss))

#eliminate rows after the member has dropped out 
temp2 <- temp1[which( (temp1$s==1 & temp1$miss==1) | (temp1$s==0 & temp1$miss==0) ),]

#make sure it's ordered
temp2 <- temp2[order(temp2$id, temp2$months),]

#make sure ID is a factor
temp2$id <- as.factor(temp2$id)

dat_may <- temp2 



#extract unique IDs for those that are active in the last month of the apply data
uid_may <- unique(dat_may[which(as.Date(dat_may$dates)=="2016-04-01" & dat_may$miss==0),]$id)


dat_val_may <- dat_may[which(dat_may$id %in% uid_may),]


write.csv(dat_val_may, "dat_val_may.csv", row.names=FALSE)
system("gzip dat_val_may.csv")

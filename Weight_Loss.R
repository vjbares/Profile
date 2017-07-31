setwd(...)

# load libraries
source("Libraries.R")


# read data in
data09 <- read.csv("data09.csv.gz")











########################################
########################################
##########      6 MONTHS      ##########
########################################
########################################
# pull IDs for each month separately for those that have a weight measurement
tmp6 <- data09[which(data09$months==6 & is.na(data09$weight)==0),]
tmp5 <- as.data.frame(data09[which(data09$months==5 & is.na(data09$weight)==0),c("id","weight")]); colnames(tmp5) <- c("id","5")
tmp4 <- as.data.frame(data09[which(data09$months==4 & is.na(data09$weight)==0),c("id","weight")]); colnames(tmp4) <- c("id","4")
tmp3 <- as.data.frame(data09[which(data09$months==3 & is.na(data09$weight)==0),c("id","weight")]); colnames(tmp3) <- c("id","3")
tmp2 <- as.data.frame(data09[which(data09$months==2 & is.na(data09$weight)==0),c("id","weight")]); colnames(tmp2) <- c("id","2")
tmp1 <- as.data.frame(data09[which(data09$months==1 & is.na(data09$weight)==0),c("id","weight")]); colnames(tmp1) <- c("id","1")
#merge together with only month 6 information
dat01 <- merge(tmp1, merge(tmp2, merge(tmp3, merge(tmp4, merge(tmp5, tmp6, by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id")
dat01[,"6"] <- dat01$weight
dat02 <- dat01[which(dat01$gender != "" & is.na(dat01$age)==0 & dat01$start_bmi>=25 & dat01$marital_status!="" & is.na(dat01$plan_grp)==0 & dat01$plan_grp %in% c("Balance","Reboot Adapt","Reboot Reduce","Sustain")),]
#make missing marital status "missing"
dat02$marital_status <- as.character(dat02$marital_status)
dat02$marital_status[dat02$marital_status=="" | is.na(dat02$marital_status)==1] <- "missing"
dat02$marital_status <- as.factor(dat02$marital_status)
#create marital status combined categories
dat02$marital_status2 <- with(dat02, ifelse(marital_status %in% c("married","partner"),"relationship", ifelse(marital_status %in% c("divorced","single","widowed"),"single","missing")))
dat02$marital_status2 <- as.factor(dat02$marital_status2)
#variables to keep
dat02 <- dat02[,colnames(dat02) %in% c("id","dates","zip","employer","occupation","marital_status","marital_status2","age"
							,"gender","height","goal_weight","or_coach_id","homestore","first_weight","first_date","start_date","start_bmi"
							,"weight","pct_WL_cum","bmi","note_count_total","plan_grp"
							,"hip.median","waist.median","WHR"
							,"med_blood_pressure","med_antidepressant","med_cholesterol"
							,"med_sleep","med_diabetes","med_thyroid","med_acid_reflux","med_vitamin","med_diuretics","med_allergies"
							,"med_birthcontrol","med_asthma","med_aspirin","med_bloodthinner","med_total","med_indicator",
							"1","2","3","4","5","6")]
#fix med total
dat02$med_total <-  ifelse(is.na(dat02$med_total)==1, 0, dat02$med_total)
#calculate actual weight lost in pounds
dat02$WL <- with(dat02, (pct_WL_cum/100)*first_weight)							
#calculate change in BMI
dat02$bmi_change <- dat02$bmi - dat02$start_bmi
#add new gender variable
dat02$female <- ifelse(dat02$gender=="female", 1, 0)
#marital status variables
dat02$relationship <- ifelse(dat02$marital_status2=="relationship", 1, 0)
dat02$single <- ifelse(dat02$marital_status2=="single", 1, 0)
#group coach meetings
dat02$note_count_total_grp <- with(dat02, 
									ifelse(note_count_total<=9, "1-Low", 
									ifelse(note_count_total>9 & note_count_total<=15, "2-Medium",
									ifelse(note_count_total>15, "3-High", "other"
									))))











##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

#########################################
#########################################
##########      12 MONTHS      ##########
#########################################
#########################################


# pull IDs for each month separately for those that have a weight measurement
tmp12 <- data09[which(data09$months==12 & is.na(data09$weight)==0),]
tmp11 <- as.data.frame(data09[which(data09$months==11 & is.na(data09$weight)==0),"id"]); colnames(tmp11) <- "id"
tmp10 <- as.data.frame(data09[which(data09$months==10 & is.na(data09$weight)==0),"id"]); colnames(tmp10) <- "id"
tmp9 <- as.data.frame(data09[which(data09$months==9 & is.na(data09$weight)==0),"id"]); colnames(tmp9) <- "id"
tmp8 <- as.data.frame(data09[which(data09$months==8 & is.na(data09$weight)==0),"id"]); colnames(tmp8) <- "id"
tmp7 <- as.data.frame(data09[which(data09$months==7 & is.na(data09$weight)==0),"id"]); colnames(tmp7) <- "id"
tmp6 <- as.data.frame(data09[which(data09$months==6 & is.na(data09$weight)==0),"id"]); colnames(tmp6) <- "id"
tmp5 <- as.data.frame(data09[which(data09$months==5 & is.na(data09$weight)==0),"id"]); colnames(tmp5) <- "id"
tmp4 <- as.data.frame(data09[which(data09$months==4 & is.na(data09$weight)==0),"id"]); colnames(tmp4) <- "id"
tmp3 <- as.data.frame(data09[which(data09$months==3 & is.na(data09$weight)==0),"id"]); colnames(tmp3) <- "id"
tmp2 <- as.data.frame(data09[which(data09$months==2 & is.na(data09$weight)==0),"id"]); colnames(tmp2) <- "id"
tmp1 <- as.data.frame(data09[which(data09$months==1 & is.na(data09$weight)==0),"id"]); colnames(tmp1) <- "id"

#merge together with only month 12 information
dat03 <- merge(tmp1, merge(tmp2, merge(tmp3, merge(tmp4, merge(tmp5, merge(tmp6, merge(tmp7, merge(tmp8, merge(tmp9, merge(tmp10, merge(tmp11, tmp12, by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id"), by.x="id", by.y="id")

#2308 members that have 12 straight months of weight measurements
nrow(dat03)



#remove the 1 missing gender
#remove missing age
#only obese or overweight
#doesnt matter what meal plan they were on at month 12
dat04 <- dat03[which(dat03$gender != "" & is.na(dat03$age)==0 & dat03$start_bmi>=25 & dat03$marital_status!=""),]

#make missing marital status "missing"
dat04$marital_status <- as.character(dat04$marital_status)
dat04$marital_status[dat04$marital_status=="" | is.na(dat04$marital_status)==1] <- "missing"
dat04$marital_status <- as.factor(dat04$marital_status)

#create marital status combined categories
dat04$marital_status2 <- with(dat04, ifelse(marital_status %in% c("married","partner"),"relationship", ifelse(marital_status %in% c("divorced","single","widowed"),"single","missing")))
dat04$marital_status2 <- as.factor(dat04$marital_status2)

#variables to keep
dat04 <- dat04[,colnames(dat04) %in% c("id","dates","zip","employer","occupation","marital_status","marital_status2","age"
							,"gender","height","goal_weight","or_coach_id","homestore","first_weight","first_date","start_date","start_bmi", "plan_grp"
							,"weight","pct_WL_cum","bmi","note_count_total","med_blood_pressure","med_antidepressant","med_cholesterol"
							,"med_sleep","med_diabetes","med_thyroid","med_acid_reflux","med_vitamin","med_diuretics","med_allergies"
							,"med_birthcontrol","med_asthma","med_aspirin","med_bloodthinner","med_total","med_indicator")]

#fix med total
dat04$med_total <-  ifelse(is.na(dat04$med_total)==1, 0, dat04$med_total)
							
#calculate actual weight lost in pounds
dat04$WL <- with(dat04, (pct_WL_cum/100)*first_weight)							
					
#calculate change in BMI
dat04$bmi_change <- dat04$bmi - dat04$start_bmi

#add new gender variable
dat04$female <- ifelse(dat04$gender=="female", 1, 0)

#marital status variables
dat04$relationship <- ifelse(dat04$marital_status2=="relationship", 1, 0)
dat04$single <- ifelse(dat04$marital_status2=="single", 1, 0)

#group coach meetings
dat04$note_count_total_grp <- with(dat04, 
									ifelse(note_count_total<=9, "1-Low", 
									ifelse(note_count_total>9 & note_count_total<=15, "2-Medium",
									ifelse(note_count_total>15, "3-High", "other"
									))))
					
#add month 6 weights
dat05 <- merge(dat04, dat02[,c("id","weight","WL","pct_WL_cum","plan_grp","note_count_total","bmi")], by.x="id", by.y="id", all.x=TRUE)
## .y indicates 6 month measures

dat06 <- dat05[which(is.na(dat05$plan_grp.y)==0),]












##summary statistics - TABLES
# library(psych)
table(dat06$gender)/nrow(dat06)
table(dat06$marital_status2)/nrow(dat06)
describe(dat06$med_indicator,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_antidepressant,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_blood_pressure,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_cholesterol,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_diuretics,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_diabetes,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_sleep,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_acid_reflux,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_vitamin,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_allergies,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_aspirin,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_bloodthinner,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$med_thyroid,skew=FALSE)[c(2,3,8,5,6)]


describe(dat06$age,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$height,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$first_weight,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$start_bmi,skew=FALSE)[c(2,3,8,5,6)]
##month 6
describe(dat06$weight.y,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$bmi.y,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$pct_WL_cum.y,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$WL.y,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$note_count_total.y,skew=FALSE)[c(2,3,8,5,6)] 
##month 12
describe(dat06$weight.x,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$bmi.x,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$pct_WL_cum.x,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$WL.x,skew=FALSE)[c(2,3,8,5,6)]
describe(dat06$note_count_total.x,skew=FALSE)[c(2,3,8,5,6)] 



#ANOTHER TABLE
with(dat06, cor.test(pct_WL_cum.x, age, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, age, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, female, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, female, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_indicator, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_indicator, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_antidepressant, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_antidepressant, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_blood_pressure, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_blood_pressure, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_cholesterol, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_cholesterol, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_diuretics, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_diuretics, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_diabetes, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_diabetes, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_sleep, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_sleep, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_acid_reflux, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_acid_reflux, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, med_vitamin, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_vitamin, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_allergies, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_allergies, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_aspirin, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_aspirin, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_bloodthinner, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_bloodthinner, type="spearman")$p.value) 
# with(dat06, cor.test(pct_WL_cum.x, med_thyroid, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, med_thyroid, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, height, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, height, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, first_weight, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, first_weight, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, start_bmi, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, start_bmi, type="spearman")$p.value) 
##month6
with(dat06, cor.test(pct_WL_cum.x, weight.y, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, weight.y, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, bmi.y, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, bmi.y, type="spearman")$p.value) 
with(dat06, cor.test(pct_WL_cum.x, pct_WL_cum.y, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, pct_WL_cum.y, type="spearman")$p.value)
with(dat06, cor.test(pct_WL_cum.x, WL.y, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, WL.y, type="spearman")$p.value)  
with(dat06, cor.test(pct_WL_cum.x, note_count_total.y, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, note_count_total.y, type="spearman")$p.value) 
##month12
with(dat06, cor.test(pct_WL_cum.x, note_count_total.x, type="spearman")$estimate); with(dat06, cor.test(pct_WL_cum.x, note_count_total.x, type="spearman")$p.value)












####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

## WEIGHT LOSS OVER TIME GRAPH FOR THIS DATA

ids <- dat06$id
length(ids)

# pull IDs for each month separately for those that have a weight measurement
t12 <- data09[which(data09$months==12 & data09$id %in% ids),c("id","months","pct_WL_cum")]
t11 <- as.data.frame(data09[which(data09$months==11 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t10 <- as.data.frame(data09[which(data09$months==10 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t9 <- as.data.frame(data09[which(data09$months==9 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t8 <- as.data.frame(data09[which(data09$months==8 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t7 <- as.data.frame(data09[which(data09$months==7 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t6 <- as.data.frame(data09[which(data09$months==6 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t5 <- as.data.frame(data09[which(data09$months==5 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t4 <- as.data.frame(data09[which(data09$months==4 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t3 <- as.data.frame(data09[which(data09$months==3 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t2 <- as.data.frame(data09[which(data09$months==2 & data09$id %in% ids),c("id","months","pct_WL_cum")])
t1 <- as.data.frame(data09[which(data09$months==1 & data09$id %in% ids),c("id","months","pct_WL_cum")])

#combine together with only month 12 information
d1 <- rbind(t12,t11,t10,t9,t8,t7,t6,t5,t4,t3,t2,t1)



##GRAPH
# PLOT OF AVERAGE monthly WEIGHT LOSS OVER TIME #
WL_time <- summaryBy(pct_WL_cum ~ months, data = d1, FUN=mean)
colnames(WL_time)[2] <- "pct_WL_cum"

png("WL_time12.png",width=7, height=5, units='in', res=300)
	ggplot(WL_time, aes(x=months, y=pct_WL_cum)) +
		geom_line(size=2) +	
		geom_point(size=2, shape=21, fill="white") +
		xlab("Months in Program") +	
		ylab("Average Cumulative Weight Loss") + 
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











####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#weight loss at month 12 by coach meetings



#scatter cum WL by cum coach meetings at month 12
png("WL_meetings12.png",width=6, height=4, units='in', res=300)
	ggplot(dat06, aes(note_count_total.x, pct_WL_cum.x)) +
		stat_binhex(bins=50, geom="hex") +
		scale_fill_gradientn(colours=c("lightskyblue","black"),name = "Frequency",na.value=NA) +
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
		

summary(lm(pct_WL_cum.x ~ note_count_total.x, data=dat06))

#scatter cum WL by cum coach meetings at month 12 with regression line
png("WL_meetings12_reg.png",width=6, height=4, units='in', res=300)
	ggplot(dat06, aes(note_count_total.x, pct_WL_cum.x)) +
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


#scatter cum WL by cum coach meetings at month 12 with loess curve
png("WL_meetings12_loess.png",width=6, height=4, units='in', res=300)
	ggplot(dat06, aes(note_count_total.x, pct_WL_cum.x)) +
		stat_binhex(bins=50, geom="hex") +
		scale_fill_gradientn(colours=c("lightskyblue","black"),name = "Frequency",na.value=NA) +
		geom_smooth(method='lm', formula=y~x, se=FALSE, aes(color="Linear"), show.legend=TRUE, size=1) + # method= lm, glm, gam -- all the same
		geom_smooth(method='loess', se=FALSE, aes(color="Loess"), show.legend=TRUE, size=1) +
		xlab("Cumulative Coach Meetings") + 
		ylab("Cumulative Weight Loss") +	
		scale_x_continuous(breaks = seq(0,100,10)) +
		scale_y_continuous(breaks = seq(-60,60,10)) + scale_colour_manual(name="", values=c("gray90", "black")) +
		theme(
			axis.text = element_text(size = 16),
			axis.title = element_text(size = 18),
			panel.grid.major = element_line(colour = "white"),
			panel.grid.minor = element_blank(),
			panel.background = element_rect(fill = "grey70"),
			legend.text = element_text(size = 16),
			legend.title = element_text(size = 16),
			legend.position="right",
			plot.title = element_text(size=18, face="bold"))
dev.off()











####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

## look for confounding factors

#coach meetings
summary(lm(pct_WL_cum.x ~ note_count_total.x, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + start_bmi, data=dat06)) 

summary(lm(note_count_total.x ~ start_bmi, data=dat06)) 
summary(lm(pct_WL_cum.x ~ start_bmi, data=dat06)) 

summary(lm(pct_WL_cum.x ~ note_count_total.x, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + gender, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + age, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + marital_status2, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + first_weight, data=dat06)) #maybe
summary(lm(pct_WL_cum.x ~ note_count_total.x + start_bmi, data=dat06)) #maybe
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_total, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_indicator, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + homestore, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_blood_pressure, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_antidepressant, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_diabetes, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_vitamin, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_diuretics, data=dat06))
summary(lm(pct_WL_cum.x ~ note_count_total.x + med_sleep, data=dat06))






#blood pressure
summary(lm(pct_WL_cum.x ~ med_blood_pressure, data=dat06))
summary(lm(pct_WL_cum.x ~ med_blood_pressure + start_bmi, data=dat06))
summary(lm(pct_WL_cum.x ~ med_blood_pressure + gender, data=dat06))
summary(lm(pct_WL_cum.x ~ med_blood_pressure + start_bmi + gender, data=dat06))
#
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + gender, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + age, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + marital_status2, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + first_weight, data=dat06)) 
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + start_bmi, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_total, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_indicator, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + homestore, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_antidepressant, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_diabetes, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_vitamin, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_diuretics, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_blood_pressure + med_sleep, data=dat06))




#antidepressant
summary(lm(pct_WL_cum.x ~ med_antidepressant, data=dat06))
summary(lm(pct_WL_cum.x ~ med_antidepressant + gender, data=dat06))
summary(lm(pct_WL_cum.x ~ med_antidepressant + start_bmi, data=dat06))
summary(lm(pct_WL_cum.x ~ med_antidepressant + start_bmi + gender, data=dat06))
#
# summary(lm(pct_WL_cum.x ~ med_antidepressant + gender, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + age, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + marital_status2, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + first_weight, data=dat06)) 
# summary(lm(pct_WL_cum.x ~ med_antidepressant + start_bmi, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_total, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_indicator, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + homestore, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_blood_pressure, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_diabetes, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_vitamin, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_diuretics, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_antidepressant + med_sleep, data=dat06))

#checking confounding sex on starting bmi
summary(lm(pct_WL_cum.x ~ start_bmi, data=dat06))
summary(lm(pct_WL_cum.x ~ start_bmi + gender, data=dat06))



#diabetes
summary(lm(pct_WL_cum.x ~ med_diabetes, data=dat06))
summary(lm(pct_WL_cum.x ~ med_diabetes + age, data=dat06))
summary(lm(pct_WL_cum.x ~ med_diabetes + start_bmi, data=dat06))
summary(lm(pct_WL_cum.x ~ med_diabetes + start_bmi + age, data=dat06))
#
# summary(lm(pct_WL_cum.x ~ med_diabetes + gender, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + age, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + marital_status2, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + first_weight, data=dat06)) 
# summary(lm(pct_WL_cum.x ~ med_diabetes + start_bmi, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_total, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_indicator, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + homestore, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_blood_pressure, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_antidepressant, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_vitamin, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_diuretics, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_diabetes + med_sleep, data=dat06))



#vitamins
summary(lm(pct_WL_cum.x ~ med_vitamin, data=dat06))
summary(lm(pct_WL_cum.x ~ med_vitamin + start_bmi, data=dat06))
#
# summary(lm(pct_WL_cum.x ~ med_vitamin + gender, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + age, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + marital_status2, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + first_weight, data=dat06)) 
# summary(lm(pct_WL_cum.x ~ med_vitamin + start_bmi, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_total, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_indicator, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + homestore, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_blood_pressure, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_antidepressant, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_diabetes, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_diuretics, data=dat06))
# summary(lm(pct_WL_cum.x ~ med_vitamin + med_sleep, data=dat06))



summary(lm(pct_WL_cum.x ~ gender, data=dat06))
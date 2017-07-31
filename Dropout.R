setwd(...)

# load libraries
source("Libraries.R")



## read in data
long_dev <- read.csv("long_dev.csv.gz")
surv_dev <- read.csv("surv_dev.csv.gz")

long_val <- read.csv("long_val.csv.gz")
surv_val <- read.csv("surv_val.csv.gz")










# monthly weight loss by drop out by month
## looking at weight loss split by if they dropped out in the following month
tmp1 <- matrix(ncol=5,nrow=66)
rowcount <- NA

for(current in 1:11){
	dat <- long_dev[which(long_dev$months==current),]
	
	for(status in eval(current+1):12){
		rowcount <- ifelse(is.na(rowcount)==1, 1, rowcount+1)
		
		dat$target <- ifelse(dat$month<=status & dat$event==1, 1, 0)

		tmp1[eval(rowcount),1] <- current
		tmp1[eval(rowcount),2] <- status		
		tmp1[eval(rowcount),3] <- t.test(dat$pct_WL_mo ~ dat$target)$estimate[1]
		tmp1[eval(rowcount),4] <- t.test(dat$pct_WL_mo ~ dat$target)$estimate[2]
		tmp1[eval(rowcount),5] <- t.test(dat$pct_WL_mo ~ dat$target)$p.value
	}
}		
colnames(tmp1) <- c("CurrentMonth","StatusMonth","WL_Active","WL_Drop","P-Value")
rownames(tmp1) <- NULL
tmp1







## same thing with cumulative weight loss ##
tmp2 <- matrix(ncol=5,nrow=66)
rowcount <- NA

for(current in 1:11){
	dat <- long_dev[which(long_dev$months==current),]
	
	for(status in eval(current+1):12){
		rowcount <- ifelse(is.na(rowcount)==1, 1, rowcount+1)
		
		dat$target <- ifelse(dat$month<=status & dat$event==1, 1, 0)

		tmp2[eval(rowcount),1] <- current
		tmp2[eval(rowcount),2] <- status		
		tmp2[eval(rowcount),3] <- t.test(dat$pct_WL_cum ~ dat$target)$estimate[1]
		tmp2[eval(rowcount),4] <- t.test(dat$pct_WL_cum ~ dat$target)$estimate[2]
		tmp2[eval(rowcount),5] <- t.test(dat$pct_WL_cum ~ dat$target)$p.value
	}
}		
colnames(tmp2) <- c("CurrentMonth","StatusMonth","WL_Active","WL_Drop","P-Value")
rownames(tmp2) <- NULL
tmp2


setwd(...)

## load libraries
library(JM)
library(nlme)





#load longitudinal model
load(file="model2.rda")
summary(model2)

#load survival model
load(file="survFit04.rda")
summary(survFit04)









#run joint model
joint5 <- jointModel(model2, survFit04, timeVar = "months", method = "piecewise-PH-aGH", control=list(lng.in.kn=4))
summary(joint5)

#save joint model
save(joint5, file="joint5.rda")









#run joint model - slope
dform <- list(fixed = ~ -1 + I(2 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 1) / 12)
							+ I(-2 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 1) / 12)
				, indFixed = c(3,4,5,2,3,4)
				, random = ~ -1 + I(2 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 1) / 12)
							+ I(-2 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 1) / 12)
				, indRandom = c(3,4,5,2,3,4))
joint5a <- update(joint5, parameterization = "both", derivForm = dform)
anova(joint5a, process="Event")
summary(joint5a)

#save joint model
save(joint5a, file="joint5a.rda")









#run joint model - cumulative
iform <- list(fixed = ~ -1 + I(12 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 3) / 3)
							+ I(months*prev_note_count) + I(months*prev_MonthlyRecordings) + I(months*start_bmi)
				, indFixed = 1:8
				, random = ~ -1 + I(12 * bs(months, knots = c(3,6), Boundary.knots = c(0, 12), degree = 3) / 3) 
				, indRandom = 1:5)
joint5b <- update(joint5, parameterization = "both", derivForm = iform)
anova(joint5b, process="Event")
summary(joint5b)

#save joint model
save(joint5b, file="joint5b.rda")


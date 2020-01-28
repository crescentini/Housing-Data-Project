rm(list=ls())

library(MASS)
library(olsrr)

house <- read.csv(file = "C:/Users/Chandler/Documents/STAT_4212/HouseData_Crescentini.csv", header = T)
head(house)

# remove property number and webpage columns
house <- house[,-2]
house <- house[,-1]
str(house)
levels(house$SchoolDist)
summary(house)

# take 20% of data for holdout
house_holdout <- house[65:80,]
house <- house[1:64,]

# plot individual factors
plot(house$Value, house$Beds, xlab = "Number of Bedrooms", ylab = "Price of House")
plot(house$Value, house$Baths, xlab = "Number of Bathrooms", ylab = "Price of House")
plot(house$Value, house$LivingSpace, xlab = "Amount of Living Space (sq. ft.)", ylab = "Price of House")
plot(house$Value, house$LotSize, xlab = "Lot Size (acres)", ylab = "Price of House")
plot(house$Value, house$YearBuilt, xlab = "Year Built", ylab = "Price of House")
plot(house$Value, house$TotRooms, xlab = "Total Number of Rooms", ylab = "Price of House")
plot(house$Value, house$BuildExtType, xlab = "Building Exterior Type", ylab = "Price of House")
plot(house$Value, house$SchoolDist, xlab = "School District", ylab = "Price of House")

# Empty and full models
fit0 <- lm(Value ~ 1, data = house)
fit1 <- lm(Value ~ ., data = house)
summary(fit1)

# VIF values
vif <- ols_coll_diag(fit1)
vif$vif_t

# step AIC for different approaches (unused)
step.model1 <- stepAIC(fit1, direction = "backward") #LotSize, Beds, LivingSpace
step.model2 <- stepAIC(fit0, direction = "forward", scope = list(upper = fit1, lower = fit0)) #LivingSpace, Beds, LotSize
step.model3 <- stepAIC(fit0, direction = "both", scope = list(upper = fit1, lower = fit0))

# Use this
ols_step_forward_p(fit1, penter = .25) # LivingSpace, Beds, LotSize
ols_step_backward_p(fit1, prem = .1) # Beds, LivingSpace, LotSize
ols_step_both_p(fit1, pent = .25, prem = .1) # LivingSpace, Beds, LotSize

# All three approaches return same model
fit2 <- lm(Value ~ LotSize + Beds + LivingSpace, data = house)
summary(fit2)

vif2 <- ols_coll_diag(fit2)
vif2$vif_t

# Residual analysis
ols_plot_resid_stud_fit(fit2)

plot(hatvalues(fit2))
abline(h = 2*3/64, col = "red")

# take a look at 49 and 61
plot(cooks.distance(fit2))
abline(h = 1.0, col = "red")

house1 <- house[-61,]
fit3 <- lm(Value ~ 1, data = house1)
fit4 <- lm(Value ~ ., data = house1)

ols_step_forward_p(fit4, penter = .25) # LivingSpace, LotSize, YearBuilt, BuildExtType, TotRooms
ols_step_backward_p(fit4, prem = .1) # LivingSpace, LotSize, YearBuilt, BuildExtType
ols_step_both_p(fit4, pent = .25, prem = .1) # LivingSpace, LotSize, YearBuilt, BuildExtType

fit5 <- lm(Value ~ LotSize + LivingSpace + YearBuilt + BuildExtType, data = house1)
summary(fit5)

ols_plot_resid_stud_fit(fit5)
plot(hatvalues(fit5))
abline(h = 2*6/63, col = "red")
plot(cooks.distance(fit5), ylim = c(0.0, 1.1))
abline(h = 1.0, col = "red")

ols_coll_diag(fit5)

resid <- rstudent(fit5)

plot(fitted(fit5), resid)
qqnorm(fit5$residuals)
qqline(fit5$residuals)

# Variables in model
plot(house1$LivingSpace, resid)
plot(house1$LotSize, resid)
plot(house1$YearBuilt, resid)
plot(house1$BuildExtType, resid)

# Variables not in model
plot(house1$Beds, resid, xlab = "Number of Bedrooms", ylab = "Studentized Residuals")
plot(house1$Baths, resid, xlab = "Number of Bathrooms", ylab = "Studentized Residuals")
plot(house1$TotRooms, resid, xlab = "Total Number of Rooms", ylab = "Studentized Residuals")
plot(house1$SchoolDist, resid, xlab = "School District", ylab = "Studentized Residuals")

# Prediction
pred <- predict(fit5, house_holdout, interval = "prediction")

predDiff <- house_holdout$Value - pred[,1]
predDiff

plot(house_holdout$Value)
points(pred[,1], pch = 20, col = "red")
lines(pred[,1], col = "red")

rmse(fit5, house_holdout)

# Unused
pred1 <- predict(fit2, house_holdout, interval = "prediction")

predDiff1 <- house_holdout$Value - pred1[,1]
predDiff1

plot(house_holdout$Value)
points(pred1[,1], pch = 20, col = "green")
lines(pred1[,1], col = "green")





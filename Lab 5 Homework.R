attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (Hispanic == 1) & (female == 1) & ((educ_college == 1) | (educ_advdeg == 1))
dat_use <- subset(acs2017_ny,use_varb) 
detach()
lm((INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) ), data=dat_use )
log(AGE)

lm(INCWAGE ~ AGE + I(AGE^2) + female + I(female*AGE) + I(female*(AGE^2) + I(female*(AGE^3), data = dat_use ) 
log(AGE)

install.packages("stargazer")    

regression1 <- lm(INCWAGE ~ AGE + I(AGE^2) + female +I(female*AGE) + I(female*AGE^2), data = dat_use)
summary(regression1)

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.9) 
dat_graph <-subset(dat_use,graph_obs) 

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted2 <- data.frame(AGE = 30:65, educ_college = 0)
to_be_predicted2$yhat <- predict(regression1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
to_be_predicted2 <- data.frame(AGE = 25:55, educ_college = 1)

to_be_predicted2$yhat <- predict(regression1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
setwd(put here path to data)

### The data provided here refers to simulated data.
### The actual HRS data can be obtained at https://g2aging.org/

### Read in made-up data
prev_unhealthy2 = read.table("HRS_FAKE.txt")

###Smooth
library(mgcv)

model.gam.f <- gam(unhealthy ~ s(age, bs="cr") + s(cohort),
                   family=binomial, data=subset(prev_unhealthy2, gender_name=="female"), weights=Freq,
                   method="REML")

model.gam.m <- gam(unhealthy ~ s(age, bs="cr") + s(cohort),
                   family=binomial, data=subset(prev_unhealthy2, gender_name=="male"), weights=Freq,
                   method="REML")


### Predict data for 1980 to 2019

### Container for predicted values
predict.data.f <- data.frame(year = rep(1980:2019, each = 51),
                             age = rep(50:100, times = length(1980:2019)))

predict.data.m <- data.frame(year = rep(1980:2019, each = 51),
                             age = rep(50:100, times = length(1980:2019)))

predict.data.f$cohort <- predict.data.f$year-predict.data.f$age
predict.data.m$cohort <- predict.data.m$year-predict.data.m$age

### Fill it with predicted values
predicted.Prev.f <- predict(model.gam.f, newdata = predict.data.f, type = "response")
predict.data.f$Prev.predicted <- predicted.Prev.f

predicted.Prev.m <- predict(model.gam.m, newdata = predict.data.m, type = "response")
predict.data.m$Prev.predicted <- predicted.Prev.m

### check out how it looks for birth cohort 1910
bcohort = 1910

par(mfrow=c(2,1))
plot(filter(prev_unhealthy2, cohort==bcohort & gender_name=="female")$age,
     filter(prev_unhealthy2, cohort==bcohort & gender_name=="female")$unhealthy,
     type="p", ylim=c(0,1), xlim=c(50,100))
lines(filter(predict.data.f, cohort==bcohort)$age,
      filter(predict.data.f, cohort==bcohort)$Prev.predicted, col="red")

plot(filter(prev_unhealthy2, cohort==bcohort & gender_name=="male")$age,
     filter(prev_unhealthy2, cohort==bcohort & gender_name=="male")$unhealthy,
     type="p", ylim=c(0,1),xlim=c(50,100))
lines(filter(predict.data.m, cohort==bcohort)$age,
      filter(predict.data.m, cohort==bcohort)$Prev.predicted, col="red")

### Let's save it
write.table(predict.data.m, "SmoothUSA_m_FAKE.txt")
write.table(predict.data.f, "SmoothUSA_f_FAKE.txt")
write.table(prev_unhealthy2, "Merged_USA_FAKE.txt")

### End

library(dplyr)

### Preparing data for figure 1

Country.merge.m <- read.table("USA_data_male_FAKE.txt")
### The file contains the probability of dying and the predicted prevalence data
### from the made-up data files, i.e., it is simply the merged data file

colnames(Country.merge.m) <- c("age","year","cohort","Prev.predict","qx")
Country.merge.m$Prev.predict <- 1-Country.merge.m$Prev.predict
USA.merge.m <- arrange(Country.merge.m, age)

### function required
CAL_px_fun <- function(country.data) {

    CAL.px <- list()

    for (x in 1:51) {
        CAL.px[[x]] <-
            data.frame(Age=country.data$age[country.data$cohort==c(2020-50-x)],
                       px=1-c(country.data$qx[country.data$cohort==c(2020-50-x)]),
                       Prev=country.data$Prev.predict[country.data$cohort==c(2020-50-x)])
    }

    for (x in 1:51) {
        CAL.px[[x]]$lx <- c(cumprod(CAL.px[[x]]$px))
    }

    for (x in 1:51) {
        CAL.px[[x]]$lxH <- c(CAL.px[[x]]$lx*(1-CAL.px[[x]]$Prev))
    }

    for (x in 1:51) {
        CAL.px[[x]]$pxH <- c(CAL.px[[x]]$lxH[1], CAL.px[[x]]$lxH[-1] / CAL.px[[x]]$lxH[-length(CAL.px[[x]]$lxH)])
    }

    return(CAL.px)

}

### for men
USA.px <- CAL_px_fun(USA.merge.m)

CAL.lx <- c()
HCAL.lx <- c()
HCAL.pxH <- c()


for (x in 1:51) {
  CAL.lx[x] <- rev(USA.px[[x]]$lx)[1]
  HCAL.lx[x] <- rev(USA.px[[x]]$lxH)[1]
  HCAL.pxH[x] <- rev(prod(USA.px[[x]]$pxH))[1]
}

sum(c(CAL.lx))+0.5 #29.19
TCAL.m #29.19
sum(c(HCAL.lx))+0.5 #27.42
HTCAL.m #27.42


### for women
Country.merge.f <- read.table("USA_data_female_FAKE.txt")
colnames(Country.merge.f) <- c("age","year","cohort","Prev.predict","qx")
Country.merge.f$Prev.predict <- 1-Country.merge.f$Prev.predict
USA.merge.f <- arrange(Country.merge.f, age)

USA.px.f <- CAL_px_fun(USA.merge.f)

CAL.lx <- c()
HCAL.lx <- c()
HCAL.pxH <- c()


for (x in 1:51) {
  CAL.lx[x] <- rev(USA.px.f[[x]]$lx)[1]
  HCAL.lx[x] <- rev(USA.px.f[[x]]$lxH)[1]
  HCAL.pxH[x] <- rev(prod(USA.px.f[[x]]$pxH))[1]
}

sum(c(CAL.lx))+0.5 #31.42
TCAL.f #31.42

sum(c(HCAL.lx))+0.5 #28.89
HTCAL.f #28.89

### Finally, the plot shown in Figure 1

par(mfrow=c(2,1))
plot(USA.px.f[[31]]$Age, USA.px.f[[31]]$lx, xlim=c(50,100), ylim=c(0,1), type="l",
     xlab="Age", ylab="Proportion of Survivors", main="Women", lwd=2, cex.lab=1.3, cex.axis=1.3, col="darkred")
lines(USA.px.f[[31]]$Age, USA.px.f[[31]]$lxH, lty=2, lwd=2,col="darkred")
lines(USA.px.f[[39]]$Age, USA.px.f[[39]]$lx, col="orangered", lwd=2, lty=1)
lines(USA.px.f[[39]]$Age, USA.px.f[[39]]$lxH, col="orangered", lwd=2, lty=2)
#legend(80, 0.9, legend=c("Overall Survival", "Healthy Survival"), bty="n", fill=c("darkred","orangered"), cex=1.3)
legend(47, 0.43, legend=c("Birth Cohort 1939, Overall Survival",
                          "Birth Cohort 1939, Healthy Survival",
                          "Birth Cohort 1931, Overall Survival",
                          "Birth Cohort 1931, Healthy Survival"),
       bty="n", lty=c(1,2,1,2), cex=1.1, lwd=2, col=c("darkred","darkred","orangered","orangered"), ncol=1)

plot(USA.px[[31]]$Age, USA.px[[31]]$lx, xlim=c(50,100), ylim=c(0,1), type="l",
     xlab="Age", ylab="Proportion of Survivors", main="Men", lwd=2, cex.lab=1.3, cex.axis=1.3, col="navy")
lines(USA.px[[31]]$Age, USA.px[[31]]$lxH, lty=2, lwd=2, col="navy")
lines(USA.px[[39]]$Age, USA.px[[39]]$lx, col="skyblue", lwd=2, lty=1)
lines(USA.px[[39]]$Age, USA.px[[39]]$lxH, col="skyblue", lwd=2, lty=2)

legend(47, 0.41, legend=c("Birth Cohort 1939, Overall Survival",
                          "Birth Cohort 1939, Healthy Survival",
                          "Birth Cohort 1931, Overall Survival",
                          "Birth Cohort 1931, Healthy Survival"),
       bty="n", lty=c(1,2,1,2), cex=1.1, lwd=2, col=c("navy","navy","skyblue","skyblue"), ncol=1)

### End

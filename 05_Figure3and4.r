### Figure 3 and 4
### I used some of the R code provided by Nepomuceno et al. (2022), which is available here https://osf.io/n3945/
### For the sake of readability, I show only a simple version of the plot. Combining two plots on one panel requires some modification of the
### filled.contour function. This is shown in more detail in the already mentioned code https://osf.io/n3945/

Mort.matrix <- readRDS("MorteffectFAKE.rds")
Dis.matrix <- readRDS("DisffectFAKE.rds")

TCAL.matrix <- readRDS("TCALFAKE.rds")
HCAL.matrix <- readRDS("HCALFAKE.rds")

library(RColorBrewer)
options(scipen=10)
WildColors<-rev(c(rev(c("#92c5de","#4393c3","#2166ac","#053061")), ## from dark red to light red
                  "white","white",
                  rev(c("#67001f","#b2182b","#d6604d","#f4a582"))))## from light blue to dark blue



levels<-c(-1,-0.1,-0.01,-0.001,-0.0001,0,.0001,.001,.01,.1,1)
YEARS <- 1919:2019
Age <- 0:100

### Mortality
filled.contour(xlim=c(1980,2019),YEARS,Age,t(Mort.matrix),levels=levels,col=WildColors,key.axes=customAxis(),
                ylab="Age- & Cohort-Contribution to THCAL Difference",xlab="Calendar Year",
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="Mortality Effect", ylim=c(50,100))

### Health
filled.contour(xlim=c(1980,2019),YEARS,Age,t(Dis.matrix),levels=levels,col=WildColors,key.axes=customAxis(),
                ylab="Age- & Cohort-Contribution to THCAL Difference",xlab="Calendar Year",
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="Health Effect", ylim=c(50,100))

### TCAL
filled.contour(xlim=c(1980,2019),YEARS,Age,t(TCAL.matrix),levels=levels,col=WildColors,key.axes=customAxis(),
                ylab="Age- & Cohort-Contribution to TCAL Difference",xlab="Calendar Year",
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="TCAL", ylim=c(50,100))
### THCAL
filled.contour(xlim=c(1980,2019),YEARS,Age,t(HCAL.matrix),levels=levels,col=WildColors,key.axes=customAxis(),
                ylab="Age- & Cohort-Contribution to THCAL Difference",xlab="Calendar Year",
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="THCAL", ylim=c(50,100))

### End

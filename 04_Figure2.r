### Preparing data for figure 2

### Women
### TCAL.f.list is created in "02_Calculation_Decomposition.r"

for (i in 1:101) {
    TCAL.f.list[[i]]$Cohort <- 2020-i
}

comp.f <- arrange(do.call( "rbind", TCAL.f.list)[, c("Age","lx","pi", "Cohort")], Cohort)
comp.f$Year <- comp.f$Cohort + comp.f$Age
comp.f.select <- filter(comp.f, Year>=1980)

empty <- data.frame(Age = rep(0:100, length(1980:2019)),
                    Year = rep(1980:2019, each=101))

comp.f.plot <- filter(arrange(merge(empty, comp.f.select[,-4], by=c("Age","Year"), all=TRUE), Year, Age), Age>=50)
comp.f.matrix.lx <- matrix(comp.f.plot$lx, nrow=51, ncol=40)
comp.f.matrix.pi <- matrix(comp.f.plot$pi, nrow=51, ncol=40)

### Men
### TCAL.m.list is created in "02_Calculation_Decomposition.r"
for (i in 1:101) {
    TCAL.m.list[[i]]$Cohort <- 2020-i
}

comp.m <- arrange(do.call( "rbind", TCAL.m.list)[, c("Age","lx","pi", "Cohort")], Cohort)
comp.m$Year <- comp.m$Cohort + comp.m$Age
comp.m.select <- filter(comp.m, Year>=1980)

empty <- data.frame(Age = rep(0:100, length(1980:2019)),
                    Year = rep(1980:2019, each=101))

comp.m.plot <- filter(arrange(merge(empty, comp.m.select[,-4], by=c("Age","Year"), all=TRUE), Year, Age), Age>=50)
comp.m.matrix.lx <- matrix(comp.m.plot$lx, nrow=51, ncol=40)
comp.m.matrix.pi <- matrix(comp.m.plot$pi, nrow=51, ncol=40)

levels <- round(seq(-0.7, 0.7, 0.1),1)
theyears <- 1980:2019
theages <- 50:100

blues <-  brewer.pal(9, "Blues")
Blues <-  rev(colorRampPalette(c(blues[1], blues[7]))(7))
Reds <-  brewer.pal(6, "Reds")
WildColors <- c(Blues, "white", Reds)

## lx levels and colors
levels <- seq(0,1,0.1)
blues <-  brewer.pal(9, "Blues")
Blues <-  rev(colorRampPalette(c(blues[2], blues[8]))(10))
WildColors <- rev(Blues)
##

lx.log <- log(comp.f.matrix.lx/comp.m.matrix.lx)
lx.log2 <- ifelse(lx.log>1, 1, lx.log)
pi.log <- log(comp.f.matrix.pi/comp.m.matrix.pi)

### better axis

customAxis <- function() {
n <- length(levels)
y <- seq(min(levels), max(levels), length.out=n)
rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors)
axis(4, at=y, labels=levels)
}

### Figure 2, differences in cohort survivor proportions
filled.contour(theyears,theages,t(lx.log2),
               levels=levels,col=WildColors,
               key.axes=customAxis(),
               ylab="Age",
               xlab="Calendar Year",
               main= "Survivors (Log)Ratios",
               cex.main=1.5,
               cex.lab=1.2,
               key.title = {par(cex.main=1.1);title(main="")},
               plot.axes={
                 axis(2,seq(50,100,5), lwd=1.2, cex.lab=1.2,cex.axis=1.2)
                 axis(1,c(seq(1980,2019, 5),2019), lwd=1.2, cex.lab=1.2,cex.axis=1.2)
               }
)


## pi levels and colors
levels <- round(seq(-0.45,0,0.045),2)
reds <-  brewer.pal(9, "Reds")
Reds <-  rev(colorRampPalette(c(reds[2], reds[8]))(10))
WildColors <- Reds

### Figure 2, differences in prevalence proportions

filled.contour(theyears,theages,t(pi.log),
               levels=levels,col=WildColors,
               key.axes=customAxis(),
               ylab="",
               xlab="Calendar Year",
               main= "Prevalence (Log)Ratios",
               cex.main=1.5,
               cex.lab=1.2,
               key.title = {par(cex.main=1.1);title(main="")},
               plot.axes={
                 axis(2,seq(50,100,5), lwd=1.2, cex.lab=1.2,cex.axis=1.2)
                 axis(1,c(seq(1980,2019, 5),2019), lwd=1.2, cex.lab=1.2,cex.axis=1.2)
               }
)

### End

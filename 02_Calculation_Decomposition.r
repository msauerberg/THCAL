rm(list=ls())

setwd(put here the path to the data)

###package required
library(dplyr)

### Data preparation, please note that the uploaded data files contain simulated data
### The actual mortality data can be obtained from www.mortality.org and from www.g2aging.org/
### Accordingly, the code below will produce different results than presented in the paper

prev.data.f <- read.table("SmoothUSA_f_FAKE.txt")
colnames(prev.data.f) <- c("Year", "Age", "Cohort", "Prev")
prev.data.m <- read.table("SmoothUSA_m_FAKE.txt")
colnames(prev.data.m) <- c("Year", "Age", "Cohort", "Prev")

USA.f <- read.table("fltper_1x1_FAKE.txt", header=TRUE)
USA.m <- read.table("mltper_1x1_FAKE.txt", header=TRUE)

### Ladies first
TCAL.f <- filter(USA.f[, c(1:2,4)], Year>=1980 & Age %in% 0:100)
TCAL.f$Age <- as.numeric(TCAL.f$Age)
TCAL.f$Cohort <- TCAL.f$Year - TCAL.f$Age
TCAL.f <- arrange(TCAL.f, Cohort)

###merge with prev data
USA.merge.f <- arrange(merge(prev.data.f, TCAL.f, all=T,
                             by=c("Age","Year","Cohort")), Cohort)
USA.merge.f <- arrange(USA.merge.f, Age)
USA.merge.f$Prev <- ifelse(USA.merge.f$Age <= 50, 0, USA.merge.f$Prev)
USA.merge.f$Prev <- 1 - USA.merge.f$Prev

USA.merge.f$qx <- ifelse(USA.merge.f$Age <= 50, 0, USA.merge.f$qx)

### My general coding approach uses a list object,
### in each list element is a dataframe with values for all the included TCAL cohorts

max.year <- max(USA.merge.f$Year)

TCAL.f.list <- list()

  for (x in 1:101) {
        TCAL.f.list[[x]] <-
            data.frame(Age=USA.merge.f$Age[USA.merge.f$Cohort==max.year+1-x],
                       px=1-c(USA.merge.f$qx[USA.merge.f$Cohort==c(max.year+1-x)]),
                       pi=c(USA.merge.f$Prev[USA.merge.f$Cohort==c(max.year+1-x)]))
    }

for (x in 1:101) {
    TCAL.f.list[[x]]$lx <- c(cumprod(TCAL.f.list[[x]]$px))
    TCAL.f.list[[x]]$healthy.lx <- TCAL.f.list[[x]]$lx * TCAL.f.list[[x]]$pi
    TCAL.f.list[[x]]$pxH <- c(TCAL.f.list[[x]]$healthy.lx[1], TCAL.f.list[[x]]$healthy.lx[-1] /
                                                               TCAL.f.list[[x]]$healthy.lx[-length(TCAL.f.list[[x]]$healthy.lx)])
    TCAL.f.list[[x]]$pi.lx <- c(1, c(TCAL.f.list[[x]]$pi[-1]) / c(TCAL.f.list[[x]]$pi[-length(TCAL.f.list[[x]]$pi)]))
    }

### Here are the CAL, HCAL functions, note that one function was build from the
### "health transition probabilites" that I calculated in the for loop above, named "pxH"

CAL.lx.f <- c()
HCAL.lx.f <- c()
HCAL.lx.pxH.f <- c()
pi_c.f <- c()

for (x in 1:101) {

  CAL.lx.f[x] <- rev(TCAL.f.list[[x]]$lx)[1]
  HCAL.lx.f[x] <- rev(TCAL.f.list[[x]]$healthy.lx)[1]
  HCAL.lx.pxH.f[x] <- prod(TCAL.f.list[[x]]$pxH)
  pi_c.f[x] <- rev(TCAL.f.list[[x]]$pi)[1]
}

TCAL.f <- sum(CAL.lx.f)+0.5-50
TCAL.f # 31.422
HTCAL.f <- sum(HCAL.lx.f)+0.5-50
HTCAL2.f <- sum(HCAL.lx.pxH.f)+0.5-50
sum(CAL.lx.f * c(pi_c.f))+0.5-50
HTCAL.f
HTCAL2.f # all three approaches yield 28.89

### Now, copy paste for men
TCAL.m <- filter(USA.m[, c(1:2,4)], Year>=1980 & Age %in% 0:100)
TCAL.m$Age <- as.numeric(TCAL.m$Age)
TCAL.m$Cohort <- TCAL.m$Year - TCAL.m$Age
TCAL.m <- arrange(TCAL.m, Cohort)

###merge with prev data
USA.merge.m <- arrange(merge(prev.data.m, TCAL.m, all=T,
                             by=c("Age","Year","Cohort")), Cohort)
USA.merge.m <- arrange(USA.merge.m, Age)
USA.merge.m$Prev <- ifelse(USA.merge.m$Age <= 50, 0, USA.merge.m$Prev)
USA.merge.m$Prev <- 1 - USA.merge.m$Prev

USA.merge.m$qx <- ifelse(USA.merge.m$Age <= 50, 0, USA.merge.m$qx)

### My general coding approach uses a list object,
### in each list element is a dataframe with values for the included TCAL cohorts

max.year <- max(USA.merge.m$Year)

TCAL.m.list <- list()

 for (x in 1:101) {
        TCAL.m.list[[x]] <-
            data.frame(Age=USA.merge.m$Age[USA.merge.m$Cohort==max.year+1-x],
                       px=1-c(USA.merge.m$qx[USA.merge.m$Cohort==c(max.year+1-x)]),
                       pi=c(USA.merge.m$Prev[USA.merge.m$Cohort==c(max.year+1-x)]))
    }

for (x in 1:101) {
    TCAL.m.list[[x]]$lx <- c(cumprod(TCAL.m.list[[x]]$px))
    TCAL.m.list[[x]]$healthy.lx <- TCAL.m.list[[x]]$lx * TCAL.m.list[[x]]$pi
    TCAL.m.list[[x]]$pxH <- c(TCAL.m.list[[x]]$healthy.lx[1], TCAL.m.list[[x]]$healthy.lx[-1] /
                                                               TCAL.m.list[[x]]$healthy.lx[-length(TCAL.m.list[[x]]$healthy.lx)])
    TCAL.m.list[[x]]$pi.lx <- c(1, c(TCAL.m.list[[x]]$pi[-1]) / c(TCAL.m.list[[x]]$pi[-length(TCAL.m.list[[x]]$pi)]))
    }

### Here are the CAL, HCAL functions, note that one function was build from the
### "health transition probabilites" that I calculated in the for loop above, named "pxH"

CAL.lx.m <- c()
HCAL.lx.m <- c()
HCAL.lx.pxH.m <- c()
pi_c.m <- c()

for (x in 1:101) {

  CAL.lx.m[x] <- rev(TCAL.m.list[[x]]$lx)[1]
  HCAL.lx.m[x] <- rev(TCAL.m.list[[x]]$healthy.lx)[1]
  HCAL.lx.pxH.m[x] <- prod(TCAL.m.list[[x]]$pxH)
  pi_c.m[x] <- rev(TCAL.m.list[[x]]$pi)[1]
}

TCAL.m <- sum(CAL.lx.m)+0.5-50
TCAL.m # 29.19
HTCAL.m <- sum(HCAL.lx.m)+0.5-50
HTCAL2.m <- sum(HCAL.lx.pxH.m)+0.5-50
HTCAL.m
HTCAL2.m
sum(CAL.lx.m * c(pi_c.m))+0.5-50 # all three approaches yield 27.42

### Decomposing the difference between women and men

### Empty container
Diff.list <- list()

for (x in 1:101) {
    Diff.list[[x]] <- data.frame(Age = 0:100)
}

the.border <- 2019-1980+1

for (x in 1:the.border) {
    Diff.list[[x]]$pxCH <- c(log(TCAL.f.list[[x]]$px/TCAL.m.list[[x]]$px), rep(0,101-x))
    Diff.list[[x]]$pxCH.healthy <- c(log(TCAL.f.list[[x]]$pi.lx/TCAL.m.list[[x]]$pi.lx), rep(0,101-x))
    Diff.list[[x]]$pxH <- c(log(TCAL.f.list[[x]]$pxH/TCAL.m.list[[x]]$pxH), rep(0,101-x))

}

for (x in c(the.border+1):101) {
    Diff.list[[x]]$pxCH <- c(rep(0,(x-the.border)),c(log(TCAL.f.list[[x]]$px/TCAL.m.list[[x]]$px)),
                             rep(0, 101-x))

    Diff.list[[x]]$pxCH.healthy <- c(rep(0,(x-the.border)),c(log(TCAL.f.list[[x]]$pi.lx/TCAL.m.list[[x]]$pi.lx)),
                                     rep(0, 101-x))

    Diff.list[[x]]$pxH <- c(rep(0,(x-the.border)),c(log(TCAL.f.list[[x]]$pxH/TCAL.m.list[[x]]$pxH)),
                               rep(0, 101-x))

}

### Getting the matrix for px
PxCh <- Diff.list[[1]]$pxCH

for(x in 2:101) {
    PxCh <- cbind(PxCh, Diff.list[[x]]$pxCH)
}

### Getting the matrix for px healthy
PxH <- Diff.list[[1]]$pxH

for(x in 2:101) {
    PxH <- cbind(PxH, Diff.list[[x]]$pxH)
}

### Getting the matrix for pi
PiCh <- Diff.list[[1]]$pxCH.healthy

for(x in 2:101) {
    PiCh <- cbind(PiCh, Diff.list[[x]]$pxCH.healthy)
}

### Getting the average functions
CALlx.average <- t(matrix(rep((c(CAL.lx.m * c(pi_c.m)) + c(CAL.lx.f * c(pi_c.f)))/2,101),101))
TCALlx.average <- t(matrix(rep((c(CAL.lx.m) + c(CAL.lx.f))/2,101),101))

###
TCAL.f-TCAL.m # Diff is 2.23 years
sum(PxCh * TCALlx.average) # Estimated diff is 2.27

###
HTCAL.f-HTCAL.m # Diff is 1.47
sum(PxH * CALlx.average) # Estimated diff is 1.48

### Decomposition components
Mort.term <- sum(PxCh * CALlx.average)
Dis.term <- sum(CALlx.average * PiCh)
Total.term <- Mort.term + Dis.term
HTCAL.f - HTCAL.m # Diff is 1.47
Total.term # Estimated diff is 1.48

### Simple decompo for HTCAL using the method by Nusselder and Looman (2004)
diff.lx <- c(CAL.lx.f) - c(CAL.lx.m)
diff.pi <- c(pi_c.f) - c(pi_c.m)

average.pi <- c(c(pi_c.m) + c(pi_c.f)) / 2
average.lx <- c(CAL.lx.m + CAL.lx.f) / 2

sum(diff.lx * average.pi)+sum(average.lx * diff.pi) #1.47
HTCAL.f - HTCAL.m #1.47

### Preparing the age- and cohort-specific contributions for graphs

assign.fun <- function(Decomp.matrix) {

    CALlxDecomp <- Decomp.matrix
    CALlxD<-matrix(0,101,101)
    CALlxDS<-CALlxD

    for (y in 1:101){
		for (x in 1:y){
			CALlxD[x,(101-y+x)]<-CALlxDecomp[x,y]
			CALlxDS[x,(101-y+x)]<-sum(CALlxDecomp[(1:x),y])
		}
	}
    return(CALlxDS)
    }


Decomp.matrix1 <- PxCh * CALlx.average
out1 <- assign.fun(Decomp.matrix1)

Decomp.matrix2 <- CALlx.average * PiCh
out2 <- assign.fun(Decomp.matrix2)

Decomp.matrix3 <- CALlx.average * PxH
out3 <- assign.fun(Decomp.matrix3)

Decomp.matrix4 <- TCALlx.average * PxCh
out4 <- assign.fun(Decomp.matrix4)



saveRDS(out1, "MorteffectFAKE.rds")
saveRDS(out2, "DisffectFAKE.rds")
saveRDS(out3, "HCALFAKE.rds")
saveRDS(out4, "TCALFAKE.rds")

### Estimating CI

### Using this function
CI.fun <- function(x, CAL.px, ns=1000, level=0.95) {

    qx <- 1-CAL.px[[x]]$px
    Dx <- CAL.px[[x]]$Dx
    m <- length(qx)
    Ntil <- round(Dx/qx)

    Y <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      qx),
                             m,ns))

    QX <- Y/Ntil

    pix <- c(1-CAL.px[[x]]$pi)
    H <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      pix),
                                 m,ns))

    PIX <- H/Ntil

    TCAL.CI <- c()
    HCAL.CI <- c()

    for (i in 1:ns) {
        TCAL.CI[i] <- prod(1-QX[,i])
        HCAL.CI[i] <- rev(cumprod(1-QX[,i])*(1-PIX[,i]))[1]
    }

    CI.TCAL <- quantile(TCAL.CI,
                        probs = c((1-level)/2,
                                  1 - (1-level)/2))

    CI.HCAL <- quantile(HCAL.CI,
                        probs = c((1-level)/2,
                                  1 - (1-level)/2))

    out.CI <- rbind(c(CI.TCAL[1], CI.TCAL[2]),
                    c(CI.HCAL[1], CI.HCAL[2])
                    )

    return(out.CI)
}

### Again, I use simulated counts
Counts <- read.table("Counts_FAKE.txt")
Counts$Cohort <- Counts$Year-Counts$Age

### CI for men
USA.px <- TCAL.m.list

for (x in 1:101) {
  USA.px[[x]]$Dx <- filter(Counts, Cohort==2020-x)$Male
}

for (i in 51:101) {
    USA.px[[i]] <- filter(USA.px[[i]], Age>=51)
}


output <- lapply(X=52:101, FUN=CI.fun, CAL.px=USA.px)

TCAL.lower <- c()
TCAL.upper <- c()

HCAL.lower <- c()
HCAL.upper <- c()

for (i in 1:50) {
  TCAL.lower[i] <- as.vector(output[[i]][1,1])
  TCAL.upper[i] <- as.vector(output[[i]][1,2])

  HCAL.lower[i] <- as.vector(output[[i]][2,1])
  HCAL.upper[i] <- as.vector(output[[i]][2,2])
}

TCAL.CI.lower <- sum(c(1,TCAL.lower))+0.5
TCAL.CI.upper <- sum(c(1,TCAL.upper))+0.5

HCAL.CI.lower <- sum(c(1,HCAL.lower))+0.5
HCAL.CI.upper <- sum(c(1,HCAL.upper))+0.5

cbind(TCAL.CI.lower, TCAL.CI.upper)
cbind(HCAL.CI.lower, HCAL.CI.upper)

TCAL.m
HTCAL.m

### Same for women
USA.px <- TCAL.f.list

for (x in 1:101) {
  USA.px[[x]]$Dx <- filter(Counts, Cohort==2020-x)$Female
}

for (i in 51:101) {
    USA.px[[i]] <- filter(USA.px[[i]], Age>=51)
}


output <- lapply(X=52:101, FUN=CI.fun, CAL.px=USA.px)

TCAL.lower <- c()
TCAL.upper <- c()

HCAL.lower <- c()
HCAL.upper <- c()

for (i in 1:50) {
  TCAL.lower[i] <- as.vector(output[[i]][1,1])
  TCAL.upper[i] <- as.vector(output[[i]][1,2])

  HCAL.lower[i] <- as.vector(output[[i]][2,1])
  HCAL.upper[i] <- as.vector(output[[i]][2,2])
}

TCAL.CI.lower <- sum(c(1,TCAL.lower))+0.5
TCAL.CI.upper <- sum(c(1,TCAL.upper))+0.5

HCAL.CI.lower <- sum(c(1,HCAL.lower))+0.5
HCAL.CI.upper <- sum(c(1,HCAL.upper))+0.5

cbind(TCAL.CI.lower, TCAL.CI.upper)
cbind(HCAL.CI.lower, HCAL.CI.upper)

TCAL.f
HTCAL.f

### End

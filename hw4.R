#3.1

streamflow <- data.frame('year' = c(1941:1960), 'flow.cfs' = c(369, 683, 923, 1193, 413, 1025, 894, 859, 1157, 524, 327, 574, 762, 578, 379, 375, 581, 581, 530, 929))

flow.cfs <- streamflow$flow.cfs

gd <- data.frame('granodiorite' = c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0, 5.0, 0.6, 1.2, 0.3, 0.2, 0.5, 0.5, 10, 0.2, 0.2, 1.7, 3.0))
gdsort <- sort(gd$granodiorite) #ascending order
gdsort

hist(gdsort)

#nonparametric n<20
#confidence interval 95%
#quantile 0.5
a <- 0.05
1-a
a/2 #x' = 4
n.gd <- 18 #n
Rl <- 4+1 #x'+1
Ru <- n.gd-4 #n-x'


#at n=18
gdsort[Rl]
gdsort[Ru]

lowgd <- gdsort[Rl]+0.025*(gdsort[Rl]-gdsort[Rl])
uppgd <- gdsort[Ru]+ 0.025*(gdsort[Ru]-gdsort[Ru])

print(paste(lowgd, "to", uppgd, "mg/L"))

#parametric
log.gd <- log(gdsort)

ybar <- mean(log.gd)
sd.gd <- sd(log.gd)^2
n.gd <- 18


hist(log.gd) #not normally distributed

lowgd1 <- exp(ybar-2.110*sqrt(sd.gd/n.gd)) #2.110 from t table of n-1
uppgd1 <- exp(ybar+2.110*sqrt(sd.gd/n.gd))

print(paste(lowgd1, "to", uppgd1, "mg/L"))


#3.4
hist(flow.cfs)
mean.flow <- mean(flow.cfs)
flowsort <- sort(flow.cfs)
median.flow <- median(flowsort)


#median
#n = 20
#quantile = 0.5
#x'=5 from table of binomial distribution
a <- 0.05
1-a
a/2 #x' = 5
n.flow <- 20 #n
Rlf <- 5+1 #x'+1
Ruf <- n.flow-5 #n-x'

lowflow1<- flowsort[Rlf]+0.025*(flowsort[Rlf]-flowsort[Rlf])
uppflow1<- flowsort[Ruf]+ 0.025*(flowsort[Ruf]-flowsort[Ruf])
print(paste('Median of', median.flow, 'from', lowflow1, "to", uppflow1, "mg/L"))

#mean
sd.flow <- sd(flow.cfs)

lowflow <- mean.flow-2.093*sqrt(sd(flow.cfs)^2/n.flow) #lower bound, 2.093 from t table 
uppflow <- mean.flow+2.093*sqrt(sd(flow.cfs)^2/n.flow) #upper bound

int <- mean.flow-lowflow

print(paste("Mean of", mean.flow, "+/-", int, "from", lowflow, "to", uppflow, "cfs"))
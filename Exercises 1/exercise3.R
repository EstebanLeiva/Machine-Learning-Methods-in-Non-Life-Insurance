
#PART 1

#Data and estimators
data <- rnorm(10000,0,1)
median <- median(data)
mean <- mean(data)

#Losses 
L1_distance <- function(x1,x2){
  return(x1-x2)
}
L2_squared <- function(x1,x2){
  return((x1-x2)**2)
}
#Empirical risk 
empirical_risk <- function(data,loss,estimator){
  R <- 0
  for(i in 1:length(data)){
    R <- R + loss(data[i],estimator)
    return(R)
  }
}

R_L1_median <- empirical_risk(data,L1_distance,median)
R_L1_mean <- empirical_risk(data,L1_distance,mean)
R_L2_median <- empirical_risk(data,L2_squared,median)
R_L2_mean <- empirical_risk(data,L2_squared,mean)


#PART 2

data <- rt(10000, 1)
median <- median(data)
mean <- mean(data)

R_L1_median <- empirical_risk(data,L1_distance,median)
R_L1_mean <- empirical_risk(data,L1_distance,mean)
R_L2_median <- empirical_risk(data,L2_squared,median)
R_L2_mean <- empirical_risk(data,L2_squared,mean)

#Regarding the normal distribution it can be seen that the loss function affects significantly the emprirical risk values, however in both functions the best estimator was found to be the median. 
#Regarding the tstudent distribution it was found that the mean was far better estimator than the median, in contrast with the normal distribution, which had similar results. 

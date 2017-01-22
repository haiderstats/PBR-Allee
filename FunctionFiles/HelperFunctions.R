#This is the file that creates the functions needed to run the analysis for:
#Incorporating the Allee Effect into the Potential Biological Removal calculation.
#The structure for this is creating many helper functions for one large function call 
#later on. 


#Load in the libraries we will use.
library(reshape2); library(ggplot2); library(gridExtra); library(grid)



#logisticMod is used to get population size from a logisitc model given the population (n), 
#the growth rate (r), the carrying capacity (K), and the density dependence parameter (theta).
#When theta = 1, density dependence is linear. For the paper, we use theta = 1. This corresponds
#to equation 6 in the paper.

logisticMod = function(n, r, K, theta){
  population = n + n * r * (1- (n/K)^theta)
  return(population)
}

#populationSample is used to get a sample from the true population.
#This way we introduce sampling error into our population estimates.
#This equation follows a log normal distribution, see equation 8 in the paper.
#Here, n is the population size and cv is the coeffcient of variation.

#If the population sample is 0 then the new sample should also be 0, since the population has died.

populationSample = function(n, cv){
  
  if( n >0){
    newSample = exp(log(n/sqrt(1 + cv^2))+ rnorm(1,0,1)*sqrt(log(1 + cv^2)))
  }
  else{
    newSample = 0
  }
  return(newSample)
}

#nMinEstimation is used to estimate nMin as a Perc. of the log-normal distribution. This is equation 9 in the paper.
#nHat is the estimated population. This will be that value that populationSample returns.
#cv is the coeffcient of variation, and z is the standard normal variate used for getting Percentiles. 
#For example, z = 1.96 is associated with the 2.5th Percentile. and .842 is associated with the 20th Percentile. 

nMinEstimation= function(nHat, cv, z){
  
  nMin = nHat/ (exp(z*sqrt(log(1 + cv^2))))
  
  return(nMin)
}

#pbrLogistic is used to calculate the PBR (potential biological removal) levels for the logisitic model. 
#This is equation 1 from the paper
#r is the maximum growth rate,nMin is the estimate of the population size -gotten from the nMinEstimation function
#f is the safety/recovery factor.

pbrLogistic = function(r, nMin, f){
  return((r/2)*nMin*f) 
}


#simulateLogModel creates a single simulation (of t years), following the steps as specified in the paper.
#It starts off with an initial population (initialPop), usually a set fraction of the carrying capacity (K). 
#Then we get the true population size from the logisticMod function.
#We estimate the true population size using popEstimate and then we get our nMin calculation using nMinEstimation. 
#Then we can find the level of PBR and subtract that from our population size.
#From there we give that as the population size of the next year.

simulateLogModel = function(initialPop, r, K, theta, t, cv, z, f){
  population = c()
  population[1] = initialPop
  
  for(i in 2:(t+1)){
    popSize = logisticMod(population[i-1], r, K, theta)
    
    popEstimate = populationSample(popSize, cv)
    
    nMin = nMinEstimation(popEstimate, cv,z)
    
    PBR = pbrLogistic(r, nMin,f)
    
    newPBR = rnorm(1,PBR, 0.3*PBR)
    
    population[i] = popSize - newPBR
  }
  return(population)
}

#This function is used to simulate the population process many times.
#It takes in all the parameters needed and then does N number of simulations.
#It only needs to call simulateLogModel in order to be effective. 

monteCarloModelLog = function(initialPop, r, K, theta, t, cv, z, f, N){
  populationSimulations = data.frame(Year = 0:t)
  
  for(i in 1:N){
    simulation = simulateLogModel(initialPop, r, K, theta, t, cv, z, f)
    
    #This is used to create a nice data structure to work with.
    populationSimulations = cbind.data.frame(populationSimulations, simulation)
  }
  return(populationSimulations)
}

#The following funcitons follow a similar process as the previous ones. However,
#now the Allee effect has been introduced.

#alleeMod is used to get population size from a model with an added allee effect
#given the population this year (n), the growth rate (r), the carrying capacity (K),
#the minimum a population size can be (alleeThreshold), 
#the density dependence parameter for the logisitc part (theta1), 
#and the density dependence parameter for the allee effect part (theta2).

#This function corresponds to equation 5 from the paper.

alleeMod = function(n, r, K, alleeThreshold, theta1, theta2){
  
  population = n + ((alleeThreshold * r)/(K - alleeThreshold)) * n * (1- (n/K)^theta1) * ((n/alleeThreshold)^theta2 - 1)
  return(population) 
}


#NOTE: populationSample and nMinEstimation (previous functions) give the quantitatively same results
#Even when there is an added allee effect. Thus we do not need to rewrite them.

#helpForPBRAlee is used to calculate the PBR (potential biological removal) levels for the model with the added Allee effect.
#Since the equation for calculating PBR for the added Allee effect model is very complicated we choose to do simulations 
#To find the MNPL of the population. h is our harvesting factor (PBR as a proportion).

helpForPBRAllee = function(rmax, K, nMin){
  pbrVec = c()
  hVec = c()
  
  #We run valus of h starting from 0.0001.
  for(i in 0:10000){
    h = i/10000
    
    #This is the calculation used to find the value of PBR for a given h.
    pbrVal = h*((K+nMin)/2 + (sqrt((K-nMin)^2-(4*nMin*K*h)/((nMin * rmax)/(K - nMin))))/2)
    
    #This if statement catches when PBR has gone past the threshold of real numbers.
    if(pbrVal < 0 || is.nan(pbrVal)){
      break
    }
    #This saves all the values of the PBR and the proportional harvest so that we can maximize PBR.
    pbrVec[i] = pbrVal
    hVec[i] = h
  }
  return(cbind.data.frame(h = hVec, PBR =pbrVec))
}


#pbrAllee is used to calculate the max level of h that can be used for PBR. Then we perform the PBR calculation.
pbrAllee = function(K, r, alleeThreshold, nMin, f){
  
  #We suppressWarnings since it will give us one warning that one of our values does not exist. (We get one value that is non-real)
  #From the square root in our PBR calculation.
  
  pbrCalc = suppressWarnings(helpForPBRAllee(r,K, alleeThreshold))
  
  rowNum = which.max(pbrCalc$PBR)
  
  h = pbrCalc[rowNum,1]
  
  #return the PBR calculation.
  return(h*nMin*f) 
  
}

#simulateAlleeModel creates a single simulation (of t years), following the steps from Wades paper. 
#It starts off with an initial population (initialPop), usually a set fraction of the carrying capacity (K). 
#Then we get the true population size from the logisticMod function.
#We estimate the true population size using popEstimate and then we get our nMin calculation using nMinEstimation. 
#Then we can find the level of PBR and subtract that from our population size.
#From there we give that as the population size of the next year.
#The only difference between this and the simulateLogModel is that the logModel does not take into account the alleeThreshold parameter.

simulateAlleeModel = function(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold){
  population = c()
  population[1] = initialPop
  
  for(i in 2:(t+1)){
    
    popSize = alleeMod(population[i-1], r, K, alleeThreshold, theta1, theta2)
    
    popEstimate = populationSample(popSize, cv)
    
    nMin = nMinEstimation(popEstimate, cv,z)
    
    PBR = pbrAllee(K, r, alleeThreshold,nMin,f)

    newPBR = rnorm(1,PBR, 0.3*PBR)
    
    population[i] = popSize - newPBR
    
  }
  return(population)
}

#We are also choosing to simulate a population exhibiting an Allee effect, however, we use the PBR calculation from the log model.
simulateAlleeModelWithNormPBR = function(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold){
  population = c()
  population[1] = initialPop
  
  for(i in 2:(t+1)){
    
    popSize = alleeMod(population[i-1], r, K, alleeThreshold, theta1, theta2)
    
    popEstimate = populationSample(popSize, cv)
    
    nMin = nMinEstimation(popEstimate, cv,z)
    
    PBR = pbrLogistic(r, nMin,f)
    #print(PBR)
    newPBR = rnorm(1,PBR, 0.3*PBR)
    
    population[i] = popSize - newPBR
    
  }
  return(population)
}


#This function is used to simulate the population process many times.
#It takes in all the parameters needed and then does N number of simulations. 
#It only needs to call simulateAlleeModel in order to be effective. 
monteCarloModelAllee = function(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold, N){
  populationSimulations = data.frame(Year = 0:t)
  
  for(i in 1:N){
    simulation = simulateAlleeModel(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold)
    populationSimulations = cbind.data.frame(populationSimulations, simulation)
  }
  return(populationSimulations)
}

#This function is used to simulate the population process many times.
#It takes in all the parameters needed and then does N number of simulations. 
#It only needs to call simulateAlleeModelWithNormPBR in order to be effective.
monteCarloModelAlleeNormPBR = function(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold, N){
  populationSimulations = data.frame(Year = 0:t)
  
  for(i in 1:N){
    simulation = simulateAlleeModelWithNormPBR(initialPop, r, K, theta1, theta2, t, cv, z, f, alleeThreshold)
    populationSimulations = cbind.data.frame(populationSimulations, simulation)
  }
  return(populationSimulations)
}

#Because we have populations that have an Allee effect we cannot start them below the Allee threshold (The lower equilibrium.)
#For this reason, we calcualte the lower equilibrium and use a value slightly above this as our initial population (given the simulation).
findEquilibrium = function(rmax, K, alleeThreshold, h){
  x = sqrt((K-alleeThreshold)^2 - 4*((h*alleeThreshold*K)/((alleeThreshold*rmax)/(K - alleeThreshold))))/2
  return(c((K + alleeThreshold)/2 + x, (K + alleeThreshold)/2 - x))
}


#This function will maximize our h and just return of the value of h. This way we can find the equilibrium for our population.
biggestTakeAllee = function(rmax, K, alleeThreshold){
  alleePBR = suppressWarnings(helpForPBRAllee(rmax,K,alleeThreshold))
  
  maximum = alleePBR[which.max(alleePBR$PBR),]
  
  return(maximum)
}

#The following function is to find the values for error bars.
#Taken from: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) { 
    if (na.rm) sum(!is.na(x))
    else       length(x)
  } 
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     Lq   = unname(quantile(xx[[col]], .05)),
                     Hq = unname(quantile(xx[[col]], .95))
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  
  
  return(datac)
}

#get_legend is used to create multiple plots in one figure. This takes the legend from a plot and then uses it for the full figure 
#(Typically 4 plots, 1 figure). This was taken from a response on stackoverflow.com. The original function can
#be found here: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots.
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

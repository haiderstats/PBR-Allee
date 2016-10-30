source('./FunctionFiles/HelperFunctions.R')

#This file is used for the carrying capacity simulation.
#NOTE THAT THIS SIMULATION WILL PROBABLY TAKE ABOUT A DAY TO RUN.
#For each population and CV we have lots of different values of the recovery factor,
#thus the number of simulation is quite high

#Note our choice for z reflects the 15th percentile.

#Set the seed so we can replicate our results.
set.seed(42)

#Find our equilibirum.
DetPBR = biggestTakeAllee(.04, 10000, 1000)$h
EQ = findEquilibrium(.04, 10000, 1000, DetPBR)

#Get the second equilibrium (Stable)
startingPop = EQ[2] + .05*10000

#Low CV, Cetceans.
#Do the simulations.

#Note we take the last year in the data structure, and all the 
#columns that aren't the year so the 2nd through the 2001st.
#We divide by 10000 to get a proporiton of K instead of a specific number.
alleeSmallCet1 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.95 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .95, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.9 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .9, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.85 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .85, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.8 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .8, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.75 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .75, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.7 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .7, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.65 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .65, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.6 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .6, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.55 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .55, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .5, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .45, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.4 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .4, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .35, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.3 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .3, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .25, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.2 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .2, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .15, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.1 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallCet.05 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .05, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 

#Label the simulations
alleeSmallCet1 = cbind.data.frame(Factor = 1, alleeSmallCet1)
alleeSmallCet.95 = cbind.data.frame(Factor = .95, alleeSmallCet.95)
alleeSmallCet.9 = cbind.data.frame(Factor = .9, alleeSmallCet.9)
alleeSmallCet.85 = cbind.data.frame(Factor = .85, alleeSmallCet.85)
alleeSmallCet.8 = cbind.data.frame(Factor = .8, alleeSmallCet.8)
alleeSmallCet.75 = cbind.data.frame(Factor = .75, alleeSmallCet.75)
alleeSmallCet.7 = cbind.data.frame(Factor = .7, alleeSmallCet.7)
alleeSmallCet.65 = cbind.data.frame(Factor = .65, alleeSmallCet.65)
alleeSmallCet.6 = cbind.data.frame(Factor = .6, alleeSmallCet.6)
alleeSmallCet.55 = cbind.data.frame(Factor = .55, alleeSmallCet.55)
alleeSmallCet.5 = cbind.data.frame(Factor = .5, alleeSmallCet.5)
alleeSmallCet.45 = cbind.data.frame(Factor = .45, alleeSmallCet.45)
alleeSmallCet.4 = cbind.data.frame(Factor = .4, alleeSmallCet.4)
alleeSmallCet.35 = cbind.data.frame(Factor = .35, alleeSmallCet.35)
alleeSmallCet.3 = cbind.data.frame(Factor = .3, alleeSmallCet.3)
alleeSmallCet.25 = cbind.data.frame(Factor = .25, alleeSmallCet.25)
alleeSmallCet.2 = cbind.data.frame(Factor = .2, alleeSmallCet.2)
alleeSmallCet.15 = cbind.data.frame(Factor = .15, alleeSmallCet.15)
alleeSmallCet.1 = cbind.data.frame(Factor = .10, alleeSmallCet.1)
alleeSmallCet.05 = cbind.data.frame(Factor = .05, alleeSmallCet.05)

#Get the data in long form.
alleeSmallCet1 = melt(alleeSmallCet1, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.95 = melt(alleeSmallCet.95, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.9 = melt(alleeSmallCet.9, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.85 = melt(alleeSmallCet.85, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.8 = melt(alleeSmallCet.8, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.75 = melt(alleeSmallCet.75, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.7 = melt(alleeSmallCet.7, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.65 = melt(alleeSmallCet.65, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.6 = melt(alleeSmallCet.6, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.55 = melt(alleeSmallCet.55, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.5 = melt(alleeSmallCet.5, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.45 = melt(alleeSmallCet.45, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.4 = melt(alleeSmallCet.4, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.35 = melt(alleeSmallCet.35, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.3 = melt(alleeSmallCet.3, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.25 = melt(alleeSmallCet.25, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.2 = melt(alleeSmallCet.2, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.15 = melt(alleeSmallCet.15, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.1 = melt(alleeSmallCet.1, id.vars = "Factor")[,c(1,3)]
alleeSmallCet.05 = melt(alleeSmallCet.05, id.vars = "Factor")[,c(1,3)]

#Combine the data.
allAlleeSmallCet = rbind.data.frame(alleeSmallCet1,
                                 alleeSmallCet.95,
                                 alleeSmallCet.9,
                                 alleeSmallCet.85,
                                 alleeSmallCet.8,
                                 alleeSmallCet.75,
                                 alleeSmallCet.7,
                                 alleeSmallCet.65,
                                 alleeSmallCet.6,
                                 alleeSmallCet.55,
                                 alleeSmallCet.5,
                                 alleeSmallCet.45,
                                 alleeSmallCet.4,
                                 alleeSmallCet.35,
                                 alleeSmallCet.3,
                                 alleeSmallCet.25,
                                 alleeSmallCet.2,
                                 alleeSmallCet.15,
                                 alleeSmallCet.1,
                                 alleeSmallCet.05)

#Label the data.
allAlleeSmallCet = cbind.data.frame(Type = "Cetacean, CV = 0.2", allAlleeSmallCet)


#High CV, Cetceans.

#Same process as above.
alleeBigCet1 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.95 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .95, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.9 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .9, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.85 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .85, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.8 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .8, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.75 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .75, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.7 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .7, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.65 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .65, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.6 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .6, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.55 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .55, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .5, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .45, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.4 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .4, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .35, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.3 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .3, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .25, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.2 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .2, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .15, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.1 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigCet.05 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .05, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 


alleeBigCet1 = cbind.data.frame(Factor = 1, alleeBigCet1)
alleeBigCet.95 = cbind.data.frame(Factor = .95, alleeBigCet.95)
alleeBigCet.9 = cbind.data.frame(Factor = .9, alleeBigCet.9)
alleeBigCet.85 = cbind.data.frame(Factor = .85, alleeBigCet.85)
alleeBigCet.8 = cbind.data.frame(Factor = .8, alleeBigCet.8)
alleeBigCet.75 = cbind.data.frame(Factor = .75, alleeBigCet.75)
alleeBigCet.7 = cbind.data.frame(Factor = .7, alleeBigCet.7)
alleeBigCet.65 = cbind.data.frame(Factor = .65, alleeBigCet.65)
alleeBigCet.6 = cbind.data.frame(Factor = .6, alleeBigCet.6)
alleeBigCet.55 = cbind.data.frame(Factor = .55, alleeBigCet.55)
alleeBigCet.5 = cbind.data.frame(Factor = .5, alleeBigCet.5)
alleeBigCet.45 = cbind.data.frame(Factor = .45, alleeBigCet.45)
alleeBigCet.4 = cbind.data.frame(Factor = .4, alleeBigCet.4)
alleeBigCet.35 = cbind.data.frame(Factor = .35, alleeBigCet.35)
alleeBigCet.3 = cbind.data.frame(Factor = .3, alleeBigCet.3)
alleeBigCet.25 = cbind.data.frame(Factor = .25, alleeBigCet.25)
alleeBigCet.2 = cbind.data.frame(Factor = .2, alleeBigCet.2)
alleeBigCet.15 = cbind.data.frame(Factor = .15, alleeBigCet.15)
alleeBigCet.1 = cbind.data.frame(Factor = .10, alleeBigCet.1)
alleeBigCet.05 = cbind.data.frame(Factor = .05, alleeBigCet.05)


alleeBigCet1 = melt(alleeBigCet1, id.vars = "Factor")[,c(1,3)]
alleeBigCet.95 = melt(alleeBigCet.95, id.vars = "Factor")[,c(1,3)]
alleeBigCet.9 = melt(alleeBigCet.9, id.vars = "Factor")[,c(1,3)]
alleeBigCet.85 = melt(alleeBigCet.85, id.vars = "Factor")[,c(1,3)]
alleeBigCet.8 = melt(alleeBigCet.8, id.vars = "Factor")[,c(1,3)]
alleeBigCet.75 = melt(alleeBigCet.75, id.vars = "Factor")[,c(1,3)]
alleeBigCet.7 = melt(alleeBigCet.7, id.vars = "Factor")[,c(1,3)]
alleeBigCet.65 = melt(alleeBigCet.65, id.vars = "Factor")[,c(1,3)]
alleeBigCet.6 = melt(alleeBigCet.6, id.vars = "Factor")[,c(1,3)]
alleeBigCet.55 = melt(alleeBigCet.55, id.vars = "Factor")[,c(1,3)]
alleeBigCet.5 = melt(alleeBigCet.5, id.vars = "Factor")[,c(1,3)]
alleeBigCet.45 = melt(alleeBigCet.45, id.vars = "Factor")[,c(1,3)]
alleeBigCet.4 = melt(alleeBigCet.4, id.vars = "Factor")[,c(1,3)]
alleeBigCet.35 = melt(alleeBigCet.35, id.vars = "Factor")[,c(1,3)]
alleeBigCet.3 = melt(alleeBigCet.3, id.vars = "Factor")[,c(1,3)]
alleeBigCet.25 = melt(alleeBigCet.25, id.vars = "Factor")[,c(1,3)]
alleeBigCet.2 = melt(alleeBigCet.2, id.vars = "Factor")[,c(1,3)]
alleeBigCet.15 = melt(alleeBigCet.15, id.vars = "Factor")[,c(1,3)]
alleeBigCet.1 = melt(alleeBigCet.1, id.vars = "Factor")[,c(1,3)]
alleeBigCet.05 = melt(alleeBigCet.05, id.vars = "Factor")[,c(1,3)]


allAlleeBigCet = rbind.data.frame(alleeBigCet1,
                                    alleeBigCet.95,
                                    alleeBigCet.9,
                                    alleeBigCet.85,
                                    alleeBigCet.8,
                                    alleeBigCet.75,
                                    alleeBigCet.7,
                                    alleeBigCet.65,
                                    alleeBigCet.6,
                                    alleeBigCet.55,
                                    alleeBigCet.5,
                                    alleeBigCet.45,
                                    alleeBigCet.4,
                                    alleeBigCet.35,
                                    alleeBigCet.3,
                                    alleeBigCet.25,
                                    alleeBigCet.2,
                                    alleeBigCet.15,
                                    alleeBigCet.1,
                                    alleeBigCet.05)


allAlleeBigCet = cbind.data.frame(Type = "Cetacean, CV = 0.8 ", allAlleeBigCet)

##############

#Low CV, Bats.

#Same process, different level of growth rate.
DetPBR = biggestTakeAllee(.1, 10000, 500)$h
EQ = findEquilibrium(.1, 10000, 500, DetPBR)[2]

startingPop = EQ + .05*10000

alleeSmallBat1 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.95 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .95, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.9 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .9, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.85 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .85, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.8 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .8, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.75 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .75, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.7 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .7, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.65 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .65, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.6 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .6, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.55 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .55, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.5 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .5, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.45 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .45, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.4 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .4, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.35 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .35, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.3 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .3, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.25 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .25, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.2 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .2, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.15 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .15, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.1 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeSmallBat.05 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .05, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 


alleeSmallBat1 = cbind.data.frame(Factor = 1, alleeSmallBat1)
alleeSmallBat.95 = cbind.data.frame(Factor = .95, alleeSmallBat.95)
alleeSmallBat.9 = cbind.data.frame(Factor = .9, alleeSmallBat.9)
alleeSmallBat.85 = cbind.data.frame(Factor = .85, alleeSmallBat.85)
alleeSmallBat.8 = cbind.data.frame(Factor = .8, alleeSmallBat.8)
alleeSmallBat.75 = cbind.data.frame(Factor = .75, alleeSmallBat.75)
alleeSmallBat.7 = cbind.data.frame(Factor = .7, alleeSmallBat.7)
alleeSmallBat.65 = cbind.data.frame(Factor = .65, alleeSmallBat.65)
alleeSmallBat.6 = cbind.data.frame(Factor = .6, alleeSmallBat.6)
alleeSmallBat.55 = cbind.data.frame(Factor = .55, alleeSmallBat.55)
alleeSmallBat.5 = cbind.data.frame(Factor = .5, alleeSmallBat.5)
alleeSmallBat.45 = cbind.data.frame(Factor = .45, alleeSmallBat.45)
alleeSmallBat.4 = cbind.data.frame(Factor = .4, alleeSmallBat.4)
alleeSmallBat.35 = cbind.data.frame(Factor = .35, alleeSmallBat.35)
alleeSmallBat.3 = cbind.data.frame(Factor = .3, alleeSmallBat.3)
alleeSmallBat.25 = cbind.data.frame(Factor = .25, alleeSmallBat.25)
alleeSmallBat.2 = cbind.data.frame(Factor = .2, alleeSmallBat.2)
alleeSmallBat.15 = cbind.data.frame(Factor = .15, alleeSmallBat.15)
alleeSmallBat.1 = cbind.data.frame(Factor = .10, alleeSmallBat.1)
alleeSmallBat.05 = cbind.data.frame(Factor = .05, alleeSmallBat.05)


alleeSmallBat1 = melt(alleeSmallBat1, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.95 = melt(alleeSmallBat.95, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.9 = melt(alleeSmallBat.9, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.85 = melt(alleeSmallBat.85, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.8 = melt(alleeSmallBat.8, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.75 = melt(alleeSmallBat.75, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.7 = melt(alleeSmallBat.7, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.65 = melt(alleeSmallBat.65, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.6 = melt(alleeSmallBat.6, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.55 = melt(alleeSmallBat.55, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.5 = melt(alleeSmallBat.5, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.45 = melt(alleeSmallBat.45, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.4 = melt(alleeSmallBat.4, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.35 = melt(alleeSmallBat.35, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.3 = melt(alleeSmallBat.3, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.25 = melt(alleeSmallBat.25, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.2 = melt(alleeSmallBat.2, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.15 = melt(alleeSmallBat.15, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.1 = melt(alleeSmallBat.1, id.vars = "Factor")[,c(1,3)]
alleeSmallBat.05 = melt(alleeSmallBat.05, id.vars = "Factor")[,c(1,3)]


allAlleeSmallBat = rbind.data.frame(alleeSmallBat1,
                                    alleeSmallBat.95,
                                    alleeSmallBat.9,
                                    alleeSmallBat.85,
                                    alleeSmallBat.8,
                                    alleeSmallBat.75,
                                    alleeSmallBat.7,
                                    alleeSmallBat.65,
                                    alleeSmallBat.6,
                                    alleeSmallBat.55,
                                    alleeSmallBat.5,
                                    alleeSmallBat.45,
                                    alleeSmallBat.4,
                                    alleeSmallBat.35,
                                    alleeSmallBat.3,
                                    alleeSmallBat.25,
                                    alleeSmallBat.2,
                                    alleeSmallBat.15,
                                    alleeSmallBat.1,
                                    alleeSmallBat.05)

allAlleeSmallBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.2", allAlleeSmallBat)



#High CV, Bats.


alleeBigBat1 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.95 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .95, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.9 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .9, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.85 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .85, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.8 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .8, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.75 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .75, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.7 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .7, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.65 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .65, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.6 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .6, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.55 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .55, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.5 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .5, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.45 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .45, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.4 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .4, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.35 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .35, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.3 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .3, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.25 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .25, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.2 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .2, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.15 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .15, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.1 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .1, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 
alleeBigBat.05 = monteCarloModelAllee(initialPop = startingPop, r = 0.10, K = 10000, theta1 = 1, theta2 = 1, t = 1000, cv = .2, z = 1.036, f = .05, alleeThreshold = 1000, N = 2000)[1001, 2:2001]/10000 


alleeBigBat1 = cbind.data.frame(Factor = 1, alleeBigBat1)
alleeBigBat.95 = cbind.data.frame(Factor = .95, alleeBigBat.95)
alleeBigBat.9 = cbind.data.frame(Factor = .9, alleeBigBat.9)
alleeBigBat.85 = cbind.data.frame(Factor = .85, alleeBigBat.85)
alleeBigBat.8 = cbind.data.frame(Factor = .8, alleeBigBat.8)
alleeBigBat.75 = cbind.data.frame(Factor = .75, alleeBigBat.75)
alleeBigBat.7 = cbind.data.frame(Factor = .7, alleeBigBat.7)
alleeBigBat.65 = cbind.data.frame(Factor = .65, alleeBigBat.65)
alleeBigBat.6 = cbind.data.frame(Factor = .6, alleeBigBat.6)
alleeBigBat.55 = cbind.data.frame(Factor = .55, alleeBigBat.55)
alleeBigBat.5 = cbind.data.frame(Factor = .5, alleeBigBat.5)
alleeBigBat.45 = cbind.data.frame(Factor = .45, alleeBigBat.45)
alleeBigBat.4 = cbind.data.frame(Factor = .4, alleeBigBat.4)
alleeBigBat.35 = cbind.data.frame(Factor = .35, alleeBigBat.35)
alleeBigBat.3 = cbind.data.frame(Factor = .3, alleeBigBat.3)
alleeBigBat.25 = cbind.data.frame(Factor = .25, alleeBigBat.25)
alleeBigBat.2 = cbind.data.frame(Factor = .2, alleeBigBat.2)
alleeBigBat.15 = cbind.data.frame(Factor = .15, alleeBigBat.15)
alleeBigBat.1 = cbind.data.frame(Factor = .10, alleeBigBat.1)
alleeBigBat.05 = cbind.data.frame(Factor = .05, alleeBigBat.05)


alleeBigBat1 = melt(alleeBigBat1, id.vars = "Factor")[,c(1,3)]
alleeBigBat.95 = melt(alleeBigBat.95, id.vars = "Factor")[,c(1,3)]
alleeBigBat.9 = melt(alleeBigBat.9, id.vars = "Factor")[,c(1,3)]
alleeBigBat.85 = melt(alleeBigBat.85, id.vars = "Factor")[,c(1,3)]
alleeBigBat.8 = melt(alleeBigBat.8, id.vars = "Factor")[,c(1,3)]
alleeBigBat.75 = melt(alleeBigBat.75, id.vars = "Factor")[,c(1,3)]
alleeBigBat.7 = melt(alleeBigBat.7, id.vars = "Factor")[,c(1,3)]
alleeBigBat.65 = melt(alleeBigBat.65, id.vars = "Factor")[,c(1,3)]
alleeBigBat.6 = melt(alleeBigBat.6, id.vars = "Factor")[,c(1,3)]
alleeBigBat.55 = melt(alleeBigBat.55, id.vars = "Factor")[,c(1,3)]
alleeBigBat.5 = melt(alleeBigBat.5, id.vars = "Factor")[,c(1,3)]
alleeBigBat.45 = melt(alleeBigBat.45, id.vars = "Factor")[,c(1,3)]
alleeBigBat.4 = melt(alleeBigBat.4, id.vars = "Factor")[,c(1,3)]
alleeBigBat.35 = melt(alleeBigBat.35, id.vars = "Factor")[,c(1,3)]
alleeBigBat.3 = melt(alleeBigBat.3, id.vars = "Factor")[,c(1,3)]
alleeBigBat.25 = melt(alleeBigBat.25, id.vars = "Factor")[,c(1,3)]
alleeBigBat.2 = melt(alleeBigBat.2, id.vars = "Factor")[,c(1,3)]
alleeBigBat.15 = melt(alleeBigBat.15, id.vars = "Factor")[,c(1,3)]
alleeBigBat.1 = melt(alleeBigBat.1, id.vars = "Factor")[,c(1,3)]
alleeBigBat.05 = melt(alleeBigBat.05, id.vars = "Factor")[,c(1,3)]


allAlleeBigBat = rbind.data.frame(alleeBigBat1,
                                    alleeBigBat.95,
                                    alleeBigBat.9,
                                    alleeBigBat.85,
                                    alleeBigBat.8,
                                    alleeBigBat.75,
                                    alleeBigBat.7,
                                    alleeBigBat.65,
                                    alleeBigBat.6,
                                    alleeBigBat.55,
                                    alleeBigBat.5,
                                    alleeBigBat.45,
                                    alleeBigBat.4,
                                    alleeBigBat.35,
                                    alleeBigBat.3,
                                    alleeBigBat.25,
                                    alleeBigBat.2,
                                    alleeBigBat.15,
                                    alleeBigBat.1,
                                    alleeBigBat.05)



allAlleeBigBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.8 ", allAlleeBigBat)

#Combine it all
allAllee = rbind.data.frame(allAlleeSmallCet, allAlleeBigCet, allAlleeSmallBat, allAlleeBigBat)

#Get our error bars.
allAlleeDats = summarySE(allAllee, measurevar = 'value', groupvars = c('Factor','Type'))

#SAVE THE DATA TO A FILE!!!!

#####################

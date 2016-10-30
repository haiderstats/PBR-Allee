source('./FunctionFiles/HelperFunctions.R')

#This file is used for the 20 year performance criterion simulation. There is a lot of simulations here. We have to try varying levels 
#of z (our percentile) for both a recovery factor of 1 and a recovery factor of 0.5. This follows the same structure as all other simulations.
#Do the simulations, structure them, label them. Move onto next simulation group.


######################################################################################

#Recovery Factor = 1.

#Set the seed
set.seed(42)

#Get the lower EQ as our starting point.
DetPBR = biggestTakeAllee(.04, 10000, 1000)$h
EQ = findEquilibrium(.04, 10000, 1000, DetPBR)

startingPop = EQ[2] + .05*10000

#Note we take the last year in the data structure, and all the 
#columns that aren't the year so the 2nd through the 2001st.
#We divide by 10000 to get a proporiton of K instead of a specific number.

#Do the simulations
alleeSmallCet50 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.126, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet40 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.253, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.385, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet30 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.524, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.675, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet20 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.842, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet10 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.282, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.645, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet2.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.96, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 

#Structure the simulation
alleeSmallCet50 = cbind.data.frame(Percentile = 50, alleeSmallCet50)
alleeSmallCet45 = cbind.data.frame(Percentile = 45, alleeSmallCet45)
alleeSmallCet40 = cbind.data.frame(Percentile = 40, alleeSmallCet40)
alleeSmallCet35 = cbind.data.frame(Percentile = 35, alleeSmallCet35)
alleeSmallCet30 = cbind.data.frame(Percentile = 30, alleeSmallCet30)
alleeSmallCet25 = cbind.data.frame(Percentile = 25, alleeSmallCet25)
alleeSmallCet20 = cbind.data.frame(Percentile = 20, alleeSmallCet20)
alleeSmallCet15 = cbind.data.frame(Percentile = 15, alleeSmallCet15)
alleeSmallCet10 = cbind.data.frame(Percentile = 10, alleeSmallCet10)
alleeSmallCet5 = cbind.data.frame(Percentile = 5, alleeSmallCet5)
alleeSmallCet2.5 = cbind.data.frame(Percentile = 2.5, alleeSmallCet2.5)

#Long form data
alleeSmallCet50 = melt(alleeSmallCet50, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet45 = melt(alleeSmallCet45, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet40 = melt(alleeSmallCet40, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet35 = melt(alleeSmallCet35, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet30 = melt(alleeSmallCet30, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet25 = melt(alleeSmallCet25, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet20 = melt(alleeSmallCet20, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet15 = melt(alleeSmallCet15, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet10 = melt(alleeSmallCet10, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet5 = melt(alleeSmallCet5, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet2.5 = melt(alleeSmallCet2.5, id.vars = "Percentile")[,c(1,3)]

#Combine the data
allAlleeSmallCet = rbind.data.frame(alleeSmallCet50,
                                    alleeSmallCet45,
                                    alleeSmallCet40,
                                    alleeSmallCet35,
                                    alleeSmallCet30,
                                    alleeSmallCet25,
                                    alleeSmallCet20,
                                    alleeSmallCet15,
                                    alleeSmallCet10,
                                    alleeSmallCet5,
                                    alleeSmallCet2.5)

#Label the data
allAlleeSmallCet = cbind.data.frame(Type = "Cetacean, CV = 0.2", allAlleeSmallCet)

########

alleeBigCet50 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.126, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet40 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.253, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.385, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet30 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.524, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.675, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet20 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.842, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet10 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.282, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.645, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet2.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.96, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 


alleeBigCet50 = cbind.data.frame(Percentile = 50, alleeBigCet50)
alleeBigCet45 = cbind.data.frame(Percentile = 45, alleeBigCet45)
alleeBigCet40 = cbind.data.frame(Percentile = 40, alleeBigCet40)
alleeBigCet35 = cbind.data.frame(Percentile = 35, alleeBigCet35)
alleeBigCet30 = cbind.data.frame(Percentile = 30, alleeBigCet30)
alleeBigCet25 = cbind.data.frame(Percentile = 25, alleeBigCet25)
alleeBigCet20 = cbind.data.frame(Percentile = 20, alleeBigCet20)
alleeBigCet15 = cbind.data.frame(Percentile = 15, alleeBigCet15)
alleeBigCet10 = cbind.data.frame(Percentile = 10, alleeBigCet10)
alleeBigCet5 = cbind.data.frame(Percentile = 5, alleeBigCet5)
alleeBigCet2.5 = cbind.data.frame(Percentile = 2.5, alleeBigCet2.5)

alleeBigCet50 = melt(alleeBigCet50, id.vars = "Percentile")[,c(1,3)]
alleeBigCet45 = melt(alleeBigCet45, id.vars = "Percentile")[,c(1,3)]
alleeBigCet40 = melt(alleeBigCet40, id.vars = "Percentile")[,c(1,3)]
alleeBigCet35 = melt(alleeBigCet35, id.vars = "Percentile")[,c(1,3)]
alleeBigCet30 = melt(alleeBigCet30, id.vars = "Percentile")[,c(1,3)]
alleeBigCet25 = melt(alleeBigCet25, id.vars = "Percentile")[,c(1,3)]
alleeBigCet20 = melt(alleeBigCet20, id.vars = "Percentile")[,c(1,3)]
alleeBigCet15 = melt(alleeBigCet15, id.vars = "Percentile")[,c(1,3)]
alleeBigCet10 = melt(alleeBigCet10, id.vars = "Percentile")[,c(1,3)]
alleeBigCet5 = melt(alleeBigCet5, id.vars = "Percentile")[,c(1,3)]
alleeBigCet2.5 = melt(alleeBigCet2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeBigCet = rbind.data.frame(alleeBigCet50,
                                  alleeBigCet45,
                                  alleeBigCet40,
                                  alleeBigCet35,
                                  alleeBigCet30,
                                  alleeBigCet25,
                                  alleeBigCet20,
                                  alleeBigCet15,
                                  alleeBigCet10,
                                  alleeBigCet5,
                                  alleeBigCet2.5)
allAlleeBigCet = cbind.data.frame(Type = "Cetacean, CV = 0.8 ", allAlleeBigCet)

##############################################################################

#Higher growth rate population (r = .10)

DetPBR = biggestTakeAllee(.1, 10000, 500)$h
EQ = findEquilibrium(.1, 10000, 500, DetPBR)

startingPop = EQ[2] + .05*10000

alleeSmallBat50 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat45 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.126, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat40 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.253, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat35 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.385, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat30 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.524, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat25 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.675, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat20 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.842, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat15 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat10 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.282, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.645, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat2.5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.96, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 

alleeSmallBat50 = cbind.data.frame(Percentile = 50, alleeSmallBat50)
alleeSmallBat45 = cbind.data.frame(Percentile = 45, alleeSmallBat45)
alleeSmallBat40 = cbind.data.frame(Percentile = 40, alleeSmallBat40)
alleeSmallBat35 = cbind.data.frame(Percentile = 35, alleeSmallBat35)
alleeSmallBat30 = cbind.data.frame(Percentile = 30, alleeSmallBat30)
alleeSmallBat25 = cbind.data.frame(Percentile = 25, alleeSmallBat25)
alleeSmallBat20 = cbind.data.frame(Percentile = 20, alleeSmallBat20)
alleeSmallBat15 = cbind.data.frame(Percentile = 15, alleeSmallBat15)
alleeSmallBat10 = cbind.data.frame(Percentile = 10, alleeSmallBat10)
alleeSmallBat5 = cbind.data.frame(Percentile = 5, alleeSmallBat5)
alleeSmallBat2.5 = cbind.data.frame(Percentile = 2.5, alleeSmallBat2.5)

alleeSmallBat50 = melt(alleeSmallBat50, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat45 = melt(alleeSmallBat45, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat40 = melt(alleeSmallBat40, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat35 = melt(alleeSmallBat35, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat30 = melt(alleeSmallBat30, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat25 = melt(alleeSmallBat25, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat20 = melt(alleeSmallBat20, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat15 = melt(alleeSmallBat15, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat10 = melt(alleeSmallBat10, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat5 = melt(alleeSmallBat5, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat2.5 = melt(alleeSmallBat2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeSmallBat = rbind.data.frame(alleeSmallBat50,
                                    alleeSmallBat45,
                                    alleeSmallBat40,
                                    alleeSmallBat35,
                                    alleeSmallBat30,
                                    alleeSmallBat25,
                                    alleeSmallBat20,
                                    alleeSmallBat15,
                                    alleeSmallBat10,
                                    alleeSmallBat5,
                                    alleeSmallBat2.5)

allAlleeSmallBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.2", allAlleeSmallBat)

########

alleeBigBat50 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat45 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.126, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat40 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.253, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat35 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.385, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat30 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.524, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat25 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.675, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat20 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.842, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat15 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.036, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat10 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.282, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.645, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat2.5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.96, f = 1, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 


alleeBigBat50 = cbind.data.frame(Percentile = 50, alleeBigBat50)
alleeBigBat45 = cbind.data.frame(Percentile = 45, alleeBigBat45)
alleeBigBat40 = cbind.data.frame(Percentile = 40, alleeBigBat40)
alleeBigBat35 = cbind.data.frame(Percentile = 35, alleeBigBat35)
alleeBigBat30 = cbind.data.frame(Percentile = 30, alleeBigBat30)
alleeBigBat25 = cbind.data.frame(Percentile = 25, alleeBigBat25)
alleeBigBat20 = cbind.data.frame(Percentile = 20, alleeBigBat20)
alleeBigBat15 = cbind.data.frame(Percentile = 15, alleeBigBat15)
alleeBigBat10 = cbind.data.frame(Percentile = 10, alleeBigBat10)
alleeBigBat5 = cbind.data.frame(Percentile = 5, alleeBigBat5)
alleeBigBat2.5 = cbind.data.frame(Percentile = 2.5, alleeBigBat2.5)

alleeBigBat50 = melt(alleeBigBat50, id.vars = "Percentile")[,c(1,3)]
alleeBigBat45 = melt(alleeBigBat45, id.vars = "Percentile")[,c(1,3)]
alleeBigBat40 = melt(alleeBigBat40, id.vars = "Percentile")[,c(1,3)]
alleeBigBat35 = melt(alleeBigBat35, id.vars = "Percentile")[,c(1,3)]
alleeBigBat30 = melt(alleeBigBat30, id.vars = "Percentile")[,c(1,3)]
alleeBigBat25 = melt(alleeBigBat25, id.vars = "Percentile")[,c(1,3)]
alleeBigBat20 = melt(alleeBigBat20, id.vars = "Percentile")[,c(1,3)]
alleeBigBat15 = melt(alleeBigBat15, id.vars = "Percentile")[,c(1,3)]
alleeBigBat10 = melt(alleeBigBat10, id.vars = "Percentile")[,c(1,3)]
alleeBigBat5 = melt(alleeBigBat5, id.vars = "Percentile")[,c(1,3)]
alleeBigBat2.5 = melt(alleeBigBat2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeBigBat = rbind.data.frame(alleeBigBat50,
                                  alleeBigBat45,
                                  alleeBigBat40,
                                  alleeBigBat35,
                                  alleeBigBat30,
                                  alleeBigBat25,
                                  alleeBigBat20,
                                  alleeBigBat15,
                                  alleeBigBat10,
                                  alleeBigBat5,
                                  alleeBigBat2.5)
allAlleeBigBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.8 ", allAlleeBigBat)

#Combine all the data
allAllee = rbind.data.frame(allAlleeSmallCet, allAlleeBigCet, allAlleeSmallBat, allAlleeBigBat)

#Get our error bounds
allAlleeDats = summarySE(allAllee, measurevar = 'value', groupvars = c('Percentile','Type'))



######################################################################################

#Recovery Factor = 0.5.
set.seed(42)

DetPBR = biggestTakeAllee(.04, 10000, 1000)$h
EQ = findEquilibrium(.04, 10000, 1000, DetPBR)

startingPop = EQ[2] + .05*10000

alleeSmallCet50 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.126, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet40 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.253, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.385, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet30 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.524, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.675, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet20 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.842, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.036, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet10 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.282, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.645, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallCet2.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.96, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 

alleeSmallCet50 = cbind.data.frame(Percentile = 50, alleeSmallCet50)
alleeSmallCet45 = cbind.data.frame(Percentile = 45, alleeSmallCet45)
alleeSmallCet40 = cbind.data.frame(Percentile = 40, alleeSmallCet40)
alleeSmallCet35 = cbind.data.frame(Percentile = 35, alleeSmallCet35)
alleeSmallCet30 = cbind.data.frame(Percentile = 30, alleeSmallCet30)
alleeSmallCet25 = cbind.data.frame(Percentile = 25, alleeSmallCet25)
alleeSmallCet20 = cbind.data.frame(Percentile = 20, alleeSmallCet20)
alleeSmallCet15 = cbind.data.frame(Percentile = 15, alleeSmallCet15)
alleeSmallCet10 = cbind.data.frame(Percentile = 10, alleeSmallCet10)
alleeSmallCet5 = cbind.data.frame(Percentile = 5, alleeSmallCet5)
alleeSmallCet2.5 = cbind.data.frame(Percentile = 2.5, alleeSmallCet2.5)

alleeSmallCet50 = melt(alleeSmallCet50, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet45 = melt(alleeSmallCet45, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet40 = melt(alleeSmallCet40, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet35 = melt(alleeSmallCet35, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet30 = melt(alleeSmallCet30, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet25 = melt(alleeSmallCet25, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet20 = melt(alleeSmallCet20, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet15 = melt(alleeSmallCet15, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet10 = melt(alleeSmallCet10, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet5 = melt(alleeSmallCet5, id.vars = "Percentile")[,c(1,3)]
alleeSmallCet2.5 = melt(alleeSmallCet2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeSmallCet = rbind.data.frame(alleeSmallCet50,
                                    alleeSmallCet45,
                                    alleeSmallCet40,
                                    alleeSmallCet35,
                                    alleeSmallCet30,
                                    alleeSmallCet25,
                                    alleeSmallCet20,
                                    alleeSmallCet15,
                                    alleeSmallCet10,
                                    alleeSmallCet5,
                                    alleeSmallCet2.5)

allAlleeSmallCet = cbind.data.frame(Type = "Cetacean, CV = 0.2", allAlleeSmallCet)



alleeBigCet50 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet45 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.126, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet40 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.253, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet35 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.385, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet30 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.524, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet25 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.675, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet20 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.842, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet15 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.036, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet10 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.282, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.645, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigCet2.5 = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.96, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 


alleeBigCet50 = cbind.data.frame(Percentile = 50, alleeBigCet50)
alleeBigCet45 = cbind.data.frame(Percentile = 45, alleeBigCet45)
alleeBigCet40 = cbind.data.frame(Percentile = 40, alleeBigCet40)
alleeBigCet35 = cbind.data.frame(Percentile = 35, alleeBigCet35)
alleeBigCet30 = cbind.data.frame(Percentile = 30, alleeBigCet30)
alleeBigCet25 = cbind.data.frame(Percentile = 25, alleeBigCet25)
alleeBigCet20 = cbind.data.frame(Percentile = 20, alleeBigCet20)
alleeBigCet15 = cbind.data.frame(Percentile = 15, alleeBigCet15)
alleeBigCet10 = cbind.data.frame(Percentile = 10, alleeBigCet10)
alleeBigCet5 = cbind.data.frame(Percentile = 5, alleeBigCet5)
alleeBigCet2.5 = cbind.data.frame(Percentile = 2.5, alleeBigCet2.5)

alleeBigCet50 = melt(alleeBigCet50, id.vars = "Percentile")[,c(1,3)]
alleeBigCet45 = melt(alleeBigCet45, id.vars = "Percentile")[,c(1,3)]
alleeBigCet40 = melt(alleeBigCet40, id.vars = "Percentile")[,c(1,3)]
alleeBigCet35 = melt(alleeBigCet35, id.vars = "Percentile")[,c(1,3)]
alleeBigCet30 = melt(alleeBigCet30, id.vars = "Percentile")[,c(1,3)]
alleeBigCet25 = melt(alleeBigCet25, id.vars = "Percentile")[,c(1,3)]
alleeBigCet20 = melt(alleeBigCet20, id.vars = "Percentile")[,c(1,3)]
alleeBigCet15 = melt(alleeBigCet15, id.vars = "Percentile")[,c(1,3)]
alleeBigCet10 = melt(alleeBigCet10, id.vars = "Percentile")[,c(1,3)]
alleeBigCet5 = melt(alleeBigCet5, id.vars = "Percentile")[,c(1,3)]
alleeBigCet2.5 = melt(alleeBigCet2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeBigCet = rbind.data.frame(alleeBigCet50,
                                  alleeBigCet45,
                                  alleeBigCet40,
                                  alleeBigCet35,
                                  alleeBigCet30,
                                  alleeBigCet25,
                                  alleeBigCet20,
                                  alleeBigCet15,
                                  alleeBigCet10,
                                  alleeBigCet5,
                                  alleeBigCet2.5)
allAlleeBigCet = cbind.data.frame(Type = "Cetacean, CV = 0.8 ", allAlleeBigCet)

##############

DetPBR = biggestTakeAllee(.1, 10000, 1000)$h
EQ = findEquilibrium(.1, 10000, 1000, DetPBR)

startingPop = EQ[2] + .05*10000

#startingPop = EQ[1]

alleeSmallBat50 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat45 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.126, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat40 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.253, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat35 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.385, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat30 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.524, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat25 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.675, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat20 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 0.842, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat15 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.036, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat10 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.282, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.645, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeSmallBat2.5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .2, z = 1.96, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 

alleeSmallBat50 = cbind.data.frame(Percentile = 50, alleeSmallBat50)
alleeSmallBat45 = cbind.data.frame(Percentile = 45, alleeSmallBat45)
alleeSmallBat40 = cbind.data.frame(Percentile = 40, alleeSmallBat40)
alleeSmallBat35 = cbind.data.frame(Percentile = 35, alleeSmallBat35)
alleeSmallBat30 = cbind.data.frame(Percentile = 30, alleeSmallBat30)
alleeSmallBat25 = cbind.data.frame(Percentile = 25, alleeSmallBat25)
alleeSmallBat20 = cbind.data.frame(Percentile = 20, alleeSmallBat20)
alleeSmallBat15 = cbind.data.frame(Percentile = 15, alleeSmallBat15)
alleeSmallBat10 = cbind.data.frame(Percentile = 10, alleeSmallBat10)
alleeSmallBat5 = cbind.data.frame(Percentile = 5, alleeSmallBat5)
alleeSmallBat2.5 = cbind.data.frame(Percentile = 2.5, alleeSmallBat2.5)

alleeSmallBat50 = melt(alleeSmallBat50, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat45 = melt(alleeSmallBat45, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat40 = melt(alleeSmallBat40, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat35 = melt(alleeSmallBat35, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat30 = melt(alleeSmallBat30, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat25 = melt(alleeSmallBat25, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat20 = melt(alleeSmallBat20, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat15 = melt(alleeSmallBat15, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat10 = melt(alleeSmallBat10, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat5 = melt(alleeSmallBat5, id.vars = "Percentile")[,c(1,3)]
alleeSmallBat2.5 = melt(alleeSmallBat2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeSmallBat = rbind.data.frame(alleeSmallBat50,
                                    alleeSmallBat45,
                                    alleeSmallBat40,
                                    alleeSmallBat35,
                                    alleeSmallBat30,
                                    alleeSmallBat25,
                                    alleeSmallBat20,
                                    alleeSmallBat15,
                                    alleeSmallBat10,
                                    alleeSmallBat5,
                                    alleeSmallBat2.5)

allAlleeSmallBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.2", allAlleeSmallBat)

########

alleeBigBat50 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat45 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.126, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat40 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.253, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat35 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.385, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat30 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.524, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat25 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.675, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat20 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 0.842, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat15 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.036, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat10 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.282, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.645, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 
alleeBigBat2.5 = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 20, cv = .8, z = 1.96, f = 0.5, alleeThreshold = 1000, N = 2000)[101, 2:2001]/10000 


alleeBigBat50 = cbind.data.frame(Percentile = 50, alleeBigBat50)
alleeBigBat45 = cbind.data.frame(Percentile = 45, alleeBigBat45)
alleeBigBat40 = cbind.data.frame(Percentile = 40, alleeBigBat40)
alleeBigBat35 = cbind.data.frame(Percentile = 35, alleeBigBat35)
alleeBigBat30 = cbind.data.frame(Percentile = 30, alleeBigBat30)
alleeBigBat25 = cbind.data.frame(Percentile = 25, alleeBigBat25)
alleeBigBat20 = cbind.data.frame(Percentile = 20, alleeBigBat20)
alleeBigBat15 = cbind.data.frame(Percentile = 15, alleeBigBat15)
alleeBigBat10 = cbind.data.frame(Percentile = 10, alleeBigBat10)
alleeBigBat5 = cbind.data.frame(Percentile = 5, alleeBigBat5)
alleeBigBat2.5 = cbind.data.frame(Percentile = 2.5, alleeBigBat2.5)

alleeBigBat50 = melt(alleeBigBat50, id.vars = "Percentile")[,c(1,3)]
alleeBigBat45 = melt(alleeBigBat45, id.vars = "Percentile")[,c(1,3)]
alleeBigBat40 = melt(alleeBigBat40, id.vars = "Percentile")[,c(1,3)]
alleeBigBat35 = melt(alleeBigBat35, id.vars = "Percentile")[,c(1,3)]
alleeBigBat30 = melt(alleeBigBat30, id.vars = "Percentile")[,c(1,3)]
alleeBigBat25 = melt(alleeBigBat25, id.vars = "Percentile")[,c(1,3)]
alleeBigBat20 = melt(alleeBigBat20, id.vars = "Percentile")[,c(1,3)]
alleeBigBat15 = melt(alleeBigBat15, id.vars = "Percentile")[,c(1,3)]
alleeBigBat10 = melt(alleeBigBat10, id.vars = "Percentile")[,c(1,3)]
alleeBigBat5 = melt(alleeBigBat5, id.vars = "Percentile")[,c(1,3)]
alleeBigBat2.5 = melt(alleeBigBat2.5, id.vars = "Percentile")[,c(1,3)]

allAlleeBigBat = rbind.data.frame(alleeBigBat50,
                                  alleeBigBat45,
                                  alleeBigBat40,
                                  alleeBigBat35,
                                  alleeBigBat30,
                                  alleeBigBat25,
                                  alleeBigBat20,
                                  alleeBigBat15,
                                  alleeBigBat10,
                                  alleeBigBat5,
                                  alleeBigBat2.5)
allAlleeBigBat = cbind.data.frame(Type = "Indiana Bat, CV = 0.8 ", allAlleeBigBat)

#Combine all of our data
allAllee = rbind.data.frame(allAlleeSmallCet, allAlleeBigCet, allAlleeSmallBat, allAlleeBigBat)

#Get our error bounds.
allAlleeDats2 = summarySE(allAllee, measurevar = 'value', groupvars = c('Percentile','Type'))

################################################################################################

#label the recovery factor for 1
allAlleeDats = cbind.data.frame(allAlleeDats, 'Recovery Factor' = "1")

#Label the recovery factorfor 0.5
allAlleeDats2 = cbind.data.frame(allAlleeDats2, 'Recovery Factor' = "0.5")

#Combine it
fullData = rbind.data.frame(allAlleeDats, allAlleeDats2)

#SAVE THE DATA
write.csv(fullData, "20YearPerformanceData.csv", row.names = F)




#####################

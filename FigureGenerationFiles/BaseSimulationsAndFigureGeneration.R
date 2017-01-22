#Base Case Simulations Simulations

#Call our helper functions.
source('./FunctionFiles/HelperFunctions.R')

#Set the seed so we can replicate our results.
set.seed(42)

#Determine the equilibrium for our population that has a carrying capacity of 10,000, allee Threshold of 1000, and a growth rate of 0.04. 
maximizedHarvest = biggestTakeAllee(rmax = .04, K = 10000,alleeThreshold =  1000)$h
EQ = findEquilibrium(rmax = .04, K = 10000, alleeThreshold =  1000, h = maximizedHarvest)[2]

#Let our population start at the equilibrium plus 5% of the carrying capacity.
startingPop = EQ + .05*10000

#We will run simulations for varying levels of CV (0.2, 0.8) for a population with a low growth rate (0.04), and a population with a 
#high growth rate (0.10). We also have varying levels of our population estimation. We have the best guess (50th Percentile, z = 0)
#and we have a more averse estimate (20th Percentile, z = 0.842).

#In addition to this we are simulating the population three different ways, with an allee effect and a PBR calculation as a function
#of the Allee threshold, a population with an Allee effect using PBR WITHOUT taking into account the allee threshold, and a normal
#logistic model, to replicate Wade's paper.

#For this reason we have 24 different simulations, and 
#break it down into 2 sections of 12. 12 For the low growth rate population and 12 for the high growth rate population.

#We break it down like so, each set of three is ordered such that we have the allee effect model with allee PBR, the logistic model,
#and the allee model with logistic PBR calculation. The first set of 3 has 50th percentile and low CV. The second set has 20th Percentile 
#and a low CV. The third set has a big CV and a 50th percentile. The fourth set is 20th percentile with a big CV.

alleeSmallBest = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 30)
logisticSmallBest = monteCarloModelLog(initialPop = 3000, r = .04, K = 10000, theta = 1, t = 100, cv = .2, z = 0, f =1 , N = 30)
alleeSmallBestNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 30)


alleeSmall20th = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = .842, f = 1, alleeThreshold = 1000, N = 30)
logisticSmall20th = monteCarloModelLog(initialPop = 3000, r = .04, K = 10000, theta = 1, t = 100, cv = .2, z = .842, f =1 , N = 30)
alleeSmall20thNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = .842, f = 1, alleeThreshold = 1000, N = 30)


alleeBigBest = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 30)
logisticBigBest = monteCarloModelLog(initialPop = 3000, r = .04, K = 10000, theta = 1, t = 100, cv = .8, z = 0, f =1 , N = 30)
alleeBigBestNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 30)


alleeBig20th = monteCarloModelAllee(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = .842, f = 1, alleeThreshold = 1000, N = 30)
logisticBig20th = monteCarloModelLog(initialPop = 3000, r = .04, K = 10000, theta = 1, t = 100, cv = .8, z = .842, f =1 , N = 30)
alleeBig20thNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .04, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = .842, f = 1, alleeThreshold = 1000, N = 30)


#This is a simple way of relabeling all of the column names by simulation number.
alleeSmallBest = alleeSmallBest[1:31]
#Make the data long form
alleeSmallBest = melt(alleeSmallBest, id.vars = "Year")

#Create proportions.
alleeSmallBest[3] = alleeSmallBest[3]/10000 


#Apply the same cleaning method as the three stpes above.
logisticSmallBest = logisticSmallBest[1:31]
logisticSmallBest = melt(logisticSmallBest, id.vars = "Year")

logisticSmallBest[3] = logisticSmallBest[3]/10000



alleeSmallBestNormPBR = alleeSmallBestNormPBR[1:31]
alleeSmallBestNormPBR = melt(alleeSmallBestNormPBR, id.vars = "Year")

alleeSmallBestNormPBR[3] = alleeSmallBestNormPBR[3]/10000 

#This is the structure of the plot we want. 
smallBest = ggplot(data = alleeSmallBest, aes(Year)) +
  theme_bw() + 
  geom_line(aes(y = value, group = variable, color = "Allee Model; Allee PBR")) +
  ylim(0,1) +
  geom_line(data = logisticSmallBest, aes(x = Year, y = value, group = variable, color = "Logistic Model; Logistic PBR")) + 
  geom_line(data = alleeSmallBestNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model; Logisitic PBR")) +
  labs(title = expression(list(N[min] == paste(50, th)~~Perc.~~ CV == 0.2)), y = "", x = "") +
  geom_hline(yintercept = .5) + 
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30)) + 
  theme(legend.text=element_text(size=20)) + theme(legend.title = element_text(size = 20)) + 
  scale_colour_brewer(name = "Model", palette = "Set1") + 
  theme(axis.text = element_text(face = "bold", color = "black", size = 18))  +
  geom_hline(yintercept = .6841641, linetype = "dashed") +
   scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "C", size = 13)

#We will take the legend from this plot so we are choosing to label this one.
smallBest$labels$colour = "Model"

####

alleeSmall20th = alleeSmall20th[1:31]
alleeSmall20th = melt(alleeSmall20th, id.vars = "Year")

alleeSmall20th[3] = alleeSmall20th[3]/10000 

logisticSmall20th = logisticSmall20th[1:31]
logisticSmall20th = melt(logisticSmall20th, id.vars = "Year")

logisticSmall20th[3] = logisticSmall20th[3]/10000


alleeSmall20thNormPBR = alleeSmall20thNormPBR[1:31]
alleeSmall20thNormPBR = melt(alleeSmall20thNormPBR, id.vars = "Year")

alleeSmall20thNormPBR[3] = alleeSmall20thNormPBR[3]/10000 


small20th = ggplot(data = alleeSmall20th, aes(Year))+ 
  theme_bw() + 
  geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  ylim(0,1) + 
  geom_line(data = logisticSmall20th, aes(x = Year, y = value, group = variable, color = "Logistic Model"))  +
  geom_line(data = alleeSmall20thNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR")) +
  labs(title = expression(list(N[min] == paste(20, th)~~Perc.~~CV == 0.2)),x = "", y = "") +
  geom_hline(yintercept = .5) + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30))  + 
  theme(legend.text=element_text(size=15)) + theme(legend.title = element_text(size = 15)) +
  scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18))  +
  geom_hline(yintercept = .6841641, linetype = "dashed")+
    scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "D", size = 13) 

####

alleeBigBest = alleeBigBest[1:31]
alleeBigBest = melt(alleeBigBest, id.vars = "Year")

alleeBigBest[3] = alleeBigBest[3]/10000 

logisticBigBest = logisticBigBest[1:31]
logisticBigBest = melt(logisticBigBest, id.vars = "Year")

logisticBigBest[3] = logisticBigBest[3]/10000

alleeBigBestNormPBR = alleeBigBestNormPBR[1:31]
alleeBigBestNormPBR = melt(alleeBigBestNormPBR, id.vars = "Year")

alleeBigBestNormPBR[3] = alleeBigBestNormPBR[3]/10000 


bigBest = ggplot(data = alleeBigBest, aes(Year))+ 
  theme_bw() + 
  geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  ylim(0,1) +
  geom_line(data = logisticBigBest, aes(x = Year, y = value, group = variable, color = "Logistic Model")) + 
  geom_line(data = alleeBigBestNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR"))  +
  labs(title = expression(list(N[min] == paste(50, th)~~Perc.~~ CV == 0.8)),x = "", y = "") +
  geom_hline(yintercept = .5) +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30))  +
  theme(legend.text=element_text(size=15)) + theme(legend.title = element_text(size = 15)) + 
  scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18))  +
  geom_hline(yintercept = .6841641, linetype = "dashed") +
    scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "A", size = 13) 

####

alleeBig20th = alleeBig20th[1:31]
alleeBig20th = melt(alleeBig20th, id.vars = "Year")

alleeBig20th[3] = alleeBig20th[3]/10000 

logisticBig20th = logisticBig20th[1:31]
logisticBig20th = melt(logisticBig20th, id.vars = "Year")

logisticBig20th[3] = logisticBig20th[3]/10000

alleeBig20thNormPBR = alleeBig20thNormPBR[1:31]
alleeBig20thNormPBR = melt(alleeBig20thNormPBR, id.vars = "Year")

alleeBig20thNormPBR[3] = alleeBig20thNormPBR[3]/10000 

big20th = ggplot(data = alleeBig20th, aes(Year))+
  geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  theme_bw() + 
  ylim(0,1) +
  geom_line(data = logisticBig20th, aes(x = Year, y = value, group = variable, color = "Logistic Model")) +
  geom_line(data = alleeBig20thNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR")) + 
  labs(title = expression(list(N[min] == paste(20, th)~~Perc.~~ CV == 0.8)), x = "", y = "")+ 
  geom_hline(yintercept = .5) +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30))  + 
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text(size = 15)) +
  scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18)) +
  geom_hline(yintercept = .6841641, linetype = "dashed") +
    scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "B", size = 13) 

#Get the legend of the smallBest plot and get rid of the other plots legends. 
legend <- get_legend(smallBest)
smallBest = smallBest + theme(legend.position = "none")
bigBest = bigBest + theme(legend.position = "none")
small20th = small20th + theme(legend.position = "none")
big20th = big20th + theme(legend.position = "none")

#Arrange the four plots into one big plot. Note the lots of whitespace in the year name. This is to make it look centered.
#Maybe there is an argument for this? I'm unsure.

#First Simulation Figure
grid.arrange(smallBest, bigBest, big20th, small20th, legend, ncol=3, layout_matrix = cbind(c(2,1), c(3,4), 5),widths=c(5,5, 3),
             bottom = textGrob("Year                          ", gp=gpar(fontsize=35,font=1)),
             left = textGrob("Fraction of K", gp=gpar(fontsize=35,font=1), rot = 90))



##High growth population Simulation.

#We follow the same steps as above.

DetPBR = biggestTakeAllee(.1, 10000, 1000)$h
EQ = findEquilibrium(.1, 10000, 1000, DetPBR)

startingPop = EQ[2] +0.05*10000

alleeSmallBest = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 30)
logisticSmallBest = monteCarloModelLog(initialPop = 3000, r = .10, K = 10000, theta = 1, t = 100, cv = .2, z = 0, f =1 , N = 30)
alleeSmallBestNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0, f = 1, alleeThreshold = 1000, N = 30)


alleeSmall20th = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0.842, f = 1, alleeThreshold = 1000, N = 30)
logisticSmall20th = monteCarloModelLog(initialPop = 3000, r = .10, K = 10000, theta = 1, t = 100, cv = .2, z = 0.842, f =1 , N = 30)
alleeSmall20thNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .2, z = 0.842, f = 1, alleeThreshold = 1000, N = 30)


alleeBigBest = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 30)
logisticBigBest = monteCarloModelLog(initialPop = 3000, r = .10, K = 10000, theta = 1, t = 100, cv = .8, z = 0, f =1 , N = 30)
alleeBigBestNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0, f = 1, alleeThreshold = 1000, N = 30)


alleeBig20th = monteCarloModelAllee(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0.842, f = 1, alleeThreshold = 1000, N = 30)
logisticBig20th = monteCarloModelLog(initialPop = 3000, r = .10, K = 10000, theta = 1, t = 100, cv = .8, z = 0.842, f =1 , N = 30)
alleeBig20thNormPBR = monteCarloModelAlleeNormPBR(initialPop = startingPop, r = .10, K = 10000, theta1 = 1, theta2 = 1, t = 100, cv = .8, z = 0.842, f = 1, alleeThreshold = 1000, N = 30)

alleeSmallBest = alleeSmallBest[1:31]
alleeSmallBest = melt(alleeSmallBest, id.vars = "Year")

alleeSmallBest[3] = alleeSmallBest[3]/10000 

logisticSmallBest = logisticSmallBest[1:31]
logisticSmallBest = melt(logisticSmallBest, id.vars = "Year")

logisticSmallBest[3] = logisticSmallBest[3]/10000



alleeSmallBestNormPBR = alleeSmallBestNormPBR[1:31]
alleeSmallBestNormPBR = melt(alleeSmallBestNormPBR, id.vars = "Year")

alleeSmallBestNormPBR[3] = alleeSmallBestNormPBR[3]/10000 


smallBest = ggplot(data = alleeSmallBest, aes(Year)) +
  theme_bw() + 
  geom_line(aes(y = value, group = variable, color = "Allee Model; Allee PBR")) +
  ylim(0,1) +
  geom_line(data = logisticSmallBest, aes(x = Year, y = value, group = variable, color = "Logistic Model; Logistic PBR")) +
  geom_line(data = alleeSmallBestNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model; Logisitic PBR")) +
  labs(title = expression(list(N[min] == paste(50, th)~~Perc.~~ CV == 0.2)), y = "", x = "") + geom_hline(yintercept = .5) + 
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30)) + theme(legend.text=element_text(size=20)) +
  theme(legend.title = element_text(size = 20)) + scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18)) + 
  geom_hline(yintercept = .6841641, linetype = "dashed") +
    scale_color_grey(start = .01, end = .85)  + 
  annotate("text", x = 10, y = .85, label = "C", size = 13) 

smallBest$labels$colour = "Model"

####

alleeSmall20th = alleeSmall20th[1:31]
alleeSmall20th = melt(alleeSmall20th, id.vars = "Year")

alleeSmall20th[3] = alleeSmall20th[3]/10000 

logisticSmall20th = logisticSmall20th[1:31]
logisticSmall20th = melt(logisticSmall20th, id.vars = "Year")

logisticSmall20th[3] = logisticSmall20th[3]/10000


alleeSmall20thNormPBR = alleeSmall20thNormPBR[1:31]
alleeSmall20thNormPBR = melt(alleeSmall20thNormPBR, id.vars = "Year")

alleeSmall20thNormPBR[3] = alleeSmall20thNormPBR[3]/10000 


small20th = ggplot(data = alleeSmall20th, aes(Year))+
  geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  theme_bw() + 
  ylim(0,1) + 
  geom_line(data = logisticSmall20th, aes(x = Year, y = value, group = variable, color = "Logistic Model"))  +
  geom_line(data = alleeSmall20thNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR")) +
  labs(title = expression(list(N[min] == paste(20, th)~~Perc.~~ CV == 0.2)),x = "", y = "") + 
  geom_hline(yintercept = .5)+ theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30)) + 
  theme(legend.text=element_text(size=15)) + theme(legend.title = element_text(size = 15)) +
  scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18)) +
  geom_hline(yintercept = .6841641, linetype = "dashed") +  scale_color_grey(start = .01, end = .85)  + 
  annotate("text", x = 10, y = .85, label = "D", size = 13) 

####

alleeBigBest = alleeBigBest[1:31]
alleeBigBest = melt(alleeBigBest, id.vars = "Year")

alleeBigBest[3] = alleeBigBest[3]/10000 

logisticBigBest = logisticBigBest[1:31]
logisticBigBest = melt(logisticBigBest, id.vars = "Year")

logisticBigBest[3] = logisticBigBest[3]/10000

alleeBigBestNormPBR = alleeBigBestNormPBR[1:31]
alleeBigBestNormPBR = melt(alleeBigBestNormPBR, id.vars = "Year")

alleeBigBestNormPBR[3] = alleeBigBestNormPBR[3]/10000 


bigBest = ggplot(data = alleeBigBest, aes(Year))+ geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  theme_bw() + 
  ylim(0,1) +
  geom_line(data = logisticBigBest, aes(x = Year, y = value, group = variable, color = "Logistic Model")) +
  geom_line(data = alleeBigBestNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR"))  +
  labs(title = expression(list(N[min] == paste(50, th)~~Perc.~~ CV == 0.8)), y = "", x = "", y = "") + 
  geom_hline(yintercept = .5) + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30)) +
  theme(legend.text=element_text(size=15)) + theme(legend.title = element_text(size = 15)) +
  scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18)) +
  geom_hline(yintercept = .6841641, linetype = "dashed") +   scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "A", size = 13) 

####

alleeBig20th = alleeBig20th[1:31]
alleeBig20th = melt(alleeBig20th, id.vars = "Year")

alleeBig20th[3] = alleeBig20th[3]/10000 

logisticBig20th = logisticBig20th[1:31]
logisticBig20th = melt(logisticBig20th, id.vars = "Year")

logisticBig20th[3] = logisticBig20th[3]/10000

alleeBig20thNormPBR = alleeBig20thNormPBR[1:31]
alleeBig20thNormPBR = melt(alleeBig20thNormPBR, id.vars = "Year")

alleeBig20thNormPBR[3] = alleeBig20thNormPBR[3]/10000 

big20th = ggplot(data = alleeBig20th, aes(Year))+ geom_line(aes(y = value, group = variable, color = "Allee Effect")) +
  theme_bw() + 
  ylim(0,1) + geom_line(data = logisticBig20th, aes(x = Year, y = value, group = variable, color = "Logistic Model")) +
  geom_line(data = alleeBig20thNormPBR, aes(x = Year, y = value, group = variable, color = "Allee Model With Original PBR")) + 
  labs(title = expression(list(N[min] == paste(20, th)~~Perc.~~ CV == 0.8)),x = "", y = "")+ geom_hline(yintercept = .5) + 
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=30)) + theme(legend.text=element_text(size=15)) + 
  theme(legend.title = element_text(size = 15)) + scale_colour_brewer(name = "Model", palette = "Set1") +
  theme(axis.text = element_text(face = "bold", color = "black", size = 18)) +
  geom_hline(yintercept = .6841641, linetype = "dashed")+
  scale_color_grey(start = .01, end = .85) + 
  annotate("text", x = 10, y = .85, label = "B", size = 13) 



legend <- get_legend(smallBest)
smallBest = smallBest + theme(legend.position = "none")
bigBest = bigBest + theme(legend.position = "none")
small20th = small20th + theme(legend.position = "none")
big20th = big20th + theme(legend.position = "none")



grid.arrange(smallBest, bigBest, big20th, small20th, legend, ncol=3, 
             layout_matrix = cbind(c(2,1), c(3,4), 5),widths=c(5,5,3), 
             bottom = textGrob("Year                          ", gp=gpar(fontsize=35,font=1)), 
             left = textGrob("Fraction of K", gp=gpar(fontsize=35,font=1), rot = 90))






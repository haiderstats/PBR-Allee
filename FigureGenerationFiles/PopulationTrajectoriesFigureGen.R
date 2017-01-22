#This file generates the data for Figure 1 and generates the figure.

library(ggplot2)
library(reshape2)

#This simulates a discrete form logistic growth model.
logisticMod = function(initialPop, rmax, K, t){
  population = c()
  population[1] = initialPop
  

  for(i in 2:t){
    n = population[i-1]
    population[i]  = max(n + rmax*n  *(1- (n/K)), 0)
  }
  return(population)
}

#This simulates a discrete form growth model that incorporates the Allee effect.
alleeMod = function(initialPop, rmax, K, nMin, t){
  population = c()
  population[1] = initialPop
  
  scalar = (nMin*rmax)/(K-nMin)
  
  for(i in 2:t){
    n = population[i-1]
    population[i]  = max(n + scalar*n * ((n/nMin)-1) *(1- (n/K)), 0)
  }
  return(population)
}

#Generates the logistic growth model.
logModel = cbind.data.frame(Year = 1:350, logModel = logisticMod(1,.1,100,350))

#Generates the Allee model that dies off (it started below the allee threshold).
deadAllee = cbind.data.frame(Year = 1:350, deadAllee = alleeMod(9,.1,100,10,350))

#Generates the Allee model that survives, (it started above the allee threshold).
happyAllee = cbind.data.frame(Year = 1:350, happyAllee = alleeMod(11, .1,100,10,350))

allModels = cbind.data.frame(Year = 1:350, logModel = logModel, deadAllee = deadAllee, happyAllee = happyAllee)
allModelsMelted = melt(allModels, id = "Year")
names(allModelsMelted) = c("Year", "Model","Value")

#Plot it.
ggplot(data = logModel, aes(Year)) +
  theme_bw() + 
  scale_color_grey() +
  geom_line(aes(x = Year, y =logModel, color = "Logistic Model"),size = 3) +
  geom_line(data = happyAllee, aes(x = Year, y =happyAllee, color = "Allee Above Threshold"), size = 3) +
  geom_line(data = deadAllee, aes(x = Year, y =deadAllee, color = "Allee Below Threshold", fill = "model"), size =3 ) +
  geom_hline(yintercept = 10, linetype = "dashed", size = 1) +
  labs(x = "Year", y = "Population") +
  theme(text = element_text(size = 25)) + 
  theme(axis.title = element_text(size=30)) +
  labs(colour = "Model") 



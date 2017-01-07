library(ggplot2)
library(reshape2)


logisticMod = function(initialPop, rmax, K, t){
  population = c()
  population[1] = initialPop
  

  for(i in 2:t){
    n = population[i-1]
    population[i]  = max(n + rmax*n  *(1- (n/K)), 0)
  }
  return(population)
}

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

logModel = cbind.data.frame(Year = 1:350, logModel = logisticMod(1,.1,100,350))
deadAllee = cbind.data.frame(Year = 1:350, deadAllee = alleeMod(9,.1,100,10,350))
happyAllee = cbind.data.frame(Year = 1:350, happyAllee = alleeMod(11, .1,100,10,350))

allModels = cbind.data.frame(Year = 1:350, logModel = logModel, deadAllee = deadAllee, happyAllee = happyAllee)
allModelsMelted = melt(allModels, id = "Year")
names(allModelsMelted) = c("Year", "Model","Value")

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



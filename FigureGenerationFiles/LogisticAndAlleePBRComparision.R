library(ggplot2)
library(reshape)

pbrWithAllee = function(rmax, K, alleeThreshold){
  pbrVec = c()
  hVec = c()
  
  for(i in 0:10000){
    h = i/10000
    pbrVal = h*((K+alleeThreshold)/2 + (sqrt((K-alleeThreshold)^2-(4*alleeThreshold*K*h)/((alleeThreshold * rmax)/(K - alleeThreshold))))/2)
    if(pbrVal < 0 || is.nan(pbrVal)){
      break
    }
    pbrVec[i] = pbrVal
    hVec[i] = h
  }
  return(cbind.data.frame(h = hVec, PBR =pbrVec))
}

#This calculates PBR and harvesting rate in the same fashion as the Allee is calculated.
pbrLogistic = function(rmax, K){
  pbrVec = c()
  hVec = c()
  
  for(i in 0:10000){
    h = i/10000
    pbrVal = (h - (h^2/rmax))*K
    if(pbrVal < 0){
      break
    }
    pbrVec[i] = pbrVal
    hVec[i] = h
  }
  return(cbind.data.frame(h = hVec, PBR =pbrVec))
}


PBRForLogistic = pbrLogistic(.1, 10000)
PBRForLogistic = cbind.data.frame(PBRForLogistic, Model = rep("Logistic Model", nrow(PBRForLogistic)))

PBRForAllee = pbrWithAllee(.1, 10000, 1000)
PBRForAllee = cbind.data.frame(PBRForAllee, Model = rep("Allee Model", nrow(PBRForAllee)))


BothPBR = melt(list(Logistic = PBRForLogistic, Allee = PBRForAllee), id.vars = c("Model", "h"))[c(-5, -3)]


ggplot(data = BothPBR, aes(h, value, linetype = Model)) +
  theme_bw() + 
  geom_line(size = 1.2)  +
  labs(x = "Proportion Harvested (h)", y = "Quantity Harvested (PBR)") + 
  theme(axis.text = element_text(size = 18)) +
  theme(text = element_text(size = 25)) + 
  theme(axis.title = element_text(size=30)) 





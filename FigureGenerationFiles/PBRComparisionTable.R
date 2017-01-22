#This file is used to generate the values needed to compare the PBR's of the 
#Allee model and logistic model. These functions corresponds to section 2.4 of the paper.

#This is the same function as that in 'HelperFunction.R'. It generates our Allee PBR and the harvesting rate.
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


#This gives us our maximizing value of h and the max value of PBR for the allee model
biggestTakeAllee = function(rmax, K, alleeThreshold){
  alleePBR = suppressWarnings(pbrWithAllee(rmax,K,alleeThreshold))
  
  maximum = alleePBR[which.max(alleePBR$PBR),]
  
  return(maximum)
}

#Same thing as above just for the logistic model.
biggestTakeLogistic = function(rmax, K){
  logisticPBR = pbrLogistic(rmax, K)
  
  maximum = logisticPBR[which.max(logisticPBR$PBR),]
  
  return(maximum)
}


#This function will take the the allee model and logisitic model and compare them for given r, K, alee threshold. 
compareAlleeLogistic = function(rmax, K, alleeThreshold){
  allee = biggestTakeAllee(rmax, K, alleeThreshold)
  logistic = biggestTakeLogistic(rmax, K)
  
  #Quick function to calculate percent change.
  percentChange = function(original,change){
    theChange = (change$PBR  - original$PBR)/original$PBR
    return(theChange)
  }
  
  perChange = percentChange(logistic, allee)
  
  difference = allee$PBR - logistic$PBR
  
  ratio = logistic$PBR/allee$PBR
  return(data.frame(PercentChange = perChange, Logisitc = logistic$PBR, Allee = allee$PBR, Difference = difference, Ratio = ratio, K = K, alleeThreshold = alleeThreshold, rMax = rmax))
}

#This iterates over r max from .01 to .30 to generate compairison values.
checkRmax = function(K, alleeThreshold){
  comparision = data.frame()
  for(i in 1:30){
    comparision = rbind.data.frame(comparision, compareAlleeLogistic(.01*i, K, alleeThreshold))
  }
  return(comparision)
}

#initialize comparision
PBRComp = checkRmax(100, 1)

#Do comparision
for(i in 2:25){
  toAdd = checkRmax(100, i)
  PBRComp = rbind.data.frame(PBRComp, toAdd)
}

#Calcualte the mean of the ratios.
mean(PBRComp$Ratio)
#1.823589 This means we overestimate PBR by 82.4%.




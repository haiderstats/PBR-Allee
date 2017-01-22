
#This is the file for creating the figures in the PBR Paper. The simulations take
#A very long time to run (varies between an hour and a day) so I have saved the simulation data to three different files.
#Here we load in the data file and create our graph.

#Load the library
library(ggplot2)
############################

#100 Year Criterion:

#Load the data.
Year100Dat = read.csv("./Data/100YearPerformanceData.csv")

#Give labels to our data. Lq is our lower quantile and Hq is the higher quantile.
names(Year100Dat) = c("Percentile", "Type", "N", "value", "sd", "Lq","Hq", "Recovery Factor")

#Turn this into a factor variable.
Year100Dat$`Recovery Factor` = factor(Year100Dat$`Recovery Factor`)

#Make the plot... lots of details.
 ggplot(data = Year100Dat, aes(Percentile, value, colour = `Recovery Factor`)) +
  theme_bw() +
  facet_wrap(~Type, ncol = 1) +
  geom_errorbar(aes(ymin = Lq, ymax = Hq))+ 
  stat_summary(fun.y=median, geom="point") + 
  scale_x_continuous(breaks=seq(0, 50, 5))+
  scale_y_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1.00))+ 
  labs( x = expression(N[min]~~Percentile), y = "Fraction of K in Year 100") +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 25)) +
  theme(axis.title.y = element_text(margin = margin(0,30,0,0)), plot.title = element_text(size = 25)) + 
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text(size = 15))+
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) + 
  theme(strip.text.x = element_text(size = 20)) +
  geom_hline(yintercept = .6841641, linetype = "dashed") +
  guides(col = guide_legend(reverse = FALSE)) + 
  scale_color_grey(end = .6) +
  geom_text(data = data.frame(x = 3.5, y = 0.25, label = c("A", "B","C","D"), Type = levels(Year100Dat$Type)), aes(x, y, label = label), inherit.aes = FALSE, parse = FALSE,size = 12)+
  theme(strip.background = element_rect(fill = "white",size = .5))

########################################################################

#20 Year Criterion

#Same steps as above. 
 
Year20Dat = read.csv("./Data/20YearPerformanceData.csv")

names(Year20Dat) = c("Percentile", "Type", "N", "value", "sd", "Lq","Hq", "Recovery Factor")

Year20Dat$`Recovery Factor` = factor(Year20Dat$`Recovery Factor`)

ggplot(data = Year20Dat, aes(Percentile, value, colour = `Recovery Factor`)) +
  theme_bw() + 
  facet_wrap(~Type, ncol = 1) +
  geom_errorbar(aes(ymin = Lq, ymax = Hq))+ 
  stat_summary(fun.y=median, geom="point") + 
  scale_x_continuous(breaks=seq(0, 50, 5))+
  scale_y_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1.00))+ 
  labs(x = expression(N[min]~~Percentile), y = "Fraction of K in Year 20") +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 25)) +
  theme(axis.title.y = element_text(margin = margin(0,30,0,0)), plot.title = element_text(size = 25)) + 
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text(size = 15))+
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) + 
  theme(strip.text.x = element_text(size = 20)) +
  geom_hline(yintercept = .6841641, linetype = "dashed") +
  guides(col = guide_legend(reverse = FALSE))+
  scale_color_grey(end = .6) +
  geom_text(data = data.frame(x = 3.5, y = 0.25, label = c("A", "B","C","D"), Type = levels(Year100Dat$Type)), aes(x, y, label = label), inherit.aes = FALSE, parse = FALSE,size = 12)+
  theme(strip.background = element_rect(fill = "white",size = .5))



#########################################

#Carrying Capacity Simulation

fullData = read.csv("./Data/CarryingCapGoalData.csv")

ggplot(data = fullData, aes(Factor, value)) +
  theme_bw() + 
  facet_wrap(~Type, ncol = 1) +
  geom_errorbar(aes(ymin = Lq, ymax = Hq))+ 
  stat_summary(fun.y=median, geom="point") + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1.00))+ 
  scale_x_continuous(breaks=seq(0, 1, .1))+
  labs(x = "Recovery Factor", y = "Fraction of K After Recovery to Equilibrium") +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 25)) +
  theme(axis.title.y = element_text(margin = margin(0,30,0,0)), plot.title = element_text(size = 25)) + 
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text(size = 15))+
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) + 
  theme(strip.text.x = element_text(size = 20)) +
  geom_hline(yintercept = .9, linetype = "dashed") +
  guides(col = guide_legend(reverse = TRUE)) +
  geom_text(data = data.frame(x = .05, y = 0.25, label = c("A", "B","C","D"), Type = levels(Year100Dat$Type)), aes(x, y, label = label), inherit.aes = FALSE, parse = FALSE,size = 12)+
  theme(strip.background = element_rect(fill = "white",size = .5))


##########################################







require(ggplot2)
library("RColorBrewer")

setwd("V:/carolinet/Rabies")

cost_ratio <- vector()
v_cost_ratio <- vector()
incidence <- vector()

birth_cohort <- 100000
immunity_duration <- 10

for(i in 1:10000){

cost_PrEP_birth <- runif(1,5,20)
cost_PEP_2 <- cost_PrEP_birth

cost_PEP_4 <- cost_PrEP_birth + cost_PEP_2 + runif(1,0,120)

cost_ratio[i] <- cost_PEP_4/(cost_PrEP_birth + cost_PEP_2)
 
incidence[i] <- runif(1,10,500)

v_cost_PrEP_birth <- cost_PrEP_birth*birth_cohort
v_cost_PEP_2 <- incidence[i]*immunity_duration*cost_PEP_2

v_cost_PEP_4 <- incidence[i]*immunity_duration*cost_PEP_4

v_cost_ratio[i] <- (v_cost_PrEP_birth + v_cost_PEP_2)/v_cost_PEP_4

}


alldata <- data.frame(incidence = incidence, cost_ratio = cost_ratio, final_cost_ratio = v_cost_ratio)


myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
my.labels <- c(ceiling(1.5*min(alldata$final_cost_ratio)),100,200, floor(0.9*max(alldata$final_cost_ratio)))
sc <- scale_colour_gradientn(colours = myPalette(200), limits=c(min(v_cost_ratio), max(v_cost_ratio)), breaks=my.labels, labels=my.labels)

ggplot(alldata) + geom_point(aes(x = incidence, y = cost_ratio, color = final_cost_ratio), size=2) + sc + ylab("ratio of cost") + theme_bw()

ggsave("rabies_tradeoff.png")
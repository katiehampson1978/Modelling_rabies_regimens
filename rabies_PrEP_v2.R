require(ggplot2)
library("RColorBrewer")

# setwd("C:/Users/ak889/Dropbox/Rabies")

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
my.labels <- c(ceiling(1.5*min(alldata$final_cost_ratio)),100,200, 400)
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(min(v_cost_ratio), max(v_cost_ratio)), breaks=my.labels, labels=my.labels)

main <- ggplot(alldata) + geom_point(aes(x = incidence, y = cost_ratio, color = final_cost_ratio), size=3) + sc + ylab("ratio of cost") + theme_bw()  
   # geom_point(aes(x = 24, y = 1), size=3, pch=3, col="black")  + annotate("text", x = 24, y = 1.5, label = "Tanzania")

plot2 <- ggplot(alldata) + geom_point(aes(x = incidence, y = cost_ratio, color = final_cost_ratio), size=2) + sc  + theme_bw() +
  geom_point(aes(x = 24, y = 1), size=2, pch=3, col="black") + annotate("text", x = 20, y = 2, label = "Tanzania") + scale_y_continuous(limits = c(1,5)) +
  scale_x_continuous(limits = c(10,30)) + theme(legend.position="none", axis.title.x=element_blank(),  axis.title.y=element_blank())
  

main + annotation_custom(ggplotGrob(plot2), xmin = 12 , xmax = 180, ymin = 8, ymax = 12)


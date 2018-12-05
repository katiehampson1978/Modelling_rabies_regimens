###############################################################################
# Plot of costs in N'Djamena
###############################################################################

rm(list=ls())
costs <- read.csv("data/Costs_Chad.csv")
million = 1000000

postscript(file="figs/Chad_costs.eps", width=7, height=4, horizontal=F)
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.2,0.2,0), tck=-0.01)
plot(1:length(costs$Year), costs$PrEP..PEP/million, ylim=c(0,60), xlim=c(0,20),
     col="red", type="l", ylab="Costs in millions of USD", xlab = "Year of programme", axes=FALSE)
lines(1:length(costs$Year), costs$PEP.only/million, col="blue")
lines(1:length(costs$Year), costs$PEP...Dog.Vax/million, col = "green")
axis(1); axis(2)
legend(0, 55, legend=c("PrEP +PEP", "PEP only", "PEP & dog vax"), col=c("red", "blue", "green"),
       lty = c(1,1), bty="n", cex=0.75)
dev.off()

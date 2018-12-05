# SAGE meeting preparations - 15 Jun 2017
# Examine how costs of vaccination vary with patient throughput
# Calculate vials used, cost, 'wastage' under different throughput scenarios
rm(list=ls())
source("R/PEPfunctions.R")
source("R/PEPcosts.R")

#Vaccination schedule and regimens current in use
schedule = c(0, 3, 7, 14, 21, 28, 90) # dates for delivery

# essen = c(1,1,1,1,0,1,0)      #IM Essen 5 doses - 1 vial/ vaccination
essen4 = c(1,1,1,1,0,0,0)     #IM Essen reduced 4 dose
zagreb = c(2,0,1,0,1,0,0)     #IM Zagreb 3 visit
TRC4 = c(2,2,2,0,0,2,0)       #ID reduced 4 dose TRC
# site8 = c(8,0,4,0,0,1,1)		  #ID 8-site

# new vaccination schedules and regimens to investigate
ID4 = c(4,0,2,0,0,1,0)        #ID 4-site economical - 3 visits (1-month simplified 4-site ID)
ID4v2 = c(4,0,2,0,0,0,0)		  #ID 4-site economical - 2 visits (MW)
weekID=c(4,4,4,0,0,0,0)       #ID 1-week - Narayana et al, Shantavasinkul et al, Sudarshen et al.
IPC = c(2,2,2,0,0,0,0)        #ID 1-week 2-site (IPC)
weekIM1 = c(1,1,1,0,0,0,0)
weekIM2 = c(2,0,1,0,0,0,0) # Huang et al

# All different regimens combined (9)
regimens=list(TRC4=c(2,2,2,0,0,2,0), # Default ID
              essen4=c(1,1,1,1,0,0,0) , zagreb=c(2,0,1,0,1,0,0), # Default IM
              ID4=c(4,0,2,0,0,1,0), ID4v2=c(4,0,2,0,0,0,0), # M Warrell
              weekID=c(4,4,4,0,0,0,0), # Under investigation ID
              IPC=c(2,2,2,0,0,0,0), # Under investigation ID
              weekIM1=c(1,1,1,0,0,0,0), weekIM2=c(2,0,1,0,0,0,0)) # Under investigation IM

v=data.frame(regimen = c("TRC4", "essen4", "zagreb", # Default
              "ID4", "ID4v2", # M Warrell
              "weekID", "IPC", "weekIM1", "weekIM2")) # Under investigation IM
v$vdoses_waste = c(4,1,1,4,4,4,4,1,1)
v$vdoses_nowaste = c(5,1,1,5,5,5,5,1,1)
v$vdoses_waste1 = c(8,1,1,4,4,8,8,1,1)
v$vdoses_nowaste1 = c(10,1,1,5,5,10,10,1,1)

# ANALYSE:
# 100% compliance and 75% compliance
# 0.5 and 1mL vials
# Assume perfect usage versus some wastage

#look at different numbers of vials used for different regimens
patients=c(1,5,10,20,30,40,50,75,100,500) # New patients per month
reps = 1000 # increase to 1000

#___________________________________________________________
# PrEP
prEP_IM = c(1,1,1)
prEP_ID = c(1,1,1)
prPEP_IM = c(1,1)
prPEP_ID1 = c(1,1)
prPEP_ID1 = c(4)
schedule_prEP = c(0,7,21)
schedule_prPEP = c(0,3)

# to investigate
prEP_IM_i1 = c(1,1,1)
prEP_ID_i1 = c(2,2,2)
prEP_IM_i2 = c(1,1)
prEP_ID_i2 = c(2,2)
prEP_IM_i3 = c(1)
prEP_ID_i3 = c(2)
schedule_prEP_i1 = c(0,3,7)
schedule_prPEP_i2 = c(0,7)
schedule_prPEP_i3 = c(0)

#####################################################################################
#Work out healthcare costs per rabies death averted
overhead=1.2
vial.cost=10
prabies=0.19

material=0.033 # IM - standard 1CC syringes
material1=0.066 # ID - standard 1CC syringes - 2 needles per injection
material2=(14.55/100)/2 # ID insulin syringes - 1 needle per patient

#####################################################################################
# CALCULATE COSTS PER RABIES DEATH AVERTED
# extract vaccine simulation outputs
vuse0.5 = vuse1 = vuse0.5waste = vuse1waste = vuse0.5insulin = vuse1insulin = list() #

for (i in 1: length(regimens)){
  # IM
  #1 - 100% compliance - 0.5mL vials - perfect usage
  vials_used = read.csv(paste("output/v", names(regimens)[i], ".csv", sep=""))
  vuse0.5[[i]] = vuse0.5insulin = vials_used

  #2 - 100% compliance - 1mL vials - perfect usage
  vials_used = read.csv(paste("output/v", names(regimens)[i], "_1ml.csv", sep=""))
  vuse1[[i]] = vuse1insulin = vials_used

  #3 - 100% compliance - 0.5mL vials - some waste
  vials_used = read.csv(paste("output/v", names(regimens)[i], "_waste.csv", sep=""))
  vuse0.5waste[[i]] = vials_used

  #4 -100% compliance - 1mL vials - some waste
  vials_used = read.csv(paste("output/v", names(regimens)[i], "_1ml_waste.csv", sep=""))
  vuse1waste[[i]] = vials_used
}

vials = read.csv(paste("output/v", "ID4", "_1ml.csv", sep=""))

#Compare the costs per rabies deaths prevented
costPRDA=costPRDAuci=costPRDAlci=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDA1=costPRDAuci1=costPRDAlci1=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDAinsulin=costPRDAuciinsulin=costPRDAlciinsulin=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDA1insulin=costPRDAuci1insulin=costPRDAlci1insulin=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDAwaste=costPRDAwasteuci=costPRDAwastelci=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDAwaste1=costPRDAwasteuci1=costPRDAwastelci1=matrix(NA, nrow=length(regimens), ncol=length(patients))

DA=prabies*patients*12 # deaths averted
PT=patients*12 # deaths averted

# Calculate costs per patient treated
for (i in 1:length(regimens)){

  # 0.5ml vials
  costPRDA[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5[i])[,3], unlist(regimens[i]))/PT
  costPRDAuci[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5[i])[,1], unlist(regimens[i]))/PT
  costPRDAlci[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5[i])[,5], unlist(regimens[i]))/PT

  # 1ml vials
  costPRDA1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1[i])[,3], unlist(regimens[i]))/PT
  costPRDAuci1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1[i])[,1], unlist(regimens[i]))/PT
  costPRDAlci1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1[i])[,5], unlist(regimens[i]))/PT

  # INSULIN SYRINGES
  # 0.5ml vials
  costPRDAinsulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse0.5[i])[,3], unlist(regimens[i]))/PT
  costPRDAuciinsulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse0.5[i])[,1], unlist(regimens[i]))/PT
  costPRDAlciinsulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse0.5[i])[,5], unlist(regimens[i]))/PT

  # 1ml vials
  costPRDA1insulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse1[i])[,3], unlist(regimens[i]))/PT
  costPRDAuci1insulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse1[i])[,1], unlist(regimens[i]))/PT
  costPRDAlci1insulin[i,] = costs(overhead, material2, vial.cost, patients, as.data.frame(vuse1[i])[,5], unlist(regimens[i]))/PT

  # 0.5ml vials - some waste
  costPRDAwaste[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5waste[i])[,3], unlist(regimens[i]))/PT
  costPRDAwasteuci[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5waste[i])[,1], unlist(regimens[i]))/PT
  costPRDAwastelci[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse0.5waste[i])[,5], unlist(regimens[i]))/PT

  # 1ml vials - some waste  - some waste - useful for MW regimens
  costPRDAwaste1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1waste[i])[,3], unlist(regimens[i]))/PT
  costPRDAwasteuci1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1waste[i])[,1], unlist(regimens[i]))/PT
  costPRDAwastelci1[i,] = costs(overhead, material1, vial.cost, patients, as.data.frame(vuse1waste[i])[,5], unlist(regimens[i]))/PT
}


costPRDA_4site0.1ml=costPRDAuci_4site0.1ml=costPRDAlci_4site0.1ml=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDA_4site0.1ml = costs(overhead, material1, vial.cost, patients, as.data.frame(vials)[,3], unlist(regimens[4]))/PT
costPRDAuci_4site0.1ml = costs(overhead, material1, vial.cost, patients, as.data.frame(vials)[,1], unlist(regimens[4]))/PT
costPRDAlci_4site0.1ml = costs(overhead, material1, vial.cost, patients, as.data.frame(vials)[,5], unlist(regimens[4]))/PT



# Compare for IPC vs ID4
# vuse1[[7]]
costs(overhead, material2, vial.cost, patients, vuse1[[7]][,3], unlist(regimens[7]))/PT

# vials
costs(overhead, material1, vial.cost, patients, vials[,3], unlist(regimens[4]))/PT

#___________________________________________________________________________

#Plot the cost to healthcare providers per rabies death averted and compare vial use
# Essen4 (dark grey); zagreb (light grey)
# WeekIM (dark blue); weekIM2 (light blue)
# TRC4 (dark red), weekID (red), IPC (light red) - normal syringes (waste) & insulin syringes (no waste)
# ID4 (orange), ID4v2 (light orange) - normal syringes (waste)

#ICER=cost/(effectiveness(No PEP)-effectiveness(PEP))
#ICER=cost(deathsNoPEP-deathsPEP)



# COSTS OF ID CURRENT AND NEW (compared to IM)
postscript(file="figs/costs_ID_per_patient_Vaccine.eps", width=9, height=4, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.1, 0.37, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="",
     ylab="Direct medical costs per patient treated (USD)")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "0.5mL vials", font=2)

# IM - # ZAGREB or ESSEN
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)
text(150, 225*prabies, "IM")

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[1,], costPRDAwastelci[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste[1,], col="dark red", lwd=2)
text(140, 140*prabies, "Updated TRC", col="dark red")

# 1-week 4-site ID (red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[6,], costPRDAwastelci[6,length(patients):1]),
        border=NA, col="indianred1")
lines(patients, costPRDAwaste[6,], col="dark red", lwd=2)
text(140, 34, "1-week 4-site", col="dark red")

# 1-month simplified 4-site (orange), - normal syringes (waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[4,], costPRDAwastelci[4,length(patients):1]),
        border=NA, col="tan1")
lines(patients, costPRDAwaste[4,], col="darkorange2", lwd=2)
text(130, 113*prabies, "    1-month \n simplified 4-site", col="darkorange2")

# # ID4v2 (light orange) - normal syringes (waste)
# polygon(c(patients, sort(patients, decreasing=T)),
#         c(costPRDAwasteuci[5,], costPRDAwastelci[5,length(patients):1]),
#         border=NA, col="peachpuff")
# lines(patients, costPRDAwaste[5,], col="orange", lwd=2)
# text(150, 100, "4-site ID 2-dose", col="orange")

# IPC (light red) - (no waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[7,], costPRDAwastelci[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDAwaste[7,], col="indianred1", lwd=2)
text(150, 17, "1-week 2-site", col="indianred1")
mtext("A", line =-3, side=3, adj=0, cex=1.5)

############## 1 mL vials
par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.4, 0.67, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="Monthly clinic throughput (new bite patients)",
     ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "1mL vials", font=2)

# IM - ZAGREB or ESSEN
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[1,], costPRDAwastelci1[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste1[1,], col="dark red", lwd=2)

# weekID (red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[6,], costPRDAwastelci1[6,length(patients):1]),
        border=NA, col="indianred1")
lines(patients, costPRDAwaste1[6,], col="dark red", lwd=2)

# ID4 (orange), - normal syringes (waste) - DON'T SHOW VERSION WITH 0.2mL injections!
# polygon(c(patients, sort(patients, decreasing=T)),
#         c(costPRDAwasteuci1[4,], costPRDAwastelci1[4,length(patients):1]),
#         border=NA, col="tan1")
# lines(patients, costPRDAwaste1[4,], col="darkorange2", lwd=2)

# # ID4v2 (light orange) - normal syringes (waste)
# polygon(c(patients, sort(patients, decreasing=T)),
#         c(costPRDAwasteuci1[5,], costPRDAwastelci1[5,length(patients):1]),
#         border=NA, col="peachpuff")
# lines(patients, costPRDAwaste1[5,], col="orange", lwd=2)

# SAME - BUT SHOW WITH 0.1ml INJ!!! (MW)
# ID4 (orange), - normal syringes (waste) - 1-month simplified 4-site ID
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAuci_4site0.1ml, costPRDAlci_4site0.1ml[length(patients):1]),
        border=NA, col="tan1")
lines(patients, costPRDA_4site0.1ml, col="darkorange2", lwd=2, lty=1)

# IPC (light red) - (waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[7,], costPRDAwastelci1[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDAwaste1[7,], col="indianred1", lwd=2)
mtext("B", line =-3, side=3, adj=0, cex=1.5)

########################################################################
# # INSULIN SYRINGES - 1mL syringes - show overlapping lines
par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.7, 0.97, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "1mL vials", font=2)
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)

polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[7,], costPRDAwastelci1[7,length(patients):1]),
        border=NA, col="pink")

polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAuci1insulin[7,], costPRDAlci1insulin[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDA1insulin[7,], col="indianred1", lwd=2, lty=2)
lines(patients, costPRDAwaste1[7,], col="indianred1", lwd=2)
text(150, 14, "Standard syringe", col="indianred1")
text(150, 8, "Insulin syringe", col="indianred1")
mtext("C", line =-3, side=3, adj=0, cex=1.5)

dev.off()


# Look at vial use for different regimens
# Assume limited vaccine access in clinc - with 1ml vials because most effective:
# 250 vials - Essen 5 patients - vuse1[[2]][2,3]
# 1000 vials - Essen 20 patients - vuse1[[2]][4,3]
# 3500 vials - Essen 100 patients - vuse1[[2]][8,3]

# Patients
vuse250 = vuse1000 = vuse3500 = rep(NA, length(regimens))
patients # is a list of 10 throughoputs per MONTH
med_value = 3 # Median estimate of vials used


for(i in 1:length(regimens)){ # for each regimen
  vuse250[i] = vuse1[[i]][2, med_value] # select the median vials used for 5 patients/ month (patients[2]) i.e. 60 patients
  vuse1000[i] = vuse1[[i]][4, med_value] # for 20 patients/ month (patients[4]) i.e. 240 patients
  vuse3500[i] = vuse1[[i]][8, med_value] # for 75 patients/ month (patients[8]) i.e. 900 patients
}

#
patients250 = vuse250[2]*(5*12)/vuse250 # vials used for / vials used for each regimen
patients1000 = vuse1000[2]*(20*12)/vuse1000
patients3500 = vuse3500[2]*(75*12)/vuse3500

names(regimens)[c(2,1,6,4,7)]
bars = c("IM ", "Updated TRC ", "1-week 4-site ID", "1 month simplified 4-site ID", "1-week 2-site ID ")
cols = c("black", "red", "indianred1", "tan1", "pink")

# regimens - essen4, weekIM1, UTRC, ID4, IPC c(2,8,1,4,7)
postscript(file="figs/patients_treated_Vaccine.eps", width=9, height=4, horizontal=F)
par(cex=0.75, lwd=0.5, plt=c(0.05, 0.35, 0.35, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients250[c(2,1,6,4,7)],
           axisnames = FALSE, main="250 vials available",
           col=cols, ylim=c(0,1000), ylab="Patients treated")
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("D", line = 1, side=3, adj=-0.1, cex=1.5)

par(cex=0.75, new=TRUE, lwd=0.5, plt=c(0.375, 0.675, 0.35, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients1000[c(2,1,6,4,7)],
           axisnames = FALSE, main="1000 vials available", col=cols, ylim=c(0,1000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("E", line = 1, side=3, adj=-0.1, cex=1.5)

par(cex=0.75, new=TRUE, lwd=0.5, plt=c(0.7, 1, 0.35, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients3500[c(2,1,6,4,7)],
           axisnames = FALSE, main="3500 vials available", col=cols, ylim=c(0,7000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("F", line = 1, side=3, adj=-0.1, cex=1.5)
dev.off()



postscript(file="figs/Fig1_combined.eps", width=9, height=8, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.1, 0.37, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="",
     ylab="Direct medical costs per patient treated (USD)")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "0.5mL vials", font=2)

# IM - # ZAGREB or ESSEN
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)
text(150, 225*prabies, "IM")

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[1,], costPRDAwastelci[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste[1,], col="dark red", lwd=2)
text(140, 140*prabies, "Updated TRC", col="dark red")

# 1-week 4-site ID (red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[6,], costPRDAwastelci[6,length(patients):1]),
        border=NA, col="indianred1")
lines(patients, costPRDAwaste[6,], col="dark red", lwd=2)
text(140, 34, "1-week 4-site", col="dark red")

# 1-month simplified 4-site (orange), - normal syringes (waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[4,], costPRDAwastelci[4,length(patients):1]),
        border=NA, col="tan1")
lines(patients, costPRDAwaste[4,], col="darkorange2", lwd=2)
text(130, 113*prabies, "    1-month \n simplified 4-site", col="darkorange2")

# IPC (light red) - (no waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[7,], costPRDAwastelci[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDAwaste[7,], col="indianred1", lwd=2)
text(150, 17, "1-week 2-site", col="indianred1")
mtext("A", line =-3, side=3, adj=-0.1, cex=1.5)

############## 1 mL vials
par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.4, 0.67, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="Monthly clinic throughput (new bite patients)",
     ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "1mL vials", font=2)

# IM - ZAGREB or ESSEN
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[1,], costPRDAwastelci1[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste1[1,], col="dark red", lwd=2)

# weekID (red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[6,], costPRDAwastelci1[6,length(patients):1]),
        border=NA, col="indianred1")
lines(patients, costPRDAwaste1[6,], col="dark red", lwd=2)

# SAME - BUT SHOW WITH 0.1ml INJ!!! (MW)
# ID4 (orange), - normal syringes (waste) - 1-month simplified 4-site ID
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAuci_4site0.1ml, costPRDAlci_4site0.1ml[length(patients):1]),
        border=NA, col="tan1")
lines(patients, costPRDA_4site0.1ml, col="darkorange2", lwd=2, lty=1)

# IPC (light red) - (waste)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[7,], costPRDAwastelci1[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDAwaste1[7,], col="indianred1", lwd=2)
mtext("B", line =-3, side=3, adj=-0.1, cex=1.5)

########################################################################
# # INSULIN SYRINGES - 1mL syringes - show overlapping lines
par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.7, 0.97, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250*prabies, "1mL vials", font=2)
lines(patients, costPRDA[3,], col="black", lwd=2, lty=1)

polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[7,], costPRDAwastelci1[7,length(patients):1]),
        border=NA, col="pink")

polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAuci1insulin[7,], costPRDAlci1insulin[7,length(patients):1]),
        border=NA, col="pink")
lines(patients, costPRDA1insulin[7,], col="indianred1", lwd=2, lty=2)
lines(patients, costPRDAwaste1[7,], col="indianred1", lwd=2)
text(150, 14, "Standard syringe", col="indianred1")
text(150, 8, "Insulin syringe", col="indianred1")
mtext("C", line =-3, side=3, adj=-0.1, cex=1.5)

# Look at vial use for different regimens
# Patients
vuse250 = vuse1000 = vuse3500 = rep(NA, length(regimens))
patients # is a list of 10 throughoputs per MONTH
med_value = 3 # Median estimate of vials used


for(i in 1:length(regimens)){ # for each regimen
  vuse250[i] = vuse1[[i]][2, med_value] # select the median vials used for 5 patients/ month (patients[2]) i.e. 60 patients
  vuse1000[i] = vuse1[[i]][4, med_value] # for 20 patients/ month (patients[4]) i.e. 240 patients
  vuse3500[i] = vuse1[[i]][8, med_value] # for 75 patients/ month (patients[8]) i.e. 900 patients
}

#
patients250 = vuse250[2]*(5*12)/vuse250 # vials used for / vials used for each regimen
patients1000 = vuse1000[2]*(20*12)/vuse1000
patients3500 = vuse3500[2]*(75*12)/vuse3500

names(regimens)[c(2,1,6,4,7)]
bars = c("IM ", "Updated TRC ", "1-week 4-site ID", "1 month simplified 4-site ID", "1-week 2-site ID ")
cols = c("black", "red", "indianred1", "tan1", "pink")

# regimens - essen4, weekIM1, UTRC, ID4, IPC c(2,8,1,4,7)
par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.1, 0.37, 0.1, 0.4), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients250[c(2,1,6,4,7)],
           axisnames = FALSE, main="250 vials available",
           col=cols, ylim=c(0,1000), ylab="Patients treated")
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("D", line = 1, side=3, adj=-0.1, cex=1.5)

par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.4, 0.67, 0.1, 0.4), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients1000[c(2,1,6,4,7)],
           axisnames = FALSE, main="1000 vials available", col=cols, ylim=c(0,1000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("E", line = 1, side=3, adj=-0.1, cex=1.5)

par(cex=0.7, new=TRUE, lwd=0.5, plt=c(0.7, 0.97, 0.1, 0.4), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(patients3500[c(2,1,6,4,7)],
           axisnames = FALSE, main="3500 vials available", col=cols, ylim=c(0,7000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1)
mtext("F", line = 1, side=3, adj=-0.1, cex=1.5)
dev.off()


patients_treated = data.frame(regimen = bars, patients = patients1000[c(2,8,1,4,7)])
write.csv(patients_treated, "output/patients_treated.csv", row.names=FALSE)

vuse3500[2]/vuse3500
vuse250[2]/vuse250

# SEQUENCE OF PATIENT DATES
set.seed(3) # make repeatable!
startdates=floor(sort(runif(5*12, 1, 365))) # Days patients initiate PEP
vaccdates=unlist(lapply(startdates, VusePC, pc=1, regimen=IPC)) # clinic visits - with full compliance!
vs = vial.count(vaccdates, doses.per.vial=10); vs # take vector of attendance dates and calculate vials used:

postscript(file="figs/vial_sharing_low_thruput.eps", width=6, height=3, horizontal=F)
par(cex=0.75, lwd=0.5, plt=c(0.15, 0.9, 0.15, 0.9), mgp=c(1.5,0.5,0), tck=-0.01)
vsharing = hist(vaccdates, breaks=0:385, plot=FALSE)$counts/2
vstart = hist(startdates, breaks=0:385, plot=FALSE)$counts
ticks = barplot(vsharing, border=NA, col="pink", xlab = "months", ylab = "daily patients")
barplot(vstart, border=NA, col="indianred1", add=TRUE)
axis(1, at = ticks[floor(seq(1,365,30.5))], labels=1:12, tck=-0.015)
axis(1, at = ticks[floor(seq(1,365,30.5*3))], labels=FALSE, tck=-0.03)
dev.off()

vs; 5*12*3; vs/(5*12*3) # sharing vs no sharing





###########################################################################
# COSTS OF CURRENT (IM vs ID)
postscript(file="figs/costs_current_per_patient.eps", width=4.4, height=4, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.35, 0.95, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, rep(NA, length(patients)), ylim=c(0,250*prabies), type="l", lwd=2, log="x", axes=FALSE,
     xlab="Monthly clinic throughput (new bite patients)",
     ylab="Direct medical costs (in USD) per patient treated")
axis(2, lwd=0.5); axis(1, lwd=0.5)

# IM
# ZAGREB or ESSEN
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAuci[3,], costPRDAlci[3,length(patients):1]),
        border=NA, col="dark gray")
lines(patients, costPRDA[3,], col="black", lwd=2)
text(100, 225*prabies, "Essen 4/ Zagreb")

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci[1,], costPRDAwastelci[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste[1,], col="dark red", lwd=2)
text(150, 150*prabies, "Updated TRC 0.5mL", col="dark red")

# TRC4 (dark red)
polygon(c(patients, sort(patients, decreasing=T)),
        c(costPRDAwasteuci1[1,], costPRDAwastelci1[1,length(patients):1]),
        border=NA, col="red")
lines(patients, costPRDAwaste1[1,], col="dark red", lwd=2)
text(150, 95*prabies, "Updated TRC 1mL", col="dark red")
dev.off()

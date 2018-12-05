###############################################################################
# Analysis to investigate possible changes to RIG administration policy
###############################################################################

# CURRENT - inject at site of wound with remainder at other site on the body:
# HRIG - 20 IU/kg of body weight
# ERIG - 40 IU/kg of body weight
# CHANGE to only injecting at site of wound - with resulting savings in RIG use
# Assume 5 ml vials of ERIG containing 550 IU each
# Vials can be shared but opened vials must be discarded at end of day and sometimes one person is given multiple vials

# Q. What is the proportion of Category III exposures eligible for RIG vs the proportion given RIG?
# high priority criteria for RIG administrated were identified by SAGE:
# **Multiple bites
# **Inervated sites: head, hands
# Immunocompromised
# Deep wounds
# **Confirmed vs Suspect dogs

# For simplicity we assume that only those bitten on the head and hands are administered RIG
# diagnostic testing is not conducted in most settings and many bite victims have multiple bites

###############################################################################
#                            Load in Libraries                                #
###############################################################################
rm(list=ls())
library(dplyr)
require(bbmle)

RIG <- read.csv("data/RIG_OB_anon.csv")
nrow(RIG)

# clean up weight
RIG$Wt <- as.numeric(gsub("kg", "", as.character(RIG$Approx.Wt)))
hist(RIG$Wt, breaks=seq(0,105,5)) #0:101)

# Gather unique areas
sort(unique(RIG$Address))
table(RIG$Address)

###############################################################################
#                  Patients, age, sex, weight etc                             #
###############################################################################
quantile(RIG$Age, na.rm=TRUE)
age_breaks = c(0,1, seq(5, 100, 5))
hist(RIG$Age, breaks=age_breaks)
mean(RIG$Age); median(RIG$Age)

# Sex
table(RIG$Sex)
table(RIG$Sex)/sum(table(RIG$Sex))

# Weight
mean(RIG$Wt, na.rm=TRUE); median(RIG$Wt, na.rm=TRUE)
wt.hist = hist(RIG$Wt)
pdf("figs/patients_weight.pdf", width = 5, height = 4)
par(mfrow=c(1,1), cex=0.8)
barplot(wt.hist$counts, names.arg=wt.hist$mids, xlab = "Weight (kgs)", ylab = "Patient frequency", col="red")
dev.off()

###############################################################################
#                        Extract information on volume                        #
###############################################################################
sort(unique(RIG$RIGs.Vol.Actually.Given))
table(RIG$RIGs.Vol.Actually.Given)
length(which(is.na(RIG$RIGs.Vol.Actually.Given)))

df <- dplyr::select(RIG, Site.of.wnd, RIGs.Vol.Actually.Given, Wt)

# Use function to transform free text of bite location to the set 6 columns:
# Head, Arm, Hand, Trunk, Leg, Foot
table(df$Site.of.wnd)
source("R/add_bite_loc.R")
df2 <- add_bite_loc(df, bite.site.col="Site.of.wnd")
table(df2$Bite.site.Head...neck) # could not assign bite location for 20 patients

# Add information on bite location to a single new column
for(m in 1:nrow(df)){
  if(df2[["Bite.site.Head...neck"]][m]=="true"){
    df2$Bite.loc[m] <- "Head"
  } else if(df2[["Bite.site.Arms"]][m]=="true"){
    df2$Bite.loc[m] <- "Arm"
  } else if(df2[["Bite.site.Hands"]][m]=="true"){
    df2$Bite.loc[m] <- "Hand"
  } else if(df2[["Bite.site.Trunk"]][m]=="true"){
    df2$Bite.loc[m] <- "Trunk"
  } else if(df2[["Bite.site.Legs"]][m]=="true"){
    df2$Bite.loc[m] <- "Leg"
  } else if(df2[["Bite.site.Feet"]][m]=="true"){
    df2$Bite.loc[m] <- "Foot"
  } else {
    df2$Bite.loc[m] <- NA
  }
}

# Distribution of where bitten
table(df2$Bite.loc)
table(df2$Bite.loc)/sum(table(df2$Bite.loc))

# NOTE 30 bites do not have locations. Which ones?
df2$Site.of.wnd[which(is.na(df2$Bite.loc))] # I would treat multi wounds as high risk! Scepult/ Scaplt - is trunk?

# Plot of RIG volume as counts
rig.dose.breaks <- c(0,0.1,0.25,0.5,1:9)
rig.hist = hist(RIG$RIGs.Vol.Actually.Given, breaks=rig.dose.breaks)
pdf("figs/patients_RIG_vol.pdf")
plot(rig.hist$counts, xaxt = "n", xlab="IU", ylab="Patients", type="l")
axis(1, at=0:12, labels=rig.dose.breaks)
dev.off()

pdf("figs/patients_RIG_vol_barplot.pdf", width = 8, height = 4)
par(mfrow=c(1,1), cex=0.8)
barplot(rig.hist$counts, names.arg=rig.hist$mids, xlab = "RIG volume", ylab = "Patient frequency", col="red")
dev.off()


# Plot RIG dose volume as proportion of individuals
df3 <- as.data.frame(table(RIG$RIGs.Vol.Actually.Given))
df3$prop <- df3$Freq/sum(df3$Freq)
plot(df3$Var1, df3$prop, xlab="IU", ylab="Patients", cex.axis=0.6)

# Plot RIG vol vs Wt
plot(RIG$Wt, RIG$RIGs.Vol.Actually.Given, pch=20) # Wt vs vol of RIG (vials of ERIG)
plot(RIG$Wt*40/1000, RIG$RIGs.Vol.Actually.Given, pch=20)
mean(RIG$RIGs.Vol.Actually.Given); mean(RIG$Wt*40/1000, na.rm=TRUE)

###############################################################################
#                       Simulation analysis
###############################################################################
RIG$Wt[which(is.na(RIG$Wt))] <- mean(RIG$Wt, na.rm=TRUE) # replace the NA

# Function to calculate RIG volume given type and potency
RIGweight = function(type = "ERIG", IU = 300, patient_weight){ # IU = 550
  if(type=="ERIG"){ vol = (patient_weight * 40)/IU
  } else { vol = (patient_weight * 20)/IU }
  vol
}

# Assign throughput
throughput = 50

# sample RIG patients to get IDs
ids = sample(1:700, throughput*12, replace = TRUE)

# Use sampled IDs to create patient linelist
PEP = data.frame(
  presentations = sort(sample(1:365, throughput*12, replace = TRUE)), # presentation days
  weights = RIG$Wt[ids], # Weights
  vol = RIG$RIGs.Vol.Actually.Given[ids]) # Volume received

PEP_df = tbl_df(PEP) # Convert to tibble
daily <- group_by(PEP_df, presentations) # group by presentation day
per_day <- dplyr::summarize(daily,
                            patients = n(),
                            vol_wt = sum(RIGweight(patient_weight=weights)), # Weight volume
                            vials_wt = ceiling(sum(RIGweight(patient_weight=weights))/5), # Weight vials
                            vol_wound = sum(vol), # Wound volume
                            vials_wound = ceiling(sum(vol)/5))

# Calculate vial use for each scenario
sum(per_day$vials_wt)
sum(per_day$vials_wound)

# look at different numbers of vials used for different regimens
patients=c(1,5,10,20,30,40,50,75,100,200,300,400,500) # New patients per month
reps = 100 # increase to 1000


# SIMULATION
vials_wt = vials_wound = matrix(nrow=length(patients), ncol=5) #set up matrix of results
count = 0

for(i in patients){ # loop through patient throughput
  count=count+1
  vials_wt_vec = vials_wound_vec = rep(NA, reps) # set up vectors

  for(j in 1:reps){ # realizations
    throughput = i # define throughput
    ids = sample(1:700, throughput*12, replace = TRUE) # sample RIG patients to get IDs

    # Use sampled IDs to create patient linelist
    PEP = data.frame(
      presentations = sort(sample(1:365, throughput*12, replace = TRUE)), # presentation days
      weights = RIG$Wt[ids], # Weights
      vol = RIG$RIGs.Vol.Actually.Given[ids]) # Volume received

    PEP_df = tbl_df(PEP) # Convert to tibble
    daily <- group_by(PEP_df, presentations) # group by presentation day
    per_day <- dplyr::summarize(daily,
                                patients = n(),
                                vol_wt = sum(RIGweight(patient_weight=weights)), # Weight volume
                                vials_wt = ceiling(sum(RIGweight(patient_weight=weights))/5), # Weight vials
                                vol_wound = sum(vol), # Wound volume
                                vials_wound = ceiling(sum(vol)/5)) # wound vials

    vials_wt_vec[j] = sum(per_day$vials_wt) # record vial use from patient weight
    vials_wound_vec[j] = sum(per_day$vials_wound) # record vial use from wound infiltration
  }
  # Store quantiles
  print(i); print(count)
  vials_wt[count,] = quantile(vials_wt_vec, c(0.01, 0.05, 0.5, 0.95, 0.99))
  vials_wound[count,] = quantile(vials_wound_vec, c(0.01, 0.05, 0.5, 0.95, 0.99))
}

vials_wt
vials_wound

# RIG use under different patient throughput
# logged xy
plot(patients, vials_wt[,3], type="l", log="xy")
lines(patients, vials_wt[,1], lwd=0.5)
lines(patients, vials_wt[,5], lwd=0.5)

lines(patients, vials_wound[,3], lwd=1, col="red")
lines(patients, vials_wound[,1], lwd=0.5, col="red")
lines(patients, vials_wound[,5], lwd=0.5, col="red")


# natural scale
plot(patients, vials_wt[,3], type="l")
lines(patients, vials_wt[,1], lwd=0.5)
lines(patients, vials_wt[,5], lwd=0.5)

lines(patients, vials_wound[,3], lwd=1, col="red")
lines(patients, vials_wound[,1], lwd=0.5, col="red")
lines(patients, vials_wound[,5], lwd=0.5, col="red")

# CALCULATE UNDER RESTRICTED VIALS
patient_dose_wound = (vials_wound[,3]/12)/patients
patient_dose_wt = (vials_wt[,3]/12)/patients
throughput = patients * 12
RIGvol = patient_dose_wound*throughput
RIGvol2 = patient_dose_wt*throughput

# plots of vial use versus throughput
par(mfrow=c(1,2), mar=c(3,3,1,1))
plot(patients*12, patient_dose_wound*throughput,
     ylab = "RIG volume", xlab = "Patients", type="l", log="x", ylim=c(0,3000)) # by wound
lines(throughput, patient_dose_wt*throughput, col="red") # by weight

plot(patients*12, patient_dose_wound*throughput,
     ylab = "RIG volume", xlab = "Patients", type="l", ylim=c(0,3000))
lines(throughput, patient_dose_wt*throughput, col="red")

# try to predict relationship
rlinear = RIGvol[-c(1:8)]; rlinear2 = RIGvol2[-c(1:8)] # RIG volume
tlinear = throughput[-c(1:8)] # throughput
m1 = glm(rlinear ~ tlinear)
m2 = lm(RIGvol~poly(throughput,2,raw=TRUE))
t2 <- 1:5000

m1wt = glm(rlinear2 ~ tlinear)
m2wt = lm(RIGvol2~poly(throughput,2,raw=TRUE))

# Relationship between throughput and vials use
plot(t2, predict(m2, data.frame(throughput=t2)), type="l", ylim=c(0,3000))
lines(t2, predict(m1, data.frame(tlinear=t2)), col="red")# Good fit
points(throughput, patient_dose_wound*throughput, col="red", pch=20)

points(throughput, patient_dose_wt*throughput, col="blue", pch=20)
lines(t2, predict(m2wt, data.frame(throughput=t2)), col="blue")
lines(t2, predict(m1wt, data.frame(tlinear=t2)), col="blue", lty=2) # Good fit

postscript(file="figs/RIG_wt_v_wound.eps", width=4, height=4, horizontal=F)
plot(throughput, patient_dose_wound*throughput,
     col="red", type="l", ylim=c(0,3000), ylab="Vials used", xlab = "patients treated")
lines(throughput, patient_dose_wt*throughput, col="blue")
legend(3000, 2000, legend=c("wound + IM", "wound only"), col=c("blue", "red"), lty = c(1,1), bty="n", cex=0.75)
dev.off()

# For wound
vials = coef(m1)["(Intercept)"] + coef(m1)["tlinear"]*throughput
(c(100,1000,5000) - coef(m1)["(Intercept)"])/ coef(m1)["tlinear"]

v=c(250,1000,5000)
p_wound = (v - coef(m1)["(Intercept)"])/ coef(m1)["tlinear"] # For wound
p_wt = (v - coef(m1wt)["(Intercept)"])/ coef(m1wt)["tlinear"] # For weight
bars = c("wound + IM", "wound only")

# Previous plot set up (for 550 IU / vial)
postscript(file="figs/patients_treated_RIG.eps", width=10, height=4, horizontal=F)
par(cex=0.65, lwd=0.5, plt=c(0.05, 0.35, 0.2, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(c(p_wt[1], p_wound[1]),
           axisnames = FALSE,
           main="250 vials available",
           col=c("dark blue", "red"),
           ylim=c(0,500), ylab="Patients treated")
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1.2)

par(cex=0.65, new=TRUE, lwd=0.5, plt=c(0.375, 0.675, 0.2, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(c(p_wt[2], p_wound[2]),
           axisnames = FALSE,
           main="1000 vials available",
           col=c("dark blue", "red"),
           ylim=c(0,10000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1.2)

dev.off()

# plot set up for paper (for 300 IU / vial)
postscript(file="figs/patients_treated_RIG_2.eps", width=5, height=4, horizontal=F)
par(cex=0.75, lwd=0.5, plt=c(0.05, 0.35, 0.2, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(c(p_wt[2], p_wound[2]),
           axisnames = FALSE,
           main="1000 vials available",
           col=c("dark blue","red"),
           ylim=c(0,50000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1.2)

par(cex=0.75, new=TRUE, lwd=0.5, plt=c(0.375, 0.675, 0.2, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
bp=barplot(c(p_wt[3], p_wound[3]),
           axisnames = FALSE,
           main="5000 vials available",
           col=c("dark blue", "red"),
           ylim=c(0,50000))
text(bp, par("usr")[3], labels = bars, srt = 60, adj = 1, xpd = TRUE, cex=1.2)
dev.off()

# 107.090, 1138.786, 6641.165
# 175.0547,  6739.6061, 41750.5470


#____________________________________Additional exploring ____________________________________________
# When 1000 vials are available:
# 1139 patients can be treated under current policy (by weight)
# wheras 6740 can be treated assuming administration at the wound site only
# i.e. 6749/1139 = 5.9x more patients can be treated OR 6749-1139 = 5610 more patients can be treated!


# OB sees 8-10 people per day
p = 9*365/12; p # 273 patients per month
v1 = coef(m1)["(Intercept)"] + coef(m1)["tlinear"]*p # 262 vials
v2 = coef(m1wt)["(Intercept)"] + coef(m1wt)["tlinear"]*p # 369 vials
v2/v1

# in a clinic like Shimla, where 8-10 people are seen per day (assuming all need RIG!) = 273 patients/month
# 1.4 x patients can be treated if given at wound only

v1 = coef(m1)["(Intercept)"] + coef(m1)["tlinear"]*patients
v2 = coef(m1wt)["(Intercept)"] + coef(m1wt)["tlinear"]*patients


RIGvials = seq(50,1000,50)

treated_wt = treated_wound = matrix(NA, ncol=length(RIGvials), nrow=length(patients))
for(i in 1:length(RIGvials)){
  treated_wound[,i] = RIGvials[i]/patient_dose_wound
  treated_wt[,i] = RIGvials[i]/patient_dose_wt
}

# Can now see under different vial constraints how many patients could be seen
v = c(50, 100, 250, 1000)
(patients*12)*treated_wound
patients*12

# persons treated - given different availability
plot(patients*12, treated_wound[,10], type="l", ylim=c(0,3500))
lines(patients*12, treated_wound[,2])
lines(patients*12, treated_wound[,3])
lines(patients*12, treated_wound[,5])
lines(patients*12, treated_wound[,8])

# FOR GAVI MODELING: in Lancet ID paper used numbers from 550 IU vials
write.csv(data.frame(throughput = patients,
                     dose_wound = patient_dose_wound,
                     dose_weight = patient_dose_wt),
          "output/RIGdosage.csv")


# When 1000 vials are available:
# 1139 patients can be treated under current policy (by weight)
# wheras 6740 can be treated assuming administration at the wound site only
# i.e. 6749/1139 = 5.9x more patients can be treated OR 6749-1139 = 5610 more patients can be treated!

# in a clinic like Shimla, where 8-10 people are seen per day (assuming all need RIG!) = 273 patients/month
# 1.4 x patients can be treated if given at wound only



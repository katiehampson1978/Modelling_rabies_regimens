#Examine how costs of vaccination vary with patient throughput
# 3 Jun 2017
rm(list=ls())
# Calculate vials used, cost, 'wastage' under different throughput scenarios

#Vaccination schedule and regimens current in use
schedule=c(0, 3, 7, 14, 21, 28, 90) #dates for delivery
essen=c(1,1,1,1,0,1,0)      #IM Essen 5 doses - 1 vial/ vaccination
essen4=c(1,1,1,1,0,0,0)     #IM Essen reduced 4 dose
zagreb=c(2,0,1,0,1,0,0)     #IM Zagreb 3 visit
TRC=c(2,2,2,0,0,1,1)        #ID Thai Red Cross - 0.1 ml/ vaccination
TRC4=c(2,2,2,0,0,2,0)       #ID reduced 4 dose TRC
site8=c(8,0,4,0,0,1,1)		#ID 8-site
ID4=c(4,0,2,0,0,1,1)        #ID 4-site economical (not yet approved)
ID4mw=c(4,0,2,0,0,1,0)		#ID 4-site - 3 visit regimen proposed by MW

# new vaccination schedules and regimens to investigate
weekPEP=c(4,4,4,0,0,0,0)    #ID 1-week - Narayana et al, Shantavasinkul et al, Sudarshen et al.
IM_i1 = c(1,1,1,0,0,0,0)
ID_i1 = c(2,2,2,0,0,0,0)
IM_i2 = c(2,0,1,0,0,0,0) # Huang et al

source("R/PEPfunctions.R")

# ANALYSE:
# 1. IM essen4 - note equivalent to zagreb! (except if with low compliance 2 doses at start more effective)
# 2. ID TRC4
# 3. IM_i1, IM_i2
# 4. ID_i1, weekPEP
# 5. optional ID4mw

# 100% compliance and 75% compliance
# 0.5 and 1mL vials
# Assume perfect usage versus some wastage

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
#___________________________________________________________

#look at different numbers of vials used for different regimens
patients=c(1,5,10,20,30,40,50,75,100,500) # New patients per month

# 1. 100% compliance, 0.5ml vials, perfect usage
vTRC4pc100=vEssen4pc100=vID4pc100=vZagrebpc100=vwkPEPpc100=vID_i1pc100=vIM_i1pc100=vIM_i2pc100=matrix(nrow=length(patients), ncol=5)

# TRC4
for(i in 1:length(patients)){ # 500 simulations
	vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=5, TRC4, pc=1))
	vTRC4pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
	print(i)
	}
write.csv(vTRC4pc100, file="output/vTRC4.csv", row.names=FALSE)

# ID4
for(i in 1:length(patients)){
	vial.res=replicate(500,	eval.vials(m.patients=patients[i], doses.per.vial=5, ID4, pc=1))
	vID4pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
	print(i)
	}
write.csv(vID4pc100, file="output/vID4.csv", row.names=FALSE)

#essen4 (or Zagreb - they are equivalent)
for(i in 1:length(patients)){
	vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, essen4, pc=1))
	vEssen4pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
	print(i)
	}
write.csv(vEssen4pc100, file="output/vEssen4.csv", row.names=FALSE)

#Zagreb - just included for completeness
for(i in 1:length(patients)){
	vial.res=replicate(500, #change to 500 simulations
		eval.vials(m.patients=patients[i], doses.per.vial=1, zagreb, pc=1))
	vZagrebpc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
	print(i)
	}
write.csv(vZagrebpc100, file="output/vZagreb.csv", row.names=FALSE)

#1 week ID
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 5, weekPEP, pc=1))
  vwkPEPpc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vwkPEPpc100, file="output/vwkPEP.csv", row.names=FALSE)

# Investigation of new ID1 (c(2,2,2,0,0,0,0)) - half of 1 week PEP
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 5, ID_i1, pc=1))
  vID_i1pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID_i1pc100, file="output/vID_i1.csv", row.names=FALSE)

#IM_i1
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i1, pc=1))
  vIM_i1pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i1pc100, file="output/vIM_i1.csv", row.names=FALSE)

# IM_i2
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i2, pc=1))
  vIM_i2pc100[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i2pc100, file="output/vIM_i2.csv", row.names=FALSE)

#_______________________________________________________________________________
#2
#100% compliance - 1mL vials - perfect usage
vTRC4pc100_1mL=vID4pc100_1mL=vwkPEPpc100_1mL=vID_i1pc100_1ml=vID4mw_pc100_1mL=matrix(nrow=length(patients), ncol=5)

#TRC4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, TRC4, pc=1))
  vTRC4pc100_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vTRC4pc100_1mL, file="output/vTRC4_1mL.csv", row.names=FALSE)

#ID4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4, pc=1))
  vID4pc100_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4pc100_1mL, file="output/vID4_1mL.csv", row.names=FALSE)

#ID4mw
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4mw, pc=1))
  vID4mw_pc100_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4mw_pc100_1mL, file="output/vID4mw__1mL.csv", row.names=FALSE)


#1 week ID
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 10, weekPEP, pc=1))
  vwkPEPpc100_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vwkPEPpc100_1mL, file="output/vwkPEP_1mL.csv", row.names=FALSE)

# Investigation of new ID1 (c(2,2,2,0,0,0,0)) - half of 1 week PEP
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 10, ID_i1, pc=1))
  vID_i1pc100_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID_i1pc100_1ml, file="output/vID_i1_1ml.csv", row.names=FALSE)

#_______________________________________________________________________________
#3 - 100% compliance - 1mL vials - some waste
#Some wastage - 1mL vials
vTRC4pc100waste1mL=vID4pc100waste1mL=vwkPEPpc100waste1mL=vID_i1pc100waste1ml=vID4mw_pc100waste1mL=matrix(nrow=length(patients), ncol=5)



#TRC4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=8, TRC4, pc=1))
  vTRC4pc100waste1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vTRC4pc100waste1mL, file="output/vTRC4waste1mL.csv", row.names=FALSE)

#ID4
  for(i in 1:length(patients)){
    vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=8, ID4, pc=1))
    vID4pc100waste1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
write.csv(vID4pc100waste1mL, file="output/vID4waste1mL.csv", row.names=FALSE)

#ID4mw
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=8, ID4mw, pc=1))
  vID4mw_pc100waste1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4mw_pc100waste1mL, file="output/vID4mw_waste1mL.csv", row.names=FALSE)


#1 week ID
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 8, weekPEP, pc=1))
  vwkPEPpc100waste1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vwkPEPpc100waste1mL, file="output/vwkPEPwaste1mL.csv", row.names=FALSE)

# Investigation of new ID1 (c(2,2,2,0,0,0,0)) - half of 1 week PEP
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 8, ID_i1, pc=1))
  vID_i1pc100waste1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID_i1pc100waste1ml, file="output/vID_i1waste1ml.csv", row.names=FALSE)



#_______________________________________________________________________________
#Compliance calculations...
#75% Compliance - 1mL vials - perfect usage
vTRC4pc75_1mL=vEssen4pc75_1mL=vZagreb_pc75_1mL=vwkPEP_pc75_1mL=vIM_i1pc75_1ml=vID_i1pc75_1ml=vIM_i2pc75_1ml=vID4mw_pc75_1mL=vID4_pc75_1mL = matrix(nrow=length(patients), ncol=5)

#TRC4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, TRC4, pc=0.75))
  vTRC4pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vTRC4pc75_1mL, file="output/vTRC4pc75_1mL.csv", row.names=FALSE)

#essen4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, essen4, pc=0.75))
  vEssen4pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vEssen4pc75_1mL, file="output/vEssen4pc75_1mL.csv", row.names=FALSE)

#Zagreb
for(i in 1:length(patients)){
  vial.res=replicate(500,
                     eval.vials(m.patients=patients[i], doses.per.vial=1, zagreb, pc=0.75))
  vZagreb_pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vZagreb_pc75_1mL, file="output/vZagreb_pc75_1mL.csv", row.names=FALSE)

#1 week ID
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=5, weekPEP, pc=0.75))
  vwkPEP_pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vwkPEP_pc75_1mL, file="output/vwkPEP_pc75_1mL", row.names=FALSE)

# Investigation of new ID1 (c(2,2,2,0,0,0,0)) - half of 1 week PEP
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 10, ID_i1, pc=0.75))
  vID_i1pc75_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID_i1pc75_1ml, file="output/vID_i1pc75_1ml", row.names=FALSE)

#IM_i1
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i1, pc=0.75))
  vIM_i1pc75_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i1pc75_1ml, file="output/vIM_i1pc75_1ml", row.names=FALSE)

# IM_i2
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i2, pc=0.75))
  vIM_i2pc75_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i2pc75_1ml, file="output/vIM_i2pc75_1ml.csv", row.names=FALSE)

#ID4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4, pc=.75))
  vID4_pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4_pc75_1mL, file="output/vID4_pc75_1mL.csv", row.names=FALSE)

#ID4mw
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4mw, pc=.75))
  vID4mw_pc75_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4mw_pc75_1mL, file="output/vID4mw_pc75_1mL.csv", row.names=FALSE)




#___________________________________________________________________________
#50% Compliance - 1mL vials - perfect usage
vTRC4pc50_1mL=vEssen4pc50_1mL=vZagreb_pc50_1mL=vwkPEP_pc50_1mL=vIM_i1pc50_1ml=vID_i1pc50_1ml=vIM_i2pc50_1ml=
  vID4mw_pc50_1mL=vID4_pc50_1mL=matrix(nrow=length(patients), ncol=5)

#50% Compliance - 1mL vials - perfect usage
vTRC4pc50_1mL=vEssen4pc50_1mL=vZagreb_pc50_1mL=vwkPEP_pc50_1mL=vIM_i1pc50_1ml=vID_i1pc50_1ml=vIM_i2pc50_1ml=
  vID4mw_pc50_1mL=vID4_pc50_1mL=matrix(nrow=length(patients), ncol=5)

#TRC4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, TRC4, pc=0.50))
  vTRC4pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vTRC4pc50_1mL, file="output/vTRC4pc50_1mL.csv", row.names=FALSE)

#essen4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, essen4, pc=0.50))
  vEssen4pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vEssen4pc50_1mL, file="output/vEssen4pc50_1mL.csv", row.names=FALSE)

#Zagreb
for(i in 1:length(patients)){
  vial.res=replicate(500,
                     eval.vials(m.patients=patients[i], doses.per.vial=1, zagreb, pc=0.50))
  vZagreb_pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vZagreb_pc50_1mL, file="output/vZagreb_pc50_1mL.csv", row.names=FALSE)

#1 week ID
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=5, weekPEP, pc=0.50))
  vwkPEP_pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vwkPEP_pc50_1mL, file="output/vwkPEP_pc50_1mL", row.names=FALSE)

# Investigation of new ID1 (c(2,2,2,0,0,0,0)) - half of 1 week PEP
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], 10, ID_i1, pc=0.50))
  vID_i1pc50_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID_i1pc50_1ml, file="output/vID_i1pc50_1ml", row.names=FALSE)

#IM_i1
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i1, pc=0.50))
  vIM_i1pc50_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i1pc50_1ml, file="output/vIM_i1pc50_1ml", row.names=FALSE)

# IM_i2
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=1, IM_i2, pc=0.50))
  vIM_i2pc50_1ml[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vIM_i2pc50_1ml, file="output/vIM_i2pc50_1ml.csv", row.names=FALSE)

#ID4
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4, pc=.5))
  vID4_pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4_pc50_1mL, file="output/vID4_pc50_1mL.csv", row.names=FALSE)

#ID4mw
for(i in 1:length(patients)){
  vial.res=replicate(500, eval.vials(m.patients=patients[i], doses.per.vial=10, ID4mw, pc=.5))
  vID4mw_pc50_1mL[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}
write.csv(vID4mw_pc50_1mL, file="output/vID4mw_pc50_1mL.csv", row.names=FALSE)


#___________________________________________________________________________
#####################################################################################
#Work out healthcare costs per rabies death averted
overhead=0.5
material=0.1
vial.cost=10
prabies=0.19
source("R/PEPcosts.R")

# All different regimens combined
regimens=list(TRC4=TRC4, # Default ID
              ID4=ID4,
              essen4=essen4, zagreb=zagreb, # Default IM
              weekPEP=weekPEP, ID_i1=ID_i1, # Under investigation ID
              IM_i1=IM_i1, IM_i2=IM_i2, # Under investigation IM
              ID4mw = ID4mw) # Mary warrell

#####################################################################################
#1 - 100% compliance - 0.5mL vials - perfect usage
vuse0.5=list(vTRC4pc100=read.csv("output/vTRC4.csv"),
             vID4pc100=read.csv("output/vID4.csv"),
             vEssen4pc100=read.csv("output/vEssen4.csv") ,
             vZagrebpc100=read.csv("output/vZagreb.csv"),
             vwkPEPpc100=read.csv("output/vwkPEP.csv"),
             vID_i1pc100=read.csv("output/vID_i1.csv"),
             vIM_i1pc100=read.csv("output/vIM_i1.csv"),
             vIM_i2pc100=read.csv("output/vIM_i2.csv")
)
#_______________________________________________________________________________
#2 - 100% compliance - 1mL vials - perfect usage
vuse1=list(vTRC4pc100_1mL=read.csv("output/vTRC4_1mL.csv"),
           vID4pc100_1mL=read.csv("output/vID4_1mL.csv"),
           vEssen4pc100=read.csv("output/vEssen4.csv"),
           vZagrebpc100=read.csv("output/vZagreb.csv"),
           vwkPEPpc100_1mL=read.csv("output/vwkPEP_1mL.csv"),
           vID_i1pc100=read.csv("output/vID_i1_1ml.csv"),
           vIM_i1pc100=read.csv("output/vIM_i1.csv"),
           vIM_i2pc100=read.csv("output/vIM_i2.csv"),
           vID4mw_pc100=read.csv("output/vIM_i2.csv")
)
#_______________________________________________________________________________________
#3 -100% compliance - 1mL vials - some waste
vuse1waste=list(vTRC4pc100_1mL=read.csv("output/vTRC4waste1mL.csv"),
                vID4pc100_1mL=read.csv("output/vID4waste1mL.csv"),
                vEssen4pc100=read.csv("output/vEssen4.csv"),
                vZagrebpc100=read.csv("output/vZagreb.csv"),
                vwkPEPpc100_1mL=read.csv("output/vwkPEPwaste1mL.csv"),
                vID_i1pc100=read.csv("output/vID_i1waste1ml.csv"),
                vIM_i1pc100=read.csv("output/vIM_i1.csv"),
                vIM_i2pc100=read.csv("output/vIM_i2.csv")
)

#Compare the costs per rabies deaths prevented
costPRDA=costPRDAuci=costPRDAlci=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDA1=costPRDAuci1=costPRDAlci1=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDAwaste=costPRDAwasteuci=costPRDAwastelci=matrix(NA, nrow=length(regimens), ncol=length(patients))
costPRDA1waste=costPRDA1wasteuci=costPRDA1wastelci=matrix(NA, nrow=length(regimens), ncol=length(patients))

lccostPRDA=lccostPRDAuci=costPRDAlci=matrix(NA, nrow=length(regimens), ncol=length(patients))
lccostPRDA1=lccostPRDAuci1=costPRDAlci1=matrix(NA, nrow=length(regimens), ncol=length(patients))
lccostPRDAwaste=lccostPRDAwasteuci=costPRDAwastelci=matrix(NA, nrow=length(regimens), ncol=length(patients))
lccostPRDA1waste=lccostPRDA1wasteuci=costPRDA1wastelci=matrix(NA, nrow=length(regimens), ncol=length(patients))


for (i in 1:length(regimens)){
	DA=(prabies*patients*12) #deaths averted
	# costPRDA[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse0.5[i])[,3], unlist(regimens[i]))/DA
	# costPRDAuci[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse0.5[i])[,1], unlist(regimens[i]))/DA
	# costPRDAlci[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse0.5[i])[,5], unlist(regimens[i]))/DA

	costPRDA1[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1[i])[,3], unlist(regimens[i]))/DA
	costPRDAuci1[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1[i])[,1], unlist(regimens[i]))/DA
	costPRDAlci1[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1[i])[,5], unlist(regimens[i]))/DA

	# costPRDA1waste[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1waste[i])[,3], unlist(regimens[i]))/DA
	# costPRDA1wasteuci[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1waste[i])[,1], unlist(regimens[i]))/DA
	# costPRDA1wastelci[i,]=costs(overhead, material, vial.cost, patients, as.data.frame(vuse1waste[i])[,5], unlist(regimens[i]))/DA
}

#ICER=cost/(effectiveness(No PEP)-effectiveness(PEP))
#ICER=cost(deathsNoPEP-deathsPEP)

#Plot the cost to healthcare providers per rabies death averted and compare vial use (but use previous stats because pics are not great)
# Essen4 (dark grey), TRC4 (blue), ID4 (dark red), weekPEP (light grey), ID_i1 (blue), IM_i1 (green)

# FOR PRESENTATION
postscript(file="figs/cost_PRDAmw.eps", width=4.4, height=4, horizontal=F)
par(cex=0.5, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)


###########################################################################

postscript(file="figs/cost_PRDA.eps", width=4.4, height=4, horizontal=F)
par(cex=0.5, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA1[3,], ylim=c(0,250), type="l", lwd=2, log="x", axes=FALSE,
     xlab="Monthly clinic throughput (new bite patients)", ylab="Direct medical costs (in USD) per rabies death averted")
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[3,], costPRDAlci1[3,length(patients):1]), border=NA, col="dark gray")
lines(patients, costPRDA1[3,], col="black", lwd=2) # ZAGREB or ESSEN
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[7,], costPRDAlci1[7,length(patients):1]), border=NA, col="green")
lines(patients, costPRDA1[7,], col="dark green", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[8,], costPRDAlci1[8,length(patients):1]), border=NA, col="light green")
lines(patients, costPRDA1[8,], col="green", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[9,], costPRDAlci1[9,length(patients):1]), border=NA, col="yellow")
lines(patients, costPRDA1[9,], col="orange", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250, "0.5mL vials", font=2)

# IM
plot(patients, costPRDA[3,], ylim=c(0,250), type="l", lwd=2, log="x", axes=FALSE,
	xlab="Monthly clinic throughput (new bite patients)", ylab="Direct medical costs (in USD) per rabies death averted")
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[3,], costPRDAlci[3,length(patients):1]), border=NA, col="dark gray")
lines(patients, costPRDA[3,], col="black", lwd=2) # ZAGREB or ESSEN
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[7,], costPRDAlci[7,length(patients):1]), border=NA, col="green")
lines(patients, costPRDA[7,], col="dark green", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[8,], costPRDAlci[8,length(patients):1]), border=NA, col="light green")
lines(patients, costPRDA[8,], col="green", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 250, "0.5mL vials", font=2)

plot(patients, costPRDA[3,], ylim=c(0,250), type="l", lwd=2, log="x", axes=FALSE,
     xlab="Monthly clinic throughput (new bite patients)", ylab="Direct medical costs (in USD) per rabies death averted")
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[3,], costPRDAlci[3,length(patients):1]), border=NA, col="dark gray")
lines(patients, costPRDA[3,], col="black", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[1,], costPRDAlci[1,length(patients):1]), border=NA, col="blue")
lines(patients, costPRDA[1,], col="dark blue", lwd=2)
# polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[2,], costPRDAlci[2,length(patients):1]), border=NA, col="red")
# lines(patients, costPRDA[2,], col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[5,], costPRDAlci[5,length(patients):1]), border=NA, col="light gray")
lines(patients, costPRDA[5,], col="gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[6,], costPRDAlci[6,length(patients):1]), border=NA, col="light blue")
lines(patients, costPRDA[6,], col="blue", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[7,], costPRDAlci[7,length(patients):1]), border=NA, col="green")
lines(patients, costPRDA[7,], col="dark green", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[8,], costPRDAlci[8,length(patients):1]), border=NA, col="light green")
lines(patients, costPRDA[8,], col="green", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)


text(200, 230, "High throughput: \n complete \n vial sharing")
text(5, 30, "Low throughput:\n infrequent \n vial sharing")
lines(-10:1001, rep(95,1012), lty=2, col="blue")
# lines(-10:1001, rep(95,1012), lty=2, col="red")
lines(-10:1001, rep(132,1012), lty=2, col="gray")
lines(-10:1001, rep(70,1012), lty=2, col="light blue")
text(30, 250, "0.5mL vials", font=2)


par(new=TRUE, plt=c(0.52, 0.92, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA1[3,], ylim=c(0,250), type="l", lwd=2, log="x", axes=FALSE,
	xlab="Monthly clinic throughput (new bite patients)", ylab="")

polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[3,], costPRDAlci1[3,length(patients):1]), border=NA, col="dark gray")
lines(patients, costPRDA1[3,], col="black", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[1,], costPRDAlci1[1,length(patients):1]), border=NA, col="blue")
lines(patients, costPRDA1[1,], col="dark blue", lwd=2)
# polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[2,], costPRDAlci1[2,length(patients):1]), border=NA, col="red")
# lines(patients, costPRDA1[2,], col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[6,], costPRDAlci1[6,length(patients):1]), border=NA, col="light blue")
lines(patients, costPRDA1[6,], col="blue", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci1[5,], costPRDAlci1[5,length(patients):1]), border=NA, col="light gray")
lines(patients, costPRDA1[5,], col="gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[7,], costPRDAlci[7,length(patients):1]), border=NA, col="green")
lines(patients, costPRDA[7,], col="dark green", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[8,], costPRDAlci[8,length(patients):1]), border=NA, col="light green")
lines(patients, costPRDA[8,], col="green", lwd=2)

axis(2, lwd=0.5); axis(1, lwd=0.5)

#text(200, 220, "High throughput"); text(3, 30, "Low throughput")
lines(-10:1001, rep(55,1012), lty=2, col="blue")
#lines(-10:1001, rep(55,1012), lty=2, col="red")
lines(-10:1001, rep(73,1012), lty=2, col="gray")
lines(-10:1001, rep(41,1012), lty=2, col="light blue")
text(30, 250, "1mL vials", font=2)
dev.off()


#Plot the cost to healthcare providers per rabies death averted and compare vial use (but use previous stats because pics are not great)
postscript(file="figs/cost_PRDA_waste.eps", width=4.4, height=4, horizontal=F)
par(cex=0.5, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA1waste[3,], ylim=c(0,250), type="l", lwd=2, log="x", axes=FALSE,
	xlab="Monthly clinic throughput (new bite patients)", ylab="Direct medical costs (in USD) per rabies death averted")
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDA1wasteuci[3,], costPRDA1wastelci[3,length(patients):1]), border=NA, col="dark gray")
lines(patients, costPRDA1waste[3,], col="black", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDA1wasteuci[1,], costPRDA1wastelci[1,length(patients):1]), border=NA, col="blue")
lines(patients, costPRDA1waste[1,], col="dark blue", lwd=2)
# polygon(c(patients, sort(patients, decreasing=T)), c(costPRDA1wasteuci[2,], costPRDA1wastelci[2,length(patients):1]), border=NA, col="red")
# lines(patients, costPRDA1waste[2,], col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDA1wasteuci[5,], costPRDA1wastelci[5,length(patients):1]), border=NA, col="light gray")
lines(patients, costPRDA1waste[5,], col="gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDA1wasteuci[6,], costPRDA1wastelci[6,length(patients):1]), border=NA, col="light blue")
lines(patients, costPRDA1waste[6,], col="blue", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)

text(200, 230, "High throughput: \n complete \n vial sharing")
text(5, 30, "Low throughput:\n infrequent \n vial sharing")
lines(-10:1001, rep(55,1012), lty=2, col="blue")
#lines(-10:1001, rep(55,1012), lty=2, col="red")
lines(-10:1001, rep(73,1012), lty=2, col="gray")
lines(-10:1001, rep(41,1012), lty=2, col="light blue")
text(30, 250, "1mL vials", font=2)
dev.off()



#FIGURE 1
postscript(file="figs/VialUseProp.eps", width=8.5, height=4, horizontal=F)

par(cex=0.65, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, as.data.frame(vuse1[1])[,3]/as.data.frame(vuse1[3])[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, log = "x",
     xlab="Average number of new patients per month",
     ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 1.015, "1mL vials", font=2)

uciIM_i2_1mL=as.data.frame(vuse1[8])[,5]/as.data.frame(vuse1[3])[,3]
lciIM_i2_1mL=as.data.frame(vuse1[8])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciIM_i2_1mL, lciIM_i2_1mL[length(patients):1]), border=NA, col="green")
lines(patients, as.data.frame(vuse1[8])[,3]/as.data.frame(vuse1[3])[,3], type="l", lwd=2, col="dark green")


uciTRC4_1mL=as.data.frame(vuse1[1])[,5]/as.data.frame(vuse1[3])[,3]
lciTRC4_1mL=as.data.frame(vuse1[1])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4_1mL, lciTRC4_1mL[length(patients):1]), border=NA, col="blue")
lines(patients, as.data.frame(vuse1[1])[,3]/as.data.frame(vuse1[3])[,3], type="l", lwd=2, col="dark blue")

uci1wk_1mL=as.data.frame(vuse1[5])[,5]/as.data.frame(vuse1[3])[,3]
lci1wk_1mL=as.data.frame(vuse1[5])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk_1mL, lci1wk_1mL[length(patients):1]), border=NA, col="light gray")
lines(patients, as.data.frame(vuse1[5])[,3]/as.data.frame(vuse1[3])[,3], type="l", col="gray", lwd=2)

uciIM_i1_1mL=as.data.frame(vuse1[6])[,5]/as.data.frame(vuse1[3])[,3]
lciIM_i1_1mL=as.data.frame(vuse1[6])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciIM_i1_1mL, lciIM_i1_1mL[length(patients):1]), border=NA, col="light blue")
lines(patients, as.data.frame(vuse1[6])[,3]/as.data.frame(vuse1[3])[,3], type="l", col="blue", lwd=2)

#lines(rep(50,11), seq(-0.1, 0.95, by=0.1))
text(200, 0.9, "High throughput \nclinics: almost \n complete vial \n sharing")

#lines(rep(10,11), seq(-0.1, 0.95, by=0.1))
text(3, 0.2, "Low \n throughput\n clinics:  \n almost no \n vial \n sharing")
lines(c(-10:100,1000), rep(1.48/5,112), lty=2, col="gray")
lines(c(-10:100,1000), rep(1/5,112), lty=2, col="blue")
lines(c(-10:100,1000), rep(0.6/5,112), lty=2, col="light blue")
dev.off()



par(new=TRUE, plt=c(0.52, 0.92, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, as.data.frame(vuse1[1])[,3]/as.data.frame(vuse1[3])[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, log = "x",
  xlab="Average number of new patients per month",
  ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 1.015, "1mL vials", font=2)

uciTRC4_1mL=as.data.frame(vuse1[1])[,5]/as.data.frame(vuse1[3])[,3]
lciTRC4_1mL=as.data.frame(vuse1[1])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4_1mL, lciTRC4_1mL[length(patients):1]), border=NA, col="blue")
lines(patients, as.data.frame(vuse1[1])[,3]/as.data.frame(vuse1[3])[,3], type="l", lwd=2, col="dark blue")

uciID4_1mL=as.data.frame(vuse1[2])[,5]/as.data.frame(vuse1[3])[,3]
lciID4_1mL=as.data.frame(vuse1[2])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4_1mL, lciID4_1mL[length(patients):1]), border=NA, col="red")
lines(patients, as.data.frame(vuse1[2])[,3]/as.data.frame(vuse1[3])[,3], type="l", col="dark red", lwd=2)

uci1wk_1mL=as.data.frame(vuse1[5])[,5]/as.data.frame(vuse1[3])[,3]
lci1wk_1mL=as.data.frame(vuse1[5])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk_1mL, lci1wk_1mL[length(patients):1]), border=NA, col="light gray")
lines(patients, as.data.frame(vuse1[5])[,3]/as.data.frame(vuse1[3])[,3], type="l", col="gray", lwd=2)

uciID4mw_1mL=as.data.frame(vuse1[6])[,5]/as.data.frame(vuse1[3])[,3]
lciID4mw_1mL=as.data.frame(vuse1[6])[,1]/as.data.frame(vuse1[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4mw_1mL, lciID4mw_1mL[length(patients):1]), border=NA, col=colors()[373])
lines(patients, as.data.frame(vuse1[6])[,3]/as.data.frame(vuse1[3])[,3], type="l", col="dark red", lwd=2, lty=2)


#lines(rep(50,11), seq(-0.1, 0.95, by=0.1))
text(250, 0.8, "High throughput")
#lines(rep(10,11), seq(-0.1, 0.95, by=0.1))
text(3, 0.05, "Low \n throughput")
#arrows(100,0.1,5,0.23, 0.05)

lines(c(-10:100,1000), rep(1.48/5,112), lty=2, col="gray")
lines(c(-10:100,1000), rep(1/5,112), lty=2, col="blue")
lines(c(-10:100,1000), rep(1/5,112), lty=2, col="red")
lines(c(-10:100,1000), rep(0.88/5,112), lty=3, col="red")
mtext("B", font=2, at=0.4, line=-1)
dev.off()


#FIGURE 1
postscript(file="VialUsePropWaste.eps", width=4.4, height=4, horizontal=F)
par(cex=0.45, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, as.data.frame(vuse0.5waste[1])[,3]/as.data.frame(vuse0.5waste[3])[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, log = "x",
  xlab="Average number of new patients per month",
  ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 1.015, "0.5mL vials", font=2)

uciTRC4=as.data.frame(vuse0.5waste[1])[,5]/as.data.frame(vuse0.5waste[3])[,3]
lciTRC4=as.data.frame(vuse0.5waste[1])[,1]/as.data.frame(vuse0.5waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4, lciTRC4[length(patients):1]), border=NA, col="blue")
lines(patients, as.data.frame(vuse0.5waste[1])[,3]/as.data.frame(vuse0.5waste[3])[,3], type="l", lwd=2, col="dark blue")

uciID4=as.data.frame(vuse0.5waste[2])[,5]/as.data.frame(vuse0.5waste[3])[,3]
lciID4=as.data.frame(vuse0.5waste[2])[,1]/as.data.frame(vuse0.5waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4, lciID4[length(patients):1]), border=NA, col="red")
lines(patients, as.data.frame(vuse0.5waste[2])[,3]/as.data.frame(vuse0.5waste[3])[,3], type="l", col="dark red", lwd=2)

uci1wk=as.data.frame(vuse0.5waste[5])[,5]/as.data.frame(vuse0.5waste[3])[,3]
lci1wk=as.data.frame(vuse0.5waste[5])[,1]/as.data.frame(vuse0.5waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk, lci1wk[length(patients):1]), border=NA, col="light gray")
lines(patients, as.data.frame(vuse0.5waste[5])[,3]/as.data.frame(vuse0.5waste[3])[,3], type="l", col="gray", lwd=2)

uciID4mw=as.data.frame(vuse0.5waste[6])[,5]/as.data.frame(vuse0.5waste[3])[,3]
lciID4mw=as.data.frame(vuse0.5waste[6])[,1]/as.data.frame(vuse0.5waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4mw, lciID4mw[length(patients):1]), border=NA, col=colors()[373])
lines(patients, as.data.frame(vuse0.5waste[6])[,3]/as.data.frame(vuse0.5waste[3])[,3], type="l", col="dark red", lwd=2, lty=2)

#lines(rep(50,11), seq(-0.1, 0.95, by=0.1))
text(200, 0.9, "High throughput \nclinics: almost \n complete vial \n sharing")

#lines(rep(10,11), seq(-0.1, 0.95, by=0.1))
text(3, 0.2, "Low \n throughput\n clinics:  \n almost no \n vial \n sharing")

lines(-10:1001, rep(1.5/2.5,1012), lty=2, col="gray")
lines(-10:1001, rep(1.02/2.5,1012), lty=2, col="blue")
lines(-10:1001, rep(1.02/2.5,1012), lty=2, col="red")
lines(-10:1001, rep(0.88/2.5,1012), lty=3, col="red", lwd="1")
mtext("A", font=2, at=0.4, line=-1)
text(10, 0.33, "3-visit, 4 site ID, no waste", col="red")

par(new=TRUE, plt=c(0.52, 0.92, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, as.data.frame(vuse1waste[1])[,3]/as.data.frame(vuse1waste[3])[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, log = "x",
  xlab="Average number of new patients per month",
  ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
text(30, 1.015, "1mL vials", font=2)

uciTRC4_1mL=as.data.frame(vuse1waste[1])[,5]/as.data.frame(vuse1waste[3])[,3]
lciTRC4_1mL=as.data.frame(vuse1waste[1])[,1]/as.data.frame(vuse1waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4_1mL, lciTRC4_1mL[length(patients):1]), border=NA, col="blue")
lines(patients, as.data.frame(vuse1waste[1])[,3]/as.data.frame(vuse1waste[3])[,3], type="l", lwd=2, col="dark blue")

uciID4_1mL=as.data.frame(vuse1waste[2])[,5]/as.data.frame(vuse1waste[3])[,3]
lciID4_1mL=as.data.frame(vuse1waste[2])[,1]/as.data.frame(vuse1waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4_1mL, lciID4_1mL[length(patients):1]), border=NA, col="red")
lines(patients, as.data.frame(vuse1waste[2])[,3]/as.data.frame(vuse1waste[3])[,3], type="l", col="dark red", lwd=2)

uci1wk_1mL=as.data.frame(vuse1waste[5])[,5]/as.data.frame(vuse1waste[3])[,3]
lci1wk_1mL=as.data.frame(vuse1waste[5])[,1]/as.data.frame(vuse1waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk_1mL, lci1wk_1mL[length(patients):1]), border=NA, col="light gray")
lines(patients, as.data.frame(vuse1waste[5])[,3]/as.data.frame(vuse1waste[3])[,3], type="l", col="gray", lwd=2)

uciID4mw_1mL=as.data.frame(vuse1waste[6])[,5]/as.data.frame(vuse1waste[3])[,3]
lciID4mw_1mL=as.data.frame(vuse1waste[6])[,1]/as.data.frame(vuse1waste[3])[,3]
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4mw_1mL, lciID4mw_1mL[length(patients):1]), border=NA, col=colors()[373])
lines(patients, as.data.frame(vuse1waste[6])[,3]/as.data.frame(vuse1waste[3])[,3], type="l", col="dark red", lwd=2, lty=2)


#lines(rep(50,11), seq(-0.1, 0.95, by=0.1))
text(250, 0.8, "High throughput")
#lines(rep(10,11), seq(-0.1, 0.95, by=0.1))
text(3, 0.05, "Low \n throughput")
#arrows(100,0.1,5,0.23, 0.05)

lines(c(-10:100,1000), rep(1.48/5,112), lty=2, col="gray")
lines(c(-10:100,1000), rep(1/5,112), lty=2, col="blue")
lines(c(-10:100,1000), rep(1/5,112), lty=2, col="red")
lines(c(-10:100,1000), rep(0.88/5,112), lty=3, col="red", lwd="1")
mtext("B", font=2, at=0.4, line=-1)
dev.off()

regimens$IM_i1*10






#Vaccine use - not going to include for now...
par(cex=0.65, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.8,0.2,0), tck=-0.01)
plot(patients, as.data.frame(vuse0.5[3])[,3], ylim=c(10,20000), type="l", lwd=2, log="xy", axes=FALSE, xlab="", ylab="")
polygon(c(patients, sort(patients, decreasing=T)), c(as.data.frame(vuse0.5[3])[,1], as.data.frame(vuse0.5[3])[length(patients):1,5]), border=NA, col="dark gray")
lines(patients, as.data.frame(vuse0.5[3])[,3], lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(as.data.frame(vuse0.5[1])[,1], as.data.frame(vuse0.5[1])[length(patients):1,5]), border=NA, col="blue")
lines(patients, as.data.frame(vuse0.5[1])[,3], col="dark blue", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(as.data.frame(vuse0.5[5])[,1], as.data.frame(vuse0.5[5])[length(patients):1,5]), border=NA, col="red")
lines(patients, as.data.frame(vuse0.5[5])[,3], col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(as.data.frame(vuse0.5[2])[,1], as.data.frame(vuse0.5[2])[length(patients):1,5]), border=NA, col="light gray")
lines(patients, as.data.frame(vuse0.5[2])[,3], col="dark gray", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)

#LOOK AT EFFECTIVENESS AND COST-EFFECTIVENESS GIVEN POOR COMPLIANCE
postscript(file="Fig2_lc.eps", width=8, height=4, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.1, 0.35, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA[1,], type="l", col="white", axes=FALSE, xlim=c(0,100), ylim=c(0,400),
  xlab="", ylab="Direct medical costs (USD) per rabies death averted")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[1,], costPRDAlci[1,length(patients):1]), border=NA, col="blue")
lines(patients, costPRDA[1,], lwd=2, col="dark blue")
text(50, 0.3, "Updated TRC"); mtext("A", font=2, at=-20, line=-2)

eff=c(0, 0.25, 0.5, 0.75, 0.9)
pcDA=rep(NA, length(regimens))
for (j in 2:length(eff)){
	DA=(prabies*patients*12) #deaths averted
	for (i in 1:length(regimens)){
		lc=0.5
		DAv1=DA*lc*eff[j] #the deaths averted by the half of patients that get only 1 dose
		DAv2=DA*lc*lc*(eff[j]+eff[j]/10) #Half again (1/4) are medium compliance (2 doses) but very good protection
		DAv3=DA*lc*lc*lc*(eff[j]+eff[j]/10+eff[j]/100) #Half again (1/8) get vvv.good protection (3 doses)
		DAv4=prabies*((patients*12)-(patients*12*lc*lc*lc)-(patients*12*lc*lc)-(patients*12*lc))#The rest get complete protection
		if(n.visit(unlist(regimens[i]))==4){lcDA=DAv1+DAv2+DAv3+DAv4}
		if(n.visit(unlist(regimens[i]))==3){lcDA=DAv1+DAv2+(DAv4*2)}
		pcDA[i]=round(lcDA/DA, 2)[1]*100
		lccostPRDA[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))/lcDA
		lccostPRDAuci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,1], unlist(regimens[i]))/lcDA
		lccostPRDAlci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,5], unlist(regimens[i]))/lcDA
		}
	polygon(c(patients, sort(patients, decreasing=T)), c(lccostPRDAuci[1,], lccostPRDAlci[1,length(patients):1]), border=NA, col="light blue")
	lines(patients, lccostPRDA[1,], lty=1, lwd=1, col="dark blue")
	text(paste("VE=",100*eff[j],"%, DA=", pcDA[1],"%"), y=lccostPRDA[1,length(patients)]+10, x=80, cex=0.75)
}

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.38, 0.63, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA[1,], type="l", col="white", axes=FALSE, xlim=c(0,100), ylim=c(0,400), xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[2,], costPRDAlci[2,length(patients):1]), border=NA, col="red")
lines(patients, costPRDA[2,], lwd=2, col="dark red")
text(50, 0.3, "4-site"); mtext("A", font=2, at=-20, line=-2)

for (j in 2:length(eff)){
	DA=(prabies*patients*12) #deaths averted
	for (i in 1:length(regimens)){
		lc=0.5
		DAv1=DA*lc*eff[j] #the deaths averted by the half of patients that get only 1 dose
		DAv2=DA*lc*lc*(eff[j]+eff[j]/10) #Half again (1/4) are medium compliance (2 doses) but very good protection
		DAv3=DA*lc*lc*lc*(eff[j]+eff[j]/10+eff[j]/100) #Half again (1/8) get vvv.good protection (3 doses)
		DAv4=prabies*((patients*12)-(patients*12*lc*lc*lc)-(patients*12*lc*lc)-(patients*12*lc))#The rest get complete protection
		if(n.visit(unlist(regimens[i]))==4){lcDA=DAv1+DAv2+DAv3+DAv4}
		if(n.visit(unlist(regimens[i]))==3){lcDA=DAv1+DAv2+(DAv4*2)}
		pcDA=round(lcDA/DA, 2)[1]*100

		lccostPRDA[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))/lcDA
		lccostPRDAuci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,1], unlist(regimens[i]))/lcDA
		lccostPRDAlci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,5], unlist(regimens[i]))/lcDA
		}
	polygon(c(patients, sort(patients, decreasing=T)), c(lccostPRDAuci[2,], lccostPRDAlci[2,length(patients):1]), border=NA, col=colors()[373])
	lines(patients, lccostPRDA[2,], lty=1, lwd=1, col="red")
	text(paste("VE=",100*eff[j],"%, DA=", pcDA[1],"%"), y=lccostPRDA[2,length(patients)]+10, x=80, cex=0.75)
}

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.66, 0.91, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costPRDA[1,], type="l", col="white", axes=FALSE, xlim=c(0,100), ylim=c(0,400), xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(costPRDAuci[5,], costPRDAlci[5,length(patients):1]), border=NA, col="gray")
lines(patients, costPRDA[5,], lwd=2, col="dark gray")
text(50, 0.3, "1-week"); mtext("A", font=2, at=-20, line=-2))

for (j in 2:length(eff)){
	DA=(prabies*patients*12) #deaths averted
	for (i in 1:length(regimens)){
		lc=0.5
		DAv1=DA*lc*eff[j] #the deaths averted by the half of patients that get only 1 dose
		DAv2=DA*lc*lc*(eff[j]+eff[j]/10) #Half again (1/4) are medium compliance (2 doses) but very good protection
		DAv3=DA*lc*lc*lc*(eff[j]+eff[j]/10+eff[j]/100) #Half again (1/8) get vvv.good protection (3 doses)
		DAv4=prabies*((patients*12)-(patients*12*lc*lc*lc)-(patients*12*lc*lc)-(patients*12*lc))#The rest get complete protection
		if(n.visit(unlist(regimens[i]))==4){lcDA=DAv1+DAv2+DAv3+DAv4}
		if(n.visit(unlist(regimens[i]))==3){lcDA=DAv1+DAv2+(DAv4*2)}
		pcDA[i]=round(lcDA/DA, 2)[1]*100

		lccostPRDA[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))/lcDA
		lccostPRDAuci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,1], unlist(regimens[i]))/lcDA
		lccostPRDAlci[i,]=costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,5], unlist(regimens[i]))/lcDA
		}
	polygon(c(patients, sort(patients, decreasing=T)), c(lccostPRDAuci[5,], lccostPRDAlci[5,length(patients):1]), border=NA, col="light gray")
	lines(patients, lccostPRDA[5,], lty=1, lwd=1, col="grey")
	text(paste("VE=",100*eff[j],"%, DA=", pcDA[5],"%"), y=lccostPRDA[5,length(patients)]+10, x=80, cex=0.75)
}
dev.off()


#Plot cost-effectiveness versus addative vaccine efficacy (for clinics with 100 patients/month)
#Plot the cost to healthcare providers per rabies death averted and compare vial use (but use previous stats because pics are not great)
eff=seq(0,1,0.01)
CE=pcDA=matrix(nrow=length(regimens), ncol=length(eff))

cDA=function(compliance, ve, visits, thruput){
	DA=prabies*thruput*12
	pv4=compliance^3*thruput*12
	pv3=(compliance^2*thruput*12)-pv4
	pv2=(compliance*thruput*12)-pv3-pv4
	pv1=(compliance*thruput*12)-pv2-pv3-pv4
	DA4l=prabies*pv4
	DA3l=prabies*(pv4+pv3)
	DA3=prabies*pv3*(1-(1-ve)^3)
	DA2=prabies*pv2*(1-(1-ve)^2)
	DA1=prabies*pv2*(1-(1-ve))
	if(visits==4){lcDA=(DA1+DA2+DA3+DA4l)}
	if(visits==3){lcDA=(DA1+DA2+DA3l)}
	lcDA
	}



postscript(file="compliance.eps", width=8, height=4.5, horizontal=F)

lc=0.5
for (i in 1:length(regimens)){
	for (j in 1:length(eff)){
		DA=(prabies*patients*12) #deaths averted
		lcDA=cDA(lc, eff[j], n.visit(unlist(regimens[i])), patients)[42]
		pcDA[i,j]=lcDA/DA[42]*100
		CE[i,j]=(costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))/lcDA)[42]
		}
	}
par(cex=0.6, lwd=0.5, plt=c(0.1, 0.45, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(eff*100, CE[1,], col="dark blue", type="l", axes=FALSE, xlim=c(0,100), ylim=c(0,400),
	xlab="Efficacy of incomplete PEP vaccination",
	ylab="Cost-effectiveness of PEP per rabies case averted", main="compliance=0.5", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)
#lines(eff*100, rep(costPRDA[1, 42], length(eff)), lwd=1, col="dark blue")
lines(eff*100, CE[2,], col="dark red", lwd=2) #lines(eff*100, rep(costPRDA[2, 42], length(eff)), lwd=1, col="dark red")
lines(eff*100, CE[3,], col="black", lwd=2) #lines(eff*100, rep(costPRDA[3, 42], length(eff)), lwd=1, col="black")
lines(eff*100, CE[4,], col="black", lty=2, lwd=2) #lines(eff*100, rep(costPRDA[4, 42], length(eff)), lwd=1, col="black", lty=2)
lines(eff*100, CE[5,], col="gray", lwd=2) #lines(eff*100, rep(costPRDA[5, 42], length(eff)), lwd=1, col="gray")

par(new=TRUE); plot(eff*100, pcDA[1,], axes=F, xlab="", ylab="", type="l", lty=1, ylim=c(0,100), col="blue")
axis(side=4, cex=0.6, lwd=0.5); mtext(side=4, line=1.2, "Percentage of rabies deaths averted", cex=0.6)
lines(eff*100, pcDA[2,], col="red")
text(20, 20, "4 visit \n regimens"); text(7, 40, "3 visit \n regimens")

lc=0.75
for (i in 1:length(regimens)){
	for (j in 1:length(eff)){
		DA=(prabies*patients*12) #deaths averted
		lcDA=cDA(lc, eff[j], n.visit(unlist(regimens[i])), patients)[42]
		pcDA[i,j]=lcDA/DA[42]*100
		CE[i,j]=(costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))/lcDA)[42]
		}
	}

par(new=TRUE, plt=c(0.55, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(eff*100, CE[1,], col="dark blue", type="l", axes=FALSE, xlim=c(0,100), ylim=c(0,400), lwd=2,
	xlab="Efficacy of incomplete PEP vaccination", ylab="Cost-effectiveness of PEP per rabies case averted")
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(eff*100, CE[2,], col="dark red", lwd=2)
lines(eff*100, CE[3,], col="black", lwd=2)
lines(eff*100, CE[4,], col="black", lty=2, lwd=2)
lines(eff*100, CE[5,], col="gray", lwd=2)

par(new=T);
plot(eff*100, pcDA[1,], axes=F, xlab="", ylab="", type="l", lty=1, ylim=c(0,100), col="blue", main="compliance=0.75")
axis(side=4, cex=0.6, lwd=0.5); mtext(side=4, line=1.2, "Percentage of rabies deaths averted", cex=0.6)
lines(eff*100, pcDA[2,], col="red")
dev.off()


#Plot vaccination efficacy versus cost effectiveness for different levels of compliance
postscript(file="complianceHPs.eps", width=8, height=8, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.1, 0.45, 0.55, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(eff*100, eff, col="white", type="l", axes=FALSE, xlim=c(0,100), ylim=c(0,400),
	xlab="Efficacy of incomplete PEP vaccination",
	ylab="Cost-effectiveness of PEP per rabies case averted", lwd=2)
axis(2, lwd=0.5); axis(1, lwd=0.5)
mtext("A", font=2, at=-15, line=-1)

compliance=c(0.5, 1)
for (k in 1:length(compliance)){
	for (i in 1:length(regimens)){
		for (j in 1:length(eff)){
			lc=compliance[k]
			DA=(prabies*patients*12)[42] #deaths averted
			lcDA=cDA(lc, eff[j], n.visit(unlist(regimens[i])), patients[42])
			pcDA[i,j]=lcDA/DA*100
			CE[i,j]=(costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))[42]/lcDA)
			}
		}
		lines(eff*100, CE[1,], col="dark blue", lwd=2, lty=k)
		lines(eff*100, CE[2,], col="dark red", lwd=2, lty=k)
		lines(eff*100, CE[3,], col="black", lwd=2, lty=k)
		lines(eff*100, CE[4,], col="black", lwd=1, lty=k)
		lines(eff*100, CE[5,], col="gray", lwd=2, lty=k)
	}
text(c(20,80), c(50,300), c("100% compliance", "50% compliance"))

par(new=TRUE, plt=c(0.55, 0.9, 0.55, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(eff*100, CE[1,], col="white", type="l", axes=FALSE, xlim=c(0,100), ylim=c(0,100), lwd=2,
	xlab="Efficacy of incomplete PEP vaccination", ylab="Percentage of rabies deaths averted")
	axis(2, lwd=0.5); axis(1, lwd=0.5)
mtext("B", font=2, at=-15, line=-1)

compliance=c(0.5, 0.7, 1)
for (k in 1:length(compliance)){
	for (i in 1:length(regimens)){
		for (j in 1:length(eff)){
			lc=compliance[k]
			DA=(prabies*patients*12)[42] #deaths averted
			lcDA=cDA(lc, eff[j], n.visit(unlist(regimens[i])), patients[42])
			pcDA[i,j]=lcDA/DA*100
			CE[i,j]=(costs_lc(overhead, material, vial.cost, patients, as.data.frame(vuse0.5lc[i])[,3], unlist(regimens[i]))[42]/lcDA)
			}
		}
		lines(eff*100, pcDA[1,], col="black", lwd=2, lty=k)
		lines(eff*100, pcDA[2,], col="black", lwd=1, lty=k)
	}
text(c(50,20,20), c(40,75, 95), c("50% compliance", "75% compliance", "100% compliance"))
dev.off()




#Plot cost-effectiveness versus % of deaths avoided
#Plot costs to patients versus risk of death assuming different vaccine efficacy and relationships between costs and compliance

#Now compare the costs of charging different amounts
#in terms of losses and profit to health provider and cost to patients who live near and far away
ltc=2.5 #low travel costs ie 25% of the vial cost
htc=15 #High travel costs ie 1.5x cost of vial

#compare charging $2.5 and $3 per vial
income2.5=patients*(sum(ID4)*0.25)*10 #No. injections per patient x $2.5/injection (vial costs $10) * the monthly number of patients
income3=patients*(sum(ID4)*0.3)*10
cost=(vID4[,3]/11)*10  #No. 0.5mL vials used per month (given the number of attending patients)
cost1=(vID4_1mL[,3]/11)*10 #And for 1mL vials
netID4_2.5=income2.5-cost; netID4_2.5_1mL=income2.5-cost1
netID4_3=income3-cost; netID4_3_1mL=income3-cost1

income2.5=patients*(sum(TRC4)*0.25)*10 #No. injections per patient x $2.5/injection (vial costs $10)
income3=patients*(sum(TRC4)*0.3)*10
cost=(vTRC4[,3]/11)*10  #No. vials used per month - given the monthly average of patients
cost1=(vTRC4_1mL[,3]/11)*10
netTRC4_2.5=income2.5-cost; netTRC4_2.5_1mL=income2.5-cost1
netTRC4_3=income3-cost; netTRC4_3_mL=income3-cost1

income2.5=patients*(sum(weekPEP)*0.25)*10 #No. injections per patient x $2.5/injection (vial costs $10)
income3=patients*(sum(weekPEP)*0.3)*10
cost=(vwkPEP[,3]/11)*10  #No. vials used per month - given the monthly average of patients
cost1=(vwkPEP_1mL[,3]/11)*10
netwkPEP_2.5=income2.5-cost; netwkPEP_2.5_1mL=income2.5-cost1
netwkPEP_3=income3-cost; netwkPEP_3_1mL=income3-cost1


#compare charging $15 for first visit (and all others)
income=patients*15 #all patients pays 1 (cost of 1 vial ~$10)
cost=(vTRC4[,3]/11)*10  #No. vials used per patient
cost1=(vTRC4_1mL[,3]/11)*10
netTRC4_15=income-cost; netTRC4_15_1mL=income-cost1

cost=(vID4[,3]/11)*10  #No. vials used per patient
cost1=(vID4_1mL[,3]/11)*10
netID4_15=income-cost; netID4_15_1mL=income-cost1

cost=(vwkPEP[,3]/11)*10  #No. vials used per patient
cost1=(vwkPEP_1mL[,3]/11)*10
netwkPEP_15=income-cost; netwkPEP_15_1mL=income-cost1



#compare charging full $10 for first and second visit only
income=patients*20 #all patients pays 1.5 ($7.5 on first visit and on second)
cost=(vTRC4[,3]/11)*10 #No. vials used per patient
cost1=(vTRC4_1mL[,3]/11)*10
netTRC4_20=income-cost; netTRC4_20_1mL=income-cost1

cost=(vID4[,3]/11)*10  #No. vials used per patient
cost1=(vID4_1mL[,3]/11)*10
netID4_20=income-cost; netID4_20_1mL=income-cost1

cost=(vwkPEP[,3]/11)*10  #No. vials used per patient
cost1=(vwkPEP_1mL[,3]/11)*10
netwkPEP_20=income-cost; netwkPEP_20_1mL=income-cost1










#FIGURE 2  - 1 mL vials
postscript(file="Fig2_compliance1ml.eps", width=8, height=4, horizontal=F)

par(cex=0.7, lwd=0.5, plt=c(0.1, 0.35, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4_1ml[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, xlim=c(0,100),
  xlab="", ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4_1ml, lciTRC4_1ml[length(patients):1]), border=NA, col="blue")
lines(patients, propTRC4_1ml[,3], type="l", lwd=2, col="dark blue")
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4pc50_1ml, lciTRC4pc50_1ml[length(patients):1]),
	border=NA, col="light blue")
lines(patients, propTRC4pc50_1ml[,3], lwd=2, col="dark blue", lty=2)
text(50, 0.2, "Updated TRC")
mtext("A", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.38, 0.63, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propID4_1ml[,3], type="l", ylim=c(0,1), col="dark red", axes=FALSE, xlim=c(0,100),
  xlab="Average number of new patients per month", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4_1ml, lciID4_1ml[length(patients):1]), border=NA, col="red")
lines(patients, propID4_1ml[,3], type="l", col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4pc50_1ml, lciID4pc50_1ml[length(patients):1]),
	border=NA, col=colors()[373])
lines(patients, propID4pc50_1ml[,3], lty=2, col="dark red", lwd=2)
text(50, 0.2, "4-site")
mtext("B", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.66, 0.91, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, prop1wk_1ml[,3], type="l", ylim=c(0,1),, xlim=c(0,100), col="dark gray", axes=FALSE, xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk_1ml, lci1wk_1ml[length(patients):1]), border=NA, col="gray")
lines(patients, prop1wk_1ml[,3], type="l", col="dark gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wkpc50_1ml, lci1wkpc50_1ml[length(patients):1]),
	border=NA, col="light gray")
lines(patients, prop1wkpc50_1ml[,3], lty=2, col="dark gray", lwd=2)
text(50, 0.2, "1-week")
mtext("C", font=2, at=-20, line=-2)
dev.off()

#legend(20, 0.29, legend=c("TRC 100% compliance, 99% CIs", "ID 4-site 100% compliance, 99% CIs"), bty="n")
#polygon(c(15,22, 22, 15), c(0.245,0.245,0.275, 0.275), col="blue", border=NA)
#polygon(c(15,22, 22, 15), c(0.205,0.205,0.235, 0.235), col="red", border=NA)
#lines(15:22, rep(0.26, 8), col="dark blue", lwd=2)
#lines(15:22, rep(0.22,8), col="d




#The Essen 4 dose should use the most used vaccine therefore compare everything to that

#0.5mL vials, 100% compliance, no waste - 1
propTRC4=vTRC4pc100/vEssen4pc100 #proportional cost of ID cf IM
lciTRC4=vTRC4pc100[,1]/vEssen4pc100[,3] #CIs - presented against average essen use
uciTRC4=vTRC4pc100[,5]/vEssen4pc100[,3]

propID4=vID4pc100/vEssen4pc100
lciID4=vID4pc100[,1]/vEssen4pc100[,3]; uciID4=vID4pc100[,5]/vEssen4pc100[,3]

prop1wk=vwkPEPpc100/vEssen4pc100
lci1wk=vwkPEPpc100[,1]/vEssen4pc100[,3]; uci1wk=vwkPEPpc100[,5]/vEssen4pc100[,3]

propZagreb=vZagrebpc100/vEssen4pc100
lciZagreb=vZagrebpc100[,1]/vEssen4pc100[,3]; uciZagreb=vZagrebpc100[,5]/vEssen4pc100[,3]


#0.5mL vials, 50% compliance, no waste - 2
propTRC4pc50=vTRC4/vEssen4 #proportional cost of ID cf IM
lciTRC4pc50=vTRC4[,1]/vEssen4[,3] #CIs - presented against average essen use
uciTRC4pc50=vTRC4[,5]/vEssen4[,3]

propID4pc50=vID4/vEssen4
lciID4pc50=vID4[,1]/vEssen4[,3]; uciID4pc50=vID4[,5]/vEssen4[,3]

prop1wkpc50=vwkPEP/vEssen4
lci1wkpc50=vwkPEP[,1]/vEssen4[,3]; uci1wkpc50=vwkPEP[,5]/vEssen4[,3]


#0.5mL vials, 75% compliance, no waste - 3
propTRC4pc75=vTRC4pc75/vEssen4pc75 #proportional cost of ID cf IM
lciTRC4pc75=vTRC4pc75[,1]/vEssen4pc75[,3] #CIs - presented against average essen use
uciTRC4pc75=vTRC4pc75[,5]/vEssen4pc75[,3]

propID4pc75=vID4pc75/vEssen4pc75
lciID4pc75=vID4pc75[,1]/vEssen4pc75[,3]; uciID4pc75=vID4pc75[,5]/vEssen4pc75[,3]

prop1wkpc75=vwkPEPpc75/vEssen4pc75
lci1wkpc75=vwkPEPpc75[,1]/vEssen4pc75[,3]; uci1wkpc75=vwkPEPpc75[,5]/vEssen4pc75[,3]



#1mL vials, 100% compliance, no waste - 4
propTRC4_1ml=vTRC4pc100_1mL/vEssen4pc100 #proportional cost of ID cf IM
lciTRC4_1ml=vTRC4pc100_1mL[,1]/vEssen4pc100[,3] #CIs - presented against average essen use
uciTRC4_1ml=vTRC4pc100_1mL[,5]/vEssen4pc100[,3]

propID4_1ml=vID4pc100_1mL/vEssen4pc100
lciID4_1ml=vID4pc100_1mL[,1]/vEssen4pc100[,3]; uciID4_1ml=vID4pc100_1mL[,5]/vEssen4pc100[,3]

prop1wk_1ml=vwkPEPpc100_1mL/vEssen4pc100
lci1wk_1ml=vwkPEPpc100_1mL[,1]/vEssen4pc100[,3]; uci1wk_1ml=vwkPEPpc100_1mL[,5]/vEssen4pc100[,3]


#1mL vials, 50% compliance, no waste - 5
propTRC4pc50_1ml=vTRC4_1mL/vEssen4 #proportional cost of ID cf IM
lciTRC4pc50_1ml=vTRC4_1mL[,1]/vEssen4[,3] #CIs - presented against average essen use
uciTRC4pc50_1ml=vTRC4_1mL[,5]/vEssen4[,3]

propID4pc50_1ml=vID4_1mL/vEssen4
lciID4pc50_1ml=vID4_1mL[,1]/vEssen4[,3]; uciID4pc50_1ml=vID4_1mL[,5]/vEssen4[,3]

prop1wkpc50_1ml=vwkPEP_1mL/vEssen4
lci1wkpc50_1ml=vwkPEP_1mL[,1]/vEssen4[,3]; uci1wkpc50_1ml=vwkPEP_1mL[,5]/vEssen4[,3]


#1mL vials, 75% compliance, no waste - 6
propTRC4pc75_1ml=vTRC4pc75_1mL/vEssen4pc75 #proportional cost of ID cf IM
lciTRC4pc75_1ml=vTRC4pc75_1mL[,1]/vEssen4pc75[,3] #CIs - presented against average essen use
uciTRC4pc75_1ml=vTRC4pc75_1mL[,5]/vEssen4pc75[,3]

propID4pc75_1ml=vID4pc75_1mL/vEssen4pc75
lciID4pc75_1ml=vID4pc75_1mL[,1]/vEssen4pc75[,3]; uciID4pc75_1ml=vID4pc75_1mL[,5]/vEssen4pc75[,3]

prop1wkpc75_1ml=vwkPEPpc75_1mL/vEssen4pc75
lci1wkpc75_1ml=vwkPEPpc75_1mL[,1]/vEssen4pc75[,3]; uci1wkpc75_1ml=vwkPEPpc75_1mL[,5]/vEssen4pc75[,3]



#0.5mL vials, 100% compliance, waste - 7
propTRC4waste=vTRC4pc100waste/vEssen4pc100 #proportional cost of ID cf IM
lciTRC4waste=vTRC4pc100waste[,1]/vEssen4pc100[,3] #CIs - presented against average essen use
uciTRC4waste=vTRC4pc100waste[,5]/vEssen4pc100[,3]

propID4waste=vID4pc100waste/vEssen4pc100
lciID4waste=vID4pc100waste[,1]/vEssen4pc100[,3]; uciID4waste=vID4pc100waste[,5]/vEssen4pc100[,3]

prop1wkwaste=vwkPEPpc100waste/vEssen4pc100
lci1wkwaste=vwkPEPpc100waste[,1]/vEssen4pc100[,3]; uci1wkwaste=vwkPEPpc100waste[,5]/vEssen4pc100[,3]


#0.5mL vials, 50% compliance, waste - 8
propTRC4pc50waste=vTRC4waste/vEssen4 #proportional cost of ID cf IM
lciTRC4pc50waste=vTRC4waste[,1]/vEssen4[,3] #CIs - presented against average essen use
uciTRC4pc50waste=vTRC4waste[,5]/vEssen4[,3]

propID4pc50waste=vID4waste/vEssen4
lciID4pc50waste=vID4waste[,1]/vEssen4[,3]; uciID4pc50waste=vID4waste[,5]/vEssen4[,3]

prop1wkpc50waste=vwkPEPwaste/vEssen4
lci1wkpc50waste=vwkPEPwaste[,1]/vEssen4[,3]; uci1wkpc50waste=vwkPEPwaste[,5]/vEssen4[,3]


#0.5mL vials, 75% compliance, waste - 9
propTRC4pc75waste=vTRC4pc75waste/vEssen4pc75 #proportional cost of ID cf IM
lciTRC4pc75waste=vTRC4pc75waste[,1]/vEssen4pc75[,3] #CIs - presented against average essen use
uciTRC4pc75waste=vTRC4pc75waste[,5]/vEssen4pc75[,3]

propID4pc75waste=vID4pc75waste/vEssen4pc75
lciID4pc75waste=vID4pc75waste[,1]/vEssen4pc75[,3]; uciID4pc75waste=vID4pc75waste[,5]/vEssen4pc75[,3]

prop1wkpc75waste=vwkPEPpc75waste/vEssen4pc75
lci1wkpc75waste=vwkPEPpc75waste[,1]/vEssen4pc75[,3]; uci1wkpc75waste=vwkPEPpc75waste[,5]/vEssen4pc75[,3]


#1mL vials, 100% compliance, waste - 10
propTRC4waste1ml=vTRC4pc100waste1mL/vEssen4pc100 #proportional cost of ID cf IM
lciTRC4waste1ml=vTRC4pc100waste1mL[,1]/vEssen4pc100[,3] #CIs - presented against average essen use
uciTRC4waste1ml=vTRC4pc100waste1mL[,5]/vEssen4pc100[,3]

propID4waste1ml=vID4pc100waste1mL/vEssen4pc100
lciID4waste1ml=vID4pc100waste1mL[,1]/vEssen4pc100[,3]
uciID4waste1ml=vID4pc100waste1mL[,5]/vEssen4pc100[,3]

prop1wkwaste1ml=vwkPEPpc100waste1mL/vEssen4pc100
lci1wkwaste1ml=vwkPEPpc100waste1mL[,1]/vEssen4pc100[,3];
uci1wkwaste1ml=vwkPEPpc100waste1mL[,5]/vEssen4pc100[,3]


#1mL vials, 50% compliance, waste - 11
propTRC4pc50waste1ml=vTRC4waste1mL/vEssen4 #proportional cost of ID cf IM
lciTRC4pc50waste1ml=vTRC4waste1mL[,1]/vEssen4[,3] #CIs - presented against average essen use
uciTRC4pc50waste1ml=vTRC4waste1mL[,5]/vEssen4[,3]

propID4pc50waste1ml=vID4waste1mL/vEssen4
lciID4pc50waste1ml=vID4waste1mL[,1]/vEssen4[,3]; uciID4pc50waste1ml=vID4waste1mL[,5]/vEssen4[,3]

prop1wkpc50waste1ml=vwkPEPwaste1mL/vEssen4
lci1wkpc50waste1ml=vwkPEPwaste1mL[,1]/vEssen4[,3]; uci1wkpc50waste1ml=vwkPEPwaste1mL[,5]/vEssen4[,3]


#1mL vials, 75% compliance, waste - 12
propTRC4pc75waste1ml=vTRC4waste1mLpc75/vEssen4pc75 #proportional cost of ID cf IM
lciTRC4pc75waste1ml=vTRC4waste1mLpc75[,1]/vEssen4pc75[,3] #CIs - presented against average essen use
uciTRC4pc75waste1ml=vTRC4waste1mLpc75[,5]/vEssen4pc75[,3]

propID4pc75waste1ml=vID4waste1mLpc75/vEssen4pc75
lciID4pc75waste1ml=vID4waste1mLpc75[,1]/vEssen4pc75[,3];
uciID4pc75waste1ml=vID4waste1mLpc75[,5]/vEssen4pc75[,3]

prop1wkpc75waste1ml=vwkPEPwaste1mLpc75/vEssen4pc75
lci1wkpc75waste1ml=vwkPEPwaste1mLpc75[,1]/vEssen4pc75[,3];
uci1wkpc75waste1ml=vwkPEPwaste1mLpc75[,5]/vEssen4pc75[,3]








#Plot these comparisons
postscript(file="vial_use.eps", width=8, height=4, horizontal=F)

par(cex=0.6, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4[,3], type="l", ylim=c(0,1), col="dark green", axes=FALSE,
  xlab="Average number of patients per month",
  ylab="Proportion of vials used in comparison to the 4-dose reduced Essen IM schedule")
axis(2, lwd=0.5); axis(1, lwd=0.5)

polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4, lciTRC4[length(patients):1]), border=NA, col="green")
lines(patients, propTRC4[,3], type="l", lwd=2, col="dark green")

polygon(c(patients, sort(patients, decreasing=T)), c(uciID4, lciID4[length(patients):1]), border=NA, col="blue")
lines(patients, propID4[,3], type="l", col="dark blue", lwd=2)
#Note Zagreb is exactly comparable!

polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4pc50, lciTRC4pc50[length(patients):1]), border=NA, col="light green")
lines(patients, propTRC4pc50[,3], type="l", lwd=1, col="dark green")

polygon(c(patients, sort(patients, decreasing=T)), c(uciID4pc50, lciID4pc50[length(patients):1]), border=NA, col="light blue")
lines(patients, propID4pc50[,3], type="l", col="dark blue", lwd=1)


legend(20, 0.4, legend=c("TRC 100% compliance, 99% CIs", "ID 4-site 100% compliance, 99% CIs",
                          "TRC 50% compliance, 99% CIs", "ID 4-site 50% compliance, 99% CIs"), bty="n")
polygon(c(15,22, 22, 15), c(0.345,0.345,0.375, 0.375), col="green", border=NA)
polygon(c(15,22, 22, 15), c(0.305,0.305,0.335, 0.335), col="blue", border=NA)
polygon(c(15,22, 22, 15), c(0.265,0.265,0.295, 0.295), col="light green", border=NA)
polygon(c(15,22, 22, 15), c(0.225,0.225,0.255, 0.255), col="light blue", border=NA)

lines(15:22, rep(0.36, 8), col="dark green", lwd=2)
lines(15:22, rep(0.32,8), col="dark blue", lwd=2)
lines(15:22, rep(0.28, 8), col="dark green", lwd=1)
lines(15:22, rep(0.24,8), col="dark blue", lwd=1)

dev.off()



#Figure showing use up to really high throughput clincs (assume some wastage - and compare to both 0.5 and 1mL vials but 100% compliance)
postscript(file="Throughput_range_with_waste.eps", width=6.5, height=4, horizontal=F)

par(cex=0.55, lwd=0.5, plt=c(0.1, 0.7, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4waste[,3], type="l", ylim=c(0,1), xlim=c(0,1000), col="dark blue", axes=FALSE,
  xlab="Average number of new patients per month",
  ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)

polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4waste, lciTRC4waste[length(patients):1]), border=NA, col="blue")
lines(patients, propTRC4waste[,3], type="l", lwd=2, col="dark blue")
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4waste, lciID4waste[length(patients):1]), border=NA, col="red")
lines(patients, propID4waste[,3], type="l", col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wkwaste, lci1wkwaste[length(patients):1]), border=NA, col="light gray")
lines(patients, prop1wkwaste[,3], type="l", col="gray", lwd=2)
lines(rep(50,12), seq(-0.1, 1, by=0.1)); lines(rep(10,12), seq(-0.1, 1, by=0.1))
lines(c(-10:100,1000), rep(0.351,112), lty=2, col="red")
lines(c(-10:100,1000), rep(0.41,112), lty=2, col="blue")
lines(c(-10:100,1000), rep(0.6,112), lty=2, col="gray")

par(cex=0.55, lwd=0.5, plt=c(0.1, 0.7, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4waste1ml[,3], type="l", ylim=c(0,1), xlim=c(0,1000), col="dark blue", axes=FALSE,
  xlab="Average number of new patients per month",
  ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)

polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4waste1ml, lciTRC4waste1ml[length(patients):1]), border=NA, col="blue")
lines(patients, propTRC4waste1ml[,3], type="l", lwd=2, col="dark blue")
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4waste1ml, lciID4waste1ml[length(patients):1]), border=NA, col="red")
lines(patients, propID4waste1ml[,3], type="l", col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wkwaste1ml, lci1wkwaste1ml[length(patients):1]), border=NA, col="light gray")
lines(patients, prop1wkwaste1ml[,3], type="l", col="gray", lwd=2)

lines(rep(50,12), seq(-0.1, 1, by=0.1)); lines(rep(10,12), seq(-0.1, 1, by=0.1))
lines(c(-10:100,1000), rep(0.3,112), lty=2, col="gray")
lines(c(-10:100,1000), rep(0.21,112), lty=2, col="blue")
lines(c(-10:100,1000), rep(0.18,112), lty=2, col="red")

dev.off()





#FIGURE 2
postscript(file="Fig2_compliance.eps", width=8, height=4, horizontal=F)

par(cex=0.7, lwd=0.5, plt=c(0.1, 0.35, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, xlim=c(0,100),
  xlab="", ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4, lciTRC4[length(patients):1]), border=NA, col="blue")
lines(patients, propTRC4[,3], type="l", lwd=2, col="dark blue")
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4pc50, lciTRC4pc50[length(patients):1]),
	border=NA, col="light blue")
lines(patients, propTRC4pc50[,3], lwd=2, col="dark blue", lty=2)
text(50, 0.3, "Updated TRC")
mtext("A", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.38, 0.63, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propID4[,3], type="l", ylim=c(0,1), col="dark red", axes=FALSE, xlim=c(0,100),
  xlab="Average number of new patients per month", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4, lciID4[length(patients):1]), border=NA, col="red")
lines(patients, propID4[,3], type="l", col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4pc50, lciID4pc50[length(patients):1]),
	border=NA, col=colors()[373])
lines(patients, propID4pc50[,3], lty=2, col="dark red", lwd=2)
text(50, 0.3, "4-site")
mtext("B", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.66, 0.91, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, prop1wk[,3], type="l", ylim=c(0,1),, xlim=c(0,100), col="dark gray", axes=FALSE, xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk, lci1wk[length(patients):1]), border=NA, col="gray")
lines(patients, prop1wk[,3], type="l", col="dark gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wkpc50, lci1wkpc50[length(patients):1]),
	border=NA, col="light gray")
lines(patients, prop1wkpc50[,3], lty=2, col="dark gray", lwd=2)
text(50, 0.3, "1-week")
mtext("C", font=2, at=-20, line=-2)
dev.off()

#FIGURE 2  - 1 mL vials
postscript(file="Fig2_compliance1ml.eps", width=8, height=4, horizontal=F)

par(cex=0.7, lwd=0.5, plt=c(0.1, 0.35, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propTRC4_1ml[,3], type="l", ylim=c(0,1), col="dark blue", axes=FALSE, xlim=c(0,100),
  xlab="", ylab="Proportion of vials used for ID schedules relative to IM")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4_1ml, lciTRC4_1ml[length(patients):1]), border=NA, col="blue")
lines(patients, propTRC4_1ml[,3], type="l", lwd=2, col="dark blue")
polygon(c(patients, sort(patients, decreasing=T)), c(uciTRC4pc50_1ml, lciTRC4pc50_1ml[length(patients):1]),
	border=NA, col="light blue")
lines(patients, propTRC4pc50_1ml[,3], lwd=2, col="dark blue", lty=2)
text(50, 0.2, "Updated TRC")
mtext("A", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.38, 0.63, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, propID4_1ml[,3], type="l", ylim=c(0,1), col="dark red", axes=FALSE, xlim=c(0,100),
  xlab="Average number of new patients per month", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4_1ml, lciID4_1ml[length(patients):1]), border=NA, col="red")
lines(patients, propID4_1ml[,3], type="l", col="dark red", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uciID4pc50_1ml, lciID4pc50_1ml[length(patients):1]),
	border=NA, col=colors()[373])
lines(patients, propID4pc50_1ml[,3], lty=2, col="dark red", lwd=2)
text(50, 0.2, "4-site")
mtext("B", font=2, at=-20, line=-2)

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.66, 0.91, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, prop1wk_1ml[,3], type="l", ylim=c(0,1),, xlim=c(0,100), col="dark gray", axes=FALSE, xlab="", ylab="")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wk_1ml, lci1wk_1ml[length(patients):1]), border=NA, col="gray")
lines(patients, prop1wk_1ml[,3], type="l", col="dark gray", lwd=2)
polygon(c(patients, sort(patients, decreasing=T)), c(uci1wkpc50_1ml, lci1wkpc50_1ml[length(patients):1]),
	border=NA, col="light gray")
lines(patients, prop1wkpc50_1ml[,3], lty=2, col="dark gray", lwd=2)
text(50, 0.2, "1-week")
mtext("C", font=2, at=-20, line=-2)
dev.off()

#legend(20, 0.29, legend=c("TRC 100% compliance, 99% CIs", "ID 4-site 100% compliance, 99% CIs"), bty="n")
#polygon(c(15,22, 22, 15), c(0.245,0.245,0.275, 0.275), col="blue", border=NA)
#polygon(c(15,22, 22, 15), c(0.205,0.205,0.235, 0.235), col="red", border=NA)
#lines(15:22, rep(0.26, 8), col="dark blue", lwd=2)
#lines(15:22, rep(0.22,8), col="dark red", lwd=2)







#Plot the comparisons
postscript(file="cost_comparison_sequence.eps", width=6, height=4.5, horizontal=F)
par(cex=0.65, lwd=0.5, plt=c(0.1, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
#text(85, 100, "$3 or $2.5 \n per injection")

#NOW INCLUDE 1mL VIALS
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")

lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
#text(85, 100, "$3 or $2.5 \n per injection")

#Focus on low thru-put areas
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")


#Look at alternative costings (full course)
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

#And two payments
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.1, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

#And full course from a 1mL vial
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

lines(patients, netTRC4_15_1mL, lwd=2, lty=2, col="dark blue")
lines(patients, netwkPEP_15_1mL, lwd=2, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=2, lty=2, col="red")


#LOOK AT LOW THRU-PUT CLINICS
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

lines(patients, netTRC4_15_1mL, lwd=2, lty=2, col="dark blue")
lines(patients, netwkPEP_15_1mL, lwd=2, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=2, lty=2, col="red")

dev.off()


postscript(file="cost_comparison_sequence.eps", width=6, height=4.5, horizontal=F)
par(cex=0.65, lwd=0.5, plt=c(0.1, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
#text(85, 100, "$3 or $2.5 \n per injection")

#NOW INCLUDE 1mL VIALS
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")

lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
#text(85, 100, "$3 or $2.5 \n per injection")

#Focus on low thru-put areas
plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

plot(patients, netID4_3, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5, lwd=1, col="red")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")


#Look at alternative costings (full course)
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

#And two payments
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.1, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

#And full course from a 1mL vial
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

lines(patients, netTRC4_15_1mL, lwd=2, lty=2, col="dark blue")
lines(patients, netwkPEP_15_1mL, lwd=2, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=2, lty=2, col="red")


#LOOK AT LOW THRU-PUT CLINICS
plot(patients, netID4_15, lwd=2, col="red", type="l", ylim=c(-100,600), xlim=c(0,100),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

lines(patients, netTRC4_15_1mL, lwd=2, lty=2, col="dark blue")
lines(patients, netwkPEP_15_1mL, lwd=2, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=2, lty=2, col="red")

dev.off()


postscript(file="Fig3cost_comparison.eps", width=12, height=3, horizontal=F)
par(cex=0.65, lwd=1, plt=c(0.04, 0.24, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs #$2.5/ injection
plot(patients, netID4_2.5, col="red", type="l", ylim=c(-100,15000),
  xlab="", ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
text(650, 800, "$2.5 per injection")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.055, 0.155, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_2.5, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$3/injection
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.27, 0.47, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_3, lwd=1, col="red", type="l", ylim=c(-100,15000),
  xlab="Average number of new patients reporting with animal bites per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_mL, lwd=1, lty=2, col="dark blue")
text(650, 800, "$3 per injection")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.2855, 0.3855, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_3, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$15 full course
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.5, 0.7, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
text(650, 800, "$15 full course")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.515, 0.615, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$20 full course
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.73, 0.93, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
text(650, 800, "$20 full course \n $10 for 1st and 2nd \n clinic visits")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.745, 0.845, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
dev.off()


postscript(file="Fig3_PEP.eps", width=6, height=6, horizontal=F)
par(cex=0.7, lwd=1, plt=c(0.05, 0.45, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs #$2.5/ injection
plot(patients, netID4_2.5, col="red", type="l", ylim=c(-100,15000),
  xlab="", ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
text(200, 12000, "$2.5 per injection")
mtext("A", font=2, at=-135, line=-1)
polygon(c(0,50,50,0), c(-100,-100, 600, 600), lwd=0.5)

#lower panel on low thru-put areas
par(new=TRUE, lwd=0.5, plt=c(0.05, 0.45, 0.1, 0.45), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_2.5, lwd=1, col="red", type="l", ylim=c(-100,600), xlim=c(0,50), xlab="",
ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
mtext("C", font=2, at=-6, line=-1)
mtext("Average number of new patients reporting with animal bites per month", line=1.5, side=1, at=45, cex=0.7)

#$15 full course
par(new=TRUE, plt=c(0.5, 0.9, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
text(200, 12000, "$15 full course")
mtext("B", font=2, at=-135, line=-1)
polygon(c(0,50,50,0), c(-100,-100, 600, 600))

#lower panel on low thru-put areas
par(new=TRUE, plt=c(0.5, 0.9, 0.1, 0.45), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
mtext("D", font=2, at=-6, line=-1)
dev.off()

postscript(file="FigS3_PEP.eps", width=6, height=6, horizontal=F)
par(cex=0.7, lwd=1, plt=c(0.05, 0.45, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs ##$3/injection
plot(patients, netID4_3, col="red", type="l", ylim=c(-100,15000),
  xlab="", ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_1mL, lwd=1, lty=2, col="dark blue")
text(200, 12000, "$3 per injection")
mtext("A", font=2, at=-135, line=-1)
polygon(c(0,50,50,0), c(-100,-100, 600, 600), lwd=0.5)

#lower panel on low thru-put areas
par(new=TRUE, lwd=0.5, plt=c(0.05, 0.45, 0.1, 0.45), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_3, lwd=1, col="red", type="l", ylim=c(-100,600), xlim=c(0,50), xlab="",
ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
mtext("C", font=2, at=-6, line=-1)
mtext("Average number of new patients reporting with animal bites per month", line=1.5, side=1, at=45, cex=0.7)

#$20 full course
par(new=TRUE, plt=c(0.5, 0.9, 0.5, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
text(200, 12000, "$20 full course \n $10 for 1st and 2nd \n clinic visits")
mtext("B", font=2, at=-135, line=-1)
polygon(c(0,50,50,0), c(-100,-100, 600, 600))

#lower panel on low thru-put areas
par(new=TRUE, plt=c(0.5, 0.9, 0.1, 0.45), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
mtext("D", font=2, at=-6, line=-1)
dev.off()





postscript(file="Fig3cost_comparison.eps", width=12, height=3, horizontal=F)
par(cex=0.65, lwd=1, plt=c(0.04, 0.24, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

#Look across the range of throughputs #$2.5/ injection
plot(patients, netID4_2.5, col="red", type="l", ylim=c(-100,15000),
  xlab="", ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
text(650, 800, "$2.5 per injection")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.055, 0.155, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_2.5, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, netID4_2.5_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_2.5_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_2.5_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$3/injection
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.27, 0.47, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_3, lwd=1, col="red", type="l", ylim=c(-100,15000),
  xlab="Average number of new patients reporting with animal bites per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_mL, lwd=1, lty=2, col="dark blue")
text(650, 800, "$3 per injection")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.2855, 0.3855, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_3, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=1, col="dark blue")
lines(patients, netwkPEP_3, lwd=1, col="dark gray")
lines(patients, netID4_3_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_3_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_3_mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$15 full course
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.5, 0.7, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
text(650, 800, "$15 full course")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.515, 0.615, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=1, col="dark blue")
lines(patients, netwkPEP_15, lwd=1, col="dark gray")
lines(patients, netID4_15_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_15_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_15_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))

#$20 full course
par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.73, 0.93, 0.15, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", ylim=c(-100,15000), xlim=c(0,1000),
  xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue") #NOW INCLUDE 1mL VIALS
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
text(650, 800, "$20 full course \n $10 for 1st and 2nd \n clinic visits")

#Inset on low thru-put areas
par(new=TRUE, cex=0.5, lwd=0.5, plt=c(0.745, 0.845, 0.65, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_20, lwd=1, col="red", type="l", , ylim=c(-100,600), xlim=c(0,50), xlab="", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, netID4_20_1mL, lwd=1, lty=2, col="red")
lines(patients, netwkPEP_20_1mL, lwd=1, lty=2, col="dark gray")
lines(patients, netTRC4_20_1mL, lwd=1, lty=2, col="dark blue")
lines(patients, rep(0, length(patients)))
dev.off()











postscript(file="Profit_comparison.eps", width=7, height=3.5, horizontal=F)
par(cex=0.65, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)

plot(patients, netID4_3, lwd=2, col="dark red", type="l", ylim=c(-100,600),
  xlab="Average number of new patients per month",
  ylab="Net monthly gains/losses (US$)", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_3, lwd=2, col="dark blue")
lines(patients, netTRC4_2.5, lwd=1, col="dark blue")
lines(patients, netID4_2.5, lwd=1, col="dark red")
lines(patients, netwkPEP_3, lwd=2, col="dark gray")
lines(patients, netwkPEP_2.5, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)

legend(70, 5, legend=c("TRC 2-site", "4-site ID", "1-week ID"), bty="n")
lines(65:72, rep(-25, 8), col="dark blue", lwd=2)
lines(65:72, rep(-60, 8), col="dark red", lwd=2)
lines(65:72, rep(-95, 8), col="dark gray", lwd=2)
text(85, 100, "$3 or $2.5 \n per injection")


par(new=TRUE, cex=0.65, lwd=0.5, plt=c(0.53, 0.96, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netID4_15, lwd=2, col="dark red", type="l", ylim=c(-100,600),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_15, lwd=2, col="dark blue")
lines(patients, netTRC4_20, lwd=1, col="dark blue")
lines(patients, netID4_20, lwd=1, col="dark red")
lines(patients, netwkPEP_15, lwd=2, col="dark gray")
lines(patients, netwkPEP_20, lwd=1, col="dark gray")
lines(patients, rep(0, length(patients)), lty=2)
text(25, 400, "$15 full course \n or \n $10 for each of the first 2 visits")

dev.off()


postscript(file="best_regimens.eps", width=6, height=4.5, horizontal=F)
par(cex=0.65, lwd=0.5, plt=c(0.1, 0.9, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, netTRC4_15_1mL, lwd=2, col="dark blue", type="l", ylim=c(-100,10000), xlim=c(0,1000),
  xlab="Average number of new patients per month", ylab="", axes=FALSE)
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, netTRC4_2.5_1mL, lwd=2, lty=2, col="dark blue")
axis(2, lwd=0.5); axis(1, lwd=0.5)
dev.off()


#Calculations for different countries
savings=function(ppm){
	c(netTRC4_2.5[ppm],
	netTRC4_2.5_1mL[ppm],
	netTRC4_3[ppm],
	netTRC4_3_mL[ppm],
	netTRC4_15[ppm],
	netTRC4_15_1mL[ppm]	,
	netTRC4_20[ppm],
	netTRC4_20_1mL[ppm])*12
	}

net_gains=rbind(tz_rural=savings(15),
tz_urban=savings(44)*2.4,
bali_rural=savings(20),
bali_urban=savings(44)*1.2,
chad=savings(30),
uganda_rural=savings(17),
manila=savings(44)*2.4,
pp=savings(45)*1.2,
india=savings(45)*4)

write.csv(net_gains, "savings.csv")


bali=c(600, 9000)
tz=netTRC4_15_1mL[15]*12

uganda=17
manila=600
cambodia=14475/12; netTRC4_15_1mL[45]*1.2*12 #Assuming 1,200 patients plus
india=netTRC4_15_1mL[45]*4*12 #4000


bali_urban=150*365; bali_urban/12 #300 reported patients/ day but could be some returning/ referred patients
bali_rural=7*8*35; bali_rural/12 #8 regencies, with 7 clinics per regency
bali=(netTRC4_15_1mL[35]*12*7*8)+(netTRC4_15_1mL[43]*12)

tanzania=25; netTRC4_15_1mL[25]*12*127


manila=7660/12; 7000*12
india=4000; netTRC4_15_1mL[45]*4*12


#how do these prices compare for the patient given low (0.25) and high travel costs (1.5)
regime=c("TDC4","ID4","1-week", "Essen4","Zagreb")
far.travelcosts=c(length(which(TRC4>0))*htc, length(which(ID4>0))*htc, length(which(weekPEP>0))*htc,
                  length(which(essen4>0))*htc, length(which(zagreb>0))*htc)
nr.travelcosts=c(length(which(TRC4>0))*ltc, length(which(ID4>0))*ltc, length(which(weekPEP>0))*ltc,
                  length(which(essen4>0))*ltc, length(which(zagreb>0))*ltc)

cbind(regime,far.travelcosts, nr.travelcosts)

nr.patient.cost=c((length(which(TRC4>0))*ltc)+(sum(TRC4)*0.3*10),
                (length(which(ID4>0))*ltc)+(sum(ID4)*0.3*10),
                (length(which(weekPEP>0))*ltc)+(sum(weekPEP)*0.3*10),
                (length(which(essen4>0))*ltc)+(sum(essen4)*10),
                (length(which(zagreb>0))*ltc)+(sum(zagreb)*10))

nr.patient.cost2.5=nr.patient.cost1=c((length(which(TRC4>0))*ltc)+(sum(TRC4)*0.25*10),
                (length(which(ID4>0))*ltc)+(sum(ID4)*0.25*10),
                (length(which(weekPEP>0))*ltc)+(sum(weekPEP)*0.25*10),
                (length(which(essen4>0))*ltc)+(sum(essen4)*10),
                (length(which(zagreb>0))*ltc)+(sum(zagreb)*10))

far.patient.cost=c((length(which(TRC4>0))*htc)+(sum(TRC4)*0.3*10),
                (length(which(ID4>0))*htc)+(sum(ID4)*0.3*10),
                (length(which(weekPEP>0))*htc)+(sum(weekPEP)*0.3*10),
                (length(which(essen4>0))*htc)+(sum(essen4)*10),
                (length(which(zagreb>0))*htc)+(sum(zagreb)*10))

far.patient.cost2.5=c((length(which(TRC4>0))*htc)+(sum(TRC4)*0.25*10),
                (length(which(ID4>0))*htc)+(sum(ID4)*0.25*10),
                (length(which(weekPEP>0))*htc)+(sum(weekPEP)*0.25*10),
                (length(which(essen4>0))*htc)+(sum(essen4)*10),
                (length(which(zagreb>0))*htc)+(sum(zagreb)*10))

nr.patient.cost20=c((length(which(TRC4>0))*ltc)+20,
                (length(which(ID4>0))*ltc)+20,
                (length(which(weekPEP>0))*ltc)+20,
                (length(which(essen4>0))*ltc)+(sum(essen4)*10),
                (length(which(zagreb>0))*ltc)+(sum(zagreb)*10))

nr.patient.cost15=c((length(which(TRC4>0))*ltc)+15,
                (length(which(ID4>0))*ltc)+15,
                (length(which(weekPEP>0))*ltc)+15,
                (length(which(essen4>0))*ltc)+(sum(essen4)*10),
                (length(which(zagreb>0))*ltc)+(sum(zagreb)*10))

far.patient.cost20=c((length(which(TRC4>0))*htc)+20,
                (length(which(ID4>0))*htc)+20,
                (length(which(weekPEP>0))*htc)+20,
                (length(which(essen4>0))*htc)+(sum(essen4)*10),
                (length(which(zagreb>0))*htc)+(sum(zagreb)*10))

far.patient.cost15=c((length(which(TRC4>0))*htc)+15,
                (length(which(ID4>0))*htc)+15,
                (length(which(weekPEP>0))*htc)+15,
                (length(which(essen4>0))*htc)+(sum(essen4)*10),
                (length(which(zagreb>0))*htc)+(sum(zagreb)*10))

cost.summary=as.data.frame(cbind(regime,
              nr.travelcosts, far.travelcosts,
              nr.patient.cost, nr.patient.cost2.5, far.patient.cost,far.patient.cost2.5,
              nr.patient.cost20,nr.patient.cost15, far.patient.cost20, far.patient.cost15))

write.csv(cost.summary, file="costsummary.csv", row.names=FALSE)



#how do these prices compare for the patient given low and high travel costs?
nr.patient.cost=c((length(which(TRC4>0))*ltc)+1.5,
                (length(which(ID4>0))*ltc)+1.5,
                (length(which(essen4>0))*ltc)+sum(essen4),
                (length(which(zagreb>0))*ltc)+sum(zagreb))

far.patient.cost=c((length(which(TRC4>0))*htc)+1.5,
                (length(which(ID4>0))*htc)+1.5,
                (length(which(essen4>0))*htc)+sum(essen4),
                (length(which(zagreb>0))*htc)+sum(zagreb))


#_________________________________________________________________





#For different numbers of patients per month (1:120)
#calculate the price of ID as a proportion of IM, N times
eval.cost=function(m.patients, doses.per.vial=5, vial.cost){
  startdates=floor(sort(runif(m.patients*12, 1, 365)))    #Start PEP dates for patients
  vaccdates=unlist(lapply(startdates, Vuse))             #delivery dates for 0.1 mL ID injections
  prop=vial.count(vaccdates[which(vaccdates>31)])/(length(which(startdates>31))*5) #proportional cost of ID cf IM
  prop_if_perfect=(length(vaccdates>31)*0.1)/(length(which(startdates>31))*5)
  IMcharge=(vial.cost*(length(which(startdates>31))*5))/11 #monthly income from IM
  IDcharge=((vial.cost*0.3)*length(vaccdates>31))/11 #monthy income from ID
  c(prop_if_perfect, prop, IMcharge, IDcharge)
}


costs=costs50=IDincome=IMincome=IDincome50=IMincome50=matrix(nrow=length(patients), ncol=5)
for(i in 1:length(patients)){
  costrecords=replicate(500, eval.cost(patients[i], vial.cost=10)) #repeat analysis     #change to 500
  costs[i,]=quantile(costrecords[2,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IMincome[i,]=quantile(costrecords[3,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IDincome[i,]=quantile(costrecords[4,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  costrecords2=replicate(500, eval.costPC(patients[i], compliance=0.5, vial.cost=10)) #repeat analysis  #change to 500
  costs50[i,]=quantile(costrecords2[2,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IMincome50[i,]=quantile(costrecords2[3,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IDincome50[i,]=quantile(costrecords2[4,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}


#evaluates the costs under different levels of compliance
#and calculates gains and losses if ID is charged at 0.3 of a IM dose
eval.costPC=function(m.patients, compliance, doses.per.vial=5, vial.cost){
  startdates=floor(sort(runif(m.patients*12, 1, 365))) #Start PEP dates for patients
  vaccdatesID=unlist(lapply(startdates, pc=compliance, VusePC, regime=TRC)) #delivery dates for 0.1 mL ID injections
  vaccdatesIM=unlist(lapply(startdates, pc=compliance, VusePC, regime=essen)) #delivery dates for 0.1 mL ID injections
  prop=vial.count(vaccdatesID[which(vaccdatesID>31)])/length(which(vaccdatesIM>31)) #proportional cost of ID cf IM
  prop_if_perfect=(length(vaccdatesID>31)*0.1)/(length(which(vaccdatesIM>31)))
  IMcharge=(vial.cost*length(which(vaccdatesIM>31)))/11 #monthly income from IM
  IDcharge=((vial.cost*0.3)*length(which(vaccdatesID>31)))/11 #monthy income from ID
  c(prop_if_perfect, prop, IMcharge, IDcharge)
}

#Redo to look at costs if ID PEP course charged at price of 1 vial
#look at 1ml and 0.5ml vials
#look at reduced 4 dose regimes


#eval.cost(3)  #check it works
#eval.costPC(3, 0.5)

#_______________________________________________________________________________
#plot from 1:100 patients a month the price of vaccine that would on average cover the costs
#and also assume compliance is not 100%

patients=c(1:50, seq(55, 120, 5))
costs=costs50=IDincome=IMincome=IDincome50=IMincome50=matrix(nrow=length(patients), ncol=5)
for(i in 1:length(patients)){
  costrecords=replicate(500, eval.cost(patients[i], vial.cost=10)) #repeat analysis     #change to 500
  costs[i,]=quantile(costrecords[2,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IMincome[i,]=quantile(costrecords[3,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IDincome[i,]=quantile(costrecords[4,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  costrecords2=replicate(500, eval.costPC(patients[i], compliance=0.5, vial.cost=10)) #repeat analysis  #change to 500
  costs50[i,]=quantile(costrecords2[2,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IMincome50[i,]=quantile(costrecords2[3,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  IDincome50[i,]=quantile(costrecords2[4,], c(0.01, 0.05, 0.5, 0.95, 0.99))
  print(i)
}

setwd("C:\\Documents and Settings\\Katie Hampson\\My Documents\\Vaccination")
write.csv(costs, file="costs.csv", row.names=FALSE)
write.csv(costs50, file="costs50.csv", row.names=FALSE)
write.csv(IMincome, file="IMincome.csv", row.names=FALSE)
write.csv(IDincome, file="IDincome.csv", row.names=FALSE)
write.csv(IMincome50, file="IMincome50.csv", row.names=FALSE)
write.csv(IDincome50, file="IDincome50.csv", row.names=FALSE)


setwd("C:\\Documents and Settings\\Katie Hampson\\My Documents\\Vaccination")
postscript(file="IDvsIM.eps", width=8, height=4, horizontal=F)
par(cex=0.7, lwd=0.5, plt=c(0.1, 0.5, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, costs[,3], type="l", ylim=c(0,1), col="dark green", axes=FALSE,
  xlab="Average number of patients per month",
  ylab="Cost of ID administration of PEP \n as a proportion of the costs of IM administration")
axis(2, lwd=0.5); axis(1, lwd=0.5)
polygon(c(patients, sort(patients, decreasing=T)),
  c(costs[,1], costs[length(patients):1,5]),
  border=NA, col="green")
lines(patients, costs[,3], type="l", lwd=2, col="dark green")

polygon(c(patients, sort(patients, decreasing=T)),
  c(costs50[,1], costs50[length(patients):1,5]),
  border=NA, col="blue")
lines(patients, costs50[,3], type="l", col="dark blue", lwd=2)

legend(50, 0.6, legend=c("100% compliance with 99% CIs", "50% compliance with 99% CIs"),
  bty="n")
polygon(c(45,52, 52, 45), c(0.545,0.545,0.575, 0.575), col="green", border=NA)
polygon(c(45,52, 52, 45), c(0.505,0.505,0.535, 0.535), col="blue", border=NA)
lines(45:52, rep(0.56, 8), col="dark green", lwd=2)
lines(45:52, rep(0.52,8), col="dark blue", lwd=2)

#calculate the savings that would accrue assuming a single standard ID cost
#USE median effective regimen (ie on average recoup costs):
#100% compliance and cost of 1 vial $10, but ID patients pay $2 for each injection
#therefore IM cost = $50 and ID cost = $24

#IDcost=IMincome[,3]*costs[,3]
#IDrecouped=IDincome[,3]-IDcost
#plot(patients, IDrecouped)

IDcost=IMincome50[,3]*costs50[,3]
IDrecouped=IDincome50[,3]-IDcost

par(new=TRUE, cex=0.7, lwd=0.5, plt=c(0.57, 1, 0.1, 0.9), mgp=c(1.2,0.2,0), tck=-0.01)
plot(patients, IDrecouped, type="l", ylim=c(-100,1000), axes=FALSE,
  xlab="Average number of patients per month",
  ylab="Net monthly gains/losses when charging \n $3/ID injection (0.5mL vial cost ~$10)")
axis(2, lwd=0.5); axis(1, lwd=0.5)
lines(patients, rep(0, length(patients)), lty=2)
lines(rep(18,20), seq(-50,1000, length=20), col="red")
#text(10, 500, labels="25% of /n vaccination /n posts")
#text(10, 500, labels="10% of /n patients")

#dev.off()

#now look at the proportion of visitors coming from high and low throughput clinics
par(mfrow=c(1,2))
plot(patients, costs[,3], type="l", ylim=c(0,1),
  xlab="Average number of patients per month",
  ylab="Cost as proportion of costs of IM administration")
polygon(c(patients, sort(patients, decreasing=T)),
  c(costs[,1], costs[length(patients):1,5]),
  border=NA, col="light gray")
polygon(c(patients,sort(patients, decreasing=T)),
  c(costs[,2], costs[length(patients):1,4]),
  border=NA, col="gray")
lines(patients, costs[,3], type="l")

plot(patients, costs50[,3], type="l", ylim=c(0,1))
polygon(c(patients, sort(patients, decreasing=T)),
  c(costs50[,1], costs50[length(patients):1,5]),
  border=NA, col="light gray")
polygon(c(patients,sort(patients, decreasing=T)),
  c(costs50[,2], costs50[length(patients):1,4]),
  border=NA, col="gray")
lines(patients, costs50[,3], type="l")

setwd("C:\\Documents and Settings\\Katie Hampson\\My Documents\\Vaccination")
write.csv(costs, file="costs.csv", row.names=FALSE)
write.csv(costs50, file="costs50.csv", row.names=FALSE)

#check PEP admin incidence:

VTN=615000/82000000
China=2500000/1306000000
India=2300000/1027000000
Indonesia=6770/219000000
Philippines=55301/84000000
SL=200000/20000000
Thailand=351535/63000000

countries=c("Vietnam", "China", "India", "Indonesia", "Philippines", "Sri Lanka", "Thailand")
PEP=100000*c(VTN, China, India, Indonesia, Philippines, SL, Thailand)
cbind(countries, PEP)

plot(patients, costs[,3], type="l", ylim=c(0,1))
polygon(c(patients,patients[max(patients):min(patients)]),
  c(costs[,1], costs[max(patients):min(patients),5]),
  border=NA, col="gray")

polygon(c(patients,patients[max(patients):min(patients)]),
  c(costs[,1], costs[max(patients):min(patients),5]),
  border=NA, col="light gray")
polygon(c(patients,patients[max(patients):min(patients)]),
  c(costs[,2], costs[max(patients):min(patients),4]),
  border=NA, col="gray")
lines(patients, costs[,3], type="l")


#Now assume compliance is not 100%
#plot from 1:100 patients a month the price of vaccine that would cover costs
#given 50% compliance!

Total_mL=length(Vdates)*0.1   #total mL vaccine use - equivalent to perfect delivery
                              #equiv. to No. patients * 0.8mL

#____________________________________________________________________
#Plot price proportion of per vial price
#Examine costs needed to charge to recover price given
    #Variation in compliance (%)
    #Variation clustering of patients (over/under-dispersed)
    #Plot CI's for mean recovery of all costs (95% of time etc) and profits....
    #Also look at use within 6 hours or use on same day
    #Also look at increased budget reuired for initiation of change from IM-ID
    #look at costs depending upon whether 5 doses or 4 doses administered from 1 vial

#Also look at 1mL vs 0.5mL vials


vial.cost=c(7.5, 10, 30)
Pvial=0.2; vial=10

IDdose=0.2; IDvials1=1; IDvials0.5=2; ID.visits=4 #ID regimen= 2,2,2,2  (0,3,7,28)
zagreb.vials=4; zagreb.visits=3 #2,1,1 (0,7,21)
zagrebTZ.vials=3

visitUSDlow=15; visitUSDhigh=30; visitUSDvlow=5  #$15 or $30 per visit or PC=0.55 #% of indirect costs

#calculations assuming different travel costs
zagreb.costs=ID.costsMIN=ID.costsMAX=matrix(NA, nrow=length(vial.cost), ncol=length(patients))
for(i in 1:length(vial.cost)){
  zagreb.costs[i,]=patients*((zagreb.visits*visitUSDlow)+(zagreb.vials*vial.cost[i]))
  ID.costsMIN[i,]=((1+(IDdose*patients)%/%IDvials1)*ID.visits*vial.cost[i])+(patients*ID.visits*visitUSDlow)
  ID.costsMAX[i,]=((1+(IDdose*patients)%/%IDvials0.5)*ID.visits*vial.cost[i])+(patients*ID.visits*visitUSDlow)
}

#patient costs
zagreb.Pcosts=patients*((zagreb.visits*visitUSDlow)+(zagreb.vials*Pvial))
ID.PcostsMIN=((1+(IDdose*patients)%/%IDvials1)*ID.visits*Pvial)+(patients*ID.visits*visitUSDlow)
ID.PcostsMAX=((1+(IDdose*patients)%/%IDvials0.5)*ID.visits*Pvial)+(patients*ID.visits*visitUSDlow)
zagreb.PcostsvL=patients*((zagreb.visits*visitUSDvlow)+(zagreb.vials*Pvial))
ID.PcostsMINvL=((1+(IDdose*patients)%/%IDvials1)*ID.visits*Pvial)+(patients*ID.visits*visitUSDvlow)
ID.PcostsMAXvL=((1+(IDdose*patients)%/%IDvials0.5)*ID.visits*Pvial)+(patients*ID.visits*visitUSDvlow)

#Health Provider costs
#Calculate so that health providers pay remainder after patients pay 20c.
zagreb.HPcosts=patients*((zagreb.vials*(vial-Pvial)))
ID.HPcostsMIN=((1+(IDdose*patients)%/%IDvials1)*ID.visits*vial)-(patients*Pvial)
ID.HPcostsMAX=((1+(IDdose*patients)%/%IDvials0.5)*ID.visits*vial)-(patients*Pvial)


par(mfrow=c(1,3))

#pdf(file="C://Documents and Settings//Katie Hampson//My Documents//Vaccination//HumanARV.pdf", width=4, height=4)

postscript(file="C://Documents and Settings//Katie Hampson//My Documents//Vaccination//HumanARVtest.eps",
  width=10, height=3, horizontal=F)
par(mfrow=c(1,1), cex=0.6, lwd=0.5, plt=c(0.05, 0.3, 0.12, 0.9), tck=-0.01, mgp=c(1.2,0.3,0), yaxs="i", xaxs="i")

plot(patients, zagreb.costs[2,], xlab="Patients/day", ylab="Cost of ARV (USD)",
    type="l", ylim=c(0,1500), xlim=c(0,20), lwd=2, axes=F)
mtext("Overall Costs of PEP", side=3, font=2, cex=0.75)
axis(1, lwd=0.5); axis(2, lwd=0.5)
#lines(patients, zagreb.costs[1,], lty=2); lines(patients, zagreb.costs[3,], lty=2)
lines(patients, ID.costsMIN[2,], col="red", lwd=2)
#lines(patients, ID.costsMIN[1,], col="red", lty=2); lines(patients, ID.costsMIN[3,], col="red", lty=2)
lines(patients, ID.costsMAX[2,], col="green", lwd=2)
#lines(patients, ID.costsMAX[1,], col="green", lty=2); lines(patients, ID.costsMAX[3,], col="green", lty=2)
legend(x=2, y=1250, legend=c("Zagreb", "ID 0.5ml", "ID 1ml"), col=c("black", "red", "green"), lwd=c(2,2,2), bty="n")

par(new=T, yaxs="r", xaxs="i", plt=c(0.35, 0.6, 0.12, 0.9), mgp=c(1.2,0.3,0))
plot(patients, zagreb.Pcosts, type="l", xlab="Patients/day", ylab="Patient-borne costs (USD)",
    ylim=c(0,1500), xlim=c(0,20), lwd=2, axes=F)
mtext("Patient-borne costs of PEP", side=3, font=2, cex=0.75)
axis(1, lwd=0.5); axis(2, lwd=0.5)
lines(patients, ID.PcostsMIN, col="red", lwd=2)
lines(patients, ID.PcostsMAX, col="green", lwd=2)
lines(patients, zagreb.PcostsvL, lwd=2, lty=2)
lines(patients, ID.PcostsMINvL, col="red", lwd=2, lty=2)
lines(patients, ID.PcostsMAXvL, col="green", lwd=2, lty=2)
legend(x=1, y=1250, legend=c("Travel costs ~$15/visit", "Low travel costs ~$5/visit"),
col=c("black", "black"), lty=c(1,2), bty="n")


par(new=T, yaxs="r", xaxs="i", plt=c(0.65, 0.9, 0.12, 0.9), mgp=c(1.2,0.3,0))
plot(patients, zagreb.HPcosts, type="l", xlab="Patients/day", ylab="Health provider costs (USD)",
    ylim=c(0,1500), xlim=c(0,20), lwd=2, axes=F)
mtext("Health provider costs of PEP", side=3, font=2, cex=0.75)
axis(1, lwd=0.5); axis(2, lwd=0.5)
lines(patients, ID.HPcostsMIN, col="red", lwd=2)
lines(patients, ID.HPcostsMAX, col="green", lwd=2)
dev.off()

ID.PcostsMAX=(1+(IDdose*patients)%/%IDvials0.5)*ID.visits*(vial.cost[2]+visitUSDlow)
ID.HPcostsMAX=(1+(IDdose*patients)%/%IDvials0.5)*ID.visits*(vial.cost[2]+visitUSDlow)

#health provider costs



zagreb.costs=patients*((zagreb.visits*visitUSDlow)+(zagreb.vials*vial.cost))
ID.costsMIN=(1+(IDdose*patients)%/%IDvials1)*ID.visits*(vial.cost+visitUSDlow)
ID.costsMAX=(1+(IDdose*patients)%/%IDvials0.5)*ID.visits*(vial.cost+visitUSDlow)

zagreb.costs=patients*((zagreb.visits*visitUSDhigh)+(zagreb.vials*vial.cost))
zagrebTZ.costs=patients*((zagreb.visits*visitUSDhigh)+(zagrebTZ.vials*vial.cost))
ID.costsMIN=(1+(IDdose*patients)%/%IDvials1)*ID.visits*(vial.cost+visitUSDhigh)
ID.costsMAX=(1+(IDdose*patients)%/%IDvials0.5)*ID.visits*(vial.cost+visitUSDhigh)

plot(patients, zagreb.costs, xlab="Patients/day", ylab="costs of ARV (USD)", type="l", ylim=c(0,1500), xlim=c(0,20))
lines(patients, zagrebTZ.costs, lty=2)
lines(patients, ID.costsMIN, col="red")
lines(patients, ID.costsMAX, col="green")

plot(patients, zagreb.costs/patients, xlab="Patients/day", ylab="costs of ARV (USD)", type="l", ylim=c(0,200), xlim=c(0,20))
lines(patients, ID.costsMIN/patients, col="red")
lines(patients, ID.costsMAX/patients, col="green")



#How many people receive less than the 5 dose Essen regimen

#based on the PLoS NTDs paper:

AI = c(63.04, 99.79)/100000 #annual incidence of bites
TZpop = 34671453 #Total population in Tanzania
agro=0.7	 #expected proportion of poulation which is rural
TZdose=0.7 #assume 70% of TZ bite victims receive the 3 dose schedule

bites=AI*TZpop  #annual no. exposures in TZ
bites*agro*TZdose #The number of exposures that get TZ 3 dose schedule pa - 16953!

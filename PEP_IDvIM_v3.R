# SAGE meeting preparations - 15 Jun 2017
# Examine how costs of vaccination vary with patient throughput
rm(list=ls())

# Calculate vials used, cost, 'wastage' under different throughput scenarios

#Vaccination schedule and regimens current in use
schedule = c(0, 3, 7, 14, 21, 28, 90) # dates for delivery

# essen = c(1,1,1,1,0,1,0)      #IM Essen 5 doses - 1 vial/ vaccination
essen4 = c(1,1,1,1,0,0,0)     #IM Essen reduced 4 dose
zagreb = c(2,0,1,0,1,0,0)     #IM Zagreb 3 visit
TRC4 = c(2,2,2,0,0,2,0)       #ID reduced 4 dose TRC
# site8 = c(8,0,4,0,0,1,1)		  #ID 8-site

# new vaccination schedules and regimens to investigate
ID4 = c(4,0,2,0,0,1,0)        #ID 4-site economical - 3 visits
ID4v2 = c(4,0,2,0,0,0,0)		  #ID 4-site economical - 2 visits (MW)
weekID=c(4,4,4,0,0,0,0)       #ID 1-week - Narayana et al, Shantavasinkul et al, Sudarshen et al.
IPC = c(2,2,2,0,0,0,0)
weekIM1 = c(1,1,1,0,0,0,0)
weekIM2 = c(2,0,1,0,0,0,0) # Huang et al

# All different regimens combined (9)
regimens=list(TRC4=c(2,2,2,0,0,2,0), # Default ID
              essen4=c(1,1,1,1,0,0,0) , zagreb=c(2,0,1,0,1,0,0), # Default IM
              ID4=c(4,0,2,0,0,1,0), ID4v2=c(4,0,2,0,0,0,0), # Mary Warrell
              weekID=c(4,4,4,0,0,0,0), # Under investigation ID
              IPC=c(2,2,2,0,0,0,0), # Under investigation ID
              weekIM1=c(1,1,1,0,0,0,0), weekIM2=c(2,0,1,0,0,0,0)) # Under investigation IM

v=data.frame(regimen = c("TRC4", "essen4", "zagreb", # Default
              "ID4", "ID4v2", # Mary Warrell
              "weekID", "IPC", "weekIM1", "weekIM2")) # Under investigation IM
v$vdoses_waste = c(4,1,1,4,4,4,4,1,1)
v$vdoses_nowaste = c(5,1,1,5,5,5,5,1,1)
v$vdoses_waste1 = c(8,1,1,4,4,8,8,1,1)
v$vdoses_nowaste1 = c(10,1,1,5,5,10,10,1,1)

source("R/PEPfunctions.R")

# ANALYSE:
# 100% compliance and 75% compliance
# 0.5 and 1mL vials
# Assume perfect usage versus some wastage

#look at different numbers of vials used for different regimens
patients=c(1,5,10,20,30,40,50,75,100,500) # New patients per month
reps = 500 # increase to 1000

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

# 1. 100% compliance, 0.5ml vials, perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], ".csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste[j], unlist(regimens[j]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

#_______________________________________________________________________________
#2
#100% compliance - 1mL vials - perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "_1ml.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste1[j], unlist(regimens[j]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

#_______________________________________________________________________________
#3 - 100% compliance - 1mL vials - some waste
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "_1ml_waste.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_waste1[j], unlist(regimens[j]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

#_______________________________________________________________________________
#4 - 100% compliance - 0.5mL vials - some waste
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "_waste.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_waste[j], unlist(regimens[j]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}



# 100% compliance, 1ml vials with wasteage using the MW 4-site regimen with 0.1ml injections
vials = matrix(nrow=length(patients), ncol=5)
fname = "output/vID4waste_0.1ml.csv"; print(fname)

for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=8, unlist(regimens[4]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }

write.csv(vials, file=fname, row.names=FALSE) # write to csv


#_______________________________________________________________________________

#_______________________________________________________________________________
#Compliance calculations...
#75% Compliance - 0.5mL vials - perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "_pc0.75.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste[j], unlist(regimens[j]), pc=0.75))
    vials[i, ]=quantile(vial.res, c(0.025, 0.05, 0.5, 0.95, 0.975))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}


#75% Compliance - 1mL vials - perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "1ml_pc0.75.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste1[j], unlist(regimens[j]), pc=0.75))
    vials[i, ]=quantile(vial.res, c(0.025, 0.05, 0.5, 0.95, 0.975))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

#50% Compliance - 0.5mL vials - perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "_pc0.5.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste[j], unlist(regimens[j]), pc=0.5))
    vials[i, ]=quantile(vial.res, c(0.025, 0.05, 0.5, 0.95, 0.975))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

#50% Compliance - 1mL vials - perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("output/v", names(regimens)[j], "1ml_pc0.5.csv", sep="")
  print(fname)

  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste1[j], unlist(regimens[j]), pc=0.5))
    vials[i, ]=quantile(vial.res, c(0.025, 0.05, 0.5, 0.95, 0.975))
    print(i)
  }
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}

# # FOR GAVI MODELING PURPOSES
# patients
# multiplier = 12/11.25 # WORK OUT WHY MULTIPLIER NEEDED!
#
# # IM administration - 4 vials, 4 visits, no change under high, medium and low throughput
# vuse1[[which(names(regimens)=="essen4")]][,3]/(patients*12)*multiplier # Essen 4
#
# # For ID admininstration:
# vuse1[[which(names(regimens)=="TRC4")]][,3]/(patients*12)*multiplier # Updated TRC
# vuse1[[which(names(regimens)=="IPC")]][,3]/(patients*12) # IPC
#
# # FOR GAVI MODELING:
# write.csv(data.frame(throughput = patients,
#                      IM_vials = vuse1[[which(names(regimens)=="essen4")]][,3]/(patients*12)* multiplier, # Essen 4,
#                      TRC_vials = vuse1[[which(names(regimens)=="TRC4")]][,3]/(patients*12)* multiplier, # Updated TRC
#                      IPC_vials = vuse1[[which(names(regimens)=="IPC")]][,3]/(patients*12)), # IPC
#                      "output/Vaccine_vials.csv")



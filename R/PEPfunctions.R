#functions for vaccine use
# VACCINE USE
Vuse=function(dose1, regimen=TRC, days=schedule){ #DEFAULT = TRC 5 visit
  #list days when each vaccination is delivered:
  # requires starting vacc dates, regimen used schedule
  Vdays=dose1+days
  rep(Vdays, regimen)
}
# examples
# Vuse(1, zagreb); length(Vuse(1, zagreb))*1 # days when shots delivered; 4 shots for 1 patient
# length(Vuse(3, essen4)) # 4 shots for 1 patient


# VACCINE USE ACCORDING TO PATIENT COMPLIANCE
#####################################################################
# CONSIDER SPECIFYING COMPLIANCE FOR EACH SPECIFIC DOSE FOR
# SPECIFIC REGIMENS BASED ON DATA
# E.G. TRC4, d1-4: c(1, 0.9, 0.3, 01)
# ALSO CONSIDER COMPARING DATES OF PATIENT VISITS WITH RANDOM UNIFORM
# SAMPLING VS CLUSTERED SAMPLING i.e. patients clustering together in
# time. Need to check poisson spatial sampling model, where for a subset
# of patients,
#####################################################################
VusePC=function(pc, sdate, regimen=TRC4){
  # list days when each vaccination is delivered according to the probability of compliance (pc)
  last=NA
  finish=which(rbinom(4,1,pc)==0)[1]       # end when first visit is abandoned!
  last=ifelse(is.na(finish), 6, finish+1)  # chose a long enough sequence
  days=schedule[which(regimen!=0)][1:last]
  doses=regimen[which(regimen!=0)][1:last]
  rep(days[!is.na(days)], doses[!is.na(doses)])+sdate #work out from the startdate!
}
# VusePC(0.5, 5) #examples - set.seed(50); startdates=floor(sort(runif(10*12, 1, 365)))


#####################################################################
# VIAL COUNT BASED ON HOSPITAL VISITS & DOSES PER VIAL
vial.count=function(visits, doses.per.vial){
  # calculate no. vials used based on patient reporting dates
  # requires days of patient visits and assumption about doses/vial,
  # default=5 (i.e. 0.5mL vial, each dose=0.1mL)
  vcount=table(visits)
  max.vials=ceiling(max(vcount)/doses.per.vial) #max no. vials used in 1 day
  sum(hist(vcount, breaks=seq(0, max.vials*doses.per.vial, by=doses.per.vial), plot=FALSE)$counts * (1:max.vials)) #vials used
}

#####################################################################
# CALCULATE VIAL USE UNDER DIFFERENT REGIMENS, THRU-PUT, COMPLIANCE & DOSES/VIAL
eval.vials=function(m.patients, doses.per.vial=5, regimen=TRC4, pc=1){
  # DEFAULT=TRC, 5 inj/vial and 100% compliance
  # m.patients = monthly NEW bite patients!
  startdates=floor(sort(runif(m.patients*12, 1, 365)))          # Start PEP dates for patients (over year)
  vaccdates=unlist(lapply(startdates, VusePC, pc=pc, regimen=regimen))  #d elivery dates for injections
  vial.count(vaccdates[which(vaccdates>31)], doses.per.vial)
  # cut last line because make proportion of vials used >1 sometimes....
  #  vial.count(vaccdates, doses.per.vial)
  # last line means counting vials after first month
}

#examples
# eval.vials(m.patients=12); eval.vials(12, 1, essen4)
# # eval.vials(m.patients=12, 5, TRC, pc=0.5)
# eval.vials(m.patients=3, 5, TRC4, pc=0.7); eval.vials(m.patients=3, 5, TRC4, pc=1)
# eval.vials(m.patients=12, 5, pc=1, weekID)
# eval.vials(m.patients=4, doses.per.vial=5, regimen=TRC4, pc=1)
#####################################################################

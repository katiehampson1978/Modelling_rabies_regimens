#functions for costs of PEP

# Number of visits for given regimen
n.visit=function(regimen){length(which(regimen>0))}

# Number of injections for a given regimen
n.inj=function(regimen){regimen[which(regimen>0)]}

# Costs of PEP administration
costs=function(overhead, material, vial.cost, patients, vials, regimen){
  (vials*vial.cost)+
    (((sum(n.inj(regimen))*material)+(n.visit(regimen)*overhead))*patients*12)
}
# costs(overhead, material, vial.cost, 20, 21, essen4)

# Costs of PEP administration given different levels of compliance
costs_lc=function(overhead, material, vial.cost, patients, vials, regimen){
  (vials*vial.cost)+
    (overhead+((regimen[which(regimen>0)][1])*material))*((patients*12))+ #First visit only
    (overhead+((regimen[which(regimen>0)][2])*material))*(patients*12/2)+ #1st & 2nd visit only
    (overhead+((regimen[which(regimen>0)][3])*material))*(patients*12/2/2)+ #1st-3rd visit only
    ifelse(is.na(regimen[which(regimen>0)][4]), 0, (overhead+(regimen[which(regimen>0)][4]*material))*(patients*12/2/2)) #1st-3rd visit only
}
# costs_lc(overhead, material, vial.cost, 20, 21, essen4)


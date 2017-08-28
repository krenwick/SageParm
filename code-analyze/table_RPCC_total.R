################################################################################
# Make table of RPCC values for total GPP and LAI + sage LAI and cmass + seasonal
################################################################################
rm(list=ls())
library(xtable)

# Set working directory to access scripts and data:
setwd("~/Documents/SageParm")

# Pull in RPCC function:
source("code-analyze/fxn_RPCC.R")
# NOTE: if using different # of PFTs (not 2) must change line 52 in function
# could fix this later... must enter # columns in annual out (5+n(PFTs))
# also lines 26-27
# and check columns to drop in line 53

# Loop through variables and calculate RPCC
vars <- c("mnpp","mnee","mgpp",#"mevap","maet","mrh","mra","mpet",
          "fpc","lai","anpp","dens","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:14)) # final # = # parameters
for(var in vars) {
  P1 <- RPCC(paste("ModOut/Output_LHC/", var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1 <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCC(paste("ModOut/Output_LHC/",var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1 <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCC(paste("ModOut/Output_LHC/",var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1 <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCC(paste("ModOut/Output_LHC/",var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1 <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Re-do all this for grass, but only pulling in PFT-specific variables
################################################################################
# Loop through variables and calculate RPCC
vars <- c("fpc","lai","anpp","dens","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:14)) # final # = # parameters
for(var in vars) {
  P1 <- RPCCgrass(paste("ModOut/Output_LHC/", var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1g <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCgrass(paste("ModOut/Output_LHC/",var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1g <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCgrass(paste("ModOut/Output_LHC/",var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1g <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCgrass(paste("ModOut/Output_LHC/",var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1g <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Re-do all this for TOTAL, but only pulling in PFT-specific variables
################################################################################
# Loop through variables and calculate RPCC
vars <- c("fpc","lai","anpp","dens","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:14)) # final # = # parameters
for(var in vars) {
  P1 <- RPCCtotal(paste("ModOut/Output_LHC/", var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1t <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCtotal(paste("ModOut/Output_LHC/",var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1t <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCtotal(paste("ModOut/Output_LHC/",var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1t <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCtotal(paste("ModOut/Output_LHC/",var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1t <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Get average |RPCC| and drop parameters <.2
# Format into pretty table ready for export
################################################################################
# Ecosystem-level params: NEE, GPP, total LAI
# Sage parms: lai, cmass
################################################################################
# Try getting total (annual) LAI:
################################################################################
# Table of all parameters and average RPCC across sites and variables
# First, make old table that was good:
t1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(Parameter,site,mgpp,lai,cmass) %>%
  mutate_each(funs(a=abs),mgpp:cmass) %>%
  group_by(Parameter) %>%
  summarise_each(funs(mean), mgpp:cmass_a) %>%
  mutate(mean=rowMeans(.[,5:7])) %>%
  mutate(max=apply(.[,5:7],1,max)) %>%
  # cut rows where max < .2
  filter(max>=.2) %>%
  dplyr::select(Parameter, mean,mgpp:cmass) %>%
  arrange(desc(mean)) %>%
  mutate_each(funs(round(.,2)),mean:cmass)

t2 <- rbind.data.frame(mbsec1t,losec1t,wbsec1t,fire1t) %>%
  dplyr::select(Parameter,fpc,lai,anpp,cmass) %>%
  mutate_each(funs(a=abs),fpc:cmass) %>%
  group_by(Parameter) %>%
  summarise_each(funs(mean), fpc:cmass_a) %>%
  mutate(mean=rowMeans(.[,6:9])) %>%
  mutate(max=apply(.[,6:9],1,max)) %>%
  # cut rows where max < .2
  #filter(max>=.2) %>%
  dplyr::select(Parameter, lai,cmass) %>%
  arrange(desc(abs(lai))) %>%
  mutate_each(funs(round(.,2)),lai:cmass) %>%
  rename(TotLAI=lai,Totcmass=cmass)

t3 <- merge(t1,t2,by="Parameter") %>%
  arrange(desc(abs(mgpp))) %>%
  select(Parameter,mgpp,TotLAI,Totcmass,lai,cmass) %>%
  mutate(Parameter=factor(Parameter, levels=c("sla","ltor_max","root_up",
                                              "phengdd5","latosa","pstemp_max"),
                          labels=c("sla","ltor\\textsubscript{max}",
                                              "root\\textsubscript{up}",
                                              "GDD\\textsubscript{5}",
                                              "latosa",
                                              "pstemp\\textsubscript{max}"),
                          ordered=T))
t3

names(t3) <- c("Parameter","GPP","LAI","MASS","LAI","MASS")

t4 <- xtable(t3)
print(t4,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      sanitize.text.function = identity,
      file="figures/RPCCecosage.tex")


################################################################################
# Make table just for biomass
f1
f2 <- f1[1:5,]
names(f2) <- c("Parameter", "mean", "burn","losec","mbsec","wbsec")
f3 <- dplyr::select(f2, Parameter,mean,wbsec,losec,burn,mbsec) %>%
  mutate(mean=rowMeans(.[,3:6])) %>%
  mutate(max=apply(abs(.[,3:6]),1,max)) %>%
  # cut rows where max < .2
  filter(max>=.2) %>%
  dplyr::select(-max) %>%
  mutate_each(funs(round(.,2)),mean:mbsec)

f4 <- xtable(f3)
print(f4,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=TRUE,
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="figures/cmass.tex")

################################################################################
# Pull in seasonal variables and look at those
################################################################################
seas <- c("spring","summer","fall")
sites <- c("wbsec","losec","h08ec","mbsec")
dat1 <- data.frame(ID=seq(1:14))
d <- RPCCseas("ModOut/Output_LHC/mgpp.txt")
d %>% group_by(Lon) %>%
  summarise(mean=mean(spring))
for(sea in seas) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- d %>% filter(Lon==lon) %>%
      dplyr::select(spring:turnover_sap) %>%
      gather(Season, Value, spring:fall) %>%
      filter(Season==sea)
    R <- pcc(X=d1[,1:14], y=d1[,16], rank=T)
    aa <- R$PRCC
    names(aa) <- paste(sea,site,sep="_")
    dat1 <- cbind(aa,dat1)
  }
}
dat2 <- rownames_to_column(dat1, "Parameter") %>% dplyr::select(-ID) %>%
  gather(site,RPCC,fall_mbsec:spring_wbsec) %>%
  group_by(Parameter) %>%
  mutate(mean=mean(abs(RPCC)), max=max(abs(RPCC))) %>%
  spread(site,RPCC) %>%
  arrange(desc(mean)) %>%
  # Cut parameters where the maximum < .2
  filter(max>=.2) %>%
  mutate_each(funs(round(.,2)),mean:summer_wbsec) %>%
  dplyr::select(Parameter,spring_wbsec,spring_losec,spring_h08ec,spring_mbsec,
                summer_wbsec,summer_losec,summer_h08ec,summer_mbsec,
                fall_wbsec,fall_losec,fall_h08ec,fall_mbsec)

# Order rows differently
#target <- c("sla","")
#df[match(target, df$name),]

dat3 <- xtable(dat2)
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c("Parameter & wbsec & losec & burn & mbsec &
                      wbsec & losec & burn & mbsec &
                      wbsec & losec & burn & mbsec \\\\") # need 4 \ to get 2 in output

# "& \multicolumn{4}{c}{Spring} & \multicolumn{4}{c}{Summer} & 
#                       \multicolumn{4}{c}{Fall} \\",
# 		"\cmidrule(lr){2-5} \cmidrule(lr){6-9} \cmidrule(lr){10-13}",

print(dat3,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      add.to.row = addtorow,
      booktabs = TRUE,
      type="latex",
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="figures/RPCC_seas.tex")

################################################################################
# Look at RPCC for month of peak GPP (also month exceed then drop below 20% max)
################################################################################

################################################################################
# Pull in monthly GPP and look at those
################################################################################
mo <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Nov","Dec")
dat1 <- data.frame(ID=seq(1:14))
d <- RPCCmonth("ModOut/Output_LHC/mgpp.txt")
for(month in mo) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- d %>% filter(Lon==lon) %>%
      dplyr::select(Jan:turnover_sap) %>%
      dplyr::select(-row) %>%
      gather(Month, Value, Jan:Dec) %>%
      filter(Month==month)
    R <- pcc(X=d1[,1:14], y=d1[,16], rank=T)
    aa <- R$PRCC
    names(aa) <- paste(month,site,sep="_")
    dat1 <- cbind(aa,dat1)
  }
}
dat2 <- rownames_to_column(dat1, "Parameter") %>% dplyr::select(-ID) %>%
  gather(site,RPCC,Dec_mbsec:Jan_wbsec) %>%
  group_by(Parameter) %>%
  mutate(mean=mean(abs(RPCC)), max=max(abs(RPCC)),
         mean_orig=mean(RPCC),max_orig=max(RPCC)) %>%
  spread(site,RPCC) %>%
  arrange(desc(mean)) %>%
  # Cut parameters where the maximum < .2
  #filter(max>=.2) %>%
  mutate_each(funs(round(.,2)),mean:Sep_wbsec) 

#dat2 %>% select(Parameter:max_orig)
# Nope, original scale is bad idea.
# parm can have neg impact in some months and pos in others
dat2 %>% select(Parameter:max) %>% arrange(max)
# > .2 by meanL: sla, root_up
# By max, add: pstemp_lo, ltor_max, pstemp_min, phengdd5, latosa.
# New grass: sla, root_up, ltor_max, phengdd5, est_max, pstemp_max, pstemp_lo, latosa, pstemp_min

# Re-do this for LAI to see if it differs:--------------------------------------
dat1b <- data.frame(ID=seq(1:14))
db <- RPCCmonth("ModOut/Output_LHC/mlai.txt")
for(month in mo) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- db %>% filter(Lon==lon) %>%
      dplyr::select(Jan:turnover_sap) %>%
      dplyr::select(-row) %>%
      gather(Month, Value, Jan:Dec) %>%
      filter(Month==month)
    R <- pcc(X=d1[,1:14], y=d1[,16], rank=T)
    aa <- R$PRCC
    names(aa) <- paste(month,site,sep="_")
    dat1b <- cbind(aa,dat1b)
  }
}
dat2b <- rownames_to_column(dat1b, "Parameter") %>% dplyr::select(-ID) %>%
  gather(site,RPCC,Dec_mbsec:Jan_wbsec) %>%
  group_by(Parameter) %>%
  mutate(mean=mean(abs(RPCC)), max=max(abs(RPCC)),
         mean_orig=mean(RPCC),max_orig=max(RPCC)) %>%
  spread(site,RPCC) %>%
  arrange(desc(mean)) %>%
  # Cut parameters where the maximum < .2
  #filter(max>=.2) %>%
  mutate_each(funs(round(.,2)),mean:Sep_wbsec) 
head(dat2b)
dat2b %>% select(Parameter:max)
# > .2 by mean: sla, root_up
# By max: ltor_max, phengdd5, latosa

lai <- dat2b %>% select(Parameter:max) %>%
  rename(lai_mean=mean,lai_max=max)
gpp <- dat2 %>% select(Parameter:max) %>%
  rename(gpp_mean=mean,gpp_max=max)

merge(lai,gpp,by="Parameter",all=T) %>%
  arrange(desc(gpp_mean))


################################################################################
# Calculate RPCC for each variable and each site: including 2 new phen parameters
# Make a couple of tables to show results
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
vars <- c("mnpp","mnee","mgpp",
          "fpc","lai","anpp","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:14)) # final # = # parameters
for(var in vars) {
  P1 <- RPCCnewphen(paste("ModOut/Output_LHC_newphen/", var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1 <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphen(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1 <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphen(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1 <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphen(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1 <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Re-do all this for TOTAL, but only pulling in PFT-specific variables
################################################################################
# Loop through variables and calculate RPCC
vars <- c("fpc","lai","anpp","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:14)) # final # = # parameters
for(var in vars) {
  P1 <- RPCCnewphenTotal(paste("ModOut/Output_LHC_newphen/", var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1t <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphenTotal(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1t <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphenTotal(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1t <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:14))
for(var in vars) {
  P1 <- RPCCnewphenTotal(paste("ModOut/Output_LHC_newphen/",var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1t <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Merge all and prep a table
################################################################################
# First, merge original (sagebrush) data:
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

# Second, merge the "total" data
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
  mutate(Parameter=factor(Parameter, levels=c("apheng","phen_winter","phen5g",
                                              "root_up","sla","pstemp_lo",
                                              "ltor_max","latosa"),
                          labels=c("phen\\textsubscript{max,grass}",
                                   "phen\\textsubscript{winter}",
                                   "GDD\\textsubscript{5,grass}",
                                   "root\\textsubscript{up}",
                                   "sla",
                                   "pstemp\\textsubscript{low}",
                                   "ltor\\textsubscript{max}",
                                   "latosa"), ordered=T))
t3

names(t3) <- c("Parameter","GPP","LAI","MASS","LAI","MASS")

t4 <- xtable(t3)
print(t4,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      sanitize.text.function = identity,
      file="figures/RPCCecosagenewphen.tex")

################################################################################
# Subset by parameters I care about:
# Get average |RPCC| and drop parameters <.2
# Format into pretty table ready for export (xtable, maybe try stargazer or knitr::kable)
# try tabular fxn in tables package?
################################################################################
# Ecosystem-level params: NEE, GPP
# Sage parms: fpc, lai, cmass, NPP
#################################################
# Table 1: NEE, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(mnee,site,Parameter) %>%
  spread(site,mnee) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  dplyr::select(Parameter,mean,fire:wbsec)
a1
# hehe my new parms matter lots

# Table 2: FPC, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(fpc,site,Parameter) %>%
  spread(site,fpc) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  dplyr::select(Parameter,mean,fire:wbsec)
a1

# Table 3: LAI, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(lai,site,Parameter) %>%
  spread(site,lai) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  dplyr::select(Parameter,mean,fire:wbsec)
a1

# Table 4: NPP, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(anpp,site,Parameter) %>%
  spread(site,anpp) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  dplyr::select(Parameter,mean,fire:wbsec)
a1

# Table 5: biomass, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(cmass,site,Parameter) %>%
  spread(site,cmass) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  dplyr::select(Parameter,mean,fire:wbsec)
a1
################################################################################
# Table of all parameters and average RPCC across sites and variables

t1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  dplyr::select(Parameter,site,mgpp,lai) %>%
  mutate_each(funs(a=abs),mgpp:lai) %>%
  group_by(Parameter) %>%
  summarise_each(funs(mean), mgpp:lai_a) %>%
  mutate(mean=rowMeans(.[,4:5])) %>%
  mutate(max=apply(.[,4:5],1,max)) %>%
  # cut rows where max < .2
  filter(max>=.2) %>%
  dplyr::select(Parameter, mean,mgpp:lai) %>%
  arrange(desc(mean)) %>%
  select(-mean) %>%
  mutate_each(funs(round(.,2)),mgpp:lai)

names(t1) <- c("Parameter","GPP","LAI")

t3 <- xtable(t1)
print(t3,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="figures/RPCCnewphen.tex")

################################################################################

################################################################################
# Pull in seasonal variables and look at those: GPP
################################################################################
seas <- c("spring","summer","fall")
sites <- c("wbsec","losec","h08ec","mbsec")
dat1 <- data.frame(ID=seq(1:14))
d <- RPCCseasphen("ModOut/Output_LHC_newphen/mgpp.txt")
d %>% group_by(Lon) %>%
  summarise(mean=mean(spring))
for(sea in seas) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- d %>% filter(Lon==lon) %>%
      dplyr::select(spring:sla) %>%
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
                fall_wbsec,fall_losec,fall_h08ec,fall_mbsec) %>%
  ungroup() %>%
  mutate(Parameter=factor(Parameter, levels=c("apheng","phen_winter","phen5g",
                                              "sla","pstemp_lo","root_up","aphenmax"),
                          labels=c("phen\\textsubscript{max,grass}",
                                   "phen\\textsubscript{winter}",
                                   "GDD\\textsubscript{5,grass}",
                                   "sla",
                                   "pstemp\\textsubscript{low}",
                                   "root\\textsubscript{up}",
                                   "phen\\textsubscript{max,sage}"),
                          ordered=T))

dat3 <- xtable(dat2)
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c("Parameter & WBS & LOS & PFS & MBS &
                      WBS & LOS & PFS & MBS &
                      WBS & LOS & PFS & MBS \\\\") # need 4 \ to get 2 in output

print(dat3,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      add.to.row = addtorow,
      booktabs = TRUE,
      type="latex",
      sanitize.text.function = identity,
      file="figures/RPCC_seas_newphen.tex")

################################################################################
# Look at RPCC for month of peak GPP (also month exceed then drop below 20% max)
################################################################################

################################################################################
# Pull in monthly GPP and look at those
################################################################################
mo <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Nov","Dec")
dat1 <- data.frame(ID=seq(1:14))
d <- RPCCmonthphen("ModOut/Output_LHC_newphen/mgpp.txt")
for(month in mo) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- d %>% filter(Lon==lon) %>%
      dplyr::select(Jan:sla) %>%
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
dat2 %>% select(Parameter:max)
# Interesting! No parameter has RPCC > .2
# Wait... with downramp, now they do???

# Re-do this for LAI to see if it differs:--------------------------------------
dat1b <- data.frame(ID=seq(1:14))
dblai <- RPCCmonthphen("ModOut/Output_LHC_newphen/mlai.txt")
for(month in mo) {
  for(site in sites) {
    if(site=="mbsec"){lon <- -116.7486}
    if(site=="losec"){lon <- -116.7356}
    if(site=="wbsec"){lon <- -116.7132}
    if(site=="h08ec"){lon <- -116.7231}
    d1 <- dblai %>% filter(Lon==lon) %>%
      dplyr::select(Jan:sla) %>%
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
# > .2 by mean: several!
# downramp has minimal effect- perhaps because grass swamps signal?

lai <- dat2b %>% select(Parameter:max) %>%
  rename(lai_mean=mean,lai_max=max)
gpp <- dat2 %>% select(Parameter:max) %>%
  rename(gpp_mean=mean,gpp_max=max)

merge(lai,gpp,by="Parameter",all=T) %>%
  arrange(desc(gpp_mean))


################################################################################
# Calculate RPCC for each variable and each site
# Make a couple of tables to show results:
################################################################################
library(xtable)

# Set working directory to access scripts and data:
setwd("~/Documents/SageParm/automate_tests/merged")

# Pull in RPCC function:
source("../Rcode/fxn_RPCC.R")

# Loop through variables and calculate RPCC
vars <- c("mrh","mra","mpet","mnpp","mnee","mgpp","mevap","maet",
          "fpc","lai","anpp","dens","cmass")

# site 1: mbsec -------------------------
mbsec <- data.frame(ID=seq(1:12))
for(var in vars) {
  P1 <- RPCC(paste(var,".txt",sep=""), "mbsec")
  names(P1) <- var
  mbsec <- cbind(mbsec,P1)
}
mbsec1 <- rownames_to_column(mbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="mbsec")

# site 2: losec -------------------------
losec <- data.frame(ID=seq(1:12))
for(var in vars) {
  P1 <- RPCC(paste(var,".txt",sep=""), "losec")
  names(P1) <- var
  losec <- cbind(losec,P1)
}
losec1 <- rownames_to_column(losec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="losec")

# site 3: wbsec -------------------------
wbsec <- data.frame(ID=seq(1:12))
for(var in vars) {
  P1 <- RPCC(paste(var,".txt",sep=""), "wbsec")
  names(P1) <- var
  wbsec <- cbind(wbsec,P1)
}
wbsec1 <- rownames_to_column(wbsec, "Parameter") %>% dplyr::select(-ID) %>%
  mutate(site="wbsec")

# site 4: fire -------------------------
fire <- data.frame(ID=seq(1:12))
for(var in vars) {
  P1 <- RPCC(paste(var,".txt",sep=""), "138h08ec")
  names(P1) <- var
  fire <- cbind(fire,P1)
}
fire1 <- rownames_to_column(fire, "Parameter") %>% dplyr::select(-ID) %>% 
  mutate(site="fire")

################################################################################
# Subset by parameters I care about:
# Get average |RPCC| and drop parameters <.2
# Format into pretty table ready for export (xtable, maybe try stargazer or knitr::kable)
# try tabular fxn in tables package?
################################################################################

#library(stargazer)

# Ecosystem-level params: NEE, GPP
# SAge parms: fpc, lai, cmass, NPP

#################################################
# Table 1: NEE, compare parm and rank across 4 sites
a1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(mnee,site,Parameter) %>%
  spread(site,mnee) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
a1
# NO parameter affects NEE!

#################################################
# Table 2: GPP, compare parm and rank across 4 sites
b1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(mgpp,site,Parameter) %>%
  spread(site,mgpp) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
b1
# SLA > root_up > ltor_max

#################################################
# Table 3: NPP, compare parm and rank across 4 sites
c1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(mnpp,site,Parameter) %>%
  spread(site,mnpp) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
c1
# root_up > SLA > ltor_max

#################################################
# Table 4: FPC, compare parm and rank across 4 sites
d1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(fpc,site,Parameter) %>%
  spread(site,fpc) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
d1
# SLA > root_up > ltor_max > latosa > turnover_sap > est_max

#################################################
# Table 5: LAI, compare parm and rank across 4 sites
e1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(lai,site,Parameter) %>%
  spread(site,lai) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
e1
# SLA > root_up > ltor_max > latosa

#################################################
# Table 6: Cmass, compare parm and rank across 4 sites
f1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(cmass,site,Parameter) %>%
  spread(site,cmass) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
f1
# SLA > root_up > ltor_max > latosa > turnover_sap

#################################################
# Table 7: NPPsage, compare parm and rank across 4 sites
g1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(anpp,site,Parameter) %>%
  spread(site,anpp) %>%
  mutate_each(funs(a=abs), -Parameter) %>%
  mutate(mean=abs(rowMeans(.[,6:9]))) %>%
  arrange(desc(mean)) %>%
  select(Parameter,mean,fire:wbsec)
round(g1[,2:6],2)
# SLA > root_up > ltor_max

################################################################################
# Table of all parameters and average RPCC across sites and variables
t1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(Parameter,site,mnee,mgpp,fpc,lai,anpp,cmass) %>%
  mutate_each(funs(a=abs),mnee:cmass) %>%
  group_by(Parameter) %>%
  summarise_each(funs(mean)) %>%
  mutate(mean=rowMeans(.[,9:14])) %>%
  select(Parameter, mean,mnee:cmass) %>%
  arrange(desc(mean)) %>%
  mutate_each(funs(round(.,2)),mean:cmass)

t1 <- rbind.data.frame(mbsec1,losec1,wbsec1,fire1) %>%
  select(Parameter,site,mnee,mgpp,fpc,lai,anpp,cmass) %>%
  mutate_each(funs(a=abs),mnee:cmass) %>%
  group_by(Parameter) %>%
  summarise_each(funs(mean)) %>%
  mutate(mean=rowMeans(.[,9:14])) %>%
  mutate(max=apply(.[,9:14],1,max)) %>%
  # cut rows where max < .2
  filter(max>=.2) %>%
  select(Parameter, mean,mnee:cmass) %>%
  arrange(desc(mean)) %>%
  mutate_each(funs(round(.,2)),mean:cmass)

names(t1) <- c("Parameter","mean","NEE","GPP","FPC","LAI","NPP","CMASS")
  
t3 <- xtable(t1)
print(t3,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="../../figures/tblout.tex")

################################################################################
# Make table just for biomass
f1
f2 <- f1[1:5,]
names(f2) <- c("Parameter", "mean", "burn","losec","mbsec","wbsec")
f3 <- select(f2, Parameter,mean,wbsec,losec,burn,mbsec) %>%
  mutate(mean=rowMeans(.[,3:6])) %>%
  mutate(max=apply(abs(.[,3:6]),1,max)) %>%
  # cut rows where max < .2
  filter(max>=.2) %>%
  select(-max) %>%
  mutate_each(funs(round(.,2)),mean:mbsec)

f4 <- xtable(f3)
print(f4,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=TRUE,
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="../../figures/cmass.tex")

################################################################################
# Pull in seasonal variables and look at those
################################################################################
seas <- c("spring","summer","fall")
sites <- c("wbsec","losec","h08ec","mbsec")
dat1 <- data.frame(ID=seq(1:12))
d <- RPCCseas("mgpp.txt")
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
  R <- pcc(X=d1[,1:12], y=d1[,14], rank=T)
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
  select(Parameter,spring_wbsec,spring_losec,spring_h08ec,spring_mbsec,
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

"& \multicolumn{4}{c}{Spring} & \multicolumn{4}{c}{Summer} & 
                      \multicolumn{4}{c}{Fall} \\",
		"\cmidrule(lr){2-5} \cmidrule(lr){6-9} \cmidrule(lr){10-13}",
                      
print(dat3,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      add.to.row = addtorow,
      booktabs = TRUE,
      type="latex",
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="../../figures/RPCC_seas.tex")

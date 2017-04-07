################################################################################
# Pull in and format merged data
# export results
# look at model sensitivity to each parameter
# calculate RMSE for each parameter set
# select top 10 sets and extract parameter ranges
################################################################################
library(tidyverse)
library(data.table) # for fread function (fast for big data)
library(ppcor) # for partial correlations

# Set working directory
setwd("~/Documents/SageParm/automate_tests/merged")

# read in NEE data
d <- fread("mnee.txt",header=T)
head(d)
names(d)[16] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
d1 <- d %>% filter(Year>=1154) %>%
  mutate(Year=Year+860)

d2 <- d1 %>% separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                                 "root_up","turnover_sap","pstemp_min",
                                 "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
                                 sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  # calculate annual NEE
  gather(Month,NEE, Jan:Dec) %>%
  group_by(Year, parameter,value, row) %>%
  summarise(NEE_annual=sum(NEE))
head(d2)
dim(d2)
summary(d2$row)
d2$value <- as.numeric(d2$value)

# plot sensitivity to parameter values
ggplot(data=d2[d2$Year==2015,], aes(x=value, y=NEE_annual)) +
  geom_point() +
  stat_smooth(method="lm") +
  facet_wrap(~parameter, scales="free") +
  theme_bw()

################################################################################
# Calculate partial correlation coefficients btwn parms and:
# 2015 annual: NEE, GPP, NPP
# 2015 Sagebrush: LAI, FPC, CMASS, NPP
################################################################################
# Calculate partial correlation coefficients btwn parms and annual NEE
# get matrix of relevant variables
d3 <- d2 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  ungroup() %>%
  dplyr::select(NEE_annual:turnover_sap)

# PCCs
p1 <- pcor(d3, method="spearman")
pval <- p1$p.value[,1]
round(pval,2) 
#Significant at p<.01: latosa, pstemp_min, root_up, sla, turnover_sap

cor <- p1$estimate[,1]
round(cor,3)
# cor above |.1|: latosa, root_up, sla, turnover_sap (lost pstemp_min)

# Calculate for GPP----------------------------------
a <- fread("mgpp.txt",header=T)
names(a)[16] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
a1 <- a %>% filter(Year>=1154) %>%
  mutate(Year=Year+860) %>% 
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                                   "root_up","turnover_sap","pstemp_min",
                                   "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
                      sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  gather(Month,GPP, Jan:Dec) %>%
  group_by(Year, parameter,value, row) %>%
  summarise(GPP_annual=sum(GPP))
a1$value <- as.numeric(a1$value)
a2 <- a1 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  ungroup() %>%
  dplyr::select(GPP_annual:turnover_sap)

# PCCs
GPP <- pcor(a2, method="spearman")
pval <- GPP$p.value[,1]
round(pval,2) 
#Significant at p<.01: est_max, ltor_max, pstemp_min, root_up, sla

cor_GPP <- GPP$estimate[,1]
round(cor_GPP,3)
# cor above |.1|: ltor_max, root_up, sla

# Correlations for NPP ------------------------------------------------
b <- fread("mnpp.txt",header=T)
names(b)[16] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
b1 <- b %>% mutate(Year=Year+860) %>% 
  filter(Year>=1986) %>%
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  gather(Month,NPP, Jan:Dec) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(Year, parameter,value, row) %>%
  summarise(NPP_annual=sum(NPP)) %>%
  group_by(parameter,value,row) %>%
  summarise(mean=mean(NPP_annual))
  spread(parameter,value) %>%
  ungroup() %>%
  dplyr::select(NPP_annual:turnover_sap)

# PCCs
NPP <- pcor(b2, method="spearman")
pval <- NPP$p.value[,1]
round(pval,2) 
#Significant at p<.01: ltor_max, pstemp_min, root_up, sla, turnover_sap

cor_NPP <- NPP$estimate[,1]
round(cor_NPP,3)
# cor above |.1|: ltor_max, root_up, sla

############################### SAGE-SPECIFIC VARIABLES
# Calculate PCC for LAI ---------------------------------------------------
c <- fread("lai.txt",header=T)
names(c)[8] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
c1 <- c %>% filter(Year>=1154) %>%
  mutate(Year=Year+860) %>% 
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name, -C3, -C4, -Total)

c1$value <- as.numeric(c1$value)
c2 <- c1 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  dplyr::select(ARTR:turnover_sap, -row)

# PCCs
LAI <- pcor(c2, method="spearman")
pval <- LAI$p.value[,1]
round(pval,2) 
#Significant at p<.01: est_max, latosa, ltor_max, pstemp_min, root_up, sla, turnover_sap

cor_LAI <- LAI$estimate[,1]
round(cor_LAI,3)
# cor above |.1|: ltor_max, root_up, sla

# Calculate PCC for FPC ---------------------------------------------------
e <- fread("fpc2.txt",header=T, fill=T, quote='')[,1:8]
names(e)[8] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
e1 <- e %>% filter(Year>=1154) %>%
  mutate(Year=Year+860) %>% 
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name, -C3, -C4, -Total)

e1$value <- as.numeric(e1$value)
e2 <- e1 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  dplyr::select(ARTR:turnover_sap, -row)

# PCCs
FPC <- pcor(e2, method="spearman")
pval <- FPC$p.value[,1]
round(pval,2) 
#Significant at p<.01: est_max, latosa, ltor_max, root_up, sla, turnover_sap

cor_FPC <- FPC$estimate[,1]
round(cor_FPC,3)
# cor above |.1|: est_max, latosa, ltor_max, root_up, sla, turnover_sap

# Calculate PCC for CMASS ---------------------------------------------------
f <- fread("cmass.txt",header=T)
names(f)[8] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
f1 <- f %>% filter(Year>=1154) %>%
  mutate(Year=Year+860) %>% 
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name, -C3, -C4, -Total)

f1$value <- as.numeric(f1$value)
f2 <- f1 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  dplyr::select(ARTR:turnover_sap, -row)

# PCCs
CMASS <- pcor(f2, method="spearman")
pval <- CMASS$p.value[,1]
round(pval,2) 
#Significant at p<.01: latosa, ltor_max, pstemp_min, root_up, sla, turnover_sap

cor_CMASS <- CMASS$estimate[,1]
round(cor_CMASS,3)
# cor above |.1|: latosa, ltor_max, root_up, sla, turnover_sap

# Calculate PCC for NPP ---------------------------------------------------
g <- fread("anpp.txt",header=T)
names(g)[8] <- "file"

# cut out all years except 2014-2015 (speeds subsequent manipulation)
g1 <- g %>% filter(Year>=1154) %>%
  mutate(Year=Year+860) %>% 
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name, -C3, -C4, -Total)

g1$value <- as.numeric(g1$value)
g2 <- g1 %>% filter(Year==2015) %>%
  spread(parameter,value) %>%
  dplyr::select(ARTR:turnover_sap, -row)

# PCCs
NPP <- pcor(g2, method="spearman")
pval <- NPP$p.value[,1]
round(pval,2) 
#Significant at p<.01: est_max, ltor_max, pstemp_min, root_up, sla

cor_NPP <- NPP$estimate[,1]
round(cor_NPP,3)
# cor above |.1|: ltor_max, root_up, sla

# Test if sensitivity package gets same values
library(sensitivity)
new <- pcc(X=g2[,2:13], y=g2[,1], rank=T)
new$PRCC
bb <- round(new$PRCC,3)
aa <- round(cor_NPP,3)
cbind(aa[2:13],bb) # Exactly the same!
print(new)

################################################################################
# Calculate average correlation coefficient for several responses each variable
# Responses: Sage NPP, LAI, CMASS; ecosystem 



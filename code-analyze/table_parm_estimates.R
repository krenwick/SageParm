################################################################################
# Get optimized paramaters from model objects
# Build a table comparing these to original parameters
################################################################################
rm(list=ls())
library(DEoptim)
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)
library(xtable)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm/")
object1 <- "HyaliteOutput/DE1parimage_monthgl_summergrass.RData"
object2 <- "HyaliteOutput/NewPhenImage_mgl_summergrass.RData"

# List parameters estimated in round 1:
parms <- c("SLA","latosa","ltor_max","root_up","pstemp_min",
           "pstemp_low","pstemp_max","GDD5")

# List original values for parameters:
orig <- c(30,6000,1,.6,-4,10,38,200)

# Get parameter values from optim 1:
load(object1)
pars1 <- cbind.data.frame(parms,orig,DE1$optim$bestmem) %>%
  #mutate_if(is.numeric,funs(round(.,1))) %>%
  rename(Estimate=`DE1$optim$bestmem`)

# Get parameter values from optim 2:
parms <- c("SLA","latosa","ltor_max","root_up","phen_winter","pstemp_low",
            "pstemp_min","apheng","aphensage","GDD5g")
load(object2)
pars2 <- cbind.data.frame(parms,DE1$optim$bestmem) %>%
  #mutate_if(is.numeric,funs(round(.,1))) %>%
  rename(Estimate2=`DE1$optim$bestmem`)

# Merge both into 1 table:
all <- merge(pars1,pars2,by="parms",all=T) %>%
  mutate(Parameter=factor(parms, levels=c("SLA","ltor_max","root_up","GDD5",
              "latosa","pstemp_min","pstemp_low","pstemp_max","phen_winter",
             "aphensage","apheng","GDD5g"), ordered=T, labels=c("sla",
              "ltor\\textsubscript{max}","root\\textsubscript{up}",
              "GDD\\textsubscript{5,sage}","latosa",
              "pstemp\\textsubscript{min}","pstemp\\textsubscript{low}",
              "pstemp\\textsubscript{max}","phen\\textsubscript{winter}",
              "phen\\textsubscript{max,sage}","phen\\textsubscript{max,grass}",
              "GDD\\textsubscript{5,grass}"))) %>%
  mutate_at(vars(orig:Estimate2),funs(ifelse(abs(.)>1,round(.),round(.,1)))) %>%
  dplyr::select(Parameter,Original=orig,`Optimal Parameters`=Estimate,`New Phenology`=Estimate2)
all

# Add original value for GDD5 grass:
all[11,2] <- 100

# pretty table
digits <- matrix
t <- xtable(all, digits=1)
print(t)
matrix(c(0,0,1,0,0,0,1,0,0,0,0,1), nrow = 12, ncol = 3)
digits(t) <- matrix(c(0,0,1,0,0,0,1,0,0,0,0,1), nrow = 12, ncol(t)+1)
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      sanitize.text.function = identity,
      file="figures/parm_estimates.tex")

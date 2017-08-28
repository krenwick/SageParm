################################################################################
# Function to: 
# 1. Read in data from LHC model runs
# 2. Extract years 1986-2015, calculate averages, and select needed variables
# 3. Calculate Ranked Partial Correlation Coefficients
################################################################################
library(tidyverse)
library(data.table) # for fread function (fast for big data)
library(sensitivity) # for partial correlations

RPCC <- function(data,site){
  if(site=="mbsec"){lon <- -116.7486}
  if(site=="losec"){lon <- -116.7356}
  if(site=="wbsec"){lon <- -116.7132}
  if(site=="138h08ec"){lon <- -116.7231}
  
  b <- fread(data,header=T)
  
  # For monthly data:
  if(ncol(b)==16){
    names(b)[16] <- "file"
  }

  # For annual (sagebrush) data:
  if(ncol(b)<=9){
    b=b[,1:7] # deal with 9th column in FPC due to fucked up merge
    names(b)[7] <- "file"
  }

    # select appropriate years (1986-2015)
    b1 <- b %>% mutate(Year=Year+860) %>% 
      filter(Year>=1986, Lon==lon) %>%
      mutate(id1=file) %>%
      separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
      dplyr::select(-cut) %>%
      separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                            "root_up","turnover_sap","pstemp_min",
                            "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                            "k_chillb","phengdd5"),
               sep = "_",extra="drop") %>%
      separate(phengdd5, into=c("phengdd5","del"), sep=-5) %>%
      dplyr::select(-del) %>%
      mutate(row=seq(1:nrow(.))) %>%
      gather(parameter,value, sla:phengdd5) %>%
      separate(value, into=c("name","value"), sep=5, extra="drop") %>%
      dplyr::select(-name) %>%
      mutate(value=as.numeric(value)) %>%
      {if(ncol(b)==16) { # Get annual average if data monthly
        gather(.,Month,N, Jan:Dec) %>%
        group_by(Year, parameter,value, row, id) %>%
        summarise(N_annual=sum(N))
          } else .} %>%
      {if(ncol(b)==7) { # If data are annual
        dplyr::select(., -C3, -Total) %>%
        rename(N_annual=ARTR)
      } else .} %>%
      group_by(parameter,value,id) %>%
      summarise(mean=mean(N_annual)) %>%
      spread(parameter,value) %>%
      ungroup() %>%
      dplyr::select(mean:turnover_sap)

  # Calculate PRCC
  R <- pcc(X=b1[,2:15], y=b1[,1], rank=T)
  R$PRCC
}

RPCCnewphen <- function(data,site){
  if(site=="mbsec"){lon <- -116.7486}
  if(site=="losec"){lon <- -116.7356}
  if(site=="wbsec"){lon <- -116.7132}
  if(site=="138h08ec"){lon <- -116.7231}
  b <- fread(data,header=T)
  
  # For monthly data:
  if(ncol(b)==16){
    names(b)[16] <- "file"
  }
  
  # For annual (sagebrush) data:
  if(ncol(b)<=9){
    b=b[,1:7] # deal with 9th column in FPC due to fucked up merge
    names(b)[7] <- "file"
  }
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986, Lon==lon) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","ltor_max","root_up","latosa",
                          "pstemp_lo","pstemp_min",
                          "pstemp_max", "apheng", "phengdd5","phen_winter", 
                          "aphenmax","downramp","downg", "phen5g"),
             sep = "_",extra="drop") %>%
    separate(phen5g, into=c("phen5g","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phen5g) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    {if(ncol(b)==16) { # Get annual average if data monthly
      gather(.,Month,N, Jan:Dec) %>%
        group_by(Year, parameter,value, row, id) %>%
        summarise(N_annual=sum(N))
    } else .} %>%
    {if(ncol(b)==7) {
      dplyr::select(., -C3, -Total) %>%
        rename(N_annual=ARTR)
    } else .} %>%
    group_by(parameter,value,id) %>%
    summarise(mean=mean(N_annual)) %>%
    spread(parameter,value) %>%
    ungroup() %>%
    dplyr::select(mean:sla)
  
  # Calculate PRCC
  R <- pcc(X=b1[,2:15], y=b1[,1], rank=T)
  R$PRCC
}

RPCCseas <- function(data){
  b <- fread(data,header=T)
  names(b)[16] <- "file"

  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                          "root_up","turnover_sap","pstemp_min",
                          "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                          "k_chillb","phengdd5"),
             sep = "_",extra="drop") %>%
    separate(phengdd5, into=c("phengdd5","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phengdd5) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    gather(Month,N, Jan:Dec) %>%
    group_by(Year, parameter,value, row, id, Lon) %>%
    mutate(N_annual=sum(N)) %>%
    spread(Month,N) %>%
    mutate(spring=(Mar+Apr+May)/N_annual, summer=(Jun+Jul+Aug)/N_annual, 
           fall=(Sep+Oct+Nov)/N_annual) %>%
    group_by(parameter,value,id,Lon) %>%
    summarise(spring=mean(spring), summer=mean(summer), fall=mean(fall)) %>%
    spread(parameter,value) %>%
    ungroup() 
  b1
}

RPCCmonth <- function(data){
  b <- fread(data,header=T)
  names(b)[16] <- "file"
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                          "root_up","turnover_sap","pstemp_min",
                          "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                          "k_chillb","phengdd5"),
             sep = "_",extra="drop") %>%
    separate(phengdd5, into=c("phengdd5","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phengdd5) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    group_by(parameter,value,id,Lon) %>%
    summarise_each(funs(mean)) %>%
    spread(parameter,value) %>%
    ungroup() 
  b1
}

RPCCseasphen <- function(data){
  b <- fread(data,header=T)
  names(b)[16] <- "file"
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","ltor_max","root_up","latosa",
                          "pstemp_lo","pstemp_min",
                          "pstemp_max", "apheng",
                          "phengdd5","phen_winter", "aphenmax","downramp",
                          "downg","phen5g"),
             sep = "_",extra="drop") %>%
    separate(phen5g, into=c("phen5g","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phen5g) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    gather(Month,N, Jan:Dec) %>%
    group_by(Year, parameter,value, row, id, Lon) %>%
    mutate(N_annual=sum(N)) %>%
    spread(Month,N) %>%
    filter(N_annual!=0) %>%
    mutate(spring=(Mar+Apr+May)/N_annual, summer=(Jun+Jul+Aug)/N_annual, 
           fall=(Sep+Oct+Nov)/N_annual) %>%
    group_by(parameter,value,id,Lon) %>%
    summarise(spring=mean(spring), summer=mean(summer), fall=mean(fall)) %>%
    spread(parameter,value) %>%
    ungroup() 
  b1
}

RPCCmonthphen <- function(data){
  b <- fread(data,header=T)
  names(b)[16] <- "file"
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","ltor_max","root_up","latosa",
                          "pstemp_lo","pstemp_min",
                          "pstemp_max", "apheng",
                          "phengdd5","phen_winter", "aphenmax","downramp",
                          "downg","phen5g"),
             sep = "_",extra="drop") %>%
    separate(phen5g, into=c("phen5g","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phen5g) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    group_by(parameter,value,id,Lon) %>%
    summarise_each(funs(mean)) %>%
    spread(parameter,value) %>%
    ungroup() 
  b1
}

################################################################################
# GRASS functions part 1: original model
################################################################################
RPCCgrass <- function(data,site){
  if(site=="mbsec"){lon <- -116.7486}
  if(site=="losec"){lon <- -116.7356}
  if(site=="wbsec"){lon <- -116.7132}
  if(site=="138h08ec"){lon <- -116.7231}
  
  b <- fread(data,header=T)
  
  # For monthly data:
  if(ncol(b)==16){
    names(b)[16] <- "file"
  }
  
  # For annual (sagebrush) data:
  if(ncol(b)<=9){
    b=b[,1:7] # deal with 9th column in FPC due to fucked up merge
    names(b)[7] <- "file"
  }
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986, Lon==lon) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                          "root_up","turnover_sap","pstemp_min",
                          "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                          "k_chillb","phengdd5"),
             sep = "_",extra="drop") %>%
    separate(phengdd5, into=c("phengdd5","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phengdd5) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    {if(ncol(b)==16) { # Get annual average if data monthly
      gather(.,Month,N, Jan:Dec) %>%
        group_by(Year, parameter,value, row, id) %>%
        summarise(N_annual=sum(N))
    } else .} %>%
    {if(ncol(b)==7) { # If data are annual
      dplyr::select(., -ARTR, -Total) %>%
        rename(N_annual=C3)
    } else .} %>%
    group_by(parameter,value,id) %>%
    summarise(mean=mean(N_annual)) %>%
    spread(parameter,value) %>%
    ungroup() %>%
    dplyr::select(mean:turnover_sap)
  
  # Calculate PRCC
  R <- pcc(X=b1[,2:15], y=b1[,1], rank=T)
  R$PRCC
}

# Function for TOTAL LAI etc. (grass + sage)
RPCCtotal <- function(data,site){
  if(site=="mbsec"){lon <- -116.7486}
  if(site=="losec"){lon <- -116.7356}
  if(site=="wbsec"){lon <- -116.7132}
  if(site=="138h08ec"){lon <- -116.7231}
  
  b <- fread(data,header=T)
  
  # For monthly data:
  if(ncol(b)==16){
    names(b)[16] <- "file"
  }
  
  # For annual (sagebrush) data:
  if(ncol(b)<=9){
    b=b[,1:7] # deal with 9th column in FPC due to fucked up merge
    names(b)[7] <- "file"
  }
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986, Lon==lon) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                          "root_up","turnover_sap","pstemp_min",
                          "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                          "k_chillb","phengdd5"),
             sep = "_",extra="drop") %>%
    separate(phengdd5, into=c("phengdd5","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phengdd5) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    {if(ncol(b)==16) { # Get annual average if data monthly
      gather(.,Month,N, Jan:Dec) %>%
        group_by(Year, parameter,value, row, id) %>%
        summarise(N_annual=sum(N))
    } else .} %>%
    {if(ncol(b)==7) { # If data are annual
      dplyr::select(., -ARTR, -C3) %>%
        rename(N_annual=Total)
    } else .} %>%
    group_by(parameter,value,id) %>%
    summarise(mean=mean(N_annual)) %>%
    spread(parameter,value) %>%
    ungroup() %>%
    dplyr::select(mean:turnover_sap)
  
  # Calculate PRCC
  R <- pcc(X=b1[,2:15], y=b1[,1], rank=T)
  R$PRCC
}

RPCCnewphenTotal <- function(data,site){
  if(site=="mbsec"){lon <- -116.7486}
  if(site=="losec"){lon <- -116.7356}
  if(site=="wbsec"){lon <- -116.7132}
  if(site=="138h08ec"){lon <- -116.7231}
  b <- fread(data,header=T)
  
  # For monthly data:
  if(ncol(b)==16){
    names(b)[16] <- "file"
  }
  
  # For annual (sagebrush) data:
  if(ncol(b)<=9){
    b=b[,1:7] # deal with 9th column in FPC due to fucked up merge
    names(b)[7] <- "file"
  }
  
  # select appropriate years (1986-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986, Lon==lon) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","ltor_max","root_up","latosa",
                          "pstemp_lo","pstemp_min",
                          "pstemp_max", "apheng", "phengdd5","phen_winter", 
                          "aphenmax","downramp","downg", "phen5g"),
             sep = "_",extra="drop") %>%
    separate(phen5g, into=c("phen5g","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:phen5g) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
    {if(ncol(b)==16) { # Get annual average if data monthly
      gather(.,Month,N, Jan:Dec) %>%
        group_by(Year, parameter,value, row, id) %>%
        summarise(N_annual=sum(N))
    } else .} %>%
    {if(ncol(b)==7) {
      dplyr::select(., -C3, -ARTR) %>%
        rename(N_annual=Total)
    } else .} %>%
    group_by(parameter,value,id) %>%
    summarise(mean=mean(N_annual)) %>%
    spread(parameter,value) %>%
    ungroup() %>%
    dplyr::select(mean:sla)
  
  # Calculate PRCC
  R <- pcc(X=b1[,2:15], y=b1[,1], rank=T)
  R$PRCC
}


source('fun-gen-params-2.R')
source('fun-gendata-3.R')
source('fun-methods.R')
source('wrappers.R')

library("did2s")
#library(etwfe)
library("tidyr")
library("dplyr")
library("did")
library(data.table)
library(marginaleffects)
library(fixest)


# parse scen, nobs, nsims
# args = commandArgs(trailingOnly=TRUE)
# 
# scenario <- as.numeric(as.character(args[1]))
# nsims <- as.numeric(as.character(args[2]))
# sampsize <- as.numeric(as.character(args[3]))
# idx <- as.numeric(as.character(args[4]))

scenario <- 0
nsims <- 100
sampsize <- 100
idx <- 1

# submitted on July 26 (updated names and staggered lagged effect)
methods.sim <- c('did', 'did2s', 'sunab')
# methods.sim <- c('etwfe')

out.many <- do.many(scenario, methods.sim,
                    sampsize, nsims)

dir.create('Results')
newdir <- paste0('ResSim-scen', scenario)

if(!newdir %in% list.files('.')){
  dir.create(file.path('.','/Results/',newdir))
}

if('etwfe' %in% methods.sim){
  filename <- paste(paste(sampsize, scenario, nsims, idx, 'etwfe', sep = '-'),'.RData', sep = '')
}else{
  filename <- paste(paste(sampsize, scenario, nsims, idx, sep = '-'),'.RData', sep = '')
}

save(methods.sim, out.many, file = file.path('.','/Results/',newdir, filename))

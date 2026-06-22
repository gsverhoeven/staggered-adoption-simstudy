
do.one <- function(params.dat, params.methods, methods.sim){
  
  sim.data <- gen.data.cluster(params.dat) 
  
  # extract group and methods in this scenario 
  group <- params.methods$group.out 
  time <- params.methods$time.out 
  
  #### get data in long format 
  dat_long <- sim.data$long_dat 
  
  #### get parameters such as distance values, group, and time
  store_dist_vals <- sim.data$store_dist_vals
  group <- params.methods$group.out 
  time <- params.methods$time.out 
  
  strip_before_dot <- function(s) {
    return(sub(".*\\.", "", s))
  }
  params_methods_2 <- sapply(names(params.methods), strip_before_dot)
  
  all.res.grouptime <- list()
  all.res.group <- list()
  all.res.reltime <- list()
  all.res.marginal <- list()
  
  for(i in 1:length(methods.sim)){
    
    curr.method <- methods.sim[i]
    curr.fun <- paste(curr.method, '_wrapper', sep = '')
    idx.params <- as.numeric(which(params_methods_2 == curr.method))
    
    print(paste("Iteration:", i))
    print(paste("Current Method:", curr.method))
    print(paste("Index Params:", idx.params))
    print(paste("Params Methods 2:", params_methods_2))
    
    if(length(idx.params) > 0){
      print(paste("Running", curr.method))
      
      curr.output <- get(curr.fun)(dat_long, params.methods[[idx.params]], 
                                   group, time, store_dist_vals)
      
      # store results
      if("clean.res.grouptime" %in% names(curr.output)){
        if(length(curr.output$clean.res.grouptime) > 0) {
          curr.clean.res.grouptime <- do.call(rbind, curr.output$clean.res.grouptime)
          all.res.grouptime[[i]] <-  curr.clean.res.grouptime
        } else {
          print(paste("Warning: clean.res.grouptime is empty in", curr.method, "output."))
        }
      } else {
        print(paste("Warning: clean.res.grouptime not found in", curr.method, "output."))
      }
      if("clean.res.group" %in% names(curr.output)){
        if(length(curr.output$clean.res.group) > 0) {
          curr.clean.res.group <- do.call(rbind, curr.output$clean.res.group)
          all.res.group[[i]] <- curr.clean.res.group
        } else {
          print(paste("Warning: clean.res.group is empty in", curr.method, "output."))
        }
      } else {
        print(paste("Warning: clean.res.group not found in", curr.method, "output."))
      }
      if("clean.res.reltime" %in% names(curr.output)){
        if(length(curr.output$clean.res.reltime) > 0) {
          curr.clean.res.reltime <- do.call(rbind, curr.output$clean.res.reltime)
          all.res.reltime[[i]] <- curr.clean.res.reltime
        } else {
          print(paste("Warning: clean.res.reltime is empty in", curr.method, "output."))
        }
      } else {
        print(paste("Warning: clean.res.reltime not found in", curr.method, "output."))
      }
      if("clean.res.marginal" %in% names(curr.output)){
        if(length(curr.output$clean.res.marginal) > 0) {
          curr.clean.res.marginal <- do.call(rbind, curr.output$clean.res.marginal)
          all.res.marginal[[i]] <- curr.clean.res.marginal
          rownames(all.res.marginal[[i]]) <- NULL
        } else {
          print(paste("Warning: clean.res.marginal is empty in", curr.method, "output."))
        }
      } else {
        print(paste("Warning: clean.res.marginal not found in", curr.method, "output."))
      }
    } else {
      print(paste(curr.method, 'method not found'))
    }
  }
  
  all.res.grouptime.2 <- do.call(rbind, all.res.grouptime)
  all.res.group.2 <- do.call(rbind, all.res.group)
  all.res.reltime.2 <- do.call(rbind, all.res.reltime)
  all.res.marginal.2 <- do.call(rbind, all.res.marginal)
  
  return(list(all.res.grouptime = all.res.grouptime.2, 
              all.res.group = all.res.group.2, 
              all.res.reltime = all.res.reltime.2, 
              all.res.marginal = all.res.marginal.2))
}

do.one_2 <- function(params.dat, params.methods, methods.sim){
  
  sim.data <- gen.data.cluster(params.dat) 
  
  # extract group and methods in this scenario 
  group <- params.methods$group.out 
  time <- params.methods$time.out 
  
  #### get data in long format 
  dat_long <- sim.data$long_dat 
  
  #### get parameters such as distance values, group, and time
  store_dist_vals <- sim.data$store_dist_vals
  group <- params.methods$group.out 
  time <- params.methods$time.out 
  
  strip_before_dot <- function(s) {
    return(sub(".*\\.", "", s))
  }
  params_methods_2 <- sapply(names(params.methods), strip_before_dot)
  
  all.res.grouptime <- list()
  all.res.group <- list()
  all.res.reltime <- list()
  all.res.marginal <- list()
  
  for(i in 1:length(methods.sim)){
    
    curr.method <- methods.sim[i]
    curr.fun <- paste(curr.method, '_wrapper', sep = '')
    idx.params <- as.numeric(which(params_methods_2 == curr.method))
    
    print(paste("Iteration: ", i))
    print(paste("Current Method: ", curr.method))
    print(paste("Index Params: ", idx.params))
    print(paste("Params Methods 2: ", params_methods_2))
    
    if(length(idx.params) > 0){
      
      print(paste("Running", curr.method))
      
      curr.output <- get(curr.fun)(dat_long, params.methods[[idx.params]], 
                                   group, time, store_dist_vals)
      
      # store results
      if("clean.res.grouptime" %in% names(curr.output)){
        if(length(clean.res.grouptime))
        curr.clean.res.grouptime <- do.call(rbind, curr.output$clean.res.grouptime)
        all.res.grouptime[[i]] <-  curr.clean.res.grouptime
      }
      if("clean.res.group" %in% names(curr.output)){
        curr.clean.res.group <- do.call(rbind, curr.output$clean.res.group)
        all.res.group[[i]] <- curr.clean.res.group
      }
      if("clean.res.reltime" %in% names(curr.output)){
        curr.clean.res.reltime <- do.call(rbind, curr.output$clean.res.reltime)
        all.res.reltime[[i]] <- curr.clean.res.reltime
      }
      if("clean.res.marginal" %in% names(curr.output)){
        curr.clean.res.marginal <- do.call(rbind, curr.output$clean.res.marginal)
        all.res.marginal[[i]] <- curr.clean.res.marginal
        rownames(all.res.marginal[[i]]) <- NULL
      }
    }else{
      print(paste(curr.method , 'method not found'))
    }
  
  }

  all.res.grouptime.2 <- do.call(rbind, all.res.grouptime)
  all.res.group.2 <- do.call(rbind, all.res.group)
  all.res.reltime.2 <- do.call(rbind, all.res.reltime)
  all.res.marginal.2 <- do.call(rbind, all.res.marginal)
  
  return(list( all.res.grouptime = all.res.grouptime.2, 
               all.res.group = all.res.group.2, 
               all.res.reltime = all.res.reltime.2, 
               all.res.marginal = all.res.marginal.2))
}

do.many_2 <- function(scen, methods.sim, nobs, nsim){
  
  # where we store results
  all.res.grouptime <- list()
  all.res.group <- list()
  all.res.reltime <- list()
  all.res.marginal <- list()
  
  # generate parameters for data and covariates
  params.dat <- gen_params(scen, nobs)
  # for loop from 1 to n.sims
  for(i in 1:nsim){
    # generate parameters for method depending on the scenario
    params.methods <- set_methods(params.dat, scen)
    # call do one
    temp <- do.one(params.dat, params.methods, methods.sim)
    
    all.res.grouptime[[i]] <- temp$all.res.grouptime
    all.res.grouptime[[i]]$sim <- i 
    all.res.grouptime[[i]]$scen <- scen
    all.res.grouptime[[i]]$nobs <- nobs
    
    all.res.group[[i]] <- temp$all.res.group
    all.res.group[[i]]$sim <- i 
    all.res.group[[i]]$scen <- scen
    all.res.group[[i]]$nobs <- nobs
    
    all.res.reltime[[i]] <- temp$all.res.reltime
    all.res.reltime[[i]]$sim <- i 
    all.res.reltime[[i]]$scen <- scen
    all.res.reltime[[i]]$nobs <- nobs
    
    all.res.marginal[[i]] <- temp$all.res.marginal
    all.res.marginal[[i]]$sim <- i 
    all.res.marginal[[i]]$scen <- scen
    all.res.marginal[[i]]$nobs <- nobs
    
  }
  
  all.res.grouptime.2 <- do.call(rbind, all.res.grouptime)
  all.res.group.2 <- do.call(rbind, all.res.group)
  all.res.reltime.2 <- do.call(rbind, all.res.reltime)
  all.res.marginal.2 <- do.call(rbind,  all.res.marginal)
  
  return(list(all.res.grouptime = all.res.grouptime.2, 
              all.res.group = all.res.group.2, 
              all.res.reltime = all.res.reltime.2, 
              all.res.marginal  = all.res.marginal.2))
  
}

do.many <- function(scen, methods.sim, nobs, nsim) {
  
  # where we store results
  all.res.grouptime <- list()
  all.res.group <- list()
  all.res.reltime <- list()
  all.res.marginal <- list()
  
  # generate parameters for data and covariates
  params.dat <- gen_params(scen, nobs)
  
  # generate parameters for method depending on the scenario
  params.methods <- set_methods(params.dat, scen)
  
  # for loop from 1 to n.sims
  for (i in 1:nsim) {
    # call do one
    temp <- tryCatch(do.one(params.dat, params.methods, methods.sim), 
                     error = function(e) NULL)
    
    if (!is.null(temp)) {
      if (!is.null(temp$all.res.grouptime)) {
        all.res.grouptime[[i]] <- temp$all.res.grouptime
        all.res.grouptime[[i]]$sim <- i 
        all.res.grouptime[[i]]$scen <- scen
        all.res.grouptime[[i]]$nobs <- nobs
      }
      
      if (!is.null(temp$all.res.group)) {
        all.res.group[[i]] <- temp$all.res.group
        all.res.group[[i]]$sim <- i 
        all.res.group[[i]]$scen <- scen
        all.res.group[[i]]$nobs <- nobs
      }
      
      if (!is.null(temp$all.res.reltime)) {
        all.res.reltime[[i]] <- temp$all.res.reltime
        all.res.reltime[[i]]$sim <- i 
        all.res.reltime[[i]]$scen <- scen
        all.res.reltime[[i]]$nobs <- nobs
     }
      
     if (!is.null(temp$all.res.marginal)) {
        all.res.marginal[[i]] <- temp$all.res.marginal
        all.res.marginal[[i]]$sim <- i 
        all.res.marginal[[i]]$scen <- scen
        all.res.marginal[[i]]$nobs <- nobs
      }
    }
    
    print(paste("simulation number:", i, "finished"))
  }
  
  # Remove NULL entries before combining
  all.res.grouptime <- Filter(Negate(is.null), all.res.grouptime)
  all.res.group <- Filter(Negate(is.null), all.res.group)
  all.res.reltime <- Filter(Negate(is.null), all.res.reltime)
  all.res.marginal <- Filter(Negate(is.null), all.res.marginal)
  
  all.res.grouptime.2 <- do.call(rbind, all.res.grouptime)
  all.res.group.2 <- do.call(rbind, all.res.group)
  all.res.reltime.2 <- do.call(rbind, all.res.reltime)
  all.res.marginal.2 <- do.call(rbind, all.res.marginal)
  
  return(list(all.res.grouptime = all.res.grouptime.2, 
              all.res.group = all.res.group.2, 
              all.res.reltime = all.res.reltime.2, 
              all.res.marginal = all.res.marginal.2))
}



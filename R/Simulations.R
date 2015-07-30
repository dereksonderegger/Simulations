#registerDoParallel()

#' Run a simulation study 
#' 
#' Conduct a simulation study where we have a matrix
#' of parameter values that we are interested. For
#' each row in the parameter matrix, the simulation
#' function is called num.reps times and the output
#' of each run is saved to the simulation directory.
#' 
#' The simulation can be re-run with higher num.reps and
#' the original simulations (still stored in the simulation
#' directory) will be used and only the additional replications
#' will be run.  Likewise, if the simulation is interupted
#' (say a power outage...), the simulation can be re-started 
#' and the previous iterations will not be lost.
#' @param sim.function A function to run a single simulation.
#' @param params A data frame of parameter values. The order
#' of the columns must match the order of the arguments to the
#' sim.function.
#' @param num.reps The number of replicate simulations for each
#' parameter combination.
#' @param sim.directory The local directory to store the simulation
#' results.
#' @param prep.function A function to be run before each new parameter
#' combination is started.
#' @param verbose If information about what parameter combination and 
#' replicate number is about to be run.
#' @examples 
#' # Simulation is creating data and fitting 
#' # a regression model to estimate a slope.
#' Sim.Function <- function(N, alpha, beta, sigma){
#'   x <- runif(N, 0, 10)
#'   y <- alpha + beta*x + rnorm(N, 0, sigma)
#'   model <- lm(y~x)
#'   return(model)
#' }
#' 
#' # Create a matrix where each row represents a set of parameters
#' Params <- expand.grid(N=20, alpha=0, beta=c(0,1), sigma=c(.2, 1))
#' 
#' # Run the simulations.  
#' run.sims(Sim.Function, Params, num.reps=10)
#'
#' # Now that the simulations are run, we want to analyze
#' # them.  Create a function to be applied to each simulation
#' # that results in a single row.  Those rows will be concatenated
#' # together into a large data frame that summarizes the simulation.
#' 
#' # Because the output of the Sim.Function was a linear model output,
#' # that is the input for this function
#' Summary.Function <- function(model, params){
#'   beta.hat <- coef(model)[2]
#'   beta.lwr <- confint(model)[2,1]
#'   beta.upr <- confint(model)[2,2]
#'   return( data.frame(beta=beta.hat, lwr=beta.lwr, upr=beta.upr))
#' }
#' 
#' Sims <- summarize.sims(Summary.Function, Params)
#' @export
run.sims <- function( sim.function, param.matrix, 
                      num.reps, sim.directory='sim_directory', 
                      prep.function=NULL, verbose=FALSE ){

	num.sims <- dim(param.matrix)[1]

	# create the sim_directory if necessary
	if( !file.exists(sim.directory) ){ 
 		dir.create(sim.directory)
	}
	
  for(i in 1:num.sims){
    reps.to.do <- missing.reps(reps=num.reps, sim.num=i, sim.directory)
    if( length(reps.to.do) > 0 ){
      if(!is.null(prep.function)){
        do.call(prep.function, as.list(param.matrix[i,]))
      }
  		x <- foreach( j = reps.to.do ) %dopar% {
	  		try( sim <- do.call(sim.function, as.list(param.matrix[i,])) )
		  	if( class(sim) != 'try-error' ){
          save(sim, file=paste(sim.directory,'/sim',i,'rep',j, '.RData', sep=''))
        }else{
          # Should throw a warning that this simulation failed
          try( sim <- do.call(sim.function, as.list(param.matrix[i,])) )
          save(sim, file=paste(sim.directory,'/sim',i,'rep',j, '.RData', sep=''))        
        }
  		}  # for each rep
    } # if there are any reps to do
	} # for each simulation
} # function


  

#' Summarize a simulation study 
#' 
#' Summarize a simulation study where we have a matrix
#' of parameter values that we are interested. 
#' 
#' @param summary.function A function to summarize a single
#' simulation into a vector of output statistics.
#' @param params A data frame of parameter values. This should
#' be the same data frame as was used to create the simulation.
#' @param sim.directory The local directory to store the simulation
#' results.
#' @examples 
#' # Simulation is creating data and fitting 
#' # a regression model to estimate a slope.
#' Sim.Function <- function(N, alpha, beta, sigma){
#'   x <- runif(N, 0, 10)
#'   y <- alpha + beta*x + rnorm(N, 0, sigma)
#'   model <- lm(y~x)
#'   return(model)
#' }
#' 
#' # Create a matrix where each row represents a set of parameters
#' Params <- expand.grid(N=20, alpha=0, beta=c(0,1), sigma=c(.2, 1))
#' 
#' # Run the simulations.  
#' run.sims(Sim.Function, Params, num.reps=10)
#'
#' # Now that the simulations are run, we want to analyze
#' # them.  Create a function to be applied to each simulation
#' # that results in a single row.  Those rows will be concatenated
#' # together into a large data frame that summarizes the simulation.
#' 
#' # Because the output of the Sim.Function was a linear model output,
#' # that is the input for this function
#' Summary.Function <- function(model, params){
#'   beta.hat <- coef(model)[2]
#'   beta.lwr <- confint(model)[2,1]
#'   beta.upr <- confint(model)[2,2]
#'   return( data.frame(beta=beta.hat, lwr=beta.lwr, upr=beta.upr))
#' }
#' 
#' Sims <- summarize.sims(Summary.Function, Params)
#' @export
summarize.sims <- function(summary.function, Params, 
        sim.directory='./sim_directory', ...){
	files <- as.character( dir(path=sim.directory) )
	files <- data.frame(file = files,
	                    sim = get.sim.number(files),
	                    rep = get.rep.number(files))
	files <- files %>% arrange(sim, rep)
	
	out <- list()
	for(k in 1:nrow(files)){
	  i <- files$sim[k]
	  j <- files$rep[k]
		load(paste(sim.directory,'/sim',i,'rep',j,'.RData', sep=''))
		temp1 <- summary.function(sim)
		temp2 <- cbind( do.call(rbind, replicate(nrow(temp1), Params[i,], simplify = FALSE)),
		                 temp1 )
		out[[k]] <- temp2  
	}
	return( rbind_all(out) )
}


get.next.rep.num <- function(i, num.reps, sim.directory){
  files <- list.files(sim.directory)
  for(j in 1:num.reps){
    foo <- grep( paste('sim',i,'rep',j,'.RData',sep=''), files)
    if(length(foo)==0){
      return(j)
    }
  }
  return(num.reps+1)
}


get.sim <- function(sim=1, rep=1, sim.directory='sim_directory'){
  load(paste(sim.directory,'/sim',sim,'rep',rep,'.RData', sep=''))
  return(sim)
}

get.sim.number <- Vectorize(function( file ){
	temp <- strsplit(file, 'sim')
	temp <- strsplit(temp[[1]][2], 'rep')
	sim <- as.integer(temp[[1]][1])
	names(sim) <- NULL
	return(sim)
}, USE.NAMES=FALSE)

get.rep.number <- Vectorize(function( file ){
	temp <- strsplit(file, 'rep')
	temp <- strsplit(temp[[1]][2], '.RData')
	rep <- as.integer(temp[[1]][1])
	return(rep)
}, USE.NAMES=FALSE)
		
missing.reps <- function(reps, sim.num, sim.directory='./sim_directory'){
  files <- dir(sim.directory)
  if(length(files) == 0){
    out <- 1:reps
  }else{
    foo <- str_extract_all(files,"[0-9]+") 
    foo <- data.frame( do.call("rbind", foo) )
    foo[,1] <- as.integer(as.character(foo[,1]))
    foo[,2] <- as.integer(as.character(foo[,2]))
    colnames(foo) <- c('sim','rep')
    foo <- foo %>% 
      arrange(sim, rep) %>%
      filter(sim == sim.num)
    out <- which(!is.element(1:reps, foo$rep)) 
  }
  return(out)
}


#' Predict the relation of objects to a list of variables of interest
#'
#' By comparison of significant correlations within variables and the variables the objects
#' incorporate \code{predictvo()} makes an prediction about the relation to given variables
#' for every object. 
#' 
#' @param matrix data.frame with numeric values
#' @param reltable table of correlation values (e.g. produced by \code{reltable()})
#' @param mvars vector of variables of interest (full name)
#' @return table with predicted, normalized relation values of every object and given
#' variables of interest
#' 
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#'  
#' testcorr <- corrmat(testmatrixrand, "lambda", chi2limit = 0.1, dim = 1)
#' 
#' rel <- reltable(testcorr)
#' 
#' predictvo(testmatrixrand, rel, c("X1", "X2", "X3"))
#' 
#' @export
#'

predictvo <- function (matrix, reltable, mvars) {
  
  # loop: check relations of every variable of interest
  for (pointer in 1:length(mvars)){
    
    mvar <- mvars[pointer]
    
    # find variables, that are linked to the current variable of interest
    redtovar <- dplyr::filter(
      reltable, 
      namevar1 == mvar | namevar2 == mvar
    )
    
    # extract partner variables of the variable of interest (1. Level)
    withoutmvar <- c(
      redtovar[redtovar$namevar1 != mvar,]$namevar1, 
      redtovar[redtovar$namevar2 != mvar,]$namevar2
    )
    
   # extract partner variables of partner variables of interest (2. Level)
    mvarnet <- filter(
      reltable, 
      namevar1 == withoutmvar[1] | 
        namevar2 == withoutmvar[1]
    )
    for (i in 2:length(withoutmvar)) {
      mvarnet <- rbind(
        mvarnet, 
        filter (
          reltable, 
          namevar1 == withoutmvar[i] |
            namevar2 == withoutmvar[i]  
        )
      )
    }
    mvarvec <- c(mvarnet$namevar1, mvarnet$namevar2)
    
    # remove multiple values to get a simple list of 2. Level partner variables
    mvarvec <- unique(mvarvec)
    
    mvarrel <- c()
    # loop: check relation of the variable of interest with every object
    for (i in 1:length(matrix[,1])){
      # determine variables present in current object
      cur <- colnames(matrix)[as.logical(matrix[i,])]
      # compare variables present in current object with the list of 2. Level
      # partner variables. Count overlap
      mvarrel[i] <- length(mvarvec[mvarvec %in% cur])
    }
    
    # normalize overlap vector
    for (i in 1:length(mvarrel)){
      mvarrel[i] <- mvarrel[i]/max(mvarrel)
    }
    
    # write overlap vector into a data.frame to collect the information for
    # every variable of information in one table
    if (pointer == 1) {
      relvaluetable <- data.frame(mvarrel)
    } else {
      relvaluetable <- data.frame(relvaluetable, mvarrel)
    }
    
  }
  
  # adjust colnames of resulting data.frame
  colnames(relvaluetable) <- paste(mvars, "PREDICTION")
  
  return(relvaluetable)
  
}
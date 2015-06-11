#' Delete rows and columns of a data.frame by amount of values > 0 
#'
#' A data.frame with numeric values often contains rows and columns with an insufficient 
#' amount of values > 0 for a certain task. For example Correspondence Analysis or 
#' Bivariate Correlation Analysis requires a minimum amount of usable values. 
#' In an archaeological context this could apply for example, if certain find categories
#' are particularly rare in burial site context.
#' \code{delrc} allows to remove rows and columns, that don't fulfill the requirements.
#' It's an extension of the idea of \code{varnastats::delempty()}
#'
#' @param matrix data.frame with numeric values
#' @param climit numeric value: How many values > 0 have to be present to consider a 
#' column sufficiently linked to perform further analysis? Every column with less values > 0 
#' will be removed. 
#' 
#' default = 0 (no column will be deleted)
#' 
#' @param rlimit numeric value: How many values > 0 have to be present to consider a 
#' row sufficiently linked to perform further analysis? Every row with less values > 0 
#' will be removed. 
#' 
#' default = 0 (no row will be deleted)
#' 
#' @return data.frame without removed rows/colums
#'
#' @examples
#' testmatrix <- data.frame(c1 = c(0,3,8,2), c2 = c(0,6,7,8), c3 = c(0,0,0,0))
#' 
#' delrc(testmatrix, climit = 3, rlimit = 1)
#'  
#' @export
#' 

delrc <- function(matrix, climit = 0, rlimit = 0) {
  
  if (climit > length(matrix[1,])) {
    climit <- length(matrix[1,])
  }
  
  if (rlimit > length(matrix[,1])) {
    rlimit <- length(matrix[,1])
  }
  
  # search empty columns and save index in a vector 
  cdel <- function(matrix, climit) {
    n <- c()
    p <- 1
    if (!(length(matrix[,1]) == 0 | length(matrix[,1]) == 0)) {
      for (s in 1:length(matrix[1,])) {
        if (length(which(matrix[,s] > 0)) < climit) {
          n[p] <- s
          p <- p + 1
        }
      }
    }
    return(n)
  }
  
  # search empty rows and save index in a vector
  rdel <- function(matrix, rlimit) {
    z <- c()
    i <- 1
    u <- 1
    if (!(length(matrix[,1]) == 0 | length(matrix[,1]) == 0)) {
      for (i in 1:length(matrix[,1])) {
        if (length(which(matrix[i,] > 0)) < rlimit) {
          z[u] <- i
          u <- u + 1
        }
      }
    }
    return(z)
  }

  while (length(matrix[,1]) > 0 && 
         length(matrix[1,]) > 0 && 
         (!is.null(cdel(matrix, climit)) ||
         !is.null(rdel(matrix, rlimit)))
         ) {
    
    n <- cdel(matrix, climit)
    z <- rdel(matrix, rlimit)
    
    # delete colums
    if (!is.null(n) && length(matrix[1,]) >= max(n))  {
      matrix <- matrix[,-n]
    } 
    
    # delete rows
    if (!is.null(z) && length(matrix[,1]) >= max(z)) {
      matrix <- matrix[-z,]
    } 
    
  }
  
  return(matrix)
  
 

}
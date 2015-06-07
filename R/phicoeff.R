#' Create a phi coefficient correlation matrix of an input data.frame
#'
#' \code{phicoeff} returns a correlation matrix of a data.frame. The correlation value
#' is the phi coefficient ("mean square contingency coefficient") by Karl Pearson. 
#' The correlation matrix can be created for column or row relations of the input 
#' data.frame. 
#'
#' @param matrix data.frame with numeric values
#' @param dim switch to define, whether the correlation matrix should be created 
#' for columns or rows. 
#' 1 (default): table is created for column (variables) relations.
#' 2: table is created for row (objects) relations.
#' @return phi coefficient correlation matrix
#' 
#'@examples
#' testmatrixrand <- data.frame(
#' matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#' 
#' # correlation table is created for the columns of the input data.frame testmatrixrand
#' phicoeff(testmatrixrand)
#' phicoeff(testmatrixrand, 1)
#' phitabcolumns <- phicoeff(matrix = testmatrixrand, dim = 1)
#'
#' # correlation table is created for the rows of the input data.frame testmatrixrand    
#' phicoeff(testmatrixrand, 2)
#' phitabrows <- phicoeff(matrix = testmatrixrand, dim = 2)
#' 

phicoeff <- function (matrix, dim = 1) {
  
  # create empty correlation table for the input data.frame
  corrtab <- varnastats:::newcorrtable(matrix, dim)
  
  if (dim == 1) {
    # loop to compare every column with every other column
    for (z in 1:length(matrix)) { 
      for (s in 1:length(matrix)) {
        # calculation of a 2*2 contingency table for the current column-column relation
        tbl = table(matrix[,z], matrix[,s])
        # perform chi-square test for the current relation
        x <- chisq.test(tbl) 
        # calculation of phi = √(chi2/n)
        # -> chi2 is the chisquare value
        # -> n is the sum of the 2*2 contingency table
        corrtab[z,s] <- sqrt(unlist(x[1])/sum(tbl))
      } 
    }
  }
  
  if (dim == 2) {
    # loop to compare every row with every other row
    # other comments: see above
    for (z in 1:length(matrix[,1])) { 
      for (s in 1:length(matrix[,1])) {
        tbl = table(unlist(matrix[z,]), unlist(matrix[s,]))
        x <- chisq.test(tbl) 
        corrtab[z,s] <- sqrt(unlist(x[1])/sum(tbl))
      } 
    }
  }

  return(corrtab) 
}
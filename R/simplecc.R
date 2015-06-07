#----------------------------------------
#' Create a Pearson's contingency coefficient correlation matrix of an input data.frame
#'
#' \code{simplecc} returns a correlation matrix of a data.frame. The correlation value
#' is Pearson's contingency coefficient. 
#' The correlation matrix can be created for column or row relations of the input 
#' data.frame. 
#'
#' @param matrix data.frame with numeric values
#' @param dim switch to define, whether the correlation matrix should be created 
#' for columns or rows. 
#' 1 (default): table is created for column (variables) relations.
#' 2: table is created for row (objects) relations.
#' @return contingency coefficient correlation matrix
#' 
#'@examples
#' testmatrixrand <- data.frame(
#' matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#' 
#' # correlation table is created for the columns of the input data.frame testmatrixrand
#' simplecc(testmatrixrand)
#' simplecc(testmatrixrand, 1)
#' cccolumns <- simplecc(matrix = testmatrixrand, dim = 1)
#'
#' # correlation table is created for the rows of the input data.frame testmatrixrand    
#' simplecc(testmatrixrand, 2)
#' ccrows <- simplecc(matrix = testmatrixrand, dim = 2)
#' 
#' @export
#' 

simplecc <- function (matrix, dim=1) {
  
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
        # calculation of CC = √(chi2/(chi2+n))
        # -> chi2 is the chisquare value
        # -> n is the sum of the 2*2 contingency table
        corrtab[z,s] <- sqrt(unlist(x[1])/(unlist(x[1])+sum(tbl)))
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
        corrtab[z,s] <- sqrt(unlist(x[1])/(unlist(x[1])+sum(tbl)))
      } 
    }
  }

  return(corrtab) 
}
#----------------------------------------
#' Create a correlation matrix with Goodman and Kruskal's lambda value of an input data.frame
#'
#' \code{lambdasym} returns a correlation matrix of a data.frame. The correlation value
#' is Goodman and Kruskal's lambda. 
#' The correlation matrix can be created for column or row relations of the input 
#' data.frame. 
#'
#' @param matrix data.frame with numeric values
#' @param dim switch to define, whether the correlation matrix should be created 
#' for columns or rows. 
#' 1 (default): table is created for column (variables) relations.
#' 2: table is created for row (objects) relations.
#' @return lambda value correlation matrix
#' 
#'@examples
#' testmatrixrand <- data.frame(
#' matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#' 
#' # correlation table is created for the columns of the input data.frame testmatrixrand
#' lambdasym(testmatrixrand)
#' lambdasym(testmatrixrand, 1)
#' labmbdacolumns <- lambdasym(matrix = testmatrixrand, dim = 1)
#'
#' # correlation table is created for the rows of the input data.frame testmatrixrand    
#' lambdasym(testmatrixrand, 2)
#' lambdarows <- lambdasym(matrix = testmatrixrand, dim = 2)
#' 
#' @export
#' 

lambdasym <- function (matrix, dim=1) {
  
  # create empty correlation table for the input data.frame
  corrtab <- varnastats:::newcorrtable(matrix, dim)
  
  if (dim == 1) {
    # loop to compare every column with every other column
    for (z in 1:length(matrix)) { 
      for (s in 1:length(matrix)) {
        # calculation of a 2*2 contingency table for the current column-column relation
        tbl = table(matrix[,z], matrix[,s])
        # calculation of lambda value
        corrtab[z,s] <- rapport::lambda.test(tbl, direction = 2)
      } 
    }
  }
  
  if (dim == 2) {
    # loop to compare every row with every other row
    # other comments: see above
    for (z in 1:length(matrix[,1])) { 
      for (s in 1:length(matrix[,1])) {
        tbl = table(unlist(matrix[z,]), unlist(matrix[s,]))
        corrtab[z,s] <- rapport::lambda.test(tbl, direction = 2)
      } 
    }
  }

  return(corrtab) 
}
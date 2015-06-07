#' Create a chi-square correlation matrix of an input data.frame
#'
#' \code{chi2test} returns a correlation matrix of a data.frame. The correlation value
#' is the test decision of the chi-square test for a defined decision niveau. A significant
#' relation of two variables/objects is marked with a numeric "1", a negativ test result however
#' with a numeric "0". The correlation matrix can be created for column or row relations of the
#' input data.frame. 
#'
#' @param matrix data.frame with numeric values
#' @param dim switch to define, whether the correlation matrix should be created 
#' for columns or rows. 
#' 1 (default): table is created for column (variables) relations.
#' 2: table is created for row (objects) relations.
#' @param chi2limit significance level for the test decision. default: 0.05 -> 5\%
#' @return chi-square correlation matrix
#' 
#'@examples
#' testmatrixrand <- data.frame(
#' matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#' 
#' # correlation table is created for the columns of the input data.frame testmatrixrand
#' chi2test(testmatrixrand)
#' chi2test(testmatrixrand, 1, 0.1)
#' chitabcolumns <- chi2test(matrix = testmatrixrand, dim = 1, chi2limit = 0.1)
#'
#' # correlation table is created for the rows of the input data.frame testmatrixrand    
#' chi2test(testmatrixrand, 2)
#' chitabrows <- chi2test(matrix = testmatrixrand, dim = 2, chi2limit = 0.03)
#' 
#' @export
#' 

chi2test <- function (matrix, dim = 1, chi2limit = 0.05) {
  
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
        # comparing p-Value with defined decision niveau chi2limit to make a test decision
        # DE: p-Value (Verwerfungsniveau) wird aus den Testergebnissen extrahiert und mit der eingegebenen 
        # Irrtumswahrscheinlichkeit chi2limit verglichen. Wenn die Wahrscheinlichkeit, dass man sich bei einer
        # Ablehnung der Nullhypothese (H0: kein Zusammenhang der Variablen) irrt, kleiner als chi2limit ausfällt,
        # dann kann ein signifikanter Zusammenhang angenommen werden, der mit einer 1 in der Ergebnismatrix
        # testtable festgehalten wird. Umgekehrt weist eine 0 auf keinen signifikanten Zusammenhang hin.
        if (unlist(x[3]) < chi2limit) {   
          corrtab[z,s] <- 1
        } else {
          corrtab[z,s] <- 0
        }
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
        if (unlist(x[3]) < chi2limit) {   
          corrtab[z,s] <- 1
        } else {
          corrtab[z,s] <- 0
        }
      } 
    }
  }
  
  return(corrtab) 
}
##' Cleans and sanitises strings of column names
##'
##' This function takes a vector of the strings of column names and
##' then converts them all into column names more suitable for easy
##' data exploration. The main conversion is switching all names to
##' lower case characters.
##' @title clean_names
##' @param colnames
##' @return
##' @author Mick Cooney <mickcooney@gmail.com>

clean_names <- function(colnames) {
    colnames <- gsub(" ",        "_", colnames)
    colnames <- gsub("/",        "_", colnames)
    colnames <- gsub("\\.",      "_", colnames)
    colnames <- gsub("\\-",      "_", colnames)
    colnames <- gsub("\\(",       "", colnames)
    colnames <- gsub("\\)",       "", colnames)
    colnames <- gsub("\\?",       "", colnames)
    colnames <- gsub("\\%",       "", colnames)
    colnames <- gsub("\u20AC", "EUR", colnames)

    return(colnames)
}

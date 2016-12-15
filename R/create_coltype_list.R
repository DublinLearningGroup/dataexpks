##' Create_coltype_list() splits columns into various types
##'
##' .. content for \details{} ..
##' @title
##' @param data_tbl
##' @return
##' @author Mick Cooney


create_coltype_list <- function(data_tbl) {
    coltypes <- sapply(data_tbl, categorise_datatype)

    split_lst <- sapply(unique(coltypes), function(x)
        names(coltypes[coltypes %in% x]))

    coltype_lst <- list(
        split   = split_lst
       ,columns = coltypes
    )

    return(coltype_lst)
}


### Checks if variable is a date/time
is_date <- function(.x) inherits(.x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))


categorise_datatype <- function (x) {
    if(all(is.na(x))) return("na")

    if(is_date(x))                          "datetime"
    else if (!is.null(attributes(x)) ||
             all(is.character(x))    ||
             all(is.logical(x)))            "discrete"
    else                                    "continuous"
}

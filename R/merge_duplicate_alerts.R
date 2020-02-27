##' Collapse multiple strings into one
##'
##' Where ProMED or HealthMap data contain multiple rows with the same
##' alert-id, we merge the alerts into one. When this is done, the mete
##' data associated with the rows is collapsed intto a sungle string.
##' Examples include the URL associated with the report. This is done
##' to prevent the metadata from being lost when rows are merged.
##' @title
##' @param cols_dup
##' @return
##' @author Sangeeta Bhatia
##'
paste_single_column <- function(column, sep) {
    paste(column, collapse = sep)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param df data frame containing duplicate alerts. Must contain a
##' column called cases. All columns except cases will be merged by
##' collpasing their content into a single string for each column.
##' The column cases will be merged accrding to the rule argument.
##' E.g., median will return the median of cases.
##' @param rule any valid R function that accepts a numeric vector
##' and returns a number. Defaults to median
##' @param sep
##' @return data.frame with a single row
##' @author Sangeeta Bhatia
##' @export
merge_duplicate_alerts <- function(df, rule = median, sep = " / ") {

    cols_to_merge <- colnames(df)
    cols_to_merge <- cols_to_merge[! cols_to_merge %in% "cases"]

    ##template for output
    out <- df[1, ]

    for (column in cols_to_merge) {

        out[[column]] <- paste_single_column(df[[column]], sep)

    }


    out$cases <- rule(df$cases, na.rm = TRUE)


    out

}

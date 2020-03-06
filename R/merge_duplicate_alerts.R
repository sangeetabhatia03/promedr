##' Merge rows with duplicate alerts
##'
##' Each record in ProMED and HealthMap data feeds is (in principle)
##' associated with
##' a unique alert-id. Occasionally, we get multiple rows that have
##' the same alert-id. In such instances, we want to merge these rows
##' into a single row in a meaningful way. For the meta-data associated
##' with the records e.g., the URL, it would be useful to retain all
##' of them, especially if these columns are not being used in the
##' analysis downstream. For others, e.g., the longitude and latitude,
##' we expect them to be the same across the set of records, but if they
##' are not, we want to retain one of them. Finally, for numeric columns
##' (particularly cases) we want a summary statistic like median or
##' mean.
##' This function merges the records with the user picking which
##' columns should be merged in which way i.e., whether all values
##' or only one of them should be retained.
##' The only exception is the column called cases, which is always
##' summarised using a mathematical function specified by the arg rule.
##'
##' @param df data frame containing duplicate alerts. Must contain a
##' column called cases. All columns except cases will be merged by
##' collpasing their content into a single string for each column.
##' The column cases will be merged accrding to the rule argument.
##' E.g., median will return the median of cases.
##' @param keep_all character vector. Names of columns for which values
##' in all rows should be retained.
##' @param keep_first character vector. Names of columns for which values
##' for which only the first value should be retained.
##' @param use_rule columns that should be summarised using rule.
##' These should all be numeric.
##' @param rule any valid R function that accepts a numeric vector
##' and returns a number. Defaults to median
##' @param sep separator used to paste multiple values
##' from a column
##' @return data.frame with a single row
##' @author Sangeeta Bhatia
##' @examples ## Made-up data
##' made_up <- data.frame(
##'    country = rep("singapore", 3),
##'    cases = c(3, 7, 9),
##'    alert_id = rep(letters[1], 3),
##'    longitude = c(103.8, 103.8, 103.8),
##'    latitude = c(1.4, 1.5, 1.4)
##' )
##' ##Alert-ids in this data.frame are duplicated. Merging the rows then
##' merged <-  merge_duplicate_alerts(
##'   made_up,
##'   keep_all = c("country", "alert_id"),
##'   keep_first = c("longitude", "latitude"))
##' @importFrom stats median
##' @export
merge_duplicate_alerts <- function(df,
                                   keep_all,
                                   keep_first,
                                   use_rule = c("cases"),
                                   rule = stats::median,
                                   sep = " / ") {

    all_cols <- colnames(df)

    ## Check that at least one column has duplicated values across
    ## all rows, else there would be little point in merging.
    unique_vals <- sapply(all_cols, function(x) length(unique(df[[x]])))

    if (! any(unique_vals == 1)) {

        masg <- "None of the columns in the data have identical values
                 across rows. Will merge anyway but check that you
                 really want to merge rows."
        warning(msg)
    }


    missing <- which(! all_cols %in% c(keep_all, keep_first, use_rule))

    if (length(missing) > 0) {
        msg <- "A merging rule should be specified for all columns."
        msg <- paste(msg, "No rule specified for following columns: ")
        msg <- paste(msg, all_cols[missing], " Defaults to keep_first.")

        warning(msg)
    }
    ## cases really should be dealt separately. Issue warning if user
    ## specifies keep_first or keep_all for cases.

    if ("cases" %in% keep_first) {
        msg <- "You have chosen to retain only the first value in cases"
        warning(msg)
    }
    ## This is an error since cases will no longer be numeric and will
    ## cause problem in downstream analysis.
    if ("cases" %in% keep_all) {
        msg <- "You have chosen to retain all values in column cases."
        msg <- paste(
            msg, "This will make column cases non-numeric."
        )
        stop(msg, call. = FALSE)
    }

    common <- intersect(keep_first, keep_all)
    if (length(common) > 1) {
        msg <- paste(
            "Columns", common, "are in both keep_first and keep_all."
        )
        msg <- paste(
            msg, "Only first value will be retained for these columns."
        )
        warning(msg)
    }

    are_numeric <- sapply(
            use_rule, function(x) is.numeric(df[[x]])
    )


    if (! all(are_numeric)) {
        msg <- "All columns specified using use_rule should be numeric."
        msg <- paste(
            msg, "Not numeric ", use_rule[! are_numeric]
        )
        stop(
            ,
            call. = FALSE
            )
    }
    ##template for output
    out <- df[1, ]

    for (column in keep_all) {

        out[[column]] <- paste(
            df[[column]], sep = sep, collapse = sep
        )

    }

    for (column in keep_first) {

        vals <- unique(out[[column]])
        if (length(vals) > 1) {
            msg <- "Not all values in"
            msg <- paste(column, "are identical. Retaining only first")
            warning(msg)
        }

        out[[column]] <- df[[column]][1]

    }

    if (! "cases" %in% keep_first) {
        out$cases <- rule(df$cases, na.rm = TRUE)
    }



    out

}

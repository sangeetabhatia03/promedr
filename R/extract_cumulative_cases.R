sum_only_na_stays_na <- function(x){

    if (all(is.na(x))) out <- NA
    else out <- sum(x, na.rm = TRUE)

    out
}

get_case_categories <- function(case_type) {

    out <- list(
        scc = c("sc", "cc"),
        sc = "sc",
        cc = "cc",
        scd = c("sd", "cd"),
        sd = "sd",
        cd = "cd",
        ALL = c(
            "sc", "cc", "sd", "cd"
       )
    )

    out[[case_type]]
}

##' Get cumulative case count as sum of specified case categories
##'
##' Data from ProMED and HealthMap contain counts for suspected
##' and confirmed cases, and suspected and confirmed deaths.
##' This function adds up the case counts in the specified categories
##' as the cumulative case count. The categories are:
##'
##' scc: suspected and confirmed cases
##' sc: suspected cases
##' cc: confirmed cases
##' scd: suspected and confirmed deaths
##' sd: suspected deaths
##' cd: confirmed deaths
##' ALL: suspected and confirmed cases, and suspected and confirmed deaths
##'
##' The cumulative case count for a date will be NA if ALL counts in the specified categories
##' on are NA for this date. Otherwise, it is the sum of the available values with NAs removed.
##'
##' @param x data from ProMED and HealthMap.
##' Should only contain numeric columns called "sc", "cc", "sd", "cd".
##' @param case_type quoted character. should be one of scc, sc, cc, scd, sd, cd or ALL.
##' @return numeric vector of cumulative case counts.
##' @author Sangeeta Bhatia
##' @export
get_cumulative_cases <- function(x,
                                 case_type = c("scc", "sc", "cc",
                                               "scd", "sd", "cd",
                                               "ALL")) {

    case_type <- match.arg(case_type)
    cols <- get_case_categories(case_type)

    out <- apply(x[ , cols], 1, sum_only_na_stays_na)

    out
}

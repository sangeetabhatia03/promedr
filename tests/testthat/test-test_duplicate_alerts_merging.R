context("Merging Duplicate Alerts")

## data set-up
bad <- data.frame(
    alert_id = rep("a123", 3),
    cases = c(1, 2, 6),
    URL = c(
        "https://promedmail.org/promed-post/?id=7051670",
        "https://promedmail.org/promed-post/?id=7051671",
        "https://healthmap.org/promed/?p=155"
    ),
    long = c(100, 110, 101),
    lat = c(13, 14, 15)
)

test_that("Merging duplicate rows works", {

    ## 2. all column names should be covered
    ## between keep_all, keep_first and use_rule
    ## If not, expect warning.

    keep_all <- c("alert_id", "URL")
    keep_first <- "long"
    msg <- "A merging rule should be specified for all columns."
    msg <- paste(msg, "No rule specified for following columns: ")
    msg <- paste(msg," lat Defaults to keep_first.")

    expect_warning(
        merge_duplicate_alerts(
            df = bad,
            keep_all = keep_all,
            keep_first = keep_first
        )
    )

    ## 3. Merged data.frame has a single row
    merged <- data.frame(
        alert_id = "a123 / a123 / a123",
        cases = 2,
        URL = "https://promedmail.org/promed-post/?id=7051670 / https://promedmail.org/promed-post/?id=7051671 / https://healthmap.org/promed/?p=155",
        long = 100,
        lat = 13
    )
    expect_equal(nrow(merged), 1)

    ## 4. first value of a column not specified in keep_first,
    ## keep_last or use_rules is
    expect_equal(bad$lat[1], merged$lat[1])


}

)

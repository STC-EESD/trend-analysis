
pixel.time.series.analysis <- function(x) {

    thisFunctionName <- "pixel.time.series.analysis";

    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if( any(is.na(x)) ) {

        output.vector <- c(NA,NA,NA);

    } else {

        require(Kendall);
        require(trend);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.ts <- stats::ts(
            data      = x,
            start     = c(1979, 1),
            frequency = 12
            );

        results.SeasonalMannKendall <- Kendall::SeasonalMannKendall(x = temp.ts);
        results.seasonalSenSlope    <- trend::sea.sens.slope(x = temp.ts);
        results.smk.test            <- trend::smk.test(x = temp.ts, alternative = "two.sided");

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        output.vector <- c(
            results.seasonalSenSlope,
            results.smk.test[['statistic']],
            results.smk.test[['p.value']]
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    attr(x = output.vector, "names") <- c(
        "Sen.Slope",
        "Z.statistic",
        "p.value"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat(paste0("\n# ",thisFunctionName,"() exits."));
    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.vector );

    }

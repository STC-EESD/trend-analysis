
pixelwise.time.series.analysis <- function(x) {

    thisFunctionName <- "pixelwise.time.series.analysis";

    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if( any(is.na(x)) ) {

        output.vector <- rep(x = NA, times = 11);

    } else {

        require(Kendall);
        require(trend);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.ts <- stats::ts(
            data      = x,
            start     = c(1979, 1),
            frequency = 12
            );

        # results.SeasonalMannKendall <- Kendall::SeasonalMannKendall(x = temp.ts);
        results.seasonalSenSlope  <- trend::sea.sens.slope(x = temp.ts);
        results.smk.test          <- trend::smk.test(x = temp.ts, alternative = "two.sided");
        results.tslm.trend        <- forecast::tslm(formula = temp.ts ~ trend);
        results.tslm.trend.season <- forecast::tslm(formula = temp.ts ~ trend + season);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        coeffs.results.tslm.trend.season <- summary(results.tslm.trend.season)[['coefficients']];
        coeffs.results.tslm.trend        <- summary(results.tslm.trend)[[       'coefficients']];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        output.vector <- c(
            Sen.slope             = results.seasonalSenSlope,
            smk.z.stat            = results.smk.test[['statistic']],
            smk.p.value           = results.smk.test[['p.value']],
            lm.slope.estimate     = coeffs.results.tslm.trend.season['trend','Estimate'  ],
            lm.slope.stderr       = coeffs.results.tslm.trend.season['trend','Std. Error'],
            lm.slope.t.value      = coeffs.results.tslm.trend.season['trend','t value'   ],
            lm.slope.p.value      = coeffs.results.tslm.trend.season['trend','Pr(>|t|)'  ],
            lm.intercept.estimate = coeffs.results.tslm.trend['(Intercept)', 'Estimate'  ],
            lm.intercept.stderr   = coeffs.results.tslm.trend['(Intercept)', 'Std. Error'],
            lm.intercept.t.value  = coeffs.results.tslm.trend['(Intercept)', 't value'   ],
            lm.intercept.p.value  = coeffs.results.tslm.trend['(Intercept)', 'Pr(>|t|)'  ]
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    attr(x = output.vector, which = "names") <- c(
        "Sen.slope",
        "smk.z.stat",
        "smk.p.value",
        "lm.slope.estimate",
        "lm.slope.stderr",
        "lm.slope.t.value",
        "lm.slope.p.value",
        "lm.intercept.estimate",
        "lm.intercept.stderr",
        "lm.intercept.t.value",
        "lm.intercept.p.value"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat(paste0("\n# ",thisFunctionName,"() exits."));
    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.vector );

    }

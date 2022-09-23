
get.DF.dates <- function(
    date.reference = as.Date("1970-01-01", tz = "UTC"),
    parquet.dates  = "DF-dates.parquet"
    ){

    thisFunctionName <- "get.DF.dates";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(parquet.dates) ) {

        DF.dates <- arrow::read_parquet(parquet.dates);

    } else {

        years <- seq(1979,2016);
        first.of.months <- paste0(stringr::str_pad(string = 1:12, pad = "0", width = 2, side = "left"),"-01");
        months <- paste(rep(years, each = 12), first.of.months, sep = "-");

        DF.dates <- data.frame(
            date.string = months,
            date        = as.Date(x = months, tz = lubridate::tz(date.reference))
            );
        DF.dates <- DF.dates[order(DF.dates[,'date']),c('date.string','date')];
        DF.dates[,'date.index'  ] <- seq(1,nrow(DF.dates));
        DF.dates[,'date.integer'] <- as.integer(DF.dates[,'date'] - date.reference);
        DF.dates[,'year'] <- as.integer(lubridate::year(DF.dates[,'date']));
        DF.dates <- DF.dates[,c('date.index',setdiff(colnames(DF.dates),'date.index'))];
        rownames(DF.dates) <- seq(1,nrow(DF.dates));

        arrow::write_parquet(x = DF.dates, sink = parquet.dates);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.dates );

    }

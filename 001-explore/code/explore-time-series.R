
explore.time.series <- function(
    data.sets              = NULL,
    DF.dates               = NULL,
    SF.coordinates         = NULL,
    ncdf4.aridity          = NULL,
    FILE.coords.to.indexes = NULL
    ){

    thisFunctionName <- "explore.time.series";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    nc.obj.aridity <- ncdf4::nc_open(ncdf4.aridity);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    coords.to.indexes <- readRDS(FILE.coords.to.indexes);
    n.months <- nc.obj.aridity[['dim']][['time']][['len']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(DF.dates)\n");
    print( str(DF.dates)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.data.set in data.sets[1] ) {

        cat("\n### processing:",temp.data.set,"\n");

        temp.varid <- ifelse(test = grepl(x = temp.data.set, pattern = "deficit", ignore.case = TRUE), yes = "deficit", no = "stress");

        temp.stem <- stringr::str_extract(string = tolower(temp.data.set), pattern = "(deficit|stress)");
        SF.stats <- getData.ts.stats(
            SF.coordinates = SF.coordinates,
            CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output = paste0("SF-Zdenek-",temp.stem,"-SenSlope.parquet")
            );

        cat("\nstr(SF.stats)\n");
        print( str(SF.stats)   );

        index.min.TestZ <- which(SF.stats$TestZ == min(SF.stats$TestZ));

        cat("\nstr(index.min.TestZ)\n");
        print( str(index.min.TestZ)   );

        cat("\nSF.stats[index.min.TestZ,]\n");
        print( SF.stats[index.min.TestZ,]   );

        temp.xy <- round(as.numeric(sf::st_coordinates(SF.stats[index.min.TestZ,])));

        cat("\nstr(temp.xy)\n");
        print( str(temp.xy)   );

        temp.xy.indexes <- coords.to.indexes(temp.xy);

        cat("\nstr(temp.xy.indexes)\n");
        print( str(temp.xy.indexes)   );

        temp.series <- ncvar_get(
            nc    = nc.obj.aridity,
            varid = temp.varid,
            start = c(temp.xy.indexes,1),
            count = c(1,1,n.months)
            );

        temp.ts <- stats::ts(
            data      = temp.series,
            start     = c(1979, 1),
            frequency = 12
            );

        cat("\nstr(temp.series)\n");
        print( str(temp.series)   );

        cat("\nsummary(temp.series)\n");
        print( summary(temp.series)   );

        results.SeasonalMannKendall <- Kendall::SeasonalMannKendall(x = temp.ts);
        results.seasonalSenSlope    <- trend::sea.sens.slope(x = temp.ts);
        results.smk.test            <- trend::smk.test(x = temp.ts, alternative = "two.sided");

        DF.temp <- data.frame(
            date  = DF.dates[,'date.index'],
            value = temp.series
            );

        cat("\nstr(DF.temp)\n");
        print( str(DF.temp)   );

        results.mblm <- mblm::mblm(
            formula   = value ~ date,
            dataframe = DF.temp
            );

        cat("\nsummary(results.SeasonalMannKendall)\n");
        print( summary(results.SeasonalMannKendall)   );

        cat("\nresults.SeasonalMannKendall\n");
        print( results.SeasonalMannKendall   );

        cat("\nresults.seasonalSenSlope\n");
        print( results.seasonalSenSlope   );

        cat("\nstr(results.seasonalSenSlope)\n");
        print( str(results.seasonalSenSlope)   );

        cat("\nsummary(results.smk.test)\n");
        print( summary(results.smk.test)   );

        cat("\nstr(results.smk.test)\n");
        print( str(results.smk.test)   );

        cat("\nsummary(results.mblm)\n");
        print( summary(results.mblm)   );

        cat("\nresults.mblm\n");
        print( results.mblm   );

        cat("\nSF.stats[index.min.TestZ,]\n");
        print( SF.stats[index.min.TestZ,]   );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(nc.obj.aridity);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

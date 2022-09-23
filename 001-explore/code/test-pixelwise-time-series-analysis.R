
test_pixelwise.time.series.analysis <- function(
    data.sets              = NULL,
    DF.dates               = NULL,
    SF.coordinates         = NULL,
    ncdf4.aridity          = NULL,
    FILE.coords.to.indexes = NULL,
    FUN.pixelwise          = NULL
    ){

    thisFunctionName <- "test_pixelwise.time.series.analysis";

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

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.stem <- stringr::str_extract(string = tolower(temp.data.set), pattern = "(deficit|stress)");
        SF.ZP.SenSlope <- getData.ts.stats(
            SF.coordinates = SF.coordinates,
            CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output = paste0("SF-ZP-",temp.stem,"-SenSlope.parquet")
            );

        cat("\nstr(SF.ZP.SenSlope)\n");
        print( str(SF.ZP.SenSlope)   );

        SF.ZP.linear <- getData.ts.stats(
            SF.coordinates = SF.coordinates,
            CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output = paste0("SF-ZP-",temp.stem,"-linear.parquet")
            );

        cat("\nstr(SF.ZP.linear)\n");
        print( str(SF.ZP.linear)   );

        SF.ZP.arima <- getData.ts.stats(
            SF.coordinates = SF.coordinates,
            CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output = paste0("SF-ZP-",temp.stem,"-arima.parquet")
            );

        cat("\nstr(SF.ZP.arima)\n");
        print( str(SF.ZP.arima)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        index.min.TestZ <- which(SF.ZP.SenSlope$TestZ == max(SF.ZP.SenSlope$TestZ));

        cat("\nstr(index.min.TestZ)\n");
        print( str(index.min.TestZ)   );

        temp.pointID <- as.integer(sf::st_drop_geometry(SF.ZP.SenSlope[index.min.TestZ,'pointID']));

        cat("\nstr(temp.pointID)\n");
        print( str(temp.pointID)   );

        temp.xy <- round(as.numeric(sf::st_coordinates(SF.ZP.SenSlope[index.min.TestZ,])));

        cat("\nstr(temp.xy)\n");
        print( str(temp.xy)   );

        temp.xy.indexes <- coords.to.indexes(temp.xy);

        cat("\nstr(temp.xy.indexes)\n");
        print( str(temp.xy.indexes)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.series <- ncvar_get(
            nc    = nc.obj.aridity,
            varid = temp.varid,
            start = c(temp.xy.indexes,1),
            count = c(1,1,n.months)
            );

        saveRDS(file = "tmp-single-time-series.RData", object = temp.series);

        results.FUN.pixelwise <- FUN.pixelwise(x = temp.series);

        cat("\nresults.FUN.pixelwise\n");
        print( results.FUN.pixelwise   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        cat("\nSF.ZP.SenSlope[SF.ZP.SenSlope$pointID == temp.pointID,]\n");
        print( SF.ZP.SenSlope[SF.ZP.SenSlope$pointID == temp.pointID,]   );

        cat("\nSF.ZP.linear[SF.ZP.linear$pointID == temp.pointID,]\n");
        print( SF.ZP.linear[SF.ZP.linear$pointID == temp.pointID,]   );

        cat("\nSF.ZP.arima[SF.ZP.arima$pointID == temp.pointID,]\n");
        print( SF.ZP.arima[SF.ZP.arima$pointID == temp.pointID,]   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        test_pixelwise.time.series.analysis_new.methods(x = temp.series);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(nc.obj.aridity);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
test_pixelwise.time.series.analysis_new.methods <- function(
    x = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # results.lm <- stats::lm(
    #     formula = x ~ time,
    #     # data  = data.frame(time = seq(0,455), x = x)
    #     data    = data.frame(time = seq(1,456), x = x)
    #     );
    #
    # cat("\nsummary(results.lm)\n");
    # print( summary(results.lm)   );

    # cat("\nresults.lm\n");
    # print( results.lm   );
    #
    # cat("\nstr(results.lm)\n");
    # print( str(results.lm)   );
    #
    # cat("\nstr(summary(results.lm))\n");
    # print( str(summary(results.lm))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.ts <- stats::ts(
        data      = x,
        start     = c(1979, 1),
        frequency = 12
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # results.tslm.trend <- forecast::tslm(formula = temp.ts ~ trend);
    #
    # cat("\nsummary(results.tslm.trend)\n");
    # print( summary(results.tslm.trend)   );
    #
    # cat("\nsummary(results.tslm.trend)[['coefficients']]\n");
    # print( summary(results.tslm.trend)[['coefficients']]   );
    #
    # results.tslm.trend.season <- forecast::tslm(formula = temp.ts ~ trend + season);
    #
    # cat("\nsummary(results.tslm.trend.season)\n");
    # print( summary(results.tslm.trend.season)   );
    #
    # cat("\nsummary(results.tslm.trend.season)[['coefficients']]\n");
    # print( summary(results.tslm.trend.season)[['coefficients']]   );

    # cat("\nresults.tslm\n");
    # print( results.tslm   );
    #
    # cat("\nstr(results.tslm)\n");
    # print( str(results.tslm)   );
    #
    # cat("\nstr(summary(results.tslm))\n");
    # print( str(summary(results.tslm))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.orders <- list(

        c(0L,0L,0L),
        c(0L,0L,1L),
        c(0L,1L,0L),
        c(0L,1L,1L),

        c(1L,0L,0L),
        c(1L,0L,1L),
        c(1L,1L,0L),
        c(1L,1L,1L)

        );

    # for ( temp.order in temp.orders ) {
    for ( p in seq(0,5) ) {
    for ( d in c(0)     ) {
    for ( q in seq(0,5) ) {

        temp.order <- c(p,d,q);
        results.arima <- stats::arima(
            x        = temp.ts,
            order    = temp.order,
            seasonal = list(order = c(1,0,0)),
            xreg     = stats::time(temp.ts) - 1979
            );
        cat("\nresults.arima -- temp.order = c(",paste(temp.order,collapse=","),")\n");
        print( results.arima   );

        }}}

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

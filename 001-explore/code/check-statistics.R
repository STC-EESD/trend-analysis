
check.statistcs <- function(
    varid               = "deficit",
    SF.coordinates      = NULL,
    ncdf4.aridity       = NULL,
    MARGIN              = c(1,2),
    FUN.pixelwise       = NULL,
    parquet.ZP.SenSlope = paste0("SF-ZP-",varid,"-SenSlope.parquet"),
    parquet.ZP.linear   = paste0("SF-ZP-",varid,"-linear.parquet"),
    parquet.output      = paste0("DF-check-",varid,".parquet")
    ){

    thisFunctionName <- "check.statistcs";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.KC.ts.stats <- nc_apply_3to2D(
        nc             = ncdf4.aridity,
        varid          = varid,
        MARGIN         = MARGIN,
        FUN            = FUN.pixelwise,
        parquet.output = parquet.output
        );

    DF.KC.ts.stats <- sf::st_drop_geometry(dplyr::left_join(
        x  = SF.coordinates,
        y  = DF.KC.ts.stats,
        by = c('x','y')
        ));

    cat("\nstr(DF.KC.ts.stats) --",varid,"\n");
    print( str(DF.KC.ts.stats)   );

    cat("\nsummary(DF.KC.ts.stats)\n");
    print( summary(DF.KC.ts.stats)   );

    SF.ZP.SenSlope <- sfarrow::st_read_parquet(parquet.ZP.SenSlope);
    SF.ZP.linear   <- sfarrow::st_read_parquet(parquet.ZP.linear);

    cat("\nstr(SF.ZP.SenSlope) --",varid,"\n");
    print( str(SF.ZP.SenSlope)   );

    cat("\nsummary(SF.ZP.SenSlope) --",varid,"\n");
    print( summary(SF.ZP.SenSlope)   );

    cat("\nstr(SF.ZP.linear) --",varid,"\n");
    print( str(SF.ZP.linear)   );

    cat("\nsummary(SF.ZP.linear) --",varid,"\n");
    print( summary(SF.ZP.linear)   );

    DF.check <- dplyr::left_join(
        x  = sf::st_drop_geometry(SF.ZP.SenSlope),
        y  = sf::st_drop_geometry(SF.ZP.linear),
        by = c('pointID','x','y')
        );

    DF.check <- dplyr::left_join(
        x  = DF.check,
        y  = DF.KC.ts.stats,
        by = c('pointID','x','y')
        );

    cat("\nstr(DF.check)\n");
    print( str(DF.check)   );

    cat("\nsummary(DF.check)\n");
    print( summary(DF.check)   );

    DF.check[,'check.Sen.Slope'  ] <- apply(X = DF.check[,c('Sen.slope',  'SenQ'  )], MARGIN = 1, FUN = FUN.relative.error);
    DF.check[,'check.smk.z.stat' ] <- apply(X = DF.check[,c('smk.z.stat', 'TestZ' )], MARGIN = 1, FUN = FUN.relative.error);
    DF.check[,'check.smk.p.value'] <- apply(X = DF.check[,c('smk.p.value','PValue')], MARGIN = 1, FUN = FUN.relative.error);

    DF.check[,'check.lm.slope.estimate'    ] <- apply(X = DF.check[,c('lm.slope.estimate',    'estimate.slope'    )], MARGIN = 1, FUN = FUN.relative.error);
    DF.check[,'check.lm.intercept.estimate'] <- apply(X = DF.check[,c('lm.intercept.estimate','estimate.intercept')], MARGIN = 1, FUN = FUN.relative.error);

    arrow::write_parquet(sink = parquet.output, x = DF.check);

    cat("\nsummary(DF.check[,c('check.Sen.Slope','check.smk.z.stat','check.smk.p.value','check.lm.slope.estimate','check.lm.intercept.estimate')])\n");
    print( summary(DF.check[,c('check.Sen.Slope','check.smk.z.stat','check.smk.p.value','check.lm.slope.estimate','check.lm.intercept.estimate')])   );

    is.selected <- (DF.check[,'check.smk.z.stat'] > 1e-2);
    cat("\nsum(is.selected)\n");
    print( sum(is.selected)   );

    selected.colnames <- c('pointID','TestZ','smk.z.stat','check.smk.z.stat');
    cat("\nDF.check[is.selected,selected.colnames]\n");
    print( DF.check[is.selected,selected.colnames]   );

    is.selected <- (DF.check[,'check.smk.p.value'] > 1e-2);
    cat("\nsum(is.selected)\n");
    print( sum(is.selected)   );

    selected.colnames <- c('pointID','PValue','smk.p.value','check.smk.p.value');
    DF.temp <- DF.check[is.selected,];

    DF.temp[,'PValue'] <- format(x = DF.temp[,'PValue'], scientific = TRUE);
    cat("\nDF.check[is.selected,selected.colnames]\n");
    print( DF.temp[ 1:100,      selected.colnames]   );

    DF.temp <- DF.check[is.selected,];
    DF.temp[,'smk.p.value'] <- round( x = DF.temp[,'smk.p.value'], digits = 5);
    DF.temp[,'check.smk.p.value'] <- apply(X = DF.temp[,c('smk.p.value','PValue')], MARGIN = 1, FUN = FUN.relative.error);
    DF.temp[,'PValue'] <- format(x = DF.temp[,'PValue'], scientific = TRUE);
    is.selected <- (DF.temp[,'check.smk.p.value'] > 9e-2);
    cat("\nsum(is.selected)\n");
    print( sum(is.selected)   );

    DF.temp       <- DF.temp[is.selected,];
    display.order <- order(DF.temp[,'check.smk.p.value'], decreasing = TRUE);
    cat("\nDF.temp[display.order,selected.colnames]\n");
    print( DF.temp[display.order,selected.colnames]   );

    cat("\ncor(x = DF.check[,'lm.slope.estimate'], y = DF.check[,'estimate.slope'])\n");
    print( cor(x = DF.check[,'lm.slope.estimate'], y = DF.check[,'estimate.slope'])   );

    cat("\ncor(x = DF.check[,'lm.intercept.estimate'], y = DF.check[,'estimate.intercept'])\n");
    print( cor(x = DF.check[,'lm.intercept.estimate'], y = DF.check[,'estimate.intercept'])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.colname.pairs <- list(
        c(x.var = "SenQ",               y.var = "check.Sen.Slope",             PNG = paste0("plot-check-",varid,"-SenSlope.png")    ),
        c(x.var = "estimate.slope",     y.var = "check.lm.slope.estimate",     PNG = paste0("plot-check-",varid,"-lm-slope.png")    ),
        c(x.var = "estimate.intercept", y.var = "check.lm.intercept.estimate", PNG = paste0("plot-check-",varid,"-lm-intercept.png"))
        );

    for ( pair.index in 1:length(list.colname.pairs) ) {
        check.statistcs_scatterplot(
            DF.input   = DF.check,
            x.var      = list.colname.pairs[[pair.index]][['x.var']],
            y.var      = list.colname.pairs[[pair.index]][['y.var']],
            PNG.output = list.colname.pairs[[pair.index]][['PNG']]
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }


##################################################
FUN.relative.error <- function(x) {
    if ( all(x == c(0,0)) ) {
        return( 0 );
    } else {
        return( abs(diff(x))/mean(abs(x)) )
        }
    }

check.statistcs_scatterplot <- function(
    DF.input      = NULL,
    x.var         = NULL,
    y.var         = NULL,
    x.label       = x.var,
    y.label       = y.var,
    PNG.output    = "plot-check-statistics.png",
    dots.per.inch = 300
    ) {

    require(ggplot2);

    DF.temp <- DF.input[,c(x.var,y.var)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = paste0("^",x.var,"$"),
        replacement = "x.var"
        );
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = paste0("^",y.var,"$"),
        replacement = "y.var"
        );

    my.ggplot <- initializePlot();
    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.temp,
        mapping = ggplot2::aes(
            x = x.var,
            y = y.var
            ),
        size  = 0.1,
        alpha = 0.5
        );
    my.ggplot <- my.ggplot + ggplot2::xlab(label = x.label);
    my.ggplot <- my.ggplot + ggplot2::ylab(label = y.label);

    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.ggplot,
        # scale  = 1,
        width    = 16,
        height   = 12, # 16 * (range.y/range.x),
        units    = "in",
        dpi      = dots.per.inch
        );

    return( NULL );

    }


generate.timeplots <- function(
    data.sets              = NULL,
    ncdf4.input            = NULL,
    get.coordinate.indexes = NULL,
    output.directory       = "plots-timeplots",
    dots.per.inch          = 300
    ){

    thisFunctionName <- "generate.timeplots";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !dir.exists(output.directory) ) {
        dir.create(path = output.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4.input.object <- ncdf4::nc_open(filename = ncdf4.input);
    n.time.points <- ncdf4.input.object[['dim']][['time']][['len']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    reference.date <- as.Date(gsub(
        x       = ncdf4.input.object[['dim']][['time']][['units']],
        pattern = "days since ",
        replacement = ""
        ));

    date.indexes <- ncdf4.input.object[['dim']][['time']][['vals']];
    my.dates     <- reference.date + date.indexes;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.data.set in data.sets ) {

        cat("\n### processing:",temp.data.set,"\n");

        DF.coordinates.to.plot <- generate.timeplots_get.coordinates(
            GDB.SpatialData = GDB.SpatialData,
            dir.aridity     = dir.aridity,
            data.set        = temp.data.set
            );

        cat("\nstr(DF.coordinates.to.plot)\n");
        print( str(DF.coordinates.to.plot)   );

        cat("\nsummary(DF.coordinates.to.plot)\n");
        print( summary(DF.coordinates.to.plot)   );

        # for ( row.index in 1:20 ) {
        for ( row.index in 1:nrow(DF.coordinates.to.plot) ) {

            pointID <- DF.coordinates.to.plot[row.index,'pointID'];
            x.coord <- DF.coordinates.to.plot[row.index,'x'      ];
            y.coord <- DF.coordinates.to.plot[row.index,'y'      ];
            TestZ   <- DF.coordinates.to.plot[row.index,'TestZ'  ];
            x.index <- DF.coordinates.to.plot[row.index,'x.index'];
            y.index <- DF.coordinates.to.plot[row.index,'y.index'];

            DF.time.series <- data.frame(
                date  = my.dates,
                value = ncdf4::ncvar_get(
                    nc = ncdf4.input.object,
                    varid = "deficit",
                    start = c(x.index,y.index,             1),
                    count = c(       1,      1,n.time.points)
                    )
                );

            cat("\nDF.coordinates.to.plot[row.index,]\n");
            print( DF.coordinates.to.plot[row.index,]   );

            cat("\nstr(DF.time.series)\n");
            print( str(DF.time.series)   );

            # cat("\nsummary(DF.time.series)\n");
            # print( summary(DF.time.series)   );

            if ( !any(is.na(DF.time.series[,'value'])) ) {
                generate.timeplots_plot(
                    pointID          = pointID,
                    x.coord          = x.coord,
                    y.coord          = y.coord,
                    TestZ            = TestZ,
                    DF.time.series   = DF.time.series,
                    output.directory = output.directory,
                    dots.per.inch    = dots.per.inch
                    );
                }

            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(nc = ncdf4.input.object);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
generate.timeplots_plot <- function(
    pointID          = NULL,
    x.coord          = NULL,
    y.coord          = NULL,
    TestZ            = NULL,
    DF.time.series   = NULL,
    output.directory = NULL,
    dots.per.inch    = 300
    ){


    DF.time.series[,'moving.average'] <- zoo::rollmean(
        x      = DF.time.series[,'value'],
        k      = 48,
        align  = 'center',
        fill   = 'NA'
        );

    # cat("\nstr(DF.time.series) -- generate.timeplots_plot(...)\n");
    # print( str(DF.time.series)   )

    # my.ggplot <- ggplot2::ggplot(data = NULL) + ggplot2::theme_bw();
    my.ggplot <- initializePlot(
        subtitle = paste0("pointID = ",pointID,", (x,y) = (",x.coord,",",y.coord,"), TestZ = ",TestZ)
        );

    my.ggplot <- my.ggplot + ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.x     = element_text(face = "bold", angle = 90, vjust = 0.5)
        );

    my.years <- unique(lubridate::year(DF.time.series[,'date']));
    is.selected <- rep(c(TRUE,FALSE), times = ceiling((1+length(my.years))/2));
    my.years <- my.years[is.selected[1:length(my.years)]];
    my.breaks = as.Date(paste0(my.years,"-01-01"));

    my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(
        breaks = my.breaks,
        labels = my.breaks
        );

    my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
        limits = 175 * c(  -1,1),
        breaks =  50 * seq(-4,4)
        );

    my.ggplot <- my.ggplot + ggplot2::geom_hline(
        yintercept = 0,
        size       = 1.3,
        color      = "grey85"
        );

    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data    = DF.time.series,
        mapping = ggplot2::aes(x = date, y = value)
        );

    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data    = DF.time.series,
        mapping = ggplot2::aes(x = date, y = moving.average, colour = "red")
        );

    # my.ggplot <- my.ggplot + tidyquant::geom_ma(ma_fun = SMA, n = 365, color = "red");

    PNG.output <- file.path(output.directory,paste0("plot-timeplot-",pointID,".png"));
    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.ggplot,
        # scale  = 1,
        width    = 30,
        height   =  8,
        units    = "in",
        dpi      = dots.per.inch
        );

    }

generate.timeplots_get.coordinates <- function(
    GDB.SpatialData = NULL,
    dir.aridity     = NULL,
    data.set        = NULL
    ){

    SF.stats <- getData.ts.stats(
        GDB.SpatialData = GDB.SpatialData,
        CSV.ts.stats    = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
        parquet.output  = paste0("data-",data.set,".parquet")
        );

    SF.stats <- SF.stats %>% dplyr::filter(TestZ > 4 | TestZ < -8);
    SF.stats <- cbind(sf::st_coordinates(SF.stats),SF.stats);

    colnames(SF.stats) <- gsub(
        x           = colnames(SF.stats),
        pattern     = "^X$",
        replacement = "x"
        );

    colnames(SF.stats) <- gsub(
        x           = colnames(SF.stats),
        pattern     = "^Y$",
        replacement = "y"
        );

    SF.stats <- SF.stats[,c('pointID',setdiff(colnames(SF.stats),'pointID'))];
    SF.stats[,c('x.index','y.index')] <- t(apply(
        X      = sf::st_drop_geometry(SF.stats[,c('x','y')]),
        MARGIN = 1,
        FUN    = get.coordinate.indexes
        ));

    cat("\nstr(SF.stats)\n");
    print( str(SF.stats)   );

    return( sf::st_drop_geometry(SF.stats) );

    }

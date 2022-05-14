
getData.water <- function(
    GDB.SpatialData = NULL,
    dir.water       = NULL,
    DF.metadata     = NULL,
    date.reference  = as.Date("1970-01-01", tz = "UTC"),
    ncdf4.output    = "data-water.nc"
    ) {

    thisFunctionName <- "getData.water";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(sf);
    require(arrow);
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.vars <- getData.water_get.list.vars(
        DF.metadata    = DF.metadata,
        date.reference = date.reference,
        dir.water      = dir.water
        );

    cat("\nstr(list.vars)\n");
    print( str(list.vars)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.ncdf4.object <- ncdf4::nc_create(
        filename = ncdf4.output,
        vars     = list.vars
        );

    my.crs <- getData.water_get.crs(
        dir.water   = dir.water,
        DF.metadata = DF.metadata
        );

    ncdf4::ncatt_put(
        nc         = output.ncdf4.object,
        varid      = 0,
        attname    = "crs",
        attval     = as.character(my.crs),
        prec       = "text",
        verbose    = FALSE,
        definemode = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # for ( metadatum.index in seq(1,nrow(DF.metadata)) ) {
    #     getData.water_put.variable <- function(
    #         dir.water     = dir.water,
    #         ncdf4.object  = output.ncdf4.object,
    #         variable      = DF.metadata[metadatum.index,'variable'     ],
    #         sub.directory = DF.metadata[metadatum.index,'sub.directory']
    #         );
    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(output.ncdf4.object);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
getData.water_get.crs <- function(
    dir.water   = NULL,
    DF.metadata = NULL
    ) {
    temp.sub.directory <- DF.metadata[1,'sub.directory'];
    temp.raster.files  <- list.files(path = file.path(dir.water,temp.sub.directory), pattern = "\\.bil");
    temp.raster        <- raster::stack(file.path(dir.water,temp.sub.directory,temp.raster.files[1]));
    return( raster::crs(temp.raster) );
    }

getData.water_put.variable <- function(
    dir.water     = NULL,
    ncdf4.object  = NULL,
    variable      = NULL,
    sub.directory = NULL
    ) {

    raster.files <- list.files(path = file.path(dir.water,sub.directory), pattern = "\\.bil");
    for ( temp.raster in raster.files ) {


        # ncdf4::ncvar_put(
        #     nc    = ncdf4.object,
        #     varid = variable,
        #     vals  = DF.temp,
        #     start = c(1,1,score.index),
        #     count = c(n.lats,n.lons,1)
        #     );

        }

    return( NULL );

    }

getData.water_get.list.vars <- function(
    DF.metadata    = NULL,
    date.reference = NULL,
    dir.water      = NULL
    ){

    list.vars <- list();
    for ( metadatum.index in seq(1,nrow(DF.metadata)) ) {

        temp.variable      <- DF.metadata[metadatum.index,'variable'     ];
        temp.units         <- DF.metadata[metadatum.index,'units'        ];
        temp.sub.directory <- DF.metadata[metadatum.index,'sub.directory'];
        temp.description   <- DF.metadata[metadatum.index,'description'  ];

        temp.raster.files <- list.files(path = file.path(dir.water,temp.sub.directory), pattern = "\\.bil");
        temp.raster       <- raster::stack(file.path(dir.water,temp.sub.directory,temp.raster.files[1]));

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.coordinates <- raster::coordinates(temp.raster);
        spatial.units  <- sf::st_crs(raster::crs(temp.raster))$units;

        dimension.x <- ncdf4::ncdim_def(
            name  = "x",
            units = spatial.units,
            vals  = sort(unique(DF.coordinates[,'x']))
            );

        dimension.y <- ncdf4::ncdim_def(
            name  = "y",
            units = spatial.units,
            vals  = sort(unique(DF.coordinates[,'y']))
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        years <- unique(stringr::str_extract(string = temp.raster.files, pattern = "_[0-9]{4}_"));
        years <- sort(as.integer(gsub(x = years, pattern = "_", replacement = "")));
        first.of.months <- paste0(stringr::str_pad(string = 1:12, pad = "0", width = 2, side = "left"),"-01");
        months <- paste(rep(years, each = 12), first.of.months, sep = "-");

        DF.dates <- data.frame(
            date.string = months,
            date        = as.Date(x = months, tz = lubridate::tz(date.reference))
            );
        DF.dates <- DF.dates[order(DF.dates[,'date']),c('date.string','date')];
        DF.dates[,'date.integer'] <- as.integer(DF.dates[,'date'] - date.reference);
        DF.dates[,'year'] <- as.integer(lubridate::year(DF.dates[,'date']));
        DF.dates <- DF.dates[,c('year',setdiff(colnames(DF.dates),'year'))];
        rownames(DF.dates) <- seq(1,nrow(DF.dates));

        cat("\nstr(DF.dates)\n");
        print( str(DF.dates)   );

        cat("\nsummary(DF.dates)\n");
        print( summary(DF.dates)   );

        dimension.time <- ncdf4::ncdim_def(
            name  = "time",
            units = paste("days since",date.reference,lubridate::tz(date.reference)),
            vals  = DF.dates[,'date.integer']
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        list.vars[[temp.variable]] <- ncdf4::ncvar_def(
            name     = temp.variable,
            longname = temp.description,
            units    = temp.units,
            dim = list(
                x    = dimension.x,
                y    = dimension.y,
                time = dimension.time
                )
            );

        }

    return( list.vars );

    }

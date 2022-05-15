
getData.aridity <- function(
    SF.SpatialData = NULL,
    dir.aridity    = NULL,
    DF.dates       = NULL,
    DF.metadata    = NULL,
    date.reference = as.Date("1970-01-01", tz = "UTC"),
    ncdf4.output   = "data-aridity.nc"
    ) {

    thisFunctionName <- "getData.aridity";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(ncdf4.output) ) {
        cat("\n");
        cat(paste0("The file ",ncdf4.output," already exists; do nothing."));
        cat("\n");
        cat(paste0("\n# ",thisFunctionName,"() exits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( NULL );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(sf);
    require(arrow);
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.vars <- getData.aridity_get.list.vars(
        SF.SpatialData = SF.SpatialData,
        DF.metadata    = DF.metadata,
        date.reference = date.reference,
        DF.dates       = DF.dates
        );

    cat("\nstr(list.vars)\n");
    print( str(list.vars)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.ncdf4.object <- ncdf4::nc_create(
        filename = ncdf4.output,
        vars     = list.vars
        );

    ncdf4::ncatt_put(
        nc         = output.ncdf4.object,
        varid      = 0,
        attname    = "crs",
        attval     = sf::st_crs(sf::st_read(GDB.SpatialData))$proj4string,
        prec       = "text",
        verbose    = FALSE,
        definemode = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # for ( metadatum.index in seq(1,nrow(DF.metadata)) ) {
    #     getData.aridity_put.variable(
    #         dir.aridity   = dir.aridity,
    #         ncdf4.object  = output.ncdf4.object,
    #         variable      = DF.metadata[metadatum.index,'variable'     ],
    #         sub.directory = DF.metadata[metadatum.index,'sub.directory'],
    #         DF.dates      = DF.dates
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
getData.aridity_put.variable <- function(
    dir.aridity   = NULL,
    ncdf4.object  = NULL,
    variable      = NULL,
    sub.directory = NULL,
    DF.dates      = NULL
    ) {

    raster.files <- list.files(path = file.path(dir.aridity,sub.directory), pattern = "\\.bil$");
    for ( temp.raster.file in raster.files ) {

        cat("\ntemp.raster.file\n");
        print( temp.raster.file   );

        n.x <- ncdf4.object[['dim']][['x']][['len']];
        n.y <- ncdf4.object[['dim']][['y']][['len']];

        cat("\nn.x\n");
        print( n.x   );

        cat("\nn.y\n");
        print( n.y   );

        temp.month <- unique(stringr::str_extract(string = temp.raster.file, pattern = "_[0-9]{4}_[0-9]{2}"));
        temp.month <- gsub(x = temp.month, pattern = "^_", replacement = "" );
        temp.month <- gsub(x = temp.month, pattern = "_",  replacement = "-");
        temp.date.string  <- paste0(temp.month,"-01");
        date.index <- DF.dates[DF.dates[,'date.string'] == temp.date.string,'date.index'];

        cat("\nDF.dates[date.index,]\n");
        print( DF.dates[date.index,]   );

        temp.raster <- raster::raster(file.path(dir.aridity,sub.directory,temp.raster.file));
        DF.tidy     <- cbind(raster::coordinates(temp.raster),raster::getValues(temp.raster));
        DF.tidy[DF.tidy[,3] <= -32750,3] <- NA;

        ### !!! It is crucial to order DF.tidy, first by 'y', then by 'x', !!!
        ### !!! for the next two lines to work properly                    !!!
        DF.tidy <- DF.tidy[order(DF.tidy[,'y'],DF.tidy[,'x']),];
        DF.temp <- matrix(data = DF.tidy[,3], nrow = n.x, ncol = n.y, byrow = FALSE);

        cat("\nstr(DF.tidy)\n");
        print( str(DF.tidy)   );

        cat("\nDF.tidy[1:20,]\n");
        print( DF.tidy[1:20,]   );

        cat("\nstr(DF.temp)\n");
        print( str(DF.temp)   );

        ncdf4::ncvar_put(
            nc    = ncdf4.object,
            varid = variable,
            vals  = DF.temp,
            start = c(1,  1,  date.index),
            count = c(n.x,n.y,1         )
            );

        }

    return( NULL );

    }

getData.aridity_get.list.vars <- function(
    SF.SpatialData = NULL,
    DF.metadata    = NULL,
    date.reference = NULL,
    DF.dates       = NULL
    ){

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    spatial.units <- sf::st_crs(SF.SpatialData)$units;
    vals.x <- sort(unique(unlist(  sf::st_drop_geometry(SF.SpatialData[,'X'])  )));
    vals.y <- sort(unique(unlist(  sf::st_drop_geometry(SF.SpatialData[,'Y'])  )));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    dimension.x <- ncdf4::ncdim_def(
        name  = "x",
        units = spatial.units,
        vals  = vals.x
        );

    dimension.y <- ncdf4::ncdim_def(
        name  = "y",
        units = spatial.units,
        vals  = vals.y
        );

    dimension.time <- ncdf4::ncdim_def(
        name  = "time",
        units = paste("days since",date.reference,lubridate::tz(date.reference)),
        vals  = DF.dates[,'date.integer']
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.vars <- list();
    for ( metadatum.index in seq(1,nrow(DF.metadata)) ) {
        temp.variable <- DF.metadata[metadatum.index,'variable'];
        list.vars[[temp.variable]] <- ncdf4::ncvar_def(
            name     = temp.variable,
            longname = DF.metadata[metadatum.index,'description'],
            units    = DF.metadata[metadatum.index,'units'      ],
            dim = list(
                x    = dimension.x,
                y    = dimension.y,
                time = dimension.time
                )
            );
        }

    return( list.vars );

    }

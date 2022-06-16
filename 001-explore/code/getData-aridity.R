
getData.aridity <- function(
    GDB.SpatialData = NULL,
    SF.coordinates  = NULL,
    dir.aridity     = NULL,
    DF.dates        = NULL,
    DF.metadata     = NULL,
    date.reference  = as.Date("1970-01-01", tz = "UTC"),
    ncdf4.output    = "data-aridity.nc"
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
        SF.coordinates = SF.coordinates,
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
    for ( metadatum.index in seq(1,nrow(DF.metadata)) ) {
        getData.aridity_put.variable(
            dir.aridity    = dir.aridity,
            ncdf4.object   = output.ncdf4.object,
            variable       = DF.metadata[metadatum.index,'variable'     ],
            sub.directory  = DF.metadata[metadatum.index,'sub.directory'],
            DF.coordinates = sf::st_drop_geometry(SF.coordinates),
            DF.dates       = DF.dates
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(output.ncdf4.object);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
getData.aridity_put.variable <- function(
    dir.aridity    = NULL,
    ncdf4.object   = NULL,
    variable       = NULL,
    sub.directory  = NULL,
    DF.dates       = NULL,
    DF.coordinates = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.x <- ncdf4.object[['dim']][['x']][['len']];
    n.y <- ncdf4.object[['dim']][['y']][['len']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.grid <- expand.grid(
        x = ncdf4.object[['dim']][['x']][['vals']],
        y = ncdf4.object[['dim']][['y']][['vals']]
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    txt.files <- list.files(path = file.path(dir.aridity,sub.directory), pattern = "\\.txt$");
    for ( temp.txt.file in txt.files ) {

        DF.txt <- read.csv(file.path(dir.aridity,sub.directory,temp.txt.file));
        colnames(DF.txt) <- gsub(x = colnames(DF.txt), pattern = "pointid", replacement = "pointID");
        date.colnames <- grep(x = colnames(DF.txt), pattern = "[0-9]{4}.[0-9]{2}", value = TRUE);

        for ( temp.date.colname in date.colnames ) {

            cat("\n### temp.txt.file:",temp.txt.file,", temp.date.colname:",temp.date.colname,"\n");

            temp.date.string <- stringr::str_extract(string = temp.date.colname, pattern = "[0-9]{4}.[0-9]{2}");
            temp.date.string <- gsub(x = temp.date.string, pattern = "_", replacement = "-");
            temp.date.string <- paste0(temp.date.string,"-01");
            date.index       <- DF.dates[DF.dates[,'date.string'] == temp.date.string,'date.index'];

            cat("\ntemp.date.colname:",temp.date.colname,"\n");
            cat("\ntemp.date.string: ",temp.date.string, "\n");
            cat("\nDF.dates[DF.dates[,'date.string'] == temp.date.string,]\n");
            print( DF.dates[DF.dates[,'date.string'] == temp.date.string,]   );

            DF.temp <- DF.txt[,c('pointID',temp.date.colname)];
            DF.temp <- as.data.frame(dplyr::left_join(
                x  = DF.coordinates,
                y  = DF.temp,
                by = "pointID"
                ));

            DF.temp <- as.data.frame(dplyr::left_join(
                x  = DF.grid,
                y  = DF.temp,
                by = c('x','y')
                ));

            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            cat("\nsummary(DF.temp)\n");
            print( summary(DF.temp)   );

            cat("\nsum(!is.na(DF.temp[,temp.date.colname]))\n");
            print( sum(!is.na(DF.temp[,temp.date.colname]))   );

            ### !!! It is crucial to order DF.tidy, first by 'y', then by 'x', !!!
            ### !!! for the next two lines to work properly                    !!!
            DF.temp <- DF.temp[order(DF.temp[,'y'],DF.temp[,'x']),c('y','x',temp.date.colname)];
            DF.temp <- matrix(data = DF.temp[,temp.date.colname], nrow = n.x, ncol = n.y, byrow = FALSE);

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

        }

    return( NULL );

    }

getData.aridity_get.list.vars <- function(
    SF.coordinates = NULL,
    DF.metadata    = NULL,
    date.reference = NULL,
    DF.dates       = NULL
    ){

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    spatial.units <- sf::st_crs(SF.coordinates)$units;
    vals.x <- sort(unique(unlist(  sf::st_drop_geometry(SF.coordinates[,'x'])  )));
    vals.y <- sort(unique(unlist(  sf::st_drop_geometry(SF.coordinates[,'y'])  )));

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

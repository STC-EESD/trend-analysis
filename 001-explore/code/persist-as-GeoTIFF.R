
persist.as.GeoTIFF <- function(
    ncdf4.input.file = NULL,
    DF.dates         = NULL,
    output.directory = "geotiffs"
    ) {

    thisFunctionName <- "persist.as.GeoTIFF";
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !dir.exists(output.directory) ) {
        dir.create(
            path      = output.directory,
            recursive = TRUE
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4.input.object <- ncdf4::nc_open(ncdf4.input.file);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( row.index in seq(1,nrow(DF.dates)) ) {

        temp.date.index   <- DF.dates[row.index,'date.index'  ];
        temp.date.string  <- DF.dates[row.index,'date.string' ];
        temp.date         <- DF.dates[row.index,'date'        ];
        temp.date.integer <- DF.dates[row.index,'date.integer'];
        temp.year         <- DF.dates[row.index,'year'        ];

        persist.as.GeoTIFF_inner(
            ncdf4.input.object = ncdf4.input.object,
            date.index         = temp.date.index,
            date.string        = temp.date.string,
            date.integer       = temp.date.integer,
            year               = temp.year,
            output.directory   = output.directory
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(ncdf4.input.object);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
persist.as.GeoTIFF_inner <- function(
    ncdf4.input.object = NULL,
    date.index         = NULL,
    date.string        = NULL,
    date.integer       = NULL,
    year               = NULL,
    output.directory   = NULL
    ) {

    thisFunctionName <- "persist.as.GeoTIFF_inner";
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(raster);
    require(terra);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.x <- ncdf4.input.object[['dim']][['x']][['len']];
    n.y <- ncdf4.input.object[['dim']][['y']][['len']];

    x.min <- min(ncdf4.input.object[['dim']][['x']][['vals']]);
    x.max <- max(ncdf4.input.object[['dim']][['x']][['vals']]);
    y.min <- min(ncdf4.input.object[['dim']][['y']][['vals']]);
    y.max <- max(ncdf4.input.object[['dim']][['y']][['vals']]);

    var.names <- names(ncdf4.input.object[['var']]);

    crs.string <- ncdf4::ncatt_get(
        nc      = ncdf4.input.object,
        varid   = 0,
        attname = 'crs'
        )[['value']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    matrix.extent <- matrix(
        data  = c(x.min,x.max,y.min,y.max),
        nrow  = 2,
        ncol  = 2,
        byrow = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( var.name in var.names ) {

        DF.temp <- ncdf4::ncvar_get(
            nc    = ncdf4.input.object,
            varid = var.name,
            start = c(1,1,date.index),
            count = c(n.x,n.y,1)
            );
        DF.temp <- DF.temp[,seq(ncol(DF.temp),1,-1)];

        layer.temp <- raster::raster(
            nrows = n.y,
            ncols = n.x,
            crs   = crs.string
            );
        raster::values(layer.temp) <- t(DF.temp);

        list.layers <- list();
        list.layers[[var.name]] <- layer.temp;

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.stack <- raster::stack(x = list.layers);
        names(my.stack) <- names(list.layers);

        my.stack <- raster::setExtent(
            x   = my.stack,
            ext = raster::extent(matrix.extent)
            );

        my.rast <- terra::rast(x = my.stack);

        cat("\nclass(my.rast)\n");
        print( class(my.rast)   );

        cat("\nnames(my.rast)\n");
        print( names(my.rast)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        tiff.file.output <- file.path(
            output.directory,
            paste0(date.string,"-",var.name,".tiff")
            );

        # raster::writeRaster(x = my.stack, filename = tiff.file.output);
        terra::writeRaster(x = my.rast, filename = tiff.file.output);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        remove(list = c(
            'layer.temp','DF.temp',
            'my.stack','my.rast','tiff.file.output','list.layers'
            ));
        gc();

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

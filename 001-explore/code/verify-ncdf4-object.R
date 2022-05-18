
verify.ncdf4.object <- function(
    ncdf4.input            = NULL,
    dir.aridity            = NULL,
    DF.SpatialData         = NULL,
    DF.dates               = NULL,
    get.coordinate.indexes = NULL,
    DF.metadata            = NULL,
    n.sampled.rows         = 10
    ) {

    thisFunctionName <- "verify.ncdf4.object";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(sf);
    require(arrow);
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4.input.object <- ncdf4::nc_open(ncdf4.input);
    n.x <- ncdf4.input.object[['dim']][['x']][['len']];
    n.y <- ncdf4.input.object[['dim']][['y']][['len']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.check <- data.frame();
    for ( row.index in 1:nrow(DF.metadata) ) {

        varid     <- DF.metadata[row.index,'varid'    ];
        directory <- DF.metadata[row.index,'directory'];

        txt.files <- list.files(path = file.path(dir.aridity,directory), pattern = "\\.txt");

        # cat("\ntxt.files\n");
        # print( txt.files   );

        for ( txt.file in txt.files ) {

            year <- as.numeric(stringr::str_extract(txt.file, pattern = "[0-9]{4}"));
            date.string <- paste0(year,"-01-01");
            date.index  <- DF.dates[DF.dates[,'date.string'] == date.string,'date.index'];

            DF.txt <- verify.ncdf4.object_get.DF.txt(
                dir.aridity            = dir.aridity,
                directory              = directory,
                txt.file               = txt.file,
                DF.SpatialData         = DF.SpatialData,
                get.coordinate.indexes = get.coordinate.indexes
                );

            # cat("\nstr(DF.txt)\n");
            # print( str(DF.txt)   );

            value.colnames <- grep(x = colnames(DF.txt), pattern = "[0-9]{4}_[0-9]{2}", value = TRUE);

            # cat("\nstr(value.colnames)\n");
            # print( str(value.colnames)   );

            sampled.row.indices <- sample(x = 1:nrow(DF.txt), size = n.sampled.rows);
            for ( row.index in sampled.row.indices ) {

                # cat("\nt(DF.txt[row.index,value.colnames])\n");
                # print( t(DF.txt[row.index,value.colnames])   );

                pointID <- DF.txt[row.index,'pointID'];
                x.coord <- DF.txt[row.index,'x'      ];
                y.coord <- DF.txt[row.index,'y'      ];
                x.index <- DF.txt[row.index,'x.index'];
                y.index <- DF.txt[row.index,'y.index'];

                temp.vector <- ncdf4::ncvar_get(
                    nc    = ncdf4.input.object,
                    varid = varid,
                    start = c(x.index,y.index,date.index),
                    count = c(1,      1,      12        )
                    );

                # cat("\ntemp.vector\n");
                # print( temp.vector   );

                DF.check.temp <- data.frame(
                    varid     = rep(varid,  times = nrow(temp.vector)),
                    pointID   = rep(pointID,times = nrow(temp.vector)),
                    x         = rep(x.coord,times = nrow(temp.vector)),
                    y         = rep(y.coord,times = nrow(temp.vector)),
                    x.index   = rep(x.index,times = nrow(temp.vector)),
                    y.index   = rep(y.index,times = nrow(temp.vector)),
                    date      = paste0(year,"-",stringr::str_pad(1:12, pad = "0", width = 2),"-01"),
                    txt       = as.numeric(DF.txt[row.index,value.colnames]),
                    nc        = temp.vector,
                    row.names = NULL
                    );

                cat("\nDF.check.temp\n");
                print( DF.check.temp   );

                DF.check <- rbind(DF.check,DF.check.temp);

                }

            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.check[,'diff'] <- abs(DF.check[,'txt'] - DF.check[,'nc']);
    write.csv(
        x         = DF.check,
        file      = "DF-verify-ncdf4-object.csv",
        row.names = FALSE
        );

    cat("\nsummary(DF.check)\n");
    print( summary(DF.check)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(ncdf4.input.object);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
verify.ncdf4.object_get.DF.txt <- function(
    dir.aridity            = NULL,
    directory              = NULL,
    txt.file               = NULL,
    DF.SpatialData         = NULL,
    get.coordinate.indexes = NULL
    ) {
    DF.output <- read.csv(file.path(dir.aridity,directory,txt.file));
    colnames(DF.output) <- tolower(colnames(DF.output));
    DF.output <- DF.output[,setdiff(colnames(DF.output),'objectid')];
    colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = "pointid", replacement = "pointID");
    DF.output <- dplyr::left_join(x = DF.output, y = DF.SpatialData, by = "pointID");
    DF.output <- DF.output[order(DF.output[,'y'],DF.output['x']),];
    DF.output[,c('x.index','y.index')] <- t(apply(X = DF.output[,c('x','y')], MARGIN = 1, FUN = get.coordinate.indexes));
    leading.colnames <- c('pointID','x','y','x.index','y.index');
    DF.output <- DF.output[,c(leading.colnames,setdiff(colnames(DF.output),leading.colnames))];
    return( DF.output );
    }

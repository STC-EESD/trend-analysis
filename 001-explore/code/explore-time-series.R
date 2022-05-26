
explore.time.series <- function(
    data.sets              = NULL,
    DF.dates               = NULL,
    GDB.SpatialData        = NULL,
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

        SF.stats <- getData.ts.stats(
            GDB.SpatialData = GDB.SpatialData,
            CSV.ts.stats    = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output  = paste0("SF-",temp.data.set,".parquet")
            );

        cat("\nstr(SF.stats)\n");
        print( str(SF.stats)   );

        index.min.TestZ <- which(SF.stats$TestZ == min(SF.stats$TestZ));

        cat("\nstr(index.min.TestZ)\n");
        print( str(index.min.TestZ)   );

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

        cat("\nstr(temp.series)\n");
        print( str(temp.series)   );

        cat("\nsummary(temp.series)\n");
        print( summary(temp.series)   );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(nc.obj.aridity);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

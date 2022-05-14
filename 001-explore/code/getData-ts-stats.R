
getData.ts.stats <- function(
    GDB.SpatialData = NULL,
    CSV.ts.stats    = NULL,
    parquet.output  = "ts-stats.parquet"
    ) {

    thisFunctionName <- "getData.ts.stats";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(sf);
    require(arrow);
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(parquet.output) ) {

        cat(paste0("\nThe file ",parquet.output," already exists; loading the file ...\n"));
        SF.output <- sf::st_as_sf(arrow::read_parquet(file = parquet.output));

    } else {

        cat(paste0("\nThe file ",parquet.output," does not yet exists; reading the file now ...\n"));

        SF.SpatialData <- sf::st_read(GDB.SpatialData);

        colnames(SF.SpatialData) <- gsub(
            x           = colnames(SF.SpatialData),
            pattern     = "pointid",
            replacement = "pointID"
            );

        # SF.SpatialData <- cbind(
        #     SF.SpatialData,
        #     sf::st_coordinates(SF.SpatialData)
        #     );

        cat("\ntype(SF.SpatialData)\n");
        print( type(SF.SpatialData)   );

        cat("\nsf::st_crs(SF.SpatialData)\n");
        print( sf::st_crs(SF.SpatialData)   );

        cat("\ncolnames(SF.SpatialData)\n");
        print( colnames(SF.SpatialData)   );

        cat("\nstr(SF.SpatialData)\n");
        print( str(SF.SpatialData)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        SF.output <- read.csv(file = CSV.ts.stats);

        SF.output[,'Domain'] <- as.numeric(gsub(
            x           = SF.output[,'Domain'],
            pattern     = "^_",
            replacement = ""
            ));

        colnames(SF.output) <- gsub(
            x           = colnames(SF.output),
            pattern     = "Domain",
            replacement = "pointID"
            );

        cat("\nstr(SF.output) -- from CSV\n");
        print( str(SF.output)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        SF.output <- getData.ts.stats_remove.rows.with.dots(SF.input = SF.output);
        SF.output <- getData.ts.stats_recast.columns(       SF.input = SF.output);

        cat("\nstr(SF.output) -- from CSV -- after cleaning\n");
        print( str(SF.output)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        cat("\nsetdiff( SF.SpatialData$pointID , SF.output$pointID )\n");
        print( setdiff( SF.SpatialData$pointID , SF.output$pointID )   );

        cat("\nsetdiff( SF.output$pointID , SF.SpatialData$pointID )\n");
        print( setdiff( SF.output$pointID , SF.SpatialData$pointID )   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        SF.output <- dplyr::inner_join(
            x  = SF.SpatialData,
            y  = SF.output,
            by = "pointID"
            );

        cat("\nstr(SF.output) -- after inner join\n");
        print( str(SF.output)   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        arrow::write_parquet(
            sink = parquet.output,
            x    = SF.output
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( SF.output );

    }

##################################################
getData.ts.stats_remove.rows.with.dots <- function(
    SF.input = NULL
    ) {
    SF.output <- SF.input;
    SF.output[,'has.dots'] <- apply(X = SF.output, MARGIN = 1, FUN = function(x) {any(x == ".")} );
    SF.output <- SF.output[!SF.output[,'has.dots'],];
    SF.output <- SF.output[,setdiff(colnames(SF.output),'has.dots')]
    return( SF.output );
    }

getData.ts.stats_recast.columns <- function(
    SF.input = NULL
    ) {
    SF.output <- SF.input;
    numeric.colnames <- setdiff(colnames(SF.output),c("pointID","Shape"));
    for ( temp.colname in numeric.colnames ) {
        if ( "character" == typeof(unlist(SF.output[,temp.colname])) ) {
            # SF.output[,temp.colname] <- as.numeric(unlist(sf::st_drop_geometry(SF.output[,temp.colname])));
            SF.output[,temp.colname] <- gsub(x = SF.output[,temp.colname], pattern = "^\\.$", replacement = "NA");
            SF.output[,temp.colname] <- as.numeric(SF.output[,temp.colname]);
            }
        }
    return( SF.output );
    }

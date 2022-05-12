
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

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        SF.output <- dplyr::full_join(
            x  = SF.SpatialData,
            y  = SF.output,
            by = "pointID"
            );

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
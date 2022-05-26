
get.DF.coordinates <- function(
    GDB.SpatialData     = NULL,
    parquet.SpatialData = "SF-SpatialData.parquet"
    ){

    thisFunctionName <- "get.DF.coordinates";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    SF.SpatialData <- sf::st_read(GDB.SpatialData);
    SF.SpatialData <- cbind(SF.SpatialData,sf::st_coordinates(SF.SpatialData));
    colnames(SF.SpatialData) <- gsub(
        x           = colnames(SF.SpatialData),
        pattern     = "pointid",
        replacement = "pointID"
        );
    colnames(SF.SpatialData) <- gsub(
        x           = colnames(SF.SpatialData),
        pattern     = "^X$",
        replacement = "x"
        );
    colnames(SF.SpatialData) <- gsub(
        x           = colnames(SF.SpatialData),
        pattern     = "^Y$",
        replacement = "y"
        );
    sfarrow::st_write_parquet(obj = SF.SpatialData, dsn = parquet.SpatialData);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    x.coords <- sort(unique(unlist(sf::st_drop_geometry(SF.SpatialData[,'x']))));
    y.coords <- sort(unique(unlist(sf::st_drop_geometry(SF.SpatialData[,'y']))));

    get.coordinate.indexes <- function(coordinates = NULL) {
        which.x <- which( coordinates[1] == x.coords );
        which.y <- which( coordinates[2] == y.coords );
        if (length(which.x) == 0) { which.x <- NA };
        if (length(which.y) == 0) { which.y <- NA };
        output.vector <- c(which.x,which.y);
        return(output.vector);
        }

    get.integer.coordinate.indexes <- function(coordinates = NULL) {
        which.x <- which( coordinates[1] == round(x.coords) );
        which.y <- which( coordinates[2] == round(y.coords) );
        if (length(which.x) == 0) { which.x <- NA };
        if (length(which.y) == 0) { which.y <- NA };
        output.vector <- c(which.x,which.y);
        return(output.vector);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- list(
        SF.SpatialData                 = SF.SpatialData,
        get.coordinate.indexes         = get.coordinate.indexes,
        get.integer.coordinate.indexes = get.integer.coordinate.indexes
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }


get.DF.coordinates <- function(
    GDB.SpatialData     = NULL,
    parquet.coordinates = "SF-coordinates.parquet"
    ){

    thisFunctionName <- "get.DF.coordinates";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    SF.coordinates <- sf::st_read(GDB.SpatialData);
    SF.coordinates <- cbind(SF.coordinates,sf::st_coordinates(SF.coordinates));
    colnames(SF.coordinates) <- gsub(
        x           = colnames(SF.coordinates),
        pattern     = "pointid",
        replacement = "pointID"
        );
    colnames(SF.coordinates) <- gsub(
        x           = colnames(SF.coordinates),
        pattern     = "^X$",
        replacement = "x"
        );
    colnames(SF.coordinates) <- gsub(
        x           = colnames(SF.coordinates),
        pattern     = "^Y$",
        replacement = "y"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nmax(abs(SF.coordinates$x - round(SF.coordinates$x)))\n");
    print( max(abs(SF.coordinates$x - round(SF.coordinates$x)))   );

    cat("\nmax(abs(SF.coordinates$y - round(SF.coordinates$y)))\n");
    print( max(abs(SF.coordinates$y - round(SF.coordinates$y)))   );

    SF.coordinates$x <- round(SF.coordinates$x);
    SF.coordinates$y <- round(SF.coordinates$y);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    sfarrow::st_write_parquet(obj = SF.coordinates, dsn = parquet.coordinates);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    x.coords <- sort(unique(unlist(sf::st_drop_geometry(SF.coordinates[,'x']))));
    y.coords <- sort(unique(unlist(sf::st_drop_geometry(SF.coordinates[,'y']))));

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
        SF.coordinates                 = SF.coordinates,
        get.coordinate.indexes         = get.coordinate.indexes,
        get.integer.coordinate.indexes = get.integer.coordinate.indexes
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

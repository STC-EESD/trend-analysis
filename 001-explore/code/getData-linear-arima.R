
getData.linear.arima <- function(
    SF.coordinates = NULL,
    directory      = NULL,
    parquet.linear = paste0("SF-Zdenek-linear.parquet"),
    parquet.arima  = paste0("SF-Zdenek-arima.parquet")
    ) {

    thisFunctionName <- "getData.linear.arima";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(sf);
    require(arrow);
    require(dplyr);
    require(readxl);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(parquet.linear) ) {
        cat(paste0("\nThe file ",parquet.linear," already exists; do nothing ...\n"));
        # SF.output <- sfarrow::st_read_parquet(dsn = parquet.linear);
    } else {
        cat(paste0("\nThe file ",parquet.linear," does not yet exists; loading data ...\n"));
        getData.linear.arima_linear(
            SF.coordinates = SF.coordinates,
            directory      = directory,
            parquet.linear = parquet.linear
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(parquet.arima) ) {
        cat(paste0("\nThe file ",parquet.arima," already exists; do nothing ...\n"));
        # SF.output <- sfarrow::st_read_parquet(dsn = parquet.linear);
    } else {
        cat(paste0("\nThe file ",parquet.arima," does not yet exists; loading data ...\n"));
        getData.linear.arima_arima(
            SF.coordinates = SF.coordinates,
            directory      = directory,
            parquet.arima  = parquet.arima
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
getData.linear.arima_linear <- function(
    SF.coordinates = NULL,
    directory      = NULL,
    parquet.linear = NULL
    ) {

    DF.output <- data.frame();

    temp.files <- list.files(path = directory, pattern = "Trend[0-9]+\\.xlsx");
    for ( temp.file in temp.files ) {

        DF.temp <- as.data.frame(readxl::read_xlsx(
            path  = file.path(directory,temp.file),
            sheet = "Linear"
            ));

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        colnames(DF.temp) <- tolower(colnames(DF.temp));
        DF.temp[,  'rowID'] <- rep(x = seq(1,nrow(DF.temp)/2), each = 2);
        DF.temp[,'groupID'] <- rep(x = c(2,1), times = nrow(DF.temp)/2);
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "^\\.+[0-9]+$",
            replacement = "pointID"
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "^domain$",
            replacement = "pointID"
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "std\\. error",
            replacement = "std.err"
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "t value",
            replacement = "t.value"
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "pr\\(.+\\)",
            replacement = "p.value"
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.temp.1 <- DF.temp[DF.temp[,'groupID'] == 1,];
        DF.temp.2 <- DF.temp[DF.temp[,'groupID'] == 2,];
        DF.temp.1 <- DF.temp.1[,setdiff(colnames(DF.temp.1),c('groupID'          ))];
        DF.temp.2 <- DF.temp.2[,setdiff(colnames(DF.temp.2),c('groupID','pointID'))];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        cat("\nstr(DF.temp.1)\n");
        print( str(DF.temp.1)   );

        print( "A-1" );
        DF.temp.1[,'pointID'] <- as.numeric(gsub(
            x           = DF.temp.1[,'pointID'],
            pattern     = "^X_",
            replacement = ""
            ));
        print( "A-2" );
        colnames(DF.temp.1) <- paste0(colnames(DF.temp.1),".slope");
        colnames(DF.temp.1) <- gsub(
            x           = colnames(DF.temp.1),
            pattern     = "pointID\\.slope",
            replacement = "pointID"
            );
        colnames(DF.temp.1) <- gsub(
            x           = colnames(DF.temp.1),
            pattern     = "rowID\\.slope",
            replacement = "rowID"
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        colnames(DF.temp.2) <- paste0(colnames(DF.temp.2),".intercept");
        colnames(DF.temp.2) <- gsub(
            x           = colnames(DF.temp.2),
            pattern     = "rowID\\.intercept",
            replacement = "rowID"
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.temp.3 <- dplyr::left_join(
            x  = DF.temp.1,
            y  = DF.temp.2,
            by = "rowID"
            );
        DF.temp.3 <- DF.temp.3[,setdiff(colnames(DF.temp.3),'rowID')];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.output <- rbind(DF.output,DF.temp.3);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        rm(list = c('DF.temp','DF.temp.1','DF.temp.2','DF.temp.3'));

        }

    SF.output <- dplyr::left_join(
        x  = SF.coordinates,
        y  = DF.output,
        by = "pointID"
        );

    sfarrow::st_write_parquet(
        dsn = parquet.linear,
        obj = SF.output
        );

    return( NULL );

    }

getData.linear.arima_arima <- function(
    SF.coordinates = NULL,
    directory      = NULL,
    parquet.arima  = NULL
    ) {

    DF.output <- data.frame();

    temp.files <- list.files(path = directory, pattern = "Trend[0-9]+\\.xlsx");
    for ( temp.file in temp.files ) {
        DF.temp <- as.data.frame(readxl::read_xlsx(
            path  = file.path(directory,temp.file),
            sheet = "Arima"
            ));
        colnames(DF.temp) <- tolower(colnames(DF.temp));
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "domain",
            replacement = "pointID"
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = "\\)",
            replacement = ""
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = " \\(",
            replacement = "."
            );
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = " ",
            replacement = "."
            );
        DF.temp[,'pointID'] <- gsub(
            x           = DF.temp[,'pointID'],
            pattern     = "^_",
            replacement = ""
            );
        DF.temp[,'pointID'] <- as.numeric(DF.temp[,'pointID']);
        DF.output <- rbind(DF.output,DF.temp);
        rm(list = c('DF.temp'));
        }

    SF.output <- dplyr::left_join(
        x  = SF.coordinates,
        y  = DF.output,
        by = "pointID"
        );

    sfarrow::st_write_parquet(
        dsn = parquet.arima,
        obj = SF.output
        );

    return( NULL );

    }

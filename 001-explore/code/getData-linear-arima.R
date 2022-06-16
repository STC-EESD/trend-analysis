
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
    # if ( file.exists(parquet.linear) ) {
    #     cat(paste0("\nThe file ",parquet.linear," already exists; do nothing ...\n"));
    #     # SF.output <- sfarrow::st_read_parquet(dsn = parquet.linear);
    # } else {
    #     cat(paste0("\nThe file ",parquet.linear," does not yet exists; loading data ...\n"));
    #     getData.linear.arima_linear(
    #         SF.coordinates = SF.coordinates,
    #         directory      = directory,
    #         parquet.linear = parquet.linear
    #         );
    #     }

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

        cat("\nstr(DF.temp)\n");
        print( str(DF.temp)   );

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

        cat("\nstr(DF.output)\n");
        print( str(DF.output)   );

        cat("\nstr(DF.temp)\n");
        print( str(DF.temp)   );

        print("A-1");

        DF.output <- rbind(DF.output,DF.temp);

        print("A-2");

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

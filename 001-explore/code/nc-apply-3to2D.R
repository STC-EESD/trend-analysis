
nc_apply_3to2D <- function(
    nc             = NULL,
    varid          = NULL,
    MARGIN         = NULL,
    FUN            = "identity",
    parquet.output = "nc_apply_3to2D.parquet",
    RData.output   = gsub(x = gsub(x = parquet.output, pattern = "\\.parquet", ".RData"), pattern = "^DF-", replacement = "tmp-A3D-")
    ) {

    thisFunctionName <- "nc_apply_3to2D";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(arrow);
    require(ncdf4);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(parquet.output) ) {
        cat("\n");
        cat(paste0("The file ",parquet.output," already exists; loading the file."));
        cat("\n");
        output.matrix <- arrow::read_parquet(file = parquet.output);
        cat(paste0("\n# ",thisFunctionName,"() exits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( output.matrix );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    nc.obj <- ncdf4::nc_open(filename = nc);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(RData.output) ) {
        cat("\n");
        cat(paste0("The file ",RData.output," already exists; loading the file ..."));
        cat("\n");
        output.array <- readRDS(file = RData.output);
        # names(dimnames(output.array))[3] <- "statistics";
    } else {

        cat("\n");
        cat(paste0("The file ",RData.output," does NOT yet exist; creating the output array ..."));
        cat("\n");

        input.array  <- ncdf4::ncvar_get(nc = nc.obj, varid = varid);
        dimnames.input.array <- list();
        for ( dim.index in seq(1,length(nc.obj[['dim']])) ) {
            dimnames.input.array[[ nc.obj[['dim']][[dim.index]][['name']] ]] <- nc.obj[['dim']][[dim.index]][['vals']];
            }
        dimnames(input.array) <- dimnames.input.array;

        if ( "function" == class(FUN) ) {
            if ( file.exists("tmp-diagnostics-output-array.RData") ) {
                output.array <- readRDS(file = "tmp-diagnostics-output-array.RData");
            } else {
                output.array <- apply(X = input.array, MARGIN = MARGIN, FUN = FUN);
                saveRDS(file = "tmp-diagnostics-output-array.RData", object = output.array);
                }
            output.colunmn.indexes <- c(setdiff(seq(1,length(dim(output.array))),MARGIN),MARGIN);
            output.array <- base::aperm(a = output.array, perm = order(output.colunmn.indexes));
            names(dimnames(output.array)) <- gsub(x = names(dimnames(output.array)), pattern = "^$", replacement = "statistics");
        } else {
            output.array <- input.array;
            dimnames.output.array <- list();
            dimnames.output.array[[ nc.obj[['dim']][[1]][['name']] ]] <- nc.obj[['dim']][[1]][['vals']];
            dimnames.output.array[[ nc.obj[['dim']][[2]][['name']] ]] <- nc.obj[['dim']][[2]][['vals']];
            reference.Date <- nc.obj[['dim']][['time']][['units']];
            reference.Date <- gsub(x = reference.Date, pattern = "days since ", replacement = "");
            reference.Date <- as.Date(reference.Date);
            dimnames.output.array[[ nc.obj[['dim']][[3]][['name']] ]] <- as.character(reference.Date + nc.obj[['dim']][[3]][['vals']]);
            dimnames(output.array) <- dimnames.output.array;
            }

        saveRDS(file = RData.output, object = output.array);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.matrix <- nc_apply_3to2D_array.3D.to.2D(
        x.coords       = nc.obj[['dim']][[1]][['vals']],
        y.coords       = nc.obj[['dim']][[2]][['vals']],
        input.array.3D = output.array
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ncdf4::nc_close(nc = nc.obj);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    arrow::write_parquet(
       sink = parquet.output,
       x    = output.matrix
       );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.matrix );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
nc_apply_3to2D_array.3D.to.2D <- function(
    x.coords       = NULL,
    y.coords       = NULL,
    input.array.3D = NULL
    ) {

    thisFunctionName <- "nc_apply_array.3D.to.2D";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\ndim(input.array.3D)\n");
    print( dim(input.array.3D)   );

    cat("\ndimnames(input.array.3D)\n");
    print( dimnames(input.array.3D)   );

    cat("\nstr(input.array.3D)\n");
    print( str(input.array.3D)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.matrix <- cbind(
        expand.grid(x = x.coords, y = y.coords),
        matrix(
            data = as.vector(input.array.3D),
            nrow = prod(dim(input.array.3D)[c(1,2)]),
            ncol = dim(input.array.3D)[3]
            )
        );

    colnames(output.matrix) <- c(
        names(dimnames(input.array.3D))[c(1,2)],
        dimnames(input.array.3D)[[3]]
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.matrix );

    }

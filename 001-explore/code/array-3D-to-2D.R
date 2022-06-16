
array.3D.to.2D <- function(
    x.coords       = NULL,
    y.coords       = NULL,
    input.array.3D = NULL
    ) {

    thisFunctionName <- "array.3D.to.2D";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

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

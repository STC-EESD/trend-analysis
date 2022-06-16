
test_array.3D.to.2D <- function() {

    thisFunctionName <- "test_array.3D.to.2D";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.array <- test_array.3D.to.2D_generate.data();

    cat("\noriginal.array\n");
    print( original.array   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    transformed.array <- nc_apply_3to2D_array.3D.to.2D(
        x.coords       = seq(1,dim(original.array)[1]),
        y.coords       = seq(1,dim(original.array)[2]),
        input.array.3D = original.array
        );

    cat("\nstr(transformed.array)\n");
    print( str(transformed.array)   );

    cat("\ntransformed.array\n");
    print( transformed.array   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( k in seq(1,dim(original.array)[3]) ) {
        transformed.array[,paste0("v.",k)] <- 100*transformed.array[,1] + 10*transformed.array[,2] + k;
        }

    for ( k in seq(1,dim(original.array)[3]) ) {
        transformed.array[,paste0("c.",k)] <- abs(transformed.array[,paste0("z.",k)] - transformed.array[,paste0("v.",k)]);
        }

    cat("\nstr(transformed.array)\n");
    print( str(transformed.array)   );

    cat("\ntransformed.array\n");
    print( transformed.array   );

    cat("\nsummary(transformed.array)\n");
    print( summary(transformed.array)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
test_array.3D.to.2D_generate.data <- function() {

    x.coords <- seq(1,7);
    y.coords <- seq(1,5);
    z.coords <- seq(1,3);

    temp.array <- array(
        dim = c(length(x.coords),length(y.coords),length(z.coords)),
        dimnames = list(
            lon = paste0("lon.",seq(1,length(x.coords))),
            lat = paste0("lat.",seq(1,length(y.coords))),
            z   = paste0("z.",  seq(1,length(z.coords)))
            )
        );

    xyz.grid <- expand.grid(
        x = x.coords,
        y = y.coords,
        z = z.coords
        );

    for (x in x.coords) {
    for (y in y.coords) {
    for (z in z.coords) {
        temp.array[x,y,z] <- 100*x + 10*y + z
        }}}

    return( temp.array );

    }

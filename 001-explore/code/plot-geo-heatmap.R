
plot.geo.heatmap <- function(
    SF.input        = NULL,
    variable        = NULL,
    palette.size    = 255,
    palette.colours = c('Navy','Blue','Green','Yellow','Red'),
    PNG.output      = paste0('plot-geo-heatmap-',variable,'.png'),
    dots.per.inch   = 300
    ) {

    thisFunctionName <- "plot.geo.heatmap";
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n"));

    require(arrow);
    require(terrainr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FUN.colours <- grDevices::colorRampPalette(palette.colours);
    DF.colours  <- data.frame(
        colour.index = seq(0,palette.size),
        colour.hex   = FUN.colours(1+palette.size)
        );
    DF.colours <- cbind(
        DF.colours,
        colorspace::hex2RGB(DF.colours[,'colour.hex'])@coords
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.geo.heatmap <- plot.geo.heatmap_terrainr(
        SF.input   = SF.input,
        variable   = variable,
        DF.colours = DF.colours
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.geo.heatmap,
        # scale  = 1,
        width    = 16,
        height   = 16,
        units    = "in",
        dpi      = dots.per.inch
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
plot.geo.heatmap_terrainr <- function(
    SF.input   = NULL,
    variable   = NULL,
    DF.colours = NULL
    ) {

    require(ggplot2);
    require(terrainr);

    crs.object <- sf::st_crs(SF.input);

    SF.temp <- cbind(SF.input,sf::st_coordinates(SF.input));

    SF.temp[,'colour.index'] <- plot.geo.heatmap_to.colour.index(
        x = sf::st_drop_geometry(SF.temp[,variable])[,1],
        palette.size = nrow(DF.colours)
        );

    SF.temp <- dplyr::left_join(
        x  = SF.temp,
        y  = DF.colours,
        by = "colour.index"
        );

    SF.temp <- SF.temp[,c("X","Y","R","G","B")];
    SF.temp <- SF.temp[!is.na(sf::st_drop_geometry(SF.temp[,"R"])),];

    SF.temp <- sf::st_drop_geometry(SF.temp);

    my.ggplot <- ggplot2::ggplot(data = NULL) + ggplot2::theme_bw();

    # my.ggplot <- my.ggplot + ggplot2::theme(
    #     plot.subtitle = ggplot2::element_text(size = textsize.title, face = "bold")
    #     );
    # my.ggplot <- my.ggplot + ggplot2::labs(title = NULL, subtitle = year);

    my.ggplot <- my.ggplot + terrainr::geom_spatial_rgb(
        data    = SF.temp,
        mapping = ggplot2::aes(
            x = X,
            y = Y,
            r = R,
            g = G,
            b = B
            )
        );

    # my.ggplot <- my.ggplot + ggplot2::theme(legend.position = "none");

    my.ggplot <- my.ggplot + ggplot2::coord_sf(crs = crs.object);

    # range.y <- sum(range(DF.temp[,'x']) * c(-1,1));
    # range.x <- sum(range(DF.temp[,'y']) * c(-1,1));

    # ggplot2::ggsave(
    #     filename = PNG.output,
    #     plot     = my.ggplot,
    #     # scale  = 1,
    #     width    = 16,
    #     height   = 16, # 16 * (range.y/range.x),
    #     units    = "in",
    #     dpi      = dots.per.inch
    #     );

    # remove(list = c('DF.temp','my.ggplot','range.lat','range.lon'));

    return( my.ggplot );

    }

plot.geo.heatmap_to.colour.index <- function(
    x            = NULL,
    palette.size = NULL
    ) {
    y <- (x - min(x, na.rm = TRUE)) / sum(c(-1,1) * range(x, na.rm = TRUE), na.rm = TRUE);
    y <- ceiling(palette.size * y);
    return( y );
    }

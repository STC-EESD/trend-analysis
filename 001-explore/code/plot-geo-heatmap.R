
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
    DF.colours <- plot.geo.heatmap_DF.colors(
        SF.input        = SF.input,
        variable        = variable,
        palette.size    = palette.size,
        palette.colours = palette.colours
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    print("A-1");

    my.geo.heatmap <- plot.geo.heatmap_terrainr(
        SF.input   = SF.input,
        variable   = variable,
        DF.colours = DF.colours
        );

    print("A-2");

    my.density.plot <- plot.geo.heatmap_density(
        SF.input = SF.input,
        variable = variable
        );

    print("A-3");

    my.legend <- plot.geo.heatmap_legend(
        DF.colours = DF.colours
        );

    print("A-4");

    ggplot2::ggsave(
        filename = "plot-legend.png",
        plot     = my.legend,
        # scale  = 1,
        width    =  3,
        height   = 16,
        units    = "in",
        dpi      = dots.per.inch
        );

    print("A-4a");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    inner.cowplot <- cowplot::plot_grid(
        my.legend,
        my.density.plot,
        nrow       = 1,
        align      = "h",
        rel_widths = c(1,2)
        );

    my.cowplot <- cowplot::plot_grid(
        my.geo.heatmap,
        inner.cowplot,
        nrow       = 1,
        rel_widths = c(10,3)
        );

    print("A-5");

    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.cowplot,
        # scale  = 1,
        width    = 32,
        height   = 16,
        units    = "in",
        dpi      = dots.per.inch
        );

    print("A-6");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
plot.geo.heatmap_legend <- function(
    DF.colours = NULL
    ) {

    require(ggplot2);

    DF.temp <- DF.colours[,c('value','colour.hex')];

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );

    my.ggplot <- ggplot2::ggplot(data = NULL) + ggplot2::theme_bw();

    # my.ggplot <- my.ggplot + ggplot2::theme(
    #     plot.subtitle = ggplot2::element_text(size = textsize.title, face = "bold")
    #     );
    # my.ggplot <- my.ggplot + ggplot2::labs(title = NULL, subtitle = year);

    my.ggplot <- my.ggplot + ggplot2::geom_segment(
        data    = DF.temp,
        mapping = ggplot2::aes(
            x     = -0.5,
            y     = value,
            xend  =  0.5,
            yend  = value
            ),
        color = DF.temp$colour.hex
        );

    my.ggplot <- my.ggplot + ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_text(size = 30, face = "bold"),
        # axis.ticks.y = ggplot2::element_line(size = 30),
        axis.ticks.length.y = ggplot2::unit(0.25,"in")
        );

    my.ggplot <- my.ggplot + ggplot2::theme(legend.position = "none");

    return( my.ggplot );

    }

plot.geo.heatmap_DF.colors <- function(
    SF.input        = NULL,
    variable        = NULL,
    palette.size    = NULL,
    palette.colours = NULL
    ) {
    temp.range  <- range(sf::st_drop_geometry(SF.input[,variable]), na.rm = TRUE);
    step.size   <- sum( c(-1,1) * temp.range ) / palette.size;
    temp.values <- seq(temp.range[1],temp.range[2],step.size);
    FUN.colours <- grDevices::colorRampPalette(palette.colours);
    DF.colours  <- data.frame(
        colour.index = seq(0,palette.size),
        value        = temp.values,
        colour.hex   = FUN.colours(1+palette.size)
        );
    DF.colours <- cbind(
        DF.colours,
        colorspace::hex2RGB(DF.colours[,'colour.hex'])@coords
        );
    return( DF.colours );
    }

plot.geo.heatmap_density <- function(
    SF.input = NULL,
    variable = NULL
    ) {

    require(ggplot2);

    DF.temp <- sf::st_drop_geometry(cbind(SF.input,sf::st_coordinates(SF.input)));
    DF.temp <- DF.temp[,c("X","Y",variable)];
    DF.temp <- DF.temp[!is.na(DF.temp[,variable]),];

    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = variable,
        replacement = "variable"
        );

    my.ggplot <- ggplot2::ggplot(data = NULL) + ggplot2::theme_bw();

    # my.ggplot <- my.ggplot + ggplot2::theme(
    #     plot.subtitle = ggplot2::element_text(size = textsize.title, face = "bold")
    #     );
    # my.ggplot <- my.ggplot + ggplot2::labs(title = NULL, subtitle = year);

    my.ggplot <- my.ggplot + ggplot2::geom_density(
        data    = DF.temp,
        mapping = ggplot2::aes(y = variable)
        );

    # my.ggplot <- my.ggplot + ggplot2::theme(legend.position = "none");

    return( my.ggplot );

    }

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

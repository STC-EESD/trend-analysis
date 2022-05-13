
plot.geo.heatmap <- function(
    SF.input              = NULL,
    variable              = NULL,
    palette.mid.point     = NULL,
    # palette.colours = c('Navy','Blue','Green','Yellow','Red'),
    upper.palette.colours = c('black','orange'),
    lower.palette.colours = c('cyan','black'),
    upper.palette.size    = 255,
    lower.palette.size    = 255,
    PNG.output            = paste0('plot-geo-heatmap-',variable,'.png'),
    dots.per.inch         = 300
    ) {

    thisFunctionName <- "plot.geo.heatmap";
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n"));

    require(arrow);
    require(terrainr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.colours <- plot.geo.heatmap_DF.colors(
        SF.input              = SF.input,
        variable              = variable,
        palette.mid.point     = palette.mid.point,
        upper.palette.colours = upper.palette.colours,
        lower.palette.colours = lower.palette.colours,
        upper.palette.size    = upper.palette.size,
        lower.palette.size    = lower.palette.size
        );

    cat("\nsummary(DF.colours)\n");
    print( summary(DF.colours)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.geo.heatmap <- plot.geo.heatmap_terrainr(
        SF.input           = SF.input,
        variable           = variable,
        DF.colours         = DF.colours,
        palette.mid.point  = palette.mid.point,
        upper.palette.size = upper.palette.size,
        lower.palette.size = lower.palette.size
        );

    my.density.plot <- plot.geo.heatmap_density(
        SF.input = SF.input,
        variable = variable
        );

    my.legend <- plot.geo.heatmap_legend(
        DF.colours = DF.colours
        );

    # ggplot2::ggsave(
    #     filename = "plot-legend.png",
    #     plot     = my.legend,
    #     # scale  = 1,
    #     width    =  3,
    #     height   = 16,
    #     units    = "in",
    #     dpi      = dots.per.inch
    #     );

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

    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.cowplot,
        # scale  = 1,
        width    = 25,
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
        axis.title.y = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
        # axis.text.y  = ggplot2::element_blank(),
        # axis.ticks.y = ggplot2::element_blank()
        );

    my.ggplot <- my.ggplot + ggplot2::theme(legend.position = "none");

    return( my.ggplot );

    }

plot.geo.heatmap_DF.colors <- function(
    SF.input              = NULL,
    variable              = NULL,
    palette.mid.point     = NULL,
    upper.palette.colours = NULL,
    lower.palette.colours = NULL,
    upper.palette.size    = NULL,
    lower.palette.size    = NULL
    ) {

    if ( is.null(palette.mid.point)) {
        palette.mid.point <- median(sf::st_drop_geometry(SF.input[,variable]), na.rm = TRUE);
        }

    full.range <- range(sf::st_drop_geometry(SF.input[,variable]), na.rm = TRUE);

    lower.range  <- c(full.range[1],palette.mid.point);
    lower.values <- seq(lower.range[1], lower.range[2], length.out = lower.palette.size);
    lower.values <- lower.values[seq(1,length(lower.values)-1)];

    upper.range  <- c(palette.mid.point,full.range[2]);
    upper.values <- seq(upper.range[1], upper.range[2], length.out = upper.palette.size);

    DF.lower.colours  <- data.frame(
        colour.index = seq(1,length(lower.values)),
        value        = lower.values,
        colour.hex   = grDevices::colorRampPalette(lower.palette.colours)(length(lower.values))
        );
    DF.lower.colours <- cbind(
        DF.lower.colours,
        colorspace::hex2RGB(DF.lower.colours[,'colour.hex'])@coords
        );

    DF.upper.colours  <- data.frame(
        colour.index = lower.palette.size + seq(0,length(upper.values)-1),
        value        = upper.values,
        colour.hex   = grDevices::colorRampPalette(upper.palette.colours)(length(upper.values))
        );
    DF.upper.colours <- cbind(
        DF.upper.colours,
        colorspace::hex2RGB(DF.upper.colours[,'colour.hex'])@coords
        );

    return( rbind(DF.lower.colours,DF.upper.colours) );

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
        mapping = ggplot2::aes(y = variable, size = 5)
        );

    my.ggplot <- my.ggplot + ggplot2::theme(
        legend.position     = "none",
        axis.title.x        = ggplot2::element_blank(),
        axis.title.y        = ggplot2::element_blank(),
        axis.text.x         = ggplot2::element_blank(),
        axis.text.y         = ggplot2::element_text(size = 30, face = "bold"),
        axis.ticks.length.y = ggplot2::unit(0.25,"in")
        );

    return( my.ggplot );

    }

plot.geo.heatmap_terrainr <- function(
    SF.input           = NULL,
    variable           = NULL,
    DF.colours         = NULL,
    palette.mid.point  = NULL,
    upper.palette.size = NULL,
    lower.palette.size = NULL
    ) {

    require(ggplot2);
    require(terrainr);

    crs.object <- sf::st_crs(SF.input);

    SF.temp <- cbind(SF.input,sf::st_coordinates(SF.input));

    print("A-1");

    SF.temp[,'colour.index'] <- plot.geo.heatmap_to.colour.index(
        x          = sf::st_drop_geometry(SF.temp[,variable])[,1],
        DF.colours = DF.colours
        );

    print("A-2");

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

    my.ggplot <- my.ggplot + ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_blank(),
        axis.ticks.x    = ggplot2::element_blank()
        );

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
    x          = NULL,
    DF.colours = NULL
    ) {
    DF.temp <- data.frame(
        x     = x,
        dummy = rep(NA,length(x))
        );
    DF.temp[,'color.index'] <- apply(
        X      = DF.temp,
        MARGIN = 1,
        FUN    = function(z) { return( min(which(z[1] < DF.colours$value)) ) }
        );
    return( as.numeric(DF.temp[,'color.index']) );
    }

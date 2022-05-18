
generate.geo.heatmaps <- function(
    data.sets       = NULL,
    GDB.SpatialData = NULL,
    dir.aridity     = NULL
    ){

    thisFunctionName <- "generate.geo.heatmaps";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.data.set in data.sets ) {

        cat("\n### processing:",temp.data.set,"\n");

        SF.stats <- getData.ts.stats(
            GDB.SpatialData = GDB.SpatialData,
            CSV.ts.stats    = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
            parquet.output  = paste0("data-",temp.data.set,".parquet")
            );

        cat("\nstr(SF.stats)\n");
        print( str(SF.stats)   );

        numeric.colnames <- setdiff(colnames(SF.stats),c("pointID","Shape"));
        for ( temp.colname in numeric.colnames ) {

            palette.mid.point <- 0;

            upper.palette.colours <- c('green','green','yellow','orange','red','red');
            lower.palette.colours <- c('violet','navy','blue3','blue','green4','green');

            if ( temp.colname == "PValue" ) {
                palette.mid.point     <- 0.05;
                upper.palette.colours <- c('grey25','grey25');
                lower.palette.colours <- c('orange','grey25');
            } else if ( temp.data.set == "WaterStress" ) {
                # upper.palette.colours <- c('green','yellow','yellow','orange','red');
                # lower.palette.colours <- c('violet','navy','blue3','blue','green','green');
                if ( temp.colname == "TestZ" ) { palette.mid.point <- -1.25; }
                }

            plot.geo.heatmap(
                data.set              = temp.data.set,
                SF.input              = SF.stats,
                variable              = temp.colname,
                palette.mid.point     = palette.mid.point,
                upper.palette.colours = upper.palette.colours,
                lower.palette.colours = lower.palette.colours,
                upper.palette.size    = 1000,
                lower.palette.size    = 1000,
                dots.per.inch         = 300
                );

            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

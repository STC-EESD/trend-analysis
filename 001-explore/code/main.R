
command.arguments <- commandArgs(trailingOnly = TRUE);
data.directory    <- normalizePath(command.arguments[1]);
code.directory    <- normalizePath(command.arguments[2]);
output.directory  <- normalizePath(command.arguments[3]);

print( data.directory );
print( code.directory );
print( output.directory );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

start.proc.time <- proc.time();

# set working directory to output directory
setwd( output.directory );

##################################################
require(sf);
require(arrow);
require(ggplot2);
require(raster);

# require(stringr);
# require(ncdf4);
# require(openssl);
# require(terrainr);
# require(sf);
# require(tidyr);

# source supporting R code
code.files <- c(
    "get-DF-dates.R",
    "getData-ts-stats.R",
    "getData-aridity.R",
    "getData-water.R",
    "initializePlot.R",
    "plot-geo-heatmap.R",
    "utils-rgb.R"
    # "compute-fpc-scores.R",
    # "getData-colour-scheme.R",
    # "getData-geojson.R",
    # "persist-fpc-scores.R",
    # "plot-RGB-fpc-scores.R",
    # "preprocess-training-data.R",
    # "train-fpc-FeatureEngine.R",
    # "visualize-fpc-approximations.R",
    # "visualize-training-data.R",
    # "tiff2parquet.R",
    );

for ( code.file in code.files ) {
    source(file.path(code.directory,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my.seed <- 7654321;
set.seed(my.seed);

is.macOS  <- grepl(x = sessionInfo()[['platform']], pattern = 'apple', ignore.case = TRUE);
n.cores   <- ifelse(test = is.macOS, yes = 2, no = parallel::detectCores() - 1);
cat(paste0("\n# n.cores = ",n.cores,"\n"));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
GDB.SpatialData <- file.path(data.directory,"2022-05-04-hugo","SpatialData.gdb");

dir.water   <- file.path(data.directory,"2022-05-04-hugo");
dir.aridity <- file.path(data.directory,"2022-05-06-aridity");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.metadata <- data.frame(
#     variable      = c('mp.ET','m.prec'),
#     units         = c('millimeter','millimeter'),
#     sub.directory = c('MELAKE','MPREC'),
#     description   = c('monthly.potential.evapotranspiration','monthly.precipitation')
#     );
#
# ncdf4.water <- "data-MELAKE-MPREC.nc";
# getData.water(
#     GDB.SpatialData = GDB.SpatialData,
#     dir.water       = dir.water,
#     DF.metadata     = DF.metadata,
#     ncdf4.output    = ncdf4.water
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.dates <- get.DF.dates();
arrow::write_parquet(x = DF.dates, sink = "DF-dates.parquet");

cat("\nstr(DF.dates)\n");
print( str(DF.dates)   );

cat("\nsummary(DF.dates)\n");
print( summary(DF.dates)   );

SF.SpatialData <- sf::st_read(GDB.SpatialData);
SF.SpatialData <- cbind(SF.SpatialData,sf::st_coordinates(SF.SpatialData));
colnames(SF.SpatialData) <- gsub(
    x           = colnames(SF.SpatialData),
    pattern     = "pointid",
    replacement = "pointID"
    );
colnames(SF.SpatialData) <- gsub(
    x           = colnames(SF.SpatialData),
    pattern     = "^X$",
    replacement = "x"
    );
colnames(SF.SpatialData) <- gsub(
    x           = colnames(SF.SpatialData),
    pattern     = "^Y$",
    replacement = "y"
    );
arrow::write_parquet(x = SF.SpatialData, sink = "SF-SpatialData.parquet");

cat("\nstr(SF.SpatialData)\n");
print( str(SF.SpatialData)   );

cat("\nsummary(SF.SpatialData)\n");
print( summary(SF.SpatialData)   );

DF.metadata <- data.frame(
    variable      = c('deficit','stress'),
    units         = c('numeral','numeral'),
    sub.directory = c('Water_Deficit_TXT','Water_Stress_TXT'),
    description   = c('deficit','stress')
    );

ncdf4.aridity <- "data-aridity.nc";
getData.aridity(
    SF.SpatialData = SF.SpatialData,
    dir.aridity    = dir.aridity,
    DF.dates       = DF.dates,
    DF.metadata    = DF.metadata,
    ncdf4.output   = ncdf4.aridity
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

quit(save = "no");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.sets <- c("WaterDeficit","WaterStress");

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

        # palette.colours = c('Navy','Blue','Green','Yellow','Red'),

        # upper.palette.colours <- c('grey25','grey50','yellow','orange','red','red');
        # lower.palette.colours <- c('cyan1','cyan2','cyan3','cyan4','grey50','grey25');

        # upper.palette.colours <- c('grey25','grey50','yellow','orange','red','red');
        # lower.palette.colours <- c('cyan1','cyan2','cyan3','cyan4','grey50','grey25');

        upper.palette.colours <- c('green','green','yellow','orange','red','red');
        lower.palette.colours <- c('violet','navy','blue3','blue','green4','green');

        if ( temp.colname == "PValue" ) {
            palette.mid.point     <- 0.05;
            upper.palette.colours <- c('grey25','grey25');
            lower.palette.colours <- c('orange','grey25');
        } else if ( temp.data.set == "WaterStress" & temp.colname == "TestZ" ) {
            palette.mid.point <- -1.25;
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#   FILE.mprec.2016.09 <- file.path(data.directory,"2022-05-04-hugo","MPREC","MPREC_2016_09.bil");
# RASTER.mprec.2016.09 <- raster::raster(FILE.mprec.2016.09);
#     DF.mprec.2016.09 <- cbind(raster::coordinates(RASTER.mprec.2016.09),raster::getValues(RASTER.mprec.2016.09));
#
# cat("\nraster::crs(RASTER.mprec.2016.09)\n");
# print( raster::crs(RASTER.mprec.2016.09)   );
#
# cat("\nstr(DF.mprec.2016.09)\n");
# print( str(DF.mprec.2016.09)   );
#
# cat("\nsummary(DF.mprec.2016.09)\n");
# print( summary(DF.mprec.2016.09)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

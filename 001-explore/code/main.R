
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
    "getData-ts-stats.R",
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
dir.aridity <- file.path(data.directory,"2022-05-06-aridity","From_Zdenek");

GDB.SpatialData <- file.path(data.directory,"2022-05-04-hugo","SpatialData.gdb")

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
SF.stats.water.deficit <- getData.ts.stats(
    GDB.SpatialData = GDB.SpatialData,
    CSV.ts.stats    = file.path(dir.aridity,"WaterDeficit.csv"),
    parquet.output  = "data-water-deficit.parquet"
    );

summary(SF.stats.water.deficit);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
plot.geo.heatmap(
    SF.input      = SF.stats.water.deficit,
    variable      = "TestZ",
    dots.per.inch = 300
    );

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

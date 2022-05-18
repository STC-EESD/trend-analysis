
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
require(arrow);
require(ggplot2);
require(ncdf4);
require(raster);
require(sf);
require(stringr);
require(terrainr);
require(tidyquant);
require(tidyr);
require(zoo);

# require(openssl);

# source supporting R code
code.files <- c(
    "generate-geo-heatmaps.R",
    "generate-timeplots.R",
    "get-DF-coordinates.R",
    "get-DF-dates.R",
    "getData-ts-stats.R",
    "getData-aridity.R",
    "getData-water.R",
    "initializePlot.R",
    "plot-geo-heatmap.R",
    "utils-rgb.R"
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

data.sets <- c("WaterDeficit","WaterStress");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
date.reference      <- as.Date("1970-01-01", tz = "UTC");
parquet.dates       <- "DF-dates.parquet";
parquet.SpatialData <- "SF-SpatialData.parquet";
ncdf4.water         <- "data-MELAKE-MPREC.nc";
ncdf4.aridity       <- "data-aridity.nc";

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.dates <- get.DF.dates(
    date.reference = date.reference
    );
arrow::write_parquet(x = DF.dates, sink = parquet.dates);

temp.list <- get.DF.coordinates(
    GDB.SpatialData     = GDB.SpatialData,
    parquet.SpatialData = parquet.SpatialData
    );
SF.SpatialData         <- temp.list[['SF.SpatialData'        ]];
get.coordinate.indexes <- temp.list[['get.coordinate.indexes']];
rm(list = "temp.list"); gc();

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.metadata <- data.frame(
    variable      = c('mp.ET','m.prec'),
    units         = c('millimeter','millimeter'),
    sub.directory = c('MELAKE','MPREC'),
    description   = c('monthly.potential.evapotranspiration','monthly.precipitation')
    );

getData.water(
    dir.water      = dir.water,
    DF.metadata    = DF.metadata,
    DF.dates       = DF.dates,
    date.reference = date.reference,
    ncdf4.output   = ncdf4.water
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.metadata <- data.frame(
    variable      = c('deficit','stress'),
    units         = c('numeral','numeral'),
    sub.directory = c('Water_Deficit_TXT','Water_Stress_TXT'),
    description   = c('deficit','stress')
    );

getData.aridity(
    SF.SpatialData = SF.SpatialData,
    DF.dates       = DF.dates,
    dir.aridity    = dir.aridity,
    DF.metadata    = DF.metadata,
    ncdf4.output   = ncdf4.aridity
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# generate.geo.heatmaps(
#     data.sets       = data.sets,
#     GDB.SpatialData = GDB.SpatialData,
#     dir.aridity     = dir.aridity
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
generate.timeplots(
    data.sets              = data.sets,
    ncdf4.input            = ncdf4.aridity,
    get.coordinate.indexes = get.coordinate.indexes,
    threshold.top          = 3.75,
    threshold.zero         = 1e-2,
    threshold.bottom       = -10
    );

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

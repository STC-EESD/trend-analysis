
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
require(tidyr);
require(zoo);

# require(openssl);
# require(tidyquant);

# source supporting R code
code.files <- c(
    "check-statistics.R",
    "explore-time-series.R",
    "generate-geo-heatmaps.R",
    "generate-timeplots.R",
    "get-DF-coordinates.R",
    "get-DF-dates.R",
    "getData-aridity.R",
    "getData-linear-arima.R",
    "getData-ts-stats.R",
    "getData-water.R",
    "initializePlot.R",
    "nc-apply-3to2D.R",
    "pixelwise-time-series-analysis.R",
    "plot-geo-heatmap.R",
    "test-array-3D-to-2D.R",
    "test-pixelwise-time-series-analysis.R",
    "utils-rgb.R",
    "verify-ncdf4-object.R"
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
date.reference                       <- as.Date("1970-01-01", tz = "UTC");
parquet.dates                        <- "DF-dates.parquet";
parquet.coordinates                  <- "SF-coordinates.parquet";
ncdf4.water                          <- "data-MELAKE-MPREC.nc";
ncdf4.aridity                        <- "data-aridity.nc";
RData.get.integer.coordinate.indexes <- "get-integer-coordinate-indexes.RData";

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.dates <- get.DF.dates(
    date.reference = date.reference
    );
arrow::write_parquet(x = DF.dates, sink = parquet.dates);

temp.list <- get.DF.coordinates(
    GDB.SpatialData     = GDB.SpatialData,
    parquet.coordinates = parquet.coordinates
    );
SF.coordinates         <- temp.list[['SF.coordinates'        ]];
get.coordinate.indexes <- temp.list[['get.coordinate.indexes']];

base::saveRDS(
    file   = RData.get.integer.coordinate.indexes,
    object = temp.list[['get.integer.coordinate.indexes']]
    );

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
    GDB.SpatialData = GDB.SpatialData,
    SF.coordinates  = SF.coordinates,
    DF.dates        = DF.dates,
    dir.aridity     = dir.aridity,
    DF.metadata     = DF.metadata,
    ncdf4.output    = ncdf4.aridity
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# verify.ncdf4.object(
#     ncdf4.input            = ncdf4.aridity,
#     dir.aridity            = dir.aridity,
#     DF.coordinates         = sf::st_drop_geometry(SF.coordinates),
#     DF.dates               = DF.dates,
#     get.coordinate.indexes = get.coordinate.indexes,
#     DF.metadata            = data.frame(
#         varid     = c('deficit',          'stress'          ),
#         directory = c('Water_Deficit_TXT','Water_Stress_TXT')
#         )
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
for ( temp.data.set in data.sets ) {
    cat("\n### processing (Sen Slope):",temp.data.set,"\n");
    temp.stem <- stringr::str_extract(string = tolower(temp.data.set), pattern = "(deficit|stress)");
    SF.stats <- getData.ts.stats(
        SF.coordinates = SF.coordinates,
        CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
        parquet.output = paste0("SF-ZP-",temp.stem,"-SenSlope.parquet")
        );
    remove(list = c("SF.stats"));
    }

for ( temp.folder in c('Water_Deficit_Xls','Water_Stress_Xls') ) {
    cat("\n### processing (linear, ARIMA):",temp.folder,"\n");
    temp.stem <- stringr::str_extract(string = tolower(temp.folder), pattern = "(deficit|stress)");
    getData.linear.arima(
        SF.coordinates = SF.coordinates,
        directory      = file.path(dir.aridity,temp.folder),
        parquet.linear = paste0("SF-ZP-",temp.stem,"-linear.parquet"),
        parquet.arima  = paste0("SF-ZP-",temp.stem,"-arima.parquet")
        );
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# generate.geo.heatmaps(
#     data.sets      = data.sets,
#     SF.coordinates = SF.coordinates,
#     dir.aridity    = dir.aridity
#     );
#
# generate.timeplots(
#     data.sets              = data.sets,
#     ncdf4.input            = ncdf4.aridity,
#     get.coordinate.indexes = get.coordinate.indexes,
#     SF.coordinates         = SF.coordinates,
#     threshold.top          = 3.75,
#     threshold.zero         = 1e-2,
#     threshold.bottom       = -10
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# explore.time.series(
#     data.sets              = data.sets,
#     DF.dates               = DF.dates,
#     SF.coordinates         = SF.coordinates,
#     ncdf4.aridity          = ncdf4.aridity,
#     FILE.coords.to.indexes = "get-integer-coordinate-indexes.RData"
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
test_pixelwise.time.series.analysis(
    data.sets              = data.sets,
    DF.dates               = DF.dates,
    SF.coordinates         = SF.coordinates,
    ncdf4.aridity          = ncdf4.aridity,
    FILE.coords.to.indexes = RData.get.integer.coordinate.indexes,
    FUN.pixelwise          = pixelwise.time.series.analysis
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
check.statistcs(
    varid          = "deficit",
    SF.coordinates = SF.coordinates,
    ncdf4.aridity  = ncdf4.aridity,
    FUN.pixelwise  = pixelwise.time.series.analysis
    );

# check.statistcs(
#     varid          = "stress",
#     SF.coordinates = SF.coordinates,
#     ncdf4.aridity  = ncdf4.aridity,
#     FUN.pixelwise  = pixelwise.time.series.analysis
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

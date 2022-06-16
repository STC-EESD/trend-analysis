
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
    "explore-time-series.R",
    "generate-geo-heatmaps.R",
    "generate-timeplots.R",
    "get-DF-coordinates.R",
    "get-DF-dates.R",
    "getData-ts-stats.R",
    "getData-aridity.R",
    "getData-water.R",
    "initializePlot.R",
    "nc-apply-3to2D.R",
    "pixel-time-series-analysis.R",
    "plot-geo-heatmap.R",
    "test-array-3D-to-2D.R",
    "test-pixel-time-series-analysis.R",
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

base::saveRDS(
    file   = "get-integer-coordinate-indexes.RData",
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
    SF.SpatialData = SF.SpatialData,
    DF.dates       = DF.dates,
    dir.aridity    = dir.aridity,
    DF.metadata    = DF.metadata,
    ncdf4.output   = ncdf4.aridity
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# verify.ncdf4.object(
#     ncdf4.input            = ncdf4.aridity,
#     dir.aridity            = dir.aridity,
#     DF.SpatialData         = sf::st_drop_geometry(SF.SpatialData),
#     DF.dates               = DF.dates,
#     get.coordinate.indexes = get.coordinate.indexes,
#     DF.metadata            = data.frame(
#         varid     = c('deficit',          'stress'          ),
#         directory = c('Water_Deficit_TXT','Water_Stress_TXT')
#         )
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
for ( temp.data.set in data.sets ) {
    cat("\n### processing:",temp.data.set,"\n");
    SF.stats <- getData.ts.stats(
        SF.SpatialData = SF.SpatialData,
        CSV.ts.stats   = file.path(dir.aridity,"From_Zdenek",paste0(temp.data.set,".csv")),
        parquet.output = paste0("SF-",temp.data.set,".parquet")
        );
    cat("\nstr(SF.stats)\n");
    print( str(SF.stats)   );
    remove(list = c("SF.stats"));
    }

# generate.geo.heatmaps(
#     data.sets      = data.sets,
#     SF.SpatialData = SF.SpatialData,
#     dir.aridity    = dir.aridity
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# generate.timeplots(
#     data.sets              = data.sets,
#     ncdf4.input            = ncdf4.aridity,
#     get.coordinate.indexes = get.coordinate.indexes,
#     SF.SpatialData         = SF.SpatialData,
#     threshold.top          = 3.75,
#     threshold.zero         = 1e-2,
#     threshold.bottom       = -10
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# explore.time.series(
#     data.sets              = data.sets,
#     DF.dates               = DF.dates,
#     SF.SpatialData         = SF.SpatialData,
#     ncdf4.aridity          = ncdf4.aridity,
#     FILE.coords.to.indexes = "get-integer-coordinate-indexes.RData"
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# my.FUN <- function(x) {
#     if(any(is.na(x))) {return(c(NA,NA))};
#     return(c(mean(x),var(x)))
#     }
#
# nc.obj.aridity <- ncdf4::nc_open(filename = ncdf4.aridity);
# ARRAY.deficit  <- ncdf4::ncvar_get(nc = nc.obj.aridity, varid = "deficit");
# ARRAY.temp     <- apply(X = ARRAY.deficit, MARGIN = c(1,2), FUN = my.FUN);
# ARRAY.temp     <- base::aperm(a = ARRAY.temp, perm = c(2,3,1));
# ncdf4::nc_close(nc = nc.obj.aridity);
#
# cat("\nstr(ARRAY.deficit)\n");
# print( str(ARRAY.deficit)   );
#
# cat("\nstr(ARRAY.temp)\n");
# print( str(ARRAY.temp)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
test_array.3D.to.2D();

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# test_pixel.timeq.series.analysis(
#     data.sets              = data.sets,
#     DF.dates               = DF.dates,
#     SF.SpatialData         = SF.SpatialData,
#     ncdf4.aridity          = ncdf4.aridity,
#     FILE.coords.to.indexes = "get-integer-coordinate-indexes.RData",
#     FUN.pixel              = pixel.timeq.series.analysis
#     );
#
# nc.obj.aridity <- ncdf4::nc_open(filename = ncdf4.aridity);
# ARRAY.deficit  <- ncdf4::ncvar_get(nc = nc.obj.aridity, varid = "deficit");
# ARRAY.temp     <- apply(X = ARRAY.deficit, MARGIN = c(1,2), FUN = pixel.time.series.analysis);
# ARRAY.temp     <- base::aperm(a = ARRAY.temp, perm = c(2,3,1));
# ncdf4::nc_close(nc = nc.obj.aridity);
#
# cat("\nstr(ARRAY.deficit)\n");
# print( str(ARRAY.deficit)   );
#
# cat("\nstr(ARRAY.temp)\n");
# print( str(ARRAY.temp)   );
#
# base::saveRDS(
#     object = ARRAY.temp,
#     file   = "array-temp.RData"
#     );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.ts.deficit <- nc_apply_3to2D(
    nc             = ncdf4.aridity,
    varid          = "deficit",
    MARGIN         = c(1,2),
    FUN            = pixel.time.series.analysis,
    parquet.output = "DF-ts-deficit.parquet"
    );

DF.ts.deficit <- sf::st_drop_geometry(dplyr::left_join(
    x  = SF.SpatialData,
    y  = DF.ts.deficit,
    by = c('x','y')
    ));

cat("\nstr(DF.ts.deficit)\n");
print( str(DF.ts.deficit)   );

cat("\nsummary(DF.ts.deficit)\n");
print( summary(DF.ts.deficit)   );

SF.stats.deficit <- sfarrow::st_read_parquet("SF-WaterDeficit.parquet");

cat("\nstr(SF.stats.deficit)\n");
print( str(SF.stats.deficit)   );

cat("\nsummary(SF.stats.deficit)\n");
print( summary(SF.stats.deficit)   );

DF.check <- sf::st_drop_geometry(dplyr::left_join(
    x  = SF.stats.deficit,
    y  = DF.ts.deficit,
    by = c('pointID','x','y')
    ));

DF.check[,'check.Sen.Slope'  ] <- abs(DF.check[,'Sen.Slope'  ] - DF.check[,'SenQ'  ]);
DF.check[,'check.Z.statistic'] <- abs(DF.check[,'Z.statistic'] - DF.check[,'TestZ' ]);
DF.check[,'check.p.value'    ] <- abs(DF.check[,'p.value'    ] - DF.check[,'PValue']);

cat("\nsummary(DF.check[,c('check.Sen.Slope','check.Z.statistic','check.p.value')])\n");
print( summary(DF.check[,c('check.Sen.Slope','check.Z.statistic','check.p.value')])   );

# selected.colnames <- c('pointID','x','y','SenQ','Sen.Slope','TestZ','Z.statistic','PValue','p.value','check.Sen.Slope','check.Z.statistic','check.p.value');
# selected.colnames <- c('pointID','x','y','SenQ','Sen.Slope','TestZ','Z.statistic','PValue','p.value');
selected.colnames <- c('pointID','x','y','TestZ','Z.statistic','PValue','p.value');
is.selected <- (DF.check[,'check.Z.statistic'] > 1e-4) | (DF.check[,'check.p.value'] > 1e-4);
cat("\nDF.check[is.selected,selected.colnames]\n");
print( DF.check[is.selected,selected.colnames]   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

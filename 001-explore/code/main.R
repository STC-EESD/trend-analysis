
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
DF.KC.deficit.ts.stats <- nc_apply_3to2D(
    nc             = ncdf4.aridity,
    varid          = "deficit",
    MARGIN         = c(1,2),
    FUN            = pixelwise.time.series.analysis,
    parquet.output = "DF-KC-deficit-ts-tats.parquet"
    );

DF.KC.deficit.ts.stats <- sf::st_drop_geometry(dplyr::left_join(
    x  = SF.coordinates,
    y  = DF.KC.deficit.ts.stats,
    by = c('x','y')
    ));

cat("\nstr(DF.KC.deficit.ts.stats)\n");
print( str(DF.KC.deficit.ts.stats)   );

cat("\nsummary(DF.KC.deficit.ts.stats)\n");
print( summary(DF.KC.deficit.ts.stats)   );

SF.ZP.deficit.SenSlope <- sfarrow::st_read_parquet("SF-ZP-deficit-SenSlope.parquet");
SF.ZP.deficit.linear   <- sfarrow::st_read_parquet("SF-ZP-deficit-linear.parquet");

cat("\nstr(SF.ZP.deficit.SenSlope)\n");
print( str(SF.ZP.deficit.SenSlope)   );

cat("\nsummary(SF.ZP.deficit.SenSlope)\n");
print( summary(SF.ZP.deficit.SenSlope)   );

cat("\nstr(SF.ZP.deficit.linear)\n");
print( str(SF.ZP.deficit.linear)   );

cat("\nsummary(SF.ZP.deficit.linear)\n");
print( summary(SF.ZP.deficit.linear)   );

DF.check <- dplyr::left_join(
    x  = sf::st_drop_geometry(SF.ZP.deficit.SenSlope),
    y  = sf::st_drop_geometry(SF.ZP.deficit.linear),
    by = c('pointID','x','y')
    );

DF.check <- dplyr::left_join(
    x  = DF.check,
    y  = DF.KC.deficit.ts.stats,
    by = c('pointID','x','y')
    );

FUN.relative.error <- function(x) {
    if ( all(x == c(0,0)) ) {
        return( 0 );
    } else {
        return( abs(diff(x)/mean(x)) )
        }
    }

DF.check[,'check.Sen.Slope'  ] <- apply(X = DF.check[,c('Sen.slope',  'SenQ'  )], MARGIN = 1, FUN = FUN.relative.error);
DF.check[,'check.smk.z.stat' ] <- apply(X = DF.check[,c('smk.z.stat', 'TestZ' )], MARGIN = 1, FUN = FUN.relative.error);
DF.check[,'check.smk.p.value'] <- apply(X = DF.check[,c('smk.p.value','PValue')], MARGIN = 1, FUN = FUN.relative.error);

cat("\nsummary(DF.check[,c('check.Sen.Slope','check.smk.z.stat','check.smk.p.value')])\n");
print( summary(DF.check[,c('check.Sen.Slope','check.smk.z.stat','check.smk.p.value')])   );

is.selected <- (DF.check[,'check.smk.z.stat'] > 1e-2);
cat("\nsum(is.selected)\n");
print( sum(is.selected)   );

selected.colnames <- c('pointID','TestZ','smk.z.stat','check.smk.z.stat');
cat("\nDF.check[is.selected,selected.colnames]\n");
print( DF.check[is.selected,selected.colnames]   );

is.selected <- (DF.check[,'check.smk.p.value'] > 1e-2);
cat("\nsum(is.selected)\n");
print( sum(is.selected)   );

selected.colnames <- c('pointID','PValue','smk.p.value','check.smk.p.value');
DF.temp <- DF.check[is.selected,];
saveRDS(file = "tmp-check.RData", object = DF.temp);

DF.temp[,'PValue'] <- format(x = DF.temp[,'PValue'], scientific = TRUE);
cat("\nDF.check[is.selected,selected.colnames]\n");
print( DF.temp[ 1:100,      selected.colnames]   );

DF.temp <- DF.check[is.selected,];
DF.temp[,'smk.p.value'] <- round( x = DF.temp[,'smk.p.value'], digits = 5);
DF.temp[,'check.smk.p.value'] <- apply(X = DF.temp[,c('smk.p.value','PValue')], MARGIN = 1, FUN = FUN.relative.error);
DF.temp[,'PValue'] <- format(x = DF.temp[,'PValue'], scientific = TRUE);
is.selected <- (DF.temp[,'check.smk.p.value'] > 9e-2);
cat("\nsum(is.selected)\n");
print( sum(is.selected)   );

DF.temp       <- DF.temp[is.selected,];
display.order <- order(DF.temp[,'check.smk.p.value'], decreasing = TRUE);
cat("\nDF.temp[display.order,selected.colnames]\n");
print( DF.temp[display.order,selected.colnames]   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

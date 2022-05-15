
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

# require(openssl);

# source supporting R code
code.files <- c(
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
date.reference      <- as.Date("1970-01-01", tz = "UTC");
parquet.dates       <- "DF-dates.parquet";
parquet.SpatialData <- "SF-SpatialData.parquet";
ncdf4.water         <- "data-MELAKE-MPREC.nc";
ncdf4.aridity       <- "data-aridity.nc";

upper.palette.colours <- c('green','green','yellow','orange','red','red');
lower.palette.colours <- c('violet','navy','blue3','blue','green4','green');

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

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

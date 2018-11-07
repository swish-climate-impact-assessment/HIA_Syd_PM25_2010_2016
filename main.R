## HIA_Syd_PM25_2010_2016
## ivanhanigan
## 20181107

#### settings ####
datadir <- "/home/public_share_data/ResearchData_CAR/Environment_General"
projdir <- "~/ownCloud/projects_transfers/HIA_Health_Impact_Assessments/HIA_Syd_PM25_2010_2016"
setwd(projdir)
dir.create("working_temporary")
dir.create("data_derived")
state <- "NSW"
timepoint <- 2016

## libraries
source("code/func.R")


#### 1: population and health data ####
indir_dth_sa2 <- file.path(datadir, "Australian_Mortality_ABS/ABS_MORT_2006_2016/data_derived")
flist <- dir(indir_dth_sa2)
flist
infile_dth_sa2 <- flist[grep(tolower(state), flist)]
indat_dth_sa2 <- read.csv(file.path(indir_dth_sa2, infile_dth_sa2 ))
str(indat_dth_sa2)
# this has death counts (no.), pop (persons) and standardised rates (rate_xxx)

#### 2: spatial boundaries ####
indir_sa2_shp <- file.path(datadir,"ABS_data/ABS_Census_2016/abs_sa2_2016_data_derived")
dir(indir_sa2_shp)
infile_sa2_shp <- sprintf("SA2_2016_%s", state)
indat_sa2_shp <- readOGR(indir_sa2_shp, infile_sa2_shp)
str(indat_sa2_shp@data)

## we have different names so harmonise these
indat_sa2_shp@data$SA2_MAINCODE_2016 <- as.numeric(as.character(indat_sa2_shp@data$SA2_MAIN16))

indat_dth_sa2$SA2_MAINCODE_2016 <- as.numeric(as.character(indat_dth_sa2$ASGScode))
str(indat_dth_sa2[,c(ncol(indat_dth_sa2), 1:2, grep(timepoint, names(indat_dth_sa2)))])

## join
indat_dth_sa2V2 <- indat_sa2_shp
indat_dth_sa2V2@data <- merge(indat_dth_sa2V2@data, indat_dth_sa2, by = "SA2_MAINCODE_2016")
str(indat_dth_sa2V2@data)

## subset to greater city centre
indat_dth_sa2V3 <- indat_dth_sa2V2[indat_dth_sa2V2@data$GCC_NAME16 == "Greater Sydney",]

## visualise with QGIS
writeOGR(indat_dth_sa2V3, "working_temporary", 
         sprintf("indat_dth_sa2V3_%s", state),
         driver = "ESRI Shapefile", overwrite_layer = T)


#### 3: monitors interpolated ####
indir_mon_stns <- file.path(datadir, "Air_Pollution_Monitoring_Stations_NSW/OEH_monitor_Sydney_PM25_1996_2014/data_derived_sydney_pm25_grids_2013_2014")
dir(indir_mon_stns)
infile_mon_stns <- "sydney_pm25_grids_2013_2014_station_data"
input_data <- readOGR(indir_mon_stns, infile_mon_stns)
str(input_data@data)

outputfilename <- "data_derived/sydney_pm25_2014_krige_20181107.tif"
## For a raster prediction, load a template
## get the extent of the points
bb <- indat_dth_sa2V3@bbox
## resolution in decimal degrees (0.01 = 1km)
res <- 0.01
## open the file code/do_kriging.R and follow instructions
## or just source this
source("code/do_kriging.R")
## from checking this looks like IDW is a more sensible result

#### 4: SEIFA ####
indir_seifa <- file.path(datadir, "ABS_data/ABS_SEIFA/ABS_SEIFA_2011_SA2_data_derived")
dir(indir_seifa)
infile_seifa <- "ABS_SEIFA_2011_IRSD_SA2.csv" 
indat_seifa <- read.csv(file.path(indir_seifa, infile_seifa))
str(indat_seifa)

## note that this is 2011 data, so need other spatial file
indir_sa2_2011 <- file.path(datadir, "ABS_data/ABS_Census_2011/abs_sa2_2011_data_provided")
dir(indir_sa2_2011)
infile_sa2_2011 <- "SA2_2011_AUST"
indat_sa2_2011 <- readOGR(indir_sa2_2011, infile_sa2_2011)  
str(indat_sa2_2011@data)
## match name to join
indat_sa2_2011@data$sa2 <- as.numeric(as.character(indat_sa2_2011@data$SA2_MAIN11))

## join
indat_sa2_2011V2 <- indat_sa2_2011
indat_sa2_2011V2@data <- merge(indat_sa2_2011V2@data, indat_seifa)
str(indat_sa2_2011V2@data)

#### final merge ####
## merge pop/health with pm25
r2 <- velox::velox(r)
pm25 <- r2$extract(indat_dth_sa2V3, fun = function(x) mean(x, na.rm = T))
indat_dth_sa2V4 <- indat_dth_sa2V3
indat_dth_sa2V4@data <- cbind(indat_dth_sa2V3@data, pm25)
str(indat_dth_sa2V4@data)

## merge with seifa 
pts <- rgeos::gCentroid(indat_dth_sa2V4, byid = T)
plot(pts)
ptsV2 <- sp::over(pts, indat_sa2_2011V2)
str(ptsV2)

linkd_pop_health_pm25_seifa <- cbind(indat_dth_sa2V4@data, ptsV2)
str(linkd_pop_health_pm25_seifa)
## add the centroid coords
linkd_pop_health_pm25_seifaV2 <- data.frame(linkd_pop_health_pm25_seifa, pts@coords)
str(linkd_pop_health_pm25_seifaV2)

#### exploratory analysis ####
## short names are better for modelling
ana <- linkd_pop_health_pm25_seifaV2
str(ana)
with(ana, plot(pm25, rate_2014))
ana$irsd_dec <- as.factor(ana$irsd_decile_ste)
with(ana, plot(irsd_dec, rate_2014))

fit <- mgcv::gam(ana$rate_2014 ~ s(pm25) + irsd_dec, data = ana)
summary(fit)
par(mfrow = c(2,1))
plot(fit, all.terms = T)

library(gstat)
library(rgdal)
library(sp)
library(automap)
library(raster)


## Select the name of the column we want
var <- "X2014"

proj4string(input_data)

insp <- input_data[!is.na(input_data@data[,var]),var]
##insp@data
##class(insp)
    
## This is how to autofit a variogram using the automap package
## I am creating this as a text string so that later we can do a loop
## over many vars
txt <- sprintf("vmod <- autofitVariogram(%s ~ 1,insp)", var)
cat(txt)

eval(parse(text=txt))
  
mod <- vmod$var_model
    
##   # Or else, manually fit a variogram
##   txt <- sprintf("vg <- variogram(%s ~ 1,data =insp)", var)
##   eval(parse(text=txt))
##   # See what it looks like
##   plot(vg)
##   vg
##   # Fit a model to the varigram
##   mod <- fit.variogram(object = vg,
##                        model = vgm(psill = cov_sill, model = "Exp", range = cov_range, nugget = nugget)
##   )
      
  
# Perform ordinary kriging, using the variogram calculated in “mod”
txt <- sprintf("kmod <- gstat(NULL,'%s',%s~1,insp, model=mod)", var, var)
eval(parse(text=txt))
  

#plot(insp)
#axis(1); axis(2)
x.range <- c(bb[1,])
y.range <- c(bb[2,])
  
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = res),
                   y = seq(from = y.range[1], to = y.range[2], by = res))  # expand points to grid
  
coordinates(grd) <- ~x + y
  
gridded(grd) <- TRUE

proj4string(grd) <- CRS(proj4string(input_data))

var
txt <- sprintf("idw <- idw(formula = %s ~ 1, locations = insp, newdata = grd)", var)
eval(parse(text=txt))

## apply idw model for the data
##plot(idw)

r <- raster(idw)
# now apply the kriging model to this
out_raster <- interpolate(r, kmod)
  
##plot(out_raster)
##plot(insp, add = T)
##dev.off()
##getwd()
##dir()

out_raster
proj4string(insp)

writeRaster(out_raster, outputfilename, format = "GTiff", overwrite=T)
## and for comparison just IDW
writeRaster(r, gsub("krige", "idw", outputfilename), format = "GTiff", overwrite=T)

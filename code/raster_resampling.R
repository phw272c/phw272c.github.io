
#   title: "Raster resampling"

#  This is a deep dive on raster resampling - an important aspect of spatial analyses. 
#  There are a number of ways that rasters can be resampled and the method you use is dependent on the data.

# Why do resampling - because you might need to use two or more rasters in the same analysis
# and they probably won't be the same resolution

library(raster)
library(rgeos)

#### Load and check data ####


# Let's load up the data we need. For this example, we are going to load up 3 different rasters of data from Burkina Faso. Elevation (continuous outcome), land use (categorical) and population (count - from the [WorldPop project](https://www.worldpop.org/))


BF_elev <- raster::getData("alt", country="BF")
BF_land_use <- raster("https://github.com/phw272c/phw272c.github.io/raw/master/data/BF_land_use.tif")
pop <- raster("https://github.com/phw272c/phw272c.github.io/raw/master/data/BF_pop.tif")


# let's check out what's in these "things"called rasters
#dimensions, resolution (in degrees), extent, projection, values
BF_elev
BF_land_use
pop



# First thing we notice is that, in this case, `BF_land_use` isn't in the same projection as `BF_elev` and `pop`, so let's reproject. 
# we also need to know something about the data we are importing

# Elevation is obviously continuous
# let's take a look at summary of the elevation values
# we can do a frequency table of values
freq(BF_elev)

# maybe just look at a fraction of the cell values to get an idea
summary(sampleRegular(BF_elev, size=10000))

# or alternatively 
cellStats(BF_elev, quantile)

# Land use is categorical but let's inspect to make sure
freq(BF_land_use)
table(sampleRegular(BF_land_use, size=10000))

# population is also continuous but let's check what we have
freq(pop)
cellStats(pop, quantile)

# oops, that negative value doesn't look good
# let's take a closer look
table(values(pop)<0)

#alternatively
table(pop[]<0)


# 3 cells, let's ignore that in our toy example


# remember from before that the projections were not the same so we need to reproject
crs(BF_land_use)
crs(BF_elev)


# reproject - can time a little time, ~16 second on my machine
system.time(
  BF_land_use <- projectRaster(BF_land_use, crs=crs(BF_elev), method="ngb") 
)

# Remember that as land use is categorical, we have to specify `method = "ngb"` so that nearest neighbor interpolation is used during reprojection, otherwise it will try to use binlinear interpolation which will result in meaningless values.
# !unfortunately, you have to be careful, because projectRaster has bilinear as a default and will use it with no error if you don't specify method

#### resample elevation raster ####
#For demo purposes, we are going to crop the rasters across an arbitrary bounding box. 
# notice that the full extent of BF_elev is available in the raster summary
BF_elev

# one way to use the  function extent is to give it column and row numbers
# it will return the extent
extent(BF_elev,499,507,301,309)


#now we are going to crop the raster this extent using crop
BF_elev_crop <- crop(BF_elev, extent(BF_elev,499,507, 301,309))

#and crop the other rasters to the same extents
pop_crop <- crop(pop, extent(BF_elev_crop))
BF_land_use_crop <- crop(BF_land_use, extent(BF_elev_crop))

# Now let's create a dummy lower resolution raster we are going to resample to
# this creates a lower resolution raster aggregating a number of cells horizontally and vertically using a specified function
# in this case, the default is mean which is fine for elevation
new_raster <- aggregate(BF_elev_crop, fact=3)

# as you might expect this function shifts the raster
new_raster <- raster::shift(new_raster, dx=0.004, dy=0.002)


#and this creates lines out of this lower resolution dummy raster
new_raster_outline <- rasterToPolygons(new_raster, dissolve=TRUE)


#Let's plot the cropped elevation raster and overlay the grid of the new raster we are resample to

plot(BF_elev_crop)

# plot cell outlines
lines(new_raster_outline)




#Now let's resample elevation to the new raster. Here we can specify `method = bilinear` because this type of interpolation makes sense for a continuous outcome like elevation. In fact `method = binlinear` is the default so we don't really need to specify.
# bilinear is linear interpolation in two dimensions,
# there's a good visual explainer on the wiki https://en.wikipedia.org/wiki/Bilinear_interpolation

BF_elev_crop_resampled_bilin <- resample(BF_elev_crop, new_raster, method="bilinear")
plot(BF_elev_crop_resampled_bilin)
lines(new_raster_outline)


BF_elev_crop
BF_elev_crop_resampled_bilin


# now display these side by side with the same legend
brks <- seq(270, 350, by=5) 
nb <- length(brks)-1 
cols <- rev(terrain.colors(nb))
par(mfrow=c(1,2))
plot(BF_elev_crop, breaks=brks, col=cols, lab.breaks=brks, zlim=c(270,350), main='Original') 
plot(BF_elev_crop_resampled_bilin, breaks=brks, col=cols, lab.breaks=brks,  zlim=c(270,350), main='Resampled') 



# reset to single graph plotting
par(mfrow=c(1,1))



#### Resampling land use raster ####
# Now let's repeat the resampling process with the land use raster. First let's plot and take a look at the frequency of different land use categories.
plot(BF_land_use_crop)
BF_land_use_crop
table(BF_land_use_crop[])



#As this is categorical, and given that new raster is coarser (lower resolution), first aggregate using the mode (i.e. most frequent) class. 
# Here we aggregate by a factor of 9 because the resolution of `the`new_raster` is 9 times coarser than BF_land_use_crop
BF_land_use_crop_aggregated <- aggregate(BF_land_use_crop, fun='modal', fact = 9)
plot(BF_land_use_crop_aggregated)
lines(new_raster_outline)

#Now that the raster is at the right resolution, let's resample to the same grid
BF_land_use_crop_aggregated_resamp <- resample(BF_land_use_crop_aggregated, new_raster, method = "ngb")
plot(BF_land_use_crop_aggregated_resamp)
lines(new_raster_outline)

#### Resampling population raster ####

#Now we have resampled a continuous and categorical raster, let's repeat on population. 
# The raw data in the `pop_crop` shows the number of individuals per cell. If we want to resample to a coarser resolution, we are probably most interested in maintaining these as counts and 
# therefore we want to sum the numbers as opposed to use interpolation or nearest neighbour.

#Let's have a look at the population raster
plot(pop_crop, col=topo.colors(64))
cellStats(pop_crop, sum) 

# As for the land use example, the new raster is at lower resolution. Therefore have to aggregate first. 
# As we want total population to remain the same, aggregate by summing population
pop_crop_aggregated <- aggregate(pop_crop, fact = 3, fun = sum) #3.012048

# Check the total population is the same as the original raster
cellStats(pop_crop_aggregated, sum)

# Plot with new raster cell outlines
plot(pop_crop_aggregated, col=topo.colors(64))
lines(new_raster_outline)


#Now that the raster is at the right resolution, we can use the resample function to estimate the counts in the new grid
pop_crop_aggregated_resamp <- resample(pop_crop_aggregated, new_raster)
plot(pop_crop_aggregated_resamp, col=topo.colors(64))
lines(new_raster_outline)

#The total should more or less be equal to the original - its not exact as we have to use interpolation to predict populations in the new cell. 
cellStats(pop_crop_aggregated_resamp, sum) # Not bad..

# maggieGridFuncs.R
# Created: 17 Aug 2020
# Author:  Margaret Swift <mes114@duke.edu>

# for lane

################################################################################
aggregateShapesToGrid <- function(shp, grid, types.df, desc.col) {
  # Map each grid square by %types
  # Right now I've made this the % of area w/in the park
  
  # Remove geometry for processing
  shp.nogeom <- st_set_geometry(shp, NULL)
  
  # loop over each row in shapefile to grab descriptions
  for (i in 1:nrow(shp)) {
    desc <- tolower(as.character(shp.nogeom[i, desc.col]))
    
    # map descriptions to types
    go.on <- TRUE
    for (j in 1:ncol(types.df)) {
      if (go.on) {
        types <- as.character(types.df[,j])
        type <- sum(sapply(types, function(x) grepl(x, desc)))
        if (type) {
          type <- names(types.df[j])
          go.on <- FALSE
        }
      }
    }
    
    # save types to shape
    shp$type[i] <- ifelse(type==0, NA, type)
  }
  
  types <- names(types.df)
  for (type in types) {
    grid[,type] <- 0
    my.shp <- st_union(shp[which(shp$type == type),])
    for (j in 1:nrow(grid)) {
      x <- pkgcond::suppress_messages(st_intersection(my.shp, grid$.[j]))
      x.a <- ifelse(length(x)>0, st_area(x), 0)
      grid[j,type] <- round(x.a / grid$shp_area_m2[j], 2) 
    }
  }
  return(grid)
}
as.c <- function(str) return(as.character(str))
borderPlot <- function(path, fill="transparent", color="black",
                    grid=NULL, gridcolor="gray", gridfill='transparent') {
  g <- getShape(shp_name=path)
  p <- ggplot() + 
    geom_sf(data=g$geometry, color=color, fill=fill) + 
    scale_x_continuous(breaks=c(31, 31.5, 32)) +
    theme(text = element_text(size=20))
  if (!is.null(grid)) {
    p <- p + geom_sf(data=grid$., fill=gridfill, color=gridcolor)
  }
  return(p)
}
createGrid <- function(shp, nlon=10, nlat=50, sizekm=1000, crs=crs, effort=10, latlon=T) {
  ## Create a grid of effort values, each being the fraction of area within the ##
  ## grid square that is contained within the underlying shape.##
  if (latlon) {
    grid_size <- c(nlon, nlat)
    grid <- st_make_grid(shp, n=grid_size) %>% 
      st_sf(grid_id = 1:length(.))
  } else {
    # x <- attributes(shp$geometry)$bbox
    a_km <- as.numeric(st_area(shp)) / 1e6
    grid_size <- sizekm / a_km
    grid <- st_make_grid(shp, cellsize=grid_size) %>% 
      st_sf(grid_id = 1:length(.))
  }
  grid$area_m2 <- st_area(grid$.)
  grid$area_km2 <- grid$area_m2/1e6
  
  # Assign effort to each square of grid, defined as the percentage
  # of each grid square that is contained in KNP
  L <- nrow(grid)
  for (i in 1:L) {
    x <- pkgcond::suppress_messages(st_intersection(st_geometry(shp), 
                                                    st_geometry(grid$.[i])))
    x.a <- st_area(x)
    if (!length(x.a)) x.a <- 0
    if (!length(x)) x <- 0
    grid$shp_int[i] <- x
    grid$shp_area_m2[i] <- x.a
  }
  grid$shp_area_pct <- round(as.numeric(grid$shp_area_m2 / grid$area_m2),4)
  grid <- grid[-which(grid$shp_area_m2 == 0),]
  grid <- st_transform(grid, crs=crs)
  return(grid)
}
getShape <- function(shp_name, p4s=proj4string) {
  ## Get a shape from a shapefile and transform into SF ##
  shp <- st_read(shp_name) %>% st_transform(p4s)
  return(shp)
}
isShape <- function(file, returnCols=F) {
  lon.list <- c('lon', 'long', 'longitude', 'x_coord')
  lat.list <- c('lat', 'latitude', 'y_coord')
  cols <- tolower(colnames(file)) 
  bool <- any(cols %in% c(lon.list, lat.list, 'geometry'))
  bool <- any(bool, 'sf' %in% class(file))
  if (returnCols) {
    lon.col <- which(cols %in% lon.list)
    lat.col <- which(cols %in% lat.list)
    return(c(lon.col, lat.col))
  } else { return(bool) }
}
readFile <- function(path, doGeom=T, crs=NULL) {
  path <- as.c(path)
  end <- tolower(gsub('.*\\.', '', path))
  if ( end == 'shp' ) { return(invisible(getShape(path)))
  } else if ( end == 'csv' ) { file <- read.csv(path)
  } else if ( end == 'txt' ) { file <- read.table(path, header=T)
  } else if ( end == 'rds' ) { file <- readRDS(path)
  } else if ( end == 'rdata' ) { file <- load(path)
  } else if ( end == 'dbf' ) { file <- read.dbf(path)
  } else { stop(paste0('Path extension "', end,
                       '" not found. Cannot read file at this time.')) }
  
  # Return with coords if necessary
  makeShape <- !("geometry" %in% colnames(file))
  if (isShape(file) && makeShape && doGeom) {
    cols <- isShape(file, returnCols=T)
    loncol <- file[,cols[1]]
    latcol <- file[,cols[2]]
    file <- sfFromLongLat(file, cols[1], cols[2], crs=crs)
    file$lon <- loncol
    file$lat <- latcol
  }
  return(file)
}
sfFromLongLat <- function(data, long, lat, crs=crs) {
  # Take long/lat points and make them into a simple features type
  sf <- st_as_sf(data, coords=c(long, lat), crs=crs)
  return(sf)
}
sfPointsInPoly <- function(points, sf, col) {
  # Find which polygon each point is in; set NA for edges
  insects <- st_intersects(points, sf, sparse = FALSE)
  inx <- vector(mode="numeric", length=nrow(insects))
  for (i in 1:nrow(insects)) {
    val <- which(insects[i,])
    inx[i] <- ifelse(!length(val), NA, val)
  }
  
  # Edge cases need to use distance; this is slower!!
  inx.na <- which(is.na(inx))
  dists <- st_distance(points$geometry[inx.na], sf)
  inx[inx.na] <- whichMin(dists)
  
  # Return assignments
  sf$geometry <- NULL
  result <- as.character(sf[inx,col])
  return(result)
}

################################################################################
# Usage
pacman::p_load('sf')
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Map params should be appropriate for your data
proj4string <- "+proj=longlat +zone=36 +ellps=WGS84 +datum=WGS84"
crs <- CRS(proj4string)

# path is your points
pts.path <- "buffalo_1979.csv"
bord.path <- "border/knpBorder.shp"

# read your files
df <- readFile(pts.path, crs=crs)
bord <- readFile(bord.path, crs=crs)

# make your grid; function has sizekm OR nLat depending on how you want to define
grid <- createGrid(shp=bord, latlon=F, sizekm=1000, crs=crs) 

# Find the intersection of points with grid cells
inx <- st_intersects(df$geometry, grid$.) #ignore the errors
df$grid_id <- sapply(inx, function(x) grid$grid_id[x])

# plot 'em
borderPlot(path=bord.path, grid=grid) + geom_sf(data=df)  #aes(size=count, color=species...)

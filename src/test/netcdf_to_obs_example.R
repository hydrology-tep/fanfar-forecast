################################################################
#
# Example how to use netcdf_to_obs_utils.R
#   
# David Gustafsson, 2019-10-08
#
################################################################

#### ERRORS/WARNINGS if you try to run this file with Rscript or source it from another file
if(!interactive()){
  print("ERROR: this script includes examples how to use netcdf_to_obs_utils.R INTERACTIVELY in a R session - it shall not be called with Rscript!")
  q(status = 1)
}else{
  print("WARNING: this script includes examples how to use netcdf_to_obs_utils.R INTERACTIVELY in a R session - it shall not be called with Rscript!")
  print("WARNING: if you sourced this file in an R-session, please make sure you have configured it correctly to your system")
  print("WARNING: You need to edit the example >>  look for [EDIT HERE]")
}

#### Preparations [EDIT HERE!!!!] #################################################

#### set working directory
{
  setwd("D:/David.Gustafsson/netcdf_to_obs")
}
  
#### source the utility file (assumed to be located in the working directory)
{
  source("./netcdf_to_obs_utils.R")
}

#### define input/output data
{
  # 1) netcdf input
  {
    # path to folder with netcdf grid files (may also be a vector in case files are saved separately as /../pr and /../../tas )
    grid_dir_path_one     <- "./exampledata/grid.data/onefolder"
    grid_dir_path_sub     <- c("./exampledata/grid.data/subfolders/pr"
                               ,"./exampledata/grid.data/subfolders/tas"
                               ,"./exampledata/grid.data/subfolders/tasmin"
                               ,"./exampledata/grid.data/subfolders/tasmax")
    
    # path to folder with resources (shapefiles with (gfd) grid points and polygons)
    resource_dir_path <- "./exampledata/grid.meta"
    
    # path to netcdf file with elevation and/or just a file with the grid file geometry 
    # (it could actually be one of the netcfd P or T grids if elevation is missing)
    grid_elev_path    <- "./exampledata/grid.meta/CRU_elevation.nc"
    
    # the following 6 variables can be vectors (such as c("pr","tas","tasmin","tasmax")) or size 1:
    #
    #  >> nc_var_name, hype_obs_type, and nc_file_pattern must have the same size
    #  >> obsScale, obsOffset, and obsDigits can be size 1 or same size as the other three
    {
      # netcdf variables (can be pr, tas, tasmin and/or tasmax for GFD data)
      nc_var_name = c("pr","tas","tasmin","tasmax")
      
      # hype obs type (pobs, tobs, etc)
      hype_obs_type = c("Pobs","Tobs","TMINobs","TMAXobs")
      
      # file patterns (for instance "pr_GLB_" or "tas_GLB_")
      nc_file_pattern = c("pr_GLB-0.5","tas_GLB-0.5","tasmin_GLB-0.5","tasmax_GLB-0.5")
      
      # scale and offsets to be applied on the netcdf data (may be )
      obsScale  = c(86400,1,1,1)
      obsOffset = c(0,-273.15,-273.15,-273.15)
      
      # number of digits in output
      obsDigits = c(3,1,1,1)
    }
  }
  
  # 2) model input
  {
    # path to model shapefile (example is Niger-HYPE v2.23)
    shape_file_path   <- "./exampledata/model.data/niger-hype.shp"
  }
  
  # 3) output options
  {
    # output folder
    out_path ="./example_output"
  }
  
  # 4) processing options
  {
    # force re-generation of gridLink (it will always be generated if [out_path]/gridLink.Rdata is missing)
    redoGridLink = F
    
    # force re-generation of grid.point.shp and grid.polygon.shp
    redoGridLayers = F
    
    # prefered projected crs to use for calculating areal weights and distances
    #  - if not given, UTM will be used, with UTM zone adapted for each subbasin
    crsProjProc = NULL

    # Check and clean subbasin polygons for corrupted geometries? 
    #  - preferably this should be done in advance and not as part of operational use
    cleanGeometry = F
    
    # nearest(1) or weighted(0)
    weightedOrNearest=0
    
  }
  
  # 5) time period options (optional)
  {
    # first example 2016 Jan-Jun, where we specify start and end dates
    timeStart.1=as.POSIXct("2016-01-01",tz = "GMT")
    timeEnd.1=as.POSIXct("2016-06-30",tz = "GMT")
    
    # secondly, we append 2016 Jul-Dec (please note how we leave the end date open by setting timeEnd.2=NULL, it will be set by available data in folders)
    timeStart.2=as.POSIXct("2016-07-01",tz = "GMT")
    timeEnd.2=NULL
  }
  
}

#### Actions #######################################################

#### 1. Check/re-generate gridLink
####         
####    - a list array with netcdf gridcell ids and weights 
#### 
####    - used when reading/averaging the netcdf data to model subbasins 
####
####    - generated if requested or if [out_folder]/gridLink.Rdata is missing
####
####    - please note that the model shapefle is not needed if the gridLink.Rdata already exists
{
  
  # some comments for use/improvements:
  #
  #   this function looks for gridLink.Rdata in out_path folder [out_path]/gridLink.Rdata
  #
  #   if missing, it tries to generate the griLink data and save to [out_path]/gridLink.Rdata
  #
  #   if successful it returns 0, otherwise 1
  #
  #   we may want to save the gridLink.Rdata in other place for later re-use
  #
  #   maybe it would be better to separate the expected path to the gridLink.Rdata from the general output path?
  #
  #   IT MAY BE SLOW! This function may take a long time, depending on how many subbasins your model have,
  #   and how large your netcdf grid files are:
  #       - for each subbasin, there is a spatial overlay made to generate list of netcdf grid cells and weights
  #       - it involves identification of potential overlay cells, transformation to projected CRS, intersection
  #         and area calculations.
  #       - BE PATIENT
  #       - there is a loop counter indicating the subbasin row number
  
  isGridLink = gridLinkPreparation(grid.path = grid_dir_path_one[1]
                                   ,grid.pattern = nc_file_pattern[1]
                                   ,grid.elev = grid_elev_path
                                   ,var.name = nc_var_name[1]
                                   ,grid.meta = resource_dir_path
                                   ,output.path = out_path
                                   ,redoGridLink = redoGridLink
                                   ,model.shape = shape_file_path
                                   ,cleanGeometry = cleanGeometry
                                   ,crsProj = crsProjProc)
}

#### 2. Read netcdf data from the grid.data folder and generate (new) PT-obs files and ForcKey.txt using a gridLink.Rdata
{
  # some comments for use/improvements:
  #
  #    - path to the gridLink.Rdata is given in the 4th argument
  #
  #    - file pattern and var.name can be vectors for P, T, TMIN and TMAX
  #
  #    - corresponding vectors with output file names must be specified as well
  #
  #    - overwrite existing obsfiles must be specified (merging/appending is shown in next example)
  #
  #    - the function returns 0 on success and 1 on any type of failure
  #
  #   Please observe, that it is also possible to process only a subset of the subbasins for speedup (good for debugging)
  #   See the commented row at the end of the function call below.
  #
  #   The code is now quite optimized - major time consumer is writing the text-files
  
  readWriteResult = readGridsAndWriteObs(
                           grid.path         = grid_dir_path_one
                          ,grid.pattern      = nc_file_pattern
                          ,var.name          = nc_var_name
                          ,obs.type          = hype_obs_type
                          ,obs.scale         = obsScale
                          ,obs.offset        = obsOffset
                          ,obs.digits        = obsDigits
                          ,gridLink.path     = paste(out_path,"/gridLink.Rdata",sep="")
                          ,output.path       = out_path
                          ,overwrite         = T
                          ,doForcKey         = T
                          ,elev.digits       = 1
                          ,time.start        = timeStart.1
                          ,time.end          = timeEnd.1
                          ,weightedOrNearest = weightedOrNearest)
#                          ,model.subset      = 2:11)
}

#### 3. Read netcdf data from new time period and merge with existing data 
####
####    please note that data is appended to existing files without checking the temporal consistency
####
####    ->> but copies of old files are saved as default with current date and time in filename!
####
####   also note that in this second case, we read data from different subfolders (../pr/, ../tas/ etc...)
{
  readWriteResult = readGridsAndWriteObs(
    grid.path         = grid_dir_path_sub
    ,grid.pattern      = nc_file_pattern
    ,var.name          = nc_var_name
    ,obs.type          = hype_obs_type
    ,obs.scale         = obsScale
    ,obs.offset        = obsOffset
    ,obs.digits        = obsDigits
    ,gridLink.path     = paste(out_path,"/gridLink.Rdata",sep="")
    ,output.path       = out_path
    ,overwrite         = F            # overwrite=F means that we will append to existing data 
    ,doForcKey         = F            # we dont need to overwrite forckey.txt now
    ,elev.digits       = 1
    ,time.start        = timeStart.2  # new time period requested!
    ,time.end          = timeEnd.2    # and here we input NULL to keep end data open 
    ,weightedOrNearest = weightedOrNearest)
#    ,model.subset      = 2:11)
}


#### 4. Diagnostic plot to check the output
{
  # read subbasin shapefile
  subbasin.shp = readOGR(dsn = "./exampledata/model.data",layer="niger-hype",stringsAsFactors = F,GDAL1_integer64_policy = T)
  
  # read obs files
  tobsData = ReadPTQobs("./example_output/Tobs.txt")
  pobsData = ReadPTQobs("./example_output/Pobs.txt")
  
  # plot map with annual precipitation
  plotData = data.frame("subid"=subbasin.shp@data$SUBID,"x"=colSums(pobsData[,2:ncol(pobsData)]))
  PlotMapOutput(x = plotData,map = subbasin.shp,map.subid.column = 1,var.name = "Prec",col.ramp.fun = "ColPrec")
  
  # plot map with annual temperature
  plotData2 = data.frame("subid"=subbasin.shp@data$SUBID,"x"=colMeans(tobsData[,2:ncol(tobsData)]))
  PlotMapOutput(x = plotData2,map = subbasin.shp,map.subid.column = 1,var.name = "Temp",col.ramp.fun = "ColTemp")
}

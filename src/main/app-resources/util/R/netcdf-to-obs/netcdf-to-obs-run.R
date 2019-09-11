# hydrogfd.to.hype.obsfiles_first.run.R
#
# R script
#
# Purpose: to generate HYPE model forcing data based on HydroGFD NetCDF data 
#
# inputs data:
#   - HYPE model subbasin polygon shapefile (with subbasin identifier SUBID in attribute data)
#   - HydroGFD nc-files
#      + daily precipitation        pr_***[yyyymm]***.nc
#      + daily mean temperature     tas_***[yyyymm]***.nc
#      + daily minimum temperature  tasmin_***[yyyymm]***.nc
#      + daily maximum temperature  tasmax_***[yyyymm]***.nc
#   - GFD elevations netcdf file
#
# input arguments/parameters:
#   - see first section in the code
#
# outputs:
#   - HYPE model input:   Pobs.txt, Tobs.txt, TMINobs.txt, TMAXobs.txt, ForcKey.txt
#   - Subbasin-GFD grid cell Links and Weights:  
#     + gfdLinks.Rdata (binary file in Rdata format)
#   - GFD point and polygon layers (gfd.point.shp, gfd.polygon.shp)
#
# dependancy:
#   - hydrogfd.to.hype.obsfiles_utils.R
#   - R packages: sp, rgdal, rgeos, raster, HYPEtools
#
# usage:
#   - see example in this file
#
# [EDIT HERE] indicate where you might edit for your case 
#
# Last updated 2019-01-30, David Gustafsson, SMHI
# ------------------------------------------------------------------------------------------
# INPUT PARAMETERS [EDIT HERE] (@Victor: these could be read as arguments to the function)
# ------------------------------------------------------------------------------------------



library("rciop")


options(show.error.locations = TRUE)

args <- commandArgs(TRUE)

# Setup netcdfs
#setup_netcdfs(args[1], args[2])

rciop.log ("DEBUG", "Starting netcdf-to-obs-run.R", "/util/R/netcdf-to-obs-run.R")

system(paste0("Rscript ", Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf-to-obs/netcdf-to-obs-get-files.R"), intern=T)

q()

system("source activate cairo-env")
{
  # force re-generation of gfdLink (it will anyway be re-generated if gfdLink.Rdata is missing)
  redoGfdLink = F
  
  # force re-generation of gfd.point.shp and gfd.polygon.shp
  redoGfdLayers = F
  
  # select existing use case, or make your own
  useCase = "fanfar_hydrogfdei_to_niger_hype"
  
  # start date/end date for obs-files (tbd)
  
  # append/reload - append to existing obs-files or reload from scratch (tbd)
  
  # diagnostic plots?
  doLinkPlots = F      # one plot for each sub-basin showing the selected GFD griddcells
  doDataPlots = T      # diagnostic plots for the mean result over the loaded data period
  debugPlots  = F      # additional debug plots
  
  # nearest or weighted by areal coverage? (to be done!!)
  #link.method = "weighted" # weighed or nearest
  
}
# -------------------------------------------------------------------------------------------
# Initialisations [EDIT HERE]
# -------------------------------------------------------------------------------------------
{
  # WORKING FOLDER [EDIT HERE]
  {
    setwd("/application/")
  }
  
  # LOAD UTILILTY Functions [EDIT HERE]
  {
    source("./util/R/netcdf-to-obs/netcdf-to-obs-utils.R")  
  }
}
# ------------------------------------------------------------------------------------------
# Use case settings - INPUT and OUTPUT files and folders [EDIT HERE]
# ------------------------------------------------------------------------------------------
{
  # @Victor, we could also make a default useCase, where the input-output file and folder names
  #         follows a standard structure (just like in the corresponding aegir functions)

  # niger-hype obsfiles from hydrogfd files extracted for FANFAR project (ftp.smhi.se)
  if(useCase=="fanfar_hydrogfdei_to_niger_hype"){
    
    # GFD files and folders
    {
      # NB. FANFAR gfd file name structure  [gfd.base.name][YYYYmm][gfd.end.name]
      
      # gfd base names
      gfd.tas.base    = "tas_"
      gfd.pr.base     = "pr_"
      gfd.tasmin.base = "tasmin_"
      gfd.tasmax.base = "tasmax_"
      
      # gfd file name end      
      gfd.file.end    = "_fanfar_SMHI.nc"
      
      # gfd root folders
      gfd.root      = paste(getwd(),"data",sep="/")
      
      # gfd data folder (hydrogfdei in the example)
      gfd.data      = paste(gfd.root,"current_run",sep="/")
      
      # gfd elevation netcdf
      gfd.elev      = paste(gfd.root,"hydrogfd2.0/HydroGFD2elevation.nc",sep="/")
      
      # path to folder with gfd point and polygon layers
      gfd.shapes.dsn = paste(gfd.root,"gfd.resources",sep="/")
      if(!dir.exists(gfd.shapes.dsn)){
        dir.create(gfd.shapes.dsn,recursive = T)
      }
      
    }
    
    # Model subbasin shapefile
#    {
#      model.shape.dsn    = paste(gfd.root,"models/niger-hype",sep="/")
#      model.shape.layer  = "niger-hype"        
#    }
              # Model subbasin shapefile
    {
      model.shape.dsn    = paste(gfd.root,"/models/niger-hype/",sep="/")
      #model.shape.layer  = "Current_Qstn_onedelta_2014-01-27"
      model.shape.layer  = "niger-hype"
    }

    
    # OUTPUT folder
    {
      output.folder = paste(getwd(),"output",useCase,sep="/")
      if(!dir.exists(output.folder)){
        dir.create(output.folder,recursive = T)
      }
    }
  }
}
# -------------------------------------------------------------------------------------------
# Execution [DONT EDIT HERE]
# -------------------------------------------------------------------------------------------
{
  # 1. Read or Generate gfdLink (lists of gfd gridcells and weights for each model subbasin)
  {
    gfdLink.file = paste(output.folder,"gfdLink.Rdata",sep="/")
    if(!file.exists(gfdLink.file)|redoGfdLink){
      
      # a. Read HYPE model subbasin shapefile
      {
        subbasin.shp = readOGR(dsn = model.shape.dsn,layer = model.shape.layer)
      }
      
      # b. Generate and/or Read GFD point and polygon layers
      {
        if(!file.exists(paste(gfd.shapes.dsn,"gfd.point.shp",sep="/"))|
           !file.exists(paste(gfd.shapes.dsn,"gfd.polygon.shp",sep="/"))){
          
          if(file.exists(gfd.elev)){
            # generate gfd shapefiles
            makeGFDSpatialLayers(CRU_elevation.nc = gfd.elev
                                 ,dsn = gfd.shapes.dsn
                                 ,layer.point = "gfd.point"
                                 ,layer.poly = "gfd.polygon")
            # read files
            gfd.point = readOGR(dsn = gfd.shapes.dsn,layer = "gfd.point")
            gfd.poly  = readOGR(dsn = gfd.shapes.dsn,layer = "gfd.polygon")
          }else{
            print("WARNING gfd.point.shp and/or gfd.polygon.shp is missing AND gfd.elev is also missing!")
            print("NOTHING TO DO....")
          }
        }else{
          # read files
          gfd.point = readOGR(dsn = gfd.shapes.dsn,layer = "gfd.point")
          gfd.poly  = readOGR(dsn = gfd.shapes.dsn,layer = "gfd.polygon")
        }
      }
      
      # c. List GFD files in download folder
      {
        tasFiles    = dir(path = gfd.data,pattern = gfd.tas.base)
        tasminFiles = dir(path = gfd.data,pattern = gfd.tasmin.base)
        tasmaxFiles = dir(path = gfd.data,pattern = gfd.tasmax.base)
        prFiles     = dir(path = gfd.data,pattern = gfd.pr.base)
      }
      
      # d. Read one NC-file to get spatial coverage of the GFD domain (may be smaller than the global files)
      {
        # set gfd.point = NULL and gfd.poly = NULL to generate these datasets when reading the file
        tasFirst = readGFDGCMfiles(gfd.files = paste(gfd.data,tasFiles[1],sep="/")
                                   ,gfd.point = gfd.point
                                   ,gfd.poly = gfd.poly
                                   ,dataOffset = -273.15
                                   ,dataScale = 1,
                                   ncvarname="tas")
      }
      
      # e. generate new gfdLink list array
      {
        gfdLink = gfdCellsOverAOI(aoi = subbasin.shp,gfd.poly = tasFirst$firstObs$nc.poly,doPlot = doLinkPlots)
      }
      
      # f. save to file
      {
        save(list = "gfdLink",file = gfdLink.file)
      }
      
      # [DEBUG] plot model and gfd polygons for checkning
      if(debugPlots){
        
        # plot the extracted gfd polygon with elevations for checking
        PlotMapOutput(x = data.frame("SUBID"=tasFirst$firstObs$nc.poly@data$id.gfd,"ELEV"=tasFirst$firstObs$nc.poly@data$elev.gfd)
                      ,map = tasFirst$firstObs$nc.poly
                      ,map.subid.column = 5,plot.legend = F,plot.scale = F,map.adj = 0.5)
        
        # gfd grid borders
        plot(tasFirst$firstObs$nc.poly,add=T,border="grey")
        
        # model subbasin
        plot(subbasin.shp,add=T,border="black")
        
      }
    }else{
      # load existing gfdLink file
      load(gfdLink.file)
    }
    
  }
  
  # 2. Read NC-files, all variables, request weighted outputs
  {
    # @Victor: here we can introduce some further fine-tuning:
    #
    #  select gfd-files to cover requested time span only
    #  select gfd-files to cover time-span after end of existing obs files (append)
    #  select variables
    #  select weighted or nearest link method
    #  etc...
    
    # a. List GFD files in download folder
    {
      tasFiles    = dir(path = gfd.data,pattern = gfd.tas.base)
      tasminFiles = dir(path = gfd.data,pattern = gfd.tasmin.base)
      tasmaxFiles = dir(path = gfd.data,pattern = gfd.tasmax.base)
      prFiles     = dir(path = gfd.data,pattern = gfd.pr.base)
    }
    
    # b. Read GFD data
    {
      # please note the following:
      # ----------------------------------------------------------------------------
      # >> The obsid is generated as SUBID + 1E6
      #
      # When we do weighted data, we get one obs-data column per model sub-basin.
      # However, the elevation represented by the weighted GFD data is still not the same as the
      # sub-basin elevations. Thus, we still need to use the ForcKey.txt to be able to do
      # elevation corrections in the HYPE model. And to avoid mistakes, it is better to 
      # introduce an independent OBSID for the obsdata files, rather than to use the SUBID.
      #
      # The OBSID offset (1E6) must be chosen so that the OBSID becomes unique 
      #   -> please check maximum SUBID in the model
      #
      # >> Set dataScale and dataOffset for Temperature and Precipitation!!!
      #
      #    Temperature     dataOffset = -273.15      dataScale = 1
      #  
      #    Precipitation   dataOffset = 0           dataScale = 86400
      # ----------------------------------------------------------------------------
      
      # read shapefile if missing
      if(!exists("subbasin.shp")){
        subbasin.shp = readOGR(dsn = model.shape.dsn,layer = model.shape.layer)
      }

      t=Sys.time()
      # read the data
      {
        # daily mean temperature
        tasData = readGFDGCMfiles(gfd.files = paste(gfd.data,tasFiles,sep="/")
                                  ,gfdLink = gfdLink
                                  ,dataOffset = -273.15
                                  ,dataScale = 1
                                  ,obsid = subbasin.shp$SUBID + 1E6)
        # daily min temperature
        tasminData = readGFDGCMfiles(gfd.files = paste(gfd.data,tasminFiles,sep="/")
                                     ,gfdLink = gfdLink
                                     ,dataOffset = -273.15
                                     ,dataScale = 1
                                     ,obsid = subbasin.shp$SUBID + 1E6)
        # daily max temperature
        tasmaxData = readGFDGCMfiles(gfd.files = paste(gfd.data,tasmaxFiles,sep="/")
                                     ,gfdLink = gfdLink
                                     ,dataOffset = -273.15
                                     ,dataScale = 1
                                     ,obsid = subbasin.shp$SUBID + 1E6)
        # daily precipitation
        prData = readGFDGCMfiles(gfd.files = paste(gfd.data,prFiles,sep="/")
                                 ,gfdLink = gfdLink
                                 ,dataOffset = 0
                                 ,dataScale = 86400
                                 ,obsid = subbasin.shp$SUBID + 1E6)
      }
      Sys.time()-t
    }
    
  }
  
  # 3. Write outputs:
  #   
  #   >> Pobs.txt, Tobs.txt, TMINobs.txt, TMAXobs.txt
  #   >> ForcKey.txt
  #   >> diagnostoc plots
  {
    # determine which columns is the subbasin ID number
    map.subid.column = which(tolower(colnames(subbasin.shp@data))=="subid")
    
    # write output
    obsdata2hype(pobs = prData$mergedObs
                 ,tobs = tasData$mergedObs
                 ,tminobs = tasminData$mergedObs
                 ,tmaxobs = tasmaxData$mergedObs
                 ,outputfolder = output.folder
                 ,gfdLink = gfdLink
                 ,subid = subbasin.shp@data[,map.subid.column]
                 ,sub.poly = subbasin.shp
                 ,map.subid.column = map.subid.column
                 ,doPlot = doDataPlots)
  }
}
  

system("source deactivate cairo-env")

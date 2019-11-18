#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

# Constants
nameOfSrcFile_PC <- "/util/R/process-configuration.R"

verbose <- TRUE
#verbose <- FALSE

# Constants application.xml (tag option)
cModelConfigVariant1 <- "WestAfrica-HYPE + HydroGFD 2.0 + ECOPER"
cModelConfigVariants <- c(cModelConfigVariant1)

# Global constants not part of application.xml that
# may be used for comparsion in if-statements etc.
cHydModelVariant1 <- "Niger-HYPE"
cHydModelVariant2 <- "WestAfrica-HYPE"
cHydModelVariants <- c(cHydModelVariant1,cHydModelVariant2)

cMetHCVariant1 <- "GFD 1.3"
cMetHCVariant2 <- "HydroGFD 2.0"
cMetHCVariants <- c(cMetHCVariant1,cMetHCVariant2)

cMetFCVariant1 <- "ECOPER"
cMetFCVariants <- c(cMetFCVariant1)

cRunTypeVariant1 <- "Operational"
cRunTypeVariant2 <- "Reforecast"
cRunTypeVariants <- c(cRunTypeVariant1,cRunTypeVariant2)


# Process user options/selections to later select between different variants
# of models, datasets etc.
process_configuration_application_runtime_options <- function()
{
    # Outputs
    modelConfig <- NULL

    # hydModel <- NULL
    # metHC    <- NULL
    # metFC    <- NULL
    runType  <- NULL

    # hydModelIn <- rciop.getparam("hydmodel")
    # if (hydModelIn == cHydModelVariant1){ hydModel <- cHydModelVariant1 }
    # if (hydModelIn == cHydModelVariant2){ hydModel <- cHydModelVariant2 }

    # metHCIn <- rciop.getparam("methc")
    # if (metHCIn == cMetHCVariant1){ metHC <- cMetHCVariant1 }
    # if (metHCIn == cMetHCVariant2){ metHC <- cMetHCVariant2 }

    # metFCIn <- rciop.getparam("metfc")
    # if (metFCIn == cMetFCVariant1){ metFC <- cMetFCVariant1 }

    modelConfigIn <- rciop.getparam("model_config")
    if (modelConfigIn == cModelConfigVariant1) {
        modelConfig <- cModelConfigVariant1
        
        # ToDo: Necessary any longer
        # hydModel <- cHydModelVariant2
        # metHC    <- cMetHCVariant2
        # metFC    <- cMetFCVariant1
    }

    runTypeIn <- rciop.getparam("runtype")
    if (runTypeIn == cRunTypeVariant1){ runType <- cRunTypeVariant1 }
    if (runTypeIn == cRunTypeVariant2){ runType <- cRunTypeVariant2 }

    # ToDo: Remove when date parsed from HYPE state filename etc.
    #hypeStateDate <- rciop.getparam("hypeStateDate")

    rciop.log("INFO", "-------Global configuration options:-------", nameOfSrcFile_PC)
    rciop.log("INFO", paste0("modelConfig:   ",modelConfig), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("hydModel:      ",hydModel), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("metHC:         ",metHC), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("metFC:         ",metFC), nameOfSrcFile_PC)
    rciop.log("INFO", paste0("runType:       ",runType), nameOfSrcFile_PC)
    #rciop.log("INFO", paste0("hypeStateDate: ", hypeStateDate), nameOfSrcFile_PC)
    rciop.log("INFO", "-------------------------------------------", nameOfSrcFile_PC)

    outputApplRuntimeOptions <- list("modelConfig"=modelConfig,
                                    #  "hydModel"=hydModel,
                                    #  "metHC"=metHC,
                                    #  "metFC"=metFC,
                                     "runType"=runType
                                     #"hypeStateDate"=hypeStateDate
                                    )

} # process_configuration_application_runtime_options


# Extract information from file dependencies.txt, part of the application configuration object
# Output is now urls to configuration items to later be downloaded, e.g.:
# modelData   - locating HYPE model dataset. Data files etc.
# gfdHydrogfd - locating netcdf files for HydroGFD2
process_configuration_application_inputs <- function(applConfig=NULL, # Application configuration from user, ToDo
                                                     applInput=NULL)  # Input to application from stdin
{
  # Download the configuration object from the applications input reference url
  # For non-valid reference urls, use the defined urls in this function
  # Read file dependencies.txt, part of the downloaded configuration object
  # Return all data in a list
  
  # Outputs
  modelDataSubDir  <- NULL
  modelDataUrl     <- NULL
  modelDataQuery   <- NULL
  modelDataComment <- NULL
  
  # modelDataOldSubDir  <- NULL
  # modelDataOldUrl     <- NULL
  # modelDataOldQuery   <- NULL
  # modelDataOldComment <- NULL
  
  gfdHydrogfdeiSubDir  <- NULL
  gfdHydrogfdeiUrl     <- NULL
  gfdHydrogfdeiQuery   <- NULL
  gfdHydrogfdeiComment <- NULL
  
  gfdHydrogfdodSubDir  <- NULL
  gfdHydrogfdodUrl     <- NULL
  gfdHydrogfdodQuery   <- NULL
  gfdHydrogfdodComment <- NULL
  
  gfdOdDailySubDir  <- NULL
  gfdOdDailyUrl     <- NULL
  gfdOdDailyQuery   <- NULL
  gfdOdDailyComment <- NULL
  
  gfdEcoperDailySubDir  <- NULL
  gfdEcoperDailyUrl     <- NULL
  gfdEcoperDailyQuery   <- NULL
  gfdEcoperDailyComment <- NULL
  
  gfdElevationSubDir  <- NULL
  gfdElevationUrl     <- NULL
  gfdElevationQuery   <- NULL
  gfdElevationComment <- NULL
  
  # Query the input reference
  
  # Check if the input reference url is valid, else use the defaults below
  # It cannot be completely empty due to condition of the main while loop in run.R.
  # Paste this in a web browser to check for alternative zip-files: https://recast.terradue.com/t2api/search/hydro-smhi/models?uid
  urlLen <- 103
  if (is.null(applInput) || nchar(applInput) < urlLen){
      # User
      # if(applConfig$hydModel == cHydModelVariant1){
      #     print("Using default URL for Niger-HYPE model configuration object")
      #     urlAndQuery <- 'https://recast.terradue.com/t2api/search/hydro-smhi/models?uid=9345ED73B72F49E6FF31B07B57013BC519210E24'
      #     opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
      # }
      #else if(applConfig$hydModel == cHydModelVariant2){
      if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
          print("Using default URL for WestAfrica-HYPE model configuration object")
          urlAndQuery <- 'https://recast.terradue.com/t2api/search/hydro-smhi/models?uid=40A27455C9498A70A4C12E458E527499331B96AE'
          opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
      }else{
          print("UNSUPPORTED URL for HYPE model configuration object")
          # Exit the application
          rciop.log("ERROR", "Unsupported configuration",nameOfSrcFile_PC)
          q(save="no",status=99)
      }
  }else {
      # Using URL and query from stdin (i.e. data in the field 'HYPE model configuration')
      opensearchCmd=paste0("opensearch-client '",applInput,"' enclosure")
  }
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)


  # Download the dir(s)
  model_config_dir <- rciop.copy(input_enclosure, TMPDIR, uncompress=TRUE) # tmpDir
  if (model_config_dir$exit.code==0) {
    local.model_config_dir <- model_config_dir$output # Returns path to local dir or file, dir in this case
  }else {
    rciop.log("ERROR", "Could not access the model configuration file.",nameOfSrcFile_PC)
    q(save="no",status=99)
  }
  
  # Filenames
  model_config_file <- "dependencies.txt"
  
  path_to_file <- paste(local.model_config_dir, model_config_file, sep="/")
  
  # Read contents of config file, handling separators ';' into a df
  model_config_data <- read.csv2(path_to_file, header=FALSE, sep=";")
  names(model_config_data) <- c('subdir','url','querypattern','info')
  for (r in 1:nrow(model_config_data)) {
    subdir <- model_config_data[r,'subdir']
    if (subdir == 'model-data') {
      modelDataSubDir  <- model_config_data[r,'subdir'] # Intended to be used as dir name for rciop.copy TMPDIR/subdir/
      modelDataUrl     <- model_config_data[r,'url']
      modelDataQuery   <- model_config_data[r,'querypattern']
      modelDataComment <- model_config_data[r,'info']
    }
    # if (subdir == 'model-data-old-url') {
    #   modelDataOldSubDir  <- model_config_data[r,'subdir'] # Not used
    #   modelDataOldUrl     <- model_config_data[r,'url']
    #   modelDataOldQuery   <- model_config_data[r,'querypattern'] # Not used
    #   modelDataOldComment <- model_config_data[r,'info']
    # }
    if (subdir == 'hydrogfdei') {
      gfdHydrogfdeiSubDir  <- model_config_data[r,'subdir'] # Intended to be used both for dir name (rciop.copy) and part of filename
      gfdHydrogfdeiUrl     <- model_config_data[r,'url']
      gfdHydrogfdeiQuery   <- model_config_data[r,'querypattern']
      gfdHydrogfdeiComment <- model_config_data[r,'info']
    }
    if (subdir == 'hydrogfdod') {
      gfdHydrogfdodSubDir  <- model_config_data[r,'subdir']
      gfdHydrogfdodUrl     <- model_config_data[r,'url']
      gfdHydrogfdodQuery   <- model_config_data[r,'querypattern']
      gfdHydrogfdodComment <- model_config_data[r,'info']
    }
    if (subdir == 'od-daily') {
      gfdOdDailySubDir  <- model_config_data[r,'subdir']
      gfdOdDailyUrl     <- model_config_data[r,'url']
      gfdOdDailyQuery   <- model_config_data[r,'querypattern']
      gfdOdDailyComment <- model_config_data[r,'info']
    }
    if (subdir == 'ecoper') {
      gfdEcoperSubDir  <- model_config_data[r,'subdir']
      gfdEcoperUrl     <- model_config_data[r,'url']
      gfdEcoperQuery   <- model_config_data[r,'querypattern']
      gfdEcoperComment <- model_config_data[r,'info']
    }
    # Skip subdir 'ei-monthly' and 'od-monthly'
    if (subdir == 'elevation') {
      gfdElevationSubDir  <- model_config_data[r,'subdir']
      gfdElevationUrl     <- model_config_data[r,'url']
      gfdElevationQuery   <- model_config_data[r,'querypattern']
      gfdElevationComment <- model_config_data[r,'info']
    }
  }
  
  # Return information as a list
  outputModelConfig <- list("modelConfigObjectDir"=local.model_config_dir,
    
                            "modelDataSubDir"=modelDataSubDir,
                            "modelDataUrl"=modelDataUrl,
                            "modelDataQuery"=modelDataQuery,
                            "modelDataComment"=modelDataComment,
                            
                            # "modelDataOldSubDir"=modelDataOldSubDir,
                            # "modelDataOldUrl"=modelDataOldUrl,
                            # "modelDataOldQuery"=modelDataOldQuery,
                            # "modelDataOldComment"=modelDataOldComment,
                            
                            "gfdHydrogfdeiSubDir"=gfdHydrogfdeiSubDir,
                            "gfdHydrogfdeiUrl"=gfdHydrogfdeiUrl,
                            "gfdHydrogfdeiQuery"=gfdHydrogfdeiQuery,
                            "gfdHydrogfdeiComment"=gfdHydrogfdeiComment,
                            
                            "gfdHydrogfdodSubDir"=gfdHydrogfdodSubDir,
                            "gfdHydrogfdodUrl"=gfdHydrogfdodUrl,
                            "gfdHydrogfdodQuery"=gfdHydrogfdodQuery,
                            "gfdHydrogfdodComment"=gfdHydrogfdodComment,
                            
                            "gfdOdDailySubDir"=gfdOdDailySubDir,
                            "gfdOdDailyUrl"=gfdOdDailyUrl,
                            "gfdOdDailyQuery"=gfdOdDailyQuery,
                            "gfdOdDailyComment"=gfdOdDailyComment,
                            
                            "gfdEcoperSubDir"=gfdEcoperSubDir,
                            "gfdEcoperUrl"=gfdEcoperUrl,
                            "gfdEcoperQuery"=gfdEcoperQuery,
                            "gfdEcoperComment"=gfdEcoperComment,
                            
                            "gfdElevationSubDir"=gfdElevationSubDir,
                            "gfdElevationUrl"=gfdElevationUrl,
                            "gfdElevationQuery"=gfdElevationQuery,
                            "gfdElevationComment"=gfdElevationComment
  )
  
  return (outputModelConfig)
} # process_configuration_application_inputs


check_dir_exist <- function(absPath)
{
    if(is.null(absPath) || !dir.exists(absPath)) {
        rciop.log("INFO",paste0("Dir missing: ",absPath),nameOfSrcFile_PC)
    }
}


check_file_exist <- function(absPath)
{
    if(is.null(absPath) || !file.exists(absPath)) {
        rciop.log("INFO",paste0("File missing: ",absPath),nameOfSrcFile_PC)
    }
}


# Download HYPE dataset from the url - part of the model configuration object
# Output is paths to downloaded data
process_configuration_hype_data <- function(applConfig,  # Application configuration from user, ToDo
                                            modelConfig) # Application model config object (urls)
{
  # Outputs
  dirGridMetaData   <- NULL
  dirModelFiles     <- NULL
  dirForcingArchive <- NULL
  dirReturnLevels   <- NULL
  dirShapeFiles     <- NULL
  dirStateFiles     <- NULL
  dirHYPE2CSVFiles  <- NULL

  fileInfoTxtColdStart <- NULL
  fileInfoTxtHindcast  <- NULL
  fileInfoTxtForecast  <- NULL
  
  url <- modelConfig$modelDataUrl
  query <- modelConfig$modelDataQuery
  
  # Query the input reference
  opensearchCmd=paste0("opensearch-client '", url, query, "' enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)
  
  # Download HYPE dataset model specific data dir(s)
  modelDataDirs <- rciop.copy(input_enclosure, TMPDIR, uncompress=TRUE)
  if (modelDataDirs$exit.code==0) {
    local.modelDataDirs <- modelDataDirs$output
  }
  else {
    rciop.log("ERROR", "Could not access the model data dirs.","")
    q(save="no",status=98)
  }
  
  # Support different HYPE model data versions via input/configuration
  # if(applConfig$hydModel == cHydModelVariant1){
  #     dirModelFiles <- paste(local.modelDataDirs,"v2.23",sep="/")
      
  #     # Dirs (absolute paths)
  #     #dirGridMetaData   <-
  #     dirForcingArchive <- paste(dirModelFiles,"forcingarchive",sep="/")
  #     dirReturnLevels   <- paste(dirModelFiles,"returnlevels",sep="/")
  #     dirShapeFiles     <- paste(dirModelFiles,"shapefiles",sep="/")
  #     dirStateFiles     <- paste(dirModelFiles,"statefiles",sep="/")
      
  #     # Individual files - these info files should maybe be fetched from the model/application config object? (parallel files to dependencies.txt in zip-file)
  #     fileInfoTxtColdStart <- paste(dirModelFiles,"info-coldstart-19791994.txt",sep="/")
  #     fileInfoTxtHindcast  <- paste(dirModelFiles,"info-hindcast.txt",sep="/")
  #     fileInfoTxtForecast  <- paste(dirModelFiles,"info-forecast.txt",sep="/")
  # }
  #if(applConfig$hydModel == cHydModelVariant2){
  if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
      dirModelFiles <- paste(local.modelDataDirs,"v1.3.6",sep="/")
      
      # Dirs (absolute paths)
      dirGridMetaData   <- paste(dirModelFiles,"grid.meta",sep="/")
      dirForcingArchive <- paste(dirModelFiles,"forcingarchive",sep="/")
      dirReturnLevels   <- paste(dirModelFiles,"returnlevels",sep="/")
      dirShapeFiles     <- paste(dirModelFiles,"subidshapefiles",sep="/") # ToDo: Temporary disabled during tests with shapefiles for niger-hype
      #dirShapeFiles     <- paste(dirModelFiles,"shapefiles",sep="/")
      dirStateFiles     <- paste(dirModelFiles,"statefiles",sep="/")
      dirHYPE2CSVFiles  <- paste(dirModelFiles,"hype2csv",sep="/")
      
      # Individual files - these info files should maybe be fetched from the model/application config object? (parallel files to dependencies.txt in zip-file)
      #fileInfoTxtColdStart <- paste(dirModelFiles,"info-coldstart-19791994.txt",sep="/")
      #fileInfoTxtHindcast  <- paste(dirModelFiles,"info-hindcast.txt",sep="/") # With this path, already done in updateModelInput() as standard
      #fileInfoTxtForecast  <- paste(dirModelFiles,"info-forecast.txt",sep="/")

      # Currently disabled in hypeapps-utils.R, not used
      #print("INFO Using info-coldstart-19791994.txt, info-hindcast.txt and info-forcast.txt from the configuration object, westafrica-hype-model-1.3.6.zip")
      fileInfoTxtColdStart <- paste(modelConfig$modelConfigObjectDir,"info-coldstart-19791994.txt",sep="/")
      fileInfoTxtHindcast  <- paste(modelConfig$modelConfigObjectDir,"info-hindcast.txt",sep="/")
      fileInfoTxtForecast  <- paste(modelConfig$modelConfigObjectDir,"info-forecast.txt",sep="/")
  }

  # Check that dirs/files do exist
  check_dir_exist(dirGridMetaData)
  check_dir_exist(dirModelFiles)
  check_dir_exist(dirForcingArchive)
  check_dir_exist(dirReturnLevels)
  check_dir_exist(dirShapeFiles)
  check_dir_exist(dirStateFiles)
  check_dir_exist(dirHYPE2CSVFiles)

  check_file_exist(fileInfoTxtColdStart)
  check_file_exist(fileInfoTxtHindcast)
  check_file_exist(fileInfoTxtForecast)

  # Return information as a list
  outputModelData <- list("dirModelFiles"=dirModelFiles,
                          "dirGridMetaData"=dirGridMetaData,
                          "dirForcingArchive"=dirForcingArchive,
                          "dirReturnLevels"=dirReturnLevels,
                          "dirShapeFiles"=dirShapeFiles,
                          "dirStateFiles"=dirStateFiles,
                          "dirHYPE2CSVFiles"=dirHYPE2CSVFiles,
                          "fileInfoTxtColdStart"=fileInfoTxtColdStart,
                          "fileInfoTxtHindcast"=fileInfoTxtHindcast,
                          "fileInfoTxtForecast"=fileInfoTxtForecast
  )
  
  #print(outputModelData)
  #rciop.log("INFO", outputModelData)
  return (outputModelData)
} # process_configuration_hype_data

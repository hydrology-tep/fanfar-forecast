#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

# Constants
nameOfSrcFile_PC <- "/util/R/process-configuration.R"

#verbose <- TRUE
verbose <- FALSE

source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/constants.R",sep="/"))

# Process user options/selections to later select between different variants
# of models, datasets etc.
process_configuration_application_runtime_options <- function()
{
    # Outputs
    modelConfig <- NULL

    # hydModel <- NULL
    metHC    <- NULL
    # metFC    <- NULL
    runType  <- NULL
    runTypeStateFileCreation <- NULL

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
        metHC    <- cMetHCVariant2
        # metFC    <- cMetFCVariant1
    }else{
        metHC    <- cMetHCVariant1
    }

    runTypeIn <- rciop.getparam("runtype")
    if (runTypeIn == cRunTypeVariantOperational){
      runType                  <- cRunTypeVariantOperational
      runTypeStateFileCreation <- cRunTypeVariantOperational # False
    }
    if (runTypeIn == cRunTypeVariantReforecast){
      runType                  <- cRunTypeVariantReforecast
      runTypeStateFileCreation <- cRunTypeVariantReforecast
    }
    if (runTypeIn == cRunTypeVariantStatefile){
      runType                  <- cRunTypeVariantReforecast
      runTypeStateFileCreation <- cRunTypeVariantStatefile # True
    }

    rciop.log("INFO", "-------Global configuration options:-------", nameOfSrcFile_PC)
    rciop.log("INFO", paste0("modelConfig:   ",modelConfig), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("hydModel:      ",hydModel), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("metHC:         ",metHC), nameOfSrcFile_PC)
    # rciop.log("INFO", paste0("metFC:         ",metFC), nameOfSrcFile_PC)
    rciop.log("INFO", paste0("runType:       ",runType), nameOfSrcFile_PC)
    if (runTypeStateFileCreation == cRunTypeVariantStatefile){
      rciop.log("INFO", "runTypeStateFileCreation: TRUE", nameOfSrcFile_PC)
    }
    rciop.log("INFO", paste0("idate:         ",rciop.getparam("idate")), nameOfSrcFile_PC)
    rciop.log("INFO", paste0("hcperiodlen:   ",rciop.getparam("hcperiodlen")), nameOfSrcFile_PC)
    rciop.log("INFO", "-------------------------------------------", nameOfSrcFile_PC)

    outputApplRuntimeOptions <- list("modelConfig"=modelConfig,
                                    #  "hydModel"=hydModel,
                                    "metHC"=metHC,
                                    #  "metFC"=metFC,
                                     "runType"=runType,
                                     "runTypeStateFileCreation"=runTypeStateFileCreation
                                    )

} # process_configuration_application_runtime_options


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


# Search for the HYPE model configuration and download the model data
search_download_model_configuration <- function(url,
                                                query)
{
    # Outputs
    modelConfigPath <- NULL

    if (is.null(url) || is.null(query)){
        print("ERROR, HYPE model configuration items missing")
        q(save="no",status=1)
    }

    # Query the model config reference
    opensearchCmd=paste0("opensearch-client '",url,query,"' enclosure")
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    rciop.log("INFO", res_enclosure)

    # Download
    tmp_dir=paste0(TMPDIR,"/hype-model/config")
    res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

    if (res_copy$exit.code==0) {
        modelConfigPath <- res_copy$output
    }else{
        print("ERROR, could not find or download the configuration")
        q(save="no",status=1)
    }
    
    return (modelConfigPath)
} # search_download_model_configuration


# Search for the HydroGFD meteo configuration and download the configuration object
search_download_meteo_configuration <- function(url,
                                                query)
{
    # Outputs
    gfdHydrogfdeiSubDir  <- NULL
    gfdHydrogfdeiUrl     <- NULL
    gfdHydrogfdeiQuery   <- NULL
    #gfdHydrogfdeiComment <- NULL
    
    gfdHydrogfdodSubDir  <- NULL
    gfdHydrogfdodUrl     <- NULL
    gfdHydrogfdodQuery   <- NULL
    #gfdHydrogfdodComment <- NULL
    
    gfdOdDailySubDir  <- NULL
    gfdOdDailyUrl     <- NULL
    gfdOdDailyQuery   <- NULL
    #gfdOdDailyComment <- NULL
    
    gfdEcoperDailySubDir  <- NULL
    gfdEcoperDailyUrl     <- NULL
    gfdEcoperDailyQuery   <- NULL
    #gfdEcoperDailyComment <- NULL
    
    gfdElevationSubDir  <- NULL
    gfdElevationUrl     <- NULL
    gfdElevationQuery   <- NULL
    #gfdElevationComment <- NULL
    
    gfdGridSubDir  <- NULL
    gfdGridUrl     <- NULL
    gfdGridQuery   <- NULL
    #gfdGridComment <- NULL

    if (is.null(url) || is.null(query)){
        print("ERROR, HydroGFD meteo configuration items missing")
        q(save="no",status=1)
    }

    # Query the meteo config reference
    opensearchCmd=paste0("opensearch-client '",url,query,"' enclosure")
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    rciop.log("INFO", res_enclosure)

    # Download
    tmp_dir=paste0(TMPDIR,"/hydrogfd/config")
    res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

    local.meteoConfig <- NULL
    if (res_copy$exit.code==0) {
        local.meteoConfig <- res_copy$output
    }else{
        print("ERROR, could not find or download the configuration")
        q(save="no",status=1)
    }

    # Read the hydrogfd meteo configuration file
    meteo_config_file <- paste0(local.meteoConfig,"/dependencies.txt")

    if (! file.exists(meteo_config_file)) {
        print("ERROR, configuration file missing")
        q(save="no",status=1)
    }

    meteo_config_data <- read.csv2(meteo_config_file, header=TRUE, sep=";")

    for (r in 1:nrow(meteo_config_data)) {
        subdir <- meteo_config_data[r,'localdirectory']
        if (subdir == 'hydrogfdei') {
          gfdHydrogfdeiSubDir  <- meteo_config_data[r,'localdirectory'] # Intended to be used both for dir name (rciop.copy) and part of filename
          gfdHydrogfdeiUrl     <- meteo_config_data[r,'url']
          gfdHydrogfdeiQuery   <- meteo_config_data[r,'searchquery']
          #gfdHydrogfdeiComment <- meteo_config_data[r,'comment']
        }
        if (subdir == 'hydrogfdod') {
          gfdHydrogfdodSubDir  <- meteo_config_data[r,'localdirectory']
          gfdHydrogfdodUrl     <- meteo_config_data[r,'url']
          gfdHydrogfdodQuery   <- meteo_config_data[r,'searchquery']
          #gfdHydrogfdodComment <- meteo_config_data[r,'comment']
        }
        if (subdir == 'od-daily') {
          gfdOdDailySubDir  <- meteo_config_data[r,'localdirectory']
          gfdOdDailyUrl     <- meteo_config_data[r,'url']
          gfdOdDailyQuery   <- meteo_config_data[r,'searchquery']
          #gfdOdDailyComment <- meteo_config_data[r,'comment']
        }
        if (subdir == 'ecoper') {
          gfdEcoperSubDir  <- meteo_config_data[r,'localdirectory']
          gfdEcoperUrl     <- meteo_config_data[r,'url']
          gfdEcoperQuery   <- meteo_config_data[r,'searchquery']
          #gfdEcoperComment <- meteo_config_data[r,'comment']
        }
        # Skip subdir 'ei-monthly' and 'od-monthly'
        if (subdir == 'elevation') {
          gfdElevationSubDir  <- meteo_config_data[r,'localdirectory']
          gfdElevationUrl     <- meteo_config_data[r,'url']
          gfdElevationQuery   <- meteo_config_data[r,'searchquery']
          #gfdElevationComment <- meteo_config_data[r,'comment']
        }
        if (subdir == 'grid-meta') {
          gfdGridSubDir  <- meteo_config_data[r,'localdirectory']
          gfdGridUrl     <- meteo_config_data[r,'url']
          gfdGridQuery   <- meteo_config_data[r,'searchquery']
          #gfdGridComment <- meteo_config_data[r,'comment']
        }
    }

    output <- list("gfdHydrogfdeiSubDir"=gfdHydrogfdeiSubDir,
                   "gfdHydrogfdeiUrl"=gfdHydrogfdeiUrl,
                   "gfdHydrogfdeiQuery"=gfdHydrogfdeiQuery,
                   #"gfdHydrogfdeiComment"=gfdHydrogfdeiComment,
                            
                   "gfdHydrogfdodSubDir"=gfdHydrogfdodSubDir,
                   "gfdHydrogfdodUrl"=gfdHydrogfdodUrl,
                   "gfdHydrogfdodQuery"=gfdHydrogfdodQuery,
                   #"gfdHydrogfdodComment"=gfdHydrogfdodComment,
                            
                   "gfdOdDailySubDir"=gfdOdDailySubDir,
                   "gfdOdDailyUrl"=gfdOdDailyUrl,
                   "gfdOdDailyQuery"=gfdOdDailyQuery,
                   #"gfdOdDailyComment"=gfdOdDailyComment,
                            
                   "gfdEcoperSubDir"=gfdEcoperSubDir,
                   "gfdEcoperUrl"=gfdEcoperUrl,
                   "gfdEcoperQuery"=gfdEcoperQuery,
                   #"gfdEcoperComment"=gfdEcoperComment,
                            
                   "gfdElevationSubDir"=gfdElevationSubDir,
                   "gfdElevationUrl"=gfdElevationUrl,
                   "gfdElevationQuery"=gfdElevationQuery,
                   #"gfdElevationComment"=gfdElevationComment,
                            
                   "gfdGridSubDir"=gfdGridSubDir,
                   "gfdGridUrl"=gfdGridUrl,
                   "gfdGridQuery"=gfdGridQuery #,
                   #"gfdGridComment"=gfdGridComment
                  )

    return (output)
} # search_download_meteo_configuration



process_configuration_application_inputs <- function(applInput=NULL,          # Input to application from stdin
                                                     applRuntimeOptions=NULL) # Application configuration from user, ToDo
{
  
  # Outputs
  modelConfigPath   <- NULL
  meteoConfigSearch <- NULL
 
  urlDefault <- 'https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast?uid=C67D3BFEC0ACEDAEFAD0D1BD5464FBE4BD3C6FCB'
  urlLen     <- nchar(urlDefault)

  # Check if the input reference url is valid, else use the defaults below
  # It cannot be completely empty due to condition of the main while loop in run.R.
  if (is.null(applInput) || nchar(applInput) < urlLen){
      # User
      if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
          rciop.log("INFO",paste0("Using default URL for the main configuration object: ",cModelConfigVariant1),nameOfSrcFile_PC)
          urlAndQuery <- urlDefault
          opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
      }else{
          # Exit the application
          rciop.log("ERROR", "Unsupported configuration",nameOfSrcFile_PC)
          q(save="no",status=99)
      }
  }else {
      # Using URL and query from stdin
      opensearchCmd=paste0("opensearch-client '",applInput,"' enclosure")
  }

  # Search for the main configuration and download the configuration object
  message(opensearchCmd)
  res_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", res_enclosure)

  # Download
  tmp_dir=paste0(TMPDIR,"/forecast/config")
  res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

  local.mainConfig <- NULL
  if (res_copy$exit.code==0) {
      #print("Status ok")
      local.mainConfig <- res_copy$output
  }else{
      rciop.log("ERROR","Could not find or download the main configuration object",nameOfSrcFile_PC)
      q(save="no",status=1)
  }

  # Read main configuration
  main_config_file <- paste0(local.mainConfig,"/dependencies.txt")

  if (! file.exists(main_config_file)) {
      rciop.log("ERROR",paste0("main configuration file missing: ",main_config_file),nameOfSrcFile_PC)
      q(save="no",status=1)
  }

  main_config_data <- read.csv2(main_config_file,header=TRUE,sep=";")

  #local.modelConfigSubDir <- NULL
  local.modelConfigUrl <- NULL
  local.modelConfigQuery <- NULL
  #local.modelConfigComment <- NULL
  #local.hydroGFDConfigSubDir <- NULL
  local.hydroGFDConfigUrl <- NULL
  local.hydroGFDConfigQuery <- NULL
  #local.hydroGFDConfigComment <- NULL
  statefileHindcastDate <- "1800-01-01"
  for (r in 1:nrow(main_config_data)) {
      subdir <- main_config_data[r,'localdirectory']
      if (subdir == 'hype-model') {
          #local.modelConfigSubDir  <- main_config_data[r,'localdirectory'] # Intended to be used as dir name for rciop.copy TMPDIR/subdir/
          local.modelConfigUrl     <- main_config_data[r,'url']
          local.modelConfigQuery   <- main_config_data[r,'searchquery']
          #local.modelConfigComment <- main_config_data[r,'comment']
      }
      if (subdir == 'hydrogfd-config') {
          #local.hydroGFDConfigSubDir  <- main_config_data[r,'localdirectory']
          local.hydroGFDConfigUrl     <- main_config_data[r,'url']
          local.hydroGFDConfigQuery   <- main_config_data[r,'searchquery']
          #local.hydroGFDConfigComment <- main_config_data[r,'comment']
      }
      if (subdir == 'statefile-bdate') {
          # Supposed date for hindcast period used when state file was created, e.g. "2019-01-01" or "20190101"
          # Later used when searching for available state files.
          statefileHindcastDate <- as.character(main_config_data[r,'searchquery'])
      }
  }

  modelConfigPath <- search_download_model_configuration(local.modelConfigUrl,
                                                         local.modelConfigQuery)
  #print(list.files(modelConfigPath))

  meteoConfigSearch <- search_download_meteo_configuration(local.hydroGFDConfigUrl,
                                                           local.hydroGFDConfigQuery)
    
  output <- list("modelConfig"=modelConfigPath,
                 "meteoConfig"=meteoConfigSearch,
                 "statefileHindcastDate"=statefileHindcastDate)

  return (output)
} # process_configuration_application_inputs

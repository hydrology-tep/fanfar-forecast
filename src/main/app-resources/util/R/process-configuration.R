#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

# Constants
nameOfSrcFile <- "/util/R/process-configuration.R"

verbose <- TRUE
#verbose <- FALSE

# Constants application.xml (tag option)
cHydModelVariant1 <- "WestAfrica-HYPE"
cHydModelVariant2 <- "Niger-HYPE"
cHydModelVariants <- c(cHydModelVariant1,cHydModelVariant2)

cMetHCVariant1 <- "GFD 1.3 (SMHI)"
cMetHCVariant2 <- "HydroGFD 2.0 (SMHI)"
cMetHCVariants <- c(cMetHCVariant1,cMetHCVariant2)

cMetFCVariant1 <- "ECOPER (SMHI)"
cMetFCVariants <- c(cMetFCVariant1)

cRunTypeVariant1 <- "Operational"
cRunTypeVariant2 <- "Reforecast"
cRunTypeVariants <- c(cRunTypeVariant1,cRunTypeVariant2)


# Process user options/selections to later select between different variants
# of models, datasets etc.
process_application_runtime_options <- function()
{
    hydModelIn <- rciop.getparam("hydmodel")
    if (hydModelIn == cHydModelVariant1){ hydModel <- cHydModelVariant1 }
    if (hydModelIn == cHydModelVariant2){ hydModel <- cHydModelVariant2 }

    metHCIn <- rciop.getparam("methc")
    if (metHCIn == cMetHCVariant1){ metHC <- cMetHCVariant1 }
    if (metHCIn == cMetHCVariant2){ metHC <- cMetHCVariant2 }

    metFCIn <- rciop.getparam("metfc")
    if (metFCIn == cMetFCVariant1){ metFC <- cMetFCVariant1 }

    runTypeIn <- rciop.getparam("runtype")
    if (runTypeIn == cRunTypeVariant1){ runType <- cRunTypeVariant1 }
    if (runTypeIn == cRunTypeVariant2){ runType <- cRunTypeVariant2 }

    # ToDo: Remove when date parsed from HYPE state filename etc.
    hypeStateDate <- rciop.getparam("hypeStateDate")

    rciop.log("INFO", "-------Global configuration options:-------", nameOfSrcFile)
    rciop.log("INFO", paste0("hydModel:      ",hydModel), nameOfSrcFile)
    rciop.log("INFO", paste0("metHC:         ",metHC), nameOfSrcFile)
    rciop.log("INFO", paste0("metFC:         ",metFC), nameOfSrcFile)
    rciop.log("INFO", paste0("runType:       ",runType), nameOfSrcFile)
    #rciop.log("INFO", paste0("hypeStateDate: ", hypeStateDate), nameOfSrcFile)
    rciop.log("INFO", "-------------------------------------------", nameOfSrcFile)

    outputApplRuntimeOptions <- list("hydModel"=hydModel,
                                     "metHC"=metHC,
                                     "metFC"=metFC,
                                     "runType"=runType,
                                     "hypeStateDate"=hypeStateDate
                                    )

} # process_application_runtime_options


# Extract information from file dependencies.txt, part of the model/application config object
# Output is now urls to configuration items to later be downloaded, e.g.:
# modelData   - locating HYPE model (config) data. Data files etc.
# gfdHydrogfd - locating netcdf files
process_input_model_configuration <- function(applConfig, # Application configuration from user, ToDo
                                              applInput)  # Input to application from stdin
                                              #tmpDir)    # Local dir for temporary items only
{
  # Download and read model config object from the applications input
  # Parse file dependencies.txt
  # Return all data in a list (skip the data frame)
  
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
  
  # Query the input reference
  opensearchCmd=paste0("opensearch-client '",applInput,"' enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)
  
  # Download the dir(s)
  model_config_dir <- rciop.copy(input_enclosure, TMPDIR, uncompress=TRUE) # tmpDir
  if (model_config_dir$exit.code==0) {
    local.model_config_dir <- model_config_dir$output # Returns path to local dir or file, dir in this case
  }else {
    rciop.log("ERROR Could not access the model configuration file.")
    q(save="no",status=99)
  }
  
  # Filenames
  model_config_file <- "dependencies.txt"
  #hype_config_file <- "info-forecast.txt"
  
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
  }
  
  # Return information as a list
  outputModelConfig <- list("modelDataSubDir"=modelDataSubDir,
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
                            "gfdEcoperComment"=gfdEcoperComment
  )
  
  #print(outputModelConfig)
  #rciop.log("INFO model config", outputModelConfig)
  return (outputModelConfig)
}


# Download HYPE model (config) data from the url part of the model/application config object
# Output is paths to downloaded data
process_input_hype_model_data <- function(applConfig,  # Application configuration from user, ToDo
                                          modelConfig) # Application model config object (urls)
                                          #tmpDir)      # Dir to store downloaded items
{
  # Outputs
  pathModelFiles <- NULL
  pathForcingArchive <- NULL
  pathStateFiles <- NULL  
  
  url <- modelConfig$modelDataUrl
  query <- modelConfig$modelDataQuery
  
  # Query the input reference
  opensearchCmd=paste0("opensearch-client '", url, query, "' enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)
  
  # Download the Hype model specific data dir(s)
  modelDataDirs <- rciop.copy(input_enclosure, TMPDIR, uncompress=TRUE) # tmpDir
  if (modelDataDirs$exit.code==0) {
    local.modelDataDirs <- modelDataDirs$output
  }
  else {
    rciop.log("ERROR Could not access the model data dirs.")
    q(save="no",status=98)
  }
  
  # ToDo: Support different HYPE model data versions via input/configuration
  pathModelFiles <- paste(local.modelDataDirs,"v2.23",sep="/") # Instead of model.files.url
  
  pathForcingArchive <- paste(pathModelFiles,"forcingarchive",sep="/") # Instead of forcing.archive.url
  pathStateFiles     <- paste(pathModelFiles,"statefiles",sep="/") # Instead of state.files.url
  
  # Return information as a list
  outputModelData <- list("pathModelFiles"=pathModelFiles,
                          "pathForcingArchive"=pathForcingArchive,
                          "pathStateFiles"=pathStateFiles
  )
  
  #print(outputModelData)
  rciop.log("INFO", outputModelData)
  return (outputModelData)
} # process_input_hype_model_data

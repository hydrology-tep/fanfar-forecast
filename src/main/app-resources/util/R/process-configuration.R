#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

# Constants
verbose <- TRUE
#verbose <- FALSE

process_input_model_configuration <- function(applInput, # Input to application
                                              tmpDir)    # Local temporary dir
{
  # Read input from application
  # Parse file dependencies.txt
  # Return all data in a list (skip the data frame)
  
  # Outputs
  modelDataSubDir  <- NULL
  modelDataUrl     <- NULL
  modelDataQuery   <- NULL
  modelDataComment <- NULL
  
  modelDataOldSubDir  <- NULL
  modelDataOldUrl     <- NULL
  modelDataOldQuery   <- NULL
  modelDataOldComment <- NULL
  
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
  opensearchCmd=paste("opensearch-client '",applInput,"' enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)
  
  # Download the dir(s)
  model_config_dir <- rciop.copy(input_enclosure, tmpDir, uncompress=TRUE)
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
    if (subdir == 'model-data-old-url') {
      modelDataOldSubDir  <- model_config_data[r,'subdir'] # Not used
      modelDataOldUrl     <- model_config_data[r,'url']
      modelDataOldQuery   <- model_config_data[r,'querypattern'] # Not used
      modelDataOldComment <- model_config_data[r,'info']
    }
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
                            
                            "modelDataOldSubDir"=modelDataOldSubDir,
                            "modelDataOldUrl"=modelDataOldUrl,
                            "modelDataOldQuery"=modelDataOldQuery,
                            "modelDataOldComment"=modelDataOldComment,
                            
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


process_input_model_data <- function(modelConfig,
                                     tmpDir)    # Local temporary dir
{
  # Outputs
  pathModelFiles <- NULL
  pathForcingArchive <- NULL
  pathStateFiles <- NULL  
  
  url <- modelConfig$modelDataUrl
  query <- modelConfig$modelDataQuery
  
  # Query the input reference
  opensearchCmd=paste("opensearch-client '", url, query, "' enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  rciop.log("INFO", input_enclosure)
  
  # Download the Hype model specific data dir(s)
  modelDataDirs <- rciop.copy(input_enclosure, tmpDir, uncompress=TRUE) # or TMPDIR/subdir
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
}

#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

# Constants
nameOfSrcFile_PC <- "/util/R/process-configuration.R"

#verbose <- TRUE
#verbose <- FALSE

source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/constants.R",sep="/"))

# Process user options/selections to later select between different variants
# of models, datasets etc.
process_configuration_application_runtime_options <- function(applInput=NULL) # Input to application from stdin
{
    # N.B. input to the application (input) cannot be completely empty due to the condition for the main/first while loop in run.R.

    # Outputs
    prelMainConfig <- NULL
    prelModelConfig <- NULL
    runType  <- NULL
    runTypeStateFileCreation <- NULL


    urlDefault <- 'https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=E8063368628860F74178C1F3C7FB5DA11B5D97C0'
    urlLen     <- nchar(urlDefault)

    if (is.null(applInput) || nchar(applInput) < urlLen){
        # Using (url) from parameter field 'model config file' or
        # selected model configuration from parameter field 'model config'
        urlSelected <- FALSE
        urlModelConfigFile <- "No value"
        modelConfigIn <- "No value"

        # It's up to the user to specify a correct url, so skip any checks of a valid url.
        # Check available configurations: https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid&format=json
        urlModelConfigFile <- rciop.getparam("model_config_file")
        if ( (length(urlModelConfigFile) > 0) && ! is.null(urlModelConfigFile) ) {
            if (nchar(urlModelConfigFile) > nchar("https://recast.terradue.com")) {
                cmn.log("Using URL from optional parameter field (model config file) as the main model configuration object", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
                urlAndQuery <- urlModelConfigFile
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE
            }
        }

        if (urlSelected) {
            # Read model config name string from csv file. Information not available at this stage
        }else{
            # No model config file as input
            # Use the user selected configuration from the drop down field
            modelConfigIn <- rciop.getparam("model_config")

            tmpAssimOn    <- rciop.getparam("assimOn")     # Assimilation on/off
            assimOnARUpd  <- FALSE
            if(tmpAssimOn == "on with auto-regressive updating"){
                assimOnARUpd <- TRUE
            }

            if (modelConfigIn == cModelConfigVariant1) {
                prelModelConfig <- cModelConfigVariant1
                urlAndQuery <- paste0("No model config URL for ",cModelConfigVariant1)
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                #urlSelected <- TRUE

            }else if (modelConfigIn == cModelConfigVariant2) {
                prelModelConfig <- cModelConfigVariant2
                urlAndQuery <- urlDefault
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }else if ((modelConfigIn == cModelConfigVariant3) & (assimOnARUpd == FALSE)) {
                prelModelConfig <- cModelConfigVariant3
                urlAndQuery <- "https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=956373199977FBE1A7FFAD708379E7B3ABA108A5"
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }else if ((modelConfigIn == cModelConfigVariant3) & (assimOnARUpd == TRUE)) {
                # Configuration with a state file supporting AR update
                prelModelConfig <- cModelConfigVariant3
                urlAndQuery <- "https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=41C4191C145B1730C28313B1D6AC80A7A29DC172"
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }else if ((modelConfigIn == cModelConfigVariant4) & (assimOnARUpd == FALSE)) {
                prelModelConfig <- cModelConfigVariant4
                urlAndQuery <- "https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=8936FADB1602786E7178B389A854F224BA4A552F"
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }else if ((modelConfigIn == cModelConfigVariant4) & (assimOnARUpd == TRUE)) {
                # Configuration with a state file supporting AR update
                prelModelConfig <- cModelConfigVariant4
                urlAndQuery <- "https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=77D99AC80CEB0F94B4711967D7D37A4C73AB283A"
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }else if (modelConfigIn == cModelConfigVariant5) {
                prelModelConfig <- cModelConfigVariant5
                urlAndQuery <- "https://recast.terradue.com/t2api/search/hydro-smhi/fanfar/forecast/config?uid=278E284C83C4301BA86312B75536A73B7B8F3A9F"
                opensearchCmd=paste0("opensearch-client '",urlAndQuery,"' enclosure")
                urlSelected <- TRUE

            }
            if (! urlSelected) {
                cmn.log(paste0("Unsupported configuration via parameter 'model_config': ",modelConfigIn), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
            }
        }

        if (! urlSelected) {
            # Exit the application
            cmn.log(paste0("Unsupported configuration via parameter 'model_config_file' or 'model_config': ",urlModelConfigFile,modelConfigIn), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
            q(save="no",status=99)
        }
    }else {
        # Using URL and query from input (stdin)
        opensearchCmd=paste0("opensearch-client '",applInput,"' enclosure")
    }

    # Search for the main model configuration and download the configuration object
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    cmn.log(res_enclosure, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)

    # Download
    tmp_dir=paste0(TMPDIR,"/forecast/config")
    res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

    if (res_copy$exit.code==0) {
        prelMainConfig <- res_copy$output
    }


    runTypeIn <- rciop.getparam("runtype")
    if (runTypeIn == cRunTypeVariantOperational){
      runType                  <- cRunTypeVariantOperational
      runTypeStateFileCreation <- cRunTypeVariantOperational # False
    }
    if (runTypeIn == cRunTypeVariantReforecast){
      runType                  <- cRunTypeVariantReforecast
      runTypeStateFileCreation <- cRunTypeVariantReforecast # False
    }
    if (runTypeIn == cRunTypeVariantStatefile){
      runType                  <- cRunTypeVariantReforecast
      runTypeStateFileCreation <- cRunTypeVariantStatefile # True
    }

    cmn.log("-------Run configuration options:-------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log(paste0("run type:               ",runType), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    if (runTypeStateFileCreation == cRunTypeVariantStatefile){
           cmn.log("statefile creation:     TRUE", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    }
    cmn.log(paste0("forecast issue date:    ",rciop.getparam("idate")), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log(paste0("hindcast period length: ",rciop.getparam("hcperiodlen")), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log("-------------------------------------------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)

    outputApplRuntimeOptions <- list("prelMainConfig"=prelMainConfig,
                                     "prelModelConfig"=prelModelConfig,
                                     "runType"=runType,
                                     "runTypeStateFileCreation"=runTypeStateFileCreation
                                     )

    return (outputApplRuntimeOptions)

} # process_configuration_application_runtime_options


check_dir_exist <- function(absPath)
{
    if(is.null(absPath) || !dir.exists(absPath)) {
        cmn.log(paste0("Dir missing: ",absPath), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    }
}


check_file_exist <- function(absPath)
{
    if(is.null(absPath) || !file.exists(absPath)) {
        cmn.log(paste0("File missing: ",absPath), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    }
}


# Search for the HYPE model configuration and download the model data
search_download_model_configuration <- function(url,
                                                query)
{
    # Outputs
    modelConfigPath <- NULL

    if (is.null(url) || is.null(query)){
        cmn.log("HYPE model configuration items missing", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=11)
    }

    # Query the model config reference
    opensearchCmd=paste0("opensearch-client '",url,query,"' enclosure")
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    cmn.log(res_enclosure, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)

    # Download
    tmp_dir=paste0(TMPDIR,"/hype-model/config")
    res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

    if (res_copy$exit.code==0) {
        modelConfigPath <- res_copy$output
    }else{
        cmn.log("Could not find or download the configuration", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=12)
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
    
    gfdHydrogfdodSubDir  <- NULL
    gfdHydrogfdodUrl     <- NULL
    gfdHydrogfdodQuery   <- NULL
    
    gfdOdDailySubDir  <- NULL
    gfdOdDailyUrl     <- NULL
    gfdOdDailyQuery   <- NULL
    
    gfdEcoperSubDir  <- NULL
    gfdEcoperUrl     <- NULL
    gfdEcoperQuery   <- NULL
    
    gfdElevationSubDir  <- NULL
    gfdElevationUrl     <- NULL
    gfdElevationQuery   <- NULL
    gfdElevationDoSearch <- NULL
    
    gfdGridSubDir  <- NULL
    gfdGridUrl     <- NULL
    gfdGridQuery   <- NULL
    gfdGridDoSearch <- NULL
    
    gfdHe5SubDir  <- NULL
    gfdHe5Url     <- NULL
    gfdHe5Query   <- NULL

    gfdHe5tmSubDir  <- NULL
    gfdHe5tmUrl     <- NULL
    gfdHe5tmQuery   <- NULL

    gfdHe5tdSubDir  <- NULL
    gfdHe5tdUrl     <- NULL
    gfdHe5tdQuery   <- NULL
    
    gfdOdSubDir  <- NULL
    gfdOdUrl     <- NULL
    gfdOdQuery   <- NULL
    
    gfdOdfSubDir  <- NULL
    gfdOdfUrl     <- NULL
    gfdOdfQuery   <- NULL

    if (is.null(url) || is.null(query)){
        cmn.log("HydroGFD meteo configuration items missing", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=13)
    }

    # Query the meteo config reference
    opensearchCmd=paste0("opensearch-client '",url,query,"' enclosure")
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    cmn.log(res_enclosure, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)

    # Download
    tmp_dir=paste0(TMPDIR,"/hydrogfd/config")
    res_copy <- rciop.copy(res_enclosure,target=tmp_dir,uncompress=TRUE,createOutputDirectory=TRUE)

    local.meteoConfig <- NULL
    if (res_copy$exit.code==0) {
        local.meteoConfig <- res_copy$output
    }else{
        cmn.log("Could not find or download the configuration", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=14)
    }

    # Read the hydrogfd meteo configuration file
    meteo_config_file <- paste0(local.meteoConfig,"/dependencies.txt")

    if (! file.exists(meteo_config_file)) {
        cmn.log("Configuration file missing", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=15)
    }

    meteo_config_data <- read.csv2(meteo_config_file, header=TRUE, sep=";")

    for (r in 1:nrow(meteo_config_data)) {
        subdir <- meteo_config_data[r,'localdirectory']
        if (subdir == 'hydrogfdei') {
          gfdHydrogfdeiSubDir  <- meteo_config_data[r,'localdirectory'] # Intended to be used both for dir name (rciop.copy) and part of filename
          gfdHydrogfdeiUrl     <- meteo_config_data[r,'url']
          gfdHydrogfdeiQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'hydrogfdod') {
          gfdHydrogfdodSubDir  <- meteo_config_data[r,'localdirectory']
          gfdHydrogfdodUrl     <- meteo_config_data[r,'url']
          gfdHydrogfdodQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'od-daily') {
          gfdOdDailySubDir  <- meteo_config_data[r,'localdirectory']
          gfdOdDailyUrl     <- meteo_config_data[r,'url']
          gfdOdDailyQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'ecoper') {
          gfdEcoperSubDir  <- meteo_config_data[r,'localdirectory']
          gfdEcoperUrl     <- meteo_config_data[r,'url']
          gfdEcoperQuery   <- meteo_config_data[r,'searchquery']
        }
        # Skip subdir 'ei-monthly' and 'od-monthly'
        if (subdir == 'elevation') {
          gfdElevationSubDir  <- meteo_config_data[r,'localdirectory']
          gfdElevationUrl     <- meteo_config_data[r,'url']
          gfdElevationQuery   <- meteo_config_data[r,'searchquery']
          gfdElevationDoSearch <- meteo_config_data[r,'dosearch']
        }
        if (subdir == 'grid-meta') {
          gfdGridSubDir  <- meteo_config_data[r,'localdirectory']
          gfdGridUrl     <- meteo_config_data[r,'url']
          gfdGridQuery   <- meteo_config_data[r,'searchquery']
          gfdGridDoSearch <- meteo_config_data[r,'dosearch']
        }
        if (subdir == 'he5tm') {
          gfdHe5tmSubDir  <- meteo_config_data[r,'localdirectory']
          gfdHe5tmUrl     <- meteo_config_data[r,'url']
          gfdHe5tmQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'he5td') {
          gfdHe5tdSubDir  <- meteo_config_data[r,'localdirectory']
          gfdHe5tdUrl     <- meteo_config_data[r,'url']
          gfdHe5tdQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'he5') {
          gfdHe5SubDir  <- meteo_config_data[r,'localdirectory']
          gfdHe5Url     <- meteo_config_data[r,'url']
          gfdHe5Query   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'odf') {
          gfdOdfSubDir  <- meteo_config_data[r,'localdirectory']
          gfdOdfUrl     <- meteo_config_data[r,'url']
          gfdOdfQuery   <- meteo_config_data[r,'searchquery']
        }
        if (subdir == 'od') {
          gfdOdSubDir  <- meteo_config_data[r,'localdirectory']
          gfdOdUrl     <- meteo_config_data[r,'url']
          gfdOdQuery   <- meteo_config_data[r,'searchquery']
        }
    }

    output <- list("gfdHydrogfdeiSubDir"=gfdHydrogfdeiSubDir,
                   "gfdHydrogfdeiUrl"=gfdHydrogfdeiUrl,
                   "gfdHydrogfdeiQuery"=gfdHydrogfdeiQuery,
                            
                   "gfdHydrogfdodSubDir"=gfdHydrogfdodSubDir,
                   "gfdHydrogfdodUrl"=gfdHydrogfdodUrl,
                   "gfdHydrogfdodQuery"=gfdHydrogfdodQuery,
                            
                   "gfdOdDailySubDir"=gfdOdDailySubDir,
                   "gfdOdDailyUrl"=gfdOdDailyUrl,
                   "gfdOdDailyQuery"=gfdOdDailyQuery,
                            
                   "gfdEcoperSubDir"=gfdEcoperSubDir,
                   "gfdEcoperUrl"=gfdEcoperUrl,
                   "gfdEcoperQuery"=gfdEcoperQuery,
                            
                   "gfdElevationSubDir"=gfdElevationSubDir,
                   "gfdElevationUrl"=gfdElevationUrl,
                   "gfdElevationQuery"=gfdElevationQuery,
                   "gfdElevationDoSearch"=gfdElevationDoSearch,
                            
                   "gfdGridSubDir"=gfdGridSubDir,
                   "gfdGridUrl"=gfdGridUrl,
                   "gfdGridQuery"=gfdGridQuery,
                   "gfdGridDoSearch"=gfdGridDoSearch,
                   
                   "gfdHe5SubDir"=gfdHe5SubDir,
                   "gfdHe5Url"=gfdHe5Url,
                   "gfdHe5Query"=gfdHe5Query,

                   "gfdHe5tmSubDir"=gfdHe5tmSubDir,
                   "gfdHe5tmUrl"=gfdHe5tmUrl,
                   "gfdHe5tmQuery"=gfdHe5tmQuery,

                   "gfdHe5tdSubDir"=gfdHe5tdSubDir,
                   "gfdHe5tdUrl"=gfdHe5tdUrl,
                   "gfdHe5tdQuery"=gfdHe5tdQuery,
                    
                   "gfdOdSubDir"=gfdOdSubDir,
                   "gfdOdUrl"=gfdOdUrl,
                   "gfdOdQuery"=gfdOdQuery,
                    
                   "gfdOdfSubDir"=gfdOdfSubDir,
                   "gfdOdfUrl"=gfdOdfUrl,
                   "gfdOdfQuery"=gfdOdfQuery
                  )

    return (output)
} # search_download_meteo_configuration


# Download selected main configuration and sub-parts
process_configuration_application_inputs <- function(applRuntimeOptions=NULL) # Application configuration from user, ToDo
{

    # Outputs
    modelConfigName        <- NULL
    hydrologicalModel      <- NULL
    meteoHindcast          <- NULL
    #meteoForecast          <- NULL
    modelFiles             <- NULL
    meteoConfigSearch      <- NULL
    statefileHindcastDate  <- "1800-01-01"
    configGridLinkFilename <- NULL
    reforecastingMethod    <- NULL
    modelBin               <- NULL
    

    # Read model configuration
    main_config_file <- paste0(applRuntimeOptions$prelMainConfig,"/dependencies.txt")

    if (! file.exists(main_config_file)) {
        cmn.log(paste0("Model configuration file missing: ",main_config_file), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
        q(save="no",status=16)
    }

    main_config_data <- read.csv2(main_config_file,header=TRUE,sep=";")

    local.modelConfigName <- NULL
    local.modelConfigUrl <- NULL
    local.modelConfigQuery <- NULL
    local.hydroGFDConfigUrl <- NULL
    local.hydroGFDConfigQuery <- NULL
    for (r in 1:nrow(main_config_data)) {
        subdir <- main_config_data[r,'localdirectory']
        if (subdir == 'model-config-name') {
            # Optional
            local.modelConfigName   <- main_config_data[r,'searchquery']
        }
        if (subdir == 'hype-model') {
            #local.modelConfigSubDir  <- main_config_data[r,'localdirectory'] # Intended to be used as dir name for rciop.copy TMPDIR/subdir/
            local.modelConfigUrl     <- main_config_data[r,'url']
            local.modelConfigQuery   <- main_config_data[r,'searchquery']
            #local.modelConfigComment <- main_config_data[r,'comment']
        }
        if (subdir == 'hydrogfd-config') {
            local.hydroGFDConfigUrl     <- main_config_data[r,'url']
            local.hydroGFDConfigQuery   <- main_config_data[r,'searchquery']
        }
        if (subdir == 'statefile-bdate') {
            # Supposed date for hindcast period used when state file was created, e.g. "2019-01-01" or "20190101"
            # Later used when searching for available state files.
            statefileHindcastDate <- as.character(main_config_data[r,'searchquery'])
        }
        if (subdir == 'gridlink-filename') {
            # Filename of corresponding file gridLink.Rdata in dir <configuration object>/shapefiles/
            configGridLinkFilename <- as.character(main_config_data[r,'searchquery'])
        }
        if (subdir == 'reforecasting-method') {
            # 1 - standard, 2 - he5+od
            reforecastingMethod <- as.numeric(main_config_data[r,'searchquery'])
        }
        if (subdir == 'model-bin') {
            # Filename of HYPE binary/executable file
            modelBin <- as.character(main_config_data[r,'searchquery'])
        }
    }

    # Main model configuration

    if (is.null(local.modelConfigName)){
        # No configuration name from csv file (optional)

        # Check selection by user
        if (is.null(applRuntimeOptions$prelModelConfig)){
            # Exit the application
            cmn.log(paste0("Unsupported configuration, 'model-config-name' missing in ",main_config_file), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
            q(save="no",status=98)
        }else{
            # Use selection from parameter field 'model_config'
            modelConfigName = applRuntimeOptions$prelModelConfig
        }
    }else{
        # Check against supported configurations
        resConfigName = NULL
        
        for (v in 1:length(cModelConfigVariants)) {
            if (local.modelConfigName == cModelConfigVariants[v]){
                resConfigName = cModelConfigVariants[v]
            }
        }

        if (is.null(resConfigName)){
            # Unknown variant, try to extract meteo variants at least for hindcast and forecast
            if (grepl(tolower('Niger'),tolower(local.modelConfigName),fixed=TRUE)) {
                hydrologicalModel = cHydrologicalModelVariant1
            }else if (grepl(tolower('World'),tolower(local.modelConfigName),fixed=TRUE)) {
                hydrologicalModel = cHydrologicalModelVariant2
            }else if (grepl(tolower('West'),tolower(local.modelConfigName),fixed=TRUE)) {
                hydrologicalModel = cHydrologicalModelVariant3
            }else{
                # Not found, use complete name
                hydrologicalModel = local.modelConfigName # ToDo: Extract first part of string
            }

            if (grepl(cMeteoHindcastVariant1,local.modelConfigName,fixed=TRUE)) {
                meteoHindcast = cMeteoHindcastVariant1
            }else if (grepl(cMeteoHindcastVariant2,local.modelConfigName,fixed=TRUE)) {
                meteoHindcast = cMeteoHindcastVariant2
            }else if (grepl(cMeteoHindcastVariant3,local.modelConfigName,fixed=TRUE)) {
                meteoHindcast = cMeteoHindcastVariant3
            }

            # if (grepl(cMeteoForecastVariant1,local.modelConfigName,fixed=TRUE)) {
            #     meteoForecast = cMeteoForecastVariant1
            # }else if (grepl(cMeteoForecastVariant2,local.modelConfigName,fixed=TRUE)) {
            #     meteoForecast = cMeteoForecastVariant2
            # }

            #if (! is.null(meteoHindcast) && ! is.null(meteoForecast)){
            if (! is.null(meteoHindcast)){
                # Still an unsupported configuration and hydrologicalModel may be unknown/incorrect, but
                # at least meteo forcing for hindcast and forecast is known
                resConfigName = local.modelConfigName
                cmn.log("Not fully supported configuration from 'model_config_file', attempt to run with the following settings:", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
                cmn.log(paste0("hydrological model:      ",hydrologicalModel), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
                # cmn.log(paste0("meteo forcing hindcast:  ",meteoHindcast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
                # cmn.log(paste0("meteo forcing forecast:  ",meteoForecast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
                cmn.log(paste0("meteo forcing:           ",meteoHindcast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
            }
        }

        if (is.null(resConfigName)){
            # Exit the application
            cmn.log(paste0("Unsupported configuration, user selection not valid ",local.modelConfigName), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
            q(save="no",status=97)
        }else{
            modelConfigName = resConfigName
        }
    # }else{
    #     # Check against supported configurations
    #     resConfigName = NULL
        
    #     for (v in 1:length(cModelConfigVariants)) {
    #         if (local.modelConfigName == cModelConfigVariants[v]){
    #             resConfigName = cModelConfigVariants[v]
    #         }
    #     }
    #     if (is.null(resConfigName)){
    #         # Exit the application
    #         cmn.log(paste0("Unsupported configuration, user selection not valid ",local.modelConfigName), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PC)
    #         q(save="no",status=97)
    #     }else{
    #         modelConfigName = resConfigName
    #     }
    } # local.modelConfigName

    # Main configuration available
    # Assign outputs mainly for use as conditions in run.R to call different functions
    if (modelConfigName == cModelConfigVariant1) {
        hydrologicalModel = cHydrologicalModelVariant1
        meteoHindcast     = cMeteoHindcastVariant1
        #meteoForecast     = cMeteoForecastVariant1

    }else if (modelConfigName == cModelConfigVariant2) {
        hydrologicalModel = cHydrologicalModelVariant2
        meteoHindcast     = cMeteoHindcastVariant2
        #meteoForecast     = cMeteoForecastVariant1

    }else if (modelConfigName == cModelConfigVariant3) {
        hydrologicalModel = cHydrologicalModelVariant3
        meteoHindcast     = cMeteoHindcastVariant2
        #meteoForecast     = cMeteoForecastVariant1

    }else if (modelConfigName == cModelConfigVariant4) {
        hydrologicalModel = cHydrologicalModelVariant3
        meteoHindcast     = cMeteoHindcastVariant3
        #meteoForecast     = cMeteoForecastVariant2

    }else if (modelConfigName == cModelConfigVariant5) {
        hydrologicalModel = cHydrologicalModelVariant1
        meteoHindcast     = cMeteoHindcastVariant3
        #meteoForecast     = cMeteoForecastVariant2
    }

    # HYPE model
    modelFiles <- search_download_model_configuration(local.modelConfigUrl,
                                                      local.modelConfigQuery)
    #print(list.files(modelFiles))

    # GFD/HydroGFD
    meteoConfigSearch <- search_download_meteo_configuration(local.hydroGFDConfigUrl,
                                                             local.hydroGFDConfigQuery)

    cmn.log("-------Model configuration options:-------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log(modelConfigName, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log(paste0("hydrological model:      ",hydrologicalModel), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    # cmn.log(paste0("meteo forcing hindcast:  ",meteoHindcast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    # cmn.log(paste0("meteo forcing forecast:  ",meteoForecast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    cmn.log(paste0("meteo forcing:           ",meteoHindcast), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    if (! is.null(reforecastingMethod)){
        cmn.log(paste0("reforcasting method:     ",reforecastingMethod), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    }
    if (! is.null(modelBin)){
        cmn.log(paste0("HYPE binary/executable:  ",modelBin), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)
    }
    cmn.log("-------------------------------------------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PC)

    output <- list("modelConfigName"=modelConfigName, # Do not use for if statements etc. Rather use the individual parts meteoHindcast etc.
                   "hydrologicalModel"=hydrologicalModel,  # Do not use for if statements etc. at this time, for now treat more as info
                   "meteoHindcast"=meteoHindcast,
                   #"meteoForecast"=meteoForecast,
                   "modelFiles"=modelFiles,
                   "meteoConfig"=meteoConfigSearch,
                   "statefileHindcastDate"=statefileHindcastDate,
                   "configGridLinkFilename"=configGridLinkFilename,
                   "reforecastingMethod"=reforecastingMethod,
                   "modelBin"=modelBin)

    return (output)
} # process_configuration_application_inputs

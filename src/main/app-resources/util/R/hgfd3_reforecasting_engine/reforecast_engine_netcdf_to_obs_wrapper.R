
#
# Interface/Wrapper to netcdf_to_obs
# Depending on system (local,server or tep) path to the netcdf_to_obs source code
# and wrapper script is handled as input.
# Differences within netcdf_to_obs, e.g. source of less R packages, can be handled
# via the input currentSystem (local,server or tep).
#



netcdf_to_obs_wrapper<-function(pathToNetcdfToObs,        # path to external netcdf_to_obs source code
                                pathToNetcdfToObsRScript, # path+filename to wrapper script (external netcdf_to_obs source code)
                                currentSystem='local',    # Corresponds to app.sys variable used on different systems (local,server or tep)
                                workDir,           # Temporary dir
                                netcdfRootDir,     # Path to netcdf files
                                isNetcdfSubDir=F,  # True - netcdfRootDir contains the subdirs: pr tas tasmin tasmax
                                resourceDir,       # Path to resources (shapefiles named grid.point.shp and grid.polygon.shp),
                                gridElevFile,      # Path+filename to netcdf file with elevation data
                                shapeFile,         # Path+filename to subid shapefile (e.g. from dir <model>/subshapefile/)
                                gridLinkFile=NULL, # Path+filename to an existing gridLink.Rdata (e.g. from dir <model>/shapefiles/)
                                startDate,         # String or date object
                                endDate,           # String or date object
                                outputDir,         # Path to output dir for produced P/T/TMIN/TMAXobs files and gridLink.Rdata.
                                verbose=F          # List dirs, enable printout etc.
                               )
{
    if (verbose) {
        print('netcdf_to_obs_wrapper:')
        print(pathToNetcdfToObs)
        print(pathToNetcdfToObsRScript)
        print(currentSystem)
        print(workDir)
        print(netcdfRootDir)
        print(isNetcdfSubDir)
        print(resourceDir)
        print(gridElevFile)
        print(shapeFile)
        print(gridLinkFile)
        print(startDate)
        print(endDate)
        print(outputDir)
    }

    resGridLink = 0
    resObs = 0

    print(paste0('source: ',pathToNetcdfToObsRScript))

    if (! file.exists(pathToNetcdfToObsRScript)){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",pathToNetcdfToObsRScript),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs - file missing: ",pathToNetcdfToObsRScript)) # Change to cmn.log()
        q(save="no", status = 1)
    }
    source(pathToNetcdfToObsRScript) 

    # Filename used by netcdf_to_obs
    fileGridLinkNetcdfToObs = paste0(outputDir,'/gridLink.Rdata')


    # Copy and rename file
    if (! is.null(gridLinkFile)){ 
        if (file.exists(gridLinkFile)) {
            if (! dir.exists(outputDir)){
                dir.create(outputDir)
            }
            print(paste0('cp ',gridLinkFile,' to ',fileGridLinkNetcdfToObs,'/'))
            file.copy(from=gridLinkFile,to=fileGridLinkNetcdfToObs,overwrite=TRUE)
            #ToDo enable   cmn.log(paste0('cp ',gridLinkFile,' to ',fileGridLinkNetcdfToObs,'/'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_PN)
        }
    }

    if (is.null(gridLinkFile) || ! file.exists(fileGridLinkNetcdfToObs)) {
        # Create gridLink.Rdata

        # Rename these input/output parameters below to names according to names used in the different netcdf_to_obs functions
        resGridLink <- netcdf_to_obs_gridLinkPreparation(
                            pathToNetcdfToObs=pathToNetcdfToObs,
                            workDir=workDir,
                            ncRootDir=netcdfRootDir,
                            ncSubDir=isNetcdfSubDir,
                            resourceDir=resourceDir,
                            gridElevPath=gridElevFile,
                            shapeFilePath=shapeFile,
                            outPath=outputDir, # Location of produced gridLink.Rdata file
                            startDate=startDate,
                            endDate=endDate,
                            currentSystem=currentSystem,
                            verbose=verbose,
                            verboseVerbose=F)

        if (resGridLink > 0){
            #rciop.log('INFO',paste0('netcdf_to_obs_gridLinkPreparation, exit code=',resGridLink),nameOfSrcFile_PN)
            print(paste0('netcdf_to_obs_gridLinkPreparation, exit code=',resGridLink)) # Replace by cmn.log()
        }

        if (verbose) {
            # List produced files
            print(list.files(outputDir))
        }
    }


    if (! file.exists(fileGridLinkNetcdfToObs)){
        #ToDo enable   cmn.log('Aborting, no gridLink.Rdata file', logHandle, rciopStatus='ERROR', rciopProcess=nameOfSrcFile_PN)
        print('Error: Aborting, no gridLink.Rdata file')
    }else {
        # Create obs files

        # Rename these input/output parameters below to names according to names used in the different netcdf_to_obs functions
        resObs <- netcdf_to_obs_readGridsAndWriteObs(
                        pathToNetcdfToObs=pathToNetcdfToObs,
                        workDir=workDir,
                        ncRootDir=netcdfRootDir,
                        ncSubDir=isNetcdfSubDir,
                        gridLinkFile=fileGridLinkNetcdfToObs,
                        outPath=outputDir, # Location of produced obs files
                        startDate=startDate,
                        endDate=endDate,
                        currentSystem=currentSystem,
                        verbose=verbose,
                        verboseVerbose=F)

        if (resObs > 0){
            #rciop.log('INFO',paste0('netcdf_to_obs_readGridsAndWriteObs, exit code=',resObs),nameOfSrcFile_PN)
            print(paste0('netcdf_to_obs_readGridsAndWriteObs, exit code=',resObs)) # Replace by cmn.log()
        }
    }

    if (verbose) {
        # List produced files
        print(list.files(outputDir))
    }

    return (resGridLink + resObs)

} # netcdf_to_obs_wrapper

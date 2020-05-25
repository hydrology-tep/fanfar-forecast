#
# Common log functionality.
# Redirect log text to all or one or more selected outputs,
# selected by calling logOpen
#
# To only output log text to stdout via 'print' its not necessary
# to first call logOpen() to receive a log handle, instead use
# log() directly.
#
# Log text directed to file is prepended with a timestamp and a tab
# character.
#


cmn.logOpen <- function(currentSystem=NULL, # app.sys='tep'
                        toStdout=FALSE,     # print()
                        toRCIOPLOG=FALSE,   # rciop.log()
                        fileName=NULL,      # to file
                        filePath=NULL)      #
{
    # verbose = F

    # if (verbose) {
    #     print('cmn.logOpen():')
    #     print(currentSystem)
    #     print(toStdout)
    #     print(toRCIOPLOG)
    #     print(fileName)
    #     print(filePath)
    # }

    stdout     = FALSE
    rciopLog   = FALSE
    fileHandle = NULL
    f          = NULL

    if (toStdout) {
        print('Redirect log text to stdout via print()')
        stdout = TRUE
    }

    if (toRCIOPLOG && ! is.null(currentSystem)) {
        if (currentSystem == 'tep'){
            library('rciop') # ToDo: Check if lib available and/or already loaded

            print('Redirect log text to rciop.log()')
            rciopLog = TRUE
        }
    }

    if (! is.null(fileName)) {

        if (is.null(filePath)) {
            path = getwd()
        } else {
            path = filePath
        }

        f = paste(path,fileName,sep='/')
        fileHandle = file(f,open='wt')

        if (! is.null(fileHandle)) {
            print(paste0('Redirect log text to file: ',f))
            writeLines(paste0(format(Sys.time(), format = '%Y-%m-%dT%H:%M:%S'),
                              '\t',
                              'Log started'),
                       fileHandle)
        }
    }

    return(list('stdout'=stdout,
                'rciopLog'=rciopLog,
                'fileHandle'=fileHandle,
                'file'=f))
} # cmn.logOpen


cmn.log <- function(logText,            # text string to file and/or output
                    logHandle=NULL,     # handle from open function
                    logFileDate=TRUE,   # text to file preprended with time stamp and tab character
                    rciopStatus='INFO', # INFO, ERROR or DEBUG
                    rciopProcess=NULL)  # optional text, see rciop.log()
{

    if (is.null(logHandle)) {
        print(logText)
    } else {
        if (logHandle$stdout) {
            print(logText)
        }

        if (logHandle$rciopLog) {
            if (is.null(rciopProcess)) {
                rciop.log(rciopStatus,logText)
            } else {
                rciop.log(rciopStatus,logText,rciopProcess)
            }
        }

        if (! is.null(logHandle$fileHandle)) {
            if (logFileDate) {
                writeLines(paste0(format(Sys.time(), format = '%Y-%m-%dT%H:%M:%S'),
                                 '\t',
                                 logText),
                           logHandle$fileHandle)
            } else {
                writeLines(logText,
                           logHandle$fileHandle)
            }
        }
   }
} # cmn.log


cmn.logClose <- function(logHandle=NULL) # handle from open function
{
    status = 0

    if (! is.null(logHandle)) {
        if (! is.null(logHandle$fileHandle)) {
            print(paste0('Closing log file: ',logHandle$file))
            writeLines(paste0(format(Sys.time(), format = '%Y-%m-%dT%H:%M:%S'),
                              '\t',
                              'Log ended'),
                       logHandle$fileHandle)
            status = close(logHandle$fileHandle)
            #print('close status: ')
            #print(status)
            #if (status != 0) {
            #    print('Error closing file')
            #}
        }  
    }

    return(status)
} # cmn.logClose

#
# Example of usage from a main program:
#
# logHandle = cmn.logOpen(currentSystem=app.sys,
#                          toStdout=(verbose==TRUE),
#                          toRCIOPLOG=(app.sys=='tep'),
#                          fileName=fileName,
#                          filePath=TMPDIR)

# # In main and other source'ed code
# cmn.log('Inputs and parameters read',logHandle,rciopStatus='INFO',rciopProcess='run.R')
# cmn.log(paste0('File missing: ',fromFile),logHandle,rciopStatus='ERROR',rciopProcess='utils/R/xyz.R')

# status = cmn.logClose(logHandle)
# if (status != 0) {
#     print('Error closing file')
#     print(logHandle$file)
# }

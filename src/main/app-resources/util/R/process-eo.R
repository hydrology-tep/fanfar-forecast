# INITALISATION
if(app.sys=='tep') {
    source(paste(Sys.getenv('_CIOP_APPLICATION_PATH'), 'util/R/hgfd3_reforecasting_engine/utils_log.R',sep='/'))
    source(paste(Sys.getenv('_CIOP_APPLICATION_PATH'), 'util/R/hypeapps-hype-utils.R',sep='/'))
}else{
    source ('utils_log.R')
    source ('hypeapps-hype-utils.R')
}
library(foreign)


# CONSTANTS
nameOfSrcFile_EOP = '/util/R/process-eo.R'


######################
# FUNCTION DEFINITIONS

# G-P002-KATI-M.csv seems to miss the last end of line character in the file, can maybe be handled by readLine
# csvdata <- read.table( 
#     text = readLines(file.choose(), warn = FALSE), 
#     header = TRUE,  
#     sep = "," 
# )

# read HTEP csv files 
read.htep.csv.file<-function(csvfile,supp_vars,verbose=F){ # csvfile<-"./physical/GN-P002-FARANAH-M.wl.csv" #rm(csvfile)
  # read file and convert date
  if (verbose){
    print("-----------------")
    print(csvfile)
  }
  csvdata = read.csv(file = csvfile,header = T,stringsAsFactors = F)  # tail(csvdata)
  csvdata$date=as.POSIXct(as.character(csvdata$Timestamp),tz = "GMT")
  #head(csvdata)
  
  # check on units
  #todo  if(any(csvdata$Type=="DerivedWaterLevel")) {if(!unique(csvdata[which(csvdata$Type=="DerivedWaterLevel"),"Uom"])=="cm") stop("DerivedWaterLevel units are not in cm in CSV files")}
  if(any(csvdata$Type=="WaterLevel")) {if(!unique(csvdata[which(csvdata$Type=="WaterLevel"),"Uom"])=="cm") stop("Water Level units are not in cm in CSV files")}
  if(any(csvdata$Type=="DerivedDischarge")) {if(!unique(csvdata[which(csvdata$Type=="DerivedDischarge"),"Uom"])=="m3/s") stop("DerivedDischarge units are not in m3/s in CSV files")}
  if(any(csvdata$Type=="Discharge")) {if(!unique(csvdata[which(csvdata$Type=="Discharge"),"Uom"])=="m3/s") stop("Discharge units are not in m3/s in CSV files")}  

  # aggregate to daily resolution
  types<-unique(csvdata$Type)
  if (verbose){
    print(types)
  }

  # remove unknowns
  valid_idx<-(types %in% supp_vars)
  types<-types[valid_idx]

  csvdata2<-as.data.frame(matrix(nrow=length(unique(csvdata$date)),ncol=1+length(types)))
  colnames(csvdata2)<-c("Date",types)
    #head(csvdata2)
  csvdata2$Date<-sort(unique(csvdata$date))

  for (i in 1:length(types)) { #i<-2
    mytype<-types[i]
    myrows<-which(csvdata$Type==mytype)
    # csvdata[myrows,]
    myag <- aggregate(csvdata[myrows,"Value"],by=list(csvdata[myrows,"date"]),FUN=mean)
    myag[,2] <- round(myag[,2],1)
    csvdata2[match(myag[,1],csvdata2[,"Date"]),mytype]<-myag[,2]
  }
  #csvdata2
  if (verbose){
    print(csvdata2)
  }
  myname<-unique(csvdata$Name)
  return(list(myname,csvdata2))
}

# TODO: avoid the hard-coded aspects in csvcomp
read.csv.batch<-function(path,debugPublish=F){ #path<-tmpDir
  # list files
  files = dir(path = path,pattern = ".csv")
  
  # create object to store in
  files_htepids<-sapply(strsplit(files,split="\\."),"[",1)

  supported_vars<-c("WaterLevel","DerivedDischarge","Discharge")
  csvcomp<-array(NA,c(11323,length(files),length(supported_vars)),dimnames=list("Date"=as.character(seq(as.Date("2000-01-01"),as.Date("2030-12-31"),by=1)),"Stn"=files_htepids,"Var"=supported_vars))
    # note we hardcoded the dates here, can be a bug if the dates fall outside this range, in that case fix it
    # note we also hardcoded the variables to be 4 and specifically these names, change if it is needed

  if (length(files) > 0){
    # read files 
    for(i in 1:length(files)) { #i<-1
      fname = paste(path,"/",files[i],sep="")

      if (debugPublish) {
        rciop.publish(path=fname, recursive=FALSE, metalink=TRUE)
      }

      mycsv<-read.htep.csv.file(csvfile = fname,supported_vars)
      #mycsv[[1]];head(mycsv[[2]])
      
      mm<-match(as.Date(mycsv[[2]]$Date),as.Date(dimnames(csvcomp)$Date))
      mytypes<-colnames(mycsv[[2]])[-1]
      for(j in 1:length(mytypes)) { #j<-1
        csvcomp[mm,match(mycsv[[1]],dimnames(csvcomp)[["Stn"]]),match(mytypes[j],dimnames(csvcomp)[["Var"]])] <- mycsv[[2]][,mytypes[j]]
      }
    }
    
    # check data
      # which(!is.na(csvcomp),arr.ind=T)
      # csvcomp[7238:7242,,]
    
    # clean NA timesteps
      dateswithdata<-sort(unique(which(!is.na(csvcomp),arr.ind=T)[,"Date"]))
      csvcomp<-csvcomp[dateswithdata,,]
  }else{
    print("INFO: read.csv.batch - no csv files available")
  }

  return(csvcomp)
}

# Read DBF file with stationID and SUBID
read_stations_from_dbf <- function(shapefile_dbf) { # shapefile_dbf<-shapefileDbf
    # File exists at entry

    df_station_dbf = read.dbf(file=shapefile_dbf,as.is=T)
    
    # Remove any ending whitespace characters (new line etc.)
    df_station_dbf$StationId = trimws(df_station_dbf$StationId,which='right')
    
    # Filter/Sort stations
    df_station_dbf = subset(df_station_dbf,StationId != 0)
    if ("USEFULNESS" %in% colnames(df_station_dbf)){
        df_station_dbf = subset(df_station_dbf,USEFULNESS > 0)
    }
    if ("PRIORITY" %in% colnames(df_station_dbf)){
        df_station_dbf = df_station_dbf[order(df_station_dbf$PRIORITY,decreasing=F),]
    }

    idlist<-df_station_dbf[,c("StationId","SUBID")]
    
    return (idlist)
}

# Read Qobs
read_qobs <- function(csv_file)
{
    # File exists at entry

    df_qobs = ReadPTQobs(filename=csv_file) # -9999 replaced by NA, dates as posixct
    colnames(df_qobs)<-toupper(colnames(df_qobs))  # convert to uppercase to simplify stuff later

    last_date = df_qobs[nrow(df_qobs),'DATE']

    next_date = last_date + 86400

    return (list('last_date'=last_date,
                 'next_date'=next_date,
                 'df_qobs'=df_qobs))

}


# Check if GeoData.txt contains rating curve attributes
is_geodata_rc_attr <- function(geodata)
{
    status = FALSE
    if ( ("MRRATCK_NOI" %in% colnames(geodata)) & 
         ("MRRATCP_NOI" %in% colnames(geodata)) &
         ("MRRATCW0" %in% colnames(geodata)) ){
        status = TRUE
    }
    return (status)
}


# Read GeoData  (note if needed it can be moved to the other script with functions)
read_geodata <- function(gdf) { # gfd<-geodataFile
  status = 0 # OK

  gd<-read.table(gdf,header=T,sep = "\t")  # can be optimized if it is slow to read everything...
  colnames(gd)<-toupper(colnames(gd))  # convert to uppercase to simplify stuff later

  if (is_geodata_rc_attr(gd)){
    gd[which(gd[,"MRRATCK_NOI"]==0),"MRRATCK_NOI"] <- NA  # set zero to missing, possibly adapt if it changes to -9999 or whatever...
    gd[which(gd[,"MRRATCP_NOI"]==0),"MRRATCP_NOI"] <- NA  # set zero to missing, possibly adapt if it changes to -9999 or whatever...
    gd[which(gd[,"MRRATCW0"]==0),"MRRATCW0"] <- NA  # set zero to missing, possibly adapt if it changes to -9999 or whatever...    
  }else{
    cmn.log('GeoData.txt does not contain rating curve attributes: MRRATCK_NOI,MRRATCP_NOI,MRRATCW0', logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
  }

  #gd[1:10,1:10]
  return(gd)
}


# Download CSV data for physical stations
# Search data for one station
# Example station_id string: NE-P004-BOSSEY_BANGOU-A
# Requires library('rciop')
# Successful downloads to csv_output_dir
download_data_physical_station <- function(station_id_string,csv_output_dir,filename_save,verbose=F)
{
    if (verbose){
        print(paste0('station_id_string: ',station_id_string))
        print(paste0('csv_output_dir: ',csv_output_dir))
    }

    status = 1 # NOK

    osClientApp = "opensearch-client"
    url = "https://catalog.terradue.com/fanfar/cat/station/search"

    query = paste0("-p 'count=unlimited'"," ","-p 'uid=",station_id_string,"'"," {} | xmllint --format - | grep GetStation")
    opensearchCmd = paste0(osClientApp," '",url,"' ",query)
    #print(opensearchCmd)
    res_station_href_string = system(command = opensearchCmd,intern = T)
    #print(res_station_href_string)
    if (length(res_station_href_string) == 0){
        #print(paste0('No data available for station: ',station_id_string))
        status = 1
        return (status)
    }


    ref_href=""
    if (nchar(res_station_href_string) > 0){
        ref_to_parse = strsplit(res_station_href_string,split='"')
        ref_to_parse = unlist(ref_to_parse)
        ref_href = ref_to_parse[4] # https://-section
    	#print('extracted href string:')
        #print(ref_href)
    }

    if (nchar(ref_href) > 0){
        # Download GetData.csv file
        # Example input from prev step: 'https://recast-dev.terradue.com/t2api/swc/fanfar/v1/stations/st-855cf2cd7813ae88bffcd586ff8f231864995e92'
        # Change to:                    'https://recast-dev.terradue.com/t2api/swc/fanfar/v1/timeseries/ts_st-855cf2cd7813ae88bffcd586ff8f231864995e92_all/GetData.csv'

        # Replace stations with timeseries
        ref_href = sub('stations','timeseries',ref_href)

        # Prefix station identifier st- with ts_
        ref_href = sub('/st-','/ts_st-',ref_href)

        # Suffix href with _all and filename
        ref_href = paste0(ref_href,'_all','/GetData.csv')
        #print('---------------------')
        #print(ref_href)
        #print('---------------------')

        res_copy = rciop.copy(ref_href,target=csv_output_dir,uncompress=F,createOutputDirectory=T)
        #print('res_copy:')
        #print(res_copy)

        if (res_copy$exit.code == 0) {
            # Rename file
            from_file = paste0(csv_output_dir,'/GetData.csv')
            to_file   = paste0(csv_output_dir,'/',filename_save)
            #print(from_file)
            #print(to_file)
            status = 3

            if (file.exists(from_file)){
                file.copy(from=from_file,to=to_file,overwrite=TRUE) # ToDo: Check if last line contains end of line, else write "\n" to file
                file.remove(from_file)
                status = 0 # OK
            }
        }else{
            status = 2
        }
    }

    return (status)
}


# Rating curve function - from Davids 'rating-curve-tools.R'
rating.curve<-function(h=NULL,c,e,b,Q=NULL,opt="forward",listout=F){
  # Jafets interpretation
    # h = water level vector
    # c = coefficient in the rating curve (mrratck_noi or mrratck_ice, depending on ice conditions)
    # e = base reference waterlevel (mrratcw0 in geodata)
    # b = exponent in the rating curve (mrratcp_noi or mrratcp_ice, depending on ice conditions)
    # Q is discharge vector
  if(opt=="forward"){
    if(!is.null(h[1])){
      Q=c*(h-e)^b
      if(listout){
        return(list("Q"=Q,"h"=h,"c"=c,"e"=e,"b"=b))
      }else{
        return(Q)
      }
    }else{
      print("Warning: opt==forward and h==NULL")
      return(NULL)
    }
  }else if(opt=="inverse"){
    if(!is.null(Q[1])){
      #Q=c*(h-e)^b
      h=(Q/c)^(1/b) + e
      if(listout){
        return(list("Q"=Q,"h"=h,"c"=c,"e"=e,"b"=b))
      }else{
        return(h)
      }
    }else{
      print("Warning: opt==inverse and Q==NULL")
      return(NULL)
    }
  }else{
    print("Warning: unknown opt (allowed is forward or inverse)")
    return(NULL)
  }
}


# External function
# Returns a file with comma separated list of subids' if any subid been updated the last n_days, else the text NO_DATA.
# Assumes at least n_days of timesteps in Qobs file
subids_updated <- function(in_file='Qobs.txt',
                           out_file=paste0('/tmp','/subid_updated.txt'),
                           n_days=30,
                           variant=1, # 1 - last n_days from file contents, 2 - last n_days from today
                           test_no_data_found=FALSE)
{
    status = 1 # NOK
    text_not_found = 'NO_DATA'

    if (! file.exists(in_file)){
        # Handle model configuration without Qobs file
        print('No check of updated subids')
        write(text_not_found,file=out_file)
        return (status)
    }
    
    # Parse Qobs.txt csv file
    csv_data = read.csv(file=in_file,header=T,skip=0,sep='\t',stringsAsFactors=F)
    colnames(csv_data) = toupper(colnames(csv_data))

    n_rows = nrow(csv_data)
    if (variant == 1){
        # Last x days based on index
        csv_data_red = csv_data[(n_rows-n_days):n_rows,]
    }else{
        # Last x days from today based on timesteps
        today = Sys.Date()
        format='%Y-%m-%d'
        start_date = as.POSIXct(strptime(today,format=format),tz='GMT') - (n_days*24*60*60)
        start_date_str = strftime(start_date,format=format)
        
        # Last date in file
        end_date_str = csv_data$DATE[n_rows]
        end_date = as.POSIXct(strptime(end_date_str,format=format),tz='GMT')
        
        if (start_date <= end_date){
            # Recent updated data
            csv_data_red = csv_data[which(csv_data$DATE == start_date_str):n_rows,]
        }else{
            # Handle model configuration without updated Qobs file
            print('No recent data in Qobs.txt')
            write(text_not_found,file=out_file)
            return (status)
        }
    }
    rm(csv_data)
    
    if (test_no_data_found){
        # Get only -9999
        csv_data_red = csv_data_red[,sapply(csv_data_red,max) < 0.0]
    }
    
    # Reduce/Filter columns: any value > 0.0
    csv_data_red      = csv_data_red[,sapply(csv_data_red,max) > 0.0]
    csv_data_red_cols = colnames(csv_data_red) # Character vector
    if (length(csv_data_red_cols > 1)){
        # All columns except DATE
        csv_data_red_cols = csv_data_red_cols[csv_data_red_cols!='DATE']
        csv_data_red_cols = gsub('X','',csv_data_red_cols)
        write.table(as.list(csv_data_red_cols),file=out_file,sep=',',row.names=F,col.names=F,quote=F) # All subid on a line
        status = 0 # OK
    }else{
        print('No recent data in Qobs.txt after filtering')
        write(text_not_found,file=out_file)
    }

    return (status)
} #subids_updated


# External function
# Wrapper for updating Qobs with discharge data from physical stations
# Output: When successful, file 'Qobs.txt' updated in dir modelFilesRunDir
process_eo_data_physical <- function(app_sys,             # Reduce global configuration settings (variable app.sys) if needed
                                     qobsFile,            # Path + filename
                                     shapefileDbf,        # Path + filename of shapefile with station id
                                     geodataFile,         # Path + filename of geodata
                                     modelFilesRunDir,    # HYPE model data files dir, output dir
                                     tmpDir,              # For app.sys=="tep", temporary dir to use for download of csv files, created by ciop-copy
                                     localCSVDir=NULL,    # For app.sys!="tep", dir with csv files
                                     enableAnadia=F,      # Enable download of local observations from Anadia and convert to H-TEP format
                                     moduleDbfreadPath=NULL, # Path to python module dbfread
                                     #outputFileSubidUpdated=NULL, # Path + filename of csv file to contain recently updated SUBIDs
                                     debugPublishFiles=F, # Condition to publish files during development
                                     verbose=F)           # More output
{
    if (verbose){
        print(paste0('qobsFile: ',qobsFile))
        print(paste0('shapefileDbf: ',shapefileDbf))
        print(paste0('geodataFile: ',geodataFile))
        print(paste0('modelFilesRunDir: ',modelFilesRunDir))
        print(paste0('tmpDir: ',tmpDir))
        print(paste0('debugPublishFiles: ',debugPublishFiles))
    }

    # Outputs/Internal status
    use_new_qobs = FALSE


    # Check inputs, otherwise return and continue without EO
    if (! file.exists(qobsFile)){
        cmn.log('Qobs.txt missing, continuing without EO', logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
        return ()
    }

    if (! file.exists(shapefileDbf)){
        cmn.log('Shapefile missing, continuing without EO', logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
        return ()
    }

    if (! file.exists(geodataFile)){
        cmn.log('GeoData.txt missing, continuing without EO', logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
        return ()
    }

    # Read Qobs.txt
    qobs_info = read_qobs(csv_file=qobsFile) #TODO: just read directly qobs.init!
    qobs.init<-qobs_info[["df_qobs"]]
    # qobs.init<-qobs.init[1:21000,]  # remove some data to test functionality # tail(qobs.init[,1:10])
    
    # names(qobs_info)
    #tail(qobs_info[["df_qobs"]][,1:10])
    #print(qobs_info$last_date) # 2020-01-12
    #print(qobs_info$next_date)

    # Read and filter stationid subid table
    dbf_df = read_stations_from_dbf(shapefile_dbf=shapefileDbf)
    
    # read geodata file
    geodata <- read_geodata(geodataFile)  #geodata[1:10,1:10]; colnames(geodata)

    # Search and download data for physical stations
    if(app_sys == 'tep'){
        for (id_idx in 1:length(dbf_df$StationId)){
            status = download_data_physical_station(
                        station_id=dbf_df$StationId[id_idx],
                        csv_output_dir=tmpDir,
                        filename_save=paste0(dbf_df$StationId[id_idx],'.csv'),
                        verbose=F)
            if (status == 0){
                cmn.log(paste0(dbf_df$StationId[id_idx],' - Successful download of CSV data'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }else if (status == 1){
                cmn.log(paste0(dbf_df$StationId[id_idx],' - No data available'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }else if (status == 2){
                cmn.log(paste0(dbf_df$StationId[id_idx],' - Failed to download CSV data'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }else if (status == 3){
                cmn.log(paste0(dbf_df$StationId[id_idx],' - CSV file not available'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }else{
                cmn.log(paste0(dbf_df$StationId[id_idx],' - Other error'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }
        }

        if (enableAnadia){
            app_path = Sys.getenv("_CIOP_APPLICATION_PATH")
            command = paste(app_path,'util/python','download_convert_Anadia.py',sep="/")

            if (! file.exists(command)){
                cmn.log(paste0(command,' - file do not exist'), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
            }else{
                # Call external script
                tmpOutputDir=paste(tmpDir,'anadia',sep='/')
                tmpOutputCSVDir=paste(tmpOutputDir,'htep_format',sep='/') # Path to CSV files
                args = paste0('--input-file',' ',shapefileDbf,' ','--output-dir',' ',tmpOutputDir,' ','--dbfread-path',' ',moduleDbfreadPath)
                status = system2(command=command,args=args)
                if (status == 0){
                    # Move csv files from local download dir to common download dir
                    csvFiles = dir(path=tmpOutputCSVDir,pattern=".csv")
                    if (length(csvFiles) > 0){
                        for (f in 1:length(csvFiles)) {
                            file.copy(from=paste(tmpOutputCSVDir,csvFiles[f],sep='/'),to=tmpDir,overwrite=TRUE)
                            cmn.log(paste0("cp ",paste(tmpOutputCSVDir,csvFiles[f],sep='/')," to ",tmpDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_EOP)
                        }
                    }
                }else{
                    cmn.log(paste0('exit status',status), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
                }
            }
        } # enableAnadia

        dir_csv = tmpDir
    }else{
        # Local dir with csv files
        dir_csv = localCSVDir
    }

    # Read all downloaded csv files + aggregate to daily resolution
      physical_data<-read.csv.batch(path=dir_csv,debugPublishFiles)
    
    # Combine data
      # initiate temporary df to store data after qobs.init last date and identify stations to process
      newdates<-seq(as.Date(tail(qobs.init[,"DATE"],n=1L))+1,tail(sort(as.Date(dimnames(physical_data)$Date)),n=1L),by=1)
      tempdf<-as.data.frame(matrix(data=NA,nrow=length(newdates),ncol=ncol(qobs.init)));colnames(tempdf)<-colnames(qobs.init)  # object with new data in qobs format
      tempdf$DATE<-as.POSIXct(as.character(newdates),tz="GMT")
      mm<-match(dimnames(physical_data)[["Date"]],as.character(tempdf$DATE)) # match dates
      htepstn<-dimnames(physical_data)$Stn      

      if (length(htepstn) > 0 & length(dbf_df$StationId) > 0){
        # Loop over htep stations

          for (p in 1:length(dbf_df$StationId)){ #i<-17
            i=match(dbf_df$StationId[p],htepstn)
            if (is.na(i)){
              next # Continue with next station
            }

          # find the right subbasin
            mysubid<-dbf_df[match(htepstn[i],dbf_df$StationId),"SUBID"]
            
            if (! (mysubid %in% colnames(tempdf))){
              cmn.log(paste0('New SUBID, append to Qobs.txt: ',mysubid), logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
              tempdf$NEWSUBID<-rep(NA,length(newdates))
              names(tempdf)[names(tempdf)=="NEWSUBID"]<-mysubid
              qobs.init$NEWSUBID<-rep(NA,nrow(qobs.init))
              names(qobs.init)[names(qobs.init)=="NEWSUBID"]<-mysubid

              # Update objects internal attribute obsid (integer) defined by ReadPTQobs()
              attr(qobs.init, which = "obsid") <- c(attr(qobs.init, which = "obsid"),mysubid)
            }
                
            # insert data directly for variable "Discharge"
              tempdf[mm[which(!is.na(mm))],match(mysubid,colnames(tempdf))] <- physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"Discharge"]
              #plot(tempdf[mm[which(!is.na(mm))],match(mysubid,colnames(tempdf))],type="l")#,ylim=c(0,5000))

            # insert data directly for variable "DerivedDischarge", remove once the WL conversion works 
              # tempdf[mm[which(!is.na(mm))],match(mysubid,colnames(tempdf))] <- physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"]
              # tempdf[mm[which(!is.na(mm))][1:100],match(mysubid,colnames(tempdf))] <- physical_data[which(!is.na(mm))[1:100],match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"] # first rows only
              # plot(physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"],col="red",type="l")
              # summary(physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"])

            # Convert water Level to discharge, and insert, start with this, and then fill with "Discharge" afterwards so that the latter takes precedence
              # conditioned on that the rating curve parameters are set, and that there is some water level data to use
            if(is_geodata_rc_attr(geodata)) {
              if(!is.na(geodata[match(mysubid,geodata$SUBID),"MRRATCK_NOI"]) &
                !is.na(geodata[match(mysubid,geodata$SUBID),"MRRATCP_NOI"]) &
                !is.na(geodata[match(mysubid,geodata$SUBID),"MRRATCW0"]) & 
                !all(is.na(physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"WaterLevel"])) ) {
              
                # first convert water level from cm to meters and move to the reference level
                  thiswl<-physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"WaterLevel"]/100 + geodata[match(mysubid,geodata$SUBID),"MRRATCW0"]
                  # summary(thiswl)
                  # summary(physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"WaterLevel"])
                  # plot(as.POSIXct(as.character(dimnames(physical_data)[["Date"]][which(!is.na(mm))][507:545]),tz="GMT"),physical_data[which(!is.na(mm))[507:545],match(htepstn[i],dimnames(physical_data)[["Stn"]]),"WaterLevel"],type="b",xlab="",ylab="WaterLevel (cm)",main=htepstn[i])
                  
                # apply rating curve to derive discharge
                  thisq<-rating.curve(h=thiswl,
                                      c=geodata[match(mysubid,geodata$SUBID),"MRRATCK_NOI"],
                                      e=geodata[match(mysubid,geodata$SUBID),"MRRATCW0"],
                                      b=geodata[match(mysubid,geodata$SUBID),"MRRATCP_NOI"]
                                      )
                  
                  # plot(as.POSIXct(as.character(dimnames(physical_data)[["Date"]][which(!is.na(mm))]),tz="GMT"),thisq,type="l",main=paste(htepstn[i],"\nSUBID",mysubid),xlab="",ylab="Discharge(m3/s)"); summary(thisq)
                  # lines(as.POSIXct(as.character(dimnames(physical_data)[["Date"]][which(!is.na(mm))]),tz="GMT"),physical_data[which(!is.na(mm)),match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"],col="red")
                  # lines(qobs_info[["df_qobs"]][,"DATE"],qobs_info[["df_qobs"]][,mysub],col="blue",type="l")
                  # abline(h=seq(2000,3000,by=100),col=rgb(0,0,0,0.5))
                  # legend("topright",legend=c("Discharge_rc-smhi","DerivedDischarge_T2"),lwd=1,col=c("black","red"))
                  #   plot(thiswl,thisq) 
                  # plot(qobs.init[,"DATE"],qobs.init[,match(mysubid,colnames(qobs.init))],type="l")
                  # lines(as.POSIXct(as.character(dimnames(physical_data)[["Date"]][which(!is.na(mm))]),tz="GMT"),thisq,col="red")
                  

                # insert the data where there were NAs so far
                  nn<-which(is.na(tempdf[mm[which(!is.na(mm))],match(mysubid,colnames(tempdf))]))  # find rows that still have NA values
                  tempdf[mm[which(!is.na(mm))][nn],match(mysubid,colnames(tempdf))] <- thisq[nn]  # insert the calculated Q only on NA rows
                    # lines(nn,tempdf[mm[which(!is.na(mm))][nn],match(mysubid,colnames(tempdf))],col="blue")
                  rm(thisq,nn,thiswl)
              } # !is.na
            } # else {  # move on to insert DerivedDischarge if it exists but e.g. if rating curve pars are missing
              # nn<-which(is.na(tempdf[mm[which(!is.na(mm))],match(mysubid,colnames(tempdf))]))  # find rows that still have NA values
              # tempdf[mm[which(!is.na(mm))][nn],match(mysubid,colnames(tempdf))] <- physical_data[which(!is.na(mm))[nn],match(htepstn[i],dimnames(physical_data)[["Stn"]]),"DerivedDischarge"]  # insert the derived Q only on NA rows
              # rm(nn)
            # }
            rm(mysubid)

            use_new_qobs = TRUE
          } # end of loop around stations
      }else{
          cmn.log('No physical stations available', logHandle, rciopStatus='INFO', rciopProcess=nameOfSrcFile_EOP)
      }
      
      # put in one new qobs object
        qobs.new<-rbind(qobs.init,tempdf);rm(tempdf)
      
      # clean dates without data and set decimals (can happen e.g. if another variable in physcal_data has values)
        allna<-function(x) {all(is.na(x))}
        qrd<-apply(qobs.new[,-1],1,allna)
        qobs.new<-qobs.new[1:tail(which(!qrd),n=1L),]
        qobs.new[,-1]<-round(qobs.new[,-1],5)  # round to 5 decimal places, as it was in qobs.init before
      
      # some plotting checks 
      #  dbf_df[match(htepstn,dbf_df$StationId),"SUBID"]
      #  mysub<-"203643"
      #  plot(qobs.new[,"DATE"],qobs.new[,mysub],type="l",col="blue",main=dbf_df[match(mysub,dbf_df[,"SUBID"]),"StationId"])  
      #  lines(qobs.init[,"DATE"],qobs.init[,mysub])
      
      
    # TODO: either return with qobs.new, or continue to print the new Qobs.txt file here using qobs.new
    init_qobs_file = paste0(modelFilesRunDir,"/Qobs.txt")
    new_qobs_file  = paste0(modelFilesRunDir,"/Qobs.txt-local-obs")
    
    WritePTQobs(x=qobs.new,filename=new_qobs_file) # hypeapps-hype-utils.R

    if (debugPublishFiles){
        if(app_sys == 'tep'){
            if (file.exists(init_qobs_file)){
                rciop.publish(path=init_qobs_file,recursive=FALSE,metalink=TRUE)
            }
            if (file.exists(new_qobs_file)){
                rciop.publish(path=new_qobs_file,recursive=FALSE,metalink=TRUE)
            }
        }else{
            # Rename file?
            #file.copy(from=,to=)
        }
    }

    # ToDo: logic to handle which Qobs to use - either here or in run.R
    if (use_new_qobs && file.exists(new_qobs_file)){
        # For now - always replace initial Qobs.txt
        file.copy(from=new_qobs_file,to=init_qobs_file,overwrite=T)
        cmn.log(paste0("cp ",new_qobs_file," to ",init_qobs_file), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_EOP)
        file.remove(new_qobs_file)
    }

    # # Check recently updated subids'
    # if (! is.null(outputFileSubidUpdated)){
    #     # Output path+filename defined
    #     st = subids_updated(in_file=init_qobs_file,
    #                         out_file=outputFileSubidUpdated,
    #                         n_days=30,
    #                         variant=2)
    #     if(app_sys == 'tep'){
    #         if (file.exists(outputFileSubidUpdated)){
    #             rciop.publish(path=outputFileSubidUpdated,recursive=FALSE,metalink=TRUE)
    #         }
    #     }
    # }

} # process_eo_data

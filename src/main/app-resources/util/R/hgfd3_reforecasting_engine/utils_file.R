#
# File related utililties
#


check_dir_exist<-function(path,create_dir=F)
{
    status = T

    path_as_charstr = dirname(path) # Strip filename if exists
    if (! dir.exists(path_as_charstr)){
        print(paste0('Info: Dir not exist:',path_as_charstr))
        status = F

        if (create_dir){
            print('Info: Attempting to create dir')
            st = dir.create(path_as_charstr,showWarnings=T,recursive=T,mode="0775")
            if (! st){
                print(paste0('Error: Failed to create dir:',path_as_charstr))
            }else{
                status = T
            }
        }
    }

    return (status)
}


# Start and end for one or max 4 intervals
# write_dateobjects_to_file<-function(part1Start=NULL,part1End=NULL,
#                                     part2Start=NULL,part2End=NULL,
#                                     part3Start=NULL,part3End=NULL,
#                                     part4Start=NULL,part4End=NULL,
#                                     csvFile=NULL,
#                                     verbose=F)
write_dateobjects_to_file<-function(part1Start='',part1End='',
                                    part2Start='',part2End='',
                                    part3Start='',part3End='',
                                    part4Start='',part4End='',
                                    csvFile=NULL,
                                    verbose=F)
{
    if (! is.null(csvFile)){
        filename = csvFile
    }
    else{
        path = getcwd()
        filename = paste(path,'tmp-time-data.csv',sep='/')
    }

    if (verbose){
        print(filename)
    }

    dir_status = check_dir_exist(filename,create_dir=T)

    if (! dir_status){
        print('Error: No write to file')
    }else{
        data = matrix(c(part1Start,part1End,
                        part2Start,part2End,
                        part3Start,part3End,
                        part4Start,part4End),nrow=4,ncol=2,byrow=T)
        # ToDo: error handling
        # fh = open(description=filename,open='w')
        # write.csv(data,file=fh)
        # close(fh)
        write.csv(data,file=filename)
    }
}


# Start and end for one or max 4 intervals
read_dateobjects_from_file<-function(csvFile=NULL,
                                     verbose=F)
{
    part1Start=part1End=NULL
    part2Start=part2End=NULL
    part3Start=part3End=NULL
    part4Start=part4End=NULL

    if (! is.null(csvFile)){
        filename = csvFile
    }
    else{
        path = getcwd()
        filename = paste(path,'tmp-time-data.csv',sep='/')
    }
    
    if (verbose){
        print(filename)
    }

    if (! file.exists(filename)){
        print('Error: No read from file')
    }else{
        # ToDo: error handling, return df instead?
        fh = open(description=filename,open='r')
        read.csv(data,file=fh)
        for (r in 1:nrow(data)) {
            if (r == 1) {
                part1Start = data[r,'V1']
                part1End   = data[r,'V2']
            }else if (r == 2) {
                part2Start = data[r,'V1']
                part2End   = data[r,'V2']
            }else if (r == 3) {
                part3Start = data[r,'V1']
                part3End   = data[r,'V2']
            }else if (r == 4) {
                part4Start = data[r,'V1']
                part4End   = data[r,'V2']
            }
        }
        close(fh)
    }

    return (list('part1Start'=part1Start,'part1End'=part1End,
                 'part2Start'=part2Start,'part2End'=part2End,
                 'part3Start'=part3Start,'part3End'=part3End,
                 'part4Start'=part4Start,'part4End'=part4End))
}


# File contains all necessary info as applications, paths to forcing and run directories etc.
read_configuration_from_file<-function(csvFile=NULL,
                                       verbose=F)
{
    output = NULL
    
    if (verbose){
        print(csvFile)
    }

    if (is.null(csvFile)){
        print('Configuration file will not be used')
        return (output)
    }else{
        filename = csvFile

        if (! file.exists(filename)){
            print('Error: No read from file')
            return (output)
        }else{
            # ToDo: error handling
            config = read.csv(file=filename,header=T,sep=';',stringsAsFactors=F)
            if (nrow(config) > 0){
                # Config data exists
                output = list()

                for (r in 1:nrow(config)){
                    key_as_string     = config[r,'configkey']
                    value_as_string   = config[r,'configstringasvalue']
                    #comment_as_string = config[r,'comment']

                    if (value_as_string == 'TRUE' || value_as_string == 'T'){
                        value = TRUE
                    }else if (value_as_string == 'FALSE' || value_as_string == 'F'){
                        value = FALSE
                    #}else  if (value_as_string == 'NULL'){
                    #    value = NULL
                    }else{
                        value = value_as_string
                    }

                    # Append to list
                    output[[key_as_string]] = value
                }
            }
        }
    }

    return (output)
}


# Extract date from string/url/filename
# Problems with if statements around gsub, thereby three separate functions
extract_date_from_string_ymd<-function(date_string)
{
    resDate = NULL

    # Extract date from string/url/filename
    # .*  - match any character
    # ()  - capturing group with expected match pattern
    # \1 - reference to first captured pattern
    date_str = gsub(pattern='.*([0-9]{8}).*',replace='\\1',date_string)

    if (nchar(date_str) > 0 && nchar(date_str) < 9){
        resDate = date_str
    }

    return (resDate)
}


# Extract date from string/url/filename
extract_date_from_string_ym<-function(date_string)
{
    resDate = NULL

    # Also include '_' to skip dates with e.g. 8 digits
    date_str = gsub(pattern='.*_([0-9]{6})_.*',replace='\\1',date_string)

    if (nchar(date_str) > 0 && nchar(date_str) < 7){
        resDate = date_str
    }

    return (resDate)
}


# Extract date from string/url/filename
extract_date_from_string_ymd_dash<-function(date_string)
{
    resDate = NULL

    date_str = gsub(pattern='.*([0-9]{4}-[0-9]{2}-[0-9]{2}).*',replace='\\1',date_string)

    if (nchar(date_str) > 0 && nchar(date_str) < 11){
        resDate = date_str
    }

    return (resDate)
}


# Locate latest file and its date
latest_filedate<-function(path,file_pattern,ym=F,ymd_dash=F)
{
    #resDate = '19010101'
    resDate = NULL
    filename = NULL

    if (ym){
        # Also include '_' to skip dates with e.g. 8 digits
        sub_pattern  = '.*_([0-9]{6})_.*'
    }else if (ymd_dash){
        sub_pattern  = '.*([0-9]{4}-[0-9]{2}-[0-9]{2}).*'
    }else {
        # ymd
        sub_pattern  = '.*([0-9]{8}).*'
    }

    files = list.files(path=path,pattern=file_pattern)
    dates = grepl(sub_pattern,files) # Indexes that match
    files = files[dates] # Reduce

    if (length(files) > 0) {
        idxs = sort.list(files,decreasing=T) # First index contains latest
        latest_file = files[idxs[1]]

        # Extract date from filename
        if (ym){
            resDate = extract_date_from_string_ym(latest_file)
        }else if (ymd_dash){
            resDate = extract_date_from_string_ymd_dash(latest_file)
        }else {
            resDate = extract_date_from_string_ymd(latest_file)
        }
    }

    return (list('date'=resDate,'filename'=latest_file))
}


# Create symbolic links for filenames (file_list) located in dir file_dir.
# Symbolic links created in dir at path_link_dir.
create_symlink_to_files<-function(file_dir,file_list,path_link_dir=NULL,sym_file_list=NULL,verbose=F)
{
    status = 1 # NOK

    if (! is.null(path_link_dir)){
        if (! dir.exists(path_link_dir)){
            print(paste0('INFO: Creating dir: ',path_link_dir))
            dir.create(path_link_dir,recursive=T,mode='0775') # user,group=rwx,other=rx
        }

        if (length(file_list) > 0){
            for (f in 1:length(file_list)){

                src_file = paste(file_dir,file_list[f],sep='/')
                if (is.null(sym_file_list)){
                    sym_file = paste(path_link_dir,file_list[f],sep='/')
                }else{
                    sym_file = paste(path_link_dir,sym_file_list[f],sep='/')
                }
                
                if (! file.exists(src_file)){
                    print(paste0('INFO: Skipping symlink for file: ',src_file))
                }else{
                    #file_link = paste(path_link_dir,file_list[f],sep='/')

                    #if (! file.exists(file_link)){
                    if (! file.exists(sym_file)){
                        # No existing symlink

                        command = 'ln'
                        #args = paste0('-s',' ',src_file,' ','-t',' ',path_link_dir)
                        args = paste0('-s',' ',src_file,' ',sym_file)
                        if (verbose){
                            print(paste(command,args,sep=' '))
                        }

                        status = system2(command=command,args=args)
                    }
                }
            }
        }
    }

    return (status)
}


# Create symbolic links for filenames with prefix located in dir file_dir.
# Symbolic links created in dir at path_link_dir.
# This function do not check for existing symbolic links.
create_symlink_to_files_with_prefix<-function(file_dir,file_prefix,path_link_dir=NULL,verbose=F)
{
    status = 1 # NOK

    if (! is.null(path_link_dir)){
        if (! dir.exists(path_link_dir)){
            print(paste0('INFO: Creating dir: ',path_link_dir))
            dir.create(path_link_dir,recursive=T,mode='0775')
        }

        if (nchar(file_prefix) > 0){

            file=paste0(file_dir,'/',file_prefix,'*')

            command = 'ln'
            args = paste0('-s',' ',file,' ','-t',' ',path_link_dir)
            if (verbose){
                print(paste(command,args,sep=' '))
            }

            status = system2(command=command,args=args)
        }
    }

    return (status)
}


# Copy files (file_list) located in dir file_dir via cdo command to remove ending time steps.
# Files output to in dir path_link_dir.
copy_nc_files_remove_ending_time_steps<-function(file_dir,file_list,path_link_dir=NULL,dst_file_list=NULL,dateobj_start_date,verbose=F)
{
    status = 1 # NOK

    if (! is.null(path_link_dir)){
        if (! dir.exists(path_link_dir)){
            print(paste0('INFO: Creating dir: ',path_link_dir))
            dir.create(path_link_dir,recursive=T,mode='0775') # user,group=rwx,other=rx
        }

        if (length(file_list) > 0){
            for (f in 1:length(file_list)){

                src_file = paste(file_dir,file_list[f],sep='/')
                if (is.null(dst_file_list)){
                    dst_file = paste(path_link_dir,file_list[f],sep='/')
                }else{
                    dst_file = paste(path_link_dir,dst_file_list[f],sep='/')
                }
                
                if (! file.exists(src_file)){
                    print(paste0('INFO: Skipping symlink for file: ',src_file))
                }else{
                    if (! file.exists(dst_file)){
                        # No existing file

                        command = 'cdo'

                        start_date = dateobj_start_date # format: '2020-03-28'
                        end_date   = dateobj_subtract_days(dateobj_start_date,0,setDayLast=T) # format: '2020-03-28'

                        # cdo version 1.9
                        # cdo delete,startdate=2020-03-28T00:00:00,enddate=2020-03-31T00:00:00 he5tm_202003_tasmin_fanfar_SMHI.nc outfile.nc
                        # time_stamp = 'T00:00:00'
                        #args = paste0('delete,startdate=',start_date,time_stamp,',enddate=',end_date,time_stamp,' ',src_file,' ',dst_file)

                        # cdo version 1.6
                        seq_timestamps = seq.Date(from=start_date,to=end_date,by='day')
                        
                        if (length(seq_timestamps) > 0){
                            first=T
                            for (day in 1:length(seq_timestamps)){
                                dd = strftime(seq_timestamps[day],format='%d')
                                if (first){
                                    list_of_days = paste0(dd)
                                    first = F
                                }else{
                                    list_of_days = paste0(list_of_days,',',dd)
                                }
                            }

                            args = paste0('delete,day=',list_of_days,' ',src_file,' ',dst_file)

                            if (verbose){
                                print(paste(command,args,sep=' '))
                            }

                            status = system2(command=command,args=args)
                            print(status)
                        }
                    }
                }
            }
        }
    }

    return (status)
}

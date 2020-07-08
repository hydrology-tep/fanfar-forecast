#! /opt/anaconda/bin/Rscript
############! /usr/bin/Rscript

# if(app.sys=='tep') {
#     source(paste(Sys.getenv('_CIOP_APPLICATION_PATH'), 'util/R/hgfd3_reforecasting_engine/utils_log.R',sep='/'))
#     source(paste(Sys.getenv('_CIOP_APPLICATION_PATH'), 'util/R/hypeapps-hype-utils.R',sep='/'))
#     #source(paste(Sys.getenv('_CIOP_APPLICATION_PATH'), 'util/R/hypeapps-utils.R',sep='/'))
# }else{
#     source ('hgfd3_reforecasting_engine/utils_log.R')
#     source ('hypeapps-hype-utils.R')
#     #source ('hypeapps-utils.R')
# }

library('rciop')

# Constants
nameOfSrcFile_EOV = '/util/R/process-eo-virtual.R'


# Search data for all virtual stations (since station_ids' are unknown)
# Output: Water level file (csv), log file (.txt) and water mask (.xml)
# Requires library('rciop')
download_data_virtual_stations <- function(output_dir,verbose=F)
{
    if (verbose){
        print(paste0('output_dir: ',output_dir))
    }

    osClientApp = "opensearch-client"
    # All data items that have effectively produced a time series. Link to the time series is included in each item.
    url = "https://catalog.terradue.com/fanfar-00001/cat/dataitem/search"

    query = paste0("-p 'count=unlimited'") # Not ok with enclosure
    query = paste0("-p 'count=50'")

    opensearchCmd = paste0(osClientApp," '",url,"' ",query)
    if (verbose){
        print(opensearchCmd)
    }

    res_all_virtual_hrefs = system(command = opensearchCmd,intern = T)
    if (verbose){
        print(res_all_virtual_hrefs)
    }
    # [1] "https://catalog.terradue.com//fanfar-00001/cat/dataitem/search?format=atom&uid=TD-VS3B_4661_157_21_1941E_101N_V3-UNKNOWN-R"
    # [2] "https://catalog.terradue.com//fanfar-00001/cat/dataitem/search?format=atom&uid=TD-VS3B_4433_314_5_1536E_1141N_V3-UNKNOWN-R"

    if (length(res_all_virtual_hrefs) == 0){
        print('No virtual data available')
    }else{
        # for (h in 1:length(res_all_virtual_hrefs)){
        for (h in 1:50){
            # Extract station_id, mainly to use as filename for output files
            href_parts = strsplit(res_all_virtual_hrefs[h],split="=")
            station_id = href_parts[[1]][3]
            if (verbose){
                print(station_id)
            }

            if (! is.na(station_id)){
                if (nchar(station_id) > 5){
                    opensearchCmd = paste0(osClientApp," '",res_all_virtual_hrefs[h],"'"," enclosure")
                    if (verbose){
                        print(opensearchCmd)
                    }
                    res_href = system(command = opensearchCmd,intern = T)
                    if (verbose){
                        print(res_href)
                    }

                    if (nchar(res_href) > 0){
                        res_file = rciop.copy(paste0(res_href,"/waterlevel.csv"),target=output_dir,createOutputDirectory=T)
                        if (res_file$exit.code == 0) {
                            if (verbose){
                                print(res_file$output)
                            }
                            path_plus_filename <- res_file$output

                            if (file.exists(path_plus_filename)){
                                file.copy(from=path_plus_filename,to=paste0(output_dir,"/",station_id,".csv"))
                                file.remove(path_plus_filename)
                            }
                        }

                        log_file = paste0(output_dir,"/",station_id,".log")
                        res_file = rciop.copy(paste0(res_href,"/log.txt"),target=output_dir,createOutputDirectory=T)
                        if (res_file$exit.code == 0) {
                            if (verbose){
                                print(res_file$output)
                            }
                            path_plus_filename <- res_file$output

                            if (file.exists(path_plus_filename)){
                                file.copy(from=path_plus_filename,to=log_file)
                                file.remove(path_plus_filename)
                            }
                        }

                        # Water mask
                        if (file.exists(log_file)){
                            # Check the first 10 lines in the log file
                            grep_pattern = 'Water mask'
                            res_text = grep(grep_pattern,readLines(con=log_file,n=10),value=T)
                            # Example: "- Water mask: https://catalog.terradue.com//virtual-stations-staging/series/fanfar-current-vs/search?format=atom&uid=ML-VS3B_4469_350_8_007E_1611N_V3-UNKNOWN-R"
                            if (verbose){
                                print(res_text)
                            }

                            if (nchar(res_text) > 0){  
                                # Extract href for water mask
                                wmsk_href_parts = strsplit(res_text," ")
                                wmsk_href = wmsk_href_parts[[1]][4]
                                if (verbose){
                                    print(wmsk_href)
                                }                            

                                if (nchar(wmsk_href) > 0){
                                    # Get data via opensearch
                                    query = paste0(" {} | xmllint --format - > ",output_dir,"/",station_id,".w-msk.xml") # Not compatible for e.g. Windows
                                    opensearchCmd = paste0(osClientApp," '",url,"' ",query)
                                    if (verbose){
                                        print(opensearchCmd)
                                    }

                                    res_status = system(command = opensearchCmd,intern = T)
                                }
                            }
                        }
                        
                    } # if (nchar(res_href)
                } # if (nchar(station_id
            }    # if (! is.na(station_id
        } # for
    }

} # download_data_virtual_stations


# Call test object

print('start')

download_data_virtual_stations(output_dir='/var/tmp/csv_station_virtual',verbose=T)

print('end')

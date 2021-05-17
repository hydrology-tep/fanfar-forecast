
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_time.R",sep="/"))
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_file_htep.R",sep="/"))
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_engine_common.R",sep="/"))
}else{
    source ('reforecast_engine_common.R')
}


run_hindcast_calc<-function(
    app_sys,
    issue_date=NULL,
    hindcast_period_length=NULL,
    hindcast_begin_date=NULL,
    hindcast_end_date=NULL,
    run_mode_reforecast=T,
    run_mode_operational=F,
    run_mode_state_file_creation=F,
    monthly_break_day=6,
    hindcast_window_days=130,
    hype_state_file=NULL,
    output_intervals_to_csv_file=NULL,
    output_hype_dates_to_csv_file=NULL, # Path + filename
    reforecasting_method=1, # 1 -standard, 2 -
    # h-tep
    part1_url=NULL, # open-search
    part1_query=NULL, # open-search
    part2_url=NULL,
    part2_query=NULL,
    hype_state_file_bdate=NULL,
    hype_state_files_path=NULL,
    meteo_variant="HydroGFD 3.0")
{

    # Call function that returns the latest file date for he5 and he5tm
    # Need to be handled differently depending on which system the application
    # runs on. Filenames may also differ between local and tep.
    latest_date_monthly_part1 = NULL
    latest_date_monthly_part2 = NULL
    if (run_mode_operational){
        if (app_sys == 'server'){
            tmpDate = Sys.Date() # run date
            nMonths = 6
            while(nMonths >= 0){
                tmpDate = dateobj_subtract_months(dateobj=tmpDate,months=1)
                subdirs = dateobj_to_string_ymd_list(tmpDate)
                hgfd_dir = paste(hydrogfd3_root,subdirs$year,subdirs$month,sep='/')

                if (is.null(latest_date_monthly_part1)){
                    if (file.exists(paste(hgfd_dir,'hgfd3_e5-gpccm_pr.nc',sep='/'))){ # 'he5_'
                        latest_date_monthly_part1 = paste0(subdirs$year,subdirs$month,'01')
                    }
                }
                if (is.null(latest_date_monthly_part2)){
                    if (file.exists(paste(hgfd_dir,'hgfd3_e5t-gpccf_pr.nc',sep='/'))){ # 'he5tm_'
                        latest_date_monthly_part2 = paste0(subdirs$year,subdirs$month,'01')
                    }
                }
                nMonths = nMonths - 1
            }
        }
        #if (app_sys == 'local'){
        #     res = latest_filedate(path=x,file_pattern='he5_')
        #     if (! is.null(res$resDate)){
        #         latest_date_monthly_part1 = res$resDate
        #     }
        #     res = latest_filedate(path=x,file_pattern='he5tm_')
        #     if (! is.null(res$resDate)){
        #         latest_date_monthly_part2 = res$resDate
        #     }
        #}
        if (app_sys == 'tep'){
            # Both part1 and part2 necessary
            if (! is.null(part1_url) && ! is.null(part1_query) &&
                ! is.null(part2_url) && ! is.null(part2_query)){
                # he5
                res = utils_file_search_and_locate_latest_date(part1_url,part1_query)
                if (res$status == 0) {
                    latest_date_monthly_part1 = res$date
                }
                # he5tm
                res = utils_file_search_and_locate_latest_date(part2_url,part2_query)
                if (res$status == 0) {
                    latest_date_monthly_part2 = res$date
                }
            }
        }
    } # Operational


    # Call function that returns file date for a valid statefile (that matches this configuration).
    # Need to be handled differently depending on which system the application
    # runs on. Filenames also differs between local and tep.
    date_statefile = NULL
    statefile      = NULL
    if (app_sys == 'server' || app_sys == 'local'){
        if (! is.null(hype_state_file)){
            if (file.exists(hype_state_file)){
                # Handle variant with meteo forcing version and bdate in file name
                # ToDo

                if (is.null(date_statefile)){
                    # Standard name: <path>/state_save20170101.txt
                    date_statefile = extract_date_from_string_ymd(hype_state_file)
                    if (! is.null(date_statefile)){
                        statefile = hype_state_file
                    }
                }
            }
        }
    }
    if (app_sys == 'tep'){
        if (! is.null(hype_state_file_bdate) && ! is.null(hype_state_files_path)){
            res = utils_file_check_latest_statefiles_category(filePath=hype_state_files_path,
                                                              meteo=meteo_variant,
                                                              meteoVersion=NULL,
                                                              hindcastStartDate=hype_state_file_bdate)
            if (res$status == 0) {
                date_statefile = res$fileDate
                statefile      = res$fileName
            }
        }
    }

    # Obs date intervals
    intervals = determine_hindcast_intervals(
                    idate=issue_date,
                    hindcast_period_length=hindcast_period_length,
                    hindcast_begin_date=hindcast_begin_date,
                    hindcast_end_date=hindcast_end_date,
                    #hindcastPeriodDays=hindcast_window_days, # default: 130 days
                    date_statefile=date_statefile,
                    run_mode_reforecast=run_mode_reforecast,
                    run_mode_operational=run_mode_operational,
                    latest_date_monthly_part1=latest_date_monthly_part1,
                    latest_date_monthly_part2=latest_date_monthly_part2,
                    run_mode_state_file_creation=run_mode_state_file_creation,
                    monthlybreakday=monthly_break_day, # default: monthday 6
                    output_intervals_to_csv_file=output_intervals_to_csv_file,
                    output_hype_dates_to_csv_file=output_hype_dates_to_csv_file, # Path + filename
                    reforecasting_method=reforecasting_method,
                    verbose=T)
    
    if (! intervals$useStatefile){
        date_statefile = NULL
        statefile      = NULL
    }

    #return (intervals)
    output = list('intervals'=intervals,
                  'statefile_instate_date'=date_statefile,
                  'statefile_instate'=statefile)

    return (output)
}


run_forecast_calc<-function(
    issue_date,
    forecast_begin_date,
    output_intervals_to_csv_file=NULL,
    output_hype_dates_to_csv_file=NULL) # Path + filename
{
    # Obs date intervals
    intervals = determine_forecast_intervals(
                    idate=issue_date,
                    forecast_begin_date=forecast_begin_date,
                    output_intervals_to_csv_file=output_intervals_to_csv_file,
                    output_hype_dates_to_csv_file=output_hype_dates_to_csv_file,
                    verbose=T)

    return (intervals)
}

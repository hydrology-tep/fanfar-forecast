
#
# Hindcast/Forecast engine
# Based on configuration or user selected options
# determine intervals for the different hydrogfd data
# that shall be used to produce P/Tobs.
#
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_time.R",sep="/"))
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_file.R",sep="/"))
}else{
    source('utils_time.R')
    source('utils_file.R')
}

# ToDo:
# In the main part, check that the provided dates for operational
# differs:
# Diff should be 2 months or more, else quit
# If part1 updated/newer, diff 1 month, still use the previous months for part1


set_hype_dates<-function(startDate,
                         endDate,
                         hindcast=T,
                         hindcastPeriodDays=NULL,
                         runModeStatefileCreation=F,
                         verbose=F)
{
    if (! is.null(hindcastPeriodDays)){
        windowHindcastPeriodDays = as.numeric(hindcastPeriodDays)
    }else{
        # Default
        windowHindcastPeriodDays = 130
    }

    if (hindcast){
        if (runModeStatefileCreation){
            # Not supporting "outstatedate"
            bdate = dateobj_to_string(startDate)
            cdate = dateobj_to_string(startDate)
            edate = dateobj_to_string(endDate)

        }else{
            # Standard
            bdate = dateobj_to_string(startDate)
            edate = dateobj_to_string(endDate)

            # cdate = edate - x days
            cdate = dateobj_subtract_days(endDate,
                                          windowHindcastPeriodDays)
            if (cdate < startDate){
                # Limit to bdate
                cdate = startDate
            }
            cdate = dateobj_to_string(cdate)
        }

    }else{
        # Forecast
        bdate = dateobj_to_string(startDate)
        cdate = dateobj_to_string(startDate)
        edate = dateobj_to_string(endDate)
    }

    if (verbose){
        print(paste0('bdate: ',bdate))
        print(paste0('cdate: ',cdate))
        print(paste0('edate: ',edate))
    }

    return (list('bdate'=bdate,
                 'cdate'=cdate,
                 'edate'=edate))
}


# Date parameters shall be of class Date
determine_interval_part1<-function(hindcastStartDate,
                                   hindcastEndDate,
                                   runModeOperational,
                                   operationalEndDate,
                                   runModeReforecast,
                                   runModeStatefileCreation,
                                   monthlybreakday=NULL,
                                   reforecastingMethod=1, # 1 - standard, 2 - he5 until od (not end of month)
                                   verbose=F)
{
    if (verbose){
        print('determine_interval_part1:')
        print(hindcastStartDate)
        print(hindcastEndDate)
        print(runModeOperational)
        print(operationalEndDate)
        print(runModeReforecast)
        print(runModeStatefileCreation)
        print(monthlybreakday)
        print(reforecastingMethod)
    }

    status = 0 # OK

    if (! is.null(monthlybreakday)){
        monthDay = as.numeric(monthlybreakday)
    }else{
        # Default
        monthDay = 6
    }

    startDate = hindcastStartDate

    if (runModeOperational){
        # Use date for the latest available monthly file
        endDate = operationalEndDate
        if (endDate < startDate){
            print(paste('Error: determine_interval_part1(). Not possible to determine a correct interval end date. Check forecast issue date (idate), use a more recent date.','Start:',startDate,'End:',endDate,sep=' '))
            status = 1 # NOK
        }
    }
    
    if (runModeReforecast){
        if (reforecastingMethod == 1){
            hcEndDate = as.POSIXlt(hindcastEndDate)
            if (hcEndDate$mday < monthDay){
                # Newer files not yet available
                endDate = dateobj_subtract_months(hindcastEndDate,4,setDayLast=T)
            }else{
                endDate = dateobj_subtract_months(hindcastEndDate,3,setDayLast=T)
            }
        }else{
            # Variant 2
            endDate = dateobj_subtract_days(hindcastEndDate,5)
        }
    }

    if (runModeStatefileCreation){
        endDate = hindcastEndDate
    }

    return (list('status'=status,
                 'startDate'=startDate,
                 'endDate'=endDate))
}


determine_interval_part2<-function(part1EndDate,
                                   hindcastEndDate,
                                   runModeOperational,
                                   operationalEndDate,
                                   runModeReforecast,
                                   monthlybreakday=NULL,
                                   verbose=F)
{

    if (verbose){
        print('determine_interval_part2:')
        print(part1EndDate)
        print(hindcastEndDate)
        print(runModeOperational)
        print(operationalEndDate)
        print(runModeReforecast)
        print(monthlybreakday)
    }

    status = 0 # OK

    if (! is.null(monthlybreakday)){
        monthDay = as.numeric(monthlybreakday)
    }else{
        # Default
        monthDay = 6
    }

    startDate = dateobj_add_days(part1EndDate,1) # Should be day 1 in next month

    if (runModeOperational){
        endDate = operationalEndDate
        if (endDate < startDate){
            print(paste('Error: determine_interval_part2(). Not possible to determine a correct interval end date. Check forecast issue date (idate), use a more recent date.','Start:',startDate,'End:',endDate,sep=' '))
            status = 2 # NOK
        }
    }
    
    if (runModeReforecast){
        hcEndDate = as.POSIXlt(hindcastEndDate)
        if (hcEndDate$mday < monthDay){
            endDate = dateobj_subtract_months(hindcastEndDate,2,setDayLast=T)
        }else{
            endDate = dateobj_subtract_months(hindcastEndDate,1,setDayLast=T)
        }
    }
    
    return (list('status'=status,
                 'startDate'=startDate,
                 'endDate'=endDate))
}


determine_interval_part3<-function(part2EndDate,
                                   hindcastEndDate,
                                   verbose=F)
{
    if (verbose){
        print('determine_interval_part3:')
        print(part2EndDate)
        print(hindcastEndDate)
    }

    status = 0 # OK

    startDate = dateobj_add_days(part2EndDate,1) # Should be day 1 in next month
    endDate   = dateobj_subtract_days(hindcastEndDate,5)

    if (endDate < startDate){
        # Operational
        print(paste('Error: determine_interval_part3(). Not possible to determine a correct interval end date. Check forecast issue date (idate), use a more recent date.','Start:',startDate,'End:',endDate,sep=' '))
        status = 3 # NOK
    }
 
    return (list('status'=status,
                 'startDate'=startDate,
                 'endDate'=endDate))
}


determine_interval_part4<-function(part3EndDate,
                                   hindcastEndDate,
                                   verbose=F)
{
    if (verbose){
        print('determine_interval_part4:')
        print(part3EndDate)
        print(hindcastEndDate)
    }

    status = 0 # OK

    startDate = dateobj_add_days(part3EndDate,1)
    endDate   = hindcastEndDate

    if (endDate < startDate){
        # Operational
        print(paste('Error: determine_interval_part4(). Not possible to determine a correct interval end date. Check forecast issue date (idate), use a more recent date.','Start:',startDate,'End:',endDate,sep=' '))
        status = 4 # NOK
    }
    
    return (list('status'=status,
                 'startDate'=startDate,
                 'endDate'=endDate))
}


determine_hindcast_intervals<-function(idate=NULL, # Forecast issue date (htep)
                                       hindcast_period_length=NULL, # Days (htep)
                                       hindcast_begin_date=NULL, # Optional: Use instead of idate
                                       hindcast_end_date=NULL, # Optional: Use instead of hindcast_period_length
                                       #hindcastPeriodDays=NULL, # Window/Offset for use of statefile
                                       date_statefile=NULL, # HYPE statefile date
                                       run_mode_reforecast=T,
                                       run_mode_operational=F,
                                       latest_date_monthly_part1=NULL, # For mode operational, use latest end date for he5
                                       latest_date_monthly_part2=NULL, # For mode operational, use latest end date for he5tm 
                                       run_mode_state_file_creation=F, # Reduce hindcast interval to base type he5
                                       monthlybreakday=NULL, # Default monthday 6
                                       output_intervals_to_csv_file=NULL, # Path + filename
                                       output_hype_dates_to_csv_file=NULL, # Path + filename
                                       reforecasting_method=1, # 1 - standard, 2 - only he5 and od
                                       verbose=F)
{
    # Outputs
    hindcastStartDate = NULL
    hindcastEndDate = NULL
    he5StartDate = NULL
    he5EndDate = NULL
    he5tmStartDate = NULL
    he5tmEndDate = NULL
    he5tdStartDate = NULL
    he5tdEndDate = NULL
    odStartDate = NULL
    odEndDate = NULL
    useStatefile = F

    # Constants
    hgfdStartDate = string_to_dateobj('19790201')

    # # ToDo: Either skip this and use 130, or create a new input argument
    # # The same input argument was primarily intended for cdate calculation in other function
    # if (! is.null(hindcastPeriodDays)){
    #     windowHindcastPeriodDays = as.numeric(hindcastPeriodDays)
    # }else{
    # Default
    windowHindcastPeriodDays = 130
    #}

    # Check inputs and convert to date objects
    if (! is.null(idate) && ! is.null(hindcast_period_length)){
        inputEndDate   = string_to_dateobj(idate)
        inputStartDate = dateobj_subtract_days(inputEndDate,
                                               hindcast_period_length,
                                               setDayFirst=T)

    }else if (! is.null(hindcast_begin_date) && ! is.null(hindcast_end_date)){
        inputEndDate   = string_to_dateobj(hindcast_end_date)
        inputStartDate = string_to_dateobj(hindcast_begin_date)

    }else{
        print('Error: No time intervals specified, or too many specified')
        q(save='no',status=1)
    }

    if (inputStartDate < hgfdStartDate){
        # Limit
        print(paste0('Info: Limiting start date to: ',hgfdStartDate))
        inputStartDate = hgfdStartDate
    }

    # Check run mode
    runModeOperational = (run_mode_operational &&
                          ! is.null(latest_date_monthly_part1) &&
                          ! is.null(latest_date_monthly_part2))
    runModeReforecast = run_mode_reforecast
    runModeStatefileCreation = run_mode_state_file_creation

    operationalEndDatePart1 = hgfdStartDate # Only to assign value to variable for upcoming function calls
    operationalEndDatePart2 = hgfdStartDate
    if (runModeStatefileCreation){
        # For time calculations only, this is a third run type.
        runModeOperational = F
        runModeReforecast  = F

    } else if (runModeOperational){
        operationalEndDatePart1 = dateobj_set_monthday_last(string_to_dateobj(latest_date_monthly_part1))
        operationalEndDatePart2 = dateobj_set_monthday_last(string_to_dateobj(latest_date_monthly_part2))
        runModeReforecast        = F
        runModeStatefileCreation = F

    }else if (runModeReforecast){
        runModeOperational       = F
        runModeStatefileCreation = F

    }else{
        print('Error: Unsupported run mode')
        q(save='no',status=1)
    }


    #
    # Determine total hindcast interval
    #

    # Start date
    hindcastStartDate = inputStartDate

    # State file
    if (! is.null(date_statefile)){
        inputDateStatefile = string_to_dateobj(date_statefile)

        limitDateUseStatefile = dateobj_add_days(inputDateStatefile,
                                                 windowHindcastPeriodDays)
        
        # Use when idate (end date) is more recent than window upper limit
        # and hindcast start date preceeds lower limit (date statefile)
        useStatefileDate = limitDateUseStatefile < inputEndDate
        
        if ((inputStartDate <= inputDateStatefile) && useStatefileDate){
            hindcastStartDate = inputDateStatefile
            useStatefile      = T
        }else{
            print(paste0('Info: Not using available state file with date: ',date_statefile,' Requires either a longer hindcast period or a forecast issue date > ',limitDateUseStatefile))
        }
    }

    # End date
    if (runModeStatefileCreation){
        # HYPE adaption - one day longer for run type 'state file creation'
        hindcastEndDate = inputEndDate
    }else{
        hindcastEndDate = dateobj_subtract_days(inputEndDate,1)
    }


    #
    # Determine internal hindcast intervals for the different data types
    #
    status_dates = 0 # OK, Overall status, instead of quiting in sub-functions

    output = determine_interval_part1(hindcastStartDate,
                                      hindcastEndDate,
                                      runModeOperational,
                                      operationalEndDatePart1,
                                      runModeReforecast,
                                      runModeStatefileCreation,
                                      monthlybreakday,
                                      reforecasting_method)
    status_dates = status_dates + output$status
    he5StartDate = output$startDate
    he5EndDate   = output$endDate

    if (! runModeStatefileCreation){
        if (reforecasting_method == 1){
                output = determine_interval_part2(he5EndDate,
                                                  hindcastEndDate,
                                                  runModeOperational,
                                                  operationalEndDatePart2,
                                                  runModeReforecast,
                                                  monthlybreakday)
                status_dates   = status_dates + output$status
                he5tmStartDate = output$startDate
                he5tmEndDate   = output$endDate


                output = determine_interval_part3(he5tmEndDate,
                                                  hindcastEndDate)
                status_dates   = status_dates + output$status
                he5tdStartDate = output$startDate
                he5tdEndDate   = output$endDate


                output = determine_interval_part4(he5tdEndDate,
                                                  hindcastEndDate)
                status_dates = status_dates + output$status
                odStartDate  = output$startDate
                odEndDate    = output$endDate
        }else{
            # Variant 2
            output = determine_interval_part4(he5EndDate,
                                              hindcastEndDate)
            status_dates = status_dates + output$status
            odStartDate  = output$startDate
            odEndDate    = output$endDate
        }
    } # ! runModeStatefileCreation

    if (! is.null(output_intervals_to_csv_file)){
        # write_dateobjects_to_file(he5StartDate,he5EndDate,
        #                           he5tmStartDate,he5tmEndDate,
        #                           he5tdStartDate,he5tdEndDate,
        #                           odStartDate,odEndDate,
        if (reforecasting_method == 1){
            write_dateobjects_to_file(dateobj_to_string(he5StartDate),dateobj_to_string(he5EndDate),
                                     dateobj_to_string(he5tmStartDate),dateobj_to_string(he5tmEndDate),
                                     dateobj_to_string(he5tdStartDate),dateobj_to_string(he5tdEndDate),
                                     dateobj_to_string(odStartDate),dateobj_to_string(odEndDate),
                                     csvFile=output_intervals_to_csv_file)
        }else{
            # Variant 2
            write_dateobjects_to_file(part1Start=dateobj_to_string(he5StartDate),part1End=dateobj_to_string(he5EndDate),
                                      part4Start=dateobj_to_string(odStartDate),part4End=dateobj_to_string(odEndDate),
                                      csvFile=output_intervals_to_csv_file)
    }
    }

    # Output bdate, cdate and edate
    res = set_hype_dates(hindcastStartDate,
                         hindcastEndDate,
                         hindcast=T,
                         hindcastPeriodDays=windowHindcastPeriodDays,
                         runModeStatefileCreation=runModeStatefileCreation)


    if (! is.null(output_hype_dates_to_csv_file)){
        write_dateobjects_to_file(part1Start=res$bdate,
                                  part2Start=res$cdate,
                                  part3Start=res$edate,
                                  csvFile=output_hype_dates_to_csv_file)
    }

    if (verbose || status_dates != 0){
        print(paste('hindcast interval:',hindcastStartDate,'->',hindcastEndDate,sep=' '))
        print(paste('he5:              ',he5StartDate,'->',he5EndDate,sep=' '))
        print(paste('he5tm:            ',he5tmStartDate,'->',he5tmEndDate,sep=' '))
        print(paste('he5td:            ',he5tdStartDate,'->',he5tdEndDate,sep=' '))
        print(paste('od:               ',odStartDate,'->',odEndDate,sep=' '))
    }

    if (status_dates != 0){
        print(paste('Error: determine_hindcast_intervals(), error code:',status_dates,'Aborting, not possible to determine correct hindcast intervals. For run mode Operational, use a more recent forecast issue date (idate).',sep=' '))
        print('Info: For run mode Operational, the latest available monthly file(s) determines end date for the hindcast period.')
        q(save='no',status=1)
    }

    return (list('hindcastStartDate'=hindcastStartDate,'hindcastEndDate'=hindcastEndDate,
                 'he5StartDate'=he5StartDate,'he5EndDate'=he5EndDate,
                 'he5tmStartDate'=he5tmStartDate,'he5tmEndDate'=he5tmEndDate,
                 'he5tdStartDate'=he5tdStartDate,'he5tdEndDate'=he5tdEndDate,
                 'odStartDate'=odStartDate,'odEndDate'=odEndDate,
                 'bdate'=res$bdate,'cdate'=res$cdate,'edate'=res$edate,
                 'useStatefile'=useStatefile))
} # determine_hindcast_intervals


determine_forecast_intervals<-function(idate=NULL, # Forecast issue date
                                       forecast_begin_date=NULL, # Optional: Use instead of idate
                                       output_intervals_to_csv_file=NULL, # Path + filename
                                       output_hype_dates_to_csv_file=NULL, # Path + filename
                                       verbose=F)
{
    # Outputs
    forecastStartDate = NULL
    forecastEndDate   = NULL

    # Constants
    forecastPeriodDays = 9

    # Input to date object
    if (! is.null(idate) && is.null(forecast_begin_date)){
        forecastStartDate = string_to_dateobj(idate)
    }

    else if (is.null(idate) && ! is.null(forecast_begin_date)){
        forecastStartDate = string_to_dateobj(forecast_begin_date)
    
    }else{
        print('Error: Not able to use the provided inputs')
        q(save='no',status=1)
    }

    forecastEndDate = dateobj_add_days(forecastStartDate,forecastPeriodDays)

    if (! is.null(output_intervals_to_csv_file)){
        #write_dateobjects_to_file(forecastStartDate,forecastEndDate,
        write_dateobjects_to_file(dateobj_to_string(forecastStartDate),dateobj_to_string(forecastEndDate),
                                  csvFile=output_intervals_to_csv_file,
                                  verbose=verbose)
    }

    # Output bdate, cdate and edate
    res = set_hype_dates(forecastStartDate,
                         forecastEndDate,
                         hindcast=F,
                         hindcastPeriodDays=NULL)

    if (! is.null(output_hype_dates_to_csv_file)){
        write_dateobjects_to_file(part1Start=res$bdate,
                                  part2Start=res$cdate,
                                  part3Start=res$edate,
                                  csvFile=output_hype_dates_to_csv_file)
    }

    if (verbose){
        print(paste('forecast interval (odf):',forecastStartDate,'->',forecastEndDate,sep=' '))
    }

    return (list('forecastStartDate'=forecastStartDate,
                 'forecastEndDate'=forecastEndDate,
                 'bdate'=res$bdate,'cdate'=res$cdate,'edate'=res$edate))
} # determine_forecast_intervals



# Replace global variables default values by values from configuration file
value_from_config_file<-function(current_value,key,config_list,verbose=T)
{

    check_conf<-function(config_string){
        output = config_string
        if (output == 'NULL'){
            output = NULL
        }
        return (output)
    }

    value = current_value

    if (exists(key,where=config_list)){
        config_value = check_conf(config_list[[key]])
        
        if (! is.null(config_value)){
            value = config_value
        }
    }

    if (verbose){
        print(value)
    }

    return (value)
}

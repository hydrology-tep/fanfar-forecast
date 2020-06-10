#
# Date/Time related utililties
#


string_ymd_to_string<-function(datestr)
{
    str=as.character(datestr)
    if (nchar(str) > 8){
        # Remove '-'
        res_str = paste(substr(str,1,4),substr(str,6,7),substr(str,9,10),sep="")
    }else{
        #res_str=paste(substr(str,1,4),substr(str,5,6),substr(str,7,8),sep="")
        res_str = str
    }

    return (res_str)
}


ymd_to_dateobj<-function(year,month,day)
{
    dateobj = ISOdate(as.numeric(year),as.numeric(month),as.numeric(day))

    return (dateobj)
}


string_to_dateobj<-function(datestr,ym=F,ymd_dash=F)
{
    str = datestr
    format='%Y%m%d'

    if (ym){
        str = paste(str,'01')
    }else if (ymd_dash){
        format='%Y-%m-%d'
    }

    dateobj = as.Date(str,format=format)
    #dateobj = as.POSIXlt(dateobj)

    return (dateobj)
}


dateobj_to_string<-function(dateobj,ym=F,ymd_dash=F)
{
    if (is.null(dateobj)){
        return (NULL)
    }

    if (ym){
        format='%Y%m'
    }else if (ymd_dash){
        format='%Y-%m-%d'
    }else {
        # ymd
        format='%Y%m%d'
    }

    # From base class date to string
    return (strftime(dateobj,format=format))
}


dateobj_to_string_ymd_list<-function(dateobj)
{
    if (is.null(dateobj)){
        return (NULL)
    }

    ymd = list('year'=strftime(dateobj,format='%Y'),
               'month'=strftime(dateobj,format='%m'),
               'day'=strftime(dateobj,format='%d'))
    return (ymd)
}


# Return TRUE if leap year
leap_year<-function(year)
{
  # Not vector based logical operators
  leap = ifelse( (year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0, TRUE, FALSE)
  return (leap)
}


# Return number of days in requested month
# in  : year  - to determine leap year
# in  : month - 1 to 12
# out : days 28 to 31
last_day_in_month<-function(year,month)
{
  month_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  is_leap_year = leap_year(year)
  if (is_leap_year) {
    month_days[2] = 29
  }
  day = month_days[month]
  return (day)
}


# in  : of type date
# out : still of base class date
dateobj_set_monthday_first<-function(dateobj,verbose=F)
{
    do = as.POSIXlt(dateobj)
    do$mday = 1

    do = as.Date(do)
    
    if (verbose){
        print(dateobj)
        print(do)
    }

    return (do)
}


dateobj_set_monthday_last<-function(dateobj,verbose=F)
{
    year_as_char = strftime(dateobj,format='%Y')
    year_as_num  = as.numeric(year_as_char)
    mon_as_char  = strftime(dateobj,format='%m')
    mon_as_num   = as.numeric(mon_as_char)

    do = as.POSIXlt(dateobj)
    do$mday = last_day_in_month(year_as_num,mon_as_num)

    do = as.Date(do)
    
    if (verbose){
        print(dateobj)
        print(do)
    }

    return (do)
}


dateobj_subtract_days<-function(dateobj,days,setDayFirst=F,setDayLast=F,verbose=F)
{
    do = as.POSIXlt(dateobj)
    do$mday = do$mday - as.numeric(days)

    if (setDayFirst && ! setDayLast){
        # Restore change to base class, else R seems still to operate on the original object
        do = as.Date(do)
        do = dateobj_set_monthday_first(do)
    }
    else if (! setDayFirst && setDayLast){
        do = as.Date(do)
        do = dateobj_set_monthday_last(do)
    }

    do = as.Date(do)
    
    if (verbose){
        print(dateobj)
        print(days)
        print(do)
    }

    return (do)
}


dateobj_add_days<-function(dateobj,days,setDayFirst=F,setDayLast=F,verbose=F)
{
    do = dateobj_subtract_days(dateobj,-days,setDayFirst,setDayLast,verbose)

    if (verbose){
        print(dateobj)
        print(days)
        print(do)
    }

    return (do)
}


dateobj_subtract_months<-function(dateobj,months,setDayFirst=F,setDayLast=F,verbose=F)
{
    # Handle case for mday 29-31, where resulting month may rollover to month+1
    mday_as_char = strftime(dateobj,format='%d')
    mday_as_num  = as.numeric(mday_as_char)

    do = as.POSIXlt(dateobj)
    do$mday = 15

    do$mon = do$mon - as.numeric(months)

    if (setDayFirst && ! setDayLast){
        do = as.Date(do)
        do = dateobj_set_monthday_first(do)
    }
    else if (! setDayFirst && setDayLast){
        do = as.Date(do)
        do = dateobj_set_monthday_last(do)
    }
    else{
        # Check mday for resulting month
        year_as_char = strftime(do,format='%Y')
        year_as_num  = as.numeric(year_as_char)
        mon_as_char  = strftime(do,format='%m')
        mon_as_num   = as.numeric(mon_as_char)
        l_day = last_day_in_month(year_as_num,mon_as_num)
        if (mday_as_num > l_day) {
            print('Info: adjusting mday for resulting month')
            do$mday = l_day
        }
        else {
            do$mday = mday_as_num
        }
    }

    do = as.Date(do)
    
    if (verbose){
        print(dateobj)
        print(months)
        print(do)
    }

    return (do)
}


dateobj_add_months<-function(dateobj,months,setDayFirst=F,setDayLast=F,verbose=F)
{
    do = dateobj_subtract_months(dateobj,-months,setDayFirst,setDayLast,verbose)

    if (verbose){
        print(dateobj)
        print(months)
        print(do)
    }

    return (do)
}

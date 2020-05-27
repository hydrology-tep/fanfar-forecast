
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_file.R",sep="/"))
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_time.R",sep="/"))
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_engine_netcdf_to_obs_wrapper.R",sep="/"))
}else{
    source ('utils_file.R')
    source ('utils_time.R')
    source ('reforecast_engine_netcdf_to_obs_wrapper.R')
}


#
# Hindcast section
#
run_hindcast_netcdf_to_obs<-function(
    application_root,
    application,
    app_sys,
    #
    hgfd_nc_dir_monthly=NULL, # app_sys=server
    hgfd_nc_dir_daily=NULL,   # app_sys=server
    hgfd_nc_dir_user=NULL,    # app_sys=local
    hgfd_nc_dir_htep=NULL,    # app_sys=tep
    hgfd_nc_subdir=F,         # TRUE - Variable name (pr/tas/tasmin/tasmax) as sub directory
    grid_shape_file_dir,
    grid_elevation_file,
    subid_shape_file,
    grid_link_file,
    output_dir,
    tmp_dir, # app_sys=server => symbolic links to files
    #
    reforecasting_method=1, # 1 - standard, 2 - he5 until od (not end of month)
    run_mode_state_file_creation=F,
    dateobj_hindcast_startdate=NULL,
    dateobj_hindcast_enddate=NULL,
    dateobj_he5_startdate=NULL,
    dateobj_he5_enddate=NULL,
    dateobj_he5tm_startdate=NULL,
    dateobj_he5tm_enddate=NULL,
    dateobj_he5td_startdate=NULL,
    dateobj_he5td_enddate=NULL,
    dateobj_od_startdate=NULL,
    dateobj_od_enddate=NULL
    )
{
    #if (TRUE){
        print(paste0("application_root: ",application_root))
        print(paste0("application: ",application))
        print(paste0("app_sys: ",app_sys))
        print(paste0("hgfd_nc_dir_htep: ",hgfd_nc_dir_htep))
        print(paste0("grid_shape_file_dir: ",grid_shape_file_dir))
        print(paste0("grid_elevation_file: ",grid_elevation_file))
        print(paste0("subid_shape_file: ",subid_shape_file))
        print(paste0("grid_link_file: ",grid_link_file))
        print(paste0("output_dir: ",output_dir))
        print(paste0("tmp_dir: ",tmp_dir))
        print(paste0("reforecasting_method: ",reforecasting_method))
        print(paste0("run_mode_state_file_creation: ",run_mode_state_file_creation))
        print(paste0("dateobj_hindcast_startdate: ",dateobj_hindcast_startdate))
        print(paste0("dateobj_hindcast_enddate: ",dateobj_hindcast_enddate))
        print(paste0("dateobj_he5_startdate: ",dateobj_he5_startdate))
        print(paste0("dateobj_he5_enddate: ",dateobj_he5_enddate))
        print(paste0("dateobj_he5tm_startdate: ",dateobj_he5tm_startdate))
        print(paste0("dateobj_he5tm_enddate: ",dateobj_he5tm_enddate))
        print(paste0("dateobj_he5td_startdate: ",dateobj_he5td_startdate))
        print(paste0("dateobj_he5td_enddate: ",dateobj_he5td_enddate))
        print(paste0("dateobj_od_startdate: ",dateobj_od_startdate))
        print(paste0("dateobj_od_enddate: ",dateobj_od_enddate))
    #}

    status_netcdf_2_obs = 1 # NOK

    if (app_sys == 'server'){
        # Setup symbolic links (not hard links) in dir tmp_nc_dir_hindcast to hgfd3 files
        # by iterating the different hindcast intervals.

        tmp_nc_dir = paste(tmp_dir,'symlinks_hc',sep='/')

        # Clean any previous symlinks
        if (dir.exists(tmp_nc_dir)){
            unlink(tmp_nc_dir,recursive=T,force=F)
        }

        # he5
        he5_until_201612_org = c('hgfd3_e5-gpcch_pr_yyyymm.nc','hgfd3_e5-cru_tas_yyyymm.nc','hgfd3_e5-cru_tasmin_yyyymm.nc','hgfd3_e5-cru_tasmax_yyyymm.nc')
        he5_from_201701_org = c('hgfd3_e5-gpccm_pr_yyyymm.nc','hgfd3_e5-cpc_tas_yyyymm.nc','hgfd3_e5-cpc_tasmin_yyyymm.nc','hgfd3_e5-cpc_tasmax_yyyymm.nc') 
        he5_repl = c('he5_yyyymm_pr.nc','he5_yyyymm_tas.nc','he5_yyyymm_tasmin.nc','he5_yyyymm_tasmax.nc')
        if (! is.null(dateobj_he5_startdate) && ! is.null(dateobj_he5_enddate)){
            copy_file = F
            symLinkDate = dateobj_subtract_months(dateobj_he5_startdate,0,setDayLast=T)
            while (symLinkDate <= dateobj_he5_enddate){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_monthly,subdirs$year,subdirs$month,sep='/')

                if (as.numeric(subdirs$year) < 2017){
                    srcFile=gsub('_yyyymm','',he5_until_201612_org)
                }else{
                    srcFile=gsub('_yyyymm','',he5_from_201701_org)
                }
                symFile=gsub('yyyymm',paste0(subdirs$year,subdirs$month),he5_repl)

                create_symlink_to_files(
                    file_dir=hgfd_dir,
                    file_list=srcFile,
                    path_link_dir=tmp_nc_dir,
                    sym_file_list=symFile)

                copy_file = (symLinkDate != dateobj_he5_enddate)
                symLinkDate = dateobj_add_months(dateobj=symLinkDate,months=1,setDayLast=T)
            }
            # Since changes for reforecasting_method 2, changing from start date (yyyymm01) < end date (yyyymm(31)),compared to 'he5tm' below, to (yyyymm(31) <= (yyyymm(xx))
            # handle the last month
            if (reforecasting_method == 2 && copy_file){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_monthly,subdirs$year,subdirs$month,sep='/')

                if (as.numeric(subdirs$year) < 2017){
                    srcFile=gsub('_yyyymm','',he5_until_201612_org)
                }else{
                    srcFile=gsub('_yyyymm','',he5_from_201701_org)
                }
                symFile=gsub('yyyymm',paste0(subdirs$year,subdirs$month),he5_repl)

                if (symLinkDate > dateobj_he5_enddate){
                    copy_nc_files_remove_ending_time_steps(
                        file_dir=hgfd_dir,
                        file_list=srcFile,
                        path_link_dir=tmp_nc_dir,
                        dst_file_list=symFile,
                        dateobj_start_date=dateobj_add_days(dateobj=dateobj_he5_enddate,days=1),
                        verbose=T)
                }
            }else if (run_mode_state_file_creation){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_monthly,subdirs$year,subdirs$month,sep='/')

                if (as.numeric(subdirs$year) < 2017){
                    srcFile=gsub('_yyyymm','',he5_until_201612_org)
                }else{
                    srcFile=gsub('_yyyymm','',he5_from_201701_org)
                }
                symFile=gsub('yyyymm',paste0(subdirs$year,subdirs$month),he5_repl)

                create_symlink_to_files(
                    file_dir=hgfd_dir,
                    file_list=srcFile,
                    path_link_dir=tmp_nc_dir,
                    sym_file_list=symFile)
            }
        }

        # he5tm (tm-trailing monthly)
        he5tm_org = c('hgfd3_e5t-gpccf_pr_yyyymm.nc','hgfd3_e5t-cpc_tas_yyyymm.nc','hgfd3_e5t-cpc_tasmin_yyyymm.nc','hgfd3_e5t-cpc_tasmax_yyyymm.nc')
        he5tm_repl = c('he5tm_yyyymm_pr.nc','he5tm_yyyymm_tas.nc','he5tm_yyyymm_tasmin.nc','he5tm_yyyymm_tasmax.nc')
        if (! is.null(dateobj_he5tm_startdate) && ! is.null(dateobj_he5tm_enddate)){
            symLinkDate = dateobj_he5tm_startdate
            while (symLinkDate < dateobj_he5tm_enddate){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_monthly,subdirs$year,subdirs$month,sep='/')

                srcFile=gsub('_yyyymm','',he5tm_org)
                symFile=gsub('yyyymm',paste0(subdirs$year,subdirs$month),he5tm_repl)
                create_symlink_to_files(
                    file_dir=hgfd_dir,
                    file_list=srcFile,
                    path_link_dir=tmp_nc_dir,
                    sym_file_list=symFile)
                symLinkDate = dateobj_add_months(dateobj=symLinkDate,months=1)
            }
        }

        # he5td (td-trailing daily)
        he5td_org = c('hgfd3_e5t-cpc_pr_yyyymmdd.nc','hgfd3_e5t-cpc_tas_yyyymmdd.nc','hgfd3_e5t-cpc_tasmin_yyyymmdd.nc','hgfd3_e5t-cpc_tasmax_yyyymmdd.nc')
        he5td_repl = c('he5td_yyyymmdd_pr.nc','he5td_yyyymmdd_tas.nc','he5td_yyyymmdd_tasmin.nc','he5td_yyyymmdd_tasmax.nc')
        if (! is.null(dateobj_he5td_startdate) && ! is.null(dateobj_he5td_enddate)){
            symLinkDate = dateobj_he5td_startdate
            while (symLinkDate <= dateobj_he5td_enddate){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_daily,subdirs$year,subdirs$month,sep='/')

                srcFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),he5td_org)
                symFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),he5td_repl)
                create_symlink_to_files(
                    file_dir=hgfd_dir,
                    file_list=srcFile,
                    path_link_dir=tmp_nc_dir,
                    sym_file_list=symFile)
                symLinkDate = dateobj_add_days(dateobj=symLinkDate,days=1)
            }
        }

        # od
        od_org = c('basedata_od_pr_yyyymmdd.nc','basedata_od_tas_yyyymmdd.nc','basedata_od_tasmin_yyyymmdd.nc','basedata_od_tasmax_yyyymmdd.nc')
        od_repl = c('od_yyyymmdd_pr.nc','od_yyyymmdd_tas.nc','od_yyyymmdd_tasmin.nc','od_yyyymmdd_tasmax.nc')
        if (! is.null(dateobj_od_startdate) && ! is.null(dateobj_od_enddate)){
            symLinkDate = dateobj_od_startdate
            while (symLinkDate <= dateobj_od_enddate){
                subdirs = dateobj_to_string_ymd_list(symLinkDate)
                hgfd_dir = paste(hgfd_nc_dir_daily,subdirs$year,subdirs$month,sep='/')

                srcFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),od_org)
                symFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),od_repl)
                create_symlink_to_files(
                    file_dir=hgfd_dir,
                    file_list=srcFile,
                    path_link_dir=tmp_nc_dir,
                    sym_file_list=symFile)
                symLinkDate = dateobj_add_days(dateobj=symLinkDate,days=1)
            }
        }

        nc_dir = tmp_nc_dir

    }else if (app_sys == 'tep'){

        if (reforecasting_method == 2){
            # For he5, remove ending time steps
            utils_file_htep_remove_ending_time_steps_he5(
                file_dir=hgfd_nc_dir_htep,
                var_as_subdir=hgfd_nc_subdir,
                dateobj_end_date=dateobj_he5_enddate,
                verbose=T,
                debug_publish=T)
        }

        nc_dir = hgfd_nc_dir_htep

    }else {
        # local
        # Similar as for server above, but all files are located in one dir.
        nc_dir = hgfd_nc_dir_user
    }

    # Call wrapper for netcdf_to_obs
    status_netcdf_2_obs = netcdf_to_obs_wrapper(
        pathToNetcdfToObs=application_root,
        pathToNetcdfToObsRScript=application,
        currentSystem=app_sys,
        workDir=tmp_dir,
        netcdfRootDir=nc_dir,
        isNetcdfSubDir=hgfd_nc_subdir,
        resourceDir=grid_shape_file_dir,
        gridElevFile=grid_elevation_file,
        shapeFile=subid_shape_file,
        gridLinkFile=grid_link_file,
        startDate=dateobj_hindcast_startdate,
        endDate=dateobj_hindcast_enddate,
        outputDir=output_dir,
        debugPublishFiles=run_mode_state_file_creation,
        verbose=T)

    return (status_netcdf_2_obs)

} # Hindcast



#
# Forecast section
#
run_forecast_netcdf_to_obs<-function(
    application_root,
    application,
    app_sys,
    #
    hgfd_nc_dir_monthly=NULL,
    hgfd_nc_dir_daily=NULL,
    hgfd_nc_dir_user=NULL, # app_sys=local
    hgfd_nc_dir_htep=NULL, # app_sys=tep
    hgfd_nc_subdir=F,      # TRUE - Variable name (pr/tas/tasmin/tasmax) as sub directory
    grid_shape_file_dir,
    grid_elevation_file,
    subid_shape_file,
    grid_link_file,
    output_dir,
    tmp_dir, # app_sys=server => symbolic links to files
    #
    dateobj_forecast_startdate=NULL,
    dateobj_forecast_enddate=NULL
    )
{

    status_netcdf_2_obs = 1 # NOK

    if (app_sys == 'server'){
        # Setup symbolic links (not hard links) in dir tmp_nc_dir_forecast to hgfd3 files
        # by iterating the different forecast intervals.

        tmp_nc_dir = paste(tmp_dir,'symlinks_fc',sep='/')

        # Clean any previous symlinks
        if (dir.exists(tmp_nc_dir)){
            unlink(tmp_nc_dir,recursive=T,force=F)
        }

        # odf
        odf_org = c('basedata_odf_pr_yyyymmdd.nc','basedata_odf_tas_yyyymmdd.nc','basedata_odf_tasmin_yyyymmdd.nc','basedata_odf_tasmax_yyyymmdd.nc')
        odf_repl = c('odf_yyyymmdd_pr.nc','odf_yyyymmdd_tas.nc','odf_yyyymmdd_tasmin.nc','odf_yyyymmdd_tasmax.nc')
        symLinkDate = dateobj_forecast_startdate
        subdirs = dateobj_to_string_ymd_list(symLinkDate)
        hgfd_dir = paste(hgfd_nc_dir_daily,subdirs$year,subdirs$month,sep='/')

        srcFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),odf_org)
        symFile=gsub('yyyymmdd',dateobj_to_string(symLinkDate),odf_repl)
        create_symlink_to_files(
            file_dir=hgfd_dir,
            file_list=srcFile,
            path_link_dir=tmp_nc_dir,
            sym_file_list=symFile)

        nc_dir = tmp_nc_dir

    }else if (app_sys == 'tep'){
        nc_dir = hgfd_nc_dir_htep

    }else {
        # local
        # Similar as for server above, but all files are located in one dir.
        nc_dir = hgfd_nc_dir_user
    }

    # Call wrapper for netcdf_to_obs
    status_netcdf_2_obs = netcdf_to_obs_wrapper(
        pathToNetcdfToObs=application_root,
        pathToNetcdfToObsRScript=application,
        currentSystem=app_sys,
        workDir=tmp_dir,
        netcdfRootDir=nc_dir,
        isNetcdfSubDir=hgfd_nc_subdir,
        resourceDir=grid_shape_file_dir,
        gridElevFile=grid_elevation_file,
        shapeFile=subid_shape_file,
        gridLinkFile=grid_link_file,
        startDate=dateobj_forecast_startdate,
        endDate=dateobj_forecast_enddate,
        outputDir=output_dir,
        debugPublishFiles=F,
        verbose=T)

    return (status_netcdf_2_obs)

} # Forecast

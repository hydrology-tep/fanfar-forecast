
#
# Test different variant of indata
#
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,tas]' -p 'start=2019-03-01' -p 'stop=2019-03-01' enclosure")
# With unlimited and no time stamp we receive tas_od-daily_20190301_fanfar_SMHI.nc and tas_od-daily_20190228_fanfar_SMHI.nc - partial ok

opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,tas]' -p 'start=2019-03-01T00:00:01' -p 'stop=2019-03-01T23:59:59' enclosure")
# With unlimited and time stamp we receive tas_od-daily_20190301_fanfar_SMHI.nc only - ok

# Check all variables for daily-od with count 4
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,daily,od]' -p 'start=2019-03-01T00:00:01' -p 'stop=2019-03-01T23:59:59' enclosure")
# ok

# Check all variables for daily-od with count, cat and different date/time intervals (timestamp to not get the previous day in results)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,daily,od]' -p 'start=2019-04-15T00:00:01' -p 'stop=2019-04-15T23:59:59' enclosure") # ok, files for 20190415 (pr,tas,tasmin,tasmax)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,pr]' -p 'start=2019-04-01T00:00:01' -p 'stop=2019-04-01T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,tasmin]' -p 'start=2019-04-02T00:00:01' -p 'stop=2019-04-02T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,tasmax]' -p 'start=2019-04-03T00:00:01' -p 'stop=2019-04-03T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,od,tas]' -p 'start=2019-04-05T00:00:01' -p 'stop=2019-04-05T23:59:59' enclosure") # 2019-04-04 did not exist at smhi # 05 ok

# Check all variables for ecoper with count, cat and different date/time intervals (timestamp to not get the previous day in results)
# cat=daily maybe only intended for gfd files in dir od-daily...
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, nothing
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, everything 2017..2019
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,daily,ecoper]' -p 'start=2019-05-20' -p 'stop=2019-05-20' enclosure") # nok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ecoper]' -p 'start=2019-05-20' -p 'stop=2019-05-20' enclosure") # nok, everything 2017..2019
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ecoper]' -p 'start=2019052000' -p 'stop=2019052000' enclosure") # filename style, nok, everything 2017..2019
# Try ecoper without SMHI and operational, ignore daily, only intended for od-daily)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, everything 2017..2019
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[operational,ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, everything 2017..2019
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, everything 2017..2019
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[ecoper]' -p 'start=2019-05-20T00:00:01' -p 'stop=2019-05-20T23:59:59' enclosure") # nok, everything 2017..2019

# Check all variables for hydrogfdod with count, cat and different date/time intervals
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdod]' -p 'start=2019-06-13T00:00:01' -p 'stop=2019-06-13T23:59:59' enclosure") # ok, files for 201906 (pr,tas,tasmin,tasmax)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdod]' -p 'start=2019-07-01T00:00:01' -p 'stop=2019-07-31T23:59:59' enclosure") # ok, files for 201907 (pr,tas,tasmin,tasmax)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdod,pr]' -p 'start=2018-03-01T00:00:01' -p 'stop=2018-03-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdod,tas]' -p 'start=2018-02-11T00:00:01' -p 'stop=2018-02-11T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdod,tasmin]' -p 'start=2017-01-11T00:00:01' -p 'stop=2017-01-11T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdod,tasmax]' -p 'start=2016-11-20T00:00:01' -p 'stop=2016-11-20T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,monthly,hydrogfdod,tasmax]' -p 'start=2016-11-20T00:00:01' -p 'stop=2016-11-20T23:59:59' enclosure") # ok without cat=monthly

# Check all variables for hydrogfdei with count, cat and different date/time intervals
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdei]' -p 'start=2019-02-07T00:00:01' -p 'stop=2019-02-07T23:59:59' enclosure") # ok, files for 201902 (pr,tas,tasmin,tasmax)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdei]' -p 'start=2019-01-01T00:00:01' -p 'stop=2019-01-01T23:59:59' enclosure") # ok, files for 201901 (pr,tas,tasmin,tasmax)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,pr]' -p 'start=2018-01-01T00:00:01' -p 'stop=2018-01-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tas]' -p 'start=2017-01-01T00:00:01' -p 'stop=2017-01-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tasmin]' -p 'start=2017-11-01T00:00:01' -p 'stop=2017-11-30T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tasmax]' -p 'start=2016-12-01T00:00:01' -p 'stop=2016-12-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,monthly,hydrogfdei,tasmax]' -p 'start=2016-12-01T00:00:01' -p 'stop=2016-12-31T23:59:59' enclosure") # ok without cat=monthly

# Check all variables for ei-monthly with count, cat and different date/time intervals
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=2' -p 'cat=[SMHI,operational,ei,monthly]' -p 'start=1997-01-01T00:00:01' -p 'stop=1997-01-31T23:59:59' enclosure") # ok, files for 199701 (pr,tas)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=2' -p 'cat=[SMHI,operational,ei,monthly]' -p 'start=1998-02' -p 'stop=1998-02' enclosure") # ok, files for 199802 (pr,tas)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ei,monthly,pr]' -p 'start=2004-03-01T00:00:01' -p 'stop=2004-03-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,ei,monthly,tas]' -p 'start=2007-11-01T00:00:01' -p 'stop=2007-11-30T23:59:59' enclosure") # ok

# Check all variables for od-monthly with count, cat and different date/time intervals
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=2' -p 'cat=[SMHI,operational,od,monthly]' -p 'start=2016-08-01T00:00:01' -p 'stop=2016-08-31T23:59:59' enclosure") # ok, files for 201608 (pr,tas)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=2' -p 'cat=[SMHI,operational,od,monthly]' -p 'start=2017-10' -p 'stop=2017-10' enclosure") # ok, files for 201710 (pr,tas)
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,od,monthly,pr]' -p 'start=2017-12-01T00:00:01' -p 'stop=2017-12-31T23:59:59' enclosure") # ok
opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,od,monthly,tas]' -p 'start=2019-08-01T00:00:01' -p 'stop=2019-08-31T23:59:59' enclosure") # ok


#
# Search
#
input_enclosure <- system(command = opensearchCmd,intern = T)
# Print
message(input_enclosure)

if (length(input_enclosure >= 1)) {
    nc_tmpdir <- paste(TMPDIR,"nc-files",sep="/")
    if (! dir.exists(nc_tmpdir)) { dir.create(nc_tmpdir) }
    for (url in 1:length(input_enclosure)) {
        rciop.copy(input_enclosure[url],nc_tmpdir)
    }
}
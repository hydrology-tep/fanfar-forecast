#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os
import os.path
import sys
import urllib.request
#from dbfread import DBF
import subprocess
from datetime import datetime
import time

# Victor Näslund <victor.naslund@smhi.se> 20200810
# Script that convert file from Anadia dataformat to HTEP format
# Libraries: pip3 install --user dbfread

# NOTE: We assume all stations have timezone UTC+2  (Info from Leandro 30 Sep 2020: "our stations use UTC + 1 time zone." --> IS THIS A BUG BELOW?

script_ver="v4"
# 20200910 Tobias: v2, H-TEP run-time adaptions, Changed Depth to WaterLevel
# 20210121 Tobias: v3, Niamey added.
# 20210630 Tobias: v4, Duplicates.


def search_subid_stationid_linkage_file(subid_stationid_file,name_or_id):
    table = DBF(subid_stationid_file, load=True, encoding='utf-8')
    
    for row in range(0, len(table.records)):
        if name_or_id.lower().strip() == table.records[row]["StationNam"].lower().strip():
            return table.records[row]
        if name_or_id.lower().strip() in table.records[row]["StationId"].lower().strip():
            return table.records[row]
            
    print ("Could not find StationNam or StationId for " + name_or_id)
    return None
    
def parse_data(url, name, subid_stationid_file, year):

    row = search_subid_stationid_linkage_file(subid_stationid_file,name)
    name = row["StationId"]
    if row is None:
        return 1

    lat = str(row["Latitude"])
    lon = str(row["Longitude"])
    UoM = ["cm", "m3/s"]
    datatype = ["WaterLevel", "Discharge"]

    counter = 0
    
    if year == 2019:
        outfile_J = open("./htep_format/" + name + ".csv_J", "w")
        outfile_Q = open("./htep_format/" + name + ".csv_Q", "w")
        outfile = [outfile_J, outfile_Q]
        outfile_J.write("Id,Name,Timestamp,Latitude,Longitude,Uom,Value,Type\n")
    else:
        with open("./htep_format/" + name + ".csv_J", "r") as f:
            for line in f:
                counter += 1
        counter -= 1
        outfile_J = open("./htep_format/" + name + ".csv_J", "a")
        outfile_Q = open("./htep_format/" + name + ".csv_Q", "a")
        outfile = [outfile_J, outfile_Q]

        
    for x in range(0, len(datatype)):
        # download data, url contains station and year, latest time step first
        with urllib.request.urlopen(url) as response:
            data = response.read().decode('utf-8')
            #ToDo: Write data to dir original, remove use of curl
        
            for line in reversed(data.split("\n")):

                if len(line) < 5:
                    continue
                
                if "date" in line or "Date" in line:
                    continue

                # date, hour, depth, Q
                date = line.split(",")[0]    # 2020-01-08
                hour = line.split(",")[1]    # 07:00
                value = line.split(",")[2+x] # -999.0, 0.0 or float value

                # if not str(year) in date:
                #     print("INFO: Downloaded data contains incorrect year",date)
                #     continue

                if int(float(value)) == -999:
                    # Skip line for water level and discharge
                    continue

                # if x == 0 and value == "0.0":
                #     # Skip line for water level
                #     continue
                
                # if x == 1 and value == "0.0":
                #     # Skip line for discharge
                #     continue

                #Id,Name,Timestamp,Latitude,Longitude,Uom,Value,Type
                outfile[x].write(str(counter) + "," + name + "," + date + "T" + hour + ":00.0000000+01:00" + "," + lat + "," + lon + "," + UoM[x] + "," + value + "," + datatype[x] + "\n")
                
                counter += 1
    return 0

def combine_datatypes(subid_stationid_linkage_file, url):
    row = search_subid_stationid_linkage_file(subid_stationid_linkage_file,url)
    name = row["StationId"]

    counter = -1
    with open("./htep_format/" + name + ".csv_J") as f:
        for line in f:
            counter += 1
    
    with open("./htep_format/" + name + ".csv_Q") as f:
        with open("./htep_format/" + name + ".csv_Q2", "w") as w:
            for line in f:
                line = line.strip()
                w.write(str(counter) + line[line.find(","):] + "\n")
                counter += 1
            
    subprocess.call("mv " + "./htep_format/" + name + ".csv_J " + "./htep_format/" + name + ".csv", shell=True)
    subprocess.call("cat " + "./htep_format/" + name + ".csv_Q2 >> " + "./htep_format/" + name + ".csv", shell=True)
    subprocess.call("rm -rf " + "./htep_format/" + name + ".csv_Q", shell=True)
    subprocess.call("rm -rf " + "./htep_format/" + name + ".csv_Q2", shell=True)

# Check for duplicate timestamps per data type, keep the last found
# Update existing file in dir htep_format
# Assumes csv file timestamps are ordered
# Sequential ids' are not updated after removal of duplicates
def check_remove_duplicate_timestamps(subid_stationid_linkage_file, url,verbose=False):
    row = search_subid_stationid_linkage_file(subid_stationid_linkage_file,url)
    stn_id = row["StationId"]

    import csv

    curr_file="htep_format/" + stn_id + ".csv"
    if not os.path.isfile(curr_file):
        print("file missing, abort",curr_file)
        return(1)
        
    data_in = []
    with open(curr_file,"r",) as csv_file:
        reader = csv.reader(csv_file,delimiter=',')
        for row in reader:
            data_in.append(row)
            # data_in[0]
            #     ['Id', 'Name', 'Timestamp', 'Latitude', 'Longitude', 'Uom', 'Value', 'Type']
            # data_in[1]
            #     ['0', 'NE-P001-NIAMEY-A', '2020-01-01T00:00:00.0000000+01:00', '13.502', '2.1038', 'cm', '-999.0', 'WaterLevel']
            # data_in[-1]
            #     ['21721', 'NE-P001-NIAMEY-A', '2021-06-23T00:00:00.0000000+01:00', '13.502', '2.1038', 'm3/s', '-999.0', 'Discharge']

    if len(data_in) > 0:
        # Rename file extension, to not trigger other codes pattern match for .csv
        org_file="htep_format/" + "filtered-" + stn_id + ".txt"
        with open(org_file,"w",) as csv_file:
            writer = csv.writer(csv_file,delimiter=',')
            writer.writerows(data_in)

    data_out = data_in
    
    col_timestamp = 2
    col_value = 6
    col_type = 7
    last_row = len(data_in)
    #data_in[0][col_timestamp]
    #data_in[0][col_value]
    #data_in[0][col_type]

    list_row_indexes_to_remove = []
    if last_row > 0:
        for r1 in range(0,last_row - 1):
            for r2 in range(r1,last_row):
                if r1 != r2:
                    if data_in[r1][col_timestamp] == data_in[r2][col_timestamp]:
                        if data_in[r1][col_type] == data_in[r2][col_type]:
                            if verbose:
                                print(r1,r2,data_in[r1][col_timestamp],data_in[r1][col_type],data_in[r1][col_value],data_in[r2][col_value])
                                print('remove row from data_out',r1,data_in[r1])
                                print('keep row from data_out',r2,data_in[r2])
                            list_row_indexes_to_remove.append(r1)
                            #check remaining    break # for r2
   
    if len(list_row_indexes_to_remove) > 0:
        row_indexes_removed = 0
        for r1 in range(0, len(list_row_indexes_to_remove)):
            if verbose:
                print("remove",r1, list_row_indexes_to_remove[r1],len(data_out))
            del data_out[list_row_indexes_to_remove[r1] - row_indexes_removed]
            row_indexes_removed += 1

        if len(data_out) > 0:
            with open(curr_file,"w",) as csv_file:
                writer = csv.writer(csv_file,delimiter=',')
                writer.writerows(data_out)
    
    return(0)


def clean():
    now = time.time()

    for f in os.listdir("./original"):
        if os.stat("./original/" + f).st_mtime < now - 7 * 86400:
            if "01.tar" not in f:
                os.remove(os.path.join("./original/", f))

def main(subid_stationid_linkage_file,output_dir,backup_raw_data=True,check_duplicates=True):
    # Main function

    # If not exists, create output dir and sub-dirs
    if not os.path.isfile(subid_stationid_linkage_file):
        print("File not found")
        exit(1)

    original    = os.path.join(output_dir,"original")
    htep_format = os.path.join(output_dir,"htep_format")
    if not os.path.exists(original):
        print("Creating dir: ",original)
        os.makedirs(original,exist_ok=True)
    if not os.path.exists(htep_format):
        print("Creating dir: ",htep_format)
        os.makedirs(htep_format,exist_ok=True)

    os.chdir(output_dir)

    clean()

    subprocess.check_call("rm -rf htep_format/*", shell=True)

    urls = {}
    urls["Garbey Kourou"]    = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/1/GarbeyKourou/"
    urls["Bossey Bangou"]    = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/2/BosseyBangou/"
    #urls["Larba Touloumbo"]  = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/3/LarbaTouloumbo/"
    #urls["Larba Birno"]      = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/4/LarbaBirno/"
    #urls["Garbey Kourou"]    = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/5/GarbeyKourou/" # Garbey Kourou [1.607889,13.738068]
    #urls["Talle"]            = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/6/Talle/" # TallÃ© / Tallé
    #urls["Toure"]            = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/7/Toure/" # TourÃ© / Touré
    urls["Niamey"]           = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/8/Niamey/"

    # + datetime.today().strftime('%Y-%m-%d') 

    if backup_raw_data:
        for url in urls:
            subprocess.check_call("rm -rf original/" + urls[url].split("/")[-2] + ".txt", shell=True)
            for year in range(2019, (datetime.now().year)+1):
                # The download of data via curl is not used when parsing data and creating output file in htep format
                subprocess.call("curl " + "'" + urls[url] + str(year) + "' > original/" + urls[url].split("/")[-2] + "_" + str(year) + ".txt", shell=True) # Use separate yearly files instead of append
        subprocess.check_call("tar cf original/data_" + datetime.today().strftime('%Y-%m-%d') + ".tar original/*.txt", shell=True)
    
    failed_input_files = []
    for url in urls:
        for year in range(2019, (datetime.now().year)+1):
            print(url,year)
            # Download the same raw data via python urllib, create or append yearly data to separate waterlevel and discharge files for each station
            ret = parse_data(urls[url] + str(year), url, subid_stationid_linkage_file, year)
            time.sleep(2)
            
            if ret != 0:
                failed_input_files.append(urls[url])
                break

        # Combine the separate yearly waterlevel and discharge files in dir htep_format
        combine_datatypes(subid_stationid_linkage_file, url)
        if check_duplicates:
            check_remove_duplicate_timestamps(subid_stationid_linkage_file, url)
                         
    if len(failed_input_files) > 0:
        # Save failed_convertions into a file
        with open ("./htep_format/failed_convertions.txt", "w") as w:
            w.write("# Unable to convert these files, most likely the subid stationid link was missing\n")
            for x in failed_input_files:
                w.write(x + "\n")

if __name__ == "__main__":

    run_env = os.getenv('_CIOP_APPLICATION_PATH',default='local')
    print('version',script_ver)
    print(run_env)
    
    if run_env == 'local':
        from dbfread import DBF
        subid_stationid_linkage_file = "/data/proj9/Fouh/Global/Projekt/FANFAR/Model/WestAfricaHYPE_v1.0/Gauges&Subids/SUBID-StationID-linkage_WWH_1.3.7_20200813.dbf"
        output_dir                   = "/data/proj9/Fouh/Global/Projekt/FANFAR/Data/Sirba-Anadia/dataFROMAnadia/auto_down"
        backup_raw_data_to_dir_original = True # Separate download via curl to backup downloaded data in original format
    else:
        # H-TEP
        import argparse

        parser = argparse.ArgumentParser()
        parser.add_argument("-i", "--input-file", help="Path+filename to file containing linkage between subid and stationid")
        parser.add_argument("-o", "--output-dir", help="Path+dirname to put csv files")
        parser.add_argument("-d", "--dbfread-path", help="Path to module dbfread")

        subid_stationid_linkage_file = None
        output_dir = None
        module_dbfread_path = None
        args = parser.parse_args()
        if args.input_file:
            subid_stationid_linkage_file = args.input_file

        if args.output_dir:
            output_dir = args.output_dir

        if args.dbfread_path:
            module_dbfread_path = args.dbfread_path

        backup_raw_data_to_dir_original = False
        
        if subid_stationid_linkage_file == None or output_dir == None or module_dbfread_path == None:
            print("Path to subid stationid linkage file missing or output dir or module dbfread not defined")
            print("./download_convert_Anadia.py -i <path+filename> -o <path + name of output dir>")
            exit(1)

        #print('INFO: Using python3 with module dbfread from appended path: /opt/anaconda/pkgs/dbfread-2.0.7-py_0/site-packages')
        #sys.path.append('/opt/anaconda/pkgs/dbfread-2.0.7-py_0/site-packages')
        print('INFO: Using python3 with module dbfread from appended path: ',module_dbfread_path)
        sys.path.append(module_dbfread_path)
        from dbfread import DBF

    main(subid_stationid_linkage_file,output_dir,backup_raw_data_to_dir_original)

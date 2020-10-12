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

# NOTE: We assume all stations have timezone UTC+2

# 20200910 Tobias: v2, H-TEP run-time adaptions, Changed Depth to WaterLevel


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
    datatype = ["WaterLevel", "Discharge"] #"Depth"

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
        with urllib.request.urlopen(url) as response:
            data = response.read().decode('utf-8')
        
            for line in reversed(data.split("\n")):

                if len(line) < 5:
                    continue
                
                if "date" in line or "Date" in line:
                    continue

                #Id,Name,Timestamp,Latitude,Longitude,Uom,Value,Type
                # date, hour, depth, battery 
                date = line.split(",")[0]
                hour = line.split(",")[1]
                value = line.split(",")[2+x]

                if int(float(value)) == -999:
                    continue

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


def clean():
    now = time.time()

    for f in os.listdir("./original"):
        if os.stat("./original/" + f).st_mtime < now - 7 * 86400:
            if "01.tar" not in f:
                os.remove(os.path.join("./original/", f))

def main(subid_stationid_linkage_file,output_dir):
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
    urls["Garbey Kourou"] = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/1/GarbeyKourou/"
    urls["Bossey Bangou"] = "http://slapis.fi.ibimet.cnr.it:8080/SlapisWS/api/data/j_get_stations_data/csv/2/BosseyBangou/"

    # + datetime.today().strftime('%Y-%m-%d') 

    failed_input_files = []
    for url in urls:
        subprocess.check_call("rm -rf original/" + urls[url].split("/")[-2] + ".txt", shell=True)
        for year in range(2019, (datetime.now().year)+1):
            subprocess.call("curl " + "'" + urls[url] + str(year) + "' >> original/" + urls[url].split("/")[-2] + ".txt", shell=True) 
            ret = parse_data(urls[url] + str(year), url, subid_stationid_linkage_file, year)
            time.sleep(2)
            
            if ret != 0:
                failed_input_files.append(urls[url])
                break

        combine_datatypes(subid_stationid_linkage_file, url)
                         
    subprocess.check_call("tar cf original/data_" + datetime.today().strftime('%Y-%m-%d') + ".tar original/*.txt", shell=True)

    if len(failed_input_files) > 0:
        # Save failed_convertions into a file
        with open ("./htep_format/failed_convertions.txt", "w") as w:
            w.write("# Unable to convert these files, most likely the subid stationid link was missing\n")
            for x in failed_input_files:
                w.write(x + "\n")

if __name__ == "__main__":

    run_env = os.getenv('_CIOP_APPLICATION_PATH',default='local')
    print(run_env)
    if run_env == 'local':
        from dbfread import DBF
        subid_stationid_linkage_file = "/data/proj9/Fouh/Global/Projekt/FANFAR/Model/WestAfricaHYPE_v1.0/Gauges&Subids/SUBID-StationID-linkage_WWH_1.3.7_20200813.dbf"
        output_dir                   = "/data/proj9/Fouh/Global/Projekt/FANFAR/Data/Sirba-Anadia/dataFROMAnadia/auto_down"
        #output_dir                   = "/var/tmp/Data/Sirba-Anadia/dataFROMAnadia/auto_down"

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

        if subid_stationid_linkage_file == None or output_dir == None or module_dbfread_path == None:
            print("Path to subid stationid linkage file missing or output dir or module dbfread not defined")
            print("./download_convert_Anadia.py -i <path+filename> -o <path + name of output dir>")
            exit(1)

        #print('INFO: Using python3 with module dbfread from appended path: /opt/anaconda/pkgs/dbfread-2.0.7-py_0/site-packages')
        #sys.path.append('/opt/anaconda/pkgs/dbfread-2.0.7-py_0/site-packages')
        print('INFO: Using python3 with module dbfread from appended path: ',module_dbfread_path)
        sys.path.append(module_dbfread_path)
        from dbfread import DBF

    main(subid_stationid_linkage_file,output_dir)

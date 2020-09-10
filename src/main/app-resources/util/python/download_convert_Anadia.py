#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os
import os.path
import sys
import urllib.request
from dbfread import DBF
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
    
def parse_data(url, name, subid_stationid_file):

    row = search_subid_stationid_linkage_file(subid_stationid_file,name)
    name = row["StationId"]
    if row is None:
        return 1

    lat = str(row["Latitude"])
    lon = str(row["Longitude"])
    UoM = "cm"
    datatype = "WaterLevel" #"Depth"

    outfile = open("./htep_format/" + name + ".csv", "w")
    outfile.write("Id,Name,Timestamp,Latitude,Longitude,Uom,Value,Type\n")
    with urllib.request.urlopen(url) as response:
        data = response.read().decode('utf-8')
        
        counter = 0
        for line in data.split("\n"):

            if len(line) < 5:
                continue
                
            if "date" in line or "Date" in line:
                continue

            #Id,Name,Timestamp,Latitude,Longitude,Uom,Value,Type
            # date, hour, depth, battery 
            date = line.split(",")[0]
            hour = line.split(",")[1]
            value = line.split(",")[2]            

            outfile.write(str(counter) + "," + name + "," + date + "T" + hour + ":00.0000000+02:00" + "," + lat + "," + lon + "," + UoM + "," + value + "," + datatype + "\n")
            
            counter += 1
    return 0

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
    urls["Garbey Kourou"] = "http://slapis.fi.ibimet.cnr.it:8080/slapisws/api/data/j_get_stations_alldata_csv/1"
    urls["Bossey Bangou"] = "http://slapis.fi.ibimet.cnr.it:8080/slapisws/api/data/j_get_stations_alldata_csv/2"

    # + datetime.today().strftime('%Y-%m-%d') 

    failed_input_files = []
    for url in urls:
        subprocess.call("curl " + "'" + urls[url] + "' > original/" + urls[url].split("/")[-1] + ".txt", shell=True) 
        ret = parse_data(urls[url], url, subid_stationid_linkage_file)

        if ret != 0:
            failed_input_files.append(urls[url])

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
        subid_stationid_linkage_file = "/data/proj9/Fouh/Global/Projekt/FANFAR/Model/WestAfricaHYPE_v1.0/Gauges&Subids/SUBID-StationID-linkage_WWH_1.3.7_20200617.dbf"
        output_dir                   = "/data/proj9/Fouh/Global/Projekt/FANFAR/Data/Sirba-Anadia/dataFROMAnadia/auto_down"
        #output_dir                   = "/var/tmp/Data/Sirba-Anadia/dataFROMAnadia/auto_down"

    else:
        # H-TEP
        import argparse
        parser = argparse.ArgumentParser()
        parser.add_argument("-i", "--input-file", help="Path+filename to file containing linkage between subid and stationid")
        parser.add_argument("-o", "--output-dir", help="Path+dirname to put csv files")

        subid_stationid_linkage_file = None
        output_dir = None
        args = parser.parse_args()
        if args.input_file:
            subid_stationid_linkage_file = args.input_file

        if args.output_dir:
            output_dir = args.output_dir

        if subid_stationid_linkage_file == None or output_dir == None:
            print("Path to subid stationid linkage file missing or output dir not defined")
            print("./download_convert_Anadia.py -i <path+filename> -o <path + name of output dir>")
            exit(1)

    main(subid_stationid_linkage_file,output_dir)

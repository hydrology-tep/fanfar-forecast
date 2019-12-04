#! /bin/bash

local_path=~/R
local_path_r_packages=${local_path}/x86_64-redhat-linux-gnu-library

echo ""
echo "Install additional R packages at..."
echo "${local_path_r_packages}"

if [ ! -d ${local_path_r_packages} ]; then
    #already_installed=1
    mkdir -p ${local_path_r_packages}
fi

# opensearch to retrieve zip-file including HYPEtools tar file...
# tmp
#cp /workspace/HYPEtools_0.4-6.tar.gz ${local_path}/

already_installed=0
if [ -d ${local_path_r_packages}/HYPEtools/libs ]; then
    already_installed=1
    echo "Already installed..."
    #exit 0
fi

if [ ${already_installed} -eq 0  ]; then
    # Dependencies for HYPEtools
    # Problem extracting value from bash variables
    
    #list_of_packages='c(     )'
    Rscript -e 'install.packages(c("pbapply","hydroGOF","cleangeo","rgdal","rgeos","ncdf4","raster","maptools","foreign","sp","data.table","rciop"), repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'

    #conda Rscript -e 'install.packages("sp", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #Rscript -e 'install.packages("pbapply", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #Rscript -e 'install.packages("hydroGOF", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #conda Rscript -e 'install.packages("data.table", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'

    # Unpack and install HYPEtools
    pushd ${local_path}
    tar -xvzf HYPEtools_0.4-6.tar.gz # Unpacked to dir HYPEtools

    #Rscript -e 'install.packages(pkgs="~/R/HYPEtools", repos=NULL, lib="~/R/x86_64-redhat-linux-gnu-library")'

    if [ -d HYPEtools ]; then
	rm -rf HYPEtools
    fi

    # Other R packages
    #Rscript -e 'install.packages("ncdf4", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #conda Rscript -e 'install.packages("rgeos", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #Rscript -e 'install.packages("cleangeo", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'
    #conda Rscript -e 'install.packages("foreign", repos="https://cran.rstudio.com", lib="~/R/x86_64-redhat-linux-gnu-library")'

    echo "Done installing R packages..."
    echo "Add this to R script:"
    echo ".libPaths(c(.libPaths(), "~/R/x86_64-redhat-linux-gnu-library/"))"
fi

echo "Done..."

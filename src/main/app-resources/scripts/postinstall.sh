#!/bin/bash

#
# Add R libraries/packages and other necessary SW components to the develop and run-time environment.
# Prefereably, install the main part of the major SW components first via yum to reduce any
# incompability that may arise when conda upgrades/downgrades or installs additional library
# dependecies. And not all libraries/packages are available from the conda channels.
# If more SW components have been added, also add the installation dependency to pom.xml and the
# user guide for setting up a sandbox environment.
#

/opt/anaconda/bin/conda install -y --file /application/dependencies/R/packages.list

# Now when postprocessing (maps, trigger etc) moved to fanfar-postprocessing.git we shouldn't need
# to change version of library/package jpeg
#/opt/anaconda/bin/conda create --name cairo-env --file /application/dependencies/R/cairo-env.list

# Disabled due to variant 2 of "netcdf to obs"
#/opt/anaconda/bin/conda create --name cdo-env --file /application/dependencies/R/cdo-env.list

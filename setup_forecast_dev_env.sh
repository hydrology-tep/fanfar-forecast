#! /bin/env bash

update_users_bashrc_file=0
conda_packages_file=new_attempt_packages.list # packages.list
repo_url=https://github.com/hydrology-tep/fanfar-forecast
git_branch=develop
local_dir=fanfar-forecast

echo "Download git repository fanfar-forecast from github"
git clone ${repo_url} ${local_dir}
exit_code=$?
if [ $exit_code -ne 0 ]; then
    echo "Problem cloning repository, aborting"
    exit 1
fi

if [ ! -d ${local_dir} ]; then
    echo "The dir ${local_dir} with the cloned repository doesn't exist, aborting"
    exit 1
fi

pushd ${local_dir}
git checkout ${git_branch}
exit_code=$?
if [ $exit_code -ne 0 ]; then
    echo "Problem changing to branch ${git_branch}, aborting"
    exit 1
fi
popd

echo "Install base SW libraries and components via yum"
# proj?
# libgfortran for HYPE 5.x.0 built with gcc 4.4.7
sudo yum install -y miniconda proj libgfortran hdf5.x86_64 netcdf.x86_64
exit_code=$?
if [ $exit_code -ne 0 ]; then
    echo "Problem installing with yum, aborting"
    exit 1
fi

# Due to run-time problems
echo "Downgrade proj from version 4.8 to 4.7"
sudo yum downgrade proj

echo "Install additional SW libraries and components via conda"
# Since post-processing moved to fanfar-postprocessing and
# 'netcdf to obs' do not use cdo, we ignore the previous used conda
# environments cairo-env and cdo-env and only use the default base environemnt

if [ "x${update_users_bashrc_file}" == "x1" ]; then
    echo "Update users .bashrc with path to conda"
    echo "export PATH=/opt/anaconda/bin/:$PATH" >> ~/.bashrc
    source ~/.bashrc
#else
#    export PATH=/opt/anaconda/bin/:$PATH
fi

# Source above may not affect this session, do this anyway for this session
export PATH=/opt/anaconda/bin/:$PATH

sudo conda install -y --file ${local_dir}/src/main/app-resources/dependencies/R/${conda_packages_file}
exit_code=$?
if [ $exit_code -ne 0 ]; then
    echo "Problem installing with conda, aborting"
    exit 1
fi

echo "Install the fanfar-forecast application in /application/"
pushd ${local_dir}
mvn clean install
popd

echo "Done"
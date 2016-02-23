#!/bin/bash

WORK_DIR="$(pwd)"

if groups $USER | grep &>/dev/null '\bdocker\b'; then
  DOCKER="docker"
else
  DOCKER="sudo docker"
fi

sudo chmod -R a+rw $WORK_DIR

$DOCKER rm r-dev 2> /dev/null | true

echo "Cheat sheet - run the following commands:"
echo
echo "setwd('/src')"
echo "devtools::load_all()"
echo "  # Load the code in the current project"
echo
echo "setwd('/src')"
echo "formatR::tidy_dir(\"R\")"
echo "  # Format your source code"
echo
echo "setwd('/src')"
echo "lintr::lint_package()"
echo "  # Checks the style of the source code"
echo
echo "setwd('/src')"
echo "devtools::document()"
echo "  # Generates the documentation"
echo
echo "setwd('/src')"
echo "devtools::use_testthat()"
echo "  # Setup the package to use testthat"
echo
echo "-----------------------------------------"

# Bind mount your data
# assuming that current folder contains the data
$DOCKER run -v $WORK_DIR:/home/docker/data:rw \
    -v $WORK_DIR/:/src/ \
    -v $WORK_DIR/tests:/src/tests/ \
    -i -t --rm --name r-dev \
    hbpmip/r-interactive R

sudo chown -R $USER:$USER $WORK_DIR

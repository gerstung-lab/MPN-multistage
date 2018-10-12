# MPN-multistage
This repository contains the MPN multistage calculator accompanying the manuscript 
[_Classification and Personalized Prognosis in Myeloproliferative Neoplasms._
J. Grinfeld, J. Nangalia, et al. New England Journal of Medicine, 379:1416-1430, 2018.](http://dx.doi.org/10.1056/NEJMoa1716614)

A live version can be accessed at http://cancer.sanger.ac.uk/MPN-multistage.

The calculator runs as a shiny app inside a docker container. 
The calculator was developed by Jacob Grinfeld, based on work by Moritz Gerstung

## Running the shiny app directly
This requires R-3.2 or newer.
```R
install.packages(c("devtools","ggplot2","xtable","shiny")) 
devtools::install_github("mg14/mg14")
devtools::install_github("mg14/CoxHD/CoxHD")
shiny::runApp("[path-to-repository]/mpn-multistage")
```
This will open a browser window with the calculator.

## Building the docker container
This requires docker to be installed on your system
```shell
cd [path-to-repository]
docker build -t my_docker_id/mpn-multistage .
docker run -d -p 3838:3838 my_docker_id/mpn-multistage
```
Open your brower and point it to http://localhost:3838.

FROM rocker/shiny
MAINTAINER Jacob Grinfeld <jg738@cam.ac.uk>
RUN apt-get install -y libssl-dev
RUN R -e 'install.packages("devtools"); devtools::install_github("mg14/mg14"); devtools::install_github("mg14/CoxHD/CoxHD")' 
RUN rm -rf /srv/shiny-server/*
COPY ./mpn-multistage /srv/shiny-server/mpn-multistage
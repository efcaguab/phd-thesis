# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.5.2
# Installing texlive though apt-get cause I was having trouble using TinyTex
RUN apt-get update \
  && apt-get -y --no-install-recommends install texlive-full
RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
# bookdownplus depends on the R package magick which in turns depends on libmagick++-dev
RUN apt-get -y --no-install-recommends install libmagick++-dev
# Bookdown plus is used for its Classic thesis template
RUN R -e "install.packages('bookdownplus', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('kableExtra', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"

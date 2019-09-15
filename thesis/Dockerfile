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
# Packages for driver species chapter
RUN R -e "install.packages('bipartite', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('latex2exp', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('cowplot', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"

## Installing NEOVIM
RUN apt-get update \
    && apt-get -y --no-install-recommends install curl python3-dev python3-pip python3-setuptools silversearcher-ag
# Install some python pre-requisites for neovim plugnins
COPY init.vim /root/.config/nvim/init.vim
# Pyton requirements for autocompletion
RUN python3 -m pip install wheel \
  && python3 -m pip install pynvim \
# Install neovim software
  && wget https://github.com/neovim/neovim/releases/download/v0.3.8/nvim.appimage \
  && chmod u+x nvim.appimage \
  && ./nvim.appimage --appimage-extract \
  && ln -s /squashfs-root/AppRun /usr/local/bin/nvim \
# Install plugins
  && curl -fLo /root/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim \
  && squashfs-root/AppRun +PlugInstall +qall


RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"


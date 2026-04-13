FROM rocker/shiny:latest

ENV TZ="Etc/UTC"

# Install system dependencies
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev libzstd-dev

COPY scripts/bin/ /rocker_scripts/bin/
COPY scripts/setup_R.sh /rocker_scripts/setup_R.sh
RUN /rocker_scripts/setup_R.sh

ENV CRAN="https://cloud.r-project.org"
ENV LANG=en_US.UTF-8

# Install system dependencies for openssl and textshaping
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libzstd-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    default-libmysqlclient-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev

RUN R -e "install.packages(c('pak', 'tidyr', 'lubridate',  'bslib', 'ggtext'), repos='https://cloud.r-project.org', dependencies = TRUE, verbose = TRUE)"
RUN R -e "pak::pkg_install('RMySQL')"
RUN R -e "pak::pkg_install('RJDBC')"
RUN R -e "pak::pkg_install('ggfx')"
RUN R -e "pak::pkg_install('fitdistrplus')"
RUN R -e "pak::pkg_install('tidymodels')"
RUN R -e "pak::pkg_install('shinydashboard')"
RUN R -e "pak::pkg_install('tidyverse')"
RUN R -e "pak::pkg_install('RODBC')"
RUN R -e "pak::pkg_install('patchwork')"
RUN R -e "pak::pkg_install('ggiraph')"
RUN R -e "pak::pkg_install('ggh4x')"


ENV S6_VERSION="v2.1.0.2"
ENV SHINY_SERVER_VERSION="latest"
ENV PANDOC_VERSION="default"

COPY scripts/install_shiny_server.sh /rocker_scripts/install_shiny_server.sh
COPY scripts/install_s6init.sh /rocker_scripts/install_s6init.sh
COPY scripts/install_pandoc.sh /rocker_scripts/install_pandoc.sh
COPY scripts/init_set_env.sh /rocker_scripts/init_set_env.sh
RUN /rocker_scripts/install_shiny_server.sh

EXPOSE 8787
CMD ["/init"]

COPY scripts /rocker_scripts
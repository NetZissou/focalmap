FROM rockylinux/rockylinux:8
LABEL maintainer 'Jeff Ohrstrom <johrstrom@osc.edu>'

ENV R_BASE_VERSION 4.0.3
ENV CRAN https://cran.case.edu/

RUN dnf update -y && dnf clean all && rm -rf /var/cache/dnf/*
RUN dnf install -y \
        dnf-utils \
        epel-release \
    && dnf config-manager --set-enabled powertools \
    && dnf clean all && rm -rf /var/cache/dnf/*
RUN dnf install -y \
        gcc gcc-c++ gcc-gfortran gdb make curl curl-devel openssl-devel libxml2-devel libjpeg-turbo-devel \
        udunits2-devel cairo-devel proj-devel sqlite-devel geos-devel gdal gdal-devel \
        readline-devel libXt-devel java-11-openjdk-devel doxygen doxygen-latex texlive \
        freetype-devel libpng-devel libtiff-devel \
    && dnf clean all && rm -rf /var/cache/dnf/*

RUN curl -o R.tar.gz https://cran.r-project.org/src/base/R-4/R-${R_BASE_VERSION}.tar.gz \
    && tar -xzf R.tar.gz; \
    cd /R-${R_BASE_VERSION}; ./configure && make && make install; \
    cd /; rm -rf /R-${R_BASE_VERSION} && rm /R.tar.gz;

RUN cd /lib64; ln -s /usr/local/lib64/R/lib/libRblas.so libRblas.so \
        && ln -s /usr/local/lib64/R/lib/libRlapack.so libRlapack.so;

# cairo is available, but sadly not found first, so force to find it.
RUN echo "options(bitmapType='cairo')" > /usr/local/lib64/R/etc/Rprofile.site && \
    echo "options(repos=structure(c(CRAN='${CRAN}')))" >> /usr/local/lib64/R/etc/Rprofile.site

RUN curl --fail -sSLo /etc/yum.repos.d/passenger.repo https://oss-binaries.phusionpassenger.com/yum/definitions/el-passenger.repo && \
    dnf install -y passenger && \
    dnf clean all && rm -rf /var/cache/dnf/* && \
    passenger-config validate-install

RUN groupadd -g 2925 PZS0523 && \
    groupadd -g 7175 PDE0001 && \
    useradd -g PZS0523 -u 36970 focalmap && \
    useradd -g PZS0523 -u 36971 focalmapdev && \
    usermod -aG PDE0001 focalmap && \
    usermod -aG PDE0001 focalmapdev

# build all the dependencies before you copy the app to cache these layers
RUN Rscript -e "install.packages('devtools')"
RUN mkdir /tmp/build
COPY opioidDashboard.Rproj /tmp/build
COPY DESCRIPTION /tmp/build
RUN cd /tmp/build; Rscript -e "library ('devtools'); install()"

COPY . /app
WORKDIR /app


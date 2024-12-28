FROM fedora:latest

RUN dnf install -y gcc \
               g++ \
               rlwrap \
               automake \
               sbcl \
               libcurl-devel \
               zlib-devel \
               texlive-amsfonts \
               texlive-mdwtools \
               pandoc \
               texlive-collection-fontsextra \
               jq \
               texlive-latex && \
               dnf clean all

RUN useradd builder
USER builder
WORKDIR /home/builder

RUN git clone -b release https://github.com/roswell/roswell.git && \
    mkdir -p ~/.local && \
    cd roswell && \
    sh bootstrap && \
    ./configure --prefix=~/.local && \
    make && \
    make install && \
    cd ..

ENV PATH=/home/builder/.local/bin:/usr/local/bin:/usr/bin:/bin

RUN ros install sbcl-bin

RUN git clone https://github.com/ocicl/ocicl && \
    cd ocicl && \
    sbcl --load setup.lisp && \
    chmod a+x ocicl && \
    mv ocicl ~/.local/bin

COPY docker/init.lisp /home/builder/.roswell/init.lisp

ENTRYPOINT "/home/builder/.local/bin/ros"
CMD ["run"]

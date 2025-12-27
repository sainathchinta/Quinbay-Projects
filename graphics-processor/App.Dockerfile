USER root

RUN mkdir -p /data \
  && chown -R  appuser:appuser /data

ENV PKG_VER=1.3.45

WORKDIR /

COPY GraphicsMagick-$PKG_VER.tar.gz .

# Installing graphicsmagick dependencies
RUN apk add g++ make libjpeg-turbo-dev libpng-dev libtool libwebp-dev && \
  gzip -d -c GraphicsMagick-$PKG_VER.tar.gz | tar -xvf - && \
  cd GraphicsMagick-$PKG_VER && ./configure && make && make install && \
  cd / && \
  rm -rf GraphicsMagick-$PKG_VER && \
  rm GraphicsMagick-$PKG_VER.tar.gz

USER appuser

WORKDIR /app
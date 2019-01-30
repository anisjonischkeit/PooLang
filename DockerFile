FROM haskell:8.6.3

COPY . /opt/server
WORKDIR /opt/server

RUN stack build

CMD ["stack","exec","poolang"]


FROM haskell:9.2
RUN mkdir /server
COPY . /server
WORKDIR /server
RUN stack setup
RUN stack install 
RUN stack build
ENTRYPOINT ["stack", "run"]

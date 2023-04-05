FROM haskell
RUN mkdir /server
COPY . /server
WORKDIR /server
RUN stack setup
RUN stack install --resolver lts-20.13
RUN stack build
CMD ["stack", "run"]

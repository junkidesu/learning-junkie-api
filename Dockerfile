FROM --platform=linux/amd64 haskell:9.6.4 as build-stage

WORKDIR /usr/app

COPY . . 

RUN stack install --local-bin-path .

FROM --platform=linux/amd64 ubuntu:20.04 as deployment

WORKDIR /bin/

RUN apt-get update -y --assume-yes
RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes libpq-dev

COPY --from=build-stage /usr/app/learning-junkie-api-exe /bin/

ENTRYPOINT ["learning-junkie-api-exe"]

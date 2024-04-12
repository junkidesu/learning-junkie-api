FROM --platform=linux/amd64 haskell:9.6.4 as build-stage

WORKDIR /usr/app


COPY ./package.yaml /stack.yaml ./stack.yaml.lock .

RUN stack build --only-dependencies

COPY . . 

RUN stack install --local-bin-path .

FROM --platform=linux/amd64 ubuntu:20.04 as deployment

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y libpq-dev
RUN apt-get install -y curl

COPY --from=build-stage /usr/app/learning-junkie-api-exe /bin/

ENTRYPOINT ["/bin/learning-junkie-api-exe"]

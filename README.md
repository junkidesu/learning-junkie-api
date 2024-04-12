# Learning Junkie REST API

This repository contains the source code for the Learning Junkie REST API.

## Technologies Used

- [Haskell](https://www.haskell.org/)
- [Servant](https://docs.servant.dev/en/stable/index.html) (API)
- [PostgreSQL](https://www.postgresql.org/) (DB)
- [Diagrams](https://diagrams.github.io/) (Certificate Generation)
- [Amazon S3](https://aws.amazon.com/s3/) (Storage)

## Getting Started

First and foremost, clone the repository on your local machine:

```sh
$ git clone https://github.com/junkidesu/learning-junkie-api
```

### Prerequisites

#### Build Tools

To build and run the application locally, ensure that the following are installed:

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cabal](https://cabal.readthedocs.io/en/stable/)
- [Docker](https://www.docker.com/)

Stack and Cabal can be installed either independently or with the [GHCup](https://www.haskell.org/ghcup/) tool.

#### Services

The application uses PostgreSQL for the database and Amazon S3 for storage. Thus, a running PostgreSQL server (either local or remote), as well as a publicly readable [Amazon S3 Bucket](https://aws.amazon.com/s3/), are required.

#### Environment Variables

See [`.env.sample`](./.env.sample) to see the environment variables that must be set. You can either place them in a `.env` file, or supply them directly to the executable.

### Build and Start Executable

At the root of the repository, run the following:

```sh
$ stack install
$ learning-junkie-api-exe
```

### Start in Container

You may start the application along with a local PostgreSQL server using Docker Compose.

```sh
$ docker compose -f docker-compose.dev.yml
```

You still need to provide the necessary environment variables, though.

## Documentation

Swagger documentation of the API is available at https://learning-junkie-api-main.onrender.com/swagger-ui.

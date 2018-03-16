## Dockerization of the Shiny App for Fragility Index of meta-analyses
=======================

This is the Dockerized Shiny App Fragility MA

This Dockerfile is based on Debian "testing" and r-base image.

The image is available from [Docker Hub](https://hub.docker.com/r/iatal/fragility_ma/).

## Usage:

To run this Shiny App on your computer:

```sh
docker run --rm -p 80:80 iatal/fragility_ma
```

and it will avaliable at http://127.0.0.1/ or http://localhost


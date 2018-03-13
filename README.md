Dockerized Shiny App
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

=============================
TO DO:
- [ ] in forest plot, for RD, put absolute values, not log
- [ ] add a "see details" button to see trial level modifications
- [ ] csv file upload: remove existing datasets when uploading a dataset
- [ ] csv file upload: write explanation (variable names, etc, maybe show the example as a table)
- [ ] add "computing"
- [ ] about: explanation of fragility index, references, "example", github to fragility function

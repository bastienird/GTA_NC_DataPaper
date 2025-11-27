# syntax=docker/dockerfile:1.7
ARG MODE=prod
ARG BASE_IMAGE
FROM ${BASE_IMAGE:-rocker/r-ver:4.2.3}

# Maintainer information
LABEL org.opencontainers.image.authors="bastien.grasset@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Grasset Bastien <grasset.bastien@ird.fr>"



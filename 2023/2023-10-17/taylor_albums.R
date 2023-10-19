### Tidy Tuesday: Taylor Albums ###
### 10/17/2023 ###

# load libraries
library(tidyverse)
library(ggtext)
library(ggplot2)
library(viridis)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(statebins)
library(ggimage)
library(tayloRswift)
library(ggnewscale)

# load data
tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

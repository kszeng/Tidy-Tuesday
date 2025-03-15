## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# install.packages("ggalt")
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("devtools")
# devtools::install_github("hrbrmstr/streamgraph")
# install.packages("htmlwidgets")
# install.packages("ggstream")

library(ggplot2)
library(ggalt)
library(hrbrthemes)
library(viridis)
library(skimr)
library(DataExplorer)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(scales)
library(ggpubr)
library(tidyverse)
library(tidytext)
library(quarto)
library(devtools)
library(streamgraph)
library(htmlwidgets)
library(ggstream)

## -----------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2025, week = 9)
longbeach <- tuesdata$longbeach


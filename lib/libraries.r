library(sp)
library(maptools)
library(digest)
gpclibPermit()
library(knitr)
library(XML)
library(RCurl)
library(gdata)
library(dplyr)
library(plyr)
library(shiny)
library(plotly)
library(ggplot2)
library(reshape2)
library(reshape)
library(maps)
library(DT)
library(pvclust)

# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

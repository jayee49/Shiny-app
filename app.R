require(shiny)
require(tidyverse)
require("shinyjs")

country <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/country.csv",header = F)
region <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/region.csv",header = F)
global <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/global_situation_data-2.csv",
                   sep = "\t", header = T, fileEncoding="UCS-2LE")

global$Country = as.character(global$Country)
global$Indicator.short.code = as.character(global$Indicator.short.code)
global$Region = as.character(global$Region)

region = as.matrix.data.frame(region)
colnames(region) = region[3,]
region = as.data.frame(region[-(1:3),])
region$`WHO region` = as.character(region$`WHO region`)

country = as.matrix.data.frame(country)
colnames(country) = country[3,]
country = as.data.frame(country[-(1:3),])
country$Country = as.character(country$Country)


### find out the lost country 
global$Country[which(is.na(match(global$Country,country$Country)))] 
country$Country[which(is.na(match(country$Country,global$Country)))]

mean(global$Value)
mean(global[global$Region=="Europe",]$Value)

country$Country[which(country$Country == "Eswatini")] = "Swaziland"
country$Country[which(country$Country == "United Kingdom of Great Britain and Northern Ireland")] = "United Kingdom"

global.n.sex = merge(global,country,by.x = "Country")

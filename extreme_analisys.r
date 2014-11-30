################################################################################################
# PURPOSE:
# Modeling rainfall extremes of tuscany 

# INPUT:
# WrkgDir: Root directoryectory (root directory)
# WrkgDir/'predictors': contains GeoTiff static and meteorological predictors (the latter begin with 'r_')
#
# OUTPUT:
# Creates...
#
# INSTITUTIONS:
# Consorzio LaMMA - www.lamma.rete.toscana.it &
# Istituto di Biometeorologia - www.ibimet.cnr.it
#
# DEVELOPERS:
# Massimo Perna - perna@lamma.rete.toscana.it 
# Ramona Magno magno@lamma.rete.toscana.it
# Valerio Capecchi - capecchi@lamma.rete.toscana.it; valcap74@gmail.com &
# Alfonso Crisci - a.crisci@ibimet.cnr.it; alfcrisci@gmail.com
#
# CREDITS AND REFERENCES:
# https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4016761/ Rainfall extremes: Toward reconciliation after the battle of distributions





##########################################################################################################
# Load library

if (!require(devtools)) {install.packages("devtools"); library(devtools)}
if (!require(extRemes)) {install.packages("extRemes"); library(extRemes)}
if (!require(ismev)) {install.packages("ismev"); library(ismev)}
if (!require(evir)) {install.packages("evir"); library(evir)}
if (!require(evd)) {install.packages("evd"); library(evd)}
if (!require(SpatialExtremes)) {install.packages("SpatialExtremes"); library(SpatialExtremes)}
if (!require(evmix)) {install.packages("evmix"); library(evmix)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}

##########################################################################################################
# Setup working dir if necessary

setwd("")

##########################################################################################################
# Load daily data in  1960-1990 and metadata


rain_data=readRDS(serie_toscane_1960_1990.rds)
nomi_stazioni=sapply(res,function(x) x$stazione[1])
metadati_stazioni=read.csv("dati_stazioni_pio_file.csv")

##########################################################################################################
# Variable in data.frames 

# anno,mese,giorno,rain_mm,stazione,data

# Variable in metadata

# ID,lon,lat,quota,nome,nomelista

####################################################################################
# Create list to store results 

list_perc_missing=list()
list_fit_gpareto=list()
list_fit_pp=list()
list_return_level=list()
list_return_level_cv=list()


###################################################################################
# USO IL PACCHETTO R Extremes based on evd

tresh_pio=40 # mm
tresh_missing=40 # % of missing data

for ( i in 1:length(rain_data)){

list_perc_missing[[i]]=(length(which(is.na(rain_data[[i]]$rain_mm)))/length(rain_data[[i]]$rain_mm))*100

if (list_perc_missing[[i]] < tresh_missing) {

                                            #fitto una GP 
                                             list_fit_gpareto[[i]] <- fevd(rain_data[[i]]$rain_mm, x, threshold=tresh_pio, type="GP", units="mm", verbose=TRUE)
                                
                                            #....OPPURE fitto una PP

                                            list_fit_pp[[i]] <- fevd(rain_data[[i]]$rain_mm, x, threshold=tresh_pio, type="PP", units="mm", verbose=TRUE)
 
                                            #trovo i tempi di ritorno e i return levels

                                            list_return_level[[i]] <- return.level(fit_GP, return.period = c(2, 10, 50, 100, 200)

                                            #valuto l'intervallo di confidenza (sulla pioggia di 100 anni)

                                            list_return_level_cv[[i]] <- ci.fevd(fit_GP)
                                            }
}


###########################################################################################
# Reference
# http://www.esapubs.org/archive/ecol/E086/060/rextreme.txt

###########################################################################################
# Supplementary code



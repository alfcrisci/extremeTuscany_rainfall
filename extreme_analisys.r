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
if (!require(reshape)) {install.packages("reshape"); library(reshape)}



##########################################################################################################
# Setup working dir if necessary

setwd("")

mainDir <- getwd()
subDir <- "images"

if (!file.exists(subDir)){
     dir.create(file.path(mainDir, subDir))
}

##########################################################################################################
# Load daily data in  1960-1990 and metadata


rain_data=readRDS("serie_toscana_1960_1990.rds")

nomi_stazioni=sapply(rain_data,function(x) x$stazione[1])

metadati_stazioni=read.csv("dati_stazioni_pio_file.csv")

##########################################################################################################
# Variable in data.frames di lista caricati come rds file dopo averli riuniti

# anno,mese,giorno,rain_mm,stazione,data

# Variable in metadata

# ID,lon,lat,quota,nome,nomelista

####################################################################################
# Create list to store results 

list_perc_missing=list()
list_fit_gpd=list()
list_fit_pp=list()
list_return_level_ci_PP=list()
list_return_level_ci_gpd=list()
list_metadata=list()


###################################################################################
# USO IL PACCHETTO R Extremes based on evd

tresh_pio=30 # mm
tresh_missing=0.6 # % of missing data

for ( i in 1:length(rain_data)){

list_perc_missing[[i]]=(length(which(is.na(rain_data[[i]]$rain_mm)))/length(rain_data[[i]]$rain_mm))


if (list_perc_missing[[i]] < tresh_missing) {
                                           
                                            
                                            #   Ometto dati mancanti 
					   
                                            temp=na.omit(rain_data[[i]])
                                            
					    #    fitto una GP 
					   						
					    #valuto l'intervallo di confidenza (sulla pioggia di2,5, 10,30,50,75,100,150,200 anni) ,rperiods= c(2,5, 10,30,50,75,100,150,200)

                                            list_fit_gpd[[i]] <- fevd(rain_mm, temp, threshold=tresh_pio, type="GP",time.units = "days", period.basis = "year", units="mm", verbose=TRUE)
                                            
					    png(paste0("images/",metadati_stazioni$nomelista[i],"_gpd.png"), width = 800, height = 600)
					    plot(list_fit_gpd[[i]])
					    dev.off()
                                            #....OPPURE fitto una PP

                                            list_fit_pp[[i]] <- fevd(rain_mm, temp, threshold=tresh_pio,time.units = "days", period.basis = "year", type="PP", units="mm", verbose=TRUE)
                                            png(paste0("images/",metadati_stazioni$nomelista,"_pp.png"), width = 800, height = 600)
					    plot(list_fit_gpd[[i]])
					    dev.off()
                                            #trovo i tempi di ritorno e i return levels

                                            list_return_level_ci_PP[[i]] <- ci.fevd(list_fit_gpd[[i]],return.period= c(2,5, 10,30,50,75,100,150,200))
                                           
                                            list_return_level_ci_gpd[[i]] <- ci.fevd(list_fit_pp[[i]],return.period= c(2,5, 10,30,50,75,100,150,200))
                                            
											
					    resume_data=as.data.frame(c(metadati_stazioni[i,],
					                               list_fit_gpd[[i]]$results$par,
								       list_fit_pp[[i]]$results$par,
								       perc_manc=unlist(list_perc_missing[[i]]),
								       as.numeric(t(list_return_level_ci_PP[[i]][5,])),
								       as.numeric(t(list_return_level_ci_gpd[[i]][5,]))))
																		
					   names(resume_data)=c("ID","lon","lat","quota",
					                        "nome","nomelista",
					                        "scale_gpd","shape_gpd",
					                        "location_pp","scale_pp","shape_pp",
					                        "perc_manc",
					                        "ci_inf_100_pp","ci_e_100_pp","ci_max_100_pp",
					                        "ci_inf_100_gpd","ci_e_100_gpd","ci_max_100_gpd")
											
											
					      list_metadata[[i]]=resume_data
											
										
											}
                                             }

saveRDS(list_perc_missing,"list_perc_missing.rds")
saveRDS(list_fit_gpd,"list_fit_gpareto.rds")
saveRDS(list_fit_pp,"list_fit_pp.rds")
saveRDS(list_return_level_ci_PP,"list_return_level_ci_PP.rds")
saveRDS(list_return_level_ci_gpd,"list_return_level_ci_gpd.rds")

###################################################################################

saveRDS(list_metadata,"list_metadata.rds")
###################################################################################
# unisco i dati
final_data=reshape::merge_all(list_metadata)


write.csv(final_data,"final_data.csv",row.names=F)
###################################################################################


###########################################################################################
# Reference
# http://www.esapubs.org/archive/ecol/E086/060/rextreme.txt
# Fatichi  Simone, Susana M. Barbosa, Enrica Caporali and Maria E. Silva. 2009. Deterministic vs stochastic trends: detection and challenges, Journal of Geophysical Research, 114, D18121, doi:10.1029/2009JD011960.
# Fatichi  Simone and Enrica Caporali. 2009. A comprehensive analysis of changes in precipitation regime in Tuscany. International Journal of Climatology, 29(13), 1883-1893,  doi: 10.1002/joc.1921.
# Enrica Caporali, Elisabetta Cavigli and Alessandra Petrucci. 2008. The index rainfall in the regional frequency analysis of extreme events in Tuscany (Italy). United States. Environmetrics (Wiley InterScience), 19: 714-724. DOI: 10.1002/env.949. ISSN: 1180-4009 E-ISSN: 1099-095x.
# Tartaglia V., Enrica Caporali, E. Cavigli, and A. Moro. 2005. L-moments based assessment of a mixture model for frequency analysis of rainfall extremes. Copernicus Publications Production Office Germany. Advances in Geosciences (2), 331-334. http://www.adv-geosci.net/2/331/2006/adgeo-2-331-2006.pdf. ISSN: 16807340. EISSN: 16807359.
# Caporali Enrica, M. Rinaldi and N. Casagli. 2005. The Arno River Floods. Italy. Giornale di Geologia Applicata, Vol. 1, 177:192. DOI: 10.1474/GGA. 2005-01.0-18.0018. ISSN: 1825-6635.

###########################################################################################
# Supplementary code
# list_perc_missing=readRDS(,"list_perc_missing.rds")
# list_fit_gpareto=readRDS("list_fit_gpareto.rds")
# list_fit_pp=readRDS("list_fit_pp.rds")
# list_return_level_ci_PP=readRDS("list_return_level_ci_PP.rds")
# list_return_level_ci_gpd=readRDS("list_return_level_ci_gpd.rds")



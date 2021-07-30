library("foreign")
library("AICcmodavg")
library(lmtest)
library(sandwich)
library(dplyr)
library("spatialreg")
library("spdep")
library(rgdal)
library(lme4)
library(MuMIn)
library(installr)
library(bbmle)
library("McSpatial")
library(ggplot2)
library(reshape2)
library(broom)
library("margins")
library("ivprobit")
library(jtools)
library(sjPlot)
library(party) 
library(gstat)


cat("\014") 
rm(list = ls())
graphics.off()

#Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma
#Estimate!!HOUSEHOLDS BY TYPE!!Total households!!Family households (families)!!Female householder, no husband present, family


########################joint Dataset
################################################
##############      Mexico      ################
################Street connectivity
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_Municipalities\\Revisted_July2019\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, nrows = 1, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
master2 <- do.call("rbind",filelist)
master2$COUNTER <- as.character(master2$COUNTER)
################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_Municipalities\\Revisted_July2019\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
inter <- do.call("rbind",filelist)
inter$COUNTER <- as.character(inter$COUNTER)
################ >4 way intersections
inter4 <- subset(inter, inter$X==4)
inter4 <- inter4[,c("COUNTER","streets_per_node_proportion")]
inter4 <- aggregate(inter4$streets_per_node_proportion, by=list(COUNTER=inter4$COUNTER), FUN=sum)
names(inter4) <- c("COUNTER","proportion_4ormore_intersections")
################3 way intersections
inter3 <- subset(inter, inter$X==3)
inter3 <- inter3[,c("COUNTER","streets_per_node_proportion")]
inter3 <- aggregate(inter3$streets_per_node_proportion, by=list(COUNTER=inter3$COUNTER), FUN=sum)
names(inter3) <- c("COUNTER","proportion_3orless_intersections")
#
inter <- merge(x=inter3,y=inter4, by="COUNTER", all.x=TRUE)
master2 <- merge(x=master2, y=inter, by="COUNTER", all.x=TRUE)
master2$proportion_4ormore_intersections[is.na(master2$proportion_4ormore_intersections)] <- 0
master2$proportion_3orless_intersections[is.na(master2$proportion_3orless_intersections)] <- 0
master2$regularity <- (master2$proportion_4ormore_intersections-master2$proportion_3orless_intersections) / (master2$proportion_4ormore_intersections+master2$proportion_3orless_intersections)
####### Adding areas
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX")

area <- read.dbf("Shapefiles\\Municipios_2010_5_projected_meters.dbf")
area$COUNTER <- as.character(area$COUNTER)
area <- area[,c("COUNTER","area")]
master2 <- merge(x=master2,y=area, by="COUNTER", all.x=TRUE)
master2$node_density_km <- master2$n /master2$area
master2$intersection_density_km <- master2$intersection_count / master2$area
master2$edge_density_km <- (master2$edge_length_total/1000) / master2$area
master2$street_density_km <- master2$street_length_total / master2$area
master2$street_segments_by_intersections <- master2$street_segments_count/master2$intersection_count
master2 <- master2[,c("COUNTER","node_density_km","edge_density_km","intersection_density_km","regularity","area")]

#######Adding Gini
master <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\MX_municipalities_Gini\\Gini_MX_mun.csv",header = TRUE)
master <- merge(x=master, y=master2, by="COUNTER", all.y=T)
#######Adding Marginality
marginality <- read.csv("Data\\Marginality_Index_mun.csv", header = TRUE)
marginality$code <- as.character(marginality$CVE_MUN)
marginality2015 <- subset(marginality, marginality$AÑO==2015) 

mapping <- read.dbf("Shapefiles\\Municipios_2010_5.dbf")
mapping$code <-as.character(mapping$CONCAT)
mapping <- mapping[,c("code","NOM_MUN","ZM","COUNTER")]
mapping <- merge(mapping,marginality2015,by="code")
mapping$IM <- as.numeric(as.character(mapping$IM))
mapping$ANALF <- as.numeric(as.character(mapping$ANALF))
mapping$SPRIM <- as.numeric(as.character(mapping$SPRIM))
mapping$OVSDE <- as.numeric(as.character(mapping$OVSDE))
mapping$OVSEE <- as.numeric(as.character(mapping$OVSEE))
mapping$OVSAE <- as.numeric(as.character(mapping$OVSAE))
mapping$VHAC <- as.numeric(as.character(mapping$VHAC))
mapping$OVPT <- as.numeric(as.character(mapping$OVPT))
mapping$OVSDE <- as.numeric(as.character(mapping$OVSDE))
mapping$PL.5000 <- as.numeric(as.character(mapping$PL.5000))
mapping$PO2SM <- as.numeric(as.character(mapping$PO2SM))
mapping$IM_positive <- mapping$IM - min(mapping$IM) + 0.001

mapping$MUN <- NULL
mapping$ZM.y <- NULL
names(mapping)[names(mapping) == "ZM.x"] <- "ZM"
mapping$COUNTER <- as.character(mapping$COUNTER)
mapping <- merge(x=mapping, y=master, by="COUNTER")
############## GDP
### TIpo de cambio 2013 0.07841

gdp <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\GDP_per_municipality.csv",
                header = T, colClasses=c("code"="character"))
mapping <- merge(x=mapping, y=gdp, by="code", all.x=T)
mapping$gdp_percapita <- (mapping$VACB_millones/mapping$POB_TOT)*(1000)*0.07841 ## In thousands of USD per person
mapping$pop_density <- mapping$POB_TOT/mapping$area
mapping$urban <- ifelse(mapping$POB_TOT>=50000,1,0)
######Coordenates
coord <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\MX_municipalites_lat_lon.csv",
                  header = TRUE, colClasses=c("COUNTER"="character"))
coord <- coord[,c("COUNTER","xcoord",'ycoord')]
mapping <- merge(x=mapping, y=coord, by="COUNTER", all.x=T)

#############################################################
################################################################
################ Play with the poverty type########################
mapping$perc_poverty <- mapping$perc_poverty_income
###########################################################

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\02-Code\\Subcode")
list.files()
source(here::here('MX_MAs.R'))


mapping <- mapping[,c("COUNTER","code","NOM_MUN","xcoord","ycoord","ZM","ENT","urban","IM_positive",
                      "perc_poverty","pop_density","gdp_percapita","area",
                      "POB_TOT", "unemployment","perc_secondary_compelte" , "pea_perc_of_totpob",
                      "hous_fem_perc","income_gini","bet_cen_Gini","clo_cen_Gini","inf_cen_Gini",
                      "pagerank_Gini","node_density_km","edge_density_km",
                      "intersection_density_km","regularity")]

mapping$perc_poverty <- as.numeric(as.character(mapping$perc_poverty))

####################### Deflactor precios 2012
#El INPC promedio en 98, 03,  08  13, de acuerdo con INEGI es: 
#48.46347855, 72.25662527, 89.09304683, 1.13588416.
#Estos datos coinciden con los tuyos. La quincena base (la que tendría 100) es la primera de 2011.  
#Para los datos de 1998 lo que hay que hacer es dividirlos por 0.4846347855 para así obtener 
#los datos de 1998 en pesos de 2011. El mismo procedimiento se usa para 03 y 08, 
#con lo que se obtiene al final todas las cifras en pesos de 2011. Y
#por tanto ya pueden compararse entre sí, por lo que ya se puede hacer entonces el DEA y el MPI 
#con esos datos reales.
#My propio deflactor = 1.169367909
mapping$gdp_percapita <- mapping$gdp_percapita / 1.169367909

##write.csv(mapping,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Final_Mexico.csv", row.names = F)


mapping$gdp_percapita <- ifelse( mapping$gdp_percapita<=0, 0.001129935, mapping$gdp_percapita)
mapping$regularity <- ifelse(mapping$regularity==-Inf,0,mapping$regularity)
mapping$regularity <- ifelse(mapping$regularity==Inf,0,mapping$regularity)  

mapping$y <- log(( (mapping$perc_poverty)/100)/(1-(mapping$perc_poverty)/100))
mapping$metro <- ifelse(mapping$ZM>0, 1,0)
mapping$metro <- ifelse(is.na(mapping$metro), 0, mapping$metro)

mapping$gdp_percapita <- log(mapping$gdp_percapita)
mapping$pop_density <- log(mapping$pop_density )
#mapping$edge_density_km <- log(mapping$edge_density_km )
#mapping$tion_density_km <- log(mapping$tion_density_km)


mapping_mx <- mapping
mapping_mx$country <- "Mexico"
rm(list=setdiff(ls(), c("mapping_mx", "mapping_mx_metro_agg")))


################################################
##############      USA      ################
################Street connectivity
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Networks_County\\Revisted_July2019\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, nrows = 1, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
master2 <- do.call("rbind",filelist)
master2$COUNTER <- as.character(master2$COUNTER)
################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Networks_County\\Revisted_July2019\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
inter <- do.call("rbind",filelist)
inter$COUNTER <- as.character(inter$COUNTER)
################ >4 way intersections
inter4 <- subset(inter, inter$X==4)
inter4 <- inter4[,c("COUNTER","streets_per_node_proportion")]
inter4 <- aggregate(inter4$streets_per_node_proportion, by=list(COUNTER=inter4$COUNTER), FUN=sum)
names(inter4) <- c("COUNTER","proportion_4ormore_intersections")
################3 way intersections
inter3 <- subset(inter, inter$X==3)
inter3 <- inter3[,c("COUNTER","streets_per_node_proportion")]
inter3 <- aggregate(inter3$streets_per_node_proportion, by=list(COUNTER=inter3$COUNTER), FUN=sum)
names(inter3) <- c("COUNTER","proportion_3orless_intersections")
#
inter <- merge(x=inter3,y=inter4, by="COUNTER", all.x=TRUE)
master2 <- merge(x=master2, y=inter, by="COUNTER", all.x=TRUE)
master2$proportion_4ormore_intersections[is.na(master2$proportion_4ormore_intersections)] <- 0
master2$proportion_3orless_intersections[is.na(master2$proportion_3orless_intersections)] <- 0
master2$regularity <- (master2$proportion_4ormore_intersections-master2$proportion_3orless_intersections) / (master2$proportion_4ormore_intersections+master2$proportion_3orless_intersections)
####### Adding areas
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County")

area <- read.dbf("cb_2017_us_county_20m.dbf")
area$GEOID <- as.character(area$GEOID)
area_aux <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers_SUTD\\Sharing_Economy\\Data\\US_County\\Land_Area\\County_area.csv", 
                     header = T, colClasses=c("geoid"="character"))
area_aux$GEOID <- area_aux$geoid
area <- merge(x=area,y=area_aux, by="GEOID", all.x=T)
area$COUNTER <- as.character(area$COUNTER)
area <- area[,c("COUNTER","area_sq_km")]
colnames(area) <- c("COUNTER","area")
rm(area_aux)
master2 <- merge(x=master2,y=area, by="COUNTER", all.x=TRUE)
master2$node_density_km <- master2$n /master2$area
master2$intersection_density_km <- master2$intersection_count / master2$area
master2$edge_density_km <- (master2$edge_length_total/1000) / master2$area
master2$street_density_km <- master2$street_length_total / master2$area
master2$street_segments_by_intersections <- master2$street_segments_count/master2$intersection_count
master2 <- master2[,c("COUNTER","node_density_km","edge_density_km","intersection_density_km","regularity","area")]

#######Adding Gini
master <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\USA_municipalities_Gini\\Gini_USA_mun.csv",header = TRUE)
master <- merge(x=master, y=master2, by="COUNTER", all.y=T)

########Codes
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County")

mapping <- read.dbf("cb_2017_us_county_20m.dbf")
mapping$COUNTER <- as.character(mapping$COUNTER)
mapping <- mapping[,c("COUNTER","GEOID")]
colnames(mapping) <- c("COUNTER","code")
############## GDP
### TIpo de cambio 2013 0.07841

gdp <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\USA_indicators.csv",
                header = T, colClasses=c("code"="character"))
mapping <- merge(x=mapping, y=gdp, by="code", all.y=T)
mapping$urban <- ifelse(mapping$POB_TOT>=50000,1,0)
mapping <- merge(x=mapping, y=master, by="COUNTER", all.x=TRUE)

mapping$ENT <-  sub('.*,\\s*', '', mapping$NOM_MUN)

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\02-Code\\Subcode")
list.files()
source(here::here('USA_MAs.R'))



mapping <- mapping[,c("COUNTER","code","NOM_MUN","xcoord","ycoord","ZM","ENT","urban","IM_positive",
                      "perc_poverty","pop_density","gdp_percapita","area",
                      "POB_TOT", "unemployment","perc_secondary_compelte" ,"pea_perc_of_totpob",
                      "hous_fem_perc","income_gini",
                      "bet_cen_Gini","clo_cen_Gini","inf_cen_Gini",
                      "pagerank_Gini","node_density_km","edge_density_km",
                      "intersection_density_km","regularity")]

mapping$perc_poverty <- as.numeric(as.character(mapping$perc_poverty))



#write.csv(mapping,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Final_USA.csv", row.names = F)


mapping$regularity <- ifelse(mapping$regularity==-Inf,0,mapping$regularity)
mapping$regularity <- ifelse(mapping$regularity==Inf,0,mapping$regularity)  


mapping$y <- log(( (mapping$perc_poverty)/100)/(1-(mapping$perc_poverty)/100))
mapping$metro <- ifelse(!is.na(mapping$ZM), 1,0)
mapping$metro <- ifelse(is.na(mapping$metro), 0, mapping$metro)
testing <- subset(mapping, mapping$metro==1)

mapping$gdp_percapita <- as.numeric(as.character(mapping$gdp_percapita))
mapping$gdp_percapita <- log(mapping$gdp_percapita)
mapping$pop_density <- log(mapping$pop_density)
#mapping$edge_density_km <- log(mapping$edge_density_km )
#mapping$intersection_density_km <- log(mapping$intersection_density_km)

mapping_usa <- mapping
mapping_usa$country <- "US"
mapping_usa <- subset(mapping_usa, mapping_usa$ENT!="Alaska")
mapping_usa <- subset(mapping_usa, mapping_usa$ENT!="Hawaii")
mapping_usa <- subset(mapping_usa, mapping_usa$COUNTER!="46113")


rm(list=setdiff(ls(), c("mapping_mx", "mapping_usa","mapping_mx_metro","mapping_usa_metro",
                        "mapping_mx_metro_agg","mapping_usa_metro_agg")))

mapping_mx$income_gini <- as.numeric(as.character(mapping_mx$income_gini))

mapping_mx$metro <- as.factor(mapping_mx$metro)
mapping_mx$perc_poverty <- mapping_mx$perc_poverty/100
mapping_mx$perc_secondary_compelte<- mapping_mx$perc_secondary_compelte/100
mapping_mx$hous_fem_perc <- mapping_mx$hous_fem_perc/100
mapping_mx$unemployment <- mapping_mx$unemployment/100
mapping_mx$pea_perc_of_totpob <- mapping_mx$pea_perc_of_totpob/100

mapping_usa$metro  <- as.character(mapping_usa$metro)
mapping_usa$perc_poverty <- mapping_usa$perc_poverty/100
mapping_usa$perc_secondary_compelte<- mapping_usa$perc_secondary_compelte/100
mapping_usa$hous_fem_perc <- mapping_usa$hous_fem_perc/100
mapping_usa$unemployment <- mapping_usa$unemployment/100
mapping_usa$pea_perc_of_totpob <- mapping_usa$pea_perc_of_totpob/100



#############################################
##################### Regressions
mapping <- rbind(mapping_mx,mapping_usa)

################################################################33
### Ordering variables
################################################################
mapping$A0urban <- as.factor(as.character(mapping$urban))
mapping$A1metro <- mapping$metro 
mapping$A2pop_density <- mapping$pop_density 
mapping$A3gdp_percapita  <- mapping$gdp_percapita  
mapping$A4unemployment <- mapping$unemployment  
mapping$A5perc_secondary_compelte <- mapping$perc_secondary_compelte
mapping$A6hous_fem_perc <- mapping$hous_fem_perc 
mapping$A7income_gini <- mapping$income_gini
mapping$A8pea_perc_of_totpob <- mapping$pea_perc_of_totpob
mapping$X1edge_density_km  <- mapping$edge_density_km 
mapping$X2intersection_density_km <- mapping$intersection_density_km
mapping$X3regularity <- mapping$regularity
mapping$X4bet_cen_Gini <- mapping$bet_cen_Gini
mapping$X5clo_cen_Gini <- mapping$clo_cen_Gini
mapping$X6inf_cen_Gini <- mapping$inf_cen_Gini


mapping_mx <- subset(mapping, mapping$country=="Mexico" )
mapping_usa <- subset(mapping, mapping$country=="US" )

mapping_mx_metro <- subset(mapping_mx, mapping_mx$metro==1)
mapping_usa_metro <- subset(mapping_usa, mapping_usa$metro==1)
mapping_metro <- subset(mapping, mapping$metro==1)

rm(list=setdiff(ls(), c("mapping_mx", "mapping_usa","mapping",
                        "mapping_mx_metro","mapping_usa_metro",
                        "mapping_mx_metro","mapping_usa_metro",
                        "mapping_mx_metro_agg","mapping_usa_metro_agg")))

datos_mx <- mapping_mx[,c("COUNTER","code","perc_poverty","xcoord","ycoord","perc_poverty","A1metro","A2pop_density",
                          "A3gdp_percapita","A4unemployment","A5perc_secondary_compelte",
                          "A6hous_fem_perc","A7income_gini","A8pea_perc_of_totpob",
                          "X1edge_density_km","X2intersection_density_km","X3regularity",
                          "X4bet_cen_Gini","X5clo_cen_Gini","X6inf_cen_Gini","country")] 
datos_usa <- mapping_usa[,c("COUNTER","code","perc_poverty","xcoord","ycoord","perc_poverty","A1metro","A2pop_density",
                            "A3gdp_percapita","A4unemployment","A5perc_secondary_compelte",
                            "A6hous_fem_perc","A7income_gini","A8pea_perc_of_totpob",
                            "X1edge_density_km","X2intersection_density_km","X3regularity",
                            "X4bet_cen_Gini","X5clo_cen_Gini","X6inf_cen_Gini","country")] 
datos_figures <- mapping

lags = 5

formula1 <- perc_poverty    ~    A1metro +  A3gdp_percapita +
                A2pop_density +
                A5perc_secondary_compelte +  #A6hous_fem_perc +  A4unemployment +
                A7income_gini + A8pea_perc_of_totpob 

formula2 <- perc_poverty~  A1metro +A3gdp_percapita +
                A2pop_density +
                A5perc_secondary_compelte + #A6hous_fem_perc +  A4unemployment + 
                A7income_gini + A8pea_perc_of_totpob +
                X1edge_density_km + X2intersection_density_km + X3regularity +
                X4bet_cen_Gini + X5clo_cen_Gini + X6inf_cen_Gini 

#formulaiv <-  target   ~ A1metro  + A2pop_density +  A3gdp_percapita  +  
               #A4unemployment + A5perc_secondary_compelte +
               #A6hous_fem_perc  |  perc_poverty| 
#               A1metro + A2pop_density +  A3gdp_percapita  +
#                A4unemployment + A5perc_secondary_compelte +
#                A6hous_fem_perc + perc_poverty +
#                X1edge_density_km + X2intersection_density_km + X3regularity +
#                X4bet_cen_Gini + X5clo_cen_Gini + X6inf_cen_Gini

formula3 <- perc_poverty ~  A1metro + A3gdp_percapita  +
                A2pop_density +
                A5perc_secondary_compelte + #A6hous_fem_perc + A4unemployment +  
                A7income_gini + A8pea_perc_of_totpob +
                X1edge_density_km + X2intersection_density_km + X3regularity +
                X4bet_cen_Gini + X5clo_cen_Gini + X6inf_cen_Gini  +
                z1_edge_density_km + z2_intersection_density_km + z3_regularity + 
                z4_bet_cen_Gini + z5_clo_cen_Gini +  z6_inf_cen_Gini  + z9_perc_perc_poverty

##################### Mexico             
m_logit_mx_basic <- glm(formula1, family=binomial(logit),
                     data=datos_mx,na.action=na.exclude)
td_logit_t_mx_basic <- tidy(m_logit_mx_basic, conf.int = TRUE, xintercept=FALSE)
td_logit_t_mx_basic$significance1 <- ifelse(as.data.frame(td_logit_t_mx_basic$p.value)<=0.01, "***","")
td_logit_t_mx_basic$significance2 <- ifelse( (as.data.frame(td_logit_t_mx_basic$p.value)>0.01&
                                              as.data.frame(td_logit_t_mx_basic$p.value)<=0.05), "**","")
td_logit_t_mx_basic$significance3 <- ifelse( (as.data.frame(td_logit_t_mx_basic$p.value)>0.05&
                                              as.data.frame(td_logit_t_mx_basic$p.value)<=0.1), "*","")
td_logit_t_mx_basic$significance <- paste(td_logit_t_mx_basic$significance1,
                                          td_logit_t_mx_basic$significance2,
                                          td_logit_t_mx_basic$significance3)
td_logit_t_mx_basic$significance1 <- NULL
td_logit_t_mx_basic$significance2 <- NULL
td_logit_t_mx_basic$significance3 <- NULL



m_logit_mx_sci <- glm(formula2, family=binomial(logit),
                      data=datos_mx, na.action=na.exclude) 
td_logit_t_mx_sci <- tidy(m_logit_mx_sci, conf.int = TRUE, xintercept=FALSE)
td_logit_t_mx_sci$significance1 <- ifelse(as.data.frame(td_logit_t_mx_sci$p.value)<=0.01, "***","")
td_logit_t_mx_sci$significance2 <- ifelse( (as.data.frame(td_logit_t_mx_sci$p.value)>0.01&
                                                as.data.frame(td_logit_t_mx_sci$p.value)<=0.05), "**","")
td_logit_t_mx_sci$significance3 <- ifelse( (as.data.frame(td_logit_t_mx_sci$p.value)>0.05&
                                                as.data.frame(td_logit_t_mx_sci$p.value)<=0.1), "*","")
td_logit_t_mx_sci$significance <- paste(td_logit_t_mx_sci$significance1,
                                          td_logit_t_mx_sci$significance2,
                                          td_logit_t_mx_sci$significance3)
td_logit_t_mx_sci$significance1 <- NULL
td_logit_t_mx_sci$significance2 <- NULL
td_logit_t_mx_sci$significance3 <- NULL

m_qb_mx_simple <- glm(formula1, family= quasibinomial(logit),
               data=datos_mx,na.action=na.exclude)
td_qb_t_mx_simple <- tidy(m_qb_mx_simple, conf.int = TRUE, xintercept=FALSE)
td_qb_t_mx_simple$significance1 <- ifelse(as.data.frame(td_qb_t_mx_simple$p.value)<=0.01, "***","")
td_qb_t_mx_simple$significance2 <- ifelse( (as.data.frame(td_qb_t_mx_simple$p.value)>0.01&
                                       as.data.frame(td_qb_t_mx_simple$p.value)<=0.05), "**","")
td_qb_t_mx_simple$significance3 <- ifelse( (as.data.frame(td_qb_t_mx_simple$p.value)>0.05&
                                       as.data.frame(td_qb_t_mx_simple$p.value)<=0.1), "*","")
td_qb_t_mx_simple$significance <- paste(td_qb_t_mx_simple$significance1,
                                 td_qb_t_mx_simple$significance2,
                                 td_qb_t_mx_simple$significance3)
td_qb_t_mx_simple$significance1 <- NULL
td_qb_t_mx_simple$significance2 <- NULL
td_qb_t_mx_simple$significance3 <- NULL


m_qb_mx <- glm(formula2, family= quasibinomial(logit),
                        data=datos_mx,na.action=na.exclude)
td_qb_t_mx <- tidy(m_qb_mx, conf.int = TRUE, xintercept=FALSE)
td_qb_t_mx$significance1 <- ifelse(as.data.frame(td_qb_t_mx$p.value)<=0.01, "***","")
td_qb_t_mx$significance2 <- ifelse( (as.data.frame(td_qb_t_mx$p.value)>0.01&
                                              as.data.frame(td_qb_t_mx$p.value)<=0.05), "**","")
td_qb_t_mx$significance3 <- ifelse( (as.data.frame(td_qb_t_mx$p.value)>0.05&
                                              as.data.frame(td_qb_t_mx$p.value)<=0.1), "*","")
td_qb_t_mx$significance <- paste(td_qb_t_mx$significance1,
                                        td_qb_t_mx$significance2,
                                        td_qb_t_mx$significance3)
td_qb_t_mx$significance1 <- NULL
td_qb_t_mx$significance2 <- NULL
td_qb_t_mx$significance3 <- NULL


#######Spatial MX
spdat_mx <- datos_mx[,c("COUNTER","code","xcoord","ycoord","perc_poverty", 
                          "A1metro", "A2pop_density", "A3gdp_percapita",
                            "A4unemployment", "A5perc_secondary_compelte",
                            "A6hous_fem_perc", "A7income_gini", "A8pea_perc_of_totpob",
                            "X4bet_cen_Gini","X5clo_cen_Gini","X6inf_cen_Gini",
                            "X1edge_density_km","X2intersection_density_km","X3regularity","country")]

lags = 10
#spdat <- subset(spdat, spdat$metro==1)
spdat_mx <- spdat_mx[!(rowSums(is.na(spdat_mx))),]
coordinates(spdat_mx)<-~xcoord+ycoord
proj4string(spdat_mx)<-CRS("+proj=longlat +datum=NAD83")
us.nb4<-knearneigh(coordinates(spdat_mx), k=lags)
us.nb4<-knn2nb(us.nb4)
us.nb4<-make.sym.nb(us.nb4)
us.wt4<-nb2listw(us.nb4, style="W")
spdat_mx$z9_perc_perc_poverty <-lag.listw(x=us.wt4, var=(spdat_mx$perc_poverty))
spdat_mx$z4_bet_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_mx$X4bet_cen_Gini))
spdat_mx$z5_clo_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_mx$X5clo_cen_Gini))
spdat_mx$z6_inf_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_mx$X6inf_cen_Gini))
spdat_mx$z1_edge_density_km <-lag.listw(x=us.wt4, var=(spdat_mx$X1edge_density_km))
spdat_mx$z2_intersection_density_km <-lag.listw(x=us.wt4, var=(spdat_mx$X2intersection_density_km))
spdat_mx$z3_regularity <-lag.listw(x=us.wt4, var=(spdat_mx$X3regularity))

m_SAR_mx_logit <- glm(formula3, family=binomial(logit), data=spdat_mx)
m_SAR_mx_qb <- glm(formula3, family=quasibinomial(logit), data=spdat_mx)

lm.morantest(m_SAR_mx_qb, listw = us.wt4)
mx_spatial_residual_test <- lm.LMtests(m_SAR_mx_qb, listw=us.wt4, test=c("LMlag","LMerr",
                                                                             "RLMlag", "RLMerr","SARMA"))
mx_spatial_residual_test
td_SAR_qb_t_mx <- tidy(m_SAR_mx_qb, conf.int = TRUE, xintercept=FALSE)
td_SAR_qb_t_mx$significance1 <- ifelse(as.data.frame(td_SAR_qb_t_mx$p.value)<=0.01, "***","")
td_SAR_qb_t_mx$significance2 <- ifelse( (as.data.frame(td_SAR_qb_t_mx$p.value)>0.01&
                                              as.data.frame(td_SAR_qb_t_mx$p.value)<=0.05), "**","")
td_SAR_qb_t_mx$significance3 <- ifelse( (as.data.frame(td_SAR_qb_t_mx$p.value)>0.05&
                                              as.data.frame(td_SAR_qb_t_mx$p.value)<=0.1), "*","")
td_SAR_qb_t_mx$significance <- paste(td_SAR_qb_t_mx$significance1,
                                        td_SAR_qb_t_mx$significance2,
                                        td_SAR_qb_t_mx$significance3)
td_SAR_qb_t_mx$significance1 <- NULL
td_SAR_qb_t_mx$significance2 <- NULL
td_SAR_qb_t_mx$significance3 <- NULL
td_SAR_qb_t_mx$significance3 <- NULL


tables_mx <- merge( x=td_qb_t_mx_simple, y=td_qb_t_mx, all.y=T, by="term" )
tables_mx <- merge( x=tables_mx, y=td_SAR_qb_t_mx , all.y=T, by="term" )
tables_mx <- tables_mx[,c("term","estimate.x","std.error.x","significance.x",
                          "estimate.y","std.error.y","significance.y",
                          "estimate","std.error","significance")]
AIC_mx <- c("AIC",AIC(m_logit_mx_basic), NA,NA,AIC(m_logit_mx_sci),NA,NA,AIC(m_SAR_mx_logit),NA,NA) 
AIC_mx <- as.data.frame(t(AIC_mx))
colnames(AIC_mx) <- colnames(tables_mx)
n_mx <- c("Observations",nobs(m_logit_mx_basic), NA,NA,nobs(m_logit_mx_sci),NA,NA,nobs(m_SAR_mx_logit),NA,NA) 
n_mx <- as.data.frame(t(n_mx))
colnames(n_mx) <- colnames(tables_mx)

tables_mx <- rbind(tables_mx,AIC_mx)
tables_mx <- rbind(tables_mx,n_mx)


mexico_temporal <- glm(formula2, family=quasibinomial(logit), data=spdat_mx)
mx_spatial_residual_test <- lm.LMtests(mexico_temporal, listw=us.wt4, test=c("LMlag","LMerr",
                                                                            "RLMlag", "RLMerr","SARMA"))
mx_spatial_error_library <- errorsarlm(formula2, data=spdat_mx, 
                                      listw =us.wt4, tol.solve=1.0e-30)
summary(mx_spatial_error_library)
AIC(mx_spatial_error_library)

spdat_mx_no_zeros <- subset(spdat_mx, spdat_mx$perc_poverty!=0)
spdat_mx_no_zeros$y <- log(spdat_mx_no_zeros$perc_poverty/(1-(spdat_mx_no_zeros$perc_poverty)))
mx_spatial_error_model <-  errorsarlm(y ~ A1metro + A3gdp_percapita + A2pop_density + A5perc_secondary_compelte + 
                                        A7income_gini + A8pea_perc_of_totpob + X1edge_density_km + 
                                        X2intersection_density_km + X3regularity + X4bet_cen_Gini + 
                                        X5clo_cen_Gini + X6inf_cen_Gini, data=spdat_mx_no_zeros, 
                                       listw =us.wt4, tol.solve=1.0e-30)
summary(mx_spatial_error_model)




##step 1
mexico_temporal <- glm(formula2, family=quasibinomial(logit), data=spdat_mx)
mexico_temporal_predictions <- predict(mexico_temporal, type = "response")
#Step 2
#######Spatial MX
spdat_mx$Easting <- coordinates(spdat_mx)[,1]
spdat_mx$Northing <- coordinates(spdat_mx)[,2]

mexico_temporal_vgm <- variogram(log(mexico_temporal_predictions / (1 ??? mexico_temporal_predictions) ) ~ 
                                   Easting + Northing, data = spdat_mx)
plot(mexico_temporal_vgm)
#Step 3
b.exp <- 1.0
alpha.exp <- 20000
#Step 4
Set2.kmeans <- with(spdat_mx@data, data.frame(scale(Easting)+scale(Northing) ) )
spdat_mx$ClusterID <- kmeans(Set2.kmeans, 5, iter.max = 100)
n.sites <- as.numeric(table(data.Set2C$clusID) )
v.half <- diag(as.vector(p.irwgls * (1 ??? p.irwgls) / n.sites) )



##################### USA          
m_logit_usa_basic <- glm(formula1, family=binomial(logit),
                         data=datos_usa,na.action=na.exclude)
td_logit_t_usa_basic <- tidy(m_logit_usa_basic, conf.int = TRUE, xintercept=FALSE)
td_logit_t_usa_basic$significance1 <- ifelse(as.data.frame(td_logit_t_usa_basic$p.value)<=0.01, "***","")
td_logit_t_usa_basic$significance2 <- ifelse( (as.data.frame(td_logit_t_usa_basic$p.value)>0.01&
                                                 as.data.frame(td_logit_t_usa_basic$p.value)<=0.05), "**","")
td_logit_t_usa_basic$significance3 <- ifelse( (as.data.frame(td_logit_t_usa_basic$p.value)>0.05&
                                                 as.data.frame(td_logit_t_usa_basic$p.value)<=0.1), "*","")
td_logit_t_usa_basic$significance <- paste(td_logit_t_usa_basic$significance1,
                                           td_logit_t_usa_basic$significance2,
                                           td_logit_t_usa_basic$significance3)
td_logit_t_usa_basic$significance1 <- NULL
td_logit_t_usa_basic$significance2 <- NULL
td_logit_t_usa_basic$significance3 <- NULL



m_logit_usa_sci <- glm(formula2, family=binomial(logit),
                       data=datos_usa, na.action=na.exclude) 
td_logit_t_usa_sci <- tidy(m_logit_usa_sci, conf.int = TRUE, xintercept=FALSE)
td_logit_t_usa_sci$significance1 <- ifelse(as.data.frame(td_logit_t_usa_sci$p.value)<=0.01, "***","")
td_logit_t_usa_sci$significance2 <- ifelse( (as.data.frame(td_logit_t_usa_sci$p.value)>0.01&
                                               as.data.frame(td_logit_t_usa_sci$p.value)<=0.05), "**","")
td_logit_t_usa_sci$significance3 <- ifelse( (as.data.frame(td_logit_t_usa_sci$p.value)>0.05&
                                               as.data.frame(td_logit_t_usa_sci$p.value)<=0.1), "*","")
td_logit_t_usa_sci$significance <- paste(td_logit_t_usa_sci$significance1,
                                         td_logit_t_usa_sci$significance2,
                                         td_logit_t_usa_sci$significance3)
td_logit_t_usa_sci$significance1 <- NULL
td_logit_t_usa_sci$significance2 <- NULL
td_logit_t_usa_sci$significance3 <- NULL

m_qb_usa_simple <- glm(formula1, family= quasibinomial(logit),
                       data=datos_usa,na.action=na.exclude)
td_qb_t_usa_simple <- tidy(m_qb_usa_simple, conf.int = TRUE, xintercept=FALSE)
td_qb_t_usa_simple$significance1 <- ifelse(as.data.frame(td_qb_t_usa_simple$p.value)<=0.01, "***","")
td_qb_t_usa_simple$significance2 <- ifelse( (as.data.frame(td_qb_t_usa_simple$p.value)>0.01&
                                               as.data.frame(td_qb_t_usa_simple$p.value)<=0.05), "**","")
td_qb_t_usa_simple$significance3 <- ifelse( (as.data.frame(td_qb_t_usa_simple$p.value)>0.05&
                                               as.data.frame(td_qb_t_usa_simple$p.value)<=0.1), "*","")
td_qb_t_usa_simple$significance <- paste(td_qb_t_usa_simple$significance1,
                                         td_qb_t_usa_simple$significance2,
                                         td_qb_t_usa_simple$significance3)
td_qb_t_usa_simple$significance1 <- NULL
td_qb_t_usa_simple$significance2 <- NULL
td_qb_t_usa_simple$significance3 <- NULL


m_qb_usa <- glm(formula2, family= quasibinomial(logit),
                data=datos_usa,na.action=na.exclude)
td_qb_t_usa <- tidy(m_qb_usa, conf.int = TRUE, xintercept=FALSE)
td_qb_t_usa$significance1 <- ifelse(as.data.frame(td_qb_t_usa$p.value)<=0.01, "***","")
td_qb_t_usa$significance2 <- ifelse( (as.data.frame(td_qb_t_usa$p.value)>0.01&
                                        as.data.frame(td_qb_t_usa$p.value)<=0.05), "**","")
td_qb_t_usa$significance3 <- ifelse( (as.data.frame(td_qb_t_usa$p.value)>0.05&
                                        as.data.frame(td_qb_t_usa$p.value)<=0.1), "*","")
td_qb_t_usa$significance <- paste(td_qb_t_usa$significance1,
                                  td_qb_t_usa$significance2,
                                  td_qb_t_usa$significance3)
td_qb_t_usa$significance1 <- NULL
td_qb_t_usa$significance2 <- NULL
td_qb_t_usa$significance3 <- NULL

lags=15
#######Spatial usa
spdat_usa <-datos_usa[,c("COUNTER","code","xcoord","ycoord","perc_poverty", 
                            "A1metro", "A2pop_density", "A3gdp_percapita",
                            "A4unemployment", "A5perc_secondary_compelte",
                            "A6hous_fem_perc", "A7income_gini", "A8pea_perc_of_totpob",
                            "X4bet_cen_Gini","X5clo_cen_Gini","X6inf_cen_Gini",
                            "X1edge_density_km","X2intersection_density_km","X3regularity","country")]

#spdat <- subset(spdat, spdat$metro==1)
spdat_usa <- spdat_usa[!(rowSums(is.na(spdat_usa))),]
coordinates(spdat_usa)<-~xcoord+ycoord
proj4string(spdat_usa)<-CRS("+proj=longlat +datum=NAD83")
us.nb4<-knearneigh(coordinates(spdat_usa), k=lags)
us.nb4<-knn2nb(us.nb4)
us.nb4<-make.sym.nb(us.nb4)
us.wt4<-nb2listw(us.nb4, style="W")
spdat_usa$z9_perc_perc_poverty <-lag.listw(x=us.wt4, var=(spdat_usa$perc_poverty))
spdat_usa$z4_bet_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_usa$X4bet_cen_Gini))
spdat_usa$z5_clo_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_usa$X5clo_cen_Gini))
spdat_usa$z6_inf_cen_Gini <-lag.listw(x=us.wt4, var=(spdat_usa$X6inf_cen_Gini))
spdat_usa$z1_edge_density_km <-lag.listw(x=us.wt4, var=(spdat_usa$X1edge_density_km))
spdat_usa$z2_intersection_density_km <-lag.listw(x=us.wt4, var=(spdat_usa$X2intersection_density_km))
spdat_usa$z3_regularity <-lag.listw(x=us.wt4, var=(spdat_usa$X3regularity))

m_SAR_usa_logit <- glm(formula3, family=binomial(logit), data=spdat_usa)
m_SAR_usa_qb <- glm(formula3, family=quasibinomial(logit), data=spdat_usa)

a<-glm(perc_poverty ~ A1metro + A3gdp_percapita + A2pop_density + A5perc_secondary_compelte + 
      A7income_gini + A8pea_perc_of_totpob + X1edge_density_km + 
      poly(X2intersection_density_km, 2) + X3regularity + X4bet_cen_Gini + 
      X5clo_cen_Gini + X6inf_cen_Gini, family=quasibinomial(logit), data=spdat_usa)
plot_model(a, type = "pred", terms = "X2intersection_density_km")+
  labs(y="% of people in poverty", x="Bet. centrality (Gini)", title="Mexico")+ theme_bw(base_size = 27)



lm.morantest(m_SAR_usa_qb, listw = us.wt4)
usa_spatial_residual_test <- lm.LMtests(m_SAR_usa_qb, listw=us.wt4, test=c("LMlag","LMerr",
                                                                         "RLMlag", "RLMerr","SARMA"))
usa_spatial_residual_test

td_SAR_qb_t_usa <- tidy(m_SAR_usa_qb, conf.int = TRUE, xintercept=FALSE)
td_SAR_qb_t_usa$significance1 <- ifelse(as.data.frame(td_SAR_qb_t_usa$p.value)<=0.01, "***","")
td_SAR_qb_t_usa$significance2 <- ifelse( (as.data.frame(td_SAR_qb_t_usa$p.value)>0.01&
                                            as.data.frame(td_SAR_qb_t_usa$p.value)<=0.05), "**","")
td_SAR_qb_t_usa$significance3 <- ifelse( (as.data.frame(td_SAR_qb_t_usa$p.value)>0.05&
                                            as.data.frame(td_SAR_qb_t_usa$p.value)<=0.1), "*","")
td_SAR_qb_t_usa$significance <- paste(td_SAR_qb_t_usa$significance1,
                                      td_SAR_qb_t_usa$significance2,
                                      td_SAR_qb_t_usa$significance3)
td_SAR_qb_t_usa$significance1 <- NULL
td_SAR_qb_t_usa$significance2 <- NULL
td_SAR_qb_t_usa$significance3 <- NULL
td_SAR_qb_t_usa$significance3 <- NULL


tables_usa <- merge( x=td_qb_t_usa_simple, y=td_qb_t_usa, all.y=T, by="term" )
tables_usa <- merge( x=tables_usa, y=td_SAR_qb_t_usa , all.y=T, by="term" )
tables_usa <- tables_usa[,c("term","estimate.x","std.error.x","significance.x",
                            "estimate.y","std.error.y","significance.y",
                            "estimate","std.error","significance")]
AIC_usa <- c("AIC",AIC(m_logit_usa_basic), NA,NA,AIC(m_logit_usa_sci),NA,NA,AIC(m_SAR_usa_logit),NA,NA) 
AIC_usa <- as.data.frame(t(AIC_usa))
colnames(AIC_usa) <- colnames(tables_usa)
n_usa <- c("Observations",nobs(m_logit_usa_basic), NA,NA,nobs(m_logit_usa_sci),
           NA,NA,nobs(m_SAR_usa_logit),NA,NA) 
n_usa <- as.data.frame(t(n_usa))
colnames(n_usa) <- colnames(tables_usa)
tables_usa <- rbind(tables_usa,AIC_usa)
tables_usa <- rbind(tables_usa,n_usa)

write.csv(tables_usa,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\04-Results\\Tables\\Results_usa.csv", row.names = FALSE)
write.csv(tables_mx,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\04-Results\\Tables\\Results_mx.csv", row.names = FALSE)

datos_mx_clean <- subset(datos_mx, !is.na(datos_mx$perc_poverty))

#rodCT <- ctree(perc_poverty ~ X1edge_density_km + X2intersection_density_km + X3regularity + 
#                 X4bet_cen_Gini + X5clo_cen_Gini + X6inf_cen_Gini,data = datos_mx_clean,
#                controls=ctree_control(testtype="Teststatistic"))
#plot(rodCT)


mapping_stata <- rbind(mapping_mx,mapping_usa)
mapping_stata_spatial <- rbind(as.data.frame(spdat_mx),as.data.frame(spdat_usa))
write.dta(mapping_mx,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\MX.dta")
write.dta(mapping_usa,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\USA.dta")
write.dta(as.data.frame(spdat_usa) ,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\USA_spatial.dta")
write.dta(as.data.frame(spdat_mx) ,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\MX_spatial.dta")

write.dta(mapping_stata ,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\MX_and_USA.dta")
write.dta(mapping_stata_spatial ,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\MX_and_USA_spatial.dta")


setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\04-Results")
png("Effect_intersection_densty_US.png", width = 8, height = 8, units = 'in', res = 300)
a<- plot_model(m_SAR_usa_qb, type = "pred", terms = c("X2intersection_density_km [all]"))+
  labs(y="% of people in poverty", x="Intersection density", title="US")+ scale_x_continuous(limits=c(0, 80)) +
  theme_bw(base_size = 27)
print(a)
dev.off()
graphics.off()

png("Effect_intersection_densty_MX.png", width = 8, height = 8, units = 'in', res = 300)
a<- plot_model(m_SAR_mx_qb, type = "pred", terms = c("X2intersection_density_km [all]"))+
  labs(y="% of people in poverty", x="Intersection density", title="Mexico")+ scale_x_continuous(limits=c(0, 80)) +
   theme_bw(base_size = 27)
print(a)
dev.off()
graphics.off()

png("Effect_bet_cent_USA.png", width = 8, height = 8, units = 'in', res = 300)
a  <- plot_model(m_SAR_usa_qb, type = "pred", terms = c("X4bet_cen_Gini [all]"))+
  labs(y="% of people in poverty", x="Bet. centrality (Gini)", title="US")+ scale_x_continuous(limits=c(0.6, 0.9)) +
  theme_bw(base_size = 27)
print(a)
dev.off()
graphics.off()

png("Effect_bet_cent_MX.png", width = 8, height = 8, units = 'in', res = 300)
a <- plot_model(m_SAR_mx_qb, type = "pred", terms = c("X4bet_cen_Gini [all]"))+
  labs(y="% of people in poverty", x="Bet. centrality (Gini)", title="Mexico")+ scale_x_continuous(limits=c(0.4, 0.8)) +
  theme_bw(base_size = 27)
print(a)
dev.off()
graphics.off()


#p1 <- mapping[,c("country","income_gini","node_density_km")]
#p1$var <- "Nodes per sq. km"

p2 <- datos_figures[,c("country","perc_poverty","edge_density_km")]
p2$var <- "Street density"
p3 <- datos_figures[,c("country","perc_poverty","intersection_density_km")]
p3$var <- "Intersection density"
p4 <- datos_figures[,c("country","perc_poverty","regularity")]
p4$var <- "Regularity"
p5 <- datos_figures[,c("country","perc_poverty","bet_cen_Gini")]
p5$var <- "Bet. centrality (Gini)"
p6 <- datos_figures[,c("country","perc_poverty","clo_cen_Gini")]
p6$var <- "Clo. centrality (Gini)"
p7 <- datos_figures[,c("country","perc_poverty","inf_cen_Gini")]
p7$var <- "Inf. centrality (Gini)"
#p8 <- mapping[,c("country","income_gini","pagerank_Gini")]
#p8$var <- "PageRank (Gini)"

#colnames(p1) <- c("Country","gini", "var","varname")
colnames(p2) <- c("Country","gini", "var","varname")
colnames(p3) <- c("Country","gini", "var","varname")
colnames(p4) <- c("Country","gini", "var","varname")
colnames(p5) <- c("Country","gini", "var","varname")
colnames(p6) <- c("Country","gini", "var","varname")
colnames(p7) <- c("Country","gini", "var","varname")
#colnames(p8) <- c("Country","gini", "var","varname")



p <- rbind(p2,p3,p4,p5,p6,p7)#,p8)
p$varname <- as.factor(p$varname) 
p$varname  <- factor(p$varname , levels = c("Street density", 
                                            "Intersection density", "Regularity",
                                            "Bet. centrality (Gini)","Clo. centrality (Gini)",
                                            "Inf. centrality (Gini)")) 
p$Country <- ifelse(p$Country=="Mexico","Mexico (2,432 municipalities)","US (3,106 counties)")
p$Country  <- factor(p$Country , levels = c("US (3,106 counties)","Mexico (2,432 municipalities)")) 

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("Scatter_plot_poverty.png", width = 12, height = 8, units = 'in', res = 300)
a <- ggplot(data=p, aes(x=var, y=gini))+
  ylab("Share of people in poverty") + xlab("Street network indicators")+ 
  geom_point(size=1, shape=23, aes(fill=Country)) +  facet_wrap(~varname, scales = "free")+
  theme_bw(base_size = 25) +  theme(legend.position = "bottom")#"none")
print(a)
dev.off()
graphics.off()

rm(p,p2,p3,p4,p5,p6,p7)





###
experimental <- subset(mapping_mx_metro, mapping_mx_metro$POB_TOT >300000 )


################### Fresno  
fresno_e <- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes\\304\\edges\\edges.dbf")
fresno_e$rowid <- rownames(fresno_e)
fresno_n <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\simplify\\304.csv", header = TRUE )
fresno_n <- fresno_n[,c("index_col","inf_centrality")]

fresno_n_from <- fresno_n
colnames(fresno_n_from) <- c("from","inf_cent_from")
fresno_n_to <- fresno_n
colnames(fresno_n_to) <- c("to","inf_cent_to")

fresno_e <- merge(x=fresno_e, y=fresno_n_from, by="from", all.x=TRUE)
fresno_e <- merge(x=fresno_e, y=fresno_n_to, by="to", all.x=TRUE)
fresno_e$inf_cent_avg <- (fresno_e$inf_cent_from + fresno_e$inf_cent_to)/2

fresno_n <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\simplify\\304.csv", header = TRUE )
fresno_n <- fresno_n[,c("index_col","bet_centrality_l")]

fresno_n_from <- fresno_n
colnames(fresno_n_from) <- c("from","bet_cent_from")
fresno_n_to <- fresno_n
colnames(fresno_n_to) <- c("to","bet_cent_to")

fresno_e <- merge(x=fresno_e, y=fresno_n_from, by="from", all.x=TRUE)
fresno_e <- merge(x=fresno_e, y=fresno_n_to, by="to", all.x=TRUE)
fresno_e$bet_cent_avg <- (fresno_e$bet_cent_from + fresno_e$bet_cent_to)/2


fresno_e <- fresno_e[order(as.numeric(fresno_e$rowid)),]

write.dbf(fresno_e, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes\\304\\edges\\edges.dbf")
rm(fresno_e, fresno_n, fresno_n_from, fresno_n_to)

fresno_tracts <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\Laredo_tracts.csv", header = TRUE)
fresno_tracts$GEO_ID <- fresno_tracts$tract_id
fresno_tracts$DP03_0119PE <- fresno_tracts$avg_PovertyRate


fresno_tracts <- fresno_tracts[,c("GEO_ID","DP03_0119PE")]
#fresno_tracts <- fresno_tracts[-1, ]

fresno_tracts_shape <- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\Laredo.dbf")
fresno_tracts_shape$rowid <- rownames(fresno_tracts_shape)
fresno_tracts_shape$GEO_ID <- as.factor(as.character(fresno_tracts_shape$GEOID))

fresno_tracts$GEO_ID <- gsub("1400000US", "",as.character(fresno_tracts$GEO_ID))
fresno_tracts$GEO_ID <- as.factor(as.character(fresno_tracts$GEO_ID))

fresno_tracts_shape <- merge(x=fresno_tracts_shape, y= fresno_tracts, by="GEO_ID", all.x=TRUE)
fresno_tracts_shape$GEO_ID <- NULL

fresno_tracts_shape$Poverty <- as.numeric(as.character(fresno_tracts_shape$DP03_0119PE)) 


fresno_tracts_shape$Poverty <- fresno_tracts_shape$Poverty *100
fresno_tracts_shape$p1 <- ifelse(fresno_tracts_shape$Poverty <= 18, "1.[0,18]", "" )
fresno_tracts_shape$p2 <- ifelse( (fresno_tracts_shape$Poverty > 18 &
                                   fresno_tracts_shape$Poverty <= 34), "2.(18,34]", "" )
fresno_tracts_shape$p3 <- ifelse( (fresno_tracts_shape$Poverty > 34 &
                                   fresno_tracts_shape$Poverty <= 50), "3.(34,50]", "" )
fresno_tracts_shape$p4 <- ifelse( (fresno_tracts_shape$Poverty > 50 &
                                   fresno_tracts_shape$Poverty <= 70), "4.(50,70]", "" )
fresno_tracts_shape$p5 <- ifelse(fresno_tracts_shape$Poverty > 70, "5.(70,100]", "" )
fresno_tracts_shape$pov_l <- paste(fresno_tracts_shape$p1 ,fresno_tracts_shape$p2, fresno_tracts_shape$p3,
                                   fresno_tracts_shape$p4, fresno_tracts_shape$p5, sep="")
fresno_tracts_shape$p1 <- NULL
fresno_tracts_shape$p2 <- NULL
fresno_tracts_shape$p3 <- NULL
fresno_tracts_shape$p4 <- NULL
fresno_tracts_shape$p5 <- NULL

fresno_tracts_shape$pov_l <- gsub("NANANANANA", "NA",as.character(fresno_tracts_shape$pov_l))


fresno_tracts_shape <- fresno_tracts_shape[order(as.numeric(as.character(fresno_tracts_shape$rowid))),]
write.dbf(fresno_tracts_shape, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\Laredo.dbf")





################### Orizaba
pablo_e <- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes\\45\\edges\\edges.dbf")
pablo_e$rowid <- rownames(pablo_e) 
pablo_n <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_MAs\\Processed_results\\GPU\\corrected_Aug_2019\\simplify\\45.csv", header = TRUE )
pablo_n <- pablo_n[,c("index_col","inf_centrality")]

pablo_n_from <- pablo_n
colnames(pablo_n_from) <- c("from","inf_cent_from")
pablo_n_to <- pablo_n
colnames(pablo_n_to) <- c("to","inf_cent_to")

pablo_e <- merge(x=pablo_e, y=pablo_n_from, by="from", all.x=TRUE)
pablo_e <- merge(x=pablo_e, y=pablo_n_to, by="to", all.x=TRUE)
pablo_e$inf_cent_avg <- (pablo_e$inf_cent_from + pablo_e$inf_cent_to)/2

pablo_n <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_MAs\\Processed_results\\GPU\\corrected_Aug_2019\\simplify\\45.csv", header = TRUE )
pablo_n <- pablo_n[,c("index_col","closeness_centrality")]

pablo_n_from <- pablo_n
colnames(pablo_n_from) <- c("from","clo_cent_from")
pablo_n_to <- pablo_n
colnames(pablo_n_to) <- c("to","clo_cent_to")

pablo_e <- merge(x=pablo_e, y=pablo_n_from, by="from", all.x=TRUE)
pablo_e <- merge(x=pablo_e, y=pablo_n_to, by="to", all.x=TRUE)
pablo_e$clo_cent_avg <- (pablo_e$clo_cent_from + pablo_e$clo_cent_to)/2
pablo_e <- pablo_e[order(as.numeric(as.character(pablo_e$rowid))),]

write.dbf(pablo_e, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes\\45\\edges\\edges.dbf")

rm(pablo_e, pablo_n, pablo_n_from, pablo_n_to)

pablo_tracts <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\Agebs_poverty_levels.csv", 
                         header = TRUE, colClasses=c("aegb_code"="character"))
pablo_tracts <- pablo_tracts[,c("aegb_code","poverty_level")]

pablo_tracts_shape <- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\AGEB_urb_2010_5.dbf")
pablo_tracts_shape$rowid <- rownames(pablo_tracts_shape)
pablo_tracts_shape$CVEGEO <- as.factor(as.character(pablo_tracts_shape$CVEGEO))

pablo_tracts$CVEGEO <- as.factor(as.character(pablo_tracts$aegb_code))

pablo_tracts_shape <- merge(x=pablo_tracts_shape, y= pablo_tracts, by="CVEGEO", all.x=TRUE)
pablo_tracts_shape$aegb_code <- NULL

pablo_tracts_shape$Poverty <- as.character(pablo_tracts_shape$poverty_level) 
pablo_tracts_shape$p1 <- ifelse(pablo_tracts_shape$Poverty == "[ 0, 18]", "1.[ 0, 18]", "" )
pablo_tracts_shape$p2 <- ifelse(pablo_tracts_shape$Poverty == "(18, 34]", "2.(18, 34]", "" )
pablo_tracts_shape$p3 <- ifelse(pablo_tracts_shape$Poverty == "(34, 50]", "3.(34, 50]", "" )
pablo_tracts_shape$p4 <- ifelse(pablo_tracts_shape$Poverty == "(50, 70]", "4.(50, 70]", "" )
pablo_tracts_shape$p5 <- ifelse(pablo_tracts_shape$Poverty == "(70, 100]", "5.(70, 100]", "" )
pablo_tracts_shape$pov_l <- paste(pablo_tracts_shape$p1, pablo_tracts_shape$p2, pablo_tracts_shape$p3,
                                   pablo_tracts_shape$p4, pablo_tracts_shape$p5, sep="")

pablo_tracts_shape$p1 <- NULL
pablo_tracts_shape$p2 <- NULL
pablo_tracts_shape$p3 <- NULL
pablo_tracts_shape$p4 <- NULL
pablo_tracts_shape$p5 <- NULL

pablo_tracts_shape$pov_l <- gsub("NANANANANA", "NA",as.character(pablo_tracts_shape$pov_l))
pablo_tracts_shape$pov_l <- ifelse(pablo_tracts_shape$pov_l=="","NA",pablo_tracts_shape$pov_l)


pablo_tracts_shape <- pablo_tracts_shape[order(as.numeric(pablo_tracts_shape$rowid)),]
write.dbf(pablo_tracts_shape, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\AGEB_urb_2010_5.dbf")





############### Laredo New poverty levels matching with MX

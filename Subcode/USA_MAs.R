
mapping_usa_metro_agg <- mapping[,c("ZM","POB_TOT", "perc_poverty","gdp_percapita","area")]
mapping_usa_metro_agg <- subset(mapping_usa_metro_agg, !is.na(mapping_usa_metro_agg$ZM))

mapping_usa_metro_agg$pp_in_poverty <-  (as.numeric(as.character(mapping_usa_metro_agg$perc_poverty)) *mapping_usa_metro_agg$POB_TOT)/100
mapping_usa_metro_agg$ZM <- as.factor(mapping_usa_metro_agg$ZM)
mapping_usa_metro_agg$gdp <- as.numeric(as.character(mapping_usa_metro_agg$gdp_percapita)) * mapping_usa_metro_agg$POB_TOT
mapping_usa_metro_agg$gdp_percapita <- NULL
mapping_usa_metro_agg$perc_poverty <- NULL

mapping_usa_metro_agg <- aggregate(.~ ZM, data=mapping_usa_metro_agg, FUN=sum)
mapping_usa_metro_agg$gdp_percapita <- mapping_usa_metro_agg$gdp/ mapping_usa_metro_agg$POB_TOT
mapping_usa_metro_agg$perc_poverty <- mapping_usa_metro_agg$pp_in_poverty / mapping_usa_metro_agg$POB_TOT

names_mas <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes\\Codes_MAs_USA.csv",header = TRUE)
names_mas$ZM <- names_mas$NAME
names_mas$COUNTER <- rownames(names_mas)
names_mas <- names_mas[,c("COUNTER","ZM")]

mapping_usa_metro_agg <- merge(x=mapping_usa_metro_agg, y=names_mas, by="ZM")
mapping_usa_metro_agg$ZM <- mapping_usa_metro_agg$COUNTER
################Street connectivity
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, nrows = 1, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
master2 <- do.call("rbind",filelist)
master2$COUNTER <- as.character(master2$COUNTER)
################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\simplify_basic_statistics")
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
area <- mapping_usa_metro_agg[,c("area","COUNTER")]
master2 <- merge(x=master2,y=area, by="COUNTER", all.x=TRUE)
master2$node_density_km <- master2$n /master2$area
master2$intersection_density_km <- master2$intersection_count / master2$area
master2$edge_density_km <- (master2$edge_length_total/1000) / master2$area
master2$street_density_km <- master2$street_length_total / master2$area
master2$street_segments_by_intersections <- master2$street_segments_count/master2$intersection_count
master2 <- master2[,c("COUNTER","node_density_km","edge_density_km","intersection_density_km","regularity","area")]


#######Adding Gini
master <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\USA_MAs_Gini\\Gini_USA_MAs.csv",header = TRUE)
master2$COUNTER <- as.character(master2$COUNTER)
master <- merge(x=master, y=master2, by="COUNTER", all.y=T)
master$ZM <- as.factor(as.character(master$COUNTER))
mapping_usa_metro_agg <- merge(x=master, y=mapping_usa_metro_agg, by="ZM")
mapping_usa_metro_agg$COUNTER.y <- NULL
mapping_usa_metro_agg$area.y <- NULL
names(mapping_usa_metro_agg)[names(mapping_usa_metro_agg) == "COUNTER.x"] <- "COUNTER"
names(mapping_usa_metro_agg)[names(mapping_usa_metro_agg) == "area.x"] <- "area"
mapping_usa_metro_agg$pop_density <- mapping_usa_metro_agg$POB_TOT/ mapping_usa_metro_agg$area
mapping_usa_metro_agg <- merge(x=mapping_usa_metro_agg, y=names_mas, by="COUNTER")

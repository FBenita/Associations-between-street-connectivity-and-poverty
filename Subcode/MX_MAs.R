
mapping_mx_metro_agg <- mapping[,c("ZM","POB_TOT", "perc_poverty","VACB_millones",
                                   "hous_total","hous_fem",
                                   "p_15_y_mas","perc_secondary_compelte")]
mapping_mx_metro_agg <- subset(mapping_mx_metro_agg, !is.na(mapping_mx_metro_agg$ZM))
mapping_mx_metro_agg$pp_in_poverty <-  (as.numeric(as.character(mapping_mx_metro_agg$perc_poverty)) *mapping_mx_metro_agg$POB_TOT)/100
mapping_mx_metro_agg$p_15_y_mas_sec_complete <- (mapping_mx_metro_agg$perc_secondary_compelte  * mapping_mx_metro_agg$p_15_y_mas )/100
mapping_mx_metro_agg <- mapping_mx_metro_agg[,c("ZM","POB_TOT", "pp_in_poverty","VACB_millones",
                                                "hous_total","hous_fem",
                                                "p_15_y_mas","p_15_y_mas_sec_complete")]
mapping_mx_metro_agg$ZM <- as.factor(mapping_mx_metro_agg$ZM)
mapping_mx_metro_agg <- aggregate(.~ ZM, data=mapping_mx_metro_agg, FUN=sum,  na.action = na.omit )
mapping_mx_metro_agg$gdp_percapita <- ( (mapping_mx_metro_agg$VACB_millones / 1.169367909 )*(1000)*0.07841 )/ mapping_mx_metro_agg$POB_TOT
mapping_mx_metro_agg$perc_secondary_compelte <- mapping_mx_metro_agg$p_15_y_mas_sec_complete/ mapping_mx_metro_agg$p_15_y_mas
mapping_mx_metro_agg$perc_poverty <- mapping_mx_metro_agg$pp_in_poverty / mapping_mx_metro_agg$POB_TOT


################Street connectivity
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_MAs\\Processed_results\\GPU\\corrected_Aug_2019\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, nrows = 1, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER"=ID, SIMPLIFY=F)
master2 <- do.call("rbind",filelist)
master2$COUNTER <- as.character(master2$COUNTER)
################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_MAs\\Processed_results\\GPU\\corrected_Aug_2019\\simplify_basic_statistics")
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

area <- read.csv("2020\\03-Data\\AREAS_Metropolitan_areas\\MX_area_MAs.csv")
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
master <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\MX_MAs_Gini\\Gini_MX_MAs.csv",header = TRUE)
master <- merge(x=master, y=master2, by="COUNTER", all.y=T)
master$ZM <- as.factor(as.character(master$COUNTER))
mapping_mx_metro_agg <- merge(x=master, y=mapping_mx_metro_agg, by="ZM")
mapping_mx_metro_agg$pop_density <- mapping_mx_metro_agg$POB_TOT/ mapping_mx_metro_agg$area



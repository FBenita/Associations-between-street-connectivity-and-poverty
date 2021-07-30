library("foreign")
library("readxl")
library(readr)
library(ggplot2)
#library(ggpval)
library(ggpubr)
library(plyr)
library(dplyr)
library(reshape)
library(raster)
library("sf")
library(rgdal)

cat("\014") 
rm(list = ls())
graphics.off()

a1 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract.xlsx")
a2 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(1).xlsx")
a3 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(2).xlsx")
a4 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(3).xlsx")
a5 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(4).xlsx")
a6 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(5).xlsx")
a7 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(6).xlsx")
a8 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(7).xlsx")
a9 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(8).xlsx")
a10 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(9).xlsx")
a11 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(10).xlsx")
a12 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(11).xlsx")
a13 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(12).xlsx")
a14 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(13).xlsx")
a15 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(14).xlsx")
a16 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(15).xlsx")
a17 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(16).xlsx")
a18 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(17).xlsx")
a19 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(18).xlsx")
a20 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(19).xlsx")
a21 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(20).xlsx")
a22 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(21).xlsx")
a23 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(22).xlsx")
a24 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(23).xlsx")
a25 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(24).xlsx")
a26 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(25).xlsx")
a27 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(26).xlsx")
a28 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(27).xlsx")
a29 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(28).xlsx")
a30 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(29).xlsx")
a31 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(30).xlsx")
a32 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(31).xlsx")
a33 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(32).xlsx")
a34 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(33).xlsx")
a35 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(34).xlsx")
a36 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(35).xlsx")
a37 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(36).xlsx")
a38 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(37).xlsx")
a39 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(38).xlsx")
a40 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban\\qct_data_extract(39).xlsx")

b1 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract.xlsx")
b2 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(1).xlsx")
b3 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(2).xlsx")
b4 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(3).xlsx")
b5 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(4).xlsx")
b6 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(5).xlsx")
b7 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(6).xlsx")
b8 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(7).xlsx")
b9 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(8).xlsx")
b10 <- read_excel("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Rural\\qct_data_extract(9).xlsx")

areas <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,
               a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,
               b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

metro <- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\tl_2017_us_cbsa.dbf")
metro <- subset(metro, metro$LSAD=="M1")
metro$COUNTER <- as.character(seq(1,389))
metro <- metro[,c("COUNTER","NAME","NAMELSAD","GEOID")]
colnames(metro) <- c("COUNTER","NAME","NAMELSAD","cbsa")

areas <- merge(x=areas, y=metro, by="cbsa",all.x = T)


areas$perc_poverty <- as.numeric(areas$avg_PovertyRate)
areas$perc_poverty <- ifelse(areas$p0010001==0, NA, areas$perc_poverty)

areas$Poverty <- as.numeric(areas$perc_poverty) *100
areas$p1 <- ifelse(areas$Poverty <= 18, "1.[ 0,18]", "" )
areas$p2 <- ifelse( (areas$Poverty > 18 &
                       areas$Poverty <= 34), "2.(18,34]", "" )
areas$p3 <- ifelse( (areas$Poverty > 34 &
                       areas$Poverty <= 50), "3.(34,50]", "" )
areas$p4 <- ifelse( (areas$Poverty > 50 &
                       areas$Poverty <= 70), "4.(50,70]", "" )
areas$p5 <- ifelse(areas$Poverty > 70, "5.(70,100]", "" )
areas$p6 <- ifelse(is.na(areas$Poverty), "NA", "" )

areas$pov_l <- paste(areas$p1 ,areas$p2, areas$p3,
                     areas$p4, areas$p5, areas$p6, sep="")
areas$p1 <- NULL
areas$p2 <- NULL
areas$p3 <- NULL
areas$p4 <- NULL
areas$p5 <- NULL
areas$p6 <- NULL
areas$pov_l <- gsub("NANANANANANA", "NA",as.character(areas$pov_l))

rm(list=setdiff(ls(), c("areas","metro")))

length(unique(areas$tract_id))
areas[duplicated(areas$tract_id),]
#Remove qct_id
#230190090002
#230190080012
#250092141002
#330150625002
#330110225002
#250039351002
#250277022002
areas <- subset(areas, (areas$qct_id!=230190090002& areas$qct_id!=230190080012& 
                   areas$qct_id!=250092141002& areas$qct_id!=330150625002&
                   areas$qct_id!=330110225002& areas$qct_id!=250039351002& areas$qct_id!=250277022002))
#write.csv(areas, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\Urban_cencus_tracts_US_poverty_scales_matched_MX.csv",row.names = F)




t<- read.dbf("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Census_Tracts\\Census_Tracts.dbf")
t$POV_L <- NULL
t$POVERTY <- NULL
t$CBSA <- NULL
t$POP <- NULL

poverty <- areas[c("cbsa","tract_id","p0010001","Poverty","pov_l")]
colnames(poverty) <- c("cbsa","GEOID","pop","Poverty","pov_l")

t$rowid <- rownames(t)
t <- merge(x=t, y=poverty, by="GEOID", all.x=T)
t<-t[order(as.numeric(t$rowid)),]
t$rowid <- NULL

#write.dbf(t,"C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Census_Tracts\\Census_Tracts.dbf")

rm(t)
t_map = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Census_Tracts\\Census_Tracts.shp"
t_map = readOGR(t_map, layer = basename(strsplit(t_map, "\\.")[[1]])[1]) 

files <- list.files(path = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\MAs_US\\simplify", pattern = "*.csv", full.names = T)
tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
tbl$COUNTER <- gsub("C:\\\\Users\\\\franc\\\\Dropbox\\\\Francisco\\\\Papers 2018\\\\OSMNX\\\\US_County\\\\MAs_US\\\\simplify/", "", tbl$id)
tbl$COUNTER2 <- gsub(".csv", "", tbl$COUNTER)
tbl$COUNTER <- tbl$COUNTER2

tbl <- tbl[,c("COUNTER","x","y","degree_centrality","bet_centrality_l","closeness_centrality","inf_centrality")]

coordinates(tbl)<-~x+y
proj4string(tbl)<-CRS("+proj=longlat +datum=NAD83")
tbl <- spTransform(tbl, CRS(proj4string(t_map)))

res_5 <- over(tbl, t_map)
res_5 <- mutate(res_5, id = rownames(res_5))

res6 <- res_5[,c("GEOID","id")]
tbl_df <- as.data.frame(tbl)
tbl_df <- cbind(tbl_df,res6)
tbl_df$rowid <- NULL
tbl_df <-  aggregate(list(deg_cen=tbl_df$degree_centrality, 
                          bet_cen=tbl_df$bet_centrality_l, 
                          clos_cen=tbl$closeness_centrality,
                          inf_cen=tbl$inf_centrality), by=list(GEOID=tbl_df$GEOID), FUN=mean)
tbl_df$GEOID <- as.character(tbl_df$GEOID)
poverty1 <- merge(x=poverty, y=tbl_df, by="GEOID",all.x=TRUE)
metro$cbsa <- as.character(metro$cbsa)
poverty1<- merge(x=poverty1, y=metro, by="cbsa", all.x=TRUE)

#write.csv(poverty1, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\poverty_centrality_all_tracts_scales_matched_MX.csv", row.names = FALSE)
poverty1 <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Shapes_cencus_tract\\All_tracts\\poverty_centrality_all_tracts_scales_matched_MX.csv",
                     header = TRUE, colClasses=c("cbsa"="character","GEOID"="character"))

poverty1_metro <- subset(poverty1, poverty1$COUNTER=="304")
poverty1_metro <- poverty1_metro[,c("COUNTER","NAMELSAD","pov_l","inf_cen")]
colors <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\colors_poverty.csv", header = TRUE)
colors$pov_l <- as.character(colors$pov_l)
poverty1_metro$pov_l <- as.character(poverty1_metro$pov_l)
poverty1_metro <- merge(x=poverty1_metro, y=colors, by="pov_l", all.x=TRUE)
poverty1_metro$pov_l <- as.factor(poverty1_metro$pov_l)
poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
poverty1_metro$color_code <- as.factor(poverty1_metro$color_code)
poverty1_metro <- melt(poverty1_metro)

sd <- as.data.frame(
  c( sd(subset(poverty1_metro, poverty1_metro$pov_l=="1.[ 0,18]")$value),
     sd(subset(poverty1_metro, poverty1_metro$pov_l=="2.(18,34]")$value),
     sd(subset(poverty1_metro, poverty1_metro$pov_l=="3.(34,50]")$value),
     sd(subset(poverty1_metro, poverty1_metro$pov_l=="4.(50,70]")$value))
    # sd(subset(poverty1_metro, poverty1_metro$pov_l=="5.(70,100]")$value))
)
colnames(sd) <- c("sd")
sd$pov_l <- as.factor(c("1.[ 0,18]","2.(18,34]","3.(34,50]","4.(50,70]"))
sd$color_code <- as.factor(c("#edf8fb","#c7dceb", "#a6bbda", "#8a7cba"))
#sd$pov_l <- as.factor(c("1.[ 0,10]","2.(10,20]","3.(20,30]","4.(30,40]","5.(40,100]"))
#sd$color_code <- as.factor(c("#edf8fb","#c7dceb", "#a6bbda", "#8a7cba","#87489f"))
#sd$color_code <- as.factor(c("white","white", "white", "white","white"))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("Boxplot_Laredo_inf_cen.png", width = 8, height = 8, units = 'in', res = 300)
a <- ggplot(poverty1_metro, aes(x=as.factor(pov_l), y=value, fill= color_code)) + geom_boxplot() + 
 # geom_label(data = sd, aes(x = pov_l, y = 0, fill=color_code,
 #                          label = paste( "\nStd. dev.: ", round(sd, 4))))+
  scale_fill_manual( values=c("#8a7cba", "#a6bbda", "#c7dceb", "#edf8fb")) +
  labs(title="Laredo metro area, US", subtitle="(61 census tracts)") + 
  ylab("Avg. information centrality") + xlab("% of people below the poverty line")+ 
  stat_compare_means(method = "kruskal.test", size=8)+
  theme_bw(base_size = 24)+  theme(legend.position = "none")# +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
a
print(a)
dev.off()
graphics.off()

summ <- poverty1_metro %>% 
  group_by(as.character(pov_l)) %>% 
  summarize(mean = mean(value), median = median(value), sd = sd(value))


aa <- as.numeric(poverty1$COUNTER)
bb <- subset(poverty1, poverty1$Poverty>70)

poverty1_original <- poverty1
poverty1 <- subset(poverty1_original, poverty1_original$COUNTER!="105") # all have level 1.[ 0,18]
poverty1 <- subset(poverty1, poverty1$COUNTER!="181") # one tract h ave  4.(50,470]
poverty1$COUNTER_original <- poverty1$COUNTER
poverty1$COUNTER_new <- as.numeric(as.factor(poverty1$COUNTER))
poverty1$COUNTER <- as.character(poverty1$COUNTER_new)

bet_cen <- c()
for (i in 1:387){ # Have removed 2 MAs
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")]
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="bet_cen")) 
  bet_cen[i] <- value$p.value
}

inf_cen <- c()
for (i in 1:387){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")]
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #deg
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="inf_cen")) 
  inf_cen[i] <- value$p.value
}

clos_cen <- c()
for (i in 1:387){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")]
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="clos_cen")) 
  clos_cen[i] <- value$p.value
}


################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\US_County\\Census_Tracts\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER_FILE_ID"=ID, SIMPLIFY=F)
inter <- do.call("rbind",filelist)
inter$COUNTER_FILE_ID <- as.character(inter$COUNTER_FILE_ID)
inter$X <- sequence(rle(inter$COUNTER)$lengths)
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
inter_aux <- merge(x=inter3,y=inter4, by="COUNTER", all.x=TRUE)
t_map_df <- as.data.frame(t_map)
t_map_df$COUNTER <- seq(1:length(t_map_df$GEOID ))
t_map_df <- t_map_df[,c("COUNTER","GEOID")]

#t_map_df <- areas
#t_map_df <- t_map_df[,c("COUNTER","tract_id")]
#colnames(t_map_df) <- c("COUNTER","GEOID")


t_map_df <- merge(x=t_map_df, y=inter_aux, by="COUNTER", all.x=TRUE)
inter_aux2 <- subset(inter, inter$X==1)
inter_aux2 <- inter_aux2[,c("COUNTER","edge_density_km", "intersection_density_km")]
t_map_df <- merge(x=t_map_df, y=inter_aux2, by="COUNTER", all.x=TRUE)
t_map_df$COUNTER <- NULL
t_map_df$proportion_4ormore_intersections[is.na(t_map_df$proportion_4ormore_intersections)] <- 0
t_map_df$proportion_3orless_intersections[is.na(t_map_df$proportion_3orless_intersections)] <- 0
t_map_df$regularity <- (t_map_df$proportion_4ormore_intersections-t_map_df$proportion_3orless_intersections) / (t_map_df$proportion_4ormore_intersections+t_map_df$proportion_3orless_intersections)
t_map_df$GEOID <- as.character(t_map_df$GEOID)
poverty1 <- merge(x=poverty1, y=t_map_df, by="GEOID", all.x=T)

edge_density_km <- c()
for (i in 1:387){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="edge_density_km")) 
  edge_density_km[i] <- value$p.value
}

intersection_density_km <- c()
for (i in 1:387){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="intersection_density_km")) 
  intersection_density_km[i] <- value$p.value
}

regularity <- c()
for (i in 1:387){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="regularity")) 
  regularity[i] <- value$p.value
}



mex_test <- as.data.frame(cbind(inf_cen, bet_cen, clos_cen, edge_density_km, intersection_density_km, regularity))
mex_test$inf_cen_b <- ifelse(mex_test$inf_cen<=0.05, "Reject","Not reject")
mex_test$bet_cen_b <- ifelse(mex_test$bet_cen<=0.05, "Reject","Not reject")
mex_test$clos_cen_b <- ifelse(mex_test$clos_cen<=0.05, "Reject","Not reject")
mex_test$edge_density_km_b <- ifelse(mex_test$edge_density_km<=0.05, "Reject","Not reject")
mex_test$intersection_density_km_b <- ifelse(mex_test$intersection_density_km<=0.05, "Reject","Not reject")
mex_test$regularity_b <- ifelse(mex_test$regularity<=0.05, "Reject","Not reject")
mex_test$regularity_b <- ifelse(is.na(mex_test$regularity_b), "Not reject", mex_test$regularity_b)


mex_test <- mex_test[,c("inf_cen_b","bet_cen_b","clos_cen_b","edge_density_km_b",
                        "intersection_density_km_b","regularity_b")]
mex_test$inf_cen_b <- as.factor(mex_test$inf_cen_b)
mex_test$bet_cen_b <- as.factor(mex_test$bet_cen_b)
mex_test$clos_cen_b <- as.factor(mex_test$clos_cen_b)
mex_test$edge_density_km_b <- as.factor(mex_test$edge_density_km_b)
mex_test$intersection_density_km_b <- as.factor(mex_test$intersection_density_km_b)
mex_test$regularity_b <- as.factor(mex_test$regularity_b)

a <- as.data.frame(mex_test$inf_cen_b)
a$varname <- "Inf. cent."
colnames(a) <- c("var","varname")
b <- as.data.frame(mex_test$bet_cen_b)
b$varname <- "Bet. cent."
colnames(b) <- c("var","varname")
c <- as.data.frame(mex_test$clos_cen_b)
c$varname <- "Clo. cent."
colnames(c) <- c("var","varname")
d <- as.data.frame(mex_test$edge_density_km_b)
d$varname <- "Street den."
colnames(d) <- c("var","varname")
e <- as.data.frame(mex_test$intersection_density_km_b)
e$varname <- "Intersection den."
colnames(e) <- c("var","varname")
f <- as.data.frame(mex_test$regularity_b)
f$varname <- "Regularity"
colnames(f) <- c("var","varname")


abc <- rbind(a,b,c,d,e,f)
abc$varname <- as.factor(abc$varname)
abc$varname <- factor(abc$varname, levels = c("Street den.", "Intersection den.", "Regularity" ,
                                              "Bet. cent.", "Clo. cent.","Inf. cent.")) 
#", "#8a7cba", "#a6bbda", "#c7dceb", "#edf8fb"
# Pinkish colors "#c7dceb","#8a7cba"
#"gray","gray8"
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("Test_USA_match_Mx_levels.png", width = 10, height = 9, units = 'in', res = 300)
a <- ggplot(abc, aes(fill=var, x = varname)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),position="fill", width = .5) + 
  scale_y_continuous(labels=scales::percent)  + 
  labs(title="Kruskal-Wallis Test (US)", 
       subtitle=expression(paste(H[0], ": Median values of street connectivity are equal amorng categories of poverty")),
       y="% of metro areas", x="", 
       fill="") +
  scale_fill_manual(values = c("grey","grey8")) +
  theme_bw(base_size = 26) +  
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 30),
        plot.subtitle = element_text(size=17))
a 
add_pval(a,pairs = list(c(1, 5)),  test='kruskal.test')
print(a)
dev.off()
graphics.off()

abc_table <- as.data.frame(cbind(
  ddply(mex_test, .(mex_test$edge_density_km_b), nrow),
  ddply(mex_test, .(mex_test$intersection_density_km_b), nrow),
  ddply(mex_test, .(mex_test$regularity_b), nrow),
  ddply(mex_test, .(mex_test$bet_cen_b), nrow),
  ddply(mex_test, .(mex_test$clos_cen_b), nrow),
  ddply(mex_test, .(mex_test$inf_cen_b), nrow)
))

write.csv(abc_table, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021\\US_tracts_MA_kruskal.csv",row.names = F)





###############Mexico
t_map = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Shapefiles\\AGEB_urb_2010_5.shp"
t_map = readOGR(t_map, layer = basename(strsplit(t_map, "\\.")[[1]])[1]) 

files <- list.files(path = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_MAs\\Processed_results\\GPU\\corrected_Aug_2019\\simplify", pattern = "*.csv", full.names = T)
tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
tbl$COUNTER <- gsub("C:\\\\Users\\\\franc\\\\Dropbox\\\\Francisco\\\\Papers 2018\\\\OSMNX\\\\Results\\\\Networks_MAs\\\\Processed_results\\\\GPU\\\\corrected_Aug_2019\\\\simplify/", "", tbl$id)
tbl$COUNTER2 <- gsub(".csv", "", tbl$COUNTER)
tbl$COUNTER <- tbl$COUNTER2

tbl <- tbl[,c("COUNTER","x","y","degree_centrality","bet_centrality_l","closeness_centrality","inf_centrality")]

coordinates(tbl)<-~x+y
proj4string(tbl)<-CRS("+proj=longlat +datum=NAD83")
tbl <- spTransform(tbl, CRS(proj4string(t_map)))

res_5 <- over(tbl, t_map)
res_5 <- mutate(res_5, id = rownames(res_5))

res6 <- res_5[,c("CVEGEO","id")]
tbl_df <- as.data.frame(tbl)
tbl_df <- cbind(tbl_df,res6)
tbl_df$id <- NULL
tbl$COUNTER <- as.numeric(tbl$COUNTER)
tbl_df <-  aggregate(list(deg_cen=tbl_df$degree_centrality, 
                          bet_cen=tbl_df$bet_centrality_l, 
                          clos_cen=tbl$closeness_centrality,
                          inf_cen=tbl$inf_centrality, COUNTER=tbl$COUNTER), 
                     by=list(CVEGEO=tbl_df$CVEGEO),
                     FUN= mean )
tbl_df$CVEGEO <- as.character(tbl_df$CVEGEO)

poverty <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\Poverty_Ageb_Census_tract\\Agebs_poverty_levels.csv", header = T)
colnames(poverty) <- c("mun_code","mun_name","CVEGEO","pov_l")
poverty1 <- merge(x=poverty, y=tbl_df, by="CVEGEO",all.x=TRUE)
pp <- subset(poverty1, !is.na(poverty1$deg_cen))
poverty1$pov_l1 <- ifelse(poverty1$pov_l=="[ 0, 18]","1.[ 0,18]","")
poverty1$pov_l2 <- ifelse(poverty1$pov_l=="(18, 34]","2.(18,34]","")
poverty1$pov_l3 <- ifelse(poverty1$pov_l=="(34, 50]","3.(34,50]","")
poverty1$pov_l4 <- ifelse(poverty1$pov_l=="(50, 70]","4.(50,70]","")
poverty1$pov_l5 <- ifelse(poverty1$pov_l=="(70, 100]","5.(70,100]","")
poverty1$pov_l6 <- ifelse(poverty1$pov_l=="Una vivienda particular habitada","NA","")
poverty1$pov_l7 <- ifelse(poverty1$pov_l=="Sin viviendas particulares habitadas","NA","")
poverty1$pov_l8 <- ifelse(poverty1$pov_l=="No disponible","NA","")
poverty1$pov_ll <- paste(poverty1$pov_l1,poverty1$pov_l2,poverty1$pov_l3,
                         poverty1$pov_l4,poverty1$pov_l5,poverty1$pov_l6,
                         poverty1$pov_l7,poverty1$pov_l8, sep="")
poverty1$pov_l1 <- NULL
poverty1$pov_l2 <- NULL
poverty1$pov_l3 <- NULL
poverty1$pov_l4 <- NULL
poverty1$pov_l5 <- NULL
poverty1$pov_l6 <- NULL
poverty1$pov_l7 <- NULL
poverty1$pov_l8 <- NULL

poverty1$pov_ll <- as.factor(poverty1$pov_ll)

poverty1_metro <- subset(poverty1, poverty1$COUNTER=="45")
poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen")]
colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen")
poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
#poverty1_metro$pov_l <- as.character(poverty1_metro$pov_l )
  
colors <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\03-Data\\colors_poverty.csv", header = TRUE)
#colors$pov_l <- as.character(colors$pov_l)

poverty1_metro <- merge(x=poverty1_metro, y=colors, by="pov_l", all.x=TRUE)
poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
poverty1_metro <- melt(poverty1_metro)

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("Boxplot_Nuevo_Laredo_inf_cen.png", width = 8, height = 8, units = 'in', res = 300)
a <- ggplot(poverty1_metro, aes(x=as.factor(pov_l), y=value, fill=color_code)) + geom_boxplot() + 
  scale_fill_manual( values=c("#87489f", "#8a7cba", "#a6bbda", "#c7dceb", "#edf8fb")) +
  labs(title="Nuevo Laredo metro area, Mexico", subtitle="(221 census tracts)") + 
  ylab("Avg. information centrality") + xlab("% of people below the poverty line")+ 
  stat_compare_means(method = "kruskal.test", size=8)+
  theme_bw(base_size = 24)+  theme(legend.position = "none")# +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
a
add_pval(a,pairs = list(c(1, 5)),  test='kruskal.test')
print(a)
dev.off()
graphics.off()

bet_cen <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="bet_cen")) 
  bet_cen[i] <- value$p.value
}

inf_cen <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #deg
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="inf_cen")) 
  inf_cen[i] <- value$p.value
}

clos_cen <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="clos_cen")) 
  clos_cen[i] <- value$p.value
}


################ Intersections
setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\Results\\Networks_Agebs\\simplify_basic_statistics")
files <- list.files(path=getwd(),pattern =".csv")
myfiles = lapply(files, function(x) read.csv(x,skip = 0, header = TRUE))
ID <- gsub(".csv","",files)
filelist <- mapply(cbind, myfiles, "COUNTER_FILE_ID"=ID, SIMPLIFY=F)
inter <- do.call("rbind",filelist)
inter$COUNTER_FILE_ID <- as.character(inter$COUNTER_FILE_ID)
inter$X <- sequence(rle(inter$COUNTER)$lengths)
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
inter_aux <- merge(x=inter3,y=inter4, by="COUNTER", all.x=TRUE)
t_map_df <- as.data.frame(t_map)
t_map_df <- t_map_df[,c("COUNTER","CVEGEO")]
t_map_df <- merge(x=t_map_df, y=inter_aux, by="COUNTER", all.x=TRUE)
inter_aux2 <- inter[,c("COUNTER","edge_density_km", "intersection_density_km")]
t_map_df <- merge(x=t_map_df, y=inter_aux2, by="COUNTER", all.x=TRUE)
t_map_df$COUNTER <- NULL
t_map_df$proportion_4ormore_intersections[is.na(t_map_df$proportion_4ormore_intersections)] <- 0
t_map_df$proportion_3orless_intersections[is.na(t_map_df$proportion_3orless_intersections)] <- 0
t_map_df$regularity <- (t_map_df$proportion_4ormore_intersections-t_map_df$proportion_3orless_intersections) / (t_map_df$proportion_4ormore_intersections+t_map_df$proportion_3orless_intersections)

poverty1 <- merge(x=poverty1, y=t_map_df, by="CVEGEO", all.x=T)

edge_density_km <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="edge_density_km")) 
  edge_density_km[i] <- value$p.value
}

intersection_density_km <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="intersection_density_km")) 
  intersection_density_km[i] <- value$p.value
}

regularity <- c()
for (i in 1:74){
  poverty1_metro <- subset(poverty1, poverty1$COUNTER==paste(i,sep = ""))
  poverty1_metro <- poverty1_metro[,c("COUNTER","pov_ll","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")]
  colnames(poverty1_metro) <- c("COUNTER","pov_l","inf_cen","bet_cen","clos_cen","edge_density_km","intersection_density_km","regularity")
  poverty1_metro$COUNTER <- as.factor(poverty1_metro$COUNTER)
  poverty1_metro <- subset(poverty1_metro, poverty1_metro$pov_l!="NA")
  poverty1_metro$pov_l <- as.factor(as.character(poverty1_metro$pov_l))
  poverty1_metro <- melt(poverty1_metro)
  #bet
  value <- kruskal.test(value ~ pov_l, data = subset(poverty1_metro, poverty1_metro$variable=="regularity")) 
  regularity[i] <- value$p.value
}


mex_test <- as.data.frame(cbind(inf_cen, bet_cen, clos_cen, edge_density_km, intersection_density_km, regularity))
mex_test$inf_cen_b <- ifelse(mex_test$inf_cen<=0.05, "Reject","Not reject")
mex_test$bet_cen_b <- ifelse(mex_test$bet_cen<=0.05, "Reject","Not reject")
mex_test$clos_cen_b <- ifelse(mex_test$clos_cen<=0.05, "Reject","Not reject")
mex_test$edge_density_km_b <- ifelse(mex_test$edge_density_km<=0.05, "Reject","Not reject")
mex_test$intersection_density_km_b <- ifelse(mex_test$intersection_density_km<=0.05, "Reject","Not reject")
mex_test$regularity_b <- ifelse(mex_test$regularity<=0.05, "Reject","Not reject")


mex_test <- mex_test[,c("inf_cen_b","bet_cen_b","clos_cen_b","edge_density_km_b",
                        "intersection_density_km_b","regularity_b")]
mex_test$inf_cen_b <- as.factor(mex_test$inf_cen_b)
mex_test$bet_cen_b <- as.factor(mex_test$bet_cen_b)
mex_test$clos_cen_b <- as.factor(mex_test$clos_cen_b)
mex_test$edge_density_km_b <- as.factor(mex_test$edge_density_km_b)
mex_test$intersection_density_km_b <- as.factor(mex_test$intersection_density_km_b)
mex_test$regularity_b <- as.factor(mex_test$regularity_b)

a <- as.data.frame(mex_test$inf_cen_b)
a$varname <- "Inf. cent."
colnames(a) <- c("var","varname")
b <- as.data.frame(mex_test$bet_cen_b)
b$varname <- "Bet. cent."
colnames(b) <- c("var","varname")
c <- as.data.frame(mex_test$clos_cen_b)
c$varname <- "Clo. cent."
colnames(c) <- c("var","varname")
d <- as.data.frame(mex_test$edge_density_km_b)
d$varname <- "Street den."
colnames(d) <- c("var","varname")
e <- as.data.frame(mex_test$intersection_density_km_b)
e$varname <- "Intersection den."
colnames(e) <- c("var","varname")
f <- as.data.frame(mex_test$regularity_b)
f$varname <- "Regularity"
colnames(f) <- c("var","varname")


abc <- rbind(a,b,c,d,e,f)
abc$varname <- as.factor(abc$varname)
abc$varname <- factor(abc$varname, levels = c("Street den.", "Intersection den.", "Regularity" ,
                                              "Bet. cent.", "Clo. cent.","Inf. cent.")) 

#", "#8a7cba", "#a6bbda", "#c7dceb", "#edf8fb"

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("Test_MX.png", width = 10, height = 9, units = 'in', res = 300)
a <-  ggplot(abc, aes(fill=var, x = varname)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),position="fill", width = .5) + 
  scale_y_continuous(labels=scales::percent)  + 
  labs(title="Kruskal-Wallis Test (Mexico)", 
       subtitle=expression(paste(H[0], ": Median values of street connectivity are equal amorng categories of poverty")),
       y="% of metro areas", x="", 
       fill="") +
  scale_fill_manual(values = c("grey","grey8")) +
  theme_bw(base_size = 26) +  
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 30),
        plot.subtitle = element_text(size=17))
#
#  ggplot(abc, aes(fill=var, x = varname)) + 
#  geom_bar(aes(y = (..count..)/sum(..count..)),position="fill", width = .5) + 
#  scale_y_continuous(labels=scales::percent)  + 
#  labs(subtitle="Kruskal-Wallis Test (Mexico)", y="% of metro areas", x="", fill="Test result") +
#  scale_fill_manual(values = c("grey","grey8")) +
#  theme_bw(base_size = 24) +  theme(legend.position = "bottom",axis.text.x = element_text(angle = 30))
a
add_pval(a,pairs = list(c(1, 5)),  test='kruskal.test')
print(a)
dev.off()
graphics.off()

abc_table <- as.data.frame(cbind(
                ddply(mex_test, .(mex_test$edge_density_km_b), nrow),
                ddply(mex_test, .(mex_test$intersection_density_km_b), nrow),
                ddply(mex_test, .(mex_test$regularity_b), nrow),
                ddply(mex_test, .(mex_test$bet_cen_b), nrow),
                ddply(mex_test, .(mex_test$clos_cen_b), nrow),
                ddply(mex_test, .(mex_test$inf_cen_b), nrow)
                                         ))

write.csv(abc_table, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021\\MX_tracts_MA_kruskal.csv",row.names = F)



###############Plot diagram
cat("\014") 
rm(list = ls())
graphics.off()

data <- data.frame(
  type = c( rep("Very low", 200), 
            rep("Low", 200),
            rep("Medium", 200),
            rep("High", 200),
            rep("Very high", 200)),
  value = c( rnorm(200), rnorm(200, mean=4),
             rnorm(200, mean=6),
             rnorm(200, mean=8),
             rnorm(200, mean=11))
)
data$type <- factor(data$type, levels = c("Very low", "Low","Medium", 
                                      "High", "Very high")) 
nn <- ddply(data, "type", transform, 
            median  = median(value))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\OSMNX\\2020\\07-Figures-2021")
png("k_test_diagram.png", width = 14, height = 8, units = 'in', res = 300)
a <- nn %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", position = 'identity') +
  ylab("count") + xlab("Value of street conectivity indicator")+ 
  labs(fill="Share of poverty") +
  scale_fill_manual(values=c('#87489f',"#8a7cba", "#a6bbda","#c7dceb","#edf8fb")) +
  theme_dark(base_size = 24)+
  geom_vline(aes(xintercept = median),data=nn,linetype = 5, color="black", size=1.5)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),legend.position="bottom")
a
print(a)
dev.off()
graphics.off()




ggsave(filename = "k_test_diagram.eps", plot=a,family="Times",
       width = 14, height = 8, dpi = 300, units = "in")


#'#87489f',"#8a7cba", "#a6bbda", "#c7dceb", "#edf8fb"

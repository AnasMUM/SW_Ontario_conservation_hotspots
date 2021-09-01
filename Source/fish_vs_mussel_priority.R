comb.n<-cbind(all, res.fish.rich, res.mussel.rich, res.fish.SAR.ratio, res.Mussel.SAR.ratio)

comb.n <- comb.n %>% mutate(fish.rich.quart= ntile(res.fish.rich, 3))

comb.n <- comb.n %>% mutate(mussel.rich.quart= ntile(res.mussel.rich, 3))

comb.n <- comb.n %>% mutate(fish.SAR.quart= ntile(res.fish.SAR.ratio, 3))

comb.n <- comb.n %>% mutate(mussel.SAR.quart= ntile(res.Mussel.SAR.ratio, 3))

#For table----
comb.n$rich_hot_overlap<-ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart==3, 1, 0)

comb.n$rich_cold_overlap<-ifelse(comb.n$fish.rich.quart==1 & comb.n$mussel.rich.quart==1, 1, 0)

comb.n$rich_fm_hotcold_overlap<-ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart==1, 1, 0)

comb.n$rich_mf_hotcold_overlap<-ifelse(comb.n$fish.rich.quart==1 & comb.n$mussel.rich.quart==3, 1, 0)



comb.n$SAR_hot_overlap<-ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart==3, 1, 0)

comb.n$SAR_cold_overlap<-ifelse(comb.n$fish.SAR.quart==1 & comb.n$mussel.SAR.quart==1, 1, 0)

comb.n$SAR_fm_hotcold_overlap<-ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart==1, 1, 0)

comb.n$SAR_mf_hotcold_overlap<-ifelse(comb.n$fish.SAR.quart==1 & comb.n$mussel.SAR.quart==3, 1, 0)


set1<-subset(comb.n, select = c(48:55))

change1<-as.data.frame(((colSums(set1))/7)*100)
change1 <- tibble::rownames_to_column(change1, "Difference")
colnames(change1)<-c("Difference", "pcnt_change")

#Mapping----
library(dplyr)
library(stringr)
library(tmap)
library(tmaptools)
library(sf)
library(viridis)
library(colorspace)
library(grid)
library(cowplot)
library(patchwork)


#Representativeness/Native Richness
comb.n$rep_hot<-ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart==3, "Both fish and mussel", 
                       ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart!=3, "Fish only",
                              ifelse(comb.n$fish.rich.quart!=3 & comb.n$mussel.rich.quart==3, "Mussel only", NA)))

comb.n$rep_cold<-ifelse(comb.n$fish.rich.quart==1 & comb.n$mussel.rich.quart==1, "Both fish and mussel", 
                       ifelse(comb.n$fish.rich.quart==1 & comb.n$mussel.rich.quart!=1, "Fish only",
                              ifelse(comb.n$fish.rich.quart!=1 & comb.n$mussel.rich.quart==1, "Mussel only", NA)))

comb.n$irrep_hot<-ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart==3, "Both fish and mussel", 
                         ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart!=3, "Fish only", 
                                ifelse(comb.n$fish.SAR.quart!=3 & comb.n$mussel.SAR.quart==3, "Mussel only", NA)))

comb.n$irrep_cold<-ifelse(comb.n$fish.SAR.quart==1 & comb.n$mussel.SAR.quart==1, "Both fish and mussel", 
                         ifelse(comb.n$fish.SAR.quart==1 & comb.n$mussel.SAR.quart!=1, "Fish only", 
                                ifelse(comb.n$fish.SAR.quart!=1 & comb.n$mussel.SAR.quart==1, "Mussel only", NA)))

SWON<-st_read("data/SW_ON_Hybas_07_sel.shp")

glakes<-st_read("data/great_lakes_sel_reprojected.shp")

surround<-st_read("data/SW_ON_Hybas_07_with_surround_reprojected_120721.shp")

CN<-st_read("data/CN_boundary_2.shp")

border<-st_read("data/Hybas7_study_wshds.shp")

CN_Pr<-st_read("data/CN_provinces_reprojected.shp")

#map_data<-merge(SWON, (index))




lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

SWON_lmb = st_transform(SWON, crs = lambert)

SWON_lmb = st_transform(SWON, crs = lambert)

glakes_lmb = st_transform(glakes, crs = lambert)

surround_lmb = st_transform(surround, crs = lambert)

border_lmb = st_transform(border, crs = lambert)

CN_Pr_lmb =  st_transform(CN_Pr, crs = lambert)

CN_lmb =  st_transform(CN, crs = lambert)

#map_data<-left_join(SWON_lmb, (index))
map_data<-merge(SWON_lmb, (comb.n))




#for (i in 1:length(breaks.total)-1){
#breaks.lab <- paste0(breaks.total[i]+1, " - ", breaks.total[i+1])
#}
sel<-bb(surround_lmb, ext=0.38)

tm.rep_hot<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("rep_hot", palette="plasma", contrast= c(0,1), colorNA="gray70", textNA = "Not hotspots", showNA = T,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = F, title= "Hotspots",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Representativeness hotspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, 0.02, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")+
  tm_scale_bar( breaks = c(0, 25, 50, 75, 100), position=c("right", "top"), text.size =0.7)+
  tm_compass(type="arrow", position=c("left", "top"), show.labels = 1)


tm.rep_cold<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("rep_cold", palette="plasma", contrast= c(0,1), colorNA="gray70", textNA = "Not coldspots", showNA = T,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = F, title= "Coldspots",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Representativeness coldspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")


tm.irrep_hot<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("irrep_hot", palette="plasma", contrast= c(0,1), colorNA="gray70", textNA = "Not hotspots", showNA = T,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = F, title= "Hotspots",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Irreplaceability hotspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, 0.02, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

tm.irrep_cold<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("irrep_cold", palette="plasma", contrast= c(0,1), colorNA="gray70", textNA = "Not coldspots", showNA = T,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = F, title= "Coldspots",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Irreplaceability coldspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

tiff("hotspot_coldspot_comb_130821.tiff",
     height = 18.5, width = 18, units = "cm",  pointsize=8,
     res=300)

op <- par(mar = c(2,0.1,2.5,0.1))
#par(op)
tmap_arrange( tm.rep_hot, tm.rep_cold, tm.irrep_hot, tm.irrep_cold,   ncol=2, outer.margins = NULL,   asp=NA)
#print(insetmap, vp = vp)
#tmap_arrange(tm.total, tm.native, tm.nonnative,tm.SAR, ncol=2, outer.margins = c(0.01, 0.01, 0.01,0.03),   asp=NA)
par(op)

layout(1)
dev.off()



#Mapping - approach 2
#Mapping----
library(dplyr)
library(stringr)
library(tmap)
library(tmaptools)
library(sf)
library(viridis)
library(colorspace)
library(grid)
library(cowplot)
library(patchwork)


#Representativeness/Native Richness
comb.n$rep_hot<-ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart==3, "FH-MH", 
                       ifelse(comb.n$fish.rich.quart==3 & comb.n$mussel.rich.quart!=1, "FH-MC",
                              ifelse(comb.n$fish.rich.quart!=1 & comb.n$mussel.rich.quart==3, "FC-MH", 
                                     ifelse(comb.n$fish.rich.quart!=1 & comb.n$mussel.rich.quart==1, "FC-MC",NA))))



comb.n$irrep_hot<-ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart==3, "FH-MH", 
                         ifelse(comb.n$fish.SAR.quart==3 & comb.n$mussel.SAR.quart!=1, "FH-MC", 
                                ifelse(comb.n$fish.SAR.quart!=1 & comb.n$mussel.SAR.quart==3, "FC-MH",
                                       ifelse(comb.n$fish.SAR.quart!=1 & comb.n$mussel.SAR.quart==1, "FC-MC",NA))))


SWON<-st_read("data/SW_ON_Hybas_07_sel.shp")

glakes<-st_read("data/great_lakes_sel_reprojected.shp")

surround<-st_read("data/SW_ON_Hybas_07_with_surround_reprojected_120721.shp")

CN<-st_read("data/CN_boundary_2.shp")

border<-st_read("data/Hybas7_study_wshds.shp")

CN_Pr<-st_read("data/CN_provinces_reprojected.shp")

#map_data<-merge(SWON, (index))




lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

SWON_lmb = st_transform(SWON, crs = lambert)

SWON_lmb = st_transform(SWON, crs = lambert)

glakes_lmb = st_transform(glakes, crs = lambert)

surround_lmb = st_transform(surround, crs = lambert)

border_lmb = st_transform(border, crs = lambert)

CN_Pr_lmb =  st_transform(CN_Pr, crs = lambert)

CN_lmb =  st_transform(CN, crs = lambert)

#map_data<-left_join(SWON_lmb, (index))
map_data<-merge(SWON_lmb, (comb.n))

#for (i in 1:length(breaks.total)-1){
#breaks.lab <- paste0(breaks.total[i]+1, " - ", breaks.total[i+1])
#}
sel<-bb(surround_lmb, ext=0.38)

tm.irrep_hot<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("irrep_hot", palette="plasma", contrast= c(0,1), colorNA="gray70",  showNA = F,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = T, title= "Overlap",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.7, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Irreplaceability hot and coldspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, 0.02, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")+
  tm_scale_bar( breaks = c(0, 25, 50, 75, 100), position=c("right", "top"), text.size =0.7)+
  tm_compass(type="arrow", position=c("left", "top"), show.labels = 1)




tm.rep_hot<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("rep_hot", palette="plasma", contrast= c(0,1), colorNA="gray70",  showNA = F,  lwd=0.9,  as.count=T, style="fixed", n=3, legend.show = T, legend.reverse = T, title= "Overlap",  lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.7, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Representativeness hot and coldspots", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, 0.02, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-5, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")
  






tiff("hot_coldspots_overlap_comb_310821_2.tiff",
     height = 9, width = 18, units = "cm",  pointsize=8,
     res=300)

op <- par(mar = c(2,0.1,2.5,0.1))
#par(op)
tmap_arrange(tm.irrep_hot, tm.rep_hot,   ncol=2, outer.margins = NULL,   asp=NA)
#print(insetmap, vp = vp)
#tmap_arrange(tm.total, tm.native, tm.nonnative,tm.SAR, ncol=2, outer.margins = c(0.01, 0.01, 0.01,0.03),   asp=NA)
par(op)

layout(1)
dev.off()

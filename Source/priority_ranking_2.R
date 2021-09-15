###Ranking watersheds based on different templates and map them
library(tidyverse)

all<-read.csv("data/all_var_180621.csv")
all=na.omit(all)
sp<-all[,15:21]
human<-all[,22:39]
area=all$Area_KM2.x

fish.native<-sp$Fish.native.richness
fish.nonnative<-sp$Fish.nonnative.richness
fish.foreign<-sp$Foreign.richness
fish.trans<-sp$Translocated.richness
fish.SAR<-sp$Fish.SAR.richness
Mussel.native<-sp$Mussel.native.richness
Mussel.SAR<-sp$Mussel.SAR.richness

human.sel<-subset(human, select = -c(Mean_human_Intrution,Mean_energy_mining, Mining_D_2008, Manuf_D_2008, Util_D_2008,  Dwelling_D, Pcnt_imp_surf, Manuf_D, Util_D, Mining_D   ))

resid.nonnative<-resid(lm(fish.nonnative~area))
stress<-cbind(human.sel,resid.nonnative)

stress.scaled<-apply(stress, MARGIN = 2, FUN = function(X) ((X-min(X))/(max(X)-min(X))))


overall.stress<-rowSums(stress.scaled)

scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
scale1 <- function(x){(x)/(max(x))}

overall.stress.scaled<-scale01(overall.stress)

#cor(fish.native, area)
#cor(fish.native, log10(area))
#cor(fish.SAR, area)
#cor(fish.SAR, log10(area))

#cor(Mussel.native, area)
#cor(Mussel.native, log10(area))
#cor(Mussel.SAR, area)
#cor(Mussel.SAR, log10(area))

#library(BAT)



#sar(fish.native,,area)

res.fish.rich=scale01(resid(lm(fish.native~area)))
fish.SAR.ratio = fish.SAR/fish.native
res.fish.SAR.ratio=scale01(resid(lm(fish.SAR.ratio~area)))

res.mussel.rich=scale01(resid(lm(Mussel.native~area)))
Mussel.SAR.ratio = Mussel.SAR/Mussel.native
res.Mussel.SAR.ratio=scale01(resid(lm(Mussel.SAR.ratio~area)))

overall.index<- overall.stress.scaled+res.fish.rich+res.fish.SAR.ratio+res.mussel.rich+res.Mussel.SAR.ratio

##Reactive
reactive.index<-overall.stress.scaled+(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2

reactive.index

##Proactive
stress.inverse<-1-overall.stress.scaled

proactive.index.1<-stress.inverse

proactive.index.2<-(stress.inverse*2)+(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2

proactive.index.2

proactive.index.3<-(stress.inverse*2)+(res.fish.rich+res.mussel.rich)/2

proactive.index.3

#proactive.index.3<-(stress.inverse*4)+(res.fish.rich+res.mussel.rich)/2+(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2

#proactive.index.3

#Representative
representative.index<-((res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2)*2+((res.fish.rich+res.mussel.rich)/2)

#Mapping

index<-as.data.frame(cbind(all$HYBAS_ID, reactive.index, proactive.index.1, proactive.index.2,proactive.index.3, representative.index))

fix(index)

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
map_data<-merge(SWON_lmb, (index))

range(index$reactive.index, na.rm = T)
hist(index$reactive.index)

#breaks.total<-c(1, 21, 41, 61, 81,  Inf)
breaks.reactive<-c( 0.4, 0.8, 1.2, 1.8)


#for (i in 1:length(breaks.total)-1){
#breaks.lab <- paste0(breaks.total[i]+1, " - ", breaks.total[i+1])
#}
sel<-bb(surround_lmb, ext=0.38)

tm.proactive.1<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking", labels = c ("Low (0.40 - 1.72)", "Moderate (1.72 - 2.20)","High (2.20 - 2.75)" ), lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Proactive 1 template", 
            panel.label.fontface="bold", panel.label.size = 1.21,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-4.6, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")+
  tm_scale_bar( breaks = c(0, 25, 50, 75, 100), position=c("right", "top"), text.size =0.7)+
  tm_compass(type="arrow", position=c("left", "top"), show.labels = 1)

#tm.proactive.1

tm.proactive.2<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("proactive.index.2", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking", labels = c ("Low (0.40 - 1.72)", "Moderate (1.72 - 2.20)","High (2.20 - 2.75)" ), lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Proactive 2 template", 
            panel.label.fontface="bold", panel.label.size = 1.2, between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

tm.proactive.3<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("proactive.index.3", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking", labels = c ("Low (0.40 - 1.72)", "Moderate (1.72 - 2.20)","High (2.20 - 2.75)" ), lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, legend.width=0.4,legend.height = 0.3,
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Proactive 3 template", 
            panel.label.fontface="bold", panel.label.size = 1.2,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

tm.reactive<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("reactive.index", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking", labels = c ("Low (0.20 - 0.48)", "Moderate (0.48 - 1.05)","High (1.05 - 1.78)" ), lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Reactive template", 
            panel.label.fontface="bold", panel.label.size = 1.2,  between.margin=30,  inner.margins=0.00,outer.margins = c(0.02, -0.35, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

tm.representative<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  tm_polygons("representative.index", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking", labels = c ("Low (0.20 - 0.48)", "Moderate (0.48 - 1.05)","High (1.05 - 1.78)" ), lwd = 0.8) + 
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("right" , "bottom"), legend.text.size=0.8, 
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Representative template", 
            panel.label.fontface="bold", panel.label.size = 1.2, between.margin=30,  inner.margins=0.00,outer.margins = c(0.02,-0.35, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

#tm.proactive.1.g<-tmap_grob(tm.proactive.1)
#tm.proactive.2.g<-tmap_grob(tm.proactive.2)
#tm.proactive.3.g<-tmap_grob(tm.proactive.3)
#tm.reactive.g<-tmap_grob(tm.reactive)
#tm.representative.g<-tmap_grob(tm.representative)

#tiff("templates_comb.tiff",
#height = 27.5, width = 18.2, units = "cm",  pointsize=8,
#res=50)

#op <- par(mar = c(2,4,2.5,0.4) + 0.1)
#wrap_elements( tm.proactive.1.g)+ wrap_elements(tm.reactive.g)+wrap_elements(tm.proactive.2.g)+wrap_elements(tm.representative.g)+ wrap_elements(tm.proactive.3.g)+plot_layout(ncol = 2)
#print(insetmap, vp = vp)
#tmap_arrange(tm.total, tm.native, tm.nonnative,tm.SAR, ncol=2, outer.margins = c(0.01, 0.01, 0.01,0.03),   asp=NA)
#par(op)

#layout(1)
#dev.off()


tiff("templates_comb_260721.tiff",
     height = 25, width = 17.9, units = "cm",  pointsize=8,
     res=300)

op <- par(mar = c(2,0.1,2.5,0.1))
#par(op)
tmap_arrange( tm.proactive.1, tm.reactive,tm.proactive.2, tm.representative, tm.proactive.3,   ncol=2, outer.margins = NULL,   asp=NA)
#print(insetmap, vp = vp)
#tmap_arrange(tm.total, tm.native, tm.nonnative,tm.SAR, ncol=2, outer.margins = c(0.01, 0.01, 0.01,0.03),   asp=NA)
par(op)

layout(1)
dev.off()

#Map criteria

Vulnerability<-overall.stress.scaled
Irreplaceability<-(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2
Representativeness<-(res.fish.rich+res.mussel.rich)/2

criteria<-as.data.frame(cbind(all$HYBAS_ID, Vulnerability, Irreplaceability, Representativeness))

fix(criteria)

#Insert map

map_data<-merge(SWON_lmb, (criteria))

range(criteria$Vulnerability, na.rm = T)
hist(criteria$Vulnerability, na.rm = T)

#breaks.total<-c(1, 21, 41, 61, 81,  Inf)
breaks.reactive<-c( 0.4, 0.8, 1.2, 1.8)


#for (i in 1:length(breaks.total)-1){
#breaks.lab <- paste0(breaks.total[i]+1, " - ", breaks.total[i+1])
#}
sel<-bb(surround_lmb, ext=0.38)

tm.vul<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  #tm_polygons("Vulnerability", palette="OrRd", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="pretty", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + #Pretty categories
  tm_polygons("Vulnerability", palette="OrRd", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + #Quantie categories
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("left" , "top"), legend.text.size=0.8, legend.width = 0.25,
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Vulnerability", 
            panel.label.fontface="bold", panel.label.size = 1.4,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  tm_text("NAME", size = 0.8, xmod = c(-4.8, 0, 0, -1), ymod = c(-1.9, 0, -4.7, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")+
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1)+
  tm_scale_bar( breaks = c(0, 25, 50, 75, 100), position=c("left", "bottom"), text.size =0.7)


#tm.vul

##Irreplacilibity
tm.irr<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  #tm_polygons("Irreplaceability", palette="PuRd", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="pretty", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + 
  tm_polygons("Irreplaceability", palette="PuRd", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + #quantile
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("left" , "top"), legend.text.size=0.8, legend.width = 0.25,
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Irreplaceability", 
            panel.label.fontface="bold", panel.label.size = 1.4,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  #tm_text("NAME", size = 0.8, xmod = c(-4.6, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")

#tm.irr

tm.rep<-tm_shape(surround_lmb,  bbox=sel)+
  #tm.reactive<-tm_shape(surround, bbox=sel)+  
  tm_polygons(lwd = 0.8, 
              col = "#e2e2e2", border.alpha = 0.5)+
  tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  #tm_polygons("reactive.index", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.reactive, legend.show = T, legend.reverse = T, title= "Reactive Index",  lwd = 0.8) + 
  #tm_polygons("Representativeness", palette="BuGn", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="pretty", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + 
  tm_polygons("Representativeness", palette="BuGn", contrast= c(0,0.8), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Score", lwd = 0.8) + #quantile
  #tm_polygons("proactive.index.1", palette="-inferno", contrast= c(0,0.7), colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="quantile", n=3, legend.show = T, legend.reverse = T, title= "Priority Ranking",  lwd = 0.8) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 1, legend.position=c("left" , "top"), legend.text.size=0.8, legend.width = 0.25,
            legend.title.size = 1,legend.title.fontface ="bold", panel.show = T, panel.labels = "Representativeness", 
            panel.label.fontface="bold", panel.label.size = 1.4,  between.margin = 30,  inner.margins=0.00,outer.margins = c(0.02, -0.1, 0.02, 0))+
  #tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)+
  tm_shape(glakes_lmb)+
  tm_polygons(lwd = 0.8, 
              col = "#bbebff", border.alpha=0.5)+
  #tm_text("NAME", size = 0.8, xmod = c(-4.6, 0, 0, -1), ymod = c(-1.8, 0, -3.5, 0 ))+
  tm_shape(border_lmb)+
  tm_borders(lwd = 1.7, 
             col = "black")


region<-st_read("data/SW_ON_Hybas_07_with_surround_reprojected_2.shp")

region_lmb=st_transform(region, crs = lambert)

sg <- bb_poly(region_lmb)
xy <- st_bbox(CN_Pr_lmb)
asp<- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)

insetmap = tm_shape(CN_Pr_lmb) + tm_fill(col="#e2e2e2") +
  tm_borders(lwd = 1, alpha = 0.6) +
  tm_text("Pr2", size = 0.6)+
  tm_shape(CN_lmb) + 
  tm_borders(lwd = 0.5, col="black") +
  tm_shape(sg) + tm_borders(lw=1.5, col="red") +
  tm_layout(inner.margins = c(0.06,0.04,0.04,0.04), outer.margins=c(0,0,0,0))

#insetmap 

# Create viewport
w <- 0.35
h <- asp * w
vp <- viewport(x=0.928, y=0.538, width = w, height=h, just=c("right", "bottom"))





tiff("Criteria_map_quantile_4_150921.tiff",
     height = 25.5, width = 8.5, units = "cm",  pointsize=8,
     res=300)

op <- par(mar = c(2,4,2.5,2) + 0.1)
tmap_arrange( tm.vul, tm.irr, tm.rep,   ncol=1, outer.margins =  c(0.01, 0.01, 0.01,0.03),   asp=NA)
print(insetmap, vp = vp)
#tmap_arrange(tm.total, tm.native, tm.nonnative,tm.SAR, ncol=2, outer.margins = c(0.01, 0.01, 0.01,0.03),   asp=NA)
par(op)

layout(1)
dev.off()

Feqaul#proactive
range(index$proactive.index.2, na.rm = T)
hist(index$proactive.index.2)

breaks.proactive<-c( 0, 1.5, 2.0, 2.5, 3)

tm.proactive<-tm_shape(map_data) + 
  #tm_polygons("reactive.index", palette="viridis", colorNA="white", textNA = NA, showNA = F,   legend.show = T, legend.reverse = F, title= "Fish Biodiversity Index", lwd=0.25, border.alpha = 0.5) +
  tm_polygons("proactive.index.2", palette="viridis",  colorNA="gray", textNA = NA, showNA = F,  lwd=0.9,  as.count=F, style="fixed", breaks = breaks.proactive, legend.show = T, legend.reverse = T, title= "Reactive Index", lwd = 0.65) + 
  #tm_polygons("jaccard.S", palette="Blues", contrast= c(0.1,1), colorNA="white", textNA = NA, showNA = F,  lwd=0.9,   style="quantile", n=4, legend.show = T, legend.reverse = T, title= "Jaccard Index", lwd = 0.65) + 
  #tm_fill("Total.richness", palette="Purples", contrast= c(0.1,1), colorNA="white", lwd=0.9,  as.count=T, style="fixed", breaks = breaks.total, legend.show = T, legend.reverse = T, title= "Total richness") + 
  #tm_borders(lwd = 0.65)+
  tm_layout(bg.color = "white",  legend.position=c("left" , "top"), legend.text.size=0.5, 
            legend.title.size = 0.7,legend.title.fontface ="bold", panel.show = T, panel.labels = "Reactive Index", 
            panel.label.fontface="bold", between.margin=6, inner.margins=0.04, outer.margins = c(0.01, 0.01, 0.01,0.01))+
  tm_graticules(labels.show=c(T, T), ticks = FALSE, lines=F, labels.size = 0.6)

tm.proactive

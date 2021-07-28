library(tidyverse)
##Calculate PA area in each watershed

PAs<-read.csv("data/PAs_SWON.csv")
sum_PA<-aggregate(Int_area_KM2 ~ HYBAS_ID, PAs, sum)

all<-read.csv("data/all_var_180621.csv")
all=na.omit(all)

all.n<-merge(all, sum_PA,  by.x = "HYBAS_ID", all.x = T)

all.n[is.na(all.n)] <- 0

#Total % PA in study area (0.36%)

(sum(all.n$Int_area_KM2)/sum(all.n$Area_KM2.x))*100

#Calculate criteria
#all.n=na.omit(all.n)
sp<-all.n[,15:21]
human<-all.n[,22:39]
area=all.n$Area_KM2.x

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

cor(fish.native, area)
cor(fish.native, log10(area))
cor(fish.SAR, area)
cor(fish.SAR, log10(area))

cor(Mussel.native, area)
cor(Mussel.native, log10(area))
cor(Mussel.SAR, area)
cor(Mussel.SAR, log10(area))


res.fish.rich=scale01(resid(lm(fish.native~area)))
fish.SAR.ratio = fish.SAR/fish.native
res.fish.SAR.ratio=scale01(resid(lm(fish.SAR.ratio~area)))

res.mussel.rich=scale01(resid(lm(Mussel.native~area)))
Mussel.SAR.ratio = Mussel.SAR/Mussel.native
res.Mussel.SAR.ratio=scale01(resid(lm(Mussel.SAR.ratio~area)))


##Proactive 1
stress.inverse<-1-overall.stress.scaled
proactive.index.1<-(stress.inverse*1)

temp<-as.data.frame(cbind(all.n,proactive.index.1))
fix(temp)

temp <- temp %>% mutate(quartile.1 = ntile(proactive.index.1, 3))

pro1.sum<-temp %>% 
  select(quartile.1, Area_KM2.x, Int_area_KM2)%>%
  group_by(quartile.1) %>% 
  summarise(across(everything(), sum))

pro1.sum<-pro1.sum%>%
  dplyr::mutate(
    Pcnt_PAs= (Int_area_KM2/Area_KM2.x)*100,
    Template = "Proactive 1",
    Priority= factor(quartile.1, 
                     levels=c(1,2,3),
                     labels = c("low", "Moderate", "High")))

pro1.sel<-pro1.sum%>%
  select(Template, Priority, Pcnt_PAs)


    

#Proactive 2

proactive.index.2<-(stress.inverse*2)+(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2


temp<-as.data.frame(cbind(temp,proactive.index.2))

temp <- temp %>% mutate(quartile.2= ntile(proactive.index.2, 3))

pro2.sum<-temp %>% 
  select(quartile.2, Area_KM2.x, Int_area_KM2)%>%
  group_by(quartile.2) %>% 
  summarise(across(everything(), sum))

pro2.sum<-pro2.sum%>%
  dplyr::mutate(
    Pcnt_PAs= (Int_area_KM2/Area_KM2.x)*100,
    Template = "Proactive 2",
    Priority= factor(quartile.2, 
                     levels=c(1,2,3),
                     labels = c("low", "Moderate", "High")))

pro2.sel<-pro2.sum%>%
  select(Template, Priority, Pcnt_PAs)


#Proactive 3
proactive.index.3<-(stress.inverse*2)+(res.fish.rich+res.mussel.rich)/2

temp<-as.data.frame(cbind(temp,proactive.index.3))

temp <- temp %>% mutate(quartile.3= ntile(proactive.index.3, 3))

pro3.sum<-temp %>% 
  select(quartile.3, Area_KM2.x, Int_area_KM2)%>%
  group_by(quartile.3) %>% 
  summarise(across(everything(), sum))

pro3.sum<-pro3.sum%>%
  dplyr::mutate(
    Pcnt_PAs= (Int_area_KM2/Area_KM2.x)*100,
    Template = "Proactive 3",
    Priority= factor(quartile.3, 
                     levels=c(1,2,3),
                     labels = c("low", "Moderate", "High")))

pro3.sel<-pro3.sum%>%
  select(Template, Priority, Pcnt_PAs)

#Reactive
reactive.index<-overall.stress.scaled+(res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2

temp<-as.data.frame(cbind(temp,reactive.index))

temp <- temp %>% mutate(quartile.4= ntile(reactive.index, 3))

react.sum<-temp %>% 
  select(quartile.4, Area_KM2.x, Int_area_KM2)%>%
  group_by(quartile.4) %>% 
  summarise(across(everything(), sum))

react.sum<-react.sum%>%
  dplyr::mutate(
    Pcnt_PAs= (Int_area_KM2/Area_KM2.x)*100,
    Template = "Reactive",
    Priority= factor(quartile.4, 
                 levels=c(1,2,3),
                 labels = c("low", "Moderate", "High")))

react.sel<-react.sum%>%
  select(Template, Priority, Pcnt_PAs)


#Representative
representative.index<-((res.fish.SAR.ratio+res.Mussel.SAR.ratio)/2)*2+((res.fish.rich+res.mussel.rich)/2)

temp<-as.data.frame(cbind(temp,representative.index))

temp <- temp %>% mutate(quartile.5= ntile(representative.index, 3))

rep.sum<-temp %>% 
  select(quartile.5, Area_KM2.x, Int_area_KM2)%>%
  group_by(quartile.5) %>% 
  summarise(across(everything(), sum))

rep.sum<-rep.sum%>%
  dplyr::mutate(
    Pcnt_PAs= (Int_area_KM2/Area_KM2.x)*100, 
    Template = "Representative",
    Priority= factor(quartile.5, 
                        levels=c(1,2,3),
                        labels = c("low", "Moderate", "High")))
rep.sel<-rep.sum%>%
  select(Template, Priority, Pcnt_PAs)

comb<-rbind(pro1.sel, pro2.sel, pro3.sel, react.sel, rep.sel)



summary_PAs<-comb%>%
  pivot_wider(values_from=Pcnt_PAs,
              names_from=Priority)
write.csv(summary_PAs, "Pcnt_PAs_by_template.csv")


#Continuation after generating priority ranking - priority ranking-2.R




temp<-as.data.frame(cbind(all,proactive.index.1, proactive.index.2, proactive.index.3, reactive.index, representative.index))
fix(temp)

temp <- temp %>% 
  mutate(P1_Q = ntile(proactive.index.1, 3),
         P2_Q = ntile(proactive.index.2, 3),
         P3_Q = ntile(proactive.index.3, 3),
         R_Q = ntile(reactive.index, 3),
         Rep_Q= ntile(representative.index, 3))

####Fish
##Manipulate fish data----
fish<-read.csv("data/Fish_atlas_DFO_combined_070821.csv")

fish[fish=="native"] <- 1
fish[fish=="SAR"] <- 100
fish[fish=="Foreign"] <- 0
fish[fish=="foreign"] <- 0
fish[fish=="extinct"] <- 0
fish[fish=="Translocated"] <- 0
fish[fish=="translocated"] <- 0

fish.sel<-fish%>%select(-c(HYBAS_ID, WSCSSDA))

fish.sel2 <- fish.sel %>% 
  mutate_all( funs(recode(., `1`=1, `100`=100, `0` = 0, .default = NaN)))

#Select non-zero columns only
fish.native<-fish.sel2 %>% select_if(~ !is.numeric(.) || sum(.) != 0)

#fish.native<-fish.sel2[,colSums(fish.sel2 != 0) > 0]

total.native<-ncol(fish.native)

#110 native fish species

total.SAR<-sum(colSums(fish.native)>100)

#18 SAR species

fish.ed<-as.data.frame(cbind(fish$HYBAS_ID, fish.native))

names(fish.ed)[1] <- "HYBAS_ID"

temp.sel<-temp%>%select(c(HYBAS_ID, P1_Q, P2_Q, P3_Q, R_Q, Rep_Q))

comb<-merge(temp.sel, fish.ed, by.x="HYBAS_ID")

fix(comb)

#Fish_analysis----

#Proactive 1

#Q3

P1_sel_3<-filter(comb, P1_Q == 3)

sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>0)

(sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>0))/total.native*100

sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>100)

(sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>100))/total.SAR*100

#Q_2

P1_sel_2<-filter(comb, P1_Q == 2)

sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>0)

(sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>0))/total.native*100

sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>100)

(sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>100))/total.SAR*100

#Q1

P1_sel_1<-filter(comb, P1_Q == 1)

sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>0)

(sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>0))/total.native*100

sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>100)

(sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>100))/total.SAR*100


#Proactive 2

#Q3

P2_sel_3<-filter(comb, P2_Q == 3)

sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>0)

(sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>0))/total.native*100

sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>100)

(sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>100))/total.SAR*100

#Q_2

P2_sel_2<-filter(comb, P2_Q == 2)

sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>0)

(sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>0))/total.native*100

sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>100)

(sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>100))/total.SAR*100

#Q1

P2_sel_1<-filter(comb, P2_Q == 1)

sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>0)

(sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>0))/total.native*100

sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>100)

(sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>100))/total.SAR*100


#Proactive 3

#Q3

P3_sel_3<-filter(comb, P3_Q == 3)

sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>0)

(sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>0))/total.native*100

sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>100)

(sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>100))/total.SAR*100

#Q_2

P3_sel_2<-filter(comb, P3_Q == 2)

sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>0)

(sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>0))/total.native*100

sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>100)

(sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>100))/total.SAR*100

#Q1

P3_sel_1<-filter(comb, P3_Q == 1)

sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>0)

(sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>0))/total.native*100

sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>100)

(sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>100))/total.SAR*100

#Reactive
R_sel_3<-filter(comb, R_Q == 3)

sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>0)

(sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>0))/total.native*100

sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>100)

(sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>100))/total.SAR*100

#Q_2

R_sel_2<-filter(comb, R_Q == 2)

sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>0)

(sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>0))/total.native*100

sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>100)

(sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>100))/total.SAR*100

#Q1

R_sel_1<-filter(comb, R_Q == 1)

sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>0)

(sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>0))/total.native*100

sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>100)

(sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>100))/total.SAR*100

#Representative

Rep_sel_3<-filter(comb, Rep_Q == 3)

sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>0)

(sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>0))/total.native*100

sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>100)

(sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>100))/total.SAR*100

#Q_2

Rep_sel_2<-filter(comb, Rep_Q == 2)

sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>0)

(sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>0))/total.native*100

sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>100)

(sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>100))/total.SAR*100

#Q1

Rep_sel_1<-filter(comb, Rep_Q == 1)

sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>0)

(sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>0))/total.native*100

sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>100)

(sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>100))/total.SAR*100

###Mussel

##Manipulate fish data----
mussel<-read.csv("data/mussel_DFO_atlas_combined_170621.csv")

mussel[mussel=="native"] <- 1
mussel[mussel=="SAR"] <- 100


mussel.sel<-mussel%>%select(-c(HYBAS_ID))

mussel.sel2 <- mussel.sel %>% 
  mutate_all( funs(recode(., `1`=1, `100`=100, `0` = 0, .default = NaN)))

mussel.sel2 <- na.omit(mussel.sel2)

#Select non-zero columns only
mussel.native<-mussel.sel2 %>% select_if(~  sum(.) != 0)

#mussel.native<-mussel.sel2[,colSums(mussel.sel2 != 0) > 0]

total.native<-ncol(mussel.native)

#40 native mussel species

total.SAR<-sum(colSums(mussel.native)>100)

#16 SAR species

mussel2<-na.omit(mussel)

mussel.ed<-as.data.frame(cbind(mussel2$HYBAS_ID, mussel.native))

names(mussel.ed)[1] <- "HYBAS_ID"

temp.sel<-temp%>%select(c(HYBAS_ID, P1_Q, P2_Q, P3_Q, R_Q, Rep_Q))

comb<-merge(temp.sel, mussel.ed, by.x="HYBAS_ID")

fix(comb)

#mussel_analysis----

#Proactive 1

#Q3

P1_sel_3<-filter(comb, P1_Q == 3)

sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>0)

(sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>0))/total.native*100

sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>100)

(sum(colSums(P1_sel_3[,7:ncol(P1_sel_3)])>100))/total.SAR*100

#Q_2

P1_sel_2<-filter(comb, P1_Q == 2)

sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>0)

(sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>0))/total.native*100

sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>100)

(sum(colSums(P1_sel_2[,7:ncol(P1_sel_2)])>100))/total.SAR*100

#Q1

P1_sel_1<-filter(comb, P1_Q == 1)

sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>0)

(sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>0))/total.native*100

sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>100)

(sum(colSums(P1_sel_1[,7:ncol(P1_sel_1)])>100))/total.SAR*100


#Proactive 2

#Q3

P2_sel_3<-filter(comb, P2_Q == 3)

sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>0)

(sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>0))/total.native*100

sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>100)

(sum(colSums(P2_sel_3[,7:ncol(P2_sel_3)])>100))/total.SAR*100

#Q_2

P2_sel_2<-filter(comb, P2_Q == 2)

sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>0)

(sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>0))/total.native*100

sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>100)

(sum(colSums(P2_sel_2[,7:ncol(P2_sel_2)])>100))/total.SAR*100

#Q1

P2_sel_1<-filter(comb, P2_Q == 1)

sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>0)

(sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>0))/total.native*100

sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>100)

(sum(colSums(P2_sel_1[,7:ncol(P2_sel_1)])>100))/total.SAR*100


#Proactive 3

#Q3

P3_sel_3<-filter(comb, P3_Q == 3)

sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>0)

(sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>0))/total.native*100

sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>100)

(sum(colSums(P3_sel_3[,7:ncol(P3_sel_3)])>100))/total.SAR*100

#Q_2

P3_sel_2<-filter(comb, P3_Q == 2)

sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>0)

(sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>0))/total.native*100

sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>100)

(sum(colSums(P3_sel_2[,7:ncol(P3_sel_2)])>100))/total.SAR*100

#Q1

P3_sel_1<-filter(comb, P3_Q == 1)

sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>0)

(sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>0))/total.native*100

sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>100)

(sum(colSums(P3_sel_1[,7:ncol(P3_sel_1)])>100))/total.SAR*100

#Reactive
R_sel_3<-filter(comb, R_Q == 3)

sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>0)

(sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>0))/total.native*100

sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>100)

(sum(colSums(R_sel_3[,7:ncol(R_sel_3)])>100))/total.SAR*100

#Q_2

R_sel_2<-filter(comb, R_Q == 2)

sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>0)

(sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>0))/total.native*100

sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>100)

(sum(colSums(R_sel_2[,7:ncol(R_sel_2)])>100))/total.SAR*100

#Q1

R_sel_1<-filter(comb, R_Q == 1)

sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>0)

(sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>0))/total.native*100

sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>100)

(sum(colSums(R_sel_1[,7:ncol(R_sel_1)])>100))/total.SAR*100

#Representative

Rep_sel_3<-filter(comb, Rep_Q == 3)

sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>0)

(sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>0))/total.native*100

sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>100)

(sum(colSums(Rep_sel_3[,7:ncol(Rep_sel_3)])>100))/total.SAR*100

#Q_2

Rep_sel_2<-filter(comb, Rep_Q == 2)

sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>0)

(sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>0))/total.native*100

sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>100)

(sum(colSums(Rep_sel_2[,7:ncol(Rep_sel_2)])>100))/total.SAR*100

#Q1

Rep_sel_1<-filter(comb, Rep_Q == 1)

sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>0)

(sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>0))/total.native*100

sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>100)

(sum(colSums(Rep_sel_1[,7:ncol(Rep_sel_1)])>100))/total.SAR*100


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
proactive.index.3<-(stress.inverse*2)+(res.mussel.rich+res.mussel.rich)/2

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
reactive.index<-overall.stress.scaled+(res.mussel.SAR.ratio+res.Mussel.SAR.ratio)/2

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
representative.index<-((res.mussel.SAR.ratio+res.Mussel.SAR.ratio)/2)*2+((res.mussel.rich+res.mussel.rich)/2)

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



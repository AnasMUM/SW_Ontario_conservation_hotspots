comb.n<-cbind(all, res.fish.rich, res.mussel.rich, res.fish.SAR.ratio, res.Mussel.SAR.ratio)

comb.n <- comb.n %>% mutate(fish.rich.quart= ntile(res.fish.rich, 3))

comb.n <- comb.n %>% mutate(mussel.rich.quart= ntile(res.mussel.rich, 3))

comb.n <- comb.n %>% mutate(fish.SAR.quart= ntile(res.fish.SAR.ratio, 3))

comb.n <- comb.n %>% mutate(mussel.SAR.quart= ntile(res.Mussel.SAR.ratio, 3))

comb.n$rich_diff<-ifelse(comb.n$fish.rich.quart!=3 & comb.n$mussel.rich.quart==3, 1, 0)

comb.n$SAR_diff<-ifelse(comb.n$fish.SAR.quart!=3 & comb.n$mussel.SAR.quart==3, 1, 0)

set1<-subset(comb.n, select = c("rich_diff", "SAR_diff"))

change1<-as.data.frame(((colSums(set1))/7)*100)
change1 <- tibble::rownames_to_column(change1, "Difference")
colnames(change1)<-c("Difference", "pcnt_change")

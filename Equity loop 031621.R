library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(stringi)
library(jtools)
library(ggplot2)
library(questionr)
library(viridis)
library(xlsx)
library(forcats)
library(data.table)
library(sjlabelled)

options(scipen=999)


surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Track20/Equity/Equity Loop List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                       colClasses = c("character", "character", "character", "numeric",
                                      "character", "character", "character", "character", "numeric",
                                      "character", "numeric", "character", "character", "character"));


surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

results_dsbelow <- setNames(data.frame(matrix(ncol = 11, nrow =0)), c( "Var1",     "Freq",     "National", "below10",  "below20",  "below30",  "Labels",   "Group",    "Country",  "Year" , "Survey")) %>%
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq), National=as.numeric(National), below10=as.numeric(below10), below20=as.numeric(below20), below30=as.numeric(below30), 
         Labels=as.character(Labels), Group=as.character(Group), Country=as.character(Country), Year=as.numeric(Year), Survey=as.character(Survey) )
results_ratio <- setNames(data.frame(matrix(ncol = 7, nrow =0)), c("Group" ,  "Ratio",   "Min",     "Max",     "Country" , "Year" , "Survey"  )) %>%
  mutate(Group=as.character(Group), Ratio=as.numeric(Ratio), Min=as.character(Min), Max=as.character(Max), Country=as.character(Country), Year=as.numeric(Year), Survey=as.character(Survey))
results_group <- setNames(data.frame(matrix(ncol = 10, nrow =0)), c("Var1",    "Freq",    "group" ,  "x"   ,    "label",   "rank" ,   "label1",  "Country", "Year" , "Survey"     )) %>%
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq), group=as.numeric(group), x=as.numeric(x), label=as.character(label), rank=as.numeric(rank), label1=as.character(label1), Country=as.character(Country), Year=as.numeric(Year), Survey=as.character(Survey))



for (row in 1:nrow(surveys)) {
  women_data <- surveys[row, "IRfile"]
  country <- surveys[row, "Country"]
  year <- surveys[row, "StartYear"]
  surveyid <- surveys[row, "Survey"]
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  women <- read_dta(women_data)

  # Outcomes
  women <- women %>% mutate(ds=case_when(v313==3 ~ 1, v626a==1 | v626a==2 | v626a==3 | v626a==4 ~ 0))
  
  # Inequality Groups
  # wealth (v190), education (recode- v149), geography (v024), age (v013), parity (top code- v201 )
  women <- women %>% mutate(parity=case_when(v201 <= 6 ~ v201,
                                             v201>6 ~ 6),
                            education=case_when(v149==0 ~ 0,
                                                v149==1 | v149==2 ~ 1,
                                                v149==3 | v149==4 ~ 2,
                                                v149==5 ~ 3))
  
  women$n <- 1
  
  # Sample Weights
  women$sampleweights <- women$v005/1000000
  design.women <- svydesign(ids=~v021, strata=~v023, weights=~sampleweights, data=women, nest=TRUE)
  options(survey.lonely.psu="adjust")
  
  national.ds <- as.data.frame(svymean(women$ds, design.women, na.rm=TRUE)) %>% select(mean)
  national <- national.ds$mean[1]*100
  
  
  
  
  # outcomes by group
  Labels=get_labels(women$v024)
  Var1=get_values(women$v024)
  regionnames=as.data.frame(cbind(Labels, Var1))
  
  ds_geog<- as.data.frame(prop.table(wtd.table(as.factor(women[ which(!is.na(women$ds)),]$v024), women[ which(!is.na(women$ds)),]$ds, weights = women[ which(!is.na(women$ds)),]$sampleweights),1)) %>% 
    filter(Var2==1) %>% mutate(Freq=Freq*100)  %>% select(-Var2)  %>% mutate(National=national) %>%
    mutate(below10=case_when(Freq < National*.9 ~ 1, Freq>=National*.9 ~ 0),
           below20=case_when(Freq < National*.8 ~ 1, Freq>=National*.8 ~ 0),
           below30=case_when(Freq < National*.7 ~ 1, Freq>=National*.7 ~ 0)) %>%
    full_join(regionnames, by="Var1") %>%  mutate(Group="Geography")
  
  geog_women <- ds_geog %>% rename(v024=Var1, below10_geog=below10,  below20_geog=below20,  below30_geog=below30) %>% mutate(v024=as.numeric(as.character(v024))) %>% select(v024, below10_geog, below20_geog, below30_geog)
  
  ds_wealth<- as.data.frame(prop.table(wtd.table(as.factor(women[ which(!is.na(women$ds)),]$v190), women[ which(!is.na(women$ds)),]$ds, weights = women[ which(!is.na(women$ds)),]$sampleweights),1)) %>% 
    filter(Var2==1) %>% mutate(Freq=Freq*100)  %>% select(-Var2)   %>% mutate(National=national) %>% 
    mutate(below10=case_when(Freq < National*.9 ~ 1, Freq>=National*.9 ~ 0),
           below20=case_when(Freq < National*.8 ~ 1, Freq>=National*.8 ~ 0),
           below30=case_when(Freq < National*.7 ~ 1, Freq>=National*.7 ~ 0)) %>% 
    mutate(Labels=case_when(Var1==1 ~ "Poorest", Var1==2 ~ "Poorer", Var1==3 ~ "Middle", Var1==4 ~ "Richer", Var1==5 ~ "Richest"))  %>% mutate(Group="Wealth")
  
  wealth_women <- ds_wealth %>% rename(v190=Var1, below10_wealth=below10,  below20_wealth=below20,  below30_wealth=below30) %>% mutate(v190=as.numeric(as.character(v190))) %>% select(v190, below10_wealth, below20_wealth, below30_wealth)
  
  ds_education<- as.data.frame(prop.table(wtd.table(women[ which(!is.na(women$ds)),]$education, women[ which(!is.na(women$ds)),]$ds, weights = women[ which(!is.na(women$ds)),]$sampleweights),1)) %>% 
    filter(Var2==1) %>% mutate(Freq=Freq*100)   %>% select(-Var2)   %>% mutate(National=national) %>% 
    mutate(below10=case_when(Freq < National*.9 ~ 1, Freq>=National*.9 ~ 0),
           below20=case_when(Freq < National*.8 ~ 1, Freq>=National*.8 ~ 0),
           below30=case_when(Freq < National*.7 ~ 1, Freq>=National*.7 ~ 0)) %>% 
    mutate(Labels=case_when(Var1==0 ~ "None", Var1==1 ~ "Primary", Var1==2 ~ "Secondary", Var1==3 ~ "Higher")) %>% mutate(Group="Education")
  
  education_women <- ds_education %>% rename(education=Var1, below10_education=below10,  below20_education=below20,  below30_education=below30) %>% mutate(education=as.numeric(as.character(education))) %>% select(education, below10_education, below20_education, below30_education)
  
  ds_age<- as.data.frame(prop.table(wtd.table(as.factor(women[ which(!is.na(women$ds)),]$v013), women[ which(!is.na(women$ds)),]$ds, weights = women[ which(!is.na(women$ds)),]$sampleweights),1)) %>%
    filter(Var2==1) %>% mutate(Freq=Freq*100)   %>% select(-Var2)   %>% mutate(National=national) %>% 
    mutate(below10=case_when(Freq < National*.9 ~ 1, Freq>=National*.9 ~ 0),
           below20=case_when(Freq < National*.8 ~ 1, Freq>=National*.8 ~ 0),
           below30=case_when(Freq < National*.7 ~ 1, Freq>=National*.7 ~ 0)) %>% 
    mutate(Labels=case_when(Var1==1 ~ "Age 15-19", Var1==2 ~ "Age 20-24", Var1==3 ~ "Age 25-29", Var1==4 ~ "Age 30-34", Var1==5 ~ "Age 35-39" , Var1==6 ~ "Age40-44" , Var1==7 ~ "Age 45-49"))  %>% mutate(Group="Age")
  
  age_women <- ds_age %>% rename(v013=Var1, below10_age=below10,  below20_age=below20,  below30_age=below30) %>% mutate(v013=as.numeric(as.character(v013)))  %>% select(v013, below10_age, below20_age, below30_age)
  
  ds_parity<- as.data.frame(prop.table(wtd.table(as.factor(women[ which(!is.na(women$ds)),]$parity), women[ which(!is.na(women$ds)),]$ds, weights = women[ which(!is.na(women$ds)),]$sampleweights),1)) %>% 
    filter(Var2==1) %>% mutate(Freq=Freq*100)   %>% select(-Var2)   %>% mutate(National=national) %>% 
    mutate(below10=case_when(Freq < National*.9 ~ 1, Freq>=National*.9 ~ 0),
           below20=case_when(Freq < National*.8 ~ 1, Freq>=National*.8 ~ 0),
           below30=case_when(Freq < National*.7 ~ 1, Freq>=National*.7 ~ 0)) %>% 
    mutate(Labels=case_when( Var1==0 ~ "Parity 0", Var1==1 ~ "Parity 1", Var1==2 ~ "Parity 2", Var1==3 ~ "Parity 3", Var1==4 ~ "Parity 4", Var1==5 ~ "Parity 5" , Var1==6 ~ "Parity 6+")) %>% mutate(Group="Parity")
  
  parity_women <- ds_parity %>% rename(parity=Var1, below10_parity=below10,  below20_parity=below20,  below30_parity=below30)   %>% mutate(parity=as.numeric(as.character(parity))) %>% select(parity, below10_parity, below20_parity, below30_parity)
  
  
  # will be used for table showing what groups have inequity and will be showed for DS level display
  ds_below <- bind_rows( ds_geog, ds_wealth, ds_education, ds_age, ds_parity)
  ds_below$Country <-country
  ds_below$Year <- year
  ds_below$Survey <- surveyid
  ################################################################
  
  # ratio of highest to lowest
  min_df <-  ds_below %>% group_by(Group) %>% 
    filter(Freq==min(Freq)) %>% rename(Min=Labels) %>% select(Min, Group)
  max_df <-  ds_below %>% group_by(Group) %>% 
    filter(Freq==max(Freq)) %>% rename(Max=Labels) %>% select(Max, Group)
  ratio_df <- ds_below %>%  group_by(Group) %>% 
    summarise(min=min(Freq), max=max(Freq)) %>% mutate(Ratio=max/min) %>% select(Group, Ratio) %>% 
    full_join(min_df, by="Group") %>% full_join(max_df, by="Group")
  
  ratio_df$Country <-country
  ratio_df$Year <- year
  ratio_df$Survey <- surveyid
  
  ################################################################
  # adding belows back into women group
  women <- women %>% mutate(v024= as.numeric(as.character(v024)),
                            v190= as.numeric(as.character(v190)),
                            v013= as.numeric(as.character(v013))) %>%
    full_join(geog_women, by="v024") %>% 
    full_join(wealth_women, by="v190") %>% 
    full_join(education_women, by="education") %>% 
    full_join(age_women, by="v013") %>% 
    full_join(parity_women, by="parity") %>%
    filter(v013<=7)
  
  
  # Concentration of Inequality
  #women <- women %>%  mutate(ds_score= score_ds_geog+ score_ds_wealth + score_ds_educ+ score_ds_age + score_ds_par)
  
  women <- women %>%  mutate(score_below10=below10_geog+below10_wealth+below10_education+below10_age+below10_parity,
                             score_below20=below20_geog+below20_wealth+below20_education+below20_age+below20_parity,
                             score_below30=below30_geog+below30_wealth+below30_education+below30_age+below30_parity)
  
  women <- women %>% mutate(score_group_num10=case_when(score_below10==0 ~ "0",
                                                        score_below10==1 ~ "1:",
                                                        score_below10==2 ~ "2:",
                                                        score_below10==3 ~ "3:",
                                                        score_below10==4 ~ "4:",
                                                        score_below10==5 ~ "5:"),
                            score_group_num20=case_when(score_below20==0 ~ "0",
                                                        score_below20==1 ~ "1:",
                                                        score_below20==2 ~ "2:",
                                                        score_below20==3 ~ "3:",
                                                        score_below20==4 ~ "4:",
                                                        score_below20==5 ~ "5:"),
                            score_group_num30=case_when(score_below30==0 ~ "0",
                                                        score_below30==1 ~ "1:",
                                                        score_below30==2 ~ "2:",
                                                        score_below30==3 ~ "3:",
                                                        score_below30==4 ~ "4:",
                                                        score_below30==5 ~ "5:"),
                            score_ds_geognum10=case_when(below10_geog==1 ~ "G", below10_geog==0 ~ ""),
                            score_ds_wealthnum10=case_when(below10_wealth==1 ~ "W", below10_wealth==0 ~ ""),
                            score_ds_educnum10=case_when(below10_education==1 ~ "E", below10_education==0 ~ ""),
                            score_ds_agenum10=case_when(below10_age==1 ~ "A", below10_age==0 ~ ""),
                            score_ds_parnum10=case_when(below10_parity==1 ~ "P", below10_parity==0 ~ "") ,
                            score_group10=paste(score_group_num10, score_ds_agenum10, score_ds_educnum10, score_ds_geognum10,    score_ds_parnum10, score_ds_wealthnum10, sep="" ),
                            score_ds_geognum20=case_when(below20_geog==1 ~ "G", below20_geog==0 ~ ""),
                            score_ds_wealthnum20=case_when(below20_wealth==1 ~ "W", below20_wealth==0 ~ ""),
                            score_ds_educnum20=case_when(below20_education==1 ~ "E", below20_education==0 ~ ""),
                            score_ds_agenum20=case_when(below20_age==1 ~ "A", below20_age==0 ~ ""),
                            score_ds_parnum20=case_when(below20_parity==1 ~ "P", below20_parity==0 ~ "") ,
                            score_group20=paste(score_group_num20, score_ds_agenum20, score_ds_educnum20, score_ds_geognum20,    score_ds_parnum20, score_ds_wealthnum20, sep="" ),
                            score_ds_geognum30=case_when(below30_geog==1 ~ "G", below30_geog==0 ~ ""),
                            score_ds_wealthnum30=case_when(below30_wealth==1 ~ "W", below30_wealth==0 ~ ""),
                            score_ds_educnum30=case_when(below30_education==1 ~ "E", below30_education==0 ~ ""),
                            score_ds_agenum30=case_when(below30_age==1 ~ "A", below30_age==0 ~ ""),
                            score_ds_parnum30=case_when(below30_parity==1 ~ "P", below30_parity==0 ~ "") ,
                            score_group30=paste(score_group_num30, score_ds_agenum30, score_ds_educnum30, score_ds_geognum30,    score_ds_parnum30, score_ds_wealthnum30, sep="" ))
  
  
  
  # Design
  design.women <- svydesign(ids=~v021, strata=~v023, weights=~sampleweights, data=women, nest=TRUE)
  ####################################################################################################
  groups10<- as.data.frame(prop.table(wtd.table(women[ which(!is.na(women$score_group10) ),]$score_group10,  weights = women[ which(!is.na(women$score_group10)),]$sampleweights))) %>% mutate(Freq=Freq*100, group=10)
  groups20<- as.data.frame(prop.table(wtd.table(women[ which(!is.na(women$score_group20) ),]$score_group20,  weights = women[ which(!is.na(women$score_group20)),]$sampleweights))) %>% mutate(Freq=Freq*100, group=20)
  groups30<- as.data.frame(prop.table(wtd.table(women[ which(!is.na(women$score_group30) ),]$score_group30,  weights = women[ which(!is.na(women$score_group30)),]$sampleweights))) %>% mutate(Freq=Freq*100, group=30)
  
  groups <- bind_rows(groups10, groups20, groups30)
  
  groups$x <- 1
  
  groups$label <- substr(groups$Var1, 3, 20)
  
  groups <- groups %>% group_by(group) %>% mutate(rank=rank(Freq)) %>% mutate(rank=case_when(Var1=="0" ~ 0, Var1!="0" ~ rank))
  groups$label1 <- as.character(groups$Var1)
  groups$label1 <- ifelse(groups$Freq>2.5, groups$label1 , " ")
  
  groups$Country <-country
  groups$Year <- year
  groups$Survey <- surveyid
  groups <- groups %>% mutate(Var1=as.character(Var1))
  ####################################################################################################
  

  
  results_dsbelow <- bind_rows(results_dsbelow, ds_below)
  results_ratio <- bind_rows(results_ratio, ratio_df)
  results_group <- bind_rows(results_group, groups)
  
  
  
}




results_ratio_clean <- results_ratio %>% mutate(Min=str_to_title(Min), Max=str_to_title(Max)) %>% mutate(Min=case_when(Min=="Age40-44" ~ "Age 40-44", Min!= "Age40-44" ~ Min))
results_dsbelow_clean <- results_dsbelow  %>% mutate(Labels=str_to_title(Labels)) %>% mutate(Labels=case_when(Labels=="Age40-44" ~ "Age 40-44", Labels!= "Age40-44" ~ Labels))

write.csv(results_dsbelow_clean, "C:/Users/KristinBietsch/files/Track20/Equity/DHSEquity_dsbelow080421.csv", row.names = F)
write.csv(results_ratio_clean, "C:/Users/KristinBietsch/files/Track20/Equity/DHSEquity_ratio080421.csv", row.names = F)
write.csv(results_group, "C:/Users/KristinBietsch/files/Track20/Equity/DHSEquity_group080421.csv", row.names = F)


results_below_clean <- results_below %>% 
  filter(below==1) %>%
  group_by(Category, Country, Year) %>%
  summarise(group_list = paste0(Group, collapse = ", ")) %>%
  ungroup() %>%
  select(Country, Year, Category, group_list) %>%
  spread(Category, group_list)

#write.csv(results_below_clean, "C:/Users/KristinBietsch/files/Track20/Equity/DHSEquityResults2_123020.csv", row.names = F, na="")

####################################################################################################
####################################################################################################
####################################################################################################

selected_data <- results %>% filter(Country=="Ethiopia")

# https://stackoverflow.com/questions/61784635/stack-bars-in-plot-without-preserving-label-order/61839573#61839573
selected_data1 <- setDT(selected_data)[, .(frequencySum=sum(Freq)), by=.(Year,Var1)]

selected_data <- full_join(selected_data, selected_data1, by=c("Year", "Var1"))

selected_data <- selected_data[order(selected_data$Freq, decreasing = TRUE),] # (1)
selected_data$Freq<-factor(selected_data$Freq, levels = unique(selected_data$Freq) )

ggplot(selected_data)+
  geom_bar(aes(x=as.factor(Year), y=frequencySum, group=Freq, fill=Var1), 
           stat="identity", color="white")   +
  scale_fill_viridis_d( alpha=.4) +
  geom_text(aes(x=as.factor(Year), y=frequencySum, group=Freq, label = label1), size = 6,   position = position_stack(vjust = 0.5))  +
  labs(title="Concentration of Inequality:\nIn Groups 10% below National Average", x="", y="Proportion of Women")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background =element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(1,0,0,0), "cm"),
        axis.text = element_text(color="black", size=14) ,
        axis.title = element_text(color="black", size=14) ,
        plot.title = element_text(size=22),
        legend.position = "none")




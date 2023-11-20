

## @knitr dataSection
library(intsvy)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)

bcg15 <- read_rds("../2015/bcg.rds")
bsa15 <- read_rds("../2015/bsa.rds")
bsg15 <- read_rds("../2015/bsg.rds")
#bsr15 <- read_rds("../2015/bsr.rds")
bst15 <- read_rds("../2015/bst.rds")
btm15 <- read_rds("../2015/btm.rds")
bts15 <- read_rds("../2015/bts.rds")

acg15 <- read_rds("../2015/acg.rds")
asa15 <- read_rds("../2015/asa.rds")
asg15 <- read_rds("../2015/asg.rds")
ash15 <- read_rds("../2015/ash.rds")
#asr15 <- read_rds("../2015/asr.rds")
ast15 <- read_rds("../2015/ast.rds")
atg15 <- read_rds("../2015/atg.rds")

######### 2011 TIMSS ########
bcg11 <- read_rds("../2011/bcg.rds")
bsa11 <- read_rds("../2011/bsa.rds")
bsg11 <- read_rds("../2011/bsg.rds")
#bsr11 <- read_rds("../2011/bsr.rds")
bst11 <- read_rds("../2011/bst.rds")
btm11 <- read_rds("../2011/btm.rds")
bts11 <- read_rds("../2011/bts.rds")

######### 2007 TIMSS ########

bcg07 <- read_rds("../2007/bcg.rds")
bsa07 <- read_rds("../2007/bsa.rds")
bsg07 <- read_rds("../2007/bsg.rds")
#bsr07 <- read_rds("../2007/bsr.rds")
bst07 <- read_rds("../2007/bst.rds")
btm07 <- read_rds("../2007/btm.rds")
bts07 <- read_rds("../2007/bts.rds")

######### 2003 TIMSS ########

bcg03 <- read_rds("../2003/bcg.rds")
bsa03 <- read_rds("../2003/bsa.rds")
bsg03 <- read_rds("../2003/bsg.rds")
#bsr03 <- read_rds("../2003/bsr.rds")
bst03 <- read_rds("../2003/bst.rds")
btm03 <- read_rds("../2003/btm.rds")
bts03 <- read_rds("../2003/bts.rds")
acgPirls16 <- read_rds("../Pirls2016/ACG_Pirls16.rds")
asaPirls16 <- read_rds("../Pirls2016/ASA_Pirls16.rds")
asgPirls16 <- read_rds("../Pirls2016/ASG_Pirls16.rds")
ashPirls16 <- read_rds("../Pirls2016/ASH_Pirls16.rds")
astPirls16 <- read_rds("../Pirls2016/AST_Pirls16.rds")
atgPirls16 <- read_rds("../Pirls2016/ATG_Pirls16.rds")


student.statPirls16 <- asaPirls16 %>% 
  group_by(IDCNTRY,IDSTUD,IDSCHOOL) %>% 
  mutate(score = mean(ASRREA01:ASRREA05)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,score)

student.statPirls16 %>% 
  group_by(IDCNTRY,IDSCHOOL) %>% 
  summarise(score = mean(score)) -> schoolPerformancePirls16 

astPirls16 %>% 
  mutate(overall = (ASRREA01 + ASRREA02 + ASRREA03 + ASRREA04 + ASRREA05)/5) %>%
  select(IDCNTRY, IDSTUD, IDSCHOOL,IDTEACH,overall) %>%
  group_by(IDCNTRY, IDSCHOOL,IDTEACH) %>% 
  summarise(score = mean(overall))-> teacherPerformancePirls16

student.stat8th <- read_rds("../2015/bsa.rds") %>%
  group_by(idcntry, idstud, idschool) %>%
  mutate(mat = mean(bsmmat01:bsmmat05), 
         sci = mean(bsssci01:bsssci05)) %>%
  select(idcntry, idstud, idschool, mat, sci) %>%
  mutate(overall = (mat + sci) / 2)

student.stat4th <- read_rds("../2015/asa.rds") %>%
  group_by(idcntry, idstud, idschool) %>%
  mutate(mat = mean(asmmat01:asmmat05), 
         sci = mean(asssci01:asssci05)) %>%
  select(idcntry, idstud, idschool, mat, sci) %>%
  mutate(overall = (mat + sci) / 2)

student.stat4th %>% 
  group_by(idcntry,idschool) %>% 
  summarise(score = mean(overall)) -> schoolPerformance4th

student.stat8th %>% 
  group_by(idcntry,idschool) %>% 
  summarise(score = mean(overall)) -> schoolPerformance8th

ast15 %>% 
  mutate(mat = (asmmat01+asmmat02+ asmmat03+asmmat04+asmmat05)/5, 
         sci = (asssci01+asssci02+asssci03+asssci04+asssci05)/5) %>%
  select(idcntry, idstud, idschool,idteach, mat, sci) %>%
  mutate(overall = (mat + sci) / 2) %>% 
  group_by(idcntry,idschool,idteach) %>% 
  summarise(score = mean(overall))-> teacherPerformance4th

bst15 %>% 
  mutate(mat = (bsmmat01+bsmmat02+ bsmmat03+bsmmat04+bsmmat05)/5, 
         sci = (bsssci01+bsssci02+bsssci03+bsssci04+bsssci05)/5) %>%
  select(idcntry, idstud, idschool,idteach, mat, sci) %>%
  mutate(overall = (mat + sci) / 2) %>% 
  group_by(idcntry,idschool,idteach) %>% 
  summarise(score = mean(overall))-> teacherPerformance8th

student.statPirls16 %>% 
  group_by(IDCNTRY) %>% 
  summarise(score = mean(score)) %>% 
  arrange(desc(score)) -> countriesPerformancePirls16

ISO_3166_1 %>% 
  select(Country = Name , IDCNTRY = Numeric) -> countryCodes
c("England","Northern Ireland","Belgium (Flemish)","Dubai","Belgium (French)") -> Country
c(928,926,956,7841,957) -> IDCNTRY
data.frame(Country,IDCNTRY) -> df
rbind(countryCodes,df)->countryCodes
countryCodes$IDCNTRY <- as.double(countryCodes$IDCNTRY)

left_join(countriesPerformancePirls16,countryCodes) -> countriesPerformancePirls16

countriesPerformancePirls16 %>% 
  filter(!is.na(Country)) %>% 
  arrange(desc(score)) -> countriesPerformancePirls16

countriesPerformancePirls16 %>% 
  slice(1:5) -> Top5Pirls16
#####
student.statPirls16 %>% 
  mutate(benchmark = ifelse(score >= 625 ,"Advanced",
                            ifelse(score >= 550 , "High",
                                   ifelse(score >= 475 , "Intermediate","Low")))) -> student.BenchmarkStatPirls16

student.BenchmarkStatPirls16 %>% 
  group_by(IDCNTRY,benchmark) %>% 
  summarise(count = n()) -> countriesBenchmarksPirls16

student.statPirls16 %>% 
  group_by(IDCNTRY) %>% 
  summarise(all = n()) -> countries

left_join(countriesBenchmarksPirls16,countries) -> countriesBenchmarksPirls16
left_join(countriesBenchmarksPirls16,countryCodes) ->countriesBenchmarksPirls16
countriesBenchmarksPirls16 %>% 
  filter(!is.na(Country)) -> countriesBenchmarksPirls16


countriesBenchmarksPirls16 %>% 
  group_by(IDCNTRY,benchmark) %>% 
  mutate(percentage = round((count / all) * 100,0)) -> countriesBenchmarksPirls16

download_map_data("custom/world") -> world


## @knitr timss8th

## TIMSS 8TH GRADE 2015 


# 2015 --------------------------------------------------------------------

### GATHERING THE DATA 
res15 <- bsg15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
res15f <- res15 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
res15m <- res15 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
res15 <- full_join(res15m, res15f, by = "idcntry") 

res15 <- res15 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2015)
results <- res15

## DETERMINING THE CODE FOR EACH COUNTRY
library(ISOcodes)
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2) %>%
  mutate(idcntry = as.character(idcntry))
results <- data.frame(apply(results, 2, function(x) round(as.numeric(x), 3))) %>%
  mutate(idcntry = as.character(idcntry))
results <- left_join(results, codes, by = "idcntry") %>%
  filter(!is.na(country)) %>%
  mutate(country = tolower(country)) %>%
  group_by(country) %>%
  arrange(year)

## CHANGING THE NAME TO MORE MEANINGFUL ONES

results <- results %>%
  select(`Total Participants` = totcount, 
         `Average` = totoavg, 
         `Mathematics Average` = totmavg, 
         `Science Average` = totsavg,
         `Female Participants` = female_count, 
         `Female Average` = female_oavg, 
         `Female Mathematics Average` = female_mavg,
         `Female Science Average` = female_savg,
         `Male Participants` = male_count, 
         `Male Average` = male_oavg, 
         `Male Mathematics Average` = male_mavg,
         `Male Science Average` = male_savg,
         `International Rank` = rank,
         year,
         country
  )

### CONSTRUCTING THE TOOLTIP
ttvars <- names(results %>%
                  ungroup() %>%
                  select(-year, -country))
tt <- tooltip_table(
  ttvars,
  sprintf("{point.%s}", ttvars), style = "text-align: left;")

### COLOR 

library("viridisLite")
cols <-viridis(7, alpha = 1, begin = 0, end = 1, direction = -1)
cols <- substr(cols, 0, 7)

highchart() %>% 
  hc_add_series_map(world,results,name="Average Score",
                    value = "Average", joinBy = c("hc-key", "country"),
                    dataLabels = list(enabled = F,
                                      format = '{point.name}'), 
                    borderWidth = 0.25) %>% 
  hc_title(text = "Timss 2015 Wordwide 8th Grade Results") %>% 
  hc_subtitle(text = "Colored by Average Score") %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_colorAxis(stops = color_stops(n = 7, cols)) %>% 
  hc_tooltip(table = T ,headerFormat = as.character(tags$h4("{point.key}", tags$br())),
             pointFormat = tt,
             positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: 450 + 0 }; }"),
             style = list(color = "white", fontSize = "0.8em", fontWeight = "normal"),
             shape = "square", backgroundColor = 'rgb(43, 59, 112)',
             borderColor = "transparent",
             shadow = T) %>% 
  hc_size(height = 850,width = 1100)

rm(results, res15, res15m, res15f)

## @knitr timss4th

## TIMSS 4TH GRADE 2015 

# 2015 --------------------------------------------------------------------

### GATHERING THE DATA 
ares15 <- asg15 %>%
  mutate(mat = (asmmat01 + asmmat02 + asmmat03 + asmmat04 + asmmat05) / 5, 
         sci = (asssci01 + asssci02 + asssci03 + asssci04 + asssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
ares15f <- ares15 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
ares15m <- ares15 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
ares15 <- full_join(ares15m, ares15f, by = "idcntry") 

ares15 <- ares15 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2015)
aresults <- ares15

## DETERMINING THE CODE FOR EACH COUNTRY
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2) %>%
  mutate(idcntry = as.character(idcntry))
aresults <- data.frame(apply(aresults, 2, function(x) round(as.numeric(x), 3))) %>%
  mutate(idcntry = as.character(idcntry))
aresults <- left_join(aresults, codes, by = "idcntry") %>%
  filter(!is.na(country)) %>%
  mutate(country = tolower(country)) %>%
  group_by(country) %>%
  arrange(year)

## CHANGING THE NAME TO MORE MEANINGFUL ONES

aresults <- aresults %>%
  select(`Total Participants` = totcount, 
         `Average` = totoavg, 
         `Mathematics Average` = totmavg, 
         `Science Average` = totsavg,
         `Female Participants` = female_count, 
         `Female Average` = female_oavg, 
         `Female Mathematics Average` = female_mavg,
         `Female Science Average` = female_savg,
         `Male Participants` = male_count, 
         `Male Average` = male_oavg, 
         `Male Mathematics Average` = male_mavg,
         `Male Science Average` = male_savg,
         `International Rank` = rank,
         year,
         country
  )

### CONSTRUCTING THE TOOLTIP
attvars <- names(aresults %>%
                   ungroup() %>%
                   select(-year, -country))
att <- tooltip_table(
  attvars,
  sprintf("{point.%s}", attvars), style = "text-align: left;")

### DOWNLOADING THE MAP



### COLOR 

library("viridisLite")
acols <-magma(7, alpha = 1, begin = 0, end = 1, direction = -1)
acols <- substr(acols, 0, 7)

## DRAWING THE MAP 
highchart() %>% 
  hc_add_series_map(world,aresults,name="Average Score",
                    value = "Average", joinBy = c("hc-key", "country"),
                    dataLabels = list(enabled = F,
                                      format = '{point.name}'), 
                    borderWidth = 0.25) %>% 
  hc_title(text = "Timss 2015 Wordwide 4th Grade Results") %>% 
  hc_subtitle(text = "Colored by Average Score") %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_colorAxis(stops = color_stops(n = 7, acols)) %>% 
  hc_tooltip(table = T ,headerFormat = as.character(tags$h4("{point.key}", tags$br())),
             pointFormat = att,
             positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: 450 + 0 }; }"),
             style = list(color = "white", fontSize = "0.8em", fontWeight = "normal"),
             shape = "square", backgroundColor = 'rgb(185, 150, 210)',
             borderColor = "transparent",
             shadow = T)%>% 
  hc_size(height = 850,width = 1100)

rm(aresults, ares15, ares15m, ares15f)

## @knitr pirls
ISO_3166_1 %>% 
  select(Country = Alpha_2 , IDCNTRY = Numeric) -> countryCodes2
c("gb","be","ae") -> Country
c(928,956,7841) -> IDCNTRY
data.frame(Country,IDCNTRY) -> df2
rbind(countryCodes2,df2)->countryCodes2
countryCodes2$IDCNTRY <- as.double(countryCodes2$IDCNTRY)

countriesBenchmarksPirls16 %>% 
  select(-Country) -> countriesBenchmarksMapPirls16

left_join(countriesBenchmarksMapPirls16,countryCodes2) -> countriesBenchmarksMapPirls16
left_join(countriesBenchmarksMapPirls16,countriesPerformancePirls16,by = "IDCNTRY") %>% 
  select(IDCNTRY,benchmark,count,all,percentage,Country = Country.x,AvgScore = score) -> countriesBenchmarksMapPirls16
countriesBenchmarksMapPirls16 %>% 
  filter(!is.na(Country))->countriesBenchmarksMapPirls16

countriesBenchmarksMapPirls16 %>% 
  select(IDCNTRY,Country,AvgScore,all,benchmark,percentage) %>% 
  spread(key = benchmark,value = percentage) -> countriesBenchmarksMapTidyPirls16

countriesBenchmarksMapTidyPirls16$Country <- tolower(countriesBenchmarksMapTidyPirls16$Country)
countriesBenchmarksMapTidyPirls16 %>% 
  select(IDCNTRY,Country,Participants = all ,AvgScore,Advanced,High,Intermediate,Low) -> countriesBenchmarksMapTidyPirls16
countriesBenchmarksMapTidyPirls16$AvgScore <- round(countriesBenchmarksMapTidyPirls16$AvgScore,0)


countriesBenchmarksMapTidyPirls16$Advanced <- paste(countriesBenchmarksMapTidyPirls16$Advanced,"%") 
countriesBenchmarksMapTidyPirls16$High <- paste(countriesBenchmarksMapTidyPirls16$High,"%") 
countriesBenchmarksMapTidyPirls16$Intermediate <- paste(countriesBenchmarksMapTidyPirls16$Intermediate,"%") 
countriesBenchmarksMapTidyPirls16$Low <- paste(countriesBenchmarksMapTidyPirls16$Low,"%") 

colnames(countriesBenchmarksMapTidyPirls16)[5] <- "Advanced Benchmark Percentage"
colnames(countriesBenchmarksMapTidyPirls16)[6] <- "High Benchmark Percentage"
colnames(countriesBenchmarksMapTidyPirls16)[7] <- "Intermediate Benchmark Percentage"
colnames(countriesBenchmarksMapTidyPirls16)[8] <- "Low Benchmark Percentage"

ttvars <- names(countriesBenchmarksMapTidyPirls16 %>%
                  ungroup() %>%
                  select(-IDCNTRY, -Country))
tt <- tooltip_table(
  ttvars,
  sprintf("{point.%s}", ttvars), style = "text-align: left;")

highchart() %>% 
  hc_add_series_map(world,countriesBenchmarksMapTidyPirls16,name="AvgScore",
                    value = "AvgScore", joinBy = c("hc-key", "Country"),
                    dataLabels = list(enabled = T,
                                      format = '{point.name}')) %>% 
  hc_tooltip(table = T ,headerFormat = as.character(tags$h4("{point.key}", tags$br())),
             pointFormat = tt,
             positioner = JS("function () { return { x: this.chart.plotLeft , y: 450 + 0 }; }"),
             style = list(color = "black", fontSize = "1em", fontWeight = "normal"),shape = "square",backgroundColor = "transparent",
             borderColor = "transparent",
             shadow = T) %>% 
  hc_legend(title = list(text = "Average Score")) %>% 
  hc_title(text = "Pirls 2015 Worldwide") %>% 
  hc_size(height = 850,width = 1100)

## @knitr timssMotion
###### TIMSS 8TH GRADE MOTION MAP

# 2015 --------------------------------------------------------------------


res15 <- bsg15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
res15f <- res15 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
res15m <- res15 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
res15 <- full_join(res15m, res15f, by = "idcntry") 

res15 <- res15 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2015)


# 2011 --------------------------------------------------------------------
res11 <- bsg11 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
res11f <- res11 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
res11m <- res11 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
res11 <- full_join(res11m, res11f, by = "idcntry") 

res11 <- res11 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2011)

# 2007 --------------------------------------------------------------------


res07 <- bsg07 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
res07f <- res07 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
res07m <- res07 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
res07 <- full_join(res07m, res07f, by = "idcntry")

res07 <- res07 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2007)
# 2003 --------------------------------------------------------------------

res03 <- bsg03 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5, 
         overall = (mat + sci) / 2) %>%
  mutate(itsex = ifelse(itsex == 1, "female", "male")) %>%
  group_by(idcntry, gender = itsex) %>%
  filter(!is.na(gender)) %>%
  summarise(count = n(), savg = mean(sci, na.rm = T), mavg = mean(mat, na.rm = T), oavg = mean(overall, na.rm = T)) %>%
  mutate(status = paste(count, savg, mavg, oavg, sep = " ")) %>%
  spread(gender, status) %>%
  separate(female, c("female_count", "female_savg", "female_mavg", "female_oavg"), sep = " ") %>%
  separate(male, c("male_count", "male_savg", "male_mavg", "male_oavg"), sep = " ", convert = F) %>%
  select(-count, -savg, -mavg, -oavg) 
res03f <- res03 %>%
  filter(!is.na(female_count)) %>%
  select(idcntry, female_count, female_savg, female_mavg, female_oavg)
res03m <- res03 %>%
  filter(!is.na(male_count)) %>%
  select(idcntry, male_count, male_savg, male_mavg, male_oavg)
res03 <- full_join(res03m, res03f, by = "idcntry")

res03 <- res03 %>%
  mutate(female_count = as.numeric(female_count), 
         male_count = as.numeric(male_count), 
         male_oavg = as.numeric(male_oavg), 
         female_oavg = as.numeric(female_oavg), 
         male_mavg = as.numeric(male_mavg), 
         female_mavg = as.numeric(female_mavg), 
         female_savg = as.numeric(female_savg),
         male_savg = as.numeric(male_savg),
         totcount = female_count + male_count, 
         totoavg = ((male_count * male_oavg) + (female_count * female_oavg)) / totcount, 
         totmavg = ((male_count * male_mavg) + (female_count * female_mavg)) / totcount, 
         totsavg = ((male_count * male_savg) + (female_count * female_savg)) / totcount) %>%
  ungroup() %>%
  arrange(desc(totoavg)) %>%
  mutate(rank = rank(-totoavg)) %>%
  mutate(idcntry = ifelse(idcntry < 100, paste("0", idcntry, sep = ""), idcntry)) %>%
  mutate(idcntry = as.character(idcntry)) %>%
  mutate(year = 2003)


# gather it all ___________________________________________________________________________
results <- rbind(res15, res11, res07, res03)

codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2) %>%
  mutate(idcntry = as.character(idcntry))

results <- data.frame(apply(results, 2, function(x) round(as.numeric(x), 3))) %>%
  mutate(idcntry = as.character(idcntry))

results <- left_join(results, codes, by = "idcntry") %>%
  filter(!is.na(country)) %>%
  mutate(country = tolower(country)) %>%
  group_by(country) %>%
  arrange(year)

results <- results %>%
  select(`Total Participants` = totcount, 
         score = totoavg, 
         `Mathematics Average` = totmavg, 
         `Science Average` = totsavg,
         `Female Participants` = female_count, 
         `Female Average` = female_oavg, 
         `Female Mathematics Average` = female_mavg,
         `Female Science Average` = female_savg,
         `Male Participants` = male_count, 
         `Male Average` = male_oavg, 
         `Male Mathematics Average` = male_mavg,
         `Male Science Average` = male_savg,
         `International Rank` = rank,
         year,
         country
  )
results <- results %>%
  select(year, country, score)

yechizi <- results %>%
  group_by(country) %>%
  do(item = list(
    country = first(.$country), 
    sequence = .$score, 
    value = first(.$score)
  )) %>%
  .$item

################### tooltips 

ttvars <- c("country", "value")
tt <- tooltip_table(
  ttvars,
  sprintf("{point.%s}", ttvars), style = "text-align: left;")

#### map 

highchart(type = "map") %>% 
  hc_add_series(data = yechizi,
                name = "Average Score",
                value = "score",
                mapData = world,
                joinBy = c("hc-key", "country"),
                borderWidth = 0.01) %>% 
  hc_title(text = "Timss 8th Grade Results Throught The Years") %>%
  hc_legend(reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_colorAxis(stops = color_stops(n = 10, cols)) %>% 
  hc_motion(
    enabled = TRUE,
    axisLabel = "year",
    labels = sort(unique(results$year)),
    series = 0,
    updateIterval = 1000
  ) %>%
  hc_tooltip(table = T ,headerFormat = as.character(tags$h4("{point.key}", tags$br())),
             pointFormat = tt,
             positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: 350 + 0 }; }"),
             style = list(color = "white", fontSize = "0.75em", 
                          fontWeight = "normal"),shape = "square",backgroundColor = 'purple',
             borderColor = "black",
             shadow = T)%>% 
  hc_size(height = 850,width = 1100)

rm(res15, res15f, res15m, res11, res11m, res11f, res07, res07m, res07f, 
   res03, res03m, res03f, results)


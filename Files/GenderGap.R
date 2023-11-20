## @knitr dataSection
library(jsonlite)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(ggthemr)

bsgTimss15 <- read_rds("../2015/bsg.rds")
asgTimss15 <- read_rds("../2015/asg.rds")
bsgTimss11 <- read_rds("../2011/bsg.rds")
asgTimss11 <- read_rds("../2011/asg.rds")
bsgTimss07 <- read_rds("../2007/bsg.rds")
asgTimss07 <- read_rds("../2007/asg.rds")
bsgTimss03 <- read_rds("../2003/bsg.rds")
asgTimss03 <- read_rds("../2003/asg.rds")


bsgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th15

asgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th15

bsgTimss11 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th11

asgTimss11 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th11

bsgTimss07 %>% 
  select(idcntry,idschool,idstud,itsex,bsmmat01:bsmmat05,bsssci01:bsssci05) %>% 
  drop_na() %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) -> student.stat8th07

asgTimss07 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th07

bsgTimss03 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th03

asgTimss03 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th03


student.stat4th03 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st4th03

student.stat8th03 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st8th03

student.stat4th07 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st4th07

student.stat8th07 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st8th07

student.stat4th11 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st4th11

student.stat8th11 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st8th11

student.stat4th15 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st4th15

student.stat8th15 %>%
  group_by(idcntry,itsex) %>%
  filter(!is.na(itsex)) %>% 
  summarise(mathScore = ceiling(mean(mathScore)),scienceScore = ceiling(mean(scienceScore))) %>% 
  mutate(itsex = ifelse(itsex == 1,"Female","Male")) -> st8th15

rbind(st4th03 %>% filter(idcntry == 364) %>% mutate(Year = 2003),st4th07 %>% filter(idcntry == 364)%>% mutate(Year = 2007),
      st4th11 %>% filter(idcntry == 364) %>% mutate(Year = 2011), st4th15 %>% filter(idcntry == 364)%>% mutate(Year = 2015)) -> genderGapIran4th


rbind(st8th03 %>% filter(idcntry == 364) %>% mutate(Year = 2003),st8th07 %>% filter(idcntry == 364)%>% mutate(Year = 2007),
      st8th11 %>% filter(idcntry == 364) %>% mutate(Year = 2011), st8th15 %>% filter(idcntry == 364)%>% mutate(Year = 2015)) -> genderGapIran8th

student.stat4th03 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat4th03

student.stat4th03 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat4th03

student.stat8th03 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat8th03

student.stat8th03 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat8th03

student.stat4th07 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat4th07

student.stat4th07 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat4th07

student.stat8th07 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat8th07

student.stat8th07 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat8th07

student.stat4th11 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat4th11

student.stat4th11 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat4th11

student.stat8th11 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat8th03

student.stat8th11 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat8th11

student.stat4th15 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat4th15

student.stat4th15 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat4th15

student.stat8th15 %>% 
  mutate(mathBenchmark = ifelse(mathScore >= 650 ,"Advanced",
                                ifelse(mathScore >= 500 , "High",
                                       ifelse(mathScore >= 370 , "Intermediate","Low")))) -> student.stat8th15

student.stat8th15 %>% 
  mutate(scienceBenchmark = ifelse(scienceScore >= 650 ,"Advanced",
                                   ifelse(scienceScore >= 500 , "High",
                                          ifelse(scienceScore >= 370 , "Intermediate","Low")))) -> student.stat8th15


student.stat4th15 %>% 
  filter(idcntry == 364 && itsex == 2) -> iran4thBoys

student.stat4th15 %>% 
  filter(idcntry == 364 && itsex == 1) -> iran4thGirls

student.stat8th15 %>% 
  filter(idcntry == 364 && itsex == 2) -> iran8thBoys

student.stat8th15 %>% 
  filter(idcntry == 364 && itsex == 1) -> iran8thGirls

data.frame(Gender = c("Girls","Boys"),N = c(1863,1960),MathAverage = c(mean(iran4thGirls$mathScore),mean(iran4thBoys$mathScore)),
           MathSD = c(sd(iran4thGirls$mathScore),sd(iran4thBoys$mathScore)),ScienceAverage = c(mean(iran4thGirls$scienceScore),mean(iran4thBoys$scienceScore)),
           ScienceSD = c(sd(iran4thGirls$scienceScore),sd(iran4thBoys$scienceScore))) -> iran4thGenderStat

data.frame(Gender = c("Girls","Boys"),N = c(3000,3130),MathAverage = c(mean(iran8thGirls$mathScore),mean(iran8thBoys$mathScore)),
           MathSD = c(sd(iran8thGirls$mathScore),sd(iran8thBoys$mathScore)),ScienceAverage = c(mean(iran8thGirls$scienceScore),mean(iran8thBoys$scienceScore)),
           ScienceSD = c(sd(iran8thGirls$scienceScore),sd(iran8thBoys$scienceScore))) -> iran8thGenderStat

iran4thGenderStat %>% apply(2, function(x) x %>% as.numeric() %>% round(1)) %>% data.frame() ->iran4thGenderStat
iran4thGenderStat$Gender <- c("Girls","Boys")

iran8thGenderStat %>% apply(2, function(x) x %>% as.numeric() %>% round(1)) %>% data.frame() ->iran8thGenderStat
iran8thGenderStat$Gender <- c("Girls","Boys")


## @knitr gg4thmath
genderGapIran4th %>% 
  hchart(type = "line",hcaes(x = Year,y = mathScore,group = itsex)) %>% 
  hc_xAxis(categories = genderGapIran4th$Year) %>% 
  hc_title(text = "Iran 4th Graders : Boys vs. Girls In Math") %>% 
  hc_yAxis(title = list(text = "Math Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

## @knitr gg4thscience
genderGapIran4th %>% 
  hchart(type = "line",hcaes(x = Year,y = scienceScore,group = itsex)) %>% 
  hc_xAxis(categories = genderGapIran4th$Year) %>% 
  hc_title(text = "Iran 4th Graders : Boys vs. Girls In Science") %>% 
  hc_yAxis(title = list(text = "Science Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr gg8thmath
genderGapIran8th %>% 
  hchart(type = "line",hcaes(x = Year,y = mathScore,group = itsex)) %>% 
  hc_xAxis(categories = genderGapIran8th$Year) %>% 
  hc_title(text = "Iran 8th Graders : Boys vs. Girls In Math") %>% 
  hc_yAxis(title = list(text = "Math Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr gg8thscience
genderGapIran8th %>% 
  hchart(type = "line",hcaes(x = Year,y = scienceScore,group = itsex)) %>% 
  hc_xAxis(categories = genderGapIran8th$Year) %>% 
  hc_title(text = "Iran 8th Graders : Boys vs. Girls In Science") %>% 
  hc_yAxis(title = list(text = "Math Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

## @knitr benchmarks4thmath
student.stat4th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex,mathBenchmark) %>% 
  summarise(count = n()) -> countriesBenchmarks4th15Math

student.stat4th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex,scienceBenchmark) %>% 
  summarise(count = n()) -> countriesBenchmarks4th15Science

student.stat4th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex) %>% 
  summarize(all = n()) -> countries4th15

left_join(countriesBenchmarks4th15Math,countries4th15) -> countriesBenchmarks4th15Math
left_join(countriesBenchmarks4th15Science,countries4th15) -> countriesBenchmarks4th15Science

countriesBenchmarks4th15Math %>% 
  mutate(percentage = round((count / all) * 100,1)) -> countriesBenchmarks4th15Math

countriesBenchmarks4th15Science %>% 
  mutate(percentage = round((count / all) * 100,1)) -> countriesBenchmarks4th15Science

countriesBenchmarks4th15Math %>% 
  ungroup() %>% 
  filter(idcntry == 364 ) %>% 
  mutate(Sex = ifelse(itsex==1,"Girls","Boys")) %>% 
  select(Sex,mathBenchmark,percentage) %>% 
  ungroup() -> IranBenchmark4thGenderGapMath 

IranBenchmark4thGenderGapMath %>% 
  group_by(name = Sex) %>% 
  do(categories = .$mathBenchmark) %>% 
  list_parse() -> categories_groupedMath


highchart() %>% 
  hc_xAxis(categories = categories_groupedMath) %>% 
  hc_add_series(data = IranBenchmark4thGenderGapMath, type = "column", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#40DA3A") %>% 
  hc_title(text = "Iran 4th Graders: Boys And Girls Benchmarks in Math") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


## @knitr benchmarks4thscience
countriesBenchmarks4th15Science %>% 
  ungroup() %>% 
  filter(idcntry == 364 ) %>% 
  mutate(Sex = ifelse(itsex==1,"Girls","Boys")) %>% 
  select(Sex,scienceBenchmark,percentage) %>% 
  ungroup() -> IranBenchmark4thGenderGapScience 

IranBenchmark4thGenderGapScience %>% 
  group_by(name = Sex) %>% 
  do(categories = .$scienceBenchmark) %>% 
  list_parse() -> categories_grouped


highchart() %>% 
  hc_xAxis(categories = categories_grouped) %>% 
  hc_add_series(data = IranBenchmark4thGenderGapScience, type = "bar", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#40DAFA") %>% 
  hc_title(text = "Iran 4th Graders: Boys And Girls Benchmarks in Science") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr benchmarks8thmath
student.stat8th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex,mathBenchmark) %>% 
  summarise(count = n()) -> countriesBenchmarks8th15Math

student.stat8th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex,scienceBenchmark) %>% 
  summarise(count = n()) -> countriesBenchmarks8th15Science

student.stat8th15 %>% 
  drop_na() %>% 
  group_by(idcntry,itsex) %>% 
  summarize(all = n()) -> countries8th15

left_join(countriesBenchmarks8th15Math,countries8th15) -> countriesBenchmarks8th15Math
left_join(countriesBenchmarks8th15Science,countries8th15) -> countriesBenchmarks8th15Science

countriesBenchmarks8th15Math %>% 
  mutate(percentage = round((count / all) * 100,1)) -> countriesBenchmarks8th15Math

countriesBenchmarks8th15Science %>% 
  mutate(percentage = round((count / all) * 100,1)) -> countriesBenchmarks8th15Science

countriesBenchmarks8th15Math %>% 
  ungroup() %>% 
  filter(idcntry == 364 ) %>% 
  mutate(Sex = ifelse(itsex==1,"Girls","Boys")) %>% 
  select(Sex,mathBenchmark,percentage) %>% 
  ungroup() -> IranBenchmark8thGenderGapMath 

IranBenchmark8thGenderGapMath %>% 
  group_by(name = Sex) %>% 
  do(categories = .$mathBenchmark) %>% 
  list_parse() -> categories_groupedMath8th


highchart() %>% 
  hc_xAxis(categories = categories_groupedMath8th) %>% 
  hc_add_series(data = IranBenchmark8thGenderGapMath, type = "bar", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#40DA3A") %>% 
  hc_title(text = "Iran 8th Graders: Boys And Girls Benchmarks in Math") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr benchmarks8thscience
countriesBenchmarks8th15Science %>% 
  ungroup() %>% 
  filter(idcntry == 364 ) %>% 
  mutate(Sex = ifelse(itsex==1,"Girls","Boys")) %>% 
  select(Sex,scienceBenchmark,percentage) %>% 
  ungroup() -> IranBenchmark8thGenderGapScience 

IranBenchmark8thGenderGapScience %>% 
  group_by(name = Sex) %>% 
  do(categories = .$scienceBenchmark) %>% 
  list_parse() -> categories_grouped8th


highchart() %>% 
  hc_xAxis(categories = categories_grouped8th) %>% 
  hc_add_series(data = IranBenchmark8thGenderGapScience, type = "column", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#40DAFA") %>% 
  hc_title(text = "Iran 8th Graders: Boys And Girls Benchmarks in Science") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


## @knitr dataSection
library(jsonlite)

library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(ggthemr)

### PIRLS 

## WORDWIDE
## 2016

acgPirls16 <- read_rds("../Pirls2016/ACG_Pirls16.rds")
asgPirls16 <- read_rds("../Pirls2016/ASG_Pirls16.rds")
ashPirls16 <- read_rds("../Pirls2016/ASH_Pirls16.rds")
atgPirls16 <- read_rds("../Pirls2016/ATG_Pirls16.rds")

student.statPirls16 <- read_rds("../Pirls2016/ASA_Pirls16.rds") %>% 
  group_by(IDCNTRY,IDSTUD,IDSCHOOL) %>% 
  mutate(score = mean(ASRREA01:ASRREA05)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,score)

student.statPirls16 %>% 
  group_by(IDCNTRY,IDSCHOOL) %>% 
  summarise(score = mean(score)) -> schoolPerformancePirls16 

read_rds("../Pirls2016/AST_Pirls16.rds") %>% 
  mutate(overall = (ASRREA01 + ASRREA02 + ASRREA03 + ASRREA04 + ASRREA05)/5) %>%
  select(IDCNTRY, IDSTUD, IDSCHOOL,IDTEACH,overall) %>%
  group_by(IDCNTRY, IDSCHOOL,IDTEACH) %>% 
  summarise(score = mean(overall))-> teacherPerformancePirls16


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


## @knitr top2
countriesBenchmarksPirls16 %>% 
  filter(Country %in% c("Russian Federation","Singapore")) -> dfTopPirls16 

dfTopPirls16 %>% 
  group_by(name = Country) %>% 
  do(categories = .$benchmark) %>% 
  list_parse() -> categories_grouped


highchart() %>% 
  hc_xAxis(categories = categories_grouped) %>% 
  hc_add_series(data = dfTopPirls16, type = "bar", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#00FA9A") %>% 
  hc_title(text = "Top 2 Countries Benchmarks") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

## @knitr top5
countriesBenchmarksPirls16 %>% 
  filter(Country %in% c("Hong Kong","Ireland","Poland")) -> dfTop3Pirls16 

dfTop3Pirls16 %>% 
  group_by(name = Country) %>% 
  do(categories = .$benchmark) %>% 
  list_parse() -> categories_grouped2


highchart() %>% 
  hc_xAxis(categories = categories_grouped2) %>% 
  hc_add_series(data = dfTop3Pirls16, type = "bar", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T),color= "#87CEFA") %>% 
  hc_title(text = "Countries Benchmarks, Rank 3 to 5") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

## @knitr iran
countriesBenchmarksPirls16 %>% 
  filter(Country %in% c("Iran, Islamic Republic of")) -> dfIranPirls16 

dfIranPirls16 %>% 
  group_by(name = Country) %>% 
  do(categories = .$benchmark) %>% 
  list_parse() -> categories_grouped3


highchart() %>% 
  hc_xAxis(categories = categories_grouped3) %>% 
  hc_add_series(data = dfIranPirls16, type = "bar", hcaes(y = percentage), name = "Percentage",
                showInLegend = F,dataLabels = list(enabled = T)) %>% 
  hc_title(text = "Iran Benchmarks") %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

## @knitr earlyStart
ashPirls16 %>% 
  mutate(literacy = ASBH02A + ASBH02B+ ASBH02C+ ASBH02D+ ASBH02E+ ASBH02F+ ASBH02G+ ASBH02H+ ASBH02I) %>%
  mutate(literacy = ifelse(literacy <= 13  ,"Often",
                           ifelse(literacy <= 22,"Sometimes","Never Or Almost Never"))) %>% 
  filter(!is.na(literacy)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,literacy) -> studentEarlyLiteracyPirls16


left_join(studentEarlyLiteracyPirls16, student.statPirls16) -> studentEarlyLiteracyPirls16
ashPirls16 %>% 
  mutate(preprimary = ASBH05B) %>%
  mutate(preprimary = ifelse(preprimary == 1  ,"Did Not Attend",
                             ifelse(preprimary == 2,"1 Year Or Less",
                                    ifelse(preprimary == 3,"2 Years","3 Years Or More")))) %>% 
  filter(!is.na(preprimary)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,preprimary) -> studentPreprimaryPirls


left_join(studentPreprimaryPirls, student.statPirls16) -> studentPreprimaryPirls
##############   
left_join(student.BenchmarkStatPirls16,studentEarlyLiteracyPirls16) -> earlyLiteracyPirls
earlyLiteracyPirls %>% 
  filter(!is.na(literacy)) -> earlyLiteracyPirls

left_join(earlyLiteracyPirls,studentPreprimaryPirls) -> earlyLiteracyPirls
earlyLiteracyPirls %>% 
  filter(!is.na(preprimary)) -> earlyLiteracyPirls

earlyLiteracyPirls %>% 
  mutate(earlyLiteracy = ifelse(preprimary == "Did Not Attend" | preprimary == "1 Year Or Less" ,ifelse(literacy == "Never Or Almost Never","No Early Activities","Literacy Activities Only"),
                                ifelse(literacy == "Never Or Almost Never", "Preprimary Only","Preprimary And Early Literacy Activities"))) ->earlyLiteracyPirls

earlyLiteracyPirls$score <- round(earlyLiteracyPirls$score,0)

earlyLiteracyPirls %>%
  filter(benchmark == "Advanced" | benchmark == "High") %>% 
  nrow() -> rows

earlyLiteracyPirls %>% 
  group_by(benchmark,earlyLiteracy) %>% 
  filter(benchmark == "Advanced" | benchmark == "High") %>% 
  mutate(early = ifelse(earlyLiteracy == "Preprimary And Early Literacy Activities","Yes","No")) %>%
  group_by(early) %>% 
  summarise(percentage = n()/rows *100) ->earlyLiteracyPirls


highchart() %>% 
  hc_plotOptions(
    pie=list(
      allowPointSelect = T , cursor = "pointer" ,innerSize = "50%",
      dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -30),startAngle = -90,
      endAngle = 90
    )
  ) %>% 
  hc_add_series(earlyLiteracyPirls, "pie", hcaes(name = early, y = percentage), name = "Percentage",size = 520,center = list(380,350)) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_tooltip(valueDecimals = 2, valueSuffix = " %") %>% 
  hc_title(text="Good Readers Had<br> an Early Start <br>in Literacy Learning",align = "center",y=0,verticalAlign = "middle")

## @knitr wellResourced
########### Good Readers Attended Well Resourced, Academically Oriented Schools
acgPirls16 %>% 
  mutate(resources = ACBG12AA + ACBG12AB +ACBG12AC + ACBG12AD + ACBG12AE +ACBG12AF +ACBG12AG + ACBG12AH +
           ACBG12AI + ACBG12BA + ACBG12BB + ACBG12BC + ACBG12BD) %>%
  mutate(resources = ifelse(resources < 19 ,"Not Affected",
                            ifelse(resources < 33 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSCHOOL,resources) -> resourceShortagePirls


left_join(resourceShortagePirls, schoolPerformancePirls16) -> resourceShortagePirls

acgPirls16 %>% 
  mutate(economicState = ifelse(ACBG03B >=3,
                                ifelse(ACBG03A <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(ACBG03A >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(IDCNTRY,IDSCHOOL,economicState) -> EconomicBackgroundPirls


left_join(EconomicBackgroundPirls, schoolPerformancePirls16) -> EconomicBackgroundPirls

left_join(student.BenchmarkStatPirls16,EconomicBackgroundPirls) -> orientedSchoolsPirls
orientedSchoolsPirls %>% 
  filter(!is.na(economicState)) -> orientedSchoolsPirls

left_join(orientedSchoolsPirls,resourceShortagePirls) -> orientedSchoolsPirls
orientedSchoolsPirls %>% 
  filter(!is.na(resources)) -> orientedSchoolsPirls

orientedSchoolsPirls %>% 
  mutate(resource = ifelse(resources == "Not Affected",ifelse(economicState == "More Affluent","Well Resourced School",ifelse(economicState == "More Disadvantaged","Resourced School","Well Resourced School")),
                           ifelse(economicState == "More Affluent", "Resourced School",ifelse(resources == "Affected A Lot" & economicState == "More Disadvantaged","Badly Resourced School","Well Resourced School")))) -> orientedSchoolsPirls

orientedSchoolsPirls$score <- round(orientedSchoolsPirls$score,0)

orientedSchoolsPirls %>%
  filter(score > 500) %>% 
  nrow() -> rows

orientedSchoolsPirls %>% 
  filter(score > 500) %>% 
  group_by(resource) %>% 
  summarise(percentage = n()/rows *100) -> orientedSchoolsPirlsPercentage



round(orientedSchoolsPirlsPercentage$percentage,0) ->orientedSchoolsPirlsPercentage$percentage

highchart() %>% 
  hc_add_series(orientedSchoolsPirlsPercentage, "column", hcaes(x = resource, y = percentage),name = "Percentage") %>%
  hc_add_series(orientedSchoolsPirlsPercentage, "pie", hcaes(name = resource, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    series = list(
      pointFormat = "{point.y}%"
    ),
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE
    ),
    pie = list(
      colorByPoint = TRUE, center = c('35%', '30%'),
      size = 230,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -10)
    )) %>%
  hc_yAxis(
    title = list(text = "Percent"),
    labels = list(format = "{value}%"), max = 100
  ) %>% 
  hc_xAxis(categories = orientedSchoolsPirlsPercentage$resource) %>%
  hc_title(text = "Good Readers Attended Well Resourced, Academically Oriented Schools" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr safe
#############  Good Readers Attended Safe Schools
asgPirls16 %>% 
  mutate(bullying = ASBG13A + ASBG13B + ASBG13C + ASBG13D + ASBG13E + ASBG13F + ASBG13G + ASBG13H ) %>%
  mutate(bullying = ifelse(bullying > 27 ,"Almost Never",
                           ifelse(bullying > 19 ,"About Monthly","About Weekly"))) %>% 
  filter(!is.na(bullying)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,bullying) -> studentBullyingPirls


left_join(studentBullyingPirls, student.statPirls16) -> studentBullyingPirls

left_join(student.BenchmarkStatPirls16,studentBullyingPirls) -> safeSchoolsPirls
safeSchoolsPirls %>% 
  filter(!is.na(bullying)) -> safeSchoolsPirls

atgPirls16 %>% 
  mutate(safe = ATBG08A + ATBG08F + ATBG08B + ATBG08C + ATBG08D + ATBG08E + ATBG08G + ATBG08H ) %>%
  mutate(safe = ifelse(safe < 13 ,"Very Safe",
                       ifelse(safe < 22 ,"Somewhat Safe","Less Than Safe"))) %>% 
  filter(!is.na(safe)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,safe) -> schoolSafePirls1
left_join(safeSchoolsPirls,schoolSafePirls1) -> safeSchoolsPirls
safeSchoolsPirls %>% 
  filter(!is.na(safe)) -> safeSchoolsPirls

acgPirls16 %>% 
  mutate(discipline = ACBG14A + ACBG14B + ACBG14C+ ACBG14D+ ACBG14E+ ACBG14F+ ACBG14G+ ACBG14H+ ACBG14I + ACBG14J) %>%
  mutate(discipline = ifelse(discipline < 15 ,"Hardly Any Problems",
                             ifelse(discipline < 26 ,"Minor Problems","Moderate to Severe Problems"))) %>% 
  filter(!is.na(discipline)) %>% 
  select(IDCNTRY,IDSCHOOL,discipline) -> schoolDisciplinePirls1

left_join(safeSchoolsPirls,schoolDisciplinePirls1) -> safeSchoolsPirls
safeSchoolsPirls %>% 
  filter(!is.na(discipline)) -> safeSchoolsPirls

safeSchoolsPirls$score <- round(safeSchoolsPirls$score,0)
safeSchoolsPirls %>% select(-IDTEACH) -> safeSchoolsPirls

safeSchoolsPirls %>% 
  filter(score > 530) -> safeSchoolsPirls

safeSchoolsPirls %>% 
  group_by(discipline) %>% 
  summarise(DisciplinePercentage = n()/nrow(safeSchoolsPirls) *100) -> safeSchoolsPirlsDiscipline
round(safeSchoolsPirlsDiscipline$DisciplinePercentage,1) -> safeSchoolsPirlsDiscipline$DisciplinePercentage

safeSchoolsPirls %>% 
  group_by(bullying) %>% 
  summarise(BullyingPercentage = n()/nrow(safeSchoolsPirls) *100) -> safeSchoolsPirlsBullying
round(safeSchoolsPirlsBullying$BullyingPercentage,1) -> safeSchoolsPirlsBullying$BullyingPercentage

safeSchoolsPirls %>% 
  group_by(safe) %>% 
  summarise(SafePercentage = n()/nrow(safeSchoolsPirls) *100) -> safeSchoolsPirlsSafe
round(safeSchoolsPirlsSafe$SafePercentage,1) -> safeSchoolsPirlsSafe$SafePercentage

highchart() %>% 
  hc_plotOptions(
    pie=list(
      allowPointSelect = T , cursor = "pointer" ,innerSize = "50%",
      dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -30),startAngle = -90,
      endAngle = 90
    )
  ) %>% 
  hc_add_series(safeSchoolsPirlsDiscipline, "pie", hcaes(name = discipline, y = DisciplinePercentage), name = "Percentage",size = 320,center = list(200,130)) %>%
  hc_add_series(safeSchoolsPirlsBullying, "pie", hcaes(name = bullying, y = BullyingPercentage), name = "Percentage",size = 320,center = list(570,300)) %>%
  hc_add_series(safeSchoolsPirlsSafe, "pie", hcaes(name = safe, y = SafePercentage), name = "Percentage",size = 320,center = list(200,520)) %>% 
  hc_title(text = "Good Readers Attended Safe Schools<br>" ) %>%
  hc_subtitle(text = '<p>Discipline Problems at School<br>.<br>.<br>.<br>.<br>School Bullying<br>.<br>.<br>.<br>.<br>Safe And Orderly School</p>',align = "left",floating = T,x= 100,y = 230) %>% 
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_gridlight())


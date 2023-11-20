library(intsvy)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)

bcg15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bcg.rds")
bsa15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bsa.rds")
bsg15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bsg.rds")
bsr15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bsr.rds")
bst15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bst.rds")
btm15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/btm.rds")
bts15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bts.rds")

acg15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/acg.rds")
asa15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/asa.rds")
asg15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/asg.rds")
ash15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/ash.rds")
asr15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/asr.rds")
ast15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/ast.rds")
atg15 <- read_rds("C:/Users/Matin/Desktop/R Project/2015/atg.rds")

acgPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/ACG_Pirls16.rds")
asaPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/ASA_Pirls16.rds")
asgPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/ASG_Pirls16.rds")
ashPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/ASH_Pirls16.rds")
astPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/AST_Pirls16.rds")
atgPirls16 <- read_rds("C:/Users/Matin/Desktop/R Project/Pirls2016/ATG_Pirls16.rds")


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
  
student.stat8th <- read_rds("C:/Users/Matin/Desktop/R Project/2015/bsa.rds") %>%
  group_by(idcntry, idstud, idschool) %>%
  mutate(mat = mean(bsmmat01:bsmmat05), 
         sci = mean(bsssci01:bsssci05)) %>%
  select(idcntry, idstud, idschool, mat, sci) %>%
  mutate(overall = (mat + sci) / 2)

student.stat4th <- read_rds("C:/Users/Matin/Desktop/R Project/2015/asa.rds") %>%
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

############# School Composition and Resources

#### Economic background of students
## 4th grade
acg15 %>% 
  mutate(economicState = ifelse(acbg03b >=3,
                                ifelse(acbg03a <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(acbg03a >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(idcntry,idschool,economicState) -> EconomicBackground4th


left_join(EconomicBackground4th, schoolPerformance4th, by = c("idcntry","idschool")) -> EconomicBackground4th

EconomicBackground.fit <- aov(score ~ as.factor(economicState), data = EconomicBackground4th)
summary(EconomicBackground.fit)

EconomicBackground4th %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackground4th.avg

EconomicBackground4th.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),name = 'Average Score') %>% 
  hc_title(text = "Economic Background Of Students") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

EconomicBackground4th$economicState <- factor(EconomicBackground4th$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))
hcboxplot(x = EconomicBackground4th$score,var = EconomicBackground4th$economicState) %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of the Economic Background") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_add_theme(hc_theme_flat())

#### Economic background of students
##8th grade
bcg15 %>% 
  mutate(economicState = ifelse(bcbg03b >=3,
                                ifelse(bcbg03a <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(bcbg03a >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(idcntry,idschool,economicState) -> EconomicBackground8th


left_join(EconomicBackground8th, schoolPerformance8th, by = c("idcntry","idschool")) -> EconomicBackground8th

EconomicBackground8th.fit <- aov(score ~ as.factor(economicState), data = EconomicBackground8th)
summary(EconomicBackground8th.fit)

EconomicBackground8th %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackground8th.avg

EconomicBackground8th.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),name = "Average Score") %>% 
  hc_title(text = "Economic Background Of Students") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

EconomicBackground8th$economicState <- factor(EconomicBackground8th$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))
hcboxplot(x = EconomicBackground8th$score,var = EconomicBackground8th$economicState,outliers = F,name = "Score") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of the Economic Background") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_add_theme(hc_theme_google())


######## Schools Where Students Enter the Primary Grades with Literacy and Numeracy Skills
##4th rade
acg15 %>% 
  mutate(skillscore = acbg18a+acbg18b +acbg18c + acbg18d + acbg18e +acbg18f +acbg18g + acbg18h + acbg18i + acbg18j + acbg18k) %>%
  mutate(skillscore = ifelse(skillscore > 38 ,"More than 75% Enter with Skills",
                             ifelse(skillscore > 17,"25-75% Enter with Skills", "Less than 25% Enter with Skills"))) %>% 
  filter(!is.na(skillscore)) %>% 
  select(idcntry,idschool,skillscore) -> studentBackgroundSkill4th


left_join(studentBackgroundSkill4th, schoolPerformance4th, by = c("idcntry","idschool")) -> studentBackgroundSkill4th

studentBackgroundSkill4th.fit <- aov(score ~ as.factor(skillscore), data = studentBackgroundSkill4th)
summary(studentBackgroundSkill4th.fit)

studentBackgroundSkill4th %>% 
  group_by(skillscore) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentBackgroundSkill4th.avg

studentBackgroundSkill4th.avg %>% 
  hchart(type = "column", hcaes(x = skillscore, y = avgScore),name = "Average Score") %>% 
  hc_title(text = "Entering the Primary Grades with Literacy and Numeracy Skills") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


studentBackgroundSkill4th$skillscore <- factor(studentBackgroundSkill4th$skillscore,levels = c("More than 75% Enter with Skills","25-75% Enter with Skills","Less than 25% Enter with Skills"))
hcboxplot(x = studentBackgroundSkill4th$score,var = studentBackgroundSkill4th$skillscore,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_add_theme(hc_theme_elementary()) 


####### Instruction Affected by Resource Shortages

### 4th grade

acg15 %>% 
  mutate(resources = acbg14aa+acbg14ab +acbg14ac + acbg14ad + acbg14ae +acbg14af +acbg14ag + acbg14ah + 
           acbg14ai + acbg14ba + acbg14bb + acbg14bc + acbg14bd + acbg14be + acbg14ca + acbg14cb + acbg14cc+ acbg14cd) %>%
  mutate(resources = ifelse(resources < 25 ,"Not Affected",
                             ifelse(resources < 60 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(idcntry,idschool,resources) -> resourceShortage4th


left_join(resourceShortage4th, schoolPerformance4th, by = c("idcntry","idschool")) -> resourceShortage4th

resourceShortage4th.fit <- aov(score ~ as.factor(resources), data = resourceShortage4th)
summary(resourceShortage4th.fit)

resourceShortage4th %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortage4th.avg

resourceShortage4th.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore)) %>% 
  hc_title(text = "Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_db())

resourceShortage4th$resources <- factor(resourceShortage4th$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortage4th$score,var = resourceShortage4th$resources,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_db()) 

####### Instruction Affected by Resource Shortages

### 8th grade

bcg15 %>% 
  mutate(resources = bcbg13aa+bcbg13ab +bcbg13ac + bcbg13ad + bcbg13ae +bcbg13af +bcbg13ag + bcbg13ah + 
           bcbg13ai + bcbg13ba + bcbg13bb + bcbg13bc + bcbg13bd + bcbg13be + bcbg13ca + bcbg13cb +bcbg13cc+ bcbg13cd) %>%
  mutate(resources = ifelse(resources < 25 ,"Not Affected",
                            ifelse(resources < 60 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(idcntry,idschool,resources) -> resourceShortage8th


left_join(resourceShortage8th, schoolPerformance8th, by = c("idcntry","idschool")) -> resourceShortage8th

resourceShortage8th.fit <- aov(score ~ as.factor(resources), data = resourceShortage8th)
summary(resourceShortage8th.fit)

resourceShortage8th %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortage8th.avg

resourceShortage8th.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore))%>% 
  hc_title(text = "Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_sandsignika())

resourceShortage8th$resources <- factor(resourceShortage8th$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortage8th$score,var = resourceShortage8th$resources,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_sandsignika()) 


################## School Climate

########### Problems with School Conditions and Resources
### 4th grade
atg15 %>% 
  mutate(condition = atbg08a + atbg08b + atbg08c + atbg08d + atbg08e + atbg08f + atbg08g) %>%
  mutate(condition = ifelse(condition < 8 ,"Hardly Any Problems",
                            ifelse(condition < 17 ,"Minor Problems", "Moderate to Severe Problemm"))) %>% 
  filter(!is.na(condition)) %>% 
  select(idcntry,idschool,idteach,condition) -> schoolcondition4th


left_join(schoolcondition4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> schoolcondition4th

schoolcondition4th.fit <- aov(score ~ as.factor(condition), data = schoolcondition4th)
summary(schoolcondition4th.fit)

schoolcondition4th %>% 
  group_by(condition) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolcondition4th.avg

schoolcondition4th.avg %>% 
  hchart(type = "column", hcaes(x = condition, y = avgScore),color = "#2980b9")%>% 
  hc_title(text = "Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolcondition4th$condition <- factor(schoolcondition4th$condition,levels = c("Hardly Any Problems","Minor Problems", "Moderate to Severe Problemm"))
hcboxplot(x = schoolcondition4th$score,var = schoolcondition4th$condition,outliers = T,name = "Score",color = "#2980b9") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

########### Problems with School Conditions and Resources

##8th grade

btm15 %>% 
  mutate(condition = btbg08a + btbg08b + btbg08c + btbg08d + btbg08e + btbg08f + btbg08g) %>%
  mutate(condition = ifelse(condition < 8 ,"Hardly Any Problems",
                            ifelse(condition < 17 ,"Minor Problems", "Moderate to Severe Problemm"))) %>% 
  filter(!is.na(condition)) %>% 
  select(idcntry,idschool,idteach,condition) -> schoolcondition8th


left_join(schoolcondition8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> schoolcondition8th

schoolcondition8th.fit <- aov(score ~ as.factor(condition), data = schoolcondition8th)
summary(schoolcondition8th.fit)

schoolcondition8th %>% 
  group_by(condition) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolcondition8th.avg

schoolcondition8th.avg %>% 
  hchart(type = "column", hcaes(x = condition, y = avgScore),color = "#59f0b9")%>% 
  hc_title(text = "Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolcondition8th$condition <- factor(schoolcondition8th$condition,levels = c("Hardly Any Problems","Minor Problems", "Moderate to Severe Problemm"))
hcboxplot(x = schoolcondition8th$score,var = schoolcondition8th$condition,outliers = T,name = "Score",color = "#59f0b9") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 


####### School Emphasis on Academic Success
## 4th grade
acg15 %>% 
  mutate(emphasis = acbg15a + acbg15b + acbg15c+ acbg15d+ acbg15e+ acbg15f+ acbg15g+ acbg15h+ acbg15i+
           acbg15j + acbg15k + acbg15l + acbg15m) %>%
  mutate(emphasis = ifelse(emphasis < 15 ,"Very High Emphasis",
                            ifelse(emphasis < 28 ,"High Emphasis",
                                   ifelse(emphasis < 41 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(idcntry,idschool,emphasis) -> schoolEmphasis4th


left_join(schoolEmphasis4th, schoolPerformance4th, by = c("idcntry","idschool")) -> schoolEmphasis4th

schoolEmphasis4th.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasis4th)
summary(schoolEmphasis4th.fit)

schoolEmphasis4th %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasis4th.avg

schoolEmphasis4th.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore),color = "#393029")%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolEmphasis4th$emphasis <- factor(schoolEmphasis4th$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasis4th$score,var = schoolEmphasis4th$emphasis,outliers = T,name = "Score",color = "#393029") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

####### School Emphasis on Academic Success

## 8th grade

bcg15 %>% 
  mutate(emphasis = bcbg14a + bcbg14b + bcbg14c+ bcbg14d+ bcbg14e+ bcbg14f+ bcbg14g+ bcbg14h+ bcbg14i+
           bcbg14j + bcbg14k +bcbg14l + bcbg14m) %>%
  mutate(emphasis = ifelse(emphasis < 15 ,"Very High Emphasis",
                           ifelse(emphasis < 28 ,"High Emphasis",
                                  ifelse(emphasis < 41 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(idcntry,idschool,emphasis) -> schoolEmphasis8th


left_join(schoolEmphasis8th, schoolPerformance8th, by = c("idcntry","idschool")) -> schoolEmphasis8th

schoolEmphasis8th.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasis8th)
summary(schoolEmphasis8th.fit)

schoolEmphasis8th %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasis8th.avg

schoolEmphasis8th.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore))%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

schoolEmphasis8th$emphasis <- factor(schoolEmphasis8th$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasis8th$score,var = schoolEmphasis8th$emphasis,outliers = F,name = "Score") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())


######  Students' Sense of School Belonging
## 4th grade
asg15 %>% 
  mutate(belonging = asbg11a + asbg11b + asbg11c + asbg11d + asbg11e + asbg11f + asbg11g) %>%
  mutate(belonging = ifelse(belonging < 15 ,"High Sense of Belonging",
                            ifelse(belonging < 25 ,"Sense of Belonging", "Little Sense of Belonging"))) %>% 
  filter(!is.na(belonging)) %>% 
  select(idcntry,idschool,belonging) -> schoolBelonging4th


left_join(schoolBelonging4th, schoolPerformance4th, by = c("idcntry","idschool")) -> schoolBelonging4th

schoolBelonging4th.fit <- aov(score ~ as.factor(belonging), data = schoolBelonging4th)
summary(schoolBelonging4th.fit)

schoolBelonging4th %>% 
  group_by(belonging) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolBelonging4th.avg

schoolBelonging4th.avg %>% 
  hchart(type = "column", hcaes(x = belonging, y = avgScore))%>% 
  hc_title(text = "Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_economist())


schoolBelonging4th$belonging <- factor(schoolBelonging4th$belonging,levels = c("High Sense of Belonging","Sense of Belonging", "Little Sense of Belonging"))
hcboxplot(x = schoolBelonging4th$score,var = schoolBelonging4th$belonging,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_economist()) 

######  Students' Sense of School Belonging

## 8th grade
bsg15 %>% 
  mutate(belonging = bsbg15a + bsbg15b + bsbg15c + bsbg15d + bsbg15e + bsbg15f + bsbg15g) %>%
  mutate(belonging = ifelse(belonging < 15 ,"High Sense of Belonging",
                            ifelse(belonging < 25 ,"Sense of Belonging", "Little Sense of Belonging"))) %>% 
  filter(!is.na(belonging)) %>% 
  select(idcntry,idschool,belonging) -> schoolBelonging8th


left_join(schoolBelonging8th, schoolPerformance8th, by = c("idcntry","idschool")) -> schoolBelonging8th

schoolBelonging8th.fit <- aov(score ~ as.factor(belonging), data = schoolBelonging8th)
summary(schoolBelonging8th.fit)

schoolBelonging8th %>% 
  group_by(belonging) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolBelonging8th.avg

schoolBelonging8th.avg %>% 
  hchart(type = "column", hcaes(x = belonging, y = avgScore))%>% 
  hc_title(text = "Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

schoolBelonging8th$belonging <- factor(schoolBelonging8th$belonging,levels = c("High Sense of Belonging","Sense of Belonging", "Little Sense of Belonging"))
hcboxplot(x = schoolBelonging8th$score,var = schoolBelonging8th$belonging,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx()) 


########## TEACHERS' AND PRINCIPALS' PREPARATION

##### Principal Experience 
## 4th grade
acg15 %>% 
  mutate(experience = acbg19) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                           ifelse(experience <= 15  ,"5-15 Years",
                                  ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,experience) -> principalExperience4th


left_join(principalExperience4th, schoolPerformance4th, by = c("idcntry","idschool")) -> principalExperience4th

principalExperience4th.fit <- aov(score ~ as.factor(experience), data = principalExperience4th)
summary(principalExperience4th.fit)

principalExperience4th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalExperience4th.avg

principalExperience4th.avg %>% 
  hchart(type = "column", hcaes(x = experience, y = avgScore),color = "#FF9500")%>% 
  hc_title(text = "Principal Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

principalExperience4th$experience <- factor(principalExperience4th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = principalExperience4th$score,var = principalExperience4th$experience,outliers = F,name = "Score",color = "#FF9500") %>%
  hc_title(text = "BoxPlot of Principal Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())  

##### Principal Experience 

## 8th grade
bcg15 %>% 
  mutate(experience = bcbg19) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,experience) -> principalExperience8th


left_join(principalExperience8th, schoolPerformance8th, by = c("idcntry","idschool")) -> principalExperience8th

principalExperience8th.fit <- aov(score ~ as.factor(experience), data = principalExperience8th)
summary(principalExperience8th.fit)

principalExperience8th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalExperience8th.avg

principalExperience8th.avg %>% 
  hchart(type = "column", hcaes(x = experience, y = avgScore),color = "#20B2AA")%>% 
  hc_title(text = "Principal Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

principalExperience8th$experience <- factor(principalExperience8th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = principalExperience8th$score,var = principalExperience8th$experience,outliers = F,name = "Score",color = "#20B2AA") %>%
  hc_title(text = "BoxPlot of Principal Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   

#### Principal Education
## 4th grade
acg15 %>% 
  mutate(education = acbg21) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                             ifelse(education == 2 ,"Bachelor's or Equivalent",
                                    ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,education) -> principalEducation4th


left_join(principalEducation4th, schoolPerformance4th, by = c("idcntry","idschool")) -> principalEducation4th

principalEducation4th.fit <- aov(score ~ as.factor(education), data = principalEducation4th)
summary(principalEducation4th.fit)

principalEducation4th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalEducation4th.avg

principalEducation4th.avg %>% 
  hchart(type = "column", hcaes(x = education, y = avgScore),color = "#FFE4B5")%>% 
  hc_title(text = "Principal Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

principalEducation4th$education <- factor(principalEducation4th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = principalEducation4th$score,var = principalEducation4th$education,outliers = F,name = "Score",color = "#FFE4B5") %>%
  hc_title(text = "BoxPlot of Principal Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   

#### Principal Education

## 8th grade
bcg15 %>% 
  mutate(education = bcbg21) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,education) -> principalEducation8th


left_join(principalEducation8th, schoolPerformance8th, by = c("idcntry","idschool")) -> principalEducation8th

principalEducation8th.fit <- aov(score ~ as.factor(education), data = principalEducation8th)
summary(principalEducation8th.fit)

principalEducation8th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalEducation8th.avg

principalEducation8th.avg %>% 
  hchart(type = "column", hcaes(x = education, y = avgScore),color = "#FF6347")%>% 
  hc_title(text = "Principal Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

principalEducation8th$education <- factor(principalEducation8th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = principalEducation8th$score,var = principalEducation8th$education,outliers = F,name = "Score",color = "#FF6347") %>%
  hc_title(text = "BoxPlot of Principal Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   


##### Teachers Experience
## 4th grade
atg15 %>% 
  mutate(experience = atbg01) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,idteach,experience) -> teacherExperience4th



left_join(teacherExperience4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherExperience4th

teacherExperience4th.fit <- aov(score ~ as.factor(experience), data = teacherExperience4th)
summary(teacherExperience4th.fit)

teacherExperience4th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherExperience4th.avg

teacherExperience4th.avg %>% 
  hchart(type = "bar", hcaes(x = experience, y = avgScore),color = "#E0FFFF")%>% 
  hc_title(text = "Teachers Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())


teacherExperience4th$experience <- factor(teacherExperience4th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = teacherExperience4th$score,var = teacherExperience4th$experience,outliers = F,name = "Score",color = "#008B8B") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   


##### Teachers Experience

## 8th grade
btm15 %>% 
  mutate(experience = btbg01) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,idteach,experience) -> teacherExperience8th



left_join(teacherExperience8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherExperience8th

teacherExperience8th.fit <- aov(score ~ as.factor(experience), data = teacherExperience8th)
summary(teacherExperience8th.fit)

teacherExperience8th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherExperience8th.avg

teacherExperience8th.avg %>% 
  hchart(type = "bar", hcaes(x = experience, y = avgScore),color = "#800000")%>% 
  hc_title(text = "Teachers Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

teacherExperience8th$experience <- factor(teacherExperience8th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = teacherExperience8th$score,var = teacherExperience8th$experience,outliers = F,name = "Score",color = "#800000") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   


#### Teacher Education
## 4th grade
atg15 %>% 
  mutate(education = atbg04) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,idteach,education) -> teacherEducation4th


left_join(teacherEducation4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherEducation4th

teacherEducation4th.fit <- aov(score ~ as.factor(education), data = teacherEducation4th)
summary(teacherEducation4th.fit)

teacherEducation4th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherEducation4th.avg

teacherEducation4th.avg %>% 
  hchart(type = "bar", hcaes(x = education, y = avgScore),color = "#FF7F50")%>% 
  hc_title(text = "Teachers Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

teacherEducation4th$education <- factor(teacherEducation4th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = teacherEducation4th$score,var = teacherEducation4th$education,outliers = F,name = "Score",color = "#FF7F50") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   

#### Teacher Education
## 8th grade
btm15 %>% 
  mutate(education = btbg04) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                             ifelse(education == 2 ,"Bachelor's or Equivalent",
                                    ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,idteach,education) -> teacherEducation8th


left_join(teacherEducation8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherEducation8th

teacherEducation8th.fit <- aov(score ~ as.factor(education), data = teacherEducation8th)
summary(teacherEducation8th.fit)

teacherEducation8th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherEducation8th.avg

teacherEducation8th.avg %>% 
  hchart(type = "bar", hcaes(x = education, y = avgScore),color = "#5F9EA0")%>% 
  hc_title(text = "Teachers Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())


teacherEducation8th$education <- factor(teacherEducation8th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = teacherEducation8th$score,var = teacherEducation8th$education,outliers = F,name = "Score",color = "#5F9EA0") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   




############ Classroom Instruction

#### Amount of Homework
## 8 th grade

bsg15 %>%
  mutate(homework = bsbm25aa + bsbs25ab)  %>% 
  mutate(homework = ifelse(homework <= 2,"Everyday",
                           ifelse(homework <= 4,"3 or 4 Times a Week",
                                  ifelse(homework <= 6, "1 or 2 Times a Week",
                                         ifelse(homework <= 8,"Less Than Once a Week","Never"))))) %>% 
  filter(!is.na(homework)) %>% 
  select(idcntry,idschool,homework) -> homework8th
  
left_join(homework8th, schoolPerformance8th, by = c("idcntry","idschool")) -> homework8th

homework8th.fit <- aov(score ~ as.factor(homework), data = homework8th)
summary(homework8th.fit)

homework8th %>% 
  group_by(homework) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> homework8th.avg

homework8th.avg %>% 
  hchart(type = "column", hcaes(x = homework, y = avgScore))%>% 
  hc_title(text = "Amount of Homework") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

homework8th$homework <- factor(homework8th$homework,levels = c("Never","Less Than Once 
                                                               a Week","1 or 2 Times a Week",
                                                                                   "3 or 4 Times a Week","Everyday"))
hcboxplot(x = homework8th$score,var = homework8th$homework,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Amount of Homework") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx()) 

####  Teaching Limited by Student Needs
## 4th grade
atg15 %>% 
  mutate(limit = atbg15a + atbg15b+ atbg15c+ atbg15d+ atbg15e+ atbg15f+ atbg15g) %>%
  mutate(limit = ifelse(limit <= 10 ,"Not At All",
                             ifelse(limit <= 17 ,"Some","A Lot"))) %>% 
  filter(!is.na(limit)) %>% 
  select(idcntry,idschool,idteach,limit) -> teacherLimit4th


left_join(teacherLimit4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherLimit4th

teacherLimit4th.fit <- aov(score ~ as.factor(limit), data = teacherLimit4th)
summary(teacherLimit4th.fit)

teacherLimit4th %>% 
  group_by(limit) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherLimit4th.avg

teacherLimit4th.avg %>% 
  hchart(type = "column", hcaes(x = limit, y = avgScore))%>% 
  hc_title(text = "Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

teacherLimit4th$limit <- factor(teacherLimit4th$limit,levels = c("Not At All","Some","A Lot"))
hcboxplot(x = teacherLimit4th$score,var = teacherLimit4th$limit,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat()) 

####  Teaching Limited by Student Needs

## 8th grade
btm15 %>% 
  mutate(limit = btbg15a + btbg15b+ btbg15c+ btbg15d+ btbg15e+ btbg15f+ btbg15g) %>%
  mutate(limit = ifelse(limit <= 10 ,"Not At All",
                        ifelse(limit <= 17 ,"Some","A Lot"))) %>% 
  filter(!is.na(limit)) %>% 
  select(idcntry,idschool,idteach,limit) -> teacherLimit8th


left_join(teacherLimit8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherLimit8th

teacherLimit8th.fit <- aov(score ~ as.factor(limit), data = teacherLimit8th)
summary(teacherLimit8th.fit)

teacherLimit8th %>% 
  group_by(limit) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore)) -> teacherLimit8th.avg

teacherLimit8th.avg %>% 
  hchart(type = "column", hcaes(x = limit, y = avgScore))%>% 
  hc_title(text = "Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


teacherLimit8th$limit <- factor(teacherLimit8th$limit,levels = c("Not At All","Some","A Lot"))
hcboxplot(x = teacherLimit8th$score,var = teacherLimit8th$limit,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary()) 



####  Students Absences
## 4th grade
asg15 %>% 
  mutate(absent = asbg08) %>%
  mutate(absent = ifelse(absent == 1 ,"Once A Week Or More",
                        ifelse(absent == 2 ,"Once Every Two Weeks",
                               ifelse(absent == 3,"Once A Month","Never Or Almost Never")))) %>% 
  filter(!is.na(absent)) %>% 
  select(idcntry,idschool,absent) -> studentAbsences4th


left_join(studentAbsences4th, schoolPerformance4th, by = c("idcntry","idschool")) -> studentAbsences4th

studentAbsences4th.fit <- aov(score ~ as.factor(absent), data = studentAbsences4th)
summary(studentAbsences4th.fit)

studentAbsences4th %>% 
  group_by(absent) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentAbsences4th.avg

studentAbsences4th.avg %>% 
  hchart(type = "column", hcaes(x = absent, y = avgScore))%>% 
  hc_title(text = "Students Absences") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())


studentAbsences4th$absent <- factor(studentAbsences4th$absent,levels = c("Once A Week Or More","Once Every Two Weeks","Once A Month","Never Or Almost Never"))
hcboxplot(x = studentAbsences4th$score,var = studentAbsences4th$absent,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students Absences") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica()) 

####  Students Absences

## 8th grade
bsg15 %>% 
  mutate(absent = bsbg11) %>%
  mutate(absent = ifelse(absent == 1 ,"Once A Week Or More",
                         ifelse(absent == 2 ,"Once Every Two Weeks",
                                ifelse(absent == 3,"Once A Month","Never Or Almost Never")))) %>% 
  filter(!is.na(absent)) %>% 
  select(idcntry,idschool,absent) -> studentAbsences8th


left_join(studentAbsences8th, schoolPerformance8th, by = c("idcntry","idschool")) -> studentAbsences8th

studentAbsences8th.fit <- aov(score ~ as.factor(absent), data = studentAbsences8th)
summary(studentAbsences8th.fit)

studentAbsences8th %>% 
  group_by(absent) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentAbsences8th.avg

studentAbsences8th.avg %>% 
  hchart(type = "bar", hcaes(x = absent, y = avgScore),color= "#B22222")%>% 
  hc_title(text = "Students Absences") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())

studentAbsences8th$absent <- factor(studentAbsences8th$absent,levels = c("Once A Week Or More","Once Every Two Weeks","Once A Month","Never Or Almost Never"))
hcboxplot(x = studentAbsences8th$score,var = studentAbsences8th$absent,outliers = F,name = "Score",color= "#B22222") %>%
  hc_title(text = "BoxPlot of Students Absences") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())  


######## Instructional Time Spent
## 4th grade
acg15 %>% 
  mutate(hours = acbg08a * acbg08b/60 ) %>%
  mutate(hours = ifelse(hours <= 1000 ,"450 - 1000 Instructional Hours",
                         ifelse(hours <= 1500 ,"1000 - 1500 Instructional Hours",
                                ifelse(hours <= 2500,"1500-2500 Instructional Hours","2500 - 3000 Instructional Hours")))) %>% 
  filter(!is.na(hours)) %>% 
  select(idcntry,idschool,hours) -> instructionHours4th


left_join(instructionHours4th, schoolPerformance4th, by = c("idcntry","idschool")) -> instructionHours4th

instructionHours4th.fit <- aov(score ~ as.factor(hours), data = instructionHours4th)
summary(instructionHours4th.fit)

instructionHours4th %>% 
  group_by(hours) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> instructionHours4th.avg

instructionHours4th.avg %>% 
  hchart(type = "bar", hcaes(x = hours, y = avgScore),color= "#20B2AA")%>% 
  hc_title(text = "Instructional Time Spent") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())


instructionHours4th$hours <- factor(instructionHours4th$hours,levels = c("450 - 1000 Instructional Hours","1000 - 1500 Instructional Hours","1500-2500 Instructional Hours","2500 - 3000 Instructional Hours"))
hcboxplot(x = instructionHours4th$score,var = instructionHours4th$hours,outliers = F,name = "Score",color= "#20B2AA") %>%
  hc_title(text = "BoxPlot of Instructional Time Spent") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())  




 
########################################## PIRLS    ################################################

###################### HOME ENViRONMENT SUPPORT

############ Home Resources

asgPirls16 %>% 
  mutate(resources = ASBG04 + 2-ASBG05A + 2-ASBG05B + 2-ASBG05C + 2-ASBG05D) %>%
  mutate(resources = ifelse(resources >= 6 ,"Many Resources",
                         ifelse(resources >=  4,"Some Resources","Few Resources"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,resources) -> studentHomePirls16


left_join(studentHomePirls16, student.statPirls16) -> studentHomePirls16
#########
studentHomePirls16 %>% 
  group_by(resources) %>% 
  summarise(percentage = round(n()/nrow(studentHomePirls16) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentHomePirls16Summary

highchart() %>% 
  hc_add_series(studentHomePirls16Summary, "column", hcaes(x = resources, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentHomePirls16Summary, "pie", hcaes(name = resources, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 230,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -10)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentHomePirls16Summary$resources) %>%
  hc_title(text = "Home Resources for Learning" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_538())

########
studentHomePirls16.fit <- aov(score ~ as.factor(resources), data = studentHomePirls16)
summary(studentHomePirls16.fit)

studentHomePirls16 %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentHomePirls16.avg

studentHomePirls16.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore),color= "#8FBC8F")%>% 
  hc_title(text = "Home Resources") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

studentHomePirls16$resources <- factor(studentHomePirls16$resources,levels = c("Many Resources","Some Resources","Few Resources"))
hcboxplot(x = studentHomePirls16$score,var = studentHomePirls16$resources,outliers = F,name = "Score",color= "#8FBC8F") %>%
  hc_title(text = "BoxPlot of Home Resources") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())   

########### Early Literacy Activities Before Beginning Primary School

ashPirls16 %>% 
  mutate(literacy = ASBH02A + ASBH02B+ ASBH02C+ ASBH02D+ ASBH02E+ ASBH02F+ ASBH02G+ ASBH02H+ ASBH02I) %>%
  mutate(literacy = ifelse(literacy <= 13  ,"Often",
                            ifelse(literacy <= 22,"Sometimes","Never Or Almost Never"))) %>% 
  filter(!is.na(literacy)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,literacy) -> studentEarlyLiteracyPirls16


left_join(studentEarlyLiteracyPirls16, student.statPirls16) -> studentEarlyLiteracyPirls16

studentEarlyLiteracyPirls16.fit <- aov(score ~ as.factor(literacy), data = studentEarlyLiteracyPirls16)
summary(studentEarlyLiteracyPirls16.fit)

studentEarlyLiteracyPirls16 %>% 
  group_by(literacy) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentEarlyLiteracyPirls16.avg

studentEarlyLiteracyPirls16.avg %>% 
  hchart(type = "column", hcaes(x = literacy, y = avgScore),color= "#8FBC8F")%>% 
  hc_title(text = "Early Literacy Activities") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

studentEarlyLiteracyPirls16$literacy <- factor(studentEarlyLiteracyPirls16$literacy,levels = c("Often","Sometimes","Never Or Almost Never"))

hcboxplot(x = studentEarlyLiteracyPirls16$score,var = studentEarlyLiteracyPirls16$literacy,outliers = F,name = "Score",color= "red") %>%
  hc_chart(type="column") %>% 
  hc_title(text = "BoxPlot of Early Literacy Activities") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())    


############## Students Attended Preprimary Education
ashPirls16 %>% 
  mutate(preprimary = ASBH05B) %>%
  mutate(preprimary = ifelse(preprimary == 1  ,"Did Not Attend",
                           ifelse(preprimary == 2,"1 Year Or Less",
                                  ifelse(preprimary == 3,"2 Years","3 Years Or More")))) %>% 
  filter(!is.na(preprimary)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,preprimary) -> studentPreprimaryPirls


left_join(studentPreprimaryPirls, student.statPirls16) -> studentPreprimaryPirls
#########
studentPreprimaryPirls %>% 
  group_by(preprimary) %>% 
  summarise(percentage = round(n()/nrow(studentPreprimaryPirls) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentPreprimaryPirlsSummary

highchart() %>% 
  hc_add_series(studentPreprimaryPirlsSummary, "column", hcaes(x = preprimary, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentPreprimaryPirlsSummary, "pie", hcaes(name = preprimary, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 230,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -15)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentPreprimaryPirlsSummary$preprimary) %>%
  hc_title(text = "An Early Start in School" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_flat())

########


studentPreprimaryPirls.fit <- aov(score ~ as.factor(preprimary), data = studentPreprimaryPirls)
summary(studentPreprimaryPirls.fit)

studentPreprimaryPirls %>% 
  group_by(preprimary) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentPreprimaryPirls.avg

studentPreprimaryPirls.avg %>% 
  hchart(type = "bar", hcaes(x = preprimary, y = avgScore),color= "#F08080")%>% 
  hc_title(text = "Students Attended Preprimary Education") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

studentPreprimaryPirls$preprimary <- factor(studentPreprimaryPirls$preprimary,levels = c("Did Not Attend","1 Year Or Less","2 Years","3 Years Or More"))
hcboxplot(x = studentPreprimaryPirls$score,var = studentPreprimaryPirls$preprimary,outliers = F,name = "Score",color= "#F08080") %>%
  hc_title(text = "BoxPlot of Students Attended Preprimary Education") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())     

############ Could Do Literacy Tasks When Beginning Primary School
ashPirls16 %>% 
  mutate(literacy = ASBH07A +ASBH07B + ASBH07C + ASBH07D + ASBH07E + ASBH07F) %>%
  mutate(literacy = ifelse(literacy <= 7  ,"Very Well",
                             ifelse(literacy <= 13,"Moderately Well",
                                    ifelse(literacy <= 19,"Not Well","Not at all")))) %>% 
  filter(!is.na(literacy)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,literacy) -> studentLiteracyPirls16


left_join(studentLiteracyPirls16, student.statPirls16) -> studentLiteracyPirls16

studentLiteracyPirls16.fit <- aov(score ~ as.factor(literacy), data = studentLiteracyPirls16)
summary(studentLiteracyPirls16.fit)

studentLiteracyPirls16 %>% 
  group_by(literacy) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentLiteracyPirls16.avg

studentLiteracyPirls16.avg %>% 
  hchart(type = "column", hcaes(x = literacy, y = avgScore),color= "#FdA500")%>% 
  hc_title(text = "Literacy Tasks When Beginning Primary School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 


studentLiteracyPirls16$literacy <- factor(studentLiteracyPirls16$literacy,levels = c("Very Well","Moderately Well","Not Well","Not at all"))
hcboxplot(x = studentLiteracyPirls16$score,var = studentLiteracyPirls16$literacy,outliers = F,name = "Score",color= "#FFA500") %>%
  hc_title(text = "BoxPlot of Literacy Tasks When Beginning Primary School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())      


########################## SCHOOL COMPOSiTiON AND RESOURCES

###### School Composition by Socioeconomic Background of the Student Body

acgPirls16 %>% 
  mutate(economicState = ifelse(ACBG03B >=3,
                                ifelse(ACBG03A <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(ACBG03A >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(IDCNTRY,IDSCHOOL,economicState) -> EconomicBackgroundPirls


left_join(EconomicBackgroundPirls, schoolPerformancePirls16) -> EconomicBackgroundPirls

#########
EconomicBackgroundPirls %>% 
  group_by(economicState) %>% 
  summarise(percentage = round(n()/nrow(EconomicBackgroundPirls) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> EconomicBackgroundPirlsSummary

highchart() %>% 
  hc_add_series(EconomicBackgroundPirlsSummary, "column", hcaes(x = economicState, y = AvgScore),name = "Average Score") %>%
  hc_add_series(EconomicBackgroundPirlsSummary, "pie", hcaes(name = economicState, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -15)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = EconomicBackgroundPirlsSummary$economicState) %>%
  hc_title(text = "An Early Start in School" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_flat())

########
EconomicBackgroundPirls.fit <- aov(score ~ as.factor(economicState), data = EconomicBackgroundPirls)
summary(EconomicBackgroundPirls.fit)

EconomicBackgroundPirls %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackgroundPirls.avg

EconomicBackgroundPirls.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),color= "#FdA500")%>% 
  hc_title(text = "School Composition by Socioeconomic Background of the Student Body") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      


EconomicBackgroundPirls$economicState <- factor(EconomicBackgroundPirls$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))

hcboxplot(x = EconomicBackgroundPirls$score,var = EconomicBackgroundPirls$economicState,outliers = F,name = "Score",color= "#FFA500") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "School Composition by Socioeconomic Background of the Student Body") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

############  : Instruction Affected by Reading Resource Shortages

acgPirls16 %>% 
  mutate(resources = ACBG12AA + ACBG12AB +ACBG12AC + ACBG12AD + ACBG12AE +ACBG12AF +ACBG12AG + ACBG12AH +
           ACBG12AI + ACBG12BA + ACBG12BB + ACBG12BC + ACBG12BD) %>%
  mutate(resources = ifelse(resources < 19 ,"Not Affected",
                            ifelse(resources < 33 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSCHOOL,resources) -> resourceShortagePirls


left_join(resourceShortagePirls, schoolPerformancePirls16) -> resourceShortagePirls

resourceShortagePirls.fit <- aov(score ~ as.factor(resources), data = resourceShortagePirls)
summary(resourceShortagePirls.fit)

resourceShortagePirls %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortagePirls.avg

resourceShortagePirls.avg %>% 
  hchart(type = "bar", hcaes(x = resources, y = avgScore),color= "#87CEFA")%>% 
  hc_title(text = "Instruction Affected by Reading Resource Shortages") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      


resourceShortagePirls$resources <- factor(resourceShortagePirls$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortagePirls$score,var = resourceShortagePirls$resources,outliers = F,name = "Score",color= "#87CEFA") %>%
  hc_title(text = "Instruction Affected by Reading Resource Shortages") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

########### Size of School Library

acgPirls16 %>% 
  mutate(books = ACBG09A) %>%
  mutate(books = ifelse(ACBG09 == 2 ,"No School Library",
                            ifelse(books >= 5 ,"More than 5,000 Book Title", 
                                   ifelse(books >= 3,"501-5,000 Book Titles","500 Book Titles or Fewer")))) %>% 
  filter(!is.na(books) & !is.na(ACBG09)) %>% 
  select(IDCNTRY,IDSCHOOL,books) -> schoolLibraryPirls


left_join(schoolLibraryPirls, schoolPerformancePirls16) -> schoolLibraryPirls

schoolLibraryPirls.fit <- aov(score ~ as.factor(books), data = schoolLibraryPirls)
summary(schoolLibraryPirls.fit)

schoolLibraryPirls %>% 
  group_by(books) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolLibraryPirls.avg

schoolLibraryPirls.avg %>% 
  hchart(type = "column", hcaes(x = books, y = avgScore),color= "#8A2BE2")%>% 
  hc_title(text = "Size of School Library") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolLibraryPirls$books <- factor(schoolLibraryPirls$books,levels = c("More than 5,000 Book Title","501-5,000 Book Titles","500 Book Titles or Fewer","No School Library"))
hcboxplot(x = schoolLibraryPirls$score,var = schoolLibraryPirls$books,outliers = F,name = "Score",color= "#8A2BE2") %>%
  hc_title(text = "Size of School Library") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

######################## SCHOOL CLiMATE

######## Parents' Perceptions of Their Child's School

ashPirls16 %>% 
  mutate(perception = ASBH09A +ASBH09B + ASBH09C + ASBH09D + ASBH09E + ASBH09F) %>%
  mutate(perception = ifelse(perception <= 9 ,"Very Satisfied",
                           ifelse(perception <= 15,"Somewhat Satisfied","Less Than Satisfied"))) %>% 
  filter(!is.na(perception)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,perception) -> parentsPerceptionPirls


left_join(parentsPerceptionPirls, student.statPirls16) -> parentsPerceptionPirls

parentsPerceptionPirls.fit <- aov(score ~ as.factor(perception), data = parentsPerceptionPirls)
summary(parentsPerceptionPirls.fit)

parentsPerceptionPirls %>% 
  group_by(perception) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> parentsPerceptionPirls.avg

parentsPerceptionPirls.avg %>% 
  hchart(type = "column", hcaes(x = perception, y = avgScore),color= "#B0C4DE")%>% 
  hc_title(text = "Parents' Perceptions of Their Child's School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

parentsPerceptionPirls$perception <- factor(parentsPerceptionPirls$perception,levels = c("Very Satisfied","Somewhat Satisfied","Less Than Satisfied"))
hcboxplot(x = parentsPerceptionPirls$score,var = parentsPerceptionPirls$perception,outliers = F,name = "Score",color= "#B0C4DE") %>%
  hc_title(text = "Parents' Perceptions of Their Child's School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  


#######  School Emphasis on Academic Success 
acgPirls16 %>% 
  mutate(emphasis = ACBG13A + ACBG13B + ACBG13C+ ACBG13D+ ACBG13E+ ACBG13F+ ACBG13G+ ACBG13H+ ACBG13I) %>%
  mutate(emphasis = ifelse(emphasis < 18 ,"Very High Emphasis",
                           ifelse(emphasis < 30 ,"High Emphasis",
                                  ifelse(emphasis < 40 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(IDCNTRY,IDSCHOOL,emphasis) -> schoolEmphasisPirls


left_join(schoolEmphasisPirls, schoolPerformancePirls16) -> schoolEmphasisPirls


#########
schoolEmphasisPirls %>% 
  group_by(emphasis) %>% 
  summarise(percentage = round(n()/nrow(schoolEmphasisPirls) * 100,3), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> schoolEmphasisPirlsSummary

highchart() %>% 
  hc_add_series(schoolEmphasisPirlsSummary, "column", hcaes(x = emphasis, y = AvgScore),name = "Average Score") %>%
  hc_add_series(schoolEmphasisPirlsSummary, "pie", hcaes(name = emphasis, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.3f} %",distance = 20)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = schoolEmphasisPirlsSummary$emphasis) %>%
  hc_title(text = "School Emphasis on Academic Success" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_elementary())

########
schoolEmphasisPirls.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasisPirls)
summary(schoolEmphasisPirls.fit)

schoolEmphasisPirls %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasisPirls.avg

schoolEmphasisPirls.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore),color= "#4169E1")%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolEmphasisPirls$emphasis <- factor(schoolEmphasisPirls$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasisPirls$score,var = schoolEmphasisPirls$emphasis,outliers = F,name = "Score",color= "#4169E1") %>%
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  
  

############################### SCHOOL DiSCiPLiNE AND SAFETY

######### School Discipline - Principals' Reports
acgPirls16 %>% 
  mutate(discipline = ACBG14A + ACBG14B + ACBG14C+ ACBG14D+ ACBG14E+ ACBG14F+ ACBG14G+ ACBG14H+ ACBG14I + ACBG14J) %>%
  mutate(discipline = ifelse(discipline < 15 ,"Hardly Any Problems",
                           ifelse(discipline < 26 ,"Minor Problems","Moderate to Severe Problems"))) %>% 
  filter(!is.na(discipline)) %>% 
  select(IDCNTRY,IDSCHOOL,discipline) -> schoolDisciplinePirls


left_join(schoolDisciplinePirls, schoolPerformancePirls16) -> schoolDisciplinePirls

schoolDisciplinePirls.fit <- aov(score ~ as.factor(discipline), data = schoolDisciplinePirls)
summary(schoolDisciplinePirls.fit)

schoolDisciplinePirls %>% 
  group_by(discipline) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolDisciplinePirls.avg

schoolDisciplinePirls.avg %>% 
  hchart(type = "bar", hcaes(x = discipline, y = avgScore),color= "#F0F8FF")%>% 
  hc_title(text = "School Discipline - Principals' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolDisciplinePirls$discipline <- factor(schoolDisciplinePirls$discipline,levels = c("Hardly Any Problems","Minor Problems","Moderate to Severe Problems"))
hcboxplot(x = schoolDisciplinePirls$score,var = schoolDisciplinePirls$discipline,outliers = F,name = "Score",color= "#483D8B") %>%
  hc_title(text = "School Discipline - Principals' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

############## Safe and Orderly School - Teachers' Reports

atgPirls16 %>% 
  mutate(safe = ATBG08A + ATBG08F + ATBG08B + ATBG08C + ATBG08D + ATBG08E + ATBG08G + ATBG08H ) %>%
  mutate(safe = ifelse(safe < 13 ,"Very Safe",
                             ifelse(safe < 22 ,"Somewhat Safe","Less Than Safe"))) %>% 
  filter(!is.na(safe)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,safe) -> schoolSafePirls


left_join(schoolSafePirls, teacherPerformancePirls16) -> schoolSafePirls

schoolSafePirls.fit <- aov(score ~ as.factor(safe), data = schoolSafePirls)
summary(schoolSafePirls.fit)

schoolSafePirls %>% 
  group_by(safe) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolSafePirls.avg

schoolSafePirls.avg %>% 
  hchart(type = "bar", hcaes(x = safe, y = avgScore),color= "#1E90FF")%>% 
  hc_title(text = "Safe and Orderly School - Teachers' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  


schoolSafePirls$safe <- factor(schoolSafePirls$safe,levels = c("Very Safe","Somewhat Safe","Less Than Safe"))
hcboxplot(x = schoolSafePirls$score,var = schoolSafePirls$safe,outliers = F,name = "Score",color= "#1E90FF") %>%
  hc_title(text = "Safe and Orderly School - Teachers' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      

##########  Student Bullying
asgPirls16 %>% 
  mutate(bullying = ASBG13A + ASBG13B + ASBG13C + ASBG13D + ASBG13E + ASBG13F + ASBG13G + ASBG13H ) %>%
  mutate(bullying = ifelse(bullying > 27 ,"Almost Never",
                       ifelse(bullying > 19 ,"About Monthly","About Weekly"))) %>% 
  filter(!is.na(bullying)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,bullying) -> studentBullyingPirls


left_join(studentBullyingPirls, student.statPirls16) -> studentBullyingPirls

#########
studentBullyingPirls %>% 
  group_by(bullying) %>% 
  summarise(percentage = round(n()/nrow(studentBullyingPirls) * 100,0), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentBullyingPirlsSummary

highchart() %>% 
  hc_add_series(studentBullyingPirlsSummary, "column", hcaes(x = bullying, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentBullyingPirlsSummary, "pie", hcaes(name = bullying, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = 10)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentBullyingPirlsSummary$bullying) %>%
  hc_title(text = "Student Bullying" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_smpl())

########
studentBullyingPirls.fit <- aov(score ~ as.factor(bullying), data = studentBullyingPirls)
summary(studentBullyingPirls.fit)

studentBullyingPirls %>% 
  group_by(bullying) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentBullyingPirls.avg

studentBullyingPirls.avg %>% 
  hchart(type = "column", hcaes(x = bullying, y = avgScore),color= "#00BFFF")%>% 
  hc_title(text = "Student Bullying") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  


studentBullyingPirls$bullying <- factor(studentBullyingPirls$bullying,levels = c("Almost Never","About Monthly","About Weekly"))
hcboxplot(x = studentBullyingPirls$score,var = studentBullyingPirls$bullying,outliers = F,name = "Score",color= "#00BFFF") %>%
  hc_title(text = "Student Bullying") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      


################### CLASSROOM iNSTRUCTiON

######  Organizing Students for Reading Instruction :
## teach reading as Whole-class 

atgPirls16 %>% 
  mutate(wholeClass = ATBR08A ) %>%
  mutate(wholeClass = ifelse(wholeClass == 1 ,"Always Or Almost Always",
                           ifelse(wholeClass == 2 ,"Often",ifelse(wholeClass == 3 ,"Sometimes","Never")))) %>% 
  filter(!is.na(wholeClass)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,wholeClass) -> teachWholeClassPirls


left_join(teachWholeClassPirls, teacherPerformancePirls16) -> teachWholeClassPirls

teachWholeClassPirls.fit <- aov(score ~ as.factor(wholeClass), data = teachWholeClassPirls)
summary(teachWholeClassPirls.fit)

teachWholeClassPirls %>% 
  group_by(wholeClass) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teachWholeClassPirls.avg

teachWholeClassPirls.avg %>% 
  hchart(type = "column", hcaes(x = wholeClass, y = avgScore),color= "#FFFACD")%>% 
  hc_title(text = "Teach Reading as Whole-Class ") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

teachWholeClassPirls$wholeClass <- factor(teachWholeClassPirls$wholeClass,levels = c("Always Or Almost Always","Often","Sometimes","Never"))
hcboxplot(x = teachWholeClassPirls$score,var = teachWholeClassPirls$wholeClass,outliers = F,name = "Score",color= "#CCCC00") %>%
  hc_title(text = "Teach Reading as Whole-Class ") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      

## mixed-ability groups
atgPirls16 %>% 
  mutate(mixedGroups = ATBR08A ) %>%
  mutate(mixedGroups = ifelse(mixedGroups == 1 ,"Always Or Almost Always",
                             ifelse(mixedGroups == 2 ,"Often",ifelse(mixedGroups == 3 ,"Sometimes","Never")))) %>% 
  filter(!is.na(mixedGroups)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,mixedGroups) -> teachMixedGroupsPirls


left_join(teachMixedGroupsPirls, teacherPerformancePirls16) -> teachMixedGroupsPirls

teachMixedGroupsPirls.fit <- aov(score ~ as.factor(mixedGroups), data = teachMixedGroupsPirls)
summary(teachMixedGroupsPirls.fit)

teachMixedGroupsPirls %>% 
  group_by(mixedGroups) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teachMixedGroupsPirls.avg

teachMixedGroupsPirls.avg %>% 
  hchart(type = "column", hcaes(x = mixedGroups, y = avgScore),color= "#708090")%>% 
  hc_title(text = "Teach Reading as Mixed-Ability Groups") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

teachMixedGroupsPirls$mixedGroups <- factor(teachMixedGroupsPirls$mixedGroups,levels = c("Always Or Almost Always","Often","Sometimes","Never"))
hcboxplot(x = teachMixedGroupsPirls$score,var = teachMixedGroupsPirls$mixedGroups,outliers = F,name = "Score",color= "#DCDCDC") %>%
  hc_title(text = "Teach Reading as Mixed-Ability Groups") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      


###########################   STUDENT ENGAGEMENT AND ATTiTUDES

#########  Students Engaged in Reading Lessons

asgPirls16 %>% 
  mutate(engagement = ASBR01A + ASBR01B + ASBR01C + ASBR01D + ASBR01E + ASBR01F + ASBR01G + ASBR01H + ASBR01I ) %>%
  mutate(engagement = ifelse(engagement <= 13 ,"Very Engaged",
                              ifelse(engagement <= 23 ,"Somewhat Engaged","Less Than Engaged"))) %>% 
  filter(!is.na(engagement)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,engagement) -> studentEngagementPirls


left_join(studentEngagementPirls, student.statPirls16) -> studentEngagementPirls

studentEngagementPirls.fit <- aov(score ~ as.factor(engagement), data = studentEngagementPirls)
summary(studentEngagementPirls.fit)

studentEngagementPirls %>% 
  group_by(engagement) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentEngagementPirls.avg

studentEngagementPirls.avg %>% 
  hchart(type = "column", hcaes(x = engagement, y = avgScore),color= "#FFC0CB")%>% 
  hc_title(text = "Students Engaged in Reading Lessons") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

studentEngagementPirls$engagement <- factor(studentEngagementPirls$engagement,levels = c("Very Engaged","Somewhat Engaged","Less Than Engaged"))
hcboxplot(x = studentEngagementPirls$score,var = studentEngagementPirls$engagement,outliers = F,name = "Score",color= "#FFC0CB") %>%
  hc_title(text = "Students Engaged in Reading Lessons") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      


######## Students Confident in Reading

asgPirls16 %>% 
  mutate(confident = ASBR07A + ASBR07B + ASBR07C + ASBR07D + ASBR07E + ASBR07F ) %>%
  mutate(confident = ifelse(confident <= 10 ,"Not Confident",
                             ifelse(confident <= 16 ,"Somewhat Confident","Very Confident"))) %>% 
  filter(!is.na(confident)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,confident) -> studentConfidentPirls


left_join(studentConfidentPirls, student.statPirls16) -> studentConfidentPirls

studentConfidentPirls.fit <- aov(score ~ as.factor(confident), data = studentConfidentPirls)
summary(studentConfidentPirls.fit)

studentConfidentPirls %>% 
  group_by(confident) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentConfidentPirls.avg

studentConfidentPirls.avg %>% 
  hchart(type = "column", hcaes(x = confident, y = avgScore),color= "#7FFFD4")%>% 
  hc_title(text = "Students Confident in Reading") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

studentConfidentPirls$confident <- factor(studentConfidentPirls$confident,levels = c("Very Confident","Somewhat Confident","Not Confident"))
hcboxplot(x = studentConfidentPirls$score,var = studentConfidentPirls$confident,outliers = F,name = "Score",color= "#008B8B") %>%
  hc_title(text = "Students Confident in Reading") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      



#################################################################################################

download_map_data("custom/world") -> world

############################################################################

########### WHat makes a good reader

################## Top countries


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

 
  
##################
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

####
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

########
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
             positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: 350 + 0 }; }"),
             style = list(color = "black", fontSize = "1em", fontWeight = "normal"),shape = "square",backgroundColor = "transparent",
             borderColor = "transparent",
             shadow = T) %>% 
  hc_legend(title = list(text = "Average Score")) %>% 
  hc_title(text = "Pirls 2015 Worldwide")


#########################   Good Readers Had an Early Start in Literacy Learning

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


########### Good Readers Attended Well Resourced, Academically Oriented Schools

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

#############  Good Readers Attended Safe Schools

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


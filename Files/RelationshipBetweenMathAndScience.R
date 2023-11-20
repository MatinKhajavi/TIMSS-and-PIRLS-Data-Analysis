## @knitr dataSection
library(intsvy)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(Hmisc)
library(reshape2)

bsgTimss15 <- read_rds("../2015/bsg.rds")
asgTimss15 <- read_rds("../2015/asg.rds")

bsgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th15

asgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th15

bsgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(Algebra = mean(bsmalg01:bsmalg05),`Data And Chance` = mean(bsmdat01:bsmdat05), 
         Number= mean(bsmnum01:bsmnum05), Geometry = mean(bsmgeo01:bsmgeo05), Chemistry = mean(bssche01:bssche05),
         `Earth Science` = mean(bssear01:bssear05), Biology = mean(bssbio01:bssbio05), Physics = mean(bssphy01:bssphy05)) %>% 
  select(idcntry,idschool,idstud,itsex,Algebra,`Data And Chance`,
         Number,Geometry,Chemistry,`Earth Science`,Biology,Physics) -> Timss8thCompleteStat

asgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(`Data Display` = mean(asmdat01:asmdat05), Number= mean(asmnum01:asmnum05),
         Geometry = mean(asmgeo01:asmgeo05), `Earth Science` = mean(assear01:assear05),
         Physics = mean(assphy01:assphy05), `Life Science` = mean(asslif01:asslif05)) %>% 
  select(idcntry,idschool,idstud,itsex,`Data Display`,
         Number,Geometry,`Earth Science`,`Life Science`,Physics) -> Timss4thCompleteStat



## @knitr chart1
ggplot(data = student.stat4th15,aes(mathScore, scienceScore)) +
  geom_point() +
  xlab("Math Score") + ylab("Science Score") +
  labs(title = "Math vs. Science Scores 4th Grade",subtitle = "Based on the data") + 
  scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=1,
                                 color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")))

## @knitr chart2
student.stat4th15 %>% filter(idcntry==364) %>%
  hchart(type= "scatter" , hcaes(x = mathScore , y = scienceScore)) %>% 
  hc_xAxis(title = list(text = "Math Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_title(text = "Math vs. Science Scores In Iran 4th Grade") %>% 
  hc_yAxis(title = list(text = "Science Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr chart3 
ggplot(data = student.stat8th15,aes(mathScore, scienceScore)) +
  geom_point() +
  xlab("Math Score") + ylab("Science Score") +
  labs(title = "Math vs. Science Scores Worldwide 8th Grade",subtitle = "Based on the data") + 
  scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=1,
                                 color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")))



## @knitr chart4
student.stat8th15 %>% filter(idcntry==364) %>%
  hchart(type= "scatter" , hcaes(x = mathScore , y = scienceScore)) %>% 
  hc_xAxis(title = list(text = "Math Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_title(text = "Math vs. Science Scores In Iran 8th Grade") %>% 
  hc_yAxis(title = list(text = "Science Score",style = list(color = "black")),tickColor = "black",tickLength = 3,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr cor1
Timss4thCompleteStat %>% 
  ungroup() %>% 
  select(`Data Display`:Physics) %>% 
  as.matrix() %>% 
  rcorr()  %>% 
  .$r %>% 
  round(2) -> Timss4thCorrelation


hchart(Timss4thCorrelation) %>%
  hc_plotOptions(series = list(boderWidth = 0,
                               dataLabels = list(enabled = TRUE))) %>% 
  hc_title(text = "Correlation Matrix 4th Grade Subscales Worldwide") 

## @knitr cor2
Timss4thCompleteStat %>% 
  filter(idcntry == 364) %>% 
  ungroup() %>% 
  select(`Data Display`:Physics) %>% 
  as.matrix() %>% 
  rcorr()  %>% 
  .$r %>% 
  round(2) -> Iran4thCorrelation

hchart(Iran4thCorrelation) %>%
  hc_plotOptions(series = list(boderWidth = 0,
                               dataLabels = list(enabled = TRUE))) %>% 
  hc_title(text = "Correlation Matrix 4th Grade Subscales In Iran") 

## @knitr cor3
Timss8thCompleteStat %>% 
  ungroup() %>% 
  select(Algebra:Physics) %>% 
  as.matrix() %>% 
  rcorr()  %>% 
  .$r %>% 
  round(2) -> Timss8thCorrelation


hchart(Timss8thCorrelation) %>%
  hc_plotOptions(series = list(boderWidth = 0,
                               dataLabels = list(enabled = TRUE))) %>% 
  hc_title(text = "Correlation Matrix 8th Grade Subscales Worldwide") 

## @knitr cor4
Timss8thCompleteStat %>% 
  filter(idcntry == 364) %>% 
  ungroup() %>% 
  select(Algebra:Physics) %>% 
  as.matrix() %>% 
  rcorr()  %>% 
  .$r %>% 
  round(2) -> Iran8thCorrelation

hchart(Iran8thCorrelation) %>%
  hc_plotOptions(series = list(boderWidth = 0,
                               dataLabels = list(enabled = TRUE))) %>% 
  hc_title(text = "Correlation Matrix 8th Grade Subscales In Iran") 

## @knitr regression
lm(`Earth Science` ~ Algebra + `Data And Chance` + Number + Geometry , data = Timss8thCompleteStat) -> EarthScienceRegression
lm(Chemistry ~ Algebra + `Data And Chance` + Number + Geometry , data = Timss8thCompleteStat) -> ChemistryRegression
lm(Physics ~ Algebra + `Data And Chance` + Number + Geometry , data = Timss8thCompleteStat) -> PhysicsRegression
lm(Biology ~ Algebra + `Data And Chance` + Number + Geometry , data = Timss8thCompleteStat) -> BiologyRegression

rbind(ChemistryRegression$coefficients,EarthScienceRegression$coefficients,
      BiologyRegression$coefficients,PhysicsRegression$coefficients) %>% 
  as.data.frame() -> ScienceRegressionCoefficients 
rownames(ScienceRegressionCoefficients) <- c("Chemistry","Earth Science","Biology","Physics")

ScienceRegressionCoefficients %>% 
  select(Algebra:Geometry) %>% 
  round(3) -> ScienceRegressionCoefficients

## @knitr dataSection
library(jsonlite)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(ggthemr)
library(shiny)

####################################################################################
##########################   TIMSS  ################################################
####################################################################################
## WORDWIDE


######### 2015 TIMSS ########
bcg15 <- read_rds("../2015/bcg.rds")
bsa15 <- read_rds("../2015/bsa.rds")
bsg15 <- read_rds("../2015/bsg.rds")
#bsr15 <- read_rds("../2015/bsr.rds")
bst15 <- read_rds("../2015/bst.rds")
btm15 <- read_rds("../2015/btm.rds")
bts15 <- read_rds("../2015/bts.rds")

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

## 2015 TIMSS IRAN 

ibcg15 <- bcg15 %>%
  filter(idcntry == 364)
ibsa15 <- bsa15 %>%
  filter(idcntry == 364)
ibsg15 <- bsg15 %>%
  filter(idcntry == 364)
#ibsr15 <- bsr15 %>%
#filter(idcntry == 364)
ibst15 <- bst15 %>%
  filter(idcntry == 364)
ibtm15 <- btm15 %>%
  filter(idcntry == 364)
ibts15 <- bts15 %>%
  filter(idcntry == 364)

## 2011 TIMSS IRAN 

ibcg11 <- bcg11 %>%
  filter(idcntry == 364)
ibsa11 <- bsa11 %>%
  filter(idcntry == 364)
ibsg11 <- bsg11 %>%
  filter(idcntry == 364)
#ibsr11 <- bsr11 %>%
# filter(idcntry == 364)
ibst11 <- bst11 %>%
  filter(idcntry == 364)
ibtm11 <- btm11 %>%
  filter(idcntry == 364)
ibts11 <- bts11 %>%
  filter(idcntry == 364)

## 2007 TIMSS IRAN 

ibcg07 <- bcg07 %>%
  filter(idcntry == 364)
ibsa07 <- bsa07 %>%
  filter(idcntry == 364)
ibsg07 <- bsg07 %>%
  filter(idcntry == 364)
#ibsr07 <- bsr07 %>%
# filter(idcntry == 364)
ibst07 <- bst07 %>%
  filter(idcntry == 364)
ibtm07 <- btm07 %>%
  filter(idcntry == 364)
ibts07 <- bts07 %>%
  filter(idcntry == 364)

## 2003 TIMSS IRAN 

ibcg03 <- bcg03 %>%
  filter(idcntry == 364)
ibsa03 <- bsa03 %>%
  filter(idcntry == 364)
ibsg03 <- bsg03 %>%
  filter(idcntry == 364)
#ibsr03 <- bsr03 %>%
# filter(idcntry == 364)
ibst03 <- bst03 %>%
  filter(idcntry == 364)
ibtm03 <- btm03 %>%
  filter(idcntry == 364)
ibts03 <- bts03 %>%
  filter(idcntry == 364)


#### IRAN 

# ST_SCORES 2015 ----------------------------------------------------------

ibscores15 <- ibsa15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  mutate(mat.state = ifelse(mat >= 625, "Advanced", 
                            ifelse(mat >= 550, "High", 
                                   ifelse(mat >= 475, "Intermediate", "Low"))), 
         sci.state = ifelse(sci >= 625, "Advanced", 
                            ifelse(sci >= 550, "High", 
                                   ifelse(sci >= 475, "Intermediate", "Low"))), 
         over.state = ifelse(overall >= 625, "Advanced", 
                             ifelse(overall >= 550, "High", 
                                    ifelse(overall >= 475, "Intermediate", "Low"))))
ibscores15 %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall), 
            mat.avg = mean(mat), 
            sci.avg = mean(sci)) -> ibschool15
ibst15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(mat.avg = mean(mat), 
            sci.avg = mean(sci), 
            over.avg = mean(overall)) -> ibteacher15


# ST_SCORES 2011 ----------------------------------------------------------
ibscores11 <- ibsa11 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  mutate(mat.state = ifelse(mat >= 625, "Advanced", 
                            ifelse(mat >= 550, "High", 
                                   ifelse(mat >= 475, "Intermediate", "Low"))), 
         sci.state = ifelse(sci >= 625, "Advanced", 
                            ifelse(sci >= 550, "High", 
                                   ifelse(sci >= 475, "Intermediate", "Low"))), 
         over.state = ifelse(overall >= 625, "Advanced", 
                             ifelse(overall >= 550, "High", 
                                    ifelse(overall >= 475, "Intermediate", "Low"))))
ibscores11 %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall), 
            mat.avg = mean(mat), 
            sci.avg = mean(sci)) -> ibschool11
ibst11 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(mat.avg = mean(mat), 
            sci.avg = mean(sci), 
            over.avg = mean(overall)) -> ibteacher11


# ST_SCORE 2007 -----------------------------------------------------------
ibscores07 <- ibsa07 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  mutate(mat.state = ifelse(mat >= 625, "Advanced", 
                            ifelse(mat >= 550, "High", 
                                   ifelse(mat >= 475, "Intermediate", "Low"))), 
         sci.state = ifelse(sci >= 625, "Advanced", 
                            ifelse(sci >= 550, "High", 
                                   ifelse(sci >= 475, "Intermediate", "Low"))), 
         over.state = ifelse(overall >= 625, "Advanced", 
                             ifelse(overall >= 550, "High", 
                                    ifelse(overall >= 475, "Intermediate", "Low"))))
ibscores07 %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall), 
            mat.avg = mean(mat), 
            sci.avg = mean(sci)) -> ibschool07
ibst07 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(mat.avg = mean(mat), 
            sci.avg = mean(sci), 
            over.avg = mean(overall)) -> ibteacher07


# ST_SCORE 2003 -----------------------------------------------------------
ibscores03 <- ibsa03 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  mutate(mat.state = ifelse(mat >= 625, "Advanced", 
                            ifelse(mat >= 550, "High", 
                                   ifelse(mat >= 475, "Intermediate", "Low"))), 
         sci.state = ifelse(sci >= 625, "Advanced", 
                            ifelse(sci >= 550, "High", 
                                   ifelse(sci >= 475, "Intermediate", "Low"))), 
         over.state = ifelse(overall >= 625, "Advanced", 
                             ifelse(overall >= 550, "High", 
                                    ifelse(overall >= 475, "Intermediate", "Low"))))
ibscores03 %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall),
            mat.avg = mean(mat), 
            sci.avg = mean(sci)) -> ibschool03

ibst03 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(over.avg = mean(overall),
            mat.avg = mean(mat), 
            sci.avg = mean(sci)) -> ibteacher03

## @knitr numOfPARTICIPANTS
# NUMBER OF PARTICIPANTS IN IRAN  ----------------------------------------------

totnum <- rbind(bcg15 %>%
                  group_by(idcntry) %>%
                  summarise(count = n()) %>%
                  mutate(year = 2015),
                bcg11 %>%
                  group_by(idcntry) %>%
                  summarise(count = n()) %>%
                  mutate(year = 2011),
                bcg07 %>%
                  group_by(idcntry) %>%
                  summarise(count = n()) %>%
                  mutate(year = 2007),
                bcg03 %>%
                  group_by(idcntry) %>%
                  summarise(count = n()) %>%
                  mutate(year = 2003)) %>%
  mutate(idcntry = as.character(idcntry))

library(ISOcodes)
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2) %>%
  mutate(idcntry = as.character(idcntry))
totnum <- left_join(totnum, codes, by = "idcntry") %>%
  filter(!is.na(country)) %>%
  mutate(country = tolower(country)) %>%
  group_by(country) %>%
  arrange(year) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(count) %>%
  mutate(rank = rank(-count), 
         rank = row_number(rank))
totnum15 <- totnum %>% filter(year == 2015)
totnum11 <- totnum %>% filter(year == 2011)
totnum07 <- totnum %>% filter(year == 2007)
totnum03 <- totnum %>% filter(year == 2003)
totnum15$country <- factor(totnum15$country, 
                           levels = totnum15$country[order(totnum15$rank)])
totnum11$country <- factor(totnum11$country, 
                           levels = totnum11$country[order(totnum11$rank)])
totnum07$country <- factor(totnum07$country, 
                           levels = totnum07$country[order(totnum07$rank)])
totnum03$country <- factor(totnum03$country, 
                           levels = totnum03$country[order(totnum03$rank)])

ggthemr('grape')
p1 <- (ggplot(totnum15,
              aes(x = country, y = count, fill = (country == 'ir'))) + 
         geom_bar(stat = "identity") +
         theme_minimal()) %>%
  ggplotly(tooltip = c("country", "count"))  %>% hide_legend() %>%
  layout(xaxis = list(showticklabels = F))

p2 <- (ggplot(totnum11,
              aes(x = country, y = count, fill = (country == 'ir'))) + 
         geom_bar(stat = "identity") +
         theme_minimal()) %>%
  ggplotly(tooltip = c("country", "count"))  %>% hide_legend() %>%
  layout(xaxis = list(showticklabels = F))

p3 <- (ggplot(totnum07,
              aes(x = country, y = count, fill = (country == 'ir'))) + 
         geom_bar(stat = "identity") + 
         theme_minimal()) %>% ggplotly(tooltip = c("country", "count"))  %>% hide_legend() %>%
  layout(xaxis = list(showticklabels = F))

p4 <- (ggplot(totnum03,
              aes(x = country, y = count, fill = (country == 'ir'))) + 
         geom_bar(stat = "identity") + 
         theme_minimal()) %>% ggplotly(tooltip = c("country", "count"))  %>% hide_legend() %>%
  layout(xaxis = list(showticklabels = F))

p <- subplot(p1, p2, p3, p4, nrows = 2) %>% 
  layout(title = "Timss 8th Grade: Number of Participants in Each Country\nFrom top left to bottom down: 2015 to 2003", 
         margin = list(l = 80, b = 50, t = 150, pad = 4))
div(p, align = 'left')

rm(totnum, totnum03, totnum07, totnum11, totnum15, p, p1, p2, p3, p4)


## @knitr iranAvg
# IRAN AVERAGE SCORE THROUGHT THE YEARS -----------------------------------

iavg <- rbind(ibscores15 %>%
                group_by(idcntry) %>%
                summarise(over.avg = mean(overall), 
                          mat.avg = mean(mat), 
                          sci.avg = mean(sci)) %>%
                mutate(year = 2015), 
              ibscores11 %>%
                group_by(idcntry) %>%
                summarise(over.avg = mean(overall), 
                          mat.avg = mean(mat), 
                          sci.avg = mean(sci)) %>%
                mutate(year = 2011), 
              ibscores07 %>%
                group_by(idcntry) %>%
                summarise(over.avg = mean(overall), 
                          mat.avg = mean(mat), 
                          sci.avg = mean(sci)) %>%
                mutate(year = 2007), 
              ibscores03 %>%
                group_by(idcntry) %>%
                summarise(over.avg = mean(overall), 
                          mat.avg = mean(mat), 
                          sci.avg = mean(sci)) %>%
                mutate(year = 2003)) 
library("viridisLite")
cols <- viridis(3)
cols <- substr(cols, 0, 7)
highchart() %>% 
  hc_chart(type = "bar", showLegend = F) %>% 
  hc_title(text = "12 Years of Iran Timss History in 12 Seconds",
           align = 'center', 
           style = list(fontFamily = 'B-Nazanin', 
                        fontWeight = "italic", fontSize = "35px")) %>%
  hc_xAxis(categories = c("Overall", "Mathematics", "Science"),
           gridLineColor = "transparent", lineColor = 'black', 
           tickColor = "black", tickLength = 30, 
           labels = list(style = list(fontSize="20px"))) %>%
  hc_add_series(name = "Score",
                data = list(
                  list(sequence = iavg$over.avg %>% round(3), color = 'green', borderColor = 'black'),
                  list(sequence = iavg$mat.avg %>% round(3), color = 'white', borderColor = 'black'),
                  list(sequence = iavg$sci.avg %>% round(3), color = 'red', borderColor = 'black')
                )) %>% 
  hc_colors("skyblue") %>% 
  hc_motion(enabled = TRUE,
            labels = seq(2003, 2015, 4),
            series = c(1,0),
            updateIterval = 1000,
            autoPlay = T) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_plotOptions(
    series = list(
      showInLegend =F    
    )) 

rm(iavg)

## @knitr iranAvg2
# IRAN AVERAGE SCORE THROUGHOUT THE YEARS - DOMAIN ------------------------

ibscorestot15 <- ibst15 %>%
  mutate(algebra = (bsmalg01 + bsmalg02 + bsmalg03 + bsmalg04 + bsmalg05) / 5, 
         data = (bsmdat01 + bsmdat02 + bsmdat03 + bsmdat04 + bsmdat05) / 5, 
         number = (bsmnum01 + bsmnum02 + bsmnum03 + bsmnum04 + bsmnum05) / 5, 
         geometry = (bsmgeo01 + bsmgeo02 + bsmgeo03 + bsmgeo04 + bsmgeo05) / 5, 
         chemistry = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         earth = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         bio = (bssbio01 + bssbio02 + bssbio03 + bssbio04 + bssbio05) / 5, 
         physics = (bssphy01 + bssphy02 + bssphy03 + bssphy04 + bssphy05) / 5) %>%
  mutate(year = 2015)

ibscorestot11 <- ibst11 %>%
  mutate(algebra = (bsmalg01 + bsmalg02 + bsmalg03 + bsmalg04 + bsmalg05) / 5, 
         data = (bsmdat01 + bsmdat02 + bsmdat03 + bsmdat04 + bsmdat05) / 5, 
         number = (bsmnum01 + bsmnum02 + bsmnum03 + bsmnum04 + bsmnum05) / 5, 
         geometry = (bsmgeo01 + bsmgeo02 + bsmgeo03 + bsmgeo04 + bsmgeo05) / 5, 
         chemistry = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         earth = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         bio = (bssbio01 + bssbio02 + bssbio03 + bssbio04 + bssbio05) / 5, 
         physics = (bssphy01 + bssphy02 + bssphy03 + bssphy04 + bssphy05) / 5) %>%
  mutate(year = 2011)

ibscorestot07 <- ibst07 %>%
  mutate(algebra = (bsmalg01 + bsmalg02 + bsmalg03 + bsmalg04 + bsmalg05) / 5, 
         data = (bsmdat01 + bsmdat02 + bsmdat03 + bsmdat04 + bsmdat05) / 5, 
         number = (bsmnum01 + bsmnum02 + bsmnum03 + bsmnum04 + bsmnum05) / 5, 
         geometry = (bsmgeo01 + bsmgeo02 + bsmgeo03 + bsmgeo04 + bsmgeo05) / 5, 
         chemistry = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         earth = (bssche01 + bssche02 + bssche03 + bssche04 + bssche05) / 5, 
         bio = (bssbio01 + bssbio02 + bssbio03 + bssbio04 + bssbio05) / 5, 
         physics = (bssphy01 + bssphy02 + bssphy03 + bssphy04 + bssphy05) / 5) %>%
  mutate(year = 2007)

iavgdomain <- rbind(ibscorestot15 %>%
                      group_by(idcntry) %>%
                      summarise(algebra.avg = mean(algebra), 
                                data.avg = mean(data), 
                                number.avg = mean(number), 
                                geometry.avg = mean(geometry), 
                                chemistry.avg = mean(chemistry), 
                                earth.avg = mean(earth), 
                                bio.avg = mean(bio), 
                                physics.avg = mean(physics)) %>%
                      mutate(year = 2015), 
                    ibscorestot11 %>%
                      group_by(idcntry) %>%
                      summarise(algebra.avg = mean(algebra), 
                                data.avg = mean(data), 
                                number.avg = mean(number), 
                                geometry.avg = mean(geometry), 
                                chemistry.avg = mean(chemistry), 
                                earth.avg = mean(earth), 
                                bio.avg = mean(bio), 
                                physics.avg = mean(physics)) %>%
                      mutate(year = 2011), 
                    ibscorestot07 %>%
                      group_by(idcntry) %>%
                      summarise(algebra.avg = mean(algebra), 
                                data.avg = mean(data), 
                                number.avg = mean(number), 
                                geometry.avg = mean(geometry), 
                                chemistry.avg = mean(chemistry), 
                                earth.avg = mean(earth), 
                                bio.avg = mean(bio), 
                                physics.avg = mean(physics)) %>%
                      mutate(year = 2007))

iavgdomain <- iavgdomain %>%
  gather("subject", "average", algebra.avg:physics.avg) %>%
  mutate(subject = str_replace(subject, '.avg', ''))
ggthemr_reset()
p <- ggplot(iavgdomain, aes(x = subject, y = average)) + 
  geom_bar(aes(fill = as.factor(year)), stat = "identity", color = 'black', size = 0.25) + 
  theme_minimal() + 
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        axis.title = element_blank()) + 
  scale_fill_manual(values = c("red", "white", "forestgreen"))
(ggplotly(p, tooltip = c("average")) %>% hide_legend() %>%
    layout(title = "Iranian's Average Score in Each Domain", 
           margin = list(l = 80, b = 75, t = 75, pad = 4))) %>% div(align = 'center')

rm(ibscorestot07, ibscorestot11, ibscorestot15, 
   p, iavgdomain)

## @knitr iranRank
# IRAN INTERNATIONAL RANK -------------------------------------------------


international.ranking15 <- bsg15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry) %>%
  summarise(mat.avg = mean(mat), sci.avg = mean(sci), over.avg = mean(overall)) %>%
  arrange(desc(over.avg)) %>%
  mutate(rank.over = row_number(-over.avg)) %>%
  arrange(desc(sci.avg)) %>%
  mutate(rank.sci = row_number(-sci.avg)) %>%
  arrange(desc(mat.avg)) %>%
  mutate(rank.mat = row_number(-mat.avg)) %>%
  filter(idcntry == 364) %>%
  select(idcntry, rank.over, rank.sci, rank.mat) %>%
  mutate(year = 2015)

international.ranking11 <- bsg11 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry) %>%
  summarise(mat.avg = mean(mat), sci.avg = mean(sci), over.avg = mean(overall)) %>%
  arrange(desc(over.avg)) %>%
  mutate(rank.over = row_number(-over.avg)) %>%
  arrange(desc(sci.avg)) %>%
  mutate(rank.sci = row_number(-sci.avg)) %>%
  arrange(desc(mat.avg)) %>%
  mutate(rank.mat = row_number(-mat.avg)) %>%
  filter(idcntry == 364) %>%
  select(idcntry, rank.over, rank.sci, rank.mat) %>%
  mutate(year = 2011)

international.ranking07 <- bsg07 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry) %>%
  summarise(mat.avg = mean(mat), sci.avg = mean(sci), over.avg = mean(overall)) %>%
  arrange(desc(over.avg)) %>%
  mutate(rank.over = row_number(-over.avg)) %>%
  arrange(desc(sci.avg)) %>%
  mutate(rank.sci = row_number(-sci.avg)) %>%
  arrange(desc(mat.avg)) %>%
  mutate(rank.mat = row_number(-mat.avg)) %>%
  filter(idcntry == 364) %>%
  select(idcntry, rank.over, rank.sci, rank.mat) %>%
  mutate(year = 2007)

international.ranking03 <- bsg03 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry) %>%
  summarise(mat.avg = mean(mat), sci.avg = mean(sci), over.avg = mean(overall)) %>%
  arrange(desc(over.avg)) %>%
  mutate(rank.over = row_number(-over.avg)) %>%
  arrange(desc(sci.avg)) %>%
  mutate(rank.sci = row_number(-sci.avg)) %>%
  arrange(desc(mat.avg)) %>%
  mutate(rank.mat = row_number(-mat.avg)) %>%
  filter(idcntry == 364) %>%
  select(idcntry, rank.over, rank.sci, rank.mat) %>%
  mutate(year = 2003)

international.ranking <- rbind(international.ranking15, 
                               international.ranking11, 
                               international.ranking07, 
                               international.ranking03) %>%
  select(year, `Overall Rank` = rank.over, 
         `Mathematics Rank` = rank.mat, 
         `Science Rank` = rank.sci)

international.ranking

rm(international.ranking, international.ranking03, 
   international.ranking07, international.ranking11, 
   international.ranking15)


## @knitr benchmarkpercentage
# BENCHMARK PERCENTAGE ----------------------------------------------------

istperc15 <- ibscores15 %>%
  group_by(over.state) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(year = 2015)

istperc11 <- ibscores11 %>%
  group_by(over.state) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(year = 2011)

istperc07 <- ibscores07 %>%
  group_by(over.state) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(year = 2007)

istperc03 <- ibscores03 %>%
  group_by(over.state) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(year = 2003)

colors <- c('gold', 'lilac', 'skyblue', 'lightgray')
plot_ly(textposition = 'inside',
        textinfo = 'percent',
        insidetextfont = list(color = '#FFFFFF'), 
        hoverinfo = 'text',
        text = ~paste('Number of <br>Students:', count), 
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1))) %>%
  add_pie(data = istperc03, labels = ~over.state, values = ~count,
          name = "Cut", domain = list(x = c(0.2, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = istperc07, labels = ~over.state, values = ~count,
          name = "Color", domain = list(x = c(0.8, 1), y = c(0.4, 1))) %>%
  add_pie(data = istperc11, labels = ~over.state, values = ~count,
          name = "Clarity", domain = list(x = c(0.2, 0.4), y = c(0, 0.6))) %>%
  add_pie(data = istperc15, labels = ~over.state, values = ~count,
          name = "Clarity", domain = list(x = c(0.8, 1), y = c(0, 0.6))) %>%
  layout(title = 'Percentage of Students In Each Category \nBased on Timss International Benchmarks',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

rm(istperc03, istperc07, istperc11, istperc15)


## @knitr data
## 8TH GRADE

# 2015

ibsg15.res <- ibsg15 %>%
  mutate(booknum = bsbg04, 
         internet = bsbg06e, 
         room = bsbg06d, 
         medu = ifelse(is.na(bsbg07a) | bsbg07a >= 8, 0, bsbg07a), 
         fedu = ifelse(is.na(bsbg07b) | bsbg07b >= 8, 0, bsbg07b), 
         pedu = ifelse(medu > fedu, medu, fedu)) %>%
  mutate(res.cat = ifelse((booknum >= 4) & (room == 1) & (internet == 1) & (pedu >= 6), "Many",
                          ifelse((booknum <= 2) & (room == 2) & (internet == 2) & (pedu <= 3), "Few", "Some"))) %>%
  filter(!is.na(res.cat)) %>%
  select(idstud, idschool, res.cat, booknum, internet, room, pedu, medu, fedu) %>%
  mutate(year = 2015)

ihome15.stat8th <- left_join(ibsg15.res, 
                             ibscores15, 
                             by = c("idstud", "idschool")) %>%
  select(idstud, idschool, res.cat, year,
         overall, mat, sci, over.state, mat.state, sci.state)

ihome15.stat8th$res.cat <- factor(ihome15.stat8th$res.cat,
                                  levels = c("Many",
                                             "Some", 
                                             "Few"))
ihome15.stat8th.summary <- ihome15.stat8th %>%
  group_by(over.state, res.cat, year) %>%
  summarise(freq = n())
ihome15.stat8th.avg <- ihome15.stat8th %>%
  group_by(res.cat, year) %>%
  summarise(over.avg = mean(overall), mat.avg = mean(mat), sci.avg = mean(sci), 
            participants = n())

# 2011

ibsg11.res <- ibsg11 %>%
  mutate(booknum = bsbg04, 
         internet = bsbg05e, 
         room = bsbg05d, 
         medu = ifelse(is.na(bsbg06a) | bsbg06a >= 8, 0, bsbg06a), 
         fedu = ifelse(is.na(bsbg06b) | bsbg06b >= 8, 0, bsbg06b), 
         pedu = ifelse(medu > fedu, medu, fedu)) %>%
  filter(booknum <= 4) %>%
  mutate(res.cat = ifelse((booknum >= 4) & (room == 1) & (internet == 1) & (pedu >= 6), "Many",
                          ifelse((booknum <= 2) & (room == 2) & (internet == 2) & (pedu <= 3), "Few", "Some"))) %>%
  filter(!is.na(res.cat)) %>%
  select(idstud, idschool, res.cat) %>%
  mutate(year = 2011)

ihome11.stat8th <- left_join(ibsg11.res, 
                             ibscores11, 
                             by = c("idstud", "idschool")) %>%
  select(idstud, idschool, res.cat, year,
         overall, mat, sci, over.state, mat.state, sci.state)

ihome11.stat8th$res.cat <- factor(ihome11.stat8th$res.cat,
                                  levels = c("Many",
                                             "Some", 
                                             "Few"))
ihome11.stat8th.summary <- ihome11.stat8th %>%
  group_by(over.state, res.cat, year) %>%
  summarise(freq = n())
ihome11.stat8th.avg <- ihome11.stat8th %>%
  group_by(res.cat, year) %>%
  summarise(over.avg = mean(overall), mat.avg = mean(mat), sci.avg = mean(sci), 
            participants = n())

# 2007

ibsg07.res <- ibsg07 %>%
  mutate(booknum = bs4gbook, 
         internet = bs4gth05, 
         desk = bs4gth03, 
         medu = ifelse(is.na(bs4gmfed) | bs4gmfed >= 8, 0, bs4gmfed), 
         fedu = ifelse(is.na(bs4gfmed) | bs4gfmed >= 8, 0, bs4gfmed), 
         pedu = ifelse(medu > fedu, medu, fedu)) %>%
  filter(booknum <= 4) %>%
  mutate(res.cat = ifelse((booknum >= 4) & (desk == 1) & (internet == 1) & (pedu >= 6), "Many",
                          ifelse((booknum <= 2) & (desk == 2) & (internet == 2) & (pedu <= 3), "Few", "Some"))) %>%
  filter(!is.na(res.cat)) %>%
  select(idstud, idschool, res.cat) %>%
  mutate(year = 2007)

ihome07.stat8th <- left_join(ibsg07.res, 
                             ibscores07, 
                             by = c("idstud", "idschool")) %>%
  select(idstud, idschool, res.cat, year,
         overall, mat, sci, over.state, mat.state, sci.state)

ihome07.stat8th$res.cat <- factor(ihome07.stat8th$res.cat,
                                  levels = c("Many",
                                             "Some", 
                                             "Few"))
ihome07.stat8th.summary <- ihome07.stat8th %>%
  group_by(over.state, res.cat, year) %>%
  summarise(freq = n())
ihome07.stat8th.avg <- ihome07.stat8th %>%
  group_by(res.cat, year) %>%
  summarise(over.avg = mean(overall), mat.avg = mean(mat), sci.avg = mean(sci), 
            participants = n())

# 2003

ibsg03.res <- ibsg03 %>%
  mutate(booknum = bsbgbook, 
         computer = bsbgps02, 
         desk = bsbgps03, 
         medu = ifelse(is.na(bsbgmfed) | bsbgmfed >= 8, 0, bsbgmfed), 
         fedu = ifelse(is.na(bsbgfmed) | bsbgfmed >= 8, 0, bsbgfmed), 
         pedu = ifelse(medu > fedu, medu, fedu)) %>%
  filter(booknum <= 4) %>%
  mutate(res.cat = ifelse((booknum >= 4) & (desk == 1) & (computer == 1) & (pedu >= 6), "Many",
                          ifelse((booknum <= 2) & (desk == 2) & (computer == 2) & (pedu <= 3), "Few", "Some"))) %>%
  filter(!is.na(res.cat)) %>%
  select(idstud, idschool, res.cat) %>%
  mutate(year = 2003)

ihome03.stat8th <- left_join(ibsg03.res, 
                             ibscores03, 
                             by = c("idstud", "idschool")) %>%
  select(idstud, idschool, res.cat, year,
         overall, mat, sci, over.state, mat.state, sci.state)

ihome03.stat8th$res.cat <- factor(ihome03.stat8th$res.cat,
                                  levels = c("Many",
                                             "Some", 
                                             "Few"))
ihome03.stat8th.summary <- ihome03.stat8th %>%
  group_by(over.state, res.cat, year) %>%
  summarise(freq = n())
ihome03.stat8th.avg <- ihome03.stat8th %>%
  group_by(res.cat, year) %>%
  summarise(over.avg = mean(overall), mat.avg = mean(mat), sci.avg = mean(sci), 
            participants = n())


## @knitr chart1
## 2003 - 2015
ihome.stat8th <- rbind(ihome15.stat8th, ihome11.stat8th, 
                       ihome07.stat8th, ihome03.stat8th)
ihome.stat8th.summary <- rbind(ihome15.stat8th.summary, ihome11.stat8th.summary, 
                               ihome07.stat8th.summary, ihome03.stat8th.summary)
ihome.stat8th.avg <- rbind(ihome15.stat8th.avg, ihome11.stat8th.avg,
                           ihome07.stat8th.avg, ihome03.stat8th.avg) %>%
  mutate(tmp = paste(res.cat, year, sep = " "))

## STACKED BAR CHART

mynewdata2 <- ihome.stat8th.avg %>%
  group_by(name = res.cat) %>%  
  do(data = round(.$over.avg, 2)) 
series <- list_parse(mynewdata2)


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "More Resources, More Scores",
           align = 'center', 
           style = list(fontFamily = 'B-Nazanin', 
                        fontWeight = "italic", fontSize = "35px")) %>%
  hc_subtitle(text = "Score Gap of Over A 100 - A Visual Demonstration", 
              align = 'center', 
              style = list(fontFamily = 'B-Nazanin', 
                           fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(lineColor = 'black', tickColor = "black", tickLength = 30, 
           labels = list(style = list(fontSize="20px"))) %>%
  hc_yAxis(title = list(text = "Average Score", 
                        style = list(fontSize="20px"))) %>% 
  hc_plotOptions(column = list(
    dataLabels = list(enabled = TRUE),
    stacking = "normal")
  ) %>% 
  hc_xAxis(categories = c(2003, 2007, 2011, 2015)) %>%
  hc_add_series_list(series) %>%
  hc_add_theme(hc_theme_economist())

#####
ggthemr('fresh')
ggplot(ihome.stat8th) + 
  geom_boxplot(aes(x = as.factor(year), y = overall, fill = res.cat), outlier.alpha = 0) +
  labs(x = "Year", y = "Overall Score", 
       title = "More Resources At Home: More Scores At School", 
       subtitle = "Don't belive us, just watch") + 
  theme_minimal() + 
  theme(plot.subtitle=element_text(size = 12), 
        plot.title = element_text(size = 20)) + 
  guides(fill=guide_legend(title="Resources")) + 
  scale_x_discrete() + 
  scale_y_continuous()

rm(ibsg03.res, ibsg07.res, ibsg11.res, ibsg15.res, 
   ihome.stat8th, ihome.stat8th.avg, ihome.stat8th.summary, 
   ihome03.stat8th, ihome03.stat8th.avg, ihome03.stat8th.summary, 
   ihome07.stat8th, ihome07.stat8th.avg, ihome07.stat8th.summary, 
   ihome11.stat8th, ihome11.stat8th.avg, ihome11.stat8th.summary, 
   ihome15.stat8th, ihome15.stat8th.avg, ihome15.stat8th.summary, 
   mynewdata2)

## @knitr economic
## ECONOMIC BACKGROUND

#8TH GRADE

# 2015
ibcg15.econ <- ibcg15 %>%
  mutate(affluent = bcbg03b, disadvantage = bcbg03a, 
         econ.cat = ifelse((affluent >= 3) & (disadvantage <= 2), "More Affluent", 
                           ifelse((affluent <= 2) & (disadvantage >= 3), "More Disadvantages", 
                                  "Neutral"))) %>%
  select(idschool, econ.cat) %>%
  filter(!is.na(econ.cat)) %>%
  mutate(year = 2015)

iecon15.stat8th <- left_join(ibcg15.econ, ibschool15, by = "idschool") %>%
  select(idschool, econ.cat, over.avg, mat.avg, sci.avg, year)
iecon15.stat8th$econ.cat <- factor(iecon15.stat8th$econ.cat,
                                   levels = c("More Affluent",
                                              "Neutral", 
                                              "More Disadvantages"))

iecon15.stat8th.avg <- iecon15.stat8th %>%
  group_by(econ.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))


# 2011
ibcg11.econ <- ibcg11 %>%
  mutate(affluent = bcbg03b, disadvantage = bcbg03a, 
         econ.cat = ifelse((affluent >= 3) & (disadvantage <= 2), "More Affluent", 
                           ifelse((affluent <= 2) & (disadvantage >= 3), "More Disadvantages", 
                                  "Neutral"))) %>%
  select(idschool, econ.cat) %>%
  filter(!is.na(econ.cat)) %>%
  mutate(year = 2011)

iecon11.stat8th <- left_join(ibcg11.econ, ibschool11, by = "idschool") %>%
  select(idschool, econ.cat, over.avg, mat.avg, sci.avg, year)
iecon11.stat8th$econ.cat <- factor(iecon11.stat8th$econ.cat,
                                   levels = c("More Affluent",
                                              "Neutral", 
                                              "More Disadvantages"))

iecon11.stat8th.avg <- iecon11.stat8th %>%
  group_by(econ.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2007
ibcg07.econ <- ibcg07 %>%
  mutate(affluent = bc4gsbea, disadvantage = bc4gsbed, 
         econ.cat = ifelse((affluent >= 3) & (disadvantage <= 2), "More Affluent", 
                           ifelse((affluent <= 2) & (disadvantage >= 3), "More Disadvantages", 
                                  "Neutral"))) %>%
  select(idschool, econ.cat) %>%
  filter(!is.na(econ.cat)) %>%
  mutate(year = 2007)

iecon07.stat8th <- left_join(ibcg07.econ, ibschool07, by = "idschool") %>%
  select(idschool, econ.cat, over.avg, mat.avg, sci.avg, year)
iecon07.stat8th$econ.cat <- factor(iecon07.stat8th$econ.cat,
                                   levels = c("More Affluent",
                                              "Neutral", 
                                              "More Disadvantages"))

iecon07.stat8th.avg <- iecon07.stat8th %>%
  group_by(econ.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2003
ibcg03.econ <- ibcg03 %>%
  mutate(affluent = bcbgsbea, disadvantage = bcbgsbed, 
         econ.cat = ifelse((affluent >= 3) & (disadvantage <= 2), "More Affluent", 
                           ifelse((affluent <= 2) & (disadvantage >= 3), "More Disadvantages", 
                                  "Neutral"))) %>%
  select(idschool, econ.cat) %>%
  filter(!is.na(econ.cat)) %>%
  mutate(year = 2003)


iecon03.stat8th <- left_join(ibcg03.econ, ibschool03, by = "idschool") %>%
  select(idschool, econ.cat, over.avg, mat.avg, sci.avg, year)
iecon03.stat8th$econ.cat <- factor(iecon03.stat8th$econ.cat,
                                   levels = c("More Affluent",
                                              "Neutral", 
                                              "More Disadvantages"))

iecon03.stat8th.avg <- iecon03.stat8th %>%
  group_by(econ.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2003 - 2015

iecon.stat8th <- rbind(iecon03.stat8th, iecon07.stat8th, iecon11.stat8th, iecon15.stat8th)
iecon.stat8th.avg <- rbind(iecon03.stat8th.avg, iecon07.stat8th.avg, iecon11.stat8th.avg, iecon15.stat8th.avg)


ggplot(iecon.stat8th) + 
  geom_boxplot(aes(x = as.factor(year), y = over.avg, fill = econ.cat), outlier.alpha = 0) + 
  theme_minimal() + 
  guides(fill=guide_legend(title="Economic Background")) + 
  labs(x = "Year", y = "Average f Students' Overall Score",
       title = "Money Buys Knowledge") + 
  theme(plot.title = element_text(size = 20), 
        legend.position = "bottom") + 
  scale_x_discrete() + 
  scale_y_continuous()

rm(ibcg03.econ, ibcg07.econ, ibcg11.econ, ibcg15.econ, 
   iecon.stat8th, iecon.stat8th.avg, 
   iecon03.stat8th, iecon03.stat8th.avg, iecon07.stat8th, iecon07.stat8th.avg, iecon11.stat8th, iecon11.stat8th.avg, iecon15.stat8th, iecon15.stat8th.avg)

## @knitr resource
## RESOURCE SHORTAGE

#8TH GRADE

# 2015

ibcg15.short <- ibcg15 %>%
  mutate(shortage = bcbg13aa + bcbg13ab + bcbg13ac + bcbg13ad + bcbg13ae + 
           bcbg13af + bcbg13ag + bcbg13ah + bcbg13ai + 
           bcbg13ba + bcbg13bb + bcbg13bc + bcbg13bd + bcbg13be + 
           bcbg13ca + bcbg13cb + bcbg13cc + bcbg13cd + bcbg13ce) %>%
  filter(!is.na(shortage)) %>%
  mutate(shortage.cat = ifelse(shortage <= 36, "Not Affected", 
                               ifelse(shortage <= 57, "Affected", "Affected A Lot"))) %>%
  mutate(year = 2015)



ishort15.stat8th <- left_join(ibcg15.short, ibschool15, by = "idschool") %>%
  select(idschool, shortage.cat, over.avg, mat.avg, sci.avg, year)
ishort15.stat8th$shortage.cat <- factor(ishort15.stat8th$shortage.cat,
                                        levels = c("Not Affected",
                                                   "Affected", 
                                                   "Affected A Lot"))

ishort15.stat8th.avg <- ishort15.stat8th %>%
  group_by(shortage.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))


# 2011

ibcg11.short <- ibcg11 %>%
  mutate(shortage = bcbg09aa + bcbg09ab + bcbg09ac + bcbg09ad + bcbg09ae + bcbg09af +
           bcbg09ba + bcbg09bb + bcbg09bc + bcbg09bd + bcbg09be + bcbg09bf +
           bcbg09ca + bcbg09cb + bcbg09cc + bcbg09cd + bcbg09ce + bcbg09cf) %>%
  filter(!is.na(shortage)) %>%
  mutate(shortage.cat = ifelse(shortage <= 36, "Not Affected", 
                               ifelse(shortage <= 54, "Affected", "Affected A Lot"))) %>%
  mutate(year = 2011)

ishort11.stat8th <- left_join(ibcg11.short, ibschool11, by = "idschool") %>%
  select(idschool, shortage.cat, over.avg, mat.avg, sci.avg, year)
ishort11.stat8th$shortage.cat <- factor(ishort11.stat8th$shortage.cat,
                                        levels = c("Not Affected",
                                                   "Affected", 
                                                   "Affected A Lot"))

ishort11.stat8th.avg <- ishort11.stat8th %>%
  group_by(shortage.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2007

ibcg07.short <- ibcg07 %>%
  mutate(shortage = bc4gst01 + bc4gst02 + bc4gst03 + bc4gst04 + bc4gst05 + bc4gst06 +
           bc4mst07 + bc4mst08 + bc4mst09 + bc4mst10 + bc4mst11 + bc4sst12 +
           bc4sst13 + bc4sst14 + bc4sst15 + bc4sst16 + bc4sst17 + bc4gsh18 + 
           bc4gsh19) %>%
  filter(!is.na(shortage)) %>%
  mutate(shortage.cat = ifelse(shortage <= 37, "Not Affected", 
                               ifelse(shortage <= 55, "Affected", "Affected A Lot"))) %>%
  mutate(year = 2007)

ishort07.stat8th <- left_join(ibcg07.short, ibschool07, by = "idschool") %>%
  select(idschool, shortage.cat, over.avg, mat.avg, sci.avg, year)
ishort07.stat8th$shortage.cat <- factor(ishort07.stat8th$shortage.cat,
                                        levels = c("Not Affected",
                                                   "Affected", 
                                                   "Affected A Lot"))

ishort07.stat8th.avg <- ishort07.stat8th %>%
  group_by(shortage.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2003

ibcg03.short <- ibcg03 %>%
  mutate(shortage = bcbgst01 + bcbgst02 + bcbgst03 + bcbgst04 + bcbgst05 + bcbgst06 +
           bcbmst07 + bcbmst08 + bcbmst09 + bcbmst10 + bcbmst11 + bcbsst12 +
           bcbsst13 + bcbsst14 + bcbsst15 + bcbsst16 + bcbsst17 + bcbgsh18 + 
           bcbgsh19) %>%
  filter(!is.na(shortage)) %>%
  mutate(shortage.cat = ifelse(shortage <= 39, "Not Affected", 
                               ifelse(shortage <= 57, "Affected", "Affected A Lot"))) %>%
  mutate(year = 2003)

ishort03.stat8th <- left_join(ibcg03.short, ibschool03, by = "idschool") %>%
  select(idschool, shortage.cat, over.avg, mat.avg, sci.avg, year)
ishort03.stat8th$shortage.cat <- factor(ishort03.stat8th$shortage.cat,
                                        levels = c("Not Affected",
                                                   "Affected", 
                                                   "Affected A Lot"))

ishort03.stat8th.avg <- ishort03.stat8th %>%
  group_by(shortage.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

# 2003 - 2015

ishort.stat8th <- rbind(ishort03.stat8th, ishort07.stat8th, ishort11.stat8th, ishort15.stat8th)
ishort.stat8th.avg <- rbind(ishort03.stat8th.avg, ishort07.stat8th.avg, ishort11.stat8th.avg, ishort15.stat8th.avg)

ishort.stat8th.avg %>%
  hchart(hcaes(x = as.factor(year), y = over.avg, group = shortage.cat), 
         type = "column") %>%
  hc_title(text = "Students Attending Schools Not Affected By Resource Shortage Achieve Higher Scores",
           align = 'center', 
           style = list(fontFamily = 'B-Nazanin', 
                        fontWeight = "italic", fontSize = "35px")) %>%
  hc_xAxis(lineColor = 'blue', tickColor = "red", tickLength = 30, 
           labels = list(style = list(fontSize="20px")), 
           categories = c(2003, 2007, 2011, 2015)) %>%
  hc_yAxis(title = list(text = "Average Score", 
                        style = list(fontSize="20px"))) %>% 
  hc_add_theme(hc_theme_google())

hcboxplot(x = ishort.stat8th$over.avg,
          var = as.factor(ishort.stat8th$year), 
          var2 = as.factor(ishort.stat8th$shortage.cat), outliers = F) %>%
  hc_title(text = "Instruction is Affected By Resource Shortages", 
           style = list(fontFamily = 'B-Nazanin', 
                        fontWeight = "italic", fontSize = "35px")) %>%
  hc_add_theme(hc_theme_google())

rm(ibcg03.short, ibcg07.short, ibcg11.short, ibcg15.short,  ishort.stat8th, ishort.stat8th.avg, ishort03.stat8th, ishort03.stat8th.avg, ishort07.stat8th, ishort07.stat8th.avg, ishort11.stat8th, ishort11.stat8th.avg, ishort15.stat8th,  ishort15.stat8th.avg)


## @knitr principal
## PRINCIPAL'S POV

# 8TH GRADE

# 2015
ibcg15.char <- ibcg15 %>%
  mutate(character = bcbg14a + bcbg14b + bcbg14c + bcbg14d + bcbg14e + 
           bcbg14f + bcbg14h + bcbg14i + bcbg14j + bcbg14k + bcbg14l + 
           bcbg14m) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 20, "Very High", 
                                ifelse(character <= 27, "High", 
                                       ifelse(character <= 34, "Medium", 
                                              ifelse(character <= 42, "Low", 
                                                     "Very Low"))))) %>%
  mutate(year = 2015)

ichar15.stat8th <- left_join(ibcg15.char, ibschool15, by = "idschool") %>%
  select(idschool, character.cat, over.avg, mat.avg, sci.avg, year)
ichar15.stat8th$character.cat <- factor(ichar15.stat8th$character.cat,
                                        levels = c("Very High",
                                                   "High", 
                                                   "Medium", 
                                                   "Low", "Very Low"))

ichar15.stat8th.avg <- ichar15.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))

####################################################################################

y <- c("Teachers' Understanding of<br>The School's Curricular Goals",
       "Teachers' Degree of Success in<br> Implementing The School's Curriculum",
       "Techers' Expectations For<br> Student Achievement",
       "Teacher Working Together to<br> Improve Student Achievement")
Q1 <- ibcg15 %>% group_by(bcbg14a) %>% summarise(count = n())
Q2 <- ibcg15 %>% group_by(bcbg14b) %>% summarise(count = n())
Q3 <- ibcg15 %>% group_by(bcbg14c) %>% summarise(count = n())
Q4 <- ibcg15 %>% group_by(bcbg14d) %>% summarise(count = n())

x1 <- c((Q1 %>% filter(bcbg14a == 1))$count, 
        (Q2 %>% filter(bcbg14b == 1))$count, 
        (Q3 %>% filter(bcbg14c == 1))$count, 
        (Q4 %>% filter(bcbg14d == 1))$count)
x2 <- c((Q1 %>% filter(bcbg14a == 2))$count, 
        (Q2 %>% filter(bcbg14b == 2))$count, 
        (Q3 %>% filter(bcbg14c == 2))$count, 
        (Q4 %>% filter(bcbg14d == 2))$count)
x3 <- c((Q1 %>% filter(bcbg14a == 3))$count, 
        (Q2 %>% filter(bcbg14b == 3))$count, 
        (Q3 %>% filter(bcbg14c == 3))$count, 
        (Q4 %>% filter(bcbg14d == 3))$count)
x4 <- c((Q1 %>% filter(bcbg14a == 4))$count, 
        (Q2 %>% filter(bcbg14b == 4))$count, 
        (Q3 %>% filter(bcbg14c == 4))$count, 
        (Q4 %>% filter(bcbg14d == 4))$count)
x5 <- c((Q1 %>% filter(bcbg14a == 5))$count, 
        (Q2 %>% filter(bcbg14b == 5))$count, 
        (Q3 %>% filter(bcbg14c == 5))$count, 
        (Q4 %>% filter(bcbg14d == 5))$count)

data <- data.frame(y, x1, x2, x3, x4, x5)

top_labels <- c('Very<br>High', 'High', 'Medium', 'Low', 'Very<br>Low')

p <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
             marker = list(color = 'rgba(38, 24, 74, 0.8)',
                           line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
  add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
  add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
  add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
  add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
         margin = list(l = 120, r = 10, t = 140, b = 80),
         showlegend = FALSE) %>%
  add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right') %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 / 2, y = y,
                  text = paste(data[,"x1"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 / 2, y = y,
                  text = paste(data[,"x2"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 / 2, y = y,
                  text = paste(data[,"x3"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 / 2, y = y,
                  text = paste(data[,"x4"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                  text = paste(data[,"x5"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'paper',
                  x = c(x1[1] / 2, x1[1] + x2[1] / 2, x1[1] + x2[1] + x3[1] / 2,
                        x1[1] + x2[1] + x3[1] + x4[1] / 2,
                        x1[1] + x1[1] + x3[1] + x4[1] + x5[1] / 2),
                  y = 1.15,
                  text = top_labels,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE)

div(p, align = 'center')

rm(x1, x2, x3, x4, x5, y, p)

####################################################################################
# 2011
ibcg11.char <- ibcg11 %>%
  mutate(character = bcbg11a + bcbg11b + bcbg11c + bcbg11d + bcbg11e + 
           bcbg11f + bcbg11h) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 11, "Very High", 
                                ifelse(character <= 15, "High",
                                       ifelse(character <= 18, "Medium", 
                                              ifelse(character <= 21, "Low", 
                                                     "Very Low"))))) %>%
  mutate(year = 2011)

ichar11.stat8th <- left_join(ibcg11.char, ibschool11, by = "idschool") %>%
  select(idschool, character.cat, over.avg, mat.avg, sci.avg, year)
ichar11.stat8th$character.cat <- factor(ichar11.stat8th$character.cat,
                                        levels = c("Very High",
                                                   "High", 
                                                   "Medium", "Low", 
                                                   "Very Low"))

ichar11.stat8th.avg <- ichar11.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))


# 2007
ibcg07.char <- ibcg07 %>%
  mutate(character = bc4gchts + bc4gchtu + bc4gchtc + bc4gches + bc4gchps + 
           bc4gchpi + bc4gchsr + bc4gchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 14, "Very High", 
                                ifelse(character <= 18, "High",
                                       ifelse(character <= 22, "Medium", 
                                              ifelse(character <= 26, "Low", 
                                                     "Very Low"))))) %>%
  mutate(year = 2007)

ichar07.stat8th <- left_join(ibcg07.char, ibschool07, by = "idschool") %>%
  select(idschool, character.cat, over.avg, mat.avg, sci.avg, year)
ichar07.stat8th$character.cat <- factor(ichar07.stat8th$character.cat,
                                        levels = c("Very High",
                                                   "High", 
                                                   "Medium", "Low", 
                                                   "Very Low"))

ichar07.stat8th.avg <- ichar07.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))


# 2003
ibcg03.char <- ibcg03 %>%
  mutate(character = bcbgchts + bcbgchtu + bcbgchtc + bcbgches + bcbgchps + 
           bcbgchpi + bcbgchsr + bcbgchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 14, "Very High", 
                                ifelse(character <= 18, "High",
                                       ifelse(character <= 22, "Medium", 
                                              ifelse(character <= 26, "Low", 
                                                     "Very Low"))))) %>%
  mutate(year = 2003)

ichar03.stat8th <- left_join(ibcg03.char, ibschool03, by = "idschool") %>%
  select(idschool, character.cat, over.avg, mat.avg, sci.avg, year)
ichar03.stat8th$character.cat <- factor(ichar03.stat8th$character.cat,
                                        levels = c("Very High",
                                                   "High", 
                                                   "Medium", "Low", 
                                                   "Very Low"))

ichar03.stat8th.avg <- ichar03.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(over.avg = mean(over.avg), mat.avg = mean(mat.avg), sci.avg = mean(sci.avg))


# 2003 - 2015

ichar.stat8th <- rbind(ichar03.stat8th, ichar07.stat8th, ichar11.stat8th, ichar15.stat8th)
ichar.stat8th.avg <- rbind(ichar03.stat8th.avg, ichar07.stat8th.avg, 
                           ichar11.stat8th.avg, ichar15.stat8th.avg)

ggthemr('lilac')
ggplot(ichar.stat8th) + 
  geom_boxplot(aes(x = as.factor(year), y = over.avg, fill = character.cat), outlier.alpha = 0) + 
  labs(x = "Year", y = "Average Overall Score", 
       title = "Students' Performance Based On \nThe Principal's Characterization of The School") +
  theme_minimal() + 
  scale_x_discrete() + 
  scale_y_continuous() + 
  guides(fill=guide_legend(title = "Characterization"))

rm(ibcg03.char, ibcg07.char, ibcg11.char, ibcg15.char, ichar.stat8th, ichar.stat8th.avg, ichar03.stat8th, ichar07.stat8th, ichar11.stat8th, ichar15.stat8th, ichar03.stat8th.avg, ichar07.stat8th.avg, ichar11.stat8th.avg, ichar15.stat8th.avg)

## @knitr teacher
## TEACHER'S POV

#8TH GRADE

## MATH 

# 2015

ibtm15.char <- ibtm15 %>%
  mutate(character = btbg06a + btbg06b + btbg06c + btbg06d + btbg06e + btbg06f + 
           btbg06g + btbg06h + btbg06i + btbg06j + btbg06k + btbg06l + btbg06m + 
           btbg06n + btbg06o + btbg06p + btbg06q) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 30, "Very High", 
                                ifelse(character <= 44, "High", 
                                       ifelse(character <= 58, "Medium", 
                                              ifelse(character <= 72, "Low", "Very Low"))))) %>%
  mutate(year = 2015)

imchar15.stat8th <- left_join(ibtm15.char, ibteacher15, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, mat.avg, year)
imchar15.stat8th$character.cat <- factor(imchar15.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

imchar15.stat8th.avg <- imchar15.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(mat.avg = mean(mat.avg))

# 2011

ibtm11.char <- ibtm11 %>%
  mutate(character = btbg06a + btbg06b + btbg06c + btbg06d + btbg06e + btbg06f + 
           btbg06g + btbg06h) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 13, "Very High", 
                                ifelse(character <= 19, "High", 
                                       ifelse(character <= 25, "Medium", 
                                              ifelse(character <= 30, "Low", "Very Low"))))) %>%
  mutate(year = 2011)

imchar11.stat8th <- left_join(ibtm11.char, ibteacher11, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, mat.avg, year)
imchar11.stat8th$character.cat <- factor(imchar11.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

imchar11.stat8th.avg <- imchar11.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(mat.avg = mean(mat.avg))

# 2007

ibtm07.char <- ibtm07 %>%
  mutate(character = bt4gchts + bt4gchtu + bt4gchtc + bt4gches + bt4gchps + bt4gchpi + 
           bt4gchsr + bt4gchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 14, "Very High", 
                                ifelse(character <= 21, "High", 
                                       ifelse(character <= 27, "Medium", 
                                              ifelse(character <= 33, "Low", "Very Low"))))) %>%
  mutate(year = 2007)

imchar07.stat8th <- left_join(ibtm07.char, ibteacher07, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, mat.avg, year)
imchar07.stat8th$character.cat <- factor(imchar07.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

imchar07.stat8th.avg <- imchar07.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(mat.avg = mean(mat.avg))


# 2003

ibtm03.char <- ibtm03 %>%
  mutate(character = btbgchts + btbgchtu + btbgchtc + btbgches + btbgchps + btbgchpi + 
           btbgchsr + btbgchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 14, "Very High", 
                                ifelse(character <= 20, "High", 
                                       ifelse(character <= 26, "Medium", 
                                              ifelse(character <= 31, "Low", "Very Low"))))) %>%
  mutate(year = 2003)

imchar03.stat8th <- left_join(ibtm03.char, ibteacher03, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, mat.avg, year)
imchar03.stat8th$character.cat <- factor(imchar03.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

imchar03.stat8th.avg <- imchar03.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(mat.avg = mean(mat.avg))

##

# 2003 - 2015

imchar.stat8th <- rbind(imchar03.stat8th, imchar07.stat8th, imchar11.stat8th, imchar15.stat8th)
imchar.stat8th.avg <- rbind(imchar03.stat8th.avg, imchar07.stat8th.avg, 
                            imchar11.stat8th.avg, imchar15.stat8th.avg)
ggthemr('fresh')
imchar.stat8th.avg %>%
  hchart(hcaes(x = as.factor(year), y = mat.avg, group = character.cat), 
         type = "column") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Average Mathematics Score")) %>%
  hc_title(text = "Students' Performance in Mathematics <br> Based on Their Mathematics' Teachers' View on Their School", 
           style = list(fontSize = "20px"),
           align = 'left', 
           useHTML = T) %>%
  hc_add_theme(hc_theme_538())

rm(ibtm03.char, ibtm07.char, ibtm11.char, ibtm15.char, 
   imchar.stat8th, imchar03.stat8th, imchar07.stat8th, imchar11.stat8th, 
   imchar15.stat8th, imchar.stat8th.avg, imchar03.stat8th.avg, imchar07.stat8th.avg, imchar11.stat8th.avg, imchar15.stat8th.avg)

## @knit science
## SCI

# 2015

ibts15.char <- ibts15 %>%
  mutate(character = btbg06a + btbg06b + btbg06c + btbg06d + btbg06e + btbg06f + 
           btbg06g + btbg06h + btbg06i + btbg06j + btbg06k + btbg06l + btbg06m + 
           btbg06n + btbg06o + btbg06p + btbg06q) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 28, "Very High", 
                                ifelse(character <= 38, "High", 
                                       ifelse(character <= 48, "Medium", 
                                              ifelse(character <= 59, "Low", "Very Low"))))) %>%
  mutate(year = 2015)

ischar15.stat8th <- left_join(ibts15.char, ibteacher15, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, sci.avg, year)
ischar15.stat8th$character.cat <- factor(ischar15.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

ischar15.stat8th.avg <- ischar15.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(sci.avg = mean(sci.avg))

# 2011

ibts11.char <- ibts11 %>%
  mutate(character = btbg06a + btbg06b + btbg06c + btbg06d + btbg06e + btbg06f + 
           btbg06g + btbg06h) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 16, "Very High", 
                                ifelse(character <= 20, "High", 
                                       ifelse(character <= 25, "Medium", 
                                              ifelse(character <= 30, "Low", "Very Low"))))) %>%
  mutate(year = 2011)

ischar11.stat8th <- left_join(ibts11.char, ibteacher11, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, sci.avg, year)
ischar11.stat8th$character.cat <- factor(ischar11.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

ischar11.stat8th.avg <- ischar11.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(sci.avg = mean(sci.avg))

# 2007

ibts07.char <- ibts07 %>%
  mutate(character = bt4gchts + bt4gchtu + bt4gchtc + bt4gches + bt4gchps + bt4gchpi + 
           bt4gchsr + bt4gchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 13, "Very High", 
                                ifelse(character <= 19, "High", 
                                       ifelse(character <= 25, "Medium", 
                                              ifelse(character <= 30, "Low", "Very Low"))))) %>%
  mutate(year = 2007)

ischar07.stat8th <- left_join(ibts07.char, ibteacher07, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, sci.avg, year)
ischar07.stat8th$character.cat <- factor(ischar07.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

ischar07.stat8th.avg <- ischar07.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(sci.avg = mean(sci.avg))


# 2003

ibts03.char <- ibts03 %>%
  mutate(character = btbgchts + btbgchtu + btbgchtc + btbgches + btbgchps + btbgchpi + 
           btbgchsr + btbgchsd) %>%
  filter(!is.na(character)) %>%
  mutate(character.cat = ifelse(character <= 14, "Very High", 
                                ifelse(character <= 20, "High", 
                                       ifelse(character <= 25, "Medium", 
                                              ifelse(character <= 30, "Low", "Very Low"))))) %>%
  mutate(year = 2003)

ischar03.stat8th <- left_join(ibts03.char, ibteacher03, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, character.cat, sci.avg, year)
ischar03.stat8th$character.cat <- factor(ischar03.stat8th$character.cat,
                                         levels = c("Very High",
                                                    "High", 
                                                    "Medium", "Low", 
                                                    "Very Low"))

ischar03.stat8th.avg <- ischar03.stat8th %>%
  group_by(character.cat, year) %>%
  summarise(sci.avg = mean(sci.avg))

##

# 2003 - 2015

ischar.stat8th <- rbind(ischar03.stat8th, ischar07.stat8th, ischar11.stat8th, ischar15.stat8th)
ischar.stat8th.avg <- rbind(ischar03.stat8th.avg, ischar07.stat8th.avg, 
                            ischar11.stat8th.avg, ischar15.stat8th.avg)

ggplot(ischar.stat8th) + 
  geom_boxplot(aes(x = as.factor(year), y = sci.avg, fill = character.cat), outlier.alpha = 0) + 
  theme_minimal() + 
  labs(x = "Year", y = "Average Science Score", 
       title = "Students' Performance in Mathematics \n Based on Their Science Teachers' View on Their School")+
  guides(fill=guide_legend(title = "View")) + 
  scale_x_discrete() + 
  scale_y_continuous()

rm(ibts03.char, ibts07.char, ibts11.char, ibts15.char, 
   ischar.stat8th, ischar03.stat8th, ischar15.stat8th, ischar11.stat8th, ischar03.stat8th.avg, ischar07.stat8th.avg, ischar11.stat8th.avg, ischar15.stat8th.avg)

## @knitr bullying
### BULLYING - STUDENT'S POV

#8TH GRADE

# 2015

ibsg15.bully <- ibsg15 %>%
  mutate(bullying = bsbg16a + bsbg16b + bsbg16c + bsbg16d + bsbg16e + 
           bsbg16f + bsbg16g + bsbg16h) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying <= 16, "Frequently", 
                               ifelse(bullying <= 24, "Sometimes", 
                                      "Never"))) %>%
  mutate(year = 2015)

ibully15.stat8th <- left_join(ibsg15.bully, ibscores15, 
                              by = c("idschool", "idstud")) %>%
  select(idschool, bullying.cat, overall, year)
ibully15.stat8th$bullying.cat <- factor(ibully15.stat8th$bullying.cat,
                                        levels = c("Frequently",
                                                   "Sometimes", 
                                                   "Never"))

ibully15.stat8th.avg <- ibully15.stat8th %>%
  group_by(bullying.cat, year) %>%
  summarise(over.avg = mean(overall))


# 2011

ibsg11.bully <- ibsg11 %>%
  mutate(bullying = bsbg13a + bsbg13b + bsbg13c + bsbg13d + bsbg13e + bsbg13f) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying <= 12, "Frequently", 
                               ifelse(bullying <= 18, "Sometimes", 
                                      "Never"))) %>%
  mutate(year = 2011)

ibully11.stat8th <- left_join(ibsg11.bully, ibscores11, 
                              by = c("idschool", "idstud")) %>%
  select(idschool, bullying.cat, overall, year)
ibully11.stat8th$bullying.cat <- factor(ibully11.stat8th$bullying.cat,
                                        levels = c("Frequently",
                                                   "Sometimes", 
                                                   "Never"))

ibully11.stat8th.avg <- ibully11.stat8th %>%
  group_by(bullying.cat, year) %>%
  summarise(over.avg = mean(overall))

# 2007

ibsg07.bully <- ibsg07 %>%
  mutate(bullying = bs4gstol + bs4ghurt + bs4gmade + bs4gmfun + bs4gleft) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying == 5, "Frequently", 
                               ifelse(bullying <= 8, "Sometimes", "Never"))) %>%
  mutate(year = 2007)

ibully07.stat8th <- left_join(ibsg07.bully, ibscores07, 
                              by = c("idschool", "idstud")) %>%
  select(idschool, bullying.cat, overall, year)
ibully07.stat8th$bullying.cat <- factor(ibully07.stat8th$bullying.cat,
                                        levels = c("Frequently",
                                                   "Sometimes", 
                                                   "Never"))

ibully07.stat8th.avg <- ibully07.stat8th %>%
  group_by(bullying.cat, year) %>%
  summarise(over.avg = mean(overall))


# 2003

ibsg03.bully <- ibsg03 %>%
  mutate(bullying = bsbgstol + bsbghurt + bsbgmade + bsbgmfun + bsbgleft) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying == 5, "Frequently", 
                               ifelse(bullying <= 8, "Sometimes", "Never"))) %>%
  mutate(year = 2003)

ibully03.stat8th <- left_join(ibsg03.bully, ibscores03, 
                              by = c("idschool", "idstud")) %>%
  select(idschool, bullying.cat, overall, year)
ibully03.stat8th$bullying.cat <- factor(ibully03.stat8th$bullying.cat,
                                        levels = c("Frequently",
                                                   "Sometimes", 
                                                   "Never"))

ibully03.stat8th.avg <- ibully03.stat8th %>%
  group_by(bullying.cat, year) %>%
  summarise(over.avg = mean(overall))

# 2003 - 2015

ibully.stat8th <- rbind(ibully03.stat8th, ibully07.stat8th, ibully11.stat8th, ibully15.stat8th)
ibully.stat8th.avg <- rbind(ibully03.stat8th.avg, ibully07.stat8th.avg, 
                            ibully11.stat8th.avg, ibully15.stat8th.avg)

ibully.stat8th.avg %>%
  hchart(hcaes(x = as.factor(year), y = over.avg %>% round(2), group = bullying.cat), 
         type = "column") %>%
  hc_title(text = "Bullying Affects Performance",
           align = 'center', 
           style = list(fontSize = "20px")) %>%
  hc_xAxis(tickLength = 20, 
           title = list(text = "Year"), lineColor = 'white', 
           labels = list(style = list(color = 'white')), 
           tickColor = 'white', tickWidth = 1, tickmarkPlacement = "between") %>%
  hc_yAxis(title = list(text = "Average Overall Score"), lineColor = 'white', 
           lineWidth = 1, 
           labels = list(style = list(color = 'white')),
           tickColor = 'white', tickWidth = 1) %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_legend(align = 'center')

rm(ibsg03.bully, ibsg07.bully, ibsg11.bully, ibsg15.bully)
rm(ibully.stat8th, ibully.stat8th.avg)
rm(ibully03.stat8th, ibully03.stat8th.avg, ibully07.stat8th, ibully07.stat8th.avg, 
   ibully11.stat8th, ibully11.stat8th.avg, ibully15.stat8th, ibully15.stat8th.avg)

## @knitr safe

## SAFE AND ORDERLY SCHOOLS - TEACHER'S POV

#8TH GRADE
# MATH


# 2015

ibtm15.safe <- ibtm15 %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + btbg07e + btbg07f + 
           btbg07g + btbg07h) %>%
  filter(!is.na(safety)) %>%
  select(idschool, idteach, safety)

ibts15.safe <- ibts15 %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + btbg07e + btbg07f + 
           btbg07g + btbg07h ) %>%
  filter(!is.na(safety)) %>%
  select(idschool, idteach, safety)



issafe15.stat8th <- left_join(ibts15.safe, ibteacher15, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = sci.avg)
imsafe15.stat8th <- left_join(ibtm15.safe, ibteacher15, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = mat.avg)
isafe15.stat8th <- rbind(imsafe15.stat8th, issafe15.stat8th) %>%
  mutate(safety.cat = ifelse(safety <= 15, "Very Safe and Orderly", 
                             ifelse(safety <= 21, "Safe and Orderly", 
                                    "Less Than Safe and Orderly"))) %>%
  mutate(year = 2015)

isafe15.stat8th$safety.cat <- factor(isafe15.stat8th$safety.cat,
                                     levels = c("Very Safe and Orderly",
                                                "Safe and Orderly", 
                                                "Less Than Safe and Orderly"))

isafe15.stat8th.avg <- isafe15.stat8th %>%
  group_by(safety.cat, year) %>%
  summarise(avg = mean(avg))


# 2011


ibtm11.safe <- ibtm11 %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + btbg07e) %>%
  filter(!is.na(safety))

ibts11.safe <- ibts11 %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + btbg07e) %>%
  filter(!is.na(safety))

imsafe11.stat8th <- left_join(ibtm11.safe, ibteacher11, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = mat.avg)
issafe11.stat8th <- left_join(ibts11.safe, ibteacher11, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = sci.avg)
isafe11.stat8th <- rbind(imsafe11.stat8th, issafe11.stat8th) %>%
  mutate(safety.cat = ifelse(safety <= 9, "Very Safe and Orderly", 
                             ifelse(safety <= 13, "Safe and Orderly", 
                                    "Less Than Safe and Orderly"))) %>%
  mutate(year = 2011)

isafe11.stat8th$safety.cat <- factor(isafe11.stat8th$safety.cat,
                                     levels = c("Very Safe and Orderly",
                                                "Safe and Orderly", 
                                                "Less Than Safe and Orderly"))

isafe11.stat8th.avg <- isafe11.stat8th %>%
  group_by(safety.cat, year) %>%
  summarise(avg = mean(avg))

# 2007


ibtm07.safe <- ibtm07 %>%
  mutate(safety = bt4gcusn + bt4gcusa + bt4gcuas) %>%
  filter(!is.na(safety)) 
ibts07.safe <- ibts07 %>%
  mutate(safety = bt4gcusn + bt4gcusa + bt4gcuas) %>%
  filter(!is.na(safety)) 

imsafe07.stat8th <- left_join(ibtm07.safe, ibteacher07, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = mat.avg)
issafe07.stat8th <- left_join(ibts07.safe, ibteacher07, by = c("idschool", "idteach")) %>%
  select(idschool, idteach, safety, avg = sci.avg)
isafe07.stat8th <- rbind(imsafe07.stat8th, issafe07.stat8th) %>%
  mutate(safety.cat = ifelse(safety <= 6, "Very Safe and Orderly", 
                             ifelse(safety <= 9, "Safe and Orderly", 
                                    "Less Than Safe and Orderly"))) %>%
  mutate(year = 2007)

isafe07.stat8th$safety.cat <- factor(isafe07.stat8th$safety.cat,
                                     levels = c("Very Safe and Orderly",
                                                "Safe and Orderly", 
                                                "Less Than Safe and Orderly"))

isafe07.stat8th.avg <- isafe07.stat8th %>%
  group_by(safety.cat, year) %>%
  summarise(avg = mean(avg))


# 2003 - 2015

isafe.stat8th <- rbind(isafe07.stat8th, isafe11.stat8th, isafe15.stat8th)
isafe.stat8th.avg <- rbind(isafe07.stat8th.avg, 
                           isafe11.stat8th.avg, isafe15.stat8th.avg)


hcboxplot(x = round(isafe.stat8th$avg, 2),
          var = as.factor(isafe.stat8th$year), 
          var2 = as.factor(isafe.stat8th$safety.cat), outliers = F) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Average Student Performance") %>%
  hc_subtitle(text = "Categorized Based on School Safety From Teachers' POV") %>%
  hc_legend(align = 'center') %>%
  hc_xAxis(lineWidth = 1, tickWidth = 1, tickLength = 30, lineColor = 'white', 
           tickColor = 'white') %>%
  hc_yAxis(lineWidth = 1)

rm(ibtm15.safe, ibtm11.safe, ibtm07.safe, ibts11.safe, ibts15.safe, ibts07.safe, imsafe07.stat8th, imsafe11.stat8th, imsafe15.stat8th, issafe07.stat8th, issafe11.stat8th, issafe15.stat8th, isafe07.stat8th, isafe11.stat8th, isafe15.stat8th, isafe07.stat8th.avg, isafe11.stat8th.avg, isafe15.stat8th.avg)


## @knitr principal2
### DISCIPLINE - PRINCIPAL'S POV

# 8TH GRADE

#2015

ibcg15.disc <- ibcg15 %>%
  mutate(problem = bcbg15a + bcbg15b + bcbg15c + bcbg15d + bcbg15e + 
           bcbg15f + bcbg15g + bcbg15h + bcbg15i + bcbg15j + bcbg15k, 
         problem.cat = ifelse(problem <= 22, "Hardly Any Problems", 
                              ifelse(problem <= 33, "Minor Problems", "Moderate to Severe Problems"))) %>%
  select(idschool, problem.cat, problem) %>%
  filter(!is.na(problem.cat)) %>%
  mutate(year = 2015)

idisc15.stat8th <- left_join(ibcg15.disc, ibschool15, by = "idschool") %>%
  select(idschool, problem.cat, over.avg, year)
idisc15.stat8th$problem.cat <- factor(idisc15.stat8th$problem.cat,
                                      levels = c("Hardly Any Problems",
                                                 "Minor Problems", 
                                                 "Moderate to Severe Problems"))

idisc15.stat8th.avg <- idisc15.stat8th %>%
  group_by(problem.cat, year) %>%
  summarise(over.avg = mean(over.avg))

#2011

ibcg11.disc <- ibcg11 %>%
  mutate(problem = bcbg12aa + bcbg12ab + bcbg12ac + bcbg12ad + bcbg12ae + 
           bcbg12af + bcbg12ag + bcbg12ah + bcbg12ai + bcbg12aj + bcbg12ak, 
         problem.cat = ifelse(problem <= 18, "Hardly Any Problems", 
                              ifelse(problem <= 26, "Minor Problems", "Moderate to Severe Problems"))) %>%
  select(idschool, problem.cat, problem) %>%
  filter(!is.na(problem.cat)) %>%
  mutate(year = 2011)

idisc11.stat8th <- left_join(ibcg11.disc, ibschool11, by = "idschool") %>%
  select(idschool, problem.cat, over.avg, year)
idisc11.stat8th$problem.cat <- factor(idisc11.stat8th$problem.cat,
                                      levels = c("Hardly Any Problems",
                                                 "Minor Problems", 
                                                 "Moderate to Severe Problems"))

idisc11.stat8th.avg <- idisc11.stat8th %>%
  group_by(problem.cat, year) %>%
  summarise(over.avg = mean(over.avg))

#2007

ibcg07.disc <- ibcg07 %>%
  mutate(problem = bc4gsp01 + bc4gsp02 + bc4gsp03 + bc4gsp04 + bc4gsp05 + 
           bc4gsp06 + bc4gsp07 + bc4gsp08 + bc4gsp09 + bc4gsp10 + bc4gsp11 + 
           bc4gsp12 + bc4gsp13, 
         problem.cat = ifelse(problem <= 21, "Hardly Any Problems", 
                              ifelse(problem <= 29, "Minor Problems", "Moderate to Severe Problems"))) %>%
  select(idschool, problem.cat, problem) %>%
  filter(!is.na(problem.cat)) %>%
  mutate(year = 2007)

idisc07.stat8th <- left_join(ibcg07.disc, ibschool07, by = "idschool") %>%
  select(idschool, problem.cat, over.avg, year)
idisc07.stat8th$problem.cat <- factor(idisc07.stat8th$problem.cat,
                                      levels = c("Hardly Any Problems",
                                                 "Minor Problems", 
                                                 "Moderate to Severe Problems"))

idisc07.stat8th.avg <- idisc07.stat8th %>%
  group_by(problem.cat, year) %>%
  summarise(over.avg = mean(over.avg))

#2003

ibcg03.disc <- ibcg03 %>%
  mutate(problem = bcbgsp01 + bcbgsp02 + bcbgsp03 + bcbgsp04 + bcbgsp05 + 
           bcbgsp06 + bcbgsp03 + bcbgsp08 + bcbgsp09 + bcbgsp10 + bcbgsp11 + 
           bcbgsp12 + bcbgsp13, 
         problem.cat = ifelse(problem <= 22, "Hardly Any Problems", 
                              ifelse(problem <= 30, "Minor Problems", "Moderate to Severe Problems"))) %>%
  select(idschool, problem.cat, problem) %>%
  filter(!is.na(problem.cat)) %>%
  mutate(year = 2003)

idisc03.stat8th <- left_join(ibcg03.disc, ibschool03, by = "idschool") %>%
  select(idschool, problem.cat, over.avg, year)
idisc03.stat8th$problem.cat <- factor(idisc03.stat8th$problem.cat,
                                      levels = c("Hardly Any Problems",
                                                 "Minor Problems", 
                                                 "Moderate to Severe Problems"))

idisc03.stat8th.avg <- idisc03.stat8th %>%
  group_by(problem.cat, year) %>%
  summarise(over.avg = mean(over.avg))

## 2003 - 2015
idisc.stat8th <- rbind(idisc03.stat8th, idisc07.stat8th, idisc11.stat8th, idisc15.stat8th)
idisc.stat8th.avg <- rbind(idisc03.stat8th.avg, idisc07.stat8th.avg, 
                           idisc11.stat8th.avg, idisc15.stat8th.avg)


idisc.stat8th.avg %>%
  hchart(hcaes(x = as.factor(year), y = over.avg, group = problem.cat), 
         type = "column") %>%
  hc_title(text = "Discipline in Schools",
           align = 'center', 
           style = list(fontSize = "20px")) %>%
  hc_xAxis(tickLength = 20, 
           title = list(text = "Year"), lineColor = 'white', 
           labels = list(style = list(color = 'white')), 
           tickColor = 'white', tickWidth = 1, tickmarkPlacement = "between") %>%
  hc_yAxis(title = list(text = "Average Overall Score"), lineColor = 'white', 
           lineWidth = 1, 
           labels = list(style = list(color = 'white')),
           tickColor = 'white', tickWidth = 1) %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_legend(align = 'center')

rm(ibcg03.disc, ibcg07.disc, ibcg11.disc, ibcg15.disc, 
   idisc.stat8th, idisc.stat8th.avg, idisc03.stat8th, idisc07.stat8th, idisc11.stat8th, idisc15.stat8th, idisc03.stat8th.avg, idisc07.stat8th.avg, idisc11.stat8th.avg, idisc15.stat8th.avg)


## @knitr likemath
# LIKE LEARNING MATH

#8TH GRADE

# 2015

ibsg15.like <- ibsg15 %>%
  mutate(like = bsbm17a + bsbm17b + bsbm17c + bsbm17d + bsbm17e + bsbm17f + 
           bsbm17g + bsbm17h + bsbm17i) %>%
  mutate(like.cat = ifelse(like <= 18, "Very Much Like Learning Math", 
                           ifelse(like <= 27, "Nuetral Towards Learning Math", 
                                  "Dislike Learning Math"))) %>%
  filter(!is.na(like)) %>%
  mutate(year = 2015)


ilike15.stat8th <- left_join(ibsg15.like, ibscores15, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, like.cat, mat, year)
ilike15.stat8th$like.cat <- factor(ilike15.stat8th$like.cat,
                                   levels = c("Very Much Like Learning Math",
                                              "Nuetral Towards Learning Math", 
                                              "Dislike Learning Math"))

ilike15.stat8th.avg <- ilike15.stat8th %>%
  group_by(like.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2011

ibsg11.like <- ibsg11 %>%
  mutate(like = bsbm14a + bsbm14b + bsbm14c + bsbm14d + bsbm14e + bsbm14f) %>%
  mutate(like.cat = ifelse(like <= 12, "Very Much Like Learning Math", 
                           ifelse(like <= 18, "Nuetral Towards Learning Math", 
                                  "Dislike Learning Math"))) %>%
  filter(!is.na(like)) %>%
  mutate(year = 2011)


ilike11.stat8th <- left_join(ibsg11.like, ibscores11, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, like.cat, mat, year)
ilike11.stat8th$like.cat <- factor(ilike11.stat8th$like.cat,
                                   levels = c("Very Much Like Learning Math",
                                              "Nuetral Towards Learning Math", 
                                              "Dislike Learning Math"))

ilike11.stat8th.avg <- ilike11.stat8th %>%
  group_by(like.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2007

ibsg07.like <- ibsg07 %>%
  mutate(like = 5 - bs4mabor + bs4malik + bs4maenj + bs4mamor) %>%
  mutate(like.cat = ifelse(like <= 8, "Very Much Like Learning Math", 
                           ifelse(like <= 12, "Nuetral Towards Learning Math", 
                                  "Dislike Learning Math"))) %>%
  filter(!is.na(like)) %>%
  mutate(year = 2007)


ilike07.stat8th <- left_join(ibsg07.like, ibscores07, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, like.cat, mat, year)
ilike07.stat8th$like.cat <- factor(ilike07.stat8th$like.cat,
                                   levels = c("Very Much Like Learning Math",
                                              "Nuetral Towards Learning Math", 
                                              "Dislike Learning Math"))

ilike07.stat8th.avg <- ilike07.stat8th %>%
  group_by(like.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2003

ibsg03.like <- ibsg03 %>%
  mutate(like = bsbmtenj + bsbmtmor) %>%
  mutate(like.cat = ifelse(like <= 4, "Very Much Like Learning Math", 
                           ifelse(like <= 6, "Nuetral Towards Learning Math", 
                                  "Dislike Learning Math"))) %>%
  filter(!is.na(like)) %>%
  mutate(year = 2003)


ilike03.stat8th <- left_join(ibsg03.like, ibscores03, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, like.cat, mat, year)
ilike03.stat8th$like.cat <- factor(ilike03.stat8th$like.cat,
                                   levels = c("Very Much Like Learning Math",
                                              "Nuetral Towards Learning Math", 
                                              "Dislike Learning Math"))

ilike03.stat8th.avg <- ilike03.stat8th %>%
  group_by(like.cat, year) %>%
  summarise(mat.avg = mean(mat))


## 2003 - 2015

ilike.stat8th <- rbind(ilike03.stat8th, ilike07.stat8th, ilike11.stat8th, ilike15.stat8th)
ilike.stat8th.avg <- rbind(ilike03.stat8th.avg, ilike07.stat8th.avg, 
                           ilike11.stat8th.avg, ilike15.stat8th.avg)

hcboxplot(x = round(ilike.stat8th$mat, 2),
          var = as.factor(ilike.stat8th$year), 
          var2 = as.factor(ilike.stat8th$like.cat), outliers = F) %>%
  hc_add_theme(hc_theme_monokai()) %>%
  hc_title(text = "I like it, So I'm Good at it.",
           align = 'center', 
           style = list(fontSize = "30px")) %>%
  hc_xAxis(tickLength = 30, 
           labels = list(style = list(fontSize="20px")))

ggthemr('grape')
p <- ggplot(ilike.stat8th, aes(x = as.factor(like.cat), y = mat)) + 
  geom_point(aes(col = like.cat, frame = year), position = "jitter", 
             size = 1.5, shape = 8) + 
  theme_minimal() 
(ggplotly(p) %>%
    hide_legend()) %>% div(align = 'left')


## @knitr confidence
## CONFIDENCE 

# 8TH GRADE

# 2015

ibsg15.conf <- ibsg15 %>%
  mutate(conf = bsbm19a + 5 - bsbm19b + 5 - bsbm19c + bsbm19d + 5 - bsbm19e +
           bsbm19f + 
           bsbm19g + 5 - bsbm19h + 5 - bsbm19i) %>%
  filter(!is.na(conf)) %>%
  mutate(conf.cat = ifelse(conf <= 18, "Very Confident", 
                           ifelse(conf <= 27, "Confident", "Not Confident"))) %>%
  mutate(year = 2015)


iconf15.stat8th <- left_join(ibsg15.conf, ibscores15, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, conf.cat, mat, year)
iconf15.stat8th$conf.cat <- factor(iconf15.stat8th$conf.cat,
                                   levels = c("Very Confident",
                                              "Confident", 
                                              "Not Confident"))

iconf15.stat8th.avg <- iconf15.stat8th %>%
  group_by(conf.cat, year) %>%
  summarise(mat.avg = mean(mat))


# 2011

ibsg11.conf <- ibsg11 %>%
  mutate(conf = bsbm16a + 5 - bsbm16b + 5 - bsbm16c + bsbm16d + 5 - bsbm16e +
           bsbm16f + 
           bsbm16g + bsbm16h + 5 - bsbm16i) %>%
  filter(!is.na(conf)) %>%
  mutate(conf.cat = ifelse(conf <= 18, "Very Confident", 
                           ifelse(conf <= 27, "Confident", "Not Confident"))) %>%
  mutate(year = 2011)

iconf11.stat8th <- left_join(ibsg11.conf, ibscores11, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, conf.cat, mat, year)
iconf11.stat8th$conf.cat <- factor(iconf11.stat8th$conf.cat,
                                   levels = c("Very Confident",
                                              "Confident", 
                                              "Not Confident"))

iconf11.stat8th.avg <- iconf11.stat8th %>%
  group_by(conf.cat, year) %>%
  summarise(mat.avg = mean(mat))


# 2007

ibsg07.conf <- ibsg07 %>%
  mutate(conf = bs4mawel + 5 - bs4maclm + 5 - bs4mastr + bs4maqky) %>%
  filter(!is.na(conf)) %>%
  mutate(conf.cat = ifelse(conf <= 8, "Very Confident", 
                           ifelse(conf <= 12, "Confident", "Not Confident"))) %>%
  mutate(year = 2007)

iconf07.stat8th <- left_join(ibsg07.conf, ibscores07, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, conf.cat, mat, year)
iconf07.stat8th$conf.cat <- factor(iconf07.stat8th$conf.cat,
                                   levels = c("Very Confident",
                                              "Confident", 
                                              "Not Confident"))

iconf07.stat8th.avg <- iconf07.stat8th %>%
  group_by(conf.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2003

ibsg03.conf <- ibsg03 %>%
  mutate(conf = bsbmtwel + 5 - bsbmtclm + 5 - bsbmtstr + bsbmtqky) %>%
  filter(!is.na(conf)) %>%
  mutate(conf.cat = ifelse(conf <= 8, "Very Confident", 
                           ifelse(conf <= 12, "Confident", "Not Confident"))) %>%
  mutate(year = 2003)

iconf03.stat8th <- left_join(ibsg03.conf, ibscores03, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, conf.cat, mat, year)
iconf03.stat8th$conf.cat <- factor(iconf03.stat8th$conf.cat,
                                   levels = c("Very Confident",
                                              "Confident", 
                                              "Not Confident"))

iconf03.stat8th.avg <- iconf03.stat8th %>%
  group_by(conf.cat, year) %>%
  summarise(mat.avg = mean(mat))

## 2003 - 2015
iconf.stat8th <- rbind(iconf15.stat8th, iconf11.stat8th,  iconf07.stat8th, iconf03.stat8th)
iconf.stat8th.avg <- rbind(iconf15.stat8th.avg, iconf11.stat8th.avg,
                           iconf07.stat8th.avg, iconf03.stat8th.avg)

(plot_ly(iconf.stat8th, x = ~year, y = ~mat, color = ~conf.cat, type = "box", colors = "Set1") %>%
    layout(boxmode = "group", 
           legend = list(orientation = 'h'), 
           yaxis = list(title = "Score", showline = T), 
           xaxis = list(title = "", showline = T, tickfont = list(size = 20)),
           title = "What's Wrong With Being Confident?")) %>% div(align = 'center')


## @knitr value
## VALUE 

# 8TH GRADE

# 2015


ibsg15.value <- ibsg15 %>%
  mutate(value = bsbm20a + bsbm20b + bsbm20c + bsbm20d + bsbm20e + 
           bsbm20f + bsbm20g + bsbm20h + bsbm20i) %>%
  filter(!is.na(value)) %>%
  mutate(value.cat = ifelse(value <= 18, "Strongly Value", 
                            ifelse(value <= 27, "Value", "Do Not Value"))) %>%
  mutate(year = 2015)

ivalue15.stat8th <- left_join(ibsg15.value, ibscores15, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, value.cat, mat, year)
ivalue15.stat8th$value.cat <- factor(ivalue15.stat8th$value.cat,
                                     levels = c("Strongly Value",
                                                "Value", 
                                                "Do Not Value"))

ivalue15.stat8th.avg <- ivalue15.stat8th %>%
  group_by(value.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2011


ibsg11.value <- ibsg11 %>%
  mutate(value = bsbm16f + bsbm16j + bsbm16k + bsbm16l + bsbm16m + bsbm16n) %>%
  filter(!is.na(value)) %>%
  mutate(value.cat = ifelse(value <= 12, "Strongly Value", 
                            ifelse(value <= 18, "Value", "Do Not Value"))) %>%
  mutate(year = 2011)

ivalue11.stat8th <- left_join(ibsg11.value, ibscores11, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, value.cat, mat, year)
ivalue11.stat8th$value.cat <- factor(ivalue11.stat8th$value.cat,
                                     levels = c("Strongly Value",
                                                "Value", 
                                                "Do Not Value"))

ivalue11.stat8th.avg <- ivalue11.stat8th %>%
  group_by(value.cat, year) %>%
  summarise(mat.avg = mean(mat))


# 2007


ibsg07.value <- ibsg07 %>%
  mutate(value = bs4mahdl + bs4maoss + bs4mauni + bs4maget) %>%
  filter(!is.na(value)) %>%
  mutate(value.cat = ifelse(value <= 8, "Strongly Value", 
                            ifelse(value <= 12, "Value", "Do Not Value"))) %>%
  mutate(year = 2007)

ivalue07.stat8th <- left_join(ibsg07.value, ibscores07, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, value.cat, mat, year)
ivalue07.stat8th$value.cat <- factor(ivalue07.stat8th$value.cat,
                                     levels = c("Strongly Value",
                                                "Value", 
                                                "Do Not Value"))

ivalue07.stat8th.avg <- ivalue07.stat8th %>%
  group_by(value.cat, year) %>%
  summarise(mat.avg = mean(mat))

# 2003


ibsg03.value <- ibsg03 %>%
  mutate(value = bsbmahdl + bsbmaoss + bsbmauni + bsbmajob + bsbmaget) %>%
  filter(!is.na(value)) %>%
  mutate(value.cat = ifelse(value <= 10, "Strongly Value", 
                            ifelse(value <= 15, "Value", "Do Not Value"))) %>%
  mutate(year = 2003)

ivalue03.stat8th <- left_join(ibsg03.value, ibscores03, by = c("idschool", "idstud")) %>%
  select(idschool, idstud, value.cat, mat, year)
ivalue03.stat8th$value.cat <- factor(ivalue03.stat8th$value.cat,
                                     levels = c("Strongly Value",
                                                "Value", 
                                                "Do Not Value"))

ivalue03.stat8th.avg <- ivalue03.stat8th %>%
  group_by(value.cat, year) %>%
  summarise(mat.avg = mean(mat))


## 2003 - 2015
library("viridisLite")
cols
cols <- viridis(3)
cols <- substr(cols, 0, 7)
colors <- c('#E89CE8', 'skyblue', 'lightgray')
ivalue.stat8th <- rbind(ivalue15.stat8th, ivalue11.stat8th, 
                        ivalue07.stat8th, ivalue03.stat8th)
ivalue.stat8th.avg <- rbind(ivalue15.stat8th.avg, ivalue11.stat8th.avg,
                            ivalue07.stat8th.avg, ivalue03.stat8th.avg)

ivalue.stat8th.avg %>%
  hchart(hcaes(x = as.factor(year), y = mat.avg, group = value.cat), 
         type = "column", borderColor = 'gray') %>%
  hc_colors(colors) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Average Mathematics Score")) %>%
  hc_title(text = "Caring is Succeeding", 
           style = list(fontSize = "20px"),
           align = 'left', 
           useHTML = T) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_legend(align = 'center')

ggplot(ivalue.stat8th, aes(x = as.factor(year), y = mat)) + 
  geom_point(aes(col = value.cat), 
             position = "jitter", alpha = 0.75, size = 2, shape = 8) + 
  scale_color_brewer(palette = "BuPu") + 
  labs(title = "Scatter Plot of Students' Mathematics Score",
       subtitle = "Based on How Much They Value Mathematics", 
       y = "Score", 
       x = "Year") + 
  guides(color = guide_legend(title="")) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_y_continuous()

rm(ivalue.stat8th, ivalue.stat8th.avg, ivalue03.stat8th, ivalue03.stat8th.avg, ivalue07.stat8th, ivalue07.stat8th.avg, ivalue11.stat8th, ivalue11.stat8th.avg, ivalue15.stat8th, ivalue15.stat8th.avg)


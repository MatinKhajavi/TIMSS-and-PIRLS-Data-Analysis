
## @knitr dataSection

library(tidyverse)
library(highcharter)
library(plotly)
library(shiny)
library(ggbiplot)
library(kableExtra)
library(dendextend)
library(factoextra)
library(FactoMineR)

bsg15 <- read_rds("../2015/bsg.rds")
library(ISOcodes)
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2, Name) %>%
  mutate(idcntry = as.character(idcntry))

set.seed(19)

student.like <- bsg15 %>% 
  dplyr::mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>%
  dplyr::select(idcntry, bsbm17a:bsbm17i, mat) %>%
  apply(2, function(x) x %>% as.numeric() %>% round(3)) %>% 
  data.frame() %>% 
  dplyr::mutate(idcntry = idcntry %>% as.character()) %>% 
  left_join(codes, by = "idcntry") %>%
  filter(!is.na(country)) %>% 
  dplyr::select(country = Name, 
                `Enjoy Learning Math` = bsbm17a, 
                `Wish Did Not Have to Study Math` = bsbm17b, 
                `Math Is Boring` = bsbm17c, 
                `Learn Interesting Things in Math` = bsbm17d, 
                `Like Math` = bsbm17e, `Like Numbers` = bsbm17f, 
                `Like Math Problems` = bsbm17g, `Look Forward to Math Class` = bsbm17h, 
                `Math is Favorite Subject` = bsbm17i, 
                score = mat) %>% drop_na()
country.like <- student.like %>%
  dplyr::group_by(country) %>% summarise_all(funs(mean))

students.talent <- bsg15 %>% 
  dplyr::mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>%
  dplyr::select(idcntry, bsbm19a:bsbm19i, mat) %>%
  apply(2, function(x) x %>% as.numeric() %>% round(3)) %>% 
  data.frame() %>% 
  dplyr::mutate(idcntry = idcntry %>% as.character()) %>% 
  left_join(codes, by = "idcntry") %>%
  filter(!is.na(country)) %>% 
  dplyr::select(country = Name, 
                `Usually Do Well in Math` = bsbm19a, 
                `Math Is More Difficult For Me` = bsbm19b, 
                `Math Is Not My Strength` = bsbm19c, 
                `Learn Quickly in Math` = bsbm19d, 
                `Math Makes Me Nervous` = bsbm19e, `Good at Working Out Problems` = bsbm19f, 
                `Good at Math` = bsbm19g, `Math Is Harder For Me` = bsbm19h, 
                `Math Makes Me Confused` = bsbm19i, 
                score = mat) %>% drop_na()
country.talent <- students.talent %>%
  dplyr::group_by(country) %>% summarise_all(funs(mean))

student.value <- bsg15 %>% 
  dplyr::mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>%
  dplyr::select(idcntry, bsbm20a:bsbm20i, mat) %>%
  apply(2, function(x) x %>% as.numeric() %>% round(3)) %>% 
  data.frame() %>% 
  dplyr::mutate(idcntry = idcntry %>% as.character()) %>% 
  left_join(codes, by = "idcntry") %>%
  filter(!is.na(country)) %>% 
  dplyr::select(country = Name, 
                `Math Will Help Me` = bsbm20a, 
                `Need Math to Learn Other Things` = bsbm20b, 
                `Need Math to Get Into University` = bsbm20c, 
                `Need Math to Get The Job I Want` = bsbm20d, 
                `Like A Job Involving Math` = bsbm20e, 
                `Math Is Important to Get Ahead in The World` = bsbm20f, 
                `More Job Opportunities` = bsbm20g, `Parents Think Math Is Important` = bsbm20h, 
                `Important to Do Well in Math` = bsbm20i, 
                score = mat) %>% drop_na()
country.value <- student.value %>%
  dplyr::group_by(country) %>% summarise_all(funs(mean))




## @knitr table
country.like %>% head(3) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")

## @knitr biplot1
k.like <- eclust(country.like %>% dplyr::select(-country, -score), 
                 "kmeans", k = 3, nstart = 25, graph = FALSE)

country.like$cluster <- k.like$cluster
rownames(country.like) <- country.like$country

pca.like <- PCA(country.like %>% dplyr::select(-country, -cluster, -score), graph = FALSE)
fviz_pca_biplot(pca.like, repel = TRUE,
                geom = "point",
                col.var = "#B71515", # Variables color
                col.ind = country.like$cluster %>% as.factor(), 
                palette = c("#DED3D3", "#DED3D3", "#DED3D3"),
                addEllipses = TRUE, 
                legend.title = "Cluster"
)

## @knitr corHC1
hchart(get_pca_var(pca.like)$cor)

## @knitr biplot2
k.talent <- eclust(country.talent %>% dplyr::select(-country, -score), 
                   "kmeans", k = 3, nstart = 25, graph = FALSE)

country.talent$cluster <- k.talent$cluster
rownames(country.talent) <- country.talent$country

pca.talent <- PCA(country.talent %>% dplyr::select(-country, -cluster, -score), graph = FALSE)
fviz_pca_biplot(pca.talent, repel = TRUE,
                geom = "point",
                col.var = "#B71515", # Variables color
                col.ind = country.talent$cluster %>% as.factor(), 
                palette = c("#DED3D3", "#DED3D3", "#DED3D3"),
                addEllipses = TRUE, 
                legend.title = "Cluster"
)

## @knitr corHC2
hchart(get_pca_var(pca.talent)$cor)

## @knitr biplot3
k.value <- eclust(country.value %>% dplyr::select(-country, -score), 
                  "kmeans", k = 3, nstart = 25, graph = FALSE)

country.value$cluster <- k.value$cluster
rownames(country.value) <- country.value$country

pca.value <- PCA(country.value %>% dplyr::select(-country, -cluster, -score), graph = FALSE)
fviz_pca_biplot(pca.value, repel = TRUE,
                geom = "point",
                col.var = "#B71515", # Variables color
                col.ind = country.value$cluster %>% as.factor(), 
                palette = c("#DED3D3", "#DED3D3", "#DED3D3"),
                addEllipses = TRUE, 
                legend.title = "Cluster"
)

## @knitr corHC3
hchart(get_pca_var(pca.value)$cor)

## @knitr data
temp1 <- country.like %>% dplyr::select(-cluster, -score)
temp2 <- country.value %>% dplyr::select(-cluster, -score)
temp3 <- country.talent %>% dplyr::select(-cluster)
country.attitude <- temp1 %>% 
  full_join(temp2, by = "country") %>% 
  full_join(temp3, by = "country")

library(psych)
library(GPArotation)

## @knitr FA
fa.parallel(country.attitude %>% dplyr::select(-score, -country), 
            fm = "ml", fa = "fa")

## @knitr FA2
country.fa <- fa(r = country.attitude %>% dplyr::select(-score, -country), 
                 nfactor = 3, rotate = "oblimin", fm = "minchi")



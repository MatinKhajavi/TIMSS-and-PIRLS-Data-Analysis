install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
library(devtools)
install_github( "ajdamico/lodown" , dependencies = TRUE )
library(lodown)

timss_cat <-
  get_catalog( "pirls" ,
               output_dir = file.path( path.expand( "~" ) , "PIRLS" ) )

timss_cat <- subset( timss_cat , year == 2015 )

timss_cat <- lodown( "timss" , timss_cat )

read_sav("C:/Users/Matin/Desktop/New folder (6)/.sav") -> test1
rbind(test,test1)->test2

read_sav("C:/Users/Matin/Downloads/Compressed/T15_G4_SPSSData_pt3.zip")->test

intsvy.var.label(folder = "C:/Users/Matin/Downloads/Compressed/T15_G4_SPSSData_pt2",config = timss4_conf)


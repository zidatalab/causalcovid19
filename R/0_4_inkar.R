library(readr)
header <- read_delim(file = "data/sociodemographic/inkar_sdg_etc.csv",
                      delim=";",
                      col_names = FALSE,
                      n_max = 1)
inkar <- read_delim("data/sociodemographic/inkar_sdg_etc.csv", 
                            ";", escape_double = FALSE, col_names = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE, skip = 2)

colnames(inkar) <- header[1, ]

inkar <- inkar %>%
  mutate(`Age (pop. younger 18)`=`Einwohner unter 6 Jahre`+`Einwohner von 6 bis unter 18 Jahren`,
         Frauenanteil=`Geschlechterproportion insgesamt`/(1+`Geschlechterproportion insgesamt`)) %>%
  dplyr::select(contains("Haushalte mit niedrigem"),
                contains("Jahre"),
                contains("Ausländeranteil"), contains("Schutzsuchende"),
                contains("Wahlbeteiligung"), contains("AfD"),
                Einwohnerdichte, Frauenanteil,
                contains("Age"),
                Kennziffer, Raumeinheit, Aggregat,
                -contains("Einwohner unter 6"), -contains("Einwohner von 6")
  )

write_csv(inkar, "./data/inkar/inkar.csv")

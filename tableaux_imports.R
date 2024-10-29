## Import des tableaux

library(tidyverse)
library(ofce)
library(tools)
library(readxl)

dossier_tableaux <- "Tableaux 2023"


liste_imports <- list.files(path=file.path(dossier_tableaux)) 
noms_instituts <- file_path_sans_ext(liste_imports) 


import_tableau <- function(file) {

  sheet_1 <-   read_excel(file.path(dossier_tableaux,file),sheet = 1,range = "C4:F38") |> rename_all(~tolower(.x))
  sheet_2 <-   read_excel(file.path(dossier_tableaux,file),sheet = 2,range = "C4:F17") |> rename_all(~tolower(.x))
  sheet_3 <-   read_excel(file.path(dossier_tableaux,file),sheet = 3,range = "C4:F17") |> rename_all(~tolower(.x))

  date <-read_excel(file.path(dossier_tableaux,file),sheet = 1,range = "B2:B2" , col_names = FALSE) |> rename_all(~"date") |> mutate(unit = 1)
  
  res <- rbind(sheet_1,sheet_2,sheet_3) |> 
    filter(!is.na(code)) |> 
    mutate(institut = basename(file_path_sans_ext(file)) ) |> 
    mutate(date_prev = date$date[which(date$unit == 1)])

}

codes <-  rbind(
  read_excel(file.path(dossier_tableaux,liste_imports[[1]]),sheet = 1,range = "A4:C38",col_names = FALSE) |> rename_all(~tolower(.x)) |> as.data.frame(),
  read_excel(file.path(dossier_tableaux,liste_imports[[1]]),sheet = 2,range = "A4:C17",col_names = FALSE) |> rename_all(~tolower(.x)) |> as.data.frame(),
  read_excel(file.path(dossier_tableaux,liste_imports[[1]]),sheet = 3,range = "A4:C17",col_names = FALSE) |> rename_all(~tolower(.x)) |> as.data.frame()
  ) |> select(nom_var = 1, code = 3) |> 
  filter(code != "code" & !is.na(code))
  




donnees_tableau <- map(liste_imports, ~import_tableau(file=.x)) |> reduce(rbind) |> 
  as.data.frame() |> 
  select(-valeur) |> 
  left_join(codes, by ="code") |> 
  pivot_longer(cols = c(`2023`,`2024`), names_to = "year", values_to = "prev") |> 
  mutate(char_prev = ifelse(grepl("\\d",prev),
                       prev,
                       NA),
         num_prev = as.numeric(char_prev)) |> 
  mutate(
    institut = str_replace_all(institut,"INSEE","Insee")
  )

donnees_tableau |> filter(is.na(num_prev) & !is.na(char_prev))




saveRDS(donnees_tableau,"donnees_tableau2023.rds")




library(tidyverse)
library(openxlsx)
library(rsdmx)
library(insee)
library(DT)
library(zoo)


# Données observées & Tableaux --------------------------------------------

##### Observé ####

# IPCH et chomage : valeurs en fin d'année
outturn2023 <- data.frame(
  variable = c("PIB","IPCH", "Chomage",  "Solde public"),
  "out2023" = c(1.1,4.2,7.5,-5.5)
  )

outturn2023b <- data.frame(
  variable = c("PIB","IPCH", "Chomage",  "Solde public"),
  "out2023" = c(1.1,5.7,7.3,-5.5)
)
# piblast = get_insee_idbank("011779991","011779992",lastNObservations = 1)
# pibfirst = get_insee_idbank("010548500", "010548499", lastNObservations = 1)


outturn2023_old <- data.frame(
  variable = c("PIB","IPCH", "Chomage",  "Solde public"),
  "out2023" = c(0.9,4.2,7.5,-5.5)
)

outturn2023b_old <- data.frame(
  variable = c("PIB","IPCH", "Chomage",  "Solde public"),
  "out2023" = c(0.9,5.7,7.3,-5.5)
)

pib_vintage |> 
  group_by(year) |> 
  summarise(PIB = sum(OBS_VALUE))
#0.9%


ipch_levels_q<- data.frame(readSDMX(providerId = "ESTAT", resource = "data", flowRef = "PRC_HICP_MIDX", key = "M.I15.CP00.FR"))|> 
  mutate(Date = as.yearqtr(obsTime, format = "%Y-%m")) %>%
  group_by(coicop,Date)%>%
  summarise(ipch = mean(obsValue, na.rm = TRUE)) %>%
  ungroup() |> 
  mutate(inflation = 100*(ipch/lag(ipch,4)-1)
  )

structure <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/GOV_10DD_EDPT1")@datastructures[[1]]@Components)

pib <- data.frame(readSDMX(providerId = "ESTAT", flowRef = "nama_10_gdp", resource = "data",
                           key = "A.CLV_PCH_PRE.B1GQ+P31_S14+P3_S13+P51G+P6+P7.FR", start = 2018, end = 2022)) %>% 
  select(Date = obsTime, valeur = obsValue, Composante = na_item)

deficit <- data.frame(readSDMX(providerId = "ESTAT", flowRef = "gov_10dd_edpt1", resource = "data", 
                               key = "A.PC_GDP.S13.B9.FR", start = 2018, end = 2022))%>% 
  select(Date = obsTime, valeur = obsValue, Composante = na_item)

ipch <- data.frame(readSDMX(providerId = "ESTAT", flowRef = "prc_hicp_aind", resource = "data", 
                            key = "A.RCH_A_AVG.CP00.FR", start = 2018, end = 2022)) %>% 
  select(Date = obsTime, valeur = obsValue) %>% 
  mutate(Composante = "IPCH")
chom <- data.frame(readSDMX(providerId = "ESTAT", flowRef = "lfsa_urgan", resource = "data", 
                            key = "A.PC.T.Y15-74.TOTAL.FR", start = 2018, end = 2022))%>% 
  select(Date = obsTime, valeur = obsValue) %>% 
  mutate(Composante = "Chomage")


observees <- Reduce(function(...) bind_rows(...), list(pib,ipch,chom,deficit)) %>% 
  pivot_wider(names_from = "Date", values_from = "valeur")


##### 2023 ####
tableaux2022 <- readRDS("tableaux_corriges.rds") #|> 
  list_rbind()

  composantes = c("PIB", "Conso Ménages", "Conso APU", "FBCF", "Exports", "Imports", "IPCH", "Chomage", "Solde public")


prev2022 <- bind_rows(map(tableaux2022, function(x) x %>% slice(17:20, 24:25, 30,32,36) %>% select(Prev2022_2023 = `2023`))) %>% 
  mutate(institut = rep(names(tableaux2022), each = 9),
         variable = rep(composantes, times = length(names(tableaux2022))),
         Prev2022_2023 = round(digits = 1, as.numeric(Prev2022_2023)))%>%
  #group_by(Institut) %>% 
  filter(variable %in% c("PIB", "IPCH","Chomage","Solde public")) |> 
  mutate(Ecart2022_2023 = Prev2022_2023-rep(pull(outturn2023, "out2023"),times = length(names(tableaux2022)))) %>% 
  filter(!institut %in%  c("Morgan Stanley")) %>% 
  ungroup()

Classement_2023 <- prev2022 |> 
  group_by(variable) |> 
  mutate(score = (abs(Ecart2022_2023) - min(abs(Ecart2022_2023), na.rm = TRUE) )/sd(abs(Ecart2022_2023),na.rm = TRUE)  ) |> 
  select(institut,variable,score) |> 
  pivot_wider(names_from = "variable", values_from = "score") |> 
  mutate(across(-institut, ~ rank(., ties.method = "min"))) |> 
  rowwise() |> 
  mutate(total = (PIB+IPCH+Chomage +`Solde public`)/4)

gt_2023 <- Classement_2023 |>  
  gt() |> 
  opt_interactive(
    use_pagination = FALSE,
    use_pagination_info = FALSE
  ) %>% 
  tab_style(
    locations = cells_body(columns = institut),
    style = cell_text(weight = "bold")
  ) %>% 
  data_color(
    columns = PIB,
    rows = PIB < sort(PIB)[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) %>% 
  data_color(
    columns = IPCH,
    rows = IPCH < sort(IPCH)[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) %>% 
  data_color(
    columns = Chomage,
    rows = Chomage < sort(Chomage)[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) %>% 
  data_color(
    columns = `Solde public`,
    rows = `Solde public` < sort(`Solde public`)[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) %>% 
  data_color(
    columns = `Solde public`,
    rows = `Solde public` < sort(total)[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) |> 
tab_options(
  table.font.size = 3)

gt_classements <- list()

for (i in c("PIB", "IPCH", "Chomage", "Solde public")){
  gt_classements[[i]] <- Classement_2023 |> 
    rename("Solde_public" = `Solde public`) |> 
    select(institut, {{i}}) |> 
    mutate(institut := case_when(parse_expr({{i}}) ==1 ~ paste(institut, "\U1F947"),
                                 {{i}} ==2 ~ paste(institut, "\U1F948"),
                                 {{i}} == 3 ~ paste(institut, "\U1F949"), 
                                 TRUE~institut)
           
    ) |> 
    gt() |> 
    opt_interactive(
      use_pagination = FALSE,
      use_pagination_info = FALSE
    ) %>%
    tab_style(
      locations = cells_body(columns = institut),
      style = cell_text(weight = "bold")
    ) %>%
    data_color(
      columns = {{i}},
      rows = {{i}} < sort({{i}})[4],
      method = "factor",
      palette = c("gold", "grey", "brown" ))

}


test <-list()
for (i in c("PIB", "IPCH", "Chomage", "Solde_public")){
test[[i]] <- Classement_2023 |> 
  rename("Solde_public" = `Solde public`) |> 
  select(institut, {{i}}) |> 
  mutate(institut := case_when(parse_expr({{i}}) ==1 ~ paste(institut, "\U1F947"),
                              {{i}} ==2 ~ paste(institut, "\U1F948"),
                              {{i}} == 3 ~ paste(institut, "\U1F949"), 
                         TRUE~institut)
    
  ) |> 
  gt() |> 
  opt_interactive(
    use_pagination = FALSE,
    use_pagination_info = FALSE
  ) %>%
  tab_style(
    locations = cells_body(columns = institut),
    style = cell_text(weight = "bold")
  ) %>%
  data_color(
    columns = {{i}},
    rows = {{i}} < sort({{i}})[4],
    method = "factor",
    palette = c("gold", "grey", "brown" )) #%>%
#   data_color(
#     columns = IPCH,
#     rows = IPCH < sort(IPCH)[4],
#     method = "factor",
#     palette = c("gold", "grey", "brown" )) %>% 
#   data_color(
#     columns = Chomage,
#     rows = Chomage < sort(Chomage)[4],
#     method = "factor",
#     palette = c("gold", "grey", "brown" )) %>% 
#   data_color(
#     columns = `Solde public`,
#     rows = `Solde public` < sort(`Solde public`)[4],
#     method = "factor",
#     palette = c("gold", "grey", "brown" )) %>% 
#   data_color(
#     columns = `Solde public`,
#     rows = `Solde public` < sort(total)[4],
#     method = "factor",
#     palette = c("gold", "grey", "brown" )) |> 
# tab_options(
#   table.font.size = 3)
}

test[[1]]


prev2022scores <- prev2022 |> 
  group_by(variable) |> 
  mutate(na =case_when(is.na(Ecart2022_2023) ~ "NA",
                       TRUE ~ "ok"),
         Ecart2022_2023 = case_when(is.na(Ecart2022_2023) ~ max(abs(Ecart2022_2023),na.rm = TRUE),
                                    TRUE ~ Ecart2022_2023),
         score = abs( (abs(Ecart2022_2023) - min(abs(Ecart2022_2023))) /sd(abs(Ecart2022_2023))) ) |> 
  ungroup() |> 
  mutate(na = str_c(na, variable))

Classement_scores <- prev2022scores |> 
  select(institut,variable,score) |> 
  pivot_wider(names_from = variable, values_from = score) |> 
  rowwise() |> 
  mutate(score_total = (PIB+IPCH+Chomage+`Solde public`),
         score_pondéré = (0.5*PIB+0.2*Chomage+0.2*`Solde public`+0.1*IPCH))
  


##### 2022-Avant changement de base CN ####

prev2022_old <- bind_rows(map(tableaux2022, function(x) x %>% slice(17:20, 24:25, 30,32,36) %>% select(Prev2022_2023 = `2023`))) %>% 
  mutate(institut = rep(names(tableaux2022), each = 9),
         variable = rep(composantes, times = length(names(tableaux2022))),
         Prev2022_2023 = round(digits = 1, as.numeric(Prev2022_2023)))%>%
  #group_by(Institut) %>% 
  filter(variable %in% c("PIB", "IPCH","Chomage","Solde public")) |> 
  mutate(Ecart2022_2023 = Prev2022_2023-rep(pull(outturn2023_old, "out2023"),times = length(names(tableaux2022)))) %>% 
  filter(!institut %in%  c("Morgan Stanley")) %>% 
  ungroup()

Classement_2023_old <- prev2022_old |> 
  group_by(variable) |> 
  mutate(score = (abs(Ecart2022_2023) - min(abs(Ecart2022_2023), na.rm = TRUE) )/sd(abs(Ecart2022_2023),na.rm = TRUE)  ) |> 
  select(institut,variable,score) |> 
  pivot_wider(names_from = "variable", values_from = "score") |> 
  mutate(across(-institut, ~ rank(., ties.method = "min"))) |> 
  rowwise() |> 
  mutate(total = (PIB+IPCH+Chomage +`Solde public`)/4)


prev2023 <- readRDS("donnees_tableau2023.rds") |> 
  filter(code %in% c("PIB_FR", "IPCH_FR","TCHO_FR", "DEFG"), year == "2023") |> 
  select(institut,variable = code,Prev2023_2023 = num_prev) |> 
  mutate(variable = str_remove_all(variable, "_FR"),
         variable = case_when(variable == "TCHO"~ "Chomage",
                              variable == "DEFG"~ "Solde public",
                          TRUE ~ variable),
         Ecart2023_2023 = Prev2023_2023-rep(pull(outturn2023b, "out2023"),times = length(names(tableaux2022)))
  )

Classement_2023b <- prev2023 |> 
  group_by(variable) |> 
  mutate(score = (abs(Ecart2023_2023) - min(abs(Ecart2023_2023), na.rm = TRUE) )/sd(abs(Ecart2023_2023),na.rm = TRUE)  ) |> 
  select(institut,variable,score) |> 
  pivot_wider(names_from = "variable", values_from = "score") |> 
  mutate(across(-institut, ~ rank(., ties.method = "min"))) |> 
  rowwise() |> 
  mutate(total = (PIB+IPCH+Chomage +`Solde public`)/4)

Classement_consolidated <- Classement_2023 |> 
  full_join(Classement_2023, by = "institut", suffix = c("_lt", "_ct")) |> 
  mutate(total = total_lt+total_ct)

save(list = ls(pattern = "Classement_.+"),
     file = "classements2023.rda")
save(list=ls(pattern = "prev\\d{4}.?"),
     file = "prev2023.rda")

save(list=ls(pattern = "^gt_.+"),
     file = "gt2023.rda")
load("Tableaux Classements.rda")

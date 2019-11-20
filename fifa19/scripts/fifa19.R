# Análisis para presentar en clase de Nacho. Analítica en el deporte
# Datos de fifa 19 sobre características de los jugadores.
# It means that when the game was launched the value before the + is the true value. 
# However, as the season proceeds a players form may improve. And if it does it will improve by the value on the right
# side of the +. For the purpose of analysis, you can ignore the values on the right side of the +.

paquetes <- c("data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)

dt <- data.table::fread(cmd = 'unzip -cq fifa19/data/fifa19.zip', encoding = "UTF-8")
dt[, V1 := NULL]
clean_names <- function(x) {
    names(x) %>%
        tolower(.) %>%
        trimws(.) %>% 
        stringr::str_replace_all(., pattern = " ", replacement = "_")
    # Si no se desea usar paquetes extras, no usar el pipe %>% ni usar stringr
    # En dicho caso sería:
    # gsub(x = tolower(names(x)), pattern = " ", replacement = "_")
}
# Arreglamos los nombres
setnames(dt, old = names(dt), new = clean_names(dt))
# Que tipo de variables tenemos
dt[, table(sapply(.SD, class))]
# Solo character y numeric? Claramente hay factores que los tomo como character.
dt$contract_valid_until

# Limpieza valores €, M, K, {Jul 1, 2004}
dt[, value]
cols <- c("value", "wage","release_clause")
dt[, .SD, .SDcols = cols]
clean_values <- function(x) {
    base::gsub(pattern = "€", replacement = "", fixed = TRUE, x = x) %>% 
        # strsplit(., "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
        data.table::tstrsplit(.,"(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", type.convert = TRUE, perl = TRUE)
}
# correct_values <- function(DT, value, value_unit) {
#     DT[, c(value, value_unit) := clean_values(get(value))
#        ][get(value_unit) == "M", (value) := get(value) * 1000000
#          ][get(value_unit) == "K", (value) := get(value) * 1000
#            ][, `:=`(value = get(value)/1000000,
#                     value_unit = NULL)]
# }
# correct_values(dt, value = "value", value_unit = "value_unit")

# Hacer una función con esto...
dt[, c("value", "value_unit") := clean_values(value)
   ][value_unit == "M", value := value * 1000000
     ][value_unit == "K", value := value * 1000
       ][, `:=`(value_million = value/1000000,
                value_unit = NULL,
                value = NULL)]

dt[, c("wage", "wage_unit") := clean_values(wage)
   ][wage_unit == "M", wage := wage * 1000000
     ][wage_unit == "K", wage := wage * 1000
       ][, `:=`(wage_million = wage/1000000,
                wage_unit = NULL,
                wage = NULL)]

dt[, c("release_clause", "release_clause_unit") := clean_values(release_clause)
   ][release_clause_unit == "M", release_clause := release_clause * 1000000
     ][release_clause_unit == "K", release_clause := release_clause * 1000
       ][, `:=`(release_clause_million = release_clause/1000000,
                release_clause_unit = NULL,
                release_clause = NULL)]

cols = c("value_million", "wage_million", "release_clause_million")
dt[, .SD, .SDcols = cols]

# Limpieza de fechas: joined
dt[, joined]
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
dt[, joined := as.Date(joined, format = "%b %d, %Y")]
Sys.setlocale("LC_TIME", lct)

# Limpieza de medidas, height & weight
# feet to inches: feet*12 
# 1 inches ~ 2.54cm
dt[, height := {
    a = tstrsplit(height, split = "'", fixed = TRUE, type.convert = TRUE)
    as.integer((a[[1]]*12 + a[[2]])*2.54)
}]
# 1 pund 0.453592
dt[, weight := {
    as.integer(as.integer(gsub(weight, pattern = "[a-z]", replacement = ""))*0.453592)
}]

# Limpiar variables con signos +
names_f = names(dt)
cols = names_f[names_f %>% nchar(.) <= 3 & !names_f %in% c("id", "age")]
dt[, (cols) := lapply(.SD, function(x) as.integer(gsub(x, pattern = "\\+\\d", replacement = ""))), .SDcols = cols]

# variables que no me interesan a borrar
cols = c("id", "photo", "flag", "club_logo")
dt[, (cols) := NULL]

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv') %>% 
    as.data.table(.)
df[, .(CODE, COUNTRY)]
dt[df, on = "nationality == COUNTRY"]
df[dt, on = "COUNTRY == nationality"][is.na(CODE), unique((COUNTRY))]
# UK England, Scotland, Wales and Northern Ireland
# Republic of Ireland = Irland
# Cote d'Ivoire = Ivory Coast
# Bosnia Herzegovina = Bosnia and Herzegovina
# China PR = China
# Korea Republic = Korea, South
dt[, nationality2 := dplyr::case_when(
    nationality == "Wales"     ~ "United Kingdom",
    nationality == "England"   ~ "United Kingdom",
    nationality == "Scotland"  ~ "United Kingdom",
    nationality == "Northern Ireland" ~ "United Kingdom",
    TRUE ~ nationality
)]
dt[nationality == "Republic of Ireland", nationality2 := "Ireland"]
dt[nationality == "Ivory Coast", nationality2 := "Cote d'Ivoire"]
dt[nationality == "Bosnia Herzegovina", nationality2 := "Bosnia and Herzegovina"]
dt[nationality == "China PR", nationality2 := "China"]
dt[nationality == "Korea Republic", nationality2 := "Korea, South"]
df[, unique(COUNTRY) %>% sort]

# Pierdo estos países, como no son relevantes no los arreglo por ahora. (171 observaciones)
#        "Central African Rep." "DR Congo"             "Cape Verde"          
# [6] "FYR Macedonia"                     "Guinea Bissau"        "Gambia"               "Congo"               
# [11] "Trinidad & Tobago"    "São Tomé & Príncipe"  "Korea DPR"            "St Kitts Nevis"       "Antigua & Barbuda"   
# [16] "Montserrat"           "Palestine"            "St Lucia"

dt[df, on = "nationality2 == COUNTRY", `:=`(code = CODE,
                                            gdp = GDP..BILLIONS.)]


# GK: Goalkeeper
 
# CB: Center-back
# LCB: Left center-back
# RCB: Right center-back
# LB: Left-back (Full-back)
# RB: Right-back (Full-back)

# CM : Centre midfield
# LDM: Left center midfield
# LAM: Left attacking midfield
# RDM: Right center midfield
# RAM: Right attacking midfield
# CDM: Centre defensive midfield
# CAM: Centre attacking midfield
# LM : Left midfield
# RM : Right midfield

# ST: Striker
# CF: Center forward
# LW: Left winger
# RW: Right winger

# CAM   CB  CDM   CF   CM   GK  LAM   LB  LCB  LCM  LDM   LF   LM   LS   LW  LWB  RAM   RB  RCB  RCM  RDM   RF   RM   RS 
# 60  958 1778  948   74 1394 2025   21 1322  648  395  243   15 1095  207  381   78   21 1291  662  391  248   16 1124  203 
# RW  RWB   ST 
# 370   87 2152 

# Hacerlo con regex y no como un boludo


change_position <- function(x) {
    # gsub(x = x, pattern = "(CB)|(LB)|(RB)|(LCB)|(RCB)", replacement = "defender", perl = T) %>% 
    # gsub(x = ., pattern = "(CM)|(LDM)|(LAM)|(RDM)|(RAM)|(CDM)|(CAM)|(LM)|(RM)", replacement = "midfielder", perl = T) %>% 
    # gsub(x = ., pattern = "(ST)|(CF)|(LW)|(RW)", replacement = "attacker", perl = T)
    # stringr::str_replace_all(string = x, pattern = .pattern, replacement = .replace)
    stringr::str_detect()
}


dt[, position_summary := "no_detectado"
   ][stringr::str_detect(pattern = "GK", position), position_summary := "GK"
   ][stringr::str_detect(pattern = "CB|LB|RB|LCB|RCB", position), position_summary := "defender"
     ][stringr::str_detect(pattern = "CM|LDM|LAM|RDM|RAM|CDM|CAM|LM|RM", position), position_summary := "midfielder"
       ][stringr::str_detect(pattern = "ST|CF|LW|RW|RF|RS", position), position_summary := "attacker"]

dt[, position_summary2 := "no_detectado"
   ][stringr::str_detect(pattern = "GK", position), position_summary2 := "GK"
   ][stringr::str_detect(pattern = "CB|LCB|RCB", position), position_summary2 := "center_back"
     ][stringr::str_detect(pattern = "LB|RB", position), position_summary2 := "full_back"
       ][stringr::str_detect(pattern = "CM|LDM|RDM|CDM", position), position_summary2 := "defensive_midfield"
         ][stringr::str_detect(pattern = "LAM|RAM|CAM|LM|RM|RF", position), position_summary2 := "attacking_midfield"
           ][stringr::str_detect(pattern = "ST|CF|RS", position), position_summary2 := "center_forward"
             ][stringr::str_detect(pattern = "LW|RW", position), position_summary2 := "wing"]
# RF que es messi lo puse de wing o attacking midfield

dt[, table(position_summary)]
dt[, table(position_summary2)]
dt[, gdp := NULL]

saveRDS(dt, here::here("fifa19", "data", "fifa.rds"), compress = FALSE)

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
dt[, (cols) := lapply(.SD, function(x) gsub(x, pattern = "\\+\\d", replacement = "")), .SDcols = cols]

# variables que no me interesan a borrar
cols = c("id", "photo", "flag", "club_logo")
dt[, (cols) := NULL]
saveRDS(dt, here::here("fifa19", "data", "fifa.rds"), compress = FALSE)

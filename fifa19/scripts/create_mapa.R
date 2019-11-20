# Creación de mapa
# En el script Mapa_Uruguay se trabaja la base para el mapa y se utiliza la función fun_mapa, luego se hacen 2 matcheos y 
# se genera la tabla que es insumo para la función CreateMapa

fun_mapa <- function() {
    uruguay              <- raster::getData("countries")
    # uruguay_states       <- raster::getData("GADM", country = "UY", level = 1)
    uystates_UTM         <- sp::spTransform(uruguay, sp::CRS("+init=EPSG:5383"))
    uystates_UTM@data$id <- rownames(uystates_UTM@data)
    uystates_dt          <- ggplot2::fortify(uystates_UTM) %>% 
        data.table::as.data.table(.)
    uystates_dt[uystates_UTM@data, on = "id == id", `:=`(departamento = NAME_1)]
    uystates_dt <-  uystates_dt[!(departamento == "Rivera"& lat < 6400000), ]
    uystates_dt[, `:=`(departamento = factor(departamento),
                       hole = NULL,
                       piece = NULL)]
    return(uystates_dt)
}

CreateMapa <- function(base, filtrar = FALSE, MesFiltro = "Diciembre", 
                       plotly = TRUE, colorear = Media, ...) {
    
    # valor = rlang::enquo(colorear)
    # Si plotly TRUE generar el mapa con plotly. En caso contrario con ggplot2
    if(plotly) {
        # Layout
        map_axis <- list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        # Mapa
        {if (filtrar) {
            base[mes == MesFiltro, ]
        } else {
            base
        }} %>% 
            plotly::plot_ly(
                source = "map",
                x = ~long,
                y = ~lat,
                # color =  rlang::enquo(colorear), # No usar =~ al dar un parámetro da error
                color = ~get(colorear),
                split = ~group) %>%
            plotly::add_polygons(
                hoverinfo = "text",
                text = ~paste("Alumnos:", N_alumno, "<br>",
                              "Escuelas:", N_centro, "<br>",
                              "Q1:", round(Q1, 2), "<br>",
                              "Promedio:", round(Media, 2), "<br>",
                              "Q2:", round(Mediana, 2), "<br>",
                              "Q3:", round(Q3, 2)),
                hoveron = "fills",
                line = list(width = 0.4),
                showlegend = FALSE) %>% 
            plotly::layout(title = "Índice de engagement",
                           xaxis = map_axis, 
                           yaxis = map_axis) %>%
            plotly::colorbar(title = "Valor")
    } else {
        # Tema a utilizar
        theme_opts <- list(theme(panel.grid.minor = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.background = element_blank(),
                                 plot.background = element_blank(),
                                 axis.line = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank(),
                                 plot.title = element_blank()
        )) # aspect.ratio = 1
        
        
        etiquetas <- base[, .(mlong = mean(long), mlat = mean(lat)), by = departamento]
        
        ggplot() +
            geom_polygon(data = base, aes_string(x = "long", y = "lat", group = "group", 
                                                 fill = colorear), color = "black", size = 0.25) +
            geom_text(data = etiquetas, aes(label = departamento, x = mlong, y = mlat)) +
            # theme(aspect.ratio = 1) + labs(fill = "Cantidad")+
            # scale_fill_gradient2( midpoint = mean(uystates_df_eng$`Q1.25%`)) +
            theme_opts
    }
    
}


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv') %>% 
    as.data.table(.)
df[, .(CODE, COUNTRY)] %>% View()
dt[df, on = "nationality == COUNTRY"]
df[dt, on = "COUNTRY == nationality"][is.na(CODE), unique((COUNTRY))]
# [1] "England"              "Wales"                "Bosnia Herzegovina"   "Korea Republic"       "Scotland"            
# [6] "Central African Rep." "DR Congo"             "Ivory Coast"          "Republic of Ireland"  "Cape Verde"          
# [11] "FYR Macedonia"        "China PR"             "Guinea Bissau"        "Gambia"               "Congo"               
# [16] "Northern Ireland"     "Trinidad & Tobago"    "São Tomé & Príncipe"  "Korea DPR"            "St Kitts Nevis"      
# [21] "Antigua & Barbuda"    "Montserrat"           "Palestine"            "St Lucia"    
# UK England, Scotland, Wales and Northern Ireland
# Republic of Ireland = Irland
# Cote d'Ivoire = Ivory Coast
# Bosnia Herzegovina = Bosnia and Herzegovina
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
df[, unique(COUNTRY) %>% sort]

# Pierdo estos países, como no son relevantes no los arreglo por ahora.
# [1] "Bosnia Herzegovina"   "Korea Republic"       "Central African Rep." "DR Congo"             "Cape Verde"          
# [6] "FYR Macedonia"        "China PR"             "Guinea Bissau"        "Gambia"               "Congo"               
# [11] "Trinidad & Tobago"    "São Tomé & Príncipe"  "Korea DPR"            "St Kitts Nevis"       "Antigua & Barbuda"   
# [16] "Montserrat"           "Palestine"            "St Lucia"

dt[df, on = "nationality2 == COUNTRY", `:=`(code = CODE,
                                            gdp = GDP..BILLIONS.)]

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
)

plot_geo(dt) %>%
    add_trace(
        z = ~gdp, color = ~gdp, colors = 'Blues',
        text = ~nationality2, locations = ~code, marker = list(line = l)
    ) %>%
    colorbar(title = 'GDP Billions US$', tickprefix = '$') %>%
    layout(
        title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
        geo = g
    )

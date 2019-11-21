library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    temp <- reactive({
        if (input$func == "sum") {
            temp = dt[, .(variable = sum(get(input$covariable1)), na.rm = TRUE),  by = .(code, nationality2)]
        } else if (input$func == "mean") {
            temp = dt[, .(variable = mean(get(input$covariable1)), na.rm = TRUE),  by = .(code, nationality2)]
        } else if (input$func == "sd") {
            temp = dt[, .(variable = sd(get(input$covariable1)), na.rm = TRUE),  by = .(code, nationality2)]
        } else if (input$func == "max") {
            temp = dt[, .(variable = max(get(input$covariable1)), na.rm = TRUE),  by = .(code, nationality2)]
        } else if (input$func == "min") {
            temp = dt[, .(variable = min(get(input$covariable1)), na.rm = TRUE),  by = .(code, nationality2)]
        }
    }
    )

    output$mapa <- renderPlotly({
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        
        # map projection/options
        g <- list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator')
        )

        plot_geo(temp()) %>%
            add_trace(
                z = ~variable, color = ~variable, colors = 'Blues',
                text = ~nationality2, locations = ~code, marker = list(line = l)
            ) %>%
            colorbar(title = 'euros', tickprefix = '') %>%
            layout(
                title = '<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">Kaggle</a>',
                geo = g
            )
    })
    
        # Estimación de kernel
    output$kernel <- renderPlotly({
    
        (ggplot(dt, aes_string(input$covariable1)) +
                stat_density(aes_string(fill = input$covariable3), alpha = 0.3, na.rm = FALSE, 
                             position = "identity", color = "black", kernel = "epanechnikov") +
                labs(title = paste("Estimación de kernel por", input$covariable3),
                     y = "Densidad",
                     x = input$covariable3) 
                # geom_vline(xintercept = intercepto,
                #            linetype = "dotted", color = "red", size = 0.5)
        ) %>%
            ggplotly(.)
    })
    
    output$scatter <- renderPlot({
        dt %>% 
            ggplot(., aes_string(x = input$covariable1, y = input$covariable2)) +
            geom_point(alpha = 0.3) +
            facet_wrap(as.formula(paste("~", input$covariable3)), scale = "fixed") +
            scale_color_viridis(discrete = FALSE) +
            theme_ipsum() +
            # theme(legend.position = "none")
            labs(title = paste("Diagrama dispersión", input$covariable1, input$covariable2), 
                 x = input$covariable1, y = input$covariable2)
        
    })
    
    output$scatter3D <- renderPlotly({
        plot_ly(data = dt) %>%
            add_trace(
                x =~ get(input$covariable1), 
                y =~ get(input$covariable2), 
                z =~ get(input$covariable4), 
                name = "z", 
                type = "scatter3d", alpha = 0.3, 
                color = I("blue")
            ) %>% 
            layout(
                scene = list(
                    aspectratio = list(
                        x = 1,
                        y = 1,
                        z = 1
                    ),
                    camera = list(
                        center = list(
                            x = 0,
                            y = 0,
                            z = 0
                        ),
                        eye = list(
                            x = 1.96903462608,
                            y = -1.09022831971,
                            z = 0.405345349304
                        ),
                        up = list(
                            x = 0,
                            y = 0,
                            z = 1
                        )
                    ),
                    dragmode = "turntable",
                    xaxis = list(
                        title = input$covariable1,
                        type = ""
                    ),
                    yaxis = list(
                        title = input$covariable2,
                        type = ""
                    ),
                    zaxis = list(
                        title = input$covariable4,
                        type = ""
                    )
                ),
                xaxis = list(title = "x"),
                yaxis = list(title = "y")
            )
    })
    
    output$distribucion <- renderPlotly({

        (dt[get(input$covariable1) > quantile(get(input$covariable1), 
                                              probs = 1 - as.numeric(input$top_x_dist)/100, na.rm = TRUE), ] %>% 
           ggplot(., aes_string((input$covariable1))) +
               geom_histogram(aes(fill =..count..)) +
               theme_ipsum() +
               labs(title = paste0("Distribución del ", input$top_x_dist, "% ","de ", input$covariable1))) %>% 
               ggplotly(.)
           
    })
    
    # Matriz de correlación
    output$correlacion <- renderPlot({
        cols_numeric = names(dt)[dt[, sapply(.SD, is.numeric)]]
        dt[, 
           corrplot::corrplot(corr = cor(.SD, use = "complete.obs"), type = "lower", 
                              diag = FALSE, order = "hclust", addrect = 4, tl.col = "black",
                              tl.srt = 45), 
           .SDcols = cols_numeric]    
    })
    

# Clubes ------------------------------------------------------------------

# Ranking de clubes
    output$top_n_clubes <- renderPlotly({
        dt[, .(valor = mean(get(input$covariable1))), by = (club = factor(club))
           ][1:as.integer(input$top_n), 
             (ggplot(data = .SD, aes(x = reorder(club, valor), y = valor)) +
                 geom_bar(stat = "identity", aes(fill = valor)) +
                 coord_flip() +
                 theme_ipsum() +
                 scale_color_viridis(discrete = FALSE) +
                 labs(x = "Clubes", y = input$covariable1,
                      title = paste("Top", input$top_n, "clubes más valiosos", "según", input$covariable1))
              ) %>% ggplotly(.)
             ]    
    })
    
    # Distribución top clubes
#     sum(get(input$covariable1)))
# factor(club)
#     output$distribucion_top_clubes <- renderPlotly({
#         dt[, .(valor = sum(wage_million)), by = (club = factor(club))
#            ][1:as.integer(input$top_n), 
#              (ggplot(data = .SD, aes(x = reorder(club, valor), y = valor)) +
#                   geom_violin(trim = F) +
#                   geom_boxplot(width = 0.1) +
#                   # theme_ipsum() +
#                   # scale_color_viridis(discrete = FALSE) +
#                   labs(x = "Clubes", y = input$covariable1,
#                        title = paste("Top", input$top_n, "clubes más valiosos", "según", input$covariable1))
#              ) %>% ggplotly(.)
#              ]    
#     })
    
    output$distribucion_clubes <- renderPlotly({
        most_value_clubs <- dt %>% 
            group_by(club) %>% 
            summarise(club.squad.value = round(sum(wage_million))) %>% 
            arrange(-club.squad.value) %>% 
            head(n = input$top_n)
        
        player_list <- list()
        
        for ( i in 1:NROW(most_value_clubs)) {
            temp_data <- dt %>% 
                filter(club == most_value_clubs[[1]][i])
            
            player_list[[i]] <- temp_data
        }
        
        data <- lapply(player_list, as.data.frame) %>%
            bind_rows()
        
        data$club <- as.factor(data$club)
        
        (ggplot(data, aes_string(x = "club", y = input$covariable1, fill = "club")) +
                geom_violin(trim = F) +
                geom_boxplot(width = 0.1) +
                theme(axis.text.x = element_text(angle = 90), legend.position = "none")) %>% 
            ggplotly(.)
    })
    

# Jugadores ---------------------------------------------------------------

    #  QUEDE ACÁ
# Jugadores más valiosos por posición
    output$top_n_jugadores <- renderPlotly({
        # setorder(dt, -"wage_million" )
        # dt[, .SD[1:10], by = position_summary
        #    ][, .(get("wage_million"), get("position_summary"), get("name"))
        #      ][,(ggplot(.SD, aes(x = reorder(V3, V1), y = V1)) +
        #             geom_bar(stat = "identity", aes_string(fill = V1)) +
        #             coord_flip() +
        #             # facet_wrap(as.formula(paste("~", "position_summary"))) +
        #             facet_wrap(. ~ V2) +
        #             theme_ipsum() +
        #             scale_color_viridis(discrete = FALSE) +
        #             labs(x = "Clubes", y = "input$covariable1",
        #                  title = paste("Top", "input$top_n", "clubes más valiosos \n", "según", "input$covariable1)"))
        #      ) %>% ggplotly(.)]
        
           dt[,(ggplot(.SD, aes(x = reorder(name, wage_million), y = wage_million)) +
                  geom_bar(stat = "identity", aes_string(fill = input$covariable1)) +
                  coord_flip() +
                  facet_wrap(as.formula(paste("~", input$covariable3))) +
                  theme_ipsum() +
                  scale_color_viridis(discrete = FALSE) +
                  labs(x = "Clubes", y = input$covariable1,
                       title = paste("Top", input$top_n, "clubes más valiosos \n", "según", input$covariable1))
        ) %>% ggplotly(.)]
        
    })
    
# Componentes principales
    
    cols_numeric = names(dt)[dt[, sapply(.SD, is.numeric)]]
    sub_dt <- copy(dt[, .SD, .SDcols = cols_numeric])
    sub_dt[, `:=`(value_million = value_million * 1000000,
                  wage_million  = wage_million  * 1000000,
                  release_clause_million = release_clause_million * 1000000)]
    acp = dt[complete.cases(dt), prcomp(.SD, center = TRUE, scale = TRUE) %>% 
                 summary(.), .SDcols = cols_numeric]
    # Agregar biplot también
    
    output$pca <- renderPlotly({
        acp %>% 
            `[[`("rotation") %>% `^`(2) %>% `*`(100) %>% `[`(, 1:as.integer(input$n_acp)) %>% 
            as.data.table(keep.rownames = "variables") %>% 
            melt(data = ., id.vars = "variables", variable.name = "componentes", value.name = "ponderacion") %>% 
            plot_ly(data = ., 
                    y =~ variables, 
                    x =~ componentes, 
                    z =~ ponderacion,
                    type = "heatmap") %>% 
            layout(title = 'Ponderación de variables por componentes',
                   xaxis = list(title = 'componentes'),
                   yaxis = list(title = 'variables'))    
    })
    
    output$tablePca <- DT::renderDataTable({
        temp <- acp$importance %>% as.data.table(keep.rownames = "medidas")
        # cols <-  names(temp %>% `[`(,1:3))[2:3]
        cols <-  names(temp %>% `[`(,1:(1+as.integer(input$n_acp))))[2:(1+as.integer(input$n_acp))]
        temp %>% `[`(,1:(1+as.integer(input$n_acp))) %>% 
            DT::datatable(data = ., rownames = FALSE) %>% 
            DT::formatRound(columns = cols, digits = 2)
        # temp %>% `[`(,1:(1 + `input$n_acp`)) %>% 
        #     DT::datatable(data = ., rownames = FALSE) %>% 
        #     DT::formatRound(columns = cols, digits = 2)
    })
    

# Modelo predictivo -------------------------------------------------------
    
    
    
    
})

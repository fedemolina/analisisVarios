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
        dt[, .(valor = sum(get(input$covariable1))), by = (club = factor(club))
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
    
    
    
    
})

install.packages(c("shiny", "gapminder", "ggplot2", "dplyr", 
                   "plotly", "sf", "rnaturalearth", "rnaturalearthdata",
                   "gganimate", "gifski", "transformr", "DT", "tidyr"))
library(gapminder)
head(gapminder) 


# DASHBOARD GAPMINDER 1972 - 2007

library(shiny)
library(gapminder)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)

datos_gapminder <- gapminder %>%
  filter(year >= 1972 & year <= 2007)

ui <- fluidPage(
  titlePanel("Dashboard Gapminder - Evolucion Mundial 1972-2007"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controles"),
      
      sliderInput("year_select",
                  "Selecciona el Año:",
                  min = 1972,
                  max = 2007,
                  value = 2007,
                  step = 5,
                  animate = TRUE),
      
      selectInput("continent_select",
                  "Selecciona Continente(s):",
                  choices = unique(datos_gapminder$continent),
                  selected = unique(datos_gapminder$continent),
                  multiple = TRUE),
      
      checkboxInput("log_scale", 
                    "Usar escala logaritmica en PIB", 
                    value = TRUE),
      
      helpText("Fuente: Gapminder (paquete de R)."),
      helpText("Años disponibles: 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007")
    ),
    
    mainPanel(
      tabsetPanel(
        
        
        tabPanel("Mapa Mundial", 
                 plotlyOutput("mapa_mundial", height = "600px")),
        
        tabPanel("Tendencias Historicas", 
                 plotlyOutput("tendencia", height = "500px")),
        
        tabPanel("Proyecciones", 
                 plotlyOutput("proyeccion", height = "500px")),
        
        tabPanel("Relacion de Variables", 
                 plotlyOutput("relacion_variables", height = "550px")),
        
        tabPanel("Datos Filtrados", 
                 DTOutput("tabla_datos"),
                 downloadButton("descargar", "Descargar CSV"))
      )
    )
  )
)


server <- function(input, output, session) {
  datos_filtrados <- reactive({
    datos_gapminder %>%
      filter(year == input$year_select,
             continent %in% input$continent_select)
  })
  
  # Mapa Mundial
  output$mapa_mundial <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos para esta seleccion"))
    }
    
    # Coordenadas 
    df <- df %>%
      mutate(
        lon = case_when(
          continent == "Africa" ~ 20,
          continent == "Americas" ~ -80,
          continent == "Asia" ~ 100,
          continent == "Europe" ~ 10,
          continent == "Oceania" ~ 140
        ),
        lat = case_when(
          continent == "Africa" ~ 5,
          continent == "Americas" ~ 15,
          continent == "Asia" ~ 25,
          continent == "Europe" ~ 50,
          continent == "Oceania" ~ -25
        )
      )
    
    df$texto <- paste(
      "Pais: ", df$country, "<br>",
      "Continente: ", df$continent, "<br>",
      "Esperanza de vida: ", round(df$lifeExp, 1), " años<br>",
      "PIB per capita: $", round(df$gdpPercap, 0), "<br>",
      "Poblacion: ", round(df$pop / 1e6, 1), " millones"
    )
    
    # Mapa
    plot_ly(
      data = df,
      type = 'scattergeo',
      mode = 'markers',
      lon = ~lon,
      lat = ~lat,
      text = ~texto,
      marker = list(
        size = ~sqrt(pop) / 150,
        color = ~lifeExp,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "Esperanza de vida (años)"),
        opacity = 0.8,
        line = list(width = 1, color = "black")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste("Esperanza de vida por continente - Año", input$year_select),
          x = 0.5
        ),
        geo = list(
          projection = list(type = 'natural earth'),
          showland = TRUE,
          landcolor = 'rgb(230, 230, 230)',
          countrycolor = 'rgb(200, 200, 200)',
          showocean = TRUE,
          oceancolor = 'rgb(200, 220, 240)'
        )
      )
  })
  
  #  TENDENCIAS HISTORICAS
  output$tendencia <- renderPlotly({
    tendencia <- datos_gapminder %>%
      filter(continent %in% input$continent_select) %>%
      group_by(continent, year) %>%
      summarise(lifeExp_prom = mean(lifeExp), .groups = 'drop')
    
    p <- ggplot(tendencia, aes(x = year, y = lifeExp_prom, color = continent)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Evolucion de la Esperanza de Vida por Continente",
           x = "Año", 
           y = "Esperanza de vida promedio (años)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  #  PROYECCION CON REGRESION LINEAL
  output$proyeccion <- renderPlotly({
    
    datos_region <- datos_gapminder %>%
      filter(continent == "Americas") %>%
      group_by(year) %>%
      summarise(lifeExp_prom = mean(lifeExp), .groups = 'drop')
    
    modelo <- lm(lifeExp_prom ~ year, data = datos_region)
    
    # Proyeccion hasta 2025
    anos_futuros <- data.frame(year = seq(1972, 2025, by = 5))
    anos_futuros$lifeExp_prom <- predict(modelo, newdata = anos_futuros)
    anos_futuros$tipo <- ifelse(anos_futuros$year <= 2007, "Historico", "Proyeccion")
    
    datos_region$tipo <- "Historico"
    datos_completos <- bind_rows(datos_region, anos_futuros)
    
    p <- ggplot(datos_completos, aes(x = year, y = lifeExp_prom)) +
      geom_line(data = filter(datos_completos, tipo == "Historico"), 
                aes(color = "Historico"), size = 1.5) +
      geom_point(data = filter(datos_completos, tipo == "Historico"), 
                 aes(color = "Historico"), size = 3) +
      geom_line(data = filter(datos_completos, tipo == "Proyeccion"), 
                aes(color = "Proyeccion"), size = 1.5, linetype = "dashed") +
      geom_point(data = filter(datos_completos, tipo == "Proyeccion"), 
                 aes(color = "Proyeccion"), size = 3) +
      geom_smooth(data = datos_region, aes(x = year, y = lifeExp_prom), 
                  method = "lm", se = TRUE, color = "darkred", fill = "red", alpha = 0.2,
                  inherit.aes = FALSE) +
      labs(title = "Proyeccion de Esperanza de Vida - Continente Americano",
           subtitle = paste("Ecuacion: Esperanza de vida =", 
                            round(coef(modelo)[1], 2), "+", 
                            round(coef(modelo)[2], 2), "x Año"),
           x = "Año", 
           y = "Esperanza de vida promedio (años)") +
      scale_color_manual(name = "Tipo", 
                         values = c("Historico" = "blue", "Proyeccion" = "orange")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  #  RELACION PIB vs ESPERANZA DE VIDA
  output$relacion_variables <- renderPlotly({
    df_relacion <- datos_filtrados()
    
    p <- ggplot(df_relacion, aes(x = gdpPercap, y = lifeExp, 
                                 size = pop, color = continent)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
      {if(input$log_scale) scale_x_log10() else scale_x_continuous()} +
      scale_size_continuous(range = c(2, 15), guide = guide_legend(title = "Poblacion")) +
      labs(title = paste("Relacion: PIB vs Esperanza de vida -", input$year_select),
           x = if(input$log_scale) "PIB per capita (USD, escala logaritmica)" else "PIB per capita (USD)",
           y = "Esperanza de vida (años)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y", "colour", "size"))
  })
  
  #  TABLA DE DATOS FILTRADOS
  output$tabla_datos <- renderDT({
    datatable(
      datos_filtrados() %>%
        select(Pais = country, 
               Continente = continent, 
               Ano = year,
               Esperanza_vida = lifeExp, 
               Poblacion = pop, 
               PIB_per_capita = gdpPercap),
      options = list(pageLength = 10, scrollX = TRUE),
      caption = paste("Datos del año", input$year_select)
    )
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste("gapminder_", input$year_select, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

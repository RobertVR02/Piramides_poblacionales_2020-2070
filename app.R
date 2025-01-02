library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

pob <- read.csv("Pob_1950_2070.csv", encoding = "latin1") %>%
  rename_all(tolower)

#Datos para los años 2020 hasta 2070.
#útiles para crear la interfaz gráfica.
datos <- filter(pob, año %in% c(2020,2030,2040,2050,2060,2070) & entidad =="República Mexicana")

datos <- mutate(datos, grupo_edades=case_when(
  edad>=0 & edad<=8 ~1,
  edad>=9 & edad<=16 ~2,
  edad>=17 & edad<=24 ~3,
  edad>=25 & edad<=32 ~4,
  edad>=33 & edad<=40 ~5,
  edad>=41 & edad<=48 ~6,
  edad>=49 & edad<=56 ~7,
  edad>=57 & edad<=64 ~8,
  edad>=65 & edad<=72 ~9,
  edad>=73 & edad<=80 ~10,
  edad>=81 & edad<=88 ~11,
  edad>=89 & edad<=96 ~12,
  edad>=97 ~13  
))
datos$grupo_edades <- 
  factor(datos$grupo_edades, 
         levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
         labels=c("0 a 8 años","9 a 16","17 a 24",
                  "25 a 32","33 a 40","41 a 48",
                  "49 a 56","57 a 64","65 a 72",
                  "73 a 80","81 a 88","89 a 96",
                  "97 años y más"))
datos <- mutate(datos, pob=poblacion/1000000)
datos <- datos%>% group_by(año, sexo, grupo_edades) %>%
  summarise(pob=sum(pob))

#==================== APP:

ui <- fluidPage( 
  titlePanel("",),
  fluidRow( 
    column(12,selectInput("selected_year", "Selecciona el Año:", choices = unique(datos$año))
           ) 
    ), 
  fluidRow( 
    column(12, plotlyOutput("piramide_plot")  )) )

# Definir la aplicación shiny dentro de un documento Quarto
server <- function(input, output) {
  output$piramide_plot <- renderPlotly({
    data_filtrada <- subset(datos, año == input$selected_year)
    
    g<- ggplot(data=data_filtrada, 
               mapping=aes(x=grupo_edades, y=pob, 
                           text = paste("Grupo de Edad:", grupo_edades, "<br>Sexo:", sexo, "<br>Población:", abs(pob)))) +
      geom_col(data=subset(data_filtrada, sexo=="Hombres") %>% 
                 mutate(pob = -1 * (pob)), 
               fill="lightblue", width = 0.95) +
      geom_col(data=subset(data_filtrada, sexo=="Mujeres"),
               fill="lightpink", width = 0.95) +
      coord_flip() + theme_bw() +
      scale_y_continuous(
        breaks = round(c(seq(-14, -2, 2), seq(0, 14, 2)), 2),
        labels = round(c(seq(-14, -2, 2) * -1, seq(0, 14, 2)), 2)) +
      theme(legend.position = "bottom") +
      labs(title = paste("Pirámide poblacional del", input$selected_year),
           y = "Millones de personas", x = "Grupo de edades",
           caption = "Fuente: Elaboración propia con base en la CONAPO.")
    ggplotly(g, tooltip = "text")
  })
}
shinyApp(ui = ui, server = server)


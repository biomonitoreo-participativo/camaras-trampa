# Paquetes
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(shiny)
library(shinydashboard)


# Lectura de datos
# detection
deteccion <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/detection.csv"
    )


# Curación de datos
# detection
deteccion <-
    deteccion %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y:%m:%d %H:%M:%OS")) %>%
    mutate(monthCaptured = month(dateTimeCaptured)) %>%
    mutate(hourCaptured = hour(dateTimeCaptured))


# Lista ordenada de especies + "Todas"
lista_especies <- unique(deteccion$species)
lista_especies <- sort(lista_especies)
lista_especies <- c("Todas", lista_especies)


# Definición del objeto ui
ui <-
    dashboardPage(
        dashboardHeader(title = "Cámaras trampa"),
        dashboardSidebar(sidebarMenu(
            menuItem(
                text = "Filtros",
                selectInput(
                    inputId = "especie",
                    label = "Especie",
                    choices = lista_especies,
                    selected = "Todas"
                ),
                startExpanded = TRUE
            )
        )),
        dashboardBody(fluidRow(
            box(
                title = "Horas de detección",
                plotOutput(outputId = "histograma"),
                width = 12
            ),
        ))
    )

# Definición de la función server
server <- function(input, output) {
    filtrarDatos <- reactive({
        deteccion_filtrado <-
            deteccion
        
        # Filtrado por especie
        if (input$especie != "Todas") {
            deteccion_filtrado <-
                deteccion_filtrado %>%
                filter(species == input$especie)
        }
        
        return(deteccion_filtrado)
    })
    
    output$histograma <- renderPlot({
        deteccion_filtrado <-
            filtrarDatos()
        
        deteccion_filtrado %>%
            ggplot(aes(x = hourCaptured)) +
            geom_histogram(binwidth = 1,
                           color = "black",
                           fill = "white") +
            geom_density(
                aes(y = ..count..),
                color = "gray",
                fill = "gray",
                alpha = 0.4
            ) +
            ggtitle(input$especie) +
            xlab("Hora") +
            ylab("Frecuencia") +
            theme_ipsum()
    })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)

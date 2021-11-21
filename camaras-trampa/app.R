# Paquetes
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(shiny)
library(shinydashboard)


# Lectura de datos
# Especies detectadas
deteccion <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/detection.csv"
    )

# Indicadores
indicadores <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/indicator.csv"
    )


# Curación de datos
# Especies detectadas
deteccion <-
    deteccion %>%
    subset(species %in% indicadores$scientificName) %>%
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
        dashboardBody(box(
            title = "Distribución de fotografías en las horas del día",
            plotOutput(outputId = "deteccion"),
            width = 12
        ),)
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
    
    output$deteccion <- renderPlot({
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
            ggtitle(if_else(input$species == "Todas", "Todas las especies", input$species)) +
            xlab("Hora") +
            ylab("Fotografías")
    })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
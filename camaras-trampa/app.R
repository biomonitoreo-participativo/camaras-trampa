# Paquetes
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
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


# Función para asignación de grupo de especies
grupo = function(especie) {
    ifelse(
        especie %in% c(
            "Leopardus pardalis",
            "Leopardus tigrinus",
            "Leopardus wiedii",
            "Panthera onca",
            "Puma concolor",
            "Puma yagouaroundi"
        ),
        "Felinos",
        ifelse(
            especie %in% c("Cuniculus paca", "Dasyprocta punctata"),
            "Guatusas y tepezcuintles",
            ifelse(
                especie %in% c(
                    "Mazama temama",
                    "Odocoileus virginianus",
                    "Pecari tajacu",
                    "Tapirus bairdii",
                    "Tayassu pecari"
                ),
                "Ungulados",
                "Otros"
            )
        )
    )
}


# Curación de datos
# Especies detectadas
deteccion <-
    deteccion %>%
    subset(species %in% indicadores$scientificName) %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y:%m:%d %H:%M:%OS")) %>%
    mutate(monthCaptured = month(dateTimeCaptured)) %>%
    mutate(hourCaptured = hour(dateTimeCaptured)) %>%
    mutate(group = grupo(species))


# Lista ordenada de grupos + "Todos"
lista_grupos <- unique(deteccion$group)
lista_grupos <- sort(lista_grupos)
lista_grupos <- c("Todos", lista_grupos)

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
                    inputId = "grupo",
                    label = "Grupo",
                    choices = lista_grupos,
                    selected = "Todos"
                ),                
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
            title = "Distribución en las horas del día de las fotografías tomadas",
            plotOutput(outputId = "distribucion_horas_fotografias"),
            width = 12
        ),)
    )

# Definición de la función server
server <- function(input, output, session) {
    filtrarDatos <- reactive({
        deteccion_filtrado <-
            deteccion

        # Filtrado por grupo
        if (input$grupo != "Todos") {
            deteccion_filtrado <-
                deteccion_filtrado %>%
                filter(group == input$grupo)
            
            if (input$especie == "Todas") {
                # Lista ordenada de especies del grupo + "Todas"
                deteccion_grupo <- filter(deteccion, group == input$grupo)
                lista_especies_grupo <- unique(deteccion_grupo$species)
                lista_especies_grupo <- sort(lista_especies_grupo)
                lista_especies_grupo <- c("Todas", lista_especies_grupo)
                
                updateSelectInput(
                    session,
                    "especie",
                    label = "Especie",
                    choices = lista_especies_grupo,
                    selected = "Todas"
                )
            }            
        } else {
            updateSelectInput(
                session,
                "especie",
                label = "Especie",
                choices = lista_especies,
                selected = "Todas"
            )            
        }       
                
        # Filtrado por especie
        if (input$especie != "Todas") {
            deteccion_filtrado <-
                deteccion_filtrado %>%
                filter(species == input$especie)
        }
        
        return(deteccion_filtrado)
    })
    
    output$distribucion_horas_fotografias <- renderPlot({
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
            ylab("Cantidad de fotografías")
    })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
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
                startExpanded = TRUE,
                menuSubItem(text = "Resumen", tabName = "tab_resumen"),
                menuSubItem(text = "Horas por especie", tabName = "tab_distribucion_horas_fotografias_detalle"),
                menuSubItem(text = "Meses por especie", tabName = "tab_distribucion_meses_fotografias_detalle")
            )
        )),
        dashboardBody(
            tags$head(
                tags$script(
                    '
              // Este código JS permite ampliar el largo de un box de shinydashboard.
              // Fue publicado en:
              // https://stackoverflow.com/questions/56965843/height-of-the-box-in-r-shiny

              // Define function to set height of "map" and "map_container"
              setHeight = function() {
                var window_height = $(window).height();
                var header_height = $(".main-header").height();

                var boxHeight = window_height - header_height - 30;

                $("#box_distribucion_horas_fotografias_detalle").height(boxHeight - 20);
                // $("#distribucion_horas_fotografias_detalle").height(boxHeight - 20);
                $("#distribucion_horas_fotografias_detalle").height(boxHeight - 40);
              };

              // Set input$box_height when the connection is established
              $(document).on("shiny:connected", function(event) {
                setHeight();
              });

              // Refresh the box height on every window resize event
              $(window).on("resize", function(){
                setHeight();
              });
            '
                )
            ),
            tabItems(
                tabItem(tabName = "tab_resumen",
                        fluidRow(
                            box(
                                title = "Distribución en las horas del día de las fotografías tomadas",
                                plotOutput(outputId = "distribucion_horas_fotografias_resumen", height = 250),
                                width = 12
                            )
                        ), fluidRow(
                            box(
                                title = "Distribución en los meses del año de las fotografías tomadas",
                                plotOutput(outputId = "distribucion_meses_fotografias_resumen", height = 250),
                                width = 12
                            )
                        )),
                tabItem(tabName = "tab_distribucion_horas_fotografias_detalle",
                        fluidRow(
                            box(
                                id = "box_distribucion_horas_fotografias_detalle",
                                title = "Distribución en las horas del día de las fotografías tomadas",
                                plotOutput(outputId = "distribucion_horas_fotografias_detalle"),
                                width = 12
                            )
                        )),
                tabItem(tabName = "tab_distribucion_meses_fotografias_detalle",
                        fluidRow(
                            box(
                                id = "box_distribucion_meses_fotografias_detalle",
                                title = "Distribución en los meses del año de las fotografías tomadas",
                                plotOutput(outputId = "distribucion_meses_fotografias_detalle"),
                                width = 12
                            )
                        ))
            )
        )
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
                deteccion_grupo <-
                    filter(deteccion, group == input$grupo)
                lista_especies_grupo <-
                    unique(deteccion_grupo$species)
                lista_especies_grupo <- sort(lista_especies_grupo)
                lista_especies_grupo <-
                    c("Todas", lista_especies_grupo)
                
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
    
    output$distribucion_horas_fotografias_resumen <- renderPlot({
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
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Cantidad de fotografías")
    })
    
    output$distribucion_meses_fotografias_resumen <- renderPlot({
        deteccion_filtrado <-
            filtrarDatos()
        
        deteccion_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count") +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Mes") +
            ylab("Cantidad de fotografías")
    })
    
    output$distribucion_horas_fotografias_detalle <- renderPlot({
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
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Cantidad de fotografías") +
            facet_wrap(~ species, ncol = 2)
    })
    
    output$distribucion_meses_fotografias_detalle <- renderPlot({
        deteccion_filtrado <-
            filtrarDatos()
        
        deteccion_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count") +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Mes") +
            ylab("Cantidad de fotografías") +
            facet_wrap(~ species, ncol = 2)
    })
    
    
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
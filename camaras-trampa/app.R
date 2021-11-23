# Paquetes
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinydashboard)


# Lectura de datos
# Detecciones de especies en las cámaras
detecciones <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/detection.csv"
    )

# Estaciones en dónde están ubicadas las cámaras
estaciones <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/station.csv"
    )

# Registros de cámaras trampa (detecciones + estaciones)
# Se usan dos conjuntos de datos (detecciones y registros_camaras)
# debido a que hay detecciones sin coordenadas.
# registros_camaras tiene solo las detecciones con coordenadas.
registros_camaras <- inner_join(detecciones, estaciones)
registros_camaras <-
    registros_camaras %>% drop_na(longitude, latitude)
registros_camaras <-
    registros_camaras %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Especies indicadoras
indicadores <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/indicator.csv"
    )


# Función para asignación de grupo a una especie
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
detecciones <-
    detecciones %>%
    subset(species %in% indicadores$scientificName) %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y:%m:%d %H:%M:%OS")) %>%
    mutate(hourCaptured = hour(dateTimeCaptured)) %>%
    mutate(group = grupo(species))

registros_camaras <-
    registros_camaras %>%
    subset(species %in% indicadores$scientificName) %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y:%m:%d %H:%M:%OS")) %>%
    mutate(hourCaptured = hour(dateTimeCaptured)) %>%
    mutate(group = grupo(species))


# Lista ordenada de grupos + "Todos"
lista_grupos <- unique(detecciones$group)
lista_grupos <- sort(lista_grupos)
lista_grupos <- c("Todos", lista_grupos)

# Lista ordenada de especies + "Todas"
lista_especies <- unique(detecciones$species)
lista_especies <- sort(lista_especies)
lista_especies <- c("Todas", lista_especies)


# Definición del objeto ui
ui <-
    dashboardPage(
        dashboardHeader(title = "Registros de cámaras trampa"),
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
                menuSubItem(text = "Distribución en horas por spp.", tabName = "tab_distribucion_registros_horas_especies"),
                menuSubItem(text = "Distribución en meses por spp.", tabName = "tab_distribucion_registros_meses_especies")
            )
        )),
        dashboardBody(
            tags$head(
                tags$script(
                    '
              // Este bloque de código JavaScript permite ampliar el largo de un box de shinydashboard.
              // Está basado en:
              // https://stackoverflow.com/questions/56965843/height-of-the-box-in-r-shiny

              // Define function to set height of "map" and "map_container"
              setHeight = function() {
                var window_height = $(window).height();
                var header_height = $(".main-header").height();

                var boxHeight = window_height - header_height - 30;

                $("#box_distribucion_registros_horas_especies").height(boxHeight - 20);
                $("#box_distribucion_registros_meses_especies").height(boxHeight - 20);
                // $("#distribucion_registros_horas_especies").height(boxHeight - 20);
                $("#distribucion_registros_horas_especies").height(boxHeight - 40);
                $("#distribucion_registros_meses_especies").height(boxHeight - 40);
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
                            column(
                                width = 6,
                                box(
                                    title = "Ubicación de las cámaras",
                                    leafletOutput(outputId = "mapa", height = 500),
                                    width = NULL
                                )
                            ),
                            column(
                                width = 6,
                                box(
                                    title = "Distribución de los registros en las horas del día",
                                    plotOutput(outputId = "distribucion_registros_horas_resumen", height = 250),
                                    width = NULL
                                ),
                                box(
                                    title = "Distribución de los registros en los meses del año",
                                    plotOutput(outputId = "distribucion_registros_meses_resumen", height = 250),
                                    width = NULL
                                )
                            ),
                            
                        )),
                tabItem(tabName = "tab_distribucion_registros_horas_especies",
                        fluidRow(
                            box(
                                id = "box_distribucion_registros_horas_especies",
                                title = "Distribución de los registros en las horas del día",
                                plotOutput(outputId = "distribucion_registros_horas_especies"),
                                width = 12
                            )
                        )),
                tabItem(tabName = "tab_distribucion_registros_meses_especies",
                        fluidRow(
                            box(
                                id = "box_distribucion_registros_meses_especies",
                                title = "Distribución de los registros en los meses del año",
                                plotOutput(outputId = "distribucion_registros_meses_especies"),
                                width = 12
                            )
                        ))
            )
        )
    )

# Definición de la función server
server <- function(input, output, session) {
    filtrarDetecciones <- reactive({
        detecciones_filtrado <-
            detecciones
        
        # Filtrado por grupo
        if (input$grupo != "Todos") {
            detecciones_filtrado <-
                detecciones_filtrado %>%
                filter(group == input$grupo)
            
            if (input$especie == "Todas") {
                # Lista ordenada de especies del grupo + "Todas"
                detecciones_grupo <-
                    filter(detecciones, group == input$grupo)
                lista_especies_grupo <-
                    unique(detecciones_grupo$species)
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
        }
        
        # Filtrado por especie
        if (input$especie != "Todas") {
            detecciones_filtrado <-
                detecciones_filtrado %>%
                filter(species == input$especie)
        }
        
        return(detecciones_filtrado)
    })
    
    
    filtrarRegistrosCamaras <- reactive({
        registros_camaras_filtrado <-
            registros_camaras
        
        # Filtrado por grupo
        if (input$grupo != "Todos") {
            registros_camaras_filtrado <-
                registros_camaras_filtrado %>%
                filter(group == input$grupo)
            
            if (input$especie == "Todas") {
                # Lista ordenada de especies del grupo + "Todas"
                detecciones_grupo <-
                    filter(detecciones, group == input$grupo)
                lista_especies_grupo <-
                    unique(detecciones_grupo$species)
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
        }
        
        # Filtrado por especie
        if (input$especie != "Todas") {
            registros_camaras_filtrado <-
                registros_camaras_filtrado %>%
                filter(species == input$especie)
        }
        
        return(registros_camaras_filtrado)
    })
    
    
    output$distribucion_registros_horas_resumen <- renderPlot({
        detecciones_filtrado <-
            filtrarDetecciones()
        
        detecciones_filtrado %>%
            ggplot(aes(x = hourCaptured)) +
            geom_histogram(binwidth = 1) +
            geom_density(
                aes(y = ..count..),
                alpha = 0.4
            ) +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Registros de cámaras")
    })
    
    
    output$distribucion_registros_meses_resumen <- renderPlot({
        detecciones_filtrado <-
            filtrarDetecciones()
        
        detecciones_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count") +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Mes") +
            ylab("Registros de cámaras")
    })
    
    
    output$distribucion_registros_horas_especies <- renderPlot({
        detecciones_filtrado <-
            filtrarDetecciones()
        
        detecciones_filtrado %>%
            ggplot(aes(x = hourCaptured)) +
            geom_histogram(binwidth = 1) +
            geom_density(
                aes(y = ..count..),
                alpha = 0.4
            ) +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Registros de cámaras") +
            facet_wrap( ~ species, ncol = 2)
    })
    
    
    output$distribucion_registros_meses_especies <- renderPlot({
        detecciones_filtrado <-
            filtrarDetecciones()
        
        detecciones_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count") +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Mes") +
            ylab("Registros de cámaras") +
            facet_wrap( ~ species, ncol = 2)
    })
    
    
    output$mapa <- renderLeaflet({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        # Mapa Leaflet con capas de ...
        leaflet() %>%
            addTiles(group = "OpenStreetMap") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
            addCircleMarkers(
                data = registros_camaras_filtrado,
                group = "Registros de cámaras",
                stroke = TRUE,
                radius = 4,
                fillColor = 'red',
                fillOpacity = 1,
                label = paste0(
                    registros_camaras_filtrado$species,
                    ", ",
                    registros_camaras_filtrado$deploymentLocationID,
                    ", ",
                    registros_camaras_filtrado$dateTimeCaptured
                ),
                popup = paste0(
                    "<strong>Grupo: </strong>",
                    registros_camaras_filtrado$group,
                    "<br>",                    
                    "<strong>Especie: </strong>",
                    registros_camaras_filtrado$species,
                    "<br>",
                    "<strong>Ubicación: </strong>",
                    registros_camaras_filtrado$deploymentLocationID,
                    "<br>",        
                    "<strong>Fecha y hora: </strong>",
                    registros_camaras_filtrado$dateTimeCaptured,
                    "<br>",        
                    "<strong>Organización: </strong>",
                    registros_camaras_filtrado$Organization
                )                
            ) %>%
            addLayersControl(
                baseGroups = c(
                    "OpenStreetMap",
                    "Stamen Toner Lite",
                    "CartoDB Dark Matter",
                    "Imágenes de ESRI"
                ),
                overlayGroups = c(
                    "Registros de cámaras"
                )
            ) %>%
            addScaleBar(position = "bottomleft",
                        options = scaleBarOptions(imperial = FALSE)) %>%
            addMouseCoordinates() %>%
            addSearchOSM() %>%
            addResetMapButton()            
    })
    
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
# Paquetes
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DT)
library(ggplot2)
library(ggthemes)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinydashboard)



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
                ifelse(
                    especie %in% c(
                        "Alouatta palliata",
                        "Ateles geoffroyi",
                        "Cebus imitator",
                        "Saimiri oerstedii"
                    ),
                    "Primates",
                    ifelse(
                        especie %in% c(
                            "Pharomachrus mocinno",
                            "Trogon rufus",
                            "Trogon bairdii",
                            "Trogon caligatus",
                            "Trogon massena",
                            "Trogon collaris",
                            "Procnias tricarunculatus",
                            "Myadestes melanops",
                            "Tinamus major",
                            "Nothocercus bonapartei",
                            "Crypturellus soui",
                            "Crax rubra",
                            "Penelope purpurascens",
                            "Ortalis cinereiceps",                            
                            "Chamaepetes unicolor"
                        ),
                        "Aves",
                        ifelse(
                            especie %in% c(
                                "Atelopus varius"
                            ),
                            "Herpetofauna",
                            "Otros"
                        )
                    )
                )
            )
        )
    )
}



# Valores globales
MINUTOS_AGRUPACION = 30.0



# Lectura de datos en archivos
# Detecciones de especies en las cámaras
detecciones <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/detection.csv"
    )

# Instalaciones de cámaras
instalaciones <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/deployment.csv"
    )

# Estaciones en dónde están ubicadas las cámaras
estaciones <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/crtms/station.csv"
    )

# Especies indicadoras
indicadores <-
    read.csv(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/indicator.csv"
    )


# Correcciones preliminares (deberían corregirse en los archivos de datos)
detecciones <-
    detecciones %>%
    rename(deploymentID = SampligCode) %>%
    rename(numberOfShots = Numberofshots) %>%
    mutate(
        deploymentID = replace(
            deploymentID,
            deploymentLocationID == "La Chiripa",
            "ACLAP_JER_CHR_01"
        )
    )

instalaciones <-
    instalaciones %>%
    filter(
        !(
            deploymentID == 'ACLAP_ANG_CAV_02' &
                cameraDeploymentBeginDate == '2020-12-11' |
                deploymentID == 'ACLAP_CBC_EVE_01' &
                cameraDeploymentBeginDate == '2021-02-02'
        )
    )

estaciones <-
    estaciones %>%
    rename(organization = Organization) %>%
    filter(!(
        deploymentLocationID == 'Sendero Catarata' &
            organization == 'Tres Colinas'
    ))


# Otras modificaciones
detecciones <-
    detecciones %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y:%m:%d %H:%M:%OS")) %>%
    select(-numberOfShots)


# Filtro de detecciones por especies indicadoras
detecciones <-
    detecciones %>%
    subset(species %in% indicadores$scientificName)


# Agrupación de detecciones con la misma especie y ubicación y un máximo de MINUTOS_AGRUPACION de diferencia de tiempo
# Ordenamiento de detecciones
detecciones <-
    detecciones %>%
    arrange(projectID,
            deploymentID,
            deploymentLocationID,
            species,
            dateTimeCaptured)

# Dataframe para detecciones agrupadas
detecciones_agrupadas <-
    data.frame(
        projectID = character(),
        deploymentID = character(),
        deploymentLocationID = character(),
        species = character(),
        dateTimeCaptured = character()
    )

projectID_1 <- as.data.frame(detecciones)[1, "projectID"]
deploymentID_1 <- as.data.frame(detecciones)[1, "deploymentID"]
deploymentLocationID_1 <- as.data.frame(detecciones)[1, "deploymentLocationID"]
species_1 <- as.data.frame(detecciones)[1, "species"]
dateTimeCaptured_1 <- as.data.frame(detecciones)[1, "dateTimeCaptured"]

detecciones_agrupadas[1,] <-
    c(
        projectID_1,
        deploymentID_1,
        deploymentLocationID_1,
        species_1,
        format(dateTimeCaptured_1, "%Y-%m-%d %H:%M:%OS")
    )

for (row in 2:nrow(detecciones)) {
    projectID_2 <- as.data.frame(detecciones)[row, "projectID"]
    deploymentID_2 <- as.data.frame(detecciones)[row, "deploymentID"]
    deploymentLocationID_2 <- as.data.frame(detecciones)[row, "deploymentLocationID"]
    species_2 <- as.data.frame(detecciones)[row, "species"]
    dateTimeCaptured_2 <- as.data.frame(detecciones)[row, "dateTimeCaptured"]
    
    if (projectID_2 != projectID_1 |
        deploymentID_2 != deploymentID_1 |
        deploymentLocationID_2 != deploymentLocationID_1 |
        species_2 != species_1 |
        abs(
            difftime(dateTimeCaptured_2, dateTimeCaptured_1, units = 'mins') > MINUTOS_AGRUPACION
        )) {
        detecciones_agrupadas[nrow(detecciones_agrupadas) + 1,] <-
            c(
                projectID_2,
                deploymentID_2,
                deploymentLocationID_2,
                species_2,
                format(dateTimeCaptured_2, "%Y-%m-%d %H:%M:%OS")
            )
        
        projectID_1 <- projectID_2
        deploymentID_1 <- deploymentID_2
        deploymentLocationID_1 <- deploymentLocationID_2
        species_1 <- species_2
        dateTimeCaptured_1 <- dateTimeCaptured_2
    }
}

# Transformaciones y columnas adicionales
detecciones_agrupadas <-
    detecciones_agrupadas %>%
    mutate(dateTimeCaptured = as_datetime(dateTimeCaptured, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(hourCaptured = hour(dateTimeCaptured)) %>%
    mutate(group = grupo(species))


# Join de detecciones, instalaciones y estaciones
registros_camaras_temp <-
    left_join(detecciones_agrupadas,
              instalaciones,
              by = c("deploymentID" = "deploymentID")) %>%
    rename(deploymentLocationID = deploymentLocationID.x) %>%
    select(-deploymentLocationID.y,-cameraFailureDetails,-treatment)


# Objeto sf final con registros de cámaras trampa
registros_camaras <-
    left_join(
        registros_camaras_temp,
        estaciones,
        by = c("deploymentLocationID" = "deploymentLocationID")
    ) %>%
    rename(projectID = projectID.x) %>%
    select(-projectID.y) %>%
    mutate(cameraDeploymentBeginDate = as_datetime(cameraDeploymentBeginDate, format = "%Y-%m-%d")) %>%
    mutate(cameraDeploymentEndDate = as_datetime(cameraDeploymentEndDate, format = "%Y-%m-%d")) %>%
    mutate(days = as.numeric(cameraDeploymentEndDate - cameraDeploymentBeginDate, units="days")) %>%
    mutate(year = year(cameraDeploymentEndDate)) %>% ## 2022-06-05
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Cálculo del IAR

# IAR por instalación
iar_x_instalacion <-
    registros_camaras %>%
    st_drop_geometry() %>%
    group_by(projectID, organization, deploymentLocationID, deploymentID, days, group, species, year) %>% # 2022-06-05 , year
    summarize(n = n()) %>%
    mutate(iar_instalacion = n/days*100)

# Esto duplica la lista de especies que hay más adelante
especies <- unique(registros_camaras$species)
especies <- as.data.frame(especies)
especies <- rename(especies, species=especies)

# instalaciones-especies
instalaciones_especies <-
    full_join(
        select(
            instalaciones,
            deploymentID,
            cameraDeploymentBeginDate,
            cameraDeploymentEndDate
        ),
        especies,
        by = character()
    ) %>%
    mutate(cameraDeploymentBeginDate = as_datetime(cameraDeploymentBeginDate, format = "%Y-%m-%d")) %>%
    mutate(cameraDeploymentEndDate = as_datetime(cameraDeploymentEndDate, format = "%Y-%m-%d")) %>%
    mutate(days = as.numeric(cameraDeploymentEndDate - cameraDeploymentBeginDate, units="days"))

# Join para incluir las especies no detectadas en una estación
iar_x_instalacion_2 <-
    right_join(iar_x_instalacion, instalaciones_especies, by=c('deploymentID', 'species'))

iar_x_instalacion_2 <-
    rename(iar_x_instalacion_2, days = days.y) %>%
    select(-days.x)

iar_x_instalacion_2 <- iar_x_instalacion_2 %>% mutate(n = ifelse(is.na(n), 0, n))
iar_x_instalacion_2 <- iar_x_instalacion_2 %>% mutate(iar_instalacion = ifelse(is.na(iar_instalacion), 0, iar_instalacion))

# IAR por especie
iar_x_especie <-
    iar_x_instalacion_2 %>%
    group_by(species) %>%
    summarize(iar_especie=mean(iar_instalacion), sd=sd(iar_instalacion))



# Listas de selección
# Lista ordenada de grupos + "Todos"
lista_grupos <- unique(registros_camaras$group)
lista_grupos <- sort(lista_grupos)
lista_grupos <- c("Todos", lista_grupos)

# Lista ordenada de especies + "Todas"
lista_especies <- unique(registros_camaras$species)
lista_especies <- sort(lista_especies)
lista_especies <- c("Todas", lista_especies)

# Lista ordenada de localidades + "Todas"
lista_localidades <- unique(registros_camaras$deploymentLocationID)
lista_localidades <- sort(lista_localidades)
lista_localidades <- c("Todas", lista_localidades)

# Lista ordenada de organizaciones + "Todas"
lista_organizaciones <- unique(registros_camaras$organization)
lista_organizaciones <- sort(lista_organizaciones)
lista_organizaciones <- c("Todas", lista_organizaciones)



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
                selectInput(
                    inputId = "organizacion",
                    label = "Brigada",
                    choices = lista_organizaciones,
                    selected = "Todas"
                ),                     
                selectInput(
                    inputId = "localidad",
                    label = "Localidad",
                    choices = lista_localidades,
                    selected = "Todas"
                ),                        
                startExpanded = TRUE,
                menuSubItem(text = "Resumen", tabName = "tab_resumen"),
                menuSubItem(text = "Distribución en horas por spp.", tabName = "tab_distribucion_registros_horas_especies"),
                menuSubItem(text = "Distribución en meses por spp.", tabName = "tab_distribucion_registros_meses_especies"),
                menuSubItem(text = "Registros de cámaras trampa", tabName = "tab_tabla_registros_camaras"),
                menuSubItem(text = "Índice de abundancia relativa", tabName = "tab_indice_abundancia_relativa")
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
                                    plotOutput(outputId = "distribucion_registros_horas_resumen", height = 200),
                                    width = NULL
                                ),
                                box(
                                    title = "Distribución de los registros en los meses del año",
                                    plotOutput(outputId = "distribucion_registros_meses_resumen", height = 200),
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
                        )),
                tabItem(tabName = "tab_tabla_registros_camaras",
                        fluidRow(
                            box(
                                id = "box_tabla_registros_camaras",
                                title = "Registros de cámaras trampa",
                                DTOutput(outputId = "tabla_registros_camaras"),
                                width = 12
                            )
                        )),
                tabItem(tabName = "tab_indice_abundancia_relativa",
                        fluidRow(
                            box(
                                id = "box_indice_abundancia_relativa",
                                title = "Índice de abundancia relativa",
                                plotOutput(outputId = "indice_abundancia_relativa"),
                                width = 12
                            )
                        ))                                
            )
        )
    )

# Definición de la función server
server <- function(input, output, session) {
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
                registros_camaras_grupo <-
                    filter(registros_camaras_filtrado, group == input$grupo)
                lista_especies_grupo <-
                    unique(registros_camaras_grupo$species)
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

        # Filtrado por organización
        if (input$organizacion != "Todas") {
            registros_camaras_filtrado <-
                registros_camaras_filtrado %>%
                filter(organization == input$organizacion)
        }        
        
        # Filtrado por localidad
        if (input$localidad != "Todas") {
            registros_camaras_filtrado <-
                registros_camaras_filtrado %>%
                filter(deploymentLocationID == input$localidad)
        }            
        
        return(registros_camaras_filtrado)
    })
    
    filtrarIAR <- reactive({
        iar_x_instalacion_filtrado <-
            iar_x_instalacion
        
        # Filtrado por grupo
        if (input$grupo != "Todos") {
            iar_x_instalacion_filtrado <-
                iar_x_instalacion_filtrado %>%
                filter(group == input$grupo)
            
            if (input$especie == "Todas") {
                # Lista ordenada de especies del grupo + "Todas"
                iar_x_instalacion_grupo <-
                    filter(iar_x_instalacion_filtrado, group == input$grupo)
                lista_especies_grupo <-
                    unique(iar_x_instalacion_grupo$species)
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
            iar_x_instalacion_filtrado <-
                iar_x_instalacion_filtrado %>%
                filter(species == input$especie)
        }
        
        # Filtrado por organización
        if (input$organizacion != "Todas") {
            iar_x_instalacion_filtrado <-
                iar_x_instalacion_filtrado %>%
                filter(organization == input$organizacion)
        }        
        
        # Filtrado por localidad
        if (input$localidad != "Todas") {
            iar_x_instalacion_filtrado <-
                iar_x_instalacion_filtrado %>%
                filter(deploymentLocationID == input$localidad)
        }            
        
        iar_x_especie_filtrado <-
            iar_x_instalacion_filtrado %>%
            group_by(species, year) %>% # 2022-06-05 , year
            summarize(iar_especie=mean(iar_instalacion))
        
        return(iar_x_especie_filtrado)
    })    
    
    output$distribucion_registros_horas_resumen <- renderPlot({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            ggplot(aes(x = hourCaptured)) +
            geom_histogram(binwidth = 1,
                           color = "black",
                           fill = "white") +
            geom_density(
                aes(y = ..count..),
                color = "black",
                fill = "gray",
                alpha = 0.4                
            ) +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Registros de cámaras") +
            theme_economist()
    })
    
    
    output$distribucion_registros_meses_resumen <- renderPlot({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count", color = "black",
                     fill = "white") +
            ggtitle(if_else(input$species == "Todas",
                            "Todas las especies",
                            input$species)) +
            xlab("Mes") +
            ylab("Registros de cámaras") +
            theme_economist()
    })
    
    output$indice_abundancia_relativa_resumen <- renderPlot({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            ggplot(aes(species)) +
            geom_bar(stat = "count", color = "black",
                     fill = "white") +
            ggtitle(if_else(input$species == "Todas",
                            "Todas las especies",
                            input$species)) +
            xlab("Mes") +
            ylab("Registros de cámaras") +
            coord_flip() +
            theme_economist()
    })    
    
    
    output$distribucion_registros_horas_especies <- renderPlot({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            ggplot(aes(x = hourCaptured)) +
            geom_histogram(binwidth = 1,
                           color = "black",
                           fill = "white") +
            geom_density(
                aes(y = ..count..),
                color = "black",
                fill = "gray",
                alpha = 0.4                    
            ) +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Hora") +
            ylab("Registros de cámaras") +
            facet_wrap( ~ species, ncol = 2) +
            theme_economist()
    })
    
    
    output$distribucion_registros_meses_especies <- renderPlot({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            ggplot(aes(format(dateTimeCaptured, "%m"))) +
            geom_bar(stat = "count", color = "black",
                     fill = "white") +
            ggtitle(if_else(
                input$species == "Todas",
                "Todas las especies",
                input$species
            )) +
            xlab("Mes") +
            ylab("Registros de cámaras") +
            facet_wrap( ~ species, ncol = 2) +
            theme_economist()
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
                    "<strong>Brigada: </strong>",
                    registros_camaras_filtrado$organization,     
                    "<br>",                            
                    "<strong>Localidad: </strong>",
                    registros_camaras_filtrado$deploymentLocationID,
                    "<br>",        
                    "<strong>Instalación: </strong>",
                    registros_camaras_filtrado$deploymentID,
                    "<br>",        
                    "<strong>Fecha y hora: </strong>",
                    registros_camaras_filtrado$dateTimeCaptured
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
    
    output$tabla_registros_camaras <- renderDT({
        registros_camaras_filtrado <-
            filtrarRegistrosCamaras()
        
        registros_camaras_filtrado %>%
            st_drop_geometry() %>%
            select(
                group,
                species,
                organization,                                
                deploymentLocationID,                
                deploymentID,
                dateTimeCaptured,                
                cameraDeploymentBeginDate,
                cameraDeploymentEndDate
            ) %>%
            datatable(
                rownames = FALSE,
                colnames = c("Grupo", "Especie", "Brigada", "Localidad", "Instalación", "Fecha y hora", "Fecha inicial", "Fecha final"),
                extensions = c("Buttons"),
                options = list(
                    pageLength = 10,
                    searchHighlight = TRUE,
                    lengthMenu = list(
                        c(10, 20, 30, 40, 50, -1),
                        c(10, 20, 30, 40, 50, "Todos")
                    ),
                    dom = 'Bfrtlip',
                    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                    buttons = list(
                        list(extend = 'copy', text = 'Copiar'),
                        list(extend = 'csv', text = 'CSV'),
                        list(extend = 'csv', text = 'Excel'),
                        list(extend = 'pdf', text = 'PDF')
                    )
                )
            )        
        
    })
    
    output$indice_abundancia_relativa <- renderPlot({
        iar_x_especie_filtrado <-
            filtrarIAR()
        
        iar_x_especie_filtrado %>%
            filter(year == 2020 | year == 2021) %>%
            ggplot(aes(x = factor(year),
                       y = iar_especie)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = round(iar_especie, digits = 2)), vjust = 1.5, colour = "white") +
            ggtitle(if_else(input$species == "Todas",
                            "Todas las especies",
                            input$species)) +
            xlab("Año") +
            ylab("Índice de abundancia relativa") +
            facet_wrap( ~ species) +
            theme_economist()
    })            
    
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
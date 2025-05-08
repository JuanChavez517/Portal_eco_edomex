
# ----------------------- Portal de Datos Economicos ------------------------- #

# ------------------- Autor: Juan Manuel Chávez Rebollar --------------------- #


# definicion del directorio de trabajo ------------------------------------

setwd("C:/Users/juamc/OneDrive/Documentos/Juan_Chavez/Articulos/2025_portal/shiny_app")

# shiny app ------------------------------------------------------------
# 1. lectura de datos --------------------------------------------------------

ta_nac_tot = readRDS("Datos/ta_nac_tot.rds")
map_ent = readRDS("Datos/map_ent.rds")
ta_ent_tot = readRDS("Datos/ta_ent_tot.rds")
ta_ent_hom = readRDS("Datos/ta_ent_hom.rds")
ta_ent_muj = readRDS("Datos/ta_ent_muj.rds")
ta_ent_ed1 = readRDS("Datos/ta_ent_ed1.rds")
ta_ent_ed2 = readRDS("Datos/ta_ent_ed2.rds")
ta_ent_ed3 = readRDS("Datos/ta_ent_ed3.rds")
ta_ent_sm1 = readRDS("Datos/ta_ent_sm1.rds")
ta_ent_sm2 = readRDS("Datos/ta_ent_sm2.rds")
ta_ent_sm3 = readRDS("Datos/ta_ent_sm3.rds")
map_mun = readRDS("Datos/map_mun.rds")
ta_mun_tot = readRDS("Datos/ta_mun_tot.rds")
denue_cifras_ent = readRDS("Datos/denue_cifras_ent.rds")
denue_cifras_mun = readRDS("Datos/denue_cifras_mun.rds")
map_mun_ue = readRDS("Datos/map_mun_ue.rds")
#denue = readRDS("Datos/denue.rds")
denue_mic = readRDS("Datos/denue_mic.rds")
denue_peq = readRDS("Datos/denue_peq.rds")
denue_med = readRDS("Datos/denue_med.rds")
denue_gra = readRDS("Datos/denue_gra.rds")
pal1 = readRDS("Datos/pal1.rds")
pal2 = readRDS("Datos/pal2.rds")
pal3 = readRDS("Datos/pal3.rds")

# 2. UI -----------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
library(sf)
library (leaflet)
library(DT)
library(dplyr)

# espcificar el puerto de la aplicacion
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# UI: interfaz de usuario

ui = dashboardPage(
  dashboardHeader(title = "Indicadores"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Empleo IMSS", tabName = "empleo", icon = icon("users")),
      menuItem("Unidades económicas INEGI", tabName = "ue", icon = icon("store"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      
# 2.1 UI empleo formal IMSS (*) ---------------------------------------------------
      
      tabItem(tabName = "empleo",
              
              fluidRow(
                valueBoxOutput("ta_nac_tot", width =12),
                valueBoxOutput("ta_var_men", width = 6),
                valueBoxOutput("ta_var_ano", width = 6)
              ),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo en el IMSS por cada 100 mil 
              habitantes de 15 años o más, marzo 2025",
                    status = "primary", width = 12,
                    leafletOutput("gra_emp_1"))
              ),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo, marzo 2025", status = "warning", width = 12,
                    plotlyOutput("gra_emp_2"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "warning",
                    radioButtons("boton1", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Variación mensual de los puestos de trabajo en niveles,
              marzo 2025", status = "success", width = 12,
                    plotlyOutput("gra_emp_3"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "success",
                    radioButtons("boton2", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Variacion anual de los puestos de trabajo en niveles,
              marzo 2025", status = "info", width = 12,
                    plotlyOutput("gra_emp_4"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "info",
                    radioButtons("boton3", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Variacion mensual de los puestos de trabajo en porcentaje,
               marzo 2025", status = "danger", width = 12,
                    plotlyOutput("gra_emp_5"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "danger",
                    radioButtons("boton4", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Variacion anual de los puestos de trabajo en porcentaje,
              marzo 2025", status = "warning", width = 12,
                    plotlyOutput("gra_emp_6"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "warning",
                    radioButtons("boton5", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              fluidRow(
                box(title = "Histórico de puestos de trabajo", status = "primary",
                    width = 12,
                    plotlyOutput("gra_emp_7"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "primary",
                    selectInput("boton6", "Entidad:",
                                choices = c("Aguascalientes", "Baja California", "Baja California Sur",
                                            "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                            "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                                            "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
                                            "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                                            "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                                            "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
                                            "Yucatán", "Zacatecas"),
                                selected = "Estado de México")
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo por sexo, marzo 2025",
                    status = "success",
                    width = 12,
                    plotlyOutput("gra_emp_8")
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo por edad, marzo 2025",
                    status = "info",
                    width = 12,
                    plotlyOutput("gra_emp_9")
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo por nivel salarial, marzo 2025",
                    status = "danger",
                    width = 12,
                    plotlyOutput("gra_emp_10")
                )),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Puestos de trabajo en el IMSS por
              cada 10 mil habitantes de 15 años o más, marzo 2025",
                    status = "warning", width = 12,
                    leafletOutput("gra_emp_11"))
              ),
              
              fluidRow(
                box(title = "Histórico de puestos de trabajo", status = "primary",
                    width = 12,
                    plotlyOutput("gra_emp_12"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "primary",
                    selectInput("boton7", "Municipio:",
                                choices = c("Acambay", "Acolman", "Aculco", "Amanalco", "Amatepec", "Amecameca", "Apaxco", "Atenco",
                                            "Atizapán", "Atizapán de Zaragoza", "Atlacomulco", "Atlautla", "Axapusco", "Ayapango",
                                            "Calimaya", "Capulhuac", "Chalco", "Chapa de Mota", "Chapultepec", "Chiautla", "Chicoloapan",
                                            "Chiconcuac", "Chimalhuacán", "Coacalco", "Coatepec Harinas", "Cocotitlán", "Coyotepec",
                                            "Cuautitlán", "Cuautitlán Izcalli", "Donato Guerra", "Ecatzingo", "Ecatepec", "El Oro", "Huehuetoca",
                                            "Hueypoxtla", "Huixquilucan", "Isidro Fabela", "Ixtapan de la Sal", "Ixtapan del Oro", "Ixtapaluca",
                                            "Ixtlahuaca", "Jaltenco", "Jilotzingo", "Jilotepec", "Jiquipilco", "Jocotitlán", "Joquicingo",
                                            "Juchitepec", "La Paz", "Lerma", "Luvianos", "Malinalco", "Melchor Ocampo", "Mexicaltzingo",
                                            "Metepec", "Morelos", "Naucalpan", "Nextlalpan", "Nezahualcóyotl", "Nicolás Romero", "Nopaltepec",
                                            "Ocoyoacac", "Ocuilan", "Otumba", "Otzoloapan", "Otzolotepec", "Ozumba", "Papalotla", "Polotitlán",
                                            "Rayón", "San Antonio la Isla", "San Felipe del Progreso", "San José del Rincón", "San Martín de las Pirámides",
                                            "San Mateo Atenco", "San Simón de Guerrero", "Santo Tomás", "Soyaniquilpan", "Sultepec", "Tecámac",
                                            "Tejupilco", "Temamatla", "Temascalapa", "Temascalcingo", "Temascaltepec", "Temoaya", "Tenancingo",
                                            "Tenango del Aire", "Tenango del Valle", "Teoloyucan", "Teotihuacán", "Tepetlaoxtoc",
                                            "Tepetlixpa", "Tepotzotlán", "Tequixquiac", "Texcalyacac", "Texcaltitlán", "Texcoco", "Tezoyuca", "Tianguistenco",
                                            "Tlalmanalco", "Tlalnepantla", "Tlatlaya", "Toluca", "Tonanitla", "Tonatico", "Valle de Bravo",
                                            "Valle de Chalco Solidaridad", "Villa de Allende", "Villa del Carbón", "Villa Guerrero", "Villa Victoria",
                                            "Xalatlaco", "Xonacatlán", "Zacazonapan", "Zacualpan", "Zinacantepec", "Zumpahuacán", "Zumpango"),
                                selected = "Acambay")
                ))
      ),
      
      
# 2.2. UI unidades economicas (*) ---------------------------------------
      
      tabItem(tabName = "ue",
              
              fluidRow(
                valueBoxOutput("ue_tot", width =12),
                valueBoxOutput("ue_var_sem", width = 6),
                valueBoxOutput("ue_var_ano", width = 6)
              ),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Unidades económicas por cada 100 mil habitantes de 15
              años o más, noviembre 2024",
                    status = "primary", width = 12,
                    leafletOutput("gra_ue_1"))
              ),
              
              fluidRow(
                ### CAMBIO MANUAL FECHAS
                box(title = "Unidades económicas, marzo 2025", status = "warning", width = 12,
                    plotlyOutput("gra_ue_2"))
              ),
              
              fluidRow(
                box(width = 5,
                    status = "warning",
                    radioButtons("boton8", "Ordenar por:",
                                 choices = c("Nombre" = "nombre",
                                             "Mayor a menor" = "desc",
                                             "Menor a mayor" = "asc"),
                                 selected = "nombre",
                                 inline = TRUE)
                )),
              
              # fluidRow(
              #   box(title = "Histórico de unidades económicas", status = "success",
              #       width = 12,
              #       plotlyOutput("gra_ue_3"))
              # ),
              # 
              # fluidRow(
              #   box(width = 5,
              #       status = "success",
              #       selectInput("boton9", "Municipio:",
              #                   choices = c("Acambay de Ruíz Castañeda", "Acolman", "Aculco", "Amanalco", "Amatepec", "Amecameca", "Apaxco", "Atenco",
              #                               "Atizapán", "Atizapán de Zaragoza", "Atlacomulco", "Atlautla", "Axapusco", "Ayapango",
              #                               "Calimaya", "Capulhuac", "Chalco", "Chapa de Mota", "Chapultepec", "Chiautla", "Chicoloapan",
              #                               "Chiconcuac", "Chimalhuacán", "Coacalco de Berriozábal", "Coatepec Harinas", "Cocotitlán", "Coyotepec",
              #                               "Cuautitlán", "Cuautitlán Izcalli", "Donato Guerra", "Ecatzingo", "Ecatepec de Morelos", "El Oro", "Huehuetoca",
              #                               "Hueypoxtla", "Huixquilucan", "Isidro Fabela", "Ixtapan de la Sal", "Ixtapan del Oro", "Ixtapaluca",
              #                               "Ixtlahuaca", "Jaltenco", "Jilotzingo", "Jilotepec", "Jiquipilco", "Jocotitlán", "Joquicingo",
              #                               "Juchitepec", "La Paz", "Lerma", "Luvianos", "Malinalco", "Melchor Ocampo", "Mexicaltzingo",
              #                               "Metepec", "Morelos", "Naucalpan de Juárez", "Nextlalpan", "Nezahualcóyotl", "Nicolás Romero", "Nopaltepec",
              #                               "Ocoyoacac", "Ocuilan", "Otumba", "Otzoloapan", "Otzolotepec", "Ozumba", "Papalotla", "Polotitlán",
              #                               "Rayón", "San Antonio la Isla", "San Felipe del Progreso", "San José del Rincón", "San Martín de las Pirámides",
              #                               "San Mateo Atenco", "San Simón de Guerrero", "Santo Tomás", "Soyaniquilpan de Juárez", "Sultepec", "Tecámac",
              #                               "Tejupilco", "Temamatla", "Temascalapa", "Temascalcingo", "Temascaltepec", "Temoaya", "Tenancingo",
              #                               "Tenango del Aire", "Tenango del Valle", "Teoloyucan", "Teotihuacán", "Tepetlaoxtoc",
              #                               "Tepetlixpa", "Tepotzotlán", "Tequixquiac", "Texcalyacac", "Texcaltitlán", "Texcoco", "Tezoyuca", "Tianguistenco",
              #                               "Tlalmanalco", "Tlalnepantla de Baz", "Tlatlaya", "Toluca", "Tonanitla", "Tonatico", "Valle de Bravo",
              #                               "Valle de Chalco Solidaridad", "Villa de Allende", "Villa del Carbón", "Villa Guerrero", "Villa Victoria",
              #                               "Xalatlaco", "Xonacatlán", "Zacazonapan", "Zacualpan", "Zinacantepec", "Zumpahuacán", "Zumpango"),
              #                   selected = "Acambay de Ruíz Castañeda")
              #   )),
              
              fluidRow(
                box(title = "Unidades económicas en el Estado de México por tamaño",
                    status = "info",
                    width = 12,
                    plotlyOutput("gra_ue_4")
                ))
      )
    )
  )
)

# 3. server ---------------------------------------------------------------

server = function(input, output, session) {
  
  # 3.1. mensaje inicial ----------------------------------------------------
  
  showModal(modalDialog(
    title = "¡Bienvenido!",
    "Sistema automatizado de seguimiento a los principales agregados económicos 
    del Estado de México y sus municipios. Diseñado para posibilitar al usuario 
    la visualización, análisis y descarga de datos de manera simultánea a su publicación. 
    El desarrollo de la plataforma se llevó a cabo mediante el lenguaje de 
    programación R; enfocándose en la integración de productos geoinformáticos.",
    easyClose = TRUE,
    footer = NULL
  ))
  
  # 3.1.1. server empleo formal IMSS ----------------------------------------
  
  output$ta_nac_tot = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(ta_nac_tot$mar_25, format = "d", big.mark = ","),
      ### CAMBIO MANUAL FECHAS
      "Puestos de trabajo a nivel nacional registrados en el IMSS, marzo 2025",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$ta_var_men = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(ta_nac_tot$var_men, format = "d", big.mark = ","),
      "Variación mensual",
      icon = icon("square-poll-vertical"),
      color = "green"
    )
  })
  
  output$ta_var_ano = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(ta_nac_tot$var_ano, format = "d", big.mark = ","),
      "Variación anual",
      icon = icon("chart-simple"),
      color = "yellow"
    )
  })
  
  output$gra_emp_1 = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = map_ent, fillColor = ~pal1(tas_ent), fillOpacity = 0.8,
                  color = "black", weight = 0.7, opacity = 0.6, 
                  label = ~paste(ESTADO)) %>% 
      addLegend(pal = pal1, values = map_ent$tas_ent, 
                title = "Tasa (intervalos iguales)",
                position = "bottomright", opacity = 1
      )
  })
  
  output$gra_emp_2 = renderPlotly({
    
    orden1 = switch(input$boton1,
                    "nombre" = ta_ent_tot %>% arrange(nom_ent),
                    "desc"   = ta_ent_tot %>% arrange(desc(mar_25)),
                    "asc"    = ta_ent_tot %>% arrange(mar_25))
    
    orden1$nom_ent = factor(orden1$nom_ent, levels = orden1$nom_ent)
    
    plot_ly(
      data = orden1,
      x = ~nom_ent,
      y = ~mar_25,
      type = "bar",
      marker = list(color = "#5dade2")
    ) %>%
      layout(
        yaxis = list(
          title = "Puestos de trabajo",
          tickformat = ",d"
        ),
        xaxis = list(title = "Entidad federativa")
      )
  })
  
  output$gra_emp_3 = renderPlotly({
    
    orden2 = switch(input$boton2,
                    "nombre" = ta_ent_tot %>% arrange(nom_ent),
                    "desc"   = ta_ent_tot %>% arrange(desc(var_men)),
                    "asc"    = ta_ent_tot %>% arrange(var_men))
    
    orden2$nom_ent = factor(orden2$nom_ent, levels = orden2$nom_ent)
    
    color1 = ifelse(orden2$var_men < 0, "#ec7063",
                    ifelse(orden2$var_men == 0, "#f9e79f",
                           "#7dcea0"))
    
    plot_ly(
      data = orden2,
      x = ~nom_ent,
      y = ~var_men,
      type = "bar",
      marker = list(color = color1)
    ) %>%
      layout(
        yaxis = list(
          title = "Variación en niveles",
          tickformat = ",d"
        ),
        xaxis = list(title = "Entidad federativa")
      )
  })
  
  output$gra_emp_4 = renderPlotly({
    
    orden3 = switch(input$boton3,
                    "nombre" = ta_ent_tot %>% arrange(nom_ent),
                    "desc"   = ta_ent_tot %>% arrange(desc(var_ano)),
                    "asc"    = ta_ent_tot %>% arrange(var_ano))
    
    orden3$nom_ent = factor(orden3$nom_ent, levels = orden3$nom_ent)
    
    color2 = ifelse(orden3$var_ano < 0, "#ec7063",
                    ifelse(orden3$var_ano == 0, "#f9e79f",
                           "#7dcea0"))
    
    plot_ly(
      data = orden3,
      x = ~nom_ent,
      y = ~var_ano,
      type = "bar",
      marker = list(color = color2)
    ) %>%
      layout(
        yaxis = list(
          title = "Variación en niveles",
          tickformat = ",d"
        ),
        xaxis = list(title = "Entidad federativa")
      )
  })
  
  output$gra_emp_5 = renderPlotly({
    
    orden4 = switch(input$boton4,
                    "nombre" = ta_ent_tot %>% arrange(nom_ent),
                    "desc"   = ta_ent_tot %>% arrange(desc(var_men_por)),
                    "asc"    = ta_ent_tot %>% arrange(var_men_por))
    
    orden4$nom_ent = factor(orden4$nom_ent, levels = orden4$nom_ent)
    
    color3 = ifelse(orden4$var_men_por < 0, "#ec7063",
                    ifelse(orden4$var_men_por == 0, "#f9e79f",
                           "#7dcea0"))
    
    plot_ly(
      data = orden4,
      x = ~nom_ent,
      y = ~var_men_por,
      type = "bar",
      marker = list(color = color3)
    ) %>%
      layout(
        yaxis = list(
          title = "Variación porcentual",
          tickformat = ".1%"
        ),
        xaxis = list(title = "Entidad federativa")
      )
  })
  
  output$gra_emp_6 = renderPlotly({
    
    orden5 = switch(input$boton5,
                    "nombre" = ta_ent_tot %>% arrange(nom_ent),
                    "desc"   = ta_ent_tot %>% arrange(desc(var_ano_por)),
                    "asc"    = ta_ent_tot %>% arrange(var_ano_por))
    
    orden5$nom_ent = factor(orden5$nom_ent, levels = orden5$nom_ent)
    
    color4 = ifelse(orden5$var_ano_por < 0, "#ec7063",
                    ifelse(orden5$var_ano_por == 0, "#f9e79f",
                           "#7dcea0"))
    
    plot_ly(
      data = orden5,
      x = ~nom_ent,
      y = ~var_ano_por,
      type = "bar",
      marker = list(color = color4)
    ) %>%
      layout(
        yaxis = list(
          title = "Variación porcentual",
          tickformat = ".1%"
        ),
        xaxis = list(title = "Entidad federativa")
      )
  })
  
  output$gra_emp_7 = renderPlotly({
    
    filtro1 = ta_ent_tot %>%
      filter(nom_ent == input$boton6)
    
    ### CAMBIO MANUAL FECHAS
    fecha1 = c("2025-ene", "2025-feb", "2025-mar")
    
    hist_emp1 = data.frame(
      Fecha1 = fecha1,
      ### CAMBIO MANUAL FECHAS
      Empleo1 = c(filtro1$ene_25, filtro1$feb_25, filtro1$mar_25)
    )
    
    plot_ly(data = hist_emp1,
            x = ~Fecha1,
            y = ~Empleo1,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#5dade2")) %>%
      layout(
        title = paste(input$boton6),
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Puestos de trabajo",
                     tickformat = ",d"),
        showlegend = FALSE
      )
    
  })
  
  output$gra_emp_8 = renderPlotly({
    fig1 = plot_ly(
      data = ta_ent_hom,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = 'Hombres',
      marker = list(color = "#5dade2")
    )
    
    fig1 = fig1 %>% add_trace(
      data = ta_ent_muj,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = 'Mujeres',
      marker = list(color = "#c39bd3")
    )
    
    fig1 %>% layout(
      barmode = 'stack',
      xaxis = list(title = "Entidad Federativa"),
      yaxis = list(title = "Puestos de trabajo",
                   tickformat = ",d")
    )
  })
  
  output$gra_emp_9 = renderPlotly({
    fig2 = plot_ly(
      data = ta_ent_ed1,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '15 a 34 años',
      marker = list(color = "#82e0aa")
    )
    
    fig2 = fig2 %>% add_trace(
      data = ta_ent_ed2,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '35 a 59 años',
      marker = list(color = "#85c1e9")
    )
    
    fig2 = fig2 %>% add_trace(
      data = ta_ent_ed3,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '60 o más años',
      marker = list(color = "#717d7e")
    )
    
    fig2 %>% layout(
      barmode = 'stack',
      xaxis = list(title = "Entidad Federativa"),
      yaxis = list(title = "Puestos de trabajo",
                   tickformat = ",d")
    )
  })
  
  output$gra_emp_10 = renderPlotly({
    fig3 = plot_ly(
      data = ta_ent_sm1,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '1 a 3 salarios mínimos',
      marker = list(color = "#f9e79f")
    )
    
    fig3 = fig3 %>% add_trace(
      data = ta_ent_sm2,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '4 a 6 salarios mínimos',
      marker = list(color = "#76d7c4")
    )
    
    fig3 = fig3 %>% add_trace(
      data = ta_ent_sm3,
      x = ~nom_ent,
      y = ~mar_25,
      type = 'bar',
      name = '7 o más salarios mínimos',
      marker = list(color = "#cd6155")
    )
    
    fig3 %>% layout(
      barmode = 'stack',
      xaxis = list(title = "Entidad Federativa"),
      yaxis = list(title = "Puestos de trabajo",
                   tickformat = ",d")
    )
  })
  
  output$gra_emp_11 = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      ### CAMBIO MANUAL FECHAS
      addPolygons(data = map_mun, fillColor = ~pal2(tasa), fillOpacity = 0.8,
                  color = "black", weight = 0.7, opacity = 0.6,
                  label = ~paste(nom_mun)) %>%
      ### CAMBIO MANUAL FECHAS
      addLegend(pal = pal2, values = map_mun$tasa,
                title = "Tasa (cuantiles)",
                position = "bottomright", opacity = 1
      )
  })
  
  output$gra_emp_12 = renderPlotly({
    
    filtro2 = ta_mun_tot %>%
      filter(nom_mun == input$boton7)
    
    ### CAMBIO MANUAL FECHAS
    fecha2 = c("2025-ene", "2025-feb", "2025-mar")
    
    hist_emp2 = data.frame(
      Fecha2 = fecha2,
      ### CAMBIO MANUAL FECHAS
      Empleo2 = c(filtro2$ene_25, filtro2$feb_25, filtro2$mar_25)
    )
    
    plot_ly(data = hist_emp2,
            x = ~Fecha2,
            y = ~Empleo2,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#5dade2")) %>%
      layout(
        title = paste(input$boton7),
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Puestos de trabajo",
                     tickformat = ",d"),
        showlegend = FALSE
      )
    
  })
  
  # 3.1.2. server unidades economicas ----------------------------------------------
  
  output$ue_tot = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(denue_cifras_ent$ue_nov24, format = "d", big.mark = ","),
      ### CAMBIO MANUAL FECHAS
      "Unidades económicas en el Estado de México, noviembre 2024",
      icon = icon("cash-register"),
      color = "aqua"
    )
  })
  
  output$ue_var_sem = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(denue_cifras_ent$var_sem, format = "d", big.mark = ","),
      "Variación semestral",
      icon = icon("cart-shopping"),
      color = "green"
    )
  })
  
  output$ue_var_ano = renderValueBox({
    valueBox(
      ### CAMBIO MANUAL FECHAS
      formatC(denue_cifras_ent$var_ano, format = "d", big.mark = ","),
      "Variación anual",
      icon = icon("credit-card"),
      color = "yellow"
    )
  })
  
  output$gra_ue_1 = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = map_mun_ue, fillColor = ~pal3(tasa), fillOpacity = 0.8,
                  color = "black", weight = 0.7, opacity = 0.6,
                  label = ~paste(nom_mun)) %>%
      addLegend(pal = pal3, values = map_mun_ue$tasa,
                title = "Tasa (cuantil)",
                position = "bottomright", opacity = 1
      )
  })
  
  output$gra_ue_2 = renderPlotly({
    
    orden6 = switch(input$boton8,
                    "nombre" = denue_cifras_mun %>% arrange(nom_mun),
                    "desc"   = denue_cifras_mun %>% arrange(desc(ue)),
                    "asc"    = denue_cifras_mun %>% arrange(ue))
    
    orden6$nom_mun = factor(orden6$nom_mun, levels = orden6$nom_mun)
    
    plot_ly(
      data = orden6,
      x = ~nom_mun,
      y = ~ue,
      type = "bar",
      marker = list(color = "#5dade2")
    ) %>%
      layout(
        yaxis = list(
          title = "Unidades económicas",
          tickformat = ",d"
        ),
        xaxis = list(title = "Municipio")
      )
  })
  
  # output$gra_ue_3 = renderPlotly({
  #   
  #   filtro3 = denue %>%
  #     filter(municipio == input$boton9)
  #   
  #   hist_ue = filtro3 %>%
  #     group_by(fecha) %>%
  #     summarise(ue = n()) %>%
  #     ungroup()
  #   
  #   plot_ly(data = hist_ue,
  #           x = ~fecha,
  #           y = ~ue,
  #           type = "scatter",
  #           mode = "lines+markers",
  #           line = list(color = "#5dade2")) %>%
  #     layout(
  #       title = paste("Histórico de unidades económicas en", input$boton9),
  #       xaxis = list(title = "Mes"),
  #       yaxis = list(title = "Unidades económicas",
  #                    tickformat = ",d"),
  #       showlegend = FALSE
  #     )
  #   
  # })
  
  hist_ue_mic = denue_mic %>%
    group_by(fecha) %>%
    summarise(ue = sum(cve_ent, na.rm = TRUE)) %>%
    ungroup()
  
  hist_ue_peq = denue_peq %>%
    group_by(fecha) %>%
    summarise(ue = sum(cve_ent, na.rm = TRUE)) %>%
    ungroup()
  
  hist_ue_med = denue_med %>%
    group_by(fecha) %>%
    summarise(ue = sum(cve_ent, na.rm = TRUE)) %>%
    ungroup()
  
  hist_ue_gra = denue_gra %>%
    group_by(fecha) %>%
    summarise(ue = sum(cve_ent, na.rm = TRUE)) %>%
    ungroup()
  
  output$gra_ue_4 = renderPlotly({
    
    fig4 = plot_ly(
      data = hist_ue_mic,
      x = ~fecha,
      y = ~ue,
      type = 'bar',
      name = 'Micro empresa',
      marker = list(color = "#e6b0aa")
    )
    
    fig4 = fig4 %>% add_trace(
      data = hist_ue_peq,
      x = ~fecha,
      y = ~ue,
      type = 'bar',
      name = 'Pequeña empresa',
      marker = list(color = "#5dade2")
    )
    
    fig4 = fig4 %>% add_trace(
      data = hist_ue_med,
      x = ~fecha,
      y = ~ue,
      type = 'bar',
      name = 'Mediana empresa',
      marker = list(color = "#76d7c4")
    )
    
    fig4 = fig4 %>% add_trace(
      data = hist_ue_gra,
      x = ~fecha,
      y = ~ue,
      type = 'bar',
      name = 'Gran empresa',
      marker = list(color = "#f5b041")
    )
    
    fig4 = fig4 %>% layout(
      barmode = 'stack',
      xaxis = list(title = "Fecha"),
      yaxis = list(title = "Unidades económicas", tickformat = ",d")
    )
    
  })
  
  
}

# ejecutar la aplicacion shiny
shinyApp(ui = ui, server = server)

################################################
# Proyecto: MarcaPatrones                      #
#           Herramiento para manualmente       #
#           marcar eventos de incubacion       #
#           con series de tiempo               #
#           de temperatura con ruido de        #
#           HOBO dataloggers                   #
# Autores: Justin Baldwin,                     #
#       Mario Agustin Loaiza Muñoz,            #
#       Gustavo Adolfo Londoño                 #
# Fechas: Diciembre 2017-Enero 2018            #
################################################

shinyUI(fluidPage(
  #for reset button
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode),                      # Add the js code to the page
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ), #turns off warnings

  # Application title
  titlePanel("Temperatura"),

  # Sidebar with a file selector
  sidebarLayout(
    sidebarPanel(


      fluidRow(
       selectInput("archivo",
                   "Seleccione Archivo",
                   choices = archivos
                   )),

      fluidRow(h3("Indice cual columna contiene cual informacion (0 si esta ausente)"),
        box(width=12, title ="" ,
             splitLayout(
       numericInput("col_fecha", "Fecha", min=0, max=10, value = 1, width = '50px'),
       numericInput("col_hora", "Hora", min=0, max=10, value = 2, width = '50px'),
       numericInput("col_huevo", "Huevo", min=0, max=10, value = 0, width = '50px'),
       numericInput("col_nido", "Nido", min=0, max=10, value = 3, width ='50px'),
       numericInput("col_amb", "Ambiental", min=0, max=10, value = 4, width = '50px')
       ))
      ),
      actionButton("load", "Carga los datos"),

      fluidRow(
       sliderInput("ylim", "Limite de Y", min=0, max=100, value = c(10,40))
       )
      ,
conditionalPanel(condition="input.tabs=='manual'",
      HTML(
      rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(#replacewith dynamic ui
        #uiOutput("event_class_control"),
        actionButton("switch_event", "Cambia el evento marcado"),
        verbatimTextOutput("monitor_evt_class")


      )
      ,
      HTML(
        rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(
        uiOutput("fitcontrols")

        ),



      ###Init helper


      conditionalPanel(
        "output.event_class=='1' && input.tabs=='manual'"
        ,
        fluidRow(

          checkboxGroupInput("off_models", "Cuales modelos quieres correr?",choiceValues = models, choiceNames = models_labels, selected = models),


          numericInput("umbral_off", "Umbral off", 10, min=0, max=100)
          ,numericInput("init_newton_off_a", "Initial parameter a", -0.5, min=-10, max=10),
          numericInput("init_newton_off_T_s", "Initial parameter T_s", 10, min=--10, max=50)

          ))
      ,
      conditionalPanel(
        "output.event_class=='2'&& input.tabs=='manual'"
        ,
        fluidRow(

          checkboxGroupInput("on_models", "Cuales modelos quieres correr?",choiceValues = models, choiceNames = models_labels, selected = models),

          numericInput("umbral_on","Umbral On",  10, min=0, max=100)
           ,numericInput("init_newton_on_a", "Initial parameter a", -0.7, min=-10, max=10),
           numericInput("init_newton_on_T_s", "Initial parameter T_s", 10, min=--10, max=50)
        ))
      #,

),
########################################################################################################################################################
conditionalPanel(condition="input.tabs=='cpa'",
                 uiOutput("fitcontrols_cpa"),
                 numericInput("movingaverage_width", "Ventana del Promedio", value = 10, min = 1,max=500),
                 radioButtons("cpa_meth", "Metodologia de CPA", choices = c("PELT", "PELT Manual", "BinSeg"), selected = "PELT"),
                 sliderInput("cpa_max_dif_on", "Maximum Difference: On-Bout", c(1,3), min = 0,max=15, round=-3, step=0.001),
                 sliderInput("cpa_max_dif_off", "Maximum Difference: Off-Bout", c(-3, -1), min = -15,max=0, round=-3, step=0.001)

                 #breakpoint controls             #1plot of time series with breakpoints, zoomable w brush
                 #
                 #move selected breakpoints       #2zoom window with removeable breakpoints (dblclick)
                 #move y position of breakpoints  #select breakpoints to move
                 #add breakpoints (single click)
                 #generate fits from breakpoints
                 #fit controls (umbrals) #window that shows marked_rects
                 #for each event, is slope negative -> fit off, positive -> fit on

                 ),
HTML(
  rep(c("<br>" ,"</br>"),10)
),

dateInput("day_zero", "Cuando es el dia 0? (si es desconocido dejalo vacio)", value = NA)
      ),

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

    # Show a plot of the selected points
    mainPanel(
      tabsetPanel(


        tabPanel("Manual",

                 h3("La tabla cruda"),

                 verbatimTextOutput("header"),

                 h3("La serie de tiempo"),
                  dataTableOutput("obs_raw"),

                 plotOutput("plot1", width=1250,
                            brush = brushOpts(
                              id = "plot1_brush", direction = "x"
                            )),

                 fluidRow(
                   column(width=12,

                          h3("Marca un evento"),
                          uiOutput("plotui")

                   )
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("fit_plot1", width=600, height = 600)
                   ),
                   column(6,
                          plotOutput("evento_marcado", width=500, height = 300),
                          plotOutput("fit_plot2", width=500, height = 300)
                   )


                 ),
                 fluidRow(actionButton("mark_pattern", "Marca el patron")),

                 fluidRow(

                   plotOutput("plot_final", width=1250)

                 ),

                 fluidRow(verbatimTextOutput("monitor_markedrects")),


                 fluidRow(column(10,
                                 actionButton("pre_save_raw", "Guardar para reanudar ahorita"),
                                 fileInput("pre_saved_file", "Seleccional archivo",
                                           accept = c("txt")
                                 ),
                                 actionButton("resume", "Reanudar")),
                          column(2,
                          actionButton("save_raw", "Guarda el archivo final"))),
                 #fluidRow(verbatimTextOutput("monitor_preloaded")),




                 value="manual"

        ),
        ###############################################################################################
        tabPanel("CPA",
                 h3("La tabla cruda"),

                 verbatimTextOutput("header_cpa"),

                 h3("La serie de tiempo"),

          plotOutput("cpa_plot1", width=1250,
                     brush = brushOpts(
                       id = "cpa_plot1_brush", direction = "x"
                     )),

          fluidRow(
            column(width=12,

                   h3("Marca un evento"),
                   uiOutput("cpa_plotui")

            )
          ),

         dygraphOutput("cpa_plot2_dygraph"),
          verbatimTextOutput("cpatab"),

      #breakpoint controls             #1plot of time series with breakpoints, zoomable w brush
      #
      #move selected breakpoints       #2zoom window with removeable breakpoints (dblclick)
      #move y position of breakpoints  #select breakpoints to move
                                       #add breakpoints (single click)
      #generate fits from breakpoints
      #fit controls (umbrals) #window that shows marked_rects
                              #for each event, is slope negative -> fit off, positive -> fit on
                 value="cpa"),

      selected = "manual", id="tabs")


    )
  )
))

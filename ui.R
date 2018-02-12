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
      fluidRow(
       radioButtons("sep", "Cual es el separador?", choices=c(",", ";"),
                          selected=c(";"))
      ),
      fluidRow(h3("Indice cual columna contiene cual informacion (0 si esta ausente)"),
        box(width=12, title ="" , 
             splitLayout(
       numericInput("col_fecha", "Fecha", min=0, max=10, value = 1, width = '50px'),
       numericInput("col_hora", "Hora", min=0, max=10, value = 2, width = '50px'),
       numericInput("col_huevo", "Huevo", min=0, max=10, value = 3, width = '50px'),
       numericInput("col_nido", "Nido", min=0, max=10, value = 4, width ='50px'),
       numericInput("col_amb", "Ambiental", min=0, max=10, value = 5, width = '50px')
       ))
      ),
      actionButton("load", "Carga los datos"),
      dateInput("day_zero", "Cuando es el dia 0? (dejala vacio si desconocido)", value = NA),
      fluidRow(
       sliderInput("ylim", "Limite de Y", min=0, max=100, value = c(10,40))
       )
      ,
      HTML(
      rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(
       radioButtons("event_class", "Cual tipo de evento es?",
                    choiceNames =   events,
                    choiceValues = c(1:2)
                    )
      )
      ,
      HTML(
        rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(
        radioButtons("fit_selector", "Que fit quieres correr?",
                     choices =  c("nido", "huevo")
        )
      )
      ,
      
      ###Init helper
      
      
      conditionalPanel(
        "input.event_class=='1'"
        ,
        fluidRow(
          numericInput("umbral_off", "umbral_off", 0.1, min=0, max=3),
          
          numericInput("init_newton_off_a", "init_newton_off_a", -0.5, min=-1000, max=1000)
          ))
      ,
      conditionalPanel(
        "input.event_class=='2'"
        ,
        fluidRow(
          numericInput("umbral_on", "umbral_on", 0.1, min=0, max=3),
          
          numericInput("init_newton_on_a", "init_newton_on_a", -0.7, min=-1000, max=1000),
          numericInput("init_newton_on_T_s", "init_newton_on_T_s", 10, min=-10000, max=10000)
        ))
      #,
    
      
      ),
      
    
    # Show a plot of the selected points
    mainPanel(
      
      h3("La tabla cruda"),
      
      verbatimTextOutput("header"),
      
       h3("La serie de tiempo"),
      
    #dataTableOutput("obs_raw"),
       
       plotOutput("plot1", width=1250,
                  brush = brushOpts(
                    id = "plot1_brush", direction = "x"
                  )),
      
      fluidRow(
        column(width=12,
      
      h3("Seleccione el tipo y marca un evento"),
      uiOutput("plotui")

      )
      ),
      fluidRow(
        column(8,
        plotOutput("evento_marcado", width=800, height = 800)
        ),
        column(4,
        plotOutput("fit_plot1", width=400),        
        plotOutput("fit_plot2", width=400)
        )

        
      ),
      fluidRow(actionButton("mark_pattern", "Marca el patron")),
      
      fluidRow(
        
        plotOutput("plot_final", width=1250)
        
      )
      
      
      ,
      fluidRow(actionButton("save_raw", "Guarda el archivo"))
      
    )
  )
))

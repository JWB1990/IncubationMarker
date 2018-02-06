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
  
  # Application title
  titlePanel("Temperatura"),
  
  # Sidebar with a file selector
  sidebarLayout(
    sidebarPanel(
      fluidRow(
       selectInput("archivo",
                   "Seleccione Archivo",
                   choices = archivos
                   )
      ),
      HTML(
      rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(
       radioButtons("event_class", "Cual tipo de evento es?",
                    choiceNames =   events,
                    choiceValues = c(1:2)
                    )
      ),
      HTML(
        rep(c("<br>" ,"</br>"),10)
      ),
      fluidRow(
        radioButtons("fit_selector", "Que fit quieres correr?",
                     choices =  c("nido", "huevo")
        )
      ),
      
      ###Init helper
      
      
      conditionalPanel(
        "input.event_class=='1'"
        ,
        fluidRow(
          numericInput("umbral_off", "umbral_off", 0.5, min=0, max=3),
          
          numericInput("init_newton_off_a", "init_newton_off_a", -0.5, min=-1000, max=1000)
          ))
      ,
      conditionalPanel(
        "input.event_class=='2'"
        ,
        fluidRow(
          numericInput("umbral_on", "umbral_on", 0.5, min=0, max=3),
          
          numericInput("init_newton_on_a", "init_newton_on_a", -0.7, min=-1000, max=1000),
          numericInput("init_newton_on_T_s", "init_newton_on_T_s", 10, min=-10000, max=10000)
        ))
      #,
      # #Only for Double Logistic
      # conditionalPanel(
      #   "input.event_class=='3'"
      #   ,
      #   fluidRow(
      #     numericInput("init_logistic_dbl_A", "init_logistic_dbl_A", 10, min=-1000, max=1000),
      #     numericInput("init_logistic_dbl_mu", "init_logistic_dbl_mu", 4, min=-10000, max=10000),
      #     numericInput("init_logistic_dbl_lambda", "init_logistic_dbl_lambda", 2, min=-10000, max=10000)
      #   ))
      
      ),
      
    
    # Show a plot of the selected points
    mainPanel(
       h3("La serie de tiempo"),
      # textOutput("all_cols"),
    
       
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

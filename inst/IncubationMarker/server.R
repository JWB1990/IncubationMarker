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


shinyServer(function(input, output) {
  
  
  
  datos_crudos_pre <- reactive({
    cru=read.table(paste("hobo", "/", input$archivo, sep = ""), header = T, stringsAsFactors = F, row.names = NULL)
    cru$patron<-c(NA)
    if("row.names" %in% names(cru)){
      names(cru)<-c(names(cru)[2:ncol(cru)], paste0("X", ncol(cru)))
    }
  
    cru
  })
  
  output$header<-renderPrint({
    o=datos_crudos_pre()
    rbind(names(o))
  })
    
    datos_crudos<-eventReactive(input$load, {
        
      
      cru<-datos_crudos_pre()
      if(input$col_fecha!=0){names(cru)[input$col_fecha]<-"fecha"}
      if(input$col_hora!=0){names(cru)[input$col_hora]<-"hora"}
      if(input$col_huevo!=0){names(cru)[input$col_huevo]<-"huevo"
      cru$huevo<-as.numeric(gsub(cru$huevo, pattern = ",", replacement = ".", fixed = T))}
      if(input$col_nido!=0){names(cru)[input$col_nido]<-"nido"
      cru$nido<-as.numeric(gsub(cru$nido, pattern = ",", replacement = ".", fixed = T))}
      if(input$col_amb!=0){names(cru)[input$col_amb]<-"amb"
      cru$amb<-as.numeric(gsub(cru$amb, pattern = ",", replacement = ".", fixed = T))}
      
      cru$hora<-substr(cru$hora, 1, 8)
      
  #asegura que los formatos estan corectos  
  cru$ts<-parse_date_time(paste(cru$hora, cru$fecha), orders = c("HMS mdy", "HMS mdY"))
  cru
  })
  observeEvent(input$load, {
    isolate({
      d<-datos_crudos()
      empty_df<-d[-(1:nrow(d)),]
      values$all_datos_marcados <-  empty_df #this is the empty data frame for nests with 2 sensors
      
    })
  })  
    # output$obs_raw<-renderDataTable({
    #   head(values$all_datos_marcados)
    #   })
  datos<-reactive({
  #cambia la tabla formato larga 
    datos_crudos() %>% gather("sensor", "temperatura", which(names(datos_crudos()) %in% c("huevo", "nido", "amb")))
  })
  
  
  
 
   
  
  output$plot1 <- renderPlot({
    cr<-datos()
    ggplot(data=cr,
           aes(x=ts, y=temperatura, colour=sensor))+geom_line()+
      scale_x_datetime(breaks = seq(floor_date(min(cr$ts), unit = "days"),floor_date(max(cr$ts), unit = "days"), 
                                                                                              by="12 hours"))+
      ylim(input$ylim)+theme(legend.position="left")
   
  })
  
  #get brushed points of first viewer
  bpts <- reactive({
    validate(
      need(is.null(input$plot1_brush) == FALSE, "Selecciona unos puntos")
    )
    brushedPoints(datos(), input$plot1_brush)
    })
  
  #get single event
  output$plot2 <- renderPlot({
    points2<-bpts()
    
    
    ggplot(data=bpts(),
           aes(x=ts, y=temperatura, colour=sensor))+geom_line()+
      scale_x_datetime(breaks = date_breaks("15 min"), 
                       minor_breaks=date_breaks("1 min"), labels=date_format("%H:%M:%S"))+
      ylim(input$ylim)
    
    
    
  })
  
  output$plotui <- renderUI({
    plotOutput("plot2",  
               brush = brushOpts(id = "plot2_brush", direction = "x",
                                 fill = "red", opacity = 0.2)
    )
  })
  
  bpts2 <- reactive({
    validate(
      need(is.null(input$plot2_brush) == FALSE, "Selecciona un evento")
    )
    brushedPoints(bpts(), input$plot2_brush)
  })
  
  #make reactive fit choice
  output$fitcontrols <- renderUI({
    avail<-names(datos_crudos())[names(datos_crudos()) %in% c("nido", "huevo")]
    radioButtons("fit_selector", "Que fit quieres correr?",
                 choices =  avail)
  })
  
  
  T_amb_event <- reactive({
    selpoints<-bpts2()
    
    data_to_fit<-data_to_fit()  
    mean(selpoints$temperatura[selpoints$sensor=="amb"], na.rm = T)-min(data_to_fit$temperatura)
    
  })
  
  min_sel_event<-reactive({
    m<-bpts2()
    min(m$ts)
  })
  max_sel_event<-reactive({
    n<-bpts2()
    max(n$ts)
  })
  
  output$print_final <- renderPrint({
    c(min_sel_event(), max_sel_event())
  })
  
  # observer event
  datos_marcados<-eventReactive(input$mark_pattern, {
       d<-datos_crudos() 
       #T_s from mean amb temp
       data_to_fit<-data_to_fit() 
       
         if(input$event_class=="2") {
           
           
           
           
           d<-d[d$ts >= min_sel_event() & d$ts <= max_sel_event(),]
           d$patron<-"warming"
           if(ceiling(t_at_half_from_T_s_on())<nrow(d)){
              d$patron[ceiling(t_at_half_from_T_s_on()):nrow(d)]<-"on_asymptote"}
           d$event_number<-0
           
           fits<-fits()
           on_newton_pars<-fits$cool_newton_on$m$getAllPars()
           d$alpha<- as.numeric(on_newton_pars[names(on_newton_pars)=="a"])
           d$T_s<- as.numeric(on_newton_pars[names(on_newton_pars)=="T_s"])+min(data_to_fit$temperatura)
           
         } else {
           
           d<-d[d$ts >= min_sel_event() & d$ts <= max_sel_event(),]
           d$patron<-"cooling"
           if(ceiling(t_at_half_from_T_s_off())<nrow(d)){
             d$patron[ceiling(t_at_half_from_T_s_off()):nrow(d)]<-"off_asymptote"}
           d$event_number<-0
           
           
           fits<-fits()
           #alpha from newton fit
           off_newton_pars<-fits$cool_newton_off$m$getAllPars()
           d$alpha<- as.numeric(off_newton_pars[names(off_newton_pars)=="a"])
            
           d$T_s<- T_amb_event()+min(data_to_fit$temperatura)
           
         }
       
       
       d
  })
  

    values<- reactiveValues()
    
  #aca viven los datos crudos marcados
  

  new_entry<-observeEvent(input$mark_pattern, {
    res<-rbind(
      values$all_datos_marcados, 
      datos_marcados() 
      )
    #allow for fuckups: when ppl select the wrong thing, they
    #can select it again and overwrite it
    #so first rbind, then retain only last etnries for each timestamp
    
    res<-res[!rev(duplicated(rev(res$ts))),]
    
    values$all_datos_marcados <- res
    
  })
  
  marked_rects<-reactive({
    
    values$all_datos_marcados$event_number <-   rep(seq_along(rle(values$all_datos_marcados$patron)$lengths),
                                                    rle(values$all_datos_marcados$patron)$lengths)
    
    values$all_datos_marcados %>% 
    group_by(event_number) %>% summarise(start=min(ts),
                                         end=max(ts),
                                         patron=patron[1],
                                         temperatura=20, 
                                         sensor="nido")
  })
  
  # 
  output$evento_marcado <- renderPlot({
    
    ggplot()+geom_point(data=bpts2() %>% filter(sensor==input$fit_selector),
                        aes(x=ts, y=temperatura, colour=sensor), size=4)+
      geom_point(data=bpts2() %>% filter(sensor!=input$fit_selector),
                 aes(x=ts, y=temperatura, colour=sensor), alpha=0.6, size=2)
       
    

    
  })
  
  ############################################################ fits ################################################################
  
  data_to_fit<-reactive({
    bpts2() %>% filter(sensor==input$fit_selector)
  })
  
  #normalize x and y to start at zero
  x<-reactive({
    data_to_fit<-data_to_fit()  
    ( as.numeric(data_to_fit$ts)-min(as.numeric(data_to_fit$ts)) )/60
  })
  y<-reactive({
    data_to_fit<-data_to_fit()  
    data_to_fit$temperatura-min(data_to_fit$temperatura)
    #data_to_fit$temperatura
  })


cool_newton_off<-reactive({
  mFunction = function(x, params) {
    # params model parameters, 
    a = params[["a"]]
    #T_s = params[["T_s"]]
    
    return( T_amb_event() - (T_amb_event() - max(y()))*exp(a*x))
  }
  # 2) Name
  mName = "cool_newton_off"
  # 3) Formula
  mFormula = y ~ T_amb_event() - (T_amb_event() - max(y()))*exp(a*x)
  # 4) Model parameters
  mParams = c("a")
  # 5) List of starting values for the parameters
  mStarting = list(a = -0.5)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})

cool_newton_off_noamb<-reactive({
  mFunction = function(x, params) {
    # params model parameters, 
    a = params[["a"]]
    T_s = params[["T_s"]]
    
    return( T_s - (T_s - max(y()))*exp(a*x))
  }
  # 2) Name
  mName = "cool_newton_off_noamb"
  # 3) Formula
  mFormula = y ~ T_s - (T_s - max(y()))*exp(a*x)
  # 4) Model parameters
  mParams = c("a", "T_s")
  # 5) List of starting values for the parameters
  mStarting = list(a = -0.5,
                   T_s=20)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})


cool_newton_on<-reactive({
  mFunction = function(x, params) {
    # params model parameters, 
    a = params[["a"]]
    T_s = params[["T_s"]]
    
    return( T_s - (T_s - min(y()))*exp(a*x))
  }
  # 2) Name
  mName = "cool_newton_on"
  # 3) Formula
  mFormula = y ~ T_s - (T_s - min(y()))*exp(a*x)
  # 4) Model parameters
  mParams = c("a", "T_s")
  # 5) List of starting values for the parameters
  mStarting = list(a = -0.7, 
                   T_s=10)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})


O2_poly_on<-reactive({
mFunction = function(x, params) {
  # params model parameters, a1, a2
  a1 = params[["a1"]]
  a2 = params[["a2"]]
  return(a1 * x + a2 * x^2)
}
# 2) Name
mName = "2nd-order polynomial-on"
# 3) Formula
mFormula = y ~ a1 * x + a2 * x^2
# 4) Model parameters
mParams = c("a1", "a2")
# 5) List of starting values for the parameters
mStarting = list(a1 = 1, a2 = 0.5)
# Create the customModel object
buildModel(mFunction, mName, mFormula, mParams, mStarting)
})


O3_poly_on<-reactive({
  mFunction = function(x, params) {
    # params model parameters, a1, a2, a3
    a1 = params[["a1"]]
    a2 = params[["a2"]]
    a3 = params[["a3"]]
    
    return(a1 * x + a2 * x^2+a3 * x^3)
    
  }
  # 2) Name
  mName = "3rd-order polynomial-on"
  # 3) Formula
  mFormula = y ~ a1 * x + a2 * x^2+ a3 * x^3
  # 4) Model parameters
  mParams = c("a1", "a2", "a3")
  # 5) List of starting values for the parameters
  mStarting = list(a1 = 1, a2 = 0.5, a3=0.5)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})

O2_poly_off<-reactive({
  mFunction = function(x, params) {
    # params model parameters, a1, a2
    a1 = params[["a1"]]
    a2 = params[["a2"]]
    return(max(y())+a1 * x + a2 * x^2)
  }
  # 2) Name
  mName = "2nd-order polynomial-off"
  # 3) Formula
  mFormula = y ~ max(y())+a1 * x + a2 * x^2
  # 4) Model parameters
  mParams = c("a1", "a2")
  # 5) List of starting values for the parameters
  mStarting = list(a1 = 1, a2 = 0.5)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})


O3_poly_off<-reactive({
  mFunction = function(x, params) {
    # params model parameters, a1, a2, a3
    a1 = params[["a1"]]
    a2 = params[["a2"]]
    a3 = params[["a3"]]
    
    return(max(y())+a1 * x + a2 * x^2+a3 * x^3)
    
  }
  # 2) Name
  mName = "3rd-order polynomial-off"
  # 3) Formula
  mFormula = y ~ max(y())+a1 * x + a2 * x^2+ a3 * x^3
  # 4) Model parameters
  mParams = c("a1", "a2", "a3")
  # 5) List of starting values for the parameters
  mStarting = list(a1 = 1, a2 = 0.5, a3=0.5)
  # Create the customModel object
  buildModel(mFunction, mName, mFormula, mParams, mStarting)
})


  fits<-reactive({
    
    ##################
    #define subsets of models to be run with switch
    models_on = getModelLibrary()[c("linearFit")]
    models_on[["O2_poly_on"]] <- O2_poly_on()
    models_on[["O3_poly_on"]] <- O3_poly_on()
    models_on[["cool_newton_on"]] <- cool_newton_on() 
    
    
    
    models_off = getModelLibrary()[c("linearFit")]
    models_off[["O2_poly_off"]] <- O2_poly_off()
    models_off[["O3_poly_off"]] <- O3_poly_off()
    models_off[["cool_newton_off"]] <- cool_newton_off()
    
    
    models_off_noamb = getModelLibrary()[c("linearFit")]
    models_off_noamb[["O2_poly_off"]] <- O2_poly_off()
    models_off_noamb[["O3_poly_off"]] <- O3_poly_off()
    models_off_noamb[["cool_newton_off_noamb"]] <- cool_newton_off_noamb()
    
    if(input$event_class==2){
      return(fitModels(models_on , x(), y()))
    } else if(input$event_class==1 & input$col_amb!=0){
        return(fitModels(models_off , x(), y()))
    } else if(input$event_class==1 & input$col_amb==0){
      return(fitModels(models_off_noamb , x(), y()))
    }
    })
  
################ this determines the spot at which the threshold is reached
  t_at_half_from_T_s_on<-reactive({


    if(input$event_class!="2"){0}
    else{

      fits<-fits()
      on_newton_pars<-fits$cool_newton_on$m$getAllPars()
      T_s<-as.numeric(on_newton_pars[names(on_newton_pars)=="T_s"])
      a<-as.numeric(on_newton_pars[names(on_newton_pars)=="a"])
      
      log((input$umbral_on/100*T_s)/(T_s-0))/a

    }
  })
  
  t_at_half_from_T_s_off<-reactive({
    
    
    if(input$event_class!="1"){0}
    else if(input$col_amb!=0){
      fits<-fits()
      off_newton_pars<-fits$cool_newton_off$m$getAllPars()
      a<-as.numeric(off_newton_pars[names(off_newton_pars)=="a"])
      log((-input$umbral_off/100*max(y()))/(T_amb_event()-max( y() )))/a
    } else if(input$col_amb==0){
      fits<-fits()
      off_newton_pars<-fits$cool_newton_off_noamb$m$getAllPars()
      a<-as.numeric(off_newton_pars[names(off_newton_pars)=="a"])
      T_s<-as.numeric(off_newton_pars[names(off_newton_pars)=="T_s"])
      
      log((-input$umbral_off/100*max(y()))/(T_s-max( y() )))/a
    }
  })

  #this plots the fits
  output$fit_plot1 <- renderPlot({
    fs<-fits()
    all_converged<-all(sapply(fs[-length(fs)], FUN=function(x){x$convInfo$isConv}))
    validate(need(all_converged==T, "Unas curvas no convergieron. Evento correcto?"))
    #make this better 
    #use singluar gradient estimate warning
    #make the points toggleable
    
    if(input$event_class=="2"){
      plot(fits())
      abline(v=t_at_half_from_T_s_on(), lty=2)
    } else {
      plot(fits())
      abline(v=t_at_half_from_T_s_off(), lty=2)
      
    }
  
    
    
  
  })
  #this plots the fit results
  #kind of unneccesary
  
  output$fit_plot2 <- renderPlot({
    
  weights = calculateAIC(fits())
  plot(weights)
  
  })
  #   
  #   
  # 
  # 
  
  
  #
  #final plot at bottom to check
  #crece con cada patron marcado
  output$plot_final <- renderPlot({
    
    validate(
      need(is.null(datos_marcados()) == FALSE & nrow(marked_rects())>0, "Selecciona y marca un evento")
    )
    
    
    points_final<-values$all_datos_marcados %>% gather("sensor", "temperatura", which(names(datos_crudos()) %in% c("huevo", "nido")))
    
    min2<-min(points_final$ts)-60
    max2<-max(points_final$ts)+60
    
    #do sth to this
    
if(input$mark_pattern==0){
  p<-   ggplot(data=points_final,
           aes(x=ts, y=temperatura, colour=sensor))+geom_line()+scale_x_datetime(breaks = seq(floor_date(min2, unit = "days"), 
                                                                                              floor_date(max2, unit = "days"), 
                                                                                              by="12 hours"))
}
    #add the shapes
 else{
   mr<-data.frame(marked_rects())
   mr$patron<-factor(mr$patron, levels = unique(mr$patron))
   
   
      p<-ggplot()+geom_line(data=points_final,
                            aes(x=ts, y=temperatura, colour=sensor))+scale_x_datetime(breaks = seq(floor_date(min2, unit = "days"), 
                                                                                                   floor_date(max2, unit = "days"), 
                                                                                                   by="12 hours"))+ 
        geom_rect(data=mr, aes(xmin=start-30, xmax=end+30, ymin=-Inf, ymax=Inf, fill=patron), alpha=0.4)+
        scale_fill_manual(values=palette)
      
 }
    
   
    p
    
  })
  
  # #check
  # output$table_final<-renderPrint({
  #   head(
  #     marked_rects(),
  #   50
  #   )
  # })
  # 
  # output$table_bpts2<-renderPrint({
  #   data_to_fit<-data_to_fit()  
  #   
  #   T_amb_event()+min(data_to_fit$temperatura)
  # })

  
  observeEvent(input$save_raw, {
    
    saving_df<-values$all_datos_marcados
    
    ##
    # make a thing that uses input$day_zero to get d since
    ###
    if(input$day_zero!=Sys.Date()){
    saving_df$day_since_laying<-yday(saving_df$ts)-yday(parse_date_time(as.character(date(input$day_zero)), orders = c("Ymd")))
    } else {saving_df$day_since_laying<-NA}
    
    if (file.exists("result")){
      
    } else {
      dir.create("result")
    }
    
    write.csv(saving_df, 
              file = paste("result/marcado_", input$archivo, sep = "")
              )
    js$reset()
  })
  
  
  ####################################################
  # CPA
  ####################################################
  
  output$header_cpa<-renderPrint({
    o=datos_crudos_pre()
    rbind(names(o))
  })
  #make reactive fit choice
  output$fitcontrols_cpa <- renderUI({
    avail<-names(datos_crudos())[names(datos_crudos()) %in% c("nido", "huevo")]
    radioButtons("fit_selector_cpa", "Que fit quieres correr?",
                 choices =  avail)
  })
  cpa_pts1<-reactive({
    cru<-datos_crudos()
    
    col<-input$fit_selector_cpa
    
    m.data<-rollmean(cru[,col], k=input$movingaverage_width)
    
    #fix this so t is right
    
    d=data.frame("ts"=cru$ts[1:length(m.data)],
               "temperatura"=m.data)
    m.pelt=cpt.mean(d$temperatura,method="PELT")
    m.points<-cpts(m.pelt)
    
    d[m.points,]
    
  })
  
  output$cpa_plot1 <- renderPlot({
    cr<-datos()
    ggplot(data=cr,
           aes(x=ts, y=temperatura))+geom_line(aes(colour=sensor))+
      scale_x_datetime(breaks = seq(floor_date(min(cr$ts), unit = "days"),floor_date(max(cr$ts), unit = "days"), 
                                    by="12 hours"))+
      ylim(input$ylim)+geom_point(data=cpa_pts1(), aes(x=ts, y=temperatura))+theme(legend.position="left")
    
  })
  
})
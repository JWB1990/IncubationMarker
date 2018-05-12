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

  values<- reactiveValues()


  datos_crudos_pre <- reactive({
    cru=read.table(paste("hobo", "/", input$archivo, sep = ""), header = T, stringsAsFactors = F, row.names = NULL)

    if("row.names" %in% names(cru)){
      names(cru)<-c(names(cru)[2:ncol(cru)], paste0("X", ncol(cru)))
    }
    cru$patron<-c(NA)
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
  #cru


  cru


  })
  observeEvent(input$load, {
    isolate({
      d<-datos_crudos()
      empty_df<-d[-(1:nrow(d)),]
      values$all_datos_marcados <-  empty_df #this is the empty data frame for nests with 2 sensors

    })
  })
    # output$obs_raw<-renderPrint({
    #   values$plot2_brush_x_width
    #   })
  datos<-reactive({
  #cambia la tabla formato larga
    datos_crudos() %>% gather("sensor", "temperatura", which(names(datos_crudos()) %in% c("huevo", "nido", "amb")))
  })






  output$plot1 <- renderPlot({
    cr<-datos()
    nightboundaries<-cr %>% filter(hour(ts)==0 & minute(ts)==0)
    p<-ggplot(data=cr)+
      geom_line(data=cr,aes(x=ts, y=temperatura, colour=sensor))+
      scale_x_datetime(breaks = seq(floor_date(min(cr$ts), unit = "days"),
                                    floor_date(max(cr$ts), unit = "days"),
                                    by="12 hours"),labels=date_format("%Y-%m-%d\n%H:%M"))+
      ylim(input$ylim)+theme(legend.position="left")+xlab("Tiempo")+ylab("Temperatura")+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)

    if(nrow(marked_rects())>0){
      mr<-data.frame(marked_rects())
      p<-p+geom_rect(data=mr, aes(xmin=start-30, xmax=end+30, ymin=-Inf, ymax=Inf), fill="grey", alpha=0.4)}
    p
  })

  #get brushed points of first viewer
  firstbrushstilldeadly<-observeEvent(input$plot1_brush,{
    values$bpts<-brushedPoints(datos(), input$plot1_brush)
  })

  #get single event
  output$plot2 <- renderPlot({

    validate(
      need(is.null(input$plot1_brush) == FALSE, "Selecciona unos puntos")
    )
    points2<-values$bpts
    nightboundaries<-points2 %>% filter(hour(ts)==0 & minute(ts)==0)
    m<-min(points2$ts)
    day<-paste0("Fecha: ", paste(year(m), month(m), day(m), sep="-"))


    p<-ggplot(data=points2)+
      geom_line(data=points2,
                aes(x=ts, y=temperatura, colour=sensor))+
      geom_point(data=points2,
                 aes(x=ts, y=temperatura, colour=sensor))+

      scale_x_datetime(breaks = date_breaks("15 min"),
                       minor_breaks=date_breaks("1 min"), labels=date_format("%H:%M"),
                       limits = range(points2$ts))+
      ylim(input$ylim)+theme(legend.position="left")+xlab("Tiempo")+ylab("Temperatura")+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)+
      ggtitle(day)

    if(nrow(marked_rects())>0){
      mr<-data.frame(marked_rects())
      p<-p+geom_rect(data=mr, aes(xmin=start-30, xmax=end+30, ymin=-Inf, ymax=Inf), fill="grey", alpha=0.4)}

p

  })

  output$plotui <- renderUI({
    plotOutput("plot2",
               brush = brushOpts(id = "plot2_brush", direction = "x",
                                 fill = "red", opacity = 0.2)
    )
  })

  some_event<-observeEvent( input$plot2_brush, {
    values$plot2_brush<- input$plot2_brush


  })

  values$bpts2 <- reactive({
    validate(
      need(is.null(values$plot2_brush) == FALSE, "Selecciona un evento")
    )
    brushedPoints(values$bpts, values$plot2_brush)
  })

  #make reactive fit choice
  output$fitcontrols <- renderUI({
    avail<-names(datos_crudos())[names(datos_crudos()) %in% c("nido", "huevo")]
    radioButtons("fit_selector", "Que fit quieres correr?",
                 choices =  avail)
  })


  T_amb_event <- reactive({
    selpoints<-values$bpts2()

    data_to_fit<-data_to_fit()
    mean(selpoints$temperatura[selpoints$sensor=="amb"], na.rm = T)-min(data_to_fit$temperatura)

  })

  min_sel_event<-reactive({
    m<-values$bpts2()
    min(m$ts)
  })
  max_sel_event<-reactive({
    n<-values$bpts2()
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

         if(evt_class_end()=="2") {




           d<-d[d$ts >= min_sel_event() & d$ts <= max_sel_event(),]
           d$patron<-"warming"
           if(ceiling(t_at_half_from_T_s_on())<nrow(d)){
              d$patron[ceiling(t_at_half_from_T_s_on()):nrow(d)]<-"on_asymptote"}
           d$event_number<-0

           fits<-fits()
           if(!is.null(fits$newton)){
           on_newton_pars<-fits$newton$m$getAllPars()

           d$alpha<- as.numeric(on_newton_pars[names(on_newton_pars)=="a"])
           d$T_s<- as.numeric(on_newton_pars[names(on_newton_pars)=="T_s"])+min(data_to_fit$temperatura)
           } else {d$alpha<-NA; d$T_s<-NA}

         } else {

           d<-d[d$ts >= min_sel_event() & d$ts <= max_sel_event(),]
           d$patron<-"cooling"
           if(ceiling(t_at_half_from_T_s_off())<nrow(d)){
             d$patron[ceiling(t_at_half_from_T_s_off()):nrow(d)]<-"off_asymptote"}
           d$event_number<-0


           fits<-fits()
           #alpha from newton fit
           if(!is.null(fits$newton)){

           off_newton_pars<-fits$newton$m$getAllPars()
           d$alpha<- as.numeric(off_newton_pars[names(off_newton_pars)=="a"])
           d$T_s<- T_amb_event()+min(data_to_fit$temperatura)
           } else {d$alpha<-NA; d$T_s<-NA}


         }


       d
  })


    values$evt_class_end<-1
    values$evt_class_sum<-1

    hitswtich<-observeEvent(input$switch_event, {values$evt_class_sum<-values$evt_class_sum+1})

    evt_class_end<-reactive({(values$evt_class_sum)%%2+1})
    output$monitor_evt_class<-renderText({ifelse(evt_class_end()==1, off_text, on_text)})

    #

output$event_class<-reactive({evt_class_end()})
outputOptions(output, "event_class", suspendWhenHidden = FALSE)


  new_entry<-observeEvent(input$mark_pattern, {
    #first select the marked data
    res<-rbind(
      values$all_datos_marcados,
      datos_marcados()
      )


    #allow for fuckups: when ppl select the wrong thing, they
    #can select it again and overwrite it
    #so first rbind, then retain only last etnries for each timestamp

    res<-res[!rev(duplicated(rev(res$ts))),]

    values$all_datos_marcados <- res

    # ####################### to scoot the marked data in brushed point2, first remember how far to scoot
    # isolate({
    #   values$plot2_brush_x_width<-as.numeric(values$plot2_brush$xmax-values$plot2_brush$xmin)
    # })

    ###################################and scoot bpts to clip LHS selected frame
    brushedpoints1<-values$bpts
    brushedpoints2<-values$bpts2()
    values$bpts<-subset(brushedpoints1, brushedpoints1$ts>max(brushedpoints2$ts))

    ###################################last update evt_class
    values$evt_class_sum<-values$evt_class_sum+1


    # ####################### now scoot the marked data in brushed point2
    # values$plot2_brush$xmax<-values$plot2_brush$xmax+values$plot2_brush_x_width
    # values$plot2_brush$xmin<-values$plot2_brush$xmin+values$plot2_brush_x_width

  })



  marked_rects<-reactive({

    #validate(need(nrow(values$all_datos_marcados)>0, "Marca unos eventos"))

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

    ggplot()+geom_point(data=values$bpts2() %>% filter(sensor==input$fit_selector),
                        aes(x=ts, y=temperatura, colour=sensor), size=4)+
      geom_point(data=values$bpts2() %>% filter(sensor!=input$fit_selector),
                 aes(x=ts, y=temperatura, colour=sensor), alpha=0.6, size=2)+xlab("Tiempo")+ylab("Temperatura")




  })

  ############################################################ fits ################################################################

  data_to_fit<-reactive({
    values$bpts2() %>% filter(sensor==input$fit_selector)
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
  mStarting = list(a = input$init_newton_off_a)
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
  mStarting = list(a = input$init_newton_off_a,
                   T_s=input$init_newton_off_T_s)
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
  mStarting = list(a = input$init_newton_on_a,
                   T_s=input$init_newton_on_T_s)
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
#
# nullmodel<-reactive({
#   mFunction = function(x, params) {
#     a = params[["a"]]
#     return(rep(a, length(x)))
#
#   }
#   # 2) Name
#   mName = "mean"
#   # 3) Formula
#   mFormula = y ~ a
#   # 4) Model parameters
#   mParams = c("a")
#   # 5) List of starting values for the parameters
#   mStarting = list(a=1)
#   # Create the customModel object
#   buildModel(mFunction, mName, mFormula, mParams, mStarting)
# })



  fits<-reactive({


    ##################
    #define subsets of models to be run with switch
    models_on = getModelLibrary()[c("linearFit")]
    # models_on[["null"]]<-nullmodel()
    models_on[["O2_poly"]] <- O2_poly_on()
    models_on[["O3_poly"]] <- O3_poly_on()
    models_on[["newton"]] <- cool_newton_on()
    models_on_chosen<-models_on[input$on_models]



    models_off = getModelLibrary()[c("linearFit")]
    # models_off[["null"]]<-nullmodel()
    models_off[["O2_poly"]] <- O2_poly_off()
    models_off[["O3_poly"]] <- O3_poly_off()
    models_off[["newton"]] <- cool_newton_off()
    models_off_chosen<-models_off[input$off_models]


    models_off_noamb = getModelLibrary()[c("linearFit")]
    # models_off_noamb[["null"]]<-nullmodel()
    models_off_noamb[["O2_poly"]] <- O2_poly_off()
    models_off_noamb[["O3_poly"]] <- O3_poly_off()
    models_off_noamb[["newton"]] <- cool_newton_off_noamb()
    models_off_noamb_chosen<-models_off_noamb[input$off_models]

    if(evt_class_end()==2){
      res<-( (fitModels(models_on_chosen , x(), y())))
    } else if(evt_class_end()==1 & input$col_amb!=0){
        res<-((fitModels(models_off_chosen , x(), y())))
    } else if(evt_class_end()==1 & input$col_amb==0){
      res<-((fitModels(models_off_noamb_chosen , x(), y())))
    }
res
    })

################ this determines the spot at which the threshold is reached
  #make sure it uses weighted y (green)

  t_at_half_from_T_s_on<-reactive({


  if(evt_class_end()==1 | !c("newton") %in% input$on_models ){u<-1000000}
  else{


    fits<-fits()
    xpred<-seq(0, max(x()), length.out = 100)
    ypred<-averageCurve(fits, xpred)
    on_newton_pars<-fits$newton$m$getAllPars()
    T_s<-as.numeric(on_newton_pars[names(on_newton_pars)=="T_s"])


    u<-min(xpred[ypred$y>((T_s)*0.01*(100-input$umbral_on))])

  }
  u

  })


  t_at_half_from_T_s_off<-reactive({

    data_to_fit<-data_to_fit()


    if(evt_class_end()==2 | !c("newton") %in% input$off_models ){u<-1000000}
    else if(input$col_amb!=0){
      fits<-fits()
      xpred<-seq(0, max(x()), length.out = 100)
      ypred<-averageCurve(fits, xpred)
      T_a<-T_amb_event()
      T_max<-max(y())

      u<-min(xpred[ypred$y<((T_a)+0.01*(input$umbral_off)*(T_max-T_a))])



    } else if(input$col_amb==0){
      fits<-fits()
      xpred<-seq(0, max(x()), length.out = 100)
      ypred<-averageCurve(fits, xpred)
      off_newton_pars<-fits$newton$m$getAllPars()
      T_a<-as.numeric(off_newton_pars[names(off_newton_pars)=="T_s"])
      T_max<-max(y())
      u<-min(xpred[ypred$y<((T_a)+0.01*(input$umbral_off)*(T_max-T_a))])
    }
    u

  })




  #this plots the fits
  output$fit_plot1 <- renderPlot({

    #all_converged<-all(sapply(fs[-length(fs)], FUN=function(x){x$convInfo$isConv}))
    validate(
    need(nrow(values$bpts2())>0  ,"Seleccione un evento"),
    need(try(is.null(fits())==F)  ,"Unas curvas no convergieron. Curvas correctas?")
    )


    #make this better
    #use singluar gradient estimate warning
    #make the points toggleable

    if(evt_class_end()=="2"){
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
    validate(
      need(nrow(values$bpts2())>0  ,"Seleccione un evento"),
      need(try(is.null(fits())==F)  ,"Unas curvas no convergieron. Curvas correctas?")
    )
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
      need( nrow(values$all_datos_marcados)>0, "Selecciona y marca un evento")
    )


    points_final<-values$all_datos_marcados %>% gather("sensor", "temperatura", first(which(names(datos_crudos()) %in% c("huevo", "nido"))))
    #this is assuming 60 seconds recording intervals
    points_final$contig<-rep(1:(length(which(diff(as.numeric(points_final$ts))>60))+1),
                             (rle(diff(as.numeric(points_final$ts)))$lengths+1)[rle(diff(as.numeric(points_final$ts)))$values==60])

    min2<-min(points_final$ts)-60
    max2<-max(points_final$ts)+60

    nightboundaries<-points_final %>% filter(hour(ts)==0 & minute(ts)==0)

    #do sth to this

if(nrow(marked_rects())==0){
  p<-   ggplot(data=points_final,
           aes(x=ts, y=temperatura, colour=sensor, group=contig))+geom_line()
}
    #add the shapes
 else{
   mr<-data.frame(marked_rects())
   mr$patron<-factor(mr$patron, levels = unique(mr$patron))


      p<-ggplot()+geom_line(data=points_final,
                            aes(x=ts, y=temperatura, colour=sensor, group=contig))+
        geom_rect(data=mr, aes(xmin=start-30, xmax=end+30, ymin=-Inf, ymax=Inf, fill=patron), alpha=0.4)+
        scale_fill_manual(values=palette)+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)

 }


    p+theme(legend.position="left")+xlab("Tiempo")+ylab("Temperatura")
    #+scale_x_datetime(breaks = seq(floor_date(min2, unit = "days"),ceiling_date(max2, unit = "days"),by="1 hour"))
    #scale_x_datetime(breaks = date_breaks("12 hours"),
    #                 minor_breaks=date_breaks("1 hour"), labels=date_format("%Y:%m:%d %H:%M"))
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
    if(length(input$day_zero)!=0){
    saving_df$day_since_laying<-yday(saving_df$ts)-yday(parse_date_time(as.character(date(input$day_zero)), orders = c("Ymd")))
    } else {saving_df$day_since_laying<-NA}

    if (file.exists("result")){

    } else {
      dir.create("result")
    }

    write.csv(saving_df,
              file = paste("result/marcado_", input$archivo, sep = ""),
              row.names = F)
    js$reset()
  })

  observeEvent(input$pre_save_raw, {

    pre_saving_df<-values$all_datos_marcados



    if (file.exists("result")){

    } else {
      dir.create("result")
    }

    write.csv(pre_saving_df,
              file = paste("result/temp_", input$archivo, sep = ""),
    row.names = F)
  })

resume <- observeEvent(input$resume, {
    inFile <- input$pre_saved_file

    if (!is.null(inFile)){
    loaded<-read.table(inFile$datapath, stringsAsFactors = F, sep=",", header = T)
    loaded$ts<-parse_date_time(loaded$ts, "Ymd HMS")

    }
#tack on to values$all_datos_marcados

    values$all_datos_marcados<-rbind(values$all_datos_marcados, loaded)
    #marked_rects()

  })




  ########################################################################################################################################################
  ########################################################################################################################################################
  ########################################################################################################################################################
  ################# CPA ##################################################################################################################################

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

    #add filter here
    # cols<-which(names(cru) %in% c("nido", "huevo", "amb"))
    # maxes<-apply(cru[,cols], 2, FUN=function(x){max(x, na.rm=T)})
    # mins<-apply(cru[,cols], 2, FUN=function(x){min(x, na.rm=T)})
    # cru<-subset(cru, maxes < max(input$ylim) & mins > min(input$ylim))

    col<-input$fit_selector_cpa

    # m.data<-rollmean(cru[,col], k=input$movingaverage_width)
    #
    # #fix this so t is right
    # nrow_all<-nrow(cru)
    # nrow_rollmean<-length(m.data)
    # diff_in_length<-nrow_all-nrow_rollmean
    # dil<-ceiling(diff_in_length/2)
    #
    # d=data.frame("ts"=cru$ts[dil:(nrow(cru)-dil)],
    #            "temperatura"=m.data)
    #
    # m.pelt=cpt.mean(d$temperatura,method="PELT")
    # m.points<-cpts(m.pelt)

    #d[m.points,]
     d=data.frame("ts"=cru$ts,
                "dif"=c(diff(cru[,col],1),0),
                "temperatura"=c(cru[,col]))
    d.pts<-which((d$dif>min(input$cpa_max_dif_on) & d$dif<max(input$cpa_max_dif_on)) | (d$dif<max(input$cpa_max_dif_off) & d$dif>min(input$cpa_max_dif_off)))
    d[d.pts,]



  })

  output$cpatab <- renderPrint({
    points<-datos()

    #breaks
    breaks<-values$cpa_pts2()

    #prep for dygraphs
    dy_points<-points[,c("ts", "sensor", "temperatura")]
    dy_points$ts<-gsub(as.character(dy_points$ts), pattern = "-", replacement = "/")

    dy_points<-tidyr::spread(dy_points, sensor, temperatura)
    rownames(dy_points)<-dy_points$ts
    dy_points$ts<-NULL



    clicked_x_point<-as.character(input$cpa_plot2_dygraph_click$x_closest_point)
    clicked_x_point<-paste0(substr(clicked_x_point, 1,10), " ",substr(clicked_x_point, 12,19))
    #clicked_x_point<-parse_date_time(clicked_x_point, orders = "ymd HMS")

    list(
      #(input$cpa_plot2_dygraph_date_window),

    str(input$cpa_plot2_dygraph_click),
  #  dy_points,
  clicked_x_point,
    values$toggles
    )
    })

  output$cpa_plot1 <- renderPlot({
    cr<-datos()
    nightboundaries<-cr %>% filter(hour(ts)==0 & minute(ts)==0)

    cpa_pts1<-cpa_pts1()
    cpa_pts1<-subset(cpa_pts1, cpa_pts1$temperatura > input$ylim[1] & cpa_pts1$temperatura < input$ylim[2])

    ggplot(data=cr,
           aes(x=ts, y=temperatura))+geom_line(aes(colour=sensor))+
      scale_x_datetime(breaks = seq(floor_date(min(cr$ts), unit = "days"),floor_date(max(cr$ts), unit = "days"),
                                    by="12 hours"))+
      ylim(input$ylim)+geom_point(data=cpa_pts1, aes(x=ts, y=temperatura))+theme(legend.position="left")+
      xlab("Tiempo")+ylab("Temperatura")+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)

  })


  cpa_bpts2<-reactive({
    validate(
      need(is.null(input$cpa_plot1_brush) == FALSE, "Selecciona un evento")
    )
    brushedPoints(datos(), input$cpa_plot1_brush)


  })

  values$cpa_pts2<-reactive({
    validate(
      need(is.null(input$cpa_plot1_brush) == FALSE, "Selecciona un evento")
    )

      cpa_cpts1<-cpa_pts1()
      subset(cpa_cpts1, cpa_cpts1$temperatura > input$ylim[1] & cpa_cpts1$temperatura < input$ylim[2])




  })

  # output$cpa_plot2 <- renderPlot({
  #
  #   validate(
  #     need(is.null(input$cpa_plot1_brush) == FALSE, "Selecciona unos puntos")
  #   )
  #   points2<-brushedPoints(datos(), input$cpa_plot1_brush)
  #   nightboundaries<-points2 %>% filter(hour(ts)==0 & minute(ts)==0)
  #   m<-min(points2$ts)
  #   day<-paste0("Fecha: ", paste(year(m), month(m), day(m), sep="-"))
  #
  #   #breaks
  #   breaks<-values$cpa_pts2()
  #
  #
  #   p<-ggplot(data=points2)+
  #     geom_line(data=points2,
  #               aes(x=ts, y=temperatura, colour=sensor))+
  #     geom_point(data=points2,
  #                aes(x=ts, y=temperatura, colour=sensor))+
  #
  #     scale_x_datetime(breaks = date_breaks("15 min"),
  #                      minor_breaks=date_breaks("1 min"), labels=date_format("%H:%M"),
  #                      limits = range(points2$ts))+
  #     ylim(input$ylim)+theme(legend.position="left")+xlab("Tiempo")+ylab("Temperatura")+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)+
  #     ggtitle(day)+
  #     geom_vline(data=breaks, aes(xintercept = ts))
  #
  #
  #   p
  #
  # })

  output$cpa_plot2_dygraph <- renderDygraph({

    validate(
      need(is.null(input$cpa_plot1_brush) == FALSE, "Selecciona unos puntos")
    )

    points<-datos()
    points2<-brushedPoints(datos(), input$cpa_plot1_brush)
    min_pt2<-min(points2$ts)
    max_pt2<-max(points2$ts)

    nightboundaries<-points %>% filter(hour(ts)==0 & minute(ts)==0)
    m<-min(points$ts)
    day<-paste0("Fecha: ", paste(year(m), month(m), day(m), sep="-"))

    #breaks
    breaks<-values$cpa_pts2()

#prep for dygraphs
    dy_points<-points[,c("ts", "sensor", "temperatura")]
    dy_points$ts<-gsub(as.character(dy_points$ts), pattern = "-", replacement = "/")
    dy_points<-tidyr::spread(dy_points, sensor, temperatura)
    rownames(dy_points)<-dy_points$ts
    dy_points$ts<-NULL
#prep for dygraphs
    dy_points2<-points2[,c("ts", "sensor", "temperatura")]
    dy_points2$ts<-gsub(as.character(dy_points2$ts), pattern = "-", replacement = "/")
    dy_points2<-tidyr::spread(dy_points2, sensor, temperatura)
    rownames(dy_points2)<-dy_points2$ts
    dy_points2$ts<-NULL

    cols<-c("huevo", "nido", "amb")
    cols<-cols[cols %in% names(dy_points2)]

    p<-dygraph(dy_points, main="Coloca las demarcaciones de eventos") %>%
      dySeries(cols[1], label=cols[1]) %>% dySeries(cols[2], label=cols[2]) %>%
      dyAxis("y", label = "Temp (C)", valueRange = c(input$ylim)) %>%
      dyRangeSelector(dateWindow = c(min_pt2, max_pt2)) %>%
      dyOptions(labelsUTC = TRUE)
 #
    for(i in 1:nrow(breaks)){
      p<-p %>%  dyEvent(x = breaks[i,1], label='', labelLoc='bottom')
    }
p


    #then add new series with 2x temp resolution as data
    #restriction
    #only one action available
    #so it toggles breakpoints,


#
#     p<-ggplot(data=points2)+
#       geom_line(data=points2,
#                 aes(x=ts, y=temperatura, colour=sensor))+
#       geom_point(data=points2,
#                  aes(x=ts, y=temperatura, colour=sensor))+
#
#       scale_x_datetime(breaks = date_breaks("15 min"),
#                        minor_breaks=date_breaks("1 min"), labels=date_format("%H:%M"),
#                        limits = range(points2$ts))+
#       ylim(input$ylim)+theme(legend.position="left")+xlab("Tiempo")+ylab("Temperatura")+geom_vline(data=nightboundaries, aes(xintercept=ts), alpha=0.5, lty=2)+
#       ggtitle(day)+
#       geom_vline(data=breaks, aes(xintercept = ts))
#p

  })

  set_up_event<-observe({
    dc=datos_crudos()
    tog_df<-data.frame("toggle"=rep(FALSE, nrow(dc)),
               "ts"=dc$ts)

    breaks<-values$cpa_pts2()
    tog_df$toggle[tog_df$ts %in% breaks$ts]<-TRUE
    values$toggles<-  tog_df
               })



  #change_a_barrier<-eventReactive(input$cpa_max_dif_on,{
   # values$toggles<-
    #add influence of cpa_bpts()
  #})
  # change_a_barrier2<-eventReactive(input$cpa_max_dif_off,{
  #   #values$toggles<-
  #   #add influence of cpa_bpts()
  # })
  change_a_toggle<-observeEvent(input$cpa_plot2_dygraph_click,{
    values$toggles$ts<-as.character(values$toggles$ts)
    clicked_x_point<-as.character(input$cpa_plot2_dygraph_click$x_closest_point)
    clicked_x_point<-paste0(substr(clicked_x_point, 1,10), " ",substr(clicked_x_point, 12,19))
    #clicked_x_point<-parse_date_time(clicked_x_point, orders = "ymd HMS")
    current_state<-values$toggles$toggle[values$toggles$ts==clicked_x_point]
    newstate<-ifelse(isTRUE(current_state), FALSE, TRUE)
    values$toggles$toggle[values$toggles$ts==clicked_x_point]<-newstate

  })

  output$cpa_plotui <- renderUI({
    plotOutput("cpa_plot2",
               click = brushOpts(id = "cpa_plot2_click")
    )
  })


})

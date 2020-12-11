
PortfolioAnalysis<-function(input,output,session,DATA){

  # # 1er filtre
  # thechoice_portfolio <- reactiveVal(NULL)  
  # thechoice_type <- reactiveVal(NULL)  
  # thechoice_region <- reactiveVal(NULL)  
  # thechoice_brand <- reactiveVal(NULL)  
  # thechoice_product <- reactiveVal(NULL)
  # thechoice_volume <- reactiveVal(TRUE) 
 # # 2èmes filtres
  # thechoice_portfolio2 <- reactiveVal(NULL)  
  # thechoice_type2 <- reactiveVal(NULL)  
  # thechoice_region2 <- reactiveVal(NULL)  
  # thechoice_brand2 <- reactiveVal(NULL)  
  # thechoice_product2 <- reactiveVal(NULL)
  # thechoice_volume2 <- reactiveVal(TRUE) 
 
  
####################################A  COMPOSITION ET FILTRES DU PETIT DEJEUNER ##################################
  
  observeEvent(input$valid_settings,{
    
#Erreur si aucun aliment dans le petit déjeuner

    if(input$choice_cereals=="none"&input$choice_compo1=="none"&input$choice_compo2=="none"&input$choice_compo3=="none"&
       input$choice_compo4=="none"&input$choice_compo5=="none"){
      
      output$erreur_portfolio<-renderUI({h4("Please choose at least one food in your breakfast ")})
      output$erreur_cereals2<-renderUI({NULL})
      output$erreur_compo1<-renderUI({NULL})
      output$erreur_compo2<-renderUI({NULL})
      output$erreur_compo3<-renderUI({NULL})
      output$erreur_compo4<-renderUI({NULL})
      output$erreur_compo5<-renderUI({NULL})
      output$erreur_name<-renderUI({NULL})

      
    }else if(is.null(DATA$recommendations)){
        
        output$erreur_portfolio<-renderUI({h4("File not found. Please import recommendations file")})
        output$erreur_cereals2<-renderUI({NULL})
        output$erreur_compo1<-renderUI({NULL})
        output$erreur_compo2<-renderUI({NULL})
        output$erreur_compo3<-renderUI({NULL})
        output$erreur_compo4<-renderUI({NULL})
        output$erreur_compo5<-renderUI({NULL})
        output$erreur_name<-renderUI({NULL})

        
      }else{ # sinon :continue en analysant chaque composante du petit dej (ceréales et autres)
    
      output$erreur_portfolio<-renderUI({NULL})
      check_erreur=NULL
      
#A.1. Cereales ####
   
  if(input$choice_cereals=="yes" & is.null(DATA$compo_cereals)){# si fichier céréales manquants mais appuie quand même sur valider
  check_erreur[6]=TRUE  
  }
      
 if(!is.null(input$choice_cereals_portion)){ # taille de portion      
  if(input$choice_cereals=="yes"&input$choice_cereals_portion=="change"){
    
    if(is.numeric(input$cereals_size)& input$cereals_size>0){
    DATA$compo_cereals=DATA$compo_cereals%>%mutate(Serving_size=input$cereals_size)
    output$erreur_cereals2<-renderUI({NULL})
    
    }else{
      
    output$erreur_cereals2<-renderUI({h4("Please fill cereals serving size with numeric and positive value.")})
    check_erreur[6]=TRUE
    }
  }}


#A.2 Composantes #### 
components_choices=c(input$choice_compo1,input$choice_compo2,input$choice_compo3,input$choice_compo4,input$choice_compo5)

if("inca2"%in%components_choices|"import"%in%components_choices){ 

    list_filecompo=c("Product_name","Serving_size","Energy.kcal", "Proteins.g","Carbohydrates.g","Add_sugars.g","Fat.g",#Sugars.g,
                     "Saturated_fat.g","Fibre.g","Sodium.mg","Vitamin_D.mcg", "Vitamin_C.mg", "Vitamin_B1.mg","Vitamin_B2.mg",
                     "Vitamin_B3.mg","Vitamin_B6.mg","Vitamin_B9.mcg","Vitamin_B12.mcg","Vitamin_A.mg",
                     "Calcium.mg","Iron.mg","Zinc.mg","Magnesium.mg")

   #compte le nombre des composantes non null et de quels types elles sont (fichier importé ou table ciqual)
   components_choices_nn=which(components_choices!="none") #trouve les composantes non nulles
    
    compo_autres=NULL
    vecpos=NULL
    
    if(length(components_choices_nn)>0){
    for (i in 1:length(components_choices_nn)){
      
     vecpos[i]=components_choices_nn[i] #position de la composante non nulle dans components_choices --> numero de la variable
     var1=paste0("choice_compo",vecpos[i])
     var2=paste0("compo",vecpos[i],"_lab")
     var3=paste0("compo",vecpos[i],"_size")
     check_erreur[i]=FALSE   

     if (input[[var1]]%in%c("inca2","import")){ # si choix dans la food database (inca2 ou personnelle)
       if(!is.null(input[[var2]])){ # si choix n'est pas nul
         if(is.numeric(input[[var3]]) & input[[var3]]>0){ #si la taille de portion est numérique et non nulle
           if(input[[var1]]=="inca2"){ # si choix parmis inca 2 
           compo_ligne=INCA2%>%filter(Product_name==input[[var2]])%>%mutate(Product_ID=i,Serving_size=input[[var3]])%>%
                       select(Product_ID,list_filecompo)
           }else{ # si choix parmis aliments importés
             compo_ligne=DATA$compo_imported%>%filter(Product_name==input[[var2]])%>%mutate(Product_ID=i,Serving_size=input[[var3]])%>%
               select(Product_ID,list_filecompo)           
           }
          compo_autres=bind_rows(compo_autres,compo_ligne)
         }else{ # si serving size non numérique
        check_erreur[i]=TRUE
          }
       }else{ # si choix nul
         check_erreur[i]=TRUE
         }
     }
     
  }}# end for
    
#UI erreurs     
    lapply(1:length(components_choices_nn), function(i) {
   if(check_erreur[i]==TRUE){
      output[[paste0("erreur_compo",vecpos[i])]]<-renderUI({h4(paste("Please choose a food for Component",vecpos[i],"or 
                                                                      fill its serving size with numeric and positive value"))})
   }else{
     output[[paste0("erreur_compo",vecpos[i])]]<-renderUI({NULL})
   }
    })
    
    
DATA$compo_autres<-compo_autres
}else{ # si aucune composante en + des céréales
  DATA$compo_autres<-NULL
} 
  
    
#A.2 Nom du portfolio ####
    existing_names=unique(DATA$input_table$DATA_name)
    if(is.null(input$portfolio_name)|input$portfolio_name==""){
      output$erreur_name<-renderUI({h4("Please choose a name for your breakfast")})
    }else if(input$portfolio_name%in%existing_names){
      output$erreur_name<-renderUI({h4("Please choose a breakfast name different from existing one(s)")})
    }else{
     output$erreur_name<-renderUI({NULL}) 
    }
    
#Si tous les fichiers sont ok --> on vérife que les contenus des colonnes sont bien numériques
#A.5 Validation des toutes les données ####
    
  if (!is.null(input$portfolio_name)&input$portfolio_name!="" &!(TRUE%in%check_erreur) & !(input$portfolio_name%in%existing_names)){

  if(input$choice_cereals=="none"){
     compo_cereals<-NULL
     input_file_name_cereals<-NA
    }else{
     compo_cereals<-DATA$compo_cereals
     input_file_name_cereals=unique(DATA$compo_cereals$file_name)
   }

    df=calcul_apports(compo_cereals,DATA$recommendations,DATA$compo_autres)%>%
       mutate(DATA_name=input$portfolio_name,file_name_reco=unique(DATA$recommendations$file_name),file_name_cereals=input_file_name_cereals)%>%
       select(DATA_name,everything(),file_name_reco,file_name_cereals)
    
    DATA$input_table<- bind_rows(DATA$input_table,df)
    
    #Noms de colonnes nutriments modifiés après passage de la fonction de calcul des apports en pct des recommendations 
    # NB : modifier plus tard noms des nutriments en fonction des modifs apportées au graphique
    
    list_nut=c("Energy.kcal_dej_lower.pct","Proteins.g_dej_lower.pct","Carbohydrates.g_dej_lower.pctKCAL",
               "Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL","Fat.g_dej_lower.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
               "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
               "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
               "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct")
    label_nut=c("Energy","Protein","Carbohydrates *","Fibre","Added sugars *",
                "Fat *", "Saturated fat *",
                "Vitamin A","Vitamin B1", "Vitamin B2", "Vitamin B3", "Vitamin B6","Vitamin B9","Vitamin B12","Vitamin C", "Vitamin D",
                "Calcium","Iron","Magnesium","Sodium","Zinc")
    
    output$Panel2<-renderUI({
      tagList(
        div(class="panel panel-import", style="margin:20px;",
            #En-tête du panel
            div(class="panel-heading",
                fluidRow(column(12,h3("Settings")))#,
            ),
            fluidRow(class="formulaire",
                     column(12,align="center",
                         column(3,offset=3,selectInput("chosen_portfolio",h4("Set 1: Breakfast name"),
                                                    as.character(unique(DATA$input_table$DATA_name)),multiple=FALSE)),
                         column(6,align="left",style="margin-top:35px;",actionBttn(inputId="see_breakfasts",
                                            label="See your breakfasts",icon = icon("eye"),style = "simple",color = "primary")),
                          column(12,uiOutput("filtres",style="margin-bottom:20px;")),
                          column(12,
                            checkboxGroupButtons(inputId = "selected_nut",
                                                 label = h4("Nutrients :"),
                                                 choiceNames = label_nut,
                                                 choiceValues= list_nut,
                                                 selected=list_nut,
                                                 # status = "primary",
                                                 checkIcon = list(
                                                   yes = tags$i(class = "fa fa-check-square", 
                                                                style = "color: steelblue"),
                                                   no = tags$i(class = "fa fa-square-o", 
                                                               style = "color: steelblue"))
                            )),
                            column(12,align="left",h6("*Energy fixed at recommended value for breakfast")),
                                   uiOutput("modal_comp1"),br(),
                                   uiOutput("modal_comp2"),br(),
                                   # btn compare et erase varie en fonction des comparaisons
                                   column(12,align="center",uiOutput("compare_with"),
                                          uiOutput("erase_comp")),br(),
                                   column(12,style="margin-top:5px;",
                                          actionBttn(inputId="valid_param",label="Confirm settings",icon = icon("gear"),style = "simple",color = "success")),
                                   uiOutput("erreur_msg",style="font-color:red;"),
                                   uiOutput("erreur_msg_comp1",style="font-color:red;"),
                                   uiOutput("erreur_msg_comp2",style="font-color:red;")
                     )
            )))
    })

    
    #si pas de comparaison en cours --> ajout du bout "compare_with"
    if(DATA$compare_nb[1]==0 & DATA$compare_nb[2]==0){
    output$compare_with<-renderUI({actionBttn(inputId="compare_with1",label="Compare with ...",icon = icon("plus-circle"),
                         style = "simple",color = "warning")})
    output$erase_comp<-renderUI({NULL})
    }
    #sinon : on touche à rien
    
    sendSweetAlert(session = session,title = "Breakfast saved",type="success")  
    
  }} 
  })
  
  observeEvent(input$go_to_analysis,{  
  updateTabsetPanel(session,"cpw_app",selected="analysis")
  })
  
  
################################################################ B FILTRES ##################################################
  
observeEvent(input$chosen_portfolio,{ #Filtres dépendent du Breakfast choisi
 
  if(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio)%>%pull(Cereals_nb)%>%unique()==0){
    output$filtres<-renderUI({NULL})
  }else{
  
  region_levels=as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio)%>%pull(Region))) #regroupe tous les noms de régions tels qu'écrits dans la colonnes
  DATA$region_levels <-unique(unlist(strsplit(region_levels,","))) #séparer les noms séparés par des , et enlève les doublons
    
    output$filtres<-renderUI({
         tagList(
                column(12,align="center",
                       column(3,
                              selectInput("chosen_type",h4("Product type"),
                                          c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio)%>%
                                                                      pull(Product_type)))),selected="All",multiple=TRUE)),
                       column(3,
                              selectInput("chosen_region",h4("Region"),c("All",DATA$region_levels),selected="All",multiple=TRUE)),
                       column(3,
                              selectInput("chosen_brand",h4("Brand"),
                                          c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio)%>%
                                                                        pull(Brand)))),selected="All",multiple=TRUE)),
                       column(3,
                              selectInput("chosen_product",h4("Product name"),
                                          c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio)%>%
                                                                        pull(Product_name)))),selected="All",multiple=FALSE))
                   ),
                awesomeCheckbox(inputId = "volume_checkbox",label =h4("Weight by volume",style="margin-top:-1px;"),
                                value = FALSE,status = "primary"),
                uiOutput("warning_volume",style="font-color:red;")
                
                )
    })
  }
})
    
    
observeEvent(c(input$volume_checkbox,input$chosen_region),{ 
  if(!(is.null(input$chosen_region))&!("All"%in%input$chosen_region)&input$volume_checkbox==TRUE){
    output$warning_volume<-renderUI({h6("Warning : You chose specific region(s) and weighting by volume.
                                        Volumes can be defined for several regions at same time so weighted values in charts might be incorrect",
                                        style="color:red")})
  }else{
    output$warning_volume<-renderUI({NULL})
  }
  })
 
#B.1. Tableau de recap des breakfasts #### 
observeEvent(input$see_breakfasts2,{

  showModal(modalDialog(
    title=fluidRow(column(11,p(style="display:inline;",
                               paste("Breakfasts summmary"))), # titre de ton modal
                   column(1,p(style="display:inline;",
                              actionButton("close_summary",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
    size="l",
    fluidPage(style="margin-bottom:30px;padding:0px;",
              column(12,align="center",
                     dataTableOutput("summary_breakfasts"),
                     uiOutput("no_bf")
              )),
    footer=NULL
  ))
 
if(!is.null(DATA$input_table)){

  table=DATA$input_table%>%group_by(DATA_name)%>%
    summarize(Cereals_nb=ifelse(unique(Cereals_nb)==1,"Yes","No"),
              Cereals_serving_size=ifelse(Cereals_nb=="Yes",
                                          ifelse(length(unique(Serving_size_cereals))>1,"Variable amounts",paste0(Serving_size_cereals,"g")),""),
              compo1_name=ifelse(unique(Components_nb)>=1,paste0(unique(compo1_name)," (",Serving_size_compo1," g)"),""),
              compo2_name=ifelse(unique(Components_nb)>=2,paste0(unique(compo2_name)," (",Serving_size_compo2," g)"),""),
              compo3_name=ifelse(unique(Components_nb)>=3,paste0(unique(compo3_name)," (",Serving_size_compo3," g)"),""),
              compo4_name=ifelse(unique(Components_nb)>=4,paste0(unique(compo4_name)," (",Serving_size_compo4," g)"),""),
              compo5_name=ifelse(unique(Components_nb)>=5,paste0(unique(compo5_name)," (",Serving_size_compo5," g)"),""),
              file_name_reco=unique(file_name_reco),file_name_cereals=unique(file_name_cereals))%>%
    mutate(Cereals=paste0(Cereals_nb," (",Cereals_serving_size,")"))%>%select(-Cereals_nb,-Cereals_serving_size)%>%
    select(DATA_name,Cereals,everything())

  #transpose la table
  table_t<-data.frame(t(table[-1]))
  colnames(table_t) <- table$DATA_name
  
  output$summary_breakfasts<-renderDataTable({
    DT::datatable({table_t},
                  rownames=c('Cereals ?','Component 1','Component 2','Component 3','Component 4','Component 5',
                             'Recommendations file','Cereals file'),
                  options = list(dom='t',pageLength=50,display="compact",autoWidth = TRUE,scrollX=TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#5cb85c', 'color': '#fff'});",
                                   "}"),
                                 columnDefs = list(list(className = 'dt-center', targets = 1:length(table_t))))
    )
  })
  
  output$no_bf<-renderUI({NULL})
  
}else{
  output$summary_breakfasts<-renderDataTable({NULL})
  output$no_bf<-renderUI({column(12,align="center",h4("No added breakfast",style="color:red;"))
    })
}
  
},ignoreNULL = TRUE,ignoreInit = TRUE)

observeEvent(input$see_breakfasts,{
  
  showModal(modalDialog(
    title=fluidRow(column(11,p(style="display:inline;",
                               paste("Breakfasts summmary"))), # titre de ton modal
                   column(1,p(style="display:inline;",
                              actionButton("close_summary",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
    size="l",
    fluidPage(style="margin-bottom:30px;padding:0px;",
              column(12,align="center",
                     dataTableOutput("summary_breakfasts")
              )),
    footer=NULL
  ))
  
    table=DATA$input_table%>%group_by(DATA_name)%>%
      summarize(Cereals_nb=ifelse(unique(Cereals_nb)==1,"Yes","No"),
                Cereals_serving_size=ifelse(Cereals_nb=="Yes",
                                            ifelse(length(unique(Serving_size_cereals))>1,"Variable amounts",paste0(Serving_size_cereals,"g")),""),
                compo1_name=ifelse(unique(Components_nb)>=1,paste0(unique(compo1_name)," (",Serving_size_compo1," g)"),""),
                compo2_name=ifelse(unique(Components_nb)>=2,paste0(unique(compo2_name)," (",Serving_size_compo2," g)"),""),
                compo3_name=ifelse(unique(Components_nb)>=3,paste0(unique(compo3_name)," (",Serving_size_compo3," g)"),""),
                compo4_name=ifelse(unique(Components_nb)>=4,paste0(unique(compo4_name)," (",Serving_size_compo4," g)"),""),
                compo5_name=ifelse(unique(Components_nb)>=5,paste0(unique(compo5_name)," (",Serving_size_compo5," g)"),""),
                file_name_reco=unique(file_name_reco),file_name_cereals=unique(file_name_cereals))%>%
      mutate(Cereals=paste0(Cereals_nb," (",Cereals_serving_size,")"))%>%select(-Cereals_nb,-Cereals_serving_size)%>%
      select(DATA_name,Cereals,everything())
    
    #transpose la table
    table_t<-data.frame(t(table[-1]))
    colnames(table_t) <- table$DATA_name
    
    output$summary_breakfasts<-renderDataTable({
      DT::datatable({table_t},
                    rownames=c('Cereals ?','Component 1','Component 2','Component 3','Component 4','Component 5',
                               'Recommendations file','Cereals file'),
                    options = list(dom='t',pageLength=50,display="compact",autoWidth = TRUE,scrollX=TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#5cb85c', 'color': '#fff'});",
                                     "}"),
                                   columnDefs = list(list(className = 'dt-center', targets = 1:length(table_t))))
      )
    })
  
},ignoreNULL = TRUE,ignoreInit = TRUE)

observeEvent(input$close_summary,{
  output$summary_breakfasts <- renderDataTable({NULL})
  output$no_bf<-renderUI({NULL})
  removeModal(session = getDefaultReactiveDomain())
})


#B.. Comparaison 

observeEvent(input$compare_with1,{
  
  DATA$compare_nb=c(1,DATA$compare_nb[2])
  
  output$modal_comp1<-renderUI({
    tagList(column(12,align="center",
                   column(3,offset=3,selectInput("chosen_portfolio_comp1",h4("Set 2: Breakfast name"),
                                                 as.character(unique(DATA$input_table$DATA_name)),multiple=FALSE)),
                   column(12,uiOutput("filtres_comp1",style="margin-bottom:20px;"))
    ))
  })
  
  output$compare_with<-renderUI({column(6,align="right",
                                        actionBttn(inputId="compare_with2",label="Compare with ...",icon = icon("plus-circle"),
                                                   style = "simple",color = "warning"))
  })
  output$erase_comp<-renderUI({column(6,align="left",
    actionBttn(inputId="erase_comp1",label="Erase comparison",icon = icon("minus-circle"),
                                          style = "simple",color = "danger"))
    })
  
})

#Ajout comparaison 2
observeEvent(input$compare_with2,{
  
  DATA$compare_nb=c(DATA$compare_nb[1],1)
  
  output$modal_comp2<-renderUI({
    tagList(column(12,align="center",
                   column(3,offset=3,selectInput("chosen_portfolio_comp2",h4("Set 3: Breakfast name"),
                                                 as.character(unique(DATA$input_table$DATA_name)),multiple=FALSE)),
                   column(12,uiOutput("filtres_comp2",style="margin-bottom:20px;"))
    ))
  })
  
  output$compare_with<-renderUI({NULL})
  output$erase_comp<-renderUI({actionBttn(inputId="erase_comp2",label="Erase comparison",icon = icon("minus-circle"),
                                          style = "simple",color = "danger")})
  
})



observeEvent(input$chosen_portfolio_comp1,{ 
  
  if(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp1)%>%pull(Cereals_nb)%>%unique()==0){
    
    output$filtres_comp1<-renderUI({NULL})
    
  }else{
    
    region_levels_comp1=as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp1)%>%pull(Region))) #regroupe tous les noms de régions tels qu'écrits dans la colonnes
    DATA$region_levels_comp1 <-unique(unlist(strsplit(region_levels_comp1,","))) #séparer les noms séparés par des , et enlève les doublons
    
    output$filtres_comp1<-renderUI({
      tagList(
        column(12,align="center",
               column(3,
                      selectInput("chosen_type_comp1",h4("Product type"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp1)%>%
                                                                pull(Product_type)))),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_region_comp1",h4("Region"),c("All",DATA$region_levels_comp1),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_brand_comp1",h4("Brand"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp1)%>%
                                                                pull(Brand)))),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_product_comp1",h4("Product name"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp1)%>%
                                                                pull(Product_name)))),selected="All",multiple=FALSE))
        ),
        awesomeCheckbox(inputId = "volume_checkbox_comp1",label =h4("Weight by volume",style="margin-top:-1px;"),
                        value = FALSE,status = "primary"),
        uiOutput("warning_volume_comp1",style="font-color:red;")
      )
    })
  }
  
})

observeEvent(input$chosen_portfolio_comp2,{ 
  
  if(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp2)%>%pull(Cereals_nb)%>%unique()==0){
    
    output$filtres_comp2<-renderUI({NULL})
    
  }else{
    
    region_levels_comp2=as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp2)%>%pull(Region))) #regroupe tous les noms de régions tels qu'écrits dans la colonnes
    DATA$region_levels_comp2<-unique(unlist(strsplit(region_levels_comp2,","))) #séparer les noms séparés par des , et enlève les doublons
    
    output$filtres_comp2<-renderUI({
      tagList(
        column(12,align="center",
               column(3,
                      selectInput("chosen_type_comp2",h4("Product type"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp2)%>%
                                                                pull(Product_type)))),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_region_comp2",h4("Region"),c("All",DATA$region_levels_comp2),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_brand_comp2",h4("Brand"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp2)%>%
                                                                pull(Brand)))),selected="All",multiple=TRUE)),
               column(3,
                      selectInput("chosen_product_comp2",h4("Product name"),
                                  c("All",as.character(unique(DATA$input_table%>%filter(DATA_name%in%input$chosen_portfolio_comp2)%>%
                                                                pull(Product_name)))),selected="All",multiple=FALSE))
        ),
        awesomeCheckbox(inputId = "volume_checkbox_comp2",label =h4("Weight by volume",style="margin-top:-1px;"),
                        value =FALSE,status = "primary"),
        uiOutput("warning_volume_comp2",style="font-color:red;")
      )
    })
  }
  
})

observeEvent(input$erase_comp1,{
  output$modal_comp1<-renderUI({NULL})
  output$compare_with<-renderUI({actionBttn(inputId="compare_with1",label="Compare with ...",icon = icon("plus-circle"),
                                            style = "simple",color = "warning")})
  output$erase_comp<-renderUI({NULL})
  DATA$compare_nb=c(0,DATA$compare_nb[2])
})

observeEvent(input$erase_comp2,{
  output$modal_comp2<-renderUI({NULL})
  output$compare_with<-renderUI({column(6,align="right",
                                        actionBttn(inputId="compare_with2",label="Compare with ...",icon = icon("plus-circle"),
                                            style = "simple",color = "warning"))
                                        })
  output$erase_comp<-renderUI({column(6,align="left",
                                      actionBttn(inputId="erase_comp1",label="Erase comparison",icon = icon("minus-circle"),
                                          style = "simple",color = "danger"))
    })
  DATA$compare_nb=c(DATA$compare_nb[1],0)
})

########################################################## C GRAPHIQUES   ############################################################################# 
#C.1. Validation des paramètres et calcul des graphiques ####

observeEvent(input$valid_param,{

   #determine le nombre de comparaisons -> nombre de graphiques à afficher
  compare_pos=which(DATA$compare_nb==1) 
  nb_graph=length(compare_pos)+1
  
  #initialisation
  
  list_input_table=list()
  vec_volume_checkbox=NULL
  erreur=NULL
  
  #BOUCLE SUR LE NOMBRE DE GRAPHIQUES A COMPARER
  for (i in 1:nb_graph){
    if(!TRUE%in%erreur){ #on continue tant qu'il n'y a pas d'erreur
    
  suff=ifelse(i==1,"", # definition d'un suffixe pour les variables
                ifelse(i==2,"_comp1",
                      ifelse(i==3,"_comp2",NA)))
   
  if (!(is.null(input[[paste0("chosen_portfolio",suff)]]))&!(is.null(input$selected_nut))){
  
  erreur[i]=FALSE  
  
  selected_nut=input$selected_nut
  # Rajout de bornes supérieurs à la sélections pour les nutriments Energy, Carb et Fat
  if ("Energy.kcal_dej_lower.pct"%in%input$selected_nut){
    selected_nut=c(selected_nut,"Energy.kcal_dej_upper.pct","Energy.kcal_dej_fixed.pct")
  }
  if ("Carbohydrates.g_dej_lower.pctKCAL"%in%input$selected_nut){
    selected_nut=c(selected_nut,"Carbohydrates.g_dej_upper.pctKCAL")
  }
  if ("Fat.g_dej_lower.pctKCAL"%in%input$selected_nut){
    selected_nut=c(selected_nut,"Fat.g_dej_upper.pctKCAL")
  }
  
  selected_nut_abs= sub("_lower.pctKCAL|_upper.pctKCAL|_fixed.pctKCAL|_lower.pct|_upper.pct|_fixed.pct",
                        "",selected_nut) # noms de colonnes des nutriments en valeur absolue

 # Regarde si le petit dej comprend des céréales ou pas
  
  x=input[[paste0("chosen_portfolio",suff)]]
  assign(paste0("chosen_portfolio",suff),paste(x,collapse=",")) #variable à afficher + tard
  
  input_table=DATA$input_table%>%filter(DATA_name%in%x)
  cereals_nb=input_table%>%pull(Cereals_nb)%>%unique()
  
# Sans céreales 
  if (cereals_nb==0){
    
    df1 <-input_table%>%select(DATA_name,selected_nut_abs,selected_nut)%>%
      dplyr::rename("Breakfast name"=DATA_name)%>%
      dplyr::rename_at(vars(selected_nut_abs), ~ paste(.,"(Absolute value)"))%>%
      dplyr::rename_at(vars(selected_nut), ~ sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",.))%>%
      dplyr::rename_at(vars(sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",selected_nut)), 
                ~ sub("_upper.pctKCAL|_upper.pct"," (% of maximum breakfast recommendation)",.))
    
    this.name <- paste0("EXPORT_TABLE",suff)
    DATA[[this.name]] <- df1
    
  # Affiliation de la variable volume pour le graphique
    input_volume=FALSE
    assign(paste0("volume_checkbox",suff),FALSE)
 
# Avec céréales : 
  
  }else if (cereals_nb==1){
            
   if (!(is.null(input[[paste0("chosen_product",suff)]]))&!(is.null(input[[paste0("chosen_type",suff)]]))&
       !(is.null(input[[paste0("chosen_region",suff)]]))&!(is.null(input[[paste0("chosen_brand",suff)]]))&
       !(is.null(input[[paste0("volume_checkbox",suff)]]))){ # si tous les inputs sont ok
  
  erreur[i]=FALSE
  
  x=input[[paste0("volume_checkbox",suff)]]
  assign(paste0("volume_checkbox",suff),x)
  input_volume=x
 
  # Determine variables pour filter correctement sur le tableau des petits dej
  
    if ("All"%in%input[[paste0("chosen_type",suff)]]){
      chosen_type=unique(input_table$Product_type)
    }else{
      chosen_type=input[[paste0("chosen_type",suff)]]
    } 
    if ("All"%in%input[[paste0("chosen_region",suff)]]){
      chosen_region= DATA[[paste0("region_levels",suff)]]
    }else{
      chosen_region=input[[paste0("chosen_region",suff)]]
    }
    if ("All"%in%input[[paste0("chosen_brand",suff)]]){
      chosen_brand= unique(input_table$Brand)
    }else{
      chosen_brand=input[[paste0("chosen_brand",suff)]]
    }
   if ("All"%in%input[[paste0("chosen_product",suff)]]){
     chosen_product=unique(input_table$Product_name)
   }else{
     chosen_product=input[[paste0("chosen_product",suff)]]
   }
     
  #Assigne les variables à écrire dans les encadrés plus tard: variable unique par set 
  
    x1=input[[paste0("chosen_type",suff)]] 
    x2=input[[paste0("chosen_region",suff)]]  
    x3=input[[paste0("chosen_brand",suff)]]  
    x4=input[[paste0("chosen_product",suff)]] 
    
    assign(paste0("chosen_type_char",suff),paste(x1,collapse=",")) 
    assign(paste0("chosen_region_char",suff),paste(x2,collapse=","))
    assign(paste0("chosen_brand_char",suff),paste(x3,collapse=",")) 
    assign(paste0("chosen_product_char",suff),paste(x4,collapse=",")) 
    
     df1 <-input_table%>%filter(Product_name%in%chosen_product & Product_type%in%chosen_type & 
                                                   Brand%in%chosen_brand & grepl(paste(chosen_region, collapse="|"),Region))%>%
      select(CP_ID,volume,Region,Product_type,Brand,Product_name,selected_nut_abs,selected_nut)%>%
      dplyr::rename_at(vars(selected_nut_abs), ~ paste(.,"(Absolute value)"))%>%
      dplyr::rename_at(vars(selected_nut), ~ sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",.))%>%
      dplyr::rename_at(vars(sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",selected_nut)), 
                ~ sub("_upper.pctKCAL|_upper.pct"," (% of maximum breakfast recommendation)",.))
    
    this.name <- paste0("EXPORT_TABLE",suff)
    DATA[[this.name]] <- df1
    
    nb_product=nrow(DATA[[paste0("EXPORT_TABLE",suff)]])

  if(nb_product!=0){
  
  erreur[i]=FALSE  
     
  # Table utilisé par la fonction de création des graphiques
   input_table=input_table%>%filter(Product_name%in%chosen_product &Product_type%in%chosen_type & Brand%in%chosen_brand & 
                                         grepl(paste(chosen_region, collapse="|"),Region))
   
      
    }else{ # si pas de petits déjeuners dans ces filtres
 erreur[i]=TRUE
 
   chosen_type=input[[paste0("chosen_type",suff)]]
   chosen_region=input[[paste0("chosen_region",suff)]]
   chosen_brand=input[[paste0("chosen_brand",suff)]]
   chosen_product=input[[paste0("chosen_product",suff)]]
      
   output[[paste0("erreur_msg",suff)]]<-renderUI({
      fluidPage(column(12,align="center",
                       h4 (paste("Set",i,": No breakfast of type",paste(chosen_type,collapse=", "),
                                 "in region",paste(chosen_region,collapse=", "),
                                 "and brand",paste(chosen_brand,collapse=", "),
                                 "named",paste(chosen_product,collapse=", ")), style="color:red;")))
      })
      
    }

}else{ # si cereals==1 et si un des filtres est nul
    
    output[[paste0("erreur_msg",suff)]]<-renderUI({
      fluidPage(column(12,align="center",
                       h4 (paste("Please fill all the settings for set",i), style="color:red;")))
    })
erreur[i]=TRUE
}
  
}# end cereals ==1
 
  #Arguments à conserver pour le plot des graphiques 
  list_input_table[[i]]=input_table
  vec_volume_checkbox=c(vec_volume_checkbox,input_volume)
  
  }else{ #si le nom de petit dej ou les nutriments selectionnnés sont nuls
    
    output[[paste0("erreur_msg",suff)]]<-renderUI({
      fluidPage(column(12,align="center",
                       h4 (paste("Please fill all the settings for set",i), style="color:red;")))
      erreur[i]=TRUE
    })
  }
  
  
}} #fin de la boucle sur le nombre de comparaisons/ de graphiques 
  

#C.2. Affichage des graphiques hors de la boucle ####
# En fonction du nombre de comparaison : 1,2 ou 3
 
  
if (!TRUE%in%erreur){#si pas d'erreur

#"Nettoie" l'ancien contenu de onglets 
output$onglet_means_col1<-renderUI({NULL})
output$onglet_means_col2<-renderUI({NULL})
output$onglet_means_col3<-renderUI({NULL})
output$onglet_means2<-renderUI({NULL})
output$onglet_compliance_col1<-renderUI({NULL})
output$onglet_compliance_col2<-renderUI({NULL})
output$onglet_compliance_col3<-renderUI({NULL}) 
output$download_btn <- renderUI({NULL})
output$download_btn_comp1<- renderUI({NULL})
output$download_btn_comp2<- renderUI({NULL})

# Taille de l'encadré change en fonction du nombre de graph -> changement colonne bootstrap   
col=ifelse(nb_graph==1,12,ifelse(nb_graph==2,6,ifelse(nb_graph==3,4)))  

# Set 1 

cereals_nb1=DATA$input_table%>%filter(DATA_name%in%chosen_portfolio)%>%pull(Cereals_nb)%>%unique()
if(cereals_nb1==0){
  
  output$download_btn<- renderUI({tagList(
    column(col,
           h3("Set 1 : 1 breakfast"),
           downloadButton('download_results','',class='btn btn-success'),
           column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                                "Breakfast name :",chosen_portfolio,"<br>")))
    ))
  }) 
  
  
}else if(cereals_nb1==1){

nb_product1=nrow(DATA$EXPORT_TABLE)

output$download_btn<- renderUI({tagList(
  column(col,
         h3("Set 1:",nb_product1, "breakfasts"),
         downloadButton('download_results','',class='btn btn-success'),
         column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                        "Breakfast name :",chosen_portfolio,"<br>
                        Product name :",chosen_product_char,"<br>
                        Type :",chosen_type_char,"<br>
                        Region :",chosen_region_char,"<br>
                        Brand :",chosen_brand_char,"<br>
                        Weighted by volume :",volume_checkbox,"</h4></p>")))
         
         ))
})
}

# Boutons téléchargements des résultats 
output$download_results<-downloadHandler(filename = function() { paste("Breakfast 1", Sys.Date(), ".csv", sep="")},
                                                           content = function(file) {
                                                             write.csv2(DATA$EXPORT_TABLE, file)
                                                          })

# Set 2 
if(nb_graph>=2){
  
cereals_nb2=DATA$input_table%>%filter(DATA_name%in%chosen_portfolio_comp1)%>%pull(Cereals_nb)%>%unique()
if(cereals_nb2==0){
  
  output$download_btn_comp1<- renderUI({tagList(
    column(col,
           h3("Set 2 : 1 breakfast"),
           downloadButton('download_results_comp1','',class='btn btn-success'),
           column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                                "Breakfast name :",chosen_portfolio_comp1,"<br>")))
    ))
  }) 
  
  
}else if(cereals_nb2==1){
  
  nb_product2=nrow(DATA$EXPORT_TABLE_comp1)
  
  output$download_btn_comp1<- renderUI({tagList(
    column(col,
           h3("Set 2:",nb_product2, "breakfasts"),
           downloadButton('download_results_comp1','',class='btn btn-success'),
           column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                                "Breakfast name :",chosen_portfolio_comp1,"<br>
                                Product name :",chosen_product_char_comp1,"<br>
                                Type :",chosen_type_char_comp1,"<br>
                                Region :",chosen_region_char_comp1,"<br>
                                Brand :",chosen_brand_char_comp1,"<br>
                                Weighted by volume :",volume_checkbox_comp1,"</h4></p>")))
           
           ))
  })
}
# Boutons téléchargements des résultats 
output$download_results_comp1<-downloadHandler(filename = function() { paste("Breakfast 2", Sys.Date(), ".csv", sep="")},
                                         content = function(file) {
                                           write.csv2(DATA$EXPORT_TABLE_comp1, file)
                                         })
}

# Set 3 
if(nb_graph>=3){
  
  cereals_nb3=DATA$input_table%>%filter(DATA_name%in%chosen_portfolio_comp2)%>%pull(Cereals_nb)%>%unique()
  if(cereals_nb3==0){
    
    output$download_btn_comp2<- renderUI({tagList(
      column(col,
             h3("Set 3 : 1 breakfast"),
             downloadButton('download_results_comp2','',class='btn btn-success'),
             column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                                  "Breakfast name :",chosen_portfolio_comp2,"<br>")))
      ))
    }) 
    
    
  }else if(cereals_nb3==1){
    
    nb_product3=nrow(DATA$EXPORT_TABLE_comp2)
    
    output$download_btn_comp2<- renderUI({tagList(
      column(col,
             h3("Set 3:",nb_product3, "breakfasts"),
             downloadButton('download_results_comp2','',class='btn btn-success'),
             column(12,HTML(paste("<p><h4 class='bordered col-sm-6 col-sm-offset-3'>",
                                  "Breakfast name :",chosen_portfolio_comp2,"<br>
                                Product name :",chosen_product_char_comp2,"<br>
                                Type :",chosen_type_char_comp2,"<br>
                                Region :",chosen_region_char_comp2,"<br>
                                Brand :",chosen_brand_char_comp2,"<br>
                                Weighted by volume :",volume_checkbox_comp2,"</h4></p>")))
             
      ))
    })
  }
  # Boutons téléchargements des résultats 
  output$download_results_comp2<-downloadHandler(filename = function() { paste("Breakfast 3", Sys.Date(), ".csv", sep="")},
                                           content = function(file) {
                                             write.csv2(DATA$EXPORT_TABLE_comp2, file)
                                           })
}
 
# Calcul des graphiques avec petits dej tous sur le même 
DATA$graph_all=plot_dej(list_input_table,vec_volume_checkbox,selected_nut=selected_nut,compare=TRUE)


#Calcul des graphiques séparés --> max à déterminer

nut_colnames2=c("Energy.kcal_dej_fixed.pct","Energy.kcal_dej_lower.pct","Energy.kcal_dej_upper.pct","Proteins.g_dej_lower.pct", 
               "Carbohydrates.g_dej_lower.pctKCAL","Carbohydrates.g_dej_upper.pctKCAL","Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL", 
               "Fat.g_dej_lower.pctKCAL","Fat.g_dej_upper.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
               "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
               "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
               "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct")
#Ajout des colonnes dialy
nut_colnamesDAILY2=paste0(gsub("KCAL","",nut_colnames2),"DAILY") 
nut_colnames2=c(nut_colnames2,nut_colnamesDAILY2)

macro_nut=nut_colnames2[2:11] #on inclut pas la reco énergie fixe --> utilisée pour DAILY
vit_nut=nut_colnames2[12:20]
min_nut=nut_colnames2[21:25]
macro_nutDAILY=nut_colnames2[26:36]
vit_nutDAILY=nut_colnames2[37:45]
min_nutDAILY=nut_colnames2[46:50]

bind_table=NULL
for (i in 1:nb_graph){

if(vec_volume_checkbox[i]==FALSE){
  bind_table=bind_rows(bind_table,list_input_table[[i]]%>%mutate(volume=1))
}
else{
  bind_table=bind_rows(bind_table,list_input_table[[i]]%>%select(everything(),volume))
}
}

bind_table=bind_table%>%select(CP_ID,nut_colnames2,everything())%>%
  gather("Nutrient","Value",nut_colnames2)%>%group_by(Nutrient,DATA_name)%>%
  dplyr::summarise(Mean.pct_vol=weighted.mean(Value,volume))

max_ordinate_macro=bind_table%>%filter(Nutrient%in%macro_nut)%>%pull(Mean.pct_vol)%>%max()
max_ordinate_vit=bind_table%>%filter(Nutrient%in%vit_nut)%>%pull(Mean.pct_vol)%>%max()
max_ordinate_min=bind_table%>%filter(Nutrient%in%min_nut)%>%pull(Mean.pct_vol)%>%max()
max_ordinate_macroDAILY=bind_table%>%filter(Nutrient%in%macro_nutDAILY[c(-2,-3)])%>%pull(Mean.pct_vol)%>%max()
max_ordinate_vitDAILY=bind_table%>%filter(Nutrient%in%vit_nutDAILY)%>%pull(Mean.pct_vol)%>%max()
max_ordinate_minDAILY=bind_table%>%filter(Nutrient%in%min_nutDAILY)%>%pull(Mean.pct_vol)%>%max()

max_ordinate_graph=c(max_ordinate_macro,max_ordinate_vit,max_ordinate_min,max_ordinate_macroDAILY,max_ordinate_vitDAILY,max_ordinate_minDAILY)

DATA$graph=plot_dej(list_input_table[[1]],vec_volume_checkbox[1],selected_nut=selected_nut,max_ordinate=max_ordinate_graph)
if(nb_graph>=2){
DATA$graph_comp1=plot_dej(list_input_table[[2]],vec_volume_checkbox[2],selected_nut=selected_nut,max_ordinate=max_ordinate_graph)
}
if(nb_graph>=3){
DATA$graph_comp2=plot_dej(list_input_table[[3]],vec_volume_checkbox[3],selected_nut=selected_nut,max_ordinate=max_ordinate_graph)
}

# Affichage des graphiques
graph_UI(input,output,session,DATA,nb_graph)

# Montre les onglets (si ils étaient cachés au début)
showElement("div_analysis")   

}
   
})



## C.1. Mise en place des filtres de comparaison ###

# observeEvent(input$compare_with1,{
#   showModal(modalDialog(
#     title=fluidRow(column(11,p(style="display:inline;",
#                                "Compare 2 different sets of breakfast")), # titre de ton modal
#                    column(1,p(style="display:inline;",
#                               actionButton("close_graph",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
#     size="l",
#     fluidPage(style="margin-bottom:30px;",
#               uiOutput("modal_comp")
#     ),
#     footer=NULL,easyClose = FALSE
#   ))
#   
#   list_nut=c("Energy.kcal_dej_lower.pct","Proteins.g_dej_lower.pct","Carbohydrates.g_dej_lower.pctKCAL",
#              "Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL","Fat.g_dej_lower.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
#              "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
#              "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
#              "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct")
#   label_nut=c("Energy","Protein","Carbohydrates *","Fibre","Added sugars *",
#               "Fat *", "Saturated fat *",
#               "Vitamin A","Vitamin B1", "Vitamin B2", "Vitamin B3", "Vitamin B6","Vitamin B9","Vitamin B12","Vitamin C", "Vitamin D",
#               "Calcium","Iron","Magnesium","Sodium","Zinc")
#   
#   
#   output$modal_comp<-renderUI({column(12,align="center",
#                                       h3("Set 1"),br(),
#                                       column(12,
#                                              column(3,
#                                                     selectInput("chosen_portfolio_comp1",
#                                                                 h4("Breakfast name"),unique(DATA$input_table$DATA_name),
#                                                     selected=thechoice_portfolio())),
#                                              column(9,
#                                                     uiOutput("filtres_comp1")),
#                                              style="margin-bottom:20px;"),
#                                       awesomeCheckbox(inputId = "volume_checkbox_comp1",
#                                                       label =h4("Weight by volume",style="margin-top:-1px;"),
#                                                       value = thechoice_volume(),status = "primary"),
#                                       uiOutput("warning_volume_comp1",style="font-color:red;"),
#                                       
#                                       h3("Set 2"),br(),
#                                       column(12,
#                                              column(3,
#                                                     selectInput("chosen_portfolio_comp2",
#                                                                 h4("Breakfast name"),unique(DATA$input_table$DATA_name),
#                                                                 selected=thechoice_portfolio2())),
#                                              column(9,
#                                                     uiOutput("filtres_comp2")),
#                                              style="margin-bottom:20px;"),
#                                       awesomeCheckbox(inputId = "volume_checkbox_comp2",label =h4("Weight by volume",style="margin-top:-1px;"),value = TRUE,status = "primary"),
#                                       uiOutput("warning_volume_comp2",style="font-color:red;"),            
#                                       
#                                       
#                                       checkboxGroupButtons(inputId = "selected_nut_comp",
#                                                            label = h4("Nutrients :"),
#                                                            choiceNames =label_nut,
#                                                            choiceValues= list_nut,
#                                                            selected=list_nut
#                                                            # status = "primary",
#                                                            checkIcon = list(
#                                                              yes = tags$i(class = "fa fa-check-square", 
#                                                                           style = "color: steelblue"),
#                                                              no = tags$i(class = "fa fa-square-o", 
#                                                                          style = "color: steelblue"))
#                                       ),#),
#                                       h6("*Energy fixed at recommended value for breakfast"),
#                                       column(12,style="margin-top:20px;",
#                                              actionBttn(inputId="valid_compare",label="Confirm Comparaison",icon = icon("gear"),style = "simple",color = "primary"),
#                                              br(),
#                                              uiOutput("erreur_comp",style="font-color:red;")),br()
#   )})
#   
#   output$erreur_comp<-renderUI({NULL})
#   
# },ignoreInit=TRUE)

#2 Synchronisation des output
# obligé de synchroniser aussi 2ème filtres avec eux même sinon disparaissent quand on change la selection de nutriments (va savoir pk ...)

#  observeEvent(input$chosen_portfolio,{
#    thechoice_portfolio(input$chosen_portfolio)
#  })
# 
# observeEvent(input$chosen_type,{
#   thechoice_type(input$chosen_type)
# })
# 
# observeEvent(input$chosen_region,{
#   thechoice_region(input$chosen_region)
# })
# 
# observeEvent(input$chosen_brand,{
#   thechoice_brand(input$chosen_brand)
# })
# 
# observeEvent(input$chosen_product,{
#   thechoice_product(input$chosen_product)
# })
# 
# observeEvent(input$volume_checkbox,{
#   thechoice_volume(input$volume_checkbox)
# })
# 
#  observeEvent(input$chosen_portfolio_comp2,{
#    thechoice_portfolio2(input$chosen_portfolio_comp2)
#  })
# 
# observeEvent(input$chosen_type_comp2,{
#   thechoice_type2(input$chosen_type_comp2)
# })
# 
# observeEvent(input$chosen_region_comp2,{
#   thechoice_region2(input$chosen_region_comp2)
# })
# 
# observeEvent(input$chosen_brand_comp2,{
#   thechoice_brand2(input$chosen_brand_comp2)
# })
# 
# observeEvent(input$chosen_product_comp2,{
#   thechoice_product2(input$chosen_product_comp2)
# })
# 
# observeEvent(input$volume_checkbox_comp2,{
#   thechoice_volume2(input$volume_checkbox_comp2)
# })
# 
#  observeEvent(input$chosen_portfolio_comp1,{
#    thechoice_portfolio(input$chosen_portfolio_comp1)
#  })
# 
# observeEvent(input$chosen_type_comp1,{
#   thechoice_type(input$chosen_type_comp1)
# })
# 
# observeEvent(input$chosen_region_comp1,{
#   thechoice_region(input$chosen_region_comp1)
# })
# 
# observeEvent(input$chosen_brand_comp1,{
#   thechoice_brand(input$chosen_brand_comp1)
# })
# 
# observeEvent(input$chosen_product_comp1,{
#   thechoice_product(input$chosen_product_comp1)
# })
# 
# observeEvent(input$volume_checkbox_comp1,{
#   thechoice_volume(input$volume_checkbox_comp1)
# })
# 
# observeEvent(input$selected_nut,{
#   thechoice_nut(input$selected_nut)
# })
# 
# observeEvent(input$selected_nut_comp,{
#   thechoice_nut(input$selected_nut_comp)
# })


## C.2. Validation de la comparaison et re-calcul des graphiques ###

# observeEvent(input$valid_compare,{
#   
#   if(#!(is.null(input$chosen_age_comp1))& !(is.null(input$chosen_age_comp2))&
#      !(is.null(input$chosen_portfolio_comp1))& !(is.null(input$chosen_portfolio_comp2))&
#      !(is.null(input$chosen_type_comp1))&!(is.null(input$chosen_region_comp1))&
#      !(is.null(input$chosen_brand_comp1))&!(is.null(input$volume_checkbox_comp1))&
#      !(is.null(input$chosen_type_comp2))&!(is.null(input$chosen_region_comp2))&
#      !(is.null(input$chosen_brand_comp2))&!(is.null(input$volume_checkbox_comp2))&!(is.null(input$selected_nut_comp))){
#     
#     
#     #Graphiques de gauche   
#     volume_checkbox_comp1=input$volume_checkbox_comp1
#   #  chosen_age_comp1=input$chosen_age_comp1
#     chosen_portfolio_comp1=input$chosen_portfolio_comp1
#     
#     if ("All"%in%input$chosen_type_comp1){
#       chosen_type_comp1=unique(DATA$input_table$Product_type)
#     }else{
#       chosen_type_comp1=input$chosen_type_comp1
#     } 
#     if ("All"%in%input$chosen_region_comp1){
#       chosen_region_comp1= DATA$region_levels
#     }else{
#       chosen_region_comp1=input$chosen_region_comp1
#     }
#     if ("All"%in%input$chosen_brand_comp1){
#       chosen_brand_comp1= unique(DATA$input_table$Brand)
#     }else{
#       chosen_brand_comp1=input$chosen_brand_comp1
#     }
#     #A mettre dans les titres :
#     chosen_type_comp1_char=input$chosen_type_comp1
#     chosen_region_comp1_char=input$chosen_region_comp1
#     chosen_brand_comp1_char=input$chosen_brand_comp1
#     
#     #Graphiques de droite  
#     volume_checkbox_comp2=input$volume_checkbox_comp2
#    # chosen_age_comp2=input$chosen_age_comp2
#     chosen_portfolio_comp2=input$chosen_portfolio_comp2
#     
#     if ("All"%in%input$chosen_type_comp2){
#       chosen_type_comp2=unique(DATA$input_table$Product_type)
#     }else{
#       chosen_type_comp2=input$chosen_type_comp2
#     } 
#     if ("All"%in%input$chosen_region_comp2){
#       chosen_region_comp2= DATA$region_levels_comp1
#     }else{
#       chosen_region_comp2=input$chosen_region_comp2
#     }
#     if ("All"%in%input$chosen_brand_comp2){
#       chosen_brand_comp2= unique(DATA$input_table$Brand)
#     }else{
#       chosen_brand_comp2=input$chosen_brand_comp2
#     }
#     #A mettre dans les titres :
#     chosen_type_comp2_char=input$chosen_type_comp2
#     chosen_region_comp2_char=input$chosen_region_comp2
#     chosen_brand_comp2_char=input$chosen_brand_comp2
#     
#     
#     selected_nut_comp=input$selected_nut_comp
#     # Rajout de bornes supérieurs à la sélections pour les nutriments Energy, Carb et Fat
#     if ("Energy.kcal_dej_lower.pct"%in%input$selected_nut_comp){
#       selected_nut_comp=c(selected_nut_comp,"Energy.kcal_dej_upper.pct","Energy.kcal_dej_fixed.pct")
#     }
#     if ("Carbohydrates.g_dej_lower.pctKCAL"%in%input$selected_nut_comp){
#       selected_nut_comp=c(selected_nut_comp,"Carbohydrates.g_dej_upper.pctKCAL")
#     }
#     if ("Fat.g_dej_lower.pctKCAL"%in%input$selected_nut_comp){
#       selected_nut_comp=c(selected_nut_comp,"Fat.g_dej_upper.pctKCAL")
#     }
#     
#     
#     # Formate la table à exporter pour l'utilisateur
#     DATA$EXPORT_TABLE1 <-DATA$input_table
#     DATA$EXPORT_TABLE2 <-DATA$input_table
#     
#     # if (chosen_age_comp1=="Children"){
#     #   colnames(DATA$EXPORT_TABLE1) <- sub("300", "KCAL", colnames(DATA$EXPORT_TABLE1))
#     # } else{
#     #   colnames(DATA$EXPORT_TABLE1) <- sub("400", "KCAL", colnames(DATA$EXPORT_TABLE1))    
#     # } 
#     # 
#     # if (chosen_age_comp2=="Children"){
#     #   colnames(DATA$EXPORT_TABLE2) <- sub("300", "KCAL", colnames(DATA$EXPORT_TABLE2))
#     # } else{
#     #   colnames(DATA$EXPORT_TABLE2) <- sub("400", "KCAL", colnames(DATA$EXPORT_TABLE2))    
#     # } 
#     
#     selected_nut_abs= sub("_lower.pctKCAL|_upper.pctKCAL|_lower.pct|_upper.pct|_fixed.pctKCAL|_fixed.pct",
#                           "",selected_nut_comp) # téléchargement des nutriments en valeur absolue
#     
#     
#     DATA$EXPORT_TABLE1 <-DATA$EXPORT_TABLE1%>%filter(#Age%in%chosen_age_comp1 & 
#                                                     DATA_name%in%chosen_portfolio_comp1&Product_type%in%chosen_type_comp1 & 
#                                                    Brand%in%chosen_brand_comp1 & grepl(paste(chosen_region_comp1, collapse="|"),Region))%>%
#       select(CP_ID,volume,#Age,
#              Region,Product_type,Brand,Product_name,selected_nut_abs,selected_nut_comp)%>%
#       dplyr::rename_at(vars(selected_nut_abs), ~ paste(.,"(Absolute value)"))%>%
#       dplyr::rename_at(vars(selected_nut_comp), ~ sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",.))%>%
#       dplyr::rename_at(vars(sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",selected_nut_comp)), 
#                 ~ sub("_upper.pctKCAL|_upper.pct"," (% of maximum breakfast recommendation)",.))
#     
#     DATA$EXPORT_TABLE2 <-DATA$EXPORT_TABLE2%>%filter(#Age%in%chosen_age_comp2 & 
#                                                      DATA_name%in%chosen_portfolio_comp2&Product_type%in%chosen_type_comp2 & 
#                                                      Brand%in%chosen_brand_comp2 & grepl(paste(chosen_region_comp2, collapse="|"),Region))%>%
#       select(CP_ID,volume,#Age,
#              Region,Product_type,Brand,Product_name,selected_nut_abs,selected_nut_comp)%>%
#       dplyr::rename_at(vars(selected_nut_abs), ~ paste(.,"(Absolute value)"))%>%
#       dplyr::rename_at(vars(selected_nut_comp), ~ sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",.))%>%
#       dplyr::rename_at(vars(sub("_lower.pctKCAL|_lower.pct"," (% of minimum breakfast recommendation)",selected_nut_comp)), 
#                 ~ sub("_upper.pctKCAL|_upper.pct"," (% of maximum breakfast recommendation)",.))
#     
#     nb_product1=nrow(DATA$EXPORT_TABLE1)
#     nb_product2=nrow(DATA$EXPORT_TABLE2)
#     
#     if(nb_product1!=0 & nb_product2!=0){
#       
#       
#       # Boutons téléchargements des résultats (au dessus des pannels 1 et 2)
#       output$download_btn<- renderUI({tagList(
#         column(6,
#                column(6,align="right",h3(paste("Set 1 :",nb_product1,"breakfasts"))),
#                column(6,align="left",downloadButton('download_results','',class='btn btn-success',style="margin-top:15px;")),
#                column(12,HTML(paste("<p><h4 class='bordered col-sm-8 col-sm-offset-2'>",
#                                    # "Age :",chosen_age_comp1,"<br>",
#                                    "Breakfast name :",paste(chosen_portfolio_comp1,collapse=","),"<br>
#                                     Type :",paste(chosen_type_comp1_char,collapse=","),"<br>
#                                     Region :",paste(chosen_region_comp1_char,collapse=","),"<br>
#                                     Brand :",paste(chosen_brand_comp1_char,collapse=","),"<br>
#                                     Weighted by volume :",as.character(volume_checkbox_comp1),"</h4></p>")))#,
#                # sidebarPanel(
#                #   sliderInput("width", "width in inches:", min = 2, max = 10, value = 6),
#                #   sliderInput("height", "height in inches:", min = 2, max = 10, value = 6)
#                # )
#                #br(),(column(12,actionBttn(inputId="compare_with2",label="Compare with ...",icon = icon("plus-circle"),
#                #                           style = "simple",color = "warning")))
#                
#                ))
#       })
#       
#       
#       # Création du fichier
#       output$download_results<-downloadHandler(filename = function() { paste("Breakfast-Set1", Sys.Date(), ".csv", sep="")},
#                                                content = function(file) {
#                                                  write.csv2(DATA$EXPORT_TABLE1, file)
#                                                })
#       
#       
#       
#       # Boutons téléchargements des résultats (au dessus des pannels 1 et 2)
#       output$download_btn2<- renderUI({
#         tagList(
#           column(6,
#                  column(6,align="right",h3(paste("Set 2 :",nb_product2,"breakfasts"))),
#                  column(6,align="left",downloadButton('download_results2','',class='btn btn-success',style="margin-top:15px;")),
#                  HTML(paste("<p><h4 class='bordered col-sm-8 col-sm-offset-2'>",
#                             #"Age :",chosen_age_comp2,"<br>",
#                            "Breakfast name :",paste(chosen_portfolio_comp2,collapse=","),"<br>
#                             Type :",paste(chosen_type_comp2_char,collapse=","),"<br>
#                             Region :",paste(chosen_region_comp2_char,collapse=","),"<br>
#                             Brand :",paste(chosen_brand_comp2_char,collapse=","),"<br>
#                             Weighted by volume :",as.character(volume_checkbox_comp2),"</h4></p>"))
#                  
#                  ))
#       })
#       
#       
#       # Création du fichier 
#       output$download_results2<-downloadHandler(filename = function() { paste("Breakfast-Set2-", Sys.Date(), ".csv", sep="")},
#                                                 content = function(file) {
#                                                   write.csv2(DATA$EXPORT_TABLE2, file)
#                                                 })
#       
#       
#       #Filtres :
#       
#       input_dat=DATA$input_table%>%filter(#Age%in%chosen_age_comp1 & 
#                                            DATA_name%in%chosen_portfolio_comp1 & Product_type%in%chosen_type_comp1 & 
#                                              Brand%in%chosen_brand_comp1 & grepl(paste(chosen_region_comp1, collapse="|"),Region))
#       
#       input_dat2=DATA$input_table%>%filter(#Age%in%chosen_age_comp2 & 
#                                            DATA_name%in%chosen_portfolio_comp2 & Product_type%in%chosen_type_comp2 & 
#                                            Brand%in%chosen_brand_comp2 & grepl(paste(chosen_region_comp2, collapse="|"),Region))
#       
#       # Appel de la fonction pour créer tous les graphiques
#       DATA$graph=plot_dej(input_dat,volume=volume_checkbox_comp1,selected_nut=selected_nut_comp)
#       DATA$graph2=plot_dej(input_dat2,volume=volume_checkbox_comp2,selected_nut=selected_nut_comp)
#       
#       # graph_test<-plot_dej(list(input_dat,input_dat2),volume=c(volume_checkbox_comp1,volume_checkbox_comp2),selected_nut=selected_nut_comp,
#       #                       compare=TRUE)
#       
#       ## C.3. Affichages des graphiques ###
#       
#       graph_comp_UI(input,output,session,DATA)
#       
#       #Passage à l'autre onglet 
#       updateTabsetPanel(session,"cpw_app",selected="analysis")
#       showElement("div_analysis")
#       
#       #Ferme le modal
#       removeModal(session = getDefaultReactiveDomain())
#       
#       # js$style500px_remove_RG()
#       
#       
#       
#     }else{
#       
#       # output$onglet_means<-renderUI({NULL})
#       # output$onglet_compliance<-renderUI({NULL})
#       # output$download_btn<- renderUI({NULL})
#       # output$download_btn2<- renderUI({NULL})
#       
#       chosen_type_comp1=input$chosen_type_comp1   
#       chosen_region_comp1=input$chosen_region_comp1
#       chosen_brand_comp1=input$chosen_brand_comp1
#       chosen_type_comp2=input$chosen_type_comp2  
#       chosen_region_comp2=input$chosen_region_comp2
#       chosen_brand_comp2=input$chosen_brand_comp2
#       
#       output$erreur_comp<-renderUI({
#         if(nrow(DATA$EXPORT_TABLE1)==0){
#           fluidPage(column(12,align="center",
#                            h4 (paste("Set 1 : No breakfast of type",paste(chosen_type_comp1,collapse=", "),
#                                      "in region",paste(chosen_region_comp1,collapse=", "),
#                                      "and brand",paste(chosen_brand_comp1,collapse=", ")),style="color:red;")))
#         }
#         if(nrow(DATA$EXPORT_TABLE2)==0){
#           fluidPage(column(12,align="center",
#                            h4 (paste("Set 2 : No breakfast of type",paste(chosen_type_comp2,collapse=", "),
#                                      "in region",paste(chosen_region_comp2,collapse=", "),
#                                      "and brand",paste(chosen_brand_comp2,collapse=", ")),style="color:red;")))
#         }
#       })
#       
#     }
#     
#     }else{
#       
#       output$erreur_comp<-renderUI({
#         fluidPage(column(12,align="center",
#                          h4 (paste("Please fill all the settings")))
#         )
#       }) 
#       
#     }
#   
#   })

# Visionnage des tableaux avec recommandations ###

# observeEvent(input$see_reco1,{
#   
#   showModal(modalDialog(
#     title=fluidRow(column(11,p(style="display:inline;",
#                                paste("Recommendations summmary"))), # titre de ton modal
#                    column(1,p(style="display:inline;",
#                               actionButton("close_graph",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
#     size="l",
#     fluidPage(style="margin-bottom:30px;",
#               column(12,align="center",
#                      img(src="demeter_reco.png",style="width:90%;margin:20px;"),
#                      h5("Added sugars, Fats and Carbohydrates can be expressed with fixed breakfast Energy (ie 400 kcal for adults and 300kcal for children)")
#               )),
#     footer=NULL,easyClose = FALSE
#   ))
#   
#   # table_reco(output,recommendations,page=1)
#   
# },ignoreInit = TRUE,ignoreNULL = TRUE)
# 
# # see reco 2 : same fucking thing ...
# observeEvent(input$see_reco2,{
#   
#   showModal(modalDialog(
#     title=fluidRow(column(11,p(style="display:inline;",
#                                paste("Recommendations summmary"))), # titre de ton modal
#                    column(1,p(style="display:inline;",
#                               actionButton("close_graph",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
#     size="l",
#     fluidPage(style="margin-bottom:30px;",
#               column(12,align="center",
#                      img(src="demeter_reco.png",style="width:90%;margin:20px;"),
#                      h5("Added sugars, Fats and Carbohydrates can be expressed with fixed breakfast Energy (ie 400 kcal for adults and 300kcal for children)")
#               )),
#     footer=NULL,easyClose = FALSE
#   ))
#   
#   #reco_table(output,recommendations,page=2)
#   
# },ignoreInit = TRUE,ignoreNULL = TRUE)


###D CLIC SUR BARRE DES GRAPHIQUES DE PREVALENCE ####

selected_chart1 <- reactive({
  if( is.null(input$dejPlot13_selected)){
    NULL
  } else input$dejPlot13_selected
})

selected_chart2 <- reactive({
  if( is.null(input$dejPlot14_selected)){
    NULL
  } else input$dejPlot14_selected
})

selected_chart3 <- reactive({
  if( is.null(input$dejPlot15_selected)){
    NULL
  } else input$dejPlot15_selected
})

selected_chart1_comp1 <- reactive({
  if( is.null(input$dejPlot13_comp1_selected)){
    NULL
  } else input$dejPlot13_comp1_selected
})

selected_chart2_comp1 <- reactive({
  if( is.null(input$dejPlot14_comp1_selected)){
    NULL
  } else input$dejPlot14_comp1_selected
})

selected_chart3_comp1 <- reactive({
  if( is.null(input$dejPlot15_comp1_selected)){
    NULL
  } else input$dejPlot15_comp1_selected
})

selected_chart1_comp2 <- reactive({
  if( is.null(input$dejPlot13_comp2_selected)){
    NULL
  } else input$dejPlot13_comp2_selected
})

selected_chart2_comp2 <- reactive({
  if( is.null(input$dejPlot14_comp2_selected)){
    NULL
  } else input$dejPlot14_comp2_selected
})

selected_chart3_comp2 <- reactive({
  if( is.null(input$dejPlot15_comp2_selected)){
    NULL
  } else input$dejPlot15_comp2_selected
})



# Clic sur une des barres #MACRO
observeEvent(selected_chart1(),{
  if (!(is.null(selected_chart1()))){
    
    render_selection(output,DATA,selected_chart1(),set=1)
    
  }
})

#Clic sur une des barres #VIT
observeEvent(selected_chart2(),{
  if (!(is.null(selected_chart2()))){
    
    render_selection(output,DATA,selected_chart2(),set=1)
  }
})


# Clic sur une des barres #MIN
observeEvent(selected_chart3(),{
  
  if (!(is.null(selected_chart3()))){
    render_selection(output,DATA,selected_chart3(),set=1)
  }
  
})

# Clic sur une des barres #MACRO (Set 2)
observeEvent(selected_chart1_comp1(),{
  
  if (!(is.null(selected_chart1_comp1()))){
    render_selection(output,DATA,selected_chart1_comp1(),set=2)
  }
  
  
})

# Clic sur une des barres #VIT (Set 2)
observeEvent(selected_chart2_comp1(),{
  
  if (!(is.null(selected_chart2_comp1()))){
    render_selection(output,DATA,selected_chart2_comp1(),set=2)
  }
  
})


# Clic sur une des barres #MIN (set 2)
observeEvent(selected_chart3_comp1(),{
  
  if (!(is.null(selected_chart3_comp1()))){
    render_selection(output,DATA,selected_chart3_comp1(),set=2)
  }
})

# Clic sur une des barres #MACRO (Set 3)
observeEvent(selected_chart1_comp2(),{
  
  if (!(is.null(selected_chart1_comp2()))){
    render_selection(output,DATA,selected_chart1_comp2(),set=3)
  }
  
  
})

# Clic sur une des barres #VIT (Set 3)
observeEvent(selected_chart2_comp2(),{

  if (!(is.null(selected_chart2_comp2()))){
    render_selection(output,DATA,selected_chart2_comp2(),set=3)
  }
  
})


# Clic sur une des barres #MIN (set 3)
observeEvent(selected_chart3_comp2(),{

  if (!(is.null(selected_chart3_comp2()))){
    render_selection(output,DATA,selected_chart3_comp2(),set=3)
  }
})


observeEvent(input$close_graph,{
  output$graph_selected_chart1 <- renderGirafe({NULL})
  output$graph_selected_chart2 <- renderGirafe({NULL})
  output$graph_selected_chart3 <- renderGirafe({NULL})
  output$graph_selected_chart1_comp1 <- renderGirafe({NULL})
  output$graph_selected_chart2_comp1 <- renderGirafe({NULL})
  output$graph_selected_chart3_comp1 <- renderGirafe({NULL})
  output$graph_selected_chart1_comp2 <- renderGirafe({NULL})
  output$graph_selected_chart2_comp2 <- renderGirafe({NULL})
  output$graph_selected_chart3_comp2 <- renderGirafe({NULL})
#  output$reco_table1<-renderDataTable({NULL}) 
#  output$reco_table2<-renderDataTable({NULL})
  removeModal(session = getDefaultReactiveDomain())
})

}
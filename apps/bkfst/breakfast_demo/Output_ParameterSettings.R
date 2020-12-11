


ParameterSettings<-function(input,output,session,DATA){

  
####### 1 Importation des recommandations ###################
  
  observeEvent(input$file_reco, {
    
    sep_reco <- reactive({
      if( is.null(input$sep_reco)){
        ';'
      } else input$sep_reco
    })
    
    dec_reco <- reactive({
      if( is.null(input$dec_reco)){
        ','
      } else input$dec_reco
    })
    
    header_reco <- reactive({
      if( is.null(input$header_reco)){
        TRUE
      } else input$header_reco
    }) 
    
    
    DATA$file_reco=NULL
    inFile<-input$file_reco

    #Détecte extension du fichier -- si fichier excel
    if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
      
      #Feuille sélectionnée
      nsheet_reco <- reactive({
        if( is.null(input$selectedsheet_reco)){
          1
        } else match(input$selectedsheet_reco,user_sheets_reco)
      })
      user_sheets_reco=readxl::excel_sheets(inFile$datapath)
      
      #Demande feuille et nom des colonnes défini ou non + vérif fichier
      output$file_reco_info <- renderUI({tagList(
        column(12, style="margin-left:-13px;",selectInput('selectedsheet_reco','Excel sheet', 
                                                          choices=user_sheets_reco, selected=user_sheets_reco[1]))#,
        # column(12,style="margin-left:-13px;",checkboxInput('header', 'Noms de colonnes', TRUE),style='margin-top:-10px;'),
        #  column(12,style="margin-left:-13px;",h5('Merci de vérifier que votre fichier est correctement importé')),
        #  actionButton(class='btn btn-success',style="margin-left:80px;margin-top:10px",'ok_fichier',label='Fichier vérifié')
      )
      })
      
      #file_reco détecté par son chemin
      file_reco<- rio::import(inFile$datapath,which=nsheet_reco())  
    }
    
    #Si fichier texte ou CSV
    if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
      
      output$file_reco_info <- renderUI({
        fluidRow(
          #  checkboxInput('header', 'Noms de colonnes', TRUE),
          radioButtons('sep_reco', 'Delimiter',c('Comma'=',','Semi-colon' =';',Tabulation ='\t'),selected=';'),
          radioButtons('dec_reco', 'Decimal',c("Comma"=',',"Dot" ='.'),selected=',')#,
          #  column(12,style="margin-left:-13px;",h5('Merci de vérifier que votre fichier est correctement importé')),
          #" attention pour les fichiers csv mettre point comme  sep de décimales et non virgule, blablabla ...."
          #  actionButton(class='btn btn-success',style="margin-left:80px;margin-top:10px",'ok_fichier',label='Fichier vérifié')
          #  radioButtons('quote', 'Guillemets',c(Aucuns ='','Doubles'='"','Simples'="'"))
        )
      })
      
      file_reco<- read.csv(inFile$datapath,sep=sep_reco(),dec=dec_reco(),header=header_reco(),quote="")
    }
  
    # Remplace NA par 0
    file_reco[is.na(file_reco)]<-0
    DATA$file_reco <- file_reco
    
    # Mise à jour "directement" lors de la modif de l'un des paramètres
    observeEvent(c(input$selectedsheet_reco,input$sep_reco,input$dec_reco,input$header_reco),{
      inFile<-input$file_reco
      if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
        #file 2 détecté par son chemin
        file_reco<- rio::import(inFile$datapath,which=nsheet_reco()) 
      }
      
      if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
        file_reco<- read.csv(inFile$datapath,sep=sep_reco(),dec=dec_reco(),header=header_reco(),quote="")
      }
      # Remplace NA par 0
      file_reco[is.na(file_reco)]<-0
      DATA$file_reco <- file_reco
    })
    
  }) 
  

#### 2 Importation du fichier céréales  #####################
  
  output$file1_input<-renderUI({
  fileInput('file1', 'Choose a file',
            accept=c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv',
              'application/vnd.ms-excel',
              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
              '.xlsx',
              '.xls'))
  }) 
  
  
  observeEvent(input$file1, {
    
    DATA$file1=NULL
    inFile<-input$file1
    
    sep <- reactive({
      if( is.null(input$sep)){
        ';'
      } else input$sep
    })
    
    dec <- reactive({
      if( is.null(input$dec)){
        ','
      } else input$dec
    })
    
    header <- reactive({
      if( is.null(input$header)){
        TRUE
      } else input$header
    })
    
    #Détecte extension du fichier -- si fichier excel
    if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
     
       user_sheets=readxl::excel_sheets(inFile$datapath)
       
      #Demande feuille et nom des colonnes défini ou non + vérif fichier
      #Feuille sélectionnée
      nsheet <- reactive({
        if( is.null(input$selectedsheet)){
          1
        } else match(input$selectedsheet,user_sheets)
      })
  
      output$file1_info <- renderUI({tagList(
        column(12, style="margin-left:-13px;",selectInput('selectedsheet','Excel sheet', choices=user_sheets, selected=user_sheets[1]))#,
        # column(12,style="margin-left:-13px;",checkboxInput('header', 'Noms de colonnes', TRUE),style='margin-top:-10px;'),
        #  column(12,style="margin-left:-13px;",h5('Merci de vérifier que votre fichier est correctement importé')),
        #  actionButton(class='btn btn-success',style="margin-left:80px;margin-top:10px",'ok_fichier',label='Fichier vérifié')
      )
      })
      
      #file 1 détectée par son chemin
      file1<- rio::import(inFile$datapath,which=nsheet()) 
    }
    
    #Si fichier texte ou CSV
    if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
      
      output$file1_info <- renderUI({
        fluidRow(
          #  checkboxInput('header', 'Noms de colonnes', TRUE),
          radioButtons('sep', 'Delimiter',c('Comma'=',','Semi-colon' =';',Tabulation ='\t'),selected=';'),
          radioButtons('dec', 'Decimal',c("Comma"=',',"Dot" ='.'),selected=',')#,
          #  column(12,style="margin-left:-13px;",h5('Merci de vérifier que votre fichier est correctement importé')),
          #" attention pour les fichiers csv mettre point comme  sep de décimales et non virgule, blablabla ...."
          #  actionButton(class='btn btn-success',style="margin-left:80px;margin-top:10px",'ok_fichier',label='Fichier vérifié')
          #  radioButtons('quote', 'Guillemets',c(Aucuns ='','Doubles'='"','Simples'="'"))
        )
      })
      
      file1<- read.csv(inFile$datapath,sep=sep(),dec=dec(),header=header(),quote="")
    }
   
    
    # Remplace NA par 0
    file1[is.na(file1)]<-0
    DATA$file1 <- file1
    
    
  # # Mise à jour "directement" lors de la modif de l'un des paramètres
  observeEvent(c(input$selectedsheet,input$sep,input$dec,input$header),{

      inFile<-input$file1
      if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
        
        user_sheets=readxl::excel_sheets(inFile$datapath)
        
        #Demande feuille et nom des colonnes défini ou non + vérif fichier
        #Feuille sélectionnée
        nsheet <- reactive({
          if( is.null(input$selectedsheet)){
            1
          } else match(input$selectedsheet,user_sheets)
        })
        
        #file 2 détecté par son chemin
        file1<- rio::import(inFile$datapath,which=nsheet())
      }
      if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
        file1<- read.csv(inFile$datapath,sep=sep(),dec=dec(),header=header(),quote="")
      }
      # Remplace NA par 0
      file1[is.na(file1)]<-0
      DATA$file1 <- file1

    })
    
  })
  
  observeEvent(input$reset_file1,{
    DATA$file1=NULL
    DATA$erreurs_file$file1=NULL
    output$file1_input<-renderUI({
      fileInput('file1', 'Choose a file',
                accept=c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  'application/vnd.ms-excel',
                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                  '.xlsx',
                  '.xls'))
    })
    output$file1_info<-renderUI({NULL})
  })

  #3. Imporation du fichier Composantes #### 
  
  output$file2_input<-renderUI({
    fileInput('file2', 'Choose a file',
              accept=c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv',
                'application/vnd.ms-excel',
                'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                '.xlsx',
                '.xls'))
  })


  observeEvent(input$file2, {

    DATA$file2=NULL
    inFile<-input$file2
    
    sep2 <- reactive({
      if( is.null(input$sep2)){
        ';'
      } else input$sep2
    })
    
    dec2 <- reactive({
      if( is.null(input$dec2)){
        ','
      } else input$dec2
    })
    
    header2 <- reactive({
      if( is.null(input$header2)){
        TRUE
      } else input$header2
    })
    
    #Détecte extension du fichier -- si fichier excel
    if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
      
      #Feuille sélectionnée
      nsheet2 <- reactive({
        if( is.null(input$selectedsheet2)){
          1
        } else match(input$selectedsheet2,user_sheets2)
      })
      user_sheets2=readxl::excel_sheets(inFile$datapath)
      
      #Demande feuille et nom des colonnes défini ou non + vérif fichier
      output$file2_info <- renderUI({tagList(
        column(12, style="margin-left:-13px;",selectInput('selectedsheet2','Excel sheet', choices=user_sheets2, selected=user_sheets2[1]))#,
      )     })
      
      #file 2 détecté par son chemin
      file2<- rio::import(inFile$datapath,which=nsheet2())
    }
    
    #Si fichier texte ou CSV
    if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
      
      output$file2_info <- renderUI({
        fluidRow(
          #  checkboxInput('header', 'Noms de colonnes', TRUE),
          radioButtons('sep2', 'Delimiter',c('Comma'=',','Semi-colon' =';',Tabulation ='\t'),selected=';'),
          radioButtons('dec2', 'Decimal',c("Comma"=',',"Dot" ='.'),selected=',')
        )
      })
      
      file2<- read.csv(inFile$datapath,sep=sep2(),dec=dec2(),header=header2(),quote="")
      
    }
    # Remplace NA par 0
    file2[is.na(file2)]<-0
    DATA$file2 <- file2
    
    # Mise à jour "directement" lors de la modif de l'un des paramètres
    observeEvent(c(input$selectedsheet2,input$sep2,input$dec2,input$header2),{
      
      inFile<-input$file2
      if(stri_sub(inFile$name,-4)=="xlsx"|stri_sub(inFile$name,-3)=="xls"){
        user_sheets2=readxl::excel_sheets(inFile$datapath)
        
        #Demande feuille et nom des colonnes défini ou non + vérif fichier
        #Feuille sélectionnée
        nsheet2 <- reactive({
          if( is.null(input$selectedsheet2)){
            1
          } else match(input$selectedsheet2,user_sheets2)
        })
        #file 2 détecté par son chemin
        file2<- rio::import(inFile$datapath,which=nsheet2()) 
      }
      if (stri_sub(inFile$name,-3)=="csv"|stri_sub(inFile$name,-3)=="txt"){
        file2<- read.csv(inFile$datapath,sep=sep2(),dec=dec2(),header=header2(),quote="")
      }
      # Remplace NA par 0
      file2[is.na(file2)]<-0
      DATA$file2 <- file2
    })
    
  })
  
  observeEvent(input$reset_file2,{
    DATA$file2<-NULL
    DATA$erreurs_file$file2=NULL
    output$file2_input<-renderUI({
      fileInput('file2', 'Choose a file',
                accept=c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  'application/vnd.ms-excel',
                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                  '.xlsx',
                  '.xls'))
    })
    output$file2_info <- renderUI({NULL})
  })
  
  ## Validation des bases de données ####
  
  observeEvent(input$valid_db,{

    # 1 Fichier recommandation ####

    list_filereco=c("Nutrient","Type","Bkf_reco","Bkf_reco_unit","Daily_reco","Daily_reco_unit","Bkf_reco_pct")

    #Si tous les noms de colonnes de la liste se retrouvent dans le fichier recommandations
    if (!(is.null(DATA$file_reco))){
    if (FALSE%in%(list_filereco%in%colnames(DATA$file_reco))){
      output$erreur_reco<-renderUI({h4("Wrong colnames in recommendations file. Please check your file is correctly imported or use template")})
      DATA$erreurs_file$file_reco="ERROR"
    }else{
      output$erreur_reco<-renderUI({NULL})
      DATA$erreurs_file$file_reco="OK"
    }}else{
      output$erreur_reco<-renderUI({h4("Please import file for recommendations")})
      DATA$erreurs_file$file_reco=NULL  
    }
  

    # 2 Fichier céréales #####
    list_filenut=c("CP_ID","volume","Product_type","Product_name","Region","Serving_size", "Energy.kcal", "Proteins.g", #Sugars.g,
                   "Carbohydrates.g","Add_sugars.g","Fat.g","Saturated_fat.g","Fibre.g","Sodium.mg","Vitamin_D.mcg",
                   "Vitamin_C.mg", "Vitamin_B1.mg","Vitamin_B2.mg","Vitamin_B3.mg","Vitamin_B6.mg","Vitamin_B9.mcg",
                   "Vitamin_B12.mcg","Vitamin_A.mg","Calcium.mg","Iron.mg","Zinc.mg","Magnesium.mg")

    #Si tous les noms de colonnes de la liste se retrouvent dans le fichier céréales
    if (!(is.null(DATA$file1))){
    if (FALSE%in%(list_filenut%in%colnames(DATA$file1))){
      output$erreur_cereals<-renderUI({h4("Wrong colnames in cereals file. Please check your file is correctly imported or use template")})
      output$Panel2<-renderUI({NULL})
      DATA$erreurs_file$file1="ERROR"
    }else{
      output$erreur_cereals<-renderUI({NULL})
      DATA$erreurs_file$file1="OK"
    }}else{
      output$erreur_cereals<-renderUI({NULL})
      DATA$erreurs_file$file1=NULL 
    }

    #3 Fichier Composantes ####

    list_filecompo=c("Product_name","Energy.kcal", "Proteins.g","Carbohydrates.g","Add_sugars.g","Fat.g",#Sugars.g,
                     "Saturated_fat.g","Fibre.g","Sodium.mg","Vitamin_D.mcg", "Vitamin_C.mg", "Vitamin_B1.mg","Vitamin_B2.mg",
                     "Vitamin_B3.mg","Vitamin_B6.mg","Vitamin_B9.mcg","Vitamin_B12.mcg","Vitamin_A.mg",
                     "Calcium.mg","Iron.mg","Zinc.mg","Magnesium.mg")

    if(!(is.null(DATA$file2))){
    if (FALSE%in%(list_filecompo%in%colnames(DATA$file2))){
      
      output$erreur_compo<-renderUI({h4(paste0("Wrong colnames in Component file. Please check your file is correctly imported or use template"))})
      DATA$erreurs_file$file2="ERROR"
      
      }else{ # si noms des lignes OK
        DATA$file2=DATA$file2%>%select(list_filecompo)
        DATA$file2$Product_name=iconv(as.character(DATA$file2$Product_name),from="LATIN1",to="UTF8") #évite conflits avec accents
        DATA$file2$Product_name=gsub("'","",DATA$file2$Product_name) #évite conflits avec noms
        output$erreur_compo<-renderUI({NULL})
        DATA$erreurs_file$file2="OK"
      }}else{
        output$erreur_compo<-renderUI({NULL})
        DATA$erreurs_file$file2=NULL  
      }


    #Si tous les fichiers sont ok --> on vérife que les contenus des colonnes sont bien numériques
    #4 Validation des 3 fichiers ####

    if (!(is.null(DATA$file_reco)) &!("ERROR"%in%DATA$erreurs_file)){

      #Vérification des colonnes numériques

      col_num1=list_filereco[c(3,5,7)]#colonne des recommandations qui doivent être sous format numériques
      output$erreur_reco<-renderUI({erreur_col(DATA$file_reco,col_num1,type="reco")})
      tryCatch({erreur_col(DATA$file_reco,col_num1,type="reco")}) #vérifient si colonnes sont numériques

    if (!(is.null(DATA$file1))){
      col_num2=list_filenut[c(2,6:27)]#colonne des céréales qui doivent être sous format numériques
      output$erreur_cereals<-renderUI({erreur_col(DATA$file1,col_num2,type="cereals")})
      tryCatch({erreur_col(DATA$file1,col_num2,type="cereals")}) #vérifient si colonnes sont numériques
    }

    if (!(is.null(DATA$file2))){
     col_num3=list_filecompo[2:22]#colonne des composantes qui doivent être sous format numériques
     output$erreur_compo<-renderUI({erreur_col(DATA$file2,col_num3,type="compo")})
     tryCatch({erreur_col(DATA$file2,col_num3,type="compo")}) #vérifient si colonnes sont numériques
    }

   #Validation des fichiers -> noms définitifs
      
    file_reco_name=stringr::str_trunc(input$file_reco$name,30)
    DATA$recommendations<-DATA$file_reco%>%mutate(file_name=file_reco_name)
    
    if(!(is.null(DATA$file1))){
    file1_name=stringr::str_trunc(input$file1$name,30)
    DATA$compo_cereals<-DATA$file1%>%mutate(file_name=file1_name,Serving_size_input=Serving_size)
    }else{
    file1_name="No file"
    DATA$compo_cereals<-NULL
    }
    
    if(!(is.null(DATA$file2))){ 
    file2_name= stringr::str_trunc(input$file2$name,30)
    DATA$compo_imported<-DATA$file2%>%mutate(file_name=file2_name)
    }else{
    file2_name="No file"
    DATA$compo_imported<-NULL 
    }
 
    # Passage à la page suivante ####
    
    output$resume_file <-renderUI({
      HTML(paste0("<div class='bordered col-sm-4 col-sm-offset-4'>
                  <p><b>Recommendation file</b> : ",file_reco_name,"<br>
                  <b>Cereals file</b> : ",file1_name,"<br>
                  <b>Components file</b> : ",file2_name,"</p>
                  </div>"))
    }) 
   
    #Passage à l'autre onglet
    updateTabsetPanel(session,"cpw_app",selected="setting")
    sendSweetAlert(session = session,title = "Validated databases",type="success")
    
    }
  })
  
 # Constitution du petit-déjeuner ####
   
  #### Choix des céréales ####
  
  observeEvent(input$choice_cereals,{
    
    if(input$choice_cereals=="none"){
      
      output$UI_cereals<-renderUI({NULL})
    }
    
    if(input$choice_cereals=="yes"){
      if(!(is.null(DATA$compo_cereals))){
          
      output$UI_cereals<-renderUI({
        fluidPage(style="margin-left:5px;",
        awesomeRadio(inputId = "choice_cereals_portion",
                     label =NULL,
                     choices = c("Keep serving sizes"="keep","Change serving size"="change"),
                     status = "info"),
        uiOutput("serving_size_cereals")
      )
      })
      
      }else{
       output$UI_cereals<-renderUI({h5("File not found. Please ensure cereals file is imported correctly",style="color:red;")})
       }}
})
  
  observeEvent(input$choice_cereals_portion,{

    if(input$choice_cereals_portion=="change"){
      output$serving_size_cereals<-renderUI({numericInput("cereals_size",label="Serving size (g)",value=0)}) 
    }else{
      output$serving_size_cereals<-renderUI({NULL}) 
    }
  },ignoreInit = TRUE,ignoreNULL = TRUE)
    
   
  #### Choix de la composante 1  ####
  
  observeEvent(input$choice_compo1,{
    
  if(input$choice_compo1=="none"){
      
    output$UI_compo1<-renderUI({NULL})
  }

  if(input$choice_compo1=="inca2"){
    
    output$UI_compo1<-renderUI({
      withProgress(message = 'Please wait',
                   detail = 'Loading food database',{
      tagList(
      selectInput("compo1_lab","Component",
                  choices=split(INCA2$Product_name,INCA2$FDXL1_N)),
      numericInput("compo1_size","Serving size (g)",value=0)
      )
      })
    })
  }
    
   if (input$choice_compo1=="import"){
     if(!(is.null(DATA$compo_imported))){
       
       output$UI_compo1<-renderUI({
         tagList(
           selectInput("compo1_lab","Component",choices=DATA$compo_imported$Product_name),
           numericInput("compo1_size","Serving size (g)",value=0)
         )
       })
     }else{
       output$UI_compo1<-renderUI({h5("File not found. Please ensure components file is imported correctly",style="color:red;")})
     }
   }
     
})

  
  
  #### Choix de la composante 2  ####
  
  observeEvent(input$choice_compo2,{
    
    if(input$choice_compo2=="none"){
      
      output$UI_compo2<-renderUI({NULL})
    }
    
    if(input$choice_compo2=="inca2"){
      
      output$UI_compo2<-renderUI({
        withProgress(message = 'Please wait',
                     detail = 'Loading food database',{
                       tagList(
                         selectInput("compo2_lab","Component",
                                     choices=split(INCA2$Product_name,INCA2$FDXL1_N)),
                         numericInput("compo2_size","Serving size (g)",value=0)
                       )
                     })
      })
    }
    
    if (input$choice_compo2=="import"){
      if(!(is.null(DATA$compo_imported))){
        
        output$UI_compo2<-renderUI({
          tagList(
            selectInput("compo2_lab","Component",choices=DATA$compo_imported$Product_name),
            numericInput("compo2_size","Serving size (g)",value=0)
          )
        })
      }else{
        output$UI_compo2<-renderUI({h5("File not found. Please ensure components file is imported correctly",style="color:red;")})
      }
    }
    
  })
  
  
  #### Choix de la composante 3  ####
  
  observeEvent(input$choice_compo3,{
    
    if(input$choice_compo3=="none"){
      
      output$UI_compo3<-renderUI({NULL})
    }
    
    if(input$choice_compo3=="inca2"){
      
      output$UI_compo3<-renderUI({
        withProgress(message = 'Please wait',
                     detail = 'Loading food database',{
                       tagList(
                         selectInput("compo3_lab","Component",
                                     choices=split(INCA2$Product_name,INCA2$FDXL1_N)),
                         numericInput("compo3_size","Serving size (g)",value=0)
                       )
                     })
      })
    }
    
    if (input$choice_compo3=="import"){
      if(!(is.null(DATA$compo_imported))){
        
        output$UI_compo3<-renderUI({
          tagList(
            selectInput("compo3_lab","Component",choices=DATA$compo_imported$Product_name),
            numericInput("compo3_size","Serving size (g)",value=0)
          )
        })
      }else{
        output$UI_compo3<-renderUI({h5("File not found. Please ensure components file is imported correctly",style="color:red;")})
      }
    }
    
  })
  
  
#### Choix de la composante 4  ####
  
  observeEvent(input$choice_compo4,{
    
    if(input$choice_compo4=="none"){
      
      output$UI_compo4<-renderUI({NULL})
    }
    
    if(input$choice_compo4=="inca2"){
      
      output$UI_compo4<-renderUI({
        withProgress(message = 'Please wait',
                     detail = 'Loading food database',{
                       tagList(
                         selectInput("compo4_lab","Component",
                                     choices=split(INCA2$Product_name,INCA2$FDXL1_N)),
                         numericInput("compo4_size","Serving size (g)",value=0)
                       )
                     })
      })
    }
    
    if (input$choice_compo4=="import"){
      if(!(is.null(DATA$compo_imported))){
        
        output$UI_compo4<-renderUI({
          tagList(
            selectInput("compo4_lab","Component",choices=DATA$compo_imported$Product_name),
            numericInput("compo4_size","Serving size (g)",value=0)
          )
        })
      }else{
        output$UI_compo4<-renderUI({h5("File not found. Please ensure components file is imported correctly",style="color:red;")})
      }
    }
  })
  
  #### Choix de la composante 5  ####
  
  observeEvent(input$choice_compo5,{
    
    if(input$choice_compo5=="none"){
      
      output$UI_compo5<-renderUI({NULL})
    }
    
    if(input$choice_compo5=="inca2"){
      
      output$UI_compo5<-renderUI({
        withProgress(message = 'Please wait',
                     detail = 'Loading food database',{
                       tagList(
                         selectInput("compo5_lab","Component",
                                     choices=split(INCA2$Product_name,INCA2$FDXL1_N)),
                         numericInput("compo5_size","Serving size (g)",value=0)
                       )
                     })
      })
    }
    
    if (input$choice_compo5=="import"){
      if(!(is.null(DATA$compo_imported))){
        
        output$UI_compo5<-renderUI({
          tagList(
            selectInput("compo5_lab","Component",choices=DATA$compo_imported$Product_name),
            numericInput("compo5_size","Serving size (g)",value=0)
          )
        })
      }else{
        output$UI_compo5<-renderUI({h5("File not found. Please ensure components file is imported correctly",style="color:red;")})
      }
    }
    
  })
  

}
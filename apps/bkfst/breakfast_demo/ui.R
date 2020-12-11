
#Fonction pour enlever le height=500px des graphs ggirafe
#jsCode <- "shinyjs.style500px_remove_RG =function(){
#$document.getElementById('dejPlot1').setAttribute('style','width:100%');}"

#$("div").find("[style='width:100%;height:500px;']").attr("style","width:100%;");
#document.getElementById("dejPlot1").setAttribute('style','width:100%')

fluidPage(shinyjs::useShinyjs(),
          style="padding-right:0px; padding-left:0px;",
      #Fonctions JavaScript
         # extendShinyjs(text=jsCode), #pour appeller les fonction définies dans jsCode
          #Fonction pour remettre un input à null  #NE MARCHE PAS :PQ ?
          # tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName) {
          #             Shiny.onInputChange(variableName, null);
          #             });
          #             "),  
          ## Affichage du bandeau MS-NUTRITION ##
          div(id="bandeau-tx",class="navbar", style="margin-bottom:0px;",
              #Styles de la bannière (fond, marges, taille, bords, ....)
              fluidRow(style="width:100% ;margin-right:0px;margin-left:0px;
                       border-bottom: medium solid rgb(2,128, 170);
                       border-color: #72bba2;
                       width: 100%;
                       background-image:url(fondbis.png);
                       background-color: #f5f5f5;
                       ",
                       # Elements de la bannières :
                       div(
                         #. Titre
                         column(12,div(style=" height: 65px;
                                      line-height: 65px;
                                      text-align: center;",class="titre",tags$span(
                                        style=" display: inline-block;
                                        vertical-align: middle;
                                        line-height: normal;",
                                        h2(style="margin-top: 0px;padding-right:80px;color:#72bba2","Breakfast application"))))

                                )
                                )),
          
#Titre = lien vers ce qu'on veut
 navbarPage(title=NULL, id="cpw_app",collapsible=TRUE, inverse=FALSE,
                     theme = "theme_cpw.css",windowTitle = 'Breakfast app',
                     
################################################################ Page 1 : Importation des données ######################################################################

                  
 tabPanel(title=h4("Import databases"),value="data",
    fluidPage(   

      #Pannel 1 : Téléchargement du ficher
      #Template
      tagList(
        div(class="panel panel-import", style="margin:20px;",
            #En-tête du panel
            div(class="panel-heading",
                fluidRow(column(12,h3("Databases")))#,
            ),
        fluidRow(class="formulaire",
        #    column(12,align="center",
                     # Composition du petit déjeuner : 
                    #   column(12,uiOutput("breakfast_description"),style="margin-bottom:20px;"),
                  # h3("Enter your breakfast components (please use templates)")),
    column(12,
     #Recommandation     
     column(4, 
            h3("1. Import recommendations"),
            downloadButton(outputId="template_reco", label="Download template for recommendations"),
            fileInput('file_reco', 'Choose a file',
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
                        '.xls')),
            #Questions sur fichier
            uiOutput('file_reco_info')
     ),
      #Cereals      
       column(4, 
       h3("2. Import Cereals (optional) "),
       downloadButton(outputId="template_cereals", label="Download template for cereals"),
              uiOutput("file1_input"),
              #Questions sur fichier
              actionButton("reset_file1","Reset cereals file",class = "btn-danger",style="margin-top:-20px;"),
              uiOutput('file1_info')
       ),
     #Components     
     column(4, 
            h3("3. Import Components (optional)"),
            downloadButton(outputId="template_component", label="Download template for components"),
              uiOutput("file2_input"),
              #Questions sur fichier
              actionButton("reset_file2","Reset component file",class = "btn-danger",style="margin-top:-20px;"),
              uiOutput('file2_info')
     )),
    
    column(12,align="center",
           div(style="margin-top:20px;",actionBttn(inputId = "valid_db",label = "Submit all",style = "simple",
                                                   color = "success",icon = icon("check-square "))),
           uiOutput("erreur_reco",style="color:red;margin:15px;"),
           uiOutput("erreur_cereals",style="color:red;margin:15px;"),
           uiOutput("erreur_compo",style="color:red;margin:15px;")
           )
  )))
)),

###################################################### Page 2 : Breakfast Setting ############################################################

tabPanel(title=h4("Breakfasts Setting"),value="setting",
         fluidPage(   
           tagList(
             div(class="panel panel-import", style="margin:20px;",
                 #En-tête du panel
                 div(class="panel-heading",
                     fluidRow(column(12,h3("Customize your breakfast ")))#,
                 ),
                 fluidRow(class="formulaire",
                    
 column(12,align="center",style="margin-bottom:10px;",
        uiOutput("resume_file"),
        textInput(inputId="portfolio_name",HTML("<h4> Breakfast name (memorable) </h4>"))),                         

  column(12,
      column(4,
          h4("Component 0 (Cereals)"),
           awesomeRadio(
             inputId = "choice_cereals",
              label =NULL,
              choices = c("No"="none","Yes"="yes"),
              selected = "none",
              status = "info"),
             uiOutput("UI_cereals")
         ),
      column(4,
             h4("Component 1 (e.g. Milk)"),
             awesomeRadio(
               inputId = "choice_compo1",
               label =NULL,
               choices = c("None"="none","Choose in french CIQUAL database"="inca2", "Choose in imported database"="import"),
               selected = "none",
               status = "warning"),
             uiOutput("UI_compo1")
             ),
     column(4,
            h4("Component 2 (e.g. Fruit)"),
            awesomeRadio(
              inputId = "choice_compo2",
              label =NULL,
              choices = c("None"="none","Choose in french CIQUAL database"="inca2", "Choose in imported database"="import"),
              selected = "none",
              status = "warning"),
            uiOutput("UI_compo2")
     )),
  column(12,
     column(4,
            h4("Component 3"),
            awesomeRadio(
              inputId = "choice_compo3",
              label =NULL,
              choices = c("None"="none","Choose in french CIQUAL database"="inca2", "Choose in imported database"="import"),
              selected = "none",
              status = "warning"),
            uiOutput("UI_compo3")
     ),
     column(4,
            h4("Component 4"),
            awesomeRadio(
              inputId = "choice_compo4",
              label =NULL,
              choices = c("None"="none","Choose in french CIQUAL database"="inca2", "Choose in imported database"="import"),
              selected = "none",
              status = "warning"),
            uiOutput("UI_compo4")
     ),
     column(4,
            h4("Component 5"),
            awesomeRadio(
              inputId = "choice_compo5",
              label =NULL,
              choices = c("None"="none","Choose in french CIQUAL database"="inca2", "Choose in imported database"="import"),
              selected = "none",
              status = "warning"),
            uiOutput("UI_compo5")
     )),
      column(12,align="center",
             div(style="margin-top:20px;",
                 actionBttn(inputId="see_breakfasts2",label="See your breakfasts",icon = icon("eye"),style = "simple",color = "primary"),
                 actionBttn(inputId = "valid_settings",label = "Add Breakfast",style = "simple",color = "primary",icon = icon("plus-square")),
                 actionBttn(inputId = "go_to_analysis",label = "Go to breakfast analysis",style = "simple",color = "success",icon = icon("arrow-circle-right"))),
              uiOutput("erreur_cereals2",style="color:red;margin:15px;"),
              uiOutput("erreur_compo1",style="color:red;margin:15px;"),
              uiOutput("erreur_compo2",style="color:red;margin:15px;"),
              uiOutput("erreur_compo3",style="color:red;margin:15px;"),
              uiOutput("erreur_compo4",style="color:red;margin:15px;"),
              uiOutput("erreur_compo5",style="color:red;margin:15px;"),
              uiOutput("erreur_name",style="color:red;margin:15px;"),
              uiOutput("erreur_portfolio",style="color:red;margin:15px;"))
   )))
  )),
             
################################################################### Page 3 : Portfolio Analysis ##################################################################
tabPanel(title=h4("Breakfast Analysis"),value="analysis",
 
# Panel 2 : définition des filtres  
uiOutput("Panel2"),    
# fluidPage(
# sliderInput("width", "width in inches:", min = 2, max = 10, value = 6),
# sliderInput("height", "height in inches:", min = 2, max = 10, value = 6)
# ),        
div(id="div_analysis",
        #Téléchargement des résultats
         column(12,align="center",
         uiOutput("download_btn"),
         uiOutput("download_btn_comp1"),uiOutput("download_btn_comp2")),

############################################################## Onglet 1 : Graphiques des moyennes en comparaison ############################
tabsetPanel(id="Tabset_analysis",
            
tabPanel("Overall breakfasts",value="onglet2",
             
             fluidPage(
               column(12,align="center",style="margin-bottom:40px;",
                      h2("Average nutritional composition"),
               awesomeRadio(inputId = "choice_graph_means2",
                            label =NULL,inline=TRUE,
                            choices = c("% of daily recommendations"="DAILYreco",
                                        "% of breakfast recommendations"="BKFreco"),
                            status = "info")),br(),br(),
               
               uiOutput("onglet_means2"))       
             
),
################################################################ Onglet 2 : Graphiques de moyennes #####################################################################

tabPanel("Breakfasts by components",value="onglet1",
         
   fluidPage(
     column(12,align="center",style="margin-bottom:40px;",
            h2("Average nutritional composition by components"),
            awesomeRadio(inputId = "choice_graph_means",
                         label =NULL,inline=TRUE,
                         choices = c("% of daily recommendations"="DAILYreco",
                                     "% of breakfast recommendations"="BKFreco"),
                         status = "info")),
     uiOutput("onglet_means_col1"),uiOutput("onglet_means_col2"),uiOutput("onglet_means_col3")
     )       

   ),


################################################################ Onglet 3 : Graphiques de prévalence #####################################################################
tabPanel("Compliance with recommendation",value="onglet3",
         
   fluidPage(
     column(12,align="center",h2("Percentage of breakfasts meeting recommendations")),
     uiOutput("onglet_compliance_col1"),uiOutput("onglet_compliance_col2"),uiOutput("onglet_compliance_col3")) 
   
)

))#end tabsetanalysis

)# end tabPanel Analysis

)# end navbar page
)
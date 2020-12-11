#: UI GRAPH AVEC COMPARAISON ####

#' @param nb_graph : nombre de graphiues à afficher : 1, 2 ou 3

graph_UI<-function(input,output,session,DATA,nb_graph){


col=ifelse(nb_graph==1,12,ifelse(nb_graph==2,6,ifelse(nb_graph==3,4,12)))
gwidth=ifelse(nb_graph==1,10,ifelse(nb_graph==2,5,ifelse(nb_graph==3,6,6)))
gheight=ifelse(nb_graph==1,5,ifelse(nb_graph==2,4,ifelse(nb_graph==3,6,6)))
divstyle=ifelse(nb_graph==1,"width:1200px;margin-bottom:100px;",
               ifelse(nb_graph==2,"width:600px;height:460px;",
                      ifelse(nb_graph==3,"height:500px;","")))
divstyle_compliance=ifelse(nb_graph==1,"width:1200px;margin-bottom:50px;",
                           ifelse(nb_graph==2,"width:600px;height:420px;",""))
margin_btn="margin-bottom:50px;"
 
# Moyennes ####
 
observeEvent(input$choice_graph_means,{

 if (input$choice_graph_means=="DAILYreco"){
   
  output$onglet_means_col1<-renderUI({  
    tagList(
      #ANCIENS GRAPHS
      column(col,
             #DAILY
             column(12,align="center",
                    h3(textOutput("titre2")),
                    div(style=divstyle,#class="cadre-graph",
                      girafeOutput("dejPlot2")),
                    column(12,h5(textOutput("sub_titre2")),
                           uiOutput("downloadPlot2",style=margin_btn))
             ),
             #DAILY
             column(12,align="center",
                      h3(textOutput("titre4")),
                      div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot4")),
                      column(12,h5(textOutput("sub_titre4")),
                             uiOutput("downloadPlot4",style=margin_btn))
                    ),
             #DAILY
             column(12,align="center",
                      h3(textOutput("titre6")),
                  div(style=divstyle,#class="cadre-graph",
                    girafeOutput("dejPlot6")) ,
                      column(12,h5(textOutput("sub_titre6")),
                             uiOutput("downloadPlot6",style=margin_btn))
                    )
      ))
  })
  
 }else{
  
   output$onglet_means_col1<-renderUI({  
     tagList(
       #ANCIENS GRAPHS
       column(col,
              #BF       
              column(12,align="center",
                     h3(textOutput("titre1")),
                     div(style=divstyle,#class="cadre-graph",
                         girafeOutput("dejPlot1")) ,
                     column(12,h5(textOutput("sub_titre1")),
                            uiOutput("downloadPlot1",style=margin_btn))
              ),
              #BF
              column(12,align="center",
                     h3(textOutput("titre3")),
                     div(style=divstyle,#class="cadre-graph",
                         girafeOutput("dejPlot3")) ,
                     column(12,h5(textOutput("sub_titre3")),
                            uiOutput("downloadPlot3",style=margin_btn))
              ),
              #BF
              column(12,align="center",
                     h3(textOutput("titre5")),
                     div(style=divstyle,#class="cadre-graph",
                         girafeOutput("dejPlot5")),
                     column(12,h5(textOutput("sub_titre5")),
                            uiOutput("downloadPlot5",style=margin_btn))
              )
       ))
   }) 
   
 }
      
if(nb_graph>=2){
  
if (input$choice_graph_means=="DAILYreco"){
  
  output$onglet_means_col2<-renderUI({  
    tagList(
      #NOUVEAUX GRAPHS
      column(col,
             #DAILY
             column(12,align="center",
                      h3(textOutput("titre2_comp1")),
                  div(style=divstyle,#class="cadre-graph",
                      girafeOutput("dejPlot2_comp1")) ,
                      column(12,h5(textOutput("sub_titre2_comp1")),
                             uiOutput("downloadPlot2_comp1",style=margin_btn))
                    ),
             #DAILY
             column(12,align="center",
                      h3(textOutput("titre4_comp1")),
                    div(style=divstyle,#class="cadre-graph",
                      girafeOutput("dejPlot4_comp1")) ,
                      column(12,h5(textOutput("sub_titre4_comp1")),
                             uiOutput("downloadPlot4_comp1",style=margin_btn))
                    ),
             #DAILY
             column(12,align="center",
                      h3(textOutput("titre6_comp1")),
                    div(style=divstyle,#class="cadre-graph",
                      girafeOutput("dejPlot6_comp1")) ,
                      column(12,h5(textOutput("sub_titre6_comp1")),
                             uiOutput("downloadPlot6_comp1",style=margin_btn))
                    )
      ))
  })
  
}else{
  
  output$onglet_means_col2<-renderUI({  
    tagList(
      #NOUVEAUX GRAPHS
      column(col,
             #BF
             column(12,align="center",
                    h3(textOutput("titre1_comp1")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot1_comp1")) ,
                    column(12,h5(textOutput("sub_titre1_comp1")),
                           uiOutput("downloadPlot1_comp1",style=margin_btn))
             ),
             #BF
             column(12,align="center",
                    h3(textOutput("titre3_comp1")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot3_comp1")) ,
                    column(12,h5(textOutput("sub_titre3_comp1")),
                           uiOutput("downloadPlot3_comp1",style=margin_btn))
             ),
             #BF
             column(12,align="center",
                    h3(textOutput("titre5_comp1")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot5_comp1")) ,
                    column(12,h5(textOutput("sub_titre5_comp1")),
                           uiOutput("downloadPlot5_comp1",style=margin_btn))
             )
      ))
  })
  
}
  
}else{
  
  onglet_means_col2<-renderUI({NULL})
  
}
  
  if(nb_graph>=3){

if (input$choice_graph_means=="DAILYreco"){
    
    output$onglet_means_col3<-renderUI({  
      tagList(
        #NOUVEAUX GRAPHS
        column(col,
               #DAILY
               column(12,align="center",
                        h3(textOutput("titre2_comp2")),
                      div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot2_comp2")) ,
                        column(12,h5(textOutput("sub_titre2_comp2")),
                               uiOutput("downloadPlot2_comp2",style=margin_btn))
                      ),
               #DAILY
               column(12,align="center",
                        h3(textOutput("titre4_comp2")),
                      div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot4_comp2")) ,
                        column(12,h5(textOutput("sub_titre4_comp2")),
                               uiOutput("downloadPlot4_comp2",style=margin_btn))
                      ),
               #DAILY
               column(12,align="center",
                        h3(textOutput("titre6_comp2")),
                      div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot6_comp2")) ,
                        column(12,h5(textOutput("sub_titre6_comp2")),
                               uiOutput("downloadPlot6_comp2",style=margin_btn))
                      )
        ))
    })
    
}else{
  
  output$onglet_means_col3<-renderUI({  
    tagList(
      #NOUVEAUX GRAPHS
      column(col,
             #BF
             column(12,align="center",
                    h3(textOutput("titre1_comp2")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot1_comp2")),
                    column(12,h5(textOutput("sub_titre1_comp2")),
                           uiOutput("downloadPlot1_comp2",style=margin_btn))
             ),
             #BF
             column(12,align="center",
                    h3(textOutput("titre3_comp2")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot3_comp2")) ,
                    column(12,h5(textOutput("sub_titre3_comp2")),
                           uiOutput("downloadPlot3_comp2",style=margin_btn))
             ),
             #BF
             column(12,align="center",
                    h3(textOutput("titre5_comp2")),
                    div(style=divstyle,#class="cadre-graph",
                        girafeOutput("dejPlot5_comp2")),
                    column(12,h5(textOutput("sub_titre5_comp2")),
                           uiOutput("downloadPlot5_comp2",style=margin_btn))
             )
      ))
  })
  
}
    
  }else{
    
    onglet_means_col3<-renderUI({NULL})
  
    }
})
   
# Moyennes (comparaison) ####

observeEvent(input$choice_graph_means2,{ 

if (input$choice_graph_means2=="DAILYreco"){
 
  output$onglet_means2<-renderUI({  
    tagList(
      #DAILY
      column(12,align="center",
             h3(textOutput("titre8")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot8"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre8")),
                        uiOutput("downloadPlot8",style=margin_btn))
             )),
      #DAILY
      column(12,align="center",
             h3(textOutput("titre10")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot10"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre10")),
                        uiOutput("downloadPlot10",style=margin_btn))
             )),
      #DAILY
      column(12,align="center",
             h3(textOutput("titre12")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot12"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre12")),
                        uiOutput("downloadPlot12",style=margin_btn))
             ))    
      
    )
  })
  
}else{
  
  output$onglet_means2<-renderUI({  
    tagList(
      #BF
      column(12,align="center",
             h3(textOutput("titre7")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot7"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre7")),
                        uiOutput("downloadPlot7",style=margin_btn))
             )),
      #BF
      column(12,align="center",
             h3(textOutput("titre9")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot9"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre9")),
                        uiOutput("downloadPlot9",style=margin_btn))
             )),
      
      #BF
      column(12,align="center",
             h3(textOutput("titre11")),
             div(class="cadre-graph",
                 girafeOutput("dejPlot11"),
                 column(12,style="margin-top:100px;",h5(textOutput("sub_titre11")),
                        uiOutput("downloadPlot11",style=margin_btn))
             ))  
    )
  }) 
  
}
  
}) 

  # Prévalences ####
  
  #UI OUTPUT
  
  output$onglet_compliance_col1<-renderUI({
    tagList(
      #ANCIENS GRAPHS
      column(col,
             column(12,align="center",
                      h3(textOutput("titre13")),
                    div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot13")),
                      column(12,h5(textOutput("sub_titre13")),
                             uiOutput("downloadPlot13",style=margin_btn))
                    ),
             
             column(12,align="center",
                      h3(textOutput("titre14")),
                      div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot14")) ,
                      column(12,h5(textOutput("sub_titre14")),
                             uiOutput("downloadPlot14",style=margin_btn))
                    ),
             
             column(12,align="center",
                      h3(textOutput("titre15")),
                    div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot15")), 
                      column(12,h5(textOutput("sub_titre15")),
                             uiOutput("downloadPlot15",style=margin_btn))
                    )
      ))
  })
  
  if(nb_graph>=2){ 
 
output$onglet_compliance_col2<-renderUI({
  tagList(  
  column(col,
         column(12,align="center",
                      h3(textOutput("titre13_comp1")),
                div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot13_comp1")),
                      column(12,h5(textOutput("sub_titre13_comp1")),
                             uiOutput("downloadPlot13_comp1",style=margin_btn))
                    ),
             
             column(12,align="center",
                      h3(textOutput("titre14_comp1")),
                    div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot14_comp1")) ,
                      column(12,h5(textOutput("sub_titre14_comp1")),
                             uiOutput("downloadPlot14_comp1",style=margin_btn))
                    ),
             
             column(12,align="center",
                      h3(textOutput("titre15_comp1")),
                    div(style=divstyle_compliance,#class="cadre-graph",
                      girafeOutput("dejPlot15_comp1")) ,
                      column(12,h5(textOutput("sub_titre15_comp1")),
                             uiOutput("downloadPlot15_comp1",style=margin_btn))
                    )
      ))
  })

}else{
  output$onglet_compliance_col2<-renderUI({NULL})
} 
  
  if(nb_graph>=3){ 
    
    output$onglet_compliance_col3<-renderUI({
      tagList(  
        column(col,
               column(12,align="center",
                        h3(textOutput("titre13_comp2")),
                      div(style=divstyle_compliance,#class="cadre-graph",
                        girafeOutput("dejPlot13_comp2")) ,
                        column(12,h5(textOutput("sub_titre13_comp2")),
                               uiOutput("downloadPlot13_comp2",style=margin_btn))
                      ),
               
               column(12,align="center",
                        h3(textOutput("titre14_comp2")),
                      div(style=divstyle_compliance,#class="cadre-graph",
                        girafeOutput("dejPlot14_comp2")) ,
                        column(12,h5(textOutput("sub_titre14_comp2")),
                               uiOutput("downloadPlot14_comp2",style=margin_btn))
                      ),
               
               column(12,align="center",
                        h3(textOutput("titre15_comp2")),
                      div(style=divstyle_compliance,#class="cadre-graph",
                        girafeOutput("dejPlot15_comp2")) ,
                        column(12,h5(textOutput("sub_titre15_comp2")),
                               uiOutput("downloadPlot15_comp2",style=margin_btn))
                      )
        ))
    })
    
  }else{
    output$onglet_compliance_col3<-renderUI({NULL})
  }
  
# Onglet 1 ####
  
  output$titre1<-renderText({"Macronutrients (% of breakfast recommendation)"})
  output$dejPlot1 <- renderGirafe({
    if (nrow(DATA$graph$data_mean1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean1,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
      
    }
  })
  
  output$sub_titre1<-renderText({
    if (nrow(DATA$graph$data_mean1)==0) {
      NULL
    }else{"*Energy fixed at recommended value for breakfast"} 
  })
  
  
  output$titre2<-renderText({"Macronutrients (% of daily recommendation)"})
  output$dejPlot2  <- renderGirafe({
    if (nrow(DATA$graph$data_mean_daily1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean_daily1,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
      
    }
  })
  
  output$sub_titre2<-renderText({
    if (nrow(DATA$graph$data_mean_daily1)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })
  
  output$titre3<-renderText({"Vitamins (% of breakfast recommendation)"})
  output$dejPlot3  <- renderGirafe({
    if (nrow(DATA$graph$data_mean2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean2,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$titre4<-renderText({"Vitamins (% of daily recommendation)"})
  output$dejPlot4  <- renderGirafe({
    if (nrow(DATA$graph$data_mean_daily2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean_daily2,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$sub_titre4<-renderText({
    if (nrow(DATA$graph$data_mean_daily2)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })
  
  output$titre5<-renderText({"Minerals (% of breakfast recommendation)"})
  output$dejPlot5 <- renderGirafe({
    if (nrow(DATA$graph$data_mean3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean3,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$titre6<-renderText({"Minerals (% of daily recommendation)"})
  output$dejPlot6  <- renderGirafe({
    if (nrow(DATA$graph$data_mean_daily3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$mean_daily3,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$sub_titre6<-renderText({
    if (nrow(DATA$graph$data_mean_daily3)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })

  if (nb_graph>=2){
    
  output$titre1_comp1<-renderText({"Macronutrients (% of breakfast recommendation)"})
  output$dejPlot1_comp1 <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean1,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
      
    }
  })
  
  output$sub_titre1_comp1<-renderText({
    if (nrow(DATA$graph_comp1$data_mean1)==0) {
      NULL
    }else{"*Energy fixed at recommended value for breakfast"} 
  })
  
  
  output$titre2_comp1<-renderText({"Macronutrients (% of daily recommendation)"})
  output$dejPlot2_comp1  <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean_daily1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean_daily1,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
      
    }
  })
  
  output$sub_titre2_comp1<-renderText({
    if (nrow(DATA$graph_comp1$data_mean_daily1)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })
  
  output$titre3_comp1<-renderText({"Vitamins (% of breakfast recommendation)"})
  output$dejPlot3_comp1  <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean2,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$titre4_comp1<-renderText({"Vitamins (% of daily recommendation)"})
  output$dejPlot4_comp1  <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean_daily2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean_daily2,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$sub_titre4_comp1<-renderText({
    if (nrow(DATA$graph_comp1$data_mean_daily2)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })
  
  output$titre5_comp1<-renderText({"Minerals (% of breakfast recommendation)"})
  output$dejPlot5_comp1 <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean3,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$titre6_comp1<-renderText({"Minerals (% of daily recommendation)"})
  output$dejPlot6_comp1  <- renderGirafe({
    if (nrow(DATA$graph_comp1$data_mean_daily3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph_comp1$mean_daily3,width=1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="none"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE)
      )
    }
  })
  
  output$sub_titre6_comp1<-renderText({
    if (nrow(DATA$graph_comp1$data_mean_daily3)==0) {
      NULL
    }else{"Red point shows breakfast recommendation"} 
  })
  
  }
  
  if(nb_graph>=3){
    
    output$titre1_comp2<-renderText({"Macronutrients (% of breakfast recommendation)"})
    output$dejPlot1_comp2 <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean1,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
        
      }
    })
    
    output$sub_titre1_comp2<-renderText({
      if (nrow(DATA$graph_comp2$data_mean1)==0) {
        NULL
      }else{"*Energy fixed at recommended value for breakfast"} 
    })
    
    
    output$titre2_comp2<-renderText({"Macronutrients (% of daily recommendation)"})
    output$dejPlot2_comp2  <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean_daily1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean_daily1,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
        
      }
    })
    
    output$sub_titre2_comp2<-renderText({
      if (nrow(DATA$graph_comp2$data_mean_daily1)==0) {
        NULL
      }else{"Red point shows breakfast recommendation"} 
    })
    
    output$titre3_comp2<-renderText({"Vitamins (% of breakfast recommendation)"})
    output$dejPlot3_comp2  <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean2,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
      }
    })
    
    output$titre4_comp2<-renderText({"Vitamins (% of daily recommendation)"})
    output$dejPlot4_comp2  <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean_daily2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean_daily2,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
      }
    })
    
    output$sub_titre4<-renderText({
      if (nrow(DATA$graph_comp2$data_mean_daily2)==0) {
        NULL
      }else{"Red point shows breakfast recommendation"} 
    })
    
    output$titre5_comp2<-renderText({"Minerals (% of breakfast recommendation)"})
    output$dejPlot5_comp2 <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean3,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
      }
    })
    
    output$titre6_comp2<-renderText({"Minerals (% of daily recommendation)"})
    output$dejPlot6_comp2  <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_mean_daily3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$mean_daily3,width=1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="none"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE)
        )
      }
    })
    
    output$sub_titre6_comp2<-renderText({
      if (nrow(DATA$graph_comp2$data_mean_daily3)==0) {
        NULL
      }else{"Red point shows breakfast recommendation"} 
    })
    
  }
  
# Téléchargement des tableaux de données 
  
  output$downloadPlot1<- renderUI({
    if (nrow(DATA$graph$data_mean1)==0) { 
      NULL
    }else{
      downloadButton('download_plot1','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  output$downloadPlot2<- renderUI({
    if (nrow(DATA$graph$data_mean_daily1)==0) { 
      NULL
    }else{
      downloadButton('download_plot2','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  output$downloadPlot3<- renderUI({
    if (nrow(DATA$graph$data_mean2)==0) { 
      NULL
    }else{
      downloadButton('download_plot3','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  output$downloadPlot4<- renderUI({
    if (nrow(DATA$graph$data_mean_daily2)==0) { 
      NULL
    }else{
      downloadButton('download_plot4','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  output$downloadPlot5<- renderUI({
    if (nrow(DATA$graph$data_mean3)==0) { 
      NULL
    }else{
      downloadButton('download_plot5','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  output$downloadPlot6<- renderUI({
    if (nrow(DATA$graph$data_mean_daily3)==0) { 
      NULL
    }else{
      downloadButton('download_plot6','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  #graph supplémentaires 
  
  if(nb_graph>=2){
    
    output$downloadPlot1_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean1)==0) { 
        NULL
      }else{
        downloadButton('download_plot1_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    output$downloadPlot2_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean_daily1)==0) { 
        NULL
      }else{
        downloadButton('download_plot2_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    output$downloadPlot3_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean2)==0) { 
        NULL
      }else{
        downloadButton('download_plot3_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot4_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean_daily2)==0) { 
        NULL
      }else{
        downloadButton('download_plot4_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot5_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean3)==0) { 
        NULL
      }else{
        downloadButton('download_plot5_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot6_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_mean_daily3)==0) { 
        NULL
      }else{
        downloadButton('download_plot6_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })  
    
  }
  
  if(nb_graph>=3){
    
    output$downloadPlot1_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean1)==0) { 
        NULL
      }else{
        downloadButton('download_plot1_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    output$downloadPlot2_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean_daily1)==0) { 
        NULL
      }else{
        downloadButton('download_plot2_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    output$downloadPlot3_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean2)==0) { 
        NULL
      }else{
        downloadButton('download_plot3_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot4_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean_daily2)==0) { 
        NULL
      }else{
        downloadButton('download_plot4_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot5_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean3)==0) { 
        NULL
      }else{
        downloadButton('download_plot5_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot6_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_mean_daily3)==0) { 
        NULL
      }else{
        downloadButton('download_plot6_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })  
    
  }
  
  # Création des fichiers CSV
  output$download_plot1<- downloadHandler(filename = function() { paste0("Chart_table_1_1_1.csv")},
                                                             content = function(file) {
                                                               write.csv2(DATA$graph$data_mean1, file)
                                                             })
  
  output$download_plot2 <- downloadHandler(filename = function() { paste0("Chart_table_1_2_1.csv")},
                                                             content = function(file) {
                                                               write.csv2(DATA$graph$data_mean_daily1, file)
                                                             })
  output$download_plot3<- downloadHandler(filename = function() { paste0("Chart_table_1_3_1.csv")},
                                                            content = function(file) {
                                                              write.csv2(DATA$graph$data_mean2, file)
                                                            })
  output$download_plot4 <- downloadHandler(filename = function() { paste("Chart_table_1_4_1.csv")},
                                                             content = function(file) {
                                                               write.csv2(DATA$graph$data_mean_daily2, file)
                                                             })
  output$download_plot5 <- downloadHandler(filename = function() { paste("Chart_table_1_5_1.csv")},
                                                             content = function(file) {
                                                               write.csv2(DATA$graph$data_mean3, file)
                                                             })
  output$download_plot6 <- downloadHandler(filename = function() { paste("Chart_table_1_6_1.csv")},
                                                            content = function(file) {
                                                              write.csv2(DATA$graph$data_mean_daily3, file)
                                                            })  
  
  if(nb_graph>=2){
    output$download_plot1_comp1<- downloadHandler(filename = function() { paste0("Chart_table_1_1_2.csv")},
                                            content = function(file) {
                                              write.csv2(DATA$graph_comp1$data_mean1, file)
                                            })
    
    output$download_plot2_comp1 <- downloadHandler(filename = function() { paste0("Chart_table_1_2_2.csv")},
                                             content = function(file) {
                                               write.csv2(DATA$graph_comp1$data_mean_daily1, file)
                                             })
    output$download_plot3_comp1<- downloadHandler(filename = function() { paste0("Chart_table_1_3_2.csv")},
                                            content = function(file) {
                                              write.csv2(DATA$graph_comp1$data_mean2, file)
                                            })
    output$download_plot4_comp1 <- downloadHandler(filename = function() { paste("Chart_table_1_4_2.csv")},
                                             content = function(file) {
                                               write.csv2(DATA$graph_comp1$data_mean_daily2, file)
                                             })
    output$download_plot5_comp1 <- downloadHandler(filename = function() { paste("Chart_table_1_5_2.csv")},
                                             content = function(file) {
                                               write.csv2(DATA$graph_comp1$data_mean3, file)
                                             })
    output$download_plot6_comp1 <- downloadHandler(filename = function() { paste("Chart_table_1_6_2.csv")},
                                             content = function(file) {
                                               write.csv2(DATA$graph_comp1$data_mean_daily3, file)
                                             })    
  
    }
  
  if(nb_graph>=3){
    output$download_plot1_comp2<- downloadHandler(filename = function() { paste0("Chart_table_1_1_3.csv")},
                                                  content = function(file) {
                                                    write.csv2(DATA$graph_comp2$data_mean1, file)
                                                  })
    
    output$download_plot2_comp2 <- downloadHandler(filename = function() { paste0("Chart_table_1_2_3.csv")},
                                                   content = function(file) {
                                                     write.csv2(DATA$graph_comp2$data_mean_daily1, file)
                                                   })
    output$download_plot3_comp2<- downloadHandler(filename = function() { paste0("Chart_table_1_3_3.csv")},
                                                  content = function(file) {
                                                    write.csv2(DATA$graph_comp2$data_mean2, file)
                                                  })
    output$download_plot4_comp2 <- downloadHandler(filename = function() { paste("Chart_table_1_4_3.csv")},
                                                   content = function(file) {
                                                     write.csv2(DATA$graph_comp2$data_mean_daily2, file)
                                                   })
    output$download_plot5_comp2 <- downloadHandler(filename = function() { paste("Chart_table_1_5_3.csv")},
                                                   content = function(file) {
                                                     write.csv2(DATA$graph_comp2$data_mean3, file)
                                                   })
    output$download_plot6_comp2 <- downloadHandler(filename = function() { paste("Chart_table_1_6_3.csv")},
                                                   content = function(file) {
                                                     write.csv2(DATA$graph_comp2$data_mean_daily3, file)
                                                   })    
    
  }
  
  
  
# Onglet 2 ####

output$titre7<-renderText({"Macronutrients (% of breakfast recommendation)"})
output$dejPlot7 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean1,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
    
  }
})

output$sub_titre7<-renderText({
  if (nrow(DATA$graph_all$data_mean1)==0) {
    NULL
  }else{"*Energy fixed at recommended value for breakfast"} 
})


output$titre8<-renderText({"Macronutrients (% of daily recommendation)"})
output$dejPlot8 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean_daily1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean_daily1,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
    
  }
})

output$sub_titre8<-renderText({
  if (nrow(DATA$graph_all$data_mean_daily1)==0) {
    NULL
  }else{"Red point shows breakfast recommendation"} 
})

output$titre9<-renderText({"Vitamins (% of breakfast recommendation)"})
output$dejPlot9 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean2,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
  }
})

output$titre10<-renderText({"Vitamins (% of daily recommendation)"})
output$dejPlot10 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean_daily2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean_daily2,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
  }
})

output$sub_titre10<-renderText({
  if (nrow(DATA$graph_all$data_mean_daily2)==0) {
    NULL
  }else{"Red point shows breakfast recommendation"} 
})

output$titre11<-renderText({"Minerals (% of breakfast recommendation)"})
output$dejPlot11 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean3,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
  }
})

output$titre12<-renderText({"Minerals (% of daily recommendation)"})
output$dejPlot12 <- renderGirafe({
  if (nrow(DATA$graph_all$data_mean_daily3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
    NULL
  }else{
    girafe_options(girafe(ggobj=DATA$graph_all$mean_daily3,width=1,width_svg = 10, height_svg = 5),
                   opts_selection(type="none"),
                   opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                   opts_toolbar(saveaspng = FALSE)
    )
  }
})

output$sub_titre12<-renderText({
  if (nrow(DATA$graph_all$data_mean_daily3)==0) {
    NULL
  }else{"Red point shows breakfast recommendation"} 
})


## Téléchargements des tableaux de données 

output$downloadPlot7<- renderUI({
  if (nrow(DATA$graph_all$data_mean1)==0) { 
    NULL
  }else{
    downloadButton('download_plot7','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

output$downloadPlot8<- renderUI({
  if (nrow(DATA$graph_all$data_mean_daily1)==0) { 
    NULL
  }else{
    downloadButton('download_plot8','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

output$downloadPlot9<- renderUI({
  if (nrow(DATA$graph_all$data_mean2)==0) { 
    NULL
  }else{
    downloadButton('download_plot9','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

output$downloadPlot10<- renderUI({
  if (nrow(DATA$graph_all$data_mean_daily2)==0) { 
    NULL
  }else{
    downloadButton('download_plot10','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

output$downloadPlot11<- renderUI({
  if (nrow(DATA$graph_all$data_mean3)==0) { 
    NULL
  }else{
    downloadButton('download_plot11','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

output$downloadPlot12<- renderUI({
  if (nrow(DATA$graph_all$data_mean_daily3)==0) { 
    NULL
  }else{
    downloadButton('download_plot12','Download data table',class='btn btn-primary',
                   style='font-size:12px;')}
})

#contenu des tableaux de données 

output$download_plot7 <- downloadHandler(filename = function() { paste("Chart_table_2_1.csv")},
                                         content = function(file) {
                                           write.csv2(DATA$graph_all$data_mean1, file)
                                         }) 
output$download_plot8 <- downloadHandler(filename = function() { paste("Chart_table_2_2.csv")},
                                         content = function(file) {
                                           write.csv2(DATA$graph_all$data_mean_daily1, file)
                                         })
output$download_plot9 <- downloadHandler(filename = function() { paste("Chart_table_2_3.csv")},
                                         content = function(file) {
                                           write.csv2(DATA$graph_all$data_mean2, file)
                                         })
output$download_plot10 <- downloadHandler(filename = function() { paste("Chart_table_2_4.csv")},
                                          content = function(file) {
                                            write.csv2(DATA$graph_all$data_mean_daily2, file)
                                          })
output$download_plot11 <- downloadHandler(filename = function() { paste("Chart_table_2_5.csv")},
                                          content = function(file) {
                                            write.csv2(DATA$graph_all$data_mean3, file)
                                          })
output$download_plot12 <- downloadHandler(filename = function() { paste("Chart_table_2_6.csv")},
                                          content = function(file) {
                                            write.csv2(DATA$graph_all$data_mean_daily3, file)
                                          })


#Onglet 3 ####
 
  output$titre13<-renderText({"Macronutrients"})
  output$dejPlot13  <- renderGirafe({
    if (nrow(DATA$graph$data_preval1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$preval1,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE))
    }
  })
  
  output$sub_titre13<-renderText({
    if (nrow(DATA$graph$data_preval1)==0) {
      NULL
    }else{"*Energy fixed at recommended value for breakfast"} 
  })
  
  output$titre14<-renderText({"Vitamins"})
  output$dejPlot14  <- renderGirafe({
    if (nrow(DATA$graph$data_preval2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$preval2,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE))
    }
  })
  
  output$titre15<-renderText({"Minerals"})
  output$dejPlot15 <- renderGirafe({
    if (nrow(DATA$graph$data_preval3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
      NULL
    }else{
      girafe_options(girafe(ggobj=DATA$graph$preval3,width_svg=gwidth,height_svg=gheight),
                     opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                     opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                     opts_toolbar(saveaspng = FALSE))
    }
  })
  
 # comparaison à 2
  
  if (nb_graph>=2){
    
    output$titre13_comp1<-renderText({"Macronutrients"})
    output$dejPlot13_comp1  <- renderGirafe({
      if (nrow(DATA$graph$data_preval1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp1$preval1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
    output$sub_titre13_comp1<-renderText({
      if (nrow(DATA$graph_comp1$data_preval1)==0) {
        NULL
      }else{"*Energy fixed at recommended value for breakfast"} 
    })
    
    output$titre14_comp1<-renderText({"Vitamins"})
    output$dejPlot14_comp1  <- renderGirafe({
      if (nrow(DATA$graph_comp1$data_preval2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp1$preval2,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
    output$titre15_comp1<-renderText({"Minerals"})
    output$dejPlot15_comp1 <- renderGirafe({
      if (nrow(DATA$graph_comp1$data_preval3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp1$preval3,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
  }
  
#Comparaison à 3
  
  if (nb_graph>=3){
    
    output$titre13_comp2<-renderText({"Macronutrients"})
    output$dejPlot13_comp2  <- renderGirafe({
      if (nrow(DATA$graph$data_preval1)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$preval1,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
    output$sub_titre13_comp2<-renderText({
      if (nrow(DATA$graph_comp2$data_preval1)==0) {
        NULL
      }else{"*Energy fixed at recommended value for breakfast"} 
    })
    
    output$titre14_comp2<-renderText({"Vitamins"})
    output$dejPlot14_comp2  <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_preval2)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$preval2,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
    output$titre15_comp2<-renderText({"Minerals"})
    output$dejPlot15_comp2 <- renderGirafe({
      if (nrow(DATA$graph_comp2$data_preval3)==0) { # si il n'y a pas de graphique, on ne l'affiche pas
        NULL
      }else{
        girafe_options(girafe(ggobj=DATA$graph_comp2$preval3,width_svg=gwidth,height_svg=gheight),
                       opts_selection(type="single",css="cursor:pointer;fill-opacity:.6;"),
                       opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                       opts_toolbar(saveaspng = FALSE))
      }
    })
    
  }
  
  
 ## Téléchargement des tableaux de données ##
  
  output$downloadPlot13<- renderUI({
    if (nrow(DATA$graph$data_preval1)==0) { 
      NULL
    }else{
      downloadButton('download_plot13','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  output$downloadPlot14<- renderUI({
    if (nrow(DATA$graph$data_preval2)==0) { 
      NULL
    }else{
      downloadButton('download_plot14','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  output$downloadPlot15<- renderUI({
    if (nrow(DATA$graph$data_preval3)==0) { 
      NULL
    }else{
      downloadButton('download_plot15','Download data table',class='btn btn-primary',
                     style='font-size:12px;')}
  })
  
  #comparaison à 2
  
  if(nb_graph>=2){
    
    output$downloadPlot13_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_preval1)==0) { 
        NULL
      }else{
        downloadButton('download_plot13_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot14_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_preval2)==0) { 
        NULL
      }else{
        downloadButton('download_plot14_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot15_comp1<- renderUI({
      if (nrow(DATA$graph_comp1$data_preval3)==0) { 
        NULL
      }else{
        downloadButton('download_plot15_comp1','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
  }
  
#comparaison à 3
  
  if(nb_graph>=3){
    
    output$downloadPlot13_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_preval1)==0) { 
        NULL
      }else{
        downloadButton('download_plot13_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot14_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_preval2)==0) { 
        NULL
      }else{
        downloadButton('download_plot14_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })
    
    output$downloadPlot15_comp2<- renderUI({
      if (nrow(DATA$graph_comp2$data_preval3)==0) { 
        NULL
      }else{
        downloadButton('download_plot15_comp2','Download data table',class='btn btn-primary',
                       style='font-size:12px;')}
    })

  }
  
  # Création des fichiers CSV

  output$download_plot13<- downloadHandler(filename = function() { paste("Chart_table_3_1_1.csv")},
                                                             content = function(file) {
                                             write.csv2(DATA$graph$data_preval1, file)
                                           }) 
  output$download_plot14 <- downloadHandler(filename = function() { paste("Chart_table_3_2_1.csv")},
                                                              content = function(file) {
                                             write.csv2(DATA$graph$data_preval2, file)
                                           })
  output$download_plot15 <- downloadHandler(filename = function() { paste("Chart_table_3_3_1.csv")},
                                                              content = function(file) {
                                             write.csv2(DATA$graph$data_preval3, file)
                                           })

#compraison à 2
  if(nb_graph>=2){
    
    output$download_plot13_comp1<- downloadHandler(filename = function() { paste("Chart_table_3_1_2.csv")},
                                             content = function(file) {
                                               write.csv2(DATA$graph_comp1$data_preval1, file)
                                             }) 
    output$download_plot14_comp1 <- downloadHandler(filename = function() { paste("Chart_table_3_2_2.csv")},
                                              content = function(file) {
                                                write.csv2(DATA$graph_comp1$data_preval2, file)
                                              })
    output$download_plot15_comp1 <- downloadHandler(filename = function() { paste("Chart_table_3_3_2.csv")},
                                              content = function(file) {
                                                write.csv2(DATA$graph_comp1$data_preval3, file)
                                              })
    
  }

#comparaison à 3
  
  if(nb_graph>=3){
    
    output$download_plot13_comp2<- downloadHandler(filename = function() { paste("Chart_table_3_1_3.csv")},
                                                   content = function(file) {
                                                     write.csv2(DATA$graph_comp2$data_preval1, file)
                                                   }) 
    output$download_plot14_comp2 <- downloadHandler(filename = function() { paste("Chart_table_3_2_3.csv")},
                                                    content = function(file) {
                                                      write.csv2(DATA$graph_comp2$data_preval2, file)
                                                    })
    output$download_plot15_comp2 <- downloadHandler(filename = function() { paste("Chart_table_3_3_3.csv")},
                                                    content = function(file) {
                                                      write.csv2(DATA$graph_comp2$data_preval3, file)
                                                    })
    
  }

output$erreur_msg<-renderUI({NULL})
output$erreur_msg_comp1<-renderUI({NULL})
output$erreur_msg_comp2<-renderUI({NULL})

  
}








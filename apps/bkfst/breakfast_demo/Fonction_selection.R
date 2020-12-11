#FONCTION QUI AFFICHE LE GRAPHIQUE DES POINTS LORS D'UN CLIC SUR LA BARRE SELECTIONNEE

render_selection<-function(output,DATA,selected_chart,set){

#selected_chart= graphique sur lequel on fait la selection  
#set = numéro du set de selection
  
if (set==1){
  DATA_graph=DATA$graph
}
if (set==2){
  DATA_graph=DATA$graph_comp1 
}
if (set==3){
  DATA_graph=DATA$graph_comp2
}
if(!(set%in%c(1,2,3))){
 stop ("Please precise set between 1,2 and 3")
}
  
DATA$x=setNames(DATA$list_colnut,DATA$label_colnut) 
  
selection= unlist(strsplit(selected_chart,",")) #sépare dans un vecteur nutriment ([1]) et intervalle ([2])
#selection_nut=selection[1]
selection_int=selection[2]

#Définition du graphique
if(grepl("_int",selection[1])&selection_int==">MAX"){
  selection_nut=gsub("_int","_upper",selection[1])
  selection_table=DATA_graph$data_points%>%filter(Nutrient==selection_nut &Interval.pct4==selection_int)
}
if(grepl("_int",selection[1])&selection_int!=">MAX"){
  selection_nut=gsub("_int","_lower",selection[1])
  selection_table=DATA_graph$data_points%>%filter(Nutrient==selection_nut&Interval.pct4==selection_int)
}
if(grepl("_lower",selection[1])|grepl("_upper",selection[1])){
  selection_nut=selection[1]
  selection_table=DATA_graph$data_points%>%filter(Nutrient_int==selection_nut&Interval.pct4==selection_int)
  
}

selection_graph <- plot_points(selection_table)

title_var=ifelse(selection_int%in%c("Meet","<MAX",">MIN","MIDDLE"),"meeting",
                 ifelse(selection_int==">MAX","above maximum",
                        ifelse(selection_int=="<MIN","under minimum",NA)))

graph_name=paste0("graph_",deparse(substitute(selected_chart))) #prend nom de la variable
graph_name=gsub("\\()","",graph_name) #enlève les "()" si il y en a


suppressMessages(
#Apparition de la fenêtre
showModal(modalDialog(
  title=fluidRow(column(11,p(style="display:inline;",
                             paste("Nutrient intakes for breakfasts",title_var,"recommendation"))), # titre de ton modal
                 column(1,p(style="display:inline;",
                            actionButton("close_graph",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
  size="l",
  fluidPage(style="margin-bottom:30px;",
            shinyjs::useShinyjs(),
            column(12,align="center",
                   h3(names(DATA$x)[DATA$x == as.character(selection_nut)]),
                   girafeOutput(as.character(graph_name)),
                   h6("*Energy fixed at recommended value for breakfast"),
                   h6("Nutrient intakes over 200% were cut to 200"))),
  footer=NULL,easyClose = FALSE
))
)

#Render du graph = composé de pluisuers graphs
output[[as.character(graph_name)]] <- renderGirafe({
  girafe_options(girafe(ggobj=selection_graph,width=0.75),
                 opts_selection(type="single"),
                 opts_hover(css="cursor:pointer;fill-opacity:.4;"),
                 opts_tooltip(zindex=100000000),
                 opts_toolbar(saveaspng = FALSE)
  )
})



}
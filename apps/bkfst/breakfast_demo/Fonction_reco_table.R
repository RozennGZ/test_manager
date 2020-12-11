
#Construit tableau des recos qui s'affiche
reco_table<-function(output,recommendations,page){


showModal(modalDialog(
  title=fluidRow(column(11,p(style="display:inline;",
                             paste("Recommendations summmary"))), # titre de ton modal
                 column(1,p(style="display:inline;",
                            actionButton("close_graph",icon=icon("remove"),label=NULL,class="btn btn-danger")))),
  size="l",
  fluidPage(style="margin-bottom:30px;",
            column(12,align="center",
                   dataTableOutput("reco_table1"),
                   h4("%EN: % of breakfast ENergy"),
                   h5("Added sugars, Fats and Carbohydrates can be expressed with fixed breakfast Energy (ie 400 kcal for adults and 300kcal for children)")
            )),
  footer=NULL,easyClose = FALSE
))

recommendations$Nutrient=as.character(recommendations$Nutrient)
table=recommendations%>%mutate(order=ifelse(startsWith(Nutrient,"Add_sugars"),5,
                                                      ifelse(startsWith(Nutrient,"Carb"),3,
                                                             ifelse(startsWith(Nutrient,"Fat"),6,
                                                                    ifelse(startsWith(Nutrient,"Saturated_fat"),7,
                                                                           ifelse(startsWith(Nutrient,"Sodium"),12,
                                                                                  ifelse(startsWith(Nutrient,"Energy"),1,
                                                                                         ifelse(startsWith(Nutrient,"Proteins"),2,
                                                                                                ifelse(startsWith(Nutrient,"Fibre"),4,
                                                                                                       ifelse(startsWith(Nutrient,"Calcium"),9,
                                                                                                              ifelse(startsWith(Nutrient,"Iron"),10,
                                                                                                                     ifelse(startsWith(Nutrient,"Magnesium"),11,
                                                                                                                            ifelse(startsWith(Nutrient,"Zinc"),13,
                                                                                                                                   ifelse(startsWith(Nutrient,"Vitamin"),8,0))))))))))))))%>%
  filter(order!=0)%>%arrange(order)%>%select(Nutrient,Age,Daily_reco_unit,Bkf_reco_lower,Bkf_reco_upper)%>%
  mutate(Bkf_reco=gsub("EN","",paste(Bkf_reco_lower,Bkf_reco_upper,sep="-")))%>%select(-Bkf_reco_lower,-Bkf_reco_upper)%>%spread(Age,Bkf_reco)

output[[paste0('reco_table',page)]]<-renderDataTable({
  DT::datatable({table},
                rownames=FALSE,
                colnames=c('Recommendation unite'='Daily_reco_unite','Adults (MIN-MAX)'='Adult',
                           'Children (MIN-MAX)'='Children'),
                options = list(dom='t',ordering = FALSE,pageLength=50,paging = FALSE,
                               display="compact",
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#5cb85c', 'color': '#fff'});",
                                 "}"),
                               columnDefs = list(list(className = 'dt-center', targets = 1:3)))
  )#%>%
  #    formatStyle("Adults (MIN-MAX)",fontWeight = 'bold')
})

}
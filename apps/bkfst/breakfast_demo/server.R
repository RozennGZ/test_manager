function(input, output,session) {
  
  DATA<-reactiveValues(file_reco=NULL,file1=NULL,file2=NULL,erreurs_file=list(file_reco=NULL,file1=NULL,file2=NULL),
                       recommendations=NULL,compo_cereals=NULL,compo_imported=NULL,compo_autres=NULL,
                       region_levels=NULL,region_levels_comp1=NULL,region_levels_comp2=NULL,input_table=NULL,EXPORT_TABLE=NULL,EXPORT_TABLE_comp1=NULL,EXPORT_TABLE_comp2=NULL,
                       compare_nb=c(0,0),graph=NULL,graph_comp1=NULL,graph_comp2=NULL,graph_all=NULL,
                       list_colnut=c("Energy.kcal_dej_lower.pct","Energy.kcal_dej_upper.pct","Energy.kcal_dej_int.pct","Proteins.g_dej_lower.pct", 
                                      "Carbohydrates.g_dej_lower.pctKCAL","Carbohydrates.g_dej_upper.pctKCAL","Carbohydrates.g_dej_int.pctKCAL","Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL", 
                                      "Fat.g_dej_lower.pctKCAL","Fat.g_dej_upper.pctKCAL", "Fat.g_dej_int.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
                                      "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
                                      "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
                                      "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct"),
                       label_colnut=c("Energy (% of minimum recommendation)","Energy (% of maximum recommendation)","Energy","Protein (% of minimum recommendation)",
                                      "Carb. (% of minimum recommendation)*","Carb. (% of maximum recommendation)*","Carb.*","Fibre (% of minimum recommendation)",
                                       "Ad. Sugars* (% of maximum recommendation)","Fat (% of minimum recommendation)*","Fat (% of maximum recommendation)*",
                                        "Fat*","SFA (% of maximum recommendation)*","Vit. A  (% of minimum recommendation)","Vit. B1  (% of minimum recommendation)", 
                                       "Vit. B2  (% of minimum recommendation)", "Vit. B3  (% of minimum recommendation)","Vit. B6  (% of minimum recommendation)",
                                       "Vit. B9  (% of minimum recommendation)","Vit. B12  (% of minimum recommendation)","Vit. C  (% of minimum recommendation)", 
                                        "Vit. D  (% of minimum recommendation)","Calcium  (% of minimum recommendation)","Iron  (% of minimum recommendation)",
                                         "Magnesium  (% of minimum recommendation)","Sodium  (% of minimum recommendation)","Zinc  (% of minimum recommendation)"),
                       x=NULL)
  

  

# Encadré avec description breakfast
 # output$breakfast_description<-renderUI({
 #    HTML("<div class='bordered col-sm-4 col-sm-offset-4'>
 #         <p><b>Cereals</b> : Variable<br>
 #         <b>Milk</b> : Semi-skimmed, 125 mL<br>
 #         <b>Fruit</b> : Average fruit, 99 g for children and 105 g for adults</p>
 #         </div>")
 #  }) 
  
 #Template 
 output$template_cereals <- downloadHandler(
   filename = 'TEMPLATE_CEREALS.csv',
   content = function(file) {
     file.copy("data/TEMPLATE_CEREALS.csv", file)
   }
 )
 
 output$template_reco <- downloadHandler(
   filename = 'TEMPLATE_RECOMMENDATIONS.xlsx',
   content = function(file) {
     file.copy("data/TEMPLATE_RECOMMENDATIONS.xlsx", file)
   }
 )
 
 output$template_component <- downloadHandler(
   filename = 'TEMPLATE_COMPONENT.csv',
   content = function(file) {
     file.copy("data/TEMPLATE_COMPONENT.csv", file)
   }
 )
 
#Chargement de la BD aliments INCA2
 if (exists("INCA2")==FALSE){
   load(file="data/INCA2.Rdata")
   INCA2$FDXL1_N=as.character(INCA2$FDXL1_N)
   INCA2=INCA2%>%filter(!(FDXL1_N%in%c("Alcoholic beverages","Animal composite dishes","Vegetable composite dishes"))&
                          FOOD_N!="0")%>%
         rename(Product_name=FOOD_N)
   INCA2$Product_name=iconv(as.character(INCA2$Product_name),from="LATIN1",to="UTF8")
   INCA2$Product_name=gsub("'","",INCA2$Product_name)
   INCA2=INCA2[!duplicated(INCA2$Product_name), ] #enlève les lignes dupliquées
   INCA2 <<- INCA2
 }
 

  
##################################### Parameter Settings ################################################  


hideElement("div_analysis") #cache la division résultats  
  
ParameterSettings(input,output,session,DATA)
  

  
#################################### Portfolio Analysis #########################################

output$Panel2<-renderUI({column(12,align="center",h4("Add at least one breakfast to access analysis",style="color:red;"))})

PortfolioAnalysis(input,output,session,DATA)




}
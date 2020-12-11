
#Chargement des fichiers 

#calcul_apports(compo_cereals_test,compo_autres_test,reco_test) #utilisation de la fonction

# Fonction génère tableaux avec apports en %
calcul_apports<-function(compo_cereals,reco,compo_autres){

withProgress(message = "Please wait...",{  
  
  #Tableau de compos des céréales : Nutriments
  # + colonnes CP_ID,  Serving_size pour les céréales
  # + colonnes Product_name, Product_ID, Serving_size pour les autres composantes 

if (!(is.null(compo_cereals))){ # si il y a un fichier pour les céréales
  
    if("Add_sugars.g"%in%colnames(compo_cereals)==FALSE){
   compo_cereals<-compo_cereals%>%dplyr::mutate(Add_sugars.g=Sugars.g) 
  }

  compo_cereals=compo_cereals%>%
    #Ajout de la colonne "Brand" en fonction du nom du céréale
    dplyr::mutate(Brand=ifelse(grepl("\\<nesquik\\>",Product_name,ignore.case=TRUE),"Nesquik",
                 ifelse(grepl("\\<fitness\\>",Product_name,ignore.case=TRUE),"Fitness",
                 ifelse(grepl("\\<cheerios\\>",Product_name,ignore.case=TRUE),"Cheerios",       
                 ifelse(grepl("\\<chocapic\\>",Product_name,ignore.case=TRUE),"Chocapic",
                 ifelse(grepl("\\<cini minis\\>",Product_name,ignore.case=TRUE),"Cini Minis",
                 ifelse(grepl("\\<cookie crisp\\>",Product_name,ignore.case=TRUE),"Cookie Crisp",
                 ifelse(grepl("\\<corn flakes\\>",Product_name,ignore.case=TRUE),"Corn Flakes",
                 ifelse(grepl("\\<crunch\\>",Product_name,ignore.case=TRUE),"Crunch",
                 ifelse(grepl("\\<estrellitas\\>",Product_name,ignore.case=TRUE),"Estrellitas",
                 ifelse(grepl("\\<gold\\>",Product_name,ignore.case=TRUE),"Gold",
                 ifelse(grepl("\\<golden grahams\\>",Product_name,ignore.case=TRUE),"Golden Grahams",
                 ifelse(grepl("\\<golden nuggets\\>",Product_name,ignore.case=TRUE),"Golden Nuggets",
                 ifelse(grepl("\\<khrutka\\>",Product_name,ignore.case=TRUE),"Khrutka",
                 ifelse(grepl("\\<koko krunch\\>",Product_name,ignore.case=TRUE),"Koko Krunch",
                 ifelse(grepl("\\<lion\\>",Product_name,ignore.case=TRUE),"Lion",
                 ifelse(grepl("\\<milo\\>",Product_name,ignore.case=TRUE),"Milo",
                 ifelse(grepl("\\<clusters\\>",Product_name,ignore.case=TRUE),"Clusters",
                 ifelse(grepl("\\<muesli\\>",Product_name,ignore.case=TRUE),"Muesli",
                 ifelse(grepl("\\<nesplus\\>",Product_name,ignore.case=TRUE),"Nesplus",
                 ifelse(grepl("\\<oat crisp\\>",Product_name,ignore.case=TRUE),"Oat Crisp",
                 ifelse(grepl("\\<bran\\>",Product_name,ignore.case=TRUE),"Bran",
                 ifelse(grepl("\\<shredded wheat\\>",Product_name,ignore.case=TRUE),"Shredded Wheat",
                 ifelse(grepl("\\<shreddies\\>",Product_name,ignore.case=TRUE),"Shreddies",
                 ifelse(grepl("\\<snow flakes\\>",Product_name,ignore.case=TRUE),"Snow Flakes",
                 ifelse(grepl("\\<stars\\>",Product_name,ignore.case=TRUE),"Stars",
                 ifelse(grepl("\\<uncle tobys\\>",Product_name,ignore.case=TRUE),"Uncle Tobys (UT)",
                 ifelse(grepl("\\<zucosos\\>",Product_name,ignore.case=TRUE),"Zucosos",
                 ifelse(grepl("\\<vitabrits\\>",Product_name,ignore.case=TRUE),"VitaBrits",
                 ifelse(grepl("\\<plus\\>",Product_name,ignore.case=TRUE),"Plus",      
                 ifelse(grepl("\\<cui man pian\\>",Product_name,ignore.case=TRUE),"Cui Man Pian",  
                 ifelse(grepl("\\<fibre 1\\>",Product_name,ignore.case=TRUE),"Fibre 1",
                 ifelse(grepl("\\<frutina\\>",Product_name,ignore.case=TRUE),"Frutina",
                 ifelse(grepl("\\<healthwise heart\\>",Product_name,ignore.case=TRUE),"Healthwise Heart",
                 ifelse(grepl("\\<honey flakes\\>",Product_name,ignore.case=TRUE),"Honey Flakes",
                 ifelse(grepl("\\<kangus\\>",Product_name,ignore.case=TRUE),"Kangus",
                 ifelse(grepl("\\<kosmostars\\>",Product_name,ignore.case=TRUE),"Kosmostars",
                 ifelse(grepl("\\<moca\\>",Product_name,ignore.case=TRUE),"Moca",
                 ifelse(grepl("\\<oatbrits\\>",Product_name,ignore.case=TRUE),"Oatbrits",
                 ifelse(grepl("\\<rice crispies\\>",Product_name,ignore.case=TRUE),"Rice Crispies",
                 ifelse(grepl("\\<strawberry minis\\>",Product_name,ignore.case=TRUE),"Strawberry Minis",
                   "Other")))))))))))))))))))))))))))))))))))))))))%>%
    #Prend toutes les colonnes dans l'ordre :    
    select(CP_ID,volume,Product_type,Product_name,Brand,Region,Serving_size, Energy.kcal, Proteins.g, Carbohydrates.g,#Sugars.g, 
           Add_sugars.g,Fat.g,Saturated_fat.g,Fibre.g,Sodium.mg,Vitamin_D.mcg,Vitamin_C.mg,Vitamin_B1.mg,Vitamin_B2.mg,Vitamin_B3.mg,
           Vitamin_B6.mg,Vitamin_B9.mcg,Vitamin_B12.mcg,Vitamin_A.mg,Calcium.mg,Iron.mg,Zinc.mg,Magnesium.mg,
           everything())              
  
  
  
  #ETAPE 1A : Calcul de la compo des petits dej et des autres composantes pour une portion
  
  #Compo céréales pour une portion (serving_size)
  compo_cereals=compo_cereals%>%mutate_at(.funs = list(cereals=~(./100)*Serving_size), .vars = vars(Energy.kcal:Magnesium.mg))%>%
                arrange(CP_ID)%>%dplyr::rename(Serving_size_cereals=Serving_size)%>% 
                select(-(Energy.kcal:Magnesium.mg))   #Supprime les colonnes avec compo pour 100 g de céréales (inutile pour la suite):
}
  
# si il y a des composants en plus : 
  
if (!is.null(compo_autres)){ 
  
  components_nb=nrow(compo_autres)
  compo_components=list()
 
  if(components_nb>=1){ 
  for(i in 1:components_nb){
    
   compo_components[[i]]= compo_autres%>%filter(Product_ID==i)%>%
                          mutate_at(.funs = list(newcol=~(./100)*Serving_size), .vars = vars(Energy.kcal:Magnesium.mg))%>%
                          setNames(sub("_newcol",paste0("_compo",i), names(.)))%>% #change le nom du suffixe par le numéro du composant
                          dplyr::rename(!!paste0("compo",i,"_name"):=Product_name,
                                        !!paste0("Serving_size_compo",i):=Serving_size)%>%
                          select(#Age,
                            !!paste0("compo",i,"_name"),!!paste0("Serving_size_compo",i),!!paste0("Energy.kcal_compo",i):!!paste0("Magnesium.mg_compo",i))

  }}
  
}else{
  components_nb=0
}
  
   # compo_lait=compo_autres%>%filter(Product_ID=="Milk")%>%
   #    mutate_at(.funs = list(milk=~(./100)*Serving_size), .vars = vars(Energy.kcal:Magnesium.mg))%>%
   #    dplyr::rename(Milk_name=Product_name,Serving_size_milk=Serving_size)%>%select(#Age,
   #      Milk_name,Serving_size_milk,Energy.kcal_milk:Magnesium.mg_milk)
   #  compo_fruit=compo_autres%>%filter(Product_ID=="Fruit")%>%
   #    mutate_at(.funs = list(fruit=~(./100)*Serving_size), .vars = vars(Energy.kcal:Magnesium.mg))%>%
   #    dplyr::rename(Fruit_name=Product_name,Serving_size_fruit=Serving_size)%>%select(#Age,
   #      Fruit_name,Serving_size_fruit,Energy.kcal_fruit:Magnesium.mg_fruit)
   

#ETAPE 1B : Calcul de la compo totale = 1 céréale de petit dej + composantes ####
    
  #dédouble ligne céréales en adulte/enfants
  #  compo_cereals_adult=compo_cereals%>%dplyr::mutate(Age="Adult")
  #  compo_cereals_children=compo_cereals%>%dplyr::mutate(Age="Children")
    
#  compo_cereals=rbind(compo_cereals_children,compo_cereals_adult)

if (!(is.null(compo_cereals))){
  
  #Merge de la table des céréales avec les compo et taille de portions des composantes
  total_table=compo_cereals #Initialisation
  cereals_nb=1
  if (components_nb>=1){
    for(i in 1:components_nb){
      total_table<-total_table%>%tibble::add_column(!!!compo_components[[i]])
    }}
  
}else{
  
  total_table=NULL #Initialisation
  cereals_nb=0
  for(i in 1:components_nb){
    total_table<-bind_cols(total_table,compo_components[[i]])
  }
  total_table=total_table%>%mutate(CP_ID="CP_0")
}
  
 total_table=total_table%>%mutate(Components_nb=components_nb,Cereals_nb=cereals_nb) 
       
 # tibble::add_column(!!!compo_fruit%>%filter(Fruit_name=="Fruit_average")) #choisi le fruit (ici : fruit moyen)
 
 # Aditionne compo céréale + compo composants (méthode : https://stackoverflow.com/questions/49694353/dplyr-mutating-multiple-columns-by-prefix-and-suffix) 

 incProgress(amount = 0.1)
      suppressMessages(
      output_table <- purrr::map(c( "Serving_size","Energy.kcal","Proteins.g","Carbohydrates.g",#"Sugars.g",
                            "Add_sugars.g","Fat.g","Saturated_fat.g",
                           "Fibre.g","Sodium.mg","Vitamin_D.mcg","Vitamin_C.mg","Vitamin_B1.mg","Vitamin_B2.mg","Vitamin_B3.mg","Vitamin_B6.mg",
                           "Vitamin_B9.mcg","Vitamin_B12.mcg","Vitamin_A.mg","Calcium.mg","Iron.mg","Zinc.mg","Magnesium.mg"),
                          ~ total_table %>%
                          dplyr::mutate(!!paste0(.x, "_dej") :=
                                          ifelse(Components_nb==0,
                                                 !!as.name(paste0(.x, "_cereals")),
                                                 ifelse(Components_nb==1,
                                                        ifelse(Cereals_nb==1,
                                                        !!as.name(paste0(.x, "_cereals")) + !!as.name(paste0(.x, "_compo1")),
                                                        !!as.name(paste0(.x, "_compo1"))),
                                                        ifelse(Components_nb==2,
                                                               ifelse(Cereals_nb==1,
                                                               !!as.name(paste0(.x, "_cereals")) + !!as.name(paste0(.x, "_compo1")) +!!as.name(paste0(.x, "_compo2")),
                                                               !!as.name(paste0(.x, "_compo1")) +!!as.name(paste0(.x, "_compo2"))),
                                                               ifelse(Components_nb==3,
                                                                      ifelse(Cereals_nb==1,
                                                                      !!as.name(paste0(.x, "_cereals")) + !!as.name(paste0(.x, "_compo1")) +
                                                                       !!as.name(paste0(.x, "_compo2"))+ !!as.name(paste0(.x, "_compo3")),
                                                                      !!as.name(paste0(.x, "_compo1")) +!!as.name(paste0(.x, "_compo2"))+ !!as.name(paste0(.x, "_compo3"))),
                                                                      ifelse(Components_nb==4,
                                                                             ifelse(Cereals_nb==1,
                                                                             !!as.name(paste0(.x, "_cereals")) + !!as.name(paste0(.x, "_compo1")) +
                                                                              !!as.name(paste0(.x, "_compo2"))+ !!as.name(paste0(.x, "_compo3"))+!!as.name(paste0(.x, "_compo4")),
                                                                             !!as.name(paste0(.x, "_compo1")) +!!as.name(paste0(.x, "_compo2"))+ 
                                                                              !!as.name(paste0(.x, "_compo3"))+!!as.name(paste0(.x, "_compo4"))),
                                                                             ifelse(Components_nb==5,
                                                                                    ifelse(Cereals_nb==1,
                                                                                    !!as.name(paste0(.x, "_cereals")) + !!as.name(paste0(.x, "_compo1")) +
                                                                                     !!as.name(paste0(.x, "_compo2"))+ !!as.name(paste0(.x, "_compo3"))+
                                                                                      !!as.name(paste0(.x, "_compo4"))+  !!as.name(paste0(.x, "_compo5")),
                                                                                    !!as.name(paste0(.x, "_compo1")) +!!as.name(paste0(.x, "_compo2"))+ 
                                                                                      !!as.name(paste0(.x, "_compo3"))+!!as.name(paste0(.x, "_compo4"))+  
                                                                                       !!as.name(paste0(.x, "_compo5"))),NA
                                                                                    )))))))
                        )%>%reduce(left_join) 
                      )
      
    
incProgress(amount = 0.4,message = "Calculation ...")
 
# ETAPE 1C : COMPARAISON AVEC LES RECOMMANDATIONS ####

      # Mise en forme reco :
      reco_dej=reco%>%select(Nutrient,Type,Bkf_reco,Bkf_reco_unit)%>%dplyr::mutate(Nutrient=paste0(Nutrient,"_dej"))
      
      reco_daily=reco%>%select(Nutrient,Type,Daily_reco,Daily_reco_unit,Bkf_reco_pct)%>%dplyr::mutate(Nutrient=paste0(Nutrient,"_dej"))
      ####    
      
 #RECOMMANDATIONS PETIT DEJEUNERS ####
      
  #dédouble en "lower/upper/fixed" pour les recommandations hautes et basses  et fixes (pour énergie)
  calcul_table_lower=output_table%>%dplyr::mutate(Type="lower")
  calcul_table_upper=output_table%>%dplyr::mutate(Type="upper")
  calcul_table_fixed=output_table%>%dplyr::mutate(Type="fixed")
 
 calcul_table=rbind(calcul_table_lower,calcul_table_upper,calcul_table_fixed)

 #Energie fixée hypothetique d'un petit déj :(Energie hypothétique pour un petit dej = Reco journalière fixée en energie *  Pourcentage du petit dej (20% en général))
  Energy.kcal_fixed=reco_dej%>%filter(Nutrient=="Energy.kcal_dej"&Type=="fixed")%>%pull(Bkf_reco)
 # Nom de la première variables dépends des composantes du petit dej 
  first_var=ifelse(cereals_nb==1,"Energy.kcal_cereals","Energy.kcal_compo1")

  if(cereals_nb==1){
    additionnal_vars= c("CP_ID","volume","Product_type","Product_name","Region","Serving_size_cereals")
  }else{
    additionnal_vars=c("CP_ID","compo1_name","Serving_size_compo1")}
                       
suppressWarnings(
calcul_table<-calcul_table%>% select(Type,!!additionnal_vars,!!first_var:Magnesium.mg_dej)%>%
             #Isole l'apport énergétique réelle
             mutate(Energy.kcal_tot=Energy.kcal_dej)%>%
             #met nutriments en colonnes
             gather("Nutrient","Value",Energy.kcal_dej:Magnesium.mg_dej)%>%
             left_join(reco_dej,by=c("Nutrient",#"Age",
                                     "Type"))%>%

  #converti les nutriments en energie
  dplyr::mutate(Value.kcal=ifelse(Nutrient%in%c("Proteins.g_dej","Carbohydrates.g_dej",#"Sugars.g_dej",
                                                            "Add_sugars.g_dej"),Value*4,
                                       ifelse(Nutrient%in%c("Fat.g_dej","Saturated_fat.g_dej"),Value*9,NA)))%>%

  # Converti en % de la reco
  dplyr::mutate(Value.pct=ifelse(Bkf_reco_unit=="%EN",(Value.kcal/(Bkf_reco*0.01*Energy.kcal_tot))*100,
                                 Value/Bkf_reco*100))%>%
  dplyr::mutate(Nutrient.pct=paste0(Nutrient,"_",Type,".pct"))%>%

 # # Converti en % de la recco pour 400 kcal (energie recommandée pour un petit dej ADULTES)
 # #pour les reccos exprimées en % d'energie (sucres ajoutés, glucides, lipides, ags)
  # dplyr::mutate(Value.pct400=ifelse(Bkf_reco_unit=="%EN",(Value.kcal/(Bkf_reco*0.01*400))*100,
  #                                   NA))%>%
  # dplyr::mutate(Nutrient.pct400=paste0(Nutrient,"_",Type,".pct400"))%>%
  # 
  # 
  # # Converti en % de la recco pour 300 kcal (energie recommandée pour un petit dej ENFANTS)
  # #pour les reccos exprimées en % d'energie (sucres ajoutés, glucides, lipides, ags)
  # dplyr::mutate(Value.pct300=ifelse(Bkf_reco_unit=="%EN",(Value.kcal/(Bkf_reco*0.01*300))*100,
  #                                   NA))%>%
  # dplyr::mutate(Nutrient.pct300=paste0(Nutrient,"_",Type,".pct300"))%>%
  
# # Converti en % de la recco pour l'Energie du petit dejeuné fixée
# #pour les reccos exprimées en % d'energie (sucres ajoutés, glucides, lipides, ags)
  dplyr::mutate(Value.pctKCAL=ifelse(Bkf_reco_unit=="%EN",(Value.kcal/(Bkf_reco*0.01*Energy.kcal_fixed))*100,
                                      NA))%>%
  dplyr::mutate(Nutrient.pctKCAL=paste0(Nutrient,"_",Type,".pctKCAL"))

)

#Réarrange le tableau de calcul : nutriments en colonnes
calcul_table.pct=calcul_table%>%select(CP_ID,#Age,
                                       Nutrient.pct,Value.pct)%>%spread(Nutrient.pct,Value.pct)%>%
                  select(CP_ID,#Age,
                         Add_sugars.g_dej_lower.pct:Zinc.mg_dej_upper.pct)
# calcul_table.pct400=calcul_table%>%select(CP_ID,Age,Nutrient.pct400,Value.pct400)%>%spread(Nutrient.pct400,Value.pct400)%>%
#                      select(CP_ID,Age,Add_sugars.g_dej_lower.pct400:Zinc.mg_dej_upper.pct400)
#calcul_table.pct300=calcul_table%>%select(CP_ID,Age,Nutrient.pct300,Value.pct300)%>%spread(Nutrient.pct300,Value.pct300)%>%
#                     select(CP_ID,Age,Add_sugars.g_dej_lower.pct300:Zinc.mg_dej_upper.pct300)
calcul_table.pctKCAL=calcul_table%>%select(CP_ID,Nutrient.pctKCAL,Value.pctKCAL)%>%spread(Nutrient.pctKCAL,Value.pctKCAL)%>%
                     select(CP_ID,Add_sugars.g_dej_lower.pctKCAL:Zinc.mg_dej_upper.pctKCAL)


#RECOMMANDATIONS DAILY ####

 #Isole recommandation journalière en Energie et l'ajoute en colonne au tableau
  reco_daily=reco_daily%>%
    tibble::add_column(!!!reco_daily%>%filter(Nutrient=="Energy.kcal_dej"&Type=="fixed")%>%select(#Age,
                                                                                            Nutrient,Daily_reco)%>%
                         spread(Nutrient,Daily_reco)%>%dplyr::rename(Energy.kcal_daily=Energy.kcal_dej)) #isole Energie dans colonne Energy.kcal_daily
             

suppressWarnings( 
calcul_table.pctDAILY<-rbind(calcul_table_lower,calcul_table_upper,calcul_table_fixed)%>%
                      select(#Age,
                        Type,!!additionnal_vars,!!first_var:Magnesium.mg_dej)%>%
                      #met nutriments en colonnes
                      gather("Nutrient","Value",Energy.kcal_dej:Magnesium.mg_dej)%>%
                      left_join(reco_daily,by=c("Nutrient",#"Age",
                                                "Type"))%>%
  
  #converti recommandation journalière en Energie en g en fonction du macronutriments
  dplyr::mutate(Energy.g_daily=ifelse(Nutrient%in%c("Proteins.g_dej","Carbohydrates.g_dej",#"Sugars.g_dej",
                                                    "Add_sugars.g_dej"),Energy.kcal_daily/4,
                                      ifelse(Nutrient%in%c("Fat.g_dej","Saturated_fat.g_dej"),Energy.kcal_daily/9,NA)))%>%

  # Converti les reco DAILY en g si besoin (si unité = %EN)
  dplyr::mutate(Daily_reco=ifelse(Daily_reco_unit=="%EN",Daily_reco*0.01*Energy.g_daily,Daily_reco))%>%
 
   # Converti les valeurs en nutriments en % de la reco DAILY
  dplyr::mutate(Value.pctDAILY=Value/Daily_reco*100)%>%dplyr::mutate(Nutrient.pctDAILY=paste0(Nutrient,"_",Type,".pctDAILY"))%>%
  #réarrange le tableau :
  select(CP_ID,#Age,
         Nutrient.pctDAILY,Value.pctDAILY)%>%spread(Nutrient.pctDAILY,Value.pctDAILY)%>%
  select(CP_ID,#Age,
         Add_sugars.g_dej_lower.pctDAILY:Zinc.mg_dej_upper.pctDAILY)
)
 
  #Donne la position en % du point (pt) représent la reco Bfk par rapport à Daily
  calcul_table.pt=reco_daily%>%select(#Age,
                                      Nutrient,Type,Bkf_reco_pct)%>%
                   dplyr::mutate(Nutrient.pt=paste0(Nutrient,"_",Type,".pt"))%>%select(-Nutrient,-Type)%>%
                   spread(Nutrient.pt,Bkf_reco_pct)

########## Assemblage de toutes les valeurs
  suppressWarnings(
  output_table<-output_table%>%left_join(calcul_table.pct,by=c("CP_ID"#,"Age"
                                                               ))%>%
                              # left_join(calcul_table.pct400,by=c("CP_ID","Age"))%>%
                              # left_join(calcul_table.pct300,by=c("CP_ID","Age"))%>%
                              left_join(calcul_table.pctKCAL,by="CP_ID")%>%
                              left_join(calcul_table.pctDAILY,by=c("CP_ID"#,"Age"
                                                                   ))%>%
                              tibble::add_column(!!!calcul_table.pt)%>% #ajoute valeurs en %DV (valeurs fixes quelque soit CP_ID)
    select(CP_ID,#Age,
           everything())
)
  #Enlève les colonnes vides (= nutriments pour lesquels il n'y a pas de reco)
   output_table<-output_table[,colSums(is.na(output_table))<nrow(output_table)]

   
  ### ATTENTION AVEC LA TECHNIQUE SUIVANTE : RESULTATS ADULT ET CHILDREN SONT INVERSES : pourquoi ? 
  #    output_table=output_table%>% select(Age,CP_ID:Serving_size_cereals,!!first_var:Magnesium.mg_dej)%>%
  #     group_by(CP_ID)%>%nest()%>%
  #     # dans reco_nut :met les nutriments en colonne et colle le tableau recco pour les reccos inf et sup
  #     dplyr::mutate(reco_nut = purrr::map(data, ~ .x%>%gather("Nutrient","Value",Energy.kcal_dej:Magnesium.mg_dej)%>%
  #                             left_join(reco_dej,by=c("Nutrient","Age"))
  # 
  #     ))%>%
  #     #Isole l'apport énergétique du petit dej en fonction de enfant/adulte
  #     dplyr::mutate(reco_nut = purrr::map(reco_nut, ~ .x%>%dplyr::mutate(Energy.kcal_dej=ifelse(Age=="Adult",.x%>%filter(Nutrient=="Energy.kcal_dej"&Age=="Adult")%>%pull(Value),
  #                                                                          .x%>%filter(Nutrient=="Energy.kcal_dej"&Age=="Children")%>%pull(Value)))%>%
  #                             #converti les nutriments en energie
  #                             dplyr::mutate(Value.kcal=ifelse(Nutrient%in%c("Proteins.g_dej","Carbohydrates.g_dej",#"Sugars.g_dej",
  #                                                                    "Add_sugars.g_dej"),Value*4,
  #                                                      ifelse(Nutrient%in%c("Fat.g_dej","Saturated_fat.g_dej"),Value*9,NA)))
  #     ))%>%
  #     # Converti en % de la recco inf
  #     dplyr::mutate(reco_lower_pct=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_lower_pct=ifelse(Bkf_reco_lower=="20%EN",(Value.kcal/(0.2*Energy.kcal_dej))*100,
  #                                                                             ifelse(Bkf_reco_lower=="55%EN",(Value.kcal/(0.55*Energy.kcal_dej))*100,
  #                                                                                    (Value/as.numeric(sub(",", ".",Bkf_reco_lower,fixed=TRUE)))*100)))%>%
  #                                 select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                 dplyr::mutate(Nutrient=paste0(Nutrient,"_lower.pct"))%>%spread(Nutrient,reco_lower_pct)%>%
  #                                 select(Add_sugars.g_dej_lower.pct:Zinc.mg_dej_lower.pct)
  #     ))%>%
  #     #converti en % de la recco sup
  #     dplyr::mutate(reco_upper_pct=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_upper_pct=ifelse(Bkf_reco_upper=="10%EN",(Value.kcal/(0.1*Energy.kcal_dej))*100,
  #                                                                             ifelse(Bkf_reco_upper=="30%EN",(Value.kcal/(0.3*Energy.kcal_dej))*100,
  #                                                                                    ifelse(Bkf_reco_upper=="75%EN",(Value.kcal/(0.75*Energy.kcal_dej))*100,
  #                                                                                           (Value/as.numeric(sub(",", ".",Bkf_reco_upper,fixed=TRUE)))*100))))%>%
  #                                 select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                 dplyr::mutate(Nutrient=paste0(Nutrient,"_upper.pct"))%>%spread(Nutrient,reco_upper_pct)%>%
  #                                 select(Add_sugars.g_dej_upper.pct:Zinc.mg_dej_upper.pct)
  #     ))%>%
  #     # Converti en % de la recco pour 400 kcal (energie recommandée pour un petit dej ADULTE)
  #     #pour les reccos exprimées en % d'energie (sucres ajoutés, glucides, lipides, ags)
  #     dplyr::mutate(reco_lower_pct400=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_lower_pct400=ifelse(Bkf_reco_lower=="20%EN",(Value.kcal/(0.2*400))*100,
  #                                                                             ifelse(Bkf_reco_lower=="55%EN",(Value.kcal/(0.55*400))*100,
  #                                                                              NA)))%>%
  #                                 select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                 dplyr::mutate(Nutrient=paste0(Nutrient,"_lower.pct400"))%>%spread(Nutrient,reco_lower_pct400)%>%
  #                                 select(Add_sugars.g_dej_lower.pct400:Zinc.mg_dej_lower.pct400)
  #     ))%>%
  #     dplyr::mutate(reco_upper_pct400=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_upper_pct400=ifelse(Bkf_reco_upper=="10%EN",(Value.kcal/(0.1*400))*100,
  #                                                                             ifelse(Bkf_reco_upper=="30%EN",(Value.kcal/(0.3*400))*100,
  #                                                                                    ifelse(Bkf_reco_upper=="75%EN",(Value.kcal/(0.75*400))*100,
  #                                                                                     NA))))%>%
  #                                 select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                 dplyr::mutate(Nutrient=paste0(Nutrient,"_upper.pct400"))%>%spread(Nutrient,reco_upper_pct400)%>%
  #                                 select(Add_sugars.g_dej_upper.pct400:Zinc.mg_dej_upper.pct400)
  #     ))%>%
  #      # Converti en % de la recco pour 300 kcal (energie recommandée pour un petit dej ENFANTS)
  #      #pour les reccos exprimées en % d'energie (sucres ajoutés, glucides, lipides, ags)
  #      dplyr::mutate(reco_lower_pct300=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_lower_pct300=ifelse(Bkf_reco_lower=="20%EN",(Value.kcal/(0.2*300))*100,
  #                                                                                    ifelse(Bkf_reco_lower=="55%EN",(Value.kcal/(0.55*300))*100,
  #                                                                                           NA)))%>%
  #                                     select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                     dplyr::mutate(Nutrient=paste0(Nutrient,"_lower.pct300"))%>%spread(Nutrient,reco_lower_pct300)%>%
  #                                     select(Add_sugars.g_dej_lower.pct300:Zinc.mg_dej_lower.pct300)
  #      ))%>%
  #      dplyr::mutate(reco_upper_pct300=purrr::map(reco_nut, ~ .x%>%dplyr::mutate(reco_upper_pct300=ifelse(Bkf_reco_upper=="10%EN",(Value.kcal/(0.1*300))*100,
  #                                                                                    ifelse(Bkf_reco_upper=="30%EN",(Value.kcal/(0.3*300))*100,
  #                                                                                           ifelse(Bkf_reco_upper=="75%EN",(Value.kcal/(0.75*300))*100,
  #                                                                                                  NA))))%>%
  #                                     select(-Value,-Bkf_reco_lower,-Bkf_reco_upper,-Energy.kcal_dej,-Value.kcal)%>%
  #                                     dplyr::mutate(Nutrient=paste0(Nutrient,"_upper.pct300"))%>%spread(Nutrient,reco_upper_pct300)%>%
  #                                     select(Add_sugars.g_dej_upper.pct300:Zinc.mg_dej_upper.pct300)
  #      ))%>%
  # 
  #   select(-reco_nut)%>%unnest()%>%ungroup()
  #    
  # 
  #   #Enlève les colonnes vides (= nutriments pour lesquels il n'y a pas de reco) 
  #   output_table<-output_table[,colSums(is.na(output_table))<nrow(output_table)]
  
   
incProgress(amount = 0.6)  

# PROPORTION PAR COMPOSANTE ####
      
    # Calcul des proportions par composante : séparation cas où apport déjeuner = 0(donc pas de division) et apport déjeuner !=0
   
      suppressMessages(
        output_table<- purrr::map(c("Serving_size","Energy.kcal","Proteins.g","Carbohydrates.g",#"Sugars.g",
                          "Add_sugars.g","Fat.g","Saturated_fat.g","Fibre.g","Sodium.mg","Vitamin_D.mcg","Vitamin_C.mg",
                          "Vitamin_B1.mg","Vitamin_B2.mg","Vitamin_B3.mg","Vitamin_B6.mg","Vitamin_B9.mcg",
                          "Vitamin_B12.mcg","Vitamin_A.mg","Calcium.mg","Iron.mg","Zinc.mg", "Magnesium.mg"),
                         ~ output_table %>%
                           dplyr::mutate(!!paste0(.x, "_cereals.prop"):=  ifelse(Cereals_nb>=1,
                                                                                 ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                        !!as.name(paste0(.x, "_cereals"))/!!as.name(paste0(.x, "_dej")),0),NA))%>%
                           
                           dplyr::mutate(!!paste0(.x, "_compo1.prop"):= ifelse(Components_nb>=1,
                                                                               ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                      !!as.name(paste0(.x, "_compo1"))/!!as.name(paste0(.x, "_dej")),0),NA))%>%
                           dplyr::mutate(!!paste0(.x, "_compo2.prop"):=ifelse(Components_nb>=2,
                                                                             ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                    !!as.name(paste0(.x, "_compo2"))/!!as.name(paste0(.x, "_dej")),0),NA))%>%
                           dplyr::mutate(!!paste0(.x, "_compo3.prop"):=ifelse(Components_nb>=3,
                                                                              ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                     !!as.name(paste0(.x, "_compo3"))/!!as.name(paste0(.x, "_dej")),0),NA))%>%
                           dplyr::mutate(!!paste0(.x, "_compo4.prop"):=ifelse(Components_nb>=4,
                                                                              ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                     !!as.name(paste0(.x, "_compo4"))/!!as.name(paste0(.x, "_dej")),0),NA))%>%
                           dplyr::mutate(!!paste0(.x, "_compo5.prop"):=ifelse(Components_nb>=5,
                                                                              ifelse(!!as.name(paste0(.x, "_dej"))!=0,
                                                                                     !!as.name(paste0(.x, "_compo5"))/!!as.name(paste0(.x, "_dej")),0),NA))
                           
        )%>%reduce(left_join) 
      )

  #garde en mémoire les noms des composants :
  output_table<-output_table%>%mutate(compo1_name=ifelse(Components_nb>=1,as.character(compo_autres%>%filter(Product_ID==1)%>%pull(Product_name)),NA),
                        compo2_name=ifelse(Components_nb>=2,as.character(compo_autres%>%filter(Product_ID==2)%>%pull(Product_name)),NA),
                        compo3_name=ifelse(Components_nb>=3,as.character(compo_autres%>%filter(Product_ID==3)%>%pull(Product_name)),NA),
                        compo4_name=ifelse(Components_nb>=4,as.character(compo_autres%>%filter(Product_ID==4)%>%pull(Product_name)),NA),
                        compo5_name=ifelse(Components_nb>=5,as.character(compo_autres%>%filter(Product_ID==5)%>%pull(Product_name)),NA))
  
  #Enlève les colonnes vides (= composantes qui n'existent pas
  output_table<-output_table[,colSums(is.na(output_table))<nrow(output_table)]
    
incProgress(amount = 0.9,message = "Done")
    
    return(output_table)

    }) 
}


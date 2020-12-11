
#FONCTION DE CONSTRUCTION DES GRAPHIQUES ####

#' @title Graphiques d'apports nutritionnels des petites déj
#' @import ggplot2
#' @import ggiraph
#' @param compo_dej : composition de tous les petites déjeuners. si comparaison : doit etre sous forme de liste
#' @param volume : pondération par le volume


plot_dej<-function(compo_dej,volume=FALSE,selected_nut,compare=FALSE,max_ordinate=NULL){

if (!is.null(max_ordinate)){
  if(length(max_ordinate)!=6){
stop("Please enter a vector of length 6 for maximum ordinate value")
  }
}  
   
withProgress(message = "Please wait...",{ 

  
# Nutriments selectionnées
nut_colnames=c("Energy.kcal_dej_fixed.pct","Energy.kcal_dej_lower.pct","Energy.kcal_dej_upper.pct","Energy.kcal_dej_int.pct","Proteins.g_dej_lower.pct", 
               "Carbohydrates.g_dej_lower.pctKCAL","Carbohydrates.g_dej_upper.pctKCAL","Carbohydrates.g_dej_int.pctKCAL","Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL", 
               "Fat.g_dej_lower.pctKCAL","Fat.g_dej_upper.pctKCAL", "Fat.g_dej_int.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
               "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
               "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
               "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct")
#ajout des colonnes DAILY
nut_colnamesDAILY=paste0(gsub("KCAL","",nut_colnames),"DAILY") 
nut_colnames=c(nut_colnames,nut_colnamesDAILY)

nut_truenames=c("Energy","Energy MIN","Energy MAX","Energy","Protein","Carb. MIN*","Carb. MAX*","Carb.*","Fibre","Ad. Sugars*","Fat MIN*","Fat MAX*","Fat*","SFA*",
                "Vit. A","Vit. B1", "Vit. B2", "Vit. B3","Vit. B6","Vit. B9","Vit. B12","Vit. C", "Vit. D",
                "Ca","Iron","Mg","Sodium","Zinc",
                "Energy","Energy MIN","Energy MAX","Energy","Protein","Carb. MIN","Carb. MAX","Carb.","Fibre","Ad. Sugars","Fat MIN","Fat MAX","Fat","SFA",
                "Vit. A","Vit. B1", "Vit. B2", "Vit. B3","Vit. B6","Vit. B9","Vit. B12","Vit. C", "Vit. D",
                "Ca","Iron","Mg","Sodium","Zinc"
)

nut_labels <- setNames(nut_truenames,nut_colnames) 
macro_nut=nut_colnames[2:14] #on inclut pas la reco énergie fixe --> utilisée pour DAILY
vit_nut=nut_colnames[15:23]
min_nut=nut_colnames[24:28]
macro_nutDAILY=nut_colnames[29:42]
vit_nutDAILY=nut_colnames[43:51]
min_nutDAILY=nut_colnames[52:56]

#Ajout du suffixe "DAILY" aux nutriments sélectionnés (car plusieurs échelles : pct et pctDIALY)
selected_nutDAILY=paste0(gsub("KCAL","",selected_nut),"DAILY")
selected_nut=c(selected_nut,selected_nutDAILY)


if(compare==FALSE){
  compo_dej=list(compo_dej)
}

data_dej=list() 

compare_nb=length(compo_dej)

# Boucle de formatage des tableaux   
for (i in 1:compare_nb){
 
if(volume[i]==FALSE){
  compo_dej[[i]]<-compo_dej[[i]]%>%mutate(volume=1)
}

incProgress(amount = 0.1)

# Tableau de données standard pour les graphiques

#Proportion de chaque composante dans l'apport nut
  
cereals_nb=(compo_dej[[i]]%>%pull(Cereals_nb)%>%unique()%>%as.numeric())[1]

if(cereals_nb>=1){
data_cereals= compo_dej[[i]]%>%select(CP_ID,ends_with("_cereals.prop"))%>%
               gather("Nutrient_cereals","cereals_prop",Energy.kcal_cereals.prop:Magnesium.mg_cereals.prop)%>%
               mutate(Nutrient_prefixe=gsub("_cereals.prop","",Nutrient_cereals))%>%
               select(-Nutrient_cereals)
}

components_nb=(compo_dej[[i]]%>%pull(Components_nb)%>%unique()%>%as.numeric())[1]

if (components_nb>=1){
data_compo1=compo_dej[[i]]%>%select(CP_ID,ends_with("_compo1.prop"))%>%
  gather("Nutrient_compo1","compo1_prop",Energy.kcal_compo1.prop:Magnesium.mg_compo1.prop)%>%
  mutate(Nutrient_prefixe=gsub("_compo1.prop","",Nutrient_compo1))%>%
  select(-Nutrient_compo1)
}

if (components_nb>=2){
    data_compo2=compo_dej[[i]]%>%select(CP_ID,ends_with("_compo2.prop"))%>%
    gather("Nutrient_compo2","compo2_prop",Energy.kcal_compo2.prop:Magnesium.mg_compo2.prop)%>%
    mutate(Nutrient_prefixe=gsub("_compo2.prop","",Nutrient_compo2))%>%
    select(-Nutrient_compo2)
}

if (components_nb>=3){
  data_compo3=compo_dej[[i]]%>%select(CP_ID,ends_with("_compo3.prop"))%>%
    gather("Nutrient_compo3","compo3_prop",Energy.kcal_compo3.prop:Magnesium.mg_compo3.prop)%>%
    mutate(Nutrient_prefixe=gsub("_compo3.prop","",Nutrient_compo3))%>%
    select(-Nutrient_compo3)
}

if (components_nb>=4){
  data_compo4=compo_dej[[i]]%>%select(CP_ID,ends_with("_compo4.prop"))%>%
    gather("Nutrient_compo4","compo4_prop",Energy.kcal_compo4.prop:Magnesium.mg_compo4.prop)%>%
    mutate(Nutrient_prefixe=gsub("_compo4.prop","",Nutrient_compo4))%>%
    select(-Nutrient_compo4)
}
if (components_nb>=5){
  data_compo5=compo_dej[[i]]%>%select(CP_ID,ends_with("_compo5.prop"))%>%
    gather("Nutrient_compo5","compo5_prop",Energy.kcal_compo5.prop:Magnesium.mg_compo5.prop)%>%
    mutate(Nutrient_prefixe=gsub("_compo5.prop","",Nutrient_compo5))%>%
    select(-Nutrient_compo5)
}

incProgress(amount = 0.2,"Calculating")

suppressMessages(
 data_dej[[i]]<-compo_dej[[i]]%>%select(CP_ID,nut_colnames[c(-4,-8,-13,-32,-36,-41)],everything())%>%
         gather("Nutrient","Value",nut_colnames[c(-4,-8,-13,-32,-36,-41)])%>%
         filter(Value!="Inf")%>% #enlève les nutriments qui ont une valeur = "Inf" (ceux pour lesquels la reco était de 0%)
         mutate(Nutrient_prefixe=gsub("_dej_lower.pctKCAL|_dej_upper.pctKCAL|_dej_lower.pctDAILY|_dej_upper.pctDAILY|_dej_fixed.pctDAILY","",Nutrient))%>%
         mutate(Nutrient_prefixe=gsub("_dej_lower.pct|_dej_upper.pct|_dej_fixed.pct","",Nutrient_prefixe))%>%
         mutate(Value_new=ifelse(Value>=200,200,Value),Value=round(Value,2))%>%filter(!(is.na(Value_new)))%>%
         mutate(Nutrient_truenames = plyr::mapvalues(Nutrient, nut_colnames[c(-4,-8,-13,-32,-36,-41)], nut_truenames[c(-4,-8,-13,-32,-36,-41)]),
                tooltip_label=ifelse(cereals_nb>=1, paste0(Product_name,"\n",Nutrient_truenames,"\n",Value, "%"),
                                     paste0(Nutrient_truenames,"\n",Value, "%")),
                tooltip_label=gsub("'", " ",tooltip_label))%>% #remplace les ' pour éviter les erreurs
         mutate(order=ifelse(startsWith(Nutrient,"Add_sugars"),5,
                       ifelse(startsWith(Nutrient,"Carbohydrates.g_dej_upper"),3,
                        ifelse(startsWith(Nutrient,"Fat.g_dej_upper"),6,
                         ifelse(startsWith(Nutrient,"Saturated_fat.g_dej_upper"),7,
                          ifelse(startsWith(Nutrient,"Sodium.mg_dej_upper.pct"),4,    
                           ifelse(startsWith(Nutrient,"Energy.kcal_dej"),1,      
                             ifelse(startsWith(Nutrient,"Carbohydrates.g_dej_lower"),3, 
                              ifelse(startsWith(Nutrient,"Fat.g_dej_lower"),6, 
                               ifelse(startsWith(Nutrient,"Proteins.g_dej_lower"),2, 
                                ifelse(startsWith(Nutrient,"Fibre"),4, 
                                 ifelse(startsWith(Nutrient,"Calcium"),1, 
                                  ifelse(startsWith(Nutrient,"Iron"),2, 
                                   ifelse(startsWith(Nutrient,"Magnesium"),3, 
                                    ifelse(startsWith(Nutrient,"Zinc"),5, 
                                     ifelse(startsWith(Nutrient,"Vitamin"),1,0))))))))))))))))%>%
   mutate(Interval.pct=ifelse(Value<25,"5 0-25%",ifelse(Value>=25&Value<50,"4 25-50%", # Découpe les barres en fonction des catégories
                                                           ifelse(Value>=50&Value<75,"3 50-75%",ifelse(Value>=75&Value<100,"2 75-100%",
                                                                                                       ifelse(Value>=100,"1 >=100%",NA))))))%>%
   mutate(Interval.pct2=ifelse(Value>=100,">=100%","< 100%"))%>%
   mutate(Interval.pct3=ifelse(grepl("_lower|_fixed",Nutrient)&Interval.pct2=="< 100%","<MIN", #si inférieur à la reco min
                                      ifelse(grepl("_lower|_fixed",Nutrient)&Interval.pct2==">=100%",">MIN", #si supérieur à la reco min
                                             ifelse(grepl("_upper",Nutrient)&Interval.pct2=="< 100%","<MAX", #si inférieur à la reco max
                                                    ifelse(grepl("_upper",Nutrient)&Interval.pct2==">=100%",">MAX",NA)))))%>% # si supérieur à la reco max
   #Différentiation entre ceux à  l'intérieur et à  l'extérieur des intervalles si 2 reco (Energy, Carb et Fat) --> on choisit un seul nom pour les deux recos (lower et upper)
   mutate(Nutrient_int=ifelse(startsWith(Nutrient,"Energy")&grepl("_fixed",Nutrient)==FALSE,gsub("_lower|_upper","_int",Nutrient),
                              ifelse(startsWith(Nutrient,"Carbohydrates"),gsub("_lower|_upper","_int",Nutrient),
                                     ifelse(startsWith(Nutrient,"Fat"),gsub("_lower|_upper","_int",Nutrient),Nutrient))))
)

#Ajout d'une 3ème cat pour Energy; Carb et Fat : MIIDLE (>MIN ET <MAX)
 data_dej_int=data_dej[[i]]%>%filter(grepl("_int",Nutrient_int))%>%select(CP_ID,#Age,
                                                                          Nutrient,Nutrient_int,Interval.pct3)%>%
              mutate(interval=ifelse(grepl("_lower",Nutrient),"lower",
                                     ifelse(grepl("_upper",Nutrient),"upper",NA)))%>%
              select(-Nutrient)%>%spread(interval,Interval.pct3)%>%
              mutate(Interval.pct4=ifelse(lower=="<MIN"&upper=="<MAX","<MIN",
                                             ifelse(lower==">MIN"&upper==">MAX",">MAX",
                                                    ifelse(lower==">MIN"&upper=="<MAX","MIDDLE",NA))))%>%select(-lower,-upper)
 
# Merge à base de donnée complète et remplace catégorisation des intervalles si 2 recos
 data_dej[[i]]=left_join(data_dej[[i]],data_dej_int,by=c("CP_ID", #"Age",
                                                         "Nutrient_int"))%>%
          mutate(Interval.pct4=ifelse(!(is.na(Interval.pct4)),Interval.pct4,Interval.pct3))

data_dej[[i]]$tooltip_label=iconv(data_dej[[i]]$tooltip_label,from="LATIN1",to="UTF-8")


# Joint les proportions de chaque composante par nutriment

if (cereals_nb>=1){
data_dej[[i]]= data_dej[[i]]%>%left_join(data_cereals,by = c("CP_ID","Nutrient_prefixe"))
}

if (components_nb>=1){
  Components_name=NULL
for(j in 1:components_nb){
  df=eval(parse(text = paste0("data_compo",j))) #prend la valeur de la variable nommée data_compoj
  data_dej[[i]]=data_dej[[i]]%>%left_join(df,by = c("CP_ID","Nutrient_prefixe"))
  Name=(compo_dej[[i]]%>%pull(paste0("compo",j,"_name"))%>%unique()%>%as.character())[1]
  Components_name=c(Components_name,Name)
}}

data_dej[[i]]<-data_dej[[i]]%>%filter(Nutrient%in%selected_nut)

}

############################################## GRAPHIQUES EN POINT #####################################################################

#Graphique 1 : Nutriment des petits dej en % d'adéquation de la reccomandation :

graph1<- ggplot(data_dej[[1]]%>%filter(endsWith(Nutrient,"DAILY")==FALSE&Nutrient!="Energy.kcal_dej_fixed.pct"),
                aes(x=CP_ID,y=Value_new))+
          geom_point_interactive(aes(tooltip=tooltip_label,data_id=Nutrient,colour=Nutrient,shape=Nutrient),size=2.5)+ 
          scale_shape_manual(values=c(16,seq(15,20),seq(15,20),seq(15,20),seq(15,19)),labels=nut_labels)+ #paramètre les formes : https://github.com/davidgohel/ggiraph/issues/31
          scale_colour_manual(values=rep(c("#FF7F50", "#1E90FF", "#EEEE00", "#FF3030", "#228B22", "#9A32CD", 
                                       "#FF6EB4", "#080808", "#8B4513", "#CCCCCC", "#104E8B", "#8EE5EE", 
                                       "#8B7500", "#00868B", "#68228B", "#32CD32", "#8B1A1A", "#EED8AE",
                                       "#EEAD0E", "#EE30A7"),2),labels = nut_labels)+
         ylab("Nutrient Intake (% of breakfast recommendations)")+xlab("Breakfasts")+
         scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
         theme(legend.position = "top",
               axis.text.x = element_text(face="bold", size=7,angle=90),
               axis.text.y = element_text(face="bold", size=9),
               panel.background = element_rect(fill = "white", colour = "grey50"),
               legend.key = element_rect(fill = "white", colour = "black"))+
            geom_hline(yintercept = 100)



#################################################### GRAPHIQUES EN BARRE ####################################################################


#### Graphiques Prévalence : ####

#Nombre de petits déjeuners au dessus et en dessous des recos (ou au milieu)


# EX de plots avec comparaison sur le même graphique 

# data_dej[[1]]=data_dej[[1]]%>%mutate(compare="1")
# data_dej[[2]]=data_dej[[2]]%>%mutate(compare="2")
# data_bar2=bind_rows(data_dej[[1]],data_dej[[2]])%>%
# filter(!(grepl("_int",Nutrient_int)&grepl("upper",Nutrient)))%>% #supprime les nutriments comptés 2 fois (lower et upper de fat, Energy et Carb) --> on enlève les upper
#   group_by(Age,Nutrient_int,compare,Interval.pct4)%>%
#   #   #Calcul du % pondéré avec les volumes 
#   dplyr::summarise(count=n(),vol=sum(volume))%>%mutate(perc=count/sum(count),perc_vol=vol/sum(vol))%>%ungroup()
# # #compte aussi les nut qui ont 0 breakfast dans une intervale (complète toutes les combinaisons possibles)
# data_bar2$Interval.pct4=as.factor(data_bar2$Interval.pct4)
# 
# data_bar2=data_bar2%>%#tidyr::complete(nesting(Nutrient_int,order),Interval.pct4, fill = list(perc=0,perc_vol=0))%>%
#   mutate(Interval.pct4_lab=ifelse(Interval.pct4%in%c("MIDDLE","<MAX",">MIN"),"Meet",
#                                   ifelse(Interval.pct4==">MAX","Above maximum amount",
#                                          ifelse(Interval.pct4=="<MIN","Under minimum amount",NA))))
# 
# data_bar2$Nutrient_int <- factor(data_bar2$Nutrient_int, levels =unique(data_bar2$Nutrient_int[order(data_bar2$order)]))


# ggplot(data_bar2%>%filter(Nutrient_int%in%macro_nut),aes(x=compare,y = perc_vol*100))+
#   geom_bar_interactive(stat="identity",width=0.7,aes(fill=factor(Interval.pct4_lab,levels=c("Above maximum amount","Meet","Under minimum amount")),
#                                                      tooltip=paste(round(perc_vol*100,2),"% \n Click for details !"),
#                                                      data_id=paste(Nutrient_int,Interval.pct4,sep=",")))+
#   labs(y = ylab2,x=NULL)+
#   scale_fill_manual(name="",
#                     values = c("Meet"="#BCEE68","Above maximum amount"="#EE2C2C","Under minimum amount"="#8B1A1A"))+
#   scale_x_discrete(labels = nut_labels)+
#   theme(axis.text.x = element_text(face="bold", size=8),
#         axis.text.y = element_text(face="bold", size=10),
#         panel.background = element_rect(fill = "white", colour = "grey50"),
#         legend.key = element_rect(fill = "white", colour = "black"),
#         legend.position = "top")+ facet_wrap(~Nutrient_int,nrow=1,labeller = as_labeller(nut_labels))

# ggplot(data_bar2%>%filter(Nutrient_int%in%macro_nut),aes(x=Nutrient_int,y = perc_vol*100))+
#   geom_bar_interactive(stat="identity",width=0.8,size=1,position = position_dodge(width = 0.9),aes(fill=factor(Interval.pct4_lab,levels=c("Above maximum amount","Meet","Under minimum amount")),
#                                                                                                    group=compare,color=compare,tooltip=paste(round(perc_vol*100,2),"% \n Click for details !"),
#                                                                                                    data_id=paste(Nutrient_int,Interval.pct4,sep=",")))+
#   labs(y = ylab2,x=NULL)+
#   scale_fill_manual(name="",
#                     values = c("Meet"="#BCEE68","Above maximum amount"="#EE2C2C","Under minimum amount"="#8B1A1A"))+
#   scale_x_discrete(labels = nut_labels)+
#   theme(axis.text.x = element_text(face="bold", size=8),
#         axis.text.y = element_text(face="bold", size=10),
#         panel.background = element_rect(fill = "white", colour = "grey50"),
#         legend.key = element_rect(fill = "white", colour = "black"),
#         legend.position = "top")+scale_colour_manual(name="Set",values=c("Set 1"="black","Set 2"="#FF7F50"))

#if (compare==FALSE){

data_bar2=data_dej[[1]]%>%
  filter(!(grepl("_int",Nutrient_int)&grepl("upper",Nutrient)))%>% #supprime les nutriments comptés 2 fois (lower et upper de Fat, Energy et Carb) --> on enlève les upper
  group_by(#Age,
           Nutrient_int,order,Interval.pct4)%>%
  #   #Calcul du % pondéré avec les volumes 
     dplyr::summarise(count=n(),vol=sum(volume))%>%mutate(perc=count/sum(count),perc_vol=vol/sum(vol))%>%ungroup()
  # #compte aussi les nut qui ont 0 breakfast dans une intervale (complète toutes les combinaisons possibles)
   data_bar2$Interval.pct4=as.factor(data_bar2$Interval.pct4)
   
data_bar2=data_bar2%>%tidyr::complete(nesting(Nutrient_int,order),Interval.pct4, fill = list(perc=0,perc_vol=0))%>%
          mutate(Interval.pct4_lab=ifelse(Interval.pct4%in%c("MIDDLE","<MAX",">MIN"),"Meet",
                                          ifelse(Interval.pct4==">MAX","Above maximum amount",
                                                 ifelse(Interval.pct4=="<MIN","Under minimum amount",NA))))
 
data_bar2$Nutrient_int <- factor(data_bar2$Nutrient_int, levels =unique(data_bar2$Nutrient_int[order(data_bar2$order)]))


if(volume[1]==TRUE){
  ylab2="Weighted percentage of breakfasts"
}else{
  ylab2="Percentage of breakfasts"
}


#MACRONUTRIMENTS
graph_barMACRO<-ggplot(data_bar2%>%filter(Nutrient_int%in%macro_nut),aes(x=Nutrient_int,y = perc_vol*100))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Interval.pct4_lab,levels=c("Above maximum amount","Meet","Under minimum amount")),
                                           tooltip=paste(round(perc_vol*100,2),"% \n Click for details !"),
                                           data_id=paste(Nutrient_int,Interval.pct4,sep=",")))+
  labs(y = ylab2,x=NULL)+
  scale_fill_manual(name="",
                    values = c("Meet"="#BCEE68","Above maximum amount"="#EE2C2C","Under minimum amount"="#8B1C62"))+
  scale_x_discrete(labels = nut_labels)+
  theme(axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")


#VITAMINES

graph_barVIT<-ggplot(data_bar2%>%filter(Nutrient_int%in%vit_nut),aes(x=Nutrient_int,y = perc_vol*100))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Interval.pct4_lab,levels=c("Above maximum amount","Meet","Under minimum amount")),
                                           tooltip=paste(round(perc_vol*100,2),"% \n Click for details !"),
                                           data_id=paste(Nutrient_int,Interval.pct4,sep=",")))+
  labs(y = ylab2,x=NULL)+
  scale_fill_manual(name="",
                    values = c("Meet"="#BCEE68","Above maximum amount"="#EE2C2C","Under minimum amount"="#8B1C62"))+
  scale_x_discrete(labels = nut_labels)+
  theme(axis.text.x = element_text(face="bold", size=8),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")

#MINERAUX
graph_barMIN<-ggplot(data_bar2%>%filter(Nutrient_int%in%min_nut),aes(x=Nutrient_int,y = perc_vol*100))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Interval.pct4_lab,levels=c("Above maximum amount","Meet","Under minimum amount")),
                                           tooltip=paste(round(perc_vol*100,2),"% \n Click for details !"),
                                           data_id=paste(Nutrient_int,Interval.pct4,sep=",")))+
  labs(y = ylab2,x=NULL)+
  scale_fill_manual(name="",
                    values = c("Meet"="#BCEE68","Above maximum amount"="#EE2C2C","Under minimum amount"="#8B1C62"))+
  scale_x_discrete(labels = nut_labels)+
  theme(axis.text.x = element_text(face="bold", size=8),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")

incProgress(amount = 0.6)

#}

# Graphiques MOYENS :  ####
#% moyens des apports pour reco lower, avec barres séparées en fonction de chaque composante du petit dej 

# data_mean=bind_rows(data_dej[[1]],data_dej[[2]])%>%
#   group_by(Age,Nutrient,order,compare)%>%
#   dplyr::summarise(Mean.pct_vol=weighted.mean(Value,volume),
#             Cereals=weighted.mean(Value*cereals_prop,volume),
#             Milk=weighted.mean(Value*milk_prop,volume),
#             Fruit=weighted.mean(Value*fruit_prop,volume))%>%
#   gather("Component","Mean.pct_compo", Cereals: Fruit)%>%
#   mutate(Mean_tooltip=paste(Component,"\n",round(Mean.pct_compo,2),"%"))%>%
#   # position du label de la valeur en y
#   plyr::ddply(c("Nutrient","compare"),transform, label_ypos=cumsum(Mean.pct_compo))%>%
#   mutate(numb_label=ifelse(Mean.pct_compo!=0,round(Mean.pct_compo,2),NA))
# 
# #avec facet
# ggplot(data_mean%>%filter(Nutrient%in%macro_nut),aes(x=compare,y = Mean.pct_compo))+
#   geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Almond","Fruit","Milk","Cereals")),
#                                                      tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
#   labs(y = ylab3,x=NULL)+
#   scale_x_discrete(labels = nut_labels)+
#   theme(legend.position = "top",
#         axis.text.x = element_text(face="bold", size=8,angle=20),
#         axis.text.y = element_text(face="bold", size=10),
#         panel.background = element_rect(fill = "white", colour = "grey50"),
#         legend.key = element_rect(fill = "white", colour = "black"))+
#   geom_hline(yintercept = 100)+
#   scale_fill_manual(name="Breakfast Components",
#                     values=c("Almond"="#F08080","Fruit"="#B4EEB4","Milk"="#6CA6CD","Cereals"="#FFC125"))+ facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
#   geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)

if(compare==FALSE){
 
  data_mean=data_dej[[1]]%>%group_by(Nutrient,order)%>%
    dplyr::summarise(Mean.pct_vol=weighted.mean(Value,volume),
                     Cereals=ifelse(cereals_nb>=1,
                                    weighted.mean(Value*cereals_prop,volume),0),
                     Compo1=ifelse(components_nb>=1,
                                   weighted.mean(Value*compo1_prop,volume),0),
                     Compo2=ifelse(components_nb>=2,
                                   weighted.mean(Value*compo2_prop,volume),0),
                     Compo3=ifelse(components_nb>=3,
                                   weighted.mean(Value*compo3_prop,volume),0),
                     Compo4=ifelse(components_nb>=4,
                                   weighted.mean(Value*compo4_prop,volume),0),
                     Compo5=ifelse(components_nb>=5,
                                   weighted.mean(Value*compo5_prop,volume),0))%>%
    gather("Component","Mean.pct_compo", Cereals: Compo5)%>%
    mutate(Mean_tooltip=ifelse(Component=="Compo1"&components_nb>=1,
                        paste(Components_name[1],"\n",round(Mean.pct_compo,2),"%"),
                                     ifelse(Component=="Compo2"&components_nb>=2,
                                     paste(Components_name[2],"\n",round(Mean.pct_compo,2),"%"),
                                           ifelse(Component=="Compo3"&components_nb>=3,
                                           paste(Components_name[3],"\n",round(Mean.pct_compo,2),"%"),
                                                 ifelse(Component=="Compo4"&components_nb>=4,
                                                 paste(Components_name[4],"\n",round(Mean.pct_compo,2),"%"),
                                                       ifelse(Component=="Compo5"&components_nb>=5,
                                                       paste(Components_name[5],"\n",round(Mean.pct_compo,2),"%"),
                                                         ifelse(Component=="Cereals"&cereals_nb>=1,
                                                                paste("Cereals \n",round(Mean.pct_compo,2),"%"),NA))))))
           )%>%
    # position du label de la valeur en y
    plyr::ddply("Nutrient",transform, label_ypos=cumsum(Mean.pct_compo))%>%
    #Pas d'affichage de la valeur si part de la composante dans la valeur nut = 0
    mutate(Mean.pct_compo=ifelse(Mean.pct_compo!=0,Mean.pct_compo,NA),
           label_ypos=ifelse(Mean.pct_compo!=0,label_ypos,NA),
           numb_label=ifelse(Mean.pct_compo!=0,round(Mean.pct_compo,2),NA))%>%ungroup()
  
}else{
  
  data_bind=NULL
  
  for (i in 1:compare_nb){
  data_dej[[i]]=data_dej[[i]]%>%mutate(compare=as.character(i))
  data_bind=bind_rows(data_bind,data_dej[[i]])
  }

  data_mean=data_bind%>%group_by(Nutrient,order,compare)%>%
    summarise(Mean.pct_vol=weighted.mean(Value,volume))%>%
    # position du label de la valeur en y
    mutate(Mean_tooltip =ifelse(Mean.pct_vol!=0,paste(round(Mean.pct_vol,2),"%"),NA),
           label_ypos=ifelse(Mean.pct_vol!=0,Mean.pct_vol,NA),
           numb_label=ifelse(Mean.pct_vol!=0,round(Mean.pct_vol,2),NA))%>%ungroup()
  
}

#remet les levels dans l'ordre
data_mean$Nutrient <- factor(data_mean$Nutrient, levels =unique(data_mean$Nutrient[order(data_mean$order)]))


# % BREAKFAST RECOMMANDATION ####

if(compare==FALSE){
 
if(volume[1]==TRUE){
  ylab3="Weighted mean by volume (% of breakfast recommendations)"
}else{
    ylab3="Mean (% of breakfast recommendations)"
}
  
#Fixation d'un maximum en ordonnées en fonction du maximum des données
  if(!is.null(max_ordinate)){
    ymax_macro=ifelse(max_ordinate[1]>50,ceiling(max_ordinate[1]),50)#arrondi à l'entier supérieur
    ymax_vit=ifelse(max_ordinate[2]>50,ceiling(max_ordinate[2]),50)
    ymax_min=ifelse(max_ordinate[3]>50,ceiling(max_ordinate[3]),50)
  } 
  
#ordre des noms dans label change en fonction du nombre de composantes du petit dej (l'un au dessus de l'autre)

if(cereals_nb==0){
cerealab=NULL
}else{
cerealab="Cereals"
}

if(components_nb==0){
compolab=cerealab
}
if(components_nb==1){
  compolab=c(stringr::str_trunc(Components_name[1],15),cerealab)
}
if(components_nb==2){
  compolab=c(stringr::str_trunc(Components_name[c(2,1)],15),cerealab)
}
if(components_nb==3){
  compolab=c(stringr::str_trunc(Components_name[c(3,2,1)],15),cerealab)
}
if(components_nb==4){
  compolab=c(stringr::str_trunc(Components_name[c(4,3,2,1)],15),cerealab)
}
if(components_nb==5){
  compolab=c(stringr::str_trunc(Components_name[c(5,4,3,2,1)],15),cerealab)
}

#MACRONUTRIMENTS
    
graph_meanMACRO<-ggplot(data_mean%>%filter(Nutrient%in%macro_nut),aes(x=Nutrient,y = Mean.pct_compo))+
            geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
            scale_x_discrete(labels = nut_labels,name="")+
            scale_y_continuous(name=ylab3, limits=c(0, ymax_macro))+
              theme(legend.position = "top",
                  axis.text.x = element_text(face="bold", size=8,angle=20),
                  axis.text.y = element_text(face="bold", size=10),
                  panel.background = element_rect(fill = "white", colour = "grey50"),
                  legend.key.size = unit(0.5, "cm"))+
            geom_hline(yintercept = 100)+
            scale_fill_manual(name=NULL,label=compolab,
                              values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                                       "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))#+
          #  geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)

#VITAMINES

graph_meanVIT<-ggplot(data_mean%>%filter(Nutrient%in%vit_nut),aes(x=Nutrient,y = Mean.pct_compo))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
  scale_y_continuous(name=ylab3, limits=c(0, ymax_vit))+
  scale_x_discrete(labels = nut_labels,name="")+
  theme(legend.position = "top",
        axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"))+
  geom_hline(yintercept = 100)+
  scale_fill_manual(name=NULL,label=compolab,
                    values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                             "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))#+
#  geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)


#MINERAUX

graph_meanMIN<-ggplot(data_mean%>%filter(Nutrient%in%min_nut),aes(x=Nutrient,y = Mean.pct_compo))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
  scale_y_continuous(name=ylab3, limits=c(0, ymax_min))+
  scale_x_discrete(labels = nut_labels,name="")+
  theme(legend.position = "top",
        axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"))+
  geom_hline(yintercept = 100)+
  scale_fill_manual(name="Breakfast Components",label=compolab,
                    values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                             "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))#+
 # geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)


incProgress(amount = 0.7)

}else{
  
  
  
ylab3="Mean (% of breakfast recommendations)" 

ymax_macro=ifelse(max(data_mean%>%filter(Nutrient%in%macro_nut)%>%pull(Mean.pct_vol))>50,
                        ceiling(max(data_mean%>%filter(Nutrient%in%macro_nut)%>%pull(Mean.pct_vol))),50)
ymax_vit=ifelse(max(data_mean%>%filter(Nutrient%in%vit_nut)%>%pull(Mean.pct_vol))>50,
                      ceiling(max(data_mean%>%filter(Nutrient%in%vit_nut)%>%pull(Mean.pct_vol))),50)
ymax_min=ifelse(max(data_mean%>%filter(Nutrient%in%min_nut)%>%pull(Mean.pct_vol))>50,
                      ceiling(max(data_mean%>%filter(Nutrient%in%min_nut)%>%pull(Mean.pct_vol))),50)
  
#MACRONUTRIMENTS (comparaison) 

graph_meanMACRO<- ggplot(data_mean%>%filter(Nutrient%in%macro_nut),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
    scale_y_continuous(name=ylab3, limits=c(0, ymax_macro))+
    scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
   # geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
    guides(fill=FALSE)# elève légende de couleur
 
#VITAMINES (comparaison)
 
graph_meanVIT<- ggplot(data_mean%>%filter(Nutrient%in%vit_nut),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
    scale_y_continuous(name=ylab3, limits=c(0, ymax_vit))+
    scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
   # geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
    guides(fill=FALSE)# elève légende de couleur
  
#MINERAUX (comparaison)
  
graph_meanMIN<- ggplot(data_mean%>%filter(Nutrient%in%min_nut),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
    scale_y_continuous(name=ylab3, limits=c(0, ymax_min))+
    scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
 #   geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
    guides(fill=FALSE)# elève légende de couleur
  
}





# % DAILY RECOMMANDATION ####

#data pour tracer les points (recommandation du petit déjeuner en % de la reco daily)
selected_nut.pt=paste0(gsub(".pctDAILY","",selected_nutDAILY),".pt")

if (compare==FALSE){

data_pt=data_dej[[1]][, names(data_dej[[1]]) %in% selected_nut.pt]%>% # contrairement à select, si nom de colonne n'existe pas, pas d'erreur
              gather("Nutrient","ybkf",selected_nut.pt)%>%
              mutate(Nutrient=paste0(gsub(".pt","",Nutrient),".pctDAILY"))%>%
              group_by(Nutrient)%>%summarize(ybkf=mean(ybkf))%>%mutate(ybkf_legend="Breakfast recommendation")
        
suppressWarnings(data_mean<-data_mean%>%left_join(data_pt,by="Nutrient"))

#remet les levels dans l'ordre
data_mean$Nutrient <- factor(data_mean$Nutrient, levels =unique(data_mean$Nutrient[order(data_mean$order)]))


if(volume[1]==TRUE){
  ylab4="Weighted mean by volume (% of daily recommendations)"
}else{
  ylab4="Mean (% of daily recommendations)"
}

#Fixation d'un maximum en ordonnées en fonction du maximum des données
if(!is.null(max_ordinate)){
  ymax_macroDAILY=ifelse(max_ordinate[4]>50,ceiling(max_ordinate[4]),50)
  ymax_vitDAILY=ifelse(max_ordinate[5]>50,ceiling(max_ordinate[5]),50)
  ymax_minDAILY=ifelse(max_ordinate[6]>50,ceiling(max_ordinate[6]),50)
}


#MACRONUTRIMENTS

graph_meanMACRODAILY<-ggplot(data_mean%>%filter(Nutrient%in%macro_nutDAILY),aes(x=Nutrient,y = Mean.pct_compo))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
  scale_y_continuous(name=ylab4, limits=c(0, ymax_macroDAILY))+
  scale_x_discrete(labels = nut_labels,name="")+
  theme(legend.position = "top",
        axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"))+
  geom_hline(yintercept = 100)+
  scale_fill_manual(name=NULL,label=compolab,
                    values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                             "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))+
 # geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
 #Point représentant le % de reco de petit dej par rapport à la reco daily
  geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
  scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))


incProgress(amount = 0.7)

#VITAMINES

graph_meanVITDAILY<-ggplot(data_mean%>%filter(Nutrient%in%vit_nutDAILY),aes(x=Nutrient,y = Mean.pct_compo))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
  scale_y_continuous(name=ylab4, limits=c(0, ymax_vitDAILY))+
  scale_x_discrete(labels = nut_labels,name="")+
  theme(legend.position = "top",
        axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"))+
  geom_hline(yintercept = 100)+
  scale_fill_manual(name= NULL,label=compolab,
                    values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                             "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))+
#  geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
  #Point représentant le % de reco de petit dej par rapport à la reco daily
  geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
  scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))


#MINERAUX

graph_meanMINDAILY<-ggplot(data_mean%>%filter(Nutrient%in%min_nutDAILY),aes(x=Nutrient,y = Mean.pct_compo))+
  geom_bar_interactive(stat="identity",width=0.8,aes(fill=factor(Component,levels=c("Compo5","Compo4","Compo3","Compo2","Compo1","Cereals")),
                                                     tooltip=Mean_tooltip,data_id=paste(Nutrient,Component)))+
  scale_y_continuous(name=ylab4, limits=c(0, ymax_minDAILY))+
  scale_x_discrete(labels = nut_labels,name="")+
  theme(legend.position = "top",
        axis.text.x = element_text(face="bold", size=8,angle=20),
        axis.text.y = element_text(face="bold", size=10),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key.size = unit(0.5, "cm"))+
  geom_hline(yintercept = 100)+
  scale_fill_manual(name= NULL,label=compolab,
                    values=c("Compo5"="darkturquoise","Compo4"="thistle3","Compo3"="lightcoral",
                             "Compo2"="darkseagreen2","Compo1"="skyblue3","Cereals"="goldenrod1"))+
#  geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
  #Point représentant le % de reco de petit dej par rapport à la reco daily
  geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
  scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))


}else{
  
  data_pt=data_bind[, names(data_bind) %in% selected_nut.pt]%>% # contrairement à select, si nom de colonne n'existe pas, pas d'erreur
    gather("Nutrient","ybkf",selected_nut.pt)%>%
    mutate(Nutrient=paste0(gsub(".pt","",Nutrient),".pctDAILY"))%>%
    group_by(Nutrient)%>%summarize(ybkf=mean(ybkf))%>%mutate(ybkf_legend="Breakfast recommendation")
  
  suppressWarnings(data_mean<-data_mean%>%left_join(data_pt,by="Nutrient"))
  
  #remet les levels dans l'ordre
  data_mean$Nutrient <- factor(data_mean$Nutrient, levels =unique(data_mean$Nutrient[order(data_mean$order)]))
  
  ylab4="Mean (% of daily recommendations)"
  
  ymax_macroDAILY=ifelse(max(data_mean%>%filter(Nutrient%in%macro_nutDAILY)%>%pull(Mean.pct_vol))>50,
                             ceiling(max(data_mean%>%filter(Nutrient%in%macro_nutDAILY)%>%pull(Mean.pct_vol))),50)
  ymax_vitDAILY=ifelse(max(data_mean%>%filter(Nutrient%in%vit_nutDAILY)%>%pull(Mean.pct_vol))>50,
                         ceiling(max(data_mean%>%filter(Nutrient%in%vit_nutDAILY)%>%pull(Mean.pct_vol))),50)
  ymax_minDAILY=ifelse(max(data_mean%>%filter(Nutrient%in%min_nutDAILY)%>%pull(Mean.pct_vol))>50,
                         ceiling(max(data_mean%>%filter(Nutrient%in%min_nutDAILY)%>%pull(Mean.pct_vol))),50)

#MACRONUTRIMENTS (comparaison)
  
  graph_meanMACRODAILY<- ggplot(data_mean%>%filter(Nutrient%in%macro_nutDAILY),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
    scale_y_continuous(name=ylab4, limits=c(0, ymax_macroDAILY))+
    scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(legend.position = "top",
          axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
 #   geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
    #Point représentant le % de reco de petit dej par rapport à la reco daily
    geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
    scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))+
    guides(fill=FALSE)# elève légende de couleur

#VITAMINES (comparaison)

  graph_meanVITDAILY<- ggplot(data_mean%>%filter(Nutrient%in%vit_nutDAILY),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
    scale_y_continuous(name=ylab4, limits=c(0, ymax_vitDAILY))+
    scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(legend.position = "top",
          axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
   # geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
    #Point représentant le % de reco de petit dej par rapport à la reco daily
    geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
    scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))+
    guides(fill=FALSE)# elève légende de couleur
  
#MINERAUX  (comparaison)
  
graph_meanMINDAILY<- ggplot(data_mean%>%filter(Nutrient%in%min_nutDAILY),aes(fill=compare,x=paste("Set",compare),y = Mean.pct_vol))+
    geom_bar_interactive(stat="identity",width=0.8,aes(tooltip=Mean_tooltip))+
   scale_y_continuous(name=ylab4, limits=c(0, ymax_minDAILY))+
   scale_x_discrete(labels = nut_labels,name="")+
    scale_fill_manual(values=c("deepskyblue2", "slategray2", "springgreen3"))+
    theme(legend.position = "top",
          axis.text.x = element_text(face="bold", size=8,angle=20),
          axis.text.y = element_text(face="bold", size=10),
          panel.background = element_rect(fill = "white", colour = "grey50"))+geom_hline(yintercept = 100)+ 
    facet_wrap(~Nutrient,nrow=1,labeller = as_labeller(nut_labels))+
  #  geom_text(aes(y=label_ypos, label=numb_label), vjust=1.2,color="white", size=3)+
  #Point représentant le % de reco de petit dej par rapport à la reco daily
   geom_point(aes(y=ybkf,colour=ybkf_legend),size=2,show.legend = FALSE)+
   scale_color_manual(name="",values=c("Breakfast recommendation"="orangered"))+
    guides(fill=FALSE)# elève légende de couleur

}

incProgress(amount = 0.8)



####################################### Graphiques à part pour Gab (non visibles dans l'appli) #####################################

# data_bonus_vol1=data_bar%>%
#                 filter((Interval.pct=="1 >=100%"&endsWith(Nutrient,"_lower.pct"))|
#                         (Interval.pct=="1 >=100%"&endsWith(Nutrient,"_lower.pctKCAL")))%>% 
#                 mutate(bar_colour=ifelse(endsWith(Nutrient,"_lower.pct"),"Lower",
#                                     ifelse(endsWith(Nutrient,"_lower.pctKCAL"),"Lower KCAL kcal",NA)))%>%
#                 arrange(order)%>%select(-order)
# 
# graph_bar_bonus1<-ggplot(data_bonus_vol1,aes(x=Nutrient,y = perc_vol*100))+
#                   geom_bar_interactive(width=0.8,stat="identity",colour="black",
#                             aes(fill=bar_colour,tooltip=paste(round(perc_vol*100,2),"%"),data_id=Nutrient))+
#                   labs(y = " Weighted percentage of breakfasts by volume above recommendations")+
#                   scale_x_discrete(labels = nut_labels,limits = data_bonus_vol1$Nutrient,drop=FALSE)+
#                   scale_fill_manual(breaks = c( "Lower KCAL kcal","Lower"),values=c("#66CD00", "#006400"))+
#                   theme(axis.text.x = element_text(face="bold", size=8,angle=30),
#                         axis.text.y = element_text(face="bold", size=9),
#                         panel.background = element_rect(fill = "white", colour = "grey50"),
#                         legend.key = element_rect(fill = "white", colour = "black"))+
#                   geom_hline(yintercept = 100)+
#                   guides(fill=FALSE)
# 
# data_bonus_vol2=data_bar%>%
#                 filter((Interval.pct!="1 >=100%"&endsWith(Nutrient,"_upper.pct"))|
#                        (Interval.pct!="1 >=100%"&endsWith(Nutrient,"_upper.pctKCAL")))%>% 
#                 group_by(Nutrient,order)%>%dplyr::summarise(count=sum(count),vol=sum(vol),perc=sum(perc),perc_vol=sum(perc_vol))%>%
#                 mutate(bar_colour=ifelse(endsWith(Nutrient,"_upper.pct"),"Upper",
#                                    ifelse(endsWith(Nutrient,"_upper.pctKCAL"),"Upper KCAL kcal",NA)))%>%
#                 arrange(order)%>%select(-order)
#            #%>%filter(startsWith(Nutrient,"Sugars.g")==FALSE)
# 
# graph_bar_bonus2<-ggplot(data_bonus_vol2,aes(x=Nutrient,y = perc_vol*100))+
#                         geom_bar_interactive(width=0.8,stat="identity",colour="black",
#                                              aes(fill=bar_colour,tooltip=paste(round(perc_vol*100,2),"%"),data_id=Nutrient))+
#                   labs(y = " Weighted percentage of breakfasts by volume under recommendation")+
#                   scale_x_discrete(labels = nut_labels,limits = data_bonus_vol2$Nutrient,drop=FALSE)+
#                   scale_fill_manual(breaks = c("Upper KCAL kcal","Upper"),values=c("#8B1A1A","#FF3030"))+
#                   theme(axis.text.x = element_text(face="bold", size=8,angle=30),
#                         axis.text.y = element_text(face="bold", size=9),
#                         panel.background = element_rect(fill = "white", colour = "grey50"),
#                         legend.key = element_rect(fill = "white", colour = "black"))+
#                   geom_hline(yintercept = 100)+
#                   guides(fill=FALSE)
# 
# 
 incProgress(amount = 0.9,"Done")

#data à exporter pour la sélection de breakfasts à afficher sur le graphique des points


if (compare==FALSE){
  
  data_mean1=data_mean%>%filter(Nutrient%in%macro_nut)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  data_mean2=data_mean%>%filter(Nutrient%in%vit_nut)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  data_mean3=data_mean%>%filter(Nutrient%in%min_nut)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  data_mean_daily1=data_mean%>%filter(Nutrient%in%macro_nutDAILY)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  data_mean_daily2=data_mean%>%filter(Nutrient%in%vit_nutDAILY)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  data_mean_daily3=data_mean%>%filter(Nutrient%in%min_nutDAILY)%>%select(Nutrient,Mean.pct_vol,Component,Mean.pct_compo)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol,"Average percentage by component"=Mean.pct_compo)
  
}else{
 
 data_mean1=data_mean%>%filter(Nutrient%in%macro_nut)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)
  data_mean2=data_mean%>%filter(Nutrient%in%vit_nut)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)
  data_mean3=data_mean%>%filter(Nutrient%in%min_nut)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)
  data_mean_daily1=data_mean%>%filter(Nutrient%in%macro_nutDAILY)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)
  data_mean_daily2=data_mean%>%filter(Nutrient%in%vit_nutDAILY)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)
  data_mean_daily3=data_mean%>%filter(Nutrient%in%min_nutDAILY)%>%select(Nutrient,Mean.pct_vol)%>%
    dplyr::rename("Average percentage"=Mean.pct_vol)

}

if(cereals_nb==0){
data_points=data_dej[[1]]%>%mutate(Product_name="Breakfast")%>%
                           select(CP_ID,Product_name,Nutrient,Nutrient_int,order,Value,Interval.pct3,Interval.pct4)
}else{
data_points=data_dej[[1]]%>%select(CP_ID,Product_name,Nutrient,Nutrient_int,order,Value,Interval.pct3,Interval.pct4)
}


return(list(points=graph1,
            preval1=graph_barMACRO,
            preval2=graph_barVIT,
            preval3=graph_barMIN,
            mean1=graph_meanMACRO,
            mean2=graph_meanVIT, 
            mean3=graph_meanMIN,
            mean_daily1=graph_meanMACRODAILY,
            mean_daily2=graph_meanVITDAILY,
            mean_daily3=graph_meanMINDAILY,
            #Tableaux de données 
            data_points=data_points,
            data_preval1=data_bar2%>%filter(Nutrient_int%in%macro_nut)%>%select(Nutrient_int,perc_vol,Interval.pct4)%>%
                                      dplyr::rename(Nutrient=Nutrient_int,"Percentage"=perc_vol,"Class"=Interval.pct4),
            data_preval2=data_bar2%>%filter(Nutrient_int%in%vit_nut)%>%select(Nutrient_int,perc_vol,Interval.pct4)%>%
                                     dplyr::rename(Nutrient=Nutrient_int,"Percentage"=perc_vol,"Class"=Interval.pct4),
            data_preval3=data_bar2%>%filter(Nutrient_int%in%min_nut)%>%select(Nutrient_int,perc_vol,Interval.pct4)%>%
                                     dplyr::rename(Nutrient=Nutrient_int,"Percentage"=perc_vol,"Class"=Interval.pct4),
            data_mean1=data_mean1,
            data_mean2=data_mean2,
            data_mean3=data_mean3,
            data_mean_daily1=data_mean_daily1,
            data_mean_daily2=data_mean_daily2,
            data_mean_daily3=data_mean_daily3
            ))  

})

}


plot_points<-function(data_points,graph.type=c("lower","upper")){
  
############## Refait le graphique des points #######################################################################
  
  nut_colnames=c("Energy.kcal_dej_lower.pct","Energy.kcal_dej_upper.pct","Energy.kcal_dej_int.pct","Proteins.g_dej_lower.pct", 
                    "Carbohydrates.g_dej_lower.pctKCAL","Carbohydrates.g_dej_upper.pctKCAL","Carbohydrates.g_dej_int.pctKCAL","Fibre.g_dej_lower.pct","Add_sugars.g_dej_upper.pctKCAL", 
                    "Fat.g_dej_lower.pctKCAL","Fat.g_dej_upper.pctKCAL", "Fat.g_dej_int.pctKCAL","Saturated_fat.g_dej_upper.pctKCAL",
                    "Vitamin_A.mg_dej_lower.pct","Vitamin_B1.mg_dej_lower.pct","Vitamin_B2.mg_dej_lower.pct","Vitamin_B3.mg_dej_lower.pct",
                    "Vitamin_B6.mg_dej_lower.pct","Vitamin_B9.mcg_dej_lower.pct","Vitamin_B12.mcg_dej_lower.pct","Vitamin_C.mg_dej_lower.pct","Vitamin_D.mcg_dej_lower.pct",
                    "Calcium.mg_dej_lower.pct","Iron.mg_dej_lower.pct","Magnesium.mg_dej_lower.pct","Sodium.mg_dej_upper.pct","Zinc.mg_dej_lower.pct")
  
  nut_truenames=c("Energy MIN","Energy MAX","Energy","Protein","Carb. MIN*","Carb. MAX*","Carb.*","Fibre",
                     "Ad. Sugars*","Fat MIN*","Fat MAX*","Fat*","SFA*",
                     "Vit. A","Vit. B1", "Vit. B2", "Vit. B3","Vit. B6","Vit. B9","Vit. B12","Vit. C", "Vit. D",
                     "Ca","Iron","Mg","Sodium","Zinc")
  
  nut_labels <- setNames(nut_truenames,nut_colnames) 
  
  suppressMessages(
  data_points<-data_points%>%  
    mutate(Value_new=ifelse(Value>=200,200,Value),Value=round(Value,2))%>%
    mutate(Nutrient_truenames = plyr::mapvalues(Nutrient, nut_colnames, nut_truenames),
                                   tooltip_label=paste0(Product_name,"\n",Nutrient_truenames,"\n",Value, "%"),
                                   tooltip_label=gsub("'", " ",tooltip_label)) #remplace les ' pour éviter les erreurs
  )

#data_points$Nutrient <- factor(data_points$Nutrient, levels =unique(data_points$Nutrient[order(data_points2$order)]))  
data_points$tooltip_label=iconv(data_points$tooltip_label,from="LATIN1",to="UTF-8")

  
  graph<- ggplot(data_points,
                  aes(x=CP_ID,y=Value_new))+
    geom_point_interactive(aes(tooltip=tooltip_label,data_id=CP_ID,colour=Nutrient,shape=Nutrient),size=2.5)+ 
    scale_shape_manual(values=c(16,seq(15,20),seq(15,20),seq(15,20),seq(15,19)),labels=nut_labels)+ #paramètre les formes : https://github.com/davidgohel/ggiraph/issues/31
    scale_colour_manual(values=c("#FF7F50", "#1E90FF", "#EEEE00", "#FF3030", "#228B22", "#9A32CD", 
                                     "#FF6EB4", "#080808", "#8B4513", "#CCCCCC", "#104E8B", "#8EE5EE", 
                                     "#8B7500", "#00868B", "#68228B", "#32CD32", "#8B1A1A", "#EED8AE",
                                     "#EEAD0E", "#EE30A7"),labels = nut_labels)+
    ylab("Nutritionnal composition (% of breakfast recommendations)")+xlab("Breakfasts")+
    scale_y_continuous(limits=c(0,200),breaks=c(0,20,40,60,80,100,120,140,160,180,200))+
    theme(axis.text.x = element_text(face="bold", size=7,angle=90),
          axis.text.y = element_text(face="bold", size=9),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          legend.key = element_rect(fill = "white", colour = "black"))+
    geom_hline(yintercept = 100)+guides(colour=FALSE,shape=FALSE)

  
  return(graph) 
}


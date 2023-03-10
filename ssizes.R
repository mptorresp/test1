rm(list = ls())
library(pacman)

p_load(tidyverse, samplesize4surveys, readxl)

# Población
poblacion<- read_xlsx("data/Poblacion 2023acr.xlsx")

# Grupos política
poblacionR<- poblacion %>%
  mutate(edades=ifelse(`EDADES SIMPLES`=="100 y más",100,`EDADES SIMPLES`)) %>%
  mutate(edades=as.numeric(edades)) %>% 
  mutate(rangos_polit= case_when(between(edades,18,25)~ "18-25",
                                 between(edades,26,40)~ "26-40",
                                 between(edades,41,55)~ "41-55",
                                         edades>55 ~"56 o más",
                                TRUE ~ "Menores"))
# SETUP
DEFF <-  1.2
mpios<- c(Bello="05088",
         Envigado="05266",
         Girardota="05308",
         Barbosa="05079")


# totales de población #
mpio_pob<- poblacionR %>% filter(DPMP %in% mpios) %>% 
  select(-`EDADES SIMPLES`,-edades) %>% 
  group_by(DPMP,MPIO) %>%
  summarise_if(is.numeric,sum) 

# tamaños de muestra #





### Funciones para distribuir muestra cabecera x sexo## 

####### 
#Escenario 1

funcionSSmpiospol<- function(mpios){

  
pob<- mpio_pob %>% 
  filter(DPMP==mpios) %>% 
  pull(Total)

tabpob<- poblacionR %>% 
  filter(rangos_polit!="Menores",DPMP==mpios) %>% 
  select(`Cabecera Mujeres`,`Cabecera Hombres`,rangos_polit) %>% 
  group_by(rangos_polit) %>% 
  summarise_if(is.numeric,sum)

ss<-ss4p(N=pob, P = 0.5, DEFF = DEFF, error = "me", delta = 0.05) 
a<- e4p(N = pob,n = ss,P = 0.5,DEFF = DEFF,plot = F)  

tabss<- tabpob %>%
  mutate_if(is.numeric, function(x) x^0.8) %>% 
  mutate(t=`Cabecera Mujeres`+`Cabecera Hombres`) %>% 
  mutate_if(is.numeric, function(x) round(ss*(x/sum(.$t)))) %>% 
  select(-t) %>% 
  mutate(Total= rowSums( across(where(is.numeric)))) %>% 
  bind_rows(summarise(., across(where(is.numeric),~sum(.))) %>% 
              mutate(rangos_polit="Total")) %>% 
  mutate(Error= "5.0%")

return(tabss)
  
}


Ssizes1<- list(map_dfr(mpios,funcionSSmpiospol,.id = "mpio"))

names(Ssizes1)<- "Esc_1"

######
## Otros escenarios

funcionSSmpiospol_otros<- function(mpios,ss){
  
  
  pob<- mpio_pob %>% 
    filter(DPMP==mpios) %>% 
    pull(Total)
  
  tabpob<- poblacionR %>% 
    filter(rangos_polit!="Menores",DPMP==mpios) %>% 
    select(`Cabecera Mujeres`,`Cabecera Hombres`,rangos_polit) %>% 
    group_by(rangos_polit) %>% 
    summarise_if(is.numeric,sum)
  

  a<- e4p(N = pob,n = ss,P = 0.5,DEFF = DEFF,plot = F)  
  
  tabss<- tabpob %>%
    mutate_if(is.numeric, function(x) x^0.8) %>% 
    mutate(t=`Cabecera Mujeres`+`Cabecera Hombres`) %>% 
    mutate_if(is.numeric, function(x) round(ss*(x/sum(.$t)))) %>% 
    select(-t) %>% 
    mutate(Total= rowSums( across(where(is.numeric)))) %>% 
    bind_rows(summarise(., across(where(is.numeric),~sum(.))) %>% 
                mutate(rangos_polit="Total")) %>% 
    mutate(Error= paste0(format( round(a$Margin_of_error,digits = 1),nsmall=1),"%"))
  
  return(tabss)
  
}

ss<- c(700,1000) # tamaños otros escenarios

Ssizes2<-list()
for (i in 1:length(ss)) {

Ssizes2[[i]]<- map2_df(mpios,ss[i],funcionSSmpiospol_otros,.id = "mpio")

}

names(Ssizes2)<- paste0(ss)

salida<- c(Ssizes1,Ssizes2)
## excel

writexl::write_xlsx(salida,path = "salidas/tamanos2.xlsx")


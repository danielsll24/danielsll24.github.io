library(tidyverse)
Sys.setlocale("LC_ALL","UTF-8")
datos <- read.csv(file.choose())

fem <- datos %>% 
  filter(Tipo.de.delito == "Feminicidio")
  
  
names(fem)
fem2 <- gather(fem,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
       'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=fem2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Modalidad),rows=vars(Año))+
  labs(title="Feminicidios en México (SESNSP)")+coord_flip()

fem3<- gather(fem,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=fem3,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Feminicidios en México 2 (SESNSP)")+coord_flip()



ab <- datos %>% 
  filter(Tipo.de.delito == "Aborto")
ab2 <- gather(ab,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
               'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=ab2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Abortos en México (SESNSP)")+coord_flip()

viol <- datos %>% 
  filter(Tipo.de.delito == "Violación simple")
viol2 <- gather(viol,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=viol2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Violación simple en México (SESNSP)")+coord_flip()


violgen <- datos %>% 
  filter(Tipo.de.delito == "Violencia de género en todas sus modalidades distinta a la violencia familiar")
violgen2 <- gather(violgen,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=violgen2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Violencia de género distinta a familiar (SESNSP)")+coord_flip()

violfam <- datos %>% 
  filter(Tipo.de.delito == "Violencia familiar")
violfam2 <- gather(violfam,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                   'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=violfam2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Violencia familiar (SESNSP)")+coord_flip()


absex <- datos %>% 
  filter(Tipo.de.delito == "Abuso sexual")
absex2 <- gather(absex,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                 'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=absex2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Abuso sexual (SESNSP)")+coord_flip()

acoso <- datos %>% 
  filter(Tipo.de.delito=="Acoso sexual")
acoso2 <- gather(violfam,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
       'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=acoso2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Acoso sexual (SESNSP)")+coord_flip()


violeq <- datos %>% 
  filter(Tipo.de.delito=="Violación equiparada")
violeq2 <- gather(violeq,'Enero','Febrero','Marzo','Abril','Mayo','Junio',
                  'Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre',key="Mes",value="Casos")
ggplot(data=violeq2,aes(fct_inorder(Mes),Casos))+geom_col(position=position_dodge())+scale_fill_discrete()+
  facet_grid(cols=vars(Clave_Ent),rows=vars(Año))+
  labs(title="Violación equiparada (SESNSP)")+coord_flip()

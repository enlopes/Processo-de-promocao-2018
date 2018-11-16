# Processo-de-promocao-2018
Código utilizado para o cálculo da produtividade dos magistrados

library(lubridate)
library(dplyr)
library(reshape)
library(excel.link)
library(stringr)
library(outliers)

setwd("X:/SGE/GABINETE/AVALIAÇÃO DE PROMOÇÃO DE MAGISTRADOS 2018/Edital 12_2018")
#-----------------------------------ATENÇÃO---------------------------------------------------------------
#Atenção: Sempre ajustar esses parâmetros para coincidir com o relatório do Mentorh que está sendo utilizado
p1<-c(1,2008) #Mês de início dos afastamentos - relatório do Mentorh
p2<-c(9,2018) #Mês do término dos afastamentos - relatório do Mentorh
n<-2018 #Número de registros constantes no relatório extraído do Mentorh


#Atenção: Aqui estão definidas as pontuações máximas que serão atribuídas a cada critério
pont_sent<-5 #Sentenças = 5 pontos
pont_inc<-3  #Incidentes =3 pontos
pont_aud<-1  #Audiências = 1 ponto
pont_conc<-1 #Conciliações = 1 ponto


#-----------------Construção da base de dados dos afastamentos dos magistrados--------------------------------

#Importando a base de dados inicial extraída do MENTORH
#Essa base de dados contempla os afastamentos ocorridos no período de Jan/2008 a Julho/2018

afastamentos<-read.csv("afastamentos_2008_2018_titulares_.csv",h=T,sep=";",stringsAsFactors = FALSE,encoding="UTF-8")
afastamentos<-afastamentos[1:n,1:9]
attach(afastamentos)

#---Desmembrando os registros que precisam ser separados em mais de uma linha. Estes são os períodos que-------
#---iniciam em um determinado mês e finalizam em outro mês. Logo, precisamos calcular o número de dias---------
#---de afastamento para cada mês.------------------------------------------------------------------------------

k<-0
j<-1
afast<-data.frame((matrix(0,1,6)),stringsAsFactors = FALSE)
colnames(afast)=c("Lotação","Juiz","Inicio","Fim","DiasC","Descrição")

for(i in 1:n){
  diainicio<-dmy(Início[i])
  diafim<-dmy(Fim[i])
  mesinicio<-month(diainicio)
  mesfim<-month(diafim)   
  anoinicio<-year(diainicio)
  anofim<-year(diafim)
  
  #Cálculo do GAP de meses
  difanos<-anofim-anoinicio
  if(anoinicio==anofim){
      gap<-mesfim-mesinicio
  }else{
      gap<-(mesfim-mesinicio)+12*difanos
  }
  
  if(gap==0){ #Verifica se o afastamento é dentro do mesmo mês e calcula o número de dias de afastamento
      diascalc<-((day(diafim))-day(diainicio))+1
      afast[j,1:4]<-afastamentos[i,c(1,3,6,7)]
      afast[j,5]<-diascalc
      afast[j,6]<-afastamentos[i,9]
      j<-j+1
  }
  else{
      for(k in 0:gap){
            diascalc<-c(days_in_month(mesinicio)-day(diainicio)+1,day(diafim)) 
            afast[j+k,1:2]<-afastamentos[i,c(1,3)]
            if(k==0){
                #Divide um período de afastamento em mais de um registro no banco de dados e calcula o número
                #de dias de afastamento.
                #Aqui, é trabalhado o caso em que há a inclusão de apenas mais uma linha
                afast[j+k,3]<-paste(day(diainicio),"/",mesinicio,"/",anoinicio,sep="")
                afast[j+k,4]<-paste(days_in_month(mesinicio),"/",mesinicio,"/",anoinicio,sep="")
                afast[j+k,5]<-diascalc[1]
            }
            else{
                if(k==gap){
                  #Divide um período de afastamento em mais de um registro no banco de dados e calcula o número
                  #de dias de afastamento.
                  #Aqui, é trabalhado o caso do último intervalo de afastamento (último mês)
                    afast[j+k,3]<-paste("01/",mesfim,"/",anofim,sep="")
                    afast[j+k,4]<-afastamentos[i,7]
                    afast[j+k,5]<-diascalc[2]
                }
                else{
                    if(mesinicio+k<13){
                   #Divide um período de afastamento em mais de um registro no banco de dados e calcula o número
                   #de dias de afastamento.
                   #Aqui, é trabalhado o caso em que o período de afastamento é de até 1 ano
                        afast[j+k,3]<-paste("01/",mesinicio+k,"/",anoinicio,sep="")
                        afast[j+k,4]<-paste(days_in_month(mesinicio+k),"/",mesinicio+k,"/",anoinicio,sep="")
                        afast[j+k,5]<-days_in_month(mesinicio+k)
                    }
                    else{
                        if(mesinicio+k<25){
                    #Divide um período de afastamento em mais de um registro no banco de dados e calcula o número
                    #de dias de afastamento.
                    #Aqui, é trabalhado o caso em que o período de afastamento está entre 1 e 2 anos
                            afast[j+k,3]<-paste("01/",mesinicio+k-12,"/",anoinicio+1,sep="")
                            afast[j+k,4]<-paste(days_in_month(mesinicio+k-12),"/",mesinicio+k-12,"/",anoinicio+1,sep="")
                            afast[j+k,5]<-days_in_month(mesinicio+k-12)
                        }
                        else{
                    #Divide um período de afastamento em mais de um registro no banco de dados e calcula o número
                    #de dias de afastamento.
                    #Aqui, é trabalhado o caso em que o período de afastamento é maior que 2 anos                          
                          afast[j+k,3]<-paste("01/",mesinicio+k-24,"/",anoinicio+2,sep="")
                          afast[j+k,4]<-paste(days_in_month(mesinicio+k-24),"/",mesinicio+k-24,"/",anoinicio+2,sep="")
                          afast[j+k,5]<-days_in_month(mesinicio+k-24)
                        }
                    }
                }
            }
            afast[j+k,6]<-afastamentos[i,9]
      }
      j<-j+gap+1
  }
  
}


#------ -------Cálculo dos períodos a serem considerados na apuração da produtividade-------------------------------

afast$MesAno<-paste(month(dmy(afast[,3])),"/",year(dmy(afast[,3])),sep="") #inclui uma coluna identificando o mês do afastamento
afast$Mes<-month(dmy(afast[,3]))
afast$Ano<-year(dmy(afast[,3]))

afast<-afast[order(afast$Ano,afast$Mes,afast$Juiz,decreasing=c(TRUE,TRUE,FALSE)),]

res<-aggregate(afast$DiasC,by=list(afast$Juiz,afast$MesAno),FUN=sum) #Matriz que abriga os dados de dias de afastamento por mês de todos os juizes
names(res)<-c("Juiz","MesAno","Dias") #Nomeia as colunas da matriz
res<-data.frame(res,colsplit(res$MesAno,split="/",names=c("Mes","Ano")))
res<-res[order(res$Juiz,res$Ano,res$Mes,decreasing=c(F,T,T),method="radix"),]

juizes<-unique(res[,1]) #Todos os juízes que aparecem na listagem de afastamentos
njuizes<-length(juizes) #Número de juízes "unicos" que existem nos dados dos afastamentos

perpjuiz<-matrix(0,njuizes,25)#Matriz que abriga os períodos que serão considerados para aferição da produtividade dos juízes


#-------------------------------------------------------------------------------------------------------
#A variável "período" abrigará todos os períodos considerados para extração do relatório Mentorh
periodo<-0
l<-1
for(i in p1[2]:p2[2]){
  if (i!=p2[2]){
    for(j in 1:12){
      periodo[l]<-paste(j,i,sep="/")
      l<-l+1
    }
  }
  else{
    for(j in 1:p2[1]){
      periodo[l]<-paste(j,i,sep="/")
      l<-l+1
    }
  }
}
#------------------------------------------------------------------------------------------------------------
#---Armazena na variável "perpjuiz" os períodos que devem ser considerados para a mensuração da produtividade---
bduniao<-0
for(i in 1:njuizes){
  prov<-subset(res,res[,1]==juizes[i]&res[,3]>15)[,2] #Períodos que tem que ser desconsiderados do cálculo
  pconsid<-periodo
  for(l in 1:length(prov)){
    pconsid<-pconsid[pconsid!=prov[l]]
  }
  perpjuiz[i,]<-cbind(juizes[i],t(pconsid[length(pconsid):(length(pconsid)-23)]))
  bduniao<-rbind(bduniao,cbind(perpjuiz[i,1],perpjuiz[i,2:25]))
}
bduniao<-bduniao[-1,]
colnames(bduniao)<-c("Juiz","MesAno")
bduniao<-data.frame(bduniao)
bduniao<-mutate(bduniao,Dias=0)
#------------------------------------------------------------------------------------------------------------
#---Armazena no data frame resfinal os períodos que devem ser considerados para a mensuração da------------- 
#---produtividade, bem como os dias de afastamento ocorridos em cada mês ------------------------------------
bdfinal<-0
for(i in 1:njuizes){  
  resfinal<-res %>%
    select(Juiz,MesAno,Dias) %>%
    filter(MesAno %in% perpjuiz[i,2:25] & Juiz==perpjuiz[i,1])
  
  bdtemp<-bduniao %>%
    filter(Juiz==resfinal[1,1] & !MesAno %in% resfinal[,2])
  
  bdfinal<-rbind(bdfinal,resfinal,bdtemp)
  
}
bdfinal<-data.frame(bdfinal[-1,])
bdfinal<-data.frame(bdfinal,colsplit(bdfinal$MesAno,split="/",names=c("Mes","Ano")))
bdfinal<-bdfinal[order(bdfinal$Juiz,bdfinal$Ano,bdfinal$Mes,decreasing=c(F,T,T),method="radix"),]

#------- variável "bdfinal" abriga os dados de todos os juizes com seus respectivos meses que devem ser---------
#-------------considerados na apuração da produtividade, bem como os dias de afastamento em cada mês------------

#View(bdfinal)


#----------------------IMPORTANDO A BASE DE DADOS REFERENTES ÀS PRODUTIVIDADES DOS MAGISTRADOS-----------------
#Dados de audiência extraídos do e-Gestão: Pastas públicas/e-Gestão/1ª Instância/Relatórios Gerenciais/
#Dos Juízes/Diversos/B.3.3 - Audiências realizadas por magistrado - Universo Unx
#Dados de incidentes extraídos do e-Gestão: Pastas públicas/e-Gestão/1ª Instância/Relatórios Gerenciais/
#Dos Juízes/Diversos/B.3.2 - Incidentes processuais por magistrado
#Dados de audiências e sentenças extraídos do e-Gestão: Pastas públicas/e-Gestão/1ª Instância/Relatórios Gerenciais/
#Dos Juízes/Produtividade/B.1.1 - Solucionados


prod<-read.csv("produtividade.csv",h=T,sep=";",stringsAsFactors = FALSE)

bdfinal$JA<-paste(bdfinal[,1],bdfinal[,2])
prod$JAno<-paste (prod[,2],prod[,1])

#--data frame "tests" armazenam os dados das produtividades dos magistrados apenas dos meses que devem ser considerados----
test<-prod %>%
  filter(JAno %in% bdfinal$JA & CRITÉRIO=="SENTENÇAS")

testinc<-prod %>%
  filter(JAno %in% bdfinal$JA & CRITÉRIO=="INCIDENTES")

testaud<-prod %>%
  filter(JAno %in% bdfinal$JA & CRITÉRIO=="AUDIÊNCIAS")

testconc<-prod %>%
  filter(JAno %in% bdfinal$JA & CRITÉRIO=="CONCILIAÇÕES")


#---Esses são os registros em que não houve produtividade mesmo os juizes trabalhando no período-------
sem_prod<-match(bdfinal$JA,test$JAno,nomatch=0)
falta<-data.frame(bdfinal$JA[sem_prod==0])

sem_prodinc<-match(bdfinal$JA,testinc$JAno,nomatch=0)
faltainc<-data.frame(bdfinal$JA[sem_prodinc==0])

sem_prodaud<-match(bdfinal$JA,testaud$JAno,nomatch=0)
faltaaud<-data.frame(bdfinal$JA[sem_prodaud==0])

sem_prodconc<-match(bdfinal$JA,testconc$JAno,nomatch=0)
faltaconc<-data.frame(bdfinal$JA[sem_prodconc==0])


#--Acrescenta as variáveis sent (Sentenças), inc (incidentes), aud(audiencias) e conc (conciliações) no banco de dados bdfinal
#--observando os meses em que, mesmo trabalhando não foi observada produtividade do juiz--------------

bdfinal$sent<-0
for(i in 1:length(sem_prod)){
    if(sem_prod[i]!=0){
        bdfinal[i,7]<-test[sem_prod[i],3]
    }
}

bdfinal$inc<-0
for(i in 1:length(sem_prodinc)){
  if(sem_prodinc[i]!=0){
    bdfinal[i,8]<-testinc[sem_prodinc[i],3]
  }
}

bdfinal$aud<-0
for(i in 1:length(sem_prodaud)){
  if(sem_prodaud[i]!=0){
    bdfinal[i,9]<-testaud[sem_prodaud[i],3]
  }
}

bdfinal$conc<-0
for(i in 1:length(sem_prodconc)){
  if(sem_prodconc[i]!=0){
    bdfinal[i,10]<-testconc[sem_prodconc[i],3]
  }
}

#-----Armazena no data frame "totais" os totais das produtividades, por juiz, de cada variável--------------------------
#---Este data frame foi criado apenas para conferência dos valores importados da base de dados do excel----------------
totaissent<-aggregate(bdfinal$sent,by=list(bdfinal$Juiz),FUN=sum)
totaisinc<-aggregate(bdfinal$inc,by=list(bdfinal$Juiz),FUN=sum)
totaisaud<-aggregate(bdfinal$aud,by=list(bdfinal$Juiz),FUN=sum)
totaisconc<-aggregate(bdfinal$conc,by=list(bdfinal$Juiz),FUN=sum)
totais<-data.frame(totaissent,totaisinc$x,totaisaud$x,totaisconc$x)
colnames(totais)<-c("Juiz","Sentenças","Incidentes","Audiencias","Conciliacoes")

#---Inclui nos data frames bdcariri, bdcaucaia, bdfortaleza,bdg3,bdg4,bdg5,bdg6,bdg7,bdmaracanau e bdsobral os dados--------
#--- das produtividades necessários para o cálculo das pontuações-----------------------------------------------------------

#------------------------------------Iniciando o agrupamento das unidades-----------------------------------
#---AGrupamento feito aqui é baseado nos seguintes critérios:
#----------1) Grupos de unidades que fazem parte da mesma jurisdição
#----------2) Unidades que são únicas na jurisdição são agrupadas segundo as faixas da Resol. CSJT 63/2010


juizestit<-read.csv("juizes titulares de varas.csv",h=T,sep=";",stringsAsFactors = FALSE)

#Cariri - Aqui são agrupados os seguintes magistrados:
#---1) FERNANDA MONTEIRO LIMA VERDE
#---2) REGIANE FERREIRA CARVALHO SILVA
#---3) CLOVIS VALENÇA ALVES FILHO

cariri<-juizestit[c(1,6,11),]
bdcariri<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% cariri[,2]) 
  grupocariri<-setNames(aggregate(bdcariri$sent,by=list(bdcariri$Juiz),FUN=sum),c("Juiz","sentencas")) 
  grupocariri<-grupocariri %>%
  mutate(incidentes=aggregate(bdcariri$inc,by=list(bdcariri$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdcariri$aud,by=list(bdcariri$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdcariri$conc,by=list(bdcariri$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdcariri$Dias,by=list(bdcariri$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 
    
  grupocariri<-grupocariri %>%
  mutate(pontuacao_sent=(grupocariri$media_sent/max(grupocariri$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupocariri$media_inc/max(grupocariri$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupocariri$media_aud/max(grupocariri$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupocariri$media_conc/max(grupocariri$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)
  

#Caucaia - Aqui são agrupados os seguintes magistrados:
#---1) ANTONIO GONÇALVES PEREIRA
#---2)HERMANO QUEIROZ JUNIOR

caucaia<-juizestit[c(2,7),]
bdcaucaia<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% caucaia[,2]) 

grupocaucaia<-setNames(aggregate(bdcaucaia$sent,by=list(bdcaucaia$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupocaucaia<-grupocaucaia %>%
  mutate(incidentes=aggregate(bdcaucaia$inc,by=list(bdcaucaia$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdcaucaia$aud,by=list(bdcaucaia$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdcaucaia$conc,by=list(bdcaucaia$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdcaucaia$Dias,by=list(bdcaucaia$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupocaucaia<-grupocaucaia %>%
  mutate(pontuacao_sent=(grupocaucaia$media_sent/max(grupocaucaia$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupocaucaia$media_inc/max(grupocaucaia$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupocaucaia$media_aud/max(grupocaucaia$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupocaucaia$media_conc/max(grupocaucaia$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)

    
#Fortaleza - Aqui são agrupados os seguintes magistrados:
#---1) JOSE MARIA COELHO FILHO
#---2) RAFAEL MARCILIO XEREZ
#---3) GERMANO SILVEIRA DE SIQUEIRA
#---4) MARIA ROSA DE ARAUJO MESTRES
#---5) ROSSANA RAIA DOS SANTOS
#---6) MILENA MOREIRA DE SOUSA
#---7) FRANCISCO ANTONIO DA SILVA FORTUNA
#---8) ROSA DE LOURDES AZEVEDO BRINGEL
#---9) FRANCISCO GERARDO DE SOUZA JUNIOR
#---10) IVANIA SILVA ARAUJO
#---11) ANA LUIZA RIBEIRO BEZERRA
#---12) ANTONIO TEOFILO FILHO
#---13) SINEZIO BERNARDO DE OLIVEIRA
#---14) SANDRA HELENA BARROS DE SIQUEIRA
#---15) JOAO CARLOS DE OLIVEIRA UCHOA
#---16) ALDENORA MARIA DE SOUZA SIQUEIRA
#---17) JOSE HENRIQUE AGUIAR
#---18) PAULO REGIS MACHADO BOTELHO


fortaleza<-juizestit[c(3,8,12:27),]
bdfortaleza<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% fortaleza[,2])
  
grupofortaleza<-setNames(aggregate(bdfortaleza$sent,by=list(bdfortaleza$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupofortaleza<-grupofortaleza %>%
  mutate(incidentes=aggregate(bdfortaleza$inc,by=list(bdfortaleza$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdfortaleza$aud,by=list(bdfortaleza$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdfortaleza$conc,by=list(bdfortaleza$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdfortaleza$Dias,by=list(bdfortaleza$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupofortaleza<-grupofortaleza %>%
  mutate(pontuacao_sent=(grupofortaleza$media_sent/max(grupofortaleza$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupofortaleza$media_inc/max(grupofortaleza$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupofortaleza$media_aud/max(grupofortaleza$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupofortaleza$media_conc/max(grupofortaleza$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)

    
#Sobral - Aqui são agrupados os seguintes magistrados:
#---1) SUYANE BELCHIOR PARAIBA DE ARAGAO
#---2) LUCIVALDO MUNIZ FEITOSA

sobral<-juizestit[c(5,10),]
bdsobral<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% sobral[,2]) 

gruposobral<-setNames(aggregate(bdsobral$sent,by=list(bdsobral$Juiz),FUN=sum),c("Juiz","sentencas")) 
gruposobral<-gruposobral %>%
  mutate(incidentes=aggregate(bdsobral$inc,by=list(bdsobral$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdsobral$aud,by=list(bdsobral$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdsobral$conc,by=list(bdsobral$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdsobral$Dias,by=list(bdsobral$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

gruposobral<-gruposobral %>%
  mutate(pontuacao_sent=(gruposobral$media_sent/max(gruposobral$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(gruposobral$media_inc/max(gruposobral$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(gruposobral$media_aud/max(gruposobral$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(gruposobral$media_conc/max(gruposobral$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)


  
#Maracanaú - Aqui são agrupados os seguintes magistrados:
#---1) ROSSANA TALIA MODESTO GOMES SAMPAIO
#---2) CARLOS ALBERTO TRINDADE REBONATTO


maracanau<-juizestit[c(4,9),]
bdmaracanau<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% maracanau[,2]) 

grupomaracanau<-setNames(aggregate(bdmaracanau$sent,by=list(bdmaracanau$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupomaracanau<-grupomaracanau %>%
  mutate(incidentes=aggregate(bdmaracanau$inc,by=list(bdmaracanau$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdmaracanau$aud,by=list(bdmaracanau$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdmaracanau$conc,by=list(bdmaracanau$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdmaracanau$Dias,by=list(bdmaracanau$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupomaracanau<-grupomaracanau %>%
  mutate(pontuacao_sent=(grupomaracanau$media_sent/max(grupomaracanau$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupomaracanau$media_inc/max(grupomaracanau$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupomaracanau$media_aud/max(grupomaracanau$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupomaracanau$media_conc/max(grupomaracanau$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)
  
#Grupo 3 Res. CSJT 63/2010 - De 751 a 1000 processos recebidos. Aqui são agrupados os seguintes magistrados:
#---1) LENA MARCILIO XEREZ - Baturité
#---2) MARCELO LIMA GUERRA - Quixadá

g3<-juizestit[c(29,35),]
bdgrupo3<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g3[,2]) 

grupo3<-setNames(aggregate(bdgrupo3$sent,by=list(bdgrupo3$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupo3<-grupo3 %>%
  mutate(incidentes=aggregate(bdgrupo3$inc,by=list(bdgrupo3$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo3$aud,by=list(bdgrupo3$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo3$conc,by=list(bdgrupo3$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo3$Dias,by=list(bdgrupo3$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo3<-grupo3 %>%
  mutate(pontuacao_sent=(grupo3$media_sent/max(grupo3$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo3$media_inc/max(grupo3$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo3$media_aud/max(grupo3$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo3$media_conc/max(grupo3$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)
  
#Grupo 4 Res. CSJT 63/2010 - De 1001 a 1500 processos recebidos. Aqui são agrupados os seguintes magistrados:
#---1) ROBERIO MAIA DE OLIVEIRA - Aracati
#---2) LAURA ANISIA MOREIRA DE SOUSA PINTO - Crateús
#---3) MATEUS MIRANDA DE MORAES - Limoeiro

g4<-juizestit[c(28,30,33),]
bdgrupo4<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g4[,2])

grupo4<-setNames(aggregate(bdgrupo4$sent,by=list(bdgrupo4$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupo4<-grupo4 %>%
  mutate(incidentes=aggregate(bdgrupo4$inc,by=list(bdgrupo4$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo4$aud,by=list(bdgrupo4$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo4$conc,by=list(bdgrupo4$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo4$Dias,by=list(bdgrupo4$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo4<-grupo4 %>%
  mutate(pontuacao_sent=(grupo4$media_sent/max(grupo4$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo4$media_inc/max(grupo4$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo4$media_aud/max(grupo4$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo4$media_conc/max(grupo4$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)
  
#Grupo 5 Res. CSJT 63/2010 - De 1501 a 2000 processos recebidos. Aqui são agrupados os seguintes magistrados:
#---1) CHRISTIANNE FERNANDES CARVALHO DIOGENES RIBEIRO - Iguatu
#---2) KELLY CRISTINA DINIZ PORTO - Pacajus
#---3) LUCIO FLAVIO APOLIANO RIBEIRO - Tianguá

g5<-juizestit[c(32,34,37),]
bdgrupo5<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g5[,2])
  
  grupo5<-setNames(aggregate(bdgrupo5$sent,by=list(bdgrupo5$Juiz),FUN=sum),c("Juiz","sentencas")) 
  grupo5<-grupo5 %>%
  mutate(incidentes=aggregate(bdgrupo5$inc,by=list(bdgrupo5$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo5$aud,by=list(bdgrupo5$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo5$conc,by=list(bdgrupo5$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo5$Dias,by=list(bdgrupo5$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo5<-grupo5 %>%
  mutate(pontuacao_sent=(grupo5$media_sent/max(grupo5$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo5$media_inc/max(grupo5$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo5$media_aud/max(grupo5$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo5$media_conc/max(grupo5$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)

    
#Grupo 6 Res. CSJT 63/2010 - De 2001 a 2500 processos recebidos. Aqui são agrupados os seguintes magistrados:
#---1) KONRAD SARAIVA MOTA - São Gonçalo

g6<-juizestit[36,]
bdgrupo6<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g6[,2])

grupo6<-setNames(aggregate(bdgrupo6$sent,by=list(bdgrupo6$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupo6<-grupo6 %>%
  mutate(incidentes=aggregate(bdgrupo6$inc,by=list(bdgrupo6$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo6$aud,by=list(bdgrupo6$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo6$conc,by=list(bdgrupo6$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo6$Dias,by=list(bdgrupo6$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo6<-grupo6 %>%
  mutate(pontuacao_sent=(grupo6$media_sent/max(grupo6$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo6$media_inc/max(grupo6$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo6$media_aud/max(grupo6$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo6$media_conc/max(grupo6$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)
  
#Grupo 7 Res. CSJT 63/2010 - Acima de 2500 processos recebidos. Aqui são agrupados os seguintes magistrados:
#---1) JUDICAEL SUDARIO DE PINHO

g7<-juizestit[31,]
bdgrupo7<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g7[,2])
  
  grupo7<-setNames(aggregate(bdgrupo7$sent,by=list(bdgrupo7$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupo7<-grupo7 %>%
  mutate(incidentes=aggregate(bdgrupo7$inc,by=list(bdgrupo7$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo7$aud,by=list(bdgrupo7$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo7$conc,by=list(bdgrupo7$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo7$Dias,by=list(bdgrupo7$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo7<-grupo7 %>%
  mutate(pontuacao_sent=(grupo7$media_sent/max(grupo7$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo7$media_inc/max(grupo7$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo7$media_aud/max(grupo7$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo7$media_conc/max(grupo7$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)

#----Armazena no data frame "pontuacoes" os dados (pontos obtidos no processo de promoção) de todos os juizes 
#----titulares

pontuacoes<-data.frame(rbind(grupocariri,grupocaucaia,grupofortaleza,grupomaracanau,gruposobral,grupo3,grupo4,
                             grupo5,grupo6,grupo7))


pontuacoes<-pontuacoes[order(pontuacoes$pontuacao_tot,decreasing=T),]
#xl.sheet.activate("Plan1")
#xl[a1]<-t(names(pontuacoes))
#xl[a2]<-pontuacoes

#-------------Aqui, vou agrupar a VT São Gonçalo do Amarante à VT Eusébio, conforme sugerido na proposição da
#---------presidência. A saber:
#
#"§ 5º A unidade judicial que se enquadra na hipótese do parágrafo 4º, cuja quantidade de processos novos 
#recebidos no triênio não se insere na faixa de movimentação processual de nenhuma outra Vara única, deve
#ser agrupada com as Varas que se enquadram na faixa de movimentação processual superior ou, não havendo 
#Varas com faixa de movimentação processual superior, deve ser considerada como próprio parâmetro.

#----------------------  O resultado está contido no data frame "pontuacoes_1"  ----------------------------------------



g7_1<-juizestit[c(31,36),]
bdgrupo7_1<-bdfinal %>%
  select(Juiz,MesAno,Mes,Dias,sent,inc,aud,conc) %>%
  filter(Juiz %in% g7_1[,2])

grupo7_1<-setNames(aggregate(bdgrupo7_1$sent,by=list(bdgrupo7_1$Juiz),FUN=sum),c("Juiz","sentencas")) 
grupo7_1<-grupo7_1 %>%
  mutate(incidentes=aggregate(bdgrupo7_1$inc,by=list(bdgrupo7_1$Juiz),FUN=sum)$x) %>%
  mutate(audiencias=aggregate(bdgrupo7_1$aud,by=list(bdgrupo7_1$Juiz),FUN=sum)$x) %>%
  mutate(conciliacoes=aggregate(bdgrupo7_1$conc,by=list(bdgrupo7_1$Juiz),FUN=sum)$x) %>%
  mutate(dias_trab=730-aggregate(bdgrupo7_1$Dias,by=list(bdgrupo7_1$Juiz),FUN=sum)$x) %>%
  mutate(media_sent=(sentencas/dias_trab)*30) %>%
  mutate(media_inc=(incidentes/dias_trab)*30) %>%
  mutate(media_aud=(audiencias/dias_trab)*30) %>%
  mutate(media_conc=(conciliacoes/dias_trab)*30) 

grupo7_1<-grupo7_1 %>%
  mutate(pontuacao_sent=(grupo7_1$media_sent/max(grupo7_1$media_sent))*pont_sent) %>%
  mutate(pontuacao_inc=(grupo7_1$media_inc/max(grupo7_1$media_inc))*pont_inc) %>%
  mutate(pontuacao_aud=(grupo7_1$media_aud/max(grupo7_1$media_aud))*pont_aud) %>%
  mutate(pontuacao_conc=(grupo7_1$media_conc/max(grupo7_1$media_conc))*pont_conc) %>%
  mutate(pontuacao_tot=pontuacao_sent+pontuacao_inc+pontuacao_aud+pontuacao_conc)

pontuacoes_1<-data.frame(rbind(grupocariri,grupocaucaia,grupofortaleza,grupomaracanau,gruposobral,grupo3,grupo4,
                             grupo5,grupo7_1))


pontuacoes_1<-pontuacoes_1[order(pontuacoes_1$pontuacao_tot,decreasing=T),]

#xl.sheet.activate("Plan2")
#xl[a1]<-t(names(pontuacoes_1))
#xl[a2]<-pontuacoes_1




#---------------------------------CÁLCULO DA PONTUAÇÃO DO CRITÉRIO PRESTEZA------------------------------------

#Importa o banco de dados que contem as datas das audiencias iniciais realizadas e a respectiva data de ajuizamento
#Importa o banco de dados que contem apenas os juizes titulares de varas
#Importa o banco de dados que contem as datas das audiencias de prosseguimento e a respectiva data da audiência inicial
audporjuiz<-read.csv("ajuizamento_audiencia_inicial.csv",h=T,sep=";")
juizestit_sem_matricula<-read.csv("juizes titulares de varas_sem_matricula.csv",h=T,sep=";")
audprosseg<-read.csv("aud_inicial_aud_prosseg.csv",h=T,sep=";")
sentencas<-read.csv("sentencas.csv",h=T,sep=";")

anos<-c(2010:2018) #Define quais anos de ajuizamento serão utilizados para o cálculo do prazo. Considerei aqui que 
#o prazo para realização de audiência superior a um ano é erro no select.

#Inclui a variável ano_audiencia, mes_audiencia e ano_ajuizamento no banco de dados
audporjuiz<-audporjuiz%>%
  mutate(ano_audiencia=year(dmy_hm(Audiencia)),mes_audiencia=month(dmy_hm(Audiencia)),ano_ajuizamento=year(dmy_hm(Ajuizamento)))
audprosseg<-audprosseg%>%
  mutate(ano_audpros=year(dmy_hm(Data_2_aud)),mes_audpros=month(dmy_hm(Data_2_aud)))
sentencas<-sentencas%>%
  mutate(ano_sent=year(dmy_hm(Prol_sentenca)),mes_sent=month(dmy_hm(Prol_sentenca)))

#padroniza o nome dos juizes sem acentos e cedilhas
audporjuiz$Nome_juiz<-(iconv(audporjuiz$Nome_juiz,to="ASCII//TRANSLIT"))
audprosseg$Juiz<-iconv(audprosseg$Juiz,to="ASCII//TRANSLIT")
sentencas$Juiz<-iconv(sentencas$Juiz,to="ASCII//TRANSLIT")

#Padroniza os nomes dos juizes que são diferentes nas bases de dados do SPT1 e PJE
juiz<-case_when(as.character(audporjuiz$Nome_juiz)=="CHRISTIANNE FERNANDES CARVALHO DIOGENES"~"CHRISTIANNE FERNANDES CARVALHO DIOGENES RIBEIRO",
            as.character(audporjuiz$Nome_juiz)=="ANA PAULA BARROSO SOBREIRA"~"ANA PAULA BARROSO SOBREIRA PINHEIRO",
            TRUE~as.character(audporjuiz$Nome_juiz))
audporjuiz$Nome_juiz<-juiz
juiz1<-case_when(as.character(audprosseg$Juiz)=="CHRISTIANNE FERNANDES CARVALHO DIOGENES"~"CHRISTIANNE FERNANDES CARVALHO DIOGENES RIBEIRO",
                 as.character(audprosseg$Juiz)=="ANA PAULA BARROSO SOBREIRA"~"ANA PAULA BARROSO SOBREIRA PINHEIRO",
                 TRUE~as.character(audprosseg$Juiz))
audprosseg$Juiz<-juiz1
juiz2<-case_when(as.character(sentencas$Juiz)=="CHRISTIANNE FERNANDES CARVALHO DIOGENES"~"CHRISTIANNE FERNANDES CARVALHO DIOGENES RIBEIRO",
                 as.character(sentencas$Juiz)=="ANA PAULA BARROSO SOBREIRA"~"ANA PAULA BARROSO SOBREIRA PINHEIRO",
                 TRUE~as.character(sentencas$Juiz))
sentencas$Juiz<-juiz2

#define as variáveis como do tipo tibble
audjuiztit<-as_tibble(audporjuiz)
juizestit_sem_matricula<-as_tibble(juizestit_sem_matricula)
audprossegtit<-as_tibble(audprosseg)
sentencastit<-as_tibble(sentencas)

#Armazena na variável audjuiztit apenas os dados dos juizes titulares e de processos com data de ajuizamento após 2010
audjuiztit<-audjuiztit%>%
  filter(audjuiztit$Nome_juiz %in% juizestit_sem_matricula$Juiz,audjuiztit$ano_ajuizamento %in% anos)
#Armazena na variável audprossegtit apenas os dados dos juizes titulares
audprossegtit<-audprossegtit%>%
  filter(audprossegtit$Juiz %in% juizestit_sem_matricula$Juiz)
#Armazena na variável sentencastit apenas os dados dos juizes titulares
sentencastit<-sentencastit%>%
  filter(sentencastit$Juiz %in% juizestit_sem_matricula$Juiz)


#Identificando e removendo os outliers
audjuiztit_out<-audjuiztit
source("https://goo.gl/4mthoF")
outlierKD(audjuiztit_out,Prazo)
yes
audjuiztit_out<-filter(audjuiztit_out,audjuiztit_out$Prazo!="NA")
audjuiztit_out$mesano<-paste(audjuiztit_out$mes_audiencia,"/",audjuiztit_out$ano_audiencia,sep="")
audjuiztit_out$juizmesano<-paste(audjuiztit_out$Nome_juiz,"-",audjuiztit_out$mes_audiencia,"/",audjuiztit_out$ano_audiencia,sep="")

audprossegtit_out<-audprossegtit
outlierKD(audprossegtit_out,Prazo)
yes
audprossegtit_out<-filter(audprossegtit_out,audprossegtit_out$Prazo!="NA")
audprossegtit_out$mesano<-paste(audprossegtit_out$mes_audpros,"/",audprossegtit_out$ano_audpros,sep="")
audprossegtit_out$juizmesano<-paste(audprossegtit_out$Juiz,"-",audprossegtit_out$mes_audpros,"/",audprossegtit_out$ano_audpros,sep="")

sentencastit_out_enc<-sentencastit
outlierKD(sentencastit_out_enc,Prazo_enc_sent)
yes
sentencastit_out_enc<-filter(sentencastit_out_enc,sentencastit_out_enc$Prazo_enc_sent!="NA")
sentencastit_out_enc$mesano<-paste(sentencastit_out_enc$mes_sent,"/",sentencastit_out_enc$ano_sent,sep="")
sentencastit_out_enc$juizmesano<-paste(sentencastit_out_enc$Juiz,"-",sentencastit_out_enc$mes_sent,"/",sentencastit_out_enc$ano_sent,sep="")


#Armazena na variável resumo_mesano_out o tempo médio transcorrido do ajuizamento a realização da audiência inicial
#por MesAno referente apenas aos períodos que devem ser considerados no processo de promoção. Para este 
#cálculo foi efetuada a remoção dos outliers com o intuito de remover os prazos que estavam com erro 
#devido ao fato do select efetuado ainda extrair dados com erros de prazo.
periodos<-data.frame(bdfinal,colsplit(bdfinal$Juiz,split="  - ",names=c("Juiz_titular","Matricula")))
JMA<-paste(str_trim(periodos$Juiz_titular),"-",str_trim(periodos$MesAno),sep="")
audjuiztit_out<-filter(audjuiztit_out,juizmesano %in% JMA)
tab<-table(audjuiztit_out$Nome_juiz)
audprossegtit_out<-filter(audprossegtit_out,juizmesano %in% JMA)
tab1<-table(audprossegtit_out$Juiz)
sentencastit_out_enc<-filter(sentencastit_out_enc,juizmesano %in% JMA)
tab2<-table(sentencastit_out_enc$Juiz)

resumo_mesano_out<-aggregate(audjuiztit_out$Prazo,by=list(audjuiztit_out$Nome_juiz,audjuiztit_out$mesano),FUN=sum)
names(resumo_mesano_out)<-c("Juiz","Ano","Prazo")
resumo_mesano_out<-arrange(resumo_mesano_out,Juiz)

res_audpros_out<-aggregate(audprossegtit_out$Prazo,by=list(audprossegtit_out$Juiz,audprossegtit_out$mesano),FUN=sum)
names(res_audpros_out)<-c("Juiz","Ano","Prazo")
res_audpros_out<-arrange(res_audpros_out,Juiz)

res_sentencas_out_enc<-aggregate(sentencastit_out_enc$Prazo_enc_sent,by=list(sentencastit_out_enc$Juiz,sentencastit_out_enc$mesano),FUN=sum)
names(res_sentencas_out_enc)<-c("Juiz","Ano","Prazo_enc_sent")
res_sentencas_out_enc<-arrange(res_sentencas_out_enc,Juiz)


#Armazena na variável resumo_juiz_out os dados de prazo médio do ajuizamento a realização da primeira
#audiência considerando apenas os períodos atinentes ao processo de promoção por Juiz.
resumo_juiz_out<-aggregate(resumo_mesano_out$Prazo,by=list(resumo_mesano_out$Juiz),FUN=sum)
resumo_juiz_out$Prazo_medio<-resumo_juiz_out$x/tab
names(resumo_juiz_out)<-c("Juiz","Soma dos prazos","Prazo_medio")

res_audpros_juiz_out<-aggregate(res_audpros_out$Prazo,by=list(res_audpros_out$Juiz),FUN=sum)
res_audpros_juiz_out$Prazo<-res_audpros_juiz_out$x/tab1
names(res_audpros_juiz_out)<-c("Juiz","Soma dos prazos","Prazo_medio")

res_sentencas_juiz_out_enc<-aggregate(res_sentencas_out_enc$Prazo,by=list(res_sentencas_out_enc$Juiz),FUN=sum)
res_sentencas_juiz_out_enc$Prazo<-res_sentencas_juiz_out_enc$x/tab2
names(res_sentencas_juiz_out_enc)<-c("Juiz","Soma dos prazos","Prazo_medio_enc_sent")


#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

####----------------Código para comparação dos resultados do select tratado (com a remoção dos
####------------------outliers) com os resultados apresentados pelo e-Gestão------------------------------------


#Armazena na variável resumo_ano_out o tempo médio transcorrido do ajuizamento a realização da audiência inicial
#por Ano. Essa variável é útil apenas para comparação do prazo calculado nesse programa com o valor
#obtido no e-Gestão. Nesta variável, são considerados os processos de todos os meses (não apenas os
#considerados no processo de promoção) que não são outliers.
            #resumo_ano<-aggregate(audjuiztit$Prazo,by=list(audjuiztit$Nome_juiz,audjuiztit$ano_audiencia),FUN=mean)
            #names(resumo_ano)<-c("Juiz","Ano","Prazo")
            #resumo_ano<-arrange(resumo_ano,Juiz)
resumo_ano_out<-aggregate(audjuiztit_out$Prazo,by=list(audjuiztit_out$Nome_juiz,audjuiztit_out$ano_audiencia),FUN=mean)
names(resumo_ano_out)<-c("Juiz","Ano","Prazo")
resumo_ano_out<-arrange(resumo_ano_out,Juiz)

#Armazena na variável resumo o tempo médio transcorrido do ajuizamento a realização da audiência inicial
#por Juiz. Essa variável é útil apenas para comparação do prazo calculado nesse programa com o valor
#obtido no e-Gestão. Nesta variável, são considerados os processos de todos os meses (não apenas os
#considerados no processo de promoção) que não são outliers.
            #resumo<-aggregate(audjuiztit$Prazo,by=list(audjuiztit$Nome_juiz),FUN=mean)
            #names(resumo)<-c("Juiz","Prazo")
            #resumo<-arrange(resumo,Juiz)
resumo_out<-aggregate(audjuiztit_out$Prazo,by=list(audjuiztit_out$Nome_juiz),FUN=mean)
names(resumo_out)<-c("Juiz","Prazo")
resumo_out<-arrange(resumo_out,Juiz)

#verifica se existe alguma inconsistência do tipo: Data de ajuizamento > Data da audiência
#no select feito pela SETIC
neg<-filter(audjuiztit,Prazo<0)

#-----------------------------------------------------------------------------------------------




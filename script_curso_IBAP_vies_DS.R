#pacotes que serão utilizados
library(readxl)
library(psych)
library(lavaan)

#importar o banco de dados

banco <- read_excel("banco_curso_ibap.xlsx")


#selecao itens
selecao<-c("BFI_8","BFI_13","BFI_16","BFI_28",
           "BFI_15","BFI_27","BFI_32","BFI_41")
banco_selec <- banco[,selecao]

### mapa dos itens
conteudo<-c(-1,1,1,-1,   -1,-1,1,1)
ds<-      c(1,1,-1,-1,   1,-1,-1,1)
mapa<-cbind.data.frame(selecao, conteudo, ds)


#análise fatorial
?fa
AF1<-fa(banco_selec, nfactors=2, missing=T, impute="median",
       cor="poly", )
summary(AF1)
AF1$loadings


#com rotação bifactor (equivalente a bigeomin do MPlus)
AF2<-fa(banco_selec, nfactors=3, rotate = "bifactor",
        missing=T, impute="median",
        cor="poly", )
summary(AF2)
AF2$loadings

##AFC confirmatória
?sem
modeloSemDS <-" amab =~ BFI_8 + BFI_13 + BFI_16 + BFI_28
                cons =~ BFI_15 + BFI_27 + BFI_32 +BFI_41"
#atençã: amabailidade fica invertida
AFCSemDs<-sem(model=modeloSemDS, data=banco_selec, ordered=selecao)
summary(AFCSemDs, standardized=T)

#bifactor
modeloBifactorDS <-" amab =~ BFI_8 + BFI_13 + BFI_16 + BFI_28
                     cons =~ BFI_15 + BFI_27 + BFI_32 +BFI_41
                     DS =~ NA*BFI_8 + BFI_13 + BFI_16 + BFI_28 + 
                           BFI_15 + BFI_27 + BFI_32 +BFI_41
                     DS ~~ 1*DS
                     DS ~~ 0*amab
                     DS ~~ 0*cons
                               "
AFCBifactorDs<-sem(model=modeloBifactorDS, data=banco_selec)
summary(AFCBifactorDs, standardized=T)

#bifactor restrito
modeloBifactorRest <-" amab =~ BFI_8 + BFI_13 + BFI_16 + BFI_28
                       cons =~ BFI_27 + BFI_41 + BFI_15 + BFI_32 
                       DS =~ 1*BFI_8 + 1*BFI_13 + -1*BFI_16 + -1*BFI_28 + 
                             1*BFI_15 + -1*BFI_27 + -1*BFI_32 + 1*BFI_41
                      
                      DS ~~ 0*amab
                      DS ~~ 0*cons
                               "
AFCBifactorRest<-sem(model=modeloBifactorRest, data=banco_selec)
summary(AFCBifactorRest, standardized=T)


#bifactor restrito 2 
modeloBifactorRest2 <-" amab =~ BFI_8 + BFI_13 + BFI_16 + BFI_28
                       cons =~ 1*BFI_41 + -1*BFI_27 +  BFI_15 + BFI_32 
                       DS =~ 1*BFI_8 + 1*BFI_13 + -1*BFI_16 + -1*BFI_28 + 
                             1*BFI_15 + -1*BFI_27 + -1*BFI_32 + 1*BFI_41
                      
                      DS ~~ 0*amab
                      DS ~~ 0*cons
                               "
AFCBifactorRest2<-sem(model=modeloBifactorRest2, data=banco_selec)
summary(AFCBifactorRest2, standardized=T)



#bifactor restrito  
modeloBifactorRest3 <-" amab =~ BFI_8 + BFI_13 + BFI_16 + BFI_28
                       cons =~ BFI_32 + L41*BFI_41 + L27*BFI_27 +  L15*BFI_15 
                       DS =~ 1*BFI_8 + 1*BFI_13 + -1*BFI_16 + -1*BFI_28 + 
                             1*BFI_15 + -1*BFI_27 + -1*BFI_32 + 1*BFI_41
                      DS ~~ 0*amab
                      DS ~~ 0*cons
                    
                      #constraints
                      L41 < 1
                      L27 < 0
                      L15 < 0"
AFCBifactorRest3<-sem(model=modeloBifactorRest3, data=banco_selec)
summary(AFCBifactorRest3, standardized=T)





#Curso de "Psicometria e controle de viés de resposta: desejabilidade social, 
#aquiescência e respostas extremas"
#Profs. Felipe Valentini e Nelson Hauck
#10º Congresso Brasileiro de Avaliação Psicológica, 30 de junho de 2021

#Esta sintaxe contém os códigos utilizados durante o curso. São comandos 
#básicos que auxiliarão pesquisadoras e pesquisadores a conduzirem seus
#próprios estudos psicométricos de controle de vieses de resposta. O foco
#desta sintaxe é no controle da aquiescência e das respostas extremas.

#Pacotes a serem utilizados
library(lavaan)
library(mirt)
library(psych)
library(dplyr)

#Dados a serem utilizados neste exercício: 25 itens do instrumento
#Big Five Inventory, respondidos por 2.800 indivíduos, em uma escala 
#de concordância de seis pontos
data("bfi")#load the data
View(bfi)
bfi<-as.data.frame(apply(bfi[,1:25],2,as.numeric))

#Análise fatorial dos itens
fit_raw<-fa(r=bfi[,1:25],nfactors=5,fm="ml",rotate="oblimin",cor="poly")
print(fit_raw, sort=FALSE)

#AQUIESCÊNCIA
#Método 1: parcialização dos escores
#Selecionar pares com base em correlações negativas e conteúdo
corr.test(bfi[,21:25])
#Foram escolhidos os seguintes pares opostos
#A1,A2,C2,C4,C3,C5,E2,E4,E1,E3,O3,O5
#Criar índice de aquiescência
bfi$acq=((bfi$A1+bfi$A2+bfi$C2+bfi$C4+bfi$C3+bfi$C5+bfi$E2+bfi$E4+bfi$E1+bfi$E3+bfi$O3+bfi$O5)/12)
describe(bfi$acq)
bfi_acq_corrected <- partial.r(bfi, x = 1:25, y = 26, use="pairwise", method="spearman")
bfi_acq_corrected<-as.data.frame(apply(bfi_acq_corrected,2,as.numeric))

fit_par<-fa(r=bfi_acq_corrected,nfactors=5,n.obs=2800,fm="ml",rotate="oblimin")
print(fit_par, sort=FALSE)

#Método 2: modelo fatorial de interceptos randômicos
modelo_bfi_ARS='
E=~E1+E2+E3+E4+E5
A=~A1+A2+A3+A4+A5
C=~C1+C2+C3+C4+C5
N=~N1+N2+N3+N4+N5
O=~O1+O2+O3+O4+O5
ARS=~1*E1+1*E2+1*E3+1*E4+1*E5+1*A1+1*A2+1*A3+1*A4+1*A5+
1*C1+1*C2+1*C3+1*C4+1*C5+1*N1+1*N2+1*N3+1*N4+1*N5+
1*O1+1*O2+1*O3+1*O4+1*O5
ARS~~ARS
ARS~~0*E
ARS~~0*A
ARS~~0*C
ARS~~0*N
ARS~~0*O
'
fit_bfi_ARS=cfa(modelo_bfi_ARS, data = bfi,ordered = names(bfi))
summary(fit_bfi_ARS, standardized = TRUE, fit.measures = TRUE)


#RESPOSTAS EXTREMAS
#Criar, a partir do banco de dados, novos indicadores dicotômicos de 
#respostas extremas
extreme<-apply(bfi[,1:25],2,function (x) recode(x,`1`=1,`6`=1,`2`=0,`3`=0,`4`=0,`5`=0))
extreme<-as.data.frame(apply(extreme[,1:25],2,as.numeric))
ext <- grepl("[1-5]",names(extreme)); names(extreme)[ext] <- paste0(names(extreme)[ext],"_ext")
ext<-cbind(bfi[,1:25],extreme)#banco com itens originais + itens recodificados
ext$ext<-rowSums(ext[,26:50])#contagem de respostas extremas

#Método 1: parcialização dos escores
bfi_ext_corrected <- partial.r(ext, x = 1:25, y = 51, use="pairwise", method="spearman")
bfi_ext_corrected<-as.data.frame(apply(bfi_ext_corrected,2,as.numeric))

fit_ext<-fa(r=bfi_ext_corrected,nfactors=5,n.obs=2800,fm="ml",rotate="oblimin")
print(fit_ext, sort=FALSE)

#Método 2: modelagem multidimensional
#O ideal é utilizar variáveis dicotomizadas que não sejam as mesmas
#do modelo de mensuração da variável latente. Por isso, o exemplo a seguir
#utiliza apenas o controle de respostas extremas no fator C tendo como
#indicadores de respostas extremas os itens dicotomizados dos demais fatores
model_C_ERS='
C=~C1+C2+C3+C4+C5
ERS=~A1_ext+A2_ext+A3_ext+A4_ext+A5_ext+
E1_ext+E2_ext+E3_ext+E4_ext+E5_ext+
N1_ext+N2_ext+N3_ext+N4_ext+N5_ext+
O1_ext+O2_ext+O3_ext+O4_ext+O5_ext+
C1+C2+C3+C4+C5
ERS~~ERS
ERS~~0*C
'

fit_C_ERS=cfa(model_C_ERS, data = ext,ordered = names(ext))
summary(fit_C_ERS, standardized = TRUE, fit.measures = TRUE)


#CONTROLE CONJUNTO DE AQUIESCÊNCIA E RESPOSTAS EXTREMAS
#Método 1
model_C_ARS_ERS='
C=~C1+C2+C3+C4+C5
ARS=~1*C1+1*C2+1*C3+1*C4+1*C5
ARS~~ARS
ARS~~0*C
ERS=~A1_ext+A2_ext+A3_ext+A4_ext+A5_ext+
E1_ext+E2_ext+E3_ext+E4_ext+E5_ext+
N1_ext+N2_ext+N3_ext+N4_ext+N5_ext+
O1_ext+O2_ext+O3_ext+O4_ext+O5_ext+
C1+C2+C3+C4+C5
ERS~~ERS
ERS~~0*ARS
ERS~~0*C
'

fit_C_ARS_ERS=cfa(model_C_ARS_ERS, data = ext,ordered = names(ext))
summary(fit_C_ARS_ERS, standardized = TRUE, fit.measures = TRUE)

#Método 2: controle multidimensional utilizando scoring keys
#Modelo de base agora é o Generalized Partial Credits
#Definir os "scoring keys" que definem a relação entre os fatores e os itens
scoring<-list()
for(i in 1:5){
  scoring[[i]]<-matrix(
    c(1,2,3,4,5,6, # trait
      0,0,0,0,1,2,#ARS
      2,1,0,0,1,2# ERS
    ), 6, 3)
}

# Define the model: all items load on the three dimensions according to constraints
model<-"
 C = 1-5
 ARS = 1-5
 ERS = 1-5
 COV = C*0ARS
 COV = C*0ERS
 COV = ARS*0ERS
 "
# Estimate the multidimensional GPCM
fit<-mirt(bfi[,6:10],model,itemtype="gpcm",gpcm_mats=scoring,TOL = .0001,technical=list(NCYCLES=1000))
summary(fit)
coef(fit,simplify=TRUE, irt.parms = TRUE)

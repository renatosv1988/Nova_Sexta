# clear memory
rm(list=ls())
gc()

options(scipen = 999)

# libraries
library(foreign)
library(data.table)
library(DescTools)
library(MatchIt)
library(cobalt)
library(ggplot2)

# read OD data
df <- read.dbf("data/Banco2023_divulgacao_190225.dbf")


# contar dias da semana por pessoa

#criar uma base por pessoa
df <- as.data.table(df)
bi <- df[,.(DATA = first(DATA),
            DIA_SEM = first(DIA_SEM),
            FE_PESS = first(FE_PESS),
            viagens = max(N_VIAG),
            DISTANCIA = sum(DISTANCIA),
            RENDA_FA = first(RENDA_FA),
            IDADE = first(IDADE),
            ESTUDA = first(ESTUDA),
            GRAU_INS = first(GRAU_INS),
            ZONA = first(ZONA),
            MUNI_DOM = first(MUNI_DOM),
            CD_ATIVI = first(CD_ATIVI)),
         by=ID_PESS]

# check data subset (população da RMSP)
sum(bi$FE_PESS)

# criar uma base por viagem
bv <- df[N_VIAG>0,
         .(DATA = first(DATA),
           DIA_SEM = first(DIA_SEM),
           FE_VIA = first(FE_VIA),
           N_VIAG = first(N_VIAG)),
         by=ID_ORDEM]

# excluir viagens sem DIA_SEM
bv <- bv[DIA_SEM>0,]

# criar uma base de datas com o dia da semana mais comum
bd <- df[DIA_SEM>0,
         .(DIA_SEM_MODE = Mode(DIA_SEM)),
         by=c("DATA")]

# adicionar dia da semana modal a base de individuos
bi <- merge(bi, bd, by= "DATA", all.x = T)

# definir dia da semana ajustado
bi[, DIA_SEM_aj := fifelse(DIA_SEM>0, DIA_SEM, DIA_SEM_MODE)]
bv[, DIA_SEM_aj := DIA_SEM]


# tabela descritiva ---------------------------------------------
# agregar individuos por dia da semana
asi <- bi[,.(N_ENTR  =sum(FE_PESS)),by=DIA_SEM_aj]
# agregar viagens por dia da semana
asv <- bv[,.(N_VIAG  =sum(FE_VIA)),by=DIA_SEM_aj]
# juntar bases por dia da semana
bs <- merge(asi, asv, by="DIA_SEM_aj")
# calcular viagens por entrevista por dia da semana
bs$viag_entr <- bs$N_VIAG/bs$N_ENTR


# REGRESSÃO -----------------------------------------------------
m1 <- lm(viagens~ -1 + as.character(DIA_SEM_aj), data=bi, weights = bi$FE_PESS)
summary(m1)


m2 <- lm(viagens~ as.character(DIA_SEM_aj), data=bi, weights = bi$FE_PESS)
summary(m2)


m3 <- lm(DISTANCIA~ as.character(DIA_SEM_aj), data=bi, weights = bi$FE_PESS)
summary(m3)


# plot renda distancia
ggplot() +
  geom_point(aes(x=log(RENDA_FA), y=viagens),data=bi) +
  geom_smooth(aes(x=log(RENDA_FA), y=viagens),data=bi) +
  theme_classic()

m1 <- lm(viagens~ as.character(DIA_SEM_aj), data=bi, weights = bi$FE_PESS)
summary(m1)

# clear memory
rm(list=ls())
gc()

# libraries
library(foreign)
library(data.table)

# read OD data
df <- read.dbf("data/Banco2023_divulgacao_190225.dbf")

#criar uma base por pessoa
df <- as.data.table(df)
bi <- df[,.(DATA = first(DATA),
            FE_PESS = first(FE_PESS)),
         by=ID_PESS]

# check data subset (população da RMSP)
sum(bi$FE_PESS)

#criar uma base por pessoa
bv <- df[N_VIAG>0,
         .(DATA = first(DATA),
           DIA_SEM = first(DIA_SEM),
           FE_VIA = first(FE_VIA),
           N_VIAG = first(N_VIAG)),
         by=ID_ORDEM]


#criar uma base por data
bd <- df[,
         .(DIA_SEM = first(DIA_SEM)),
         by=DATA]


# adicionar dia da semana a base de individuos
bi <- merge(bi, bd, by= "DATA", all.x = T)


# agregar individuos por dia da semana
asi <- bi[,.(N_ENTR  =sum(FE_PESS)),by=DIA_SEM]

# agregar viagens por dia da semana
asv <- bv[,.(N_VIAG  =sum(FE_VIA)),by=DIA_SEM]

options(scipen = 999)













df$N_VIAG[1:20]









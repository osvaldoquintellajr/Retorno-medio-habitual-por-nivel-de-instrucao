#Carregar os pacotes necessários----
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(arrow)

options(timeout = 10000000)
#Definir anos e trimestres a serem processados pelo código----
anos_a_processar <- c(2012:2026)
trimestres_a_processar <- c(1:4)

my_tibble_anos_trimestres <- expand_grid(anos_a_processar, trimestres_a_processar)


#Loop para fazer o download dos arquivos trimestrais e calcular o rendimento habitual por nível de instrução----

lista_pnad_completa_rendimento_habitual <- list()

for(i in 1:nrow(my_tibble_anos_trimestres)){
  
  ano <- my_tibble_anos_trimestres$anos_a_processar[i]
  
  trimestre <- my_tibble_anos_trimestres$trimestres_a_processar[i]
  
  tryCatch({
    
    cat(str_glue("Baixando microdados trimestrais de {ano}, T{trimestre}\n\n"))
    
    my_pnadc <- get_pnadc(
      year = ano, 
      quarter = trimestre,
      vars = c("Ano", "UF", "Capital", 
               "V1028", "V2007", "V2009", "V2010",
               "VD3004", 
               "VD4016"),
      labels = FALSE, 
      design = TRUE, 
      deflator = TRUE
    )
    
    my_pnadc$variables <- mutate(my_pnadc$variables, VD4016_real = VD4016*Habitual)
    
    my_pnadc$variables <- mutate(my_pnadc$variables, 
                                 VD3004 = case_when(
                                   VD3004 == "1" ~ "Sem instrução e menos de 1 ano de estudo",
                                   VD3004 == "2" ~ "Fundamental incompleto ou equivalente",
                                   VD3004 == "3" ~ "Fundamental completo ou equivalente",
                                   VD3004 == "4" ~ "Médio incompleto ou equivalente",
                                   VD3004 == "5" ~ "Médio completo ou equivalente",
                                   VD3004 == "6" ~ "Superior incompleto ou equivalente", 
                                   VD3004 == "7" ~ "Superior completo",
                                   .default = NA_character_
                                 )
    )
    
    resultados <- svyby(formula = ~VD4016_real, by = ~VD3004, 
                        design = my_pnadc, 
                        FUN = svymean, 
                        keep.names = FALSE, 
                        na.rm = TRUE)
    
    rm(my_pnadc)
    
    gc()
    
    chave_lista <- str_glue("{ano}_{trimestre}")
    
    lista_pnad_completa_rendimento_habitual[[as.character(chave_lista)]] <- resultados
    
    rm(resultados)
    
    gc()
    
  }, error = function(e){
    
    cat(str_glue("\n Erro ao baixar dados do servidor em {ano} T{trimestre}. \n"))
    
    return(NULL)
    
  })
  
}

lista_pnad_completa_rendimento_habitual <- bind_rows(lista_pnad_completa_rendimento_habitual, .id = "chave")

#Salva os arquivos no disco----
write_parquet(x = lista_pnad_completa_rendimento_habitual, 
              sink = "D:/Documentos/github/Retorno-medio-habitual-por-nivel-de-instrucao/lista_pnad_completa_rendimento_habitual.parquet")


#Baixar dados anuais para análise econométrica----
anos_a_processar <- 2012:2026

lista_pnad_completa_regressao <- list()

for(ano in anos_a_processar){
  
  tryCatch({
    
    cat(str_glue("Baixando microdados de {ano}\n\n"))
    
    my_pnadc <- get_pnadc(
      year = ano,
      interview = 1,
      vars = c("Ano", "UF", "Capital", "UPA", "Estrato",
               "V1032", #peso
               "V2007", #sexo
               "V2009", #Idade
               "V2010", #Cor ou raça
               "VD3004", #Nìvel de instrução
               "VD4016", #Rendimento habitual médio do trabalho principal
               "VD4009"), #posição na ocupação
      labels = FALSE, 
      design = FALSE, 
      deflator = TRUE
    )
    
    lista_pnad_completa_regressao[[as.character(ano)]] <- my_pnadc
    
    rm(my_pnadc)
    
    gc()
    
  }, error = function(e){
    
    cat(str_glue("\n Erro ao baixar dados do servidor em {ano}. \n"))
    
    return(NULL)
    
  })
  
}

pnad_completa_regressao <- bind_rows(lista_pnad_completa_regressao) 

rm(lista_pnad_completa_regressao)

gc()

write_parquet(x = pnad_completa_regressao, sink = "D:/Documentos/github/Retorno-medio-habitual-por-nivel-de-instrucao/pnad_completa_regressao.parquet")

#Carregar os pacotes necessários----
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(arrow)
library(broom)

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
               "VD4016", "VD4002"),
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
anos_a_processar <- 2012:2025

lista_pnad_completa_coeficientes <- list()

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
               "VD4009",#posição na ocupação 
               "VD4002"),  #Condição de ocupação  
      labels = FALSE, 
      design = FALSE,
      deflator = TRUE
    )
    
    my_pnadc_design <- pnadc_design(my_pnadc)
      
    my_pnadc_design$variables <- mutate(my_pnadc_design$variables, 
                                                       VD4016_real = VD4016*CO1,
                                                       log_renda = log(VD4016_real),
                                                       MV2007 = as_factor(case_when(
                                                         V2007 == "1" ~ "Masculino",
                                                         V2007 == "2" ~ "Feminino"
                                                       )),
                                                       MV2007 = fct_relevel(MV2007, "Masculino"),
                                                       MVD3004 = as_factor(case_when(
                                                         VD3004 %in% c("1", "2", "3", "4", "5", "6") ~ "Sem_ensino_superior",
                                                         VD3004 == "7" ~ "Superior_completo",
                                                         .default = NA_character_
                                                       )),
                                                       MVD3004 = fct_relevel(MVD3004, "Sem_ensino_superior"),
                                                       MV2010 = as_factor(case_when(
                                                         V2010 == "1" ~ "Branca",
                                                         V2010 == "2" ~ "Preta",
                                                         V2010 == "3" ~ "Amarela",
                                                         V2010 == "4" ~ "Parda",
                                                         V2010 == "5" ~"Indigena",
                                                         .default = NA_character_
                                                       )),
                                                       MV2010 = fct_relevel(MV2010, "Branca"),
                                                       MUF = as_factor(UF),
                                                       Ano = as_factor(Ano),
                                                       Ano = fct_relevel(Ano, "2012"),
                                                       MVVD4009 = as_factor(case_when(
                                                         VD4009 %in% c("05", "06", "07") ~ "Setor_publico", 
                                                         VD4009 %in% c("01", "02", "03", "04", "08", "09", "10") ~ "Setor_privado",
                                                         .default = NA_character_
                                                       )),
                                                       MVVD4009 = fct_relevel(MVVD4009, "Setor_privado")
                                        )
    
    my_pnadc_design$variables <- rename(my_pnadc_design$variables, 
                                        Idade = V2009,
                                        sexo = MV2007,
                                        cor_raca = MV2010,
                                        Ensino_superior = MVD3004,
                                        Unidade_federacao = MUF,
                                        Setor_publico = MVVD4009)
                         
                         
    invisible(gc())
    
    design_valido <- subset(my_pnadc_design, 
                            VD4002 == "1" &
                              !is.na(VD4016_real) &
                              VD4016_real > 0)
    
    options(survey.lonely.psu = "adjust")
    
    my_model <- svyglm(formula = log_renda ~ Ensino_superior + 
                         Idade + I(Idade^2) + sexo + cor_raca + Setor_publico + Unidade_federacao, 
                       design = design_valido)
    
    invisible(gc())
    
    tabela_coef <- tidy(my_model, conf.int = TRUE) 
    
    lista_pnad_completa_coeficientes[[as.character(ano)]] <- tabela_coef
    
    rm(my_pnadc, my_pnadc_design, design_valido, tabela_coef, my_model)
    
    gc()
    
  }, error = function(e){
    
    cat(str_glue("\n Erro ao baixar dados do servidor em {ano}. \n"))
    
    return(NULL)
    
  })
  
}

pnad_completa_coeficientes <- bind_rows(lista_pnad_completa_coeficientes, .id = "ano")


rm(lista_pnad_completa_coeficientes)

gc()

write_parquet(x = pnad_completa_coeficientes, sink = "D:/Documentos/github/Retorno-medio-habitual-por-nivel-de-instrucao/pnad_completa_coeficientes.parquet")

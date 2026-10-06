# Retorno-medio-habitual-por-nivel-de-instrucao

**Análise da evolução do prêmio salarial do ensino superior (2012-2025) com microdados da PNAD Contínua (IBGE). **

## Sobre o projeto

Estre projeto analia o **rendimento habitual médio real** por nível de instrução no Brasil e estima o **prêmio salarial do ensino superior** entre os anos de 2012 e 2025. Utiliza microdados da **Pesquisa Nacional por Amostra de Domicílios Contínnua (PNAD Contínua)**, do IBGE.

## Principais achados

-O prêmio salarial do ensino superior **permanece substancialmente positivo** em todo o período.

-Os resultados foram obtidos por **estimativas anuais independentes** com desenho amostral complexo da PNADc.

## Metodologia

### Dados

-**Fonte:** PNAD Contínua (IBGE), 1ª visita, 2012-2025.

-**Rendimento:** `VD4006` (rendimento habitual médio do trabalho principal).

-**Deflator:** CO2 (preços médios do último ano disponível).

-**Plano amostral:** A criação do objeto do plano amostral foi feita diretamente pela função pnadc_design.

## Estratégia empírica

- **Descritiva (trimestral):** rendimento médio real por nível de instrução, 

- **Econométrica (anual):** equação minceriana estimada separadamente para cada ano da análise:

   $$\log(Y_i) = \alpha + \beta \cdot \text{Superior}_i + \gamma X_i + \varepsilon_i$$
   
   em que: $$X_i$$ é um vetor de controles que incluem: indade, idade², sexo, cor/raça, setor de atividade e unidade da federação.
   


https://osvaldoquintellajr.github.io/Retorno-medio-habitual-por-nivel-de-instrucao


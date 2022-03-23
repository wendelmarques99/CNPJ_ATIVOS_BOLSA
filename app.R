
# CNPJ - BOLSA ------------------------------------------------------------


# Librarys ----------------------------------------------------------------
library(magrittr)

cnpj_banco_B3 <- "00.997.185/0001-50"
cnpj_banco_Bradesco <- "60.746.948/0001‐12" 
cnpj_banco_Itau <- "60.701.190/0001-04 "


# CNPJ - BDR --------------------------------------------------------------
BDR_CNPJ <- rvest::read_html("https://informederendimentos.com/cnpj-bdrs/") %>% 
  rvest::html_table() %>% .[[1]] %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(instituicao_depositaria = 
                  dplyr::case_when(
                    instituicao_depositaria == "Banco B3" ~ cnpj_banco_B3, 
                    instituicao_depositaria == "Banco Bradesco" ~ cnpj_banco_Bradesco, 
                    instituicao_depositaria == "Banco Itaú" ~ cnpj_banco_Itau, 
                    
                  )) %>% 
  dplyr::mutate(codigo_bdr_empresa = stringr::word(codigo_bdr_empresa, 1)) %>% 
  dplyr::rename("codigo" = "codigo_bdr_empresa")

# CNPJ - FII --------------------------------------------------------------
FII_CNPJ <- rvest::read_html("https://informederendimentos.com/cnpj-dos-fiis-e-administradoras-de-fundos-de-investimento-imobiliario/") %>% 
  rvest::html_table() %>% .[[1]] %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(fii = stringr::word(fii, 1)) %>% 
  dplyr::rename("codigo" = "fii")

# CNPJ - AITVOS B3 --------------------------------------------------------
ATIVO_B3 <- rvest::read_html("https://informederendimentos.com/cnpj-das-empresas-da-bolsa/") %>% 
  rvest::html_table() %>% .[[1]] %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(tamanho_da_palavra = stringr::str_count(codigo, "\\w+")) %>% 
  dplyr::mutate(coluna_resto2 = ifelse(tamanho_da_palavra >= 2, 
                               stringr::word(codigo, 2, sep = "-"), NA)) %>% 
  dplyr::mutate(coluna_resto3 = ifelse(tamanho_da_palavra == 3, 
                                       stringr::word(codigo, 3, sep = "-"), NA)) %>% 
  dplyr::mutate(codigo = stringr::word(codigo, 1, sep = "-")) %>% 
  dplyr::select(-c(tamanho_da_palavra))



ATIVO_B3_resto1 <- tibble::tibble(codigo = ATIVO_B3$coluna_resto2, 
               empresa = ATIVO_B3$empresa, 
               cnpj = ATIVO_B3$cnpj) %>% 
  na.omit()


ATIVO_B3_resto3 <- tibble::tibble(codigo = ATIVO_B3$coluna_resto3, 
                                  empresa = ATIVO_B3$empresa, 
                                  cnpj = ATIVO_B3$cnpj) %>% 
  na.omit()


ATIVO_B3 <- dplyr::bind_rows(ATIVO_B3 %>% 
                dplyr::select(-c(coluna_resto2, coluna_resto3)), 
                ATIVO_B3_resto1, ATIVO_B3_resto3) 

# Juntando todos os dfs ---------------------------------------------------
dplyr::bind_rows(
  BDR_CNPJ, FII_CNPJ, ATIVO_B3
) %>% 
  dplyr::mutate(pais_bolsa = 
                  ifelse(is.na(pais_bolsa), "Brasil - B3", pais_bolsa))


#' Decodifica o arquivo EXPORTA BPA
#'
#' @return Dois arquivos: BPAC e BPAI
#' @export
Decodificaramb <- function() {

  arquivo <- list.files("../../3 - DADOS/EXPORTA BPA/", pattern = ".txt")
  filename <- stringr::str_c("../../3 - DADOS/EXPORTA BPA/", arquivo)

  conn <- file(filename,open="r")
  linn <- readLines(conn)
  close(conn)

  # Descrição Consolidado
  character1 <- c("prd-ident","prd-cnes","prd-cmp","Prd_cbo","prd-flh","prd-seq","prd-pa","prd-ldade","prd-qt","prd-org")
  formato1 <- c("I2","I7","A6","A6","I3","I2","A10","I3","I6","A3")

  # Descrição Individualizado
  character2 <- c("prd-ident","prd-cnes","prd-cmp","Prd_cnsmed","Prd_cbo","Prd_dtaten","prd-flh","prd-seq","prd-pa","Prd-cnspac","Prd-sexo","Prd-ibge","Prd-cid","prd-ldade","prd-qt","Prd-caten","Prd-naut","prd-org","prd-nmpac","prd-dtnasc","prd-raca","prd-etnia","prd-nac","prd_SRV","prd_CLF","prd_equipe_Seq","prd_equipe_Area","prd_cnpj","prd_cep_pcnte","prd_lograd_pcnte","prd_end_pcnte","prd_compl_pcnte","prd_num_pcnte","prd_bairro_pcnte","prd_ddtel_pcnte","prd_email_pcnte")
  formato2 <- c("I2","I7","A6","A15","A6","A8","I3","I2","I10","A15","A1","I6","A4","I3","I6","I2","A13","A3","A30","A8","I2","A4","I3","I3","I3","I8","I4","I14","I8","A3","A30","A10","A5","A30","A11","A40")


  key <- c(0,0)
  for (ii in 2:length(linn)){

    # Seleciona a linha
    ll <- gsub("\t", " ",linn[ii])

    # Identifica qual é o layout da linha (ID02 - BPA Consolidado / ID03 - BPA Individualizado)
    ID02 <- substring(linn[ii],01,02)=="02"
    ID03 <- substring(linn[ii],01,02)=="03"

    # Cria um arquivo temporário para a leitura dos dados
    TmpFile01 <- tempfile()
    cat(file = TmpFile01,ll,sep = "\n")

    if (ID02){
      temp_BPAC <- read.fortran(TmpFile01,col.names = character1, format = formato1)
      if (key[1]==0){
        key[1] <- 1
        BPAC <- temp_BPAC
      } else {
        BPAC <- rbind(BPAC,temp_BPAC)
      }
    }

    if (ID03){
      temp_BPAI <- read.fortran(TmpFile01,col.names = character2, format = formato2)
      if (key[2]==0){
        key[2] <- 1
        BPAI <- temp_BPAI
      } else {
        BPAI <- rbind(BPAI,temp_BPAI)
      }
    }

    unlink(TmpFile01)
  }

  # Escrevendo os arquivos
  if (key[1] == 1)
  {
    writexl::write_xlsx(BPAC, path = stringr::str_c("../../3 - DADOS/DECODIFICAÇÕES AMB/1 - BPAC/", stringr::str_sub(arquivo, end = 10), "_BPAC.xlsx"))
  }

  if (key[2] == 1)
  {
    writexl::write_xlsx(BPAI, path = stringr::str_c("../../3 - DADOS/DECODIFICAÇÕES AMB/2 - BPAI/", stringr::str_sub(arquivo, end = 10), "_BPAI.xlsx"))
  }
}

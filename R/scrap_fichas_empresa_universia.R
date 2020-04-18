#' @title Scrapea el directorio de empresas de Universia.
#'
#' @description Scrapea el directorio de empresas de Universia devolviendo un json resultado y envíandolo a la plataforma Smart City.
#'
#' @param municipio, provincias
#'
#' @return json
#'
#' @examples scrap_fichas_empresas_universia('Camargo', 'Cantabria')
#'
#' @import jsonlite
#' magrittr
#' xml2
#' rvest
#' lubridate
#' httr
#' stringr
#' anytime
#'
#' @export

scrap_fichas_empresas_universia <- function(municipio, provincias){

  print("Inicio")

  provincias <- toupper(provincias)
  municipios <- toupper(municipio)

  url_general <- "https://guiaempresas.universia.es/localidad/"
  url_empresa_gen <- "https://guiaempresas.universia.es"


  #======================================================
  # Paginación

  url <- paste(url_general,municipios,"-",provincias, "/?qPagina=1",sep = "")
  html_inicial <- read_html(url)

  Sys.sleep(20)

  # max pag
  pags <- html_inicial %>% html_nodes(".pagination") %>% html_text() %>% str_trim() %>% unlist()
  pag_final <- pags %>% gsub(".*[...]","",.) %>% gsub("»","",.) %>% as.numeric()
  #======================================================


  # DECLRACIÓN VECTORES DATOS
  empresa <- c()
  direccion <- c()
  telefono <- c()
  cnae <- c()
  objeto_social <- c()
  fecha_creacion <- c()
  ventas <- c()
  empleados <- c()

  Sys.sleep(2)

  print("Entrando en bucle for")

  for(i in 1:pag_final){
    print("i")
    print(i)

    Sys.sleep(15)

    #======================================================
    # Links empresas

    url <- paste(url_general,municipios,"-",provincias, "/?qPagina=", i,sep = "")


    empresa_link <- NA
    while(is.na(empresa_link)){
      html_general <- read_html(url)

      nombre_empresa <- html_general %>% html_nodes(".ranking_einf .textleft") %>% html_text() %>% str_trim() %>% unlist()
      nombre_empresa <- nombre_empresa[-1]

      empresa_link <- html_general %>% html_nodes(xpath = "//td[contains(@class,'textleft')]/a") %>% html_attr("href")
      empresa_link <- empresa_link[1:length(nombre_empresa)]

      Sys.sleep(20)
    }


    #======================================================

    for(j in 1:length(empresa_link)){
      print("j")
      print(j)

      Sys.sleep(15)

      #======================================================
      #EMPRESA

      #url_empresa <- paste(url_empresa_gen, empresa_link)
      url_empresa <- paste(url_empresa_gen, empresa_link[j], sep = "")

      html_empresa <- read_html(url_empresa)

      datos_ref <- html_empresa %>% html_nodes(xpath = "//tr/th[contains(@class,'td_ficha_univ')]") %>% html_text() %>% str_trim() %>% unlist()
      datos <- html_empresa %>% html_nodes(xpath = "//tr/td[contains(@class,'td_ficha_univ')]") %>% html_text() %>% str_trim() %>% unlist()

      if(identical(datos,character(0))){
        next
      }

      tryCatch({
        empresa1 <-html_empresa %>% html_nodes(xpath = "//h1[contains(@itemprop,'name')]") %>% html_text() %>% str_trim() %>% unlist()
        empresa <- c(empresa, ifelse(identical(empresa1, character(0)), "-", empresa1))
      },error = function(e){
        empresa <<- "-"
      })
      tryCatch({
        direccion1 <- html_empresa %>% html_nodes(xpath = "//span[contains(@class,'street-address')]") %>% html_text() %>% str_trim() %>% unlist()
        direccion <- c(direccion, ifelse(identical(direccion1, character(0)), "-", direccion1))
      },error = function(e){
        direccion <<- "-"
      })
      tryCatch({
        telefono1 <- html_empresa %>% html_nodes(xpath = "//td[contains(@itemprop,'telephone')]") %>% html_text() %>% str_trim() %>% unlist()
        telefono <- c(telefono, ifelse(identical(telefono1, character(0)), "-", telefono1))
      },error = function(e){
        telefono <<- "-"
      })
      tryCatch({
        cnae1 <- datos[grep("CNAE",datos_ref, ignore.case = T)]
        cnae <- c(cnae, ifelse(identical(cnae1, character(0)), "-", cnae1))
      },error = function(e){
        cnae <<- "-"
      })
      tryCatch({
        objeto_social1 <- html_empresa %>% html_nodes(xpath = "//span[contains(@id,'span_objsoc_2')]") %>% html_text() %>% str_trim() %>% unlist()
        objeto_social <- c(objeto_social, ifelse(identical(objeto_social1, character(0)), "-", objeto_social1))
      },error = function(e){
        objeto_social <<- "-"
      })
      tryCatch({
        fecha_creacion1 <- datos[grep("Fecha de creación:",datos_ref, ignore.case = T)]
        fecha_creacion <- c(fecha_creacion, ifelse(identical(fecha_creacion1, character(0)), "-", fecha_creacion1))
      },error = function(e){
        fecha_creacion <<- "-"
      })

      #texto_ficha <- html_empresa %>% html_nodes(xpath = "//div[contains(@id,'texto_ficha')]") %>% html_text() %>% str_trim() %>% unlist()
      #texto_ficha_split <- str_split(texto_ficha,"[\t\t\t]")

      #Ventas
      tryCatch({
        ventas1 <- html_empresa %>% html_nodes(xpath = "//p[contains(@id,'bloque-ventas')]") %>% html_text() %>% str_trim() %>% unlist()
        ventas1 <- gsub(".*: ","",ventas1)
        ventas <- c(ventas, ifelse(identical(ventas1, character(0)), "-", ventas1))
      },error = function(e){
        ventas <<- "-"
      })

      #frase_ventas <- texto_ficha_split[[1]][grep("Las ventas",texto_ficha_split[[1]])]
      #pos_interes_ventas <-  gregexpr('\"', frase_ventas)
      #ventas <- c(ventas, substring(frase_ventas,pos_interes_ventas[[1]][1]+1,pos_interes_ventas[[1]][2]-1))

      #Empleados
      tryCatch({
        empleados1 <- html_empresa %>% html_nodes(xpath = "//p[contains(@id,'bloque-empleados')]") %>% html_text() %>% str_trim() %>% unlist()
        empleados1 <- gsub(".*: ","",empleados1)
        empleados <- c(empleados, ifelse(identical(empleados1, character(0)), "-", empleados1))
      },error = function(e){
        empleados <<- "-"
      })

      #frase_empleados <- texto_ficha_split[[1]][grep("tamaño",texto_ficha_split[[1]])]
      #pos_interes_empleados <-  gregexpr('\"', frase_empleados)
      #empleados <- c(empleados, substring(frase_empleados,pos_interes_empleados[[1]][1]+1,pos_interes_empleados[[1]][2]-1))

      #======================================================
    }
  }


  longitud <- c()
  latitud <- c()
  url_geocod <- "https://geocoder.api.here.com/6.2/geocode.json?app_id=LeZNjDC1ce9FDIky92FJ&app_code=yAhBxFxOsIQkZCXk8fhXAA&searchtext="
  for(i in 1:length(direccion)){
    print(i)
    coordenadas <- jsonlite::fromJSON(paste(url_geocod, URLencode(direccion[i]),"%20",URLencode(as.character(municipios)),"%20(Espa%C3%B1a)",sep = ""))
    coordenadas <- coordenadas$Response$View$Result %>% as.data.frame()
    longitud1 <- coordenadas$Location$DisplayPosition$Longitude
    latitud1 <- coordenadas$Location$DisplayPosition$Latitude

    print(paste(url_geocod, URLencode(direccion[i]),municipios,"%20(Espa%C3%B1a)",sep = " "))

    longitud <- c(longitud, ifelse(is.null(longitud1[1]),"-",longitud1[1]))
    latitud <- c(latitud, ifelse(is.null(latitud1[1]),"-",latitud1[1]))
  }


  #Generación DF
  df_fichas <- data.frame(empresa, direccion, latitud, longitud, telefono, cnae, objeto_social, fecha_creacion, ventas, empleados, stringsAsFactors = F)
  names(df_fichas) <- c("Empresa", "Dirección", "Latitud", "Longitud", "teléfono", "CNAE", "Objeto social", "Fecha creación", "Ventas", "Empleados")

  #===============================================================
  # CREACIÓN JSON Y ENVÍO A PLATAFORMA SMART CITY
  #===============================================================

  #Variables envío JSON a plataforma
  TB_token <- "KeEzsCYrm5sCbENbwgJz"
  TB_url   <- paste("http://78.47.39.122:8080/api/v1/",TB_token,"/telemetry",sep="")

  json_fichas_return <- toJSON(df_fichas,pretty=T)

  #Extracción timestamp en formato unix
  tsi <- format(as.numeric(anytime(Sys.Date()))*1000,scientific = F)
  #tsi <- sub("\\..*", "",tsi)
  for(i in 1:nrow(df_fichas)){
    ts <- as.numeric(tsi) +i  #Añade i ms al timestamp para poder verse sin solapamiento en el widget de la plataforma smart city.

    #Creación de JSON noticias y eliminación de ][ para cumplir con el formato json con modificación de timestamp de thingsboard.
    json_fichas <- toJSON(df_fichas[i,],pretty=T)
    json_fichas <- sub("[[]","",json_fichas)
    json_fichas <- sub("[]]","",json_fichas)

    #Formato json con modificación de timestamp de thingsboard.
    json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_fichas,"}",sep="")

    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)
  }

  return(json_fichas_return)

}







#' @title Scrapea el directorio de empresas de Expansión online y enriquece los datos extraídos con datos del Economista
#'
#' @description Scrapea el directorio de empresas de Expansión online
#'  y enriquece el contenido mediante la extracción de datos del Economista online
#'  devolviendo un json resultado y envíandolo a la plataforma Smart City.
#'
#' @param municipio, provincias
#'
#' @return json
#'
#' @examples scrap_fichas_empresas('Camargo', 'Cantabria')
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

scrap_fichas_empresas <- function(municipio, provincias){
  #Funciones
  #===========================================================================

  checkvar <- function(texto_input)
  {
    texto_output <- texto_input
    if(identical(texto_output,NULL)){texto_output<-"-"}
    if(identical(texto_output,character(0))){texto_output<-"-"}
    if(identical(texto_output,numeric(0))){texto_output<-"-"}
    if(is.na(texto_output)){texto_output<-"-"}
    return(texto_output)
  }

  #Configuración
  #===========================================================================

  #provincias <- c("cantabria")
  #municipios <- c("CAMARGO")
  provincias <- tolower(provincias)
  municipios <- toupper(municipio)
  iteracion  <- 100
  url_geocod <- "https://geocoder.api.here.com/6.2/geocode.json?app_id=LeZNjDC1ce9FDIky92FJ&app_code=yAhBxFxOsIQkZCXk8fhXAA&searchtext="

  #Lectura de subcapítulos de actividad de directorio de empresas de Expansión
  #===========================================================================

  web <- read_html("https://www.expansion.com/directorio-empresas.html")
  actividades <- web %>% html_nodes(xpath = "//ul[contains(@class,'subcategorias')]/li/a") %>% html_attr("href")
  actividades <- gsub("index.html","",actividades)

  #Lectura de todas las empresas dadas de alta
  #===========================================================================

  iteraciones <- 1:iteracion
  iteraciones[1] <- "index"
  iteraciones <- paste(iteraciones,".html",sep="")

  cabeceras <- NULL

  for(i in 1:length(actividades))
  {
    print(i)
    for(j in 1:length(iteraciones))
    {

      cabecera <- NULL #se reinicia cada ciclo para no re-copiar

      link <- paste(actividades[i],provincias,"/",iteraciones[j],sep="")

      error_404 <-FALSE
      error_404 <- tryCatch(web2 <- read_html(link),error=function(e){"error"})  #Se comprueba que la URL buscada no da error para, en su caso, break-ear el fo
      if(error_404[1]=="error") next()

      var_cap <- web2 %>% html_nodes(xpath="//div[contains(@class,'cabecera-seccion')]/h1") %>% html_text()
      if(length(var_cap) == 0){
        var_cap <- NA
      }
      var_emp <- web2 %>% html_nodes(xpath="//ul/li/ul/li[contains(@class,'col1')]/a") %>% html_text()
      var_url <- web2 %>% html_nodes(xpath="//ul/li/ul/li[contains(@class,'col1')]/a") %>% html_attr("href")
      var_mun <- web2 %>% html_nodes(xpath="//ul/li/ul/li[contains(@class,'col2')]") %>% html_text()
      var_prv <- web2 %>% html_nodes(xpath="//ul/li/ul/li[contains(@class,'col3')]") %>% html_text()

      reset <- web2 %>% html_nodes(xpath="//li/a[contains(@class,'siguiente')]") %>% html_attr("href")
      #if(identical(reset,character(0))&&j>500){break()}

      cabecera <- cbind(link,var_emp,var_url,var_mun,var_prv) %>% as.data.frame()
      cabecera$var_lnk <- link
      cabecera$var_cap <- var_cap
      cabeceras <- rbind(cabeceras,cabecera)

      print(link)

    }
  }

  #write.table(cabeceras,"d:/CAMARGO_EMPRESAS/cantabria.csv",sep=";",dec=",",row.names = FALSE)




  #Selección de empresas y enriquecimiento de datos Expansion
  #===========================================================================

  seleccion <- cabeceras %>% subset(cabeceras$var_mun %in% municipios)

  seleccion$var_n_empleados    <- "-"
  seleccion$var_facturacion    <- "-"
  seleccion$var_fo_juridica    <- "-"
  seleccion$var_sector_empr    <- "-"
  seleccion$var_direccion      <- "-"
  seleccion$var_tamanyo        <- "-"
  seleccion$var_constitucion   <- "-"
  seleccion$var_telefono       <- "-"
  seleccion$var_actividad      <- "-"
  seleccion$var_cnae           <- "-"
  seleccion$var_ultimo_depo    <- "-"
  seleccion$var_capital_soc    <- "-"
  seleccion$var_ultimo_camb    <- "-"
  seleccion$var_longitud       <- "-"
  seleccion$var_latitud        <- "-"
  seleccion$var_empresite_cif  <- "-"
  seleccion$var_empresite_tel  <- "-"
  seleccion$var_empresite_fax  <- "-"
  seleccion$var_empresite_dom  <- "-"
  seleccion$var_empresite_lat  <- "-"
  seleccion$var_empresite_lon  <- "-"
  seleccion$var_empresite_eml  <- "-"
  seleccion$var_empresite_web  <- "-"
  seleccion$var_empresite_obj  <- "-"
  seleccion$var_empresite_vts  <- "-"
  seleccion$var_bing_url       <- "-"

  for(i in 312:nrow(seleccion))
  {
    link <- seleccion$var_url[i] %>% as.character()

    print(nrow(seleccion)-i)
    print(link)

    error_404 <-FALSE
    error_404 <- tryCatch(web3 <- read_html(link),error=function(e){"error"})  #Se comprueba que la URL buscada no da error para, en su caso, break-ear el fo
    if(error_404[1]=="error") next()

    tabla <- web3 %>% html_table()
    tabla <- tabla[1] %>% data.frame()

    seleccion$var_n_empleados[i]  <- tabla[grep("EMPLEADOS",tabla[,1]),2]               %>% checkvar()
    seleccion$var_facturacion[i]  <- tabla[grep("FACTURACION",tabla[,1]),2]             %>% checkvar()
    seleccion$var_fo_juridica[i]  <- tabla[grep("FORMA JUR",tabla[,1]),2]               %>% checkvar()
    seleccion$var_sector_empr[i]  <- tabla[grep("SECTOR DE LA EMPRESA",tabla[,1]),2]    %>% checkvar()
    seleccion$var_direccion[i]    <- tabla[grep("DIRECC",tabla[,1]),2]                  %>% checkvar()
    seleccion$var_tamanyo[i]      <- tabla[grep("TAMA",tabla[,1]),2]                    %>% checkvar()
    seleccion$var_constitucion[i] <- tabla[grep("FECHA DE CONSTITU",tabla[,1]),2]       %>% checkvar()
    seleccion$var_telefono[i]     <- tabla[grep("FONO",tabla[,1]),2]                    %>% checkvar()
    seleccion$var_actividad[i]    <- tabla[grep("ACTIVIDAD",tabla[,1]),2]               %>% checkvar()
    seleccion$var_cnae[i]         <- tabla[grep("CNAE",tabla[,1]),2]                    %>% checkvar()
    seleccion$var_ultimo_depo[i]  <- tabla[grep("DE CUENTAS",tabla[,1]),2]              %>% checkvar()
    seleccion$var_ultimo_depo[i]  <- gsub(" » Acceder","",seleccion$var_ultimo_depo[i]) %>% checkvar()
    seleccion$var_capital_soc[i]  <- tabla[grep("CAPITAL SOCIAL",tabla[,1]),2]          %>% checkvar()
    seleccion$var_ultimo_camb[i]  <- tabla[grep("TIMO CAMBIO",tabla[,1]),2]             %>% checkvar()

    # Búsqueda de datos Empresite
    # ---------------------------

    link <- paste("https://www.bing.com/search?q=site%3Ahttps%3A%2F%2Fempresite.eleconomista.es+",seleccion$var_emp[i],sep="")
    link <- gsub(" ","%2b",link)

    error_404 <-FALSE
    error_404 <- tryCatch(web4 <- read_html(link),error=function(e){"error"})  #Se comprueba que la URL buscada no da error para, en su caso, break-ear el fo
    if(!error_404[1]=="error")
    {
      url <- web4 %>% html_nodes(xpath="//li[contains(@class,'b_algo')][1]/h2/a") %>% html_attr("href")
    }

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

    error_404 <-FALSE
    error_404 <- tryCatch(web5 <- html_session(url, user_agent(uastring)),error=function(e){"error"})  #Se comprueba que la URL buscada no da error para, en su caso, break-ear el fo
    if(!error_404[1]=="error")
    {
      seleccion$var_empresite_cif[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-cif')]")        %>% html_text() %>% checkvar()
      seleccion$var_empresite_tel[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-telefono')]")   %>% html_text() %>% checkvar()
      seleccion$var_empresite_dom[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-domicilio')]")  %>% html_text() %>% checkvar()
      seleccion$var_empresite_eml[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-email')]")      %>% html_text() %>% checkvar()
      seleccion$var_empresite_web[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-web')]")        %>% html_text() %>% checkvar()
      seleccion$var_empresite_obj[i]  <- web5 %>% html_nodes(xpath="//li[contains(@class,'ico-objeto')]/span[contains(@class,'category')]") %>% html_text() %>% checkvar()
      seleccion$var_empresite_lat[i]  <- web5 %>% html_nodes(xpath="//meta[contains(@itemprop,'latitude')]")  %>% html_attr("content") %>% checkvar()
      seleccion$var_empresite_lon[i]  <- web5 %>% html_nodes(xpath="//meta[contains(@itemprop,'longitude')]") %>% html_attr("content") %>% checkvar()

      datos3 <- web5 %>% html_nodes(xpath="//ul/li") %>% html_text()
      datos3 <- datos3[grep("Rango de Ventas: ",datos3)]

      posicion_inicial <- 18
      posicion_final   <- str_locate(datos3,"Ver Ventas de la empresa")[1] - 1
      if(!is.na(posicion_final))
      {
        seleccion$var_empresite_vts[i] <- str_sub(datos3,posicion_inicial,posicion_final)
      }


      if(nchar(seleccion$var_empresite_tel[i])>12)
      {
        seleccion$var_empresite_tel[i] <- web5 %>% html_nodes(xpath="//span[contains(@class,'tel')]") %>% html_text() %>% checkvar()
      }

      seleccion$var_empresite_cif[i]  <- gsub("CIF: ","",seleccion$var_empresite_cif[i])
      seleccion$var_empresite_tel[i]  <- gsub("Teléfono: ","",seleccion$var_empresite_tel[i])
      seleccion$var_empresite_dom[i]  <- gsub("Domicilio Social: ","",seleccion$var_empresite_dom[i])
      seleccion$var_empresite_dom[i]  <- gsub("ESPAÑA","",seleccion$var_empresite_dom[i])
      seleccion$var_empresite_dom[i]  <- gsub(" ¿Como llegar\\?","",seleccion$var_empresite_dom[i])
      seleccion$var_empresite_eml[i]  <- gsub("Email: ","",seleccion$var_empresite_eml[i])
      seleccion$var_empresite_web[i]  <- gsub("Web: ","",seleccion$var_empresite_web[i])
      seleccion$var_empresite_obj[i]  <- gsub("Objeto Social: ","",seleccion$var_empresite_obj[i])}

    # Búsqueda de datos BING
    # ---------------------------

    link <- paste("https://www.bing.com/search?q=",seleccion$var_emp[i]," ",seleccion$var_mun[i],sep="")
    link <- gsub(" ","+",link)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

    error_404 <-FALSE
    error_404 <- tryCatch(session <- html_session(link, user_agent(uastring)),error=function(e){"error"})  #Se comprueba que la URL buscada no da error para, en su caso, break-ear el fo
    if(!error_404[1]=="error")
    {
      if(seleccion$var_empresite_web[i]=="-")
      {
        seleccion$var_bing_url[i] <- session %>% html_nodes(xpath="//li/a[contains(@class,'cbtn b_noTarget')]") %>% html_attr("href") %>% checkvar()
        if(seleccion$var_bing_url[i]=="-")
        {
          seleccion$var_bing_url[i] <- session %>% html_nodes(xpath="//li[contains(@class,'b_algo')][1]") %>% html_nodes("a") %>% html_attr("href") %>% checkvar()
        }
      }
    }

    # Búsqueda de coordenadas GPS
    # ---------------------------

    url <- paste(url_geocod,seleccion$var_emp[i],seleccion$var_direccion[i],seleccion$var_mun[i],seleccion$var_prv[i],"Spain")
    url <- gsub(" ","%20",url)

    print(url)

    respuesta <- GET(url) %>% content()

    if(!identical(respuesta$Response$View,list())&&!identical(respuesta,NULL))
    {
      latitud <- respuesta$Response$View[[1]]$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
      seleccion$var_latitud[i] <- latitud

      longitud  <- respuesta$Response$View[[1]]$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
      seleccion$var_longitud[i] <- longitud
    }

    print(seleccion[i,])

  }


  #===============================================================
  # CREACIÓN JSON Y ENVÍO A PLATAFORMA SMART CITY
  #===============================================================

  #Variables envío JSON a plataforma
  TB_token <- "XDghy3jJM5qLsTOAnA0t"
  TB_url   <- paste("http://94.130.77.253:8080/api/v1/",TB_token,"/telemetry",sep="")

  json_fichas_return <- toJSON(seleccion,pretty=T)

  #Extracción timestamp en formato unix
  tsi <- format(as.numeric(anytime(Sys.Date()))*1000,scientific = F)
  #tsi <- sub("\\..*", "",tsi)
  for(i in 1:nrow(seleccion)){
    ts <- as.numeric(tsi) +i  #Añade i ms al timestamp para poder verse sin solapamiento en el widget de la plataforma smart city.

    #Creación de JSON noticias y eliminación de ][ para cumplir con el formato json con modificación de timestamp de thingsboard.
    json_fichas <- toJSON(seleccion[i,],pretty=T)
    json_fichas <- sub("[[]","",json_fichas)
    json_fichas <- sub("[]]","",json_fichas)

    #Formato json con modificación de timestamp de thingsboard.
    json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_fichas,"}",sep="")

    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)
  }

  return(json_fichas_return)

  #write.table(cabeceras,"d:/CAMARGO_EMPRESAS/empresas.csv",sep=";",dec=",",row.names = FALSE)
}

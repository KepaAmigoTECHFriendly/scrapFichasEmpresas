% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrap_fichas_empresa.R
\name{scrap_fichas_empresas}
\alias{scrap_fichas_empresas}
\title{Scrapea el directorio de empresas de Expansión online y enriquece los datos extraídos con datos del Economista}
\usage{
scrap_fichas_empresas(municipio, provincias)
}
\arguments{
\item{municipio, }{provincias}
}
\value{
json
}
\description{
Scrapea el directorio de empresas de Expansión online
 y enriquece el contenido mediante la extracción de datos del Economista online
 devolviendo un json resultado y envíandolo a la plataforma Smart City.
}
\examples{
scrap_fichas_empresas('Camargo', 'Cantabria')

}

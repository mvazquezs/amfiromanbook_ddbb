### 'amphi_tool.required_packages()'

#' @title Gestió de llibreries i configuracions bàsiques per 'ipdl_tool'.
#' @description Aquesta funció carrega tots els paquets necessaris per 'ipdl_tool'.
#' @param locale Canvia els paràmetres locals de l'ordinador (per defecte 'es_ES.UTF-8').
#' @param update_packages Actualitza els paquets de R necessaris (per defecte FALSE).
#' 
#' @return Un espai de treball amb tots els paquets de R requerits carregats.
#'
#' @import pacman
#' @seealso \code{\link{pacman::p_load}}
#'
#' @rdname amphi_tool.required_packages
#' 
#' @export
amphi_tool.required_packages <- function(
  locale = 'es_ES.UTF-8',
  update_packages = FALSE) {

### load library(pacman)
  if (!requireNamespace('pacman', quietly = TRUE)) {
    
    install.packages('pacman')
  
  } else {

    library(pacman)
  }

### Llegeix dades amb la configuració local
## Alerta a la configuració local
  options(datatable.fread.dec.locale = 'es_ES.UTF-8')	

### Repositoris
	local({r <- getOption('repos')
   r['CRAN'] <- 'https://cloud.r-project.org/'
   options(repos = r)})


### Funció per a la auto-carrega
	 auto_load <- function(a.package){
	    suppressWarnings(
        suppressPackageStartupMessages(
          library(a.package, character.only = TRUE)))
	 }

### Llista de paquets a carregar
  l_paquets <- c(
    'dplyr', 'tibble', 'readxl', 'data.table', 'stringr',
    'rlang', 'purrr', 'tidyr', 'missForest', 'psych',
    'DT', 'gtsummary', 'gt')


### Carrega o instal·la paquets
  pacman::p_load(
    char = l_paquets,
    install = TRUE,
    update = update_packages
    )

### Configura la llibreria 'missForest'
  suppressWarnings(
    suppressPackageStartupMessages(
      library(missForest)))

### Opcions d'ample de pantalla
 options(width = getOption('width'))


### Evitar carrega de espai de treball
### rm(list = ls(all = TRUE))
}
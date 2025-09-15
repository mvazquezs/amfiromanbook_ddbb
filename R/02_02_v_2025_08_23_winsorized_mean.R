### amphi_func.mitjana_windsorized

#' @title Calcula la mitjana winsoritzada.
#' @description Calcula la mitjana winsoritzada d'un vector numèric. Els valors extrems són substituïts per quantils prèviament definits abans de calcular la mitjana.
#'
#' @param x Un vector numèric.
#' @param limits Un vector numèric de longitud 2. Defineix els quantils inferior i superior per a la winsorització. Per defecte, retalla l'1% inferior i superior.
#' @param na.rm Un valor lògic. Si és `TRUE` (per defecte), els valors `NA` s'eliminen abans de la winsorització.
#'
#' @details Aquest mètode és útil per reduir la influència de valors extrems (outliers) en el càlcul de la mitjana. Els valors per sota del quantil inferior es substitueixen per aquest quantil, i els valors per sobre del quantil superior es substitueixen per aquest darrer.
#'
#' @return Un valor numèric amb la mitjana winsoritzada.
#'
#' @seealso \code{\link[base]{mean}}
#' @importFrom stats::quantile
#'
#' @examples
#' # Mitjana winsoritzada amb valors extrems
#' mitjana_windso <- amphi_func.mitjana_winsorized(c(1, 2, 3, 4, 100))
#' 
#' # Personalitzant els límits
#' mitjana_windzo_limits <- amphi_func.mitjana_winsorized(c(1, 2, 3, 4, 100), limits = c(.20, .80))
#' 
#' @rdname amphi_func.mitjana_winsorize
#'
#' @export
amphi_func.mitjana_winsorized <- function(
  x, 
  limits = c(0.1, 0.9), 
  na.rm = TRUE) 
{
### Evita valors perduts o NA
  if (all(is.na(x))) {

    warning("El vector conté només valors NA.")
    
    return(NA)
  
  }
  
  if (na.rm) {

    x <- x[!is.na(x)]
  
  }

### Comprovacions addicionals
  if (length(x) == 0) { return(NA_real_) }

  if (length(limits) != 2 || !is.numeric(limits)) {
    
    stop("L'argument 'limits' ha de ser un vector numèric de longitud 2.")
  
  }

### Càlcul de la mitjana winsorized
  lower_limit <- stats::quantile(x, limits[1], na.rm = na.rm)
  upper_limit <- stats::quantile(x, limits[2], na.rm = na.rm)

### Winsorització
  x_winsorized <- pmax(pmin(x, upper_limit), lower_limit)
  
### Retorna la mitjana
  return(mean(x_winsorized, na.rm = na.rm))
}
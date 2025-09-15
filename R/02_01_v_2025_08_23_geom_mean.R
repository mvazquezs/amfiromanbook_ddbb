### amphi_func.mitjana_geometrica

#' @title Calcula la mitjana geomètrica i el seu interval de confiança.
#' @description Calcula la mitjana geomètrica d'un vector numèric i, opcionalment, el seu interval de confiança. Permet escollir entre una distribució t i una normal per al càlcul de l'interval.
#'
#' @param x Un vector numèric amb valors positius.
#' @param na.rm Un valor lògic. Si és `TRUE` (per defecte), els valors `NA` s'eliminen abans del càlcul.
#' @param ci Un valor lògic. Si és `TRUE` (per defecte), la funció retorna la mitjana geomètrica i el seu interval de confiança. Si és `FALSE`, només retorna la mitjana.
#' @param conf_level El nivell de confiança per a l'interval, si `ci = TRUE`. El valor per defecte és de 0.95.
#' @param distribution El mètode de distribució per calcular l'interval de confiança. Pot ser "t" (per defecte) o "normal".
#' @param base La base del logaritme per a la transformació de dades. Pot ser "e" (per defecte) o "10".
#' @details La funció transforma les dades a escala logarítmica, calcula la mitjana i l'error estàndard, i després transforma els valors de l'interval de confiança de nou a l'escala original. Aquesta aproximació és més robusta per a dades amb distribució log-normal.
#'
#' @return Un objecte de classe `data.frame` amb la mitjana geomètrica i els límits de l'interval de confiança si `ci = TRUE`, o un valor numèric amb la mitjana si `ci = FALSE`.
#' 
#' @importFrom stats::qt 
#' @importFrom stats::na.omit
#' @importFrom stats::qnorm
#' @importFrom tibble::as_tibble
#' @seealso \code{\link[base]{mean}}
#' @seealso
#' Codi per al càclcul de la mitjana geometrica: \url{https://github.com/tagteam/Publish/blob/master/R/ci.geomean.R}
#'
#' @examples
#' # Mitjana geomètrica d'un vector
#' # mitjana_geom <- amphi_func.mitjana_geometrica(c(1, 2, 8))
#'
#' # Amb valors NA
#' # mitjana_geom_na <- amphi_func.mitjana_geometrica(c(1, 2, 8, NA), na.rm = TRUE)
#'
#' # Amb valors negatius (generarà una advertència)
#' # mitjana_geom_neg <- amphi_func.mitjana_geometrica(c(-1, 2, 8))
#'
#' @rdname amphi_func.mitjana_geometrica
#'
#' @export
amphi_func.mitjana_geometrica <- function(
  x, 
  na.rm = TRUE, 
  ci = TRUE, 
  conf_level = .95, 
  distribution = c('t_student', 'gaussian'), 
  base = c('e', '10'),
  returnar_df = FALSE) 
{

### Si na.rm és TRUE, elimina els valors NA
  if (na.rm) {
  
    x <- stats::na.omit(x)
  
  }
  
### Atura l'execució si hi ha valors no positius
  if (any(x <= 0)) {
  
    stop("No es pot calcular la mitjana geomètrica amb valors zero o negatius.")
  
  }
  
### Realitza la transformació logarítmica
### Bases: '10', 'e'
  if (base == '10') {
  
    log_x <- log10(x)
  
  } else if (base == 'e') {
    
    log_x <- log(x)
  
  } else {
  
    stop("La base ha de ser 'e' o '10'.")
  
  }

  n <- length(log_x)
  
### Si el vector té menys de 2 elements, no es pot calcular CI
  if (n < 2 & ci == TRUE) {
  
    warning("Poques dades per calcular l'interval de confiança.")
  
    ci <- FALSE
  
  }
  
### Calcula la mitjana geomètrica en l'escala original
  if (base == '10') {
    
    geomean <- 10^(mean(log_x))
  
  } else {
  
    geomean <- exp(mean(log_x))
  
  }
  
### Si es demana l'interval de confiança, el calcula
  if (ci) {
  
  # Calcula l'error estàndard de la mitjana logarítmica
    log_se <- sd(log_x) / sqrt(n)
    
  # Obté el valor quantil segons la distribució
    if (distribution == 't_student') {
      
      q_val <- stats::qt(1 - ((1 - conf_level) / 2), df = n - 1)
    
    } else if (distribution == 'gaussian') {
    
      q_val <- stats::qnorm(1 - ((1 - conf_level) / 2))
    
    } else {
    
      stop("La distribució ha de ser 't_student' o 'gaussian'.")
    
    }
    
  # Calcula l'interval de confiança en l'escala logarítmica
    log_ci_lower <- mean(log_x) - (q_val * log_se)
    log_ci_upper <- mean(log_x) + (q_val * log_se)
    
  # Transforma l'interval a l'escala original
    if (base == '10') {
  
      lower_ci <- 10^(log_ci_lower)
      upper_ci <- 10^(log_ci_upper)
  
    } else {
  
      lower_ci <- exp(log_ci_lower)
      upper_ci <- exp(log_ci_upper)
  
    }
    
### Retorna un tibble amb els resultats
  if(returnar_df == FALSE) {
    
    return(geomean)
    
  } else {
  
    return(
      data.frame(
        gm_mean = geomean,
        ci_lower = lower_ci,
        ci_upper = upper_ci) %>%
      tibble::as_tibble())
  }
}
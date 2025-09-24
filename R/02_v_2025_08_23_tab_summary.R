### the 'amphi_func.tab_summary'

#' @title Calcula estadístiques descriptives per a un `data.frame`.
#' @description Calcula un conjunt complet d'estadístiques descriptives per a variables numèriques, incloent quantils, mitjana, mediana, asimetria, curtosi i el test de Shapiro-Wilk. La funció està optimitzada per al seu ús amb el `tidyverse`.
#'
#' @param df El `data.frame` o `tibble` d'entrada.
#' @param seleccio_variables Les variables numèriques a analitzar. Es pot utilitzar la sintaxi de selecció de `dplyr` (per exemple, `starts_with("var")`, `contains("grup")`).
#' @param grup_by Les variables per agrupar les dades. També pot utilitzar la sintaxi de selecció de `dplyr`. Si es deixa com a `NULL`, les estadístiques es calculen per a tot el `data.frame`.
#' @param stats_adicionals Un vector de caràcters per especificar estadístiques addicionals a calcular. Les opcions inclouen: "shapiro" (per al test de Shapiro-Wilk), "skewness" (asimetria) i "kurtosis" (curtosi).
#' @param q_lower Un valor numèric per a calcular un quantil de límit inferior.
#' @param q_upper Un valor numèric per a calcular un quantil de límit superior.
#' @param sd_num Un valor numèric per definir el nombre de desviacions estàndard per a les estadístiques de límit inferior i superior.
#' @param na.rm Un valor lògic. Si és `TRUE` (per defecte), els valors `NA` s'eliminen abans de tots els càlculs.
#' @param bind_rows Un valor lògic. Si és `TRUE` (per defecte), la llista de `tibbles` es combina en un sol `tibble`, afegint una columna anomenada `variable` amb el nom de la variable analitzada.
#' @param digits El nombre de dígits decimals a mostrar per a les variables `double`. Per defecte és 2. Les variables `integer` no s'arrodoneixen.
#'
#' @details Aquesta funció utilitza `dplyr::summarise()` i `dplyr::across()` per aplicar de manera eficient múltiples funcions a diverses variables. Les proves de Shapiro-Wilk, asimetria i curtosi només s'executaran si s'especifiquen a `additional_stats`. El test de Shapiro-Wilk requereix com a mínim 3 dades no perdudes per a cada grup i variable.
#'
#' @return Un `tibble` que conté les estadístiques descriptives calculades per a cada variable i grup.
#'
#' @importFrom dplyr::select
#' @importFrom dplyr::group_by
#' @importFrom dplyr::summarise_at
#' @importFrom tibble::as_tibble
#' @importFrom e1071::skewness
#' @importFrom e1071::kurtosis
#' @seealso \code{\link[stats]{shapiro.test}} per al test de Shapiro-Wilk.
#' @seealso \code{\link[e1071]{skewness}} i \code{\link[e1071]{kurtosis}} per al càlcul d'asimetria i curtosi.
#'
#' @examples
#' # Crea un data.frame de prova
#' df_test <- data.frame(
#'   grup_a = c("A", "A", "A", "B", "B", "B"),
#'   grup_b = c("X", "Y", "Z", "X", "Y", "Z"),
#'   valor1 = c(1, 2, 3, 4, 5, 6),
#'   valor2 = c(10.123, 20.456, NA, 40.789, 50.123, 60.456),
#'   valor_extra = c(10, 20, 30, 40, 50, 60))
#'
#' # Calcula estadístiques descriptives bàsiques per grup amb arrodoniment
#' amphi_func.tab_summary(
#'   df = df_test, 
#'   seleccio_variables = starts_with('valor'),
#'   grup_by = 'grup_a',
#'   stats_adicionals = TRUE,
#'   sd_num = 3,
#'   q_lower = .05,
#'   q_upper = .95,
#'   na.rm = TRUE,
#'   bind_rows = TRUE,
#'   digits = 2)
#'
#' @rdname amphi_func.tab_summary
#' @export
amphi_func.tab_summary <- function(
    df,
    grup_by = NULL,
    seleccio_variables,
    stats_adicionals = FALSE,
    sd_num = NULL,
    q_lower = NULL,
    q_upper = NULL,
    na.rm = TRUE,
    bind_rows = TRUE,
    digits = 2)
{
  ### Captura les expressions de selecció de l'usuari amb enquo
  sel_exp_variables <- rlang::enquo(seleccio_variables)
  grup_exp_by <- rlang::enquo(grup_by)
  
  ### Obté els noms de les columnes per al bucle lapply
  sel_vars_names <- names(
    dplyr::select(df, !!sel_exp_variables))
  
  group_cols_names <- if (!is.null(grup_by)) {
    
    names(
      dplyr::select(df, !!grup_exp_by))
    
  } else {
    
    character(0)
  }
  
  ### Converteix les columnes de caràcters a factors si s'agrupa
  if (!is.null(grup_by)) {
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(group_cols_names), as.factor))
  }
  
  ### Definició de les estadístiques bàsiques
  ### suppressWarnings() evita avisos tipus 'Inf'
  suppressWarnings(
    tab_bases <- lapply(
      sel_vars_names, function(x) {
        df[, x] <- df[, x]
        
        df %>%
          group_by_at(group_cols_names) %>%
          summarise_at(vars(all_of(x)),
                       list(
                         n = ~ sum(!is.na(.)),
                         na = ~ sum(is.na(.)),
                         min = ~ min(., na.rm = na.rm),
                         q_1 = ~ quantile(., probs = 0.25, na.rm = na.rm),
                         median = ~ median(., na.rm = na.rm),
                         mean = ~ mean(., na.rm = na.rm),
                         sd = ~ sd(., na.rm = na.rm),
                         q_3 = ~ quantile(., probs = 0.75, na.rm = na.rm),
                         max = ~ max(., na.rm = na.rm),
                         iqr = ~ stats::IQR(., na.rm = na.rm))) %>%
          as_tibble() %>%
          distinct()
      }))
  
  names(tab_bases) <- sel_vars_names
  
  ### 'stats_adicionals' argument
  if(isTRUE(stats_adicionals)){
    
    suppressWarnings(
      tab_adicionals <- lapply(
        sel_vars_names, function(x) {
          df[, x] <- df[, x]
          
          df %>%
            group_by_at(group_cols_names) %>%
            summarise_at(vars(all_of(x)),
                         list(
                           q_lower = ~ quantile(., probs = q_lower, na.rm = na.rm),
                           sd_lower = ~ as.double(mean(., na.rm = na.rm) - sd_num * sd(., na.rm = na.rm)), 
                           sd_upper = ~ as.double(mean(., na.rm = na.rm) + sd_num * sd(., na.rm = na.rm)),
                           q_upper = ~ quantile(., probs = q_upper, na.rm = na.rm),
                           kurt = ~ e1071::kurtosis(., na.rm = na.rm),
                           skew = ~ e1071::skewness(., na.rm = na.rm))) %>%
            as_tibble() %>%
            distinct()
        }))
    
    ### Unir 'tab_bases' i 'tab_adicionals'
    l_tab_all <- list()
    
    for(i in seq_along(tab_bases)) { 
      
      if(!is.null(grup_by)) {
        
        tab_all <- dplyr::left_join(
          tab_bases[[i]], 
          tab_adicionals[[i]], 
          by = group_cols_names)
        
      } else {
        
        tab_all <- dplyr::cross_join(
          tab_bases[[i]], 
          tab_adicionals[[i]])
        
      }
      
      l_tab_all[[length(l_tab_all) + 1]] = tab_all
      
    }
    
    names(l_tab_all) <- sel_vars_names
    
    # Arrodoniment
    l_tab_all <- lapply(l_tab_all, function(df) {
      df %>% dplyr::mutate(dplyr::across(where(is.double), ~round(., digits)))
    })
    
    if (isTRUE(bind_rows)) {
      
      df_tab_all <- dplyr::bind_rows(l_tab_all, .id = 'variable') %>%
        dplyr::mutate(variable = as.factor(variable))

      return(df_tab_all)
      
    } else {
      
      return(l_tab_all)
    }
    
  } else {
    
    # Arrodoniment
    tab_bases <- lapply(tab_bases, function(df) {
      df %>% dplyr::mutate(dplyr::across(where(is.double), ~round(., digits)))
    })
    
    if (isTRUE(bind_rows)) {
      
      df_tab_bases <- dplyr::bind_rows(tab_bases, .id = 'variable') %>%
        dplyr::mutate(variable = as.factor(variable))
      
      return(df_tab_bases)
      
    } else {
      
      return(tab_bases)
      
    }
  }
}
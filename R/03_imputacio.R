#' Imputació per estadístics de tendència central
#'
#' Aquesta funció realitza una imputació de valors perduts (NA) en un data frame.
#' Permet agrupar les dades per una o més variables i aplicar diferents mètodes
#' d'imputació.
#'
#' @param df Un data frame.
#' @param seleccio_variables Columnes a imputar (tidy-select).
#' @param grup_by Columnes per agrupar la imputació (tidy-select).
#' @param grup_by_reserva Columnes per agrupar si queden NAs (tidy-select).
#' @param metode_imputacio Mètode d'imputació: 'aritmetica', 'truncada', 'geometrica', 'winsoritzada', 'mediana'.
#' @param valor_trim Valor de trim per a les mitjanes 'truncada' i 'winsoritzada'.
#' @param report_imputacio Retorna un report de la imputació? (default: TRUE).
#' @param retorna_original Retorna el data frame original juntament amb l'imputat? (default: FALSE).
#'
#' @return Una llista que conté:
#'   - `imputed_df`: El data frame amb els valors imputats.
#'   - `original_df`: (Opcional) El data frame original.
#'   - `report`: (Opcional) Un tibble amb l'informe d'imputació.
#' @export
#'
#' @examples
#' # Crear un data frame d'exemple
#' df_exemple <- data.frame(
#'   grup = c("A", "A", "B", "B", "A", "B"),
#'   valor1 = c(1, NA, 3, 4, 5, NA),
#'   valor2 = c(NA, 2, 3, NA, 5, 6)
#' )
#'
#' # Imputar sense agrupar
#' resultat_simple <- imputacio_estadistics(
#'   df = df_exemple,
#'   seleccio_variables = c(valor1, valor2)
#' )
#'
#' # Imputar agrupant per la columna 'grup'
#' resultat_agrupat <- imputacio_estadistics(
#'   df = df_exemple,
#'   seleccio_variables = c(valor1, valor2),
#'   grup_by = grup
#' )
imputacio_estadistics <- function(
  df,
  seleccio_variables,
  grup_by = NULL,
  grup_by_reserva = NULL,
  metode_imputacio = c('aritmetica', 'truncada', 'geometrica', 'winsoritzada', 'mediana'),
  valor_trim = 0.1,
  report_imputacio = TRUE,
  retornar_original = FALSE)
{
### Renomena df
  df_out <- df

### 'Quositation' de les variables clau
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

### Assegurar que les variables de selecció siguin numèriques i la resta factors
  df_out <- df_out %>%
    dplyr::mutate(
      dplyr::across(!!seleccio_variables_enquo, as.numeric)) %>%
    dplyr::mutate(
      dplyr::across(!(!!seleccio_variables_enquo), as.factor))

### Càlcul del total de cel·les per al report
  num_vars_seleccionades <- ncol(df_out %>% dplyr::select(!!seleccio_variables_enquo))
  total_cells <- nrow(df_out) * num_vars_seleccionades

### Comptatge de NAs en matriu original
  na_originals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo)))
  
### Report matriu de dades
  report_df <- tibble::tibble(
    etapa = 'NAs originals',
    na_n = na_originals,
    na_percent = round(na_originals / total_cells * 100, digits = 2))

  f_imputacio <- function(
    df, 
    vars, 
    metode, 
    v_trim) { 
  
    df %>%
      dplyr::mutate(
        dplyr::across({{ vars }},
        ~ tidyr::replace_na(., switch(metode,
          'aritmetica' = mean(., na.rm = TRUE),
          'truncada' = mean(., trim = v_trim, na.rm = TRUE),
          'geometrica' = psych::geometric.mean(., na.rm = TRUE),
          'winsoritzada' = psych::winsor.mean(., trim = v_trim, na.rm = TRUE),
          'mediana' = median(., na.rm = TRUE)))))
  
  }

### 'grup_by' == NULL & 'grup_by_reserva' == NULL
  if (rlang::quo_is_null(grup_by_enquo) && rlang::quo_is_null(grup_by_reserva_enquo)) {
    
    df_out <- f_imputacio(
      df = df_out, 
      vars = !!seleccio_variables_enquo,
      metode = metode_imputacio, 
      v_trim = valor_trim)

### 'grup_by' != NULL 
  } else if (!rlang::quo_is_null(grup_by_enquo)) {
    
    df_out <- df_out %>%
      dplyr::group_by(across(!!grup_by_enquo)) %>%
      f_imputacio(
        df = .,
        vars = !!seleccio_variables_enquo, 
        metode = metode_imputacio, 
        v_trim = valor_trim) %>%
      dplyr::ungroup()

  ### Si queden 'NAs' & 'grup_by_reserva' != NULL
    if (any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) && !rlang::quo_is_null(grup_by_reserva_enquo)) {
      
      df_out <- df_out %>%
        dplyr::group_by(across(!!grup_by_reserva_enquo)) %>%
          f_imputacio(
            df = .,
            vars = !!seleccio_variables_enquo, 
            metode = metode_imputacio, 
            v_trim = valor_trim) %>%
          dplyr::ungroup()
      
    }
  }

### Comptatge de NAs matriu final
  na_restants <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo)))
      
  report_df <- report_df %>% 
    tibble::add_row(
      etapa = 'NAs restants',
      na_n = na_restants,
      na_percent = round(na_restants / total_cells * 100, digits = 2))


  # Assign the final report dataframe
  report <- report_df

### Sortida
  result <- list(
    imputed_df = df_out)
  
  if (isTRUE(retornar_original)) {
  
    result$original_df <- df
  
  }
  
  if (isTRUE(report_imputacio)) {
  
    result$report <- report
  
  }

  return(result)
}


#' Imputació de dades amb `missForest` amb optimització i agrupació
#'
#' @description
#' Aquesta funció realitza una imputació de valors perduts utilitzant l'algorisme
#' `missForest`. Permet executar l'algorisme de manera global o per grups, i
#' incorpora un procediment opcional per optimitzar l'hiperparàmetre `mtry`.
#'
#' @param df Un data.frame amb els valors a imputar.
#' @param seleccio_variables Un vector amb els noms de les columnes a imputar i/o utilitzar com a predictors.
#' @param grup_by (Opcional) Columnes per agrupar la imputació (tidy-select).
#' @param grup_by_reserva (Opcional) Columnes per agrupar si queden NAs després de la primera agrupació.
#' @param optim_mtry Un booleà (`TRUE`/`FALSE`). Si és `TRUE`, la funció buscarà el valor
#'   òptim de `mtry` que minimitzi l'error d'imputació (OOB). Aquest procés és
#'   computacionalment més costós. Per defecte és `FALSE`.
#' @param ntree Nombre d'arbres a créixer al bosc. El valor per defecte és 100.
#'   Es recomana utilitzar valors més alts (p. ex., 300-500) per obtenir
#'   imputacions més estables.
#' @param maxiter Nombre màxim d'iteracions de l'algorisme. El valor per defecte
#'   és 10. Normalment, l'algorisme convergeix ràpidament.
#' @param report_imputacio Retorna un report de la imputació? (default: TRUE).
#' @param retorna_original Retorna el data frame original juntament amb l'imputat? (default: FALSE).
#' @param ... Altres paràmetres que es poden passar a `missForest::missForest()` (p. ex., `ntree`).
#'
#' @return Una llista que conté:
#'   - `imputed_df`: El data frame amb els valors imputats.
#'   - `original_df`: (Opcional) El data frame original.
#'   - `report`: (Opcional) Un tibble amb l'informe d'imputació.
#'
#' @section Funcionament intern de `f_missforest`:
#' La funció interna `f_missforest` és el motor de la imputació.
#' Si `optim_mtry` és `FALSE`, simplement executa `missForest` amb els paràmetres
#' proporcionats.
#' Si `optim_mtry` és `TRUE`, duu a terme un procés d'optimització per trobar
#' el millor hiperparàmetre `mtry` (el nombre de variables a mostrejar en cada
#' node). Aquest procés itera sobre tots els valors possibles de `mtry` (d'1
#' fins al nombre total de variables), executa `missForest` per a cada un, i
#' en recull l'error "Out-of-Bag" (OOB). Finalment, selecciona el `mtry` que
#' ha generat l'error mínim i l'utilitza per a la imputació definitiva,
#' assegurant així una major precisió.
#'
#' @source L'estratègia d'optimització de `mtry` es basa en les anàlisis de:
#'   - Stekhoven, D.J. and Bühlmann, P. (2012). 'missForest - Non-parametric missing 
#'     value imputation for mixed-type data'. Bioinformatics, 28(1), 112-118.
#'     Vignette: https://cran.r-project.org/web/packages/missForest/vignettes/missForest_1.5.pdf
#'   - Anàlisi pràctica de `missForest`: https://rpubs.com/lmorgan95/MissForest
#'   - Comparativa de mètodes d'imputació: https://rpubs.com/JkrAndres/dataimputation
#'
#' @import dplyr
#' @import tidyr
#' @import missForest
#' @import purrr
#'
#' @examples
#' # Crear un data.frame d'exemple amb valors NA a partir de 'iris'
#' df_exemple <- iris
#' df_exemple[sample(1:150, 40), "Sepal.Length"] <- NA
#' df_exemple[sample(1:150, 20), "Petal.Width"] <- NA
#' df_exemple$Grup.Test <- sample(c("G1", "G2"), 150, replace = TRUE)
#'
#' # Variables a seleccionar per a la imputació
#' vars_a_imputar <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#'
#' # Execució simple, sense agrupació ni optimització
#' resultat <- imputacio_missforest(
#'   df = df_exemple,
#'   seleccio_variables = vars_a_imputar
#' )
#' print(resultat$report)
#'
#' \dontrun{
#' # Execució amb agrupació i optimització (pot trigar més)
#' resultat_agrupat <- imputacio_missforest(
#'   df = df_exemple,
#'   seleccio_variables = vars_a_imputar,
#'   grup_by = Grup.Test,
#'   optim_mtry = TRUE
#' )
#' print(resultat_agrupat$report)
#' }
imputacio_missforest <- function(
  df,
  seleccio_variables,
  grup_by = NULL,
  grup_by_reserva = NULL,
  optim_mtry = FALSE,
  ntree = 100,
  maxiter = 10,
  verbose = FALSE,
  set_seed = 19810424,
  report_imputacio = FALSE,
  retornar_original = FALSE)
{
  ### Renomena df
  df_out <- df

  ### 'Quositation' de les variables clau
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

### Assegurar que les variables de selecció siguin numèriques i la resta factors
  df_out <- df_out %>%
    dplyr::mutate(
      dplyr::across(!!seleccio_variables_enquo, as.numeric),
      dplyr::across(-c(!!seleccio_variables_enquo), as.factor))

### Funció interna per executar missForest
  f_missforest <- function(
    df_miss, 
    ntree,
    maxiter,
    optim = FALSE, ...) {
    
    if (isTRUE(optim)) {
      
      p <- ncol(df_miss)
      mtry_candidates <- 1:p
      oob_errors <- numeric(length(mtry_candidates))
      
      for (i in seq_along(mtry_candidates)) {
      
        result <- missForest::missForest(
          df_miss, 
          mtry = mtry_candidates[i], 
          ntree = ntree,
          maxiter = maxiter,...)$OOBerror
      
        oob_errors[i] <- result
      
      }
      
      best_mtry <- mtry_candidates[which.min(oob_errors)]
      df_imputat <- missForest::missForest(
        df_miss, 
        mtry = best_mtry,
        ntree = ntree,
        maxiter = maxiter, ...)
    
    } else {
      
      df_imputat <- missForest::missForest(
        df_miss, 
        ntree = ntree,
        maxiter = maxiter,...)
    
    }
    return(df_imputat)
  }

### Lògica d'imputació
  safe_f_missforest <- purrr::possibly(
    f_missforest, 
    otherwise = NULL, 
    quiet = !verbose)

### set.seed
  set.seed(set_seed)

  if (rlang::quo_is_null(grup_by_enquo)) {
    
  ### Imputació global
    if(sum(is.na(df_out)) > 0) {
      
      suppressWarnings(
        missforest_result <- f_missforest(
          as.data.frame(df_out), 
          optim = optim_mtry, 
          ntree = ntree, 
          maxiter = maxiter,
          verbose = verbose))
      
      df_out <- missforest_result$ximp
      report_list$error_global <- missforest_result$OOBerror
    
    }
  
  } else {
  
  ### Imputació per grup_by
    nested_data <- df_out %>%
      dplyr::group_by_at(grup_by) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        missforest_model = purrr::map(data, ~{
          if (any(is.na(.x))) {
            suppressWarnings(
            safe_f_missforest(
              df_miss = as.data.frame(.x),
              optim = optim_mtry,
              ntree = ntree,
              maxiter = maxiter,
              verbose = verbose)) } else { NULL }
        }),
        imputed_data = purrr::map2(data, missforest_model, ~{
          if (!is.null(.y)) { .y$ximp } else { .x }
        }))
    
    imputed_grup_by <- nested_data %>%
      dplyr::select(-data, -missforest_model) %>%
      tidyr::unnest(cols = c(imputed_data))
    
    df_out <- imputed_grup_by

### Imputació per grup_by_reserva si queden NAs
  if (any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) && !rlang::quo_is_null(grup_by_reserva_enquo)) {
      
    nested_data_reserva <- df_out %>%
      dplyr::group_by_at(grup_by_reserva) %>%
      tidyr::nest() %>%
        dplyr::mutate(
                  missforest_model = purrr::map(data, ~{
          if (any(is.na(.x))) {
            suppressWarnings(
            safe_f_missforest(
              df_miss = as.data.frame(.x),
              optim = optim_mtry,
              ntree = ntree,
              maxiter = maxiter,
              verbose = verbose)) } else { NULL }
        }),
        imputed_data = purrr::map2(data, missforest_model, ~{
          if (!is.null(.y)) { .y$ximp } else { .x }
        }))
      
      imputed_reserva <- nested_data_reserva %>%
        dplyr::select(-data, -missforest_model) %>%
        tidyr::unnest(cols = c(imputed_data))

      df_out <- imputed_reserva
    }
  }

### Sortida
  result <- list(imputed_df = df_out)
  if (isTRUE(retornar_original)) {
    result$original_df <- df
  }
  if (isTRUE(report_imputacio)) {
    result$report <- report
  }

  return(result)
}

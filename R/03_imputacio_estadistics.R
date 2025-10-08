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
  df_original <- df

### Add row id for reordering at the end
  df_out <- df_original %>% dplyr::mutate(.row_id = dplyr::row_number())

### 'Quositation' de les variables clau
  grup_by_sym <- rlang::ensym(grup_by)
  grup_by_reserva_sym <- rlang::ensym(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables) # Mantenim enquo per seleccions complexes

### Validació d'arguments i preparació
  metode_imputacio <- match.arg(metode_imputacio)
  
  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(!!seleccio_variables_enquo, as.numeric))

### Creació de les flags (inicialitzades a 0)
  impute_vars_names <- df_out %>% dplyr::select(!!seleccio_variables_enquo) %>% names()
  flag_vars_names <- paste0('flag_', impute_vars_names)
  
  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(all_of(impute_vars_names), ~0, .names = 'flag_{.col}'))


### Funció interna d'imputació (més robusta)
  f_imputacio <- function(
    df, 
    vars, 
    metode, 
    v_trim) { 
    df %>%
      dplyr::mutate(
        dplyr::across({{ vars }},
        ~ if (all(is.na(.))) . else tidyr::replace_na(., switch(metode,
            'aritmetica'   = mean(., na.rm = TRUE),
            'truncada'     = mean(., trim = v_trim, na.rm = TRUE),
            'geometrica'   = psych::geometric.mean(., na.rm = TRUE),
            'winsoritzada' = psych::winsor.mean(., trim = v_trim, na.rm = TRUE),
            'mediana'      = median(., na.rm = TRUE)
          ))
        ))
  }

### Report - Estat inicial
  report <- NULL
  if (isTRUE(report_imputacio)) {
    na_originals <- df_out %>%
      dplyr::select(!!seleccio_variables_enquo) %>%
      purrr::map_df( ~ sum(is.na(.))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'n_na_ori')
    report <- na_originals
  }

### Lògica d'imputació
  # 1. Imputació per grup principal (si s'especifica)
  if (!rlang::is_missing(grup_by_sym)) {
    df_out <- df_out %>%
      dplyr::group_by(!!grup_by_sym) %>%
      dplyr::mutate(
        dplyr::across(all_of(impute_vars_names), 
          ~ ifelse(is.na(.), 1, get(paste0('flag_', dplyr::cur_column()))), .names = 'flag_{.col}')) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!grup_by_sym) %>%
      f_imputacio(
        df = .,
        vars = !!seleccio_variables_enquo, 
        metode = metode_imputacio, 
        v_trim = valor_trim) %>%
      dplyr::ungroup()

    if (isTRUE(report_imputacio)) {
      na_post_grup <- df_out %>%
        dplyr::select(!!seleccio_variables_enquo) %>%
        purrr::map_df( ~ sum(is.na(.))) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'na_post_grup')
      report <- dplyr::left_join(report, na_post_grup, by = 'variable')
    }
  }

  # 2. Imputació per grup de reserva (si queden NAs i s'especifica)
  any_na_left <- any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo)))
  if (any_na_left && !rlang::is_missing(grup_by_reserva_sym)) {
    df_out <- df_out %>%
      dplyr::group_by(!!grup_by_reserva_sym) %>%
      dplyr::mutate(
        dplyr::across(all_of(impute_vars_names), 
          ~ ifelse(is.na(.), 2, get(paste0('flag_', dplyr::cur_column()))), .names = 'flag_{.col}')) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!grup_by_reserva_sym) %>%
      f_imputacio(
        df = .,
        vars = !!seleccio_variables_enquo, 
        metode = metode_imputacio, 
        v_trim = valor_trim) %>%
      dplyr::ungroup()

    if (isTRUE(report_imputacio)) {
      na_post_reserva <- df_out %>%
        dplyr::select(!!seleccio_variables_enquo) %>%
        purrr::map_df( ~ sum(is.na(.))) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'na_post_reserva')
      report <- dplyr::left_join(report, na_post_reserva, by = 'variable')
    }
  }

  # 3. Imputació global (si no s'ha especificat cap grup o si encara queden NAs)
  any_na_left <- any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo)))
  if (any_na_left && rlang::is_missing(grup_by_sym) && rlang::is_missing(grup_by_reserva_sym)) {
    df_out <- df_out %>%
      dplyr::mutate(
        dplyr::across(all_of(impute_vars_names), 
          ~ ifelse(is.na(.), 3, get(paste0('flag_', dplyr::cur_column()))), .names = 'flag_{.col}'))

    df_out <- f_imputacio(
      df = df_out, 
      vars = !!seleccio_variables_enquo,
      metode = metode_imputacio, 
      v_trim = valor_trim)
  }

### Muntatge del report final
  if (isTRUE(report_imputacio)) {
    na_finals <- df_out %>%
      dplyr::select(!!seleccio_variables_enquo) %>%
      purrr::map_df( ~ sum(is.na(.))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'n_na_finals')
    report <- dplyr::left_join(report, na_finals, by = 'variable')

    # Assegurar que les columnes existeixen i tenen el format correcte
    # Calcular valors imputats i reanomenar columnes
    report <- report %>%
      dplyr::mutate(
        n_imp_grup = if ('na_post_grup' %in% names(.)) n_na_ori - na_post_grup else 0,
        n_imp_reserva = if (all(c('na_post_grup', 'na_post_reserva') %in% names(.))) na_post_grup - na_post_reserva else 0
      )

    # Calcular fila de totals
    report_totals <- report %>%
      dplyr::summarise(
        variable = 'Total',
        n_na_ori = sum(n_na_ori, na.rm = TRUE),
        n_imp_grup = sum(n_imp_grup, na.rm = TRUE),
        n_imp_reserva = sum(n_imp_reserva, na.rm = TRUE),
        n_na_finals = sum(n_na_finals, na.rm = TRUE))

    # Unir report i totals i seleccionar columnes finals
    report <- dplyr::bind_rows(report, report_totals) %>%
      dplyr::select(
        variable, 
        n_na_ori, n_imp_grup, n_imp_reserva, n_na_finals) %>%
      dplyr::filter(!str_detect(variable, pattern = 'flag_'))
  
  }

### Sortida
  # Reorder rows to match original and select columns
  original_cols <- names(df_original)
  flag_cols <- names(df_out) %>% stringr::str_subset("^flag_")

  df_out <- df_out %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(all_of(original_cols), all_of(flag_cols))

  result <- list(imputed_df = df_out)
  
  if (isTRUE(retornar_original)) {
    result$original_df <- df_original

  }
  
  if (isTRUE(report_imputacio)) {
    result$report <- report
  
  }

  return(result)
}
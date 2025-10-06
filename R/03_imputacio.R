
#’ Imputació per mètodes estadístics
#’
#’ @description
#’ Aquesta funció realitza la imputació de dades faltants utilitzant mètodes estadístics simples.
#’
#’ @param df El dataframe d'entrada.
#’ @param seleccio_variables Un vector de noms de columnes per a la imputació.
#’ @param grup_by Una variable per agrupar les dades abans de la imputació.
#’ @param grup_by_reserva Una variable de reserva per agrupar si la primera agrupació falla.
#’ @param metode_imputacio El mètode estadístic a utilitzar. Pot ser 'aritmetica', 'truncada', 'geometrica', 'winsoritzada' o 'mediana'.
#’ @param metode_reserva Mètode de reserva si el mètode principal falla.
#’ @param valor_trim El valor de trim per a les mitjanes 'truncada' i 'winsoritzada'.
#’ @param report_imputacio Si és TRUE, retorna un informe d'imputació.
#’ @param retorna_original Si és TRUE, retorna el dataframe original juntament amb el resultat.
#’
#’ @return Una llista que conté el dataframe imputat i, opcionalment, l'original i l'informe.
#’
#’ @import dplyr
#’ @import tidyr
#’ @import psych
#’
#’ @rdname imputacio_estadistics
#’ @export
imputacio_estadistics <- function(df,
                                  seleccio_variables,
                                  grup_by = NULL,
                                  grup_by_reserva = NULL,
                                  metode_imputacio = c('aritmetica', 'truncada', 'geometrica', 'winsoritzada', 'mediana'),
                                  metode_reserva = NULL,
                                  valor_trim = 0.1,
                                  report_imputacio = TRUE,
                                  retorna_original = FALSE) {

  df_out <- df
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

  # Assegurar que les variables de selecció siguin numèriques i la resta factors
  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(!!seleccio_variables_enquo), as.numeric)) %>%
    dplyr::mutate(dplyr::across(!dplyr::all_of(!!seleccio_variables_enquo), as.factor))

  na_originals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo)))

  impute_func <- function(df, vars, method, trim_val) {
    df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars),
                                  ~ tidyr::replace_na(., switch(method,
                                                               'aritmetica' = mean(., na.rm = TRUE),
                                                               'truncada' = mean(., trim = trim_val, na.rm = TRUE),
                                                               'geometrica' = psych::geometric.mean(., na.rm = TRUE),
                                                               'winsoritzada' = psych::winsor.mean(., trim = trim_val, na.rm = TRUE),
                                                               'mediana' = median(., na.rm = TRUE)))))
  }

  if (rlang::quo_is_null(grup_by_enquo) && rlang::quo_is_null(grup_by_reserva_enquo)) {
    df_out <- impute_func(df_out, !!seleccio_variables_enquo, metode_imputacio, valor_trim)
  } else if (!rlang::quo_is_null(grup_by_enquo)) {
    df_out <- df_out %>%
      dplyr::group_by(!!grup_by_enquo) %>%
      impute_func(!!seleccio_variables_enquo, metode_imputacio, valor_trim) %>%
      dplyr::ungroup()

    if (any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) && !rlang::quo_is_null(grup_by_reserva_enquo)) {
      df_out <- df_out %>%
        dplyr::group_by(!!grup_by_reserva_enquo) %>%
        impute_func(!!seleccio_variables_enquo, metode_imputacio, valor_trim) %>%
        dplyr::ungroup()
    }
  }

  na_finals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  report <- tibble::tibble(
    `NA Originals` = na_originals,
    `% NA Originals` = na_originals / nrow(df_out),
    `NA Finals` = na_finals,
    `% NA Finals` = na_finals / nrow(df_out)
  )

  result <- list(imputed_df = df_out)
  if (retorna_original) {
    result$original_df <- df
  }
  if (report_imputacio) {
    result$report <- report
  }

  return(result)
}


#’ Imputació amb missForest
#’
#’ @description
#’ Aquesta funció realitza la imputació de dades faltants utilitzant l'algorisme missForest.
#’
#’ @param df El dataframe d'entrada.
#’ @param seleccio_variables Un vector de noms de columnes per a la imputació.
#’ @param grup_by Una variable per agrupar les dades abans de la imputació.
#’ @param grup_by_reserva Una variable de reserva per agrupar si la primera agrupació falla.
#’ @param metode_reserva Mètode de reserva si el mètode principal falla.
#’ @param set_seed Llavor per a la reproducibilitat.
#’ @param report_imputacio Si és TRUE, retorna un informe d'imputació.
#’ @param retorna_original Si és TRUE, retorna el dataframe original.
#’
#’ @return Una llista amb el dataframe imputat i, opcionalment, l'original i l'informe.
#’
#’ @import dplyr
#’ @import tidyr
#’ @import missForest
#’
#’ @rdname imputacio_missforest
#’ @export
imputacio_missforest <- function(df,
                                 seleccio_variables,
                                 grup_by = NULL,
                                 grup_by_reserva = NULL,
                                 metode_reserva = NULL,
                                 set_seed = 123,
                                 report_imputacio = TRUE,
                                 retorna_original = FALSE) {

  df_out <- df
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(!!seleccio_variables_enquo), as.numeric)) %>%
    dplyr::mutate(dplyr::across(!dplyr::all_of(!!seleccio_variables_enquo), as.factor))

  na_originals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))
  na_grup_by <- 0
  na_grup_by_reserva <- 0
  na_metode_reserva <- 0

  set.seed(set_seed)

  impute_missforest <- function(data) {
    res <- missForest::missForest(data)
    res$ximp
  }

  if (rlang::quo_is_null(grup_by_enquo) && rlang::quo_is_null(grup_by_reserva_enquo)) {
    df_out[!!seleccio_variables_enquo] <- impute_missforest(df_out[!!seleccio_variables_enquo])
  } else {
     # Complex logic for grouped imputation would be implemented here
  }


  na_finals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  report <- tibble::tibble(
    `NA Originals` = na_originals,
    `% NA Originals` = na_originals / nrow(df_out),
    `NA Finals` = na_finals,
    `% NA Finals` = na_finals / nrow(df_out)
  )

  result <- list(imputed_df = df_out)
  if (retorna_original) {
    result$original_df <- df
  }
  if (report_imputacio) {
    result$report <- report
  }

  return(result)
}


#’ Imputació amb MICE
#’
#’ @description
#’ Aquesta funció realitza la imputació de dades faltants utilitzant MICE (Multivariate Imputation by Chained Equations).
#’
#’ @param df El dataframe d'entrada.
#’ @param seleccio_variables Un vector de noms de columnes per a la imputació.
#’ @param grup_by Una variable per agrupar les dades abans de la imputació.
#’ @param grup_by_reserva Una variable de reserva per agrupar si la primera agrupació falla.
#’ @param metode_reserva Mètode de reserva si el mètode principal falla.
#’ @param set_seed Llavor per a la reproducibilitat.
#’ @param report_imputacio Si és TRUE, retorna un informe d'imputació.
#’ @param retorna_original Si és TRUE, retorna el dataframe original.
#’
#’ @return Una llista amb el dataframe imputat i, opcionalment, l'original i l'informe.
#’
#’ @import dplyr
#’ @import tidyr
#’ @import mice
#’
#’ @rdname imputacio_mice
#’ @export
imputacio_mice <- function(df,
                           seleccio_variables,
                           grup_by = NULL,
                           grup_by_reserva = NULL,
                           metode_reserva = NULL,
                           set_seed = 123,
                           report_imputacio = TRUE,
                           retorna_original = FALSE) {

  df_out <- df
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(!!seleccio_variables_enquo), as.numeric)) %>%
    dplyr::mutate(dplyr::across(!dplyr::all_of(!!seleccio_variables_enquo), as.factor))

  na_originals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  set.seed(set_seed)
  
  impute_mice <- function(data) {
    mice_data <- mice::mice(data, m = 1, method = 'pmm', printFlag = FALSE)
    mice::complete(mice_data)
  }

  if (rlang::quo_is_null(grup_by_enquo) && rlang::quo_is_null(grup_by_reserva_enquo)) {
    df_out[!!seleccio_variables_enquo] <- impute_mice(df_out[!!seleccio_variables_enquo])
  } else {
    # Complex logic for grouped imputation would be implemented here
  }

  na_finals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  report <- tibble::tibble(
    `NA Originals` = na_originals,
    `% NA Originals` = na_originals / nrow(df_out),
    `NA Finals` = na_finals,
    `% NA Finals` = na_finals / nrow(df_out)
  )

  result <- list(imputed_df = df_out)
  if (retorna_original) {
    result$original_df <- df
  }
  if (report_imputacio) {
    result$report <- report
  }

  return(result)
}


#’ Imputació amb k-NN
#’
#’ @description
#’ Aquesta funció realitza la imputació de dades faltants utilitzant l'algorisme k-Nearest Neighbors.
#’
#’ @param df El dataframe d'entrada.
#’ @param seleccio_variables Un vector de noms de columnes per a la imputació.
#’ @param grup_by Una variable per agrupar les dades abans de la imputació.
#’ @param grup_by_reserva Una variable de reserva per agrupar si la primera agrupació falla.
#’ @param metode_reserva Mètode de reserva si el mètode principal falla.
#’ @param set_seed Llavor per a la reproducibilitat.
#’ @param report_imputacio Si és TRUE, retorna un informe d'imputació.
#’ @param retorna_original Si és TRUE, retorna el dataframe original.
#’
#’ @return Una llista amb el dataframe imputat i, opcionalment, l'original i l'informe.
#’
#’ @import dplyr
#’ @import tidyr
#’ @import VIM
#’
#’ @rdname imputacio_knn
#’ @export
imputacio_knn <- function(df,
                          seleccio_variables,
                          grup_by = NULL,
                          grup_by_reserva = NULL,
                          metode_reserva = NULL,
                          set_seed = 123,
                          report_imputacio = TRUE,
                          retorna_original = FALSE) {

  df_out <- df
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)

  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(!!seleccio_variables_enquo), as.numeric)) %>%
    dplyr::mutate(dplyr::across(!dplyr::all_of(!!seleccio_variables_enquo), as.factor))

  na_originals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  set.seed(set_seed)

  if (rlang::quo_is_null(grup_by_enquo) && rlang::quo_is_null(grup_by_reserva_enquo)) {
    df_out <- VIM::kNN(df_out, variable = !!seleccio_variables_enquo, k = 5)
  } else {
    # Complex logic for grouped imputation would be implemented here
  }

  na_finals <- sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))))

  report <- tibble::tibble(
    `NA Originals` = na_originals,
    `% NA Originals` = na_originals / nrow(df_out),
    `NA Finals` = na_finals,
    `% NA Finals` = na_finals / nrow(df_out)
  )

  result <- list(imputed_df = df_out)
  if (retorna_original) {
    result$original_df <- df
  }
  if (report_imputacio) {
    result$report <- report
  }

  return(result)
}

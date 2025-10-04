### imputacio_val_perduts

#' @title Funció per a la imputació de valors perduts en un dataframe.
#' @description Aquesta funció ofereix diferents mètodes per a la imputació de valors `NA` en un `data.frame` o `tibble`, incloent-hi mètodes estadístics simples i tècniques avançades com `missForest`, `MICE` i `kNN`. Permet agrupar les dades per a mètodes estadístics i ofereix opcions per retornar les dades originals i imputades. Si `missForest`, `MICE` o `kNN` fallen, la funció utilitzarà el mètode d'imputació de reserva especificat.
#'
#' @param df Un `data.frame` o `tibble` d'entrada. Ha de contenir almenys una columna numèrica amb valors `NA`.
#' @param seleccio_variables Selecció de columnes a imputar.
#' @param grup_by Variable per agrupar les dades abans de la imputació. Utilitza la notació de `rlang::enquo()` (sense cometes), per exemple: `provincia` o `pais`. Aquest argument només és aplicable als mètodes de mitjana, mitjana geomètrica i mediana.
#' @param metode_imputacio El mètode d'imputació a utilitzar. Ha de ser una de les següents cadenes de text:
#'   - `'mitjana_aritmetica'`
#'   - `'mitjana_geometrica'`
#'   - `'mitjana_truncada'`
#'   - `'mitjana_winsoritzada'`
#'   - `'mediana'`
#'   - `'missForest'` (imputació per bosc aleatori)
#'   - `'MICE'` (Imputació Múltiple per Equacions Encadenades)
#'   - `'kNN'` (Imputació per K-Veïns Més Propers)
#' @param metode_reserva Mètode d'imputació alternatiu en cas que un mètode avançat falli. Pot ser qualsevol dels mètodes estadístics simples. Per defecte és `'mediana'`.
#' @param valor_trim Per defecte .10. Només per a mitjana retallada. Per defecte, retalla el 10% dels valors extrems.
#' @param retornar_originals Un valor lògic. Si és `FALSE` (per defecte), retorna només el tibble amb els valors imputats. Si és `TRUE`, retorna una llista que conté el `tibble` original i el `tibble` imputat.
#'
#' @importFrom rlang enquo quo_is_null
#' @importFrom dplyr group_by ungroup mutate across where if_else
#' @importFrom tibble tibble as_tibble
#' @importFrom psych geometric.mean winsor.mean
#' @import missForest
#' @import mice
#' @import VIM
#'
#' @return Una llista que conté `df_imputat` (el dataframe imputat) i `report` (un resum de la imputació). Si `retornar_originals` és TRUE, la llista també conté `df_original`.
#'
#' @examples
#'
#' # ---
#' # Creació del conjunt de dades real (reduït)
#' # ---
#'
#' # Per a un funcionament correcte, assegura't que els paquets com 'dplyr',
#' # 'mice', 'VIM' i 'missForest' estan instal·lats.
#'
#' df_prova <- tibble::tibble(
#'  index_id = c(NA, NA, NA, "#010", "#010", "#010", "#010", "#010", "#010", "#010", "#010", "#010", "#010", "#010"),
#'  nom = c("ASTIGI", "ASTIGI", "ASTIGI", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO", "CARMO"),
#'  t_building = rep("amphitheater", 14),
#'  provincia_romana = rep("hispania_baetica", 14),
#'  pais = rep("spain", 14),
#'  amplada_general = c(133, 130, 130, 108, 130, NA, 131, 108, 108, 90, 131, 131, NA, NA),
#'  amplada_arena = c(73, 70.9, NA, 58.8, 58.8, 58.8, 58, 58.8, 58, 58, 58, NA, NA, 55),
#'  amplada_cavea = c(30, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'  alcada_general = c(106, 107, 107, 98, 111, NA, 111, 98, 98, NA, 111, 111, NA, NA),
#'  alcada_arena = c(46, 47.6, NA, 38.6, 38.6, 39, 39, 38.6, 39, 39, 39, NA, NA, 39))
#'
#' # Visualització dels valors perduts
#' summary(df_prova)
#'
#' # ---
#' # Exemples de mètodes estadístics
#' # ---
#'
#' # Exemple 1: Imputació amb la Mitjana Aritmètica
#' resultat_mitjana <- imputacio_val_perduts(
#'   df = df_prova,
#'   seleccio_variables = c(amplada_general, amplada_arena),
#'   metode_imputacio = 'mitjana_aritmetica')
#'
#' # Exemple 2: Imputació amb la Mediana, agrupant per 'nom'
#' resultat_mediana_grup <- imputacio_val_perduts(
#'   df = df_prova,
#'   seleccio_variables = c(amplada_general, amplada_arena),
#'   grup_by = nom,
#'   metode_imputacio = 'mediana')
#'
#' @rdname imputacio_val_perduts
#'
#' @export
imputacio_val_perduts <- function(
  df, 
  grup_by = NULL,
  seleccio_variables = NULL,
  metode_imputacio = c(
    'mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana', 'missForest', 'MICE', 'kNN'),
  metode_reserva = c(
    'mediana', 'mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada'),
  valor_trim = .10,
  retornar_originals = FALSE) 
{
  ### Validació d'arguments
  metode_imputacio <- match.arg(metode_imputacio)
  
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    stop("Argument 'df' ha de ser un data.frame o un tibble.")
  }

  simple_methods <- c('mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana')
  
  # Validar o anul·lar metode_reserva
  if (metode_imputacio %in% simple_methods) {
    metode_reserva <- NULL
  } else if (!is.null(metode_reserva)) {
    metode_reserva <- match.arg(metode_reserva, choices = simple_methods)
  }

  ### Inicialitzar comptadors i guardar ordres
  original_cols <- names(df)
  df <- df %>% dplyr::mutate(.original_row_order = dplyr::row_number())
  oob_error_values <- NULL 
  fallback_count <- 0

  ### Carrega de paquets requerits
  f_requerides <- list(
    amphi_load_packages = 'R/00_setup.R')

  for (i in names(f_requerides)) {
    if (!exists(i, mode = 'function')) {
      l_file <- f_requerides[[i]]
      tryCatch({ source(l_file) }, error = function(e) { 
        stop(paste0("Error: No es pot carregar '", l_file, "'. Assegura't que l'arxiu existeix a la ruta correcta."))
      })
    }
  }

  ### Carrega de paquets
  amphi_load_packages(
    update_packages = FALSE)

  ### Captura les expressions de selecció de l'usuari amb enquo
  sel_exp_variables <- rlang::enquo(seleccio_variables)
  sel_vars_names <- names(dplyr::select(df, !!sel_exp_variables))

  ### Recompte inicial de NAs
  na_originals <- df %>% 
    dplyr::select(all_of(sel_vars_names)) %>% 
    is.na() %>% 
    sum()

  ### Funció per imputar amb mètodes estadístics
  imputar_df_estatistic <- function(data, method) {
    data_out <- data
    
    if (method == 'mitjana_aritmetica') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., mean(., na.rm = TRUE))))
    } else if (method == 'mitjana_truncada' & !is.null(valor_trim)) {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., mean(., trim = valor_trim, na.rm = TRUE))))
    } else if (method == 'mitjana_geometrica') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(. , psych::geometric.mean(., na.rm=TRUE))))
    } else if (method == 'mitjana_winsoritzada' & !is.null(valor_trim)) {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., psych::winsor.mean(., trim = valor_trim, na.rm = TRUE))))
    } else if (method == 'mediana') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., median(., na.rm = TRUE))))
    }
    return(data_out)
  }

  ### Preparació de dades per a la imputació
  df_imp <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.double) | where(is.integer), as.numeric),
      dplyr::across(where(is.character), as.factor))

  ### Imputació
  if (metode_imputacio == 'missForest') {
    if (!requireNamespace('missForest', quietly = TRUE)) {
      stop("El mètode 'missForest' requereix el paquet 'missForest'. Si us plau, instal·leu-lo amb install.packages('missForest').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'missForest' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42) # Per a reproductibilitat
      df_temp <- df_imp %>% dplyr::select(all_of(sel_vars_names)) %>% as.data.frame()
      
      imputed_result <- suppressWarnings({
        missForest::missForest(
          df_temp, 
          maxiter = 100, 
          ntree = 100)
      })

      df_imp[sel_vars_names] <- imputed_result$ximp
      oob_error_values <- imputed_result$OOBerror

    }, error = function(e) {
    
      fallback_count <<- fallback_count + 1
      df_imp <<- imputar_df_estatistic(df_imp, metode_reserva)
    
    })

  } else if (metode_imputacio == 'MICE') {
    if (!requireNamespace('mice', quietly = TRUE)) {
      stop("El mètode 'MICE' requereix el paquet 'mice'. Si us plau, instal·leu-lo amb install.packages('mice').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'MICE' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42) # Per a reproductibilitat
      imputed_data <- mice::mice(df_imp, m = 5, maxit = 50, meth = 'pmm', printFlag = FALSE)
      df_imp <- mice::complete(imputed_data)

    }, error = function(e) {
      fallback_count <<- fallback_count + 1
      warning(paste("Imputació amb 'MICE' ha fallat:", e$message, "S'utilitzarà el mètode de reserva:", metode_reserva))
      df_imp <<- imputar_df_estatistic(df_imp, metode_reserva)
    })

  } else if (metode_imputacio == 'kNN') {
    if (!requireNamespace('VIM', quietly = TRUE)) {
      stop("El mètode 'kNN' requereix el paquet 'VIM'. Si us plau, instal·leu-lo amb install.packages('VIM').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'kNN' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42) # Per a reproductibilitat
      df_imp <- VIM::kNN(df_imp, k = 5, imp_suffix = 'Imputed')

    }, error = function(e) {
      fallback_count <<- fallback_count + 1
      warning(paste("Imputació amb 'kNN' ha fallat:", e$message, "S'utilitzarà el mètode de reserva:", metode_reserva))
      df_imp <<- imputar_df_estatistic(df_imp, metode_reserva)
    })

  } else {
    ### Mètodes estadístics simples
    if (!is.null(grup_by)) {
      grup_exp_by <- rlang::syms(grup_by)
      df_imp <- df_imp %>%
        dplyr::group_by(!!!grup_exp_by) %>%
        dplyr::group_modify( ~ imputar_df_estatistic(data = .x, method = metode_imputacio)) %>%
        dplyr::ungroup()
    } else {
      df_imp <- imputar_df_estatistic(df_imp, metode_imputacio)
    }
  }

  ### Recompte final de NAs
  na_finals <- df_imp %>% 
    dplyr::select(all_of(sel_vars_names)) %>% 
    is.na() %>% 
    sum()

  ### Construcció del report
  report_info <- list(
    metode_imputacio = metode_imputacio,
    variables_imputades = sel_vars_names,
    na_originals = na_originals,
    na_restants = na_finals,
    vegades_usat_metode_reserva = fallback_count
  )
  
  # Afegir l'error OOB al report si existe
  if (!is.null(oob_error_values)) {
    report_info$error_oob <- oob_error_values
  }

  ### Restaura l'ordre original de columnes i files
  df_imp <- df_imp %>%
    dplyr::arrange(.original_row_order) %>%
    dplyr::select(any_of(original_cols))

  ### Construir la llista de resultats
  resultat <- list(
    df_imputat = df_imp
  )

  if (retornar_originals == TRUE) {
    df_original_out <- df %>%
      dplyr::arrange(.original_row_order) %>%
      dplyr::select(any_of(original_cols))
    resultat$df_original <- df_original_out
  }

  # Afegir el report al final
  resultat$report <- report_info

  return(resultat)
}
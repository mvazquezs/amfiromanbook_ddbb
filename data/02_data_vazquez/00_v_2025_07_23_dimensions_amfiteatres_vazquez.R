#' @title Carrega i combina múltiples dataframes de dades d'amfiteatres
#'
#' @description Aquesta funció cerca arxius de dades d'amfiteatres amb un patró específic
#'              en una carpeta donada, els llegeix com a dataframes amb un separador de punt i coma,
#'              i els combina en un sol `tibble` utilitzant `bind_rows`.
#'
#' @details La funció realitza els següents passos:
#'    1. Cerca arxius CSV amb un patró específic a una carpeta donada.
#'    2. Llegeix cada arxiu i els emmagatzema en una llista de dataframes.
#'    3. Renombra i homogeneïtza les columnes de cada dataframe.
#'    4. Calcula noves columnes derivades com la ràtio de l'arena, superfícies i perímetres.
#'    5. Aplica filtres opcionals per tipus d'edifici o selecció de columnes.
#'    6. Combina tots els dataframes de la llista en un únic 'tibble' ordenat.
#'
#' @param retornar_originals Lògic. Si és TRUE, retorna una llista amb els conjunts de dades orginals i el fusionat. Si és FALSE, retorna només el conjunt de dades fusionat.
#' @param filtrar_edifici Lògic. Accepta les següents opcions: amphitheater, arena_hippodrome, arena_stadium, gallo_roman, oval_structure i practice_arena.
#' @param filtrar_provincia Lògic. Accepta noms de provincies altimperials.
#' @param filtrar_pais Lògic. Accepta noms de païssos moderns en anglès.
#' @param retornar_original. Lògic (per defecte FALSE). Si és TRUE retorna el llistat de dades.
#' @param seleccionar_columnes Selecciona les columnes desitjades per la sortida del dataframe.
#'
#' @return Un dataframe de R (o una llista de dataframes) amb les dades fusionades i estructurades dels amfiteatres romans i republicans.
#'
#' @rdname taules_dimensions
#' 
#' @import dplyr
#' @import tibble
#' @import rlang
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{dplyr::mutate}}
#' @seealso \code{\link{dplyr::arrange}}
#' @seealso \code{\link{tibble::tibble}}
#' @seealso \coce{\link{rlang::enquo}}
#'
#' @examples
#' # Exemple 1: Carregar i fusionar tots els dataframes
#' # df_complet <- taules_dimensions()
#' 
#' # Exemple 2: Carregar i filtrar per tipus d'edifici específic
#' # df_filtrat <- taules_dimensions(
#' #  filtrar_edifici = 'amphitheater')
#' 
#' # Exemple 3: Carregar i seleccionar columnes específiques
#' # df_hispania_sel <- taules_dimensions(
#' #  filtrar_edifici = 'amphitheater',
#' #  filtrar_provincia = 'hispania',
#' #  filtrar_pais = NULL,
#' #  seleccionar_columnes = c(contains('amplada'), contains('alcada'), starts_with('nombre'), -contains('cavea'), 'bib'),
#' #  retornar_originals = FALSE)
#' 
#' #' # Exemple 4: Incompatibilitat entre `filtrar_provincia` i `filtrar_pais`
#' # df_spain_france_sel <- taules_dimensions(
#' #  filtrar_edifici = 'amphitheater',
#' #  filtrar_provincia = NULL,
#' #  filtrar_pais = c('spain', 'france'),
#' #  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), starts_with('nombre') 'bib'),
#' #  retornar_originals = FALSE)
#'
#' @export
taules_dimensions <- function(
  seleccionar_columnes = NULL,
  filtrar_edifici = NULL,
  filtrar_provincia = NULL,
  filtrar_pais = NULL,
  retornar_originals = FALSE) {

### Double Check 01
  if(!is.null(filtrar_provincia) && !is.null(filtrar_pais)) {
  
    warning(
      "S'han proporcionat filtres per província i país. Ambdues combinacions són incompatibles.")
  
  }

### Carrega de paquets requerits
### Carrega de funcions necessaries
  source('R/00_v_2025_07_23_setup_dir.R')
  source('R/01_v_2025_07_23_setup_load.R')

  seleccionar_columnes <- rlang::enquo(seleccionar_columnes)

### Carrega de paquets
  amphi_tool.required_packages(
    locale = 'es_ES.UTF-8',
    update_packages = FALSE)

### Carrega de fitxers
  l_fitxers <- amphi_list.dir_files(
	  home_folder = 'data/02_data_vazquez',
    recursive = TRUE,
    pattern = 'csv')[[1]][-1]

### Carrega de dades
  l_data_vazquez <- amphi_load.read_data(
    l_files = l_fitxers,
    type_file = 'csv',
    sep = ';',
    dec = '.',
    skip_rows = 0,
    na_strings = NULL,
    clean_names = TRUE)

### Assignar noms de cada fitxer
  names(l_data_vazquez) <- stringr::str_sub(
    l_fitxers, 
    start = 37, 
    end = -5)
    
  columnes_golvin <- c(
    'index_id', 'nom', 
    'amplada_arena', 'alcada_arena', 'amplada_general', 'alcada_general',
    'nombre_places', 'amplada_cavea')

  columnes_vazquez <- c(
    'golvin_class', 'golvin_name',
    'arena_max', 'arena_min', 'overall_max', 'overall_min',
    'seat_est', 'cavea_wide')

  columnes <- setNames(columnes_vazquez, columnes_golvin)

### Homogeneitzar les dades
  for(i in seq_along(l_data_vazquez)) {
   
    l_data_vazquez[[i]] <- l_data_vazquez[[i]] %>%
      dplyr::rename(!!!columnes, pais = mod_country) %>%
      dplyr::mutate(
        provincia_romana = paste0(names(l_data_vazquez)[[i]]),
        ratio_arena = amplada_arena / alcada_arena,
        ratio_general = amplada_general / alcada_general,
        superficie_arena = amplada_arena / 2 * alcada_arena / 2 * pi,
        superficie_general = amplada_general / 2 * alcada_general / 2 * pi,
        superficie_cavea = superficie_general - superficie_arena,
        perimetre_arena = pi * (amplada_arena / 2 + alcada_arena / 2),
        perimetre_general = pi * (amplada_general / 2 + alcada_general / 2),
        ratio_cavea = superficie_arena / superficie_general)

    ### Argument 'filtrar_edifici'
      if(!is.null(filtrar_edifici)) {

        l_data_vazquez[[i]] <- l_data_vazquez[[i]] %>%
        dplyr::filter_at(
          vars('t_building'), all_vars(. %in% filtrar_edifici)) %>%
          droplevels()
  
      }

    ### Argument 'filtrar_provincia'
      if(!is.null(filtrar_provincia) & is.null(filtrar_pais)) {

        l_data_vazquez[[i]] <- l_data_vazquez[[i]] %>%
        dplyr::filter(
          stringr::str_detect(provincia_romana, paste(filtrar_provincia, collapse = '|'))) %>%
          droplevels()
  
      }

    ### Argument 'filtrar_pais'
      if(!is.null(filtrar_pais) & is.null(filtrar_provincia)) {

        l_data_vazquez[[i]] <- l_data_vazquez[[i]] %>%
        dplyr::filter(
          stringr::str_detect(pais, paste(filtrar_pais, collapse = '|'))) %>%
          droplevels()
  
      }
    

    ### Argument 'seleccionar_columnes'
      if(!rlang::quo_is_null(seleccionar_columnes)) {

        l_data_vazquez[[i]] <- l_data_vazquez[[i]] %>% 
          dplyr::select('index_id', 'nom', 't_building', 'provincia_romana', 'pais', !!seleccionar_columnes)

      }
  }    

### 'bind_rows' de la llista
  df_data_vazquez <- data.table::rbindlist(l_data_vazquez) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(index_id, nom, provincia_romana, pais) 
    

  if (retornar_originals) {

    return(l_data_vazquez)

  } else {

    return(df_data_vazquez)
  
  }
}
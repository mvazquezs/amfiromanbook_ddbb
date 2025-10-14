#' @title Extracció, estructuració i fusió de dades d'amfiteatres romans
#' 
#' @description Aquesta funció processa dades d'amfiteatres romans i republicans extretes de taules,
#'              calcula mètriques derivades i fusiona les dades en un únic dataframe.
#' 
#' @details La funció realitza els següents passos:
#'          1. Extreu les dades de la taula d'amfiteatres romans.
#'          2. Calcula les columnes derivades 'sasc' (percentatge de l'arena respecte a la cavea) i 'ab_forma_arena' (relació d'aspecte de l'arena).
#'          3. Extreu les dades de la taula d'amfiteatres republicans.
#'          4. Fusiona les dues taules, conservant totes les files de totes dues taules.
#'          5. Afegeix etiquetes descriptives als noms de les columnes del dataframe resultant.
#' @param filtrar_provincia Lògic. Accepta noms de provincies altimperials.
#' @param filtrar_pais Lògic. Accepta noms de païssos moderns en anglès.
#' @param seleccionar_columnes Selecciona les columnes desitjades per la sortida del dataframe.
#' @param retornar_originals Lògic. Si és TRUE, retorna una llista amb els conjunts de dades originals i el fusionat. Si és FALSE, retorna només el conjunt de dades fusionat.
#' @param format_llarg Lògic. Si és TRUE, el dataframe de sortida es retorna en format llarg (pivotat).
#' 
#' @return Un dataframe de R (o una llista de dataframes) amb les dades fusionades i estructurades dels amfiteatres romans i republicans.
#' 
#' @rdname load_dimensions_golvin
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
#' # df_complet <- load_dimensions_golvin()
#' 
#' # Exemple 2: Carregar i seleccionar columnes específiques
#' # df_filtrat <- tableau_dimension(
#' #  seleccionar_columnes = contains('_'),
#' #  retornar_originals = FALSE)
#' 
#' # Exemple 3: Carregar i seleccionar columnes específiques
#' # df_columnes_sel <- load_dimensions_golvin(
#' #  filtrar_provincia = 'hispania',
#' #  filtrar_pais = NULL,
#' #  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), 'bib'), 
#' #  retornar_originals = FALSE)
#'
#' # Exemple 4: Carregar i pivotar les dades a format llarg
#' # df_llarg <- load_dimensions_golvin(format_llarg = TRUE)
#' 
#' @export
load_dimensions_golvin <- function(
  filtrar_provincia = NULL,
  filtrar_pais = NULL,
  seleccionar_columnes = NULL,
  retornar_originals = FALSE,
  format_llarg = FALSE) {

### Double Check 01
  if(!is.null(filtrar_provincia) && !is.null(filtrar_pais)) {
  
    warning(
      "S'han proporcionat filtres per província i país. Ambdues combinacions són incompatibles.")
    
  }

### Carrega de paquets requerits
### Carrega de funcions necessaries
  source('R/00_setup.R')
  source('R/01_data_load.R')

  seleccionar_columnes <- rlang::enquo(seleccionar_columnes)

### Carrega de paquets
  amphi_load_packages(
    update_packages = FALSE)

  ### Extracció i càlculs de la taula d'amfiteatres romans
  ### Identificació de taula 01 (pàgina 43)
    tableau_01_pp_43 <- tibble::tibble(
      index_id = as.factor(c(
        paste0('#', sprintf('%0.3d', 1:11)))),
      nom = c(
      'CUMAE', 'POMPEI', 'ABELLA', 'TEANUM CALES', 'PUTEOLI P. AMPHI.', 
      'TELESIA', 'PAESTUM 1', 'SUTRIUM', 'FERENTIUM', 'CARMO', 'UCUBI'),
      provincia_romana = c(
        rep('italia_i_campania_et_latium', times = 6),
        rep('italia_iii_lucania_et_bruttii', times = 1),
        rep('italia_vii_etruria', times = 2),
        rep('hispania_baetica', times = 2)),
      pais = c(
        rep('italy', times = 9),
        rep('spain', times = 2)),
      amplada_arena = c(NA, 66, NA, NA, 69, 68, 36.8, 50, NA, 58.8, 35),
      alcada_arena = c(NA, 34.5, NA, NA, 35, 46, 34.4, NA, NA, NA, NA),
      amplada_general = c(NA, 134, 79, NA, 130, 99, 77.3, 85, 67.5, 131.2, NA),
      alcada_general = c(NA, 102.5, 53, NA, 95, 77, 54.8, 75, 40, 111.4, NA),
      nombre_places = c(NA, 22497, NA, NA, 19507, 8825, 4480, 8590, NA, 24195, NA),
      amplada_cavea = c(NA, 34, NA, NA, 30.5, 15.5, 10.25, 17.5, NA, 36.2, NA))


  ### Extracció i càlculs de la taula d'amfiteatres romans
  ### Identificació de taula 28 (pàgines 284 - 285)
    tableau_28_pp_284_285 <- tibble::tibble(
      index_id = as.factor(c(
        paste0('#', sprintf('%0.3d', 12:25)), '#025b',
        paste0('#', sprintf('%0.3d', 26:27)),
        paste0('#', sprintf('%0.3d', 29:30)), '#030b', 
        paste0('#', sprintf('%0.3d', 31:35)),
        paste0('#', sprintf('%0.3d', 38:44)), 
        '#046', '#047', '#051', '#052',
        paste0('#', sprintf('%0.3d', 54:57)),
        paste0('#', sprintf('%0.3d', 59:62)),
        '#064', '#066', 
        paste0('#', sprintf('%0.3d', 70:76)))),
      nom = c(
        'LUCERIA', 'RUSELLAE', 'VELEIA', 'SEGUSIUM', 'CEMENELUM',
        'VINDONISSA', 'VETERA', 'IULIOBONA', 'AQUAE NERI', 'AUGUSTA SYLVANECTUM',
        'SEGODUNUM', 'ALBA FUCENS', 'LEPTIS MAGNA', 'THYSDRUS P. AMPHI. 1', 'ALBANUM',
        'HERDONIAE', 'OCTODURUS', 'AUGUSTA RAURICA 1 A', 'THEVESTIS 1', 'THEVESTIS 2', 
        'TOMEN Y MUR','CHARTERHOUSE', 'CORINIUM DOBUNNORUM', 'DURNOVARIA', 'NOV. REGENSIUM',
        'NOVIAMAGUS BATAVORUM', 'POROLISSUM', 'CALLEVA ATREBATUM', 'AUGUSTA TREVERORUM', 'ENGE',
        'LUGDUNUM CONVENARUM', 'GEMELLAE', 'TIGAVA CASTRA', 'MICIA', 'VIRUNUM',
        'FLAVIA SOLVA', 'CAESARODUNUM TURONUM', 'EPOREDIA', 'FORUM CORNELII', 'INTERAMNIA PRAETUTTIORUM',
        'POLLENTIA', 'LAMBAESIS 1', 'UTICA', 'THIBARI', 'AGBIA',
        'SERESSI', 'ULISPPIRA', 'THAENAE', 'RUSICADE', 'AUGUSTA RAURICA',
        'ALBINGAUNUM', 'CYRENE', 'PTOLEMAIS'),
      provincia_romana = c(
        rep('italia_ii_apulia_et_calabria', times = 1),
        rep('italia_vii_etruria', times = 1),
        rep('italia_viii_aemilia', times = 1),
        rep('alpes_cottiae', times = 1),
        rep('alpes_maritimae', times = 1),
        rep('germania_superior', times = 1),
        rep('germania_inferior', times = 1),
        rep('gallia_belgica', times = 1),
        rep('gallia_lugdunensis', times = 1),
        rep('gallia_belgica', times = 1),
        rep('gallia_aquitania', times = 1),
        rep('italia_iiii_samnium', times = 1),
        rep('africa_proconsularis', times = 2),
        rep('italia_i_campania_et_latium', times = 1),
        rep('italia_ii_apulia_et_calabria', times = 1),
        rep('germania_superior', times = 1),
        rep('pannonia_superior', times = 1),
        rep('africa_proconsularis', times = 2),
        rep('britania', times = 5),
        rep('germania_inferior', times = 1),
        rep('dacia', times = 1),
        rep('britania', times = 1),
        rep('gallia_belgica', times = 1),
        rep('germania_superior', times = 1),
        rep('gallia_aquitania', times = 1),
        rep('numidia', times = 1),
        rep('mauretania_caesariensis', times = 1),
        rep('dacia', times = 1),
        rep('noricum', times = 2),
        rep('gallia_lugdunensis', times = 1),
        rep('italia_regio_xi_gallia_transpadana', times = 1),
        rep('italia_viii_aemilia', times = 1),
        rep('italia_v_picenum', times = 1),
        rep('italia_viiii_liguria', times = 1),
        rep('numidia', times = 1),
        rep('africa_proconsularis', times = 6),
        rep('numidia', times = 1),
        rep('germania_superior', times = 1),
        rep('italia_viiii_liguria', times = 1),
        rep('crete_et_cyrenaica', times = 2)),
      pais = c(
        rep('italy', times = 4),
        rep('france', times = 1),
        rep('switzerland', times = 1),
        rep('germany', times = 3),
        rep('france', times = 2),
        rep('italy', times = 1),
        rep('lybia', times = 1),
        rep('tunisia', times = 1),
        rep('italia', times = 2),
        rep('switzerland', times = 1),
        rep('austria', times = 1),
        rep('algeria', times = 2),
        rep('wales', times = 1),
        rep('england', times = 4),
        rep('netherland', times = 1),
        rep('romania', times = 1),
        rep('england', times = 1),
        rep('germany', times = 1),
        rep('switzerland', times = 1),
        rep('france', times = 1),
        rep('algeria', times = 2),
        rep('romania', times = 1),
        rep('austria', times = 2 ),
        rep('france', times = 1),
        rep('italy', times = 4),
        rep('algeria', times = 1),
        rep('tunisia', times = 6),
        rep('algeria', times = 1),
        rep('switzerland', times = 1),
        rep('italy', times = 1),
        rep('libya', times = 2)),
      amplada_arena = c(
        75.2, 30.0, 37.0, 44.0, 46.0, 64.0, 55.5, 48.0, 50.0, 41.5,
        41.5, 64.0, 57.0, 49.0, 67.5, 44.6, 74.0, 49.0, 52.8, 52.8, 
        35.0, 35.0, 49.0, 58.0, 56.0, 60.0, 60.0, 49.0, 70.5, 27.6,
        53.6, 72.0, 44.0, 31.6, NA, 85.0, 68.0, 67.0, 67.0, NA,
        NA, 68.0, 50.0, 30.8, NA, 41.0, 45.0, 42.0, 50.0, 48.0,
        50.0, 32.7, 47.5),
      alcada_arena = c(
        43.2, 20.0, 25.5, 36.0, 34.8, 51.0, 42.5, 45.0, 30.0, 34.5,
        29.5, 37.0, 47.0, 40.0, 45.0, 28.4, 62.0, 36.0, 39.5, 39.5, 
        28.0, 24.0, 41.0, 47.0, 47.0, 40.0, 40.0, 40.0, 48.9, 23.5,
        25.6, 52.0, 26.0, 29.5, NA, 35.0, 50.0, 42.0, 42.0, NA,
        NA, 55.0, 30.0, 21.0, NA, 26.0, 35.0, 30.0, 36.0, 33.0,
        30.0, 28.8, 44.5),
      amplada_general = c(
        126.8, 18.0, 54.9, 60.0, 67.2, 112.0, 98.0, 78.0, 70.0, 90.0,
        110.0, 103.0, 121.0, 79.0, 91.5, 75.5, 118.0, 81.0, 83.0, 94.8,
        52.0, 50.0, 89.0, 88.0, 88.0, 80.0, NA, 80.0, 100.0, 42.6,
        83.0, 84.0, 56.0, 43.6, 96.0, 97.0, 145.0, 96.0, 108.0, 73.9,
        112.9, 88.0, 118.0, 61.2, 60.0, 69.0, 60.0, 66.0, 78.0, 102.0,
        70.0, NA, 89.0),
      alcada_general = c(
        94.5, 37.0, 44.1, 52.0, 56.0, 98.0, 84.0, 75.0, 50.0, 83.0,
        97.0, 76.0, 111.0, 70.0, 69.0, 59.4, 106.0, 68.0, 70.0, 81.5,
        46.0, 39.0, 81.0, 77.0, NA, NA, NA, 70.0, 79.0, 38.5,
        55.0, 64.0, 37.0, 41.5, 42.0, 45.0, 127.0, 72.0, 81.0, 56.2,
        NA, 75.0, 98.0, 51.5, NA, 54.0, 50.0, 54.0, 59.0, 87.0,
        50.0, NA, 86.0),
      nombre_places = c(
        17149, 2309, 2901, 3015, 4245, 15142, 11532, 7245, 3926, 11856,
        18546, 10720, 21111, 7009, 6432, 6318, 15550, 7351, 7312, 11075,
        2772, 2179, 10210, 7952, NA, NA, NA, 7147, 8742, 1946,
        6269, 3204, 1822, 1722, NA, 2729, 29481, 8046, 11914, NA,
        NA, 5615, 19760, 4918, NA, 5222, 2797, 4523, 5501, 14313,
        3926, NA, 10878),
      amplada_cavea = c(
        25.8, 9.0, 8.95, 8.0, 10.6, 24.0, 21.25, 15.0, 10.0, 24.5,
        34.25, 19.5, 32.0, 15.0, 12.0, 15.45, 22.0, 16.0, 15.1, 21.0,
        8.5, 7.5, 20.0, 15.0, NA, NA, NA, 15.5, 14.75, 7.5,
        14.7, 6.0, 6.0, 6.0, NA, 6.0, 38.5, 14.5, 20.5, NA, 
        NA, 10.0, 34.0, 15.2, NA, 14.0, 7.5, 12.0, 14.0, 27.0,
        10.0, NA, 20.75))


  ### Extracció i càlculs de la taula d'amfiteatres romans
  ### Identificació de taula 29 (pàgines 285 - 286)
    tableau_29_pp_285_286 <- tibble::tibble(
      index_id = as.factor(c(
        paste0('#', sprintf('%0.3d', 77:81)), '#081b',
        paste0('#', sprintf('%0.3d', 82:84)), '#084b',
        paste0('#', sprintf('%0.3d', 85:86)),
        paste0('#', sprintf('%0.3d', 88:93)),
        paste0('#', sprintf('%0.3d', 95:98)), '#098b',
        paste0('#', sprintf('%0.3d', 99:108)),
        paste0('#', sprintf('%0.3d', 110:131)))),
      nom = c(
        'EMERITA AUGUSTA', 'ANCONA', 'AUGUSTA BAGIENNORUM', 'CARSULAE', 'IOL CAESAREA 1',
        'IOL CAESAREA 2', 'CASINUM', 'SYRACUSAE', 'NUCERIA CONSTANTIA', 'NUCERIA CONSTANTIA',
        'LUGDUNUM 1', 'SPOLETIUM 1', 'SEGOBRIGA', 'VENUSIA', 'PATAVIUM',
        'ASCULUM PICENUM', 'GRUMENTUM', 'EMPORIAE', 'CARTHAGO 1', 'PARMA',
        'MEDIOLANUM', 'TOLOSA 1', 'TOLOSA 2', 'FALERIO', 'MEDIOLANUM SANTONUM',
        'CONIMBRIGA', 'SAMAROBRIVA', 'SURGERES', 'AVENTICUM', 'LIBARNA',
        'FRUSINO', 'DEVA 2', 'ISCA SILURUM', 'SARMIZEGETUSA', 'LAMBAESIS 2',
        'THYSDRUS P. AMPHI. 2', 'SUFETULA', 'LEPTIS MINOR', 'UTHINA', 'THIGNICA',
        'BULLA REGIA', 'ACHOLLA', 'MACTARIS', 'SABRATHA', 'THUBURBO MAIUS',
        'CARNUNTUM A. MIL.', 'CARNUNTUM A. CIV.', 'AQUINCUM A. MIL.', 'AQUINCUM A. CIV.', 'CORINTHUS',
        'MARCIANOPOLIS', 'VENTA SILURUM', 'DURA EUROPOS', 'TIPASA', 'ALERIA'),
       provincia_romana = c(
        rep('hispania_lusitania', times = 1),
        rep('italia_v_picenum', times = 1),
        rep('italia_viiii_liguria', times = 1),
        rep('italia_vi_umbria_et_ager_gallicus', times = 1),
        rep('mauretania_caesariensis', times = 2),
        rep('italia_i_campania_et_latium', times = 1),
        rep('sicilia', times = 1),
        rep('italia_i_campania_et_latium', times = 2),
        rep('gallia_lugdunensis', times = 1),
        rep('italia_vi_umbria_et_ager_gallicus', times = 1),
        rep('hispania_tarraconensis', times = 1),
        rep('italia_ii_apulia_et_calabria', times = 1),
        rep('italia_x_venetia_et_histria', times = 1),
        rep('italia_v_picenum', times = 1),
        rep('italia_iii_lucania_et_bruttii', times = 1),
        rep('hispania_tarraconensis', times = 1),
        rep('africa_proconsularis', times = 1),
        rep('italia_viii_aemilia', times = 1),
        rep('italia_regio_xi_gallia_transpadana', times = 1),
        rep('gallia_narbonensis', times = 2),
        rep('italia_v_picenum', times = 1),
        rep('gallia_aquitania', times = 1),
        rep('hispania_lusitania', times = 1),
        rep('gallia_belgica', times = 1),
        rep('gallia_aquitania', times = 1),
        rep('germania_superior', times = ),
        rep('italia_viiii_liguria', times = 1),
        rep('italia_i_campania_et_latium', times = 1),
        rep('britania', times = 2),
        rep('dacia', times = 1),
        rep('numidia', times = 1),
        rep('africa_proconsularis', times = 10),
        rep('pannonia_superior', times = 2),
        rep('pannonia_inferior', times = 2),
        rep('achaea', times = 1),
        rep('moesia_inferior', times = 1),
        rep('britania', times = 1),
        rep('syria', times = 1),
        rep('mauretania_caesariensis', times = 1),
        rep('corsica_et_sardinia', times = 1)),
      pais = c(
        rep('spain', times = 1),
        rep('italy', times = 3),
        rep('algeria', times = 2),
        rep('italy', times = 4),
        rep('france', times = 1),
        rep('italy', times = 1),
        rep('spain', times = 1),
        rep('italy', times = 4),
        rep('spain', times = 1),
        rep('tunisia', times = 1),
        rep('italy', times = 2),
        rep('france', times = 2),
        rep('italy', times = 1),
        rep('france', times = 1),
        rep('portugal', times = 1),
        rep('france', times = 2),
        rep('switzerland', times = 1),
        rep('italy', times = 2),
        rep('england', times = 1),
        rep('wales', times = 1),
        rep('romania', times = 1),
        rep('algeria', times = 1),
        rep('tunisia', times = 8),
        rep('lybia', times = 1),
        rep('tunisia', times = 1),
        rep('austria', times = 2),
        rep('hungary', times = 2),
        rep('greece', times = 1),
        rep('bulgary', times = 1),
        rep('wales', times = 1),
        rep('syria', times = 1),
        rep('algeria', times = 1),
        rep('france', times = 1)),
      amplada_arena = c(
        64.5, 67.0, NA, 62.1, 101.0, 101.0, 52.0, 69.8, 58.0, 58.0,
        67.8, NA, 40.5, 58.0, 76.4, NA, 60.0, 75.0, 64.7, 72.0,
        74.0, 59.0, 59.0, NA, 65.2, 54.0, 55.0, NA, 51.0, 66.0, 
        NA, 56.7, 55.2, 66.0, 68.0, 60.0, 41.0, 45.0, 50.0, 39.0,
        40.0, 48.0, 38.0, 63.0, 45.0, 72.2, 68.0, 88.0, 51.0, 58.0,
        50.0, 44.0, 32.0, 57.0, 29.6),
      alcada_arena = c(
        41.2, 53.0, NA, 37.6, 44.0, 44.0, 36.0, 39.7, 34.0, 34.0,
        41.9, NA, 34.0, 37.0, 39.5, NA, 40.0, 43.0, 36.7, 43.0,
        44.0, 49.0, 49.0, NA, 39.2, 40.0, 42.0, NA, 39.0, 36.7,
        NA, 48.3, 41.0, 47.0, 55.0, 40.0, 29.0, 26.0, 35.0, 26.5,
        20.0, 32.0, 25.0, 49.0, 32.0, 44.3, 52.0, 66.4, 42.0, 30.0,
        40.0, 35.0, 26.0, 35.0, 24.0),
      amplada_general = c(
        126.3, 111.0, 104.0, 86.5, 124.0, 134.0, 85.0, 146.8, 95.0, 125.0,
        80.3, 115.0, 75.0, 98.0, 102.5, 148.0, 90.0, 88.0, 120.0, 135.0,
        155.0, 84.0, 111.0, 88.8, 126.4, 94.0, 114.0, 75.0, 99.0, 88.0,
        78.0, 95.5, 80.1, 88.0, 104.6, 92.0, 71.0, 81.0, 96.0, 56.0, 
        60.0, 72.0, 62.0, 115.0, 77.0, 98.0, 122.0, 131.0, 86.5, 98.0,
        70.0, 63.0, 50.0, 77.0, 39.6),
      alcada_general = c(
        102.6, 97.0, 78.0, 62.0, 67.0, 77.0, 69.0, 118.7, 72.0, 102.0,
        54.4, 85.0, 68.5, 77.0, 65.5, 125.0, 70.0, 56.0, 93.0, 108.0,
        125.0, 74.0, 101.0, 80.0, 101.6, 80.0, 100.0, 55.0, 87.0, 58.7,
        57.0, 86.6, 66.6, 69.0, 94.0, 72.0, 59.0, 62.0, 81.0, 46.0,
        40.0, 56.0, 49.0, 99.0, 62.5, 76.0, 106.0, 107.0, 75.5, 70.0,
        60.0, 53.0, 44.0, 55.0, 34.0),
      nombre_places = c(
        20225, 14168, NA, 5945, 7586, 11533, 7840, 28773, 9558, 21162, 
        2999, NA, 7383, 10602, 7256, NA, 7657, 3343, 17250, 22548,
        31649, 6528, 16336, NA, 20197, 10524, 17848, NA, 1306, 5386,
        NA, 10861, 6036, 5831, 11962, 8293, 5890, 7503, 11832, 3028,
        3141, 4900, 4099, 16100, 6621, 8343, 18449, 1049, 8617, 10053,
        4319, 3532, 2686, 4398, 1248),
      amplada_cavea = c(
        30.9, 22.0, NA, 12.2, 11.5, 16.5, 16.5, 38.5, 18.5, 33.5,
        6.25, NA, 17.25, 20.0, 13.05, NA, 15.0, 6.5, 27.65, 31.5,
        40.5, 12.5, 26.0, NA, 30.6, 20.0, 29.5, NA, 24.0, 11.0,
        NA, 19.4, 12.45, 11.0, 18.3, 16.0, 15.0, 18.0, 23.0, 8.5,
        10.0, 12.0, 12.0, 25.0, 16.0, 12.9, 27.0, 21.5, 17.75, 20.0,
        10.0, 9.5, 9.0, 10.0, 5.0))


  ### Extracció i càlculs de la taula d'amfiteatres romans
  ### Identificació de taula 30 (pàgines 286 - 287)
    tableau_30_pp_286_287 <- tibble::tibble(
        index_id = as.factor(c(
          paste0('#', sprintf('%0.3d', 132:133)), '#133b',
          paste0('#', sprintf('%0.3d', 134:160)),
          paste0('#', sprintf('%0.3d', 163:167)), '#167b',
          paste0('#', sprintf('%0.3d', 168:169)), '#169b',
          paste0('#', sprintf('%0.3d', 170:177)),
          paste0('#', sprintf('%0.3d', 179:190)))), 
        nom = c(
          'AUGUSTA PRAETORIA', 'LUPIAE 1', 'LUPIAE 2', 'POLA 1', 'LUCA',
          'VESUNNA PETRUCORIORUM', 'LIMONUM PICTONUM', 'FORUM IULII', 'TARRACO',
          'FLORENTIA', 'AQUILEIA', 'OCRICULUM', 'LUNA', 'INTERAMNA NAHARS', 
          'LUCUS FERONIAE', 'FALERII NOVI', 'AQUINUM', 'TARRACINA', 'ARRETIUM 1',
          'VERONA', 'POLA 2', 'ROMA A. FLAVIEN', 'PUTEOLI G. AMPHI.', 'ARELATE',
          'NEMAUSUS', 'AUG. LEMOVICUM', 'AUGUSTODUNUM', 'VESONTIO', 'BAETERRAE',
          'NARBO MARTIUS', 'VOLSINII', 'THERMAE HIMERAE', 'URBS SALVIA', 'AMITERNUM', 
          'COL. ULPIA TRAIANA 1', 'COL. ULPIA TRAIANA 2', 'PAESTUM 2', 'ARRETIUM 2', 'SPOLETIUM 2',
          'DIVODURUM MEDIOMATRICORUM', 'LUGDUNUM 2', 'ARIMINUM', 'TIBUR', 'CARTHAGO 2',
          'ITALICA', 'CYZICUS', 'PERGAMUM', 'CAPUA G. AMPHI.', 'CATANA',
          'SALONAE', 'CARALES', 'SICCA VENERIA', 'TUSCULUM', 'THAPSUS',
          'THYSDRUS G. AMPHI.', 'BARARUS', 'BURDIGALA', 'ROMA A. CASTRENSE', 
          'CASTRA ALBANA'),
        provincia_romana = c(
          rep('italia_regio_xi_gallia_transpadana', times = 1),
          rep('italia_ii_apulia_et_calabria', times = 2),
          rep('italia_x_venetia_et_histria', times = 1),
          rep('italia_vii_etruria', times = 1), 
          rep('gallia_aquitania', times = 2),
          rep('gallia_narbonensis', times = 1),
          rep('hispania_tarraconensis', times = 1),  
          rep('italia_vii_etruria', times = 1),
          rep('italia_x_venetia_et_histria', times = 1), 
          rep('italia_vi_umbria_et_ager_gallicus', times = 1),
          rep('italia_viiii_liguria', times = 1),
          rep('italia_vi_umbria_et_ager_gallicus', times = 1),
          rep('italia_vii_etruria', times = 2),
          rep('italia_i_campania_et_latium', times = 2),
          rep('italia_vii_etruria', times = 1),
          rep('italia_x_venetia_et_histria', times = 2),
          rep('italia_i_campania_et_latium', times = 2), 
          rep('gallia_narbonensis', times = 2),
          rep('gallia_aquitania', times = 1),
          rep('gallia_lugdunensis', times = 1),
          rep('germania_superior', times = 1),
          rep('gallia_narbonensis', times = 2),
          rep('italia_vii_etruria', times = 1),
          rep('sicilia', times = 1),
          rep('italia_v_picenum', times = 1),
          rep('italia_iiii_samnium', times = 1), 
          rep('germania_inferior', times = 2),
          rep('italia_iii_lucania_et_bruttii', times = 1),
          rep('italia_vii_etruria', times = 1), 
          rep('italia_vi_umbria_et_ager_gallicus', times = 1),
          rep('gallia_belgica', times = 1),
          rep('gallia_lugdunensis', times = 1),
          rep('italia_viii_aemilia', times = 1), 
          rep('italia_iiii_samnium', times = 1),
          rep('africa_proconsularis', times = 1),
          rep('hispania_baetica', times = 1),
          rep('bithynia_et_pontus', times = 1), 
          rep('asia', times = 1), 
          rep('italia_i_campania_et_latium', times = 1),
          rep('sicilia', times = 1),
          rep('dalmatia', times = 1),
          rep('corsica_et_sardinia', times = 1), 
          rep('africa_proconsularis', times = 1),
          rep('italia_i_campania_et_latium', times = 1),
          rep('africa_proconsularis', times = 3), 
          rep('gallia_aquitania', times = 1), 
          rep('italia_i_campania_et_latium', times = 2)),
        pais = c(
          rep('italy', times = 3),
          rep('croatia', times = 1),
          rep('italy', times = 1),
          rep('france', times = 3),
          rep('spain', times = 1), 
          rep('italy', times = 11),
          rep('croatia', times = 1),
          rep('italy', times = 2),
          rep('france', times = 4),
          rep('germania', times = 1),
          rep('france', times = 2),
          rep('italy', times = 4),
          rep('germany', times = 2),
          rep('italy', times = 3),
          rep('france', times = 2), 
          rep('italy', times = 2),
          rep('tunisia', times = 1),
          rep('spain', times = 1),
          rep('turkey', times = 2), 
          rep('italy', times = 2), 
          rep('croatia', times = 1), 
          rep('italy', times = 1),
          rep('tunisia', times = 1), 
          rep('italy', times = 1),
          rep('tunisia', times = 3),
          rep('france', times = 1),
          rep('italy', times = 2)),
        amplada_arena = c(
          42.0, 53.1, 53.1, 67.9, 80.0, 65.0, 72.3, 67.7, 84.4, 64.0,
          72.0, 64.0, 57.7, 52.2, 34.1, 54.3, 55.0, NA, 71.9, 75.7,
          67.9, 79.4, 74.8, 69.3, 69.1, 68.0, 74.0, 62.0, 74.0, 75.0,
          58.7, 51.0, 60.6, 64.0, 58.5, 58.5, 56.9, 71.9, NA, 65.1,
          67.8, 74.0, 61.0, 64.7, 71.5, NA, 51.0, 76.1, 81.1, 64.3,
          46.8, 70.0, 49.0, 67.0, 64.5, 63.8, 69.8, NA, 67.5),
        alcada_arena = c(
          31.0, 34.2, 34.2, 41.7, 53.0, 41.4, 47.0, 39.7, 55.2, 40.0,
          46.0, 42.0, 39.4, 28.7, 32.2, 32.7, 36.0, NA, 42.7, 44.4,
          41.7, 47.2, 42.0, 39.8, 38.4, 48.0,  49.0, 36.6, 44.4, 46.6,
          42.2, 30.0, 38.6, 42.0, 49.0, 49.0, 34.4, 42.7, NA, 41.5,
          41.9, 44.0, 41.0, 36.7, 49.0, NA, 37.0, 45.8, 58.3, 40.2,
          33.2, 50.0, 30.0, 43.0, 38.8, 37.5, 46.7, NA, 45.0),
        amplada_general = c(
          86.0, 94.0, 101.9, 123.0, 124.0, 129.0, 155.8, 113.7, 148.1, 113.0,
          142.0, 120.0, 72.8, 96.5, 46.1, 88.0, 115.0, 90.0, 109.4, 152.4,
          132.5, 187.8, 149.0, 136.2, 133.4, 137.0, 154.0, 110.0, 103.6, 121.6,
          100.7, 87.0, 96.6, 90.0, 90.5, 96.5, 84.9, 121.4, 115.0, 148.0, 143.3,
          117.7, 85.0, 156.0, 156.5, 150.0, 136.2, 165.0, 143.8, 124.8,
          92.8, 100.0, 73.0, 80.0, 147.9, 98.0, 132.3, 88.0, 116.5),
        alcada_general = c(
          73.0, 75.0, 83.0, 96.5, 96.0, 105.4, 130.5, 85.7, 118.9, 89.0,
          118.0, 98.0, 54.5, 73.0, 44.2, 66.4, 96.0, 68.0, 80.0, 123.2,
          105.1, 155.6, 116.0, 107.6, 101.4, 116.0, 130.0, 84.6, 74.0, 93.2,
          84.2, 66.0, 74.6, 68.0, 81.5, 87.0, 62.4, 92.0, 85.0, 124.3,
          117.4, 88.1, 65.0, 128.0, 134.0, 100.0, 107.4, 135.0, 121.0, 100.7,
          79.2, 80.0, 54.0, 58.0, 122.2, 73.5, 110.6, 75.8, 94.0),
        nombre_places = c(
          9770, 10276, 13040, 17746, 15048, 21413, 33250, 13854, 25427, 14720,
          26114, 17812, 3326, 10890, 1844, 7986, 17789, NA, 11156, 30266,
          21783, 50018, 27768, 23354, 21349, 24795, 32189, 13816, 8601, 15390,
          11794, 8270, 9556, 6738, 8853, 10956, 6558, 15901, NA, 30816,
          27454, 13967, 5937, 34544, 34297, NA, 25016, 36893, 24880, 19600,
          12283, 8835, 4853, 3453, 30573, 9445, 22330, NA, 15538),
        amplada_cavea = c(
          22.0, 20.45, 24.4, 27.55, 22.0, 32.0, 41.75, 23.0, 31.85, 24.5,
          35.0, 28.0, 7.55, 22.15, 6.0, 16.85, 30.0, NA, 18.75, 38.35,
          32.3, 54.2, 37.1, 33.45, 32.15, 34.5, 40.0, 24.0, 28.3, 23.3, 
          21.0, 18.0, 18.0, 13.0, 16.0, 19.0, 14.0, 25.0, NA, 41.45,
          37.75, 21.85, 12.0, 45.65, 42.5, NA, 42.6, 44.45, 31.35, 30.25,
          23.0, 15.0, 12.0, 6.5, 41.7, 17.1, 31.25, NA, 24.5))


  ### Extracció i càlculs de la taula d'amfiteatres romans
  ### Identificació de taula 31 (pàgines 287)
    tableau_31_pp_287 <- tibble::tibble(
        index_id = as.factor(c(
          paste0('#', sprintf('%0.3d', 274:275)),
          '#277', 
          paste0('#', sprintf('%0.3d', 280:283)), 
          '#289', '#294',
          paste0('#', sprintf('%0.3d', 297:298)),
          '#301', '#303', '#309')),
        nom = c(
          'SINUESSA', 'VELITRAE', 'CIRCEI', 'ASISIUM', 'HISPELLUM',
          'MEVANIA', 'TUDER', 'PELTUINUM', 'CANUSIUM', 'EGNATIA',
          'PUPPUT', 'ANTIPOLIS', 'TINTIGNAC', 'CAESAREA MARITIMA'),
        provincia_romana = c(
          rep('italia_i_campania_et_latium', times = 3),
          rep('italia_vi_umbria_et_ager_gallicus', times = 4),
          rep('italia_iiii_samnium', times = 1),
          rep('italia_ii_apulia_et_calabria', times = 2),
          rep('africa_proconsularis', times = 1),
          rep('gallia_narbonensis', times = 1),
          rep('gallia_aquitania', times = 1),
          rep('iudaea_palestina', times = 1)),
        pais = c(
          rep('italy', times = 10),
          rep('tunisia', times = 1),
          rep('france', times = 2),
          rep('israel', times = 1)),
        amplada_arena = c(
          NA, NA, NA, NA, NA, 44.0, NA, NA, NA, 37.0,
          45.0, NA, NA, NA),
        alcada_arena = c(
          NA, NA, NA, NA, NA, 24.0, NA, NA, NA, 27.0,
          36.0, NA, NA, NA),
        amplada_general = c(
          80.0, 125.0, 90.0, 60.0, 59.0, 80.0, 90.0, 78.0, 138.0, NA,
          NA, 70.0, 66.0, 95.0),
        alcada_general = c(
          45.0, 117.0, 68.0, 35.0, 35.0, 53.0, 60.0, 60.0, 108.0, NA, 
          NA, 50.0, 50.0, 62.0),
        nombre_places = c(
          2827, 11486, 4807, 1649, 1622, 3330, 4241, 3676, 11706, NA,
          NA, 2749, 2592, 4626),
        amplada_cavea = c(
          NA, NA, NA, NA, NA, 18.0, NA, NA, NA, NA, 
          NA, NA, NA, NA))

  ### Calculs comuns a la totalitat d'amfiteatres
    l_tableau <- list(
      tableau_01_pp_43, 
      tableau_28_pp_284_285,
      tableau_29_pp_285_286,
      tableau_30_pp_286_287,
      tableau_31_pp_287)

    names(l_tableau) <- c(
      'tableau 01 amphitheatres republicaine',
      'tableau 28 amphitheatres a cavea supporte per des remblais continus',
      'tableau 29 amphitheatres a cavea supporte per des remblais compartimentes',
      'tableau 30 amphitheatres a structure creuse',
      'tableau 31 amphitheatres mal connus')

### Definir aquelles columnes númeriques
  cols_num <- c(
    'amplada_arena', 'alcada_arena', 'amplada_general', 'alcada_general',
    'nombre_places', 'amplada_cavea',  
    'arena_max', 'arena_min', 'overall_max', 'overall_min',
    'seat_est', 'cavea_wide', 'cavea_height', 
    'arena_m2', 'overall_m2', 'cavea_m2',
    'ratio_arena', 'ratio_general', 'ratio_cavea',
    'superficie_arena', 'superfie_general',
    'nombre_places', 'elevation_m',
    'perimetre_arena', 'perimetre_general', 'valor')

  cols_chr <- c(
    'place', 'phase', 'nom', 'hackett_class', 
    'index_id', 'vasa_class', 't_building', 'dinasty_gr', 
    'pais', 'lat', 'long', 'provincia_romana', 'variable', 'bib')

    
  for(i in seq_along(l_tableau)) {
    
    l_tableau[[i]] <- l_tableau[[i]] %>%
      dplyr::mutate(
        ratio_arena = amplada_arena / alcada_arena,
        ratio_general = amplada_general / alcada_general,
        superficie_arena = amplada_arena / 2 * alcada_arena / 2 * pi,
        superficie_general = amplada_general / 2 * alcada_general / 2 * pi,
        superficie_cavea = superficie_general - superficie_arena,
        perimetre_arena = pi * (amplada_arena / 2 + alcada_arena / 2),
        perimetre_general = pi * (amplada_general / 2 + alcada_general / 2),
        ratio_cavea = superficie_arena / superficie_general,
        bib = '1988_golvin') %>%
      dplyr::mutate(
        across(any_of(cols_num), as.double)) %>%  
      dplyr::mutate(
        across(any_of(cols_chr), as.character)) 

    ### Argument 'filtrar_provincia'
      if(!is.null(filtrar_provincia) & is.null(filtrar_pais)) {

        l_tableau[[i]] <- l_tableau[[i]] %>%
          dplyr::filter(
            stringr::str_detect(provincia_romana, paste(filtrar_provincia, collapse = '|'))) %>%
            droplevels()
  
      }

    ### Argument 'filtrar_pais'
      if(!is.null(filtrar_pais) & is.null(filtrar_provincia)) {

        l_tableau[[i]] <- l_tableau[[i]] %>%
          dplyr::filter(
            stringr::str_detect(pais, paste(pais, collapse = '|'))) %>%
            droplevels()
  
      }

      ### Argument 'seleccionar_columnes'
        if(!rlang::quo_is_null(seleccionar_columnes)) {

          l_tableau[[i]] <- l_tableau[[i]] %>%
            dplyr::select('index_id', 'nom', 'provincia_romana', 'pais', !!seleccionar_columnes)

        }
    }

### Transforma format_ample en format_llarg
   if(isTRUE(format_llarg) & !rlang::quo_is_null(seleccionar_columnes)) {
    
    # Pivota només les columnes de la selecció de l'usuari que siguin numèriques,
    # evitant errors en intentar combinar diferents tipus de dades.
    l_tableau <- purrr::map(l_tableau, ~ .x %>%
        tidyr::pivot_longer(
          cols = c(!!seleccionar_columnes) & where(is.numeric),
          names_to = 'variable',
          values_to = 'valor',
          values_drop_na = FALSE))
    
    } else if (isTRUE(format_llarg) & rlang::quo_is_null(seleccionar_columnes)) {
      
    # Double check 02
      stopifnot(all(sapply(l_tableau, ncol) == 19))

      l_tableau <- purrr::map(l_tableau, ~ .x %>%
        tidyr::pivot_longer(
          cols = where(is.numeric),
          names_to = 'variable',
          values_to = 'valor',
          values_drop_na = FALSE))

    # Double check 03
      stopifnot(all(sapply(l_tableau, ncol) == 7))

    } else {

      l_tableau
    
    }

### Fusió de les dues taules per la columna 'nom' i 'original_id'
  taula_fusionada <- dplyr::bind_rows(l_tableau) %>%
    dplyr::arrange('index_id', 'nom',  'provincia_romana', 'pais')

### Guardar la taula fusionada com a arxiu CSV
  # Esborrar arxius CSV existents amb el mateix patró
  if(isFALSE(format_llarg) & rlang::quo_is_null(seleccionar_columnes) & is.null(filtrar_provincia) & is.null(filtrar_pais)) {
   
    arxiu_a_esborrar <- list.files(
      path = file.path('data', '01_data_golvin'),
      pattern = '^01_.*_dimensions_amphitheatres_golvin\\.csv$',
      full.names = TRUE)

    if (length(arxiu_a_esborrar) > 0) { file.remove(arxiu_a_esborrar) }

    # Crear el nom de l'arxiu nou i guardar-lo
    data_actual <- format(Sys.Date(), '%y.%m.%d')
    nom_arxiu <- paste0('01_', data_actual, '_dimensions_amphitheatres_golvin.csv')
    cami_arxiu <- file.path('data', '01_data_golvin', nom_arxiu)

    utils::write.csv2(taula_fusionada, file = cami_arxiu, na = 'NA', row.names = FALSE)
  
  }


  if (isTRUE(retornar_originals)) {

  # missatge
    cat(
      crayon::bold(crayon::blue('i')),
      crayon::black(' El llistat de dades ha estat carregat correctament\n'))

    return(l_tableau)

  } else {

    # missatge
    cat(
      crayon::bold(crayon::blue('i')),
      crayon::black(' Les dades ha estat carregat correctament\n'))

    return(taula_fusionada)
    
  }
}
#' @title Llista arxius de diferents carpetes de dades.
#' @description Llista arxius de diferents carpetes de dades donat un 'home_folder'.
#' @param home_folder Objecte R amb la ruta a les carpetes de dades.
#' @param pattern Carrega un format d'extensió específic entre 'xlsx', 'xls' i 'csv'.
#' @param recursive Lògic (TRUE per defecte). Llista els arxius de forma recursiva.
#'
#' @return Una llista d'arxius estructurada per 'carpetes' anomenades.
#' 
#' @rdname amphi_list_files
#' @export
amphi_list_files <- function(
    home_folder,
    recursive = TRUE,
    pattern = c('xlsx', 'xls', 'csv'))
{
### 1st double check
	if (!dir.exists(home_folder)) {
    
		stop("Error: 'home_folder' no es un directori vàlid o no existeix. Si us plau, verifica la ruta.")
  
	}

### 2nd double check
	valid_patterns <- c('xlsx', 'xls', 'csv')

	if (!any(pattern %in% valid_patterns)) {
    
		warning("Advertència: el 'pattern' especificat no és l'extensió dels arxius. Si us plau, assegura que siguin correctes.")
  
	}

### Llista directoris
  l_dir <- list.dirs(
  	path = home_folder,
  	recursive = recursive,
		full.names = TRUE)

### 3rd double check
	if (length(l_dir) == 0) {

    message("No s'han trobat subdirectoris: ", home_folder)
  
	  return(list())
  
	}

### Crea un patró de cerca regex
  l_pattern <- paste0('\\.(', paste(pattern, collapse = '|'), ')$', sep = '')

### Llista els arxius
	l_files <- list()

	for(i in seq_along(l_dir)) {

  	file <- list.files(
  		path = l_dir[i],
  		pattern = l_pattern,
  		full.names = TRUE,
  		recursive = TRUE)

  	### save in a list
  		l_files[[length(l_files) + 1]] = file

	}

### Nom de la llista
	names(l_files) <- basename(l_dir)

### Rerornar la llista
	return(l_files)
}


#' @title Llegeix dades en diversos formats
#' @description Aquesta funció llegeix una llista de fitxers, suportant formats Excel (.xls, .xlsx) i .csv.
#' Detecta automàticament el tipus de fitxer per la seva extensió.
#' @param l_files Una llista d'arxius obtinguda de 'amphi_list_files'.
#' @param type_file El tipus d'arxiu a llegir ('excel' o 'csv').
#' @param sep El separador de camps per a fitxers CSV. Per defecte, NULL (auto-detecció).
#' @param dec El separador decimal per a fitxers CSV. Per defecte, NULL (auto-detecció).
#' @param skip_rows Nombre de files a ometre en la lectura (només per a Excel).
#' @param na_strings Vector de cadenes de text per a identificar com a NA (només per a CSV).
#' @param clean_names Lògic. Si és TRUE, neteja els noms de les columnes amb `janitor::clean_names`.
#' 
#' @return Una llista de data.frames.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom data.table fread
#' @importFrom janitor clean_names
#' @rdname amphi_read_data
#' @export
amphi_read_data <- function(
  l_files,
  type_file = c('excel', 'csv'),
  sep = NULL,
  dec = NULL,
  skip_rows = 0,
  na_strings = NULL,
  clean_names = TRUE) {

### 1st double check
  if (!is.character(l_files) || length(l_files) == 0) {
    
    message("No s'han trobat llistat d'arxius: ", l_files)
  
  }

### list
  l_df <- list()


### loop #01
	for(i in seq_along(l_files)) {

	### argument 'type_file'
		if(type_file == 'excel') {
			
			### Obtenint info sobre totes les fulles i arxius 'excel'
  			l_sheet <- readxl::excel_sheets(l_files[[i]])

				df <- lapply(l_sheet,
					function(x) readxl::read_excel(l_files[[i]], sheet = x, skip = skip))

  		### Assigna noms a les matrius
  			names(df) <- l_sheet

		} else if(type_file == 'csv') {
    
    ### Argumnent per als NA
      if (is.null(na_strings)) {
    		csv_na_strings <- c('', 'NA')
      } else {
        csv_na_strings <- na_strings
      }

		### getting info about all 'csv' files

		  df <- lapply(l_files[[i]],
  		  data.table::fread,
					sep = sep,
					dec = dec,
        skip = skip_rows,
					na.strings = csv_na_strings)

  	}

	### save list
		l_df[[length(l_df) + 1]] = df

	}

### unlist()
	 l_df <- unlist(l_df, recursive = FALSE)


### apply 'clean_names'
  if (clean_names == TRUE && length(l_df) > 0) {
	  
    for(i in seq_along(l_df)){

		  l_df[[i]] <- l_df[[i]] %>%
			  janitor::clean_names(case = 'snake')

	  }
  }

### return
	return(l_df)
}

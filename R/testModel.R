#' Train the model from CSV data
#'
#' @param INPUT_TESTING_CSV_PATH URL or file path of csv with test data
#' @param MODEL Model object obtained from trainModel()
#' @param OUTPUT_FITTED_CSV_PATH File path ending with .CSV extension where fitted values will be output
#'
#'
#' @export
testModel = function(INPUT_TESTING_CSV_PATH, MODEL, OUTPUT_FITTED_CSV_PATH) {

  ## Import data
  rawDf = readr::read_csv(INPUT_TESTING_CSV_PATH)
  model = MODEL

  ### Initial Data Cleaning
  df =
    rawDf %>%
    dplyr::rename(
      .,
      patientId = PatientID, acct = 'Patient Acct #',
      firstname = 'First Name', mi = MI, lastname = 'Last Name',
      dob = 'Date of Birth', sex = Sex,
      street1 = 'Current Street 1', street2 = 'Current Street 2',
      city = 'Current City', state = 'Current State', zip = 'Current Zip Code',
      oldFirstname = 'Previous First Name', oldMi = 'Previous MI', oldLastname = 'Previous Last Name',
      oldStreet1 = 'Previous Street 1', oldStreet2 = 'Previous Street 2',
      oldCity = 'Previous City', oldState = 'Previous State', oldZip = 'Previous Zip Code'
    ) %>%
    # Add 3 columns: "monthDob", "dayDob", "yearDob" by
    # (1) Taking the "dob" column,
    # (2) Removing all non-numeric characters,
    # (3) Splitting the remaining string by "/" then only keeping the first 3 strings
    # (4) Taking the first 2 characters of string 1 and string 2 then left-padding it with 0
    # (5) Taking the first 4 characters of string 3 then right-padding it with 0
    dplyr::bind_cols(
      .,
      .$dob %>%
        stringr::str_replace_all(., '[^0-9/]+', '') %>%
        stringr::str_split(., '/') %>%
        purrr::map_dfr(., function(x)
          tibble(
            monthDob = stringr::str_pad(x[1], 2, side = 'left', pad = '0'),
            dayDob = stringr::str_pad(x[2], 2, side = 'left', pad = '0'),
            yearDob = stringr::str_pad(str_sub(x[3], 1, 4), 4, side = 'right', pad = '0')
          )
        )
    ) %>%
    dplyr::mutate(
      .,
      state = sapply(.$state, function(x) {
        x = str_to_title(x)
        if (str_to_upper(x) %in% state.abb) {
          return(str_to_upper(x))
        } else if (x %in% state.name) {
          return(state.abb[which(state.name == x)])
        } else if (hunspell_suggest(x)[[1]] %>% .[. %in% state.name] %>% {length(.) >= 1}) {
          return(hunspell_suggest(x)[[1]] %>% .[. %in% state.name] %>% first(.) %>% {state.abb[which(state.name == .)]})
        } else {
          return(NA)
        }
      }),
      oldState = sapply(.$oldState, function(x) {
        x = str_to_title(x)
        if (str_to_upper(x) %in% state.abb) {
          return(str_to_upper(x))
        } else if (x %in% state.name) {
          return(state.abb[which(state.name == x)])
        } else if (hunspell_suggest(x)[[1]] %>% .[. %in% state.name] %>% {length(.) >= 1}) {
          return(hunspell_suggest(x)[[1]] %>% .[. %in% state.name] %>% first(.) %>% {state.abb[which(state.name == .)]})
        } else {
          return(NA)
        }
      })
    ) %>%
    dplyr::mutate(
      .,
      firstname = str_to_lower(firstname),
      mi = str_to_lower(mi),
      lastname = str_to_lower(lastname),
      sex = str_to_lower(sex),
      street1 = str_to_lower(street1),
      city = str_to_lower(city),
      state = str_to_lower(state),
      oldFirstname = str_to_lower(oldFirstname),
      oldMi = str_to_lower(oldMi),
      oldLastname = str_to_lower(oldLastname),
      oldStreet1 = str_to_lower(oldStreet1),
      oldCity = str_to_lower(oldCity),
      oldState = str_to_lower(oldState)
    ) %>%
    dplyr::mutate(., mi = str_sub(mi, 1, 1), oldMi = str_sub(oldMi, 1, 1)) %>%
    dplyr::mutate(., street2 = str_replace_all(street2, '[^0-9]+', ''), oldStreet2 = str_replace_all(oldStreet2, '[^0-9]+', '')) %>%
    dplyr::mutate(., sex = str_to_upper(str_sub(sex, 1, 1))) %>%
    dplyr::mutate(., sex = ifelse(is.na(sex), NA, ifelse(sex == 'F', 1, 0)))

  ### Make comparison
  compDf =
    lapply(head(df$patientId, -1), function(x1)
      lapply((x1 + 1):max(df$patientId), function(x2) tibble(patientId1 = x1, patientId2 = x2))
    ) %>%
    unlist(., recursive = FALSE) %>% dplyr::bind_rows(.) %>%
    dplyr::filter(., patientId1 != patientId2) %>%
    dplyr::left_join(., df %>% setNames(., paste0(colnames(.), '_1')), by = c('patientId1' = 'patientId_1')) %>%
    dplyr::left_join(., df %>% setNames(., paste0(colnames(.), '_2')), by = c('patientId2' = 'patientId_2'))

  digitsum = function(vector) sapply(vector, function(x) sum(floor(as.numeric(x)/ 10^(0:(nchar(as.numeric(x)) - 1))) %% 10))

  ### Penalty
  penaltyDf =
    compDf %>%
    dplyr::transmute(
      .,
      monthDob = pmin(abs(as.numeric(monthDob_1) - as.numeric(monthDob_2)), abs(digitsum(monthDob_1) - digitsum(monthDob_2))),
      dayDob = pmin(abs(as.numeric(dayDob_1) - as.numeric(dayDob_2)), abs(digitsum(dayDob_1) - digitsum(dayDob_2))),
      yearDob = pmin(abs(as.numeric(yearDob_1) - as.numeric(yearDob_2)), abs(digitsum(yearDob_1) - digitsum(yearDob_2))),
      sumDob = (monthDob + dayDob + yearDob)^2,
      sex = sex_1 == sex_2,
      firstname = pmin(stringdist(firstname_1, firstname_2, method = 'lv'), stringdist(firstname_1, oldFirstname_2, method = 'lv'), stringdist(oldFirstname_1, oldFirstname_2, method = 'lv'), stringdist(oldFirstname_1, firstname_2, method = 'lv'), na.rm = TRUE),
      # firstname_soundex = pmin(stringdist(firstname_1, firstname_2, method = 'soundex'), stringdist(firstname_1, oldFirstname_2, method = 'soundex'), stringdist(oldFirstname_1, oldFirstname_2, method = 'soundex'), stringdist(oldFirstname_1, firstname_2, method = 'soundex'), na.rm = TRUE),
      mi = pmin(stringdist(mi_1, mi_2, method = 'lv'), stringdist(mi_1, oldMi_2, method = 'lv'), stringdist(oldMi_1, oldMi_2, method = 'lv'), stringdist(oldMi_1, mi_2, method = 'lv'), na.rm = TRUE),
      lastname = pmin(stringdist(lastname_1, lastname_2, method = 'lv'), stringdist(lastname_1, oldLastname_2, method = 'lv'), stringdist(oldLastname_1, oldLastname_2, method = 'lv'), stringdist(oldLastname_1, lastname_2, method = 'lv'), na.rm = TRUE),
      # lastname_soundex = pmin(stringdist(lastname_1, lastname_2, method = 'soundex'), stringdist(lastname_1, oldLastname_2, method = 'soundex'), stringdist(oldLastname_1, oldLastname_2, method = 'soundex'), stringdist(oldLastname_1, lastname_2, method = 'soundex'), na.rm = TRUE),
      street1 = pmin(stringdist(street1_1, street1_2, method = 'lv'), stringdist(street1_1, oldStreet1_2, method = 'lv'), stringdist(oldStreet1_1, oldStreet1_2, method = 'lv'), stringdist(oldStreet1_1, street1_2, method = 'lv'), na.rm = TRUE),
      street2 = pmin(stringdist(street2_1, street2_2, method = 'lv'), stringdist(street2_1, oldStreet2_2, method = 'lv'), stringdist(oldStreet2_1, oldStreet2_2, method = 'lv'), stringdist(oldStreet2_1, street2_2, method = 'lv'), na.rm = TRUE),
      city = pmin(stringdist(city_1, city_2, method = 'lv'), stringdist(city_1, oldCity_2, method = 'lv'), stringdist(oldCity_1, oldCity_2, method = 'lv'), stringdist(oldCity_1, city_2, method = 'lv'), na.rm = TRUE),
      state = pmin(stringdist(state_1, state_2, method = 'lv'), stringdist(state_1, oldState_2, method = 'lv'), stringdist(oldState_1, oldState_2, method = 'lv'), stringdist(oldState_1, state_2, method = 'lv'), na.rm = TRUE),
      zip = pmin(stringdist(zip_1, zip_2, method = 'lv'), stringdist(zip_1, oldZip_2, method = 'lv'), stringdist(oldZip_1, oldZip_2, method = 'lv'), stringdist(oldZip_1, zip_2, method = 'lv'), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      .,
      monthDob_NA = as.numeric(is.na(monthDob)),
      dayDob_NA = as.numeric(is.na(dayDob)),
      yearDob_NA = as.numeric(is.na(yearDob)),
      sumDob_NA = as.numeric(is.na(sumDob)),
      sex_NA = as.numeric(is.na(sex)),
      # firstname_NA = as.numeric(is.na(firstname)),
      # mi_NA = as.numeric(is.na(mi)),
      # lastname_NA = as.numeric(is.na(lastname)),
      street1_NA = as.numeric(is.na(street1)),
      street2_NA = as.numeric(is.na(street2)),
      city_NA = as.numeric(is.na(city)),
      state_NA = as.numeric(is.na(state)),
      zip_NA = as.numeric(is.na(zip))
    ) %>%
    replace(is.na(.), 0)

  xMat =
    penaltyDf %>%
    dplyr::select(
      .,
      monthDob, dayDob, yearDob, sumDob, sex, firstname, mi, lastname, street1, street2, city, state, zip,
      monthDob_NA, dayDob_NA, yearDob_NA, sumDob_NA, sex_NA, street1_NA, street2_NA, city_NA, state_NA, zip_NA) %>%
    as.matrix(.)

  fit = model %>% predict(., xMat, type = 'response')
  fitDf =
    tibble(
      patientId1 = compDf$patientId1,
      patientId2 = compDf$patientId2,
      fitted = fit[, 1]
    ) %>%
    dplyr::mutate(., pred = ifelse(fitted > .5, 1, 0))



  startGroupIndex = 1
  groupList = list()

  # Loop
  # Puts all into same group
  for (.patientId in df$patientId) {
    if (.patientId %in% unlist(groupList)) next
    groupList[[startGroupIndex]] = dplyr::filter(fitDf, patientId1 == .patientId, pred == 1) %>% .$patientId2 %>% c(.patientId, .)
    startGroupIndex = startGroupIndex + 1
    # Then skip to next item
  }

  finalDf =
    groupList %>%
    sapply(., function(x) paste0(x, collapse = ',')) %>% tibble(patientIds = .) %>%
    dplyr::bind_cols(groupId = 1:nrow(.), .)

  write.csv(finalDf, OUTPUT_FITTED_CSV_PATH, row.names = FALSE)
  return(finalDf)
}

### Functions

TEMP_DIRECTORY <- paste('C:',
                        '/', 'files',
                        '/', 'Sync',
                        '/', 'COSE_RIMESSE_TUTTE_AL_LORO_POSTO',
                        '/', 'RICERCA',
                        '/', 'r_functions', sep = '')

UPDATE <- FALSE
source(paste(TEMP_DIRECTORY, '/',
             'my_functions_install_libraries', '.R', sep = ''))
rm(UPDATE)

source(paste(TEMP_DIRECTORY, '/',
             'my_functions_general', '.R', sep = ''))
## my_function_general
# give_nice_single_value(IN, text = '', decimals = 3, with_equal_sign = FALSE, plus_minus = FALSE, min = NA, max = NA)
# give_nice(IN, text = '', decimals = 3, with_equal_sign = FALSE, plus_minus = FALSE, min = NA, max = NA)
# give_nice_p(IN, text = 'p', decimals = 3, with_equal_sign = FALSE, significance = 0.05)
# my_gg95ci(x)
# save_table(what, file_name = 'DF', where = getwd(), row_names = FALSE, fileEncoding = 'latin1')

source(paste(TEMP_DIRECTORY, '/',
             'my_functions_collect_tests_correction', '.R', sep = ''))
## my_functions_collect_tests_correction
# test_give_rawitem_df(CODE = NA, RAW = NA, item_names = NA, abbreviation = '', n_items = 0, save_data = FALSE, data_folder = getwd())
## 'WHOQOL-100'
# whoqol100_give_scoreditem_df(CODE = NA, RAW = NA, save_data = FALSE, data_folder = getwd())
# whoqol100_give_results_from_item(CODE = NA, ITEM = NA, are_raw_data = TRUE, save_data = FALSE, data_folder = getwd())
## 'WHOQOL-Brief'
# whoqolbrief_give_scoreditem_df(CODE = NA, RAW = NA, save_data = FALSE, data_folder = getwd())
# whoqolbrief_give_results_from_item(CODE = NA, ITEM = NA, are_raw_data = TRUE, save_data = FALSE, data_folder = getwd())

#

rm(TEMP_DIRECTORY)

#

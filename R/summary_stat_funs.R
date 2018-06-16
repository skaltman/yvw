#' Calculate the mean or standard deviation of a variable in a tibble
#'
#' @param data A tibble (formatted data from exp 1, 2, or 3)
#' @param summary_fun The summary function you want to apply
#' @param summary_var The variable you want to summarise
#' @param ... Any criteria you want to filter by
#' @param num_digits The number of significant digits you want to round the
#' result of summary_fun() to. Default is 2
#' @examples
#' get_summ_stat(three_toy_tidy, mean, age, condition == "Broken Button", num_digits = 3)
get_summ_stat <- function(data, summary_fun, summary_var, ..., num_digits = 2){
  summary_var <- enquo(summary_var)

  data %>%
    filter(...) %>%
    summarise(summary = round(summary_fun(!!summary_var, na.rm = TRUE),
                              num_digits)) %>%
    pull(summary)
}

#' Calculate the percentage of female participants
#'
#' @param data A tibble (formatted data from exp 1, 2, or 3)
#' @param num_digits The number of significant digits
#' @param ... Any criteria you want to filter by
#' @examples
#' get_percentage_female(three_toy_tidy, 3, condition == "Broken Button")
get_percentage_female <- function(data, ..., num_digits = 0){
  num_participants <- data %>% nrow()

  data %>%
    filter(gender == "F", ...) %>%
    summarise(percentage = round(n() / num_participants * 100, 0)) %>%
    pull(percentage)
}

#' Create a tibble various summary statistics (mean and sd ages; percentage female;
#' number of participants; number per condition; number per location)
#'
#' @param data A tibble (formatted data from exp 1, 2, or 3)
summary_tibble <- function(data) {
  tibble(
    num_participants = data %>% nrow(),
    num_bb = data %>% filter(condition == "Broken Button") %>% nrow(),
    num_bt = data %>% filter(condition == "Broken Toy") %>% nrow(),
    perc_female = get_percentage_female(data),
    mean_age = data %>% get_summ_stat(mean, age),
    sd_age = data %>% get_summ_stat(sd, age),
    mean_age_bt = data %>% get_summ_stat(mean, age, condition == "Broken Toy"),
    mean_age_bb = data %>% get_summ_stat(mean, age, condition == "Broken Button"),
    sd_age_bt = data %>% get_summ_stat(sd, age, condition == "Broken Toy"),
    sd_age_bb = data %>% get_summ_stat(sd, age, condition == "Broken Button"),
    num_bing = data %>% filter(str_detect(location, "Bing")) %>% nrow(),
    num_jmz = data %>% filter(location == "JMZ") %>% nrow(),
    mean_age_bing = data %>% get_summ_stat(mean, age, str_detect(location, "Bing")),
    mean_age_jmz = data %>% get_summ_stat(mean, age, location == "JMZ"),
    sd_age_bing = data %>% get_summ_stat(sd, age, str_detect(location, "Bing")),
    sd_age_jmz = data %>% get_summ_stat(sd, age, location == "JMZ")
  )
}

#' Create a tibble with counts of excluded kids for different reasons
#'
#' @param data Raw data from exp 1, 2, or 3
#' @param exclude_code_var Unquoted variable name of the variable specifying the
#' reason for exclusion
#' @param ... Unquoted names of the exclude codes that you want in the resulting
#' tibble. If you want to include all of them, set this equal to \code{everything()}
#' @return A tibble whose columns are the specified exclusion codes and values are
#' the number of children excluded under that code. The \code{total} column sums
#' over all specified exclusion codes (not the total number of excluded kids for
#' any reason)
#' @examples
#' get_num_excluded(three_toy, excludeCode, parent, sibling, exp)
get_num_excluded <- function(data, exclude_code_var, ...){
  exclude_code <- enquo(exclude_code_var)

  data %>%
    filter(`exclude?` == "yes") %>%
    count(!!exclude_code) %>%
    spread(!!exclude_code, nn) %>%
    select(...) %>%
    mutate(total = as.integer(rowSums(.))) %>%
    rename_all(funs(str_replace_all(., " ", "_")))
}

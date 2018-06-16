#' Create a tibble with the number and percentage of children in each condition
#' with variable = variable_option
#'
#' @param data Formatted data from exp 1, 2, or 3
#' @param variable Unquoted name of the variable you want to look at
#' @param variable_option String representing the value of the variable for which
#' you want the number/percentage of children
#' @return A tibble with the number of children with "variable_option" for variable
#' per condition (num_bb and num_bt) and the percentage of children with "variable_option"
#' for variable per condition (bb_"variable_option" and bt_"variable_option")
format_for_between_stats <- function(data, variable, variable_option) {
  variable <- enquo(variable)

  data %>%
    mutate(condition = fct_recode(as.factor(condition),
                                  bt = "Broken Toy",
                                  bb = "Broken Button")) %>% #this makes it easier to access variables later -- no longer have a space
    group_by(condition, !!variable) %>%
    summarise(n = n()) %>%
    mutate(percentage = n/sum(n) * 100) %>%
    ungroup() %>%
    filter(!!variable == variable_option) %>%
    unite("stat", condition, !!variable) %>%
    mutate(num_bb = .$n[[2]],
           num_bt = .$n[[1]]) %>%
    select(-n) %>%
    spread(stat, percentage) %>%
    mutate_all(~ round(., 0))
}

#'
get_fishers_p_value <- function(table, digits = 4) {
  fisher.test(table)[[1]] %>% round(digits)
}

# Returns a tibble with 5 variables -- num_bb, num_bt, perc_bb, perc_bt, and p_value
# num_bb and num_bt are the number of children with variable == variable_option in each condition (e.g., firstChoice == "Confederate's toy")
# perc_bb and perc_bt are num_bb and num_bt divided by the number of children in each condition
# p_value is the p-value from a fisher's exact test that tests for differences in `variable` by condition
# This will work for first reponse, helpfulness, correctness, etc.
between_condition_stats <- function(data, variable, variable_option, digits = 4) {
  variable <- enquo(variable)

  data %>%
    format_for_between_stats(variable = !!variable,
                             variable_option = variable_option) %>%
    rename_at(vars(contains(variable_option)),
              funs(str_c("perc_", str_extract(., "[a-z][a-z]")))) %>%
    mutate(p_value =
             get_fishers_p_value(
               table(data$condition, data %>% pull(!!variable)),
               digits = digits))
}

between_condition_helpful <- function(data, digits = 4) {
  between_condition_stats(data, helpfulCategory, "Helpful", digits = digits)
}

between_condition_response <- function(data, digits = 4) {
  between_condition_stats(data, firstChoice, "Other toy", digits = digits)
}

between_condition_correct <- function(data, digits = 4) {
  between_condition_stats(data, firstChoiceCorrect, "1", digits = digits)
}

between_condition_flip <- function(data, digits = 4) {
  between_condition_stats(data, flip, "TRUE", digits = digits)
}

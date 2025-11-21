#------------------------------------------------
# generate.Clean.Data: function to generate the clean dataset
#   based on pre-specified user-input variables (covariates, exposure, outcome) 
#
# input: dict of pre-specified variables (must be the correct variable names!)
#   first level by screener, baseline, or visit
#   second level by covariate, exposure, or outcome;
#       baseline covariates must include STATUS0
#       baseline exposure must include HORMPIL0
# output: clean_df which contains all of the covariates, exposure nodes,
#   censoring nodes, outcome nodes in the correct time-order
#------------------------------------------------

generate.Clean.Data <- function(variable_dict) {
  ##### Load Data ###############################
  all_icpsr <- list.dirs(here("data"), full.names=FALSE, recursive=FALSE)
  
  for (icpsr in all_icpsr) {
    rda <- here("data", icpsr, "DS0001", sprintf("%s-0001-Data.rda", substr(icpsr, 7, 12)))
    load(rda) # loads each icpsr dataframe into local environment!
  }
  
  ##### Define Variables ########################
  # Exposure 
  baseline_A <- variable_dict[["baseline"]]$exposure
  A_prefix <- variable_dict[["visit"]]$exposure
  
  # Outcome
  Y_prefix <- variable_dict[["visit"]]$outcome
  
  # Covariates
  baseline_W0_inscreener <- variable_dict[["screener"]]$covariate
  baseline_W0_inbaseline <- stack(variable_dict[["baseline"]]$covariate)$values
  visit_W <- variable_dict[["visit"]]$covariate
  
  ##### Construct Cohort from Baseline ##########
  # We start with N=3302 participants at baseline.
  
  # Add screener variables to baseline dataframe
  screener_df <- da04368.0001[da04368.0001$SWANID %in% da28762.0001$SWANID, 
                              c("SWANID", baseline_W0_inscreener)]
  
  # Obtain participants that are 
  # 1) pre-menopausal or early peri-menopausal, and 
  # 2) have not taken hormone medications in the last month. (N=3243)
  baseline_df <- da28762.0001 %>%
    filter(as.integer(STATUS0) %in% c(4, 5)) %>%
    filter(as.integer(HORMPIL0) == 1)
  
  # Remove women with 
  # 1) unknown age at menarche, or 
  # 2) age at menarche > 18 years. (N=3217)
  baseline_df <- baseline_df %>%
    filter(!is.na(as.integer(STARTAG0) <= 18))
  
  # Remove women with 
  # 1) NA hormone usage in visit 1 (cannot assess use of hormones between baseline and visit 1), and 
  # 2) missing baseline covariates. (N = 2689)
  visit1_A <- sapply(A_prefix, function(x) paste0(x, 1))
  visit1_hormone_df <- da29221.0001 %>% 
    filter_at(vars(visit1_A), all_vars(!is.na(.)))
  
  baseline_df <- baseline_df[baseline_df$SWANID %in% visit1_hormone_df$SWANID, ]
  
  baseline_df <- baseline_df %>%
    filter_at(vars(baseline_W0_inbaseline), all_vars(!is.na(.)))
  
  # Remove women with 
  # 1) NA SPBMDT0 and HPBMDT0 because we assume they were null as two sites did not measure BMD
  baseline_df <- baseline_df %>%
    filter_at(vars(SPBMDT0, HPBMDT0), all_vars(!is.na(.)))
  
  ##### Feature Engineering #####################
  
  # Create depression and anxiety variables
  baseline_depress0 <- stack(sapply(depress_prefix, function(x) paste0(x, 0)))$values
  baseline_anxiety0 <- stack(sapply(anxiety_prefix, function(x) paste0(x, 0)))$values
  
  # Convert factor variables to numerics - 1
  baseline_Y0 <- stack(sapply(Y_prefix, function(x) paste0(x, 0)))$values
  baseline_factor_minus1 <- c(baseline_W0_inbaseline[-c(1:5)], "HORMUSER0")
  baseline_df <- baseline_df %>%
    rename("HORMUSER0" = "HORMPIL0") %>%
    select(all_of(c("SWANID", baseline_W0_inbaseline, "HORMUSER0", baseline_Y0))) %>%
    mutate(across(all_of(baseline_factor_minus1), as.numeric)) %>%
    mutate(across(all_of(baseline_factor_minus1), ~ . -1)) %>%
    rowwise() %>%
    mutate("DEPRESSION0" = sum(c_across(all_of(baseline_depress0))),
           "ANXIETY0" = sum(c_across(all_of(baseline_anxiety0)))) %>%
    ungroup()
  
  # Remove depression and anxiety questions
  baseline_df <- baseline_df %>%
    select(-all_of(c(baseline_depress0, baseline_anxiety0)))
  
  # Reorder columns for LTMLE (L's, A's, Y's)
  baseline_W0 <- names(select(baseline_df, -all_of(c("SWANID", "HORMUSER0", baseline_Y0))))
  baseline_df <- baseline_df %>%
    select(all_of(c("SWANID", baseline_W0, "HORMUSER0", baseline_Y0)))
  
  # Get screener variables for individuals in baseline_df
  screener_df <- screener_df %>%
    filter_at(vars(baseline_W0_inscreener), all_vars(!is.na(.))) %>%
    select(c(SWANID, baseline_W0_inscreener)) 
  # mutate(across(where(is.factor), as.numeric))
  
  # Merge screener dataframe and filtered baseline dataframe
  clean_df <- merge(screener_df, baseline_df, by="SWANID")
  
  # Column names needed to remove for finding intermediate covariates
  baseline_cols <- names(clean_df)
  
  # TODO: delete
  # Create censoring variable for baseline
  # clean_df <- clean_df %>%
  #   mutate("CSPINE0" = if_else(is.na(!!sym(baseline_Y0[1])), 0, 1),
  #          "CHIP0" = if_else(is.na(!!sym(baseline_Y0[2])), 0, 1)) 
  
  ##### Add Each Visit ##########################
  for (i in 3:length(all_icpsr)) {
    icpsr <- all_icpsr[i]
    code <- substr(icpsr, 7, 12)
    visit <- i-2
    
    print(c(code, visit))
    
    visit_i_W <- stack(sapply(visit_W, function(x) paste0(x, visit)))$values
    visit_i_A <- stack(sapply(A_prefix, function(x) paste0(x, visit)))$values
    visit_i_Y <- stack(sapply(Y_prefix, function(x) paste0(x, visit)))$values
    visit_i_df <- eval(parse(text=paste(sprintf("da%s.0001", code))))
    visit_i_df <- visit_i_df %>%
      select(all_of(c("SWANID", visit_i_W, visit_i_A, visit_i_Y)))
    
    visit_i_depress <- stack(sapply(depress_prefix, function(x) paste0(x, visit)))$values
    visit_i_anxiety <- stack(sapply(anxiety_prefix, function(x) paste0(x, visit)))$values
    
    visit_i_W_factor_minus1 <- c(visit_i_W[-c(1:4)], visit_i_A)
    visit_i_df <- visit_i_df %>%
      mutate(across(all_of(visit_i_W_factor_minus1), as.numeric)) %>%
      mutate(across(all_of(visit_i_W_factor_minus1), ~ . -1)) %>%
      rowwise() %>%
      mutate("DEPRESSION#" = sum(c_across(all_of(visit_i_depress))),
             "ANXIETY#" = sum(c_across(all_of(visit_i_anxiety))),
             "HORMUSER#" = case_when(
               sum(c_across(all_of(visit_i_A))) == 0 ~ 0,
               sum(c_across(all_of(visit_i_A))) > 0 ~ 1,
               TRUE ~ NA
             )) %>%
      ungroup() %>%
      rename_with(~gsub("#", visit, .x, fixed=T))
    
    visit_i_W <- c(# "AGE", 
                   "HEIGHT",
                   "WEIGHT",
                   "STATUS", 
                   "DIABETE",
                   "INSULN1",
                   # "DRNKBEE",
                   # "PHYSACT",
                   "MARITAL",
                   "DEPRESSION",
                   "ANXIETY")
    visit_i_W <- stack(sapply(visit_i_W, function(x) paste0(x, visit)))$values
    
    visit_i_df <- visit_i_df %>%
      select(all_of(c("SWANID", visit_i_W, sprintf("HORMUSER%s", visit), visit_i_Y)))
    
    # TODO: delete
    # visit_i_df <- visit_i_df %>%
    #   mutate("CSPINE#" = if_else(is.na(!!sym(visit_i_Y[1])), 0, 1),
    #          "CHIP#" = if_else(is.na(!!sym(visit_i_Y[2])), 0, 1)) %>%
    #   rename_with(~gsub("#", visit, .x, fixed=T))
    
    # left join with baseline
    clean_df <- merge(clean_df, visit_i_df, by="SWANID", all.x=T)
  }
  
  ##### Additional Data Cleaning (EH) ###########
  
  ##### Renaming Columns ########################
  clean_df <- rename(clean_df, DIABETE0 = DIABETE)
  clean_df <- rename(clean_df, MARITAL0 = MARITALGP)
  names(clean_df) <- sub("INSULN\\d?", "INSULIN", names(clean_df)) #fixed insuln to insulin and gets rid of the extra digit
  
  ##### Factor/Numeric/One-Hot Encoding #########
  # Make the level names of all of the STATUS columns the same
  status_cols <- grep("STATUS", names(clean_df), value = TRUE)[-1]
  for (col in status_cols) {
    levels(clean_df[[col]]) <- levels(clean_df$STATUS0)
  }
  
  # Make baseline diabetes 0/1
  no_diabetes <- levels(clean_df$DIABETE0)[1]
  clean_df$DIABETE0 <- ifelse(clean_df$DIABETE0 == no_diabetes, 0, 1)
  
  ##### LVCF for Intermediate Covariates ########
  final_baseline_col_index <- which(names(clean_df) == "HPBMDT0")
  int_cov <- grep("HORMUSER|SPBMDT|HPBMDT", names(clean_df)[-c(1:final_baseline_col_index)], value = TRUE, invert = TRUE)
  
  for (cur_col in int_cov) {
    i <- as.numeric(regmatches(cur_col, regexpr('\\d+', cur_col))) #extracts the number at the end of the column name
    prev_col <- paste(regmatches(cur_col, regexpr('[[:alpha:]]+', cur_col)), i-1, sep = "") #gets the name of the column from the previous visit
    ind <- which(is.na(clean_df[cur_col])) #index of which values are NA and need to get replaced
    clean_df[cur_col][ind,] <- clean_df[prev_col][ind,] #replaces NAs with value from previous visit
  }
  
  ##### Censoring for Exposure ##################
  treat_cols <- grep("HORMUSER", names(clean_df), value = TRUE)
  delta_cols <- sub("USER", "DELTA", treat_cols)
  
  for (i in seq_along(treat_cols)) {
    clean_df <- clean_df %>%
      mutate(
        !!delta_cols[i] := ifelse(is.na(.data[[treat_cols[i]]]), 0, 1)) %>% #makes the delta col
      relocate(
        !!sym(delta_cols[i]), .after = !!sym(treat_cols[i])) #moves it right after the corresponding treatment col
  }
  
  ##### Censoring for Outcome ###################
  outcome_cols <- sapply(0:10, function(i) sapply(Y_prefix, function(x) paste0(x, i)))
  outcome_cens_cols <- sapply(0:10, function(i) sapply(c("CSPINE", "CHIP"), function(x) paste0(x, i)))
  
  for (i in seq_along(outcome_cols)) {
    clean_df <- clean_df %>%
      mutate(
        !!outcome_cens_cols[i] := ifelse(is.na(.data[[outcome_cols[i]]]), 0, 1)
      ) %>%
      relocate(
        !!sym(outcome_cens_cols[i]), .before = !!sym(outcome_cols[i])
      )
  }
  
  ##### Write clean_df! #########################
  # write.csv(clean_df, here("data", "clean_df.csv"), row.names=FALSE)
  return(clean_df)
}


#------------------------------------------------
# do.LTMLE: function to do LTMLE & get inference
# 
# input: data, Anodes, Cnodes, Lnodes, Ynodes, abar, SL_library
# output: point estimate, variance
#------------------------------------------------

do.LTMLE <- function(data, Anodes, Cnodes, Lnodes, Ynodes, abar, SL_library) {
  return("todo!")
}

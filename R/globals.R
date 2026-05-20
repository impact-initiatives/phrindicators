# Suppress R CMD check notes for unqualified column-name references used inside
# dplyr verbs (e.g. mutate, case_when) and for utility functions resolved at
# runtime from other packages.

#' @importFrom rlang `:=`
#' @importFrom dplyr all_of c_across
#' @importFrom stats sd
NULL

utils::globalVariables(c(
  # General / shared
  ":=",

  # add_standardized_roster_demographics
  ".sex_std",

  # add_mfaz, add_muac
  "age_months",
  "flag_muac_extreme",
  "flag_sd_mfaz",
  "gam_muac",
  "global_mfaz",
  "mam_muac",
  "mfaz",
  "moderate_mfaz",
  "nut_mfaz_cat",
  "nut_mfaz_cat_noflag",
  "nut_muac_cat",
  "sam_muac",
  "severe_mfaz",

  # add_ecfies
  "nut_ecfies_cat",

  # add_standardized_age
  "calc_age_days",
  "calc_age_months",

  # add_persontime
  "entry_date",
  "exit_date",
  "person_time",

  # add_fcs
  "fcs_weight_cereal1",
  "fcs_weight_dairy3",
  "fcs_weight_fruit6",
  "fcs_weight_legume2",
  "fcs_weight_meat4",
  "fcs_weight_oil7",
  "fcs_weight_sugar8",
  "fcs_weight_veg5",
  "fsl_fcs_cat",

  # add_hdds
  "fsl_hdds_cat",

  # add_hhs
  "fsl_hhs_cat",
  "fsl_hhs_cat_ipc",
  "fsl_hhs_comp1",
  "fsl_hhs_comp2",
  "fsl_hhs_comp3",
  "hhs_alldaynight_freq_numeric",
  "hhs_alldaynight_numeric",
  "hhs_nofoodhh_freq_numeric",
  "hhs_nofoodhh_numeric",
  "hhs_sleephungry_freq_numeric",
  "hhs_sleephungry_numeric",

  # add_rcsi
  "any_weighted_na",
  "fsl_rcsi_cat",
  "rcsi_borrow_weighted",
  "rcsi_lessquality_weighted",
  "rcsi_mealadult_weighted",
  "rcsi_mealnb_weighted",
  "rcsi_mealsize_weighted",

  # add_lcsi
  "complete_lcsi_inputs",
  "lcsi",

  # add_fclcm_phase / add_fcm_phase
  "cell",
  "fc_phase",
  "fcs",
  "fsl_fc_phase",
  "fsl_fclcm_phase",
  "hdds",
  "hhs",
  "rcsi",

  # add_liters_per_person_per_day
  "liters_log",
  "liters_pppd",
  "liters_pppd_log",
  "temp_num_days_col",

  # add_drinking_water_jmp_ladder
  "wash_jmp_ladder_drinking_water_cat",

  # add_sanitation_jmp_ladder
  "wash_jmp_ladder_sanitation_cat",

  # add_foregone_care
  "health_foregone_care_cat",

  # hh_check_roster_1_to_many (internal helper)
  "hh_roster_link_check"
))

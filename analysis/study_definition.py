#######################################
### Get all data required for study ###
#######################################

# Purpose: To extract data from OpenSAFELY on required measures, store in the safe environment, and then generate monthly summary statistics that can be output and analysed later.

### Set up python ###

# Define files to utilise in the script
from cohortextractor import StudyDefinition, Measure, patients, codelist, codelist_from_csv


### Load codelists ###

# 1. All ambulatory care sensitive conditions
acs_codes_all = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions.csv", system="icd10", column="icd10_code" 
)

# 2. Ambulatory care sensitive - only acute conditions
acs_codes_acute = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-acute.csv", system="icd10", column="icd10_code" 
)

# 3. Ambulatory care sensitive - only chronic conditions
acs_codes_chronic = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-chronic.csv", system="icd10", column="icd10_code" 
)

# 4. Ambulatory care sensitive - only vaccine preventable conditions
acs_codes_vacine = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-vaccine-preventable.csv", system="icd10", column="icd10_code" 
)

# 5. Emergency urgent care sensitive
eucs_codes = codelist_from_csv(
    "codelists/user-mgreen-emergency-urgent-care-sensitive.csv", system="icd10", column="icd10_code" 
)


### Define study population ###

# See https://docs.opensafely.org/study-def-variables/ for options and variables

# Define dates
index_date = "2019-01-01"
end_date = "2022-02-28"

# Define study paramteres
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"}, # Use data from these dates
        "rate": "uniform",
        "incidence": 0.5,
    },
    
    # Define index date
    index_date = "2019-01-01",
    
    # Select all people
    population = patients.all(),
    
    # # Select patients who have remained at the same practice between the following dates
    # population = patients.registered_with_one_practice_between( 
    #     "2019-01-01", "2022-01-31"
    # ),
    
    # Extract data on age
    age = patients.age_as_of( # Select age of patients on the specific date below
          "2020-03-01",
          return_expectations={ # Set expectations for patients
            "rate": "universal", # They must have a value (expect all records to have an age)
            "int": {"distribution": "population_ages"}, # Distribution of values should match UK population
          },
    ), 
    
    # ageband=patients.categorised_as(
    #     {
    #         "0": "DEFAULT",
    #         "0-15": """ age >= 0 AND age < 16 """,
    #         "16-29": """ age >= 16 AND age < 30""",
    #         "30-39": """ age >= 30 AND age < 40""",
    #         "40-49": """ age >= 40 AND age < 50""",
    #         "50-59": """ age >= 50 AND age < 60""",
    #         "60-69": """ age >= 60 AND age < 70""",
    #         "70-79": """ age >= 70 AND age < 80""",
    #         "80+": """ age >=  80 AND age < 120""",  
    #     },
    #     return_expectations={
    #         "rate": "universal",
    #         "category": {
    #             "ratios": {
    #                 "0-15": 0.125,
    #                 "16-29": 0.125,
    #                 "30-39": 0.125,
    #                 "40-49": 0.125,
    #                 "50-59": 0.125,
    #                 "60-69": 0.125,
    #                 "70-79": 0.125,
    #                 "80+": 0.125,
    #             }
    #         },
    #     },
    # ),
    
    # Extract patient's sex   
    sex = patients.sex(
          return_expectations={
              "rate": "universal",
              "category": {"ratios": {"M": 0.49, "F": 0.51}},
          }
    ),
    
    # Region
    region = patients.registered_practice_as_of(
    "2020-03-01",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2,
                },
            },
        },
    ),
        
    # IMD - quintile
    imd_quintile = patients.categorised_as(
        {
            "0": "DEFAULT", 
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""", # Most deprived quintile
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""", # Least deprived quintile
        },
        index_of_multiple_deprivation=patients.address_as_of(
            index_date,
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19,
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19,
                }
            },
        },
    ),
    
    # # IMD - score
    # imd = patients.address_as_of(
    #     "2020-03-01",
    #     returning="index_of_multiple_deprivation",
    #     round_to_nearest=100, # Might need to update
    #     return_expectations={
    #         "rate": "universal",
    #          "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}}, 
    #     },
    # ),
    
    # Urban-rural
    urban_rural = patients.address_as_of(
        "2020-03-01",
        returning="rural_urban_classification",
        return_expectations={ # Need to update if not correct categories
            "rate": "universal",
            "category": {"ratios": {"Urban": 0.8, "Rural": 0.2}},
        },
    ),

    # # These clinical events
    # avoidable_event = patients.with_these_clinical_events(
    #     codelist=opensafely-test, # Need to define
    #     between=["2019-01-01", "2022-02-10"],
    #     returning="binary_flag",
    #     return_expectations={"incidence": 0.1},
    # ),
    
    # Admitted to hospital - all admissions
    admitted = patients.admitted_to_hospital(
        returning="binary_flag",
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - all ambulatory care sensitive
    admitted_acs_all = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=acs_codes_all,
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - acute
    admitted_acs_acute = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=acs_codes_acute,
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - chronic
    admitted_acs_chronic = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=acs_codes_chronic,
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - vaccine preventable
    admitted_acs_vaccine = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=acs_codes_vacine,
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - emergency urgent care sensitive
    admitted_eucs = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=eucs_codes,
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # # SUS data - admitted to hospital for specific cause
    # avoidable_sus = patients.admitted_to_hospital(
    #     returning= "date_admitted",
    #     with_these_diagnoses=opensafely-test,
    #     between=["2019-01-01", "2022-02-10"],
    #     find_first_match_in_period=True,
    #     date_format="YYYY-MM-DD",
    #     return_expectations={"date": {"earliest"; "2019-01-01", "latest": "2022-02-10"}},
    # ),

    # # Died of any cause (ONS linked records)
    # died_any = patients.died_from_any_cause(
    #     between=["2019-01-01", "2022-01-31"],
    #     returning="binary_flag",
    #     return_expectations={"incidence": 0.05},
    # ),

    # # Died of specific causes
    # died_avoidable = patients.with_these_codes_on_death_certificate(
    #     codelist=opensafely-test,
    #     on_or_after="2020-02-01",
    #     match_only_underlying_cause=False,
    #     return_expectations={
    #         "date": {"earliest" : "2020-02-01"},
    #         "rate" : "exponential_increase"
    #     },
    # ),

    # # STP code
    # stp=patients.registered_practice_as_of(
    #       "index_date",
    #       returning="stp_code",
    #       return_expectations={
    #           "category": {"ratios": {"stp1": 0.1, "stp2": 0.2, "stp3": 0.7}},
    #           "incidence": 1,
    #       },
    #   ),

    # # Tested positive for COVID-19?
    # first_positive_test_date=patients.with_test_result_in_sgss(
    #     pathogen="SARS-CoV-2",
    #     test_result="positive",
    #     between=["2019-01-01", "2022-02-10"],
    #     find_first_match_in_period=True,
    #     returning="binary_flag",
    #     return_expectations={"incidence": 0.05},
    # ),

)


### Create monthly measures of summary statistics ###

measures = [
    
    # 1. Total hospital admissions
    
    # 1a. Overall
        Measure(
        id="hosp_admission_overall",
        numerator="admitted",
        denominator="population",
        group_by="population", 
        small_number_suppression=True,
    ),
    
    # 1b. Sex
    Measure(
        id="hosp_admission_by_sex",
        numerator="admitted",
        denominator="population",
        group_by="sex", # To calculate the measure across the entire population set group_by="population"
        # You can also do group_by=["region", "sex"] to group across multiple variables
        small_number_suppression=True,
    ),
    
    # 1c. Region
    Measure(
        id="hosp_admission_by_region",
        numerator="admitted",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 1d. Deprivation
    Measure(
        id="hosp_admission_by_imd",
        numerator="admitted",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 1e. Urban-rural
    Measure(
        id="hosp_admission_by_urban",
        numerator="admitted",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    # 2. Ambulatory care sensitive conditions - all
    
    # 2a. Overall
    Measure(
        id="acs_all_overall",
        numerator="admitted_acs_all",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
    # 2b. Sex
        Measure(
        id="acs_all_by_sex",
        numerator="admitted_acs_all",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
    # 2c. Region
        Measure(
        id="acs_all_by_region",
        numerator="admitted_acs_all",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 2d. Deprivation
        Measure(
        id="acs_all_by_imd",
        numerator="admitted_acs_all",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 2e. Urban-rural
    Measure(
        id="acs_all_by_urban",
        numerator="admitted_acs_all",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    # 3. Ambulatory care sensitive conditions - acute conditions only
    
    # 3a. Overall
    Measure(
        id="acs_acute_overall",
        numerator="admitted_acs_acute",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
    # 3b. Sex
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_acute",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
    # 3c. Region
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_acute",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 3d. Deprivation
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_acute",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 3e. Urban-rural
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_acute",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    # 4. Ambulatory care sensitive conditions - chronic conditions only
    
    # 4a. Overall
    Measure(
        id="acs_chronic_overall",
        numerator="admitted_acs_chronic",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
    # 4b. Sex
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_chronic",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
    # 4c. Region
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_chronic",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 4d. Deprivation
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_chronic",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 4e. Urban-rural
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_chronic",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    # 5. Ambulatory care sensitive conditions - vaccine preventable conditions only
    
    # 5a. Overall
    Measure(
        id="acs_vaccine_overall",
        numerator="admitted_acs_vaccine",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
    # 5b. Sex
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_vaccine",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
    # 5c. Region
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_vaccine",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 5d. Deprivation
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_vaccine",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 5e. Urban-rural
    Measure(
        id="eucs_overall",
        numerator="admitted_acs_vaccine",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    # 6. Emergency urgent care sensitive conditions
    
    # 6a. Overall
    Measure(
        id="eucs_overall",
        numerator="admitted_eucs",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
    # 6b. Sex
    Measure(
        id="eucs_overall",
        numerator="admitted_eucs",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
    # 6c. Region
    Measure(
        id="eucs_overall",
        numerator="admitted_eucs",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
    
    # 6d. Deprivation
    Measure(
        id="eucs_overall",
        numerator="admitted_eucs",
        denominator="population",
        group_by="imd_quintile",
        small_number_suppression=True,
    ),
    
    # 6e. Urban-rural
    Measure(
        id="eucs_overall",
        numerator="admitted_eucs",
        denominator="population",
        group_by="urban_rural",
        small_number_suppression=True,
    ),
    
    
    
]

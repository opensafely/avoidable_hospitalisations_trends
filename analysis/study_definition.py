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
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions.csv", system="icd10", column="code" 
)

# 2. Ambulatory care sensitive - only acute conditions
acs_codes_acute = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-acute.csv", system="icd10", column="code" 
)

# 3. Ambulatory care sensitive - only chronic conditions
acs_codes_chronic = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-chronic.csv", system="icd10", column="code" 
)

# 4. Ambulatory care sensitive - only vaccine preventable conditions
acs_codes_vacine = codelist_from_csv(
    "codelists/user-mgreen-ambulatory-care-sensitive-conditions-vaccine-preventable.csv", system="icd10", column="code" 
)

# 5. Emergency urgent care sensitive
eucs_codes = codelist_from_csv(
    "codelists/user-mgreen-emergency-urgent-care-sensitive.csv", system="icd10", column="code" 
)

# 6. Ethnicity 
ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv", system="ctv3", column="Code", category_column="Grouping_6")


### Define study population ###

# See https://docs.opensafely.org/study-def-variables/ for options and variables

# Define dates
index_date = "2019-01-01"
end_date = "2022-04-30"

# Define study paramteres
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"}, # Use data from these dates
        "rate": "uniform",
        "incidence": 0.5,
    },
    
    # Define index date
    index_date = index_date,
    
    # Select all people alive and registered at a GP in each month (with complete sex and age data)
    population=patients.satisfying(
        """
        registered
        AND
        NOT has_died
        AND 
        age <= 120
        """,
        registered=patients.registered_as_of(
            index_date,
        ),
        has_died=patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
        ),
    ),
    
    # Extract data on age
    age = patients.age_as_of( # Select age of patients on the specific date below
          index_date,
          return_expectations={ # Set expectations for patients
            "rate": "universal", # They must have a value (expect all records to have an age)
            "int": {"distribution": "population_ages"}, # Distribution of values should match UK population
          },
    ), 
    
    # Extract patient's sex   
    sex = patients.sex(
          return_expectations={
              "rate": "universal",
              "category": {"ratios": {"M": 0.49, "F": 0.51}},
          }
    ),
    
    # Define ethnicity when reported
    ethnicity_gp=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        on_or_before="index_date",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={"category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
                             "incidence": 0.75},
    ),

    # Get records from SUS
    ethnicity_sus=patients.with_ethnicity_from_sus(
        returning="group_6",
        use_most_frequent_code=True,
        return_expectations={
            "category": {
                            "ratios": {
                                "1": 0.2,
                                "2": 0.2,
                                "3": 0.2,
                                "4": 0.2,
                                "5": 0.2
                                }
                            },
            "incidence": 0.4,
            },
    ),

    # Fill missing ethnicity from SUS
    ethnicity=patients.categorised_as(
            {
                "0": "DEFAULT",
                "1": "ethnicity_gp='1' OR (NOT ethnicity_gp AND ethnicity_sus='1')",
                "2": "ethnicity_gp='2' OR (NOT ethnicity_gp AND ethnicity_sus='2')",
                "3": "ethnicity_gp='3' OR (NOT ethnicity_gp AND ethnicity_sus='3')",
                "4": "ethnicity_gp='4' OR (NOT ethnicity_gp AND ethnicity_sus='4')",
                "5": "ethnicity_gp='5' OR (NOT ethnicity_gp AND ethnicity_sus='5')",
            },
            return_expectations={
                "category": {
                                "ratios": {
                                    "0": 0.5,  # missing in 50%
                                    "1": 0.1,
                                    "2": 0.1,
                                    "3": 0.1,
                                    "4": 0.1,
                                    "5": 0.1
                                    }
                                },
                "rate": "universal",
            },
    ),
    
    # Note:
    # White = 1
    # Mixed = 2
    # Asian or Asian British = 3
    # Black or Black British = 4
    # Other ethnic groups = 5
    
    # Region
    region = patients.registered_practice_as_of(
        index_date,
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
        index_date,
        returning="rural_urban_classification",
        return_expectations={ # Need to update if not correct categories
            "rate": "universal",
            "category": {"ratios": {"Urban": 0.8, "Rural": 0.2}},
        },
    ),

    # Number of GP observations during the month
    gp_count = patients.with_gp_consultations(
        between=["index_date", "last_day_of_month(index_date)"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 6, "stddev": 3},
            "incidence": 0.6,
        },
    ),

    # # These clinical events
    # avoidable_event = patients.with_these_clinical_events(
    #     codelist=opensafely-test, # Need to define
    #     between=["2019-01-01", "2022-02-10"],
    #     returning="binary_flag",
    #     return_expectations={"incidence": 0.1},
    # ),
    
    # Admitted to hospital - all emergency admissions
    admitted = patients.admitted_to_hospital(
        returning="binary_flag",
        between=["index_date", "last_day_of_month(index_date)"],
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'], # Select all emergency admissions only
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - all ambulatory care sensitive
    admitted_acs_all = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_primary_diagnoses=acs_codes_all,
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'],
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - acute
    admitted_acs_acute = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_primary_diagnoses=acs_codes_acute,
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'],
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - chronic
    admitted_acs_chronic = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_primary_diagnoses=acs_codes_chronic,
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'],
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - ambulatory care sensitive - vaccine preventable
    admitted_acs_vaccine = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_primary_diagnoses=acs_codes_vacine,
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'],
        between=["index_date", "last_day_of_month(index_date)"],
        return_expectations={"incidence": 0.1},
    ),
    
    # Admitted to hospital - emergency urgent care sensitive
    admitted_eucs = patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_primary_diagnoses=eucs_codes,
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'],
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
    #     codelist=codelist,
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
    
    # # 1c. Region
    # Measure(
    #     id="hosp_admission_by_region",
    #     numerator="admitted",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 1d. Deprivation
    # Measure(
    #     id="hosp_admission_by_imd",
    #     numerator="admitted",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 1e. Urban-rural
    # Measure(
    #     id="hosp_admission_by_urban",
    #     numerator="admitted",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    
    
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
    
    # # 2c. Region
    #     Measure(
    #     id="acs_all_by_region",
    #     numerator="admitted_acs_all",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 2d. Deprivation
    #     Measure(
    #     id="acs_all_by_imd",
    #     numerator="admitted_acs_all",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 2e. Urban-rural
    # Measure(
    #     id="acs_all_by_urban",
    #     numerator="admitted_acs_all",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    
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
    
    # # 3c. Region
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_acute",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 3d. Deprivation
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_acute",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 3e. Urban-rural
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_acute",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    
    
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
    
    # # 4c. Region
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_chronic",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 4d. Deprivation
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_chronic",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 4e. Urban-rural
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_chronic",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    
    
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
    
    # # 5c. Region
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_vaccine",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 5d. Deprivation
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_vaccine",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 5e. Urban-rural
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_acs_vaccine",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    
    
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
    
    # # 6c. Region
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_eucs",
    #     denominator="population",
    #     group_by=["region", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 6d. Deprivation
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_eucs",
    #     denominator="population",
    #     group_by=["imd_quintile", "sex"],
    #     small_number_suppression=True,
    # ),
    # 
    # # 6e. Urban-rural
    # Measure(
    #     id="eucs_overall",
    #     numerator="admitted_eucs",
    #     denominator="population",
    #     group_by=["urban_rural", "sex"],
    #     small_number_suppression=True,
    # ),
    
    # 7. Number of GP observations
    
    # 7a. Overall
    Measure(
        id="gp_overall",
        numerator="gp_count",
        denominator="population",
        group_by="population",
        small_number_suppression=True,
    ),
    
        # 7a. Overall
    Measure(
        id="gp_by_sex",
        numerator="gp_count",
        denominator="population",
        group_by="sex",
        small_number_suppression=True,
    ),
    
]

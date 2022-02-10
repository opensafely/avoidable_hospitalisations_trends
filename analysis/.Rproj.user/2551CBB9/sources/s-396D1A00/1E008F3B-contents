# Define files to utilise in the script
from cohortextractor import StudyDefinition, Measure, patients # , codelist, codelist_from_csv

# # Load codelists
# opensafely-test = codelist_from_csv(
#     "codelists/opensafely-test.csv", system="icd10", column="icd10_code"
# )

# See https://docs.opensafely.org/study-def-variables/ for options and variables

# Define dates
index_date="2019-01-01"
end_date = "2022-01-31"

# Define study paramteres
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"}, # Use data from these dates
        "rate": "uniform",
        "incidence": 0.5,
    },

    index_date="2019-01-01",
    
    # Select all people
    population=patients.all(),
    
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
        
    # IMD
    imd = patients.address_as_of(
        "2020-03-01",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100, # Might need to update
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}},
        },
    ),

    # Urban-rural
    urban_rural=patients.address_as_of(
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
    
    # Admitted to hospital - all (SUS data)
    admitted = patients.admitted_to_hospital(
        returning="binary_flag",
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

    # Died of any cause (ONS linked records)
    died_any = patients.died_from_any_cause(
        between=["2019-01-01", "2022-01-31"],
        returning="binary_flag",
        return_expectations={"incidence": 0.05},
    ),

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

# Create monthly measures of summary statistics
measures = [
    Measure(
        id="hosp_admission_by_region",
        numerator="admitted",
        denominator="population",
        group_by="region", # To calculate the measure across the entire population set group_by="population"
        # You can also do group_by=["region", "sex"] to group across multiple variables
    ),
    Measure(
        id="death_by_region",
        numerator="died_any",
        denominator="population",
        group_by="region",
        small_number_suppression=True,
    ),
]

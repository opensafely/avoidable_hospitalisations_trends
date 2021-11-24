# Define files to utilise in the script
from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Define study paramteres
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"}, # Use data from these dates
        "rate": "uniform",
        "incidence": 0.5,
    },
    
    # Select patients who have remained at the same practice between the following dates
    population = patients.registered_with_one_practice_between( 
        "2019-02-01", "2020-02-01"
    ),
    
    # Extract data on age
    age = patients.age_as_of( # Select age of patients on the specific date below
          "2019-09-01",
          return_expectations={ # Set expectations for patients
            "rate": "universal", # They must have a value
            "int": {"distribution": "population_ages"}, # Distribution of values should match UK population
          },
    ), 
    
)

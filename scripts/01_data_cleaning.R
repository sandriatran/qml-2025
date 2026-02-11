# -----------------------------------------------------------------------------
# STEP 1: Data Loading & Preprocessing
# -----------------------------------------------------------------------------
# Assumes 'data/ota2009/key-rock.csv' exists relative to project root
data_path <- here("data", "ota2009", "key-rock.csv")
data_raw <- read_csv(data_path, show_col_types = FALSE)

data_clean <- data_raw %>%
    filter(Procedure == "TrialProc") %>%
    filter(Condition == "Unrelated") %>%
    filter(Contrast %in% c("F", "LR", "H", "PB")) %>%
    mutate(
        subject_id = factor(Subject),
        item_id = factor(Item),
        accuracy = Words.ACC,
        contrast_type = factor(Contrast, levels = c("F", "LR", "H", "PB")),

        # Theoretical Property: Phonological distinctness scale
        phonologically_distinct = case_when(
            Contrast == "F" ~ 1.0, # Spelling control: fully distinct
            Contrast == "PB" ~ 0.8, # /p/-/b/ present in Japanese: fairly distinct
            Contrast == "LR" ~ 0.3, # /l/-/r/ absent in Japanese: indeterminate
            Contrast == "H" ~ 0.0 # True homophones: phonologically fused
        ),
        phonological_status = factor(
            case_when(
                Contrast == "F" ~ "Unrelated",
                Contrast == "H" ~ "Homophone",
                Contrast == "LR" ~ "L1_absent_contrast",
                Contrast == "PB" ~ "L1_present_contrast"
            ),
            levels = c("Unrelated", "L1_present_contrast", "L1_absent_contrast", "Homophone")
        )
    )

# Create output directory for models
output_dir <- here("outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

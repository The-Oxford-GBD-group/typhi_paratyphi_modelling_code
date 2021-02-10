import os

from stgpr.common.constants import paths as stgpr_paths

##### PATHS TO DATA #####
# Directories for vetting input and output.
ORIGINAL: str = 'original'
NEW: str = 'new'
VETTING_ROOT: str = '/ihme/code/st_gpr/vetting'
VETTING_RUNS_ROOT: str = os.path.join(VETTING_ROOT, 'suite_runs')
VETTING_RUN_FORMAT: str = os.path.join(VETTING_RUNS_ROOT, '{suite_run}')
VETTING_INPUTS_ROOT: str = 'inputs'
VETTING_PDFS_ROOT: str = 'pdfs'

# Individual vetting suite input and output files.
ORIGINAL_CONFIG: str = f'{ORIGINAL}_config.csv'
NEW_CONFIG: str = f'{NEW}_config.csv'
PATH_TO_DATA: str = 'data.csv'
CUSTOM_COVARIATES: str = 'custom_covariates.csv'
CUSTOM_STAGE_1: str = 'custom_stage_1.csv'
VETTING_RUN_ID: str = 'run_id.txt'
RUN_SETTINGS: str = 'settings.pdf'
SCATTERS: str = 'scatters.pdf'

##### PATHS TO CODE #####
# Root directory for vetting scripts.
VETTING_SCRIPTS_ROOT: str = os.path.join(
    stgpr_paths.CODE_ROOT, 'vetting', 'scripts'
)

# Vetting suite scripts.
GENERATE_CONFIG_SCRIPT: str = os.path.join(
    VETTING_SCRIPTS_ROOT, 'generate_config.py'
)
RUN_VETTING_MODEL_SCRIPT: str = os.path.join(
    VETTING_SCRIPTS_ROOT, 'run_vetting_model.py'
)
PLOT_SCATTERS_SCRIPT: str = os.path.join(
    VETTING_SCRIPTS_ROOT, 'plot_scatters.py'
)

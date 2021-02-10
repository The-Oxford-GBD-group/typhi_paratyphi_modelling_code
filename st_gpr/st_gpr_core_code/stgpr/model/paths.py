import os
from pathlib import Path

# Main parent directories - go two dirs up from this file
CENTRAL_ROOT = Path(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))))

# Second level parents of central
MODEL_ROOT = CENTRAL_ROOT / 'model'
TEMPLATE_ROOT = CENTRAL_ROOT / 'templates'
SHELLS_ROOT = CENTRAL_ROOT / 'shells'

# Individual templates
MODEL_TEMPLATE = TEMPLATE_ROOT / 'model_template.csv'
RUN_TEMPLATE = TEMPLATE_ROOT / 'run_template.csv'
ME_TEMPLATE = TEMPLATE_ROOT / 'me_template.csv'
OUTLIER_TEMPLATE = TEMPLATE_ROOT / 'outlier_template.csv'

# Cluster root
CLUSTER_ROOT = Path('/ihme/covariates/ubcov/model')

# Child dirs in cluster
CLUSTER_DATA = CLUSTER_ROOT / 'data'
CLUSTER_OUTPUT = CLUSTER_ROOT / 'output'
CLUSTER_SQUARE = CLUSTER_ROOT / 'square'

# Databases
LIBRARY_ROOT = CLUSTER_ROOT / 'st_gpr_library'
DATABASE_ROOT = LIBRARY_ROOT / 'databases'

ME_DB = DATABASE_ROOT / 'me_db.csv'
DATA_DB = DATABASE_ROOT / 'data_db.csv'
MODEL_DB = DATABASE_ROOT / 'model_db.csv'
RUN_DB = DATABASE_ROOT / 'run_db.csv'

OUTLIER_ROOT = DATABASE_ROOT / 'outliers'

# TESTING PATHS - note I didn't keep databases root because
# I'd like to get rid of it when switching over
TEST_CLUSTER_ROOT = Path('/ihme/covariates/ubcov/test/model')

TEST_CLUSTER_DATA = TEST_CLUSTER_ROOT / 'data'
TEST_CLUSTER_OUTPUT = TEST_CLUSTER_ROOT / 'output'
TEST_CLUSTER_SQUARE = TEST_CLUSTER_ROOT / 'square'

TEST_LIBRARY_ROOT = TEST_CLUSTER_ROOT / 'st_gpr_library'

TEST_ME_DB = TEST_LIBRARY_ROOT / 'me_db.csv'
TEST_DATA_DB = TEST_LIBRARY_ROOT / 'data_db.csv'
TEST_MODEL_DB = TEST_LIBRARY_ROOT / 'model_db.csv'
TEST_RUN_DB = TEST_LIBRARY_ROOT / 'run_db.csv'

TEST_OUTLIER_ROOT = TEST_LIBRARY_ROOT / 'outliers'

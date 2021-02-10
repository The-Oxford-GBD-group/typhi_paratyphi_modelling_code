import os

import pandas as pd

from stgpr_helpers.api import internal as stgpr_helpers_internal
from stgpr_helpers.api.constants import columns, parameters

from stgpr.common.constants import paths as stgpr_paths
from stgpr.vetting.util import path_utils as vetting_path_utils
from stgpr.vetting.util.constants import paths as vetting_paths


_LIST_PARAMETERS = [
    parameters.DENSITY_CUTOFFS,
    parameters.GBD_COVARIATES,
    parameters.LEVEL_4_TO_3_AGGREGATE,
    parameters.LEVEL_5_TO_4_AGGREGATE,
    parameters.LEVEL_6_TO_5_AGGREGATE,
    parameters.PREDICTION_AGE_GROUP_IDS,
    parameters.PREDICTION_SEX_IDS,
    parameters.PREDICTION_YEAR_IDS,
    parameters.ST_CUSTOM_AGE_VECTOR,
    parameters.GPR_SCALE,
    parameters.ST_LAMBDA,
    parameters.ST_OMEGA,
    parameters.ST_ZETA
]


def save_config_and_data(
        suite_run: str,
        run_id: int,
        gbd_round_id: int,
        decomp_step: str,
        holdouts: int,
        draws: int
) -> None:
    """
    Saves the config and data associated with a real ST-GPR run to a vetting
    suite directory.
    """
    params = stgpr_helpers_internal.get_parameters(run_id)
    me_id = params[parameters.MODELABLE_ENTITY_ID]
    inputs_root = vetting_path_utils.get_model_input_root(suite_run, me_id)
    os.makedirs(inputs_root, exist_ok=True)

    # Convert list parameters to strings.
    for param in _LIST_PARAMETERS:
        params[param] = ','.join([str(x) for x in params[param]])

    # Assign round, step, holdout, and draws to use in each model.
    params[parameters.GBD_ROUND_ID] = gbd_round_id
    params[parameters.DECOMP_STEP] = decomp_step
    params[parameters.HOLDOUTS] = holdouts
    params[parameters.GPR_DRAWS] = draws

    # Save data to a file, and always use path_to_data instead of
    # crosswalk version (since the vetting suite runs against the test
    # database which may not contain the crosswalk version used in this run).
    data = _load_data(run_id)
    if columns.DATA in data and columns.VAL not in data:
        data = data.rename(columns={columns.DATA: columns.VAL})
    path_to_data = os.path.join(inputs_root, vetting_paths.PATH_TO_DATA)
    data.to_csv(path_to_data, index=False)
    params[parameters.PATH_TO_DATA] = path_to_data
    if params[parameters.CROSSWALK_VERSION_ID]:
        del params[parameters.CROSSWALK_VERSION_ID]

    # If this run had custom covariates, save them to a file.
    custom_covariates = stgpr_helpers_internal.get_custom_covariates(run_id)
    if custom_covariates is not None:
        custom_covariates_path = os.path.join(
            inputs_root, vetting_paths.CUSTOM_COVARIATES
        )
        params[parameters.PATH_TO_CUSTOM_COVARIATES] = custom_covariates_path
        custom_covariates.to_csv(custom_covariates_path, index=False)

    # If this run had custom stage 1, save it to a file.
    custom_stage_1 = stgpr_helpers_internal.get_custom_stage_1_estimates(run_id)
    if custom_stage_1 is not None:
        custom_stage_1 = custom_stage_1\
            .rename(columns={columns.VAL: columns.CUSTOM_STAGE_1})
        custom_stage_1_path = os.path.join(
            inputs_root, vetting_paths.CUSTOM_STAGE_1
        )
        params[parameters.PATH_TO_CUSTOM_STAGE_1] = custom_stage_1_path
        custom_stage_1.to_csv(custom_stage_1_path, index=False)

    # If this run did not have a custom offset, don't save the offset.
    if params[parameters.IS_CUSTOM_OFFSET]:
        del params[parameters.TRANSFORM_OFFSET]

    # Now save the config.
    params_df = pd\
        .DataFrame(params, index=[0])\
        .drop(columns=[parameters.NOTES])
    config_files_to_save = [
        vetting_path_utils.get_config_path(suite_run, me_id, True),
        vetting_path_utils.get_config_path(suite_run, me_id, False)
    ]
    for config_file in config_files_to_save:
        params_df.to_csv(config_file, index=False)


def _load_data(run_id: int) -> pd.DataFrame:
    model_root = stgpr_paths.OUTPUT_ROOT_FORMAT.format(run_id=run_id)
    data_filepath = os.path.join(model_root, stgpr_paths.DATA_H5)
    return pd.read_hdf(data_filepath, stgpr_paths.DATA_OBJ)

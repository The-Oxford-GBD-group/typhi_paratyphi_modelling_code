import os

from stgpr.common.constants import paths as stgpr_paths
from stgpr.vetting.util.constants import paths as vetting_paths


def get_run_root(suite_run: str) -> str:
    """
    Returns the path to the root directory of a vetting run.
    This directory contains configs, input data, model runs, and PDF outputs.
    """
    return vetting_paths.VETTING_RUN_FORMAT.format(suite_run=suite_run)


def get_me_root(suite_run: str, modelable_entity_id: int) -> str:
    """
    Returns the path to the root directory of a ME used in the vetting suite.
    This directory contains configs, data, anything needed to run a model for
    that ME.
    """
    vetting_run_root = get_run_root(suite_run)
    return os.path.join(vetting_run_root, str(modelable_entity_id))


def get_model_input_root(suite_run: str, modelable_entity_id: int) -> str:
    """
    Returns the path to the directory of configs and data for a model run in
    the vetting suite.
    """
    vetting_me_root = get_me_root(suite_run, modelable_entity_id)
    return os.path.join(vetting_me_root, vetting_paths.VETTING_INPUTS_ROOT)


def get_model_output_root(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> str:
    """
    Returns the path to the output directory of a model that ran as part of a
    vetting suite run.
    """
    vetting_me_root = get_me_root(suite_run, modelable_entity_id)
    original = vetting_paths.ORIGINAL if is_original else vetting_paths.NEW
    return os.path.join(vetting_me_root, original)


def get_me_pdfs_root(suite_run: str, modelable_entity_id: int) -> str:
    """
    Returns the path to the directory of PDFs generated for a ME during a
    vetting suite run.
    """
    vetting_me_root = get_me_root(suite_run, modelable_entity_id)
    return os.path.join(vetting_me_root, vetting_paths.VETTING_PDFS_ROOT)


def get_config_path(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> str:
    """
    Returns the path to a config (original or new) used in a vetting suite run.
    """
    vetting_inputs_root = get_model_input_root(suite_run, modelable_entity_id)
    config_file = (
        vetting_paths.ORIGINAL_CONFIG if is_original
        else vetting_paths.NEW_CONFIG
    )
    return os.path.join(vetting_inputs_root, config_file)


def get_run_id_path(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> str:
    """
    Returns the path to a file containing a run ID associated with a model
    that ran as part of the vetting suite.
    """
    vetting_model_root = get_model_output_root(
        suite_run, modelable_entity_id, is_original
    )
    return os.path.join(vetting_model_root, vetting_paths.VETTING_RUN_ID)


def get_draws_root_path(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool,
        holdout: int
) -> str:
    """
    Returns the path to the directory of output draws for a model that ran as
    part of a vetting suite run.
    """
    vetting_model_root = get_model_output_root(
        suite_run, modelable_entity_id, is_original
    )
    return os.path.join(
        vetting_model_root,
        stgpr_paths.DRAWS_ROOT_FORMAT.format(holdout=holdout)
    )

from distutils import dir_util
import getpass
import os
from typing import Optional

from db_tools import ezfuncs
from stgpr_helpers.api import internal as stgpr_helpers_internal
from stgpr_helpers.api.constants import conn_defs

import stgpr
from stgpr.common.constants import paths
from stgpr.model import run_master
from stgpr.vetting.util import path_utils, vetting_helpers


def run_vetting_model(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool,
        project: str,
        original_run_id: Optional[int]
) -> None:
    config_path = path_utils.get_config_path(
        suite_run, modelable_entity_id, is_original
    )
    output_path = path_utils.get_model_output_root(
        suite_run, modelable_entity_id, is_original
    )

    if is_original and original_run_id:
        # Create original model output directory.
        os.umask(0o0002)
        os.makedirs(output_path, exist_ok=True)

        # Copy original model run ID and outputs.
        run_id = original_run_id
        vetting_helpers.save_run_id(
            run_id, suite_run, modelable_entity_id, is_original
        )
        original_output_path = paths.OUTPUT_ROOT_FORMAT.format(
            run_id=original_run_id
        )
        dir_util.copy_tree(original_output_path, output_path)
    else:
        # Point stgpr_helpers to the test database and register a new model.
        with ezfuncs.session_scope(conn_defs.STGPR_TEST) as scoped_session:
            run_id = stgpr_helpers_internal.create_stgpr_version(
                config_path, output_path=output_path, session=scoped_session
            )

        vetting_helpers.save_run_id(
            run_id, suite_run, modelable_entity_id, is_original
        )

        # Launch the model.
        username = getpass.getuser()
        run_master.run_job_swarm(
            run_id=run_id,
            error_log_path=paths.ERROR_LOG_PATH_FORMAT.format(
                username=username
            ),
            output_log_path=paths.OUTPUT_LOG_PATH_FORMAT.format(
                username=username
            ),
            project=project,
            nparallel=25,
            code_version=stgpr.__version__,
            output_path=output_path
        )

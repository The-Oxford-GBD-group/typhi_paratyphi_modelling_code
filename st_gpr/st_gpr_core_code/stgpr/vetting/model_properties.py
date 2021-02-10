from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd
from sqlalchemy import orm

import gbd
from stgpr_helpers.api import internal as stgpr_helpers_internal
from stgpr_helpers.api.constants import enums, parameters

from stgpr.common.constants import paths

# Threshold that determines whether data is close to 0 or 1.
_THRESHOLD = 0.01


class ModelProperties:
    """Contains high-level summary of a model"""

    def __init__(self, run_id: int, session: orm.Session):
        params = stgpr_helpers_internal.get_parameters(run_id)
        self.num_datapoints: int = _get_num_datapoints(run_id)
        self.all_age_both_sex: bool = _get_all_age_both_sex(params)
        self.num_ages: int = len(params[parameters.PREDICTION_AGE_GROUP_IDS])
        self.num_sexes: int = len(params[parameters.PREDICTION_SEX_IDS])
        self.measure: str = _get_measure(params, session)
        self.transform: str = params[parameters.DATA_TRANSFORM]
        self.density_cutoffs: bool = \
            len(params[parameters.DENSITY_CUTOFFS]) > 0
        self.custom_age_vector: bool = \
            len(params[parameters.ST_CUSTOM_AGE_VECTOR]) > 0
        self.aggregates: List[str] = _get_aggregates(params)
        self.percent_data_close_to_0: float = \
            _get_percent_data_close(run_id, 0)
        self.percent_data_close_to_1: float = \
            _get_percent_data_close(run_id, 1)
        self.random_effects: bool = \
            params[parameters.STAGE_1_MODEL_FORMULA] is None or \
            '|' in params[parameters.STAGE_1_MODEL_FORMULA]
        self.prod_stgpr: bool = params[parameters.ST_VERSION] == enums.Version.PROD.value
        self.nsv: bool = params[parameters.ADD_NSV] == 1
        self.st_lambda: float = params[parameters.ST_LAMBDA]
        self.st_omega: float = params[parameters.ST_OMEGA]
        self.st_zeta: float = params[parameters.ST_ZETA]
        self.gpr_scale: float = params[parameters.GPR_SCALE]
        self.amp_method: str = params[parameters.GPR_AMP_METHOD]
        self.amp_cutoff: int = params[parameters.GPR_AMP_CUTOFF]


def get_best_run(
        modelable_entity_id: int,
        decomp_step_id: int,
        session: orm.Session
) -> int:
    run_id = stgpr_helpers_internal.get_best_model(
        modelable_entity_id, decomp_step_id, session
    )
    if not run_id:
        raise ValueError(
            f'No best run for ME {modelable_entity_id} for decomp step ID '
            f'{decomp_step_id}'
        )
    return run_id


def _get_num_datapoints(run_id: int) -> int:
    model_root = paths.OUTPUT_ROOT_FORMAT.format(run_id=run_id)
    data_filepath = f'{model_root}/data.h5'
    data = pd.read_hdf(data_filepath, 'data')
    return len(data)


def _get_measure(
        params: Dict[str, Any],
        session: orm.Session
) -> Optional[str]:
    crosswalk_version_id = params[parameters.CROSSWALK_VERSION_ID]
    if crosswalk_version_id:
        measure_id = session.execute(
            'SELECT measure_id '
            f'FROM crosswalk_version.crosswalk_{crosswalk_version_id}_row '
            'LIMIT 1'
        ).scalar()
        measure_map = {
            measure_id: measure.lower()
            for measure, measure_id in gbd.constants.measures.items()
        }
        return measure_map[measure_id]
    else:
        return None


def _get_all_age_both_sex(params: Dict[str, Any]) -> bool:
    return params[parameters.PREDICTION_AGE_GROUP_IDS] == [22] and \
        params[parameters.PREDICTION_SEX_IDS] == [3]


def _get_aggregates(params: Dict[str, Any]) -> List[str]:
    aggregates = []
    if params[parameters.LEVEL_4_TO_3_AGGREGATE]:
        aggregates.append('level_4_to_3')
    if params[parameters.LEVEL_5_TO_4_AGGREGATE]:
        aggregates.append('level_5_to_4')
    if params[parameters.LEVEL_6_TO_5_AGGREGATE]:
        aggregates.append('level_6_to_5')
    return aggregates


def _get_percent_data_close(run_id: int, val: int) -> float:
    model_root = paths.OUTPUT_ROOT_FORMAT.format(run_id=run_id)
    data_filepath = f'{model_root}/{paths.DATA_H5}'
    data = pd.read_hdf(data_filepath, paths.PREPPED_OBJ)
    data = data[~data.original_data.isna()]
    num_points_close = len(
        data[np.isclose(data.original_data, val, atol=_THRESHOLD)]
    )
    return 100.0 * num_points_close / len(data)

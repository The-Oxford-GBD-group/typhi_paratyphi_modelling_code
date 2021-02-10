import sys

import pandas as pd
from sqlalchemy import orm

from db_tools import ezfuncs

from stgpr_helpers.api import internal as stgpr_helpers_internal
from stgpr_helpers.api.constants import columns, conn_defs


def _upload_stage_1_estimates(stgpr_version_id: int, session: orm.Session) -> None:
    """Uploads stage 1 estimates to the database.

    Custom stage 1 estimates were uploaded during prep, so those are skipped here.
    """
    output_path = stgpr_helpers_internal.get_output_path(stgpr_version_id, session)
    file_utility = stgpr_helpers_internal.StgprFileUtility(output_path)

    stage_1_statistics_df = file_utility.read_stage_1_statistics().pipe(
        _format_statistics
    )
    stgpr_helpers_internal.load_stage_1_statistics(
        stgpr_version_id, stage_1_statistics_df, session
    )

    stage_1_estimates_df = file_utility.read_stage_1_estimates().rename(
        columns={"stage1": columns.VAL}
    )
    stgpr_helpers_internal.load_stage_1_estimates(
        stgpr_version_id, stage_1_estimates_df, session
    )


def _format_statistics(stats_df: pd.DataFrame) -> pd.DataFrame:
    """Transforms stage 1 statistics into the format the database expects.
    
    Uses regexes to extract covariate name and factor from the "covariate" column.
    """
    return (
        stats_df.assign(
            **{
                columns.FACTOR: lambda df: df[columns.COVARIATE].str.extract(
                    r"as\.factor\(.*\)(.*)", expand=False
                ),
                columns.COVARIATE: lambda df: df[columns.COVARIATE]
                .str.extract(r"as\.factor\((.*)\).*", expand=False)
                .fillna(stats_df[columns.COVARIATE]),
            }
        )
        .rename(
            columns={
                "betas": columns.BETA,
                "se": columns.STANDARD_ERROR,
                "zval": columns.Z_VALUE,
                "pval": columns.P_VALUE,
            }
        )
        .drop(columns=["model"])
    )


if __name__ == "__main__":
    stgpr_version_id = int(sys.argv[1])
    with ezfuncs.session_scope(conn_defs.STGPR) as scoped_session:
        _upload_stage_1_estimates(stgpr_version_id, scoped_session)

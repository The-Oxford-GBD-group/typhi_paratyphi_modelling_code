# This must be the first code to execute, otherwise matplotlib will try to use
# an interactive backend.
import matplotlib
matplotlib.use('Agg')

import os

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

import gbd

from stgpr.common.constants import columns, paths as stgpr_paths
from stgpr.vetting.util import path_utils as vetting_path_utils
from stgpr.vetting.util.constants import paths as vetting_paths


def plot_scatters(suite_run: str, modelable_entity_id: int) -> None:
    """
    Plots run using updated code with original parameters vs run using
    original code with original parameters.
    """
    # Set plotting styles.
    matplotlib.rcParams['text.color'] = 'black'
    sns.set_style('whitegrid')

    # Prepare data.
    original_raked_df = _read_output(suite_run, modelable_entity_id, True)
    new_raked_df = _read_output(suite_run, modelable_entity_id, False)
    combined_df = original_raked_df\
        .merge(
            new_raked_df,
            on=columns.DEMOGRAPHICS,
            suffixes=('_original', '_new'))\
        .assign(sex=lambda df: df[columns.SEX_ID].replace({
            gbd.constants.sex.MALE: 'Male',
            gbd.constants.sex.FEMALE: 'Female',
            gbd.constants.sex.BOTH: 'Both'}))

    # Plot.
    _plot_scatters(suite_run, modelable_entity_id, combined_df)


def _read_output(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> pd.DataFrame:
    output_root = vetting_path_utils.get_model_output_root(
        suite_run, modelable_entity_id, is_original
    )
    output_path = os.path.join(output_root, stgpr_paths.OUTPUT_H5)
    return pd.read_hdf(output_path, stgpr_paths.RAKED_OBJ)


def _plot_scatters(
        suite_run: str,
        modelable_entity_id: int,
        df: pd.DataFrame
) -> None:
    grid = sns\
        .relplot(
            data=df,
            x=f'{columns.GPR_MEAN}_original',
            y=f'{columns.GPR_MEAN}_new',
            col=columns.SEX,
            color='brown',
            s=10)\
        .set_xlabels('Original')\
        .set_ylabels('Updated')

    for ax in grid.axes.flat:
        # Add y=x line to each plot.
        limits = [
            [min(ax.get_xlim()), min(ax.get_ylim())],
            [max(ax.get_xlim()), max(ax.get_ylim())]
        ]
        ax.plot(limits, limits, 'k-', linewidth=0.75)

        # Make x- and y-axis labels and title larger.
        ax.set_xlabel(ax.get_xlabel(), fontsize='x-large')
        ax.set_ylabel(ax.get_ylabel(), fontsize='x-large')
        ax.set_title(ax.get_title(), fontsize='x-large')

    # Add title.
    plt.subplots_adjust(top=0.85)
    grid.fig.suptitle(
        f'{modelable_entity_id} Scatter, Updated vs Original',
        fontsize='xx-large',
        fontweight='bold'
    )

    # Save it as a PDF.
    pdfs_root = vetting_path_utils.get_me_pdfs_root(
        suite_run, modelable_entity_id
    )
    os.makedirs(pdfs_root, exist_ok=True)
    scatters_path = os.path.join(pdfs_root, vetting_paths.SCATTERS)
    grid.savefig(scatters_path, facecolor='white')

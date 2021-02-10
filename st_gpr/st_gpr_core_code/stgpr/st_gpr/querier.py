###########################################################
# Author: Hayley Tymeson
### Date: 4/5/2018
### Project: ST-GPR
# Purpose: Database utility functions *temporary*
###########################################################

#############################################################################################
#                                   Table of Contents
#############################################################################################

# Base
# query

# Pulls

# get_ages


################################################################################################
#                                          Query
################################################################################################

import numpy as np
import pandas as pd

from db_tools.ezfuncs import query
from gbd.decomp_step import decomp_step_id_from_decomp_step

from stgpr.model.config import *

################################################################################################
#                                           Pulls
################################################################################################


def get_ages(ages):
    """Expects list of ages, outputs dataframe of age_group_id, age_group_name,
    age_start, age_end (in years) and the midpoint of the age group in years, with
    some modifications for edge age_group_ids (1-4yrs, 80+ yrs and 95+ yrs)"""

    host = "cod"
    if(len(ages) > 1):
        q = ("SELECT age_group_id, age_group_name, age_group_years_start, age_group_years_end "
             "FROM shared.age_group WHERE age_group_id IN {ages}").format(ages=tuple(ages))
    else:
        q = ("SELECT age_group_id, age_group_name, age_group_years_start, age_group_years_end "
             "FROM shared.age_group WHERE age_group_id ={ages}").format(ages=ages[0])

    out = query(q, conn_def=host)
    out = out.rename(
        columns={'age_group_years_start': 'age_start', 'age_group_years_end': 'age_end'})

    return(out)

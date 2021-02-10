from setuptools import find_packages, setup

import versioneer

setup(
    version=versioneer.get_version(),
    cmdclass=versioneer.get_cmdclass(),
    name='stgpr',
    description='Space-Time Gaussian Process Regression',
    url='https://stash.ihme.washington.edu/projects/CC/repos/st_gpr',
    author='Dylan Ferrer',
    author_email='dferrer@uw.edu',
    install_requires=[
        'db_queries>=21.0.0',
        'db_tools>=0.10.0',
        'gbd>=4.0.2',
        'ihme-rules>=3.0.0',
        'jobmon>=1.1.4',
        'mysqlclient',
        'numpy',
        'pandas',
        'pymc>=2.3.7',
        'sklearn',
        'stgpr_helpers',
        'tables',
        'xarray'
    ],
    python_requires='~=3.7',
    packages=find_packages(),
    include_package_data=True,
    entry_points={'console_scripts': []}
)

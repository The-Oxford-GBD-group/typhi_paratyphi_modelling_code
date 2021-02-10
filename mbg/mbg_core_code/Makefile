# targets for Jenkinsfile
# use bash and not sh (default) - https://stackoverflow.com/a/589300
SHELL := /bin/bash

# Get latest test_deploy image
newest_image := $(shell ls -t /share/singularity-images/lbd/testing_INLA_builds/lbd*.simg | head -n1)

# Define the CLUSTER variable (mocking prod-el7)
CLUSTER:= "prod-el7"

## Core Jenkinsfile/CI targets

# Unifying recipe
ci-all:	ci-verify-env ci-run-tests

# Verify environment
.PHONY: ci-verify-env
ci-verify-env:
	SINGULARITYENV_SGE_ENV=$(CLUSTER) /share/singularity-images/lbd/shells/singR.sh -s $(newest_image) -e s "testing/verifieR.R"

# Run R tests
.PHONY: ci-run-tests
ci-run-tests:
	SINGULARITYENV_SGE_ENV=$(CLUSTER) /share/singularity-images/lbd/shells/singR.sh -s $(newest_image) -e s "testing/run_tests.R"

# Create documentation
.PHONY: ci-make-docs
ci-make-docs:
	if [[ "$(GIT_BRANCH)" =~ merge ]]; then \
        	echo 'Making docs'; \
		SINGULARITYENV_SGE_ENV=$(CLUSTER) /share/singularity-images/lbd/shells/singR.sh -s $(newest_image) -e s "mbg_central/LBDCore/preample_setup/CI_build_and_deploy_package_docs.R"; \
	else \
		echo 'Not a merge; skipping ci-make-docs'; \
    	fi

## HELPER targets
# Mike's one-off targets
.PHONY: test
test:
	testing/run-tests

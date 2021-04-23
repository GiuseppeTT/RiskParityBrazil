.PHONY: _install_package_remotes
_install_package_remotes:
	Rscript -e 'if (! "remotes" %in% rownames(installed.packages())) install.packages("remotes")'

.PHONY: install_dependencies
install_dependencies: _install_package_remotes
	Rscript -e 'remotes::install_deps(dependencies = TRUE)'

.PHONY: install_deploy_dependencies
install_deploy_dependencies: _install_package_remotes
	Rscript -e 'remotes::install_deps(dependencies = NA)'

.PHONY: document
document:
	Rscript -e 'devtools::document()'

.PHONY: check
check:
	Rscript -e 'devtools::check()'

.PHONY: lint
lint:
	Rscript -e 'lintr::lint_package()'

.PHONY: install
install:
	Rscript -e 'devtools::install()'

.PHONY: run
run:
	Rscript -e 'targets::tar_make()' 2>&1 | tee log/run.log

.PHONY: all
all: document check lint install run

.PHONY: clean
clean:
	Rscript -e 'targets::tar_destroy()'

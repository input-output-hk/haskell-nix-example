.DEFAULT_GOAL := help

.PHONY: help
help: ## Display this help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} \
		/^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2 } \
		/^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } \
		' $(MAKEFILE_LIST)

##@ Dependency Management

check-deps: ## Check current versions of dependencies against latest releases (without updating)
	@./scripts/manage-deps.sh check

check-and-update-deps: ## Check and update cardano-node and cardano-cli to latest releases
	@./scripts/manage-deps.sh update
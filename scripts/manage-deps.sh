#!/usr/bin/env bash
set -euo pipefail

# Common functions
get_current_ref() {
    local component=$1
    grep -A1 "${component}.url.*github:IntersectMBO/${component}" flake.nix | grep 'ref=' | sed -n 's/.*ref=\([^"]*\).*/\1/p'
}

get_latest_release() {
    local component=$1
    curl -s "https://api.github.com/repos/IntersectMBO/${component}/releases/latest" | jq -r .tag_name
}

# Function to check current versions
check_deps() {
    echo "Checking current versions against latest releases..."

    # Check cardano-node
    CURRENT_NODE_REF=$(get_current_ref "cardano-node")
    LATEST_NODE_REF=$(get_latest_release "cardano-node")
    echo "cardano-node:"
    echo "  Current: $CURRENT_NODE_REF"
    echo "  Latest:  $LATEST_NODE_REF"

    # Check cardano-cli
    CURRENT_CLI_REF=$(get_current_ref "cardano-cli")
    LATEST_CLI_REF=$(get_latest_release "cardano-cli")
    echo "cardano-cli:"
    echo "  Current: $CURRENT_CLI_REF"
    echo "  Latest:  $LATEST_CLI_REF"
}

# Function to update dependencies
update_deps() {
    echo "Checking for new releases..."

    # Track if any updates were made
    UPDATES_NEEDED=0

    # Loop through components
    for component in cardano-node cardano-cli; do
        CURRENT_REF=$(get_current_ref "$component")
        LATEST_REF=$(get_latest_release "$component")
        if [ "$CURRENT_REF" != "$LATEST_REF" ]; then
            echo "Updating $component from $CURRENT_REF to $LATEST_REF"
            sed -i.bak "s|\($component?ref=\)[^\"]*|\1$LATEST_REF|" flake.nix
            UPDATES_NEEDED=1
        else
            echo "$component is up to date at $CURRENT_REF"
        fi
    done

    # Run nix flake update if any updates were made
    if [ $UPDATES_NEEDED -eq 1 ]; then
        nix flake update cardano-node cardano-cli
    fi

    # Clean up backup files
    rm -f flake.nix.bak
}

# Main command router
case "${1:-}" in
    "check")
        check_deps
        ;;
    "update")
        update_deps
        ;;
    *)
        echo "Usage: $0 {check|update}"
        exit 1
        ;;
esac
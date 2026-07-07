#!/bin/bash

# Ensure required arguments are provided
if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <registry_url> [username] [password]"
    echo "Example: $0 https://example.com admin secret123"
    exit 1
fi

# Parse arguments
REGISTRY_URL="${1%/}" # Strips trailing slash if present
USERNAME="$2"
PASSWORD="$3"

# Setup authentication flags for curl
AUTH_FLAGS="-s"
if [ -n "$USERNAME" ] && [ -n "$PASSWORD" ]; then
    AUTH_FLAGS="-s -u ${USERNAME}:${PASSWORD}"
fi

echo "Connecting to $REGISTRY_URL..."

# Fetch catalog
CATALOG=$(curl $AUTH_FLAGS "${REGISTRY_URL}/v2/_catalog")

# Check if the catalog request failed
if [ $? -ne 0 ] || echo "$CATALOG" | jq -e '.errors' > /dev/null 2>&1; then
    echo "Error: Failed to fetch catalog. Check your URL, credentials, or network."
    echo "$CATALOG" | jq '.errors[]?.message' 2>/dev/null || echo "$CATALOG"
    exit 1
fi

REPOS=$(echo "$CATALOG" | jq -r '.repositories[]')

for REPO in $REPOS; do
    echo "--------------------------------------"
    echo "Repository: $REPO"

    # Fetch all tags for this repository
    TAG_LIST=$(curl $AUTH_FLAGS "${REGISTRY_URL}/v2/${REPO}/tags/list")
    TAGS=$(echo "$TAG_LIST" | jq -r '.tags[]?' 2>/dev/null)

    if [ -z "$TAGS" ]; then
        echo "  No tags found."
        continue
    fi

    for TAG in $TAGS; do
        # Fetch manifest with the required Header for V2 Schema 2 (to get layer sizes)
        MANIFEST=$(curl $AUTH_FLAGS -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
            "${REGISTRY_URL}/v2/${REPO}/manifests/${TAG}")

        # Verify the manifest structure and sum the layer sizes
        if echo "$MANIFEST" | jq -e '.layers' > /dev/null 2>&1; then
            SIZE_BYTES=$(echo "$MANIFEST" | jq '[.layers[].size] | add')
            SIZE_HUMAN=$(numfmt --to=iec-i --suffix=B "$SIZE_BYTES" 2>/dev/null || echo "$SIZE_BYTES bytes")
            echo "  Tag: $TAG -> $SIZE_HUMAN"
        else
            echo "  Tag: $TAG -> Unknown size (Schema v1 or multi-arch index)"
        fi
    done
done

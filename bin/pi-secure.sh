#!/bin/bash

# Configuration
LITELLM_ENDPOINT="http://host.docker.internal:4000" # Adjust to your LiteLLM address
ALLOWED_URLS="github.com,npmjs.org,pypi.org,openai.com" # Your Allow-list

# 1. Capture the current directory to mount as the 'workspace'
CURRENT_DIR=$(pwd)

# 2. Run the Docker container
# --add-host lets the container talk to your local LiteLLM
# -v mounts ONLY the current folder so it can't see your whole Mac
# --network-alias + proxy logic (simplified for single-run)
#  -v "/Users/tsureshkumar/.pi:/home/pi/.pi" \
docker run -it --rm \
  --name "pi-agent-$(date +%s)" \
  --add-host=host.docker.internal:host-gateway \
  -v "$CURRENT_DIR:/home/pi/workspace:ro" \
  -v "$CURRENT_DIR/agent_workspace:/home/pi/workspace/agent_workspace:rw" \
  -v "/Users/tsureshkumar/.pi/agents/models.json.template:/home/pi/.pi/agents/models.json.template" \
  -w /home/pi/workspace \
  -e LITELLM_URL="$LITELLM_ENDPOINT" \
  -e HTTP_PROXY=http://host.docker.internal:3128 \
  -e HTTPS_PROXY=http://host.docker.internal:3128 \
  -e ALL_PROXY=http://host.docker.internal:3128 \
  -e NO_PROXY=localhost,127.0.0.1,squid-proxy,litellm-proxy \
  -e LITELLM_API_KEY="${LITELLM_API_KEY}" \
  pi-agent-loaded:latest \
  pi "$@"


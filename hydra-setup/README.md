# Installation

Make sure you have the following installed:

- Docker / Docker Compose

**Optional:**

- `cardano-cli`
- `websocat`
- `jq`

# Containers

The `docker-compose` file runs 3 services:

- A cluster of two Hydra nodes, directly connected to one another and configured with Hydra credentials: **Alice** and **Bob**
- A single `cardano-node` producing blocks, used as a (very fast) local devnet

# Running the Devnet

In a terminal, run:

```bash
$> docker compose up
```

This will start the 3 services.

The `cardano-node` has a synced database as of **24/6/2025**. Most of the time when starting the services, there are a few minutes of wait time for the `cardano-node` to fully sync with the Preprod testnet.

You can check the sync status with:

```bash
$> cardano-cli query tip --testnet-magic 1 --socket-path=$PATH_TO_SOCKET
```

> **Note:** We established `PATH_TO_SOCKET` to be `cardano-node/node.socket` in this directory.

For a smoother Hydra Head flow, we recommend waiting for the sync level to reach **100%**.

# Watching the Peers

Each Hydra node exposes:

- An HTTP API for requests
- A WebSocket for sending and listening to events

Their URLs are:

**Alice:**

- WS → `ws://127.0.0.1:4001`
- API → `http://127.0.0.1:4001`

**Bob:**

- WS → `ws://127.0.0.1:4002`
- API → `http://127.0.0.1:4002`

> **Note:** the API urls here are the ones you'll need to use when opening heads through the Blazar Hydra API.

You can observe events at either Hydra Head using:

```bash
$> websocat -B 2000000 "ws://127.0.0.1:$PORT/?history=yes" | jq
```

# Hydra Pay Load Balancer

A load balancing server for the [Blazar Hydra](https://github.com/blazarlabs-io/blazar-hydra) payment system. This server acts as a single entry point that distributes user requests across multiple Blazar Hydra service instances, enabling horizontal scaling of the payment infrastructure.

## Overview

The Hydra Pay Load Balancer is built on top of the [Blazar Hydra](https://github.com/blazarlabs-io/blazar-hydra) protocol, which enables fast and cheap Cardano payments through Hydra heads. As described in the [Blazar Hydra requirements and design document](https://github.com/blazarlabs-io/blazar-hydra/blob/main/doc/requirements_and_design.md), the system allows users to deposit funds from L1 (Cardano mainnet) into a Hydra head, make payments to merchants, and withdraw funds back to L1.

This load balancer extends the Blazar Hydra architecture by:

- **Service Discovery**: Automatically discovers available Blazar Hydra service instances by scanning configured port ranges
- **Request Distribution**: Routes user requests to appropriate service instances based on operation type
- **Result Aggregation**: Combines results from multiple services when querying user funds across the system
- **Fault Tolerance**: Handles service failures gracefully, ensuring system availability even when some services are down

## Architecture

![Hydra Pay Load Balancer Architecture](out/hydrapay-architecture/hydrapay-architecture.svg)

The load balancer sits between client applications (mobile apps, web interfaces) and multiple Blazar Hydra service instances. Each service instance manages its own Hydra head and connects to a shared Cardano node.

For more details on the underlying Blazar Hydra architecture and protocol design, see the [requirements and design document](https://github.com/blazarlabs-io/blazar-hydra/blob/main/doc/requirements_and_design.md).

## Features

### Service Discovery

The server automatically discovers available Blazar Hydra services by:

1. **Initial Scan**: On startup, scans ports 3000-3019 (configurable) for available services
2. **Periodic Updates**: Refreshes the service list every 30 seconds to detect new services or handle failures
3. **Health Checks**: Verifies service availability by making HTTP requests to each discovered service

Services are stored in thread-safe memory and updated automatically without requiring server restarts.

### API Endpoints

The load balancer exposes a subset of the Blazar Hydra API focused on user-facing operations:

#### `GET /query-funds?address=<address>`

Queries funds for a given address across **all available services** and aggregates the results.

**Behavior:**
- Sends the query to all discovered services in parallel
- Aggregates L2 (Hydra head) funds from all services:
  - Combines all `fundsInL2` UTxO references
  - Sums numeric values in `totalInL2` JSON objects (e.g., `{"lovelace": 1000}` + `{"lovelace": 500}` = `{"lovelace": 1500}`)
- Keeps L1 (Cardano mainnet) values from the first successful response
- Returns empty results if no services are available or all queries fail

**Use Case**: Users can query their total funds across all Hydra heads managed by different service instances.

#### `POST /deposit`

Forwards deposit requests to the **first available service**.

**Behavior:**
- Routes to the first service in the discovered list
- Returns the transaction CBOR for the user to sign and submit
- Returns 503 if no services are available
- Returns 502 with error details if the service fails

**Use Case**: Users deposit funds into a Hydra head. The load balancer ensures requests are handled even if some services are unavailable.

#### `POST /withdraw`

Forwards withdraw requests to the **first available service**.

**Behavior:**
- Routes to the first service in the discovered list
- Returns the transaction CBOR for the user to sign and submit
- Returns 503 if no services are available
- Returns 502 with error details if the service fails

**Use Case**: Users withdraw funds from a Hydra head back to L1.

#### `POST /pay-merchant`

Forwards payment requests to **all available services** and returns the first successful response.

**Behavior:**
- Sends the payment request to all discovered services in parallel
- Returns the first successful `TxBuiltResponse`
- If all services fail, returns 502 with aggregated error messages from all services

**Why "Try All, Return First Success"?**

The `pay-merchant` request includes the output reference (`fundsUtxoRef`) of the user's funds UTxO within the Hydra Head. This UTxO reference is **guaranteed to be unique** and exists in exactly one Hydra Head managed by one service instance.

When the load balancer forwards the request to all services:
- **One service** will successfully process the payment because it manages the Hydra Head containing that specific UTxO
- **Other services** will fail (typically with an error indicating the UTxO doesn't exist in their head)
- The load balancer returns the first successful response, which is guaranteed to be from the correct service

This approach eliminates the need for the load balancer to know which service manages which Hydra Head. Instead, it relies on the uniqueness of UTxO references to automatically route to the correct service.

**Use Case**: Users can make payments without knowing which Hydra Head their funds are in. The load balancer automatically finds the correct service by trying all services and returning the one that successfully processes the payment.

## Load Balancing Logic

The load balancer implements different routing strategies based on operation type:

| Operation | Strategy | Rationale |
|-----------|----------|-----------|
| `query-funds` | **Aggregate All** | Users need to see their total funds across all Hydra heads |
| `deposit` | **First Available** | Deposits are typically routed to a single head; first service handles it |
| `withdraw` | **First Available** | Withdrawals are specific to a head; first service handles it |
| `pay-merchant` | **Try All, Return First Success** | UTxO references are unique per Hydra Head; only one service will have the UTxO and succeed |

This design balances between:
- **Availability**: Operations succeed even when some services are down
- **Consistency**: Users can query their complete balance across the system
- **Performance**: Parallel requests minimize latency

## Building and Running

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) 9.6.6 or compatible
- [Cabal](https://www.haskell.org/cabal/) 3.0 or later
- Access to Blazar Hydra service instances running on ports 3000-3019 (or configured range)

### Build

```bash
cabal build server
```

### Run

```bash
cabal run server
```

The server will:
1. Start on port 8080 (default)
2. Scan ports 3000-3019 for available services
3. Begin serving requests
4. Refresh the service list every 30 seconds

### Configuration

Service discovery parameters are currently hardcoded in `app/Server.hs`:

- **Host**: `127.0.0.1` (localhost)
- **Start Port**: `3000`
- **Port Range**: `20` ports (3000-3019)
- **Server Port**: `8080`
- **Update Interval**: `30` seconds

To modify these, edit the `updateServices` and `main` functions in `app/Server.hs`.

## Project Structure

```
hydra-pay-loadbalancer/
├── app/
│   ├── Server.hs          # Main load balancer server
│   ├── Main.hs            # TUI application (separate executable)
│   └── TUI.hs             # Terminal UI for admin operations
├── src/
│   ├── HydraPay/
│   │   ├── API.hs         # API type definitions (UserAPI, AdminAPI)
│   │   ├── API/
│   │   │   └── Types.hs   # Request/response types
│   │   ├── Client.hs      # HTTP client for Blazar Hydra services
│   │   ├── ServiceScanner.hs  # Service discovery implementation
│   │   └── Operations.hs  # Business logic operations
│   └── ...
├── out/
│   └── hydrapay-architecture/
│       └── hydrapay-architecture.svg  # Architecture diagram
└── README.md
```

## Related Documentation

- [Blazar Hydra Requirements and Design](https://github.com/blazarlabs-io/blazar-hydra/blob/main/doc/requirements_and_design.md) - Complete protocol specification
- [Hydra Head Protocol](https://hydra.family/head-protocol/) - Underlying Hydra protocol documentation
- [Hydra Benchmarks](https://hydra.family/head-protocol/benchmarks/transaction-cost/) - Performance characteristics and limits

## Implementation Details

### Service Discovery

The service discovery mechanism (`HydraPay.ServiceScanner`) works by:

1. Creating a list of potential service endpoints (host + port combinations)
2. Making HTTP GET requests to `/state?id=test-availability-check` on each port
3. Treating any HTTP response (even 400/404) as "service available"
4. Filtering out connection errors/timeouts

This approach is lightweight and doesn't require service registration or coordination.

### Result Aggregation

For `query-funds`, the server aggregates JSON objects by:

- **Numeric Values**: Adding values with matching keys (e.g., `{"lovelace": 100}` + `{"lovelace": 50}` = `{"lovelace": 150}`)
- **Nested Objects**: Recursively aggregating nested JSON structures
- **UTxO Lists**: Concatenating all UTxO references from all services

Only L2 (Hydra head) values are aggregated; L1 (Cardano mainnet) values are taken from the first successful response, as they represent the same on-chain state.

### Error Handling

The load balancer implements graceful error handling:

- **Service Unavailable**: Returns empty results or 503 errors, but continues operating
- **Partial Failures**: For `query-funds`, ignores failed services and aggregates successful ones
- **Complete Failures**: For `pay-merchant`, aggregates all error messages for debugging
- **Error Propagation**: Forwards detailed error messages from services to clients

## Limitations

- **Single Head Limitation**: The current Blazar Hydra design opens one head per day. The load balancer helps distribute load but doesn't change this fundamental limitation.
- **Service Discovery**: Currently scans a fixed port range. Dynamic service registration would improve scalability.
- **No Load Metrics**: Doesn't consider service load or response times when routing requests (future enhancement).

## Future Enhancements

- **Health-based Routing**: Route requests based on service health metrics and response times
- **Service Registration**: Allow services to register themselves instead of port scanning
- **Admin API Support**: Expose admin endpoints (open-head, close-head) through the load balancer
- **Metrics and Monitoring**: Add Prometheus metrics and health check endpoints
- **Configuration File**: Support configuration via file or environment variables

## License

Apache-2.0 (see LICENSE file)

## Contributing

This project extends the Blazar Hydra ecosystem. For contributions, please follow the same guidelines as the [Blazar Hydra project](https://github.com/blazarlabs-io/blazar-hydra).


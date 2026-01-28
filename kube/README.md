# Kube Package

Internal package containing the Kube data structure and builder implementation.

## Public API

Only the following module should be imported by external code:
- **Kube** - Exports `add`, `Kube`, `Kid`, `Bi`, `emptyKube`, `bi0` (re-exported from Types)

## Internal Modules

- **Builder** - Internal implementation, do not import directly
- **Types** - Core types (re-exported through Kube)

When this package is published, only the `Kube` module will be documented as the public API.

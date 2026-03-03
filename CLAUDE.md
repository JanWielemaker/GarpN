# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

GarpN is a SWI-Prolog system that takes qualitative reasoning models (from GARP 3 / DynaLearn) and helps users construct and run numerical (ODE-based) simulations. It proposes equations from qualitative relations, simulates them numerically, and maps results back to GARP qualitative states for validation. A web UI (HTMX + MathLive + Plotly) is served directly from SWI-Prolog.

## Running the Server

```bash
swipl run.pl -i -p 8080
```

Navigate to `http://localhost:8080/garp/home`. The `-i` flag keeps an interactive Prolog top-level available.

## Running Tests

From the Prolog top-level:

```prolog
?- run_garp_tests.
?- run_garp_test(File, Options).   % single test
```

Test data lives in `tests/`. There is no separate build step.

## Architecture

### Entry Point

`run.pl` sets the `dynalearn` flag and loads: `gui`, `map`, either `dynalearn` or the Garp3 startup, and `test`.

### Core Modules

| File | Responsibility |
|------|----------------|
| `gui.pl` | HTTP handlers, HTML generation (SWI html_write DCG), HTMX endpoints |
| `map.pl` | Maps numeric simulation results to GARP qualitative states; correspondence validation; state ordering |
| `model.pl` | `propose_model/3` — derives numeric equations from qualitative relations |
| `gsim.pl` | Numerical integrator (`simulate/3`); supports Euler and RK4; formula evaluator |
| `equations.pl` | LaTeX ↔ Prolog conversion; MathLive editor support |
| `patterns.pl` | Cycle detection (`final_cycle/3`) and cycle alignment |
| `dynalearn.pl` | Fetches models from the DynaLearn API |
| `test.pl` | Test harness; stores and replays test cases |
| `identifiers.pl` | Translates GARP/DynaLearn IDs to Prolog terms |
| `csv_util.pl` | Series formatting, CSV export |

### Key Data Structures

- **Quantity values:** `d(Value, D1, D2, D3)` — value plus up to three derivatives.
- **States:** SWI-Prolog dicts `#{quantity_id: d(...), ...}`, one per time step.
- **Equations:** Prolog terms `Quantity := Expression`; stored and rendered as LaTeX via MathLive.
- **Cycles:** Represented as lists; `patterns.pl` detects and normalizes them.

### Data Flow

```
DynaLearn API / Garp .db file
  → dynalearn.pl / garp3 integration
  → model.pl: propose_model/3      (qualitative → numeric equations)
  → equations.pl: latex rendering  (Prolog ↔ LaTeX)
  → gsim.pl: simulate/3            (numeric integration)
  → map.pl: nq_series/3            (results → qualitative states)
  → patterns.pl: final_cycle/3     (cycle detection)
  → gui.pl: Plotly + HTMX response (visualization)
```

### Web Stack

Frontend is served by SWI-Prolog's http library. No separate Node server. `package.json` exists only to vendor JS libraries (htmx, mathlive) into `node_modules/`; they are served as static files under `/garp/node_modules/`.

- `web/simulator.js` — Plotly integration, quantity space UI
- `web/equations.js` — MathLive editor wiring
- `web/simulator.css` — Styling

## External Dependency

When `dynalearn` flag is `true`, models are fetched from `https://api.dynalearn.nl/garpN/`. When `false`, Garp3 `.db` files in `garp/` are used via a linked Garp3 installation at `../Garp3-v1.5.2/`.

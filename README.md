**DISCLAIMER: THIS IS WORK IN PROGRESS. THERE ARE UNFINISHED PARTS, GAPS AND PROBABLY ERRORS!**

# KID for PRIIPs

In the PRIIPs (Packaged Retail and Insurance-based Investment Products) regulation of the European Union (No. 1286/2014) the KID (Key Information Document) is defined as a measure to improve investor protection. The document specified in the regulation consists of at most three pages presenting the details of a financial product, a risk indicator, different scenarios for the behavior of the product over time, as well a disclosure of the costs inherent to the product. In the [annexes](https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32017R0653) the exact form of the document and the calculation methodologies are specified.

# Architecture

## Calculations / Simulations

We implement the calculations/simulations for Category 2 and 3 KIDs. These are based on the following methodologies, resp. algorithms:

* Cornish Fisher expansion
* Monte-Carlo simulations

The GPU-based Monte-Carlo simulation framework is implemented in the programming language [Futhark](http://futhark-lang.org)

The implementation is done the repository [kid-annexes](https://github.com/monoid-gmbh/kid-annexes).

### Payoff Language

A generic, certified payoff language has been proposed in the following paper:

Danil Annenkov and Martin Elsman. Certified Compilation of Financial Contracts. In Proceedings of the 20th International Symposium on Principles and Practice of Declarative Programming (PPDP’18). Frankfurt am Main, Germany. September 2018. [PDF](http://hiperfit.dk/pdf/annenkov-elsman-ppdp18.pdf)

The proposed language allows to generate payoff functions in [Futhark](http://futhark-lang.org) that run on the GPU-based simulation engine for the KID calculations.

### Historical data

Historical data is loaded from [Quandl](http://quandl.com). In order to use the Quandl API without restrictions, an API key has to be provided.

## Document generation

The documents are generated using LaTeX that are then compiled to a PDF document.

## Building the project

### Compile

#### Futhark

The dependency to [kid-annexes](https://github.com/monoid-gmbh/kid-annexes) needs to be established as follows:

```
$ cd src/fut
$ make pkgs
$ make
```

#### Haskell

```
$ cabal build
```

### Run the application server

The application server is started as follows. Optionally provide the Quandl API key on the command-line:

```
$ cabal run kid-exe -- -k <quandl-key>
```

Sample request:

```
$ curl -f -X POST --data '@test/example/contract.json' -H 'Accept: application/pdf' -H 'Content-type: application/json' -o kid.pdf http://localhost:8081/kid?lang=EN
```

> **Note:** pass `curl -f` (`--fail`). On an error the server responds with HTTP
> 400 and a plain-text message describing what went wrong (also logged to the
> server's stderr). Without `-f`, `curl -o kid.pdf` would happily save that
> error text into `kid.pdf`, producing what looks like an empty/corrupt PDF.

### Historical-data backend

The source of historical data is pluggable, selected with `--backend`:

```
$ cabal run kid-exe -- --backend quandl -k <quandl-key>   # default
$ cabal run kid-exe -- --backend file
```

* `quandl` — fetches from the Quandl API. Underlyings use `QuandlId` instrument ids.
* `file` — reads from local CSV files. Underlyings use `FileId` instrument ids,
  each naming a CSV file and the value column to read:

  ```json
  "underlying_instrument_id": {
    "tag": "FileId",
    "file_path": "test/example/data/underlying.csv",
    "file_column": "Price"
  }
  ```

  The CSV has a header row whose first column is an ISO date (`YYYY-MM-DD`)
  followed by named value columns; rows outside the historical window are
  ignored:

  ```
  Date,Price
  2013-11-01,100.00
  2013-11-04,100.31
  ```

  A runnable example is provided — `test/example/contract-file.json` together
  with `test/example/data/underlying.csv`:

  ```
  $ cabal run kid-exe -- --backend file
  $ curl -f -X POST --data '@test/example/contract-file.json' -H 'Accept: application/pdf' -H 'Content-type: application/json' -o kid.pdf http://localhost:8081/kid?lang=EN
  ```

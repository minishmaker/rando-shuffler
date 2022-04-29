# Rando Shuffler
This repository contains a partial implementation of the Minishmaker Randomizer setup.
It is incomplete and poorly tested. Unfortunately, I (Berylliosis) am unable to continue working on it,
so the situation isn't likely to improve in the short term.

## Available Features
- Logic lang parsing (good error messages)
- Descriptor lang parsing (poor error messages, missing terminator)
- Relation lang parsing (poor error messages, missing `data`, `relation` branch)
- Query engine (`engine` branch). Untested since rewrite, supports state.
    - Support for keys requires minor modifications

## Missing Features
- Key support in query engine
    - Support needs to be added in `statey.rs` and `database.rs`
    - Minor modification, but requires statey cleanup and refactoring
- Logic/descriptor/relation trait implementations
    - Shouldn't be implemented with existing AST
    - Names/values/logic nodes assumed to be cheap to copy
    - Modification syntax
- Actual shuffling
    - Initial implementation should be guess and check
    - This requires solving key values
- I/O hookup
    - Load single module
    - Load global config
    - Output SPIFFY (probably json containing shuffles and their values)


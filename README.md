# concurtimes

Prints out one or more text files in justified columns.

## Installation

There is a standalone executable in the bin directory.

## Usage
    $ cd /path/to/concurtimes
    $ bin/concurtimes --help

## Options
    $ bin/concurtimes --help
    
## Examples
To see 3 built-in test files printed in a "page" 80 characters
wide with 4 spaces padding between columns:

    $ bin/concurtimes -w80 -s4 -t3

To include arbitrary other files with one built-in:

    $ bin/concurtimes -w80 -s4 -t1 LICENSE src/concurtimes/core.clj
    
### Bugs
No known bugs.

## License

Copyright © 2016 Kenneth Tilton

Distributed under the MIT license.

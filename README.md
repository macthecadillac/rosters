# Setup

`lab-tools` has no external dependency aside from a functioning OS. Simply
download the appropriate executable for your platform from the [release
page](https://github.com/macthecadillac/lab-tools/releases) and run in the
terminal.

# Usage

This program has a "multitool" interface, where the main command is followed by a
subcommand, which takes its own arguments. The general syntax of the tool is
`lab-tools <subcommand> <arguments> [options]`. The subcommands will be described
in the following subsections.

Run `lab-tools --help`, `lab-tools rosters --help`, `lab-tools merge --help` etc.
to see the correct syntax and a full list of available options.

## rosters
    
This subcommand generates random rosters for every section in the lab. Go to
Canvas course > Grades > Actions > Export Entire Gradebook. Save the CSV file
somewhere in your system. Run `lab-tools rosters --lab 1 --input
/path/to/data.csv`, replacing '1' with the lab number of the week and the last
part with the actual path to the CSV file. If your path contains spaces, you
will need to either escape the spaces or put the entire path in between quotes.
This will generate randomized rosters for every section in the current working
directory along with an xlsx file mirroring the pdfs for data entry purposes.
You can specify checkpoints for the lab through the configuration file.

## open-config

`lab-tools open-config` opens the configuration file in the default text editor.

# Configuration

On Windows, the configuration file is located at
`%LOCALAPPDATA%\lab-tools.toml`. On Unix (such as macOS)/Unix-like (such as
Linux) systems with the environmental variable `$XDG_CONFIG_HOME` set, the
configuration file is located at `$XDG_CONFIG_HOME/lab-tools.toml`, otherwise it
is located at `$HOME/.config/lab-tools.toml`, where `$HOME` is your home folder.

The configuration is done via the TOML language. Here is a sample configuration
with all the recognized keys:

```toml
# This is an example configuration to help you get started. This file is
# already written to the correct location, so once you are done editing it,
# simply save and close your text editor. You can always access this file by
# running `lab-tools open-config`.
#
# This file is written in the TOML format. Lines prefixed with the \"#\" sign
# are comments and will be ignored.
#
# This section is mandatory--`lab-tools` won't run without it.
[ta-assignment]
# LHS is the name of the TA. There cannot be spaces within a name.
# RHS is the list of sections that the TA is assigned to. It must be a list of
# integers separated by commas.
Joe = [38, 40]
Donny = [17, 19, 29, 35]
Barry = [3, 21, 23, 33]
Walker = [7, 9, 28, 30]
Billy = [10, 18, 20, 36]
Herbert = [5, 13, 15, 25]
Ronny = [1, 11, 27, 31]
Jimmy = [2, 12, 22, 32]
Jerry = [6, 8, 16, 26]
Ricky = [4, 14, 24, 34]
Lyndon = [37, 39]

# This section is optional. `lab-tools` will run with default values if this is
# missing. This section is only used for roster generation. The values below are
# for 1AL.
[checkpoints]
# LHS must be in the form of "lab" followed by an integer.
# RHS is a list of strings. Entries must be enclosed in single or double quotes
# separted by commas.
lab1 = ['A', 'B5', 'C1', 'C2']
lab2 = ['B5', 'B6', 'Part C7']
lab3 = ['A7b', 'B7', 'B8/B9', 'B12']
lab4 = ['A4', 'B6', 'D1', 'D2']
lab5 = ['A2', 'B2', 'After C5', 'C6']
lab6 = ['A', 'B5', 'C']
lab7 = ['A3', 'B6', 'B13', 'C10']
lab8 = ['A6', 'A12', 'B2', 'B7']
lab9 = ['A3', 'B2', 'B9', 'C4']
```

# Examples

```sh
# open configuration
$ lab-tools open-config

# generate rosters from a Canvas exported CSV file
$ lab-tools rosters --lab 1 --input /path/to/canvas.csv

# generate rosters from a Canvas exported CSV file, specifying output file
$ lab-tools rosters --lab 2 -i /path/to/canvas.csv -o /path/to/output/directory
```

# Building from source
Compiling from source requires the following build tools:

|Tool|Version requirement|
|----|-----|
|Rust| >= 1.64.0|
|OCaml| =4.14.0|
| OPAM| >= 2.0|
|GCC or Clang|Any recent version|

With these tools properly installed, run `opam install .` from the root
directory of the project.

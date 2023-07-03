# Setup

`rosters` has no external dependency aside from a functioning OS. Simply
download the appropriate executable for your platform from the [release
page](https://github.com/macthecadillac/rosters/releases) and execute in a
terminal.

While configuration is entirely optional, you are strongly encouraged to create
one. The program can group sections by TAs if configured to do so. This can
significantly reduce the number of PDFs generated, especially for on-sequence
labs.

# Usage

This program has a "multitool" interface, where the main command is followed by a
subcommand which takes its own arguments. The general syntax is
`rosters <subcommand> <arguments> [options]`. The subcommands will be described
in the following subsections.

Run `rosters --help` and `rosters generate --help` to see the correct syntax and
a full list of available options.

## generate
    
This subcommand generates random rosters for every section in a lab. Go to
Canvas course > Grades > Actions > Export Entire Gradebook and save the CSV file
somewhere on your system. Run `rosters generate --lab 1 --input
/path/to/data.csv`, replacing '1' with the lab number of the week and the last
part with the actual path to the CSV file. If your path contains spaces, you
will need to either escape the spaces or put the entire path in between quotes.
This will generate randomized rosters for every section in the current working
directory along with an xlsx file mirroring the pdfs for data entry purposes. To
skip Excel file generation, pass the `--nox` flag. You can specify checkpoints
for the lab through the configuration file.

## config

`rosters config --output /path/to/file` generates a starter configuration file.

# Configuration

A configuration file can be supplied at run time through the `--config` option,
otherwise the program will look for a configuration file at a standard location
on your platform. If no configuration is found, the program will run with
default parameters.

On Windows, the configuration file is located at `%LOCALAPPDATA%\rosters.toml`.
On macOS, the configuration file is located at `~/Library/Application
Support/rosters.toml`. On Unix (with the exception of macOS)/Unix-like (such as
Linux) systems with the environmental variable `$XDG_CONFIG_HOME` configured,
the configuration file is located at `$XDG_CONFIG_HOME/rosters.toml`, otherwise
it is located at `~/.config/rosters.toml`. TOML files are plain text files and
they can be edited with any text editor (except perhaps for Microsoft Notepad,
which is notorious for creating files with encoding issues).

## Changes from version `w22`

- You can now specify a configuration file at run time instead of relying on a
  file at a default (and hidden) location.

- The configuration file format has not changed. However, the file name (and
  specifically on macOS, the location) has changed. If you wish to reuse the
  configuration from `w22`, you need to rename the file from `rosters.txt` to
  `rosters.toml`. On macOS, you also need to move the file from `~/.config` to
  `~/Library/Application Support`, which is a more standard location for
  application configuration. This location can be accessed from Finder through
  the "Go to Folder" menu item or the key binding ⇧ ⌘ G. You can of course forgo
  this step and place the configuration file anywhere on your system and specify
  its path every time you run the program.

## Sample Configuration

Here is a sample configuration with all the recognized keys:

```toml
# This is an example configuration to help you get started.

# Configuration is entirely optional. `rosters` will run
# with default values if a given section is not found.

# This file is written in the TOML format. Lines prefixed with the "#" sign
# are comments and will be ignored.

[ta-assignment]
# LHS is the name of the TA. There cannot be spaces within a name.
# RHS is the list of sections that the TA is assigned to. It must be a list of
# integers separated by commas. The generator only supports up to 8 sections per
# TA.
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

# Comment out/delete section if you don't want to customize the checkpoint
# columns. These example values are for 1CL.
[checkpoints]
# LHS must be in the form of "lab" followed by an integer.
# RHS is a list of strings. Entries must be enclosed in single or double quotes
# separted by commas. The generator only supports up to 8 checkpoints per lab.
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
#  generate a starter configuration file
$ rosters config --output /path/to/file

# generate rosters from a Canvas exported CSV file
$ rosters generate --lab 1 --input /path/to/canvas.csv

# generate rosters from a Canvas exported CSV file with the supplied configuration file
$ rosters generate --lab 1 --input /path/to/canvas.csv --config /path/to/config.toml

# generate rosters from a Canvas exported CSV file, specifying output file
$ rosters generate --lab 2 -i /path/to/canvas.csv -o /path/to/output/directory

# generate rosters from a Canvas exported CSV file, skipping Excel file generation
$ rosters generate --lab 2 -i /path/to/canvas.csv --nox
```

# Building from source
Compiling from source requires the following build tools:

|Tool|Version requirement|
|----|-----|
|Rust| >= 1.70 |

With these tools properly installed, run `cargo install --path .` from the root
directory of the project.

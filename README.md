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

On macOS, the configuration file is located at `~/Library/Application
Support/rosters.toml`. On Unix (with the exception of macOS)/Unix-like (such as
Linux) systems with the environmental variable `$XDG_CONFIG_HOME` configured,
the configuration file is located at `$XDG_CONFIG_HOME/rosters.toml`, otherwise
it is located at `~/.config/rosters.toml`. On windows, the configuration file is
located at `%LOCALAPPDATA%\rosters.toml`. TOML files are plain text files and
they can be edited with any text editor (except perhaps for Microsoft Notepad,
which is notorious for creating files with encoding issues).

## Sample Configuration

Here is a sample configuration with all the recognized keys:

```toml
# This is an example configuration to help you get started.

# Configuration is entirely optional. `rosters` will run with default values if
# a given section is not found.

# This file is written in the TOML format. Lines prefixed with the "#" sign
# are comments and will be ignored.

# This is the hard upper limit of the number of groups in a section
number-of-groups = 6

##### Uncomment the following block to enable per-TA PDF generation

# [ta-assignment]
# # LHS is the name of the TA. There cannot be spaces within a name.
# # RHS is the list of sections that the TA is assigned to. It must be a list of
# # integers separated by commas. The generator only supports up to 20 sections per
# # TA.
# Joe = [38, 40]
# Donny = [17, 19, 29, 35]
# Barry = [3, 21, 23, 33]
# Walker = [7, 9, 28, 30]
# Billy = [10, 18, 20, 36]
# Herbert = [5, 13, 15, 25]
# Ronny = [1, 11, 27, 31]
# Jimmy = [2, 12, 22, 32]
# Jerry = [6, 8, 16, 26]
# Ricky = [4, 14, 24, 34]
# Lyndon = [37, 39]

##### The following block will override the course specific settings that follow

# # Comment out/delete section if you don't want to customize the checkpoint
# # columns. These example values are for 1CL.
# [checkpoints]
# # LHS must be "mathbootcamp" or in the form of "lab" followed by an integer.
# "lab0" is automatically understood as "mathbootcamp".
# # RHS is a list of strings. Entries must be enclosed in single or double quotes
# # separted by commas. The generator only supports up to 10 checkpoints per lab.
# # Checkpoint labels cannot be longer than 20 characters each.
# mathbootcamp = []
# lab1 = ['A', 'B3', 'C7', 'D1']
# lab2 = ['A8', 'A12', 'C12', 'D4(a)', 'D4(b)']
# lab3 = ['B4', 'C6', 'D11', 'E10']
# lab4 = ['A6', 'B12', 'C1', 'C5', 'C8']
# lab5 = ['A5', 'B10', 'C2', 'D2(a)', 'D2(b)']

##### Course specific settings. Overriden by the [checkpoints] block. See above

# These are the checkpoints during Summer Session II 2024.
[1AL.checkpoints]
# See "checkpoints" section above
lab0 = []
lab1 = ['A', 'B4', 'C1', 'C2']
lab2 = ['B3-B4', 'B6', 'C8(a)', 'C8(b)']
lab3 = ['A', 'B2', 'Pre-C6', 'C6(a)', 'C6(b)']
lab4 = ['A3', 'B6-B7', 'C2', 'C3(a)', 'C3(b)']
lab5 = ['A', 'B3-B4', 'C2']

[1BL.checkpoints]
lab0 = []
lab1 = ['A5-A7', 'B9-B10', 'C2', 'C6-C7']
lab2 = ['A', 'C', 'D', 'E10', 'E16-E17']
lab3 = ['A', 'B', 'C', 'D(a)', 'D(b)']
lab4 = ['A7', 'B4', 'B7-B9', 'C']
lab5 = ['A', 'B3-B4', 'B7-B13', 'Post-C5']

[1CL.checkpoints]
lab0 = []
lab1 = ['A', 'B3', 'C7', 'D1']
lab2 = ['A8', 'A12', 'C12', 'D4(a)', 'D4(b)']
lab3 = ['B4', 'C6', 'D11', 'E10']
lab4 = ['A6', 'B12', 'C1', 'C5', 'C8']
lab5 = ['A5', 'B10', 'C2', 'D2(a)', 'D2(b)']
```

# Examples

```sh
#  generate a starter configuration file
$ rosters config --output /path/to/file

# generate rosters from a Canvas exported CSV file
$ rosters generate --lab 1 --input /path/to/canvas.csv

# generate rosters from a Canvas exported CSV file and try to max out the group
# sizes at three people
$ rosters generate --lab 1 --group-size 3 --input /path/to/canvas.csv

# generate rosters from a Canvas exported CSV file for the Math Bootcamp with
# the default configuration shipped with the program
$ rosters generate --lab mathbootcamp --defaults --input /path/to/canvas.csv

# generate rosters from a Canvas exported CSV file with the supplied configuration file
$ rosters generate --lab 1 --input /path/to/canvas.csv --config /path/to/config.toml

# generate rosters from a Canvas exported CSV file, specifying output file
$ rosters generate --lab 2 --group-size 3 -i /path/to/canvas.csv -o /path/to/output/directory

# generate rosters from a Canvas exported CSV file, skipping Excel file generation
$ rosters generate --lab 2 --group-size 3 -i /path/to/canvas.csv --nox
```

# Building from source
Compiling from source requires the following build tools:

|Tool|Version requirement|
|----|-----|
|Rust| >= 1.70 |

With these tools properly installed, run `cargo install --path .` from the root
directory of the project. The Windows port builds against the MinGW toolchain.

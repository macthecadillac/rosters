# Setup

`rosters` has no external dependency aside from a functioning OS. Simply
download the appropriate executable for your platform from the [release
page](https://github.com/macthecadillac/rosters/releases).

While configuration is entirely optional, you are strongly encouraged to create
one. The program can group sections by TAs if configured to do so. This can
significantly reduce the number of PDFs generated, especially for on-sequence
labs.

# Usage

This program requires a data file exported from Canvas to work. The data file
could be obtained by going to Canvas course > Grades > Actions > Export Entire
Gradebook.

# Configuration

A configuration file can be supplied at run time through the GUI, otherwise the
program will look for a configuration file at `%LOCALAPPDATA%\rosters.toml`. If
no configuration is found, the program will run with default parameters. TOML
files are plain text files and they can be edited with any text editor (except
perhaps for Microsoft Notepad, which is notorious for creating files with
encoding issues).

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
# integers separated by commas. The generator only supports up to 20 sections per
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
# separted by commas. The generator only supports up to 10 checkpoints per lab.
# Checkpoint labels cannot be longer than 20 characters each.
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

# Building from source
Compiling from source requires the following build tools:

|Tool|Version requirement|
|----|-----|
|Rust| >= 1.70 |

With these tools properly installed, run `cargo install --path .` from the root
directory of the project. This is only tested with the MinGW toolchain. It
should theoretically work with the MSVC toolchain as well.

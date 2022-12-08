# Setup

`lab-tools` has no external dependency aside from a functioning OS. Just
download the appropriate executable for your platform and run in the terminal.

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

## new-spreadsheet

`lab-tools new-spreadsheet --input /path/to/data.csv` will
generate an Excel file from the Canvas exported `data.csv` for grade entry.

## merge

`lab-tools merge --published /path/to/data.csv --latest /path/to/current.xlsx`
will combine the up-to-date grades in `current.xlsx` with data in `data.csv` and
generate an updated data file suitable for uploading to Canvas. It also outputs
another Excel file that takes into account added/dropped students reflected in
the latest data file from Canvas. If and when the TAs make changes to the file,
make sure that ALL entries have correct student IDs in the spreadsheet or
`lab-tools` will skip the row.

## open-config

`lab-tools open-config` opens the configuration file in the default text editor.

# Configuration

On windows, the configuration file is located at
`%LOCALAPPDATA%\lab-tools.toml`. On Unix (such as macOS)/Unix-like (such as
Linux) systems with the environmental variable `$XDG_CONFIG_HOME` set, the
configuration file is located at `$XDG_CONFIG_HOME/lab-tools.toml`, otherwise it
is located at `$HOME/.config/lab-tools.toml`, where `$HOME` is your home folder.

The configuration is done via the TOML language. Here is a sample configuration
with all the recognized keys:

```toml
# This section is mandatory. `lab-tools` won't run without this section. If this
# section is incomplete, `lab-tools` might generate garbage.
[ta-assignment]
# LHS is the name of the TA. There cannot be spaces within a name.
# RHS is the list of sections that the TA is assigned to. It must be a list of
# integers separated by commas.
Casey = [38, 40]
Harry = [17, 19, 29, 35]
Luke = [3, 21, 23, 33]
Kshitij = [7, 9, 28, 30]
Sophia = [10, 18, 20, 36]
Wanda = [5, 13, 15, 25]
Guru = [1, 11, 27, 31]
Hannah = [2, 12, 22, 32]
Billy = [6, 8, 16, 26]
Ting-Chun = [4, 14, 24, 34]
Aniket = [37, 39]

# This section is optional. `lab-tools` will run with default values if this is
# missing
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
$ lab-tools open-config
$ lab-tools rosters --lab 1 --input /path/to/canvas.csv
$ lab-tools rosters --lab 2 -i /path/to/canvas.csv -o /path/to/output/directory
$ lab-tools new-spreadsheet --input /path/to/canvas.csv
$ lab-tools new-spreadsheet --input /path/to/canvas.csv --output /path/to/output/file
$ lab-tools new-spreadsheet -i /path/to/canvas.csv -o /path/to/output/file
$ lab-tools merge --published /path/to/canvas.csv --latest /path/to/TA/grading/spreadsheet
$ lab-tools merge --published /path/to/canvas.csv --latest /path/to/TA/grading/spreadsheet --csv-out /path/to/updated.csv --xlsx-out /path/to/updated.xlsx
```

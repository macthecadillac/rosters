# Setup

## Software requirements

This script leverages the power of LaTeX to generate PDFs. For macOS or Linux,
install TeXLive (the TeXLive installer for macOS is also known as MacTeX). For
Windows, install MikTeX. There might be issues with missing packages if you
installed LaTeX some other way and that your LaTeX installation is a partial
one, such as the `basictex` option from Homebrew. In that case, make sure that
the `multirow`, `tabularx`, `makecell`, `collcell`, `calc`, and `carlito` LaTeX
packages are installed.

## Setup using `pip` (only if you do not have `conda`)

Run `pip install --user .` in the directory containing `setup.py`. This will
install all the required python packages for you and place the script in your 
`PATH`.

## Setup using `conda`

Run `conda install pandas openpyxl matplotlib toml` in the terminal followed
by `pip install .` in the directory containing `setup.py`.

# Usage

This script has a 'multitool' interface, where the main program needs to be
followed by a subcommand, which takes its own arguments. The general syntax is
`1l-tools <subcommand> <arguments> [options]`. There are five subcommands in
total, the usage of which is described below.

Run `1l-tools --help`, `1l-tools rosters --help`, `1l-tools merge --help` etc.
to see the correct syntax and a full list of available options.

## rosters
    
This subcommand generates random rosters for every section in the lab. Go to
Canvas course > Grades > Actions > Export Entire Gradebook. Save the CSV file
somewhere in your system. Run `1l-tools-tools rosters 1 /path/to/data.csv`,
replacing '1' with the lab number of the week and the last part with the actual
path to the CSV file. If your path contains spaces, you will need to either
escape the spaces or put the entire path in between quotes. This will generate
randomized rosters for every section in the current working directory along with
an xlsx file mirroring the pdfs for data entry purposes. You can specify
checkpoints for the lab through the configuration file, which can be overridden
at runtime by the `--checkpoints` option.

## merge

This subcommand serves two purposes. `1l-tools merge /path/to/data.csv` will
generate an Excel file from the Canvas exported `data.csv` for grade entry,
whereas `1l-tools merge /path/to/data.csv -- current /path/to/current.xlsx` will
merge the up-to-date grades in `current.xlsx`  with that of `data.csv` and
generating a data file suitable for uploading to Canvas. It also adds/removes
rows in `current.xlsx` to match the current roster on Canvas.

## review

This subcommand generates a report from the current grades for grade review. Run
`1l-tools review /path/to/current.xlsx`.

## edit-config

`1l-tools edit-config` opens the configuration file in the default text editor.
On macOS/Linux, the terminal text editor is opened if `$EDITOR` is set.

## reset

`1l-tools reset` removes the configuration file.

# Configuration

The configuration file resides in `%LOCALAPPDATA%` or `%APPDATA%` on Windows and
`~/.config` on macOS/Linux. It can be opened by `1l-tools edit-config`. A
configuration is mostly optional but is required for grade review.

The configuration is done via the TOML language. Here is a sample configuration
with all the recognized keys:

```toml
[ta-assignment]
Hanyi = [3, 5, 9, 12]
Keilan = [4, 6, 10, 11]
Sheikh = [1, 2, 7, 8]

[checkpoints]
lab1 = ['A', 'B4', 'C7', 'D1']
lab2 = ['A5', 'A7', 'C12', 'D5']
lab3 = ['B10', 'C17-C19', 'D3-D4', 'D6']
lab4 = ['A6&A7', 'B8', 'Before C1', 'C5']
lab5 = ['A6', 'B11', 'C', 'D2']
lab6 = ['A6', 'B9', 'C9', 'D9']
lab7 = ['A7', 'B10', 'C11']
lab8 = ['A4', 'C3', 'D2', 'E']
lab9 = ['A9', 'B5', 'B8', 'C13', 'D10']

[paths]
# directory to which generated rosters are saved
rosters = '/Volumes/GoogleDrive/Shared drives/1ABCL Fall 22 - Current Quarter/Attendance/1CL'
# must be the full path to an xlsx file
current-grade-sheet = '/Users/maclee/Documents/code/1l-tools-tools/test.xlsx'
# directory to save grade review output
grade-review = '/Users/maclee/Documents/code/1l-tools-tools/Grade Review'
```

The keys for checkpoints do not need to be 'lab1', 'lab2' etc. Anything will
work as long as it ends with a number.

# Examples

```sh
1l-tools rosters 1 /path/to/canvas.csv
1l-tools rosters 2 /path/to/canvas.csv --checkpoints A7 A8 C13 D5
```

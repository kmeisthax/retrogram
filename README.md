# retrogram

Retrogram is a binary disassembler designed for integration with split
disassembly projects, such as those built with the ProjectBase series of
split disassemply templates. Retrogram is flexible enough to support any
processor architecture; however, it's primary focus is on analysis within
architectures commonly used in old game systems.

The typical use case of retrogram would be to use it to analyze your base ROM,
using the `dis` command to export individual routines in a given assembler
format. Data outside of retrogram can be imported into it's database using the
`import` command. For example, an existing split disassembly project's symbol
table can be imported into retrogram, and code exported from retrogram will use
the project's correct symbol names.

## Creating a new retrogram project

Retrogram looks for program parameters in a project file called
`retrogram.json`. This file lists all of the programs and data sources in your
project, and will be referenced by various commands.

This is what a retrogram project file might look like:

    {
        "programs": {
            "mygame": {
                "platform": "gb",
                "arch": "sm83",
                "assembler": "rgbds",
                "images": ["mygame.gbc"],
                "data_sources": ["mygame_sym"]
            }
        },
        "data_sources": {
            "mygame_sym": {
                "format": "rgbds_sym",
                "files": ["mygame.sym"],
                "programs": ["mygame"]
            }
        }
    }

By default, retrogram also creates a project database file. Multiple programs
can reside within the same project and use the same database file as long as
they have the same architecture.

(Due to technical limitations, projects with multiple architectures must specify
a separate database file for each architecture. They cannot share the same
file.)

## CLI Syntax

    retrogram [command] [start_pc] --program=[prog_name]

The program name specified must match a valid program name specified in the
`retrogram.json` file.

### Available commands

 * `scan` - From a given start point, traverse the control flow graph and find
   everything that is called or jumped to by that routine.
 * `dis` - Extract all code for a given subroutine. The location must have
   already been scanned, either directly or as a result of scanning another
   routine that calls this one.
 * `import` - Copy information from a data source into the retrogram project
   database. You must also specify an `--external_db` parameter naming a data
   source registered in your project.
 * `backrefs` - List all code that references a particular memory address,
   either as a jump/call target or as a memory location.
 * `rename` - Rename a given location, giving it a new label. This can be used
   to either rename a label or assign one to an unlabeled location. You must
   provide a label string after the `start_pc` argument.

### Available parameters

The following parameters name things within the project file:

 * `--program` - Names a program in the current project to analyze.
 * `--external_db` - Names a data source in the current project to import.
 * `--project` - Point retrogram to a different `retrogram.json` file, in case
   you don't want to use the current location's.

It is possible to override a project's program parameters for various reasons
by specifying an override parameter:

 * `--image` - Replace the current program's image file.
 * `--platform` - Analyze the current program with a different platform.
 * `--arch` - Analyze the current program as a different architecture.
 * `--asm` - Use a different assembler syntax when outputting disassembled code.

When importing data from an external data source, the following options are also
meaningful, and can be used to specify a data source not mentioned in the
project database:

 * `--external_db_format` - Change the format of the selected external data
   source.
 * `--external_db_file` - Pull the data from a different file.

### Specifying the PC

Program locations can be specified either by symbol name or address. If
specified as an address, you may also specify additional *contexts* defined by
your platform or architecture, such as bank addresses. Each context must be
separated by colons; their format is platform or architecture specific. The
actual pointer value must be a hexdecimal value.

For example, to specify a subroutine in AArch32 THUMB encoding, you would
specify:

    T:80A21C3

The same address without the `T:` would be treated as non-THUMB.

## Architectural support

Retrogram supports, or currently has support planned, for the following
processor architectures, platforms, and assemblers:

 * SM83, aka LR35902, aka "GB Z80" (more or less complete)
   * Game Boy platform model
   * RGBDS syntax
 * ARM/THUMB (in progress)
   * Game Boy Advance platform model
   * Nintendo DS platform model (not started)
   * ARMIPS syntax
 * WDC 65C816 (planned)
   * Super Famicom/NES platform model (planned)

### Adding new architectures to retrogram

Retrogram currently does not support plugins (for various reasons, this is not
expected to change). However, if you would like to extend retrogram to a new
architecture, please see the documentation in the `arch` module for more
information. There is a minimum of seven types that will need to support a
number of traits in order to properly model the architecture in question.
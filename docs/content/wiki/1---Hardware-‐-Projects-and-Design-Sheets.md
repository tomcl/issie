## Introduction

Issie captures hardware as a set of connected components on one or more *design sheets* (schematics).

Sheets are implemented as `type CanvasState = Component list * Connection list`.

A hierarchical design is implemented by creating *custom components* which represent design sheets, with ports defined by the Input and Output ports on the underlying sheet. One design sheet can have an arbitrary number of custom component instances.

The default order of ports on an instance symbol is determined by the (vertical) alignment of its sheet's Input and Output components. By default all inputs are on the LHS, all outputs on the RHS. Custom component instances can have symbol port order and side rearranged (per instance) via a drag and drop interface.

Custom components are implemented with a datatype:

```
    /// Name identifies the LoadedComponent used.
    /// The labels define legends on symbol designating inputs or outputs: 
    /// and are the names of the Input or Output components of the CC sheet.
    /// Label strings are unique per CustomComponent.
    /// Label position in list determines inputPortNumber or outputPortNumber of label.
    /// Multiple CustomComponent instances are differentiated by Component data.
    type CustomComponentType = {
        Name: string
        // Tuples with (label * connection width).
        InputLabels: (string * int) list
        OutputLabels: (string * int) list
    }
```

This defines the interface between a custom component and its underlying sheet. When sheet input or output ports are added, deleted, or renamed every custom component instance of the sheet must be correspondingly modified. A dialog to do this automatically is contained in `UI/CustomCompPorts.fs`.

## Files and sheets

Issie designs consist of a `Project`:

```
    /// Type for an open project which represents a complete design.
    /// ProjectPath is directory containing project files.
    /// OpenFileName is name of file from which current schematic sheet 
    /// is loaded/saved, without extension or path
    /// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
    type Project = {
        /// directory which contains the project files
        ProjectPath : string
        /// name of open sheet (without extension)
        OpenFileName : string
        /// componnets have one-one correspondence with files
        LoadedComponents : LoadedComponent list
        }
```

where each sheet is represented in memory as a `LoadedComponent`:

```
    /// Static data describing a schematic sheet loaded as a custom component.
    /// Every sheet is always identified with a file from which it is loaded/saved. 
    /// Name is human readable (and is the filename - without extension) and identifies sheet.
    /// File path is the sheet directory and name (with extension).
    /// InputLabels, OutputLabels are the I/O connections.
    /// The I/O connection integers are bus widths.
    /// The I/O connection strings are human readable. The strings are guaranteed
    /// to be unique in the I/O connection list. I.e. An input label may be the same
    /// as an output label, but two input (or output) labels cannot be the same.
    /// The position in the I/O connections list is important as it implicitly
    /// indicates the port number. For example, the first element in the InputLabels
    /// list is related to the Component's Port with PortNumber 0.
    /// Two instances of a loaded component have the same LoadedComponent data.
    type LoadedComponent = {
        /// File name without extension = sheet name
        Name: string
        /// When the component was last saved
        TimeStamp: System.DateTime 
        /// Complete file path, including name and dgm extension
        FilePath : string
        /// Info on WaveSim settings
        WaveInfo: SavedWaveInfo option
        /// F# equivalent of Diagram components and connections including layout
        CanvasState : CanvasState
        /// Input port names, and port numbers in any created custom component
        InputLabels : (string * int) list
        /// Output port names, and port numbers in any created custom component
        OutputLabels : (string * int) list
    }

```

On disk each project is represented by a single directory containing

* One `projName.dprj` file. This is (currently) 0 length and a placeholder only - it is never read.
* One `sheetName.dgm` file for each sheet. 

Sheets are self-contained and linked only via custom component instances. Therefore sheet files representing parts of designs can be copied and moved between projects arbitrarily. One subtle point: a sheet file is not allowed to be copied within a single project. That is because component IDs muts be unique and that will no longer be the case if two copies of the same sheet exist within a project.

## Backup

Issie takes snapshots of each sheet whenever its contents have changed a lot when the sheet is saved - this happens manually or when the currently edited sheet is changed. These backups are put into a subdirectory `Backup` named by the sheet name and the date and time of creation. Backup files can be (manually) renamed and added to the project directory to view them, or used to replace the more recent sheet file in the project directory. This is quite crude, but very safe: if Issie crashes and corrupts a sheet file a backup will be available.

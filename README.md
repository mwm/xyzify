# XYZify

This is a tool for converting gcode files generated by slic3r (the open source
object slicing tool) for use by XYZ Printings XYZware printing tool.

## Slic3r configuration

XYZware reads the comments in a gcode file, looking for specific information. If it fails
to find that, it won't load the file or printing may fail. So we configure slic3r
to provide that information.

Since this information is in the comments, we use the slic3r "Start G-code"
setting. In my version of slic3r, this is in the logical place: "Printer Settings"
under "Custom G-code".

Add the following to the **start** of that section:

```
; --- MOVE THIS SECTION TO THE TOP AND DELETE THIS LINE ---
; filename = composition.3w
; machine = daVinciF10
; material = abs
; layer_height = [layer_height]
; total_layers = 173
; total_filament = 0.00
; extruder = 1
; --- END SECTION ---
```

The XYZify program finds the initial comment (`--MOVE THIS ...`), removing any comments
and blank lines it finds along the way. The comments - and any actual g-code - between
that line and the last one (`-- END SECTION...`) are output, the lines preceeding this
section that haven't been removed are output, then the rest of the file is output
unchanged.

## Usage

The command line version is very raw, as there's a web version if you want a nice
interface.

### Console

Download the files `Main.hs` and `GCode.hs` into a directory, and then run the command
`ghc -O2 -o xyzify Main.hs` to compile `xyzify`. Copy it to somewhere where your
command processor will find it.

From favorite command line access tool - run xyzify with standard
input being a gcode file from a properly configured slic3r, and output going to
the 3w file:

```
$ xyzify < myslic3r.gcode > myslic3r.3w
```

### Web

The web version is also very simple. If you prettify it, please let me know!

Click "Choose File" to select the gcode file to convert. Then click "Convert" to
upload and convert the file. The results will be downloaded with the extension ".3w"
added to the file name. If the name ended with ".gcode", then that will be stripped
removed, so the converted `myslic3r.gcode` will be downloaded as `myslic3r.3w` 
# XYZifier

This is a tool for converting `.gcode` files from slic3r and similar
tools into `.3w` files for use by XYZ Printing's XYZWare tool. This
lets you use the open source version of slic3r, which is much better
than the version built into XYZWare.

Main.hs provides a command line tool. Run with no arguments, it acts
as a filter. With one argument, it'll convert the file named in the
argument.

Web.hs is a simple web application that will upload, convert and
download files.

XComponents Package
===================

Overview
--------

The package provides a set of Lazarus components which can be used in the Lazarus IDE to design pages, 
and are capable of being cross-compiled using the pas2js compiler, to produce an equivalent HTML/Javascript executable.

The package must be installed in the Lazarus IDE 
(for Lazarus downloads, see https://wiki.lazarus.freepascal.org/fpcupdeluxe )

XComponents is developed and tested on Lazarus version 2.1.0, FPC version 3.3.1.

Installation
------------

The Lazarus package FrameViewer09 must first be installed into the Lazarus IDE.  The package is available 
from https://github.com/BerndGabriel/HtmlViewer.  This supports the XHTMLText component within XComponents.
(also see https://wiki.lazarus.freepascal.org/THtmlPort for background)

The XComponents package can be installed either with or without components which use the ‘Chromium Embedded Framework’.  
The compiler directive ‘Chromium’ must be set in ‘Package Options’ to include these components.

If the ‘Chromium’ directive is set, then the CEF4Delphi_Lazarus package must also have been installed.
(https://github.com/salvadordf/CEF4Delphi)

Compile and install the XComponents package into Lazarus IDE.

The supplied XComponents will be available in the ‘XComponents’ tab in the IDE.

See docs/XComponentsProjectNotes.md for how to use XComponents in a Lazarus project.





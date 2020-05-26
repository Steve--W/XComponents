XComponents Package
===================

Overview
--------

The package provides a set of Lazarus components which can be used in the IDE to design pages, 
and are capable of being cross-compiled using the pas2js compiler, to produce an equivalent HTML/Javascript executable.

The package must be installed in the Lazarus IDE 
(for Lazarus downloads, see https://www.lazarus-ide.org/index.php?page=downloads )

Installation
------------

The package can be installed either with or without components which use the ‘Chromium Embedded Framework’.  
The compiler directive ‘Chromium’ must be set in both ‘Package Options’ and ‘Project Options’ to include these components.

If the ‘Chromium’ directive is set, then the CEF4Delphi_Lazarus package must also have been installed.
(https://github.com/salvadordf/CEF4Delphi)

Compile and install the package into Lazarus IDE.

The supplied XComponents will be available in the ‘XComponents’ tab in the IDE.

See XComponentsProjectNotes.txt for usage of XComponents in a Lazarus project.





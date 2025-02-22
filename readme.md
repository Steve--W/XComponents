XComponents Package
===================

Overview
--------

The package provides a set of Lazarus components which can be used in the Lazarus IDE to design pages, 
and are capable of being cross-compiled using the pas2js compiler, to produce an equivalent HTML/Javascript executable.

The package must be installed in the Lazarus IDE 
(for Lazarus downloads, see https://wiki.lazarus.freepascal.org/fpcupdeluxe )

XComponents is developed and tested on Lazarus version 3.4, FPC version 3.2.2.

Installation
------------

The Lazarus package FrameViewer09 must first be installed into the Lazarus IDE.  The package is available 
from https://github.com/BerndGabriel/HtmlViewer.  This supports the XHTMLText component within XComponents.
(also see https://wiki.lazarus.freepascal.org/THtmlPort for background)
NOTE: In order for hotspot links to work properly in frame viewer, the following fix is required:
  Make this change to the file framview.pas, on line 3100, in method TFrameViewer.HotSpotClick: 
  //if not HotSpotClickHandled(ExpURL, Target) then       //!!!! 10/11/2020 replaced this to avoid stack overflow
  if (((Dest='') or (Dest[1]<>'#')) and (not HotSpotClickHandled(ExpURL, Target)))
  or ((Dest<>'') and (Dest[1]='#') ) then
  begin
    ...

The XComponents package can be installed either with or without components which use the ‘Chromium Embedded Framework’.  
The compiler directive ‘Chromium’ must be set in ‘Package Options’ to include these components.

If the ‘Chromium’ directive is set, then the CEF4Delphi_Lazarus package must also have been installed.
(https://github.com/salvadordf/CEF4Delphi)

Compile and install the XComponents package into Lazarus IDE.

The supplied XComponents will be available in the ‘XComponents’ tab in the Lazarus IDE.

See docs/XComponentsProjectNotes.md for how to use XComponents in a Lazarus project.





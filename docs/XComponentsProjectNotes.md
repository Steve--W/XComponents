Desktop-and-Web-Compilable Projects using Lazarus IDE and XComponents
=====================================================================

Pre-Requisites
--------------

1  Pas2JS
   XComponents projects require access to pas2js source units pas2jsCompiler etc.  
   A default working set of these is already provided in the XComponents subfolder pas2js0. 
 
2  CEF (Chromium Embedded Framework)
   Use of CEF is optional.  To use �CEF� based components on the desktop, download the cef4delphi_lazarus package and cef4 framework 
   (see https://github.com/salvadordf/CEF4Delphi - the README file there contains download and installation details) .

   Create a new folder (eg. FrameworkDir) that will contain the elements that are required by a Lazarus project at runtime � copy everything from the cef4 �release� and �resources� folders into it.

   In Lazarus IDE, compile and install the package cef4Delphi_Lazarus.lpk.

   Installation of CEF4 is not mandatory � if it is omitted, projects containing �Iframe� based elements will run normally in the browser/JS environment, but on the desktop will launch separate browser windows to display content.

   At time of writing, cef4 is tested only for Windows desktop.

3  XComponents Package
   In Lazarus IDE, open the package XComponents.lpk.

   In Options>Custom Options>Conditionals, check the path settings for the listed pas2js elements (macro XComponents_pas2JSpaths) and edit as required.  This is set by default to point to the �pas2js0� subfolder.

   If you have installed cef4, then:
   set the compiler directive �dChromium in �Custom Options�.

   Compile the package, and install in the IDE.

4  Project Template
   In Lazarus IDE, Select Tools>Project Template Options. 

   Set the directory to the templates directory under the Xcomponents source directory. (eg.) C:\XComponents\templates\.  OR - copy the template folder XcomponentsTemplate into your existing templates folder.




To Create a new Project
-----------------------

In Lazarus IDE, Select Project>New Project>Web-Compilable (XComponents), and set values for the listed parameters.

This will create a new project in the IDE.

In the Project Inspector, add a requirement for the package XComponents.

Select Project>Options.  Make the following changes:

Config & Target :    Set the �WG checkbox.
Custom Options :     If you have cef4 installed, set �dChromium.
Custom Options/Conditionals: Add the line: IncPath += ';$PkgDir(XComponents)';
Custom Options/Conditionals: Add the line: UnitPath +=';$PkgOutDir(CEF4Delphi_Lazarus)';

If using cef4, the runtime project will need to locate the folder containing the cef runtime framework files. 
Edit the project.lpr file and set the string variable CEFLibDir as required.

The project should now compile and run successfully.


XComponents
-----------

Build your project using only components provided on the XComponents palette.

Position components using the vertical and horizontal layout container panels TXHBox and TXVBox.  
There is no absolute positioning.  
A new panel may have zero width � add the first child component to the form, and use the object inspector tree to drag it into the Vbox or Hbox panel node.

Component heights and widths can be expressed in pixels (20, 20px) or as a percentage of the parent dimension (20%).


Additional Forms
----------------

To add a form to your project, create a form in the normal way in the IDE, then amend its type to TXForm.

There are also a few coding requirements � see the template unit provided in XComponents/docs/TemplateForAdditionalForm.pas.

In event code, open/close a popup form using the procedures ShowXForm, CloseXForm.  Alternatively set the form�s �Showing� property to �Normal�,�Modal� or �No�.


Running in the Browser
----------------------

Run your project on the desktop.  In the main menu, select  Web > Run in Browser.

This will recompile your project using pas2js to generate Javascript, and will launch a new browser page.
At time of writing, this is tested only for Chrome browser.




{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit XComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  WrapperPanel, StringUtils, LazsUtils, NodeUtils, HTMLUtils, Events, XForm, 
  EventsInterface, UtilsJSCompile, XVBox, XHBox, XScrollBox, XTabControl, 
  XGroupBox, XButton, XEditBox, XCheckBox, XLabel, XHyperLink, XComboBox, 
  XMemo, XTree, XTable, XCode, XProgressBar, XNumericSlider, XNumberSpinner, 
  XDatePicker, XColorPicker, XImage, XBitMap, XRadioBtns, XMenu, XIFrame, 
  XHTMLText, XHTMLEditor, XSVGContainer, XGPUCanvas, XThreads, XTrapEvents, 
  XStore, PasteDialogUnit, CompilerLogUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('XVBox', @XVBox.Register);
  RegisterUnit('XHBox', @XHBox.Register);
  RegisterUnit('XScrollBox', @XScrollBox.Register);
  RegisterUnit('XTabControl', @XTabControl.Register);
  RegisterUnit('XGroupBox', @XGroupBox.Register);
  RegisterUnit('XButton', @XButton.Register);
  RegisterUnit('XEditBox', @XEditBox.Register);
  RegisterUnit('XCheckBox', @XCheckBox.Register);
  RegisterUnit('XLabel', @XLabel.Register);
  RegisterUnit('XHyperLink', @XHyperLink.Register);
  RegisterUnit('XComboBox', @XComboBox.Register);
  RegisterUnit('XMemo', @XMemo.Register);
  RegisterUnit('XTree', @XTree.Register);
  RegisterUnit('XTable', @XTable.Register);
  RegisterUnit('XCode', @XCode.Register);
  RegisterUnit('XProgressBar', @XProgressBar.Register);
  RegisterUnit('XNumericSlider', @XNumericSlider.Register);
  RegisterUnit('XNumberSpinner', @XNumberSpinner.Register);
  RegisterUnit('XDatePicker', @XDatePicker.Register);
  RegisterUnit('XColorPicker', @XColorPicker.Register);
  RegisterUnit('XImage', @XImage.Register);
  RegisterUnit('XBitMap', @XBitMap.Register);
  RegisterUnit('XRadioBtns', @XRadioBtns.Register);
  RegisterUnit('XMenu', @XMenu.Register);
  RegisterUnit('XIFrame', @XIFrame.Register);
  RegisterUnit('XHTMLText', @XHTMLText.Register);
  RegisterUnit('XHTMLEditor', @XHTMLEditor.Register);
  RegisterUnit('XSVGContainer', @XSVGContainer.Register);
  RegisterUnit('XGPUCanvas', @XGPUCanvas.Register);
  RegisterUnit('XThreads', @XThreads.Register);
  RegisterUnit('XTrapEvents', @XTrapEvents.Register);
  RegisterUnit('XStore', @XStore.Register);
end;

initialization
  RegisterPackage('XComponents', @Register);
end.

program UberPDFExample;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// include the file to let uber figure out what compiler and version is used
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_compiler_version_defs.pascal-inc'}
//------------------------------------------------------------------------------

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UberPDFExample.Forms.Main
  { you can add units after this };

{$R *.res}

//------------------------------------------------------------------------------
// Include the Uber base types
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_defs.pascal-inc'}
//------------------------------------------------------------------------------

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.


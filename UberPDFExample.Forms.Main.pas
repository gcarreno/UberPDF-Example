unit UberPDFExample.Forms.Main;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// Example producing "hello-world.pdf" in the working directory.
//------------------------------------------------------------------------------
// Written using Lazarus 1.9.0 / FPC 3.1.1.
//------------------------------------------------------------------------------
// Tested: Lazarus 1.9.0 / FPC 3.1.1 - Linux 64 (Ubuntu 17.10)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// NOTES:
//------------------------------------------------------------------------------
// YOU MUST ADD TO PROJECT DEFINES the toolchain you are linking to. Examples:
// Menu->Project->Project Options->Compiler Options->Custom Options: (Linux 32)
// -dUBER_LINTEL32_GCC_LINTEL32
// Menu->Project->Project Options->Compiler Options->Custom Options: (Linux 64)
// -dUBER_LINTEL64_GCC_LINTEL64

// YOU MUST ADD TO PROJECT SEARCHPATH the include you are linking to. Examples:
// Menu->Project->Project Options->Other Unit Files (-Fu): (Linux 64bits)
// Path/To/UberPDFSDK/userbaselibs/uberpdfsdk/include/lintel64/gcc
// Menu->Project->Project Options->Include Files (-Fi):
// $(ProjOutDir);Path/To/UberPDFSDK/uberbaselibs/uberpdfsdk/include/lintel64/gcc
// Menu->Project->Project Options->Libraries (-FU):
// Path/To/UberPDFSDK/userbaselibs/uberpdfsdk/lib/lintel64/gcc/librtl
//
// Menu->Project->Project Options->Other Unit Files (-Fu): (Linux 32bits)
// Path/To/UberPDFSDK/userbaselibs/uberpdfsdk/include/lintel32/gcc
// Menu->Project->Project Options->Include Files (-Fi):
// $(ProjOutDir);Path/To/UberPDFSDK/uberbaselibs/uberpdfsdk/include/lintel32/gcc
// Menu->Project->Project Options->Libraries (-FU):
// Path/To/UberPDFSDK/userbaselibs/uberpdfsdk/lib/lintel32/gcc/librtl
//------------------------------------------------------------------------------
// AFTER THE "USES" CLAUSE BELOW YOU MUST DEFINE THE PATH TO THE UBER_PDFSDK_SO
// using the const UBER_PDFSDK_SO_NAME = 'PATH_TO_UBER_PDFSDK_DYN_LIB';
// (see below).
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// UberPdfSdk currently dynamiclly links for Lazarus. Static link version soon.
//------------------------------------------------------------------------------
{$DEFINE UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// include the file to let uber figure out what compiler and version is used
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_compiler_version_defs.pascal-inc'}
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, StdCtrls,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

//------------------------------------------------------------------------------
// If we dynamic link, define the path to the UberPDF-SDK .dll, .so, or .bundle
//------------------------------------------------------------------------------
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
//------------------------------------------------------------------------------
{$IFDEF UBER_LINTEL32_GCC_LINTEL32}
const UBER_PDFSDK_SO_NAME = './uber/uberbaselibs/uberpdfsdk/lib/lintel32/gcc/librtl/libuberpdfsdkdyn.so';
{$ENDIF}
{$IFDEF UBER_LINTEL64_GCC_LINTEL64}
const UBER_PDFSDK_SO_NAME = './uber/uberbaselibs/uberpdfsdk/lib/lintel64/gcc/librtl/libuberpdfsdkdyn.so';
{$ENDIF}
//------------------------------------------------------------------------------
{$ENDIF}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// Include the Uber base types
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_defs.pascal-inc'}
//------------------------------------------------------------------------------

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExecute: TButton;
    memlog: TMemo;
    psMain: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    procedure btnExecuteClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnExecuteClick(Sender: TObject);
begin
  btnExecute.Enabled := False;
  btnExecute.Enabled := True;
end;

end.


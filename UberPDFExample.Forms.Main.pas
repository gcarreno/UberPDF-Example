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
//
// All these have been added under the Build Options available on the project.
// I've also adjusted the OS and CPU in each Build Option.
// Make sure you select the correct Build Option for your system.

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
//
// All these have been added under the Build Options available on the project.
// Make sure you select the correct Build Option for your system.
//------------------------------------------------------------------------------
// AFTER THE "USES" CLAUSE BELOW YOU MUST DEFINE THE PATH TO THE UBER_PDFSDK_SO
// using the const UBER_PDFSDK_SO_NAME = 'PATH_TO_UBER_PDFSDK_DYN_LIB';
// (see below).
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// These are the paths for the *.a in order to get a static compile
//------------------------------------------------------------------------------
{$IFNDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  {$IFDEF UNIX}
    {$LINKLIB libuberpdfsdk.a}
    {$LINKLIB libqpdf.a}
    {$LINKLIB libjpeg.a}
    {$LINKLIB libpng.a}
    {$LINKLIB libz.a}
  {$ENDIF}
  {$IFDEF WINDOWS}
    {$LINKLIB libuberpdfsdk.obj}
    {$LINKLIB libqpdf.obj}
    {$LINKLIB libjpeg.obj}
    {$LINKLIB libpng.obj}
    {$LINKLIB libz.obj}
  {$ENDIF}
{$ENDIF}
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, StdCtrls,
  UberPDFExample.Common,
  UberPDFExample.Examples.HelloWorld,
  UberPDFExample.Examples.AddJPEG,
  UberPDFExample.Examples.SplitDocument,
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
//------------------------------------------------------------------------------
// Update these paths acording to your path to the UberPDF folders
//------------------------------------------------------------------------------
{$IFDEF UBER_LINTEL32_GCC_LINTEL32}
const UBER_PDFSDK_SO_NAME = './uber/uberbaselibs/uberpdfsdk/lib/lintel32/gcc/librtl/libuberpdfsdkdyn.so';
{$ENDIF}
{$IFDEF UBER_LINTEL64_GCC_LINTEL64}
const UBER_PDFSDK_SO_NAME = './uber/uberbaselibs/uberpdfsdk/lib/lintel64/gcc/librtl/libuberpdfsdkdyn.so';
{$ENDIF}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Update these paths acording to your path to the UberPDF folders
//------------------------------------------------------------------------------
{$IFDEF UBER_WINTEL32_MSVC_WINTEL32}
const UBER_PDFSDK_SO_NAME = '.\uber\uberbaselibs\uberpdfsdk\lib\wintel32\msvc\???\libuberpdfsdkdyn.dll';
{$ENDIF}
{$IFDEF UBER_WINTEL64_MSVC_WINTEL64}
const UBER_PDFSDK_SO_NAME = '.\uber\uberbaselibs\uberpdfsdk\lib\wintel64\msvc\???\libuberpdfsdkdyn.dll';
{$ENDIF}
//------------------------------------------------------------------------------
{$ENDIF}
//------------------------------------------------------------------------------

type

{ TfrmMain }
  TfrmMain = class(TForm)
    btnEx_HelloWorld: TButton;
    btnEx_AddJPG: TButton;
    btnEx_SplitDocument: TButton;
    lblHeader: TLabel;
    memlog: TMemo;
    psMain: TPairSplitter;
    pssButtons: TPairSplitterSide;
    pssLog: TPairSplitterSide;
    procedure btnEx_AddJPGClick(Sender: TObject);
    procedure btnEx_HelloWorldClick(Sender: TObject);
    procedure btnEx_SplitDocumentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure LogClear;

  public
    procedure Log(aMSG: String);

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LogClear;
  if (LoadUberPDFSdk()) then begin
    Log('LoadUberPDFSdk() Succeeded');
  end else begin
    Log('LoadUberPDFSdk() Failed');
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  UnLoadUberPDFSdk();
end;

procedure TfrmMain.btnEx_HelloWorldClick(Sender: TObject);
begin
  btnEx_HelloWorld.Enabled := False;
  UberPDFExample.Examples.HelloWorld.CreatePdfDocument;
  btnEx_HelloWorld.Enabled := True;
end;

procedure TfrmMain.btnEx_SplitDocumentClick(Sender: TObject);
begin
  btnEx_SplitDocument.Enabled := False;
  UberPDFExample.Examples.SplitDocument.CreatePdfDocument;
  btnEx_SplitDocument.Enabled := True;
end;

procedure TfrmMain.btnEx_AddJPGClick(Sender: TObject);
begin
  btnEx_AddJPG.Enabled := False;
  UberPDFExample.Examples.AddJPEG.CreatePdfDocument;
  btnEx_AddJPG.Enabled := True;
end;

procedure TfrmMain.LogClear;
begin
  memLog.Clear;
end;

procedure TfrmMain.Log(aMSG: String);
begin
  memLog.Lines.Add(aMSG);
end;

end.


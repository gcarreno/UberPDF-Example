unit UberPDFExample.Forms.Main;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// Example producing various PDF files in the working directory:
// hello-world.pdf
// add-jpeg.pdf
// split-document.pdf
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
// IMPORTANT:
// Please have a look at unit UberPDFExample.Common.pas for more paths to be
// taken care of.
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, StdCtrls,
  UberPDFExample.Common,
  UberPDFExample.Examples.HelloWorld,
  UberPDFExample.Examples.AddJPEG,
  UberPDFExample.Examples.SplitDocument;

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


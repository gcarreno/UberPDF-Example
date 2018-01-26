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
  {$LINKLIB libuberpdfsdk.a}
  {$LINKLIB libqpdf.a}
  {$LINKLIB libjpeg.a}
  {$LINKLIB libpng.a}
  {$LINKLIB libz.a}
{$ENDIF}
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



//------------------------------------------------------------------------------
// Include the Uber base types
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_defs.pascal-inc'}
//------------------------------------------------------------------------------

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnEx_HelloWorld: TButton;
    btnEx_AddJPG: TButton;
    lblHeader: TLabel;
    memlog: TMemo;
    psMain: TPairSplitter;
    pssButtons: TPairSplitterSide;
    pssLog: TPairSplitterSide;
    procedure btnEx_HelloWorldClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure LogClear;
    procedure Log(aMSG: String);

    function LoadUberPDFSdk: Boolean;
    function UnLoadUberPDFSdk: Boolean;
    function IsUberPDFSdkLoaded: Boolean;
  public
    procedure Ex_HelloWorld_CreatePdfDocument;
    function Ex_HelloWorld_CreatePdfPage(UberPdfSdkInstanceH: UBERPDFSDK_INSTANCE_HANDLE): UBERPDFSDK_PDF_PAGE_HANDLE;
    procedure Ex_HelloWorld_AddUberPDFFancyTextLine(var ContentString : AnsiString);
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
  Ex_HelloWorld_CreatePdfDocument();
  btnEx_HelloWorld.Enabled := True;
end;

procedure TfrmMain.LogClear;
begin
  memLog.Clear;
end;

procedure TfrmMain.Log(aMSG: String);
begin
  memLog.Lines.Add(aMSG);
end;

function TfrmMain.LoadUberPDFSdk: Boolean;
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
var
  FileName : AnsiString;
{$ENDIF}
begin
  // Load the DLL (if we are dynamic linking) else return true
  {$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
    Log('Loading Dynamic Library');
    FileName := UBER_PDFSDK_SO_NAME;
    Result := (UberPdfSdkDynLibLoad(pAnsiChar(FileName)) <> UBER_FALSE);
    exit;
  {$ENDIF}
  Result := True;
end;

function TfrmMain.UnLoadUberPDFSdk: Boolean;
begin
  // unload the DLL (if we are dynamic linking) else return true
  {$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
    Log('Unloading Dynamic Library');
    Result := (UberPdfSdkDynLibUnload() <> UBER_FALSE);
    exit;
  {$ENDIF}
  result := True;
end;

function TfrmMain.IsUberPDFSdkLoaded: Boolean;
begin
  // test if the DLL is loaded (if we are dynamic linking) else return true
  {$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
    Result := (IsUberPdfSdkDynLibLoaded() <> UBER_FALSE);
    exit;
  {$ENDIF}
  Result := True;
end;

procedure TfrmMain.Ex_HelloWorld_CreatePdfDocument;
var
  UberPdfSdkSystemInitStruct : UBERPDFSDK_SYSTEM_INIT_STRUCT;
  UberPdfSdkInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  PageHandle : UBERPDFSDK_PDF_PAGE_HANDLE;
  FileName : AnsiString;
begin
  if (not(IsUberPDFSdkLoaded())) then begin
    Log('UberPDFSdk is not loaded - CreatePdfDocument() cannot proceeed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdk instance
  //----------------------------------------------------------------------------
  Log('Initialising System');
  FillChar(UberPdfSdkSystemInitStruct,
           sizeof(UberPdfSdkSystemInitStruct),
           0);
  UberPdfSdkSystemInitStruct.StructSize := sizeof(UberPdfSdkSystemInitStruct);
  UberPdfSdkSystemInitStruct.StructVersion :=  1;
  UberPdfSdkInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    Log('UberPdfSdk_System_Init() failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an empty Pdf document
  //----------------------------------------------------------------------------
  Log('Creating an empty PDF Document');
  if (UberPdfSdk_Pdf_Doc_CreateEmpty(UberPdfSdkInstanceH) <> UBER_STATUS_OK) then begin
    Log('UberPdfSdk_Pdf_Doc_CreateEmpty() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create a page
  //----------------------------------------------------------------------------
  Log('Creating a page');
  PageHandle := Ex_HelloWorld_CreatePdfPage(UberPdfSdkInstanceH);
  if (PageHandle = UBER_HANDLE_NULL) then begin
    Log('CreatePdfPage failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Insert the page into the beginning of the document
  //----------------------------------------------------------------------------
  Log('Inserting the page into the beginning of the document');
  if (UberPdfSdk_Pdf_Doc_Pages_InsertPage(UberPdfSdkInstanceH,
                                          PageHandle,
                                          1) <> UBER_STATUS_OK) then begin
    Log('UberPdfSdk_Pdf_Doc_Pages_InsertPage() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the page
  //----------------------------------------------------------------------------
  Log('Free-ing the page');
  if (UberPdfSdk_Pdf_Page_Free(PageHandle) <> UBER_HANDLE_NULL) then begin
    Log('UberPdfSdk_Pdf_Page_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Save the document
  //----------------------------------------------------------------------------
  Log('Saving the document');
  FileName := 'hello-world.pdf';
  if (UberPdfSdk_Pdf_Doc_SaveToFileA(UberPdfSdkInstanceH,
                                     pAnsiChar(FileName),
                                     Length(FileName),
                                     nil,  // lpOwnerPasswordA
                                     0,    // cchlpOwnerPasswordBufferSize
                                     nil,  // lpUserPasswordA
                                     0,    // cchlpUserPasswordBufferSize
                                     nil,  // lpPdfOptionalMinVersionStrA
                                     0,    // cchlpPdfOptionalMinVersionStrBufferSize
                                     nil,  // lpPdfOptionalForceVersionStrA
                                     0,    // cchlpPdfOptionalForceVersionStrBufferSize
                                     nil,  // lpPdfOptionalHeaderExtraStrA
                                     0,    // cchlpPdfOptionalHeaderExtraStrBufferSize
                                     UBERPDFSDK_PDF_DOC_SAVE_FLAGS_USE_DEFAULT,
                                     UBERPDFSDK_PDF_DOC_SAVE_FLAGS_ENCRYPT_USE_NONE) <> UBER_STATUS_OK) then begin
    Log('UberPdfSdk_Pdf_Doc_SaveToFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the UberPdfSdk instance
  //----------------------------------------------------------------------------
  Log('Free-ing the UberPdfSdk instance');
  if (UberPdfSdk_System_Free(UberPdfSdkInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    Log('UberUberPdfSdk_System_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  Log('CreatePdfDocument() succeeded');
  Log('');
end;

function TfrmMain.Ex_HelloWorld_CreatePdfPage(UberPdfSdkInstanceH: UBERPDFSDK_INSTANCE_HANDLE
  ): UBERPDFSDK_PDF_PAGE_HANDLE;
var
  PageHandle : UBERPDFSDK_PDF_PAGE_HANDLE;
  MediaWidth :UBER_FLOAT;
  MediaHeight : UBER_FLOAT;
  MediaRotation : UBER_INT;
  ResourceDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  ContentsArrayHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  FontHandle : UBERPDFSDK_PDF_FONT_HANDLE;
  FontDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  ContentString : AnsiString;
  ContentStreamHandle : UBERPDFSDK_PDF_STREAM_HANDLE;
begin
  //----------------------------------------------------------------------------
  // Create a new letter sized page in landscape orientation from scratch using
  // only minor error checking (for clairity). UberPdfSdk_Pdf_Page_New_Ex() will
  // (if successfull) return a ResourceDictHandle to use to add fonts and images
  // to, and a ContentsArrayHandle for us to push streams of content to.
  //----------------------------------------------------------------------------
  MediaWidth    := 792; // in points (72 equals one inch)
  MediaHeight   := 612; // in points (72 equals one inch)
  MediaRotation := 0;   // 0, 90, 180, 270
  PageHandle := UberPdfSdk_Pdf_Page_New_Ex(UberPdfSdkInstanceH,
                                           @MediaWidth,
                                           @MediaHeight,
                                           @MediaRotation,
                                           @ResourceDictHandle,
                                           @ContentsArrayHandle);
  //----------------------------------------------------------------------------
  if (PageHandle = UBER_HANDLE_NULL) then begin
    result := UBER_HANDLE_NULL;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create a font entry to hold the fonts we use in the Page's Resource Dict
  //----------------------------------------------------------------------------
  FontDictHandle := UberPdfSdk_Pdf_Dict_AddKeyNewDict(ResourceDictHandle,
                                                      '/Font');
  //----------------------------------------------------------------------------
  // Create a document wide (available to the whole document) font(Times Roman)
  //----------------------------------------------------------------------------
  FontHandle := UberPdfSdk_Pdf_Doc_Fonts_Std_New(UberPdfSdkInstanceH,
                                                 UBERPDFSDK_PDF_DOC_FONTS_STD_TYPE_TIMES_ROMAN,
                                                 UBERPDFSDK_PDF_DOC_FONTS_ENCODING_TYPE_WIN_ANSI_ENCODING,
                                                 UBERPDFSDK_PDF_DICT_HANDLE_NULL);
  //----------------------------------------------------------------------------
  // Add a named reference to the Page's Resource Font Dict and free the font
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKey(FontDictHandle,
                             '/FontTimesRoman',
                             FontHandle);
  UberPdfSdk_Pdf_Doc_Fonts_Std_Free(FontHandle);
  //----------------------------------------------------------------------------
  // Create a document wide (available to the whole document) font (Helvetica Bold)
  //----------------------------------------------------------------------------
  FontHandle := UberPdfSdk_Pdf_Doc_Fonts_Std_New(UberPdfSdkInstanceH,
                                                 UBERPDFSDK_PDF_DOC_FONTS_STD_TYPE_HELVETICA_BOLD,
                                                 UBERPDFSDK_PDF_DOC_FONTS_ENCODING_TYPE_WIN_ANSI_ENCODING,
                                                 UBERPDFSDK_PDF_DICT_HANDLE_NULL);
  //----------------------------------------------------------------------------
  // Add a named reference to the Page's Resource Font Dict and free the font
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKey(FontDictHandle,
                             '/FontHelveticaBold',
                             FontHandle);
  UberPdfSdk_Pdf_Doc_Fonts_Std_Free(FontHandle);
  //----------------------------------------------------------------------------
  // Create a document wide (available to the whole document) font (Symbol)
  //----------------------------------------------------------------------------
  FontHandle := UberPdfSdk_Pdf_Doc_Fonts_Std_New(UberPdfSdkInstanceH,
                                                 UBERPDFSDK_PDF_DOC_FONTS_STD_TYPE_SYMBOL,
                                                 UBERPDFSDK_PDF_DOC_FONTS_ENCODING_TYPE_BUILT_IN,
                                                 UBERPDFSDK_PDF_DICT_HANDLE_NULL);
  //----------------------------------------------------------------------------
  // Add a named reference to the Page's Resource Font Dict and free the font
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKey(FontDictHandle,
                             '/FontSymbol',
                             FontHandle);
  UberPdfSdk_Pdf_Doc_Fonts_Std_Free(FontHandle);
  //----------------------------------------------------------------------------
  // Start our ContentStream with a 'gsave' (saves the graphic state)
  //----------------------------------------------------------------------------
  ContentString := 'q ';
  //----------------------------------------------------------------------------
  // We will output some text - Send the 'Begin Text' sequence
  //----------------------------------------------------------------------------
  ContentString := ContentString + 'BT ';
  //----------------------------------------------------------------------------
  // Use our FontTimesRoman with:
  //   a size of 40 points (72 equals one inch)
  //   normal font scaling and font rotation
  //   starting  36 points from page edge left    (72 equals one inch)
  //   starting 540 points from page edge bottom  (72 equals one inch)
  //   advance   48 points of leading if we line break (72 equals one inch)
  //----------------------------------------------------------------------------
  ContentString := ContentString + '/FontTimesRoman 40 Tf 1 0 0 1 36 540 Tm 48 TL ';
  //----------------------------------------------------------------------------
  // Set the color to RGB black for non stroking operations (like filled text)
  ContentString := ContentString + '0 0 0 rg ';
  //----------------------------------------------------------------------------
  // Output some text
  //----------------------------------------------------------------------------
  ContentString := ContentString + '(Hello World) Tj ';
  //----------------------------------------------------------------------------
  // Add the UberPDF fancy text example with:
  //   normal font scaling and font rotation
  //   starting 36 points from page edge left    (72 equals one inch)
  //   starting 36 points from page edge bottom  (72 equals one inch)
  //----------------------------------------------------------------------------
  ContentString := ContentString + '1 0 0 1 36 36 Tm ';
  Ex_HelloWorld_AddUberPDFFancyTextLine(ContentString);
  //----------------------------------------------------------------------------
  // We have finished outputting text - Send the 'End Text' sequence
  //----------------------------------------------------------------------------
  ContentString := ContentString + 'ET ';
  //----------------------------------------------------------------------------
  // Restore the graphic state by sending a 'grestore'
  //----------------------------------------------------------------------------
  ContentString := ContentString + 'Q'; //  we are done (no extra space needed)
  //----------------------------------------------------------------------------
  // Create a Content stream object from our ContentString
  //----------------------------------------------------------------------------
  ContentStreamHandle := UberPdfSdk_Pdf_Stream_NewFromString(UberPdfSdkInstanceH,
                                                             pAnsiChar(ContentString));
  //----------------------------------------------------------------------------
  // Push the ContentStream in to the ContentsArray
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Array_PushItem(ContentsArrayHandle,
                                ContentStreamHandle);
  //----------------------------------------------------------------------------
  // Clean up
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Stream_Free(ContentStreamHandle);
  UberPdfSdk_Pdf_Dict_Free(FontDictHandle);
  UberPdfSdk_Pdf_Dict_Free(ResourceDictHandle);
  UberPdfSdk_Pdf_Array_Free(ContentsArrayHandle);
  //----------------------------------------------------------------------------
  // return the page handle
  //----------------------------------------------------------------------------
  result := PageHandle;
  //----------------------------------------------------------------------------
end;

procedure TfrmMain.Ex_HelloWorld_AddUberPDFFancyTextLine(var ContentString: AnsiString);
begin
  //----------------------------------------------------------------------------
  // Output the string 'UberPDF(tm)sdk' at the current positon with:
  //   Uber in RGB blue (with RockDots)
  //   PDF(tm)sdk in RGB red
  // using special glyph positioning, sizing, and a text rise on the 'tm'
  //----------------------------------------------------------------------------
  ContentString := ContentString + '/FontHelveticaBold 40 Tf ';
  ContentString := ContentString + '0 0 1 rg ';
  ContentString := ContentString + '[ ';
  ContentString := ContentString +   '0 (';
  ContentString := ContentString +   #220;
  ContentString := ContentString +   ')';
  ContentString := ContentString +   '90 (b) ';
  ContentString := ContentString +   '27 (e) ';
  ContentString := ContentString +   '40 (r) ';
  ContentString := ContentString + '] TJ ';
  ContentString := ContentString + '1 0 0 rg ';
  ContentString := ContentString + '-1 Ts '; //TextRise
  ContentString := ContentString + '[ ';
  ContentString := ContentString +   '35 (P) ';
  ContentString := ContentString +   '70 (D) ';
  ContentString := ContentString +   '70 (F) ';
  ContentString := ContentString + '] TJ ';
  ContentString := ContentString + '/FontSymbol 20 Tf ';
  ContentString := ContentString + '14.125 Ts '; //TextRise
  ContentString := ContentString + '[ ';
  ContentString := ContentString +   '10 (';
  ContentString := ContentString +   #228;
  ContentString := ContentString +   ')';
  ContentString := ContentString + '] TJ ';
  ContentString := ContentString + '-1 Ts '; //TextRise
  ContentString := ContentString + '/FontHelveticaBold 30 Tf ';
  ContentString := ContentString + '[ ';
  ContentString := ContentString +   '600 (s) ';
  ContentString := ContentString +   '50 (d) ';
  ContentString := ContentString +   '50 (k) ';
  ContentString := ContentString + '] TJ ';
end;

end.


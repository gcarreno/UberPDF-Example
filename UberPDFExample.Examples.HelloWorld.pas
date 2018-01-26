unit UberPDFExample.Examples.HelloWorld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  UberPDFExample.Common,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

procedure CreatePdfDocument;
function CreatePdfPage(UberPdfSdkInstanceH: UBERPDFSDK_INSTANCE_HANDLE): UBERPDFSDK_PDF_PAGE_HANDLE;
procedure AddUberPDFFancyTextLine(var ContentString : AnsiString);

implementation

uses
  UberPDFExample.Forms.Main;

procedure CreatePdfDocument;
var
  UberPdfSdkSystemInitStruct : UBERPDFSDK_SYSTEM_INIT_STRUCT;
  UberPdfSdkInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  PageHandle : UBERPDFSDK_PDF_PAGE_HANDLE;
  FileName : AnsiString;
begin
  if (not(IsUberPDFSdkLoaded())) then begin
    frmMain.Log('UberPDFSdk is not loaded - Ex_HelloWorld_CreatePdfDocument() cannot proceeed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdk instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Initialising System');
  FillChar(UberPdfSdkSystemInitStruct,
           sizeof(UberPdfSdkSystemInitStruct),
           0);
  UberPdfSdkSystemInitStruct.StructSize := sizeof(UberPdfSdkSystemInitStruct);
  UberPdfSdkSystemInitStruct.StructVersion :=  1;
  UberPdfSdkInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Hello World: UberPdfSdk_System_Init() failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an empty Pdf document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Creating an empty PDF Document');
  if (UberPdfSdk_Pdf_Doc_CreateEmpty(UberPdfSdkInstanceH) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Hello World: UberPdfSdk_Pdf_Doc_CreateEmpty() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create a page
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Creating a page');
  PageHandle := CreatePdfPage(UberPdfSdkInstanceH);
  if (PageHandle = UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Hello World: CreatePdfPage failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Insert the page into the beginning of the document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Inserting the page into the beginning of the document');
  if (UberPdfSdk_Pdf_Doc_Pages_InsertPage(UberPdfSdkInstanceH,
                                          PageHandle,
                                          1) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Hello World: UberPdfSdk_Pdf_Doc_Pages_InsertPage() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the page
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Free-ing the page');
  if (UberPdfSdk_Pdf_Page_Free(PageHandle) <> UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Hello World: UberPdfSdk_Pdf_Page_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Save the document
  //----------------------------------------------------------------------------
  FileName := 'hello-world.pdf';
  frmMain.Log('Example Hello World: Saving the document "'+FileName+'"');
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
    frmMain.Log('Example Hello World: UberPdfSdk_Pdf_Doc_SaveToFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the UberPdfSdk instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: Free-ing the UberPdfSdk instance');
  if (UberPdfSdk_System_Free(UberPdfSdkInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Hello World: UberUberPdfSdk_System_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  frmMain.Log('Example Hello World: CreatePdfDocument() succeeded');
  frmMain.Log('');
end;

function CreatePdfPage(UberPdfSdkInstanceH: UBERPDFSDK_INSTANCE_HANDLE
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
  AddUberPDFFancyTextLine(ContentString);
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

procedure AddUberPDFFancyTextLine(var ContentString: AnsiString);
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

unit UberPDFExample.Examples.AddJPEG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntF,
  UberPDFExample.Common,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

procedure CreatePdfDocument;
function CreatePdfPage(UberPdfSdkInstanceH : UBERPDFSDK_INSTANCE_HANDLE) : UBERPDFSDK_PDF_PAGE_HANDLE;

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
  //----------------------------------------------------------------------------
  if (not(IsUberPDFSdkLoaded())) then begin
    frmMain.Log('Example Add JPEG: UberPDFSdk is not loaded - CreatePdfDocument() cannot proceeed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdk instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Initialising System');
  FillChar(UberPdfSdkSystemInitStruct,
           sizeof(UberPdfSdkSystemInitStruct),
           0);
  UberPdfSdkSystemInitStruct.StructSize := sizeof(UberPdfSdkSystemInitStruct);
  UberPdfSdkSystemInitStruct.StructVersion :=  1;
  UberPdfSdkInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Add JPEG: UberPdfSdk_System_Init() failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an empty Pdf document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Creating an empty PDF Document');
  if (UberPdfSdk_Pdf_Doc_CreateEmpty(UberPdfSdkInstanceH) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Add JPEG: UberPdfSdk_Pdf_Doc_CreateEmpty() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create a page by calling the CreatePdfPage() function below
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Creating a page');
  PageHandle := CreatePdfPage(UberPdfSdkInstanceH);
  if (PageHandle = UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Add JPEG: CreatePdfPage failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Insert the page into the beginning of the document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Inserting the page into the beginning of the document');
  if (UberPdfSdk_Pdf_Doc_Pages_InsertPage(UberPdfSdkInstanceH,
                                          PageHandle,
                                          1) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Add JPEG: UberPdfSdk_Pdf_Doc_Pages_InsertPage() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the page
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Free-ing the page');
  if (UberPdfSdk_Pdf_Page_Free(PageHandle) <> UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Add JPEG: UberPdfSdk_Pdf_Page_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Save the document
  //----------------------------------------------------------------------------
  FileName := 'add-jpeg.pdf';
  frmMain.Log('Example Add JPEG: Saving the document "'+FileName+'"');
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
    frmMain.Log('Example Add JPEG: UberPdfSdk_Pdf_Doc_SaveToFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  frmMain.Log('Example Add JPEG: Opening the document with the system default viewer');
  OpenDocument(FileName);
  //----------------------------------------------------------------------------
  // Free the UberPdfSdk instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: Free-ing the UberPdfSdk instance');
  if (UberPdfSdk_System_Free(UberPdfSdkInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Add JPEG: UberUberPdfSdk_System_Free() failed');
    UberPdfSdk_System_Free(UberPdfSdkInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  frmMain.Log('Example Add JPEG: CreatePdfDocument() succeeded');
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
  JpegFileName : AnsiString;
  JpegHandle : UBERPDFSDK_PDF_JPEG_HANDLE;
  XObjectDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  ContentString : AnsiString;
  ContentStreamHandle : UBERPDFSDK_PDF_STREAM_HANDLE;
begin
  //----------------------------------------------------------------------------
  // define where the jpeg file is located for this example
  //----------------------------------------------------------------------------
  {$IFDEF UBER_DEFINED_OS_PERSONA_WINDOWS_LIKE}
    JpegFileName := 'uber\3rdpartylibs\_testfiles\testfiles\jpg\uberdude-uberairshare-stage-board-01.jpg';
  {$ELSE}
    JpegFileName := './uber/3rdpartylibs/_testfiles/testfiles/jpg/uberdude-uberairshare-stage-board-01.jpg';
  {$ENDIF}
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
  // Add an XObject dict to the Page's Resource Dict to hold image and other resources
  //----------------------------------------------------------------------------
  XObjectDictHandle := UberPdfSdk_Pdf_Dict_AddKeyNewDict(ResourceDictHandle,
                                                         '/XObject');
  //----------------------------------------------------------------------------
  // Create a document wide (available to the whole document) jpeg
  //----------------------------------------------------------------------------
  JpegHandle := UberPdfSdk_Pdf_Doc_Images_Jpeg_NewFromFileA(UberPdfSdkInstanceH,
                                                            pAnsiChar(JpegFileName),
                                                            Length(JpegFileName),
                                                            0 // maximum jpeg filesize allowed to prevent explots (zero is no checking)
                                                           );
  if (JpegHandle = UBER_HANDLE_NULL) then begin
    UberPdfSdk_Pdf_Dict_Free(XObjectDictHandle);
    UberPdfSdk_Pdf_Dict_Free(ResourceDictHandle);
    UberPdfSdk_Pdf_Array_Free(ContentsArrayHandle);
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := UBER_HANDLE_NULL;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Add a name entry to the jpeg in the Page's XObject Dict called ImageUberDude
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKey(XObjectDictHandle,
                             '/ImageUberDude',
                             JpegHandle);
  //----------------------------------------------------------------------------
  // Start our ContentStream with a 'gsave' (saves the graphic state)
  //----------------------------------------------------------------------------
  ContentString := 'q ';
  //----------------------------------------------------------------------------
  // Output the jpeg:
  //   433 points wide (72 equals one inch)
  //   no Y rotation/skew
  //   no X rotation/skew
  //   300 points high (72 equals one inch)
  //   323 points from page edge left    (72 equals one inch)
  //   276 points from page edge bottom  (72 equals one inch)
  ContentString := ContentString + '433 0 0 300 323 276 cm /ImageUberDude Do ';
  //----------------------------------------------------------------------------
  // Restore the graphic state by sending a 'grestore'
  //----------------------------------------------------------------------------
  ContentString := ContentString + 'Q'; //  we are done (no extra space needed)
  //----------------------------------------------------------------------------
  // Create a Content stream object from the ContentString
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
  UberPdfSdk_Pdf_Doc_Images_Jpeg_Free(JpegHandle);
  UberPdfSdk_Pdf_Dict_Free(XObjectDictHandle);
  UberPdfSdk_Pdf_Dict_Free(ResourceDictHandle);
  UberPdfSdk_Pdf_Array_Free(ContentsArrayHandle);
  //----------------------------------------------------------------------------
  // return the page handle
  //----------------------------------------------------------------------------
  result := PageHandle;
  //----------------------------------------------------------------------------
end;

end.

unit UberPDFExample.Examples.StampedDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntF, Math,
  UberPDFExample.Common,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

procedure CreatePdfDocument;
function StampPage(UberPdfSdkDocumentInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
                   PageNumber : UBERPDFSDK_PDF_PAGE_NUMBER;
                   StampStreamHandle : UBERPDFSDK_PDF_STREAM_HANDLE;
                   StampWidth : UBER_FLOAT;
                   StampHeight : UBER_FLOAT) : integer;
function FloatToStrD(Value: Extended): string;


implementation

uses
  UberPDFExample.Forms.Main;

procedure CreatePdfDocument;
var
  UberPdfSdkSystemInitStruct : UBERPDFSDK_SYSTEM_INIT_STRUCT;
  UberPdfSdkDocumentToStampInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  DocumentToStampPageCount : UBERPDFSDK_PDF_PAGE_NUMBER;
  UberPdfSdkDocumentWithStampInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  StampPageHandle : UBERPDFSDK_PDF_PAGE_HANDLE;
  MediaBoxArrayHandle : UBERPDFSDK_PDF_ARRAY_HANDLE;
  StampRect : UBER_FLOAT_RECT;
  StampContentsHandle : UBERPDFSDK_PDF_OBJ_HANDLE;
  StampStreamHandle : UBERPDFSDK_PDF_STREAM_HANDLE;
  StampStreamDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  BBoxArrayHandle : UBERPDFSDK_PDF_ARRAY_HANDLE;
  MatrixArrayHandle : UBERPDFSDK_PDF_ARRAY_HANDLE;
  StampResourcesDictHandle : UBERPDFSDK_PDF_OBJ_HANDLE;
  StampPageIdx : UBERPDFSDK_PDF_PAGE_NUMBER;
  DocumentToStampFileNameIn : AnsiString;
  DocumentWithStampFileNameIn : AnsiString;
  FileNameOut : AnsiString;
//------------------------------------------------------------------------------
begin
  //----------------------------------------------------------------------------
  DocumentToStampFileNameIn   := 'PDF_files/DocumentToStamp.pdf';
  DocumentWithStampFileNameIn := 'PDF_files/PlatypusStamp.pdf';
  FileNameOut                 := 'stamped-document.pdf';
  //----------------------------------------------------------------------------
  if (not(IsUberPDFSdkLoaded())) then begin
    frmMain.Log('Example Stamped Document: UberPDFSdk is not loaded - CreatePdfDocument() cannot proceed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Fill out an UberPdfSdk UberPdfSdkSystemInitStruct
  //----------------------------------------------------------------------------
  FillChar(UberPdfSdkSystemInitStruct,
           sizeof(UberPdfSdkSystemInitStruct),
           0);
  UberPdfSdkSystemInitStruct.StructSize := sizeof(UberPdfSdkSystemInitStruct);
  UberPdfSdkSystemInitStruct.StructVersion :=  1;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdkDocumentToStampInstanceH
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Initializing System');
  UberPdfSdkDocumentToStampInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkDocumentToStampInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_System_Init(UberPdfSdkDocumentToStampInstanceH) failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Load the Pdf to UberPdfSdkDocumentToStampInstanceH
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Loading document to be stamped');
  if (UberPdfSdk_Pdf_Doc_LoadFromFileA(UberPdfSdkDocumentToStampInstanceH,
                                       pAnsiChar(DocumentToStampFileNameIn),
                                       Length(DocumentToStampFileNameIn),
                                       nil,  //lpPasswordA
                                       0,    //cchlpPasswordBufferSize
                                       UBERPDFSDK_PDF_DOC_LOAD_FLAGS_USE_DEFAULT) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_LoadFromFileA(UberPdfSdkDocumentToStampInstanceH) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get the count of pages from the UberPdfSdkDocumentToStampInstanceH and range check
  //------------------------------------------------------------------------------
  DocumentToStampPageCount := UberPdfSdk_Pdf_Doc_Pages_GetCount(UberPdfSdkDocumentToStampInstanceH);
  if (DocumentToStampPageCount < 1) then begin
    frmMain.Log('Example Stamped Document: DocumentToStamp has no pages - nothing to stamp');
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdk instance to import stamp from
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Creating an UberPdfSdk instance to import stamp from');
  UberPdfSdkDocumentWithStampInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkDocumentWithStampInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_System_Init(UberPdfSdkDocumentWithStampInstanceH) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Load the UberPdfSdkDocumentWithStampInstanceH with the pdf we will stamp with
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Loading UberPdfSdkDocumentWithStampInstanceH with the pdf we will stamp with');
  if (UberPdfSdk_Pdf_Doc_LoadFromFileA(UberPdfSdkDocumentWithStampInstanceH,
                                       pAnsiChar(DocumentWithStampFileNameIn),
                                       Length(DocumentWithStampFileNameIn),
                                       nil,  //lpPasswordA
                                       0,    //cchlpPasswordBufferSize
                                       UBERPDFSDK_PDF_DOC_LOAD_FLAGS_USE_DEFAULT) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_LoadFromFileA(UberPdfSdkDocumentWithStampInstanceH) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get the count of pages from the UberPdfSdkDocumentWithStampInstanceH and range check
  //------------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Doc_Pages_GetCount(UberPdfSdkDocumentWithStampInstanceH) < 1) then begin
    frmMain.Log('Example Stamped Document: DocumentWithStamp has no pages - nothing to stamp');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Import the first page from UberPdfSdkDocumentWithStampInstanceH as page one
  //------------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Doc_Pages_ImportPage(UberPdfSdkDocumentToStampInstanceH,
                                          UberPdfSdkDocumentWithStampInstanceH,
                                          1,
                                          0) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_Pages_ImportPage(UberPdfSdkDocumentWithStampInstanceH) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //------------------------------------------------------------------------------
  // Get a handle to page one (the stamp page)
  //------------------------------------------------------------------------------
  StampPageHandle := UberPdfSdk_Pdf_Doc_Pages_GetPage(UberPdfSdkDocumentToStampInstanceH,
                                                      1);
  if (StampPageHandle = UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_Pages_GetPage(1) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get a handle to the stamp page's MediaBox array (the un-cropped page size)
  //----------------------------------------------------------------------------
  MediaBoxArrayHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(StampPageHandle,
                                                         '/MediaBox');
  if (MediaBoxArrayHandle = UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: MediaBox dos not exist');
    UberPdfSdk_Pdf_Page_Free(StampPageHandle);
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get the stamp page's MediaBox as a rectangle (StampRect)
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Array_GetFloatRectValues(MediaBoxArrayHandle,
                                              @StampRect) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Stamped Document: MediaBox is invalid');
    UberPdfSdk_Pdf_Array_Free(MediaBoxArrayHandle);
    UberPdfSdk_Pdf_Page_Free(StampPageHandle);
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  UberPdfSdk_Pdf_Array_Free(MediaBoxArrayHandle);
  //----------------------------------------------------------------------------
  // Make sure the stamp page's MediaBox has a reasonable width and a height
  //----------------------------------------------------------------------------
  if ((StampRect.right < UBER_EPSILON) or
      (StampRect.top   < UBER_EPSILON)) then begin
    frmMain.Log('Example Stamped Document: MediaBox values are too small to calculate');
    UberPdfSdk_Pdf_Page_Free(StampPageHandle);
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Make sure the stamp page is not empty
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(StampPageHandle,
                                 '/Contents') = UBER_FALSE) then begin
    frmMain.Log('Example Stamped Document: DocumentWithStamp page 1 has no contents - nothing to stamp');
    UberPdfSdk_Pdf_Page_Free(StampPageHandle);
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Make a new stream to stamp the contents as an XObject/XForm
  //----------------------------------------------------------------------------
  // PDF Page contents can be a single stream or an array of streams. This
  // example uses the XObject/XForm method to stamp with, requiring a single
  // content stream. UberPdfSdk_Pdf_Stream_CatContents() takes in a contents obj
  // handle (to either a single stream or an array of streams) and returns a
  // single contents stream (compressed as specified) that we can use for the
  // XObject/XForm we will stamp pages with.
  //----------------------------------------------------------------------------
  StampContentsHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(StampPageHandle,
                                                         '/Contents');
  StampStreamHandle := UberPdfSdk_Pdf_Stream_CatContents(UberPdfSdkDocumentToStampInstanceH,
                                                         StampContentsHandle,
                                                         UBERPDFSDK_PDF_STREAM_COMPRESSION_TYPE_FLATE);
  if (StampStreamHandle = UBER_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Stream_CatContents() failed - DocumentWithStamp page 1  probably has no contents - nothing to stamp');
    UberPdfSdk_Pdf_Page_Free(StampContentsHandle);
    UberPdfSdk_Pdf_Page_Free(StampPageHandle);
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  UberPdfSdk_Pdf_Obj_Free(StampContentsHandle);
  //----------------------------------------------------------------------------
  // XObject/XForm types need entries for BBox, Matrix, Subtype, and Type. Also,
  // the Resources that the stamp page used must be copied to the XObject/XForm.
  // Get the StampStream's dictionary to add the entries and copy resources to.
  //----------------------------------------------------------------------------
  StampStreamDictHandle := UberPdfSdk_Pdf_Stream_GetDict(StampStreamHandle);
  //----------------------------------------------------------------------------
  // Add a BBox entry. This is simply the stamp page's former media box.
  //----------------------------------------------------------------------------
  BBoxArrayHandle := UberPdfSdk_Pdf_Dict_AddKeyNewArray(StampStreamDictHandle,
                                                        '/BBox');
  UberPdfSdk_Pdf_Array_PushNewFloatRectValues(BBoxArrayHandle,
                                              @StampRect);
  UberPdfSdk_Pdf_Array_Free(BBoxArrayHandle);
  //----------------------------------------------------------------------------
  // Add a Matrix entry. This is simply an Identity matrix (1:1 scale).
  //----------------------------------------------------------------------------
  MatrixArrayHandle := UberPdfSdk_Pdf_Dict_AddKeyNewArray(StampStreamDictHandle,
                                                          '/Matrix');
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           1);
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           0);
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           0);
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           1);
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           0);
  UberPdfSdk_Pdf_Array_PushNewIntegerValue(MatrixArrayHandle,
                                           0);
  UberPdfSdk_Pdf_Array_Free(MatrixArrayHandle);
  //----------------------------------------------------------------------------
  // If the stamp page has resources, create an indirect reference (a pointer)
  // to the Resources entry, then add the pointer to the StampStream dictionary.
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(StampPageHandle,
                                 '/Resources') <> UBER_FALSE) then begin
    StampResourcesDictHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(StampPageHandle,
                                                                '/Resources');
    if (UberPdfSdk_Pdf_Obj_IsDirect(StampResourcesDictHandle) <> UBER_FALSE) then begin
      UberPdfSdk_Pdf_Dict_MakeIndirect(StampResourcesDictHandle);
    end;
    UberPdfSdk_Pdf_Dict_AddKey(StampStreamDictHandle,
                               '/Resources',
                                StampResourcesDictHandle);
    UberPdfSdk_Pdf_Dict_Free(StampResourcesDictHandle);
  end;
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_Free(StampPageHandle);
  //----------------------------------------------------------------------------
  // Add the Subtype entry.
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKeyNewNameValue(StampStreamDictHandle,
                                         '/Subtype',
                                         '/Form');
  //----------------------------------------------------------------------------
  // Add the Type entry. This is an optional "best practices".
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_AddKeyNewNameValue(StampStreamDictHandle,
                                         '/Type',
                                         '/XObject');
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Dict_Free(StampStreamDictHandle);
  //----------------------------------------------------------------------------
  // The stamp page was added as page 1, followed by the DocumentToStamp pages.
  // Since we added the stamp page, there is one extra page.
  // Stamp document pages. Pass StampStreamHandle, StampWidth, and StampHeight.
  //----------------------------------------------------------------------------
  for StampPageIdx := 2 to DocumentToStampPageCount + 1 do begin
    if (StampPage(UberPdfSdkDocumentToStampInstanceH,
                  StampPageIdx,
                  StampStreamHandle,
                  StampRect.right,
                  StampRect.top) <> 0) then begin
      frmMain.Log('Example Stamped Document: StampPage() failed');
      UberPdfSdk_Pdf_Stream_Free(StampStreamHandle);
      UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
      UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
      exit;
    end;
    //--------------------------------------------------------------------------
  end;
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Stream_Free(StampStreamHandle);
  //----------------------------------------------------------------------------
  // Delete the first page (the page with the stamp).
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Doc_Pages_DeletePage(UberPdfSdkDocumentToStampInstanceH,
                                          1) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_Pages_DeletePage() failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Save the UberPdfSdkDocumentToStampInstanceH document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Saving the document "'+FileNameOut+'"');
  if (UberPdfSdk_Pdf_Doc_SaveToFileA(UberPdfSdkDocumentToStampInstanceH,
                                     pAnsiChar(FileNameOut),
                                     Length(FileNameOut),
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
    frmMain.Log('Example Stamped Document: UberPdfSdk_Pdf_Doc_SaveToFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  frmMain.Log('Example Stamped Document: Opening the document with the system default viewer');
  OpenDocument(FileNameOut);
  //----------------------------------------------------------------------------
  // Free the UberPdfSdkDocumentWithStampInstanceH instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Free-ing the document to be stamped instance');
  if (UberPdfSdk_System_Free(UberPdfSdkDocumentWithStampInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberUberPdfSdk_System_Free(Export) failed');
    UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the UberPdfSdkDocumentToStampInstanceH instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: Free-ing the document that stamps instance');
  if (UberPdfSdk_System_Free(UberPdfSdkDocumentToStampInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Stamped Document: UberUberPdfSdk_System_Free(Import) failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  frmMain.Log('Example Stamped Document: CreatePdfDocument() succeeded');
  //----------------------------------------------------------------------------
end;

function StampPage(UberPdfSdkDocumentInstanceH: UBERPDFSDK_INSTANCE_HANDLE;
  PageNumber: UBERPDFSDK_PDF_PAGE_NUMBER;
  StampStreamHandle: UBERPDFSDK_PDF_STREAM_HANDLE; StampWidth: UBER_FLOAT;
  StampHeight: UBER_FLOAT): integer;
//------------------------------------------------------------------------------
// Purpose: Stamp page using StampStreamHandle as an XObject/XForm object.
// Center stamp rotated along a line from page(lower,left) to page(upper, right)
// scaled to (BestFit(MediaWidth, MediaHeight) / StampCornerToCornerDistance).
// Returns: Zero on successful page stamp (or if page is already stamped), else
// traceable error.
// Notes: Parameter values are assumed to be pre-checked for validity.
//------------------------------------------------------------------------------
var
  PageHandle : UBERPDFSDK_PDF_PAGE_HANDLE;
  PageMediaBoxArrayHandle : UBERPDFSDK_PDF_ARRAY_HANDLE;
  PageMediaBoxFloatRect : UBER_FLOAT_RECT;
  MediaWidth  : UBER_FLOAT;
  MediaHeight : UBER_FLOAT;
  PageResourcesDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  PageResourcesXObjectDictHandle : UBERPDFSDK_PDF_DICT_HANDLE;
  ContentString : AnsiString;
  ContentsHandle : UBERPDFSDK_PDF_OBJ_HANDLE;
  ContentsArrayHandle : UBERPDFSDK_PDF_ARRAY_HANDLE;
  t : UBER_FLOAT;
  c : UBER_FLOAT;
  s : UBER_FLOAT;
  StampCornerToCornerDistance : UBER_FLOAT;
  ScaleStampToToBestFitPage : UBER_FLOAT_POINT;
  StampScale : UBER_FLOAT;
  ContentStreamHandle : UBERPDFSDK_PDF_STREAM_HANDLE;
//------------------------------------------------------------------------------
begin
  //----------------------------------------------------------------------------
  // Get the page to stamp
  //----------------------------------------------------------------------------
  PageHandle := UberPdfSdk_Pdf_Doc_Pages_GetPage(UberPdfSdkDocumentInstanceH,
                                                 PageNumber);
  if (PageHandle = UBER_HANDLE_NULL) then begin
    result := 1;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get the MediaBox of the page to stamp  (the un-cropped  page size)
  //----------------------------------------------------------------------------
  PageMediaBoxArrayHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(PageHandle,
                                                             '/MediaBox');
  if (PageMediaBoxArrayHandle = UBER_HANDLE_NULL) then begin
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 2;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Get the MediaBox values as a rectangle, assign MediaWidth and MediaHeight
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Array_GetFloatRectValues(PageMediaBoxArrayHandle,
                                              @PageMediaBoxFloatRect) <> UBER_STATUS_OK) then begin
    UberPdfSdk_Pdf_Array_Free(PageMediaBoxArrayHandle);
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 3;
    exit;
  end;
  MediaWidth  := PageMediaBoxFloatRect.right;
  MediaHeight := PageMediaBoxFloatRect.top;
  UberPdfSdk_Pdf_Array_Free(PageMediaBoxArrayHandle);
  //----------------------------------------------------------------------------
  // Make sure the Media size is valid
  //----------------------------------------------------------------------------
  if ((MediaWidth  < UBER_EPSILON) or
      (MediaHeight < UBER_EPSILON)) then begin
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 4;
    exit;
  end;
  //----------------------------------------------------------------------------
  // If the page to stamp does not have a Resources entry, then add one to hold
  // the stamp's XObject/XForm, else retrieve a handle to the existing Resources
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(PageHandle,
                                 '/Resources') = UBER_FALSE) then begin
    PageResourcesDictHandle := UberPdfSdk_Pdf_Dict_AddKeyNewDict(PageHandle,
                                                                 '/Resources');
  end else begin
    PageResourcesDictHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(PageHandle,
                                                              '/Resources');
  end;
  if (PageResourcesDictHandle = UBER_HANDLE_NULL) then begin
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 5;
    exit;
  end;
  //----------------------------------------------------------------------------
  // If the page to stamp Resources dictionary does not have a XObject entry,
  // then add one to hold the stamp XObject/XForm, else else retrieve a handle
  // to the existing XObject dictionary.
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(PageResourcesDictHandle,
                                 '/XObject') = UBER_FALSE) then begin
    PageResourcesXObjectDictHandle := UberPdfSdk_Pdf_Dict_AddKeyNewDict(PageResourcesDictHandle,
                                                                        '/XObject');
  end else begin
    PageResourcesXObjectDictHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(PageResourcesDictHandle,
                                                                      '/XObject');
  end;
  if (PageResourcesXObjectDictHandle = UBER_HANDLE_NULL) then begin
    UberPdfSdk_Pdf_Dict_Free(PageResourcesDictHandle);
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 6;
    exit;
  end;
  //----------------------------------------------------------------------------
  // If the XObject dictionary already has a "/PlatypusStamp" entry, we will
  // assume the page is already stamped and return "OK". Additional checks could
  // be made here to see if the page's content stream actually draws the stamp,
  // or optionally update or even remove the stamp.
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(PageResourcesXObjectDictHandle,
                                 '/PlatypusStamp') <> UBER_FALSE) then begin
    UberPdfSdk_Pdf_Dict_Free(PageResourcesXObjectDictHandle);
    UberPdfSdk_Pdf_Dict_Free(PageResourcesDictHandle);
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 0;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Add the StampStream named "/PlatypusStamp" to the page's XObject dictionary
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_AddKey(PageResourcesXObjectDictHandle,
                                 '/PlatypusStamp',
                                 StampStreamHandle) <> UBER_STATUS_OK) then begin
    UberPdfSdk_Pdf_Dict_Free(PageResourcesXObjectDictHandle);
    UberPdfSdk_Pdf_Dict_Free(PageResourcesDictHandle);
    UberPdfSdk_Pdf_Page_Free(PageHandle);
    result := 7;
    exit;
  end;
  //----------------------------------------------------------------------------
  // Initialize a string used to draw the stamp on the page to stamp.
  //----------------------------------------------------------------------------
  ContentString := '';
  //----------------------------------------------------------------------------
  // If the page has no contents, then make a content array container, else
  // if the page has a single content stream, replace contents entry with
  // an array containing that single content stream as it's first element, else
  // use existing contents array found. If contents are found, be kind to
  // other parsers by prefixing a wasteful whitespace character to the stamp
  // content so that the stamp's content will not bump up against the end of a
  // possibly existing token in the previous content.
  //----------------------------------------------------------------------------
  if (UberPdfSdk_Pdf_Dict_HasKey(PageHandle,
                                 '/Contents') = UBER_FALSE) then begin
    ContentsArrayHandle := UberPdfSdk_Pdf_Dict_AddKeyNewArray(PageHandle,
                                                              '/Contents');
  //----------------------------------------------------------------------------
  end else begin
  //----------------------------------------------------------------------------
    ContentString := ContentString + ' ';
    ContentsHandle := UberPdfSdk_Pdf_Dict_GetKeyValue(PageHandle,
                                                      '/Contents');
    if (UberPdfSdk_Pdf_Obj_IsArray(ContentsHandle) = UBER_FALSE) then begin
      // replace a single stream with an array containing the single stream
      ContentsArrayHandle := UberPdfSdk_Pdf_Dict_AddKeyNewArray(PageHandle,
                                                                '/Contents');
      UberPdfSdk_Pdf_Array_PushItem(ContentsArrayHandle,
                                    ContentsHandle);
    end else begin
      ContentsArrayHandle := ContentsHandle;
    end;
  //----------------------------------------------------------------------------
  end;
  //----------------------------------------------------------------------------
  // Mark the content to make it easily identifiable for possible later removal.
  // The beginning and end of the stamp drawing sequence will be marked.
  // The tag uses a "Third class" naming prefix as defined in Annex E.2 "Name
  // Registry" of the PDF ISO 32000-1 specification document. Further, there are
  // additional requirements and considerations for tags if saving as a tagged
  // or structured PDF file format (that are beyond the scope of this example).
  // Please refer to sections 7.3.5, 14.6, 14.7, and 14.8 of the PDF ISO 32000-1
  // 2008 documents (and addendum documents) for further information.
  //----------------------------------------------------------------------------
  ContentString := ContentString + '/XXPLATYPUSSTAMP_BEGIN MP ';
  //----------------------------------------------------------------------------
  // The stamp drawing sequence will be in the form of:
  // "gsave CenterToPage Rotate ScaleToFit CenterStampContents stamp grestore".
  //----------------------------------------------------------------------------
  // Start with a gsave (saves the graphics state)
  //----------------------------------------------------------------------------
  ContentString := ContentString + 'q ';
  //----------------------------------------------------------------------------
  // Move the drawing origin to the center of the page
  //----------------------------------------------------------------------------
  ContentString := ContentString +
                   '1 0 0 1 ' +
                   AnsiString(FloatToStrD(MediaWidth  * 0.50)) +
                   ' ' +
                   AnsiString(FloatToStrD(MediaHeight * 0.50)) +
                   ' cm ';
  //----------------------------------------------------------------------------
  // Calculate the values needed for the angle of the page corners (lower, left) to (upper, right)
  //----------------------------------------------------------------------------
  t := ArcTan2(MediaHeight,
               MediaWidth);
  c := cos(t);
  s := sin(t);
  //----------------------------------------------------------------------------
  // Rotate the drawing origin, rotating along a line from page(lower, left) to page(upper, right)
  //----------------------------------------------------------------------------
  ContentString := ContentString +
                   AnsiString(FloatToStrD(c)) +
                   ' ' +
                   AnsiString(FloatToStrD(s)) +
                   ' ' +
                   AnsiString(FloatToStrD(-s)) +
                   ' ' +
                   AnsiString(FloatToStrD(c)) +
                   ' 0 0 cm ';
  //----------------------------------------------------------------------------
  // Calculate the StampCornerToCornerDistance
  //----------------------------------------------------------------------------
  StampCornerToCornerDistance := sqrt((StampWidth * StampWidth) + (StampHeight * StampHeight));
  //----------------------------------------------------------------------------
  // Calculate BestFit(MediaWidth, MediaHeight) / StampCornerToCornerDistance)
  //----------------------------------------------------------------------------
  if (MediaWidth >= MediaHeight) then begin
    ScaleStampToToBestFitPage.x := MediaWidth  / StampCornerToCornerDistance;
    ScaleStampToToBestFitPage.y := MediaHeight / StampCornerToCornerDistance;
    if (ScaleStampToToBestFitPage.x >= ScaleStampToToBestFitPage.y) then begin
      ScaleStampToToBestFitPage.x := ScaleStampToToBestFitPage.y;
    end;
  end else begin
    ScaleStampToToBestFitPage.x := MediaWidth / StampCornerToCornerDistance;
  end;
  StampScale := ScaleStampToToBestFitPage.x;
  //----------------------------------------------------------------------------
  // Scale any drawing we do by StampScale in both the x and y directions
  //----------------------------------------------------------------------------
  ContentString := ContentString +
                   AnsiString(FloatToStrD(StampScale)) +
                   ' 0 0 ' +
                   AnsiString(FloatToStrD(StampScale)) +
                   ' 0 0 cm ';
  //----------------------------------------------------------------------------
  // Move the drawing origin by minus half stamp size (to center it)
  //----------------------------------------------------------------------------
  ContentString := ContentString + '1 0 0 1 ' +
                                    AnsiString(FloatToStrD(-StampWidth  * 0.50)) +
                                    ' ' +
                                    AnsiString(FloatToStrD(-StampHeight * 0.50)) +
                                    ' cm ';
  //----------------------------------------------------------------------------
  // Draw the stamp and grestore
  //----------------------------------------------------------------------------
  ContentString := ContentString + '/PlatypusStamp Do Q ';
  //----------------------------------------------------------------------------
  // Mark the content to make it easily identifiable for possible later removal.
  //----------------------------------------------------------------------------
  ContentString := ContentString + '/XXPLATYPUSSTAMP_END MP';
  //----------------------------------------------------------------------------
  // Create a Content stream object from the ContentString
  //----------------------------------------------------------------------------
  ContentStreamHandle := UberPdfSdk_Pdf_Stream_NewFromString(UberPdfSdkDocumentInstanceH,
                                                             pAnsiChar(ContentString));
  //----------------------------------------------------------------------------
  // Push the ContentStream in to the page to stamp's ContentsArray
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Array_PushItem(ContentsArrayHandle,
                                ContentStreamHandle);
  //----------------------------------------------------------------------------
  // Clean up
  //----------------------------------------------------------------------------
  UberPdfSdk_Pdf_Stream_Free(ContentStreamHandle);
  UberPdfSdk_Pdf_Array_Free(ContentsArrayHandle);
  UberPdfSdk_Pdf_Dict_Free(PageResourcesXObjectDictHandle);
  UberPdfSdk_Pdf_Dict_Free(PageResourcesDictHandle);
  UberPdfSdk_Pdf_Page_Free(PageHandle);
  //----------------------------------------------------------------------------
  result := 0;
  //----------------------------------------------------------------------------
end;

function FloatToStrD(Value: Extended): string;
var
  lastDecimalSeparator : char;
//------------------------------------------------------------------------------
begin
  {$IFDEF UBER_DEFINED_COMPILER_FPC}
    // remove deprecation warning
    {$warnings off}
      LastDecimalSeparator := DecimalSeparator;
      DecimalSeparator := '.';
      result := FloatToStr(Value);
      DecimalSeparator := LastDecimalSeparator;
    {$warnings on}
  {$ELSE}
    {$IFDEF UBER_DEFINED_COMPILER_DELPHI_VERSION_XE_OR_ABOVE}
      LastDecimalSeparator := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := '.';
      result := FloatToStr(Value);
      FormatSettings.DecimalSeparator := LastDecimalSeparator;
    {$ELSE}
      LastDecimalSeparator := DecimalSeparator;
      DecimalSeparator := '.';
      result := FloatToStr(Value);
      DecimalSeparator := LastDecimalSeparator;
    {$ENDIF}
  {$ENDIF}
end;

end.


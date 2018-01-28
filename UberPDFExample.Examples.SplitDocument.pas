unit UberPDFExample.Examples.SplitDocument;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// include the file to let uber figure out what compiler and version is used
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_compiler_version_defs.pascal-inc'}
//------------------------------------------------------------------------------
// AFTER THE "USES" CLAUSE BELOW YOU MUST DEFINE THE PATH TO THE UBER_PDFSDK_SO
// using the const UBER_PDFSDK_SO_NAME = 'PATH_TO_UBER_PDFSDK_DYN_LIB';
// (see below).
//------------------------------------------------------------------------------

uses
  Classes, SysUtils, LCLIntF,
  UberPDFExample.Common,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

//------------------------------------------------------------------------------
// Include the Uber base types
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_defs.pascal-inc'}
//------------------------------------------------------------------------------

procedure CreatePdfDocument;

implementation

uses
  UberPDFExample.Forms.Main;

procedure CreatePdfDocument;
var
  UberPdfSdkSystemInitStruct : UBERPDFSDK_SYSTEM_INIT_STRUCT;
  UberPdfSdkImportFromInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  UberPdfSdkExportToInstanceH : UBERPDFSDK_INSTANCE_HANDLE;
  importFromPageCount : UBERPDFSDK_PDF_PAGE_NUMBER;
  importFromPageIdx : UBERPDFSDK_PDF_PAGE_NUMBER;
  exportToPageIdx : UBERPDFSDK_PDF_PAGE_NUMBER;
  FileNameIn : AnsiString;
  FileNameOut : AnsiString;
//------------------------------------------------------------------------------
begin
  //----------------------------------------------------------------------------
  FileNameIn  := 'PDF_files/NASA_Solar_Dynamics_Observatory_PressKit.pdf';
  FileNameOut := 'split-document.pdf';
  //----------------------------------------------------------------------------
  if (not(IsUberPDFSdkLoaded())) then begin
    frmMain.Log('Example Split Document: UberPDFSdk is not loaded - CreatePdfDocument() cannot proceed');
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
  // Create an UberPdfSdk instance to import from
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Initialising System');
  UberPdfSdkImportFromInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkImportFromInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Split Document: UberPdfSdk_System_Init(Import) failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Load the Pdf to UberPdfSdkInstanceHImportFrom
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Loading PDF "'+FileNameIn+'"');
  if (UberPdfSdk_Pdf_Doc_LoadFromFileA(UberPdfSdkImportFromInstanceH,
                                       pAnsiChar(FileNameIn),
                                       Length(FileNameIn),
                                       nil,  //lpPasswordA
                                       0,    //cchlpPasswordBufferSize
                                       UBERPDFSDK_PDF_DOC_LOAD_FLAGS_USE_DEFAULT) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Split Document: UberPdfSdk_Pdf_Doc_LoadFromFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    exit;
  end;
  //------------------------------------------------------------------------------
  // Get the count of pages from the Import From document and range check
  //------------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Getting page count');
  importFromPageCount := UberPdfSdk_Pdf_Doc_Pages_GetCount(UberPdfSdkImportFromInstanceH);
  if (importFromPageCount < 20) then begin
    frmMain.Log('Example Split Document: importFromPageCount < 20 failed');
    UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Create an UberPdfSdk instance to export to
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Creating instance of UberPdfSdk for export');
  UberPdfSdkExportToInstanceH := UberPdfSdk_System_Init(@UberPdfSdkSystemInitStruct);
  if (UberPdfSdkExportToInstanceH = UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Split Document: UberPdfSdk_System_Init(Export) failed');
    UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  // Initialize the UberPdfSdkExportToInstanceH with an empty Pdf document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Initilising empty document');
  if (UberPdfSdk_Pdf_Doc_CreateEmpty(UberPdfSdkExportToInstanceH) <> UBER_STATUS_OK) then begin
    frmMain.Log('Example Split Document: UberPdfSdk_Pdf_Doc_CreateEmpty() failed');
    UberPdfSdk_System_Free(UberPdfSdkExportToInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    exit;
  end;
  //------------------------------------------------------------------------------
  // import 10 pages, adding them to the end of the new document
  //------------------------------------------------------------------------------
  exportToPageIdx := 1; // number greater than the page count adds to the end of the document
  for importFromPageIdx := 1 to 10 do begin
    if (UberPdfSdk_Pdf_Doc_Pages_ImportPage(UberPdfSdkExportToInstanceH,
                                            UberPdfSdkImportFromInstanceH,
                                            importFromPageIdx,
                                            exportToPageIdx) <> UBER_STATUS_OK) then begin
      frmMain.Log('Example Split Document: UberPdfSdk_Pdf_Doc_Pages_ImportPage() failed');
      UberPdfSdk_System_Free(UberPdfSdkExportToInstanceH);
      UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    end;
    inc(exportToPageIdx);
  end;
  //----------------------------------------------------------------------------
  // Save the UberPdfSdkExportToInstanceH document
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Saving the document "'+FileNameOut+'"');
  if (UberPdfSdk_Pdf_Doc_SaveToFileA(UberPdfSdkExportToInstanceH,
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
    frmMain.Log('Example Split Document: UberPdfSdk_Pdf_Doc_SaveToFileA() failed');
    UberPdfSdk_System_Free(UberPdfSdkExportToInstanceH);
    UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH);
    exit;
  end;
  frmMain.Log('Example Split Document: Opening the document with the system default viewer');
  OpenDocument(FileNameOut);
  //----------------------------------------------------------------------------
  // Free the UberPdfSdkExportToInstanceH instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Free-ing Export to instance');
  if (UberPdfSdk_System_Free(UberPdfSdkExportToInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Split Document: UberUberPdfSdk_System_Free(Export) failed');
    exit;
  end;
  //----------------------------------------------------------------------------
  // Free the UberPdfSdkImportFromInstanceH instance
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: Free-ing Import from instance');
  if (UberPdfSdk_System_Free(UberPdfSdkImportFromInstanceH) <> UBERPDFSDK_INSTANCE_HANDLE_NULL) then begin
    frmMain.Log('Example Split Document: UberUberPdfSdk_System_Free(Import) failed');
    UberPdfSdk_System_Free(UberPdfSdkExportToInstanceH);
    exit;
  end;
  //----------------------------------------------------------------------------
  frmMain.Log('Example Split Document: CreatePdfDocument() succeeded');
  frmMain.Log('');
end;

end.

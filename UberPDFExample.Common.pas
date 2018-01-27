unit UberPDFExample.Common;

{$mode objfpc}{$H+}

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
  Classes, SysUtils
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  ,libuberpdfsdkdyn_pascal;
{$ELSE}
  ;
{$ENDIF}

//------------------------------------------------------------------------------
// Include the Uber base types
//------------------------------------------------------------------------------
{$INCLUDE 'uber_base_unit_defs.pascal-inc'}
//------------------------------------------------------------------------------

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

function LoadUberPDFSdk: Boolean;
function UnLoadUberPDFSdk: Boolean;
function IsUberPDFSdkLoaded: Boolean;

implementation

{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
uses
  UberPDFExample.Forms.Main;
{$ENDIF}

function LoadUberPDFSdk: Boolean;
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
var
  FileName : AnsiString;
{$ENDIF}
begin
// Load the DLL (if we are dynamic linking) else return true
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  frmMain.Log('Loading Dynamic Library');
  FileName := UBER_PDFSDK_SO_NAME;
  Result := (UberPdfSdkDynLibLoad(pAnsiChar(FileName)) <> UBER_FALSE);
  exit;
{$ENDIF}
  Result := True;
end;

function UnLoadUberPDFSdk: Boolean;
begin
// unload the DLL (if we are dynamic linking) else return true
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  frmMain.Log('Unloading Dynamic Library');
  Result := (UberPdfSdkDynLibUnload() <> UBER_FALSE);
  exit;
{$ENDIF}
  result := True;
end;

function IsUberPDFSdkLoaded: Boolean;
begin
// test if the DLL is loaded (if we are dynamic linking) else return true
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  Result := (IsUberPdfSdkDynLibLoaded() <> UBER_FALSE);
  exit;
{$ENDIF}
  Result := True;
end;

end.

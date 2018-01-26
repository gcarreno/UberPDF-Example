unit UberPDFExample.Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF UBER_PDFSDK_USE_DYNAMIC_LIB_LOADER}
  libuberpdfsdkdyn_pascal;
{$ELSE}
  uberpdfsdk_pascal;
{$ENDIF}

function LoadUberPDFSdk: Boolean;
function UnLoadUberPDFSdk: Boolean;
function IsUberPDFSdkLoaded: Boolean;

implementation

uses
  UberPDFExample.Forms.Main;

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

unit jsdosbox;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, Web, Classes;

type
  TDOSMain = reference to function(params: array of string): TJSPromise;

  TDOSFileSystem = class(TJSObject)
  Public
    function extract(ZipFile, MountPoint: string): TJSPromise; external name 'extract';
  end;

  TDOSBoxReady = reference to procedure(FileSystem: TDOSFileSystem; Main: TDOSMain);

  TJSDOSBox = class external name 'Dos' (TJSObject)
  Public
    constructor new(canvas: TJSHTMLCanvasElement);
    procedure ready(callback: TDOSBoxReady); external name 'ready';
  end;

  { TDOSBox }

  TDOSBox = class(TComponent)
  Private
    FApp: TJSDOSBox;
    FZipFile, FEXE, FMP: string;
    FFS: TDOSFileSystem;
    FMain: TDOSMain;
    procedure ExtractApp(AFS: TDOSFileSystem; AMain: TDOSMain);
    function StartApp(AValue: JSValue): JSValue;
    function AppRunning(AValue: JSValue): JSValue;
  Public
    constructor Create(AOwner: TComponent; App, Exe, Path: string);
  end;

implementation

{ TDOSBox }

procedure TDOSBox.ExtractApp(AFS: TDOSFileSystem; AMain: TDOSMain);
var
  e: TJSHTMLElement;
  p: TJSPromise;
begin
  FFS:=AFS;
  FMain:=AMain;
  e:=TJSHTMLElement(document.getElementsByClassName('dosbox-container').Items[0]);
  e.style.setProperty('display', 'inline');
  p:=FFS.extract(FZipFile, FMP);
  p._then(@StartApp);
end;

function TDOSBox.StartApp(AValue: JSValue): JSValue;
var
  p: TJSPromise;
begin
  if FEXE <> '' then
    p:=FMain(['-c', FEXE])
  else
    p:=FMain([]);
  p._then(@AppRunning);
end;

function TDOSBox.AppRunning(AValue: JSValue): JSValue;
begin
  {WriteLn('Looks like it is all up and running now!');}
end;

constructor TDOSBox.Create(AOwner: TComponent; App, Exe, Path: string);
begin
  inherited Create(AOwner);
  FZipFile:='/'+App+'.zip?dl=y';
  FEXE:=Exe;
  FMP:=Path;
  FApp:=TJSDOSBox.new(TJSHTMLCanvasElement(document.getElementById('dosapp')));
  FApp.ready(@ExtractApp);
end;

end.


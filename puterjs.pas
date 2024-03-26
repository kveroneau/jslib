unit puterjs;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web;

type
  TPuterFSItem = Class external name 'FSItem' (TJSObject)
  public
    name, path, content: string;
    size: Integer;
    created: TJSDate;
    isDirectory: Boolean;
    function read: TJSPromise external name 'read';
  end;

  TPuterDirList = array of TPuterFSItem;

  TPuterUserInfo = class(TJSObject)
  public
    uuid, username: string;
    email_confirmed: boolean;
  end;

  TPuterErrorMsg = class(TJSObject)
  public
    message, code: string;
  end;

  TPuterWindowOptions = Class external name 'Object' (TJSObject)
  public
    center, disable_parent_window, has_head, is_resizable, show_in_taskbar: boolean;
    content, title, uri: string;
    height, width: Integer;
  end;

  TPuterWebsite = class(TJSObject)
  public
    id, subdomain: string;
    created_on: TJSDate;
  end;

  TPuterHostList = Array of TPuterWebsite;

  TLaunchCallback = reference to procedure(AItems: TJSArray);

  TPuterFS = class(TJSObject)
  public
    function write(AFile, AData: string): TJSPromise external name 'write';
    function read(AFile: string): TJSPromise external name 'read';
    function mkdir(APath: string): TJSPromise external name 'mkdir';
    function readdir(APath: string): TJSPromise external name 'readdir';
    function rename(APath, ANewName: string): TJSPromise external name 'rename';
    function copy(ASource, ADest: string): TJSPromise external name 'copy';
    function move(ASource, ADest: string): TJSPromise external name 'move';
    function stat(AFilePath: string): TJSPromise external name 'stat';
    function delete(APath: string): TJSPromise external name 'delete';
    function upload(AFileList: TJSHTMLFileList): TJSPromise external name 'upload';
  end;

  TPuterKeyValue = class(TJSObject)
  public
    function _set(AKey, AValue: string): TJSPromise external name 'set';
    function _set(AKey: string; AValue: integer): TJSPromise external name 'set';
    function get(AKey: string): TJSPromise external name 'get';
    function incr(AKey: string): TJSPromise external name 'incr';
    function incr(AKey: string; AAmount: integer): TJSPromise external name 'incr';
    function decr(AKey: string): TJSPromise external name 'decr';
    function decr(AKey: string; AAmount: integer): TJSPromise external name 'decr';
    function del(AKey: string): TJSPromise external name 'del';
    function list: TJSPromise external name 'list';
    function list(APattern: string): TJSPromise external name 'list';
    function flush: TJSPromise external name 'flush';
  end;

  TPuterHosting = class(TJSObject)
  public
    function create(ASubDomain, ADirPath: string): TJSPromise external name 'create';
    function list: TJSPromise external name 'list';
    function delete(ASubDomain: string): TJSPromise external name 'delete';
    function update(ASubDomain, ADirPath: string): TJSPromise external name 'update';
    function get(ASubDomain: string): TJSPromise external name 'get';
  end;

  TPuterAuth = class(TJSObject)
  public
    function signIn: TJSPromise external name 'signIn';
    function signOut: TJSPromise external name 'signOut';
    function isSignedIn: Boolean external name 'isSignedIn';
    function getUser: TJSPromise external name 'getUser';
  end;

  { TPuterUI }

  TPuterUI = class(TJSObject)
  public
    env: string; external name 'env';
    function authenticateWithPuter: TJSPromise external name 'authenticateWithPuter';
    function alert(AMessage: string): TJSPromise external name 'alert';
    function alert(AMessage: string; AButtons: TJSArray): TJSPromise external name 'alert';
    function prompt(AMessage: string): TJSPromise external name 'prompt';
    function prompt(AMessage, APlaceHolder: string): TJSPromise external name 'prompt';
    procedure createWindow(options: TPuterWindowOptions) external name 'createWindow';
    function launchApp: TJSPromise external name 'launchApp';
    function launchApp(appName: string): TJSPromise external name 'launchApp';
    function launchApp(appName: string; args: TJSObject): TJSPromise external name 'launchApp';
    function launchApp(args: TJSObject): TJSPromise external name 'launchApp';
    procedure onLaunchWithItems(ACallback: TLaunchCallback) external name 'onLaunchWithItems';
    function showOpenFilePicker: TJSPromise external name 'showOpenFilePicker';
    function showSaveFilePicker(data: string): TJSPromise external name 'showSaveFilePicker';
    function showDirectoryPicker: TJSPromise external name 'showDirectoryPicker';
    function showColorPicker: TJSPromise external name 'showColorPicker';
    function showColorPicker(DefaultColor: string): TJSPromise external name 'showColorPicker';
    function showFontPicker: TJSPromise external name 'showFontPicker';
    function showFontPicker(DefaultFont: string): TJSPromise external name 'showFontPicker';
    function setWindowTitle(ATitle: string): TJSPromise external name 'setWindowTitle';
    procedure setWindowHeight(AHeight: integer) external name 'setWindowHeight';
    procedure setWindowWidth(AWidth: integer) external name 'setWindowWidth';
    procedure setWindowSize(AWidth, AHeight: integer) external name 'setWindowSize';
  end;

  TPuterJS = class(TJSObject)
  public
    fs: TPuterFS;
    kv: TPuterKeyValue;
    hosting: TPuterHosting;
    auth: TPuterAuth;
    ui: TPuterUI;
    args: TJSObject;
    function randName: string external name 'randName';
    procedure exit external name 'exit';
  end;

  TPuterErrorEvent = reference to procedure(AError: TPuterErrorMsg);
  TPuterFileEvent = reference to procedure(AFile: TPuterFSItem);
  TReadFileEvent = reference to procedure(AContent: string);
  TPuterDirectoryEvent = reference to procedure(ADir: TPuterFSItem);
  TDirListEvent = reference to procedure(ADirList: TPuterDirList);
  TUploadEvent = reference to procedure(AFileList: TPuterFSItem);
  TPuterAuthEvent = reference to procedure;
  TPuterAlertEvent = reference to procedure(ALabel: string);
  TPuterAppLaunchEvent = reference to procedure;
  TColorEvent = reference to procedure(AColor: string);
  TFontEvent = reference to procedure(AFont: string);

  { TPuter }

  TPuter = class(TComponent)
  private
    FOnLaunchWithItems: TLaunchCallback;
    FOnPuterError: TPuterErrorEvent;
    FOnWriteSuccess: TPuterFileEvent;
    FOnReadSuccess: TReadFileEvent;
    FOnDirSuccess: TPuterDirectoryEvent;
    FOnDirListSuccess: TDirListEvent;
    FOnRenameSuccess: TPuterFileEvent;
    FOnCopySuccess: TPuterFileEvent;
    FOnMoveSuccess: TPuterFileEvent;
    FOnStatSuccess: TPuterFileEvent;
    FOnUploadSuccess: TUploadEvent;
    FOnOpenFileSuccess: TPuterFileEvent;
    FOnAuthSuccess: TPuterAuthEvent;
    FOnAlertButton: TPuterAlertEvent;
    FOnAppLaunch: TPuterAppLaunchEvent;
    FOnColorSuccess: TColorEvent;
    FOnFontSuccess: TFontEvent;
    FWindowHeight: integer;
    FWindowTitle: string;
    FWindowWidth: integer;
    procedure SetOnLaunchWithItems(AValue: TLaunchCallback);
    procedure SetWindowHeight(AValue: integer);
    procedure SetWindowTitle(AValue: string);
    procedure SetWindowWidth(AValue: integer);
    function WriteSuccess(AValue: JSValue): JSValue;
    function PuterError(AValue: JSValue): JSValue;
    function ReadSuccess(AValue: JSValue): JSValue;
    function MkdirSuccess(AValue: JSValue): JSValue;
    function ReadDirSuccess(AValue: JSValue): JSValue;
    function RenameSuccess(AValue: JSValue): JSValue;
    function CopySuccess(AValue: JSValue): JSValue;
    function MoveSuccess(AValue: JSValue): JSValue;
    function StatSuccess(AValue: JSValue): JSValue;
    function UploadSuccess(AValue: JSValue): JSValue;
    procedure ReadFile(f: TPuterFSItem); async;
    function OpenFile(AValue: JSValue): JSValue;
    function SaveFile(AValue: JSValue): JSValue;
    function AuthSuccess(AValue: JSValue): JSValue;
    function AlertSuccess(AValue: JSValue): JSValue;
    function AppLaunchSuccess(AValue: JSValue): JSValue;
    function ColorSuccess(AValue: JSValue): JSValue;
    function FontSuccess(AValue: JSValue): JSValue;
  public
    property OnPuterError: TPuterErrorEvent read FOnPuterError write FOnPuterError;
    property OnWriteSuccess: TPuterFileEvent read FOnWriteSuccess write FOnWriteSuccess;
    property OnReadSuccess: TReadFileEvent read FOnReadSuccess write FOnReadSuccess;
    property OnDirSuccess: TPuterDirectoryEvent read FOnDirSuccess write FOnDirSuccess;
    property OnDirListSuccess: TDirListEvent read FOnDirListSuccess write FOnDirListSuccess;
    property OnRenameSuccess: TPuterFileEvent read FOnRenameSuccess write FOnRenameSuccess;
    property OnCopySuccess: TPuterFileEvent read FOnCopySuccess write FOnCopySuccess;
    property OnMoveSuccess: TPuterFileEvent read FOnMoveSuccess write FOnWriteSuccess;
    property OnStatSuccess: TPuterFileEvent read FOnStatSuccess write FOnStatSuccess;
    property OnUploadSuccess: TUploadEvent read FOnUploadSuccess write FOnUploadSuccess;
    property OnOpenFileSuccess: TPuterFileEvent read FOnOpenFileSuccess write FOnOpenFileSuccess;
    property OnAuthSuccess: TPuterAuthEvent read FOnAuthSuccess write FOnAuthSuccess;
    property OnAlertButton: TPuterAlertEvent read FOnAlertButton write FOnAlertButton;
    property OnAppLaunch: TPuterAppLaunchEvent read FOnAppLaunch write FOnAppLaunch;
    property OnLaunchWithItems: TLaunchCallback read FOnLaunchWithItems write SetOnLaunchWithItems;
    property OnColorSuccess: TColorEvent read FOnColorSuccess write FOnColorSuccess;
    property OnFontSuccess: TFontEvent read FOnFontSuccess write FOnFontSuccess;
    property WindowTitle: string read FWindowTitle write SetWindowTitle;
    property WindowHeight: integer read FWindowHeight write SetWindowHeight;
    property WindowWidth: integer read FWindowWidth write SetWindowWidth;
    procedure WriteFile(AFile, AData: string);
    procedure ReadFile(AFile: string); async;
    procedure MakeDirectory(ADir: string);
    procedure GetDirectory(ADir: string);
    procedure RenameFile(AFile, ANewName: string);
    procedure CopyFile(AFile, ADestDir: string);
    procedure MoveFile(AFile, ADestDir: string);
    procedure StatFile(AFile: string);
    procedure DeleteFile(AFile: string); async;
    procedure UploadFile(AFileList: TJSHTMLFileList);
    procedure OpenFileDialog;
    procedure SaveFileDialog(AData: string);
    procedure AuthenticateWithPuter;
    procedure Alert(AMessage: string);
    procedure Alert(AMessage: string; AButtons: TJSArray);
    procedure Prompt(AMessage: string);
    procedure Prompt(AMessage, APlaceHolder: string);
    procedure CreateWindow(AOptions: TPuterWindowOptions);
    procedure Exit;
    procedure LaunchApp;
    procedure LaunchApp(AName: string);
    procedure ColorPicker;
    procedure ColorPicker(DefaultColor: string);
    procedure FontPicker;
    procedure FontPicker(DefaultFont: string);
  end;

var
  PuterAPI: TPuterJS; external name 'window.puter';
  Puter: TPuter;

implementation

{ TPuterUI }

{ TPuter }

function TPuter.WriteSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnWriteSuccess) then
    FOnWriteSuccess(TPuterFSItem(AValue));
end;

procedure TPuter.SetOnLaunchWithItems(AValue: TLaunchCallback);
begin
  if FOnLaunchWithItems=AValue then Exit;
  PuterAPI.ui.onLaunchWithItems(AValue);
  FOnLaunchWithItems:=AValue;
end;

procedure TPuter.SetWindowHeight(AValue: integer);
begin
  if FWindowHeight=AValue then Exit;
  PuterAPI.ui.setWindowHeight(AValue);
  FWindowHeight:=AValue;
end;

procedure TPuter.SetWindowTitle(AValue: string);
begin
  if FWindowTitle=AValue then Exit;
  PuterAPI.ui.setWindowTitle(AValue);
  FWindowTitle:=AValue;
end;

procedure TPuter.SetWindowWidth(AValue: integer);
begin
  if FWindowWidth=AValue then Exit;
  PuterAPI.ui.setWindowWidth(AValue);
  FWindowWidth:=AValue;
end;

function TPuter.PuterError(AValue: JSValue): JSValue;
begin
  if Assigned(FOnPuterError) then
    FOnPuterError(TPuterErrorMsg(AValue));
end;

function TPuter.ReadSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnReadSuccess) then
    FOnReadSuccess(String(AValue));
end;

function TPuter.MkdirSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnDirSuccess) then
    FOnDirSuccess(TPuterFSItem(AValue));
end;

function TPuter.ReadDirSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnDirListSuccess) then
    FOnDirListSuccess(TPuterDirList(AValue));
end;

function TPuter.RenameSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnRenameSuccess) then
    FOnRenameSuccess(TPuterFSItem(AValue));
end;

function TPuter.CopySuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnCopySuccess) then
    FOnCopySuccess(TPuterFSItem(AValue));
end;

function TPuter.MoveSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnMoveSuccess) then
    FOnMoveSuccess(TPuterFSItem(AValue));
end;

function TPuter.StatSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnStatSuccess) then
    FOnStatSuccess(TPuterFSItem(AValue));
end;

function TPuter.UploadSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnUploadSuccess) then
    FOnUploadSuccess(TPuterFSItem(AValue));
end;

procedure TPuter.ReadFile(f: TPuterFSItem);
var
  b: TJSBlob;
begin
  if Assigned(FOnOpenFileSuccess) then
  begin
    b:=AWait(TJSBlob, f.read);
    f.content:=AWait(String, b.text);
    FOnOpenFileSuccess(f);
  end;
end;

function TPuter.OpenFile(AValue: JSValue): JSValue;
begin
  ReadFile(TPuterFSItem(AValue));
end;

function TPuter.SaveFile(AValue: JSValue): JSValue;
begin
  if Assigned(FOnWriteSuccess) then
    FOnWriteSuccess(TPuterFSItem(AValue));
end;

function TPuter.AuthSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnAuthSuccess) then
    FOnAuthSuccess;
end;

function TPuter.AlertSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnAlertButton) then
    FOnAlertButton(String(AValue));
end;

function TPuter.AppLaunchSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnAppLaunch) then
    FOnAppLaunch;
end;

function TPuter.ColorSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnColorSuccess) then
    FOnColorSuccess(String(AValue));
end;

function TPuter.FontSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnFontSuccess) then
    FOnFontSuccess(String(AValue));
end;

procedure TPuter.WriteFile(AFile, AData: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.write(AFile, AData);
  p._then(@WriteSuccess, @PuterError);
end;

procedure TPuter.ReadFile(AFile: string);
var
  s: TJSBlob;
  p: TJSPromise;
begin
  s:=AWait(TJSBlob, PuterAPI.fs.read(AFile));
  p:=s.text;
  p._then(@ReadSuccess, @PuterError);
end;

procedure TPuter.MakeDirectory(ADir: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.mkdir(ADir);
  p._then(@MkdirSuccess, @PuterError);
end;

procedure TPuter.GetDirectory(ADir: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.readdir(ADir);
  p._then(@ReadDirSuccess, @PuterError);
end;

procedure TPuter.RenameFile(AFile, ANewName: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.rename(AFile, ANewName);
  p._then(@RenameSuccess, @PuterError);
end;

procedure TPuter.CopyFile(AFile, ADestDir: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.copy(AFile, ADestDir);
  p._then(@CopySuccess, @PuterError);
end;

procedure TPuter.MoveFile(AFile, ADestDir: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.move(AFile, ADestDir);
  p._then(@MoveSuccess, @PuterError);
end;

procedure TPuter.StatFile(AFile: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.stat(AFile);
  p._then(@StatSuccess, @PuterError);
end;

procedure TPuter.DeleteFile(AFile: string);
begin
  AWait(JSValue, PuterAPI.fs.delete(AFile));
end;

procedure TPuter.UploadFile(AFileList: TJSHTMLFileList);
var
  p: TJSPromise;
begin
  p:=PuterAPI.fs.upload(AFileList);
  p._then(@UploadSuccess, @PuterError);
end;

procedure TPuter.OpenFileDialog;
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showOpenFilePicker;
  p._then(@OpenFile);
end;

procedure TPuter.SaveFileDialog(AData: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showSaveFilePicker(AData);
  p._then(@SaveFile);
end;

procedure TPuter.AuthenticateWithPuter;
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.authenticateWithPuter;
  p._then(@AuthSuccess, @PuterError);
end;

procedure TPuter.Alert(AMessage: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.alert(AMessage);
  p._then(@AlertSuccess, @PuterError);
end;

procedure TPuter.Alert(AMessage: string; AButtons: TJSArray);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.alert(AMessage, AButtons);
  p._then(@AlertSuccess, @PuterError);
end;

procedure TPuter.Prompt(AMessage: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.prompt(AMessage);
  p._then(@AlertSuccess, @PuterError);
end;

procedure TPuter.Prompt(AMessage, APlaceHolder: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.prompt(AMessage, APlaceHolder);
  p._then(@AlertSuccess, @PuterError);
end;

procedure TPuter.CreateWindow(AOptions: TPuterWindowOptions);
begin
  PuterAPI.ui.createWindow(AOptions);
end;

procedure TPuter.Exit;
begin
  PuterAPI.exit;
end;

procedure TPuter.LaunchApp;
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.launchApp;
  p._then(@AppLaunchSuccess, @PuterError);
end;

procedure TPuter.LaunchApp(AName: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.launchApp(AName);
  p._then(@AppLaunchSuccess, @PuterError);
end;

procedure TPuter.ColorPicker;
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showColorPicker;
  p._then(@ColorSuccess, @PuterError);
end;

procedure TPuter.ColorPicker(DefaultColor: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showColorPicker(DefaultColor);
  p._then(@ColorSuccess, @PuterError);
end;

procedure TPuter.FontPicker;
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showFontPicker;
  p._then(@FontSuccess, @PuterError);
end;

procedure TPuter.FontPicker(DefaultFont: string);
var
  p: TJSPromise;
begin
  p:=PuterAPI.ui.showFontPicker(DefaultFont);
  p._then(@FontSuccess, @PuterError);
end;

initialization
  Puter:=TPuter.Create(Nil);

end.


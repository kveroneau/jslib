unit puterjs;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web;

type
  TPuterFile = class(TJSObject)
  public
    name, path, content: string;
    size: Integer;
    created: TJSDate;
    function read: TJSPromise external name 'read';
  end;

  TPuterDirectory = class(TJSObject)
  public
    path: string;
  end;

  TPuterUserInfo = class(TJSObject)
  public
    uuid, username: string;
    email_confirmed: boolean;
  end;

  TPuterErrorMsg = class(TJSObject)
  public
    message, code: string;
  end;

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

  TPuterUI = class(TJSObject)
  public
    function showOpenFilePicker: TJSPromise external name 'showOpenFilePicker';
    function showSaveFilePicker(data: string): TJSPromise external name 'showSaveFilePicker';
    function showDirectoryPicker: TJSPromise external name 'showDirectoryPicker';
  end;

  TPuterJS = class(TJSObject)
  public
    fs: TPuterFS;
    kv: TPuterKeyValue;
    hosting: TPuterHosting;
    auth: TPuterAuth;
    ui: TPuterUI;
    function randName: string external name 'randName';
  end;

  TPuterErrorEvent = reference to procedure(AError: TPuterErrorMsg);
  TPuterFileEvent = reference to procedure(AFile: TPuterFile);
  TReadFileEvent = reference to procedure(AContent: string);
  TPuterDirectoryEvent = reference to procedure(ADir: TPuterDirectory);
  TDirListEvent = reference to procedure(ADirList: TJSArray);
  TUploadEvent = reference to procedure(AFileList: TJSArray);

  { TPuter }

  TPuter = class(TComponent)
  private
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
    procedure ReadFile(f: TPuterFile); async;
    function OpenFile(AValue: JSValue): JSValue;
    function SaveFile(AValue: JSValue): JSValue;
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
  end;

var
  PuterAPI: TPuterJS; external name 'window.puter';
  Puter: TPuter;

implementation

{ TPuter }

function TPuter.WriteSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnWriteSuccess) then
    FOnWriteSuccess(TPuterFile(AValue));
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
    FOnDirSuccess(TPuterDirectory(AValue));
end;

function TPuter.ReadDirSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnDirListSuccess) then
    FOnDirListSuccess(TJSArray(AValue));
end;

function TPuter.RenameSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnRenameSuccess) then
    FOnRenameSuccess(TPuterFile(AValue));
end;

function TPuter.CopySuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnCopySuccess) then
    FOnCopySuccess(TPuterFile(AValue));
end;

function TPuter.MoveSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnMoveSuccess) then
    FOnMoveSuccess(TPuterFile(AValue));
end;

function TPuter.StatSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnStatSuccess) then
    FOnStatSuccess(TPuterFile(AValue));
end;

function TPuter.UploadSuccess(AValue: JSValue): JSValue;
begin
  if Assigned(FOnUploadSuccess) then
    FOnUploadSuccess(TJSArray(AValue));
end;

procedure TPuter.ReadFile(f: TPuterFile);
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
  ReadFile(TPuterFile(AValue));
end;

function TPuter.SaveFile(AValue: JSValue): JSValue;
begin
  if Assigned(FOnWriteSuccess) then
    FOnWriteSuccess(TPuterFile(AValue));
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

initialization
  Puter:=TPuter.Create(Nil);

end.


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

var
  Puter: TPuterJS; external name 'window.puter';

implementation

end.


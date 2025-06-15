unit webfs;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, ajaxlib, Types, JS;

type

  TVFSFile = Class external name 'Object' (TJSObject)
    fname: string;
    ico: string;
    wh: Integer;
    data: string;
  end;

  TFileSystem = Array of TVFSFile;

  { TWebFileSystem }

  TWebFileSystem = class(TComponent)
  private
    FWebRequest: TWebRequest;
    FOnVFSLoaded: TProc;
    FLoadStatus: Integer;
    FFS: TFileSystem;
    procedure LoadVFS;
  public
    property LoadStatus: Integer read FLoadStatus;
    constructor Create(AOwner: TComponent; AFile: string; AOnLoaded: TProc);
    function GetFile(FileId: Integer): TVFSFile;
    function GetFile(AFile: string): TVFSFile;
    function FindType(AIco: string; wh: Integer): Integer;
    function FindFile(AFile: string; wh: Integer): Integer;
  end;

implementation

{ TWebFileSystem }

procedure TWebFileSystem.LoadVFS;
var
  j: TJSObject;
begin
  if not FWebRequest.Complete then
    Exit;
  FLoadStatus:=FWebRequest.Status;
  if FLoadStatus <> 200 then
    Exit;
  try
    j:=TJSJSON.parseObject(FWebRequest.responseText);
    FFS:=TFileSystem(j);
  except
    FLoadStatus:=-2;
  end;
  FOnVFSLoaded;
end;

constructor TWebFileSystem.Create(AOwner: TComponent; AFile: string;
  AOnLoaded: TProc);
begin
  inherited Create(AOwner);
  FLoadStatus:=-1;
  FWebRequest:=TWebRequest.Create(Self, 'get', AFile);
  FWebRequest.OnChange:=@LoadVFS;
  FOnVFSLoaded:=AOnLoaded;
  FWebRequest.DoRequest;
end;

function TWebFileSystem.GetFile(FileId: Integer): TVFSFile;
begin
  if (FileId > -1) and (FileId < Length(FFS)) then
    Result:=FFS[FileId]
  else
    Result:=Nil;
end;

function TWebFileSystem.GetFile(AFile: string): TVFSFile;
var
  i: Integer;
begin
  for i:=0 to Length(FFS)-1 do
    if FFS[i].fname = AFile then
    begin
      Result:=FFS[i];
      Exit;
    end;
  Result:=Nil;
end;

function TWebFileSystem.FindType(AIco: string; wh: Integer): Integer;
var
  i: Integer;
begin
  for i:=0 to Length(FFS)-1 do
    if (FFS[i].wh = wh) and (FFS[i].ico = AIco) then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function TWebFileSystem.FindFile(AFile: string; wh: Integer): Integer;
var
  i: Integer;
begin
  for i:=0 to Length(FFS)-1 do
    if (FFS[i].wh = wh) and (FFS[i].fname = AFile) then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

end.


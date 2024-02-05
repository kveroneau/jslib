unit WebScript;

{$mode objfpc}

interface

uses
  Classes, SysUtils, browserconsole, marked, Web, ajaxlib, qvfs, strutils;

type

  TVMOutput = reference to procedure(s: string);
  TVMOnOpCode = reference to function(op: char): Boolean;

  { TWebScript }

  TWebScript = class(TComponent)
  private
    FData, FTarget: string;
    FCP: integer;
    FRemoteFile: TWebRequest;
    FOnOpCode: TVMOnOpCode;
    function NextOp: char;
    Procedure MarkdownFail(Sender: TObject; Const aString : String);
    procedure AssignProp(aID, aProp, aValue: string);
    procedure HandleFile;
    procedure LoadFile(AFile: string);
  protected
    procedure Output(s: string); virtual;
  public
    property Target: string read FTarget write FTarget;
    property OnOpCode: TVMOnOpCode read FOnOpCode write FOnOpCode;
    constructor Create(AOwner: TComponent; AData: string);
    function GetString: string;
    procedure Run;
  end;

  { TSysRunner }

  TSysRunner = class(TWebScript)
  private
    FOnOutput: TVMOutput;
    procedure GotDir(f: TVFSFile);
    procedure GetDir(f: TVFSFile);
    procedure VMCallback(f: TVFSFile);
  protected
    procedure Output(s: string); override;
  public
    property OnOutput: TVMOutput read FOnOutput write FOnOutput;
    procedure RunFile(AFile, ALoc: string);
  end;

var
  FileSys: TVFSRequest;
  VFS_ENDPOINT, VFS_ROOT: string;

implementation

{ TSysRunner }

procedure TSysRunner.GotDir(f: TVFSFile);
var
  e: TJSHTMLElement;
  icon: string;
begin
  if f.typ = -1 then
    Exit;
  case f.typ of
    0: icon:='dir';
    1: icon:='binary';
    2: icon:='layout';
    3: icon:='text';
    4: icon:='portal';
    5: icon:='folder.sec';
    6: icon:='quill';
    7: icon:='comp.blue';
    8: icon:='comp.gray';
    13: icon:='script';
  else
    icon:='generic';
  end;
  e:=TJSHTMLElement(document.getElementById(FTarget));
  icon:='<img src="/icons/'+icon+'.png"/>';
  e.innerHTML:=e.innerHTML+'<a href="#'+VFS_ROOT+f.wh+':'+f.fna+'">'+icon+f.fna+'</a><br/>';
end;

procedure TSysRunner.GetDir(f: TVFSFile);
var
  data, fna: string;
  fs: TVFSRequest;
begin
  if f.data[1] = 'V' then
  begin
    TJSHTMLElement(document.getElementById(FTarget)).innerHTML:='VFS Directory of '+f.fna+'<hr>';
    data:=RightStr(f.data, Length(f.data)-1);
    repeat
      fna:=Copy2SymbDel(data, ';');
      if fna <> '' then
      begin
        fs:=TVFSRequest.Create(Nil, 'get', VFS_ENDPOINT);
        fs.OnCallback:=@GotDir;
        fs.GetFile(fna, f.fna);
      end;
    until fna = '';
  end
  else
    Output('Directory Unavailable.');
end;

procedure TSysRunner.VMCallback(f: TVFSFile);
var
  t: TMarkdownFile;
begin
  if f.typ = -1 then
    Output('File not found.')
  else if f.typ = 0 then
    GetDir(f)
  else if f.typ = 2 then
    t:=TMarkdownFile.Create(Self, f.data, FTarget, @MarkdownFail)
  else if f.typ = 7 then
  begin
    FData:=f.data;
    FCP:=1;
    Run;
  end
  else
    Output('Unhandled file-type.');
end;

procedure TSysRunner.Output(s: string);
begin
  if Assigned(FOnOutput) then
    FOnOutput(s);
end;

procedure TSysRunner.RunFile(AFile, ALoc: string);
begin
  FileSys.OnCallback:=@VMCallback;
  FileSys.GetFile(AFile, ALoc);
end;

{ TWebScript }

procedure TWebScript.Output(s: string);
begin
  Writeln(s);
end;

function TWebScript.NextOp: char;
begin
  Result:=FData[FCP];
  Inc(FCP);
end;

function TWebScript.GetString: string;
var
  c: char;
begin
  Result:='';
  repeat
    c:=NextOp;
    if c <> '~' then
      Result:=Result+c;
  until (c = '~') or (FCP > Length(FData));
end;

procedure TWebScript.MarkdownFail(Sender: TObject; const aString: String);
begin
  TJSHTMLElement(document.getElementById(FTarget)).innerHTML:='Markdown failed to load: '+aString;
end;

procedure TWebScript.AssignProp(aID, aProp, aValue: string);
begin
  TJSHTMLElement(document.getElementById(aID)).Properties[aProp]:=aValue;
end;

procedure TWebScript.HandleFile;
begin
  if not FRemoteFile.Complete then
    Exit;
  Output(FRemoteFile.responseText);
  FreeAndNil(FRemoteFile);
end;

procedure TWebScript.LoadFile(AFile: string);
begin
  if Assigned(FRemoteFile) then
  begin
    Output('File Load Request already in progress...');
    Exit;
  end;
  FRemoteFile:=TWebRequest.Create(Nil, 'get', AFile);
  FRemoteFile.OnChange:=@HandleFile;
  FRemoteFile.DoRequest;
end;

constructor TWebScript.Create(AOwner: TComponent; AData: string);
begin
  inherited Create(AOwner);
  FData:=AData;
  FCP:=1;
  FTarget:='TabBody';
end;

procedure TWebScript.Run;
var
  op: char;
  running: boolean;
  t: TMarkdownFile;
  target: string;
begin
  running:=True;
  repeat
    op:=NextOp;
    case op of
      '$': target:=FTarget;
      'A': window.alert(GetString);
      'a': AssignProp(GetString, GetString, GetString);
      'h': TJSHTMLElement(document.getElementById(GetString)).hidden:=True;
      's': TJSHTMLElement(document.getElementById(GetString)).hidden:=False;
      'C': TJSHTMLElement(document.getElementById(GetString)).classList.add(GetString);
      'c': TJSHTMLElement(document.getElementById(GetString)).classList.remove(GetString);
      'P': Output(GetString);
      'L': LoadFile(GetString);
      'p': TJSHTMLElement(document.getElementById(target)).innerHTML:=GetString;
      {'B': LoadFileInto(GetString, FBody);}
      'T': target:=GetString;
      'M': t:=TMarkdownFile.Create(Self, GetString, target, @MarkdownFail);
      'X': running:=False;
    else
      running:=False;
      if op = '#' then
        Output('Program can only run on server.')
      else
      begin
        if Assigned(FOnOpCode) then
          running:=FOnOpCode(op);
        if not running then
          Output('Invalid Op Code at '+IntToStr(FCP-1));
      end;
    end;
  until not running;
end;

end.


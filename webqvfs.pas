unit webqvfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, jsontable;

type

  TOutputEvent = reference to procedure(data: string); safecall;

  { TWebQVFS }

  TWebQVFS = class(TComponent)
  private
    FVFS: TJSONTable;
    FEcho: TOutputEvent;
    FError: TOutputEvent;
    FOnVFSReady: TProc;
    FOnVFSFail: TProc;
    procedure Echo(const msg: string);
    procedure Error(const msg: string);
    function GetTitle: string;
    function GetType: Integer;
    function GetURL: string;
  public
    property OnEcho: TOutputEvent write FEcho;
    property OnError: TOutputEvent write FError;
    property OnVFSReady: TProc write FOnVFSReady;
    property OnVFSFail: TProc write FOnVFSFail;
    property Title: string read GetTitle;
    property FileType: Integer read GetType;
    property URL: string read GetURL;
    constructor Create(AOwner: TComponent); override;
    procedure OpenVFS(const VFSFile: string);
    function LocateFile(const aFile: string): boolean;
    function TypeIcon: string;
    procedure FilterDir(const aDir: string);
    procedure SetDir(const aDir: string);
    procedure ListDir;
    procedure VFSLoaded;
    procedure VFSLoadFail;
  end;

implementation

const
  DATE_FORMAT = 'dddd mmmm d, yyyy';

{ TWebQVFS }

procedure TWebQVFS.Echo(const msg: string);
begin
  if Assigned(FEcho) then
    FEcho(msg);
end;

procedure TWebQVFS.Error(const msg: string);
begin
  if Assigned(FError) then
    FError(msg);
end;

function TWebQVFS.GetTitle: string;
begin
  Result:=FVFS.Strings['TITLE'];
end;

function TWebQVFS.GetType: Integer;
begin
  Result:=FVFS.Ints['TYP'];
end;

function TWebQVFS.GetURL: string;
begin
  Result:=FVFS.Strings['URL'];
end;

constructor TWebQVFS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVFS:=Nil;
  FEcho:=Nil;
  FError:=Nil;
  FOnVFSReady:=Nil;
  FOnVFSFail:=Nil;
end;

procedure TWebQVFS.OpenVFS(const VFSFile: string);
begin
  if Assigned(FVFS) then
  begin
    FVFS.Active:=False;
    FVFS.Free;
  end;
  FVFS:=TJSONTable.Create(Self);
  FVFS.Datafile:=VFSFile;
  FVFS.OnSuccess:=@VFSLoaded;
  FVFS.OnFailure:=@VFSLoadFail;
  FVFS.Active:=True;
end;

function TWebQVFS.LocateFile(const aFile: string): boolean;
begin
  Result:=False;
  with FVFS.DataSet do
  begin
    First;
    repeat
      if FVFS.Strings['TITLE'] = aFile then
      begin
        Result:=True;
        Exit;
      end;
      Next;
    until EOF;
  end;
end;

function TWebQVFS.TypeIcon: string;
var
  png: string;
begin
  png:='unknown';
  case FVFS.Ints['TYP'] of
    0: png:='text-plain';
    1: png:='folder';
    2: png:='text-html';
    3: png:='script';
    4: png:='text-html';
    5: png:='image';
    6: png:='cmd';
    50: png:='database';
    51: png:='table';
    52: png:='video';
    53: png:='video';
    54: png:='dosbox';
  end;
  Result:='<img src="assets/'+png+'.png" border="0"/>';
end;

procedure TWebQVFS.FilterDir(const aDir: string);
begin
  with FVFS.DataSet do
  begin
    Filtered:=False;
    Filter:='WH='+QuotedStr(adir);
    Filtered:=True;
  end;
end;

procedure TWebQVFS.SetDir(const aDir: string);
begin
  { Original version in WebTerminal has components for the WebTerminal, and
    will need to be cleaned before bringing it here. }
end;

procedure TWebQVFS.ListDir;
var
  typ: Integer;
begin
  with FVFS.DataSet do
  begin
    if IsEmpty then
    begin
      Error('?DIRECTORY EMPTY');
      Exit;
    end;
    First;
    repeat
      typ:=Integer(FieldValues['TYP']);
      if typ = 1 then
        Echo('  <'+string(FieldValues['TITLE'])+'>  - '+FormatDateTime(DATE_FORMAT, FVFS.Dates['CREATED']))
      else
        Echo('  '+string(FieldValues['TITLE'])+'  - '+FormatDateTime(DATE_FORMAT, FVFS.Dates['CREATED']));
      Next;
    until EOF;
  end;
end;

procedure TWebQVFS.VFSLoaded;
begin
  Echo('VFS Loaded successfully.');
  Echo(IntToStr(FVFS.DataSet.RecordCount)+' file nodes.');
  if Assigned(FOnVFSReady) then
    FOnVFSReady;
end;

procedure TWebQVFS.VFSLoadFail;
begin
  Error('VFS Failed to load.');
  if Assigned(FOnVFSFail) then
    FOnVFSFail;
end;

end.


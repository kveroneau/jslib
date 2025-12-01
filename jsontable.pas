unit jsontable;
{
  I wrote this while working on my new kveroneau.github.io website, and decided
  that it is a rather abstract class that why don't I just put it into jslib?
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p2jsres, Web, ExtJSDataset, JS, ajaxlib;

type

  EJSONTable = class(Exception);
  EBadDatafile = class(EJSONTable);
  ENoDataSet = class(EJSONTable);
  EBadTable = class(EJSONTable);

  { TJSONTable }

  TJSONTable = class(TComponent)
  private
    FActive: Boolean;
    FDataSet: TExtJSJSONDataSet;
    FDatafile: string;
    FFilter: string;
    FRequest: TWebRequest;
    FOnSuccess: TJSOnReadyStateChangeHandler;
    FOnFailure: TJSOnReadyStateChangeHandler;
    function GetBooleans(AField: string): Boolean;
    function GetDataSet: TExtJSJSONDataSet;
    function GetDates(AField: string): TDateTime;
    function GetInts(AField: string): Integer;
    function GetStrings(AField: string): string;
    procedure SetActive(AValue: Boolean);
    procedure SetBooleans(AField: string; AValue: Boolean);
    procedure SetDatafile(AValue: string);
    procedure ParseJSON;
    procedure OpenTable;
    procedure CloseTable;
    procedure SetDates(AField: string; AValue: TDateTime);
    procedure SetFilter(AValue: string);
    procedure SetInts(AField: string; AValue: Integer);
    procedure SetStrings(AField: string; AValue: string);
  protected
    function StringField(const AField: string): string;
    function IntField(const AField: string): Integer;
  public
    property Active: Boolean read FActive write SetActive;
    property OnSuccess: TJSOnReadyStateChangeHandler read FOnSuccess write FOnSuccess;
    property OnFailure: TJSOnReadyStateChangeHandler read FOnFailure write FOnFailure;
    property Datafile: string read FDatafile write SetDatafile;
    property Filter: string read FFilter write SetFilter;
    property DataSet: TExtJSJSONDataSet read GetDataSet;
    property Strings[AField: string]: string read GetStrings write SetStrings;
    property Ints[AField: string]: Integer read GetInts write SetInts;
    property Booleans[AField: string]: Boolean read GetBooleans write SetBooleans;
    property Dates[AField: string]: TDateTime read GetDates write SetDates;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetJSON: string;
    procedure ParseTable(data: string);
  end;

  { TJSONDatabase }

  TJSONDatabase = class(TComponent)
  private
    FActive: Boolean;
    FJSON: TJSObject;
    FOnFailure: TJSOnReadyStateChangeHandler;
    FOnSuccess: TJSOnReadyStateChangeHandler;
    FRequest: TWebRequest;
    FDatafile: string;
    function GetTable(ATable: string): TJSONTable;
    function GetTables: TJSArray;
    procedure SetActive(AValue: Boolean);
    procedure parseJSON;
    procedure ParseDatabase(data: string);
  public
    property Active: Boolean read FActive write SetActive;
    property OnSuccess: TJSOnReadyStateChangeHandler read FOnSuccess write FOnSuccess;
    property OnFailure: TJSOnReadyStateChangeHandler read FOnFailure write FOnFailure;
    property Datafile: string read FDatafile write FDatafile;
    property TableNames: TJSArray read GetTables;
    property Table[ATable: string]: TJSONTable read GetTable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TJSONDatabase }

procedure TJSONDatabase.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue and (FDatafile = '') then
    raise EBadDatafile.Create('Datafile is not set.');
  if AValue then
  begin
    FRequest:=TWebRequest.Create(Self, 'get', FDatafile+'.json');
    FRequest.OnChange:=@parseJSON;
    FRequest.DoRequest;
  end;
  FActive:=AValue;
end;

function TJSONDatabase.GetTables: TJSArray;
begin
  Result:=TJSArray(FJSON.Properties['tables']);
end;

function TJSONDatabase.GetTable(ATable: string): TJSONTable;
var
  tbl: TJSObject;
begin
  if not GetTables.includes(ATable) then
    raise EBadTable.Create('Table does not exist: '+ATable);
  tbl:=TJSObject(FJSON.Properties[ATable]);
  Result:=TJSONTable.Create(Self);
  with Result.FDataSet do
  begin
    MetaData:=TJSObject(tbl.Properties['metaData']);
    Rows:=TJSArray(tbl.Properties['Data']);
    Open;
  end;
  Result.FActive:=True;
end;

procedure TJSONDatabase.parseJSON;
begin
  if not FRequest.Complete then
    Exit;
  if FRequest.Status <> 200 then
  begin
    if Assigned(FOnFailure) then
      FOnFailure;
  end
  else
  begin
    ParseDatabase(FRequest.responseText);
    if Assigned(FOnSuccess) then
      FOnSuccess;
  end;
end;

procedure TJSONDatabase.ParseDatabase(data: string);
begin
  FJSON:=TJSJSON.parseObject(data);
end;

constructor TJSONDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatafile:='';
end;

destructor TJSONDatabase.Destroy;
begin
  inherited Destroy;
end;

{ TJSONTable }

procedure TJSONTable.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue and (FDatafile = '') then
    raise EBadDatafile.Create('Datafile is not set.');
  if AValue then
    OpenTable
  else
    CloseTable;
end;

procedure TJSONTable.SetBooleans(AField: string; AValue: Boolean);
begin
  FDataSet.Fields.FieldByName(AField).Value:=AValue;
end;

function TJSONTable.GetDataSet: TExtJSJSONDataSet;
begin
  if not FActive then
    raise ENoDataSet.Create('Dataset not available.');
  Result:=FDataSet;
end;

function TJSONTable.GetDates(AField: string): TDateTime;
begin
  Result:=FDataSet.FieldByName(AField).AsDateTime;
end;

function TJSONTable.GetBooleans(AField: string): Boolean;
begin
  Result:=FDataSet.FieldByName(AField).AsBoolean;
end;

function TJSONTable.GetInts(AField: string): Integer;
begin
  Result:=IntField(AField);
end;

function TJSONTable.GetStrings(AField: string): string;
begin
  Result:=StringField(AField);
end;

procedure TJSONTable.SetDatafile(AValue: string);
begin
  if FDatafile=AValue then Exit;
  if FActive then
    raise EBadDatafile.Create('Datafile cannot be set while active.');
  FDatafile:=AValue;
end;

procedure TJSONTable.ParseTable(data: string);
var
  json: TJSObject;
begin
  if FActive then
    raise EJSONTable.Create('Cannot parse table with active dataset.');
  json:=TJSJSON.parseObject(data);
  FDataSet.MetaData:=TJSObject(json.Properties['metaData']);
  FDataSet.Rows:=TJSArray(json.Properties['Data']);
  FDataSet.Open;
  FActive:=True;
end;

procedure TJSONTable.ParseJSON;
begin
  if not FRequest.Complete then
    Exit;
  if FRequest.Status <> 200 then
  begin
    if Assigned(FOnFailure) then
      FOnFailure;
  end
  else
  begin
    ParseTable(FRequest.responseText);
    if Assigned(FOnSuccess) then
      FOnSuccess;
  end;
end;

procedure TJSONTable.OpenTable;
var
  info: TResourceInfo;
begin
  if GetResourceInfo(rsJS, FDatafile, info) then
  begin
    ParseTable(window.atob(info.data));
    if Assigned(FOnSuccess) then
      FOnSuccess;
  end
  else
  begin
    FRequest:=TWebRequest.Create(Self, 'get', FDatafile+'.json');
    FRequest.OnChange:=@ParseJSON;
    FRequest.DoRequest;
  end;
end;

procedure TJSONTable.CloseTable;
begin
  FDataSet.Close;
  FActive:=False;
end;

procedure TJSONTable.SetDates(AField: string; AValue: TDateTime);
begin
  FDataSet.FieldByName(AField).AsDateTime:=AValue;
end;

procedure TJSONTable.SetFilter(AValue: string);
begin
  if FFilter=AValue then Exit;
  if not FActive then
    raise ENoDataSet.Create('Dataset is not active.');
  if AValue = '' then
    FDataSet.Filtered:=False
  else
  begin
    FDataSet.Filter:=AValue;
    FDataSet.Filtered:=True;
    FDataSet.First;
  end;
  FFilter:=AValue;
end;

procedure TJSONTable.SetInts(AField: string; AValue: Integer);
begin
  FDataSet.Fields.FieldByName(AField).Value:=AValue;
end;

procedure TJSONTable.SetStrings(AField: string; AValue: string);
begin
  FDataSet.Fields.FieldByName(AField).Value:=AValue;
end;

function TJSONTable.StringField(const AField: string): string;
begin
  Result:=FDataSet.FieldByName(AField).AsString;
end;

function TJSONTable.IntField(const AField: string): Integer;
begin
  Result:=FDataSet.FieldByName(AField).AsInteger;
end;

constructor TJSONTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSet:=TExtJSJSONDataSet.Create(Self);
  FDatafile:='';
end;

destructor TJSONTable.Destroy;
begin
  FDataSet.Free;
  inherited Destroy;
end;

function TJSONTable.GetJSON: string;
var
  j: TJSObject;
begin
  j:=TJSObject.new;
  j.Properties['metaData']:=FDataSet.MetaData;
  j.Properties['Data']:=FDataSet.Rows;
  Result:=TJSJSON.stringify(j);
end;

end.


unit jsontable;
{
  I wrote this while working on my new kveroneau.github.io website, and decided
  that it is a rather abstract class that why don't I just put it into jslib?
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p2jsres, Web, ExtJSDataset, JS;

type

  EJSONTable = class(Exception);
  EBadDatafile = class(EJSONTable);
  ENoDataSet = class(EJSONTable);

  { TJSONTable }

  TJSONTable = class(TComponent)
  private
    FActive: Boolean;
    FDataSet: TExtJSJSONDataSet;
    FDatafile: string;
    FFilter: string;
    function GetBooleans(AField: string): Boolean;
    function GetDataSet: TExtJSJSONDataSet;
    function GetInts(AField: string): Integer;
    function GetStrings(AField: string): string;
    procedure SetActive(AValue: Boolean);
    procedure SetBooleans(AField: string; AValue: Boolean);
    procedure SetDatafile(AValue: string);
    procedure OpenTable;
    procedure CloseTable;
    procedure SetFilter(AValue: string);
    procedure SetInts(AField: string; AValue: Integer);
    procedure SetStrings(AField: string; AValue: string);
  protected
    function StringField(const AField: string): string;
    function IntField(const AField: string): Integer;
  public
    property Active: Boolean read FActive write SetActive;
    property Datafile: string read FDatafile write SetDatafile;
    property Filter: string read FFilter write SetFilter;
    property DataSet: TExtJSJSONDataSet read GetDataSet;
    property Strings[AField: string]: string read GetStrings write SetStrings;
    property Ints[AField: string]: Integer read GetInts write SetInts;
    property Booleans[AField: string]: Boolean read GetBooleans write SetBooleans;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DS: TExtJSJSONDataSet;

implementation

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
  FActive:=AValue;
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

procedure TJSONTable.OpenTable;
var
  info: TResourceInfo;
  json: TJSObject;
begin
  if not GetResourceInfo(rsJS, FDatafile, info) then
    Exit;
  json:=TJSJSON.parseObject(window.atob(info.data));
  FDataSet.MetaData:=TJSObject(json.Properties['metaData']);
  FDataSet.Rows:=TJSArray(json.Properties['Data']);
  FDataSet.Open;
end;

procedure TJSONTable.CloseTable;
begin
  FDataSet.Close;
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

end.


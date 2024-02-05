unit jsonlib;

{$mode objfpc}

interface

uses
  Classes, Web, JS;

type

  { TJSONData }

  TJSONData = class(TComponent)
  private
    FData: TJSObject;
    function GetArray(Name: String): TJSArray;
    function GetBoolean(Name: String): Boolean;
    function GetInt(Name: String): NativeInt;
    function GetObject(Name: String): TJSONData;
    function GetString(Name: String): String;
  public
    property Strings[Name: String]: String read GetString;
    property Booleans[Name: String]: Boolean read GetBoolean;
    property Integers[Name: String]: NativeInt read GetInt;
    property Objects[Name: String]: TJSONData read GetObject;
    property Arrays[Name: String]: TJSArray read GetArray;
    constructor Create(AOwner: TComponent; data: TJSObject);
    function contains(aName: String): boolean;
  end;

implementation

{ TJSONData }

function TJSONData.GetString(Name: String): String;
begin
  Result:=String(FData.Properties[Name]);
end;

function TJSONData.GetBoolean(Name: String): Boolean;
begin
  Result:=Boolean(FData.Properties[Name]);
end;

function TJSONData.GetArray(Name: String): TJSArray;
begin
  Result:=TJSArray(FData.Properties[Name]);
end;

function TJSONData.GetInt(Name: String): NativeInt;
begin
  Result:=NativeInt(FData.Properties[Name]);
end;

function TJSONData.GetObject(Name: String): TJSONData;
begin
  Result:=TJSONData.Create(Self, TJSObject(FData.Properties[Name]));
end;

constructor TJSONData.Create(AOwner: TComponent; data: TJSObject);
begin
  inherited Create(AOwner);
  FData:=data;
end;

function TJSONData.contains(aName: String): boolean;
begin
  Result:=FData.hasOwnProperty(aName);
end;

end.


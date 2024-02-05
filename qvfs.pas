unit qvfs;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, ajaxlib, jsonlib;

type
  TVFSFile = Class external name 'Object' (TJSObject)
    fna: string;
    wh: string;
    typ: integer;
    data: string;
  end;

  TVFSCallback = reference to procedure(f: TVFSFile);

  { TVFSRequest }

  TVFSRequest = class(TWebRequest)
  private
    FOnCallback: TVFSCallback;
    procedure ParseJSON;
    procedure SetOnCallback(AValue: TVFSCallback);
  public
    property OnCallback: TVFSCallback read FOnCallback write SetOnCallback;
    procedure GetFile(AFile, ALoc: string);
  end;

implementation

{ TVFSRequest }

procedure TVFSRequest.ParseJSON;
var
  f: TVFSFile;
  json: TJSObject;
  data: TJSONData;
begin
  if not Complete then
    Exit;
  json:=TJSJSON.parseObject(responseText);
  data:=TJSONData.Create(Nil, json);
  if data.contains('error') then
  begin
    f:=TVFSFile.new;
    f.typ:=-1;
    f.data:='File not found.';
  end
  else
    f:=TVFSFile(json);
  data.Free;
  FOnCallback(f);
end;

procedure TVFSRequest.SetOnCallback(AValue: TVFSCallback);
begin
  if FOnCallback=AValue then Exit;
  FOnCallback:=AValue;
  OnChange:=@ParseJSON;
end;

procedure TVFSRequest.GetFile(AFile, ALoc: string);
begin
  DoRequest('path='+ALoc+':'+AFile);
end;

end.


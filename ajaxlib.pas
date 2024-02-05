unit ajaxlib;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Web, Types, JS, jsonlib;

type

  { TWebRequest }

  TWebRequest = class(TComponent)
  private
    FMethod, FURI: string;
    FCallback: TJSOnReadyStateChangeHandler;
    FRequest: TJSXMLHttpRequest;
    FHeaders: TStringList;
    function GetComplete: Boolean;
    function GetResponseText: string;
    function GetState: NativeInt;
    procedure SetCallback(AValue: TJSOnReadyStateChangeHandler);
    procedure ProcessHeaders;
  public
    property OnChange: TJSOnReadyStateChangeHandler read FCallback write SetCallback;
    property readyState: NativeInt read GetState;
    property Complete: Boolean read GetComplete;
    property responseText: string read GetResponseText;
    constructor Create(AOwner: TComponent; AMethod, AURI: string);
    procedure SetHeader(ATitle, AValue: string);
    procedure DoRequest;
    procedure DoRequest(body: string);
    procedure DoRequest(FormData: TStringList);
  end;

  TJSONCallback = reference to procedure(json: TJSONData);

  { TJSONRequest }

  TJSONRequest = class(TWebRequest)
  private
    FJSONCallback: TJSONCallback;
    procedure ParseJSON;
    procedure SetJSONCallback(AValue: TJSONCallback);
  public
    property OnJSON: TJSONCallback read FJSONCallback write SetJSONCallback;
  end;

  TVMData = Class external name 'Object' (TJSObject)
    command: string;
    params: string;
    enabled: boolean;
  end;

  TVMCallback = reference to procedure(vm: TVMData);

  { TVMRequest }

  TVMRequest = class(TWebRequest)
  private
    FOnVM: TVMCallback;
    procedure ParseJSON;
    procedure SetOnVM(AValue: TVMCallback);
  public
    property OnVM: TVMCallback read FOnVM write SetOnVM;
  end;

implementation

{ TVMRequest }

procedure TVMRequest.ParseJSON;
var
  data: TVMData;
begin
  if not Complete then
    Exit;
  data:=TVMData(TJSJSON.parseObject(responseText));
  FOnVM(data);
end;

procedure TVMRequest.SetOnVM(AValue: TVMCallback);
begin
  if FOnVM=AValue then Exit;
  FOnVM:=AValue;
  OnChange:=@ParseJSON;
end;

{ TJSONRequest }

procedure TJSONRequest.ParseJSON;
var
  data: TJSObject;
begin
  if not Complete then
    Exit;
  data:=TJSJSON.parseObject(responseText);
  FJSONCallback(TJSONData.Create(Self, data));
end;

procedure TJSONRequest.SetJSONCallback(AValue: TJSONCallback);
begin
  if FJSONCallback=AValue then Exit;
  FJSONCallback:=AValue;
  OnChange:=@ParseJSON;
end;

{ TWebRequest }

procedure TWebRequest.SetCallback(AValue: TJSOnReadyStateChangeHandler);
begin
  if FCallback=AValue then Exit;
  FCallback:=AValue;
  FRequest.onreadystatechange:=AValue;
end;

procedure TWebRequest.ProcessHeaders;
var
  i: Integer;
begin
  for i:=0 to FHeaders.Count-1 do
    FRequest.setRequestHeader(FHeaders.Names[i], FHeaders.ValueFromIndex[i]);
end;

function TWebRequest.GetState: NativeInt;
begin
  Result:=FRequest.readyState;
end;

function TWebRequest.GetComplete: Boolean;
begin
  if FRequest.readyState = FRequest.DONE then
    Result:=True
  else
    Result:=False;
end;

function TWebRequest.GetResponseText: string;
begin
  Result:=FRequest.responseText;
end;

constructor TWebRequest.Create(AOwner: TComponent; AMethod, AURI: string);
begin
  FRequest:=TJSXMLHttpRequest.new;
  FHeaders:=TStringList.Create;
  FMethod:=AMethod;
  FURI:=AURI;
end;

procedure TWebRequest.SetHeader(ATitle, AValue: string);
begin
  FHeaders.Add(ATitle+'='+AValue);
end;

procedure TWebRequest.DoRequest;
begin
  DoRequest('');
end;

procedure TWebRequest.DoRequest(body: string);
var
  uri: string;
begin
  if FMethod = 'get' then
    uri:=FURI+'?'+body
  else
    uri:=FURI;
  FRequest.open(FMethod, uri, True);
  ProcessHeaders;
  if FMethod = 'post' then
  begin
    FRequest.setRequestHeader('content-type', 'application/x-www-form-urlencoded');
    if body <> '' then
    begin
      FRequest.send(body);
      FHeaders.Clear;
      Exit;
    end;
  end;
  FRequest.send;
  FHeaders.Clear;
end;

procedure TWebRequest.DoRequest(FormData: TStringList);
var
  buf: string;
  i: integer;
begin
  buf:='';
  for i:=0 to FormData.Count-1 do
    buf:=buf+FormData.Names[i]+'='+FormData.ValueFromIndex[i]+'&';
  DoRequest(LeftStr(buf, Length(buf)-1));
end;

end.


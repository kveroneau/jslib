unit webvt100;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Types, JS, strutils;

type

  { TVT100 }

  TVT100 = class external name 'VT100' (TJSObject)
  Private
    FKeysPressed: TProcString; external name 'keysPressed';
    FSendKeys: TProcString; external name 'sendKeys';
    FReconnect: TProc; external name 'reconnect';
    FCursorX: Integer; external name 'cursorX';
    FCursorY: Integer; external name 'cursorY';
  Public
    constructor new;
    constructor new(container: string);
    function write(data: string): string; external name 'vt100';
    procedure showReconnect(value: Boolean);
    procedure reset(value: Boolean);
    property OnKeysPressed: TProcString read FKeysPressed write FKeysPressed;
    property OnSendKeys: TProcString read FSendKeys write FSendKeys;
    property OnReconnect: TProc read FReconnect write FReconnect;
    property CursorX: Integer read FCursorX;
    property CursorY: Integer read FCursorY;
  end;

  TTermMode = (tmNormal, tmRaw);
  TTermMouseEvent = reference to procedure(Button, col, row: Integer);

  { TWebTerminal }

  TWebTerminal = class(TObject)
  Private
    FTerm: TVT100;
    FBuffer: string;
    FPrompt: string;
    FAcceptInput: Boolean;
    FTermMode: TTermMode;
    FOnPayload: TProcString;
    FOnControlCode: TProcString;
    FOnMouse: TTermMouseEvent;
    FCurPos: Integer;
    FMask: Boolean;
    FRows, FCols: Integer;
    procedure SetupEvents;
    procedure HandleNormal(const data: string);
    procedure HandleRawMode(const data: string);
    procedure HandleInput(const data: string);
    procedure HandleOOB(const data: string);
    procedure Esc(const data: string);
    procedure Csi(const data: string);
    procedure GetTermSize;
    procedure SetMouse(value: Boolean);
    procedure DrawPrompt;
    procedure SetPrompt(value: string);
    procedure SetMode(value: TTermMode);
    function ProcessEsc(const esc: string): boolean;
    function Ctrl(const data: string): boolean;
    procedure GetMouse(const esc: string);
    procedure SetAttr(const attr: Integer);
  Public
    constructor Create;
    constructor Create(const target: string);
    destructor Destroy; override;
    procedure Write(const data: string);
    procedure WriteLn(const data: string);
    procedure Clear;
    procedure MoveTo(row, col: Integer);
    property Mouse: Boolean write SetMouse;
    property OnPayload: TProcString read FOnPayload write FOnPayload;
    property Prompt: string read FPrompt write SetPrompt;
    property Mask: Boolean read FMask write FMask;
    property Mode: TTermMode read FTermMode write SetMode;
    property OnControlCode: TProcString read FOnControlCode write FOnControlCode;
    property OnMouse: TTermMouseEvent read FOnMouse write FOnMouse;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property Attr: Integer write SetAttr;
  end;

const
  tcBlack = 0;
  tcRed = 1;
  tcGreen = 2;
  tcYellow = 3;
  tcBlue = 4;
  tcMagenta = 5;
  tcCyan = 6;
  tcWhite = 7;
  taForeground = 30;
  taBackground = 40;

implementation

{ TWebTerminal }

constructor TWebTerminal.Create;
begin
  FTerm:=TVT100.new;
  SetupEvents;
end;

constructor TWebTerminal.Create(const target: string);
begin
  FTerm:=TVT100.new(target);
  SetupEvents;
end;

destructor TWebTerminal.Destroy;
begin
  { TODO : Figure out how to free a JavaScript prototype. }
  inherited Destroy;
end;

procedure TWebTerminal.Write(const data: string);
begin
  FTerm.write(data);
end;

procedure TWebTerminal.WriteLn(const data: string);
begin
  FTerm.write(data+#10#13);
end;

procedure TWebTerminal.Clear;
begin
  Csi('H');
  Csi('J');
end;

procedure TWebTerminal.MoveTo(row, col: Integer);
begin
  Csi(IntToStr(row)+';'+IntToStr(col)+'H');
end;

procedure TWebTerminal.SetupEvents;
begin
  FTerm.OnKeysPressed:=@HandleInput;
  FTerm.OnSendKeys:=@HandleOOB;
  FAcceptInput:=True;
  FTermMode:=tmNormal;
  GetTermSize;
end;

procedure TWebTerminal.HandleNormal(const data: string);
var
  tmp: string;
  editing: Boolean;
begin
  if data = #13 then
  begin
    FAcceptInput:=False;
    FTerm.write(#10#13);
    tmp:=FBuffer;
    FBuffer:='';
    FCurPos:=0;
    if Assigned(FOnPayload) then
      FOnPayload(tmp);
    Exit;
  end
  else if data = #127 then
  begin
    if (FCurPos < Length(FBuffer)) and (FCurPos > 0) then
    begin
      tmp:=TJSString(FBuffer).slice(0, FCurPos-1);
      FBuffer:=tmp+TJSString(FBuffer).slice(FCurPos);
      Csi('D');
      Dec(FCurPos);
      Csi('s');
      editing:=True;
    end
    else
    begin
      Dec(FCurPos);
      FBuffer:=TJSString(FBuffer).substr(0, FCurPos);
      if FCurPos < 0 then
        FCurPos:=0;
    end;
  end
  else if data = #9 then
    Ctrl('t'+FBuffer)
  else if data = #3 then
    Ctrl('C')
  else if data = #26 then
    Ctrl('Z')
  else if data = #4 then
    Ctrl('D')
  else if (Length(data) > 0) and (Ord(data[1]) = 27) then
  begin
    if ProcessEsc(TJSString(data).slice(2)) then
      Exit;
  end
  else if (Length(data) = 1) and (Ord(data[1]) < 127) then
  begin
    if FCurPos < Length(FBuffer) then
    begin
      tmp:=TJSString(FBuffer).slice(0, FCurPos);
      FBuffer:=tmp+data+TJSString(FBuffer).slice(FCurPos);
      Csi('C');
      Inc(FCurPos);
      Csi('s');
      editing:=True;
    end
    else
    begin
      FBuffer:=FBuffer+data;
      Inc(FCurPos);
    end;
  end;
  DrawPrompt;
  if editing then
    Csi('u');
end;

procedure TWebTerminal.HandleRawMode(const data: string);
var
  tmp: string;
begin
  FBuffer:='';
  if data = #26 then
  begin
    Ctrl('Z');
    Exit;
  end;
  if (Length(data) > 0) and (data[1] = #27) then
  begin
    ProcessEsc(TJSString(data).slice(2));
    Exit;
  end;
  tmp:=data;
  if data = #13 then
    tmp:=#10;
  FOnPayload(tmp);
end;

procedure TWebTerminal.HandleInput(const data: string);
begin
  if not FAcceptInput then
    Exit;
  case FTermMode of
    tmNormal: HandleNormal(data);
    tmRaw: HandleRawMode(data);
  end;
end;

procedure TWebTerminal.HandleOOB(const data: string);
begin
  System.WriteLn('OOB - Length of input='+IntToStr(Length(data)));
end;

procedure TWebTerminal.Esc(const data: string);
var
  tmp: string;
begin
  tmp:=FTerm.write(#27+data);
  if tmp <> '' then
    HandleInput(tmp);
end;

procedure TWebTerminal.Csi(const data: string);
begin
  Esc('['+data);
end;

procedure TWebTerminal.GetTermSize;
begin
  Csi('s');
  Csi('999;999H');
  Csi('6n');
  Csi('u');
end;

procedure TWebTerminal.SetMouse(value: Boolean);
begin
  if value then
    Csi('?9h')
  else
    Csi('?9l');
end;

procedure TWebTerminal.DrawPrompt;
var
  buf: string;
begin
  if FMask then
    buf:=DupeString('*', Length(FBuffer))
  else
    buf:=FBuffer;
  FTerm.write(#13#27+'[K'+FPrompt+buf);
end;

procedure TWebTerminal.SetPrompt(value: string);
begin
  FAcceptInput:=True;
  if FTermMode = tmRaw then
    Exit;
  FPrompt:=value;
  FTerm.write(#13#27+'[K'+FPrompt);
end;

procedure TWebTerminal.SetMode(value: TTermMode);
begin
  FAcceptInput:=True;
  FTermMode:=value;
end;

function TWebTerminal.ProcessEsc(const esc: string): boolean;
var
  StrA: TStringDynArray;
  tmp: string;
begin
  if Length(esc) = 0 then
  begin
    Result:=Ctrl('ESC');
    Exit;
  end
  else if esc = 'A' then
    Result:=Ctrl('UP')
  else if esc = 'B' then
    Result:=Ctrl('DN')
  else if esc = 'C' then
  begin
    if FTermMode = tmRaw then
    begin
      Result:=Ctrl('RT');
      Exit;
    end;
    Inc(FCurPos);
    if FCurPos > Length(FBuffer) then
      FCurPos:=Length(FBuffer)
    else
      Csi('C');
    Result:=True;
  end
  else if esc = 'D' then
  begin
    if FTermMode = tmRaw then
    begin
      Result:=Ctrl('LT');
      Exit;
    end;
    Dec(FCurPos);
    if FCurPos < 0 then
      FCurPos:=0
    else
      Csi('D');
    Result:=True;
  end
  else if esc = 'F' then
  begin
    if FTermMode = tmRaw then
    begin
      Result:=Ctrl('EN');
      Exit;
    end;
    FCurPos:=Length(FBuffer);
  end
  else if esc = 'H' then
  begin
    if FTermMode = tmRaw then
    begin
      Result:=Ctrl('HM');
      Exit;
    end;
    FCurPos:=0;
    Result:=True;
  end
  else if esc[Length(esc)] = 'R' then
  begin
    tmp:=TJSString(esc).slice(0, Length(esc)-1);
    StrA:=TJSString(tmp).split(';');
    FRows:=StrToInt(StrA[0]);
    FCols:=StrToInt(StrA[1]);
  end
  else if esc[1] = 'M' then
  begin
    GetMouse(esc);
  end;
  case esc of
    'P': Ctrl('F1');
    'Q': Ctrl('F2');
    'R': Ctrl('F3');
    'S': Ctrl('F4');
    '15~': Ctrl('F5');
    '17~': Ctrl('F6');
    '18~': Ctrl('F7');
    '19~': Ctrl('F8');
    '20~': Ctrl('F9');
    '21~': Ctrl('F10');
    '23~': Ctrl('F11');
    '24~': Ctrl('F12');
  end;
end;

function TWebTerminal.Ctrl(const data: string): boolean;
begin
  If Assigned(FOnControlCode) then
    FOnControlCode(data);
end;

procedure TWebTerminal.GetMouse(const esc: string);
var
  but, row, col: Integer;
begin
  but:=Ord(esc[2])-32;
  col:=Ord(esc[3])-32;
  row:=Ord(esc[4])-32;
  if Assigned(FOnMouse) then
    FOnMouse(but, col, row);
end;

procedure TWebTerminal.SetAttr(const attr: Integer);
begin
  Csi(IntToStr(attr)+'m');
end;

end.


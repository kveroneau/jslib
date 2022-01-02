unit vtwidgets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webvt100, timer, Types, JS, strutils;

type

  TVTWidgetState = (wsActive, wsInactive);

  TVTScreen = class;

  { TVTWidget }

  TVTWidget = class(TComponent)
  Private
    FState: TVTWidgetState;
    FWrite: TProcString;
    {procedure doIdle; virtual; abstract;}
  Public
    constructor Create(AOwner: TComponent); override;
    procedure HandleMouse(const Button, Row, Col: integer); virtual;
    procedure Render; virtual;
  Protected
    FTerm: TWebTerminal;
    FScreen: TVTScreen;
    procedure SetInactive;
    property WidgetState: TVTWidgetState read FState;
  end;

  { TVTDramaPrint }

  TVTDramaPrint = class(TVTWidget)
  Private
    FTimer: TTimer;
    FString: TJSString;
    FPosition: TPoint;
    FCharPos: integer;
    FOnComplete: TProc;
    procedure PrintChar(Sender: TObject);
  Public
    constructor Create(AOwner: TComponent; msg: string; x,y: integer);
    constructor Create(AOwner: TComponent; msg: string; x,y: integer; OnComplete: TProc);
  end;

  { TVTBSOD }

  TVTBSOD = class(TVTWidget)
  Public
    constructor Create(AOwner: TComponent; msg: string);
  end;

  { TVTDataLabel }

  TVTDataLabel = class(TVTWidget)
  Private
    FLabel: String;
    FData: String;
    FPosition: TPoint;
    procedure Draw;
    procedure SetLabel(value: string);
    procedure SetData(value: string);
  Public
    constructor Create(AOwner: TComponent; x, y: integer);
    property Title: string read FLabel write SetLabel;
    property Data: string read FData write SetData;
  end;

  { TVTLabel }

  TVTLabel = class(TVTWidget)
  Private
    FLabel: String;
  Public
    constructor Create(AOwner: TComponent; const s: string);
    procedure Render; override;
  end;

  { TWidgetContainer }

  TWidgetContainer = class(TComponent)
  Private
    FTerm: TWebTerminal;
    FScreen: TVTScreen;
  Protected
    property WebTerminal: TWebTerminal read FTerm;
    property Screen: TVTScreen read FScreen;
  Public
    constructor Create(AOwner: TComponent); override;
  end;

  { TVTWindow }

  TVTWindow = class(TVTWidget)
  Private
    FTitle: String;
    FPosition: TRect;
    FCurPos: TPoint;
    procedure Draw;
  Public
    constructor Create(AOwner: TComponent; const title: string; x, y, w, h: integer);
    destructor Destroy; override;
    procedure write(const s: string);
    procedure writeln(const s: string);
    procedure MoveTo(const x,y: integer);
    procedure HandleMouse(const Button, Row, Col: integer); override;
    procedure Render; override;
    procedure AddLabel(const s: string);
    property Position: TRect read FPosition;
  end;

  { TWindowManager }

  TWindowManager = class(TVTWidget)
  Private
    FActiveWindow: TVTWindow;
  Public
    function NewWindow(const title: string; x, y, w, h: integer): TVTWindow;
    procedure HandleMouse(const Button, Row, Col: integer); override;
    procedure Render; override;
  end;

  { TVTScreen }

  TVTScreen = class(TComponent)
  Private
    FTerm: TWebTerminal;
    FTimer: TTimer;
    FOnRefresh: TProc;
    FCountDown: integer;
    FOnCountDown: TProc;
    FForeground: integer;
    FBackground: integer;
    FWidgets: TWidgetContainer;
    procedure HandleInput(const data: string);
    procedure HandleCtrl(const data: string);
    procedure HandleMouse(Button, col, row: Integer);
    procedure SetupScreen;
    procedure SetInverse(value: boolean);
  Public
    constructor Create(AOwner: TComponent);
    constructor Create(AOwner: TComponent; const container: string);
    destructor Destroy; override;
    procedure DramaPrint(const msg: string; x,y: integer);
    procedure DramaPrint(const msg: string; x,y: integer; OnComplete: TProc);
    procedure SetColor(const fg, bg: integer);
    procedure BSOD(const msg: string);
    procedure CountDown(secs: integer; OnCountDown: TProc);
    procedure Center(const row: integer; const msg: string);
    procedure Clear;
    property OnRefresh: TProc read FOnRefresh write FOnRefresh;
    property Inverse: boolean write SetInverse;
    property Widgets: TWidgetContainer read FWidgets;
  Protected
    procedure WidgetStateChange(Sender: TObject);
    property WebTerminal: TWebTerminal read FTerm;
    property Foreground: integer read FForeground;
    property Background: integer read FBackground;
  end;

implementation

{ TVTLabel }

constructor TVTLabel.Create(AOwner: TComponent; const s: string);
begin
  inherited Create(AOwner);
  FLabel:=s;
end;

procedure TVTLabel.Render;
begin
  FWrite(FLabel);
end;

{ TWindowManager }

function TWindowManager.NewWindow(const title: string; x, y, w, h: integer
  ): TVTWindow;
begin
  Result:=TVTWindow.Create(Self, title, x, y, w, h);
  FActiveWindow:=Result;
end;

procedure TWindowManager.HandleMouse(const Button, Row, Col: integer);
var
  i: integer;
  w: TVTWindow;
begin
  for i:=ComponentCount-1 downto 0 do
  begin
    w:=TVTWindow(Components[i]);
    if (Button = 0) and (Row = w.Position.Top) then
    begin
      if (Col >= w.Position.Left) and (Col < w.Position.Left+2) then
      begin
        w.Free;
        Render;
      end;
    end
    else
      w.HandleMouse(Button, Row, Col);
  end;
end;

procedure TWindowManager.Render;
var
  i: integer;
  w: TVTWindow;
begin
  for i:=0 to ComponentCount-1 do
  begin
    w:=TVTWindow(Components[i]);
    w.Render;
  end;
end;

{ TWidgetContainer }

constructor TWidgetContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreen:=TVTScreen(AOwner);
  FTerm:=FScreen.WebTerminal;
end;

{ TVTWindow }

procedure TVTWindow.Draw;
var
  frame: String;
  i: integer;
begin
  frame:=DupeString(' ', FPosition.Right);
  FTerm.MoveTo(FPosition.Top, FPosition.Left);
  FScreen.Inverse:=True;
  for i:=0 to FPosition.Bottom-1 do
  begin
    FTerm.MoveTo(FPosition.Top+i, FPosition.Left);
    FTerm.Write(frame);
  end;
  FTerm.MoveTo(FPosition.Top, FPosition.Left);
  FTerm.Write('[X] '+FTitle);
  frame:=DupeString(' ', FPosition.Right-2);
  FScreen.Inverse:=False;
  for i:=1 to FPosition.Bottom-2 do
  begin
    FTerm.MoveTo(FPosition.Top+i, FPosition.Left+1);
    FTerm.Write(frame);
  end;
  MoveTo(0, 0);
end;

constructor TVTWindow.Create(AOwner: TComponent; const title: string; x, y, w,
  h: integer);
begin
  inherited Create(AOwner);
  FTitle:=title;
  FPosition.Left:=y;
  FPosition.Top:=x;
  FPosition.Right:=w;
  FPosition.Bottom:=h;
end;

destructor TVTWindow.Destroy;
var
  i: integer;
  frame: string;
begin
  frame:=DupeString(' ', FPosition.Right);
  for i:=0 to FPosition.Bottom-1 do
  begin
    FTerm.MoveTo(FPosition.Top+i, FPosition.Left);
    FTerm.Write(frame);
  end;
  inherited Destroy;
end;

procedure TVTWindow.write(const s: string);
begin
  FTerm.MoveTo(FCurPos.x, FCurPos.y);
  FTerm.Write(s);
  Inc(FCurPos.y, Length(s));
end;

procedure TVTWindow.writeln(const s: string);
begin
  FTerm.MoveTo(FCurPos.x, FCurPos.y);
  FTerm.Write(s);
  Inc(FCurPos.x);
end;

procedure TVTWindow.MoveTo(const x, y: integer);
begin
  FCurPos.x:=FPosition.Top+1+x;
  FCurPos.y:=FPosition.Left+1+y;
  FTerm.MoveTo(FCurPos.x, FCurPos.y);
end;

procedure TVTWindow.HandleMouse(const Button, Row, Col: integer);
begin
  if (Button = 0) and (Row = FPosition.Top) and (Col = FPosition.Left+1) then
    System.WriteLn('Window close requested!');
end;

procedure TVTWindow.Render;
var
  i: integer;
  w: TVTWidget;
begin
  Draw;
  for i:=0 to ComponentCount-1 do
  begin
    w:=TVTWidget(Components[i]);
    w.Render;
  end;
end;

procedure TVTWindow.AddLabel(const s: string);
begin
  TVTLabel.Create(Self, s);
end;

{ TVTDataLabel }

procedure TVTDataLabel.Draw;
begin
  FTerm.MoveTo(FPosition.x, FPosition.y);
  FScreen.Inverse:=True;
  FTerm.Write(FLabel);
  FScreen.Inverse:=False;
  FTerm.Write(' '+FData);
end;

procedure TVTDataLabel.SetLabel(value: string);
begin
  FLabel:=value;
  Draw;
end;

procedure TVTDataLabel.SetData(value: string);
begin
  FData:=value;
  Draw;
end;

constructor TVTDataLabel.Create(AOwner: TComponent; x, y: integer);
begin
  inherited Create(AOwner);
  FPosition.x:=x;
  FPosition.y:=y;
end;

{ TVTBSOD }

constructor TVTBSOD.Create(AOwner: TComponent; msg: string);
begin
  inherited Create(AOwner);
  FScreen.SetColor(tcWhite, tcBlue);
  FScreen.Inverse:=True;
  FScreen.Center(0, 'Website Error!');
  FScreen.Inverse:=False;
  FTerm.MoveTo(10,10);
  Fterm.Write('Website Error: '+msg);
end;

{ TVTWidget }

constructor TVTWidget.Create(AOwner: TComponent);
var
  w: TVTWindow;
begin
  inherited Create(AOwner);
  FTerm:=TWidgetContainer(AOwner).WebTerminal;
  FScreen:=TWidgetContainer(AOwner).Screen;
  if AOwner.ClassName = 'TWidgetContainer' then
  begin
    FWrite:=@FTerm.Write;
  end
  else if AOwner.ClassName = 'TVTWindow' then
  begin
    w:=TVTWindow(AOwner);
    FWrite:=@w.write;
  end;
  FState:=wsActive;
end;

procedure TVTWidget.HandleMouse(const Button, Row, Col: integer);
begin
  // Is overridden by sub-classes which need to handle mouse events.
end;

procedure TVTWidget.Render;
begin
  // Override in sub-classes
end;

procedure TVTWidget.SetInactive;
begin
  FState:=wsInactive;
end;

{ TVTDramaPrint }

procedure TVTDramaPrint.PrintChar(Sender: TObject);
begin
  FTerm.MoveTo(FPosition.x, FPosition.y);
  FTerm.Write(FString.slice(0, FCharPos));
  Inc(FCharPos);
  if FCharPos = FString.length+1 then
  begin
    FTimer.Enabled:=False;
    if Assigned(FOnComplete) then
      FOnComplete;
    SetInactive;
  end;
end;

constructor TVTDramaPrint.Create(AOwner: TComponent; msg: string; x, y: integer
  );
begin
  inherited Create(AOwner);
  FString:=TJSString.New(msg);
  FPosition.x:=x;
  FPosition.y:=y;
  FCharPos:=0;
  FTimer:=TTimer.Create(Self);
  FTimer.Interval:=50;
  FTimer.OnTimer:=@PrintChar;
end;

constructor TVTDramaPrint.Create(AOwner: TComponent; msg: string; x,
  y: integer; OnComplete: TProc);
begin
  Create(AOwner, msg, x, y);
  FOnComplete:=OnComplete;
end;

{ TVTScreen }

procedure TVTScreen.HandleInput(const data: string);
begin

end;

procedure TVTScreen.HandleCtrl(const data: string);
begin
  if (data = 'F5') and Assigned(FOnRefresh) then
    FOnRefresh;
end;

procedure TVTScreen.HandleMouse(Button, col, row: Integer);
var
  i: integer;
  w: TVTWidget;
begin
  for i:=0 to FWidgets.ComponentCount-1 do
  begin
    w:=TVTWidget(FWidgets.Components[i]);
    w.HandleMouse(Button, row, col);
  end;
end;

procedure TVTScreen.SetupScreen;
begin
  FWidgets:=TWidgetContainer.Create(Self);
  FTimer:=TTimer.Create(Self);
  FTimer.OnTimer:=@WidgetStateChange;
  FTerm.OnPayload:=@HandleInput;
  FTerm.OnControlCode:=@HandleCtrl;
  FTerm.OnMouse:=@HandleMouse;
  FTerm.Mode:=tmRaw;
  FTerm.Mouse:=True;
  FForeground:=taForeground+tcWhite;
  FBackground:=taBackground+tcBlack;
end;

procedure TVTScreen.SetInverse(value: boolean);
begin
  if value then
    FTerm.Attr:=7
  else
  begin
    FTerm.Attr:=0;
    FTerm.Attr:=taForeground+FForeground;
    FTerm.Attr:=taBackground+FBackground;
  end;
end;

constructor TVTScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTerm:=TWebTerminal.Create;
  SetupScreen;
end;

constructor TVTScreen.Create(AOwner: TComponent; const container: string);
begin
  inherited Create(AOwner);
  { TODO : This doesn't work and initialize the VT100 for some reason }
  FTerm:=TWebTerminal.Create(container);
  SetupScreen;
end;

destructor TVTScreen.Destroy;
begin
  FTerm.Free;
  inherited Destroy;
end;

procedure TVTScreen.DramaPrint(const msg: string; x, y: integer);
begin
  TVTDramaPrint.Create(FWidgets, msg, x, y);
end;

procedure TVTScreen.DramaPrint(const msg: string; x, y: integer;
  OnComplete: TProc);
begin
  TVTDramaPrint.Create(FWidgets, msg, x, y, OnComplete);
end;

procedure TVTScreen.SetColor(const fg, bg: integer);
begin
  FForeground:=fg;
  FBackground:=bg;
  FTerm.Attr:=taForeground+FForeground;
  FTerm.Attr:=taBackground+FBackground;
  Fterm.Clear;
end;

procedure TVTScreen.BSOD(const msg: string);
begin
  Clear;
  FTimer.Enabled:=False;
  TVTBSOD.Create(FWidgets, msg);
end;

procedure TVTScreen.CountDown(secs: integer; OnCountDown: TProc);
begin
  FCountDown:=secs;
  FOnCountDown:=OnCountDown;
end;

procedure TVTScreen.Center(const row: integer; const msg: string);
begin
  WriteLn(FTerm.Cols);
  FTerm.MoveTo(row, ((FTerm.Cols-Length(msg)) div 2));
  FTerm.Write(msg);
end;

procedure TVTScreen.Clear;
var
  i: integer;
  w: TVTWidget;
begin
  for i:=FWidgets.ComponentCount-1 downto 0 do
  begin
    w:=TVTWidget(FWidgets.Components[i]);
    w.Free;
  end;
  FTerm.Clear;
end;

procedure TVTScreen.WidgetStateChange(Sender: TObject);
var
  i: integer;
  w: TVTWidget;
begin
  for i:=FWidgets.ComponentCount-1 downto 0 do
  begin
    w:=TVTWidget(FWidgets.Components[i]);
    if w.WidgetState = wsInactive then
      w.Free;
  end;
  Dec(FCountDown);
  if (FCountDown = 0) and Assigned(FOnCountDown) then
    FOnCountDown;
  {for i:=0 to ComponentCount-1 do
  begin
    w:=TVTWidget(Components[i]);
    try
      w.doIdle;
    except
      on EJS do BSOD('EJS Error!');
      else BSOD('TypeError!');
    end;
  end;}
end;

end.


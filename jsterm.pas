unit jsterm;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  libjquery, JS, strutils;

type

  TCommandCallback = reference to procedure(command: string; term: TJQuery);
  TOnInitCallback = reference to procedure(term: TJQuery);
  TOnExitCallback = reference to procedure(term: TJQuery);
  TLoginEvent = reference to procedure(token: string);
  TLoginCallback = reference to procedure(user, password: string; callback: TLoginEvent);

  TJSTerminalOptions = Class external name 'Object' (TJSObject)
    greeting: string; external name 'greetings';
    height: integer; external name 'height';
    width: integer; external name 'width';
    prompt: string; external name 'prompt';
    exit: boolean; external name 'exit';
    onInit: TOnInitCallback; external name 'onInit';
    onExit: TOnExitCallback; external name 'onExit';
    OnLogin: TLoginCallback; external name 'login';
  end;

  TTerminalState = Class external name 'Object' (TJSObject)
    name: string; external name 'name';
    prompt: string; external name 'prompt';
    onInit: TOnInitCallback; external name 'onStart';
    onExit: TOnInitCallback; external name 'onExit';
    onLogin: TLoginCallback; external name 'login';
    infiniteLogin: boolean; external name 'infiniteLogin';
  end;

  { TJSTerminal }

  TJSTerminal = Class helper for TJQuery
    function Terminal(cmdCB: TCommandCallback; params: TJSTerminalOptions): TJQuery; external name 'terminal';
    procedure Echo(msg: string); external name 'echo';
    procedure Error(msg: string); external name 'error';
    procedure Clear; external name 'clear';
    procedure Push(cmdCB: TCommandCallback; state: TTerminalState); external name 'push';
    procedure Pop; external name 'pop';
    procedure Logout; external name 'logout';
    function Token(local: boolean): string; external name 'token';
    procedure Purge; external name 'purge';
  private
    procedure SetPrompt(prompt: string); external name 'set_prompt';
    procedure SetMask(value: boolean); external name 'set_mask';
    function GetPrompt: string; external name 'get_prompt';
    function GetLevel: integer; external name 'level';
    function GetLogin: string; external name 'login_name';
    function GetName: string; external name 'name';
    procedure Pause; external name 'pause';
    procedure Resume; external name 'resume';
    procedure SetEnabled(value: boolean);
    function GetEnabled: boolean; external name 'paused';
  public
    property Prompt: string read GetPrompt write SetPrompt;
    property Mask: boolean write SetMask;
    property Level: integer read GetLevel;
    property Username: string read GetLogin;
    property Name: string read GetName;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

function InitTerminal(cntr: string; cmdCB: TCommandCallback; params: TJSTerminalOptions; initCB: TOnInitCallback): TJQuery;
function SimpleTerm: TJSTerminalOptions;
function LoginTerm(greeting, prompt: string; loginCB: TLoginCallback): TJSTerminalOptions;
function SimpleState(name, prompt: string): TTerminalState;
function LoginState(name, prompt: string; loginCB: TLoginCallback): TTerminalState;

function getToken(var src: string): string;

implementation

function InitTerminal(cntr: string; cmdCB: TCommandCallback; params: TJSTerminalOptions; initCB: TOnInitCallback): TJQuery;
begin
  params.onInit:=initCB;
  Result:=JQuery('#'+cntr).Terminal(cmdCB, params);
end;

function SimpleTerm: TJSTerminalOptions;
begin
  Result:=TJSTerminalOptions.new;
  With Result do
  begin
    height:=480;
    greeting:='';
    exit:=True;
  end;
end;

function LoginTerm(greeting, prompt: string; loginCB: TLoginCallback
  ): TJSTerminalOptions;
begin
  Result:=SimpleTerm;
  Result.greeting:=greeting;
  Result.prompt:=prompt;
  Result.onLogin:=loginCB;
end;

function SimpleState(name, prompt: string): TTerminalState;
begin
  Result:=TTerminalState.new;
  Result.name:=name;
  Result.prompt:=prompt;
end;

function LoginState(name, prompt: string; loginCB: TLoginCallback
  ): TTerminalState;
begin
  Result:=SimpleState(name, prompt);
  Result.onLogin:=loginCB;
end;

function getString(var src: string): string;
begin
  src:=RightStr(src, Length(src)-1);
  Result:=Copy2SymbDel(src, '"');
end;

function getToken(var src: string): string;
begin
  if src[1] = '"' then
    Result:=getString(src)
  else
    Result:=Copy2SpaceDel(src);
end;

{ TJSTerminal }

procedure TJSTerminal.SetEnabled(value: boolean);
begin
  if value then
    Resume
  else
    Pause;
end;

end.


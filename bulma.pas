unit bulma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, web, Types;

type

  { TBulmaWidget }

  TBulmaWidget = class(TComponent)
  private
    FBody: TJSHTMLElement;
  public
    constructor Create(AOwner: TComponent; const target: string);
    procedure setContent(const content: string);
  end;

  { TBulmaModal }

  TBulmaModal = class(TBulmaWidget)
  private
    FModal: TJSHTMLElement;
    FTarget: string;
    FOnClose: TProc;
    FOnSave, FOnCancel: THTMLClickEventHandler;
    function GetCancelText: string;
    function GetSaveText: string;
    function GetTitle: string;
    function HandleClose(aEvent : TJSMouseEvent) : boolean;
    procedure SetCancelText(AValue: string);
    procedure SetOnCancel(AValue: THTMLClickEventHandler);
    procedure SetOnSave(AValue: THTMLClickEventHandler);
    procedure SetSaveText(AValue: string);
    procedure SetTitle(AValue: string);
  public
    property Title: string read GetTitle write SetTitle;
    property OnSave: THTMLClickEventHandler read FOnSave write SetOnSave;
    property OnCancel: THTMLClickEventHandler read FOnCancel write SetOnCancel;
    property SaveText: string read GetSaveText write SetSaveText;
    property CancelText: string read GetCancelText write SetCancelText;
    constructor Create(AOwner: TComponent; const target: string);
    procedure SetOnClose(AProc: TProc);
    procedure ShowModal;
    procedure HideModal;
  end;

  { TBulmaMenuItem }

  TBulmaMenuItem = class(TComponent)
  private
    FTitle: string;
    FItemID: string;
    FOnClick: THTMLClickEventHandler;
  public
    constructor Create(AOwner: TComponent; ATitle, ItemID: string; onClick: THTMLClickEventHandler);
    function renderHTML: string;
    procedure Bind;
  end;

  { TBulmaMenuSection }

  TBulmaMenuSection = class(TComponent)
  private
    FTitle: string;
  public
    constructor Create(AOwner: TComponent; ATitle: string);
    procedure AddItem(const Title, ItemID: string; onClick: THTMLClickEventHandler);
    function renderHTML: string;
    procedure Bind;
  end;

  { TBulmaMenu }

  TBulmaMenu = class(TBulmaWidget)
  private
    FSections: Array of TBulmaMenuSection;
    procedure Bind;
  public
    function AddSection(ATitle: string): TBulmaMenuSection;
    procedure renderHTML;
  end;

  { TBulmaTab }

  TBulmaTab = class(TComponent)
  private
    FTitle: string;
    FItemID: string;
    FOnClick: THTMLClickEventHandler;
    FActive: Boolean;
    FTab: TJSHTMLElement;
    procedure SetActive(AValue: Boolean);
    function TabClicked(aEvent: TJSMouseEvent): Boolean;
  public
    property Active: Boolean read FActive write SetActive;
    constructor Create(AOwner: TComponent; ATitle, ItemID: string; onClick: THTMLClickEventHandler);
    function renderHTML: string;
    procedure Bind;
  end;

  TBulmaTabType = (tbNone, tbCenter, tbBoxed, tbToggle, tbToggleRounded,
                   tbFullWidth);

  { TBulmaTabs }

  TBulmaTabs = class(TBulmaWidget)
  private
    FTabs: Array of TBulmaTab;
    FActiveTab: TBulmaTab;
    FTabType: TBulmaTabType;
    procedure Bind;
    procedure SetActiveTab(AValue: TBulmaTab);
  public
    property ActiveTab: TBulmaTab read FActiveTab write SetActiveTab;
    property TabType: TBulmaTabType read FTabType write FTabType;
    function AddTab(ATitle, ItemID: string; onClick: THTMLClickEventHandler): TBulmaTab;
    procedure renderHTML;
  end;

  { TBulmaCard }

  TBulmaCard = class(TComponent)
  private
    FCardImage: string;
    FCardTitle: string;
    FCardHeader: string;
    FCardBody: string;
    FTarget: string;
    FCard: TJSHTMLElement;
    procedure SetCardBody(AValue: string);
  public
    property CardImage: string read FCardImage write FCardImage;
    property CardTitle: string read FCardTitle write FCardTitle;
    property CardHeader: string read FCardHeader write FCardHeader;
    property CardBody: string read FCardBody write SetCardBody;
    constructor Create(AOwner: TComponent; const target: string);
    function renderHTML: string;
  end;

  { TBulmaField }

  TBulmaField = class(TComponent)
  private
    FLabel: string;
    FFieldID: string;
    FHelpText: string;
    FHasIcons: Boolean;
  public
    property HelpText: string read FHelpText write FHelpText;
    constructor Create(AOwner: TComponent; ALabel, AFieldID: string);
    function renderHTML: string;
    function renderControl: string; virtual; abstract;
  end;

  TBulmaControlType = (ctInput, ctEmail, ctPassword);

  { TBulmaInput }

  TBulmaInput = class(TBulmaField)
  private
    FControlType: TBulmaControlType;
    FElement: TJSHTMLInputElement;
    function GetJSValue: TJSString;
    function GetValue: string;
    procedure SetControlType(AValue: TBulmaControlType);
    procedure SetValue(AValue: string);
    procedure Setup;
  public
    property ControlType: TBulmaControlType read FControlType write SetControlType;
    property Value: string read GetValue write SetValue;
    property JSValue: TJSString read GetJSValue;
    function renderControl: string; override;
  end;

  { TBulmaForm }

  TBulmaForm = class(TComponent)
  private
    FFields: TComponent;
    FOnSubmit: THTMLClickEventHandler;
    FSubmitText: string;
  public
    property OnSubmit: THTMLClickEventHandler read FOnSubmit write FOnSubmit;
    property SubmitText: string read FSubmitText write FSubmitText;
    constructor Create(AOwner: TComponent);
    function AddInput(ALabel, AFieldID: string): TBulmaInput;
    function renderHTML: string;
    procedure Bind;
  end;

  { TBulmaButton }

  TBulmaButton = class(TComponent)
  private
    FTitle, FFieldID: string;
    FOnClick: THTMLClickEventHandler;
    FStyle: string;
  public
    property ButtonStyle: string read FStyle write FStyle;
    constructor Create(AOwner: TComponent; ATitle, AFieldID: string; onClick: THTMLClickEventHandler);
    function renderHTML: string;
    procedure Bind;
  end;

implementation

{ TBulmaButton }

constructor TBulmaButton.Create(AOwner: TComponent; ATitle, AFieldID: string;
  onClick: THTMLClickEventHandler);
begin
  inherited Create(AOwner);
  FTitle:=ATitle;
  FFieldID:=AFieldID;
  FOnClick:=onClick;
  FStyle:='';
end;

function TBulmaButton.renderHTML: string;
begin
  Result:='<button id="'+FFieldID+'" class="button '+FStyle+'">'+FTitle+'</button>';
end;

procedure TBulmaButton.Bind;
var
  e: TJSHTMLElement;
begin
  e:=TJSHTMLElement(document.getElementById(FFieldID));
  e.onclick:=FOnClick;
end;

{ TBulmaInput }

function TBulmaInput.GetValue: string;
begin
  Setup;
  Result:=FElement.value;
end;

function TBulmaInput.GetJSValue: TJSString;
begin
  Setup;
  Result:=TJSString(FElement.value);
end;

procedure TBulmaInput.SetControlType(AValue: TBulmaControlType);
begin
  if FControlType=AValue then Exit;
  FControlType:=AValue;
  if (FControlType = ctEmail) or (FControlType = ctPassword) then
    FHasIcons:=True
  else
    FHasIcons:=False;
end;

procedure TBulmaInput.SetValue(AValue: string);
begin
  Setup;
  FElement.value:=AValue;
end;

procedure TBulmaInput.Setup;
begin
  FElement:=TJSHTMLInputElement(document.getElementById(FFieldID));
end;

function TBulmaInput.renderControl: string;
var
  htype, icon, buf: string;
begin
  if FControlType = ctInput then
  begin
    htype:='text';
    icon:='';
  end
  else if FControlType = ctEmail then
  begin
    htype:='email';
    icon:='envelope';
  end
  else if FControlType = ctPassword then
  begin
    htype:='password';
    icon:='lock';
  end;
  buf:='<input id="'+FFieldID+'" class="input" type="'+htype+'"/>';
  if icon <> '' then
  begin
    buf:=buf+'<span class="icon is-small is-left">';
    buf:=buf+'<i class="fas fa-'+icon+'"></i></span>';
  end;
  Result:=buf;
end;

{ TBulmaField }

constructor TBulmaField.Create(AOwner: TComponent; ALabel, AFieldID: string);
begin
  inherited Create(AOwner);
  FLabel:=ALabel;
  FFieldID:=AFieldID;
  FHelpText:='';
  FHasIcons:=False;
end;

function TBulmaField.renderHTML: string;
var
  buf: string;
begin
  buf:='<div class="field"><label class="label">'+FLabel+'</label>';
  buf:=buf+'<div class="control';
  if FHasIcons then
    buf:=buf+' has-icons-left';
  buf:=buf+'">'+renderControl+'</div>';
  if FHelpText <> '' then
    buf:=buf+'<p class="help">'+FHelpText+'</p>';
  Result:=buf+'</div>';
end;

{ TBulmaForm }

constructor TBulmaForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields:=TComponent.Create(Self);
end;

function TBulmaForm.AddInput(ALabel, AFieldID: string): TBulmaInput;
begin
  Result:=TBulmaInput.Create(FFields, ALabel, AFieldID);
end;

function TBulmaForm.renderHTML: string;
var
  buf: string;
  i: Integer;
begin
  buf:='';
  for i:=0 to FFields.ComponentCount-1 do
    buf:=buf+TBulmaField(FFields.Components[i]).renderHTML;
  if FSubmitText <> '' then
  begin
    buf:=buf+'<div class="field"><p class="control">';
    buf:=buf+'<a id="formSubmit" class="button is-primary">'+FSubmitText+'</a>';
    buf:=buf+'</p></div>';
  end;
  Result:=buf;
end;

procedure TBulmaForm.Bind;
var
  e: TJSHTMLElement;
begin
  if FSubmitText = '' then
    Exit;
  e:=TJSHTMLElement(document.getElementById('formSubmit'));
  e.onclick:=FOnSubmit;
end;

{ TBulmaCard }

procedure TBulmaCard.SetCardBody(AValue: string);
begin
  if FCardBody=AValue then Exit;
  FCardBody:=AValue;
end;

constructor TBulmaCard.Create(AOwner: TComponent; const target: string);
begin
  inherited Create(AOwner);
  FTarget:=target;
  {FCard:=TJSHTMLElement(document.getElementById(target));}

end;

function TBulmaCard.renderHTML: string;
var
  buf: string;
begin
  buf:='<div class="card">';
  if FCardImage <> '' then
  begin
    buf:=buf+'<div class="card-image"><figure class="image is-4by3">';
    buf:=buf+'<img src="'+FCardImage+'"/></figure></div>';
  end;
  if FCardHeader <> '' then
  begin
    buf:=buf+'<header class="card-header"><p class="card-header-title">';
    buf:=buf+FCardHeader+'</p></header>';
  end;
  buf:=buf+'<div class="card-content"><div class="content">'+FCardBody;
  Result:=buf+'</div></div></div>';
end;

{ TBulmaTab }

function TBulmaTab.TabClicked(aEvent: TJSMouseEvent): Boolean;
var
  tabs: TBulmaTabs;
begin
  tabs:=TBulmaTabs(Owner);
  if tabs.ActiveTab = Self then
    Exit;
  tabs.ActiveTab:=Self;
  if Assigned(FOnClick) then
    Result:=FOnClick(aEvent)
  else
    Result:=False;
end;

procedure TBulmaTab.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if FActive then
    FTab.classList.add('is-active')
  else
    FTab.classList.remove('is-active');
end;

constructor TBulmaTab.Create(AOwner: TComponent; ATitle, ItemID: string;
  onClick: THTMLClickEventHandler);
begin
  inherited Create(AOwner);
  FTitle:=ATitle;
  FItemID:=ItemID;
  FOnClick:=onClick;
  FActive:=False;
end;

function TBulmaTab.renderHTML: string;
var
  isactive: string;
begin
  if FActive then
    isactive:=' class="is-active"'
  else
    isactive:='';
  Result:='<li id="t'+FItemID+'"'+isactive+'><a id="a'+FItemID+'">'+FTitle+'</a></li>';
end;

procedure TBulmaTab.Bind;
var
  e: TJSHTMLElement;
begin
  FTab:=TJSHTMLElement(document.getElementById('t'+FItemID));
  e:=TJSHTMLElement(document.getElementById('a'+FItemID));
  e.onclick:=@TabClicked;
end;

{ TBulmaTabs }

procedure TBulmaTabs.Bind;
var
  i: Integer;
begin
  for i:=0 to Length(FTabs)-1 do
    FTabs[i].Bind;
end;

procedure TBulmaTabs.SetActiveTab(AValue: TBulmaTab);
begin
  if FActiveTab=AValue then Exit;
  FActiveTab.Active:=False;
  FActiveTab:=AValue;
  FActiveTab.Active:=True;
end;

function TBulmaTabs.AddTab(ATitle, ItemID: string;
  onClick: THTMLClickEventHandler): TBulmaTab;
var
  i: Integer;
begin
  i:=Length(FTabs);
  SetLength(FTabs, i+1);
  Result:=TBulmaTab.Create(Self, ATitle, ItemID, onClick);
  FTabs[i]:=Result;
  if i = 0 then
  begin
    FActiveTab:=Result;
    FActiveTab.FActive:=True;
  end;
end;

procedure TBulmaTabs.renderHTML;
var
  buf: string;
  i: Integer;
  cls: string;
begin
  case FTabType of
    tbCenter: cls:=' is-centered';
    tbBoxed: cls:=' is-boxed';
    tbToggle: cls:=' is-toggle';
    tbToggleRounded: cls:=' is-toggle is-toggle-rounded';
    tbFullWidth: cls:=' is-full-width';
  else
    cls:='';
  end;
  buf:='<div class="tabs'+cls+'"><ul>';
  for i:=0 to Length(FTabs)-1 do
    buf:=buf+FTabs[i].renderHTML;
  FBody.innerHTML:=buf+'</ul></div>';
  Bind;
end;

{ TBulmaMenuItem }

constructor TBulmaMenuItem.Create(AOwner: TComponent; ATitle, ItemID: string;
  onClick: THTMLClickEventHandler);
begin
  inherited Create(AOwner);
  FTitle:=ATitle;
  FItemID:=ItemID;
  FOnClick:=onClick;
end;

function TBulmaMenuItem.renderHTML: string;
begin
  Result:='<li><a id="'+FItemID+'" href="#/docs/'+FItemID+'.md">'+FTitle+'</a></li>';
end;

procedure TBulmaMenuItem.Bind;
var
  e: TJSHTMLElement;
begin
  e:=TJSHTMLElement(document.getElementById(FItemID));
  e.onclick:=FOnClick;
end;

{ TBulmaMenuSection }

constructor TBulmaMenuSection.Create(AOwner: TComponent; ATitle: string);
begin
  inherited Create(AOwner);
  FTitle:=ATitle;
end;

procedure TBulmaMenuSection.AddItem(const Title, ItemID: string;
  onClick: THTMLClickEventHandler);
var
  c: TBulmaMenuItem;
begin
  c:=TBulmaMenuItem.Create(Self, Title, ItemID, onClick);
end;

function TBulmaMenuSection.renderHTML: string;
var
  i: integer;
begin
  Result:='<p class="menu-label">'+FTitle+'</p><ul class="menu-list">';
  for i:=0 to ComponentCount-1 do
    Result:=Result+TBulmaMenuItem(Components[i]).renderHTML;
  Result:=Result+'</ul>';
end;

procedure TBulmaMenuSection.Bind;
var
  i: integer;
begin
  for i:=0 to ComponentCount-1 do
    TBulmaMenuItem(Components[i]).Bind;
end;

{ TBulmaMenu }

function TBulmaMenu.AddSection(ATitle: string): TBulmaMenuSection;
var
  i: integer;
begin
  i:=Length(FSections);
  SetLength(FSections, i+1);
  Result:=TBulmaMenuSection.Create(Self, ATitle);
  FSections[i]:=Result;
end;

procedure TBulmaMenu.renderHTML;
var
  i: integer;
  s: string;
begin
  for i:=0 to Length(FSections)-1 do
    s:=s+FSections[i].renderHTML;
  FBody.innerHTML:=s;
  Bind;
end;

procedure TBulmaMenu.Bind;
var
  i: integer;
begin
  for i:=0 to Length(FSections)-1 do
    FSections[i].Bind;
end;

{ TBulmaModal }

function TBulmaModal.HandleClose(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  if Assigned(FOnClose) then
    FOnClose;
  HideModal;
end;

procedure TBulmaModal.SetCancelText(AValue: string);
begin
  TJSHTMLElement(document.getElementById(FTarget+'Cancel')).innerHTML:=AValue;
end;

procedure TBulmaModal.SetOnCancel(AValue: THTMLClickEventHandler);
begin
  if FOnCancel=AValue then Exit;
  FOnCancel:=AValue;
  TJSHTMLElement(document.getElementById(FTarget+'Cancel')).onclick:=AValue;
end;

procedure TBulmaModal.SetOnSave(AValue: THTMLClickEventHandler);
begin
  if FOnSave=AValue then Exit;
  FOnSave:=AValue;
  TJSHTMLElement(document.getElementById(FTarget+'Save')).onclick:=AValue;
end;

procedure TBulmaModal.SetSaveText(AValue: string);
begin
  TJSHTMLElement(document.getElementById(FTarget+'Save')).innerHTML:=AValue;
end;

function TBulmaModal.GetTitle: string;
begin
  Result:=TJSHTMLElement(document.getElementById(FTarget+'Title')).innerHTML;
end;

function TBulmaModal.GetSaveText: string;
begin
  Result:=TJSHTMLElement(document.getElementById(FTarget+'Save')).innerHTML;
end;

function TBulmaModal.GetCancelText: string;
begin
  Result:=TJSHTMLElement(document.getElementById(FTarget+'Cancel')).innerHTML;
end;

procedure TBulmaModal.SetTitle(AValue: string);
begin
  TJSHTMLElement(document.getElementById(FTarget+'Title')).innerHTML:=AValue;
end;

constructor TBulmaModal.Create(AOwner: TComponent; const target: string);
var
  e: TJSHTMLElement;
begin
  inherited Create(AOwner, target);
  FTarget:=target;
  FModal:=TJSHTMLElement(document.getElementById(target+'Modal'));
  e:=TJSHTMLElement(document.getElementById(target+'Close'));
  e.onclick:=@HandleClose;
end;

procedure TBulmaModal.SetOnClose(AProc: TProc);
begin
  FOnClose:=AProc;
end;

procedure TBulmaModal.ShowModal;
begin
  FModal.classList.add('is-active');
end;

procedure TBulmaModal.HideModal;
begin
  FModal.classList.remove('is-active');
end;

{ TBulmaWidget }

procedure TBulmaWidget.setContent(const content: string);
begin
  FBody.innerHTML:=content;
end;

constructor TBulmaWidget.Create(AOwner: TComponent; const target: string);
begin
  inherited Create(AOwner);
  FBody:=TJSHTMLElement(document.getElementById(target));
end;

end.


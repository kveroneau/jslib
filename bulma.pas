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
    FOnClose: TProc;
    function HandleClose(aEvent : TJSMouseEvent) : boolean;
  public
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

implementation

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
  Result:='<li><a id="'+FItemID+'">'+FTitle+'</a></li>';
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

constructor TBulmaModal.Create(AOwner: TComponent; const target: string);
var
  e: TJSHTMLElement;
begin
  inherited Create(AOwner, target);
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


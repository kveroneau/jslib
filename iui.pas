unit iui;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, Web;

type
  TAjaxErrEvent = reference to procedure(msg: string);

  TiUI = class external name 'iUI' (TJSObject)
  public
    logging, busy, animOn: boolean;
    ajaxErrHandler: TAjaxErrEvent;
    httpHeaders: TJSObject;
    procedure showPage(page: string; backwards: boolean);
    procedure gotoView(view: string; replace: boolean);
    procedure showPageById(pageId: string);
    procedure goBack;
    procedure replacePage(view: string);
    function getSelectedPage: TJSHTMLElement;
    function getAllViews: TJSArray;
    procedure insertPages(frag: TJSHTMLElement);
  end;

  TScreenType = (stBasic, stPanel, stList, stForm);

  { TiUIScreen }

  TiUIScreen = class(TObject)
  private
    FPage: TJSHTMLElement;
    FID: string;
    FScreenType: TScreenType;
    function CreateHTMLElement(aTag: String; aID: String = ''): TJSHTMLElement;
  public
    constructor Create(ScreenType: TScreenType; aId, aTitle: string); virtual;
    procedure AddContent(aElement: TJSHTMLElement);
    procedure AddHTML(html: string);
    procedure AddListHeader(title: string);
    procedure AddListItem(item: string);
    procedure Go; virtual;
  end;

  { TiUIForm }

  TiUIForm = class(TiUIScreen)
  private
    FForm: TJSHTMLElement;
  public
    constructor Create(aId, aTitle, aName, aAction, aMethod: string);
    procedure AddTextField(aTitle, aId, aName, aType, aPlaceHolder: string);
    procedure AddBooleanField(aTitle: string; AOnClick: THTMLClickEventHandler);
    procedure AddSubmitButton(aTitle: string; AOnClick: THTMLClickEventHandler);
    function GetFormValue(aId: string): string;
    procedure Go; override;
  end;

var
  iUIApp: TiUI; external name 'window.iui';

implementation

{ TiUIForm }

constructor TiUIForm.Create(aId, aTitle, aName, aAction, aMethod: string);
begin
  inherited Create(stForm, aId, aTitle);
  FPage.Attrs['name']:=aName;
  FPage.Attrs['action']:=aAction;
  FPage.Attrs['method']:=aMethod;
  FForm:=CreateHTMLElement('fieldset');
end;

procedure TiUIForm.AddTextField(aTitle, aId, aName, aType, aPlaceHolder: string
  );
var
  e, lbl, inp: TJSHTMLElement;
begin
  e:=CreateHTMLElement('div');
  e.Attrs['class']:='row';
  lbl:=CreateHTMLElement('label');
  lbl.Attrs['for']:=aId;
  lbl.innerText:=aTitle;
  inp:=CreateHTMLElement('input', aId);
  inp.Attrs['type']:=aType;
  inp.name:=aName;
  inp.Attrs['placeholder']:=aPlaceHolder;
  e.append(lbl);
  e.append(inp);
  FForm.append(e);
end;

procedure TiUIForm.AddBooleanField(aTitle: string; AOnClick: THTMLClickEventHandler);
var
  e, lbl, tgl, s1, s2, s3: TJSHTMLElement;
begin
  e:=CreateHTMLElement('div');
  e.Attrs['class']:='row';
  lbl:=CreateHTMLElement('label');
  lbl.innerText:=aTitle;
  tgl:=CreateHTMLElement('div');
  tgl.Attrs['class']:='toggle';
  tgl.onclick:=AOnClick;
  s1:=CreateHTMLElement('span');
  s1.className:='thumb';
  s2:=CreateHTMLElement('span');
  s2.className:='toggleOn';
  s2.innerText:='ON';
  s3:=CreateHTMLElement('span');
  s3.className:='toggleOff';
  s3.innerText:='OFF';
  tgl.append(s1);
  tgl.append(s2);
  tgl.append(s3);
  e.append(lbl);
  e.append(tgl);
  FForm.append(e);
end;

procedure TiUIForm.AddSubmitButton(aTitle: string;
  AOnClick: THTMLClickEventHandler);
var
  e, btn: TJSHTMLElement;
begin
  e:=CreateHTMLElement('div');
  e.className:='row';
  btn:=CreateHTMLElement('input');
  btn.Attrs['type']:='submit';
  btn.Attrs['value']:=aTitle;
  if Assigned(AOnClick) then
    btn.onclick:=AOnClick;
  e.append(btn);
  FForm.append(e);
end;

function TiUIForm.GetFormValue(aId: string): string;
var
  field: TJSHTMLInputElement;
begin
  field:=TJSHTMLInputElement(document.getElementById(aId));
  Result:=field.value;
end;

procedure TiUIForm.Go;
begin
  FPage.append(FForm);
  inherited Go;
end;

{ TiUIScreen }

function TiUIScreen.CreateHTMLElement(aTag: String; aID: String
  ): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.createElement(aTag));
  if aID<>'' then
    Result.ID:=aID;
end;

constructor TiUIScreen.Create(ScreenType: TScreenType; aId, aTitle: string);
var
  aTag: string;
begin
  FID:=aId;
  aTag:='div';
  if ScreenType = stList then
    aTag:='ul'
  else if ScreenType = stForm then
    aTag:='form';
  FScreenType:=ScreenType;
  FPage:=CreateHTMLElement(aTag, aId);
  FPage.Attrs['title']:=aTitle;
  if (ScreenType = stPanel) or (ScreenType = stForm) then
    FPage.Attrs['class']:='panel';
end;

procedure TiUIScreen.AddContent(aElement: TJSHTMLElement);
begin
  FPage.append(aElement);
end;

procedure TiUIScreen.AddHTML(html: string);
var
  e: TJSHTMLElement;
begin
  e:=CreateHTMLElement('p');
  e.innerHTML:=html;
  AddContent(e);
end;

procedure TiUIScreen.AddListHeader(title: string);
var
  e: TJSHTMLElement;
begin
  if FScreenType <> stList then
    Exit;
  e:=CreateHTMLElement('li');
  e.Attrs['class']:='group';
  e.innerHTML:=title;
  AddContent(e);
end;

procedure TiUIScreen.AddListItem(item: string);
var
  e: TJSHTMLElement;
begin
  if FScreenType <> stList then
    Exit;
  e:=CreateHTMLElement('li');
  e.innerHTML:=item;
  AddContent(e);
end;

procedure TiUIScreen.Go;
var
  span: TJSHTMLElement;
begin
  span:=CreateHTMLElement('span');
  span.append(FPage);
  iUIApp.insertPages(span);
  iUIApp.showPageById(FID);
end;

end.


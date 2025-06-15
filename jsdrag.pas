unit jsdrag;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, JS, Web;

type

  { Dragable }

  TDragable = class(TComponent)
  private
    FElement: TJSHTMLDivElement;
    pos1, pos2, pos3, pos4: Double;
    function MouseDown(AEvent: TJSMouseEvent): boolean;
    function MouseUp(AEvent: TJSMouseEvent): boolean;
    function MouseMove(AEvent: TJSMouseEvent): boolean;
  public
    constructor Create(AOwner: TComponent; AElement: string);
  end;

implementation

{ Dragable }

function TDragable.MouseDown(AEvent: TJSMouseEvent): boolean;
begin
  AEvent.preventDefault;
  Writeln('MouseDown');
  pos3:=AEvent.clientX;
  pos4:=AEvent.clientY;
  document.onmouseup:=@MouseUp;
  document.onmousemove:=@MouseDown;
end;

function TDragable.MouseUp(AEvent: TJSMouseEvent): boolean;
begin
  document.onmouseup:=Nil;
  document.onmousemove:=Nil;
end;

function TDragable.MouseMove(AEvent: TJSMouseEvent): boolean;
begin
  AEvent.preventDefault;
  pos1:=pos3-AEvent.clientX;
  pos2:=pos4-AEvent.clientY;
  pos3:=AEvent.clientX;
  pos4:=AEvent.clientY;
  FElement.style.setProperty('top', FloatToStr(FElement.offsetTop-pos2)+'px');
  FElement.style.setProperty('left', FloatToStr(FElement.offsetLeft-pos1)+'px');
end;

constructor TDragable.Create(AOwner: TComponent; AElement: string);
begin
  WriteLn('Object.');
  FElement:=TJSHTMLDivElement(document.getElementById(AElement));
  pos1:=0;
  pos2:=0;
  pos3:=0;
  pos4:=0;
  if Assigned(document.getElementById(AElement+'header')) then
    TJSHTMLDivElement(document.getElementById(AElement+'header')).onmousedown:=@MouseDown
  else
    FElement.onmousedown:=@MouseDown;
end;

end.


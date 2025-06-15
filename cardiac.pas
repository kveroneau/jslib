unit cardiac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TMemory = Array[0..99] of Integer;

  { TCardiacVM }

  TCardiacVM = class(TComponent)
  private
    FMemory: TMemory;
    FPC, FAcc: byte;
    FRunning: Boolean;
    procedure ClearMemory;
  public
    constructor Create(AOwner: TComponent); override; reintroduce;
  end;

implementation

{ TCardiacVM }

procedure TCardiacVM.ClearMemory;
var
  i: integer;
begin
  FPC:=0;
  FAcc:=0;
  FRunning:=False;
  for i:=1 to 98 do
    FMemory[i]:=-1;
  FMemory[0]:=1;
  FMemory[99]:=800;
end;

constructor TCardiacVM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClearMemory;
end;

end.


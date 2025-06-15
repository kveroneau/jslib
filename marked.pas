unit marked;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, Web;

type

  { TMarkdownFile }

  TMarkdownFile = class(TComponent)
  private
    FTarget: TJSHTMLElement;
    FFile: TStringStream;
    procedure onLoaded(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; const FileName, target: string; onLoadError: TStringNotifyEventRef);
  end;

function markdown(const s: string): string; external name 'window.marked.parse';

implementation

{ TMarkdownFile }

procedure TMarkdownFile.onLoaded(Sender: TObject);
begin
  FTarget.innerHTML:=markdown(FFile.DataString);
end;

constructor TMarkdownFile.Create(AOwner: TComponent; const FileName,
  target: string; onLoadError: TStringNotifyEventRef);
begin
  inherited Create(AOwner);
  FTarget:=TJSHTMLElement(document.getElementById(target));
  FFile:=TStringStream.Create;
  FFile.LoadFromURL(FileName, True, @onLoaded, onLoadError);
end;

end.


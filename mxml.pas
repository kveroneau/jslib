unit mxml;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, Web;

type
  TMagicXML = class external name 'magicXML' (TJSObject)
  public
    procedure configure(xmlSrcAttr, xslSrcAttr, xslParamAttr: string);
    function transform(xmlSource, xslSource: string): TJSNode;
    procedure transformAndReplace(target, xmlSource, xslSource: string);
    procedure parse(selector: string);
    procedure parse;
  end;

var
  MagicXML: TMagicXML; external name 'window.magicXML';

procedure LoadXMLContent(target, xmlSource, xslSource: string);

implementation

procedure LoadXMLContent(target, xmlSource, xslSource: string);
var
  content: TJSElement;
begin
  content:=document.getElementById(target);
  content.innerHTML:='';
  content.appendChild(MagicXML.transform(xmlSource, xslSource));
end;

end.


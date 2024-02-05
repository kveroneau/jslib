unit libvideojs;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS;

type
  TVideoJS = class external name 'VideoJS' (TJSObject)
  private
    function GetID: string; external name 'id';
  public
    property VideoID: string read GetID;
    procedure play;
    procedure pause;
  end;

  { TVideoWidget }

  TVideoWidget = class(TComponent)
  private
    FVideoJS: TVideoJS;
    FSources: TStringList;
    FTarget: String;
  public
    constructor Create(AOwner: TComponent; target: string);
    procedure AddSource(AFile: string);
    procedure AddStream(AStream: string);
    function renderHTML: string;
    procedure Bind;
  end;

var
  RTMP_SERVER: string;

function videojs(const target: string): TVideoJS; external name 'window.videojs';

implementation

{ TVideoWidget }

constructor TVideoWidget.Create(AOwner: TComponent; target: string);
begin
  inherited Create(AOwner);
  FSources:=TStringList.Create;
  FTarget:=target;
end;

procedure TVideoWidget.AddSource(AFile: string);
begin
  FSources.Add('<source src="'+AFile+'" type="video/mp4">');
end;

procedure TVideoWidget.AddStream(AStream: string);
begin
  FSources.Add('<source src="'+RTMP_SERVER+AStream+'.m3u8" type="application/x-mpegURL">');
end;

function TVideoWidget.renderHTML: string;
var
  i: Integer;
begin
  Result:='<video id="'+FTarget+'" controls width="640" height="480" class="video-js vjs-default-skin">';
  for i:=0 to FSources.Count-1 do
    Result:=Result+FSources.Strings[i];
  Result:=Result+'</video>';
end;

procedure TVideoWidget.Bind;
begin
  FVideoJS:=videojs(FTarget);
  FVideoJS.play;
end;

end.


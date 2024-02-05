unit notifications;

{$mode objfpc}
{$modeswitch externalclass}

{ Notifications only work with HTTPS and localhost. }

interface

uses JS, Web;

type
  TNotificationOptions = class external name 'Object' (TJSObject)
    dir, lang, badge, body, tag, icon, image, data, vibrate, renotify: string;
    requireInteraction: string;
    silent: boolean;
  end;

  TWebNotification = class external name 'Notification' (TJSObject)
  public
    class procedure requestPermission;
    constructor New(msg: string);
    constructor New(msg: string; options: TNotificationOptions);
    procedure close;
  end;

implementation

var
  NotifyPerms: string; external name 'Notification.permission';

procedure InitNotifications;
begin
  if NotifyPerms <> 'granted' then
    TWebNotification.requestPermission;
end;

initialization
  InitNotifications;

end.


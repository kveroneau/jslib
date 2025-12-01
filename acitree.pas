unit acitree;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  libjquery, JS;

type
  TTreeItem = Class external name 'Object' (TJSObject)
    id: Integer;
    title: string; external name 'label';
    inode: Boolean;
    branch: Array of TTreeItem;
  end;

  TTreeList = Array of TTreeItem;

  TTreeEvent = reference to procedure(parent, item: TJSObject; itemData: TTreeItem; level: TJSObject);

  TACITreeOptions = Class external name 'Object' (TJSObject)
    ajax: TJSObject;
    fullRow: Boolean;
    persist: string;
    itemHook: TTreeEvent;
  end;

  TItemOptions = Class external name 'Object' (TJSObject)
    itemData: TTreeItem;
  end;

  TListOptions = Class external name 'Object' (TJSObject)
    itemData: TJSArray;
  end;

  TItemLabel = Class external name 'Object' (TJSObject)
    title: string; external name 'label';
  end;

  { TTreeAPI }

  TTreeAPI = Class external name 'Object' (TJSObject)
    function itemData(item: TJSObject): TTreeItem;
    function first: TJSObject;
    procedure append(item: TJSObject; params: TItemOptions);
    procedure SetLabel(item: TJSObject; params: TItemLabel);
    procedure LoadFrom(item: TJSObject; params: TListOptions);
  end;

  TJSACITree = Class helper for TJQuery
    function ACITree(options: TACITreeOptions): TJQuery; external name 'aciTree';
    function ACITree(api: string): TTreeAPI; external name 'aciTree';
  end;

  { TACITree }

  TACITree = class(TObject)
  private
    FTree: TJQuery;
    FAPI: TTreeAPI;
  public
    property API: TTreeAPI read FAPI;
    constructor Create(params: TACITreeOptions);
    function Append(parent: TJSObject; ATitle: string; AId: integer; AInode: boolean): TJSObject;
    procedure SetLabel(item: TJSObject; ATitle: string);
    procedure LoadFrom(parent: TJSObject; ATree: TJSArray);
  end;

implementation

{ TACITree }

constructor TACITree.Create(params: TACITreeOptions);
begin
  FTree:=JQuery('#tree').ACITree(params);
  FAPI:=JQuery('#tree').ACITree('api');
end;

function TACITree.Append(parent: TJSObject; ATitle: string; AId: integer;
  AInode: boolean): TJSObject;
var
  opt: TItemOptions;
begin
  opt:=TItemOptions.new;
  opt.itemData:=TTreeItem.new;
  with opt.itemData do
  begin
    id:=AId;
    title:=ATitle;
    inode:=AInode;
  end;
  FAPI.append(parent, opt);
end;

procedure TACITree.SetLabel(item: TJSObject; ATitle: string);
var
  opt: TItemLabel;
begin
  opt:=TItemLabel.new;
  opt.title:=ATitle;
  FAPI.SetLabel(item, opt);
end;

procedure TACITree.LoadFrom(parent: TJSObject; ATree: TJSArray);
var
  opt: TListOptions;
begin
  opt:=TListOptions.new;
  opt.itemData:=ATree;
  FAPI.LoadFrom(parent, opt);
end;

end.


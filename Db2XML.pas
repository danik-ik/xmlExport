unit Db2XML;

interface
uses
  XMLDoc, XMLIntf, DB;

  type TDb2Xml = class
  private
    XmlDoc: TXMLDocument;
  public
    RootNode: IXmlNode;
    Constructor Create(FileName, RootNodeName, RootNodeParams: string);
    procedure Save;
  end;

  type TDb2XmlDataLayer = class
  private
    Rules: string;
    Parent_node: IXmlNode;
    Dataset: TDataSet;
  public
    Constructor Create(Rules: string; Dataset: TDataset);
    procedure Execute(ParentNode: IXmlNode);
  end;

implementation

uses SysUtils, DDD_Str;

{ TDb2Xml }

constructor TDb2Xml.Create(FileName, RootNodeName, RootNodeParams: string);
var
  s: string;
begin
  inherited Create;
  XmlDoc := TXMLDocument.Create('');
  with XmlDoc do
  begin
    Active := true;

    Version := '1.0';
    Encoding := 'UTF-8';
    rootNode:=AddChild(RootNodeName);
    s := CutString('=', RootNodeParams);
    rootNode.Attributes[s] := RootNodeParams;
  end;
  XmlDoc.FileName := FileName;
end;

procedure TDb2Xml.Save;
begin
  XmlDoc.SaveToFile();
end;

{ TDb2XmlDataLayer }

constructor TDb2XmlDataLayer.Create(Rules: string; Dataset: TDataset);
begin
  inherited Create;
end;

procedure TDb2XmlDataLayer.Execute(ParentNode: IXmlNode);
begin

end;

end.

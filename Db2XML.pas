unit Db2XML;

interface
uses
  XMLDoc, XMLIntf, DB;

  type TDb2XmlRoot = class
  private
    XmlDoc: TXMLDocument;
    function GetFileName: string;
    procedure setFileName(const Value: string);
  public
    RootNode: IXmlNode;
    Constructor Create(FileName, RootNodeName, RootNodeParams: string);
    procedure Save;
    property FileName: string read GetFileName write SetFileName;
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

constructor TDb2XmlRoot.Create(FileName, RootNodeName, RootNodeParams: string);
var
  s: string;
begin
  inherited Create;
  XmlDoc := TXMLDocument.Create('');
  with XmlDoc do
  begin
    Active := true;
    Options := [doNodeAutoCreate,doNodeAutoIndent,doAttrNull,doAutoPrefix,doNamespaceDecl];
    Version := '1.0';
    Encoding := 'UTF-8';
    rootNode:=AddChild(RootNodeName);
    s := CutString('=', RootNodeParams);
    rootNode.Attributes[s] := RootNodeParams;
  end;
  XmlDoc.FileName := FileName;
end;

function TDb2XmlRoot.GetFileName: string;
begin
  result := XmlDoc.FileName;
end;

procedure TDb2XmlRoot.Save;
begin
  XmlDoc.SaveToFile();
end;

{ TDb2XmlDataLayer }

constructor TDb2XmlDataLayer.Create(Rules: string; Dataset: TDataset);
begin
  inherited Create;
  Self.Dataset := Dataset;
  Self.Rules := rules;
end;

procedure TDb2XmlDataLayer.Execute(ParentNode: IXmlNode);
var Node: IXMLNode;
begin
  Dataset.First;
  while not (Dataset.Eof) do
  begin
    Node := ParentNode.AddChild('RECORD'); //// заглушка
    Dataset.Next;
  end;

end;

procedure TDb2XmlRoot.setFileName(const Value: string);
begin
  XmlDoc.FileName := value;
end;

end.

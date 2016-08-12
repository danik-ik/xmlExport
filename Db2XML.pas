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
    procedure OneRecordToXml(ParentNode: IXmlNode; Rules: String);
  public
    Constructor Create(Rules: string; Dataset: TDataset);
    procedure Execute(ParentNode: IXmlNode);
  end;

implementation

uses SysUtils, classes, DDD_Str;

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
begin
  Dataset.First;
  while not (Dataset.Eof) do
  begin
    OneRecordToXml(ParentNode, Self.Rules);
    Dataset.Next;
  end;

end;

procedure TDb2XmlRoot.setFileName(const Value: string);
begin
  XmlDoc.FileName := value;
end;

procedure TDb2XmlDataLayer.OneRecordToXml(ParentNode: IXmlNode;
  Rules: String);
var
  Node: IXMLNode;
  sl: TStringList;
  i: integer;
  RuleLine: string;
  NodeName, DataField: string;
  Level: integer;
  Levels: array of IXMLNode;
  CurrentLevel: integer;
begin
  try
    sl:=TStringList.Create;
    SetLength(Levels, 1);

    Levels[0] := ParentNode;
    // На нулевом уровне -- родитель!!!

    sl.Text := Rules;
    for i := 0 to sl.Count - 1 do
    begin
      RuleLine := sl.Strings[i];
      // Игнор комментариев и пустых строк
      if RuleLine[1] = ';' then break;
      if trim(RuleLine) = '' then break;

      Level := Length(RuleLine) - length(TrimLeft(RuleLine)) + 1;

      NodeName := CutString('=',RuleLine);
      DataField := RuleLine;

      if Level = Length(Levels) - 1 then
      begin
        // Остальсь на том же уровне вложенности

        Node := Levels[Level];

        // Изменилось ли имя?
        if NodeName <> Node.NodeName then
        begin
          Node := Node.ParentNode.AddChild(NodeName);
          Levels[Level] := Node;
        end;
      end
      else if Level > Length(Levels) - 1 then
      begin
        // Углубляемся
        Node := Levels[Level-1].AddChild(NodeName);
        SetLength(Levels, Level + 1);
        Levels[Level] := Node;
      end
      else if Level < Length(Levels) - 1 then
      begin
        // Всплываем
        Node := Levels[Level];
        SetLength(Levels, Level + 1);
        // Изменилось ли имя?
        if NodeName <> Node.NodeName then
        begin
          Node := Node.ParentNode.AddChild(NodeName);
          Levels[Level] := Node;
        end;
      end
    end;

  finally
    sl.Free;
  end;
end;

end.

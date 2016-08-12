unit Db2XML;

interface
uses
  XMLIntf, DB;

  type TDb2Xml = class
    RootNode: IXmlNode;
    Constructor Create(FileName, RootNodeName, RootNodeParams: string); override;
  end;

  type TDb2XmlDataLayer = class
  private
    Rules: string;
    Parent_node: IXmlNode;
    Dataset: TDataSet;
  public
    Constructor Create(Rules: string; Dataset: TDataset); override;
    procedure Execute(ParentNode: IXmlNode);
  end;

implementation

end.

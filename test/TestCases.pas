unit TestCases;

interface

uses
 TestFrameWork,
 db2XML,
 XMLDoc, XMLIntf, DB, AdoDb,
 TestExtensions;

type

  TTestDBSetup = class(TTestSetup)
  public
    Connection: TADOConnection;
    Query: TADOQuery;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TRootTestCase = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  private
    Convertor: TDb2Xml;
  published
    procedure CreationTest;
    procedure RootNodeTest;
    procedure SaveTest;
  end;

  TLayerTestCase = class(TTestCase)
  private
    Rules: string;
    Dataset: TADOQuery;
    RootNode: IXmlNode;
    XmlDoc: TXMLDocument;

  published

  end;


implementation

uses SysUtils, Classes;
var TestDB: TTestDBSetup;


{ TRootTestCase }

procedure TRootTestCase.SetUp;
begin
  inherited;
  Convertor := TDb2Xml.Create('out\Simple.xml','ROOT_NAME', 'ID=12345');
end;

procedure TRootTestCase.TearDown;
begin
  inherited;
  Convertor.Free;
end;

procedure TRootTestCase.CreationTest;
begin
  Check(true);
end;

procedure TRootTestCase.RootNodeTest;
var x: TDb2Xml;
begin
  x:= convertor;
  CheckEqualsString(x.RootNode.NodeName, 'ROOT_NAME');
  CheckEquals(x.RootNode.Attributes['ID'], 12345);
end;

procedure TRootTestCase.SaveTest;
var
  etalon, test: string;
  sl: TStringList;
begin
  Convertor.Save;
  sl := TStringList.Create;
  try
    sl.LoadFromFile('testData\Simple.xml');
    etalon := sl.Text;
    sl.LoadFromFile('Out\Simple.xml');
    test := sl.Text;
  finally
    sl.Free;
  end;

  CheckEqualsString(etalon, Test);

end;

{ TTestDBSetup }

procedure TTestDBSetup.SetUp;
begin
  inherited;
  Connection := TADOConnection.Create(self);
  Connection.ConnectionString := 'FILE NAME=.\TestData\csv.udl';
end;

procedure TTestDBSetup.TearDown;
begin
  inherited;

end;

initialization
  TestFramework.RegisterTest(TRootTestCase.Suite);
  RegisterTest(TTestDBSetup.Create(TLayerTestCase.Suite));

end.

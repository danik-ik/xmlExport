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
    Convertor: TDb2XmlRoot;
  published
    procedure CreationTest;
    procedure RootNodeTest;
    procedure SaveTest;
  end;

  TLayerTestCase = class(TTestCase)
  private
    Convertor: TDb2XmlRoot;
    Rules: string;
    Dataset: TADOQuery;
    RootNode: IXmlNode;
    XmlDoc: TXMLDocument;
    procedure SaveTest(TestFileName: string);
    procedure ProcessTable(Layer: TDb2XmlDataLayer; table, OutFile: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Проверяет только перебор записей и сохранение для каждой записи ноды <Record/>
    procedure NoLinesSimpleTest;
    procedure OneLinesSimpleTest;
    procedure TwoLinesSimpleTest;
  end;


implementation

uses SysUtils, Classes;
var TestDB: TTestDBSetup;


{ TRootTestCase }

procedure TRootTestCase.SetUp;
begin
  inherited;
  Convertor := TDb2XmlRoot.Create('out\Simple.xml','ROOT_NAME', 'ID=12345');
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
var x: TDb2XmlRoot;
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
  Connection := TADOConnection.Create(nil);
  Connection.ConnectionString := 'FILE NAME=.\TestData\csv.udl';
  Query :=TADOQuery.Create(nil);
  Query.Connection := Connection;
end;

procedure TTestDBSetup.TearDown;
begin
  Query.Free;
  Connection.Free;
  inherited;
end;

{ TLayerTestCase }
const
  SimpleRules = 'Record';
  FlatRules = 'Record'#13#10' No_data'#13#10' No_Data2='#13#10'  #param_a=a'#13#10'  NODE_B=b'#13#10' Node_C=c';
  DeepRules =
    'Record='#13#10' AA=a'#13#10' AAA='#13#10' BB=b'#13#10'  #c=c'#13#10' DD='#13#10'  DDD=d'#13#10'  ff=ff'#13#10'  A2=a';

procedure TLayerTestCase.NoLinesSimpleTest;
Var
  Layer: TDb2XmlDataLayer;
begin
  Layer := TDb2XmlDataLayer.Create(
    SimpleRules,
    TestDb.Query);
  try
    ProcessTable(Layer, 'TestData\TwoRec.csv', 'simple0.xml');
    SaveTest('simple0.xml');
  finally
    Layer.Free;
  end;
end;

procedure TLayerTestCase.OneLinesSimpleTest;
Var
  Layer: TDb2XmlDataLayer;
begin
  Layer := TDb2XmlDataLayer.Create(
    SimpleRules,
    TestDb.Query);
  try
    ProcessTable(Layer, 'TestData\TwoRec.csv', 'simple1.xml');
    SaveTest('simple1.xml');
  finally
    Layer.Free;
  end;
end;

procedure TLayerTestCase.TwoLinesSimpleTest;
Var
  Layer: TDb2XmlDataLayer;
begin
  Layer := TDb2XmlDataLayer.Create(
    SimpleRules,
    TestDb.Query);
  try
    ProcessTable(Layer, 'TestData\TwoRec.csv', 'simple2.xml');
    SaveTest('simple2.xml');
  finally
    Layer.Free;
  end;

end;

procedure TLayerTestCase.ProcessTable(Layer: TDb2XmlDataLayer; table, OutFile: string);
begin
  with TestDb.Query do
  begin
    Active := false;
    sql.Text := 'select * from ' + table;
    Active := true;
  end;
  Layer.Execute(Convertor.RootNode);
  Convertor.FileName := 'out\'+ OutFile;
  Convertor.Save;
end;

procedure TLayerTestCase.SetUp;
begin
  inherited;
  Convertor := TDb2XmlRoot.Create('out\Simple.xml','ROOT_NAME', 'ID=12345');
end;

procedure TLayerTestCase.TearDown;
begin
  inherited;
end;

procedure TLayerTestCase.SaveTest(TestFileName: string);
var
  sl: TStringList;
  Etalon, test: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile('testData\' + TestFileName);
    etalon := sl.Text;
    sl.LoadFromFile('Out\'+ TestFileName);
    test := sl.Text;
  finally
    sl.Free;
  end;

  CheckEqualsString(etalon, Test);
end;

initialization
  TestFramework.RegisterTest(TRootTestCase.Suite);

  TestDb := TTestDBSetup.Create(TLayerTestCase.Suite);
  RegisterTest(TestDb);

end.

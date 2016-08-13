unit TestCases;

interface

uses
 TestFrameWork,
 Db2XML,
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
    Dataset: TADOQuery;
    RootNode: IXmlNode;
    XmlDoc: TXMLDocument;
    procedure SaveTest(TestFileName: string);
    procedure ProcessTable(Layer: TDb2XmlDataLayer; table, OutFile: string);
    procedure LayerTest(TableIndex, RuleIndex: integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // ѕровер€ет только перебор записей и сохранение дл€ каждой записи ноды <Record/>
    procedure NoLinesTest;
    procedure OneLinesTest;
    procedure TwoLinesTest;

    // ѕровер€ет работу с пол€ми:
    // ѕол€ записываютс€ как тексты вложенных нод
    procedure NoAttrTest;
    // ѕол€ записываютс€ как параметры нод
    procedure OnlyAttrTest;
    // ѕол€ записываютс€ и как тексты вложенных нод и как параметры нод
    procedure AttrAndDeepTest;

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
  Rules: array[0..3] of string = (
      'RECORD'
    , 'RECORD'#13#10' No_data'#13#10' No_Data2='#13#10'  NODE_B=b'#13#10' Node_C=c'#13#10' Node_e=e'#13#10'  NODE_B=b'
    , 'RECORD='#13#10' #a=a'#13#10' #aaa=a'#13#10' #f=f'#13#10' #e=e'#13#10' #d=d'#13#10' #b=b'
    , 'RECORD'#13#10' No_data'#13#10' No_Data2='#13#10'  #param_a=a'#13#10'  NODE_B=b'#13#10' Node_C=c'#13#10' Node_e=e'#13#10'  #param_a=a'#13#10'  #param_b=b'#13#10'  NODE_B=b'
  );

  RuleNames: array[0..3] of string = (
      'simple'
    , 'NoAttr'
    , 'OnlyAttr'
    , 'Attr&Deep'
  );

  TableNames: array[0..2] of string = (
      'noRec'
    , 'OneRec'
    , 'TwoRec'
  );

procedure TLayerTestCase.LayerTest(TableIndex, RuleIndex: integer);
Var
  Layer: TDb2XmlDataLayer;
begin
  Layer := TDb2XmlDataLayer.Create(
    Rules[RuleIndex],
    TestDb.Query);
  try
    ProcessTable(
      Layer,
      'TestData\' + TableNames[TableIndex] + '.csv',
      RuleNames[RuleIndex] + IntToStr(TableIndex) +'.xml'
    );
    SaveTest(RuleNames[RuleIndex] + IntToStr(TableIndex) + '.xml');
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

procedure TLayerTestCase.NoLinesTest;
begin
  LayerTest(0, 0);
end;

procedure TLayerTestCase.OneLinesTest;
begin
  LayerTest(1, 0);
end;

procedure TLayerTestCase.TwoLinesTest;
begin
  LayerTest(2, 0);
end;

procedure TLayerTestCase.NoAttrTest;
begin
  LayerTest(2, 1);
end;

procedure TLayerTestCase.OnlyAttrTest;
begin
  LayerTest(2, 2);
end;

procedure TLayerTestCase.AttrAndDeepTest;
begin
  LayerTest(2, 3);
end;

initialization
  TestFramework.RegisterTest(TRootTestCase.Suite);

  TestDb := TTestDBSetup.Create(TLayerTestCase.Suite);
  RegisterTest(TestDb);

end.

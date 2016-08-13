program XmlExport;

{$APPTYPE CONSOLE}
  (*
  ��������� ������������� ��� ����������� ������� �� (��� ��������� ������
  � �������������) � ������ XML.
  �����������:
    ���� ������� ����� ���� �������� � ���� ���������� �������� ����
    ���� ������� ����� ���� �������� � ���� �������� ����
    ���� ����� ������������ ��� �����������

  ���������� � ����� ������: data.udl
  ������: query.sql
  ������� �����������: convert.rules

  ��������� ��������� ������:

  ��� ��������� �����: 1-� �������� ��������� ������
  ��� �������� ����: 2-� �������� ��������� ������
  ��������� �������� ����: 3-� �������� ��������� ������

  *)

uses
  SysUtils,
  ActiveX,
  Classes,
  Db2XML,
  DDD_Str,
  XMLDoc, XMLIntf, DB, AdoDb;

var
  OutFileName: string; // ��� ��������� xml-�����
  RootNodeName: string; // ��� �������� ����
  RootParams: string; // ��������� ������� ����

  QuerySQL: string; // SQL-������
  Rules: string; // ������� �����������

const
  CLI_FILENAME_N = 1;
  CLI_ROOT_NODE_N = 2;
  CLI_ROOTPARAMS_N = 3;

  CLI_PARAMCOUNT = 3;
  CLI_USAGE = '�ॡ����� ��ࠬ����: ��� ��室���� 䠩��, ��� ��୥���� 㧫� XML,'#13#10
            + '��ࠬ���� ��୥���� 㧫� XML.'#13#10
            + '�ਬ��: XmlExport Distr_dist_000_01_Customer_xxx.xml DISTR_CONN_ID ID=dist_000_01';

  CONNECTION_STRING = 'FILE NAME=.\data.udl';
  QUERY_FILE_NAME = '.\query.sql';
  RULES_FILE_NAME = '.\convert.rules';

var
  Connection: TADOConnection = nil;
  Query: TADOQuery = nil;

  XmlRoot: TDb2XmlRoot = nil;        // ������ ����������
  XmlLayer: TDb2XmlDataLayer = nil;  // ������ ������ ������

  procedure GetParameters;
  var
    TextReader: TStringList;
  begin

    If ParamCount <> CLI_PARAMCOUNT then
    begin
      Writeln(CLI_USAGE);
      raise Exception.Create('���ࠢ��쭮� ������⢮ ��ࠬ��஢');
    end;

    OutFileName := ParamStr(CLI_FILENAME_N);
    RootNodeName := ParamStr(CLI_ROOT_NODE_N);
    RootParams := ParamStr(CLI_ROOTPARAMS_N);

    try
      TextReader := TStringList.Create;

      try
        TextReader.LoadFromFile(QUERY_FILE_NAME);
      except
        raise Exception.Create('Error reading SQL file: ' + QUERY_FILE_NAME);
      end;
      QuerySQL := TextReader.Text;

      try
        TextReader.LoadFromFile(RULES_FILE_NAME);
      except
        raise Exception.Create('Error reading rules file: ' + RULES_FILE_NAME);
      end;
      Rules := TextReader.Text;

    finally
      TextReader.Free;
    end;

  end;

procedure OpenDataBase;
begin
  CoInitialize(nil);

  Connection := TADOConnection.Create(nil);
  Connection.ConnectionString := CONNECTION_STRING;

  Query :=TADOQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := QuerySQL;
  Query.Open;
end;

procedure CreateConvertor;
begin
  XmlRoot := TDb2XmlRoot.Create(OutFileName, RootNodeName, RootParams);
  XmlLayer := TDb2XmlDataLayer.Create(Rules, Query);
end;

procedure RunConvertor;
begin
  XmlLayer.Execute(XmlRoot.RootNode);
  XmlRoot.Save 
end;

procedure ClearInstances;
begin
  XmlLayer.Free;
  XmlRoot.Free;
  Query.Free;
  Connection.Free;
end;

begin
  // ���������� ��������� ����������: ������ � ErrOutput, ���������� 1
  try
    try
      GetParameters;
      OpenDataBase;
      CreateConvertor;
      RunConvertor;
    finally
      ClearInstances;
    end;
  except
    on e: Exception do
    begin

      WriteLn(ErrOutput, AnsiToOem(e.Message));
      if ExitCode = 0 then
        ExitCode := 1;
    end;
  end;

end.

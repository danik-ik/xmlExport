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

  ��� ��������� �����: /file
  ��� �������� ����: /root
  ��������� �������� ����: /rootparams

  *)

uses
  SysUtils,
  Classes,
  Db2XML,
  XMLDoc, XMLIntf, DB, AdoDb;

var
  OutFileName: string; // ��� ��������� xml-�����
  RootNodeName: string; // ��� �������� ����
  RootParans: string; // ��������� ������� ����

  QuerySQL: string; // SQL-������
  Rules: string; // ������� �����������

const
  CONNECTION_STRING = 'FILE NAME=.\data.udl';
  QUERY_FILE_NAME = '.\query.sql';
  RULES_FILE_NAME = '.\convert.rules';

var
  Connection: TADOConnection;
  Query: TADOQuery;

  procedure GetParameters;
  var
    TextReader: TStringList;
  begin
    try
      TextReader := TStringList.Create;
      ////
    finally
      TextReader.Free;
    end;

  end;

procedure OpenDataBase;
begin

end;

procedure CreateConvertor;
begin

end;

procedure RunConvertor;
begin

end;


begin
  // ���������� ��������� ����������: ������ � ���, ���������� 1
  try
    GetParameters;
    OpenDataBase;
    CreateConvertor;
    RunConvertor;
  except
    on e: Exception do
    begin
      WriteLn(ErrOutput, e.Message);
      if ExitCode = 0 then
        ExitCode := 1;
    end;
  end;
end.

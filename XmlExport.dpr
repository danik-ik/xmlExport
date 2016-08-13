program XmlExport;

{$APPTYPE CONSOLE}
  (*
  Программа предназначена для конвертации таблицы БД (без связанных таблих
  с детализациями) в формат XML.
  Особенности:
    Поля запроса могут быть записаны в виде текстового значения ноды
    Поля запроса могут быть записаны в виде атрибута ноды
    Ноды могут вкладываться для группировки

  Соединение с базой данных: data.udl
  Запрос: query.sql
  Правила конвертации: convert.rules

  Параметры командной строки:

  Имя выходного файла: 1-й параметр командной строки
  Имя корневой ноды: 2-й параметр сомандной строки
  Параметры корневой ноды: 3-й параметр командной строки

  *)

uses
  SysUtils,
  ActiveX,
  Classes,
  Db2XML,
  DDD_Str,
  XMLDoc, XMLIntf, DB, AdoDb;

var
  OutFileName: string; // Имя выходного xml-файла
  RootNodeName: string; // Имя корневой ноды
  RootParams: string; // Параметры коневой ноды

  QuerySQL: string; // SQL-запрос
  Rules: string; // Правила конвертации

const
  CLI_FILENAME_N = 1;
  CLI_ROOT_NODE_N = 2;
  CLI_ROOTPARAMS_N = 3;

  CLI_PARAMCOUNT = 3;
  CLI_USAGE = '’аҐЎговбп Ї а ¬Ґвал: Ё¬п ўле®¤­®Ј® д ©« , Ё¬п Є®а­Ґў®Ј® г§«  XML,'#13#10
            + 'Џ а ¬Ґвал Є®а­Ґў®Ј® г§«  XML.'#13#10
            + 'ЏаЁ¬Ґа: XmlExport Distr_dist_000_01_Customer_xxx.xml DISTR_CONN_ID ID=dist_000_01';

  CONNECTION_STRING = 'FILE NAME=.\data.udl';
  QUERY_FILE_NAME = '.\query.sql';
  RULES_FILE_NAME = '.\convert.rules';

var
  Connection: TADOConnection = nil;
  Query: TADOQuery = nil;

  XmlRoot: TDb2XmlRoot = nil;        // Объект конвертора
  XmlLayer: TDb2XmlDataLayer = nil;  // Объект уровня данных

  procedure GetParameters;
  var
    TextReader: TStringList;
  begin

    If ParamCount <> CLI_PARAMCOUNT then
    begin
      Writeln(CLI_USAGE);
      raise Exception.Create('ЌҐЇа ўЁ«м­®Ґ Є®«ЁзҐбвў® Ї а ¬Ґва®ў');
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
  // Глобальная обработка исключений: запись в ErrOutput, возвращаем 1
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

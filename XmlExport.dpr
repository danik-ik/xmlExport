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

  Имя выходного файла: /file
  Имя корневой ноды: /root
  Параметры корневой ноды: /rootparams

  *)

uses
  SysUtils,
  Classes,
  Db2XML,
  XMLDoc, XMLIntf, DB, AdoDb;

var
  OutFileName: string; // Имя выходного xml-файла
  RootNodeName: string; // Имя корневой ноды
  RootParans: string; // Параметры коневой ноды

  QuerySQL: string; // SQL-запрос
  Rules: string; // Правила конвертации

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
  // Глобальная обработка исключений: запись в лог, возвращаем 1
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

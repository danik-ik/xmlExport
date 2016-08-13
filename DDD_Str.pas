unit DDD_Str;

interface

function AnsiToOem(const S: string): string;
function OemToAnsi(const S: string): string;

function CutString(delimiter: string;
  var Source: string; UseTrim: boolean=true): string;
{ Функция делит строку на две подстроки: до первого вхождения
  разделителя Delimiter (эта часть является результатом функции)
  и после него (остаётся в Source после выполнения функции).
  Сам разделитель при этом удаляется.
  Если разделителя в строке нет, то возвращается вся строка
  целиком, а Source опустошается.

  Может применяться для разделения последовательности команд
  на отдельные команды в цикле вроде такого:
  while MyBatch <> '' do
  begin
    OneCommand := CutString(#13#10, MyBatch);
    ProceedCommand(OneCommand);
  end; }

function CutCount(delimiter: string;
  Source: string; UseTrim: boolean=true): integer;
{ Считает разделители }

function StringToArray(delimiter: string; Source: string;
  var Dest: array of string; UseTrim: boolean=true): integer;
{ Рубит строку в массив. К сожалению, не нашёл возможности
  устанавливать размер динамического массива из функции.
  возвращает потребное/занятое количество элементов массива }

function ExcludeComments(Source: string; OpenChars: string;
  CloseChars: string = #13#10): string;
{ Превращает текст с комментариями в текст без комментариев.
  Удаляется текст между открывающей и закрывающей
  последовательностями символов, включая сами эти символы.
  Исключение: если комментарий закрывается концом строки,
  то конец строки остаётся на месте. }

implementation
uses Windows, SysUtils;

//Перекодировка ANSI->OEM
function AnsiToOem(const S: string): string;
begin
  SetLength(Result,Length(S));
  if Length(Result) > 0 then Windows.AnsiToOem(PChar(S),PChar(Result));
end; //ConvertAnsiToOem

//Перекодировка OEM->ANSI
function OemToAnsi(const S: string): string;
begin
  SetLength(Result,Length(S));
  if Length(Result) > 0 then Windows.OemToAnsi(PChar(S),PChar(Result));
end; //ConvertOemToAnsi

function AsIs(const S: string): string;
begin
  Result := s;
end;

function CutString(delimiter: string; var Source: string;
  UseTrim: boolean=true): string;
var
  i: integer;
  Fn: function (const S: string): string;
  s: string;
begin
  if UseTrim then Fn := Trim else Fn := AsIs;
  i := pos(Delimiter,Source);
  if i=0 then
  begin
    s := Source;
    Source := '';
    Result := Fn(S);
  end
  else begin
    s := Source;
    Source := Fn(Copy(S,i+Length(delimiter),Length(S)));
    Result := Fn(Copy(S,1,i-1));
  end;
end;

function CutCount(delimiter: string;
  Source: string; UseTrim: boolean=true): integer;
var
  Fn: function (const S: string): string;
begin
  if UseTrim then Fn := Trim else Fn := AsIs;
  Result := 0;
  While Fn(Source) <> '' do
  begin
    CutString(delimiter, Source, UseTrim);
    inc(Result);
  end;
end;

function StringToArray(delimiter: string; Source: string;
  var Dest: array of string; UseTrim: boolean=true): integer;
var
  i: integer;
  Fn: function (const S: string): string;
begin
  if UseTrim then Fn := Trim else Fn := AsIs;
  i := 0;
  while Fn(Source) <> '' do
  begin
    if i <= High(Dest) then
      Dest[i] := CutString(delimiter,Source, UseTrim);
    inc(i)
  end;
  Result := i;
end;

function ExcludeComments(Source: string; OpenChars: string;
  CloseChars: string = #13#10): string;
begin
    // Разбираем запрос
    Result := CutString(OpenChars, Source, false);
    while Source <> '' do
    begin
      if (CloseChars = #13#10)and(AnsiPos(CloseChars,Source) > 0) then
        Result := Result + #13#10;
      CutString(CloseChars, Source, false);
      Result := Result + CutString(OpenChars, Source, false);
    end;
end;

end.
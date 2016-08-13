unit DDD_Str;

interface

function AnsiToOem(const S: string): string;
function OemToAnsi(const S: string): string;

function CutString(delimiter: string;
  var Source: string; UseTrim: boolean=true): string;
{ ������� ����� ������ �� ��� ���������: �� ������� ���������
  ����������� Delimiter (��� ����� �������� ����������� �������)
  � ����� ���� (������� � Source ����� ���������� �������).
  ��� ����������� ��� ���� ���������.
  ���� ����������� � ������ ���, �� ������������ ��� ������
  �������, � Source ������������.

  ����� ����������� ��� ���������� ������������������ ������
  �� ��������� ������� � ����� ����� ������:
  while MyBatch <> '' do
  begin
    OneCommand := CutString(#13#10, MyBatch);
    ProceedCommand(OneCommand);
  end; }

function CutCount(delimiter: string;
  Source: string; UseTrim: boolean=true): integer;
{ ������� ����������� }

function StringToArray(delimiter: string; Source: string;
  var Dest: array of string; UseTrim: boolean=true): integer;
{ ����� ������ � ������. � ���������, �� ����� �����������
  ������������� ������ ������������� ������� �� �������.
  ���������� ���������/������� ���������� ��������� ������� }

function ExcludeComments(Source: string; OpenChars: string;
  CloseChars: string = #13#10): string;
{ ���������� ����� � ������������� � ����� ��� ������������.
  ��������� ����� ����� ����������� � �����������
  �������������������� ��������, ������� ���� ��� �������.
  ����������: ���� ����������� ����������� ������ ������,
  �� ����� ������ ������� �� �����. }

implementation
uses Windows, SysUtils;

//������������� ANSI->OEM
function AnsiToOem(const S: string): string;
begin
  SetLength(Result,Length(S));
  if Length(Result) > 0 then Windows.AnsiToOem(PChar(S),PChar(Result));
end; //ConvertAnsiToOem

//������������� OEM->ANSI
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
    // ��������� ������
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
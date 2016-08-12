unit TestCases;

interface
uses

 TestFrameWork;

type

 TTestCaseFirst = class(TTestCase)
  public
    procedure ch;

 published

   procedure TestFirst;
   procedure TestSecond;
   procedure TestThird;

 end;


implementation

uses SysUtils;

procedure TTestCaseFirst.ch;
var i: extended;
begin
  i:=0;
  i := i / i;
  if i = 0 then ShowException(nil, nil);
end;

procedure TTestCaseFirst.TestFirst;
begin
  Check(1 + 1 = 2, 'Catastrophic arithmetic failure!');
end;
procedure TTestCaseFirst.TestSecond;

begin
 Check(1 + 1 = 3, 'Deliberate failure');
end;

procedure TTestCaseFirst.TestThird;
var
 i : Integer;
begin
  i := 0;
  CheckException( ch, EInvalidOp, 'Invalid floating point operation');
end;

initialization

 TestFramework.RegisterTest(TTestCaseFirst.Suite);

end.

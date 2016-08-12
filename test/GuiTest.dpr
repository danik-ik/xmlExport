program GuiTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  TestCases in 'TestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

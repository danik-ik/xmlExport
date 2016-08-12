program XmlExport;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  Db2XML in 'Db2XML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, db2xml, xmldom, XMLIntf, msxmldom, XMLDoc, DB, ADODB;

type
  TForm1 = class(TForm)
    XMLDocument1: TXMLDocument;
    ADOQuery1: TADOQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.

unit uformParserTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls;

type

  { TFormParserSet }

  TFormParserSet = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ValueListEditorParser: TValueListEditor;
    procedure ButtonOkClick(Sender: TObject);
  private

  public

  end;

var
  FormParserSet: TFormParserSet;

implementation

{$R *.lfm}

uses DefaultTranslator;

{ TFormParserSet }

procedure TFormParserSet.ButtonOkClick(Sender: TObject);
begin
  ValueListEditorParser.Values['LogEleTag']:=UpperCase(ValueListEditorParser.Values['LogEleTag']);
  ValueListEditorParser.Values['LogEleAlertAttr']:=UpperCase(ValueListEditorParser.Values['LogEleAlertAttr']);
end;

end.


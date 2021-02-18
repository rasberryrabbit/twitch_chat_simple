unit uformUserlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormUserList }

  TFormUserList = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MemoAlert: TMemo;
    MemoSkip: TMemo;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormUserList: TFormUserList;

implementation

{$R *.lfm}

uses DefaultTranslator;

{ TFormUserList }

procedure TFormUserList.FormShow(Sender: TObject);
begin

end;


end.


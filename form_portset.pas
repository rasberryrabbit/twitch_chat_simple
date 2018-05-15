unit form_portset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormPortSet }

  TFormPortSet = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditIntr: TEdit;
    EditRChat: TEdit;
    EditChat: TEdit;
    EditAlert: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    PortRawChat, PortChat, PortAlert: string;
    Interval: Integer;
  end;

var
  FormPortSet: TFormPortSet;

implementation

{$R *.lfm}

uses DefaultTranslator;

function CheckAssign(const s, defvalue:string):string;
var
  x:Integer;
begin
  x:=StrToIntDef(s,0);
  if x=0 then
    Result:=defvalue
    else
      Result:=s;
end;

{ TFormPortSet }

procedure TFormPortSet.ButtonOkClick(Sender: TObject);
begin
  PortRawChat:=CheckAssign(EditRChat.Text,'8100');
  PortChat:=CheckAssign(EditChat.Text,'8096');
  PortAlert:=CheckAssign(EditAlert.Text,'8098');
  Interval:=StrToIntDef(EditIntr.Text,500);
end;

procedure TFormPortSet.FormShow(Sender: TObject);
begin
  EditRChat.Text:=PortRawChat;
  EditChat.Text:=PortChat;
  EditAlert.Text:=PortAlert;
  EditIntr.Text:=IntToStr(Interval);
end;

end.


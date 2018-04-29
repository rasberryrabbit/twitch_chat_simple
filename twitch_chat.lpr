program twitch_chat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, twitch_chat_main, lnetvisual, uniqueinstance_package, uChatBuffer,
  uWebsockSimple, form_portset, uformParserTag;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormTwitchChat, FormTwitchChat);
  Application.Run;
end.


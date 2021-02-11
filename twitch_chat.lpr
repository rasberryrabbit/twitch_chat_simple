program twitch_chat;

{$mode objfpc}{$H+}
//{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, twitch_chat_main, lnetvisual, uniqueinstance_package, uChatBuffer,
  uWebsockSimple, form_portset, uformParserTag, uhashimpl, uformUserlist,
  uCEFApplication;

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then begin
    RequireDerivedFormResource:=True;
  Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TFormTwitchChat, FormTwitchChat);
    Application.Run;
  end;

  DestroyGlobalCEFApp;
end.


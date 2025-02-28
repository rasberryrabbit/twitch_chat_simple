program Twitch_chat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Windows,
  uCEFApplication,
  TwitchChat_main, uChecksumList, uniqueinstance_package
  { you can add units after this };

{$R *.res}

//{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  CreateGlobalCEFApp;

  RequireDerivedFormResource:=True;
  IsMultiThread:=True;
  Application.Scaled:=True;
  if GlobalCEFApp.StartMainProcess then begin
    Application.Initialize;
  Application.CreateForm(TFormTwitchChat, FormTwitchChat);
    Application.Run;
  end;

  DestroyGlobalCEFApp;
end.


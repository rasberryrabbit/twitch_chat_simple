unit TwitchChat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uCEFWindowParent, uCEFChromium, uCEFApplication, uCEFConstants,
  uCEFInterfaces, uCEFChromiumEvents, uCEFTypes, uCEFChromiumCore, LMessages,
  ExtCtrls, ActnList, Menus, XMLPropStorage, uCEFWinControl, UniqueInstance,
  RxVersInfo, uWebExtHandler;


const
  LM_Execute_script = LM_USER+$102;

type

  { TFormTwitchChat }

  TFormTwitchChat = class(TForm)
    ActionDisableMention: TAction;
    ActionWSLog: TAction;
    ActionChatuser: TAction;
    ActionWSockUnique: TAction;
    ActionOpenChatFull: TAction;
    ActionChatTime: TAction;
    ActionOpenNotify: TAction;
    ActionOpenChat: TAction;
    ActionWSPort: TAction;
    ActionList1: TActionList;
    ButtonHome: TButton;
    ButtonGo: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Editurl: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    RxVersionInfo1: TRxVersionInfo;
    Timer1: TTimer;
    Timer2: TTimer;
    UniqueInstance1: TUniqueInstance;
    XMLConfig1: TXMLConfig;
    XMLPropStorage1: TXMLPropStorage;
    procedure ActionChatTimeExecute(Sender: TObject);
    procedure ActionChatuserExecute(Sender: TObject);
    procedure ActionDisableMentionExecute(Sender: TObject);
    procedure ActionOpenChatExecute(Sender: TObject);
    procedure ActionOpenChatFullExecute(Sender: TObject);
    procedure ActionOpenNotifyExecute(Sender: TObject);
    procedure ActionWSLogExecute(Sender: TObject);
    procedure ActionWSockUniqueExecute(Sender: TObject);
    procedure ActionWSPortExecute(Sender: TObject);
    procedure ButtonHomeClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonGoClick(Sender: TObject);
    procedure Chromium1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    // CEF
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser
      );
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeDevToolsPopup(Sender: TObject;
      const browser: ICefBrowser; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue; var use_default_window: boolean);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure Chromium1ChromeCommand(Sender: TObject;
      const browser: ICefBrowser; command_id: integer;
      disposition: TCefWindowOpenDisposition; var aResult: boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction: TCefCloseBrowserAction);
    procedure Chromium1DocumentAvailableInMainFrame(Sender: Tobject;
      const browser: ICefBrowser);
    procedure Chromium1LoadingStateChange(Sender: TObject;
      const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
      Result: Boolean);
    procedure EditurlKeyPress(Sender: TObject; var Key: char);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvXPButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    procedure ExecuteScript(var Msg:TLMessage); message LM_Execute_script;

    // CEF
    procedure CEFCreated(var Msg:TLMessage); message CEF_AFTERCREATED;
    procedure CEFDestroy(var Msg:TLMessage); message CEF_DESTROY;
  protected
    // prevent crash on CEF close
    FClosing: Boolean;
    FCanClose: Boolean;
  public
    procedure SetFormCaption;

    procedure SetUpWebSocketPort;
    procedure ReplaceWSPortHTML(const fname, port1, port2: string);

  end;

var
  FormTwitchChat: TFormTwitchChat;

procedure CreateGlobalCEFApp;

implementation

uses
  uCEFMiscFunctions, uCEFProcessMessage, uCEFDomVisitor, uCEFStringMap,
  Windows, uWebsockSimple, ShellApi, DateUtils, StrUtils,
  regexpr;


{$R *.lfm}

const
  MaxLength = 2048;
  cqueryjs = 'var obser=document.querySelector("div.chat-scrollable-area__message-container");'+
             'var observer;'+
             'if(obser) {'+
             'if(observer) { observer.disconnect(); };'+
             'browserExt.postMessage("!Observer Start!");'+
             'var observer = new MutationObserver((mutations) => {'+
             'mutations.forEach(mutat => {'+
             'mutat.addedNodes.forEach(node => {'+
             'browserExt.postMessage(node.outerHTML);'+
             '});'+
             '});'+
             '});'+
             'observer.observe(obser, {'+
             '    subtree: false,'+
             '    attributes: false,'+
             '    childList: true,'+
             '    characterData: false,'+
             '    });'+
             'observer.start();'+
             'window.addEventListener(''unload'', function() {'+
             '  observer.disconnect();'+
             '});'+
             '}';

  syschat_str = 'user-notice-line';
  mention_str = 'mention-fragment';


var
  WSPortChat: string = '63102';
  WSPortSys: string = '63103';
  WSPortUnique: Boolean = False;
  SockServerChat: TSimpleWebsocketServer;
  SockServerSys: TSimpleWebsocketServer;
  IncludeChatTime: Boolean = False;
  chatlog_full: string = 'doc\webchatlog_list.html';
  chatlog_full_unique: string = 'doc\webchatlog_list_unique.html';
  chatlog_donation: string = 'doc\webchatlog_donation_sub.html';
  chatlog_chatonly: string = 'doc\webchatlog_chatbox.html';
  chatlog_userid: string = 'doc\webchatlog_user_unique.html';
  observer_started: Boolean = False;
  disable_mention: Boolean = True;


{ TFormTwitchChat }

procedure TFormTwitchChat.ButtonHomeClick(Sender: TObject);
begin
  Chromium1.LoadURL('https://www.twitch.tv');
end;

procedure TFormTwitchChat.ActionWSPortExecute(Sender: TObject);
var
  ir: Integer;
begin
  ir:=InputCombo('WebSocket port','Select WebSocket port',['63102','63110','63120','63130','63140']);
  case ir of
  1: WSPortChat:='63102';
  2: WSPortChat:='63110';
  3: WSPortChat:='63120';
  4: WSPortChat:='63130';
  5: WSPortChat:='63140';
  end;
  if ir<>-1 then
    begin
      try
        SetUpWebSocketPort;
        ReplaceWSPortHTML(chatlog_chatonly,WSPortChat,'');
        ReplaceWSPortHTML(chatlog_donation,WSPortSys,'');
        ReplaceWSPortHTML(chatlog_userid,WSPortChat,'');
        ReplaceWSPortHTML(chatlog_full_unique,WSPortChat,'');
        ReplaceWSPortHTML(chatlog_full,WSPortChat,WSPortSys);
        XMLConfig1.SetValue('WS/PORT',WSPortChat);
        XMLConfig1.SetValue('WS/PORTSYS',WSPortSys);
        SetFormCaption;
      except
        on e:exception do
          ShowMessage(e.Message);
      end;
    end;
end;

procedure TFormTwitchChat.ActionOpenChatExecute(Sender: TObject);
begin
  ShellExecuteW(0,'open',pwidechar(ExtractFilePath(Application.ExeName)+UTF8Decode(chatlog_chatonly)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFormTwitchChat.ActionOpenChatFullExecute(Sender: TObject);
begin
  if not WSPortUnique then
   ShellExecuteW(0,'open',pwidechar(ExtractFilePath(Application.ExeName)+UTF8Decode(chatlog_full)),nil,nil,SW_SHOWNORMAL)
   else
    ShellExecuteW(0,'open',pwidechar(ExtractFilePath(Application.ExeName)+UTF8Decode(chatlog_full_unique)),nil,nil,SW_SHOWNORMAL)
end;

procedure TFormTwitchChat.ActionChatTimeExecute(Sender: TObject);
begin
  ActionChatTime.Checked:=not ActionChatTime.Checked;
  IncludeChatTime:=ActionChatTime.Checked;
  XMLConfig1.SetValue('IncludeTime',IncludeChatTime);
end;

procedure TFormTwitchChat.ActionChatuserExecute(Sender: TObject);
begin
  ShellExecuteW(0,'open',pwidechar(ExtractFilePath(Application.ExeName)+UTF8Decode(chatlog_userid)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFormTwitchChat.ActionDisableMentionExecute(Sender: TObject);
begin
  ActionDisableMention.Checked:=not ActionDisableMention.Checked;
  disable_mention:=ActionDisableMention.Checked;
end;

procedure TFormTwitchChat.ActionOpenNotifyExecute(Sender: TObject);
begin
  ShellExecuteW(0,'open',pwidechar(ExtractFilePath(Application.ExeName)+UTF8Decode(chatlog_donation)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFormTwitchChat.ActionWSLogExecute(Sender: TObject);
begin
  ActionWSLog.Checked:=not ActionWSLog.Checked;
  Timer2.Enabled:=ActionWSLog.Checked;
end;

procedure TFormTwitchChat.ActionWSockUniqueExecute(Sender: TObject);
begin
  ActionWSockUnique.Checked:=not ActionWSockUnique.Checked;
  WSPortUnique:=ActionWSockUnique.Checked;
  XMLConfig1.SetValue('WS/UNIQUE',WSPortUnique);
end;

procedure TFormTwitchChat.ButtonRunClick(Sender: TObject);
begin

end;

procedure TFormTwitchChat.ButtonGoClick(Sender: TObject);
begin
  Chromium1.LoadURL(Editurl.Text);
end;

procedure TFormTwitchChat.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  Editurl.Text:=url;
  observer_started:=False;
end;

procedure TFormTwitchChat.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0,0);
end;

procedure TFormTwitchChat.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  // prevent crash on CEF close
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0,0);
end;

procedure TFormTwitchChat.Chromium1BeforeDevToolsPopup(Sender: TObject;
  const browser: ICefBrowser; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var use_default_window: boolean);
begin

end;

procedure TFormTwitchChat.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TFormTwitchChat.Chromium1ChromeCommand(Sender: TObject;
  const browser: ICefBrowser; command_id: integer;
  disposition: TCefWindowOpenDisposition; var aResult: boolean);
begin
    aResult:=True;
end;

procedure TFormTwitchChat.Chromium1Close(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TFormTwitchChat.Chromium1DocumentAvailableInMainFrame(
  Sender: Tobject; const browser: ICefBrowser);
begin

end;

procedure TFormTwitchChat.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
end;

function InsertTime(var s:ustring):Boolean;
var
  tp, sp: Integer;
begin
  tp:=Pos('<',s);
  sp:=Pos(' ',s);
  if (tp+sp>1) and (sp>tp) then
    begin
      Inc(sp);
      Insert('Time="'+IntToStr(DateTimeToUnix(Now))+'" ', s, sp);
    end;
end;

procedure TFormTwitchChat.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
  Result: Boolean);
var
  buf: ustring;
begin
  // browser message
  Result := False;
  if message=nil then
    exit;
  if message.Name='postMessage' then
    begin
      buf:=message.ArgumentList.GetString(0);
      if not observer_started then begin
        if buf='!Observer Start!' then
          observer_started:=True;
      end else
      begin
        if (Pos(UTF8Decode(syschat_str),buf)>0) then
         begin
           SockServerSys.BroadcastMsg(UTF8Encode(buf));
           if WSPortUnique then
             SockServerChat.BroadcastMsg(UTF8Encode(buf));
         end
         else
           if (not disable_mention) or (disable_mention and (Pos(mention_str,buf)=0)) then
             SockServerChat.BroadcastMsg(UTF8Encode(buf));

        //if IncludeChatTime then
        //  InsertTime(s);
      end;
      Result:=True;
    end;
end;

procedure TFormTwitchChat.EditurlKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    begin
      Key:=#0;
      ButtonGo.Click;
    end;
end;

procedure TFormTwitchChat.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin

end;

procedure TFormTwitchChat.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // prevent crash on CEF close
  CanClose := FCanClose;
  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
      CEFWindowParent1.Free;
    end;
end;

procedure TFormTwitchChat.FormCreate(Sender: TObject);
begin
  // prevent crash on CEF close
  FClosing:=False;
  FCanClose:=False;
end;

procedure TFormTwitchChat.FormDestroy(Sender: TObject);
begin
  SockServerChat.Free;
  SockServerSys.Free;
  XMLConfig1.SetValue('CHAT/FULL',UTF8Decode(chatlog_full));
  XMLConfig1.SetValue('CHAT/FULLUNIQUE',UTF8Decode(chatlog_full_unique));
  XMLConfig1.SetValue('CHAT/CHAT',UTF8Decode(chatlog_chatonly));
  XMLConfig1.SetValue('CHAT/DONATION',UTF8Decode(chatlog_donation));
  XMLConfig1.SetValue('CHAT/USERID',UTF8Decode(chatlog_userid));
  XMLConfig1.SetValue('CHAT/DMENTION',disable_mention);
  if XMLConfig1.Modified then
    XMLConfig1.SaveToFile('config.xml');
end;

procedure TFormTwitchChat.FormShow(Sender: TObject);
begin
  if FileExists('config.xml') then
    XMLConfig1.LoadFromFile('config.xml');

  IncludeChatTime:=XMLConfig1.GetValue('IncludeTime',False);
  WSPortChat:=XMLConfig1.GetValue('WS/PORT','63102');
  WSPortSys:=XMLConfig1.GetValue('WS/PORTSYS','63103');
  WSPortUnique:=XMLConfig1.GetValue('WS/UNIQUE',WSPortUnique);
  disable_mention:=XMLConfig1.GetValue('CHAT/DMENTION',disable_mention);
  ActionChatTime.Checked:=IncludeChatTime;
  ActionWSockUnique.Checked:=WSPortUnique;
  ActionDisableMention.Checked:=disable_mention;

  chatlog_full:=UTF8Encode(XMLConfig1.GetValue('CHAT/FULL',UTF8Decode(chatlog_full)));
  chatlog_full_unique:=UTF8Encode(XMLConfig1.GetValue('CHAT/FULLUNIQUE',UTF8Decode(chatlog_full_unique)));
  chatlog_chatonly:=UTF8Encode(XMLConfig1.GetValue('CHAT/CHAT',UTF8Decode(chatlog_chatonly)));
  chatlog_donation:=UTF8Encode(XMLConfig1.GetValue('CHAT/DONATION',UTF8Decode(chatlog_donation)));
  chatlog_userid:=UTF8Encode(XMLConfig1.GetValue('CHAT/USERID',UTF8Decode(chatlog_userid)));

  SetUpWebSocketPort;
  SetFormCaption;

  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TFormTwitchChat.JvXPButton1Click(Sender: TObject);
begin
end;

procedure TFormTwitchChat.Timer1Timer(Sender: TObject);
begin
  // prepare chromium
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TFormTwitchChat.Timer2Timer(Sender: TObject);
begin
  PostMessage(Handle, LM_Execute_script, 0, 0);
end;

function check_url(const url:ustring):boolean;
const
  twitch_base = 'www.twitch.tv/';
var
  i: Integer;
  s: ustring;
begin
  Result:=False;
  i:=Pos(twitch_base,url);
  if i>0 then begin
    s:=Copy(url,i+Length(twitch_base));
    Result:=(s<>'') and (s<>'directory');
  end;
end;

procedure TFormTwitchChat.ExecuteScript(var Msg: TLMessage);
begin
  // Send Message to Renderer for parsing
  if (not observer_started) and check_url(Chromium1.DocumentURL) then
    Chromium1.ExecuteJavaScript(cqueryjs,'');
  Msg.Result:=-1;
end;

procedure TFormTwitchChat.CEFCreated(var Msg: TLMessage);
begin
  CEFWindowParent1.UpdateSize;
  // loading twitch live
  ButtonGo.Click;
  Msg.Result:=-1;
end;

procedure TFormTwitchChat.CEFDestroy(var Msg: TLMessage);
begin
  //CEFWindowParent1.Free;
  if XMLConfig1.Modified then
    XMLConfig1.SaveToFile('config.xml');
  Msg.Result:=-1;
end;

procedure TFormTwitchChat.SetFormCaption;
var
  cefVer: Cardinal;
begin
  cefVer:=GetFileVersion('libcef');
  Caption:='TwitchChat '+RxVersionInfo1.FileVersion+' '+IntToHex(cefVer,8)+' @'+WSPortChat;
end;

procedure TFormTwitchChat.SetUpWebSocketPort;
var
  i, j: Integer;
begin
  // start websocket server
  if Assigned(SockServerChat) then
    SockServerChat.Free;
  if Assigned(SockServerSys) then
    SockServerSys.Free;
  j:=0;
  if not TryStrToInt(WSPortChat,i) then
  begin
    WSPortChat:='63102';
    i:=63102;
  end;
  while j<8 do begin
    try
      SockServerChat:=TSimpleWebsocketServer.Create(WSPortChat);
      try
        WSPortSys:=IntToStr(i+1);
        SockServerSys:=TSimpleWebsocketServer.Create(WSPortSys);
        break;
      except
        SockServerChat.Free;
        raise;
      end;
    except
      Inc(j,2);
      Inc(i,j);
      WSPortChat:=IntToStr(i);
    end;
  end;
end;

procedure TFormTwitchChat.ReplaceWSPortHTML(const fname, port1, port2: string);
const
  rport = '(?-s)(WebSocket\(\"ws\:.+)(\:\d+)(\",\"chat\"\);)';
var
  fs: TStringStream;
  regport: TRegExpr;
  res: string;
  i, j: Integer;
begin
  res:='';
  i:=1;
  fs := TStringStream.Create('');
  try
    fs.LoadFromFile(fname);
    regport:=TRegExpr.Create(rport);
    try
      // first item
      if (port1<>'') and regport.Exec(fs.DataString) then
      begin
        j:=regport.MatchPos[2];
        res:=res+Copy(fs.DataString,i,j-i+1)+port1;
        i:=regport.MatchPos[3];
      end;
      // second item
      if (port2<>'') and regport.ExecNext then
      begin
        j:=regport.MatchPos[2];
        res:=res+Copy(fs.DataString,i,j-i+1)+port2;
        i:=regport.MatchPos[3];
      end;
      res:=res+Copy(fs.DataString,i);
      // save to file
      fs.Clear;
      fs.WriteString(res);
      fs.SaveToFile(fname);
    finally
      regport.Free;
    end;
  finally
    fs.Free;
  end;
end;

procedure GlobalCEFApp_OnWebKitInitialized;
var
  TempExtensionCode : string;
  TempHandler       : ICefv8Handler;
begin

  TempExtensionCode := 'var '+ExtensionName+';' +
                       'if (!'+ExtensionName+')' +
                       '  '+ExtensionName+' = {};' +
                       '(function() {' +
                       '  '+ExtensionName+'.'+MessagePostName+' = function(b) {' +
                       '    native function '+MessagePostName+'();' +
                       '    '+MessagePostName+'(b);' +
                       '  };' +
                       '})();';

  try
    TempHandler := TWebExtensionHandler.Create;

    if CefRegisterExtension(ExtensionName, TempExtensionCode, TempHandler) then
      {CEFDebugLog('JavaScript extension registered successfully!')}
     else
      {CefDebugLog('There was an error registering the JavaScript extension!')};
  finally
    TempHandler := nil;
  end;
end;

// initialize CEF

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplication.Create;
  GlobalCEFApp.OnWebKitInitialized := @GlobalCEFApp_OnWebKitInitialized;
  GlobalCEFApp.cache               := 'cache';
  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_ERROR;
  GlobalCEFApp.EnablePrintPreview  := False;
  GlobalCEFApp.EnableGPU           := True;
  GlobalCEFApp.SetCurrentDir       := True;
  GlobalCEFApp.CheckCEFFiles       := False;
  //GlobalCEFApp.SingleProcess       := True;
end;

initialization

finalization


end.


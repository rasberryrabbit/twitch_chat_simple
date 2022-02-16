unit twitch_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, uCEFApplication, uCEFWindowParent, uCEFChromiumWindow,
  uCEFTypes, uCEFLibFunctions, uCEFInterfaces, uCEFRenderProcessHandler,
  uCEFChromium, uCEFDomVisitor, uCEFChromiumCore, uCEFProcessMessage,
  lNetComponents, lhttp, lNet, UniqueInstance, loglistfpc, syncobjs,
  Messages, uCEFChromiumEvents, uCEFConstants;

const
  WM_CEFMsg = WM_USER+100;

type

  { TFormTwitchChat }

  TFormTwitchChat = class(TForm)
    ActionUserList: TAction;
    ActionActiveStart: TAction;
    ActionParserSet: TAction;
    ActionPortSet: TAction;
    ActionList1: TActionList;
    ButtonAct: TButton;
    Button2: TButton;
    CEFWindowParent1: TCEFWindowParent;
    CheckBoxShowReal: TCheckBox;
    CheckBoxImgLoading: TCheckBox;
    CheckBoxAutoUrl: TCheckBox;
    CheckBoxRemSyS: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    CheckBoxClearB: TCheckBox;
    Chromium1: TChromium;
    EditCEFUrl: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel2: TPanel;
    Timer1: TTimer;
    TimerChrome: TTimer;
    TimerNav: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ActionActiveStartExecute(Sender: TObject);
    procedure ActionParserSetExecute(Sender: TObject);
    procedure ActionPortSetExecute(Sender: TObject);
    procedure ActionUserListExecute(Sender: TObject);
    procedure ButtonActClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxImgLoadingClick(Sender: TObject);
    procedure Chromium1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser
      );
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction: TCefCloseBrowserAction);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure Chromium1LoadStart(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
      Result: Boolean);
    procedure EditCEFUrlKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerChromeTimer(Sender: TObject);
    procedure TimerNavTimer(Sender: TObject);
  private
    FEventMain:TEvent;
    // chrome
    procedure BrowserCreateMsg(var aMsg:TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMsg:TMessage); message CEF_DESTROY;
    procedure WMMove(var aMsg:TMessage); message WM_MOVE;
    procedure WMMoving(var aMsg:TMessage); message WM_MOVING;
    procedure WMCEFMsg(var aMsg:TMessage); message WM_CEFMsg;
  public
    log:TLogListFPC;
    // chrome
    FCanClose:Boolean;
    FClosing:Boolean;

    function TryEnter:Boolean;
    procedure Leave;

  end;

  procedure CreateGlobalCEFApp;
  function CheckParam(const str:string):Boolean;

var
  FormTwitchChat: TFormTwitchChat;

implementation

{$R *.lfm}

uses
  Windows, uChatBuffer, uRequestHandler, uWebsockSimple, form_portset, IniFiles,
  uformParserTag, uhashimpl, Hash, DefaultTranslator, Contnrs, uformUserlist,
  uCEFMiscFunctions;

const
  MaxChecksum = 11; // odd length
  rootUrl = 'https://www.twitch.tv';

var
  MainBrowser : ICefBrowser;

  lastchecksum : array[0..MaxChecksum] of THashDigest;
  lastchkCount : Integer;
  lastDupChk : array[0..MaxChecksum] of Integer;

  //ChatBuffer:TCefChatBuffer;

  WebSockChat:TSimpleWebsocketServer;
  WebSockAlert:TSimpleWebsocketServer;
  WebSockRawChat:TSimpleWebsocketServer;


  PortChat:string  = '8096';
  PortAlert:string = '8098';
  PortRawChat:string  = '8100';
  CInterval:Integer = 500;

  // html parsing
  LogEleTag : UnicodeString = 'DIV';
  LogEleAttr : UnicodeString = 'role';
  LogEleName : UnicodeString = 'log';

  LogEleAlertAttr : UnicodeString = 'CLASS';
  LogEleAlert : UnicodeString = 'user-notice-line';
  LogEleSys : UnicodeString = 'chat-line__status';

  LogEleChatAttr : UnicodeString = 'class';
  LogEleChatName : UnicodeString = 'chat-line__message';
  LogEleChatEmote : UnicodeString = 'chat-line__message--emote-button';
  LogEleChatSkipDefault : UnicodeString = 'scrollable-trigger__wrapper';
  LogEleChatFrag : UnicodeString = 'text-fragment';

  LogEleUser : UnicodeString = 'chat-line__username';
  LogEleUserCon : UnicodeString = 'chat-line__username-container';
  LogEleUserName : UnicodeString = 'chat-author__display-name';
  LogEleUserAttr : UnicodeString = 'data-a-user';
  LogEleCon : UnicodeString = '-container';
  LogEleHigh : UnicodeString = '-highlight';
  LogEleEmote : UnicodeString = 'emote';
  LogEleNotice : UnicodeString = '-notice';

  LogAddHead : string = '<li class="twitch_chat">';
  LogAddTail : string = '</li>';

  skipchecksum : THashDigest;

  UseTimeInRawChat : Boolean = True;

type

  { TElementIdVisitor }

  TElementIdVisitor = class(TCefDomVisitorOwn)
  private
    FNameID: string;
    FEvent:TEvent;
  protected
    procedure Visit(const document: ICefDomDocument); override;
  public
    constructor Create(const AId: string); reintroduce;
    destructor Destroy; override;

    function TryEnter:Boolean;
    procedure Leave;
  end;

  { TFPStringHashTableList }

  TFPStringHashTableList = class(TFPStringHashTable)
    private
      fText:string;
      function GetText: string;
      procedure SetText(const Value:string);
      procedure _IterStr(Item: String; const Key: string; var Continue: Boolean);
    protected
    public
      property Text:string read GetText write SetText;
  end;

var
  UserAlertID : TFPStringHashTableList;
  UserSkipID : TFPStringHashTableList;
  LogEleChatSkip : TFPStringHashTableList;


procedure ProcessElementsById(const AFrame: ICefFrame; const AId: string);
var
  Visitor: TElementIdVisitor;
  surl:string;
  retv8, v8:ICefV8Value;
  errv8:ICefV8Exception;
begin
  if Assigned(AFrame) then
  begin
    if (0<>Pos('.twitch.tv/',AFrame.GetUrl)) then begin
      Visitor := TElementIdVisitor.Create(AId);
      AFrame.VisitDom(Visitor);
    end;
  end;
end;


procedure OnCEFProcessMsg(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; var aHandled : boolean);
var
  chatframe:ICefFrame;
  fcount, i:NativeUInt;
  fid:array of int64;
  surl:string;
begin
  aHandled:=False;
  if Message.Name='visitdom' then begin
    { thread-safe? }
    if FormTwitchChat.TryEnter then begin
      try
        //fcount:=browser.GetFrameCount;
        //SetLength(fid,fcount);
        //try
        //  browser.GetFrameIdentifiers(@fcount,@fid[0]);
        //  for i:=0 to fcount-1 do begin
        //    chatframe:=browser.GetFrameByident(fid[i]);
        //    ProcessElementsById(chatframe,'log');
        //  end;
        //finally
        //  SetLength(fid,0);
        //end;
        ProcessElementsById(browser.MainFrame,LogEleName);
        aHandled:=True;
      finally
        FormTwitchChat.Leave;
      end;
    end;
  end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                  := TCefApplication.Create;
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_ERROR;
  GlobalCEFApp.SingleProcess:=True;
  GlobalCEFApp.OnProcessMessageReceived:=@OnCEFProcessMsg;
  GlobalCEFApp.CheckCEFFiles:=False;
  if CheckParam('IGNORECERT') then
    GlobalCEFApp.IgnoreCertificateErrors:=True;
end;

{ TFPStringHashTableList }

function TFPStringHashTableList.GetText: string;
var
  temp: THTStringNode;
begin
  fText:='';
  ForEachCall(@_IterStr);
  Result:=fText;
end;

procedure TFPStringHashTableList.SetText(const Value: string);
var
  temp : TStringList;
  i : Integer;
begin
  Clear;
  temp:=TStringList.Create;
  try
    temp.Delimiter:=',';
    temp.DelimitedText:=Value;
    for i:=0 to temp.Count-1 do
      try
        Add(temp.Strings[i],'1');
      except
      end;
  finally
    temp.Free;
  end;
end;

procedure TFPStringHashTableList._IterStr(Item: String; const Key: string;
  var Continue: Boolean);
begin
  if fText<>'' then
    fText:=fText+',';
  fText:=fText+Key;
end;

{ TElementNameVisitor }

constructor TElementIdVisitor.Create(const AId: string);
begin
  inherited Create;
  FNameID := AId;
  FEvent:=TEvent.Create(nil,True,True,'CEFELVI'+IntToStr(GetTickCount64));
end;

destructor TElementIdVisitor.Destroy;
begin
  FEvent.SetEvent;
  FEvent.Free;
  inherited Destroy;
end;

function TElementIdVisitor.TryEnter: Boolean;
begin
  Result:=FEvent.WaitFor(0)<>wrTimeout;
  if Result then
    FEvent.ResetEvent;
end;

procedure TElementIdVisitor.Leave;
begin
  FEvent.SetEvent;
end;

function IsContainUniStringSemi(const s:UnicodeString):Boolean;
begin
  Result:=LogEleChatSkip.Find(pchar(UTF8Encode(s)))<>nil;
end;

function GetEmoteName(const vNode:ICefDomNode; MaxLen:Integer):UnicodeString;
var
  NodeL, NodeM: ICefDomNode;
begin
  Result:='';
  NodeL:=vNode.FirstChild;
  while Assigned(NodeL) do begin
    if Pos(LogEleEmote,NodeL.GetElementAttribute(LogEleChatAttr))>0 then begin
      NodeM:=NodeL;
      while Assigned(NodeM) do begin
        if Length(Result)<MaxLen then
        if NodeM.ElementTagName='IMG' then
          Result:=Result+' '+NodeM.GetElementAttribute('ALT');
        NodeM:=NodeM.FirstChild;
      end;
    end else
      if Length(Result)<MaxLen then
        Result:=Result+' '+Copy(NodeL.ElementInnerText,1,MaxLen)
        else
          Result:=Result+'..';
    NodeL:=NodeL.NextSibling;
  end;
end;

procedure TElementIdVisitor.Visit(const document: ICefDomDocument);
const
  checksumlen=128;
  loglinelen=1024;
var
  NodeH : ICefDomNode;
  stemp : string;
  procedure ProcessNode(ANode: ICefDomNode);
  var
    Node, Nodex, NodeN, NodeIcon, NodeChat, NodeStart, NodeEnd: ICefDomNode;
    s, sclass, sbuf, scheck : UnicodeString;
    checksumN : THashDigest;
    bottomchecksum : array[0..MaxChecksum] of THashDigest;
    dupCount, dupCountChk : array[0..MaxChecksum] of Integer;
    chkCount, i, j, ItemCount, vcode : Integer;
    matched, skipAddMarkup, disLog, RemoveSys, doAddMsg, IsAlert, ShowReal, containchat, skipcheck : Boolean;
    ssockout, stemp: string;
  begin
    if Assigned(ANode) then
    begin
      RemoveSys:=FormTwitchChat.CheckBoxRemSyS.Checked;
      disLog:=FormTwitchChat.CheckBoxDisableLog.Checked;
      ShowReal:=FormTwitchChat.CheckBoxShowReal.Checked;
      Node := ANode.FirstChild;
      while Assigned(Node) do begin
        if (Node.GetElementAttribute(LogEleAttr)=LogEleName) and (Node.GetElementTagName=LogEleTag) then begin
          ItemCount:=0;
          Nodex:=Node.LastChild;
          NodeEnd:=Nodex;
          chkCount:=0;
          while Assigned(Nodex) do begin
            // check MaxChecksum+1 bottom lines
            NodeN:=Nodex;
            matched:=lastchkCount>0;
            i:=0;
            dupCountChk:=lastDupChk;
            while Assigned(NodeN) do begin
              // check chatline only
              //if NodeN.GetElementAttribute(LogEleChatAttr)=LogEleChatName then begin
              if not IsContainUniStringSemi(NodeN.GetElementAttribute(LogEleChatAttr)) then begin
                scheck:='';
                skipcheck:=False;
                NodeIcon:=NodeN;
                // find chat container
                containchat:=False;
                vcode:=0;
                while Assigned(NodeIcon) do begin
                  sclass:=NodeIcon.GetElementAttribute(LogEleChatAttr);
                  if Pos(LogEleUserCon,sclass)>0 then begin
                    containchat:=True;
                    break;
                  end else
                  if Pos(LogEleHigh,sclass)>0 then
                    vcode:=1
                  else
                  if Pos(LogEleCon,sclass)>0 then
                    vcode:=0
                  else
                  // notice chat
                  if Pos(LogEleAlert,sclass)>0 then begin
                    NodeIcon:=NodeIcon.FirstChild;
                    while Assigned(NodeIcon) do begin
                      NodeChat:=NodeIcon;
                      while Assigned(NodeChat) do begin
                        sclass:=NodeChat.GetElementAttribute(LogEleChatAttr);
                        if Pos(LogEleChatName,sclass)>0 then begin
                          vcode:=3;
                          break;
                        end;
                        NodeChat:=NodeChat.FirstChild;
                      end;
                      if vcode=3 then
                        break;
                      NodeIcon:=NodeIcon.NextSibling;
                    end;
                    if vcode=3 then
                      vcode:=0;
                  end;
                  if Assigned(NodeIcon) then begin
                    if vcode=0 then
                      NodeIcon:=NodeIcon.FirstChild
                      else
                        NodeIcon:=NodeIcon.NextSibling;
                  end else
                    break;
                end;
                if containchat and Assigned(NodeIcon) then begin
                  // id : first, text : after
                  // class text-fragment
                  NodeChat:=NodeIcon;
                  while Assigned(NodeChat) do begin
                    if NodeChat.HasElementAttribute(LogEleChatAttr) then begin
                      sclass:=NodeChat.GetElementAttribute(LogEleChatAttr);
                      if Pos(LogEleNotice,sclass)>0 then begin
                        containchat:=False;
                        break;
                        //FormTwitchChat.log.AddLog('>>'+sclass);
                      end
                      else
                      // emote checksum
                      if NodeChat.HasChildren then
                        scheck:=scheck+GetEmoteName(NodeChat,checksumlen)
                      else if Length(scheck)<checksumlen then
                             scheck:=scheck+' '+Copy(NodeChat.ElementInnerText,1,checksumlen)
                             else
                               scheck:=scheck+'..';
                    end else
                      if Length(scheck)<checksumlen then
                        scheck:=scheck+' '+Copy(NodeChat.ElementInnerText,1,checksumlen)
                        else
                          scheck:=scheck+'..';

                    NodeChat:=NodeChat.NextSibling;
                  end;
                  //FormTwitchChat.log.AddLog('>>'+scheck);
                  if not containchat then
                    skipcheck:=True;
                end else begin
                  // non chat
                  if Assigned(NodeN) then
                    if Length(scheck)<checksumlen then
                    scheck:=scheck+Copy(NodeN.ElementInnerText,1,checksumlen);
                  //FormTwitchChat.log.AddLog(scheck);
                end;

                if skipcheck then
                  checksumN:=skipchecksum
                  else
                    checksumN:=MakeHash(@scheck[1],Length(scheck)*SizeOf(WideChar));

                if matched then begin
                  if i<lastchkCount then begin
                    if skipcheck or
                       CompareHash(checksumN,lastchecksum[i]) then begin
                      Dec(dupCountChk[i]);
                      if dupCountChk[i]=0 then
                        Inc(i);
                    end else
                      matched:=False;
                  end;
                end;

                // fill bottom checksum
                if chkCount<MaxChecksum then begin
                  // check duplication on first checksum
                  if (chkCount>0) and
                     CompareHash(checksumN,bottomchecksum[chkCount-1]) then
                    Inc(dupCount[chkCount-1])
                  else begin
                    bottomchecksum[chkCount]:=checksumN;
                    dupCount[chkCount]:=1;
                    Inc(chkCount);
                  end;
                end else
                  if (i>=lastchkCount) or (not matched) then
                    break;
              end;
              NodeN:=NodeN.PreviousSibling;
            end;
            if matched then
              break;

            NodeStart:=Nodex;
            Nodex:=Nodex.PreviousSibling;

            Inc(ItemCount);
            if ItemCount>=500 {ChatBuffer.MaxLines} then
              break;
          end;

          // add chat messages
          ssockout:='';
          Nodex:=NodeStart;
          while Nodex<>nil do begin
            // check skip element
            //if Nodex.GetElementAttribute(LogEleChatAttr)<>LogEleChatSkip then begin
            if not IsContainUniStringSemi(Nodex.GetElementAttribute(LogEleChatAttr)) then begin
              doAddMsg:=True;
              // system
              sclass:=Nodex.GetElementAttribute(LogEleAlertAttr);
              IsAlert:=False;
              // syslog, alert
              if Pos(LogEleAlert,sclass)<>0 then
                IsAlert:=True;
              if RemoveSys then
                doAddMsg:=Pos(LogEleSys,sclass)=0;

              // find chat
              NodeIcon:=Nodex;
              containchat:=False;
              vcode:=0;
              while Assigned(NodeIcon) do begin
                sclass:=NodeIcon.GetElementAttribute(LogEleChatAttr);
                if Pos(LogEleUserCon,sclass)>0 then begin
                  containchat:=True;
                  break;
                end else
                if Pos(LogEleHigh,sclass)>0 then
                  vcode:=1
                else
                if Pos(LogEleCon,sclass)>0 then
                  vcode:=0
                else
                // notice chat
                if Pos(LogEleAlert,sclass)>0 then begin
                  NodeIcon:=NodeIcon.FirstChild;
                  while Assigned(NodeIcon) do begin
                    NodeChat:=NodeIcon;
                    while Assigned(NodeChat) do begin
                      sclass:=NodeChat.GetElementAttribute(LogEleChatAttr);
                      if Pos(LogEleChatName,sclass)>0 then begin
                        vcode:=3;
                        break;
                      end;
                      NodeChat:=NodeChat.FirstChild;
                    end;
                    if vcode=3 then
                      break;
                    NodeIcon:=NodeIcon.NextSibling;
                  end;
                  if vcode=3 then
                    vcode:=0;
                end;
                if Assigned(NodeIcon) then begin
                  if vcode=0 then
                    NodeIcon:=NodeIcon.FirstChild
                    else
                      NodeIcon:=NodeIcon.NextSibling;
                end else
                  break;
              end;
              NodeChat:=nil;
              if not containchat then
                NodeIcon:=Nodex.FirstChild
                else
                  NodeChat:=NodeIcon.NextSibling;

              scheck:=Nodex.AsMarkup;
              skipAddMarkup:=True;
              // get chat message
              if not disLog then
                sbuf:=NodeIcon.ElementInnerText;

              if containchat then begin
                // check user alert and user skip
                if (UserSkipID.Count>0) or (UserAlertID.Count>0) then begin
                  NodeIcon:=NodeIcon.FirstChild;
                  while Assigned(NodeIcon) do begin
                    sclass:=NodeIcon.GetElementAttribute(LogEleAlertAttr);
                    if Pos(LogEleUser,sclass)>0 then begin
                      NodeN:=NodeIcon;
                      while Assigned(NodeN) do begin
                        sclass:=NodeN.GetElementAttribute(LogEleAlertAttr);
                        if Pos(LogEleUserName,sclass)>0 then begin
                          sclass:=NodeN.GetElementAttribute(LogEleUserAttr);
                          // user alert
                          if (not IsAlert) and
                             (UserAlertID.Count>0) and
                             (UserAlertID.Items[sclass]='1') then
                            IsAlert:=True;
                          // user skip
                          if (UserSkipID.Count>0) and
                             (UserSkipID.Items[sclass]='1') then
                            doAddMsg:=False;
                          if ShowReal then
                            sbuf:=sbuf+'['+sclass+']';
                        end;
                        NodeN:=NodeN.FirstChild;
                      end;
                    end;
                    NodeIcon:=NodeIcon.NextSibling;
                  end;
                end;
              end;
              // add chat message
              if not disLog then begin
                while Assigned(NodeChat) do begin
                  if NodeChat.HasElementAttribute(LogEleChatAttr) then begin
                    if NodeChat.HasChildren then
                      sbuf:=sbuf+GetEmoteName(NodeChat,loglinelen)
                    else
                      if Length(sbuf)<loglinelen then
                        sbuf:=sbuf+NodeChat.ElementInnerText
                        else
                          sbuf:=sbuf+'..';
                  end else
                    if Length(sbuf)<loglinelen then
                      sbuf:=sbuf+NodeChat.ElementInnerText
                      else
                        sbuf:=sbuf+'..';
                  NodeChat:=NodeChat.NextSibling;
                end;
              end;

              if doAddMsg then begin
                // fill by markup
                if not skipAddMarkup then
                  scheck:=Nodex.AsMarkup;

                stemp:=LogAddHead+pchar(UTF8Encode(scheck))+LogAddTail;
                WebSockChat.BroadcastMsg(stemp);
                if IsAlert then
                  WebSockAlert.BroadcastMsg(stemp);
                //ssockout:=ssockout+stemp+#13;
                //ChatBuffer.Add(stemp);
                if UseTimeInRawChat then
                  sbuf:=TimeToStr(Now)+' '+sbuf;
                WebSockRawChat.BroadcastMsg(sbuf);

                // log
                if not disLog then
                  FormTwitchChat.log.AddLog(UTF8Encode(Copy(sbuf,1,loglinelen)));
              end;
            end;

            if Nodex=NodeEnd then
              break;
            Nodex:=Nodex.NextSibling;
          end;

          //if ssockout<>'' then
          //  WebSockChat.BroadcastMsg(ssockout);

          // set last checksum
          if chkCount>0 then begin
            for i:=0 to chkCount-1 do
              lastchecksum[i]:=bottomchecksum[i];
            dupCount[chkCount]:=0;
            lastDupChk:=dupCount;
            lastchkCount:=chkCount;
          end;

          break;

        end;
        ProcessNode(Node);
        Node := Node.NextSibling;
      end;
    end;
  end;

begin
  if TryEnter then begin
    try
      if Assigned(document.Head) then begin
        {
        NodeH := document.Head.FirstChild;
        while Assigned(NodeH) do begin
          stemp:=UTF8Encode(NodeH.AsMarkup);
          if ChatHead.IndexOf(stemp)=-1 then
            ChatHead.Add(stemp);
          NodeH:=NodeH.NextSibling;
        end;
        }
      end;
      ProcessNode(document.Body);
    finally
      Leave;
    end;
  end;
end;

function CheckParam(const str:string):Boolean;
var
  s:string;
  i:Integer;
begin
  Result:=False;
  if ParamCount>0 then
    for i:=1 to ParamCount do begin
      s:=ParamStr(i);
      if CompareText(s,str)=0 then begin
        Result:=True;
        break;
      end;
    end;
end;

{ TFormTwitchChat }

procedure TFormTwitchChat.FormCreate(Sender: TObject);
const
  dummy : string = #1;
begin
  // Chrome
  FCanClose:=False;
  FClosing:=False;

  skipchecksum:=MakeHash(@dummy[1],Length(dummy));

  IsMultiThread:=True;
  UserAlertID:=TFPStringHashTableList.Create;
  UserSkipID:=TFPStringHashTableList.Create;
  LogEleChatSkip:=TFPStringHashTableList.Create;
  lastchkCount:=0;
  //ChatBuffer:=TCefChatBuffer.Create;
  log:=TLogListFPC.Create(self);
  log.Parent:=Panel2;
  log.Align:=alClient;
  FEventMain:=TEvent.Create(nil,True,True,'TWITCHMAIN'+IntToStr(GetTickCount64));

  // chrome
  CreateGlobalCEFApp;
end;

procedure TFormTwitchChat.FormDestroy(Sender: TObject);
begin
  //ChatBuffer.Free;
  FEventMain.Free;

  WebSockChat.Free;
  WebSockAlert.Free;
  WebSockRawChat.Free;
  LogEleChatSkip.Free;
  UserAlertID.Free;
  UserSkipID.Free;
  Sleep(100);
end;

procedure TFormTwitchChat.ButtonActClick(Sender: TObject);
begin
  Timer1.Enabled:=not Timer1.Enabled;
  if Timer1.Enabled then
    ButtonAct.Caption:='Stop'
    else
      ButtonAct.Caption:='Activate';
end;

procedure TFormTwitchChat.Button2Click(Sender: TObject);
begin
  if Chromium1.Initialized then
    Chromium1.LoadURL(UTF8Encode(rootUrl));
end;

procedure TFormTwitchChat.CheckBoxImgLoadingClick(Sender: TObject);
begin
  if CheckBoxImgLoading.Checked then
    Chromium1.Options.ImageLoading:=STATE_DISABLED
    else
      Chromium1.Options.ImageLoading:=STATE_ENABLED;
end;

procedure TFormTwitchChat.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
const
  baseaddr='twitch.tv/';
var
  s:string;
  i,j,l,k:Integer;
begin
    s:=UTF8Encode(url);
    if s<>'about:blank' then begin
      EditCEFUrl.Text:=s;
      if CheckBoxAutoUrl.Checked then begin
        l:=Length(s);
        i:=Pos(baseaddr,LowerCase(s));
        j:=Pos('/chat',LowerCase(s));
        j:=j+Pos('/search',LowerCase(s)); // prevent infinite renaming
        // auto navigate to chat url
        if i<>0 then begin
          if (i+Length(baseaddr)>l) then
          i:=0
          else
            Inc(i,Length(baseaddr));
        end;
        if (i<>0) and (j=0) then begin
          k:=l;
          while k>i do begin
            if s[k]='/' then
              break;
            Dec(k);
          end;
          if k<=i then
            TimerNav.Enabled:=True;
        end;
      end;
    end;
end;

procedure TFormTwitchChat.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TFormTwitchChat.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TFormTwitchChat.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TFormTwitchChat.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
  const errorText, failedUrl: ustring);
var
  errorstr:string;
begin
  case errorCode of
  ERR_ABORTED: errorstr:='Aborted';
  ERR_ACCESS_DENIED: errorstr:='Access denied';
  ERR_ADDRESS_INVALID: errorstr:='Invalid Address';
  ERR_ADDRESS_UNREACHABLE: errorstr:='Address unreachable';
  ERR_INVALID_URL: errorstr:='Invalid URL';
  ERR_NAME_NOT_RESOLVED: errorstr:='Name not resolved';
  else
    errorstr:='error';
  end;
  log.AddLog(Format('%s %s %d %s',[errorText,failedUrl,errorCode,errorstr]));
end;

procedure TFormTwitchChat.Chromium1LoadStart(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  transitionType: TCefTransitionType);
begin
  lastchkCount:=0;
  if TryEnter then begin
    try
      //if CheckBoxClearB.Checked then
      //  ChatBuffer.Clear;
      log.Font.Name:='Default';
    finally
      Leave;
    end;
  end;
end;

procedure TFormTwitchChat.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
  Result: Boolean);
var
  chatframe:ICefFrame;
  fcount, i:NativeUInt;
  fid:array of int64;
  surl:string;
begin
  if Message.Name='visitdom' then begin
    { thread-safe? }
    //if FormTwitchChat.TryEnter then begin
      try
        //fcount:=browser.GetFrameCount;
        //SetLength(fid,fcount);
        //try
        //  browser.GetFrameIdentifiers(@fcount,@fid[0]);
        //  for i:=0 to fcount-1 do begin
        //    chatframe:=browser.GetFrameByident(fid[i]);
        //    ProcessElementsById(chatframe,'log');
        //  end;
        //finally
        //  SetLength(fid,0);
        //end;
        ProcessElementsById(browser.MainFrame,LogEleName);
      finally
     //   FormTwitchChat.Leave;
      end;
    //end;
  end;
end;

procedure TFormTwitchChat.EditCEFUrlKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    Key:=#0;
    if Chromium1.Initialized then
      Chromium1.LoadURL(UTF8Decode(EditCEFUrl.Text));
  end;
end;

procedure TFormTwitchChat.ActionPortSetExecute(Sender: TObject);
var
  formPort:TFormPortSet;
  bTimer:Boolean;
begin
  formPort:=TFormPortSet.Create(self);
  try
    formPort.PortChat:=PortChat;
    formPort.PortAlert:=PortAlert;
    formPort.PortRawChat:=PortRawChat;
    formPort.Interval:=CInterval;
    if mrOK=formPort.ShowModal then begin
      PortChat:=formPort.PortChat;
      PortAlert:=formPort.PortAlert;
      PortRawChat:=formPort.PortRawChat;
      CInterval:=formPort.Interval;
      try
        bTimer:=Timer1.Enabled;
        Timer1.Enabled:=False;
        Timer1.Interval:=CInterval;
        WebSockChat.Free;
        WebSockAlert.Free;
        WebSockRawChat.Free;
        Sleep(100);
        WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat);
        WebSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert);
        WebSockRawChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortRawChat);
      except
        on e:Exception do begin
          ShowMessage(e.Message);
        end;
      end;
      Timer1.Enabled:=bTimer;
    end;
  finally
    formPort.Free;
  end;
end;

procedure TFormTwitchChat.ActionUserListExecute(Sender: TObject);
var
  userlist:TFormUserList;
  lastTimer:Boolean;
begin
  userlist:=TFormUserList.Create(self);
  try
    userlist.MemoAlert.Lines.Delimiter:=',';
    userlist.MemoAlert.Lines.DelimitedText:=UserAlertID.Text;
    userlist.MemoSkip.Lines.Delimiter:=',';
    userlist.MemoSkip.Lines.DelimitedText:=UserSkipID.Text;
    if userlist.ShowModal=mrOK then begin
      lastTimer:=Timer1.Enabled;
      Timer1.Enabled:=False;
      UserAlertID.Text:=userlist.MemoAlert.Lines.DelimitedText;
      UserSkipID.Text:=userlist.MemoSkip.Lines.DelimitedText;
      Timer1.Enabled:=lastTimer;
    end;
  finally
    userlist.Free;
  end;
end;

procedure TFormTwitchChat.ActionParserSetExecute(Sender: TObject);
var
  pform:TFormParserSet;
begin
  pform:=TFormParserSet.Create(self);
  try
    pform.ValueListEditorParser.Values['LogEleTag']:=LogEleTag;
    pform.ValueListEditorParser.Values['LogEleAttr']:=LogEleAttr;
    pform.ValueListEditorParser.Values['LogEleName']:=LogEleName;
    pform.ValueListEditorParser.Values['LogEleAlertAttr']:=LogEleAlertAttr;
    pform.ValueListEditorParser.Values['LogEleAlert']:=LogEleAlert;
    pform.ValueListEditorParser.Values['LogEleSys']:=LogEleSys;
    pform.ValueListEditorParser.Values['LogAddHead']:=LogAddHead;
    pform.ValueListEditorParser.Values['LogAddTail']:=LogAddTail;
    if pform.ShowModal=mrOK then begin
      LogEleTag:=pform.ValueListEditorParser.Values['LogEleTag'];
      LogEleAttr:=pform.ValueListEditorParser.Values['LogEleAttr'];
      LogEleName:=pform.ValueListEditorParser.Values['LogEleName'];
      LogEleAlertAttr:=pform.ValueListEditorParser.Values['LogEleAlertAttr'];
      LogEleAlert:=pform.ValueListEditorParser.Values['LogEleAlert'];
      LogEleSys:=pform.ValueListEditorParser.Values['LogEleSys'];
      LogAddHead:=pform.ValueListEditorParser.Values['LogAddHead'];
      LogAddTail:=pform.ValueListEditorParser.Values['LogAddTail'];
    end;
  finally
    pform.Free;
  end;
end;

procedure TFormTwitchChat.ActionActiveStartExecute(Sender: TObject);
begin
  ActionActiveStart.Checked:=not ActionActiveStart.Checked;
end;

procedure TFormTwitchChat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  config : TIniFile;
  stemp : string;
begin
  config:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    config.WriteString('PORT','CHAT',PortChat);
    config.WriteString('PORT','ALERT',PortAlert);
    config.WriteString('PORT','RAWCHAT',PortRawChat);
    config.WriteString('URL','ADDR',EditCEFUrl.Text);
    config.WriteBool('URL','NOIMG',CheckBoxImgLoading.Checked);
    config.WriteInteger('URL','INT',CInterval);
    config.WriteBool('URL','ACTIVE',ActionActiveStart.Checked);

    config.WriteString('PARSER','LogEleTag',LogEleTag);
    config.WriteString('PARSER','LogEleAttr',LogEleAttr);
    config.WriteString('PARSER','LogEleName',LogEleName);
    config.WriteString('PARSER','LogEleAlertAttr',LogEleAlertAttr);
    config.WriteString('PARSER','LogEleAlert',LogEleAlert);
    config.WriteString('PARSER','LogEleSys',LogEleSys);
    config.WriteString('PARSER','LogAddHead',LogAddHead);
    config.WriteString('PARSER','LogAddTail',LogAddTail);

    config.WriteString('PARSER','LogEleUser',LogEleUser);
    config.WriteString('PARSER','LogEleUserCon',LogEleUserCon);
    config.WriteString('PARSER','LogEleUserName',LogEleUserName);
    config.WriteString('PARSER','LogEleUserAttr',LogEleUserAttr);
    config.WriteString('PARSER','LogEleCon',LogEleCon);
    config.WriteString('PARSER','LogEleHigh',LogEleHigh);

    config.WriteString('PARSER','LogEleChatAttr',LogEleChatAttr);
    config.WriteString('PARSER','LogEleChatName',LogEleChatName);

    config.WriteString('PARSER','LogEleChatFrag',LogEleChatFrag);
    config.WriteString('PARSER','LogEleChatEmote',LogEleChatEmote);
    config.WriteString('PARSER','LogEleChatSkip',LogEleChatSkip.Text);
    config.WriteString('USERALERT','USER',UserAlertID.Text);
    config.WriteString('USERSKIP','USER',UserSkipID.Text);
  finally
    config.Free
  end;
end;

procedure TFormTwitchChat.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  CanClose:=FCanClose;
  if not FClosing then begin
    FClosing:=True;
    Visible:=False;
    Chromium1.CloseBrowser(True);
  end;
end;

procedure TFormTwitchChat.FormShow(Sender: TObject);
var
  config : TIniFile;
begin

  config:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    PortChat:=config.ReadString('PORT','CHAT',PortChat);
    PortAlert:=config.ReadString('PORT','ALERT',PortAlert);
    PortRawChat:=config.ReadString('PORT','RAWCHAT',PortRawChat);
    EditCEFUrl.Text:=config.ReadString('URL','ADDR',rootUrl);
    CheckBoxImgLoading.Checked:=config.ReadBool('URL','NOIMG',False);
    CInterval:=config.ReadInteger('URL','INT',500);
    ActionActiveStart.Checked:=config.ReadBool('URL','ACTIVE',True);

    LogEleTag:=config.ReadString('PARSER','LogEleTag',LogEleTag);
    LogEleAttr:=config.ReadString('PARSER','LogEleAttr',LogEleAttr);
    LogEleName:=config.ReadString('PARSER','LogEleName',LogEleName);
    LogEleAlertAttr:=config.ReadString('PARSER','LogEleAlertAttr',LogEleAlertAttr);
    LogEleAlert:=config.ReadString('PARSER','LogEleAlert',LogEleAlert);
    LogEleSys:=config.ReadString('PARSER','LogEleSys',LogEleSys);
    LogAddHead:=config.ReadString('PARSER','LogAddHead',LogAddHead);
    LogAddTail:=config.ReadString('PARSER','LogAddTail',LogAddTail);

    LogEleUser:=config.ReadString('PARSER','LogEleUser',LogEleUser);
    LogEleUserCon:=config.ReadString('PARSER','LogEleUserCon',LogEleUserCon);
    LogEleUserName:=config.ReadString('PARSER','LogEleUserName',LogEleUserName);
    LogEleUserAttr:=config.ReadString('PARSER','LogEleUserAttr',LogEleUserAttr);
    LogEleCon:=config.ReadString('PARSER','LogEleCon',LogEleCon);
    LogEleHigh:=config.ReadString('PARSER','LogEleHigh',LogEleHigh);

    LogEleChatAttr:=config.ReadString('PARSER','LogEleChatAttr',LogEleChatAttr);
    LogEleChatName:=config.ReadString('PARSER','LogEleChatName',LogEleChatName);

    LogEleChatFrag:=config.ReadString('PARSER','LogEleChatFrag',LogEleChatFrag);
    LogEleChatEmote:=config.ReadString('PARSER','LogEleChatEmote',LogEleChatEmote);
    LogEleChatSkip.Text:=config.ReadString('PARSER','LogEleChatSkip',LogEleChatSkipDefault);
    UserAlertID.Text:=config.ReadString('USERALERT','USER','');
    UserSkipID.Text:=config.ReadString('USERSKIP','USER','');

    UseTimeInRawChat:=config.ReadBool('PARSER','USETIMERAWCHAT',UseTimeInRawChat);
  finally
    config.Free
  end;


  try
    Timer1.Interval:=CInterval;
    WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat);
    WebSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert);
    WebSockRawChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortRawChat);

    CheckBoxImgLoadingClick(nil);

    if not(Chromium1.CreateBrowser(CEFWindowParent1, 'cefTwitch')) then
      TimerChrome.Enabled := True;
  except
    on e:exception do
      ShowMessage(e.Message);
  end;
  CheckBoxImgLoading.OnClick:=@CheckBoxImgLoadingClick;
  if ActionActiveStart.Checked then
    ButtonAct.Click;
end;

procedure TFormTwitchChat.Timer1Timer(Sender: TObject);
begin
  PostMessage(Handle,WM_CEFMsg,0,0);
end;

procedure TFormTwitchChat.TimerChromeTimer(Sender: TObject);
begin
  TimerChrome.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, 'cefTwitch')) and not(Chromium1.Initialized) then
    TimerChrome.Enabled := True;

  if Chromium1.Initialized then
    Chromium1.LoadURL(EditCEFUrl.Text);
end;

procedure TFormTwitchChat.TimerNavTimer(Sender: TObject);
var
  key:char;
begin
  TimerNav.Enabled:=False;
  try
    EditCEFUrl.Text:=EditCEFUrl.Text+'/chat';
    key:=#13;
    EditCEFUrlKeyPress(nil,key);
  except
    TimerNav.Enabled:=True;
  end;
end;

procedure TFormTwitchChat.BrowserCreateMsg(var aMsg: TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TFormTwitchChat.BrowserDestroyMsg(var aMsg: TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TFormTwitchChat.WMMove(var aMsg: TMessage);
begin
  inherited;
  if Chromium1<>nil then
     Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TFormTwitchChat.WMMoving(var aMsg: TMessage);
begin
  inherited;
  if Chromium1<>nil then
     Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TFormTwitchChat.WMCEFMsg(var aMsg: TMessage);
begin
  if (Chromium1<>nil) and Chromium1.Initialized then
    Chromium1.SendProcessMessage(PID_RENDERER,TCefProcessMessageRef.New('visitdom'));
end;

function TFormTwitchChat.TryEnter: Boolean;
begin
  Result:=FEventMain.WaitFor(0)<>wrTimeout;
  if Result then
    FEventMain.ResetEvent;
end;

procedure TFormTwitchChat.Leave;
begin
  FEventMain.SetEvent;
end;

procedure AppExceptProc(Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);
begin
  ShowMessage(Format('%s',[BacktraceStrFunc(Addr)]));
end;

initialization
  //ExceptProc:=@AppExceptProc;

end.


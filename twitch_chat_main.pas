unit twitch_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, cef3types, cef3lib, cef3intf, cef3lcl, cef3ref, cef3api,
  cef3own, cef3gui, lNetComponents, lhttp, lNet, UniqueInstance, loglistfpc,
  syncobjs;

type

  { TTwitchRenderProcessHandler }

  TTwitchRenderProcessHandler=class(TCefRenderProcessHandlerOwn)
    protected
      function OnProcessMessageReceived(const browser: ICefBrowser;
        sourceProcess: TCefProcessId; const message: ICefProcessMessage
        ): Boolean; override;
      procedure OnBrowserCreated(const browser: ICefBrowser); override;
      procedure OnUncaughtException(const browser: ICefBrowser;
        const frame: ICefFrame; const context: ICefV8Context;
        const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
        override;
  end;


  { TFormTwitchChat }

  TFormTwitchChat = class(TForm)
    ActionActiveStart: TAction;
    ActionParserSet: TAction;
    ActionPortSet: TAction;
    ActionList1: TActionList;
    ButtonAct: TButton;
    Button2: TButton;
    CheckBoxImgLoading: TCheckBox;
    CheckBoxAutoUrl: TCheckBox;
    CheckBoxRemSyS: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    CheckBoxClearB: TCheckBox;
    EditCEFUrl: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    TimerNav: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ActionActiveStartExecute(Sender: TObject);
    procedure ActionParserSetExecute(Sender: TObject);
    procedure ActionPortSetExecute(Sender: TObject);
    procedure ButtonActClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxImgLoadingClick(Sender: TObject);
    procedure EditCEFUrlKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerNavTimer(Sender: TObject);
  private
    FEventMain:TEvent;
  public
    log:TLogListFPC;

    function TryEnter:Boolean;
    procedure Leave;

    procedure CefLoadStart(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; transitionType: TCefTransitionType);
    procedure CefAddressChange(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring);

  end;

var
  FormTwitchChat: TFormTwitchChat;

implementation

{$R *.lfm}

uses
  uChatBuffer, uRequestHandler, uWebsockSimple, form_portset, IniFiles,
  uformParserTag, uhashimpl, Hash, DefaultTranslator;

const
  MaxChecksum = 3;
  rootUrl = 'https://www.twitch.tv';

var
  cefb : TChromium;
  MainBrowser : ICefBrowser;

  lastchecksum : array[0..MaxChecksum] of THashDigest;
  lastchkCount : Integer;
  lastDupChk : array[0..MaxChecksum] of Integer;

  //ChatBuffer:TCefChatBuffer;

  WebSockChat:TSimpleWebsocketServer;
  WebSockAlert:TSimpleWebsocketServer;

  //WClient:TSQLHttpClientWebsockets;


  PortChat:string  = '8096';
  PortAlert:string = '8098';
  CInterval:Integer = 500;

  PortClient:string = '8092';

  // html parsing
  LogEleTag : UnicodeString = 'DIV';
  LogEleAttr : UnicodeString = 'role';
  LogEleName : UnicodeString = 'log';

  LogEleAlertAttr : UnicodeString = 'CLASS';
  LogEleAlert : UnicodeString = 'user-notice-line';
  LogEleSys : UnicodeString = 'chat-line__status';

  LogEleUser : UnicodeString = 'chat-line__username';
  LogEleUserName : UnicodeString = 'chat-author__display-name';
  LogEleUserAttr : UnicodeString = 'data-a-user';

  LogAddHead : string = '<li class="twitch_chat">';
  LogAddTail : string = '</li>';

  UserAlertID : TStringList;

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

{ TTwitchRenderProcessHandler }

function TTwitchRenderProcessHandler.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
var
  chatframe:ICefFrame;
  fcount, i:TSize;
  fid:array of int64;
  surl:string;
begin
  Result:=inherited OnProcessMessageReceived(browser, sourceProcess, message);
  if not Result then
  if message.Name='visitdom' then begin
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
      finally
        FormTwitchChat.Leave;
      end;
    end;
    Result:=True;
  end;
end;

procedure TTwitchRenderProcessHandler.OnBrowserCreated(const browser: ICefBrowser);
begin
  inherited OnBrowserCreated(browser);
  MainBrowser:=browser;
end;

procedure TTwitchRenderProcessHandler.OnUncaughtException(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefV8Context; const exception: ICefV8Exception;
  const stackTrace: ICefV8StackTrace);
begin
  inherited OnUncaughtException(browser, frame, context, exception, stackTrace);
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

procedure TElementIdVisitor.Visit(const document: ICefDomDocument);
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
    chkCount, i, j, ItemCount : Integer;
    matched, skipAddMarkup, disLog, RemoveSys, doAddMsg, IsAlert : Boolean;
    ssockout, stemp: string;
  begin
    if Assigned(ANode) then
    begin
      RemoveSys:=FormTwitchChat.CheckBoxRemSyS.Checked;
      disLog:=FormTwitchChat.CheckBoxDisableLog.Checked;
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

              // make checksum source
              scheck:=NodeN.ElementInnerText;
              NodeIcon:=NodeN.FirstChild;
              if Assigned(NodeIcon) then begin
                scheck:=scheck+NodeIcon.ElementInnerText;
                NodeChat:=NodeIcon.NextSibling;
                if Assigned(NodeChat) then
                  scheck:=scheck+NodeChat.ElementInnerText;
              end;
              checksumN:=MakeHash(@scheck[1],Length(scheck)*SizeOf(WideChar));

              if matched and (i<lastchkCount) then begin
                if CompareHash(checksumN,lastchecksum[i]) then begin
                  Dec(dupCountChk[i]);
                  if dupCountChk[i]=0 then
                    Inc(i);
                end else
                  matched:=False;
              end;

              // fill bottom checksum
              if chkCount<=MaxChecksum then begin
                // check duplication on first checksum
                if (chkCount>0) and CompareHash(checksumN,bottomchecksum[chkCount-1]) then
                  Inc(dupCount[chkCount-1])
                else begin
                  bottomchecksum[chkCount]:=checksumN;
                  dupCount[chkCount]:=1;
                  Inc(chkCount);
                end;
              end else
                if i>=lastchkCount then
                  break;

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

            doAddMsg:=True;

            NodeIcon:=Nodex.FirstChild;
            if Assigned(NodeIcon) then
              NodeChat:=NodeIcon.NextSibling
              else
                NodeChat:=nil;

            sclass:=Nodex.GetElementAttribute(LogEleAlertAttr);
            IsAlert:=False;
            if Pos(LogEleAlert,sclass)<>0 then
              IsAlert:=True;
            if RemoveSys then
              doAddMsg:=Pos(LogEleSys,sclass)=0;

            scheck:=Nodex.AsMarkup;
            skipAddMarkup:=True;
            // get chat message
            if not disLog then
              sbuf:=NodeIcon.ElementInnerText;
            while Assigned(NodeChat) do begin
              // check user alert
              if (not IsAlert) and (UserAlertID.Count>0) then begin
                sclass:=NodeChat.GetElementAttribute(LogEleAlertAttr);
                if Pos(LogEleUser,sclass)<>0 then begin
                  NodeN:=NodeChat.FirstChild;
                  while Assigned(NodeN) do begin
                    sclass:=NodeN.GetElementAttribute(LogEleAlertAttr);
                    if sclass<>'' then begin
                      if Pos(LogEleUserName,sclass)<>0 then begin
                        sclass:=NodeN.GetElementAttribute(LogEleUserAttr);
                        if UserAlertID.IndexOf(sclass)<>-1 then
                          IsAlert:=True;
                        break;
                      end;
                      NodeN:=NodeN.NextSibling;
                    end else
                      NodeN:=NodeN.FirstChild;
                  end;
                end;
              end;

              if not disLog then
                sbuf:=sbuf+NodeChat.ElementInnerText;
              NodeChat:=NodeChat.NextSibling;
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

              // log
              if not disLog then begin
                FormTwitchChat.log.AddLog(UTF8Encode(sbuf));
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


{ TFormTwitchChat }

procedure TFormTwitchChat.FormCreate(Sender: TObject);
begin
  IsMultiThread:=True;
  UserAlertID:=TStringList.Create;
  UserAlertID.Delimiter:=',';
  lastchkCount:=0;
  //ChatBuffer:=TCefChatBuffer.Create;
  log:=TLogListFPC.Create(self);
  log.Parent:=Panel2;
  log.Align:=alClient;
  FEventMain:=TEvent.Create(nil,True,True,'TWITCHMAIN'+IntToStr(GetTickCount64));
  CefSingleProcess:=True; //must be true
  CefLogSeverity:=LOGSEVERITY_ERROR_REPORT;

  cefb:=TChromium.Create(self);
  cefb.Name:='cefTwitch';
  cefb.Parent:=Panel1;
  cefb.Align:=alClient;
  cefb.OnLoadStart:=@CefLoadStart;
  cefb.OnAddressChange:=@CefAddressChange;
end;

procedure TFormTwitchChat.FormDestroy(Sender: TObject);
begin
  //ChatBuffer.Free;
  FEventMain.Free;

  //WClient.Free;
  WebSockChat.Free;
  WebSockAlert.Free;
  UserAlertID.Free;
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
  cefb.Load(UTF8Encode(rootUrl));
end;

procedure TFormTwitchChat.CheckBoxImgLoadingClick(Sender: TObject);
begin
  if CheckBoxImgLoading.Checked then
    cefb.Options.ImageLoading:=STATE_DISABLED
    else
      cefb.Options.ImageLoading:=STATE_ENABLED;
end;

procedure TFormTwitchChat.EditCEFUrlKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    Key:=#0;
    cefb.Load(UTF8Decode(EditCEFUrl.Text));
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
    formPort.Interval:=CInterval;
    if mrOK=formPort.ShowModal then begin
      PortChat:=formPort.PortChat;
      PortAlert:=formPort.PortAlert;
      CInterval:=formPort.Interval;
      try
        bTimer:=Timer1.Enabled;
        Timer1.Enabled:=False;
        Timer1.Interval:=CInterval;
        WebSockChat.Free;
        webSockAlert.Free;
        //WClient.Free;
        Sleep(100);
        WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat); //,ChatBuffer);
        webSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert); //,ChatBuffer);
        //WClient:=TSQLHttpClientWebsockets.Create('localhost',PortClient,TSQLModel.Create([]));
        //WClient.ServerTimestampSynchronize;
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
begin
  config:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    config.WriteString('PORT','CHAT',PortChat);
    config.WriteString('PORT','ALERT',PortAlert);
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
    config.WriteString('PARSER','LogEleUserName',LogEleUserName);
    config.WriteString('PARSER','LogEleUserAttr',LogEleUserAttr);

    config.WriteString('USERALERT','USER',UserAlertID.DelimitedText);
  finally
    config.Free
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
    LogEleUserName:=config.ReadString('PARSER','LogEleUserName',LogEleUserName);
    LogEleUserAttr:=config.ReadString('PARSER','LogEleUserAttr',LogEleUserAttr);

    UserAlertID.DelimitedText:=config.ReadString('USERALERT','USER','');
  finally
    config.Free
  end;


  try
    Timer1.Interval:=CInterval;
    WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat); //,ChatBuffer);
    webSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert); //,ChatBuffer);
    //WClient:=TSQLHttpClientWebsockets.Create('localhost',PortClient,TSQLModel.Create([]));
    //WClient.ServerTimestampSynchronize;

    CheckBoxImgLoadingClick(nil);

    cefb.Load(UTF8Decode(EditCEFUrl.Text));
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
  cefb.Browser.SendProcessMessage(PID_RENDERER,TCefProcessMessageRef.New('visitdom'));
end;

procedure TFormTwitchChat.TimerNavTimer(Sender: TObject);
var
  key:char;
begin
  TimerNav.Enabled:=False;
  EditCEFUrl.Text:=EditCEFUrl.Text+'/chat';
  key:=#13;
  EditCEFUrlKeyPress(nil,key);
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

procedure TFormTwitchChat.CefLoadStart(Sender: TObject; const Browser: ICefBrowser;
  const Frame: ICefFrame; transitionType: TCefTransitionType);
begin
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

procedure TFormTwitchChat.CefAddressChange(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring);
const
  baseaddr='twitch.tv/';
var
  s:string;
  i,j,l,k:Integer;
begin
  s:=UTF8Encode(url);
  EditCEFUrl.Text:=s;
  if CheckBoxAutoUrl.Checked then begin
    l:=Length(s);
    i:=Pos(baseaddr,LowerCase(s));
    j:=Pos('/chat',LowerCase(s));
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

procedure AppExceptProc(Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);
begin
  ShowMessage(Format('%s',[BacktraceStrFunc(Addr)]));
end;

initialization
  CefRenderProcessHandler := TTwitchRenderProcessHandler.Create;
  //ExceptProc:=@AppExceptProc;

end.


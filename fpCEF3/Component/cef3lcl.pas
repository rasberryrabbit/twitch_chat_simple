(*
 *                       Free Pascal Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Author: dliw <dev.dliw@gmail.com>
 * Repository: http://github.com/dliw/fpCEF3
 *
 *)

Unit cef3lcl;

{$I ../cef.inc}

{
 Choose the right backend depending on used LCL widgetset
 Currently supported:
   Windows
   Linux / GTK2, QT
}

{$IFDEF LCLWin32}
  {$DEFINE TargetDefined}
{$ENDIF}
{$IFDEF LCLGTK2}
  {$IFDEF LINUX}
    {$DEFINE TargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLQT}
  {$IFDEF LINUX}
    {$DEFINE TargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF DARWIN}
  // Allow installation as design time package
  {$DEFINE TargetDefined}

  {$IFNDEF LCLCOCOA}
    {$WARNING This LCL widgetset is not yet supported}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TargetDefined}
  {$ERROR This LCL widgetset/OS is not yet supported}
{$ENDIF}

Interface
Uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, LCLIntf, LResources, InterfaceBase,
  Graphics, LMessages,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LCLGTK2}
  Gtk2Def, gdk2x, gtk2, Gtk2Int,  Gtk2Proc, Gtk2Extra,
  {$ENDIF}
  {$IFDEF LCLQT}
  qt4, qtwidgets,
  {$ENDIF}
  {$IFDEF LCLCOCOA}
  CocoaInt, cef3cocoa,
  {$ENDIF}
  cef3types, cef3lib, cef3intf, cef3gui, cef3context;

Type

  { TCustomChromium }

  TCustomChromium = class(TWinControl, IChromiumEvents)
    private
      fParentForm: TCustomForm;

      fHandler: ICefClient;
      fBrowser: ICefBrowser;
      fBrowserId: Integer;
      fDefaultUrl: String;

      fCanvas    : TCanvas;

      { Client }
      fOnProcessMessageReceived: TOnProcessMessageReceived;

      { ContextMenuHandler }
      fOnBeforeContextMenu: TOnBeforeContextMenu;
      fOnRunContextMenu: TOnRunContextMenu;
      fOnContextMenuCommand: TOnContextMenuCommand;
      fOnContextMenuDismissed: TOnContextMenuDismissed;

      { DialogHandler }
      fOnFileDialog: TOnFileDialog;

      { DisplayHandler }
      fOnAddressChange: TOnAddressChange;
      fOnTitleChange: TOnTitleChange;
      fOnFaviconUrlchange: TOnFaviconUrlchange;
      fOnFullscreenModeChange: TOnFullscreenModeChange;
      fOnTooltip: TOnTooltip;
      fOnStatusMessage: TOnStatusMessage;
      fOnConsoleMessage: TOnConsoleMessage;

      { DownloadHandler }
      fOnBeforeDownload: TOnBeforeDownload;
      fOnDownloadUpdated: TOnDownloadUpdated;

      { DragHandler }
      fOnDragEnter: TOnDragEnter;
      fOnDraggableRegionsChanged: TOnDraggableRegionsChanged;

      { FindHandler }
      fOnFindResult: TOnFindResult;

      { FocusHandler }
      fOnTakeFocus: TOnTakeFocus;
      fOnSetFocus: TOnSetFocus;
      fOnGotFocus: TOnGotFocus;

      { GeolocationHandler }
      fOnRequestGeolocationPermission: TOnRequestGeolocationPermission;
      fOnCancelGeolocationPermission: TOnCancelGeolocationPermission;

      { JsDialogHandler }
      fOnJsdialog: TOnJsdialog;
      fOnBeforeUnloadDialog: TOnBeforeUnloadDialog;
      fOnResetDialogState: TOnResetDialogState;
      fOnDialogClosed: TOnDialogClosed;

      { KeyboardHandler }
      fOnPreKeyEvent: TOnPreKeyEvent;
      fOnKeyEvent: TOnKeyEvent;

      { LiveSpanHandler }
      fOnBeforePopup: TOnBeforePopup;
      fOnAfterCreated: TOnAfterCreated;
      fOnBeforeClose: TOnBeforeClose;
      fOnClose: TOnClose;

      { LoadHandler }
      fOnLoadingStateChange: TOnLoadingStateChange;
      fOnLoadStart: TOnLoadStart;
      fOnLoadEnd: TOnLoadEnd;
      fOnLoadError: TOnLoadError;

      { RequestHandler }
      fOnBeforeBrowse: TOnBeforeBrowse;
      fOnOpenUrlFromTab: TOnOpenUrlFromTab;
      fOnBeforeResourceLoad: TOnBeforeResourceLoad;
      fOnGetResourceHandler: TOnGetResourceHandler;
      fOnResourceRedirect: TOnResourceRedirect;
      fOnResourceResponse: TOnResourceResponse;
      fOnGetResourceResponseFilter: TOnGetResourceResponseFilter;
      fOnResourceLoadComplete: TOnResourceLoadComplete;
      fOnGetAuthCredentials: TOnGetAuthCredentials;
      fOnQuotaRequest: TOnQuotaRequest;
      fOnProtocolExecution: TOnProtocolExecution;
      fOnCertificateError: TOnCertificateError;
      fOnSelectClientCertificate: TOnSelectClientCertificate;
      fOnPluginCrashed: TOnPluginCrashed;
      fOnRenderViewReady: TOnRenderViewReady;
      fOnRenderProcessTerminated: TOnRenderProcessTerminated;

      { RequestContext }
      fChromiumContext: TCustomChromiumContext;
      fRequestContext: ICefRequestContext;

      fOptions: TChromiumOptions;
      fFontOptions: TChromiumFontOptions;

      fDefaultEncoding: String;
      fAcceptLanguageList: String;

      procedure GetSettings(var settings: TCefBrowserSettings);
      procedure CreateBrowser;
    protected
      procedure CreateWnd; override;
      procedure WMPaint(var Msg : TLMPaint); message LM_PAINT;
      {$IFDEF WINDOWS}
        procedure WndProc(var Message : TLMessage); override;
      {$ENDIF}
      {$IFDEF LINUX}
        procedure DoExit; override;
        procedure DoEnter; override;
        procedure SetVisible(Value: Boolean); override;
      {$ENDIF}

      procedure Resize; override;
    protected
      { Client }
      function doOnProcessMessageReceived(const Browser_: ICefBrowser;
        sourceProcess: TCefProcessId; const Message: ICefProcessMessage): Boolean; virtual;

      { ContextMenuHandler }
      procedure doOnBeforeContextMenu(const Browser_: ICefBrowser; const Frame: ICefFrame;
        const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
      function doRunContextMenu(const browser_: ICefBrowser; const frame: ICefFrame;
        const params: ICefContextMenuParams; const model: ICefMenuModel;
        const callback: ICefRunContextMenuCallback): Boolean; virtual;
      function doOnContextMenuCommand(const Browser_: ICefBrowser; const Frame: ICefFrame;
        const params: ICefContextMenuParams; commandId: Integer;
        eventFlags: TCefEventFlags): Boolean; virtual;
      procedure doOnContextMenuDismissed(const Browser_: ICefBrowser; const Frame: ICefFrame); virtual;

      { DialogHandler }
      function doOnFileDialog(const browser_: ICefBrowser; mode: TCefFileDialogMode;
        const title, defaultFileName: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer;
        const callback: ICefFileDialogCallback): Boolean; virtual;

      { DisplayHandler }
      procedure doOnAddressChange(const Browser_: ICefBrowser; const Frame: ICefFrame; const url: ustring); virtual;
      procedure doOnTitleChange(const Browser_: ICefBrowser; const title: ustring); virtual;
      procedure doOnFaviconUrlchange(const browser_: ICefBrowser; iconUrls: TStrings); virtual;
      procedure doOnFullscreenModeChange(const browser_: ICefBrowser; fullscreen: Boolean); virtual;
      function doOnTooltip(const Browser_: ICefBrowser; var atext: ustring): Boolean; virtual;
      procedure doOnStatusMessage(const Browser_: ICefBrowser; const value: ustring); virtual;
      function doOnConsoleMessage(const Browser_: ICefBrowser; const Message, Source: ustring; line: Integer): Boolean; virtual;

      { DownloadHandler }
      procedure doOnBeforeDownload(const Browser_: ICefBrowser; const downloadItem: ICefDownloadItem;
        const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); virtual;
      procedure doOnDownloadUpdated(const Browser_: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback); virtual;

      { DragHandler }
      function doOnDragEnter(const Browser_: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask): Boolean; virtual;
      procedure doOnDraggableRegionsChanged(const browser_: ICefBrowser; regionsCount: TSize; const regions: TCefDraggableRegionArray); virtual;

      { FindHandler }
      procedure doOnFindResult(const browser_: ICefBrowser; identifier, count: Integer; const selectionRect: TCefRect; activeMatchOridinal: Integer; finalUpdate: Boolean); virtual;

      { FocusHandler }
      procedure doOnTakeFocus(const Browser_: ICefBrowser; next_: Boolean); virtual;
      function doOnSetFocus(const Browser_: ICefBrowser; Source: TCefFocusSource): Boolean; virtual;
      procedure doOnGotFocus(const Browser_: ICefBrowser); virtual;

      { GeolocationHandler }
      function doOnRequestGeolocationPermission(const browser_: ICefBrowser; const requestingUrl: ustring;
        requestId: Integer; const callback: ICefGeolocationCallback): Boolean; virtual;
      procedure doOnCancelGeolocationPermission(const Browser_: ICefBrowser; requestId: Integer); virtual;

      { JsDialogHandler }
      function doOnJsdialog(const Browser_: ICefBrowser; const originUrl: ustring;
        dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
        callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; virtual;
      function doOnBeforeUnloadDialog(const Browser_: ICefBrowser;
        const messageText: ustring; isReload: Boolean;
        const callback: ICefJsDialogCallback): Boolean; virtual;
      procedure doOnResetDialogState(const Browser_: ICefBrowser); virtual;
      procedure doOnDialogClosed(const browser_: ICefBrowser); virtual;

      { KeyboardHander }
      function doOnPreKeyEvent(const Browser_: ICefBrowser; const event: PCefKeyEvent;
        osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; virtual;
      function doOnKeyEvent(const Browser_: ICefBrowser; const event: PCefKeyEvent;
        osEvent: TCefEventHandle): Boolean; virtual;

      { LiveSpanHandler }
      function doOnBeforePopup(const browser_: ICefBrowser; const frame: ICefFrame;
        const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
        userGesture: Boolean; var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
        var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure doOnAfterCreated(const Browser_: ICefBrowser); virtual;
      procedure doOnBeforeClose(const Browser_: ICefBrowser); virtual;
      function doOnClose(const Browser_: ICefBrowser): Boolean; virtual;

      { LoadHandler }
      procedure doOnLoadingStateChange(const Browser_: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;
      procedure doOnLoadStart(const Browser_: ICefBrowser; const Frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure doOnLoadEnd(const Browser_: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure doOnLoadError(const Browser_: ICefBrowser; const Frame: ICefFrame; errorCode: TCefErrorCode;
        const errorText, failedUrl: ustring); virtual;

      { RenderHandler }
      function doOnGetRootScreenRect(const Browser_: ICefBrowser; rect: PCefRect): Boolean; virtual;
      function doOnGetViewRect(const Browser_: ICefBrowser; rect: PCefRect): Boolean; virtual;
      function doOnGetScreenPoint(const Browser_: ICefBrowser; viewX, viewY: Integer;
        screenX, screenY: PInteger): Boolean; virtual;
      function doOnGetScreenInfo(const browser_: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; virtual;
      procedure doOnPopupShow(const Browser_: ICefBrowser; doshow: Boolean); virtual;
      procedure doOnPopupSize(const Browser_: ICefBrowser; const rect: PCefRect); virtual;
      procedure doOnPaint(const Browser_: ICefBrowser; kind: TCefPaintElementType;
        dirtyRectsCount: TSize; const dirtyRects: TCefRectArray;
        const buffer: Pointer; awidth, aheight: Integer); virtual;
      procedure doOnCursorChange(const browser_: ICefBrowser; aCursor: TCefCursorHandle; type_: TCefCursorType;
        const customCursorInfo: PCefCursorInfo); virtual;
      function doOnStartDragging(const browser_: ICefBrowser; const dragData: ICefDragData;
        allowedOps: TCefDragOperationsMask; x, y: Integer): Boolean; virtual;
      procedure doOnUpdateDragCursor(const browser_: ICefBrowser; operation: TCefDragOperationsMask); virtual;
      procedure doOnScrollOffsetChanged(const browser_: ICefBrowser; x,y: Double); virtual;
      procedure doOnImeCompositionRangeChanged(const browser_: ICefBrowser; const selectedRange: TCefRange;
        characterBoundsCount: TSize; characterBounds: TCefRectArray); virtual;

      { RequestHandler }
      function doOnBeforeBrowse(const browser_: ICefBrowser; const frame: ICefFrame;
        const request: ICefRequest; isRedirect: Boolean): Boolean; virtual;
      function doOnOpenUrlFromTab(const browser_: ICefBrowser; const frame: ICefFrame;
        const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
        useGesture: Boolean): Boolean; virtual;
      function doOnBeforeResourceLoad(const browser_: ICefBrowser; const frame: ICefFrame;
        const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      function doOnGetResourceHandler(const Browser_: ICefBrowser; const Frame: ICefFrame;
        const request: ICefRequest): ICefResourceHandler; virtual;
      procedure doOnResourceRedirect(const browser_: ICefBrowser; const frame: ICefFrame;
        const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function doOnResourceResponse(const browser_: ICefBrowser; const frame: ICefFrame;
        const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      function doOnGetResourceResponseFilter(const browser_: ICefBrowser;
        const frame: ICefFrame; const request: ICefRequest;
        const response: ICefResponse): ICefResponseFilter; virtual;
      procedure doOnResourceLoadComplete(const browser_: ICefBrowser;
        const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse;
        status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      function doOnGetAuthCredentials(const Browser_: ICefBrowser; const Frame: ICefFrame;
        isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
        const callback: ICefAuthCallback): Boolean; virtual;
      function doOnQuotaRequest(const Browser_: ICefBrowser; const originUrl: ustring;
        newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      procedure doOnProtocolExecution(const Browser_: ICefBrowser;
        const url: ustring; out allowOsExecution: Boolean); virtual;
      function doOnCertificateError(const browser_: ICefBrowser; certError: TCefErrorCode;
        const requestUrl: ustring; const sslInfo: ICefSslinfo; callback: ICefRequestCallback): Boolean;
      function doOnSelectClientCertificate(const browser_: ICefBrowser; isProxy: Boolean;
        const host: ustring; port: Integer; certificatesCount: TSize;
        certificates: ICefX509certificateArray; callback: ICefSelectClientCertificateCallback): Boolean;
      procedure doOnPluginCrashed(const Browser_: ICefBrowser; const pluginPath: ustring); virtual;
      procedure doOnRenderViewReady(const browser_: ICefBrowser); virtual;
      procedure doOnRenderProcessTerminated(const Browser_: ICefBrowser; Status: TCefTerminationStatus); virtual;

      { Client }
      property OnProcessMessageReceived: TOnProcessMessageReceived read fOnProcessMessageReceived write fOnProcessMessageReceived;

      { ContextMenuHandler }
      property OnBeforeContextMenu: TOnBeforeContextMenu read fOnBeforeContextMenu write fOnBeforeContextMenu;
      property OnRunContextMenu: TOnRunContextMenu read fOnRunContextMenu write fOnRunContextMenu;
      property OnContextMenuCommand: TOnContextMenuCommand read fOnContextMenuCommand write fOnContextMenuCommand;
      property OnContextMenuDismissed: TOnContextMenuDismissed read fOnContextMenuDismissed write fOnContextMenuDismissed;

      { DialogHandler }
      property OnFileDialog: TOnFileDialog read fOnFileDialog write fOnFileDialog;

      { DisplayHandler }
      property OnAddressChange: TOnAddressChange read fOnAddressChange write fOnAddressChange;
      property OnTitleChange: TOnTitleChange read fOnTitleChange write fOnTitleChange;
      property OnFaviconUrlchange: TOnFaviconUrlchange read fOnFaviconUrlchange write fOnFaviconUrlchange;
      property OnFullscreenModeChange: TOnFullscreenModeChange read fOnFullscreenModeChange write fOnFullscreenModeChange;
      property OnTooltip: TOnTooltip read fOnTooltip write fOnTooltip;
      property OnStatusMessage: TOnStatusMessage read fOnStatusMessage write fOnStatusMessage;
      property OnConsoleMessage: TOnConsoleMessage read fOnConsoleMessage write fOnConsoleMessage;

      { DownloadHandler }
      property OnBeforeDownload: TOnBeforeDownload read fOnBeforeDownload write fOnBeforeDownload;
      property OnDownloadUpdated: TOnDownloadUpdated read fOnDownloadUpdated write fOnDownloadUpdated;

      { DragHandler }
      property OnDragEnter: TOnDragEnter read fOnDragEnter write fOnDragEnter;
      property OnDraggableRegionsChanged: TOnDraggableRegionsChanged read fOnDraggableRegionsChanged write fOnDraggableRegionsChanged;

      { FindHandler }
      property OnFindResult: TOnFindResult read fOnFindResult write fOnFindResult;

      { FocusHandler }
      property OnTakeFocus: TOnTakeFocus read fOnTakeFocus write fOnTakeFocus;
      property OnSetFocus: TOnSetFocus read fOnSetFocus write fOnSetFocus;
      property OnGotFocus: TOnGotFocus read fOnGotFocus write fOnGotFocus;

      { GeolocationHandler }
      property OnRequestGeolocationPermission: TOnRequestGeolocationPermission read fOnRequestGeolocationPermission write fOnRequestGeolocationPermission;
      property OnCancelGeolocationPermission: TOnCancelGeolocationPermission read fOnCancelGeolocationPermission write fOnCancelGeolocationPermission;

      { JsDialogHandler }
      property OnJsdialog: TOnJsdialog read fOnJsdialog write fOnJsdialog;
      property OnBeforeUnloadDialog: TOnBeforeUnloadDialog read fOnBeforeUnloadDialog write fOnBeforeUnloadDialog;
      property OnResetDialogState: TOnResetDialogState read fOnResetDialogState write fOnResetDialogState;
      property OnDialogClosed: TOnDialogClosed read fOnDialogClosed write fOnDialogClosed;

      { KeyboardHandler }
      property OnPreKeyEvent: TOnPreKeyEvent read fOnPreKeyEvent write fOnPreKeyEvent;
      property OnKeyEvent: TOnKeyEvent read fOnKeyEvent write fOnKeyEvent;

      { LifeSpanHandler }
      property OnBeforePopup: TOnBeforePopup read fOnBeforePopup write fOnBeforePopup;
      property OnAfterCreated: TOnAfterCreated read fOnAfterCreated write fOnAfterCreated;
      property OnClose: TOnClose read fOnClose write fOnClose;
      property OnBeforeClose: TOnBeforeClose read fOnBeforeClose write fOnBeforeClose;

      { LoadHandler }
      property OnLoadingStateChange: TOnLoadingStateChange read fOnLoadingStateChange write fOnLoadingStateChange;
      property OnLoadStart: TOnLoadStart read fOnLoadStart write fOnLoadStart;
      property OnLoadEnd: TOnLoadEnd read fOnLoadEnd write fOnLoadEnd;
      property OnLoadError: TOnLoadError read fOnLoadError write fOnLoadError;

      { RequestHandler }
      property OnBeforeBrowse: TOnBeforeBrowse read fOnBeforeBrowse write fOnBeforeBrowse;
      property OnOpenUrlFromTab: TOnOpenUrlFromTab read fOnOpenUrlFromTab write fOnOpenUrlFromTab;
      property OnBeforeResourceLoad: TOnBeforeResourceLoad read fOnBeforeResourceLoad write fOnBeforeResourceLoad;
      property OnGetResourceHandler: TOnGetResourceHandler read fOnGetResourceHandler write fOnGetResourceHandler;
      property OnResourceRedirect: TOnResourceRedirect read fOnResourceRedirect write fOnResourceRedirect;
      property OnResourceResponse: TOnResourceResponse read fOnResourceResponse write fOnResourceResponse;
      property OnGetResourceResponseFilter: TOnGetResourceResponseFilter read fOnGetResourceResponseFilter write fOnGetResourceResponseFilter;
      property OnResourceLoadComplete: TOnResourceLoadComplete read fOnResourceLoadComplete write fOnResourceLoadComplete;
      property OnGetAuthCredentials: TOnGetAuthCredentials read fOnGetAuthCredentials write fOnGetAuthCredentials;
      property OnQuotaRequest: TOnQuotaRequest read fOnQuotaRequest write fOnQuotaRequest;
      property OnProtocolExecution: TOnProtocolExecution read fOnProtocolExecution write fOnProtocolExecution;
      property OnCertificateError: TOnCertificateError read fOnCertificateError write fOnCertificateError;
      property OnSelectClientCertificate: TOnSelectClientCertificate read fOnSelectClientCertificate write fOnSelectClientCertificate;
      property OnPluginCrashed: TOnPluginCrashed read fOnPluginCrashed write fOnPluginCrashed;
      property OnRenderViewReady: TOnRenderViewReady read fOnRenderViewReady write fOnRenderViewReady;
      property OnRenderProcessTerminated: TOnRenderProcessTerminated read fOnRenderProcessTerminated write fOnRenderProcessTerminated;

      { RequestContext }
      property ChromiumContext: TCustomChromiumContext read fChromiumContext write fChromiumContext;
      property RequestContext: ICefRequestContext read fRequestContext write fRequestContext;

      property BrowserId: Integer read fBrowserId;
      property Browser: ICefBrowser read fBrowser;
      property Handler: ICefClient read fHandler;

      property DefaultUrl: String read fDefaultUrl write fDefaultUrl;

      property Options: TChromiumOptions read fOptions write fOptions;
      property FontOptions: TChromiumFontOptions read fFontOptions write fFontOptions;
      property DefaultEncoding: String read fDefaultEncoding write fDefaultEncoding;
      property AcceptLanguageList: String read fAcceptLanguageList write fAcceptLanguageList;
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure Load(const url: String);
  end;

  TChromium = class(TCustomChromium)
    public
      property BrowserId;
      property Browser;

      property RequestContext;
    published
      property Color;
      property Constraints;
      property TabStop;
      property Align;
      property Anchors;
      property TabOrder;
      property Visible;

      property OnProcessMessageReceived;

      property OnBeforeContextMenu;
      property OnRunContextMenu;
      property OnContextMenuCommand;
      property OnContextMenuDismissed;

      property OnFileDialog;

      property OnAddressChange;
      property OnTitleChange;
      property OnFaviconUrlchange;
      property OnFullscreenModeChange;
      property OnTooltip;
      property OnStatusMessage;
      property OnConsoleMessage;

      property OnBeforeDownload;
      property OnDownloadUpdated;

      property OnDragEnter;
      property OnDraggableRegionsChanged;

      property OnFindResult;

      property OnTakeFocus;
      property OnSetFocus;
      property OnGotFocus;

      property OnRequestGeolocationPermission;
      property OnCancelGeolocationPermission;

      property OnJsdialog;
      property OnBeforeUnloadDialog;
      property OnResetDialogState;
      property OnDialogClosed;

      property OnPreKeyEvent;
      property OnKeyEvent;

      property OnBeforePopup;
      property OnAfterCreated;
      property OnClose;
      property OnBeforeClose;

      property OnLoadingStateChange;
      property OnLoadStart;
      property OnLoadEnd;
      property OnLoadError;

      property OnBeforeBrowse;
      property OnOpenUrlFromTab;
      property OnBeforeResourceLoad;
      property OnGetResourceHandler;
      property OnResourceRedirect;
      property OnResourceResponse;
      property OnGetResourceResponseFilter;
      property OnResourceLoadComplete;
      property OnGetAuthCredentials;
      property OnQuotaRequest;
      property OnProtocolExecution;
      property OnCertificateError;
      property OnSelectClientCertificate;
      property OnPluginCrashed;
      property OnRenderViewReady;
      property OnRenderProcessTerminated;

      property ChromiumContext;

      property DefaultUrl;

      property Options;
      property FontOptions;
      property DefaultEncoding;
      property AcceptLanguageList;
  end;

procedure Register;

Implementation

{$R icons.res}

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
Uses ExtCtrls;
Var
  CefInstances : Integer = 0;
  Timer        : TTimer;
  Looping : Boolean = False;
{$ENDIF}

Type

  { TLCLClientHandler }

  TLCLClientHandler = class(TCustomClientHandler)
  private
    class procedure OnTimer(Sender : TObject);
  public
    constructor Create(const crm: IChromiumEvents); override;
    destructor Destroy; override;
    procedure StartTimer;
  end;

procedure Register;
begin
  RegisterComponents('Chromium', [TChromium]);
end;

class procedure TLCLClientHandler.OnTimer(Sender : TObject);
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If Looping then Exit;
  If CefInstances > 0 then
  begin
    Looping := True;
    try
      CefDoMessageLoopWork;
    finally
      Looping := False;
    end;
  end;
  {$ENDIF}
end;

constructor TLCLClientHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create(crm);

  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If (not CefExternalMessagePump) and (CefInstances = 0) then
  begin
    Timer := TTimer.Create(nil);
    Timer.Interval := 15;
    Timer.Enabled := False;
    Timer.OnTimer := @OnTimer;

    {$IFDEF DEBUG}
    Debugln('Timer created.');
    {$ENDIF}
  end;

  InterLockedIncrement(CefInstances);
  {$ENDIF}

  {$IFDEF DEBUG}
  Debugln('ClientHandler instances: ', IntToStr(CefInstances));
  {$ENDIF}
end;

destructor TLCLClientHandler.Destroy;
begin
  {$IFDEF DEBUG}
  Debugln('LCLClientHandler.Destroy');
  {$ENDIF}

  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterLockedDecrement(CefInstances);

  If (CefInstances = 0) and Assigned(Timer) then
  begin
    Timer.Enabled := False;

    FreeAndNil(Timer);

    {$IFDEF DEBUG}
    Debugln('Timer cleaned.');
    {$ENDIF}
  end;
  {$ENDIF}

  inherited;
end;

procedure TLCLClientHandler.StartTimer;
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If not Assigned(Timer) then Exit;

  Timer.Enabled := True;
  {$ENDIF}
end;

{ TCustomChromium }

procedure TCustomChromium.GetSettings(var settings : TCefBrowserSettings);
begin
  If not (settings.size >= SizeOf(settings)) then raise Exception.Create('settings invalid');
  settings.standard_font_family := CefString(fFontOptions.StandardFontFamily);
  settings.fixed_font_family := CefString(fFontOptions.FixedFontFamily);
  settings.serif_font_family := CefString(fFontOptions.SerifFontFamily);
  settings.sans_serif_font_family := CefString(fFontOptions.SansSerifFontFamily);
  settings.cursive_font_family := CefString(fFontOptions.CursiveFontFamily);
  settings.fantasy_font_family := CefString(fFontOptions.FantasyFontFamily);
  settings.default_font_size := fFontOptions.DefaultFontSize;
  settings.default_fixed_font_size := fFontOptions.DefaultFixedFontSize;
  settings.minimum_font_size := fFontOptions.MinimumFontSize;
  settings.minimum_logical_font_size := fFontOptions.MinimumLogicalFontSize;

  settings.remote_fonts := fOptions.RemoteFonts;
  settings.javascript := fOptions.Javascript;
  settings.javascript_open_windows := fOptions.JavascriptOpenWindows;
  settings.javascript_close_windows := fOptions.JavascriptCloseWindows;
  settings.javascript_access_clipboard := fOptions.JavascriptAccessClipboard;
  settings.javascript_dom_paste := fOptions.JavascriptDomPaste;
  settings.plugins := fOptions.Plugins;
  settings.universal_access_from_file_urls := fOptions.UniversalAccessFromFileUrls;
  settings.file_access_from_file_urls := fOptions.FileAccessFromFileUrls;
  settings.web_security := fOptions.WebSecurity;
  settings.image_loading := fOptions.ImageLoading;
  settings.image_shrink_standalone_to_fit := fOptions.ImageShrinkStandaloneToFit;
  settings.text_area_resize := fOptions.TextAreaResize;
  settings.tab_to_links := fOptions.TabToLinks;
  settings.local_storage := fOptions.LocalStorage;
  settings.databases := fOptions.Databases;
  settings.application_cache := fOptions.ApplicationCache;
  settings.webgl := fOptions.Webgl;

  settings.default_encoding := CefString(fDefaultEncoding);
  settings.background_color := TColorToCefColor(GetColorResolvingParent);
  settings.accept_language_list := CefString(fAcceptLanguageList);
end;

procedure TCustomChromium.CreateBrowser;
Var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;

{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
  rect : TRect;
{$ENDIF}
begin
  If not (csDesigning in ComponentState) then
  begin
    FillChar(info, SizeOf(info), 0);

    {$IFDEF WINDOWS}
      rect := GetClientRect;

      info.style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
      info.parent_window := Handle;
      info.x := rect.Left;
      info.y := rect.Top;
      info.width := rect.Right - rect.Left;
      info.height := rect.Bottom - rect.Top;
    {$ENDIF}
    {$IFDEF LINUX}
      fParentForm := GetParentForm(Self);

      {$IFDEF LCLGTK2}
        gtk_widget_realize(PGtkWidget(fParentForm.Handle));
        info.parent_window := gdk_window_xwindow(PGtkWidget(fParentForm.Handle)^.window);
      {$ENDIF}
      {$IFDEF LCLQT}
        info.parent_window := QWidget_winId(TQtWidget(fParentForm.Handle).Widget);
      {$ENDIF}

      info.x := Left;
      info.y := Top;
      info.width := Width;
      info.height := Height;
    {$ENDIF}
    {$IFDEF LCLCOCOA}
      rect := GetClientRect;

      info.parent_view := TCefWindowHandle(Handle);
      info.x := rect.Left;
      info.y := rect.Top;
      info.width := rect.Right - rect.Left;
      info.height := rect.Bottom - rect.Top;
    {$ENDIF}

    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);

    // request context priority: ChromiumContext (component) > RequestContext (manually set) > nil
    If Assigned(fChromiumContext) then fRequestContext := fChromiumContext.GetRequestContext;

{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    CefBrowserHostCreateBrowser(@info, fHandler, UTF8Decode(fDefaultUrl), @settings, fRequestContext);
{$ELSE}
    fBrowser := CefBrowserHostCreateBrowserSync(@info, fHandler, UTF8Decode(fDefaultUrl), @settings, fRequestContext);
    fBrowserId := fBrowser.Identifier;
{$ENDIF}

    (fHandler as TLCLClientHandler).StartTimer;
    Load(fDefaultUrl);
  end;
end;

procedure TCustomChromium.CreateWnd;
begin
  inherited CreateWnd;

  CreateBrowser;
end;

procedure TCustomChromium.WMPaint(var Msg : TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Msg);

  If (csDesigning in ComponentState) and (fCanvas <> nil) then
  begin
    With fCanvas do
    begin
      If Msg.DC <> 0 then Handle := Msg.DC;

      Brush.Color := clLtGray;
      Pen.Color   := clRed;
      Rectangle(0, 0, Self.Width, Self.Height);
      MoveTo(0, 0);
      LineTo(Self.Width, Self.Height);
      MoveTo(0, Self.Height);
      LineTo(Self.Width, 0);

      If Msg.DC <> 0 then Handle := 0;
    end;
  end;

  Exclude(FControlState, csCustomPaint);
end;

{$IFDEF WINDOWS}
procedure TCustomChromium.WndProc(var Message : TLMessage);
begin
  Case Message.Msg of
    WM_SETFOCUS:
      begin
        If (fBrowser <> nil) and (fBrowser.Host.WindowHandle <> 0) then
          PostMessage(fBrowser.Host.WindowHandle, WM_SETFOCUS, Message.WParam, 0);

        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      If (csDesigning in ComponentState) or (fBrowser = nil) then inherited WndProc(Message);
    CM_WANTSPECIALKEY:
      If not (TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN, VK_TAB, VK_ESCAPE, VK_RETURN]) then Message.Result := 1
      Else inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTTAB;
  Else
    inherited WndProc(Message);
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
procedure TCustomChromium.DoExit;
begin
  If (not (csDesigning in ComponentState)) and Assigned(fBrowser) then CefXLooseFocus(fBrowser);

  inherited;
end;

procedure TCustomChromium.DoEnter;
begin
  If (not (csDesigning in ComponentState)) and Assigned(fBrowser) then fBrowser.Host.SetFocus(True);

  inherited;
end;

procedure TCustomChromium.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  If (not (csDesigning in ComponentState)) and Assigned(fBrowser) then CefXSetVisibility(fBrowser, Value);
end;
{$ENDIF}

procedure TCustomChromium.Resize;
{$IFDEF WINDOWS}
Var
  Hand: THandle;
  Rect: TRect;
{$ENDIF}
{$IFDEF LINUX}
Var
  Offset: TPoint;
{$ENDIF}
begin
  inherited Resize;

  If (not (csDesigning in ComponentState)) and Assigned(fBrowser) then
  begin
    {$IFDEF WINDOWS}
      If Browser.Host.WindowHandle <> INVALID_HANDLE_VALUE then
      begin
        Rect := GetClientRect;
        Hand := BeginDeferWindowPos(1);
        try
          Hand := DeferWindowPos(Hand, fBrowser.Host.WindowHandle, 0, Rect.Left, Rect.Top,
                                 Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, SWP_NOZORDER);
        finally
          EndDeferWindowPos(Hand);
        end;
      end;
    {$ENDIF}
    {$IFDEF LINUX}
      Offset := ClientToParent(Point(0, 0), fParentForm);

      CefXWindowResize(fBrowser, Offset.Y, Offset.X, Width, Height);
    {$ENDIF}
  end;
end;

constructor TCustomChromium.Create(TheOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csAcceptsControls];

  If not (csDesigning in ComponentState) then
  begin
    {$IF DEFINED(DARWIN) AND NOT DEFINED(LCLCOCOA)}
      raise Exception.Create('This widgetset is not yet supported');
    {$ENDIF}

    fHandler := TLCLClientHandler.Create(Self);

    If not Assigned(fHandler) then raise Exception.Create('fHandler is nil');
  end
  Else
  begin
    fCanvas := TControlCanvas.Create;
    TControlCanvas(fCanvas).Control := Self;
  end;

  fOptions := TChromiumOptions.Create;
  fFontOptions := TChromiumFontOptions.Create;

  fDefaultUrl := 'about:blank';
  fDefaultEncoding := '';

  fBrowserId := 0;
  fBrowser := nil;
end;

destructor TCustomChromium.Destroy;
begin
  FreeAndNil(fCanvas);

  If fBrowser <> nil then
  begin
    fBrowser.StopLoad;
    fBrowser.Host.CloseBrowser(True);
    fBrowser := nil;
  end;

  If fHandler <> nil then
  begin
    (fHandler as ICefClientHandler).Disconnect;
    fHandler := nil;
  end;

  fFontOptions.Free;
  fOptions.Free;

  inherited;
end;

procedure TCustomChromium.Load(const url: String);
Var
  Frame : ICefFrame;
begin
  If fBrowser <> nil then
  begin
    Frame := fBrowser.MainFrame;

    If Frame <> nil then
    begin
      fBrowser.StopLoad;
      Frame.LoadUrl(UTF8Decode(url));
    end;
  end;
end;

function TCustomChromium.doOnProcessMessageReceived(
  const Browser_: ICefBrowser; sourceProcess: TCefProcessId;
  const Message: ICefProcessMessage): Boolean;
begin
  If Assigned(fOnProcessMessageReceived) then
    fOnProcessMessageReceived(Self, Browser_, sourceProcess, Message, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnBeforeContextMenu(const Browser_: ICefBrowser;
  const Frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  If Assigned(fOnBeforeContextMenu) then fOnBeforeContextMenu(Self, Browser_, Frame, params, model);
end;

function TCustomChromium.doRunContextMenu(const browser_: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel; const callback: ICefRunContextMenuCallback
  ): Boolean;
begin
  If Assigned(fOnRunContextMenu) then
    fOnRunContextMenu(Self, browser_, frame, params, model, callback, Result)
  Else Result := False;
end;

function TCustomChromium.doOnContextMenuCommand(const Browser_: ICefBrowser;
  const Frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags): Boolean;
begin
  If Assigned(fOnContextMenuCommand) then
    fOnContextMenuCommand(Self, Browser_, Frame, params, commandId, eventFlags, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnContextMenuDismissed(const Browser_: ICefBrowser;
  const Frame: ICefFrame);
begin
  If Assigned(fOnContextMenuDismissed) then fOnContextMenuDismissed(Self, Browser_, Frame);
end;

function TCustomChromium.doOnFileDialog(const browser_: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptFilters: TStrings; selectedAcceptFilter: Integer;
  const callback: ICefFileDialogCallback): Boolean;
begin
  If Assigned(fOnFileDialog) then
    fOnFileDialog(Self, browser_, mode, title, defaultFileName, acceptFilters, selectedAcceptFilter,
      callback, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnAddressChange(const Browser_: ICefBrowser;
  const Frame: ICefFrame; const url: ustring);
begin
  If Assigned(fOnAddressChange) then fOnAddressChange(Self, Browser_, Frame, url);
end;

procedure TCustomChromium.doOnTitleChange(const Browser_: ICefBrowser;
  const title: ustring);
begin
  If Assigned(fOnTitleChange) then fOnTitleChange(Self, Browser_, title);
end;

procedure TCustomChromium.doOnFaviconUrlchange(const browser_: ICefBrowser;
  iconUrls: TStrings);
begin
  If Assigned(fOnFaviconUrlchange) then fOnFaviconUrlchange(Self, browser_, iconUrls);
end;

procedure TCustomChromium.doOnFullscreenModeChange(const browser_: ICefBrowser;
  fullscreen: Boolean);
begin
  If Assigned(fOnFullscreenModeChange) then fOnFullscreenModeChange(Self, browser_, fullscreen);
end;

function TCustomChromium.doOnTooltip(const Browser_: ICefBrowser;
  var atext: ustring): Boolean;
begin
  Result := False;
  If Assigned(fOnTooltip) then fOnTooltip(Self, Browser_, atext, Result);
end;

procedure TCustomChromium.doOnStatusMessage(const Browser_: ICefBrowser;
  const value: ustring);
begin
  If Assigned(fOnStatusMessage) then fOnStatusMessage(Self, Browser_, value);
end;

function TCustomChromium.doOnConsoleMessage(const Browser_: ICefBrowser;
  const Message, Source: ustring; line: Integer): Boolean;
begin
  If Assigned(fOnConsoleMessage) then
    fOnConsoleMessage(Self, Browser_, Message, Source, line, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnBeforeDownload(const Browser_: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin
  If Assigned(fOnBeforeDownload) then
    fOnBeforeDownload(Self, Browser_, downloadItem, suggestedName, callback);
end;

procedure TCustomChromium.doOnDownloadUpdated(const Browser_: ICefBrowser;
  const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback
  );
begin
  If Assigned(fOnDownloadUpdated) then fOnDownloadUpdated(Self, Browser_, downloadItem, callback);
end;

function TCustomChromium.doOnDragEnter(const Browser_: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperationsMask): Boolean;
begin
  If Assigned(fOnDragEnter) then fOnDragEnter(Self, Browser_, dragData, mask, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnDraggableRegionsChanged(
  const browser_: ICefBrowser; regionsCount: TSize;
  const regions: TCefDraggableRegionArray);
begin
  If Assigned(fOnDraggableRegionsChanged) then
    fOnDraggableRegionsChanged(Self, browser_, regionsCount, regions);
end;

procedure TCustomChromium.doOnFindResult(const browser_: ICefBrowser;
  identifier, count: Integer; const selectionRect: TCefRect;
  activeMatchOridinal: Integer; finalUpdate: Boolean);
begin
  If Assigned(fOnFindResult) then fOnFindResult(Self, browser_, identifier, count, selectionRect,
    activeMatchOridinal, finalUpdate);
end;

procedure TCustomChromium.doOnTakeFocus(const Browser_: ICefBrowser;
  next_: Boolean);
begin
  If Assigned(fOnTakeFocus) then fOnTakeFocus(Self, Browser_, next_);
end;

function TCustomChromium.doOnSetFocus(const Browser_: ICefBrowser;
  Source: TCefFocusSource): Boolean;
begin
  Result := False;
  If Assigned(fOnSetFocus) then fOnSetFocus(Self, Browser_, Source, Result);
end;

procedure TCustomChromium.doOnGotFocus(const Browser_: ICefBrowser);
begin
  // Make Chromium the active control
  GetParentForm(Self).ActiveControl := Self;

  If Assigned(fOnGotFocus) then fOnGotFocus(Self, Browser_)
end;

function TCustomChromium.doOnRequestGeolocationPermission(
  const browser_: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer; const callback: ICefGeolocationCallback): Boolean;
begin
  If Assigned(fOnRequestGeolocationPermission) then
    fOnRequestGeolocationPermission(Self, Browser_, requestingUrl, requestId, callback, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnCancelGeolocationPermission(
  const Browser_: ICefBrowser; requestId: Integer);
begin
  If Assigned(fOnCancelGeolocationPermission) then
    fOnCancelGeolocationPermission(Self, Browser_, requestId);
end;

function TCustomChromium.doOnJsdialog(const Browser_: ICefBrowser;
  const originUrl: ustring; dialogType: TCefJsDialogType; const messageText,
  defaultPromptText: ustring; callback: ICefJsDialogCallback; out
  suppressMessage: Boolean): Boolean;
begin
  Result := False;
  suppressMessage := False;
  If Assigned(fOnJsdialog) then
    fOnJsdialog(Self, Browser_, originUrl, dialogType, messageText, defaultPromptText, callback,
      suppressMessage, Result);
end;

function TCustomChromium.doOnBeforeUnloadDialog(const Browser_: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
begin
  If Assigned(fOnBeforeUnloadDialog) then
    fOnBeforeUnloadDialog(Self, Browser_, messageText, isReload, callback, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnResetDialogState(const Browser_: ICefBrowser);
begin
  If Assigned(fOnResetDialogState) then fOnResetDialogState(Self, Browser_);
end;

procedure TCustomChromium.doOnDialogClosed(const browser_: ICefBrowser);
begin
  If Assigned(fOnDialogClosed) then fOnDialogClosed(Self, browser_);
end;

function TCustomChromium.doOnPreKeyEvent(const Browser_: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; out
  isKeyboardShortcut: Boolean): Boolean;
begin
  If Assigned(fOnPreKeyEvent) then
    fOnPreKeyEvent(Self, Browser_, event, osEvent, isKeyboardShortcut, Result)
  Else Result := False;
end;

function TCustomChromium.doOnKeyEvent(const Browser_: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  If Assigned(fOnKeyEvent) then fOnKeyEvent(Self, Browser_, event, osEvent, Result)
  Else Result := False;
end;

function TCustomChromium.doOnBeforePopup(const browser_: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean): Boolean;
begin
  If Assigned(fOnBeforePopup) then
    fOnBeforePopup(Self, browser_, frame, targetUrl, targetFrameName, targetDisposition, userGesture,
      popupFeatures, windowInfo, client, settings, noJavascriptAccess, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnAfterCreated(const Browser_: ICefBrowser);
begin
  If Assigned(fOnAfterCreated) then fOnAfterCreated(Self, Browser_);
end;

procedure TCustomChromium.doOnBeforeClose(const Browser_: ICefBrowser);
begin
  If Assigned(fOnBeforeClose) then fOnBeforeClose(Self, Browser_);
end;

function TCustomChromium.doOnClose(const Browser_: ICefBrowser): Boolean;
begin
  If Assigned(fOnClose) then fOnClose(Self, Browser_, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnLoadingStateChange(const Browser_: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin
  If Assigned(fOnLoadingStateChange) then
    fOnLoadingStateChange(Self, Browser_, isLoading, canGoBack, canGoForward);
end;

procedure TCustomChromium.doOnLoadStart(const Browser_: ICefBrowser; const Frame: ICefFrame;
  transitionType: TCefTransitionType);
begin
  If Assigned(fOnLoadStart) then fOnLoadStart(Self, Browser_, Frame, transitionType);
end;

procedure TCustomChromium.doOnLoadEnd(const Browser_: ICefBrowser; const Frame: ICefFrame;
    httpStatusCode: Integer);
begin
  If Assigned(fOnLoadEnd) then fOnLoadEnd(Self, Browser_, Frame, httpStatusCode);
end;

procedure TCustomChromium.doOnLoadError(const Browser_: ICefBrowser; const Frame: ICefFrame;
  errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
begin
  If Assigned(fOnLoadError) then fOnLoadError(Self, Browser_, Frame, errorCode, errorText, failedUrl);
end;

function TCustomChromium.doOnGetRootScreenRect(const Browser_: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnGetViewRect(const Browser_: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnGetScreenPoint(const Browser_: ICefBrowser; viewX,
  viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnGetScreenInfo(const browser_: ICefBrowser;
  var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;
end;

procedure TCustomChromium.doOnPopupShow(const Browser_: ICefBrowser;
  doshow: Boolean);
begin
  { empty }
end;

procedure TCustomChromium.doOnPopupSize(const Browser_: ICefBrowser;
  const rect: PCefRect);
begin
  { empty }
end;

procedure TCustomChromium.doOnPaint(const Browser_: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: TSize;
  const dirtyRects: TCefRectArray; const buffer: Pointer; awidth,
  aheight: Integer);
begin
  { empty }
end;

procedure TCustomChromium.doOnCursorChange(const browser_: ICefBrowser;
  aCursor: TCefCursorHandle; type_: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  { empty }
end;

function TCustomChromium.doOnStartDragging(const browser_: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperationsMask; x,
  y: Integer): Boolean;
begin
  Result := False;
end;

procedure TCustomChromium.doOnUpdateDragCursor(const browser_: ICefBrowser;
  operation: TCefDragOperationsMask);
begin
  { empty }
end;

procedure TCustomChromium.doOnScrollOffsetChanged(const browser_: ICefBrowser;
  x, y: Double);
begin
  { empty }
end;

procedure TCustomChromium.doOnImeCompositionRangeChanged(
  const browser_: ICefBrowser; const selectedRange: TCefRange;
  characterBoundsCount: TSize; characterBounds: TCefRectArray);
begin
  { empty }
end;

function TCustomChromium.doOnBeforeBrowse(const browser_: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean
  ): Boolean;
begin
  If Assigned(fOnBeforeBrowse) then
    fOnBeforeBrowse(Self, browser_, frame, request, isRedirect, Result)
  Else Result := False;
end;

function TCustomChromium.doOnOpenUrlFromTab(const browser_: ICefBrowser;
  const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; useGesture: Boolean): Boolean;
begin
  If Assigned(fOnOpenUrlFromTab) then
    fOnOpenUrlFromTab(Self, browser_, frame, targetUrl, targetDisposition, useGesture, Result)
  Else Result := False;
end;

function TCustomChromium.doOnBeforeResourceLoad(const browser_: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefRequestCallback): TCefReturnValue;
begin
  If Assigned(fOnBeforeResourceLoad) then
    fOnBeforeResourceLoad(Self, browser_, frame, request, callback, Result)
  Else Result := RV_CONTINUE;
end;

function TCustomChromium.doOnGetResourceHandler(const Browser_: ICefBrowser;
  const Frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  If Assigned(fOnGetResourceHandler) then
    fOnGetResourceHandler(Self, Browser_, Frame, request, Result)
  Else Result := nil;
end;

procedure TCustomChromium.doOnResourceRedirect(const browser_: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse; var newUrl: ustring);
begin
  If Assigned(fOnResourceRedirect) then
    fOnResourceRedirect(Self, browser_, frame, request, response, newUrl);
end;

function TCustomChromium.doOnResourceResponse(const browser_: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse): Boolean;
begin
  If Assigned(fOnResourceResponse) then
    fOnResourceResponse(Self, browser_, frame, request, response, Result)
  Else Result := False;
end;

function TCustomChromium.doOnGetResourceResponseFilter(
  const browser_: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
begin
  If Assigned(fOnGetResourceResponseFilter) then
    fOnGetResourceResponseFilter(Self, browser_, frame, request, response, Result)
  Else Result := nil;
end;

procedure TCustomChromium.doOnResourceLoadComplete(const browser_: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse; status: TCefUrlRequestStatus;
  receivedContentLength: Int64);
begin
  If Assigned(fOnResourceLoadComplete) then
    fOnResourceLoadComplete(Self, browser_, frame, request, response, status, receivedContentLength);
end;

function TCustomChromium.doOnGetAuthCredentials(const Browser_: ICefBrowser;
  const Frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  If Assigned(fOnGetAuthCredentials) then
    fOnGetAuthCredentials(Self, Browser_, Frame, isProxy, host, port, realm, scheme, callback, Result)
  Else Result := False;
end;

function TCustomChromium.doOnQuotaRequest(const Browser_: ICefBrowser;
  const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback
  ): Boolean;
begin
  If Assigned(fOnQuotaRequest) then
    fOnQuotaRequest(Self, Browser_, originUrl, newSize, callback, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnProtocolExecution(const Browser_: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin
  If Assigned(fOnProtocolExecution) then fOnProtocolExecution(Self, Browser_, url, allowOsExecution)
  Else allowOsExecution := True;
end;

function TCustomChromium.doOnCertificateError(const browser_: ICefBrowser;
  certError: TCefErrorCode; const requestUrl: ustring;
  const sslInfo: ICefSslinfo; callback: ICefRequestCallback): Boolean;
begin
  If Assigned(fOnCertificateError) then
    fOnCertificateError(Self, browser_, certError, requestUrl, sslInfo, callback, Result)
  Else Result := False;
end;

function TCustomChromium.doOnSelectClientCertificate(
  const browser_: ICefBrowser; isProxy: Boolean; const host: ustring;
  port: Integer; certificatesCount: TSize;
  certificates: ICefX509certificateArray;
  callback: ICefSelectClientCertificateCallback): Boolean;
begin
  If Assigned(fOnSelectClientCertificate) then
    fOnSelectClientCertificate(Self, browser_, isProxy, host, port, certificatesCount, certificates,
      callback, Result)
  Else Result := False;
end;

procedure TCustomChromium.doOnPluginCrashed(const Browser_: ICefBrowser;
  const pluginPath: ustring);
begin
  If Assigned(fOnPluginCrashed) then fOnPluginCrashed(Self, Browser_, pluginPath);
end;

procedure TCustomChromium.doOnRenderViewReady(const browser_: ICefBrowser);
begin
  If Assigned(fOnRenderViewReady) then fOnRenderViewReady(Self, browser_);
end;

procedure TCustomChromium.doOnRenderProcessTerminated(
  const Browser_: ICefBrowser; Status: TCefTerminationStatus);
begin
  If Assigned(fOnRenderProcessTerminated) then fOnRenderProcessTerminated(Self, Browser_, Status);
end;


Initialization
  {$IFDEF LCLCOCOA}
    InitCRApplication;
  {$ENDIF}

end.

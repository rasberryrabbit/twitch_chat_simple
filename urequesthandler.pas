unit uRequestHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCEFClient, uCEFInterfaces, uCEFResourceRequestHandler,
  uCEFTypes;

type


  { TMyClient }

  TMyClient=class(TCustomClientHandler)
    protected
      procedure GetRequestHandler(var aHandler : ICefRequestHandler); override;
  end;

  { TMyRequestHandler }

  TMyRequestHandler = class(TCefResourceRequestHandlerOwn)
    protected
      function OnBeforeResourceLoad(const browser: ICefBrowser;
        const frame: ICefFrame; const request: ICefRequest;
        const callback: ICefCallback): TCefReturnValue; override;
  end;

implementation

{ TMyClient }

procedure TMyClient.GetRequestHandler(var aHandler: ICefRequestHandler);
begin
  inherited GetRequestHandler(aHandler);
end;

{ TMyRequestHandler }

function TMyRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefCallback): TCefReturnValue;
begin
  Result:=inherited OnBeforeResourceLoad(browser, frame, request, callback);
end;

end.


{::APDATA:: David - 11-2016 :: Conversão para Unicode/64bits ::}

{ unit sdDebug

  universal method for debugging

  Exceptions often are a hindrance, so instead use these classes
  to give important info to the application or user with these
  three basic classes

  Besides debug methods, this unit also defines a few compatibility  types:
  The include file simdesign.inc defines $D5UP and after the
  uses-clause these types for D5 are defined. This way, many simdesign
  projects are compatible with Delphi 5.
  fpc: if lazarus + freepascal is defined, Utf8String just reverts to "string".

  Author: Nils Haeck M.Sc.
  Original Date: 08nov2010
  copyright (c) SimDesign BV (www.simdesign.nl)
}
unit sdDebug;

{$i simdesign.inc}

interface

uses
  Classes;



// Delphi unicode compatibility
{$ifndef UNICODE}
type
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$endif UNICODE}

type
  TsdWarnStyle = (wsInfo, wsHint, wsWarn, wsFail);

const
  cWarnStyleNames: array[TsdWarnStyle] of String = ('info', 'hint', 'warn', 'fail');

type
  // event with debug data
  TsdDebugEvent = procedure(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String) of object;

  // simple update event
  TsdUpdateEvent = procedure(Sender: TObject) of object;

  TsdDebugComponent = class(TComponent)
  protected
    FOnDebugOut: TsdDebugEvent;
  public
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String); virtual;
    // Connect to OnDebugOut to get debug information in the client application
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

  TsdDebugObject = class(TObject)
  protected
    FOnDebugOut: TsdDebugEvent;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String); virtual;
  public
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

  TsdDebugPersistent = class(TPersistent)
  protected
    FOwner: TsdDebugComponent;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String); virtual;
  public
    constructor CreateDebug(AOwner: TsdDebugComponent); virtual;
  end;

{ Functions }

function sdDebugMessageToString(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String): String;

function sdClassName(AObject: TObject): String;

implementation

{ TsdDebugComponent }

procedure TsdDebugComponent.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String);
var
  AOwner: TComponent;
begin
  AOwner := Self;
  while AOwner is TsdDebugComponent do
  begin
    if assigned(TsdDebugComponent(AOwner).FOnDebugOut) then
    begin
      TsdDebugComponent(AOwner).FOnDebugOut(Sender, WarnStyle, AMessage);
      exit;
    end;
    AOwner := AOwner.Owner;
  end;
end;

{ TsdDebugObject }

procedure TsdDebugObject.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String);
begin
  if assigned(FOnDebugOut) then
    FOnDebugOut(Sender, WarnStyle, AMessage);
end;

{ TsdDebugPersistent }

constructor TsdDebugPersistent.CreateDebug(AOwner: TsdDebugComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TsdDebugPersistent.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String);
begin
  if FOwner is TsdDebugComponent then
    TsdDebugComponent(FOwner).DoDebugOut(Sender, WarnStyle, AMessage);
end;

{ Functions }

function sdDebugMessageToString(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: String): String;
var
  SenderString: String;
begin
  if assigned(Sender)  then
    SenderString := Sender.ClassName
  else
    SenderString := '';
  Result := '[' + cWarnStyleNames[WarnStyle] + '] ' + SenderString + ': ' + AMessage;
end;

function sdClassName(AObject: TObject): String;
begin
  Result := 'nil';
  if assigned(AObject) then
    Result := AObject.ClassName;
end;

end.





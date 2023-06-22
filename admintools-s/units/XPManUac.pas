{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
{           Copyright (c) 1995-2008 CodeGear            }
{                                                       }
{*******************************************************}

unit XPManUac;

{$WEAKPACKAGEUNIT ON}

interface

uses
{$IF DEFINED(CLR)}
//  System.ComponentModel.Design.Serialization,
{$IFEND}
  SysUtils, Classes;

type
  [RootDesignerSerializerAttribute('', '', False)]
  TXPManifest = class(TComponent)
  end;

{$IF DEFINED(CLR)}
{$R WindowsXP.res}
{$ELSE}
{$R XpManUac.res}
//{$R WindowsXP.res}
{$IFEND}


implementation

end.

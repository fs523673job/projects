unit utwoferTests;

interface

uses
  StrUtils,
  

function twoFer(const AName: String = ''): String;

implementation

function twoFer(const AName: String = ''): String;
begin
  Result := Format('One for %s, one for me.', [IfThen(AName = '', 'you', AName]));
end;

end;
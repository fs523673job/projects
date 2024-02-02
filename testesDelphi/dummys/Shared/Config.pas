unit Config;

interface

uses
  AutomaticIni
  ;

type
  TConfigLdap = class(TAutomaticIni)
  private
    FLDAPUser    : String;
    FLDAPPass    : String;
    FTestDomanin : String;
    FTestUser    : String;
    FTestPass    : String;
  published
    property LDAPUser    : String read FLDAPUser    write FLDAPUser   ;
    property LDAPPass    : String read FLDAPPass    write FLDAPPass   ;
    property TestDomanin : String read FTestDomanin write FTestDomanin;
    property TestUser    : String read FTestUser    write FTestUser   ;
    property TestPass    : String read FTestPass    write FTestPass   ;

  end;


implementation



end.

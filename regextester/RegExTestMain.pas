unit RegExTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  RegularExpressions;

type
  TMainForm = class(TForm)
    txtRegEx: TMemo;
    txtSample: TMemo;
    tvResults: TTreeView;
    cmdTest: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkIgnoreCase: TCheckBox;
    chkMultiLine: TCheckBox;
    chkSingleLine: TCheckBox;
    chkExplicitCapture: TCheckBox;
    chkCompiled: TCheckBox;
    chkIgnorePatternSpace: TCheckBox;
    procedure cmdTestClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure AddMatchToTree(match : TMatch);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


procedure TMainForm.AddMatchToTree(match: TMatch);
var
  matchNode : TTreeNode;
  group : TGroup;
  groupNode : TTreeNode;
  i : integer;
begin
  matchNode := tvResults.Items.AddChild(nil,'Match[' + match.Value + ']');

  if match.Groups.Count > 1 then
  begin
    //group 0 is the entire match, so the first real group is 1
    for i := 1 to match.Groups.Count -1 do
    begin
      group := match.Groups.Item[i];
      groupNode := tvResults.Items.AddChild(matchNode,'Group[' + IntToStr(i) + '] [' + group.Value + ']');
    end;
  end;
  match := match.NextMatch;
  if match.Success then
    AddMatchToTree(match);
end;

procedure TMainForm.cmdTestClick(Sender: TObject);
var
  regexpr : TRegEx;
  options : TRegExOptions;
  match : TMatch;
begin
  tvResults.Items.Clear;

  if chkIgnoreCase.Checked then
    Include(options,TRegExOption.roIgnoreCase);
  if chkMultiLine.Checked then
    Include(options,TRegExOption.roMultiLine);
  if chkSingleLine.Checked then
    Include(options,TRegExOption.roSingleLine);
  if chkExplicitCapture.Checked then
    Include(options,TRegExOption.roIgnoreCase);
  if chkCompiled.Checked then
    Include(options,TRegExOption.roCompiled);
  if chkIgnorePatternSpace.Checked then
    Include(options,TRegExOption.roIgnoreCase);

  try
    regexpr := TRegEx.Create(txtRegEx.Text,options);

    match := regexpr.Match(txtSample.Text);
    if match.Success then
      AddMatchToTree(match)
    else
      ShowMessage('No match found');
  except
    on e : Exception do
    begin
        ShowMessage(e.Message);
    end;
  end;
end;

end.

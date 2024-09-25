unit unSanitizeHTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  System.NetEncoding
  ;


type
  TPreventXSS = class
  public
    class function SanitizeHTML(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeHTML(const AContentHTML: String): String; overload;
  end;


implementation

{ TPreventXSS }

class function TPreventXSS.SanitizeHTML(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := SanitizeHTML(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeHTML(const AContentHTML: String): String;
const
  AllowedTags: array[0..8] of string = ('b', 'i', 'u', 'p', 'br', 'img', 'div', 'font', 'span');
  AllowedAttributes: array[0..3] of string = ('href', 'src', 'style', 'face');
var
  TagRegex, AttrRegex: TRegEx;
  Matches: TMatchCollection;
  TagMatch, AttrMatch: TMatch;
  Output, TagName, TagContent: string;
  IsEndTag: Boolean;
  i, LastIndex: Integer;
  AllowedTagList, AllowedAttrList: TDictionary<string, Boolean>;
  AttrName, AttrValue: String;
  SanitizedOutput: TStringBuilder;
  IsSafeTag: Boolean;
begin
  Output := TNetEncoding.HTML.Decode(AContentHTML);

  AllowedTagList := TDictionary<string, Boolean>.Create;
  AllowedAttrList := TDictionary<string, Boolean>.Create;
  SanitizedOutput := TStringBuilder.Create;
  try
    for i := Low(AllowedTags) to High(AllowedTags) do
      AllowedTagList.Add(AllowedTags[i], True);

    for i := Low(AllowedAttributes) to High(AllowedAttributes) do
      AllowedAttrList.Add(AllowedAttributes[i], True);

    TagRegex := TRegEx.Create('<[^>]*>', [roIgnoreCase]);
    Matches := TagRegex.Matches(Output);

    LastIndex := 1;
    for TagMatch in Matches do
    begin
      SanitizedOutput.Append(Copy(Output, LastIndex, TagMatch.Index - LastIndex));

      TagContent := TagMatch.Value;

      if TRegEx.IsMatch(TagContent, '^<\/\s*([a-z0-9]+)', [roIgnoreCase]) then
      begin
        IsEndTag := True;
        TagName := TRegEx.Match(TagContent, '^<\/\s*([a-z0-9]+)', [roIgnoreCase]).Groups[1].Value;
      end
      else if TRegEx.IsMatch(TagContent, '^<\s*([a-z0-9]+)', [roIgnoreCase]) then
      begin
        IsEndTag := False;
        TagName := TRegEx.Match(TagContent, '^<\s*([a-z0-9]+)', [roIgnoreCase]).Groups[1].Value;
      end
      else
        Continue;

      TagName := LowerCase(TagName);

      if AllowedTagList.ContainsKey(TagName) then
      begin
        IsSafeTag := True;
        if not IsEndTag then
        begin
          AttrRegex := TRegEx.Create('([a-z0-9\-]+)\s*=\s*["'']([^"'']*)["'']', [roIgnoreCase]);
          TagContent := '<' + TagName;
          for AttrMatch in AttrRegex.Matches(TagMatch.Value) do
          begin
            AttrName := LowerCase(AttrMatch.Groups[1].Value);
            AttrValue := AttrMatch.Groups[2].Value;

            if AllowedAttrList.ContainsKey(AttrName) then
            begin
              if (AttrName = 'href') or (AttrName = 'src') then
              begin
                if not AttrValue.Contains(':') then
                  TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue])
                else
                  IsSafeTag := False;
              end
              else
                TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
            end
            else
              IsSafeTag := False;
          end;
          TagContent := TagContent + '>';

          if IsSafeTag then
            SanitizedOutput.Append(TagContent);
        end
        else
        begin
          SanitizedOutput.Append('</' + TagName + '>');
        end;
      end
      else
      begin

      end;

      LastIndex := TagMatch.Index + TagMatch.Length;
    end;

    if LastIndex <= Length(Output) then
      SanitizedOutput.Append(Copy(Output, LastIndex, Length(Output) - LastIndex + 1));

    Result := SanitizedOutput.ToString;
  finally
    AllowedTagList.Free;
    AllowedAttrList.Free;
    SanitizedOutput.Free;
  end;
end;

end.

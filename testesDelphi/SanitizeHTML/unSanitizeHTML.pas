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
    class function NeedsDecoding(const AContentHTML: String): Boolean; static;
    class function SanitizeHTML(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeHTML(const AContentHTML: String): String; overload;
    class function SanitizeHTMLContentTag(const AContentHTML: String): String; overload;
    class function IsProbablyHTML(const AContent: String): Boolean; static;
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

class function TPreventXSS.IsProbablyHTML(const AContent: String): Boolean;
begin
  Exit(TRegEx.IsMatch(AContent, '<\/?[a-z][\s\S]*>', [roIgnoreCase]));
end;

class function TPreventXSS.NeedsDecoding(const AContentHTML: String): Boolean;
var
  DecodedContent: String;
begin
  DecodedContent := TNetEncoding.HTML.Decode(AContentHTML);
  Exit(not AContentHTML.Equals(DecodedContent));
end;

class function TPreventXSS.SanitizeHTML(const AContentHTML: String): String;
const
  AllowedTags: array[0..10] of string = ('b', 'i', 'u', 'p', 'br', 'img', 'div', 'font', 'span', 'a', 'strong');
  AllowedAttributes: array[0..4] of string = ('href', 'src', 'style', 'face', 'class');
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
  IsNeedEncoding: Boolean;
begin
  Output := AContentHTML;

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
          AttrRegex := TRegEx.Create('([a-z0-9\-]+)\s*=\s*(?:"([^"]*)"|''([^'']*)''|([^\s>]+))', [roIgnoreCase]);
          TagContent := '<' + TagName;
          for AttrMatch in AttrRegex.Matches(TagMatch.Value) do
          begin
            AttrName := LowerCase(AttrMatch.Groups[1].Value);

            if AttrMatch.Groups[2].Success then
              AttrValue := AttrMatch.Groups[2].Value
            else if AttrMatch.Groups[3].Success then
              AttrValue := AttrMatch.Groups[3].Value
            else if AttrMatch.Groups[4].Success then
              AttrValue := AttrMatch.Groups[4].Value
            else
              AttrValue := '';

            if AllowedAttrList.ContainsKey(AttrName) then
            begin
              if (AttrName = 'href') or (AttrName = 'src') then
              begin
                if (TRegEx.IsMatch(AttrValue, '(^http|https|mailto):', [roIgnoreCase])) then
                  TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
              end
              else
                TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
            end;
          end;
          TagContent := TagContent + '>';

          if IsSafeTag then
            SanitizedOutput.Append(TagContent);
        end
        else
        begin
          SanitizedOutput.Append('</' + TagName + '>');
        end;
      end;

      LastIndex := TagMatch.Index + TagMatch.Length;
    end;

    if LastIndex <= Length(Output) then
      SanitizedOutput.Append(Copy(Output, LastIndex, Length(Output) - LastIndex + 1));

    Exit(SanitizedOutput.ToString);
  finally
    AllowedTagList.Free;
    AllowedAttrList.Free;
    SanitizedOutput.Free;
  end;
end;

class function TPreventXSS.SanitizeHTMLContentTag(const AContentHTML: String): String;
var
  Input: String;
  Output: TStringBuilder;
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  ProcessedContent: String;
  IsNeedEncoding: Boolean;
  c: Integer;
  Group: TGroup;
  str: String;
begin
  Input := AContentHTML;
  Output := TStringBuilder.Create;
  try
    Regex := TRegEx.Create('(<[^>]*>)|([^<>]+)', [roIgnoreCase]);
    Matches := Regex.Matches(Input);
    for Match in Matches do
    begin
      if (Match.Groups.Count - 1 = 1) and (Match.Groups[1].Success) then
        Output.Append(Match.Groups[1].Value);

      if (Match.Groups.Count > 2) and (Match.Groups[2].Success) then
      begin
        ProcessedContent := Match.Groups[2].Value;

        if ProcessedContent.Trim <> '' then
        begin
          IsNeedEncoding := TPreventXSS.NeedsDecoding(ProcessedContent);

          if IsNeedEncoding then
            ProcessedContent := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(ProcessedContent));

          ProcessedContent := TPreventXSS.SanitizeHTML(ProcessedContent);

          if IsNeedEncoding then
            ProcessedContent := TNetEncoding.HTML.Encode(ProcessedContent);
        end;
        Output.Append(ProcessedContent);
      end;
    end;
    Result := Output.ToString;
  finally
    Output.Free;
  end;
end;


end.

unit unSanitizeHTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  System.NetEncoding,
  System.WideStrUtils,
  System.Character
  ;


type
  TPreventXSS = class
  const
    FAKETAGBEGIN = '<faketag ';
    FAKETAGEND = ' faketagend>';
  private
    class function MatchEvaluator(const Match: TMatch): String;
    class function NormalizeHTML(const AContent: String): String;
    class function BasicDecode(const AContent: String; out ANeedEncoding: Boolean): String; static;
    class function BasicEncode(const AContent: String): String; static;
    class function PatternBlackList: TArray<String>;
    class function PatternDecode: TDictionary<String, String>;
  public
    class function IsProbablyHTML(const AContent: String): Boolean; overload; static;
    class function IsProbablyHTML(const AContentBytes: TBytes): Boolean; overload; static;
    class function NeedsDecoding(const AContentHTML: String): Boolean; static;
    class function SanitizeHTML(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeHTML(const AContentHTML: String): String; overload;
    class function SanitizeTag(const AContentHTML: String): String; overload;
    class function SanitizeTag(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeAll(const AContentHTML: String): String; overload;
    class function SanitizeAll(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeForceAll(const AContentHTML: String): String; overload;
    class function SanitizeForceAll(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeTexto(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeTexto(const AContent: String): String; overload;
    class function SanitizeSimpleLink(const ALinkText: String): String; static;
    class function SanitizeBlackList(const AContent: String; const APatterns: TArray<String>): String; overload; static;
    class function SanitizeBlackList(const AContent: String): String; overload;
    class function PossiblyHasXSS(const AContent: String): Boolean; static;
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

class function TPreventXSS.BasicDecode(const AContent: String; out ANeedEncoding: Boolean): String;
var
  DecodePatterns: TDictionary<String, String>;
  ContentDecoded: String;
begin
  DecodePatterns := PatternDecode;
  try
    ContentDecoded := AContent;
    for var pair in DecodePatterns do
      ContentDecoded := ContentDecoded.Replace(pair.Key, pair.Value, [rfReplaceAll]);
    ANeedEncoding := not ContentDecoded.Equals(AContent);
    Exit(ContentDecoded);
  finally
    if Assigned(DecodePatterns) then
      DecodePatterns.Free;
  end;
end;

class function TPreventXSS.BasicEncode(const AContent: String): String;
var
  DecodePatterns: TDictionary<String, String>;
  ContentEncode: String;
begin
  DecodePatterns := PatternDecode;
  try
    ContentEncode := AContent;
    for var pair in DecodePatterns do
      ContentEncode := ContentEncode.Replace(pair.Value, pair.Key, [rfReplaceAll]);
    Exit(ContentEncode);
  finally
    if Assigned(DecodePatterns) then
      DecodePatterns.Free;
  end;
end;

class function TPreventXSS.IsProbablyHTML(const AContent: String): Boolean;
var
  LocalContent: String;
  dummyEncode: Boolean;
begin
  LocalContent := TNetEncoding.HTML.Decode(TPreventXSS.BasicDecode(AContent, dummyEncode));
  Exit(TRegEx.IsMatch(LocalContent, '<\/?[a-z][\s\S]*>|<\/?[a-z][\s\S]*|\/?[a-z][\s\S]*>', [roIgnoreCase]));
end;

class function TPreventXSS.IsProbablyHTML(const AContentBytes: TBytes): Boolean;
var
  Encoding: TEncoding;
  ContentString: String;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  Exit(TPreventXSS.IsProbablyHTML(ContentString));
end;

class function TPreventXSS.MatchEvaluator(const Match: TMatch): String;
var
  IsHex: Boolean;
  CharCode: Integer;
begin
  IsHex := Match.Groups[1].Value = 'x';
  if IsHex then
    CharCode := StrToIntDef('$' + Match.Groups[2].Value, -1)
  else
    CharCode := StrToIntDef(Match.Groups[2].Value, -1);

  if (CharCode >= 0) and (CharCode <= $10FFFF) then
    Result := Char.ConvertFromUtf32(CharCode)
  else
    Result := Match.Value;
end;

class function TPreventXSS.NeedsDecoding(const AContentHTML: String): Boolean;
var
  DecodedContent: String;
begin
  DecodedContent := TNetEncoding.HTML.Decode(AContentHTML);
  Exit(not AContentHTML.Equals(DecodedContent));
end;

class function TPreventXSS.NormalizeHTML(const AContent: String): String;
var
  Normalized: String;
  RegEx: TRegEx;
begin
  Normalized := AContent;
  if Normalized.StartsWith(#$EF#$BB#$BF) then
    Delete(Normalized, 1, 3);
  Normalized := Normalized.Replace('＜', '<', [rfReplaceAll]);
  Normalized := Normalized.Replace('＞', '>', [rfReplaceAll]);
  RegEx := TRegEx.Create('<{2,}', [roIgnoreCase]);
  Normalized := RegEx.Replace(Normalized, '<');
  RegEx := TRegEx.Create('>{2,}', [roIgnoreCase]);
  Normalized := RegEx.Replace(Normalized, '>');
  RegEx := TRegEx.Create('<(?!/?[a-zA-Z0-9])', [roIgnoreCase]);
  Normalized := RegEx.Replace(Normalized, '');
  Exit(Normalized);
end;

class function TPreventXSS.PatternBlackList: TArray<String>;
begin
 Result := [
    '<script.*?>.*?</script>',
    '<iframe.*?>.*?</iframe>',
    'onerror\s*=',
    'onclick\s*=',
    'alert\s*\(',
    'onload\s*=',
    'onmouseover\s*=',
    'onfocus\s*=',
    'onblur\s*=',
    'onchange\s*=',
    'oninput\s*=',
    'onfilterchange\s*=',
    'eval\s*\(',
    'setTimeout\s*\(',
    'setInterval\s*\(',
    'document\.write',
    'autofocus',
    '.innerHTML',
    '.outerHTML',
    'javascript:',
    'xmlhttprequest',
    'localStorage',
    'sessionStorage'
  ];
end;

class function TPreventXSS.PatternDecode: TDictionary<String, String>;
begin
  Result := TDictionary<String, String>.Create;
  Result.Add('&lt;',   '#lt;');
  Result.Add('&gt;',   '#gt;');
  Result.Add('&quot;', '#quot;');
  Result.Add('&#39;',  '##39;');
  Result.Add('&amp;',  '#amp;');
  Result.Add('&nbsp;', '#nbsp;');
end;

class function TPreventXSS.SanitizeHTML(const AContentHTML: String): String;
const
  SPECIAL_APDATA_BEGIN = '/*APDATABEGIN*/';
  SPECIAL_APDATA_END = '/*APDATAEND*/';
  AllowedTags: array[0..16] of string = ('b', 'i', 'u', 'p', 'br', 'img', 'div', 'font', 'span', 'a', 'strong', 'h1', 'h2', 'h3', 'h4', 'h5', 'faketag');
  AllowedAttributes: array[0..5] of string = ('href', 'src', 'style', 'face', 'class', 'hrefbegin');
var
  TagRegex, AttrRegex: TRegEx;
  Matches: TMatchCollection;
  TagMatch, AttrMatch: TMatch;
  Output, TagName, TagContent: string;
  IsEndTag: Boolean;
  i, LastIndex: Integer;
  AllowedTagList, AllowedAttrList: TDictionary<string, Boolean>;
  AttrName, AttrValue, AttrValueApdata: String;
  SanitizedOutput: TStringBuilder;
  IsSafeTag: Boolean;

  function GetValueFromTagApdata(const AAttrValueApdata: String): String;
  var
    PosBegin, PosEnd: Integer;
  begin
    PosBegin := Pos(SPECIAL_APDATA_BEGIN, AAttrValueApdata);
    PosEnd := Pos(SPECIAL_APDATA_END, AAttrValueApdata);
    if (PosBegin > 0) and (PosEnd > 0) and (PosEnd > PosBegin) then
      Exit(TNetEncoding.HTML.Decode(Copy(AAttrValueApdata, PosBegin + Length(SPECIAL_APDATA_BEGIN), PosEnd - PosBegin - Length(SPECIAL_APDATA_BEGIN))).Replace('''', ''))
    else
      Exit(EmptyStr);
  end;

begin
  Output := NormalizeHTML(AContentHTML);
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
                if (TRegEx.IsMatch(AttrValue, '^(?:https?:\/\/|mailto:|www\.)?[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)+(?::\d+)?(?:\/.*)?$', [roIgnoreCase])) then
                begin
                  TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
                end
                else if (Pos(SPECIAL_APDATA_BEGIN, AttrValue) > 0) and (Pos(SPECIAL_APDATA_END, AttrValue) > 0) then
                begin
                  AttrValueApdata := GetValueFromTagApdata(AttrValue);
                  if (TRegEx.IsMatch(AttrValueApdata, '^(?:https?:\/\/|mailto:|www\.)?[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)+(?::\d+)?(?:\/.*)?$', [roIgnoreCase])) then
                    TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue.Replace(SPECIAL_APDATA_BEGIN, '').Replace(SPECIAL_APDATA_END, '')]);
                end;
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

class function TPreventXSS.SanitizeTexto(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeTexto(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeTag(const AContentHTML: String): String;
const
  NotNeedEncoded: array[0..0] of string = ('&nbsp;');
var
  Input: String;
  Output: TStringBuilder;
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  ProcessedContent: String;
  IsNeedEncoding: Boolean;
  c: Integer;
  NotNeedEncodedList: TDictionary<string, Boolean>;
  dummyEncode: Boolean;
begin
  Input := AContentHTML;
  Output := TStringBuilder.Create;
  try
    NotNeedEncodedList := TDictionary<string, Boolean>.Create;
    try
      for c := Low(NotNeedEncoded) to High(NotNeedEncoded) do
        NotNeedEncodedList.Add(NotNeedEncoded[c], True);

      Regex := TRegEx.Create('(<[^>]*>)|([^<>]+)', [roIgnoreCase]);
      Matches := Regex.Matches(Input);
      for Match in Matches do
      begin
        if (Match.Groups.Count - 1 = 1) and (Match.Groups[1].Success) then
          Output.Append(Match.Groups[1].Value);

        if (Match.Groups.Count > 2) and (Match.Groups[2].Success) then
        begin
          ProcessedContent := Match.Groups[2].Value;

          if (ProcessedContent.Trim <> '') and (not NotNeedEncodedList.ContainsKey(ProcessedContent)) then
          begin
            ProcessedContent :=  TPreventXSS.BasicDecode(ProcessedContent, dummyEncode);
            IsNeedEncoding := TPreventXSS.NeedsDecoding(ProcessedContent);

            if IsNeedEncoding then
              ProcessedContent := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(ProcessedContent));

            ProcessedContent := TPreventXSS.SanitizeHTML(ProcessedContent);

            if IsNeedEncoding then
              ProcessedContent := TNetEncoding.HTML.Encode(ProcessedContent);

            if dummyEncode then
              ProcessedContent :=  TPreventXSS.BasicEncode(ProcessedContent);
          end;
          Output.Append(ProcessedContent);
        end;
      end;
      Result := Output.ToString;
    finally
      NotNeedEncodedList.Free;
    end;
  finally
    Output.Free;
  end;
end;

class function TPreventXSS.SanitizeTag(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeTag(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeBlackList(const AContent: String): String;
begin
  Exit(TPreventXSS.SanitizeBlackList(AContent, PatternBlackList));
end;

class function TPreventXSS.SanitizeBlackList(const AContent: String; const APatterns: TArray<String>): String;
var
  Pattern: String;
  Regex: TRegEx;
  Sanitized: String;
  needEncoding: Boolean;
begin
  try
    try
      Sanitized := TNetEncoding.URL.Decode(AContent);
    except
      Sanitized := AContent;
    end;
    Sanitized := TPreventXSS.BasicDecode(Sanitized, needEncoding);
    for Pattern in APatterns do
    begin
      Regex := TRegEx.Create(Pattern, [roIgnoreCase, roMultiLine]);
      Sanitized := Regex.Replace(Sanitized, EmptyStr);
    end;
    if needEncoding then
      Exit(TPreventXSS.BasicEncode(Sanitized))
    else
      Exit(Sanitized);
  except
    Exit(AContent);
  end;
end;

class function TPreventXSS.SanitizeAll(const AContentHTML: String): String;
begin
  Exit(TPreventXSS.SanitizeHTML(TPreventXSS.SanitizeTag(AContentHTML)));
end;

class function TPreventXSS.SanitizeAll(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeAll(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeForceAll(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeForceAll(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeForceAll(const AContentHTML: String): String;

  function RemoveFakeTag(const AStringContent: String): String;
  begin
    Result := AStringContent;
    if Result.StartsWith(FAKETAGBEGIN) then
      Result := Result.Substring(FAKETAGBEGIN.Length);
    if Result.EndsWith(FAKETAGEND) then
      Result := Result.Substring(0, Result.Length - FAKETAGEND.Length);
    if Result.StartsWith(Format('%s>', [FAKETAGBEGIN.Trim])) then
      Result := Result.Substring(FAKETAGBEGIN.Trim.Length + 1);
  end;

begin
  if not (TPreventXSS.IsProbablyHTML(AContentHTML)) then
    Exit(RemoveFakeTag(TPreventXSS.SanitizeAll(Format('%s%s%s', [FAKETAGBEGIN, AContentHTML, FAKETAGEND]))))
  else
    Exit(TPreventXSS.SanitizeAll(AContentHTML));
end;

class function TPreventXSS.SanitizeTexto(const AContent: String): String;
const
  HREFTAGBEGIN = 'hrefbegin="';
  HREFTAGEND = '" hrefend';
  ENDTAG = '">';

var
  Normalized: String;
  needEncoding: Boolean;
  sanitizedContent: String;

  function RemoveFakeTag(const AStringContent: String; ACleanHref: Boolean = False): String;
  begin
    Result := AStringContent;
    if Result.StartsWith(FAKETAGBEGIN) then
      Result := Result.Substring(FAKETAGBEGIN.Length);
    if Result.EndsWith(FAKETAGEND) then
      Result := Result.Substring(0, Result.Length - FAKETAGEND.Length);
    if Result.StartsWith(Format('%s>', [FAKETAGBEGIN.Trim])) then
      Result := Result.Substring(FAKETAGBEGIN.Trim.Length + 1);
    if Result.EndsWith(Format('">', [ENDTAG.Trim])) then
      Result := Result.Substring(0, Result.Length - ENDTAG.Length);
    if ACleanHref then
      Result := Result.Replace(HREFTAGBEGIN, '', [rfReplaceAll, rfIgnoreCase]);
  end;

begin
  Normalized := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(TPreventXSS.BasicDecode(TNetEncoding.URL.Decode(AContent), needEncoding)));
  if not (TPreventXSS.IsProbablyHTML(Normalized)) then
  begin
    if (Pos(HREFTAGBEGIN, Normalized.ToLower) = 0) then
      sanitizedContent := RemoveFakeTag(TPreventXSS.SanitizeAll(Format('%s %s%s%s %s', [FAKETAGBEGIN, HREFTAGBEGIN, Normalized, HREFTAGEND, FAKETAGEND])), True)
    else
      sanitizedContent := RemoveFakeTag(TPreventXSS.SanitizeAll(Format('%s%s%s', [FAKETAGBEGIN, Normalized, FAKETAGEND])))
  end
  else
    sanitizedContent := TPreventXSS.SanitizeAll(Normalized);

  if needEncoding then
    Exit(TPreventXSS.BasicEncode(sanitizedContent))
  else
    Exit(sanitizedContent);
end;

class function TPreventXSS.PossiblyHasXSS(const AContent: String): Boolean;
var
  Pattern: String;
  Regex: TRegEx;
  Normalized: String;
  dummyEncode: Boolean;
begin
  if AContent.Trim.IsEmpty then
    Exit(False);

  Normalized := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(TPreventXSS.BasicDecode(TNetEncoding.URL.Decode(AContent), dummyEncode)));

  for Pattern in PatternBlackList do
  begin
    Regex := TRegEx.Create(Pattern, [roIgnoreCase, roSingleLine]);
    if Regex.IsMatch(Normalized) then
      Exit(True);
  end;

  Exit(False);
end;


class function TPreventXSS.SanitizeSimpleLink(const ALinkText: String): String;
begin
  if ALinkText.IsEmpty then
    Exit(EmptyStr)
  else
    Exit(ALinkText.Replace('"', ' ', [rfReplaceall, rfIgnoreCase]).Split([' '])[0]);
end;


end.

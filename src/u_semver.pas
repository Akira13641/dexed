unit u_semver;

{$I u_defines.inc}

interface

uses
  SysUtils, u_stringrange;

type

  // Record used to split the text of a semantic version into components.
  // Special helpers exit for the additional labels "alpha", "beta" or
  // "rc" as used.
  TSemVer = record
  strict private
    fAdditional: string;
    fMajor, fMinor, fPatch: word;
    fValid: boolean;
  public
    // Initializes with the semVer text.
    // When throw is set to true an Exception is  raised if the format is not compliant.
    procedure init(const text: string; throw: boolean = true);

    // Indicates wether the version is not a final release.
    function isPreRelease: boolean;
    // Indicates wether the version is an alpha ("v1.0.0-alpha", "v1.0.0-alpha.1").
    function isAlpha: boolean;
    // Indicates wether the version is a beta ("v1.0.0-beta", "v1.0.0-beta.1").
    function isBeta: boolean;
    // Indicates wether the version is a release candidate ("v1.0.0-rc", "v1.0.0-rc.1").
    function isRc: boolean;

    // Rebuild and Returns the SemVer string.
    function asString: string;

    // The major version.
    property major: word read fMajor write fMajor;
    // The minor version.
    property minor: word read fMinor write fMinor;
    // The patch.
    property patch: word read fPatch write fPatch;
    // The additional labels.
    property additional: string read fAdditional write fAdditional;
    // Indicates if the init has succeeded.
    property valid: boolean read fValid;
  end;

  PSemVer = ^TSemVer;

  operator > (constref lhs, rhs: TSemVer): boolean;
  operator < (constref lhs, rhs: TSemVer): boolean;
  operator = (constref lhs, rhs: TSemVer): boolean;

implementation

{$IFDEF DEBUG}
var v1, v2: TSemVer;
{$ENDIF}

procedure TSemVer.init(const text: string; throw: boolean = true);

  procedure resetFields();
  begin
    fMajor := 0;
    fMinor := 0;
    fPatch := 0;
    fAdditional := '';
  end;

  procedure error(const message: string);
  begin
    resetFields();
    fAdditional := '';
    fValid := false;
    raise Exception.Create(message);
  end;

var
  r: TStringRange = (ptr:nil; pos:0; len: 0);
begin
  resetFields();
  if throw and (text.length < 6) then
    error('Invalid semVer format, at least 6 characters expected');
  r.init(text);
  if throw and (r.front <> 'v') then
    error('Invalid semVer format, the text must start with "v"');
  r.popFront;
  fMajor := r.takeUntil('.').yield.ToInteger;
  if throw and r.empty then
    error('Invalid semVer format, minor and patch miss');
  fMinor := r.popFront^.takeUntil('.').yield.ToInteger;
  if throw and r.empty then
    error('Invalid semVer format, the patch misses');
  fPatch := r.popFront^.takeWhile(['0'..'9']).yield.ToInteger;
  if not r.empty then
    fAdditional := r.popFront^.takeUntil(#0).yield;
  fValid := true;
end;

function TSemVer.isPreRelease: boolean;
begin
  result := isAlpha() or isBeta() or isRc();
end;

function TSemVer.isAlpha: boolean;
begin
  result := (additional = 'alpha') or (additional.StartsWith('alpha.'));
end;

function TSemVer.isBeta: boolean;
begin
  result := (additional = 'beta') or (additional.StartsWith('beta.'));
end;

function TSemVer.isRc: boolean;
begin
  result := (additional = 'rc') or (additional.StartsWith('rc.'));
end;

function TSemVer.asString: string;
begin
  result := format('%d.%d.%d', [fMajor, fMinor, fPatch]);
  if fAdditional <> '' then
    result += '-' + fAdditional;
end;

operator > (constref lhs, rhs: TSemVer): boolean;
begin
  result := (lhs.major > rhs.major) or
            ((lhs.major = rhs.major) and (lhs.minor > rhs.minor)) or
            ((lhs.major = rhs.major) and (lhs.minor = rhs.minor)
              and (lhs.patch > rhs.patch)) or
            ((lhs.major = rhs.major) and (lhs.minor = rhs.minor)
              and (lhs.patch = rhs.patch) and (lhs.additional > rhs.additional));
end;

operator < (constref lhs, rhs: TSemVer): boolean;
begin
  result := rhs > lhs;
end;

operator = (constref lhs, rhs: TSemVer): boolean;
begin
  result := (lhs.major = rhs.major) and (lhs.minor = rhs.minor)
              and (lhs.patch = rhs.patch) and (lhs.additional = rhs.additional);
end;


{$IFDEF DEBUG}
begin
  v1.init('v1.0.0');
  v2.init('v1.0.0');
  assert(v1 = v2);

  v1.init('v2.0.0');
  v2.init('v1.0.0');
  assert(v1 > v2);
  assert(v2 < v1);

  v1.init('v1.1.0');
  v2.init('v1.0.0');
  assert(v1 > v2);
  assert(v2 < v1);

  v1.init('v1.1.1');
  v2.init('v1.1.0');
  assert(v1 > v2);
  assert(v2 < v1);

  v1.init('v1.1.1');
  v2.init('v1.0.1');
  assert(v1 > v2);
  assert(v2 < v1);

  v1.init('v1.1.1-alpha.2');
  v2.init('v1.1.1-alpha.1');
  assert(v1 > v2);
  assert(v1.isAlpha);
  assert(v2.isAlpha);
  assert(v2.asString = '1.1.1-alpha.1');

  v1.init('v1.1.1-beta.1');
  v2.init('v1.1.1-alpha.8');
  assert(v1 > v2);
  assert(v1.isBeta);
  assert(v2.isAlpha);
  assert(v1.isPreRelease);

  v1.init('v1.2.3');
  v2.init('v1.22.33');
  assert(v1.major = 1);
  assert(v1.minor = 2);
  assert(v1.patch = 3);
  assert(v2.major = 1);
  assert(v2.minor = 22);
  assert(v2.patch = 33);
  assert(v2.asString = '1.22.33');

  v1.init('v0.0.2060');
  assert(v1.major = 0);
  assert(v1.minor = 0);
  assert(v1.patch = 2060);

  v2 := v1;
  assert(v2.major = 0);
  assert(v2.minor = 0);
  assert(v2.patch = 2060);

  v1.init('v0.6.0');
  assert(v1.major = 0);
  assert(v1.minor = 6);
  assert(v1.patch = 0);
  {$ENDIF}
end.


unit VariantUtils;

interface

uses Variants, SysUtils;

function VarToStringDef(Value: Variant; default: String = ''): string;
function VarToInt(Value: Variant): int64;
function VarToIntDef(Value: Variant; default: int64 = 0): int64;
function VarToIntf(Value: Variant): IUnknown;

function VarToBool(Value: Variant): Boolean;
function VarToBoolDef(Value: Variant; default: Boolean = False): Boolean;

function VarToFloat(Value: Variant): Extended;
function VarToFloatDef(Value: Variant; default: Extended = 0): Extended;

function VarToDateTimeDef(Value: Variant; default: TDateTime = 0): TDateTime;
function VarIntToSQL(Value: Variant): string;
function VarStrToSQL(Value: Variant): string;

function GetVariantTypeName(Value: Variant): string;

implementation

function VarToStringDef(Value: Variant; default: String = ''): string;
begin
  if VarIsNull(Value) then
    result := Default
  else
    result := Value;
end;

function VarToInt(Value: Variant): int64;
begin
  if VarType(Value) in [varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord, varInt64] then
    result := Value
  else
    raise Exception.Create(format('Unable to convert value from variant type %s to Int', [GetVariantTypeName(Value)]));
end;

function VarToIntDef(Value: Variant; default: int64 = 0): int64;
begin
  if VarType(Value) in [varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord, varInt64] then
    result := Value
  else if (VarType(Value) = varString) or (VarType(Value) = varOleStr) or (VarType(Value) = varUString) then
    result := StrToIntDef(Value, Default)
  else
    result := Default;
end;

function VarToIntf(Value: Variant): IUnknown;
var
  i: nativeint;
begin
  i := VarToIntDef(Value, 0);
  result := IUnknown(i);
end;

function VarToBool(Value: Variant): Boolean;
begin
  if VarType(Value) = varBoolean then
    result := Value
  else
    raise Exception.Create(format('Unable to convert value from variant type %s to Int', [GetVariantTypeName(Value)]));
end;

function VarToBoolDef(Value: Variant; default: Boolean = False): Boolean;
begin
  if VarType(Value) = varBoolean then
    result := Value
  else
    result := VarToIntDef(Value, integer(Default)) <> 0;
end;

function VarToFloat(Value: Variant): Extended;
begin
  if VarType(Value) in [varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency, varDate] then
    result := Value
  else
    raise Exception.Create(format('Unable to convert value from variant type %s to Int', [GetVariantTypeName(Value)]));
end;

function VarToFloatDef(Value: Variant; default: Extended = 0): Extended;
begin
  if VarType(Value) in [varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency, varDate] then
    result := Value
  else if (VarType(Value) = varString) or (VarType(Value) = varOleStr) then
    result := StrToFloatDef(Value, Default)
  else
    result := Default;
end;

function GetVariantTypeName(Value: Variant): string;
begin
  case VarType(Value) of
    varEmpty:
      result := 'varEmpty';
    varNull:
      result := 'varNull';
    varSmallint:
      result := 'varSmallint';
    varInteger:
      result := 'varInteger';
    varSingle:
      result := 'varSingle';
    varDouble:
      result := 'varDouble';
    varCurrency:
      result := 'varCurrency';
    varDate:
      result := 'varDate';
    varOleStr:
      result := 'varOleStr';
    varDispatch:
      result := 'varDispatch';
    varError:
      result := 'varError';
    varBoolean:
      result := 'varBoolean';
    varVariant:
      result := 'varVariant';
    varUnknown:
      result := 'varUnknown';
    varShortInt:
      result := 'varShortInt';
    varByte:
      result := 'varByte';
    varWord:
      result := 'varWord';
    varLongWord:
      result := 'varLongWord';
    varInt64:
      result := 'varInt64';
    varStrArg:
      result := 'varStrArg';
    varString:
      result := 'varString';
    varAny:
      result := 'varAny';
    varTypeMask:
      result := 'varTypeMask';
    varArray:
      result := 'varArray';
    varByRef:
      result := 'varByRef';
  else
    result := 'Undefined';
  end;
end;

function VarToDateTimeDef(Value: Variant; default: TDateTime = 0): TDateTime;
begin
  if VarIsNull(Value) then
    result := Default
  else
    result := VarToDateTime(Value);
end;

function VarIntToSQL(Value: Variant): string;
begin
  if Value = Unassigned then
    Value := null;
  if VarIsNull(Value) then
    result := 'NULL'
  else
    result := IntToStr(VarToInt(Value));
end;

function VarStrToSQL(Value: Variant): string;
begin
  if VarIsNull(Value) then
    result := 'NULL'
  else
    result := QuotedStr(VarToStr(Value));
end;

end.

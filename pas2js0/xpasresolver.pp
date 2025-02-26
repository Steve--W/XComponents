{
    This file is part of the Free Component Library

    Pascal resolver
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Abstract:
  Resolves references by setting TPasElement.CustomData as TResolvedReference.
  Creates search scopes for elements with sub identifiers by setting
    TPasElement.CustomData as TPasScope: unit, program, library, interface,
    implementation, procs

Works:
- built-in types as TPasUnresolvedSymbolRef: longint, int64, string, pointer, ...
- references in statements, error if not found
- interface and implementation types, vars, const
- params, local types, vars, const
- nested procedures
- nested forward procs, nested must be resolved before proc body
- program/library/implementation forward procs
- search in used units
- unitname.identifier
- alias types, 'type a=b'
- type alias type 'type a=type b'
- choose the most compatible overloaded procedure
- while..do
- repeat..until
- if..then..else
- binary operators
- case..of
  - check duplicate values
- try..finally..except, on, else, raise
- for loop
  - fail to write a loop var inside the loop
- spot duplicates
- type cast base types
- char
  - ord(), chr()
- record
  - variants
  - const param makes children const too
  - const  TRecordValues
  - function default(record type): record
- class:
  - forward declaration
  - instance.a
  - find ancestor, search in ancestors
  - virtual, abstract, override
  - method body
  - Self
  - inherited
  - property
    - read var, read function
    - write var, write function
    - stored function
    - defaultexpr
  - is and as operator
  - nil
  - constructor result type, rrfNewInstance
  - destructor call type: rrfFreeInstance
  - type cast
  - class of
  - class method, property, var, const
  - class-of.constructor
  - class-of typecast upwards/downwards
  - class-of option to allow is-operator
  - typecast Self in class method upwards/downwards
  - property with params
  - default property
  - visibility, override: warn and fix if lower
  - events, proc type of object
  - sealed
  - $M+ / $TYPEINFO use visPublished as default visibility
  - note: constructing class with abstract method
- with..do
- enums - TPasEnumType, TPasEnumValue
  - propagate to parent scopes
  - function ord(): integer
  - function low(ordinal): ordinal
  - function high(ordinal): ordinal
  - function pred(ordinal): ordinal
  - function high(ordinal): ordinal
  - cast integer to enum, enum to integer
  - $ScopedEnums
- sets - TPasSetType
  - set of char
  - set of integer
  - set of boolean
  - set of enum
  - ranges 'a'..'z'  2..5
  - operators: +, -, *, ><, <=, >=
  - in-operator
  - assign operators: +=, -=, *=
  - include(), exclude()
- typed const: check expr type
- function length(const array or string): integer
- procedure setlength(var array or string; newlength: integer)
- ranges TPasRangeType
- procedure exit, procedure exit(const function result)
- check if types only refer types+const
- check const expression types, e.g. bark on "const c:string=3;"
- procedure inc/dec(var ordinal; decr: ordinal = 1)
- function Assigned(Pointer or Class or Class-Of): boolean
- arrays TPasArrayType
  - TPasEnumType, char, integer, range
  - low, high, length, setlength, assigned
  - function concat(array1,array2,...): array
  - function copy(array): array, copy(a,start), copy(a,start,end)
  - insert(item; var array; index: integer)
  - delete(var array; start, count: integer)
  - element
  - multi dimensional
  - const
  - open array, override, pass array literal, pass var
  - type cast array to arrays with same dimensions and compatible element type
  - static array range checking
  - const array of char = string
  - a:=[...]   // assignation using constant array
  - a:=[[...],[...]]
  - a:=[...]+[...]  a+[]  []+a   modeswitch arrayoperators
  - delphi: var a: dynarray = [];  // square bracket initialization
- check if var initexpr fits vartype: var a: type = expr;
- built-in functions high, low for range types
- procedure type
  - call
  - as function result
  - as parameter
  - Delphi without @
  - @@ operator
  - FPC equal and not equal
  - "is nested"
  - bark on arguments access mismatch
- function without params: mark if call or address, rrfImplicitCallWithoutParams
- procedure break, procedure continue
- built-in functions pred, succ for range type and enums
- untyped parameters
- built-in procedure str(const boolean|integer|enumvalue|classinstance,var s: string)
- pointer TPasPointerType
  - nil, assigned(), typecast, class, classref, dynarray, procvar
  - forward declaration
  - cycle detection
  - TypedPointer^, (@Some)^
  - = operator: TypedPointer, @Some, UntypedPointer
  - TypedPointer:=TypedPointer
  - TypedPointer:=@Some
  - pointer[index], (@i)[index]
  - dispose(pointerofrecord), new(pointerofrecord)
  - $PointerMath on|off
- emit hints
  - platform, deprecated, experimental, library, unimplemented
  - hiding ancestor method
  - hiding other unit identifier
- dotted unitnames
- eval:
  - nil, true, false
  - range checking:
  - integer ranges
  - boolean ranges
  - enum ranges
  - char ranges
  - +, -, *, div, mod, /, shl, shr, or, and, xor, in, ^^, ><
  - =, <>, <, <=, >, >=
  - ord(), low(), high(), pred(), succ(), length()
  - string[index]
  - call(param)
  - a:=value
  - arr[index]
- resourcestrings
- custom ranges
  - enum: low(), high(), pred(), succ(), ord(), rg(int), int(rg), enum:=rg,
    rg:=rg, rg1:=rg2, rg:=enum, =, <>, in
    array[rg], low(array), high(array)
- for..in..do :
  - type boolean, char, byte, shortint, word, smallint, longword, longint
  - type enum range, char range, integer range
  - type/var set of: enum, enum range, integer, integer range, char, char range
  - array var
  - function: enumerator
  - class
- var modifier 'absolute'
- Assert(bool[,string])
- interfaces
  - $interfaces com|corba|default
  - root interface for com: delphi: IInterface, objfpc: IUnknown
  - method resolution
  - delegation via property implements: intftype, classtype
  - IntfVar as IntfType, intfvar as classtype, ObjVar as IntfType
  - IntfVar is IntfType, intfvar is classtype, ObjVar is IntfType
  - intftype(ObjVar), classtype(IntfVar)
  - default property
  - visibility public
  - $M+
  - class interfaces, check duplicates
  - assigned()
  - IntfVar:=nil, IntfVar:=IntfVar, IntfVar:=ObjVar, ObjVar:=IntfVar
  - IntfVar=IntfVar2
- currency
  - eval type TResEvalCurrency
  - eval +, -, *, /, ^^
  - float*currency and currency*float computes to currency
- type alias type overloads
- $writeableconst off $J-
- $warn identifier ON|off|error|default

ToDo:
- Include/Exclude for set of int/char/bool
- set of CharRange
- error if property method resolution is not used
- $H-hintpos$H+
- $pop, $push
- $RTTI inherited|explicit
- range checking:
  - property defaultvalue
  - IntSet:=[-1]
  - CharSet:=[#13]
- proc: check if forward and impl default values match
- call array of proc without ()
- anonymous functions
- attributes
- object
- type helpers
- record/class helpers
- generics, nested param lists
- futures
- operator overload
   - operator enumerator
- TPasFileType
- labels
- $zerobasedstrings on|off
- FOR_LOOP_VAR_VARPAR  passing a loop var to a var parameter gives a warning
- FOR_VARIABLE  warning if using a global var as loop var
- COMPARISON_FALSE COMPARISON_TRUE Comparison always evaluates to False
- USE_BEFORE_DEF Variable '%s' might not have been initialized
- FOR_LOOP_VAR_UNDEF FOR-Loop variable '%s' may be undefined after loop
- TYPEINFO_IMPLICITLY_ADDED Published caused RTTI ($M+) to be added to type '%s'
- IMPLICIT_STRING_CAST Implicit string cast from '%s' to '%s'
- IMPLICIT_STRING_CAST_LOSS Implicit string cast with potential data loss from '%s' to '%s'
- off by default: EXPLICIT_STRING_CAST Explicit string cast from '%s' to '%s'
- off by default: EXPLICIT_STRING_CAST_LOSS Explicit string cast with potential data loss from '%s' to '%s'
- IMPLICIT_INTEGER_CAST_LOSS Implicit integer cast with potential data loss from '%s' to '%s'
- IMPLICIT_CONVERSION_LOSS Implicit conversion may lose significant digits from '%s' to '%s'
- COMBINING_SIGNED_UNSIGNED64 Combining signed type and unsigned 64-bit type - treated as an unsigned type
-

Debug flags: -d<x>
  VerbosePasResolver

Notes:
 Functions and function types without parameters:
   property P read f; // use function f, not its result
   f.  // implicit resolve f once if param less function or function type
   f[]  // implicit resolve f once if a param less function or function type
   @f;  use function f, not its result
   @p.f;  @ operator applies to f, not p
   @f();  @ operator applies to result of f
   f(); use f's result
   FuncVar:=Func; if mode=objfpc: incompatible
                  if mode=delphi: implicit addr of function f
   if f=g then : can implicit resolve each side once
   p(f), f as var parameter: can implicit
}
unit xPasResolver;

{$mode objfpc}{$H+}
{$inline on}

{$ifdef fpc}
  {$define UsePChar}
  {$define HasInt64}
{$endif}

{$IFOPT Q+}{$DEFINE OverflowCheckOn}{$ENDIF}
{$IFOPT R+}{$DEFINE RangeCheckOn}{$ENDIF}

interface

uses
  {$ifdef pas2js}
  js, NodeJSFS,
  {$endif}
  Classes, SysUtils, Math, Types, contnrs,
  xPasTree, xPScanner, xPParser, xPasResolveEval;

const
  ParserMaxEmbeddedColumn = 2048;
  ParserMaxEmbeddedRow = $7fffffff div ParserMaxEmbeddedColumn;
  po_Resolver = [
    po_ResolveStandardTypes,
    po_NoOverloadedProcs,
    po_KeepClassForward,
    po_ArrayRangeExpr,
    po_CheckModeswitches,
    po_CheckCondFunction];

type
  TResolverBaseType = (
    btNone,        // undefined
    btCustom,      // provided by descendant resolver
    btContext,     // any source declared type with LoTypeEl/HiTypeEl
    btModule,
    btUntyped,     // TPasArgument without ArgType
    btChar,        // char
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar,    // ansichar
    {$endif}
    btWideChar,    // widechar
    btString,      // string
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,  // ansistring
    btShortString, // shortstring
    btRawByteString, // rawbytestring
    {$endif}
    btWideString,  // widestring
    btUnicodeString,// unicodestring
    btSingle,      // single  1.5E-45..3.4E38, digits 7-8, bytes 4
    btDouble,      // double  5.0E-324..1.7E308, digits 15-16, bytes 8
    btExtended,    // extended  platform, double or 1.9E-4932..1.1E4932, digits 19-20, bytes 10
    btCExtended,   // cextended
    btCurrency,    // as int64 div 10000, float, not ordinal
    btBoolean,     // boolean
    btByteBool,    // bytebool  true=not zero
    btWordBool,    // wordbool  true=not zero
    btLongBool,    // longbool  true=not zero
    {$ifdef HasInt64}
    btQWordBool,   // qwordbool true=not zero
    {$endif}
    btByte,        // byte  0..255
    btShortInt,    // shortint -128..127
    btWord,        // word  unsigned 2 bytes
    btSmallInt,    // smallint signed 2 bytes
    btUIntSingle,  // unsigned integer range of single 22bit
    btIntSingle,   // integer range of single  23bit
    btLongWord,    // longword unsigned 4 bytes
    btLongint,     // longint  signed 4 bytes
    btUIntDouble,  // unsigned integer range of double 52bit
    btIntDouble,   // integer range of double  53bit
    {$ifdef HasInt64}
    btQWord,       // qword   0..18446744073709551615, bytes 8
    btInt64,       // int64   -9223372036854775808..9223372036854775807, bytes 8
    btComp,        // as Int64, not ordinal
    {$endif}
    btPointer,     // pointer  or canonical pointer (e.g. @something)
    {$ifdef fpc}
    btFile,        // file
    btText,        // text
    btVariant,     // variant
    {$endif}
    btNil,         // nil = pointer, class, procedure, method, ...
    btProc,        // TPasProcedure
    btBuiltInProc, // TPasUnresolvedSymbolRef with CustomData is TResElDataBuiltInProc
    btArrayProperty,// IdentEl is TPasProperty with Args.Count>0, LoTypeEl=nil
    btSet,         // set of '', see SubType
    btArrayLit,    // []  array literal (TParamsExpr, TArrayValues, TBinaryExpr), see SubType
    btArrayOrSet,  // []  can be set or array literal, see SubType
    btRange        // a..b  see SubType
    );
  TResolveBaseTypes = set of TResolverBaseType;
const
  btIntMax = {$ifdef HasInt64}btInt64{$else}btIntDouble{$endif};
  btAllInteger = [btByte,btShortInt,btWord,btSmallInt,btIntSingle,btUIntSingle,
    btLongWord,btLongint,btIntDouble,btUIntDouble
    {$ifdef HasInt64}
    ,btQWord,btInt64,btComp
    {$endif}];
  btAllIntegerNoQWord = btAllInteger{$ifdef HasInt64}-[btQWord]{$endif};
  btAllChars = [btChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar];
  btAllStrings = [btString,
    {$ifdef FPC_HAS_CPSTRING}btAnsiString,btShortString,btRawByteString,{$endif}
    btWideString,btUnicodeString];
  btAllStringAndChars = btAllStrings+btAllChars;
  btAllStringPointer = [btString,
    {$ifdef FPC_HAS_CPSTRING}btAnsiString,btRawByteString,{$endif}
    btWideString,btUnicodeString];
  btAllFloats = [btSingle,btDouble,
    btExtended,btCExtended,btCurrency];
  btAllBooleans = [btBoolean,btByteBool,btWordBool,btLongBool
    {$ifdef HasInt64},btQWordBool{$endif}];
  btArrayRangeTypes = btAllChars+btAllBooleans+btAllInteger;
  btAllRanges = btArrayRangeTypes+[btRange];
  btAllStandardTypes = [
    btChar,
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar,
    {$endif}
    btWideChar,
    btString,
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,
    btShortString,
    btRawByteString,
    {$endif}
    btWideString,
    btUnicodeString,
    btSingle,
    btDouble,
    btExtended,
    btCExtended,
    btCurrency,
    btBoolean,
    btByteBool,
    btWordBool,
    btLongBool,
    {$ifdef HasInt64}
    btQWordBool,
    {$endif}
    btByte,
    btShortInt,
    btWord,
    btSmallInt,
    btLongWord,
    btLongint,
    {$ifdef HasInt64}
    btQWord,
    btInt64,
    btComp,
    {$endif}
    btPointer
    {$ifdef fpc}
    ,btFile,
    btText,
    btVariant
    {$endif}
    ];

  ResBaseTypeNames: array[TResolverBaseType] of string =(
    'None',
    'Custom',
    'Context',
    'Module',
    'Untyped',
    'Char',
    {$ifdef FPC_HAS_CPSTRING}
    'AnsiChar',
    {$endif}
    'WideChar',
    'String',
    {$ifdef FPC_HAS_CPSTRING}
    'AnsiString',
    'ShortString',
    'RawByteString',
    {$endif}
    'WideString',
    'UnicodeString',
    'Single',
    'Double',
    'Extended',
    'CExtended',
    'Currency',
    'Boolean',
    'ByteBool',
    'WordBool',
    'LongBool',
    {$ifdef HasInt64}
    'QWordBool',
    {$endif}
    'Byte',
    'ShortInt',
    'Word',
    'SmallInt',
    'UIntSingle',
    'IntSingle',
    'LongWord',
    'Longint',
    'UIntDouble',
    'IntDouble',
    {$ifdef HasInt64}
    'QWord',
    'Int64',
    'Comp',
    {$endif}
    'Pointer',
    {$ifdef fpc}
    'File',
    'Text',
    'Variant',
    {$endif}
    'Nil',
    'Procedure/Function',
    'BuiltInProc',
    'array property',
    'set',
    'array',
    'set or array literal',
    'range..'
    );

type
  TResolverBuiltInProc = (
    bfCustom,
    bfLength,
    bfSetLength,
    bfInclude,
    bfExclude,
    bfBreak,
    bfContinue,
    bfExit,
    bfInc,
    bfDec,
    bfAssigned,
    bfChr,
    bfOrd,
    bfLow,
    bfHigh,
    bfPred,
    bfSucc,
    bfStrProc,
    bfStrFunc,
    bfWriteStr,
    bfConcatArray,
    bfCopyArray,
    bfInsertArray,
    bfDeleteArray,
    bfTypeInfo,
    bfAssert,
    bfNew,
    bfDispose,
    bfDefault
    );
  TResolverBuiltInProcs = set of TResolverBuiltInProc;
const
  ResolverBuiltInProcNames: array[TResolverBuiltInProc] of string = (
    'Custom',
    'Length',
    'SetLength',
    'Include',
    'Exclude',
    'Break',
    'Continue',
    'Exit',
    'Inc',
    'Dec',
    'Assigned',
    'Chr',
    'Ord',
    'Low',
    'High',
    'Pred',
    'Succ',
    'Str',
    'Str',
    'WriteStr',
    'Concat',
    'Copy',
    'Insert',
    'Delete',
    'TypeInfo',
    'Assert',
    'New',
    'Dispose',
    'Default'
    );
  bfAllStandardProcs = [Succ(bfCustom)..high(TResolverBuiltInProc)];

const
  ResolverResultVar = 'Result';

type
  {$ifdef pas2js}
  TPasResIterate = procedure(Item, Arg: pointer) of object;

  { TPasResHashList }

  TPasResHashList = class
  private
    FItems: TJSObject;
  public
    constructor Create; reintroduce;
    procedure Add(const aName: string; Item: Pointer);
    function Find(const aName: string): Pointer;
    procedure ForEachCall(const Proc: TPasResIterate; Arg: Pointer);
    procedure Clear;
    procedure Remove(const aName: string);
  end;
  {$else}
  TPasResHashList = TFPHashList;
  {$endif}

type

  { EPasResolve }

  EPasResolve = class(Exception)
  private
    FPasElement: TPasElement;
    procedure SetPasElement(AValue: TPasElement);
  public
    Id: TMaxPrecInt;
    MsgType: TMessageType;
    MsgNumber: integer;
    MsgPattern: String;
    Args: TMessageArgs;
    SourcePos: TPasSourcePos;
    destructor Destroy; override;
    property PasElement: TPasElement read FPasElement write SetPasElement; // can be nil!
  end;

type

  { TUnresolvedPendingRef }

  TUnresolvedPendingRef = class(TPasUnresolvedSymbolRef)
  public
    Element: TPasType; // TPasClassOfType or TPasPointerType
  end;

  TPSRefAccess = (
    psraNone,
    psraRead,
    psraWrite,
    psraReadWrite,
    psraWriteRead,
    psraTypeInfo
    );

  { TPasScopeReference }

  TPasScopeReference = class
  private
    FElement: TPasElement;
    procedure SetElement(const AValue: TPasElement);
  public
    {$IFDEF VerbosePasResolver}
    Owner: TObject;
    {$ENDIF}
    Access: TPSRefAccess;
    NextSameName: TPasScopeReference;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;

  TPasScope = class;

  { TPasScopeReferences - used by TPasAnalyzer to store references of a proc or initialization section }

  TPasScopeReferences = class
  private
    FScope: TPasScope;
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnCollectItem(Item, aList: pointer);
  public
    References: TPasResHashList; // hash list of TPasScopeReference
    constructor Create(aScope: TPasScope);
    destructor Destroy; override;
    procedure Clear;
    function Add(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    function Find(const aName: string): TPasScopeReference;
    function GetList: TFPList;
    property Scope: TPasScope read FScope;
  end;

  TIterateScopeElement = procedure(El: TPasElement; ElScope, StartScope: TPasScope;
    Data: Pointer; var Abort: boolean) of object;

  { TPasScope -
    Elements like TPasClassType use TPasScope descendants as CustomData for
    their sub identifiers.
    TPasResolver.Scopes has a stack of TPasScope for searching identifiers.
    }

  TPasScope = Class(TResolveData)
  public
    VisibilityContext: TPasElement; // methods sets this to a TPasClassType,
      // used to check if the current context is allowed to access a
      // private/protected element
    class function IsStoredInElement: boolean; virtual;
    class function FreeOnPop: boolean; virtual;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure WriteIdentifiers(Prefix: string); virtual;
  end;
  TPasScopeClass = class of TPasScope;

  TPasModuleScopeFlag = (
    pmsfAssertSearched, // assert constructors searched
    pmsfRangeErrorNeeded, // somewhere is range checking on
    pmsfRangeErrorSearched // ERangeError constructor searched
    );
  TPasModuleScopeFlags = set of TPasModuleScopeFlag;

  { TPasModuleScope }

  TPasModuleScope = class(TPasScope)
  private
    FAssertClass: TPasClassType;
    FAssertDefConstructor: TPasConstructor;
    FAssertMsgConstructor: TPasConstructor;
    FRangeErrorClass: TPasClassType;
    FRangeErrorConstructor: TPasConstructor;
    procedure SetAssertClass(const AValue: TPasClassType);
    procedure SetAssertDefConstructor(const AValue: TPasConstructor);
    procedure SetAssertMsgConstructor(const AValue: TPasConstructor);
    procedure SetRangeErrorClass(const AValue: TPasClassType);
    procedure SetRangeErrorConstructor(const AValue: TPasConstructor);
  public
    FirstName: string; // the 'unit1' in 'unit1', or 'ns' in 'ns.unit1'
    PendingResolvers: TFPList; // list of TPasResolver waiting for the unit interface
    Flags: TPasModuleScopeFlags;
    BoolSwitches: TBoolSwitches;
    constructor Create; override;
    destructor Destroy; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    property AssertClass: TPasClassType read FAssertClass write SetAssertClass;
    property AssertDefConstructor: TPasConstructor read FAssertDefConstructor write SetAssertDefConstructor;
    property AssertMsgConstructor: TPasConstructor read FAssertMsgConstructor write SetAssertMsgConstructor;
    property RangeErrorClass: TPasClassType read FRangeErrorClass write SetRangeErrorClass;
    property RangeErrorConstructor: TPasConstructor read FRangeErrorConstructor write SetRangeErrorConstructor;
  end;
  TPasModuleScopeClass = class of TPasModuleScope;

  TPasIdentifierKind = (
    pikNone, // not yet initialized
    pikBaseType, // e.g. longint
    pikBuiltInProc,  // e.g. High(), SetLength()
    pikSimple, // simple vars, consts, types, enums
    pikProc, // may need parameter list with round brackets
    pikNamespace
    );
  TPasIdentifierKinds = set of TPasIdentifierKind;

  { TPasIdentifier }

  TPasIdentifier = Class(TObject)
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    {$IFDEF VerbosePasResolver}
    Owner: TObject;
    {$ENDIF}
    Identifier: String;
    NextSameIdentifier: TPasIdentifier; // next identifier with same name
    Kind: TPasIdentifierKind;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;
  TPasIdentifierArray = array of TPasIdentifier;

  { TPasIdentifierScope - elements with a list of sub identifiers }

  TPasIdentifierScope = Class(TPasScope)
  private
    FItems: TPasResHashList; // hashlist of TPasIdentifier
    procedure InternalAdd(Item: TPasIdentifier);
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnCollectItem(Item, List: pointer);
  protected
    procedure OnWriteItem(Item, Dummy: pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindLocalIdentifier(const Identifier: String): TPasIdentifier; inline;
    function FindIdentifier(const Identifier: String): TPasIdentifier; virtual;
    function RemoveLocalIdentifier(El: TPasElement): boolean; virtual;
    function AddIdentifier(const Identifier: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    function FindElement(const aName: string): TPasElement;
    procedure IterateLocalElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean);
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    procedure WriteLocalIdentifiers(Prefix: string); virtual;
    function GetLocalIdentifiers: TFPList; virtual;
  end;

  { TPasDefaultScope - root scope }

  TPasDefaultScope = class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasSectionScope - e.g. interface, implementation, program, library }

  TPasSectionScope = Class(TPasIdentifierScope)
  private
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
  public
    UsesScopes: TFPList; // list of TPasSectionScope
    UsesFinished: boolean;
    Finished: boolean;
    BoolSwitches: TBoolSwitches;
    ModeSwitches: TModeSwitches;
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasSectionScopeClass = class of TPasSectionScope;

  { TPasInitialFinalizationScope - e.g. TInitializationSection, TFinalizationSection }

  TPasInitialFinalizationScope = Class(TPasScope)
  public
    References: TPasScopeReferences; // created by TPasAnalyzer, not used by resolver
    function AddReference(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    destructor Destroy; override;
  end;
  TPasInitialFinalizationScopeClass = class of TPasInitialFinalizationScope;

  { TPasEnumTypeScope }

  TPasEnumTypeScope = Class(TPasIdentifierScope)
  public
    CanonicalSet: TPasSetType;
    destructor Destroy; override;
  end;

  { TPasRecordScope }

  TPasRecordScope = Class(TPasIdentifierScope)
  end;

  TPasClassScopeFlag = (
    pcsfAncestorResolved,
    pcsfSealed,
    pcsfPublished // default visibility is published due to $M directive
    );
  TPasClassScopeFlags = set of TPasClassScopeFlag;

  { TPasClassIntfMap }

  TPasClassIntfMap = class
  public
    Element: TPasElement;
    Intf: TPasClassType;
    Procs: TFPList;// maps Interface-member-index to TPasProcedure
    AncestorMap: TPasClassIntfMap;// AncestorMap.Element=Element, AncestorMap.Intf=DirectAncestor
    destructor Destroy; override;
  end;

  { TPasClassScope }

  TPasClassScope = Class(TPasIdentifierScope)
  public
    AncestorScope: TPasClassScope;
    CanonicalClassOf: TPasClassOfType;
    DirectAncestor: TPasType; // TPasClassType or TPasAliasType, see GetPasClassAncestor
    DefaultProperty: TPasProperty;
    Flags: TPasClassScopeFlags;
    AbstractProcs: TArrayOfPasProcedure;
    Interfaces: TFPList; // list corresponds to TPasClassType(Element).Interfaces,
      // elements: TPasProperty for 'implements', or TPasClassIntfMap
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasClassScopeClass = class of TPasClassScope;

  TPasProcedureScopeFlag = (
    ppsfIsGroupOverload // mode objfpc: one overload is enough for all procs in same scope
    );
  TPasProcedureScopeFlags = set of TPasProcedureScopeFlag;

  { TPasProcedureScope }

  TPasProcedureScope = Class(TPasIdentifierScope)
  public
    DeclarationProc: TPasProcedure; // the corresponding forward declaration
    ImplProc: TPasProcedure; // the corresponding proc with Body
    OverriddenProc: TPasProcedure; // if IsOverride then this is the ancestor proc (virtual or override)
    ClassScope: TPasClassScope;
    SelfArg: TPasArgument;
    Flags: TPasProcedureScopeFlags;
    BoolSwitches: TBoolSwitches; // if Body<>nil then body start, otherwise when FinishProc
    ModeSwitches: TModeSwitches; // at proc start
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    function GetSelfScope: TPasProcedureScope; // get the next parent procscope with a classcope
    procedure WriteIdentifiers(Prefix: string); override;
    destructor Destroy; override;
  public
    References: TPasScopeReferences; // created by TPasAnalyzer in DeclrationProc
    function AddReference(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    function GetReferences: TFPList;
  end;
  TPasProcedureScopeClass = class of TPasProcedureScope;

  { TPasPropertyScope }

  TPasPropertyScope = Class(TPasIdentifierScope)
  public
    AncestorProp: TPasProperty; { if TPasProperty(Element).VarType=nil this is an override
                                  otherwise it is a redeclaration }
    destructor Destroy; override;
  end;

  { TPasExceptOnScope }

  TPasExceptOnScope = Class(TPasIdentifierScope)
  end;

  TPasWithScope = class;

  TPasWithExprScopeFlag = (
    wesfNeedTmpVar,
    wesfOnlyTypeMembers,
    wesfIsClassOf,
    wesfConstParent // not writable
    );
  TPasWithExprScopeFlags = set of TPasWithExprScopeFlag;

  { TPasWithExprScope }

  TPasWithExprScope = Class(TPasScope)
  public
    WithScope: TPasWithScope; // owner
    Index: integer;
    Expr: TPasExpr;
    Scope: TPasScope;
    Flags: TPasWithExprScopeFlags;
    class function IsStoredInElement: boolean; override;
    class function FreeOnPop: boolean; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasWithExprScopeClass = class of TPasWithExprScope;

  { TPasWithScope }

  TPasWithScope = Class(TPasScope)
  public
    // Element is the TPasImplWithDo
    ExpressionScopes: TObjectList; // list of TPasWithExprScope
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TPasForLoopScope }

  TPasForLoopScope = Class(TPasScope)
  public
    GetEnumerator: TPasFunction;
    MoveNext: TPasFunction;
    Current: TPasProperty;
  end;

  { TPasSubScope - base class for sub scopes aka dotted scopes }

  TPasSubScope = Class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasIterateFilterData }

  TPasIterateFilterData = record
    OnIterate: TIterateScopeElement;
    Data: Pointer;
  end;
  PPasIterateFilterData = ^TPasIterateFilterData;

  { TPasModuleDotScope - scope for searching unitname.<identifier> }

  TPasModuleDotScope = Class(TPasSubScope)
  private
    FModule: TPasModule;
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
    procedure SetModule(AValue: TPasModule);
  public
    ImplementationScope: TPasSectionScope;
    InterfaceScope: TPasSectionScope;
    SystemScope: TPasDefaultScope;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    property Module: TPasModule read FModule write SetModule;
  end;

  { TPasDotIdentifierScope }

  TPasDotIdentifierScope = Class(TPasSubScope)
  public
    IdentifierScope: TPasIdentifierScope;
    OnlyTypeMembers: boolean; // true=only class var/procs, false=default=all
    ConstParent: boolean;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDotRecordScope - used for aRecord.subidentifier }

  TPasDotRecordScope = Class(TPasDotIdentifierScope)
  end;

  { TPasDotEnumTypeScope - used for EnumType.EnumValue }

  TPasDotEnumTypeScope = Class(TPasDotIdentifierScope)
  end;

  { TPasDotClassScope - used for aClass.subidentifier }

  TPasDotClassScope = Class(TPasDotIdentifierScope)
  private
    FClassScope: TPasClassScope;
    procedure SetClassScope(AValue: TPasClassScope);
  public
    InheritedExpr: boolean; // this is 'inherited <name>' instead of '.<name'
    IsClassOf: boolean; // true if aClassOf.
    property ClassScope: TPasClassScope read FClassScope write SetClassScope;
  end;

  TResolvedReferenceFlag = (
    rrfDotScope, // found reference via a dot scope (TPasDotIdentifierScope)
    rrfImplicitCallWithoutParams, // a TPrimitiveExpr is an implicit call without params
    rrfNoImplicitCallWithoutParams, // a TPrimitiveExpr is an implicit call without params
    rrfNewInstance, // constructor call (without it call constructor as normal method)
    rrfFreeInstance, // destructor call (without it call destructor as normal method)
    rrfVMT, // use VMT for call
    rrfConstInherited // parent is const and children are too
    );
  TResolvedReferenceFlags = set of TResolvedReferenceFlag;

type

  { TResolvedRefContext }

  TResolvedRefContext = Class
  end;

  TResolvedRefAccess = (
    rraNone,
    rraRead,  // expression is read
    rraAssign, // expression is LHS assign
    rraReadAndAssign, // expression is LHS +=, -=, *=, /=
    rraVarParam, // expression is passed to a var parameter
    rraOutParam, // expression is passed to an out parameter
    rraParamToUnknownProc // used as param, before knowing what overladed proc to call,
      // will later be changed to rraRead, rraVarParam, rraOutParam
    );
  TPRResolveVarAccesses = set of TResolvedRefAccess;

const
  rraAllWrite = [rraAssign,rraReadAndAssign,rraVarParam,rraOutParam];

  ResolvedToPSRefAccess: array[TResolvedRefAccess] of TPSRefAccess = (
    psraNone, // rraNone
    psraRead,  // rraRead
    psraWrite, // rraAssign
    psraReadWrite, // rraReadAndAssign
    psraReadWrite, // rraVarParam
    psraWrite, // rraOutParam
    psraNone // rraParamToUnknownProc
    );

type

  { TResolvedReference - CustomData for normal references }

  TResolvedReference = Class(TResolveData)
  private
    FDeclaration: TPasElement;
    procedure SetDeclaration(AValue: TPasElement);
  public
    Flags: TResolvedReferenceFlags;
    Access: TResolvedRefAccess;
    Context: TResolvedRefContext;
    WithExprScope: TPasWithExprScope;// if set, this reference used a With-block expression.
    destructor Destroy; override;
    property Declaration: TPasElement read FDeclaration write SetDeclaration;
  end;

  { TResolvedRefCtxConstructor - constructed class of a newinstance reference }

  TResolvedRefCtxConstructor = Class(TResolvedRefContext)
  public
    Typ: TPasType; // e.g. TPasClassType
  end;

  TPasResolverResultFlag = (
    rrfReadable,
    rrfWritable,
    rrfAssignable,  // not writable in general, e.g. aString[1]:=
    rrfCanBeStatement
    );
  TPasResolverResultFlags = set of TPasResolverResultFlag;

type
  { TPasResolverResult }

  TPasResolverResult = record
    BaseType: TResolverBaseType;
    SubType: TResolverBaseType; // for btSet, btArrayLit, btArrayOrSet, btRange
    IdentEl: TPasElement; // if set then this specific identifier is the value, can be a type
    LoTypeEl: TPasType; // can be nil for const expression, all alias resolved
    HiTypeEl: TPasType; // same as BaseTypeEl, except alias types are not resolved
    ExprEl: TPasExpr;
    Flags: TPasResolverResultFlags;
  end;
  PPasResolvedElement = ^TPasResolverResult;

type
  TPasResolverComputeFlag = (
    rcSetReferenceFlags,  // set flags of references while computing type, used by Resolve* methods
    rcNoImplicitProc,    // do not call a function without params, includes rcNoImplicitProcType
    rcNoImplicitProcType, // do not call a proc type without params
    rcConstant,  // resolve a constant expression, error if not computable
    rcType       // resolve a type expression
    );
  TPasResolverComputeFlags = set of TPasResolverComputeFlag;

  TResElDataBuiltInSymbol = Class(TResolveData)
  public
  end;

  { TResElDataBaseType - CustomData for compiler built-in types (TPasUnresolvedSymbolRef), e.g. longint }

  TResElDataBaseType = Class(TResElDataBuiltInSymbol)
  public
    BaseType: TResolverBaseType;
  end;
  TResElDataBaseTypeClass = class of TResElDataBaseType;

  TResElDataBuiltInProc = Class;

  TOnGetCallCompatibility = function(Proc: TResElDataBuiltInProc;
    Exp: TPasExpr; RaiseOnError: boolean): integer of object;
  TOnGetCallResult = procedure(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
    out ResolvedEl: TPasResolverResult) of object;
  TOnEvalBIFunction = procedure(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
    Flags: TResEvalFlags; out Evaluated: TResEvalValue) of object;
  TOnFinishParamsExpr = procedure(Proc: TResElDataBuiltInProc;
    Params: TParamsExpr) of object;

  TBuiltInProcFlag = (
    bipfCanBeStatement // a call is enough for a simple statement
    );
  TBuiltInProcFlags = set of TBuiltInProcFlag;

  { TResElDataBuiltInProc - TPasUnresolvedSymbolRef(aType).CustomData for compiler built-in procs like 'length' }

  TResElDataBuiltInProc = Class(TResElDataBuiltInSymbol)
  public
    Proc: TPasUnresolvedSymbolRef;
    Signature: string;
    BuiltIn: TResolverBuiltInProc;
    GetCallCompatibility: TOnGetCallCompatibility;
    GetCallResult: TOnGetCallResult;
    Eval: TOnEvalBIFunction;
    FinishParamsExpression: TOnFinishParamsExpr;
    Flags: TBuiltInProcFlags;
    destructor Destroy; override;
  end;

  { TPRFindData }

  TPRFindData = record
    ErrorPosEl: TPasElement;
    Found: TPasElement;
    ElScope: TPasScope; // Where Found was found
    StartScope: TPasScope; // where the search started
  end;
  PPRFindData = ^TPRFindData;

  TPasResolverOption = (
    proFixCaseOfOverrides,  // fix Name of overriding proc/property to the overriden proc/property
    proClassPropertyNonStatic,  // class property accessors are non static
    proPropertyAsVarParam, // allows to pass a property as a var/out argument
    proClassOfIs, // class-of supports is and as operator
    proExtClassInstanceNoTypeMembers, // class members of external class cannot be accessed by instance
    proOpenAsDynArrays, // open arrays work like dynamic arrays
    //ToDo: proStaticArrayCopy, // copy works with static arrays, returning a dynamic array
    //ToDo: proStaticArrayConcat, // concat works with static arrays, returning a dynamic array
    proProcTypeWithoutIsNested, // proc types can use nested procs without 'is nested'
    proMethodAddrAsPointer   // can assign @method to a pointer
    );
  TPasResolverOptions = set of TPasResolverOption;

  TPasResolverStep = (
    prsInit,
    prsParsing,
    prsFinishingModule,
    prsFinishedModule
    );
  TPasResolverSteps = set of TPasResolverStep;

  TPRResolveAlias = (
    prraNone, // do not resolve alias
    prraSimple, // resolve alias, but not type alias
    prraAlias // resolve alias and type alias
    );

  TPRProcTypeDescFlag = (
    prptdUseName, // add name if available
    prptdAddPaths, // add full paths to types
    prptdResolveSimpleAlias
    );
  TPRProcTypeDescFlags = set of TPRProcTypeDescFlag;

  { TPasResolver }

  TPasResolver = Class(TPasTreeContainer)
  private
    type
      TResolveDataListKind = (lkBuiltIn,lkModule);
    function GetBaseTypes(bt: TResolverBaseType): TPasUnresolvedSymbolRef; inline;
    function GetScopes(Index: integer): TPasScope; inline;
  private
    FAnonymousElTypePostfix: String;
    FBaseTypeChar: TResolverBaseType;
    FBaseTypeExtended: TResolverBaseType;
    FBaseTypeLength: TResolverBaseType;
    FBaseTypes: array[TResolverBaseType] of TPasUnresolvedSymbolRef;
    FBaseTypeString: TResolverBaseType;
    FBuiltInProcs: array[TResolverBuiltInProc] of TResElDataBuiltInProc;
    FDefaultNameSpace: String;
    FDefaultScope: TPasDefaultScope;
    FDynArrayMaxIndex: TMaxPrecInt;
    FDynArrayMinIndex: TMaxPrecInt;
    FLastCreatedData: array[TResolveDataListKind] of TResolveData;
    FLastElement: TPasElement;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgElement: TPasElement;
    FLastMsgId: TMaxPrecInt;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FLastSourcePos: TPasSourcePos;
    FOptions: TPasResolverOptions;
    FPendingForwardProcs: TFPList; // list of TPasElement needed to check for forward procs
    FRootElement: TPasModule;
    FScopeClass_Class: TPasClassScopeClass;
    FScopeClass_InitialFinalization: TPasInitialFinalizationScopeClass;
    FScopeClass_Module: TPasModuleScopeClass;
    FScopeClass_Proc: TPasProcedureScopeClass;
    FScopeClass_Section: TPasSectionScopeClass;
    FScopeClass_WithExpr: TPasWithExprScopeClass;
    FScopeCount: integer;
    FScopes: array of TPasScope; // stack of scopes
    FStep: TPasResolverStep;
    FStoreSrcColumns: boolean;
    FSubScopeCount: integer;
    FSubScopes: array of TPasScope; // stack of scopes
    FTopScope: TPasScope;
    procedure ClearResolveDataList(Kind: TResolveDataListKind);
    function GetBaseTypeNames(bt: TResolverBaseType): string;
    function GetBuiltInProcs(bp: TResolverBuiltInProc): TResElDataBuiltInProc;
  protected
    const
      cExact = 0;
      cAliasExact = cExact+1;
      cCompatible = cAliasExact+1;
      cIntToIntConversion = ord(High(TResolverBaseType));
      cFloatToFloatConversion = 2*cIntToIntConversion;
      cTypeConversion = cExact+10000; // e.g. TObject to Pointer
      cLossyConversion = cExact+100000;
      cIntToFloatConversion = cExact+400000; // int to float is worse than bigint to smallint
      cIncompatible = High(integer);
    var
      cTGUIDToString: integer;
      cStringToTGUID: integer;
      cInterfaceToTGUID: integer;
      cInterfaceToString: integer;
    type
      TFindCallElData = record
        Params: TParamsExpr;
        Found: TPasElement; // TPasProcedure or TPasUnresolvedSymbolRef(built in proc) or TPasType (typecast)
        LastProc: TPasProcedure;
        ElScope, StartScope: TPasScope;
        Distance: integer; // compatibility distance
        Count: integer;
        List: TFPList; // if not nil then collect all found elements here
      end;
      PFindCallElData = ^TFindCallElData;

      TFindOverloadProcKind = (
        fopkSameSignature, // search method declaration for a body
        fopkProc,   // check overloads for a proc
        fopkMethod  // check overloads for a method
        );
      TFindOverloadProcData = record
        Proc: TPasProcedure;
        Args: TFPList;        // List of TPasArgument objects
        Kind: TFindOverloadProcKind;
        OnlyScope: TPasScope;
        FoundOverloadModifier: boolean;
        FoundInSameScope: integer;
        Found: TPasProcedure;
        ElScope, StartScope: TPasScope;
        FoundNonProc: TPasElement;
      end;
      PFindOverloadProcData = ^TFindOverloadProcData;

    procedure OnFindFirstElement(El: TPasElement; ElScope, StartScope: TPasScope;
      FindFirstElementData: Pointer; var Abort: boolean); virtual;
    procedure OnFindCallElements(El: TPasElement; ElScope, StartScope: TPasScope;
      FindProcsData: Pointer; var Abort: boolean); virtual; // find candidates for Name(params)
    procedure OnFindOverloadProc(El: TPasElement; ElScope, StartScope: TPasScope;
      FindOverloadData: Pointer; var Abort: boolean); virtual;
    function IsSameProcContext(ProcParentA, ProcParentB: TPasElement): boolean;
    function FindProcOverload(const ProcName: string; Proc: TPasProcedure;
      OnlyScope: TPasScope): TPasProcedure;
  protected
    procedure SetCurrentParser(AValue: TPasParser); override;
    procedure ScannerWarnDirective(Sender: TObject; Identifier: string;
      State: TWarnMsgState; var Handled: boolean); virtual;
    procedure SetRootElement(const AValue: TPasModule); virtual;
    procedure CheckTopScope(ExpectedClass: TPasScopeClass; AllowDescendants: boolean = false);
    function AddIdentifier(Scope: TPasIdentifierScope;
      const aName: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    procedure AddModule(El: TPasModule); virtual;
    procedure AddSection(El: TPasSection); virtual;
    procedure AddInitialFinalizationSection(El: TPasImplBlock); virtual;
    procedure AddType(El: TPasType); virtual;
    procedure AddRecordType(El: TPasRecordType); virtual;
    procedure AddClassType(El: TPasClassType); virtual;
    procedure AddVariable(El: TPasVariable); virtual;
    procedure AddResourceString(El: TPasResString); virtual;
    procedure AddEnumType(El: TPasEnumType); virtual;
    procedure AddEnumValue(El: TPasEnumValue); virtual;
    procedure AddProperty(El: TPasProperty); virtual;
    procedure AddProcedure(El: TPasProcedure); virtual;
    procedure AddProcedureBody(El: TProcedureBody); virtual;
    procedure AddArgument(El: TPasArgument); virtual;
    procedure AddFunctionResult(El: TPasResultElement); virtual;
    procedure AddExceptOn(El: TPasImplExceptOn); virtual;
    procedure ResolveImplBlock(Block: TPasImplBlock); virtual;
    procedure ResolveImplElement(El: TPasImplElement); virtual;
    procedure ResolveImplCaseOf(CaseOf: TPasImplCaseOf); virtual;
    procedure ResolveImplLabelMark(Mark: TPasImplLabelMark); virtual;
    procedure ResolveImplForLoop(Loop: TPasImplForLoop); virtual;
    procedure ResolveImplWithDo(El: TPasImplWithDo); virtual;
    procedure ResolveImplAsm(El: TPasImplAsmStatement); virtual;
    procedure ResolveImplAssign(El: TPasImplAssign); virtual;
    procedure ResolveImplSimple(El: TPasImplSimple); virtual;
    procedure ResolveImplRaise(El: TPasImplRaise); virtual;
    procedure ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveStatementConditionExpr(El: TPasExpr); virtual;
    procedure ResolveNameExpr(El: TPasExpr; const aName: string; Access: TResolvedRefAccess); virtual;
    procedure ResolveInherited(El: TInheritedExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveInheritedCall(El: TBinaryExpr; Access: TResolvedRefAccess);         virtual;
    procedure ResolveBinaryExpr(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveSubIdent(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveFuncParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveArrayParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveArrayParamsArgs(Params: TParamsExpr;
      const ResolvedValue: TPasResolverResult; Access: TResolvedRefAccess); virtual;
    function ResolveBracketOperatorClass(Params: TParamsExpr;
      const ResolvedValue: TPasResolverResult; ClassScope: TPasClassScope;
      Access: TResolvedRefAccess): boolean; virtual;
    procedure ResolveSetParamsExpr(Params: TParamsExpr); virtual;
    procedure ResolveArrayValues(El: TArrayValues); virtual;
    procedure ResolveRecordValues(El: TRecordValues); virtual;
    function ResolveAccessor(Expr: TPasExpr): TPasElement;
    procedure SetResolvedRefAccess(Expr: TPasExpr; Ref: TResolvedReference;
      Access: TResolvedRefAccess); virtual;
    procedure AccessExpr(Expr: TPasExpr; Access: TResolvedRefAccess);
    function MarkArrayExpr(Expr: TParamsExpr; ArrayType: TPasArrayType): boolean; virtual;
    procedure MarkArrayExprRecursive(Expr: TPasExpr; ArrType: TPasArrayType); virtual;
    procedure FinishModule(CurModule: TPasModule); virtual;
    procedure FinishUsesClause; virtual;
    procedure FinishSection(Section: TPasSection); virtual;
    procedure FinishInterfaceSection(Section: TPasSection); virtual;
    procedure FinishTypeSection(El: TPasDeclarations); virtual;
    procedure FinishTypeDef(El: TPasType); virtual;
    procedure FinishEnumType(El: TPasEnumType); virtual;
    procedure FinishSetType(El: TPasSetType); virtual;
    procedure FinishSubElementType(Parent: TPasElement; El: TPasType); virtual;
    procedure FinishRangeType(El: TPasRangeType); virtual;
    procedure FinishConstRangeExpr(RangeExpr: TBinaryExpr;
      out LeftResolved, RightResolved: TPasResolverResult);
    procedure FinishRecordType(El: TPasRecordType); virtual;
    procedure FinishClassType(El: TPasClassType); virtual;
    procedure FinishClassOfType(El: TPasClassOfType); virtual;
    procedure FinishPointerType(El: TPasPointerType); virtual;
    procedure FinishArrayType(El: TPasArrayType); virtual;
    procedure FinishResourcestring(El: TPasResString); virtual;
    procedure FinishProcedure(aProc: TPasProcedure); virtual;
    procedure FinishProcedureType(El: TPasProcedureType); virtual;
    procedure FinishMethodDeclHeader(Proc: TPasProcedure); virtual;
    procedure FinishMethodImplHeader(ImplProc: TPasProcedure); virtual;
    procedure FinishExceptOnExpr; virtual;
    procedure FinishExceptOnStatement; virtual;
    procedure FinishDeclaration(El: TPasElement); virtual;
    procedure FinishVariable(El: TPasVariable); virtual;
    procedure FinishPropertyOfClass(PropEl: TPasProperty); virtual;
    procedure FinishArgument(El: TPasArgument); virtual;
    procedure FinishAncestors(aClass: TPasClassType); virtual;
    procedure FinishMethodResolution(El: TPasMethodResolution); virtual;
    procedure FinishPropertyParamAccess(Params: TParamsExpr;
      Prop: TPasProperty);
    procedure FinishCallArgAccess(Expr: TPasExpr; Access: TResolvedRefAccess);
    procedure FinishInitialFinalization(El: TPasImplBlock);
    procedure EmitTypeHints(PosEl: TPasElement; aType: TPasType); virtual;
    function EmitElementHints(PosEl, El: TPasElement): boolean; virtual;
    procedure StoreScannerFlagsInProc(ProcScope: TPasProcedureScope);
    procedure ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope: TPasProcedureScope);
    function CreateClassIntfMap(El: TPasClassType; Index: integer): TPasClassIntfMap;
    procedure CheckConditionExpr(El: TPasExpr; const ResolvedEl: TPasResolverResult); virtual;
    procedure CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure; CheckNames: boolean);
    procedure CheckPendingForwardProcs(El: TPasElement);
    procedure CheckPointerCycle(El: TPasPointerType);
    procedure ComputeUnaryNot(El: TUnaryExpr; var ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags); virtual;
    procedure ComputeBinaryExpr(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeBinaryExprRes(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      var LeftResolved, RightResolved: TPasResolverResult); virtual;
    procedure ComputeArrayParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeArrayParams_Class(Params: TParamsExpr;
      var ResolvedEl: TPasResolverResult; ClassScope: TPasClassScope;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement); virtual;
    procedure ComputeFuncParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeTypeCast(ToLoType, ToHiType: TPasType;
      Param: TPasExpr; const ParamResolved: TPasResolverResult;
      out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags); virtual;
    procedure ComputeSetParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeDereference(El: TUnaryExpr; var ResolvedEl: TPasResolverResult);
    procedure ComputeArrayValuesExpectedType(El: TArrayValues; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    procedure ComputeRecordValues(El: TRecordValues; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    procedure CheckIsClass(El: TPasElement; const ResolvedEl: TPasResolverResult);
    function CheckTypeCastClassInstanceToClass(
      const FromClassRes, ToClassRes: TPasResolverResult;
      ErrorEl: TPasElement): integer; virtual;
    procedure CheckSetLitElCompatible(Left, Right: TPasExpr;
      const LHS, RHS: TPasResolverResult);
    function CheckIsOrdinal(const ResolvedEl: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): boolean;
    procedure CombineArrayLitElTypes(Left, Right: TPasExpr;
      var LHS: TPasResolverResult; const RHS: TPasResolverResult);
    procedure ConvertRangeToElement(var ResolvedEl: TPasResolverResult);
    function IsCharLiteral(const Value: string; ErrorPos: TPasElement): TResolverBaseType; virtual;
    function CheckForIn(Loop: TPasImplForLoop;
      const VarResolved, InResolved: TPasResolverResult): boolean; virtual;
    function CheckForInClass(Loop: TPasImplForLoop;
      const VarResolved, InResolved: TPasResolverResult): boolean; virtual;
    function CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc; Expr: TPasExpr;
      MinCount: integer; RaiseOnError: boolean): boolean;
    function CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
      MaxCount: integer; RaiseOnError: boolean): integer;
    function CheckRaiseTypeArgNo(id: TMaxPrecInt; ArgNo: integer; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; Expected: string; RaiseOnError: boolean): integer;
    function FindUsedUnitInSection(const aName: string; Section: TPasSection): TPasModule;
    function FindUsedUnit(const aName: string; aMod: TPasModule): TPasModule;
    procedure FinishAssertCall(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function FindExceptionConstructor(const aUnitName, aClassName: string;
      out aClass: TPasClassType; out aConstructor: TPasConstructor;
      ErrorEl: TPasElement): boolean; virtual;
    procedure FindAssertExceptionConstructors(ErrorEl: TPasElement); virtual;
    procedure FindRangeErrorConstructors(ErrorEl: TPasElement); virtual;
  protected
    fExprEvaluator: TResExprEvaluator;
    procedure OnExprEvalLog(Sender: TResExprEvaluator; const id: TMaxPrecInt;
      MsgType: TMessageType; MsgNumber: integer; const Fmt: String;
      Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif}; PosEl: TPasElement); virtual;
    function OnExprEvalIdentifier(Sender: TResExprEvaluator;
      Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
    function OnExprEvalParams(Sender: TResExprEvaluator;
      Params: TParamsExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
    procedure OnRangeCheckEl(Sender: TResExprEvaluator; El: TPasElement;
      var MsgType: TMessageType); virtual;
    function EvalBaseTypeCast(Params: TParamsExpr; bt: TResolverBaseType): TResEvalvalue;
  protected
    // custom types (added by descendant resolvers)
    function CheckAssignCompatibilityCustom(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean; var Handled: boolean): integer; virtual;
    function CheckEqualCompatibilityCustomType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer; virtual;
  protected
    // built-in functions
    function BI_Length_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Length_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Length_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_SetLength_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_SetLength_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_InExclude_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_InExclude_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Break_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_Continue_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_Exit_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_IncDec_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_IncDec_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Assigned_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Assigned_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Assigned_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Chr_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Chr_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Ord_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_LowHigh_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_LowHigh_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_PredSucc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_PredSucc_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_PredSucc_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; ArgNo: integer;
      RaiseOnError: boolean): integer;
    function BI_StrProc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_StrFunc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrFunc_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_StrFunc_OnEval({%H-}Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_WriteStrProc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_WriteStrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_ConcatArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_ConcatArray_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_CopyArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_CopyArray_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_InsertArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_InsertArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_DeleteArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_DeleteArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_TypeInfo_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_TypeInfo_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_Assert_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Assert_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_New_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_New_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Dispose_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Dispose_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Default_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Default_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Default_OnEval({%H-}Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual; // does not free built-in identifiers
    // overrides of TPasTreeContainer
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      overload; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement;
      overload; override;
    function FindModule(const AName: String; NameExpr, InFileExpr: TPasExpr): TPasModule; override;
    function FindUnit(const AName, InFilename: String;
      NameExpr, InFileExpr: TPasExpr): TPasModule; virtual; abstract;
    function FindElement(const aName: String): TPasElement; override; // used by TPasParser
    function FindElementWithoutParams(const AName: String; ErrorPosEl: TPasElement;
      NoProcsWithArgs: boolean): TPasElement;
    function FindElementWithoutParams(const AName: String; out Data: TPRFindData;
      ErrorPosEl: TPasElement; NoProcsWithArgs: boolean): TPasElement;
    procedure FindLongestUnitName(var El: TPasElement; Expr: TPasExpr);
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure CheckFoundElement(const FindData: TPRFindData;
      Ref: TResolvedReference); virtual;
    function GetVisibilityContext: TPasElement;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); override;
    procedure FinishTypeAlias(var NewType: TPasType); override;
    function IsUnitIntfFinished(AModule: TPasModule): boolean;
    procedure NotifyPendingUsedInterfaces; virtual;
    function GetPendingUsedInterface(Section: TPasSection): TPasUsesUnit;
    function CheckPendingUsedInterface(Section: TPasSection): boolean; override;
    procedure UsedInterfacesFinished(Section: TPasSection); virtual;
    function NeedArrayValues(El: TPasElement): boolean; override;
    function GetDefaultClassVisibility(AClass: TPasClassType
      ): TPasMemberVisibility; override;
    procedure ModeChanged(Sender: TObject; NewMode: TModeSwitch;
      Before: boolean; var Handled: boolean); override;
    // built in types and functions
    procedure ClearBuiltInIdentifiers; virtual;
    procedure AddObjFPCBuiltInIdentifiers(
      const TheBaseTypes: TResolveBaseTypes = btAllStandardTypes;
      const TheBaseProcs: TResolverBuiltInProcs = bfAllStandardProcs); virtual;
    function AddBaseType(const aName: string; Typ: TResolverBaseType): TResElDataBaseType;
    function AddCustomBaseType(const aName: string; aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
    function IsBaseType(aType: TPasType; BaseType: TResolverBaseType; ResolveAlias: boolean = false): boolean;
    function AddBuiltInProc(const aName: string; Signature: string;
      const GetCallCompatibility: TOnGetCallCompatibility;
      const GetCallResult: TOnGetCallResult;
      const EvalConst: TOnEvalBIFunction = nil;
      const FinishParamsExpr: TOnFinishParamsExpr = nil;
      const BuiltIn: TResolverBuiltInProc = bfCustom;
      const Flags: TBuiltInProcFlags = []): TResElDataBuiltInProc;
    // add extra TResolveData (E.CustomData) to free list
    procedure AddResolveData(El: TPasElement; Data: TResolveData;
      Kind: TResolveDataListKind);
    function CreateReference(DeclEl, RefEl: TPasElement;
      Access: TResolvedRefAccess;
      FindData: PPRFindData = nil): TResolvedReference; virtual;
    // scopes
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure PopScope;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; overload;
    function PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
    function PushClassDotScope(var CurClassType: TPasClassType): TPasDotClassScope;
    function PushRecordDotScope(CurRecordType: TPasRecordType): TPasDotRecordScope;
    function PushEnumDotScope(CurEnumType: TPasEnumType): TPasDotEnumTypeScope;
    procedure ResetSubScopes(out Depth: integer);
    procedure RestoreSubScopes(Depth: integer);
    function GetInheritedExprScope(ErrorEl: TPasElement): TPasProcedureScope;
    // log and messages
    class function MangleSourceLineNumber(Line, Column: integer): integer;
    class procedure UnmangleSourceLineNumber(LineNumber: integer;
      out Line, Column: integer);
    class function GetDbgSourcePosStr(El: TPasElement): string;
    function GetElementSourcePosStr(El: TPasElement): string;
    procedure SetLastMsg(const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      PosEl: TPasElement);
    procedure LogMsg(const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
      const Fmt: String; Args: Array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      PosEl: TPasElement); overload;
    class function GetWarnIdentifierNumbers(Identifier: string;
      out MsgNumbers: TIntegerDynArray): boolean; virtual;
    procedure GetIncompatibleTypeDesc(const GotType, ExpType: TPasResolverResult;
      out GotDesc, ExpDesc: String); overload;
    procedure GetIncompatibleTypeDesc(const GotType, ExpType: TPasType;
      out GotDesc, ExpDesc: String); overload;
    procedure RaiseMsg(const Id: TMaxPrecInt; MsgNumber: integer; const Fmt: String;
      Args: Array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      ErrorPosEl: TPasElement); virtual;
    procedure RaiseNotYetImplemented(id: TMaxPrecInt; El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(id: TMaxPrecInt; const Msg: string = '');
    procedure RaiseInvalidScopeForElement(id: TMaxPrecInt; El: TPasElement; const Msg: string = '');
    procedure RaiseIdentifierNotFound(id: TMaxPrecInt; Identifier: string; El: TPasElement);
    procedure RaiseXExpectedButYFound(id: TMaxPrecInt; const X,Y: string; El: TPasElement);
    procedure RaiseContextXExpectedButYFound(id: TMaxPrecInt; const C,X,Y: string; El: TPasElement);
    procedure RaiseContextXInvalidY(id: TMaxPrecInt; const X,Y: string; El: TPasElement);
    procedure RaiseConstantExprExp(id: TMaxPrecInt; ErrorEl: TPasElement);
    procedure RaiseVarExpected(id: TMaxPrecInt; ErrorEl: TPasElement; IdentEl: TPasElement);
    procedure RaiseRangeCheck(id: TMaxPrecInt; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeDesc(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      const GotDesc, ExpDesc: String; ErrorEl: TPasElement);
    procedure RaiseIncompatibleType(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      GotType, ExpType: TPasType; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeRes(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
      const GotType, ExpType: TPasResolverResult;
      ErrorEl: TPasElement);
    procedure RaiseInvalidProcTypeModifier(id: TMaxPrecInt; ProcType: TPasProcedureType;
      ptm: TProcTypeModifier; ErrorEl: TPasElement);
    procedure RaiseInvalidProcModifier(id: TMaxPrecInt; Proc: TPasProcedure;
      pm: TProcedureModifier; ErrorEl: TPasElement);
    procedure WriteScopes;
    // find value and type of an element
    procedure ComputeElement(El: TPasElement; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    function Eval(Expr: TPasExpr; Flags: TResEvalFlags; Store: boolean = true): TResEvalValue; overload;
    function Eval(const Value: TPasResolverResult; Flags: TResEvalFlags; Store: boolean = true): TResEvalValue; overload;
    // checking compatibilility
    function IsSameType(TypeA, TypeB: TPasType; ResolveAlias: TPRResolveAlias): boolean; // check if it is exactly the same
    function HasExactType(const ResolvedEl: TPasResolverResult): boolean; // false if HiTypeEl was guessed, e.g. 1 guessed a btLongint
    function CheckCallProcCompatibility(ProcType: TPasProcedureType;
      Params: TParamsExpr; RaiseOnError: boolean;
      SetReferenceFlags: boolean = false): integer;
    function CheckCallPropertyCompatibility(PropEl: TPasProperty;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
      Params: TParamsExpr; RaiseOnError: boolean; EmitHints: boolean = false): integer;
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean; SetReferenceFlags: boolean = false): integer;
    function CheckAssignCompatibilityUserType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckAssignCompatibilityArrayType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckAssignCompatibilityPointerType(LTypeEl, RTypeEl: TPasType;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function CheckEqualCompatibilityUserType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer; // LHS.BaseType=btContext=RHS.BaseType and both rrfReadable
    function CheckTypeCast(El: TPasType; Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckTypeCastRes(const FromResolved, ToResolved: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer; virtual;
    function CheckTypeCastArray(FromType, ToType: TPasArrayType;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer;
    function CheckSrcIsADstType(
      const ResolvedSrcType, ResolvedDestType: TPasResolverResult;
      ErrorEl: TPasElement): integer;
    function CheckClassIsClass(SrcType, DestType: TPasType;
      ErrorEl: TPasElement): integer; virtual;
    function CheckClassesAreRelated(TypeA, TypeB: TPasType;
      ErrorEl: TPasElement): integer;
    function GetClassImplementsIntf(ClassEl, Intf: TPasClassType): TPasClassType;
    function CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure): boolean;
    function CheckProcTypeCompatibility(Proc1, Proc2: TPasProcedureType;
      IsAssign: boolean; ErrorEl: TPasElement; RaiseOnIncompatible: boolean): boolean;
    function CheckProcArgCompatibility(Arg1, Arg2: TPasArgument): boolean;
    function CheckElTypeCompatibility(Arg1, Arg2: TPasType; ResolveAlias: TPRResolveAlias): boolean;
    function CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
      ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
    function CheckAssignCompatibility(const LHS, RHS: TPasElement;
      RaiseOnIncompatible: boolean = true; ErrorEl: TPasElement = nil): integer;
    procedure CheckAssignExprRange(const LeftResolved: TPasResolverResult; RHS: TPasExpr);
    procedure CheckAssignExprRangeToCustom(const LeftResolved: TPasResolverResult;
      RValue: TResEvalValue; RHS: TPasExpr); virtual;
    function CheckAssignResCompatibility(const LHS, RHS: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function CheckEqualElCompatibility(Left, Right: TPasElement;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean;
      SetReferenceFlags: boolean = false): integer;
    function CheckEqualResCompatibility(const LHS, RHS: TPasResolverResult;
      LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
      RErrorEl: TPasElement = nil): integer;
    function IsVariableConst(El, PosEl: TPasElement; RaiseIfConst: boolean): boolean; virtual;
    function ResolvedElCanBeVarParam(const ResolvedEl: TPasResolverResult;
      PosEl: TPasElement; RaiseIfConst: boolean = true): boolean;
    function ResolvedElIsClassInstance(const ResolvedEl: TPasResolverResult): boolean;
    // utility functions
    function GetElModeSwitches(El: TPasElement): TModeSwitches;
    function GetElBoolSwitches(El: TPasElement): TBoolSwitches;
    function GetProcTypeDescription(ProcType: TPasProcedureType;
      Flags: TPRProcTypeDescFlags = [prptdUseName,prptdResolveSimpleAlias]): string;
    function GetResolverResultDescription(const T: TPasResolverResult; OnlyType: boolean = false): string;
    function GetTypeDescription(aType: TPasType; AddPath: boolean = false): string;
    function GetTypeDescription(const R: TPasResolverResult; AddPath: boolean = false): string; virtual;
    function GetBaseDescription(const R: TPasResolverResult; AddPath: boolean = false): string; virtual;
    function GetProcFirstImplEl(Proc: TPasProcedure): TPasImplElement;
    function GetPasPropertyAncestor(El: TPasProperty; WithRedeclarations: boolean = false): TPasProperty;
    function GetPasPropertyType(El: TPasProperty): TPasType;
    function GetPasPropertyArgs(El: TPasProperty): TFPList;
    function GetPasPropertyGetter(El: TPasProperty): TPasElement;
    function GetPasPropertySetter(El: TPasProperty): TPasElement;
    function GetPasPropertyIndex(El: TPasProperty): TPasExpr;
    function GetPasPropertyStoredExpr(El: TPasProperty): TPasExpr;
    function GetPasPropertyDefaultExpr(El: TPasProperty): TPasExpr;
    function GetPasClassAncestor(ClassEl: TPasClassType; SkipAlias: boolean): TPasType;
    function ProcHasImplElements(Proc: TPasProcedure): boolean; virtual;
    function IndexOfImplementedInterface(ClassEl: TPasClassType; aType: TPasType): integer;
    function GetLoop(El: TPasElement): TPasImplElement;
    function ResolveAliasType(aType: TPasType): TPasType;
    function ResolveAliasTypeEl(El: TPasElement): TPasType; inline;
    function ExprIsAddrTarget(El: TPasExpr): boolean;
    function IsNameExpr(El: TPasExpr): boolean; inline; // TSelfExpr or TPrimitiveExpr with Kind=pekIdent
    function GetNameExprValue(El: TPasExpr): string; // TSelfExpr or TPrimitiveExpr with Kind=pekIdent
    function GetNextDottedExpr(El: TPasExpr): TPasExpr;
    function GetUsesUnitInFilename(InFileExpr: TPasExpr): string;
    function GetPathStart(El: TPasExpr): TPasExpr;
    function GetNewInstanceExpr(El: TPasExpr): TPasExpr;
    function ParentNeedsExprResult(El: TPasExpr): boolean;
    function GetReference_NewInstanceClass(Ref: TResolvedReference): TPasClassType;
    function IsDynArray(TypeEl: TPasType; OptionalOpenArray: boolean = true): boolean;
    function IsOpenArray(TypeEl: TPasType): boolean;
    function IsDynOrOpenArray(TypeEl: TPasType): boolean;
    function IsVarInit(Expr: TPasExpr): boolean;
    function IsEmptyArrayExpr(const ResolvedEl: TPasResolverResult): boolean;
    function IsClassMethod(El: TPasElement): boolean;
    function IsClassField(El: TPasElement): boolean;
    function IsExternalClass_Name(aClass: TPasClassType; const ExtName: string): boolean;
    function IsProcedureType(const ResolvedEl: TPasResolverResult; HasValue: boolean): boolean;
    function IsArrayType(const ResolvedEl: TPasResolverResult): boolean;
    function IsArrayExpr(Expr: TParamsExpr): TPasArrayType;
    function IsArrayOperatorAdd(Expr: TPasExpr): boolean;
    function IsTypeCast(Params: TParamsExpr): boolean;
    function IsInterfaceType(const ResolvedEl: TPasResolverResult;
      IntfType: TPasClassInterfaceType): boolean; overload;
    function IsInterfaceType(TypeEl: TPasType; IntfType: TPasClassInterfaceType): boolean; overload;
    function IsTGUID(RecTypeEl: TPasRecordType): boolean; virtual;
    function IsTGUIDString(const ResolvedEl: TPasResolverResult): boolean; virtual;
    function ProcNeedsParams(El: TPasProcedureType): boolean;
    function IsProcOverride(AncestorProc, DescendantProc: TPasProcedure): boolean;
    function GetTopLvlProc(El: TPasElement): TPasProcedure;
    function GetRangeLength(RangeExpr: TPasExpr): TMaxPrecInt;
    function EvalRangeLimit(RangeExpr: TPasExpr; Flags: TResEvalFlags;
      EvalLow: boolean; ErrorEl: TPasElement): TResEvalValue; virtual; // compute low() or high()
    function EvalTypeRange(Decl: TPasType; Flags: TResEvalFlags): TResEvalValue; virtual; // compute low() and high()
    function HasTypeInfo(El: TPasType): boolean; virtual;
    function GetActualBaseType(bt: TResolverBaseType): TResolverBaseType; virtual;
    function GetCombinedBoolean(Bool1, Bool2: TResolverBaseType; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function GetCombinedInt(const Int1, Int2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    procedure GetIntegerProps(bt: TResolverBaseType; out Precision: word; out Signed: boolean);
    function GetIntegerRange(bt: TResolverBaseType; out MinVal, MaxVal: TMaxPrecInt): boolean;
    function GetIntegerBaseType(Precision: word; Signed: boolean; ErrorEl: TPasElement): TResolverBaseType;
    function GetSmallestIntegerBaseType(MinVal, MaxVal: TMaxPrecInt): TResolverBaseType;
    function GetCombinedChar(const Char1, Char2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function GetCombinedString(const Str1, Str2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function IsElementSkipped(El: TPasElement): boolean; virtual;
    function FindLocalBuiltInSymbol(El: TPasElement): TPasElement; virtual;
    function GetLastSection: TPasSection;
  public
    // options
    property Options: TPasResolverOptions read FOptions write FOptions;
    property AnonymousElTypePostfix: String read FAnonymousElTypePostfix
      write FAnonymousElTypePostfix; // default empty, if set, anonymous element types are named ArrayName+Postfix and added to declarations
    property BaseTypes[bt: TResolverBaseType]: TPasUnresolvedSymbolRef read GetBaseTypes;
    property BaseTypeNames[bt: TResolverBaseType]: string read GetBaseTypeNames;
    property BaseTypeChar: TResolverBaseType read FBaseTypeChar write FBaseTypeChar;
    property BaseTypeExtended: TResolverBaseType read FBaseTypeExtended write FBaseTypeExtended;
    property BaseTypeString: TResolverBaseType read FBaseTypeString write FBaseTypeString;
    property BaseTypeLength: TResolverBaseType read FBaseTypeLength write FBaseTypeLength;
    property BuiltInProcs[bp: TResolverBuiltInProc]: TResElDataBuiltInProc read GetBuiltInProcs;
    property ExprEvaluator: TResExprEvaluator read fExprEvaluator;
    property DynArrayMinIndex: TMaxPrecInt read FDynArrayMinIndex write FDynArrayMinIndex;
    property DynArrayMaxIndex: TMaxPrecInt read FDynArrayMaxIndex write FDynArrayMaxIndex;
    // parsed values
    property DefaultNameSpace: String read FDefaultNameSpace;
    property RootElement: TPasModule read FRootElement write SetRootElement;
    property Step: TPasResolverStep read FStep;
    // scopes
    property StoreSrcColumns: boolean read FStoreSrcColumns write FStoreSrcColumns; {
       If true Line and Column is mangled together in TPasElement.SourceLineNumber.
       Use method UnmangleSourceLineNumber to extract. }
    property Scopes[Index: integer]: TPasScope read GetScopes;
    property ScopeCount: integer read FScopeCount;
    property TopScope: TPasScope read FTopScope;
    property DefaultScope: TPasDefaultScope read FDefaultScope write FDefaultScope;
    property ScopeClass_Class: TPasClassScopeClass read FScopeClass_Class write FScopeClass_Class;
    property ScopeClass_InitialFinalization: TPasInitialFinalizationScopeClass read FScopeClass_InitialFinalization write FScopeClass_InitialFinalization;
    property ScopeClass_Module: TPasModuleScopeClass read FScopeClass_Module write FScopeClass_Module;
    property ScopeClass_Procedure: TPasProcedureScopeClass read FScopeClass_Proc write FScopeClass_Proc;
    property ScopeClass_Section: TPasSectionScopeClass read FScopeClass_Section write FScopeClass_Section;
    property ScopeClass_WithExpr: TPasWithExprScopeClass read FScopeClass_WithExpr write FScopeClass_WithExpr;
    // last element
    property LastElement: TPasElement read FLastElement;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
    property LastMsgElement: TPasElement read FLastMsgElement write FLastMsgElement;
    property LastMsgId: TMaxPrecInt read FLastMsgId write FLastMsgId;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastSourcePos: TPasSourcePos read FLastSourcePos write FLastSourcePos;
  end;

function GetTreeDbg(El: TPasElement; Indent: integer = 0): string;
function GetResolverResultDbg(const T: TPasResolverResult): string;
function GetClassAncestorsDbg(El: TPasClassType): string;
function ResolverResultFlagsToStr(const Flags: TPasResolverResultFlags): string;
function GetElementTypeName(El: TPasElement): string; overload;
function GetElementTypeName(C: TPasElementBaseClass): string; overload;
function GetElementDbgPath(El: TPasElement): string; overload;
function ResolveSimpleAliasType(aType: TPasType): TPasType;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement;
  LoTypeEl, HiTypeEl: TPasType; Flags: TPasResolverResultFlags); overload;
procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType;
  Flags: TPasResolverResultFlags); overload;
procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags); overload;

function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
function ProcNeedsBody(Proc: TPasProcedure): boolean;
function ProcHasGroupOverload(Proc: TPasProcedure): boolean;
function ChompDottedIdentifier(const Identifier: string): string;
function FirstDottedIdentifier(const Identifier: string): string;
function IsDottedIdentifierPrefix(const Prefix, Identifier: string): boolean;
{$IF FPC_FULLVERSION<30101}
function IsValidIdent(const Ident: string; AllowDots: Boolean = False; StrictDots: Boolean = False): Boolean;
{$ENDIF}
function DotExprToName(Expr: TPasExpr): string;
function NoNil(o: TObject): TObject;

function dbgs(const Flags: TPasResolverComputeFlags): string; overload;
function dbgs(const a: TResolvedRefAccess): string; overload;
function dbgs(const Flags: TResolvedReferenceFlags): string; overload;
function dbgs(const a: TPSRefAccess): string; overload;

implementation

function GetTreeDbg(El: TPasElement; Indent: integer): string;

  procedure LineBreak(SubIndent: integer);
  begin
    Inc(Indent,SubIndent);
    Result:=Result+LineEnding+StringOfChar(' ',Indent);
  end;

var
  i, l: Integer;
begin
  if El=nil then exit('nil');
  Result:=El.Name+':'+El.ClassName+'=';
  if El is TPasExpr then
    begin
    if El.ClassType<>TBinaryExpr then
      Result:=Result+OpcodeStrings[TPasExpr(El).OpCode];
    if El.ClassType=TUnaryExpr then
      Result:=Result+GetTreeDbg(TUnaryExpr(El).Operand,Indent)
    else if El.ClassType=TBinaryExpr then
      Result:=Result+'Left={'+GetTreeDbg(TBinaryExpr(El).left,Indent)+'}'
         +OpcodeStrings[TPasExpr(El).OpCode]
         +'Right={'+GetTreeDbg(TBinaryExpr(El).right,Indent)+'}'
    else if El.ClassType=TPrimitiveExpr then
      Result:=Result+TPrimitiveExpr(El).Value
    else if El.ClassType=TBoolConstExpr then
      Result:=Result+BoolToStr(TBoolConstExpr(El).Value,'true','false')
    else if El.ClassType=TNilExpr then
      Result:=Result+'nil'
    else if El.ClassType=TInheritedExpr then
      Result:=Result+'inherited'
    else if El.ClassType=TSelfExpr then
      Result:=Result+'Self'
    else if El.ClassType=TParamsExpr then
      begin
      LineBreak(2);
      Result:=Result+GetTreeDbg(TParamsExpr(El).Value,Indent)+'(';
      l:=length(TParamsExpr(El).Params);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDbg(TParamsExpr(El).Params[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TRecordValues then
      begin
      Result:=Result+'(';
      l:=length(TRecordValues(El).Fields);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+TRecordValues(El).Fields[i].Name+':'
            +GetTreeDbg(TRecordValues(El).Fields[i].ValueExp,Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TArrayValues then
      begin
      Result:=Result+'[';
      l:=length(TArrayValues(El).Values);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDbg(TArrayValues(El).Values[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+']';
      end;
    end
  else if El is TPasProcedure then
    begin
    Result:=Result+GetTreeDbg(TPasProcedure(El).ProcType,Indent);
    end
  else if El is TPasProcedureType then
    begin
    if TPasProcedureType(El).IsReferenceTo then
      Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
    Result:=Result+'(';
    l:=TPasProcedureType(El).Args.Count;
    if l>0 then
      begin
      inc(Indent,2);
      for i:=0 to l-1 do
        begin
        LineBreak(0);
        Result:=Result+GetTreeDbg(TPasArgument(TPasProcedureType(El).Args[i]),Indent);
        if i<l-1 then
          Result:=Result+';'
        end;
      dec(Indent,2);
      end;
    Result:=Result+')';
    if El is TPasFunction then
      Result:=Result+':'+GetTreeDbg(TPasFunctionType(TPasFunction(El).ProcType).ResultEl,Indent);
    if TPasProcedureType(El).IsOfObject then
      Result:=Result+' '+ProcTypeModifiers[ptmOfObject];
    if TPasProcedureType(El).IsNested then
      Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
    if cCallingConventions[TPasProcedureType(El).CallingConvention]<>'' then
      Result:=Result+'; '+cCallingConventions[TPasProcedureType(El).CallingConvention];
    end
  else if El.ClassType=TPasResultElement then
    Result:=Result+GetTreeDbg(TPasResultElement(El).ResultType,Indent)
  else if El.ClassType=TPasArgument then
    begin
    if AccessNames[TPasArgument(El).Access]<>'' then
      Result:=Result+AccessNames[TPasArgument(El).Access];
    if TPasArgument(El).ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTreeDbg(TPasArgument(El).ArgType,Indent);
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    if El.CustomData is TResElDataBuiltInProc then
      Result:=Result+TResElDataBuiltInProc(TPasUnresolvedSymbolRef(El).CustomData).Signature;
    end;
end;

function GetResolverResultDbg(const T: TPasResolverResult): string;
var
  HiTypeEl: TPasType;
begin
  Result:='[bt='+ResBaseTypeNames[T.BaseType];
  if T.SubType<>btNone then
    Result:=Result+' Sub='+ResBaseTypeNames[T.SubType];
  Result:=Result
         +' Ident='+GetObjName(T.IdentEl);
  HiTypeEl:=ResolveSimpleAliasType(T.HiTypeEl);
  if HiTypeEl<>T.LoTypeEl then
    Result:=Result+' LoType='+GetObjName(T.LoTypeEl)+' HiTypeEl='+GetObjName(HiTypeEl)
  else
    Result:=Result+' Type='+GetObjName(T.LoTypeEl);
  Result:=Result
         +' Expr='+GetObjName(T.ExprEl)
         +' Flags='+ResolverResultFlagsToStr(T.Flags)
         +']';
end;

function GetClassAncestorsDbg(El: TPasClassType): string;

  function GetClassDesc(C: TPasClassType): string;
  var
    Module: TPasModule;
  begin
    if C.IsExternal then
      Result:='class external '
    else
      Result:='class ';
    Module:=C.GetModule;
    if Module<>nil then
      Result:=Result+Module.Name+'.';
    Result:=Result+GetElementDbgPath(C);
  end;

var
  Scope, AncestorScope: TPasClassScope;
  AncestorEl: TPasClassType;
begin
  if El=nil then exit('nil');
  Result:=GetClassDesc(El);
  if El.CustomData is TPasClassScope then
    begin
    Scope:=TPasClassScope(El.CustomData);
    AncestorScope:=Scope.AncestorScope;
    while AncestorScope<>nil do
      begin
      Result:=Result+LineEnding+'  ';
      AncestorEl:=NoNil(AncestorScope.Element) as TPasClassType;
      Result:=Result+GetClassDesc(AncestorEl);
      AncestorScope:=AncestorScope.AncestorScope;
      end;
    end;
end;

function ResolverResultFlagsToStr(const Flags: TPasResolverResultFlags): string;
var
  f: TPasResolverResultFlag;
  s: string;
begin
  Result:='';
  for f in Flags do
    begin
    if Result<>'' then Result:=Result+',';
    str(f,s);
    Result:=Result+s;
    end;
  Result:='['+Result+']';
end;

function GetElementTypeName(El: TPasElement): string;
var
  C: TClass;
begin
  if El=nil then
    exit('?');
  C:=El.ClassType;
  if C=TPrimitiveExpr then
    Result:=ExprKindNames[TPrimitiveExpr(El).Kind]
  else if C=TUnaryExpr then
    Result:='unary '+OpcodeStrings[TUnaryExpr(El).OpCode]
  else if C=TBinaryExpr then
    Result:=ExprKindNames[TBinaryExpr(El).Kind]
  else if C=TPasClassType then
    Result:=ObjKindNames[TPasClassType(El).ObjKind]
  else
    begin
    Result:=GetElementTypeName(TPasElementBaseClass(C));
    if Result='' then
      Result:=El.ElementTypeName;
    end;
end;

function GetElementTypeName(C: TPasElementBaseClass): string;
begin
  if C=nil then
    exit('nil');
  if C=TPrimitiveExpr then
    Result:='primitive expression'
  else if C=TUnaryExpr then
    Result:='unary expression'
  else if C=TBinaryExpr then
    Result:='binary expression'
  else if C=TBoolConstExpr then
    Result:='boolean const'
  else if C=TNilExpr then
    Result:='nil'
  else if C=TPasAliasType then
    Result:='alias'
  else if C=TPasPointerType then
    Result:='pointer'
  else if C=TPasTypeAliasType then
    Result:='type alias'
  else if C=TPasClassOfType then
    Result:='class of'
  else if C=TPasSpecializeType then
    Result:='specialize'
  else if C=TInlineSpecializeExpr then
    Result:='inline-specialize'
  else if C=TPasRangeType then
    Result:='range'
  else if C=TPasArrayType then
    Result:='array'
  else if C=TPasFileType then
    Result:='file'
  else if C=TPasEnumValue then
    Result:='enum value'
  else if C=TPasEnumType then
    Result:='enum type'
  else if C=TPasSetType then
    Result:='set'
  else if C=TPasRecordType then
    Result:='record'
  else if C=TPasClassType then
    Result:='class'
  else if C=TPasArgument then
    Result:='parameter'
  else if C=TPasProcedureType then
    Result:='procedural type'
  else if C=TPasResultElement then
    Result:='function result'
  else if C=TPasFunctionType then
    Result:='functional type'
  else if C=TPasStringType then
    Result:='string[]'
  else if C=TPasVariable then
    Result:='var'
  else if C=TPasExportSymbol then
    Result:='export'
  else if C=TPasConst then
    Result:='const'
  else if C=TPasProperty then
    Result:='property'
  else if C=TPasProcedure then
    Result:='procedure'
  else if C=TPasFunction then
    Result:='function'
  else if C=TPasOperator then
    Result:='operator'
  else if C=TPasClassOperator then
    Result:='class operator'
  else if C=TPasConstructor then
    Result:='constructor'
  else if C=TPasClassConstructor then
    Result:='class constructor'
  else if C=TPasDestructor then
    Result:='destructor'
  else if C=TPasClassDestructor then
    Result:='class destructor'
  else if C=TPasClassProcedure then
    Result:='class procedure'
  else if C=TPasClassFunction then
    Result:='class function'
  else if C=TPasMethodResolution then
    Result:='method resolution'
  else if C=TInterfaceSection then
    Result:='interfacesection'
  else if C=TImplementationSection then
    Result:='implementation'
  else if C=TProgramSection then
    Result:='program section'
  else if C=TLibrarySection then
    Result:='library section'
  else
    Result:=C.ClassName;
end;

function GetElementDbgPath(El: TPasElement): string;
begin
  if El=nil then exit('nil');
  Result:='';
  while El<>nil do
    begin
    if Result<>'' then Result:='.'+Result;
    if El.Name<>'' then
      Result:=El.Name+Result
    else
      Result:=GetElementTypeName(El)+Result;
    El:=El.Parent;
    end;
end;

function ResolveSimpleAliasType(aType: TPasType): TPasType;
var
  C: TClass;
begin
  while aType<>nil do
    begin
    C:=aType.ClassType;
    if (C=TPasAliasType) then
      aType:=TPasAliasType(aType).DestType
    else if (C=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData is TResolvedReference) then
      aType:=NoNil(TResolvedReference(aType.CustomData).Declaration) as TPasType
    else
      exit(aType);
    end;
  Result:=nil;
end;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement; LoTypeEl,
  HiTypeEl: TPasType; Flags: TPasResolverResultFlags);
begin
  if IdentEl is TPasExpr then
    raise Exception.Create('20170729101017');
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=IdentEl;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=ExprEl;
  ResolvedType.Flags:=Flags;
end;

function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
begin
  Result:=true;
  if Proc.IsExternal then exit(false);
  if Proc.IsForward then exit;
  if Proc.Parent.ClassType=TInterfaceSection then exit;
  if Proc.Parent.ClassType=TPasClassType then
    begin
    // a method declaration
    if not Proc.IsAbstract then exit;
    end;
  Result:=false;
end;

function ProcNeedsBody(Proc: TPasProcedure): boolean;
var
  C: TClass;
begin
  if Proc.IsForward or Proc.IsExternal then exit(false);
  C:=Proc.Parent.ClassType;
  if (C=TInterfaceSection) or C.InheritsFrom(TPasClassType) then exit(false);
  Result:=true;
end;

function ProcHasGroupOverload(Proc: TPasProcedure): boolean;
var
  Data: TObject;
begin
  if Proc.IsOverload then
    exit(true);
  Data:=Proc.CustomData;
  Result:=(Data is TPasProcedureScope)
    and (ppsfIsGroupOverload in TPasProcedureScope(Data).Flags);
end;

function ChompDottedIdentifier(const Identifier: string): string;
var
  p: Integer;
begin
  Result:=Identifier;
  p:=length(Identifier);
  while (p>0) do
    begin
    if Identifier[p]='.' then
      break;
    dec(p);
    end;
  Result:=LeftStr(Identifier,p-1);
end;

function FirstDottedIdentifier(const Identifier: string): string;
var
  p: SizeInt;
begin
  p:=Pos('.',Identifier);
  if p<1 then
    Result:=Identifier
  else
    Result:=LeftStr(Identifier,p-1);
end;

function IsDottedIdentifierPrefix(const Prefix, Identifier: string): boolean;
var
  l: Integer;
begin
  l:=length(Prefix);
  if (l>length(Identifier))
      or (CompareText(Prefix,LeftStr(Identifier,l))<>0) then
    exit(false);
  Result:=(length(Identifier)=l) or (Identifier[l+1]='.');
end;

function DotExprToName(Expr: TPasExpr): string;
var
  C: TClass;
  Prim: TPrimitiveExpr;
  Bin: TBinaryExpr;
  s: String;
begin
  Result:='';
  if Expr=nil then exit;
  C:=Expr.ClassType;
  if C=TPrimitiveExpr then
    begin
    Prim:=TPrimitiveExpr(Expr);
    case Prim.Kind of
      pekIdent,pekString: Result:=Prim.Value;
      pekSelf: Result:='Self';
    else
      EPasResolve.Create('[20180309155400] DotExprToName '+GetObjName(Prim)+' '+ExprKindNames[Prim.Kind]);
    end;
    end
  else if C=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Expr);
    if Bin.OpCode=eopSubIdent then
      begin
      Result:=DotExprToName(Bin.left);
      if Result='' then exit;
      s:=DotExprToName(Bin.right);
      if s='' then exit('');
      Result:=Result+'.'+s;
      end;
    end;
end;

function NoNil(o: TObject): TObject;
begin
  if o=nil then
    raise Exception.Create('');
  Result:=o;
end;

{$IF FPC_FULLVERSION<30101}
function IsValidIdent(const Ident: string; AllowDots: Boolean;
  StrictDots: Boolean): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  for I := 1 to Len do
  begin
    if First then
    begin
      Result := Ident[I] in Alpha;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        Result := I < Len;
        First := True;
      end;
    end
    else
      Result := Ident[I] in AlphaNum;
    if not Result then
      Break;
  end;
end;
{$ENDIF}

function dbgs(const Flags: TPasResolverComputeFlags): string;
var
  s: string;
  f: TPasResolverComputeFlag;
begin
  Result:='';
  for f in Flags do
    if f in Flags then
      begin
      if Result<>'' then Result:=Result+',';
      str(f,s);
      Result:=Result+s;
      end;
  Result:='['+Result+']';
end;

function dbgs(const a: TResolvedRefAccess): string;
begin
  str(a,Result);
end;

function dbgs(const Flags: TResolvedReferenceFlags): string;
var
  s: string;
  f: TResolvedReferenceFlag;
begin
  Result:='';
  for f in Flags do
    if f in Flags then
      begin
      if Result<>'' then Result:=Result+',';
      str(f,s);
      Result:=Result+s;
      end;
  Result:='['+Result+']';
end;

function dbgs(const a: TPSRefAccess): string;
begin
  str(a,Result);
end;

{$ifdef pas2js}
{ TPasResHashList }

constructor TPasResHashList.Create;
begin
  FItems:=TJSObject.new;
end;

procedure TPasResHashList.Add(const aName: string; Item: Pointer);
begin
  FItems['%'+aName]:=Item;
end;

function TPasResHashList.Find(const aName: string): Pointer;
begin
  if FItems.hasOwnProperty('%'+aName) then
    Result:=Pointer(FItems['%'+aName])
  else
    Result:=nil;
end;

procedure TPasResHashList.ForEachCall(const Proc: TPasResIterate; Arg: Pointer);
var
  key: string;
begin
  for key in FItems do
    if FItems.hasOwnProperty(key) then
      Proc(Pointer(FItems[key]),Arg);
end;

procedure TPasResHashList.Clear;
begin
  FItems:=TJSObject.new;
end;

procedure TPasResHashList.Remove(const aName: string);
begin
  if FItems.hasOwnProperty('%'+aName) then
    JSDelete(FItems,'%'+aName);
end;
{$endif}

{ TResElDataBuiltInProc }

destructor TResElDataBuiltInProc.Destroy;
begin
  ReleaseAndNil(TPasElement(Proc){$IFDEF CheckPasTreeRefCount},'TResElDataBuiltInProc.Proc'{$ENDIF});
  inherited Destroy;
end;

{ TPasClassIntfMap }

destructor TPasClassIntfMap.Destroy;
begin
  Element:=nil;
  Intf:=nil;
  FreeAndNil(Procs);
  FreeAndNil(AncestorMap);
  inherited Destroy;
end;

{ TPasInitialFinalizationScope }

function TPasInitialFinalizationScope.AddReference(El: TPasElement;
  Access: TPSRefAccess): TPasScopeReference;
begin
  if References=nil then
    References:=TPasScopeReferences.Create(Self);
  Result:=References.Add(El,Access);
end;

destructor TPasInitialFinalizationScope.Destroy;
begin
  FreeAndNil(References);
  inherited Destroy;
end;

{ TPasScopeReference }

procedure TPasScopeReference.SetElement(const AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if FElement<>nil then
    FElement.Release{$IFDEF CheckPasTreeRefCount}('TPasScopeReference.SetElement'){$ENDIF};
  FElement:=AValue;
  if FElement<>nil then
    FElement.AddRef{$IFDEF CheckPasTreeRefCount}('TPasScopeReference.SetElement'){$ENDIF};
end;

destructor TPasScopeReference.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcScopeReference.Destroy START ',ClassName,' "',GetObjName(Element),'"');
  {$ENDIF}
  Element:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcScopeReference.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasScopeReferences }

procedure TPasScopeReferences.OnClearItem(Item, Dummy: pointer);
var
  Ref: TPasScopeReference absolute Item;
  Ref2: TPasScopeReference;
begin
  if Dummy=nil then ;
  //writeln('TPasProcedureScope.OnClearReferenceItem ',GetObjName(Ref.Element));
  while Ref<>nil do
    begin
    Ref2:=Ref;
    Ref:=Ref.NextSameName;
    Ref2.Free;
    end;
end;

procedure TPasScopeReferences.OnCollectItem(Item, aList: pointer);
var
  Ref: TPasScopeReference absolute Item;
  List: TFPList absolute aList;
begin
  while Ref<>nil do
    begin
    List.Add(Ref);
    Ref:=Ref.NextSameName;
    end;
end;

constructor TPasScopeReferences.Create(aScope: TPasScope);
begin
  References:=TPasResHashList.Create;
  FScope:=aScope;
end;

destructor TPasScopeReferences.Destroy;
begin
  Clear;
  {$ifdef pas2js}
  References:=nil;
  {$else}
  FreeAndNil(References);
  {$endif}
  inherited Destroy;
end;

procedure TPasScopeReferences.Clear;
begin
  if References=nil then exit;
  References.ForEachCall(@OnClearItem,nil);
  References.Clear;
end;

function TPasScopeReferences.Add(El: TPasElement; Access: TPSRefAccess
  ): TPasScopeReference;
var
  LoName: String;
  OldItem, Item, LastItem: TPasScopeReference;
begin
  LoName:=lowercase(El.Name);
  OldItem:=TPasScopeReference(References.Find(LoName));
  Item:=OldItem;
  LastItem:=nil;
  while Item<>nil do
    begin
    if Item.Element=El then
      begin
      // already marked as used -> combine access
      case Access of
      psraNone: ;
      psraRead:
        case Item.Access of
          psraNone: Item.Access:=Access;
          //psraRead: ;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraWrite:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          //psraWrite: ;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraReadWrite:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraWriteRead:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraTypeInfo: Item.Access:=psraTypeInfo;
      else
        raise EPasResolve.Create(GetObjName(El)+' unknown Access');
      end;
      exit(Item);
      end;
    LastItem:=Item;
    Item:=Item.NextSameName;
    end;
  // new reference
  Item:=TPasScopeReference.Create;
  Item.Element:=El;
  Item.Access:=Access;
  if LastItem=nil then
    begin
    References.Add(LoName,Item);
    {$IFDEF VerbosePCUFiler}
    if TPasScopeReference(References.Find(LoName))<>Item then
      raise EPasResolve.Create('20180219230028');
    {$ENDIF}
    end
  else
    LastItem.NextSameName:=Item;
  Result:=Item;
end;

function TPasScopeReferences.Find(const aName: string): TPasScopeReference;
var
  LoName: String;
begin
  if References=nil then exit(nil);
  LoName:=lowercase(aName);
  Result:=TPasScopeReference(References.Find(LoName));
end;

function TPasScopeReferences.GetList: TFPList;
begin
  Result:=TFPList.Create;
  if References=nil then exit;
  References.ForEachCall(@OnCollectItem,Result);
end;

{ TPasPropertyScope }

destructor TPasPropertyScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy START ',ClassName);
  {$ENDIF}
  AncestorProp:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy END',ClassName);
  {$ENDIF}
end;

{ TPasEnumTypeScope }

destructor TPasEnumTypeScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy START ',ClassName);
  {$ENDIF}
  ReleaseAndNil(TPasElement(CanonicalSet){$IFDEF CheckPasTreeRefCount},'TPasEnumTypeScope.CanonicalSet'{$ENDIF});
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasDotIdentifierScope }

function TPasDotIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=IdentifierScope.FindIdentifier(Identifier);
end;

procedure TPasDotIdentifierScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  IdentifierScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  IdentifierScope.WriteIdentifiers(Prefix);
end;

{ TPasWithExprScope }

class function TPasWithExprScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

class function TPasWithExprScope.FreeOnPop: boolean;
begin
  Result:=false;
end;

procedure TPasWithExprScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  Scope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasWithExprScope.WriteIdentifiers(Prefix: string);
begin
  {AllowWriteln}
  writeln(Prefix+'WithExpr: '+GetTreeDbg(Expr,length(Prefix)));
  Scope.WriteIdentifiers(Prefix);
  {AllowWriteln-}
end;

{ TPasWithScope }

constructor TPasWithScope.Create;
begin
  inherited Create;
  ExpressionScopes:=TObjectList.Create(true);
end;

destructor TPasWithScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(ExpressionScopes);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasProcedureScope }

function TPasProcedureScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  CurScope: TPasIdentifierScope;
  ParentEl: TPasElement;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then exit;
  CurScope:=ClassScope;
  if CurScope=nil then exit;
  repeat
    Result:=CurScope.FindIdentifier(Identifier);
    if Result<>nil then exit;
    ParentEl:=TPasElement(CurScope.Element.Parent);
    if ParentEl=nil then exit;
    if (ParentEl.ClassType=TPasClassType) then
      CurScope:=TPasClassScope(ParentEl.CustomData)
    else if (ParentEl.ClassType=TPasRecordType) then
      CurScope:=TPasRecordScope(ParentEl.CustomData)
    else
      exit;
  until false;
end;

procedure TPasProcedureScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  CurScope: TPasIdentifierScope;
  ParentEl: TPasElement;
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  CurScope:=ClassScope;
  if CurScope=nil then exit;
  repeat
    CurScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
    if Abort then exit;
    ParentEl:=TPasElement(CurScope.Element.Parent);
    if ParentEl=nil then exit;
    if (ParentEl.ClassType=TPasClassType) then
      CurScope:=TPasClassScope(ParentEl.CustomData)
    else if (ParentEl.ClassType=TPasRecordType) then
      CurScope:=TPasRecordScope(ParentEl.CustomData)
    else
      exit;
  until false;
end;

function TPasProcedureScope.GetSelfScope: TPasProcedureScope;
var
  Proc: TPasProcedure;
begin
  Result:=Self;
  repeat
    if Result.ClassScope<>nil then exit;
    Proc:=TPasProcedure(Element);
    if not (Proc.Parent is TProcedureBody) then exit(nil);
    Proc:=Proc.Parent.Parent as TPasProcedure;
    Result:=TPasProcedureScope(Proc.CustomData);
  until false;
end;

procedure TPasProcedureScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  if ClassScope<>nil then
    ClassScope.WriteIdentifiers(Prefix+'CS  ');
end;

destructor TPasProcedureScope.Destroy;
begin
  FreeAndNil(References);
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy START ',ClassName);
  {$ENDIF}
  inherited Destroy;
  ReleaseAndNil(TPasElement(SelfArg){$IFDEF CheckPasTreeRefCount},'TPasProcedureScope.SelfArg'{$ENDIF});
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasProcedureScope.AddReference(El: TPasElement; Access: TPSRefAccess
  ): TPasScopeReference;
begin
  if References=nil then
    References:=TPasScopeReferences.Create(Self);
  Result:=References.Add(El,Access);
end;

function TPasProcedureScope.GetReferences: TFPList;
begin
  if References=nil then
    Result:=TFPList.Create
  else
    Result:=References.GetList;
end;

{ TPasClassScope }

destructor TPasClassScope.Destroy;
var
  i: Integer;
  o: TObject;
begin
  if Interfaces<>nil then
    begin
    for i:=0 to Interfaces.Count-1 do
      begin
      o:=TObject(Interfaces[i]);
      if o=nil then
      else if o is TPasProperty then
      else if o is TPasClassIntfMap then
        o.Free
      else
        raise Exception.Create('[20180322132757] '+GetElementDbgPath(TPasElement(Element))+' i='+IntToStr(i)+' '+GetObjName(o));
      end;
    FreeAndNil(Interfaces);
    end;
  if CanonicalClassOf<>nil then
    begin
    CanonicalClassOf.Parent:=nil;
    ReleaseAndNil(TPasElement(CanonicalClassOf){$IFDEF CheckPasTreeRefCount},'TPasClassScope.CanonicalClassOf'{$ENDIF});
    end;
  inherited Destroy;
end;

function TPasClassScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then exit;
  if AncestorScope<>nil then
    Result:=AncestorScope.FindIdentifier(Identifier);
end;

procedure TPasClassScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  if AncestorScope<>nil then
    AncestorScope.IterateElements(aName,StartScope,OnIterateElement,Data,Abort);
end;

procedure TPasClassScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  if AncestorScope<>nil then
    AncestorScope.WriteIdentifiers(Prefix+'AS  ');
end;

{ TPasDotClassScope }

procedure TPasDotClassScope.SetClassScope(AValue: TPasClassScope);
begin
  if FClassScope=AValue then Exit;
  FClassScope:=AValue;
  IdentifierScope:=AValue;
end;

{ TPasIdentifier }

procedure TPasIdentifier.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release{$IFDEF CheckPasTreeRefCount}('TPasIdentifier.SetElement'){$ENDIF};
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef{$IFDEF CheckPasTreeRefCount}('TPasIdentifier.SetElement'){$ENDIF};
end;

destructor TPasIdentifier.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy START ',ClassName,' "',Identifier,'"');
  {$ENDIF}
  Element:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy END ',ClassName);
  {$ENDIF}
end;

{ EPasResolve }

procedure EPasResolve.SetPasElement(AValue: TPasElement);
begin
  if FPasElement=AValue then Exit;
  if PasElement<>nil then
    PasElement.Release{$IFDEF CheckPasTreeRefCount}('EPasResolve.SetPasElement'){$ENDIF};
  FPasElement:=AValue;
  if PasElement<>nil then
    PasElement.AddRef{$IFDEF CheckPasTreeRefCount}('EPasResolve.SetPasElement'){$ENDIF};
end;

destructor EPasResolve.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy START ',ClassName);
  {$ENDIF}
  PasElement:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TResolvedReference }

procedure TResolvedReference.SetDeclaration(AValue: TPasElement);
begin
  if FDeclaration=AValue then Exit;
  if Declaration<>nil then
    Declaration.Release{$IFDEF CheckPasTreeRefCount}('TResolvedReference.SetDeclaration'){$ENDIF};
  FDeclaration:=AValue;
  if Declaration<>nil then
    Declaration.AddRef{$IFDEF CheckPasTreeRefCount}('TResolvedReference.SetDeclaration'){$ENDIF};
end;

destructor TResolvedReference.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy START ',ClassName);
  {$ENDIF}
  Declaration:=nil;
  FreeAndNil(Context);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasSubScope }

class function TPasSubScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasModuleDotScope }

procedure TPasModuleDotScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if (El.ClassType=TPasModule) or (El.ClassType=TPasUsesUnit) then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

procedure TPasModuleDotScope.SetModule(AValue: TPasModule);
begin
  if FModule=AValue then Exit;
  if Module<>nil then
    Module.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleDotScope.SetModule'){$ENDIF};
  FModule:=AValue;
  if Module<>nil then
    Module.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleDotScope.SetModule'){$ENDIF};
end;

destructor TPasModuleDotScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy START ',ClassName);
  {$ENDIF}
  Module:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasModuleDotScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;

  function Find(Scope: TPasIdentifierScope): boolean;
  var
    Found: TPasIdentifier;
    C: TClass;
  begin
    if Scope=nil then exit(false);
    Found:=Scope.FindLocalIdentifier(Identifier);
    FindIdentifier:=Found;
    if Found=nil then exit(false);
    C:=Found.Element.ClassType;
    Result:=(C<>TPasModule) and (C<>TPasUsesUnit);
  end;

begin
  Result:=nil;
  if Find(ImplementationScope) then exit;
  if Find(InterfaceScope) then exit;
  Find(SystemScope);
end;

procedure TPasModuleDotScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  FilterData: TPasIterateFilterData;

  function Iterate(Scope: TPasIdentifierScope): boolean;
  begin
    if Scope=nil then exit(false);
    Scope.IterateLocalElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
    Result:=Abort;
  end;

begin
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  if Iterate(ImplementationScope) then exit;
  if Iterate(InterfaceScope) then exit;
  Iterate(SystemScope);
end;

procedure TPasModuleDotScope.WriteIdentifiers(Prefix: string);
begin
  if ImplementationScope<>nil then
    ImplementationScope.WriteIdentifiers(Prefix+'  ');
  if InterfaceScope<>nil then
    InterfaceScope.WriteIdentifiers(Prefix+'  ');
  if SystemScope<>nil then
    SystemScope.WriteIdentifiers(Prefix+'  ');
end;

{ TPasSectionScope }

procedure TPasSectionScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if (El.ClassType=TPasModule) or (El.ClassType=TPasUsesUnit) then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

constructor TPasSectionScope.Create;
begin
  inherited Create;
  UsesScopes:=TFPList.Create;
end;

destructor TPasSectionScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(UsesScopes);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasSectionScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
  C: TClass;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then
    exit;
  for i:=UsesScopes.Count-1 downto 0 do
    begin
    UsesScope:=TPasIdentifierScope(UsesScopes[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.FindIdentifier "',Identifier,'" in used unit ',GetObjName(UsesScope.Element));
    {$ENDIF}
    Result:=UsesScope.FindLocalIdentifier(Identifier);
    if Result<>nil then
      begin
      C:=Result.Element.ClassType;
      if (C<>TPasModule) and (C<>TPasUsesUnit) then
        exit;
      end;
    end;
end;

procedure TPasSectionScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  i: Integer;
  UsesScope: TPasSectionScope;
  FilterData: TPasIterateFilterData;
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  for i:=UsesScopes.Count-1 downto 0 do
    begin
    UsesScope:=TPasSectionScope(UsesScopes[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.IterateElements "',aName,'" in used unit ',UsesScope.Element.ParentPath,':',GetObjName(UsesScope.Element));
    {$ENDIF}
    UsesScope.IterateLocalElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
    if Abort then exit;
    end;
end;

procedure TPasSectionScope.WriteIdentifiers(Prefix: string);
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
  SubPrefix: String;
begin
  {AllowWriteln}
  inherited WriteIdentifiers(Prefix);
  SubPrefix:=Prefix+'    ';
  for i:=UsesScopes.Count-1 downto 0 do
    begin
    UsesScope:=TPasIdentifierScope(UsesScopes[i]);
    writeln(Prefix+'  Uses: '+GetObjName(UsesScope.Element)+' "'+UsesScope.Element.GetModule.Name+'"');
    UsesScope.FItems.ForEachCall(@OnWriteItem,Pointer(SubPrefix));
    end;
  {AllowWriteln-}
end;

{ TPasModuleScope }

procedure TPasModuleScope.SetAssertClass(const AValue: TPasClassType);
begin
  if FAssertClass=AValue then Exit;
  if FAssertClass<>nil then
    FAssertClass.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertClass'){$ENDIF};
  FAssertClass:=AValue;
  if FAssertClass<>nil then
    FAssertClass.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertClass'){$ENDIF};
end;

procedure TPasModuleScope.SetAssertDefConstructor(const AValue: TPasConstructor
  );
begin
  if FAssertDefConstructor=AValue then Exit;
  if FAssertDefConstructor<>nil then
    FAssertDefConstructor.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertDefConstructor'){$ENDIF};
  FAssertDefConstructor:=AValue;
  if FAssertDefConstructor<>nil then
    FAssertDefConstructor.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertDefConstructor'){$ENDIF};
end;

procedure TPasModuleScope.SetAssertMsgConstructor(const AValue: TPasConstructor
  );
begin
  if FAssertMsgConstructor=AValue then Exit;
  if FAssertMsgConstructor<>nil then
    FAssertMsgConstructor.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertMsgConstructor'){$ENDIF};
  FAssertMsgConstructor:=AValue;
  if FAssertMsgConstructor<>nil then
    FAssertMsgConstructor.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetAssertMsgConstructor'){$ENDIF};
end;

procedure TPasModuleScope.SetRangeErrorClass(const AValue: TPasClassType);
begin
  if FRangeErrorClass=AValue then Exit;
  if FRangeErrorClass<>nil then
    FRangeErrorClass.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetRangeErrorClass'){$ENDIF};
  FRangeErrorClass:=AValue;
  if FRangeErrorClass<>nil then
    FRangeErrorClass.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetRangeErrorClass'){$ENDIF};
end;

procedure TPasModuleScope.SetRangeErrorConstructor(const AValue: TPasConstructor
  );
begin
  if FRangeErrorConstructor=AValue then Exit;
  if FRangeErrorConstructor<>nil then
    FRangeErrorConstructor.Release{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetRangeErrorConstructor'){$ENDIF};
  FRangeErrorConstructor:=AValue;
  if FRangeErrorConstructor<>nil then
    FRangeErrorConstructor.AddRef{$IFDEF CheckPasTreeRefCount}('TPasModuleScope.SetRangeErrorConstructor'){$ENDIF};
end;

constructor TPasModuleScope.Create;
begin
  inherited Create;
  PendingResolvers:=TFPList.Create;
end;

destructor TPasModuleScope.Destroy;
begin
  AssertClass:=nil;
  AssertDefConstructor:=nil;
  AssertMsgConstructor:=nil;
  RangeErrorClass:=nil;
  RangeErrorConstructor:=nil;
  FreeAndNil(PendingResolvers);
  inherited Destroy;
end;

procedure TPasModuleScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  if CompareText(aName,FirstName)<>0 then exit;
  OnIterateElement(TPasElement(Element),Self,StartScope,Data,Abort);
end;

{ TPasDefaultScope }

class function TPasDefaultScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasScope }

class function TPasScope.IsStoredInElement: boolean;
begin
  Result:=true;
end;

class function TPasScope.FreeOnPop: boolean;
begin
  Result:=not IsStoredInElement;
end;

procedure TPasScope.IterateElements(const aName: string; StartScope: TPasScope;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
begin
  if aName='' then ;
  if StartScope=nil then ;
  if Data=nil then ;
  if OnIterateElement=nil then ;
  if Abort then ;
end;

procedure TPasScope.WriteIdentifiers(Prefix: string);
begin
  {AllowWriteln}
  writeln(Prefix,'(',ClassName,') Element: ',GetObjName(Element));
  {AllowWriteln-}
end;

{ TPasIdentifierScope }

// inline
function TPasIdentifierScope.FindLocalIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=TPasIdentifier(FItems.Find(lowercase(Identifier)));
end;

procedure TPasIdentifierScope.OnClearItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Ident: TPasIdentifier;
begin
  if Dummy=nil then ;
  //writeln('TPasIdentifierScope.OnClearItem ',PasIdentifier.Identifier+':'+PasIdentifier.ClassName);
  while PasIdentifier<>nil do
    begin
    Ident:=PasIdentifier;
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    Ident.Free;
    end;
end;

procedure TPasIdentifierScope.OnCollectItem(Item, List: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  FPList: TFPList absolute List;
begin
  FPList.Add(PasIdentifier);
end;

procedure TPasIdentifierScope.OnWriteItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Prefix: String;
begin
  {AllowWriteln}
  Prefix:=String(Dummy);
  while PasIdentifier<>nil do
    begin
    writeln(Prefix,'Identifier="',PasIdentifier.Identifier,'" Element=',GetObjName(PasIdentifier.Element));
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    end;
  {AllowWriteln-}
end;

procedure TPasIdentifierScope.InternalAdd(Item: TPasIdentifier);
var
  OldItem: TPasIdentifier;
  LoName: string;
  {$ifdef pas2js}
  {$ELSE}
  Index: Integer;
  {$ENDIF}
begin
  LoName:=lowercase(Item.Identifier);
  {$ifdef pas2js}
  OldItem:=TPasIdentifier(FItems.Find(LoName));
  if OldItem<>nil then
    begin
    // insert LIFO - last in, first out
    Item.NextSameIdentifier:=OldItem;
    end;
  FItems.Add(LoName,Item);
  {$IFDEF VerbosePasResolver}
  if Item.Owner<>nil then
    raise Exception.Create('20160925184110');
  Item.Owner:=Self;
  {$ENDIF}
  {$IFDEF VerbosePasResolver}
  if FindIdentifier(Item.Identifier)<>Item then
    raise Exception.Create('20181018173201');
  {$ENDIF}
  {$else}
  Index:=FItems.FindIndexOf(LoName);
  {$IFDEF VerbosePasResolver}
  if Item.Owner<>nil then
    raise Exception.Create('20160925184110');
  Item.Owner:=Self;
  {$ENDIF}
  //writeln('  Index=',Index);
  if Index>=0 then
    begin
    // insert LIFO - last in, first out
    OldItem:=TPasIdentifier(FItems.List^[Index].Data);
    {$IFDEF VerbosePasResolver}
    if lowercase(OldItem.Identifier)<>LoName then
      raise Exception.Create('20160925183438');
    {$ENDIF}
    Item.NextSameIdentifier:=OldItem;
    FItems.List^[Index].Data:=Item;
    end
  else
    begin
    FItems.Add(LoName, Item);
    {$IFDEF VerbosePasResolver}
    if FindIdentifier(Item.Identifier)<>Item then
      raise Exception.Create('20160925183849');
    {$ENDIF}
    end;
  {$endif}
end;

constructor TPasIdentifierScope.Create;
begin
  FItems:=TPasResHashList.Create;
end;

destructor TPasIdentifierScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Destroy START ',ClassName);
  {$ENDIF}
  FItems.ForEachCall(@OnClearItem,nil);
  {$ifdef pas2js}
  FItems:=nil;
  {$else}
  FItems.Clear;
  FreeAndNil(FItems);
  {$endif}
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=FindLocalIdentifier(Identifier);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  if (Result<>nil) and (Result.Owner<>Self) then
    begin
    writeln('TPasIdentifierScope.FindIdentifier Result.Owner<>Self Owner='+GetObjName(Result.Owner));
    raise Exception.Create('20160925184159');
    end;
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasIdentifierScope.RemoveLocalIdentifier(El: TPasElement): boolean;
var
  Identifier, PrevIdentifier: TPasIdentifier;
  LoName: string;
begin
  LoName:=lowercase(El.Name);
  Identifier:=TPasIdentifier(FItems.Find(LoName));
  FindLocalIdentifier(El.Name);
  PrevIdentifier:=nil;
  Result:=false;
  while Identifier<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    if (Identifier.Owner<>Self) then
      raise Exception.Create('20160925184159');
    {$ENDIF}
    if Identifier.Element=El then
      begin
      if PrevIdentifier<>nil then
        begin
        PrevIdentifier.NextSameIdentifier:=Identifier.NextSameIdentifier;
        Identifier.Free;
        Identifier:=PrevIdentifier.NextSameIdentifier;
        end
      else
        begin
        FItems.Remove({$ifdef pas2js}LoName{$else}Identifier{$endif});
        PrevIdentifier:=Identifier;
        Identifier:=Identifier.NextSameIdentifier;
        PrevIdentifier.Free;
        PrevIdentifier:=nil;
        if Identifier<>nil then
          FItems.Add(LoName,Identifier);
        end;
      Result:=true;
      continue;
      end;
    PrevIdentifier:=Identifier;
    Identifier:=Identifier.NextSameIdentifier;
    end;
end;

function TPasIdentifierScope.AddIdentifier(const Identifier: String;
  El: TPasElement; const Kind: TPasIdentifierKind): TPasIdentifier;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.AddIdentifier Identifier="',Identifier,'" El=',GetObjName(El));
  Item:=TPasIdentifier.Create;
  Item.Identifier:=Identifier;
  Item.Element:=El;
  Item.Kind:=Kind;

  InternalAdd(Item);
  //writeln('TPasIdentifierScope.AddIdentifier END');
  Result:=Item;
end;

function TPasIdentifierScope.FindElement(const aName: string): TPasElement;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.FindElement "',aName,'"');
  Item:=FindIdentifier(aName);
  if Item=nil then
    Result:=nil
  else
    Result:=Item.Element;
  //writeln('TPasIdentifierScope.FindElement Found="',GetObjName(Result),'"');
end;

procedure TPasIdentifierScope.IterateLocalElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  Item: TPasIdentifier;
  {$IFDEF VerbosePasResolver}
  OldElement: TPasElement;
  {$ENDIF}
begin
  Item:=FindLocalIdentifier(aName);
  while Item<>nil do
    begin
    //writeln('TPasIdentifierScope.IterateLocalElements ',ClassName,' ',Item.Identifier,' ',GetObjName(Item.Element));
    {$IFDEF VerbosePasResolver}
    OldElement:=Item.Element;
    {$ENDIF}
    OnIterateElement(Item.Element,Self,StartScope,Data,Abort);
    {$IFDEF VerbosePasResolver}
    if OldElement<>Item.Element then
      raise Exception.Create('20160925183503');
    {$ENDIF}
    if Abort then exit;
    Item:=Item.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  IterateLocalElements(aName,StartScope,OnIterateElement,Data,Abort);
end;

procedure TPasIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  WriteLocalIdentifiers(Prefix+'  ');
end;

procedure TPasIdentifierScope.WriteLocalIdentifiers(Prefix: string);
begin
  FItems.ForEachCall(@OnWriteItem,Pointer(Prefix));
end;

function TPasIdentifierScope.GetLocalIdentifiers: TFPList;
begin
  Result:=TFPList.Create;
  FItems.ForEachCall(@OnCollectItem,Result);
end;

{ TPasResolver }

// inline
function TPasResolver.GetBaseTypes(bt: TResolverBaseType
  ): TPasUnresolvedSymbolRef;
begin
  Result:=FBaseTypes[bt];
end;

// inline
function TPasResolver.GetScopes(Index: integer): TPasScope;
begin
  Result:=FScopes[Index];
end;

// inline
function TPasResolver.IsNameExpr(El: TPasExpr): boolean;
begin
  Result:=(El.ClassType=TSelfExpr)
      or ((El.ClassType=TPrimitiveExpr) and (TPrimitiveExpr(El).Kind=pekIdent));
end;

function TPasResolver.GetNameExprValue(El: TPasExpr): string;
begin
  if El=nil then
    Result:=''
  else if El.ClassType=TPrimitiveExpr then
    begin
    if TPrimitiveExpr(El).Kind=pekIdent then
      Result:=TPrimitiveExpr(El).Value
    else
      Result:='';
    end
  else if El.ClassType=TSelfExpr then
    Result:='self'
  else
    Result:='';
end;

function TPasResolver.GetNextDottedExpr(El: TPasExpr): TPasExpr;
// returns TSelfExpr or TPrimitiveExpr (Kind=pekIdent)
var
  Bin: TBinaryExpr;
  C: TClass;
begin
  Result:=nil;
  if El=nil then exit;
  repeat
    if not (El.Parent is TBinaryExpr) then exit;
    Bin:=TBinaryExpr(El.Parent);
    if Bin.OpCode<>eopSubIdent then exit;
    if El=Bin.right then
      El:=Bin
    else
      begin
      El:=Bin.right;
      // find left most
      repeat
        C:=El.ClassType;
        if C=TSelfExpr then
          exit(El)
        else if C=TPrimitiveExpr then
          begin
          if TPrimitiveExpr(El).Kind<>pekIdent then
            RaiseNotYetImplemented(20170502163825,El);
          exit(El);
          end
        else if C=TBinaryExpr then
          begin
          if TBinaryExpr(El).OpCode<>eopSubIdent then
            RaiseNotYetImplemented(20170502163718,El);
          El:=TBinaryExpr(El).left;
          end
        else if C=TParamsExpr then
          begin
          if not (TParamsExpr(El).Kind in [pekFuncParams,pekArrayParams]) then
            RaiseNotYetImplemented(20170502163908,El);
          El:=TParamsExpr(El).Value;
          end;
      until El=nil;
      RaiseNotYetImplemented(20170502163953,Bin);
      end;
  until false;
end;

function TPasResolver.GetUsesUnitInFilename(InFileExpr: TPasExpr): string;
var
  Value: TResEvalValue;
begin
  if not (InFileExpr is TPrimitiveExpr) then
    RaiseXExpectedButYFound(20180221234828,'string literal',GetElementTypeName(InFileExpr),InFileExpr);
  Value:=ExprEvaluator.Eval(TPasExpr(TPrimitiveExpr(InFileExpr)),[refConst]);
  try
    if (Value=nil) then
      RaiseXExpectedButYFound(20180222000004,'string literal',GetElementTypeName(InFileExpr),InFileExpr);
    case Value.Kind of
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      Result:=ExprEvaluator.GetUTF8Str(TResEvalString(Value).S,InFileExpr);
    revkUnicodeString:
      Result:=UTF8Encode(TResEvalUTF16(Value).S);
    {$else}
    revkUnicodeString:
      Result:=TResEvalUTF16(Value).S;
    {$endif}
    else
      RaiseXExpectedButYFound(20180222000122,'string literal',Value.AsDebugString,InFileExpr);
    end;
  finally
    ReleaseEvalValue(Value);
  end;
end;

function TPasResolver.GetPathStart(El: TPasExpr): TPasExpr;
// get leftmost name element (e.g. TPrimitiveExpr or TSelfExpr)
// nil if not found
var
  C: TClass;
begin
  Result:=nil;
  while El<>nil do
    begin
    C:=El.ClassType;
    if C=TPrimitiveExpr then
      exit(El)
    else if C=TSelfExpr then
      exit(El)
    else if C=TBinaryExpr then
      begin
      if TBinaryExpr(El).OpCode=eopSubIdent then
        El:=TBinaryExpr(El).left
      else
        exit;
      end
    else if C=TParamsExpr then
      El:=TParamsExpr(El).Value
    else
      exit;
    end;
end;

function TPasResolver.GetNewInstanceExpr(El: TPasExpr): TPasExpr;
// if the expression is a constructor newinstance call,
// return the element referring the constructor
// else nil
var
  C: TClass;
begin
  Result:=nil;
  while El<>nil do
    begin
    if (El.CustomData is TResolvedReference)
        and (rrfNewInstance in TResolvedReference(El.CustomData).Flags) then
      exit(El);
    C:=El.ClassType;
    if C=TBinaryExpr then
      begin
      if TBinaryExpr(El).OpCode=eopSubIdent then
        El:=TBinaryExpr(El).right
      else
        exit;
      end
    else if C=TParamsExpr then
      El:=TParamsExpr(El).Value
    else
      exit;
    end;
end;

procedure TPasResolver.ClearResolveDataList(Kind: TResolveDataListKind);
var
  El: TPasElement;
  RData: TResolveData;
begin
  // clear CustomData
  while FLastCreatedData[Kind]<>nil do
    begin
    RData:=FLastCreatedData[Kind];
    El:=RData.Element;
    El.CustomData:=nil;
    FLastCreatedData[Kind]:=RData.Next;
    RData.Free;
    end;
end;

function TPasResolver.GetBaseTypeNames(bt: TResolverBaseType): string;
begin
  if FBaseTypes[bt]<>nil then
    Result:=FBaseTypes[bt].Name
  else
    Result:=ResBaseTypeNames[bt];
end;

function TPasResolver.GetBuiltInProcs(bp: TResolverBuiltInProc
  ): TResElDataBuiltInProc;
begin
  Result:=FBuiltInProcs[bp];
end;

procedure TPasResolver.SetRootElement(const AValue: TPasModule);
begin
  if FRootElement=AValue then Exit;
  FRootElement:=AValue;
end;

procedure TPasResolver.OnFindFirstElement(El: TPasElement; ElScope,
  StartScope: TPasScope; FindFirstElementData: Pointer; var Abort: boolean);
var
  Data: PPRFindData absolute FindFirstElementData;
  ok: Boolean;
begin
  ok:=true;
  //writeln('TPasResolver.OnFindFirstElement ',El.PathName);
  if (El is TPasProcedure)
      and ProcNeedsParams(TPasProcedure(El).ProcType) then
    // found a proc, but it needs parameters -> remember the first and continue
    ok:=false;
  if ok or (Data^.Found=nil) then
    begin
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    end;
  if ok then
    Abort:=true;
end;

procedure TPasResolver.OnFindCallElements(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcsData: Pointer; var Abort: boolean);
var
  Data: PFindCallElData absolute FindProcsData;
  Proc, PrevProc: TPasProcedure;
  Distance: integer;
  BuiltInProc: TResElDataBuiltInProc;
  CandidateFound: Boolean;
  VarType, TypeEl: TPasType;
  C: TClass;
  ProcScope: TPasProcedureScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindCallElements START --------- ',GetObjName(El),' at ',GetElementSourcePosStr(El));
  {$ENDIF}
  CandidateFound:=false;

  if (El is TPasProcedure) then
    begin
    // identifier is a proc
    Proc:=TPasProcedure(El);
    PrevProc:=nil;

    if Data^.Found=Proc then
      begin
      // this proc was already found. This happens when this is the forward
      // declaration or a previously found implementation.
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      exit;
      end;

    ProcScope:=Proc.CustomData as TPasProcedureScope;
    if ProcScope.DeclarationProc<>nil then
      begin
      // this proc has a forward declaration -> use that instead
      Proc:=ProcScope.DeclarationProc;
      El:=Proc;
      end;

    if Data^.Found is TPasProcedure then
      begin
      // there is already a previous proc
      PrevProc:=TPasProcedure(Data^.Found);

      if msDelphi in TPasProcedureScope(Data^.LastProc.CustomData).ModeSwitches then
        begin
        if (not Data^.LastProc.IsOverload) or (not Proc.IsOverload) then
          begin
          Abort:=true;
          exit;
          end;
        end
      else
        begin
        // mode objfpc
        if IsSameProcContext(Proc.Parent,Data^.LastProc.Parent) then
          // mode objfpc: procs in same context have implicit overload
        else
          begin
          // mode objfpc, different context
          if not ProcHasGroupOverload(Data^.LastProc) then
            begin
            Abort:=true;
            exit;
            end;
          end;
        end;

      if (Data^.Distance=cExact) and (PrevProc.Parent<>Proc.Parent)
          and (PrevProc.Parent.ClassType=TPasClassType) then
        begin
        // there was already a perfect proc in a descendant
        Abort:=true;
        exit;
        end;

      // check if previous found proc is override of found proc
      if IsProcOverride(Proc,PrevProc) then
        begin
        // previous found proc is override of found proc -> skip
        exit;
        end;
      end;

    if (msDelphi in ProcScope.ModeSwitches) and not Proc.IsOverload then
      Abort:=true; // stop searching after this proc

    CandidateFound:=true;
    Distance:=CheckCallProcCompatibility(Proc.ProcType,Data^.Params,false);

    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Proc Distance=',Distance,
      ' Data^.Found=',Data^.Found<>nil,' Data^.Distance=',Data^.Distance,
      ' Signature={',GetProcTypeDescription(Proc.ProcType,[prptdUseName,prptdAddPaths]),'}',
      ' Abort=',Abort);
    {$ENDIF}
    Data^.LastProc:=Proc;
    end
  else if El is TPasType then
    begin
    TypeEl:=ResolveAliasType(TPasType(El));
    C:=TypeEl.ClassType;
    if C=TPasUnresolvedSymbolRef then
      begin
      if TypeEl.CustomData.ClassType=TResElDataBuiltInProc then
        begin
        // call of built-in proc
        BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);
        if (BuiltInProc.BuiltIn in [bfStrProc,bfStrFunc])
            and ((BuiltInProc.BuiltIn=bfStrProc) = ParentNeedsExprResult(Data^.Params)) then
          begin
          // str function can only be used within an expression
          // str procedure can only be used outside an expression
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' skip');
          {$ENDIF}
          exit;
          end;
        Distance:=BuiltInProc.GetCallCompatibility(BuiltInProc,Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end
      else if TypeEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        Abort:=true; // can't be overloaded
        if Data^.Found<>nil then exit;
        Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Base type cast=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end;
      end
    else if (C=TPasClassType)
        or (C=TPasClassOfType)
        or (C=TPasPointerType)
        or (C=TPasRecordType)
        or (C=TPasEnumType)
        or (C=TPasProcedureType)
        or (C=TPasFunctionType)
        or (C=TPasArrayType)
        or (C=TPasRangeType) then
      begin
      // type cast to user type
      Abort:=true; // can't be overloaded
      if Data^.Found<>nil then exit;
      Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements type cast to =',GetObjName(El),' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El is TPasVariable then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    VarType:=ResolveAliasType(TPasVariable(El).VarType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call var of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El.ClassType=TPasArgument then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    VarType:=ResolveAliasType(TPasArgument(El).ArgType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call arg of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end;

  if not CandidateFound then
    begin
    // El does not support the () operator
    Abort:=true;
    if Data^.Found=nil then
      begin
      // El is the first element found -> raise error
      // ToDo: use the ( as error position
      RaiseMsg(20170216151525,nIllegalQualifierAfter,sIllegalQualifierAfter,
        ['(',El.ElementTypeName],Data^.Params);
      end;
    exit;
    end;

  // El is a candidate (might be incompatible)
  if (Data^.Found=nil)
      or ((Data^.Distance=cIncompatible) and (Distance<cIncompatible)) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found first candidate Distance=',Distance);
    {$ENDIF}
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Data^.Distance:=Distance;
    Data^.Count:=1;
    if Data^.List<>nil then
      begin
      Data^.List.Clear;
      Data^.List.Add(El);
      end;
    end
  else if Distance=cIncompatible then
    // another candidate, but it is incompatible -> ignore
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found another candidate, but it is incompatible -> ignore')
    {$ENDIF}
  else if (Data^.Distance=Distance)
      or ((Distance>=cLossyConversion) and (Data^.Distance>=cLossyConversion)
          and ((Distance>=cIntToFloatConversion)=(Data^.Distance>=cIntToFloatConversion))) then
    begin
    // found another similar compatible one -> collect
    // Note: cLossyConversion is better than cIntToFloatConversion, not similar
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found another candidate Distance=',Distance,' OldDistance=',Data^.Distance);
    {$ENDIF}
    inc(Data^.Count);
    if (Data^.List<>nil) then
      begin
      if (Data^.List.IndexOf(El)>=0) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Found El twice: ',GetTreeDbg(El),
          ' ',GetElementSourcePosStr(El),
          ' PrevElScope=',GetObjName(Data^.ElScope),' ',GetTreeDbg(Data^.ElScope.Element),
          ' ElScope=',GetObjName(ElScope),' ',GetTreeDbg(ElScope.Element)
          );
        {$ENDIF}
        RaiseInternalError(20160924230805);
        end;
      Data^.List.Add(El);
      end;
    end
  else if (Distance<Data^.Distance) then
    begin
    // found a better one
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found a better candidate Distance=',Distance,' Data^.Distance=',Data^.Distance);
    {$ENDIF}
    if (Distance<cLossyConversion)
        or ((Distance>=cIntToFloatConversion)<>(Data^.Distance>=cIntToFloatConversion)) then
      begin
      // found a good one
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements Found a good candidate Distance=',Distance,' Data^.Distance=',Data^.Distance);
      {$ENDIF}
      Data^.Count:=1;
      if Data^.List<>nil then
        Data^.List.Clear;
      end
    else
      begin
      // found another lossy one
      // -> collect them
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements Found another lossy candidate Distance=',Distance,' Data^.Distance=',Data^.Distance);
      {$ENDIF}
      inc(Data^.Count);
      end;
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Data^.Distance:=Distance;
    if Data^.List<>nil then
      Data^.List.Add(El);
    end
  else
    begin
    // found a worse one
    end;
end;

procedure TPasResolver.OnFindOverloadProc(El: TPasElement; ElScope,
  StartScope: TPasScope; FindOverloadData: Pointer; var Abort: boolean);
var
  Data: PFindOverloadProcData absolute FindOverloadData;
  Proc: TPasProcedure;
  Store, SameScope: Boolean;
  ProcScope: TPasProcedureScope;

  procedure CountProcInSameModule;
  begin
    inc(Data^.FoundInSameScope);
    if Proc.IsOverload then
      Data^.FoundOverloadModifier:=true;
  end;

begin
  //writeln('TPasResolver.OnFindOverloadProc START ',El.Name,':',GetElementTypeName(El),' itself=',El=Data^.Proc);
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    if (El is TPasVariable) then
      begin
      if TPasVariable(El).Visibility=visStrictPrivate then
        exit; // not visible
      if (TPasVariable(El).Visibility=visPrivate)
          and (El.GetModule<>StartScope.Element.GetModule) then
        exit; // not visible
      end;

    Data^.FoundNonProc:=El;
    Abort:=true;
    if (El.CustomData is TResElDataBuiltInProc) then
      begin
      if Data^.FoundOverloadModifier or Data^.Proc.IsOverload then
        exit; // no hint
      end;
    case Data^.Kind of
      fopkProc:
        // proc hides a non proc
        if (Data^.Proc.GetModule=El.GetModule) then
          // forbidden within same module
          RaiseMsg(20170216151649,nDuplicateIdentifier,sDuplicateIdentifier,
            [El.Name,GetElementSourcePosStr(El)],Data^.Proc.ProcType)
        else
          begin
          // give a hint
          if Data^.Proc.Parent is TPasClassType then
            LogMsg(20171118205344,mtHint,nFunctionHidesIdentifier_NonProc,sFunctionHidesIdentifier,
              [GetElementSourcePosStr(El)],Data^.Proc.ProcType);
          end;
      fopkMethod:
        // method hides a non proc
        RaiseMsg(20171118232543,nDuplicateIdentifier,sDuplicateIdentifier,
          [El.Name,GetElementSourcePosStr(El)],Data^.Proc.ProcType);
    end;
    exit;
    end;

  // identifier is a proc
  Proc:=TPasProcedure(El);
  if El=Data^.Proc then
    begin
    // found itself -> this is normal when searching for overloads
    CountProcInSameModule;
    exit;
    end;

  //writeln('TPasResolver.OnFindOverloadProc Data^.OnlyScope=',GetObjName(Data^.OnlyScope),' ElScope=',GetObjName(ElScope),' Same=',Data^.OnlyScope=ElScope);
  if (Data^.OnlyScope<>nil) and (Data^.OnlyScope<>ElScope) then
    begin
    // do not search any further, only one scope should be searched
    // for example when searching the method declaration of a method body
    Abort:=false;
    exit;
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindOverloadProc ',GetTreeDbg(El,2));
  {$ENDIF}
  Store:=CheckOverloadProcCompatibility(Data^.Proc,Proc);
  if Data^.Kind=fopkSameSignature then
    // finding a proc with same signature is enough, see above Data^.OnlyScope
  else
    begin
    if Data^.Kind=fopkProc then
      SameScope:=Data^.Proc.GetModule=Proc.GetModule
    else
      SameScope:=Data^.Proc.Parent=Proc.Parent;
    if SameScope then
      begin
      // same scope
      if (msObjfpc in CurrentParser.CurrentModeswitches) then
        begin
          if ProcHasGroupOverload(Data^.Proc) then
            Include(TPasProcedureScope(Proc.CustomData).Flags,ppsfIsGroupOverload)
          else if ProcHasGroupOverload(Proc) then
            Include(TPasProcedureScope(Data^.Proc.CustomData).Flags,ppsfIsGroupOverload);
        end;
      if Store then
        begin
        // same scope, same signature
        // Note: forward declaration was already handled in FinishProcedureHeader
        RaiseMsg(20171118221821,nDuplicateIdentifier,sDuplicateIdentifier,
                  [Proc.Name,GetElementSourcePosStr(Proc)],Data^.Proc.ProcType);
        end
      else
        begin
        // same scope, different signature
        if (msDelphi in CurrentParser.CurrentModeswitches) then
          begin
          // Delphi does not allow different procs without 'overload' in a scope
          if not Proc.IsOverload then
            RaiseMsg(20171118222112,nPreviousDeclMissesOverload,sPreviousDeclMissesOverload,
              [Proc.Name,GetElementSourcePosStr(Proc)],Data^.Proc.ProcType)
          else if not Data^.Proc.IsOverload then
            RaiseMsg(20171118222147,nOverloadedProcMissesOverload,sOverloadedProcMissesOverload,
              [GetElementSourcePosStr(Proc)],Data^.Proc.ProcType);
          end
        else
          begin
          // ObjFPC allows different procs without 'overload' modifier
          end;
        CountProcInSameModule;
        end;
      end
    else
      begin
      // different scopes
      if Data^.Proc.IsOverride then
      else if Data^.Proc.IsReintroduced then
      else
        begin
        if Store
            or ((Data^.FoundInSameScope=1) // missing 'overload' hints only for the first proc in a scope
               and not ProcHasGroupOverload(Data^.Proc)) then
          begin
          if (Data^.Kind=fopkMethod) and (Proc.IsVirtual or Proc.IsOverride) then
            // give a hint, that method hides a virtual method in ancestor
            LogMsg(20170216151712,mtWarning,nMethodHidesMethodOfBaseType,
              sMethodHidesMethodOfBaseType,
              [Data^.Proc.Name,Proc.Parent.Name,GetElementSourcePosStr(Proc)],Data^.Proc.ProcType)
          else
            begin
            // Delphi/FPC do not give a message when hiding a non virtual method
            // -> emit Hint with other message id
            if (Data^.Proc.Parent is TPasClassType) then
              begin
              ProcScope:=Proc.CustomData as TPasProcedureScope;
              if (ProcScope.ImplProc<>nil)  // not abstract, external
                  and (not ProcHasImplElements(ProcScope.ImplProc)) then
                // hidden method has implementation, but no statements -> useless
                // -> do not give a hint for hiding this useless method
                // Note: if this happens in the same unit, the body was not yet parsed
              else if (Proc is TPasConstructor)
                  and (Data^.Proc.ClassType=Proc.ClassType) then
                // do not give a hint for hiding a constructor
              else
                LogMsg(20171118214523,mtHint,
                  nFunctionHidesIdentifier_NonVirtualMethod,sFunctionHidesIdentifier,
                  [GetElementSourcePosStr(Proc)],Data^.Proc.ProcType);
              end;
            end;
          Abort:=true;
          end;
        end;
      end;
    end;

  if Store then
    begin
    Data^.Found:=Proc;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Abort:=true;
    end;
end;

function TPasResolver.IsSameProcContext(ProcParentA, ProcParentB: TPasElement
  ): boolean;
begin
  if ProcParentA=ProcParentB then exit(true);
  if (ProcParentA.ClassType=TInterfaceSection) then
    begin
    if (ProcParentB.ClassType=TImplementationSection)
        and (ProcParentB.Parent=ProcParentA.Parent) then
      exit(true);
    end
  else if (ProcParentB.ClassType=TInterfaceSection) then
    begin
    if (ProcParentA.ClassType=TImplementationSection)
        and (ProcParentA.Parent=ProcParentB.Parent) then
      exit(true);
    end;
  Result:=false;
end;

function TPasResolver.FindProcOverload(const ProcName: string;
  Proc: TPasProcedure; OnlyScope: TPasScope): TPasProcedure;
var
  FindData: TFindOverloadProcData;
  Abort: boolean;
begin
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=Proc;
  FindData.Args:=Proc.ProcType.Args;
  FindData.Kind:=fopkSameSignature;
  FindData.OnlyScope:=OnlyScope;
  Abort:=false;
  OnlyScope.IterateElements(ProcName,OnlyScope,@OnFindOverloadProc,@FindData,Abort);
  Result:=FindData.Found;
end;

procedure TPasResolver.SetCurrentParser(AValue: TPasParser);
begin
  //writeln('TPasResolver.SetCurrentParser ',AValue<>nil);
  if AValue=CurrentParser then exit;
  Clear;
  inherited SetCurrentParser(AValue);
  if CurrentParser<>nil then
    begin
    CurrentParser.Options:=CurrentParser.Options+po_Resolver;
    if (CurrentParser.Scanner<>nil) and (CurrentParser.Scanner.OnWarnDirective=nil) then
      CurrentParser.Scanner.OnWarnDirective:=@ScannerWarnDirective;
    end;
end;

procedure TPasResolver.ScannerWarnDirective(Sender: TObject;
  Identifier: string; State: TWarnMsgState; var Handled: boolean);
var
  MsgNumbers: TIntegerDynArray;
  i: Integer;
begin
  if not GetWarnIdentifierNumbers(Identifier,MsgNumbers) then exit;
  Handled:=true;
  for i:=0 to length(MsgNumbers)-1 do
    TPascalScanner(Sender).WarnMsgState[MsgNumbers[i]]:=State;
end;

procedure TPasResolver.CheckTopScope(ExpectedClass: TPasScopeClass;
  AllowDescendants: boolean);
var
  Scope: TPasScope;
begin
  Scope:=TopScope;
  if Scope=nil then
    RaiseInternalError(20160922163319,'Expected TopScope='+ExpectedClass.ClassName+' but found nil');
  if Scope.ClassType<>ExpectedClass then
    if (not AllowDescendants) or (not Scope.InheritsFrom(ExpectedClass)) then
      RaiseInternalError(20160922163323,'Expected TopScope='+ExpectedClass.ClassName+' but found '+Scope.ClassName);
end;

function TPasResolver.AddIdentifier(Scope: TPasIdentifierScope;
  const aName: String; El: TPasElement; const Kind: TPasIdentifierKind
  ): TPasIdentifier;
var
  Identifier, OlderIdentifier: TPasIdentifier;
  ClassScope: TPasClassScope;
  OlderEl: TPasElement;
  IsClassScope: Boolean;
  C: TClass;
begin
  if aName='' then exit(nil);

  IsClassScope:=(Scope is TPasClassScope);

  if (El.Visibility=visPublished) then
    begin
    C:=El.ClassType;
    if (C=TPasProperty) or (C=TPasVariable) then
      // Note: VarModifiers are not yet set
    else if (C=TPasProcedure) or (C=TPasFunction) then
      // ok
    else
      RaiseMsg(20170403223024,nSymbolCannotBePublished,sSymbolCannotBePublished,[],El);
    end;

  if (Kind=pikSimple) and IsClassScope
      and (El.ClassType<>TPasProperty) then
    begin
    // check duplicate in ancestors
    ClassScope:=TPasClassScope(Scope).AncestorScope;
    while ClassScope<>nil do
      begin
      OlderIdentifier:=ClassScope.FindLocalIdentifier(aName);
      while OlderIdentifier<>nil do
        begin
        OlderEl:=OlderIdentifier.Element;
        OlderIdentifier:=OlderIdentifier.NextSameIdentifier;
        if OlderEl is TPasVariable then
          begin
          if TPasVariable(OlderEl).Visibility=visStrictPrivate then
            continue; // OlderEl is hidden
          if (TPasVariable(OlderEl).Visibility=visPrivate)
              and (OlderEl.GetModule<>El.GetModule) then
            continue; // OlderEl is hidden
          end;
        RaiseMsg(20170221130001,nDuplicateIdentifier,sDuplicateIdentifier,
                 [aName,GetElementSourcePosStr(OlderEl)],El);
        end;
      ClassScope:=ClassScope.AncestorScope;
      end;
    end;

  Identifier:=Scope.AddIdentifier(aName,El,Kind);

  // check duplicate in current scope
  OlderIdentifier:=Identifier.NextSameIdentifier;
  if (OlderIdentifier<>nil) then
    if (Identifier.Kind=pikSimple)
        or (OlderIdentifier.Kind=pikSimple)
        or (El.Visibility=visPublished) then
      begin
      if (OlderIdentifier.Element.ClassType=TPasEnumValue)
          and (OlderIdentifier.Element.Parent.Parent<>Scope.Element) then
        // this enum was propagated from a sub type -> remove enum
        Scope.RemoveLocalIdentifier(OlderIdentifier.Element);
      RaiseMsg(20170216151530,nDuplicateIdentifier,sDuplicateIdentifier,
               [aName,GetElementSourcePosStr(OlderIdentifier.Element)],El);
      end;

  Result:=Identifier;
end;

procedure TPasResolver.FinishModule(CurModule: TPasModule);
var
  CurModuleClass: TClass;
  i: Integer;
  ModScope: TPasModuleScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule START ',CurModule.Name);
  {$ENDIF}
  FStep:=prsFinishingModule;

  CurModuleClass:=CurModule.ClassType;
  ModScope:=CurModule.CustomData as TPasModuleScope;

  if bsRangeChecks in CurrentParser.Scanner.CurrentBoolSwitches then
    begin
    Include(ModScope.Flags,pmsfRangeErrorNeeded);
    FindRangeErrorConstructors(CurModule);
    end;

  if (CurModuleClass=TPasProgram) then
    begin
    FinishSection(TPasProgram(CurModule).ProgramSection);
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    end
  else if (CurModuleClass=TPasLibrary) then
    begin
    FinishSection(TPasLibrary(CurModule).LibrarySection);
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    end
  else if (CurModuleClass=TPasModule) then
    begin
    // unit
    FinishSection(CurModule.InterfaceSection);
    FinishSection(CurModule.ImplementationSection);
    if CurModule.FinalizationSection<>nil then
      // finalization section finished -> resolve
      ResolveImplBlock(CurModule.FinalizationSection);
    if CurModule.InitializationSection<>nil then
      // initialization section finished -> resolve
      ResolveImplBlock(CurModule.InitializationSection);
    end
  else
    RaiseInternalError(20160922163327); // unknown module

  // check all methods have bodies
  // and all forward classes and pointers are resolved
  for i:=0 to FPendingForwardProcs.Count-1 do
    CheckPendingForwardProcs(TPasElement(FPendingForwardProcs[i]));
  FPendingForwardProcs.Clear;

  // close all sections
  while (TopScope<>nil) and (TopScope.ClassType=ScopeClass_Section) do
    PopScope;
  CheckTopScope(FScopeClass_Module);
  PopScope;

  FStep:=prsFinishedModule;

  if (CurrentParser<>nil) and (CurrentParser.Scanner<>nil) then
    begin
    CurrentParser.NextToken;
    if CurrentParser.Scanner.CurToken<>tkEOF then
      LogMsg(20180628131456,mtHint,nTextAfterFinalIgnored,sTextAfterFinalIgnored,
        [],nil);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule END ',CurModule.Name);
  {$ENDIF}
end;

procedure TPasResolver.FinishUsesClause;
var
  Section, CurSection: TPasSection;
  i, j: Integer;
  PublicEl, UseModule: TPasElement;
  Scope: TPasSectionScope;
  UsesScope: TPasIdentifierScope;
  UseUnit: TPasUsesUnit;
  FirstName: String;
  p: SizeInt;
  OldIdentifier: TPasIdentifier;
begin
  CheckTopScope(ScopeClass_Section);
  Scope:=TPasSectionScope(TopScope);
  Section:=TPasSection(Scope.Element);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishUsesClause Section=',Section.ClassName,' Section.UsesList.Count=',Section.UsesList.Count);
  {$ENDIF}
  if Scope.UsesFinished then
    RaiseInternalError(20180305145220);
  Scope.UsesFinished:=true;

  for i:=0 to Section.UsesList.Count-1 do
    begin
    UseUnit:=Section.UsesClause[i];
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesClause ',GetObjName(UseUnit));
    {$ENDIF}
    UseModule:=UseUnit.Module;

    // check used unit
    PublicEl:=nil;
    if (UseModule.ClassType=TPasLibrary) then
      PublicEl:=TPasLibrary(UseModule).LibrarySection
    else if (UseModule.ClassType=TPasModule) then
      PublicEl:=TPasModule(UseModule).InterfaceSection
    else
      RaiseXExpectedButYFound(20170503004803,'unit',GetElementTypeName(UseModule),UseUnit);
    if PublicEl=nil then
      RaiseInternalError(20160922163352,'uses element has no interface section: '+GetObjName(UseModule));
    if PublicEl.CustomData=nil then
      RaiseInternalError(20160922163358,'uses element has no resolver data: '
        +UseUnit.Name+'->'+GetObjName(PublicEl));
    if not (PublicEl.CustomData is TPasIdentifierScope) then
      RaiseInternalError(20160922163403,'uses element has invalid resolver data: '
        +UseUnit.Name+'->'+GetObjName(PublicEl)+'->'+PublicEl.CustomData.ClassName);

    // check if module was already used by a different name
    j:=i;
    CurSection:=Section;
    repeat
      dec(j);
      if j<0 then
        begin
        if CurSection.ClassType<>TImplementationSection then
          break;
        CurSection:=CurSection.GetModule.InterfaceSection;
        if CurSection=nil then break;
        j:=length(CurSection.UsesClause)-1;
        if j<0 then break;
        end;
      if CurSection.UsesClause[j].Module=UseModule then
        RaiseMsg(20170503004022,nDuplicateIdentifier,sDuplicateIdentifier,
          [UseModule.Name,GetElementSourcePosStr(CurSection.UsesClause[j])],UseUnit);
    until false;

    // add full uses name
    AddIdentifier(Scope,UseUnit.Name,UseUnit,pikSimple);

    // add scope
    UsesScope:=TPasIdentifierScope(PublicEl.CustomData);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesClause Add UsesScope=',GetObjName(UsesScope));
    {$ENDIF}
    Scope.UsesScopes.Add(UsesScope);

    EmitElementHints(Section,UseUnit);
    end;

  // Add first name of dotted unitname (top level subnamespace) as identifier
  for i:=Section.UsesList.Count-1 downto 0 do
    begin
    UseUnit:=Section.UsesClause[i];
    FirstName:=UseUnit.Name;
    p:=Pos('.',FirstName);
    if p<1 then continue;
    FirstName:=LeftStr(FirstName,p-1);
    OldIdentifier:=Scope.FindLocalIdentifier(FirstName);
    if (OldIdentifier=nil) then
      AddIdentifier(Scope,FirstName,UseUnit,pikNamespace);
    end;
  // Note: a sub identifier (e.g. a class member) hides all unitnames starting
  //       with this identifier
end;

procedure TPasResolver.FinishSection(Section: TPasSection);
// Note: can be called multiple times for a section
var
  Scope: TPasSectionScope;
begin
  Scope:=Section.CustomData as TPasSectionScope;
  if Scope.Finished then exit;
  Scope.Finished:=true;
  if Section is TInterfaceSection then
    FinishInterfaceSection(Section);
end;

procedure TPasResolver.FinishInterfaceSection(Section: TPasSection);
begin
  {$IFDEF VerboseUnitQueue}
  writeln('TPasResolver.FinishInterfaceSection ',GetObjName(RootElement));
  {$ENDIF}
  {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
  if not IsUnitIntfFinished(Section.GetModule) then
    RaiseInternalError(20171214004323,'TPasResolver.FinishInterfaceSection "'+RootElement.Name+'" "'+Section.GetModule.Name+'" IsUnitIntfFinished=false');
  {$ENDIF}
  NotifyPendingUsedInterfaces;
  if Section=nil then ;
end;

procedure TPasResolver.FinishTypeSection(El: TPasDeclarations);

  function ReplaceDestType(Decl: TPasType; var DestType: TPasType;
    const DestName: string; MustExist: boolean; ErrorEl: TPasElement
    {$IFDEF CheckPasTreeRefCount}; const RefId: string{$ENDIF}): boolean;
  // returns true if replaces
  var
    Abort: boolean;
    Data: TPRFindData;
    OldDestType: TPasType;
  begin
    Abort:=false;
    Data:=Default(TPRFindData);
    Data.ErrorPosEl:=ErrorEl;
    (TopScope as TPasIdentifierScope).IterateElements(DestName,
      TopScope,@OnFindFirstElement,@Data,Abort);
    if (Data.Found=nil) then
      if MustExist then
        begin
        if DestType is TUnresolvedPendingRef then
          DestType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
        RaiseIdentifierNotFound(20170216151543,DestName,ErrorEl);
        end
      else
        exit(false);
    if Data.Found=DestType then exit;
    if Decl is TPasClassOfType then
      begin
      if Data.Found.ClassType<>TPasClassType then
        RaiseXExpectedButYFound(20170216151548,'class',GetElementTypeName(Data.Found),ErrorEl);
      end;
    // replace unresolved
    OldDestType:=DestType;
    DestType:=TPasType(Data.Found);
    DestType.AddRef{$IFDEF CheckPasTreeRefCount}(RefId){$ENDIF};
    if OldDestType is TUnresolvedPendingRef then
      OldDestType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
    OldDestType.Release{$IFDEF CheckPasTreeRefCount}(RefId){$ENDIF};
    // check cycles
    if Decl is TPasPointerType then
      CheckPointerCycle(TPasPointerType(Decl));
    Result:=true;
  end;

var
  i: Integer;
  Decl: TPasElement;
  ClassOfEl: TPasClassOfType;
  UnresolvedEl: TUnresolvedPendingRef;
  OldClassType: TPasClassType;
  TypeEl: TPasType;
  C: TClass;
  PtrType: TPasPointerType;
begin
  // resolve pending forwards
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    C:=Decl.ClassType;
    if C.InheritsFrom(TPasClassType) then
      begin
      if TPasClassType(Decl).IsForward and (TPasClassType(Decl).CustomData=nil) then
        RaiseMsg(20170216151534,nForwardTypeNotResolved,sForwardTypeNotResolved,[Decl.Name],Decl);
      end
    else if (C=TPasClassOfType) then
      begin
      ClassOfEl:=TPasClassOfType(Decl);
      TypeEl:=ResolveAliasType(ClassOfEl.DestType);
      if (TypeEl.ClassType=TUnresolvedPendingRef) then
        begin
        // forward class-of -> resolve now
        UnresolvedEl:=TUnresolvedPendingRef(TypeEl);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection resolving "',ClassOfEl.Name,'" = class of unresolved "',TypeEl.Name,'"');
        {$ENDIF}
        ReplaceDestType(ClassOfEl,ClassOfEl.DestType,TypeEl.Name,true,UnresolvedEl
          {$IFDEF CheckPasTreeRefCount},'TPasAliasType.DestType'{$ENDIF});
        end
      else if TypeEl.ClassType=TPasClassType then
        begin
        // class-of has found a type
        // another later in the same type section has priority -> check
        OldClassType:=TypeEl as TPasClassType;
        if OldClassType.Parent=ClassOfEl.Parent then
          continue; // class in same type section -> ok
        // class not in same type section -> check
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection improving "',ClassOfEl.Name,'" = class of resolved "',TypeEl.Name,'"');
        {$ENDIF}
        ReplaceDestType(ClassOfEl,ClassOfEl.DestType,ClassOfEl.DestType.Name,false,ClassOfEl
          {$IFDEF CheckPasTreeRefCount},'TPasAliasType.DestType'{$ENDIF});
        end;
      end
    else if C=TPasPointerType then
      begin
      PtrType:=TPasPointerType(Decl);
      TypeEl:=ResolveAliasType(PtrType.DestType);
      if (TypeEl.ClassType=TUnresolvedPendingRef) then
        begin
        // forward pointer -> resolve now
        UnresolvedEl:=TUnresolvedPendingRef(TypeEl);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection resolving "',PtrType.Name,'" = pointer of unresolved "',TypeEl.Name,'"');
        {$ENDIF}
        ReplaceDestType(PtrType,PtrType.DestType,TypeEl.Name,true,UnresolvedEl
          {$IFDEF CheckPasTreeRefCount},'TPasPointerType.DestType'{$ENDIF});
        end
      else
        begin
        // pointer-of has found a type
        // another later in the same type section has priority -> check
        if TypeEl.Parent=Decl.Parent then
          continue; // class in same type section -> ok
        // dest not in same type section -> check
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection improving "',PtrType.Name,'" = pointer of resolved "',TypeEl.Name,'"');
        {$ENDIF}
        ReplaceDestType(PtrType,PtrType.DestType,TypeEl.Name,false,PtrType
          {$IFDEF CheckPasTreeRefCount},'TPasPointerType.DestType'{$ENDIF});
        end;
      end;
    end;
end;

procedure TPasResolver.FinishTypeDef(El: TPasType);
var
  C: TClass;
  aType: TPasType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishTypeDef El=',GetObjName(El));
  {$ENDIF}
  C:=El.ClassType;
  if C=TPasEnumType then
    FinishEnumType(TPasEnumType(El))
  else if C=TPasSetType then
    FinishSetType(TPasSetType(El))
  else if C=TPasRangeType then
    FinishRangeType(TPasRangeType(El))
  else if C=TPasRecordType then
    FinishRecordType(TPasRecordType(El))
  else if C=TPasClassType then
    FinishClassType(TPasClassType(El))
  else if C=TPasClassOfType then
    FinishClassOfType(TPasClassOfType(El))
  else if C=TPasPointerType then
    FinishPointerType(TPasPointerType(El))
  else if C=TPasArrayType then
    FinishArrayType(TPasArrayType(El))
  else if (C=TPasAliasType) or (C=TPasTypeAliasType) then
    begin
    aType:=ResolveAliasType(El);
    if (aType is TPasClassType) and (aType.CustomData=nil) then
      exit;
    EmitTypeHints(El,TPasAliasType(El).DestType);
    end
  else if (C=TPasPointerType) then
    EmitTypeHints(El,TPasPointerType(El).DestType);
end;

procedure TPasResolver.FinishEnumType(El: TPasEnumType);
begin
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishSetType(El: TPasSetType);
var
  BaseTypeData: TResElDataBaseType;
  StartResolved, EndResolved: TPasResolverResult;
  RangeExpr: TBinaryExpr;
  C: TClass;
  EnumType: TPasType;
begin
  EnumType:=ResolveAliasType(El.EnumType);
  C:=EnumType.ClassType;
  if C=TPasEnumType then
    begin
    FinishSubElementType(El,EnumType);
    exit;
    end
  else if C=TPasRangeType then
    begin
    RangeExpr:=TPasRangeType(EnumType).RangeExpr;
    if (RangeExpr.Parent=El) and (RangeExpr.CustomData=nil) then
      FinishConstRangeExpr(RangeExpr,StartResolved,EndResolved);
    FinishSubElementType(El,EnumType);
    exit;
    end
  else if C=TPasUnresolvedSymbolRef then
    begin
    if EnumType.CustomData is TResElDataBaseType then
      begin
      BaseTypeData:=TResElDataBaseType(EnumType.CustomData);
      if BaseTypeData.BaseType in (btAllChars+[btBoolean,btByte]) then
        exit;
      RaiseXExpectedButYFound(20170216151553,'char or boolean',GetElementTypeName(EnumType),EnumType);
      end;
    end;
  RaiseXExpectedButYFound(20170216151557,'enum type',GetElementTypeName(EnumType),EnumType);
end;

procedure TPasResolver.FinishSubElementType(Parent: TPasElement; El: TPasType);
var
  Decl: TPasDeclarations;
  EnumScope: TPasEnumTypeScope;
begin
  EmitTypeHints(Parent,El);
  if (El.Name<>'') or (AnonymousElTypePostfix='') then exit;
  if Parent.Name='' then
    RaiseMsg(20170415165455,nCannotNestAnonymousX,sCannotNestAnonymousX,[GetElementTypeName(El)],El);
  if not (Parent.Parent is TPasDeclarations) then
    RaiseMsg(20170416094735,nCannotNestAnonymousX,sCannotNestAnonymousX,[GetElementTypeName(El)],El);
  // give anonymous sub type a name
  El.Name:=Parent.Name+AnonymousElTypePostfix;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishSubElementType parent="',GetObjName(Parent),'" named anonymous type "',GetObjName(El),'"');
  {$ENDIF}
  Decl:=TPasDeclarations(Parent.Parent);
  Decl.Declarations.Add(El);
  El.AddRef{$IFDEF CheckPasTreeRefCount}('TPasDeclarations.Declarations'){$ENDIF};
  El.Parent:=Decl;
  Decl.Types.Add(El);
  if (El.ClassType=TPasEnumType) and (Parent.ClassType=TPasSetType) then
    begin
    // anonymous enumtype
    EnumScope:=TPasEnumTypeScope(El.CustomData);
    if EnumScope.CanonicalSet<>Parent then
      begin
      // When a TPasEnumType is created a CanonicalSet is created.
      // Release the autocreated CanonicalSet and use the parent.
      if EnumScope.CanonicalSet<>nil then
        EnumScope.CanonicalSet.Release{$IFDEF CheckPasTreeRefCount}('TPasEnumTypeScope.CanonicalSet'){$ENDIF};
      EnumScope.CanonicalSet:=TPasSetType(Parent);
      Parent.AddRef{$IFDEF CheckPasTreeRefCount}('TPasEnumTypeScope.CanonicalSet'){$ENDIF};
      end;
    end;
end;

procedure TPasResolver.FinishRangeType(El: TPasRangeType);
var
  RangeExpr: TBinaryExpr;
  StartResolved, EndResolved: TPasResolverResult;
begin
  RangeExpr:=El.RangeExpr;
  ResolveExpr(RangeExpr.left,rraRead);
  ResolveExpr(RangeExpr.right,rraRead);
  FinishConstRangeExpr(RangeExpr,StartResolved,EndResolved);
end;

procedure TPasResolver.FinishConstRangeExpr(RangeExpr: TBinaryExpr; out
  LeftResolved, RightResolved: TPasResolverResult);
// for example Left..Right
var
  RgValue: TResEvalValue;
  Left, Right: TPasExpr;
begin
  Left:=RangeExpr.left;
  Right:=RangeExpr.right;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.FinishConstRangeExpr Left=',GetObjName(Left),' Right=',GetObjName(Right));
  {$ENDIF}
  // check type compatibility
  ComputeElement(Left,LeftResolved,[rcConstant]);
  ComputeElement(Right,RightResolved,[rcConstant]);
  CheckSetLitElCompatible(Left,Right,LeftResolved,RightResolved);

  RgValue:=Eval(RangeExpr,[refConst]);
  ReleaseEvalValue(RgValue);
end;

procedure TPasResolver.FinishRecordType(El: TPasRecordType);
begin
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishClassType(El: TPasClassType);
type
  TMethResolution = record
    InterfaceIndex: integer;
    ProcClassType: TPasProcedureClass;
    InterfaceName: string;
    ImplementName: string;
    ResolutionEl: TPasMethodResolution;
    Count: integer; // needed to check if method resolution is used
  end;
var
  ClassScope: TPasClassScope;
  i, j, k: Integer;
  IntfType: TPasClassType;
  Resolutions: array of TMethResolution;
  Map: TPasClassIntfMap;
  o: TObject;
  Member: TPasElement;
  IntfProc: TPasProcedure;
  FindData: TFindOverloadProcData;
  Abort: boolean;
  MethRes: TPasMethodResolution;
  ResolvedEl: TPasResolverResult;
  ProcName, IntfProcName: String;
  Expr: TPasExpr;
begin
  Resolutions:=nil;
  if El.CustomData is TPasClassScope then
    begin
    if TopScope.Element<>El then
      RaiseInternalError(20180322142534,GetObjName(El)+'<>'+GetObjName(TopScope.Element));
    ClassScope:=El.CustomData as TPasClassScope;

    if El.ObjKind=okClass then
      begin
      if (El.Interfaces.Count>0) then
        begin
        if (ClassScope.Interfaces=nil) then
          RaiseInternalError(20180408162725,'');
        if (ClassScope.Interfaces.Count<>El.Interfaces.Count) then
          RaiseInternalError(20180408162746,'');
        end
      else if ClassScope.Interfaces<>nil then
        RaiseInternalError(20180408162803,'');

      // check explicit method resolutions, e.g. procedure intf.intfproc = implproc
      for i:=0 to El.Members.Count-1 do
        begin
        Member:=TPasElement(El.Members[i]);
        if not (Member is TPasMethodResolution) then continue;
        MethRes:=TPasMethodResolution(Member);

        // get interface
        ComputeElement(MethRes.InterfaceName,ResolvedEl,[rcNoImplicitProc]);
        if not (ResolvedEl.IdentEl is TPasType) then
          RaiseInternalError(20180323135729,GetResolverResultDbg(ResolvedEl));
        j:=El.Interfaces.IndexOf(ResolvedEl.IdentEl);
        if j<0 then
          RaiseInternalError(20180323135900,GetResolverResultDbg(ResolvedEl));
        // get class-interface-map, check delegations
        o:=TObject(ClassScope.Interfaces[j]);
        if o is TPasProperty then
          RaiseMsg(20180323140046,nCannotMixMethodResolutionAndDelegationAtX,
            sCannotMixMethodResolutionAndDelegationAtX,
            [GetElementSourcePosStr(TPasProperty(o))],MethRes.InterfaceName);
        if o=nil then
          o:=CreateClassIntfMap(El,j);
        Map:=TPasClassIntfMap(o);
        // get interface proc name
        Expr:=MethRes.InterfaceProc;
        if not (Expr is TPrimitiveExpr) then
          RaiseXExpectedButYFound(20180327162230,'method name',GetElementTypeName(Expr),Expr);
        if TPrimitiveExpr(Expr).Kind<>pekIdent then
          RaiseXExpectedButYFound(20180327162236,'method name',GetElementTypeName(Expr),Expr);
        IntfProcName:=TPrimitiveExpr(Expr).Value;
        // get implementation proc name
        Expr:=MethRes.ImplementationProc;
        if not (Expr is TPrimitiveExpr) then
          RaiseXExpectedButYFound(20180327152115,'method name',GetElementTypeName(Expr),Expr);
        if TPrimitiveExpr(Expr).Kind<>pekIdent then
          RaiseXExpectedButYFound(20180327152157,'method name',GetElementTypeName(Expr),Expr);
        ProcName:=TPrimitiveExpr(Expr).Value;

        for k:=0 to length(Resolutions)-1 do
          with Resolutions[k] do
            if (InterfaceIndex=j) and (ProcClassType=MethRes.ProcClass)
                and (InterfaceName=IntfProcName) then
              RaiseMsg(20180327164626,nDuplicateIdentifier,sDuplicateIdentifier,
                [GetElementTypeName(ProcClassType)+' '+Map.Intf.Name+'.'+InterfaceName,
                 GetElementSourcePosStr(ResolutionEl)],MethRes.InterfaceProc);

        // add resolution
        k:=length(Resolutions);
        SetLength(Resolutions,k+1);
        with Resolutions[k] do
          begin
          InterfaceIndex:=j;
          ProcClassType:=MethRes.ProcClass;
          InterfaceName:=IntfProcName;
          ImplementName:=ProcName;
          ResolutionEl:=MethRes;
          Count:=0;
          end;
        end;

      // method resolution
      for i:=0 to El.Interfaces.Count-1 do
        begin
        o:=TObject(ClassScope.Interfaces[i]);
        //writeln('TPasResolver.FinishClassType class=',GetObjName(El),' i=',i,' Intf=',GetObjName(TObject(El.Interfaces[i])),' Map=',GetObjName(o));
        if o is TPasProperty then
          continue; // interface implemented via a property
        if o=nil then
          o:=CreateClassIntfMap(El,i);
        Map:=TPasClassIntfMap(o);
        while Map<>nil do
          begin
          IntfType:=Map.Intf;
          //writeln('TPasResolver.FinishClassType ',GetObjName(Map),' ',GetObjName(IntfType),' Count=',IntfType.Members.Count);
          for j:=0 to IntfType.Members.Count-1 do
            begin
            Member:=TPasElement(IntfType.Members[j]);
            if not (Member is TPasProcedure) then continue;
            IntfProc:=TPasProcedure(Member);
            ProcName:=IntfProc.Name;
            // check resolutions
            for k:=0 to length(Resolutions)-1 do
              with Resolutions[k] do
                begin
                if (InterfaceIndex=i) and (ProcClassType=IntfProc.ClassType)
                    and SameText(InterfaceName,IntfProc.Name) then
                  begin
                  ProcName:=ImplementName;
                  inc(Count);
                  end;
                end;

            // search interface method in class
            FindData:=Default(TFindOverloadProcData);
            FindData.Proc:=IntfProc;
            FindData.Args:=IntfProc.ProcType.Args;
            FindData.Kind:=fopkSameSignature;
            Abort:=false;
            IterateElements(ProcName,@OnFindOverloadProc,@FindData,Abort);
            if FindData.Found=nil then
              RaiseMsg(20180322143202,nNoMatchingImplForIntfMethodXFound,
                sNoMatchingImplForIntfMethodXFound,
                [GetProcTypeDescription(IntfProc.ProcType,[prptdUseName,prptdAddPaths,prptdResolveSimpleAlias])],El); // ToDo: jump to interface list
            Map.Procs[j]:=FindData.Found;
            end;
          Map:=Map.AncestorMap;
          end;
        end;

      // ToDo: hint if method resolution is not used
      end;
    end;

  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishClassOfType(El: TPasClassOfType);
var
  TypeEl: TPasType;
begin
  TypeEl:=ResolveAliasType(El.DestType);
  if TypeEl is TUnresolvedPendingRef then exit;
  if (TypeEl is TPasClassType) and (TPasClassType(TypeEl).ObjKind=okClass) then exit;
  RaiseMsg(20170216151602,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
    [El.DestType.Name,'class'],El);
end;

procedure TPasResolver.FinishPointerType(El: TPasPointerType);
var
  TypeEl: TPasType;
begin
  TypeEl:=ResolveAliasType(El.DestType);
  if TypeEl is TUnresolvedPendingRef then exit;
  if El.DestType.Parent=El then
    RaiseMsg(20180429094237,nNotYetImplemented,sNotYetImplemented,['pointer of anonymous type'],El.DestType);
  CheckPointerCycle(El);
end;

procedure TPasResolver.FinishArrayType(El: TPasArrayType);
var
  i: Integer;
  Expr: TPasExpr;
  RangeResolved: TPasResolverResult;
  TypeEl: TPasType;
begin
  for i:=0 to length(El.Ranges)-1 do
    begin
    Expr:=El.Ranges[i];
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,RangeResolved,[rcConstant]);
    if (RangeResolved.IdentEl<>nil) and not (RangeResolved.IdentEl is TPasType) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishArrayType ',GetResolverResultDbg(RangeResolved));
      {$ENDIF}
      RaiseXExpectedButYFound(20170216151607,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
      end;
    if (RangeResolved.BaseType=btRange) then
      begin
      if (RangeResolved.SubType in btArrayRangeTypes) then
        // range, e.g. 1..2
      else if RangeResolved.SubType=btContext then
        begin
        TypeEl:=RangeResolved.LoTypeEl;
        if TypeEl is TPasRangeType then
          // custom range
        else if TypeEl is TPasEnumType then
          // anonymous enum range
        else
          RaiseXExpectedButYFound(20171009193629,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
        end
      else
        RaiseXExpectedButYFound(20171009193514,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
      end
    else if RangeResolved.BaseType in btArrayRangeTypes then
      // full range, e.g. array[char]
    else if (RangeResolved.BaseType=btContext) and (RangeResolved.LoTypeEl is TPasEnumType) then
      // e.g. array[enumtype]
    else
      RaiseXExpectedButYFound(20170216151609,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
    end;
  if El.ElType=nil then
    RaiseNotYetImplemented(20171005235610,El,'array of const');
  FinishSubElementType(El,El.ElType);
end;

procedure TPasResolver.FinishResourcestring(El: TPasResString);
var
  ResolvedEl: TPasResolverResult;
begin
  ResolveExpr(El.Expr,rraRead);
  ComputeElement(El.Expr,ResolvedEl,[rcConstant]);
  if not (ResolvedEl.BaseType in btAllStringAndChars) then
    RaiseXExpectedButYFound(20171004135753,'string',GetTypeDescription(ResolvedEl),El.Expr);
end;

procedure TPasResolver.FinishProcedure(aProc: TPasProcedure);
var
  i: Integer;
  Body: TProcedureBody;
  SubEl: TPasElement;
  SubProcScope, ProcScope: TPasProcedureScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishProcedure START');
  {$ENDIF}
  CheckTopScope(FScopeClass_Proc);
  ProcScope:=TPasProcedureScope(TopScope);
  if ProcScope.Element<>aProc then
    RaiseInternalError(20170220163043);

  Body:=aProc.Body;
  if Body<>nil then
    begin
    StoreScannerFlagsInProc(ProcScope);
    if Body.Body is TPasImplAsmStatement then
      aProc.Modifiers:=aProc.Modifiers+[pmAssembler];
    ResolveImplBlock(Body.Body);

    // check if all nested forward procs are resolved
    for i:=0 to Body.Declarations.Count-1 do
      begin
      SubEl:=TPasElement(Body.Declarations[i]);
      if (SubEl is TPasProcedure) and TPasProcedure(SubEl).IsForward then
        begin
        SubProcScope:=TPasProcedure(SubEl).CustomData as TPasProcedureScope;
        if SubProcScope.ImplProc=nil then
          RaiseMsg(20170216151613,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(SubEl),SubEl.Name],SubEl);
        end;
      end;
    end;
  PopScope;
end;

procedure TPasResolver.FinishProcedureType(El: TPasProcedureType);
var
  ProcName: String;
  FindData: TFindOverloadProcData;
  DeclProc, Proc, ParentProc: TPasProcedure;
  Abort, HasDots: boolean;
  DeclProcScope, ProcScope: TPasProcedureScope;
  ParentScope: TPasScope;
  pm: TProcedureModifier;
  ptm: TProcTypeModifier;
  ObjKind: TPasObjKind;
begin
  if (El.Parent is TPasProcedure) and (TPasProcedure(El.Parent).ProcType=El) then
    begin
    // finished header of a procedure declaration
    // -> search the best fitting proc
    CheckTopScope(FScopeClass_Proc);
    Proc:=TPasProcedure(El.Parent);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishProcedureHeader El=',GetTreeDbg(El),' ',GetElementSourcePosStr(El),' IsForward=',Proc.IsForward,' Parent=',GetObjName(El.Parent));
    {$ENDIF}
    ProcName:=Proc.Name;

    if (proProcTypeWithoutIsNested in Options) and El.IsNested then
      RaiseInvalidProcTypeModifier(20170402120811,El,ptmIsNested,El);

    if (Proc.Parent.ClassType=TProcedureBody) then
      begin
      // nested sub proc
      if not (proProcTypeWithoutIsNested in Options) then
        El.IsNested:=true;
      // inherit 'of Object'
      ParentProc:=Proc.Parent.Parent as TPasProcedure;
      if ParentProc.ProcType.IsOfObject then
        El.IsOfObject:=true;
      end;

    if El.IsReferenceTo then
      begin
      if El.IsNested then
        RaiseInvalidProcTypeModifier(20170419142818,El,ptmIsNested,El);
      if El.IsOfObject then
        RaiseInvalidProcTypeModifier(20170419142844,El,ptmOfObject,El);
      end;

    if Proc.IsExternal then
      begin
      for pm in Proc.Modifiers do
        if not (pm in [pmVirtual, pmDynamic, pmOverride,
                       pmOverload, pmMessage, pmReintroduce,
                       pmExternal, pmDispId,
                       pmfar]) then
          RaiseMsg(20170216151616,nInvalidXModifierY,
            sInvalidXModifierY,[GetElementTypeName(Proc),'external, '+ModifierNames[pm]],Proc);
      for ptm in Proc.ProcType.Modifiers do
        if not (ptm in [ptmOfObject,ptmIsNested,ptmStatic,ptmVarargs,ptmReferenceTo]) then
          RaiseMsg(20170411171224,nInvalidXModifierY,
            sInvalidXModifierY,[GetElementTypeName(Proc),'external, '+ProcTypeModifiers[ptm]],Proc);
      end;

    HasDots:=Pos('.',ProcName)>1;

    if Proc.Parent is TPasClassType then
      begin
      // method declaration
      ObjKind:=TPasClassType(Proc.Parent).ObjKind;
      case ObjKind of
      okInterface,okDispInterface:
        begin
        if Proc.IsVirtual then
          RaiseMsg(20180321234324,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(20180321234551,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'override'],Proc);
        end;
      end;
      if Proc.IsAbstract then
        begin
        if not Proc.IsVirtual then
          RaiseMsg(20170216151623,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'abstract without virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(20170216151625,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'abstract, override'],Proc);
        end;
      if Proc.IsVirtual and Proc.IsOverride then
        RaiseMsg(20170216151627,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'virtual, override'],Proc);
      if Proc.IsReintroduced and Proc.IsOverride then
        RaiseMsg(20171119111845,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'reintroduce, override'],Proc);
      if Proc.IsForward then
        RaiseMsg(20170216151629,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'forward'],Proc);
      if Proc.IsStatic then
        if (Proc.ClassType<>TPasClassProcedure) and (Proc.ClassType<>TPasClassFunction) then
          RaiseMsg(20170216151631,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'static'],Proc);
      end
    else
      begin
      // intf proc, forward proc, proc body, method body
      if Proc.IsAbstract then
        RaiseInvalidProcModifier(20170216151634,Proc,pmAbstract,Proc);
      if Proc.IsVirtual then
        RaiseInvalidProcModifier(20170216151635,Proc,pmVirtual,Proc);
      if Proc.IsOverride then
        RaiseInvalidProcModifier(20170216151637,Proc,pmOverride,Proc);
      if Proc.IsMessage then
        RaiseInvalidProcModifier(20170216151638,Proc,pmMessage,Proc);
      if Proc.IsStatic then
        RaiseInvalidProcTypeModifier(20170216151640,El,ptmStatic,El);
      if (not HasDots)
          and (Proc.ClassType<>TPasProcedure)
          and (Proc.ClassType<>TPasFunction) then
        RaiseXExpectedButYFound(20170419232724,'full method name','short name',El);
      end;

    if HasDots then
      begin
      FinishMethodImplHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure/method declaration

    if not IsValidIdent(ProcName) then
      RaiseNotYetImplemented(20160922163407,El);

    if El is TPasFunctionType then
      EmitTypeHints(TPasFunctionType(El).ResultEl,TPasFunctionType(El).ResultEl.ResultType);

    if Proc.LibraryExpr<>nil then
      ResolveExpr(Proc.LibraryExpr,rraRead);
    if Proc.LibrarySymbolName<>nil then
      ResolveExpr(Proc.LibrarySymbolName,rraRead);

    if Proc.Parent is TPasClassType then
      begin
      FinishMethodDeclHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure
    if ProcNeedsBody(Proc) then
      begin
      // check if there is a forward declaration
      ParentScope:=Scopes[ScopeCount-2];
      //writeln('TPasResolver.FinishProcedureType FindForward2 ParentScope=',GetObjName(ParentScope),'=',GetObjName(ParentScope.Element),' Proc=',GetObjName(Proc),' at ',GetElementSourcePosStr(Proc));
      DeclProc:=FindProcOverload(ProcName,Proc,ParentScope);
      //writeln('TPasResolver.FinishProcedureType FindForward3 DeclProc=',GetObjName(DeclProc),' Proc.Parent=',GetObjName(Proc.Parent));
      if (DeclProc=nil) and (Proc.Parent.ClassType=TImplementationSection) then
        DeclProc:=FindProcOverload(ProcName,Proc,
          (Proc.GetModule.InterfaceSection.CustomData) as TPasScope);
      //writeln('TPasResolver.FinishProcedureType FindForward4 ',GetObjName(DeclProc),' at ',GetElementSourcePosStr(DeclProc));
      if (DeclProc<>nil) then
        begin
        if ProcNeedsImplProc(DeclProc) then
          begin
          // found forward declaration
          DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
          if DeclProcScope.ImplProc<>nil then
            RaiseMsg(20180318222430,nDuplicateIdentifier,sDuplicateIdentifier,
              [DeclProcScope.ImplProc.Name,GetElementSourcePosStr(DeclProcScope.ImplProc)],Proc);
          // connect
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.FinishProcedureHeader forward found: Proc2=',GetTreeDbg(DeclProc),' ',GetElementSourcePosStr(DeclProc),' IsForward=',DeclProc.IsForward,' Parent=',GetObjName(DeclProc.Parent));
          {$ENDIF}
          CheckProcSignatureMatch(DeclProc,Proc,true);
          DeclProcScope.ImplProc:=Proc;
          ProcScope:=Proc.CustomData as TPasProcedureScope;
          ProcScope.DeclarationProc:=DeclProc;
          // remove ImplProc from scope
          (ParentScope as TPasIdentifierScope).RemoveLocalIdentifier(Proc);
          // replace arguments with declaration arguments
          ReplaceProcScopeImplArgsWithDeclArgs(ProcScope);
          exit;
          end
        else
          RaiseMsg(20180318220543,nDuplicateIdentifier,sDuplicateIdentifier,
            [DeclProc.Name,GetElementSourcePosStr(DeclProc)],Proc);
        end;
      end
    else
      begin
      // forward declaration
      ProcScope:=Proc.CustomData as TPasProcedureScope;
      // ToDo: store the scanner flags *before* it has parsed the token after the proc
      StoreScannerFlagsInProc(ProcScope);
      end;

    // check for invalid overloads
    FindData:=Default(TFindOverloadProcData);
    FindData.Proc:=Proc;
    FindData.Args:=Proc.ProcType.Args;
    FindData.Kind:=fopkProc;
    Abort:=false;
    IterateElements(ProcName,@OnFindOverloadProc,@FindData,Abort);
    end
  else if El.Name<>'' then
    begin
    // finished proc type, e.g. type TProcedure = procedure;
    end
  else
    RaiseNotYetImplemented(20160922163411,El.Parent);
end;

procedure TPasResolver.FinishMethodDeclHeader(Proc: TPasProcedure);

  procedure VisibilityLowered(Proc, OverloadProc: TPasProcedure);
  begin
    LogMsg(20170325004215,mtNote,nVirtualMethodXHasLowerVisibility,
      sVirtualMethodXHasLowerVisibility,[Proc.Name,
        VisibilityNames[Proc.Visibility],OverloadProc.Parent.Name,
        VisibilityNames[OverloadProc.Visibility]],Proc);
    Proc.Visibility:=OverloadProc.Visibility;
  end;

  {$IF defined(fpc) and (FPC_FULLVERSION<30101)}
  procedure Delete(var A: TArrayOfPasProcedure; Index, Count: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      RaiseInternalError(20171227121538);
    if Index+Count>length(A) then
      RaiseInternalError(20171227121156);
    for i:=Index+Count to length(A)-1 do
      A[i-Count]:=A[i];
    SetLength(A,length(A)-Count);
  end;

  procedure Insert(Item: TPasProcedure; var A: TArrayOfPasProcedure; Index: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      RaiseInternalError(20171227121544);
    if Index>length(A) then
      RaiseInternalError(20171227121558);
    SetLength(A,length(A)+1);
    for i:=length(A)-1 downto Index+1 do
      A[i]:=A[i-1];
    A[Index]:=Item;
  end;
  {$ENDIF}

var
  Abort: boolean;
  ClassScope: TPasClassScope;
  FindData: TFindOverloadProcData;
  OverloadProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  i: Integer;
begin
  Proc.ProcType.IsOfObject:=true;
  ProcScope:=TopScope as TPasProcedureScope;
  // ToDo: store the scanner flags *before* it has parsed the token after the proc
  StoreScannerFlagsInProc(ProcScope);
  ClassScope:=Scopes[ScopeCount-2] as TPasClassScope;
  ProcScope.ClassScope:=ClassScope;
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=Proc;
  FindData.Args:=Proc.ProcType.Args;
  FindData.Kind:=fopkMethod;
  Abort:=false;
  ClassScope.IterateElements(Proc.Name,ClassScope,@OnFindOverloadProc,@FindData,Abort);

  if FindData.Found=nil then
    begin
    // no overload
    if Proc.IsOverride then
      RaiseMsg(20170216151702,nNoMethodInAncestorToOverride,
        sNoMethodInAncestorToOverride,[GetProcTypeDescription(Proc.ProcType)],Proc.ProcType);
    end
  else
    begin
    // overload found
    OverloadProc:=FindData.Found;

    // Note: 'inherited;' needs the OverriddenProc, even without 'override' modifier
    ProcScope.OverriddenProc:=OverloadProc;

    if Proc.IsOverride then
      begin
      if (not OverloadProc.IsVirtual) and (not OverloadProc.IsOverride) then
        // the OverloadProc fits the signature, but is not virtual
        RaiseMsg(20170216151708,nNoMethodInAncestorToOverride,
          sNoMethodInAncestorToOverride,[GetProcTypeDescription(Proc.ProcType)],Proc.ProcType);
      // override a virtual method
      CheckProcSignatureMatch(OverloadProc,Proc,false);
      // check visibility
      if Proc.Visibility<>OverloadProc.Visibility then
        case Proc.Visibility of
          visPrivate,visStrictPrivate:
            if not (OverloadProc.Visibility in [visPrivate,visStrictPrivate]) then
              VisibilityLowered(Proc,OverloadProc);
          visProtected,visStrictProtected:
            if not (OverloadProc.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublic:
            if not (OverloadProc.Visibility in [visPrivate..visPublic,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublished: ;
        else
          RaiseNotYetImplemented(20170325003315,Proc,'visibility');
        end;
      // check name case
      if proFixCaseOfOverrides in Options then
        Proc.Name:=OverloadProc.Name;
      // remove abstract
      if OverloadProc.IsAbstract then
        for i:=length(ClassScope.AbstractProcs)-1 downto 0 do
          if ClassScope.AbstractProcs[i]=OverloadProc then
            Delete(ClassScope.AbstractProcs,i,1);
      end;
    end;
  // add abstract
  if Proc.IsAbstract then
    Insert(Proc,ClassScope.AbstractProcs,length(ClassScope.AbstractProcs));
end;

procedure TPasResolver.FinishMethodImplHeader(ImplProc: TPasProcedure);
var
  ProcName: String;
  CurClassType: TPasClassType;
  ImplProcScope, DeclProcScope: TPasProcedureScope;
  DeclProc: TPasProcedure;
  CurClassScope: TPasClassScope;
  SelfArg: TPasArgument;
  p: Integer;
begin
  if ImplProc.IsExternal then
    RaiseMsg(20170216151715,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ImplProc),'external'],ImplProc);
  if ImplProc.IsExported then
    RaiseMsg(20170216151717,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ImplProc),'export'],ImplProc);

  ProcName:=ImplProc.Name;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader searching declaration "',ProcName,'" ...');
  {$ENDIF}
  ImplProc.ProcType.IsOfObject:=true;

  repeat
    p:=Pos('.',ProcName);
    if p<1 then break;
    Delete(ProcName,1,p);
  until false;

  // search ImplProc in class
  if not IsValidIdent(ProcName) then
    RaiseNotYetImplemented(20160922163421,ImplProc.ProcType);

  // search proc in class
  ImplProcScope:=ImplProc.CustomData as TPasProcedureScope;
  CurClassScope:=ImplProcScope.ClassScope;
  if CurClassScope=nil then
    RaiseInternalError(20161013172346);
  CurClassType:=NoNil(CurClassScope.Element) as TPasClassType;

  DeclProc:=FindProcOverload(ProcName,ImplProc,CurClassScope);
  if DeclProc=nil then
    RaiseIdentifierNotFound(20170216151720,ImplProc.Name,ImplProc.ProcType);
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;

  // connect method declaration and body
  if DeclProcScope.ImplProc<>nil then
    RaiseMsg(20180212094546,nDuplicateIdentifier,sDuplicateIdentifier,
      [DeclProcScope.ImplProc.Name,GetElementSourcePosStr(DeclProcScope.ImplProc)],
      ImplProc);
  if DeclProc.IsAbstract then
    RaiseMsg(20170216151722,nAbstractMethodsMustNotHaveImplementation,sAbstractMethodsMustNotHaveImplementation,[],ImplProc);
  if DeclProc.IsExternal then
    RaiseXExpectedButYFound(20170216151725,'method','external method',ImplProc);
  CheckProcSignatureMatch(DeclProc,ImplProc,true);
  ImplProcScope.DeclarationProc:=DeclProc;
  DeclProcScope.ImplProc:=ImplProc;

  // replace arguments in scope with declaration arguments
  ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope);

  if not DeclProc.IsStatic then
    begin
    // add 'Self'
    if (DeclProc.ClassType=TPasClassConstructor)
        or (DeclProc.ClassType=TPasClassDestructor)
        or (DeclProc.ClassType=TPasClassProcedure)
        or (DeclProc.ClassType=TPasClassFunction) then
      begin
      if not DeclProc.IsStatic then
        begin
        // 'Self' in a class proc is the hidden classtype argument
        SelfArg:=TPasArgument.Create('Self',DeclProc);
        ImplProcScope.SelfArg:=SelfArg;
        {$IFDEF CheckPasTreeRefCount}SelfArg.RefIds.Add('TPasProcedureScope.SelfArg');{$ENDIF}
        SelfArg.Access:=argConst;
        SelfArg.ArgType:=CurClassScope.CanonicalClassOf;
        SelfArg.ArgType.AddRef{$IFDEF CheckPasTreeRefCount}('TPasArgument.ArgType'){$ENDIF};
        AddIdentifier(ImplProcScope,'Self',SelfArg,pikSimple);
        end;
      end
    else
      begin
      // 'Self' in a proc is the hidden instance argument
      SelfArg:=TPasArgument.Create('Self',DeclProc);
      ImplProcScope.SelfArg:=SelfArg;
      {$IFDEF CheckPasTreeRefCount}SelfArg.RefIds.Add('TPasProcedureScope.SelfArg');{$ENDIF}
      SelfArg.Access:=argConst;
      SelfArg.ArgType:=CurClassType;
      CurClassType.AddRef{$IFDEF CheckPasTreeRefCount}('TPasArgument.ArgType'){$ENDIF};
      AddIdentifier(ImplProcScope,'Self',SelfArg,pikSimple);
      end;
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader END of searching proc "',ImplProc.Name,'" ...');
  {$ENDIF}
end;

procedure TPasResolver.FinishExceptOnExpr;
var
  El: TPasImplExceptOn;
  ResolvedType: TPasResolverResult;
begin
  CheckTopScope(TPasExceptOnScope);
  El:=TPasImplExceptOn(FTopScope.Element);
  ComputeElement(El.TypeEl,ResolvedType,[rcType]);
  CheckIsClass(El.TypeEl,ResolvedType);
end;

procedure TPasResolver.FinishExceptOnStatement;
begin
  //writeln('TPasResolver.FinishExceptOnStatement START');
  CheckTopScope(TPasExceptOnScope);
  ResolveImplElement(TPasImplExceptOn(FTopScope.Element).Body);
  PopScope;
end;

procedure TPasResolver.FinishDeclaration(El: TPasElement);
var
  C: TClass;
begin
  C:=El.ClassType;
  if (C=TPasVariable) or (C=TPasConst) then
    FinishVariable(TPasVariable(El))
  else if C=TPasProperty then
    FinishPropertyOfClass(TPasProperty(El))
  else if C=TPasArgument then
    FinishArgument(TPasArgument(El))
  else if C=TPasMethodResolution then
    FinishMethodResolution(TPasMethodResolution(El))
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishDeclaration ',GetObjName(El));
    {$ENDIF}
    RaiseNotYetImplemented(20180127121557,El);
    end;
end;

procedure TPasResolver.FinishVariable(El: TPasVariable);
var
  ResolvedAbs: TPasResolverResult;
  C: TClass;
  Value: TResEvalValue;
begin
  if (El.Visibility=visPublished) then
    begin
    if [vmClass,vmStatic,vmCVar]*El.VarModifiers<>[] then
      RaiseMsg(20170403223837,nSymbolCannotBePublished,sSymbolCannotBePublished,[],El);
    end;
  if El.Expr<>nil then
    ResolveExpr(El.Expr,rraRead);
  if El.VarType<>nil then
    begin
    if El.Expr<>nil then
      CheckAssignCompatibility(El,El.Expr,true);
    end
  else if El.Expr<>nil then
    begin
    // no VarType, has Expr, e.g. const a = Expr
    Value:=Eval(El.Expr,[refConstExt]); // e.g. const Tau = 2*PI
    ReleaseEvalValue(Value);
    end;
  if El.AbsoluteExpr<>nil then
    begin
    if El.ClassType=TPasConst then
      RaiseMsg(20180201225530,nXModifierMismatchY,sXModifierMismatchY,
        ['absolute','const'],El.AbsoluteExpr);
    if El.VarType=nil then
      RaiseMsg(20171225235125,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    if vmExternal in El.VarModifiers then
      RaiseMsg(20171226104221,nXModifierMismatchY,sXModifierMismatchY,
        ['absolute','external'],El.AbsoluteExpr);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishVariable El=',GetObjName(El),' Absolute="',GetObjName(El.AbsoluteExpr),'"');
    {$ENDIF}
    ResolveExpr(El.AbsoluteExpr,rraRead);
    ComputeElement(El.AbsoluteExpr,ResolvedAbs,[rcNoImplicitProc]);
    if (not (rrfReadable in ResolvedAbs.Flags))
        or (ResolvedAbs.IdentEl=nil) then
      RaiseVarExpected(20171225234734,El.AbsoluteExpr,ResolvedAbs.IdentEl);
    C:=ResolvedAbs.IdentEl.ClassType;
    if (C=TPasVariable)
        or (C=TPasArgument)
        or ((C=TPasConst) and (TPasConst(ResolvedAbs.IdentEl).VarType<>nil)) then
    else
      RaiseMsg(20171225235203,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    if not (rrfReadable in ResolvedAbs.Flags) then
      RaiseVarExpected(20171225235249,El.AbsoluteExpr,ResolvedAbs.IdentEl);
    // check for cycles
    if ResolvedAbs.IdentEl=El then
      RaiseMsg(20171226000703,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    end;
  if El.VarType<>nil then
    EmitTypeHints(El,El.VarType);
end;

procedure TPasResolver.FinishPropertyOfClass(PropEl: TPasProperty);
var
  PropType: TPasType;
  ClassScope: TPasClassScope;
  AncestorProp: TPasProperty;
  IndexExpr: TPasExpr;

  procedure GetPropType;
  var
    AncEl: TPasElement;
  begin
    if PropType<>nil then exit;
    AncEl:=nil;
    if ClassScope.AncestorScope<>nil then
      AncEl:=ClassScope.AncestorScope.FindElement(PropEl.Name);
    if AncEl is TPasProperty then
      begin
      // override or redeclaration property
      AncestorProp:=TPasProperty(AncEl);
      TPasPropertyScope(PropEl.CustomData).AncestorProp:=AncestorProp;
      if proFixCaseOfOverrides in Options then
        PropEl.Name:=AncestorProp.Name;
      end
    else
      AncestorProp:=nil;

    if PropEl.VarType<>nil then
      begin
      // new property or redeclaration
      PropType:=PropEl.VarType;
      end
    else
      begin
      // property override
      if AncestorProp=nil then
        RaiseMsg(20170216151741,nNoPropertyFoundToOverride,sNoPropertyFoundToOverride,[],PropEl);
      // check property versus class property
      if PropEl.ClassType<>AncestorProp.ClassType then
        RaiseXExpectedButYFound(20170216151744,GetElementTypeName(AncestorProp),GetElementTypeName(PropEl),PropEl);
      // get inherited type
      PropType:=GetPasPropertyType(AncestorProp);
      // update DefaultProperty
      if (ClassScope.DefaultProperty=AncestorProp) then
        ClassScope.DefaultProperty:=PropEl;
      end;
  end;

  procedure CheckIndexArg(ArgNo: integer; const IndexResolved: TPasResolverResult;
    ProcArg: TPasArgument; ErrorEl: TPasElement);
  var
    ProcArgResolved: TPasResolverResult;
  begin
    // check access: const, ...
    if not (ProcArg.Access in [argDefault,argConst]) then
      RaiseMsg(20170924202437,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        [IntToStr(ArgNo),AccessDescriptions[ProcArg.Access],
         AccessDescriptions[argConst]],ErrorEl);
    // check argument type
    if ProcArg.ArgType=nil then
      RaiseMsg(20170924202531,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        [IntToStr(ArgNo),'untyped',GetTypeDescription(IndexResolved)],ErrorEl)
    else
      begin
      if CheckParamCompatibility(IndexExpr,ProcArg,ArgNo,true)=cIncompatible then
        begin
        ComputeElement(ProcArg.ArgType,ProcArgResolved,[rcType]);
        RaiseIncompatibleTypeRes(20170924203829,nIncompatibleTypeArgNo,
          [IntToStr(ArgNo)],ProcArgResolved,IndexResolved,ErrorEl);
        end;
      end;
  end;

  procedure CheckArgs(Proc: TPasProcedure; const IndexVal: TResEvalValue;
    const IndexResolved: TPasResolverResult; ErrorEl: TPasElement);
  var
    ArgNo: Integer;
    PropArg, ProcArg: TPasArgument;
    PropArgResolved, ProcArgResolved: TPasResolverResult;
    NeedCheckingAccess: Boolean;
  begin
    ArgNo:=0;
    while ArgNo<PropEl.Args.Count do
      begin
      if ArgNo>=Proc.ProcType.Args.Count then
        RaiseMsg(20170216151805,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],ErrorEl);
      PropArg:=TPasArgument(PropEl.Args[ArgNo]);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      inc(ArgNo);

      // check access: var, const, ...
      NeedCheckingAccess:=false;
      if PropArg.Access<>ProcArg.Access then
        begin

        if (PropArg.Access in [argDefault, argConst])
            and (ProcArg.Access in [argDefault, argConst]) then
          begin
          // passing an arg as default to const or const to default
          if (PropArg.ArgType<>nil)
              and (ProcArg.ArgType<>nil) then
            NeedCheckingAccess:=true;
          end;
        if not NeedCheckingAccess then
          RaiseMsg(20170216151808,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),AccessDescriptions[ProcArg.Access],
             AccessDescriptions[PropArg.Access]],ErrorEl);
        end;

      // check argument type
      if PropArg.ArgType=nil then
        begin
        if ProcArg.ArgType<>nil then
          RaiseMsg(20170216151811,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),GetElementTypeName(ProcArg.ArgType),'untyped'],ErrorEl);
        end
      else if ProcArg.ArgType=nil then
        RaiseMsg(20170216151813,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),'untyped',GetElementTypeName(PropArg.ArgType)],ErrorEl)
      else
        begin
        ComputeElement(PropArg,PropArgResolved,[rcNoImplicitProc]);
        ComputeElement(ProcArg,ProcArgResolved,[rcNoImplicitProc]);

        if (PropArgResolved.BaseType<>ProcArgResolved.BaseType) then
          RaiseMsg(20170216151816,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),BaseTypeNames[ProcArgResolved.BaseType],BaseTypeNames[PropArgResolved.BaseType]],ErrorEl);
        if PropArgResolved.LoTypeEl=nil then
          RaiseInternalError(20161010125255);
        if ProcArgResolved.LoTypeEl=nil then
          RaiseInternalError(20161010125304);
        if not IsSameType(PropArgResolved.HiTypeEl,ProcArgResolved.HiTypeEl,prraSimple) then
          RaiseIncompatibleType(20170216151819,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ProcArgResolved.HiTypeEl,PropArgResolved.HiTypeEl,ErrorEl);
        end;

        if NeedCheckingAccess then
          begin
          // passing an arg as default to const or const to default
          // e.g.
          //   function GetItems(const i: integer): byte;
          //   property Items[i: integer]: byte read GetItems;
          // => allowed for simple types
          if not (PropArgResolved.BaseType in (btAllBooleans+btAllInteger+btAllStringAndChars+btAllFloats)) then
            RaiseMsg(20181007181647,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
              [IntToStr(ArgNo),AccessDescriptions[ProcArg.Access],
               AccessDescriptions[PropArg.Access]],ErrorEl);
          end;
      end;

    if IndexVal<>nil then
      begin
      if ArgNo>=Proc.ProcType.Args.Count then
        RaiseMsg(20170924202334,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],ErrorEl);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      CheckIndexArg(ArgNo,IndexResolved,ProcArg,ErrorEl);
      end;
  end;

  procedure CheckImplements;
  var
    i, j: Integer;
    Expr: TPasExpr;
    ResolvedEl: TPasResolverResult;
    aClass, PropClassType: TPasClassType;
    IntfType, OrigIntfType, PropTypeRes: TPasType;
    o: TObject;
  begin
    if not (PropEl.Parent is TPasClassType) then
      RaiseInternalError(20180323172125,GetElementDbgPath(PropEl));
    aClass:=TPasClassType(PropEl.Parent);
    if PropEl.Args.Count>0 then
      RaiseMsg(20180323170952,nImplementsDoesNotSupportArrayProperty,
        sImplementsDoesNotSupportArrayProperty,[],PropEl.Implements[0]);
    if IndexExpr<>nil then
      RaiseMsg(20180323171354,nImplementsDoesNotSupportIndex,
        sImplementsDoesNotSupportIndex,[],PropEl.Implements[0]);
    if GetPasPropertyGetter(PropEl)=nil then
      RaiseMsg(20180323221322,nImplPropMustHaveReadSpec,
        sImplPropMustHaveReadSpec,[],PropEl.Implements[0]);
    for i:=0 to length(PropEl.Implements)-1 do
      begin
      // resolve expression
      Expr:=PropEl.Implements[i];
      ResolveExpr(Expr,rraRead);
      // check expr is an interface type
      ComputeElement(Expr,ResolvedEl,[rcType,rcNoImplicitProc]);
      if not (ResolvedEl.IdentEl is TPasType) then
        if ResolvedEl.IdentEl=nil then
          RaiseXExpectedButYFound(20180323171911,'interface',
            GetElementTypeName(ResolvedEl.LoTypeEl),Expr)
        else
          RaiseXExpectedButYFound(20180323224846,'interface',
            GetElementTypeName(ResolvedEl.IdentEl),Expr);
      OrigIntfType:=TPasType(ResolvedEl.IdentEl);
      IntfType:=ResolveAliasType(OrigIntfType);
      if (not (IntfType is TPasClassType))
          or (TPasClassType(IntfType).ObjKind<>okInterface) then
        RaiseXExpectedButYFound(20180323172904,'interface',
          GetElementTypeName(OrigIntfType),Expr);
      // check it is one of the current implemented interfaces (not of ancestors)
      j:=IndexOfImplementedInterface(aClass,IntfType);
      if j<0 then
        RaiseMsg(20180323172420,nImplementsUsedOnUnimplIntf,sImplementsUsedOnUnimplIntf,
          [OrigIntfType.Name],Expr);
      // check property type fits
      PropTypeRes:=ResolveAliasType(PropType);
      if not (PropTypeRes is TPasClassType) then
        RaiseMsg(20180323222334,nDoesNotImplementInterface,sDoesNotImplementInterface,
          [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      PropClassType:=TPasClassType(PropTypeRes);
      case PropClassType.ObjKind of
      okClass:
        // e.g. property Obj: ClassType read Getter implements IntfType
        // check ClassType or ancestors implements IntfType
        if GetClassImplementsIntf(PropClassType,TPasClassType(IntfType))=nil then
          RaiseMsg(20180323223324,nDoesNotImplementInterface,sDoesNotImplementInterface,
            [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      okInterface:
        // e.g. property IntfVar: IntfType read Getter implements IntfType2
        // check that IntfType is IntfType2
        if CheckClassIsClass(PropType,IntfType,Expr)=cIncompatible then
          RaiseIncompatibleType(20180323173746,nIncompatibleTypesGotExpected,
            [],OrigIntfType,PropType,Expr);
      else
        RaiseMsg(20180323222821,nDoesNotImplementInterface,sDoesNotImplementInterface,
          [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      end;
      // map
      o:=TObject(ClassScope.Interfaces[j]);
      if o is TPasProperty then
        RaiseMsg(20180323174240,nDuplicateImplementsForIntf,sDuplicateImplementsForIntf,
          [OrigIntfType.Name,GetElementSourcePosStr(TPasProperty(o))],Expr)
      else if o is TPasClassIntfMap then
        begin
        // properties are checked before method resolutions
        RaiseInternalError(20180323175919,GetElementDbgPath(PropEl));
        end
      else if o<>nil then
        RaiseInternalError(20180323174342,GetObjName(o))
      else
        ClassScope.Interfaces[j]:=PropEl;
      end;
  end;

  procedure CheckStoredAccessor(Expr: TPasExpr; const IndexVal: TResEvalValue;
    const IndexResolved: TPasResolverResult);
  var
    ResolvedEl: TPasResolverResult;
    Value: TResEvalValue;
    Proc: TPasProcedure;
    ResultType, TypeEl: TPasType;
    aVar: TPasVariable;
    IdentEl: TPasElement;
    ExpArgCnt: Integer;
    ProcArg: TPasArgument;
  begin
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,ResolvedEl,[rcNoImplicitProc]);
    IdentEl:=ResolvedEl.IdentEl;
    if IdentEl is TPasProcedure then
      begin
      // function
      Proc:=TPasProcedure(IdentEl);
      // check if member
      if not (Expr is TPrimitiveExpr) then
        RaiseXExpectedButYFound(20170923202002,'member function','foreign '+GetElementTypeName(Proc),Expr);
      if Proc.ClassType<>TPasFunction then
        RaiseXExpectedButYFound(20170216151925,'function',GetElementTypeName(Proc),Expr);
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if not IsBaseType(ResultType,btBoolean,true) then
        RaiseXExpectedButYFound(20170923200836,'function: boolean',
          'function:'+GetTypeDescription(ResultType),PropEl.StoredAccessor);
      // check arg count
      ExpArgCnt:=0;
      if IndexVal<>nil then
        inc(ExpArgCnt);
      if Proc.ProcType.Args.Count<>ExpArgCnt then
        RaiseMsg(20170923200840,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],Expr);
      if IndexVal<>nil then
        begin
        // check arg type
        ProcArg:=TPasArgument(Proc.ProcType.Args[0]);
        CheckIndexArg(1,IndexResolved,ProcArg,Expr);
        end;
      exit;
      end;
    if (IdentEl<>nil)
      and ((IdentEl.ClassType=TPasVariable)
        or ((IdentEl.ClassType=TPasConst) and not TPasConst(IdentEl).IsConst)) then
      begin
      // field
      aVar:=TPasVariable(IdentEl);
      // check if member
      if not (Expr is TPrimitiveExpr) then
        RaiseXExpectedButYFound(20170923202003,'member variable','foreign '+GetElementTypeName(aVar),Expr);
      // check type boolean
      TypeEl:=aVar.VarType;
      TypeEl:=ResolveAliasType(TypeEl);
      if not IsBaseType(TypeEl,btBoolean,true) then
        RaiseIncompatibleType(20170409214300,nIncompatibleTypesGotExpected,
          [],TypeEl,BaseTypes[btBoolean],Expr);
      // check class var
      if (vmClass in PropEl.VarModifiers)<>(vmClass in aVar.VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound(20170409214351,'class var','var',Expr)
        else
          RaiseXExpectedButYFound(20170409214359,'var','class var',Expr);
      exit;
      end;
    if (ResolvedEl.BaseType=btBoolean) and (ResolvedEl.ExprEl<>nil) then
      begin
      // try evaluating const boolean
      Value:=Eval(Expr,[refConst]);
      if Value<>nil then
        try
          if Value.Kind<>revkBool then
            RaiseXExpectedButYFound(20170923200256,'boolean',GetResolverResultDescription(ResolvedEl),Expr);
          exit;
        finally
          ReleaseEvalValue(Value);
        end;
      end;
    RaiseXExpectedButYFound(20170923194234,'identifier',GetResolverResultDescription(ResolvedEl),Expr);
  end;

var
  ResultType: TPasType;
  CurClassType: TPasClassType;
  AccEl: TPasElement;
  Proc: TPasProcedure;
  Arg: TPasArgument;
  PropArgCount, NeedArgCnt: Integer;
  PropTypeResolved, DefaultResolved, IndexResolved,
    AncIndexResolved: TPasResolverResult;
  m: TVariableModifier;
  IndexVal: TResEvalValue;
  AncIndexExpr: TPasExpr;
begin
  CheckTopScope(TPasPropertyScope);
  PopScope;

  if PropEl.Visibility=visPublished then
    for m in PropEl.VarModifiers do
      if not (m in [vmExternal]) then
        RaiseMsg(20170403224112,nInvalidXModifierY,sInvalidXModifierY,
          ['published property','"'+VariableModifierNames[m]+'"'],PropEl);

  PropType:=nil;
  CurClassType:=PropEl.Parent as TPasClassType;
  ClassScope:=NoNil(CurClassType.CustomData) as TPasClassScope;
  AncestorProp:=nil;
  GetPropType;
  IndexVal:=nil;
  try
    if PropEl.IndexExpr<>nil then
      begin
      // index specifier
      // -> check if simple value
      IndexExpr:=PropEl.IndexExpr;
      ResolveExpr(IndexExpr,rraRead);
      end
    else
      IndexExpr:=GetPasPropertyIndex(PropEl);
    if IndexExpr<>nil then
      begin
      ComputeElement(IndexExpr,IndexResolved,[rcConstant]);
      IndexVal:=Eval(IndexExpr,[refConst]);
      case IndexVal.Kind of
      revkBool,
      revkInt, revkUInt,
      revkFloat,
      revkCurrency,
      {$ifdef FPC_HAS_CPSTRING}
      revkString,
      {$endif}
      revkUnicodeString,
      revkEnum: ; // ok
      else
        RaiseXExpectedButYFound(20170924202837,'ordinal',GetTypeDescription(IndexResolved),PropEl.IndexExpr);
      end;
      if (PropEl.IndexExpr<>nil) and (PropEl.VarType=nil) then
        begin
        // check if index is compatible to ancestor index specifier
        AncIndexExpr:=GetPasPropertyIndex(AncestorProp);
        if AncIndexExpr=nil then
          begin
          // ancestor had no index specifier
          if PropEl.ReadAccessor=nil then
            begin
            AccEl:=GetPasPropertyGetter(AncestorProp);
            if AccEl is TPasProcedure then
              RaiseMsg(20171002144103,nAddingIndexSpecifierRequiresNewX,
                sAddingIndexSpecifierRequiresNewX,['read'],IndexExpr);
            end;
          if PropEl.WriteAccessor=nil then
            begin
            AccEl:=GetPasPropertySetter(AncestorProp);
            if AccEl is TPasProcedure then
              RaiseMsg(20171002144419,nAddingIndexSpecifierRequiresNewX,
                sAddingIndexSpecifierRequiresNewX,['write'],IndexExpr);
            end;
          if PropEl.StoredAccessor=nil then
            begin
            AccEl:=GetPasPropertyStoredExpr(AncestorProp);
            if AccEl<>nil then
              begin
              ComputeElement(AccEl,AncIndexResolved,[rcNoImplicitProc]);
              if AncIndexResolved.IdentEl is TPasProcedure then
                RaiseMsg(20171002144644,nAddingIndexSpecifierRequiresNewX,
                  sAddingIndexSpecifierRequiresNewX,['stored'],IndexExpr);
              end;
            end;
          end
        else
          // ancestor had already an index specifier -> check same type
          CheckEqualElCompatibility(PropEl.IndexExpr,AncIndexExpr,PropEl.IndexExpr,true);
        end;
      end;

    if PropEl.ReadAccessor<>nil then
      begin
      // check compatibility
      AccEl:=ResolveAccessor(PropEl.ReadAccessor);
      if (AccEl.ClassType=TPasVariable) or (AccEl.ClassType=TPasConst) then
        begin
        if (PropEl.Args.Count>0) then
          RaiseXExpectedButYFound(20170216151823,'function',GetElementTypeName(AccEl),PropEl.ReadAccessor);
        if not IsSameType(TPasVariable(AccEl).VarType,PropType,prraAlias) then
          RaiseIncompatibleType(20170216151826,nIncompatibleTypesGotExpected,
            [],PropType,TPasVariable(AccEl).VarType,PropEl.ReadAccessor);
        if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
          if vmClass in PropEl.VarModifiers then
            RaiseXExpectedButYFound(20170216151828,'class var','var',PropEl.ReadAccessor)
          else
            RaiseXExpectedButYFound(20170216151831,'var','class var',PropEl.ReadAccessor);
        end
      else if AccEl is TPasProcedure then
        begin
        // check function
        Proc:=TPasProcedure(AccEl);
        if (vmClass in PropEl.VarModifiers) then
          begin
          if Proc.ClassType<>TPasClassFunction then
            RaiseXExpectedButYFound(20170216151834,'class function',GetElementTypeName(Proc),PropEl.ReadAccessor);
          if Proc.IsStatic=(proClassPropertyNonStatic in Options) then
            if Proc.IsStatic then
              RaiseMsg(20170216151837,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],PropEl.ReadAccessor)
            else
              RaiseMsg(20170216151839,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.ReadAccessor);
          end
        else
          begin
          if Proc.ClassType<>TPasFunction then
            RaiseXExpectedButYFound(20170216151842,'function',GetElementTypeName(Proc),PropEl.ReadAccessor);
          end;
        // check function result type
        ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
        if not IsSameType(ResultType,PropType,prraAlias) then
          RaiseXExpectedButYFound(20170216151844,'function result '+GetTypeDescription(PropType,true),
            GetTypeDescription(ResultType,true),PropEl.ReadAccessor);
        // check args
        CheckArgs(Proc,IndexVal,IndexResolved,PropEl.ReadAccessor);
        NeedArgCnt:=PropEl.Args.Count;
        if IndexVal<>nil then
          inc(NeedArgCnt);
        if Proc.ProcType.Args.Count<>NeedArgCnt then
          RaiseMsg(20170216151847,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
            [Proc.Name],PropEl.ReadAccessor);
        end
      else
        RaiseXExpectedButYFound(20170216151850,'variable',GetElementTypeName(AccEl),PropEl.ReadAccessor);
      end;

    if PropEl.WriteAccessor<>nil then
      begin
      // check compatibility
      AccEl:=ResolveAccessor(PropEl.WriteAccessor);
      if (AccEl.ClassType=TPasVariable)
          or ((AccEl.ClassType=TPasConst) and (not TPasConst(AccEl).IsConst)) then
        begin
        if (PropEl.Args.Count>0) then
          RaiseXExpectedButYFound(20170216151852,'procedure',GetElementTypeName(AccEl),PropEl.WriteAccessor);
        if not IsSameType(TPasVariable(AccEl).VarType,PropType,prraAlias) then
          RaiseIncompatibleType(20170216151855,nIncompatibleTypesGotExpected,
            [],PropType,TPasVariable(AccEl).VarType,PropEl.WriteAccessor);
        if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
          if vmClass in PropEl.VarModifiers then
            RaiseXExpectedButYFound(20170216151858,'class var','var',PropEl.WriteAccessor)
          else
            RaiseXExpectedButYFound(20170216151900,'var','class var',PropEl.WriteAccessor);
        end
      else if AccEl is TPasProcedure then
        begin
        // check procedure
        Proc:=TPasProcedure(AccEl);
        if (vmClass in PropEl.VarModifiers) then
          begin
          if Proc.ClassType<>TPasClassProcedure then
            RaiseXExpectedButYFound(20170216151903,'class procedure',GetElementTypeName(Proc),PropEl.WriteAccessor);
            if Proc.IsStatic=(proClassPropertyNonStatic in Options) then
              if Proc.IsStatic then
                RaiseMsg(20170216151905,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],PropEl.WriteAccessor)
              else
                RaiseMsg(20170216151906,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.WriteAccessor);
          end
        else
          begin
          if Proc.ClassType<>TPasProcedure then
            RaiseXExpectedButYFound(20170216151910,'procedure',GetElementTypeName(Proc),PropEl.WriteAccessor);
          end;
        // check args
        CheckArgs(Proc,IndexVal,IndexResolved,PropEl.ReadAccessor);
        // check write arg
        PropArgCount:=PropEl.Args.Count;
        if IndexVal<>nil then
          inc(PropArgCount);
        if Proc.ProcType.Args.Count<>PropArgCount+1 then
          RaiseMsg(20170216151913,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
            [Proc.Name],PropEl.WriteAccessor);
        Arg:=TPasArgument(Proc.ProcType.Args[PropArgCount]);
        if not (Arg.Access in [argDefault,argConst]) then
          RaiseMsg(20170216151917,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(PropArgCount+1),AccessDescriptions[Arg.Access],
             AccessDescriptions[argConst]],PropEl.WriteAccessor);
        if not IsSameType(Arg.ArgType,PropType,prraAlias) then
          RaiseIncompatibleType(20170216151919,nIncompatibleTypeArgNo,
            [IntToStr(PropArgCount+1)],Arg.ArgType,PropType,PropEl.WriteAccessor);
        end
      else
        RaiseXExpectedButYFound(20170216151921,'variable',GetElementTypeName(AccEl),PropEl.WriteAccessor);
      end
    else if (PropEl.ReadAccessor=nil) and (PropEl.VarType<>nil) then
      RaiseMsg(20180519173551,nPropertyMustHaveReadOrWrite,sPropertyMustHaveReadOrWrite,[],PropEl);

    if length(PropEl.Implements)>0 then
      CheckImplements;

    if PropEl.StoredAccessor<>nil then
      begin
      // check compatibility
      CheckStoredAccessor(PropEl.StoredAccessor,IndexVal,IndexResolved);
      end;

    if PropEl.DefaultExpr<>nil then
      begin
      // check compatibility with type
      ResolveExpr(PropEl.DefaultExpr,rraRead);
      ComputeElement(PropEl.DefaultExpr,DefaultResolved,[rcConstant]);
      ComputeElement(PropType,PropTypeResolved,[rcType]);
      PropTypeResolved.IdentEl:=PropEl;
      PropTypeResolved.Flags:=[rrfReadable];
      CheckEqualResCompatibility(PropTypeResolved,DefaultResolved,PropEl.DefaultExpr,true);
      end;
    if PropEl.IsDefault then
      begin
      // set default array property
      if (ClassScope.DefaultProperty<>nil)
          and (ClassScope.DefaultProperty.Parent=PropEl.Parent) then
        RaiseMsg(20170216151938,nOnlyOneDefaultPropertyIsAllowed,sOnlyOneDefaultPropertyIsAllowed,[],PropEl);
      ClassScope.DefaultProperty:=PropEl;
      end;
    EmitTypeHints(PropEl,PropEl.VarType);
  finally
    ReleaseEvalValue(IndexVal);
  end;
end;

procedure TPasResolver.FinishArgument(El: TPasArgument);
begin
  if El.ValueExpr<>nil then
    begin
    ResolveExpr(El.ValueExpr,rraRead);
    if El.ArgType<>nil then
      CheckAssignCompatibility(El,El.ValueExpr,true);
    end;
  EmitTypeHints(El,El.ArgType);
end;

procedure TPasResolver.FinishAncestors(aClass: TPasClassType);
// called when the ancestor and interface list of a class has been parsed,
// before parsing the class elements
var
  DirectAncestor: TPasType; // e.g. TPasAliasType or TPasClassType
  AncestorClassEl: TPasClassType;

  procedure FindDefaultAncestor(const DefAncestorName, Expected: string);
  var
    CurEl: TPasElement;
  begin
    AncestorClassEl:=nil;
    if (CompareText(aClass.Name,DefAncestorName)=0) then exit;
    CurEl:=FindElementWithoutParams(DefAncestorName,aClass,false);
    if not (CurEl is TPasType) then
      RaiseXExpectedButYFound(20180321150128,Expected,GetElementTypeName(CurEl),aClass);
    DirectAncestor:=TPasType(CurEl);
    CurEl:=ResolveAliasType(DirectAncestor);
    if not (CurEl is TPasClassType) then
      RaiseXExpectedButYFound(20170216151941,Expected,GetElementTypeName(DirectAncestor),aClass);
    AncestorClassEl:=TPasClassType(CurEl);
  end;

var
  ClassScope, AncestorClassScope: TPasClassScope;
  AncestorType, El: TPasType;
  i: Integer;
  aModifier, DefAncestorName: String;
  IsSealed: Boolean;
  CanonicalSelf: TPasClassOfType;
  Decl: TPasElement;
  j: integer;
  IntfType, IntfTypeRes: TPasType;
  ResIntfList, Members: TFPList;
begin
  if aClass.IsForward then
    begin
    // check for duplicate forwards
    if aClass.Parent is TPasDeclarations then
      Members:=TPasDeclarations(aClass.Parent).Declarations
    else if aClass.Parent.ClassType=TPasClassType then
      Members:=TPasClassType(aClass.Parent).Members
    else
      RaiseNotYetImplemented(20180430141934,aClass,GetObjName(aClass.Parent));
    for i:=0 to Members.Count-1 do
      begin
      Decl:=TPasElement(Members[i]);
      if (CompareText(Decl.Name,aClass.Name)=0)
          and (Decl<>aClass) then
        RaiseMsg(20180212144132,nDuplicateIdentifier,sDuplicateIdentifier,
          [Decl.Name,GetElementSourcePosStr(Decl)],aClass);
      end;
    exit;
    end;

  case aClass.ObjKind of
  okClass:
    begin
    AncestorType:=ResolveAliasType(aClass.AncestorType);
    if (AncestorType is TPasClassType)
        and (TPasClassType(AncestorType).ObjKind=okInterface)
        and not (msDelphi in CurrentParser.CurrentModeswitches) then
      begin
      // e.g. type c = class(intf)
      aClass.Interfaces.Insert(0,aClass.AncestorType);
      aClass.AncestorType:=nil;
      end;
    end;
  okInterface:
    begin
    if aClass.IsExternal then
      RaiseMsg(20180321115831,nIllegalQualifier,sIllegalQualifier,['external'],aClass);
    if not (aClass.InterfaceType in [citCom,citCorba]) then
      RaiseMsg(20180321143613,nIllegalQualifier,sIllegalQualifier,
        [CurrentParser.Scanner.CurrentValueSwitch[vsInterfaces]],aClass);
    end;
  else
    RaiseNotYetImplemented(20161010174638,aClass,'Kind='+ObjKindNames[aClass.ObjKind]);
  end;

  IsSealed:=false;
  for i:=0 to aClass.Modifiers.Count-1 do
    begin
    aModifier:=lowercase(aClass.Modifiers[i]);
    case aModifier of
    'sealed': IsSealed:=true;
    else
      RaiseMsg(20170320190619,nIllegalQualifier,sIllegalQualifier,[aClass.Modifiers[i]],aClass);
    end;
    end;

  AncestorClassEl:=nil;
  DirectAncestor:=aClass.AncestorType;
  AncestorType:=ResolveAliasType(DirectAncestor);

  if AncestorType=nil then
    begin
    if DirectAncestor<>nil then
      RaiseInternalError(20180321151851,GetObjName(DirectAncestor));
    // use default ancestor
    DefAncestorName:='';
    case aClass.ObjKind of
    okClass:
      begin
      DefAncestorName:='TObject';
      if (CompareText(aClass.Name,DefAncestorName)=0) or aClass.IsExternal then
        begin
          // ok, no ancestor
          AncestorClassEl:=nil;
        end
      else
        begin
        // search default ancestor TObject
        FindDefaultAncestor(DefAncestorName,'class type');
        if TPasClassType(AncestorClassEl).ObjKind<>okClass then
          RaiseXExpectedButYFound(20180321145626,'class type',GetElementTypeName(AncestorClassEl),aClass);
        end;
      end;
    okInterface:
      begin
      if aClass.InterfaceType=citCom then
        begin
        if msDelphi in CurrentParser.CurrentModeswitches then
          DefAncestorName:='IInterface'
        else
          DefAncestorName:='IUnknown';
        if SameText(DefAncestorName,aClass.Name) then
          AncestorClassEl:=nil
        else
          begin
          // search default ancestor interface
          FindDefaultAncestor(DefAncestorName,'interface type');
          if TPasClassType(AncestorClassEl).ObjKind<>okInterface then
            RaiseXExpectedButYFound(20180321145725,'interface type',
              GetElementTypeName(AncestorClassEl),aClass);
          end;
        end;
      end;
    end;
    end
  else if AncestorType.ClassType<>TPasClassType then
    RaiseXExpectedButYFound(20170216151944,'class type',GetTypeDescription(AncestorType),aClass)
  else if aClass=AncestorType then
    RaiseMsg(20170525125854,nAncestorCycleDetected,sAncestorCycleDetected,[],aClass)
  else
    begin
    AncestorClassEl:=TPasClassType(AncestorType);
    if AncestorClassEl.ObjKind<>aClass.ObjKind then
      begin
      RaiseXExpectedButYFound(20180321152107,GetElementTypeName(aClass)+' type',
        GetElementTypeName(AncestorClassEl)+' type',aClass);
      end
    else
      EmitTypeHints(aClass,AncestorClassEl);
    end;

  AncestorClassScope:=nil;
  if AncestorClassEl=nil then
    begin
    // root class e.g. TObject, IUnknown
    end
  else
    begin
    // inherited class
    if AncestorClassEl.IsForward then
      RaiseMsg(20170216151947,nCantUseForwardDeclarationAsAncestor,
        sCantUseForwardDeclarationAsAncestor,[AncestorClassEl.Name],aClass);
    if aClass.IsExternal and not AncestorClassEl.IsExternal then
      RaiseMsg(20170321144035,nAncestorIsNotExternal,sAncestorIsNotExternal,
        [AncestorClassEl.Name],aClass);
    AncestorClassScope:=AncestorClassEl.CustomData as TPasClassScope;
    if pcsfSealed in AncestorClassScope.Flags then
      RaiseMsg(20170320191735,nCannotCreateADescendantOfTheSealedXY,
        sCannotCreateADescendantOfTheSealedXY,
        [GetElementTypeName(AncestorClassEl),AncestorClassEl.Name],aClass);
    // check for cycle
    El:=AncestorClassEl;
    repeat
      if El=aClass then
        RaiseMsg(20170216151949,nAncestorCycleDetected,sAncestorCycleDetected,[],aClass);
      if (El.ClassType=TPasAliasType)
      or (El.ClassType=TPasTypeAliasType)
      then
        El:=TPasAliasType(El).DestType
      else if El.ClassType=TPasClassType then
        El:=TPasClassType(El).AncestorType;
    until El=nil;
    end;

  // start scope for elements
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FinishAncestors ',GetObjName(aClass.CustomData));
  {$ENDIF}
  PushScope(aClass,ScopeClass_Class);
  ClassScope:=TPasClassScope(TopScope);
  ClassScope.VisibilityContext:=aClass;
  Include(ClassScope.Flags,pcsfAncestorResolved);
  if IsSealed then
    Include(ClassScope.Flags,pcsfSealed);
  ClassScope.DirectAncestor:=DirectAncestor;
  if AncestorClassEl<>nil then
    begin
    ClassScope.AncestorScope:=AncestorClassScope;
    ClassScope.DefaultProperty:=AncestorClassScope.DefaultProperty;
    if pcsfPublished in AncestorClassScope.Flags then
      Include(ClassScope.Flags,pcsfPublished);
    ClassScope.AbstractProcs:=copy(AncestorClassScope.AbstractProcs);
    end;
  if bsTypeInfo in CurrentParser.Scanner.CurrentBoolSwitches then
    Include(ClassScope.Flags,pcsfPublished);
  if aClass.ObjKind=okClass then
    begin
    // create canonical class-of for the "Self" in class functions
    CanonicalSelf:=TPasClassOfType.Create('Self',aClass);
    ClassScope.CanonicalClassOf:=CanonicalSelf;
    {$IFDEF CheckPasTreeRefCount}CanonicalSelf.RefIds.Add('TPasClassScope.CanonicalClassOf');{$ENDIF}
    CanonicalSelf.DestType:=aClass;
    aClass.AddRef{$IFDEF CheckPasTreeRefCount}('TPasAliasType.DestType'){$ENDIF};
    CanonicalSelf.Visibility:=visStrictPrivate;
    CanonicalSelf.SourceFilename:=aClass.SourceFilename;
    CanonicalSelf.SourceLinenumber:=aClass.SourceLinenumber;
    end;

  // check interfaces
  if aClass.Interfaces.Count>0 then
    begin
    if not (aClass.ObjKind in [okClass]) then
      RaiseXExpectedButYFound(20180322001341,'one ancestor',
        IntToStr(1+aClass.Interfaces.Count),aClass);
    if aClass.IsExternal then
      RaiseMsg(20180324183641,nIllegalQualifier,sIllegalQualifier,['external'],aClass);
    ResIntfList:=TFPList.Create;
    try
      for i:=0 to aClass.Interfaces.Count-1 do
        begin
        IntfType:=TPasType(aClass.Interfaces[i]);
        IntfTypeRes:=ResolveAliasType(IntfType);
        if IntfTypeRes=nil then
          RaiseMsg(20180322140044,nCantUseForwardDeclarationAsAncestor,
            sCantUseForwardDeclarationAsAncestor,[IntfType.Name],aClass);
        if not (IntfTypeRes is TPasClassType) then
          RaiseXExpectedButYFound(20180322001051,'interface type',
            GetElementTypeName(IntfTypeRes)+' type',aClass);
        if TPasClassType(IntfTypeRes).ObjKind<>okInterface then
          RaiseXExpectedButYFound(20180322001143,'interface type',
            GetElementTypeName(IntfTypeRes)+' type',aClass);
        j:=ResIntfList.IndexOf(IntfTypeRes);
        if j>=0 then
          RaiseMsg(20180322001505,nDuplicateIdentifier,sDuplicateIdentifier,
            [IntfType.Name,IntToStr(j+1)],aClass); // todo: jump to interface list
        ResIntfList.Add(IntfTypeRes);
        end;
    finally
      ResIntfList.Free;
    end;
    // create interfaces maps
    ClassScope.Interfaces:=TFPList.Create;
    ClassScope.Interfaces.Count:=aClass.Interfaces.Count;
    end;
end;

procedure TPasResolver.FinishMethodResolution(El: TPasMethodResolution);
var
  ResolvedEl: TPasResolverResult;
  aClass, IntfType: TPasClassType;
  i: Integer;
  IntfProc: TPasProcedure;
  Expr: TPasExpr;
  ProcName: String;
  IntfScope: TPasClassScope;
  Identifier: TPasIdentifier;
begin
  // procedure InterfaceName.InterfaceProc = ...
  // check InterfaceName
  ResolveExpr(El.InterfaceName,rraRead);
  ComputeElement(El.InterfaceName,ResolvedEl,[rcType,rcNoImplicitProc]);
  if not (ResolvedEl.IdentEl is TPasType) then
    RaiseXExpectedButYFound(20180323132601,'interface type',
      GetResolverResultDescription(ResolvedEl),El.InterfaceName);
  aClass:=El.Parent as TPasClassType;
  i:=IndexOfImplementedInterface(aClass,TpasType(ResolvedEl.IdentEl));
  if i<0 then
    RaiseXExpectedButYFound(20180323133055,'interface type',
      GetResolverResultDescription(ResolvedEl),El.InterfaceName);
  IntfType:=TPasClassType(ResolveAliasType(TPasClassType(aClass.Interfaces[i])));
  // check InterfaceProc
  Expr:=El.InterfaceProc;
  if not (Expr is TPrimitiveExpr) then
    RaiseXExpectedButYFound(20180327152808,'method name',GetElementTypeName(Expr),Expr);
  if TPrimitiveExpr(Expr).Kind<>pekIdent then
    RaiseXExpectedButYFound(20180327152841,'method name',GetElementTypeName(Expr),Expr);
  ProcName:=TPrimitiveExpr(Expr).Value;
  IntfScope:=IntfType.CustomData as TPasClassScope;
  IntfProc:=nil;
  while IntfScope<>nil do
    begin
    Identifier:=IntfScope.FindLocalIdentifier(ProcName);
    while Identifier<>nil do
      begin
      if not (Identifier.Element is TPasProcedure) then
        RaiseXExpectedButYFound(20180327153110,'interface method',GetElementTypeName(Identifier.Element),Expr);
      IntfProc:=TPasProcedure(Identifier.Element);
      if IntfProc.ClassType=El.ProcClass then
        break;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    IntfScope:=IntfScope.AncestorScope;
    end;
  if IntfProc=nil then
    RaiseIdentifierNotFound(20180327153044,ProcName,Expr);
  CreateReference(IntfProc,Expr,rraRead);
  if IntfProc.ClassType<>El.ProcClass then
    RaiseXExpectedButYFound(20180323144107,GetElementTypeName(El.ProcClass),GetElementTypeName(IntfProc),El.InterfaceProc);
  // Note: do not create map here. CheckImplements in FinishPropertyOfClass must be called before.

  // El.ImplementationProc is resolved in FinishClassType
end;

procedure TPasResolver.FinishPropertyParamAccess(Params: TParamsExpr;
  Prop: TPasProperty);
var
  i: Integer;
  ParamAccess: TResolvedRefAccess;
begin
  for i:=0 to length(Params.Params)-1 do
    begin
    ParamAccess:=rraRead;
    if i<Prop.Args.Count then
      case TPasArgument(Prop.Args[i]).Access of
      argVar: ParamAccess:=rraVarParam;
      argOut: ParamAccess:=rraOutParam;
      end;
    AccessExpr(Params.Params[i],ParamAccess);
    end;
end;

procedure TPasResolver.FinishCallArgAccess(Expr: TPasExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
begin
  AccessExpr(Expr,Access);
  Flags:=[rcSetReferenceFlags];
  if Access<>rraRead then
    Include(Flags,rcNoImplicitProc);
  ComputeElement(Expr,ResolvedEl,Flags);
end;

procedure TPasResolver.FinishInitialFinalization(El: TPasImplBlock);
begin
  if El=nil then ;
  CheckTopScope(ScopeClass_InitialFinalization);
  PopScope;
end;

procedure TPasResolver.EmitTypeHints(PosEl: TPasElement; aType: TPasType);
begin
  while aType<>nil do
    begin
    if EmitElementHints(PosEl,aType) then
      exit; // give only hints for the nearest
    if aType.InheritsFrom(TPasAliasType) then
      aType:=TPasAliasType(aType).DestType
    else if aType.ClassType=TPasPointerType then
      aType:=TPasPointerType(aType).DestType
    else if (aType.ClassType=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData<>nil) then
      aType:=TPasType((aType.CustomData as TResolvedReference).Declaration)
    else
      exit;
    end;
end;

function TPasResolver.EmitElementHints(PosEl, El: TPasElement): boolean;
begin
  if IsElementSkipped(El) then
    RaiseMsg(20170927160030,nNotYetImplemented,sNotYetImplemented,[GetObjName(El)],PosEl);
  if El.Hints=[] then exit(false);
  Result:=true;
  if hDeprecated in El.Hints then
    begin
    if El.HintMessage<>'' then
      LogMsg(20170422160807,mtWarning,nSymbolXIsDeprecatedY,sSymbolXIsDeprecatedY,
        [El.Name,El.HintMessage],PosEl)
    else
      LogMsg(20170419190434,mtWarning,nSymbolXIsDeprecated,sSymbolXIsDeprecated,
        [El.Name],PosEl);
    end;
  if hLibrary in El.Hints then
    LogMsg(20170419190426,mtWarning,nSymbolXBelongsToALibrary,sSymbolXBelongsToALibrary,
      [El.Name],PosEl);
  if hPlatform in El.Hints then
    LogMsg(20170419185916,mtWarning,nSymbolXIsNotPortable,sSymbolXIsNotPortable,
      [El.Name],PosEl);
  if hExperimental in El.Hints then
    LogMsg(20170419190111,mtWarning,nSymbolXIsExperimental,sSymbolXIsExperimental,
      [El.Name],PosEl);
  if hUnimplemented in El.Hints then
    LogMsg(20170419190317,mtWarning,nSymbolXIsNotImplemented,sSymbolXIsNotImplemented,
      [El.Name],PosEl);
end;

procedure TPasResolver.StoreScannerFlagsInProc(ProcScope: TPasProcedureScope);
var
  ModScope: TPasModuleScope;
begin
  ProcScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  if bsRangeChecks in ProcScope.BoolSwitches then
    begin
    ModScope:=RootElement.CustomData as TPasModuleScope;
    Include(ModScope.Flags,pmsfRangeErrorNeeded);
    end;
end;

procedure TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs(
  ImplProcScope: TPasProcedureScope);
var
  DeclProc, ImplProc: TPasProcedure;
  DeclArgs, ImplArgs: TFPList;
  i: Integer;
  DeclArg, ImplArg: TPasArgument;
  Identifier: TPasIdentifier;
begin
  ImplProc:=ImplProcScope.Element as TPasProcedure;
  ImplArgs:=ImplProc.ProcType.Args;
  DeclProc:=ImplProcScope.DeclarationProc;
  DeclArgs:=DeclProc.ProcType.Args;
  for i:=0 to DeclArgs.Count-1 do
    begin
    DeclArg:=TPasArgument(DeclArgs[i]);
    if i<ImplArgs.Count then
      begin
      ImplArg:=TPasArgument(ImplArgs[i]);
      Identifier:=ImplProcScope.FindLocalIdentifier(DeclArg.Name);
      //writeln('TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs i=',i,' replacing ',GetObjName(ImplArg),' with ',GetObjName(DeclArg));
      if Identifier.Element<>ImplArg then
        RaiseInternalError(20170203161659,GetObjName(DeclArg)+' '+GetObjName(ImplArg));
      Identifier.Element:=DeclArg;
      Identifier.Identifier:=DeclArg.Name;
      end
    else
      RaiseNotYetImplemented(20170203161826,ImplProc);
    end;
  if DeclProc is TPasFunction then
    begin
    // redirect implementation 'Result' to declaration FuncType.ResultEl
    Identifier:=ImplProcScope.FindLocalIdentifier(ResolverResultVar);
    if Identifier.Element is TPasResultElement then
      Identifier.Element:=TPasFunction(DeclProc).FuncType.ResultEl;
    end;
end;

function TPasResolver.CreateClassIntfMap(El: TPasClassType; Index: integer
  ): TPasClassIntfMap;
var
  IntfType: TPasClassType;
  Map: TPasClassIntfMap;
  ClassScope: TPasClassScope;
begin
  ClassScope:=El.CustomData as TPasClassScope;
  if ClassScope.Interfaces[Index]<>nil then
    RaiseInternalError(20180322141916,GetElementDbgPath(El)+' '+IntToStr(Index)+' '+GetObjName(TObject(ClassScope.Interfaces[Index])));
  IntfType:=TPasClassType(ResolveAliasType(TPasType(El.Interfaces[Index])));
  Map:=nil;
  while IntfType<>nil do
    begin
    if Map=nil then
      begin
      Map:=TPasClassIntfMap.Create;
      Map.Element:=El;
      Result:=Map;
      ClassScope.Interfaces[Index]:=Map;
      end
    else
      begin
      Map.AncestorMap:=TPasClassIntfMap.Create;
      Map:=Map.AncestorMap;
      Map.Element:=El;
      end;
    Map.Intf:=IntfType;
    Map.Procs:=TFPList.Create;
    Map.Procs.Count:=IntfType.Members.Count;
    IntfType:=GetPasClassAncestor(IntfType,true) as TPasClassType;
    end;
end;

procedure TPasResolver.CheckConditionExpr(El: TPasExpr;
  const ResolvedEl: TPasResolverResult);
begin
  if ResolvedEl.BaseType<>btBoolean then
    RaiseXExpectedButYFound(20170216152135,
      BaseTypeNames[btBoolean],BaseTypeNames[ResolvedEl.BaseType],El);
end;

procedure TPasResolver.CheckProcSignatureMatch(DeclProc,
  ImplProc: TPasProcedure; CheckNames: boolean);
var
  i: Integer;
  DeclArgs, ImplArgs: TFPList;
  DeclName, ImplName: String;
  ImplResult, DeclResult: TPasType;
begin
  if ImplProc.ClassType<>DeclProc.ClassType then
    RaiseXExpectedButYFound(20170216151729,DeclProc.TypeName,ImplProc.TypeName,ImplProc);
  if ImplProc.CallingConvention<>DeclProc.CallingConvention then
    RaiseMsg(20170216151731,nCallingConventionMismatch,sCallingConventionMismatch,[],ImplProc);
  if ImplProc is TPasFunction then
    begin
    // check result type
    ImplResult:=TPasFunction(ImplProc).FuncType.ResultEl.ResultType;
    DeclResult:=TPasFunction(DeclProc).FuncType.ResultEl.ResultType;

    if not CheckElTypeCompatibility(ImplResult,DeclResult,prraSimple) then
      RaiseIncompatibleType(20170216151734,nResultTypeMismatchExpectedButFound,
        [],DeclResult,ImplResult,ImplProc);
    end;

  if CheckNames then
    begin
    // check argument names
    DeclArgs:=DeclProc.ProcType.Args;
    ImplArgs:=ImplProc.ProcType.Args;
    for i:=0 to DeclArgs.Count-1 do
      begin
      DeclName:=TPasArgument(DeclArgs[i]).Name;
      ImplName:=TPasArgument(ImplArgs[i]).Name;
      if CompareText(DeclName,ImplName)<>0 then
        RaiseMsg(20170216151738,nFunctionHeaderMismatchForwardVarName,
          sFunctionHeaderMismatchForwardVarName,[DeclProc.Name,DeclName,ImplName],ImplProc);
      end;
    end;
end;

procedure TPasResolver.ResolveImplBlock(Block: TPasImplBlock);
var
  i: Integer;
begin
  if Block=nil then exit;
  for i:=0 to Block.Elements.Count-1 do
    ResolveImplElement(TPasImplElement(Block.Elements[i]));
end;

procedure TPasResolver.ResolveImplElement(El: TPasImplElement);
var
  C: TClass;
begin
  //writeln('TPasResolver.ResolveImplElement ',GetObjName(El));
  if El=nil then exit;
  C:=El.ClassType;
  if C=TPasImplBeginBlock then
    ResolveImplBlock(TPasImplBeginBlock(El))
  else if C=TPasImplAssign then
    ResolveImplAssign(TPasImplAssign(El))
  else if C=TPasImplSimple then
    ResolveImplSimple(TPasImplSimple(El))
  else if C=TPasImplBlock then
    ResolveImplBlock(TPasImplBlock(El))
  else if C=TPasImplRepeatUntil then
    begin
    ResolveImplBlock(TPasImplBlock(El));
    ResolveStatementConditionExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if C=TPasImplIfElse then
    begin
    ResolveStatementConditionExpr(TPasImplIfElse(El).ConditionExpr);
    ResolveImplElement(TPasImplIfElse(El).IfBranch);
    ResolveImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if C=TPasImplWhileDo then
    begin
    ResolveStatementConditionExpr(TPasImplWhileDo(El).ConditionExpr);
    ResolveImplElement(TPasImplWhileDo(El).Body);
    end
  else if C=TPasImplCaseOf then
    ResolveImplCaseOf(TPasImplCaseOf(El))
  else if C=TPasImplLabelMark then
    ResolveImplLabelMark(TPasImplLabelMark(El))
  else if C=TPasImplForLoop then
    ResolveImplForLoop(TPasImplForLoop(El))
  else if C=TPasImplTry then
    begin
    ResolveImplBlock(TPasImplTry(El));
    ResolveImplBlock(TPasImplTry(El).FinallyExcept);
    ResolveImplBlock(TPasImplTry(El).ElseBranch);
    end
  else if C=TPasImplExceptOn then
    // handled in FinishExceptOnStatement
  else if C=TPasImplRaise then
    ResolveImplRaise(TPasImplRaise(El))
  else if C=TPasImplCommand then
    begin
    if TPasImplCommand(El).Command<>'' then
      RaiseNotYetImplemented(20160922163442,El,'TPasResolver.ResolveImplElement');
    end
  else if C=TPasImplAsmStatement then
    ResolveImplAsm(TPasImplAsmStatement(El))
  else if C=TPasImplWithDo then
    ResolveImplWithDo(TPasImplWithDo(El))
  else
    RaiseNotYetImplemented(20160922163445,El,'TPasResolver.ResolveImplElement');
end;

procedure TPasResolver.ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
type
  TRangeItem = record
    RangeStart, RangeEnd: TMaxPrecInt;
    Expr: TPasExpr;
    aString: UnicodeString;
    // Note: for case-of-string:
    //  single values are stored in aString and RangeStart=1, RangeEnd=0
    //  ranges are stored as aString='', RangeStart, RangeEnd
  end;
  PRangeItem = ^TRangeItem;

  function CreateValues(const ResolvedEl: TPasResolverResult;
    var ValueSet: TResEvalSet): boolean;
  var
    CaseExprType: TPasType;
  begin
    Result:=false;
    if ResolvedEl.BaseType in btAllInteger then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskInt);
      Result:=true;
      end
    else if ResolvedEl.BaseType in btAllBooleans then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskBool);
      Result:=true;
      end
    else if ResolvedEl.BaseType in btAllChars then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskChar);
      Result:=true;
      end
    else if ResolvedEl.BaseType in btAllStrings then
      Result:=true
    else if ResolvedEl.BaseType=btContext then
      begin
      CaseExprType:=ResolvedEl.LoTypeEl;
      if CaseExprType.ClassType=TPasEnumType then
        begin
        ValueSet:=TResEvalSet.CreateEmpty(revskEnum,CaseExprType);
        Result:=true;
        end;
      end
    else if ResolvedEl.BaseType=btRange then
      begin
      if ResolvedEl.SubType in btAllInteger then
        begin
        ValueSet:=TResEvalSet.CreateEmpty(revskInt);
        Result:=true;
        end
      else if ResolvedEl.SubType in btAllBooleans then
        begin
        ValueSet:=TResEvalSet.CreateEmpty(revskBool);
        Result:=true;
        end
      else if ResolvedEl.SubType in btAllChars then
        begin
        ValueSet:=TResEvalSet.CreateEmpty(revskChar);
        Result:=true;
        end
      else if ResolvedEl.SubType=btContext then
        begin
        CaseExprType:=ResolvedEl.LoTypeEl;
        if CaseExprType.ClassType=TPasEnumType then
          begin
          ValueSet:=TResEvalSet.CreateEmpty(revskEnum,CaseExprType);
          Result:=true;
          end;
        end;
      end;
  end;

  function AddRangeItem(Values: TFPList; const RangeStart, RangeEnd: TMaxPrecInt;
    Expr: TPasExpr): PRangeItem;
  begin
    New(Result);
    Result^.RangeStart:=RangeStart;
    Result^.RangeEnd:=RangeEnd;
    Result^.Expr:=Expr;
    Values.Add(Result);
  end;

  function AddValue(Value: TResEvalValue; Values: TFPList; ValueSet: TResEvalSet;
    Expr: TPasExpr): boolean;

    function AddString(const s: UnicodeString): boolean;
    var
      Dupl: TPasExpr;
      i, o: Integer;
      Item: PRangeItem;
    begin
      if length(s)=1 then
        o:=ord(s[1])
      else
        o:=-1;
      for i:=0 to Values.Count-1 do
        begin
        Item:=PRangeItem(Values[i]);
        if (Item^.aString=s)
            or ((o>=Item^.RangeStart) and (o<=Item^.RangeEnd)) then
          begin
          Dupl:=PRangeItem(Values[i])^.Expr;
          RaiseMsg(20180424220139,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Dupl)],Expr);
          end;
        end;
      Item:=AddRangeItem(Values,1,0,Expr);
      Item^.aString:=s;
      Result:=true;
    end;

    function AddStringRange(CharStart, CharEnd: TMaxPrecInt): boolean;
    var
      i, o: Integer;
      s: UnicodeString;
      Item: PRangeItem;
      Dupl: TPasExpr;
    begin
      if CharEnd>$ffff then
        RaiseNotYetImplemented(20180501221359,Expr,Value.AsDebugString);
      for i:=0 to Values.Count-1 do
        begin
        Item:=PRangeItem(Values[i]);
        s:=Item^.aString;
        if length(s)=1 then
          o:=ord(s[1])
        else
          o:=-1;
        if ((o>=CharStart) and (o<=CharEnd))
            or ((Item^.RangeStart<=CharEnd) and (Item^.RangeEnd>=CharStart)) then
          begin
          Dupl:=PRangeItem(Values[i])^.Expr;
          RaiseMsg(20180501223914,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Dupl)],Expr);
          end;
        end;
      AddRangeItem(Values,CharStart,CharEnd,Expr);
      Result:=true;
    end;

  var
    RangeStart, RangeEnd: TMaxPrecInt;
    i: Integer;
    Item: PRangeItem;
  begin
    {$IFDEF VerbosePasResolver}
    //writeln('TPasResolver.ResolveImplCaseOf.AddValue Value={',Value.AsDebugString,'} Values.Count=',Values.Count);
    {$ENDIF}
    Result:=true;
    case Value.Kind of
    revkBool:
      begin
      RangeStart:=ord(TResEvalBool(Value).B);
      RangeEnd:=RangeStart;
      end;
    revkInt:
      begin
      RangeStart:=TResEvalInt(Value).Int;
      RangeEnd:=RangeStart;
      end;
    revkUInt:
      begin
      // Note: when FPC compares int64 with qword it converts the qword to an int64
      if TResEvalUInt(Value).UInt>HighIntAsUInt then
        ExprEvaluator.EmitRangeCheckConst(20180424212414,Value.AsString,
          '0',IntToStr(High(TMaxPrecInt)),Expr,mtError);
      RangeStart:=TResEvalUInt(Value).UInt;
      RangeEnd:=RangeStart;
      end;
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      if ValueSet=nil then
        exit(AddString(ExprEvaluator.GetUnicodeStr(TResEvalString(Value).S,Expr)))
      else
        begin
        if length(TResEvalString(Value).S)<>1 then
          exit(false);
        RangeStart:=ord(TResEvalString(Value).S[1]);
        RangeEnd:=RangeStart;
        end;
    {$endif}
    revkUnicodeString:
      if ValueSet=nil then
        exit(AddString(TResEvalUTF16(Value).S))
      else
        begin
        if length(TResEvalUTF16(Value).S)<>1 then
          exit(false);
        RangeStart:=ord(TResEvalUTF16(Value).S[1]);
        RangeEnd:=RangeStart;
        end;
    revkEnum:
      begin
      RangeStart:=TResEvalEnum(Value).Index;
      RangeEnd:=RangeStart;
      end;
    revkRangeInt:
      if ValueSet=nil then
        exit(AddStringRange(TResEvalRangeInt(Value).RangeStart,TResEvalRangeInt(Value).RangeEnd))
      else
        begin
        RangeStart:=TResEvalRangeInt(Value).RangeStart;
        RangeEnd:=TResEvalRangeInt(Value).RangeEnd;
        end;
    revkRangeUInt:
      begin
      // Note: when FPC compares int64 with qword it converts the qword to an int64
      if TResEvalRangeUInt(Value).RangeEnd>HighIntAsUInt then
        ExprEvaluator.EmitRangeCheckConst(20180424212648,Value.AsString,
          '0',IntToStr(High(TMaxPrecInt)),Expr,mtError);
      RangeStart:=TResEvalRangeUInt(Value).RangeStart;
      RangeEnd:=TResEvalRangeUInt(Value).RangeEnd;
      end;
    else
      Result:=false;
    end;

    if ValueSet=nil then
      RaiseNotYetImplemented(20180424215728,Expr,Value.AsDebugString);
    i:=ValueSet.Intersects(RangeStart,RangeEnd);
    if i<0 then
      begin
      ValueSet.Add(RangeStart,RangeEnd);
      AddRangeItem(Values,RangeStart,RangeEnd,Expr);
      exit(true);
      end;
    // duplicate value -> show where
    for i:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[i]);
      if (Item^.RangeStart>RangeEnd) or (Item^.RangeEnd<RangeStart) then continue;
      RaiseMsg(20180424214305,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
        [Value.AsString,GetElementSourcePosStr(Item^.Expr)],Expr);
      end;
    Result:=false;
  end;

var
  i, j: Integer;
  El: TPasElement;
  Stat: TPasImplCaseStatement;
  CaseExprResolved, OfExprResolved: TPasResolverResult;
  OfExpr: TPasExpr;
  ok: Boolean;
  Values: TFPList; // list of PRangeItem
  ValueSet: TResEvalSet;
  Value: TResEvalValue;
  Item: PRangeItem;
begin
  ResolveExpr(CaseOf.CaseExpr,rraRead);
  ComputeElement(CaseOf.CaseExpr,CaseExprResolved,[rcSetReferenceFlags]);
  ok:=false;
  Values:=TFPList.Create;
  ValueSet:=nil;
  Value:=nil;
  try
    if (rrfReadable in CaseExprResolved.Flags) then
      ok:=CreateValues(CaseExprResolved,ValueSet);
    if not ok then
      RaiseXExpectedButYFound(20170216151952,'ordinal expression',
                 GetTypeDescription(CaseExprResolved.LoTypeEl),CaseOf.CaseExpr);

    for i:=0 to CaseOf.Elements.Count-1 do
      begin
      El:=TPasElement(CaseOf.Elements[i]);
      if El.ClassType=TPasImplCaseStatement then
        begin
        Stat:=TPasImplCaseStatement(El);
        for j:=0 to Stat.Expressions.Count-1 do
          begin
          //writeln('TPasResolver.ResolveImplCaseOf Stat.Expr[',j,']=',GetObjName(El));
          OfExpr:=TPasExpr(Stat.Expressions[j]);
          ResolveExpr(OfExpr,rraRead);
          ComputeElement(OfExpr,OfExprResolved,[rcConstant,rcSetReferenceFlags]);
          if OfExprResolved.BaseType=btRange then
            ConvertRangeToElement(OfExprResolved);
          CheckEqualResCompatibility(CaseExprResolved,OfExprResolved,OfExpr,true);

          Value:=Eval(OfExpr,[refConstExt]);
          if Value<>nil then
            begin
            if Value.Kind=revkExternal then
              begin
              // external const
              end
            else if not AddValue(Value,Values,ValueSet,OfExpr) then
              RaiseIncompatibleTypeRes(20180424210815,nIncompatibleTypesGotExpected,
                [],OfExprResolved,CaseExprResolved,OfExpr);
            ReleaseEvalValue(Value);
            end
          else
            RaiseMsg(20180518102047,nConstantExpressionExpected,sConstantExpressionExpected,[],OfExpr);
          end;
        ResolveImplElement(Stat.Body);
        end
      else if El.ClassType=TPasImplCaseElse then
        ResolveImplBlock(TPasImplCaseElse(El))
      else
        RaiseNotYetImplemented(20160922163448,El);
      end;
    // Note: CaseOf.ElseBranch was already resolved via Elements
  finally
    ReleaseEvalValue(Value);
    ValueSet.Free;
    for i:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[i]);
      Dispose(Item);
      end;
    Values.Free;
  end;
end;

procedure TPasResolver.ResolveImplLabelMark(Mark: TPasImplLabelMark);
begin
  RaiseNotYetImplemented(20161014141636,Mark);
end;

procedure TPasResolver.ResolveImplForLoop(Loop: TPasImplForLoop);
var
  VarResolved, StartResolved, EndResolved,
    OrigStartResolved: TPasResolverResult;
  EnumeratorFound, HasInValues: Boolean;
  InRange, VarRange: TResEvalValue;
  InRangeInt, VarRangeInt: TResEvalRangeInt;
  bt: TResolverBaseType;
  TypeEl: TPasType;
  C: TClass;
begin
  CreateScope(Loop,TPasForLoopScope);

  // loop var
  ResolveExpr(Loop.VariableName,rraReadAndAssign);
  ComputeElement(Loop.VariableName,VarResolved,[rcNoImplicitProc,rcSetReferenceFlags]);
  if not ResolvedElCanBeVarParam(VarResolved,Loop.VariableName) then
    RaiseVarExpected(20170216151955,Loop.VariableName,VarResolved.IdentEl);

  // resolve start expression
  ResolveExpr(Loop.StartExpr,rraRead);
  ComputeElement(Loop.StartExpr,StartResolved,[rcSetReferenceFlags]);

  case Loop.LoopType of
  ltNormal,ltDown:
    begin
    // start value
    if CheckAssignResCompatibility(VarResolved,StartResolved,Loop.StartExpr,true)=cIncompatible then
      RaiseIncompatibleTypeRes(20170216151958,nIncompatibleTypesGotExpected,
        [],StartResolved,VarResolved,Loop.StartExpr);
    CheckAssignExprRange(VarResolved,Loop.StartExpr);

    // end value
    ResolveExpr(Loop.EndExpr,rraRead);
    ComputeElement(Loop.EndExpr,EndResolved,[rcSetReferenceFlags]);
    if CheckAssignResCompatibility(VarResolved,EndResolved,Loop.EndExpr,false)=cIncompatible then
      RaiseIncompatibleTypeRes(20170216152001,nIncompatibleTypesGotExpected,
        [],EndResolved,VarResolved,Loop.EndExpr);
    CheckAssignExprRange(VarResolved,Loop.EndExpr);
    end;
  ltIn:
    begin
    // check range
    EnumeratorFound:=CheckForIn(Loop,VarResolved,StartResolved);
    if (not EnumeratorFound) and (StartResolved.BaseType=btContext) then
      begin
      TypeEl:=StartResolved.LoTypeEl;
      C:=TypeEl.ClassType;
      if C=TPasClassType then
        EnumeratorFound:=CheckForInClass(Loop,VarResolved,StartResolved);
      end;

    if not EnumeratorFound then
      begin
      VarRange:=nil;
      InRange:=nil;
      try
        OrigStartResolved:=StartResolved;
        if StartResolved.IdentEl is TPasType then
          begin
          // e.g. for e in TEnum do
          TypeEl:=StartResolved.LoTypeEl;
          if TypeEl is TPasArrayType then
            begin
            if length(TPasArrayType(TypeEl).Ranges)=1 then
              InRange:=Eval(TPasArrayType(TypeEl).Ranges[0],[refConst]);
            end;
          if InRange=nil then
            InRange:=EvalTypeRange(TypeEl,[]);
          {$IFDEF VerbosePasResolver}
          {AllowWriteln}
          if InRange<>nil then
            writeln('TPasResolver.ResolveImplForLoop in type: InRange=',InRange.AsDebugString)
          else
            writeln('TPasResolver.ResolveImplForLoop in type: InRange=nil');
          {AllowWriteln-}
          {$ENDIF}
          end
        else if rrfReadable in StartResolved.Flags then
          begin
          // value  (variable or expression)
          bt:=StartResolved.BaseType;
          if bt in [btSet,btArrayOrSet] then
            begin
            if (StartResolved.IdentEl=nil) and (StartResolved.ExprEl<>nil) then
              InRange:=Eval(StartResolved.ExprEl,[]);
            if InRange=nil then
              InRange:=EvalTypeRange(StartResolved.LoTypeEl,[]);
            end
          else if bt=btContext then
            begin
            TypeEl:=StartResolved.LoTypeEl;
            C:=TypeEl.ClassType;
            if C=TPasArrayType then
              begin
              ComputeElement(TPasArrayType(TypeEl).ElType,StartResolved,[rcType]);
              StartResolved.Flags:=OrigStartResolved.Flags*[rrfReadable,rrfWritable];
              if CheckAssignResCompatibility(VarResolved,StartResolved,Loop.StartExpr,true)=cIncompatible then
                RaiseIncompatibleTypeRes(20171112210138,nIncompatibleTypesGotExpected,
                  [],StartResolved,VarResolved,Loop.StartExpr);
              EnumeratorFound:=true;
              end;
            end
          else
            begin
            bt:=GetActualBaseType(bt);
            case bt of
            {$ifdef FPC_HAS_CPSTRING}
            btAnsiString:
              InRange:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff);
            {$endif}
            btUnicodeString:
              InRange:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff);
            end;
            end;
          end;
        if (not EnumeratorFound) and (InRange<>nil) then
          begin
          // for v in <constant> do
          // -> check if same type
          VarRange:=EvalTypeRange(VarResolved.LoTypeEl,[]);
          if VarRange=nil then
            RaiseXExpectedButYFound(20171109191528,'range',
                         GetResolverResultDescription(VarResolved),Loop.VariableName);
          //writeln('TPasResolver.ResolveImplForLoop ForIn VarRange=',VarRange.AsDebugString);
          //writeln('TPasResolver.ResolveImplForLoop ForIn InRange=',InRange.AsDebugString,' ElType=',GetResolverResultDbg(StartResolved));
          case InRange.Kind of
          revkRangeInt,revkSetOfInt:
            begin
            InRangeInt:=TResEvalRangeInt(InRange);
            case VarRange.Kind of
            revkRangeInt:
              begin
              VarRangeInt:=TResEvalRangeInt(VarRange);
              HasInValues:=(InRange.Kind<>revkSetOfInt) or (length(TResEvalSet(InRange).Ranges)>0);
              case InRangeInt.ElKind of
                revskEnum:
                  if (VarRangeInt.ElKind<>revskEnum)
                      or not IsSameType(InRangeInt.ElType,VarRangeInt.ElType,prraAlias) then
                    RaiseXExpectedButYFound(20171109200752,GetTypeDescription(InRangeInt.ElType),
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskInt:
                  if VarRangeInt.ElKind<>revskInt then
                    RaiseXExpectedButYFound(20171109200752,'integer',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskChar:
                  if VarRangeInt.ElKind<>revskChar then
                    RaiseXExpectedButYFound(20171109200753,'char',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskBool:
                  if VarRangeInt.ElKind<>revskBool then
                    RaiseXExpectedButYFound(20171109200754,'boolean',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
              else
                if HasInValues then
                  RaiseNotYetImplemented(20171109200954,Loop.StartExpr);
              end;
              if HasInValues then
                begin
                if (VarRangeInt.RangeStart>InRangeInt.RangeStart) then
                  begin
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRangeInt.AsDebugString,' ',InRangeInt.AsDebugString);
                  {$ENDIF}
                  fExprEvaluator.EmitRangeCheckConst(20171109201428,
                    InRangeInt.ElementAsString(InRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeEnd),Loop.VariableName,mtError);
                  end;
                if (VarRangeInt.RangeEnd<InRangeInt.RangeEnd) then
                  begin
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRangeInt.AsDebugString,' ',InRangeInt.AsDebugString);
                  {$ENDIF}
                  fExprEvaluator.EmitRangeCheckConst(20171109201429,
                    InRangeInt.ElementAsString(InRangeInt.RangeEnd),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeEnd),Loop.VariableName,mtError);
                  end;
                end;
              EnumeratorFound:=true;
              end;
            else
              {$IFDEF VerbosePasResolver}
              writeln('TPasResolver.ResolveImplForLoop ForIn VarRange=',VarRange.AsDebugString);
              {$ENDIF}
            end;
            end;
          else
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.ResolveImplForLoop ForIn InRange=',InRange.AsDebugString);
            {$ENDIF}
          end;
          end;
        if not EnumeratorFound then
          begin
          {$IFDEF VerbosePasResolver}
          {AllowWriteln}
          writeln('TPasResolver.ResolveImplForLoop StartResolved=',GetResolverResultDbg(StartResolved));
          if VarRange<>nil then
            writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRange.AsDebugString);
          {AllowWriteln-}
          {$ENDIF}
          RaiseMsg(20171108223818,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
            [GetBaseDescription(OrigStartResolved)],Loop.StartExpr);
          end;
      finally
        ReleaseEvalValue(VarRange);
        ReleaseEvalValue(InRange);
      end;
      end;

    end;
  else
    RaiseNotYetImplemented(20171108221334,Loop);
  end;
  ResolveImplElement(Loop.Body);
end;

procedure TPasResolver.ResolveImplWithDo(El: TPasImplWithDo);
var
  i, OldScopeCount: Integer;
  Expr, ErrorEl: TPasExpr;
  ExprResolved: TPasResolverResult;
  TypeEl: TPasType;
  WithScope: TPasWithScope;
  WithExprScope: TPasWithExprScope;
  ExprScope: TPasScope;
  OnlyTypeMembers, IsClassOf: Boolean;
  ClassEl: TPasClassType;
begin
  OldScopeCount:=ScopeCount;
  WithScope:=TPasWithScope(CreateScope(El,TPasWithScope));
  PushScope(WithScope);
  for i:=0 to El.Expressions.Count-1 do
    begin
    Expr:=TPasExpr(El.Expressions[i]);
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,ExprResolved,[rcSetReferenceFlags]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveImplWithDo ExprResolved=',GetResolverResultDbg(ExprResolved));
    {$ENDIF}
    ErrorEl:=Expr;
    TypeEl:=ExprResolved.LoTypeEl;
    // ToDo: use last element in Expr for error position
    if TypeEl=nil then
      RaiseMsg(20170216152004,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [BaseTypeNames[ExprResolved.BaseType]],ErrorEl);

    OnlyTypeMembers:=false;
    IsClassOf:=false;
    if TypeEl.ClassType=TPasRecordType then
      begin
      ExprScope:=NoNil(TPasRecordType(TypeEl).CustomData) as TPasRecordScope;
      if ExprResolved.IdentEl is TPasType then
        // e.g. with TPoint do PointInCircle
        OnlyTypeMembers:=true;
      end
    else if TypeEl.ClassType=TPasClassType then
      begin
      ExprScope:=NoNil(TPasClassType(TypeEl).CustomData) as TPasClassScope;
      if ExprResolved.IdentEl is TPasType then
        // e.g. with TFPMemoryImage do FindHandlerFromExtension()
        OnlyTypeMembers:=true;
      end
    else if TypeEl.ClassType=TPasClassOfType then
      begin
      // e.g. with ImageClass do FindHandlerFromExtension()
      ClassEl:=ResolveAliasType(TPasClassOfType(TypeEl).DestType) as TPasClassType;
      ExprScope:=ClassEl.CustomData as TPasClassScope;
      OnlyTypeMembers:=true;
      IsClassOf:=true;
      end
    else
      RaiseMsg(20170216152007,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [GetElementTypeName(TypeEl)],ErrorEl);
    WithExprScope:=ScopeClass_WithExpr.Create;
    WithExprScope.WithScope:=WithScope;
    WithExprScope.Index:=i;
    WithExprScope.Expr:=Expr;
    WithExprScope.Scope:=ExprScope;
    if not (ExprResolved.IdentEl is TPasType) then
      Include(WithExprScope.Flags,wesfNeedTmpVar);
    if OnlyTypeMembers then
      Include(WithExprScope.Flags,wesfOnlyTypeMembers);
    if IsClassOf then
      Include(WithExprScope.Flags,wesfIsClassOf);
    if (not (rrfWritable in ExprResolved.Flags))
        and (ExprResolved.BaseType=btContext)
        and (ExprResolved.LoTypeEl.ClassType=TPasRecordType) then
      Include(WithExprScope.Flags,wesfConstParent);
    WithScope.ExpressionScopes.Add(WithExprScope);
    PushScope(WithExprScope);
    end;
  ResolveImplElement(El.Body);
  CheckTopScope(ScopeClass_WithExpr);
  if TopScope<>WithScope.ExpressionScopes[WithScope.ExpressionScopes.Count-1] then
    RaiseInternalError(20160923102846);
  while ScopeCount>OldScopeCount do
    PopScope;
end;

procedure TPasResolver.ResolveImplAsm(El: TPasImplAsmStatement);
begin
  if El=nil then ;
end;

procedure TPasResolver.ResolveImplAssign(El: TPasImplAssign);
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  Access: TResolvedRefAccess;
  Value: TResEvalValue;
begin
  if El.Kind=akDefault then
    Access:=rraAssign
  else
    Access:=rraReadAndAssign;
  ResolveExpr(El.left,Access);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Kind=',El.Kind,' left=',GetObjName(El.left),' right=',GetObjName(el.right));
  {$ENDIF}
  // check LHS can be assigned
  ComputeElement(El.left,LeftResolved,[rcNoImplicitProc,rcSetReferenceFlags]);
  CheckCanBeLHS(LeftResolved,true,El.left);

  // compute RHS
  ResolveExpr(El.right,rraRead);
  Flags:=[rcSetReferenceFlags];
  if IsProcedureType(LeftResolved,true) then
    begin
    if (msDelphi in CurrentParser.CurrentModeswitches) then
      Include(Flags,rcNoImplicitProc) // a proc type can use param less procs
    else
      Include(Flags,rcNoImplicitProcType); // a proc type can use a param less proc type
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Left=',GetResolverResultDbg(LeftResolved),' Flags=',dbgs(Flags));
  {$ENDIF}
  ComputeElement(El.right,RightResolved,Flags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Right=',GetResolverResultDbg(RightResolved));
  {$ENDIF}

  case El.Kind of
  akDefault:
    begin
    CheckAssignResCompatibility(LeftResolved,RightResolved,El.right,true);
    CheckAssignExprRange(LeftResolved,El.right);
    if (LeftResolved.BaseType=btContext) and (LeftResolved.LoTypeEl.ClassType=TPasArrayType) then
      MarkArrayExprRecursive(El.right,TPasArrayType(LeftResolved.LoTypeEl));
    end;
  akAdd, akMinus,akMul,akDivision:
    begin
    if (LeftResolved.BaseType in btAllInteger) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllInteger) then
        RaiseMsg(20170216152009,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (LeftResolved.BaseType in btAllStrings) and (El.Kind=akAdd) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllStringAndChars) then
        RaiseMsg(20170216152012,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (LeftResolved.BaseType in btAllFloats)
        and (El.Kind in [akAdd,akMinus,akMul,akDivision]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
        RaiseMsg(20170216152107,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (LeftResolved.BaseType=btSet) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
        RaiseMsg(20170216152110,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RightResolved.BaseType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      if (LeftResolved.SubType=RightResolved.SubType)
          or ((LeftResolved.SubType in btAllInteger) and (RightResolved.SubType in btAllInteger))
          or ((LeftResolved.SubType in btAllBooleans) and (RightResolved.SubType in btAllBooleans))
      then
      else
        RaiseMsg(20170216152117,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[RightResolved.SubType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      end
    else if LeftResolved.BaseType=btContext then
      begin
      if (LeftResolved.LoTypeEl.ClassType=TPasArrayType) and (El.Kind=akAdd)
          and (rrfReadable in RightResolved.Flags)
          and IsDynArray(LeftResolved.LoTypeEl) then
        begin
        // DynArr+=...
        CheckAssignCompatibilityArrayType(LeftResolved,RightResolved,El,true);
        exit;
        end
      else
        RaiseIncompatibleTypeRes(20180615235749,nOperatorIsNotOverloadedAOpB,[AssignKindNames[El.Kind]],LeftResolved,RightResolved,El);
      end
    else
      RaiseIncompatibleTypeRes(20180208115707,nOperatorIsNotOverloadedAOpB,[AssignKindNames[El.Kind]],LeftResolved,RightResolved,El);
    // store const expression result
    Value:=Eval(El.right,[]);
    ReleaseEvalValue(Value);
    end;
  else
    RaiseNotYetImplemented(20160927143649,El,'AssignKind '+AssignKindNames[El.Kind]);
  end;
end;

procedure TPasResolver.ResolveImplSimple(El: TPasImplSimple);
var
  ExprResolved: TPasResolverResult;
  Expr: TPasExpr;
begin
  Expr:=El.expr;
  ResolveExpr(Expr,rraRead);
  ComputeElement(Expr,ExprResolved,[rcSetReferenceFlags]);
  if (rrfCanBeStatement in ExprResolved.Flags) then
    exit;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplSimple El=',GetObjName(El),' El.Expr=',GetObjName(El.Expr),' ExprResolved=',GetResolverResultDbg(ExprResolved));
  {$ENDIF}
  RaiseMsg(20170216152127,nIllegalExpression,sIllegalExpression,[],El);
end;

procedure TPasResolver.ResolveImplRaise(El: TPasImplRaise);
var
  ResolvedEl: TPasResolverResult;
begin
  if El.ExceptObject<>nil then
    begin
    ResolveExpr(El.ExceptObject,rraRead);
    ComputeElement(El.ExceptObject,ResolvedEl,[rcSetReferenceFlags]);
    CheckIsClass(El.ExceptObject,ResolvedEl);
    if ResolvedEl.IdentEl<>nil then
      begin
      if (ResolvedEl.IdentEl is TPasVariable)
          or (ResolvedEl.IdentEl is TPasArgument)
          or (ResolvedEl.IdentEl is TPasResultElement) then
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveImplRaise ',GetResolverResultDbg(ResolvedEl));
        {$ENDIF}
        RaiseXExpectedButYFound(20170216152133,
                 'variable',GetElementTypeName(ResolvedEl.IdentEl),El.ExceptObject);
        end;
      end
    else if ResolvedEl.ExprEl<>nil then
    else
      RaiseXExpectedButYFound(201702303145230,
             'variable',GetResolverResultDbg(ResolvedEl),El.ExceptObject);
    if not (rrfReadable in ResolvedEl.Flags) then
      RaiseMsg(20170303145037,nNotReadable,sNotReadable,[],El.ExceptObject);
    end;
  if El.ExceptAddr<>nil then
    ResolveExpr(El.ExceptAddr,rraRead);
end;

procedure TPasResolver.ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess);
var
  Primitive: TPrimitiveExpr;
  ElClass: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveExpr ',GetObjName(El),' ',Access);
  {$ENDIF}
  if El=nil then
    RaiseNotYetImplemented(20160922163453,El);
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    Primitive:=TPrimitiveExpr(El);
    case Primitive.Kind of
    pekIdent: ResolveNameExpr(El,Primitive.Value,Access);
    pekNumber: ;
    pekString: ;
    pekNil,pekBoolConst: ;
    else
      RaiseNotYetImplemented(20160922163451,El);
    end;
    end
  else if ElClass=TUnaryExpr then
    ResolveExpr(TUnaryExpr(El).Operand,Access)
  else if ElClass=TBinaryExpr then
    ResolveBinaryExpr(TBinaryExpr(El),Access)
  else if ElClass=TParamsExpr then
    ResolveParamsExpr(TParamsExpr(El),Access)
  else if ElClass=TBoolConstExpr then
  else if ElClass=TNilExpr then
  else if ElClass=TSelfExpr then
    ResolveNameExpr(El,'Self',Access)
  else if ElClass=TInheritedExpr then
    ResolveInherited(TInheritedExpr(El),Access)
  else if ElClass=TArrayValues then
    begin
    if Access<>rraRead then
      RaiseMsg(20170303205743,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El);
    ResolveArrayValues(TArrayValues(El));
    end
  else if ElClass=TRecordValues then
    begin
    if Access<>rraRead then
      RaiseMsg(20180429103024,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El);
    ResolveRecordValues(TRecordValues(El));
    end
  else
    RaiseNotYetImplemented(20170222184329,El);

  if El.format1<>nil then
    ResolveExpr(El.format1,rraRead);
  if El.format2<>nil then
    ResolveExpr(El.format2,rraRead);
end;

procedure TPasResolver.ResolveStatementConditionExpr(El: TPasExpr);
var
  ResolvedCond: TPasResolverResult;
begin
  ResolveExpr(El,rraRead);
  ComputeElement(El,ResolvedCond,[rcSetReferenceFlags]);
  CheckConditionExpr(El,ResolvedCond);
end;

procedure TPasResolver.ResolveNameExpr(El: TPasExpr; const aName: string;
  Access: TResolvedRefAccess);
var
  FindData: TPRFindData;
  DeclEl: TPasElement;
  Proc, ImplProc: TPasProcedure;
  Ref: TResolvedReference;
  BuiltInProc: TResElDataBuiltInProc;
  p: SizeInt;
  DottedName: String;
  Bin: TBinaryExpr;
  ProcScope: TPasProcedureScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveNameExpr El=',GetObjName(El),' Name="',aName,'" ',Access);
  {$ENDIF}
  DeclEl:=FindElementWithoutParams(aName,FindData,El,false);
  if DeclEl.ClassType=TPasUsesUnit then
    begin
    // the first name of a unit matches -> find unit with longest match
    FindLongestUnitName(DeclEl,El);
    FindData.Found:=DeclEl;
    end;

  Ref:=CreateReference(DeclEl,El,Access,@FindData);
  CheckFoundElement(FindData,Ref);

  if DeclEl is TPasProcedure then
    begin
    // identifier is a proc and args brackets are missing
    if El.Parent.ClassType=TPasProperty then
      // a property accessor does not need args -> ok
      // Note: the detailed tests are in FinishPropertyOfClass
    else
      begin
      // examples: funca or @proca or a.funca or @a.funca ...
      Proc:=TPasProcedure(DeclEl);
      if (Access=rraAssign) and (Proc is TPasFunction)
          and (El.ClassType=TPrimitiveExpr)
          and (El.Parent.ClassType=TPasImplAssign)
          and (TPasImplAssign(El.Parent).left=El) then
        begin
        // e.g. funcname:=
        ProcScope:=Proc.CustomData as TPasProcedureScope;
        ImplProc:=ProcScope.ImplProc;
        if ImplProc=nil then
          ImplProc:=Proc;
        if El.HasParent(ImplProc) then
          begin
          // "FuncA:=" within FuncA  -> redirect to ResultEl
          Ref.Declaration:=(Proc as TPasFunction).FuncType.ResultEl;
          exit;
          end;
        end;
      if ProcNeedsParams(Proc.ProcType) and not ExprIsAddrTarget(El) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveNameExpr ',GetObjName(El));
        {$ENDIF}
        RaiseMsg(20170216152138,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],El);
        end;
      end;
    end
  else if DeclEl.ClassType=TPasUnresolvedSymbolRef then
    begin
    if DeclEl.CustomData is TResElDataBuiltInProc then
      begin
      BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
      BuiltInProc.GetCallCompatibility(BuiltInProc,El,true);
      end;
    end
  else if (DeclEl.ClassType=TPasUsesUnit) or (DeclEl is TPasModule) then
    begin
    // unit reference
    // dotted unit names needs a ref for each expression identifier
    // Note: El is the first TPrimitiveExpr of the dotted unit name reference
    DottedName:=DeclEl.Name;
    repeat
      p:=Pos('.',DottedName);
      if p<1 then break;
      Delete(DottedName,1,p);
      El:=GetNextDottedExpr(El);
      if El=nil then
        RaiseInternalError(20170503002012);
      CreateReference(DeclEl,El,Access);
      if (El.Parent is TBinaryExpr) and (TBinaryExpr(El.Parent).right=El) then
        begin
        Bin:=TBinaryExpr(El.Parent);
        while Bin.OpCode=eopSubIdent do
          begin
          CreateReference(DeclEl,Bin,Access);
          if not (Bin.Parent is TBinaryExpr) then break;
          if (TBinaryExpr(Bin.Parent).right<>Bin) then break;
          Bin:=TBinaryExpr(Bin.Parent);
          end;
        end;
    until false;
    end;
end;

procedure TPasResolver.ResolveInherited(El: TInheritedExpr;
  Access: TResolvedRefAccess);
var
  ProcScope, DeclProcScope, SelfScope: TPasProcedureScope;
  AncestorScope, ClassScope: TPasClassScope;
  DeclProc, AncestorProc: TPasProcedure;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInherited El.Parent=',GetTreeDbg(El.Parent));
  {$ENDIF}
  if (El.Parent.ClassType=TBinaryExpr)
  and (TBinaryExpr(El.Parent).OpCode=eopNone) then
    begin
    // e.g. 'inherited Proc;'
    ResolveInheritedCall(TBinaryExpr(El.Parent),Access);
    exit;
    end;

  // 'inherited;' without expression
  ProcScope:=GetInheritedExprScope(El);
  SelfScope:=ProcScope.GetSelfScope;
  if SelfScope=nil then
    RaiseMsg(20170216152141,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);
  ClassScope:=SelfScope.ClassScope;

  AncestorScope:=ClassScope.AncestorScope;
  if AncestorScope=nil then
    begin
    // 'inherited;' without ancestor class is silently ignored
    exit;
    end;

  // search ancestor in element, i.e. 'inherited' expression
  DeclProc:=SelfScope.DeclarationProc;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  AncestorProc:=DeclProcScope.OverriddenProc;
  if AncestorProc=nil then
    begin
    // 'inherited;' without ancestor method is silently ignored
    exit;
    end;
  CreateReference(AncestorProc,El,Access);
  if AncestorProc.IsAbstract then
    RaiseMsg(20170216152144,nAbstractMethodsCannotBeCalledDirectly,
      sAbstractMethodsCannotBeCalledDirectly,[],El);
end;

procedure TPasResolver.ResolveInheritedCall(El: TBinaryExpr;
  Access: TResolvedRefAccess);
// El.OpCode=eopNone
// El.left is TInheritedExpr
// El.right is the identifier and parameters
var
  ProcScope, SelfScope: TPasProcedureScope;
  AncestorScope, ClassScope: TPasClassScope;
  AncestorClass: TPasClassType;
  InhScope: TPasDotClassScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInheritedCall El=',GetTreeDbg(El));
  {$ENDIF}

  ProcScope:=GetInheritedExprScope(El);
  SelfScope:=ProcScope.GetSelfScope;
  if SelfScope=nil then
    RaiseMsg(20170216152148,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);
  ClassScope:=SelfScope.ClassScope;

  AncestorScope:=ClassScope.AncestorScope;
  if AncestorScope=nil then
    RaiseMsg(20170216152151,nInheritedNeedsAncestor,sInheritedNeedsAncestor,[],El.left);

  // search call in ancestor
  AncestorClass:=TPasClassType(AncestorScope.Element);
  InhScope:=PushClassDotScope(AncestorClass);
  InhScope.InheritedExpr:=true;
  ResolveExpr(El.right,Access);
  PopScope;
end;

procedure TPasResolver.ResolveBinaryExpr(El: TBinaryExpr;
  Access: TResolvedRefAccess);
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.ResolveBinaryExpr left=',GetObjName(El.left),' right=',GetObjName(El.right),' opcode=',OpcodeStrings[El.OpCode]);
  {$ENDIF}
  ResolveExpr(El.left,rraRead);
  if El.right=nil then exit;
  case El.OpCode of
  eopNone:
    case El.Kind of
    pekRange:
      ResolveExpr(El.right,rraRead);
    else
      if El.left.ClassType=TInheritedExpr then
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveBinaryExpr El.Kind=',ExprKindNames[El.Kind],' El.Left=',GetObjName(El.left),' El.Right=',GetObjName(El.right),' parent=',GetObjName(El.Parent));
        {$ENDIF}
        RaiseNotYetImplemented(20160922163456,El);
        end;
    end;
  eopAdd,
  eopSubtract,
  eopMultiply,
  eopDivide,
  eopDiv,
  eopMod,
  eopPower,
  eopShr,
  eopShl,
  eopNot,
  eopAnd,
  eopOr,
  eopXor,
  eopEqual,
  eopNotEqual,
  eopLessThan,
  eopGreaterThan,
  eopLessthanEqual,
  eopGreaterThanEqual,
  eopIn,
  eopIs,
  eopAs,
  eopSymmetricaldifference:
    ResolveExpr(El.right,rraRead);
  eopSubIdent:
    ResolveSubIdent(El,Access);
  else
    RaiseNotYetImplemented(20160922163459,El,OpcodeStrings[El.OpCode]);
  end;
end;

procedure TPasResolver.ResolveSubIdent(El: TBinaryExpr;
  Access: TResolvedRefAccess);
var
  aModule: TPasModule;
  ClassEl: TPasClassType;
  ClassScope: TPasDotClassScope;
  LeftResolved: TPasResolverResult;
  Left: TPasExpr;
  RecordEl: TPasRecordType;
  RecordScope: TPasDotRecordScope;
  LTypeEl: TPasType;
begin
  if El.CustomData is TResolvedReference then
    exit; // for example, when a.b has a dotted unit name

  Left:=El.left;
  //writeln('TPasResolver.ResolveSubIdent Left=',GetObjName(Left));
  ComputeElement(Left,LeftResolved,[rcSetReferenceFlags]);

  if LeftResolved.BaseType=btModule then
    begin
    // e.g. unitname.identifier
    // => search in interface and if this is our module in the implementation
    aModule:=NoNil(LeftResolved.IdentEl) as TPasModule;
    PushModuleDotScope(aModule);
    ResolveExpr(El.right,Access);
    PopScope;
    exit;
    end
  else if LeftResolved.LoTypeEl=nil then
    begin
    // illegal qualifier, see below
    end
  else
    begin
    LTypeEl:=LeftResolved.LoTypeEl;
    if (LTypeEl.ClassType=TPasPointerType)
        and (msAutoDeref in GetElModeSwitches(El))
        and (rrfReadable in LeftResolved.Flags)
        then
      begin
      // a.b  ->  a^.b
      LTypeEl:=ResolveAliasType(TPasPointerType(LTypeEl).DestType);
      Include(LeftResolved.Flags,rrfWritable);
      end;

    if LTypeEl.ClassType=TPasClassType then
      begin
      ClassEl:=TPasClassType(LTypeEl);
      ClassScope:=PushClassDotScope(ClassEl);
      if LeftResolved.IdentEl is TPasType then
        // e.g. TFPMemoryImage.FindHandlerFromExtension()
        ClassScope.OnlyTypeMembers:=true
      else
        // e.g. Image.Width
        ClassScope.OnlyTypeMembers:=false;
      ResolveExpr(El.right,Access);
      PopScope;
      exit;
      end
    else if LTypeEl.ClassType=TPasClassOfType then
      begin
      // e.g. ImageClass.FindHandlerFromExtension()
      ClassEl:=ResolveAliasType(TPasClassOfType(LTypeEl).DestType) as TPasClassType;
      ClassScope:=PushClassDotScope(ClassEl);
      ClassScope.OnlyTypeMembers:=true;
      ClassScope.IsClassOf:=true;
      ResolveExpr(El.right,Access);
      PopScope;
      exit;
      end
    else if LTypeEl.ClassType=TPasRecordType then
      begin
      RecordEl:=TPasRecordType(LTypeEl);
      RecordScope:=PushRecordDotScope(RecordEl);
      RecordScope.ConstParent:=not (rrfWritable in LeftResolved.Flags);
      if LeftResolved.IdentEl is TPasType then
        // e.g. TPoint.PointInCircle
        RecordScope.OnlyTypeMembers:=true
      else
        begin
        // e.g. aPoint.X
        AccessExpr(El.left,Access);
        RecordScope.OnlyTypeMembers:=false;
        end;
      ResolveExpr(El.right,Access);
      PopScope;
      exit;
      end
    else if LTypeEl.ClassType=TPasEnumType then
      begin
      if LeftResolved.IdentEl is TPasType then
        begin
        // e.g. TShiftState.ssAlt
        PushEnumDotScope(TPasEnumType(LTypeEl));
        ResolveExpr(El.right,Access);
        PopScope;
        exit;
        end;
      end
    else
      RaiseMsg(20170216152541,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [GetElementTypeName(LeftResolved.LoTypeEl)],El);
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSubIdent left=',GetObjName(Left),' right=',GetObjName(El.right),' leftresolved=',GetResolverResultDbg(LeftResolved));
  {$ENDIF}
  RaiseMsg(20170216152157,nIllegalQualifierAfter,sIllegalQualifierAfter,
    ['.',GetResolverResultDescription(LeftResolved)],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
var
  i, ScopeDepth: Integer;
  ParamAccess: TResolvedRefAccess;
begin
  if (Params.Kind=pekSet) and not (Access in [rraRead,rraParamToUnknownProc]) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveParamsExpr SET literal Access=',Access);
    {$ENDIF}
    RaiseMsg(20170303211052,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
    end;

  // first resolve params
  ResetSubScopes(ScopeDepth);
  if Params.Kind in [pekFuncParams,pekArrayParams] then
    ParamAccess:=rraParamToUnknownProc
  else
    ParamAccess:=rraRead;
  for i:=0 to length(Params.Params)-1 do
    ResolveExpr(Params.Params[i],ParamAccess);
  RestoreSubScopes(ScopeDepth);

  // then resolve the call, typecast, array, set
  if (Params.Kind=pekFuncParams) then
    ResolveFuncParamsExpr(Params,Access)
  else if (Params.Kind=pekArrayParams) then
    ResolveArrayParamsExpr(Params,Access)
  else if (Params.Kind=pekSet) then
    ResolveSetParamsExpr(Params)
  else
    RaiseNotYetImplemented(20160922163501,Params);
end;

procedure TPasResolver.ResolveFuncParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);

  procedure FinishProcParams(ProcType: TPasProcedureType);
  var
    ParamAccess: TResolvedRefAccess;
    i: Integer;
  begin
    if not (Access in [rraRead,rraParamToUnknownProc]) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr.FinishProcParams Params=',GetObjName(Params),' Value=',GetObjName(Params.Value),' Access=',Access);
      {$ENDIF}
      RaiseMsg(20170306104440,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
      end;
    for i:=0 to length(Params.Params)-1 do
      begin
      ParamAccess:=rraRead;
      if i<ProcType.Args.Count then
        case TPasArgument(ProcType.Args[i]).Access of
        argVar: ParamAccess:=rraVarParam;
        argOut: ParamAccess:=rraOutParam;
        end;
      AccessExpr(Params.Params[i],ParamAccess);
      end;
    CheckCallProcCompatibility(ProcType,Params,false,true);
  end;

  procedure FinishUntypedParams(ParamAccess: TResolvedRefAccess);
  var
    i: Integer;
  begin
    if ParamAccess=rraParamToUnknownProc then exit;
    for i:=0 to length(Params.Params)-1 do
      FinishCallArgAccess(Params.Params[i],ParamAccess);
  end;

var
  i: Integer;
  ElName, Msg: String;
  FindCallData: TFindCallElData;
  Abort: boolean;
  El, FoundEl: TPasElement;
  Ref: TResolvedReference;
  FindData: TPRFindData;
  BuiltInProc: TResElDataBuiltInProc;
  SubParams: TParamsExpr;
  ResolvedEl: TPasResolverResult;
  Value: TPasExpr;
  TypeEl: TPasType;
  C: TClass;
begin
  Value:=Params.Value;
  if IsNameExpr(Value) then
    begin
    // e.g. Name() -> find compatible
    if Value.ClassType=TPrimitiveExpr then
      ElName:=TPrimitiveExpr(Value).Value
    else
      ElName:='Self';
    FindCallData:=Default(TFindCallElData);
    FindCallData.Params:=Params;
    Abort:=false;
    IterateElements(ElName,@OnFindCallElements,@FindCallData,Abort);
    if FindCallData.Found=nil then
      RaiseIdentifierNotFound(20170216152544,ElName,Value);
    if FindCallData.Distance=cIncompatible then
      begin
      // FoundEl one element, but it was incompatible => raise error
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr found one element, but it was incompatible => check again to raise error. Found=',GetObjName(FindCallData.Found));
      WriteScopes;
      {$ENDIF}
      if FindCallData.Found is TPasProcedure then
        CheckCallProcCompatibility(TPasProcedure(FindCallData.Found).ProcType,Params,true)
      else if FindCallData.Found is TPasProcedureType then
        CheckTypeCast(TPasProcedureType(FindCallData.Found),Params,true)
      else if FindCallData.Found.ClassType=TPasUnresolvedSymbolRef then
        begin
        if FindCallData.Found.CustomData is TResElDataBuiltInProc then
          begin
          BuiltInProc:=TResElDataBuiltInProc(FindCallData.Found.CustomData);
          BuiltInProc.GetCallCompatibility(BuiltInProc,Params,true);
          end
        else if FindCallData.Found.CustomData is TResElDataBaseType then
          CheckTypeCast(TPasUnresolvedSymbolRef(FindCallData.Found),Params,true)
        else
          RaiseNotYetImplemented(20161006132825,FindCallData.Found);
        end
      else if FindCallData.Found is TPasType then
        // Note: check TPasType after TPasUnresolvedSymbolRef
        CheckTypeCast(TPasType(FindCallData.Found),Params,true)
      else if FindCallData.Found is TPasVariable then
        begin
        TypeEl:=ResolveAliasType(TPasVariable(FindCallData.Found).VarType);
        if TypeEl is TPasProcedureType then
          CheckCallProcCompatibility(TPasProcedureType(TypeEl),Params,true)
        else
          RaiseMsg(20170405003522,nIllegalQualifierAfter,sIllegalQualifierAfter,['(',TypeEl.ElementTypeName],Params);
        end
      else if FindCallData.Found is TPasArgument then
        begin
        TypeEl:=ResolveAliasType(TPasArgument(FindCallData.Found).ArgType);
        if TypeEl is TPasProcedureType then
          CheckCallProcCompatibility(TPasProcedureType(TypeEl),Params,true)
        else
          RaiseMsg(20180228145412,nIllegalQualifierAfter,sIllegalQualifierAfter,['(',TypeEl.ElementTypeName],Params);
        end
      else
        RaiseNotYetImplemented(20161003134755,FindCallData.Found);
      // missing raise exception
      RaiseNotYetImplemented(20180621002400,Params,'missing exception, Found='+GetObjName(FindCallData.Found));
      end;
    if FindCallData.Count>1 then
      begin
      // multiple overloads fit => search again and list the candidates
      FindCallData:=Default(TFindCallElData);
      FindCallData.Params:=Params;
      FindCallData.List:=TFPList.Create;
      try
        IterateElements(ElName,@OnFindCallElements,@FindCallData,Abort);
        Msg:='';
        for i:=0 to FindCallData.List.Count-1 do
          begin
          El:=TPasElement(FindCallData.List[i]);
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ResolveFuncParamsExpr Overload Candidate: ',GetElementSourcePosStr(El),' ',GetTreeDbg(El));
          {$ENDIF}
          // emit a hint for each candidate
          if El is TPasProcedure then
            LogMsg(20170417180320,mtHint,nFoundCallCandidateX,sFoundCallCandidateX,
              [GetProcTypeDescription(TPasProcedure(El).ProcType,
                [prptdUseName,prptdAddPaths,prptdResolveSimpleAlias])],El);
          Msg:=Msg+', '+GetElementSourcePosStr(El);
          end;
        RaiseMsg(20170216152200,nCantDetermineWhichOverloadedFunctionToCall,
          sCantDetermineWhichOverloadedFunctionToCall+Msg,[ElName],Value);
      finally
        FindCallData.List.Free;
      end;
      end;

    // FoundEl compatible element -> create reference
    FoundEl:=FindCallData.Found;
    Ref:=CreateReference(FoundEl,Value,rraRead);
    if FindCallData.StartScope.ClassType=ScopeClass_WithExpr then
      Ref.WithExprScope:=TPasWithExprScope(FindCallData.StartScope);
    FindData:=Default(TPRFindData);
    FindData.ErrorPosEl:=Value;
    FindData.StartScope:=FindCallData.StartScope;
    FindData.ElScope:=FindCallData.ElScope;
    FindData.Found:=FoundEl;
    CheckFoundElement(FindData,Ref);

    // set param expression Access flags
    if FoundEl is TPasProcedure then
      // now it is known which overloaded proc to call
      FinishProcParams(TPasProcedure(FoundEl).ProcType)
    else if FoundEl is TPasType then
      begin
      TypeEl:=ResolveAliasType(TPasType(FoundEl));
      C:=TypeEl.ClassType;
      if (C=TPasClassType)
          or (C=TPasClassOfType)
          or (C=TPasRecordType)
          or (C=TPasEnumType)
          or (C=TPasSetType)
          or (C=TPasPointerType)
          or (C=TPasArrayType)
          or (C=TPasRangeType) then
        begin
        // type cast
        FinishUntypedParams(Access);
        end
      else if (C=TPasProcedureType)
          or (C=TPasFunctionType) then
        begin
        // type cast to proc type
        AccessExpr(Params.Params[0],Access);
        end
      else if C=TPasUnresolvedSymbolRef then
        begin
        if TypeEl.CustomData is TResElDataBuiltInProc then
          begin
          // call built-in proc
          BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);
          if Assigned(BuiltInProc.FinishParamsExpression) then
            BuiltInProc.FinishParamsExpression(BuiltInProc,Params)
          else
            FinishUntypedParams(rraRead);
          end
        else if TypeEl.CustomData is TResElDataBaseType then
          begin
          // type cast to base type
          FinishUntypedParams(Access);
          end
        else
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
          {$ENDIF}
          RaiseNotYetImplemented(20170325145720,Params);
          end;
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
        {$ENDIF}
        RaiseMsg(20170306121908,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['(',TypeEl.ElementTypeName],Params);
        end;
      end
    else
      begin
      // FoundEl is not a type, maybe a var
      ComputeElement(FoundEl,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
      TypeEl:=ResolvedEl.LoTypeEl;
      if TypeEl is TPasProcedureType then
        begin
        FinishProcParams(TPasProcedureType(TypeEl));
        exit;
        end;
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData),' Resolvedel=',GetResolverResultDbg(ResolvedEl));
      {$ENDIF}
      RaiseMsg(20170306104301,nIllegalQualifierAfter,sIllegalQualifierAfter,
        ['(',TypeEl.ElementTypeName],Params);
      end;
    end
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()() or Name[]()
      ResolveExpr(SubParams,rraRead);
      ComputeElement(SubParams,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
      if IsProcedureType(ResolvedEl,true) then
        begin
        CheckCallProcCompatibility(TPasProcedureType(ResolvedEl.LoTypeEl),Params,true);
        CreateReference(TPasProcedureType(ResolvedEl.LoTypeEl),Value,Access);
        exit;
        end
      end;
    RaiseMsg(20170216152202,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['(',SubParams.ElementTypeName],Params);
    end
  else
    RaiseNotYetImplemented(20161014085118,Params.Value);
end;

procedure TPasResolver.ResolveArrayParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;

  procedure ResolveValueName(Value: TPasElement; ArrayName: string);
  var
    FindData: TPRFindData;
    Ref: TResolvedReference;
    DeclEl: TPasElement;
    Proc, ImplProc: TPasProcedure;
    ProcScope: TPasProcedureScope;
  begin
    // e.g. Name[]
    DeclEl:=FindElementWithoutParams(ArrayName,FindData,Value,true);
    Ref:=CreateReference(DeclEl,Value,Access,@FindData);
    CheckFoundElement(FindData,Ref);
    if DeclEl is TPasProcedure then
      begin
      Proc:=TPasProcedure(DeclEl);
      if (Access=rraAssign) and (Proc is TPasFunction)
          and (Value.ClassType=TPrimitiveExpr)
          and (Params.Parent.ClassType=TPasImplAssign)
          and (TPasImplAssign(Params.Parent).left=Params) then
        begin
        // e.g. funcname[]:=
        ProcScope:=Proc.CustomData as TPasProcedureScope;
        ImplProc:=ProcScope.ImplProc;
        if ImplProc=nil then
          ImplProc:=Proc;
        if Params.HasParent(ImplProc) then
          begin
          // "FuncA[]:=" within FuncA -> redirect to ResultEl
          Ref.Declaration:=(Proc as TPasFunction).FuncType.ResultEl;
          end;
        end;
      end;
    ComputeElement(Value,ResolvedEl,[rcSetReferenceFlags]);
  end;

var
  Value: TPasExpr;
  SubParams: TParamsExpr;
begin
  Value:=Params.Value;
  if Value=nil then
    RaiseInternalError(20180423093120,GetObjName(Params));
  if (Value.ClassType=TPrimitiveExpr)
      and (TPrimitiveExpr(Value).Kind=pekIdent) then
    // e.g. Name[]
    ResolveValueName(Value,TPrimitiveExpr(Value).Value)
  else if (Value.ClassType=TSelfExpr) then
    // e.g. Self[]
    ResolveValueName(Value,'Self')
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()[] or Name[][]
      ResolveExpr(SubParams,rraRead);
      ComputeElement(SubParams,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
      if Value.CustomData=nil then
        CreateReference(ResolvedEl.LoTypeEl,Value,Access);
      end
    else
      RaiseNotYetImplemented(20161010194925,Value);
    end
  else if Value.InheritsFrom(TUnaryExpr) then
    begin
    ResolveExpr(TUnaryExpr(Value).Operand,Access);
    ComputeElement(Value,ResolvedEl,[rcSetReferenceFlags]);
    end
  else
    RaiseNotYetImplemented(20160927212610,Value);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveArrayParamsExpr Value=',GetObjName(Value),' ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  ResolveArrayParamsArgs(Params,ResolvedEl,Access);
end;

procedure TPasResolver.ResolveArrayParamsArgs(Params: TParamsExpr;
  const ResolvedValue: TPasResolverResult; Access: TResolvedRefAccess);

  function CheckStringOrPointerIndex(IsStringIndex: boolean): boolean;
  var
    ArgExp: TPasExpr;
    ResolvedArg: TPasResolverResult;
  begin
    if not IsStringIndex then
      begin
      // pointer
      if not (bsPointerMath in GetElBoolSwitches(Params)) then
        exit(false);
      end;
    Result:=true;
    if not (rrfReadable in ResolvedValue.Flags) then
      RaiseXExpectedButYFound(20170216152548,'index',GetElementTypeName(ResolvedValue.LoTypeEl),Params);
    // check single argument
    if length(Params.Params)<1 then
      RaiseMsg(20170216152204,nMissingParameterX,
        sMissingParameterX,[BoolToStr(IsStringIndex,'character index','index')],Params)
    else if length(Params.Params)>1 then
      RaiseMsg(20170216152551,nIllegalQualifier,sIllegalQualifier,[','],Params.Params[1]);
    // check argument is integer
    ArgExp:=Params.Params[0];
    ComputeElement(ArgExp,ResolvedArg,[rcSetReferenceFlags]);
    if not (ResolvedArg.BaseType in btAllInteger) then
      RaiseMsg(20170216152209,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [BaseTypeNames[ResolvedArg.BaseType],'integer'],ArgExp);
    if not (rrfReadable in ResolvedArg.Flags) then
      RaiseMsg(20170216152211,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        ['type','value'],ArgExp);
    AccessExpr(ArgExp,rraRead);
  end;

var
  PropEl: TPasProperty;
  ClassScope: TPasClassScope;
  i: Integer;
  TypeEl: TPasType;
begin
  if ResolvedValue.BaseType in btAllStrings then
    begin
    // string -> check that ResolvedValue is not merely a type, but has a value
    if CheckStringOrPointerIndex(true) then
      exit;
    end
  else if (ResolvedValue.IdentEl is TPasProperty)
      and (GetPasPropertyArgs(TPasProperty(ResolvedValue.IdentEl)).Count>0) then
    begin
    PropEl:=TPasProperty(ResolvedValue.IdentEl);
    CheckCallPropertyCompatibility(PropEl,Params,true);
    FinishPropertyParamAccess(Params,PropEl);
    exit;
    end
  else if ResolvedValue.BaseType=btPointer then
    begin
    if CheckStringOrPointerIndex(false) then
      exit;
    end
  else if ResolvedValue.BaseType=btContext then
    begin
    TypeEl:=ResolvedValue.LoTypeEl;
    if TypeEl.ClassType=TPasClassType then
      begin
      ClassScope:=NoNil(TypeEl.CustomData) as TPasClassScope;
      if ResolveBracketOperatorClass(Params,ResolvedValue,ClassScope,Access) then
        exit;
      end
    else if TypeEl.ClassType=TPasArrayType then
      begin
      if ResolvedValue.IdentEl is TPasType then
        RaiseMsg(20170216152215,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['[',ResolvedValue.IdentEl.ElementTypeName],Params);
      CheckCallArrayCompatibility(TPasArrayType(TypeEl),Params,true,true);
      for i:=0 to length(Params.Params)-1 do
        AccessExpr(Params.Params[i],rraRead);
      exit;
      end
    else if TypeEl.ClassType=TPasPointerType then
      begin
      if CheckStringOrPointerIndex(false) then exit;
      end;
    end;
  RaiseMsg(20170216152217,nIllegalQualifierAfter,sIllegalQualifierAfter,
    ['[',GetResolverResultDescription(ResolvedValue,true)],Params);
end;

function TPasResolver.ResolveBracketOperatorClass(Params: TParamsExpr;
  const ResolvedValue: TPasResolverResult; ClassScope: TPasClassScope;
  Access: TResolvedRefAccess): boolean;
var
  PropEl: TPasProperty;
  Value: TPasExpr;
begin
  PropEl:=ClassScope.DefaultProperty;
  if PropEl<>nil then
    begin
    // class has default property
    if (ResolvedValue.IdentEl is TPasType) and (not PropEl.IsClass) then
      RaiseMsg(20170216152213,nIllegalQualifierAfter,sIllegalQualifierAfter,
        ['[',GetResolverResultDescription(ResolvedValue,true)],Params);
    Value:=Params.Value;
    if Value.CustomData is TResolvedReference then
      SetResolvedRefAccess(Value,TResolvedReference(Value.CustomData),rraRead);
    CreateReference(PropEl,Params,Access);
    CheckCallPropertyCompatibility(PropEl,Params,true);
    FinishPropertyParamAccess(Params,PropEl);
    exit(true);
    end;
  Result:=false;
end;

procedure TPasResolver.ResolveSetParamsExpr(Params: TParamsExpr);
// e.g. resolving '[1,2..3]'
var
  i: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSetParamsExpr ',GetTreeDbg(Params));
  {$ENDIF}
  if Params.Value<>nil then
    RaiseNotYetImplemented(20160930135910,Params);
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[rcNoImplicitProcType,rcSetReferenceFlags]);
    end;
end;

procedure TPasResolver.ResolveArrayValues(El: TArrayValues);
var
  i: Integer;
begin
  for i:=0 to length(El.Values)-1 do
    ResolveExpr(El.Values[i],rraRead);
end;

procedure TPasResolver.ResolveRecordValues(El: TRecordValues);

  function GetMember(RecType: TPasRecordType; const aName: string): TPasElement;
  var
    i: Integer;
  begin
    for i:=0 to RecType.Members.Count-1 do
      begin
      Result:=TPasElement(RecType.Members[i]);
      if SameText(Result.Name,aName) then
        exit;
      end;
    if (RecType.VariantEl is TPasVariable) then
      begin
      Result:=TPasVariable(RecType.VariantEl);
      if SameText(Result.Name,aName) then
        exit;
      end;
    if RecType.Variants<>nil then
      for i:=0 to RecType.Variants.Count-1 do
        begin
        Result:=GetMember(TPasVariant(RecType.Variants[i]).Members,aName);
        if Result<>nil then
          exit;
        end;
    Result:=nil;
  end;

var
  i, j: Integer;
  Member: TPasElement;
  RecType: TPasRecordType;
  Field: PRecordValuesItem;
  s: String;
  ResolvedEl: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveRecordValues ',El.Fields[0].Name,' ',GetObjName(El.Parent),' ',GetObjName(El.Parent.Parent));
  {$ENDIF}
  ComputeElement(El,ResolvedEl,[]);
  if (ResolvedEl.BaseType<>btContext)
      or (ResolvedEl.LoTypeEl.ClassType<>TPasRecordType) then
    begin
    RaiseIncompatibleTypeDesc(20180429104135,nIncompatibleTypesGotExpected,
      [],'record value',GetTypeDescription(ResolvedEl),El);
    end;
  RecType:=TPasRecordType(ResolvedEl.LoTypeEl);
  //writeln('TPasResolver.ResolveRecordValues ',GetObjName(El.Parent),' ',GetObjName(RecType));
  for i:=0 to length(El.Fields)-1 do
    begin
    Field:=@El.Fields[i];
    // check member exists
    Member:=GetMember(RecType,Field^.Name);
    if Member=nil then
      RaiseIdentifierNotFound(20180429104703,Field^.Name,Field^.NameExp);
    if not (Member is TPasVariable) then
      RaiseMsg(20180429121933,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],Field^.ValueExp);
    CreateReference(Member,Field^.NameExp,rraAssign);
    // check duplicates
    for j:=0 to i-1 do
      if SameText(Field^.Name,El.Fields[j].Name) then
        RaiseMsg(20180429104942,nDuplicateIdentifier,sDuplicateIdentifier,
          [Field^.Name,GetElementSourcePosStr(El.Fields[j].NameExp)],Field^.NameExp);
    // resolve expression
    ResolveExpr(El.Fields[i].ValueExp,rraRead);
    // check compatible
    CheckAssignCompatibility(Member,Field^.ValueExp);
    end;
  // hint for missing fields
  s:='';
  for i:=0 to RecType.Members.Count-1 do
    begin
    Member:=TPasElement(RecType.Members[i]);
    if not (Member is TPasVariable) then continue;
    j:=length(El.Fields)-1;
    while (j>=0) and not SameText(Member.Name,El.Fields[j].Name) do
      dec(j);
    //writeln('TPasResolver.ResolveRecordValues ',GetObjName(Member),' ',j);
    if j<0 then
      begin
      if s<>'' then s:=s+', ';
      if length(s)>30 then
        begin
        s:=s+'...';
        break;
        end;
      s:=s+Member.Name;
      end;
    end;
  // ToDo: hint for missing variants
  if s<>'' then
    LogMsg(20180429121127,mtHint,nMissingFieldsX,sMissingFieldsX,[s],El);
end;

function TPasResolver.ResolveAccessor(Expr: TPasExpr): TPasElement;
var
  Prim: TPrimitiveExpr;
  DeclEl: TPasElement;
  Identifier: TPasIdentifier;
  Scope: TPasIdentifierScope;
begin
  if Expr.ClassType=TBinaryExpr then
    begin
    if (TBinaryExpr(Expr).left is TPrimitiveExpr) then
      begin
      Prim:=TPrimitiveExpr(TBinaryExpr(Expr).left);
      if Prim.Kind<>pekIdent then
        RaiseXExpectedButYFound(20170216151746,'class',Prim.Value,Prim);
      Scope:=TopScope as TPasIdentifierScope;
      // search in class and ancestors, not in unit interface
      Identifier:=Scope.FindIdentifier(Prim.Value);
      if Identifier=nil then
        RaiseIdentifierNotFound(20170216151749,Prim.Value,Prim);
      DeclEl:=Identifier.Element;
      if DeclEl.ClassType<>TPasClassType then
        RaiseXExpectedButYFound(20170216151752,'class',GetElementTypeName(DeclEl),Prim);
      CreateReference(DeclEl,Prim,rraRead);
      end
    else
      RaiseMsg(20170216151754,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
    if TBinaryExpr(Expr).OpCode<>eopSubIdent then
      RaiseMsg(20170216151757,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
    PushClassDotScope(TPasClassType(DeclEl));
    Expr:=TBinaryExpr(Expr).right;
    Result:=ResolveAccessor(Expr);
    PopScope;
    end
  else if Expr.ClassType=TPrimitiveExpr then
    begin
    Prim:=TPrimitiveExpr(Expr);
    if Prim.Kind<>pekIdent then
      RaiseXExpectedButYFound(20170216151800,'identifier',Prim.Value,Prim);
    Scope:=TopScope as TPasIdentifierScope;
    // search in class and ancestors, not in unit interface
    Identifier:=Scope.FindIdentifier(Prim.Value);
    if Identifier=nil then
      RaiseIdentifierNotFound(20170216151803,Prim.Value,Prim);
    DeclEl:=Identifier.Element;
    CreateReference(DeclEl,Prim,rraRead);
    Result:=DeclEl;
    end
  else
    RaiseNotYetImplemented(20160922163436,Expr);
end;

procedure TPasResolver.SetResolvedRefAccess(Expr: TPasExpr;
  Ref: TResolvedReference; Access: TResolvedRefAccess);
begin
  if (Ref.Access=Access) then exit;
  if Access in [rraNone,rraParamToUnknownProc] then
    exit;
  if Expr=nil then ;

  case Ref.Access of
    rraNone,rraParamToUnknownProc:
      Ref.Access:=Access;
    rraRead:
      if Access in [rraAssign,rraReadAndAssign,rraVarParam,rraOutParam] then
        Ref.Access:=rraReadAndAssign
      else
        exit;
    rraAssign,rraOutParam:
      if Access in [rraRead,rraReadAndAssign,rraVarParam] then
        Ref.Access:=rraReadAndAssign
      else
        exit;
    rraReadAndAssign: exit;
    rraVarParam: exit;
  else
    RaiseInternalError(20170403163727);
  end;
end;

procedure TPasResolver.AccessExpr(Expr: TPasExpr;
  Access: TResolvedRefAccess);
// called after a call target was found, called for each element
// to change the rraParamToUnknownProc value to Access
var
  Ref: TResolvedReference;
  Bin: TBinaryExpr;
  Params: TParamsExpr;
  ValueResolved: TPasResolverResult;
  C: TClass;
begin
  if (Expr.CustomData is TResolvedReference) then
    begin
    Ref:=TResolvedReference(Expr.CustomData);
    SetResolvedRefAccess(Expr,Ref,Access);
    end;

  C:=Expr.ClassType;
  if C=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Expr);
    if Bin.OpCode in [eopSubIdent,eopNone] then
      AccessExpr(Bin.right,Access);
    end
  else if C=TParamsExpr then
    begin
    Params:=TParamsExpr(Expr);
    case Params.Kind of
    pekFuncParams:
      if IsTypeCast(Params) then
        FinishCallArgAccess(Params.Params[0],Access)
      else
        AccessExpr(Params.Value,Access);
    pekArrayParams:
      begin
      ComputeElement(Params.Value,ValueResolved,[]);
      if IsDynArray(ValueResolved.LoTypeEl,false) then
        // an element of a dynamic array is independent of the array variable
        // an element of an open array depends on the argument
      else
        AccessExpr(Params.Value,Access);
      // Note: an element of an open or static array or a string is connected to the variable
      end;
    pekSet:
      if Access<>rraRead then
        RaiseMsg(20170306112306,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
    else
      RaiseNotYetImplemented(20170403173831,Params);
    end;
    end
  else if (C=TSelfExpr) or ((C=TPrimitiveExpr) and (TPrimitiveExpr(Expr).Kind=pekIdent)) then
    // ok
  else if (Access in [rraRead,rraParamToUnknownProc])
      and ((C=TPrimitiveExpr)
        or (C=TNilExpr)
        or (C=TBoolConstExpr)) then
    // ok
  else if C=TUnaryExpr then
    AccessExpr(TUnaryExpr(Expr).Operand,Access)
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.AccessExpr Expr=',GetObjName(Expr),' Access=',Access,' Declaration="',Expr.GetDeclaration(false),'"');
    {$ENDIF}
    RaiseNotYetImplemented(20170306102158,Expr);
    end;
end;

function TPasResolver.MarkArrayExpr(Expr: TParamsExpr; ArrayType: TPasArrayType
  ): boolean;
var
  Ref: TResolvedReference;
begin
  if Expr.CustomData=nil then
    begin
    // mark set expression as array
    CreateReference(ArrayType,Expr,rraRead);
    Result:=true;
    end
  else if Expr.CustomData is TResolvedReference then
    begin
    // already set
    Result:=false;
    // check consistency
    Ref:=TResolvedReference(Expr.CustomData);
    if not (Ref.Declaration is TPasArrayType) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.MarkArrayExpr Expr=',GetObjName(Expr),' Ref.Declaration=',GetObjName(Ref.Declaration),' ',Ref.Declaration.ParentPath);
      {$ENDIF}
      RaiseNotYetImplemented(20180618102230,Expr,GetObjName(Ref.Declaration));
      end;
    end
  else
    // already set with something else
    RaiseNotYetImplemented(20180618102408,Expr,GetObjName(Expr.CustomData));
end;

procedure TPasResolver.MarkArrayExprRecursive(Expr: TPasExpr;
  ArrType: TPasArrayType);

  procedure Traverse(CurExpr: TPasExpr; ArrayType: TPasArrayType; RgIndex: integer);
  var
    Params: TPasExprArray;
    i: Integer;
    ResolvedElType: TPasResolverResult;
    ParamsExpr: TParamsExpr;
    BuiltInProc: TResElDataBuiltInProc;
    Ref: TResolvedReference;
  begin
    if IsArrayOperatorAdd(CurExpr) then
      begin
      Traverse(TBinaryExpr(CurExpr).left,ArrayType,RgIndex);
      Traverse(TBinaryExpr(CurExpr).right,ArrayType,RgIndex);
      end
    else if CurExpr.ClassType=TParamsExpr then
      begin
      ParamsExpr:=TParamsExpr(CurExpr);
      Params:=ParamsExpr.Params;
      if CurExpr.Kind=pekSet then
        begin
        MarkArrayExpr(ParamsExpr,ArrayType);

        // traverse into nested expressions, e.g. [ A, B ]
        if length(Params)=0 then exit;
        inc(RgIndex);
        if RgIndex>length(ArrayType.Ranges) then
          begin
          ComputeElement(ArrayType.ElType,ResolvedElType,[rcType]);
          if (ResolvedElType.BaseType=btContext)
              and (ResolvedElType.LoTypeEl is TPasArrayType) then
            begin
            ArrayType:=TPasArrayType(ResolvedElType.LoTypeEl);
            RgIndex:=0;
            end
          else
            exit; // elements are not arrays
          end;
        for i:=0 to length(Params)-1 do
          Traverse(Params[i],ArrayType,RgIndex);
        end
      else if CurExpr.Kind=pekFuncParams then
        begin
        if TParamsExpr(CurExpr).Value.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(TParamsExpr(CurExpr).Value.CustomData);
          if (Ref.Declaration is TPasUnresolvedSymbolRef)
              and (Ref.Declaration.CustomData is TResElDataBuiltInProc) then
            begin
            BuiltInProc:=TResElDataBuiltInProc(Ref.Declaration.CustomData);
            if BuiltInProc.BuiltIn=bfConcatArray then
              begin
              // concat(array1,array2,...)
              for i:=0 to length(Params)-1 do
                Traverse(Params[i],ArrayType,RgIndex);
              end
            else if BuiltInProc.BuiltIn=bfCopyArray then
              // copy(array,...)
              Traverse(Params[0],ArrayType,RgIndex);
            end;
          end;
        end;
      end;
  end;

begin
  Traverse(Expr,ArrType,0);
end;

procedure TPasResolver.CheckPendingForwardProcs(El: TPasElement);
var
  i: Integer;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
  aClassType: TPasClassType;
begin
  if IsElementSkipped(El) then exit;
  if El is TPasDeclarations then
    begin
    for i:=0 to TPasDeclarations(El).Declarations.Count-1 do
      begin
      DeclEl:=TPasElement(TPasDeclarations(El).Declarations[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        if ProcNeedsImplProc(Proc)
            and (TPasProcedureScope(Proc.CustomData).ImplProc=nil) then
          RaiseMsg(20170216152219,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(Proc),Proc.Name],Proc);
        end;
      end;
    end
  else if El.ClassType=TPasClassType then
    begin
    aClassType:=TPasClassType(El);
    if aClassType.ObjKind in [okInterface,okDispInterface] then exit;
    for i:=0 to aClassType.Members.Count-1 do
      begin
      DeclEl:=TPasElement(aClassType.Members[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        if Proc.IsAbstract or Proc.IsExternal then continue;
        if TPasProcedureScope(Proc.CustomData).ImplProc=nil then
          RaiseMsg(20170216152221,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(Proc),Proc.Name],Proc);
        end;
      end;
    end;
end;

procedure TPasResolver.CheckPointerCycle(El: TPasPointerType);
var
  C: TClass;
  CurEl, Dest: TPasType;
begin
  CurEl:=El;
  while CurEl<>nil do
    begin
    C:=CurEl.ClassType;
    if C=TPasPointerType then
      Dest:=TPasPointerType(CurEl).DestType
    else if C.InheritsFrom(TPasAliasType) then
      Dest:=TPasAliasType(CurEl).DestType
    else
      exit;
    if Dest=El then
      RaiseMsg(20180422165758,nTypeCycleFound,sTypeCycleFound,[],El);
    CurEl:=Dest;
    end;
end;

procedure TPasResolver.ComputeUnaryNot(El: TUnaryExpr;
  var ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
begin
  RaiseMsg(20180208121532,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
    [OpcodeStrings[El.OpCode],GetResolverResultDescription(ResolvedEl)],El);
  if Flags=[] then ;
end;

procedure TPasResolver.AddModule(El: TPasModule);
var
  C: TClass;
  ModScope: TPasModuleScope;
begin
  if TopScope<>DefaultScope then
    RaiseInvalidScopeForElement(20160922163504,El);
  ModScope:=TPasModuleScope(PushScope(El,FScopeClass_Module));
  ModScope.VisibilityContext:=El;
  ModScope.FirstName:=FirstDottedIdentifier(El.Name);
  C:=El.ClassType;
  if (C=TPasProgram) or (C=TPasLibrary) or (C=TPasPackage) then
    FDefaultNameSpace:=ChompDottedIdentifier(El.Name)
  else
    FDefaultNameSpace:='';
  ModScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
end;

procedure TPasResolver.AddSection(El: TPasSection);
// TInterfaceSection, TImplementationSection, TProgramSection, TLibrarySection
// Note: implementation scope is within the interface scope
var
  Scope: TPasSectionScope;
begin
  if TopScope is TPasSectionScope then
    FinishSection(TPasSectionScope(TopScope).Element as TPasSection);
  if TopScope is TPasModuleScope then
    TPasModuleScope(TopScope).BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  FPendingForwardProcs.Add(El); // check forward declarations at the end
  Scope:=TPasSectionScope(PushScope(El,ScopeClass_Section));
  Scope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  Scope.ModeSwitches:=CurrentParser.Scanner.CurrentModeSwitches;
end;

procedure TPasResolver.AddInitialFinalizationSection(El: TPasImplBlock);
begin
  PushScope(El,ScopeClass_InitialFinalization);
end;

procedure TPasResolver.AddType(El: TPasType);
begin
  if (El.Name='') then exit; // sub type
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddType El=',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163506,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddRecordType(El: TPasRecordType);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddRecordType ',GetObjName(El),' Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163508,El);
  if El.Name<>'' then begin
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
    FPendingForwardProcs.Add(El); // check forward declarations at the end
  end;

  if El.Parent.ClassType<>TPasVariant then
    PushScope(El,TPasRecordScope);
end;

procedure TPasResolver.AddClassType(El: TPasClassType);
// Note: IsForward is not yet set!
var
  Duplicate: TPasIdentifier;
  ForwardDecl: TPasClassType;
  CurScope: TPasIdentifierScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddClassType ',GetObjName(El),' Parent=',GetObjName(El.Parent),' ',GetElementSourcePosStr(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163510,El);

  CurScope:=TPasIdentifierScope(TopScope);
  Duplicate:=CurScope.FindLocalIdentifier(El.Name);
  //if Duplicate<>nil then
    //writeln('  Duplicate=',GetObjName(Duplicate.Element),' ',ord(Duplicate.Kind));

  if (Duplicate<>nil)
      and (Duplicate.Element is TPasClassType)
      and TPasClassType(Duplicate.Element).IsForward
      and (Duplicate.Element.Parent=El.Parent)
  then
    begin
    // forward declaration found
    ForwardDecl:=TPasClassType(Duplicate.Element);
    {$IFDEF VerbosePasResolver}
    writeln('  Resolving Forward=',GetObjName(ForwardDecl),' ',GetElementSourcePosStr(ForwardDecl));
    {$ENDIF}
    if ForwardDecl.CustomData<>nil then
      RaiseInternalError(20160922163513,'forward class has already customdata');
    // create a ref from the forward to the real declaration
    CreateReference(El,ForwardDecl,rraRead);
    // change the cache item
    Duplicate.Element:=El;
    end
  else
    AddIdentifier(CurScope,El.Name,El,pikSimple);

  FPendingForwardProcs.Add(El); // check forward declarations at the end
end;

procedure TPasResolver.AddVariable(El: TPasVariable);
begin
  if (El.Name='') then exit; // anonymous var
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddVariable ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205730,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddResourceString(El: TPasResString);
var
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddResourceString ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20171004092114,El);
  C:=El.Parent.ClassType;
  if not C.InheritsFrom(TPasSection) then
    RaiseNotYetImplemented(20171004092518,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddEnumType(El: TPasEnumType);
var
  CanonicalSet: TPasSetType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumType ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205732,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
  PushScope(El,TPasEnumTypeScope);
  // add canonical set
  if El.Parent is TPasSetType then
    begin
    // anonymous enumtype, e.g. "set of ()"
    CanonicalSet:=TPasSetType(El.Parent);
    CanonicalSet.AddRef{$IFDEF CheckPasTreeRefCount}('TPasEnumTypeScope.CanonicalSet'){$ENDIF};
    end
  else
    begin
    CanonicalSet:=TPasSetType.Create('',El);
    {$IFDEF CheckPasTreeRefCount}CanonicalSet.RefIds.Add('TPasEnumTypeScope.CanonicalSet'){$ENDIF};
    CanonicalSet.EnumType:=El;
    El.AddRef{$IFDEF CheckPasTreeRefCount}('TPasSetType.EnumType'){$ENDIF};
    end;
  TPasEnumTypeScope(TopScope).CanonicalSet:=CanonicalSet;
end;

procedure TPasResolver.AddEnumValue(El: TPasEnumValue);
var
  i: Integer;
  Scope: TPasScope;
  Old: TPasIdentifier;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumValue ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasEnumTypeScope) then
    RaiseInvalidScopeForElement(20160929205736,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

  // propagate enum to parent scopes
  //  TEnum = (red, green); -> dot not propagate
  //  TFlags = set of (red,blue); -> propagate
  if (bsScopedEnums in CurrentParser.Scanner.CurrentBoolSwitches)
      and not (El.Parent.Parent is TPasSetType) then
    exit;
  for i:=ScopeCount-2 downto 0 do
    begin
    Scope:=Scopes[i];
    if (Scope is TPasClassScope) or (Scope is TPasRecordScope) then
      begin
      // class or record: add if not duplicate
      Old:=TPasIdentifierScope(Scope).FindIdentifier(El.Name);
      if Old=nil then
        TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      end
    else if (Scope is TPasProcedureScope) or (Scope is TPasSectionScope) then
      begin
      // procedure or section: check for duplicate and add
      Old:=TPasIdentifierScope(Scope).FindLocalIdentifier(El.Name);
      if Old<>nil then
        RaiseMsg(20170216152224,nDuplicateIdentifier,sDuplicateIdentifier,
                 [El.Name,GetElementSourcePosStr(Old.Element)],El);
      TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      break;
      end
    else
      break;
    end;
end;

procedure TPasResolver.AddProperty(El: TPasProperty);
begin
  if (El.Name='') then
    RaiseNotYetImplemented(20160922163518,El);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProperty ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasClassScope) then
    RaiseInvalidScopeForElement(20160922163520,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
  PushScope(El,TPasPropertyScope);
end;

procedure TPasResolver.AddProcedure(El: TPasProcedure);
var
  ProcName, aClassName: String;
  p: SizeInt;
  CurClassType: TPasClassType;
  ProcScope: TPasProcedureScope;
  HasDot: Boolean;
  CurEl: TPasElement;
  Identifier: TPasIdentifier;
  CurClassScope: TPasClassScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProcedure ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163522,El);
  // Note: El.ProcType is nil !
  ProcName:=El.Name;
  HasDot:=Pos('.',ProcName)>1;
  if not HasDot then
    AddIdentifier(TPasIdentifierScope(TopScope),ProcName,El,pikProc);
  ProcScope:=TPasProcedureScope(PushScope(El,FScopeClass_Proc));
  ProcScope.ModeSwitches:=CurrentParser.CurrentModeswitches;
  if HasDot then
    begin
    // method implementation -> search class
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.AddProcedure searching class of "',ProcName,'" ...');
    {$ENDIF}
    CurClassType:=nil;
    repeat
      p:=Pos('.',ProcName);
      if p<1 then
        begin
        if CurClassType=nil then
          RaiseInternalError(20161013170829);
        break;
        end;
      aClassName:=LeftStr(ProcName,p-1);
      Delete(ProcName,1,p);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.AddProcedure searching class "',aClassName,'" ProcName="',ProcName,'" ...');
      {$ENDIF}
      if not IsValidIdent(aClassName) then
        RaiseNotYetImplemented(20161013170844,El);

      if CurClassType<>nil then
        begin
        CurClassScope:=TPasClassScope(CurClassType.CustomData);
        Identifier:=CurClassScope.FindLocalIdentifier(aClassName);
        if Identifier=nil then
          RaiseIdentifierNotFound(20180430130635,aClassName,El);
        CurEl:=Identifier.Element;
        end
      else
        CurEl:=FindElementWithoutParams(aClassName,El,false);

      if not (CurEl is TPasClassType) then
        begin
        aClassName:=LeftStr(El.Name,length(El.Name)-length(ProcName)-1);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.AddProcedure searching class "',aClassName,'" ProcName="',ProcName,'" found: '+GetObjName(CurEl));
        {$ENDIF}
        RaiseXExpectedButYFound(20170216152557,
          'class',aClassname+':'+GetElementTypeName(CurEl),El);
        end;
      CurClassType:=TPasClassType(CurEl);
      if CurClassType.ObjKind<>okClass then
        begin
        aClassName:=LeftStr(El.Name,length(El.Name)-length(ProcName)-1);
        RaiseXExpectedButYFound(20180321161722,
          'class',aClassname+':'+GetElementTypeName(CurEl),El);
        end;
      if CurClassType.GetModule<>El.GetModule then
        begin
        aClassName:=LeftStr(El.Name,length(El.Name)-length(ProcName)-1);
        RaiseMsg(20180211230432,nMethodClassXInOtherUnitY,sMethodClassXInOtherUnitY,
          [aClassName,CurClassType.GetModule.Name],El);
        end;
    until false;

    if not IsValidIdent(ProcName) then
      RaiseNotYetImplemented(20161013170956,El);

    ProcScope.VisibilityContext:=CurClassType;
    ProcScope.ClassScope:=NoNil(CurClassType.CustomData) as TPasClassScope;
    end;
end;

procedure TPasResolver.AddArgument(El: TPasArgument);
var
  ProcType: TPasProcedureType;
  i: Integer;
  Arg: TPasArgument;
begin
  if (El.Name='') then
    RaiseInternalError(20160922163526,GetObjName(El));
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArgument ',GetObjName(El));
  {$ENDIF}
  if (TopScope=nil) then
    RaiseInvalidScopeForElement(20160922163529,El);
  if El.Parent.ClassType=TPasProperty then
    begin
    if TopScope.ClassType<>TPasPropertyScope then
      RaiseInvalidScopeForElement(20161014124530,El);
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
    end
  else if El.Parent is TPasProcedureType then
    begin
    ProcType:=TPasProcedureType(El.Parent);
    if ProcType.Parent is TPasProcedure then
      begin
      if TopScope.ClassType<>FScopeClass_Proc then
        RaiseInvalidScopeForElement(20160922163529,El,GetObjName(TopScope));
      AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
      end
    else
      begin
      for i:=0 to ProcType.Args.Count-1 do
        begin
        Arg:=TPasArgument(ProcType.Args[i]);
        if (Arg<>El) and (CompareText(TPasArgument(ProcType.Args[i]).Name,El.Name)=0) then
          RaiseMsg(20170216152225,nDuplicateIdentifier,sDuplicateIdentifier,[Arg.Name,GetElementSourcePosStr(Arg)],El);
        end;
      end;
    end
  else
    RaiseNotYetImplemented(20161014124937,El);
end;

procedure TPasResolver.AddFunctionResult(El: TPasResultElement);
begin
  if TopScope.ClassType<>FScopeClass_Proc then exit;
  if El.Parent is TPasProcedureType then
    begin
    if not (El.Parent.Parent is TPasProcedure) then
      exit;
    end
  else if not (El.Parent is TPasProcedure) then
    exit;
  AddIdentifier(TPasProcedureScope(TopScope),ResolverResultVar,El,pikSimple);
end;

procedure TPasResolver.AddExceptOn(El: TPasImplExceptOn);
begin
  PushScope(El,TPasExceptOnScope);
end;

procedure TPasResolver.AddProcedureBody(El: TProcedureBody);
begin
  if El=nil then ;
  CheckTopScope(FScopeClass_Proc);
end;

procedure TPasResolver.WriteScopes;
{AllowWriteln}
var
  i: Integer;
  Scope: TPasScope;
begin
  writeln('TPasResolver.WriteScopes ScopeCount=',ScopeCount);
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    writeln('  ',i,'/',ScopeCount,' ',GetObjName(Scope));
    Scope.WriteIdentifiers('  ');
    end;
  {AllowWriteln-}
end;

procedure TPasResolver.ComputeBinaryExpr(Bin: TBinaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  LeftResolved, RightResolved: TPasResolverResult;
begin
  if (Bin.OpCode=eopSubIdent)
  or ((Bin.OpCode=eopNone) and (Bin.left is TInheritedExpr)) then
    begin
    // Note: bin.left was already resolved via ResolveSubIdent
    ComputeElement(Bin.right,ResolvedEl,Flags,StartEl);
    exit;
    end;

  if Bin.OpCode in [eopEqual,eopNotEqual] then
    begin
    if CheckEqualElCompatibility(Bin.left,Bin.right,nil,true,
        rcSetReferenceFlags in Flags)=cIncompatible then
      RaiseInternalError(20161007215912);
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                         Bin,[rrfReadable]);
    exit;
    end;

  ComputeElement(Bin.left,LeftResolved,Flags-[rcNoImplicitProc],StartEl);
  ComputeElement(Bin.right,RightResolved,Flags-[rcNoImplicitProc],StartEl);
  // ToDo: check operator overloading

  ComputeBinaryExprRes(Bin,ResolvedEl,Flags,LeftResolved,RightResolved);
end;

procedure TPasResolver.ComputeBinaryExprRes(Bin: TBinaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  var LeftResolved, RightResolved: TPasResolverResult);

  procedure SetBaseType(BaseType: TResolverBaseType);
  begin
    SetResolverValueExpr(ResolvedEl,BaseType,FBaseTypes[BaseType],FBaseTypes[BaseType],
                         Bin,[rrfReadable]);
  end;

  procedure SetLeftValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,
      LeftResolved.LoTypeEl,LeftResolved.HiTypeEl,Bin,Flags);
  end;

  procedure SetRightValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,RightResolved.BaseType,
      RightResolved.LoTypeEl,RightResolved.HiTypeEl,Bin,Flags);
  end;

var
  ElTypeResolved: TPasResolverResult;
  LeftTypeEl, RightTypeEl: TPasType;
begin
  if LeftResolved.BaseType=btRange then
    ConvertRangeToElement(LeftResolved);
  if RightResolved.BaseType=btRange then
    ConvertRangeToElement(RightResolved);

  //writeln('TPasResolver.ComputeBinaryExpr ',OpcodeStrings[Bin.OpCode],' Left=',GetResolverResultDbg(LeftResolved),' Right=',GetResolverResultDbg(RightResolved));

  if LeftResolved.BaseType in btAllInteger then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
        if (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
          case Bin.OpCode of
          eopNone:
            if (Bin.Kind=pekRange) then
              begin
              if not (RightResolved.BaseType in btAllInteger) then
                RaiseXExpectedButYFound(20170216152600,'integer',BaseTypeNames[RightResolved.BaseType],Bin.right);
              // use left type for result
              SetLeftValueExpr([rrfReadable]);
              if Bin.Parent is TPasRangeType then
                begin
                ResolvedEl.LoTypeEl:=TPasRangeType(Bin.Parent);
                ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
                end;
              exit;
              end;
          eopAdd, eopSubtract,
          eopMultiply, eopDiv, eopMod,
          eopPower,
          eopShl, eopShr,
          eopAnd, eopOr, eopXor:
            begin
            if RightResolved.BaseType in btAllFloats then
              // use right type for result
              SetRightValueExpr([rrfReadable])
            else
              // use left type for result
              SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          eopLessThan,
          eopGreaterThan,
          eopLessthanEqual,
          eopGreaterThanEqual:
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          eopDivide:
            begin
            SetBaseType(BaseTypeExtended);
            exit;
            end;
          end
        else if (RightResolved.BaseType in [btSet,btArrayOrSet]) then
          begin
          if (Bin.OpCode=eopIn) and (RightResolved.SubType in btAllInteger) then
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          end
        else if RightResolved.BaseType=btPointer then
          begin
          if (Bin.OpCode in [eopAdd,eopSubtract])
              and (bsPointerMath in GetElBoolSwitches(Bin)) then
            begin
            // integer+CanonicalPointer
            SetResolverValueExpr(ResolvedEl,btPointer,
              RightResolved.LoTypeEl,RightResolved.HiTypeEl,Bin,[rrfReadable]);
            exit;
            end;
          end
        else if RightResolved.BaseType=btContext then
          begin
          RightTypeEl:=RightResolved.LoTypeEl;
          if RightTypeEl.ClassType=TPasPointerType then
            begin
            if (Bin.OpCode in [eopAdd,eopSubtract])
                and (bsPointerMath in GetElBoolSwitches(Bin)) then
              begin
              // integer+TypedPointer
              RightTypeEl:=TPasPointerType(RightTypeEl).DestType;
              SetResolverValueExpr(ResolvedEl,btPointer,
                ResolveAliasType(RightTypeEl),RightTypeEl,Bin,[rrfReadable]);
              exit;
              end;
            end;
          end;
      end;
    end
  else if LeftResolved.BaseType in btAllBooleans then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in btAllBooleans)
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopNone:
        if Bin.Kind=pekRange then
          begin
          SetResolverValueExpr(ResolvedEl,btRange,
            FBaseTypes[LeftResolved.BaseType],FBaseTypes[LeftResolved.BaseType],
            Bin,[rrfReadable]);
          ResolvedEl.SubType:=LeftResolved.BaseType;
          exit;
          end;
      eopAnd, eopOr, eopXor:
        begin
        // use left type for result
        SetLeftValueExpr([rrfReadable]);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType in btAllStringAndChars then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in btAllStringAndChars) then
        case Bin.OpCode of
        eopNone:
          if (Bin.Kind=pekRange) and (LeftResolved.BaseType in btAllChars) then
            begin
            if not (RightResolved.BaseType in btAllChars) then
              RaiseXExpectedButYFound(20170216152603,'char',BaseTypeNames[RightResolved.BaseType],Bin.right);
            SetResolverValueExpr(ResolvedEl,btRange,
              FBaseTypes[LeftResolved.BaseType],FBaseTypes[LeftResolved.BaseType],
              Bin,[rrfReadable]);
            ResolvedEl.SubType:=LeftResolved.BaseType;
            exit;
            end;
        eopAdd:
          case LeftResolved.BaseType of
          btChar:
            begin
            case RightResolved.BaseType of
            btChar: SetBaseType(btString);
            {$ifdef FPC_HAS_CPSTRING}
            btAnsiChar:
              if BaseTypeChar=btAnsiChar then
                SetBaseType(btString)
              else
                SetBaseType(btUnicodeString);
            {$endif}
            btWideChar:
              if BaseTypeChar=btWideChar then
                SetBaseType(btString)
              else
                SetBaseType(btUnicodeString);
            else
              // use right type for result
              SetRightValueExpr([rrfReadable]);
            end;
            exit;
            end;
          {$ifdef FPC_HAS_CPSTRING}
          btAnsiChar:
            begin
            case RightResolved.BaseType of
            btChar:
              if BaseTypeChar=btAnsiChar then
                SetBaseType(btString)
              else
                SetBaseType(btUnicodeString);
            btAnsiChar:
              if BaseTypeChar=btAnsiChar then
                SetBaseType(btString)
              else
                SetBaseType(btAnsiString);
            btWideChar:
              if BaseTypeChar=btWideChar then
                SetBaseType(btString)
              else
                SetBaseType(btUnicodeString);
            else
              // use right type for result
              SetRightValueExpr([rrfReadable]);
            end;
            exit;
            end;
          {$endif}
          btWideChar:
            begin
              case RightResolved.BaseType of
              btChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar:
                if BaseTypeChar=btWideChar then
                  SetBaseType(btString)
                else
                  SetBaseType(btUnicodeString);
              else
                // use right type for result
                SetRightValueExpr([rrfReadable]);
              end;
              exit;
            end;
          {$ifdef FPC_HAS_CPSTRING}
          btShortString:
            begin
              case RightResolved.BaseType of
              btChar,btAnsiChar,btShortString,btWideChar:
                // use left type for result
                SetLeftValueExpr([rrfReadable]);
              else
                // shortstring + string => string
                SetRightValueExpr([rrfReadable]);
              end;
              exit;
            end;
          {$endif}
          btString,{$ifdef FPC_HAS_CPSTRING}btAnsiString,{$endif}btUnicodeString:
            begin
              // string + x => string
              SetLeftValueExpr([rrfReadable]);
              exit;
            end;
          end;
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end
      else if (RightResolved.BaseType in [btSet,btArrayOrSet])
          and (RightResolved.SubType in btAllChars)
          and (LeftResolved.BaseType in btAllChars) then
        begin
        case Bin.OpCode of
        eopIn:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end;
        end
      end
    end
  else if LeftResolved.BaseType in btAllFloats then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in (btAllInteger+btAllFloats))
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopAdd, eopSubtract,
      eopMultiply, eopDivide, eopMod,
      eopPower:
        begin
        if (RightResolved.BaseType=btCurrency)
            or ((RightResolved.BaseType in btAllFloats)
                and (RightResolved.BaseType>LeftResolved.BaseType)) then
          // use right side as result
          SetRightValueExpr([rrfReadable])
        else
          // use left side as result
          SetLeftValueExpr([rrfReadable]);
        exit;
        end;
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetBaseType(btBoolean);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btPointer then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in btAllInteger) then
        case Bin.OpCode of
        eopAdd,eopSubtract:
          if bsPointerMath in GetElBoolSwitches(Bin) then
            begin
            // pointer+integer -> pointer
            SetResolverValueExpr(ResolvedEl,btPointer,
              LeftResolved.LoTypeEl,LeftResolved.HiTypeEl,Bin,[rrfReadable]);
            exit;
            end;
        end
      else if RightResolved.BaseType=btPointer then
        case Bin.OpCode of
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end;
      end;
    end
  else if LeftResolved.BaseType=btContext then
    begin
    LeftTypeEl:=LeftResolved.LoTypeEl;
    case Bin.OpCode of
    eopNone:
      if Bin.Kind=pekRange then
        begin
        if (rrfReadable in LeftResolved.Flags)
            and (rrfReadable in RightResolved.Flags) then
          begin
          CheckSetLitElCompatible(Bin.left,Bin.right,LeftResolved,RightResolved);
          ResolvedEl:=LeftResolved;
          ResolvedEl.IdentEl:=nil;
          ResolvedEl.SubType:=ResolvedEl.BaseType;
          ResolvedEl.BaseType:=btRange;
          ResolvedEl.ExprEl:=Bin;
          exit;
          end;
        end;
    eopIn:
      if (rrfReadable in LeftResolved.Flags)
      and (rrfReadable in RightResolved.Flags) then
        begin
        if LeftResolved.BaseType in btArrayRangeTypes then
          begin
          if not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
            RaiseXExpectedButYFound(20170216152607,'set of '+BaseTypeNames[LeftResolved.BaseType],GetElementTypeName(LeftResolved.LoTypeEl),Bin.right);
          if LeftResolved.BaseType in btAllBooleans then
            begin
            if not (RightResolved.SubType in btAllBooleans) then
              RaiseXExpectedButYFound(20170216152610,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
            end
          else if LeftResolved.BaseType in btAllChars then
            begin
            if not (RightResolved.SubType in btAllChars) then
              RaiseXExpectedButYFound(20170216152609,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
            end
          else if not (RightResolved.SubType in btAllInteger) then
            RaiseXExpectedButYFound(20170216152612,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftResolved.BaseType=btContext)
            and (LeftTypeEl.ClassType=TPasEnumType) then
          begin
          if not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
            RaiseXExpectedButYFound(20170216152615,'set of '+LeftResolved.LoTypeEl.Name,GetElementTypeName(LeftResolved.LoTypeEl),Bin.right);
          RightTypeEl:=RightResolved.LoTypeEl;
          if LeftTypeEl=RightTypeEl then
            // enum in setofenum
          else if RightResolved.LoTypeEl.ClassType=TPasRangeType then
            begin
            ComputeElement(TPasRangeType(RightTypeEl).RangeExpr.left,ElTypeResolved,[rcConstant]);
            if LeftTypeEl<>ElTypeResolved.LoTypeEl then
              RaiseXExpectedButYFound(20171109215833,'set of '+LeftResolved.LoTypeEl.Name,'set of '+RightResolved.LoTypeEl.Name,Bin.right);
            end
          else
            RaiseXExpectedButYFound(20170216152618,'set of '+LeftResolved.LoTypeEl.Name,'set of '+RightResolved.LoTypeEl.Name,Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else
          RaiseMsg(20170216152228,nInOperatorExpectsSetElementButGot,
            sInOperatorExpectsSetElementButGot,[GetElementTypeName(LeftResolved.LoTypeEl)],Bin);
        end;
    eopIs:
      begin
      RightTypeEl:=RightResolved.LoTypeEl;
      if (LeftTypeEl is TPasClassType) then
        begin
        if not (rrfReadable in LeftResolved.Flags) then
          RaiseIncompatibleTypeRes(20180204124637,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        if (LeftResolved.IdentEl is TPasType) then
          RaiseIncompatibleTypeRes(20180204124638,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        // left side is a class instance
        if (RightResolved.IdentEl is TPasType)
            and (RightTypeEl is TPasClassType) then
          begin
          if TPasClassType(LeftTypeEl).ObjKind=TPasClassType(RightTypeEl).ObjKind then
            begin
            if CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible then
              begin
              // e.g. if obj is TFPMemoryImage then ;
              // Note: at compile time the check is reversed: right must inherit from left
              SetBaseType(btBoolean);
              exit;
              end
            else if CheckSrcIsADstType(LeftResolved,RightResolved,Bin)<>cIncompatible then
              begin
              // e.g. if Image is TObject then ;
              // This is useful after some unchecked typecast -> allow
              SetBaseType(btBoolean);
              exit;
              end;
            end
          else if TPasClassType(RightTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(LeftTypeEl).ObjKind=okClass)
                and (not TPasClassType(LeftTypeEl).IsExternal) then
              begin
              // e.g. if classintvar is intftype then ;
              SetBaseType(btBoolean);
              exit;
              end;
            end
          else if TPasClassType(LeftTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(RightTypeEl).ObjKind=okClass)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              // e.g. if intfvar is classtype then ;
              SetBaseType(btBoolean);
              exit;
              end;
            end;
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeBinaryExprRes LeftClass=',GetClassAncestorsDbg(TPasClassType(LeftResolved.LoTypeEl)));
          writeln('TPasResolver.ComputeBinaryExprRes RightClass=',GetClassAncestorsDbg(TPasClassType(RightResolved.IdentEl)));
          {$ENDIF}
          end
        else if (RightTypeEl is TPasClassOfType)
            and (rrfReadable in RightResolved.Flags) then
          begin
          // e.g. if Image is ImageClass then ;
          if (CheckClassesAreRelated(LeftResolved.LoTypeEl,
              TPasClassOfType(RightTypeEl).DestType,Bin)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          end
        else
          RaiseXExpectedButYFound(20170216152625,'class type',GetElementTypeName(RightResolved.LoTypeEl),Bin.right);
        end
      else if (proClassOfIs in Options) and (LeftTypeEl is TPasClassOfType)
          and (rrfReadable in LeftResolved.Flags) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType) then
          RaiseIncompatibleTypeRes(20180204124657,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        // left side is class-of variable
        LeftTypeEl:=ResolveAliasType(TPasClassOfType(LeftTypeEl).DestType);
        if (RightResolved.IdentEl is TPasType)
            and (ResolveAliasType(TPasType(RightResolved.IdentEl)) is TPasClassType) then
          begin
          // e.g. if ImageClass is TFPMemoryImage then ;
          // Note: at compile time the check is reversed: right must inherit from left
          if CheckClassIsClass(RightResolved.LoTypeEl,LeftTypeEl,Bin)<>cIncompatible then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else if (RightTypeEl is TPasClassOfType) then
          begin
          // e.g. if ImageClassA is ImageClassB then ;
          // or   if ImageClassA is TFPImageClass then ;
          RightTypeEl:=ResolveAliasType(TPasClassOfType(RightTypeEl).DestType);
          if (CheckClassesAreRelated(LeftTypeEl,RightTypeEl,Bin)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else
          RaiseXExpectedButYFound(20170322105252,'class type',GetElementTypeName(RightResolved.LoTypeEl),Bin.right);
        end
      else if LeftResolved.LoTypeEl=nil then
        RaiseMsg(20170216152232,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [BaseTypeNames[LeftResolved.BaseType]],Bin.left)
      else
        RaiseMsg(20170216152234,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [GetElementTypeName(LeftResolved.LoTypeEl)],Bin.left);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ComputeBinaryExprRes is-operator: left=',GetResolverResultDbg(LeftResolved),' right=',GetResolverResultDbg(RightResolved));
      {$ENDIF}
      RaiseIncompatibleTypeRes(20170216152236,nTypesAreNotRelatedXY,[],LeftResolved,RightResolved,Bin);
      end;
    eopAs:
      begin
      if (LeftTypeEl.ClassType=TPasClassType) then
        begin
        if (LeftResolved.IdentEl is TPasType)
            or (not (rrfReadable in LeftResolved.Flags)) then
          RaiseIncompatibleTypeRes(20180204124711,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        if RightResolved.IdentEl=nil then
          RaiseXExpectedButYFound(20170216152630,'class',GetElementTypeName(RightResolved.LoTypeEl),Bin.right);
        if not (RightResolved.IdentEl is TPasType) then
          RaiseXExpectedButYFound(20170216152632,'class',RightResolved.IdentEl.Name,Bin.right);
        if not (RightResolved.BaseType=btContext) then
          RaiseXExpectedButYFound(20180426195816,'class',RightResolved.IdentEl.Name,Bin.right);
        RightTypeEl:=RightResolved.LoTypeEl;
        if RightTypeEl is TPasClassType then
          begin
          if TPasClassType(LeftTypeEl).ObjKind=TPasClassType(RightTypeEl).ObjKind then
            begin
            // e.g. classinst as classtype
            if (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
              begin
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end
          else if TPasClassType(LeftTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(RightTypeEl).ObjKind=okClass)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              // e.g. intfvar as classtype
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end
          else if TPasClassType(RightTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(LeftTypeEl).ObjKind=okClass)
                and (not TPasClassType(LeftTypeEl).IsExternal) then
              begin
              // e.g. classinst as intftype
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end;
          end;
        RaiseIncompatibleTypeRes(20180324190713,nTypesAreNotRelatedXY,[],LeftResolved,RightResolved,Bin);
        end;
      end;
    eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual:
      if (rrfReadable in LeftResolved.Flags)
          and (rrfReadable in RightResolved.Flags) then
        begin
        RightTypeEl:=RightResolved.LoTypeEl;
        if (LeftTypeEl.ClassType=TPasEnumType) and (LeftTypeEl=RightTypeEl) then
          begin
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftTypeEl.ClassType=TPasPointerType)
            and (RightResolved.BaseType in btAllInteger) then
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end;
    eopSubIdent:
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    eopAdd,eopSubtract:
      if (rrfReadable in LeftResolved.Flags)
          and (rrfReadable in RightResolved.Flags) then
        begin
        if (LeftTypeEl.ClassType=TPasArrayType) then
          begin
          if IsDynArray(LeftTypeEl)
              and (Bin.OpCode=eopAdd)
              and (msArrayOperators in GetElModeSwitches(Bin))
              and ((RightResolved.BaseType in [btArrayOrSet,btArrayLit])
                or IsDynArray(RightResolved.LoTypeEl)) then
            begin
            // dynarr+[...]
            CheckAssignCompatibilityArrayType(LeftResolved,RightResolved,Bin,true);
            SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          end
        else if LeftTypeEl.ClassType=TPasPointerType then
          begin
          if (RightResolved.BaseType in btAllInteger)
              and (bsPointerMath in GetElBoolSwitches(Bin)) then
            begin
            // TypedPointer+Integer
            SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          end;
        end;
    end;

    end
  else if LeftResolved.BaseType in [btSet,btArrayOrSet] then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in [btSet,btArrayOrSet]) then
        case Bin.OpCode of
        eopAdd,
        eopSubtract,
        eopMultiply,
        eopSymmetricaldifference,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          if RightResolved.LoTypeEl=nil then
            begin
            // right is empty set/array
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=LeftResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end
          else if LeftResolved.LoTypeEl=nil then
            begin
            // left is empty set/array
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=RightResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end
          else if (LeftResolved.SubType=RightResolved.SubType)
              or ((LeftResolved.SubType in btAllBooleans)
                and (RightResolved.SubType in btAllBooleans))
              or ((LeftResolved.SubType in btAllInteger)
                and (RightResolved.SubType in btAllInteger)) then
            begin
            // compatible set
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=LeftResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end;
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeBinaryExprRes + - * >< Sets LeftSubType='+BaseTypeNames[LeftResolved.SubType]
            +' RightSubType='+BaseTypeNames[RightResolved.SubType]);
          {$ENDIF}
          end;
        end
      else if RightResolved.BaseType=btContext then
        begin
        RightTypeEl:=RightResolved.LoTypeEl;
        if RightTypeEl.ClassType=TPasArrayType then
          begin
          if IsDynArray(RightTypeEl) then
            begin
            // [...]+dynarr
            CheckAssignCompatibilityArrayType(RightResolved,LeftResolved,Bin,true);
            SetRightValueExpr([rrfReadable]);
            exit;
            end;
          end;
        end;
      end;
    end
  else if LeftResolved.BaseType=btArrayLit then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags)
        and (Bin.OpCode=eopAdd)
        and (msArrayOperators in GetElModeSwitches(Bin)) then
      begin
      if RightResolved.BaseType=btArrayLit then
        begin
        if LeftResolved.LoTypeEl<>nil then
          ResolvedEl:=LeftResolved
        else
          ResolvedEl:=RightResolved;
        ResolvedEl.IdentEl:=nil;
        ResolvedEl.ExprEl:=Bin;
        exit;
        end
      else if (RightResolved.BaseType=btContext)
          and (RightResolved.LoTypeEl.ClassType=TPasArrayType) then
        begin
        ResolvedEl:=RightResolved;
        ResolvedEl.IdentEl:=nil;
        ResolvedEl.ExprEl:=Bin;
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btModule then
    begin
    if Bin.OpCode=eopSubIdent then
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeBinaryExprRes OpCode=',OpcodeStrings[Bin.OpCode],' Kind=',Bin.Kind,' Left=',GetResolverResultDbg(LeftResolved),' Right=',GetResolverResultDbg(RightResolved));
  {$ENDIF}
  RaiseIncompatibleTypeRes(20180204114631,nOperatorIsNotOverloadedAOpB,
    [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
  if Flags=[] then ;
end;

procedure TPasResolver.ComputeArrayParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIndexProperty(Prop: TPasProperty);
  begin
    if [rcConstant,rcType]*Flags<>[] then
      RaiseConstantExprExp(20170216152635,Params);
    ComputeElement(GetPasPropertyType(Prop),ResolvedEl,[rcType],StartEl);
    ResolvedEl.IdentEl:=Prop;
    ResolvedEl.Flags:=[];
    if GetPasPropertyGetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfReadable);
    if GetPasPropertySetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfWritable);
  end;

  procedure ComputeArrayPointer(TypeEl: TPasType);
  begin
    if TypeEl=nil then
      RaiseInternalError(20180423092254);
    ComputeElement(TypeEl,ResolvedEl,[rcType],Params);
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.ExprEl:=Params;
    ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable];
  end;

var
  TypeEl: TPasType;
  ClassScope: TPasClassScope;
  ArrayEl: TPasArrayType;
  ArgNo: Integer;
  OrigResolved: TPasResolverResult;
  SubParams: TParamsExpr;
begin
  if Params.Value.CustomData is TResolvedReference then
    begin
    // e.g. Name[]
    ComputeElement(Params.Value,ResolvedEl,
      Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);
    end
  else if Params.Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Params.Value);
    if SubParams.Kind in [pekArrayParams,pekFuncParams] then
      begin
      // e.g. Name()[] or Name[][]
      ComputeElement(SubParams,ResolvedEl,
        Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);
      end
    else
      RaiseNotYetImplemented(20161010195646,SubParams);
    end
  else if Params.Value.ClassType=TUnaryExpr then
    begin
    ComputeElement(Params.Value,ResolvedEl,
      Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);
    end
  else
    RaiseNotYetImplemented(20160928174144,Params);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeArrayParams ResolvedEl=',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.BaseType in btAllStrings then
    begin
    // stringvar[] => char
    case GetActualBaseType(ResolvedEl.BaseType) of
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,btRawByteString,btShortString:
      if BaseTypeChar=btAnsiChar then
        ResolvedEl.BaseType:=btChar
      else
        ResolvedEl.BaseType:=btAnsiChar;
    {$endif}
    btWideString,btUnicodeString:
      if BaseTypeChar=btWideChar then
        ResolvedEl.BaseType:=btChar
      else
        ResolvedEl.BaseType:=btWideChar;
    else
      RaiseNotYetImplemented(20170417202354,Params);
    end;
    // keep ResolvedEl.IdentEl the string var
    ResolvedEl.LoTypeEl:=FBaseTypes[ResolvedEl.BaseType];
    ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
    ResolvedEl.ExprEl:=Params;
    ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable,rrfCanBeStatement]+[rrfAssignable];
    end
  else if ResolvedEl.BaseType=btPointer then
    // (@something)[]
    ComputeArrayPointer(ResolvedEl.LoTypeEl)
  else if (ResolvedEl.IdentEl is TPasProperty)
      and (GetPasPropertyArgs(TPasProperty(ResolvedEl.IdentEl)).Count>0) then
    // property with args
    ComputeIndexProperty(TPasProperty(ResolvedEl.IdentEl))
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl.ClassType=TPasClassType then
      begin
      ClassScope:=NoNil(TypeEl.CustomData) as TPasClassScope;
      if ClassScope.DefaultProperty<>nil then
        ComputeIndexProperty(ClassScope.DefaultProperty)
      else
        ComputeArrayParams_Class(Params,ResolvedEl,ClassScope,Flags,StartEl);
      end
    else if TypeEl.ClassType=TPasClassOfType then
      begin
      ClassScope:=ResolveAliasType(TPasClassOfType(TypeEl).DestType).CustomData as TPasClassScope;
      if ClassScope.DefaultProperty<>nil then
        ComputeIndexProperty(ClassScope.DefaultProperty)
      else
        RaiseInternalError(20161010174916);
      end
    else if TypeEl.ClassType=TPasArrayType then
      begin
      if not (rrfReadable in ResolvedEl.Flags) then
        RaiseMsg(20170517001140,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['[',TypeEl.ElementTypeName],Params);
      ArrayEl:=TPasArrayType(TypeEl);
      ArgNo:=0;
      repeat
        if length(ArrayEl.Ranges)=0 then
          begin
          inc(ArgNo); // dynamic/open array has one dimension
          if IsDynArray(ArrayEl) then
            Include(ResolvedEl.Flags,rrfWritable); // dynamic array elements are writable
          end
        else
          inc(ArgNo,length(ArrayEl.Ranges)); // static array has several dimensions
        if ArgNo>length(Params.Params) then
          RaiseInternalError(20161010185535);
        if ArgNo=length(Params.Params) then
          break;
        // continue in sub array
        ArrayEl:=NoNil(ResolveAliasType(ArrayEl.ElType)) as TPasArrayType;
      until false;
      OrigResolved:=ResolvedEl;
      ComputeElement(ArrayEl.ElType,ResolvedEl,Flags,StartEl);
      // identifier and value is the array itself
      ResolvedEl.IdentEl:=OrigResolved.IdentEl;
      ResolvedEl.ExprEl:=OrigResolved.ExprEl;
      ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
      if IsDynArray(ArrayEl) then
        // dyn array elements are writable independent of the array
        Include(ResolvedEl.Flags,rrfWritable);
      end
    else if TypeEl.ClassType=TPasPointerType then
      ComputeArrayPointer(TPasPointerType(TypeEl).DestType)
    else
      RaiseNotYetImplemented(20161010151727,Params,GetResolverResultDbg(ResolvedEl));
    end
  else
    RaiseNotYetImplemented(20160928174212,Params,GetResolverResultDbg(ResolvedEl));
end;

procedure TPasResolver.ComputeArrayParams_Class(Params: TParamsExpr;
  var ResolvedEl: TPasResolverResult; ClassScope: TPasClassScope;
  Flags: TPasResolverComputeFlags; StartEl: TPasElement);
begin
  RaiseInternalError(20161010174916);
  if Params=nil then ;
  if ClassScope=nil then ;
  if Flags=[] then ;
  if StartEl=nil then ;
  SetResolverIdentifier(ResolvedEl,btNone,nil,nil,nil,[]);
end;

procedure TPasResolver.ComputeFuncParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  DeclEl: TPasElement;
  BuiltInProc: TResElDataBuiltInProc;
  Proc: TPasProcedure;
  aClass: TPasClassType;
  ParamResolved: TPasResolverResult;
  Ref: TResolvedReference;
  DeclType: TPasType;
  Param0: TPasExpr;
begin
  if Params.Value.CustomData is TResolvedReference then
    begin
    Ref:=TResolvedReference(Params.Value.CustomData);
    DeclEl:=Ref.Declaration;
    if DeclEl.ClassType=TPasUnresolvedSymbolRef then
      begin
      if DeclEl.CustomData.ClassType=TResElDataBuiltInProc then
        begin
        BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
        if Assigned(BuiltInProc.GetCallResult) then
          // built-in function
          BuiltInProc.GetCallResult(BuiltInProc,Params,ResolvedEl)
        else
          // built-in procedure
          SetResolverIdentifier(ResolvedEl,btProc,BuiltInProc.Proc,
            BuiltInProc.Proc,BuiltInProc.Proc,[]);
        if bipfCanBeStatement in BuiltInProc.Flags then
          Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else if DeclEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        DeclType:=TPasUnresolvedSymbolRef(DeclEl);
        if length(Params.Params)<>1 then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeFuncParams DeclEl=',GetObjName(DeclEl));
          {$ENDIF}
          RaiseMsg(20180503105409,nWrongNumberOfParametersForTypeCast,
            sWrongNumberOfParametersForTypeCast,[DeclType.Name],Params);
          end;
        Param0:=Params.Params[0];
        ComputeElement(Param0,ParamResolved,[]);
        ComputeTypeCast(DeclType,DeclType,Param0,ParamResolved,ResolvedEl,Flags);
        end
      else
        RaiseNotYetImplemented(20161006133040,Params,GetResolverResultDbg(ResolvedEl));
      end
    else
      begin
      // normal identifier (not built-in)
      ComputeElement(DeclEl,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
      if ResolvedEl.BaseType=btProc then
        begin
        if not (ResolvedEl.IdentEl is TPasProcedure) then
          RaiseNotYetImplemented(20160928180201,Params,GetResolverResultDbg(ResolvedEl));
        Proc:=TPasProcedure(ResolvedEl.IdentEl);
        if rcConstant in Flags then
          RaiseConstantExprExp(20170216152637,Params);
        if Proc is TPasFunction then
          // function call => return result
          ComputeElement(TPasFunction(Proc).FuncType.ResultEl,ResolvedEl,
            Flags+[rcNoImplicitProc],StartEl)
        else if (Proc.ClassType=TPasConstructor)
            and (rrfNewInstance in Ref.Flags) then
          begin
          // new instance call -> return value of type class
          aClass:=GetReference_NewInstanceClass(Ref);
          SetResolverValueExpr(ResolvedEl,btContext,aClass,aClass,Params.Value,[rrfReadable]);
          end
        else
          // procedure call, result is neither readable nor writable
          SetResolverIdentifier(ResolvedEl,btProc,Proc,Proc.ProcType,Proc.ProcType,[]);
        Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else if ResolvedEl.LoTypeEl is TPasProcedureType then
        begin
        if Params.Value is TParamsExpr then
          begin
          // e.g. Name()() or Name[]()
          Include(ResolvedEl.Flags,rrfReadable);
          end;
        if rrfReadable in ResolvedEl.Flags then
          begin
          // call procvar
          if rcConstant in Flags then
            RaiseConstantExprExp(20170216152639,Params);
          if ResolvedEl.LoTypeEl is TPasFunctionType then
            // function call => return result
            ComputeElement(TPasFunctionType(ResolvedEl.LoTypeEl).ResultEl,
              ResolvedEl,Flags+[rcNoImplicitProc],StartEl)
          else
            // procedure call, result is neither readable nor writable
            SetResolverTypeExpr(ResolvedEl,btProc,
              ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,[]);
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end
        else
          begin
          // typecast to proctype
          if length(Params.Params)<>1 then
            begin
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.ComputeFuncParams DeclEl=',GetObjName(DeclEl),' ',GetResolverResultDbg(ResolvedEl));
            {$ENDIF}
            RaiseMsg(20170416185211,nWrongNumberOfParametersForTypeCast,
              sWrongNumberOfParametersForTypeCast,[ResolvedEl.LoTypeEl.Name],Params);
            end;
          Param0:=Params.Params[0];
          ComputeElement(Param0,ParamResolved,[]);
          ComputeTypeCast(ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,Param0,
                          ParamResolved,ResolvedEl,Flags);
          end;
        end
      else if (DeclEl is TPasType) then
        begin
        // type cast
        Param0:=Params.Params[0];
        ComputeElement(Param0,ParamResolved,[]);
        ComputeTypeCast(ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,Param0,
                        ParamResolved,ResolvedEl,Flags);
        end
      else
        RaiseNotYetImplemented(20160928180048,Params,GetResolverResultDbg(ResolvedEl));
      end;
    end
  else
    RaiseNotYetImplemented(20160928174124,Params);
end;

procedure TPasResolver.ComputeTypeCast(ToLoType, ToHiType: TPasType;
  Param: TPasExpr; const ParamResolved: TPasResolverResult; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);

  function ParamIsVar: boolean;
  var
    IdentEl: TPasElement;
  begin
    IdentEl:=ParamResolved.IdentEl;
    if IdentEl=nil then exit(false);
    if [rcConstant,rcType]*Flags<>[] then
      Result:=(IdentEl.ClassType=TPasConst) and (TPasConst(IdentEl).IsConst)
    else
      Result:=(IdentEl is TPasVariable)
           or (IdentEl.ClassType=TPasArgument)
           or (IdentEl.ClassType=TPasResultElement);
  end;

var
  WriteFlags: TPasResolverResultFlags;
  KeepWriteFlags: Boolean;
  bt: TResolverBaseType;
  Expr: TPasExpr;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeFuncParams START ToLoType=',GetObjName(ToLoType),' ',BoolToStr(ToLoType<>ToHiType,'ToHiType='+GetObjName(ToHiType),''),' ',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  if ToLoType.CustomData is TResElDataBaseType then
    begin
    // type cast to base type (or alias of base type)
    bt:=GetActualBaseType(TResElDataBaseType(ToLoType.CustomData).BaseType);
    SetResolverValueExpr(ResolvedEl,
      TResElDataBaseType(ToLoType.CustomData).BaseType,
      ToLoType,ToHiType,
      Param,[rrfReadable]);
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;

    WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable];
    if (WriteFlags<>[]) and ParamIsVar then
      begin
      KeepWriteFlags:=false;
      // Param is writable -> check if typecast keeps this
      if (bt=btPointer) then
        begin
        // typecast to pointer
        if (ParamResolved.BaseType=btPointer)
        or (ParamResolved.BaseType in [btString,btUnicodeString,btWideString])
        or (ParamResolved.LoTypeEl=nil) // untyped
        or (ParamResolved.LoTypeEl.ClassType=TPasClassType)
        or IsDynArray(ParamResolved.LoTypeEl)
        then
          // e.g. pointer(ObjVar)
          KeepWriteFlags:=true;
        end
      else if IsSameType(ToLoType,ParamResolved.LoTypeEl,prraNone) then
        // e.g. Byte(TAliasByte)
        KeepWriteFlags:=true;
      if KeepWriteFlags then
        ResolvedEl.Flags:=ResolvedEl.Flags+WriteFlags;
      end;
    end
  else if ToLoType is TPasProcedureType then
    begin
    // typecast to proctype
    if ParamIsVar then
      WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable]
    else
      WriteFlags:=[];
    SetResolverValueExpr(ResolvedEl,btContext,
      ToLoType,ToHiType,
      Param,[rrfReadable]+WriteFlags);
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;
    end
  else
    begin
    // typecast to custom type, e.g. to classtype, recordtype, arraytype, range, set
    if (Param.Parent is TParamsExpr) then
      Expr:=TParamsExpr(Param.Parent)
    else
      Expr:=Param;
    ComputeElement(ToHiType,ResolvedEl,Flags,Expr);
    ResolvedEl.ExprEl:=Expr;
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;
    ResolvedEl.Flags:=[rrfReadable];

    WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable];
    if (WriteFlags<>[]) and ParamIsVar then
      begin
      KeepWriteFlags:=false;
      if (rrfReadable in ResolvedEl.Flags) then
        begin
        // typecast a value
        if ParamResolved.BaseType=btPointer then
          begin
          if (ToLoType.ClassType=TPasClassType)
              or IsDynArray(ParamResolved.LoTypeEl) then
            // aClassType(aPointer)
            KeepWriteFlags:=true;
          end
        else if ParamResolved.LoTypeEl=nil then
          // e.g. TAliasType(untyped)
          KeepWriteFlags:=true
        else if ToLoType=ParamResolved.LoTypeEl then
          // e.g. TAliasType(ActualType)
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasClassType)
            and (ParamResolved.LoTypeEl.ClassType=TPasClassType) then
          begin
          // e.g. aClassType(ObjVar)
          if (TPasClassType(ToLoType).ObjKind<>TPasClassType(ParamResolved.LoTypeEl).ObjKind) then
            // e.g. IntfType(ObjVar)
          else
            KeepWriteFlags:=true;
          end
        else if (ToLoType.ClassType=TPasRecordType)
            and (ParamResolved.LoTypeEl.ClassType=TPasRecordType) then
          // typecast record
          KeepWriteFlags:=true;
        end
      else
        begin
        // typecast a type to a value, e.g. Pointer(TObject)
        end;
      if KeepWriteFlags then
        ResolvedEl.Flags:=ResolvedEl.Flags+WriteFlags;
      end;
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeFuncParams END ToLoType=',GetObjName(ToLoType),' ',BoolToStr(ToLoType<>ToHiType,'ToHiType='+GetObjName(ToHiType),''),' ',GetResolverResultDbg(ParamResolved),' Result=',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
end;

procedure TPasResolver.ComputeSetParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// [param,param,...]
var
  ParamResolved, FirstResolved: TPasResolverResult;
  i: Integer;
  Param: TPasExpr;
  IsRange, IsArray: Boolean;
  ArrayType: TPasArrayType;
begin
  ArrayType:=IsArrayExpr(Params);
  IsArray:=ArrayType<>nil;
  if length(Params.Params)=0 then
    begin
    SetResolverValueExpr(ResolvedEl,btArrayOrSet,nil,nil,Params,[rrfReadable]);
    if IsArray then
      ResolvedEl.BaseType:=btArrayLit;
    exit;
    end;
  FirstResolved:=Default(TPasResolverResult);
  Flags:=Flags-[rcNoImplicitProc]+[rcNoImplicitProcType];
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Params.Params[0],ParamResolved,Flags,StartEl);
    IsRange:=ParamResolved.BaseType=btRange;
    if IsRange then
      begin
      if IsArray then
        RaiseXExpectedButYFound(20180615111713,'array value','range expression',Param);
      ConvertRangeToElement(ParamResolved);
      end;
    if FirstResolved.BaseType=btNone then
      begin
      // first value -> check if type usable in a set/array
      FirstResolved:=ParamResolved;
      if IsRange then
        CheckIsOrdinal(FirstResolved,Param,true);
      if rrfReadable in FirstResolved.Flags then
        begin
        // has a value
        if (not IsArray) and (not IsRange)
            and (not CheckIsOrdinal(FirstResolved,Param,false)) then
          begin
          // can't be a set
          IsArray:=true;
          end;
        end
      else
        begin
        IsArray:=true;
        if (FirstResolved.BaseType=btContext) then
          begin
          if FirstResolved.IdentEl is TPasClassType then
            // array of classtypes
          else
            begin
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.ComputeSetParams ',GetResolverResultDbg(FirstResolved));
            {$ENDIF}
            RaiseXExpectedButYFound(20170420002328,'array value','type',Param);
            end;
          end
        else
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeSetParams ',GetResolverResultDbg(FirstResolved));
          {$ENDIF}
          RaiseXExpectedButYFound(20170420002332,'array value','type',Param);
          end;
        end;
      end
    else
      begin
      // next value
      CombineArrayLitElTypes(Params.Params[0],Param,FirstResolved,ParamResolved);
      end;
    end;

  FirstResolved.IdentEl:=nil;
  FirstResolved.ExprEl:=Params;
  FirstResolved.SubType:=FirstResolved.BaseType;
  if IsArray then
    FirstResolved.BaseType:=btArrayLit
  else
    FirstResolved.BaseType:=btArrayOrSet;
  FirstResolved.Flags:=[rrfReadable];
  ResolvedEl:=FirstResolved;
end;

procedure TPasResolver.ComputeDereference(El: TUnaryExpr;
  var ResolvedEl: TPasResolverResult);

  procedure Deref(TypeEl: TPasType);
  var
    Expr: TPasExpr;
  begin
    Expr:=ResolvedEl.ExprEl;
    if Expr=nil then
      Expr:=El;
    ComputeElement(TypeEl,ResolvedEl,[rcNoImplicitProc],El);
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.ExprEl:=Expr;
    ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable];
  end;

var
  TypeEl: TPasType;
begin
  if ResolvedEl.BaseType=btPointer then
    begin
    Deref(ResolvedEl.LoTypeEl);
    exit;
    end
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl.ClassType=TPasPointerType then
      begin
      Deref(TPasPointerType(TypeEl).DestType);
      exit;
      end;
    end;
  RaiseMsg(20180422191139,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
    [OpcodeStrings[eopDeref],GetResolverResultDescription(ResolvedEl)],El);
end;

procedure TPasResolver.ComputeArrayValuesExpectedType(El: TArrayValues; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// (expr, expr, ...)
var
  Parent: TPasElement;
  HiTypeEl, LoTypeEl: TPasType;
  Field: PRecordValuesItem;
  Ref: TResolvedReference;
  Member: TPasVariable;
  i: Integer;
  ArrType: TPasArrayType;
begin
  Parent:=El.Parent;
  if Parent is TPasVariable then
    begin
    HiTypeEl:=TPasVariable(Parent).VarType;
    if HiTypeEl=nil then
      RaiseMsg(20180429171628,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
        ['const','array values'],El);
    LoTypeEl:=ResolveAliasType(HiTypeEl);
    if LoTypeEl.ClassType=TPasArrayType then
      // ok
    else
      RaiseIncompatibleTypeDesc(20180429171714,nIncompatibleTypesGotExpected,
        [],'array value',GetTypeDescription(HiTypeEl),El);
    SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
       El,[rrfReadable]);
    end
  else if Parent.ClassType=TRecordValues then
    begin
    // record field array
    // get field
    i:=length(TRecordValues(Parent).Fields)-1;
    while (i>=0) and (TRecordValues(Parent).Fields[i].ValueExp<>El) do
      dec(i);
    if i<0 then
      RaiseInternalError(20180429181150);
    Field:=@TRecordValues(Parent).Fields[i];
    // get member
    Ref:=Field^.NameExp.CustomData as TResolvedReference;
    Member:=Ref.Declaration as TPasVariable;
    if Member=nil then
      RaiseInternalError(20180429181210);
    ComputeElement(Member,ResolvedEl,[],StartEl);
    ResolvedEl.Flags:=[rrfReadable];
    end
  else if Parent.ClassType=TArrayValues then
    begin
    // array of array
    ComputeArrayValuesExpectedType(TArrayValues(Parent),ResolvedEl,Flags,StartEl);
    if (ResolvedEl.BaseType=btContext)
        and (ResolvedEl.LoTypeEl.ClassType=TPasArrayType) then
      begin
      ArrType:=TPasArrayType(ResolvedEl.LoTypeEl);
      if length(ArrType.Ranges)>1 then
        RaiseNotYetImplemented(20180429180930,El);
      HiTypeEl:=ArrType.ElType;
      LoTypeEl:=ResolveAliasType(HiTypeEl);
      if LoTypeEl.ClassType<>TPasArrayType then
        RaiseIncompatibleTypeDesc(20180429180938,nIncompatibleTypesGotExpected,
          [],'array values',GetTypeDescription(HiTypeEl),El);
      SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
         El,[rrfReadable]);
      end
    else
      RaiseIncompatibleTypeDesc(20180429173143,nIncompatibleTypesGotExpected,
        [],'array values',GetTypeDescription(ResolvedEl),El);
    end
  else
    SetResolverValueExpr(ResolvedEl,btArrayLit,nil,nil,TArrayValues(El),[rrfReadable]);
end;

procedure TPasResolver.ComputeRecordValues(El: TRecordValues; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// (name:expr; name:expr; ...)
var
  Parent, Member: TPasElement;
  LoTypeEl, HiTypeEl: TPasType;
  i: Integer;
  Field: PRecordValuesItem;
  Ref: TResolvedReference;
  ArrType: TPasArrayType;
begin
  Parent:=El.Parent;
  if Parent is TPasVariable then
    begin
    HiTypeEl:=TPasVariable(Parent).VarType;
    if HiTypeEl=nil then
      RaiseMsg(20180429105451,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
        ['const','record values'],El);
    LoTypeEl:=ResolveAliasType(HiTypeEl);
    if LoTypeEl.ClassType<>TPasRecordType then
      RaiseIncompatibleTypeDesc(20180429104135,nIncompatibleTypesGotExpected,
        [],'record value',GetTypeDescription(HiTypeEl),El);
    SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
       El,[rrfReadable]);
    end
  else if Parent.ClassType=TRecordValues then
    begin
    // nested record
    // get field
    i:=length(TRecordValues(Parent).Fields)-1;
    while (i>=0) and (TRecordValues(Parent).Fields[i].ValueExp<>El) do
      dec(i);
    if i<0 then
      RaiseInternalError(20180429130244);
    Field:=@TRecordValues(Parent).Fields[i];
    // get member
    Ref:=Field^.NameExp.CustomData as TResolvedReference;
    Member:=Ref.Declaration as TPasVariable;
    if Member=nil then
      RaiseInternalError(20180429130548);
    ComputeElement(Member,ResolvedEl,[],StartEl);
    ResolvedEl.Flags:=[rrfReadable];
    end
  else if Parent.ClassType=TArrayValues then
    begin
    // array of record
    ComputeArrayValuesExpectedType(TArrayValues(Parent),ResolvedEl,Flags,StartEl);
    if (ResolvedEl.BaseType=btContext)
        and (ResolvedEl.LoTypeEl.ClassType=TPasArrayType) then
      begin
      ArrType:=TPasArrayType(ResolvedEl.LoTypeEl);
      if length(ArrType.Ranges)>1 then
        RaiseNotYetImplemented(20180429180450,El);
      HiTypeEl:=ArrType.ElType;
      LoTypeEl:=ResolveAliasType(HiTypeEl);
      if LoTypeEl.ClassType<>TPasRecordType then
        RaiseIncompatibleTypeDesc(20180429180642,nIncompatibleTypesGotExpected,
          [],'record values',GetTypeDescription(HiTypeEl),El);
      SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
         El,[rrfReadable]);
      end
    else
      RaiseIncompatibleTypeDesc(20180429173143,nIncompatibleTypesGotExpected,
        [],'array values',GetTypeDescription(ResolvedEl),El);
    end
  else
    RaiseMsg(20180429110227,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
     ['const','(name:'],El);
end;

procedure TPasResolver.CheckIsClass(El: TPasElement;
  const ResolvedEl: TPasResolverResult);
var
  TypeEl: TPasType;
begin
  if (ResolvedEl.BaseType<>btContext) then
    RaiseXExpectedButYFound(20170216152245,'class',BaseTypeNames[ResolvedEl.BaseType],El);
  TypeEl:=ResolvedEl.LoTypeEl;
  if (TypeEl.ClassType<>TPasClassType)
      or (TPasClassType(TypeEl).ObjKind<>okClass) then
    RaiseXExpectedButYFound(20170216152246,'class',GetElementTypeName(ResolvedEl.LoTypeEl),El);
end;

function TPasResolver.CheckTypeCastClassInstanceToClass(const FromClassRes,
  ToClassRes: TPasResolverResult; ErrorEl: TPasElement): integer;
// called when type casting a class instance into an unrelated class
begin
  if FromClassRes.BaseType=btNone then ;
  if ToClassRes.BaseType=btNone then ;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
end;

procedure TPasResolver.CheckSetLitElCompatible(Left, Right: TPasExpr;
  const LHS, RHS: TPasResolverResult);
var
  LBT, RBT: TResolverBaseType;
begin
  // check both are values
  if not (rrfReadable in LHS.Flags) then
    begin
    if LHS.LoTypeEl<>nil then
      RaiseXExpectedButYFound(20170216152645,'ordinal',GetElementTypeName(LHS.LoTypeEl),Left)
    else
      RaiseXExpectedButYFound(20170216152648,'ordinal',BaseTypeNames[LHS.BaseType],Left);
    end;
  if not (rrfReadable in RHS.Flags) then
    begin
    if RHS.LoTypeEl<>nil then
      RaiseXExpectedButYFound(20170216152651,'ordinal',GetElementTypeName(RHS.LoTypeEl),Right)
    else
      RaiseXExpectedButYFound(20170216152653,'ordinal',BaseTypeNames[RHS.BaseType],Right);
    end;
  // check both have the same ordinal type
  LBT:=GetActualBaseType(LHS.BaseType);
  RBT:=GetActualBaseType(RHS.BaseType);
  if LBT in btAllBooleans then
    begin
    if RBT in btAllBooleans then
      exit;
    RaiseXExpectedButYFound(20170216152656,'boolean',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT in btAllInteger then
    begin
    if RBT in btAllInteger then
      exit;
    RaiseXExpectedButYFound(20170216152658,'integer',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT in btAllChars then
    begin
    if RBT in btAllChars then
      exit;
    RaiseXExpectedButYFound(20170216152702,'char',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT=btContext then
    begin
    if LHS.LoTypeEl.ClassType=TPasEnumType then
      begin
      if LHS.LoTypeEl=RHS.LoTypeEl then
        exit;
      if RHS.LoTypeEl.ClassType<>TPasEnumType then
        RaiseXExpectedButYFound(20170216152707,LHS.LoTypeEl.Parent.Name,GetElementTypeName(RHS.LoTypeEl),Right);
      if LHS.LoTypeEl.Parent<>RHS.LoTypeEl.Parent then
        RaiseXExpectedButYFound(20170216152710,LHS.LoTypeEl.Parent.Name,RHS.LoTypeEl.Parent.Name,Right);
      end
    else
      RaiseXExpectedButYFound(20170216152712,'ordinal',BaseTypeNames[LHS.BaseType],Left);
    end
  else
    RaiseXExpectedButYFound(20170216152714,'ordinal',BaseTypeNames[LHS.BaseType],Left);
end;

function TPasResolver.CheckIsOrdinal(
  const ResolvedEl: TPasResolverResult; ErrorEl: TPasElement;
  RaiseOnError: boolean): boolean;
begin
  Result:=false;
  if ResolvedEl.BaseType in btAllRanges then
  else if (ResolvedEl.BaseType=btContext) then
    begin
    if ResolvedEl.LoTypeEl.ClassType=TPasEnumType then
    else if RaiseOnError then
      RaiseXExpectedButYFound(20170216152718,'ordinal value',GetElementTypeName(ResolvedEl.LoTypeEl),ErrorEl)
    else
      exit;
    end
  else if RaiseOnError then
    RaiseXExpectedButYFound(20170216152720,'ordinal value',BaseTypeNames[ResolvedEl.BaseType],ErrorEl)
  else
    exit;
  Result:=true;
end;

procedure TPasResolver.CombineArrayLitElTypes(Left, Right: TPasExpr;
  var LHS: TPasResolverResult; const RHS: TPasResolverResult);
// LHS defines the array element type
// check if RHS
var
  LBT, RBT: TResolverBaseType;
  C: TClass;
begin
  if (LHS.LoTypeEl=RHS.LoTypeEl) and (LHS.BaseType=RHS.BaseType) then
    exit; // exact same type

  LBT:=GetActualBaseType(LHS.BaseType);
  RBT:=GetActualBaseType(RHS.BaseType);
  if rrfReadable in LHS.Flags then
    begin
    if not (rrfReadable in RHS.Flags) then
      RaiseIncompatibleTypeRes(20170420004759,nIncompatibleTypesGotExpected,
        [],RHS,LHS,Right);
    // array of values
    if LBT in btAllBooleans then
      begin
      if RBT in btAllBooleans then
        begin
        LHS.BaseType:=GetCombinedBoolean(LBT,RBT,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093015,'boolean',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllInteger then
      begin
      if RBT in btAllInteger then
        begin
        LHS.BaseType:=GetCombinedInt(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093019,'integer',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllChars then
      begin
      if RBT in btAllChars then
        begin
        LHS.BaseType:=GetCombinedChar(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093024,'char',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllStrings then
      begin
      if RBT in btAllStringAndChars then
        begin
        LHS.BaseType:=GetCombinedString(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420102832,'string',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT=btNil then
      begin
      if RBT=btNil then
        exit
      else if RBT=btPointer then
        begin
        LHS:=RHS;
        exit;
        end
      else if RBT=btContext then
       begin
       C:=RHS.LoTypeEl.ClassType;
       if (C=TPasClassType)
           or (C=TPasClassOfType)
           or (C=TPasPointerType)
           or ((C=TPasArrayType) and IsDynArray(RHS.LoTypeEl))
           or (C=TPasProcedureType)
           or (C=TPasFunctionType) then
         begin
         LHS:=RHS;
         exit;
         end;
       end;
      end
    else if LBT=btContext then
      begin
      C:=LHS.LoTypeEl.ClassType;
      if C=TPasEnumType then
        begin
        if LHS.LoTypeEl=RHS.LoTypeEl then
          exit;
        end
      else if C=TPasClassType then
        begin
        // array of class instances
        if RHS.LoTypeEl.ClassType<>TPasClassType then
          RaiseIncompatibleTypeRes(20170420135637,nIncompatibleTypesGotExpected,
            [],RHS,LHS,Right);
        if CheckClassIsClass(LHS.LoTypeEl,RHS.LoTypeEl,Right)<cIncompatible then
          begin
          // right class type is a left class type -> ok
          exit;
          end
        else if CheckClassIsClass(RHS.LoTypeEl,LHS.LoTypeEl,Right)<cIncompatible then
          begin
          // left class type is a right class type -> right is the new base class type
          LHS:=RHS;
          exit;
          end;
        end;
      end;
    end
  else
    begin
    // array of types
    if rrfReadable in RHS.Flags then
      RaiseIncompatibleTypeRes(20170420004925,nIncompatibleTypesGotExpected,
        [],RHS,LHS,Right);
    if LBT=btContext then
      begin
      if LHS.LoTypeEl.ClassType=TPasClassType then
        begin
        // array of class type
        if RHS.LoTypeEl.ClassType<>TPasClassType then
          RaiseIncompatibleTypeRes(20170420091839,nIncompatibleTypesGotExpected,
            [],RHS,LHS,Right);
        if CheckClassIsClass(LHS.LoTypeEl,RHS.LoTypeEl,Right)<cIncompatible then
          begin
          // right class type is a left class type -> ok
          exit;
          end
        else if CheckClassIsClass(RHS.LoTypeEl,LHS.LoTypeEl,Right)<cIncompatible then
          begin
          // left class type is a right class type -> right is the new base class type
          LHS:=RHS;
          exit;
          end;
        end;
      end;
    end;

  // can't combine
  if LHS.LoTypeEl=nil then
    RaiseXExpectedButYFound(20170420004537,'array element',BaseTypeNames[LHS.BaseType],Left);
  if RHS.LoTypeEl=nil then
    RaiseXExpectedButYFound(20170420004602,'array element',BaseTypeNames[RHS.BaseType],Right);

  RaiseIncompatibleTypeRes(20170420092625,nIncompatibleTypesGotExpected,
    [],RHS,LHS,Right);
end;

procedure TPasResolver.ConvertRangeToElement(
  var ResolvedEl: TPasResolverResult);
var
  TypeEl: TPasType;
begin
  if ResolvedEl.BaseType<>btRange then
    RaiseInternalError(20161001155732);
  if ResolvedEl.LoTypeEl=nil then
    if ResolvedEl.IdentEl<>nil then
      RaiseNotYetImplemented(20161001155747,ResolvedEl.IdentEl)
    else
      RaiseNotYetImplemented(20161001155834,ResolvedEl.ExprEl);
  TypeEl:=ResolvedEl.LoTypeEl;
  if TypeEl is TPasRangeType then
    ComputeElement(TPasRangeType(TypeEl).RangeExpr.left,ResolvedEl,[rcConstant])
  else
    begin
    ResolvedEl.BaseType:=ResolvedEl.SubType;
    ResolvedEl.SubType:=btNone;
    end;
end;

function TPasResolver.IsCharLiteral(const Value: string; ErrorPos: TPasElement
  ): TResolverBaseType;
// returns true if Value is a Pascal char literal
// btAnsiChar: #65, #$50, ^G, 'a'
// btWideChar: #10000, 'ä'
var
  i: SizeInt;
  p, base, l: Integer;
begin
  Result:=btNone;
  //writeln('TPasResolver.IsCharLiteral ',BaseTypeChar,' "',Value,'" l=',length(Value));
  l:=length(Value);
  if l=0 then exit;
  p:=1;
  case Value[1] of
  '''':
    begin
    inc(p);
    if p>l then exit;
    {$ifdef FPC_HAS_CPSTRING}
    case Value[2] of
    '''':
      if Value='''''''''' then
        Result:=btAnsiChar; // ''''
    #32..#38,#40..#191:
      if (l=3) and (Value[3]='''') then
        Result:=btAnsiChar; // e.g. 'a'
    #192..#255:
      if BaseTypeChar=btWideChar then
        begin
        // default char is widechar: UTF-8 'ä' is a widechar
        i:=Utf8CodePointLen(@Value[2],4,false);
        //writeln('TPasResolver.IsCharLiteral "',Value,'" ',length(Value),' i=',i);
        if i<2 then
          exit;
        p:=2+i;
        if (p=l) and (Value[p]='''') then
          // single UTF-8 codepoint
          Result:=btWideChar;
        end;
    end;
    {$else}
    case Value[p] of
    '''':
      if (p+2=l) and (Value[p+1]='''') and (Value[p+2]='''') then
        Result:=btWideChar; // ''''
    #$DC00..#$DFFF: ;
    else
      if (l=3) and (Value[3]='''') then
        Result:=btWideChar; // e.g. 'a'
    end;
    {$endif}
    end;
  '#':
    begin
    inc(p);
    if p>l then exit;
    case Value[p] of
    '$': begin base:=16; inc(p); end;
    '&': begin base:=8; inc(p); end;
    '%': begin base:=2; inc(p); end;
    '0'..'9': base:=10;
    else RaiseNotYetImplemented(20170728142709,ErrorPos);
    end;
    i:=0;
    while p<=l do
      begin
      case Value[p] of
      '0'..'9': i:=i*base+ord(Value[p])-ord('0');
      'A'..'Z': i:=i*base+ord(Value[p])-ord('A')+10;
      'a'..'z': i:=i*base+ord(Value[p])-ord('a')+10;
      end;
      inc(p);
      end;
    if p>l then
      begin
      {$ifdef FPC_HAS_CPSTRING}
      if i<256 then
        Result:=btAnsiChar
      else
      {$endif}
        Result:=btWideChar;
      end;
    end;
  '^':
    begin
    if (l=2) and (Value[2] in ['a'..'z','A'..'Z']) then
      Result:={$ifdef FPC_HAS_CPSTRING}btAnsiChar{$else}btWideChar{$endif};
    end;
  end;
  if Result in [{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar] then
    begin
    if FBaseTypes[Result]=nil then
      begin
      {$ifdef FPC_HAS_CPSTRING}
      if Result=btAnsiChar then
        Result:=btWideChar
      else
      {$endif}
        Result:=btChar;
      end;
    if Result=BaseTypeChar then
      Result:=btChar;
    end;
end;

function TPasResolver.CheckForIn(Loop: TPasImplForLoop; const VarResolved,
  InResolved: TPasResolverResult): boolean;
begin
  Result:=false;
  if Loop=nil then ;
  if VarResolved.BaseType=btCustom then ;
  if InResolved.BaseType=btCustom then ;
end;

function TPasResolver.CheckForInClass(Loop: TPasImplForLoop; const VarResolved,
  InResolved: TPasResolverResult): boolean;
var
  TypeEl: TPasType;
  aClass: TPasClassType;
  ClassScope: TPasDotClassScope;
  Getter, MoveNext, Current: TPasIdentifier;
  GetterFunc, MoveNextFunc: TPasFunction;
  ptm: TProcTypeModifier;
  ResultResolved, MoveNextResolved, CurrentResolved: TPasResolverResult;
  CurrentProp: TPasProperty;
  ForScope: TPasForLoopScope;
begin
  Result:=false;
  TypeEl:=InResolved.LoTypeEl;
  if TypeEl is TPasClassType then
    begin
    if not (rrfReadable in InResolved.Flags) then
      RaiseMsg(20171221195421,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
        [GetBaseDescription(InResolved)],Loop.StartExpr);

    // check function GetEnumerator: class
    aClass:=TPasClassType(TypeEl);
    // find aClass.GetEnumerator
    ClassScope:=PushClassDotScope(aClass);
    Getter:=ClassScope.FindIdentifier('GetEnumerator');
    PopScope;
    if Getter=nil then
      RaiseIdentifierNotFound(20171221191511,'GetEnumerator',Loop.StartExpr);
    // check is function
    if Getter.Element.ClassType<>TPasFunction then
      RaiseContextXExpectedButYFound(20171221191638,'GetEnumerator','function',GetElementTypeName(Getter.Element),Loop.StartExpr);
    GetterFunc:=TPasFunction(Getter.Element);
    // check visibility
    if not (GetterFunc.Visibility in [visPublic,visPublished]) then
      RaiseContextXExpectedButYFound(20171221191824,'function GetEnumerator','public',VisibilityNames[GetterFunc.Visibility],Loop.StartExpr);
    // check arguments
    if GetterFunc.FuncType.Args.Count>0 then
      RaiseContextXExpectedButYFound(20171221191944,'function GetEnumerator','no arguments',IntToStr(GetterFunc.ProcType.Args.Count),Loop.StartExpr);
    // check proc type modifiers
    for ptm in GetterFunc.ProcType.Modifiers do
      if not (ptm in [ptmOfObject]) then
        RaiseContextXInvalidY(20171221193455,'function GetEnumerator','modifier '+ProcTypeModifiers[ptm],Loop.StartExpr);
    // check result type
    ComputeElement(GetterFunc.FuncType.ResultEl,ResultResolved,[rcType]);
    if (ResultResolved.BaseType<>btContext) then
      RaiseContextXExpectedButYFound(20171221193749,'function GetEnumerator','result class',GetTypeDescription(ResultResolved),Loop.StartExpr);
    TypeEl:=ResultResolved.LoTypeEl;
    if not (TypeEl is TPasClassType) then
      RaiseContextXExpectedButYFound(20171221193749,'function GetEnumerator','result class',GetTypeDescription(ResultResolved.LoTypeEl),Loop.StartExpr);
    if not (rrfReadable in ResultResolved.Flags) then
      RaiseContextXExpectedButYFound(20171221195506,'function GetEnumerator','result class instance',GetTypeDescription(ResultResolved.LoTypeEl),Loop.StartExpr);

    // check function MoveNext: boolean
    aClass:=TPasClassType(TypeEl);
    ClassScope:=PushClassDotScope(aClass);
    MoveNext:=ClassScope.FindIdentifier('MoveNext');
    if MoveNext=nil then
      RaiseIdentifierNotFound(20171221195632,'MoveNext',Loop.StartExpr);
    // check is function
    if MoveNext.Element.ClassType<>TPasFunction then
      RaiseContextXExpectedButYFound(20171221195651,'MoveNext','function',GetElementTypeName(MoveNext.Element),Loop.StartExpr);
    MoveNextFunc:=TPasFunction(MoveNext.Element);
    // check visibility
    if not (MoveNextFunc.Visibility in [visPublic,visPublished]) then
      RaiseContextXExpectedButYFound(20171221195712,'function MoveNext','public',VisibilityNames[MoveNextFunc.Visibility],Loop.StartExpr);
    // check arguments
    if MoveNextFunc.FuncType.Args.Count>0 then
      RaiseContextXExpectedButYFound(20171221195723,'function MoveNext','no arguments',IntToStr(MoveNextFunc.ProcType.Args.Count),Loop.StartExpr);
    // check proc type modifiers
    for ptm in MoveNextFunc.ProcType.Modifiers do
      if not (ptm in [ptmOfObject]) then
        RaiseContextXInvalidY(20171221195732,'function MoveNext','modifier '+ProcTypeModifiers[ptm],Loop.StartExpr);
    // check result type
    ComputeElement(MoveNextFunc.FuncType.ResultEl,MoveNextResolved,[rcType]);
    if not (MoveNextResolved.BaseType in btAllBooleans) then
      RaiseContextXExpectedButYFound(20171221200337,'function MoveNext','result boolean',GetTypeDescription(MoveNextResolved),Loop.StartExpr);

    // check property Current
    Current:=ClassScope.FindIdentifier('Current');
    if Current=nil then
      RaiseIdentifierNotFound(20171221200433,'Current',Loop.StartExpr);
    // check is property
    if Current.Element.ClassType<>TPasProperty then
      RaiseContextXExpectedButYFound(20171221200508,'Current','property',GetElementTypeName(Current.Element),Loop.StartExpr);
    CurrentProp:=TPasProperty(Current.Element);
    // check visibility
    if not (CurrentProp.Visibility in [visPublic,visPublished]) then
      RaiseContextXExpectedButYFound(20171221200546,'property Current','public',VisibilityNames[CurrentProp.Visibility],Loop.StartExpr);
    // check arguments
    if CurrentProp.Args.Count>0 then
      RaiseContextXExpectedButYFound(20171221200638,'property Current','no arguments',IntToStr(CurrentProp.Args.Count),Loop.StartExpr);
    // check readable
    if GetPasPropertyGetter(CurrentProp)=nil then
      RaiseContextXInvalidY(20171221200823,'property Current','read accessor',Loop.StartExpr);
    // check result type fits for-loop variable
    ComputeElement(CurrentProp,CurrentResolved,[rcType]);
    if CheckAssignResCompatibility(VarResolved,CurrentResolved,Loop.VariableName,false)=cIncompatible then
      RaiseIncompatibleTypeRes(20171221200018,nIncompatibleTypesGotExpected,[],VarResolved,CurrentResolved,Loop.VariableName);

    PopScope;

    ForScope:=Loop.CustomData as TPasForLoopScope;
    ForScope.GetEnumerator:=GetterFunc;
    ForScope.MoveNext:=MoveNextFunc;
    ForScope.Current:=CurrentProp;

    exit(true);
    end;

  RaiseMsg(20171221192929,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
    [GetBaseDescription(InResolved)],Loop.StartExpr);
end;

function TPasResolver.CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; MinCount: integer; RaiseOnError: boolean): boolean;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<MinCount) then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152248,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(false);
    end;
  Result:=true;
end;

function TPasResolver.CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; MaxCount: integer; RaiseOnError: boolean): integer;
begin
  if length(Params.Params)>MaxCount then
    begin
    if RaiseOnError then
      RaiseMsg(20170329154348,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[MaxCount]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.CheckRaiseTypeArgNo(id: TMaxPrecInt; ArgNo: integer;
  Param: TPasExpr; const ParamResolved: TPasResolverResult; Expected: string;
  RaiseOnError: boolean): integer;
begin
  if RaiseOnError then
    RaiseMsg(id,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
      [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved,true),Expected],Param);
  Result:=cIncompatible;
end;

function TPasResolver.FindUsedUnitInSection(const aName: string; Section: TPasSection): TPasModule;
var
  Clause: TPasUsesClause;
  i: Integer;
  Use: TPasUsesUnit;
  ModName: String;
begin
  Result:=nil;
  if (Section=nil) then exit;
  Clause:=Section.UsesClause;
  for i:=0 to length(Clause)-1 do
    begin
    Use:=Clause[i];
    if (Use.Module=nil) or not (Use.Module is TPasModule) then continue;
    ModName:=Use.Module.Name;
    if CompareText(ModName,aName)=0 then
      exit(TPasModule(Use.Module));
    end;
end;

function TPasResolver.FindUsedUnit(const aName: string; aMod: TPasModule): TPasModule;
var
  C: TClass;
begin
  C:=aMod.ClassType;
  if C.InheritsFrom(TPasProgram) then
    Result:=FindUsedUnitInSection(aName,TPasProgram(aMod).ProgramSection)
  else if C.InheritsFrom(TPasLibrary) then
    Result:=FindUsedUnitInSection(aName,TPasLibrary(aMod).LibrarySection)
  else
    begin
    Result:=FindUsedUnitInSection(aName,aMod.InterfaceSection);
    if Result<>nil then exit;
    Result:=FindUsedUnitInSection(aName,aMod.ImplementationSection);
    end
end;

procedure TPasResolver.FinishAssertCall(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  aConstructor: TPasConstructor;
begin
  if Proc=nil then ;
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if not (pmsfAssertSearched in ModScope.Flags) then
    FindAssertExceptionConstructors(Params);
  if ModScope.AssertClass=nil then exit;
  if length(Params.Params)>1 then
    aConstructor:=ModScope.AssertMsgConstructor
  else
    aConstructor:=ModScope.AssertDefConstructor;
  if aConstructor=nil then exit;
  CreateReference(aConstructor,Params,rraRead);
end;

function TPasResolver.FindExceptionConstructor(const aUnitName,
  aClassName: string; out aClass: TPasClassType; out
  aConstructor: TPasConstructor; ErrorEl: TPasElement): boolean;
var
  aMod, UtilsMod: TPasModule;
  SectionScope: TPasSectionScope;
  Identifier: TPasIdentifier;
  El: TPasElement;
  ClassScope: TPasClassScope;
begin
  Result:=false;
  aClass:=nil;
  aConstructor:=nil;

  // find unit in uses clauses
  aMod:=RootElement;
  UtilsMod:=FindUsedUnit(aUnitName,aMod);
  if UtilsMod=nil then exit;

  // find class in interface
  if UtilsMod.InterfaceSection=nil then exit;
  SectionScope:=NoNil(UtilsMod.InterfaceSection.CustomData) as TPasSectionScope;
  Identifier:=SectionScope.FindLocalIdentifier(aClassName);
  if Identifier=nil then exit;
  El:=Identifier.Element;
  if not (El is TPasClassType) then
    RaiseXExpectedButYFound(20180119172517,'class '+aClassName,GetElementTypeName(El),ErrorEl);
  if TPasClassType(El).ObjKind<>okClass then
    RaiseXExpectedButYFound(20180321163200,'class '+aClassName,GetElementTypeName(El),ErrorEl);
  aClass:=TPasClassType(El);

  ClassScope:=NoNil(aClass.CustomData) as TPasClassScope;
  repeat
    Identifier:=ClassScope.FindIdentifier('create');
    while Identifier<>nil do
      begin
      if Identifier.Element.ClassType=TPasConstructor then
        begin
        aConstructor:=TPasConstructor(Identifier.Element);
        if aConstructor.ProcType.Args.Count=0 then
          exit(true);
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    ClassScope:=ClassScope.AncestorScope;
  until ClassScope=nil;
  aConstructor:=nil;
end;

procedure TPasResolver.FindAssertExceptionConstructors(ErrorEl: TPasElement);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  Identifier: TPasIdentifier;
  aClass: TPasClassType;
  ClassScope: TPasClassScope;
  aConstructor: TPasConstructor;
  Arg: TPasArgument;
  ArgResolved: TPasResolverResult;
begin
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if pmsfAssertSearched in ModScope.Flags then exit;
  Include(ModScope.Flags,pmsfAssertSearched);

  FindExceptionConstructor('sysutils','EAssertionFailed',aClass,aConstructor,ErrorEl);
  if aClass=nil then exit;
  ClassScope:=NoNil(aClass.CustomData) as TPasClassScope;
  ModScope.AssertClass:=aClass;
  repeat
    Identifier:=ClassScope.FindIdentifier('create');
    while Identifier<>nil do
      begin
      if Identifier.Element.ClassType=TPasConstructor then
        begin
        aConstructor:=TPasConstructor(Identifier.Element);
        //writeln('TPasResolver.FindAssertExceptionConstructors ',aConstructor.Name,' ',aConstructor.ProcType.Args.Count);
        if aConstructor.ProcType.Args.Count=0 then
          begin
          if ModScope.AssertDefConstructor=nil then
            ModScope.AssertDefConstructor:=aConstructor;
          end
        else if aConstructor.ProcType.Args.Count=1 then
          begin
          if ModScope.AssertMsgConstructor=nil then
            begin
            Arg:=TPasArgument(aConstructor.ProcType.Args[0]);
            //writeln('TPasResolver.FindAssertExceptionConstructors ',GetObjName(Arg.ArgType),' ',GetObjName(BaseTypes[BaseTypeString]));
            ComputeElement(Arg.ArgType,ArgResolved,[rcType]);
            if ArgResolved.BaseType in btAllStrings then
              ModScope.AssertMsgConstructor:=aConstructor;
            end;
          end;
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    ClassScope:=ClassScope.AncestorScope;
  until ClassScope=nil;
end;

procedure TPasResolver.FindRangeErrorConstructors(ErrorEl: TPasElement);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  aClass: TPasClassType;
  aConstructor: TPasConstructor;
begin
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if pmsfRangeErrorSearched in ModScope.Flags then exit;
  Include(ModScope.Flags,pmsfRangeErrorSearched);

  FindExceptionConstructor('sysutils','ERangeError',aClass,aConstructor,ErrorEl);
  ModScope.RangeErrorClass:=aClass;
  ModScope.RangeErrorConstructor:=aConstructor;
end;

procedure TPasResolver.OnExprEvalLog(Sender: TResExprEvaluator;
  const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  PosEl: TPasElement);
begin
  if MsgType<=mtError then
    RaiseMsg(id,MsgNumber,Fmt,Args,PosEl)
  else
    LogMsg(id,MsgType,MsgNumber,Fmt,Args,PosEl);
  if Sender=nil then ;
end;

function TPasResolver.OnExprEvalIdentifier(Sender: TResExprEvaluator;
  Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue;
var
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
  ResolvedType: TPasResolverResult;
  EnumValue: TPasEnumValue;
  EnumType: TPasEnumType;
  EvalFlags: TResEvalFlags;
begin
  Result:=nil;
  if not (Expr.CustomData is TResolvedReference) then
    RaiseNotYetImplemented(20170518203134,Expr);
  Ref:=TResolvedReference(Expr.CustomData);
  Decl:=Ref.Declaration;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.OnExprEvalIdentifier Value=',Expr.Value,' Decl=',GetObjName(Decl));
  {$ENDIF}
  C:=Decl.ClassType;
  if C=TPasConst then
    begin
    if (TPasConst(Decl).Expr<>nil)
        and (TPasConst(Decl).IsConst or (TPasConst(Decl).VarType=nil)) then
      begin
      if TPasConst(Decl).VarType<>nil then
        begin
        // typed const
        ComputeElement(TPasConst(Decl).VarType,ResolvedType,[rcType]);
        end
      else
        ResolvedType.BaseType:=btNone;
      EvalFlags:=Flags;
      if not (refConstExt in EvalFlags) then
        Include(EvalFlags,refConst);
      Result:=fExprEvaluator.Eval(TPasConst(Decl).Expr,EvalFlags);
      if Result<>nil then
        begin
        if (Result.Element<>nil) and (Result.Element<>TPasConst(Decl).Expr) then
          Result:=Result.Clone;
        Result.IdentEl:=Decl;
        if TPasConst(Decl).VarType<>nil then
          begin
          // typed const
          if Result.Kind=revkInt then
            case ResolvedType.BaseType of
            btByte: TResEvalInt(Result).Typed:=reitByte;
            btShortInt: TResEvalInt(Result).Typed:=reitShortInt;
            btWord: TResEvalInt(Result).Typed:=reitWord;
            btSmallInt: TResEvalInt(Result).Typed:=reitSmallInt;
            btUIntSingle: TResEvalInt(Result).Typed:=reitUIntSingle;
            btIntSingle: TResEvalInt(Result).Typed:=reitIntSingle;
            btLongWord: TResEvalInt(Result).Typed:=reitLongWord;
            btLongint: TResEvalInt(Result).Typed:=reitLongInt;
            btUIntDouble: TResEvalInt(Result).Typed:=reitUIntDouble;
            {$ifdef HasInt64}
            btIntDouble: TResEvalInt(Result).Typed:=reitIntDouble;
            btInt64: TResEvalInt(Result).Typed:=reitNone; // default
            {$else}
            btIntDouble: TResEvalInt(Result).Typed:=reitNone; // default
            {$endif}
            else
              ReleaseEvalValue(Result);
              RaiseNotYetImplemented(20170624181050,TPasConst(Decl).VarType);
            end;
          end;
        exit;
        end;
      end
    else if vmExternal in TPasConst(Decl).VarModifiers then
      begin
      Result:=TResEvalExternal.Create;
      Result.IdentEl:=Decl;
      exit;
      end;
    if refConst in Flags then
      begin
      ReleaseEvalValue(Result);
      RaiseConstantExprExp(20170518214928,Expr);
      end;
    end
  else if C=TPasEnumValue then
    begin
    EnumValue:=TPasEnumValue(Decl);
    EnumType:=EnumValue.Parent as TPasEnumType;
    Result:=TResEvalEnum.CreateValue(EnumType.Values.IndexOf(EnumValue),EnumValue);
    exit;
    end
  else if C.InheritsFrom(TPasType) then
    Result:=EvalTypeRange(TPasType(Decl),Flags);
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.OnExprEvalIdentifier END Result=',dbgs(Result),' refConst=',refConst in Flags,' refConstExt=',refConstExt in Flags);
  {$ENDIF}
  if (Result=nil) and ([refConst,refConstExt]*Flags<>[]) then
    RaiseConstantExprExp(20170518213616,Expr);
end;

function TPasResolver.OnExprEvalParams(Sender: TResExprEvaluator;
  Params: TParamsExpr; Flags: TResEvalFlags): TResEvalValue;
var
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
  BuiltInProc: TResElDataBuiltInProc;
  bt: TResolverBaseType;
  ResolvedEl: TPasResolverResult;
  TypeEl: TPasType;
begin
  Result:=nil;
  case Params.Kind of
  pekArrayParams: ;
  pekFuncParams:
    if Params.Value.CustomData is TResolvedReference then
      begin
      Ref:=TResolvedReference(Params.Value.CustomData);
      Decl:=Ref.Declaration;
      if Decl is TPasType then
        Decl:=ResolveAliasType(TPasType(Decl));
      C:=Decl.ClassType;

      if C=TPasUnresolvedSymbolRef then
        begin
        if Decl.CustomData is TResElDataBuiltInProc then
          begin
          BuiltInProc:=TResElDataBuiltInProc(Decl.CustomData);
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.OnExprEvalParams Calling BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
          {$ENDIF}
          case BuiltInProc.BuiltIn of
            bfLength: BI_Length_OnEval(BuiltInProc,Params,Flags,Result);
            bfAssigned: Result:=nil;
            bfChr: BI_Chr_OnEval(BuiltInProc,Params,Flags,Result);
            bfOrd: BI_Ord_OnEval(BuiltInProc,Params,Flags,Result);
            bfLow,bfHigh: BI_LowHigh_OnEval(BuiltInProc,Params,Flags,Result);
            bfPred,bfSucc: BI_PredSucc_OnEval(BuiltInProc,Params,Flags,Result);
            bfStrFunc: BI_StrFunc_OnEval(BuiltInProc,Params,Flags,Result);
            bfConcatArray: Result:=nil;
            bfCopyArray: Result:=nil;
            bfTypeInfo: Result:=nil;
            bfDefault: BI_Default_OnEval(BuiltInProc,Params,Flags,Result);
          else
            {$IFDEF VerbosePasResEval}
            writeln('TPasResolver.OnExprEvalParams Unhandled BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
            {$ENDIF}
            RaiseNotYetImplemented(20170624192324,Params);
          end;
          {$IFDEF VerbosePasResEval}
          {AllowWriteln}
          if Result<>nil then
            writeln('TPasResolver.OnExprEvalParams Called BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn],' Result=',Result.AsString)
          else
            writeln('TPasResolver.OnExprEvalParams Called BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn],' Result=nil');
          {AllowWriteln-}
          {$ENDIF}
          exit;
          end
        else if Decl.CustomData is TResElDataBaseType then
          begin
          // typecast to basetype
          bt:=TResElDataBaseType(Decl.CustomData).BaseType;
          Result:=EvalBaseTypeCast(Params,bt);
          end;
        {$IFDEF VerbosePasResEval}
        writeln('TPasResolver.OnExprEvalParams BuiltInProc ',Decl.Name,' ',GetObjName(Decl.CustomData));
        {$ENDIF}
        end
      else if C=TPasEnumType then
        begin
        // typecast to enumtype
        Result:=fExprEvaluator.EnumTypeCast(TPasEnumType(Decl),Params.Params[0],Flags);
        end
      else if C=TPasRangeType then
        begin
        // typecast to custom range
        ComputeElement(TPasRangeType(Decl).RangeExpr.left,ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btContext then
          begin
          TypeEl:=ResolvedEl.LoTypeEl;
          if TypeEl.ClassType=TPasEnumType then
            begin
            // typecast to enumtype
            Result:=fExprEvaluator.EnumTypeCast(TPasEnumType(TypeEl),Params.Params[0],Flags);
            end
          else
            RaiseNotYetImplemented(20171009223403,Params);
          end
        else
          RaiseNotYetImplemented(20171009223303,Params);
        end;
      end;
  pekSet: ;
  end;
  if Flags=[] then ;
end;

procedure TPasResolver.OnRangeCheckEl(Sender: TResExprEvaluator;
  El: TPasElement; var MsgType: TMessageType);
begin
  if El=nil then exit;
  if (MsgType=mtWarning)
      and (bsRangeChecks in CurrentParser.Scanner.CurrentBoolSwitches) then
    MsgType:=mtError;
end;

function TPasResolver.EvalBaseTypeCast(Params: TParamsExpr;
  bt: TResolverBaseType): TResEvalvalue;

  procedure TCFloatToInt(Value: TResEvalValue; Flo: TMaxPrecFloat);
  var
    Int, MinIntVal, MaxIntVal: TMaxPrecInt;
  begin
    if bt in btAllIntegerNoQWord then
      begin
      // float to int
      GetIntegerRange(bt,MinIntVal,MaxIntVal);
      if (Flo<MinIntVal) or (Flo>MaxIntVal) then
        fExprEvaluator.EmitRangeCheckConst(20170711001228,
          Value.AsString,MinIntVal,MaxIntVal,Params,mtError);
      {$R-}
      try
        Int:=Round(Flo);
      except
        RaiseMsg(20170711002218,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      case bt of
        btByte: Result:=TResEvalInt.CreateValue(Int,reitByte);
        btShortInt: Result:=TResEvalInt.CreateValue(Int,reitShortInt);
        btWord: Result:=TResEvalInt.CreateValue(Int,reitWord);
        btSmallInt: Result:=TResEvalInt.CreateValue(Int,reitSmallInt);
        btUIntSingle: Result:=TResEvalInt.CreateValue(Int,reitUIntSingle);
        btIntSingle: Result:=TResEvalInt.CreateValue(Int,reitIntSingle);
        btLongWord: Result:=TResEvalInt.CreateValue(Int,reitLongWord);
        btLongint: Result:=TResEvalInt.CreateValue(Int,reitLongInt);
        btUIntDouble: Result:=TResEvalInt.CreateValue(Int,reitUIntDouble);
        {$ifdef HasInt64}
        btIntDouble: Result:=TResEvalInt.CreateValue(Int,reitIntDouble);
        btInt64: Result:=TResEvalInt.CreateValue(Int); // default
        {$else}
        btIntDouble: Result:=TResEvalInt.CreateValue(Int); // default
        {$endif}
      else
        RaiseNotYetImplemented(20170711001513,Params);
      end;
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      exit;
      end
    else if bt=btSingle then
      begin
      // float to single
      try
        Result:=TResEvalFloat.CreateValue({$ifdef pas2js}double{$else}single{$endif}(Flo));
      except
        RaiseMsg(20170711002315,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else if bt=btDouble then
      begin
      // float to double
      try
        Result:=TResEvalFloat.CreateValue(double(Flo));
      except
        RaiseMsg(20170711002327,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else if bt=btCurrency then
      begin
      // float to currency
      try
        Result:=TResEvalCurrency.CreateValue(Currency(Flo));
      except
        RaiseMsg(20180421171840,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else
      begin
      {$IFDEF VerbosePasResEval}
      writeln('TPasResolver.OnExprEvalParams typecast float to ',bt);
      {$ENDIF}
      RaiseNotYetImplemented(20170711002542,Params);
      end;
  end;

var
  Value: TResEvalValue;
  Int, MinIntVal, MaxIntVal: TMaxPrecInt;
  Flo: TMaxPrecFloat;
  w: WideChar;
begin
  Result:=nil;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.EvalBaseTypeCast bt=',bt);
  {$ENDIF}
  Value:=Eval(Params.Params[0],[refAutoConstExt]);
  if Value=nil then exit;
  try
    case Value.Kind of
    revkInt:
      begin
      Int:=TResEvalInt(Value).Int;
      {$ifdef HasInt64}
      if bt=btQWord then
        begin
        // int to qword
        {$R-}
        Result:=TResEvalUInt.CreateValue(TMaxPrecUInt(Int));
        {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
        end
      else
      {$endif}
      if bt in btAllIntegerNoQWord then
        begin
        // int to int
        GetIntegerRange(bt,MinIntVal,MaxIntVal);
        if (Int<MinIntVal) or (Int>MaxIntVal) then
          begin
          {$R-}
          case bt of
            btByte: Result:=TResEvalInt.CreateValue(byte(Int),reitByte);
            btShortInt: Result:=TResEvalInt.CreateValue(shortint(Int),reitShortInt);
            btWord: Result:=TResEvalInt.CreateValue(word(Int),reitWord);
            btSmallInt: Result:=TResEvalInt.CreateValue(smallint(Int),reitSmallInt);
            btLongWord: Result:=TResEvalInt.CreateValue(longword(Int),reitLongWord);
            btLongint: Result:=TResEvalInt.CreateValue(longint(Int),reitLongInt);
            {$ifdef HasInt64}
            btInt64: Result:=TResEvalInt.CreateValue(Int);
            {$endif}
            btUIntSingle,
            btIntSingle,
            btUIntDouble,
            btIntDouble:
              fExprEvaluator.EmitRangeCheckConst(20170624194534,
                Value.AsString,MinIntVal,MaxIntVal,Params,mtError);
          else
            RaiseNotYetImplemented(20170624200109,Params);
          end;
          {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
          end
        else
          begin
          {$R-}
          case bt of
            btByte: Result:=TResEvalInt.CreateValue(Int,reitByte);
            btShortInt: Result:=TResEvalInt.CreateValue(Int,reitShortInt);
            btWord: Result:=TResEvalInt.CreateValue(Int,reitWord);
            btSmallInt: Result:=TResEvalInt.CreateValue(Int,reitSmallInt);
            btUIntSingle: Result:=TResEvalInt.CreateValue(Int,reitUIntSingle);
            btIntSingle: Result:=TResEvalInt.CreateValue(Int,reitIntSingle);
            btLongWord: Result:=TResEvalInt.CreateValue(Int,reitLongWord);
            btLongint: Result:=TResEvalInt.CreateValue(Int,reitLongInt);
            btUIntDouble: Result:=TResEvalInt.CreateValue(Int,reitUIntDouble);
            {$ifdef HasInt64}
            btIntDouble: Result:=TResEvalInt.CreateValue(Int,reitIntDouble);
            btInt64: Result:=TResEvalInt.CreateValue(Int); // default
            {$else}
            btIntDouble: Result:=TResEvalInt.CreateValue(Int); // default
            {$endif}
          else
            RaiseNotYetImplemented(20170624200109,Params);
          end;
          {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
          end;
        exit;
        end
      else if bt in btAllBooleans then
        case Int of
        0: Result:=TResEvalBool.CreateValue(false);
        1: Result:=TResEvalBool.CreateValue(true);
        else
          fExprEvaluator.EmitRangeCheckConst(20170710203254,
            Value.AsString,0,1,Params,mtError);
        end
      {$ifdef FPC_HAS_CPSTRING}
      else if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btAnsiChar)) then
        try
          Result:=TResEvalString.CreateValue(Char(Int));
        except
          RaiseMsg(20180125112510,nRangeCheckError,sRangeCheckError,[],Params);
        end
      {$endif}
      else if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
        try
          w:=WideChar(Int);
          Result:=TResEvalUTF16.CreateValue(w);
        except
          RaiseMsg(20180125112716,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btSingle then
        try
          Result:=TResEvalFloat.CreateValue({$ifdef pas2js}double{$else}single{$endif}(Int));
        except
          RaiseMsg(20170711002015,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btDouble then
        try
          Result:=TResEvalFloat.CreateValue(Double(Int));
        except
          RaiseMsg(20170711002016,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btCurrency then
        try
          Result:=TResEvalCurrency.CreateValue(Currency(Int));
        except
          RaiseMsg(20180422093631,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else
        begin
        {$IFDEF VerbosePasResEval}
        writeln('TPasResolver.OnExprEvalParams typecast int to ',bt);
        {$ENDIF}
        RaiseNotYetImplemented(20170624194308,Params);
        end;
      end;
    revkFloat:
      begin
      Flo:=TResEvalFloat(Value).FloatValue;
      TCFloatToInt(Value,Flo);
      end;
    revkCurrency:
      begin
      if bt=btCurrency then
        begin
        Result:=Value;
        Value:=nil;
        end
      else
        begin
        Flo:=TResEvalCurrency(Value).Value;
        TCFloatToInt(Value,Flo);
        end;
      end;
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      begin
      if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
        begin
        if length(TResEvalString(Value).S)<>1 then
          RaiseXExpectedButYFound(20181005141025,'char','string',Params);
        Result:=Value;
        Value:=nil;
        end
      else if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
        begin
        if fExprEvaluator.GetWideChar(TResEvalString(Value).S,w) then
          begin
          Result:=Value;
          Value:=nil;
          end
        else
          RaiseXExpectedButYFound(20181005141058,'char','string',Params);
        end;
      end;
    {$endif}
    revkUnicodeString:
      if length(TResEvalUTF16(Value).S)=1 then
        begin
        w:=TResEvalUTF16(Value).S[1];
        {$ifdef FPC_HAS_CPSTRING}
        if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btAnsiChar)) then
          begin
          if ord(w)<=255 then
            begin
            Result:=Value;
            Value:=nil;
            end
          else
            RaiseMsg(20181005141632,nRangeCheckError,sRangeCheckError,[],Params);
          end
        else
        {$endif}
        if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
          begin
          Result:=Value;
          Value:=nil;
          end;
        end;
    revkExternal:
      exit;
    else
      {$IFDEF VerbosePasResEval}
      writeln('TPasResolver.OnExprEvalParams typecast to ',bt);
      {$ENDIF}
      RaiseNotYetImplemented(20170624193436,Params);
    end;
  finally
    ReleaseEvalValue(Value);
  end;
end;

function TPasResolver.CheckAssignCompatibilityCustom(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  var Handled: boolean): integer;
// called when LHS or RHS BaseType is btCustom
// if RaiseOnIncompatible=true you can raise an useful error.
begin
  Result:=cIncompatible;
  if LHS.BaseType=btNone then ;
  if RHS.BaseType=btNone then ;
  if ErrorEl=nil then ;
  if RaiseOnIncompatible then ;
  if Handled then ;
end;

function TPasResolver.CheckEqualCompatibilityCustomType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
begin
  Result:=cIncompatible;
  if LHS.BaseType=RHS.BaseType then;
  if ErrorEl=nil then;
  if RaiseOnIncompatible then ;
end;

function TPasResolver.BI_Length_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'length'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Ranges: TPasExprArray;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or dynamic array or type/const of static array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in btAllStringAndChars then
    begin
    if rrfReadable in ParamResolved.Flags then
      Result:=cExact;
    end
  else if ParamResolved.BaseType=btContext then
    begin
    if (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
      begin
      Ranges:=TPasArrayType(ParamResolved.LoTypeEl).Ranges;
      if length(Ranges)=0 then
        begin
        if rrfReadable in ParamResolved.Flags then
          Result:=cExact;
        end
      else
        // static array
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329160335,1,Param,ParamResolved,
      'string or dynamic array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Length_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Params=nil then ;
  SetResolverIdentifier(ResolvedEl,BaseTypeLength,Proc.Proc,
    FBaseTypes[BaseTypeLength],FBaseTypes[BaseTypeLength],[rrfReadable]);
end;

procedure TPasResolver.BI_Length_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param, Expr: TPasExpr;
  ParamResolved: TPasResolverResult;
  Value: TResEvalValue;
  Ranges: TPasExprArray;
  IdentEl: TPasElement;
begin
  Evaluated:=nil;
  // first param: string or dynamic array or type/const of static array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if ParamResolved.BaseType in btAllStringAndChars then
    begin
    if rrfReadable in ParamResolved.Flags then
      begin
      Value:=Eval(Param,Flags);
      if Value=nil then exit;
      case Value.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        Evaluated:=TResEvalInt.CreateValue(length(TResEvalString(Value).S));
      {$endif}
      revkUnicodeString:
        Evaluated:=TResEvalInt.CreateValue(length(TResEvalUTF16(Value).S));
      end;
      ReleaseEvalValue(Value);
      end
    end
  else if ParamResolved.BaseType=btContext then
    begin
    if (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
      begin
      Ranges:=TPasArrayType(ParamResolved.LoTypeEl).Ranges;
      if length(Ranges)=0 then
        begin
        // open or dynamic array
        IdentEl:=ParamResolved.IdentEl;
        if (IdentEl is TPasVariable)
            and (TPasVariable(IdentEl).Expr is TPasExpr) then
          begin
          Expr:=TPasVariable(IdentEl).Expr;
          if Expr is TArrayValues then
            Evaluated:=TResEvalInt.CreateValue(length(TArrayValues(Expr).Values))
          else if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
            Evaluated:=TResEvalInt.CreateValue(length(TParamsExpr(Expr).Params));
          end;
        end
      else
        begin
        // static array
        Evaluated:=TResEvalInt.CreateValue(GetRangeLength(Ranges[0]));
        end;
      end;
    end;
  if Proc=nil then ;
end;

function TPasResolver.BI_SetLength_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'setlength'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, DimResolved: TPasResolverResult;
  ArgNo: Integer;
  DynArr: TPasArrayType;
  ElType: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or array variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Result:=cIncompatible;
  DynArr:=nil;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact
    else if ParamResolved.BaseType=btContext then
      begin
      if IsDynArray(ParamResolved.LoTypeEl) then
        begin
        Result:=cExact;
        DynArr:=NoNil(ParamResolved.LoTypeEl) as TPasArrayType;
        end;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152250,1,Param,ParamResolved,
      'string or dynamic array variable',RaiseOnError));

  // second param: new length
  ArgNo:=2;
  repeat
    Param:=Params.Params[ArgNo-1];
    ComputeElement(Param,DimResolved,[]);
    Result:=cIncompatible;
    if (rrfReadable in DimResolved.Flags)
        and (DimResolved.BaseType in btAllInteger) then
      Result:=cExact;
    if Result=cIncompatible then
      exit(CheckRaiseTypeArgNo(20170329160338,ArgNo,Param,DimResolved,
        'integer',RaiseOnError));
    if (DynArr=nil) or (ArgNo=length(Params.Params)) then break;
    ElType:=ResolveAliasType(DynArr.ElType);
    if not IsDynArray(ElType) then break;
    DynArr:=NoNil(ElType) as TPasArrayType;
    inc(ArgNo);
  until false;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,ArgNo,RaiseOnError);
end;

procedure TPasResolver.BI_SetLength_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_InExclude_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'include'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  EnumType: TPasEnumType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: set variable
  // todo set of int, set of char, set of bool
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  EnumType:=nil;
  if ([rrfReadable,rrfWritable]*ParamResolved.Flags=[rrfReadable,rrfWritable])
      and ((ParamResolved.IdentEl is TPasVariable)
        or (ParamResolved.IdentEl is TPasArgument)
        or (ParamResolved.IdentEl is TPasResultElement)) then
    begin
    if (ParamResolved.BaseType=btSet)
        and (ParamResolved.LoTypeEl is TPasEnumType) then
      EnumType:=TPasEnumType(ParamResolved.LoTypeEl);
    end;
  if EnumType=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnGetCallCompatibility_InExclude ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    exit(CheckRaiseTypeArgNo(20170216152301,1,Param,ParamResolved,
      'variable of set of enumtype',RaiseOnError));
    end;

  // second param: enum
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if (not (rrfReadable in ParamResolved.Flags))
      or (ParamResolved.LoTypeEl<>EnumType) then
    begin
    if RaiseOnError then
      RaiseIncompatibleType(20170216152302,nIncompatibleTypeArgNo,
        ['2'],ParamResolved.LoTypeEl,EnumType,Param);
    exit(cIncompatible);
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_InExclude_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_Break_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152306,nMustBeInsideALoop,sMustBeInsideALoop,['Break'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Break Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Continue_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152309,nMustBeInsideALoop,sMustBeInsideALoop,['Continue'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Continue Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Exit_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResultResolved: TPasResolverResult;
  i: Integer;
  ProcScope: TPasProcedureScope;
  ResultEl: TPasResultElement;
  Flags: TPasResolverComputeFlags;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit Params=',length(Params.Params));
  {$ENDIF}

  // first param: result
  Param:=Params.Params[0];
  Result:=cIncompatible;
  i:=ScopeCount-1;
  while (i>0) and (not (Scopes[i] is TPasProcedureScope)) do dec(i);
  if i>0 then
    begin
    // first param is function result
    ProcScope:=TPasProcedureScope(Scopes[i]);
    if not (ProcScope.Element is TPasFunction) then
      begin
      if RaiseOnError then
        RaiseMsg(20170216152312,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,['procedure exit'],Params.Params[0]);
      exit(cIncompatible);
      end;
    ResultEl:=(ProcScope.Element as TPasFunction).FuncType.ResultEl;
    ComputeElement(ResultEl,ResultResolved,[rcType]);
    end
  else
    begin
    // default: main program, param is an integer
    SetResolverTypeExpr(ResultResolved,btLongint,FBaseTypes[btLongint],FBaseTypes[btLongint],
                        [rrfReadable,rrfWritable]);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ResultResolved=',GetResolverResultDbg(ResultResolved));
  {$ENDIF}

  Flags:=[];
  if IsProcedureType(ResultResolved,true) then
    Include(Flags,rcNoImplicitProc);
  ComputeElement(Param,ParamResolved,Flags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}

  if rrfReadable in ParamResolved.Flags then
    Result:=CheckAssignResCompatibility(ResultResolved,ParamResolved,Param,false);
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152314,nIncompatibleTypeArgNo,
        ['1'],ParamResolved,ResultResolved,Param);
    exit;
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

function TPasResolver.BI_IncDec_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, IncrResolved: TPasResolverResult;
  TypeEl: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var Integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_IncDec_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20170216152319,Expr,ParamResolved.IdentEl);
    exit;
    end;
  if ParamResolved.BaseType in btAllInteger then
    Result:=cExact
  else if ParamResolved.BaseType=btPointer then
    begin
    if bsPointerMath in GetElBoolSwitches(Expr) then
      Result:=cExact;
    end
  else if ParamResolved.BaseType=btContext then
    begin
    TypeEl:=ParamResolved.LoTypeEl;
    if (TypeEl.ClassType=TPasPointerType)
        and (bsPointerMath in GetElBoolSwitches(Expr)) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152320,1,Param,ParamResolved,'integer',RaiseOnError));

  if length(Params.Params)=1 then
    exit;

  // second param: increment/decrement
  Param:=Params.Params[1];
  ComputeElement(Param,IncrResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in IncrResolved.Flags then
    begin
    if IncrResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152322,2,Param,IncrResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_IncDec_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishCallArgAccess(P[0],rraVarParam);
  if Length(P)>1 then
    FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_Assigned_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Assigned'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  C: TClass;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: pointer, class, class instance, proc type or array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProcType]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in [btNil,btPointer] then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext) then
    begin
    C:=ParamResolved.LoTypeEl.ClassType;
    if (C=TPasClassType)
        or (C=TPasClassOfType)
        or C.InheritsFrom(TPasProcedureType)
        or ((C=TPasArrayType) and (length(TPasArrayType(ParamResolved.LoTypeEl).Ranges)=0)) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152329,1,Param,ParamResolved,'class or array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Assigned_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btBoolean,Proc.Proc,
                     FBaseTypes[btBoolean],FBaseTypes[btBoolean],[rrfReadable]);
end;

procedure TPasResolver.BI_Assigned_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExpr;
  ResolvedEl: TPasResolverResult;
begin
  if Proc=nil then ;
  P:=Params.Params[0];
  AccessExpr(P,rraRead);
  ComputeElement(P,ResolvedEl,[rcNoImplicitProcType,rcSetReferenceFlags]);
end;

function TPasResolver.BI_Chr_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170325185321,1,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,BaseTypeChar,Proc.Proc,
    FBaseTypes[BaseTypeChar],FBaseTypes[BaseTypeChar],[rrfReadable]);
end;

procedure TPasResolver.BI_Chr_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  Value: TResEvalValue;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  Value:=Eval(Param,Flags);
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Value=nil then
    writeln('TPasResolver.BI_Chr_OnEval Value=NIL')
  else
    writeln('TPasResolver.BI_Chr_OnEval Value=',Value.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
  if Value=nil then exit;
  try
    Evaluated:=fExprEvaluator.ChrValue(Value,Params);
  finally
    ReleaseEvalValue(Value);
  end;
  if Proc=nil then ;
end;

function TPasResolver.BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
  TypeEl: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: bool, enum or char
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in (btAllBooleans+btAllChars) then
      Result:=cExact
    else if (ParamResolved.BaseType=btContext) and (ParamResolved.LoTypeEl is TPasEnumType) then
      Result:=cExact
    else if ParamResolved.BaseType=btRange then
      begin
      if ParamResolved.SubType in btAllBooleans+btAllChars then
        Result:=cExact
      else if ParamResolved.SubType=btContext then
        begin
        TypeEl:=ParamResolved.LoTypeEl;
        if TypeEl.ClassType=TPasRangeType then
          begin
          ComputeElement(TPasRangeType(TypeEl).RangeExpr.left,ResolvedEl,[rcConstant]);
          if ResolvedEl.LoTypeEl.ClassType=TPasEnumType then
            exit(cExact);
          end;
        end;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152334,1,Param,ParamResolved,'enum or char',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btLongint,Proc.Proc,
    FBaseTypes[btLongint],FBaseTypes[btLongint],[rrfReadable]);
end;

procedure TPasResolver.BI_Ord_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  Value: TResEvalValue;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  Value:=Eval(Param,Flags);
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Value=nil then
    writeln('TPasResolver.BI_Ord_OnEval Value=NIL')
  else
    writeln('TPasResolver.BI_Ord_OnEval Value=',Value.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
  if Value=nil then exit;
  try
    Evaluated:=fExprEvaluator.OrdValue(Value,Params);
  finally
    ReleaseEvalValue(Value);
  end;
  if Proc=nil then ;
end;

function TPasResolver.BI_LowHigh_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Low' or 'High'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  C: TClass;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enumtype, range, built-in ordinal type (char, longint, ...)
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in btAllRanges then
    // e.g. high(char)
    Result:=cExact
  else if ParamResolved.BaseType=btSet then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext) then
    begin
    C:=ParamResolved.LoTypeEl.ClassType;
    if (C=TPasArrayType)
        or (C=TPasSetType)
        or (C=TPasEnumType) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_LowHigh_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    exit(CheckRaiseTypeArgNo(20170216152338,1,Param,ParamResolved,'ordinal type, array or set',RaiseOnError));
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  ArrayEl: TPasArrayType;
  Param: TPasExpr;
  TypeEl: TPasType;
begin
  Param:=Params.Params[0];
  ComputeElement(Param,ResolvedEl,[]);
  if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: result type is type of first dimension
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        SetResolverIdentifier(ResolvedEl,BaseTypeLength,Proc.Proc,
          FBaseTypes[BaseTypeLength],FBaseTypes[BaseTypeLength],[rrfReadable])
      else
        begin
        ComputeElement(ArrayEl.Ranges[0],ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btRange then
          ConvertRangeToElement(ResolvedEl);
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      begin
      ResolvedEl.LoTypeEl:=TPasSetType(TypeEl).EnumType;
      ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
      end;
    end
  else if ResolvedEl.BaseType=btSet then
    begin
    ResolvedEl.BaseType:=ResolvedEl.SubType;
    ResolvedEl.SubType:=btNone;
    end
  else
    ;// ordinal: result type is argument type
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable]+[rrfReadable];
end;

procedure TPasResolver.BI_LowHigh_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
var
  TypeEl: TPasType;
  ArrayEl: TPasArrayType;
  Value: TResEvalValue;
  EnumType: TPasEnumType;
  aSet: TResEvalSet;
  bt: TResolverBaseType;
  Int, MinInt, MaxInt: TMaxPrecInt;
  i: Integer;
  Expr: TPasExpr;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  TypeEl:=ParamResolved.LoTypeEl;
  if ParamResolved.BaseType=btContext then
    begin
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: low/high of first dimension
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        // dyn or open array
        if Proc.BuiltIn=bfLow then
          Evaluated:=TResEvalInt.CreateValue(0)
        else if (ParamResolved.IdentEl is TPasVariable)
            and (TPasVariable(ParamResolved.IdentEl).Expr is TPasExpr) then
          begin
          Expr:=TPasVariable(ParamResolved.IdentEl).Expr;
          if Expr is TArrayValues then
            Evaluated:=TResEvalInt.CreateValue(length(TArrayValues(Expr).Values)-1)
          else if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
            Evaluated:=TResEvalInt.CreateValue(length(TParamsExpr(Expr).Params)-1);
          if Evaluated=nil then
            RaiseXExpectedButYFound(20170601191003,'array constant','expression',Params);
          end
        else
          exit;
        end
      else
        begin
        // static array
        Evaluated:=EvalRangeLimit(ArrayEl.Ranges[0],Flags,Proc.BuiltIn=bfLow,Param);
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      begin
      // set: first/last enum
      TypeEl:=TPasSetType(TypeEl).EnumType;
      if TypeEl.ClassType=TPasEnumType then
        begin
        EnumType:=TPasEnumType(TPasSetType(TypeEl).EnumType);
        if Proc.BuiltIn=bfLow then
          Evaluated:=TResEvalEnum.CreateValue(0,TPasEnumValue(EnumType.Values[0]))
        else
          Evaluated:=TResEvalEnum.CreateValue(EnumType.Values.Count-1,
            TPasEnumValue(EnumType.Values[EnumType.Values.Count-1]));
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved),' TypeEl=',TypeEl.ClassName);
        {$ENDIF}
        RaiseNotYetImplemented(20170601203026,Params);
        end;
      end
    else if TypeEl.ClassType=TPasEnumType then
      begin
      EnumType:=TPasEnumType(TypeEl);
      if Proc.BuiltIn=bfLow then
        i:=0
      else
        i:=EnumType.Values.Count-1;
      Evaluated:=TResEvalEnum.CreateValue(i,TPasEnumValue(EnumType.Values[i]))
      end;
    end
  else if ParamResolved.BaseType=btSet then
    begin
    Value:=Eval(Param,Flags);
    if Value=nil then exit;
    case Value.Kind of
    revkSetOfInt:
      begin
      aSet:=TResEvalSet(Value);
      if length(aSet.Ranges)=0 then
        RaiseXExpectedButYFound(20170601201637,'ordinal value',Value.AsString,Param);
      if Proc.BuiltIn=bfLow then
        Int:=aSet.RangeStart
      else
        Int:=aSet.RangeEnd;
      case aSet.ElKind of
        revskEnum:
          begin
          EnumType:=aSet.IdentEl as TPasEnumType;
          Evaluated:=TResEvalEnum.CreateValue(Int,TPasEnumValue(EnumType.Values[Int]));
          end;
        revskInt:
          Evaluated:=TResEvalInt.CreateValue(Int);
        revskChar:
          {$ifdef FPC_HAS_CPSTRING}
          if Int<256 then
            Evaluated:=TResEvalString.CreateValue(chr(Int))
          else
          {$endif}
            Evaluated:=TResEvalUTF16.CreateValue(widechar(Int));
        revskBool:
          if Int=0 then
            Evaluated:=TResEvalBool.CreateValue(false)
          else
            Evaluated:=TResEvalBool.CreateValue(true)
      end;
      end;
    else
      RaiseXExpectedButYFound(20170601201237,'ordinal value',Value.AsString,Param);
    end;
    end
  else if (TypeEl is TPasUnresolvedSymbolRef)
      and (TypeEl.CustomData is TResElDataBaseType) then
    begin
    // low,high(base type)
    bt:=TResElDataBaseType(TypeEl.CustomData).BaseType;
    bt:=GetActualBaseType(bt);
    if bt in btAllBooleans then
      Evaluated:=TResEvalBool.CreateValue(Proc.BuiltIn=bfHigh)
    {$ifdef HasInt64}
    else if bt=btQWord then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalInt.CreateValue(0)
      else
        Evaluated:=TResEvalUInt.CreateValue(High(QWord));
      end
    {$endif}
    else if (bt in btAllIntegerNoQWord) and GetIntegerRange(bt,MinInt,MaxInt) then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalInt.CreateValue(MinInt)
      else
        Evaluated:=TResEvalInt.CreateValue(MaxInt);
      end
    {$ifdef FPC_HAS_CPSTRING}
    else if bt=btAnsiChar then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalString.CreateValue(#0)
      else
        Evaluated:=TResEvalString.CreateValue(#255);
      end
    {$endif}
    else if bt=btWideChar then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalUTF16.CreateValue(#0)
      else
        Evaluated:=TResEvalUTF16.CreateValue(#$ffff);
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20170602070738,Params);
      end;
    end
  else if ParamResolved.LoTypeEl is TPasRangeType then
    begin
    // e.g. type t = 2..10;
    Evaluated:=EvalRangeLimit(TPasRangeType(TypeEl).RangeExpr,FLags,Proc.BuiltIn=bfLow,Param);
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseNotYetImplemented(20170601202353,Params);
    end;
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Evaluated=nil then
    writeln('TPasResolver.BI_LowHigh_OnEval END ResolvedEl=',GetResolverResultDbg(ParamResolved),' Evaluated NO SET')
  else
    writeln('TPasResolver.BI_LowHigh_OnEval END ResolvedEl=',GetResolverResultDbg(ParamResolved),' Evaluated=',Evaluated.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasResolver.BI_PredSucc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Pred' or 'Succ'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enum, range, set, char or integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if CheckIsOrdinal(ParamResolved,Param,false) then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152343,1,Param,ParamResolved,'ordinal',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_PredSucc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
end;

procedure TPasResolver.BI_PredSucc_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
begin
  //writeln('TPasResolver.BI_PredSucc_OnEval START');
  Evaluated:=nil;
  Param:=Params.Params[0];
  Evaluated:=Eval(Param,Flags);
  //writeln('TPasResolver.BI_PredSucc_OnEval Param=',Evaluated<>nil);
  if Evaluated=nil then exit;
  //writeln('TPasResolver.BI_PredSucc_OnEval Param=',Evaluated.AsString);
  if Evaluated.Element<>nil then
    Evaluated:=Evaluated.Clone;
  if Proc.BuiltIn=bfPred then
    fExprEvaluator.PredValue(Evaluated,Params)
  else
    fExprEvaluator.SuccValue(Evaluated,Params);
end;

function TPasResolver.BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
  const ParamResolved: TPasResolverResult; ArgNo: integer; RaiseOnError: boolean
  ): integer;

  function CheckFormat(FormatExpr: TPasExpr; Index: integer;
    const ParamResolved: TPasResolverResult): boolean;
  var
    ResolvedEl: TPasResolverResult;
    Ok: Boolean;
  begin
    if FormatExpr=nil then exit(true);
    Result:=false;
    Ok:=false;
    if ParamResolved.BaseType in btAllFloats then
      // floats supports value:Width:Precision
      Ok:=true
    else
      // all other only support value:Width
      Ok:=Index<2;
    if not Ok then
      begin
      if RaiseOnError then
        RaiseMsg(20170319222319,nIllegalExpression,sIllegalExpression,[],FormatExpr);
      exit;
      end;
    ComputeElement(FormatExpr,ResolvedEl,[]);
    if not (ResolvedEl.BaseType in btAllInteger) then
      begin
      if RaiseOnError then
        RaiseXExpectedButYFound(20170319221515,
          'integer',GetResolverResultDescription(ResolvedEl,true),FormatExpr);
      exit;
      end;
    if not (rrfReadable in ResolvedEl.Flags) then
      begin
      if RaiseOnError then
        RaiseMsg(20170319221755,nNotReadable,sNotReadable,[],FormatExpr);
      exit;
      end;
    Result:=true;
  end;

var
  TypeEl: TPasType;
begin
  Result:=cIncompatible;
  if ParamResolved.BaseType in (btAllInteger+btAllBooleans+btAllFloats) then
    Result:=cExact
  else if IsFunc and (ParamResolved.BaseType in btAllStringAndChars) then
    Result:=cExact
  else if ParamResolved.BaseType=btContext then
    begin
      TypeEl:=ParamResolved.LoTypeEl;
      if TypeEl.ClassType=TPasEnumType then
        Result:=cExact
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220517,ArgNo,Param,ParamResolved,'boolean, integer, enum value',RaiseOnError));
  if not CheckFormat(Param.format1,1,ParamResolved) then
    exit(cIncompatible);
  if not CheckFormat(Param.format2,2,ParamResolved) then
    exit(cIncompatible);
end;

function TPasResolver.BI_StrProc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Str'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if ParentNeedsExprResult(Params) then
    begin
    if RaiseOnError then
      RaiseMsg(20170326084331,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['procedure str','function str'],Params);
    exit(cIncompatible);
    end;

  // first param: boolean, integer, enum, class instance
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=BI_Str_CheckParam(false,Param,ParamResolved,1,RaiseOnError);
  if Result=cIncompatible then
    exit;

  // second parameter: string variable
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220806,1,Param,ParamResolved,'string variable',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraRead);
  FinishCallArgAccess(P[1],rraVarParam);
end;

function TPasResolver.BI_StrFunc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if not ParentNeedsExprResult(Params) then
    begin
    // not in an expression -> the 'procedure str' is needed, not the 'function str'
    if RaiseOnError then
      RaiseMsg(20170326084622,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['function str','procedure str'],Params);
    exit(cIncompatible);
    end;

  // param: string, boolean, integer, enum, class instance
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    Result:=BI_Str_CheckParam(true,Param,ParamResolved,i+1,RaiseOnError);
    if Result=cIncompatible then
    exit;
    end;

  Result:=cExact;
end;

procedure TPasResolver.BI_StrFunc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Params=nil then ;
  SetResolverIdentifier(ResolvedEl,btString,Proc.Proc,
    FBaseTypes[btString],FBaseTypes[btString],[rrfReadable]);
end;

procedure TPasResolver.BI_StrFunc_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
begin
  Evaluated:=fExprEvaluator.EvalStrFunc(Params,Flags);
end;

function TPasResolver.BI_WriteStrProc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Str'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first parameter: string variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180527190304,1,Param,ParamResolved,'string variable',RaiseOnError));

  // other parameters: boolean, integer, enum, class instance
  for i:=1 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    Result:=BI_Str_CheckParam(false,Param,ParamResolved,i,RaiseOnError);
    if Result=cIncompatible then
      exit;
    end;
end;

procedure TPasResolver.BI_WriteStrProc_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
  i: Integer;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraOutParam);
  for i:=0 to length(Params.Params)-1 do
    FinishCallArgAccess(P[i],rraRead);
end;

function TPasResolver.BI_ConcatArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ElTypeResolved, FirstElTypeResolved: TPasResolverResult;
  i: Integer;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  FirstElTypeResolved:=Default(TPasResolverResult);
  for i:=0 to length(Params.Params)-1 do
    begin
    // all params: array
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    ElTypeResolved:=default(TPasResolverResult);
    if rrfReadable in ParamResolved.Flags then
      begin
      if ParamResolved.BaseType=btContext then
        begin
        if IsDynArray(ParamResolved.LoTypeEl) then
          ComputeElement(TPasArrayType(ParamResolved.LoTypeEl).ElType,ElTypeResolved,[rcType]);
        end
      else if ParamResolved.BaseType in [btArrayLit,btArrayOrSet] then
        SetResolverValueExpr(ElTypeResolved,ParamResolved.SubType,
          ParamResolved.LoTypeEl,ParamResolved.HiTypeEl,Param,ParamResolved.Flags);
      end;
    if ElTypeResolved.BaseType=btNone then
      exit(CheckRaiseTypeArgNo(20170329181206,i+1,Param,ParamResolved,'dynamic array',RaiseOnError));
    Include(ElTypeResolved.Flags,rrfReadable);
    if i=0 then
      begin
      FirstElTypeResolved:=ElTypeResolved;
      Include(FirstElTypeResolved.Flags,rrfWritable);
      end
    else if CheckAssignResCompatibility(FirstElTypeResolved,ElTypeResolved,Param,RaiseOnError)=cIncompatible then
      exit(cIncompatible);
    end;
  Result:=cExact;
end;

procedure TPasResolver.BI_ConcatArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  ResolvedEl.ExprEl:=Params;
  ResolvedEl.IdentEl:=nil;
  if ResolvedEl.BaseType=btArrayOrSet then
    ResolvedEl.BaseType:=btArrayLit;
end;

function TPasResolver.BI_CopyArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // first param: array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType=btContext then
      begin
      if IsDynArray(ParamResolved.LoTypeEl) then
        Result:=cExact;
      end
    else if ParamResolved.BaseType in [btArrayLit,btArrayOrSet] then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329153951,1,Param,ParamResolved,'dynamic array',RaiseOnError));
  if length(Params.Params)=1 then
    exit(cExact);

  // check optional Start index
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164210,2,Param,ParamResolved,'integer',RaiseOnError));
  if length(Params.Params)=2 then
    exit(cExact);

  // check optional Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164329,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_CopyArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  ResolvedEl.ExprEl:=Params;
  ResolvedEl.IdentEl:=nil;
  if ResolvedEl.BaseType=btArrayOrSet then
    ResolvedEl.BaseType:=btArrayLit;
end;

function TPasResolver.BI_InsertArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Insert(Item,var Array,Index)
var
  Params: TParamsExpr;
  Param, ItemParam: TPasExpr;
  ItemResolved, ParamResolved, ElTypeResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Item
  ItemParam:=Params.Params[0];
  ComputeElement(ItemParam,ItemResolved,[]);
  if not (rrfReadable in ItemResolved.Flags) then
    exit(CheckRaiseTypeArgNo(20170329171400,1,ItemParam,ItemResolved,'value',RaiseOnError));

  // check Array
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20170329171514,Param,ParamResolved.IdentEl);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.LoTypeEl) then
    exit(CheckRaiseTypeArgNo(20170329172024,2,Param,ParamResolved,'dynamic array',RaiseOnError));
  ComputeElement(TPasArrayType(ParamResolved.LoTypeEl).ElType,ElTypeResolved,[rcType]);
  if CheckAssignResCompatibility(ElTypeResolved,ItemResolved,ItemParam,RaiseOnError)=cIncompatible then
    exit(cIncompatible);

  // check insert Index
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_InsertArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
  Param0, Param1: TPasExpr;
  ArrayResolved, ElTypeResolved: TPasResolverResult;
begin
  if Proc=nil then ;
  P:=Params.Params;
  Param0:=P[0];
  Param1:=P[1];
  FinishCallArgAccess(Param0,rraRead);
  FinishCallArgAccess(Param1,rraVarParam);
  FinishCallArgAccess(P[2],rraRead);
  if not (Param0 is TPrimitiveExpr) then
    begin
    // insert complex expression, e.g. insert([1],Arr,index)
    // -> mark array and set literals
    ComputeElement(Param1,ArrayResolved,[]);
    if (ArrayResolved.BaseType<>btContext)
        or not IsDynArray(ArrayResolved.LoTypeEl) then
      RaiseNotYetImplemented(20180622144039,Param1);
    ComputeElement(TPasArrayType(ArrayResolved.LoTypeEl).ElType,ElTypeResolved,[rcType]);
    if (ElTypeResolved.BaseType=btContext)
        and (ElTypeResolved.LoTypeEl.ClassType=TPasArrayType) then
      MarkArrayExprRecursive(Param0,TPasArrayType(ElTypeResolved.LoTypeEl));
    end;
end;

function TPasResolver.BI_DeleteArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Delete(var Array; Start, Count: integer)
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20170329173421,Param,ParamResolved.IdentEl);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.LoTypeEl) then
    exit(CheckRaiseTypeArgNo(20170329173434,1,Param,ParamResolved,'dynamic array',RaiseOnError));

  // check param Start
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
     or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329173613,2,Param,ParamResolved,'integer',RaiseOnError));

  // check param Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_DeleteArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
  FinishCallArgAccess(P[2],rraRead);
end;

function TPasResolver.BI_TypeInfo_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  Decl: TPasElement;
  ParamResolved: TPasResolverResult;
  aType: TPasType;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check type or var
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Decl:=ParamResolved.IdentEl;
  aType:=nil;
  if (Decl<>nil) then
    begin
    if Decl is TPasType then
      aType:=TPasType(Decl)
    else if Decl is TPasVariable then
      aType:=TPasVariable(Decl).VarType
    else if Decl.ClassType=TPasArgument then
      aType:=TPasArgument(Decl).ArgType
    else if Decl.ClassType=TPasResultElement then
      aType:=TPasResultElement(Decl).ResultType
    else if Decl is TPasFunction then
      aType:=TPasFunction(Decl).FuncType.ResultEl.ResultType;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if aType=nil then
      writeln('TPasResolver.BI_TypeInfo_OnGetCallCompatibility Decl=',GetObjName(Decl));
    {AllowWriteln-}
    {$ENDIF}
    end;
  if aType=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_TypeInfo_OnGetCallCompatibility ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseMsg(20170411100259,nTypeIdentifierExpected,sTypeIdentifierExpected,[],Param);
    end;
  aType:=ResolveAliasType(aType);
  if not HasTypeInfo(aType) then
    RaiseMsg(20170413200118,nSymbolCannotBePublished,sSymbolCannotBePublished,[],Param);

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_TypeInfo_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Proc=nil then;
  if Params=nil then ;
  SetResolverTypeExpr(ResolvedEl,btPointer,
    FBaseTypes[btPointer],FBaseTypes[btPointer],[rrfReadable]);
end;

function TPasResolver.BI_Assert_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Assert'
//  Assert(bool)
//  Assert(bool,string)
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: boolean
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
     or not (ParamResolved.BaseType in btAllBooleans) then
    exit(CheckRaiseTypeArgNo(20180117123819,1,Param,ParamResolved,'boolean',RaiseOnError));

  // optional second parameter: string
  if length(Params.Params)>1 then
    begin
    Param:=Params.Params[1];
    ComputeElement(Param,ParamResolved,[]);
    if not (rrfReadable in ParamResolved.Flags)
       or not (ParamResolved.BaseType in btAllStringAndChars) then
      exit(CheckRaiseTypeArgNo(20180117123932,2,Param,ParamResolved,'string',RaiseOnError));
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_Assert_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  FinishAssertCall(Proc,Params);
end;

function TPasResolver.BI_New_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  TypeEl, SubTypeEl: TPasType;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var PRecord
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_New_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20180425005303,Expr,ParamResolved.IdentEl);
    exit;
    end;
  if ParamResolved.BaseType=btContext then
    begin
    TypeEl:=ParamResolved.LoTypeEl;
    if TypeEl.ClassType=TPasPointerType then
      begin
      SubTypeEl:=ResolveAliasType(TPasPointerType(TypeEl).DestType);
      if SubTypeEl.ClassType=TPasRecordType then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180425005421,1,Param,ParamResolved,'pointer of record',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_New_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  if Proc=nil then ;
  FinishCallArgAccess(Params.Params[0],rraOutParam);
end;

function TPasResolver.BI_Dispose_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  TypeEl, SubTypeEl: TPasType;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var PRecord
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_Dispose_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  if (rrfReadable in ParamResolved.Flags) then
    if ParamResolved.BaseType=btContext then
      begin
      TypeEl:=ParamResolved.LoTypeEl;
      if TypeEl.ClassType=TPasPointerType then
        begin
        SubTypeEl:=ResolveAliasType(TPasPointerType(TypeEl).DestType);
        if SubTypeEl.ClassType=TPasRecordType then
          Result:=cExact;
        end;
      end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180425010620,1,Param,ParamResolved,'pointer of record',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Dispose_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  if Proc=nil then ;
  FinishCallArgAccess(Params.Params[0],rraRead);
end;

function TPasResolver.BI_Default_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Decl: TPasElement;
  aType: TPasType;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check type or var
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Decl:=ParamResolved.IdentEl;
  aType:=nil;
  if (Decl<>nil) and (ParamResolved.LoTypeEl<>nil) then
    begin
    if Decl is TPasType then
      aType:=TPasType(Decl)
    else if Decl is TPasVariable then
      aType:=TPasVariable(Decl).VarType
    else if Decl.ClassType=TPasArgument then
      aType:=TPasArgument(Decl).ArgType;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if aType=nil then
      writeln('TPasResolver.BI_Default_OnGetCallCompatibility Decl=',GetObjName(Decl));
    {AllowWriteln-}
    {$ENDIF}
    end;
  if aType=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_Default_OnGetCallCompatibility ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseMsg(20180501004009,nTypeIdentifierExpected,sTypeIdentifierExpected,[],Param);
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Default_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  Param: TPasExpr;
begin
  Param:=Params.Params[0];
  ComputeElement(Param,ResolvedEl,[rcNoImplicitProc]);
  ResolvedEl.Flags:=[rrfReadable];
  ResolvedEl.IdentEl:=nil;
end;

procedure TPasResolver.BI_Default_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  TypeEl: TPasType;
  EnumType: TPasEnumType;
  i: Integer;
  ArrayEl: TPasArrayType;
  bt: TResolverBaseType;
  MinInt, MaxInt: TMaxPrecInt;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  TypeEl:=ParamResolved.LoTypeEl;
  if ParamResolved.BaseType=btContext then
    begin
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: []
      RaiseNotYetImplemented(20180501005214,Param);
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        // dyn or open array
        end
      else
        begin
        // static array
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      begin
      // set: first/last enum
      TypeEl:=TPasSetType(TypeEl).EnumType;
      if TypeEl.ClassType=TPasEnumType then
        begin
        EnumType:=TPasEnumType(TPasSetType(TypeEl).EnumType);
        Evaluated:=TResEvalSet.CreateEmpty(revskEnum,EnumType);
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved),' TypeEl=',TypeEl.ClassName);
        {$ENDIF}
        RaiseNotYetImplemented(20180501005348,Params);
        end;
      end
    else if TypeEl.ClassType=TPasEnumType then
      begin
      EnumType:=TPasEnumType(TypeEl);
      i:=0;
      Evaluated:=TResEvalEnum.CreateValue(i,TPasEnumValue(EnumType.Values[i]))
      end;
    end
  else if (TypeEl is TPasUnresolvedSymbolRef)
      and (TypeEl.CustomData is TResElDataBaseType) then
    begin
    // default(base type)
    bt:=TResElDataBaseType(TypeEl.CustomData).BaseType;
    bt:=GetActualBaseType(bt);
    if bt in btAllBooleans then
      Evaluated:=TResEvalBool.CreateValue(false)
    {$ifdef HasInt64}
    else if bt=btQWord then
      Evaluated:=TResEvalInt.CreateValue(0)
    {$endif}
    else if (bt in btAllIntegerNoQWord) and GetIntegerRange(bt,MinInt,MaxInt) then
      Evaluated:=TResEvalInt.CreateValue(MinInt)
    {$ifdef FPC_HAS_CPSTRING}
    else if bt in [btAnsiString,btShortString] then
      Evaluated:=TResEvalString.CreateValue('')
    {$endif}
    else if bt in [btUnicodeString,btWideString] then
      Evaluated:=TResEvalUTF16.CreateValue('')
    {$ifdef FPC_HAS_CPSTRING}
    else if bt=btAnsiChar then
      Evaluated:=TResEvalString.CreateValue(#0)
    {$endif}
    else if bt=btWideChar then
      Evaluated:=TResEvalUTF16.CreateValue(#0)
    else if bt in btAllFloats then
      Evaluated:=TResEvalFloat.CreateValue(0.0)
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20180501005645,Params);
      end;
    end
  else if ParamResolved.LoTypeEl is TPasRangeType then
    begin
    // e.g. type t = 2..10;
    Evaluated:=EvalRangeLimit(TPasRangeType(TypeEl).RangeExpr,FLags,true,Param);
    end
  else if ParamResolved.BaseType=btSet then
    begin
    if ParamResolved.SubType=btContext then
      begin
      if ParamResolved.LoTypeEl.ClassType=TPasEnumType then
        Evaluated:=TResEvalSet.CreateEmpty(revskEnum,TPasEnumType(ParamResolved.LoTypeEl))
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
        {$ENDIF}
        RaiseNotYetImplemented(20180501125138,Param);
        end;
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20180501125014,Param);
      end;
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseNotYetImplemented(20180501004839,Param);
    end;
end;

constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  FPendingForwardProcs:=TFPList.Create;
  FBaseTypeChar:={$ifdef FPC_HAS_CPSTRING}btAnsiChar{$else}btWideChar{$endif};
  FBaseTypeString:={$ifdef FPC_HAS_CPSTRING}btAnsiString{$else}btUnicodeString{$endif};
  FBaseTypeExtended:=btDouble;
  FBaseTypeLength:={$ifdef HasInt64}btInt64{$else}btIntDouble{$endif};
  FDynArrayMinIndex:=0;
  FDynArrayMaxIndex:=High(TMaxPrecInt);

  cTGUIDToString:=cTypeConversion+1;
  cStringToTGUID:=cTypeConversion+1;
  cInterfaceToTGUID:=cTypeConversion+1;
  cInterfaceToString:=cTypeConversion+2;

  FScopeClass_Class:=TPasClassScope;
  FScopeClass_InitialFinalization:=TPasInitialFinalizationScope;
  FScopeClass_Module:=TPasModuleScope;
  FScopeClass_Proc:=TPasProcedureScope;
  FScopeClass_Section:=TPasSectionScope;
  FScopeClass_WithExpr:=TPasWithExprScope;
  fExprEvaluator:=TResExprEvaluator.Create;
  fExprEvaluator.OnLog:=@OnExprEvalLog;
  fExprEvaluator.OnEvalIdentifier:=@OnExprEvalIdentifier;
  fExprEvaluator.OnEvalParams:=@OnExprEvalParams;
  fExprEvaluator.OnRangeCheckEl:=@OnRangeCheckEl;
  PushScope(FDefaultScope);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
var
  aScanner: TPascalScanner;
  SrcPos: TPasSourcePos;
begin
  // get source position for good error messages
  aScanner:=CurrentParser.Scanner;
  if (ASourceFilename='') or StoreSrcColumns then
    begin
    SrcPos.FileName:=aScanner.CurFilename;
    SrcPos.Row:=aScanner.CurRow;
    SrcPos.Column:=aScanner.CurColumn;
    end
  else
    begin
    SrcPos.FileName:=ASourceFilename;
    SrcPos.Row:=ASourceLinenumber;
    SrcPos.Column:=0;
    end;
  Result:=CreateElement(AClass,AName,AParent,AVisibility,SrcPos);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
var
  El: TPasElement;
  SrcY: integer;
  SectionScope: TPasSectionScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent),' (',ASrcPos.Row,',',ASrcPos.Column,')');
  {$ENDIF}
  if (AParent=nil) and (FRootElement<>nil) then
    RaiseInternalError(20160922163535,'more than one root element Class="'+AClass.ClassName+'" Root='+GetObjName(FRootElement));

  if ASrcPos.FileName='' then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent),' (',ASrcPos.Row,',',ASrcPos.Column,')');
    {$ENDIF}
    RaiseInternalError(20160922163541,'missing filename');
    end;
  SrcY:=ASrcPos.Row;
  if StoreSrcColumns then
    SrcY:=MangleSourceLineNumber(SrcY,ASrcPos.Column);

  // create element
  El:=AClass.Create(AName,AParent);
  {$IFDEF CheckPasTreeRefCount}El.RefIds.Add('CreateElement');{$ENDIF}
  FLastElement:=El;
  Result:=nil;
  try
    El.Visibility:=AVisibility;
    El.SourceFilename:=ASrcPos.FileName;
    El.SourceLinenumber:=SrcY;
    if FRootElement=nil then
      begin
      RootElement:=El as TPasModule;
      if FStep=prsInit then
        FStep:=prsParsing;
      end
    else if (AParent is TPasSection) and (TPasSection(AParent).Declarations.Count=0) then
      begin
      // first element of section
      SectionScope:=TPasSectionScope(AParent.CustomData);
      SectionScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
      SectionScope.ModeSwitches:=CurrentParser.Scanner.CurrentModeSwitches;
      end;

    if IsElementSkipped(El) then exit;

    // create scope
    if (AClass=TPasVariable)
        or (AClass=TPasConst) then
      AddVariable(TPasVariable(El))
    else if AClass=TPasResString then
      AddResourceString(TPasResString(El))
    else if (AClass=TPasProperty) then
      AddProperty(TPasProperty(El))
    else if AClass=TPasArgument then
      AddArgument(TPasArgument(El))
    else if AClass=TPasEnumType then
      AddEnumType(TPasEnumType(El))
    else if AClass=TPasEnumValue then
      AddEnumValue(TPasEnumValue(El))
    else if (AClass=TUnresolvedPendingRef) then
    else if (AClass=TPasAliasType)
        or (AClass=TPasTypeAliasType)
        or (AClass=TPasClassOfType)
        or (AClass=TPasPointerType)
        or (AClass=TPasArrayType)
        or (AClass=TPasProcedureType)
        or (AClass=TPasFunctionType)
        or (AClass=TPasSetType)
        or (AClass=TPasRangeType) then
      AddType(TPasType(El))
    else if AClass=TPasStringType then
      begin
      AddType(TPasType(El));
      {$ifdef FPC_HAS_CPSTRING}
      if BaseTypes[btShortString]=nil then
      {$endif}
        RaiseMsg(20170419203043,nIllegalQualifier,sIllegalQualifier,['['],El);
      end
    else if AClass=TPasRecordType then
      AddRecordType(TPasRecordType(El))
    else if AClass=TPasClassType then
      AddClassType(TPasClassType(El))
    else if AClass=TPasVariant then
    else if AClass.InheritsFrom(TPasProcedure) then
      AddProcedure(TPasProcedure(El))
    else if AClass=TPasResultElement then
      AddFunctionResult(TPasResultElement(El))
    else if AClass=TProcedureBody then
      AddProcedureBody(TProcedureBody(El))
    else if AClass=TPasMethodResolution then
    else if AClass=TPasImplExceptOn then
      AddExceptOn(TPasImplExceptOn(El))
    else if AClass=TPasImplLabelMark then
    else if AClass=TPasOverloadedProc then
    else if (AClass=TInterfaceSection)
        or (AClass=TImplementationSection)
        or (AClass=TProgramSection)
        or (AClass=TLibrarySection) then
      AddSection(TPasSection(El))
    else if (AClass=TPasModule)
        or (AClass=TPasProgram)
        or (AClass=TPasLibrary) then
      AddModule(TPasModule(El))
    else if AClass=TPasUsesUnit then
    else if AClass.InheritsFrom(TPasExpr) then
      // resolved when finished
    else if AClass=TInitializationSection then
      AddInitialFinalizationSection(TInitializationSection(El))
    else if AClass=TFinalizationSection then
      AddInitialFinalizationSection(TFinalizationSection(El))
    else if AClass.InheritsFrom(TPasImplBlock) then
      // resolved when finished
    else if AClass=TPasImplCommand then
    else if AClass=TPasUnresolvedUnitRef then
      RaiseMsg(20171018121900,nCantFindUnitX,sCantFindUnitX,[AName],El)
    else
      RaiseNotYetImplemented(20160922163544,El);
    Result:=El;
  finally
    if Result=nil then
      El.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasResolver.FindModule(const AName: String; NameExpr,
  InFileExpr: TPasExpr): TPasModule;
var
  InFilename, FileUnitName: String;
begin
  if InFileExpr<>nil then
    begin
    InFilename:=GetUsesUnitInFilename(InFileExpr);
    if InFilename='' then
      RaiseXExpectedButYFound(20180222001220,
               'file path','empty string',InFileExpr);
    if msDelphi in CurrentParser.CurrentModeswitches then
      begin
      // in delphi the last unit name must match the filename
      FileUnitName:=ChangeFileExt(ExtractFileName(InFilename),'');
      if CompareText(AName,FileUnitName)<>0 then
        RaiseXExpectedButYFound(20180222230400,AName,FileUnitName,InFileExpr);
      end;
    end;
  Result:=FindUnit(AName,InFilename,NameExpr,InFileExpr);
  if Result=nil then
    begin
    if InFileExpr<>nil then
      RaiseMsg(20180223140434,nCantFindUnitX,sCantFindUnitX,[InFilename],InFileExpr)
    else
      RaiseMsg(20180223140409,nCantFindUnitX,sCantFindUnitX,[AName],NameExpr);
    end;
end;

function TPasResolver.FindElement(const aName: String): TPasElement;
// called by TPasParser for direct types, e.g. type t = ns1.unit1.tobj.tsub
var
  p: SizeInt;
  RightPath, CurName, LeftPath: String;
  NeedPop: Boolean;
  CurScopeEl, NextEl, ErrorEl, BestEl: TPasElement;
  CurSection: TPasSection;
  i: Integer;
  UsesUnit: TPasUsesUnit;
begin
  Result:=nil;
  //writeln('TPasResolver.FindElement Name="',aName,'"');
  ErrorEl:=nil; // use nil to use scanner position as error position

  RightPath:=aName;
  LeftPath:='';
  p:=1;
  CurScopeEl:=nil;
  repeat
    p:=Pos('.',RightPath);
    if p<1 then
      begin
      CurName:=RightPath;
      RightPath:='';
      end
    else
      begin
      CurName:=LeftStr(RightPath,p-1);
      Delete(RightPath,1,p);
      if RightPath='' then
        RaiseMsg(20170328003146,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      end;
    if LeftPath='' then
      LeftPath:=CurName
    else
      LeftPath:=LeftPath+'.'+CurName;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if RightPath<>'' then
      writeln('TPasResolver.FindElement searching scope "',CurName,'" RightPath="',RightPath,'" ...');
    {AllowWriteln-}
    {$ENDIF}
    if not IsValidIdent(CurName) then
      RaiseNotYetImplemented(20170328000033,ErrorEl);
    if CurScopeEl<>nil then
      begin
      NeedPop:=true;
      if CurScopeEl.ClassType=TPasClassType then
        PushClassDotScope(TPasClassType(CurScopeEl))
      else if CurScopeEl.ClassType=TPasRecordType then
        PushRecordDotScope(TPasRecordType(CurScopeEl))
      else if CurScopeEl is TPasModule then
        PushModuleDotScope(TPasModule(CurScopeEl))
      else
        RaiseMsg(20170504174021,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['.',LeftPath],ErrorEl);
      end
    else
      NeedPop:=false;

    NextEl:=FindElementWithoutParams(CurName,ErrorEl,true);
    {$IFDEF VerbosePasResolver}
    //if RightPath<>'' then
    //  writeln('TPasResolver.FindElement searching scope "',CurName,'" RightPath="',RightPath,'" ... NextEl=',GetObjName(NextEl));
    {$ENDIF}
    if NextEl is TPasModule then
      begin
      if CurScopeEl is TPasModule then
        RaiseXExpectedButYFound(20170328001619,'class',GetElementTypeName(NextEl)+' '+NextEl.Name,ErrorEl);
      if Pos('.',NextEl.Name)>0 then
        begin
        // dotted module name -> check if the full module name is in aName
        if CompareText(NextEl.Name+'.',LeftStr(aName,length(NextEl.Name)+1))<>0 then
          begin
          if CompareText(NextEl.Name,aName)=0 then
            RaiseXExpectedButYFound(20170504165825,'type',GetElementTypeName(NextEl),ErrorEl)
          else
            RaiseIdentifierNotFound(20170504165412,aName,ErrorEl);
          end;
        RightPath:=copy(aName,length(NextEl.Name)+2,length(aName));
        end;
      CurScopeEl:=NextEl;
      end
    else if NextEl.ClassType=TPasUsesUnit then
      begin
      // the first name of a used unit matches -> find longest match
      CurSection:=NextEl.Parent as TPasSection;
      i:=length(CurSection.UsesClause)-1;
      BestEl:=nil;
      while i>=0 do
        begin
        UsesUnit:=CurSection.UsesClause[i];
        CurName:=UsesUnit.Name;
        if IsDottedIdentifierPrefix(CurName,aName)
            and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
          BestEl:=UsesUnit;
        dec(i);
        if (i<0) and (CurSection.ClassType=TImplementationSection) then
          begin
          CurSection:=(CurSection.Parent as TPasModule).InterfaceSection;
          if CurSection=nil then break;
          i:=length(CurSection.UsesClause)-1;
          end;
        end;
      // check module name too
      CurName:=RootElement.Name;
      if IsDottedIdentifierPrefix(CurName,aName)
          and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
        BestEl:=RootElement;

      if BestEl=nil then
        RaiseIdentifierNotFound(20170504172440,aName,ErrorEl);
      RightPath:=copy(aName,length(BestEl.Name)+2,length(aName));
      if BestEl.ClassType=TPasUsesUnit then
        CurScopeEl:=TPasUsesUnit(BestEl).Module
      else
        CurScopeEl:=BestEl;
      end
    else if NextEl<>nil then
      CurScopeEl:=NextEl
    else
      RaiseIdentifierNotFound(20170328001941,CurName,ErrorEl);

    // restore scope
    if NeedPop then
      PopScope;

    if RightPath='' then
      exit(NextEl);
  until false;
end;

function TPasResolver.FindElementWithoutParams(const AName: String;
  ErrorPosEl: TPasElement; NoProcsWithArgs: boolean): TPasElement;
var
  Data: TPRFindData;
begin
  Result:=FindElementWithoutParams(AName,Data,ErrorPosEl,NoProcsWithArgs);
  if Data.Found=nil then exit; // forward type: class-of or ^
  CheckFoundElement(Data,nil);
  if (Data.StartScope<>nil) and (Data.StartScope.ClassType=ScopeClass_WithExpr)
      and (wesfNeedTmpVar in TPasWithExprScope(Data.StartScope).Flags) then
    RaiseInternalError(20160923111727); // caller forgot to handle "With", use the other FindElementWithoutParams instead
end;

function TPasResolver.FindElementWithoutParams(const AName: String; out
  Data: TPRFindData; ErrorPosEl: TPasElement; NoProcsWithArgs: boolean
  ): TPasElement;
var
  Abort: boolean;
begin
  //writeln('TPasResolver.FindIdentifier Name="',AName,'"');
  Result:=Nil;
  Abort:=false;
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=ErrorPosEl;
  IterateElements(AName,@OnFindFirstElement,@Data,Abort);
  Result:=Data.Found;
  if Result=nil then
    begin
    if (ErrorPosEl=nil) and (LastElement<>nil) then
      begin
      if (LastElement.ClassType=TPasClassOfType)
        and (TPasClassOfType(LastElement).DestType=nil) then
        begin
        // 'class of' of a not yet defined class
        Result:=CreateElement(TUnresolvedPendingRef,AName,LastElement,visDefault,
                              CurrentParser.CurSourcePos);
        exit;
        end
      else if (LastElement.ClassType=TPasPointerType)
        and (TPasPointerType(LastElement).DestType=nil) then
        begin
        // pointer of a not yet defined type
        Result:=CreateElement(TUnresolvedPendingRef,AName,LastElement,visDefault,
                              CurrentParser.CurSourcePos);
        exit;
        end
      end;
    RaiseIdentifierNotFound(20170216152722,AName,ErrorPosEl);
    end;
  if NoProcsWithArgs and (Result is TPasProcedure)
      and ProcNeedsParams(TPasProcedure(Result).ProcType)
  then
    // proc needs parameters
    RaiseMsg(20170216152347,nWrongNumberOfParametersForCallTo,
      sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(TPasProcedure(Result).ProcType)],ErrorPosEl);
end;

procedure TPasResolver.FindLongestUnitName(var El: TPasElement; Expr: TPasExpr);
// Input: El is TPasUsesUnit
// Output: El is either a TPasUsesUnit or the root module
var
  CurUsesUnit: TPasUsesUnit;
  BestEl: TPasElement;
  aName, CurName: String;
  Clause: TPasUsesClause;
  i: Integer;
  Section: TPasSection;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName El=',GetObjName(El),' Expr=',GetObjName(Expr));
  {$ENDIF}
  if not (El is TPasUsesUnit) then
    RaiseInternalError(20170503000945);
  aName:=GetNameExprValue(Expr);
  if aName='' then
    RaiseNotYetImplemented(20170503110217,Expr);
  repeat
    Expr:=GetNextDottedExpr(Expr);
    if Expr=nil then break;
    CurName:=GetNameExprValue(Expr);
    if CurName='' then
      RaiseNotYetImplemented(20170502164242,Expr);
    aName:=aName+'.'+CurName;
  until false;

  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName Dotted="',aName,'"');
  {$ENDIF}
  // search in uses clause
  BestEl:=nil;
  Section:=TPasUsesUnit(El).Parent as TPasSection;
  repeat
    Clause:=Section.UsesClause;
    for i:=0 to length(Clause)-1 do
      begin
      CurUsesUnit:=Clause[i];
      CurName:=CurUsesUnit.Name;
      if IsDottedIdentifierPrefix(CurName,aName)
          and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
        BestEl:=CurUsesUnit; // a better match
      end;
    if Section is TImplementationSection then
      begin
      // search in interface uses clause too
      Section:=(Section.Parent as TPasModule).InterfaceSection;
      end
    else
      break;
  until Section=nil;
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName LongestUnit="',GetObjName(BestEl),'"');
  {$ENDIF}

  // check module name
  CurName:=El.GetModule.Name;
  if IsDottedIdentifierPrefix(CurName,aName)
      and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
    BestEl:=El.GetModule; // a better match
  if BestEl=nil then
    begin
    // no dotted module name fits the expression
    RaiseIdentifierNotFound(20170503140643,GetNameExprValue(Expr),Expr);
    end;
  El:=BestEl;
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName END Best="',GetObjName(El),'"');
  {$ENDIF}
end;

procedure TPasResolver.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  Scope: TPasScope;
begin
  for i:=FScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    Scope.IterateElements(AName,Scope,OnIterateElement,Data,Abort);
    if Abort then
      exit;
    if Scope is TPasSubScope then break;
    end;
end;

procedure TPasResolver.CheckFoundElement(
  const FindData: TPRFindData; Ref: TResolvedReference);
// check visibility rules
// Call this method after finding an element by searching the scopes.
var
  Proc: TPasProcedure;
  Context: TPasElement;
  FoundContext: TPasClassType;
  StartScope: TPasScope;
  OnlyTypeMembers, IsClassOf: Boolean;
  TypeEl: TPasType;
  C: TClass;
  ClassScope: TPasClassScope;
  i: Integer;
begin
  StartScope:=FindData.StartScope;
  OnlyTypeMembers:=false;
  IsClassOf:=false;
  if StartScope is TPasDotIdentifierScope then
    begin
    OnlyTypeMembers:=TPasDotIdentifierScope(StartScope).OnlyTypeMembers;
    if StartScope.ClassType=TPasDotClassScope then
      IsClassOf:=TPasDotClassScope(StartScope).IsClassOf;
    if Ref<>nil then
      begin
      Include(Ref.Flags,rrfDotScope);
      if TPasDotIdentifierScope(StartScope).ConstParent then
        Include(Ref.Flags,rrfConstInherited);
      end;
    end
  else if StartScope.ClassType=ScopeClass_WithExpr then
    begin
    OnlyTypeMembers:=wesfOnlyTypeMembers in TPasWithExprScope(StartScope).Flags;
    IsClassOf:=wesfIsClassOf in TPasWithExprScope(StartScope).Flags;
    if Ref<>nil then
      begin
      Include(Ref.Flags,rrfDotScope);
      if wesfConstParent in TPasWithExprScope(StartScope).Flags then
        Include(Ref.Flags,rrfConstInherited);
      end;
    end
  else if StartScope.ClassType=FScopeClass_Proc then
    begin
    Proc:=TPasProcedureScope(StartScope).Element as TPasProcedure;
    //writeln('TPasResolver.CheckFoundElement ',GetObjName(Proc),' ',IsClassMethod(Proc),' ElScope=',GetObjName(FindData.ElScope));
    if (FindData.ElScope<>StartScope) and IsClassMethod(Proc) then
      OnlyTypeMembers:=true;
    end;

  //writeln('TPasResolver.CheckFoundElOnStartScope StartScope=',StartScope.ClassName,
  //    ' StartIsDot=',StartScope is TPasDotIdentifierScope,
  //    ' OnlyTypeMembers=',(StartScope is TPasDotIdentifierScope)
  //       and TPasDotIdentifierScope(StartScope).OnlyTypeMembers,
  //    ' FindData.Found=',GetObjName(FindData.Found));
  if OnlyTypeMembers then
    begin
    //writeln('TPasResolver.CheckFoundElOnStartScope ',GetObjName(FindData.Found),' ',(FindData.Found is TPasVariable)
    //    and (vmClass in TPasVariable(FindData.Found).VarModifiers));
    // only class vars/procs allowed
    if (FindData.Found.ClassType=TPasConstructor) then
      // constructor: ok
    else if IsClassMethod(FindData.Found)
    then
      // class proc: ok
    else if (FindData.Found is TPasVariable)
        and (vmClass in TPasVariable(FindData.Found).VarModifiers) then
      // class var/const/property: ok
    else if (FindData.Found is TPasType) then
      // local type: ok
    else
      begin
      RaiseMsg(20170216152348,nCannotAccessThisMemberFromAX,
        sCannotAccessThisMemberFromAX,[GetElementTypeName(FindData.Found.Parent)],FindData.ErrorPosEl);
      end;
    end
  else if (proExtClassInstanceNoTypeMembers in Options)
      and (StartScope.ClassType=TPasDotClassScope)
      and TPasClassType(TPasDotClassScope(StartScope).ClassScope.Element).IsExternal then
    begin
    // found member in external class instance
      C:=FindData.Found.ClassType;
      if (C=TPasProcedure) or (C=TPasFunction) then
        // ok
      else if (C=TPasConst) then
        // ok
      else if C.InheritsFrom(TPasVariable)
          and (not (vmClass in TPasVariable(FindData.Found).VarModifiers)) then
        // ok
      else
        begin
        RaiseMsg(20170331184224,nExternalClassInstanceCannotAccessStaticX,
          sExternalClassInstanceCannotAccessStaticX,
          [GetElementTypeName(FindData.Found)+' '+FindData.Found.Name],
          FindData.ErrorPosEl);
        end;
    end;

  if (FindData.Found is TPasProcedure) then
    begin
    Proc:=TPasProcedure(FindData.Found);
    if Proc.IsVirtual or Proc.IsOverride then
      begin
      if (StartScope.ClassType=TPasDotClassScope)
      and TPasDotClassScope(StartScope).InheritedExpr then
        begin
        // call directly
        if Proc.IsAbstract then
          RaiseMsg(20170216152352,nAbstractMethodsCannotBeCalledDirectly,
            sAbstractMethodsCannotBeCalledDirectly,[],FindData.ErrorPosEl);
        end
      else
        begin
        // call via virtual method table
        if Ref<>nil then
          Ref.Flags:=Ref.Flags+[rrfVMT];
        end;
      end;

    // constructor: NewInstance or normal call
    //  it is a NewInstance iff the scope is a class, e.g. TObject.Create
    if (Proc.ClassType=TPasConstructor)
        and OnlyTypeMembers
        and (Ref<>nil) then
      begin
      Ref.Flags:=Ref.Flags+[rrfNewInstance]-[rrfConstInherited];
      // store the class in Ref.Context
      if Ref.Context<>nil then
        RaiseInternalError(20170131141936);
      Ref.Context:=TResolvedRefCtxConstructor.Create;
      if StartScope is TPasDotClassScope then
        ClassScope:=TPasDotClassScope(StartScope).ClassScope
      else if (StartScope is TPasWithExprScope)
          and (TPasWithExprScope(StartScope).Scope is TPasClassScope) then
        ClassScope:=TPasClassScope(TPasWithExprScope(StartScope).Scope)
      else if (StartScope is TPasProcedureScope) then
        ClassScope:=TPasProcedureScope(StartScope).ClassScope
      else
        RaiseInternalError(20170131150855,GetObjName(StartScope));
      TypeEl:=ClassScope.Element as TPasType;
      TResolvedRefCtxConstructor(Ref.Context).Typ:=TypeEl;
      if (length(ClassScope.AbstractProcs)>0) then
        begin
        if IsClassOf then
          // aClass.Create: do not warn
        else
          for i:=0 to length(ClassScope.AbstractProcs)-1 do
            LogMsg(20171227110746,mtWarning,nConstructingClassXWithAbstractMethodY,
              sConstructingClassXWithAbstractMethodY,
              [TypeEl.Name,ClassScope.AbstractProcs[i].Name],FindData.ErrorPosEl);
        end;
      end;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if (Proc.ClassType=TPasConstructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfNewInstance=',rrfNewInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope),
          ' OnlyTypeMembers=',OnlyTypeMembers);
        end;
      writeln;
      end;
    {AllowWriteln-}
    {$ENDIF}

    // destructor: FreeInstance or normal call
    // it is a normal call if 'inherited'
    if (Proc.ClassType=TPasDestructor) and (Ref<>nil) then
      if ((StartScope.ClassType<>TPasDotClassScope)
          or (not TPasDotClassScope(StartScope).InheritedExpr)) then
        Ref.Flags:=Ref.Flags+[rrfFreeInstance];
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if (Proc.ClassType=TPasDestructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfFreeInstance=',rrfFreeInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope));
        if StartScope.ClassType=TPasDotClassScope then
          write(' InheritedExpr=',TPasDotClassScope(StartScope).InheritedExpr);
        end;
      writeln;
      end;
    {AllowWriteln-}
    {$ENDIF}
    end;

  // check class visibility
  if FindData.Found.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected] then
    begin
    Context:=GetVisibilityContext;
    FoundContext:=FindData.Found.Parent as TPasClassType;
    case FindData.Found.Visibility of
      visPrivate:
        // private members can only be accessed in same module
        if FoundContext.GetModule<>Context.GetModule then
          RaiseMsg(20170216152354,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['private',FindData.Found.Name],FindData.ErrorPosEl);
      visProtected:
        // protected members can only be accessed in same module
        // or modules of descendant classes
        if FoundContext.GetModule=Context.GetModule then
          // same module -> ok
        else if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext,FindData.ErrorPosEl)<>cIncompatible) then
          // context in class or descendant
        else if (TopScope is TPasDotClassScope)
            and (TPasDotClassScope(TopScope).ClassScope.Element.GetModule=Context.GetModule) then
          // e.g. aClassInThisModule.identifier
        else if (TopScope is TPasWithExprScope)
            and (TPasWithExprScope(TopScope).Scope is TPasClassScope)
            and (TPasClassScope(TPasWithExprScope(TopScope).Scope).Element.GetModule=Context.GetModule) then
          // e.g. with aClassInThisModule do identifier
        else
          RaiseMsg(20170216152356,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['protected',FindData.Found.Name],FindData.ErrorPosEl);
      visStrictPrivate:
        // strict private members can only be accessed in their class
        if Context<>FoundContext then
          RaiseMsg(20170216152357,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['strict private',FindData.Found.Name],FindData.ErrorPosEl);
      visStrictProtected:
        // strict protected members can only be accessed in their and descendant classes
        if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext,FindData.ErrorPosEl)<>cIncompatible) then
          // context in class or descendant
        else
          RaiseMsg(20170216152400,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['strict protected',FindData.Found.Name],FindData.ErrorPosEl);
    end;
    end;
end;

function TPasResolver.GetVisibilityContext: TPasElement;
var
  i: Integer;
begin
  for i:=ScopeCount-1 downto 0 do
    begin
    Result:=Scopes[i].VisibilityContext;
    if Result<>nil then exit;
    end;
  Result:=nil;
end;

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  if IsElementSkipped(El) then exit;
  case ScopeType of
  stModule: FinishModule(El as TPasModule);
  stUsesClause: FinishUsesClause;
  stTypeSection: FinishTypeSection(El as TPasDeclarations);
  stTypeDef: FinishTypeDef(El as TPasType);
  stResourceString: FinishResourcestring(El as TPasResString);
  stProcedure: FinishProcedure(El as TPasProcedure);
  stProcedureHeader: FinishProcedureType(El as TPasProcedureType);
  stExceptOnExpr: FinishExceptOnExpr;
  stExceptOnStatement: FinishExceptOnStatement;
  stDeclaration: FinishDeclaration(El);
  stAncestors: FinishAncestors(El as TPasClassType);
  stInitialFinalization: FinishInitialFinalization(El as TPasImplBlock);
  else
    RaiseMsg(20170216152401,nNotYetImplemented,sNotYetImplemented+' FinishScope',[IntToStr(ord(ScopeType))],nil);
  end;
end;

procedure TPasResolver.FinishTypeAlias(var NewType: TPasType);
var
  TypeEl, DestType: TPasType;
  AncestorClass, aClass: TPasClassType;
  Scope: TPasIdentifierScope;
  OldType: TPasTypeAliasType;
begin
  DestType:=TPasTypeAliasType(NewType).DestType;
  TypeEl:=ResolveSimpleAliasType(DestType);
  if TypeEl is TPasClassType then
    begin
    // change "=type aClassType" to "=class(aClassType)"
    // or change "=type aInterfaceType" to "=interface(aInterfaceType)"
    AncestorClass := TPasClassType(TypeEl);

    // remove aliastype from scope
    Scope:=TopScope as TPasIdentifierScope;
    Scope.RemoveLocalIdentifier(NewType);

    // create class or interface
    aClass := TPasClassType(CreateElement(TPasClassType,
                              NewType.Name,NewType.Parent,NewType.Visibility,
                              NewType.SourceFilename,NewType.SourceLinenumber));
    aClass.ObjKind := AncestorClass.ObjKind;

    // release old alias type
    OldType := TPasTypeAliasType(NewType);
    NewType := aClass;
    TPasTypeAliasType(OldType).DestType:=nil; // clear reference
    OldType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};

    // set ancestor
    aClass.AncestorType := DestType;
    {$IFDEF CheckPasTreeRefCount}DestType.ChangeRefId('ResolveTypeReference','TPasClassType.AncestorType');{$ENDIF}
    FinishScope(stAncestors,aClass);
    end;
end;

function TPasResolver.IsUnitIntfFinished(AModule: TPasModule): boolean;
var
  CurIntf: TInterfaceSection;
begin
  CurIntf:=AModule.InterfaceSection;
  Result:=(CurIntf<>nil)
      and (CurIntf.CustomData is TPasSectionScope)
      and TPasSectionScope(CurIntf.CustomData).Finished;
end;

procedure TPasResolver.NotifyPendingUsedInterfaces;
// called after unit interface is ready to be used by other modules
var
  ModuleScope: TPasModuleScope;
  i: Integer;
  PendingResolver: TPasResolver;
  PendingSection: TPasSection;
begin
  // call all PendingResolvers
  // Note that a waiting resolver might continue parsing
  ModuleScope:=RootElement.CustomData as TPasModuleScope;
  i:=ModuleScope.PendingResolvers.Count-1;
  while i>=0 do
    begin
    PendingResolver:=TObject(ModuleScope.PendingResolvers[i]) as TPasResolver;
    PendingSection:=PendingResolver.GetLastSection;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.NotifyPendingUsedInterfaces "',ModuleScope.Element.Name,'" Pending="',PendingResolver.RootElement.Name,'"');
    {$ENDIF}
    if PendingSection=nil then
      RaiseInternalError(20180305141421);
    PendingResolver.CheckPendingUsedInterface(PendingSection); // beware: this might alter the ModuleScope.PendingResolvers
    dec(i);
    if i>=ModuleScope.PendingResolvers.Count then
      i:=ModuleScope.PendingResolvers.Count-1;
    end;
end;

function TPasResolver.GetPendingUsedInterface(Section: TPasSection
  ): TPasUsesUnit;
var
  i: Integer;
  UseUnit: TPasUsesUnit;
begin
  Result:=nil;
  for i:=0 to length(Section.UsesClause)-1 do
    begin
    UseUnit:=Section.UsesClause[i];
    if not (UseUnit.Module is TPasModule) then continue;
    if not IsUnitIntfFinished(TPasModule(UseUnit.Module)) then
      exit(UseUnit);
    end;
end;

function TPasResolver.CheckPendingUsedInterface(Section: TPasSection): boolean;
var
  PendingModule: TPasModule;
  PendingModuleScope: TPasModuleScope;
  List: TFPList;
  WasPending: Boolean;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckPendingUsedInterface START "',RootElement.Name,'" Section.PendingUsedIntf=',Section.PendingUsedIntf<>nil);
  {$ENDIF}
  WasPending:=Section.PendingUsedIntf<>nil;
  if WasPending then
    begin
    PendingModule:=Section.PendingUsedIntf.Module as TPasModule;
    if not IsUnitIntfFinished(PendingModule) then
      exit; // still pending
    // other unit interface is finished
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" UnitIntf finished of "',PendingModule.Name,'"');
    {$ENDIF}
    PendingModuleScope:=NoNil(PendingModule.CustomData) as TPasModuleScope;
    PendingModuleScope.PendingResolvers.Remove(Self);
    Section.PendingUsedIntf:=nil;
    end;

  Section.PendingUsedIntf:=GetPendingUsedInterface(Section);
  //writeln('TPasResolver.CheckPendingUsedInterface ',GetObjName(RootElement),' Section=',GetObjName(Section),' PendingUsedIntf=',GetObjName(Section.PendingUsedIntf));
  if Section.PendingUsedIntf<>nil then
    begin
    // module not yet finished due to pending used interfaces
    PendingModule:=Section.PendingUsedIntf.Module as TPasModule;
    PendingModuleScope:=NoNil(PendingModule.CustomData) as TPasModuleScope;
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" waiting for unit intf of "',PendingModule.Name,'"');
    {$ENDIF}
    List:=PendingModuleScope.PendingResolvers;
    if List.IndexOf(Self)<0 then
      List.Add(Self);
    Result:=not WasPending;
    end
  else
    begin
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    {AllowWriteln}
    if WasPending then
      writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" uses section complete: ',Section.ClassName);
    {AllowWriteln-}
    {$ENDIF}
    Result:=WasPending;
    if Result then
      UsedInterfacesFinished(Section);
    end;
end;

procedure TPasResolver.UsedInterfacesFinished(Section: TPasSection);
// if there is a unit cycle that stopped parsing this unit
// this method is called after the needed used unit interfaces have finished
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.UsesSectionFinished ',Section.ElementTypeName,' "',RootElement.Name,'"...');
  {$ENDIF}
  CurrentParser.ParseContinue;
  if Section=nil then ;
end;

function TPasResolver.NeedArrayValues(El: TPasElement): boolean;
// called by the parser when reading DoParseConstValueExpression
var
  C: TClass;
  V: TPasVariable;
  TypeEl: TPasType;
begin
  Result:=false;
  if El=nil then exit;
  C:=El.ClassType;
  if (C=TPasConst) or (C=TPasVariable) then
    begin
    V:=TPasVariable(El);
    if V.VarType=nil then exit;
    TypeEl:=ResolveAliasType(V.VarType);
    Result:=TypeEl.ClassType=TPasArrayType;
    end;
  //writeln('TPasResolver.NeedArrayValues ',GetObjName(El));
end;

function TPasResolver.GetDefaultClassVisibility(AClass: TPasClassType
  ): TPasMemberVisibility;
var
  ClassScope: TPasClassScope;
begin
  if AClass.CustomData=nil then
    exit(visDefault);
  ClassScope:=(AClass.CustomData as TPasClassScope);
  if pcsfPublished in ClassScope.Flags then
    Result:=visPublished
  else
    Result:=visPublic;
end;

procedure TPasResolver.ModeChanged(Sender: TObject; NewMode: TModeSwitch;
  Before: boolean; var Handled: boolean);
begin
  inherited ModeChanged(Sender, NewMode, Before, Handled);
  if not Before then
    begin
    if LastElement is TPasSection then
      TPasSectionScope(LastElement.CustomData).ModeSwitches:=CurrentParser.CurrentModeswitches;
    end;
end;

class procedure TPasResolver.UnmangleSourceLineNumber(LineNumber: integer; out
  Line, Column: integer);
begin
  Line:=Linenumber;
  Column:=0;
  if Line<0 then begin
    Line:=-Line;
    Column:=Line mod ParserMaxEmbeddedColumn;
    Line:=Line div ParserMaxEmbeddedColumn;
  end;
end;

class function TPasResolver.GetDbgSourcePosStr(El: TPasElement): string;
var
  Line, Column: integer;
begin
  if El=nil then exit('nil');
  UnmangleSourceLineNumber(El.SourceLinenumber,Line,Column);
  Result:=El.SourceFilename+'('+IntToStr(Line);
  if Column>0 then
    Result:=Result+','+IntToStr(Column);
  Result:=Result+')';
end;

function TPasResolver.GetElementSourcePosStr(El: TPasElement): string;
var
  Line, Column: integer;
begin
  if El=nil then exit('nil');
  UnmangleSourceLineNumber(El.SourceLinenumber,Line,Column);
  if (Line=0) then
    begin
    if El is TPasUnresolvedSymbolRef then
      exit('intrinsic');
    end;
  Result:=CurrentParser.Scanner.FormatPath(El.SourceFilename)+'('+IntToStr(Line);
  if Column>0 then
    Result:=Result+','+IntToStr(Column);
  Result:=Result+')';
end;

destructor TPasResolver.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy START ',ClassName);
  {$ENDIF}
  Clear;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy PopScope...');
  {$ENDIF}
  PopScope; // free default scope
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy FPendingForwards...');
  {$ENDIF}
  FreeAndNil(FPendingForwardProcs);
  FreeAndNil(fExprEvaluator);
  ClearBuiltInIdentifiers;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy END ',ClassName);
  {$ENDIF}
end;

procedure TPasResolver.Clear;
begin
  RestoreSubScopes(0);
  // clear stack, keep DefaultScope
  while (FScopeCount>0) and (FTopScope<>DefaultScope) do
    PopScope;
  ClearResolveDataList(lkModule);
end;

procedure TPasResolver.ClearBuiltInIdentifiers;
var
  bt: TResolverBaseType;
  bp: TResolverBuiltInProc;
begin
  ClearResolveDataList(lkBuiltIn);
  for bt in TResolverBaseType do
    ReleaseAndNil(TPasElement(FBaseTypes[bt]){$IFDEF CheckPasTreeRefCount},'TPasResolver.AddBaseType'{$ENDIF});
  for bp in TResolverBuiltInProc do
    FBuiltInProcs[bp]:=nil;
end;

procedure TPasResolver.AddObjFPCBuiltInIdentifiers(
  const TheBaseTypes: TResolveBaseTypes;
  const TheBaseProcs: TResolverBuiltInProcs);
var
  bt: TResolverBaseType;
begin
  for bt in TheBaseTypes do
    AddBaseType(BaseTypeNames[bt],bt);
  if bfLength in TheBaseProcs then
    AddBuiltInProc('Length','function Length(const String or Array): sizeint',
        @BI_Length_OnGetCallCompatibility,@BI_Length_OnGetCallResult,
        @BI_Length_OnEval,nil,bfLength);
  if bfSetLength in TheBaseProcs then
    AddBuiltInProc('SetLength','procedure SetLength(var String or Array; NewLength: sizeint)',
        @BI_SetLength_OnGetCallCompatibility,nil,nil,
        @BI_SetLength_OnFinishParamsExpr,bfSetLength,[bipfCanBeStatement]);
  if bfInclude in TheBaseProcs then
    AddBuiltInProc('Include','procedure Include(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,nil,
        @BI_InExclude_OnFinishParamsExpr,bfInclude,[bipfCanBeStatement]);
  if bfExclude in TheBaseProcs then
    AddBuiltInProc('Exclude','procedure Exclude(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,nil,
        @BI_InExclude_OnFinishParamsExpr,bfExclude,[bipfCanBeStatement]);
  if bfBreak in TheBaseProcs then
    AddBuiltInProc('Break','procedure Break',
        @BI_Break_OnGetCallCompatibility,nil,nil,nil,bfBreak,[bipfCanBeStatement]);
  if bfContinue in TheBaseProcs then
    AddBuiltInProc('Continue','procedure Continue',
        @BI_Continue_OnGetCallCompatibility,nil,nil,nil,bfContinue,[bipfCanBeStatement]);
  if bfExit in TheBaseProcs then
    AddBuiltInProc('Exit','procedure Exit(result)',
        @BI_Exit_OnGetCallCompatibility,nil,nil,nil,bfExit,[bipfCanBeStatement]);
  if bfInc in TheBaseProcs then
    AddBuiltInProc('Inc','procedure Inc(var Integer; const Incr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,nil,
        @BI_IncDec_OnFinishParamsExpr,bfInc,[bipfCanBeStatement]);
  if bfDec in TheBaseProcs then
    AddBuiltInProc('Dec','procedure Dec(var Integer; const Decr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,nil,
        @BI_IncDec_OnFinishParamsExpr,bfDec,[bipfCanBeStatement]);
  if bfAssigned in TheBaseProcs then
    AddBuiltInProc('Assigned','function Assigned(const Pointer or Class or Class-of): boolean',
        @BI_Assigned_OnGetCallCompatibility,@BI_Assigned_OnGetCallResult,
        nil,@BI_Assigned_OnFinishParamsExpr,bfAssigned);
  if bfChr in TheBaseProcs then
    AddBuiltInProc('Chr','function Chr(const Integer): char',
        @BI_Chr_OnGetCallCompatibility,@BI_Chr_OnGetCallResult,nil,nil,bfChr);
  if bfOrd in TheBaseProcs then
    AddBuiltInProc('Ord','function Ord(const Enum or Char): integer',
        @BI_Ord_OnGetCallCompatibility,@BI_Ord_OnGetCallResult,
        @BI_Ord_OnEval,nil,bfOrd);
  if bfLow in TheBaseProcs then
    AddBuiltInProc('Low','function Low(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,
        @BI_LowHigh_OnEval,nil,bfLow);
  if bfHigh in TheBaseProcs then
    AddBuiltInProc('High','function High(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,
        @BI_LowHigh_OnEval,nil,bfHigh);
  if bfPred in TheBaseProcs then
    AddBuiltInProc('Pred','function Pred(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,
        @BI_PredSucc_OnEval,nil,bfPred);
  if bfSucc in TheBaseProcs then
    AddBuiltInProc('Succ','function Succ(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,
        @BI_PredSucc_OnEval,nil,bfSucc);
  if bfStrProc in TheBaseProcs then
    AddBuiltInProc('Str','procedure Str(const var; var String)',
        @BI_StrProc_OnGetCallCompatibility,nil,nil,
        @BI_StrProc_OnFinishParamsExpr,bfStrProc,[bipfCanBeStatement]);
  if bfStrFunc in TheBaseProcs then
    AddBuiltInProc('Str','function Str(const var): String',
        @BI_StrFunc_OnGetCallCompatibility,@BI_StrFunc_OnGetCallResult,
        @BI_StrFunc_OnEval,nil,bfStrFunc);
  if bfWriteStr in TheBaseProcs then
    AddBuiltInProc('WriteStr','procedure WriteStr(out String; params...)',
        @BI_WriteStrProc_OnGetCallCompatibility,nil,nil,
        @BI_WriteStrProc_OnFinishParamsExpr,bfWriteStr,[bipfCanBeStatement]);
  if bfConcatArray in TheBaseProcs then
    AddBuiltInProc('Concat','function Concat(const Array1, Array2, ...): Array',
        @BI_ConcatArray_OnGetCallCompatibility,@BI_ConcatArray_OnGetCallResult,
        nil,nil,bfConcatArray);
  if bfCopyArray in TheBaseProcs then
    AddBuiltInProc('Copy','function Copy(const Array; Start: integer = 0; Count: integer = all): Array',
        @BI_CopyArray_OnGetCallCompatibility,@BI_CopyArray_OnGetCallResult,
        nil,nil,bfCopyArray);
  if bfInsertArray in TheBaseProcs then
    AddBuiltInProc('Insert','procedure Insert(const Element; var Array; Index: integer)',
        @BI_InsertArray_OnGetCallCompatibility,nil,nil,
        @BI_InsertArray_OnFinishParamsExpr,bfInsertArray,[bipfCanBeStatement]);
  if bfDeleteArray in TheBaseProcs then
    AddBuiltInProc('Delete','procedure Delete(var Array; Start, Count: integer)',
        @BI_DeleteArray_OnGetCallCompatibility,nil,nil,
        @BI_DeleteArray_OnFinishParamsExpr,bfDeleteArray,[bipfCanBeStatement]);
  if bfTypeInfo in TheBaseProcs then
    AddBuiltInProc('TypeInfo','function TypeInfo(type or var identifier): Pointer',
        @BI_TypeInfo_OnGetCallCompatibility,@BI_TypeInfo_OnGetCallResult,
        nil,nil,bfTypeInfo);
  if bfAssert in TheBaseProcs then
    AddBuiltInProc('Assert','procedure Assert(bool[,string])',
        @BI_Assert_OnGetCallCompatibility,nil,nil,
        @BI_Assert_OnFinishParamsExpr,bfAssert,[bipfCanBeStatement]);
  if bfNew in TheBaseProcs then
    AddBuiltInProc('New','procedure New(out ^record)',
        @BI_New_OnGetCallCompatibility,nil,nil,
        @BI_New_OnFinishParamsExpr,bfNew,[bipfCanBeStatement]);
  if bfDispose in TheBaseProcs then
    AddBuiltInProc('Dispose','procedure Dispose(var ^record)',
        @BI_Dispose_OnGetCallCompatibility,nil,nil,
        @BI_Dispose_OnFinishParamsExpr,bfDispose,[bipfCanBeStatement]);
  if bfDefault in TheBaseProcs then
    AddBuiltInProc('Default','function Default(T): T',
        @BI_Default_OnGetCallCompatibility,@BI_Default_OnGetCallResult,
        @BI_Default_OnEval,nil,bfDefault,[]);
end;

function TPasResolver.AddBaseType(const aName: string; Typ: TResolverBaseType
  ): TResElDataBaseType;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  {$IFDEF CheckPasTreeRefCount}El.RefIds.Add('TPasResolver.AddBaseType');{$ENDIF}
  if not (Typ in [btNone,btCustom]) then
    FBaseTypes[Typ]:=El;
  Result:=TResElDataBaseType.Create;
  Result.BaseType:=Typ;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBaseType);
end;

function TPasResolver.AddCustomBaseType(const aName: string;
  aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
var
  CustomData: TResElDataBaseType;
begin
  Result:=TPasUnresolvedSymbolRef.Create(aName,nil);
  {$IFDEF CheckPasTreeRefCount}Result.RefIds.Add('TPasResolver.AddCustomBaseType');{$ENDIF}
  CustomData:=aClass.Create;
  CustomData.BaseType:=btCustom;
  AddResolveData(Result,CustomData,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,Result,pikBaseType);
end;

function TPasResolver.IsBaseType(aType: TPasType; BaseType: TResolverBaseType;
  ResolveAlias: boolean): boolean;
begin
  Result:=false;
  if aType=nil then exit;
  if ResolveAlias then
    aType:=ResolveAliasType(aType);
  if aType.ClassType<>TPasUnresolvedSymbolRef then exit;
  Result:=CompareText(aType.Name,BaseTypeNames[BaseType])=0;
end;

function TPasResolver.AddBuiltInProc(const aName: string; Signature: string;
  const GetCallCompatibility: TOnGetCallCompatibility;
  const GetCallResult: TOnGetCallResult; const EvalConst: TOnEvalBIFunction;
  const FinishParamsExpr: TOnFinishParamsExpr;
  const BuiltIn: TResolverBuiltInProc; const Flags: TBuiltInProcFlags
  ): TResElDataBuiltInProc;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  Result:=TResElDataBuiltInProc.Create;
  Result.Proc:=El;
  {$IFDEF CheckPasTreeRefCount}El.RefIds.Add('TResElDataBuiltInProc.Proc');{$ENDIF}
  Result.Signature:=Signature;
  Result.BuiltIn:=BuiltIn;
  Result.GetCallCompatibility:=GetCallCompatibility;
  Result.GetCallResult:=GetCallResult;
  Result.Eval:=EvalConst;
  Result.FinishParamsExpression:=FinishParamsExpr;
  Result.Flags:=Flags;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBuiltInProc);
  if BuiltIn<>bfCustom then
    FBuiltInProcs[BuiltIn]:=Result;
end;

procedure TPasResolver.AddResolveData(El: TPasElement; Data: TResolveData;
  Kind: TResolveDataListKind);
begin
  if Data.Element<>nil then
    RaiseInternalError(20171111162227);
  if El.CustomData<>nil then
    RaiseInternalError(20171111162236);
  Data.Element:=El;
  Data.Owner:=Self;
  Data.Next:=FLastCreatedData[Kind];
  FLastCreatedData[Kind]:=Data;
  El.CustomData:=Data;
end;

function TPasResolver.CreateReference(DeclEl, RefEl: TPasElement;
  Access: TResolvedRefAccess; FindData: PPRFindData): TResolvedReference;

  procedure RaiseAlreadySet;
  var
    FormerDeclEl: TPasElement;
  begin
    {AllowWriteln}
    writeln('RaiseAlreadySet RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
    writeln('  RefEl at ',GetElementSourcePosStr(RefEl));
    writeln('  RefEl.CustomData=',GetObjName(RefEl.CustomData));
    if RefEl.CustomData is TResolvedReference then
      begin
      FormerDeclEl:=TResolvedReference(RefEl.CustomData).Declaration;
      writeln('  TResolvedReference(RefEl.CustomData).Declaration=',GetObjName(FormerDeclEl),
       ' IsSame=',FormerDeclEl=DeclEl);
      end;
    {AllowWriteln-}
    RaiseInternalError(20160922163554,'customdata<>nil');
  end;

begin
  if RefEl.CustomData<>nil then
    RaiseAlreadySet;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateReference RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
  {$ENDIF}
  Result:=TResolvedReference.Create;
  if FindData<>nil then
    begin
    if FindData^.StartScope.ClassType=ScopeClass_WithExpr then
      Result.WithExprScope:=TPasWithExprScope(FindData^.StartScope);
    end;
  AddResolveData(RefEl,Result,lkModule);
  Result.Declaration:=DeclEl;
  if RefEl is TPasExpr then
    SetResolvedRefAccess(TPasExpr(RefEl),Result,Access);
  EmitElementHints(RefEl,DeclEl);
end;

function TPasResolver.CreateScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  if not ScopeClass.IsStoredInElement then
    RaiseInternalError(20160923121858);
  if El.CustomData<>nil then
    RaiseInternalError(20160923121849);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateScope El=',GetObjName(El),' ScopeClass=',ScopeClass.ClassName);
  {$ENDIF}
  Result:=ScopeClass.Create;
  if Result.FreeOnPop then
    begin
    Result.Element:=El;
    El.CustomData:=Result;
    Result.Owner:=Self;
    end
  else
    // add to free list
    AddResolveData(El,Result,lkModule);
end;

procedure TPasResolver.PopScope;
var
  Scope: TPasScope;
begin
  if FScopeCount=0 then
    RaiseInternalError(20160922163557);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  //writeln('TPasResolver.PopScope ',FScopeCount,' ',FTopScope<>nil,' IsDefault=',FTopScope=FDefaultScope);
  writeln('TPasResolver.PopScope ',FTopScope.ClassName,' IsStoredInElement=',FTopScope.IsStoredInElement,' Element=',GetObjName(FTopScope.Element),' FreeOnPop=',FTopScope.FreeOnPop);
  {AllowWriteln-}
  {$ENDIF}
  dec(FScopeCount);
  if FTopScope.FreeOnPop then
    begin
    Scope:=FScopes[FScopeCount];
    if (Scope.Element<>nil) and (Scope.Element.CustomData=Scope) then
      Scope.Element.CustomData:=nil;
    if Scope=FDefaultScope then
      FDefaultScope:=nil;
    FScopes[FScopeCount]:=nil;
    Scope.Free;
    end;
  if FScopeCount>0 then
    FTopScope:=FScopes[FScopeCount-1]
  else
    FTopScope:=nil;
end;

procedure TPasResolver.PushScope(Scope: TPasScope);
begin
  if Scope=nil then
    RaiseInternalError(20160922163601);
  if length(FScopes)=FScopeCount then
    SetLength(FScopes,FScopeCount*2+10);
  FScopes[FScopeCount]:=Scope;
  inc(FScopeCount);
  FTopScope:=Scope;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.PushScope ScopeCount=',ScopeCount,' ',GetObjName(FTopScope));
  {$ENDIF}
end;

function TPasResolver.PushScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  Result:=CreateScope(El,ScopeClass);
  PushScope(Result);
end;

function TPasResolver.PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
begin
  Result:=TPasModuleDotScope.Create;
  Result.Owner:=Self;
  Result.Module:=aModule;
  if aModule is TPasProgram then
    begin // program
    if TPasProgram(aModule).ProgramSection<>nil then
      Result.InterfaceScope:=
        NoNil(TPasProgram(aModule).ProgramSection.CustomData) as TPasSectionScope;
    end
  else if aModule is TPasLibrary then
    begin // library
    if TPasLibrary(aModule).LibrarySection<>nil then
      Result.InterfaceScope:=
        NoNil(TPasLibrary(aModule).LibrarySection.CustomData) as TPasSectionScope;
    end
  else
    begin // unit
    if aModule.InterfaceSection<>nil then
      Result.InterfaceScope:=
        NoNil(aModule.InterfaceSection.CustomData) as TPasSectionScope;
    if (aModule=RootElement)
        and (aModule.ImplementationSection<>nil)
        and (aModule.ImplementationSection.CustomData<>nil)
    then
      Result.ImplementationScope:=NoNil(aModule.ImplementationSection.CustomData) as TPasSectionScope;
    if CompareText(aModule.Name,'system')=0 then
      Result.SystemScope:=DefaultScope;
    end;

  PushScope(Result);
end;

function TPasResolver.PushClassDotScope(var CurClassType: TPasClassType
  ): TPasDotClassScope;
var
  ClassScope: TPasClassScope;
  Ref: TResolvedReference;
begin
  if CurClassType.IsForward then
    begin
    Ref:=CurClassType.CustomData as TResolvedReference;
    CurClassType:=Ref.Declaration as TPasClassType;
    end;
  if CurClassType.CustomData=nil then
    RaiseInternalError(20160922163611);
  ClassScope:=NoNil(CurClassType.CustomData) as TPasClassScope;
  Result:=TPasDotClassScope.Create;
  Result.Owner:=Self;
  Result.ClassScope:=ClassScope;
  PushScope(Result);
end;

function TPasResolver.PushRecordDotScope(CurRecordType: TPasRecordType
  ): TPasDotRecordScope;
var
  RecScope: TPasRecordScope;
begin
  RecScope:=NoNil(CurRecordType.CustomData) as TPasRecordScope;
  Result:=TPasDotRecordScope.Create;
  Result.Owner:=Self;
  Result.IdentifierScope:=RecScope;
  PushScope(Result);
end;

function TPasResolver.PushEnumDotScope(CurEnumType: TPasEnumType
  ): TPasDotEnumTypeScope;
var
  EnumScope: TPasEnumTypeScope;
begin
  EnumScope:=NoNil(CurEnumType.CustomData) as TPasEnumTypeScope;
  Result:=TPasDotEnumTypeScope.Create;
  Result.Owner:=Self;
  Result.IdentifierScope:=EnumScope;
  PushScope(Result);
end;

procedure TPasResolver.ResetSubScopes(out Depth: integer);
// move all sub scopes from Scopes to SubScopes
begin
  Depth:=FSubScopeCount;
  while TopScope is TPasSubScope do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResetSubScopes moving ',TopScope.ClassName,' ScopeCount=',ScopeCount,' SubScopeCount=',FSubScopeCount);
    {$ENDIF}
    if FSubScopeCount=length(FSubScopes) then
      SetLength(FSubScopes,FSubScopeCount+4);
    FSubScopes[FSubScopeCount]:=TopScope;
    inc(FSubScopeCount);
    dec(FScopeCount);
    FScopes[FScopeCount]:=nil;
    if FScopeCount>0 then
      FTopScope:=FScopes[FScopeCount-1]
    else
      FTopScope:=nil;
    end;
end;

procedure TPasResolver.RestoreSubScopes(Depth: integer);
// restore sub scopes
begin
  while FSubScopeCount>Depth do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.RestoreSubScopes moving ',FSubScopes[FSubScopeCount-1].ClassName,' ScopeCount=',ScopeCount,' SubScopeCount=',FSubScopeCount);
    {$ENDIF}
    if FScopeCount=length(FScopes) then
      SetLength(FScopes,FScopeCount+4);
    dec(FSubScopeCount);
    FScopes[FScopeCount]:=FSubScopes[FSubScopeCount];
    FTopScope:=FScopes[FScopeCount];
    FSubScopes[FSubScopeCount]:=nil;
    inc(FScopeCount);
    end;
end;

function TPasResolver.GetInheritedExprScope(ErrorEl: TPasElement
  ): TPasProcedureScope;
var
  Scope: TPasScope;
  i: Integer;
begin
  i:=ScopeCount;
  repeat
    dec(i);
    if i<0 then
      RaiseMsg(20171006001229,nIllegalExpression,sIllegalExpression,[],ErrorEl);
    Scope:=Scopes[i];
    if Scope is TPasProcedureScope then
      exit(TPasProcedureScope(Scope));
  until false;
  Result:=nil;
end;

class function TPasResolver.MangleSourceLineNumber(Line, Column: integer
  ): integer;
begin
  if (Column<ParserMaxEmbeddedColumn)
      and (Line<ParserMaxEmbeddedRow) then
    Result:=-(Line*ParserMaxEmbeddedColumn+integer(Column))
  else
    Result:=Line;
end;

procedure TPasResolver.SetLastMsg(const id: TMaxPrecInt; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String;
  Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  PosEl: TPasElement);
var
{$IFDEF VerbosePasResolver}
  s: string;
{$ENDIF}
  Column, Row: integer;
begin
  FLastMsgId := id;
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  FLastElement := PosEl;
  if PosEl=nil then
    FLastSourcePos:=CurrentParser.CurSourcePos
  else
    begin
    FLastSourcePos.FileName:=PosEl.SourceFilename;
    UnmangleSourceLineNumber(PosEl.SourceLinenumber,Row,Column);
    if Row>=0 then
      FLastSourcePos.Row:=Row
    else
      FLastSourcePos.Row:=0;
    if Column>=0 then
      FLastSourcePos.Column:=Column
    else
      FLastSourcePos.Column:=0;
    end;
  CreateMsgArgs(FLastMsgArgs,Args);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  write('TPasResolver.SetLastMsg ',id,' ',GetElementSourcePosStr(PosEl),' ');
  s:='';
  str(MsgType,s);
  write(s);
  writeln(': [',MsgNumber,'] ',FLastMsg);
  {AllowWriteln-}
  {$ENDIF}
end;

procedure TPasResolver.RaiseMsg(const Id: TMaxPrecInt; MsgNumber: integer;
  const Fmt: String; Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  ErrorPosEl: TPasElement);
var
  E: EPasResolve;
begin
  SetLastMsg(Id,mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  E:=EPasResolve.Create(FLastMsg);
  E.Id:=Id;
  E.MsgType:=mtError;
  E.MsgNumber:=MsgNumber;
  E.MsgPattern:=Fmt;
  E.PasElement:=ErrorPosEl;
  E.Args:=FLastMsgArgs;
  E.SourcePos:=FLastSourcePos;
  raise E;
end;

procedure TPasResolver.RaiseNotYetImplemented(id: TMaxPrecInt; El: TPasElement;
  Msg: string);
var
  s: String;
begin
  s:=sNotYetImplemented+' ['+IntToStr(id)+']';
  if Msg<>'' then
    s:=s+' '+Msg;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseNotYetImplemented s="',s,'" El=',GetObjName(El));
  {$ENDIF}
  RaiseMsg(id,nNotYetImplemented,s,[GetObjName(El)],El);
end;

procedure TPasResolver.RaiseInternalError(id: TMaxPrecInt; const Msg: string);
begin
  raise Exception.Create('Internal error: ['+IntToStr(id)+'] '+Msg);
end;

procedure TPasResolver.RaiseInvalidScopeForElement(id: TMaxPrecInt; El: TPasElement;
  const Msg: string);
var
  i: Integer;
  s: String;
begin
  s:='['+IntToStr(id)+'] invalid scope for "'+GetObjName(El)+'": ';
  for i:=0 to ScopeCount-1 do
    begin
    if i>0 then s:=s+',';
    s:=s+Scopes[i].ClassName;
    end;
  if Msg<>'' then
    s:=s+': '+Msg;
  RaiseInternalError(id,s);
end;

procedure TPasResolver.RaiseIdentifierNotFound(id: TMaxPrecInt; Identifier: string;
  El: TPasElement);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIdentifierNotFound START "',Identifier,'" ErrorEl=',GetObjName(El));
  WriteScopes;
  {$ENDIF}
  RaiseMsg(id,nIdentifierNotFound,sIdentifierNotFound,[Identifier],El);
end;

procedure TPasResolver.RaiseXExpectedButYFound(id: TMaxPrecInt; const X, Y: string;
  El: TPasElement);
begin
  RaiseMsg(id,nXExpectedButYFound,sXExpectedButYFound,[X,Y],El);
end;

procedure TPasResolver.RaiseContextXExpectedButYFound(id: TMaxPrecInt; const C, X,
  Y: string; El: TPasElement);
begin
  RaiseMsg(id,nContextExpectedXButFoundY,sContextExpectedXButFoundY,[C,X,Y],El);
end;

procedure TPasResolver.RaiseContextXInvalidY(id: TMaxPrecInt; const X, Y: string;
  El: TPasElement);
begin
  RaiseMsg(id,nContextXInvalidY,sContextXInvalidY,[X,Y],El);
end;

procedure TPasResolver.RaiseConstantExprExp(id: TMaxPrecInt; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nConstantExpressionExpected,sConstantExpressionExpected,[],ErrorEl);
end;

procedure TPasResolver.RaiseVarExpected(id: TMaxPrecInt; ErrorEl: TPasElement;
  IdentEl: TPasElement);
begin
  if IdentEl is TPasProperty then
    RaiseMsg(id,nNoMemberIsProvidedToAccessProperty,
      sNoMemberIsProvidedToAccessProperty,[],ErrorEl)
  else
    RaiseMsg(id,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

procedure TPasResolver.RaiseRangeCheck(id: TMaxPrecInt; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nRangeCheckError,sRangeCheckError,[],ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeDesc(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  const GotDesc, ExpDesc: String; ErrorEl: TPasElement);

  function GetString(ArgNo: integer): string;
  begin
    if ArgNo>High(Args) then
      exit('invalid param '+IntToStr(ArgNo));
    {$ifdef pas2js}
    if isString(Args[ArgNo]) then
      Result:=String(Args[ArgNo])
    else
      Result:='invalid param '+jsTypeOf(Args[ArgNo]);
    {$else}
    case Args[ArgNo].VType of
    vtAnsiString: Result:=AnsiString(Args[ArgNo].VAnsiString);
    else
      Result:='invalid param '+IntToStr(Ord(Args[ArgNo].VType));
    end;
    {$endif}
  end;

begin
  case MsgNumber of
    nIllegalTypeConversionTo:
      RaiseMsg(id,MsgNumber,sIllegalTypeConversionTo,[GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypesGotExpected:
      RaiseMsg(id,MsgNumber,sIncompatibleTypesGotExpected,[GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypeArgNo:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNo,[GetString(0),GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypeArgNoVarParamMustMatchExactly:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNoVarParamMustMatchExactly,
               [GetString(0),GotDesc,ExpDesc],ErrorEl);
    nResultTypeMismatchExpectedButFound:
      RaiseMsg(id,MsgNumber,sResultTypeMismatchExpectedButFound,[GotDesc,ExpDesc],ErrorEl);
    nXExpectedButYFound:
      RaiseMsg(id,MsgNumber,sXExpectedButYFound,[GotDesc,ExpDesc],ErrorEl);
    nOperatorIsNotOverloadedAOpB:
      RaiseMsg(id,MsgNumber,sOperatorIsNotOverloadedAOpB,[GotDesc,GetString(0),ExpDesc],ErrorEl);
    nTypesAreNotRelatedXY:
      RaiseMsg(id,MsgNumber,sTypesAreNotRelatedXY,[GotDesc,ExpDesc],ErrorEl);
  else
    RaiseInternalError(20170329112911);
  end;
end;

procedure TPasResolver.RaiseIncompatibleType(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  GotType, ExpType: TPasType; ErrorEl: TPasElement);
var
  DescA, DescB: String;
begin
  DescA:=GetTypeDescription(GotType);
  DescB:=GetTypeDescription(ExpType);
  if DescA=DescB then
    begin
    DescA:=GetTypeDescription(GotType,true);
    DescB:=GetTypeDescription(ExpType,true);
    end;
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,DescA,DescB,ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeRes(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  const GotType, ExpType: TPasResolverResult;
  ErrorEl: TPasElement);
var
  GotDesc, ExpDesc: String;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIncompatibleTypeRes Got={',GetResolverResultDbg(GotType),'} Expected={',GetResolverResultDbg(ExpType),'}');
  {$ENDIF}
  GetIncompatibleTypeDesc(GotType,ExpType,GotDesc,ExpDesc);
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,GotDesc,ExpDesc,ErrorEl);
end;

procedure TPasResolver.RaiseInvalidProcTypeModifier(id: TMaxPrecInt;
  ProcType: TPasProcedureType; ptm: TProcTypeModifier; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ProcType),
    ProcTypeModifiers[ptm]],ErrorEl);
end;

procedure TPasResolver.RaiseInvalidProcModifier(id: TMaxPrecInt; Proc: TPasProcedure;
  pm: TProcedureModifier; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),
    ModifierNames[pm]],ErrorEl);
end;

procedure TPasResolver.LogMsg(const id: TMaxPrecInt; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String;
  Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  PosEl: TPasElement);
var
  Scanner: TPascalScanner;
  State: TWarnMsgState;
  {$IFDEF VerbosePasResolver}
  s: String;
  {$ENDIF}
begin
  Scanner:=CurrentParser.Scanner;
  if (Scanner<>nil) then
    begin
    if (FStep<prsFinishingModule)
        and (Scanner.IgnoreMsgType(MsgType)) then
      exit; // during parsing consider directives like $Hints on|off
    if MsgType>=mtWarning then
      begin
      State:=Scanner.WarnMsgState[MsgNumber];
      case State of
      wmsOff:
        begin
        {$IFDEF VerbosePasResolver}
        {AllowWriteln}
        write('TPasResolver.LogMsg ignoring ',id,' ',GetElementSourcePosStr(PosEl),' ');
        s:='';
        str(MsgType,s);
        write(s);
        writeln(': [',MsgNumber,'] ',SafeFormat(Fmt,Args));
        {AllowWriteln-}
        {$ENDIF}
        exit; // ignore
        end;
      wmsError:
        begin
        RaiseMsg(id,MsgNumber,Fmt,Args,PosEl);
        exit;
        end;
      end;
      end;
    end;

  SetLastMsg(id,MsgType,MsgNumber,Fmt,Args,PosEl);
  if Assigned(OnLog) then
    OnLog(Self,FLastMsg)
  else if Assigned(CurrentParser.OnLog) then
    CurrentParser.OnLog(Self,FLastMsg);
end;

class function TPasResolver.GetWarnIdentifierNumbers(Identifier: string; out
  MsgNumbers: TIntegerDynArray): boolean;

  procedure SetNumber(Number: integer);
  begin
    {$IF FPC_FULLVERSION>=30101}
    MsgNumbers:=[Number];
    {$ELSE}
    Setlength(MsgNumbers,1);
    MsgNumbers[0]:=Number;
    {$ENDIF}
  end;

  procedure SetNumbers(Numbers: array of integer);
  var
    i: Integer;
  begin
    Setlength(MsgNumbers,length(Numbers));
    for i:=0 to high(Numbers) do
      MsgNumbers[i]:=Numbers[i];
  end;

begin
  if Identifier='' then exit(false);
  if Identifier[1] in ['0'..'9'] then exit(false);

  Result:=true;
  case UpperCase(Identifier) of
  // FPC:
  'CONSTRUCTING_ABSTRACT': SetNumber(nConstructingClassXWithAbstractMethodY); //  Constructing an instance of a class with abstract methods.
  //'IMPLICIT_VARIANTS': ; //  Implicit use of the variants unit.
  // useanalyzer: 'NO_RETVAL': ; // Function result is not set.
  'SYMBOL_DEPRECATED': SetNumber(nSymbolXIsDeprecated); //   Deprecated symbol.
  'SYMBOL_EXPERIMENTAL': SetNumber(nSymbolXIsExperimental); //   Experimental symbol
  'SYMBOL_LIBRARY': SetNumber(nSymbolXBelongsToALibrary); //   Not used.
  'SYMBOL_PLATFORM': SetNumber(nSymbolXIsNotPortable); //   Platform-dependent symbol.
  'SYMBOL_UNIMPLEMENTED': SetNumber(nSymbolXIsNotImplemented); //   Unimplemented symbol.
  //'UNIT_DEPRECATED': ; //   Deprecated unit.
  //'UNIT_EXPERIMENTAL': ; //   Experimental unit.
  //'UNIT_LIBRARY': ; //
  //'UNIT_PLATFORM': ; //   Platform dependent unit.
  //'UNIT_UNIMPLEMENTED': ; //   Unimplemented unit.
  //'ZERO_NIL_COMPAT': ; //   Converting 0 to NIL
  //'IMPLICIT_STRING_CAST': ; // Implicit string type conversion
  //'IMPLICIT_STRING_CAST_LOSS': ; // Implicit string typecast with potential data loss from ”$1” to ”$2”
  //'EXPLICIT_STRING_CAST': ; //   Explicit string type conversion
  //'EXPLICIT_STRING_CAST_LOSS': ; //   Explicit string typecast with potential data loss from ”$1” to ”$2”
  //'CVT_NARROWING_STRING_LOST': ; //   Unicode constant cast with potential data loss

  // Delphi:
  'HIDDEN_VIRTUAL': SetNumber(nMethodHidesMethodOfBaseType); // method hides virtual method of ancestor
  'GARBAGE': SetNumber(nTextAfterFinalIgnored); // text after final end.
  'BOUNDS_ERROR': SetNumbers([nRangeCheckError,
      nHighRangeLimitLTLowRangeLimit,
      nRangeCheckEvaluatingConstantsVMinMax,
      nRangeCheckInSetConstructor]);
  'MESSAGE_DIRECTIVE': SetNumber(nUserDefined); // $message directive
  else
    Result:=false;
  end;
end;

procedure TPasResolver.GetIncompatibleTypeDesc(const GotType,
  ExpType: TPasResolverResult; out GotDesc, ExpDesc: String);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.GetIncompatibleTypeDesc Got={',GetResolverResultDbg(GotType),'} Expected={',GetResolverResultDbg(ExpType),'}');
  {$ENDIF}
  if GotType.BaseType<>ExpType.BaseType then
    begin
    GotDesc:=GetBaseDescription(GotType);
    if ExpType.BaseType=btNil then
      ExpDesc:=BaseTypeNames[btPointer]
    else
      ExpDesc:=GetBaseDescription(ExpType);
    if GotDesc=ExpDesc then
      begin
      GotDesc:=GetBaseDescription(GotType,true);
      ExpDesc:=GetBaseDescription(ExpType,true);
      end;
    end
  else if (GotType.LoTypeEl<>nil) and (ExpType.LoTypeEl<>nil) then
    begin
    GotDesc:=GetTypeDescription(GotType);
    ExpDesc:=GetTypeDescription(ExpType);
    if GotDesc<>ExpDesc then exit;
    if GotType.HiTypeEl<>ExpType.HiTypeEl then
      begin
      GotDesc:=GetTypeDescription(GotType.HiTypeEl);
      ExpDesc:=GetTypeDescription(ExpType.HiTypeEl);
      if GotDesc<>ExpDesc then exit;
      end;
    GotDesc:=GetTypeDescription(GotType,true);
    ExpDesc:=GetTypeDescription(ExpType,true);
    end
  else
    begin
    GotDesc:=GetResolverResultDescription(GotType,true);
    ExpDesc:=GetResolverResultDescription(ExpType,true);
    if GotDesc=ExpDesc then
      begin
      GotDesc:=GetResolverResultDescription(GotType,false);
      ExpDesc:=GetResolverResultDescription(ExpType,false);
      end;
    end;
end;

procedure TPasResolver.GetIncompatibleTypeDesc(const GotType,
  ExpType: TPasType; out GotDesc, ExpDesc: String);
begin
  GotDesc:=GetTypeDescription(GotType);
  ExpDesc:=GetTypeDescription(ExpType);
  if GotDesc<>ExpDesc then exit;
  GotDesc:=GetTypeDescription(GotType,true);
  ExpDesc:=GetTypeDescription(ExpType,true);
end;

function TPasResolver.CheckCallProcCompatibility(ProcType: TPasProcedureType;
  Params: TParamsExpr; RaiseOnError: boolean; SetReferenceFlags: boolean
  ): integer;
var
  ProcArgs: TFPList;
  i, ParamCnt, ParamCompatibility: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
begin
  Result:=cExact;
  ProcArgs:=ProcType.Args;
  // check args
  ParamCnt:=length(Params.Params);
  i:=0;
  while i<ParamCnt do
    begin
    Param:=Params.Params[i];
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckCallProcCompatibility ',i,'/',ParamCnt);
    {$ENDIF}
    if i<ProcArgs.Count then
      begin
      ParamCompatibility:=CheckParamCompatibility(Param,
        TPasArgument(ProcArgs[i]),i,RaiseOnError,SetReferenceFlags);
      if ParamCompatibility=cIncompatible then
        exit(cIncompatible);
      end
    else
      begin
      if ptmVarargs in ProcType.Modifiers then
        begin
        if SetReferenceFlags then
          Flags:=[rcNoImplicitProcType,rcSetReferenceFlags]
        else
          Flags:=[rcNoImplicitProcType];
        ComputeElement(Param,ParamResolved,Flags,Param);
        if not (rrfReadable in ParamResolved.Flags) then
          begin
          if RaiseOnError then
            RaiseVarExpected(20180712001415,Param,ParamResolved.IdentEl);
          exit(cIncompatible);
          end;
        ParamCompatibility:=cExact;
        end
      else
        begin
        // too many arguments
        if RaiseOnError then
          RaiseMsg(20170216152408,nWrongNumberOfParametersForCallTo,
            sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(ProcType)],Param);
        exit(cIncompatible);
        end;
      end;
    if Result<cTypeConversion then
      inc(Result,ParamCompatibility)
    else
      Result:=Max(Result,ParamCompatibility);
    inc(i);
    end;
  if (i<ProcArgs.Count) then
    if (TPasArgument(ProcArgs[i]).ValueExpr=nil) then
      begin
      // not enough arguments
      if RaiseOnError then
        // ToDo: position cursor on identifier
        RaiseMsg(20170216152410,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(ProcType)],Params.Value);
      exit(cIncompatible);
      end
    else
      begin
      // the rest are default params
      end;
end;

function TPasResolver.CheckCallPropertyCompatibility(PropEl: TPasProperty;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  PropArg: TPasArgument;
  ArgNo, ParamComp: Integer;
  Param: TPasExpr;
  PropArgs: TFPList;
begin
  Result:=cExact;
  PropArgs:=GetPasPropertyArgs(PropEl);
  if PropArgs.Count<length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152412,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
      [PropEl.Name],Params)
    end
  else if PropArgs.Count>length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152413,nMissingParameterX,sMissingParameterX,
      [TPasArgument(PropArgs[length(Params.Params)]).Name],Params);
    end;
  for ArgNo:=0 to PropArgs.Count-1 do
    begin
    PropArg:=TPasArgument(PropArgs[ArgNo]);
    Param:=Params.Params[ArgNo];
    ParamComp:=CheckParamCompatibility(Param,PropArg,ArgNo,RaiseOnError);
    if ParamComp=cIncompatible then
      exit(cIncompatible);
    inc(Result,ParamComp);
    end;
end;

function TPasResolver.CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
  Params: TParamsExpr; RaiseOnError: boolean; EmitHints: boolean): integer;
var
  ArgNo: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;

  procedure GetNextParam;
  begin
    if ArgNo>=length(Params.Params) then
      RaiseMsg(20170216152415,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    Param:=Params.Params[ArgNo];
    ComputeElement(Param,ParamResolved,[]);
    inc(ArgNo);
  end;

var
  DimNo: integer;
  RangeResolved, OrigRangeResolved, OrigParamResolved: TPasResolverResult;
  bt: TResolverBaseType;
  NextType, TypeEl: TPasType;
  RangeExpr: TPasExpr;
  TypeFits: Boolean;
  ParamValue: TResEvalValue;
begin
  ArgNo:=0;
  repeat
    if length(ArrayEl.Ranges)=0 then
      begin
      // dynamic/open array -> needs exactly one integer
      GetNextParam;
      if (not (rrfReadable in ParamResolved.Flags))
          or not (ParamResolved.BaseType in btAllInteger) then
        exit(CheckRaiseTypeArgNo(20170216152417,ArgNo,Param,ParamResolved,'integer',RaiseOnError));
      if EmitHints then
        begin
        ParamValue:=Eval(Param,[refAutoConstExt]);
        if ParamValue<>nil then
          try // has const value -> check range
            if ParamValue.Kind=revkExternal then
              // ignore
            else if (ParamValue.Kind<>revkInt)
                or (TResEvalInt(ParamValue).Int<DynArrayMinIndex)
                or (TResEvalInt(ParamValue).Int>DynArrayMaxIndex) then
              fExprEvaluator.EmitRangeCheckConst(20170520202212,ParamValue.AsString,
                                  DynArrayMinIndex,DynArrayMaxIndex,Param);
          finally
            ReleaseEvalValue(ParamValue);
          end;
        end;
      end
    else
      begin
      // static array
      for DimNo:=0 to length(ArrayEl.Ranges)-1 do
        begin
        GetNextParam;
        RangeExpr:=ArrayEl.Ranges[DimNo];
        ComputeElement(RangeExpr,RangeResolved,[]);
        bt:=RangeResolved.BaseType;
        if not (rrfReadable in ParamResolved.Flags) then
          begin
          if not RaiseOnError then exit(cIncompatible);
          RaiseIncompatibleTypeRes(20170216152421,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ParamResolved,RangeResolved,Param);
          end;
        TypeFits:=false;
        OrigRangeResolved:=RangeResolved;
        OrigParamResolved:=ParamResolved;

        if bt=btRange then
          begin
          ConvertRangeToElement(RangeResolved);
          bt:=RangeResolved.BaseType;
          end;
        if ParamResolved.BaseType=btRange then
          begin
          ConvertRangeToElement(ParamResolved);
          end;

        if (bt in btAllBooleans) then
          begin
          if (ParamResolved.BaseType in btAllBooleans) then
            TypeFits:=true;
          end
        else if (bt in btAllInteger) then
          begin
          if (ParamResolved.BaseType in btAllInteger) then
            TypeFits:=true;
          end
        else if (bt in btAllChars) then
          begin
          if (ParamResolved.BaseType in btAllChars) then
            TypeFits:=true;
          end
        else if (bt=btContext) then
          begin
          TypeEl:=RangeResolved.LoTypeEl;
          if ParamResolved.BaseType=btContext then
            begin
            if (TypeEl.ClassType=TPasEnumType)
                and IsSameType(TypeEl,ParamResolved.LoTypeEl,prraNone) then
              TypeFits:=true;
            end;
          end;
        if not TypeFits then
          begin
          // incompatible
          if not RaiseOnError then exit(cIncompatible);
          RaiseIncompatibleTypeRes(20170216152422,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],OrigParamResolved,OrigRangeResolved,Param);
          end;
        if EmitHints then
          fExprEvaluator.IsInRange(Param,RangeExpr,true);
        end;
      end;
    if ArgNo=length(Params.Params) then exit(cExact);

    // there are more parameters -> continue in sub array
    NextType:=ResolveAliasType(ArrayEl.ElType);
    if NextType.ClassType<>TPasArrayType then
      RaiseMsg(20170216152424,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    ArrayEl:=TPasArrayType(NextType);
  until false;
  Result:=cIncompatible;
end;

function TPasResolver.CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure
  ): boolean;
// returns if number and type of arguments fit
// does not check calling convention
var
  ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
begin
  Result:=false;
  ProcArgs1:=Proc1.ProcType.Args;
  ProcArgs2:=Proc2.ProcType.Args;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckOverloadProcCompatibility START Count=',ProcArgs1.Count,' ',ProcArgs2.Count);
  {$ENDIF}
  // check args
  if ProcArgs1.Count<>ProcArgs2.Count then
    exit;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckOverloadProcCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    if not CheckProcArgCompatibility(TPasArgument(ProcArgs1[i]),
                                     TPasArgument(ProcArgs2[i])) then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcTypeCompatibility(Proc1,
  Proc2: TPasProcedureType; IsAssign: boolean; ErrorEl: TPasElement;
  RaiseOnIncompatible: boolean): boolean;
// if RaiseOnIncompatible=true, then Expected=Proc1 Actual=Proc2

  function ModifierError(Modifier: TProcTypeModifier): boolean;
  begin
    Result:=false;
    if not RaiseOnIncompatible then exit;
    RaiseMsg(20170402112049,nXModifierMismatchY,sXModifierMismatchY,
      [GetElementTypeName(Proc1),ProcTypeModifiers[Modifier]],ErrorEl);
  end;

var
  ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
  Result1Resolved, Result2Resolved: TPasResolverResult;
  ExpectedArg, ActualArg: TPasArgument;
begin
  Result:=false;
  if Proc1.ClassType<>Proc2.ClassType then
    begin
    if RaiseOnIncompatible then
      RaiseXExpectedButYFound(20170402112353,GetElementTypeName(Proc1),GetElementTypeName(Proc2),ErrorEl);
    exit;
    end;
  if Proc1.IsReferenceTo then
    begin
    if IsAssign then
      // aRefTo:=aproc -> any IsNested/OfObject is allowed
    else
      ; // aRefTo = AnyProc -> ok
    end
  else if Proc2.IsReferenceTo then
    begin
    if IsAssign then
      // NonRefTo := aRefTo  -> not possible
      exit(ModifierError(ptmReferenceTo))
    else
      ; // AnyProc = aRefTo -> ok
    end
  else
    begin
    // neither Proc1 nor Proc2 is a reference-to  -> check isNested and OfObject
    if Proc1.IsNested<>Proc2.IsNested then
      exit(ModifierError(ptmIsNested));
    if Proc1.IsOfObject<>Proc2.IsOfObject then
      begin
      if (proProcTypeWithoutIsNested in Options) then
        exit(ModifierError(ptmOfObject))
      else if Proc1.IsNested then
        // "is nested" can handle both, proc and method.
      else
        exit(ModifierError(ptmOfObject))
      end;
    end;
  if Proc1.CallingConvention<>Proc2.CallingConvention then
    begin
    if RaiseOnIncompatible then
      RaiseMsg(20170402112253,nCallingConventionMismatch,sCallingConventionMismatch,
        [],ErrorEl);
    exit;
    end;
  ProcArgs1:=Proc1.Args;
  ProcArgs2:=Proc2.Args;
  if ProcArgs1.Count<>ProcArgs2.Count then
    begin
    if RaiseOnIncompatible then
      RaiseMsg(20170902142829,nIncompatibleTypesGotParametersExpected,
        sIncompatibleTypesGotParametersExpected,
        [IntToStr(ProcArgs1.Count),IntToStr(ProcArgs2.Count)],ErrorEl);
    exit;
    end;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckProcAssignCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    ExpectedArg:=TPasArgument(ProcArgs1[i]);
    ActualArg:=TPasArgument(ProcArgs2[i]);
    if not CheckProcArgCompatibility(ExpectedArg,ActualArg) then
      begin
      if RaiseOnIncompatible then
        begin
        if ExpectedArg.Access<>ActualArg.Access then
          RaiseMsg(20170404151541,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(i+1),'access modifier '+AccessDescriptions[ActualArg.Access],
             AccessDescriptions[ExpectedArg.Access]],
            ErrorEl);
        RaiseIncompatibleType(20170404151538,nIncompatibleTypeArgNo,
          [IntToStr(i+1)],ExpectedArg.ArgType,ActualArg.ArgType,ErrorEl);
        end;
      exit;
      end;
    end;
  if Proc1 is TPasFunctionType then
    begin
    ComputeElement(TPasFunctionType(Proc1).ResultEl.ResultType,Result1Resolved,[rcType]);
    ComputeElement(TPasFunctionType(Proc2).ResultEl.ResultType,Result2Resolved,[rcType]);
    if (Result1Resolved.BaseType<>Result2Resolved.BaseType)
        or not IsSameType(Result1Resolved.HiTypeEl,Result2Resolved.HiTypeEl,prraSimple) then
      begin
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170402112648,nResultTypeMismatchExpectedButFound,
          [],Result1Resolved,Result2Resolved,ErrorEl);
      exit;
      end;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcArgCompatibility(Arg1, Arg2: TPasArgument): boolean;
begin
  Result:=false;

  // check access: var, const, ...
  if Arg1.Access<>Arg2.Access then exit;

  // check untyped
  if Arg1.ArgType=nil then
    exit(Arg2.ArgType=nil);
  if Arg2.ArgType=nil then exit;

  Result:=CheckElTypeCompatibility(Arg1.ArgType,Arg2.ArgType,prraSimple);
end;

function TPasResolver.CheckElTypeCompatibility(Arg1, Arg2: TPasType;
  ResolveAlias: TPRResolveAlias): boolean;
var
  Arg1Resolved, Arg2Resolved: TPasResolverResult;
  C: TClass;
  Arr1, Arr2: TPasArrayType;
begin
  ComputeElement(Arg1,Arg1Resolved,[rcType]);
  ComputeElement(Arg2,Arg2Resolved,[rcType]);
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckElTypeCompatibility Arg1=',GetResolverResultDbg(Arg1Resolved),' Arg2=',GetResolverResultDbg(Arg2Resolved));
  {$ENDIF}

  if (Arg1Resolved.BaseType<>Arg2Resolved.BaseType)
      or (Arg1Resolved.LoTypeEl=nil)
      or (Arg2Resolved.LoTypeEl=nil) then
    exit(false);
  if Arg1Resolved.BaseType=Arg2Resolved.BaseType then
    begin
    if ResolveAlias=prraSimple then
      begin
      if IsSameType(Arg1Resolved.HiTypeEl,Arg2Resolved.HiTypeEl,prraSimple) then
        exit(true);
      end
    else
      begin
      if IsSameType(Arg1Resolved.LoTypeEl,Arg2Resolved.LoTypeEl,prraNone) then
        exit(true);
      end;
    end;
  C:=Arg1Resolved.LoTypeEl.ClassType;
  if (C=TPasArrayType) and (Arg2Resolved.LoTypeEl.ClassType=TPasArrayType) then
    begin
    Arr1:=TPasArrayType(Arg1Resolved.LoTypeEl);
    Arr2:=TPasArrayType(Arg2Resolved.LoTypeEl);
    if length(Arr1.Ranges)<>length(Arr2.Ranges) then
      exit(false);
    if length(Arr1.Ranges)>0 then
      RaiseNotYetImplemented(20170328093733,Arr1.Ranges[0],'anonymous static array');
    Result:=CheckElTypeCompatibility(Arr1.ElType,Arr2.ElType,ResolveAlias);
    exit;
    end;

  Result:=false;
end;

function TPasResolver.CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
  ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
var
  El: TPasElement;
begin
  Result:=false;
  El:=ResolvedEl.IdentEl;
  if El=nil then
    begin
    if (ResolvedEl.ExprEl is TUnaryExpr)
        and (TUnaryExpr(ResolvedEl.ExprEl).OpCode=eopDeref) then
      begin
      // e.g. p^:=
      end
    else
      begin
      if ErrorOnFalse then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckCanBeLHS no identifier: ',GetResolverResultDbg(ResolvedEl));
        {$ENDIF}
        if (ResolvedEl.LoTypeEl<>nil) and (ResolvedEl.ExprEl<>nil) then
          RaiseXExpectedButYFound(20170216152727,'identifier',GetElementTypeName(ResolvedEl.LoTypeEl),ResolvedEl.ExprEl)
        else
          RaiseVarExpected(20170216152426,ErrorEl,ResolvedEl.IdentEl);
        end;
      exit;
      end;
    end;
  if [rrfWritable,rrfAssignable]*ResolvedEl.Flags<>[] then
    exit(not IsVariableConst(El,ErrorEl,ErrorOnFalse));
  // not writable
  if not ErrorOnFalse then exit;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckCanBeLHS not writable: ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.IdentEl is TPasProperty then
    RaiseMsg(20170216152427,nPropertyNotWritable,sPropertyNotWritable,[],ErrorEl)
  else if ResolvedEl.IdentEl is TPasConst then
    RaiseMsg(20180430012042,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],ErrorEl)
  else
    RaiseMsg(20170216152429,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

function TPasResolver.CheckAssignCompatibility(const LHS, RHS: TPasElement;
  RaiseOnIncompatible: boolean; ErrorEl: TPasElement): integer;
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  IsProcType: Boolean;
begin
  if ErrorEl=nil then
    ErrorEl:=RHS;
  ComputeElement(LHS,LeftResolved,[rcNoImplicitProc]);
  Flags:=[];
  IsProcType:=IsProcedureType(LeftResolved,true);
  if IsProcType then
    if msDelphi in CurrentParser.CurrentModeswitches then
      Include(Flags,rcNoImplicitProc)
    else
      Include(Flags,rcNoImplicitProcType);
  ComputeElement(RHS,RightResolved,Flags);
  Result:=CheckAssignResCompatibility(LeftResolved,RightResolved,ErrorEl,RaiseOnIncompatible);
  if RHS is TPasExpr then
    CheckAssignExprRange(LeftResolved,TPasExpr(RHS));
end;

procedure TPasResolver.CheckAssignExprRange(
  const LeftResolved: TPasResolverResult; RHS: TPasExpr);
// if RHS is a constant check if it fits into range LeftResolved
var
  LRangeValue, RValue: TResEvalValue;
  Int, MinVal, MaxVal: TMaxPrecInt;
  RangeExpr: TBinaryExpr;
  C: TClass;
  EnumType: TPasEnumType;
  bt: TResolverBaseType;
  LTypeEl: TPasType;
  {$ifdef FPC_HAS_CPSTRING}
  w: WideChar;
  {$endif}
begin
  LTypeEl:=LeftResolved.LoTypeEl;
  if (LTypeEl<>nil)
      and ((LTypeEl.ClassType=TPasArrayType)
        or (LTypeEl.ClassType=TPasRecordType)) then
    exit; // arrays and records are checked by element, not by the whole value
  if LTypeEl is TPasClassOfType then
    exit; // class-of are checked only by type, not by value
  RValue:=Eval(RHS,[refAutoConstExt]);
  if RValue=nil then
    exit; // not a const expression
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.CheckAssignExprRange Left=',GetResolverResultDbg(LeftResolved),' RValue=',RValue.AsDebugString);
  {$ENDIF}
  LRangeValue:=nil;
  try
    if RValue.Kind=revkExternal then
      // skip
    else if LeftResolved.BaseType=btCustom then
      CheckAssignExprRangeToCustom(LeftResolved,RValue,RHS)
    else if LeftResolved.BaseType=btSet then
      begin
      // assign to a set
      C:=LTypeEl.ClassType;
      if C=TPasRangeType then
        begin
        RangeExpr:=TPasRangeType(LTypeEl).RangeExpr;
        LRangeValue:=Eval(RangeExpr,[refConst],false);
        end
      else if C=TPasEnumType then
        begin
        EnumType:=TPasEnumType(LTypeEl);
        LRangeValue:=TResEvalRangeInt.CreateValue(revskEnum,EnumType,
          0,EnumType.Values.Count-1);
        end
      else if C=TPasUnresolvedSymbolRef then
        begin
        // set of basetype
        if LTypeEl.CustomData is TResElDataBaseType then
          begin
          bt:=GetActualBaseType(TResElDataBaseType(LTypeEl.CustomData).BaseType);
          if (bt in btAllIntegerNoQWord) and GetIntegerRange(bt,MinVal,MaxVal) then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskInt,nil,MinVal,MaxVal)
          else if bt=btBoolean then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskBool,nil,0,1)
          {$ifdef FPC_HAS_CPSTRING}
          else if bt=btAnsiChar then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff)
          {$endif}
          else if bt=btWideChar then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff)
          else
            RaiseNotYetImplemented(20170714205110,RHS);
          end
        else
          RaiseNotYetImplemented(20170714204803,RHS);
        end
      else
        RaiseNotYetImplemented(20170714193100,RHS);
      fExprEvaluator.IsSetCompatible(RValue,RHS,LRangeValue,true);
      end
    else if LTypeEl is TPasRangeType then
      begin
      RangeExpr:=TPasRangeType(LTypeEl).RangeExpr;
      LRangeValue:=Eval(RangeExpr,[refConst]);
      if LeftResolved.BaseType=btSet then
        fExprEvaluator.IsSetCompatible(RValue,RHS,LRangeValue,true)
      else
        fExprEvaluator.IsInRange(RValue,RHS,LRangeValue,RangeExpr,true);
      end
    else if (LeftResolved.BaseType in btAllIntegerNoQWord)
        and GetIntegerRange(LeftResolved.BaseType,MinVal,MaxVal) then
      case RValue.Kind of
      revkInt:
        if (MinVal>TResEvalInt(RValue).Int)
            or (MaxVal<TResEvalInt(RValue).Int) then
          fExprEvaluator.EmitRangeCheckConst(20170530093126,
            IntToStr(TResEvalInt(RValue).Int),MinVal,MaxVal,RHS);
      revkUInt:
        if (TResEvalUInt(RValue).UInt>High(TMaxPrecInt))
            or (MinVal>TMaxPrecInt(TResEvalUInt(RValue).UInt))
            or (MaxVal<TMaxPrecInt(TResEvalUInt(RValue).UInt)) then
          fExprEvaluator.EmitRangeCheckConst(20170530093616,
            IntToStr(TResEvalUInt(RValue).UInt),IntToStr(MinVal),IntToStr(MaxVal),RHS);
      revkFloat:
        if TResEvalFloat(RValue).IsInt(Int) then
          begin
          if (MinVal>Int) or (MaxVal<Int) then
            fExprEvaluator.EmitRangeCheckConst(20170802133307,
              IntToStr(Int),MinVal,MaxVal,RHS,mtError);
          end
        else
          begin
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.CheckAssignExprRange ',Frac(TResEvalFloat(RValue).FloatValue),' ',TResEvalFloat(RValue).FloatValue<TMaxPrecFloat(low(TMaxPrecInt)),' ',TResEvalFloat(RValue).FloatValue>TMaxPrecFloat(high(TMaxPrecInt)),' ',TResEvalFloat(RValue).FloatValue,' ',high(TMaxPrecInt));
          {$ENDIF}
          RaiseRangeCheck(20170802133750,RHS);
          end;
      revkCurrency:
        if TResEvalCurrency(RValue).IsInt(Int) then
          begin
          if (MinVal>Int) or (MaxVal<Int) then
            fExprEvaluator.EmitRangeCheckConst(20180421171325,
              IntToStr(Int),MinVal,MaxVal,RHS,mtError);
          end
        else
          begin
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.CheckAssignExprRange ',Frac(TResEvalCurrency(RValue).Value),' ',TResEvalCurrency(RValue).Value,' ',high(TMaxPrecInt));
          {$ENDIF}
          RaiseRangeCheck(20180421171438,RHS);
          end;
      else
        {$IFDEF VerbosePasResEval}
        writeln('TPasResolver.CheckAssignExprRange ',RValue.AsDebugString);
        {$ENDIF}
        RaiseNotYetImplemented(20170530092731,RHS);
      end
    {$ifdef HasInt64}
    else if LeftResolved.BaseType=btQWord then
      case RValue.Kind of
      revkInt:
        if (TResEvalInt(RValue).Int<0) then
          fExprEvaluator.EmitRangeCheckConst(20170530094316,
            IntToStr(TResEvalUInt(RValue).UInt),'0',IntToStr(High(QWord)),RHS);
      revkUInt: ;
      else
        RaiseNotYetImplemented(20170530094311,RHS);
      end
    {$endif}
    else if RValue.Kind in [revkNil,revkBool] then
      // simple type check is enough
    else if LeftResolved.BaseType in [btSingle,btDouble,btCurrency] then
      // simple type check is enough
      // ToDo: warn if precision loss
    else if LeftResolved.BaseType in btAllChars then
      begin
      case RValue.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        if length(TResEvalString(RValue).S)<>1 then
          begin
          if fExprEvaluator.GetWideChar(TResEvalString(RValue).S,w) then
            Int:=ord(w)
          else
            RaiseXExpectedButYFound(20170714171352,'char','string',RHS);
          end
        else
          Int:=ord(TResEvalString(RValue).S[1]);
      {$endif}
      revkUnicodeString:
        if length(TResEvalUTF16(RValue).S)<>1 then
          RaiseXExpectedButYFound(20170714171534,'char','string',RHS)
        else
          Int:=ord(TResEvalUTF16(RValue).S[1]);
      else
        RaiseNotYetImplemented(20170714171218,RHS);
      end;
      case GetActualBaseType(LeftResolved.BaseType) of
      {$ifdef FPC_HAS_CPSTRING}
      btAnsiChar: MaxVal:=$ff;
      {$endif}
      btWideChar: MaxVal:=$ffff;
      end;
      if (Int>MaxVal) then
        fExprEvaluator.EmitRangeCheckConst(20170714171911,
          '#'+IntToStr(Int),'#0','#'+IntToStr(MaxVal),RHS);
      end
    else if LeftResolved.BaseType in btAllStrings then
      // simple type check is enough
      // ToDo: warn if unicode to non-utf8
    else if LeftResolved.BaseType=btContext then
      // simple type check is enough
    else if LeftResolved.BaseType=btRange then
      begin
      if (LeftResolved.ExprEl is TBinaryExpr)
          and (TBinaryExpr(LeftResolved.ExprEl).Kind=pekRange) then
        begin
        LRangeValue:=Eval(LeftResolved.ExprEl,[refConst]);
        try
          case LRangeValue.Kind of
          revkRangeInt:
            case TResEvalRangeInt(LRangeValue).ElKind of
            revskEnum:
              if (RValue.Kind<>revkEnum) then
                RaiseNotYetImplemented(20171009171251,RHS)
              else if (TResEvalEnum(RValue).Index<TResEvalRangeInt(LRangeValue).RangeStart)
                  or (TResEvalEnum(RValue).Index>TResEvalRangeInt(LRangeValue).RangeEnd) then
                fExprEvaluator.EmitRangeCheckConst(20171009171442,
                  TResEvalEnum(RValue).AsString,
                  TResEvalRangeInt(LRangeValue).ElementAsString(TResEvalRangeInt(LRangeValue).RangeStart),
                  TResEvalRangeInt(LRangeValue).ElementAsString(TResEvalRangeInt(LRangeValue).RangeEnd),
                  RHS);
            else
              RaiseNotYetImplemented(20171009165348,LeftResolved.ExprEl);
            end;
          else
            RaiseNotYetImplemented(20171009165326,LeftResolved.ExprEl);
          end;
        finally
          ReleaseEvalValue(LRangeValue);
        end;
        end
      else
        RaiseNotYetImplemented(20171009171005,RHS);
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckAssignExprRange LeftResolved=',GetResolverResultDbg(LeftResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20170530095243,RHS);
      end;
  finally
    ReleaseEvalValue(RValue);
    ReleaseEvalValue(LRangeValue);
  end;
end;

procedure TPasResolver.CheckAssignExprRangeToCustom(
  const LeftResolved: TPasResolverResult; RValue: TResEvalValue; RHS: TPasExpr);
begin
  if LeftResolved.BaseType<>btCustom then exit;
  if RValue=nil then exit;
  if RHS=nil then ;
end;

function TPasResolver.CheckAssignResCompatibility(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  LTypeEl, RTypeEl: TPasType;
  Handled: Boolean;
  C: TClass;
  LBT, RBT: TResolverBaseType;
  LRange, RValue, Value: TResEvalValue;
  RightSubResolved: TPasResolverResult;
  wc: WideChar;
begin
  // check if the RHS can be converted to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility START LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
  {$ENDIF}
  Result:=-1;

  Handled:=false;
  Result:=CheckAssignCompatibilityCustom(LHS,RHS,ErrorEl,RaiseOnIncompatible,Handled);
  if Handled and (Result>=cExact) and (Result<cIncompatible) then
    exit;

  if not Handled then
    begin
    LBT:=GetActualBaseType(LHS.BaseType);
    RBT:=GetActualBaseType(RHS.BaseType);
    if LHS.LoTypeEl=nil then
      begin
      if LBT=btUntyped then
        begin
        // untyped parameter
        Result:=cTypeConversion;
        end
      else
        RaiseNotYetImplemented(20160922163631,LHS.IdentEl);
      end
    else if LBT=RBT then
      begin
      if LBT=btContext then
        exit(CheckAssignCompatibilityUserType(LHS,RHS,ErrorEl,RaiseOnIncompatible))
      else
        begin
        // same base type, maybe not same type (e.g. longint and integer)
        if IsSameType(LHS.HiTypeEl,RHS.HiTypeEl,prraSimple)
            and HasExactType(RHS) then
          Result:=cExact
        else
          Result:=cAliasExact;
        end;
      end
    else if (LBT in btAllBooleans)
        and (RBT in btAllBooleans) then
      Result:=cCompatible
    else if (LBT in btAllChars) then
      begin
      if (RBT in btAllChars) then
        case LBT of
        {$ifdef FPC_HAS_CPSTRING}
        btAnsiChar:
          Result:=cLossyConversion;
        {$endif}
        btWideChar:
          {$ifdef FPC_HAS_CPSTRING}
          if RBT=btAnsiChar then
            Result:=cCompatible
          else
          {$endif}
            Result:=cLossyConversion;
        else
          RaiseNotYetImplemented(20170728132440,ErrorEl,BaseTypeNames[LBT]);
        end
      else if (RBT=btRange) and (RHS.SubType in btAllChars) then
        begin
        if LBT=btWideChar then
          exit(cCompatible);
        {$ifdef FPC_HAS_CPSTRING}
        // LHS is ansichar
        if GetActualBaseType(RHS.SubType)=btAnsiChar then
          exit(cExact);
        RValue:=Eval(RHS,[refAutoConstExt]);
        if RValue<>nil then
          try
            // ansichar:=constvalue
            case RValue.Kind of
            revkString:
              if not ExprEvaluator.GetWideChar(TResEvalString(RValue).S,wc) then
                exit(cIncompatible);
            revkUnicodeString:
              begin
              if length(TResEvalUTF16(RValue).S)<>1 then
                exit(cIncompatible);
              wc:=TResEvalUTF16(RValue).S[1];
              end;
            revkExternal:
              exit(cCompatible);
            else
              RaiseNotYetImplemented(20171108194650,ErrorEl);
            end;
            if ord(wc)>255 then
              exit(cIncompatible);
            exit(cCompatible);
          finally
            ReleaseEvalValue(RValue);
          end;
        // LHS is ansichar, RHS is not a const
        if (RHS.ExprEl is TBinaryExpr) and (TBinaryExpr(RHS.ExprEl).Kind=pekRange) then
          begin
          RValue:=Eval(RHS.ExprEl,[refConst]);
          try
            if RValue.Kind<>revkRangeInt then
              RaiseNotYetImplemented(20171108195035,ErrorEl);
            if TResEvalRangeInt(RValue).RangeStart>255 then
              exit(cIncompatible);
            if TResEvalRangeInt(RValue).RangeEnd>255 then
              exit(cLossyConversion);
            exit(cCompatible);
          finally
            ReleaseEvalValue(RValue);
          end;
          end;
        {$endif}
        RaiseNotYetImplemented(20171108195216,ErrorEl);
        end;
      end
    else if (LBT in btAllStrings) then
      begin
      if (RBT in btAllStringAndChars) then
        case LBT of
        {$ifdef FPC_HAS_CPSTRING}
        btAnsiString:
          if RBT in [btAnsiChar,btShortString,btRawByteString] then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        btShortString:
          if RBT=btAnsiChar then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        btRawByteString:
          if RBT in [btAnsiChar,btAnsiString,btShortString] then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        {$endif}
        btWideString,btUnicodeString:
          Result:=cCompatible;
        else
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.CheckAssignResCompatibility ',{$ifdef pas2js}str(LBT){$else}LBT{$ENDIF});
          {$ENDIF}
          RaiseNotYetImplemented(20170417195208,ErrorEl,BaseTypeNames[LBT]);
        end
      else if RBT=btContext then
        begin
        RTypeEl:=RHS.LoTypeEl;
        if RTypeEl.ClassType=TPasClassType then
          begin
          if (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUIDString(LHS) then
            // aGUIDString:=IntfTypeOrVar
            exit(cInterfaceToString);  // no check for rrfReadable
          end
        else if RTypeEl.ClassType=TPasRecordType then
          begin
          if IsTGUID(TPasRecordType(RTypeEl)) then
            // aString:=GUID
            Result:=cTGUIDToString;
          end;
        end;
      end
    else if (LBT in btAllInteger)
        and (RBT in btAllInteger) then
      begin
      Result:=cIntToIntConversion+ord(LBT)-ord(RBT);
      case LBT of
      btByte,
      btShortInt: inc(Result,cLossyConversion);
      btWord,
      btSmallInt:
        if not (RBT in [btByte,btShortInt]) then
          inc(Result,cLossyConversion);
      btUIntSingle:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt]) then
          inc(Result,cLossyConversion);
      btIntSingle:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle]) then
          inc(Result,cLossyConversion);
      btLongWord,
      btLongint:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle,btIntSingle]) then
          inc(Result,cLossyConversion);
      btUIntDouble:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btLongWord,btLongint]) then
          inc(Result,cLossyConversion);
      btIntDouble:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btLongWord,btLongint,btUIntDouble]) then
          inc(Result,cLossyConversion);
      {$ifdef HasInt64}
      btQWord,
      btInt64,btComp:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle,btIntSingle,
            btLongWord,btLongint,btUIntDouble,btIntDouble]) then
          inc(Result,cLossyConversion);
      {$endif}
      else
        RaiseNotYetImplemented(20170417205301,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if (LBT in btAllFloats)
        and (RBT in btAllFloats) then
      begin
      Result:=cFloatToFloatConversion+ord(LBT)-ord(RBT);
      case LBT of
      btSingle:
        if RBT>btSingle then
          inc(Result,cLossyConversion);
      btDouble:
        if RBT>btDouble then
          inc(Result,cLossyConversion);
      btExtended,btCExtended:
        if RBT>btCExtended then
          inc(Result,cLossyConversion);
      btCurrency:
        inc(Result,cLossyConversion);
      else
        RaiseNotYetImplemented(20170417205910,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if (LBT in btAllFloats)
        and (RBT in btAllInteger) then
      begin
      Result:=cIntToFloatConversion+ord(LBT)-ord(RBT);
      case LBT of
      btSingle:
        if RBT>btUIntSingle then
          inc(Result,cLossyConversion);
      btDouble:
        if RBT>btUIntDouble then
          inc(Result,cLossyConversion);
      btExtended,btCExtended:
        if RBT>btCExtended then
          inc(Result,cLossyConversion);
      btCurrency:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,
            btIntSingle,btUIntSingle,
            btLongWord,btLongint]) then
          inc(Result,cLossyConversion);
      else
        RaiseNotYetImplemented(20170417205911,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if LBT=btNil then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152431,nCantAssignValuesToAnAddress,sCantAssignValuesToAnAddress,
          [],ErrorEl);
      exit(cIncompatible);
      end
    else if LBT=btRange then
      begin
      if (LHS.ExprEl is TBinaryExpr) and (TBinaryExpr(LHS.ExprEl).Kind=pekRange) then
        begin
        LRange:=Eval(LHS.ExprEl,[refConst]);
        RValue:=nil;
        try
          {$IFDEF VerbosePasResolver}
          //writeln('TPasResolver.CheckAssignResCompatibility LeftRange=',LRange.AsDebugString);
          {$ENDIF}
          case LRange.Kind of
          revkRangeInt:
            case TResEvalRangeInt(LRange).ElKind of
            revskEnum:
              if RHS.BaseType=btContext then
                begin
                if IsSameType(TResEvalRangeInt(LRange).ElType,RHS.LoTypeEl,prraAlias) then
                  begin
                  // same enum type
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.CheckAssignResCompatibility LeftRange=',LRange.AsDebugString,' Left.ElType=',GetObjName(TResEvalRangeInt(LRange).ElType),' RHS.TypeEl=',GetObjName(RHS.LoTypeEl));
                  {$ENDIF}
                  // ToDo: check if LRange is smaller than Range of RHS (cLossyConversion)
                  exit(cExact);
                  end;
                end;
            revskInt:
              if RHS.BaseType in btAllInteger then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  // ToDo: check range
                  end;
                exit(cCompatible);
                end;
            revskChar:
              if RHS.BaseType in btAllStringAndChars then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  case RValue.Kind of
                  {$ifdef FPC_HAS_CPSTRING}
                  revkString:
                    if not fExprEvaluator.GetWideChar(TResEvalString(RValue).S,wc) then
                      exit(cIncompatible);
                  {$endif}
                  revkUnicodeString:
                    begin
                    if length(TResEvalUTF16(RValue).S)<>1 then
                      exit(cIncompatible);
                    wc:=TResEvalUTF16(RValue).S[1];
                    end;
                  revkExternal:
                    exit(cCompatible);
                  else
                    RaiseNotYetImplemented(20171108192232,ErrorEl);
                  end;
                  if (ord(wc)<TResEvalRangeInt(LRange).RangeStart)
                      or (ord(wc)>TResEvalRangeInt(LRange).RangeEnd) then
                    exit(cIncompatible);
                  end;
                exit(cCompatible);
                end;
            revskBool:
              if RHS.BaseType=btBoolean then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  // ToDo: check range
                  end;
                exit(cCompatible);
                end;
            end;
          end;
        finally
          ReleaseEvalValue(LRange);
          ReleaseEvalValue(RValue);
        end;
        end;
      end
    else if LBT=btSet then
      begin
      if RBT=btArrayOrSet then
        begin
        if RHS.SubType=btNone then
          // a:=[]
          Result:=cExact
        else if IsSameType(LHS.HiTypeEl,RHS.HiTypeEl,prraSimple)
            and HasExactType(RHS) then
          Result:=cExact
        else if LHS.SubType=RHS.SubType then
          Result:=cAliasExact
        else if (LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans) then
          Result:=cCompatible
        else if (LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger) then
          begin
          // ToDo: range check
          Result:=cCompatible;
          end
        else if (LHS.SubType in btAllChars) and (RHS.SubType in btAllChars) then
          begin
          // ToDo: range check
          Result:=cCompatible;
          end;
        end;
      end
    else if LBT in [btArrayLit,btArrayOrSet,btModule,btProc] then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152432,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if (LHS.IdentEl=nil) and (LHS.ExprEl=nil) then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152434,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if RBT=btNil then
      begin
      if LBT=btPointer then
        Result:=cExact
      else if LBT=btContext then
        begin
        LTypeEl:=LHS.LoTypeEl;
        C:=LTypeEl.ClassType;
        if (C=TPasClassType)
            or (C=TPasClassOfType)
            or (C=TPasPointerType)
            or C.InheritsFrom(TPasProcedureType)
            or IsDynArray(LTypeEl) then
          Result:=cExact;
        end;
      end
    else if RBT=btProc then
      begin
      if (msDelphi in CurrentParser.CurrentModeswitches)
          and (LHS.LoTypeEl is TPasProcedureType)
          and (RHS.IdentEl is TPasProcedure) then
        begin
        // for example  ProcVar:=Proc
        if CheckProcTypeCompatibility(TPasProcedureType(LHS.LoTypeEl),
            TPasProcedure(RHS.IdentEl).ProcType,true,ErrorEl,RaiseOnIncompatible) then
          exit(cExact);
        end;
      end
    else if LBT=btPointer then
      begin
      if RBT=btPointer then
        begin
        LTypeEl:=LHS.LoTypeEl;
        RTypeEl:=RHS.LoTypeEl;
        if IsBaseType(LTypeEl,btPointer) then
          Result:=cExact // btPointer can take any pointer
        else if IsBaseType(RTypeEl,btPointer) then
          Result:=cTypeConversion // any pointer can take a btPointer
        else if IsSameType(LTypeEl,RTypeEl,prraAlias) then
          Result:=cExact // pointer of same type
        else if (LTypeEl.ClassType=TPasPointerType)
            and (RTypeEl.ClassType=TPasPointerType) then
          Result:=CheckAssignCompatibility(TPasPointerType(LTypeEl).DestType,
            TPasPointerType(RTypeEl).DestType,RaiseOnIncompatible);
        end
      else if IsBaseType(LHS.LoTypeEl,btPointer) then
        begin
        // UntypedPointer:=...
        if RBT=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          C:=RTypeEl.ClassType;
          if C=TPasClassType then
            // UntypedPointer:=ClassTypeOrInstance
            exit(cTypeConversion)
          else if C=TPasClassOfType then
            // UntypedPointer:=ClassOfVar
            Result:=cTypeConversion
          else if C=TPasArrayType then
            begin
            if IsDynArray(RTypeEl) then
              // UntypedPointer:=DynArray
              Result:=cTypeConversion;
            end
          else if (C=TPasProcedureType) or (C=TPasFunctionType) then
            // UntypedPointer:=procvar
            Result:=cLossyConversion
          else if C=TPasPointerType then
            // UntypedPointer:=TypedPointer
            Result:=cExact;
          end;
        end;
      end
    else if (LBT=btContext) then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if (LTypeEl.ClassType=TPasArrayType) then
        Result:=CheckAssignCompatibilityArrayType(LHS,RHS,ErrorEl,RaiseOnIncompatible)
      else if LTypeEl.ClassType=TPasEnumType then
        begin
        if (RHS.BaseType=btRange) and (RHS.SubType=btContext) then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if RTypeEl.ClassType=TPasRangeType then
            begin
            ComputeElement(TPasRangeType(RTypeEl).RangeExpr.left,RightSubResolved,[rcConstant]);
            if (RightSubResolved.BaseType=btContext)
                and IsSameType(LTypeEl,RightSubResolved.LoTypeEl,prraAlias) then
              begin
              // enumtype := enumrange
              Result:=cExact;
              end;
            end;
          end;
        end
      else if LTypeEl.ClassType=TPasRecordType then
        begin
        if (RBT in btAllStrings) and IsTGUID(TPasRecordType(LTypeEl))
            and (rrfReadable in RHS.Flags) then
          begin
          // GUIDVar := string, e.g. IObjectInstance: TGuid = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}'
          Value:=Eval(RHS,[refConstExt]);
          try
            if Value=nil then
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180414105916,'string literal','string', ErrorEl)
              else
                exit(cIncompatible);
          finally
            ReleaseEvalValue(Value);
          end;
          Result:=cStringToTGUID;
          end;
        end
      else if LTypeEl.ClassType=TPasPointerType then
        begin
        // TypedPointer:=
        if RHS.BaseType=btPointer then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if IsBaseType(RTypeEl,btPointer) then
            // TypedPointer:=UntypedPointer
            Result:=cTypeConversion
          else
            begin
            // TypedPointer:=@Var
            Result:=CheckAssignCompatibilityPointerType(
              TPasPointerType(LTypeEl).DestType,RTypeEl,ErrorEl,false);
            end;
          end;
        end;
      end;
    end;

  if (Result>=0) and (Result<cIncompatible) then
    begin
    // type fits -> check readable
    if not (rrfReadable in RHS.Flags) then
      begin
      if RaiseOnIncompatible then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckAssignResCompatibility RHS not readable. LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
        {$ENDIF}
        RaiseVarExpected(20170318235637,ErrorEl,RHS.IdentEl);
        end;
      exit(cIncompatible);
      end;
    exit;
    end;

  // incompatible
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility incompatible LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
  {$ENDIF}
  if not RaiseOnIncompatible then
    exit(cIncompatible);

  // create error messages
  RaiseIncompatibleTypeRes(20170216152437,nIncompatibleTypesGotExpected,
    [],RHS,LHS,ErrorEl);
end;

function TPasResolver.CheckEqualElCompatibility(Left, Right: TPasElement;
  ErrorEl: TPasElement; RaiseOnIncompatible: boolean; SetReferenceFlags: boolean
  ): integer;
// check if the RightResolved is type compatible to LeftResolved
var
  LFlags, RFlags: TPasResolverComputeFlags;
  LeftResolved, RightResolved: TPasResolverResult;
  LeftErrorEl, RightErrorEl: TPasElement;
begin
  Result:=cIncompatible;
  // Delphi resolves both sides, so it forbids "if procvar=procvar then"
  // FPC is more clever. It supports "if procvar=@proc then", "function=value"
  if msDelphi in CurrentParser.CurrentModeswitches then
    LFlags:=[]
  else
    LFlags:=[rcNoImplicitProcType];
  if SetReferenceFlags then
    Include(LFlags,rcSetReferenceFlags);
  ComputeElement(Left,LeftResolved,LFlags);

  if (msDelphi in CurrentParser.CurrentModeswitches) then
    RFlags:=LFlags
  else
    begin
    if LeftResolved.BaseType=btNil then
      RFlags:=[rcNoImplicitProcType]
    else if IsProcedureType(LeftResolved,true) then
      RFlags:=[rcNoImplicitProcType]
    else
      RFlags:=[];
    end;
  if SetReferenceFlags then
    Include(RFlags,rcSetReferenceFlags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualElCompatibility LFlags=',dbgs(LFlags),' Left=',GetResolverResultDbg(LeftResolved),' Delphi=',msDelphi in CurrentParser.CurrentModeswitches,' RFlags=',dbgs(RFlags));
  {$ENDIF}
  ComputeElement(Right,RightResolved,RFlags);
  if ErrorEl=nil then
    begin
    LeftErrorEl:=Left;
    RightErrorEl:=Right;
    end
  else
    begin
    LeftErrorEl:=ErrorEl;
    RightErrorEl:=ErrorEl;
    end;
  Result:=CheckEqualResCompatibility(LeftResolved,RightResolved,LeftErrorEl,
    RaiseOnIncompatible,RightErrorEl);
end;

function TPasResolver.CheckEqualResCompatibility(const LHS,
  RHS: TPasResolverResult; LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  RErrorEl: TPasElement): integer;
var
  LTypeEl, RTypeEl: TPasType;
  ResolvedEl: TPasResolverResult;
begin
  Result:=cIncompatible;
  if RErrorEl=nil then RErrorEl:=LErrorEl;
  // check if the RHS is type compatible to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualResCompatibility LHS=',GetResolverResultDbg(LHS),' RHS=',GetResolverResultDbg(RHS));
  {$ENDIF}
  if not (rrfReadable in LHS.Flags) then
    begin
    if (LHS.BaseType=btContext) then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if (LTypeEl.ClassType=TPasClassType)
          and (ResolveAliasTypeEl(LHS.IdentEl)=LTypeEl) then
        begin
        // LHS is class type, e.g. TObject or IInterface
        if RHS.BaseType=btNil then
          exit(cExact)
        else if RHS.BaseType in btAllStrings then
          begin
          if (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okInterface)
              and IsTGUIDString(RHS) then
            // e.g.  IUnknown=aGUIDString
            exit(cInterfaceToString);
          end
        else if (RHS.BaseType=btContext) then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasClassOfType)
              and (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okClass) then
            // for example  if TImage=ImageClass then
            exit(cExact)
          else if (RTypeEl.ClassType=TPasRecordType)
              and (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okInterface)
              and IsTGUID(TPasRecordType(RTypeEl)) then
            // e.g.  if IUnknown=TGuidVar then
            exit(cInterfaceToTGUID);
          end;
        end;
      end;
    RaiseMsg(20170216152438,nNotReadable,sNotReadable,[],LErrorEl);
    end;
  if not (rrfReadable in RHS.Flags) then
    begin
    if (RHS.BaseType=btContext) then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if (RTypeEl.ClassType=TPasClassType)
          and (ResolveAliasTypeEl(RHS.IdentEl)=RTypeEl) then
        begin
        // RHS is class type, e.g. TObject or IInterface
        if LHS.BaseType=btNil then
          exit(cExact)
        else if LHS.BaseType in btAllStrings then
          begin
          if (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUIDString(LHS) then
            // e.g.  aGUIDString=IUnknown
            exit(cInterfaceToString);
          end
        else if (LHS.BaseType=btContext) then
          begin
          LTypeEl:=LHS.LoTypeEl;
          if (LTypeEl.ClassType=TPasClassOfType)
              and (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okClass) then
            // for example  if ImageClass=TImage then
            exit(cExact)
          else if (LTypeEl.ClassType=TPasRecordType)
              and (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUID(TPasRecordType(LTypeEl)) then
            // e.g.  if TGuidVar=IUnknown then
            exit(cInterfaceToTGUID);
          end;
        end;
      end;
    RaiseMsg(20170216152440,nNotReadable,sNotReadable,[],RErrorEl);
    end;

  if (LHS.BaseType=btCustom) or (RHS.BaseType=btCustom) then
    begin
    Result:=CheckEqualCompatibilityCustomType(LHS,RHS,LErrorEl,RaiseOnIncompatible);
    if (Result=cIncompatible) and RaiseOnIncompatible then
      RaiseIncompatibleTypeRes(20170330010727,nIncompatibleTypesGotExpected,
        [],RHS,LHS,LErrorEl);
    exit;
    end
  else if LHS.BaseType=RHS.BaseType then
    begin
    if LHS.BaseType=btContext then
      exit(CheckEqualCompatibilityUserType(LHS,RHS,LErrorEl,RaiseOnIncompatible))
    else
      exit(cExact); // same base type, maybe not same type name (e.g. longint and integer)
    end
  else if LHS.BaseType in btAllInteger then
    begin
    if RHS.BaseType in btAllInteger+btAllFloats then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllInteger) then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllFloats then
    begin
    if RHS.BaseType in btAllInteger+btAllFloats then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllBooleans then
    begin
    if RHS.BaseType in btAllBooleans then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllBooleans) then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllStringAndChars then
    begin
    if RHS.BaseType in btAllStringAndChars then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllChars) then
      exit(cCompatible)
    else if RHS.BaseType=btContext then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if (RTypeEl.ClassType=TPasClassType) then
        begin
        if (TPasClassType(RTypeEl).ObjKind=okInterface)
            and IsTGUIDString(LHS) then
          // e.g. aGUIDString=IntfVar
          exit(cInterfaceToString);
        end
      else if (RTypeEl.ClassType=TPasRecordType)
          and IsTGUID(TPasRecordType(RTypeEl)) then
        // e.g. aString=GuidVar
        exit(cTGUIDToString);
      end;
    end
  else if LHS.BaseType=btNil then
    begin
      if RHS.BaseType in [btPointer,btNil] then
        exit(cExact)
      else if RHS.BaseType=btContext then
        begin
        LTypeEl:=RHS.LoTypeEl;
        if (LTypeEl.ClassType=TPasClassType)
            or (LTypeEl.ClassType=TPasClassOfType)
            or (LTypeEl.ClassType=TPasPointerType)
            or (LTypeEl is TPasProcedureType)
            or IsDynArray(LTypeEl) then
          exit(cExact);
        end;
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152442,nIncompatibleTypesGotExpected,
          [],RHS,LHS,RErrorEl)
      else
        exit(cIncompatible);
    end
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        LTypeEl:=LHS.LoTypeEl;
        if (LTypeEl.ClassType=TPasClassType)
            or (LTypeEl.ClassType=TPasClassOfType)
            or (LTypeEl.ClassType=TPasPointerType)
            or (LTypeEl is TPasProcedureType)
            or IsDynArray(LTypeEl) then
          exit(cExact);
        end;
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152444,nIncompatibleTypesGotExpected,
          [],LHS,RHS,LErrorEl)
      else
        exit(cIncompatible);
    end
  else if LHS.BaseType=btPointer then
    begin
    if RHS.BaseType=btContext then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if RTypeEl.ClassType=TPasPointerType then
        // @Something=TypedPointer
        exit(cExact)
      else if RTypeEl.ClassType=TPasClassType then
        // @Something=ClassOrInterface
        exit(cCompatible)
      else if RTypeEl.ClassType=TPasClassOfType then
        // @Something=ClassOf
        exit(cCompatible);
      end;
    end
  else if LHS.BaseType in [btSet,btArrayOrSet] then
    begin
    if RHS.BaseType in [btSet,btArrayOrSet] then
      begin
      if LHS.LoTypeEl=nil then
        exit(cExact); // empty set
      if RHS.LoTypeEl=nil then
        exit(cExact); // empty set
      if IsSameType(LHS.LoTypeEl,RHS.LoTypeEl,prraAlias) then
        exit(cExact);
      if (LHS.SubType=RHS.SubType) and (LHS.SubType in (btAllBooleans+btAllInteger+btAllChars)) then
        exit(cExact);
      if ((LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans))
          or ((LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger)) then
        exit(cCompatible);
      if RaiseOnIncompatible then
        RaiseMsg(20170216152446,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[LHS.SubType],'set of '+BaseTypeNames[RHS.SubType]],LErrorEl)
      else
        exit(cIncompatible);
      end;
    end
  else if LHS.BaseType=btRange then
    begin
    if LHS.SubType in btAllInteger then
      begin
      // e.g. 2..4
      if RHS.BaseType in btAllInteger then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllInteger) then
        exit(cCompatible);
      end
    else if LHS.SubType in btAllBooleans then
      begin
      if RHS.BaseType in btAllBooleans then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllBooleans) then
        exit(cCompatible);
      end
    else if LHS.SubType in btAllChars then
      begin
      if RHS.BaseType in btAllStringAndChars then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllChars) then
        exit(cCompatible);
      end
    else if LHS.SubType=btContext then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if LTypeEl.ClassType=TPasRangeType then
        begin
        ComputeElement(TPasRangeType(LTypeEl).RangeExpr.left,ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btContext then
          begin
          LTypeEl:=ResolvedEl.LoTypeEl;
          if LTypeEl.ClassType=TPasEnumType then
            begin
            if RHS.BaseType=btContext then
              begin
              RTypeEl:=RHS.LoTypeEl;
              if (LTypeEl=RTypeEl) then
                exit(cCompatible);
              end;
            end;
          end;
        end;
      end;
    end
  else if LHS.BaseType=btContext then
    begin
    LTypeEl:=LHS.LoTypeEl;
    if LTypeEl.ClassType=TPasEnumType then
      begin
      if RHS.BaseType=btRange then
        begin
        RTypeEl:=RHS.LoTypeEl;
        if RTypeEl.ClassType=TPasRangeType then
          begin
          ComputeElement(TPasRangeType(RTypeEl).RangeExpr.left,ResolvedEl,[rcConstant]);
          if ResolvedEl.BaseType=btContext then
            begin
            RTypeEl:=ResolvedEl.LoTypeEl;
            if LTypeEl=RTypeEl then
              exit(cCompatible);
            end;
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasClassType then
      begin
      if RHS.BaseType=btPointer then
        exit(cCompatible)
      else if TPasClassType(LTypeEl).ObjKind=okInterface then
        begin
        if RHS.BaseType in btAllStrings then
          begin
          if IsTGUIDString(RHS) then
            // e.g. IntfVar=aGUIDString
            exit(cInterfaceToString);
          end
        else if RHS.BaseType=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasRecordType)
              and IsTGUID(TPasRecordType(RTypeEl)) then
            // e.g. IntfVar=GuidVar
            exit(cInterfaceToTGUID);
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasClassOfType then
      begin
      if RHS.BaseType=btPointer then
        exit(cCompatible);
      end
    else if LTypeEl.ClassType=TPasRecordType then
      begin
      if IsTGUID(TPasRecordType(LTypeEl)) then
        begin
        // LHS is TGUID
        if (RHS.BaseType in btAllStrings) then
          // GuidVar=aString
          exit(cTGUIDToString)
        else if RHS.BaseType=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasClassType)
              and (TPasClassType(RTypeEl).ObjKind=okInterface) then
            // GUIDVar=IntfVar
            exit(cInterfaceToTGUID);
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasPointerType then
      begin
      if RHS.BaseType=btPointer then
        // TypedPointer=@Something
        exit(cExact);
      end;
    end;
  if RaiseOnIncompatible then
    RaiseIncompatibleTypeRes(20170216152449,nIncompatibleTypesGotExpected,
      [],RHS,LHS,RErrorEl)
  else
    exit(cIncompatible);
end;

function TPasResolver.IsVariableConst(El, PosEl: TPasElement;
  RaiseIfConst: boolean): boolean;
var
  CurEl: TPasElement;
  VarResolved: TPasResolverResult;
  Loop: TPasImplForLoop;
begin
  Result:=false;
  CurEl:=PosEl;
  while CurEl<>nil do
    begin
    if (CurEl.ClassType=TPasImplForLoop) then
      begin
      Loop:=TPasImplForLoop(CurEl);
      if (Loop.VariableName<>PosEl) then
        begin
        ComputeElement(Loop.VariableName,VarResolved,[rcNoImplicitProc]);
        if VarResolved.IdentEl=El then
          begin
          if RaiseIfConst then
            RaiseMsg(20180430100719,nIllegalAssignmentToForLoopVar,
              sIllegalAssignmentToForLoopVar,[El.Name],PosEl);
          exit(true);
          end;
        end;
      end;
    CurEl:=CurEl.Parent;
    end;
end;

function TPasResolver.ResolvedElCanBeVarParam(
  const ResolvedEl: TPasResolverResult; PosEl: TPasElement;
  RaiseIfConst: boolean): boolean;

  function NotLocked(El: TPasElement): boolean;
  begin
    Result:=not IsVariableConst(El,PosEl,RaiseIfConst);
  end;

var
  IdentEl: TPasElement;
begin
  Result:=false;
  if [rrfReadable,rrfWritable]*ResolvedEl.Flags<>[rrfReadable,rrfWritable] then
    exit;
  if ResolvedEl.IdentEl=nil then
    exit(true);

  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl.ClassType=TPasVariable then
    exit(NotLocked(IdentEl));
  if (IdentEl.ClassType=TPasConst) then
    begin
    if TPasConst(IdentEl).IsConst then
      begin
      if RaiseIfConst then
        RaiseMsg(20180430100719,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],PosEl);
      exit(false);
      end;
    exit(NotLocked(IdentEl));
    end;
  if (IdentEl.ClassType=TPasArgument) then
    begin
    if TPasArgument(IdentEl).Access in [argConst,argConstRef] then
      begin
      if RaiseIfConst then
        RaiseMsg(20180430100843,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],PosEl);
      exit(false);
      end;
    Result:=(TPasArgument(IdentEl).Access in [argDefault, argVar, argOut]);
    exit(Result and NotLocked(IdentEl));
    end;
  if IdentEl.ClassType=TPasResultElement then
    exit(NotLocked(IdentEl));
  if (proPropertyAsVarParam in Options)
      and (IdentEl.ClassType=TPasProperty) then
    exit(NotLocked(IdentEl));
end;

function TPasResolver.ResolvedElIsClassInstance(
  const ResolvedEl: TPasResolverResult): boolean;
var
  TypeEl: TPasType;
begin
  Result:=false;
  if ResolvedEl.BaseType<>btContext then exit;
  TypeEl:=ResolvedEl.LoTypeEl;
  if TypeEl=nil then exit;
  if TypeEl.ClassType<>TPasClassType then exit;
  if TPasClassType(TypeEl).ObjKind<>okClass then exit;
  if (ResolvedEl.IdentEl is TPasVariable)
      or (ResolvedEl.IdentEl.ClassType=TPasArgument)
      or (ResolvedEl.IdentEl.ClassType=TPasResultElement) then
    exit(true);
end;

function TPasResolver.GetElModeSwitches(El: TPasElement): TModeSwitches;
var
  C: TClass;
begin
  while El<>nil do
    begin
    if El.CustomData<>nil then
      begin
      C:=El.CustomData.ClassType;
      if C.InheritsFrom(TPasProcedureScope) then
        exit(TPasProcedureScope(El.CustomData).ModeSwitches)
      else if C.InheritsFrom(TPasSectionScope) then
        exit(TPasSectionScope(El.CustomData).ModeSwitches);
      end;
    El:=El.Parent;
    end;
  Result:=CurrentParser.CurrentModeswitches;
end;

function TPasResolver.GetElBoolSwitches(El: TPasElement): TBoolSwitches;
var
  C: TClass;
begin
  Result:=CurrentParser.Scanner.CurrentBoolSwitches;
  while El<>nil do
    begin
    if El.CustomData<>nil then
      begin
      C:=El.CustomData.ClassType;
      if C.InheritsFrom(TPasProcedureScope) then
        exit(TPasProcedureScope(El.CustomData).BoolSwitches)
      else if C.InheritsFrom(TPasSectionScope) then
        exit(TPasSectionScope(El.CustomData).BoolSwitches)
      else if C.InheritsFrom(TPasModuleScope) then
        exit(TPasModuleScope(El.CustomData).BoolSwitches);
      end;
    El:=El.Parent;
    end;
end;

function TPasResolver.GetProcTypeDescription(ProcType: TPasProcedureType;
  Flags: TPRProcTypeDescFlags): string;
var
  Args: TFPList;
  i: Integer;
  Arg: TPasArgument;
  ArgType: TPasType;
begin
  if ProcType=nil then exit('nil');
  Result:=ProcType.TypeName;
  if ProcType.IsReferenceTo then
    Result:=ProcTypeModifiers[ptmReferenceTo]+' '+Result;
  if (prptdUseName in Flags) and (ProcType.Parent is TPasProcedure) then
    begin
    if prptdAddPaths in Flags then
      Result:=Result+' '+ProcType.Parent.FullName
    else
      Result:=Result+' '+ProcType.Parent.Name;
    end;
  Args:=ProcType.Args;
  if Args.Count>0 then
    begin
    Result:=Result+'(';
    for i:=0 to Args.Count-1 do
      begin
      if i>0 then Result:=Result+';';
      Arg:=TPasArgument(Args[i]);
      if AccessNames[Arg.Access]<>'' then
        Result:=Result+AccessNames[Arg.Access];
      if Arg.ArgType=nil then
        Result:=Result+'untyped'
      else
        begin
        ArgType:=Arg.ArgType;
        if prptdResolveSimpleAlias in Flags then
          ArgType:=ResolveSimpleAliasType(ArgType);
        Result:=Result+GetTypeDescription(ArgType,prptdAddPaths in Flags);
        end;
      end;
    Result:=Result+')';
    end;
  if ProcType.IsOfObject then
    Result:=Result+' '+ProcTypeModifiers[ptmOfObject];
  if ProcType.IsNested then
    Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
  if cCallingConventions[ProcType.CallingConvention]<>'' then
    Result:=Result+';'+cCallingConventions[ProcType.CallingConvention];
end;

function TPasResolver.GetResolverResultDescription(const T: TPasResolverResult;
  OnlyType: boolean): string;

  function GetSubTypeName: string;
  begin
    if (T.LoTypeEl<>nil) and (T.LoTypeEl.Name<>'') then
      Result:=T.LoTypeEl.Name
    else
      Result:=BaseTypeNames[T.SubType];
  end;

var
  ArrayEl: TPasArrayType;
begin
  case T.BaseType of
  btModule: exit(GetElementTypeName(T.IdentEl)+' '+T.IdentEl.Name);
  btNil: exit('nil');
  btRange:
    Result:='range of '+GetSubTypeName;
  btSet:
    Result:='set of '+GetSubTypeName;
  btArrayLit:
    Result:='array of '+GetSubTypeName;
  btArrayOrSet:
    Result:='set/array literal of '+GetSubTypeName;
  btContext:
    begin
    if T.LoTypeEl.ClassType=TPasClassOfType then
      Result:='class of '+TPasClassOfType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasAliasType then
      Result:=TPasAliasType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasTypeAliasType then
      Result:='type '+TPasAliasType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasArrayType then
      begin
      ArrayEl:=TPasArrayType(T.LoTypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        Result:='array of '+ArrayEl.ElType.Name;
        if IsOpenArray(ArrayEl) then
          Result:='open '+Result;
        end
      else
        Result:='static array[] of '+ArrayEl.ElType.Name;
      end
    else if T.LoTypeEl is TPasProcedureType then
      Result:=GetProcTypeDescription(TPasProcedureType(T.LoTypeEl),[])
    else if T.LoTypeEl.Name<>'' then
      Result:=T.LoTypeEl.Name
    else
      Result:=T.LoTypeEl.ElementTypeName;
    end;
  btCustom:
    Result:=T.LoTypeEl.Name;
  else
    Result:=BaseTypeNames[T.BaseType];
  end;
  if (not OnlyType) and (T.LoTypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
    Result:=T.IdentEl.Name+':'+Result;
end;

function TPasResolver.GetTypeDescription(aType: TPasType; AddPath: boolean): string;

  function GetName: string;
  var
    s: String;
  begin
    Result:=aType.Name;
    if Result='' then
      begin
      if aType is TPasArrayType then
        begin
        if length(TPasArrayType(aType).Ranges)>0 then
          Result:='static array'
        else if IsOpenArray(aType) then
          Result:='open array'
        else
          Result:='dynamic array';
        end
      else
        Result:=GetElementTypeName(aType);
      end;
    if AddPath then
      begin
      s:=aType.ParentPath;
      if (s<>'') and (s<>'.') then
        Result:=s+'.'+Result;
      end;
  end;

begin
  if aType=nil then exit('untyped');
  Result:=GetName;
  if (aType.ClassType=TPasUnresolvedSymbolRef) then
    begin
    if TPasUnresolvedSymbolRef(aType).CustomData is TResElDataBuiltInProc then
      Result:=Result+'()';
    exit;
    end;
end;

function TPasResolver.GetTypeDescription(const R: TPasResolverResult;
  AddPath: boolean): string;
var
  s: String;
begin
  Result:=GetTypeDescription(R.LoTypeEl,AddPath);
  if R.BaseType in [btSet,btArrayLit,btArrayOrSet] then
    Result:=BaseTypeNames[R.BaseType]+' of '+Result;
  if (R.LoTypeEl<>nil) and (R.IdentEl=R.LoTypeEl) then
    begin
    s:=GetElementTypeName(R.LoTypeEl);
    if s<>'' then
      Result:=s+' '+Result
    else
      Result:='type '+Result;
    end;
end;

function TPasResolver.GetBaseDescription(const R: TPasResolverResult;
  AddPath: boolean): string;
begin
  if R.BaseType=btContext then
    Result:=GetTypeDescription(R,AddPath)
  else if (R.BaseType=btPointer) and not IsBaseType(R.LoTypeEl,btPointer) then
    Result:='^'+GetTypeDescription(R,AddPath)
  else
    Result:=BaseTypeNames[R.BaseType];
end;

function TPasResolver.GetProcFirstImplEl(Proc: TPasProcedure): TPasImplElement;
var
  Scope: TPasProcedureScope;
  Body: TPasImplBlock;
begin
  Result:=nil;
  if Proc=nil then exit;
  if Proc.Body<>nil then
    Body:=Proc.Body.Body
  else
    Body:=nil;
  if Body=nil then
    begin
    if Proc.CustomData=nil then exit;
    Scope:=Proc.CustomData as TPasProcedureScope;
    Proc:=Scope.ImplProc;
    if Proc=nil then exit;
    if Proc.Body=nil then exit;
    Body:=Proc.Body.Body;
    if Body=nil then exit;
    end;
  if Body.Elements=nil then exit;
  if Body.Elements.Count=0 then exit;
  Result:=TPasImplElement(Body.Elements[0]);
end;

function TPasResolver.GetPasPropertyAncestor(El: TPasProperty;
  WithRedeclarations: boolean): TPasProperty;
begin
  Result:=nil;
  if El=nil then exit;
  if (not WithRedeclarations) and (El.VarType<>nil) then exit;
  if El.CustomData=nil then exit;
  Result:=TPasPropertyScope(El.CustomData).AncestorProp;
end;

function TPasResolver.GetPasPropertyType(El: TPasProperty): TPasType;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.VarType<>nil then
      exit(El.VarType);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyArgs(El: TPasProperty): TFPList;
begin
  while El<>nil do
    begin
    if El.VarType<>nil then
      exit(El.Args);
    El:=GetPasPropertyAncestor(El);
    end;
  Result:=nil;
end;

function TPasResolver.GetPasPropertyGetter(El: TPasProperty): TPasElement;
// search the member variable or getter function of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.ReadAccessor<>nil then
      begin
      DeclEl:=(El.ReadAccessor.CustomData as TResolvedReference).Declaration;
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertySetter(El: TPasProperty): TPasElement;
// search the member variable or setter procedure of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.WriteAccessor<>nil then
      begin
      DeclEl:=(El.WriteAccessor.CustomData as TResolvedReference).Declaration;
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyIndex(El: TPasProperty): TPasExpr;
// search the index expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.IndexExpr<>nil then
      begin
      Result:=El.IndexExpr;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyStoredExpr(El: TPasProperty): TPasExpr;
// search the stored expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.StoredAccessor<>nil then
      begin
      Result:=El.StoredAccessor;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyDefaultExpr(El: TPasProperty): TPasExpr;
// search the stored expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.DefaultExpr<>nil then
      begin
      Result:=El.DefaultExpr;
      exit;
      end
    else if El.IsNodefault then
      exit(nil);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.CheckParamCompatibility(Expr: TPasExpr;
  Param: TPasArgument; ParamNo: integer; RaiseOnError: boolean;
  SetReferenceFlags: boolean): integer;
var
  ExprResolved, ParamResolved: TPasResolverResult;
  NeedVar, UseAssignError: Boolean;
  RHSFlags: TPasResolverComputeFlags;
begin
  Result:=cIncompatible;

  NeedVar:=Param.Access in [argVar, argOut];

  ComputeElement(Param,ParamResolved,[]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Param=',GetTreeDbg(Param,2),' ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  if (ParamResolved.LoTypeEl=nil) and (Param.ArgType<>nil) then
    RaiseInternalError(20160922163628,'GetResolvedType returned TypeEl=nil for '+GetTreeDbg(Param));

  RHSFlags:=[];
  if NeedVar then
    Include(RHSFlags,rcNoImplicitProc)
  else if IsProcedureType(ParamResolved,true)
      or (ParamResolved.BaseType=btPointer)
      or (Param.ArgType=nil)  then
    Include(RHSFlags,rcNoImplicitProcType);
  if SetReferenceFlags then
    Include(RHSFlags,rcSetReferenceFlags);
  ComputeElement(Expr,ExprResolved,RHSFlags);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Expr=',GetTreeDbg(Expr,2),' ResolvedExpr=',GetResolverResultDbg(ExprResolved),' RHSFlags=',dbgs(RHSFlags));
  {$ENDIF}

  if NeedVar then
    begin
    // Expr must be a variable
    if not ResolvedElCanBeVarParam(ExprResolved,Expr) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckParamCompatibility NeedWritable: ',GetResolverResultDbg(ExprResolved));
      {$ENDIF}
      if RaiseOnError then
        begin
        if ExprResolved.IdentEl is TPasConst then
          RaiseMsg(20180430012609,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],Expr)
        else
          RaiseVarExpected(20180430012457,Expr,ExprResolved.IdentEl);
        end;
      exit;
      end;
    if (ParamResolved.BaseType=ExprResolved.BaseType) then
      begin
      if msDelphi in CurrentParser.CurrentModeswitches then
        begin
        if IsSameType(ParamResolved.HiTypeEl,ExprResolved.HiTypeEl,prraSimple) then
          exit(cExact);
        end
      else if IsSameType(ParamResolved.LoTypeEl,ExprResolved.LoTypeEl,prraNone) then
        exit(cExact);
      end;
    if (Param.ArgType=nil) then
      exit(cExact); // untyped argument
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152452,nIncompatibleTypeArgNoVarParamMustMatchExactly,
        [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,
        Expr);
    exit(cIncompatible);
    end;

  UseAssignError:=false;
  if RaiseOnError and (ExprResolved.BaseType in [btArrayLit,btArrayOrSet]) then
    // e.g. Call([1,2]) -> on mismatch jump to the wrong param expression
    UseAssignError:=true;

  Result:=CheckAssignResCompatibility(ParamResolved,ExprResolved,Expr,UseAssignError);
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleTypeRes(20170216152454,nIncompatibleTypeArgNo,
      [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,Expr);

  if SetReferenceFlags and (ParamResolved.BaseType=btContext)
      and (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
    MarkArrayExprRecursive(Expr,TPasArrayType(ParamResolved.LoTypeEl));
end;

function TPasResolver.CheckAssignCompatibilityUserType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  RTypeEl, LTypeEl: TPasType;
  SrcResolved, DstResolved: TPasResolverResult;
  LArray, RArray: TPasArrayType;
  GotDesc, ExpDesc: String;

  function RaiseIncompatType: integer;
  begin
    Result:=cIncompatible;
    if not RaiseOnIncompatible then exit;
    RaiseIncompatibleTypeRes(20170216152505,nIncompatibleTypesGotExpected,
      [],RHS,LHS,ErrorEl);
  end;

begin
  if (RHS.LoTypeEl=nil) then
    RaiseInternalError(20160922163645);
  if (LHS.LoTypeEl=nil) then
    RaiseInternalError(20160922163648);
  LTypeEl:=LHS.LoTypeEl;
  RTypeEl:=RHS.LoTypeEl;
  // Note: do not check if LHS is writable, because this method is used for 'const' too.
  if (LTypeEl=RTypeEl) and (rrfReadable in RHS.Flags) then
    exit(cExact);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignCompatibilityUserType LTypeEl=',GetObjName(LTypeEl),' RTypeEl=',GetObjName(RTypeEl));
  {$ENDIF}
  Result:=-1;
  if LTypeEl.ClassType=TPasClassType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if RTypeEl.ClassType=TPasClassType then
      begin
      Result:=cIncompatible;
      if not (rrfReadable in RHS.Flags) then
        exit(RaiseIncompatType);
      if TPasClassType(LTypeEl).ObjKind=TPasClassType(RTypeEl).ObjKind then
        Result:=CheckSrcIsADstType(RHS,LHS,ErrorEl)
      else if TPasClassType(LTypeEl).ObjKind=okInterface then
        begin
        if (TPasClassType(RTypeEl).ObjKind=okClass)
            and (not TPasClassType(RTypeEl).IsExternal) then
          begin
          // IntfVar:=ClassInstVar
          if GetClassImplementsIntf(TPasClassType(RTypeEl),TPasClassType(LTypeEl))<>nil then
            exit(cTypeConversion);
          end;
        end;
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleType(20170216152458,nIncompatibleTypesGotExpected,
          [],RTypeEl,LTypeEl,ErrorEl);
      end
    else
      exit(RaiseIncompatType);
    end
  else if LTypeEl.ClassType=TPasClassOfType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if (RTypeEl.ClassType=TPasClassOfType) then
      begin
      if RHS.IdentEl is TPasType then
        begin
        Result:=cIncompatible;
        if RaiseOnIncompatible then
          begin
          if ResolveAliasType(TPasType(RHS.IdentEl)) is TPasClassOfType then
            RaiseMsg(20180317103206,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
              ['type class-of','class of '+TPasClassOfType(LTypeEl).DestType.Name],ErrorEl)
          else
            RaiseMsg(20180511123859,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
              [GetResolverResultDescription(RHS),'class of '+TPasClassOfType(LTypeEl).DestType.Name],ErrorEl)
          end;
        end
      else
        begin
        // e.g. ImageClass:=AnotherImageClass;
        Result:=CheckClassIsClass(TPasClassOfType(RTypeEl).DestType,
          TPasClassOfType(LTypeEl).DestType,ErrorEl);
        if (Result=cIncompatible) and RaiseOnIncompatible then
          RaiseMsg(20170216152500,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
            ['class of '+TPasClassOfType(RTypeEl).DestType.PathName,'class of '+TPasClassOfType(LTypeEl).DestType.PathName],ErrorEl);
        end;
      end
    else if (RHS.IdentEl is TPasType)
        and (ResolveAliasType(TPasType(RHS.IdentEl)).ClassType=TPasClassType) then
      begin
      // e.g. ImageClass:=TFPMemoryImage;
      Result:=CheckClassIsClass(RTypeEl,TPasClassOfType(LTypeEl).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152501,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [RTypeEl.Name,'class of '+TPasClassOfType(LTypeEl).DestType.PathName],ErrorEl);
      // do not check rrfReadable -> exit
      exit;
      end;
    end
  else if LTypeEl is TPasProcedureType then
    begin
    if RHS.BaseType=btNil then
      exit(cExact);
    //writeln('TPasResolver.CheckAssignCompatibilityUserType LTypeEl=',GetObjName(LTypeEl),' RHS.BaseType=',BaseTypeNames[RHS.BaseType],' RTypeEl=',GetObjName(RTypeEl),' RHS.IdentEl=',GetObjName(RHS.IdentEl),' RHS.ExprEl=',GetObjName(RHS.ExprEl),' rrfReadable=',rrfReadable in RHS.Flags);
    if (LTypeEl.ClassType=RTypeEl.ClassType)
        and (rrfReadable in RHS.Flags) then
      begin
      // e.g. ProcVar1:=ProcVar2
      if CheckProcTypeCompatibility(TPasProcedureType(LTypeEl),TPasProcedureType(RTypeEl),
          true,ErrorEl,RaiseOnIncompatible) then
        exit(cExact);
      end;
    if RaiseOnIncompatible then
      begin
      if (RTypeEl is TPasProcedureType) and (rrfReadable in RHS.Flags) then
        RaiseMsg(20170404154738,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [GetElementTypeName(RTypeEl),GetElementTypeName(LTypeEl)],ErrorEl);
      end;
    end
  else if LTypeEl.ClassType=TPasArrayType then
    begin
    LArray:=TPasArrayType(LTypeEl);
    if (length(LArray.Ranges)=0) and (RTypeEl.ClassType=TPasArrayType) then
      begin
      // DynOrOpenArr:=array
      RArray:=TPasArrayType(RTypeEl);
      if length(RArray.Ranges)>1 then
        begin
        // DynOrOpenArr:=MultiDimStaticArr  -> no
        if RaiseOnIncompatible then
          RaiseIncompatibleTypeDesc(20180620115235,nIncompatibleTypesGotExpected,
            [],'multi dimensional static array','dynamic array',ErrorEl);
        exit(cIncompatible);
        end
      else if length(RArray.Ranges)>0 then
        begin
        // DynOrOpenArr:=SingleDimStaticArr
        if (msDelphi in CurrentParser.CurrentModeswitches)
            and not IsOpenArray(LArray) then
          begin
          // DynArr:=SingleDimStaticArr  forbidden in Delphi
          // Note: OpenArr:=StaticArr is allowed in Delphi
          if RaiseOnIncompatible then
            RaiseIncompatibleTypeDesc(20180620115341,nIncompatibleTypesGotExpected,
              [],'static array','dynamic array',ErrorEl);
          exit(cIncompatible);
          end;
        end
      else if not (proOpenAsDynArrays in Options) then
        begin
        if IsOpenArray(LArray) then
          // OpenArray:=OpenOrDynArr -> ok
        else if IsOpenArray(RArray) then
          begin
          // DynArray:=OpenArray
          if RaiseOnIncompatible then
            RaiseIncompatibleTypeDesc(20180620115515,nIncompatibleTypesGotExpected,
              [],'open array','dynamic array',ErrorEl);
          exit(cIncompatible)
          end
        else
          begin
          // DynArray:=DynArr
          if (msDelphi in CurrentParser.CurrentModeswitches)
              and (LArray<>RArray) then
            begin
            // Delphi does not allow assigning arrays with same element types
            if RaiseOnIncompatible then
              RaiseIncompatibleTypeRes(20180620115515,nIncompatibleTypesGotExpected,
                [],RHS,LHS,ErrorEl);
            exit(cIncompatible);
            end;
          end;
        end;

      // check element type
      if CheckElTypeCompatibility(LArray.ElType,RArray.ElType,prraAlias) then
        Result:=cExact
      else if RaiseOnIncompatible then
        begin
        GetIncompatibleTypeDesc(LArray.ElType,RArray.ElType,GotDesc,ExpDesc);
        RaiseMsg(20170328110050,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['array of '+GotDesc,
           'array of '+ExpDesc],ErrorEl)
        end
      else
        exit(cIncompatible);
      end;
    end
  else if LTypeEl.ClassType=TPasRecordType then
    begin
    if (RTypeEl is TPasClassType) and (TPasClassType(RTypeEl).ObjKind=okInterface)
        and IsTGUID(TPasRecordType(LTypeEl)) then
      begin
      // GUIDVar := IntfTypeOrVar
      exit(cInterfaceToTGUID);
      end;
    // records of different type
    end
  else if LTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    end
  else if RTypeEl.ClassType=TPasSetType then
    begin
    // sets of different type are compatible if enum types are compatible
    if LTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(LTypeEl).EnumType,DstResolved,[]);
      ComputeElement(TPasSetType(RTypeEl).EnumType,SrcResolved,[]);
      if (SrcResolved.LoTypeEl<>nil)
      and (SrcResolved.LoTypeEl=DstResolved.LoTypeEl) then
        Result:=cExact
      else if (SrcResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (DstResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (CompareText(SrcResolved.LoTypeEl.Name,DstResolved.LoTypeEl.Name)=0) then
        Result:=cExact
      else if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152510,nIncompatibleTypesGotExpected,
          [],SrcResolved,DstResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(RaiseIncompatType);
    end
  else if LTypeEl.ClassType=TPasPointerType then
    begin
    if RTypeEl.ClassType=TPasPointerType then
      begin
      // TypedPointer:=TypedPointer
      Result:=CheckAssignCompatibilityPointerType(TPasPointerType(LTypeEl).DestType,
        TPasPointerType(RTypeEl).DestType,ErrorEl,false);
      if Result=cIncompatible then
        exit(RaiseIncompatType);
      end;
    end
  else
    {$IFDEF VerbosePasResolver}
    RaiseNotYetImplemented(20160922163654,ErrorEl);
    {$ELSE}
    ;
    {$ENDIF}

  if Result=-1 then
    exit(RaiseIncompatType);
  if not (rrfReadable in RHS.Flags) then
    exit(RaiseIncompatType);
end;

function TPasResolver.CheckAssignCompatibilityArrayType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;

  procedure Check_ArrayOfChar_String(ArrType: TPasArrayType;
    ArrLength: integer; const ElTypeResolved: TPasResolverResult;
    Expr: TPasExpr; ErrorEl: TPasElement);
  // check if assigning a string to an array of char fits
  var
    Value: TResEvalValue;
    ElBT: TResolverBaseType;
    l: Integer;
    S: String;
    {$ifdef FPC_HAS_CPSTRING}
    US: UnicodeString;
    {$endif}
  begin
    if Expr=nil then exit;
    ElBT:=GetActualBaseType(ElTypeResolved.BaseType);
    if length(ArrType.Ranges)=0 then
      begin
      // dynamic array of char can hold any string
      // ToDo: check if value can be converted without loss
      Result:=cExact;
      exit;
      end;
    // static array -> check length of string
    Value:=Eval(Expr,[refAutoConst]); // no external const allowed
    try
      case Value.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        if ElBT=btAnsiChar then
          l:=length(TResEvalString(Value).S)
        else
          begin
          US:=fExprEvaluator.GetUnicodeStr(TResEvalString(Value).S,ErrorEl);
          l:=length(US);
          end;
      {$endif}
      revkUnicodeString:
        begin
        if ElBT=btWideChar then
          l:=length(TResEvalUTF16(Value).S)
        else
          begin
          S:=String(TResEvalUTF16(Value).S);
          l:=length(S);
          end;
        end;
      else
        {$IFDEF VerbosePasResolver}
        writeln('Check_ArrayOfChar_String Value=',Value.AsDebugString);
        {$ENDIF}
        exit; // incompatible
      end;
      if ArrLength<>l then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('Check_ArrayOfChar_String ElType=',ElBT,'=',GetResolverResultDbg(ElTypeResolved),' Value=',Value.AsDebugString);
        {$ENDIF}
        RaiseMsg(20170913113216,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
          [IntToStr(ArrLength),IntToStr(l)],ErrorEl);
        end;
      Result:=cExact;
    finally
      ReleaseEvalValue(Value);
    end;
  end;

  procedure CheckRange(ArrType: TPasArrayType; RangeIndex: integer;
    Values: TPasResolverResult; ErrorEl: TPasElement);
  var
    Range, Value, Expr: TPasExpr;
    RangeResolved, ValueResolved, ElTypeResolved: TPasResolverResult;
    i, ExpectedCount, ValCnt: Integer;
    IsLastRange, IsConstExpr: Boolean;
    ArrayValues: TPasExprArray;
    LeftResult: integer;
    ExprCompFlags: TPasResolverComputeFlags;
    BuiltInProc: TResElDataBuiltInProc;
    Ref: TResolvedReference;
    RArrayType: TPasArrayType;
  begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckAssignCompatibilityArrayType.CheckRange ArrType=',GetObjName(ArrType),' RgIndex=',RangeIndex,' Values=',GetResolverResultDbg(Values));
    {$ENDIF}
    if not (rrfReadable in RHS.Flags) then
      exit;
    if (Values.BaseType=btContext) and (RangeIndex=0) and (Values.LoTypeEl=ArrType) then
      begin
      Result:=cExact;
      exit;
      end;

    Expr:=Values.ExprEl;
    if (Expr=nil) and (Values.IdentEl is TPasConst)
        and (TPasConst(Values.IdentEl).VarType=nil) then
      Expr:=TPasVariable(Values.IdentEl).Expr;
    IsConstExpr:=(Expr<>nil) and ExprEvaluator.IsConst(Expr);
    if IsConstExpr then
      ExprCompFlags:=[rcConstant]
    else
      ExprCompFlags:=[];

    if Expr<>nil then
      begin
      if IsEmptyArrayExpr(Values) then
        begin
        if length(ArrType.Ranges)=0 then
          begin
          if RaiseOnIncompatible then
            MarkArrayExprRecursive(Values.ExprEl,ArrType);
          Result:=cExact; // empty set fits open and dyn array
          exit;
          end;
        end
      else if IsArrayOperatorAdd(Expr) and not (Values.BaseType in btAllStrings) then
        begin
        // a:=left+right
        if length(ArrType.Ranges)>0 then
          exit; // ToDo: StaticArray:=A+B
        // check a:=left
        ComputeElement(TBinaryExpr(Expr).left,ValueResolved,ExprCompFlags);
        CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
        if Result=cIncompatible then exit;
        LeftResult:=Result;
        // check a:=right
        Result:=cIncompatible;
        ComputeElement(TBinaryExpr(Expr).right,ValueResolved,ExprCompFlags);
        CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
        if Result=cIncompatible then exit;
        if Result<LeftResult then
          Result:=LeftResult;
        exit;
        end
      else if (Expr<>nil) and (Expr.ClassType=TParamsExpr)
          and (TParamsExpr(Expr).Kind=pekFuncParams) then
        begin
        if TParamsExpr(Expr).Value.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(TParamsExpr(Expr).Value.CustomData);
          if (Ref.Declaration is TPasUnresolvedSymbolRef)
              and (Ref.Declaration.CustomData is TResElDataBuiltInProc) then
            begin
            BuiltInProc:=TResElDataBuiltInProc(Ref.Declaration.CustomData);
            ArrayValues:=TParamsExpr(Expr).Params;
            if BuiltInProc.BuiltIn=bfConcatArray then
              begin
              // check Concat(array1,array2,...)
              Result:=cExact;
              for i:=0 to length(ArrayValues)-1 do
                begin
                LeftResult:=Result;
                Result:=cIncompatible;
                ComputeElement(ArrayValues[i],ValueResolved,ExprCompFlags);
                CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
                if Result=cIncompatible then exit;
                if Result<LeftResult then
                  Result:=LeftResult;
                end;
              exit;
              end
            else if BuiltInProc.BuiltIn=bfCopyArray then
              begin
              // check Copy(A...)
              ComputeElement(ArrayValues[0],ValueResolved,ExprCompFlags);
              CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
              exit;
              end;
            end;
          end;
        end;
      end;

    ExpectedCount:=-1;
    if length(ArrType.Ranges)=0 then
      begin
      // dynamic array
      if (Expr<>nil) then
        begin
        if Expr.ClassType=TArrayValues then
          ExpectedCount:=length(TArrayValues(Expr).Values)
        else if (Expr.ClassType=TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
          ExpectedCount:=length(TParamsExpr(Expr).Params)
        else if (Values.BaseType in btAllStringAndChars) and IsVarInit(Expr) then
          begin
          // const a: dynarray = string
          ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
          if ElTypeResolved.BaseType in btAllChars then
            Result:=cExact;
          exit;
          end
        else
          begin
          // invalid
          exit;
          end;
        end
      else
        begin
        // type check
        if (Values.BaseType<>btContext) or (Values.LoTypeEl.ClassType<>TPasArrayType) then
          exit;
        RArrayType:=TPasArrayType(Values.LoTypeEl);
        if length(RArrayType.Ranges)>0 then
          begin
          if RaiseOnIncompatible then
            RaiseXExpectedButYFound(20180622104834,'dynamic array','static array',ErrorEl);
          exit;
          end;
        // dynarr:=dynarr -> check element type
        ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
        Include(ElTypeResolved.Flags,rrfWritable);
        ComputeElement(RArrayType.ElType,ValueResolved,[rcType]);
        Include(ValueResolved.Flags,rrfReadable);
        Result:=CheckAssignResCompatibility(ElTypeResolved,ValueResolved,ErrorEl,RaiseOnIncompatible);
        exit;
        end;
      Range:=nil;
      IsLastRange:=true;
      end
    else
      begin
      // static array
      Range:=ArrType.Ranges[RangeIndex];
      ExpectedCount:=GetRangeLength(Range);
      if ExpectedCount=0 then
        begin
        ComputeElement(Range,RangeResolved,[rcConstant]);
        RaiseNotYetImplemented(20170222232409,Expr,'range '+GetResolverResultDbg(RangeResolved));
        end;
      IsLastRange:=RangeIndex+1=length(ArrType.Ranges);
      if Expr=nil then
        begin
        if (ValueResolved.BaseType=btContext) and (ValueResolved.LoTypeEl.ClassType=TPasArrayType) then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('CheckRange TODO StaticArr:=Arr');
          {$ENDIF}
          end;
        exit;
        end;
      end;

    if IsLastRange then
      begin
      ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
      ElTypeResolved.ExprEl:=Range;
      Include(ElTypeResolved.Flags,rrfWritable);
      end
    else
      ElTypeResolved.BaseType:=btNone;

    if (Expr<>nil)
        and ((Expr.ClassType=TArrayValues)
          or ((Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet))) then
      begin
      // array literal

      if (ErrorEl.Parent is TPasVariable) then
        begin
        // array initialization  e.g.  var a: tarray = []
        if msDelphi in CurrentParser.CurrentModeswitches then
          begin
          // Delphi expects square brackets for dynamic arrays
          //   and round brackets for static arrays
          if length(ArrType.Ranges)>0 then
            begin
            // static array
            if Expr.ClassType<>TArrayValues then
              begin
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180615121203,'(','[',ErrorEl);
              exit;
              end;
            end
          else
            begin
            // dyn array
            if Expr.ClassType=TArrayValues then
              begin
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180615122953,'[','(',ErrorEl);
              exit;
              end;
            end;
          end
        else
          begin
          // ObjFPC always expects round brackets in initialization
          if Expr.ClassType<>TArrayValues then
            begin
            if RaiseOnIncompatible then
              RaiseXExpectedButYFound(20170913181208,'(','[',ErrorEl);
            exit;
            end;
          end;
        end;

      // check each value
      if Expr.ClassType=TArrayValues then
        ArrayValues:=TArrayValues(Expr).Values
      else
        ArrayValues:=TParamsExpr(Expr).Params;
      ValCnt:=length(ArrayValues);
      Include(ExprCompFlags,rcNoImplicitProcType);
      for i:=0 to ExpectedCount-1 do
        begin
        if i=ValCnt then
          begin
          // not enough values
          if ValCnt>0 then
            ErrorEl:=ArrayValues[ValCnt-1];
          RaiseMsg(20170222233001,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),IntToStr(ValCnt)],ErrorEl);
          end;
        Value:=ArrayValues[i];
        ComputeElement(Value,ValueResolved,ExprCompFlags);
        if IsLastRange then
          begin
          // last dimension -> check element type
          Result:=CheckAssignResCompatibility(ElTypeResolved,ValueResolved,Value,RaiseOnIncompatible);
          if Result=cIncompatible then
            exit;
          CheckAssignExprRange(ElTypeResolved,Value);
          end
        else
          begin
          // multi dimensional array -> check next range
          CheckRange(ArrType,RangeIndex+1,ValueResolved,Value);
          end;
        end;
      if ExpectedCount<ValCnt then
        begin
        // too many values
        ErrorEl:=ArrayValues[ExpectedCount];
        if RaiseOnIncompatible then
          RaiseMsg(20170222233605,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),IntToStr(ValCnt)],ErrorEl);
        exit;
        end;

      if RaiseOnIncompatible and (Expr.ClassType=TParamsExpr) then
        // mark [] expression as an array
        MarkArrayExpr(TParamsExpr(Expr),ArrType);

      end
    else
      begin
      // single value
      // Note: the parser does not store the difference between (1) and 1
      if not IsLastRange then
        begin
        if RaiseOnIncompatible then
          RaiseMsg(20170223095307,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),'1'],ErrorEl);
        exit;
        end;
      if (Values.BaseType in btAllStrings) and (ElTypeResolved.BaseType in btAllChars) then
        begin
        // e.g. array of char = ''
        Check_ArrayOfChar_String(ArrType,ExpectedCount,ElTypeResolved,Expr,ErrorEl);
        exit;
        end;
      if (ExpectedCount>1) then
        begin
        if RaiseOnIncompatible then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('CheckRange Values=',GetResolverResultDbg(Values),' ElTypeResolved=',GetResolverResultDbg(ElTypeResolved));
          {$ENDIF}
          RaiseMsg(20170913103143,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),'1'],ErrorEl);
          end;
        exit;
        end;
      // check element type
      Result:=CheckAssignResCompatibility(ElTypeResolved,Values,ErrorEl,RaiseOnIncompatible);
      if Result=cIncompatible then
        exit;
      if Expr<>nil then
        CheckAssignExprRange(ElTypeResolved,Expr);
      end;
  end;

var
  LArrType: TPasArrayType;
begin
  Result:=cIncompatible;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignCompatibilityArrayType LHS=',GetResolverResultDbg(LHS),' RHS=',GetResolverResultDbg(RHS));
  {$ENDIF}
  if (LHS.BaseType<>btContext) or (not (LHS.LoTypeEl is TPasArrayType)) then
    RaiseInternalError(20170222230012);
  LArrType:=TPasArrayType(LHS.LoTypeEl);

  CheckRange(LArrType,0,RHS,ErrorEl);

  if (Result=cIncompatible) and RaiseOnIncompatible then
    RaiseIncompatibleTypeRes(20180622104721,nIncompatibleTypesGotExpected,[],RHS,LHS,ErrorEl);
end;

function TPasResolver.CheckAssignCompatibilityPointerType(LTypeEl,
  RTypeEl: TPasType; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  LeftResolved, RightResolved: TPasResolverResult;
begin
  ComputeElement(LTypeEl,LeftResolved,[rcNoImplicitProc]);
  ComputeElement(RTypeEl,RightResolved,[rcNoImplicitProc]);
  Include(LeftResolved.Flags,rrfWritable);
  Include(RightResolved.Flags,rrfReadable);
  Result:=CheckAssignResCompatibility(LeftResolved,RightResolved,ErrorEl,RaiseOnIncompatible);
end;

function TPasResolver.CheckEqualCompatibilityUserType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
// LHS.BaseType=btContext=RHS.BaseType and both rrfReadable
var
  LTypeEl, RTypeEl: TPasType;
  AResolved, BResolved: TPasResolverResult;

  function IncompatibleElements: integer;
  begin
    Result:=cIncompatible;
    if not RaiseOnIncompatible then exit;
    RaiseIncompatibleType(20170216152513,nIncompatibleTypesGotExpected,
      [],LTypeEl,RTypeEl,ErrorEl);
  end;

begin
  if (LHS.LoTypeEl=nil) then
    RaiseInternalError(20161007223118);
  if (RHS.LoTypeEl=nil) then
    RaiseInternalError(20161007223119);
  LTypeEl:=LHS.LoTypeEl;
  RTypeEl:=RHS.LoTypeEl;
  if LTypeEl=RTypeEl then
    exit(cExact);

  if LTypeEl.ClassType=TPasClassType then
    begin
    if RTypeEl.ClassType=TPasClassType then
      begin
      // e.g. if Sender=Button1 then
      Result:=CheckSrcIsADstType(LHS,RHS,ErrorEl);
      if Result=cIncompatible then
        Result:=CheckSrcIsADstType(RHS,LHS,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20180324190757,nTypesAreNotRelatedXY,[],LHS,RHS,ErrorEl);
      exit;
      end
    else if RTypeEl.ClassType=TPasRecordType then
      begin
      if (TPasClassType(LTypeEl).ObjKind=okInterface)
          and IsTGUID(TPasRecordType(RTypeEl)) then
        // IntfVar=GuidVar
        exit(cInterfaceToTGUID);
      end;
    exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasClassOfType then
    begin
    if RTypeEl.ClassType=TPasClassOfType then
      begin
      // for example: if ImageClass=ImageClass then
      Result:=CheckClassIsClass(TPasClassOfType(LTypeEl).DestType,
                                TPasClassOfType(RTypeEl).DestType,ErrorEl);
      if Result=cIncompatible then
        Result:=CheckClassIsClass(TPasClassOfType(RTypeEl).DestType,
                                  TPasClassOfType(LTypeEl).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20180324190804,nTypesAreNotRelatedXY,[],LHS,RHS,ErrorEl);
      exit;
      end;
    exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    if not RaiseOnIncompatible then
      exit(cIncompatible);
    if RTypeEl.ClassType=TPasEnumValue then
      RaiseIncompatibleType(20170216152523,nIncompatibleTypesGotExpected,
        [],TPasEnumType(LTypeEl),TPasEnumType(RTypeEl),ErrorEl)
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasRecordType then
    begin
    if RTypeEl.ClassType=TPasClassType then
      begin
      if (TPasClassType(RTypeEl).ObjKind=okInterface)
          and IsTGUID(TPasRecordType(LTypeEl)) then
        // GuidVar=IntfVar
        exit(cInterfaceToTGUID);
      end;
    end
  else if LTypeEl.ClassType=TPasSetType then
    begin
    if RTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(LTypeEl).EnumType,AResolved,[]);
      ComputeElement(TPasSetType(RTypeEl).EnumType,BResolved,[]);
      if (AResolved.LoTypeEl<>nil)
      and (AResolved.LoTypeEl=BResolved.LoTypeEl) then
        exit(cExact);
      if (AResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (BResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (CompareText(AResolved.LoTypeEl.Name,BResolved.LoTypeEl.Name)=0) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152524,nIncompatibleTypesGotExpected,
          [],AResolved,BResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl is TPasProcedureType then
    begin
    if RTypeEl is TPasProcedureType then
      begin
      // e.g. ProcVar1 = ProcVar2
      if CheckProcTypeCompatibility(TPasProcedureType(LTypeEl),TPasProcedureType(RTypeEl),
          false,nil,false) then
        exit(cExact);
      end
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasPointerType then
    begin
    if RTypeEl.ClassType=TPasPointerType then
      // TypedPointer=TypedPointer
      exit(cExact);
    end;
  exit(IncompatibleElements);
end;

function TPasResolver.CheckTypeCast(El: TPasType; Params: TParamsExpr;
  RaiseOnError: boolean): integer;
// for example  if TClassA(AnObject)=nil then ;
var
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
begin
  if length(Params.Params)<>1 then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152526,nWrongNumberOfParametersForTypeCast,
        sWrongNumberOfParametersForTypeCast,[El.Name],Params);
    exit(cIncompatible);
    end;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProcType]);
  ComputeElement(El,ResolvedEl,[rcType]);
  Result:=CheckTypeCastRes(ParamResolved,ResolvedEl,Param,RaiseOnError);
end;

function TPasResolver.CheckTypeCastRes(const FromResolved,
  ToResolved: TPasResolverResult; ErrorEl: TPasElement; RaiseOnError: boolean
  ): integer;
var
  ToTypeEl, ToClassType, FromClassType, FromTypeEl: TPasType;
  ToTypeBaseType: TResolverBaseType;
  C: TClass;
  ToProcType, FromProcType: TPasProcedureType;
begin
  Result:=cIncompatible;
  ToTypeEl:=ToResolved.LoTypeEl;
  if (ToTypeEl<>nil)
      and (rrfReadable in FromResolved.Flags) then
    begin
    C:=ToTypeEl.ClassType;
    if FromResolved.BaseType=btUntyped then
      begin
      // typecast an untyped parameter
      Result:=cCompatible;
      end
    else if C=TPasUnresolvedSymbolRef then
      begin
      if ToTypeEl.CustomData is TResElDataBaseType then
        begin
        // base type cast, e.g. double(aninteger)
        if ToTypeEl=FromResolved.LoTypeEl then
          exit(cExact);
        ToTypeBaseType:=(ToTypeEl.CustomData as TResElDataBaseType).BaseType;
        if ToTypeBaseType=FromResolved.BaseType then
          Result:=cExact
        else if ToTypeBaseType in btAllInteger then
          begin
          if FromResolved.BaseType in (btArrayRangeTypes+[btRange,btCurrency]) then
            Result:=cCompatible
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            if FromTypeEl.ClassType=TPasEnumType then
              // e.g. longint(TEnum)
              Result:=cCompatible;
            end;
          end
        else if ToTypeBaseType in btAllFloats then
          begin
          if FromResolved.BaseType in btAllFloats then
            Result:=cCompatible
          else if FromResolved.BaseType in btAllInteger then
            Result:=cCompatible;
          end
        else if ToTypeBaseType in btAllBooleans then
          begin
          if FromResolved.BaseType in btAllBooleans then
            Result:=cCompatible
          else if FromResolved.BaseType in btAllInteger then
            Result:=cCompatible;
          end
        else if ToTypeBaseType in btAllChars then
          begin
          if FromResolved.BaseType in (btArrayRangeTypes+[btRange]) then
            Result:=cCompatible
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            if FromTypeEl.ClassType=TPasEnumType then
              // e.g. char(TEnum)
              Result:=cCompatible;
            end;
          end
        else if ToTypeBaseType in btAllStrings then
          begin
          if FromResolved.BaseType in btAllStringAndChars then
            Result:=cCompatible
          else if (FromResolved.BaseType=btPointer)
              and (ToTypeBaseType in btAllStringPointer) then
            Result:=cExact;
          end
        else if ToTypeBaseType=btPointer then
          begin
          if FromResolved.BaseType in ([btPointer]+btAllStringPointer) then
            Result:=cExact
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            C:=FromTypeEl.ClassType;
            if (C=TPasClassType)
                or (C=TPasClassOfType)
                or (C=TPasPointerType)
                or ((C=TPasArrayType) and IsDynArray(FromTypeEl)) then
              Result:=cExact
            else if (C=TPasProcedureType) or (C=TPasFunctionType) then
              begin
              // from procvar to pointer
              FromProcType:=TPasProcedureType(FromTypeEl);
              if FromProcType.IsOfObject then
                begin
                if proMethodAddrAsPointer in Options then
                  Result:=cCompatible
                else if RaiseOnError then
                  RaiseMsg(20170416183615,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmOfObject],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else if FromProcType.IsNested then
                begin
                if RaiseOnError then
                  RaiseMsg(20170416183800,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmIsNested],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else if FromProcType.IsReferenceTo then
                begin
                if proProcTypeWithoutIsNested in Options then
                  Result:=cCompatible
                else if RaiseOnError then
                  RaiseMsg(20170419144311,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmReferenceTo],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else
                Result:=cCompatible;
              end;
            end;
          end;
        end;
      end
    else if C=TPasClassType then
      begin
      // to class
      if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        if FromTypeEl.ClassType=TPasClassType then
          begin
          if FromResolved.IdentEl is TPasType then
            RaiseMsg(20170404162606,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
          if TPasClassType(FromTypeEl).ObjKind=TPasClassType(ToTypeEl).ObjKind then
            begin
            // type cast upwards or downwards
            Result:=CheckSrcIsADstType(FromResolved,ToResolved,ErrorEl);
            if Result=cIncompatible then
              Result:=CheckSrcIsADstType(ToResolved,FromResolved,ErrorEl);
            end
          else if TPasClassType(ToTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(FromTypeEl).ObjKind=okClass)
                and (not TPasClassType(FromTypeEl).IsExternal) then
              begin
              // e.g. intftype(classinstvar)
              Result:=cCompatible;
              end;
            end
          else if TPasClassType(FromTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(ToTypeEl).ObjKind=okClass)
                and (not TPasClassType(ToTypeEl).IsExternal) then
              begin
              // e.g. classtype(intfvar)
              Result:=cCompatible;
              end;
            end;
          if Result=cIncompatible then
            Result:=CheckTypeCastClassInstanceToClass(FromResolved,ToResolved,ErrorEl);
          end
        end
      else if FromResolved.BaseType=btPointer then
        begin
        if IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to class instance
        end;
      end
    else if C=TPasClassOfType then
      begin
      //writeln('TPasResolver.CheckTypeCast class-of FromRes.TypeEl=',GetObjName(FromResolved.LoTypeEl),' FromRes.IdentEl=',GetObjName(FromResolved.IdentEl));
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasClassOfType then
          begin
          if (FromResolved.IdentEl is TPasType) then
            RaiseMsg(20170404162604,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
          // type cast  classof(classof-var)  upwards or downwards
          ToClassType:=TPasClassOfType(ToTypeEl).DestType;
          FromClassType:=TPasClassOfType(FromResolved.LoTypeEl).DestType;
          Result:=CheckClassesAreRelated(ToClassType,FromClassType,ErrorEl);
          end;
        end
      else if FromResolved.BaseType=btPointer then
        begin
        if IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to class-of
        end;
      end
    else if C=TPasRecordType then
      begin
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasRecordType then
          begin
          // typecast record to record
          Result:=cExact;
          end;
        end;
      end
    else if (C=TPasEnumType)
        or (C=TPasRangeType) then
      begin
      if CheckIsOrdinal(FromResolved,ErrorEl,true) then
        Result:=cExact;
      end
    else if C=TPasArrayType then
      begin
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasArrayType then
          Result:=CheckTypeCastArray(TPasArrayType(FromResolved.LoTypeEl),
            TPasArrayType(ToTypeEl),ErrorEl,RaiseOnError);
        end
      else if FromResolved.BaseType=btPointer then
        begin
        if IsDynArray(ToResolved.LoTypeEl)
            and IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to dynnamic array
        end;
      end
    else if (C=TPasProcedureType) or (C=TPasFunctionType) then
      begin
      ToProcType:=TPasProcedureType(ToTypeEl);
      if IsBaseType(FromResolved.LoTypeEl,btPointer) then
        begin
        // type cast untyped pointer value to proctype
        if ToProcType.IsOfObject then
          begin
          if proMethodAddrAsPointer in Options then
            Result:=cCompatible
          else if RaiseOnError then
            RaiseMsg(20170416183940,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmOfObject]],ErrorEl);
          end
        else if ToProcType.IsNested then
          begin
          if RaiseOnError then
            RaiseMsg(20170416184149,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmIsNested]],ErrorEl);
          end
        else if ToProcType.IsReferenceTo then
          begin
          if proMethodAddrAsPointer in Options then
            Result:=cCompatible
          else if RaiseOnError then
            RaiseMsg(20170419144357,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmReferenceTo]],ErrorEl);
          end
        else
          Result:=cCompatible;
        end
      else if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        if FromTypeEl is TPasProcedureType then
          begin
          // type cast procvar to proctype
          FromProcType:=TPasProcedureType(FromTypeEl);
          if ToProcType.IsReferenceTo then
            Result:=cCompatible
          else if FromProcType.IsReferenceTo then
            Result:=cCompatible
          else if (FromProcType.IsOfObject<>ToProcType.IsOfObject)
              and not (proMethodAddrAsPointer in Options) then
            begin
            if RaiseOnError then
              RaiseMsg(20170416183109,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],'')],ErrorEl);
            end
          else if FromProcType.IsNested<>ToProcType.IsNested then
            begin
            if RaiseOnError then
              RaiseMsg(20170416183305,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],'')],ErrorEl);
            end
          else
            Result:=cCompatible;
          end
        end;
      end
    else if C=TPasPointerType then
      begin
      // typecast to typedpointer
      if FromResolved.BaseType in [btPointer,btNil] then
        Result:=cExact
      else if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        C:=FromTypeEl.ClassType;
        if (C=TPasPointerType)
            or (C=TPasClassOfType)
            or (C=TPasClassType)
            or (C.InheritsFrom(TPasProcedureType))
            or IsDynArray(FromTypeEl) then
          Result:=cCompatible;
        end;
      end
    end
  else if ToTypeEl<>nil then
    begin
    // FromResolved is not readable
    if FromResolved.BaseType=btContext then
      begin
      FromTypeEl:=FromResolved.LoTypeEl;
      if (FromTypeEl.ClassType=TPasClassType)
          and (FromTypeEl=FromResolved.IdentEl)
          and (ToResolved.BaseType=btContext) then
        begin
        ToTypeEl:=ToResolved.LoTypeEl;
        if (ToTypeEl.ClassType=TPasClassOfType)
            and (ToTypeEl=ToResolved.IdentEl) then
          begin
          // for example  class-of(Self) in a class function
          ToClassType:=TPasClassOfType(ToTypeEl).DestType;
          FromClassType:=TPasClassType(FromTypeEl);
          Result:=CheckClassesAreRelated(ToClassType,FromClassType,ErrorEl);
          end;
        end;
      end;
    if (Result=cIncompatible) and RaiseOnError then
      begin
      if FromResolved.IdentEl is TPasType then
        RaiseMsg(20170404162610,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
      end;
    end;

  if Result=cIncompatible then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastRes From={',GetResolverResultDbg(FromResolved),'} To={',GetResolverResultDbg(ToResolved),'}');
    {$ENDIF}
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152528,nIllegalTypeConversionTo,
        [],FromResolved,ToResolved,ErrorEl);
    exit;
    end;
end;

function TPasResolver.CheckTypeCastArray(FromType, ToType: TPasArrayType;
  ErrorEl: TPasElement; RaiseOnError: boolean): integer;

  function NextDim(var ArrType: TPasArrayType; var NextIndex: integer;
    out ElTypeResolved: TPasResolverResult): boolean;
  begin
    inc(NextIndex);
    if NextIndex<length(ArrType.Ranges) then
      begin
      ElTypeResolved.BaseType:=btNone;
      exit(true);
      end;
    ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
    if (ElTypeResolved.BaseType<>btContext)
        or (ElTypeResolved.LoTypeEl.ClassType<>TPasArrayType) then
      exit(false);
    ArrType:=TPasArrayType(ElTypeResolved.LoTypeEl);
    NextIndex:=0;
    Result:=true;
  end;

var
  FromIndex, ToIndex: Integer;
  FromElTypeRes, ToElTypeRes: TPasResolverResult;
  StartFromType, StartToType: TPasArrayType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDescription(FromType),' ToType=',GetTypeDescription(ToType));
  {$ENDIF}
  StartFromType:=FromType;
  StartToType:=ToType;
  Result:=cIncompatible;
  // check dimensions
  FromIndex:=0;
  ToIndex:=0;
  repeat
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,' ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
    {$ENDIF}
    if length(ToType.Ranges)=0 then
      // ToType is dynamic/open array -> fits any size
    else
      begin
      // ToType is ranged
      // ToDo: check size of dimension
      end;
    // check next dimension
    if not NextDim(FromType,FromIndex,FromElTypeRes) then
      begin
      // at end of FromType
      if NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray To has more dims than From: From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has more dimensions
        end;
      // have same dimension -> check ElType
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckTypeCastArray check ElType From=',GetResolverResultDbg(FromElTypeRes),' To=',GetResolverResultDbg(ToElTypeRes));
      {$ENDIF}
      Include(FromElTypeRes.Flags,rrfReadable);
      Result:=CheckTypeCastRes(FromElTypeRes,ToElTypeRes,ErrorEl,false);
      break;
      end
    else
      begin
      // FromType has more dimensions
      if not NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray From has more dims than To: From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has less dimensions
        end;
      end;
  until false;
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleType(20170331124643,nIllegalTypeConversionTo,
      [],StartFromType,StartToType,ErrorEl);
end;

procedure TPasResolver.ComputeElement(El: TPasElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIdentifier(Expr: TPasExpr);
  var
    Ref: TResolvedReference;
    Proc: TPasProcedure;
    ProcType: TPasProcedureType;
    aClass: TPasClassType;
  begin
    Ref:=TResolvedReference(Expr.CustomData);
    ComputeElement(Ref.Declaration,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
    if rrfConstInherited in Ref.Flags then
      Exclude(ResolvedEl.Flags,rrfWritable);
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if Expr is TPrimitiveExpr then
      writeln('TPasResolver.ComputeElement.ComputeIdentifier TPrimitiveExpr "',TPrimitiveExpr(Expr).Value,'" ',GetResolverResultDbg(ResolvedEl),' Flags=',dbgs(Flags))
    else
      writeln('TPasResolver.ComputeElement.ComputeIdentifier "',GetObjName(Expr),'" ',GetResolverResultDbg(ResolvedEl),' Flags=',dbgs(Flags));
    {AllowWriteln-}
    {$ENDIF}
    if (Expr is TPrimitiveExpr) and (Expr.Parent is TParamsExpr) and (TPrimitiveExpr(Expr).Value='FA') then
      //RaiseNotYetImplemented(20180621235200,Expr);

    if not (rcSetReferenceFlags in Flags)
        and (rrfNoImplicitCallWithoutParams in Ref.Flags) then
      exit;

    if (ResolvedEl.BaseType=btProc) then
      begin
      // proc
      if rcNoImplicitProc in Flags then
        begin
         if rcSetReferenceFlags in Flags then
           Include(Ref.Flags,rrfNoImplicitCallWithoutParams);
        end
      else if [rcConstant,rcType]*Flags=[] then
        begin
        // implicit call without params is allowed -> check if possible
        Proc:=ResolvedEl.IdentEl as TPasProcedure;
        if not ProcNeedsParams(Proc.ProcType) then
          begin
          // parameter less proc -> implicit call possible
          if ResolvedEl.IdentEl is TPasFunction then
            begin
            // function => return result
            ComputeElement(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl,
              ResolvedEl,Flags+[rcType],StartEl);
            Exclude(ResolvedEl.Flags,rrfWritable);
            end
          else if (ResolvedEl.IdentEl.ClassType=TPasConstructor)
              and (rrfNewInstance in Ref.Flags) then
            begin
            // new instance constructor -> return value of type class
            aClass:=GetReference_NewInstanceClass(Ref);
            SetResolverValueExpr(ResolvedEl,btContext,aClass,aClass,
                                 TPrimitiveExpr(Expr),[rrfReadable]);
            end
          else if ParentNeedsExprResult(Expr) then
            begin
            // a procedure
            exit;
            end;
          if rcSetReferenceFlags in Flags then
            Include(Ref.Flags,rrfImplicitCallWithoutParams);
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end
    else if IsProcedureType(ResolvedEl,true) then
      begin
      // proc type
      if [rcNoImplicitProc,rcNoImplicitProcType]*Flags<>[] then
        begin
         if rcSetReferenceFlags in Flags then
           Include(Ref.Flags,rrfNoImplicitCallWithoutParams);
        end
      else if [rcConstant,rcType]*Flags=[] then
        begin
        // implicit call without params is allowed -> check if possible
        ProcType:=TPasProcedureType(ResolvedEl.LoTypeEl);
        if not ProcNeedsParams(ProcType) then
          begin
          // parameter less proc type -> implicit call possible
          if ResolvedEl.LoTypeEl is TPasFunctionType then
            // function => return result
            ComputeElement(TPasFunctionType(ResolvedEl.LoTypeEl).ResultEl,
              ResolvedEl,Flags+[rcType],StartEl)
          else if ParentNeedsExprResult(Expr) then
            begin
            // a procedure has no result
            exit;
            end;
          if rcSetReferenceFlags in Flags then
            Include(Ref.Flags,rrfImplicitCallWithoutParams);
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end;
  end;

  procedure ComputeInherited(Expr: TInheritedExpr);
  var
    Ref: TResolvedReference;
    Proc: TPasProcedure;
    TypeEl: TPasProcedureType;
    aClass: TPasClassType;
    HasName: Boolean;
  begin
    // "inherited;"
    Ref:=TResolvedReference(El.CustomData);
    Proc:=NoNil(Ref.Declaration) as TPasProcedure;
    TypeEl:=TPasProcedure(Proc).ProcType;
    SetResolverIdentifier(ResolvedEl,btProc,Proc,
      TypeEl,TypeEl,[rrfCanBeStatement]);
    HasName:=(El.Parent.ClassType=TBinaryExpr)
       and (TBinaryExpr(El.Parent).OpCode=eopNone); // true if 'inherited Proc;'
    if HasName or (rcNoImplicitProc in Flags) then
      exit;

    // inherited;  -> implicit call possible
    if Proc is TPasFunction then
      begin
      // function => return result
      ComputeElement(TPasFunction(Proc).FuncType.ResultEl,
        ResolvedEl,Flags+[rcType],StartEl);
      Exclude(ResolvedEl.Flags,rrfWritable);
      end
    else if (Proc.ClassType=TPasConstructor)
        and (rrfNewInstance in Ref.Flags) then
      begin
      // new instance constructor -> return value of type class
      aClass:=GetReference_NewInstanceClass(Ref);
      SetResolverValueExpr(ResolvedEl,btContext,aClass,aClass,Expr,[rrfReadable]);
      end
    else if ParentNeedsExprResult(Expr) then
      begin
      // a procedure
      exit;
      end;
    if rcSetReferenceFlags in Flags then
      Include(Ref.Flags,rrfImplicitCallWithoutParams);
    Include(ResolvedEl.Flags,rrfCanBeStatement);
  end;

var
  DeclEl: TPasElement;
  ElClass: TClass;
  bt: TResolverBaseType;
  TypeEl: TPasType;
begin
  if StartEl=nil then StartEl:=El;
  ResolvedEl:=Default(TPasResolverResult);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeElement El=',GetObjName(El));
  {$ENDIF}
  if El=nil then
    exit;
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(El).Kind of
      pekIdent,pekSelf:
        begin
        if not (El.CustomData is TResolvedReference) then
          RaiseNotYetImplemented(20160922163658,El,'Value="'+TPrimitiveExpr(El).Value+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
        ComputeIdentifier(TPrimitiveExpr(El));
        end;
      pekNumber:
        if Pos('.',TPrimitiveExpr(El).Value)>0 then
          SetResolverValueExpr(ResolvedEl,BaseTypeExtended,
                      FBaseTypes[BaseTypeExtended],FBaseTypes[BaseTypeExtended],
                      TPrimitiveExpr(El),[rrfReadable])
        else
          SetResolverValueExpr(ResolvedEl,btLongint,
                      FBaseTypes[btLongint],FBaseTypes[btLongint],
                      TPrimitiveExpr(El),[rrfReadable]);
      pekString:
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeElement pekString Value="',TPrimitiveExpr(El).Value,'"');
        {$ENDIF}
        bt:=IsCharLiteral(TPrimitiveExpr(El).Value,El);
        if bt in btAllChars then
          begin
          if bt=BaseTypeChar then
            bt:=btChar;
          SetResolverValueExpr(ResolvedEl,bt,FBaseTypes[bt],FBaseTypes[bt],
                               TPrimitiveExpr(El),[rrfReadable]);
          end
        else
          SetResolverValueExpr(ResolvedEl,btString,
                               FBaseTypes[btString],FBaseTypes[btString],
                               TPrimitiveExpr(El),[rrfReadable]);
        end;
      pekNil:
        SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],FBaseTypes[btNil],
                             TPrimitiveExpr(El),[rrfReadable]);
      pekBoolConst:
        SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                             TPrimitiveExpr(El),[rrfReadable]);
    else
      RaiseNotYetImplemented(20160922163701,El);
    end;
    end
  else if ElClass=TSelfExpr then
    begin
    // self is just an identifier
    if not (El.CustomData is TResolvedReference) then
      RaiseNotYetImplemented(20170216150017,El,' El="'+GetObjName(El)+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
    ComputeIdentifier(TSelfExpr(El));
    end
  else if ElClass=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    if El.CustomData is TResElDataBaseType then
      SetResolverIdentifier(ResolvedEl,TResElDataBaseType(El.CustomData).BaseType,
        El,TPasUnresolvedSymbolRef(El),TPasUnresolvedSymbolRef(El),[])
    else if El.CustomData is TResElDataBuiltInProc then
      begin
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,El,
        TPasUnresolvedSymbolRef(El),TPasUnresolvedSymbolRef(El),[]);
      if bipfCanBeStatement in TResElDataBuiltInProc(El.CustomData).Flags then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      RaiseNotYetImplemented(20160926194756,El);
    end
  else if ElClass=TBoolConstExpr then
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                         TBoolConstExpr(El),[rrfReadable])
  else if ElClass=TBinaryExpr then
    ComputeBinaryExpr(TBinaryExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TUnaryExpr then
    begin
    if TUnaryExpr(El).OpCode in [eopAddress,eopMemAddress] then
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags+[rcNoImplicitProc],StartEl)
    else
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags,StartEl);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement Unary Kind=',TUnaryExpr(El).Kind,' OpCode=',TUnaryExpr(El).OpCode,' OperandResolved=',GetResolverResultDbg(ResolvedEl),' ',GetElementSourcePosStr(El));
    {$ENDIF}
    case TUnaryExpr(El).OpCode of
      eopAdd, eopSubtract:
        if ResolvedEl.BaseType in (btAllInteger+btAllFloats) then
          exit
        else
          RaiseMsg(20170216152532,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
      eopNot:
        begin
          if ResolvedEl.BaseType in (btAllInteger+btAllBooleans) then
          else
            ComputeUnaryNot(TUnaryExpr(El),ResolvedEl,Flags);
          exit;
        end;
      eopAddress:
        if (ResolvedEl.BaseType=btProc) and (ResolvedEl.IdentEl is TPasProcedure) then
          begin
          SetResolverValueExpr(ResolvedEl,btContext,
            ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,TUnaryExpr(El).Operand,[rrfReadable]);
          exit;
          end
        else if (rrfReadable in ResolvedEl.Flags) and (ResolvedEl.BaseType<>btPointer) then
          begin
          SetResolverValueExpr(ResolvedEl,btPointer,
            ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,TUnaryExpr(El).Operand,[rrfReadable]);
          exit;
          end
        else
          RaiseMsg(20180208121541,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
      eopDeref:
        begin
        ComputeDereference(TUnaryExpr(El),ResolvedEl);
        exit;
        end;
      eopMemAddress:
        if (ResolvedEl.BaseType=btContext) and (ResolvedEl.LoTypeEl is TPasProcedureType) then
          exit
        else
          RaiseMsg(20180208121549,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
    end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement OpCode=',TUnaryExpr(El).OpCode);
    {$ENDIF}
    RaiseNotYetImplemented(20160926142426,El);
    end
  else if ElClass=TParamsExpr then
    case TParamsExpr(El).Kind of
      pekArrayParams: // a[]
        ComputeArrayParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekFuncParams: // a()
        ComputeFuncParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekSet: // []
        ComputeSetParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
    else
      RaiseNotYetImplemented(20161010184559,El);
    end
  else if ElClass=TInheritedExpr then
    begin
    // writeln('TPasResolver.ComputeElement TInheritedExpr El.CustomData=',GetObjName(El.CustomData));
    if El.CustomData is TResolvedReference then
      ComputeInherited(TInheritedExpr(El))
    else
      // no ancestor proc
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,nil,nil,nil,[rrfCanBeStatement]);
    end
  else if (ElClass=TPasAliasType) or (ElClass=TPasTypeAliasType) then
    begin
    // e.g. 'type a = b' -> compute b
    ComputeElement(TPasAliasType(El).DestType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.HiTypeEl:=TPasAliasType(El);
    end
  else if (ElClass=TPasVariable) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152737,StartEl);
    ComputeElement(TPasVariable(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if (ElClass=TPasConst) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if TPasConst(El).VarType<>nil then
      begin
      // typed const
      if (not TPasConst(El).IsConst) and ([rcConstant,rcType]*Flags<>[]) then
        RaiseConstantExprExp(20170216152739,StartEl);
      ComputeElement(TPasConst(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      if TPasConst(El).IsConst then
        ResolvedEl.Flags:=[rrfReadable]
      else
        ResolvedEl.Flags:=[rrfReadable,rrfWritable];
      end
    else
      begin
      // untyped const
      ComputeElement(TPasConst(El).Expr,ResolvedEl,Flags+[rcConstant],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable];
      end;
    end
  else if (ElClass=TPasEnumValue) then
    begin
    TypeEl:=NoNil(El.Parent) as TPasEnumType;
    SetResolverIdentifier(ResolvedEl,btContext,El,TypeEl,TypeEl,[rrfReadable])
    end
  else if (ElClass=TPasEnumType) then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasEnumType(El),TPasEnumType(El),[])
  else if (ElClass=TPasProperty) then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152741,StartEl);
    if GetPasPropertyArgs(TPasProperty(El)).Count=0 then
      begin
      ComputeElement(GetPasPropertyType(TPasProperty(El)),ResolvedEl,
        Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[];
      if GetPasPropertyGetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfReadable);
      if GetPasPropertySetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfWritable);
      if IsProcedureType(ResolvedEl,true) then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      begin
      // index property without name
      // Note: computing the pekArrayParams TParamsExpr will convert this to the type
      SetResolverIdentifier(ResolvedEl,btArrayProperty,El,nil,nil,[]);
      end;
    end
  else if ElClass=TPasArgument then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152744,StartEl);
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolverIdentifier(ResolvedEl,btUntyped,El,nil,nil,[])
    else
      begin
      // typed parameter -> use param as IdentEl, compute type
      ComputeElement(TPasArgument(El).ArgType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      end;
    ResolvedEl.Flags:=[rrfReadable];
    if TPasArgument(El).Access in [argDefault, argVar, argOut] then
      Include(ResolvedEl.Flags,rrfWritable);
    if IsProcedureType(ResolvedEl,true) then
      Include(ResolvedEl.Flags,rrfCanBeStatement);
    end
  else if ElClass=TPasClassType then
    begin
    if TPasClassType(El).IsForward and (El.CustomData<>nil) then
      begin
      DeclEl:=(TPasClassType(El).CustomData as TResolvedReference).Declaration;
      TypeEl:=NoNil(DeclEl) as TPasClassType;
      end
    else
      TypeEl:=TPasClassType(El);
    SetResolverIdentifier(ResolvedEl,btContext,
                          TypeEl,TypeEl,TypeEl,[]);
    end
  else if ElClass=TPasClassOfType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasClassOfType(El),TPasClassOfType(El),[])
  else if ElClass=TPasPointerType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasPointerType(El),TPasPointerType(El),[])
  else if ElClass=TPasRecordType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasRecordType(El),TPasRecordType(El),[])
  else if ElClass=TPasRangeType then
    begin
    ComputeElement(TPasRangeType(El).RangeExpr,ResolvedEl,[rcConstant],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.LoTypeEl:=TPasRangeType(El);
    ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
    if ResolvedEl.ExprEl=nil then
      ResolvedEl.ExprEl:=TPasRangeType(El).RangeExpr;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasSetType then
    begin
    ComputeElement(TPasSetType(El).EnumType,ResolvedEl,[rcConstant],StartEl);
    if ResolvedEl.BaseType=btRange then
      begin
      ConvertRangeToElement(ResolvedEl);
      ResolvedEl.LoTypeEl:=TPasSetType(El).EnumType;
      ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
      end;
    ResolvedEl.SubType:=ResolvedEl.BaseType;
    ResolvedEl.BaseType:=btSet;
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasResultElement then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152746,StartEl);
    ComputeElement(TPasResultElement(El).ResultType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if ElClass=TPasUsesUnit then
    begin
    if TPasUsesUnit(El).Module is TPasModule then
      SetResolverIdentifier(ResolvedEl,btModule,TPasUsesUnit(El).Module,nil,nil,[])
    else
      RaiseNotYetImplemented(20170429112047,TPasUsesUnit(El).Module);
    end
  else if El.InheritsFrom(TPasModule) then
    SetResolverIdentifier(ResolvedEl,btModule,El,nil,nil,[])
  else if ElClass=TNilExpr then
    SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],FBaseTypes[btNil],
                         TNilExpr(El),[rrfReadable])
  else if El.InheritsFrom(TPasProcedure) then
    begin
    TypeEl:=TPasProcedure(El).ProcType;
    SetResolverIdentifier(ResolvedEl,btProc,El,TypeEl,TypeEl,[rrfCanBeStatement]);
    if El is TPasFunction then
      Include(ResolvedEl.Flags,rrfReadable);
    // Note: the readability of TPasConstructor depends on the context
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if El.InheritsFrom(TPasProcedureType) then
    begin
    SetResolverIdentifier(ResolvedEl,btContext,El,
               TPasProcedureType(El),TPasProcedureType(El),[rrfCanBeStatement]);
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if ElClass=TPasArrayType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasArrayType(El),TPasArrayType(El),[])
  else if ElClass=TArrayValues then
    SetResolverValueExpr(ResolvedEl,btArrayLit,nil,nil,TArrayValues(El),[rrfReadable])
  else if ElClass=TRecordValues then
    ComputeRecordValues(TRecordValues(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TPasStringType then
    begin
    {$ifdef FPC_HAS_CPSTRING}
    SetResolverTypeExpr(ResolvedEl,btShortString,
               BaseTypes[btShortString],BaseTypes[btShortString],[rrfReadable]);
    if BaseTypes[btShortString]=nil then
    {$endif}
      RaiseMsg(20170419203146,nIllegalQualifier,sIllegalQualifier,['['],El);
    end
  else if ElClass=TPasResString then
    SetResolverIdentifier(ResolvedEl,btString,El,
                        FBaseTypes[btString],FBaseTypes[btString],[rrfReadable])
  else
    RaiseNotYetImplemented(20160922163705,El);
  {$IF defined(nodejs) and defined(VerbosePasResolver)}
  if not isNumber(ResolvedEl.BaseType) then
    begin
    {AllowWriteln}
    writeln('TPasResolver.ComputeElement ',GetObjName(El),' typeof ResolvedEl.BaseType=',jsTypeOf(ResolvedEl.BaseType),' ResolvedEl=',GetResolverResultDbg(ResolvedEl));
    RaiseInternalError(20181101123527,jsTypeOf(ResolvedEl.LoTypeEl));
    {AllowWriteln-}
    end;
  {$ENDIF}
end;

function TPasResolver.Eval(Expr: TPasExpr; Flags: TResEvalFlags;
  Store: boolean): TResEvalValue;
// Important: Caller must free result with ReleaseEvalValue(Result)
begin
  Result:=fExprEvaluator.Eval(Expr,Flags);
  if Result=nil then exit;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.Eval Expr=',GetObjName(Expr),' Result=',Result.AsDebugString);
  {$ENDIF}

  if Store
      and (Expr.CustomData=nil)
      and (Result.Element=nil)
      and (not fExprEvaluator.IsSimpleExpr(Expr))
      and (Expr.GetModule=RootElement) then
    begin
    //writeln('TPasResolver.Eval STORE Expr=',GetObjName(Expr),' Result=',Result.AsDebugString);
    AddResolveData(Expr,Result,lkModule);
    end;
end;

function TPasResolver.Eval(const Value: TPasResolverResult;
  Flags: TResEvalFlags; Store: boolean): TResEvalValue;
var
  Expr: TPasExpr;
begin
  Result:=nil;
  if Value.ExprEl<>nil then
    Result:=Eval(Value.ExprEl,Flags,Store)
  else if Value.IdentEl is TPasConst then
    begin
    Expr:=TPasVariable(Value.IdentEl).Expr;
    if Expr=nil then exit;
    Result:=Eval(Expr,Flags,Store)
    end;
end;

function TPasResolver.IsSameType(TypeA, TypeB: TPasType;
  ResolveAlias: TPRResolveAlias): boolean;
begin
  if (TypeA=nil) or (TypeB=nil) then exit(false);
  case ResolveAlias of
  prraSimple:
    begin
    TypeA:=ResolveSimpleAliasType(TypeA);
    TypeB:=ResolveSimpleAliasType(TypeB);
    end;
  prraAlias:
    begin
    TypeA:=ResolveAliasType(TypeA);
    TypeB:=ResolveAliasType(TypeB);
    end;
  end;
  if TypeA=TypeB then exit(true);
  if (TypeA.ClassType=TPasUnresolvedSymbolRef)
      and (TypeB.ClassType=TPasUnresolvedSymbolRef) then
    begin
    Result:=CompareText(TypeA.Name,TypeB.Name)=0;
    exit;
    end;
  Result:=false;
end;

function TPasResolver.HasExactType(const ResolvedEl: TPasResolverResult
  ): boolean;
var
  IdentEl: TPasElement;
begin
  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl=nil then exit(false);
  if IdentEl is TPasVariable then
    exit(TPasVariable(IdentEl).VarType<>nil)
  else if IdentEl.ClassType=TPasArgument then
    exit(TPasArgument(IdentEl).ArgType<>nil)
  else if IdentEl.ClassType=TPasResultElement then
    exit(TPasResultElement(IdentEl).ResultType<>nil)
  else
    Result:=false;
end;

function TPasResolver.GetPasClassAncestor(ClassEl: TPasClassType;
  SkipAlias: boolean): TPasType;
var
  DeclEl: TPasElement;
  ClassScope: TPasClassScope;
begin
  Result:=nil;
  if ClassEl=nil then
    exit;
  if ClassEl.CustomData=nil then
    exit;
  if ClassEl.IsForward then
    begin
    DeclEl:=(ClassEl.CustomData as TResolvedReference).Declaration;
    ClassEl:=NoNil(DeclEl) as TPasClassType;
    Result:=ClassEl;
    end
  else
    begin
    ClassScope:=ClassEl.CustomData as TPasClassScope;
    if not (pcsfAncestorResolved in ClassScope.Flags) then
      exit;
    if SkipAlias then
      begin
      if ClassScope.AncestorScope=nil then
        exit;
      Result:=TPasClassType(ClassScope.AncestorScope.Element);
      end
    else
      Result:=ClassScope.DirectAncestor;
    end;
end;

function TPasResolver.ProcHasImplElements(Proc: TPasProcedure): boolean;
begin
  Result:=GetProcFirstImplEl(Proc)<>nil;
end;

function TPasResolver.IndexOfImplementedInterface(ClassEl: TPasClassType;
  aType: TPasType): integer;
var
  List: TFPList;
  i: Integer;
begin
  if aType=nil then exit(-1);
  aType:=ResolveAliasType(aType);
  List:=ClassEl.Interfaces;
  for i:=0 to List.Count-1 do
    if ResolveAliasType(TPasType(List[i]))=aType then
      exit(i);
  Result:=-1;
end;

function TPasResolver.GetLoop(El: TPasElement): TPasImplElement;
begin
  while El<>nil do
    begin
    if (El.ClassType=TPasImplRepeatUntil)
        or (El.ClassType=TPasImplWhileDo)
        or (El.ClassType=TPasImplForLoop) then
      exit(TPasImplElement(El));
    El:=El.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.ResolveAliasType(aType: TPasType): TPasType;
var
  C: TClass;
begin
  while aType<>nil do
    begin
    C:=aType.ClassType;
    if (C=TPasAliasType) or (C=TPasTypeAliasType) then
      aType:=TPasAliasType(aType).DestType
    else if (C=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData is TResolvedReference) then
      aType:=NoNil(TResolvedReference(aType.CustomData).Declaration) as TPasType
    else
      exit(aType);
    end;
  Result:=nil;
end;

function TPasResolver.ResolveAliasTypeEl(El: TPasElement): TPasType;
begin
  if (El is TPasType) then
    Result:=ResolveAliasType(TPasType(El))
  else
    Result:=nil;
end;

function TPasResolver.ExprIsAddrTarget(El: TPasExpr): boolean;
{ returns true if El is
  a) the last element of an @ operator expression
  e.g. '@p().o[].El' or '@El[]'
  b) mode delphi: the last element of a right side of an assignment
  c) an accessor function, e.g. property P read El;
}
var
  Parent: TPasElement;
  Prop: TPasProperty;
begin
  Result:=false;
  if El=nil then exit;
  if not IsNameExpr(El) then
    exit;
  repeat
    Parent:=El.Parent;
    //writeln('TPasResolver.ExprIsAddrTarget El=',GetObjName(El),' Parent=',GetObjName(Parent));
    if Parent.ClassType=TUnaryExpr then
      begin
      if TUnaryExpr(Parent).OpCode=eopAddress then exit(true);
      end
    else if Parent.ClassType=TBinaryExpr then
      begin
      if TBinaryExpr(Parent).right<>El then exit;
      if TBinaryExpr(Parent).OpCode<>eopSubIdent then exit;
      end
    else if Parent.ClassType=TParamsExpr then
      begin
      if TParamsExpr(Parent).Value<>El then exit;
      end
    else if Parent.ClassType=TPasProperty then
      begin
      Prop:=TPasProperty(Parent);
      Result:=(Prop.ReadAccessor=El) or (Prop.WriteAccessor=El) or (Prop.StoredAccessor=El);
      exit;
      end
    else if Parent.ClassType=TPasImplAssign then
      begin
      if TPasImplAssign(Parent).right<>El then exit;
      if (msDelphi in CurrentParser.CurrentModeswitches) then exit(true);
      exit;
      end
    else
      exit;
    El:=TPasExpr(Parent);
  until false;
end;

function TPasResolver.ParentNeedsExprResult(El: TPasExpr): boolean;
var
  C: TClass;
  P: TPasElement;
begin
  if (El=nil) or (El.Parent=nil) then exit(false);
  Result:=false;
  P:=El.Parent;
  C:=P.ClassType;
  if C=TBinaryExpr then
    begin
    if TBinaryExpr(P).right=El then
      begin
      if (TBinaryExpr(P).OpCode=eopSubIdent)
          or ((TBinaryExpr(P).OpCode=eopNone) and (TBinaryExpr(P).left is TInheritedExpr)) then
        Result:=ParentNeedsExprResult(TBinaryExpr(P))
      else
        Result:=true;
      end
    else
      Result:=true;
    end
  else if C.InheritsFrom(TPasExpr) then
    Result:=true
  else if (C=TPasEnumValue)
      or (C=TPasArgument)
      or (C=TPasVariable)
      or (C=TPasExportSymbol) then
    Result:=true
  else if C=TPasClassType then
    Result:=TPasClassType(P).GUIDExpr=El
  else if C=TPasProperty then
    Result:=(TPasProperty(P).IndexExpr=El)
        or (TPasProperty(P).DispIDExpr=El)
        or (TPasProperty(P).DefaultExpr=El)
  else if C=TPasProcedure then
    Result:=(TPasProcedure(P).LibraryExpr=El)
         or (TPasProcedure(P).DispIDExpr=El)
  else if C=TPasImplRepeatUntil then
    Result:=(TPasImplRepeatUntil(P).ConditionExpr=El)
  else if C=TPasImplIfElse then
    Result:=(TPasImplIfElse(P).ConditionExpr=El)
  else if C=TPasImplWhileDo then
    Result:=(TPasImplWhileDo(P).ConditionExpr=El)
  else if C=TPasImplWithDo then
    Result:=(TPasImplWithDo(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplCaseOf then
    Result:=(TPasImplCaseOf(P).CaseExpr=El)
  else if C=TPasImplCaseStatement then
    Result:=(TPasImplCaseStatement(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplForLoop then
    Result:=(TPasImplForLoop(P).StartExpr=El)
         or (TPasImplForLoop(P).EndExpr=El)
  else if C=TPasImplAssign then
    Result:=(TPasImplAssign(P).right=El)
  else if C=TPasImplRaise then
    Result:=(TPasImplRaise(P).ExceptAddr=El);
end;

function TPasResolver.GetReference_NewInstanceClass(Ref: TResolvedReference
  ): TPasClassType;
begin
  Result:=(Ref.Context as TResolvedRefCtxConstructor).Typ as TPasClassType;
end;

function TPasResolver.IsDynArray(TypeEl: TPasType; OptionalOpenArray: boolean
  ): boolean;
begin
  TypeEl:=ResolveAliasType(TypeEl);
  if (TypeEl=nil) or (TypeEl.ClassType<>TPasArrayType) then
    exit(false);
  if length(TPasArrayType(TypeEl).Ranges)<>0 then
    exit(false);
  if OptionalOpenArray and (proOpenAsDynArrays in Options) then
    Result:=true
  else
    Result:=(TypeEl.Parent=nil) or (TypeEl.Parent.ClassType<>TPasArgument);
end;

function TPasResolver.IsOpenArray(TypeEl: TPasType): boolean;
begin
  Result:=(TypeEl<>nil)
      and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0)
      and (TypeEl.Parent<>nil)
      and (TypeEl.Parent.ClassType=TPasArgument);
end;

function TPasResolver.IsDynOrOpenArray(TypeEl: TPasType): boolean;
begin
  TypeEl:=ResolveAliasType(TypeEl);
  Result:=(TypeEl<>nil) and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0);
end;

function TPasResolver.IsVarInit(Expr: TPasExpr): boolean;
var
  C: TClass;
begin
  Result:=false;
  if Expr=nil then exit;
  if Expr.Parent=nil then exit;
  C:=Expr.Parent.ClassType;
  if C.InheritsFrom(TPasVariable) then
    Result:=(TPasVariable(Expr.Parent).Expr=Expr)
  else if C=TPasArgument then
    Result:=(TPasArgument(Expr.Parent).ValueExpr=Expr);
end;

function TPasResolver.IsEmptyArrayExpr(const ResolvedEl: TPasResolverResult): boolean;
begin
  Result:=(ResolvedEl.BaseType in [btSet,btArrayOrSet,btArrayLit])
      and (ResolvedEl.SubType=btNone);
end;

function TPasResolver.IsClassMethod(El: TPasElement): boolean;
var
  C: TClass;
begin
  if El=nil then exit(false);
  C:=El.ClassType;;
  Result:=(C=TPasClassConstructor)
       or (C=TPasClassDestructor)
       or (C=TPasClassProcedure)
       or (C=TPasClassFunction)
       or (C=TPasClassOperator);
end;

function TPasResolver.IsClassField(El: TPasElement): boolean;
begin
  Result:=((El.ClassType=TPasVariable) or (El.ClassType=TPasConst))
    and ([vmClass,vmStatic]*TPasVariable(El).VarModifiers<>[])
    and (El.Parent is TPasClassType);
end;

function TPasResolver.IsExternalClass_Name(aClass: TPasClassType;
  const ExtName: string): boolean;
var
  AncestorScope: TPasClassScope;
begin
  Result:=false;
  if aClass=nil then exit;
  while (aClass<>nil) and aClass.IsExternal do
    begin
    if aClass.ExternalName=ExtName then exit(true);
    AncestorScope:=(aClass.CustomData as TPasClassScope).AncestorScope;
    if AncestorScope=nil then exit;
    aClass:=NoNil(AncestorScope.Element) as TPasClassType;
  end;
end;

function TPasResolver.IsProcedureType(const ResolvedEl: TPasResolverResult;
  HasValue: boolean): boolean;
var
  TypeEl: TPasType;
begin
  if (ResolvedEl.BaseType<>btContext) then
    exit(false);
  TypeEl:=ResolvedEl.LoTypeEl;
  if not (TypeEl is TPasProcedureType) then
    exit(false);
  if HasValue and not (rrfReadable in ResolvedEl.Flags) then
    exit(false);
  Result:=true;
end;

function TPasResolver.IsArrayType(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  Result:=(ResolvedEl.BaseType=btContext) and (ResolvedEl.LoTypeEl is TPasArrayType);
end;

function TPasResolver.IsArrayExpr(Expr: TParamsExpr): TPasArrayType;
var
  Ref: TResolvedReference;
begin
  Result:=nil;
  if Expr=nil then exit;
  if Expr.Kind<>pekSet then exit;
  if not (Expr.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(Expr.CustomData);
  if Ref.Declaration is TPasArrayType then
    Result:=TPasArrayType(Ref.Declaration);
end;

function TPasResolver.IsArrayOperatorAdd(Expr: TPasExpr): boolean;
begin
  Result:=(Expr<>nil) and (Expr.ClassType=TBinaryExpr) and (Expr.OpCode=eopAdd)
          and (msArrayOperators in GetElModeSwitches(Expr));
end;

function TPasResolver.IsTypeCast(Params: TParamsExpr): boolean;
var
  Value: TPasExpr;
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
begin
  Result:=false;
  if (Params=nil) or (Params.Kind<>pekFuncParams) then exit;
  Value:=Params.Value;
  if not IsNameExpr(Value) then
    exit;
  if not (Value.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(Value.CustomData);
  Decl:=Ref.Declaration;
  C:=Decl.ClassType;
  if (C=TPasAliasType) or (C=TPasTypeAliasType) then
    begin
    Decl:=ResolveAliasType(TPasAliasType(Decl));
    C:=Decl.ClassType;
    end;
  if (C=TPasProcedureType)
      or (C=TPasFunctionType) then
    exit(true)
  else if (C=TPasClassType)
      or (C=TPasClassOfType)
      or (C=TPasEnumType) then
    exit(true)
  else if (C=TPasUnresolvedSymbolRef)
      and (Decl.CustomData is TResElDataBaseType) then
    exit(true);
end;

function TPasResolver.IsInterfaceType(const ResolvedEl: TPasResolverResult;
  IntfType: TPasClassInterfaceType): boolean;
begin
  if ResolvedEl.BaseType<>btContext then exit(false);
  Result:=IsInterfaceType(ResolvedEl.LoTypeEl,IntfType);
end;

function TPasResolver.IsInterfaceType(TypeEl: TPasType;
  IntfType: TPasClassInterfaceType): boolean;
begin
  if TypeEl=nil then exit(false);
  TypeEl:=ResolveAliasType(TypeEl);
  Result:=(TypeEl.ClassType=TPasClassType)
    and (TPasClassType(TypeEl).ObjKind=okInterface)
    and (TPasClassType(TypeEl).InterfaceType=IntfType);
end;

function TPasResolver.IsTGUID(RecTypeEl: TPasRecordType): boolean;
var
  Members: TFPList;
  El: TPasElement;
begin
  Result:=false;
  if not SameText(RecTypeEl.Name,'TGUID') then exit;
  if SameText(RecTypeEl.GetModule.Name,'system') then exit(true);
  Members:=RecTypeEl.Members;
  if Members.Count<4 then exit;
  El:=TPasElement(Members[0]);
  if not SameText(El.Name,'D1') then exit;
  El:=TPasElement(Members[1]);
  if not SameText(El.Name,'D2') then exit;
  El:=TPasElement(Members[2]);
  if not SameText(El.Name,'D3') then exit;
  El:=TPasElement(Members[3]);
  if not SameText(El.Name,'D4') then exit;
  Result:=true;
end;

function TPasResolver.IsTGUIDString(const ResolvedEl: TPasResolverResult
  ): boolean;
var
  TypeEl: TPasType;
  C: TClass;
  IdentEl: TPasElement;
begin
  if not (ResolvedEl.BaseType in btAllStrings) then
    exit(false);
  if (ResolvedEl.ExprEl<>nil) and (ResolvedEl.LoTypeEl<>nil) then
    exit(true); // untyped string literal
  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl<>nil then
    begin
    C:=IdentEl.ClassType;
    if C.InheritsFrom(TPasVariable) then
      TypeEl:=TPasVariable(IdentEl).VarType
    else if C=TPasArgument then
      TypeEl:=TPasArgument(IdentEl).ArgType
    else if C=TPasResultElement then
      TypeEl:=TPasResultElement(IdentEl).ResultType
    else
      TypeEl:=nil;
    while TypeEl<>nil do
      begin
      if (TypeEl.ClassType=TPasAliasType)
          or (TypeEl.ClassType=TPasTypeAliasType) then
        begin
        if SameText(TypeEl.Name,'TGUIDString') then
          exit(true);
        TypeEl:=TPasAliasType(TypeEl).DestType;
        end
      else
        break;
      end;
    end;
  Result:=false;
end;

function TPasResolver.ProcNeedsParams(El: TPasProcedureType): boolean;
begin
  Result:=(El.Args.Count>0) and (TPasArgument(El.Args[0]).ValueExpr=nil);
end;

function TPasResolver.IsProcOverride(AncestorProc, DescendantProc: TPasProcedure
  ): boolean;
var
  Proc, OverriddenProc: TPasProcedure;
begin
  Result:=false;
  Proc:=DescendantProc;
  if not Proc.IsOverride then exit;
  if not AncestorProc.IsOverride and not AncestorProc.IsVirtual then exit;
  repeat
    OverriddenProc:=TPasProcedureScope(Proc.CustomData).OverriddenProc;
    if AncestorProc=OverriddenProc then exit(true);
    Proc:=OverriddenProc;
  until Proc=nil;
end;

function TPasResolver.GetTopLvlProc(El: TPasElement): TPasProcedure;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El is TPasProcedure then
      Result:=TPasProcedure(El);
    El:=El.Parent;
    end;
end;

function TPasResolver.GetRangeLength(RangeExpr: TPasExpr): TMaxPrecInt;
var
  Range: TResEvalValue;
begin
  Result:=0;
  Range:=Eval(RangeExpr,[refConst]);
  if Range=nil then
    RaiseNotYetImplemented(20170910210416,RangeExpr);
  try
    case Range.Kind of
    revkRangeInt:
      Result:=TResEvalRangeInt(Range).RangeEnd-TResEvalRangeInt(Range).RangeStart+1;
    revkRangeUInt:
      Result:=TResEvalRangeUInt(Range).RangeEnd-TResEvalRangeUInt(Range).RangeStart+1;
    else
      RaiseNotYetImplemented(20170910210554,RangeExpr);
    end;
  finally
    ReleaseEvalValue(Range);
  end;
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  //if Result=0 then
    writeln('TPasResolver.GetRangeLength Result=',Result);
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasResolver.EvalRangeLimit(RangeExpr: TPasExpr; Flags: TResEvalFlags;
  EvalLow: boolean; ErrorEl: TPasElement): TResEvalValue;
var
  Range: TResEvalValue;
  EnumType: TPasEnumType;
begin
  Result:=nil;
  Range:=Eval(RangeExpr,Flags+[refConst]);
  if Range=nil then
    RaiseNotYetImplemented(20170601191258,RangeExpr);
  case Range.Kind of
  revkRangeInt:
    case TResEvalRangeInt(Range).ElKind of
      revskEnum:
        begin
        EnumType:=NoNil(TResEvalRangeInt(Range).ElType) as TPasEnumType;
        if EvalLow then
          Result:=TResEvalEnum.CreateValue(
            TResEvalRangeInt(Range).RangeStart,TPasEnumValue(EnumType.Values[0]))
        else
          Result:=TResEvalEnum.CreateValue(
            TResEvalRangeInt(Range).RangeEnd,
            TPasEnumValue(EnumType.Values[EnumType.Values.Count-1]));
        end;
      revskInt:
        if EvalLow then
          Result:=TResEvalInt.CreateValue(TResEvalRangeInt(Range).RangeStart)
        else
          Result:=TResEvalInt.CreateValue(TResEvalRangeInt(Range).RangeEnd);
      revskChar:
        {$ifdef FPC_HAS_CPSTRING}
        if TResEvalRangeInt(Range).RangeEnd<256 then
          begin
          if EvalLow then
            Result:=TResEvalString.CreateValue(chr(TResEvalRangeInt(Range).RangeStart))
          else
            Result:=TResEvalString.CreateValue(chr(TResEvalRangeInt(Range).RangeEnd));
          end
        else
        {$endif}
          begin
          if EvalLow then
            Result:=TResEvalUTF16.CreateValue(widechar(TResEvalRangeInt(Range).RangeStart))
          else
            Result:=TResEvalUTF16.CreateValue(widechar(TResEvalRangeInt(Range).RangeEnd));
          end;
      revskBool:
        if EvalLow then
          Result:=TResEvalBool.CreateValue(TResEvalRangeInt(Range).RangeStart<>0)
        else
          Result:=TResEvalBool.CreateValue(TResEvalRangeInt(Range).RangeEnd<>0);
    else
      ReleaseEvalValue(Range);
      RaiseNotYetImplemented(20170601195240,ErrorEl);
    end;
  revkRangeUInt:
    if EvalLow then
      Result:=TResEvalUInt.CreateValue(TResEvalRangeUInt(Range).RangeStart)
    else
      Result:=TResEvalUInt.CreateValue(TResEvalRangeUInt(Range).RangeEnd);
  else
    ReleaseEvalValue(Range);
    RaiseNotYetImplemented(20170601195336,ErrorEl);
  end;
  ReleaseEvalValue(Range);
end;

function TPasResolver.EvalTypeRange(Decl: TPasType; Flags: TResEvalFlags
  ): TResEvalValue;
var
  C: TClass;
  BaseTypeData: TResElDataBaseType;
begin
  Result:=nil;
  Decl:=ResolveAliasType(Decl);
  C:=Decl.ClassType;
  if C=TPasRangeType then
    begin
    Result:=fExprEvaluator.Eval(TPasRangeType(Decl).RangeExpr,Flags);
    if (Result<>nil) and (Result.IdentEl=nil) then
      begin
      Result.IdentEl:=Decl;
      exit;
      end;
    end
  else if C=TPasEnumType then
    begin
    Result:=TResEvalRangeInt.CreateValue(revskEnum,TPasEnumType(Decl),
                                         0,TPasEnumType(Decl).Values.Count-1);
    Result.IdentEl:=Decl;
    exit;
    end
  else if C=TPasUnresolvedSymbolRef then
    begin
    if (Decl.CustomData is TResElDataBaseType) then
      begin
      BaseTypeData:=TResElDataBaseType(Decl.CustomData);
      case BaseTypeData.BaseType of
      btChar:
        begin
        Result:=TResEvalRangeInt.Create;
        TResEvalRangeInt(Result).ElKind:=revskChar;
        TResEvalRangeInt(Result).RangeStart:=0;
        {$ifdef FPC_HAS_CPSTRING}
        if BaseTypeChar in [btChar,btAnsiChar] then
          TResEvalRangeInt(Result).RangeEnd:=$ff
        else
        {$endif}
          TResEvalRangeInt(Result).RangeEnd:=$ffff;
        end;
      {$ifdef FPC_HAS_CPSTRING}
      btAnsiChar:
        Result:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff);
      {$endif}
      btWideChar:
        Result:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff);
      btBoolean,btByteBool,btWordBool{$ifdef HasInt64},btQWordBool{$endif}:
        Result:=TResEvalRangeInt.CreateValue(revskBool,nil,0,1);
      btByte,
      btShortInt,
      btWord,
      btSmallInt,
      btLongWord,
      btLongint,
      {$ifdef HasInt64}
      btInt64,
      btComp,
      {$endif}
      btIntSingle,
      btUIntSingle,
      btIntDouble,
      btUIntDouble:
        begin
        Result:=TResEvalRangeInt.Create;
        TResEvalRangeInt(Result).ElKind:=revskInt;
        GetIntegerRange(BaseTypeData.BaseType,
          TResEvalRangeInt(Result).RangeStart,TResEvalRangeInt(Result).RangeEnd);
        end;
      end;
      end;
    end;
end;

function TPasResolver.HasTypeInfo(El: TPasType): boolean;
begin
  Result:=false;
  if El=nil then exit;
  if El.CustomData is TResElDataBaseType then
    exit(true); // base type
  if El.Parent=nil then exit;
  if (El.Parent is TPasType) and not HasTypeInfo(TPasType(El.Parent)) then
    exit;
  Result:=true;
end;

function TPasResolver.GetActualBaseType(bt: TResolverBaseType
  ): TResolverBaseType;
begin
  case bt of
  btChar: Result:=BaseTypeChar;
  btString: Result:=BaseTypeString;
  btExtended: Result:=BaseTypeExtended;
  else Result:=bt;
  end;
end;

function TPasResolver.GetCombinedBoolean(Bool1, Bool2: TResolverBaseType;
  ErrorEl: TPasElement): TResolverBaseType;
begin
  if Bool1=Bool2 then exit(Bool1);
  case Bool1 of
  btBoolean: Result:=Bool2;
  btByteBool: if Bool2<>btBoolean then Result:=Bool2;
  btWordBool: if not (Bool2 in [btBoolean,btByteBool]) then Result:=Bool2;
  btLongBool: if not (Bool2 in [btBoolean,btByteBool,btWordBool]) then Result:=Bool2;
  {$ifdef HasInt64}
  btQWordBool: if not (Bool2 in [btBoolean,btByteBool,btWordBool,btLongBool]) then Result:=Bool2;
  {$endif}
  else
    RaiseNotYetImplemented(20170420093805,ErrorEl);
  end;
end;

function TPasResolver.GetCombinedInt(const Int1, Int2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  Precision1, Precision2: word;
  Signed1, Signed2: boolean;
begin
  if Int1.BaseType=Int2.BaseType then exit;
  GetIntegerProps(Int1.BaseType,Precision1,Signed1);
  GetIntegerProps(Int2.BaseType,Precision2,Signed2);
  if Precision1=Precision2 then
    begin
    if Signed1<>Signed2 then
      Precision1:=Max(Precision1,Precision2)+1;
    end;
  Result:=GetIntegerBaseType(Max(Precision1,Precision2),Signed1 or Signed2,ErrorEl);
end;

procedure TPasResolver.GetIntegerProps(bt: TResolverBaseType; out
  Precision: word; out Signed: boolean);
begin
  case bt of
  btByte: begin Precision:=8; Signed:=false; end;
  btShortInt: begin Precision:=8; Signed:=true; end;
  btWord: begin Precision:=16; Signed:=false; end;
  btSmallInt: begin Precision:=16; Signed:=true; end;
  btIntSingle: begin Precision:=23; Signed:=true; end;
  btUIntSingle: begin Precision:=22; Signed:=false; end;
  btLongWord: begin Precision:=32; Signed:=false; end;
  btLongint: begin Precision:=32; Signed:=true; end;
  btIntDouble: begin Precision:=53; Signed:=true; end;
  btUIntDouble: begin Precision:=52; Signed:=false; end;
  {$ifdef HasInt64}
  btQWord: begin Precision:=64; Signed:=false; end;
  btInt64,btComp: begin Precision:=64; Signed:=true; end;
  {$endif}
  else
    RaiseInternalError(20170420095727);
  end;
end;

function TPasResolver.GetIntegerRange(bt: TResolverBaseType; out MinVal,
  MaxVal: TMaxPrecInt): boolean;
begin
  Result:=true;
  if bt=btExtended then bt:=BaseTypeExtended;
  case bt of
  btByte: begin MinVal:=Low(byte); MaxVal:=High(byte); end;
  btShortInt: begin MinVal:=low(ShortInt); MaxVal:=high(ShortInt); end;
  btWord: begin MinVal:=low(word); MaxVal:=high(word); end;
  btSmallInt: begin MinVal:=low(SmallInt); MaxVal:=high(SmallInt); end;
  btLongWord: begin MinVal:=low(LongWord); MaxVal:=high(LongWord); end;
  btLongint: begin MinVal:=low(LongInt); MaxVal:=high(LongInt); end;
  {$ifdef HasInt64}
  btInt64,
  btComp: begin MinVal:=low(int64); MaxVal:=high(int64); end;
  {$endif}
  btSingle,btIntSingle: begin MinVal:=MinSafeIntSingle; MaxVal:=MaxSafeIntSingle; end;
  btUIntSingle: begin MinVal:=0; MaxVal:=MaxSafeIntSingle; end;
  btDouble,btIntDouble: begin MinVal:=MinSafeIntDouble; MaxVal:=MaxSafeIntDouble; end;
  btUIntDouble: begin MinVal:=0; MaxVal:=MaxSafeIntDouble; end;
  btCurrency: begin MinVal:=MinSafeIntCurrency; MaxVal:=MaxSafeIntCurrency; end;
  else
    Result:=false;
  end;
end;

function TPasResolver.GetIntegerBaseType(Precision: word; Signed: boolean;
  ErrorEl: TPasElement): TResolverBaseType;
begin
  if Precision<=8 then
    begin
    if Signed then
      Result:=btShortInt
    else
      Result:=btByte;
    if BaseTypes[Result]<>nil then exit;
    end;
  if Precision<=16 then
    begin
    if Signed then
      Result:=btSmallInt
    else
      Result:=btWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  if (Precision<=22) and (not Signed) and (BaseTypes[btUIntSingle]<>nil) then
    exit(btUIntSingle);
  if (Precision<=23) and Signed and (BaseTypes[btIntSingle]<>nil) then
    exit(btIntSingle);
  if Precision<=32 then
    begin
    if Signed then
      Result:=btLongint
    else
      Result:=btLongWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  if (Precision<=52) and (not Signed) and (BaseTypes[btUIntDouble]<>nil) then
    exit(btUIntDouble);
  if (Precision<=53) and Signed and (BaseTypes[btIntDouble]<>nil) then
    exit(btIntDouble);
  {$ifdef HasInt64}
  if Precision<=64 then
    begin
    if Signed then
      Result:=btInt64
    else
      Result:=btQWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  {$endif}
  RaiseRangeCheck(20170420100336,ErrorEl);
end;

function TPasResolver.GetSmallestIntegerBaseType(MinVal, MaxVal: TMaxPrecInt
  ): TResolverBaseType;
var
  V: TMaxPrecInt;
begin
  if MinVal>MaxVal then
    MinVal:=MaxVal;
  if MinVal<0 then
    begin
    if MaxVal>-(MinVal+1) then
      V:=MaxVal
    else
      V:=-(MinVal+1);
    if V<=high(ShortInt) then
      Result:=btShortInt
    else if V<=high(SmallInt) then
      Result:=btSmallInt
    else if (BaseTypes[btIntSingle]<>nil) and (V<MaxSafeIntSingle) then
      Result:=btIntSingle
    else if V<=High(Longint) then
      Result:=btLongint
    else if (BaseTypes[btIntDouble]<>nil) and (V<MaxSafeIntDouble) then
      Result:=btIntDouble
    else
      Result:=btIntMax;
    end
  else
    begin
    V:=MaxVal;
    if V<=high(Byte) then
      Result:=btByte
    else if V<=high(Word) then
      Result:=btWord
    else if (BaseTypes[btUIntSingle]<>nil) and (V<MaxSafeIntSingle) then
      Result:=btUIntSingle
    else if V<=High(LongWord) then
      Result:=btLongWord
    else if (BaseTypes[btUIntDouble]<>nil) and (V<MaxSafeIntDouble) then
      Result:=btUIntDouble
    else
      Result:=btIntMax;
    end;
end;

function TPasResolver.GetCombinedChar(const Char1, Char2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  bt1, bt2: TResolverBaseType;
begin
  bt1:=GetActualBaseType(Char1.BaseType);
  bt2:=GetActualBaseType(Char2.BaseType);
  if bt1=bt2 then exit(bt1);
  if not (bt1 in btAllChars) then
    RaiseInternalError(20170420103128);
  Result:=btWideChar;
  if Result=BaseTypeChar then
    Result:=btChar;
  if ErrorEl=nil then ;
end;

function TPasResolver.GetCombinedString(const Str1, Str2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  bt1, bt2: TResolverBaseType;
begin
  bt1:=GetActualBaseType(Str1.BaseType);
  bt2:=GetActualBaseType(Str2.BaseType);
  if bt1=bt2 then exit(bt1);
  case bt1 of
  {$ifdef FPC_HAS_CPSTRING}
  btAnsiChar:
    case bt2 of
    btChar: Result:=btChar;
    btWideChar: Result:=btWideChar;
    else Result:=bt2;
    end;
  {$endif}
  btWideChar:
    case bt2 of
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar: Result:=btWideChar;
    {$endif}
    btWideString: Result:=btWideString;
    btString,btUnicodeString
    {$ifdef FPC_HAS_CPSTRING},btShortString,btAnsiString,btRawByteString{$endif}:
        Result:=btUnicodeString;
    else RaiseNotYetImplemented(20170420103808,ErrorEl);
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btShortString:
    case bt2 of
    btChar,btAnsiChar: Result:=btShortString;
    btString,btAnsiString: Result:=btAnsiString;
    btRawByteString: Result:=btRawByteString;
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420120937,ErrorEl);
    end;
  {$endif}
  btString{$ifdef FPC_HAS_CPSTRING},btAnsiString{$endif}:
    case bt2 of
    {$ifdef FPC_HAS_CPSTRING}
    btChar,btAnsiChar,btString,btShortString,btRawByteString: Result:=btAnsiString;
    {$endif}
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420121201,ErrorEl);
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btRawByteString:
    case bt2 of
    btChar,btAnsiChar,btRawByteString,btShortString: Result:=btRawByteString;
    btString,btAnsiString: Result:=btAnsiString;
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420121352,ErrorEl);
    end;
  {$endif}
  btWideString:
    case bt2 of
    btChar,btWideChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,btShortString,{$endif}btWideString:
      Result:=btWideString;
    btString,{$ifdef FPC_HAS_CPSTRING}btAnsiString,{$endif}btUnicodeString:
      Result:=btUnicodeString;
    else RaiseNotYetImplemented(20170420121532,ErrorEl);
    end;
  btUnicodeString:
    Result:=btUnicodeString;
  else
    RaiseNotYetImplemented(20170420103153,ErrorEl);
  end;
  if Result=BaseTypeChar then
    Result:=btChar
  else if Result=BaseTypeString then
    Result:=btString;
end;

function TPasResolver.IsElementSkipped(El: TPasElement): boolean;
begin
  Result:=El=nil;
end;

function TPasResolver.FindLocalBuiltInSymbol(El: TPasElement): TPasElement;
var
  Data: TObject;
begin
  Data:=El.CustomData;
  if Data=nil then
    RaiseInternalError(20180215185302,GetObjName(El));
  if Data.ClassType=TResElDataBaseType then
    Result:=BaseTypes[TResElDataBaseType(Data).BaseType]
  else if Data.ClassType=TResElDataBuiltInProc then
    Result:=BuiltInProcs[TResElDataBuiltInProc(Data).BuiltIn].Element
  else
    Result:=nil;
end;

function TPasResolver.GetLastSection: TPasSection;
var
  Module: TPasModule;
begin
  Result:=nil;
  Module:=RootElement;
  if Module=nil then exit;
  if Module is TPasProgram then
    Result:=TPasProgram(Module).ProgramSection
  else if Module is TPasLibrary then
    Result:=TPasLibrary(Module).LibrarySection
  else if Module.ImplementationSection<>nil then
    Result:=Module.ImplementationSection
  else
    Result:=Module.InterfaceSection;
end;

function TPasResolver.CheckSrcIsADstType(const ResolvedSrcType,
  ResolvedDestType: TPasResolverResult; ErrorEl: TPasElement): integer;
// finds distance between classes SrcType and DestType
begin
  Result:=CheckClassIsClass(ResolvedSrcType.LoTypeEl,ResolvedDestType.LoTypeEl,ErrorEl);
end;

function TPasResolver.CheckClassIsClass(SrcType, DestType: TPasType;
  ErrorEl: TPasElement): integer;
// check if Src is equal or descends from Dest
var
  ClassEl: TPasClassType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckClassIsClass SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
  {$ENDIF}
  if DestType=nil then exit(cIncompatible);
  DestType:=ResolveAliasType(DestType);

  Result:=cExact;
  while SrcType<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    writeln(' Step=',Result,' SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
    {$ENDIF}
    if SrcType=DestType then
      exit
    else if SrcType.ClassType=TPasAliasType then
      // alias -> skip
      SrcType:=TPasAliasType(SrcType).DestType
    else if SrcType.ClassType=TPasTypeAliasType then
      begin
      // type alias -> increases distance
      SrcType:=TPasAliasType(SrcType).DestType;
      inc(Result);
      end
    else if SrcType.ClassType=TPasClassType then
      begin
      ClassEl:=TPasClassType(SrcType);
      if ClassEl.IsForward then
        // class forward -> skip
        SrcType:=(ClassEl.CustomData as TResolvedReference).Declaration as TPasType
      else
        begin
        // class ancestor -> increase distance
        SrcType:=(ClassEl.CustomData as TPasClassScope).DirectAncestor;
        inc(Result);
        end;
      end
    else
      exit(cIncompatible);
    end;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
end;

function TPasResolver.CheckClassesAreRelated(TypeA, TypeB: TPasType;
  ErrorEl: TPasElement): integer;
begin
  Result:=CheckClassIsClass(TypeA,TypeB,ErrorEl);
  if Result<>cIncompatible then exit;
  Result:=CheckClassIsClass(TypeB,TypeA,ErrorEl);
end;

function TPasResolver.GetClassImplementsIntf(ClassEl, Intf: TPasClassType
  ): TPasClassType;
begin
  Result:=nil;
  while ClassEl<>nil do
    begin
    if IndexOfImplementedInterface(ClassEl,Intf)>=0 then
      exit(ClassEl);
    ClassEl:=GetPasClassAncestor(ClassEl,true) as TPasClassType;
    end;
end;

end.


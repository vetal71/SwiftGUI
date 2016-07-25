unit SWIFT_UMsgFormat;

interface

uses
  SysUtils, Generics.Collections, Generics.Defaults, XMLIntf, XMLDoc,
  synautil, Classes, Controls, StdCtrls;

type
  ESwiftException = class(Exception);
  ESwiftValidatorException = class(ESwiftException);

  TSwiftMessage = class;

  // базовый класс по проверке объекта (поля или сообщения)
  TSwiftValidator = class abstract
  protected
    procedure Error(const aText: string); virtual; abstract;
  public
    constructor Create(aSubject: TObject); virtual;
    function Valid(): Boolean; virtual; abstract;
  end;

  TSwiftValidatorClass = class of TSwiftValidator;
  TSwiftValidatorClassArray = TArray<TSwiftValidatorClass>;

  TSwiftValidateElement = class
  protected
    function Validate(aValidators: TSwiftValidatorClassArray): Boolean; virtual;
  end;

  TSwiftBlock = class abstract(TSwiftValidateElement)
  protected
    FColumn: Integer;
    FLine: Integer;
    FMessage: TSwiftMessage;
    FNumber: Integer;
    FText: string;
    procedure Error(aLine: Integer; const aText: string); overload;
    procedure Error(aLine: Integer; const aFormat: string; const aArgs: array of const); overload;
  public
    constructor Create(aMessage: TSwiftMessage); virtual;
    function Valid(): Boolean; virtual; abstract;
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
    property Number: Integer read FNumber;
    property Text: string read FText write FText;
  public

  end;

  // базовый класс для блоков 1 и 2
  TSwiftValueBlock = class abstract(TSwiftBlock);

  TSwiftBlock1 = class(TSwiftValueBlock)
  private
//    FText: string;
    function GetSender(): string;
  public
    constructor Create(aMessage: TSwiftMessage; const aText: string); reintroduce; virtual;
    function Valid(): Boolean; override;
    property Sender: string read GetSender;
  end;

  TSwiftBlock2 = class(TSwiftValueBlock)
  private
//    FText: string;
    function GetReciever(): string;
  public
    constructor Create(aMessage: TSwiftMessage; const aText: string); reintroduce; virtual;
    function Valid(): Boolean; override;
    property Reciever: string read GetReciever;
  end;

  TSwiftTag = class
  protected
    type
      TSwiftTagList = class(TObjectList<TSwiftTag>);
  private
    FColumn: Integer;
    FLine: Integer;
    FName: string;
    FSequence: Boolean;
    FValue: string;
    FQualifier: string;
    FTags: TSwiftTagList;
    FParentSequence: TSwiftTag;
    FEditor: TCustomEdit;
    FFullName: string;                                                          // полное имя

    procedure SetEditor(aValue : TCustomEdit);
    function GetEditor: TCustomEdit;

  public
    constructor Create(const aData: string);
    destructor Destroy(); override;
    function GetTagBy(const aName: string; aOption: Boolean = False): TSwiftTag;//???
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
    property Name: string read FName write FName;
    property Sequence: Boolean read FSequence write FSequence;
    property Value: string read FValue write FValue;
    property Tags: TSwiftTagList read FTags;
    property Qualifier: string read FQualifier write FQualifier;
    property ParentSequence: TSwiftTag read FParentSequence write FParentSequence;
    property FullName: string read FFullName write FFullName;

    {Добавление редактора для тэга}
    property Editor : TCustomEdit read GetEditor write SetEditor;
  end;

  TSwiftField = class(TSwiftValidateElement)
  protected
    FMessage: TSwiftMessage;
    FTag: TSwiftTag;
  protected
    procedure Error(aLine: Integer; const aText: string); overload;
    procedure Error(aLine: Integer;
      const aFormat: string; const aArgs: array of const); overload;
  public
    constructor Create(aMessage: TSwiftMessage; aTag: TSwiftTag); virtual;
    function Valid(): Boolean; virtual;
    property Message: TSwiftMessage read FMessage;
    property Tag: TSwiftTag read FTag;
  end;

  // базовый класс для блоков 3, 4 и 5
  TSwiftTagListBlock = class abstract(TSwiftBlock)
  protected
    type
      TSwiftTagList = class(TObjectList<TSwiftTag>);
      TSwiftFieldList = class(TObjectList<TSwiftField>);
    var
      FTags: TSwiftTagList;
      FSequence: TSwiftTag; // текущая
      function FieldBy(aTag: TSwiftTag): TSwiftField;
      function GetCount(): Integer;
      function GetTag(Index: Integer): TSwiftTag;
      function GetFieldList(): TSwiftFieldList;
  public
    constructor Create(aMessage: TSwiftMessage); override;
    destructor Destroy(); override;

    function AddTag(aTag: TSwiftTag; aSequence: TSwiftTag = nil): TSwiftTag;
    function GetTagBy(const aName: string; aOption: Boolean = False): TSwiftTag;
    function GetFieldBy(const aName: string): TSwiftField;
    function GetSequence(const aName: string): TSwiftTag;
    function ExistTag(const aName: string): Boolean;
    function ExistOptionTag(const aName: string): Boolean;
    function Valid(): Boolean; override;

    property Count: Integer read GetCount;
    property Tags[Index: Integer]: TSwiftTag read GetTag; default;
  end;

  TSwiftBlock3 = class(TSwiftTagListBlock)
  public
    constructor Create(aMessage: TSwiftMessage); override;
  end;

  TSwiftBlock4 = class(TSwiftTagListBlock)
  protected
    type
      TSwiftDict = TDictionary<string,TSwiftTag>;
  var
    FSwiftDict: TSwiftDict;
    procedure FillSwiftDict;
  public
    constructor Create(aMessage: TSwiftMessage); override;
    destructor Destroy(); override;

    function Valid(): Boolean; override;

    function GetFieldValueBy(const aName: string): string;
    function GetFieldByEx(const aFullName: string): TSwiftField;
    function ExistTagBy(const aName: string): Boolean;
    function ExistOptionTagBy(const aName: string): Boolean;
    function SwiftDictToString: string;
    procedure RecreateTags;

  end;

  TSwiftBlock5 = class(TSwiftTagListBlock)
  public
    constructor Create(aMessage: TSwiftMessage); override;
  end;

  // структура описывающая ошибку в swift сообщении
  TSwiftError = record
    Level: Integer; {при разбиении текста (0), при валидации блоков (1), ...}
    Line, Column: Integer;
    Text: string;
    Number: Integer;
    constructor Create(const aText: string; aLevel, aLine, aColumn: Integer; aNumber: Integer);
    function ToString(): string;
  end;

  TSwiftErorrComparer = class(TComparer<TSwiftError>)
  public
    function Compare(const Left, Right: TSwiftError): Integer; override;
  end;

  TSwiftErrorList = class(TList<TSwiftError>)
  public
    function ToString(): string; override;
  end;

  {$REGION 'Классы валидации полей SWIFT сообщения, нижний уровень'}
  // базовый класс по проверке полей сообщения определенному правилу
  TSwiftFieldValidator = class abstract(TSwiftValidator)
  private
    function ValidateNumber(ANumPos: Integer; out AError: string): Boolean;
  protected
    FField: TSwiftField;
    procedure Error(const aText: string); override;
  public
    constructor Create(aSubject: TObject); override;
  end;

  // имя поля не может быть пустым и соответствует формату \d{2}[A-Z]?
  TSwiftFieldNameValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // поле не должно быть пустым, а длина строки поля не превышать 35 символов
  // поля 15A, 15B, 15C, ... должны буть всегда пустыми
  // для поля 79 разрешено в строке содержать до 50 символов, а не 35
  TSwiftFieldValueValidator = class(TSwiftFieldValidator)
  private
    procedure Error2(aLine: Integer; const aText: string);
  public
    function Valid(): Boolean; override;
  end;

  // проверка поля на соответствие шаблону из базы, формат regex
  // поля 15A, 15B, 15C, ... игнорируются
  TSwiftPatternValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwiftCheckingForSlashesValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // поле должно содержать одно из следующих кодов "Y,N"
  TSwift17TValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift17UValidator = class(TSwift17TValidator);

  // для SWIFT сообщения мт300, поле должно содержать одно из следующих кодов:
  // "NEWT,EXOP,DUPL,CANC,AMND"
  // для SWIFT сообщения мт320, поле должно содержать одно из следующих кодов:
  // "NEWT,DUPL,CANC,AMND"
  TSwift22AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // коды должны располагаться в алфавитном порядке
  TSwift22CValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // дата в поле должна соответствовать формату YYYYMMDD
  TSwift30TValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift30VValidator = class(TSwift30TValidator);

  // дата в поле должна соответствовать формату YYMMDD
  TSwift30Validator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // неверный разделитель дробной части "." (проверка по шаблону)
  TSwift36Validator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift32BValidator = class(TSwift36Validator);
  TSwift33BValidator = class(TSwift36Validator);

  // значение поля не соответствует значению SWIFT-кода банка
  TSwift82AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // неверный SWIFT-код (не найден в справочнике субъектов(финансовых оргнаизаций))
  // было исключение в мт210
  TSwift56AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift57AValidator = class(TSwift56AValidator);
  TSwift87AValidator = class(TSwift56AValidator);

  TSwift58AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // поле должно содержать одно из следующих кодов "AGNT,BILA,BROK"
  TSwift94AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  // поле
  TSwift35BValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift90AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift90BValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift19AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift92AValidator = class(TSwiftFieldValidator)
  public
    function Valid(): Boolean; override;
  end;

  {$ENDREGION}

  {$REGION 'Классы валидации SWIFT сообщения, верхний уровень'}
  // базовый класс по проверке сообщения определенному правилу
  TSwiftMessageValidator = class abstract(TSwiftValidator)
  protected
    FMessage: TSwiftMessage;
    procedure Error(const aText: string); override;
  public
    constructor Create(aSubject: TObject); override;
  end;

  // проверка на наличие обязательных полей в тексте сообщения
  TSwiftMandatoryFieldsValidator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  // проверка на наличие в тексте сообщения 79 поля или копии обязательных
  // используется в сообщениях MT(392, 395, 396)
  TSwift79OrCopyValidator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  // правило сети С1 - отсутствует обязательное поле :21:
  TSwift22AAnd21Validator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift22CAnd82AValidator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwift22CAnd87AValidator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  TSwiftSQLQueryValidator = class(TSwiftMessageValidator)
  private
    function CheckElement(const aRule, aValue: string): Boolean;
    function TryGetSwiftTag(const aSequence, aTag: string; aOption: Boolean;
      out aSwiftTag: TSwiftTag): Boolean;
  public
    class function LoadXMLTemplate(aMsgTypeID: Integer): IXMLDocument;
    function Valid(): Boolean; override;
  end;

  // правило сети С1 - отсутствует обязательное поле :21: (от 22A и 22B)
  TSwift22ABAnd21Validator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  // проверка структуры для МТ5nn
  TSwiftStructureValidate = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  // проверка обязательного поля 22F если получатель EUROCLEAR
  TSwit22FValidator = class(TSwiftMessageValidator)
  public
    function Valid(): Boolean; override;
  end;

  {$ENDREGION}

  TSwiftFieldProc = reference to procedure (aField: TSwiftField);
  TSwiftFieldsProc = class(TDictionary<string, TSwiftFieldProc>);

  TSwiftMessage = class(TSwiftValidateElement)
  protected
    type
      TSwiftBlockList = class(TObjectList<TSwiftBlock>);
    var
      FBlocks: TSwiftBlockList;
      FBlock1: TSwiftBlock1;
      FBlock2: TSwiftBlock2;
      FBlock4: TSwiftBlock4;
      FErrors: TSwiftErrorList;
      FFieldsProc: TSwiftFieldsProc;
      FMsgType: Integer;
      function GetBlock(Index: Integer): TSwiftBlock;
      function GetCount(): Integer;

      function GetMsgText: string;

  public
    constructor Create(aMsgType: Integer); virtual;
    destructor Destroy(); override;

    function AddBlock(aBlock: TSwiftBlock): TSwiftBlock;
    function Valid(): Boolean; virtual;

    property Block1: TSwiftBlock1 read FBlock1;
    property Block2: TSwiftBlock2 read FBlock2;
    property Block4: TSwiftBlock4 read FBlock4;
    property Blocks[Index: Integer]: TSwiftBlock read GetBlock;
    property Count: Integer read GetCount;
    property Errors: TSwiftErrorList read FErrors;
    property FieldsProc: TSwiftFieldsProc read FFieldsProc;
    property MsgType: Integer read FMsgType;
    property MsgText: string read GetMsgText;
  end;

  TSwiftMatchTag = record
    Left, Right: string;
    Explicit, Option: Boolean;
  end;
  TSwiftMatchTagArray = TArray<TSwiftMatchTag>;

  TSwiftFieldsRec = record
    FullName: string;
    Mandatory: Integer;
    Content, Pattern: string;
  end;

  TSwift = class
  private
    class function GetInstance(aMsgType: Integer = -1): TSwiftMessage;
  public
    class function MsgFormatValid(aMsgType: Integer; const aData: string;
      out aErrors: string; aFieldsProc: TSwiftFieldsProc = nil): Boolean;
    class function Load(aMsgType: Integer; const aData: string): TSwiftMessage;
  end;

  TSwiftMatchingFunc = function(aLeft, aRight: TSwiftMessage;
    aMatchTags: array of TSwiftMatchTag): Boolean of object;
  TSwiftMatchingProtocolFunc = function(aLeft, aRight: TSwiftMessage;
    aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean of object;

  TSwiftArchive = class
  private
    class function MatchBlockFour300(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;
    class function MatchBlockFour320(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;
    class function MatchBlockFour600(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;
    class function MatchBlockFour202(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;
    class function MatchBlockFour5n(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;
    class function MatchBlockFour5nNotStrict(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag): Boolean; overload;

    class function MatchBlockFour5Types(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag;
      var aProtocol: string; aLogging: Boolean = False; aStrict: Boolean = True): Boolean;

    class function MatchBlockFour300Protocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean; overload;
    class function MatchBlockFour320Protocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean; overload;
    class function MatchBlockFour600Protocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean; overload;
    class function MatchBlockFour202Protocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean; overload;
    class function MatchBlockFour5nProtocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean; overload;

    class function MatchBlockFour(aMsgType: Integer; const aLeft, aRight: string;
      aMatchTags: array of TSwiftMatchTag; aMatching: TSwiftMatchingFunc = nil): Boolean;
    class function MatchBlockFourProtocol(aMsgType: Integer; const aLeft, aRight: string;
      aMatchTags: array of TSwiftMatchTag; aMatching: TSwiftMatchingProtocolFunc;
      var aProtocol: string): Boolean;
  public
    // для SWIFT сообщений MT300 и MT320 (в дальнейшем список может быть расширен)
    class function MsgMatching(aMsgType: Integer; const aLeft, aRight: string;
      aStrict: Boolean = True): Boolean; overload;
    class function MsgMatchingProtocol(aMsgType: Integer; const aLeft, aRight: string;
      var aProtocol: string): Boolean;
  end;

  TXMLDocumentHelper = class
  public
    class function SelectNodes(aNode: IXMLNode; const aPath: string): IXMLNodeList; static;
    class function SelectSingleNode(aNode: IXMLNode; const aPath: string): IXMLNode; static;
    class procedure ForEach(aNodes: IXMLNodeList; aProc: TProc<IXMLNode>); static;
  end;

  function CheckSWIFTMsgFormat(const aMsgText: string; aMsgType: Integer = 0): string; stdcall;

  const
   cSwiftFieldsFour518: array [0..39] of TSwiftFieldsRec =
    (
    // последовательность A
    (FullName: 'GENL.16R';                  Mandatory: 1; Content: 'GENL';                  Pattern: '^GENL$'),
    (FullName: 'GENL.20C.SEME';             Mandatory: 1; Content: ':4!c//16x';             Pattern: '^(:SEME//)[\w\-:\(\)\.,\+\?\/ ]{1,16}$'),
    (FullName: 'GENL.23G';                  Mandatory: 1; Content: '4!c[/4!c]';             Pattern: '^(CANC|NEWM)(/(CODU|COPY|DUPL))?$'),
    (FullName: 'GENL.22F.TRTR';             Mandatory: 1; Content: ':4!c/[8c]/4!c';         Pattern: '^(:TRTR/)([A-Z0-9]{1,8})?(/(BASK|INDX|LIST|PROG|TRAD))$'),
    (FullName: 'GENL.16S';                  Mandatory: 1; Content: 'GENL';                  Pattern: '^GENL$'),
    // последовательность B
    (FullName: 'CONFDET.16R';               Mandatory: 1; Content: 'CONFDET';               Pattern: '^CONFDET$'),
    (FullName: 'CONFDET.98A.SETT';          Mandatory: 1; Content: ':4!c//8!n';             Pattern: '^(:SETT//)\d{8}$'),
    (FullName: 'CONFDET.98A.TRAD';          Mandatory: 1; Content: ':4!c//8!n';             Pattern: '^(:TRAD//)\d{8}$'),
    (FullName: 'CONFDET.90A.DEAL';          Mandatory: 1; Content: ':4!c//4!c/15d';         Pattern: '^(:DEAL//)([A-Z0-9]{4})(/[(\d+)?,(\d+)?]*)$'),
    (FullName: 'CONFDET.99A.DAAC';          Mandatory: 0; Content: ':4!c//[N]3!n';          Pattern: '^(:DAAC//)N?\d{3}$'),
    (FullName: 'CONFDET.22H.BUSE';          Mandatory: 1; Content: ':4!c//4!c';             Pattern: '^(:BUSE//)(BUYI|CROF|CROT|DIVR|IPOO|REDM|SELL|SUBS|SWIF|SWIT)$'),
    (FullName: 'CONFDET.22H.PAYM';          Mandatory: 1; Content: ':4!c//4!c';             Pattern: '^(:PAYM//)(APMT|FREE)$'),
    // последовательность B1
    (FullName: 'CONFDET.CONFPRTY.16R';      Mandatory: 1; Content: 'CONFPRTY';              Pattern: '^CONFPRTY$'),
    (FullName: 'CONFDET.CONFPRTY.95P';      Mandatory: 2; Content: ':4!c//4!a2!a2!c[3!c]';  Pattern: '^(:(BUYR|SELL)//)(([A-Z]{6})([A-Z0-9]{2})([A-Z0-9]{3})?)$'),
    (FullName: 'CONFDET.CONFPRTY.97A.CASH'; Mandatory: 0; Content: ':4!c//35x';             Pattern: '^(:CASH//)([\w\-:\(\)\.,\+\?\/ ]{1,35})$'),
    (FullName: 'CONFDET.CONFPRTY.22F.TRCA'; Mandatory: 0; Content: ':4!c/[8c]/4!c';         Pattern: '^(:TRCA/)([A-Z0-9]{1,8})?(/(AGEN|BAGN|CAGN|CPRN|INFI|MKTM|MLTF|OAGNPRAG|PRIN|RMKT|SINT|TAGT))$'),
    (FullName: 'CONFDET.CONFPRTY.16S';      Mandatory: 1; Content: 'CONFPRTY';              Pattern: '^CONFPRTY$'),
    (FullName: 'CONFDET.36B.CONF';          Mandatory: 1; Content: ':4!c//4!c/15d';         Pattern: '^(:CONF//)([A-Z0-9]{4})(/[(\d+)?,(\d+)?]*)$'),
    // последовательность B2
    (FullName: 'CONFDET.FIA.16R';           Mandatory: 0; Content: 'FIA';                   Pattern: '^FIA$'),
    (FullName: 'CONFDET.FIA.12A';           Mandatory: 0; Content: ':4!c/[8c]/30x';         Pattern: '^(:(CLAS|OPST|OPTI)/)([A-Z0-9]{1,8})?(/[\w\-:\(\)\.,\+\?\/ ]{1,30})$'),
    (FullName: 'CONFDET.FIA.11A';           Mandatory: 0; Content: ':4!c//3!a';             Pattern: '^(:DENO//)([A-Z]{3})$'),
    (FullName: 'CONFDET.FIA.98A.MATU';      Mandatory: 0; Content: ':4!c//8!n';             Pattern: '^(:MATU//)(\d{8})$'),
    (FullName: 'CONFDET.FIA.92A.INTR';      Mandatory: 0; Content: ':4!c//[N]15d';          Pattern: '^(:INTR//)N?([(\d+)?,(\d+)?]*)$'),
    (FullName: 'CONFDET.FIA.70E.FIAN';      Mandatory: 0; Content: ':4!c//10*35x';          Pattern: '\A(:FIAN//)(([\w\-:\(\)\.,\+\?\/ ]{1,35}(\r\n)?))(^([\w\-:\(\)\.,\+\?\/ ]{1,35}(\r\n)?)){0,9}\Z'),
    (FullName: 'CONFDET.FIA.16S';           Mandatory: 0; Content: 'FIA';                   Pattern: '^FIA$'),
    (FullName: 'CONFDET.70E.TRPO';          Mandatory: 0; Content: ':4!c//10*35x';          Pattern: '^(:TRPO//)([\w\-:\(\)\.,\+\?\/ ]{0,35})(\r\n)?(([\w\-:\(\)\.,\+\?\/ ]{0,35}(\r\n)?){0,9})$'),
    (FullName: 'CONFDET.16S';               Mandatory: 1; Content: 'CONFDET';               Pattern: '^CONFDET$'),
    // последовательность C
    (FullName: 'SETDET.16R';                Mandatory: 1; Content: 'SETDET';                Pattern: '^SETDET$'),
    (FullName: 'SETDET.22F.SETR';           Mandatory: 1; Content: ':4!c/[8c]/4!c';         Pattern: '^(:(SETR|RTGS)/)([A-Z0-9]{1,8})?(/[A-Z]{4})$'),
    // последовательность C1
    (FullName: 'SETDET.SETPRTY.16R';        Mandatory: 1; Content: 'SETPRTY';               Pattern: '^SETPRTY$'),
    (FullName: 'SETDET.SETPRTY.95P';        Mandatory: 2; Content: ':4!c//4!a2!a2!c[3!c]';  Pattern: '^(:(SELL|DEAG|BUYR|REAG)//)(([A-Z]{6})([A-Z0-9]{2})([A-Z0-9]{3})?)$'),
    (FullName: 'SETDET.SETPRTY.16S';        Mandatory: 1; Content: 'SETPRTY';               Pattern: '^SETPRTY$'),
    // последовательность C2
    (FullName: 'SETDET.CSHPRTY.16R';        Mandatory: 1; Content: 'CSHPRTY';               Pattern: '^CSHPRTY$'),
    (FullName: 'SETDET.CSHPRTY.95P.ACCW';   Mandatory: 1; Content: ':4!c//4!a2!a2!c[3!c]';  Pattern: '^(:ACCW//)(([A-Z]{6})([A-Z0-9]{2})([A-Z0-9]{3})?)$'),
    (FullName: 'SETDET.CSHPRTY.97A.CASH';   Mandatory: 0; Content: ':4!c//35x';             Pattern: '^(:CASH//)([\w\-:\(\)\.,\+\?\/ ]{1,35})$'),
    (FullName: 'SETDET.CSHPRTY.16S';        Mandatory: 1; Content: 'CSHPRTY';               Pattern: '^CSHPRTY$'),
    // последовательность C3
    (FullName: 'SETDET.AMT.16R';            Mandatory: 1; Content: 'AMT';                   Pattern: '^AMT$'),
    (FullName: 'SETDET.AMT.19A';            Mandatory: 2; Content: ':4!c//[N]3!a15d';       Pattern: '^(:(SETT|ACRU)//)(N?)([A-Z]{3})([(\d+)?,(\d+)?]*)$'),
    (FullName: 'SETDET.AMT.16S';            Mandatory: 1; Content: 'AMT';                   Pattern: '^AMT$'),
    (FullName: 'SETDET.16S';                Mandatory: 1; Content: 'SETDET';                Pattern: '^SETDET$')
    );

  cSwiftFieldsFour541: array [0..25] of TSwiftFieldsRec =
    (
    // последовательность A
    (FullName: 'GENL.16R';                  Mandatory: 1; Content: 'GENL';                  Pattern: '^GENL$'),
    (FullName: 'GENL.20C.SEME';             Mandatory: 1; Content: ':4!c//16x';             Pattern: '^(:SEME//)[\w\-:\(\)\.,\+\?\/ ]{1,16}$'),
    (FullName: 'GENL.23G';                  Mandatory: 1; Content: '4!c[/4!c]';             Pattern: '^(CANC|NEWM)(/(CODU|COPY|DUPL))?$'),
    (FullName: 'GENL.16S';                  Mandatory: 1; Content: 'GENL';                  Pattern: '^GENL$'),
    // последовательность B
    (FullName: 'TRADDET.16R';               Mandatory: 1; Content: 'TRADDET';               Pattern: '^TRADDET$'),
    (FullName: 'TRADDET.98A.SETT';          Mandatory: 1; Content: ':4!c//8!n';             Pattern: '^(:SETT//)\d{8}$'),
    (FullName: 'TRADDET.98A.TRAD';          Mandatory: 1; Content: ':4!c//8!n';             Pattern: '^(:TRAD//)\d{8}$'),
    (FullName: 'TRADDET.90A.DEAL';          Mandatory: 0; Content: ':4!c//4!c/15d';         Pattern: '^(:DEAL//)(DISC|PRCT|PREM|YIEL)(/[(\d+)?,(\d+)?]*)$'),
    (FullName: 'TRADDET.90B.DEAL';          Mandatory: 0; Content: ':4!c//4!c/3!a15d';      Pattern: '^(:DEAL//)(ACTU|DISC|PREM)(/[A-Z]{3})([(\d+)?,(\d+)?]*)$'),
    (FullName: 'TRADDET.16S';               Mandatory: 1; Content: 'TRADDET';               Pattern: '^TRADDET$'),
    // последовательность C
    (FullName: 'FIAC.16R';                  Mandatory: 0; Content: 'FIAC';                  Pattern: '^FIAC$'),
    (FullName: 'FIAC.36B.SETT';             Mandatory: 1; Content: ':4!c//4!c/15d';         Pattern: '^(:SETT//)(AMOR|FAMT|UNIT)(/[(\d+)?,(\d+)?]*)$'),     // ^(?!(.{16,}))(\d)+(,){1}\d*$ - формат 15d
    (FullName: 'FIAC.97A.SAFE';             Mandatory: 1; Content: ':4!c//35x';             Pattern: '^(:SAFE//)([\w\-:\(\)\.,\+\?\/ ]{1,35})$'),
    (FullName: 'FIAC.97A';                  Mandatory: 2; Content: ':4!c//35x';             Pattern: '^(:(SAFE|CASH)//)([\w\-:\(\)\.,\+\?\/ ]{1,35})$'),
    (FullName: 'FIAC.16S';                  Mandatory: 0; Content: 'FIAC';                  Pattern: '^FIAC$'),
    // последовательность E
    (FullName: 'SETDET.16R';                Mandatory: 1; Content: 'SETDET';                Pattern: '^SETDET$'),
    (FullName: 'SETDET.22F.SETR';           Mandatory: 1; Content: ':4!c/[8c]/4!c';         Pattern: '^(:(SETR)/)([A-Z0-9]{1,8})?(/[A-Z]{4})$'),
    (FullName: 'SETDET.22F.RTGS';           Mandatory: 0; Content: ':4!c/[8c]/4!c';         Pattern: '^(:(RTGS)/)([A-Z0-9]{1,8})?(/[A-Z]{4})$'),
    // последовательность E1
    (FullName: 'SETDET.SETPRTY.16R';        Mandatory: 1; Content: 'SETPRTY';               Pattern: '^SETPRTY$'),
    (FullName: 'SETDET.SETPRTY.95P';        Mandatory: 2; Content: ':4!c//4!a2!a2!c[3!c]';  Pattern: '^(:(SELL|DEAG|BUYR|REAG|PSET)//)(([A-Z]{6})([A-Z0-9]{2})([A-Z0-9]{3})?)$'),
    (FullName: 'SETDET.SETPRTY.97A';        Mandatory: 0; Content: ':4!c//35x';             Pattern: '^(:(SAFE|CASH)//)([\w\-:\(\)\.,\+\?\/ ]{1,35})$'),
    (FullName: 'SETDET.SETPRTY.16S';        Mandatory: 1; Content: 'SETPRTY';               Pattern: '^SETPRTY$'),
    // последовательность E3
    (FullName: 'SETDET.AMT.16R';            Mandatory: 1; Content: 'AMT';                   Pattern: '^AMT$'),
    (FullName: 'SETDET.AMT.19A.SETT';       Mandatory: 1; Content: ':4!c//[N]3!a15d';       Pattern: '^(:SETT//)(N?)([A-Z]{3})([(\d+)?,(\d+)?]*)$'),
    (FullName: 'SETDET.AMT.16S';            Mandatory: 1; Content: 'AMT';                   Pattern: '^AMT$'),
    (FullName: 'SETDET.16S';                Mandatory: 1; Content: 'SETDET';                Pattern: '^SETDET$')
    );

////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////

uses
  StrUtils, Types, Math, RegularExpressions, DateUtils,
  SWIFT_UUtils,
  xmldom;

const
  cInvalidSymbols = 'Недопустимые символы (%s)';
  cUnbalancedBrackets = 'Несбалансированное количество скобок';
  cUnknownBlock = 'Неизвестный блок {%s}';
  cEmptyTagName = 'Поле с пустым именем недопустимо';
  cEmptyTagValue = ':%s: Поле не может быть пустым';
  cNotEmptyTagValue = ':%s: Поле должно быть пустым';
  cInvalidTagName = 'Недопустимое имя поля (%s)';
  cInvalidTagSymbols = ':%s: Поле содержит недопустимые символы (%s)';
  cInvalidTagLenght = ':%s: Длина строки поля превышает допустимую';
  cInvalidTagLinesCount = ':%s: Количество строк поля превышает допустимое';
  cInvalidTagEmpty = ':%s: Строка поля является пустой';
  cInvalidTagFormat = ':%s: Значение поля "%s" не соответствует формату "%s"';
  cEmptyBlock = 'Пустой блок {%d...}';
  cInvalidBlockLength = 'Не правильная длина данных блока {%d...}';
  сInvalidBlockType = 'Невозможно отпределить тип блока {%d...}';
  сNoFindMandatoryTag = 'Отсутствует обязательное поле %s';
  сNoFindOptionMandatoryTag = 'Отсутствует обязательное опциональное поле %s';
  cNoSlashesTagValue = ':%s: Поле не может начинаться, заканчиваться слэшем или содержать внутри двойной слэш';
  cBlockNotFound = 'Не найден блок %d';
  cEmptyMessage = 'Пустое сообщение';
  cCommaCheckError  = 'Отсутствует ","';
  cLengthCheckError = 'Длина больше 15 символов';
  cRegExpError = 'Ошибка "%s" в регулярном выражении "%s"';

const
  // теги исключения, должны содержать пустое значение
  cEmptyTags: array [0..8] of string = (
    '15A', '15B', '15C', '15D', '15E', '15F', '15G', '15H', '15I');

  cSequenceTags: array [0..9] of string = (
    '15A', '15B', '15C', '15D', '15E', '15F', '15G', '15H', '15I', '16R');

  // теги для сравнения 4 блока в 300-ых сообщениях
  cMatchTagBlockFour300: array [0..13] of TSwiftMatchTag =
    ((Left: '15A.22C'; Right: '15A.22C'; Explicit: True),
    (Left: '15A.82A'; Right: '15A.87A'; Explicit: True),
    (Left: '15A.87A'; Right: '15A.82A'; Explicit: True),
    (Left: '15B.30T'; Right: '15B.30T'; Explicit: True),
    (Left: '15B.30V'; Right: '15B.30V'; Explicit: True),
    (Left: '15B.36'; Right: '15B.36'; Explicit: True),
    (Left: '15B.32B'; Right: '15B.33B'; Explicit: True),
    (Left: '15B.32B.53'; Right: '15B.33B.53'; Explicit: True; Option: True),
    (Left: '15B.32B.56'; Right: '15B.33B.56'; Explicit: True; Option: True),
    (Left: '15B.32B.57'; Right: '15B.33B.57'; Explicit: True; Option: True),
    (Left: '15B.33B'; Right: '15B.32B'; Explicit: True),
    (Left: '15B.33B.53'; Right: '15B.32B.53'; Explicit: True; Option: True),
    (Left: '15B.33B.56'; Right: '15B.32B.56'; Explicit: True; Option: True),
    (Left: '15B.33B.57'; Right: '15B.32B.57'; Explicit: True; Option: True));

  // теги для сравнения 4 блока в 320-ых сообщениях
  cMatchTagBlockFour320: array [0..28] of TSwiftMatchTag =
    ((Left: '15A.22B'; Right: '15A.22B'; Explicit: True),
    (Left: '15A.82A'; Right: '15A.87A'; Explicit: True),
    (Left: '15A.87A'; Right: '15A.82A'; Explicit: True),
    (Left: '15B.17R'; Right: '15B.17R'; Explicit: True),
    (Left: '15B.30T'; Right: '15B.30T'; Explicit: True),
    (Left: '15B.30V'; Right: '15B.30V'; Explicit: True),
    (Left: '15B.30P'; Right: '15B.30P'; Explicit: True),
    (Left: '15B.32B'; Right: '15B.32B'; Explicit: True),
    (Left: '15B.32H'; Right: '15B.32H'; Explicit: True){*},
    (Left: '15B.30X'; Right: '15B.30X'; Explicit: True),
    (Left: '15B.34E'; Right: '15B.34E'; Explicit: True),
    (Left: '15B.37G'; Right: '15B.37G'; Explicit: True),
    (Left: '15B.14D'; Right: '15B.14D'; Explicit: True),
    (Left: '15C.53'; Right: '15D.53'; Explicit: True; Option: True),
    (Left: '15C.56'; Right: '15D.56'; Explicit: True; Option: True),
    (Left: '15C.57'; Right: '15D.57'; Explicit: True; Option: True),
    (Left: '15C.58'; Right: '15D.58'; Explicit: True; Option: True),
    (Left: '15D.53'; Right: '15C.53'; Explicit: True; Option: True),
    (Left: '15D.56'; Right: '15C.56'; Explicit: True; Option: True),
    (Left: '15D.57'; Right: '15C.57'; Explicit: True; Option: True),
    (Left: '15D.58'; Right: '15C.58'; Explicit: True; Option: True),
    (Left: '15E.53'; Right: '15F.53'; Explicit: True; Option: True),
    (Left: '15E.56'; Right: '15F.56'; Explicit: True; Option: True),
    (Left: '15E.57'; Right: '15F.57'; Explicit: True; Option: True),
    (Left: '15E.58'; Right: '15F.58'; Explicit: True; Option: True),
    (Left: '15F.53'; Right: '15E.53'; Explicit: True; Option: True),
    (Left: '15F.56'; Right: '15E.56'; Explicit: True; Option: True),
    (Left: '15F.57'; Right: '15E.57'; Explicit: True; Option: True),
    (Left: '15F.58'; Right: '15E.58'; Explicit: True; Option: True));

  // теги для сравнения 4 блока в 600-ых сообщениях
  cMatchTagBlockFour600: array [0..13] of TSwiftMatchTag =
    ((Left: '15A.22'; Right: '15A.22'; Explicit: True),
    (Left: '15A.82'; Right: '15A.87'; Explicit: True; Option: True),
    (Left: '15A.87'; Right: '15A.82'; Explicit: True; Option: True),
    (Left: '15A.30'; Right: '15A.30'; Explicit: True),
    (Left: '15A.26C'; Right: '15A.26C'; Explicit: True),
    (Left: '15A.33G'; Right: '15A.33G'; Explicit: True),
    (Left: '15B.32B'; Right: '15C.32F'; Explicit: True),
    (Left: '15B.87'; Right: '15C.87'; Explicit: True; Option: True),
    (Left: '15B.34R'; Right: '15C.34R'; Explicit: True),
    (Left: '15B.57'; Right: '15C.57'; Explicit: True; Option: True),
    (Left: '15C.32F'; Right: '15B.32B'; Explicit: True),
    (Left: '15C.87'; Right: '15B.87'; Explicit: True; Option: True),
    (Left: '15C.34R'; Right: '15B.34R'; Explicit: True),
    (Left: '15C.57'; Right: '15B.57'; Explicit: True; Option: True));

  // теги для сравнения 4 блока в 202-ых сообщениях
  cMatchTagBlockFour202: array [0..3] of TSwiftMatchTag =
    ((Left: '32A'; Right: '32A'),
    (Left: '56'; Right: '56'; Option: True),
    (Left: '57'; Right: '57'; Option: True),
    (Left: '58'; Right: '58'; Option: True));

  // теги для сравнения 4 блока в 518-ых сообщениях
  // формат строки - регулярное выражение вида
  // ^TAG\.SEQ\.SUBSEQ\.QUAL$
  // TAG - наименование поля (тэга)
  // SEQ - наименование последовательности (тэг 16R)
  // SUBSEQ - наименование подпоследовательности внутри незакрытой (тэг 16S) последовательности
  // QUAL - наименование квалификатора (значение поля вида :4!c)
  cMatchTagBlockFour518: array [0..19] of TSwiftMatchTag =
    (
     (Left: '^(23G.GENL)$';                    Right: '^(23G.GENL)$';                    Explicit: True),
     (Left: '^(22F.GENL(.TRTR)?)$';            Right: '^(22F.GENL(.TRTR)?)$';            Explicit: True),
     (Left: '^(98A.CONFDET.TRAD)$';            Right: '^(98A.CONFDET.TRAD)$';            Explicit: True),
     (Left: '^(98A.CONFDET.SETT)$';            Right: '^(98A.CONFDET.SETT)$';            Explicit: True),
     (Left: '^(90A.CONFDET.DEAL)$';            Right: '^(90A.CONFDET.DEAL)$';            Explicit: True),
     (Left: '^(99A.CONFDET.(DAAC|GIUP))$';     Right: '^(99A.CONFDET.(DAAC|GIUP))$';     Explicit: True),
     (Left: '^(22H.CONFDET.PAYM)$';            Right: '^(22H.CONFDET.PAYM)$';            Explicit: True),
     (Left: '^(95P.CONFDET.CONFPRTY.BUYR)$';   Right: '^(95P.CONFDET.CONFPRTY.BUYR)$';   Explicit: True),
     (Left: '^(95P.CONFDET.CONFPRTY.SELL)$';   Right: '^(95P.CONFDET.CONFPRTY.SELL)$';   Explicit: True),
     (Left: '^(36B.CONFDET.CONF)$';            Right: '^(36B.CONFDET.CONF)$';            Explicit: True),
     (Left: '^(35B.CONFDET)$';                 Right: '^(35B.CONFDET)$';                 Explicit: True),
     (Left: '^(11A.CONFDET.FIA.DENO)$';        Right: '^(11A.CONFDET.FIA.DENO)$';        Explicit: True),
     (Left: '^(98A.CONFDET.FIA.([A-Z]){4})$';  Right: '^(98A.CONFDET.FIA.([A-Z]){4})$';  Explicit: True),
     (Left: '^(92A.CONFDET.FIA.([A-Z]){4})$';  Right: '^(92A.CONFDET.FIA.([A-Z]){4})$';  Explicit: True),
     (Left: '^(22F.SETDET.SETR)$';             Right: '^(22F.SETDET.SETR)$';             Explicit: True),
     (Left: '^(95P.SETDET.SETPRTY.SELL)$';     Right: '^(95P.SETDET.SETPRTY.SELL)$';     Explicit: True),
     (Left: '^(95P.SETDET.SETPRTY.BUYR)$';     Right: '^(95P.SETDET.SETPRTY.BUYR)$';     Explicit: True),
     (Left: '^(95P.SETDET.CSHPRTY.ACCW)$';     Right: '^(95P.SETDET.CSHPRTY.ACCW)$';     Explicit: True),
     (Left: '^(19A.SETDET.AMT.SETT)$';         Right: '^(19A.SETDET.AMT.SETT)$';         Explicit: True),
     (Left: '^(19A.SETDET.AMT.ACRU)$';         Right: '^(19A.SETDET.AMT.ACRU)$';         Explicit: True)
    );

  cMatchTagBlockFour541: array [0..7] of TSwiftMatchTag =
    (
     (Left: '^(23G.GENL)$';                    Right: '^(23G.GENL)$';                    Explicit: True),
     (Left: '^(98A.TRADDET.(SETT|ESET))$';     Right: '^(98A.TRADDET.(SETT|ESET))$';     Explicit: True; Option: True),
     (Left: '^(35B.TRADDET)$';                 Right: '^(35B.TRADDET)$';                 Explicit: True),
     (Left: '^(36B.FIAC.(SETT|ESTT))$';        Right: '^(36B.FIAC.(SETT|ESTT))$';        Explicit: True; Option: True),
     (Left: '^(97A.FIAC.SAFE)$';               Right: '^(97A.FIAC.SAFE)$';               Explicit: True),
     (Left: '^(97A.FIAC.CASH)$';               Right: '^(97A.FIAC.CASH)$';               Explicit: True),
     (Left: '^(22F.SETDET.SETR)$';             Right: '^(22F.SETDET.SETR)$';             Explicit: True),
     (Left: '^(19A.SETDET.AMT.(SETT|ESTT))$';  Right: '^(19A.SETDET.AMT.(SETT|ESTT))$';  Explicit: True; Option: True)
    );

  // теги для сравнения 4 блока в 300-ых сообщениях (не строгий)
  cMatchTagBlockFour300NotStrict: array [0..3] of TSwiftMatchTag =
    ((Left: '15A.22C'; Right: '15A.22C'; Explicit: True),
    (Left: '15B.30V'; Right: '15B.30V'; Explicit: True),
    (Left: '15B.32B'; Right: '15B.33B'; Explicit: True),
    (Left: '15B.33B'; Right: '15B.32B'; Explicit: True));

  // теги для сравнения 4 блока в 320-ых сообщениях (не строгий)
  cMatchTagBlockFour320NotStrict: array [0..3] of TSwiftMatchTag =
    ((Left: '15A.22C'; Right: '15A.22C'; Explicit: True),
    (Left: '15B.30V'; Right: '15B.30V'; Explicit: True),
    (Left: '15B.30P'; Right: '15B.30P'; Explicit: True),
    (Left: '15B.32B'; Right: '15B.32B'; Explicit: True));

  // теги для сравнения 4 блока в 600-ых сообщениях (не строгий)
  cMatchTagBlockFour600NotStrict: array [0..10] of TSwiftMatchTag =
    ((Left: '15A.22'; Right: '15A.22'; Explicit: True),
    (Left: '15A.82'; Right: '15A.87'; Explicit: True; Option: True),
    (Left: '15A.87'; Right: '15A.82'; Explicit: True; Option: True),
    (Left: '15A.30'; Right: '15A.30'; Explicit: True),
    (Left: '15A.33G'; Right: '15A.33G'; Explicit: True),
    (Left: '15B.32B'; Right: '15C.32F'; Explicit: True),
    (Left: '15B.34R'; Right: '15C.34R'; Explicit: True),
    (Left: '15B.57'; Right: '15C.57'; Explicit: True; Option: True),
    (Left: '15C.32F'; Right: '15B.32B'; Explicit: True),
    (Left: '15C.34R'; Right: '15B.34R'; Explicit: True),
    (Left: '15C.57'; Right: '15B.57'; Explicit: True; Option: True));

  // теги для сравнения 4 блока в 202-ых сообщениях (не строгий)
  cMatchTagBlockFour202NotStrict: array [0..0] of TSwiftMatchTag =
    ((Left: '32A'; Right: '32A'));

  // теги для сравнения 4 блока в 518-ых сообщениях  
  cMatchTagBlockFour518NotStrict: array [0..8] of TSwiftMatchTag =
    (
    (Left: '^(23G.GENL)$';                   Right: '^(23G.GENL)$';                   Explicit: True),
    (Left: '^(98A.CONFDET.SETT)$';           Right: '^(98A.CONFDET.SETT)$';           Explicit: True),
    (Left: '^(90A.CONFDET.DEAL)$';           Right: '^(90A.CONFDET.DEAL)$';           Explicit: True),
    (Left: '^(36B.CONFDET.CONF)$';           Right: '^(36B.CONFDET.CONF)$';           Explicit: True),
    (Left: '^(11A.CONFDET.FIA.DENO)$';       Right: '^(11A.CONFDET.FIA.DENO)$';       Explicit: True),
    (Left: '^(98A.CONFDET.FIA.([A-Z]){4})$'; Right: '^(98A.CONFDET.FIA.([A-Z]){4})$'; Explicit: True),
    (Left: '^(92A.CONFDET.FIA.([A-Z]){4})$'; Right: '^(92A.CONFDET.FIA.([A-Z]){4})$'; Explicit: True),
    (Left: '^(19A.SETDET.AMT.SETT)$';        Right: '^(19A.SETDET.AMT.SETT)$';        Explicit: True),
    (Left: '^(19A.SETDET.AMT.ACRU)$';        Right: '^(19A.SETDET.AMT.ACRU)$';        Explicit: True)
    );

  cMatchTagBlockFour541NotStrict: array [0..3] of TSwiftMatchTag =
    (
    (Left: '^(23G.GENL)$';                   Right: '^(23G.GENL)$';                   Explicit: True),
    (Left: '^(98A.TRADDET.(SETT|ESET))$';    Right: '^(98A.TRADDET.(SETT|ESET))$';    Explicit: True; Option: True),
    (Left: '^(36B.FIAC.(SETT|ESTT))$';       Right: '^(36B.FIAC.(SETT|ESTT))$';       Explicit: True; Option: True),
    (Left: '^(19A.SETDET.AMT.(SETT|ESTT))$'; Right: '^(19A.SETDET.AMT.(SETT|ESTT))$'; Explicit: True; Option: True)
    );

type
  TSwiftValidatorRegistry = class(TDictionary<string, TSwiftValidatorClassArray>)
  public
    function Add(const aKey: string;
      aValue: TSwiftValidatorClassArray): TSwiftValidatorRegistry;
  end;

var
  gFieldValidator: TSwiftValidatorRegistry = nil;
  gFieldDefaultValidator: TSwiftValidatorClassArray = nil;
  gMessageValidator: TSwiftValidatorRegistry = nil;
  gMessageDefaultValidator: TSwiftValidatorClassArray = nil;

type
  TSwiftMsgFormat = record
  public
    Content: string;
    Description: string;
    Mandatory: Boolean;
    Notes: string;
    Pattern: string;
    constructor Create(const aContent, aPattern, aNotes, aDescription: string;
      aMandatory: Boolean);
    class function Empty(): TSwiftMsgFormat; static;
    function IsEmpty(): Boolean;
  end;

  TSwiftMandatoryField = record
  strict private
    function GetNameWithoutColon(): string;
  public
    Name: string;
    Mandatory: Integer;
    constructor Create(aName: string; aMandatory: Integer);
    property NameWithoutColon: string read GetNameWithoutColon;
  end;

  TSwiftMandatoryFieldArray = array of TSwiftMandatoryField;

  //???
  TSwiftMsgArchive = record
    ID: Integer;
    Direction: Integer;//0..1;
    Reference {22 поле}, Sender, Reciever: string;
    Text: string;
    constructor Create(aID: Integer; aDirection: Integer;
      aReference, aSender, aReciever: string; const aText: string);
  end;

  TSwiftMsgArchiveArray = TArray<TSwiftMsgArchive>;

  TSwiftDatabase = class
  public
    class function GetMsgFormat(aMsgType: Integer; const aFieldName: string): TSwiftMsgFormat; overload;
    class function GetMsgFormat(aMsgType: Integer; const aField: TSwiftField): TSwiftMsgFormat; overload;
    class function GetMandatoryFields(aMsgType: Integer): TSwiftMandatoryFieldArray;
  public
    // возможно, нужно исключить из расмотрения записи, которые уже зависимы
    //???
//    class function GetMsgArchive(aID: Integer): TSwiftMsgArchive;
//    class function GetMsgArchiveMatching(aMsgType: Integer;
//      aMsgArchive: TSwiftMsgArchive): TSwiftMsgArchiveArray;
  end;

  TSwiftParser = class
  private
    FCurrentLine: Integer;
    FErrors: TSwiftErrorList;
    FMessage: string;
    FMessageObj: TSwiftMessage;
    function CreateTag(const aText: string; aLine: Integer): TSwiftTag;
    function SubString(const aData: string; aBegin, aEnd: Integer): string;
    procedure Correct(const aData: string; var aLine: Integer; aIndex: Integer; aOffset: Integer = 1);

    procedure InvalidSymbols(aLine: Integer; const aSymbols: string);
    procedure UnknownBlock(aLine: Integer; const aData: string);
    procedure UnbalancedBrackets(aLine: Integer);

    function FindBlockStart(const aText: string; aOffset: Integer): Integer;
    function ReadUntilBlockEnd(const aText: string; aOffset: Integer): string;
    function IsBlockStart(aChar: Char): Boolean;
    function IsTextBlock(const aData: string): Boolean;
    function IsBlockEnd(aIsTextBlock: Boolean; const aData: string; aIndex: Integer): Boolean; overload;
    function IsBlockEnd(aChar: Char): Boolean; overload;
    function IdentifyBlock(const aText: string): Integer;
    // на входе текст блока, без фигурных скобок
    function TagListBlockConsume(aBlock: TSwiftTagListBlock; const aTextBlock: string; aLine: Integer): TSwiftBlock;
    function Block4Consume(aBlock: TSwiftTagListBlock; const aTextBlock: string; aLine: Integer): TSwiftBlock;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Build(aMessage: TSwiftMessage; const aData: string);
    property Errors: TSwiftErrorList read FErrors;
  end;

{$Region 'TSwiftParser'}

function TSwiftParser.Block4Consume(aBlock: TSwiftTagListBlock;
  const aTextBlock: string; aLine: Integer): TSwiftBlock;
const
  sTextBlockBegin = sLineBreak + ':';
  sTextBlockEnd = sLineBreak + '-';
  sFieldDelimiter = sLineBreak + ':';
var
  Data, eData: string;
  Start, Finish, Index, Line: Integer;
begin
  Line := aLine;

// J7852
// Для старых сообщений необходимо заменить символ '~', который
// использовался для перевода строки
  eData := aTextBlock;
  eData := ReplaceStr(eData, '~', #$D#$A);
  Data := AdjustLineBreaks(eData {aTextBlock});

  aBlock.Text := Data;

  // удаление меток в начале и конце блока данных
  Index := Pos(sTextBlockBegin, Data);
  Correct(Data, aLine, Index + Length(sTextBlockBegin) - 1);
  if Index <> 1 then
    InvalidSymbols(Line, SubString(Data, 1, Index));
  Delete(Data, 1, Index + Length(sTextBlockBegin) - 1);

  Finish := Pos(sTextBlockEnd, Data);

  Start := 1;
  Index := PosEx(sFieldDelimiter, Data, Start);

  while (Start <> 0) and (Index <> 0) do
  begin
    aBlock.AddTag(CreateTag(SubString(Data, Start, Index), aLine));

    Correct(Data, aLine, Index + Length(sFieldDelimiter) - 1, Start);
    Start := Index + Length(sFieldDelimiter);
    Index := PosEx(sFieldDelimiter, Data, Start);
  end;

  if (Start <> 0) and (Index = 0) then
    aBlock.AddTag(
      CreateTag(SubString(Data, Start, IfThen(Finish = 0, Length(Data), Finish)), aLine));

  if (Finish <> 0) and ((Finish + Length(sTextBlockEnd)) < (Length(Data) + 1)) then
  begin
    Correct(Data, aLine, Finish + Length(sTextBlockEnd) - 1, Start);
    InvalidSymbols(aLine, SubString(Data, Finish + Length(sTextBlockEnd), Length(Data) + 1));
  end;

  Result := aBlock;
end;

procedure TSwiftParser.Correct(const aData: string; var aLine: Integer; aIndex, aOffset: Integer);
var
  Point: Integer;
begin
  Point := aOffset;
  while (Point <= aIndex) do
  begin
    if (aData[Point] = #10) and (((Point - 1) > 0) and (aData[Point - 1] = #13)) then
      Inc(aLine);
    Inc(Point);
  end;
end;

constructor TSwiftParser.Create();
begin
  FCurrentLine := 1;
  FErrors := TSwiftErrorList.Create();
  FMessage := '';
end;

function TSwiftParser.CreateTag(const aText: string; aLine: Integer): TSwiftTag;
begin
  Result := TSwiftTag.Create(aText);
  Result.Line := aLine;

  // 32B и 33B являются подсекциями 15B в 300 swift-сообщении
  if Assigned(FMessageObj) and (FMessageObj.MsgType = 300) and MatchText(Result.Name, ['32B', '33B']) then
    Result.Sequence := True
  else if MatchText(Result.Name, cSequenceTags) then
    Result.Sequence := True;
end;

destructor TSwiftParser.Destroy;
begin
  FErrors.Free();
  inherited;
end;

function TSwiftParser.FindBlockStart(const aText: string; aOffset: Integer): Integer;
begin
  Result := aOffset;
  while (Result <= Length(aText)) and not IsBlockStart(aText[Result]) do
  begin
    Inc(Result);
    Correct(aText, FCurrentLine, Result, Result); // определение текущей строки
  end;

  Result := IfThen(Result <= Length(aText), Result, -1);
end;

function TSwiftParser.IdentifyBlock(const aText: string): Integer;
begin
  Result := StrToIntDef(Copy(aText, 1, Pos(':', aText) - 1), -1);
end;

procedure TSwiftParser.InvalidSymbols(aLine: Integer; const aSymbols: string);
begin
  FErrors.Add(
    TSwiftError.Create(
      Format(cInvalidSymbols, [ReplaceText(aSymbols, #13#10, ' ')]), 0, aLine, 1, FErrors.Count));
end;

function TSwiftParser.IsBlockEnd(aChar: Char): Boolean;
begin
  Result := aChar = '}';
end;

function TSwiftParser.IsBlockEnd(aIsTextBlock: Boolean;
  const aData: string; aIndex: Integer): Boolean;
const
  sTextBlockEnd = sLineBreak + '-';
begin
  if IsBlockEnd(aData[aIndex]) then
  begin
    if aIsTextBlock then
    begin
      if Copy(aData, aIndex - Length(sTextBlockEnd), Length(sTextBlockEnd)) = sTextBlockEnd then
        Result := True
      else Result := False;
    end else
      Result := True;
  end else
    Result := False;
end;

function TSwiftParser.IsBlockStart(aChar: Char): Boolean;
begin
  Result := aChar = '{';
end;

function TSwiftParser.IsTextBlock(const aData: string): Boolean;
var
  Offset: Integer;
begin
  if Length(aData) < 3 then
    Exit(False);

  Offset := 1;
  if Copy(aData, Offset, 2) = '4:' then
  begin
    Inc(Offset, 2);
    while (Offset < Length(aData)) do
    begin
      if aData[Offset] = '{' then
        Exit(False)
      else if aData[Offset] = ':' then
        Exit(True);

      Inc(Offset);
    end;
    Result := True;
  end else
    Result := False;
end;

procedure TSwiftParser.Build(aMessage: TSwiftMessage; const aData: string);
var
  Offset: Integer;
  Buffer, Before, After: string;
  Block: TSwiftBlock;
  BlockBegin, BlockEnd: Integer;
  CurrentLine: Integer;
  Ident, Index: Integer;
begin
  FCurrentLine := 1;
  FErrors.Clear();

  if (aData = '') then
    fErrors.Add(TSwiftError.Create(cEmptyMessage, 0, 1, 1, fErrors.Count));

  FMessage := aData;
  FMessageObj := aMessage;
  BlockBegin := 1;

  Offset := FindBlockStart(FMessage, 1);
  while Offset <> -1 do
  begin
    CurrentLine := FCurrentLine;
    if (BlockBegin = 1) and (Offset > 1) then
      if not SameText(SubString(FMessage, BlockBegin, Offset), #01) then
      begin
        Index := Pos(#01, SubString(FMessage, BlockBegin, Offset));
        if Index = 0 then
        begin
          if Length(SubString(FMessage, BlockBegin, Offset)) > 0 then
            InvalidSymbols(CurrentLine, SubString(FMessage, BlockBegin, Offset))
        end
        else if Index = 1 then
          InvalidSymbols(CurrentLine, SubString(FMessage, BlockBegin + Length(#01), Offset))
        else
        begin
          Before := SubString(FMessage, BlockBegin, BlockBegin + Index - 1);
          After := SubString(FMessage, BlockBegin + Index - 1 + Length(#01), Offset);
          if Before <> '' then InvalidSymbols(CurrentLine, Before);
          if After <> '' then InvalidSymbols(CurrentLine, After);
        end;
      end;

    BlockBegin := Offset;
    Buffer := ReadUntilBlockEnd(FMessage, Offset);
    BlockEnd := BlockBegin + Length(Buffer) + 1;

    Ident := IdentifyBlock(Buffer);
    if Ident in [1..5] then
    begin
      Index := Pos(':', Buffer);
      if Index > 1 then
      begin
        Correct(Buffer, FCurrentLine, Index);
        Delete(Buffer, 1, Index);
      end;
    end;

    // в Buffer(-е) содержится текст соответствующего блока без его идентификации
    case Ident of
      1: Block := TSwiftBlock1.Create(aMessage, Buffer);
      2: Block := TSwiftBlock2.Create(aMessage, Buffer);
      3: Block := TagListBlockConsume(TSwiftBlock3.Create(aMessage), Buffer, CurrentLine);
      4: Block := Block4Consume(TSwiftBlock4.Create(aMessage), Buffer, CurrentLine);
      5: Block := TagListBlockConsume(TSwiftBlock5.Create(aMessage), Buffer, CurrentLine);
      else
        Block := nil;
    end;

    if Assigned(Block) then
    begin
      Block.Line := CurrentLine;
      aMessage.AddBlock(Block);
    end else
      UnknownBlock(CurrentLine, Buffer);

    CurrentLine := FCurrentLine;

    Offset := FindBlockStart(FMessage, Offset + Length(Buffer) + 1);
    if (BlockEnd + 1) < Offset then
      InvalidSymbols(CurrentLine, SubString(FMessage, BlockEnd + 1, Offset)) //между блоками
    else if Offset = -1 then
      if not SameText(SubString(FMessage, BlockEnd + 1, Length(FMessage) + 1), #03) then
      begin
        Index := Pos(#03, SubString(FMessage, BlockEnd + 1, Length(FMessage) + 1));
        if Index = 0 then
        begin
          if Length(SubString(FMessage, BlockEnd + 1, Length(FMessage) + 1)) > 0 then
            InvalidSymbols(CurrentLine, SubString(FMessage, BlockEnd + 1, Length(FMessage) + 1))
        end
        else if Index = 1 then
          InvalidSymbols(CurrentLine, SubString(FMessage, BlockEnd + 1 + Length(#03), Length(FMessage) + 1))
        else
        begin
          Before := SubString(FMessage, BlockEnd + 1, BlockEnd + 1 + Index - 1);
          After := SubString(FMessage, BlockEnd + 1 + Index - 1 + Length(#03), Length(FMessage) + 1);
          if Before <> '' then InvalidSymbols(CurrentLine, Before);
          if After <> '' then InvalidSymbols(CurrentLine, After);
        end;
      end;
    BlockBegin := Offset;
  end;

  if (aMessage.Block1 = nil) then
    FErrors.Add(
      TSwiftError.Create(Format(cBlockNotFound, [1]), 0, 1, 1, FErrors.Count));
  if (aMessage.Block2 = nil) then
    FErrors.Add(
      TSwiftError.Create(Format(cBlockNotFound, [2]), 0, 1, 1, FErrors.Count));
  if (aMessage.FBlock4 = nil) then
    FErrors.Add(
      TSwiftError.Create(Format(cBlockNotFound, [4]), 0, 1, 1, FErrors.Count));
end;

function TSwiftParser.ReadUntilBlockEnd(const aText: string;
  aOffset: Integer): string;
var
  Index: Integer;
  Len, Starts, Count, Start: Integer;
  CheckNested, IsTextBlock_: Boolean;
  CurrentLine: Integer;
begin
  IsTextBlock_ := False; // анализируемый блок не текстовый
  CheckNested := True; // проверка на закрыващуюся скобку блока '}'
  Count := 0;
  Index := aOffset + 1;

  // позиция начала блока и его длина
  Start := aOffset;
  Len := 0;

  Starts := 1; // говорит о том, что начало блока уже положено

  CurrentLine := FCurrentLine;

  while True do
  begin
    // анализ считываемого текста на предмет специального блока, текстового
    if (not IsTextBlock_) and (Count >= 3) then
    begin
      IsTextBlock_ := IsTextBlock(Copy(aText, Start, Count));
      if IsTextBlock_ then
        CheckNested := False;
    end;

    Inc(Count);

    // если дошли до конца разбираемого текста
    if Index > Length(aText) then
      Break
    else
    begin
      if CheckNested and IsBlockStart(aText[Index]) then
        Inc(Starts);

      if IsBlockEnd(IsTextBlock_, aText, Index) then
      begin
        if CheckNested then
        begin
          Dec(Starts);
          if Starts = 0 then
            Break
          else Inc(Len);
        end else
          Break;
      end else
        Inc(Len);
    end;

    Correct(aText, FCurrentLine, Index, Index);
    Inc(Index);
  end;

  // не совпадает по количеству открывающихся и закрывающихся фигурных скобок
  if CheckNested then
    if (Starts <> 0) and (Len > 0) then
      UnbalancedBrackets(CurrentLine);

  Result := Copy(aText, Start + 1, Len);
end;

function TSwiftParser.SubString(const aData: string; aBegin,
  aEnd: Integer): string;
begin
  Result := Copy(aData, aBegin, aEnd - aBegin);
end;

function TSwiftParser.TagListBlockConsume(aBlock: TSwiftTagListBlock;
  const aTextBlock: string; aLine: Integer): TSwiftBlock;
var
  Finish, Index, Line, Temp: Integer;
  Data: string;
begin
  Data := aTextBlock;
  // Сохраним текст
  aBlock.Text := Data;

  Index := 1;
  while (Index > 0) and (Index <= Length(Data)) do
  begin
    Correct(Data, aLine, Index, Index);
    Line := aLine;

    if Data[Index] = '{' then
    begin
      Finish := PosEx('}', Data, Index);
      if Finish > 1 then
      begin
        aBlock.AddTag(CreateTag(SubString(Data, Index + 1, Finish), Line));

        Correct(Data, aLine, Finish, Index);
        Index := Finish + 1;
        Continue;
      end;
    end
    else
    begin
      Temp := Index;
      // пропускаем все символы, пока не найдем начало блока
      Index := PosEx('{', Data, Index);
      if Index >= 0 then
        InvalidSymbols(Line, SubString(Data, Temp, IfThen(Index > 0, Index, Length(Data) + 1)));

      Correct(Data, aLine, Index, Temp);
      Continue;
    end;

    Correct(Data, aLine, Index, Index);
    Inc(Index);
  end;

  Result := aBlock;
end;

procedure TSwiftParser.UnbalancedBrackets(aLine: Integer);
begin
  FErrors.Add(
    TSwiftError.Create(cUnbalancedBrackets, 0, aLine, 1, FErrors.Count));
end;

procedure TSwiftParser.UnknownBlock(aLine: Integer; const aData: string);
begin
  FErrors.Add(
    TSwiftError.Create(
      Format(cUnknownBlock, [aData]), 0, aLine, 1, FErrors.Count));
end;
{$ENDREGION}

{$Region 'TSwiftDatabase'}

class function TSwiftDatabase.GetMandatoryFields(
  aMsgType: Integer): TSwiftMandatoryFieldArray;
var
  Index: Integer;

  function GetMandatoryFieldsFromArray(aFieldsArray: array of TSwiftFieldsRec): TSwiftMandatoryFieldArray;
  var
    I : Integer;
  begin
    SetLength(Result, 0);
    for I := 0 to High(aFieldsArray) do begin
      if (aFieldsArray[ I ].Mandatory in [1, 2]) then begin
        SetLength(Result, Length(Result) + 1);
        Result[ High(Result) ] := TSwiftMandatoryField.Create( aFieldsArray[ I ].FullName,
          aFieldsArray[ I ].Mandatory);
      end;
    end;
  end;

begin
  if not Contains(aMsgType, [518, 541, 543]) then begin
    Exit;
  end else begin
    case aMsgType of
    518: Result := GetMandatoryFieldsFromArray(cSwiftFieldsFour518);
    541,543: Result := GetMandatoryFieldsFromArray(cSwiftFieldsFour541);
    end;
  end;

end;

class function TSwiftDatabase.GetMsgFormat(aMsgType: Integer;
  const aFieldName: string): TSwiftMsgFormat;
begin
  if Contains(aMsgType, [518, 541, 543]) then begin
	  Result := TSwiftMsgFormat.Empty();
  end;
end;

class function TSwiftDatabase.GetMsgFormat(aMsgType: Integer;
  const aField: TSwiftField): TSwiftMsgFormat;

  function GetFieldsFormatFromArray(aFieldsArray: array of TSwiftFieldsRec;
    const aFieldName: string): TSwiftMsgFormat;
  var
    I : Integer;
  begin
    for I := 0 to High(aFieldsArray) do begin
      if (SameText(aFieldsArray[ I ].FullName, aFieldName)) or
         (Pos(aFieldsArray[ I ].FullName, aFieldName) > 0) then begin
        Result := TSwiftMsgFormat.Create(
            aFieldsArray[ I ].Content, aFieldsArray[ I ].Pattern,'','',
            Boolean(aFieldsArray[ I ].Mandatory));
        Break;
      end else
        Result := TSwiftMsgFormat.Empty();
    end;
  end;

begin
  case aMsgType of
    518: Result := GetFieldsFormatFromArray(cSwiftFieldsFour518, aField.Tag.FullName);
    541,543: Result := GetFieldsFormatFromArray(cSwiftFieldsFour541, aField.Tag.FullName);
  end;
end;
{$ENDREGION}

{$Region 'TSwiftMsgFormat'}

constructor TSwiftMsgFormat.Create(const aContent, aPattern, aNotes, aDescription: string;
  aMandatory: Boolean);
begin
  Content := aContent;
  Pattern := aPattern;
  Notes := aNotes;
  Description := aDescription;
  Mandatory := aMandatory;
end;

class function TSwiftMsgFormat.Empty: TSwiftMsgFormat;
begin
  Result := TSwiftMsgFormat.Create('', '', '', '', False);
end;

function TSwiftMsgFormat.IsEmpty: Boolean;
begin
  Result := (Content = '') and (Pattern = '') and (Notes = '') and
    (Description = '') and (Mandatory = False);
end;
{$ENDREGION}

{$Region 'TSwift'}

class function TSwift.GetInstance(aMsgType: Integer): TSwiftMessage;
begin
  Result := nil;
  try
    Result := TSwiftMessage.Create(aMsgType);
  except
    on E: Exception do
      Exception.RaiseOuterException(ESwiftException.Create(E.Message));
  end;
end;

function SplitString(const S, Delimiter: string): TStringDynArray;
var
  StartIdx, FoundIdx: Integer;
  SplitPoints, CurrentSplit: Integer;
  i: Integer;
begin
  Result := nil;

  if S <> '' then
  begin
    { Determine the length of the resulting array }
    SplitPoints := 0;

    i := PosEx(Delimiter, s);
    while i <> 0 do
    begin
      Inc(i, Length(Delimiter));
      Inc(SplitPoints);
      i := PosEx(Delimiter, s, i);
    end;

    SetLength(Result, SplitPoints + 1);

    { Split the string and fill the resulting array }
    StartIdx := 1;
    CurrentSplit := 0;
    repeat
      FoundIdx := PosEx(Delimiter, S, StartIdx);
      if FoundIdx <> 0 then
      begin
        Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
        Inc(CurrentSplit);
        StartIdx := FoundIdx + Length(Delimiter);
      end;
    until CurrentSplit = SplitPoints;

    // copy the remaining part in case the string does not end in a delimiter
    Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
  end;
end;

function TrimEnd(const aText, aSubText: string): string; overload;
var
  I: Integer;
begin
  I := Length(aText) - Length(aSubText) + 1;
  if (I > 0) and (not SameText(Copy(aText, I, Length(aSubText)), aSubText)) then Exit(aText);
  while (I > 0) and SameText(Copy(aText, I, Length(aSubText)), aSubText) do Dec(I, Length(aSubText));
  Result := Copy(aText, 1, I + Length(aSubText) - 1);
end;

function TrimEnd(const aText: string; aChars: TSysCharSet): string; overload;
var
  I: Integer;
begin
  I := Length(aText);
  if (I > 0) and (not CharInSet(aText[I], aChars)) then Exit(aText);
  while (I > 0) and (CharInSet(aText[I], aChars)) do Dec(I);
  Result := Copy(aText, 1, I);
end;

class function TSwiftArchive.MatchBlockFour(aMsgType: Integer; const aLeft, aRight: string;
  aMatchTags: array of TSwiftMatchTag; aMatching: TSwiftMatchingFunc): Boolean;
var
  Left, Right: TSwiftMessage;
  Parser: TSwiftParser;
  eMsgType: Integer;
begin
  try
    Left := TSwift.GetInstance(aMsgType);
    // для сверки swift сообщений различных типов
    if Contains(aMsgType, [541,543,545,547]) then begin
      case aMsgType of
      541: eMsgType := 545;
      543: eMsgType := 547;
      545: eMsgType := 541;
      547: eMsgType := 543;
      end;
    end else eMsgType := aMsgType;

    Right := TSwift.GetInstance(eMsgType);
    try
      // разбор текстов SWIFT сообщений
      Parser := TSwiftParser.Create();
      try
        Parser.Build(Left, TrimEnd(aLeft, [#10, #13]));
        Parser.Build(Right, TrimEnd(aRight, [#10, #13]));
      finally
        Parser.Free();
      end;

      Result := aMatching(Left, Right, aMatchTags);
    finally
      Left.Free();
      Right.Free();
    end;
  except
    Result := False;
  end;
end;

class function TSwiftArchive.MsgMatching(aMsgType: Integer; const aLeft,
  aRight: string; aStrict: Boolean): Boolean;
begin
  try
    if aStrict then
    begin
      case aMsgType of
        300: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour300, TSwiftArchive.MatchBlockFour300);
        320: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour320, TSwiftArchive.MatchBlockFour320);
        600: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour600, TSwiftArchive.MatchBlockFour600);
        202: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour202, TSwiftArchive.MatchBlockFour202);
        518: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour518, TSwiftArchive.MatchBlockFour5n);
        541,545,543,547: Result := TSwiftArchive.MatchBlockFour(aMsgType,
          aLeft, aRight, cMatchTagBlockFour541, TSwiftArchive.MatchBlockFour5n);
        else
          Result := False;
      end;
    end
    else
    begin
      case aMsgType of
        300: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour300NotStrict, TSwiftArchive.MatchBlockFour300);
        320: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour320NotStrict, TSwiftArchive.MatchBlockFour320);
        600: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour600NotStrict, TSwiftArchive.MatchBlockFour600);
        202: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour202NotStrict, TSwiftArchive.MatchBlockFour202);
        518: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour518NotStrict, TSwiftArchive.MatchBlockFour5n);
        541,545,543,547: Result := TSwiftArchive.MatchBlockFour(aMsgType, aLeft, aRight,
          cMatchTagBlockFour541NotStrict, TSwiftArchive.MatchBlockFour5nNotStrict);
        else
          Result := False;
      end;
    end;
  except
    Result := False;
  end;
end;

class function TSwiftArchive.MsgMatchingProtocol(aMsgType: Integer; const aLeft,
  aRight: string; var aProtocol: string): Boolean;
begin
  case aMsgType of
    300: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour300, TSwiftArchive.MatchBlockFour300Protocol, aProtocol);
    320: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour320, TSwiftArchive.MatchBlockFour320Protocol, aProtocol);
    600: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour600, TSwiftArchive.MatchBlockFour600Protocol, aProtocol);
    202: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour202, TSwiftArchive.MatchBlockFour202Protocol, aProtocol);
    518: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour518, TSwiftArchive.MatchBlockFour5nProtocol, aProtocol);
    541,545,543,547: Result := TSwiftArchive.MatchBlockFourProtocol(aMsgType,
      aLeft, aRight, cMatchTagBlockFour541, TSwiftArchive.MatchBlockFour5nProtocol, aProtocol);
    else
      Result := False;
  end;
end;

class function TSwiftArchive.MatchBlockFour202(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
begin
  Result := TSwiftArchive.MatchBlockFour300(aLeft, aRight, aMatchTags);
end;

class function TSwiftArchive.MatchBlockFour202Protocol(aLeft,
  aRight: TSwiftMessage; aMatchTags: array of TSwiftMatchTag;
  var aProtocol: string): Boolean;
begin
  Result := TSwiftArchive.MatchBlockFour300Protocol(aLeft, aRight, aMatchTags, aProtocol);
end;

class function TSwiftArchive.MatchBlockFour300(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
var
  MatchTag: TSwiftMatchTag;
  SequenceLeft, Left, SequenceRight, Right: TSwiftTag;
  Data: TStringDynArray;
begin
  Result := False;
  try
    if Assigned(aLeft.Block4) and Assigned(aRight.Block4) then
      for MatchTag in aMatchTags do
      begin
        // если задан полный путь, sequence и имя поля, то...
        if MatchTag.Explicit then
        begin
          Data := SplitString(MatchTag.Left, '.');
          SequenceLeft := aLeft.Block4.GetSequence(Data[Length(Data)-2]);
          if Assigned(SequenceLeft) then
            Left := SequenceLeft.GetTagBy(Data[Length(Data)-1], MatchTag.Option) // Option - только по цифрам
          else Left := nil;

          Data := SplitString(MatchTag.Right, '.');
          SequenceRight := aRight.Block4.GetSequence(Data[Length(Data)-2]);
          if Assigned(SequenceRight) then
          begin
            if MatchTag.Option then
              if Assigned(Left) then
                Right := SequenceRight.GetTagBy(Data[Length(Data)-1] + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
              else Right := nil
            else Right := SequenceRight.GetTagBy(Data[Length(Data)-1]);
          end
          else Right := nil;

          // если нету последовательностей с обеих сторон, то это нормально
          if not Assigned(SequenceLeft) and not Assigned(SequenceRight) then
            Continue;
        end
        else
        begin
          Left := aLeft.Block4.GetTagBy(MatchTag.Left, MatchTag.Option);
          if MatchTag.Option then
            if Assigned(Left) then
              Right := aRight.Block4.GetTagBy(MatchTag.Right + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
            else Right := nil
          else Right := aRight.Block4.GetTagBy(MatchTag.Right);
        end;

        // если нету полей с обеих сторон, то это нормально
        if not Assigned(Left) and not Assigned(Right) then
          Continue;

        if not Assigned(Left) or not Assigned(Right) or
           not SameText(Left.Value, Right.Value) then
          Exit;
      end;

    Result := True;
  except
    Result := False;
  end;
end;

class function TSwiftArchive.MatchBlockFour300Protocol(aLeft,
  aRight: TSwiftMessage; aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean;
var
  MatchTag: TSwiftMatchTag;
  SequenceLeft, Left, SequenceRight, Right: TSwiftTag;
  Data: TStringDynArray;
  iTag: TSwiftTag;
begin
  aProtocol := '';
  Result := False;

  if Assigned(aLeft.Block4) and Assigned(aRight.Block4) then
  begin
    for MatchTag in aMatchTags do
    begin
      // если задан полный путь, sequence и имя поля, то...
      if MatchTag.Explicit then
      begin
        Data := SplitString(MatchTag.Left, '.');
        SequenceLeft := aLeft.Block4.GetSequence(Data[Length(Data)-2]);
        if Assigned(SequenceLeft) then
          Left := SequenceLeft.GetTagBy(Data[Length(Data)-1], MatchTag.Option) // Option - только по цифрам
        else Left := nil;

        Data := SplitString(MatchTag.Right, '.');
        SequenceRight := aRight.Block4.GetSequence(Data[Length(Data)-2]);
        if Assigned(SequenceRight) then
          Right := SequenceRight.GetTagBy(Data[Length(Data)-1], MatchTag.Option)
        else Right := nil;

        // если нету последовательностей с обеих сторон, то это нормально
        if not Assigned(SequenceLeft) and not Assigned(SequenceRight) then
          Continue;
      end
      else
      begin
        Left := aLeft.Block4.GetTagBy(MatchTag.Left, MatchTag.Option);
        Right := aRight.Block4.GetTagBy(MatchTag.Right, MatchTag.Option);
      end;

      // если нету полей с обеих сторон, то это нормально
      if not Assigned(Left) and not Assigned(Right) then
        Continue;

      if Assigned(Right) then
      begin
        if Assigned(Left) then
        begin
          if not SameText(Left.Value, Right.Value) then
            aProtocol := aProtocol + Format('Поле "%s": %s, требуется: %s',
              [iff(MatchTag.Option,
                   MatchTag.Right + copy(Right.FName, length(Right.FName), 1),
                   MatchTag.Right),
               ReplaceStr(Right.Value, ''#$D#$A'', '   '),
               ReplaceStr(Left.Value, ''#$D#$A'', '   ')]) + sLineBreak
        end
        else
          aProtocol := aProtocol + Format('Поле "%s": %s, требуется: %s',
            [iff(MatchTag.Option,
                   MatchTag.Right + copy(Right.FName, length(Right.FName), 1),
                   MatchTag.Right),
             ReplaceStr(Right.Value, ''#$D#$A'', '   '),
             'должно отсутствовать']) + sLineBreak;
      end
      else
      begin
        if Assigned(Left) then
          aProtocol := aProtocol + Format('Поле "%s": не найдено, требуется: %s',
            [iff(MatchTag.Option, MatchTag.Right + 'a', MatchTag.Right),
             ReplaceStr(Left.Value,
             ''#$D#$A'', '   ')]) + sLineBreak;
      end;
    end;

    if aRight.MsgType = 300 then
    begin
      aProtocol := aProtocol + sLineBreak + 'Дополнительная информация по сообщениям' + sLineBreak;
      iTag := aLeft.Block4.GetSequence('33B');
      if Assigned(iTag) then
      begin
        iTag := iTag.GetTagBy('58', True);
        if Assigned(iTag) then
          aProtocol := aProtocol + Format('Поле "%s": исходного сообщения: %s',
            ['15B.33B.' + iTag.Name, ReplaceStr(iTag.Value, ''#$D#$A'', '   ')]) + sLineBreak
        else
          aProtocol := aProtocol + Format('Поле "%s": исходного сообщения: %s',
            ['15B.33B.58a', 'не найдено']) + sLineBreak
      end;
      iTag := aright.Block4.GetSequence('33B');
      if Assigned(iTag) then
      begin
        iTag := iTag.GetTagBy('58', True);
        if Assigned(iTag) then
          aProtocol := aProtocol + Format('Поле "%s": сообщения, с которым сверяемся: %s',
            ['15B.33B.' + iTag.Name, ReplaceStr(iTag.Value, ''#$D#$A'', '   ')]) + sLineBreak
        else
          aProtocol := aProtocol + Format('Поле "%s": сообщения, с которым сверяемся: %s',
            ['15B.33B.58a', 'не найдено']) + sLineBreak
      end;
    end;
  end;

  if aProtocol <> '' then
    Result := True;
end;

class function TSwiftArchive.MatchBlockFour320(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
var
  MatchTag: TSwiftMatchTag;
  SequenceLeft, Left, SequenceRight, Right, Tag: TSwiftTag;
  Data: TStringDynArray;
begin
  Result := False;
  try
    if Assigned(aLeft.Block4) and Assigned(aRight.Block4) then
      for MatchTag in aMatchTags do
      begin
        // если задан полный путь, sequence и имя поля, то...
        if MatchTag.Explicit then
        begin
          Data := SplitString(MatchTag.Left, '.');
          SequenceLeft := aLeft.Block4.GetSequence(Data[Length(Data)-2]);
          if Assigned(SequenceLeft) then
            Left := SequenceLeft.GetTagBy(Data[Length(Data)-1], MatchTag.Option) // Option - только по цифрам
          else Left := nil;

          Data := SplitString(MatchTag.Right, '.');
          SequenceRight := aRight.Block4.GetSequence(Data[Length(Data)-2]);
          if Assigned(SequenceRight) then
          begin
            if MatchTag.Option then
              if Assigned(Left) then
                Right := SequenceRight.GetTagBy(Data[Length(Data)-1] + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
              else Right := nil
            else Right := SequenceRight.GetTagBy(Data[Length(Data)-1]);
          end
          else Right := nil;

          // если нету последовательностей с обеих сторон, то это нормально
          if not Assigned(SequenceLeft) and not Assigned(SequenceRight) then
            Continue;
        end
        else
        begin
          Left := aLeft.Block4.GetTagBy(MatchTag.Left, MatchTag.Option);
          if MatchTag.Option then
            if Assigned(Left) then
              Right := aRight.Block4.GetTagBy(MatchTag.Right + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
            else Right := nil
          else Right := aRight.Block4.GetTagBy(MatchTag.Right);
        end;

        // если нету полей с обеих сторон, то это нормально
        if not Assigned(Left) and not Assigned(Right) then
          Continue;

        // специальное сравнение (17R 320)
        if SameText(MatchTag.Left, '15B.17R') then
        begin
          if Assigned(Left) and Assigned(Right) then
          begin
            if (SameText(Left.Value, 'L') and SameText(Right.Value, 'B')) or
               (SameText(Left.Value, 'B') and SameText(Right.Value, 'L')) then
              Continue;
          end;
          Exit;
        end;

        // для опциональных полей (32H 320)
        // поле 22B должно быть равно "MATU", тогда сравнивается 32H
        if SameText(MatchTag.Left, '15B.32H') then
        begin
          Tag := aLeft.Block4.GetTagBy('22B');
          if not Assigned(Tag) and not SameText(Tag.Value, 'MATU') then
            Continue;

          Tag := aRight.Block4.GetTagBy('22B');
          if not Assigned(Tag) and not SameText(Tag.Value, 'MATU') then
            Continue;
        end;

        if not Assigned(Left) or not Assigned(Right) or
           not SameText(Left.Value, Right.Value) then
          Exit;
      end;

    Result := True;
  except
    Result := False;
  end;
end;

class function TSwiftArchive.MatchBlockFour320Protocol(aLeft,
  aRight: TSwiftMessage; aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean;
var
  MatchTag: TSwiftMatchTag;
  SequenceLeft, Left, SequenceRight, Right, Tag: TSwiftTag;
  Data: TStringDynArray;
begin
  aProtocol := '';
  Result := False;

  if Assigned(aLeft.Block4) and Assigned(aRight.Block4) then
    for MatchTag in aMatchTags do
    begin
      // если задан полный путь, sequence и имя поля, то...
      if MatchTag.Explicit then
      begin
        Data := SplitString(MatchTag.Left, '.');
        SequenceLeft := aLeft.Block4.GetSequence(Data[Length(Data)-2]);
        if Assigned(SequenceLeft) then
          Left := SequenceLeft.GetTagBy(Data[Length(Data)-1], MatchTag.Option) // Option - только по цифрам
        else Left := nil;

        Data := SplitString(MatchTag.Right, '.');
        SequenceRight := aRight.Block4.GetSequence(Data[Length(Data)-2]);
        if Assigned(SequenceRight) then
        begin
          if MatchTag.Option then
            if Assigned(Left) then
              Right := SequenceRight.GetTagBy(Data[Length(Data)-1] + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
            else Right := nil
          else Right := SequenceRight.GetTagBy(Data[Length(Data)-1]);
        end
        else Right := nil;

        // если нету последовательностей с обеих сторон, то это нормально
        if not Assigned(SequenceLeft) and not Assigned(SequenceRight) then
          Continue;
      end
      else
      begin
        Left := aLeft.Block4.GetTagBy(MatchTag.Left, MatchTag.Option);
        if MatchTag.Option then
          if Assigned(Left) then
            Right := aRight.Block4.GetTagBy(MatchTag.Right + Copy(Left.Name, Length(Left.Name), 1))// а символ из левой части
          else Right := nil
        else Right := aRight.Block4.GetTagBy(MatchTag.Right);
      end;

      // если нету полей с обеих сторон, то это нормально
      if not Assigned(Left) and not Assigned(Right) then
        Continue;

      // специальное сравнение (17R 320)
      if SameText(MatchTag.Left, '15B.17R') then
      begin
        if Assigned(Left) and Assigned(Right) then
        begin
          if (SameText(Left.Value, 'L') and SameText(Right.Value, 'B')) or
             (SameText(Left.Value, 'B') and SameText(Right.Value, 'L')) then
            Continue
          else
          begin
            aProtocol := aProtocol + Format('Поле "%s": %s, требуется: %s',
              [MatchTag.Right, ReplaceStr(Right.Value, ''#$D#$A'', ''), IfThen(Left.Value = 'L', 'B', 'L')]) + sLineBreak;
            Continue;
          end;
        end
        else
        begin
          if Assigned(Left) then
          begin
            aProtocol := aProtocol + Format('Поле "%s": не найдено, требуется: %s',
              [MatchTag.Right, IfThen(Left.Value = 'L', 'B', 'L')]) + sLineBreak;
            Continue;
          end
          else if Assigned(Right) then
          begin
            aProtocol := aProtocol + Format('Поле "%s": %s, требуется: должно отсутствовать',
              [MatchTag.Right, ReplaceStr(Right.Value, ''#$D#$A'', '')]) + sLineBreak;
            Continue;
          end;
          Continue;
        end;
      end;

      // для опциональных полей (32H 320)
      // поле 22B должно быть равно "MATU", тогда сравнивается 32H
      if SameText(MatchTag.Left, '15B.32H') then
      begin
        Tag := aLeft.Block4.GetTagBy('22B');
        if not Assigned(Tag) and not SameText(Tag.Value, 'MATU') then
          Continue;

        Tag := aRight.Block4.GetTagBy('22B');
        if not Assigned(Tag) and not SameText(Tag.Value, 'MATU') then
          Continue;
      end;

      if Assigned(Right) then
      begin
        if Assigned(Left) then
        begin
          if not SameText(Left.Value, Right.Value) then
            aProtocol := aProtocol + Format('Поле "%s": %s, требуется: %s',
              [MatchTag.Right, ReplaceStr(Right.Value, ''#$D#$A'', ''), ReplaceStr(Left.Value, ''#$D#$A'', '')]) + sLineBreak
        end
        else
          aProtocol := aProtocol + Format('Поле "%s": %s, требуется: %s',
            [MatchTag.Right, ReplaceStr(Right.Value, ''#$D#$A'', ''), 'должно отсутствовать']) + sLineBreak;
      end
      else
      begin
        if Assigned(Left) then
          aProtocol := aProtocol + Format('Поле "%s": не найдено, требуется: %s',
            [MatchTag.Right, ReplaceStr(Left.Value, ''#$D#$A'', '')]) + sLineBreak;
      end;
    end;

  if aProtocol <> '' then
    Result := True;
end;

class function TSwiftArchive.MatchBlockFour600(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
begin
  Result := TSwiftArchive.MatchBlockFour300(aLeft, aRight, aMatchTags);
end;

class function TSwiftArchive.MatchBlockFour600Protocol(aLeft,
  aRight: TSwiftMessage; aMatchTags: array of TSwiftMatchTag;
  var aProtocol: string): Boolean;
begin
  Result := TSwiftArchive.MatchBlockFour300Protocol(aLeft, aRight, aMatchTags, aProtocol);
end;

class function TSwiftArchive.MatchBlockFour5Types(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag; var aProtocol: string; aLogging: Boolean = False; aStrict: Boolean = True): Boolean;
type
  TSwiftMatchTagEx = record
    ATagKeyL, AQualifierL, ATagKeyR, AQualifierR: string;
  end;

const cTagKeysEx: TSwiftMatchTagEx =
  (ATagKeyL: '^(20C.GENL.SEME)$';  AQUalifierL: 'SEME'; ATagKeyR: '^(20C.GENL.LINK.RELA)$';  AQUalifierR: 'RELA');

var
  MatchTag: TSwiftMatchTag;
  eTagLeft, eTagRight: TSwiftTag;

  eDictLeft, eDictRight: TDictionary<string, TSwiftTag>;
  eSeq, eSubSeq, eTagKeyL, eTagKeyR, eQualifierL, eQualifierR: string;

  eMatches: TStringList;
  I: Integer;

  // заполняем структуру типа Dictionary из блока 4
  procedure FillDictionary(aBlock: TSwiftBlock4;
    out aDictionary: TDictionary<string, TSwiftTag>);
  var
    I: Integer;
    eTagKey: string;
    eTmpTag: TSwiftTag;
  begin
    // создаем коллекцию объектов типа ключ - значение
    // ключ строится по принципу:
    // Tag(имя поля).Seq(имя последовательности).[SubSeq(имя подпоследовательности)].[Qualifier(имя квалификатора)]
    for I := 0 to aBlock.GetFieldList.Count - 1 do begin
      eTmpTag := aBlock.GetFieldList.Items[I].Tag;
      if (SameText(eTmpTag.Name, '16R')) then begin                           // это начало последовательности
        if (eSeq > '') and (eSeq <> eTmpTag.Value) then begin
          eSubSeq := eTmpTag.Value;
        end else eSeq := eTmpTag.Value;
      end;
      if (SameText(eTmpTag.Name, '16S')) then begin                           // это закрытие секции
        if eSeq = eTmpTag.Value then
          eSeq := ''
        else if eSubSeq = eTmpTag.Value then
          eSubSeq := '';
      end;

      if MatchText(eTmpTag.Name, ['16R','16S']) then Continue;                // тэг с наименованием последовательности пропускаем

      eTagKey := eTmpTag.Name;                                                // наименование тэга
      if (Assigned(eTmpTag.ParentSequence)) and (eSeq > '') then
        eTagKey := eTagKey + '.' + eSeq;                                      // добавляем наименование последовательности
      if (Assigned(eTmpTag.ParentSequence.ParentSequence)) and (eSubSeq > '') then
        eTagKey := eTagKey + '.' + eSubSeq;                                   // добавляем наименование подпоследовательности
      if (eTmpTag.Qualifier > '') then
        eTagKey := eTagKey + '.' + eTmpTag.Qualifier;                         // добавляем наименование квалификатора

      aDictionary.AddOrSetValue( eTagKey, eTmpTag );
    end;
  end;

  function GetValueByRegExp(const aPattern: string; aDict: TDictionary<string, TSwiftTag>; out aTag: TSwiftTag): Boolean;
  var
    eKey, eFindKey: string;
    eRegEx: TRegEx;
  begin
    Result := False;
    aTag := nil;
    // поиск ключа по регулярному выражению
    eFindKey := '';
    for eKey in aDict.Keys do begin
      if eRegEx.IsMatch(eKey, aPattern) then begin
        eFindKey := eKey;
        Break;
      end;
    end;
    if eFindKey > '' then
      Result := aDict.TryGetValue(eFindKey, aTag);
  end;

  function GetMatchesByRegExp(const aPattern: string;
    aDict: TDictionary<string, TSwiftTag>; out aMatches: TStringList): Boolean;
  var
    eKey, eFindKey: string;
    eRegEx: TRegEx;
  begin
    aMatches.Clear;
    // поиск ключа по регулярному выражению
    eFindKey := '';
    for eKey in aDict.Keys do begin
      if eRegEx.IsMatch(eKey, aPattern) then begin
        aMatches.Add(eKey);
      end;
    end;
    Result := aMatches.Count > 0;
  end;

  function CompareField(const aLeftKey, aRightKey: string; Option: Boolean): Boolean;
  var
    eLeftVal, eRightVal: string;
  begin
    Result := False;
    // поиск элемента по регулярному выражению и сравнение
    GetValueByRegExp(aLeftKey, eDictLeft, eTagLeft);
    GetValueByRegExp(aRightKey, eDictRight, eTagRight);

    // если нет полей с обеих сторон, то это нормально
    if not Assigned(eTagLeft) and not Assigned(eTagRight) then begin
      Result := True;
      Exit;
    end;
    // если нет поля слева или справа или значения полей не совпадают,
    // то матчинг не пройден
    if Assigned(eTagRight) then begin
      if Assigned(eTagLeft) then begin
        // поле 35B.TRADDET - сверка только номера ISIN
        if TRegEx.IsMatch(aLeftKey, '35B.TRADDET') then begin
          eLeftVal  := Trim(GetBetweenEx('ISIN', #$D#$A, eTagLeft.Value));
          eRightVal := Trim(GetBetweenEx('ISIN', #$D#$A, eTagRight.Value));
          if (not SameText(eLeftVal, eRightVal)) then begin
            if aLogging then begin
              aProtocol := aProtocol + Format(#7#32'MT%d Поле "%s": %s, требуется: %s',
                [aRight.MsgType, eTagRight.Name, eRightVal, eLeftVal]) + sLineBreak;
              Exit;
            end;
            Exit;
          end;
        end
        else
        if ((not Option) and (not SameText(eTagLeft.Value, eTagRight.Value))) or
           ((Option) and (not SameText(SeparateRight(eTagLeft.Value, eTagLeft.Qualifier + '//'), SeparateRight(eTagRight.Value, eTagRight.Qualifier + '//')))) then begin
          if aLogging then begin
            aProtocol := aProtocol + Format(#7#32'MT%d Поле "%s": %s, требуется: %s',
              [aRight.MsgType, eTagRight.Name, ReplaceStr(eTagRight.Value, #$D#$A, #32),
               ReplaceStr(eTagLeft.Value, #$D#$A, #32)]) + sLineBreak;
            Exit;
          end;
          Exit;
        end;

      end else begin
        if aLogging then begin
          aProtocol := aProtocol + Format(#7#32'MT%d Поле "%s": не найдено, требуется: %s',
            [aRight.MsgType, eTagRight.Name, eTagRight.Value]) + sLineBreak;
            Exit;
        end;
        Exit;
      end;
    end else begin
      if Assigned(eTagLeft) and aLogging then begin
        aProtocol := aProtocol + Format(#7#32'MT%d Поле "%s": не найдено, требуется: %s',
          [aLeft.MsgType, eTagLeft.Name, eTagLeft.Value]) + sLineBreak;
        Exit;
      end;
      Exit;
    end;
    Result := True;
  end;

begin
  Result := False;
  try
    if Assigned(aLeft.Block4) and Assigned(aRight.Block4) then
    // 1. Создаем коллекцию TDictionary типа ключ = значение
    eDictLeft  := TDictionary<string, TSwiftTag>.Create;
    eDictRight := TDictionary<string, TSwiftTag>.Create;

    // 2. Заполняем структуру
    FillDictionary(aLeft.Block4,  eDictLeft);
    FillDictionary(aRight.Block4, eDictRight);

    try
      { перебор коллекции паттернов полей для матчинга }
      for MatchTag in aMatchTags do
      begin
        if not CompareField(MatchTag.Left, MatchTag.Right, MatchTag.Option) then
          if aLogging then Continue else Exit;
      end;

      // дополнительное сравнение поля 22H последовательности CONFDET
      // для МТ518
      if (aLeft.MsgType = 518) and (aStrict) then begin
        GetValueByRegExp('^(22H.CONFDET.BUSE)$', eDictLeft, eTagLeft);
        GetValueByRegExp('^(22H.CONFDET.BUSE)$', eDictRight, eTagRight);

        // если нет полей с обеих сторон, то это нормально
        if not Assigned(eTagLeft) and not Assigned(eTagRight) then begin
          Result := True;
          Exit;
        end;

        if (Assigned(eTagLeft)) and (Assigned(eTagRight)) then begin
          if (Pos('SELL', eTagLeft.Value) > 0) then begin
            if (not Pos('BUYI', eTagRight.Value) > 0) and aLogging then begin
              aProtocol := aProtocol + Format(#7#32'Поле "%s": %s, требуется: %s',
              [eTagRight.Name, eTagRight.Value, 'BUYI']) + sLineBreak;
            end;
          end
          else if (Pos('BUYI', eTagLeft.Value) > 0 ) then begin
            if (not Pos('SELL', eTagRight.Value) > 0) and aLogging then begin
              aProtocol := aProtocol + Format(#7#32'Поле "%s": %s, требуется: %s',
              [eTagRight.Name, eTagRight.Value, 'SELL']) + sLineBreak;
            end;
          end;
        end else Exit;
        if (not aLogging) or (aProtocol > '') then
          Result := True;
      end;

      // дополнительное сравнение поля 20С последовательности GENL
      // для МТ541,МТ543,MT545,MT547
      if MatchText(IntToStr(aLeft.MsgType), ['541','543','545','547'])  then begin
        // 1. Проверка поля LINK/20C/
        eTagKeyL    := ''; eQualifierL := '';
        eTagKeyR    := ''; eQualifierR := '';
        if Contains(aLeft.MsgType, [541, 543 ]) then begin
          eTagKeyL    := cTagKeysEx.ATagKeyL;
          eQualifierL := cTagKeysEx.AQualifierL;
        end;
        if Contains(aLeft.MsgType, [545, 547 ]) then begin
          eTagKeyL    := cTagKeysEx.ATagKeyR;
          eQualifierL := cTagKeysEx.AQualifierR;
        end;
        if Contains(aRight.MsgType, [541, 543 ]) then begin
          eTagKeyR    := cTagKeysEx.ATagKeyL;
          eQualifierR := cTagKeysEx.AQualifierL;
        end;
        if Contains(aRight.MsgType, [545, 547 ]) then begin
          eTagKeyR    := cTagKeysEx.ATagKeyR;
          eQualifierR := cTagKeysEx.AQualifierR;
        end;

        if not CompareField(eTagKeyL, eTagKeyR, True) then begin
          Result := True;
          Exit;
        end;

        if (not aLogging) or (aProtocol > '') then
          Result := True;
      end;

    finally
      eDictLeft.Free;
      eDictRight.Free;
    end;
  except
    Result := False;
  end;
end;

class function TSwiftArchive.MatchBlockFour5n(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
var
  eProtocol: string;
begin
  eProtocol := '';
  Result := MatchBlockFour5Types(aLeft, aRight, aMatchTags, eProtocol);
end;

class function TSwiftArchive.MatchBlockFour5nNotStrict(aLeft, aRight: TSwiftMessage;
  aMatchTags: array of TSwiftMatchTag): Boolean;
var
  eProtocol: string;
begin
  eProtocol := '';
  Result := MatchBlockFour5Types(aLeft, aRight, aMatchTags, eProtocol, False, False);
end;

class function TSwiftArchive.MatchBlockFour5nProtocol(aLeft, aRight: TSwiftMessage;
      aMatchTags: array of TSwiftMatchTag; var aProtocol: string): Boolean;
begin
  Result := MatchBlockFour5Types(aLeft, aRight, aMatchTags, aProtocol, True);
end;

class function TSwiftArchive.MatchBlockFourProtocol(aMsgType: Integer;
  const aLeft, aRight: string; aMatchTags: array of TSwiftMatchTag;
  aMatching: TSwiftMatchingProtocolFunc; var aProtocol: string): Boolean;
var
  Left, Right: TSwiftMessage;
  Parser: TSwiftParser;
  eMsgType: Integer;
begin
  try
    Left := TSwift.GetInstance(aMsgType);

    // для сверки swift сообщений различных типов
    if Contains(aMsgType, [541,543,545,547]) then begin
      case aMsgType of
      541: eMsgType := 545;
      543: eMsgType := 547;
      545: eMsgType := 541;
      547: eMsgType := 543;
      end;
    end else eMsgType := aMsgType;

    Right := TSwift.GetInstance(eMsgType);
    try
      // разбор текстов SWIFT сообщений
      Parser := TSwiftParser.Create();
      try
        Parser.Build(Left, TrimEnd(aLeft, [#10, #13]));
        Parser.Build(Right, TrimEnd(aRight, [#10, #13]));
      finally
        Parser.Free();
      end;

      Result := aMatching(Left, Right, aMatchTags, aProtocol);
    finally
      Left.Free();
      Right.Free();
    end;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
///  TSwift
////////////////////////////////////////////////////////////////////////////////
class function TSwift.Load(aMsgType: Integer; const aData: string): TSwiftMessage;
begin
  Result := TSwift.GetInstance(aMsgType);
  try
    with TSwiftParser.Create() do
      try
        Build(Result, TrimEnd(aData, [#10, #13]));
      finally
        Free();
      end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TSwift.MsgFormatValid(aMsgType: Integer; const aData: string;
  out aErrors: string; aFieldsProc: TSwiftFieldsProc): Boolean;
var
  Parser: TSwiftParser;
  Message: TSwiftMessage;
  Errors: TSwiftErrorList;
  Item: TPair<string, TSwiftFieldProc>;
begin
  Result := True;
  aErrors := '';

  try
    Message := TSwift.GetInstance(aMsgType);
    try
      Errors := TSwiftErrorList.Create(TSwiftErorrComparer.Create());
      try
        Parser := TSwiftParser.Create();
        try
          Parser.Build(Message, TrimEnd(aData, [#10, #13, '$']));
          Errors.AddRange(Parser.Errors);
        finally
          Parser.Free();
        end;

        if Assigned(aFieldsProc) then
          for Item in aFieldsProc do
            Message.FieldsProc.Add(Item.Key, Item.Value);

        Result := Message.Valid();
        Errors.AddRange(Message.Errors);
        Result := Result and (Errors.Count = 0);
      finally
        if not Result then
        begin
          Errors.Sort();
          aErrors := Errors.ToString();
        end;
        Errors.Free();
      end;
    finally
      Message.Free();
    end;
  except
    on E: Exception do
      Exception.RaiseOuterException(ESwiftException.Create(E.Message));
  end;
end;
{$ENDREGION}

{$Region 'TSwiftBlock'}

constructor TSwiftBlock.Create(aMessage: TSwiftMessage);
begin
  FColumn := 1;
  FLine := 1;
  FMessage := aMessage;
  FNumber := 0;
end;

procedure TSwiftBlock.Error(aLine: Integer; const aText: string);
begin
  FMessage.Errors.Add(TSwiftError.Create(aText, 1, aLine, 1, FMessage.Errors.Count));
end;

procedure TSwiftBlock.Error(aLine: Integer; const aFormat: string; const aArgs: array of const);
begin
  Error(aLine, Format(aFormat, aArgs));
end;
{$ENDREGION}

{$Region 'TSwiftTag'}

constructor TSwiftTag.Create(const aData: string);
var
  Index: Integer;
begin
  FColumn := 1;
  FLine := 1;
  Index := Pos(':', aData);
  FName := Copy(aData, 1, Index - 1);
  FValue := Copy(aData, Index + 1, Length(aData) - Index);
  FSequence := False;
  FTags := TSwiftTagList.Create(False);
  FParentSequence := nil;
  FQualifier := GetBetweenEx(':', '/', FValue);
end;

destructor TSwiftTag.Destroy;
begin
  FTags.Free();
  inherited;
end;

function TSwiftTag.GetTagBy(const aName: string; aOption: Boolean): TSwiftTag;
var
  Tag: TSwiftTag;
begin
  Result := nil;
  if aOption then
  begin
    for Tag in FTags do
      if Pos(aName, Tag.Name) = 1 then // только по цифрам в начале
        Exit(Tag);
  end
  else
  begin
    for Tag in FTags do
      if SameText(Tag.Name, aName) then
        Exit(Tag);
  end;
end;

procedure TSwiftTag.SetEditor(aValue: TCustomEdit);
begin
  if Assigned(aValue) then
    FEditor := aValue;
end;

function TSwiftTag.GetEditor: TCustomEdit;
begin
  Result := FEditor;
end;

{$ENDREGION}

{$Region 'TSwiftField'}

constructor TSwiftField.Create(aMessage: TSwiftMessage; aTag: TSwiftTag);
begin
  FMessage := aMessage;
  FTag := aTag;
end;

procedure TSwiftField.Error(aLine: Integer; const aText: string);
begin
  FMessage.Errors.Add(TSwiftError.Create(aText, 1, aLine, 1, FMessage.Errors.Count));
end;

procedure TSwiftField.Error(aLine: Integer; const aFormat: string; const aArgs: array of const);
begin
  Error(aLine, Format(aFormat, aArgs));
end;

function TSwiftField.Valid: Boolean;
var
  Validators: TSwiftValidatorClassArray;
begin
  // по умолчанию для всех TSwiftFieldNameValidator, TSwiftFieldValueValidator,
  // TSwiftPatternValidator
  Result := Validate(gFieldDefaultValidator);

  // валидаторы для данного поля соообщений
  if gFieldValidator.TryGetValue(FTag.Name, Validators) then
    Result := Validate(Validators) and Result;
end;
{$ENDREGION}

{$Region 'TSwiftTagListBlock'}

function TSwiftTagListBlock.AddTag(aTag: TSwiftTag; aSequence: TSwiftTag): TSwiftTag;
var
  Tag: TSwiftTag;
begin
  FTags.Add(aTag);
  Result := aTag;

  case FMessage.MsgType of
    300: begin
      // 32B и 33B являются подсекциями 15B
      if MatchText(Result.Name, ['32B', '33B']) then
      begin
        FSequence := aTag;

        Tag := GetTagBy('15B');
        if Assigned(Tag) then
        begin
          Tag.Tags.Add(aTag);
          aTag.ParentSequence := Tag;
        end;
        Exit;
      end;
    end;
  end;

  if aTag.Sequence then
  begin
    aTag.ParentSequence := FSequence;
    FSequence := aTag;
    if Assigned(aSequence) then
    begin
      FSequence := aSequence;
      FSequence.Tags.Add(aTag);
      aTag.ParentSequence := FSequence;
    end;
  end
  else
  begin
    if Assigned(aSequence) then
    begin
      FSequence := aSequence;
      FSequence.Tags.Add(aTag);
      aTag.ParentSequence := FSequence;
    end
    else if Assigned(FSequence) then
    begin
      FSequence.Tags.Add(aTag);
      aTag.ParentSequence := FSequence;
    end;
  end;
end;

constructor TSwiftTagListBlock.Create(aMessage: TSwiftMessage);
begin
  inherited;
  FTags := TSwiftTagList.Create(True);
  FSequence := nil;
end;

destructor TSwiftTagListBlock.Destroy;
begin
  FTags.Free();
  inherited;
end;

function TSwiftTagListBlock.ExistOptionTag(const aName: string): Boolean;
var
  Tag: TSwiftTag;
begin
  Result := False;
  for Tag in FTags do
    if Pos(aName, Tag.Name) = 1 then
      Exit(True);
end;

function TSwiftTagListBlock.ExistTag(const aName: string): Boolean;
var
  Tag: TSwiftTag;
begin
  Result := False;
  for Tag in FTags do
    if SameText(Tag.Name, aName) then
      Exit(True);
end;

function TSwiftTagListBlock.FieldBy(aTag: TSwiftTag): TSwiftField;
begin
  Result := TSwiftField.Create(FMessage, aTag);
end;

function TSwiftTagListBlock.GetCount: Integer;
begin
  Result := FTags.Count;
end;

function TSwiftTagListBlock.GetFieldBy(const aName: string): TSwiftField;
var
  Tag: TSwiftTag;
begin
  Tag := GetTagBy(aName);
  if Assigned(Tag) then
    Result := FieldBy(Tag)
  else Result := nil;
end;

function TSwiftTagListBlock.GetFieldList: TSwiftFieldList;
var
  Tag: TSwiftTag;
begin
  Result := TSwiftFieldList.Create(True);
  for Tag in FTags do
    Result.Add(FieldBy(Tag));
end;

function TSwiftTagListBlock.GetSequence(const aName: string): TSwiftTag;
var
  Tag: TSwiftTag;
begin
  Result := nil;
  for Tag in FTags do
    if Tag.Sequence and SameText(Tag.Name, aName) then
      Exit(Tag);
end;

function TSwiftTagListBlock.GetTag(Index: Integer): TSwiftTag;
begin
  Result := FTags[Index];
end;

function TSwiftTagListBlock.GetTagBy(const aName: string; aOption: Boolean): TSwiftTag;
var
  Tag: TSwiftTag;
begin
  Result := nil;
  if aOption then
  begin
    for Tag in FTags do
      if Pos(aName, Tag.Name) = 1 then // только по цифрам в начале
        Exit(Tag);
  end
  else
  begin
    for Tag in FTags do
      if SameText(Tag.Name, aName) then
        Exit(Tag);
  end;
end;

function TSwiftTagListBlock.Valid: Boolean;
begin
  Result := not (FTags.Count = 0);
  if not Result then
    Error(FLine, cEmptyBlock, [FNumber]);
end;
{$ENDREGION}

{$Region 'TSwiftBlock4'}

constructor TSwiftBlock4.Create(aMessage: TSwiftMessage);
begin
  inherited;
  FNumber := 4;
  FSwiftDict := TSwiftDict.Create;
end;

destructor TSwiftBlock4.Destroy();
begin
  FSwiftDict.Free;
  inherited;
end;

function TSwiftBlock4.ExistOptionTagBy(const aName: string): Boolean;
var
  eKey, eFindKey: string;
  eRegEx: TRegEx;
  eTag: TSwiftTag;
begin
  Result := False;
  eTag := nil;
  FillSwiftDict;
  if FSwiftDict.Count = 0 then Exit;

  // поиск ключа
  eFindKey := '';
  for eKey in FSwiftDict.Keys do begin
    if TRegEx.IsMatch(eKey, aName) then begin
      eFindKey := eKey;
      Break;
    end;
  end;

  if eFindKey > '' then begin
    if (FSwiftDict.TryGetValue(eFindKey, eTag)) and (Assigned(eTag)) then
      Result := True;
  end;
end;

function TSwiftBlock4.ExistTagBy(const aName: string): Boolean;
var
  eKey, eFindKey: string;
  eRegEx: TRegEx;
  eTag: TSwiftTag;
begin
  Result := False;
  eTag := nil;
  FillSwiftDict;
  if FSwiftDict.Count = 0 then Exit;

  // поиск ключа
  eFindKey := '';
  for eKey in FSwiftDict.Keys do begin
    if SameText(eKey, aName) then begin
      eFindKey := eKey;
      Break;
    end;
  end;

  if eFindKey > '' then begin
    if (FSwiftDict.TryGetValue(eFindKey, eTag)) and (Assigned(eTag)) then
      Result := True;
  end;
end;

function TSwiftBlock4.Valid: Boolean;
var
  Field: TSwiftField;
  Fields: TSwiftFieldList;
begin
  Result := inherited Valid();

  if Result then
  begin
    FillSwiftDict;
    Fields := GetFieldList();
    try
      for Field in Fields do
        Result := Field.Valid() and Result;
    finally
      Fields.Free();
    end;
  end;
end;

// заполняем структуру типа Dictionary из блока 4
procedure TSwiftBlock4.FillSwiftDict;
var
  I: Integer;
  eTmpTag: TSwiftTag;
  Field : TSwiftField;
  Fields: TSwiftFieldList;
  eTagKey, eSeq, eSubSeq: string;
begin
  // создаем коллекцию объектов типа ключ - значение
  // ключ строится по принципу:
  // Tag(имя поля).Seq(имя последовательности).[SubSeq(имя подпоследовательности)].[Qualifier(имя квалификатора)]
  FSwiftDict.Clear;
  Fields := GetFieldList;
  for Field in Fields do begin
    eTmpTag := Field.Tag;

    if (MatchText(eTmpTag.Name, cSequenceTags)) or
       (MatchText(eTmpTag.Name, ['32B','33B'])) then begin                      // это начало последовательности
      if (SameText(eTmpTag.Name, '16R')) then begin                             // для 5-ой категории
        if (eSeq > '') and (eSeq <> eTmpTag.Value) then begin
          eSubSeq := eTmpTag.Value;
        end else eSeq := eTmpTag.Value;
      end else begin
        if (MatchText(eTmpTag.Name, ['32B','33B'])) then begin
          eSubSeq := eTmpTag.Name;
        end else eSeq := eTmpTag.Name;
      end;
    end;

    // для блоков
    if (MatchText(eTmpTag.Name, cSequenceTags)) or
       (SameText(eTmpTag.Name, '16S')) then begin
      eTagKey := eSeq;
      if eSubSeq > '' then
        eTagKey := eTagKey + '.' + eSubSeq;
      eTagKey := eTagKey + '.' + eTmpTag.Name;

      eTmpTag.FullName := eTagKey;
      FSwiftDict.AddOrSetValue( eTagKey, eTmpTag );

      if (SameText(eTmpTag.Name, '16S')) then begin                               // это закрытие секции
        if eSeq = eTmpTag.Value then
          eSeq := ''
        else if eSubSeq = eTmpTag.Value then
          eSubSeq := '';
      end;

      Continue;
    end;

    eTagKey := eTmpTag.Name;                                                    // наименование тэга
    if (Assigned(eTmpTag.ParentSequence)) and (eSeq > '') then
      if (Assigned(eTmpTag.ParentSequence.ParentSequence)) and
         (eSubSeq > '') and (eSubSeq <> eTagKey) then
        eTagKey := eSeq + '.' + eSubSeq + '.' + eTagKey                         // добавляем наименование подпоследовательности
      else eTagKey := eSeq + '.' + eTagKey;                                     // добавляем наименование последовательности

    if (eTmpTag.Qualifier > '') then
      eTagKey := eTagKey + '.' + eTmpTag.Qualifier;                             // добавляем наименование квалификатора

    eTmpTag.FullName := eTagKey;
    FSwiftDict.AddOrSetValue( eTagKey, eTmpTag );
  end;
end;

function TSwiftBlock4.GetFieldByEx(const aFullName: string): TSwiftField;
var
  eKey, eFindKey: string;
  eTag: TSwiftTag;
begin
  Result := nil;
  eTag := nil;
  FillSwiftDict;
  if FSwiftDict.Count = 0 then Exit;

  // поиск ключа по регулярному выражению
  eFindKey := '';
  for eKey in FSwiftDict.Keys do begin
    if TRegEx.IsMatch(eKey, aFullName) then begin
      eFindKey := eKey;
      Break;
    end;
  end;
  if eFindKey > '' then begin
    if (FSwiftDict.TryGetValue(eFindKey, eTag)) and (Assigned(eTag)) then
      Result := TSwiftField.Create(FMessage, eTag);
  end;
end;

function TSwiftBlock4.GetFieldValueBy(const aName: string): string;
var
  eKey, eFindKey: string;
  eRegEx: TRegEx;
  eTag: TSwiftTag;
begin
  Result := '';
  eTag := nil;
  FillSwiftDict;
  if FSwiftDict.Count = 0 then Exit;

  // поиск ключа по регулярному выражению
  eFindKey := '';
  for eKey in FSwiftDict.Keys do begin
    if eRegEx.IsMatch(eKey, aName) then begin
      eFindKey := eKey;
      Break;
    end;
  end;
  if eFindKey > '' then begin
    if (FSwiftDict.TryGetValue(eFindKey, eTag)) and (Assigned(eTag)) then
      Result := eTag.Value;
  end;
end;

procedure TSwiftBlock4.RecreateTags;
begin
  FillSwiftDict;
end;

function TSwiftBlock4.SwiftDictToString: string;
var
  s: TStringBuilder;
  eKey: string;
  LArray : TArray<string>;
begin
  Result := '';
  s := TStringBuilder.Create;
  try
    FillSwiftDict;

    // Sort
    LArray := FSwiftDict.Keys.ToArray;
    TArray.Sort<string>(LArray);

    if FSwiftDict.Count = 0 then Exit;
    for eKey in LArray do
      s.AppendFormat('%s = %s'#10, [eKey, FSwiftDict.Items[ eKey ].Value]);
    Result := s.ToString;
  finally
    s.Free;
  end;
end;


{$ENDREGION}

{$Region 'TSwiftMessage'}

function TSwiftMessage.AddBlock(aBlock: TSwiftBlock): TSwiftBlock;
begin
  FBlocks.Add(aBlock);
  Result := aBlock;

  if aBlock is TSwiftBlock1 then
    FBlock1 := aBlock as TSwiftBlock1;
  if aBlock is TSwiftBlock2 then
    FBlock2 := aBlock as TSwiftBlock2;
  if aBlock is TSwiftBlock4 then
    FBlock4 := aBlock as TSwiftBlock4;
end;

constructor TSwiftMessage.Create(aMsgType: Integer);
begin
  FBlock1 := nil;
  FBlock2 := nil;
  FBlock4 := nil;
  FBlocks := TSwiftBlockList.Create(True);
  FErrors := TSwiftErrorList.Create();
  FFieldsProc := TSwiftFieldsProc.Create();
  FMsgType := aMsgType;
end;

destructor TSwiftMessage.Destroy;
begin
  FFieldsProc.Free();
  FErrors.Free();
  FBlocks.Free();
  inherited;
end;

function TSwiftMessage.GetBlock(Index: Integer): TSwiftBlock;
begin
  Result := FBlocks[Index];
end;

function TSwiftMessage.GetCount: Integer;
begin
  Result := FBlocks.Count;
end;

function TSwiftMessage.GetMsgText: string;
var
  eTag : TSwiftTag;
  eBuffer: TSWIFTBuilder;
  I: Integer;
  sHeader: string;
begin
  eBuffer := TSWIFTBuilder.Create();
  try
    sHeader := Format('{1:%s}{2:%s}', [FBlock1.Text, FBlock2.Text]);

    sHeader := sHeader + Format('{3:%s}', [Blocks[2].Text]);

    // строим текст блока 4
    eBuffer.AppendLine(sHeader + '{4:');
    for I := 0 to FBlock4.GetFieldList.Count - 1 do begin
      eTag := FBlock4.GetFieldList.Items[I].Tag;
      if Assigned(eTag.Editor) then
        eBuffer.AppendLineFmt(':%s:%s', [eTag.Name, eTag.Editor.Text]);
    end;

    eBuffer.Append('-}'#03);

    Result := eBuffer.ToString();
  finally
    eBuffer.Free();
  end;
end;

function TSwiftMessage.Valid(): Boolean;
var
  Block: TSwiftBlock;
  Validators: TSwiftValidatorClassArray;
begin
  Result := True;

  try
    for Block in FBlocks do
      Result := Block.Valid() and Result;

    // по умолчанию для всех TSwiftMandatoryFieldsValidator
    Result := Validate(gMessageDefaultValidator) and Result;

    // валидаторы для данного типа соообщений
    if gMessageValidator.TryGetValue(IntToStr(FMsgType), Validators) then
      Result := Validate(Validators) and Result;
  except
    on E: ESwiftValidatorException do raise;
    on E: Exception do
      Exception.RaiseOuterException(ESwiftValidatorException.Create(E.Message));
  end;
end;
{$ENDREGION}

{$Region 'TSwiftBlock1'}

constructor TSwiftBlock1.Create(aMessage: TSwiftMessage; const aText: string);
begin
  inherited Create(aMessage);
  FNumber := 1;
  FText := aText;
end;

function TSwiftBlock1.GetSender: string;
begin
  Result := Copy(FText, 7, 8);
  if not SameText(Copy(FText, 16, 3), 'XXX') then
    Result := Result + Copy(FText, 16, 3);
end;

function TSwiftBlock1.Valid: Boolean;
begin
  Result := not (Length(FText) <> (25));
  if not Result then
    Error(FLine, cInvalidBlockLength, [FNumber]);
end;
{$ENDREGION}

{$Region 'TSwiftBlock2'}

constructor TSwiftBlock2.Create(aMessage: TSwiftMessage; const aText: string);
begin
  inherited Create(aMessage);
  FNumber := 2;
  FText := aText;
end;

function TSwiftBlock2.GetReciever: string;
begin
  Result := Copy(FText, 5, 8);                          //8 -> 5
  if not SameText(Copy(FText, 13, 3), 'XXX') then       // 16 -> 13
    Result := Result + Copy(FText, 16, 3);
end;

function TSwiftBlock2.Valid: Boolean;
var
  Index: Integer;
begin
  Index := IndexText(Copy(FText, 1, 1), ['I', 'O']);
  case Index of
    0, 1: begin
      Result := not (((Index = 0) and (Length(FText) <> 17)) or ((Index = 1) and (Length(FText) <> 47)));
      if not Result then
        Error(FLine, cInvalidBlockLength, [FNumber]);
    end;
    else begin
      Result := False;
      Error(FLine, сInvalidBlockType, [FNumber]);
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwiftBlock3'}

constructor TSwiftBlock3.Create(aMessage: TSwiftMessage);
begin
  inherited;
  FNumber := 3;
end;
{$ENDREGION}

{$Region 'TSwiftBlock5'}

constructor TSwiftBlock5.Create(aMessage: TSwiftMessage);
begin
  inherited;
  FNumber := 5;
end;
{$ENDREGION}

{$Region 'TSwiftError'}

constructor TSwiftError.Create(const aText: string; aLevel, aLine,
  aColumn: Integer; aNumber: Integer);
begin
  Text := aText;
  Level := aLevel;
  Line := aLine;
  Column := aColumn;
  Number := aNumber;
end;

function TSwiftError.ToString: string;
begin
  if (Line = -1) or (Column = -1) then
    Result := Format('%s', [Text])
  else Result := Format('[%d:%d] %s', [Line, Column, Text]);
end;
{$ENDREGION}

{$Region 'TSwiftErrorList'}

function TSwiftErrorList.ToString: string;
var
  Error: TSwiftError;
begin
  Result := '';
  for Error in Self do
    Result := Result + Error.ToString() + sLineBreak;
end;
{$ENDREGION}

{$Region 'TSwiftMandatoryFieldsValidator'}

function TSwiftMandatoryFieldsValidator.Valid: Boolean;
var
  Fields: TSwiftMandatoryFieldArray;
  Field: TSwiftMandatoryField;
  IsValid: Boolean;
  eTagName, eError: string;
begin
  // обязательное поле в опциональном sequence является необязательным
  // поле с Field.Mandatory = 2 опциональное, т.е. (A|D|J)
  Result := True;

  if Assigned(FMessage.Block4) then
  begin
    Fields := TSwiftDatabase.GetMandatoryFields(FMessage.MsgType);
    for Field in Fields do begin
      if Field.Mandatory = 2 then
      begin
        if Contains(FMessage.MsgType, [518,541,543]) then begin
          eTagName := Field.Name;
          eError   := Format(сNoFindMandatoryTag, [ eTagName ]);
          IsValid := FMessage.Block4.ExistOptionTagBy(eTagName);
        end else begin
          eTagName := Field.NameWithoutColon;
          eError   := Format(сNoFindOptionMandatoryTag, [':' + eTagName + 'a' + ':']);
          IsValid := FMessage.Block4.ExistOptionTag(eTagName);
        end;

        if not IsValid then begin
          Result := False;
          Error(eError);
        end
      end else begin
        if Contains(FMessage.MsgType, [518,541,543]) then begin
          eTagName := Field.Name;
          IsValid := FMessage.Block4.ExistTagBy(eTagName);
        end else begin
          eTagName := Field.NameWithoutColon;
          IsValid := FMessage.Block4.ExistTag(eTagName);
        end;
        eError   := eTagName;

        if not IsValid then begin
          Result := False;
          Error(Format(сNoFindMandatoryTag, [eError]));
        end;
      end;
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwiftMessageValidator'}

constructor TSwiftMessageValidator.Create(aSubject: TObject);
begin
  FMessage := aSubject as TSwiftMessage;
end;

procedure TSwiftMessageValidator.Error(const aText: string);
begin
  FMessage.Errors.Add(TSwiftError.Create(aText, 2, -1, -1, FMessage.Errors.Count));
end;
{$ENDREGION}

{$Region 'TSwiftValidator'}

constructor TSwiftValidator.Create(aSubject: TObject);
begin

end;
{$ENDREGION}

{$Region 'TSwift79OrCopyValidator'}

function TSwift79OrCopyValidator.Valid: Boolean;
begin
  Result := True;
  if Assigned(FMessage.Block4) then
  begin
    Result := not (FMessage.Block4.ExistTag('79') and FMessage.Block4.ExistTag('15A'));
    if not Result then
      Error('Заполены поля :79: и Копия полей исходного сообщения. Одно из этих полей должно быть пустым');
  end;
end;
{$ENDREGION}

{$Region 'TSwiftErorrComparer'}

function TSwiftErorrComparer.Compare(const Left, Right: TSwiftError): Integer;
begin
  if (Left.Level in [0,1]) and (Right.Level in [0,1]) then
  begin
    Result := CompareValue(Left.Line, Right.Line);
    if Result = 0 then
    begin
      Result := CompareValue(Left.Level, Right.Level);
      if Result = 0 then
        Result := CompareValue(Left.Number, Right.Number);
    end;
  end
  else if (Left.Level = 2) and (Right.Level = 2) then
    Result := CompareValue(Left.Number, Right.Number)
  else
  begin
    if Left.Level = 2 then
      Result := 1
    else Result := -1;
  end;
end;
{$ENDREGION}

{$Region 'TSwiftFieldValidator'}

constructor TSwiftFieldValidator.Create(aSubject: TObject);
begin
  inherited;
  FField := aSubject as TSwiftField;
end;

procedure TSwiftFieldValidator.Error(const aText: string);
begin
  FField.Message.Errors.Add(
    TSwiftError.Create(aText, 1, FField.Tag.Line, 1, FField.Message.Errors.Count));
end;

function TSwiftFieldValidator.ValidateNumber(ANumPos: Integer;
  out AError: string): Boolean;
var
  eNumber: string;
begin
  eNumber := Copy(FField.Tag.Value, ANumPos);
  AError := cInvalidTagFormat;
  Result := Length(eNumber) < 16;

  if not Result then begin
    AError := AError + '.' + cLengthCheckError;
  end else begin
    Result := Pos(',', eNumber) > 1;
    if not Result then
      AError := AError + '.' + cCommaCheckError;
  end;
end;

{$ENDREGION}

{$Region 'TSwiftPatternValidator'}

function TSwiftPatternValidator.Valid: Boolean;
var
  MsgFormat: TSwiftMsgFormat;
  Errors: string;
begin
  Result := True;
  // проверка на недопустимы символы в поле
  // J.9245 - закомментировано для исключения дублирования сообщения об ошибке
  Result := not IsSWIFTPermissible(ReplaceText(FField.Tag.Value, sLineBreak, ''), Errors);
  if not Result then begin
    Error(Format(cInvalidTagSymbols, [FField.Tag.Name, Errors]));
    Exit;
  end;

  // есть теги, которые должны быть всегда пустыми
  if MatchText(FField.Tag.Name, cEmptyTags) then
    Exit;

  MsgFormat := TSwiftDatabase.GetMsgFormat(FField.Message.MsgType, FField);

  if (not MsgFormat.IsEmpty()) and (MsgFormat.Pattern <> '') then
    try
      Result := TRegEx.IsMatch(FField.Tag.Value, MsgFormat.Pattern, [roMultiLine]);
      if not Result then
        Error(Format(cInvalidTagFormat, [FField.Tag.Name, FField.Tag.Value, MsgFormat.Content]))
    except on E: Exception do begin
      Result := False;
      Error(Format(cRegExpError, [E.Message, MsgFormat.Pattern]));
      end;
    end;
end;
{$ENDREGION}

{$Region 'TSwiftCheckingForSlashesValidator'}

function TSwiftCheckingForSlashesValidator.Valid: Boolean;
begin
  Result := CheckingForSlashes(FField.Tag.Value, FField.Tag.Name) = '';
  if not Result then
    Error(Format(cNoSlashesTagValue, [FField.Tag.Name]));
end;
{$ENDREGION}

{$Region 'TSwiftFieldNameValidator'}

function TSwiftFieldNameValidator.Valid: Boolean;
begin
  // проверка корректности имени поля
  Result := False;
  if FField.Tag.Name = '' then
    Error(cEmptyTagName)
  else if not TRegEx.IsMatch(FField.Tag.Name, '^\d{2}[A-Z]?$', [roSingleLine]) then
    Error(Format(cInvalidTagName, [FField.Tag.Name]))
  else
    Result := True;
end;
{$ENDREGION}

{$Region 'TSwiftFieldValueValidator'}

procedure TSwiftFieldValueValidator.Error2(aLine: Integer; const aText: string);
begin
  FField.Message.Errors.Add(
    TSwiftError.Create(aText, 1, aLine, 1, FField.Message.Errors.Count));
end;

function TSwiftFieldValueValidator.Valid: Boolean;
var
  Errors: string;
  Lines: TStringDynArray;
  Index: Integer;
  Proc: TSwiftFieldProc;
begin
  // проверка корректности значения поля
  if MatchText(FField.Tag.Name, cEmptyTags) then
  begin
    Result := not (FField.Tag.Value <> '');
    if not Result then
      Error(Format(cNotEmptyTagValue, [FField.Tag.Name]));
  end
  else if FField.Tag.Value = '' then
  begin
    Result := False;
    Error(Format(cEmptyTagValue, [FField.Tag.Name]));
  end
  else
  begin
    Result := True;

    // проверка на длину строки в поле
    Lines := SplitString(AdjustLineBreaks(FField.Tag.Value), sLineBreak);

    // для 79 поля специальная обработка, должно быть 50 символов в строке
    if SameText(FField.Tag.Name, '79') then
    begin
      FField.Message.FieldsProc.TryGetValue('79', Proc);

      for Index := Low(Lines) to High(Lines) do
      begin
        if Length(Lines[Index]) > 50 then
        begin
          Result := False;
          Error2(FField.Tag.Line + Index, Format(cInvalidTagLenght, [FField.Tag.Name]));
        end
        else if Length(Lines[Index]) = 0 then
        begin
          Result := False;
          Error2(FField.Tag.Line + Index, Format(cInvalidTagEmpty, [FField.Tag.Name]));
        end;

        if Assigned(Proc) then
          Proc(FField);
      end;
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwift22AValidator'}

function TSwift22AValidator.Valid: Boolean;
begin
  // у сообщений типа MT 392, 395, 396 есть зависимость от MT 300
  // было замечено, что для 320 набор значений отличается
  case FField.Message.MsgType of
    300, 392, 395, 396: begin
      Result := MatchText(FField.Tag.Value, ['NEWT', 'EXOP', 'DUPL', 'CANC', 'AMND']);
      if not Result then
        Error('Поле должно содержать одно из следующих кодов "NEWT,EXOP,DUPL,CANC,AMND"');
    end;
    320, 330: begin
      Result := MatchText(FField.Tag.Value, ['NEWT', 'DUPL', 'CANC', 'AMND']);
      if not Result then
        Error('Поле должно содержать одно из следующих кодов "NEWT,DUPL,CANC,AMND"');
    end
    else
      // случай, если забыли реализовать прверку этого поля для нового сообщения
      raise ESwiftValidatorException.CreateFmt(
        'Проверка поля :22A: для сообщения типа MT%d не предусмотрена', [FField.Message.MsgType]);
  end;
end;
{$ENDREGION}

{$Region 'TSwift94AValidator'}

function TSwift94AValidator.Valid: Boolean;
begin
  Result := MatchText(FField.Tag.Value, ['AGNT', 'BILA', 'BROK']);
  if not Result then
    Error('Поле должно содержать одно из следующих кодов "AGNT,BILA,BROK"');
end;
{$ENDREGION}

{$Region 'TSwift22CValidator'}

function TSwift22CValidator.Valid: Boolean;
begin
  Result := Copy(FField.Tag.Value, 1, 4) <= Copy(FField.Tag.Value, 11, 4);
  if not Result then
    Error('Коды должны располагаться в алфавитном порядке');
end;
{$ENDREGION}

{$Region 'TSwift17TValidator'}

function TSwift17TValidator.Valid: Boolean;
begin
  Result := MatchText(FField.Tag.Value, ['N', 'Y']);
  if not Result then
    Error('Поле должно содержать одно из следующих кодов "Y,N"');
end;
{$ENDREGION}

{$Region 'TSwift82AValidator'}

function TSwift82AValidator.Valid: Boolean;
var
  Value: string;
begin
  Result := True;

  if ((FField.FMessage.FMsgType = 300) or (FField.FMessage.FMsgType = 320) ) and (Copy(FField.Tag.Value, 1, 1) <> '/') then
  begin
    Value := '';

    Result := FField.Tag.Value = Value;
    if not Result then
      Error(Format('''%s'' не соответствует значению SWIFT-кода банка ''%s''',
        [ReplaceText(FField.Tag.Value, sLineBreak, ' '), Value]));
  end;
end;
{$ENDREGION}

{$Region 'TSwift56AValidator'}

function TSwift56AValidator.Valid: Boolean;
var
  Lines: TStringDynArray;
begin
  Result := True;

  if Copy(FField.Tag.Value, 1, 1) <> '/' then
  begin
    Result := ValidateSWIFT(FField.Tag.Value);
    if not Result then
      Error(Format('''%s'' неверный SWIFT-код (не найден в справочнике банков)',
        [ReplaceText(FField.Tag.Value, sLineBreak, ' ')]));
  end
  else
  begin
    Lines := SplitString(AdjustLineBreaks(FField.Tag.Value), sLineBreak);
    if Length(Lines) > 1 then
    begin
      Result := ValidateSWIFT(Lines[1]);
      if not Result then
        Error(Format('''%s'' неверный SWIFT-код (не найден в справочнике банков)',
          [Lines[1]]));
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwift30TValidator'}

function TSwift30TValidator.Valid: Boolean;
begin
  Result := DateUtils.IsValidDate(
    StrToIntDef(Copy(FField.Tag.Value, 1, 4), 0),
    StrToIntDef(Copy(FField.Tag.Value, 5, 2), 0),
    StrToIntDef(Copy(FField.Tag.Value, 7, 2), 0));
  if not Result then
    Error(Format('''%s'' дата не соответствует формату YYYYMMDD)', [FField.Tag.Value]));
end;
{$ENDREGION}

{$Region 'TSwift36Validator'}

function TSwift36Validator.Valid: Boolean;
begin
  Result := Pos('.', FField.Tag.Value) = 0;
  if not Result then
    Error('Неверный разделитель дробной части "."');
end;
{$ENDREGION}

{$Region 'TSwift58AValidator'}

function TSwift58AValidator.Valid: Boolean;
begin
  Result := True;
  if Copy(FField.Tag.Value, 1, 1) <> '/' then
  begin
    Result := ValidateSWIFT(FField.Tag.Value);
    if not Result then
      Error(Format('''%s'' неверный SWIFT-код (не найден в справочнике банков)',
        [ReplaceText(FField.Tag.Value, sLineBreak, ' ')]));
  end;
end;
{$ENDREGION}

{$Region 'TSwift22AAnd21Validator'}

function TSwift22AAnd21Validator.Valid: Boolean;
var
  Field: TSwiftField;
begin
  Result := True;
  if Assigned(FMessage.Block4) then
  begin
    Field := FMessage.Block4.GetFieldBy('22A');
    if Assigned(Field) then
    begin
      if MatchText(Field.Tag.Value, ['AMND', 'CANC']) then
        Result := FMessage.Block4.ExistTag('21');

      if not Result then
        Error('Правило С1 - отсутствует обязательное поле :21:');
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwift22CAnd82AValidator'}

function TSwift22CAnd82AValidator.Valid: Boolean;
var
  Field22C, Field82A: TSwiftField;
  Value1, Value2: string;
begin
  Result := True;

  if Assigned(FMessage.Block4) then
  begin
    Field22C := FMessage.Block4.GetFieldBy('22C');
    Field82A := FMessage.Block4.GetFieldBy('82A');

    if Assigned(Field22C) and Assigned(Field82A) and (Copy(Field82A.Tag.Value, 1, 1) <> '/') then
    begin
      Value1 := Copy(Field22C.Tag.Value, 1, 4);
      Value2 := Copy(Field22C.Tag.Value, 11, 4);

      Result := (Pos(Value1, Field82A.Tag.Value) <> 0) or (Pos(Value2, Field82A.Tag.Value) <> 0);
      if not Result then
        Error('В поле :22С: ' + Format('''%s'',''%s'' коды не соответствуют SWIFT коду поля :82A:',[Value1, Value2]));
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwift22CAnd87AValidator'}

function TSwift22CAnd87AValidator.Valid: Boolean;
var
  Field22C, Field87A: TSwiftField;
  Value1, Value2: string;
begin
  Result := True;

  if Assigned(FMessage.Block4) then
  begin
    Field22C := FMessage.Block4.GetFieldBy('22C');
    Field87A := FMessage.Block4.GetFieldBy('87A');

    if Assigned(Field22C) and Assigned(Field87A) and (Copy(Field87A.Tag.Value, 1, 1) <> '/') then
    begin
      Value1 := Copy(Field22C.Tag.Value, 1, 4);
      Value2 := Copy(Field22C.Tag.Value, 11, 4);

      Result := (Pos(Value1, Field87A.Tag.Value) <> 0) or (Pos(Value2, Field87A.Tag.Value) <> 0);
      if not Result then
        Error('В поле :22С: ' + Format('''%s'',''%s'' коды не соответствуют SWIFT коду поля :87A:',[Value1, Value2]));
    end;
  end;
end;
{$ENDREGION}

{$Region 'TMandatoryField'}

constructor TSwiftMandatoryField.Create(aName: string; aMandatory: Integer);
begin
  Name := aName;
  Mandatory := aMandatory;
end;
{$ENDREGION}

{$Region 'TSwift30Validator'}

function TSwift30Validator.Valid: Boolean;
begin
  Result := DateUtils.IsValidDate(
    StrToIntDef(Copy(FField.Tag.Value, 1, 2), 0),
    StrToIntDef(Copy(FField.Tag.Value, 3, 2), 0),
    StrToIntDef(Copy(FField.Tag.Value, 5, 2), 0));
  if not Result then
    Error(Format('''%s'' дата не соответствует формату YYMMDD)', [FField.Tag.Value]));
end;
{$ENDREGION}

{$Region 'TSwiftValidateElement'}

function TSwiftValidateElement.Validate(aValidators: TSwiftValidatorClassArray): Boolean;
var
  Validator: TSwiftValidatorClass;
begin
  Result := True;

  try
    for Validator in aValidators do
      with Validator.Create(Self) do
        try
          Result := Valid() and Result;
        finally
          Free();
        end;
  except
    on E: ESwiftValidatorException do raise;
    on E: Exception do
      Exception.RaiseOuterException(ESwiftValidatorException.Create(E.Message));
  end;
end;
{$ENDREGION}

{$Region 'TSwift22ABAnd21Validator'}

function TSwift22ABAnd21Validator.Valid: Boolean;
var
  Field22A, Field22B: TSwiftField;
  Flag22A1, Flag22A2, Flag22B1, Flag22B2: Boolean; // по умолчанию: False
begin
  Result := True;
  if Assigned(FMessage.Block4) then
  begin
    Field22A := FMessage.Block4.GetFieldBy('22A');
    Field22B := FMessage.Block4.GetFieldBy('22B');

    if Assigned(Field22A) and Assigned(Field22B) then
    begin
      Flag22A1 := (not SameText(Field22A.Tag.Value, '')) and (not SameText(Field22A.Tag.Value, 'NEWT'));
      Flag22A2 := not SameText(Field22A.Tag.Value, '');
      Flag22B1 := SameText(Field22B.Tag.Value, 'CONF');
      Flag22B2 := (not SameText(Field22B.Tag.Value, '')) and (not SameText(Field22B.Tag.Value, 'CONF'));

      if (Flag22B1 and Flag22A1) or (Flag22B2 and Flag22A2) then
        Result := FMessage.Block4.ExistTag('21');

      if not Result then
        Error('Правило С1 - отсутствует обязательное поле :21:');
    end;
  end;
end;
{$ENDREGION}

{$Region 'TSwiftValidatorRegistry'}

function TSwiftValidatorRegistry.Add(const aKey: string;
  aValue: TSwiftValidatorClassArray): TSwiftValidatorRegistry;
begin
  inherited Add(aKey, aValue);
  Result := Self;
end;

function TSwiftMandatoryField.GetNameWithoutColon: string;
begin
  Result := ReplaceText(Name, ':', '');
end;
{$ENDREGION}

{$Region 'TSwiftMsgArchive'}

constructor TSwiftMsgArchive.Create(aID, aDirection: Integer; aReference,
  aSender, aReciever: string; const aText: string);
begin
  ID := aID;
  Direction := aDirection;
  Reference := aReference;
  Sender := aSender;
  Reciever := aReciever;
  Text := aText;
end;
{$ENDREGION}

{$Region 'TSwift32BAnd33BValidator'}

function TSwiftSQLQueryValidator.CheckElement(const aRule,
  aValue: string): Boolean;
begin
  Result := True
end;

class function TSwiftSQLQueryValidator.LoadXMLTemplate(aMsgTypeID: Integer): IXMLDocument;
const
  cnSQLText = 'select * from SWIFTMSGTYPESREF SMTR where SMTR.MSGTYPEID = :MSGTYPEID';
begin
  Result := nil;
end;

function TSwiftSQLQueryValidator.TryGetSwiftTag(const aSequence, aTag: string;
  aOption: Boolean; out aSwiftTag: TSwiftTag): Boolean;
var
  Sequence: TSwiftTag;
begin
  if aSequence = '' then
    aSwiftTag := FMessage.Block4.GetTagBy(aTag, aOption)
  else
  begin
    Sequence := FMessage.Block4.GetSequence(aSequence);
    if Assigned(Sequence) then
      aSwiftTag := Sequence.GetTagBy(aTag, aOption)
    else aSwiftTag := nil;
  end;

  Result := Assigned(aSwiftTag);
end;

function TSwiftSQLQueryValidator.Valid: Boolean;
var
  XMLTemplate: IXMLDocument;
  Rules: IXMLNode;
  Sequences: IXMLNodeList;
  Output: Boolean;
begin
  Result := True;
  Output := Result;

  if Assigned(FMessage.Block4) then
  begin
    XMLTemplate := LoadXMLTemplate(FMessage.MsgType);
    if not Assigned(XMLTemplate) then
      Exit;

    // получаем ссылку на правили swift-шаблона
    Rules := TXMLDocumentHelper.SelectSingleNode(XMLTemplate.DocumentElement,
      '/message/system/rules');

    Sequences := TXMLDocumentHelper.SelectNodes(XMLTemplate.DocumentElement,
      '/message/text-block/sequence');

    TXMLDocumentHelper.ForEach(
      Sequences,
      procedure(aSequence: IXMLNode)
      var
        Tags: IXMLNodeList;
      begin
        // получаем все теги с правилом проверки "sqlquery"
        Tags := TXMLDocumentHelper.SelectNodes(XMLTemplate.DocumentElement,
          Format('/message/text-block/sequence[@shortname="%s"]//tag[@shortname!="" and option/element/rule/@type="sqlquery"]',
            [aSequence.Attributes['shortname']]));

        TXMLDocumentHelper.ForEach(
          Tags,
          procedure(aTag: IXMLNode)
          var
            Element: IDOMNode;
            Name, Option, Value, Temp: string;
            SwiftTag: TSwiftTag;
            Checked: Boolean;
            Data: TStringDynArray;
            Rule: IXMLNode;
          begin
            // имя последовательности
            Temp := IfThen(
              (not aSequence.HasAttribute('shortname')) or (aSequence.Attributes['shortname'] = ''),
              '', '15' + aSequence.Attributes['shortname']);

            if TryGetSwiftTag(Temp, aTag.Attributes['shortname'], True, SwiftTag) then
            begin
              Name := SwiftTag.Name;
              Option := Copy(SwiftTag.Name, 3, 1);
              Value := '';

              // проверяем значение тега по правилу из xml
              // если без имени, то берем правило из тега rule в element
              // иначе в справочнике ищем по типу и имени
              Rule := TXMLDocumentHelper.SelectSingleNode(aTag,
                Format('option[@name="%0:s" or ("%0:s"="" and not(@name))]/element/rule[@type="sqlquery"]', [Option]));
              if Assigned(Rule) then
              begin
                // разбитие значения тега на составляющие
                Element := Rule.DOMNode.ParentNode;
                if SameText(Element.attributes.getNamedItem('name').nodeValue, 'Currency') then
                  Value := Copy(SwiftTag.Value, 1, 3)
                else if SameText(Element.attributes.getNamedItem('name').nodeValue, 'BIC') then
                begin
                  Data := SplitString(SwiftTag.Value, sLineBreak);
                  if (Length(Data) > 1)  then
                  begin
                   if Data[0][1] = '/' then
                    Value := Copy(Data[1], 1, 11)
                   else Value := Copy(Data[0], 1, 11);
                  end else
                    Value := Copy(Data[0], 1, 11);
                end else
                  Value := '';

                // праверка на соответствие строки правилу
                if not Rule.HasAttribute('name') then
                  Checked := CheckElement(Rule.NodeValue, Value)
                else
                begin
                  Rule := TXMLDocumentHelper.SelectSingleNode(Rules,
                    Format('rule[@name="%s" and @type="sqlquery"]', [Rule.Attributes['name']]));
                  Checked := CheckElement(Rule.NodeValue, Value);
                end;

                Output := Output and Checked;

                if not Checked then
                  Error(Format('Поле :%s: ' + Rule.Attributes['message'], [Name, Value]));
              end;
            end;
          end);
      end);
  end;

  Result := Output;
end;
{$ENDREGION}

{$Region 'TXMLDocumentHelper'}

class procedure TXMLDocumentHelper.ForEach(aNodes: IXMLNodeList;
  aProc: TProc<IXMLNode>);
var
  Index: Integer;
begin
  for Index := 0 to aNodes.Count - 1 do
    aProc(aNodes[Index]);
end;

class function TXMLDocumentHelper.SelectNodes(aNode: IXMLNode;
  const aPath: string): IXMLNodeList;
var
  intfSelect : IDomNodeSelect;
  intfAccess : IXmlNodeAccess;
  dnlResult  : IDomNodeList;
  intfDocAccess : IXmlDocumentAccess;
  doc: TXmlDocument;
  i : Integer;
  dn : IDomNode;
begin
  Result := nil;
  if not Assigned(aNode)
    or not Supports(aNode, IXmlNodeAccess, intfAccess)
    or not Supports(aNode.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;

  dnlResult := intfSelect.selectNodes(aPath);
  if Assigned(dnlResult) then
  begin
    Result := TXmlNodeList.Create(intfAccess.GetNodeObject, '', nil);
    if Supports(aNode.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
      doc := intfDocAccess.DocumentObject
    else
      doc := nil;

    for i := 0 to dnlResult.length - 1 do
    begin
      dn := dnlResult.item[i];
      Result.Add(TXmlNode.Create(dn, nil, doc));
    end;
  end;
end;

class function TXMLDocumentHelper.SelectSingleNode(aNode: IXMLNode;
  const aPath: string): IXMLNode;
var
  intfSelect : IDOMNodeSelect;
  intfResult  : IDOMNode;
  intfDocumentAccess : IXMLDocumentAccess;
  Document: TXmlDocument;
begin
  Result := nil;
  if not Assigned(aNode)
    or not Supports(aNode.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;

  intfResult := intfSelect.selectNode(aPath);
  if Assigned(intfResult) then
  begin
    if Supports(aNode.OwnerDocument, IXmlDocumentAccess, intfDocumentAccess) then
      Document := intfDocumentAccess.DocumentObject
    else Document := nil;

    Result := TXMLNode.Create(intfResult, nil, Document);
  end;
end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////

function CheckSWIFTMsgFormat(const aMsgText: string; aMsgType: Integer = 0): string;
var
  iMsgType: Integer;
  iTmp: string;
begin
  if ((pos('{2:I', aMsgText) > 0) and
      (length(aMsgText) >= pos('{2:I', aMsgText) + 7)) then
    if (aMsgType = 0) then
      iMsgType := strtoint(copy(aMsgText, pos('{2:I', aMsgText) + 4, 3))          // тип сообщения определяем по 2 блоку
    else
      iMsgType := aMsgType
  else
    iMsgType := aMsgType;

  TSwift.MsgFormatValid(iMsgType, trim(aMsgText), Result);
end;

////////////////////////////////////////////////////////////////////////////////
{ TSwiftStructureValidate }

function TSwiftStructureValidate.Valid: Boolean;
var
  StrBlock4: string;
  eBlock4: TSwiftBlock4;
  eTmpTag: TSwiftTag;

  i: Integer;
  s: string;

  function FindBlockStart( aSeqName: string; aFrom: Integer ): Boolean;
  var
    I: Integer;
    eTag: TSwiftTag;
  begin
    Result := False;
    for I := aFrom downto 0 do begin
      eTag := eBlock4.GetFieldList.Items[ I ].Tag;
      if (SameText(eTag.Name, '16R')) and
         (SameText(eTag.Value, aSeqName)) then begin
        Result := True;
        Break;
      end;
    end;
  end;

  function FindBlockEnd( aSeqName: string; aFrom: Integer ): Boolean;
  var
    I: Integer;
    eTag: TSwiftTag;
  begin
    Result := False;
    for I := aFrom to eBlock4.GetFieldList.Count - 1 do begin
      eTag := eBlock4.GetFieldList.Items[ I ].Tag;
      if (SameText(eTag.Name, '16S')) and
         (SameText(eTag.Value, aSeqName)) then begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  // проверка структуры
  Result  := True;
  eBlock4 := FMessage.Block4;
  if Assigned(eBlock4) then
  begin
    StrBlock4 := eBlock4.Text;

    for I := 0 to eBlock4.GetFieldList.Count - 1 do begin
      eTmpTag := eBlock4.GetFieldList.Items[ I ].Tag;
      if (SameText(eTmpTag.Name, '16R')) then begin
        if not FindBlockEnd(eTmpTag.Value, I) then begin
          Result := False;
          s := s + Format('[%d:1] Блок %s не закрыт'#13, [eTmpTag.Line, eTmpTag.Value]);
        end;
      end;
    end;

    for I := eBlock4.GetFieldList.Count - 1 downto 0 do begin
      eTmpTag := eBlock4.GetFieldList.Items[ I ].Tag;
      if (SameText(eTmpTag.Name, '16S')) then begin
        if not FindBlockStart(eTmpTag.Value, I) then begin
          Result := False;
          s := s + Format('[%d:1] Блок %s не открыт'#13, [eTmpTag.Line, eTmpTag.Value]);
        end;
      end;
    end;

    if not Result then
      Error(s);
  end;
end;

{ TSwit22FValidator }

function TSwit22FValidator.Valid: Boolean;
var
  eReceiver: string;
begin
  // проверка получателя
  Result := True;
  eReceiver := FMessage.Block2.GetReciever;
  if SameText(eReceiver, 'MGTCBEBE') then begin
    // проверка наличия поля SETDET.22F.RTGS
    Result := FMessage.Block4.ExistTagBy('SETDET.22F.RTGS');
    if not Result then begin
      Error(Format(сNoFindMandatoryTag, ['SETDET.22F.RTGS']));
    end;
  end;
end;

{ TSwift35BValidator }

function TSwift35BValidator.Valid: Boolean;
const
  cPatternSubField1 = '\A(ISIN [A-Z0-9]{12})\Z';
  cPatternSubField2 = '\A(^([\w\-:\(\)\.,''\+\?\/ ]{1,35}(\r\n)?)){1,4}\Z';
var
  TextStrings: TStringList;
  SubField1, SubField2: string;
begin
  // проверка формата поля 35B [ISIN1!e12!c] [4*35x]
  Result := True;
  TextStrings := TStringList.Create;
  try
    TextStrings.Text := FField.Tag.Value;
    if TextStrings.Count = 0 then Exit;

    SubField1 := TextStrings[ 0 ];
    if Pos('ISIN', SubField1) > 0 then begin
      TextStrings.Delete(0);
      SubField2 := TextStrings.Text;
      Result := TRegEx.IsMatch(SubField1, cPatternSubField1);
    end else begin
      SubField2 := TextStrings.Text;
    end;
    if Result then
      Result := TRegEx.IsMatch(SubField2, cPatternSubField2, [roMultiLine]);

    if not Result then
      Error(Format(cInvalidTagFormat, [FField.Tag.Name, FField.Tag.Value, '[ISIN1!e12!c] [4*35x]']))
  finally
    TextStrings.Free;
  end;
end;

{ TSwift90AValidator }

function TSwift90AValidator.Valid: Boolean;
var
  eError: string;
begin
  // 4!c//4!c/15d
  Result := ValidateNumber(13, eError);
  if not Result then
    Error(Format(eError, [FField.Tag.Name, FField.Tag.Value, '[4!c//4!c/15d]']));
end;

{ TSwift90BValidator }

function TSwift90BValidator.Valid: Boolean;
var
  eError: string;
begin
  // 4!c//4!c/3!a15d
  Result := ValidateNumber(16, eError);
  if not Result then
    Error(Format(eError, [FField.Tag.Name, FField.Tag.Value, '[4!c//4!c/3!a15d]']));
end;

{ TSwift19AValidator }

function TSwift19AValidator.Valid: Boolean;
var
  eStr, eError: string;
begin
  // 4!c//[N]3!a15d
  eStr := FField.Tag.Value;
  if eStr[ 8 ] = 'N' then
    Result := ValidateNumber(12, eError)
  else
    Result := ValidateNumber(11, eError);
  if not Result then
    Error(Format(eError, [FField.Tag.Name, FField.Tag.Value, '[4!c//[N]3!a15d]']));
end;

{ TSwift92AValidator }

function TSwift92AValidator.Valid: Boolean;
var
  eStr, eError: string;
begin
  // 4!c//[N]15d
  eStr := FField.Tag.Value;
  if eStr[ 8 ] = 'N' then
    Result := ValidateNumber(9, eError)
  else
    Result := ValidateNumber(8, eError);
  if not Result then
    Error(Format(eError, [FField.Tag.Name, FField.Tag.Value, '[4!c//[N]15d]']));
end;

initialization
////////////////////////////////////////////////////////////////////////////////

  // по умолчанию для всех тегов: TSwiftFieldNameValidator, TSwiftFieldValueValidator,
  // TSwiftPatternValidator
  gFieldDefaultValidator := TSwiftValidatorClassArray.Create(
    TSwiftFieldNameValidator, TSwiftFieldValueValidator, TSwiftPatternValidator);

  gFieldValidator := TSwiftValidatorRegistry.Create();
  gFieldValidator
    .Add('17T', TSwiftValidatorClassArray.Create(TSwift17TValidator))
    .Add('17U', TSwiftValidatorClassArray.Create(TSwift17UValidator))
    .Add('20', TSwiftValidatorClassArray.Create(TSwiftCheckingForSlashesValidator))
    .Add('20C', TSwiftValidatorClassArray.Create(TSwiftCheckingForSlashesValidator))
    .Add('21', TSwiftValidatorClassArray.Create(TSwiftCheckingForSlashesValidator))
    .Add('22A', TSwiftValidatorClassArray.Create(TSwift22AValidator))
    .Add('22C', TSwiftValidatorClassArray.Create(TSwift22CValidator))
    .Add('30', TSwiftValidatorClassArray.Create(TSwift30Validator))
    .Add('30T', TSwiftValidatorClassArray.Create(TSwift30TValidator))
    .Add('30V', TSwiftValidatorClassArray.Create(TSwift30VValidator))
    .Add('32B', TSwiftValidatorClassArray.Create(TSwift32BValidator))
    .Add('33B', TSwiftValidatorClassArray.Create(TSwift33BValidator))
    .Add('36', TSwiftValidatorClassArray.Create(TSwift36Validator))
    .Add('56A', TSwiftValidatorClassArray.Create(TSwift56AValidator))
    .Add('57A', TSwiftValidatorClassArray.Create(TSwift57AValidator))
    .Add('58A', TSwiftValidatorClassArray.Create(TSwift58AValidator))
    .Add('82A', TSwiftValidatorClassArray.Create(TSwift82AValidator))
    .Add('87A', TSwiftValidatorClassArray.Create(TSwift87AValidator))
    .Add('94A', TSwiftValidatorClassArray.Create(TSwift94AValidator))
    .Add('35B', TSwiftValidatorClassArray.Create(TSwift35BValidator))
    .Add('90A', TSwiftValidatorClassArray.Create(TSwift90AValidator))
    .Add('90B', TSwiftValidatorClassArray.Create(TSwift90BValidator))
    .Add('36B', TSwiftValidatorClassArray.Create(TSwift90AValidator))
    .Add('19A', TSwiftValidatorClassArray.Create(TSwift19AValidator))
    .Add('92A', TSwiftValidatorClassArray.Create(TSwift92AValidator));
  // по умолчанию для всех сообщений: TSwiftMandatoryFieldsValidator
  gMessageDefaultValidator := TSwiftValidatorClassArray.Create(
    TSwiftMandatoryFieldsValidator);

  gMessageValidator := TSwiftValidatorRegistry.Create();
  gMessageValidator
    .Add('300', TSwiftValidatorClassArray.Create(
      TSwift22AAnd21Validator, TSwift22CAnd82AValidator, TSwift22CAnd87AValidator,
      TSwiftSQLQueryValidator))
    .Add('320', TSwiftValidatorClassArray.Create(
      TSwift22ABAnd21Validator, TSwift22CAnd82AValidator, TSwift22CAnd87AValidator))
    .Add('392', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('395', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('396', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('592', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('595', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('596', TSwiftValidatorClassArray.Create(TSwift79OrCopyValidator))
    .Add('518', TSwiftValidatorClassArray.Create(TSwiftStructureValidate))
    .Add('541', TSwiftValidatorClassArray.Create(TSwiftStructureValidate, TSwit22FValidator))
    .Add('543', TSwiftValidatorClassArray.Create(TSwiftStructureValidate, TSwit22FValidator)) ;

finalization
  gFieldValidator.Free();
  gMessageValidator.Free();

end.



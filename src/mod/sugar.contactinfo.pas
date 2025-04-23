unit sugar.contactInfo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpJSON, fgl, sugar.gender;
const
  JSON_CONTACT_ID  = 'sugar_class'; // Name of the object field that can be interrogated to find out the class name

type
    NContactStatus = (
        cinfoUnknown,
        cinfoActive    = 20,
        cinfoFrozen    = 40,
        cinfoArchived  = 60,
        cinfoInvisible = 80,
        cinfoDeleted   = 200
    );

    TContact = class;
	{ TContactInfo }

    TContactInfo = class(TJSONObject)
	private
        myChangeCount : QWord;
        myBelongsTo: TContact; // Only stores the reference
		function getEnabled: boolean;
		function getChanged: boolean;
		function getcreatedOn: TDateTime;
		function getID: string;
		function getNotes: string;
		function getstatus: NContactStatus;
		function getupdatedOn: TDateTime;
		procedure setBelongsTo(const _value: TContact);
		procedure setcreatedOn(const _value: TDateTime);
		procedure setEnabled(const _value: boolean);
		procedure setID(const _value: string);
		procedure setNotes(const _value: string);
		procedure setstatus(const _value: NContactStatus);
		procedure setupdatedOn(const _value: TDateTime);

    protected
        property BelongsTo : TContact read myBelongsTo write setBelongsTo;
        procedure doUpdate; virtual;
        procedure resetChanges;

	published
        property ID        : string read getID write setID;                             {Entity ID}
        property notes     : string read getNotes write setNotes;
        property createdOn : TDateTime read getcreatedOn write setcreatedOn;
        property updatedOn : TDateTime read getupdatedOn write setupdatedOn;
        property enabled    : boolean read getEnabled write setEnabled;
        property status    : NContactStatus read getstatus write setstatus;
        property hasChanged: boolean read getChanged;

    public
        constructor Create; virtual;

	end;

    TContact = class(TContactInfo)

	end;

	{ TContactPhone }

    TContactPhone = class(TContactInfo)
	private
		function getIntPhoneNumber: int64;
		function getMask: string;
        function getPhoneNo: string;
        function getcountryCode: integer;
        function getareaCode: integer;
		function getnumber: int64;
        procedure setmask(const _value: string);
        procedure setcountryCode(const _value: integer);
        procedure setareaCode(const _value: integer);
		procedure setnumber(const _value: int64);

	published
        property BelongsTo;
        property mask: string read getMask write setmask;    // determines the input/output format of the number. Currently not implemented
        property sPhoneNo: string read getPhoneNo;           // Formatted phone number
        property intPhoneNo: int64 read getIntPhoneNumber; // phone number as an integer
        property countryCode: integer read getcountryCode write setcountryCode;
        property areaCode: integer read getareaCode write setareaCode;
        property number: int64 read getnumber write setnumber;

    public
        constructor Create; override;

	end;

	{ TContactEmail }

    TContactEmail = class(TContactInfo)
	private
		function getEmail: string;
		procedure setEmail(const _value: string);
	published
        property BelongsTo;
        property Email: string read getEmail write setEmail;
    public
        constructor Create; override;
        function isValid: boolean;

	end;

	{ TContactAddress }

    TContactAddress = class(TContactInfo)
	private
		function getCity: string;
		function getCountry: string;
		function GetLine1: string;
		function getLine2: string;
		function getPostCode: string;
		function getRegion: string;
		function getState: string;

		procedure setCity(const _value: string);
		procedure setCountry(const _value: string);
		procedure SetLine1(const _value: string);
		procedure setLine2(const _value: string);
		procedure setPostCode(const _value: string);
		procedure setRegion(const _value: string);
		procedure setState(const _value: string);
	published
        property BelongsTo;
        property Line1:    string read GetLine1 write SetLine1;
        property Line2:    string read getLine2 write setLine2;
        property City:     string read getCity write setCity;
        property Region:   string read getRegion write setRegion;
        property State:    string read getState write setState;
        property Country:  string read getCountry write setCountry;
        property PostCode: string read getPostCode write setPostCode;
    public
        constructor Create; override;
	end;

    EDuplicateContactKey = class(Exception); // There is already a contact object with this key

	{ TContactEmailList }

    TContactEmailList = class(TJSONObject)
	private
		mybelongsTo: TContact;
		mydefault: TContactEmail;
		function getemail(_key: string): TContactEmail;
		procedure setbelongsTo(const _value: TContact);
		procedure setdefault(const _value: TContactEmail);
		procedure setEmail(_key: string; const _value: TContactEmail);
	public
        property belongsTo: TContact read mybelongsTo write setbelongsTo;
        property default: TContactEmail read mydefault write setdefault; // The first added value is the default.
        property named[_key: string] : TContactEmail read getemail write setEmail;

        function keys: TStringArray; // Note this function is duplicated (simpler because these are specialized classes, abstracting is more complex)
        constructor Create;

	end;

	{ TContactPhoneList }

    TContactPhoneList = class(TJSONObject)
	private
		mybelongsTo: TContact;
		mydefault: TContactPhone;
		function getNamed(_key: string): TContactPhone;
		procedure setbelongsTo(const _value: TContact);
		procedure setdefault(const _value: TContactPhone);
		procedure setNamed(_key: string; const _value: TContactPhone);
	public
        property belongsTo: TContact read mybelongsTo write setbelongsTo;
        property default: TContactPhone read mydefault write setdefault;
        property named [_key: string]: TContactPhone read getNamed write setNamed;
        function remove(_key: string): boolean;
        function keys: TStringArray;
        constructor Create;

	end;

	{ TContactAddressList }

    TContactAddressList = class(TJSONObject)
	private
		mybelongsTo: TContact;
		mydefault: TContactAddress;
		function getNamed(_key: string): TContactAddress;
		procedure setNamed(_key: string; const _value: TContactAddress);
		procedure setbelongsTo(const _value: TContact);
		procedure setdefault(const _value: TContactAddress);
	public
        property belongsTo: TContact read mybelongsTo write setbelongsTo;
        property default: TContactAddress read mydefault write setdefault;
        property named[_key: string]: TContactAddress read getNamed write setNamed;
        function keys: TStringArray;
	end;

	{ TContactPersonBasic }

    TContactPersonBasic = class(TContact)
	private
		function getEmails: TContactEmailList;
		function getName: string;
		function getPhones: TContactPhoneList;
		procedure setEmails(const _value: TContactEmailList);
		procedure setName(const _value: string);
		procedure setPhones(const _value: TContactPhoneList);
	published
        property Name: string read getName write setName;
        property Emails : TContactEmailList read getEmails write setEmails;
        property Phones : TContactPhoneList read getPhones write setPhones;
    public
        constructor Create; override;
        destructor Destroy; override;
	end;

	{ TContactPerson }

    TContactPerson = class(TContactPersonBasic)
	private
		function getAddresses: TContactAddressList;
		function getDOB: TDateTime;
		function getGender: string;

		procedure setDOB(const _value: TDateTime);
		procedure setGender(const _value: string);

	published
        property Name;
        property Emails;
        property Phones;
        property DOB : TDateTime read getDOB write setDOB;
        property Gender : string read getGender write setGender;
        property Addresses : TContactAddressList read getAddresses;

    public
        constructor Create; override;
        destructor Destroy; override;
    end;


implementation


{ TContactInfo }

function TContactInfo.getEnabled: boolean;
begin
    Result := booleans['enabled'];
end;

function TContactInfo.getChanged: boolean;
begin
    Result := myChangeCount > 0;
end;


function TContactInfo.getcreatedOn: TDateTime;
begin
    Result := TDateTime(floats['createdOn']);
end;

function TContactInfo.getID: string;
begin
    Result := strings['ID'];
end;

function TContactInfo.getNotes: string;
begin
    Result := strings['notes'];
end;

function TContactInfo.getstatus: NContactStatus;
begin
    Result := NContactStatus(Integers['status']);
end;

function TContactInfo.getupdatedOn: TDateTime;
begin
    Result := TDateTime(floats['updatedOn']);
end;


procedure TContactInfo.setBelongsTo(const _value: TContact);
begin
    myBelongsTo := _value;
end;

procedure TContactInfo.setcreatedOn(const _value: TDateTime);
begin
    floats['createdOn'] := _value;
    doUpdate;
end;

procedure TContactInfo.setEnabled(const _value: boolean);
begin
    Booleans['enabled'] := _value;
end;

procedure TContactInfo.setID(const _value: string);
begin
    strings['ID'] := _value;
    doUpdate;
end;

procedure TContactInfo.setNotes(const _value: string);
begin
    strings['notes'] := _value;
    doUpdate
end;

procedure TContactInfo.setstatus(const _value: NContactStatus);
begin
    integers['status'] := ord(_value);
end;

procedure TContactInfo.setupdatedOn(const _value: TDateTime);
begin
    floats['updatedOn'] := _value;
    {DO NOT CALL doUpdate() here. This will trigger an endless recursive loop}
end;

procedure TContactInfo.doUpdate;
begin
    updatedOn := Now;
    Inc(myChangeCount);
end;

procedure TContactInfo.resetChanges;
begin
    myChangeCount := 0;
end;

constructor TContactInfo.Create;
begin
	inherited Create;
    {init members}
    myChangeCount         := 0;
    Strings [JSON_CONTACT_ID] := ClassName ; // Current classname
    strings ['ID']        := '';
    strings ['notes']     := '';
    floats  ['createdOn'] := 0;
    floats  ['updatedOn'] := 0;
    booleans['enabled']   := true;
    integers['status']    := ord(cinfoActive);
    myBelongsTo           := nil;
end;

{ TContactPhone }

function TContactPhone.getareaCode: integer;
begin
    Result := integers['areaCode'];
end;

function TContactPhone.getcountryCode: integer;
begin
    Result := integers['countryCode'];
end;

function TContactPhone.getIntPhoneNumber: int64;
var
    _s: string = '';
begin
    if countryCode >0 then
        _s := countryCode.ToString;
    if areaCode > 0 then
        _s := concat(_s, areaCode.ToString);
    if number > 0 then
        _s := concat(_s, number.toString);

    Result := StrToInt64(_s);
end;


function TContactPhone.getMask: string;
begin
    Result := strings['mask'];
end;

function TContactPhone.getnumber: int64;
begin
    Result := Int64s['number'];
end;


function TContactPhone.getPhoneNo: string;
begin
    Result := '';
    if countryCode > 0 then
        Result := '+' +  countryCode.ToString;

    if areaCode > 0 then begin
        if not Result.isEmpty then
            Result := Result + ' ';
        Result := concat(Result, areaCode.ToString);
	end;

	if number > 0 then begin
        if not Result.isEmpty then
            Result := Result + ' ';
        Result := concat(Result, number.ToString);
	end;
end;

procedure TContactPhone.setareaCode(const _value: integer);
begin
    integers['areaCode'] := _value;
    doUpdate;
end;

procedure TContactPhone.setcountryCode(const _value: integer);
begin
    integers['countryCode'] := _value;
    doUpdate;
end;

procedure TContactPhone.setmask(const _value: string);
begin
    strings['mask'] := _value;
    doUpdate;
end;

procedure TContactPhone.setnumber(const _value: int64);
begin
    Int64s['number'] := _value;
    doUpdate;
end;



constructor TContactPhone.Create;
begin
	inherited Create;
    strings ['mask']        := '+{{countryCode}} {{areaCode}} {{number}}';
    integers['countryCode'] := -1;
    integers['areaCode']    := -1;
    Int64s  ['number']      := -1;
end;

{ TContactEmail }

function TContactEmail.getEmail: string;
begin
    Result := strings['Email'];
end;

procedure TContactEmail.setEmail(const _value: string);
begin
    strings['Email'] := _value;
    doUpdate;
end;

constructor TContactEmail.Create;
begin
	inherited Create;
    Strings['Email'] := '';
end;

function TContactEmail.isValid: boolean;
begin
    Result := false; // TO DO
end;

{ TContactAddress }

function TContactAddress.getCity: string;
begin
    Result := strings['City'];
end;

function TContactAddress.getCountry: string;
begin
    Result := strings['Country'];
end;

function TContactAddress.GetLine1: string;
begin
    Result := strings['Line1'];
end;

function TContactAddress.getLine2: string;
begin
    Result := strings['Line2'];
end;

function TContactAddress.getPostCode: string;
begin
    Result := strings['PostCode'];
end;

function TContactAddress.getRegion: string;
begin
    Result := strings['Region'];
end;

function TContactAddress.getState: string;
begin
    Result := strings['State'];
end;

procedure TContactAddress.setCity(const _value: string);
begin
    strings['City'] := _value;
    doUpdate;
end;

procedure TContactAddress.setCountry(const _value: string);
begin
    strings['Country'] := _value;
    doUpdate;
end;


procedure TContactAddress.SetLine1(const _value: string);
begin
    strings['Line1'] := _value;
    doUpdate;
end;

procedure TContactAddress.setLine2(const _value: string);
begin
    strings['Line2'] := _value;
    doUpdate;
end;

procedure TContactAddress.setPostCode(const _value: string);
begin
    strings['PostCode'] := _value;
    doUpdate;
end;

procedure TContactAddress.setRegion(const _value: string);
begin
    strings['Region'] := _value;
    doUpdate;
end;

procedure TContactAddress.setState(const _value: string);
begin
    strings['State'] := _value;
    doUpdate;
end;

constructor TContactAddress.Create;
begin
	inherited Create;
    strings['Line1']    := '';
    strings['Line2']    := '';
    strings['City']     := '';
    strings['Region']   := '';
    strings['State']    := '';
    strings['Country']  := '';
    strings['PostCode'] := '';
end;

{ TContactEmailList }

function TContactEmailList.getemail(_key: string): TContactEmail;
begin
    if not Find(_key, TJSONObject(Result)) then begin
        Result := TContactEmail.Create;
        Result.BelongsTo := Self.belongsTo;
        objects[_key] := Result;
	end;
end;

procedure TContactEmailList.setbelongsTo(const _value: TContact);
begin
	if mybelongsTo=_value then Exit;
	mybelongsTo:=_value;
end;

procedure TContactEmailList.setdefault(const _value: TContactEmail);
begin
	if mydefault=_value then Exit;
	mydefault:=_value;

end;

procedure TContactEmailList.setEmail(_key: string; const _value: TContactEmail);
begin
    objects[_key] := _value;
    if not assigned(default) then default := _value;

end;

function TContactEmailList.keys: TStringArray;
var
	_i: Integer;
begin
    Result := [];
    SetLength(Result, Count);
    for _i := 0 to pred(Count) do
        Result[_i] := Names[_i];
end;

constructor TContactEmailList.Create;
begin
    inherited Create;
end;


{ TContactPhoneList }

function TContactPhoneList.getNamed(_key: string): TContactPhone;
begin
    if not Find(_key, TJSONObject(Result)) then begin
        Result := TContactPhone.Create;
        Result.BelongsTo := self.belongsTo;
        objects[_key] := Result;
	end;
end;

procedure TContactPhoneList.setbelongsTo(const _value: TContact);
begin
	if mybelongsTo=_value then Exit;
	mybelongsTo:=_value;

end;

procedure TContactPhoneList.setdefault(const _value: TContactPhone);
begin
	if mydefault=_value then Exit;
	mydefault:=_value;

end;

procedure TContactPhoneList.setNamed(_key: string; const _value: TContactPhone);
begin
    objects[_key] := _value;
    if not assigned(default) then default := _value;

end;


function TContactPhoneList.remove(_key: string): boolean;
begin
    Result := IndexOfName(_key) >= 0;
    Delete(_key);

end;

function TContactPhoneList.keys: TStringArray;
var
	_i: Integer;
begin
    Result := [];
    SetLength(Result, Count);
    for _i := 0 to pred(Count) do
        Result[_i] := Names[_i];
end;

constructor TContactPhoneList.Create;
begin
    inherited Create;
end;


{ TContactAddressList }

function TContactAddressList.getNamed(_key: string): TContactAddress;
var
	_i: Integer;
begin
    if not Find(_key, TJSONObject(Result)) then begin
        Result := TContactAddress.Create;
        Result.BelongsTo := belongsTo;
        objects[_key] := Result;
	end;
end;

procedure TContactAddressList.setNamed(_key: string;
	const _value: TContactAddress);
begin
    objects[_key] := _value;
    if not (_value.BelongsTo <> belongsTo) then
        _value.BelongsTo := belongsTo;
    if not assigned(default) then default := _value;

end;

procedure TContactAddressList.setbelongsTo(const _value: TContact);
begin
	if mybelongsTo=_value then Exit;
	mybelongsTo:=_value;

end;

procedure TContactAddressList.setdefault(const _value: TContactAddress);
begin
	if mydefault=_value then Exit;
	mydefault:=_value;

end;


function TContactAddressList.keys: TStringArray;
var
	_i: Integer;
begin
    Result := [];
    SetLength(Result, Count);
    for _i := 0 to pred(Count) do
        Result[_i] := Names[_i];
end;



{ TContactPersonBasic }

function TContactPersonBasic.getEmails: TContactEmailList;
begin
    result := TContactEmailList (objects['Emails']);
end;

function TContactPersonBasic.getName: string;
begin
    Result := strings['Name'];
end;

function TContactPersonBasic.getPhones: TContactPhoneList;
begin
    Result := TContactPhoneList(objects['Phones']);
end;

procedure TContactPersonBasic.setEmails(const _value: TContactEmailList);
begin
    objects['Emails'] := _value;
    doUpdate;
end;

procedure TContactPersonBasic.setName(const _value: string);
begin
    strings['Name'] := _value;
    doUpdate;
end;

procedure TContactPersonBasic.setPhones(const _value: TContactPhoneList);
begin
    objects['Phones'] := _value;
    doUpdate;
end;

constructor TContactPersonBasic.Create;
begin
    inherited Create;
    strings['Name']   := '';
    Objects['Emails'] := TContactEmailList.Create;
    Objects['Phones'] := TContactPhoneList.Create;
end;

destructor TContactPersonBasic.Destroy;
begin
	inherited Destroy;
end;


{ TContactPerson }

function TContactPerson.getAddresses: TContactAddressList;
begin
    Result := TContactAddressList(Objects['Addresses']);
end;

function TContactPerson.getDOB: TDateTime;
begin
    Result := TDateTime(Floats['DOB']);
end;



function TContactPerson.getGender: string;
begin
    Result := Strings['Gender'];
end;


procedure TContactPerson.setDOB(const _value: TDateTime);
begin
    floats['DOB'] := _value;
    doUpdate;
end;


procedure TContactPerson.setGender(const _value: string);
begin
    strings['Gender'] := _value;
    doUpdate;
end;

constructor TContactPerson.Create;
begin
	inherited Create;
    Floats['DOB']        := 0;
    strings['Gender']    := '';
    objects['Addresses'] := TContactAddressList.Create;
end;

destructor TContactPerson.Destroy;
begin
	inherited Destroy;
end;


end.



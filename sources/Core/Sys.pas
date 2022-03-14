unit Sys;

interface

type
  TNode=class
  public
    Owner : TNode;

    {$IFDEF INTERNAL}
    class var Counter : Integer;

    Constructor Create;
    Destructor Destroy; override;
    {$ENDIF}
  end;

  TData=class(TNode)
  {
  public
    function Value: TData; virtual;
  }
  end;

  TBoolean=class(TData)
  public
    Value : Boolean;
    Constructor Create(const AValue:Boolean);
  end;

  TNumber=class(TData)
  public
    Text:String; // parsed text
  end;

  TInteger=class(TNumber)
  public
    Value : Int64;
    Base : Byte;

    Constructor Create(const AValue:Int64);
  end;

  Float=Extended;

  TFloat=class(TNumber)
  public
    Value : Float;
    Constructor Create(const AValue:Float);
  end;

  TText=class(TData)
  public
    Value : String;
    Constructor Create(const AValue:String);
  end;

  TDateTime=class(TFloat)
  end;

  TDate=class(TDateTime)
  end;

  TTime=class(TDateTime)
  end;

implementation

{$IFDEF INTERNAL}
uses
  Exceptions;
{$ENDIF}

{$IFDEF INTERNAL}
Constructor TNode.Create;
begin
  inherited;
  Inc(Counter);
end;

Destructor TNode.Destroy;
begin
  Dec(Counter);
  inherited;
end;
{$ENDIF}

{ TBoolean }

constructor TBoolean.Create(const AValue: Boolean);
begin
  inherited Create;
  Value:=AValue;
end;

{ TInteger }

constructor TInteger.Create(const AValue: Int64);
begin
  inherited Create;
  Value:=AValue;
end;

{ TFloat }

constructor TFloat.Create(const AValue: Float);
begin
  inherited Create;
  Value:=AValue;
end;

{ TText }

constructor TText.Create(const AValue: String);
begin
  inherited Create;
  Value:=AValue;
end;

{ TData }

{
function TData.Value: TData;
begin
  result:=Self;
end;
}

end.



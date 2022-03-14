unit Sort;

interface

type
  TComparison=(Equal,Greater,Lower);
  TCompare=function(const a,b:Integer):TComparison of object;
  TSwap=procedure(const a,b:Integer) of object;

  TSort=record
  public
    class procedure Sort(const Ascending:Boolean;
                         const AStart,AEnd:Integer;
                         const ACompare:TCompare;
                         const ASwap:TSwap); static;
  end;

implementation

class procedure TSort.Sort(const Ascending:Boolean;
                           const AStart,AEnd:Integer;
                           const ACompare:TCompare;
                           const ASwap:TSwap);

  procedure DoSort(const l,r:Integer);
  var i : Integer;
      j : Integer;
      x : Integer;
  begin
    i:=l;
    j:=r;
    x:=(i+j) shr 1;

    while i<j do
    begin
      if Ascending then
      begin
        while ACompare(i,x)=TComparison.Lower do inc(i);
        while ACompare(x,j)=TComparison.Lower do dec(j);
      end
      else
      begin
        while ACompare(i,x)=TComparison.Greater do inc(i);
        while ACompare(x,j)=TComparison.Greater do dec(j);
      end;

      if i<j then
      begin
        ASwap(i,j);

        if i=x then
           x:=j
        else
        if j=x then
           x:=i;
      end;

      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;

    if l<j then
       DoSort(l,j);

    if i<r then
       DoSort(i,r);
  end;

begin
  if AEnd>AStart+1 then
     DoSort(AStart,AEnd);
end;

end.

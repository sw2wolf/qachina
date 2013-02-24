program Primzahl;
begin
  write(2);
  bis:=100;
  Zahl:=3;
  while Zahl<bis do begin
    Teilbar:=0;
    Faktor1:=3;
    while Faktor1<Zahl/2 do begin
      Faktor2:=3;
      while Faktor2<Zahl/2 do begin
        if Zahl=Faktor1*Faktor2 then begin
          Teilbar:=1
        end;
        Faktor2:=Faktor2+2;
      end;
      Faktor1:=Faktor1+2
    end;
    if Teilbar=0 then begin
      write(Zahl)
    end;
    Zahl:=Zahl+2
  end
end.
%--------------------
program Fakultaet;
begin
  n:= 7;
  i:= 1;
  Fak:= 1;
  while i < n do begin
    i:= i + 1;
    Fak:= Fak * i
  end;
  write(Fak)
end.
%-------------------
program Potenzerrechnen;
begin
  i:=0;
  n:=30;
  Potenz:=1;
  while i<n do begin
    Potenz:=Potenz*n;
    i:=i+1;
  end;
end.
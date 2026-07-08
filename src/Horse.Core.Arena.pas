unit Horse.Core.Arena;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  Horse.Commons;

type
  PHorseArenaPage = ^THorseArenaPage;
  THorseArenaPage = record
    Next: PHorseArenaPage;
    Size: NativeInt;
    Used: NativeInt;
    Data: array[0..0] of Byte;
  end;

  THorseArenaAllocator = class
  private
    FFirstPage: PHorseArenaPage;
    FCurrentPage: PHorseArenaPage;
    FPageSize: NativeInt;
    FRegisteredObjects: TList;
    function AllocatePage(ASize: NativeInt): PHorseArenaPage;
  public
    constructor Create(APageSize: NativeInt = 65536);
    destructor Destroy; override;
    function Alloc(ASize: NativeInt): Pointer;
    procedure RegisterObject(AObject: TObject);
    procedure Reset;
  end;

implementation

constructor THorseArenaAllocator.Create(APageSize: NativeInt);
begin
  inherited Create;
  FPageSize := APageSize;
  FRegisteredObjects := TList.Create;
  FFirstPage := AllocatePage(FPageSize);
  FCurrentPage := FFirstPage;
end;

destructor THorseArenaAllocator.Destroy;
var
  LPage, LNext: PHorseArenaPage;
  I: Integer;
begin
  Reset;
  FRegisteredObjects.Free;
  LPage := FFirstPage;
  while LPage <> nil do
  begin
    LNext := LPage.Next;
    FreeMem(LPage);
    LPage := LNext;
  end;
  inherited;
end;

function THorseArenaAllocator.AllocatePage(ASize: NativeInt): PHorseArenaPage;
var
  LAllocSize: NativeInt;
begin
  if ASize > FPageSize then
    LAllocSize := ASize
  else
    LAllocSize := FPageSize;

  // Aloca a estrutura da página mais o tamanho de dados solicitado
  // (Data[0] já consome 1 byte, então subtraímos 1)
  GetMem(Result, SizeOf(THorseArenaPage) + LAllocSize - 1);
  Result.Next := nil;
  Result.Size := LAllocSize;
  Result.Used := 0;
end;

function THorseArenaAllocator.Alloc(ASize: NativeInt): Pointer;
var
  LAlignedSize: NativeInt;
  LNewPage: PHorseArenaPage;
begin
  // Alinha o tamanho solicitado ao tamanho do ponteiro da palavra da CPU (4 ou 8 bytes)
  LAlignedSize := (ASize + (SizeOf(Pointer) - 1)) and not (SizeOf(Pointer) - 1);

  if FCurrentPage.Used + LAlignedSize <= FCurrentPage.Size then
  begin
    Result := @FCurrentPage.Data[FCurrentPage.Used];
    Inc(FCurrentPage.Used, LAlignedSize);
  end
  else
  begin
    if FCurrentPage.Next <> nil then
    begin
      FCurrentPage := FCurrentPage.Next;
      // Garante que a página existente de pool tenha tamanho adequado
      if FCurrentPage.Size < LAlignedSize then
      begin
        // Se a página encadeada é menor, recriamos uma adequada
        LNewPage := AllocatePage(LAlignedSize);
        LNewPage.Next := FCurrentPage.Next;
        FreeMem(FCurrentPage);
        FCurrentPage := LNewPage;
      end;
      FCurrentPage.Used := 0;
    end
    else
    begin
      LNewPage := AllocatePage(LAlignedSize);
      FCurrentPage.Next := LNewPage;
      FCurrentPage := LNewPage;
    end;
    Result := @FCurrentPage.Data[0];
    FCurrentPage.Used := LAlignedSize;
  end;
end;

procedure THorseArenaAllocator.RegisterObject(AObject: TObject);
begin
  if AObject <> nil then
    FRegisteredObjects.Add(AObject);
end;

procedure THorseArenaAllocator.Reset;
var
  LPage: PHorseArenaPage;
  I: Integer;
  LObj: TObject;
begin
  // Libera todos os objetos gerenciados de trás para frente (ordem reversa de criação)
  for I := FRegisteredObjects.Count - 1 downto 0 do
  begin
    LObj := TObject(FRegisteredObjects.Items[I]);
    if LObj <> nil then
    begin
      try
        LObj.Free;
      except
        // Ignora erros na destruição dos objetos registrados para não corromper a Arena
      end;
    end;
  end;
  FRegisteredObjects.Clear;

  // Reseta os deslocamentos em todas as páginas alocadas para reutilização livre de locks
  LPage := FFirstPage;
  while LPage <> nil do
  begin
    LPage.Used := 0;
    LPage := LPage.Next;
  end;
  FCurrentPage := FFirstPage;
end;

end.

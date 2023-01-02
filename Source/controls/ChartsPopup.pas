unit ChartsPopup;

interface

uses
  Classes,
  Images,
  Menus,
  SysUtils;

type
  TChartsPopup = class(TPopupMenu)
  private
    FItemAddToWishlist: TMenuItem;
    FItemRemoveFromWishlist: TMenuItem;
    FItemAddArtistToWishlist: TMenuItem;
    FItemEditAndAddToWishlist: TMenuItem;
    FItemStartStreaming: TMenuItem;
    FItemPlayStream: TMenuItem;
    FItemPlayStreamExternal: TMenuItem;
    FItemAddStream: TMenuItem;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ItemAddToWishlist: TMenuItem read FItemAddToWishlist;
    property ItemRemoveFromWishlist: TMenuItem read FItemRemoveFromWishlist;
    property ItemAddArtistToWishlist: TMenuItem read FItemAddArtistToWishlist;
    property ItemEditAndAddToWishlist: TMenuItem read FItemEditAndAddToWishlist;
    property ItemStartStreaming: TMenuItem read FItemStartStreaming;
    property ItemPlayStream: TMenuItem read FItemPlayStream;
    property ItemPlayStreamExternal: TMenuItem read FItemPlayStreamExternal;
    property ItemAddStream: TMenuItem read FItemAddStream;
  end;

implementation

{ TChartsPopup }

constructor TChartsPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemAddToWishlist := TMenuItem.Create(Self);
  FItemAddToWishlist.Caption := '&Add title to automatic wishlist';
  FItemAddToWishlist.ImageIndex := TImages.SCRIPT_BRICKS_ADD;
  Items.Add(FItemAddToWishlist);

  FItemRemoveFromWishlist := TMenuItem.Create(Self);
  FItemRemoveFromWishlist.Caption := '&Remove title from automatic wishlist';
  FItemRemoveFromWishlist.ImageIndex := TImages.SCRIPT_BRICKS_DELETE;
  Items.Add(FItemRemoveFromWishlist);

  FItemAddArtistToWishlist := TMenuItem.Create(Self);
  FItemAddArtistToWishlist.Caption := 'A&dd artist to automatic wishlist';
  FItemAddArtistToWishlist.ImageIndex := TImages.SCRIPT_USER_GRAY_COOL_ADD;
  Items.Add(FItemAddArtistToWishlist);

  Items.AddSeparator;

  FItemEditAndAddToWishlist := TMenuItem.Create(Self);
  FItemEditAndAddToWishlist.Caption := '&Edit and add to manual wishlist...';
  FItemEditAndAddToWishlist.ImageIndex := TImages.SCRIPT_HEART_ADD;
  Items.Add(FItemEditAndAddToWishlist);

  Items.AddSeparator;

  FItemStartStreaming := TMenuItem.Create(Self);
  FItemStartStreaming.Caption := '&Start recording';
  FItemStartStreaming.ImageIndex := TImages.RECORD_RED;
  Items.Add(FItemStartStreaming);

  FItemPlayStream := TMenuItem.Create(Self);
  FItemPlayStream.Caption := '&Play stream';
  FItemPlayStream.ImageIndex := TImages.PLAY_BLUE;
  Items.Add(FItemPlayStream);

  FItemPlayStreamExternal := TMenuItem.Create(Self);
  FItemPlayStreamExternal.Caption := 'P&lay stream (external player)';
  FItemPlayStreamExternal.ImageIndex := TImages.PLAY_GO;
  Items.Add(FItemPlayStreamExternal);

  FItemAddStream := TMenuItem.Create(Self);
  FItemAddStream.Caption := 'Add s&tream';
  FItemAddStream.ImageIndex := TImages.ADD;
  Items.Add(FItemAddStream);
end;

end.

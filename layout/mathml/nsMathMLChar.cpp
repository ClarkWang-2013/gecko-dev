/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "nsMathMLChar.h"
#include "mozilla/MathAlgorithms.h"

#include "nsCOMPtr.h"
#include "nsIFrame.h"
#include "nsPresContext.h"
#include "nsStyleContext.h"
#include "nsUnicharUtils.h"
#include "nsRenderingContext.h"

#include "mozilla/Preferences.h"
#include "nsIPersistentProperties2.h"
#include "nsIObserverService.h"
#include "nsIObserver.h"
#include "nsNetUtil.h"

#include "mozilla/LookAndFeel.h"
#include "nsCSSRendering.h"
#include "prprf.h"         // For PR_snprintf()

#include "nsDisplayList.h"

#include "nsMathMLOperators.h"
#include <algorithm>

#include "gfxMathTable.h"

using namespace mozilla;

//#define NOISY_SEARCH 1

static const float kLargeOpFactor = float(M_SQRT2);
static const float kIntegralFactor = 2.0;

// -----------------------------------------------------------------------------
static const nsGlyphCode kNullGlyph = {{{0, 0}}, 0};
typedef enum {eExtension_base, eExtension_variants, eExtension_parts}
  nsMathfontPrefExtension;

// -----------------------------------------------------------------------------
// nsGlyphTable is a class that provides an interface for accessing glyphs
// of stretchy chars. It acts like a table that stores the variants of bigger
// sizes (if any) and the partial glyphs needed to build extensible symbols.
//
// Bigger sizes (if any) of the char can then be retrieved with BigOf(...).
// Partial glyphs can be retrieved with ElementAt(...).
//
// A table consists of "nsGlyphCode"s which are viewed either as Unicode
// points (for nsPropertiesTable) or as direct glyph indices (for
// nsOpenTypeTable)
// -----------------------------------------------------------------------------

class nsGlyphTable {
public:
  virtual ~nsGlyphTable() {}

  virtual const nsAString&
  FontNameFor(const nsGlyphCode& aGlyphCode) const = 0;

  // Getters for the parts
  virtual nsGlyphCode ElementAt(gfxContext*   aThebesContext,
                                int32_t       aAppUnitsPerDevPixel,
                                gfxFontGroup* aFontGroup,
                                char16_t      aChar,
                                bool          aVertical,
                                uint32_t      aPosition) = 0;
  virtual nsGlyphCode BigOf(gfxContext*   aThebesContext,
                            int32_t       aAppUnitsPerDevPixel,
                            gfxFontGroup* aFontGroup,
                            char16_t      aChar,
                            bool          aVertical,
                            uint32_t      aSize) = 0;

  // True if this table contains parts to render this char
  virtual bool HasPartsOf(gfxContext*   aThebesContext,
                          int32_t       aAppUnitsPerDevPixel,
                          gfxFontGroup* aFontGroup,
                          char16_t      aChar,
                          bool          aVertical) = 0;

  virtual gfxTextRun* MakeTextRun(gfxContext*        aThebesContext,
                                  int32_t            aAppUnitsPerDevPixel,
                                  gfxFontGroup*      aFontGroup,
                                  const nsGlyphCode& aGlyph) = 0;
protected:
  nsGlyphTable() : mCharCache(0) {}
  // For speedy re-use, we always cache the last data used in the table.
  // mCharCache is the Unicode point of the last char that was queried in this
  // table.
  char16_t mCharCache;
};

// An instance of nsPropertiesTable is associated with one primary font. Extra
// glyphs can be taken in other additional fonts when stretching certain
// characters.
// These supplementary fonts are referred to as "external" fonts to the table.

// General format of MathFont Property Files from which glyph data are
// retrieved:
// -----------------------------------------------------------------------------
// Each font should have its set of glyph data. For example, the glyph data for
// the "Symbol" font and the "MT Extra" font are in "mathfontSymbol.properties"
// and "mathfontMTExtra.properties", respectively. The mathfont property file
// is a set of all the stretchy MathML characters that can be rendered with that
// font using larger and/or partial glyphs. The entry of each stretchy character
// in the mathfont property file gives, in that order, the 4 partial glyphs:
// Top (or Left), Middle, Bottom (or Right), Glue; and the variants of bigger
// sizes (if any).
// A position that is not relevant to a particular character is indicated there
// with the UNICODE REPLACEMENT CHARACTER 0xFFFD.
// -----------------------------------------------------------------------------

#define NS_TABLE_STATE_ERROR       -1
#define NS_TABLE_STATE_EMPTY        0
#define NS_TABLE_STATE_READY        1

// helper to trim off comments from data in a MathFont Property File
static void
Clean(nsString& aValue)
{
  // chop the trailing # comment portion if any ...
  int32_t comment = aValue.RFindChar('#');
  if (comment > 0) aValue.Truncate(comment);
  aValue.CompressWhitespace();
}

// helper to load a MathFont Property File
static nsresult
LoadProperties(const nsString& aName,
               nsCOMPtr<nsIPersistentProperties>& aProperties)
{
  nsAutoString uriStr;
  uriStr.AssignLiteral("resource://gre/res/fonts/mathfont");
  uriStr.Append(aName);
  uriStr.StripWhitespace(); // that may come from aName
  uriStr.AppendLiteral(".properties");
  return NS_LoadPersistentPropertiesFromURISpec(getter_AddRefs(aProperties), 
                                                NS_ConvertUTF16toUTF8(uriStr));
}

class nsPropertiesTable MOZ_FINAL : public nsGlyphTable {
public:
  explicit nsPropertiesTable(const nsString& aPrimaryFontName)
    : mFontName(1) // ensure space for primary font name.
    , mState(NS_TABLE_STATE_EMPTY)
  {
    MOZ_COUNT_CTOR(nsPropertiesTable);
    mFontName.AppendElement(aPrimaryFontName);
  }

  ~nsPropertiesTable()
  {
    MOZ_COUNT_DTOR(nsPropertiesTable);
  }

  const nsAString& PrimaryFontName() const
  {
    return mFontName[0];
  }

  const nsAString&
  FontNameFor(const nsGlyphCode& aGlyphCode) const MOZ_OVERRIDE
  {
    NS_ASSERTION(!aGlyphCode.IsGlyphID(),
                 "nsPropertiesTable can only access glyphs by code point");
    return mFontName[aGlyphCode.font];
  }

  virtual nsGlyphCode ElementAt(gfxContext*   aThebesContext,
                                int32_t       aAppUnitsPerDevPixel,
                                gfxFontGroup* aFontGroup,
                                char16_t      aChar,
                                bool          aVertical,
                                uint32_t      aPosition) MOZ_OVERRIDE;

  virtual nsGlyphCode BigOf(gfxContext*   aThebesContext,
                            int32_t       aAppUnitsPerDevPixel,
                            gfxFontGroup* aFontGroup,
                            char16_t      aChar,
                            bool          aVertical,
                            uint32_t      aSize) MOZ_OVERRIDE
  {
    return ElementAt(aThebesContext, aAppUnitsPerDevPixel, aFontGroup,
                     aChar, aVertical, 4 + aSize);
  }

  virtual bool HasPartsOf(gfxContext*   aThebesContext,
                          int32_t       aAppUnitsPerDevPixel,
                          gfxFontGroup* aFontGroup,
                          char16_t      aChar,
                          bool          aVertical) MOZ_OVERRIDE
  {
    return (ElementAt(aThebesContext, aAppUnitsPerDevPixel, aFontGroup,
                      aChar, aVertical, 0).Exists() ||
            ElementAt(aThebesContext, aAppUnitsPerDevPixel, aFontGroup,
                      aChar, aVertical, 1).Exists() ||
            ElementAt(aThebesContext, aAppUnitsPerDevPixel, aFontGroup,
                      aChar, aVertical, 2).Exists() ||
            ElementAt(aThebesContext, aAppUnitsPerDevPixel, aFontGroup,
                      aChar, aVertical, 3).Exists());
  }

  virtual gfxTextRun* MakeTextRun(gfxContext*        aThebesContext,
                                  int32_t            aAppUnitsPerDevPixel,
                                  gfxFontGroup*      aFontGroup,
                                  const nsGlyphCode& aGlyph) MOZ_OVERRIDE;
private:

  // mFontName[0] is the primary font associated to this table. The others 
  // are possible "external" fonts for glyphs not in the primary font
  // but which are needed to stretch certain characters in the table
  nsTArray<nsString> mFontName;

  // Tri-state variable for error/empty/ready
  int32_t mState;

  // The set of glyph data in this table, as provided by the MathFont Property
  // File
  nsCOMPtr<nsIPersistentProperties> mGlyphProperties;

  // mGlyphCache is a buffer containing the glyph data associated with
  // mCharCache.
  // For a property line 'key = value' in the MathFont Property File,
  // mCharCache will retain the 'key' -- which is a Unicode point, while
  // mGlyphCache will retain the 'value', which is a consecutive list of
  // nsGlyphCodes, i.e., the pairs of 'code@font' needed by the char -- in
  // which 'code@0' can be specified
  // without the optional '@0'. However, to ease subsequent processing,
  // mGlyphCache excludes the '@' symbol and explicitly inserts all optional '0'
  // that indicates the primary font identifier. Specifically therefore, the
  // k-th glyph is characterized by :
  // 1) mGlyphCache[3*k],mGlyphCache[3*k+1] : its Unicode point
  // 2) mGlyphCache[3*k+2] : the numeric identifier of the font where it comes
  // from.
  // A font identifier of '0' means the default primary font associated to this
  // table. Other digits map to the "external" fonts that may have been
  // specified in the MathFont Property File.
  nsString  mGlyphCache;
};

/* virtual */
nsGlyphCode
nsPropertiesTable::ElementAt(gfxContext*   /* aThebesContext */,
                             int32_t       /* aAppUnitsPerDevPixel */,
                             gfxFontGroup* /* aFontGroup */,
                             char16_t      aChar,
                             bool          /* aVertical */,
                             uint32_t      aPosition)
{
  if (mState == NS_TABLE_STATE_ERROR) return kNullGlyph;
  // Load glyph properties if this is the first time we have been here
  if (mState == NS_TABLE_STATE_EMPTY) {
    nsresult rv = LoadProperties(mFontName[0], mGlyphProperties);
#ifdef DEBUG
    nsAutoCString uriStr;
    uriStr.AssignLiteral("resource://gre/res/fonts/mathfont");
    LossyAppendUTF16toASCII(mFontName[0], uriStr);
    uriStr.StripWhitespace(); // that may come from mFontName
    uriStr.AppendLiteral(".properties");
    printf("Loading %s ... %s\n",
            uriStr.get(),
            (NS_FAILED(rv)) ? "Failed" : "Done");
#endif
    if (NS_FAILED(rv)) {
      mState = NS_TABLE_STATE_ERROR; // never waste time with this table again
      return kNullGlyph;
    }
    mState = NS_TABLE_STATE_READY;

    // see if there are external fonts needed for certain chars in this table
    nsAutoCString key;
    nsAutoString value;
    for (int32_t i = 1; ; i++) {
      key.AssignLiteral("external.");
      key.AppendInt(i, 10);
      rv = mGlyphProperties->GetStringProperty(key, value);
      if (NS_FAILED(rv)) break;
      Clean(value);
      mFontName.AppendElement(value); // i.e., mFontName[i] holds this font name
    }
  }

  // Update our cache if it is not associated to this character
  if (mCharCache != aChar) {
    // The key in the property file is interpreted as ASCII and kept
    // as such ...
    char key[10]; PR_snprintf(key, sizeof(key), "\\u%04X", aChar);
    nsAutoString value;
    nsresult rv = mGlyphProperties->GetStringProperty(nsDependentCString(key),
                                                      value);
    if (NS_FAILED(rv)) return kNullGlyph;
    Clean(value);
    // See if this char uses external fonts; e.g., if the 2nd glyph is taken
    // from the external font '1', the property line looks like
    // \uNNNN = \uNNNN\uNNNN@1\uNNNN.
    // This is where mGlyphCache is pre-processed to explicitly store all glyph
    // codes as combined pairs of 'code@font', excluding the '@' separator. This
    // means that mGlyphCache[3*k],mGlyphCache[3*k+1] will later be rendered
    // with mFontName[mGlyphCache[3*k+2]]
    // Note: font identifier is internally an ASCII digit to avoid the null
    // char issue
    nsAutoString buffer;
    int32_t length = value.Length();
    int32_t i = 0; // index in value
    while (i < length) {
      char16_t code = value[i];
      ++i;
      buffer.Append(code);
      // Read the next word if we have a non-BMP character.
      if (i < length && NS_IS_HIGH_SURROGATE(code)) {
        code = value[i];
        ++i;
      } else {
        code = char16_t('\0');
      }
      buffer.Append(code);

      // See if an external font is needed for the code point.
      // Limit of 9 external fonts
      char16_t font = 0;
      if (i+1 < length && value[i] == char16_t('@') &&
          value[i+1] >= char16_t('0') && value[i+1] <= char16_t('9')) {
        ++i;
        font = value[i] - '0';
        ++i;
        if (font >= mFontName.Length()) {
          NS_ERROR("Nonexistent font referenced in glyph table");
          return kNullGlyph;
        }
        // The char cannot be handled if this font is not installed
        if (!mFontName[font].Length()) {
          return kNullGlyph;
        }
      }
      buffer.Append(font);
    }
    // update our cache with the new settings
    mGlyphCache.Assign(buffer);
    mCharCache = aChar;
  }

  // 3* is to account for the code@font pairs
  uint32_t index = 3*aPosition;
  if (index+2 >= mGlyphCache.Length()) return kNullGlyph;
  nsGlyphCode ch;
  ch.code[0] = mGlyphCache.CharAt(index);
  ch.code[1] = mGlyphCache.CharAt(index + 1);
  ch.font = mGlyphCache.CharAt(index + 2);
  return ch.code[0] == char16_t(0xFFFD) ? kNullGlyph : ch;
}

/* virtual */
gfxTextRun*
nsPropertiesTable::MakeTextRun(gfxContext*        aThebesContext,
                               int32_t            aAppUnitsPerDevPixel,
                               gfxFontGroup*      aFontGroup,
                               const nsGlyphCode& aGlyph)
{
  NS_ASSERTION(!aGlyph.IsGlyphID(),
               "nsPropertiesTable can only access glyphs by code point");
  return aFontGroup->
    MakeTextRun(aGlyph.code, aGlyph.Length(), aThebesContext,
                aAppUnitsPerDevPixel, 0);
}

// An instance of nsOpenTypeTable is associated with one gfxFontEntry that
// corresponds to an Open Type font with a MATH table. All the glyphs come from
// the same font and the calls to access size variants and parts are directly
// forwarded to the gfx code.
class nsOpenTypeTable MOZ_FINAL : public nsGlyphTable {
public:
  ~nsOpenTypeTable()
  {
    MOZ_COUNT_DTOR(nsOpenTypeTable);
  }

  virtual nsGlyphCode ElementAt(gfxContext*   aThebesContext,
                                int32_t       aAppUnitsPerDevPixel,
                                gfxFontGroup* aFontGroup,
                                char16_t      aChar,
                                bool          aVertical,
                                uint32_t      aPosition) MOZ_OVERRIDE;
  virtual nsGlyphCode BigOf(gfxContext*   aThebesContext,
                            int32_t       aAppUnitsPerDevPixel,
                            gfxFontGroup* aFontGroup,
                            char16_t      aChar,
                            bool          aVertical,
                            uint32_t      aSize) MOZ_OVERRIDE;
  virtual bool HasPartsOf(gfxContext*   aThebesContext,
                          int32_t       aAppUnitsPerDevPixel,
                          gfxFontGroup* aFontGroup,
                          char16_t      aChar,
                          bool          aVertical) MOZ_OVERRIDE;

  const nsAString&
  FontNameFor(const nsGlyphCode& aGlyphCode) const MOZ_OVERRIDE {
    NS_ASSERTION(aGlyphCode.IsGlyphID(),
                 "nsOpenTypeTable can only access glyphs by id");
    return mFontEntry->FamilyName();
  }

  virtual gfxTextRun* MakeTextRun(gfxContext*        aThebesContext,
                                  int32_t            aAppUnitsPerDevPixel,
                                  gfxFontGroup*      aFontGroup,
                                  const nsGlyphCode& aGlyph) MOZ_OVERRIDE;

  // This returns a new OpenTypeTable instance to give access to OpenType MATH
  // table or nullptr if the font does not have such table. Ownership is passed
  // to the caller.
  static nsOpenTypeTable* Create(gfxFont* aFont)
  {
    if (!aFont->GetFontEntry()->TryGetMathTable(aFont)) {
      return nullptr;
    }
    return new nsOpenTypeTable(aFont->GetFontEntry());
  }

private:
  nsRefPtr<gfxFontEntry> mFontEntry;
  uint32_t mGlyphID;

  explicit nsOpenTypeTable(gfxFontEntry* aFontEntry)
    : mFontEntry(aFontEntry) {
    MOZ_COUNT_CTOR(nsOpenTypeTable);
  }

  void UpdateCache(gfxContext*   aThebesContext,
                   int32_t       aAppUnitsPerDevPixel,
                   gfxFontGroup* aFontGroup,
                   char16_t      aChar);
};

void
nsOpenTypeTable::UpdateCache(gfxContext*   aThebesContext,
                             int32_t       aAppUnitsPerDevPixel,
                             gfxFontGroup* aFontGroup,
                             char16_t      aChar)
{
  if (mCharCache != aChar) {
    nsAutoPtr<gfxTextRun> textRun;
    textRun = aFontGroup->
      MakeTextRun(&aChar, 1, aThebesContext, aAppUnitsPerDevPixel, 0);
    const gfxTextRun::CompressedGlyph& data = textRun->GetCharacterGlyphs()[0];
    if (data.IsSimpleGlyph()) {
      mGlyphID = data.GetSimpleGlyph();
    } else if (data.GetGlyphCount() == 1) {
      mGlyphID = textRun->GetDetailedGlyphs(0)->mGlyphID;
    } else {
      mGlyphID = 0;
    }
    mCharCache = aChar;
  }
}

/* virtual */
nsGlyphCode
nsOpenTypeTable::ElementAt(gfxContext*   aThebesContext,
                           int32_t       aAppUnitsPerDevPixel,
                           gfxFontGroup* aFontGroup,
                           char16_t      aChar,
                           bool          aVertical,
                           uint32_t      aPosition)
{
  UpdateCache(aThebesContext, aAppUnitsPerDevPixel, aFontGroup, aChar);

  uint32_t parts[4];
  if (!mFontEntry->GetMathVariantsParts(mGlyphID, aVertical, parts)) {
    return kNullGlyph;
  }

  uint32_t glyphID = parts[aPosition];
  if (!glyphID) {
    return kNullGlyph;
  }
  nsGlyphCode glyph;
  glyph.glyphID = glyphID;
  glyph.font = -1;
  return glyph;
}

/* virtual */
nsGlyphCode
nsOpenTypeTable::BigOf(gfxContext*   aThebesContext,
                       int32_t       aAppUnitsPerDevPixel,
                       gfxFontGroup* aFontGroup,
                       char16_t      aChar,
                       bool          aVertical,
                       uint32_t      aSize)
{
  UpdateCache(aThebesContext, aAppUnitsPerDevPixel, aFontGroup, aChar);

  uint32_t glyphID =
    mFontEntry->GetMathVariantsSize(mGlyphID, aVertical, aSize);
  if (!glyphID) {
    return kNullGlyph;
  }

  nsGlyphCode glyph;
  glyph.glyphID = glyphID;
  glyph.font = -1;
  return glyph;
}

/* virtual */
bool
nsOpenTypeTable::HasPartsOf(gfxContext*   aThebesContext,
                            int32_t       aAppUnitsPerDevPixel,
                            gfxFontGroup* aFontGroup,
                            char16_t      aChar,
                            bool          aVertical)
{
  UpdateCache(aThebesContext, aAppUnitsPerDevPixel, aFontGroup, aChar);

  uint32_t parts[4];
  if (!mFontEntry->GetMathVariantsParts(mGlyphID, aVertical, parts)) {
    return false;
  }

  return parts[0] || parts[1] || parts[2] || parts[3];
}

/* virtual */
gfxTextRun*
nsOpenTypeTable::MakeTextRun(gfxContext*        aThebesContext,
                             int32_t            aAppUnitsPerDevPixel,
                             gfxFontGroup*      aFontGroup,
                             const nsGlyphCode& aGlyph)
{
  NS_ASSERTION(aGlyph.IsGlyphID(),
               "nsOpenTypeTable can only access glyphs by id");

  gfxTextRunFactory::Parameters params = {
    aThebesContext, nullptr, nullptr, nullptr, 0, aAppUnitsPerDevPixel
  };
  gfxTextRun* textRun = gfxTextRun::Create(&params, 1, aFontGroup, 0);
  textRun->AddGlyphRun(aFontGroup->GetFontAt(0), gfxTextRange::kFontGroup, 0,
                       false);
  gfxTextRun::DetailedGlyph detailedGlyph;
  detailedGlyph.mGlyphID = aGlyph.glyphID;
  detailedGlyph.mAdvance =
    NSToCoordRound(aAppUnitsPerDevPixel *
                   aFontGroup->GetFontAt(0)->
                   GetGlyphHAdvance(aThebesContext, aGlyph.glyphID));
  detailedGlyph.mXOffset = detailedGlyph.mYOffset = 0;
  gfxShapedText::CompressedGlyph g;
  g.SetComplex(true, true, 1);
  textRun->SetGlyphs(0, g, &detailedGlyph);

  return textRun;
}

// -----------------------------------------------------------------------------
// This is the list of all the applicable glyph tables.
// We will maintain a single global instance that will only reveal those
// glyph tables that are associated to fonts currently installed on the
// user' system. The class is an XPCOM shutdown observer to allow us to
// free its allocated data at shutdown

class nsGlyphTableList : public nsIObserver
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_NSIOBSERVER

  nsPropertiesTable mUnicodeTable;

  nsGlyphTableList()
    : mUnicodeTable(NS_LITERAL_STRING("Unicode"))
  {
    MOZ_COUNT_CTOR(nsGlyphTableList);
  }

  virtual ~nsGlyphTableList()
  {
    MOZ_COUNT_DTOR(nsGlyphTableList);
  }

  nsresult Initialize();
  nsresult Finalize();

  // Add a glyph table in the list, return the new table that was added
  nsGlyphTable*
  AddGlyphTable(const nsString& aPrimaryFontName);

  // Find the glyph table in the list corresponding to the given font family.
  nsGlyphTable*
  GetGlyphTableFor(const nsAString& aFamily);

private:
  nsPropertiesTable* PropertiesTableAt(int32_t aIndex) {
    return &mPropertiesTableList.ElementAt(aIndex);
  }
  int32_t PropertiesTableCount() {
    return mPropertiesTableList.Length();
  }
  // List of glyph tables;
  nsTArray<nsPropertiesTable> mPropertiesTableList;
};

NS_IMPL_ISUPPORTS1(nsGlyphTableList, nsIObserver)

// -----------------------------------------------------------------------------
// Here is the global list of applicable glyph tables that we will be using
static nsGlyphTableList* gGlyphTableList = nullptr;

static bool gGlyphTableInitialized = false;

// XPCOM shutdown observer
NS_IMETHODIMP
nsGlyphTableList::Observe(nsISupports*     aSubject,
                          const char* aTopic,
                          const char16_t* someData)
{
  Finalize();
  return NS_OK;
}

// Add an observer to XPCOM shutdown so that we can free our data at shutdown
nsresult
nsGlyphTableList::Initialize()
{
  nsCOMPtr<nsIObserverService> obs = mozilla::services::GetObserverService();
  if (!obs)
    return NS_ERROR_FAILURE;

  nsresult rv = obs->AddObserver(this, NS_XPCOM_SHUTDOWN_OBSERVER_ID, false);
  NS_ENSURE_SUCCESS(rv, rv);

  return NS_OK;
}

// Remove our observer and free the memory that were allocated for us
nsresult
nsGlyphTableList::Finalize()
{
  // Remove our observer from the observer service
  nsresult rv = NS_OK;
  nsCOMPtr<nsIObserverService> obs = mozilla::services::GetObserverService();
  if (obs)
    rv = obs->RemoveObserver(this, NS_XPCOM_SHUTDOWN_OBSERVER_ID);
  else
    rv = NS_ERROR_FAILURE;

  gGlyphTableInitialized = false;
  // our oneself will be destroyed when our |Release| is called by the observer
  return rv;
}

nsGlyphTable*
nsGlyphTableList::AddGlyphTable(const nsString& aPrimaryFontName)
{
  // See if there is already a special table for this family.
  nsGlyphTable* glyphTable = GetGlyphTableFor(aPrimaryFontName);
  if (glyphTable != &mUnicodeTable)
    return glyphTable;

  // allocate a table
  glyphTable = mPropertiesTableList.AppendElement(aPrimaryFontName);
  return glyphTable;
}

nsGlyphTable*
nsGlyphTableList::GetGlyphTableFor(const nsAString& aFamily)
{
  for (int32_t i = 0; i < PropertiesTableCount(); i++) {
    nsPropertiesTable* glyphTable = PropertiesTableAt(i);
    const nsAString& fontName = glyphTable->PrimaryFontName();
    // TODO: would be nice to consider StripWhitespace and other aliasing
    if (fontName.Equals(aFamily, nsCaseInsensitiveStringComparator())) {
      return glyphTable;
    }
  }
  // Fall back to default Unicode table
  return &mUnicodeTable;
}

// -----------------------------------------------------------------------------

// Lookup the preferences:
// "font.mathfont-family.\uNNNN.base"     -- fonts for the base size
// "font.mathfont-family.\uNNNN.variants" -- fonts for larger glyphs
// "font.mathfont-family.\uNNNN.parts"    -- fonts for partial glyphs
// Given the char code and mode of stretch, retrieve the preferred extension
// font families.
static bool
GetFontExtensionPref(char16_t aChar,
                     nsMathfontPrefExtension aExtension, nsString& aValue)
{
  // initialize OUT param
  aValue.Truncate();

  // We are going to try two keys because some users specify their pref as 
  // user_pref("font.mathfont-family.\uNNNN.base", "...") rather than
  // user_pref("font.mathfont-family.\\uNNNN.base", "...").
  // The \uNNNN in the former is interpreted as an UTF16 escape sequence by
  // JavaScript and is converted to the internal UTF8 string that JavaScript
  // uses. 
  // But clueless users who are not savvy of JavaScript have no idea as to what 
  // is going on and are baffled as to why their pref setting is not working.
  // So to save countless explanations, we are going to support both keys.

  static const char* kMathFontPrefix = "font.mathfont-family.";

  nsAutoCString extension;
  switch (aExtension)
  {
    case eExtension_base:
      extension.AssignLiteral(".base");
      break;
    case eExtension_variants:
      extension.AssignLiteral(".variants");
      break;
    case eExtension_parts:
      extension.AssignLiteral(".parts");
      break;
    default:
      return false;
  }

  // .\\uNNNN key
  nsAutoCString key;
  key.AssignASCII(kMathFontPrefix);
  char ustr[10];
  PR_snprintf(ustr, sizeof(ustr), "\\u%04X", aChar);
  key.Append(ustr);
  key.Append(extension);
  // .\uNNNN key
  nsAutoCString alternateKey;
  alternateKey.AssignASCII(kMathFontPrefix);
  NS_ConvertUTF16toUTF8 tmp(&aChar, 1);
  alternateKey.Append(tmp);
  alternateKey.Append(extension);

  aValue = Preferences::GetString(key.get());
  if (aValue.IsEmpty()) {
    aValue = Preferences::GetString(alternateKey.get());
  }
  return !aValue.IsEmpty();
}


static bool
MathFontEnumCallback(const nsString& aFamily, bool aGeneric, void *aData)
{
  if (!gGlyphTableList->AddGlyphTable(aFamily))
    return false; // stop in low-memory situations
  return true; // don't stop
}

static nsresult
InitGlobals(nsPresContext* aPresContext)
{
  NS_ASSERTION(!gGlyphTableInitialized, "Error -- already initialized");
  gGlyphTableInitialized = true;

  // Allocate the placeholders for the preferred parts and variants
  nsresult rv = NS_ERROR_OUT_OF_MEMORY;
  gGlyphTableList = new nsGlyphTableList();
  if (gGlyphTableList) {
    rv = gGlyphTableList->Initialize();
  }
  if (NS_FAILED(rv)) {
    delete gGlyphTableList;
    gGlyphTableList = nullptr;
    return rv;
  }
  /*
  else
    The gGlyphTableList has been successfully registered as a shutdown observer.
    It will be deleted at shutdown, even if a failure happens below.
  */

  nsAutoCString key;
  nsAutoString value;
  nsCOMPtr<nsIPersistentProperties> mathfontProp;

  // Add the math fonts in the gGlyphTableList in order of preference ...
  // Note: we only load font-names at this stage. The actual glyph tables will
  // be loaded lazily (see nsGlyphTable::ElementAt()).

  // Load the "mathfont.properties" file
  value.Truncate();
  rv = LoadProperties(value, mathfontProp);
  if (NS_FAILED(rv)) return rv;

  // Get the list of mathfonts having special glyph tables to be used for
  // stretchy characters.
  // We just want to iterate over the font-family list using the
  // callback mechanism that nsFont has...
  nsFont font("", 0, 0, 0, 0, 0, 0);
  NS_NAMED_LITERAL_CSTRING(defaultKey, "font.mathfont-glyph-tables");
  rv = mathfontProp->GetStringProperty(defaultKey, font.name);
  if (NS_FAILED(rv)) return rv;

  // Parse the font list and append an entry for each family to gGlyphTableList
  nsAutoString missingFamilyList;

  font.EnumerateFamilies(MathFontEnumCallback, nullptr);
  return rv;
}

// -----------------------------------------------------------------------------
// And now the implementation of nsMathMLChar

nsMathMLChar::~nsMathMLChar()
{
  MOZ_COUNT_DTOR(nsMathMLChar);
  mStyleContext->Release();
}

nsStyleContext*
nsMathMLChar::GetStyleContext() const
{
  NS_ASSERTION(mStyleContext, "chars should always have style context");
  return mStyleContext;
}

void
nsMathMLChar::SetStyleContext(nsStyleContext* aStyleContext)
{
  NS_PRECONDITION(aStyleContext, "null ptr");
  if (aStyleContext != mStyleContext) {
    if (mStyleContext)
      mStyleContext->Release();
    if (aStyleContext) {
      mStyleContext = aStyleContext;
      aStyleContext->AddRef();
    }
  }
}

void
nsMathMLChar::SetData(nsPresContext* aPresContext,
                      nsString&       aData)
{
  if (!gGlyphTableInitialized) {
    InitGlobals(aPresContext);
  }
  mData = aData;
  // some assumptions until proven otherwise
  // note that mGlyph is not initialized
  mDirection = NS_STRETCH_DIRECTION_UNSUPPORTED;
  mBoundingMetrics = nsBoundingMetrics();
  // check if stretching is applicable ...
  if (gGlyphTableList && (1 == mData.Length())) {
    mDirection = nsMathMLOperators::GetStretchyDirection(mData);
    // default tentative table (not the one that is necessarily going
    // to be used)
  }
}

// -----------------------------------------------------------------------------
/*
 The Stretch:
 @param aContainerSize - suggested size for the stretched char
 @param aDesiredStretchSize - OUT parameter. The desired size
 after stretching. If no stretching is done, the output will
 simply give the base size.

 How it works?
 Summary:-
 The Stretch() method first looks for a glyph of appropriate
 size; If a glyph is found, it is cached by this object and
 its size is returned in aDesiredStretchSize. The cached
 glyph will then be used at the painting stage.
 If no glyph of appropriate size is found, a search is made
 to see if the char can be built by parts.

 Details:-
 A character gets stretched through the following pipeline :

 1) If the base size of the char is sufficient to cover the
    container' size, we use that. If not, it will still be
    used as a fallback if the other stages in the pipeline fail.
    Issues :
    a) The base size, the parts and the variants of a char can
       be in different fonts. For eg., the base size for '(' should
       come from a normal ascii font if CMEX10 is used, since CMEX10
       only contains the stretched versions. Hence, there are two
       style contexts in use throughout the process. The leaf style
       context of the char holds fonts with which to try to stretch
       the char. The parent style context of the char contains fonts
       for normal rendering. So the parent context is the one used
       to get the initial base size at the start of the pipeline.
    b) For operators that can be largeop's in display mode,
       we will skip the base size even if it fits, so that
       the next stage in the pipeline is given a chance to find
       a largeop variant. If the next stage fails, we fallback
       to the base size.

 2) We search for the first larger variant of the char that fits the
    container' size.  We first search for larger variants using the glyph
    table corresponding to the first existing font specified in the list of
    stretchy fonts held by the leaf style context (from -moz-math-stretchy in
    mathml.css).  Generic fonts are resolved by the preference
    "font.mathfont-family".
    Issues :
    a) the largeop and display settings determine the starting
       size when we do the above search, regardless of whether
       smaller variants already fit the container' size.
    b) if it is a largeopOnly request (i.e., a displaystyle operator
       with largeop=true and stretchy=false), we break after finding
       the first starting variant, regardless of whether that
       variant fits the container's size.

 3) If a variant of appropriate size wasn't found, we see if the char
    can be built by parts using the same glyph table.
    Issue:
       There are chars that have no middle and glue glyphs. For
       such chars, the parts need to be joined using the rule.
       By convention (TeXbook p.225), the descent of the parts is
       zero while their ascent gives the thickness of the rule that
       should be used to join them.

 4) If a match was not found in that glyph table, repeat from 2 to search the
    ordered list of stretchy fonts for the first font with a glyph table that
    provides a fit to the container size.  If no fit is found, the closest fit
    is used.

 Of note:
 When the pipeline completes successfully, the desired size of the
 stretched char can actually be slightly larger or smaller than
 aContainerSize. But it is the responsibility of the caller to
 account for the spacing when setting aContainerSize, and to leave
 any extra margin when placing the stretched char.
*/
// -----------------------------------------------------------------------------


// plain TeX settings (TeXbook p.152)
#define NS_MATHML_DELIMITER_FACTOR             0.901f
#define NS_MATHML_DELIMITER_SHORTFALL_POINTS   5.0f

static bool
IsSizeOK(nsPresContext* aPresContext, nscoord a, nscoord b, uint32_t aHint)
{
  // Normal: True if 'a' is around +/-10% of the target 'b' (10% is
  // 1-DelimiterFactor). This often gives a chance to the base size to
  // win, especially in the context of <mfenced> without tall elements
  // or in sloppy markups without protective <mrow></mrow>
  bool isNormal =
    (aHint & NS_STRETCH_NORMAL) &&
    Abs<float>(a - b) < (1.0f - NS_MATHML_DELIMITER_FACTOR) * float(b);

  // Nearer: True if 'a' is around max{ +/-10% of 'b' , 'b' - 5pt },
  // as documented in The TeXbook, Ch.17, p.152.
  // i.e. within 10% and within 5pt
  bool isNearer = false;
  if (aHint & (NS_STRETCH_NEARER | NS_STRETCH_LARGEOP)) {
    float c = std::max(float(b) * NS_MATHML_DELIMITER_FACTOR,
                     float(b) - nsPresContext::
                     CSSPointsToAppUnits(NS_MATHML_DELIMITER_SHORTFALL_POINTS));
    isNearer = Abs<float>(b - a) <= float(b) - c;
  }

  // Smaller: Mainly for transitory use, to compare two candidate
  // choices
  bool isSmaller =
    (aHint & NS_STRETCH_SMALLER) &&
    float(a) >= NS_MATHML_DELIMITER_FACTOR * float(b) &&
    a <= b;

  // Larger: Critical to the sqrt code to ensure that the radical
  // size is tall enough
  bool isLarger =
    (aHint & (NS_STRETCH_LARGER | NS_STRETCH_LARGEOP)) &&
    a >= b;

  return (isNormal || isSmaller || isNearer || isLarger);
}

static bool
IsSizeBetter(nscoord a, nscoord olda, nscoord b, uint32_t aHint)
{
  if (0 == olda)
    return true;
  if (aHint & (NS_STRETCH_LARGER | NS_STRETCH_LARGEOP))
    return (a >= olda) ? (olda < b) : (a >= b);
  if (aHint & NS_STRETCH_SMALLER)
    return (a <= olda) ? (olda > b) : (a <= b);

  // XXXkt prob want log scale here i.e. 1.5 is closer to 1 than 0.5
  return Abs(a - b) < Abs(olda - b);
}

// We want to place the glyphs even when they don't fit at their
// full extent, i.e., we may clip to tolerate a small amount of
// overlap between the parts. This is important to cater for fonts
// with long glues.
static nscoord
ComputeSizeFromParts(nsPresContext* aPresContext,
                     nsGlyphCode* aGlyphs,
                     nscoord*     aSizes,
                     nscoord      aTargetSize)
{
  enum {first, middle, last, glue};
  // Add the parts that cannot be left out.
  nscoord sum = 0;
  for (int32_t i = first; i <= last; i++) {
    if (aGlyphs[i] != aGlyphs[glue]) {
      sum += aSizes[i];
    }
  }

  // Determine how much is used in joins
  nscoord oneDevPixel = aPresContext->AppUnitsPerDevPixel();
  int32_t joins = aGlyphs[middle] == aGlyphs[glue] ? 1 : 2;

  // Pick a maximum size using a maximum number of glue glyphs that we are
  // prepared to draw for one character.
  const int32_t maxGlyphs = 1000;

  // This also takes into account the fact that, if the glue has no size,
  // then the character can't be lengthened.
  nscoord maxSize = sum - 2 * joins * oneDevPixel + maxGlyphs * aSizes[glue];
  if (maxSize < aTargetSize)
    return maxSize; // settle with the maximum size

  // Get the minimum allowable size using some flex.
  nscoord minSize = NSToCoordRound(NS_MATHML_DELIMITER_FACTOR * sum);

  if (minSize > aTargetSize)
    return minSize; // settle with the minimum size

  // Fill-up the target area
  return aTargetSize;
}

// Insert aFallbackFamilies before the first generic family in or at the end
// of a CSS aFontName.
static void
AddFallbackFonts(nsAString& aFontName, const nsAString& aFallbackFamilies)
{
  if (aFallbackFamilies.IsEmpty())
    return;

  if (aFontName.IsEmpty()) {
    return;
  }

  static const char16_t kSingleQuote  = char16_t('\'');
  static const char16_t kDoubleQuote  = char16_t('\"');
  static const char16_t kComma        = char16_t(',');

  const char16_t *p_begin, *p_end;
  aFontName.BeginReading(p_begin);
  aFontName.EndReading(p_end);

  const char16_t *p = p_begin;
  const char16_t *p_name = nullptr;
  while (p < p_end) {
    while (nsCRT::IsAsciiSpace(*p))
      if (++p == p_end)
        goto insert;

    p_name = p;
    if (*p == kSingleQuote || *p == kDoubleQuote) {
      // quoted font family
      char16_t quoteMark = *p;
      if (++p == p_end)
        goto insert;

      // XXX What about CSS character escapes?
      while (*p != quoteMark)
        if (++p == p_end)
          goto insert;

      while (++p != p_end && *p != kComma)
        /* nothing */ ;

    } else {
      // unquoted font family
      const char16_t *nameStart = p;
      while (++p != p_end && *p != kComma)
        /* nothing */ ;

      nsAutoString family;
      family = Substring(nameStart, p);
      family.CompressWhitespace(false, true);

      uint8_t generic;
      nsFont::GetGenericID(family, &generic);
      if (generic != kGenericFont_NONE)
        goto insert;
    }

    ++p; // may advance past p_end
  }

  aFontName.Append(NS_LITERAL_STRING(",") + aFallbackFamilies);
  return;

insert:
  if (p_name) {
    aFontName.Insert(aFallbackFamilies + NS_LITERAL_STRING(","),
                     p_name - p_begin);
  }
  else { // whitespace or empty
    aFontName = aFallbackFamilies;
  }
}

// Update the font if there is a family change and returns the font group.
bool
nsMathMLChar::SetFontFamily(nsPresContext*          aPresContext,
                            const nsGlyphTable*     aGlyphTable,
                            const nsGlyphCode&      aGlyphCode,
                            const nsAString&        aDefaultFamily,
                            nsFont&                 aFont,
                            nsRefPtr<gfxFontGroup>* aFontGroup)
{
  const nsAString& family =
    aGlyphCode.font ? aGlyphTable->FontNameFor(aGlyphCode) : aDefaultFamily;
  if (!*aFontGroup || !family.Equals(aFont.name)) {
    nsFont font = aFont;
    font.name = family;
    nsRefPtr<nsFontMetrics> fm;
    aPresContext->DeviceContext()->
      GetMetricsFor(font,
                    mStyleContext->StyleFont()->mLanguage,
                    aPresContext->GetUserFontSet(),
                    aPresContext->GetTextPerfMetrics(),
                    *getter_AddRefs(fm));
    // Set the font if it is an unicode table
    // or if the same family name has been found
    if (aGlyphTable == &gGlyphTableList->mUnicodeTable ||
        fm->GetThebesFontGroup()->GetFontAt(0)->GetFontEntry()->
        FamilyName() == family) {
      aFont.name = family;
      *aFontGroup = fm->GetThebesFontGroup();
    } else {
      return false; // We did not set the font
    }
  }
  return true;
}

static nsBoundingMetrics
MeasureTextRun(gfxContext* aThebesContext, gfxTextRun* aTextRun)
{
  gfxTextRun::Metrics metrics =
    aTextRun->MeasureText(0, aTextRun->GetLength(),
                          gfxFont::TIGHT_HINTED_OUTLINE_EXTENTS,
                          aThebesContext, nullptr);

  nsBoundingMetrics bm;
  bm.leftBearing = NSToCoordFloor(metrics.mBoundingBox.X());
  bm.rightBearing = NSToCoordCeil(metrics.mBoundingBox.XMost());
  bm.ascent = NSToCoordCeil(-metrics.mBoundingBox.Y());
  bm.descent = NSToCoordCeil(metrics.mBoundingBox.YMost());
  bm.width = NSToCoordRound(metrics.mAdvanceWidth);

  return bm;
}

class nsMathMLChar::StretchEnumContext {
public:
  StretchEnumContext(nsMathMLChar*        aChar,
                     nsPresContext*       aPresContext,
                     gfxContext*          aThebesContext,
                     nsStretchDirection   aStretchDirection,
                     nscoord              aTargetSize,
                     uint32_t             aStretchHint,
                     nsBoundingMetrics&   aStretchedMetrics,
                     const nsAString&     aFamilies,
                     bool&              aGlyphFound)
    : mChar(aChar),
      mPresContext(aPresContext),
      mThebesContext(aThebesContext),
      mDirection(aStretchDirection),
      mTargetSize(aTargetSize),
      mStretchHint(aStretchHint),
      mBoundingMetrics(aStretchedMetrics),
      mFamilies(aFamilies),
      mTryVariants(true),
      mTryParts(true),
      mGlyphFound(aGlyphFound) {}

  static bool
  EnumCallback(const nsString& aFamily, bool aGeneric, void *aData);

private:
  bool TryVariants(nsGlyphTable* aGlyphTable,
                   nsRefPtr<gfxFontGroup>* aFontGroup,
                   const nsAString& aFamily);
  bool TryParts(nsGlyphTable* aGlyphTable,
                nsRefPtr<gfxFontGroup>* aFontGroup,
                const nsAString& aFamily);

  nsMathMLChar* mChar;
  nsPresContext* mPresContext;
  gfxContext* mThebesContext;
  const nsStretchDirection mDirection;
  const nscoord mTargetSize;
  const uint32_t mStretchHint;
  nsBoundingMetrics& mBoundingMetrics;
  // Font families to search
  const nsAString& mFamilies;

public:
  bool mTryVariants;
  bool mTryParts;

private:
  nsAutoTArray<nsGlyphTable*,16> mTablesTried;
  bool&       mGlyphFound;
};


// 2. See if there are any glyphs of the appropriate size.
// Returns true if the size is OK, false to keep searching.
// Always updates the char if a better match is found.
bool
nsMathMLChar::
StretchEnumContext::TryVariants(nsGlyphTable* aGlyphTable,
                                nsRefPtr<gfxFontGroup>* aFontGroup,
                                const nsAString& aFamily)
{
  // Use our stretchy style context now that stretching is in progress
  nsStyleContext *sc = mChar->mStyleContext;
  nsFont font = sc->StyleFont()->mFont;

  bool isVertical = (mDirection == NS_STRETCH_DIRECTION_VERTICAL);
  nscoord oneDevPixel = mPresContext->AppUnitsPerDevPixel();
  char16_t uchar = mChar->mData[0];
  bool largeop = (NS_STRETCH_LARGEOP & mStretchHint) != 0;
  bool largeopOnly =
    largeop && (NS_STRETCH_VARIABLE_MASK & mStretchHint) == 0;
  bool maxWidth = (NS_STRETCH_MAXWIDTH & mStretchHint) != 0;

  nscoord bestSize =
    isVertical ? mBoundingMetrics.ascent + mBoundingMetrics.descent
               : mBoundingMetrics.rightBearing - mBoundingMetrics.leftBearing;
  bool haveBetter = false;

  // start at size = 1 (size = 0 is the char at its normal size)
  int32_t size = 1;
  nsGlyphCode ch;
  nscoord displayOperatorMinHeight = 0;
  if (largeopOnly) {
    NS_ASSERTION(isVertical, "Stretching should be in the vertical direction");
    ch = aGlyphTable->BigOf(mThebesContext, oneDevPixel, *aFontGroup, uchar,
                            isVertical, 0);
    if (ch.IsGlyphID()) {
      gfxFont* mathFont = aFontGroup->get()->GetFontAt(0);
      // For OpenType MATH fonts, we will rely on the DisplayOperatorMinHeight
      // to select the right size variant. Note that the value is sometimes too
      // small so we use kLargeOpFactor/kIntegralFactor as a minimum value.
      displayOperatorMinHeight =
        NSToCoordRound(mathFont->GetFontEntry()->
                       GetMathConstant(gfxFontEntry::DisplayOperatorMinHeight) *
                       mathFont->GetAdjustedSize() * oneDevPixel);
      nsAutoPtr<gfxTextRun> textRun;
      textRun = aGlyphTable->MakeTextRun(mThebesContext, oneDevPixel,
                                         *aFontGroup, ch);
      nsBoundingMetrics bm = MeasureTextRun(mThebesContext, textRun);
      float largeopFactor = kLargeOpFactor;
      if (NS_STRETCH_INTEGRAL & mStretchHint) {
        // integrals are drawn taller
        largeopFactor = kIntegralFactor;
      }
      nscoord minHeight = largeopFactor * (bm.ascent + bm.descent);
      if (displayOperatorMinHeight < minHeight) {
        displayOperatorMinHeight = minHeight;
      }
    }
  }
#ifdef NOISY_SEARCH
  printf("  searching in %s ...\n",
           NS_LossyConvertUTF16toASCII(aFamily).get());
#endif
  while ((ch = aGlyphTable->BigOf(mThebesContext, oneDevPixel, *aFontGroup,
                                  uchar, isVertical, size)).Exists()) {

    if (!mChar->SetFontFamily(mPresContext, aGlyphTable, ch, aFamily, font,
                              aFontGroup)) {
      // if largeopOnly is set, break now
      if (largeopOnly) break;
      ++size;
      continue;
    }

    nsAutoPtr<gfxTextRun> textRun;
    textRun = aGlyphTable->MakeTextRun(mThebesContext, oneDevPixel,
                                       *aFontGroup, ch);
    nsBoundingMetrics bm = MeasureTextRun(mThebesContext, textRun);
    if (ch.IsGlyphID()) {
      gfxFont* mathFont = aFontGroup->get()->GetFontAt(0);
      if (mathFont->GetFontEntry()->TryGetMathTable(mathFont)) {
        // MeasureTextRun should have set the advance width to the right
        // bearing for OpenType MATH fonts. We now subtract the italic
        // correction, so that nsMathMLmmultiscripts will place the scripts
        // correctly.
        // Note that STIX-Word does not provide italic corrections but its
        // advance widths do not match right bearings.
        // (http://sourceforge.net/p/stixfonts/tracking/50/)
        gfxFloat italicCorrection;
        if (mathFont->GetFontEntry()->
            GetMathItalicsCorrection(ch.glyphID, &italicCorrection)) {
          bm.width -=
            NSToCoordRound(italicCorrection *
                           mathFont->GetAdjustedSize() * oneDevPixel);
          if (bm.width < 0) {
            bm.width = 0;
          }
        }
      }
    }

    nscoord charSize =
      isVertical ? bm.ascent + bm.descent
      : bm.rightBearing - bm.leftBearing;

    if (largeopOnly ||
        IsSizeBetter(charSize, bestSize, mTargetSize, mStretchHint)) {
      mGlyphFound = true;
      if (maxWidth) {
        // IsSizeBetter() checked that charSize < maxsize;
        // Leave ascent, descent, and bestsize as these contain maxsize.
        if (mBoundingMetrics.width < bm.width)
          mBoundingMetrics.width = bm.width;
        if (mBoundingMetrics.leftBearing > bm.leftBearing)
          mBoundingMetrics.leftBearing = bm.leftBearing;
        if (mBoundingMetrics.rightBearing < bm.rightBearing)
          mBoundingMetrics.rightBearing = bm.rightBearing;
        // Continue to check other sizes unless largeopOnly
        haveBetter = largeopOnly;
      }
      else {
        mBoundingMetrics = bm;
        haveBetter = true;
        bestSize = charSize;
        mChar->mGlyphs[0] = textRun;
        mChar->mDraw = DRAW_VARIANT;
      }
#ifdef NOISY_SEARCH
      printf("    size:%d Current best\n", size);
#endif
    }
    else {
#ifdef NOISY_SEARCH
      printf("    size:%d Rejected!\n", size);
#endif
      if (haveBetter)
        break; // Not making an futher progress, stop searching
    }

    // If this a largeop only operator, we stop if the glyph is large enough.
    if (largeopOnly && (bm.ascent + bm.descent) >= displayOperatorMinHeight) {
      break;
    }
    ++size;
  }

  return haveBetter &&
    (largeopOnly ||
     IsSizeOK(mPresContext, bestSize, mTargetSize, mStretchHint));
}

// 3. Build by parts.
// Returns true if the size is OK, false to keep searching.
// Always updates the char if a better match is found.
bool
nsMathMLChar::StretchEnumContext::TryParts(nsGlyphTable* aGlyphTable,
                                           nsRefPtr<gfxFontGroup>* aFontGroup,
                                           const nsAString& aFamily)
{
  // Use our stretchy style context now that stretching is in progress
  nsFont font = mChar->mStyleContext->StyleFont()->mFont;

  // Compute the bounding metrics of all partial glyphs
  nsAutoPtr<gfxTextRun> textRun[4];
  nsGlyphCode chdata[4];
  nsBoundingMetrics bmdata[4];
  nscoord sizedata[4];

  bool isVertical = (mDirection == NS_STRETCH_DIRECTION_VERTICAL);
  nscoord oneDevPixel = mPresContext->AppUnitsPerDevPixel();
  char16_t uchar = mChar->mData[0];
  bool maxWidth = (NS_STRETCH_MAXWIDTH & mStretchHint) != 0;
  if (!aGlyphTable->HasPartsOf(mThebesContext, oneDevPixel, *aFontGroup,
                               uchar, isVertical))
    return false; // to next table

  for (int32_t i = 0; i < 4; i++) {
    nsGlyphCode ch = aGlyphTable->ElementAt(mThebesContext, oneDevPixel,
                                            *aFontGroup, uchar, isVertical, i);
    chdata[i] = ch;
    if (ch.Exists()) {
      if (!mChar->SetFontFamily(mPresContext, aGlyphTable, ch, aFamily, font,
                                aFontGroup))
        return false;

      textRun[i] = aGlyphTable->MakeTextRun(mThebesContext, oneDevPixel,
                                            *aFontGroup, ch);
      nsBoundingMetrics bm = MeasureTextRun(mThebesContext, textRun[i]);

      // TODO: For the generic Unicode table, ideally we should check that the
      // glyphs are actually found and that they each come from the same
      // font.
      bmdata[i] = bm;
      sizedata[i] = isVertical ? bm.ascent + bm.descent
                               : bm.rightBearing - bm.leftBearing;
    } else {
      // Null glue indicates that a rule will be drawn, which can stretch to
      // fill any space.
      textRun[i] = nullptr;
      bmdata[i] = nsBoundingMetrics();
      sizedata[i] = i == 3 ? mTargetSize : 0;
    }
  }

  // Build by parts if we have successfully computed the
  // bounding metrics of all parts.
  nscoord computedSize = ComputeSizeFromParts(mPresContext, chdata, sizedata,
                                              mTargetSize);

  nscoord currentSize =
    isVertical ? mBoundingMetrics.ascent + mBoundingMetrics.descent
               : mBoundingMetrics.rightBearing - mBoundingMetrics.leftBearing;

  if (!IsSizeBetter(computedSize, currentSize, mTargetSize, mStretchHint)) {
#ifdef NOISY_SEARCH
    printf("    Font %s Rejected!\n",
           NS_LossyConvertUTF16toASCII(fontName).get());
#endif
    return false; // to next table
  }

#ifdef NOISY_SEARCH
  printf("    Font %s Current best!\n",
         NS_LossyConvertUTF16toASCII(fontName).get());
#endif

  // The computed size is the best we have found so far...
  // now is the time to compute and cache our bounding metrics
  if (isVertical) {
    int32_t i;
    // Try and find the first existing part and then determine the extremal
    // horizontal metrics of the parts.
    for (i = 0; i <= 3 && !textRun[i]; i++);
    if (i == 4) {
      NS_ERROR("Cannot stretch - All parts missing");
      return false;
    }
    nscoord lbearing = bmdata[i].leftBearing;
    nscoord rbearing = bmdata[i].rightBearing;
    nscoord width = bmdata[i].width;
    i++;
    for (; i <= 3; i++) {
      if (!textRun[i]) continue;
      lbearing = std::min(lbearing, bmdata[i].leftBearing);
      rbearing = std::max(rbearing, bmdata[i].rightBearing);
      width = std::max(width, bmdata[i].width);
    }
    if (maxWidth) {
      lbearing = std::min(lbearing, mBoundingMetrics.leftBearing);
      rbearing = std::max(rbearing, mBoundingMetrics.rightBearing);
      width = std::max(width, mBoundingMetrics.width);
    }
    mBoundingMetrics.width = width;
    // When maxWidth, updating ascent and descent indicates that no characters
    // larger than this character's minimum size need to be checked as they
    // will not be used.
    mBoundingMetrics.ascent = bmdata[0].ascent; // not used except with descent
                                                // for height
    mBoundingMetrics.descent = computedSize - mBoundingMetrics.ascent;
    mBoundingMetrics.leftBearing = lbearing;
    mBoundingMetrics.rightBearing = rbearing;
  }
  else {
    int32_t i;
    // Try and find the first existing part and then determine the extremal
    // vertical metrics of the parts.
    for (i = 0; i <= 3 && !textRun[i]; i++);
    if (i == 4) {
      NS_ERROR("Cannot stretch - All parts missing");
      return false;
    }
    nscoord ascent = bmdata[i].ascent;
    nscoord descent = bmdata[i].descent;
    i++;
    for (; i <= 3; i++) {
      if (!textRun[i]) continue;
      ascent = std::max(ascent, bmdata[i].ascent);
      descent = std::max(descent, bmdata[i].descent);
    }
    mBoundingMetrics.width = computedSize;
    mBoundingMetrics.ascent = ascent;
    mBoundingMetrics.descent = descent;
    mBoundingMetrics.leftBearing = 0;
    mBoundingMetrics.rightBearing = computedSize;
  }
  mGlyphFound = true;
  if (maxWidth)
    return false; // Continue to check other sizes

  // reset
  mChar->mDraw = DRAW_PARTS;
  for (int32_t i = 0; i < 4; i++) {
    mChar->mGlyphs[i] = textRun[i];
    mChar->mBmData[i] = bmdata[i];
  }

  return IsSizeOK(mPresContext, computedSize, mTargetSize, mStretchHint);
}

// This is called for each family, whether it exists or not
bool
nsMathMLChar::StretchEnumContext::EnumCallback(const nsString& aFamily,
                                               bool aGeneric, void *aData)
{
  StretchEnumContext* context = static_cast<StretchEnumContext*>(aData);

  // Check font family if it is not a generic one
  // We test with the kNullGlyph
  nsStyleContext *sc = context->mChar->mStyleContext;
  nsFont font = sc->StyleFont()->mFont;
  nsRefPtr<gfxFontGroup> fontGroup;
  if (!aGeneric && !context->mChar->SetFontFamily(context->mPresContext,
                                                  nullptr, kNullGlyph, aFamily,
                                                  font, &fontGroup))
     return true; // Could not set the family

  // Determine the glyph table to use for this font.
  nsAutoPtr<nsOpenTypeTable> openTypeTable;
  nsGlyphTable* glyphTable;
  if (aGeneric) {
    // This is a generic font, use the Unicode table.
    glyphTable = &gGlyphTableList->mUnicodeTable;
  } else {
    // If the font contains an Open Type MATH table, use it.
    openTypeTable = nsOpenTypeTable::Create(fontGroup->GetFontAt(0));
    if (openTypeTable) {
      glyphTable = openTypeTable;
    } else {
      // Otherwise try to find a .properties file corresponding to that font
      // family or fallback to the Unicode table.
      glyphTable = gGlyphTableList->GetGlyphTableFor(aFamily);
    }
  }

  if (!openTypeTable) {
    if (context->mTablesTried.Contains(glyphTable))
      return true; // already tried this one
    
    // Only try this table once.
    context->mTablesTried.AppendElement(glyphTable);
  }

  // If the unicode table is being used, then search all font families.  If a
  // special table is being used then the font in this family should have the
  // specified glyphs.
  const nsAString& family = glyphTable == &gGlyphTableList->mUnicodeTable ?
    context->mFamilies : aFamily;

  if((context->mTryVariants &&
      context->TryVariants(glyphTable, &fontGroup, family)) ||
     (context->mTryParts && context->TryParts(glyphTable, &fontGroup, family)))
    return false; // no need to continue

  return true; // true means continue
}

nsresult
nsMathMLChar::StretchInternal(nsPresContext*           aPresContext,
                              gfxContext*              aThebesContext,
                              nsStretchDirection&      aStretchDirection,
                              const nsBoundingMetrics& aContainerSize,
                              nsBoundingMetrics&       aDesiredStretchSize,
                              uint32_t                 aStretchHint,
                              // These are currently only used when
                              // aStretchHint & NS_STRETCH_MAXWIDTH:
                              float                    aMaxSize,
                              bool                     aMaxSizeIsAbsolute)
{
  // if we have been called before, and we didn't actually stretch, our
  // direction may have been set to NS_STRETCH_DIRECTION_UNSUPPORTED.
  // So first set our direction back to its instrinsic value
  nsStretchDirection direction = nsMathMLOperators::GetStretchyDirection(mData);

  // Set default font and get the default bounding metrics
  // mStyleContext is a leaf context used only when stretching happens.
  // For the base size, the default font should come from the parent context
  nsFont font = mStyleContext->GetParent()->StyleFont()->mFont;

  // Override with specific fonts if applicable for this character
  nsAutoString families;
  if (GetFontExtensionPref(mData[0], eExtension_base, families)) {
    font.name = families;
  }

  // Don't modify this nsMathMLChar when doing GetMaxWidth()
  bool maxWidth = (NS_STRETCH_MAXWIDTH & aStretchHint) != 0;

  nsRefPtr<nsFontMetrics> fm;
  aPresContext->DeviceContext()->
    GetMetricsFor(font,
                  mStyleContext->StyleFont()->mLanguage,
                  aPresContext->GetUserFontSet(),
                  aPresContext->GetTextPerfMetrics(),
                  *getter_AddRefs(fm));
  uint32_t len = uint32_t(mData.Length());
  nsAutoPtr<gfxTextRun> textRun;
  textRun = fm->GetThebesFontGroup()->
    MakeTextRun(static_cast<const char16_t*>(mData.get()), len, aThebesContext,
                aPresContext->AppUnitsPerDevPixel(), 0);
  aDesiredStretchSize = MeasureTextRun(aThebesContext, textRun);
  mGlyphs[0] = textRun;

  if (!maxWidth) {
    mUnscaledAscent = aDesiredStretchSize.ascent;
  }

  //////////////////////////////////////////////////////////////////////////////
  // 1. Check the common situations where stretching is not actually needed
  //////////////////////////////////////////////////////////////////////////////

  // quick return if there is nothing special about this char
  if ((aStretchDirection != direction &&
       aStretchDirection != NS_STRETCH_DIRECTION_DEFAULT) ||
      (aStretchHint & ~NS_STRETCH_MAXWIDTH) == NS_STRETCH_NONE) {
    mDirection = NS_STRETCH_DIRECTION_UNSUPPORTED;
    return NS_OK;
  }

  // if no specified direction, attempt to stretch in our preferred direction
  if (aStretchDirection == NS_STRETCH_DIRECTION_DEFAULT) {
    aStretchDirection = direction;
  }

  // see if this is a particular largeop or largeopOnly request
  bool largeop = (NS_STRETCH_LARGEOP & aStretchHint) != 0;
  bool stretchy = (NS_STRETCH_VARIABLE_MASK & aStretchHint) != 0;
  bool largeopOnly = largeop && !stretchy;

  bool isVertical = (direction == NS_STRETCH_DIRECTION_VERTICAL);

  nscoord targetSize =
    isVertical ? aContainerSize.ascent + aContainerSize.descent
    : aContainerSize.rightBearing - aContainerSize.leftBearing;

  if (maxWidth) {
    // See if it is only necessary to consider glyphs up to some maximum size.
    // Set the current height to the maximum size, and set aStretchHint to
    // NS_STRETCH_SMALLER if the size is variable, so that only smaller sizes
    // are considered.  targetSize from GetMaxWidth() is 0.
    if (stretchy) {
      // variable size stretch - consider all sizes < maxsize
      aStretchHint =
        (aStretchHint & ~NS_STRETCH_VARIABLE_MASK) | NS_STRETCH_SMALLER;
    }

    // Use NS_MATHML_DELIMITER_FACTOR to allow some slightly larger glyphs as
    // maxsize is not enforced exactly.
    if (aMaxSize == NS_MATHML_OPERATOR_SIZE_INFINITY) {
      aDesiredStretchSize.ascent = nscoord_MAX;
      aDesiredStretchSize.descent = 0;
    }
    else {
      nscoord height = aDesiredStretchSize.ascent + aDesiredStretchSize.descent;
      if (height == 0) {
        if (aMaxSizeIsAbsolute) {
          aDesiredStretchSize.ascent =
            NSToCoordRound(aMaxSize / NS_MATHML_DELIMITER_FACTOR);
          aDesiredStretchSize.descent = 0;
        }
        // else: leave height as 0
      }
      else {
        float scale = aMaxSizeIsAbsolute ? aMaxSize / height : aMaxSize;
        scale /= NS_MATHML_DELIMITER_FACTOR;
        aDesiredStretchSize.ascent =
          NSToCoordRound(scale * aDesiredStretchSize.ascent);
        aDesiredStretchSize.descent =
          NSToCoordRound(scale * aDesiredStretchSize.descent);
      }
    }
  }

  nsBoundingMetrics initialSize = aDesiredStretchSize;
  nscoord charSize =
    isVertical ? initialSize.ascent + initialSize.descent
    : initialSize.rightBearing - initialSize.leftBearing;

  bool done = false;

  if (!maxWidth && !largeop) {
    // Doing Stretch() not GetMaxWidth(),
    // and not a largeop in display mode; we're done if size fits
    if ((targetSize <= 0) || 
        ((isVertical && charSize >= targetSize) ||
         IsSizeOK(aPresContext, charSize, targetSize, aStretchHint)))
      done = true;
  }

  //////////////////////////////////////////////////////////////////////////////
  // 2/3. Search for a glyph or set of part glyphs of appropriate size
  //////////////////////////////////////////////////////////////////////////////

  bool glyphFound = false;
  nsAutoString cssFamilies;

  if (!done) {
    font = mStyleContext->StyleFont()->mFont;
    cssFamilies = font.name;
  }

  // See if there are preferred fonts for the variants of this char
  if (!done && GetFontExtensionPref(mData[0], eExtension_variants, families)) {
    font.name = families;

    StretchEnumContext enumData(this, aPresContext, aThebesContext,
                                aStretchDirection, targetSize, aStretchHint,
                                aDesiredStretchSize, font.name, glyphFound);
    enumData.mTryParts = false;

    done = !font.EnumerateFamilies(StretchEnumContext::EnumCallback, &enumData);
  }

  // See if there are preferred fonts for the parts of this char
  if (!done && !largeopOnly
      && GetFontExtensionPref(mData[0], eExtension_parts, families)) {
    font.name = families;

    StretchEnumContext enumData(this, aPresContext, aThebesContext,
                                aStretchDirection, targetSize, aStretchHint,
                                aDesiredStretchSize, font.name, glyphFound);
    enumData.mTryVariants = false;

    done = !font.EnumerateFamilies(StretchEnumContext::EnumCallback, &enumData);
  }

  if (!done) { // normal case
    // Use the css font-family but add preferred fallback fonts.
    font.name = cssFamilies;
    NS_NAMED_LITERAL_CSTRING(defaultKey, "font.mathfont-family");
    nsAdoptingString fallbackFonts = Preferences::GetString(defaultKey.get());
    if (!fallbackFonts.IsEmpty()) {
      AddFallbackFonts(font.name, fallbackFonts);
    }

#ifdef NOISY_SEARCH
    printf("Searching in "%s" for a glyph of appropriate size for: 0x%04X:%c\n",
           font.name, mData[0], mData[0]&0x00FF);
#endif
    StretchEnumContext enumData(this, aPresContext, aThebesContext,
                                aStretchDirection, targetSize, aStretchHint,
                                aDesiredStretchSize, font.name, glyphFound);
    enumData.mTryParts = !largeopOnly;

    font.EnumerateFamilies(StretchEnumContext::EnumCallback, &enumData);
  }

  if (!maxWidth) {
    // Now, we know how we are going to draw the char. Update the member
    // variables accordingly.
    mUnscaledAscent = aDesiredStretchSize.ascent;
  }
    
  if (glyphFound) {
    return NS_OK;
  }

  // stretchy character
  if (stretchy) {
    if (isVertical) {
      float scale =
        float(aContainerSize.ascent + aContainerSize.descent) /
        (aDesiredStretchSize.ascent + aDesiredStretchSize.descent);
      if (!largeop || scale > 1.0) {
        // make the character match the desired height.
        if (!maxWidth) {
          mScaleY *= scale;
        }
        aDesiredStretchSize.ascent *= scale;
        aDesiredStretchSize.descent *= scale;
      }
    } else {
      float scale =
        float(aContainerSize.rightBearing - aContainerSize.leftBearing) /
        (aDesiredStretchSize.rightBearing - aDesiredStretchSize.leftBearing);
      if (!largeop || scale > 1.0) {
        // make the character match the desired width.
        if (!maxWidth) {
          mScaleX *= scale;
        }
        aDesiredStretchSize.leftBearing *= scale;
        aDesiredStretchSize.rightBearing *= scale;
        aDesiredStretchSize.width *= scale;
      }
    }
  }

  // We do not have a char variant for this largeop in display mode, so we
  // apply a scale transform to the base char.
  if (largeop) {
    float scale;
    float largeopFactor = kLargeOpFactor;

    // increase the width if it is not largeopFactor times larger
    // than the initial one.
    if ((aDesiredStretchSize.rightBearing - aDesiredStretchSize.leftBearing) <
        largeopFactor * (initialSize.rightBearing - initialSize.leftBearing)) {
      scale = (largeopFactor *
               (initialSize.rightBearing - initialSize.leftBearing)) /
        (aDesiredStretchSize.rightBearing - aDesiredStretchSize.leftBearing);
      if (!maxWidth) {
        mScaleX *= scale;
      }
      aDesiredStretchSize.leftBearing *= scale;
      aDesiredStretchSize.rightBearing *= scale;
      aDesiredStretchSize.width *= scale;
    }

    // increase the height if it is not largeopFactor times larger
    // than the initial one.
    if (NS_STRETCH_INTEGRAL & aStretchHint) {
      // integrals are drawn taller
      largeopFactor = kIntegralFactor;
    }
    if ((aDesiredStretchSize.ascent + aDesiredStretchSize.descent) <
        largeopFactor * (initialSize.ascent + initialSize.descent)) {
      scale = (largeopFactor *
               (initialSize.ascent + initialSize.descent)) /
        (aDesiredStretchSize.ascent + aDesiredStretchSize.descent);
      if (!maxWidth) {
        mScaleY *= scale;
      }
      aDesiredStretchSize.ascent *= scale;
      aDesiredStretchSize.descent *= scale;
    }
  }

  return NS_OK;
}

nsresult
nsMathMLChar::Stretch(nsPresContext*           aPresContext,
                      nsRenderingContext&     aRenderingContext,
                      nsStretchDirection       aStretchDirection,
                      const nsBoundingMetrics& aContainerSize,
                      nsBoundingMetrics&       aDesiredStretchSize,
                      uint32_t                 aStretchHint,
                      bool                     aRTL)
{
  NS_ASSERTION(!(aStretchHint &
                 ~(NS_STRETCH_VARIABLE_MASK | NS_STRETCH_LARGEOP |
                   NS_STRETCH_INTEGRAL)),
               "Unexpected stretch flags");

  mDraw = DRAW_NORMAL;
  mMirrored = aRTL && nsMathMLOperators::IsMirrorableOperator(mData);
  mScaleY = mScaleX = 1.0;
  mDirection = aStretchDirection;
  nsresult rv =
    StretchInternal(aPresContext, aRenderingContext.ThebesContext(), mDirection,
                    aContainerSize, aDesiredStretchSize, aStretchHint);

  // Record the metrics
  mBoundingMetrics = aDesiredStretchSize;

  return rv;
}

// What happens here is that the StretchInternal algorithm is used but
// modified by passing the NS_STRETCH_MAXWIDTH stretch hint.  That causes
// StretchInternal to return horizontal bounding metrics that are the maximum
// that might be returned from a Stretch.
//
// In order to avoid considering widths of some characters in fonts that will
// not be used for any stretch size, StretchInternal sets the initial height
// to infinity and looks for any characters smaller than this height.  When a
// character built from parts is considered, (it will be used by Stretch for
// any characters greater than its minimum size, so) the height is set to its
// minimum size, so that only widths of smaller subsequent characters are
// considered.
nscoord
nsMathMLChar::GetMaxWidth(nsPresContext* aPresContext,
                          nsRenderingContext& aRenderingContext,
                          uint32_t aStretchHint,
                          float aMaxSize, bool aMaxSizeIsAbsolute)
{
  nsBoundingMetrics bm;
  nsStretchDirection direction = NS_STRETCH_DIRECTION_VERTICAL;
  const nsBoundingMetrics container; // zero target size

  StretchInternal(aPresContext, aRenderingContext.ThebesContext(), direction,
                  container, bm, aStretchHint | NS_STRETCH_MAXWIDTH);

  return std::max(bm.width, bm.rightBearing) - std::min(0, bm.leftBearing);
}

class nsDisplayMathMLSelectionRect : public nsDisplayItem {
public:
  nsDisplayMathMLSelectionRect(nsDisplayListBuilder* aBuilder,
                               nsIFrame* aFrame, const nsRect& aRect)
    : nsDisplayItem(aBuilder, aFrame), mRect(aRect) {
    MOZ_COUNT_CTOR(nsDisplayMathMLSelectionRect);
  }
#ifdef NS_BUILD_REFCNT_LOGGING
  virtual ~nsDisplayMathMLSelectionRect() {
    MOZ_COUNT_DTOR(nsDisplayMathMLSelectionRect);
  }
#endif

  virtual void Paint(nsDisplayListBuilder* aBuilder,
                     nsRenderingContext* aCtx);
  NS_DISPLAY_DECL_NAME("MathMLSelectionRect", TYPE_MATHML_SELECTION_RECT)
private:
  nsRect    mRect;
};

void nsDisplayMathMLSelectionRect::Paint(nsDisplayListBuilder* aBuilder,
                                         nsRenderingContext* aCtx)
{
  // get color to use for selection from the look&feel object
  nscolor bgColor =
    LookAndFeel::GetColor(LookAndFeel::eColorID_TextSelectBackground,
                          NS_RGB(0, 0, 0));
  aCtx->SetColor(bgColor);
  aCtx->FillRect(mRect + ToReferenceFrame());
}

class nsDisplayMathMLCharBackground : public nsDisplayItem {
public:
  nsDisplayMathMLCharBackground(nsDisplayListBuilder* aBuilder,
                                nsIFrame* aFrame, const nsRect& aRect,
                                nsStyleContext* aStyleContext)
    : nsDisplayItem(aBuilder, aFrame), mStyleContext(aStyleContext),
      mRect(aRect) {
    MOZ_COUNT_CTOR(nsDisplayMathMLCharBackground);
  }
#ifdef NS_BUILD_REFCNT_LOGGING
  virtual ~nsDisplayMathMLCharBackground() {
    MOZ_COUNT_DTOR(nsDisplayMathMLCharBackground);
  }
#endif

  virtual void ComputeInvalidationRegion(nsDisplayListBuilder* aBuilder,
                                         const nsDisplayItemGeometry* aGeometry,
                                         nsRegion *aInvalidRegion) MOZ_OVERRIDE;
  virtual void Paint(nsDisplayListBuilder* aBuilder,
                     nsRenderingContext* aCtx);
  NS_DISPLAY_DECL_NAME("MathMLCharBackground", TYPE_MATHML_CHAR_BACKGROUND)
private:
  nsStyleContext* mStyleContext;
  nsRect          mRect;
};

void
nsDisplayMathMLCharBackground::ComputeInvalidationRegion(nsDisplayListBuilder* aBuilder,
                                                         const nsDisplayItemGeometry* aGeometry,
                                                         nsRegion *aInvalidRegion)
{
  AddInvalidRegionForSyncDecodeBackgroundImages(aBuilder, aGeometry, aInvalidRegion);

  nsDisplayItem::ComputeInvalidationRegion(aBuilder, aGeometry, aInvalidRegion);
}

void nsDisplayMathMLCharBackground::Paint(nsDisplayListBuilder* aBuilder,
                                          nsRenderingContext* aCtx)
{
  const nsStyleBorder* border = mStyleContext->StyleBorder();
  nsRect rect(mRect + ToReferenceFrame());
  nsCSSRendering::PaintBackgroundWithSC(mFrame->PresContext(), *aCtx, mFrame,
                                        mVisibleRect, rect,
                                        mStyleContext, *border,
                                        aBuilder->GetBackgroundPaintFlags());
}

class nsDisplayMathMLCharForeground : public nsDisplayItem {
public:
  nsDisplayMathMLCharForeground(nsDisplayListBuilder* aBuilder,
                                nsIFrame* aFrame, nsMathMLChar* aChar,
				                uint32_t aIndex, bool aIsSelected)
    : nsDisplayItem(aBuilder, aFrame), mChar(aChar), 
      mIndex(aIndex), mIsSelected(aIsSelected) {
    MOZ_COUNT_CTOR(nsDisplayMathMLCharForeground);
  }
#ifdef NS_BUILD_REFCNT_LOGGING
  virtual ~nsDisplayMathMLCharForeground() {
    MOZ_COUNT_DTOR(nsDisplayMathMLCharForeground);
  }
#endif

  virtual nsRect GetBounds(nsDisplayListBuilder* aBuilder, bool* aSnap) {
    *aSnap = false;
    nsRect rect;
    mChar->GetRect(rect);
    nsPoint offset = ToReferenceFrame() + rect.TopLeft();
    nsBoundingMetrics bm;
    mChar->GetBoundingMetrics(bm);
    nsRect temp(offset.x + bm.leftBearing, offset.y,
                bm.rightBearing - bm.leftBearing, bm.ascent + bm.descent);
    // Bug 748220
    temp.Inflate(mFrame->PresContext()->AppUnitsPerDevPixel());
    return temp;
  }
  
  virtual void Paint(nsDisplayListBuilder* aBuilder,
                     nsRenderingContext* aCtx)
  {
    mChar->PaintForeground(mFrame->PresContext(), *aCtx,
                           ToReferenceFrame(), mIsSelected);
  }

  NS_DISPLAY_DECL_NAME("MathMLCharForeground", TYPE_MATHML_CHAR_FOREGROUND)

  virtual nsRect GetComponentAlphaBounds(nsDisplayListBuilder* aBuilder)
  {
    bool snap;
    return GetBounds(aBuilder, &snap);
  }
  
  virtual uint32_t GetPerFrameKey() {
    return (mIndex << nsDisplayItem::TYPE_BITS)
      | nsDisplayItem::GetPerFrameKey();
  }

private:
  nsMathMLChar* mChar;
  uint32_t      mIndex;
  bool          mIsSelected;
};

#ifdef DEBUG
class nsDisplayMathMLCharDebug : public nsDisplayItem {
public:
  nsDisplayMathMLCharDebug(nsDisplayListBuilder* aBuilder,
                           nsIFrame* aFrame, const nsRect& aRect)
    : nsDisplayItem(aBuilder, aFrame), mRect(aRect) {
    MOZ_COUNT_CTOR(nsDisplayMathMLCharDebug);
  }
#ifdef NS_BUILD_REFCNT_LOGGING
  virtual ~nsDisplayMathMLCharDebug() {
    MOZ_COUNT_DTOR(nsDisplayMathMLCharDebug);
  }
#endif

  virtual void Paint(nsDisplayListBuilder* aBuilder,
                     nsRenderingContext* aCtx);
  NS_DISPLAY_DECL_NAME("MathMLCharDebug", TYPE_MATHML_CHAR_DEBUG)

private:
  nsRect mRect;
};

void nsDisplayMathMLCharDebug::Paint(nsDisplayListBuilder* aBuilder,
                                     nsRenderingContext* aCtx)
{
  // for visual debug
  int skipSides = 0;
  nsPresContext* presContext = mFrame->PresContext();
  nsStyleContext* styleContext = mFrame->StyleContext();
  nsRect rect = mRect + ToReferenceFrame();
  nsCSSRendering::PaintBorder(presContext, *aCtx, mFrame,
                              mVisibleRect, rect, styleContext, skipSides);
  nsCSSRendering::PaintOutline(presContext, *aCtx, mFrame,
                               mVisibleRect, rect, styleContext);
}
#endif


void
nsMathMLChar::Display(nsDisplayListBuilder*   aBuilder,
                      nsIFrame*               aForFrame,
                      const nsDisplayListSet& aLists,
                      uint32_t                aIndex,
                      const nsRect*           aSelectedRect)
{
  nsStyleContext* parentContext = mStyleContext->GetParent();
  nsStyleContext* styleContext = mStyleContext;

  if (mDraw == DRAW_NORMAL) {
    // normal drawing if there is nothing special about this char
    // Set default context to the parent context
    styleContext = parentContext;
  }

  if (!styleContext->StyleVisibility()->IsVisible())
    return;

  // if the leaf style context that we use for stretchy chars has a background
  // color we use it -- this feature is mostly used for testing and debugging
  // purposes. Normally, users will set the background on the container frame.
  // paint the selection background -- beware MathML frames overlap a lot
  if (aSelectedRect && !aSelectedRect->IsEmpty()) {
    aLists.BorderBackground()->AppendNewToTop(new (aBuilder)
      nsDisplayMathMLSelectionRect(aBuilder, aForFrame, *aSelectedRect));
  }
  else if (mRect.width && mRect.height) {
    const nsStyleBackground* backg = styleContext->StyleBackground();
    if (styleContext != parentContext &&
        NS_GET_A(backg->mBackgroundColor) > 0) {
      aLists.BorderBackground()->AppendNewToTop(new (aBuilder)
        nsDisplayMathMLCharBackground(aBuilder, aForFrame, mRect,
                                      styleContext));
    }
    //else
    //  our container frame will take care of painting its background

#if defined(DEBUG) && defined(SHOW_BOUNDING_BOX)
    // for visual debug
    aLists.BorderBackground()->AppendToTop(new (aBuilder)
      nsDisplayMathMLCharDebug(aBuilder, aForFrame, mRect));
#endif
  }
  aLists.Content()->AppendNewToTop(new (aBuilder)
    nsDisplayMathMLCharForeground(aBuilder, aForFrame, this,
                                  aIndex,
                                  aSelectedRect &&
                                  !aSelectedRect->IsEmpty()));
}

void
nsMathMLChar::ApplyTransforms(gfxContext* aThebesContext,
                              int32_t aAppUnitsPerGfxUnit,
                              nsRect &r)
{
  // apply the transforms
  if (mMirrored) {
    nsPoint pt = r.TopRight();
    aThebesContext->
      Translate(gfxPoint(NSAppUnitsToFloatPixels(pt.x, aAppUnitsPerGfxUnit),
                         NSAppUnitsToFloatPixels(pt.y, aAppUnitsPerGfxUnit)));
    aThebesContext->Scale(-mScaleX, mScaleY);
  } else {
    nsPoint pt = r.TopLeft();
    aThebesContext->
      Translate(gfxPoint(NSAppUnitsToFloatPixels(pt.x, aAppUnitsPerGfxUnit),
                         NSAppUnitsToFloatPixels(pt.y, aAppUnitsPerGfxUnit)));
    aThebesContext->Scale(mScaleX, mScaleY);
  }

  // update the bounding rectangle.
  r.x = r.y = 0;
  r.width /= mScaleX;
  r.height /= mScaleY;
}

void
nsMathMLChar::PaintForeground(nsPresContext* aPresContext,
                              nsRenderingContext& aRenderingContext,
                              nsPoint aPt,
                              bool aIsSelected)
{
  nsStyleContext* parentContext = mStyleContext->GetParent();
  nsStyleContext* styleContext = mStyleContext;

  if (mDraw == DRAW_NORMAL) {
    // normal drawing if there is nothing special about this char
    // Set default context to the parent context
    styleContext = parentContext;
  }

  nsRefPtr<gfxContext> thebesContext = aRenderingContext.ThebesContext();

  // Set color ...
  nscolor fgColor = styleContext->GetVisitedDependentColor(eCSSProperty_color);
  if (aIsSelected) {
    // get color to use for selection from the look&feel object
    fgColor = LookAndFeel::GetColor(LookAndFeel::eColorID_TextSelectForeground,
                                    fgColor);
  }
  thebesContext->SetColor(fgColor);
  thebesContext->Save();
  nsRect r = mRect + aPt;
  ApplyTransforms(thebesContext, aPresContext->AppUnitsPerDevPixel(), r);

  switch(mDraw)
  {
    case DRAW_NORMAL:
    case DRAW_VARIANT:
      // draw a single glyph (base size or size variant)
      // XXXfredw verify if mGlyphs[0] is non-null to workaround bug 973322.
      if (mGlyphs[0]) {
        mGlyphs[0]->Draw(thebesContext, gfxPoint(0.0, mUnscaledAscent),
                         DrawMode::GLYPH_FILL, 0, mGlyphs[0]->GetLength(),
                         nullptr, nullptr, nullptr);
      }
      break;
    case DRAW_PARTS: {
      // paint by parts
      if (NS_STRETCH_DIRECTION_VERTICAL == mDirection)
        PaintVertically(aPresContext, thebesContext, r);
      else if (NS_STRETCH_DIRECTION_HORIZONTAL == mDirection)
        PaintHorizontally(aPresContext, thebesContext, r);
      break;
    }
    default:
      NS_NOTREACHED("Unknown drawing method");
      break;
  }

  thebesContext->Restore();
}

/* =============================================================================
  Helper routines that actually do the job of painting the char by parts
 */

class AutoPushClipRect {
  gfxContext* mThebesContext;
public:
  AutoPushClipRect(gfxContext* aThebesContext, int32_t aAppUnitsPerGfxUnit,
                   const nsRect& aRect)
    : mThebesContext(aThebesContext) {
    mThebesContext->Save();
    mThebesContext->NewPath();
    gfxRect clip = nsLayoutUtils::RectToGfxRect(aRect, aAppUnitsPerGfxUnit);
    mThebesContext->SnappedRectangle(clip);
    mThebesContext->Clip();
  }
  ~AutoPushClipRect() {
    mThebesContext->Restore();
  }
};

static nsPoint
SnapToDevPixels(const gfxContext* aThebesContext, int32_t aAppUnitsPerGfxUnit,
                const nsPoint& aPt)
{
  gfxPoint pt(NSAppUnitsToFloatPixels(aPt.x, aAppUnitsPerGfxUnit),
              NSAppUnitsToFloatPixels(aPt.y, aAppUnitsPerGfxUnit));
  pt = aThebesContext->UserToDevice(pt);
  pt.Round();
  pt = aThebesContext->DeviceToUser(pt);
  return nsPoint(NSFloatPixelsToAppUnits(pt.x, aAppUnitsPerGfxUnit),
                 NSFloatPixelsToAppUnits(pt.y, aAppUnitsPerGfxUnit));
}

static void
PaintRule(gfxContext* aThebesContext,
          int32_t     aAppUnitsPerGfxUnit,
          nsRect&     aRect)
{
  aThebesContext->NewPath();
  gfxRect rect = nsLayoutUtils::RectToGfxRect(aRect, aAppUnitsPerGfxUnit);
  aThebesContext->SnappedRectangle(rect);
  aThebesContext->Fill();
}

// paint a stretchy char by assembling glyphs vertically
nsresult
nsMathMLChar::PaintVertically(nsPresContext* aPresContext,
                              gfxContext*    aThebesContext,
                              nsRect&        aRect)
{
  // Get the device pixel size in the vertical direction.
  // (This makes no effort to optimize for non-translation transformations.)
  nscoord oneDevPixel = aPresContext->AppUnitsPerDevPixel();

  // get metrics data to be re-used later
  int32_t i = 0;
  nscoord dx = aRect.x;
  nscoord offset[3], start[3], end[3];
  for (i = 0; i <= 2; ++i) {
    const nsBoundingMetrics& bm = mBmData[i];
    nscoord dy;
    if (0 == i) { // top
      dy = aRect.y + bm.ascent;
    }
    else if (2 == i) { // bottom
      dy = aRect.y + aRect.height - bm.descent;
    }
    else { // middle
      dy = aRect.y + bm.ascent + (aRect.height - (bm.ascent + bm.descent))/2;
    }
    // _cairo_scaled_font_show_glyphs snaps origins to device pixels.
    // Do this now so that we can get the other dimensions right.
    // (This may not achieve much with non-rectangular transformations.)
    dy = SnapToDevPixels(aThebesContext, oneDevPixel, nsPoint(dx, dy)).y;
    // abcissa passed to Draw
    offset[i] = dy;
    // _cairo_scaled_font_glyph_device_extents rounds outwards to the nearest
    // pixel, so the bm values can include 1 row of faint pixels on each edge.
    // Don't rely on this pixel as it can look like a gap.
    if (bm.ascent + bm.descent >= 2 * oneDevPixel) {
      start[i] = dy - bm.ascent + oneDevPixel; // top join
      end[i] = dy + bm.descent - oneDevPixel; // bottom join
    } else {
      // To avoid overlaps, we don't add one pixel on each side when the part
      // is too small.
      start[i] = dy - bm.ascent; // top join
      end[i] = dy + bm.descent; // bottom join
    }
  }

  // If there are overlaps, then join at the mid point
  for (i = 0; i < 2; ++i) {
    if (end[i] > start[i+1]) {
      end[i] = (end[i] + start[i+1]) / 2;
      start[i+1] = end[i];
    }
  }

  nsRect unionRect = aRect;
  unionRect.x += mBoundingMetrics.leftBearing;
  unionRect.width =
    mBoundingMetrics.rightBearing - mBoundingMetrics.leftBearing;
  unionRect.Inflate(oneDevPixel, oneDevPixel);

  /////////////////////////////////////
  // draw top, middle, bottom
  for (i = 0; i <= 2; ++i) {
    // glue can be null
    if (mGlyphs[i]) {
      nscoord dy = offset[i];
      // Draw a glyph in a clipped area so that we don't have hairy chars
      // pending outside
      nsRect clipRect = unionRect;
      // Clip at the join to get a solid edge (without overlap or gap), when
      // this won't change the glyph too much.  If the glyph is too small to
      // clip then we'll overlap rather than have a gap.
      nscoord height = mBmData[i].ascent + mBmData[i].descent;
      if (height * (1.0 - NS_MATHML_DELIMITER_FACTOR) > oneDevPixel) {
        if (0 == i) { // top
          clipRect.height = end[i] - clipRect.y;
        }
        else if (2 == i) { // bottom
          clipRect.height -= start[i] - clipRect.y;
          clipRect.y = start[i];
        }
        else { // middle
          clipRect.y = start[i];
          clipRect.height = end[i] - start[i];
        }
      }
      if (!clipRect.IsEmpty()) {
        AutoPushClipRect clip(aThebesContext, oneDevPixel, clipRect);
        mGlyphs[i]->Draw(aThebesContext, gfxPoint(dx, dy),
                         DrawMode::GLYPH_FILL, 0, mGlyphs[i]->GetLength(),
                         nullptr, nullptr, nullptr);
      }
    }
  }

  ///////////////
  // fill the gap between top and middle, and between middle and bottom.
  if (!mGlyphs[3]) { // null glue : draw a rule
    // figure out the dimensions of the rule to be drawn :
    // set lbearing to rightmost lbearing among the two current successive
    // parts.
    // set rbearing to leftmost rbearing among the two current successive parts.
    // this not only satisfies the convention used for over/underbraces
    // in TeX, but also takes care of broken fonts like the stretchy integral
    // in Symbol for small font sizes in unix.
    nscoord lbearing, rbearing;
    int32_t first = 0, last = 1;
    while (last <= 2) {
      if (mGlyphs[last]) {
        lbearing = mBmData[last].leftBearing;
        rbearing = mBmData[last].rightBearing;
        if (mGlyphs[first]) {
          if (lbearing < mBmData[first].leftBearing)
            lbearing = mBmData[first].leftBearing;
          if (rbearing > mBmData[first].rightBearing)
            rbearing = mBmData[first].rightBearing;
        }
      }
      else if (mGlyphs[first]) {
        lbearing = mBmData[first].leftBearing;
        rbearing = mBmData[first].rightBearing;
      }
      else {
        NS_ERROR("Cannot stretch - All parts missing");
        return NS_ERROR_UNEXPECTED;
      }
      // paint the rule between the parts
      nsRect rule(aRect.x + lbearing, end[first],
                  rbearing - lbearing, start[last] - end[first]);
      PaintRule(aThebesContext, oneDevPixel, rule);
      first = last;
      last++;
    }
  }
  else if (mBmData[3].ascent + mBmData[3].descent > 0) {
    // glue is present
    nsBoundingMetrics& bm = mBmData[3];
    // Ensure the stride for the glue is not reduced to less than one pixel
    if (bm.ascent + bm.descent >= 3 * oneDevPixel) {
      // To protect against gaps, pretend the glue is smaller than it is,
      // in order to trim off ends and thus get a solid edge for the join.
      bm.ascent -= oneDevPixel;
      bm.descent -= oneDevPixel;
    }

    nsRect clipRect = unionRect;

    for (i = 0; i < 2; ++i) {
      // Make sure not to draw outside the character
      nscoord dy = std::max(end[i], aRect.y);
      nscoord fillEnd = std::min(start[i+1], aRect.YMost());
      while (dy < fillEnd) {
        clipRect.y = dy;
        clipRect.height = std::min(bm.ascent + bm.descent, fillEnd - dy);
        AutoPushClipRect clip(aThebesContext, oneDevPixel, clipRect);
        dy += bm.ascent;
        mGlyphs[3]->Draw(aThebesContext, gfxPoint(dx, dy),
                            DrawMode::GLYPH_FILL, 0, mGlyphs[3]->GetLength(),
                            nullptr, nullptr, nullptr);
        dy += bm.descent;
      }
    }
  }
#ifdef DEBUG
  else {
    for (i = 0; i < 2; ++i) {
      NS_ASSERTION(end[i] >= start[i+1],
                   "gap between parts with missing glue glyph");
    }
  }
#endif
  return NS_OK;
}

// paint a stretchy char by assembling glyphs horizontally
nsresult
nsMathMLChar::PaintHorizontally(nsPresContext* aPresContext,
                                gfxContext*    aThebesContext,
                                nsRect&        aRect)
{
  // Get the device pixel size in the horizontal direction.
  // (This makes no effort to optimize for non-translation transformations.)
  nscoord oneDevPixel = aPresContext->AppUnitsPerDevPixel();

  // get metrics data to be re-used later
  int32_t i = 0;
  nscoord dy = aRect.y + mBoundingMetrics.ascent;
  nscoord offset[3], start[3], end[3];
  for (i = 0; i <= 2; ++i) {
    const nsBoundingMetrics& bm = mBmData[i];
    nscoord dx;
    if (0 == i) { // left
      dx = aRect.x - bm.leftBearing;
    }
    else if (2 == i) { // right
      dx = aRect.x + aRect.width - bm.rightBearing;
    }
    else { // middle
      dx = aRect.x + (aRect.width - bm.width)/2;
    }
    // _cairo_scaled_font_show_glyphs snaps origins to device pixels.
    // Do this now so that we can get the other dimensions right.
    // (This may not achieve much with non-rectangular transformations.)
    dx = SnapToDevPixels(aThebesContext, oneDevPixel, nsPoint(dx, dy)).x;
    // abcissa passed to Draw
    offset[i] = dx;
    // _cairo_scaled_font_glyph_device_extents rounds outwards to the nearest
    // pixel, so the bm values can include 1 row of faint pixels on each edge.
    // Don't rely on this pixel as it can look like a gap.
    if (bm.rightBearing - bm.leftBearing >= 2 * oneDevPixel) { 
      start[i] = dx + bm.leftBearing + oneDevPixel; // left join
      end[i] = dx + bm.rightBearing - oneDevPixel; // right join
    } else {
      // To avoid overlaps, we don't add one pixel on each side when the part
      // is too small.
      start[i] = dx + bm.leftBearing; // left join
      end[i] = dx + bm.rightBearing; // right join
    }
  }

  // If there are overlaps, then join at the mid point
  for (i = 0; i < 2; ++i) {
    if (end[i] > start[i+1]) {
      end[i] = (end[i] + start[i+1]) / 2;
      start[i+1] = end[i];
    }
  }

  nsRect unionRect = aRect;
  unionRect.Inflate(oneDevPixel, oneDevPixel);

  ///////////////////////////
  // draw left, middle, right
  for (i = 0; i <= 2; ++i) {
    // glue can be null
    if (mGlyphs[i]) {
      nscoord dx = offset[i];
      nsRect clipRect = unionRect;
      // Clip at the join to get a solid edge (without overlap or gap), when
      // this won't change the glyph too much.  If the glyph is too small to
      // clip then we'll overlap rather than have a gap.
      nscoord width = mBmData[i].rightBearing - mBmData[i].leftBearing;
      if (width * (1.0 - NS_MATHML_DELIMITER_FACTOR) > oneDevPixel) {
        if (0 == i) { // left
          clipRect.width = end[i] - clipRect.x;
        }
        else if (2 == i) { // right
          clipRect.width -= start[i] - clipRect.x;
          clipRect.x = start[i];
        }
        else { // middle
          clipRect.x = start[i];
          clipRect.width = end[i] - start[i];
        }
      }
      if (!clipRect.IsEmpty()) {
        AutoPushClipRect clip(aThebesContext, oneDevPixel, clipRect);
        mGlyphs[i]->Draw(aThebesContext, gfxPoint(dx, dy),
                         DrawMode::GLYPH_FILL, 0, mGlyphs[i]->GetLength(),
                         nullptr, nullptr, nullptr);
      }
    }
  }

  ////////////////
  // fill the gap between left and middle, and between middle and right.
  if (!mGlyphs[3]) { // null glue : draw a rule
    // figure out the dimensions of the rule to be drawn :
    // set ascent to lowest ascent among the two current successive parts.
    // set descent to highest descent among the two current successive parts.
    // this satisfies the convention used for over/underbraces, and helps
    // fix broken fonts.
    nscoord ascent, descent;
    int32_t first = 0, last = 1;
    while (last <= 2) {
      if (mGlyphs[last]) {
        ascent = mBmData[last].ascent;
        descent = mBmData[last].descent;
        if (mGlyphs[first]) {
          if (ascent > mBmData[first].ascent)
            ascent = mBmData[first].ascent;
          if (descent > mBmData[first].descent)
            descent = mBmData[first].descent;
        }
      }
      else if (mGlyphs[first]) {
        ascent = mBmData[first].ascent;
        descent = mBmData[first].descent;
      }
      else {
        NS_ERROR("Cannot stretch - All parts missing");
        return NS_ERROR_UNEXPECTED;
      }
      // paint the rule between the parts
      nsRect rule(end[first], dy - ascent,
                  start[last] - end[first], ascent + descent);
      PaintRule(aThebesContext, oneDevPixel, rule);
      first = last;
      last++;
    }
  }
  else if (mBmData[3].rightBearing - mBmData[3].leftBearing > 0) {
    // glue is present
    nsBoundingMetrics& bm = mBmData[3];
    // Ensure the stride for the glue is not reduced to less than one pixel
    if (bm.rightBearing - bm.leftBearing >= 3 * oneDevPixel) {
      // To protect against gaps, pretend the glue is smaller than it is,
      // in order to trim off ends and thus get a solid edge for the join.
      bm.leftBearing += oneDevPixel;
      bm.rightBearing -= oneDevPixel;
    }

    nsRect clipRect = unionRect;

    for (i = 0; i < 2; ++i) {
      // Make sure not to draw outside the character
      nscoord dx = std::max(end[i], aRect.x);
      nscoord fillEnd = std::min(start[i+1], aRect.XMost());
      while (dx < fillEnd) {
        clipRect.x = dx;
        clipRect.width = std::min(bm.rightBearing - bm.leftBearing, fillEnd - dx);
        AutoPushClipRect clip(aThebesContext, oneDevPixel, clipRect);
        dx -= bm.leftBearing;
        mGlyphs[3]->Draw(aThebesContext, gfxPoint(dx, dy),
                            DrawMode::GLYPH_FILL, 0, mGlyphs[3]->GetLength(),
                            nullptr, nullptr, nullptr);
        dx += bm.rightBearing;
      }
    }
  }
#ifdef DEBUG
  else { // no glue
    for (i = 0; i < 2; ++i) {
      NS_ASSERTION(end[i] >= start[i+1],
                   "gap between parts with missing glue glyph");
    }
  }
#endif
  return NS_OK;
}

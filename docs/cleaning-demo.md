# Data Cleaning in Fossil: From Dirty CSV to Clean RDF

## The Problem

You download a football players dataset. It looks fine... until you actually look at it:

```
first_name,position,height_in_cm,country_of_citizenship
" Lionel ",Missing,42,UdSSR
"Kylian  ",Missing,0,Jugoslawien (SFR)
```

- **Sentinel values**: `"Missing"` instead of null
- **Cold War ghosts**: `"UdSSR"`, `"CSSR"`, `"Jugoslawien (SFR)"` — countries that haven't existed for decades
- **Impossible values**: A player 42cm tall? A player 0cm tall?
- **Whitespace noise**: `" Lionel "` with leading/trailing spaces

In OpenRefine, you'd spend 20 minutes clicking through menus. In Python, you'd write 50 lines of pandas transforms. In Fossil, you declare what "clean" looks like.

## The Solution

```fossil
type CleanData do
  player_id: int,

  #[clean(trim)]
  #[clean(default = "Unknown")]
  first_name: string,

  #[clean(to_null = "Missing")]
  position: string?,

  #[clean(min = 159)]
  height_in_cm: int?,

  #[clean(trim)]
  #[clean(replace = "UdSSR", with = "USSR")]
  #[clean(replace = "CSSR", with = "Czechoslovakia")]
  #[clean(replace = "Jugoslawien \\(SFR\\)", with = "Yugoslavia")]
  country_of_citizenship: string,

  city_of_birth: string?
end

let clean = CleanData.clean(data)
```

One type declaration. One function call. Done.

## Before vs After

| Field | Before | After |
|-------|--------|-------|
| first_name | `" Lionel "` | `"Lionel"` |
| first_name | `null` | `"Unknown"` |
| position | `"Missing"` | `null` |
| height_in_cm | `42` | `null` |
| height_in_cm | `180` | `180` |
| country | `"UdSSR"` | `"USSR"` |
| country | `"CSSR"` | `"Czechoslovakia"` |

## How It Works

Each `#[clean(...)]` attribute on a field becomes a Polars expression, applied in order (top to bottom). Fields without `#[clean]` pass through unchanged.

### Available Operations

| Operation | Example | What it does |
|-----------|---------|-------------|
| `trim` | `#[clean(trim)]` | Strip leading/trailing whitespace |
| `lower` | `#[clean(lower)]` | Convert to lowercase |
| `upper` | `#[clean(upper)]` | Convert to uppercase |
| `slug` | `#[clean(slug)]` | Transliterate + lowercase + strip non-alphanumeric |
| `default` | `#[clean(default = "N/A")]` | Replace nulls with a default value |
| `to_null` | `#[clean(to_null = "Missing")]` | Convert sentinel values to null |
| `min` | `#[clean(min = 159)]` | Null out values below threshold |
| `max` | `#[clean(max = 250)]` | Null out values above threshold |
| `replace` | `#[clean(replace = "X", with = "Y")]` | Regex replace all occurrences |

Operations compose naturally. This:

```fossil
#[clean(trim)]
#[clean(replace = "UdSSR", with = "USSR")]
```

First trims whitespace, then replaces the country name. Order matters.

## The Full Pipeline

```
csv!("players.csv") → CleanData.clean(data) → each row -> Player(...) → Rdf.serialize("out.ttl")
```

Three stages, all declarative:
1. **Load** the dirty CSV
2. **Clean** with a type declaration
3. **Map** to RDF with projections

## vs OpenRefine

| | OpenRefine | Fossil |
|---|-----------|--------|
| Define transforms | Click through UI menus | Declare on type fields |
| Reproduce | Export/import JSON recipe | It's in the source file |
| Version control | Awkward | It's just code |
| Chain with RDF mapping | Export → reimport | One pipeline |
| Batch processing | Manual | `fossil run mapping.fossil` |

The cleaning rules live next to the data they describe. No separate config files, no UI state to export, no impedance mismatch between "cleaning step" and "mapping step".

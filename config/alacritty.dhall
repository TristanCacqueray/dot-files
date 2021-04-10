let Alacritty =
      ~/git/github.com/cideM/dhall-alacritty/linux.dhall sha256:f6557ab972f0c1814f2ca07cb597a85cbf1ead510ecc59cc5ea37729a6b91407

let Solarized =
      { Light = Alacritty.Colors.Schema::{
        , bright =
          { black = "0x002b36"
          , blue = "0x839496"
          , cyan = "0x93a1a1"
          , green = "0x586e75"
          , magenta = "0x6c71c4"
          , red = "0xcb4b16"
          , white = "0xfdf6e3"
          , yellow = "0x657b83"
          }
        , normal =
          { black = "0x073642"
          , blue = "0x268bd2"
          , cyan = "0x2aa198"
          , green = "0x859900"
          , magenta = "0xd33682"
          , red = "0xdc322f"
          , white = "0xeee8d5"
          , yellow = "0xb58900"
          }
        , primary =
          { background = "0xfdf6e3"
          , foreground = "0x586e75"
          , dim_foreground = None Text
          , dim_background = None Text
          }
        }
      , Dark = Alacritty.Colors.Schema::{
        , bright =
          { black = "0x002b36"
          , blue = "0x839496"
          , cyan = "0x93a1a1"
          , green = "0x586e75"
          , magenta = "0x6c71c4"
          , red = "0xcb4b16"
          , white = "0xfdf6e3"
          , yellow = "0x657b83"
          }
        , normal =
          { black = "0x073642"
          , blue = "0x268bd2"
          , cyan = "0x2aa198"
          , green = "0x859900"
          , magenta = "0xd33682"
          , red = "0xdc322f"
          , white = "0xeee8d5"
          , yellow = "0xb58900"
          }
        , primary =
          { background = "0x002b36"
          , foreground = "0x839496"
          , dim_foreground = None Text
          , dim_background = None Text
          }
        }
      }

in  Alacritty.Config::{
    , env = toMap { TERM = "xterm-256color" }
    , live_config_reload = False
    }
  with scrolling = Alacritty.Scrolling::{ history = 65536 }
  with window.decorations = Alacritty.Window.Decoration.none
  with colors = Solarized.Light

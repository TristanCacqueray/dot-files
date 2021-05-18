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

let Dracula =
      Alacritty.Colors.Schema::{
      , bright =
        { black = "0x4d4d4d"
        , blue = "0xcaa9fa"
        , cyan = "0x9aedfe"
        , green = "0x5af78e"
        , magenta = "0xff92d0"
        , red = "0xff6e67"
        , white = "0xe6e6e6"
        , yellow = "0xf4f99d"
        }
      , cursor = Some { cursor = "CellForeground", text = "CellBackground" }
      , dim = Some
        { black = "0x14151b"
        , blue = "0x4d5b86"
        , cyan = "0x59dffc"
        , green = "0x1ef956"
        , magenta = "0xff46b0"
        , red = "0xff2222"
        , white = "0xe6e6d1"
        , yellow = "0xebf85b"
        }
      , normal =
        { black = "0x000000"
        , blue = "0xbd93f9"
        , cyan = "0x8be9fd"
        , green = "0x50fa7b"
        , magenta = "0xff79c6"
        , red = "0xff5555"
        , white = "0xbfbfbf"
        , yellow = "0xf1fa8c"
        }
      , primary =
        { background = "0x282a36"
        , foreground = "0xf8f8f2"
        , dim_foreground = None Text
        , dim_background = None Text
        }
      , selection = Some { background = "0x44475a", text = "CellForeground" }
      , vi_mode_cursor = Some
        { cursor = "CellForeground", text = "CellBackground" }
      }

let Xterm =
      Alacritty.Colors.Schema::{
      , bright =
        { black = "0x7f7f7f"
        , blue = "0x5c5cff"
        , cyan = "0x00ffff"
        , green = "0x00ff00"
        , magenta = "0xff00ff"
        , red = "0xff0000"
        , white = "0xffffff"
        , yellow = "0xffff00"
        }
      , normal =
        { black = "0x000000"
        , blue = "0x0000ee"
        , cyan = "0x00cdcd"
        , green = "0x00cd00"
        , magenta = "0xcd00cd"
        , red = "0xcd0000"
        , white = "0xe5e5e5"
        , yellow = "0xcdcd00"
        }
      , primary =
        { background = "0x000000"
        , foreground = "0xffffff"
        , dim_foreground = None Text
        , dim_background = None Text
        }
      }

in  Alacritty.Config::{
    , env = toMap { TERM = "xterm-256color" }
    , live_config_reload = False
    }
  with scrolling = Alacritty.Scrolling::{ history = 65536 }
  with window.decorations = Alacritty.Window.Decoration.none
  with colors = (if env:DARK ? False then Xterm else Dracula)

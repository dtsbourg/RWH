module PrettyJSON (
                    renderJValue
                  ) where

import SimpleJSON
import Prettify

renderJValue :: JValue -> Doc
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue (JNumber n)    = double n
renderJValue JNull          = text "Null"
renderJValue (JString s)    = string s
renderJValue (JArray a)     = series '[' ']' renderJValue a
renderJValue (JObject obj)  = series '{' '}' field obj
                              where field (name, val) = string name <> text ":" <> renderJValue val


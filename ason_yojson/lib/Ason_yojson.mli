module Basic : sig
  module Encode : Ason.Encode.Extra with type json = Yojson.Basic.t
  module Decode : Ason.Decode.Extra with type json = Yojson.Basic.t
end

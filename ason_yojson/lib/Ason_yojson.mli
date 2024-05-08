module Basic : sig
  module Encoder : Ason.Encoder.Extension with type json = Yojson.Basic.t
  module Decoder : Ason.Decoder.Extension with type json = Yojson.Basic.t
end

open Core

module type VARID = sig
  type t = private Int63.t

  include Unique_id.Id with type t := t

  include Comparable.S with type t := t
end

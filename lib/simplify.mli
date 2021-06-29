module type NAMESUPPLY = sig
  val create : unit -> string
end

module MakeNameSupply () : NAMESUPPLY

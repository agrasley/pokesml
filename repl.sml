use "utils.sml";

signature DISPLAY =
sig
    type display;

    val show : display -> string

    val output : string -> unit
end

signature READ =
sig
    structure A : ACTION

    val read : string -> A.action
end

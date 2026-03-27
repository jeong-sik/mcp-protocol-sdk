(** MCP Server framework over Eio stdio transport.

    Thin wrapper over [Generic_server.Make(Stdio_transport)].
    Delegates all logic to the generic implementation;
    only adds the [~stdin ~stdout] convenience for [run]. *)

module Base = Generic_server.Make (Stdio_transport)

include Base

let run s ~stdin ~stdout ?clock () =
  let transport = Stdio_transport.create ~stdin ~stdout () in
  Base.run s ~transport ?clock ()

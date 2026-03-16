(** TLS helpers for HTTPS transport using tls-eio + ca-certs. *)

(** Ensure CSPRNG is initialized (required by tls-eio). *)
let ensure_rng =
  let initialized = ref false in
  fun () ->
    if not !initialized then begin
      Mirage_crypto_rng_unix.use_default ();
      initialized := true
    end

let https_authenticator () =
  ensure_rng ();
  match Ca_certs.authenticator () with
  | Error msg -> Error msg
  | Ok authenticator ->
    match Tls.Config.client ~authenticator () with
    | Error (`Msg _ as msg) -> Error msg
    | Ok tls_config ->
      let connect uri socket =
        let host =
          Uri.host uri
          |> Option.map (fun h -> Domain_name.(of_string_exn h |> host_exn))
        in
        Tls_eio.client_of_flow tls_config ?host socket
      in
      Ok connect

let make_client net =
  ensure_rng ();
  match https_authenticator () with
  | Ok f ->
    Cohttp_eio.Client.make ~https:(Some f) net
  | Error (`Msg msg) ->
    Printf.eprintf "[mcp-protocol-http] TLS initialization failed: %s. HTTPS unavailable.\n%!" msg;
    Cohttp_eio.Client.make ~https:None net

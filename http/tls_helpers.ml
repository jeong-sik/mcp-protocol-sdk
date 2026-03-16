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
      let connect _uri socket =
        Tls_eio.client_of_flow tls_config socket
      in
      Ok connect

let make_client net =
  ensure_rng ();
  let https = match https_authenticator () with
    | Ok f -> Some f
    | Error _ -> None
  in
  Cohttp_eio.Client.make ~https net

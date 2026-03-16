(** TLS helpers for HTTPS transport.

    Provides a ready-to-use HTTPS connector for cohttp-eio clients,
    using tls-eio with the system CA bundle (ca-certs). *)

(** Ensure the Mirage_crypto CSPRNG is initialized. Idempotent.
    Other modules that need RNG should call this rather than
    maintaining their own initialization flag. *)
val ensure_rng : unit -> unit

(** Create an HTTPS connector suitable for [Cohttp_eio.Client.make ~https].

    Uses the operating system's trusted CA certificates.
    Requires Mirage_crypto_rng to be initialized (done automatically). *)
val https_authenticator :
  unit ->
  (Uri.t ->
   [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t ->
   Tls_eio.t,
   [> `Msg of string]) result

(** Create a cohttp-eio client with HTTPS support.

    Falls back to HTTP-only if system CA certificates are unavailable. *)
val make_client : _ Eio.Net.t -> Cohttp_eio.Client.t

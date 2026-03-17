(** Time Compatibility Layer - Eio-native timestamps with Unix fallback.

    Provides a unified timestamp API for gradual migration from
    [Unix.gettimeofday] to [Eio.Time.now].

    Usage:
    {[
      (* At server startup *)
      Time_compat.set_clock (Eio.Stdenv.clock env);

      (* In code - uses Eio.Time.now when available, Unix.gettimeofday otherwise *)
      let ts = Time_compat.now () in
      ...
    ]}

    @since 2026-02
*)

val set_clock : float Eio.Time.clock_ty Eio.Resource.t -> unit
(** Set the global Eio clock. Call once at server startup with
    [Eio.Stdenv.clock env]. *)

val clear_clock : unit -> unit
(** Clear the global clock (for testing or shutdown). *)

val has_clock : unit -> bool
(** [true] when an Eio clock has been set via {!set_clock}. *)

val now : unit -> float
(** Current Unix timestamp in seconds. Uses [Eio.Time.now] when clock is set,
    [Unix.gettimeofday] otherwise. *)

val now_ms : unit -> int
(** Current timestamp in milliseconds (integer). *)

val now_us : unit -> Int64.t
(** Current timestamp in microseconds (Int64). *)

val sleep : float -> unit
(** Sleep for the given duration in seconds. Uses [Eio.Time.sleep] when clock
    is set, [Unix.sleepf] otherwise (with a warning for durations > 10ms). *)

val timed : (unit -> 'a) -> 'a * float
(** [timed f] runs [f ()] and returns [(result, duration_seconds)]. *)

val timed_ms : (unit -> 'a) -> 'a * int
(** [timed_ms f] runs [f ()] and returns [(result, duration_ms)]. *)

(** Resilience Module - Circuit Breaker, Retry, and Timeout patterns for MCP servers.

    Provides three core patterns:
    - {b Circuit Breaker}: Prevents cascading failures by short-circuiting
      calls to failing services.
    - {b Retry with Backoff}: Retries transient failures with exponential
      backoff and optional jitter.
    - {b Timeout}: Wraps operations with Eio-based timeouts using [Fiber.first].
*)

(** {1 Logger Interface} *)

type log_level = Debug | Info | Warn | Err

type logger = log_level -> string -> unit
(** Logger function type. Injected by caller for dependency inversion. *)

val null_logger : logger
(** No-op logger that discards all messages. *)

(** {1 Retry Policy} *)

type retry_policy = {
  max_attempts: int;
  initial_delay_ms: int;
  max_delay_ms: int;
  backoff_multiplier: float;
  jitter: bool;
}

val default_policy : retry_policy
(** Default: 3 attempts, 100ms initial delay, 10s max, 2x backoff, jitter on. *)

(** {1 Circuit Breaker} *)

type circuit_state =
  | Closed      (** Normal operation - requests pass through. *)
  | Open        (** Failing - requests are rejected immediately. *)
  | HalfOpen    (** Testing recovery - one probe request allowed. *)

type circuit_breaker
(** Opaque circuit breaker handle. Create with {!create_circuit_breaker}. *)

val create_circuit_breaker :
  ?failure_threshold:int ->
  ?success_threshold:int ->
  ?timeout_ms:int ->
  ?logger:logger ->
  name:string ->
  unit ->
  circuit_breaker
(** Create a circuit breaker.
    @param failure_threshold Failures before opening (default 5).
    @param success_threshold Successes in HalfOpen before closing (default 2).
    @param timeout_ms Time in Open state before probing (default 30000).
    @param name Identifier for logging. *)

val circuit_allows : circuit_breaker -> bool
(** Check if the circuit breaker allows a request. Transitions Open to HalfOpen
    when timeout has elapsed. Thread-safe (uses Eio.Mutex). *)

val circuit_record_success : circuit_breaker -> unit
(** Record a successful call. In HalfOpen, may transition to Closed. *)

val circuit_record_failure : circuit_breaker -> unit
(** Record a failed call. In Closed, may transition to Open.
    Uses {!Time_compat.now} for timestamps. *)

(** {1 Retry} *)

type 'a retry_result =
  | Ok of 'a
  | Error of string
  | CircuitOpen
  | TimedOut

type retry_action =
  | Retry   (** The error is transient; retry the operation. *)
  | Fail of string  (** The error is permanent; stop with this message. *)

val calculate_delay : retry_policy -> int -> float
(** [calculate_delay policy attempt] returns delay in milliseconds for the
    given attempt number. Applies exponential backoff, cap, and optional jitter. *)

val with_retry_eio :
  clock:_ Eio.Time.clock ->
  ?policy:retry_policy ->
  ?circuit_breaker:circuit_breaker option ->
  ?logger:logger ->
  op_name:string ->
  classify:(string -> retry_action) ->
  (unit -> 'a retry_result) ->
  'a retry_result
(** Eio-based retry with structured error classification.
    @param clock Eio clock for sleep between retries.
    @param classify Maps error strings to {!retry_action}.
    @param op_name Operation name for log messages. *)

(** {1 Timeout} *)

val with_timeout_eio :
  clock:_ Eio.Time.clock ->
  timeout_ms:int ->
  (unit -> 'a) ->
  'a retry_result
(** [with_timeout_eio ~clock ~timeout_ms f] runs [f ()] with a timeout.
    Returns [Ok result] on success or [Error "Timeout"] if the deadline passes.
    Uses [Eio.Fiber.first] for cooperative cancellation. *)

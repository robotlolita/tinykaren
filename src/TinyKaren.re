/**
 * Terms in the language (because we're typed)
 */
type term =
  | Var(int)
  | Pair(term, term)
  | Int(int)
  ;

/**
 * A stream (which is used for the state)
 */
type stream('x) =
  | Cons('x, stream('x))
  | Delay((unit) => stream('x))
  | Empty
  ;

type state = stream((int, term));

/**
 * A context for goals in the language.
 */
type context = {
  state: state,
  counter: int
};

type goal = (context) => stream(context);


let rec assoc = (key: 'a, stream: stream(('a, 'b))) =>
  switch (stream) {
  | Empty => None
  | Cons((k, v), tail) => 
    if (key == k) { Some(v) } else { assoc(key, tail) }
  | Delay(_) => None
  };

/**
 * Checks if we can substitute a variable in the term for one
 * in the current state.
 */
let rec walk = (term: term, state: state) =>
  switch (term) {
  | Var(n) => 
    switch (assoc(n, state)) {
    | Some(matched_term) => walk(matched_term, state)
    | None => term
    }
  | _ => term
  }

/**
 * Extends the state with a new key/value pair
 */
and extend_state = (key, term, state) =>
  Cons((key, term), state)

/**
 * Unification!
 */
and unify = (left: term, right: term, state: state) =>
  switch ((walk(left, state), walk(right, state))) {
  | (Var(l), Var(r)) when l == r => Some(state)
  | (Var(l), _) => Some(extend_state(l, right, state))
  | (_, Var(r)) => Some(extend_state(r, left, state))
  | (Pair(l0, l1), Pair(r0, r1)) =>
    switch (unify(l0, r0, state)) {
    | Some(new_state) => unify(l1, r1, new_state)
    | None => None
    }
  | (Int(l), Int(r)) => if (l == r) { Some(state) } else { None }
  | _ => None
  }

/**
 * A goal that tries to unify two terms.
 */
and equal = (left: term, right: term) =>
  (context) =>
    switch (unify(left, right, context.state)) {
    | Some(state) => unit({ state: state, counter: context.counter })
    | None => Empty
    }

/**
 * A goal that introduces a fresh variable.
 */
and call_fresh = (fn: term => goal) =>
  (context) => 
    fn(Var(context.counter))({
      state: context.state,
      counter: context.counter + 1
    })

/**
 * A goal that succeeds if either of the terms do.
 */
and disjunction = (left: goal, right: goal) =>
  (context) => 
    mplus(left(context), right(context))

/**
 * A goal that succeeds if both terms do (evaluates left-to-right).
 */
and conjunction = (left: goal, right: goal) =>
  (context) =>
    bind(left(context), right)

/**
 * Constructs a stream with a single value (monad unit).
 */
and unit = (context: context) => Cons(context, mzero)

/**
 * The empty stream (monoid zero).
 */
and mzero = Empty

/**
 * Concatenates two streams (monoid plus).
 */
and mplus = (left: stream('a), right: stream('a)) =>
  switch (left) {
  | Empty => right
  | Cons(head, tail) => Cons(head, mplus(tail, right))
  | Delay(f) => Delay(() => mplus(right, f()))
  }

/**
 * Transforms the context of a stream with a function (monad bind)
 * 
 * *slaps top of bind* this bad boy can search so many possibilities.
 */
and bind = (stream: stream(context), goal: goal) =>
  switch (stream) {
  | Empty => mzero
  | Cons(head, tail) => mplus(goal(head), bind(tail, goal))
  | Delay(f) => Delay(() => bind(f(), goal))
  }

/**
 * The initial state.
 */
and empty_state = { state: Empty, counter: 0 };

/*-- Other utilities */
let delay = (goal: unit => goal) => (context) => Delay((_) => goal()(context));

/*-- Example programs */
let five = call_fresh((q) => equal(q, Int(5)));

let a_and_b = conjunction(
  call_fresh((q) => equal(q, Int(7))),
  call_fresh((p) =>
    disjunction(
      equal(p, Int(5)),
      equal(p, Int(6))
    )
  )
);

let rec fives = (x: term) =>
  disjunction(
    equal(x, Int(5)),
    delay((_) => fives(x))
  );

let rec sixes = (x: term) =>
  disjunction(
    equal(x, Int(6)),
    delay((_) => sixes(x))
  );

let fives_and_sixes = call_fresh((x) =>
  disjunction(fives(x), sixes(x))
);

/*-- Other utilities */
let rec pull = (stream: stream('a)) =>
  switch (stream) {
  | Delay(f) => pull(f())
  | _        => stream
  };

let rec take = (n: int, stream: stream('a)) =>
  if (n == 0) {
    []
  } else {
    switch (pull(stream)) {
    | Empty => []
    | Cons(head, tail) => [head, ...take(n - 1, tail)]
    | Delay(f) => take(n, f())
    }
  };

let rec take_all = (stream: stream('a)) =>
  switch (pull(stream)) {
  | Empty => []
  | Cons(head, tail) => [head, ...take_all(tail)]
  | Delay(f) => take_all(f())
  }


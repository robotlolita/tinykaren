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

let rec assoc = (key, stream) =>
  switch (stream) {
  | Empty => None
  | Cons((k, v), tail) => 
    if (key == k) { Some(v) } else { assoc(key, tail) }
  };

/**
 * Checks if we can substitute a variable in the term for one
 * in the current state.
 */
let rec walk = (term, state) =>
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
and unify = (left, right, state) =>
  switch ((walk(left, state), walk(right, state))) {
  | (Var(l), Var(r)) when l == r => Some(state)
  | (Var(l), _) => Some(extend_state(l, right, state))
  | (_, Var(r)) => Some(extend_state(r, left, state))
  | (Pair(l0, l1), Pair(r0, r1)) =>
    switch (unify(l0, r0, state)) {
    | Some(new_state) => unify(l1, r1, new_state)
    | None => None
    }
  | (l, r) => if (l == r) { Some(state) } else { None }
  }

/**
 * A goal that tries to unify two terms.
 */
and equal = (left, right) =>
  (context) =>
    switch (unify(left, right, context.state)) {
    | Some(state) => [{ state: state, counter: context.counter }]
    | None => []
    }

/**
 * A goal that introduces a fresh variable.
 */
and call_fresh = (fn) =>
  (context) => 
    fn(Var(context.counter))({
      state: context.state,
      counter: context.counter + 1
    })

/**
 * A goal that succeeds if either of the terms do.
 */
and disjunction = (left, right) =>
  (context) => 
    mplus(left(context), context(right))

/**
 * A goal that succeeds if both terms do (evaluates left-to-right).
 */
and conjunction = (left, right) =>
  (context) =>
    bind(left(context), right)

/**
 * Constructs a stream with a single value (monad unit).
 */
and unit = (context) => Cons(context, mzero)

/**
 * The empty stream (monoid zero).
 */
and mzero = Empty

/**
 * Concatenates two streams (monoid plus).
 */
and mplus = (left, right) =>
  switch (left) {
  | Empty => right
  | Cons(head, tail) => Cons(head, mplus(tail, right))
  }

/**
 * Transforms the context of a stream with a function (monad bind)
 * 
 * *slaps top of bind* this bad boy can search so many possibilities.
 */
and bind = (stream, goal) =>
  switch (stream) {
  | Empty => mzero
  | Cons(head, tail) => mplus(goal(head), bind(tail, goal))
  }


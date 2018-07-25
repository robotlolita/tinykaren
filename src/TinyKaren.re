type term =
  | Var(int)
  | Pair(term, term)
  | Int(int)

type context = {
  state: list((int, term)),
  counter: int
}

let rec walk = (term, state) =>
  switch (term) {
  | Var(n) => 
    switch (List.assoc(n, state)) {
    | matched_term => walk(matched_term, state)
    | exception Not_found => term
    }
  | _ => term
  }

and extend_state = (key, term, state) =>
  [(key, term), ...state]

and equal = (left, right) =>
  (context) =>
    switch (unify(left, right, context.state)) {
    | Some(state) => [{ state: state, counter: context.counter }]
    | None => []
    }

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
  | (l, r) => if l == r { Some(state) } else { None }
  }


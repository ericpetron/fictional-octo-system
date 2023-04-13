let combine_substitutions a b ('a substitution option -> 'a substitution option -> 'a substitution option) =
    match a, b with
    | Some s1, Some s2 ->
        let check (x, e) acc =
          match Subst.findopt x s2 with
          | Some e' when e = e' -> Subst.add x e acc
          | None -> Subst.add x e acc
          | _ -> raise (Substitution.MalformedSubstitution "Substitution "Ddx("^str^", expr)": expr must be of type "Var(name)"")
        in Some (Subst.fold check s1 Subst.empty)
    | Some s, None | None, Some s -> Some s
    | None, None -> None

  exception MalformedSubstitution of string
end

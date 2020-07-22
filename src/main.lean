import .iohelp .entity
open ghost_state entity_data direction

@[derive decidable_eq]
inductive keycode
| left | right | up | down | pause

namespace keycode

instance : has_repr keycode :=
⟨λ k, "keycode." ++ match k with
  left := "left",
  right := "right",
  up := "up",
  down := "down",
  pause := "pause"
end⟩

def of_char : char → option keycode
| 'w' := some up
| 's' := some down
| 'a' := some left
| 'd' := some right
| ' ' := some pause
| _   := none

end keycode

def entities : list entity := [
  ⟨0, 0, pacman left⟩,
  ⟨1, 0, pacman right⟩,
  ⟨2, 0, pacman up⟩,
  ⟨3, 0, pacman down⟩,
  ⟨0, 1, blinky up chase⟩,
  ⟨1, 1, pinky up chase⟩,
  ⟨2, 1, inky up chase⟩,
  ⟨3, 1, clyde up chase⟩,
  ⟨0, 2, small_dot ff⟩,
  ⟨1, 2, small_dot tt⟩,
  ⟨2, 2, big_dot ff⟩,
  ⟨3, 2, big_dot tt⟩,
  ⟨0, 3, cherry ff⟩,
  ⟨0, 3, cherry tt⟩
]

inductive game_state : Type
| paused | start_wait | running | finishing

structure game : Type :=
(pac : entity) (hpispac : Σ' x y d,   pac = ⟨x, y, pacman d⟩)
(bli : entity) (hbisbli : Σ' x y d s, bli = ⟨x, y, blinky d s⟩)
(pin : entity) (hpispin : Σ' x y d s, pin = ⟨x, y, pinky d s⟩)
(ink : entity) (hiisink : Σ' x y d s, ink = ⟨x, y, inky d s⟩)
(cly : entity) (hciscly : Σ' x y d s, cly = ⟨x, y, clyde d s⟩)
(che : entity) (hcische : Σ' x y e,   che = ⟨x, y, e⟩)
(small_dots : list entity)
(hsdots : Π d ∈ small_dots, Σ' x y e, d = entity.mk x y (small_dot e))
(big_dots : list entity)
(hbdots : Π d ∈ big_dots, Σ' x y e, d = entity.mk x y (big_dot e))
(state : game_state) (score : ℕ) (lives : list entity)
(start_time : ℕ) (cur_time : ℕ)

private def loop_aux (xyd : ℕ × ℕ × direction) : io (option (ℕ × ℕ × direction)) := do
  c ← get_char,
  let x := xyd.fst, let y := xyd.snd.fst, let d := xyd.snd.snd,
  let xyd : ℕ × ℕ × direction := match keycode.of_char c with
  | (some keycode.left) := (x - 1, y, left)
  | (some keycode.right) := (x + 1, y, right)
  | (some keycode.up) := (x, y - 1, up)
  | (some keycode.down) := (x, y + 1, down)
  | (some _) := (x, y, d)
  | none := (x, y, d)
  end,
  color.clear_tile x y,
  entity.draw ⟨xyd.fst, xyd.snd.fst, pacman xyd.snd.snd⟩,
  io.stdout >>= io.fs.flush,
  return (some xyd)

def loop : io unit := do
  io.iterate (1, 1, down) loop_aux,
  return ()

def main : io unit := do
  color.clear_screen,
  loop

-- io.forever
/-
def main : io unit := do
  io.print $ esc_brack ++ "2J",
  monad.mapm' entity.draw entities,
  time ← get_time,
  io.print_ln $ "The time is " ++ time.repr
-/

/-
def main : io unit := do
  io.put_str "Enter a single char: ",
  c ← get_char,
  io.put_str_ln "",
  io.put_str_ln $ match c with
  | (some 'a') := repr keycode.left
  | (some 'd') := repr keycode.right
  | (some 'w') := repr keycode.up
  | (some 's') := repr keycode.down
  | (some ' ') := repr keycode.pause
  | (some '_') := "Too slow!"
  | (some c) := "Unknown key!"
  | none := "Too slow!"
  end
-/

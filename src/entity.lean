import .iohelp .text

inductive direction : Type
| left | right | up | down

inductive ghost_state : Type
| corner (n : fin 4)
| chase
| fly_back
| flee (start done : ℕ)
| spawn_wait

inductive entity_data : Type
| pacman (d : direction)
| blinky (d : direction) (gs : ghost_state)
| pinky (d : direction) (gs : ghost_state)
| inky (d : direction) (gs : ghost_state)
| clyde (d : direction) (gs : ghost_state)
| small_dot (eaten : bool)
| big_dot (eaten : bool)
| cherry (eaten : bool)

structure entity : Type :=
(x y : ℕ)
(data : entity_data)

namespace entity
open color
section moving
variable d : direction
open direction ghost_state

private def draw_pacman : io unit :=
  io.print $ fg.pacman.escape ++ bg.black
    ++ match d with
       | left := "ᗌ"
       | right := "ᗏ"
       | up := "ᗊ"
       | down := "ᗋ"
       end

private def draw_ghost : ghost_state → io unit
| fly_back := io.print $ bg.black.escape ++ fg.white ++ "∞"
| (flee _ _) := do
  t ← get_time,
  let color := bg.white.escape ++ fg.flashblue
    ++ ite (2 ∣ t) attr.enable.reverse "",
  io.print $ color ++ "ᗣ" ++ attr.disable.reverse
| _ := io.print $ bg.black.escape ++ "ᗣ"

end moving
open entity_data

private def draw_entity : entity_data → io unit
| (pacman d) := draw_pacman d
| (blinky _ s) := do io.print fg.blinky, draw_ghost s
| (pinky _ s) := do io.print fg.pinky, draw_ghost s
| (inky _ s) := do io.print fg.inky, draw_ghost s
| (clyde _ s) := do io.print fg.clyde, draw_ghost s
| (small_dot e) := if e then return () else
    io.print $ fg.dot.escape ++ bg.black ++ "◦"
| (big_dot e) := if e then return () else
    io.print $ fg.dot.escape ++ bg.black ++ "●"
| (cherry e) := if e then return () else
    io.print $ fg.cherry.escape ++ bg.black ++ "⧝"

def draw : entity → io unit
| ⟨x, y, e⟩ := do
  io.print $ cursor.to x y,
  draw_entity e,
  io.print $ cursor.to x y

end entity

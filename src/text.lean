import system.io

def esc : char := char.of_nat 27
def escs : string := esc.to_string
def esc_brack : string := escs ++ "["

namespace color

@[derive decidable_eq]
inductive bg : Type
| default
| black
| white

namespace bg
private def num_code : bg → ℕ
| default := 49
| black := 40
| white := 107

def escape (bg : bg) : string :=
esc_brack ++ (num_code bg).repr ++ "m"

instance : has_to_string _ := ⟨escape⟩
instance : inhabited _ := ⟨default⟩
instance : has_coe _ string := ⟨escape⟩
end bg

@[derive decidable_eq]
inductive fg : Type
| default
| black
| white
| pacman
| wall
| dot
| blinky
| pinky
| inky
| clyde
| flashblue
| cherry

namespace fg
private def num_code : fg → ℕ
| default := 39
| black := 30
| white := 97
| pacman := 93
| wall := 34
| dot := 37
| blinky := 91
| pinky := 95
| inky := 36
| clyde := 33
| flashblue := 34
| cherry := 91

def escape (fg : fg) : string :=
esc_brack ++ (num_code fg).repr ++ "m"

instance : has_to_string _ := ⟨escape⟩
instance : inhabited _ := ⟨default⟩
instance : has_coe _ string := ⟨escape⟩
end fg
end color

namespace attr
@[derive decidable_eq]
inductive enable : Type
| bold
| dim
| underline
| reverse

namespace enable
private def num_code : enable → ℕ
| bold := 1
| dim := 2
| underline := 4
| reverse := 7

def escape (enable : enable) : string :=
esc_brack ++ (num_code enable).repr ++ "m"

instance : has_to_string _ := ⟨escape⟩
instance : has_coe _ string := ⟨escape⟩
end enable

@[derive decidable_eq]
inductive disable : Type
| bold
| dim
| underline
| reverse
| all

namespace disable
private def num_code : disable → ℕ
| bold := 21
| dim := 22
| underline := 24
| reverse := 27
| all := 0

def escape (disable : disable) : string :=
esc_brack ++ (num_code disable).repr ++ "m"

instance : has_to_string _ := ⟨escape⟩
instance : has_coe _ string := ⟨escape⟩
end disable
end attr

@[derive decidable_eq]
inductive cursor
| up
| down
| left
| right
| to (x y : ℕ)

namespace cursor
private def aux : cursor → string
| up := "A"
| down := "B"
| left := "D"
| right := "C"
| (to x y) := y.succ.repr ++ ";" ++ x.succ.repr ++ "H"

def escape (cursor : cursor) : string :=
esc_brack ++ aux cursor

instance : has_to_string _ := ⟨escape⟩
instance : has_coe cursor string := ⟨escape⟩
end cursor

namespace color

def clear_screen : io unit :=
  io.print $ esc_brack ++ "2J"

def clear_tile (x y : ℕ) : io unit :=
  io.print ((cursor.to x y).escape ++ fg.black ++ bg.black ++ " ")

end color

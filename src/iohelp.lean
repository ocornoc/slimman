import system.io

def bash (s : string) : io string :=
  io.cmd {cmd := "bash", args := ["-c", s]}

def get_char : io char := do
  out ← bash "read -t 0.2 -N1 && echo $REPLY || echo _",
  return out.to_list.head

def get_time : io nat := do
  out ← bash "date +%s",
  return out.to_nat

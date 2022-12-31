open Bos
open Containers

module OS = struct
  include OS
  module Cmd = struct
    include Cmd
    let run' cmd () = run cmd
  end
end

let cwd = Result.get_exn @@ OS.Dir.current ()
let lib_path = Fpath.(cwd / "lib")
let zlib_path = Fpath.(lib_path / "zlib")
let libxlsxwriter_path = Fpath.(lib_path / "xlsxwriter")

let cp origin dest = OS.Cmd.run' Cmd.(v "cp" % "-R" % origin % dest)

let cp_deps =
  let open Result.Infix in
  let open Fpath in
  let origin = to_string @@ (parent @@ parent @@ parent @@ cwd) / "lib" in
  let dest = to_string @@ (parent @@ cwd) / "src" in
  cp origin dest

let build_zlib () =
  let open Monad.LazyIOResult in
  let progn =
    let open Cmd in
    OS.Cmd.run' (v "mkdir" % "build")
    *> OS.Cmd.run' (v "sh" % "configure" % "--static" % "--prefix=build")
    *> OS.Cmd.run' (v "make" % "-j" % "8")
    *> OS.Cmd.run' (v "make" % "install") in
  Result.join @@ OS.Dir.with_current zlib_path progn ()

let build_libxlsxwriter () =
  let progn () =
    let include_path = Fpath.(to_string @@ zlib_path / "build" / "include") in 
    OS.Cmd.run Cmd.(v "make" % "-I" % include_path % "-j" % "8") in
  Result.join @@ OS.Dir.with_current libxlsxwriter_path progn ()

let macos_build =
  let open Monad.LazyIOResult in
  let cp_xlsxwriter =
    let origin = Fpath.(to_string @@ libxlsxwriter_path / "lib" / "libxlsxwriter.a") in
    cp origin "libxlsxwriter.a" in
  let cp_zlib =
    let origin = Fpath.(to_string @@ zlib_path / "build" / "lib" / "libz.a") in
    cp origin "libz.a" in
  cp_deps
  *> build_zlib
  *> build_libxlsxwriter
  *> cp_xlsxwriter
  *> cp_zlib

let () =
  let open Result.Infix in
  Result.catch ~ok:(Fun.const ()) ~err:(fun (`Msg e) -> print_string e)
  @@ match Sys.os_type with
  | "Win32" -> Ok ()
  | "Unix" ->
      let* out, _ = OS.Cmd.run_out @@ Cmd.v "uname" |> OS.Cmd.out_string in
      begin
        match out with
        | "Darwin" -> macos_build ()
        | "Linux" -> Ok ()
        | _ -> Error (`Msg "unsupported OS")
      end
  | _ -> Error (`Msg "unsupported OS")

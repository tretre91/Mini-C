(** Type contenant les valeurs des arguments pouvant être passés au compilateur *)
type args = {
  input_file: string;
  output_file: string option;
  include_paths: string list;
  dump_preproc: string option;
}

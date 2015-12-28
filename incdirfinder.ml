open Core.Std
open Core.Core_sys
open Core.Core_filename

exception Unknown_Dir of string;;
(* takes a directory name, returns a ([directories], [files]) *)
(* TODO check for permissions to avoid Unknown *)
let get_subdirs dir = 
	List.partition_map (ls_dir dir) ~f:(fun x ->
		 match ( is_directory (concat dir x) ) with
			| `Yes -> `Fst x 
			| `No -> `Snd x 
			| `Unknown -> raise (Unknown_Dir ( "We found an object that is reported as Unknown, this is an error: " ^ x ) ) 
		)
;;

let is_excluded dir = 
	List.exists (ls_dir dir) ~f:(fun x -> x = ".exclude");;

let is_included dir = 
	List.exists (ls_dir dir) ~f:(fun x -> x = ".include");;

type sresult = All of string | These of string list | None;;

let rec searchinc dir = 
	match(is_excluded dir) with
	| true -> None
	(* Could do with a better lambda function for for_all *)
	| false -> let subdirres = (List.map ~f:(fun x -> searchinc (concat dir x)) (fst (get_subdirs dir))) in 
		match(List.for_all ~f:(fun x -> match(x) with All _ -> true | _ -> false) 
						subdirres) with
				| true -> All dir
				| false -> These (List.fold subdirres ~init:(List.map ~f:(fun x -> concat dir x) (snd (get_subdirs dir))) ~f:(fun accum x ->
															 match(x) with
																| All dirname -> [dirname] @ accum 
																| These dirs -> dirs @ accum 
																| None -> accum
														)		 
								 )	
;;
(* TODO: Cleanup command line arg parsing *)
let () = match(searchinc Sys.argv.(1)) with
			| These dirs -> List.iter ~f:print_endline dirs
			| All dir -> print_endline dir
			| None -> ()
;;


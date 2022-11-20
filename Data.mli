module Published : sig
  type t = { name : Common.Name.t;
             id : String.t;
             sis_user_id : String.t;
             sis_login_id : String.t;
             section : Common.Section.t;
             grades : (String.t * Float.t Option.t) List.t }

  val pp : t Common.printer

  val of_string : String.t -> t List.t
end

module TAGradeSheet : sig
  type t = { name : Common.Name.t;
             grades : (String.t * Float.t Option.t) List.t }

  val pp : t Common.printer
end

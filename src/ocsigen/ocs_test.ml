let main_service =
  Eliom_output.Html5.register_service
    ~path:["graff"]
    ~get_params:Eliom_parameters.unit
    (fun () () ->
      Lwt.return
        (HTML5.M.html
          (HTML5.M.head (HTML5.M.title (HTML5.M.pcdata "")) [])
          (HTML5.M.body [HTML5.M.h1 [HTML5.M.pcdata "Graffiti"]])))
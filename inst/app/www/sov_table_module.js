function sov_table_module_js(ns_prefix) {

  $("#" + ns_prefix + "sov_table").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "entity_id_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "sov_table").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "entity_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}

module Core  = Sharp_core
module Event = Sharp_event
module Form  = Sharp_form
module AJAX  = Sharp_ajax
module Ajax  = AJAX
module VDOM  = struct
  include Sharp_vdom
  module Subnetwork = Sharp_vdom_subnetwork
  module Sub        = Subnetwork
end
module Vdom  = VDOM

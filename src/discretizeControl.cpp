#include "discretizeControlR.h"

// [[Rcpp::plugins(cpp11)]]

std::shared_ptr<DiscControl> control_builder(Rcpp::List params)
{
  std::string methodChar = params["method"];
  DISCRETIZE_METHOD method = string2discretizeMethod(methodChar);
  if(DISCRETIZE_METHOD::MDL == method)
  {
    std::shared_ptr<DiscControl> result = std::make_shared<mdl::DiscControlMdl>(method);
    return result;
  } else if(DISCRETIZE_METHOD::EQUAL_SIZE == method)
  {
    int k = params["k"];
    std::shared_ptr<DiscControl> result = std::make_shared<equalsize::DiscControlEqualSize>(method, k);
    return result;
  }

  Rcpp::Rf_error("Selected method is not supported. Please use mdlControl() or equalsizeControl() function.");
}

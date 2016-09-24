#include "discretizeControlR.h"

// [[Rcpp::plugins(cpp11)]]

std::shared_ptr<DiscControl> control_builder(Rcpp::List params)
{
  std::string methodChar = params["method"];
  DISCRETIZE_METHOD method = string2discretizeMethod(methodChar);

  std::shared_ptr<DiscControl> result = std::make_shared<mdl::DiscControlMdl>(method);
  return result;
}

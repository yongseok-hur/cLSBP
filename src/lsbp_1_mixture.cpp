// Gibbs sampler for cLSBP (Polya-Gamma augmentation), exported to R as get_posterior_lsbp_1_mixture().

#include <RcppArmadillo.h>
#include <vector>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

double rig(const double shape, const double scale)
{
  // calling rgamma()
  return scale / as<double>(Function("rgamma")(1, shape));
}
/* overload rig() */
vec rig(const double shape, const vec &scale)
{
  // calling rgamma() from R
  return scale / as<vec>(Function("rgamma")(scale.size(), shape));
}
double rpg(const double psi)
{
  // calling rpg.devroye() from R::BayesLogit
  return as<double>(Function("rpg.devroye")(1, 1, psi));
}
std::vector<double> armaToStd(const vec &armaVec)
{
  std::vector<double> stdVec(armaVec.begin(), armaVec.end());
  return stdVec;
}
double logSigmoid(const double product)
{
  if (product < 0)
  {
    return product + logSigmoid(-product);
  }
  if (product > 18)
  {
    return exp(-2.0 * product) / 2.0 - exp(-product);
  }
  return -log(1.0 + exp(-product));
}
uword sampleR(vec &probs)
{
  probs -= max(probs);
  probs = exp(probs);
  probs /= sum(probs);
  // calling sample.int() from R
  return as<uword>(Function("sample.int")(probs.size(), 1, false, probs)) - 1;
}

class Normal
{
private:
  uword size, dominance;
  
  const double DOF, SCALE;
  double nu2, sigma2;
  
  vec param, premean;
  mat precision;
  
public:
  Normal(const double _DOF, const double _SCALE, const double global2, const vec &local2)
    : size(0), dominance(0), DOF(_DOF), SCALE(_SCALE), nu2(_DOF * _SCALE),
      premean(local2.size(), fill::zeros),
      precision(local2.size(), local2.size(), fill::zeros)
  {
    /* constructor */
    sigma2 = DOF ? rig(DOF / 2.0, nu2 / 2.0) : 1.0;
    param = randn(local2.size()) % sqrt(global2 * local2 * sigma2);
  }
  double dot(const vec &regressor) const
  {
    return sum(regressor % param);
  }
  double sigma() const
  {
    return sqrt(sigma2);
  }
  vec getScale() const
  {
    return param % param / 2.0 / sigma2;
  }
  double logLikelihood(const double regressand, const vec &regressor) const
  {
    return DOF ? as<double>(Function("dnorm")(regressand, dot(regressor), sqrt(sigma2), true)) : logSigmoid(2.0 * regressand * dot(regressor));
  }
  void updateAlgebra(const double regressand, const vec &regressor)
  {
    size++;
    dominance++;
    nu2 += regressand * regressand;
    premean += regressand * regressor;
    const double omega = DOF ? 1.0 : rpg(dot(regressor));
    precision += omega * regressor * regressor.t();
  }
  void updateParameter(const double global2, const vec &local2)
  {
    const mat upperTriangle = chol(precision + diagmat(1.0 / (global2 * local2)));
    const vec solved = solve(upperTriangle.t(), premean);
    sigma2 = DOF ? rig((DOF + size) / 2.0, (nu2 - sum(solved % solved)) / 2.0) : 1.0;
    param = solve(upperTriangle, solved + randn(local2.size()) * sqrt(sigma2));
    
    nu2 = DOF * SCALE;
    premean *= 0.0;
    precision *= 0.0;
  }
  List str()
  {
    const uword _size = size;
    size = 0;
    return List::create(
      _["size"] = _size,
      _["dominance"] = dominance,
      _["DOF"] = DOF,
      _["SCALE"] = SCALE,
      _["sigma2"] = sigma2,
      _["param"] = armaToStd(param));
  }
};

class Member
{
private:
  double Y, Z;
  vec X_out;
  vec X_beta, X_gamma;
  uword membership_out;
  
public:
  Member(const double _Y, const double _Z,
         const vec &_X_out,
         const vec &_X_beta, const vec &_X_gamma,
         const uword _membership_out)
    : Y(_Y), Z(_Z), X_out(_X_out),
      X_beta(_X_beta), X_gamma(_X_gamma),
      membership_out(_membership_out)
  {
    /* constructor */
  }
  // utilities
  double mu(std::vector<Normal *> &thetas) const
  {
    return thetas[membership_out]->dot(join_cols(X_beta, 0.0 * X_gamma));
  }
  double tau(std::vector<Normal *> &thetas) const
  {
    return thetas[membership_out]->dot(join_cols(0.0 * X_beta, X_gamma));
  }
  double outcome(std::vector<Normal *> &thetas) const
  {
    return thetas[membership_out]->dot(join_cols(X_beta, Z * X_gamma));
  }
  double sigma(std::vector<Normal *> &thetas) const
  {
    return thetas[membership_out]->sigma();
  }
  double membershipOut() const
  {
    return membership_out;
  }
  
  uword updateMembershipOut(std::vector<Normal *> &xouts, std::vector<Normal *> &thetas)
  {
    vec probs(thetas.size(), fill::zeros);
    double cumsum = 0.0;
    for (uword membership = 0; membership <= xouts.size(); membership++)
    {
      const double product = membership < xouts.size() ? xouts[membership]->dot(X_out) : 0.0;
      const double logSig = membership < xouts.size() ? logSigmoid(product) : 0.0;
      const double logLike = thetas[membership]->logLikelihood(Y, join_cols(X_beta, Z * X_gamma));
      probs(membership) = cumsum + logSig + logLike;
      cumsum += logSig - product;
    }
    membership_out = sampleR(probs);
    for (uword membership = 0; membership <= membership_out && membership < xouts.size(); membership++)
    {
      xouts[membership]->updateAlgebra(membership == membership_out ? 0.5 : -0.5, X_out);
    }
    thetas[membership_out]->updateAlgebra(Y, join_cols(X_beta, Z * X_gamma));
    
    return membership_out;
  }
};

// [[Rcpp::export]]
List get_posterior_lsbp_1_mixture(const vec &Y, const vec &Z,
                                  const mat &X_out,
                                  const mat &X_beta, const mat &X_gamma,
                                  const double DOF_XOUT, const double SCALE_XOUT,
                                  const double DOF_THETA, const double SCALE_THETA,
                                  const double DOF_SIGMA2, const double SCALE_SIGMA2,
                                  const uword nskip = 0, const uword nsim = 4000,
                                  const uword H = 4, const uword printEvery = 1)
{
  const uword n = Y.size();
  
  /* initialize xprops and xouts */
  std::vector<Normal *> xouts;
  
  double global_xout = rig(0.5, 1.0);
  double global2_xout = DOF_XOUT ? rig(DOF_XOUT / 2.0, DOF_XOUT * SCALE_XOUT / 2.0) : rig(0.5, 1.0 / global_xout);
  
  vec local_xout = rig(0.5, vec(X_out.n_cols, fill::ones));
  vec local2_xout = DOF_XOUT ? rig(DOF_XOUT / 2.0, DOF_XOUT * SCALE_XOUT / 2.0 * vec(X_out.n_cols, fill::ones)) : rig(0.5, 1.0 / local_xout);
  
  for (uword membership = 0; membership < H; membership++)
  {
    xouts.push_back(new Normal(0.0, 0.0, global2_xout, local2_xout));
  }
  
  /* initialize alphas and thetas */
  std::vector<Normal *> thetas;
  
  double global_theta = rig(0.5, 1.0);
  double global2_theta = DOF_THETA ? rig(DOF_THETA / 2.0, DOF_THETA * SCALE_THETA / 2.0) : rig(0.5, 1.0 / global_theta);
  
  vec local_theta = rig(0.5, vec(X_beta.n_cols + X_gamma.n_cols, fill::ones));
  vec local2_theta = DOF_THETA ? rig(DOF_THETA / 2.0, DOF_THETA * SCALE_THETA / 2.0 * vec(X_beta.n_cols + X_gamma.n_cols, fill::ones)) : rig(0.5, 1.0 / local_theta);
  
  for (uword membership = 0; membership <= H; membership++)
  {
    thetas.push_back(new Normal(DOF_SIGMA2, SCALE_SIGMA2, global2_theta, local2_theta));
  }
  
  /* initialize members */
  std::vector<Member *> members;
  for (uword memberId = 0; memberId < n; memberId++)
  {
    uword randomId = randi<uword>(distr_param(0, H)); // sample uniformly from 0 to H (inclusive)
    members.push_back(new Member(Y(memberId), Z(memberId),
                                 X_out.row(memberId).t(),
                                 X_beta.row(memberId).t(), X_gamma.row(memberId).t(),
                                 randomId));
  }
  Rcout << "Mixture Model Initialized!" << std::endl;
  
  const uword niter = nskip + nsim;
  List resultList;
  for (uword iter = 0; iter < niter; iter++)
  {
    for (Member *member : members)
    {
      member->updateMembershipOut(xouts, thetas);
    }
    
    vec sc_xout(X_out.n_cols, fill::zeros);
    for (uword membership = 0; membership < H; membership++)
    {
      sc_xout += xouts[membership]->getScale();
    }
    
    global_xout = rig(1.0, 1.0 + 1.0 / global2_xout);
    global2_xout = DOF_XOUT ? rig(DOF_XOUT / 2.0 + H * X_out.n_cols / 2.0, DOF_XOUT * SCALE_XOUT / 2.0 + sum(sc_xout / local2_xout)) : rig(0.5 + H * X_out.n_cols / 2.0, 1.0 / global_xout + sum(sc_xout / local2_xout));
    
    local_xout = rig(1.0, 1.0 + 1.0 / local2_xout);
    local2_xout = DOF_XOUT ? rig(DOF_XOUT / 2.0 + H / 2.0, DOF_XOUT * SCALE_XOUT / 2.0 + sc_xout / global2_xout) : rig(0.5 + H / 2.0, 1.0 / local_xout + sc_xout / global2_xout);
    
    for (uword membership = 0; membership < H; membership++)
    {
      xouts[membership]->updateParameter(global2_xout, local2_xout);
    }
    
    vec sc_theta(X_beta.n_cols + X_gamma.n_cols, fill::zeros);
    for (uword membership = 0; membership <= H; membership++)
    {
      sc_theta += thetas[membership]->getScale();
    }
    
    global_theta = rig(1.0, 1.0 + 1.0 / global2_theta);
    global2_theta = DOF_THETA ? rig(DOF_THETA / 2.0 + (H + 1) * (X_beta.n_cols + X_gamma.n_cols) / 2.0, DOF_THETA * SCALE_THETA / 2.0 + sum(sc_theta / local2_theta)) : rig(0.5 + (H + 1) * (X_beta.n_cols + X_gamma.n_cols) / 2.0, 1.0 / global_theta + sum(sc_theta / local2_theta));
    
    local_theta = rig(1.0, 1.0 + 1.0 / local2_theta);
    local2_theta = DOF_THETA ? rig(DOF_THETA / 2.0 + (H + 1) / 2.0, DOF_THETA * SCALE_THETA / 2.0 + sc_theta / global2_theta) : rig(0.5 + (H + 1) / 2.0, 1.0 / local_theta + sc_theta / global2_theta);
    
    for (uword membership = 0; membership <= H; membership++)
    {
      thetas[membership]->updateParameter(global2_theta, local2_theta);
    }
    
    if ((iter + 1) % printEvery == 0)
    {
      Rcout << "Updated!::iter::" << (iter + 1) << std::endl;
      Rcout << "global2_xout = " << global2_xout << std::endl;
      Rcout << "local2_xout = " << local2_xout << std::endl;
    }
    
    /* print */
    List xoutList, thetaList, sigmawholeVector;
    for (uword membership = 0; membership < H; membership++)
    {
      xoutList.push_back(xouts[membership]->str());
    }
    for (uword membership = 0; membership <= H; membership++)
    {
      thetaList.push_back(thetas[membership]->str());
    }
    for (uword membership = 0; membership <= H; membership++)
    {
      sigmawholeVector.push_back(thetas[membership]->str());
    }
    std::vector<double> muVector, tauVector, sigmaVector, membershipOutVector;
    for (Member *member : members)
    {
      muVector.push_back(member->mu(thetas));
      tauVector.push_back(member->tau(thetas));
      sigmaVector.push_back(member->sigma(thetas));
      membershipOutVector.push_back(member->membershipOut());
    }
    resultList.push_back(List::create(
        _["xout"] = xoutList,
        _["global2_xout"] = global2_xout,
        _["local2_xout"] = local2_xout,
        _["theta"] = thetaList,
        _["mu"] = muVector,
        _["tau"] = tauVector,
        _["sigma"] = sigmaVector,
        _["membershipOut"] = membershipOutVector));
  }
  
  return resultList;
}
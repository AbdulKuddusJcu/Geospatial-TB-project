import arviz as az
import pymc as pm
import pytensor .tensor as pt
import pandas as pd
import numpy as np
import jax
import pymc.sampling.jax as pmjax
from pymc.model.transform.optimization import freeze_dims_and_data
jax.config.update("jax_enable_x64", True)

def random_walk_model(name, T, N, output_function, fhs, prison,
                      temperature, humidity, rainfall):
    
    b_00 = pm.Normal(f"b_00_{name}", mu = 0, sigma = 10)
    
    sigma = pm.Exponential(f"sigma_ij_{name}", lam = 2)
    b_temperature = pm.Normal(f"temperature_{name}", mu = 0, sigma = 10)
    b_humidity = pm.Normal(f"humidity_{name}", mu = 0, sigma = 10)
    b_fhs = pm.Normal(f"fhs_{name}", mu = 0, sigma = 10)
    b_rainfall =  pm.Normal(f"rainfall_{name}", mu = 0, sigma = 10)
    b_prison =  pm.Normal(f"prison_{name}", mu = 0, sigma = 10)
    
    zero_initial = pt.zeros((N, 1))
    b_inc_steps = pm.Normal(f"steps_{name}", mu=0, sigma=sigma, shape=(N, T-1))
    steps_with_initial = pt.concatenate([zero_initial, b_inc_steps], axis=1)
    tmp = pm.Deterministic(f"temp_{name}", steps_with_initial.cumsum(axis=1))        
    sigma_inc_i = pm.Exponential(f"sigma_i_{name}", lam  = 2)
    b_i0 =  pm.Normal(f"b_i0_{name}", mu = 0, sigma = sigma_inc_i, shape=(N))
    b_i0_mean = pm.Deterministic(f"b_i0_mean_{name}", b_i0.mean())
    
    b_ij_mean = pm.Deterministic(f"b_ij_mean_{name}", tmp.mean(axis=1))

    b = pm.Deterministic(f"b_{name}", b_00 + b_i0[:, None] - b_i0_mean + tmp - b_ij_mean[:,None]).reshape((1, T * N))
    
    inc = pm.Deterministic(f"output_{name}", output_function(b + b_fhs * fhs
                 + b_prison * prison + b_temperature * temperature
                 + b_humidity * humidity + b_rainfall * rainfall)).reshape((-1,))
    return inc

def scale(x):
  value = np.float32(x)
  value -= value.mean()
  value /= value.std(ddof = 1)
  return value

if __name__ == "__main__":
    data = pd.read_csv("data.csv")
    x_year = pd.Categorical(np.float32(data["YEAR"])).codes + 1
    T = len(set(x_year))
    N =  len(set(data["sg_uf"]))
    y_notif = data["cases"]
    pop_100k = (data["pop_100k"])
    fhs =  scale(data["fhs_sc"])
    prison =  scale(data["has_prison"])
    temperature =  scale(data["temp"])
    humidity =  scale(data["humi"])
    rainfall =  scale(data["rain"])
    mort = np.int16(data["mor_100k"])
    mort_treat = np.int16(data["treat_death"])
    aban_treat =  np.int16(data["def"]) 
    unknown = data["other"]
    mort = (np.int32(data["mor_100k"]))
    uncertain_param = 0.1
    
    with pm.Model() as model:
        p_mort_abandon = pm.Beta("p_mort_abandon", alpha = 4.287894, beta = 81.469979)
        p_surv_no_notif = pm.Beta("p_surv_no_notif", alpha =25.65, beta = 33.32)
        pop_100k_tensor = pt.as_tensor_variable(pop_100k)
        deaths_uncertainty = np.int32(pop_100k) * pm.Normal("uncertain", mu  = mort, sigma = uncertain_param)
        inc = random_walk_model("inc", T, N, pm.math.exp, fhs, prison, temperature, humidity, rainfall)
        pn = random_walk_model("pn", T, N, pm.math.invlogit, fhs, prison, temperature, humidity, rainfall)
        p_mort_notif = (pt.as_tensor_variable(mort_treat) + p_mort_abandon *  pt.as_tensor_variable(aban_treat))/ (y_notif -unknown)
        m_deaths = pm.Deterministic("deaths",  pop_100k_tensor * inc * ((pn * p_mort_notif) + ((1-pn) * (1-p_surv_no_notif))))
        missed_cases_per_100k = pm.Deterministic("missed_cases_per100k", inc  * (1 - pn))
        missed_cases = pm.Deterministic("missed_cases", missed_cases_per_100k * pop_100k )
        notifications_mean = pop_100k_tensor * inc * pn
        notif = pm.Poisson("notif", mu = notifications_mean,  observed = y_notif)
        
        mort_gen = pm.Normal("mort_gen", mu = m_deaths, sigma = pm.math.sqrt(m_deaths))
        all_mort = pm.Potential("mort", pm.Normal.logp(deaths_uncertainty, m_deaths, sigma = pm.math.sqrt(m_deaths)))
        
    frozen_m = freeze_dims_and_data(model)
    with frozen_m as model:
        trace_hmc = pmjax.sample_numpyro_nuts(draws=2000, tune=1000, chains = 4, target_accept=0.9, nuts_kwargs = {"max_tree_depth": 14})
    s = az.summary(trace_hmc)
    s.to_csv("results.csv")
    print(trace_hmc.sample_stats['diverging'].sum(axis = 1))

    print((trace_hmc.sample_stats['tree_depth'] == 14).sum(axis = 1))


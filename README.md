# Stu 

This is meant to be a simple bayesian modeling language, much like Stan.
This is not meant to be a general PPL or for building sophisticated probabilistic deep learning models like Tensorflow Probability. 

While TFP and PyMc seem very nice, I always end up reaching for Stan (or brms) when I want to write out a model.
It doesn't have a dozen slightly different ways to specify a model, it avoids boilerplate and has all the features that I've needed. 

I absolutely adore Stan, but there are a few points that always frustrate me. 

    - Stan doesn't support the broadcasted arithmetic I've gotten so accustomed to from using numpy 
    - Stan takes forever to compile models
    - Most of the errors I end up with are shape errors that should be able to be detected statically, 
    but instead I have to wait for Stan to generate, compile, and run a big c++ program before I can find 
    out about that error 
    - Stan's defaults are good, but occasionally I want more control over the sampler. 
    I might want to use Elliptical Slice Sampling, or use Pathfinder to initialize NUTS, 
    or use whatever the hot new MCMC methods are. In such cases, I have to rewrite my model in TFP. 

Stu tries to address these issues. 
Numpy style broadcasting is supported, with all shapes checked at compile time.
Note that this means that there are array operations not supported. 

### A taste of stu 

The following is the classic Eight Schools model in stu. 

```
// model.stu 

factor School;

data treatment_effects : [School]real;
data treatment_stddevs : [School]real;

param avg_effect : []real ~ Normal(0.0,10.0);
param avg_stddev : []real ~ Normal(5.0,1.0);

param school_effects_standard : [School]real ~ Normal(0.0,1.0);

obs treatment_effects ~ Normal(
        avg_effect + exp(avg_stddev) * school_effects_standard,
        treatment_stddevs
    ); 
```

Once you run `$ stu --model model.stu -o model.py`, you end up with a python containing a handful of functions. 
Most importantly, a `mk_log_prob` function that accepts an arviz `InferenceData` object containing the relevant data, and returns the log probability function. 
Then you can use this function with blackjax to perform inference as usual. 

```
from model import mk_log_prob
import arviz as az 
import jax
import blackjax

data = az.from_dict(
    constant_data={
        'treatment_stddevs' : jax.numpy.array([15., 10, 16, 11, 9, 11, 10, 18]) 
    },
    observed_data={
        'treatment_effects': jax.numpy.array([28., 8, -3, 7, -1, 1, 18, 12])
    }
)

target_lp = mk_log_prob(data)
```


### Things I would like to add support for in the future

- The ability to define functions in stu
- The ability to define distributions in stu
- Existential sized types sort of like Futhark, allowing for a wider range of array operations 

Eventually I would like to not rely on `jax`.
It would be nice to generate the llvm myself and just bake the more common inference algorithms into stu.
Of course I would still want to support `jax` as the primary backend so that I can take advantage of `blackjax`, `arviz`, and the rest of the python ecosystem.

As I mentioned earlier, I often use brms when I want to get started quickly on a model. 
Maybe I could have something similar built as a command line tool.
```
$ stu glmer 'y ~ x + (1|a)' --output model.stu
```

import rpy2
import rpy2.robjects as robjects
import rpy2.rlike.container as rlc


r = robjects.r
r['source']('r/BNExplain.R')

create_exp_bn = robjects.globalenv['createExpBN'] #takes two parameters: path to model cmpx and path to explanandum json
create_exp_bn_disc = robjects.globalenv['createDiscExpBN']
generate_exp_content_r = robjects.globalenv['generateAbstractExplanation'] #takes three parameters, exp_bn, case, and filename for json 

def generate_exp_content(exp_bn, case:dict, filename):

    case_r = rlc.TaggedList(list(case.values()), tags=list(case.keys()))
    #_Disc var states are parsed and the correct state for the numerical value is matched in R


    exp_content = generate_exp_content_r(exp_bn, case_r, filename)

    return(exp_content)
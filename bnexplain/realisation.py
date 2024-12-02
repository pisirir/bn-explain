from simplenlg.framework import *
from simplenlg.lexicon import *
from simplenlg.realiser.english import *
from simplenlg.phrasespec import *
from simplenlg.features import *
import numpy as np
import json, math
from .content_generation import *

import tempfile, os, time

def explain_reasoning(bn, bn_type, explanandum, case, schema, intermediate_type = None, intermediate_statement = None, category_type = None):
    
    if bn_type == 'discrete':
        exp_bn = create_exp_bn(bn, explanandum)
    
    if bn_type == 'discretised_continuous':
        exp_bn = create_exp_bn_disc(bn, explanandum)
    
    tempdir = tempfile.TemporaryDirectory()
    timestr = time.strftime("%Y%m%d-%H%M%S") + '.json'

    content_path = os.path.join(tempdir.name, timestr)

    exp_content = generate_exp_content(exp_bn, case, content_path)
    
    exp_narrative = generate_explanation(content_path, schema, intermediate_type = intermediate_type, intermediate_statement = intermediate_statement, category_type = category_type)

    tempdir.cleanup()
    return exp_narrative


def generate_explanation(content, schema, intermediate_type = None, intermediate_statement = None, category_type = None):

    with open(content, "r") as file:
        j_content = file.read()

    p_content = json.loads(j_content)

    with open(schema, "r") as file:
        j_schema = file.read()

    p_schema = json.loads(j_schema)

    lexicon = Lexicon.getDefaultLexicon()
    realiser = Realiser(lexicon)
    nlgFactory = NLGFactory(lexicon)

    s1 = realise_first(nlgFactory, p_content, p_schema, category_type=category_type)
    s2 = realise_second(nlgFactory, p_content, p_schema, intermediate_type=intermediate_type, intermediate_statement=intermediate_statement)
    s3 = realise_third(nlgFactory, p_content, p_schema, category_type=category_type)

    section = nlgFactory.createSection('Explanation of prediction for this case:')
    section.addComponent(s1)
    section.addComponent(s2)
    section.addComponent(s3)

    return realiser.realise(section).getRealisation()



def realise_first(nlgFactory, p_content, p_schema, category_type = None):
#first sentence is about the likely state of the target variable

    exp_sub = p_schema['subject_name']
    target = p_schema['target']['display_name']
    target_risk = get_risk_level(p_content, p_schema, node_id = p_schema['target']['id'], node_type='target', category_type=category_type)

    first_sentence = nlgFactory.createClause()

    first_sub = nlgFactory.createNounPhrase(exp_sub)
    first_sentence.setSubject(first_sub)

    first_verb = nlgFactory.createVerbPhrase('have')
    first_sentence.setVerb(first_verb)

    first_obj = nlgFactory.createNounPhrase(target)
    first_sentence.setObject(first_obj)

    first_obj_mod = nlgFactory.createNounPhrase(target_risk + ' of')
    first_obj.addPreModifier(first_obj_mod)

    return nlgFactory.createSentence(first_sentence)


def realise_second(nlgFactory, p_content, p_schema, intermediate_type = None, intermediate_statement = None):
# second sentence is about the most likely states of intermediate variables
# template options: all causal intermediate, all explainaway intermediate, mixed intermediate
# for causals: we can use most likely state (functional now) or risk categories (similar to target, requires additions to schema)
# for explainaway: pick up probability change and only display ones that are 'significantly different' than baseline 
# (content has no_obs and all_obs so we can calculate the difference)

    def get_intermediate_Likely_state(int_id):

        for i in p_content['intermediate']:
            if i['id'] == int_id:
                case_probs = i['probabilities']['all_obs']
                i_max = case_probs.index(max(case_probs))

        for it in p_schema['intermediate']:
            if it['id'] == int_id:
                return it['state_display_name'][i_max]
        
    def get_intermediate_name(int_id):

        for it in p_schema['intermediate']:
            if it['id'] == int_id:
                return it['display_name']

    def get_intermediate_type(int_id):

        for it in p_schema['intermediate']:
            if it['id'] == int_id:
                return it['type']

    def get_intermediate_change(int_id):

        for it in p_content['intermediate']:
            if it['id'] == int_id:
                this_int_sum = 0
                for n, a in zip(it['probabilities']['no_obs'], it['probabilities']['all_obs']):
                    this_state_calc = (math.sqrt(n) - math.sqrt(a))**2
                    this_int_sum += this_state_calc
                this_int_distance = math.sqrt(this_int_sum)/math.sqrt(2)
        return this_int_distance

    second_sentence = nlgFactory.createClause()

    if intermediate_type == 'causal' or intermediate_type is None:
        #if all intermediates are causal
        # sentence: 'SUBJECT_NAME likely has INT1_MOST_LIKELY_STATE INT1_DISPLAY_NAME and INT2_MOST_LIKELY_STATE INT2_DISPLAY_NAME'
        # example: 'this patient likely has no hypoperfusion and no tissue injury burden

        exp_sub = p_schema['subject_name']
        second_sentence.setSubject(exp_sub)

        second_verb = nlgFactory.createVerbPhrase('have')
        second_verb.addPreModifier('likely')
        second_sentence.setVerb(second_verb)

        second_obj = nlgFactory.createCoordinatedPhrase()

        if intermediate_statement == 'mostlikely' or intermediate_statement is None:
            for it in p_content['intermediate']:
                this_int = nlgFactory.createNounPhrase(get_intermediate_name(it['id']))
                if get_intermediate_Likely_state(it['id']) != 'yes':
                    this_int.addPreModifier(get_intermediate_Likely_state(it['id']))
                second_obj.addCoordinate(this_int)

        if intermediate_statement == 'riskcategory':
            for it in p_content['intermediate']:
                this_int = nlgFactory.createNounPhrase(get_intermediate_name(it['id']))
                this_int_risk = get_risk_level(p_content, p_schema, node_id = it['id'], node_type='intermediate')
                this_int_mod = nlgFactory.createNounPhrase(this_int_risk + ' of')
                this_int.addPreModifier(this_int_mod)
                second_obj.addCoordinate(this_int)


        second_sentence.setObject(second_obj)

    if intermediate_type == 'explainaway':
        # if all intermediates are explaining away
        # sentence: 'alternative causes of state of SUBJECT_NAME are INT1_DISPLAY_NAME and INT2_DISPLAY_NAME'
        # example: 'alternative causes of state of this patient are infective condition and fracture'

        exp_sub = 'alterrnative causes of state of ' + p_schema['subject_name']
        exp_sub_n = nlgFactory.createNounPhrase(exp_sub)
        exp_sub_n.setFeature(Feature.NUMBER, NumberAgreement.PLURAL)

        second_sentence.setSubject(exp_sub_n)


        second_verb = nlgFactory.createVerbPhrase('be')
        second_sentence.setVerb(second_verb)

        second_obj = nlgFactory.createCoordinatedPhrase()

        for it in p_content['intermediate']:
            if get_intermediate_change(it['id']) > 0.1:
                this_int = nlgFactory.createNounPhrase(get_intermediate_name(it['id']))
                second_obj.addCoordinate(this_int)

        second_sentence.setObject(second_obj)
        

    if intermediate_type == 'mixed':
        # if intermediates are mixed (some causal some explaining away)
        # will get intermediate_type of each int separately 


        pass



    return nlgFactory.createSentence(second_sentence)


def realise_third(nlgFactory, p_content, p_schema, category_type = None):
#third sentence is about values of significant evidence variables consistent and conflicting for target

    def get_evi_name(var_id):
        for ev in p_schema['evidence']:
            if var_id == ev['id']:
                return ev['display_name']
    
    def categorise_numerical(obs):
        for nv in p_schema['numerical_variables']:
            if obs['id'] == nv['id']:
                for ix, th in enumerate(nv['categories']['thresholds']):
                    if obs['value'] <= th:
                        this_value = nv['categories']['levels'][ix]
                        break
        
        return this_value

    def get_state_name(var_id, state_id):
        for ev in p_schema['evidence']:
            if var_id == ev['id']:
                for ix, s in enumerate(ev['states']):
                    if state_id == s:
                        return ev['state_display_name'][ix]

    numvars = [nv['id'] for nv in p_schema['numerical_variables']]

    third_sentence = nlgFactory.createClause()

    third_sub = nlgFactory.createNounPhrase(get_risk_level(p_content, p_schema, node_id=p_schema['target']['id'], node_type='target', category_type=category_type))
    third_sentence.setSubject(third_sub)

    be_verb = nlgFactory.createVerbPhrase('be')
    third_sentence.setVerb(be_verb)

    if p_content['evidence']['consistent'] is not None:

        cons_phrase = nlgFactory.createClause()

        cons_phrase.setSubject(p_schema['subject_name'])

        have_verb = nlgFactory.createVerbPhrase('have')
        cons_phrase.setVerb(have_verb)

        cons_obj = nlgFactory.createCoordinatedPhrase()

        for ob in p_content['observations']:
            if ob['id'] in p_content['evidence']['consistent']:
                this_obj = nlgFactory.createNounPhrase(get_evi_name(ob['id']))
                if ob['id'] in numvars:
                    this_obj.addPreModifier(categorise_numerical(ob))
                else:
                    if get_state_name(ob['id'], ob['value']) != 'yes':
                        this_obj.addPreModifier(get_state_name(ob['id'],ob['value']))
                
                cons_obj.addCoordinate(this_obj)
                
        cons_phrase.setObject(cons_obj)
        cons_phrase.setFeature(Feature.COMPLEMENTISER, "because")
        third_sentence.addComplement(cons_phrase)

    if p_content['evidence']['conflicting'] is not None:

        conf_phrase = nlgFactory.createClause()

        conf_phrase.setSubject(p_schema['subject_name'])
        conf_phrase.setVerb(have_verb)
        conf_obj = nlgFactory.createCoordinatedPhrase()

        for ob in p_content['observations']:
            if ob['id'] in p_content['evidence']['conflicting']:
                this_obj = nlgFactory.createNounPhrase(get_evi_name(ob['id']))
                if ob['id'] in numvars:
                    this_obj.addPreModifier(categorise_numerical(ob))
                else:
                    if get_state_name(ob['id'], ob['value']) != 'yes':
                        this_obj.addPreModifier(get_state_name(ob['id'],ob['value']))
                
                conf_obj.addCoordinate(this_obj)
                
        conf_phrase.setObject(conf_obj)
        conf_phrase.setFeature(Feature.COMPLEMENTISER, "although")
        third_sentence.addComplement(conf_phrase)

    return nlgFactory.createSentence(third_sentence)


def get_risk_level(p_content, p_schema, node_id, node_type = None, category_type = None):

    def get_category_phrases(category_type):
        for ct in p_schema['target']['categories']:
            if ct['type'] == category_type:
                this_phrases = ct
            
        return this_phrases

    if node_type == 'target':
        soi = p_content['target']['state_of_interest']

        if category_type is None:
            cat_type = p_schema['target']['categories'][0]['type']
        else:
            cat_type = category_type

        selected_phrases = get_category_phrases(cat_type)

        for ix, st in enumerate(p_content['target']['states']):
            if soi == st:
                this_case_prob = p_content['target']['probabilities']['all_obs'][ix]

        for ix, th in enumerate(selected_phrases['thresholds']):
            if this_case_prob <= th:
                this_case_phrase = selected_phrases['levels'][ix]
                break

    if node_type == 'intermediate':

        for it in p_schema['intermediate']:
            if it['id'] == node_id:
                soi = it['state_of_interest']
                for ix, st in enumerate(it['states']):
                    if soi == st:
                        soi_ix = ix

        for it in p_content['intermediate']:
            if it['id'] == node_id:
                this_case_prob = it['probabilities']['all_obs'][soi_ix]

        for it in p_schema['intermediate']:
            if it['id'] == node_id:
                for ix, th in enumerate(it['categories'][0]['thresholds']):
                    if this_case_prob <= th:
                        this_case_phrase = it['categories'][0]['levels'][ix]
                        break

    return this_case_phrase

def realise_fourth():
#fourth sentence is about values of significant evidence variables consistent and conflicting for intermediate

#do we recursively run the content generation algorithm for intermediate variables to find their significant evidence
#or do we just make links between significant evidence of target and intermediate variables?

#how to make sure this does not generate a lot of repetitive information and result in unnecessary length?
    pass

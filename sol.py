class SolubilityRuleBuilder:
    def __init__(self):
        """Initialize the rule builder with empty collections."""
        self.reactions = []
        self.cations = set()
        self.anions = set()
        self.compounds = {}  # Track all compounds and their precipitation tendency
        
    def add_reaction(self, reaction_data):
        """
        Add a reaction in the format [["C1", "A1"], ["C2", "A2"], "P/-"]
        where "P" indicates precipitation and "-" indicates no precipitation.
        """
        # Extract the components
        compound1 = reaction_data[0]  # ["C1", "A1"]
        compound2 = reaction_data[1]  # ["C2", "A2"]
        result = reaction_data[2]     # "P" or "-"
        
        # Extract cations and anions
        cation1, anion1 = compound1
        cation2, anion2 = compound2
        
        # Record cations and anions
        self.cations.add(cation1)
        self.cations.add(cation2)
        self.anions.add(anion1)
        self.anions.add(anion2)
        
        # Create compound tuples
        compound1_tuple = (cation1, anion1)
        compound2_tuple = (cation2, anion2)
        
        # Record precipitation result
        has_precipitate = result == "P"
        
        # Update compound statistics
        if compound1_tuple not in self.compounds:
            self.compounds[compound1_tuple] = {"precipitate_count": 0, "total_count": 0}
        if compound2_tuple not in self.compounds:
            self.compounds[compound2_tuple] = {"precipitate_count": 0, "total_count": 0}
        
        self.compounds[compound1_tuple]["total_count"] += 1
        self.compounds[compound2_tuple]["total_count"] += 1
        
        if has_precipitate:
            # Since we don't know which compound precipitated, we increment both
            self.compounds[compound1_tuple]["precipitate_count"] += 1
            self.compounds[compound2_tuple]["precipitate_count"] += 1
        
        # Store the complete reaction
        self.reactions.append({
            'compound1': compound1_tuple,
            'compound2': compound2_tuple,
            'has_precipitate': has_precipitate
        })
    
    def add_reactions(self, reaction_list):
        """Add multiple reactions at once."""
        for reaction_data in reaction_list:
            self.add_reaction(reaction_data)
    
    def calculate_precipitation_probabilities(self):
        """Calculate the probability of each compound forming a precipitate."""
        probabilities = {}
        
        for compound, stats in self.compounds.items():
            if stats["total_count"] > 0:
                prob = stats["precipitate_count"] / stats["total_count"]
                probabilities[compound] = prob
        
        return probabilities
    
    def identify_precipitating_compounds(self):
        """Identify which compounds are likely to precipitate."""
        probabilities = self.calculate_precipitation_probabilities()
        precipitating = {}
        non_precipitating = {}
        
        # Set thresholds for classification
        high_threshold = 0.7  # Likely to precipitate
        low_threshold = 0.3   # Likely to NOT precipitate
        
        for compound, prob in probabilities.items():
            if prob >= high_threshold:
                precipitating[compound] = prob
            elif prob <= low_threshold:
                non_precipitating[compound] = 1 - prob  # Convert to solubility probability
        
        return precipitating, non_precipitating
    
    def refine_compound_classifications(self):
        """Refine which compound in each pair is actually precipitating."""
        precipitating, non_precipitating = self.identify_precipitating_compounds()
        
        # For reactions with precipitates, determine which compound is more likely to be the one
        refined_precipitating = {}
        
        for reaction in self.reactions:
            if reaction['has_precipitate']:
                compound1 = reaction['compound1']
                compound2 = reaction['compound2']
                
                # Case 1: One compound is in precipitating list, other is in non-precipitating
                if compound1 in precipitating and compound2 in non_precipitating:
                    refined_precipitating[compound1] = precipitating[compound1]
                elif compound2 in precipitating and compound1 in non_precipitating:
                    refined_precipitating[compound2] = precipitating[compound2]
                
                # Case 2: Both in precipitating list, take the one with higher probability
                elif compound1 in precipitating and compound2 in precipitating:
                    if precipitating[compound1] >= precipitating[compound2]:
                        refined_precipitating[compound1] = precipitating[compound1]
                    else:
                        refined_precipitating[compound2] = precipitating[compound2]
                
                # Case 3: Neither in clear lists, can't determine
                else:
                    # If we have probabilities, use the higher one
                    if compound1 in self.compounds and compound2 in self.compounds:
                        prob1 = self.compounds[compound1]["precipitate_count"] / self.compounds[compound1]["total_count"]
                        prob2 = self.compounds[compound2]["precipitate_count"] / self.compounds[compound2]["total_count"]
                        
                        if prob1 > prob2 and prob1 > 0.5:
                            refined_precipitating[compound1] = prob1
                        elif prob2 > prob1 and prob2 > 0.5:
                            refined_precipitating[compound2] = prob2
        
        return refined_precipitating
    
    def generate_rules(self):
        """Generate solubility rules with exceptions, for both soluble and insoluble cases."""
        precipitating_compounds = self.refine_compound_classifications()
        rules = []
        
        # Get all non-precipitating compounds (for solubility rules)
        all_compounds = set(self.compounds.keys())
        non_precipitating = {comp for comp in all_compounds if comp not in precipitating_compounds}
        
        # Generate cation-based rules
        for cation in self.cations:
            # Get all compounds with this cation that precipitate
            cation_precipitating = [(cat, an) for (cat, an) in precipitating_compounds.keys() if cat == cation]
            
            # Get all compounds with this cation that don't precipitate
            cation_soluble = [(cat, an) for (cat, an) in non_precipitating if cat == cation]
            
            if cation_precipitating or cation_soluble:
                # All anions that have been paired with this cation
                all_cation_anions = [an for (cat, an) in self.compounds.keys() if cat == cation]
                
                # If cation has enough compounds to make a meaningful rule
                if len(all_cation_anions) >= 2:
                    # Find precipitating and soluble anions
                    precipitating_anions = [an for (_, an) in cation_precipitating]
                    soluble_anions = [an for (_, an) in cation_soluble]
                    
                    # Determine if most compounds are soluble or insoluble
                    if len(precipitating_anions) > len(soluble_anions):
                        # Most are insoluble, create insoluble rule with soluble exceptions
                        rule_type = "mostly_insoluble"
                        coverage = len(precipitating_anions) / len(all_cation_anions) * 100
                        
                        rules.append({
                            'type': 'cation',
                            'ion': cation,
                            'rule_type': rule_type,
                            'majority': precipitating_anions,
                            'exceptions': soluble_anions,
                            'coverage': coverage
                        })
                    else:
                        # Most are soluble, create soluble rule with insoluble exceptions
                        rule_type = "mostly_soluble"
                        coverage = len(soluble_anions) / len(all_cation_anions) * 100
                        
                        rules.append({
                            'type': 'cation',
                            'ion': cation,
                            'rule_type': rule_type,
                            'majority': soluble_anions,
                            'exceptions': precipitating_anions,
                            'coverage': coverage
                        })
        
        # Generate anion-based rules (similar logic)
        for anion in self.anions:
            # Get all compounds with this anion that precipitate
            anion_precipitating = [(cat, an) for (cat, an) in precipitating_compounds.keys() if an == anion]
            
            # Get all compounds with this anion that don't precipitate
            anion_soluble = [(cat, an) for (cat, an) in non_precipitating if an == anion]
            
            if anion_precipitating or anion_soluble:
                # All cations that have been paired with this anion
                all_anion_cations = [cat for (cat, an) in self.compounds.keys() if an == anion]
                
                # If anion has enough compounds to make a meaningful rule
                if len(all_anion_cations) >= 2:
                    # Find precipitating and soluble cations
                    precipitating_cations = [cat for (cat, _) in anion_precipitating]
                    soluble_cations = [cat for (cat, _) in anion_soluble]
                    
                    # Determine if most compounds are soluble or insoluble
                    if len(precipitating_cations) > len(soluble_cations):
                        # Most are insoluble, create insoluble rule with soluble exceptions
                        rule_type = "mostly_insoluble"
                        coverage = len(precipitating_cations) / len(all_anion_cations) * 100
                        
                        rules.append({
                            'type': 'anion',
                            'ion': anion,
                            'rule_type': rule_type,
                            'majority': precipitating_cations,
                            'exceptions': soluble_cations,
                            'coverage': coverage
                        })
                    else:
                        # Most are soluble, create soluble rule with insoluble exceptions
                        rule_type = "mostly_soluble"
                        coverage = len(soluble_cations) / len(all_anion_cations) * 100
                        
                        rules.append({
                            'type': 'anion',
                            'ion': anion,
                            'rule_type': rule_type,
                            'majority': soluble_cations,
                            'exceptions': precipitating_cations,
                            'coverage': coverage
                        })
        
        # Sort rules by coverage (highest first)
        return sorted(rules, key=lambda x: x['coverage'], reverse=True)
    
    def format_rules(self):
        """Format the rules in readable text with compounds in brackets."""
        rules = self.generate_rules()
        formatted_rules = []
        
        for rule in rules:
            ion = rule['ion']
            
            if rule['type'] == 'cation':
                if rule['rule_type'] == "mostly_insoluble":
                    base_rule = f"All {ion} compounds are insoluble"
                    if rule['exceptions']:
                        exceptions = ", ".join([f"[{ion} {ex}]" for ex in rule['exceptions']])
                        full_rule = f"{base_rule}, except {exceptions}."
                    else:
                        full_rule = f"{base_rule}."
                else:  # mostly_soluble
                    base_rule = f"All {ion} compounds are soluble"
                    if rule['exceptions']:
                        exceptions = ", ".join([f"[{ion} {ex}]" for ex in rule['exceptions']])
                        full_rule = f"{base_rule}, except {exceptions}."
                    else:
                        full_rule = f"{base_rule}."
            else:  # anion rule
                if rule['rule_type'] == "mostly_insoluble":
                    base_rule = f"All {ion} salts are insoluble"
                    if rule['exceptions']:
                        exceptions = ", ".join([f"[{ex} {ion}]" for ex in rule['exceptions']])
                        full_rule = f"{base_rule}, except {exceptions}."
                    else:
                        full_rule = f"{base_rule}."
                else:  # mostly_soluble
                    base_rule = f"All {ion} salts are soluble"
                    if rule['exceptions']:
                        exceptions = ", ".join([f"[{ex} {ion}]" for ex in rule['exceptions']])
                        full_rule = f"{base_rule}, except {exceptions}."
                    else:
                        full_rule = f"{base_rule}."
        
            # Add coverage information
            full_rule += f" (Confidence: {rule['coverage']:.1f}%)"
            
            formatted_rules.append(full_rule)
        
        return formatted_rules
    
    def predict_solubility(self, cation, anion):
        """Predict solubility based on generated rules."""
        compound = (cation, anion)
        
        # Check if we have direct data for this compound
        if compound in self.compounds:
            stats = self.compounds[compound]
            prob = stats["precipitate_count"] / stats["total_count"] if stats["total_count"] > 0 else 0
            
            if prob >= 0.7:  # Same threshold as in identify_precipitating_compounds
                return False, prob * 100, "Direct observation"
            elif prob <= 0.3:
                return True, (1 - prob) * 100, "Direct observation"
        
        # Apply rules for prediction
        rules = self.generate_rules()
        applicable_rules = []
        
        # Check cation rules
        cation_rules = [r for r in rules if r['type'] == 'cation' and r['ion'] == cation]
        for rule in cation_rules:
            if rule['rule_type'] == "mostly_insoluble":
                # Check if it's an exception
                if anion in rule['exceptions']:
                    applicable_rules.append((True, rule['coverage'], f"Exception to {cation} insolubility rule"))
                elif anion in rule['majority']:
                    applicable_rules.append((False, rule['coverage'], f"Follows {cation} insolubility rule"))
            else:  # mostly_soluble
                # Check if it's an exception
                if anion in rule['exceptions']:
                    applicable_rules.append((False, rule['coverage'], f"Exception to {cation} solubility rule"))
                elif anion in rule['majority']:
                    applicable_rules.append((True, rule['coverage'], f"Follows {cation} solubility rule"))
        
        # Check anion rules
        anion_rules = [r for r in rules if r['type'] == 'anion' and r['ion'] == anion]
        for rule in anion_rules:
            if rule['rule_type'] == "mostly_insoluble":
                # Check if it's an exception
                if cation in rule['exceptions']:
                    applicable_rules.append((True, rule['coverage'], f"Exception to {anion} insolubility rule"))
                elif cation in rule['majority']:
                    applicable_rules.append((False, rule['coverage'], f"Follows {anion} insolubility rule"))
            else:  # mostly_soluble
                # Check if it's an exception
                if cation in rule['exceptions']:
                    applicable_rules.append((False, rule['coverage'], f"Exception to {anion} solubility rule"))
                elif cation in rule['majority']:
                    applicable_rules.append((True, rule['coverage'], f"Follows {anion} solubility rule"))
        
        # If we have applicable rules, use the highest confidence one
        if applicable_rules:
            applicable_rules.sort(key=lambda x: x[1], reverse=True)
            is_soluble, confidence, explanation = applicable_rules[0]
            return is_soluble, confidence, explanation
        
        # Default fallback if no rules apply
        return None, 0, "No applicable rules found"


def main():
    """Example usage of the SolubilityRuleBuilder class."""
    builder = SolubilityRuleBuilder()
    
    # Use the sample_reactions from the document
    # [Your large sample_reactions data would go here]
    
    # Sample reactions in the simplified format [["C1", "A1"], ["C2", "A2"], "P/-"]
    sample_reactions = [
        [["AL", "NO3"], ["AG", "CL"], "P"], 
        [["AL", "NO3"], ["CA", "CL"], "-"], 
        [["AL", "NO3"], ["CU", "CL"], "-"], 
        [["AL", "NO3"], ["FE", "CL"], "P"], 
        [["AL", "NO3"], ["SR", "CL"], "-"], 
        [["AL", "NO3"], ["ZN", "CL"], "-"], 
        [["AL", "NO3"], ["PB", "CL"], "-"], 
        [["AL", "NO3"], ["MN", "CL"], "-"], 
        [["BA", "NO3"], ["AG", "CL"], "P"], 
        [["BA", "NO3"], ["CA", "CL"], "-"], 
        [["BA", "NO3"], ["CU", "CL"], "-"], 
        [["BA", "NO3"], ["FE", "CL"], "-"], 
        [["BA", "NO3"], ["SR", "CL"], "-"], 
        [["BA", "NO3"], ["ZN", "CL"], "-"], 
        [["BA", "NO3"], ["PB", "CL"], "-"], 
        [["BA", "NO3"], ["MN", "CL"], "P"], 
        [["K", "NO3"], ["AG", "I"], "P"], 
        [["K", "NO3"], ["CA", "I"], "-"], 
        [["K", "NO3"], ["CU", "I"], "P"], 
        [["K", "NO3"], ["FE", "I"], "P"], 
        [["K", "NO3"], ["SR", "I"], "-"], 
        [["K", "NO3"], ["ZN", "I"], "-"], 
        [["K", "NO3"], ["PB", "I"], "P"], 
        [["K", "NO3"], ["MN", "I"], "P"], 
        [["K", "NO3"], ["AG", "PO4"], "P"], 
        [["K", "NO3"], ["CA", "PO4"], "P"], 
        [["K", "NO3"], ["CU", "PO4"], "P"], 
        [["K", "NO3"], ["FE", "PO4"], "P"], 
        [["K", "NO3"], ["SR", "PO4"], "P"], 
        [["K", "NO3"], ["ZN", "PO4"], "P"], 
        [["K", "NO3"], ["PB", "PO4"], "P"], 
        [["K", "NO3"], ["MN", "PO4"], "P"], 
        [["MG", "NO3"], ["AG", "CL"], "P"], 
        [["MG", "NO3"], ["CA", "CL"], "-"], 
        [["MG", "NO3"], ["CU", "CL"], "P"], 
        [["MG", "NO3"], ["FE", "CL"], "P"], 
        [["MG", "NO3"], ["SR", "CL"], "-"], 
        [["MG", "NO3"], ["ZN", "CL"], "-"], 
        [["MG", "NO3"], ["PB", "CL"], "P"], 
        [["MG", "NO3"], ["MN", "CL"], "P"], 
        [["NA", "NO3"], ["AG", "OH"], "P"], 
        [["NA", "NO3"], ["CA", "OH"], "P"], 
        [["NA", "NO3"], ["CU", "OH"], "P"], 
        [["NA", "NO3"], ["FE", "OH"], "P"], 
        [["NA", "NO3"], ["SR", "OH"], "-"], 
        [["NA", "NO3"], ["ZN", "OH"], "P"], 
        [["NA", "NO3"], ["PB", "OH"], "P"], 
        [["NA", "NO3"], ["MN", "OH"], "P"], 
        [["NA", "NO3"], ["AG", "BR"], "P"], 
        [["NA", "NO3"], ["CA", "BR"], "-"], 
        [["NA", "NO3"], ["CU", "BR"], "P"], 
        [["NA", "NO3"], ["FE", "BR"], "P"], 
        [["NA", "NO3"], ["SR", "BR"], "-"], 
        [["NA", "NO3"], ["ZN", "BR"], "-"], 
        [["NA", "NO3"], ["PB", "BR"], "-"], 
        [["NA", "NO3"], ["MN", "BR"], "P"], 
        [["NH4", "NO3"], ["AG", "CO3"], "P"], 
        [["NH4", "NO3"], ["CA", "CO3"], "-"], 
        [["NH4", "NO3"], ["CU", "CO3"], "P"], 
        [["NH4", "NO3"], ["FE", "CO3"], "P"], 
        [["NH4", "NO3"], ["SR", "CO3"], "P"], 
        [["NH4", "NO3"], ["ZN", "CO3"], "P"], 
        [["NH4", "NO3"], ["PB", "CO3"], "P"], 
        [["NH4", "NO3"], ["MN", "CO3"], "P"], 
        [["NA", "NO3"], ["AG", "SO4"], "-"], 
        [["NA", "NO3"], ["CA", "SO4"], "-"], 
        [["NA", "NO3"], ["CU", "SO4"], "-"], 
        [["NA", "NO3"], ["FE", "SO4"], "-"], 
        [["NA", "NO3"], ["SR", "SO4"], "P"], 
        [["NA", "NO3"], ["ZN", "SO4"], "-"], 
        [["NA", "NO3"], ["PB", "SO4"], "-"], 
        [["NA", "NO3"], ["MN", "SO4"], "-"], 
        [["NA", "NO3"], ["AG", "C2O4"], "P"], 
        [["NA", "NO3"], ["CA", "C2O4"], "-"], 
        [["NA", "NO3"], ["CU", "C2O4"], "P"], 
        [["NA", "NO3"], ["FE", "C2O4"], "P"], 
        [["NA", "NO3"], ["SR", "C2O4"], "P"], 
        [["NA", "NO3"], ["ZN", "C2O4"], "-"], 
        [["NA", "NO3"], ["PB", "C2O4"], "-"], 
        [["NA", "NO3"], ["MN", "C2O4"], "P"], 
        [["NA", "NO3"], ["AG", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["CA", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["CU", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["FE", "C2H3O2"], "P"], 
        [["NA", "NO3"], ["SR", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["ZN", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["PB", "C2H3O2"], "-"], 
        [["NA", "NO3"], ["MN", "C2H3O2"], "P"], 
        [["SN", "NO3"], ["AG", "CL"], "P"], 
        [["SN", "NO3"], ["CA", "CL"], "-"], 
        [["SN", "NO3"], ["CU", "CL"], "-"], 
        [["SN", "NO3"], ["FE", "CL"], "P"], 
        [["SN", "NO3"], ["SR", "CL"], "-"], 
        [["SN", "NO3"], ["ZN", "CL"], "-"], 
        [["SN", "NO3"], ["PB", "CL"], "-"], 
        [["SN", "NO3"], ["MN", "CL"], "-"], 
        [["CR", "NO3"], ["AG", "SO4"], "-"], 
        [["CR", "NO3"], ["CA", "SO4"], "-"], 
        [["CR", "NO3"], ["CU", "SO4"], "-"], 
        [["CR", "NO3"], ["FE", "SO4"], "-"], 
        [["CR", "NO3"], ["SR", "SO4"], "-"], 
        [["CR", "NO3"], ["ZN", "SO4"], "-"], 
        [["CR", "NO3"], ["PB", "SO4"], "-"], 
        [["CR", "NO3"], ["MN", "SO4"], "-"], 
        [["CO", "NO3"], ["AG", "CL"], "P"], 
        [["CO", "NO3"], ["CA", "CL"], "-"], 
        [["CO", "NO3"], ["CU", "CL"], "-"], 
        [["CO", "NO3"], ["FE", "CL"], "P"], 
        [["CO", "NO3"], ["SR", "CL"], "-"], 
        [["CO", "NO3"], ["ZN", "CL"], "-"], 
        [["CO", "NO3"], ["PB", "CL"], "P"], 
        [["CO", "NO3"], ["MN", "CL"], "P"], 
        [["K", "NO3"], ["AG", "CRO4"], "P"], 
        [["K", "NO3"], ["CA", "CRO4"], "P"], 
        [["K", "NO3"], ["CU", "CRO4"], "-"], 
        [["K", "NO3"], ["FE", "CRO4"], "P"], 
        [["K", "NO3"], ["SR", "CRO4"], "-"], 
        [["K", "NO3"], ["ZN", "CRO4"], "P"], 
        [["K", "NO3"], ["PB", "CRO4"], "P"], 
        [["K", "NO3"], ["MN", "CRO4"], "P"], 
        [["NI", "NO3"], ["AG", "CL"], "P"], 
        [["NI", "NO3"], ["CA", "CL"], "-"], 
        [["NI", "NO3"], ["CU", "CL"], "-"], 
        [["NI", "NO3"], ["FE", "CL"], "P"], 
        [["NI", "NO3"], ["SR", "CL"], "-"], 
        [["NI", "NO3"], ["ZN", "CL"], "P"], 
        [["NI", "NO3"], ["PB", "CL"], "P"], 
        [["NI", "NO3"], ["MN", "CL"], "P"], 
        [["NA", "I"], ["K", "SO4"], "P"], 
        [["NA", "PO4"], ["K", "SO4"], "-"], 
        [["NA", "CO3"], ["NH4", "SO4"], "P"], 
        [["NA", "CRO4"], ["K", "SO4"], "-"], 
        [["NA", "I"], ["K", "OH"], "-"], 
        [["NA", "PO4"], ["K", "OH"], "-"], 
        [["NA", "CO3"], ["NH4", "OH"], "-"], 
        [["NA", "CRO4"], ["K", "OH"], "-"], 
        [["NA", "I"], ["K", "BR"], "-"], 
        [["NA", "PO4"], ["K", "BR"], "-"], 
        [["NA", "CO3"], ["NH4", "BR"], "-"], 
        [["NA", "CRO4"], ["K", "BR"], "-"], 
        [["NA", "I"], ["K", "C2O4"], "-"], 
        [["NA", "PO4"], ["K", "C2O4"], "-"], 
        [["NA", "CO3"], ["NH4", "C2O4"], "-"], 
        [["NA", "CRO4"], ["K", "C2O4"], "-"], 
        [["NA", "I"], ["K", "C2H3O2"], "-"], 
        [["NA", "PO4"], ["K", "C2H3O2"], "-"], 
        [["NA", "CO3"], ["NH4", "C2H3O2"], "-"], 
        [["NA", "CRO4"], ["K", "C2H3O2"], "-"], 
        [["AL", "I"], ["K", "CL"], "-"], 
        [["AL", "PO4"], ["K", "CL"], "P"], 
        [["AL", "SO4"], ["NA", "CL"], "-"], 
        [["AL", "OH"], ["NA", "CL"], "P"], 
        [["AL", "BR"], ["NA", "CL"], "-"], 
        [["AL", "C2O4"], ["NA", "CL"], "-"], 
        [["AL", "C2H3O2"], ["NA", "CL"], "-"], 
        [["AL", "CO3"], ["NH4", "CL"], "-"], 
        [["AL", "CRO4"], ["K", "CL"], "P"], 
        [["BA", "I"], ["K", "CL"], "-"], 
        [["BA", "PO4"], ["K", "CL"], "P"], 
        [["BA", "SO4"], ["NA", "CL"], "P"], 
        [["BA", "OH"], ["NA", "CL"], "-"], 
        [["BA", "BR"], ["NA", "CL"], "-"], 
        [["BA", "C2O4"], ["NA", "CL"], "P"], 
        [["BA", "C2H3O2"], ["NA", "CL"], "-"], 
        [["BA", "CO3"], ["NH4", "CL"], "P"], 
        [["BA", "CRO4"], ["K", "CL"], "P"], 
        [["MG", "I"], ["K", "CL"], "-"], 
        [["MG", "PO4"], ["K", "CL"], "P"], 
        [["MG", "SO4"], ["NA", "CL"], "-"], 
        [["MG", "OH"], ["NA", "CL"], "P"], 
        [["MG", "BR"], ["NA", "CL"], "-"], 
        [["MG", "C2O4"], ["NA", "CL"], "-"], 
        [["MG", "C2H3O2"], ["NA", "CL"], "-"], 
        [["MG", "CO3"], ["NH4", "CL"], "-"], 
        [["MG", "CRO4"], ["K", "CL"], "-"], 
        [["SN", "I"], ["K", "CL"], "-"], 
        [["SN", "PO4"], ["K", "CL"], "P"], 
        [["SN", "SO4"], ["NA", "CL"], "P"], 
        [["SN", "OH"], ["NA", "CL"], "-"], 
        [["SN", "BR"], ["NA", "CL"], "-"], 
        [["SN", "C2O4"], ["NA", "CL"], "-"], 
        [["SN", "C2H3O2"], ["NA", "CL"], "P"], 
        [["SN", "CO3"], ["NH4", "CL"], "P"], 
        [["SN", "CRO4"], ["K", "CL"], "P"], 
        [["CO", "I"], ["K", "CL"], "P"], 
        [["CO", "PO4"], ["K", "CL"], "P"], 
        [["CO", "SO4"], ["NA", "CL"], "-"], 
        [["CO", "OH"], ["NA", "CL"], "P"], 
        [["CO", "BR"], ["NA", "CL"], "-"], 
        [["CO", "C2O4"], ["NA", "CL"], "P"], 
        [["CO", "C2H3O2"], ["NA", "CL"], "P"], 
        [["CO", "CO3"], ["NH4", "CL"], "P"], 
        [["CO", "CRO4"], ["K", "CL"], "P"], 
        [["NI", "I"], ["K", "CL"], "-"], 
        [["NI", "PO4"], ["K", "CL"], "P"], 
        [["NI", "SO4"], ["NA", "CL"], "-"], 
        [["NI", "OH"], ["NA", "CL"], "P"], 
        [["NI", "BR"], ["NA", "CL"], "-"], 
        [["NI", "C2O4"], ["NA", "CL"], "P"], 
        [["NI", "C2H3O2"], ["NA", "CL"], "P"], 
        [["NI", "CO3"], ["NH4", "CL"], "P"], 
        [["NI", "CRO4"], ["K", "CL"], "-"], 
        [["CR", "I"], ["K", "SO4"], "-"], 
        [["CR", "PO4"], ["K", "SO4"], "P"], 
        [["CR", "OH"], ["NA", "SO4"], "P"], 
        [["CR", "BR"], ["NA", "SO4"], "P"], 
        [["CR", "C2O4"], ["NA", "SO4"], "-"], 
        [["CR", "C2H3O2"], ["NA", "SO4"], "-"], 
        [["CR", "CO3"], ["NH4", "SO4"], "P"], 
        [["CR", "CRO4"], ["K", "SO4"], "-"]
    ]


    builder.add_reactions(sample_reactions)
    
    refined_precipitating = builder.refine_compound_classifications()
    
    print("Top 200 Identified Precipitating Compounds:")
    print("-" * 60)
    count = 0
    for compound, prob in sorted(refined_precipitating.items(), key=lambda x: x[1], reverse=True):
        if count >= 200:
            break
        cation, anion = compound
        print(f"[{cation} {anion}]: {prob*100:.1f}% probability of precipitating")
        count += 1
    
    # Print the generated rules (limit to top 150 for readability)
    print("\nTop 150 Solubility Rules:")
    print("-" * 60)
    rules = builder.format_rules()
    for i, rule in enumerate(rules):
        if i >= 150:
            break
        print(rule)
    
    print(f"\nTotal rules generated: {len(rules)}")
    
    # Test predictions for some common compounds
    print("\nSolubility Predictions:")
    print("-" * 60)
    test_pairs = [
        ("AG", "CL"),     # Silver chloride
        ("NA", "CL"),     # Sodium chloride
        ("BA", "SO4"),    # Barium sulfate
        ("PB", "I"),      # Lead iodide
        ("AL", "OH"),     # Aluminum hydroxide
        ("CA", "CO3"),    # Calcium carbonate
        ("MG", "PO4"),    # Magnesium phosphate
        ("NH4", "OH"),    # Ammonium hydroxide
        ("CU", "S"),      # Copper sulfide (not in dataset)
        ("K", "NO3")      # Potassium nitrate
    ]
    
    for cation, anion in test_pairs:
        is_soluble, confidence, explanation = builder.predict_solubility(cation, anion)
        
        if is_soluble is None:
            prediction = "Unknown (insufficient data)"
        elif is_soluble:
            prediction = "Soluble"
        else:
            prediction = "Insoluble (precipitate forms)"
            
        print(f"[{cation} {anion}] → {prediction} ({explanation}, Confidence: {confidence:.1f}%)")


if __name__ == "__main__":
    main()

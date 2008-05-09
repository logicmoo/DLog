:- module(dig_identifier, [identifier/5]).

identifier(
XMLNS,
Name,
Version,
Message,
[ element(identifier,
	  [ xmlns = XMLNS, %'http://dl.kr.org/dig/2003/02/lang' || 'http://dl.kr.org/dig/lang',
	    name = Name, 
	    version = Version,
	    message = Message 
	  ],
	  [ '\n\t',
	    element(supports,
		    [],
		    [ '\n\t\t',
		      element(language,
			      [],
			      [ '\n\t\t\t',
				element(top, [], []),
				'\n\t\t\t',
				element(bottom, [], []),
				'\n\t\t\t',
				element(catom, [], []),
				'\n\t\t\t',
				element(ratom, [], []),
				% '\n\t\t\t',
				% element(intmin, [], []),
				% '\n\t\t\t',
				% element(intmax, [], []),
				% '\n\t\t\t',
				% element(intrange, [], []),
				% '\n\t\t\t',
				% element(intequals, [], []),
				% '\n\t\t\t',
				% element(stringequals, [], []),
				% '\n\t\t\t',
				% element(defined, [], []),
				'\n\t\t\t',
				element(and, [], []),
				'\n\t\t\t',
				element(or, [], []),
				'\n\t\t\t',
				element(not, [], []),
				'\n\t\t\t',
				element(some, [], []),
				'\n\t\t\t',
				element(all, [], []),
				'\n\t\t\t',
				element(atmost, [], []),
				'\n\t\t\t',
				element(atleast, [], []),
				% '\n\t\t\t',
				% element(iset, [], []),
				% '\n\t\t\t',
				% element(concrete, [], []),
				'\n\t\t\t',
				element(individual, [], []),
				'\n\t\t\t',
				element(feature, [], []),
				% '\n\t\t\t',
				% element(attribute, [], []),
				% '\n\t\t\t',
				% element(chain, [], []),
				'\n\t\t\t',
				element(inverse, [], []),
				'\n\t\t'
			      ]),
		      '\n\t\t',
		      element(tell,
			      [],
			      [ '\n\t\t\t',
				element(defconcept, [], []),
				'\n\t\t\t',
				element(defrole, [], []),
				'\n\t\t\t',
				element(deffeature, [], []),
				% '\n\t\t\t',
				% element(defattribute, [], []),
				'\n\t\t\t',
				element(defindividual, [], []),
				'\n\t\t\t',
				element(impliesc, [], []),
				'\n\t\t\t',
				element(impliesr, [], []),
				'\n\t\t\t',
				element(equalc, [], []),
				'\n\t\t\t',
				element(equalr, [], []),
				'\n\t\t\t',
				element(domain, [], []),
				'\n\t\t\t',
				element(range, [], []),
				% '\n\t\t\t',
				% element(rangeint, [], []),
				% '\n\t\t\t',
				% element(rangestring, [], []),
				'\n\t\t\t',
				element(transitive, [], []),
				'\n\t\t\t',
				element(functional, [], []),
				'\n\t\t\t',
				element(disjoint, [], []),
				'\n\t\t\t',
				element(instanceof, [], []),
				'\n\t\t\t',
				element(related, [], []),
				% '\n\t\t\t',
				% element(value, [], []),
				'\n\t\t'
			      ]),
		      '\n\t\t',
		      element(ask,
			      [],
			      [
				% '\n\t\t\t',
				% element(allConceptNames, [], []),
				% '\n\t\t\t',
				% element(allRoleNames, [], []),
				% '\n\t\t\t',
				% element(allIndividuals, [], []),
				% '\n\t\t\t',
				% element(satisfiable, [], []),
				% '\n\t\t\t',
				% element(subsumes, [], []),
				% '\n\t\t\t',
				% element(disjoint, [], []),
				% '\n\t\t\t',
				% element(parents, [], []),
				% '\n\t\t\t',
				% element(children, [], []),
				% '\n\t\t\t',
				% element(ancestors, [], []),
				% '\n\t\t\t',
				% element(descendants, [], []),
				% '\n\t\t\t',
				% element(equivalents, [], []),
				% '\n\t\t\t',
				% element(rparents, [], []),
				% '\n\t\t\t',
				% element(rchildren, [], []),
				% '\n\t\t\t',
				% element(rancestors, [], []),
				% '\n\t\t\t',
				% element(rdescendants, [], []),
				'\n\t\t\t',
				element(instances, [], []),
				% '\n\t\t\t',
				% element(types, [], []),
				'\n\t\t\t',
				element(roleFillers, [], []),
				'\n\t\t\t',
				element(relatedIndividuals, [], []),
				% '\n\t\t\t',
				% element(toldValues, [], []),
				'\n\t\t\t',
				element(instance, [], []),
				'\n\t\t'
			      ]),
		      '\n\t\t',
		      element(uniqueNameAssumption, [], []),
		      '\n\t'
		    ]),
	    ' \n'
	  ])
]
).

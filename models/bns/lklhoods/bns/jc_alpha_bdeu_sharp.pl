/*---------------------------------------------------------------------
  Predicate: alpha(+Family,+ChildVal,+ParentVals,-Alpha_ijk)
       Args: Family = names of child and parents
	     ChildVal = a value for the child (j)
             ParentVals = values of the parents (k)
             Alpha_ijk = approprate Dirichlet parameter
   Succeeds: 1
   Comments: This is a function
---------------------------------------------------------------------*/

% BDeu metric written on:
% Mon Mar 29 15:30:39 BST 2004

% BDeu metric
alpha(Child-Parents,_ChildVal,_ParentVals,Alpha_ijk) :-
	once( data:domain_size(Child,ChildDomainSize) ),
	product_of_domain_sizes(Parents,1,ParentsDomainSize),
	% global_alpha(Alpha),
	Alpha_ijk is 1/(ChildDomainSize*ParentsDomainSize).
	% Alpha_ijk = Alpha/(ChildDomainSize*ParentsDomainSize).

product_of_domain_sizes([],ParentsDomainSize,ParentsDomainSize).
product_of_domain_sizes([Parent|Rest],In,Out) :-
	once( data:domain_size(Parent,DomainSize) ),
	Mid is In*DomainSize,
	product_of_domain_sizes(Rest,Mid,Out).

% global_alpha(10).

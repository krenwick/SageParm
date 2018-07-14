///////////////////////////////////////////////////////////////////////////////////////
// MODULE SOURCE CODE FILE
//
// Module:                Vegetation C allocation, litter production, tissue turnover
//                        leaf phenology, allometry and growth
//                        (includes updated FPC formulation as required for "fast"
//                        cohort/individual mode - see canexch.cpp)
// Header file name:      growth.h
// Source code file name: growth.cpp
// Written by:            Ben Smith
// Version dated:         2002-12-16
// Updated:               2010-11-22

// WHAT SHOULD THIS FILE CONTAIN?
// Module source code files should contain, in this order:
//   (1) a "#include" directive naming the framework header file. The framework header
//       file should define all classes used as arguments to functions in the present
//       module. It may also include declarations of global functions, constants and
//       types, accessible throughout the model code;
//   (2) other #includes, including header files for other modules accessed by the
//       present one;
//   (3) type definitions, constants and file scope global variables for use within
//       the present module only;
//   (4) declarations of functions defined in this file, if needed;
//   (5) definitions of all functions. Functions that are to be accessible to other
//       modules or to the calling framework should be declared in the module header
//       file.
//
// PORTING MODULES BETWEEN FRAMEWORKS:
// Modules should be structured so as to be fully portable between models (frameworks).
// When porting between frameworks, the only change required should normally be in the
// "#include" directive referring to the framework header file.

#include "config.h"
#include "growth.h"

///////////////////////////////////////////////////////////////////////////////////////
// FILE SCOPE GLOBAL CONSTANTS

double APHEN_MAX =  210.0;
// Maximum number of equivalent days with full leaf cover per growing season
// for summergreen PFTs
double phencount = 0;


///////////////////////////////////////////////////////////////////////////////////////
// LEAF PHENOLOGY
// Call function leaf_phenology each simulation day prior to calculation of FPAR, to
// calculate fractional leaf-out for each PFT and individual.
// Function leaf_phenology_pft is not intended to be called directly by the framework,

void leaf_phenology_pft(Pft& pft, Climate& climate, double wscal, double aphen,
		double& phen) {

	// DESCRIPTION
	// Calculates leaf phenological status (fractional leaf-out) for a individuals of
	// a given PFT, given current heat sum and length of chilling period (summergreen
	// PFTs) and water stress coefficient (raingreen PFTs)

	// INPUT PARAMETER
	// wscal = water stress coefficient (0-1; 1=maximum stress)
	// aphen = sum of daily fractional leaf cover (equivalent number of days with
	//         full leaf cover) so far this growing season

	// OUTPUT PARAMETER
	// phen = fraction of full leaf cover for any individual of this PFT

	bool raingreen = pft.phenology == RAINGREEN || pft.phenology == ANY;
	bool summergreen = pft.phenology == SUMMERGREEN || pft.phenology == ANY;

	phen = 1.0;

	if (summergreen) {

		// Summergreen PFT - phenology based on GDD5 sum

		if (pft.lifeform == TREE) {

			// Calculate GDD base value for this PFT (if not already known) given
			// current length of chilling period (Sykes et al 1996, Eqn 1)
			/*dprintf("chilldays %i ", climate.chilldays);
			dprintf("gdd0 %f ", pft.gdd0[climate.chilldays]);
			dprintf(" gdd5 %f ", climate.gdd5);
			dprintf(" aphen %f", aphen);*/


			if (pft.gdd0[climate.chilldays] < 0.0)
				pft.gdd0[climate.chilldays] = pft.k_chilla
				+ pft.k_chillb
				* exp(
						-pft.k_chillk
						* (double) climate.chilldays);


			if (pft.ifsage == 1) //Added by KMR in 2017
				// Changes original so APHENMAX set in ins file for sagebrush
				// Also adds phen_winter: proportion of leaves on sage in winter
				if (climate.gdd5 > pft.gdd0[climate.chilldays] && aphen < pft.aphenmax)
						phen = min(1.0,
								(climate.gdd5 - pft.gdd0[climate.chilldays])
								/ pft.phengdd5ramp + pft.phen_winter);
						else if (aphen > pft.aphenmax)
							phen = max(pft.phen_winter,
									(1-(phencount* pft.downramp)));
						else phen = pft.phen_winter;


			else
				if (climate.gdd5 > pft.gdd0[climate.chilldays] && aphen < pft.aphenmax)
					phen = min(1.0,
							(climate.gdd5 - pft.gdd0[climate.chilldays])
							/ pft.phengdd5ramp);
				else if (aphen > pft.aphenmax)
					phen = max(0,
							(1-(phencount* pft.downramp)));
				else phen = 0;
			//dprintf(" phencount %f ", phencount);
			//dprintf("On date %i", date.day);
			//dprintf(" phen =  %f\n", phen);


		} else if (pft.lifeform == GRASS || pft.lifeform == CROP) {

			// Summergreen grasses have no maximum number of leaf-on days per
			// growing season, and no chilling requirement

			// KMR addition so grass looses leaves gradually after set # days
			if (aphen > pft.aphenmax) phencount+=1;
			else (phencount = 0);
			if (aphen < pft.aphenmax)
			phen = min(1.0, climate.gdd5 / pft.phengdd5ramp);
			else phen = max(0.0, (1-(phencount* pft.downramp)));
			//dprintf("On date %i", date.day);
			//dprintf(" phen %f\n", phen);

		}
	}

	if (raingreen) {

		// Raingreen phenology based on water stress threshold
				if (wscal < pft.wscal_min)
								phen = 0.0;

	}
	//dprintf("On date %i", date.day);
				//dprintf(" phen =  %f\n", phen);
}

void leaf_phenology(Patch& patch, Climate& climate) {

	// DESCRIPTION
	// Updates leaf phenological status (fractional leaf-out) for Patch PFT objects and
	// all individuals in a particular patch.

	// Updated by Ben Smith 2002-07-24 for compatability with "fast" canopy exchange
	// code (phenology assigned to patchpft for all vegetation modes)

	// guess2008
	bool leafout = true; // CHILLDAYS

	// Obtain reference to Vegetation object
	Vegetation& vegetation = patch.vegetation;

	// INDIVIDUAL AND COHORT MODES
	// Calculate phenology for each PFT at this patch

	// Loop through patch-PFTs

	patch.pft.firstobj();
	while (patch.pft.isobj) {
		Patchpft& pft = patch.pft.getobj();

		// For this PFT ...
		leaf_phenology_pft(pft.pft, climate, pft.wscal, pft.aphen, pft.phen);

		// guess2008
		if (pft.pft.lifeform == TREE
				&& (pft.pft.phenology == SUMMERGREEN || pft.pft.phenology == ANY))
			if (pft.phen < 1.0)
				leafout = false; // CHILLDAYS

		// Update annual leaf-on sum
		if ((climate.lat >= 0.0 && date.day == COLDEST_DAY_NHEMISPHERE)
				|| (climate.lat < 0.0 && date.day == COLDEST_DAY_SHEMISPHERE))
			pft.aphen = 0.0;
		pft.aphen += pft.phen;

		// ... on to next PFT
		patch.pft.nextobj();
	}

	// guess2008
	if (leafout)
		climate.ifsensechill = true; // CHILLDAYS

	// Copy PFT-specific phenological status to individuals of each PFT

	// Loop through individuals

	vegetation.firstobj();
	while (vegetation.isobj) {
		Individual& indiv = vegetation.getobj();

		// For this individual ...
		indiv.phen = patch.pft[indiv.pft.id].phen;

		// Update annual leaf-day sum (raingreen PFTs)
		if (date.day == 0)
			indiv.aphen_raingreen = 0;
		indiv.aphen_raingreen += (indiv.phen != 0.0);

		// ... on to next individual
		vegetation.nextobj();
	}
}

///////////////////////////////////////////////////////////////////////////////////////
// TURNOVER
// Internal function (do not call directly from framework)

void turnover(double turnover_leaf, double turnover_root, double turnover_sap,
		lifeformtype lifeform, double& cmass_leaf, double& cmass_root,
		double& cmass_sap, double& cmass_heart, double& litter_leaf,
		double& litter_root, bool alive) {

	// guess2008 - new (indiv.)alive boolean throughout

	// DESCRIPTION
	// Transfers carbon from leaves and roots to litter, and from sapwood to heartwood
	// Only turnover from 'alive' individuals is transferred to litter (Ben 2007-11-28)

	// INPUT PARAMETERS
	// turnover_leaf = leaf turnover per time period as a proportion of leaf C biomass
	// turnover_root = root turnover per time period as a proportion of root C biomass
	// turnover_sap  = sapwood turnover to heartwood per time period as a proportion of
	//                 sapwood C biomass
	// lifeform      = PFT life form class (TREE or GRASS)
	// alive         = signifies new Individual object if false (see vegdynam.cpp)

	// INPUT AND OUTPUT PARAMETERS
	// cmass_leaf    = leaf C biomass (kgC/m2)
	// cmass_root    = fine root C biomass (kgC/m2)
	// cmass_sap     = sapwood C biomass (kgC/m2)

	// OUTPUT PARAMETERS
	// litter_leaf   = new leaf litter (kgC/m2)
	// litter_root   = new root litter (kgC/m2)
	// cmass_heart   = heartwood C biomass (kgC/m2)

	double turnover;

	// TREES AND GRASSES:

	// Leaf turnover
	turnover = turnover_leaf * cmass_leaf;
	cmass_leaf -= turnover;
	if (alive)
		litter_leaf += turnover;

	// Root turnover
	turnover = turnover_root * cmass_root;
	cmass_root -= turnover;
	if (alive)
		litter_root += turnover;

	if (lifeform == TREE) {

		// TREES ONLY:

		// Sapwood turnover by conversion to heartwood
		turnover = turnover_sap * cmass_sap;
		cmass_sap -= turnover;
		cmass_heart += turnover;
	}
}

void turnover_oecd(double turnover_leaf, double turnover_root,
		double turnover_sap, lifeformtype lifeform, double& cmass_leaf,
		double& cmass_root, double& cmass_sap, double& cmass_heart,
		double& litter_leaf, double& litter_root, Fluxes& fluxes, bool alive) {

	// DESCRIPTION
	// Transfers carbon from leaves and roots to litter, and from sapwood to heartwood
	// Version for OECD experiment:
	// For crops (specially labelled grass type) 50% of above-ground biomass transferred
	// to litter, remainder stored as a flux to the atmosphere (i.e. increments Rh)
	// (equal amount for each month)

	// guess2008 - new (indiv.)alive boolean throughout. Also, only turnover from 'alive' 
	// individuals is transferred to litter

	double turnover = 0.0;
	int m;

	if (lifeform == CROP) {

		if (alive)
			litter_root += cmass_root;
		cmass_root = 0.0;

		turnover = 0.5 * cmass_leaf;
		fluxes.acflux_soil += turnover;
		if (alive)
			litter_leaf += turnover;
		cmass_leaf = 0.0;

		turnover /= 12.0;
		for (m = 0; m < 12; m++)
			fluxes.mcflux_soil[m] += turnover;
	} else {

		// TREES AND GRASSES:

		// Leaf turnover
		turnover = turnover_leaf * cmass_leaf;
		cmass_leaf -= turnover;
		if (alive)
			litter_leaf += turnover;

		// Root turnover
		turnover = turnover_root * cmass_root;
		cmass_root -= turnover;
		if (alive)
			litter_root += turnover;

		if (lifeform == TREE) {

			// TREES ONLY:

			// Sapwood turnover by conversion to heartwood
			turnover = turnover_sap * cmass_sap;
			cmass_sap -= turnover;
			cmass_heart += turnover;
		}

	}
}

///////////////////////////////////////////////////////////////////////////////////////
// REPRODUCTION
// Internal function (do not call directly from framework)

void reproduction(double reprfrac, double npp, double& bminc,
		double& cmass_repr) {

	// DESCRIPTION
	// Allocation of net primary production (NPP) to reproduction and calculation of
	// assimilated carbon available for production of new biomass

	// INPUT PARAMETERS
	// reprfrac = fraction of NPP for this time period allocated to reproduction
	// npp      = NPP (i.e. assimilation minus maintenance and growth respiration) for
	//            this time period (kgC/m2)

	// OUTPUT PARAMETER
	// bminc    = carbon biomass increment (component of NPP available for production
	//            of new biomass) for this time period (kgC/m2)

	if (npp >= 0.0) {
		cmass_repr = npp * reprfrac;
		bminc = npp - cmass_repr;
		return;
	}

	// Negative NPP - no reproduction cost

	cmass_repr = 0.0;
	bminc = npp;
}

///////////////////////////////////////////////////////////////////////////////////////
// ALLOCATION
// Function allocation is an internal function (do not call directly from framework);
// function allocation_init may be called to distribute initial biomass among tissues
// for a new individual.

// File scope global variables: used by function f below (see function allocation)

static double k1, k2, k3, b;
static double ltor_g;
static double cmass_heart_g;
static double cmass_leaf_g;

inline double f(double& cmass_leaf_inc) {

	// Returns value of f(cmass_leaf_inc), given by:
	//
	// f(cmass_leaf_inc) = 0 =
	//   k1 * (b - cmass_leaf_inc - cmass_leaf_inc/ltor + cmass_heart) -
	//   [ (b - cmass_leaf_inc - cmass_leaf_inc/ltor)
	//   / (cmass_leaf + cmass_leaf_inc )*k3 ] ** k2
	//
	// See function allocation (below), Eqn (13)

	return k1 * (b - cmass_leaf_inc - cmass_leaf_inc / ltor_g + cmass_heart_g)
			- pow(
					(b - cmass_leaf_inc - cmass_leaf_inc / ltor_g)
					/ (cmass_leaf_g + cmass_leaf_inc) * k3, k2);
}

void allocation(double bminc, double cmass_leaf, double cmass_root,
		double cmass_sap, double cmass_debt, double cmass_heart, double ltor,
		double height, double sla, double wooddens, lifeformtype lifeform,
		double k_latosa, double k_allom2, double k_allom3,
		double& cmass_leaf_inc, double& cmass_root_inc, double& cmass_sap_inc,
		double& cmass_debt_inc, double& cmass_heart_inc,
		double& litter_leaf_inc, double& litter_root_inc) {

	// DESCRIPTION
	// Calculates changes in C compartment sizes (leaves, roots, sapwood, heartwood)
	// and litter for a plant individual as a result of allocation of biomass increment.
	// Assumed allometric relationships are given in function allometry below.

	// INPUT PARAMETERS
	// bminc       = biomass increment this time period on individual basis (kgC)
	// cmass_leaf  = leaf C biomass for last time period on individual basis (kgC)
	// cmass_root  = root C biomass for last time period on individual basis (kgC)
	// cmass_sap   = sapwood C biomass for last time period on individual basis (kgC)
	// cmass_heart = heartwood C biomass for last time period on individual basis (kgC)
	// ltor        = leaf to root mass ratio following allocation
	// height      = individual height (m)
	// sla         = specific leaf area (PFT-specific constant) (m2/kgC)
	// wooddens    = wood density (PFT-specific constant) (kgC/m3)
	// lifeform    = life form class (TREE or GRASS)
	// k_latosa    = ratio of leaf area to sapwood cross-sectional area (PFT-specific
	//               constant)
	// k_allom2    = constant in allometry equations
	// k_allom3    = constant in allometry equations

	// OUTPUT PARAMETERS
	// cmass_leaf_inc  = increment (may be negative) in leaf C biomass following
	//                   allocation (kgC)
	// cmass_root_inc  = increment (may be negative) in root C biomass following
	//                   allocation (kgC)
	// cmass_sap_inc   = increment (may be negative) in sapwood C biomass following
	//                   allocation (kgC)
	// cmass_heart_inc = increment in heartwood C biomass following allocation (kgC)
	// litter_leaf_inc = increment in leaf litter following allocation, on individual 
	//                   basis (kgC)
	// litter_root_inc = increment in root litter following allocation, on individual
	//                   basis (kgC)

	// MATHEMATICAL DERIVATION FOR TREE ALLOCATION
	// Allocation attempts to distribute biomass increment (bminc) among the living
	// tissue compartments, i.e.
	//   (1) bminc = cmass_leaf_inc + cmass_root_inc + cmass_sap_inc
	// while satisfying the allometric relationships (Shinozaki et al. 1964a,b; Waring
	// et al 1982, Huang et al 1992; see also function allometry, below) [** =
	// raised to the power of]:
	//   (2) (leaf area) = k_latosa * (sapwood xs area)
	//   (3) cmass_leaf = ltor * cmass_root
	//   (4) height = k_allom2 * (stem diameter) ** k_allom3
	// From (1) and (3),
	//   (5) cmass_sap_inc = bminc - cmass_leaf_inc -
	//         (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root
	// Let diam_new and height_new be stem diameter and height following allocation.
	// Then (see allometry),
	//   (6) diam_new = 2 * [ ( cmass_sap + cmass_sap_inc + cmass_heart )
	//         / wooddens / height_new / PI ]**(1/2)
	// From (4), (6) and (5),
	//   (7) height_new**(1+2/k_allom3) = 
	//         k_allom2**(2/k_allom3) * 4 * [cmass_sap + bminc - cmass_leaf_inc
	//         - (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root + cmass_heart]
	//         / wooddens / PI
	// Now,
	//   (8) wooddens = cmass_sap / height / (sapwood xs area)
	// From (8) and (2),
	//   (9) wooddens = cmass_sap / height / sla / cmass_leaf * k_latosa
	// From (9) and (1),
	//  (10) wooddens = (cmass_sap + bminc - cmass_leaf_inc -
	//         (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root)
	//          / height_new / sla / (cmass_leaf + cmass_leaf_inc) * k_latosa
	// From (10),
	//  (11) height_new**(1+2/k_allom3) =
	//         [ (cmass_sap + bminc - cmass_leaf_inc - (cmass_leaf + cmass_leaf_inc)
	//           / ltor + cmass_root) / wooddens / sla
	//           / (cmass_leaf + cmass_leaf_inc ) * k_latosa ] ** (1+2/k_allom3)
	//
	// Combining (7) and (11) gives a function of the unknown cmass_leaf_inc:
	//
	//  (12) f(cmass_leaf_inc) = 0 =
	//         k_allom2**(2/k_allom3) * 4/PI * [cmass_sap + bminc - cmass_leaf_inc
	//         - (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root + cmass_heart]
	//         / wooddens -
	//         [ (cmass_sap + bminc - cmass_leaf_inc - (cmass_leaf + cmass_leaf_inc)
	//           / ltor + cmass_root) / (cmass_leaf + cmass_leaf_inc)
	//           / wooddens / sla * k_latosa] ** (1+2/k_allom3)
	//
	// Let k1 = k_allom2**(2/k_allom3) * 4/PI / wooddens
	//     k2 = 1+2/k_allom3
	//     k3 = k_latosa / wooddens / sla
	//     b  = cmass_sap + bminc - cmass_leaf/ltor + cmass_root
	//
	// Then,
	//  (13) f(cmass_leaf_inc) = 0 =
	//         k1 * (b - cmass_leaf_inc - cmass_leaf_inc/ltor + cmass_heart) -
	//         [ (b - cmass_leaf_inc - cmass_leaf_inc/ltor)
	//         / (cmass_leaf + cmass_leaf_inc )*k3 ] ** k2
	//
	// Numerical methods are used to solve Eqn (13) for cmass_leaf_inc

	const int NSEG = 20; // number of segments (parameter in numerical methods)
	const int JMAX = 40; // maximum number of iterations (in numerical methods)
	const double XACC = 0.0001; // threshold x-axis precision of allocation solution
	const double YACC = 1.0e-10; // threshold y-axis precision of allocation solution
	const double PI = 3.14159265;
	const double CDEBT_MAXLOAN_DEFICIT = 0.8; // maximum loan as a fraction of deficit
	const double CDEBT_MAXLOAN_MASS = 0.2; // maximum loan as a fraction of (sapwood-cdebt)

	double cmass_leaf_inc_min;
	double cmass_root_inc_min;
	double x1, x2, dx, xmid, fx1, fmid, rtbis, sign;
	int j;
	double cmass_deficit, cmass_loan;

	litter_leaf_inc = 0.0;
	litter_root_inc = 0.0;
	cmass_root_inc = 0.0; // guess2008 - initialise

	if (ltor < 1.0e-10) {

		// No leaf production possible - put all biomass into roots
		// (Individual will die next time period)

		cmass_leaf_inc = 0.0;
		cmass_root_inc = bminc;

		if (lifeform == TREE) {
			cmass_sap_inc = -cmass_sap;
			cmass_heart_inc = -cmass_sap_inc;
		}

		return;
	}

	if (lifeform == TREE) {

		// TREE ALLOCATION

		cmass_heart_inc = 0.0;

		// Calculate minimum leaf increment to maintain current sapwood biomass
		// Given Eqn (2)

		if (height > 0.0)
			cmass_leaf_inc_min = k_latosa * cmass_sap
			/ (wooddens * height * sla) - cmass_leaf;
		else
			cmass_leaf_inc_min = 0.0;

		// Calculate minimum root increment to support minimum resulting leaf biomass
		// Eqn (3)

		if (height > 0.0)
			cmass_root_inc_min = k_latosa * cmass_sap
			/ (wooddens * height * sla * ltor) - cmass_root;
		else
			cmass_root_inc_min = 0.0;

		if (cmass_root_inc_min < 0.0) { // some roots would have to be killed

			cmass_leaf_inc_min = cmass_root * ltor - cmass_leaf;
			cmass_root_inc_min = 0.0;
		}

		// BLARP! C debt stuff
		if (ifcdebt) {
			cmass_deficit = cmass_leaf_inc_min + cmass_root_inc_min - bminc;
			if (cmass_deficit > 0.0) {
				cmass_loan = max(
						min(cmass_deficit * CDEBT_MAXLOAN_DEFICIT,
								(cmass_sap - cmass_debt) * CDEBT_MAXLOAN_MASS),
								0.0);
				bminc += cmass_loan;
				cmass_debt_inc = cmass_loan;
			} else
				cmass_debt_inc = 0.0;
		} else
			cmass_debt_inc = 0.0;

		if ((cmass_root_inc_min >= 0.0 && cmass_leaf_inc_min >= 0.0
				&& cmass_root_inc_min + cmass_leaf_inc_min <= bminc)
				|| (bminc <= 0.0)) {

			// Normal allocation (positive increment to all living C compartments)
			// NOTE: includes allocation of zero or negative NPP, c.f. LPJF

			// Calculation of leaf mass increment (lminc_ind) satisfying Eqn (13)
			// using bisection method (Press et al 1986)

			// Set values for global variables for reuse by function f

			k1 = pow(k_allom2, 2.0 / k_allom3) * 4.0 / PI / wooddens;
			k2 = 1.0 + 2 / k_allom3;
			k3 = k_latosa / wooddens / sla;
			b = cmass_sap + bminc - cmass_leaf / ltor + cmass_root;
			ltor_g = ltor;
			cmass_leaf_g = cmass_leaf;
			cmass_heart_g = cmass_heart;

			x1 = 0.0;
			x2 = (bminc - (cmass_leaf / ltor - cmass_root))
							/ (1.0 + 1.0 / ltor);
			dx = (x2 - x1) / (double) NSEG;

			if (cmass_leaf < 1.0e-10)
				x1 += dx; // to avoid division by zero

			// Evaluate f(x1), i.e. Eqn (13) at cmass_leaf_inc = x1

			fx1 = f(x1);

			// Find approximate location of leftmost root on the interval
			// (x1,x2).  Subdivide (x1,x2) into nseg equal segments seeking
			// change in sign of f(xmid) relative to f(x1).

			fmid = f(x1);

			xmid = x1;

			while (fmid * fx1 > 0.0 && xmid < x2) {

				xmid += dx;
				fmid = f(xmid);
			}

			x1 = xmid - dx;
			x2 = xmid;

			// Apply bisection to find root on new interval (x1,x2)

			if (f(x1) >= 0.0)
				sign = -1.0;
			else
				sign = 1.0;

			rtbis = x1;
			dx = x2 - x1;

			// Bisection loop
			// Search iterates on value of xmid until xmid lies within
			// xacc of the root, i.e. until |xmid-x|<xacc where f(x)=0

			fmid = 1.0; // dummy value to guarantee entry into loop
			j = 0; // number of iterations so far

			while (dx >= XACC && fabs(fmid) > YACC && j <= JMAX) {

				dx *= 0.5;
				xmid = rtbis + dx;

				fmid = f(xmid);

				if (fmid * sign <= 0.0)
					rtbis = xmid;
				j++;
			}

			// Now rtbis contains numerical solution for cmass_leaf_inc given Eqn (13)

			cmass_leaf_inc = rtbis;

			// Calculate increments in other compartments

			cmass_root_inc = (cmass_leaf_inc + cmass_leaf) / ltor - cmass_root; // Eqn (3)
			cmass_sap_inc = bminc - cmass_leaf_inc - cmass_root_inc; // Eqn (1)

			// guess2008 - extra check - abnormal allocation can still happen if ltor is very small
			if ((cmass_root_inc > 50 || cmass_root_inc < -50)
					&& ltor < 0.0001) {
				cmass_leaf_inc = 0.0;
				cmass_root_inc = bminc;

				if (lifeform == TREE) {
					cmass_sap_inc = -cmass_sap;
					cmass_heart_inc = -cmass_sap_inc;
				}

				return;
			}

		} else {

			// Abnormal allocation: reduction in some biomass compartment(s) to
			// satisfy allometry

			// Attempt to distribute this year's production among leaves and roots only
			// Eqn (3)

			cmass_leaf_inc = (bminc - cmass_leaf / ltor + cmass_root)
							/ (1.0 + 1.0 / ltor);

			if (cmass_leaf_inc > 0.0) {

				// Positive allocation to leaves

				cmass_root_inc = bminc - cmass_leaf_inc; // Eqn (1)

				// Add killed roots (if any) to litter

				// guess2008 - back to LPJF method in this case
				// if (cmass_root_inc<0.0) litter_root_inc=-cmass_root_inc;
				if (cmass_root_inc < 0.0) {
					cmass_leaf_inc = bminc;
					cmass_root_inc = (cmass_leaf_inc + cmass_leaf) / ltor
							- cmass_root; // Eqn (3)
					litter_root_inc = -cmass_root_inc;
				}

			} else {

				// Negative or zero allocation to leaves
				// Eqns (1), (3)

				cmass_root_inc = bminc;
				cmass_leaf_inc = (cmass_root + cmass_root_inc) * ltor
						- cmass_leaf;

				// Add killed leaves to litter

				litter_leaf_inc = -cmass_leaf_inc;

			}

			// Calculate increase in sapwood mass (which must be negative)
			// Eqn (2)

			cmass_sap_inc = (cmass_leaf_inc + cmass_leaf) * wooddens * height
					* sla / k_latosa - cmass_sap;

			// Convert killed sapwood to heartwood

			cmass_heart_inc = -cmass_sap_inc;
		}
	} else if (lifeform == GRASS || lifeform == CROP) {

		// GRASS ALLOCATION
		// Allocation attempts to distribute biomass increment (bminc) among leaf
		// and root compartments, i.e.
		//   (14) bminc = cmass_leaf_inc + cmass_root_inc
		// while satisfying Eqn(3)

		cmass_leaf_inc = (bminc - cmass_leaf / ltor + cmass_root)
						/ (1.0 + 1.0 / ltor);
		cmass_root_inc = bminc - cmass_leaf_inc;

		if (cmass_leaf_inc < 0.0) {

			// Negative allocation to leaves

			cmass_root_inc = bminc;
			cmass_leaf_inc = (cmass_root + cmass_root_inc) * ltor - cmass_leaf; // Eqn (3)

			// Add killed leaves to litter

			cmass_leaf_inc = (cmass_root + cmass_root_inc) * ltor - cmass_leaf; // Eqn (3)

			// Add killed leaves to litter

			// guess2008 - bugfix 
			// litter_leaf_inc=-cmass_leaf_inc;
			litter_leaf_inc = min(-cmass_leaf_inc, cmass_leaf);
		} else if (cmass_root_inc < 0.0) {

			// Negative allocation to roots

			cmass_leaf_inc = bminc;
			cmass_root_inc = (cmass_leaf + bminc) / ltor - cmass_root;

			// Add killed roots to litter

			// guess2008 - bugfix 
			//litter_root_inc=-cmass_root_inc;
			litter_root_inc = min(-cmass_root_inc, cmass_root);

		}
	}
}

void allocation_init(double bminit, double ltor, Individual& indiv) {

	// DESCRIPTION
	// Allocates initial biomass among tissues for a new individual (tree or grass),
	// assuming standard LPJ allometry (see functions allocation, allometry).

	// INPUT PARAMETERS
	// bminit = initial total biomass (kgC)
	// ltor   = initial leaf:root biomass ratio
	//
	// Note: indiv.densindiv (density of individuals across patch or modelled area)
	//       should be set to a meaningful value before this function is called

	double dval;
	double cmass_leaf_ind;
	double cmass_root_ind;
	double cmass_sap_ind;

	allocation(bminit, 0.0, 0.0, 0.0, 0.0, 0.0, ltor, 0.0, indiv.pft.sla,
			indiv.pft.wooddens, indiv.pft.lifeform, indiv.pft.k_latosa,
			indiv.pft.k_allom2, indiv.pft.k_allom3, cmass_leaf_ind,
			cmass_root_ind, cmass_sap_ind, dval, dval, dval, dval);

	indiv.cmass_leaf = cmass_leaf_ind * indiv.densindiv;
	indiv.cmass_root = cmass_root_ind * indiv.densindiv;
	/*dprintf("cmass_leaf_ind %f\n", cmass_leaf_ind);
	 dprintf("cmass_leaf_TOTAL %f\n", indiv.cmass_leaf);
	 dprintf("cmass_root %f\n", indiv.cmass_root);
	 dprintf("cmass_heart %f\n", indiv.cmass_heart);
	 dprintf("cmass_sap %f\n", indiv.cmass_sap);*/
	//dprintf("cmass_repr %f\n", stand.pft[indiv.pft.id].cmass_repr);

	if (indiv.pft.lifeform == TREE)
		indiv.cmass_sap = cmass_sap_ind * indiv.densindiv;
}

///////////////////////////////////////////////////////////////////////////////////////
// ALLOMETRY
// Should be called to update allometry, FPC and FPC increment whenever biomass values
// for a vegetation individual change.

bool allometry(Individual& indiv) {

	// DESCRIPTION
	// Calculates tree allometry (height and crown area) and fractional projective
	// given carbon biomass in various compartments for an individual.

	// Returns true if the allometry is normal, otherwise false - guess2008

	// TREE ALLOMETRY
	// Trees aboveground allometry is modelled by a cylindrical stem comprising an
	// inner cylinder of heartwood surrounded by a zone of sapwood of constant radius,
	// and a crown (i.e. foliage) cylinder of known diameter. Sapwood and heartwood are
	// assumed to have the same, constant, density (wooddens). Tree height is related
	// to sapwood cross-sectional area by the relation:
	//   (1) height = cmass_sap / (sapwood xs area)
	// Sapwood cross-sectional area is also assumed to be a constant proportion of
	// total leaf area (following the "pipe model"; Shinozaki et al. 1964a,b; Waring
	// et al 1982), i.e.
	//   (2) (leaf area) = k_latosa * (sapwood xs area)
	// Leaf area is related to leaf biomass by specific leaf area:
	//   (3) (leaf area) = sla * cmass_leaf
	// From (1), (2), (3),
	//   (4) height = cmass_sap / wooddens / sla / cmass_leaf * k_latosa
	// Tree height is related to stem diameter by the relation (Huang et al 1992)
	// [** = raised to the power of]:
	//   (5) height = k_allom2 * diam ** k_allom3
	// Crown area may be derived from stem diameter by the relation (Zeide 1993):
	//   (6) crownarea = min ( k_allom1 * diam ** k_rp , crownarea_max )
	// Bole height (individual/cohort mode only; currently set to 0):
	//   (7) boleht = 0

	// FOLIAR PROJECTIVE COVER (FPC)
	// The same formulation for FPC (Eqn 8 below) is now applied in all vegetation
	// modes (Ben Smith 2002-07-23). FPC is equivalent to fractional patch/grid cell
	// coverage for the purposes of canopy exchange calculations and, in population
	// mode, vegetation dynamics calculations.
	//
	//   FPC on the modelled area (stand, patch, "grid-cell") basis is related to mean
	//   individual leaf area index (LAI) by the Lambert-Beer law (Monsi & Saeki 1953,
	//   Prentice et al 1993) based on the assumption that success of a PFT population
	//   in competition for space will be proportional to competitive ability for light
	//   in the vertical profile of the forest canopy:
	//     (8) fpc = crownarea * densindiv * ( 1.0 - exp ( -0.5 * lai_ind ) )
	//   where
	//     (9) lai_ind = cmass_leaf/densindiv * sla / crownarea
	//
	//   For grasses,
	//    (10) fpc = ( 1.0 - exp ( -0.5 * lai_ind ) )
	//    (11) lai_ind = cmass_leaf * sla

	double diam; // stem diameter (m)
	double fpc_new; // updated FPC

	// guess2008 - max tree height allowed (metre).
	double HEIGHT_MAX = 150;

	if (indiv.pft.lifeform == TREE) {

		// TREES

		// Height (Eqn 4)

		// guess2008 - new allometry check 
		if (!negligible(indiv.cmass_leaf)) {
			indiv.height = indiv.cmass_sap / indiv.cmass_leaf / indiv.pft.sla
					* indiv.pft.k_latosa / indiv.pft.wooddens;
			//dprintf(" height %f", indiv.height);
			//dprintf(" dens %f", indiv.densindiv);
			//dprintf(" cmass_sap %f", indiv.cmass_sap);
			//dprintf(" cmass_leaf %f\n", indiv.cmass_leaf);
			// Stem diameter (Eqn 5)
			diam = pow(indiv.height / indiv.pft.k_allom2,
					1.0 / indiv.pft.k_allom3);

			// Stem volume
			double vol = indiv.height * 3.1415927 * diam * diam * 0.25;
			if (indiv.age
					&& (indiv.cmass_heart + indiv.cmass_sap) / indiv.densindiv
					/ vol < indiv.pft.wooddens * 0.9)
				return false;
		} else {
			indiv.height = 0.0;
			diam = 0.0;
			return false;
		}

		// guess2008 - extra height check
		if (indiv.pft.ifsage == 1) { //KMR added new max height for sage
			HEIGHT_MAX = 3;
		}
		if (indiv.height > HEIGHT_MAX) {
			dprintf("Max Height Hit!");
			indiv.height = 0.0;
			diam = 0.0;
			return false;
		}

		// Crown area (Eqn 6)
		indiv.crownarea = min(indiv.pft.k_allom1 * pow(diam, indiv.pft.k_rp),
				indiv.pft.crownarea_max);

		if (!negligible(indiv.crownarea)) {

			// Individual LAI (Eqn 9)
			indiv.lai_indiv = indiv.cmass_leaf / indiv.densindiv * indiv.pft.sla
					/ indiv.crownarea;
			//dprintf("date = %i/", date.year);
			//dprintf("%i ", date.day);
			//dprintf("indiv.lai = %f\n", indiv.lai);
			// FPC (Eqn 8)

			fpc_new = indiv.crownarea * indiv.densindiv
					* (1.0 - exp(-LAMBERTBEER_K * indiv.lai_indiv));

			// Increment deltafpc
			indiv.deltafpc += fpc_new - indiv.fpc;
			indiv.fpc = fpc_new;
		} else {
			indiv.lai_indiv = 0.0;
			indiv.fpc = 0.0;
		}

		// Bole height (Eqn 7)
		indiv.boleht = 0.0;

		// Stand-level LAI
		indiv.lai = indiv.cmass_leaf * indiv.pft.sla;
	} else if (indiv.pft.lifeform == GRASS || indiv.pft.lifeform == CROP) {

		// GRASSES

		// guess2008 - bugfix - added if 
		if (!negligible(indiv.cmass_leaf)) {

			// Grass "individual" LAI (Eqn 11)
			indiv.lai_indiv = indiv.cmass_leaf * indiv.pft.sla;

			// FPC (Eqn 10)
			indiv.fpc = 1.0 - exp(-LAMBERTBEER_K * indiv.lai_indiv);

			// Stand-level LAI
			indiv.lai = indiv.lai_indiv;
		} else
			return false;

	}

	// guess2008 - new return value (was void)
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////
// RELATIVE CHANGE IN BIOMASS
// Call this function to calculate the change in biomass on a grid cell area basis
// associated with a specified change in FPC

double fracmass_lpj(double fpc_low, double fpc_high, Individual& indiv) {

	// DESCRIPTION
	// Calculates and returns new biomass as a fraction of old biomass given an FPC
	// reduction from fpc_high to fpc_low, assuming LPJ allometry (see function
	// allometry)

	// guess2008 - check
	if (fpc_high < fpc_low)
		fail("fracmass_lpj: fpc_high < fpc_low");

	if (indiv.pft.lifeform == TREE) {

		if (negligible(fpc_high))
			return 1.0;

		// else
		return fpc_low / fpc_high;
	} else if (indiv.pft.lifeform == GRASS || indiv.pft.lifeform == CROP) { // grass

		if (fpc_high >= 1.0 || fpc_low >= 1.0 || negligible(indiv.cmass_leaf))
			return 1.0;

		// else
		return 1.0
				+ 2.0 / indiv.cmass_leaf / indiv.pft.sla
				* (log(1.0 - fpc_high) - log(1.0 - fpc_low));
	} else {
		fail("fracmass_lpj: unknown lifeform");
		return 0;
	}

	// This point will never be reached in practice, but to satisfy more pedantic
	// compilers ...

	return 1.0;
}

///////////////////////////////////////////////////////////////////////////////////////
// GROWTH
// Should be called by framework at the end of each simulation year for modelling
// of turnover, allocation and growth, prior to vegetation dynamics and disturbance

void growth(Stand& stand, Patch& patch) {

	// DESCRIPTION
	// Tissue turnover and allocation of fixed carbon to reproduction and new biomass
	// Accumulated NPP (assimilation minus maintenance and growth respiration) on
	// patch or modelled area basis assumed to be given by 'anpp' member variable for
	// each individual.

	// guess2008 - minimum carbon mass allowed (kgC/m2)
	const double MINCMASS = 1.0e-8;

	const double CDEBT_PAYBACK_RATE = 0.2;

	double bminc;
	// carbon biomass increment (component of NPP available for production of
	// new biomass) for this time period on modelled area basis (kgC/m2)
	double cmass_repr;
	// C allocated to reproduction this time period on modelled area basis (kgC/m2)
	double cmass_leaf_inc;
	// increment in leaf C biomass following allocation, on individual basis (kgC)
	double cmass_root_inc;
	// increment in root C biomass following allocation, on individual basis (kgC)
	double cmass_sap_inc;
	// increment in sapwood C biomass following allocation, on individual basis
	// (kgC)
	double cmass_debt_inc = 0.0;
	// guess2008 - bugfix - added initialisation
	double cmass_heart_inc;
	// increment in heartwood C biomass following allocation, on individual basis
	// (kgC)
	double litter_leaf_inc = 0.0; // guess2008 - bugfix - added initialisation
	// increment in leaf litter following allocation, on individual basis (kgC)
	double litter_root_inc = 0.0; // guess2008 - bugfix - added initialisation
	// increment in root litter following allocation, on individual basis (kgC)
	double cmass_excess;
	// C biomass of leaves in "excess" of set allocated last year to raingreen PFT
	// last year (kgC/m2)
	double dval;
	double cmass_payback;
	int p;
	bool killed;

	// Obtain reference to Vegetation object for this patch
	Vegetation& vegetation = patch.vegetation;

	// On first call to function growth this year (patch #0), initialise stand-PFT
	// record of summed allocation to reproduction

	if (!patch.id)
		for (p = 0; p < npft; p++)
			stand.pft[p].cmass_repr = 0.0;

	// Loop through individuals

	if (date.year > 152)
		int test = 0;

	vegetation.firstobj();
	while (vegetation.isobj) {
		Individual& indiv = vegetation.getobj();

		// For this individual ...

		indiv.deltafpc = 0.0;

		killed = false;

		// Set leaf:root mass ratio based on water stress parameter
		indiv.ltor = indiv.wscal_mean * indiv.pft.ltor_max;

		if (negligible(indiv.densindiv))
			fail("growth: negligible densindiv for %s", (char*) indiv.pft.name);

		else {

			// Allocation to reproduction

			reproduction(indiv.pft.reprfrac, indiv.anpp, bminc, cmass_repr);

			// guess2008 - added bminc check. Otherwise we get -ve litter_leaf for grasses when indiv.anpp < 0.
			if (bminc >= 0
					&& (indiv.pft.phenology == RAINGREEN
							|| indiv.pft.phenology == ANY)) {

				// Raingreen PFTs: reduce biomass increment to account for NPP
				// allocated to extra leaves during the past year.
				// Excess allocation to leaves given by:
				//   aphen_raingreen / ( leaf_longevity * 365) * cmass_leaf -
				//   cmass_leaf

				// BLARP! excess allocation to roots now also included (assumes leaf longevity = root longevity)

				cmass_excess = max(
						(double) indiv.aphen_raingreen
						/ (indiv.pft.leaflong * 365.0)
						* (indiv.cmass_leaf + indiv.cmass_root)
						- indiv.cmass_leaf - indiv.cmass_root, 0.0);

				if (cmass_excess > bminc)
					cmass_excess = bminc;

				// Transfer excess leaves to litter
				// guess2008 - only for 'alive' individuals
				if (indiv.alive)
					patch.pft[indiv.pft.id].litter_leaf += cmass_excess;

				// Deduct from this year's C biomass increment
				// guess2008 - bugfix - added alive check
				if (indiv.alive)
					bminc -= cmass_excess;
			}

			// Tissue turnover and associated litter production
			turnover_oecd(indiv.pft.turnover_leaf, indiv.pft.turnover_root,
					indiv.pft.turnover_sap, indiv.pft.lifeform,
					indiv.cmass_leaf, indiv.cmass_root, indiv.cmass_sap,
					indiv.cmass_heart, patch.pft[indiv.pft.id].litter_leaf,
					patch.pft[indiv.pft.id].litter_root, patch.fluxes,
					indiv.alive);

			// Update stand record of reproduction by this PFT
			stand.pft[indiv.pft.id].cmass_repr += cmass_repr / (double) npatch;

			// Transfer reproduction straight to litter
			// guess2008 - only for 'alive' individuals
			if (indiv.alive)
				patch.pft[indiv.pft.id].litter_repr += cmass_repr;

			if (indiv.pft.lifeform == TREE) {

				// TREE GROWTH

				// BLARP! Try and pay back part of cdebt

				if (ifcdebt && bminc > 0.0) {
					cmass_payback = min(indiv.cmass_debt * CDEBT_PAYBACK_RATE,
							bminc);
					bminc -= cmass_payback;
					indiv.cmass_debt -= cmass_payback;
				}

				// Allocation: note conversion of mass values from grid cell area
				// to individual basis

				allocation(bminc / indiv.densindiv,
						indiv.cmass_leaf / indiv.densindiv,
						indiv.cmass_root / indiv.densindiv,
						indiv.cmass_sap / indiv.densindiv,
						indiv.cmass_debt / indiv.densindiv,
						indiv.cmass_heart / indiv.densindiv, indiv.ltor,
						indiv.height, indiv.pft.sla, indiv.pft.wooddens, TREE,
						indiv.pft.k_latosa, indiv.pft.k_allom2,
						indiv.pft.k_allom3, cmass_leaf_inc, cmass_root_inc,
						cmass_sap_inc, cmass_debt_inc, cmass_heart_inc,
						litter_leaf_inc, litter_root_inc);

				// Update carbon pools and litter (on area basis)
				// (litter not accrued for not 'alive' individuals - Ben 2007-11-28)

				indiv.cmass_leaf += cmass_leaf_inc * indiv.densindiv;
				indiv.cmass_root += cmass_root_inc * indiv.densindiv;

				indiv.cmass_sap += cmass_sap_inc * indiv.densindiv;
				indiv.cmass_debt += cmass_debt_inc * indiv.densindiv;
				indiv.cmass_heart += cmass_heart_inc * indiv.densindiv;

				//dprintf(" sap inc %f", cmass_sap_inc*indiv.densindiv);
				//dprintf(" leaf inc %f", cmass_leaf_inc*indiv.densindiv);
				//dprintf(" root inc %f", cmass_root_inc*indiv.densindiv);
				//dprintf(" cmasssap %f\n", indiv.cmass_sap);
				//dprintf(" cmassleaf %f\n", indiv.cmass_leaf);
				//dprintf(" cmassroot %f\n", indiv.cmass_root);
				/*dprintf(" sap inc %f", cmass_sap_inc*indiv.densindiv);
				 dprintf(" leaf inc %f", cmass_leaf_inc*indiv.densindiv);
				 dprintf(" root inc %f", cmass_root_inc*indiv.densindiv);
				 dprintf(" cmasssap %f\n", indiv.cmass_sap);
				 dprintf(" cmassleaf %f\n", indiv.cmass_leaf);
				 dprintf(" cmassroot %f\n", indiv.cmass_root);
				 dprintf(" cmassroot %f\n", indiv.cmass_root);*/

				// guess2008
				if (indiv.alive) {
					patch.pft[indiv.pft.id].litter_leaf += litter_leaf_inc
							* indiv.densindiv;
					patch.pft[indiv.pft.id].litter_root += litter_root_inc
							* indiv.densindiv;
				}

				// Update individual age

				indiv.age++;

				// Kill individual and transfer biomass to litter if any biomass
				// compartment negative

				if (indiv.cmass_leaf < MINCMASS || indiv.cmass_root < MINCMASS
						|| indiv.cmass_sap < MINCMASS) {

					// guess2008 - alive check
					if (indiv.alive) {

						// guess2008 - catches small, negative values too
						patch.pft[indiv.pft.id].litter_leaf += indiv.cmass_leaf;
						patch.pft[indiv.pft.id].litter_root += indiv.cmass_root;
						patch.pft[indiv.pft.id].litter_wood += indiv.cmass_sap;

						patch.pft[indiv.pft.id].litter_wood += indiv.cmass_heart
								- indiv.cmass_debt;
					}

					vegetation.killobj();
					killed = true;
				}
			} else if (indiv.pft.lifeform == GRASS
					|| indiv.pft.lifeform == CROP) {

				// GRASS GROWTH

				// guess2008 - initial grass cmass
				double indiv_mass_before = indiv.cmass_leaf + indiv.cmass_root;

				allocation(bminc, indiv.cmass_leaf, indiv.cmass_root, 0.0, 0.0,
						0.0, indiv.ltor, 0.0, 0.0, 0.0, GRASS, 0.0, 0.0, 0.0,
						cmass_leaf_inc, cmass_root_inc, dval, dval, dval,
						litter_leaf_inc, litter_root_inc);

				// Update carbon pools and litter (on area basis)
				// only litter in the case of 'alive' individuals

				indiv.cmass_leaf += cmass_leaf_inc;
				indiv.cmass_root += cmass_root_inc;

				// guess2008 - bugfix - determine the (small) mass imbalance (kgC) for this individual. 
				// This can arise in the event of numerical errors in the allocation routine.
				double indiv_mass_after = indiv.cmass_leaf + indiv.cmass_root
						+ litter_leaf_inc + litter_root_inc;
				double indiv_cmass_diff = (indiv_mass_before + bminc
						- indiv_mass_after);

				// guess2008 - alive check before ensuring C balance
				if (indiv.alive) {

					patch.pft[indiv.pft.id].litter_leaf += litter_leaf_inc
							+ indiv_cmass_diff / 2;
					patch.pft[indiv.pft.id].litter_root += litter_root_inc
							+ indiv_cmass_diff / 2;

				}

				// Kill individual and transfer biomass to litter if either biomass
				// compartment negative

				if (indiv.cmass_leaf < MINCMASS
						|| indiv.cmass_root < MINCMASS) {

					// guess2008 - alive check
					if (indiv.alive) {

						patch.pft[indiv.pft.id].litter_leaf += indiv.cmass_leaf;
						patch.pft[indiv.pft.id].litter_root += indiv.cmass_root;

					}

					vegetation.killobj();
					killed = true;
				}
			}
		}

		// guess2008
		if (!killed) {

			if (!allometry(indiv)) {

				// guess2008 - bugfix - added this alive check
				if (indiv.alive) {
					patch.pft[indiv.pft.id].litter_leaf += max(indiv.cmass_leaf,
							0.0);
					patch.pft[indiv.pft.id].litter_root += max(indiv.cmass_root,
							0.0);
					patch.pft[indiv.pft.id].litter_wood += max(indiv.cmass_sap,
							0.0);
					patch.pft[indiv.pft.id].litter_wood += indiv.cmass_heart
							- indiv.cmass_debt;
				}

				vegetation.killobj();
				killed = true;
			}

			if (!killed) {
				if (!indiv.alive) {
					patch.fluxes.acflux_est -= indiv.cmass_leaf
							+ indiv.cmass_root + indiv.cmass_sap
							+ indiv.cmass_heart - indiv.cmass_debt;
					indiv.alive = true;
				}

				// ... on to next individual
				vegetation.nextobj();
			}
		}

	}
}

///////////////////////////////////////////////////////////////////////////////////////
// REFERENCES
//
// LPJF refers to the original FORTRAN implementation of LPJ as described by Sitch
//   et al 2001
// Huang, S, Titus, SJ & Wiens, DP (1992) Comparison of nonlinear height-diameter
//   functions for major Alberta tree species. Canadian Journal of Forest Research 22:
//   1297-1304
// Monsi M & Saeki T 1953 Ueber den Lichtfaktor in den Pflanzengesellschaften und
//   seine Bedeutung fuer die Stoffproduktion. Japanese Journal of Botany 14: 22-52
// Prentice, IC, Sykes, MT & Cramer W (1993) A simulation model for the transient
//   effects of climate change on forest landscapes. Ecological Modelling 65: 51-70.
// Press, WH, Teukolsky, SA, Vetterling, WT & Flannery, BT. (1986) Numerical
//   Recipes in FORTRAN, 2nd ed. Cambridge University Press, Cambridge
// Sitch, S, Prentice IC, Smith, B & Other LPJ Consortium Members (2000) LPJ - a
//   coupled model of vegetation dynamics and the terrestrial carbon cycle. In:
//   Sitch, S. The Role of Vegetation Dynamics in the Control of Atmospheric CO2
//   Content, PhD Thesis, Lund University, Lund, Sweden.
// Shinozaki, K, Yoda, K, Hozumi, K & Kira, T (1964) A quantitative analysis of
//   plant form - the pipe model theory. I. basic analyses. Japanese Journal of
//   Ecology 14: 97-105
// Shinozaki, K, Yoda, K, Hozumi, K & Kira, T (1964) A quantitative analysis of
//   plant form - the pipe model theory. II. further evidence of the theory and
//   its application in forest ecology. Japanese Journal of Ecology 14: 133-139
// Sykes, MT, Prentice IC & Cramer W 1996 A bioclimatic model for the potential
//   distributions of north European tree species under present and future climates.
//   Journal of Biogeography 23: 209-233.
// Waring, RH Schroeder, PE & Oren, R (1982) Application of the pipe model theory
//   to predict canopy leaf area. Canadian Journal of Forest Research 12:
//   556-560  
// Zeide, B (1993) Primary unit of the tree crown. Ecology 74: 1598-1602.

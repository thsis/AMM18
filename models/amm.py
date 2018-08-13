"""
Models for AMM 18.

Create handy class interface for estimation, calculation of elasticities and
simulation of price changes.
"""
import os
import itertools
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from copy import deepcopy


class AMM(object):
    """
    Model class for the 2018 Advanced Marketing Modelling lecture.

    Examples:
    >>> model = "L5+C(year)+price_liter+C(display_all)+C(feature_all)"
    >>> modifiers = {"DIET COKE": 0.25, "COKE CLASSIC": 1.25}
    >>> amm = AMM()
    >>> amm.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
    >>>         RHS=model,
    >>>         share="share",
    >>>         agg=["year", "week"])
    >>> amm.summary()
    >>> amm.simulate(modifiers)
    >>> amm.to_latex()
    """

    def __init__(self):
        self.data = None
        self.nests = None
        self.share = None
        self.agg = None
        self.grp_unit = None
        self.elasticities = None
        self.price_var = None
        self.formula = None
        self.table_elasticities = None
        self.RHS = None
        self.table_sim = None
        self.simulated = None
        self.simulation = None
        self.varlist = None
        self.alpha = None
        self.reference = None
        self.agg = None
        self.modifiers = None
        self.model = None
        self.res = None

    def fit(self, data, RHS, share, agg=None, nests=None, grp_unit="liters",
            price_var="price_liter"):
        """
        Fit AMM-model to data and calculate elasticities.

        The type of model is specified through the `nests` parameter. If
        `nests` is None (the default) an Aggregated Logit Model is fitted. If
        `nests` is a list, a Nested Aggregated Logit Model is fitted. The
        length of the list determines the nesting structure. A length of one
        corresponds to a 2 level Nested Aggregated Logit Model and a length of
        two corresponds to a 3 level Nested Aggregated Logit Model.

        Note that `data` has very specific assumptions in place. It assumes an
        IRI format which means it expects following columns to exist:

        * L5: brand names
        * *share*: precomputed shares for each observation.
        * *grp_unit*: precomputed measure of volume.
        * *price_var*: precomputed measure of price.

        Where L5 is necessary and each *column* can be customized to allow for
        different ways to infer the shares of the outside good (you don't need
        to calculate the outside good explicitly).

        For example: it is possible to compute shares of each product based on:

        * `total_rev_cola`
        * `total_rev_carbbev`
        * `total_vol_cola`
        * `total_vol_carbbev`

        Pick one, calculate the shares (by yourself), store the results as a
        column in data and set *share* to that column's name. Similarly, you
        need to proceed with *grp_unit* and *price_var*.

        Parameters:
        * `data`: a `pandas.DataFrame` formatted in the IRI format.
        * `RHS`: string of variables used in the OLS model.
        * `share`: column of data that stores the shares of the products.
        * `agg`: list of most coarse aggregation level. Defaults to `None`
        * `nests`: list of nesting structure.
        * `grp_unit`: column of data that specifies group aggregation units.
        * `price_var`: column of data that specifies prices to be used.

        Returns:
        * self

        Examples:
        >>> model = "L5+C(year)+price_liter+C(display_all)+C(feature_all)"
        >>> amm = AMM()
        >>> amm.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
        >>>         RHS=model,
        >>>         share="share",
        >>>         agg=["year", "week"])
        """
        # Store class attributes.
        self.data = deepcopy(data)
        self.RHS = RHS
        self.nests = nests
        self.share = share
        self.grp_unit = grp_unit
        self.price_var = price_var
        if agg:
            self.agg = agg
        else:
            self.agg = []
        # Calculate shares of OG and group shares.
        self.data["og"] = self.__get_og_share()

        if self.nests:
            # Define hierarchy
            self.structure = self.data[["L5"] + self.nests].drop_duplicates()
            self.structure = self.structure.set_index("L5").to_dict()

            grp_level = deepcopy(self.agg)
            for i, nest in enumerate(self.nests, 1):
                name = "G" + str(i)
                grp_level += [nest]
                self.data[name] = self.__get_grp_share(self.grp_unit,
                                                       grp_level)
        else:
            self.nests = []
        # Calculate target variable (i.e. LHS of model formula)
        self.data["delta"] = np.log(self.data[self.share])-np.log(self.data.og)
        # Parse Model formula.
        self.formula = "delta ~ -1 +" + self.RHS
        if self.nests:
            self.grp = ["+np.log(G"+str(i)+")" for i, _
                        in enumerate(self.nests, 1)]
            self.formula += ''.join(self.grp)
        # Fit Model.
        self.model = smf.ols(data=self.data, formula=self.formula)
        self.res = self.model.fit()
        # Save attributes.
        self.alpha = np.abs(self.res.params[self.price_var])
        if self.nests:
            self.sigma = [self.res.params['np.log(G'+str(i)+')'] for i, _ in
                          enumerate(self.nests, 1)]
        # Calculate Elasticities
        self.elasticities()
        return self

    def elasticities(self):
        self.elasticities = self.__get_elasticities(self.data,
                                                    "share",
                                                    "price_liter")
        if "i" in self.elasticities.columns:
            self.table_elasticities = pd.crosstab(
                values=self.elasticities.elasticity,
                columns=[self.elasticities.h, self.elasticities.k],
                index=[self.elasticities.i, self.elasticities.j],
                aggfunc=np.mean)
        else:
            self.table_elasticities = pd.crosstab(
                values=self.elasticities.elasticity,
                index=self.elasticities.j,
                columns=self.elasticities.k,
                aggfunc=np.mean)

        return self

    def simulate(self, modifiers={}):
        """
        Simulate price changes.

        Parameters:
        * `modifiers`: dictionary
            * key: brand, must appear in data's L5 column
            * value: multiplier of price

        Returns:
        * self

        Examples:
        >>> modifiers = {"DIET COKE": 0.25, "COKE CLASSIC": 1.25}
        >>> amm.simulate(modifiers)
        """
        self.simulated = deepcopy(self.data)
        self.reference = deepcopy(self.data)

        self.modifiers = modifiers
        self.simulated[self.price_var] = self.simulated.apply(self.__modify,
                                                              axis=1)
        # HACK: Set all shares to zero (log(1)) and use the predict method.
        self.simulated[self.share] = 1
        if self.nests:
            for i, _ in enumerate(self.nests, 1):
                self.simulated["G" + str(i)] = 1
                self.reference["G" + str(i)] = 1

        self.simulated["delta"] = self.res.predict(self.simulated)
        self.reference["delta"] = self.res.predict()

        if not self.nests:
            simulation = self.__predict_l1_shares(self.simulated)
        elif len(self.nests) == 1:
            simulation = self.__predict_l2_shares(self.simulated)
        elif len(self.nests) == 2:
            simulation = self.__predict_l3_shares(self.simulated)
        else:
            raise NotImplementedError

        dflist = [self.reference[["L5", self.share]+self.nests], simulation]
        self.simulation = pd.concat(dflist,
                                    axis=1)
        sim = self.simulation.groupby(self.nests + ["L5"]).mean()
        sim.columns = ["before", "after"]
        sim["change in percent"] = (sim["after"]-sim["before"]) / sim["before"]
        sim["change in percent"] *= 100

        self.table_sim = sim.T
        print("\nShares before and after price manipulation\n")
        print(self.table_sim)

    def summary(self):
        """
        Print summary of OLS-model and estimated elasticities to stdout.
        """
        print(self.res.summary())
        print()
        print("\nEstimated elasticities\n")
        print(self.table_elasticities)

    def to_latex(self, prefix=""):
        """
        Write results as Latex-tables to disk.

        Creates three files:
        * `ols_results.tex`: file containing OLS-summary tables
        * `elasticities.tex`: file containing elasticity-tables
        * `simulation.tex`: file containing simulation-tables

        Parameters:
        * `prefix`: string to prepend filename (defaults to empty string)
        """
        with open(prefix + "ols_results.tex", "w") as f:
            f.write(self.res.summary().as_latex())

        with open(prefix + "elasticities.tex", "w") as f:
            f.write(self.table_elasticities.to_latex())

        with open(prefix + "simulation.tex", "w") as f:
            caption = self.__parse_modifier()
            f.write(caption)
            f.write(self.table_sim.to_latex())

    def __predict_l1_shares(self, data):
        full = deepcopy(data)
        full["exp_delta"] = np.exp(full.delta)
        agg = full.groupby(self.agg).exp_delta.sum().to_frame("sum_exp_delta")
        full = full.merge(agg, left_on=self.agg, right_index=True)
        return full["exp_delta"] / (1 + full["sum_exp_delta"])

    def __predict_l2_shares(self, data):
        full = deepcopy(data)
        grp_level = self.agg + self.nests

        full["exp_delta"] = np.exp(full["delta"] / (1 - self.sigma[0]))
        group = full.groupby(grp_level).exp_delta.sum().to_frame("DG")
        full = full.merge(group, left_on=grp_level, right_index=True)

        full["share_jg"] = full["exp_delta"] / full["DG"]
        full["NOM"] = full["DG"] ** (1 - self.sigma[0])

        agg = full.groupby(self.agg).NOM.sum().to_frame("DENOM")
        full = full.merge(agg, left_on=self.agg, right_index=True)
        full["share_g"] = full["NOM"] / (full["DENOM"] + 1)

        return full["share_g"] * full["share_jg"]

    def __predict_l3_shares(self, data):
        full = deepcopy(data)
        grp_level = self.agg + [self.nests[0]]
        sub_level = self.agg + self.nests

        full["exp_delta"] = np.exp(full["delta"] / (1 - self.sigma[1]))
        sub = full.groupby(sub_level).exp_delta.sum().to_frame("DH")

        full = full.merge(sub, left_on=sub_level, right_index=True)
        full["share_jhg"] = full["exp_delta"] / full["DH"]
        full["NOM"] = full["DH"] ** (1-self.sigma[1]) / (1-self.sigma[0])

        group = full.groupby(grp_level).NOM.sum().to_frame("DENOM")
        full = full.merge(group, left_on=grp_level, right_index=True)
        full["share_hg"] = full["NOM"] / full["DENOM"]
        # Overwrite nominator NOM
        full["NOM"] = full["DENOM"] ** (1-self.sigma[0])
        # Drop old DENOM
        del full["DENOM"]

        agg = full.groupby(self.agg).NOM.sum().to_frame("DENOM")
        full = full.merge(agg, left_on=self.agg, right_index=True)
        full["share_g"] = full["NOM"] / full["DENOM"]

        return full["share_g"] * full["share_hg"] * full["share_jhg"]

    def __get_l1_elasticities(self, data, share, price):
        # Create cartesian product of all products.
        cartesian = itertools.product(data.L5.unique(), data.L5.unique())
        dflist = []

        # For more convenient access to brand specific data.
        pivoted = data.pivot(index=self.agg[-1],
                             columns='L5',
                             values=[share, price])

        for (j, k) in cartesian:
            # Own elasticity.
            if j == k:
                df = -self.alpha * pivoted[price][j]*(1-pivoted[share][j])
            # Cross elasticity.
            else:
                df = self.alpha * pivoted[price][k]*pivoted[share][k]

            df = df.to_frame("elasticity")
            df["j"] = j
            df["k"] = k
            dflist.append(df)

        return pd.concat(dflist)

    def __get_l2_elasticities(self, data, share, price):
        # Create cartesian product of all products.
        cartesian = itertools.product(data.L5.unique(), data.L5.unique())
        dflist = []

        pivoted = data.pivot(index=self.agg[-1],
                             columns='L5',
                             values=[share, price, "G1"])
        s = self.sigma[0]
        s_ = 1 - self.sigma[0]
        a = self.alpha

        for (j, k) in cartesian:
            # Own elasticity.
            if j == k:
                outer = -(1/s_) * a * pivoted[price][j]
                inner_a = 1 - s * pivoted["G1"][j]
                inner_b = s_ * pivoted[share][j]
                df = outer * (inner_a - inner_b)
            elif self.__in_same_group(j, k):
                outer = (1/s_)*a*pivoted[price][k]
                inner_a = s * pivoted["G1"][k]
                inner_b = s_ * pivoted[share][k]
                df = outer * (inner_a + inner_b)
            else:
                df = a * pivoted[share][k] * pivoted[price][k]

            df = df.to_frame("elasticity")

            df["j"] = j
            df["k"] = k
            dflist.append(df)
        return pd.concat(dflist)

    def __get_l3_elasticities(self, data, share, price):
        # Create pivoted DataFrame for easier access.
        pivoted = data.pivot_table(index=self.agg[-1],
                                   columns=[self.nests[0], 'L5'],
                                   values=["share", "price_liter", "G1", "G2"])
        # Create shorthands.
        sg = self.sigma[0]
        sg_ = 1 - self.sigma[0]
        shg_ = 1 - self.sigma[1]
        a = self.alpha
        # Create cartesian product of all products.
        cartesian = itertools.product(data[self.nests[0]].unique(),
                                      data[self.nests[0]].unique(),
                                      data.L5.unique(),
                                      data.L5.unique())
        dflist = []
        for (h, i, j, k) in cartesian:
            # Define shorthands.
            within = pivoted["G2"][i][j]
            subgroup = pivoted["G1"][i][j]
            group = pivoted[share][i][j]
            prices = pivoted[price][i][j]
            outer = a * prices
            # Own elasticity.
            if (h == i) & (j == k):
                inner_a = 1/shg_ - (1/shg_ - 1/sg_) * within
                inner_b = (sg/sg_) * within * subgroup + group
                df = outer * (inner_a - inner_b)
            # Share group and subgroup.
            elif (h == i) & self.__in_same_subgroup(j, k):
                inner_a = (1/sg - 1/sg_) * within
                inner_b = (sg/sg_) * within * subgroup + group
                df = - outer * (inner_a + inner_b)
            # Share group.
            elif (h == i):
                df = - outer * (sg/sg_ * within * subgroup)
            # Different nest.
            else:
                df = - outer * group

            df = df.to_frame("elasticity")
            df["j"] = j
            df["k"] = k
            df["i"] = i
            df["h"] = h
            dflist.append(df)
        return pd.concat(dflist)

    def __get_elasticities(self, data, share, price):
        if not self.nests:
            return self.__get_l1_elasticities(data, share, price)
        elif len(self.nests) == 1:
            return self.__get_l2_elasticities(data, share, price)
        elif len(self.nests) == 2:
            return self.__get_l3_elasticities(data, share, price)
        else:
            raise NotImplementedError

    def __get_grp_share(self, col, level):
        grp_agg = self.data.groupby(level)[col].sum()
        grp_agg = grp_agg.to_frame("grp_sum")
        # Merge into temporary DataFrame.
        tmp = self.data.merge(grp_agg, left_on=level, right_index=True)
        # Return shares.
        return tmp[col] / tmp["grp_sum"]

    def __get_sim_share(self, col, level):
        grp_agg = self.simulated.groupby(level)[col].sum()
        grp_agg = grp_agg.to_frame("grp_sum")
        # Merge into temporary DataFrame.
        tmp = self.simulated.merge(grp_agg, left_on=level, right_index=True)
        # Return shares.
        if col == self.share:
            return tmp[col] / (tmp["grp_sum"] + 1)
        else:
            return tmp[col] / tmp["grp_sum"]

    def __get_og_share(self):
        grp_agg = self.data.groupby(self.agg)[self.share].sum()
        grp_agg = grp_agg.to_frame("share_sum")
        # Merge into temporary DataFrame.
        tmp = self.data.merge(grp_agg, left_on=self.agg, right_index=True)
        # Return shares.
        return 1 - tmp["share_sum"]

    def __in_same_group(self, j, k):
        val_j = self.structure[self.nests[0]][j]
        val_k = self.structure[self.nests[0]][k]

        return val_j == val_k

    def __in_same_subgroup(self, j, k):
        val_j1 = self.structure[self.nests[0]][j]
        val_k1 = self.structure[self.nests[0]][k]

        val_j2 = self.structure[self.nests[1]][j]
        val_k2 = self.structure[self.nests[1]][k]

        return all([val_j1 == val_k1, val_j2 == val_k2])

    def __modify(self, x):
        if x["L5"] in self.modifiers:
            return x["price_liter"] * self.modifiers[x["L5"]]
        else:
            return x["price_liter"]

    def __parse_modifier(self):
        l_mod = len(self.modifiers)
        l_bra = len(self.data.L5.unique())
        v_mod = list(self.modifiers.values())
        same = [v == v_mod[0] for v in v_mod]
        caption = ""
        if (l_mod == l_bra) & all(same):
            if v_mod[0] == 1:
                raise RuntimeError("Values in 'modifiers' do not make sense.")
            elif v_mod[0] >= 1:
                action = "increase"
            else:
                action = "decrease"
            value = int(abs(v_mod[0] - 1) * 100)
            caption = "All brands {} their prices by {} %".format(action,
                                                                  value)
        else:
            for i, (br, pr) in enumerate(self.modifiers.items(), 1):
                if pr > 1:
                    action = "increases"
                elif pr < 1:
                    action = "decreases"
                else:
                    raise RuntimeError("Value in 'modifiers' not supported.")

                if i == l_mod:
                    sep = "."
                elif i == l_mod - 1:
                    sep = " and"
                else:
                    sep = ","

                value = int(abs(pr - 1) * 100)
                caption += "{} {} its price by {} %{} ".format(br,
                                                               action,
                                                               value,
                                                               sep)
            caption = "\\caption{%s}\\n" % caption
        return caption


if __name__ == "__main__":
    datapath = os.path.join("data", "cola_amm_boston_clean.csv")
    data = pd.read_csv(datapath)

    model = "L5+C(year)+price_liter+C(display_all)+C(feature_all)"
    modifiers = {"DIET COKE": 0.25, "COKE CLASSIC": 1.25}

    amm = AMM()
    amm.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
            RHS=model,
            share="share",
            agg=["year", "week"])
    amm.summary()
    amm.simulate(modifiers)

    amm2 = AMM()
    amm2.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
             RHS=model,
             share="share",
             agg=["year", "week"],
             nests=["sugar"])
    amm2.summary()
    amm2.simulate(modifiers)

    amm3 = AMM()
    amm3.fit(data,
             RHS=model,
             share="share",
             agg=["year", "week"],
             nests=["PACKAGE", "sugar"])
    amm3.summary()
    amm3.simulate(modifiers)

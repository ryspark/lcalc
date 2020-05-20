class LambdaTerm:

    def __init__(self, lc):
        if isinstance(lc, LambdaTerm):
            self.lc = lc
        else:
            self.lc = self.check(lc)

    def check(self, lc):
        return lc

    def alpha_equals(self, other_lc):
        raise NotImplementedError()

    def beta_equals(self, other_lc):
        raise NotImplementedError()

    def eta_equals(self, other_lc):
        raise NotImplementedError()

    def __eq__(self, other_lc):
        # return self.alpha_equals(other_lc) or self.beta_equals(other_lc) or self.eta_equals(other_lc)
        return self.lc == LambdaTerm(other_lc).lc

    def __repr__(self):
        return self.lc

    def __str__(self):
        return self.__repr__()

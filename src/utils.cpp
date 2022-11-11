
#include "utils.h"

struct OneMaxValue : public RcppParallel::Worker
{
    // This fails to compile without the "dummy" variable. Seems to be a bug in
    // RcppParallel?
    const RcppParallel::RMatrix <double> m;
    const int dummy;

    RcppParallel::RVector <double> res;

    // constructor
    OneMaxValue (
            const RcppParallel::RMatrix <double> m_in,
            const int dummy_in,
            RcppParallel::RVector <double> res_in) :
        m (m_in), dummy (dummy_in), res (res_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        for (std::size_t i = begin; i < end; i++)
        {
            const RcppParallel::RMatrix <double>::Column col_i = m.column (i);
            std::vector <double> col_i_vec (col_i.size ());
            std::copy (col_i.begin (), col_i.end (), col_i_vec.begin ());

            std::vector <double>::iterator it =
                std::max_element (col_i_vec.begin (), col_i_vec.end (),
                        NaNAwareLess <double> ());

            res [i] = *it;
        }
    }
                                   
};


//' rcpp_matrix_max
//'
//' Parallel version of `max(mat)`.
//' @noRd
// [[Rcpp::export]]
const double rcpp_matrix_max (Rcpp::NumericMatrix mat)
{
    const int dummy = 0;

    const size_t nverts = static_cast <size_t> (mat.ncol ());

    Rcpp::NumericVector res (nverts);

    OneMaxValue one_closest (RcppParallel::RMatrix <double> (mat), dummy,
            RcppParallel::RVector <double> (res));

    RcppParallel::parallelFor (0, nverts, one_closest);

    std::vector <double> res_vec (nverts);
    std::copy (res.begin (), res.end (), res_vec.begin ());
    std::vector <double>::iterator it =
        std::max_element (res_vec.begin (), res_vec.end (),
                NaNAwareLess <double> ());

    return *it;
}

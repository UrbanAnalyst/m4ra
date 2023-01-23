
#include "multi-modal.h"

//' rcpp_add_net_to_gtfs
//'
//' Add times from selected start points to all GTFS stations to total GTFS '
//' travel time matrix to generate fastest travel times to all GTFS end points.
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_add_net_to_gtfs (Rcpp::NumericMatrix net_times,
        Rcpp::NumericMatrix gtfs_times, Rcpp::List gtfs_to_net_index,
        Rcpp::List gtfs_to_net_dist, const int nverts)
{
    const size_t nfrom = static_cast <size_t> (net_times.nrow ());
    const size_t n_gtfs = static_cast <size_t> (net_times.ncol ());

    if (gtfs_times.nrow () != gtfs_times.ncol () &&
            gtfs_times.nrow () != n_gtfs)
    {
        Rcpp::stop ("GTFS and travel time matrices are not compatible");
    }

    Rcpp::NumericMatrix times_to_end_stops (static_cast <int> (nfrom), static_cast <int> (n_gtfs));
    std::fill (times_to_end_stops.begin (), times_to_end_stops.end (), INFINITE_DBL);

    // Add initial times to all closest GTFS stops to the times to all terminal
    // GTFS stops:
    for (size_t i = 0; i < nfrom; i++) {

        Rcpp::checkUserInterrupt ();

        for (size_t j = 0; j < n_gtfs; j++) {

            const double time_i_to_j = net_times (i, j);
            if (time_i_to_j < 0) // NA's have been replaced with -ve
            {
                continue;
            }

            for (size_t k = 0; k < n_gtfs; k++) {

                const double time_j_to_k = gtfs_times (j, k);
                if (time_j_to_k < 0) // NA's have been replaced with -ve
                {
                    continue;
                }
                const double time_i_to_k = std::min (net_times (i, k), time_i_to_j + time_j_to_k);

                if (time_i_to_k < times_to_end_stops (i, k))
                {
                    times_to_end_stops (i, k) = time_i_to_k;
                }
            }
        }
    }

    // Those are then the fastest times to each terminal GTFS stop. Then use the
    // lists of closest stops to each network point to calculate final fastest
    // times to each terminal network point.

    Rcpp::NumericMatrix res (static_cast <int> (nfrom), nverts);
    std::fill (res.begin (), res.end (), INFINITE_DBL);

    for (size_t i = 0; i < nfrom; i++)
    {

        Rcpp::checkUserInterrupt ();

        for (size_t j = 0; j < gtfs_to_net_index.size (); j++)
        {
            if (times_to_end_stops (i, j) == INFINITE_DBL)
            {
                continue;
            }

            Rcpp::IntegerVector index_j = gtfs_to_net_index (j);
            Rcpp::NumericVector d_j = gtfs_to_net_dist (j);

            for (size_t k = 0; k < index_j.size (); k++)
            {
                if (d_j (k) < 0)
                {
                    continue;
                }

                const double time_i_to_k = times_to_end_stops (i, j) + d_j (k);
                const size_t index_k = static_cast <size_t> (index_j [static_cast <R_xlen_t> (k)]);

                if (time_i_to_k < res (i, index_k))
                {
                    res (i, index_k) = time_i_to_k;
                }
            }
        }
    }

    return res;
}

//' rcpp_min_from_two_matrices
//'
//' Iterate over all rows and columns of two identical matrices, and return
//' minimal values from same indices in both.
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_min_from_two_matrices (Rcpp::NumericMatrix mat1,
        Rcpp::NumericMatrix mat2)
{
    if (mat1.ncol () != mat2.ncol () || mat1.nrow () != mat2.nrow ())
    {
        Rcpp::stop ("Matrices must have identical dimensions.");
    }

    Rcpp::NumericMatrix res (mat1.nrow (), mat1.ncol ());

    for (size_t i = 0; i < mat1.nrow (); i++)
    {
        for (size_t j = 0; j < mat1.ncol (); j++)
        {
            res (i, j) = std::min (mat1 (i, j), mat2 (i, j));
        }
    }

    return res;
}


#include "multi-modal.h"

struct OneTimesToEndStops : public RcppParallel::Worker
{
    const RcppParallel::RMatrix <int> net_times;
    const Rcpp::IntegerMatrix gtfs_times;
    const size_t nfrom;
    const size_t n_gtfs;
    // gtfs_times is actually 3 matrices of (times, transfers, intervals)
    // dim (n_gtfs, 3 * n_gtfs)

    RcppParallel::RMatrix <int> tout;

    // constructor
    OneTimesToEndStops (
            const RcppParallel::RMatrix <int> net_times_in,
            const Rcpp::IntegerMatrix gtfs_times_in,
            const size_t nfrom_in,
            const size_t n_gtfs_in,
            RcppParallel::RMatrix <int> tout_in) :
        net_times (net_times_in), gtfs_times (gtfs_times_in),
        nfrom (nfrom_in), n_gtfs (n_gtfs_in), tout (tout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        // i over all vertices in the input distance matrix
        for (std::size_t i = begin; i < end; i++)
        {
            for (size_t j = 0; j < n_gtfs; j++)
            {
                const int time_i_to_j = net_times (i, j);
                if (time_i_to_j < 0) // NA's have been replaced with -ve
                {
                    continue;
                }

                for (size_t k = 0; k < n_gtfs; k++)
                {
                    const int time_j_to_k = gtfs_times (j, k);
                    if (time_j_to_k < 0)
                    {
                        continue;
                    }

                    const int time_i_to_k = std::min (net_times (i, k), time_i_to_j + time_j_to_k);

                    if (time_i_to_k < tout (i, k))
                    {
                        tout (i, k) = time_i_to_k;
                        if (time_i_to_k < net_times (i, j)) // Multi-modal route
                        {
                            // set these to negative to flag multi-modal
                            // routing:
                            tout (i, k + n_gtfs) = -gtfs_times (j, k + n_gtfs); // ntransfers
                            tout (i, k + 2 * n_gtfs) = -gtfs_times (j, k + 2 * n_gtfs); // interval
                        }
                    }
                }
            }
        }
    }
};

/* Both 'times_to_end_stops' and 'tout' are tripe-matrices, with successive
 * thirds stored in columns for:
 * 1. times;
 * 2. transfers;
 * 3. intervals
 */
struct AddTwoMatricesWorker : public RcppParallel::Worker
{
    const RcppParallel::RMatrix <int> times_to_end_stops;
    const std::vector <std::vector <size_t> > gtfs_to_net_index_vec;
    const std::vector <std::vector <double> > gtfs_to_net_dist_vec;
    const size_t nfrom;

    RcppParallel::RMatrix <int> tout;

    // constructor
    AddTwoMatricesWorker (
            const RcppParallel::RMatrix <int> times_to_end_stops_in,
            std::vector <std::vector <size_t> > gtfs_to_net_index_vec_in,
            std::vector <std::vector <double> > gtfs_to_net_dist_vec_in,
            const size_t nfrom_in,
            RcppParallel::RMatrix <int> tout_in) :
        times_to_end_stops (times_to_end_stops_in),
        gtfs_to_net_index_vec (gtfs_to_net_index_vec_in),
        gtfs_to_net_dist_vec (gtfs_to_net_dist_vec_in),
        nfrom (nfrom_in),
        tout (tout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        const size_t n_gtfs = gtfs_to_net_dist_vec.size ();
        const size_t n_verts = static_cast <int> (tout.ncol () / 3);
        // i over all vertices in the input distance matrix
        for (std::size_t i = begin; i < end; i++)
        {
            for (size_t j = 0; j < n_gtfs; j++)
            {
                if (times_to_end_stops (i, j) == INFINITE_INT)
                {
                    continue;
                }

                const size_t n_j = gtfs_to_net_index_vec [j].size ();

                for (size_t k = 0; k < n_j; k++)
                {
                    if (gtfs_to_net_dist_vec [j] [k] < 0)
                    {
                        continue;
                    }

                    const int dist_j_k = static_cast <int> (round (gtfs_to_net_dist_vec [j] [k]));
                    const int time_i_to_k = times_to_end_stops (i, j) + dist_j_k;
                    const size_t index_k = gtfs_to_net_index_vec [j] [k];

                    if (time_i_to_k < tout (i, index_k))
                    {
                        tout (i, index_k) = time_i_to_k;
                        if (times_to_end_stops (i, j + n_gtfs) < 0)
                        {
                            tout (i, n_verts + index_k) = -times_to_end_stops (i, j + n_gtfs); // transfers
                            tout (i, 2 * n_verts + index_k) = -times_to_end_stops (i, j + 2 * n_gtfs); // interval
                        }
                    }
                }
            }
        }
    }
};

//' rcpp_add_net_to_gtfs
//'
//' Add times from selected start points to all GTFS stations to total GTFS '
//' travel time matrix to generate fastest travel times to all GTFS end points.
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_add_net_to_gtfs (Rcpp::IntegerMatrix net_times,
        Rcpp::IntegerMatrix gtfs_times, Rcpp::List gtfs_to_net_index,
        Rcpp::List gtfs_to_net_dist, const int nverts)
{
    const size_t nfrom = static_cast <size_t> (net_times.nrow ());
    const size_t n_gtfs = static_cast <size_t> (net_times.ncol ());

    if (gtfs_times.nrow () != gtfs_times.ncol () &&
            gtfs_times.nrow () != n_gtfs)
    {
        Rcpp::stop ("GTFS and travel time matrices are not compatible");
    }

    // times_to_end_stops holds 3 matrices of:
    // 1. Actual times;
    // 2. Numbers of transfers;
    // 3. Effective intervals to next service.
    Rcpp::IntegerMatrix times_to_end_stops (static_cast <int> (nfrom), static_cast <int> (3 * n_gtfs));
    std::fill (times_to_end_stops.begin (), times_to_end_stops.end (), INFINITE_INT);

    // Add initial times to all closest GTFS stops to the times to all terminal
    // GTFS stops:
    OneTimesToEndStops one_times (RcppParallel::RMatrix <int> (net_times),
            gtfs_times, nfrom, n_gtfs,
            RcppParallel::RMatrix <int> (times_to_end_stops));

    RcppParallel::parallelFor (0, nfrom, one_times);

    // Those are then the fastest times to each terminal GTFS stop. Then use the
    // lists of closest stops to each network point to calculate final fastest
    // times to each terminal network point.

    Rcpp::IntegerMatrix res (static_cast <int> (nfrom), 3 * nverts);
    std::fill (res.begin (), res.end (), INFINITE_INT);

    // convert the Rcpp::Lists into std::vecs:
    std::vector <std::vector <size_t> > gtfs_to_net_index_vec (gtfs_to_net_index.size ());
    std::vector <std::vector <double> > gtfs_to_net_dist_vec (gtfs_to_net_dist.size ());

    for (size_t i = 0; i < gtfs_to_net_index.size (); i++)
    {
        Rcpp::IntegerVector index_i = gtfs_to_net_index (i);
        gtfs_to_net_index_vec [i] = Rcpp::as <std::vector <size_t> > (index_i);

        Rcpp::NumericVector d_i = gtfs_to_net_dist (i);
        gtfs_to_net_dist_vec [i] = Rcpp::as <std::vector <double> > (d_i);
    }

    AddTwoMatricesWorker combine_two_mats (
            RcppParallel::RMatrix <int> (times_to_end_stops),
            gtfs_to_net_index_vec, gtfs_to_net_dist_vec, nfrom,
            RcppParallel::RMatrix <int> (res));

    RcppParallel::parallelFor (0, nfrom, combine_two_mats);

    return res;
}

//' rcpp_min_from_two_matrices
//'
//' Iterate over all rows and columns of two identical matrices, and return
//' minimal values from same indices in both.
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_min_from_two_matrices (Rcpp::IntegerMatrix mat1,
        Rcpp::IntegerMatrix mat2)
{
    if (mat1.ncol () != mat2.ncol () || mat1.nrow () != mat2.nrow ())
    {
        Rcpp::stop ("Matrices must have identical dimensions.");
    }

    Rcpp::IntegerMatrix res (mat1.nrow (), mat1.ncol ());

    for (size_t i = 0; i < mat1.nrow (); i++)
    {
        for (size_t j = 0; j < mat1.ncol (); j++)
        {
            res (i, j) = std::min (mat1 (i, j), mat2 (i, j));
        }
    }

    return res;
}

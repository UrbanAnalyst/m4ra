
#include "travel-times.h"
#include <unordered_map>

const bool is_na (const int &x)
{
    return (x >= INFINITE_INT) || (x <= -INFINITE_INT);
}

struct OneMinDists : public RcppParallel::Worker
{
    const RcppParallel::RMatrix <double> p_dists;
    const int n_closest;
    const double maxd;

    RcppParallel::RMatrix <double> dout;

    // constructor
    OneMinDists (
            const RcppParallel::RMatrix <double> dists_in,
            const int n_closest_in,
            const double maxd_in,
            RcppParallel::RMatrix <double> dout_in) :
        p_dists (dists_in), n_closest (n_closest_in), maxd (maxd_in), dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        for (std::size_t i = begin; i < end; i++)
        {
            const RcppParallel::RMatrix <double>::Column col_i = p_dists.column (i);
            std::vector <double> col_i_vec (col_i.size ());
            std::copy (col_i.begin (), col_i.end (), col_i_vec.begin ());

            std::vector <double>::iterator it = std::min_element (col_i_vec.begin (), col_i_vec.end ());
            const bool allna = (*it >= (maxd - 1.0));

            if (allna)
                continue;

            for (int j = 0; j < n_closest; j++)
            {
                dout (j, i) = *it;
                dout (n_closest + j, i) = std::distance (col_i_vec.begin (), it);
                *it = maxd;
                it = std::min_element (col_i_vec.begin (), col_i_vec.end ());
            }
        }
    }
                                   
};


//' rcpp_closest_gtfs
//'
//' Get the closest GTFS stops to a given point, based on simple metric
//' distance, not network distances.
//' @noRd
// [[Rcpp::export]]
Rcpp::List rcpp_closest_gtfs (Rcpp::DataFrame vxy,
        Rcpp::DataFrame stops, const int n_closest)
{
    const size_t nxy = vxy.nrow ();
    const size_t nstns = stops.nrow ();

    Rcpp::NumericVector vxy_x = vxy ["x"], vxy_y = vxy ["y"];
    Rcpp::NumericVector stop_x = stops ["stop_lon"], stop_y = stops ["stop_lat"];

    Rcpp::List res (nxy);

    for (size_t i = 0; i < nxy; i++)
    {
        // Find distances to all GTFS stations:
        std::vector <double> dist_vec (nstns), dist_vec_s (nstns);
        for (size_t j = 0; j < nstns; j++)
        {
            double dx = vxy_x (i) - stop_x (j),
                   dy = vxy_y (i) - stop_y (j);
            double dij = dx * dx + dy * dy;
            dist_vec [j] = round (dij * XY_PRECISION) / XY_PRECISION;
            dist_vec_s [j] = dist_vec [j];
        }

        // Then find the threshold distance of the "n_closest" stations:
        std::sort (dist_vec_s.begin (), dist_vec_s.end ());
        double dist_max = dist_vec_s [n_closest - 1];

        int len = 0;
        for (auto d: dist_vec_s)
        {
            if (d <= dist_max)
            {
                len++;
            }
        }

        // And then return all stations <= that threshold distance:
        Rcpp::IntegerVector dout (len);
        len = 0;
        for (size_t j = 0; j < nstns; j++)
        {
            if (dist_vec [j] <= dist_max)
            {
                dout (len++) = j;
            }
        }

        res (i) = dout;
    } // end for i

    return res;
}

//' rcpp_closest_pts
//'
//' From a matrix of distances from a defined set of point to all points in a
//' network, identify the 'n_closest' 'from' points for each network point.
//' This can't be done efficiently within an actual Dijkstra query, because the
//' number of 'from' points is generally << number of network points. This
//' routine post-processes the distance matrix through reducing the
//' dimensionality of it.
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_closest_pts (Rcpp::NumericMatrix dmat,
        const int n_closest, const double maxd)
{

    const int nverts = dmat.ncol ();

    Rcpp::NumericVector na_vec = Rcpp::NumericVector (n_closest * 2 * nverts,
            Rcpp::NumericVector::get_na ());
    Rcpp::NumericMatrix res (n_closest * 2, nverts, na_vec.begin ());

    OneMinDists one_closest (RcppParallel::RMatrix <double> (dmat), n_closest,
            maxd, RcppParallel::RMatrix <double> (res));

    RcppParallel::parallelFor (0, nverts, one_closest);

    return res;
}

//' rcpp_net_gtfs_travel_times
//'
//' This takes three matrices as inputs:
//' `net_to_gtfs` quantifying travel times from a selection of points to every
//' GTFS stop;
//' `gtfs_to_gtfs' holding the full GTFS travel time matrix; and
//' `gtfs_to_net` holding times back out from all GTFS stops to every network
//' point.
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_net_gtfs_travel_times (Rcpp::IntegerMatrix t_net_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_net,
        Rcpp::List closest_gtfs_to_from,
        Rcpp::IntegerMatrix closest_gtfs_to_net)
{

    const int n_from = t_net_to_gtfs.nrow ();
    const int n_gtfs = t_net_to_gtfs.ncol ();
    if ((n_gtfs != t_gtfs_to_gtfs.nrow () || n_gtfs != t_gtfs_to_net.nrow ()))
    {
        Rcpp::stop ("network and gtfs matrices have incompatible dimensions.");
    }
    if (n_from != closest_gtfs_to_from.size ())
    {
        Rcpp::stop ("network and gtfs closest stations have incompatible dimensions.");
    }
    const int n_verts = t_gtfs_to_net.ncol ();

    Rcpp::IntegerMatrix result (n_from, n_verts);
    std::fill (result.begin (), result.end (), INFINITE_INT);

    // Construct times to all terminal GTFS stops as 
    for (size_t i = 0; i < n_from; i++)
    {
        const Rcpp::IntegerVector closest_gtfs_to_from = closest_gtfs_to_from (i);
        const size_t n_closest = closest_gtfs_to_from.size ();
        if (is_na (closest_gtfs_to_from [0]))
        {
            continue;
        }

        // Minimal travel times through the n_closest GTFS stops to all other
        // GTFS stops:
        std::vector <int> times_to_gtfs_stops (n_gtfs);
        std::fill (times_to_gtfs_stops.begin (), times_to_gtfs_stops.end (), INFINITE_INT);

        for (size_t j = 0; j < n_closest; j++)
        {
            if (is_na (t_net_to_gtfs (i, closest_gtfs_to_from [j])))
            {
                continue;
            }

            for (size_t k = 0; k < n_gtfs; k++)
            {
                if (is_na (t_gtfs_to_gtfs (closest_gtfs_to_from [j], k)))
                {
                    continue;
                }

                const int t_to_gtfs_k = t_net_to_gtfs (i, closest_gtfs_to_from [j]) + t_gtfs_to_gtfs (closest_gtfs_to_from [j], k);
                if (t_to_gtfs_k < times_to_gtfs_stops [k])
                {
                    times_to_gtfs_stops [k] = t_to_gtfs_k;
                }
            }
        }

        // Then minimal times from all terminal GTFS stops to all other network
        // points, tracing only times from GTFS stops given in
        // `closest_gtfs_to_net`.
        const size_t n_closest_gtfs = closest_gtfs_to_net.nrow ();
        for (size_t j = 0; j < n_gtfs; j++)
        {
            if (is_na (times_to_gtfs_stops [j]))
            {
                continue;
            }

            for (size_t k = 0; k < n_verts; k++)
            {
                if (is_na (t_gtfs_to_net (j, k)))
                {
                    continue;
                }

                const int t_to_net = times_to_gtfs_stops [j] + t_gtfs_to_net (j, k);
                if (t_to_net < result (i, k))
                {
                    result (i, k) = t_to_net;
                }
            }
        }
        Rcpp::checkUserInterrupt ();
    }

    for (int i = 0; i < n_from; i++)
    {
        for (size_t j = 0; j < n_verts; j++)
        {
            if (result (i, j) == INFINITE_INT)
            {
                result (i, j) = NA_INTEGER;
            }
        }
    }

    return result;
}

std::vector <int> get_closest_gtfs_stns (Rcpp::IntegerMatrix &times_to_gtfs_stops,
        const int &i, const int &n_closest)
{

    const int n_gtfs = times_to_gtfs_stops.ncol ();

    std::vector <int> gtfs_stn_times (n_gtfs, INFINITE_INT);
    int min_time = INFINITE_INT;
    for (size_t j = 0; j < n_gtfs; j++)
    {
        gtfs_stn_times [j] = times_to_gtfs_stops (i, j);
        if (gtfs_stn_times [j] < min_time)
        {
            min_time = gtfs_stn_times [j];
        }
    }

    if (is_na (min_time))
    {
        return gtfs_stn_times;
    }

    std::sort (gtfs_stn_times.begin (), gtfs_stn_times.end ());
    // defaults to ascending sort

    int max_time = 0L;
    for (size_t j = 0; j < n_closest; j++)
    {
        if (gtfs_stn_times [j] > max_time)
        {
            max_time = gtfs_stn_times [j];
        }
    }

    size_t len = 0;
    for (size_t j = 0; j < n_gtfs; j++)
    {
        if (times_to_gtfs_stops (i, j) <= max_time)
        {
            len++;
        }
    }

    std::vector <int> closest_gtfs (len);
    len = 0;
    for (size_t j = 0; j < n_gtfs; j++)
    {
        if (times_to_gtfs_stops (i, j) <= max_time)
        {
            closest_gtfs [len++] = j;
        }
        if (len == times_to_gtfs_stops.size ())
        {
            break;
        }
    }

    return closest_gtfs;
}

//' The 'closest' lists returned by 'rcpp_closest_pts' or 'rcpp_closest_gtfs'
//' hold indices for each network point to the nearest GTFS stops, and
//' corresponding distances/times. This function converts those to a list of
//' elements, one for each GTFS stop, holding indices and distances out to the
//' corresponding network points for which that stop is one of the closest.
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::List rcpp_expand_closest_index (Rcpp::NumericMatrix closest)
{
    const int n_closest = closest.nrow () / 2;
    const int n_verts = closest.ncol ();
    int n_gtfs = 0;
    for (int i = 0; i < n_closest; i++)
    {
        for (int j = 0; j < n_verts; j++)
        {
            int cl_ij = static_cast <int> (closest (n_closest + i, j));
            if (cl_ij > n_gtfs)
            {
                n_gtfs = cl_ij;
            }
        }
    }
    n_gtfs++;

    Rcpp::List res (2 * n_gtfs);
    for (int i = 0; i < n_gtfs; i++)
    {
        res (i) = std::vector <int> (0);
        res (n_gtfs + i) = std::vector <double> (0);
    }

    for (int i = 0; i < n_verts; i++)
    {
        for (int j = 0; j < n_closest; j++)
        {
            int gtfs_index = static_cast <int> (closest (n_closest + j, i));
            double d = closest (j, i);
            if (gtfs_index < 0 || d < 0.0)
            {
                continue;
            }

            std::vector <int> index_ij = res (gtfs_index);
            index_ij.push_back (i);
            res (gtfs_index) = index_ij;

            std::vector <double> d_ij = res (n_gtfs + gtfs_index);
            d_ij.push_back (d);
            res (gtfs_index + n_gtfs) = d_ij;
        }
    }

    return res;
}
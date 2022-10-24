
#include "travel-times.h"

//' rcpp_closest_gtfs
//'
//' Get the closest GTFS stops to a given point
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
        Rcpp::List  closest_gtfs_stns)
{

    const int n_from = t_net_to_gtfs.nrow ();
    const int n_gtfs = t_net_to_gtfs.ncol ();
    if ((n_gtfs != t_gtfs_to_gtfs.nrow () || n_gtfs != t_gtfs_to_net.nrow ()))
    {
        Rcpp::stop ("network and gtfs matrices have incompatible dimensions.");
    }
    if (n_from != closest_gtfs_stns.size ())
    {
        Rcpp::stop ("network and gtfs closest stations have incompatible dimensions.");
    }
    const int n_verts = t_gtfs_to_net.ncol ();

    Rcpp::IntegerMatrix result (n_from, n_verts);
    std::fill (result.begin (), result.end (), INFINITE_INT);

    // Construct times to all terminal GTFS stops as 
    for (size_t i = 0; i < n_from; i++)
    {
        // Minimal travel times to all GTFS stops:
        Rcpp::IntegerMatrix times_to_gtfs_stops (n_from, n_gtfs);
        std::fill (times_to_gtfs_stops.begin (), times_to_gtfs_stops.end (), INFINITE_INT);

        for (size_t j = 0; j < n_gtfs; j++)
        {
            if (t_net_to_gtfs (i, j) == NA_INTEGER)
            {
                continue;
            }

            for (size_t k = 0; k < n_gtfs; k++)
            {
                if (t_gtfs_to_gtfs (j, k) == NA_INTEGER)
                {
                    continue;
                }

                const int t_to_gtfs_k = t_net_to_gtfs (i, j) + t_gtfs_to_gtfs (j, k);
                if (t_to_gtfs_k < times_to_gtfs_stops (i, k))
                {
                    times_to_gtfs_stops (i, k) = t_to_gtfs_k;
                }
            }
        }

        Rcpp::IntegerVector closest_gtfs = closest_gtfs_stns (i);
        const size_t n_closest = closest_gtfs.size ();

        if (closest_gtfs [0] == INFINITE_INT)
        {
            continue;
        }

        // Then minimal times from those 'n_closest' stops to all other network points:
        for (size_t j = 0; j < n_closest; j++)
        {
            if (times_to_gtfs_stops (i, closest_gtfs [j]) == INFINITE_INT)
            {
                continue;
            }

            for (size_t k = 0; k < n_verts; k++)
            {
                if (t_gtfs_to_net (closest_gtfs [j], k) == NA_INTEGER)
                {
                    continue;
                }

                const int t_to_net = times_to_gtfs_stops (i, closest_gtfs [j]) +
                    t_gtfs_to_net (closest_gtfs [j], k);
                if (t_to_net < result (i, k))
                {
                    result (i, k) = t_to_net;
                }
            }
        }
        Rcpp::checkUserInterrupt ();
    }

    Rcpp::checkUserInterrupt ();
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

    if (min_time == INFINITE_INT)
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
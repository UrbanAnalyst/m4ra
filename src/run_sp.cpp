
#include "run_sp.h"

#include "dgraph.h"
#include "bheap.h"

// # nocov start
template <typename T>
void inst_graph (std::shared_ptr<DGraph> g, size_t nedges,
        const std::map <std::string, size_t>& vert_map,
        const std::vector <std::string>& from,
        const std::vector <std::string>& to,
        const std::vector <T>& dist,
        const std::vector <T>& wt)
{
    for (size_t i = 0; i < nedges; ++i)
    {
        size_t fromi = vert_map.at(from [i]);
        size_t toi = vert_map.at(to [i]);
        g->addNewEdge (fromi, toi, dist [i], wt [i], i);
    }
}
// # nocov end

// RcppParallel jobs can be chunked to a specified "grain size"; see
// https://rcppcore.github.io/RcppParallel/#grain_size
// This function determines chunk size such that there are at least 100 chunks
// for a given `nfrom`.
size_t run_sp::get_chunk_size (const size_t nfrom)
{
    size_t chunk_size;

    if (nfrom > 1000)
        chunk_size = 100;
    else if (nfrom > 100)
        chunk_size = 10;
    else
        chunk_size = 1;

    return chunk_size;
}


std::shared_ptr <HeapDesc> run_sp::getHeapImpl(const std::string& heap_type)
{
    return std::make_shared <HeapD<BHeap> >();
}


struct OneDist : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const std::vector <size_t> toi;
    const size_t nverts;
    const std::vector <double> vx;
    const std::vector <double> vy;
    const std::shared_ptr <DGraph> g;

    RcppParallel::RMatrix <double> dout;

    // constructor
    OneDist (
            const RcppParallel::RVector <int> fromi,
            const std::vector <size_t> toi_in,
            const size_t nverts_in,
            const std::vector <double> vx_in,
            const std::vector <double> vy_in,
            const std::shared_ptr <DGraph> g_in,
            RcppParallel::RMatrix <double> dout_in) :
        dp_fromi (fromi), toi (toi_in), nverts (nverts_in),
        vx (vx_in), vy (vy_in), g (g_in), 
        dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        const std::string heap_type = "BHeap";
        std::shared_ptr<PF::PathFinder> pathfinder =
            std::make_shared <PF::PathFinder> (nverts,
                    *run_sp::getHeapImpl (heap_type), g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <long int> prev (nverts);

        std::vector <double> heuristic (nverts, 0.0);

        for (std::size_t i = begin; i < end; i++)
        {
            size_t from_i = static_cast <size_t> (dp_fromi [i]);

            for (size_t j = 0; j < nverts; j++)
            {
                const double dx = vx [j] - vx [from_i],
                dy = vy [j] - vy [from_i];
                heuristic [j] = sqrt (dx * dx + dy * dy);
            }
            pathfinder->AStar (d, w, prev, heuristic, from_i, toi);

            for (size_t j = 0; j < toi.size (); j++)
            {
                if (w [toi [j]] < INFINITE_DOUBLE)
                {
                    dout (i, j) = d [toi [j]];
                }
            }
        }
    }
                                   
};

struct OneWeightedDist : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const std::vector <size_t> toi;
    const std::vector <double> wti;
    const size_t nverts;
    const double k;
    const double dlim;
    const std::vector <double> vx;
    const std::vector <double> vy;
    const std::shared_ptr <DGraph> g;

    RcppParallel::RMatrix <double> dout;

    // constructor
    OneWeightedDist (
            const RcppParallel::RVector <int> fromi,
            const std::vector <size_t> toi_in,
            const std::vector <double> wti_in,
            const size_t nverts_in,
            const double k_in,
            const double dlim_in,
            const std::vector <double> vx_in,
            const std::vector <double> vy_in,
            const std::shared_ptr <DGraph> g_in,
            RcppParallel::RMatrix <double> dout_in) :
        dp_fromi (fromi), toi (toi_in), wti (wti_in),
        nverts (nverts_in), k (k_in), dlim (dlim_in),
        vx (vx_in), vy (vy_in), g (g_in), 
        dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        const std::string heap_type = "BHeap";
        std::shared_ptr<PF::PathFinder> pathfinder =
            std::make_shared <PF::PathFinder> (nverts,
                    *run_sp::getHeapImpl (heap_type), g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <long int> prev (nverts);

        std::vector <double> heuristic (nverts, 0.0);

        for (std::size_t i = begin; i < end; i++)
        {
            size_t from_i = static_cast <size_t> (dp_fromi [i]);

            for (size_t j = 0; j < nverts; j++)
            {
                const double dx = vx [j] - vx [from_i],
                dy = vy [j] - vy [from_i];
                heuristic [j] = sqrt (dx * dx + dy * dy);
            }
           pathfinder->DijkstraLimit (d, w, prev, from_i, dlim);

            for (size_t j = 0; j < toi.size (); j++)
            {
                if (d [toi [j]] < INFINITE_DOUBLE)
                {
                    dout (i, 0) += exp (-d [toi [j]] / k);
                    dout (i, 1) += dout (i, 0) * wti [j];
                }
            }
        }
    }
                                   
};

struct OneDistNTargets : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const std::vector <size_t> toi;
    const size_t nverts;
    const size_t n_targets;
    const std::vector <double> vx;
    const std::vector <double> vy;
    const std::shared_ptr <DGraph> g;

    RcppParallel::RMatrix <double> dout;

    // constructor
    OneDistNTargets (
            const RcppParallel::RVector <int> fromi,
            const std::vector <size_t> toi_in,
            const size_t nverts_in,
            const size_t n_targets_in,
            const std::vector <double> vx_in,
            const std::vector <double> vy_in,
            const std::shared_ptr <DGraph> g_in,
            RcppParallel::RMatrix <double> dout_in) :
        dp_fromi (fromi), toi (toi_in),
        nverts (nverts_in), n_targets (n_targets_in),
        vx (vx_in), vy (vy_in), g (g_in), 
        dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        const std::string heap_type = "BHeap";
        std::shared_ptr<PF::PathFinder> pathfinder =
            std::make_shared <PF::PathFinder> (nverts,
                    *run_sp::getHeapImpl (heap_type), g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <long int> prev (nverts);

        std::vector <double> heuristic (nverts, 0.0);

        for (std::size_t i = begin; i < end; i++)
        {
            std::fill (d.begin (), d.end (), INFINITE_DOUBLE);
            std::fill (w.begin (), w.end (), INFINITE_DOUBLE);
            std::fill (heuristic.begin (), heuristic.end (), 0.0);
            std::fill (prev.begin (), prev.end (), INFINITE_INT);

            size_t from_i = static_cast <size_t> (dp_fromi [i]);

            for (size_t j = 0; j < nverts; j++)
            {
                const double dx = vx [j] - vx [from_i],
                dy = vy [j] - vy [from_i];
                heuristic [j] = sqrt (dx * dx + dy * dy);
            }
            pathfinder->DijkstraNTargets (d, w, prev, from_i, toi, n_targets);

            // The "NTargets" scanner only accumulates distances up to the
            // closest "n_targets" values, but still includes the full "toi"
            // values, most of which are NA. The loop to find the closest ones
            // therefore has to examine the whole "toi" vector and use an index.
            size_t count = 0;
            for (size_t j = 0; j < toi.size (); j++)
            {
                if (d [toi [j]] < INFINITE_DOUBLE)
                {
                    dout (i, count) = d [toi [j]];
                    dout (i, n_targets + count) = static_cast <double> (toi [j]);
                    count++;
                }
                if (count >= n_targets)
                {
                    break;
                }
            }
        }
    }
                                   
};


struct SaveOneDist : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const std::vector <std::string> from_names;
    const std::vector <size_t> toi;
    const size_t nverts;
    const std::vector <double> vx;
    const std::vector <double> vy;
    const std::string path;
    const std::shared_ptr <DGraph> g;

    // constructor
    SaveOneDist (
            const RcppParallel::RVector <int> fromi,
            const std::vector <std::string> from_names_in,
            const std::vector <size_t> toi_in,
            const size_t nverts_in,
            const std::vector <double> vx_in,
            const std::vector <double> vy_in,
            const std::string path_in,
            const std::shared_ptr <DGraph> g_in) :
        dp_fromi (fromi), from_names (from_names_in), toi (toi_in),
        nverts (nverts_in), vx (vx_in), vy (vy_in), 
        path (path_in), g (g_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        const std::string heap_type = "BHeap";
        std::shared_ptr<PF::PathFinder> pathfinder =
            std::make_shared <PF::PathFinder> (nverts,
                    *run_sp::getHeapImpl (heap_type), g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <long int> prev (nverts);

        std::vector <double> heuristic (nverts, 0.0);

        for (std::size_t i = begin; i < end; i++)
        {
            size_t from_i = static_cast <size_t> (dp_fromi [i]);

            for (size_t j = 0; j < nverts; j++)
            {
                const double dx = vx [j] - vx [from_i],
                dy = vy [j] - vy [from_i];
                heuristic [j] = sqrt (dx * dx + dy * dy);
            }
            pathfinder->AStar (d, w, prev, heuristic, from_i, toi);

            const std::string fname = path + "m4ra_from_" + from_names [i];
            std::ofstream out_file;
            out_file.open (fname.c_str (), std::ofstream::out);

            for (size_t j = 0; j < toi.size (); j++)
            {
                double dtemp = -1.0;
                if (w [toi [j]] < INFINITE_DOUBLE)
                {
                    dtemp = d [toi [j]];
                }
                out_file << std::setprecision (10) << dtemp << std::endl;
            }
            out_file.close ();
        }
    }
                                   
};


size_t run_sp::make_vert_map (const Rcpp::DataFrame &vert_map_in,
        const std::vector <std::string> &vert_map_id,
        const std::vector <size_t> &vert_map_n,
        std::map <std::string, size_t> &vert_map)
{
    for (size_t i = 0;
            i < static_cast <size_t> (vert_map_in.nrow ()); ++i)
    {
        vert_map.emplace (vert_map_id [i], vert_map_n [i]);
    }
    size_t nverts = static_cast <size_t> (vert_map.size ());
    return (nverts);
}

//' rcpp_weighted_dists
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_weighted_dists (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        Rcpp::IntegerVector toi_in,
        Rcpp::NumericVector weights,
        const double dlim,
        const double k)
{
    std::vector <size_t> toi =
        Rcpp::as <std::vector <size_t> > ( toi_in);

    size_t nfrom = static_cast <size_t> (fromi.size ());

    const std::vector <std::string> from = graph [".vx0"];
    const std::vector <std::string> to = graph [".vx1"];
    const std::vector <double> dist = graph ["d"];
    const std::vector <double> wt = graph ["d_weighted"];

    const size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    const size_t nverts = run_sp::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::vector <double> vx (nverts), vy (nverts), wts;
    vx = Rcpp::as <std::vector <double> > (vert_map_in ["x"]);
    vy = Rcpp::as <std::vector <double> > (vert_map_in ["y"]);
    wts = Rcpp::as <std::vector <double> > (weights);

    std::shared_ptr <DGraph> g = std::make_shared <DGraph> (nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    Rcpp::NumericMatrix dout (static_cast <int> (nfrom), 2);

    // Create parallel worker
    OneWeightedDist one_dist (RcppParallel::RVector <int> (fromi), toi,
            wts, nverts, k, dlim, vx, vy, g,
            RcppParallel::RMatrix <double> (dout));

    size_t chunk_size = run_sp::get_chunk_size (nfrom);
    RcppParallel::parallelFor (0, nfrom, one_dist, chunk_size);
    
    return (dout);
}

//' rcpp_get_sp_dists_par
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_get_sp_dists_par (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        Rcpp::IntegerVector toi_in)
{
    std::vector <size_t> toi =
        Rcpp::as <std::vector <size_t> > ( toi_in);

    size_t nfrom = static_cast <size_t> (fromi.size ());
    size_t nto = static_cast <size_t> (toi.size ());

    const std::vector <std::string> from = graph [".vx0"];
    const std::vector <std::string> to = graph [".vx1"];
    const std::vector <double> dist = graph ["d"];
    const std::vector <double> wt = graph ["d_weighted"];

    const size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    const size_t nverts = run_sp::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::vector <double> vx (nverts), vy (nverts);
    vx = Rcpp::as <std::vector <double> > (vert_map_in ["x"]);
    vy = Rcpp::as <std::vector <double> > (vert_map_in ["y"]);

    std::shared_ptr <DGraph> g = std::make_shared <DGraph> (nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    Rcpp::NumericVector na_vec = Rcpp::NumericVector (nfrom * nto,
            Rcpp::NumericVector::get_na ());
    Rcpp::NumericMatrix dout (static_cast <int> (nfrom),
            static_cast <int> (nto), na_vec.begin ());

    // Create parallel worker
    OneDist one_dist (RcppParallel::RVector <int> (fromi), toi,
            nverts, vx, vy, g,
            RcppParallel::RMatrix <double> (dout));

    size_t chunk_size = run_sp::get_chunk_size (nfrom);
    RcppParallel::parallelFor (0, nfrom, one_dist, chunk_size);
    
    return (dout);
}

//' rcpp_dists_to_n_targets
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_dists_to_n_targets (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        Rcpp::IntegerVector toi_in,
        const int n_targets)
{
    std::vector <size_t> toi =
        Rcpp::as <std::vector <size_t> > ( toi_in);

    size_t nfrom = static_cast <size_t> (fromi.size ());

    const std::vector <std::string> from = graph [".vx0"];
    const std::vector <std::string> to = graph [".vx1"];
    const std::vector <double> dist = graph ["d"];
    const std::vector <double> wt = graph ["d_weighted"];

    const size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    const size_t nverts = run_sp::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::vector <double> vx (nverts), vy (nverts);
    vx = Rcpp::as <std::vector <double> > (vert_map_in ["x"]);
    vy = Rcpp::as <std::vector <double> > (vert_map_in ["y"]);

    std::shared_ptr <DGraph> g = std::make_shared <DGraph> (nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    Rcpp::NumericVector na_vec = Rcpp::NumericVector (nfrom * 2 * static_cast <size_t> (n_targets),
            Rcpp::NumericVector::get_na ());
    Rcpp::NumericMatrix dout (static_cast <int> (nfrom),
            static_cast <int> (2 * n_targets), na_vec.begin ());

    // Create parallel worker
    OneDistNTargets one_dist_n_targets (RcppParallel::RVector <int> (fromi), toi,
            nverts, static_cast <size_t> (n_targets),
            vx, vy, g,
            RcppParallel::RMatrix <double> (dout));

    const size_t chunk_size = run_sp::get_chunk_size (nfrom);

    RcppParallel::parallelFor (0, nfrom, one_dist_n_targets, chunk_size);
    
    return (dout);
}

//' rcpp_save_sp_dists_par
//'
//' This function doesn't return anything useful. The R function which calls it
//' ultimately returns the paths to all files created here, but that is not
//' really possible with Rcpp, because of difficulties getting CharacterVectors
//' to play nicely with RcppParallel::RVector. The file name matching is easily
//' done on the R side anyway.
//'
//' @noRd
// [[Rcpp::export]]
bool rcpp_save_sp_dists_par (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector from_index,
        Rcpp::CharacterVector from_names_in,
        Rcpp::IntegerVector toi_in,
        const std::string path)
{
    std::vector <size_t> toi =
        Rcpp::as <std::vector <size_t> > ( toi_in);
    std::vector <std::string> from_names =
        Rcpp::as <std::vector <std::string> > (from_names_in);

    size_t nfrom = static_cast <size_t> (from_index.size ());

    const std::vector <std::string> from = graph [".vx0"];
    const std::vector <std::string> to = graph [".vx1"];
    const std::vector <double> dist = graph ["d"];
    const std::vector <double> wt = graph ["d_weighted"];

    const size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    const size_t nverts = run_sp::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::vector <double> vx (nverts), vy (nverts);
    vx = Rcpp::as <std::vector <double> > (vert_map_in ["x"]);
    vy = Rcpp::as <std::vector <double> > (vert_map_in ["y"]);

    std::shared_ptr <DGraph> g = std::make_shared <DGraph> (nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    // Create parallel worker
    SaveOneDist one_dist (RcppParallel::RVector <int> (from_index), from_names,
            toi, nverts, vx, vy, path, g);

    size_t chunk_size = run_sp::get_chunk_size (nfrom);
    RcppParallel::parallelFor (0, nfrom, one_dist, chunk_size);

    return (true);
}

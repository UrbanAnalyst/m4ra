
#include "scan-times.h"

//' rcpp_scan_times
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_scan_time_files (const Rcpp::DataFrame tt,
        Rcpp::NumericVector &times,
        const std::string &path,
        const double t0)
{
    const std::vector <std::string> osm_id = tt ["osm_id"];
    const std::vector <int> duration = tt ["duration"];

    const size_t n = static_cast <size_t> (tt.nrow ());

    for (size_t i = 0; i < n; i++)
    {
        const std::string f_base = "m4ra_from_" + osm_id [i];
        const std::string f_name = path + f_base;

        std::ifstream in_file;
        in_file.open (f_name.c_str (), std::ifstream::in);
        if (in_file.fail ())
        {
            Rcpp::Rcout << "File: [" << f_name << "]" << std::endl;
            Rcpp::stop ("Scanning time files failed."); // # nocov
        }
        std::string linetxt;

        in_file.clear ();
        in_file.seekg (0);

        const double t_i_0 = t0 + duration [i];
        R_xlen_t index = 0;
        while (getline (in_file, linetxt, '\n'))
        {
            const double t_i = t_i_0 + std::atof (linetxt.c_str ());
            if (t_i < times [index])
            {
                times [index] = t_i;
            }
            index++;
        }
        in_file.close ();
    }
}

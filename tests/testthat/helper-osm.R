# Helper functions to create small OSM files for tests, which can be read
# without any network access.

# Write a small OSM XML file, which `osmdata::osmdata_sf (doc = )` reads
# without any network access or `sf` functions.
write_test_osm <- function (nodes = NULL, ways = NULL) {

    f <- tempfile (fileext = ".osm")
    xml <- c (
        "<?xml version='1.0' encoding='UTF-8'?>",
        "<osm version='0.6' generator='test'>",
        nodes,
        ways,
        "</osm>"
    )
    writeLines (xml, f)
    return (f)
}

osm_node <- function (id, lat, lon, tags = NULL) {

    if (is.null (tags)) {
        return (sprintf ("<node id='%d' lat='%f' lon='%f'/>", id, lat, lon))
    }
    tags <- paste0 (sprintf ("<tag k='%s' v='%s'/>", names (tags), tags),
        collapse = ""
    )
    sprintf ("<node id='%d' lat='%f' lon='%f'>%s</node>", id, lat, lon, tags)
}

osm_way <- function (id, nds, tags) {

    nds <- paste0 (sprintf ("<nd ref='%d'/>", nds), collapse = "")
    tags <- paste0 (sprintf ("<tag k='%s' v='%s'/>", names (tags), tags),
        collapse = ""
    )
    sprintf ("<way id='%d'>%s%s</way>", id, nds, tags)
}

# Input for `process_onstreet_lanes()`. Column names retain the colons of the
# OSM keys, as returned by `osmdata_sf()`.
onstreet_input <- function (ways, nodes) {

    f <- write_test_osm (nodes, ways)
    lines <- osmdata::osmdata_sf (doc = f, quiet = TRUE)$osm_lines
    list (osm_lines = lines)
}

# A small OSM file with data in two places, 'here' within the bounding box
# returned by `test_bb ()`, and 'there' well outside of it. Nodes must be in
# order of their IDs, as for any OSM file.
test_bb <- function () c (13.39, 52.49, 13.41, 52.51)

make_test_extract <- function (dir, name = "planet") {

    tags_pk <- c (amenity = "parking", capacity = "30")
    tags_bd <- c (building = "yes", height = "10")
    nodes <- c (
        # 'here': a parking node (1), and a building (nodes 10-13)
        osm_node (1L, 52.500, 13.400, tags_pk),
        # 'there': another parking node (2), and a parking lane (nodes 20-21)
        osm_node (2L, 52.900, 13.900, tags_pk),
        osm_node (10L, 52.5000, 13.4050),
        osm_node (11L, 52.5000, 13.4060),
        osm_node (12L, 52.5010, 13.4060),
        osm_node (13L, 52.5010, 13.4050),
        osm_node (14L, 52.5020, 13.4050),
        osm_node (15L, 52.5030, 13.4050),
        osm_node (20L, 52.9000, 13.9050),
        osm_node (21L, 52.9010, 13.9050)
    )
    ways <- c (
        osm_way (100L, c (10L, 11L, 12L, 13L, 10L), tags_bd),
        osm_way (101L, c (20L, 21L), c (
            highway = "residential",
            "parking:lane:both" = "parallel"
        )),
        # 'here': a street with parking lanes on both sides
        osm_way (102L, c (14L, 15L), c (
            highway = "residential",
            "parking:lane:both" = "parallel"
        ))
    )
    f_xml <- write_test_osm (nodes, ways)
    f <- fs::path (dir, paste0 (name, ".osm.pbf"))
    rosmium::osmium_cat (f_xml, f)
    f
}

extern crate svg;
use svg::Document;
use svg::node::element::Path;
use svg::node::element::path::Data;
//use svg::node::element::Circle;

extern crate rand;
use rand::{Rng, SeedableRng, StdRng};

extern crate clap;
use clap::{App, Arg};

use std::f64::consts::PI;

use std::collections::{HashMap, HashSet};

type Point = (f64, f64);
type Line = (Point, Point);

fn transform(p: Point, size: u64, offset: u64) -> Point {
    let t = |f: f64| f * size as f64 + (size + offset) as f64;
    (t(p.0), t(p.1))
}

fn draw_poly(poly: &Poly, size: u64, offset: u64) -> Option<Path> {
    if poly.closed {
        let mut data = Data::new().move_to(transform(
            poly.lines_and_segment_indexes[0].0 .0,
            size,
            offset,
        ));
        for &(line, _) in &poly.lines_and_segment_indexes {
            data = data.line_to(transform(line.1, size, offset));
        }
        data = data.close();

        let poly_color = poly.color.unwrap();
        let color = format!(
            "#{:02X}{:02X}{:02X}",
            poly_color.0, poly_color.1, poly_color.2
        );
        assert_eq!(color.len(), 7);
        let path = Path::new()
            .set("fill", color.to_string())
            .set("stroke", color)
            .set("stroke-width", 0)
            .set("d", data);
        Some(path)
    } else {
        None
    }
}

fn random_line<R: Rng>(rng: &mut R) -> Line {
    let radius_angle = rng.gen_range(0., 2. * PI);
    let position_on_radius: f64 = rng.gen_range(0., 1.);
    let offset_angle = position_on_radius.acos();
    let angle_1 = radius_angle + offset_angle;
    let angle_2 = radius_angle - offset_angle;
    (angle_1.sin_cos(), angle_2.sin_cos())
}

fn intersection(l0: Line, l1: Line) -> Point {
    let det = |l: Line| l.0 .0 * l.1 .1 - l.0 .1 * l.1 .0;

    let off0x = l0.0 .0 - l0.1 .0;
    let off0y = l0.0 .1 - l0.1 .1;
    let off1x = l1.0 .0 - l1.1 .0;
    let off1y = l1.0 .1 - l1.1 .1;

    let numx = det(l0) * off1x - det(l1) * off0x;
    let numy = det(l0) * off1y - det(l1) * off0y;
    let denom = off0x * off1y - off0y * off1x;
    (numx / denom, numy / denom)
}

fn between(a: Point, b: Point, c: Point) -> bool {
    (a < b) == (b < c)
}

fn reverse(line: Line) -> Line {
    (line.1, line.0)
}

#[derive(Debug)]
struct Segment {
    line: Line,
    low_x_neighbor_index: Option<usize>,
    high_x_neighbor_index: Option<usize>,
}

fn random_segment<R: Rng>(line: Line, segments: &[Segment], rng: &mut R) -> Segment {
    let mut endpoint_options = vec![(line.0, None), (line.1, None)];
    for (index, oth_seg) in segments.iter().enumerate() {
        let i_point = intersection(line, oth_seg.line);
        if between(oth_seg.line.0, i_point, oth_seg.line.1) {
            endpoint_options.push((i_point, Some(index)))
        }
    }
    endpoint_options.sort_by(|p1, p2| p1.0.partial_cmp(&p2.0).unwrap());
    let min_x = line.0 .0.min(line.1 .0);
    let max_x = line.0 .0.max(line.1 .0);
    let target_x = rng.gen_range(min_x, max_x);
    let high_index = endpoint_options
        .iter()
        .position(|p| p.0 .0 > target_x)
        .unwrap();
    let (low_point, low_index) = endpoint_options[high_index - 1];
    let (high_point, high_index) = endpoint_options[high_index];
    Segment {
        line: (low_point, high_point),
        low_x_neighbor_index: low_index,
        high_x_neighbor_index: high_index,
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
struct PolyIndex(usize);

#[derive(Clone, Eq, Hash, PartialEq)]
struct LineIndex(usize);

struct PolySet {
    all_polys: Vec<Poly>,
    active_polys: HashSet<PolyIndex>,
    segments_to_polys: HashMap<usize, HashSet<(PolyIndex, LineIndex)>>,
}

impl PolySet {
    fn new() -> Self {
        Self {
            all_polys: Vec::new(),
            active_polys: HashSet::new(),
            segments_to_polys: HashMap::new(),
        }
    }
    fn add(&mut self, poly: Poly) {
        let poly_index = PolyIndex(self.all_polys.len());
        self.active_polys.insert(poly_index);
        for (line_index, &(_, segment)) in poly.lines_and_segment_indexes.iter().enumerate() {
            self.segments_to_polys
                .entry(segment)
                .or_insert_with(HashSet::new)
                .insert((poly_index, LineIndex(line_index)));
        }
        self.all_polys.push(poly);
    }
    fn find(&self, segment: usize) -> HashSet<(PolyIndex, LineIndex)> {
        self.segments_to_polys[&segment].clone()
    }
    fn lookup(&self, index: PolyIndex) -> &Poly {
        assert!(self.active_polys.contains(&index));
        &self.all_polys[index.0]
    }
    fn remove_all(&mut self, indexes: &[PolyIndex]) -> Vec<Poly> {
        indexes
            .iter()
            .map(|&index| {
                let was_there = self.active_polys.remove(&index);
                assert!(was_there);
                let poly = self.all_polys[index.0].clone();
                poly.lines_and_segment_indexes.iter().enumerate().for_each(
                    |(line_index, &(_, segment))| {
                        let was_there = self.segments_to_polys
                            .get_mut(&segment)
                            .unwrap()
                            .remove(&(index, LineIndex(line_index)));
                        assert!(was_there);
                    },
                );
                poly
            })
            .collect()
    }
    fn active(self) -> Vec<Poly> {
        self.active_polys
            .iter()
            .map(|index| self.all_polys[index.0].clone())
            .collect()
    }
}

type Color = (u8, u8, u8);
// Lines overlap, curling left (ccw)
#[derive(Clone, Debug)]
struct Poly {
    lines_and_segment_indexes: Vec<(Line, usize)>,
    closed: bool,
    color: Option<Color>,
}

fn jitter<R: Rng>(color_channel: u8, age: f64, rng: &mut R, color_jitter: u8) -> u8 {
    let current_jitter = (f64::from(color_jitter) * (1. - age)) as i16;
    (i16::from(color_channel) + rng.gen_range(-current_jitter, current_jitter + 1))
        .max(0)
        .min(255) as u8
}

impl Poly {
    fn add_color<R: Rng>(
        &mut self,
        maybe_old_color: Option<Color>,
        age: f64,
        rng: &mut R,
        color_jitter: u8,
    ) {
        assert!(self.color.is_none());
        self.color = Some(if let Some(old_color) = maybe_old_color {
            (
                jitter(old_color.0, age, rng, color_jitter),
                jitter(old_color.1, age, rng, color_jitter),
                jitter(old_color.2, age, rng, color_jitter),
            )
        } else {
            (
                rng.gen_range(0, 256) as u8,
                rng.gen_range(0, 256) as u8,
                rng.gen_range(0, 256) as u8,
            )
        });
    }
}

fn on_left_side(line: Line, point: Point) -> bool {
    let along_line = (line.1 .0 - line.0 .0, line.1 .1 - line.0 .1);
    let perp_line = (along_line.1, -along_line.0);
    let offset = (point.0 - line.0 .0, point.1 - line.0 .1);
    let distance = offset.0 * perp_line.0 + offset.1 * perp_line.1;
    distance > 0.
}

fn pop_surrounding_polys(segment: &Segment, polys: &mut PolySet) -> Vec<Poly> {
    if segment.low_x_neighbor_index.is_none() && segment.high_x_neighbor_index.is_none() {
        vec![]
    } else {
        let surroundings: Vec<PolyIndex> = [
            (segment.low_x_neighbor_index, segment.line),
            (segment.high_x_neighbor_index, reverse(segment.line)),
        ].into_iter()
            .flat_map(|&(maybe_neighbor_index, line_into_poly)| {
                maybe_neighbor_index
                    .iter()
                    .flat_map(|&neighbor_index| {
                        polys
                            .find(neighbor_index)
                            .iter()
                            .filter(|&&(poly_index, ref line_index)| {
                                let (line, segment_index) = polys
                                    .lookup(poly_index)
                                    .lines_and_segment_indexes[line_index.0];
                                segment_index == neighbor_index
                                    && between(line.0, line_into_poly.0, line.1)
                                    && on_left_side(line, line_into_poly.1)
                            })
                            .map(|&(poly_index, _)| poly_index)
                            .collect::<Vec<PolyIndex>>()
                    })
                    .collect::<Vec<PolyIndex>>()
            })
            .collect::<HashSet<PolyIndex>>()
            .into_iter()
            .collect();
        assert!(
            surroundings.len() == 2 && segment.low_x_neighbor_index.is_some()
                && segment.high_x_neighbor_index.is_some() || surroundings.len() == 1
        );
        polys.remove_all(&surroundings)
    }
}

fn split_3<T: Copy>(vec: &[T], index: usize) -> (&[T], T, &[T]) {
    let (piece_1, piece_2p) = vec.split_at(index);
    let (ts, piece_2) = piece_2p.split_at(1);
    (piece_1, ts[0], piece_2)
}

fn split_poly(segment: &Segment, segment_index: usize, surrounding_polys: &[Poly]) -> (Poly, Poly) {
    match surrounding_polys.len() {
        0 => {
            let new_poly = |line: Line| Poly {
                lines_and_segment_indexes: vec![(line, segment_index)],
                closed: false,
                color: None,
            };
            (new_poly(segment.line), new_poly(reverse(segment.line)))
        }
        1 => {
            let poly = &surrounding_polys[0];
            if let (Some(low_index), Some(high_index)) =
                (segment.low_x_neighbor_index, segment.high_x_neighbor_index)
            {
                let pos = |end_index: usize| {
                    poly.lines_and_segment_indexes
                        .iter()
                        .position(|&(_line, index)| index == end_index)
                        .unwrap()
                };
                let mut sorted_pos = vec![
                    (pos(low_index), segment.line.0),
                    (pos(high_index), segment.line.1),
                ];
                sorted_pos.sort_by_key(|pos| pos.0);
                let (piece_12, max_li, piece_3) =
                    split_3(&poly.lines_and_segment_indexes, sorted_pos[1].0);
                let (piece_1, min_li, piece_2) = split_3(piece_12, sorted_pos[0].0);
                let (min_line, min_index) = min_li;
                let (max_line, max_index) = max_li;
                let min_min_line = (min_line.0, sorted_pos[0].1);
                let min_med_line = (sorted_pos[0].1, min_line.1);
                let max_med_line = (max_line.0, sorted_pos[1].1);
                let max_max_line = (sorted_pos[1].1, max_line.1);
                let outer_poly = {
                    let mut outer_lis = vec![];
                    outer_lis.extend(piece_1);
                    outer_lis.push((min_min_line, min_index));
                    outer_lis.push(((sorted_pos[0].1, sorted_pos[1].1), segment_index));
                    outer_lis.push((max_max_line, max_index));
                    outer_lis.extend(piece_3);
                    Poly {
                        lines_and_segment_indexes: outer_lis,
                        closed: poly.closed,
                        color: None,
                    }
                };
                let inner_poly = {
                    let mut inner_lis = vec![];
                    inner_lis.push(((sorted_pos[1].1, sorted_pos[0].1), segment_index));
                    inner_lis.push((min_med_line, min_li.1));
                    inner_lis.extend(piece_2);
                    inner_lis.push((max_med_line, max_li.1));
                    Poly {
                        lines_and_segment_indexes: inner_lis,
                        closed: true,
                        color: None,
                    }
                };
                (inner_poly, outer_poly)
            } else {
                assert!(!poly.closed);
                let (index, line_into_poly) = if let Some(low_index) = segment.low_x_neighbor_index
                {
                    assert!(segment.high_x_neighbor_index.is_none());
                    (low_index, segment.line)
                } else {
                    (
                        segment.high_x_neighbor_index.unwrap(),
                        reverse(segment.line),
                    )
                };
                let pos = poly.lines_and_segment_indexes
                    .iter()
                    .position(|&(_line, segment_index)| segment_index == index)
                    .unwrap();
                let (piece_1, med_li, piece_2) = split_3(&poly.lines_and_segment_indexes, pos);
                let (med_line, med_index) = med_li;
                let min_med_line = (med_line.0, line_into_poly.0);
                let med_max_line = (line_into_poly.0, med_line.1);
                let min_poly = {
                    let mut min_lis = piece_1.to_vec();
                    min_lis.push((min_med_line, med_index));
                    min_lis.push((line_into_poly, segment_index));
                    Poly {
                        lines_and_segment_indexes: min_lis,
                        closed: false,
                        color: None,
                    }
                };
                let max_poly = {
                    let mut max_lis = vec![];
                    max_lis.push((reverse(line_into_poly), segment_index));
                    max_lis.push((med_max_line, med_li.1));
                    max_lis.extend(piece_2);
                    Poly {
                        lines_and_segment_indexes: max_lis,
                        closed: false,
                        color: None,
                    }
                };
                (min_poly, max_poly)
            }
        }
        2 => {
            let low_index = segment.low_x_neighbor_index.unwrap();
            let high_index = segment.high_x_neighbor_index.unwrap();
            let mut poly_index_points = vec![];
            for poly in surrounding_polys {
                for &(target_poly_index, point) in
                    &[(low_index, segment.line.0), (high_index, segment.line.1)]
                {
                    for (out_index, &(line, poly_index)) in
                        poly.lines_and_segment_indexes.iter().enumerate()
                    {
                        if target_poly_index == poly_index && between(line.0, point, line.1)
                        {
                            poly_index_points.push((poly, out_index, point))
                        }
                    }
                }
            }
            assert_eq!(poly_index_points.len(), 2);
            let make_poly = |v: Vec<(&Poly, usize, Point)>| {
                let (poly1, index1, point1) = v[0];
                let (poly2, index2, point2) = v[1];
                assert!(!poly1.closed);
                assert!(!poly2.closed);
                let (piece11, med_lis1, _piece12) =
                    split_3(&poly1.lines_and_segment_indexes, index1);
                let (med_line1, med_index1) = med_lis1;
                let (_piece21, med_lis2, piece22) =
                    split_3(&poly2.lines_and_segment_indexes, index2);
                let (med_line2, med_index2) = med_lis2;
                let mut out_lis1: Vec<(Line, usize)> = vec![];
                out_lis1.extend(piece11);
                out_lis1.push(((med_line1.0, point1), med_index1));
                out_lis1.push(((point1, point2), segment_index));
                out_lis1.push(((point2, med_line2.1), med_index2));
                out_lis1.extend(piece22);
                Poly {
                    lines_and_segment_indexes: out_lis1,
                    closed: false,
                    color: None,
                }
            };
            (
                make_poly(poly_index_points.clone()),
                make_poly(poly_index_points.into_iter().rev().collect()),
            )
        }
        _ => panic!("Too many polys for split_poly"),
    }
}

fn break_glass(
    n: usize,
    num_segments: usize,
    color_jitter: u8,
    maybe_seed: Option<usize>,
) -> Vec<Poly> {
    let mut segments = vec![];
    let mut poly_set = PolySet::new();
    let mut rng: StdRng = if let Some(seed) = maybe_seed {
        let random_seed: &[usize] = &[0, 0, 0, seed];
        SeedableRng::from_seed(random_seed)
    } else {
        StdRng::new().unwrap()
    };
    for i in 0..num_segments {
        let line = random_line(&mut rng);
        let segment = random_segment(line, &segments, &mut rng);
        let surrounding_polys = pop_surrounding_polys(&segment, &mut poly_set);
        let (mut poly1, mut poly2) = split_poly(&segment, segments.len(), &surrounding_polys);
        let maybe_old_color = if surrounding_polys.len() == 1 {
            surrounding_polys[0].color
        } else {
            None
        };
        let age = i as f64 / n as f64;
        poly1.add_color(maybe_old_color, age, &mut rng, color_jitter);
        poly2.add_color(maybe_old_color, age, &mut rng, color_jitter);
        poly_set.add(poly1);
        poly_set.add(poly2);
        segments.push(segment);
    }
    poly_set.active()
}

fn draw_glass(
    output_file: &str,
    size: u64,
    offset: u64,
    num_segments: usize,
    color_jitter: u8,
    seed: Option<usize>,
) {
    let width = size * 2 + offset * 2;
    let mut document = Document::new().set("viewBox", (0, 0, width, width));
    let background_box = Data::new()
        .move_to((0, 0))
        .line_by((0, width))
        .line_by((width, 0))
        .line_by((0, -(width as i64)))
        .close();

    let background = Path::new().set("fill", "black").set("d", background_box);
    document = document.add(background);

    let polys = break_glass(num_segments, num_segments, color_jitter, seed);
    for poly in polys {
        if poly.closed {
            document = document.add(draw_poly(&poly, size, offset).unwrap());
        }
    }

    svg::save(output_file, &document).unwrap();
}
fn main() {
    let matches = App::new("Broken Glass")
        .version("0.3")
        .author("Isaac Grosof <isaacbg227@gmail.com>")
        .arg(
            Arg::with_name("OUTPUT")
                .help("Sets the output file to write the image to")
//                .required(true),
        )
        .arg(
            Arg::with_name("size")
                .short("s")
                .takes_value(true)
                .help("Radius of the circle"),
        )
        .arg(
            Arg::with_name("offset")
                .short("o")
                .takes_value(true)
                .help("Offset distance from the edge of the circle to the edge of the image"),
        )
        .arg(
            Arg::with_name("segments")
                .short("n")
                .takes_value(true)
                .help("Number of segments to draw"),
        )
        .arg(
            Arg::with_name("seed")
                .short("r")
                .takes_value(true)
                .help("Random seed to use, if any"),
        )
        .arg(
            Arg::with_name("jitter")
                .short("j")
                .takes_value(true)
                .help("Amount of color jitter to begin with"),
        )
        .get_matches();

    let output_file = matches.value_of("OUTPUT").unwrap_or("image.svg");
    let size = matches.value_of("size").map_or(800, |size_str| {
        size_str
            .parse()
            .expect("Size should be a nonnegative integer")
    });
    let offset = matches.value_of("offset").map_or(20, |offset_str| {
        offset_str
            .parse()
            .expect("Offset should be a nonnegative integer")
    });
    let num_segments = matches.value_of("segments").map_or(4000, |segments_str| {
        segments_str
            .parse()
            .expect("Number of segments should be a nonnegative integer")
    });
    let seed = matches.value_of("seed").map(|seed_str| {
        seed_str
            .parse()
            .expect("Random seed should be a nonnegative integer")
    });
    let color_jitter = matches.value_of("jitter").map_or(64, |jitter_str| {
        jitter_str
            .parse()
            .expect("Color jitter should be a nonnegative integer")
    });
    draw_glass(output_file, size, offset, num_segments, color_jitter, seed);
}

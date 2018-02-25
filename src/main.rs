extern crate svg;
use svg::Document;
use svg::node::element::Path;
use svg::node::element::path::Data;
use svg::node::element::Circle;

extern crate rand;
use rand::thread_rng;
use rand::Rng;

use std::f64::consts::PI;

const SIZE: u64 = 400;
const OFFSET: u64 = 20;

type Point = (f64, f64);
type Line = (Point, Point);

fn transform(p: Point) -> Point {
    (
        p.0 * SIZE as f64 + (SIZE + OFFSET) as f64,
        p.1 * SIZE as f64 + (SIZE + OFFSET) as f64,
    )
}
fn draw_line(line: Line) -> Path {
    let data = Data::new()
        .move_to(transform(line.0))
        .line_to(transform(line.1));

    let path = Path::new()
        .set("fill", "none")
        .set("stroke", "black")
        .set("stroke-width", 2)
        .set("d", data);
    path
}

fn draw_poly<R: Rng>(poly: &Poly, rng: &mut R) -> Option<Path> {
    if poly.closed {
        let mut data = Data::new().move_to(transform(poly.lines_and_segment_indexes[0].0 .0));
        for &(line, _) in &poly.lines_and_segment_indexes {
            data = data.line_to(transform(line.1));
        }
        data = data.close();

        let color = format!(
            "#{:X}{:X}{:X}",
            rng.gen_range(0, 256),
            rng.gen_range(0, 256),
            rng.gen_range(0, 256)
        );
        let path = Path::new()
            .set("fill", color.to_string())
            .set("stroke", color)
            .set("stroke-width", 1)
            .set("d", data);
        Some(path)
    } else {
        None
    }
}

fn random_line() -> Line {
    let mut rng = thread_rng();
    let radius_angle = rng.gen_range(0., 2. * PI);
    let position_on_radius: f64 = rng.gen_range(0., 1.);
    let offset_angle = position_on_radius.acos();
    let angle_1 = radius_angle + offset_angle;
    let angle_2 = radius_angle - offset_angle;
    (angle_1.sin_cos(), angle_2.sin_cos())
}

fn intersection(l0: Line, l1: Line) -> Point {
    let det0 = l0.0 .0 * l0.1 .1 - l0.0 .1 * l0.1 .0;
    let det1 = l1.0 .0 * l1.1 .1 - l1.0 .1 * l1.1 .0;

    let off0x = l0.0 .0 - l0.1 .0;
    let off0y = l0.0 .1 - l0.1 .1;
    let off1x = l1.0 .0 - l1.1 .0;
    let off1y = l1.0 .1 - l1.1 .1;

    let numx = det0 * off1x - det1 * off0x;
    let numy = det0 * off1y - det1 * off0y;
    let denom = off0x * off1y - off0y * off1x;
    (numx / denom, numy / denom)
}

fn between(a: f64, b: f64, c: f64) -> bool {
    a <= b && b <= c || a >= b && b >= c
}

struct Segment {
    line: Line,
    low_x_neighbor_index: Option<usize>,
    high_x_neighbor_index: Option<usize>,
}

fn random_segment<R: Rng>(line: Line, segments: &Vec<Segment>, rng: &mut R) -> Segment {
    let mut endpoint_options = vec![(line.0, None), (line.1, None)];
    for (index, oth_seg) in segments.iter().enumerate() {
        let i_point = intersection(line, oth_seg.line);
        if between(oth_seg.line.0 .0, i_point.0, oth_seg.line.1 .0) {
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

#[derive(Clone, Debug)]
struct Poly {
    lines_and_segment_indexes: Vec<(Line, usize)>,
    closed: bool,
}

// Add posibility of 2 surrounding polys,
// from point two disjoint circle edge polys.
// Would happen from poly defined on both sides,
// in two different polys.
fn surrounding_poly_index(segment: &Segment, polys: &Vec<Poly>) -> Option<usize> {
    if segment.low_x_neighbor_index.is_none() && segment.high_x_neighbor_index.is_none() {
        None
    } else {
        polys.iter().position(|poly| {
            segment.low_x_neighbor_index.map_or(!poly.closed, |low_index| {
                poly.lines_and_segment_indexes
                    .iter()
                    .any(|&(line, segment_index)| {
                        segment_index == low_index
                            && between(line.0 .0, segment.line.0 .0, line.1 .0)
                    })
            }) && segment.high_x_neighbor_index.map_or(!poly.closed, |high_index| {
                poly.lines_and_segment_indexes
                    .iter()
                    .any(|&(line, segment_index)| {
                        segment_index == high_index
                            && between(line.0 .0, segment.line.1 .0, line.1 .0)
                    })
            })
        })
    }
}

fn split_poly(
    segment: &Segment,
    segment_index: usize,
    surrounding_poly: &Option<Poly>,
) -> (Poly, Poly) {
    if let &Some(ref poly) = surrounding_poly {
        if let (Some(low_index), Some(high_index)) =
            (segment.low_x_neighbor_index, segment.high_x_neighbor_index)
        {
            let low_pos = poly.lines_and_segment_indexes
                .iter()
                .position(|&(_line, index)| index == low_index)
                .unwrap();
            let high_pos = poly.lines_and_segment_indexes
                .iter()
                .position(|&(_line, index)| index == high_index)
                .unwrap();
            let mut sorted_pos = vec![(low_pos, segment.line.0), (high_pos, segment.line.1)];
            sorted_pos.sort_by_key(|pos| pos.0);
            let (piece_12, piece_3p) = poly.lines_and_segment_indexes.split_at(sorted_pos[1].0);
            let (piece_1, piece_2p) = piece_12.split_at(sorted_pos[0].0);
            let (max_lis, piece_3) = piece_3p.split_at(1);
            let (min_lis, piece_2) = piece_2p.split_at(1);
            let max_li = max_lis[0];
            let min_li = min_lis[0];
            let max_line = max_li.0;
            let min_line = min_li.0;
            let min_min_line = (min_line.0, sorted_pos[0].1);
            let min_med_line = (sorted_pos[0].1, min_line.1);
            let max_med_line = (max_line.0, sorted_pos[1].1);
            let max_max_line = (sorted_pos[1].1, max_line.1);
            let outer_poly = {
                let mut outer_lis = vec![];
                outer_lis.extend(piece_1);
                outer_lis.push((min_min_line, min_li.1));
                outer_lis.push(((sorted_pos[0].1, sorted_pos[1].1), segment_index));
                outer_lis.push((max_max_line, max_li.1));
                outer_lis.extend(piece_3);
                Poly {
                    lines_and_segment_indexes: outer_lis,
                    closed: poly.closed,
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
                }
            };
            (inner_poly, outer_poly)
        } else {
            assert!(!poly.closed);
            let (index, point, non_point) = if let Some(low_index) = segment.low_x_neighbor_index {
                assert!(segment.high_x_neighbor_index.is_none());
                (low_index, segment.line.0, segment.line.1)
            } else {
                (segment.high_x_neighbor_index.unwrap(), segment.line.1, segment.line.0)
            };
            let pos = poly.lines_and_segment_indexes
                .iter()
                .position(|&(_line, segment_index)| segment_index == index)
                .unwrap();
            let (piece_1, piece_2p) = poly.lines_and_segment_indexes.split_at(pos);
            let (med_lis, piece_2) = piece_2p.split_at(1);
            let med_li = med_lis[0];
            let med_line = med_li.0;
            let min_med_line = (med_line.0, point);
            let med_max_line = (point, med_line.1);
            let min_poly = {
                let mut min_lis = piece_1.to_vec();
                min_lis.push((min_med_line, med_li.1));
                min_lis.push(((point, non_point), segment_index));
                Poly {
                    lines_and_segment_indexes: min_lis,
                    closed: false,
                }
            };
            let max_poly = {
                let mut max_lis = piece_2.to_vec();
                max_lis.insert(0, (med_max_line, med_li.1));
                max_lis.insert(0, ((non_point, point), segment_index));
                Poly {
                    lines_and_segment_indexes: max_lis,
                    closed: false,
                }
            };
            (min_poly, max_poly)
        }
    } else {
        let poly = Poly {
            lines_and_segment_indexes: vec![(segment.line, segment_index)],
            closed: false,
        };
        (poly.clone(), poly.clone())
    }
}

fn break_glass(document: Document, n: usize) -> Document {
    let mut document = document;
    let mut segments = vec![];
    let mut polys = vec![];
    let mut rng = thread_rng();
    for i in 0..n {
        let line = random_line();
        let segment = random_segment(line, &segments, &mut rng);
        document = document.add(draw_line(segment.line));
        let maybe_index = surrounding_poly_index(&segment, &mut polys);
        let surrounding_poly = maybe_index.map(|i| polys.remove(i));
        let (poly1, poly2) = split_poly(&segment, segments.len(), &surrounding_poly);
        if poly1.closed {
            document = document.add(draw_poly(&poly1, &mut rng).unwrap());
        }
        if poly2.closed {
            document = document.add(draw_poly(&poly2, &mut rng).unwrap());
        }
        polys.push(poly1);
        polys.push(poly2);
        segments.push(segment);
    }
    document
}

fn main() {
    let circle = Circle::new()
        .set("cx", SIZE + OFFSET)
        .set("cy", SIZE + OFFSET)
        .set("r", SIZE)
        .set("fill", "none")
        .set("stroke", "black")
        .set("stroke-width", 2);

    let document = Document::new()
        .set(
            "viewBox",
            (0, 0, SIZE * 2 + OFFSET * 2, SIZE * 2 + OFFSET * 2),
        )
        .add(circle);

    let document = break_glass(document, 1000);

    svg::save("image.svg", &document).unwrap();
}

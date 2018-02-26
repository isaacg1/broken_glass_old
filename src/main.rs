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
const COLOR_JITTER: i16 = 64;

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

fn draw_poly(poly: &Poly) -> Option<Path> {
    if poly.closed {
        let mut data = Data::new().move_to(transform(poly.lines_and_segment_indexes[0].0 .0));
        for &(line, _) in &poly.lines_and_segment_indexes {
            data = data.line_to(transform(line.1));
        }
        data = data.close();

        let poly_color = poly.color.unwrap();
        let color = format!("#{:02X}{:02X}{:02X}", poly_color.0, poly_color.1, poly_color.2);
        assert_eq!(color.len(), 7);
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

#[derive(Debug)]
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

type Color = (u8, u8, u8);
// Lines overlap, open is to the + side.
#[derive(Clone, Debug)]
struct Poly {
    lines_and_segment_indexes: Vec<(Line, usize)>,
    closed: bool,
    color: Option<Color>,
}

fn jitter(color_channel: u8, age: f64) -> u8 {
    let current_jitter = (COLOR_JITTER as f64 * (1.-age)) as i16;
    (color_channel as i16 + thread_rng().gen_range(-current_jitter, current_jitter+1))
        .max(0)
        .min(255) as u8
}

impl Poly {
    fn add_color(&mut self, maybe_old_color: Option<Color>, age: f64) {
        assert!(self.color.is_none());
        self.color = Some(if let Some(old_color) = maybe_old_color {
            (
                jitter(old_color.0, age),
                jitter(old_color.1, age),
                jitter(old_color.2, age),
            )
        } else {
            let mut rng = thread_rng();
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

// Add posibility of 2 surrounding polys,
// from point two disjoint circle edge polys.
// Would happen from poly defined on both sides,
// in two different polys.
fn surrounding_poly_index(segment: &Segment, polys: &Vec<Poly>) -> Vec<usize> {
    if segment.low_x_neighbor_index.is_none() && segment.high_x_neighbor_index.is_none() {
        vec![]
    } else {
        let surroundings: Vec<usize> = polys
            .iter()
            .enumerate()
            .filter(|&(_, poly)| {
                segment.low_x_neighbor_index.map_or(false, |low_index| {
                    poly.lines_and_segment_indexes
                        .iter()
                        .any(|&(line, segment_index)| {
                            segment_index == low_index
                                && between(line.0 .0, segment.line.0 .0, line.1 .0)
                                && on_left_side(line, segment.line.1)
                        })
                }) || segment.high_x_neighbor_index.map_or(false, |high_index| {
                    poly.lines_and_segment_indexes
                        .iter()
                        .any(|&(line, segment_index)| {
                            segment_index == high_index
                                && between(line.0 .0, segment.line.1 .0, line.1 .0)
                                && on_left_side(line, segment.line.0)
                        })
                })
            })
            .map(|(i, _)| i)
            .collect();
        assert!(
            surroundings.len() == 2 && segment.low_x_neighbor_index.is_some()
                && segment.high_x_neighbor_index.is_some() || surroundings.len() == 1
        );
        surroundings
    }
}

fn split_poly(
    segment: &Segment,
    segment_index: usize,
    surrounding_polys: &Vec<Poly>,
) -> (Poly, Poly) {
    if surrounding_polys.len() == 1 {
        let poly = &surrounding_polys[0];
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
            let (index, point, non_point) = if let Some(low_index) = segment.low_x_neighbor_index {
                assert!(segment.high_x_neighbor_index.is_none());
                (low_index, segment.line.0, segment.line.1)
            } else {
                (
                    segment.high_x_neighbor_index.unwrap(),
                    segment.line.1,
                    segment.line.0,
                )
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
                    color: None,
                }
            };
            let max_poly = {
                let mut max_lis = vec![];
                max_lis.push(((non_point, point), segment_index));
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
    } else if surrounding_polys.is_empty() {
        let poly1 = Poly {
            lines_and_segment_indexes: vec![(segment.line, segment_index)],
            closed: false,
            color: None,
        };
        let poly2 = Poly {
            lines_and_segment_indexes: vec![((segment.line.1, segment.line.0), segment_index)],
            closed: false,
            color: None,
        };
        (poly1, poly2)
    } else {
        assert_eq!(surrounding_polys.len(), 2);
        let low_index = segment.low_x_neighbor_index.unwrap();
        let high_index = segment.high_x_neighbor_index.unwrap();
        let mut poly_index_points = vec![];
        for poly in surrounding_polys {
            for (target_poly_index, point) in
                vec![(low_index, segment.line.0), (high_index, segment.line.1)]
            {
                for (out_index, &(line, poly_index)) in
                    poly.lines_and_segment_indexes.iter().enumerate()
                {
                    if target_poly_index == poly_index && between(line.0 .0, point.0, line.1 .0) {
                        poly_index_points.push((poly, out_index, point))
                    }
                }
            }
        }
        assert_eq!(poly_index_points.len(), 2);
        let (poly1, index1, point1) = poly_index_points[0];
        let (poly2, index2, point2) = poly_index_points[1];
        assert!(!poly1.closed);
        assert!(!poly2.closed);
        let (piece11, piece12p) = poly1.lines_and_segment_indexes.split_at(index1);
        let (med_lis1, piece12) = piece12p.split_at(1);
        let (med_line1, med_index1) = med_lis1[0];
        let (piece21, piece22p) = poly2.lines_and_segment_indexes.split_at(index2);
        let (med_lis2, piece22) = piece22p.split_at(1);
        let (med_line2, med_index2) = med_lis2[0];
        let out_poly1 = {
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
        let out_poly2 = {
            let mut out_lis2: Vec<(Line, usize)> = vec![];
            out_lis2.extend(piece21);
            out_lis2.push(((med_line2.0, point2), med_index2));
            out_lis2.push(((point2, point1), segment_index));
            out_lis2.push(((point1, med_line1.1), med_index1));
            out_lis2.extend(piece12);
            Poly {
                lines_and_segment_indexes: out_lis2,
                closed: false,
                color: None,
            }
        };
        (out_poly1, out_poly2)
    }
}

fn break_glass(document: Document, n: usize) -> Document {
    let mut segments = vec![];
    let mut polys = vec![];
    let mut rng = thread_rng();
    for i in 0..n {
        let line = random_line();
        let segment = random_segment(line, &segments, &mut rng);
        let maybe_index = surrounding_poly_index(&segment, &mut polys);
        let surrounding_polys: Vec<Poly> =
            // Must be reversed so we don't remove the wrong one
            maybe_index.into_iter().rev().map(|i| polys.remove(i)).collect();
        let (mut poly1, mut poly2) = split_poly(&segment, segments.len(), &surrounding_polys);
        let maybe_old_color = if surrounding_polys.len() == 1 {
            surrounding_polys[0].color
        } else {
            None
        };
        let age = i as f64/n as f64;
        poly1.add_color(maybe_old_color, age);
        poly2.add_color(maybe_old_color, age);
        polys.push(poly1);
        polys.push(poly2);
        segments.push(segment);
    }
    let mut document = document;
    for segment in segments {
        document = document.add(draw_line(segment.line));
    };
    for poly in polys {
        if poly.closed {
            document = document.add(draw_poly(&poly).unwrap());
        }
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

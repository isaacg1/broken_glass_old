extern crate svg;
use svg::Document;
use svg::node::element::Path;
use svg::node::element::path::Data;
use svg::node::element::Circle;

const SIZE: u64 = 200;
const OFFSET: u64 = 20;

fn main() {
    let circle = Circle::new()
        .set("cx", SIZE + OFFSET)
        .set("cy", SIZE + OFFSET)
        .set("r", SIZE)
        .set("fill", "none")
        .set("stroke", "black")
        .set("stroke-width", 1);

	let document = Document::new()
		.set("viewBox", (0, 0, SIZE * 2 + OFFSET * 2, SIZE * 2 + OFFSET * 2))
        .add(circle);

	svg::save("image.svg", &document).unwrap();
}

use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::{
    glam::Vec2,
    primitives::{Prim, PrimTree},
};

const COLLISION_LINE_WIDTH: f32 = 2.0;
///Queries the `tree`, reutrns an ID of `AttribLocation`, if any is found at `at` relative to the
///`tree`'s origin.
pub fn find_collision(tree: &PrimTree, mut at: Vec2) -> Option<AttribLocation> {
    //test self, if any is found, return, otherwise recurse

    let overlabs = match &tree.prim {
        Prim::Box(b) => {
            //we are basically always a bbox (yay)
            b.from.x < at.x && b.to.x > at.x && b.from.y < at.y && b.to.y > at.y
        }
        Prim::Offset(o) => {
            at -= Vec2::new(o.x, o.y);
            //offset always intersects...or so :eyes:
            true
        }
        Prim::Line(l) => {
            //NOTE: we just project `at` onto the line vector and check if its in range.
            //      if its in range, we check if its _closer_ then Self::LINE_WIDTH

            let lvec = Vec2::from_array((l.to - l.from).to_array());
            let local_at = at - Vec2::from_array(l.from.to_array());
            let llen = lvec.length();
            let lnorm = lvec.normalize();

            let a = local_at.dot(lnorm);
            //if at projected on lvec is < llen (is within line bound) && the distance of at to the lvec is < COLLISION_LINE_WIDTH
            // then we declare it as _intersecting_.
            if a < llen && a > 0.0 && (a * lnorm - local_at).length() < COLLISION_LINE_WIDTH {
                true
            } else {
                false
            }
        }
        Prim::Path(p) => {
            //This is the same as the line code, but for each segment

            let mut is_intersecting = false;
            for idx in 0..(p.points.len() - 1) {
                let from = Vec2::from_array(p.points[idx].to_array());
                let to = Vec2::from_array(p.points[idx + 1].to_array());
                let lvec = to - from;
                let local_at = at - from;
                let llen = lvec.length();
                let lnorm = lvec.normalize();

                let a = local_at.dot(lnorm);
                if a < llen && a > 0.0 && (a * lnorm - local_at).length() < COLLISION_LINE_WIDTH {
                    is_intersecting = true;
                    break;
                }
            }

            is_intersecting
        }
        Prim::Text(_t) => false,
    };

    if !overlabs {
        return None;
    }

    //in case of path/line do not search further
    if let Prim::Path(_) | Prim::Line(_) = tree.prim {
        return Some(tree.id.clone());
    }

    //does overlab, depending on the node kind, choose what to do next.
    //
    //In case of a _trivial_ node i.e Edges and ports, we return the id immediatly.
    //
    //In the other cases (node/region). We first check if any of the children gives us a _bette_
    //result, if not we return our selfs.

    //NOTE: the rev. is here, because the _region_ is always the first box. But we want to _use_ the region
    //      last. So we reverse the collection in order to test all sub-nodes and edges _before_ testing the region.
    for child in tree.children.iter().rev() {
        if let Some(better_result) = find_collision(child, at) {
            return Some(better_result);
        }
    }

    match tree.prim {
        Prim::Box(_) => Some(tree.id.clone()),
        _ => None,
    }
}

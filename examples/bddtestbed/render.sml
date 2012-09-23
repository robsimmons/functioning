structure Render =
struct

open Types
open GL
open BDDOps
infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:


fun draw_polygon vertexList (RGB (r, g, b)) =
    (
     glColor3d r g b;
     glBegin GL_LINE_LOOP;
     List.map (fn v => glVertex2d (BDDMath.vec2x v) (BDDMath.vec2y v)) vertexList;
     glEnd()
    )

fun draw_solid_polygon vertexList (RGB (r, g, b)) =
    (
     glEnable GL_BLEND;
     glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
     glColor4d (0.5 * r) (0.5 * g) (0.5 * b) 0.5;
     glBegin GL_TRIANGLE_FAN;
     List.map (fn v => glVertex2d (BDDMath.vec2x v) (BDDMath.vec2y v)) vertexList;
     glEnd ();
     glDisable GL_BLEND;

     glColor4d r g b 1.0;
     glBegin GL_LINE_LOOP;
     List.map (fn v => glVertex2d (BDDMath.vec2x v) (BDDMath.vec2y v)) vertexList;
     glEnd()
    )

fun draw_solid_circle center radius axis (RGB (r, g, b)) =
    let val (centerx, centery) = BDDMath.vec2xy center
        val k_segments = 16
        val k_increment = 2.0 * Math.pi / (Real.fromInt k_segments)
        val (px, py) = BDDMath.vec2xy (center :+: (radius *: axis))
        fun draw_vertex ii =
            let val theta = k_increment * (Real.fromInt ii)
                val v = center :+: (radius *: (BDDMath.vec2 (Math.cos theta,
                                                             Math.sin theta)))
                val (x, y) = BDDMath.vec2xy v
            in glVertex2d x y
            end
    in
        glEnable GL_BLEND;
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
        glColor4d (0.5 * r) (0.5 * g) (0.5 * b) 0.5;
        glBegin GL_TRIANGLE_FAN;
        List.tabulate (k_segments, draw_vertex);
        glEnd ();
        glDisable GL_BLEND;

        glColor4d r g b 1.0;
        glBegin GL_LINE_LOOP;
        List.tabulate (k_segments, draw_vertex);
        glEnd();

        (* draw radius *)
        glBegin GL_LINES;
        glVertex2d centerx centery;
        glVertex2d px py;
        glEnd ()
    end

fun draw_point p size (RGB (r, g, b)) =
    (
     glPointSize size;
     glBegin GL_POINTS;
     glColor3d r g b;
     glVertex2d (BDDMath.vec2x p) (BDDMath.vec2y p);
     glEnd();
     glPointSize 1.0
    )

fun draw_segment p1 p2 (RGB (r, g, b)) =
    (
     glColor3d r g b;
     glBegin GL_LINES;
     glVertex2d (BDDMath.vec2x p1) (BDDMath.vec2y p1);
     glVertex2d (BDDMath.vec2x p2) (BDDMath.vec2y p2);
     glEnd()
    )

end

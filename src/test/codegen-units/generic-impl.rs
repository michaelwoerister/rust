
struct Struct<T> {
    x: T,
    f: fn(x: T) -> T,
}

fn id<T>(x: T) -> T { x }

impl<T> Struct<T> {

    fn new(x: T) -> Struct<T> {
        Struct {
            x: x,
            f: id
        }
    }

    fn get<T2>(self, x: T2) -> (T, T2) {
        (self.x, x)
    }
}

fn main() {
    let _ = Struct::new(0i32).get(0i16);
    let _ = Struct::new(0i64).get(0i16);
    let _ = Struct::new('c').get(0i16);
    let _ = Struct::new(Struct::new("str")).get(0i16);

    let _ = (Struct::new(Struct::new("str")).f)(Struct::new("str"));
}

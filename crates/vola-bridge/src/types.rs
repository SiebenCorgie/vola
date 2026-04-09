use vola_lib::vola_opt::common::Ty;

impl TypedValue {
    pub fn initialize(ty: &Ty) -> Self {
        let init_value = match ty {
            &Ty::SCALAR_BOOL => vec![wasmtime::Val::I32(0)],
            &Ty::SCALAR_INT => vec![wasmtime::Val::I32(0)],
            &Ty::SCALAR_REAL => vec![wasmtime::Val::F32(0)],

            &Ty::VEC2 => vec![wasmtime::Val::F32(0), wasmtime::Val::F32(0)],
            &Ty::VEC3 => vec![
                wasmtime::Val::F32(0),
                wasmtime::Val::F32(0),
                wasmtime::Val::F32(0),
            ],
            &Ty::VEC4 => vec![
                wasmtime::Val::F32(0),
                wasmtime::Val::F32(0),
                wasmtime::Val::F32(0),
                wasmtime::Val::F32(0),
            ],

            Ty::Tuple(sub_types) => {
                //initialize all sub-types, then flatten them into a single array
                sub_types
                    .iter()
                    .map(|st| Self::initialize(st).values.into_iter())
                    .flatten()
                    .collect()
            }
            //TODO: do we want to support arbitrarly shaped data? right now this effectively supports
            // scalar, int, vec and any tuple of those.
            _other => panic!("Can not initialize {ty}"),
        };

        Self {
            ty: ty.clone(),
            values: init_value,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypedValue {
    pub(crate) ty: Ty,
    ///The pre-initialized values of the arg
    pub(crate) values: Vec<wasmtime::Val>,
}

impl TypedValue {
    pub fn unwrap_f32(&self) -> f32 {
        assert_eq!(self.ty, Ty::SCALAR_REAL);
        self.values[0].unwrap_f32()
    }

    pub fn unwrap_int(&self) -> i32 {
        assert_eq!(self.ty, Ty::SCALAR_INT);
        self.values[0].unwrap_i32()
    }
    pub fn unwrap_bool(&self) -> bool {
        assert_eq!(self.ty, Ty::SCALAR_BOOL);
        let value = self.values[0].unwrap_i32();
        if value == 0 {
            false
        } else if value == 1 {
            true
        } else {
            panic!("Unexpected boolean value {value}")
        }
    }

    pub fn unwrap_vec2(&self) -> [f32; 2] {
        assert_eq!(self.ty, Ty::VEC2);
        let a = self.values[0].unwrap_f32();
        let b = self.values[1].unwrap_f32();
        [a, b]
    }
    pub fn unwrap_vec3(&self) -> [f32; 3] {
        assert_eq!(self.ty, Ty::VEC3);
        let a = self.values[0].unwrap_f32();
        let b = self.values[1].unwrap_f32();
        let c = self.values[2].unwrap_f32();
        [a, b, c]
    }
    pub fn unwrap_vec4(&self) -> [f32; 4] {
        assert_eq!(self.ty, Ty::VEC4);
        let a = self.values[0].unwrap_f32();
        let b = self.values[1].unwrap_f32();
        let c = self.values[2].unwrap_f32();
        let d = self.values[3].unwrap_f32();
        [a, b, c, d]
    }
}

impl From<i32> for TypedValue {
    fn from(value: i32) -> Self {
        TypedValue {
            ty: Ty::SCALAR_INT,
            values: vec![wasmtime::Val::I32(value)],
        }
    }
}

impl From<bool> for TypedValue {
    fn from(value: bool) -> Self {
        TypedValue {
            ty: Ty::SCALAR_BOOL,
            values: vec![wasmtime::Val::I32(value.into())],
        }
    }
}
impl From<f32> for TypedValue {
    fn from(value: f32) -> Self {
        TypedValue {
            ty: Ty::SCALAR_REAL,
            values: vec![wasmtime::Val::F32(value.to_bits())],
        }
    }
}
impl From<[f32; 2]> for TypedValue {
    fn from(value: [f32; 2]) -> Self {
        TypedValue {
            ty: Ty::VEC2,
            values: vec![
                wasmtime::Val::F32(value[0].to_bits()),
                wasmtime::Val::F32(value[1].to_bits()),
            ],
        }
    }
}
impl From<[f32; 3]> for TypedValue {
    fn from(value: [f32; 3]) -> Self {
        TypedValue {
            ty: Ty::VEC3,
            values: vec![
                wasmtime::Val::F32(value[0].to_bits()),
                wasmtime::Val::F32(value[1].to_bits()),
                wasmtime::Val::F32(value[2].to_bits()),
            ],
        }
    }
}
impl From<[f32; 4]> for TypedValue {
    fn from(value: [f32; 4]) -> Self {
        TypedValue {
            ty: Ty::VEC4,
            values: vec![
                wasmtime::Val::F32(value[0].to_bits()),
                wasmtime::Val::F32(value[1].to_bits()),
                wasmtime::Val::F32(value[2].to_bits()),
                wasmtime::Val::F32(value[3].to_bits()),
            ],
        }
    }
}

pub trait Builder {
    type BuilderType;

    fn builder() -> Self::BuilderType;
}

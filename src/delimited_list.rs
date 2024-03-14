pub struct DelimitedList<Elem, Delim> {
    head: Option<Elem>,
    rest: Vec<(Elem, Delim)>,
}

impl<Elem, Delim> DelimitedList<Elem, Delim> {
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn first(&self) -> Option<&Elem> {
        self.head.as_ref()
    }

    pub fn rest(&self) -> &Vec<(Elem, Delim)> {
        &self.rest
    }

    pub fn iter(&self) -> impl Iterator<Item = &Elem> {
        self.head
            .iter()
            .chain(self.rest.iter().map(|(elem, _)| elem))
    }

    pub fn into_iter(self) -> impl Iterator<Item = Elem> {
        self.head
            .into_iter()
            .chain(self.rest.into_iter().map(|(elem, _)| elem))
    }
}

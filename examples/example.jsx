function Example() {
  return (
    <Container className="container">
      <C1 show>
        <C2>
          C2 content
        </C2>
        <C3 disabled>
          {'C3 content'}
        </C3>
        <C4 />
      </C1>
    </Container>
  );
}

class Foo {
    public static final int five = 5;
    private int i;

    Foo() {
        this.i = this.five;
    }

    public void inc() {
        this.i++;
    }
}

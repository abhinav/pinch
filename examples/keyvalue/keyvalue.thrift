exception KeyDoesNotExist {
    1: optional string message
}

union Value {
    1: binary bin
    2: set<binary> binSet
}

service KeyValue {
    void setValue(1: string key, 2: Value value)

    Value getValue(1: string key)
        throws (1: KeyDoesNotExist doesNotExist)
}

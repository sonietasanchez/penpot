CREATE INDEX storage_object__hash_and_backend__idx
    ON storage_object ((metadata->>'~:hash'), backend);

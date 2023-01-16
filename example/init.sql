create table xy (x integer primary key, y text);

insert into xy
    values
        (1, 'foo'),
        (2, 'bar'),
        (3, 'baz'),
        (4, 'boo');

create publication pub for table xy;

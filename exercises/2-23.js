//// 2-23
//// 2-2-1
//// 2022/03/31
//// node ./2-23.js

/// The procedure `for-each` is similar to `map`. It takes as arguments a
/// list of elements. However, rather than forming a list of the results,
/// `for-each` just applies the procedure to each of the elements in turn, from
/// left to right. The values returned by applying the procedure to the elements
/// are not used at all -- `for-each` is used with procedures that perform an
/// action, such as printing. For example,
/// (for-each (lambda (x) (newline) (display x)) (list 57 321 88))
/// 57
/// 321
/// 88

class Pair {
  constructor(key, value) {
    this.key = key;
    this.value = value;
  };

  key = undefined;
  value = undefined;

  car() {
    return this.key;
  };
  cdr() {
    return this.value;
  };
};

const list = (...rest) =>
  rest.reverse().reduce((result, current) => new Pair(current, result), null);

const for_each = (procedure, items) => {
  const _continue = items => {
    procedure(items.car());
    iterator(items.cdr());
  };
  const iterator = items => {
    if (items) {
      _continue(items);
    };
  };

  iterator(items);
};

const test = () => {
  // const items = new Pair(0, new Pair(1, new Pair(2, new Pair(3, null))));
  const items = list(0, 1, 2, 3);
  const procedure = console.log;

  for_each(procedure, items);
};

test();


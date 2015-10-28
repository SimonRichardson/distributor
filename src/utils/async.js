'use strict';

module.exports = f => {
  process.nextTick(() => {
    f().unsafePerform();
  });
};
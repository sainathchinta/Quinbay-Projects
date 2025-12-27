package com.gdn.x.product.service.api.util;

import java.util.List;
import java.util.function.Function;

public interface ListUtil {

  <T> List<T> distinct(List<T> list, Function<T, Object> function);
}

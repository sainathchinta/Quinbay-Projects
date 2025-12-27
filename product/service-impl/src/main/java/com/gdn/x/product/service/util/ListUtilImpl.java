package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static java.util.stream.Collectors.toList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.springframework.stereotype.Service;

import com.gdn.x.product.service.api.util.ListUtil;

@Service
public class ListUtilImpl implements ListUtil {

  @Override
  public <T> List<T> distinct(List<T> list, Function<T, Object> function) {
    checkArgument(list != null, "list must not be null");
    Map<Object, Boolean> map = new HashMap<>();
    return list.stream().filter(obj -> map.putIfAbsent(function.apply(obj), Boolean.TRUE) == null)
        .collect(toList());
  }

}

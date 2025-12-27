package com.gdn.partners.pbp.outbound.AGPQuery;

import java.util.Map;
import java.util.Set;

public interface AGPQueryOutbound {
  /**
   * find number of order by product sku list
   *
   * @param productSkuList
   * @return
   */
  Map<String, Boolean> findNumberOfOrderByProductSkuList(Set<String> productSkuList);
}

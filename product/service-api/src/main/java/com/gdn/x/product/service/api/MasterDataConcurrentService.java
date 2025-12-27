package com.gdn.x.product.service.api;

import java.util.Map;
import java.util.Set;

public interface MasterDataConcurrentService {

  /**
   * Fetch master data details by list of product codes
   *
   * @param masterData
   * @param username
   * @param requestId
   * @param codes
   * @param concurrentSize
   * @param inAllProducts
   * @param <T>
   * @return
   */
  <T> Map<String, T> doConcurrentCall(Class<T> masterData, String username, String requestId,
      Set<String> codes, int concurrentSize, boolean inAllProducts);

}

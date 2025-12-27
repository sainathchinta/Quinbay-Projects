package com.gdn.x.product.service.api;

import java.util.Map;
import java.util.Set;

public interface MasterDataHelperService {

  <T> Map<String, T> getMasterData(Class<T> masterDataClass, String storeId, String username,
      String requestId, Set<String> codes, boolean inAllProducts, boolean useMasterDataCache) throws Exception;

}

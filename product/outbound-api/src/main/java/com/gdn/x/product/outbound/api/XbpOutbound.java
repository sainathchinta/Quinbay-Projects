package com.gdn.x.product.outbound.api;

import com.gdn.x.businesspartner.dto.ProductCounterResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public interface XbpOutbound {

  ProfileResponse getBusinessPartnerDetails(String storeId, String requestId, String username,
      String businessPartnerCode);

  ProductCounterResponse productCounterIncrementAndGet(String storeId, String requestId, String username, String businessPartnerCode);
}

package com.gdn.partners.pcu.external.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.feign.AGPQueryFeign;
import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;

@Component
public class AGPQueryQueryFeignFallback implements AGPQueryFeign {

  @Override
  public AgpSimpleQueryResponse findNumberOfOrder(String productSku, String from, String size, String itemStatus) {
    return null;
  }
}

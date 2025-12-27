package com.gdn.partners.pcu.external.client.fallback;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;

public class AGPQueryFeignFalbackTest {

  private AGPQueryQueryFeignFallback agpQueryQueryFeignFallback = new AGPQueryQueryFeignFallback();
  private static final String PRODUCT_SKU = "product-sku";

  @Test
  public void getActiveProductListByMerchantAndCategoryCodeTest() {
    AgpSimpleQueryResponse response = agpQueryQueryFeignFallback.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X");
    Assertions.assertNull(response);
  }

}

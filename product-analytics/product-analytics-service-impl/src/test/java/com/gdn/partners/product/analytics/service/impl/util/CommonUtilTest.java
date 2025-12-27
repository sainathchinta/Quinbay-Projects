package com.gdn.partners.product.analytics.service.impl.util;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.Date;

public class CommonUtilTest {

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void testMapToAutoApprovedWebResponse() {
    AutoApprovedProducts product = new AutoApprovedProducts();
    product.setProductCode("code");
    product.setProductName("name");
    product.setCategoryCode("categoryCode");
    product.setCategoryName("categoryName");
    product.setCommissionType("commissionType");
    product.setCreatedDate(new Date());
    product.setAssignedTo("assignedTo");
    product.setAssignedDate(new Date());
    product.setSellerCode("sellerCode");
    product.setSellerName("sellerName");
    product.setSellerBadge("sellerBadge");
    product.setOfficialSeller(true);
    product.setInternationalSeller(true);
    AutoApprovedListWebResponse result = CommonUtil.mapToAutoApprovedWebResponse(product);
    Assertions.assertEquals("code", result.getProductCode());
    Assertions.assertEquals("name", result.getProductName());
    Assertions.assertEquals("categoryCode", result.getCategoryCode());
    Assertions.assertEquals("categoryName", result.getCategoryName());
    Assertions.assertEquals("commissionType", result.getCommissionType());
  }

  @Test
  void testFetchValidDate() {
    Date inputDate = new Date();
    Long result = CommonUtil.fetchValidDate(inputDate);
    Assertions.assertEquals(inputDate.getTime(), result);
  }

  @Test
  void testFetchValidDateWithNullInput() {
    Long result = CommonUtil.fetchValidDate(null);
    Assertions.assertEquals(null, result);
  }
}

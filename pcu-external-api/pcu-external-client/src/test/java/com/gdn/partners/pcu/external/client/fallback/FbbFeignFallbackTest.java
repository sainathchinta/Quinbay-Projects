package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import com.blibli.oss.common.response.Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.fbb.core.constant.SortOrder;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

public class FbbFeignFallbackTest {

  private FbbFeignFallback fbbFeignFallback = new FbbFeignFallback();
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 100;
  private static final String SORT_BY = "sortBy";
  private static final String ITEM_SKU = "itemSku";
  private static final String BUSINESS_PARTNER_CODE = "business_partner_code";


  @Test
  public void partnerConsignmentFormsByFilterForProductTest(){
    Response<List<ConsignmentStatusResponse>> response =
      fbbFeignFallback.partnerConsignmentFormsByFilterForProduct(SORT_BY, SortOrder.ASC, PAGE, SIZE,
        ITEM_SKU, BUSINESS_PARTNER_CODE, "2023-10-01","2023-10-30", "111111");
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE,
      response.getErrors().keySet().stream().findFirst().get());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
      response.getErrors().values().stream().findFirst().get());
  }

  @Test
  public void countInProgressConsignmentFormsByItemSkusTest(){
    Response<List<CountConsignmentFormsByItemSkuResponse>> inProgressConsignmentForms =
      fbbFeignFallback.countInProgressConsignmentFormsByItemSkus(
        CountConsignmentFormsByItemSkusRequest.builder()
          .itemSkus(Collections.singletonList(ITEM_SKU)).businessPartnerCode(BUSINESS_PARTNER_CODE)
          .build(),"111111");
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE,
      inProgressConsignmentForms.getErrors().keySet().stream().findFirst().get());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
      inProgressConsignmentForms.getErrors().values().stream().findFirst().get());
  }

}
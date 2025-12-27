package com.gdn.mta.product.service.partners.pbp.distributiontask;

import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public interface PredefinedAttributeAllowedValueService {

  /**
   * Find Predefined Allowed Attribute Value by Id and value
   * @param requestId
   * @param listRequest
   * @param attributeId
   * @param value
   * @return
   * @throws Exception
   */
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(
      String requestId, GdnRestListRequest listRequest, String attributeId, String value)
      throws Exception;

  /**
   * find Predefined Allowed Attribute Value by matching attribute code and value
   * @param requestId
   * @param attributeCode
   * @param value
   * @return
   * @throws Exception
   */
  PredefinedAllowedAttributeValueResponse findByStoreIdAndMatchAttributeCodeAndValue(String requestId,
      String attributeCode, String value) throws Exception;

}

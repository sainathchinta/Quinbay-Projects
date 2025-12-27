package com.gdn.partners.pbp.dao;

import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public interface IPredefinedAllowedAttributeValueDao {

  /**
   * find Predefined Allowed Attribute Value by ID and
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
   * find Predefined Allowed Attribute Value by ID and Match Attribute Code
   *
   * @param requestId
   * @param attributeCode
   * @param value
   * @return
   * @throws Exception
   */
  GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndMatchAttributeCodeAndValue(
      String requestId, String attributeCode, String value)
      throws Exception;

}

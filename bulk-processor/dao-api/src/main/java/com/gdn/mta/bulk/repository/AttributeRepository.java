package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public interface AttributeRepository {

  AttributeResponse findOne(String storeId, String id) throws Exception;

  List<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndMarkForDeleteFalse(
      String requestId, GdnRestListRequest listRequest, String attributeId, String value)
      throws Exception;

  AttributeValueResponse addNewAttribute(String requestId, String value, String attributeCode) throws Exception;
}

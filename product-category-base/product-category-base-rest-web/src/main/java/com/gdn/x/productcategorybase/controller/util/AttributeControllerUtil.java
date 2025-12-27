package com.gdn.x.productcategorybase.controller.util;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.util.BeanUtils;

import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeDetailResponse;

public class AttributeControllerUtil {
  /**
   * Convert attributes of AttributeSummaryDTO into AttributeSummaryResponse 
   * @param result
   */
  public static List<AttributeDetailResponse> convertIntoAttributeSummaryResponse(
      List<AttributeSummaryDTO> result) {
    List<AttributeDetailResponse> response = new ArrayList<AttributeDetailResponse>();
    for (AttributeSummaryDTO item : result) {
      AttributeDetailResponse attributeDetailResponse = new AttributeDetailResponse();
      BeanUtils.copyProperties(item, attributeDetailResponse, "attributeType");
      attributeDetailResponse.setAttributeType(item.getAttributeType().name());
      response.add(attributeDetailResponse);
    }
    return response;
  }
}

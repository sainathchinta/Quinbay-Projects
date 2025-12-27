package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;

public interface DimensionMappingWrapperService {

  /**
   * update dimension mapping for an attribute
   *
   * @param storeId
   * @param attributeCode                 String
   * @param dimensionMappingUpdateRequest DimensionMappingUpdateRequest
   */
  void updateDimensionMapping(String storeId, String attributeCode,
    DimensionMappingUpdateRequest dimensionMappingUpdateRequest);
}

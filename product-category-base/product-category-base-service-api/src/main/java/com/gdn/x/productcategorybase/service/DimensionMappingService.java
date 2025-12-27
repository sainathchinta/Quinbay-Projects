package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface DimensionMappingService {

  /**
   * to fetch all dimensions mapped to given attribute code
   *
   * @param attributeCode
   * @param storeId
   * @param pageable
   * @return
   */
  Page<DimensionMappingResponse> fetchDimensionMapping(String attributeCode, String storeId,
      Pageable pageable);

  /**
   * to save dimension mapping
   *
   * @param dimensionMapping
   */
  void save(DimensionMapping dimensionMapping);

  /**
   * update dimension mapping for attribute
   *
   * @param storeId
   * @param attribute                     Attribute
   * @param dimensionMappingUpdateRequest DimensionMappingUpdateRequest
   */
  void updateDimensionMapping(String storeId, Attribute attribute,
    DimensionMappingUpdateRequest dimensionMappingUpdateRequest);

  /**
   * fetch dimension mapping for an attribute
   *
   * @param storeId       String
   * @param attributeCode String
   * @return List<DimensionMapping>
   */
  List<DimensionMapping> fetchDimensionMappingForAttribute(String storeId, String attributeCode);
}

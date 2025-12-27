package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.DimensionFilterRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionResponse;
import com.gdn.x.productcategorybase.entity.Dimension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface DimensionService {

  /**
   * Save new dimensions for size-chart
   *
   * @param request
   */
  void save(String storeId, DimensionRequest request);

  /**
   * to fetch dimension details
   *
   * @param storeId       storeId
   * @param dimensionCode dimensionCode
   * @return DimensionResponse
   */
  DimensionResponse fetchDimensionDetails(String storeId, String dimensionCode);

  /**
   * fetch list of dimensions based on filter
   * @param storeId
   * @param request
   * @return
   */
  Page<DimensionResponse> filter(String storeId, DimensionFilterRequest request, Pageable pageable);

  /**
   * edit dimension for size chart
   * @param storeId
   * @param dimensionRequest
   */
  void edit(String storeId, DimensionRequest dimensionRequest);

  /**
   * to find dimension
   *
   * @param storeId
   * @param id
   * @return
   */
  Dimension findById(String storeId, String id);

  /**
   * to fetch by dimension name
   * @param storeId
   * @param name
   * @return
   */
  DimensionResponse findByName(String storeId, String name);
}

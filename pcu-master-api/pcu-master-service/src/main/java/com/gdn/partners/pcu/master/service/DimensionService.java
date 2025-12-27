package com.gdn.partners.pcu.master.service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.web.model.request.DimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import com.gdn.partners.pcu.master.web.model.request.EditDimensionWebRequest;

public interface DimensionService {

  /**
   * fetch dimensions list based on filter
   *
   * @param dimensionFilterRequest dimensionFilterRequest
   * @param page                   page
   * @param size                   size
   * @return List<DimensionResponse>
   */
  GdnRestListResponse<DimensionResponse> fetchDimensionListing(DimensionFilterRequest dimensionFilterRequest,
    int page, int size);

  /**
   * fetch dimension detail by dimension code
   *
   * @param dimensionCode dimensionCode
   * @return DimensionResponse
   */
  DimensionResponse fetchDimensionDetail(String dimensionCode);

  /**
   * Create a new Dimension
   *
   * @param dimensionWebRequest
   */
  GdnBaseRestResponse save(DimensionWebRequest dimensionWebRequest);

  /**
   * to fetch dimension mapping for given attribute code
   *
   * @param attributeCode
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<DimensionMappingResponse> fetchDimensionMapping(String attributeCode,
      int page, int size);

  /**
   * to modify the dimension mapping
   *
   * @param attributeCode
   * @param modifyDimensionMappingWebRequest
   * @return GdnBaseRestResponse
   * @throws Exception
   */
  GdnBaseRestResponse modifyDimensionMapping(String attributeCode,
      ModifyDimensionMappingWebRequest modifyDimensionMappingWebRequest) throws Exception;

  /**
   * Edit existing Dimension
   *
   * @param editDimensionWebRequest
   * @return
   */
  GdnBaseRestResponse edit(String dimensionCode, EditDimensionWebRequest editDimensionWebRequest);

  /**
   * Validate dimension name
   *
   * @param dimensionName
   */
  GdnRestSingleResponse<DimensionResponse> validate(String dimensionName);
}

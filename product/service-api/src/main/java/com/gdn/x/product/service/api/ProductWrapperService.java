package com.gdn.x.product.service.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;

public interface ProductWrapperService {

  /**
   * Updates Product content and L5 details
   *
   * @param requestId  must not be null
   * @param updateCategory  boolean
   * @param productDetailPageEditRequest must not be null
   * @param productSku must not be empty
   * @param mandatoryRequestParam must not be null
   * @return CombinedEditItemResponse
   */
  CombinedEditItemResponse updateEditedProductCombined(String requestId, boolean updateCategory,
      ProductDetailPageEditRequest productDetailPageEditRequest, String productSku,
      MandatoryRequestParam mandatoryRequestParam) throws Exception;


  /**
   *
   * @param type
   * @param merchantCode
   * @param isActiveCounts
   * @return
   */
  ProductCountResponseVo getProductCountByType(String type, String merchantCode,
      boolean isActiveCounts);
}

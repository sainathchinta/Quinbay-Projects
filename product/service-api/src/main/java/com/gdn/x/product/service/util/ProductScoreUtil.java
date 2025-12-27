package com.gdn.x.product.service.util;

import java.util.List;

import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;

public interface ProductScoreUtil {

  /**
   * Get L3 score by productScoreRequest
   *
   * @param productScoreRequest
   * @return
   */
  ProductScoreVo getProductScoreByProductScoreRequest(ProductScoreRequest productScoreRequest) throws Exception;

  /**
   * Get recommanded attributes score
   *
   * @param attributeScoreRequests
   * @param categoryCode
   * @return
   */
  double getRecommendedAttributeScore(List<AttributeScoreRequest> attributeScoreRequests, String categoryCode)
      throws Exception;
}

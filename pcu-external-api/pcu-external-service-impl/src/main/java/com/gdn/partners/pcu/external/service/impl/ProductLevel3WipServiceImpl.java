package com.gdn.partners.pcu.external.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.service.ProductLevel3WipService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;

@Service
public class ProductLevel3WipServiceImpl implements ProductLevel3WipService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public ProductLevel3WipDetailWebResponse findProductDetailByProductSku(String productSku, boolean isActive) {
    GdnRestSingleResponse<ProductLevel3WipDetailResponse> response = pbpFeign.getProductDetailByProductSku(productSku, isActive);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductLevel3WipDetailWebResponse(response.getValue());
  }
}

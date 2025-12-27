package com.gdn.partners.pcu.external.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.GeneratorService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;

@Service
public class GeneratorServiceImpl implements GeneratorService {

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public String generateProductCode() {
    GdnRestSimpleResponse<String> response = pbpFeign.generateProductCode();
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public String generateBarCode() {
    GdnRestSimpleResponse<String> response = pbpFeign.generateBarCode();
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Double generateShippingWeight(String categoryCode, double length, double width, double height, double weight) {
    RequestHelper.validateGenerateShippingWeightRequest(length, width, height, weight);
    GdnRestSingleResponse<CategoryShippingWeightResponse> response =
        pcbFeign.generateShippingWeight(categoryCode, length, width, height, weight);
    ResponseHelper.validateResponse(response);
    return response.getValue().getShippingWeight();
  }
}

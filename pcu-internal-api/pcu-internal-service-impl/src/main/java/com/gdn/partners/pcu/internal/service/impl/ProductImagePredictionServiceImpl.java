package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.security.Credential;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.service.ProductImagePredictionService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductImagePredictionServiceImpl implements ProductImagePredictionService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public boolean update(ProductImagePredictionWebRequest productImagePredictionWebRequest)
      throws Exception {
    RequestHelper.checkUpdateImagePredictionAccessibility(Credential.getAccessibilities());
    GdnBaseRestResponse gdnBaseRestResponse = this.pbpFeign
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
    ResponseHelper.validateResponse(gdnBaseRestResponse);
    return gdnBaseRestResponse.isSuccess();
  }

  @Override
  public List<ProductImagePredictionWebResponse> getListOfPredictions() {
    GdnRestListResponse<ProductImagePredictionResponse> response = pbpFeign.getListOfPredictions();
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toListOfProductImagePredictionWebResponse(response.getContent());
  }

  @Override
  public boolean updateImagePredictionAndCategoryMapping(ProductImagePredictionAndCategoryMappingWebRequest request)
      throws Exception {
    RequestHelper.checkUpdateImagePredictionAccessibility(Credential.getAccessibilities());
    GdnBaseRestResponse gdnBaseRestResponse = this.pbpFeign.updateImagePredictionAndCategoryMapping(
        RequestHelper.toProductImagePredictionAndCategoryMappingRequest(request));
    ResponseHelper.validateResponse(gdnBaseRestResponse);
    return gdnBaseRestResponse.isSuccess();
  }

  @Override
  public List<ProductImagePredictionAndCategoryMappingWebResponse> getThresholdDetailAndCategoryMapping(
      PredictionTypeListWebRequest request) {
    GdnRestListResponse<ProductImagePredictionAndCategoryMappingResponse> gdnRestListResponse =
        this.pbpFeign.getImagePredictionAndCategoryMapping(RequestHelper.toGenericStringListRequest(request));
    ResponseHelper.validateResponse(gdnRestListResponse);
    return ResponseHelper.toProductImagePredictionAndCategoryMappingResponse(gdnRestListResponse.getContent());
  }
}

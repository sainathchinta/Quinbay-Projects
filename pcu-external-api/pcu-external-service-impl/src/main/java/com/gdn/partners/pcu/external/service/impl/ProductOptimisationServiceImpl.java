package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.external.service.ProductOptimisationService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationSuggestionResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ProductOptimisationServiceImpl implements ProductOptimisationService {

  private final ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public ProductCountsWebResponse getProductCount(String storeId, String requestId, String sellerCode) {
    GdnRestSingleResponse<ProductCountsWebResponse>
        response = productAnalyticsFeign.getProductCount(storeId, requestId, sellerCode);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Page<ProductOptimisationListResponse> fetchProductOptimisationList(String sellerCode,
      ProductOptimisationListWebRequest productOptimisationListWebRequest, int page, int size) {
    ProductOptimisationListRequest productOptimisationListRequest =
        ProductOptimisationListRequest.builder()
            .categoryCode(productOptimisationListWebRequest.getCategoryCode())
            .keyword(productOptimisationListWebRequest.getKeyword())
            .sellerCode(sellerCode).build();
    GdnRestListResponse<ProductOptimisationListResponse> response =
        productAnalyticsFeign.filter(page, size, productOptimisationListRequest);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void submitSuggestionFeedback(
      ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest) {
    GdnBaseRestResponse response =
        productAnalyticsFeign.provideSuggestionFeedback(productOptimisationFeedbackRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<ProductOptimisationSuggestionResponse> getSuggestionDetails(String productSku) {
    GdnRestListResponse<ProductOptimisationSuggestionResponse> response =
        productAnalyticsFeign.getSuggestionDetail(productSku);
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public void updateStatusForProductOptimisation(String requestId,
      ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest) {
    GdnBaseRestResponse response =
        productAnalyticsFeign.updateStatus(requestId, productOptimisationUpdateStatusRequest);
    ResponseHelper.validateResponse(response);
  }
}

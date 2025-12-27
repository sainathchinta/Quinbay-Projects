package com.gdn.partners.pcu.internal.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.factory.ProductAnalyticsFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;

import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "productAnalyticsFeign", url = "${service.productAnalytics.endpoint}",
  fallbackFactory = ProductAnalyticsFeignFallbackFactory.class)
public interface ProductAnalyticsFeign {

  @RequestMapping(value = "/api/auto-approved/products-list", method = RequestMethod.POST)
  GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedProductsList(
    @RequestParam(value = "page", defaultValue = "0") int page,
    @RequestParam(value = "size", defaultValue = "10") int size,
    @RequestBody AutoApprovedRequest request);

  @PostMapping(value = "/api/auto-approved/selected-download")
  GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedSelectedProductsList(
      @RequestBody AutoApprovedSelectedDownloadRequest request);

  @PostMapping(value = "/api/auto-approved/update-assignee")
  GdnRestListResponse<ProductAssigneeChangeResponse> updateAssignedTo(
      @RequestBody AutoApprovedAssigneeRequest request);

  @PostMapping(value = "/api/auto-approved/{productCode}/upsert-user-feedback")
  GdnBaseRestResponse updateUserFeedback(@PathVariable("productCode") String productCode,
      @RequestBody AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest);

  @GetMapping(value = "/api/auto-approved/{productCode}/user-feedback-for-auto-approved-products")
  GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> fetchUserFeedback(@PathVariable("productCode") String productCode);

  @PostMapping(value = "/api/product-attribute-extractions/publish-by-product-sku")
  GdnBaseRestResponse publishEventsByProductSku(@RequestParam String storeId,
      @RequestBody ProductAttributeExtractionsRequest productAttributeExtractionsRequest);
}

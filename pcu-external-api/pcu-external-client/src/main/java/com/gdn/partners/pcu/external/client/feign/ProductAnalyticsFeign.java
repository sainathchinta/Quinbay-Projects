package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.ProductAnalyticsFeignFallbackFactory;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationSuggestionResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "productAnalyticsFeign", url = "${service.product.analytics.endpoint}",
             fallbackFactory = ProductAnalyticsFeignFallbackFactory.class)
public interface ProductAnalyticsFeign {
  @RequestMapping(value = "/api/product-optimisation/product-count", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCountsWebResponse> getProductCount(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("sellerCode") String sellerCode);

  @RequestMapping(value = "/api/product-optimisation/filter", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductOptimisationListResponse> filter(@RequestParam("page") int page,
      @RequestParam("size") int size,
      @RequestBody ProductOptimisationListRequest productOptimisationListRequest);

  @RequestMapping(value = "/api/product-optimisation/feedback", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse provideSuggestionFeedback(
      @RequestBody ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest);

  @RequestMapping(value = "/api/product-optimisation/suggestion-detail", method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductOptimisationSuggestionResponse> getSuggestionDetail(
      @RequestParam String productSku);

  @RequestMapping(value = "/api/product-optimisation/update-status", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateStatus(@RequestParam String requestId,
      @RequestBody ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest);
}

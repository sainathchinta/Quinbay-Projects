package com.gdn.partners.pcu.internal.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ProductCenterApiPath;
import com.gdn.partners.pcu.internal.service.ProductCenterService;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterListingActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Product Center API")
@RestController
@RequestMapping(value = ProductCenterApiPath.BASE_PATH)
@Validated
public class ProductCenterController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private ProductCenterService productCenterService;

  @Operation(summary = "API for product center page listing")
  @PostMapping(value = ProductCenterApiPath.PRODUCT_CENTER_SUMMARY_FILTER, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductCenterSummaryWebResponse> getProductCenterSummary(
      @RequestParam("storeId") String storeId, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "100") Integer size,
      @RequestBody ProductCenterSummaryWebRequest productCenterSummaryWebRequest) throws Exception {
    log.info("invoking controller method to get product-center listing for request : {}",
        productCenterSummaryWebRequest);
    String requestId = clientParameterHelper.getRequestId();
    Page<ProductCenterSummaryWebResponse> response = productCenterService
        .getProductCenterFilterSummary(storeId, requestId, productCenterSummaryWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "API for updating from product center page listing")
  @PutMapping(value = ProductCenterApiPath.PRODUCT_CENTER_LISTING_ACTION, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse productCenterAction(@RequestParam("storeId") String storeId,
      @RequestBody ProductCenterListingActionWebRequest productCenterListingActionWebRequest) throws Exception {
    log.info("invoking controller method to update product-center listing for request : {}",
        productCenterListingActionWebRequest);
    String requestId = clientParameterHelper.getRequestId();
    productCenterService.updateProductCenterListing(storeId, requestId, productCenterListingActionWebRequest);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API for downloading un-mapped skus")
  @GetMapping(value = ProductCenterApiPath.PRODUCT_CENTER_DOWNLOAD_UNMAPPED_SKUS, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadUnmappedSkus(@RequestParam("storeId") String storeId,
      @RequestParam("language") String language, @PathVariable("category-code") String parentCategoryCode) throws Exception {
    log.info("invoking controller method to download unmapped skus for category : {}", parentCategoryCode);
    String requestId = clientParameterHelper.getRequestId();
    String username = clientParameterHelper.getUsername();
    productCenterService.downloadUnmappedSkus(storeId, requestId, username, parentCategoryCode, language);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API to fetch product center history by product sku")
  @GetMapping(value = ProductCenterApiPath.PRODUCT_CENTER_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductCenterHistoryWebResponse> getProductCenterHistoryByProductSku(
      @PathVariable("productSku") String productSku, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    log.info("Fetch product center history for product : {}", productSku);
    String requestId = clientParameterHelper.getRequestId();
    Page<ProductCenterHistoryWebResponse> productCenterHistoryWebResponsePage =
        this.productCenterService.getProductCenterHistoryByProductSku(productSku, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, productCenterHistoryWebResponsePage.getContent(),
        new Metadata(page, size, productCenterHistoryWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "API for get details of product from product center listing")
  @GetMapping(value = ProductCenterApiPath.PRODUCT_CENTER_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductCenterDetailWebResponse> productCenterDetail(@RequestParam("storeId") String storeId,
      @PathVariable("productSku") String productSku) throws Exception {
    log.info("invoking controller method to get product details : {}", productSku);
    String requestId = clientParameterHelper.getRequestId();
    return new SingleBaseResponse<>(null, null, true, requestId,
        productCenterService.getProductCenterDetail(storeId, requestId, productSku));
  }

  @Operation(summary = "API for update product from product center detail")
  @PutMapping(value = ProductCenterApiPath.PRODUCT_CENTER_DETAIL_UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse productCenterDetailUpdate(@RequestParam("storeId") String storeId,
      @PathVariable("productSku") String productSku, @RequestBody SalesCategoryMappingWebRequest request)
      throws Exception {
    log.info("invoking controller method to update product sales category : {}", productSku);
    String requestId = clientParameterHelper.getRequestId();
    this.productCenterService.updateProductCenterDetail(storeId, requestId, productSku, request);
    return new BaseResponse(null, null, true, requestId);
  }
}

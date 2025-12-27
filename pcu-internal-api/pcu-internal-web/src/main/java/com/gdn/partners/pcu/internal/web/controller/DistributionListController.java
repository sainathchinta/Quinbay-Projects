package com.gdn.partners.pcu.internal.web.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.DistributionListControllerPath;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.DistributionListService;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Distribution page API")
@RestController
@RequestMapping(value = DistributionListControllerPath.BASE_PATH)
@Validated
public class DistributionListController {

  @Autowired
  private DistributionListService distributionListService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "API to get distribution page listing")
  @PostMapping(value = DistributionListControllerPath.PRODUCT_LIST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<DistributionProductWebResponse> getDistributionProductList(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody DistributionFilterWebRequest distributionFilterWebRequest) {
    log.info("invoking controller method to get distribution page listing for request : {}", distributionFilterWebRequest);
    Page<DistributionProductWebResponse> response =
        distributionListService.getSummaryByMultipleFilter(page, size, distributionFilterWebRequest);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "API to save product vendor mapping")
  @PostMapping(value = DistributionListControllerPath.PRODUCT_VENDOR_MAPPING, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse saveProductVendorMapping(@RequestBody ProductVendorWebRequest productVendorWebRequest) {
    GdnPreconditions
        .checkArgument(!productVendorWebRequest.getProductCodes().isEmpty(), ErrorMessages.PRODUCT_CODE_LIST_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productVendorWebRequest.getVendorCode()),
        ErrorMessages.ERR_VENDOR_CODE_EMPTY);
    log.info("invoking controller method to save product vendor mapping for request : {}", productVendorWebRequest);
    distributionListService.saveProductVendorMapping(productVendorWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }
}

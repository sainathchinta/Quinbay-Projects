package com.gdn.partners.pbp.controller.productlevel3;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.service.ProductLevel3WipWrapper;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ForceReviewImageViolationResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.web.model.ProductLevel3WipControllerErrorMessage;
import com.gdn.partners.pbp.web.model.ProductLevel3WipControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = ProductLevel3WipControllerPath.BASE_PATH)
@Tag(name = "Product Level 3 Wip", description = "Product Level 3 Wip Service API")
public class ProductLevel3WipController {

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductLevel3WipWrapper productLevel3WipWrapper;
  
  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;
  
  @RequestMapping(value = ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_STATE, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter summary", description = "filter summary")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3WipResponse> filterSummaryWithState(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody ProductLevel3WipSummaryRequest request)
      throws Exception {
    GdnPreconditions.checkArgument(request.getCriteria() != null,
        ProductLevel3WipControllerErrorMessage.CRITERIA_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBusinessPartnerCode()),
        ProductLevel3WipControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    Page<ProductLevel3WipResponse> productLevel3WipResponses = this.findSummaryByFilterWithState(request, page, size);
    return new GdnRestListResponse<ProductLevel3WipResponse>(productLevel3WipResponses.getContent(), new PageMetaData(
        size, page, productLevel3WipResponses.getTotalElements()), requestId);
  }
  
  @RequestMapping(value = ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_STATE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count summary state", description = "count summary with state")
  @ResponseBody
  public GdnRestSingleResponse<CountProductLevel3WipResponse> countSummaryWithState(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode) throws Exception {
    CountProductLevel3WipResponse countProductLevel3WipResponse = this.countSummaryWithState(businessPartnerCode);
    return new GdnRestSingleResponse<CountProductLevel3WipResponse>(countProductLevel3WipResponse, requestId);
  }

  @RequestMapping(value = ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_FILTER_STATE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count summary by filter type", description = "count summary by filter type")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3CountResponse> countSummaryWithFilterState(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode,
      @RequestParam(defaultValue = "SECONDARY") String type) throws Exception {
    ProductLevel3CountResponse productLevel3CountResponse =
        productLevel3WipWrapper.countSummaryByFilterType(businessPartnerCode, storeId, type);
    return new GdnRestSingleResponse<>(productLevel3CountResponse, requestId);
  }

  private ProductLevel3WipResponse generateProductLevel3WipResponse(ProductLevel3Wip productLevel3Wip) throws Exception {
    ProductLevel3WipResponse productLevel3WipResponse = null;
    productLevel3WipResponse = new ProductLevel3WipResponse();
    BeanUtils.copyProperties(productLevel3Wip, productLevel3WipResponse);
    return productLevel3WipResponse;
  }

  private List<ProductLevel3WipResponse> generateProductLevel3WipResponses(Page<ProductLevel3Wip> productLevel3Wips)
      throws Exception {
    List<ProductLevel3WipResponse> productLevel3WipResponses = new ArrayList<ProductLevel3WipResponse>();
    for (ProductLevel3Wip productLevel3Wip : productLevel3Wips.getContent()) {
      ProductLevel3WipResponse productLevel3WipResponse = this.generateProductLevel3WipResponse(productLevel3Wip);
      productLevel3WipResponses.add(productLevel3WipResponse);
    }
    return productLevel3WipResponses;
  }

  private List<ProductLevel3WipResponse> generateProductLevel3WipDTOResponses(
      List<ProductLevel3WipDTO> productLevel3WipsDto) throws Exception {
    List<ProductLevel3WipResponse> productLevel3WipResponses = new ArrayList<ProductLevel3WipResponse>();
    for (ProductLevel3WipDTO productLevel3Wip : productLevel3WipsDto) {
      ProductLevel3WipResponse productLevel3WipResponse = ConverterUtil.generateProductLevel3WipDTOResponse(productLevel3Wip);
      productLevel3WipResponses.add(productLevel3WipResponse);
    }
    return productLevel3WipResponses;
  }

  private CountProductLevel3WipResponse generateCountProductLevel3WipResponse(
      CountProductLevel3Wip countProductLevel3Wip) throws Exception {
    CountProductLevel3WipResponse countProductLevel3WipResponse = null;
    if (countProductLevel3Wip != null) {
      countProductLevel3WipResponse = new CountProductLevel3WipResponse();
      BeanUtils.copyProperties(countProductLevel3Wip, countProductLevel3WipResponse,
          "totalItemsByCriterias");
      for (Entry<ProductLevel3WipSummaryCriteria, Long> entry : countProductLevel3Wip.getTotalItemsByCriterias()
          .entrySet()) {
        countProductLevel3WipResponse.getTotalItemsByCriterias().put(entry.getKey(), entry.getValue());
      }
    }
    return countProductLevel3WipResponse;
  }

  private Page<ProductLevel3WipResponse> findSummaryByFilterWithState(ProductLevel3WipSummaryRequest request,
      Integer page, Integer size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductLevel3WipDTO> productLevel3Wips = this.productLevel3WipService
        .findSummaryByFilterWithState(request, pageable);
    return new PageImpl<>(this.generateProductLevel3WipDTOResponses(productLevel3Wips.getContent()), pageable,
        productLevel3Wips.getTotalElements());
  }
  
  private CountProductLevel3WipResponse countSummaryWithState(String businessPartnerCode) throws Exception {
      CountProductLevel3Wip countProductLevel3Wip =
        this.productLevel3WipWrapper.countSummaryWithState(businessPartnerCode);
      return this.generateCountProductLevel3WipResponse(countProductLevel3Wip);
  }


  @RequestMapping(value = ProductLevel3WipControllerPath.DETAIL_FILTER_PRODUCT_SKU, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 detail by product sku", description = "filter product level 3 detail by "
      + "product sku at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3WipDetailResponse> filterDetailByProductSKu(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productSku, @RequestParam(required = false) boolean isActive)
      throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productSku),
        ProductLevel3WipControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    ProductLevel3WipDetailResponse detail =
        this.productLevel3WipWrapper.findByProductSku(storeId, productSku, isActive);
    GdnRestSingleResponse<ProductLevel3WipDetailResponse> response =
        new GdnRestSingleResponse<>(detail, requestId);
    return response;
  }

  @RequestMapping(value = ProductLevel3WipControllerPath
      .FILTER_SUMMARY_WITH_EXPECTATION_ACTIVATION_DATE, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "filter summary", description = "filter summary")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3WipResponse> filterSummaryWithExpectationActivationDate(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ProductLevel3WipControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    List<ProductLevel3WipDTO> productLevel3WipDtos = productLevel3WipService
        .findProductWipByExpectationActivationDateGreater(storeId, businessPartnerCode,
            Calendar.getInstance().getTime());
    return new GdnRestListResponse<ProductLevel3WipResponse>(
        this.generateProductLevel3WipDTOResponses(productLevel3WipDtos),
        new PageMetaData(10, 0, productLevel3WipDtos.size()), requestId);
  }

  @RequestMapping(value = ProductLevel3WipControllerPath.SEND_MAIL_FOR_EXCEEDED_ACTIVATION, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "send email for exceeded activation", description = "send email for exceeded activation")
  @ResponseBody
  public GdnBaseRestResponse sendEmailForExceededActivation(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ProductLevel3WipControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    log.info("Sending email for exceeded activation of products for business partner {}", businessPartnerCode);
    productLevel3WipService.sendMailForEmailExceededActivation(businessPartnerCode, username);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}

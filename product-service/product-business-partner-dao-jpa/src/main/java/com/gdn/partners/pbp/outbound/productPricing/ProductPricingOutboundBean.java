package com.gdn.partners.pbp.outbound.productPricing;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.promo.request.FreeSampleParticipationResponse;
import com.gdn.partners.pbp.outbound.productPricing.feign.ProductPricingFeign;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingType;
import com.gdn.partners.product.pricing.web.model.dto.FailedItemReasonDto;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.pbp.dto.promo.request.PromoBundlingPricingSummaryRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSummaryResponse;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.product.enums.SolrConstants;
import com.google.common.collect.ImmutableMap;
import lombok.extern.slf4j.Slf4j;

/**
 * @author nitinmathew - created on 06/02/2020
 **/
@Component
@Slf4j
public class ProductPricingOutboundBean implements ProductPricingOutbound {

  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 1;

  private static final String START_DATE = "startDate";

  @Autowired
  private ProductPricingFeign productPricingFeign;

  @Value("${free.sample.check.new.flow}")
  private boolean freeSampleCheckNewFlow;

  @Override
  public WholesalePriceBulkUpdateResponse upsertWholesalePrice(WholesalePriceRequest wholesalePriceRequest) throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    wholesalePriceBulkUpdateRequest.setProductSku(wholesalePriceRequest.getProductSku());
    wholesalePriceBulkUpdateRequest.setNewProduct(wholesalePriceRequest.isNewProduct());
    wholesalePriceBulkUpdateRequest.setWholesalePriceSkuRequests(wholesalePriceRequest.getWholesalePriceSkuRequests());
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response;
      response = productPricingFeign.bulkUpdateV2(param.getStoreId(), param.getChannelId(),
          param.getClientId(), param.getRequestId(), param.getUsername(), wholesalePriceRequest.getMerchantCode(),
          Arrays.asList(wholesalePriceBulkUpdateRequest));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public WholesalePriceSkuResponse getWholesalePrice(String itemSku, String pickupPointCode) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    ItemInfoDto itemInfoDto = getItemInfoDto(itemSku, pickupPointCode);
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
        getWholesalePriceSkuDetailListRequest(Collections.singletonList(itemInfoDto));
      GdnRestListResponse<WholesalePriceSkuResponse> wholesalePriceListDetail = productPricingFeign
          .getWholesalePriceListDetailV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
              param.getRequestId(), param.getUsername(), wholesalePriceSkuDetailListRequest);
      if(!wholesalePriceListDetail.isSuccess() || wholesalePriceListDetail.getContent().isEmpty()) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, wholesalePriceListDetail.getErrorMessage());
      }
      return wholesalePriceListDetail.getContent().get(0);
  }

  private ItemInfoDto getItemInfoDto(String itemSku, String pickupPointCode) {
    ItemInfoDto itemInfoDto = new ItemInfoDto();
    itemInfoDto.setItemSku(itemSku);
    itemInfoDto.setPickupPointCode(pickupPointCode);
    itemInfoDto.setItemPickupPointId(CommonUtils.getItemSkuAndPickupPointKey(itemSku, pickupPointCode));
    return itemInfoDto;
  }

  @Override
  public boolean setWholesaleActivated(String itemSku, String updateSkuStatus, String pickupPointCode) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
      BulkActivateDeactivateRequest bulkActivateDeactivateRequest = new BulkActivateDeactivateRequest();
      bulkActivateDeactivateRequest.setItemSku(itemSku);
      bulkActivateDeactivateRequest.setPickUpPointCode(pickupPointCode);
      bulkActivateDeactivateRequest.setUpdateSkuStatus(updateSkuStatus);
      GdnRestSingleResponse<BulkActivateDeactivateResponse> bulkActivateDeactivateResponseResponse = productPricingFeign
          .bulkActivateOrDeactivateSkuV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
              param.getRequestId(), param.getUsername(), Collections.singletonList(bulkActivateDeactivateRequest));
      boolean success = bulkActivateDeactivateResponseResponse.isSuccess() && (
          Objects.isNull(bulkActivateDeactivateResponseResponse.getValue()) || CollectionUtils
              .isEmpty(bulkActivateDeactivateResponseResponse.getValue().getFailedItemReason()));
      if (!success) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
            bulkActivateDeactivateResponseResponse.getErrorMessage());
      }
      return success;
  }

  @Override
  public List<WholesalePriceSkuResponse> getWholesalePriceList(Set<String> itemSkus,
      Map<String, String> itemSkuAndPickupPointCodeMap) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestListResponse<WholesalePriceSkuResponse> response;
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest = new WholesalePriceSkuDetailListRequest();
    wholesalePriceSkuDetailListRequest.setItemSkus(itemSkus);
      List<ItemInfoDto> itemInfoDtoList =
          itemSkus.stream().map(itemSKu -> getItemInfoDto(itemSKu, itemSkuAndPickupPointCodeMap.get(itemSKu)))
              .collect(Collectors.toList());
      wholesalePriceSkuDetailListRequest.setItemInfo(itemInfoDtoList);
      response = productPricingFeign
          .getWholesalePriceListDetailV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
              param.getRequestId(), param.getUsername(), wholesalePriceSkuDetailListRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public BulkActivateDeactivateResponse bulkActivateOrDeactivateSku(
          List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequestList) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response;
    response = productPricingFeign
            .bulkActivateOrDeactivateSkuV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
                    param.getRequestId(), param.getUsername(), bulkActivateDeactivateRequestList);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    setWholeSaleFailedMapResponse(response);
    return response.getValue();
  }

  private void setWholeSaleFailedMapResponse(GdnRestSingleResponse<BulkActivateDeactivateResponse> response) {
    if (Objects.nonNull(response.getValue()) && CollectionUtils.isNotEmpty(response.getValue().getFailedItemReason())
        && MapUtils.isEmpty(response.getValue().getFailedItemSkuToFailedReasonMap())) {
      Map<String, String> failedItemSkuToFailedReasonMap = response.getValue().getFailedItemReason().stream().collect(
          Collectors.toMap(FailedItemReasonDto::getItemSku, FailedItemReasonDto::getFailedReason, (a, b) -> a));
      response.getValue().setFailedItemSkuToFailedReasonMap(failedItemSkuToFailedReasonMap);
    }
  }

  @Override
  public WholesalePriceBulkUpdateResponse bulkUpdateWholesalePrice(
          List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList, String merchantCode) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response;
    response = productPricingFeign
            .bulkUpdateV2(param.getStoreId(), param.getChannelId(), param.getClientId(), param.getRequestId(),
                    param.getUsername(), merchantCode, wholesalePriceBulkUpdateRequestList);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    setWholePriceMapResponse(response);
    return response.getValue();
  }

  private void setWholePriceMapResponse(GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response) {
    if (Objects.nonNull(response.getValue()) && CollectionUtils
        .isNotEmpty(response.getValue().getWholesalePriceSkuStatus()) && MapUtils
        .isEmpty(response.getValue().getWholesalePriceSkuStatusMap())) {
      Map<String, String> wholesalePriceSkuStatusMap = new HashMap<>();
      for (WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto : response.getValue().getWholesalePriceSkuStatus()) {
        wholesalePriceSkuStatusMap
            .putIfAbsent(wholeSalePriceSkuStatusDto.getItemSku(), wholeSalePriceSkuStatusDto.getSkuStatus());
      }
      response.getValue().setWholesalePriceSkuStatusMap(wholesalePriceSkuStatusMap);
    }
    if (Objects.nonNull(response.getValue()) && CollectionUtils.isNotEmpty(response.getValue().getFailedItemReason())
        && MapUtils.isEmpty(response.getValue().getFailedItemSkuToFailedReasonMap())) {
      Map<String, String> failedItemSkuToFailedReasonMap = response.getValue().getFailedItemReason().stream().collect(
          Collectors.toMap(FailedItemReasonDto::getItemSku, FailedItemReasonDto::getFailedReason, (a, b) -> a));
      response.getValue().setFailedItemSkuToFailedReasonMap(failedItemSkuToFailedReasonMap);
    }
  }

  @Override
  public WholesalePriceBulkUpdateResponse bulkUpdateWholesalePriceV2(
      List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList, String merchantCode,
      String username) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        productPricingFeign.bulkUpdateV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), merchantCode, wholesalePriceBulkUpdateRequestList);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<WholesalePriceSkuResponse> getWholesalePriceByItemSkuAndPickupPointCode(
    List<ItemInfoDto> itemInfoDtoList) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
        getWholesalePriceSkuDetailListRequest(itemInfoDtoList);
    GdnRestListResponse<WholesalePriceSkuResponse> response;
      response = productPricingFeign
          .getWholesalePriceListDetailV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
              param.getRequestId(), param.getUsername(), wholesalePriceSkuDetailListRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  private WholesalePriceSkuDetailListRequest getWholesalePriceSkuDetailListRequest(List<ItemInfoDto> itemInfoDtoList) {
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
      new WholesalePriceSkuDetailListRequest();
    wholesalePriceSkuDetailListRequest.setItemInfo(itemInfoDtoList);
    wholesalePriceSkuDetailListRequest.setItemSkus(
      itemInfoDtoList.stream().map(ItemInfoDto::getItemSku).collect(Collectors.toSet()));
    return wholesalePriceSkuDetailListRequest;
  }

  @Override
  public List<PromoBundlingSummaryResponse> getPromoBundlingSummaryResponse (
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest) throws Exception {
    GdnRestListResponse <PromoBundlingSummaryResponse> response;
    MandatoryRequestParam param = this.generateMandatoryRequestParam();
    if (freeSampleCheckNewFlow) {
      return freeSampleParticipationCheck(promoBundlingPricingSummaryRequest, param);
    } else {
      response =
          productPricingFeign.filterPromoBundlingV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
              param.getRequestId(), param.getUsername(), PAGE_NUMBER, PAGE_SIZE, START_DATE, SolrConstants.ASC,
              promoBundlingPricingSummaryRequest);
    }
    if (!response.isSuccess()) {
      log.error("filtering Promo Bundling failed with error {} for request : {}",
        response.getErrorMessage(), promoBundlingPricingSummaryRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  private List<PromoBundlingSummaryResponse> freeSampleParticipationCheck(
      PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest, MandatoryRequestParam param) {
    GdnRestSingleResponse<FreeSampleParticipationResponse> singleResponse =
        productPricingFeign.freeSampleParticipationCheck(param.getStoreId(), param.getChannelId(),
            param.getClientId(), param.getRequestId(), param.getUsername(),
            promoBundlingPricingSummaryRequest.getProductSku());
    if (!singleResponse.isSuccess() || Objects.isNull(singleResponse.getValue())) {
      log.error("filtering Promo Bundling failed with error {} for request : {}", singleResponse.getErrorMessage(),
          promoBundlingPricingSummaryRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, singleResponse.getErrorMessage());
    }
    PromoBundlingSummaryResponse promoBundlingSummaryResponse = getPromoBundlingSummaryResponse(singleResponse);
    return Collections.singletonList(promoBundlingSummaryResponse);
  }

  private static PromoBundlingSummaryResponse getPromoBundlingSummaryResponse(
      GdnRestSingleResponse<FreeSampleParticipationResponse> singleResponse) {
    PromoBundlingSummaryResponse promoBundlingSummaryResponse = new PromoBundlingSummaryResponse();
    promoBundlingSummaryResponse.setPromoBundlingType(
        singleResponse.getValue().isFreeSampleAvailable() ? PromoBundlingType.FREE_SAMPLE.name() : StringUtils.EMPTY);
    return promoBundlingSummaryResponse;
  }

  @Override
  public List<WholesalePriceSkuResponse> getWholesalePriceListV2(WholesalePriceSkuDetailListRequest
      wholesalePriceSkuDetailListRequest) throws Exception {
    MandatoryRequestParam param = generateMandatoryRequestParam();
    GdnRestListResponse<WholesalePriceSkuResponse> response = productPricingFeign.getWholesalePriceListDetailV2(param.getStoreId(),
        param.getChannelId(), param.getClientId(), param.getRequestId(), param.getUsername(), wholesalePriceSkuDetailListRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  private MandatoryRequestParam generateMandatoryRequestParam() throws Exception {
    return MandatoryRequestParam.generateMandatoryRequestParam(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  @Override
  public boolean setWholesaleActivatedFlag(String itemSku, String pickupPointCode, String updateSkuStatus) throws Exception {
    BulkActivateDeactivateRequest request = new BulkActivateDeactivateRequest();
    request.setItemSku(itemSku);
    request.setPickUpPointCode(pickupPointCode);
    request.setUpdateSkuStatus(updateSkuStatus);
    BulkActivateDeactivateResponse response = bulkActivateOrDeactivateSku(Collections.singletonList(request));
    if (CollectionUtils.isNotEmpty(response.getFailedItemReason())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getFailedItemReason().get(0).getFailedReason());
    }
    return true;
  }

}

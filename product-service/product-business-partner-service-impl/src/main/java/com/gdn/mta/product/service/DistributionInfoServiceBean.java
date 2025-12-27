package com.gdn.mta.product.service;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.AuditTrailListByProductCodeRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.UomType;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.warehouse.OMSOutBound;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class DistributionInfoServiceBean implements DistributionInfoService {

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private OMSOutBound omsOutBound;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${validate.uom.info}")
  private boolean validateUomInfo;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Override
  public void validateAndUpdateDistributionInfo(String productCode, DistributionInfoRequest distributionInfoRequest,
      String username) throws Exception {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    boolean dataUpdated = validateUomInfo(productCode, distributionInfoRequest, auditTrailDtoList);
    if (dataUpdated) {
      productOutbound.updateDistributionInfo(productCode,
          ConverterUtil.getDistributionInfoUpdateRequest(distributionInfoRequest));
      publishProductLevelHistoryToPcbForDistributionUpdate(productCode, distributionInfoRequest.getSellerCode(),
          auditTrailDtoList, username);
    }
  }

  @Override
  public void publishProductLevelHistoryToPcbForDistributionUpdate(String productCode, String businessPartnerCode,
      List<AuditTrailDto> auditTrailDtoList, String username) {
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      AuditTrailListByProductCodeRequest auditTrailListByProductCodeRequest = new AuditTrailListByProductCodeRequest();
      auditTrailListByProductCodeRequest.setProductCode(productCode);
      auditTrailListByProductCodeRequest.setBusinessPartnerCode(businessPartnerCode);
      auditTrailListByProductCodeRequest.setAuditTrailResponseList(auditTrailDtoList);
      auditTrailListByProductCodeRequest.setChangedBy(username);
      kafkaProducer.send(kafkaTopicProperties.getPopulateL3HistoryByProductCodeEvent(), productCode,
          auditTrailListByProductCodeRequest);
    }
  }

  @Override
  public boolean validateUomInfo(String productCode, DistributionInfoRequest distributionInfoRequest,
      List<AuditTrailDto> auditTrailDtoList) throws Exception {
    if (validateUomInfo) {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(distributionInfoRequest.getSellerCode()),
          ValidationErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(ranchIntegrationEnabled, ValidationErrorMessage.ACTION_NOT_ALLOWED.getMessage());
      GdnPreconditions.checkArgument(distributionSellerList.contains(distributionInfoRequest.getSellerCode()),
          ValidationErrorMessage.ACTION_NOT_ALLOWED.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode),
          ValidationErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(
          CollectionUtils.isNotEmpty(distributionInfoRequest.getProductItems()),
          ValidationErrorMessage.PRODUCT_ITEM_UOM_INFO_REQUESTS_MUST_NOT_BE_EMPTY.getMessage());
      List<DistributionInfoPerSkuResponse> existingDistributionInfo = productOutbound.getDistributionInfo(productCode);

      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo = new HashMap<>();
      for (DistributionInfoPerSkuResponse distributionInfoPerSkuResponse : existingDistributionInfo) {
        skuCodeToExistingDistributionInfo.put(distributionInfoPerSkuResponse.getSkuCode(),
            distributionInfoPerSkuResponse);
      }

      List<String> omniChannelSkusInRequest = new ArrayList<>();
      for (ProductItemDistributionInfoRequest productItemDistributionInfoRequest : distributionInfoRequest.getProductItems()) {
        String omniChannelSku = productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku();
        GdnPreconditions.checkArgument(StringUtils.isNotBlank(omniChannelSku),
            ValidationErrorMessage.OMNI_CHANNEL_SKU_CANNOT_BE_EMPTY.getMessage());
        omniChannelSkusInRequest.add(omniChannelSku);
      }

      if (CollectionUtils.isNotEmpty(existingDistributionInfo) && hasDistributionInfoNotUpdated(
          existingDistributionInfo, skuCodeToExistingDistributionInfo, distributionInfoRequest, auditTrailDtoList)) {
        log.info("Discard update distribution-info as there are no updates , request: {}", distributionInfoRequest);
        return false;
      }

      Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap =
          productOutbound.getOmniChannelSkuToItemCode(distributionInfoRequest.getSellerCode(), omniChannelSkusInRequest);
      validateDistributionInfoUpdateRequest(distributionInfoRequest, skuCodeToExistingDistributionInfo,
          omniChannelSkuToResponseMap);
    }
    log.info("Validation passed for uomInfo for productCode : {} ", productCode);
    return true;
  }

  private boolean hasDistributionInfoNotUpdated(List<DistributionInfoPerSkuResponse> existingDistributionInfoList,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo,
      DistributionInfoRequest distributionInfoRequest, List<AuditTrailDto> auditTrailDtoList) {
    AtomicBoolean dataNotUpdated = new AtomicBoolean(true);
    hasProductDistributionInfoNotUpdated(existingDistributionInfoList.getFirst().getDistributionInfoResponse(),
        distributionInfoRequest, dataNotUpdated, auditTrailDtoList);
    if (distributionInfoRequest.getProductItems().size() != skuCodeToExistingDistributionInfo.size()) {
      dataNotUpdated.set(false);
    }
    hasProductItemDistributionInfoListNotUpdated(distributionInfoRequest, skuCodeToExistingDistributionInfo,
        dataNotUpdated, auditTrailDtoList);
    return dataNotUpdated.get();
  }

  private void hasProductDistributionInfoNotUpdated(DistributionInfoResponse distributionInfoResponse,
      DistributionInfoRequest distributionInfoRequest, AtomicBoolean notUpdated,
      List<AuditTrailDto> auditTrailDtoList) {
    if (Objects.nonNull(distributionInfoResponse)) {
      if (!distributionInfoResponse.getProductName()
          .equals(distributionInfoRequest.getDistributionInfoRequest().get(Constants.PRODUCT_NAME_KEY))) {
        notUpdated.set(false);
        auditTrailDtoList.add(CommonUtils.getAuditTrailDto(SaveHistoryConstants.DISTRIBUTION_PRODUCT_NAME_UPDATED,
            distributionInfoResponse.getProductName(),
            distributionInfoRequest.getDistributionInfoRequest().get(Constants.PRODUCT_NAME_KEY), Constants.HYPHEN));
      }
      if (!distributionInfoResponse.getCategoryName()
          .equals(distributionInfoRequest.getDistributionInfoRequest().get(Constants.CATEGORY_NAME_KEY))) {
        notUpdated.set(false);
        auditTrailDtoList.add(CommonUtils.getAuditTrailDto(SaveHistoryConstants.DISTRIBUTION_CATEGORY_NAME_UPDATED,
            distributionInfoResponse.getCategoryName(),
            distributionInfoRequest.getDistributionInfoRequest().get(Constants.CATEGORY_NAME_KEY), Constants.HYPHEN));
      }
    }
  }

  private void hasProductItemDistributionInfoListNotUpdated(DistributionInfoRequest distributionInfoRequest,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo, AtomicBoolean dataNotUpdated,
      List<AuditTrailDto> auditTrailDtoList) {
    for (ProductItemDistributionInfoRequest productItemDistributionInfoRequest : distributionInfoRequest.getProductItems()) {
      DistributionInfoPerSkuResponse existingDistributionInfo =
          skuCodeToExistingDistributionInfo.get(productItemDistributionInfoRequest.getSkuCode());
      if (Objects.isNull(existingDistributionInfo)) {
        continue;
      }
      hasProductItemDistributionInfoNotUpdated(existingDistributionInfo, productItemDistributionInfoRequest,
          dataNotUpdated, auditTrailDtoList);
      Map<String, DimensionsAndUomResponse> uomCodeToDimensionsAndUomResponse =
          existingDistributionInfo.getDimensionsAndUomResponse().stream().collect(
              Collectors.toMap(DimensionsAndUomResponse::getUomCode, Function.identity(),
                  (existing, current) -> existing));
      Map<String, DimensionAndUomRequest> dimensionAndUomRequestMap =
          productItemDistributionInfoRequest.getDimensionsAndUOMRequest().stream().collect(
              Collectors.toMap(DimensionAndUomRequest::getUomCode, Function.identity(),
                  (existing, current) -> existing));
      for (DimensionAndUomRequest dimensionAndUomRequest : productItemDistributionInfoRequest.getDimensionsAndUOMRequest()) {
        if (!uomCodeToDimensionsAndUomResponse.containsKey(dimensionAndUomRequest.getUomCode())) {
          auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
              String.format(SaveHistoryConstants.NEW_UOM_ADDED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
              Constants.HYPHEN, Constants.HYPHEN, productItemDistributionInfoRequest.getSkuCode()));
          dataNotUpdated.set(false);
        } else {
          hasUomInfoNotUpdated(uomCodeToDimensionsAndUomResponse.get(dimensionAndUomRequest.getUomCode()),
              dimensionAndUomRequest, dataNotUpdated, auditTrailDtoList,
              productItemDistributionInfoRequest.getSkuCode());
        }
      }
      for (DimensionsAndUomResponse dimensionsAndUomResponse : existingDistributionInfo.getDimensionsAndUomResponse()) {
        if (!dimensionAndUomRequestMap.containsKey(dimensionsAndUomResponse.getUomCode())) {
          auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
              String.format(SaveHistoryConstants.UOM_DELETED, Uom.getNameByCode(dimensionsAndUomResponse.getUomCode())),
              Constants.HYPHEN, Constants.HYPHEN, productItemDistributionInfoRequest.getSkuCode()));
          dataNotUpdated.set(false);
        }
      }
    }
  }

  private void hasProductItemDistributionInfoNotUpdated(
      DistributionInfoPerSkuResponse distributionInfoPerSkuResponse,
      ProductItemDistributionInfoRequest productItemDistributionInfoRequest, AtomicBoolean dataNotUpdated,
      List<AuditTrailDto> auditTrailDtoList) {
    if (!distributionInfoPerSkuResponse.getDistributionItemInfoResponse().getOmniChannelSku()
        .equals(productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(SaveHistoryConstants.OMNICHANNEL_SKU_UPDATED,
          distributionInfoPerSkuResponse.getDistributionItemInfoResponse().getOmniChannelSku(),
          productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku(),
          distributionInfoPerSkuResponse.getSkuCode()));
    }
    if (!distributionInfoPerSkuResponse.getDistributionItemInfoResponse().getOrigin()
        .equals(productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOrigin())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(SaveHistoryConstants.ORIGIN_UPDATED,
          distributionInfoPerSkuResponse.getDistributionItemInfoResponse().getOrigin(),
          productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOrigin(),
          distributionInfoPerSkuResponse.getSkuCode()));
    }
    if (distributionInfoPerSkuResponse.getDistributionItemInfoResponse().isExpiry()
        != productItemDistributionInfoRequest.getDistributionItemInfoRequest().isExpiry()) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(SaveHistoryConstants.EXPIRY_UPDATED,
          String.valueOf(distributionInfoPerSkuResponse.getDistributionItemInfoResponse().isExpiry()),
          String.valueOf(productItemDistributionInfoRequest.getDistributionItemInfoRequest().isExpiry()),
          distributionInfoPerSkuResponse.getSkuCode()));
    }
  }

  private void hasUomInfoNotUpdated(DimensionsAndUomResponse dimensionsAndUomResponse,
      DimensionAndUomRequest dimensionAndUomRequest, AtomicBoolean dataNotUpdated,
      List<AuditTrailDto> auditTrailDtoList, String skuCode) {
    if (!dimensionAndUomRequest.getConversion().equals(dimensionsAndUomResponse.getConversion())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(String.format(SaveHistoryConstants.CONVERSION_UPDATED,
              Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getConversion()),
          String.valueOf(dimensionAndUomRequest.getConversion()), skuCode));
    }
    if (!dimensionAndUomRequest.getLength().equals(dimensionsAndUomResponse.getLength())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
          String.format(SaveHistoryConstants.LENGTH_UPDATED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getLength()), String.valueOf(dimensionAndUomRequest.getLength()),
          skuCode));
    }
    if (!dimensionAndUomRequest.getHeight().equals(dimensionsAndUomResponse.getHeight())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
          String.format(SaveHistoryConstants.HEIGHT_UPDATED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getHeight()), String.valueOf(dimensionAndUomRequest.getHeight()),
          skuCode));
    }
    if (!dimensionAndUomRequest.getWidth().equals(dimensionsAndUomResponse.getWidth())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
          String.format(SaveHistoryConstants.WIDTH_UPDATED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getWidth()), String.valueOf(dimensionAndUomRequest.getWidth()),
          skuCode));
    }
    if (!dimensionAndUomRequest.getWeight().equals(dimensionsAndUomResponse.getWeight())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
          String.format(SaveHistoryConstants.WEIGHT_UPDATED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getWeight()), String.valueOf(dimensionAndUomRequest.getWeight()),
          skuCode));
    }
    if (!CollectionUtils.isEqualCollection(dimensionAndUomRequest.getUpcEanList(),
        dimensionsAndUomResponse.getUpcEanList())) {
      dataNotUpdated.set(false);
      auditTrailDtoList.add(CommonUtils.getAuditTrailDto(
          String.format(SaveHistoryConstants.EAN_UPC_UPDATED, Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.valueOf(dimensionsAndUomResponse.getUpcEanList()),
          String.valueOf(dimensionAndUomRequest.getUpcEanList()), skuCode));
    }
    if(!dimensionAndUomRequest.getUomType().equals(dimensionsAndUomResponse.getUomType())) {
      dataNotUpdated.set(false);
    }
  }

  @Override
  public void validateDistributionInfoUpdateRequest(DistributionInfoRequest distributionInfoRequest,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo,
      Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap) throws Exception {
    Map<String, String> omniChannelSkuToItemCodeMap = new HashMap<>();
    for (ProductItemDistributionInfoRequest productItemDistributionInfoRequest :
        distributionInfoRequest.getProductItems()) {
      validateDuplicateOmniChannelSku(omniChannelSkuToResponseMap, omniChannelSkuToItemCodeMap,
          productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku(),
          productItemDistributionInfoRequest.getSkuCode());
    }

    Set<String> upcEanSetForAllUOMs = new HashSet<>();
    for (ProductItemDistributionInfoRequest productItemDistributionInfoRequest :
        distributionInfoRequest.getProductItems()) {
      validateProductItemDistributionInfoRequest(productItemDistributionInfoRequest,
          skuCodeToExistingDistributionInfo, upcEanSetForAllUOMs);
    }
  }

  @Override
  public void validateDuplicateOmniChannelSku(Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap,
      Map<String, String> omniChannelSkuToItemCodeMap, String omniChannelSku, String skuCode) {
    if (omniChannelSkuToItemCodeMap.containsKey(omniChannelSku) || (
        omniChannelSkuToResponseMap.containsKey(omniChannelSku) && !StringUtils.equals(
            omniChannelSkuToResponseMap.get(omniChannelSku).getSkuCode(), skuCode))) {
      log.error("Duplicate omniChannelSku present for the productCode :{} for value : {}", skuCode, omniChannelSku);
      throw new ApiDataNotFoundException(ApiErrorCode.OMNI_CHANNEL_SKU_ALREADY_EXISTS_OR_DUPLICATE.getDesc(),
          ApiErrorCode.OMNI_CHANNEL_SKU_ALREADY_EXISTS_OR_DUPLICATE);
    } else {
      omniChannelSkuToItemCodeMap.put(omniChannelSku, skuCode);
    }
  }

  private void validateProductItemDistributionInfoRequest(
      ProductItemDistributionInfoRequest productItemDistributionInfoRequest,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo,
      Set<String> upcEanSetForAllUOMs) throws Exception {
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(productItemDistributionInfoRequest.getSkuCode()),
        ValidationErrorMessage.ITEM_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(productItemDistributionInfoRequest.getDimensionsAndUOMRequest()),
        ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_MUST_NOT_BE_EMPTY.getMessage());

    int baseUomCount = 0;
    Set<String> uomCodeSetFromRequest = new HashSet<>();
    boolean conversionUpdatedOrAnyUomDeletion = false;
    Map<String, DimensionsAndUomResponse> uomCodeToDimensionsAndUomResponse = new HashMap<>();
    Map<String, String> existingSkuCodeAndBaseUom = new HashMap<>();
    getExistingSkuCodeAndBaseUomMap(productItemDistributionInfoRequest, skuCodeToExistingDistributionInfo,
        uomCodeToDimensionsAndUomResponse, existingSkuCodeAndBaseUom);

    for (DimensionAndUomRequest dimensionAndUomRequest :
        productItemDistributionInfoRequest.getDimensionsAndUOMRequest()) {
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getLength()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_LENGTH_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getWidth()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_WIDTH_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getHeight()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_HEIGHT_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getWeight()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_WEIGHT_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(dimensionAndUomRequest.getLength() > 0,
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_LENGTH_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(dimensionAndUomRequest.getWidth() > 0,
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_WIDTH_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(dimensionAndUomRequest.getHeight() > 0,
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_HEIGHT_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(dimensionAndUomRequest.getWeight() > 0,
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_WEIGHT_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getUomCode()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_UOM_CODE_MUST_NOT_BE_EMPTY.getMessage());
      GdnPreconditions.checkArgument(Objects.nonNull(dimensionAndUomRequest.getUomType()),
          ValidationErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_UOM_TYPE_MUST_NOT_BE_EMPTY.getMessage());

      baseUomCount =
          getBaseUomCount(productItemDistributionInfoRequest, dimensionAndUomRequest, existingSkuCodeAndBaseUom,
              baseUomCount);

      for (String upcEanCode : dimensionAndUomRequest.getUpcEanList()) {
        GdnPreconditions.checkArgument(!upcEanSetForAllUOMs.contains(upcEanCode),
            ValidationErrorMessage.DUPLICATE_EAN_UPC_VALUE.getMessage());
        GdnPreconditions.checkArgument(BarcodeGenerator.isValidUomUPCCode(upcEanCode),
            ValidationErrorMessage.INVALID_EAN_UPC_VALUE.getMessage());
        upcEanSetForAllUOMs.add(upcEanCode);
      }
      GdnPreconditions.checkArgument(!uomCodeSetFromRequest.contains(dimensionAndUomRequest.getUomCode()),
          ValidationErrorMessage.DUPLICATE_UOM_CODE.getMessage());
      uomCodeSetFromRequest.add(dimensionAndUomRequest.getUomCode());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(Uom.getNameByCode(dimensionAndUomRequest.getUomCode())),
          String.format(ValidationErrorMessage.INVALID_UOM_CODE.getMessage(),
              Arrays.stream(Uom.values()).map(Enum::name).collect(Collectors.toList())));
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(dimensionAndUomRequest.getUomType()),
          ValidationErrorMessage.INVALID_UOM_TYPE.getMessage());

      if (uomCodeToDimensionsAndUomResponse.containsKey(dimensionAndUomRequest.getUomCode())
          && !dimensionAndUomRequest.getConversion().equals(
          uomCodeToDimensionsAndUomResponse.get(dimensionAndUomRequest.getUomCode())
              .getConversion())) {
        conversionUpdatedOrAnyUomDeletion = true;
      }
    }

    for (Map.Entry<String, DimensionsAndUomResponse> entry :
        uomCodeToDimensionsAndUomResponse.entrySet()) {
      if (!uomCodeSetFromRequest.contains(entry.getKey())) {
        conversionUpdatedOrAnyUomDeletion = true;
        break;
      }
    }

    GdnPreconditions.checkArgument(baseUomCount == 1,
        ValidationErrorMessage.DUPLICATE_BASE_UOM.getMessage());

    if (conversionUpdatedOrAnyUomDeletion) {
      validateWarehouseInventoryAndOMSRequest(productItemDistributionInfoRequest);
    }
  }

  private static int getBaseUomCount(ProductItemDistributionInfoRequest productItemDistributionInfoRequest,
      DimensionAndUomRequest dimensionAndUomRequest, Map<String, String> existingSkuCodeAndBaseUom, int baseUomCount) {
    if (UomType.Base.name().equals(dimensionAndUomRequest.getUomType())) {
      if (existingSkuCodeAndBaseUom.containsKey(productItemDistributionInfoRequest.getSkuCode())) {
        GdnPreconditions.checkArgument(existingSkuCodeAndBaseUom.get(productItemDistributionInfoRequest.getSkuCode())
                .equals(dimensionAndUomRequest.getUomCode()),
            ValidationErrorMessage.BASE_UOM_CANNOT_BE_CHANGED.getMessage());
      }
      dimensionAndUomRequest.setConversion(1d);
      baseUomCount++;
    }
    return baseUomCount;
  }

  private static void getExistingSkuCodeAndBaseUomMap(ProductItemDistributionInfoRequest productItemDistributionInfoRequest,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo,
      Map<String, DimensionsAndUomResponse> uomCodeToDimensionsAndUomResponse,
      Map<String, String> existingSkuCodeAndBaseUom) {
    DistributionInfoPerSkuResponse distributionInfoPerSkuResponse;
    if (skuCodeToExistingDistributionInfo.containsKey(productItemDistributionInfoRequest.getSkuCode())) {
      distributionInfoPerSkuResponse =
          skuCodeToExistingDistributionInfo.get(productItemDistributionInfoRequest.getSkuCode());
      for (DimensionsAndUomResponse dimensionsAndUomResponse : distributionInfoPerSkuResponse.getDimensionsAndUomResponse()) {
        uomCodeToDimensionsAndUomResponse.putIfAbsent(dimensionsAndUomResponse.getUomCode(), dimensionsAndUomResponse);
        if (UomType.Base.name().equalsIgnoreCase(dimensionsAndUomResponse.getUomType())) {
          existingSkuCodeAndBaseUom.put(distributionInfoPerSkuResponse.getSkuCode(),
              dimensionsAndUomResponse.getUomCode());
        }
      }
    }
  }

  private void validateWarehouseInventoryAndOMSRequest(
      ProductItemDistributionInfoRequest productItemDistributionInfoRequest) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId());
    mandatoryRequestParam.setUsername(GdnMandatoryRequestParameterUtil.getUsername());
    boolean isWarehouseStockPresent =
        inventoryOutbound.isWarehouseStockPresent(mandatoryRequestParam,
            productItemDistributionInfoRequest.getSkuCode());
    if (isWarehouseStockPresent) {
      throw new ApiDataNotFoundException(ApiErrorCode.CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_WAREHOUSE.getDesc(),
          ApiErrorCode.CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_WAREHOUSE);
    }
    List<UomStockValidationResponse> uomStockValidationResponses = omsOutBound.validateUomEditable(
        UomStockValidationRequest.builder()
            .itemCodes(List.of(productItemDistributionInfoRequest.getSkuCode())).build());
    for (UomStockValidationResponse uomStockValidationResponse : uomStockValidationResponses) {
      if (Boolean.TRUE.equals(uomStockValidationResponse.getHasExistingStock())
          || Boolean.TRUE.equals(uomStockValidationResponse.getHasPendingDocuments())
          || !uomStockValidationResponse.isSuccess()) {
        throw new ApiDataNotFoundException(ApiErrorCode.CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_OMS.getDesc(),
            ApiErrorCode.CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_OMS);
      }
    }
  }

  @Override
  public GdnRestListResponse<DistributionInfoPerSkuResponse> fetchDistributionInfoByProductCode(String storeId,
      String productCode, int page, int size) {
    return productOutbound.getDistributionInfoPerSkuResponse(productCode, page, size);
  }
}
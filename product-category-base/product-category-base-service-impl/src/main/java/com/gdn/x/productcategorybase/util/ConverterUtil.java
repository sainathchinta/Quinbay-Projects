package com.gdn.x.productcategorybase.util;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.BrandAuthorisationActivity;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityAttributeModel;
import com.gdn.x.productcategorybase.dto.CategoryAttributeMappingUpdateHistoryEventDTO;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.WholesaleConfigDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueDto;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataRow;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.entity.CategoryHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.util.BeanUtils;

import org.hibernate.Hibernate;
import org.springframework.data.domain.Page;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.OldAndNewPathDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.CategoryAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponseWithCategoryAndAttribute;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.PredictionCategoryMapping;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeArchive;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductCategoryArchive;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageArchive;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemArchive;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductItemImageArchive;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.entity.RestrictedKeywordHistory;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.service.FileStorageService;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageImpl;

@Slf4j
public class ConverterUtil {

  private static FileStorageService fileStorageService;

  public static void setFileStorageService(FileStorageService fileStorageService) {
    ConverterUtil.fileStorageService = fileStorageService;
  }

  private static final int VOLUME_WEIGHT = 6000;
  private static final double SHIPPING_WEIGHT = 100;
  private static final String DEFAULT_BP_CODE = "NA";
  private static final String DOT = ".";
  private static final String NOT_ALPHA_NUMERIC_REGEX = "[^A-Za-z0-9]";
  private static final String HYPHEN = "-";
  private static final String LOGO_EXT = "-logo.";
  private static final String BANNER_EXT = "-banner.";
  private static final String PATH_SEPARATOR = "/";
  private static final double MINIMUM_DIMENSION_ALLOWED = 0.0;
  private static final String DOCUMENT_LINK = "documentLink";

  public static CategoryAttribute toCategoryAttribute(String storeId,
      CategoryAttributeUpdateDTO categoryAttributeUpdateDTO, Category category, Attribute attribute, String updatedBy,
      Date updatedDate) {
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setMainDefiningAttribute(categoryAttributeUpdateDTO.isMainDefiningAttribute());
    categoryAttribute.setUSP(categoryAttributeUpdateDTO.isUsp());
    categoryAttribute.setSequence(categoryAttributeUpdateDTO.getSequence());
    categoryAttribute.setCategory(category);
    categoryAttribute.setAttribute(attribute);
    categoryAttribute.setStoreId(storeId);
    categoryAttribute.setMarkForDelete(false);
    categoryAttribute.setCreatedBy(updatedBy);
    categoryAttribute.setCreatedDate(updatedDate);
    categoryAttribute.setUpdatedBy(updatedBy);
    categoryAttribute.setUpdatedDate(updatedDate);
    return categoryAttribute;
  }

  public static CategoryReference toCategoryReference(String storeId, Category masterCategory, Category salesCategory,
      String updatedBy, Date updatedDate) {
    CategoryReference categoryReference = new CategoryReference(masterCategory, salesCategory);
    categoryReference.setStoreId(storeId);
    categoryReference.setMarkForDelete(false);
    categoryReference.setCreatedBy(updatedBy);
    categoryReference.setCreatedDate(updatedDate);
    categoryReference.setUpdatedBy(updatedBy);
    categoryReference.setUpdatedDate(updatedDate);
    return categoryReference;
  }

  public static List<BrandAuthFilterResponse> toBrandAuthFilterResponse(
      Page<BrandAuthorisation> brandAuthorisations, String status, int brandAuthNearExpiryDaysThreshold) {
    List<BrandAuthFilterResponse> brandAuthFilterResponses =
        brandAuthorisations.getContent().stream().map(
            brandAuthorisation -> ConverterUtil.toBrandAuthorisationFilterResponse(
                brandAuthorisation, status, brandAuthNearExpiryDaysThreshold)).collect(Collectors.toList());
    return brandAuthFilterResponses;
  }

  public static List<BrandAuthFilterResponse> toBrandAuthFilterResponseForWip(
      Page<BrandAuthorisationWip> brandAuthorisationWips, String status) {
    return brandAuthorisationWips.getContent().stream().map(
        brandAuthorisationWip -> ConverterUtil.toBrandAuthorisationFilterResponse(
            brandAuthorisationWip, status)).collect(Collectors.toList());
  }

  public static List<BrandAuthorisationWip> toBrandAuthFilterResponseWip(
      Page<BrandAuthorisation> brandAuthorisations, String status) {
    return brandAuthorisations.getContent().stream().map(
        brandAuthorisationWip -> ConverterUtil.toBrandAuthorisationWipResponse(
            brandAuthorisationWip, status)).collect(Collectors.toList());
  }

  private static BrandAuthFilterResponse toBrandAuthorisationFilterResponse(
      BrandAuthorisation brandAuthorisation, String status, int brandAuthNearExpiryDaysThreshold) {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    Date date = new Date();
    BeanUtils.copyProperties(brandAuthorisation, brandAuthFilterResponse, DOCUMENT_LINK);
    if (Objects.nonNull(brandAuthorisation.getAuthorisationStatus())) {
      brandAuthFilterResponse.setStatus(brandAuthorisation.getAuthorisationStatus().name());
    }
    if (Objects.nonNull(brandAuthorisation.getAuthExpireDate())) {
      brandAuthFilterResponse.setAuthEndDate(brandAuthorisation.getAuthExpireDate());
    }
    if (StringUtils.isNotEmpty(brandAuthorisation.getDocumentLink())) {
      brandAuthFilterResponse.setDocumentLinks(
          Arrays.asList(brandAuthorisation.getDocumentLink().split(Constants.COMMA)));
    }
    if(BrandAuthorisationStatus.NEAR_EXPIRY.name().equals(status)){
      brandAuthFilterResponse.setStatus(status);
    }
    if(date.after(brandAuthorisation.getAuthExpireDate())){
      brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.EXPIRED.name());
    }
    else if (checkNearExpiry(brandAuthNearExpiryDaysThreshold, brandAuthorisation.getAuthExpireDate())) {
      brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.NEAR_EXPIRY.name());
    }
    return brandAuthFilterResponse;
  }

  private static boolean checkNearExpiry(int brandAuthNearExpiryDaysThreshold,
      Date authExpireDate) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, brandAuthNearExpiryDaysThreshold);
    Date currDatePlusThreshold = calendar.getTime();
    return currDatePlusThreshold.after(authExpireDate);
  }

  private static BrandAuthFilterResponse toBrandAuthorisationFilterResponse(
      BrandAuthorisationWip brandAuthorisationWip, String status) {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    BeanUtils.copyProperties(brandAuthorisationWip, brandAuthFilterResponse, DOCUMENT_LINK);
    if (Objects.nonNull(brandAuthorisationWip.getAuthorisationStatus())) {
      brandAuthFilterResponse.setStatus(brandAuthorisationWip.getAuthorisationStatus().name());
    }
    brandAuthFilterResponse.setAuthStartDate(brandAuthorisationWip.getAuthStartDate());
    brandAuthFilterResponse.setAuthEndDate(brandAuthorisationWip.getAuthExpireDate());
    brandAuthFilterResponse.setUpdatedDate(brandAuthorisationWip.getUpdatedDate());
    brandAuthFilterResponse.setUpdatedBy(brandAuthorisationWip.getUpdatedBy());
    brandAuthFilterResponse.setReasons(brandAuthorisationWip.getReasons());
    if (StringUtils.isNotEmpty(brandAuthorisationWip.getDocumentLink())) {
      brandAuthFilterResponse.setDocumentLinks(
          Arrays.asList(brandAuthorisationWip.getDocumentLink().split(Constants.COMMA)));
    }
    if(BrandAuthorizationWipStatus.NEAR_EXPIRY.name().equals(status)) {
      brandAuthFilterResponse.setStatus(status);
    }
    return brandAuthFilterResponse;
  }

  private static BrandAuthorisationWip toBrandAuthorisationWipResponse(
      BrandAuthorisation brandAuthorisation, String status) {
    Date currentDate = new Date();
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthorisation, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(
        BrandAuthorizationWipStatus.valueOf(brandAuthorisation.getAuthorisationStatus().name()));
    if (BrandAuthorizationWipStatus.NEAR_EXPIRY.name().equals(status)) {
      brandAuthorisationWip.setAuthorisationStatus(
          BrandAuthorizationWipStatus.valueOf(status));
    }
    if (currentDate.after(brandAuthorisation.getAuthExpireDate())) {
      brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.EXPIRED);
    }
    return brandAuthorisationWip;
  }

  public static RestrictedKeywordHistoryResponse toRestrictedKeywordHistoryResponse(
      RestrictedKeywordHistory restrictedKeywordHistory) {
    RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse = new RestrictedKeywordHistoryResponse();
    BeanUtils.copyProperties(restrictedKeywordHistory, restrictedKeywordHistoryResponse);
    return restrictedKeywordHistoryResponse;
  }

  public static RestrictedKeywordsListingResponse toRestrictedKeywordsListingResponse(
      RestrictedKeyword restrictedKeyword) {
    RestrictedKeywordsListingResponse response = new RestrictedKeywordsListingResponse();
    BeanUtils.copyProperties(restrictedKeyword, response);
    response.setValidateByDs(
      BooleanUtils.toBooleanDefaultIfNull(restrictedKeyword.getValidateByDs(), false));
    response.setValidateOnUi(
      BooleanUtils.toBooleanDefaultIfNull(restrictedKeyword.getValidateOnUi(), false));
    response.setKeywordId(restrictedKeyword.getId());
    return response;
  }

  public static List<UiValidationRestrictedKeywordsResponse> toListOfRestrictedKeywordsForUiValidation(
      List<RestrictedKeyword> restrictedKeywords) {
    return restrictedKeywords.stream().map(restrictedKeyword -> {
      UiValidationRestrictedKeywordsResponse response = new UiValidationRestrictedKeywordsResponse();
      response.setKeywordId(restrictedKeyword.getId());
      response.setKeyword(restrictedKeyword.getKeyword());
      return response;
    }).collect(Collectors.toList());
  }

  /**
   * Calculate shipping weight
   *
   * @param length
   * @param width
   * @param height
   * @param weight
   * @param logisticAdjustment
   */
  public static double generateShippingWeight(double length, double width, double height, double weight,
      int logisticAdjustment) {
    double volumeWeight = (length * width * height) / VOLUME_WEIGHT;
    double shippingWeight = volumeWeight > weight ? volumeWeight : weight;
    shippingWeight *= logisticAdjustment / SHIPPING_WEIGHT;
    return shippingWeight;
  }

  public static SolrBrandModel generateSolrBrandModelForActiveBrand(Brand brand) {
    return SolrBrandModel.builder().id(brand.getBrandWipId()).brandApproved(Boolean.TRUE)
        .brandCode(brand.getBrandCode()).brandValue(brand.getBrandName()).businessPartnerCode(DEFAULT_BP_CODE)
        .updatedDate(brand.getUpdatedDate()).protectedBrand(brand.isProtectedBrand()).build();
  }

  public static SolrBrandModel generateSolrBrandModelModelForBrandWip(BrandWip brandWip) {
    return SolrBrandModel.builder().id(brandWip.getId()).brandApproved(Boolean.FALSE)
      .brandCode(brandWip.getBrandRequestCode()).brandValue(brandWip.getBrandName())
      .businessPartnerCode(brandWip.getBusinessPartnerCode()).updatedDate(brandWip.getUpdatedDate())
      .protectedBrand(brandWip.isProtectedBrand()).build();
  }

  public static SolrDeleteBrandDomainEventModel generateSolrDeleteBrandDomainEventModel(List<String> ids) {
    return SolrDeleteBrandDomainEventModel.builder().ids(ids).build();
  }

  public static SolrUpdateBrandModel generateSolrUpdateBrandDomainEventModel(BrandWip brandWip) {
    return SolrUpdateBrandModel.builder().id(brandWip.getId()).brandCode(brandWip.getBrandCode())
        .brandApproved(Boolean.TRUE).businessPartnerCode(DEFAULT_BP_CODE).build();
  }

  public static Map<String, PredictionCategoryMapping> predictionCategoryMappingListToMap(
      List<PredictionCategoryMapping> predictionCategoryMappingSavedList) {
    return predictionCategoryMappingSavedList.stream().collect(Collectors.toMap(
        predictionCategoryMapping -> predictionCategoryMapping.getPredictionId() + HYPHEN
            + predictionCategoryMapping.getCategoryCode(), predictionCategoryMapping -> predictionCategoryMapping,
        (a, b) -> b));
  }

  public static BrandWipResponse generateBrandWipResponseFromBrandWip(BrandWip brandWip) {
    BrandWipResponse brandWipResponse = new BrandWipResponse();
    BeanUtils.copyProperties(brandWip, brandWipResponse, "description", "state");
    brandWipResponse.setBrandDescription(new String(brandWip.getBrandDescription()));
    brandWipResponse.setState(brandWip.getState().name());
    return brandWipResponse;
  }

  public static List<BrandWipResponse> generateBrandWipResponses(Page<BrandWip> brandWips) {
    return Optional.ofNullable(brandWips.getContent()).orElseGet(Collections::emptyList).stream()
        .map(brandWip -> generateBrandWipResponseFromBrandWip(brandWip)).collect(Collectors.toList());
  }

  public static SolrBrandModel generateSolrBrandModel(BrandWip brandWip) {
    return SolrBrandModel.builder().brandCode(brandWip.getBrandRequestCode()).id(brandWip.getId())
        .brandValue(brandWip.getBrandName()).businessPartnerCode(brandWip.getBusinessPartnerCode())
        .updatedDate(brandWip.getUpdatedDate()).brandApproved(Boolean.FALSE).protectedBrand(brandWip.isProtectedBrand())
        .build();
  }

  public static SolrBrandModel generateSolrBrandModelApproveExistingBrand(BrandWip brandWip) {
    return SolrBrandModel.builder().brandCode(brandWip.getBrandCode()).id(brandWip.getId())
        .brandValue(brandWip.getBrandName()).businessPartnerCode(DEFAULT_BP_CODE).updatedDate(brandWip.getUpdatedDate())
        .brandApproved(Boolean.TRUE).protectedBrand(Boolean.TRUE).build();
  }

  public static SolrBrandModel generateSolrBrandModelApproveExistingBrand(Brand brand) {
    return SolrBrandModel.builder().brandCode(brand.getBrandCode()).id(brand.getId()).brandValue(brand.getBrandName())
        .businessPartnerCode(DEFAULT_BP_CODE).updatedDate(brand.getUpdatedDate()).brandApproved(Boolean.TRUE).build();
  }

  public static List<BrandWipHistoryResponse> generateBrandWipHistoryResponses(Page<BrandWipHistory> brandWips) {
    return Optional.ofNullable(brandWips.getContent())
        .orElseGet(Collections::emptyList)
        .stream().map(brandWipHistory -> generateBrandWipHistoryResponseFromBrandWipHistory(brandWipHistory))
        .collect(Collectors.toList());
  }

  private static BrandWipHistoryResponse generateBrandWipHistoryResponseFromBrandWipHistory(
      BrandWipHistory brandWipHistory) {
    BrandWipHistoryResponse brandWipHistoryResponse = new BrandWipHistoryResponse();
    BeanUtils.copyProperties(brandWipHistory, brandWipHistoryResponse, "description");
    brandWipHistoryResponse.setDescription(new String(brandWipHistory.getDescription()));
    brandWipHistoryResponse.setState(brandWipHistory.getState().name());
    return brandWipHistoryResponse;
  }

  public static PredictionCategoryMapping getPredictionCategoryMapping(PredictionCategoryMappingRequest request) {
    PredictionCategoryMapping predictionCategoryMappingNew = new PredictionCategoryMapping();
    predictionCategoryMappingNew.setPredictionId(request.getPredictionId());
    predictionCategoryMappingNew.setCategoryCode(request.getCategoryCode());
    predictionCategoryMappingNew.setStoreId(request.getStoreId());
    return predictionCategoryMappingNew;
  }

  public static BrandRejectionInfoResponse generateBrandRejectionInfoResponse(BrandWip brandWip) {
    BrandRejectionInfoResponse brandRejectionInfoResponse =
        BrandRejectionInfoResponse.builder().brandName(brandWip.getBrandName())
            .rejectionReason(new String(brandWip.getNotes())).brandRequestCode(brandWip.getBrandRequestCode()).build();
    brandRejectionInfoResponse.setId(brandWip.getId());
    return brandRejectionInfoResponse;
  }

  public static Brand brandWipToBrand(BrandWip brandWip) {
    Brand brand = new Brand();
    BeanUtils.copyProperties(brandWip, brand);
    brand.setBrandWipId(brandWip.getId());
    return brand;
  }

  public static BrandApprovedOrRejectedDomainEventModel generateBrandApprovedOrRejectedDomainEventModel(
      BrandWip brandWip) {
    return BrandApprovedOrRejectedDomainEventModel.builder().brandRequestCode(brandWip.getBrandRequestCode())
        .brandCode(brandWip.getBrandCode()).brandName(brandWip.getBrandName())
        .brandApprovalStatus(brandWip.getState().name()).businessPartnerCode(brandWip.getBusinessPartnerCode())
        .protectedBrand(brandWip.isProtectedBrand()).build();
  }

  public static CategoryConfigurationHistory generateCategoryConfigurationHistory(String username, String categoryCode,
      String categoryName, String oldValue, String newValue, String activity, String storeId) {
    CategoryConfigurationHistory categoryConfigurationHistory = new CategoryConfigurationHistory();
    categoryConfigurationHistory.setCategoryCode(categoryCode);
    categoryConfigurationHistory.setCategoryName(categoryName);
    categoryConfigurationHistory.setOldValue(oldValue);
    categoryConfigurationHistory.setNewValue(newValue);
    categoryConfigurationHistory.setActivity(activity);
    categoryConfigurationHistory.setCreatedBy(username);
    categoryConfigurationHistory.setUpdatedBy(username);
    categoryConfigurationHistory.setMarkForDelete(false);
    categoryConfigurationHistory.setStoreId(storeId);
    return categoryConfigurationHistory;
  }

  public static MerchantConfigurationHistory generateMerchantConfigurationHistory(String username, String merchantName,
      String merchantCode, String oldValue, String newValue, String activity, String storeId) {
    MerchantConfigurationHistory merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistory.setMerchantCode(merchantCode);
    merchantConfigurationHistory.setMerchantName(merchantName);
    merchantConfigurationHistory.setOldValue(oldValue);
    merchantConfigurationHistory.setNewValue(newValue);
    merchantConfigurationHistory.setActivity(activity);
    merchantConfigurationHistory.setCreatedBy(username);
    merchantConfigurationHistory.setUpdatedBy(username);
    merchantConfigurationHistory.setMarkForDelete(false);
    merchantConfigurationHistory.setStoreId(storeId);
    return merchantConfigurationHistory;
  }

  public static List<CategoryConfigurationFilterResponse> toCategoryConfigurationFilterResponseList(
      List<CategoryConfiguration> categoryConfigurationList) {
    return categoryConfigurationList.stream().map(ConverterUtil::toCategoryConfigurationFilterResponse)
        .collect(Collectors.toList());
  }

  private static CategoryConfigurationFilterResponse toCategoryConfigurationFilterResponse(
      CategoryConfiguration categoryConfiguration) {
    CategoryConfigurationFilterResponse categoryConfigurationFilterResponse =
        CategoryConfigurationFilterResponse.builder()
            .categoryCode(categoryConfiguration.getCategory().getCategoryCode())
            .categoryName(categoryConfiguration.getCategory().getName())
            .reviewConfig(categoryConfiguration.getReviewConfig()).createdBy(categoryConfiguration.getCreatedBy())
            .createdDate(categoryConfiguration.getCreatedDate()).build();
    return categoryConfigurationFilterResponse;
  }

  public static List<MerchantConfigurationFilterResponse> toMerchantConfigurationFilterResponseList(
      List<MerchantConfiguration> merchantConfigurationList) {
    return merchantConfigurationList.stream().map(ConverterUtil::toMerchantConfigurationResponse)
        .collect(Collectors.toList());
  }

  private static MerchantConfigurationFilterResponse toMerchantConfigurationResponse(
      MerchantConfiguration merchantConfiguration) {
    MerchantConfigurationFilterResponse merchantConfigurationFilterResponse =
        MerchantConfigurationFilterResponse.builder().categoryName(merchantConfiguration.getCategoryName())
            .merchantCode(merchantConfiguration.getMerchantCode()).merchantName(merchantConfiguration.getMerchantName())
            .reviewConfig(merchantConfiguration.getReviewConfig()).createdBy(merchantConfiguration.getCreatedBy())
            .createdDate(merchantConfiguration.getCreatedDate()).build();
    return merchantConfigurationFilterResponse;
  }

  public static CategoryConfigurationHistoryResponse toCategoryConfigurationHistoryResponse(
      CategoryConfigurationHistory categoryConfigurationHistory) {
    CategoryConfigurationHistoryResponse categoryConfigurationHistoryResponse =
        CategoryConfigurationHistoryResponse.builder().categoryName(categoryConfigurationHistory.getCategoryName())
            .categoryCode(categoryConfigurationHistory.getCategoryCode())
            .activity(categoryConfigurationHistory.getActivity()).createdBy(categoryConfigurationHistory.getCreatedBy())
            .createdDate(categoryConfigurationHistory.getCreatedDate())
            .newValue(categoryConfigurationHistory.getNewValue()).oldValue(categoryConfigurationHistory.getOldValue())
            .updatedBy(categoryConfigurationHistory.getUpdatedBy())
            .updatedDate(categoryConfigurationHistory.getUpdatedDate()).build();
    return categoryConfigurationHistoryResponse;
  }

  public static MerchantConfigurationHistoryResponse toMerchantConfigurationHistoryResponse(
      MerchantConfigurationHistory merchantConfigurationHistory) {
    MerchantConfigurationHistoryResponse merchantConfigurationHistoryResponse =
        MerchantConfigurationHistoryResponse.builder().activity(merchantConfigurationHistory.getActivity())
            .createdDate(merchantConfigurationHistory.getCreatedDate())
            .updatedBy(merchantConfigurationHistory.getUpdatedBy())
            .updatedDate(merchantConfigurationHistory.getUpdatedDate())
            .newValue(merchantConfigurationHistory.getNewValue()).oldValue(merchantConfigurationHistory.getOldValue())
            .merchantCode(merchantConfigurationHistory.getMerchantCode())
            .merchantName(merchantConfigurationHistory.getMerchantName())
            .createdBy(merchantConfigurationHistory.getCreatedBy()).build();
    return merchantConfigurationHistoryResponse;
  }

  public static boolean filterProcessedItemImages(ProductItemImage productItemImage) {
    if (productItemImage.isEdited()) {
      return productItemImage.isActive();
    }
    if (Objects.isNull(productItemImage.getOriginalImage())) {
      return true;
    } else {
      return !productItemImage.getOriginalImage();
    }
  }

  public static ProductAttributeExtracted getProductAttributeExtracted(String productCode, String categoryCode) {
    ProductAttributeExtracted productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(productCode);
    productAttributeExtracted.setCnCategoryCode(categoryCode);
    productAttributeExtracted.setSource(Constants.MATRIX);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
    productAttributeExtracted.setCreatedBy(Constants.SYSTEM);
    productAttributeExtracted.setUpdatedBy(Constants.SYSTEM);
    productAttributeExtracted.setStoreId(Constants.DEFAULT_STORE_ID);
    return productAttributeExtracted;
  }

  public static boolean filterProcessedProductImages(ProductImage productImage) {
    if (productImage.isEdited()) {
      return productImage.isActive();
    }
    if (Objects.isNull(productImage.getOriginalImage())) {
      return true;
    } else {
      return !productImage.getOriginalImage();
    }
  }

  public static SimpleItemDetailResponse toSimpleItemDetailResponse(ProductItem productItem) {
    return SimpleItemDetailResponse.builder().itemCode(productItem.getSkuCode())
        .itemName(productItem.getGeneratedItemName()).productCode(productItem.getProduct().getProductCode()).build();
  }

  public static List<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCodeResponses(
      Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap) {
    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponseList = new ArrayList<>();
    for (Map.Entry<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMapResponse : predictionIdAndCategoryCodesMap.entrySet()) {
      PredictionIdAndCategoryCodeResponse predictionIdAndCategoryCodeResponse =
          new PredictionIdAndCategoryCodeResponse();
      predictionIdAndCategoryCodeResponse.setPredictionId(predictionIdAndCategoryCodesMapResponse.getKey());
      predictionIdAndCategoryCodeResponse.setCategoryCodeAndNameResponseList(
          predictionIdAndCategoryCodesMapResponse.getValue());
      predictionIdAndCategoryCodeResponseList.add(predictionIdAndCategoryCodeResponse);
    }
    return predictionIdAndCategoryCodeResponseList;
  }

  public static Map<String, List<CategoryCodeAndNameResponse>> getPredictionIdAndCategoryCodesMap(
      List<PredictionCategoryMapping> predictionCategoryMappingList,
      Map<String, String> categoryCodeAndCategoryNameMap) {
    Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap = new HashMap<>();
    for (PredictionCategoryMapping predictionCategoryMapping : predictionCategoryMappingList) {
      if (predictionIdAndCategoryCodesMap.containsKey(predictionCategoryMapping.getPredictionId())
          && StringUtils.isNotEmpty(categoryCodeAndCategoryNameMap.get(predictionCategoryMapping.getCategoryCode()))) {
        CategoryCodeAndNameResponse categoryCodeAndNameResponse = new CategoryCodeAndNameResponse();
        categoryCodeAndNameResponse.setCategoryCode(predictionCategoryMapping.getCategoryCode());
        categoryCodeAndNameResponse.setCategoryName(
            categoryCodeAndCategoryNameMap.get(predictionCategoryMapping.getCategoryCode()));
        predictionIdAndCategoryCodesMap.get(predictionCategoryMapping.getPredictionId())
            .add(categoryCodeAndNameResponse);
      } else if (StringUtils.isNotEmpty(
          categoryCodeAndCategoryNameMap.get(predictionCategoryMapping.getCategoryCode()))) {
        List<CategoryCodeAndNameResponse> categoryCodeAndNameResponseList = new ArrayList<>();
        CategoryCodeAndNameResponse categoryCodeAndNameResponse = new CategoryCodeAndNameResponse();
        categoryCodeAndNameResponse.setCategoryCode(predictionCategoryMapping.getCategoryCode());
          categoryCodeAndNameResponse.setCategoryName(
              categoryCodeAndCategoryNameMap.get(predictionCategoryMapping.getCategoryCode()));
          categoryCodeAndNameResponseList.add(categoryCodeAndNameResponse);
          predictionIdAndCategoryCodesMap.put(predictionCategoryMapping.getPredictionId(),
              categoryCodeAndNameResponseList);
      }
    }
    return predictionIdAndCategoryCodesMap;
  }

  public static ProductItemCompleteResponse toProductItemCompleteResponse(ProductItem productItem, Product product) {
    ProductItemCompleteResponse productItemCompleteResponse = new ProductItemCompleteResponse();
    productItemCompleteResponse.setProductItemAttributeValueResponses(new ArrayList<>());
    productItemCompleteResponse.setImages(new ArrayList<>());
    String skuCode = productItem.getSkuCode();
    productItem =
        product.getProductItems().stream().filter(productItem1 -> productItem1.getSkuCode().equals(skuCode)).findFirst()
            .get();
    BeanUtils
        .copyProperties(productItem, productItemCompleteResponse, "productItemAttributeValues", "productItemImages");
    setProductItemAttributeValues(productItemCompleteResponse, productItem);
    setProductItemImages(productItemCompleteResponse, productItem);
    ProductResponseWithCategoryAndAttribute productResponse = new ProductResponseWithCategoryAndAttribute();
    setProductDetails(product, productResponse);
    productItemCompleteResponse.setProductResponse(productResponse);
    return productItemCompleteResponse;
  }

  private static void setProductDetails(Product product, ProductResponseWithCategoryAndAttribute productResponse) {
    BeanUtils.copyProperties(product, productResponse, "productCategoryResponses", "productAttributeResponses", "images");
    productResponse.setProductCategoryResponses(
        Optional.ofNullable(product.getProductCategories()).orElse(new ArrayList<>()).stream()
            .map(productCategory -> convertProductCategoryToResponse(productCategory)).collect(Collectors.toList()));
    productResponse.setProductAttributeResponses(
        Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>()).stream()
            .map(productAttribute -> convertProductAttributeToResponse(productAttribute)).collect(Collectors.toList()));
    productResponse.setImages(
      Optional.ofNullable(product.getProductImages()).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(ProductImage::isMarkForDelete))
        .filter(productImage -> !Boolean.TRUE.equals(productImage.getOriginalImage()))
        .map(productImage -> convertProductImageToResponse(productImage)).collect(Collectors.toList()));
  }

  public static Image convertProductImageToResponse(ProductImage productImage) {
    Image image = new Image();
    BeanUtils.copyProperties(productImage, image, "product");
    return image;
  }

  private static void setProductItemAttributeValues(ProductItemCompleteResponse productItemCompleteResponse,
      ProductItem productItem) {
    for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
      if (!productItemAttributeValue.isMarkForDelete()) {
        ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse, "attributeType",
            "allowedAttributeValues", "predefinedAllowedAttributeValues");
        attributeResponse.setAttributeType(productItemAttributeValue.getAttribute().getAttributeType().name());
        productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
        productItemAttributeValueResponse.setValue(productItemAttributeValue.getValue());
        productItemCompleteResponse.getProductItemAttributeValueResponses().add(productItemAttributeValueResponse);
      }
    }
  }

  private static void setProductItemImages(ProductItemCompleteResponse productItemCompleteResponse,
      ProductItem productItem) {
    for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
      if (!productItemImage.isMarkForDelete() && !Boolean.TRUE.equals(productItemImage.getOriginalImage())) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemCompleteResponse.getImages().add(image);
      }
    }
  }

  public static SimpleProductCategoryResponse convertProductCategoryToResponse(ProductCategory productCategory) {
    SimpleProductCategoryResponse response = new SimpleProductCategoryResponse();
    CategoryResponse category = new CategoryResponse();
    BeanUtils.copyProperties(productCategory.getCategory(), category);
    response.setCategoryResponse(category);
    response.setCatalogCode(productCategory.getCategory().getCatalog().getCatalogCode());
    return response;
  }

  public static ProductAttributeResponse convertProductAttributeToResponse(ProductAttribute productAttribute) {
    ProductAttributeResponse response = new ProductAttributeResponse();
    BeanUtils.copyProperties(productAttribute, response, "productAttributeValues", "attribute");
    AttributeResponse attribute = new AttributeResponse();
    BeanUtils.copyProperties(productAttribute.getAttribute(), attribute, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    attribute.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
    response.setAttribute(attribute);
    List<ProductAttributeValueResponse> productAttributeValueResponses = new ArrayList<>();
    for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
      ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
      BeanUtils.copyProperties(productAttributeValue, productAttributeValueResponse, "allowedAttributeValue",
          "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");
      productAttributeValueResponse.setDescriptiveAttributeValueType(
          DescriptiveAttributeValueType.valueOf(productAttributeValue.getDescriptiveAttributeValueType().toString()));
      if (DescriptiveAttributeValueType.NONE.equals(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
        AllowedAttributeValueResponse allowedAttributeValue = new AllowedAttributeValueResponse();
        BeanUtils.copyProperties(productAttributeValue.getAllowedAttributeValue(), allowedAttributeValue);
        productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValue);
      } else {
        PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
            productAttributeValue.getPredefinedAllowedAttributeValue();
        if (Objects.nonNull(predefinedAllowedAttributeValue)) {
          PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
              new PredefinedAllowedAttributeValueResponse();
          BeanUtils.copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueResponse);
          productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
        } else {
          productAttributeValueResponse
              .setDescriptiveAttributeValue(productAttributeValue.getDescriptiveAttributeValue());
        }
      }
      productAttributeValueResponses.add(productAttributeValueResponse);
    }
    response.setProductAttributeValues(productAttributeValueResponses);
    return response;
  }

  public static RestrictedKeywordsResponse toRestrictedKeywordsResponseFromCategoryRestrictedKeyword(
      CategoryRestrictedKeyword categoryRestrictedKeyword) {
    Boolean validateByDs = null;
    String keyword = null;
    if (Objects.nonNull(categoryRestrictedKeyword.getRestrictedKeyword())) {
      validateByDs = categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs();
      keyword = categoryRestrictedKeyword.getRestrictedKeyword().getKeyword();
    }
    return RestrictedKeywordsResponse.builder().keyword(keyword)
        .keywordId(categoryRestrictedKeyword.getRestrictedKeywordId()).selected(null)
        .action(categoryRestrictedKeyword.getAction()).message(categoryRestrictedKeyword.getMessage())
        .type(categoryRestrictedKeyword.getType())
        .validateByDs(validateByDs)
        .destinationCategory(categoryRestrictedKeyword.getDestinationCategory()).build();
  }

  public static RestrictedKeywordsResponse toRestrictedKeywordsResponseFromRestrictedKeyword(
      RestrictedKeyword restrictedKeyword) {
    return RestrictedKeywordsResponse.builder().keyword(restrictedKeyword.getKeyword())
        .keywordId(restrictedKeyword.getId()).validateByDs(restrictedKeyword.getValidateByDs())
        .selected(null).build();
  }

  public static RestrictedKeywordsResponse toRestrictedKeywordsResponseWithValidationFlagFromRestrictedKeyword(
    RestrictedKeyword restrictedKeyword) {
    return RestrictedKeywordsResponse.builder().keyword(restrictedKeyword.getKeyword())
      .keywordId(restrictedKeyword.getId()).validateOnUi(restrictedKeyword.getValidateOnUi())
      .validateByDs(restrictedKeyword.getValidateByDs()).build();
  }

  public static CategoryRestrictedKeyword toCategoryRestrictedKeyword(String storeId,
      RestrictedKeyword restrictedKeyword, Category category, CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO) {
    CategoryRestrictedKeyword categoryRestrictedKeyword = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword.setCategory(category);
    categoryRestrictedKeyword.setCategoryCode(category.getCategoryCode());
    categoryRestrictedKeyword.setRestrictedKeyword(restrictedKeyword);
    categoryRestrictedKeyword.setStoreId(storeId);
    categoryRestrictedKeyword.setMarkForDelete(false);
    categoryRestrictedKeyword.setType(categoryKeywordsUpdateDTO.getType());
    categoryRestrictedKeyword.setAction(categoryKeywordsUpdateDTO.getAction());
    categoryRestrictedKeyword.setMessage(categoryKeywordsUpdateDTO.getMessage());
    categoryRestrictedKeyword.setDestinationCategory(categoryKeywordsUpdateDTO.getDestinationCategory());
    return categoryRestrictedKeyword;
  }

  public static List<RestrictedKeyword> toRestrictedKeywords(List<String> newKeywords,
    String storeId, Map<String, Boolean> newKeywordsAddedToCategoryValidateDsMap) {
    List<RestrictedKeyword> restrictedKeywordList = new ArrayList<>();
    for (String newKeyword : newKeywords) {
      RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
      restrictedKeyword.setStoreId(storeId);
      restrictedKeyword.setMarkForDelete(false);
      restrictedKeyword.setKeyword(newKeyword);
      restrictedKeyword.setValidateByDs(
        newKeywordsAddedToCategoryValidateDsMap.getOrDefault(newKeyword, null));
      restrictedKeywordList.add(restrictedKeyword);
    }
    return restrictedKeywordList;
  }

  public static Product getProductToUpdateFromProductAndItemImageRequest(
      ProductAndItemImageRequest productAndItemImageRequest, Product savedProduct, Boolean setDgLevel) {
    savedProduct.getProductImages().addAll(productAndItemImageRequest.getProductImages().stream()
        .map(image -> getProductImageFromImage(image, savedProduct)).collect(Collectors.toList()));
    Map<String, List<Image>> productItemImageMap = productAndItemImageRequest.getProductItemImages().stream()
        .collect(Collectors.toMap(ProductItemImageRequest::getSkuCode, ProductItemImageRequest::getItemImages));
    for (ProductItem productItem : savedProduct.getProductItems()) {
      if (setDgLevel) {
        productItem.setDangerousGoodsLevel(savedProduct.getProductCategories().get(0).getCategory().getDangerousGoodsLevel());
      }
      productItem.getProductItemImages().addAll(productItemImageMap.get(productItem.getSkuCode()).stream()
          .map(image -> getProductItemImageFromImage(image, productItem)).collect(Collectors.toList()));
    }
    return savedProduct;
  }

  private static ProductItemImage getProductItemImageFromImage(Image image, ProductItem productItem) {
    ProductItemImage productItemImage = new ProductItemImage();
    BeanUtils.copyProperties(image, productItemImage, "productItem");
    productItemImage.setProductItem(productItem);
    productItemImage.setProductItemId(productItem.getId());
    return productItemImage;
  }

  private static ProductImage getProductImageFromImage(Image image, Product product) {
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(image, productImage, "product");
    productImage.setProduct(product);
    return productImage;
  }

  public static ProductSalesCategoryMappingResponse getProductSalesCategoryMappingResponse(String productCode,
      ProductSalesCategoryMapping productSalesCategoryMapping) {
    ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse = new ProductSalesCategoryMappingResponse();
    productSalesCategoryMappingResponse.setProductCode(productCode);
    if (Objects.nonNull(productSalesCategoryMapping)) {
      BeanUtils.copyProperties(productSalesCategoryMapping, productSalesCategoryMappingResponse);
    }
    return productSalesCategoryMappingResponse;
  }

  public static WholesalePriceConfiguration getWholesalePriceConfiguration(String storeId, WholesaleMappingDTO wholesaleMapping, Category category) throws Exception {
    WholesalePriceConfiguration wholesalePriceConfiguration = new WholesalePriceConfiguration();
    ObjectMapper mapper = new ObjectMapper();
    String wholesaleConfigValue = mapper.writeValueAsString(wholesaleMapping.getWholesaleConfig());
    wholesalePriceConfiguration.setWholesaleConfigs(wholesaleConfigValue);
    wholesalePriceConfiguration.setConfigurationType(wholesaleMapping.getConfigurationType());
    wholesalePriceConfiguration.setStoreId(storeId);
    wholesalePriceConfiguration.setMarkForDelete(false);
    wholesalePriceConfiguration.setCategoryId(category.getId());
    return wholesalePriceConfiguration;
  }

  public static BrandResponse getBrandResponseByBrandWip(BrandWip brandWip) {
    BrandResponse brandResponse = new BrandResponse();
    BeanUtils.copyProperties(brandWip, brandResponse, "brandDescription", "brandLogoPath");
    String brandCode =
        StringUtils.isBlank(brandWip.getBrandCode()) ? brandWip.getBrandRequestCode() : brandWip.getBrandCode();
    brandResponse.setBrandCode(brandCode);
    brandResponse.setBrandDescription(
        Objects.isNull(brandWip.getBrandDescription()) ? null : new String(brandWip.getBrandDescription()));
    if (StringUtils.isNotEmpty(brandWip.getBrandLogoPath())) {
      brandResponse.setBrandLogoPath(
          Constants.PATH_SEPARATOR + brandCode + Constants.PATH_SEPARATOR + brandWip.getBrandLogoPath());
    }
    return brandResponse;
  }

  public static AttributeDomainEventModel getMasterAttributeDomainEventModel(Attribute attribute) {
    AttributeDomainEventModel masterAttributeDomainEventModel = new AttributeDomainEventModel();
    BeanUtils.copyProperties(attribute, masterAttributeDomainEventModel, "attributeType");
    masterAttributeDomainEventModel.setAttributeType(attribute.getAttributeType().toString());
    return masterAttributeDomainEventModel;
  }

  public static ProductCreationFailureDomainEventModel toProductCreationFailureDomainEventModel(
      ProductRequest productRequest) {
    if (Objects.isNull(productRequest)) {
      return new ProductCreationFailureDomainEventModel();
    }
    ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel =
        ProductCreationFailureDomainEventModel.builder().storeId(productRequest.getStoreId())
            .productCode(productRequest.getProductCode()).productName(productRequest.getName())
            .length(productRequest.getLength()).width(productRequest.getWidth()).height(productRequest.getHeight())
            .shippingWeight(productRequest.getShippingWeight()).brand(productRequest.getBrand())
            .brandCode(productRequest.getBrandCode()).brandApprovalStatus(productRequest.getBrandApprovalStatus())
            .uniqueSellingPoint(productRequest.getUniqueSellingPoint()).uom(productRequest.getUom())
            .url(productRequest.getUrl()).activated(productRequest.isActivated()).viewable(productRequest.isViewable())
            .promoSKU(productRequest.isPromoSKU()).isMarginExceed(productRequest.isMarginExceed())
            .forReview(productRequest.isForReview()).postLive(productRequest.isPostLive())
            .reviewPending(productRequest.isReviewPending()).createdMerchant(productRequest.getCreatedMerchant())
            .createdDate(productRequest.getCreatedDate()).createdBy(productRequest.getCreatedBy())
            .markForDelete(productRequest.isMarkForDelete())
            .productCategories(toProductCategoryDomainEventModel(productRequest.getProductCategories()))
            .productAttributes(toProductAttributeDomainEventModels(productRequest.getProductAttributes()))
            .images(toImageDomainEventModels(productRequest.getImages()))
            .productItems(toProductItemDomainEventModels(productRequest.getProductItems())).build();
    if (Objects.nonNull(productRequest.getDescription())) {
      productCreationFailureDomainEventModel.setDescription(new String(productRequest.getDescription()));
    }
    return productCreationFailureDomainEventModel;
  }

  private static List<ProductCategoryDomainEventModel> toProductCategoryDomainEventModel(
      List<ProductCategoryRequest> productCategoryRequests) {
    List<ProductCategoryDomainEventModel> productCategories = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productCategoryRequests)) {
      for (ProductCategoryRequest productCategoryRequest : productCategoryRequests) {
        ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
        if (Objects.nonNull(productCategoryRequest.getCategory())) {
          CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
          BeanUtils.copyProperties(productCategoryRequest.getCategory(), categoryDomainEventModel);
          productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
        }
        productCategories.add(productCategoryDomainEventModel);
      }
    }
    return productCategories;
  }

  private static List<ProductAttributeDomainEventModel> toProductAttributeDomainEventModels(
      List<ProductAttributeRequest> productAttributeRequests) {
    List<ProductAttributeDomainEventModel> productAttributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productAttributeRequests)) {
      for (ProductAttributeRequest productAttributeRequest : productAttributeRequests) {
        ProductAttributeDomainEventModel productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
        productAttributeDomainEventModel.setSequence(productAttributeRequest.getSequence());
        productAttributeDomainEventModel.setProductAttributeName(productAttributeRequest.getProductAttributeName());
        productAttributeDomainEventModel.setOwnByProductItem(productAttributeRequest.isOwnByProductItem());
        if (Objects.nonNull(productAttributeRequest.getAttribute())) {
          AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
          attributeDomainEventModel.setId(productAttributeRequest.getAttribute().getId());
          attributeDomainEventModel.setAttributeCode(productAttributeRequest.getAttribute().getAttributeCode());
          attributeDomainEventModel.setName(productAttributeRequest.getAttribute().getName());
          attributeDomainEventModel.setAttributeType(String.valueOf(productAttributeRequest.getAttribute().getAttributeType()));
          attributeDomainEventModel.setSkuValue(productAttributeRequest.getAttribute().isSkuValue());
          productAttributeDomainEventModel.setAttribute(attributeDomainEventModel);
        }
        productAttributeDomainEventModel.setProductAttributeValues(
            toProductAttributeValueDomainEventModels(productAttributeRequest.getProductAttributeValues()));
        productAttributes.add(productAttributeDomainEventModel);
      }
    }
    return productAttributes;
  }

  private static List<ProductAttributeValueDomainEventModel> toProductAttributeValueDomainEventModels(
      List<ProductAttributeValueRequest> productAttributeValueRequests) {
    List<ProductAttributeValueDomainEventModel> productAttributeValueDomainEventModels = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productAttributeValueRequests)) {
      for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeValueRequests) {
        ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel = new ProductAttributeValueDomainEventModel();
        productAttributeValueDomainEventModel.setDescriptiveAttributeValueType(String.valueOf(productAttributeValueRequest.getDescriptiveAttributeValueType()));
        productAttributeValueDomainEventModel.setDescriptiveAttributeValue(productAttributeValueRequest.getDescriptiveAttributeValue());
        if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
          AllowedAttributeValueDomainEventModel allowedAttributeValueDomainEventModel = new AllowedAttributeValueDomainEventModel();
          BeanUtils.copyProperties(productAttributeValueRequest.getAllowedAttributeValue(),
              allowedAttributeValueDomainEventModel);
          productAttributeValueDomainEventModel.setAllowedAttributeValue(allowedAttributeValueDomainEventModel);
        }
        if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
          PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValueDomainEventModel = new PredefinedAllowedAttributeValueDomainEventModel();
          BeanUtils.copyProperties(productAttributeValueRequest.getPredefinedAllowedAttributeValue(),
              predefinedAllowedAttributeValueDomainEventModel);
          productAttributeValueDomainEventModel.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
        }
        productAttributeValueDomainEventModels.add(productAttributeValueDomainEventModel);
      }
    }
    return productAttributeValueDomainEventModels;
  }

  private static List<ImageDomainEventModel> toImageDomainEventModels(List<Image> imageRequests) {
    List<ImageDomainEventModel> imageDomainEventModels = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(imageRequests)) {
      for (Image image : imageRequests) {
        ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
        imageDomainEventModel.setLocationPath(image.getLocationPath());
        imageDomainEventModel.setMainImage(image.isMainImages());
        imageDomainEventModel.setSequence(image.getSequence());
        imageDomainEventModels.add(imageDomainEventModel);
      }
    }
    return imageDomainEventModels;
  }

  private static List<ProductCreationFailureItemDomainEventModel> toProductItemDomainEventModels(
      List<ProductItemRequest> productItemRequests) {
    List<ProductCreationFailureItemDomainEventModel> productItemDomainEventModels = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemRequests)) {
      for (ProductItemRequest productItemRequest : productItemRequests) {
        ProductCreationFailureItemDomainEventModel productItemDomainEventModel = new ProductCreationFailureItemDomainEventModel();
        productItemDomainEventModel.setGeneratedItemName(productItemRequest.getGeneratedItemName());
        productItemDomainEventModel.setUpcCode(productItemRequest.getUpcCode());
        productItemDomainEventModel.setSkuCode(productItemRequest.getSkuCode());
        productItemDomainEventModel.setActivated(productItemRequest.isActivated());
        productItemDomainEventModel.setViewable(productItemRequest.isViewable());
        productItemDomainEventModel.setDangerousGoodsLevel(productItemRequest.getDangerousGoodsLevel());
        productItemDomainEventModel.setContentChanged(productItemRequest.isContentChanged());
        productItemDomainEventModel.setInternalUpdate(productItemRequest.isInternalUpdate());
        productItemDomainEventModel.setSourceItemCode(productItemRequest.getSourceItemCode());
        productItemDomainEventModel.setImages(toImageDomainEventModels(productItemRequest.getImages()));
        productItemDomainEventModel.setProductItemAttributeValues(
            toProductItemAttributeValueDomainEventModels(productItemRequest.getProductItemAttributeValues()));
        productItemDomainEventModels.add(productItemDomainEventModel);
      }
    }
    return productItemDomainEventModels;
  }

  private static List<ProductItemAttributeValueDomainEventModel> toProductItemAttributeValueDomainEventModels(
      List<ProductItemAttributeValueRequest> productItemAttributeValueRequests) {
    List<ProductItemAttributeValueDomainEventModel> productItemAttributeValueDomainEventModels = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemAttributeValueRequests)) {
      for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemAttributeValueRequests) {
        ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel = new ProductItemAttributeValueDomainEventModel();
        productItemAttributeValueDomainEventModel.setValue(productItemAttributeValueRequest.getValue());
        if (Objects.nonNull(productItemAttributeValueRequest.getAttribute())) {
          AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
          attributeDomainEventModel.setId(productItemAttributeValueRequest.getAttribute().getId());
          attributeDomainEventModel.setAttributeCode(productItemAttributeValueRequest.getAttribute().getAttributeCode());
          attributeDomainEventModel.setName(productItemAttributeValueRequest.getAttribute().getName());
          attributeDomainEventModel.setAttributeType(String.valueOf(productItemAttributeValueRequest.getAttribute().getAttributeType()));
          attributeDomainEventModel.setSkuValue(productItemAttributeValueRequest.getAttribute().isSkuValue());
          productItemAttributeValueDomainEventModel.setAttribute(attributeDomainEventModel);
        }
        productItemAttributeValueDomainEventModels.add(productItemAttributeValueDomainEventModel);
      }
    }
    return productItemAttributeValueDomainEventModels;
  }

  public static void regenerateCopyToAllVariantImages(Product product, List<Image> copyToAllVariantImages,
      boolean needCorrection) {
    Map<String, ProductItemImage> locationPathProductItemImageMap =
        toLocationPathProductItemImageMap(product, copyToAllVariantImages);
    for (ProductItem productItem : product.getProductItems()) {
      copyToAllVariantImages.stream().filter(image -> Objects.nonNull(image.getHashCode())).filter(
          image -> productItem.getProductItemImages().stream().map(ProductItemImage::getLocationPath)
              .noneMatch(locationPath -> StringUtils.equals(image.getLocationPath(), locationPath)))
          .map(image -> toProductItemNewImageCopyToAllVariants(image, productItem, needCorrection)).limit(
          Constants.MAX_IMAGE_PER_VARIANT - productItem.getProductItemImages().stream().filter(ProductItemImage::isActive)
              .filter(productItemImage -> !productItemImage.isMarkForDelete()).count())
          .collect(Collectors.toCollection(productItem::getProductItemImages));

      copyToAllVariantImages.stream().filter(image -> Objects.isNull(image.getHashCode())).filter(
          image -> productItem.getProductItemImages().stream().map(ProductItemImage::getLocationPath)
              .noneMatch(locationPath -> StringUtils.equals(image.getLocationPath(), locationPath))).map(
          image -> toProductItemExistingImageCopyToAllVariants(image, productItem, locationPathProductItemImageMap))
          .filter(Objects::nonNull)
          .limit(Constants.MAX_IMAGE_PER_VARIANT - productItem.getProductItemImages().stream().filter(ProductItemImage::isActive)
              .filter(productItemImage -> !productItemImage.isMarkForDelete()).count())
          .collect(Collectors.toCollection(productItem::getProductItemImages));
    }
  }

  private static Map<String, ProductItemImage> toLocationPathProductItemImageMap(Product product,
      List<Image> copyToAllVariantImages) {
    Map<String, ProductItemImage> locationPathProductItemImageMap = new HashMap<>();
    for (Image image : copyToAllVariantImages) {
      if (Objects.isNull(image.getHashCode())) {
        for (ProductItem productItem : product.getProductItems()) {
          for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
            if (StringUtils.equals(image.getLocationPath(), productItemImage.getLocationPath()) && Objects
                .isNull(image.getHashCode())) {
              locationPathProductItemImageMap.put(image.getLocationPath(), productItemImage);
              break;
            }
          }
        }
      }
    }
    return locationPathProductItemImageMap;
  }

  public static void regenerateAddNewProductItemImage(Product product,
      List<ProductItemImageRequest> newProductItemImages, ProductPublishUpdateDTO productPublishUpdateDTO,
      boolean uniqueMainImageAtItemLevel, boolean relaxActiveImageCheckInNRImageUpdate, boolean needCorrection) {
    List<ProductItemImage> newItemImages;
    Map<String, List<Image>> productItemImageMap = newProductItemImages.stream()
        .collect(Collectors.toMap(ProductItemImageRequest::getSkuCode, ProductItemImageRequest::getItemImages));
    for (ProductItem productItem : product.getProductItems()) {
      if (productItemImageMap.containsKey(productItem.getSkuCode())) {
        Set<String> uniqueImages = productItem.getProductItemImages().stream()
            .map(ProductItemImage::getLocationPath).collect(Collectors.toSet());
        newItemImages = productItemImageMap.get(productItem.getSkuCode()).stream()
            .filter(image -> !isDuplicateImage(uniqueImages, image.getLocationPath()))
            .map(image -> toProductItemImage(image, productItem)).collect(Collectors.toList());
        if (CommonUtil.itemLevelImagesUpdated(newItemImages, false)) {
          productPublishUpdateDTO.getUpdatedItemSkuCodes().add(productItem.getSkuCode());
        }
        if (uniqueMainImageAtItemLevel) {
          checkAndUpdateMainImageInCaseOfMainImageInNewlyAddedImages(newItemImages, productItem,
              relaxActiveImageCheckInNRImageUpdate, needCorrection);
        }
        productItem.getProductItemImages().addAll(newItemImages);
      }
    }
  }

  private static void checkAndUpdateMainImageInCaseOfMainImageInNewlyAddedImages(List<ProductItemImage> newItemImages,
      ProductItem productItem, boolean relaxActiveImageCheckInNRImageUpdate, boolean needCorrection) {
    // Check if any newly added image is marked as main image
    boolean isMainImagePresentInNewlyAddedImages =
        newItemImages.stream().anyMatch(image -> image.isMainImages() && image.isActive());
    // Relax Active check in NR as it will never be true
    if (needCorrection && relaxActiveImageCheckInNRImageUpdate) {
      isMainImagePresentInNewlyAddedImages =
          newItemImages.stream().anyMatch(ProductItemImage::isMainImages);
    }

    // If a main image is present among the newly added images, update existing images main image to false
    if (isMainImagePresentInNewlyAddedImages) {
      log.info("Removing main images for existing images for item : {}", productItem.getSourceItemCode());
      productItem.getProductItemImages()
          .stream()
          .filter(ProductItemImage::isMainImages)
          .forEach(image -> image.setMainImages(false));
    }
  }

  private static boolean isDuplicateImage(Set<String> uniqueImages, String locationPath) {
    if (uniqueImages.contains(locationPath)) {
      return true;
    } else {
      uniqueImages.add(locationPath);
      return false;
    }
  }

  public static void regenerateProductItemImageGotUpdated(Product product,
      List<ProductItemImageRequest> updateProductItemImages, ProductPublishUpdateDTO productPublishUpdateDTO) {
    List<ProductItemImage> updatedItemImages;
    Map<String, List<Image>> productItemImageMap = updateProductItemImages.stream()
        .collect(Collectors.toMap(ProductItemImageRequest::getSkuCode, ProductItemImageRequest::getItemImages));
    for (ProductItem productItem : product.getProductItems()) {
      if (productItemImageMap.containsKey(productItem.getSkuCode())) {
        List<ProductItemImage> resizedItemImageDeleted = new ArrayList<>();
        updatedItemImages = productItemImageMap.get(productItem.getSkuCode()).stream()
            .map(image -> getProductItemImageFromImage(image, productItem)).collect(Collectors.toList());
        if (CommonUtil.itemLevelImagesUpdated(updatedItemImages, false)) {
          productPublishUpdateDTO.getUpdatedItemSkuCodes().add(productItem.getSkuCode());
        }
        for (Image image : productItemImageMap.get(productItem.getSkuCode())) {
          if (image.isMarkForDelete()) {
            resizedItemImageDeleted.addAll(getProductItemImageDeleteResizeFromImage(image, productItem));
          }
        }
        updatedItemImages.addAll(resizedItemImageDeleted);
        setUpdateProductItemImages(updatedItemImages, productItem.getProductItemImages());
      }
    }
  }

  private static List<ProductItemImage> getProductItemImageDeleteResizeFromImage(Image image, ProductItem productItem) {
    List<ProductItemImage> newProductItemImageList = new ArrayList<>();
    for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
      if (StringUtils.contains(productItemImage.getLocationPath(), StringUtils
          .substringBefore(StringUtils.substringAfterLast(image.getLocationPath(), Constants.PATH_SEPARATOR),
              Constants.DOT))) {
        ProductItemImage newProductItemImage = new ProductItemImage();
        newProductItemImage.setLocationPath(productItemImage.getLocationPath());
        newProductItemImage.setSequence(productItemImage.getSequence());
        newProductItemImage.setMainImages(productItemImage.isMainImages());
        newProductItemImage.setMarkForDelete(Boolean.TRUE);
        newProductItemImage.setProductItem(productItem);
        newProductItemImageList.add(newProductItemImage);
      }
    }
    return newProductItemImageList;
  }

  private static void setUpdateProductItemImages(List<ProductItemImage> updatedItemImages,
      List<ProductItemImage> savedProductItemImages) {
    for (ProductItemImage updatedItemImage : updatedItemImages) {
      for (ProductItemImage savedProductItemImage : savedProductItemImages) {
        if (StringUtils.contains(savedProductItemImage.getLocationPath(), StringUtils.substringBefore(
            StringUtils.substringAfterLast(updatedItemImage.getLocationPath(), Constants.PATH_SEPARATOR),
            Constants.DOT))) {
          savedProductItemImage.setMainImages(updatedItemImage.isMainImages());
          savedProductItemImage.setSequence(updatedItemImage.getSequence());
          savedProductItemImage.setMarkForDelete(updatedItemImage.isMarkForDelete());
        }
      }
    }
  }

  private static ProductItemImage toProductItemImage(Image image, ProductItem productItem) {
    ProductItemImage productItemImage = new ProductItemImage();
    BeanUtils.copyProperties(image, productItemImage, "productItem");
    productItemImage.setProductItem(productItem);
    productItemImage.setEdited(image.isEdited());
    productItemImage.setRevised(image.isRevised());
    return productItemImage;
  }

  private static ProductItemImage toProductItemNewImageCopyToAllVariants(Image image, ProductItem productItem,
      boolean needCorrection) {
    ProductItemImage productItemImage = new ProductItemImage();
    BeanUtils.copyProperties(image, productItemImage, "productItem");
    productItemImage.setProductItem(productItem);
    productItemImage.setSequence(
        productItem.getProductItemImages().stream().max(Comparator.comparingInt(ProductItemImage::getSequence))
            .orElse(new ProductItemImage(0)).getSequence() + 1);
    if (needCorrection) {
      productItemImage.setRevised(true);
    } else {
      productItemImage.setEdited(true);
    }
    return productItemImage;
  }

  private static ProductItemImage toProductItemExistingImageCopyToAllVariants(Image image, ProductItem productItem,
      Map<String, ProductItemImage> locationPathProductItemImageMap) {
    ProductItemImage productItemImage = locationPathProductItemImageMap.get(image.getLocationPath());
    ProductItemImage newProductItemImage = null;
    if (Objects.nonNull(productItemImage)) {
      newProductItemImage = new ProductItemImage();
      newProductItemImage.setLocationPath(productItemImage.getLocationPath());
      newProductItemImage.setHashCode(productItemImage.getHashCode());
      newProductItemImage.setOriginalImage(productItemImage.getOriginalImage());
      newProductItemImage.setStoreId(image.getStoreId());
      newProductItemImage.setProductItem(productItem);
      newProductItemImage.setSequence(
          productItem.getProductItemImages().stream().max(Comparator.comparingInt(ProductItemImage::getSequence))
              .orElse(new ProductItemImage(0)).getSequence() + 1);
      newProductItemImage.setCreatedBy(image.getCreatedBy());
      newProductItemImage.setCreatedDate(new Date());
      newProductItemImage.setUpdatedBy(image.getUpdatedBy());
      newProductItemImage.setUpdatedDate(new Date());
      newProductItemImage.setMarkForDelete(Boolean.FALSE);
      newProductItemImage.setEdited(productItemImage.isEdited());
      newProductItemImage.setRevised(productItemImage.isRevised());
      newProductItemImage.setActive(Boolean.TRUE);
      newProductItemImage.setMainImages(Boolean.FALSE);
    }
    return newProductItemImage;
  }

  public static void regenerateProductImages(Product product, List<Image> copyToAllVariantImages,
      List<ProductItemImageRequest> newProductItemImages, boolean needCorrection,
      Set<String> commonImageLocationPathSet) {
    if (CollectionUtils.isNotEmpty(newProductItemImages)) {
      for (ProductItemImageRequest productItemImageRequest : newProductItemImages) {
        productItemImageRequest.getItemImages().stream().filter(
            image -> product.getProductImages().stream().map(ProductImage::getLocationPath)
                .noneMatch(locationPath -> StringUtils.equals(locationPath, image.getLocationPath())))
            .map(image -> toProductImage(image, product, commonImageLocationPathSet))
            .collect(Collectors.toCollection(product::getProductImages));
      }
      if (needCorrection) {
        product.setRevised(Boolean.TRUE);
      } else {
        product.setEdited(Boolean.TRUE);
      }
      product.setReviewPending(Boolean.TRUE);
    }
    if (CollectionUtils.isNotEmpty(copyToAllVariantImages)) {
      copyToAllVariantImages.stream().filter(image -> Objects.nonNull(image.getHashCode())).filter(
          image -> product.getProductImages().stream().map(ProductImage::getLocationPath)
              .noneMatch(locationPath -> StringUtils.equals(image.getLocationPath(), locationPath)))
          .map(image -> toProductImage(image, product, commonImageLocationPathSet))
          .collect(Collectors.toCollection(product::getProductImages));
      for (Image image : copyToAllVariantImages) {
        if (Objects.nonNull(image.getHashCode())) {
          if (needCorrection) {
            product.setRevised(Boolean.TRUE);
          } else {
            product.setEdited(Boolean.TRUE);
          }
          product.setReviewPending(Boolean.TRUE);
          break;
        }
      }
    }
  }

  private static ProductImage toProductImage(Image image, Product product, Set<String> commonImageLocationPathSet) {
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(image, productImage, "product", "isMainImages");
    productImage.setEdited(image.isEdited());
    productImage.setRevised(image.isRevised());
    productImage.setProduct(product);
    if (productImage.isCommonImage()) {
      commonImageLocationPathSet.add(productImage.getLocationPath());
    }
    return productImage;
  }

  public static void regenerateProductImagesUpdate(Product product,
      List<ProductItemImageRequest> updateProductItemImages, boolean activeSwitch, ProductPublishUpdateDTO productPublishUpdateDTO,
      Set<String> commonImageLocationPathSet) {
    List<ProductImage> deletedProductImages = new ArrayList<>();
    List<ProductImage> deletedResizedProductImages;

    if (CollectionUtils.isNotEmpty(updateProductItemImages)) {
      for (ProductItemImageRequest productItemImageRequest : updateProductItemImages) {
        List<ProductImage> deleteProductItemImages =
            productItemImageRequest.getItemImages().stream().filter(Image::isMarkForDelete)
                .map(image -> toUpdateProductImage(image, product)).collect(Collectors.toList());
        deletedResizedProductImages = productItemImageRequest.getItemImages().stream().filter(Image::isMarkForDelete)
            .filter(image -> filterResizeAndOriginalLocationPath(image, product.getProductImages()))
            .map(image -> getProductImageDeleteResizeFromImage(image, product)).collect(Collectors.toList());
        deletedProductImages.addAll(deletedResizedProductImages);
        deletedProductImages.addAll(deleteProductItemImages);
      }
    }

    Set<String> existingLocationPaths = new HashSet<>();
    for (ProductItem productItem : product.getProductItems()) {
      if (!activeSwitch) {
        existingLocationPaths.addAll(
            productItem.getProductItemImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
                .map(productItemImage -> productItemImage.getLocationPath()).collect(Collectors.toSet()));
      } else {
        for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
          if (!productItemImage.isMarkForDelete()) {
            if (!productItemImage.isActive()) {
              log.info("Checking for the file location : {} for product {}", productItemImage.getLocationPath(),
                  product.getProductCode());
              boolean finalImageFileExists = fileStorageService.isFinalImageFileExist(productItemImage.getLocationPath());
              if (!finalImageFileExists) {
                if (!productItemImage.isCommonImage()) {
                  productPublishUpdateDTO.getUpdatedItemSkuCodes().add(productItem.getSkuCode());
                }
                productItemImage.setActive(true);
              }
            }
            existingLocationPaths.add(productItemImage.getLocationPath());
          }
        }
      }
    }

    if (activeSwitch) {
      for (ProductImage productImage : product.getProductImages()) {
        if (!productImage.isMarkForDelete() && !productImage.isActive()) {
          boolean finalImageFileExists = fileStorageService.isFinalImageFileExist(productImage.getLocationPath());
          if (!finalImageFileExists) {
            if (productImage.isCommonImage()) {
              commonImageLocationPathSet.add(productImage.getLocationPath());
            }
            productImage.setActive(true);
          }
        }
      }
    }

    if (CollectionUtils.isNotEmpty(deletedProductImages)) {
      List<ProductImage> deletedProductImagesFinal = new ArrayList<>();
      for (ProductImage productImage : deletedProductImages) {
        if (!existingLocationPaths.contains(productImage.getLocationPath())) {
          deletedProductImagesFinal.add(productImage);
        }
      }
      updateProductImages(deletedProductImagesFinal, product.getProductImages(), commonImageLocationPathSet);
    }
  }

  private static ProductImage getProductImageDeleteResizeFromImage(Image image, Product product) {
    ProductImage newProductImage = new ProductImage();
    for (ProductImage productImage : product.getProductImages()) {
      if (StringUtils.contains(productImage.getLocationPath(), StringUtils
          .substringBefore(StringUtils.substringAfterLast(image.getLocationPath(), Constants.PATH_SEPARATOR),
              Constants.DOT))) {
        newProductImage.setLocationPath(productImage.getLocationPath());
        newProductImage.setSequence(productImage.getSequence());
        newProductImage.setMainImages(productImage.isMainImages());
        newProductImage.setMarkForDelete(Boolean.TRUE);
      }
    }
    return newProductImage;
  }

  private static boolean filterResizeAndOriginalLocationPath(Image image, List<ProductImage> productImages) {
    for (ProductImage productImage : productImages) {
      if (StringUtils.contains(productImage.getLocationPath(), StringUtils
          .substringBefore(StringUtils.substringAfterLast(image.getLocationPath(), Constants.PATH_SEPARATOR),
              Constants.DOT))) {
        return true;
      }
    }
    return false;
  }

  private static void updateProductImages(List<ProductImage> deletedProductImages, List<ProductImage> productImages,
      Set<String> commonImageLocationPathSet) {
    for (ProductImage productImage : productImages) {
      for (ProductImage deletedImage : deletedProductImages) {
        if (StringUtils.contains(productImage.getLocationPath(), StringUtils
            .substringBefore(StringUtils.substringAfterLast(deletedImage.getLocationPath(), Constants.PATH_SEPARATOR),
                Constants.DOT))) {
          productImage.setMarkForDelete(Boolean.TRUE);
          if (productImage.isCommonImage()) {
            commonImageLocationPathSet.add(productImage.getLocationPath());
          }
        }
      }
    }
  }

  private static ProductImage toUpdateProductImage(Image image, Product product) {
    ProductImage productImage = new ProductImage();
    for (ProductImage productItemImage : product.getProductImages()) {
      if (StringUtils.equals(productItemImage.getLocationPath(), image.getLocationPath())) {
        BeanUtils.copyProperties(productItemImage, productImage, "product");
      }
    }
    return productImage;
  }

  public static List<BrandSummaryResponse> convertToBrandSummaryResponse(List<Brand> brands) {
    List<BrandSummaryResponse> brandSummaryResponses = new ArrayList<>();
    for (Brand brand : brands) {
      BrandSummaryResponse brandSummaryResponse =
          new BrandSummaryResponse(brand.getBrandCode(), brand.getBrandWipId(), brand.getBrandName(),
              brand.isValidBrand());
      brandSummaryResponses.add(brandSummaryResponse);
    }
    return brandSummaryResponses;
  }

  public static Map<String, List<ProductImage>> imageNameToListOfImageMap(List<ProductImage> productImages) {
    Map<String, List<ProductImage>> imageNameToListOfImageMap = new HashMap<>();
    for (ProductImage productImage : productImages) {
      String[] imageFilename = productImage.getLocationPath().split(File.separator);
      String imageName = FilenameUtils.removeExtension(imageFilename[imageFilename.length - 1]);
      if (imageNameToListOfImageMap.containsKey(imageName)) {
        imageNameToListOfImageMap.get(imageName).add(productImage);
      } else {
        List<ProductImage> productImageList = new ArrayList<>();
        productImageList.add(productImage);
        imageNameToListOfImageMap.put(imageName, productImageList);
      }
    }
    return imageNameToListOfImageMap;
  }

  public static Map<String, List<ProductItemImage>> imageNameToListOfItemImageMap(
      List<ProductItemImage> productItemImages) {
    Map<String, List<ProductItemImage>> imageNameToListOfItemImageMap = new HashMap<>();
    for (ProductItemImage productItemImage : productItemImages) {
      String imageName = imageNameFromLocationPath(productItemImage.getLocationPath());
      if (imageNameToListOfItemImageMap.containsKey(imageName)) {
        imageNameToListOfItemImageMap.get(imageName).add(productItemImage);
      } else {
        List<ProductItemImage> productItemImageList = new ArrayList<>();
        productItemImageList.add(productItemImage);
        imageNameToListOfItemImageMap.put(imageName, productItemImageList);
      }
    }
    return imageNameToListOfItemImageMap;
  }

  public static String imageNameFromLocationPath(String locationPath) {
    String[] imageFilename = locationPath.split(File.separator);
    return FilenameUtils.removeExtension(imageFilename[imageFilename.length - 1]);
  }

  public static List<Category> toUpdatedCategory(List<Category> categories, OriginalSalesCategory originalSalesCategory,
      String userName) {
    List<Category> categoryList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(categories)) {
      for (Category category : categories) {
        category.setOriginalSalesCategory(originalSalesCategory);
        category.setOscUpdatedBy(userName);
        category.setOscUpdatedDate(new Date());
        categoryList.add(category);
      }
    }
    return categoryList;
  }

  public static OriginalSalesCategoryResponse toOriginalSalesCategoryResponse(OriginalSalesCategory originalSalesCategory) {
    OriginalSalesCategoryResponse originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
    BeanUtils.copyProperties(originalSalesCategory, originalSalesCategoryResponse, "masterCategories");
    List<CategoryResponse> masterCategories = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(originalSalesCategory.getMasterCategories())) {
      for (Category category : originalSalesCategory.getMasterCategories()) {
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(category, categoryResponse);
        masterCategories.add(categoryResponse);
      }
    }
    originalSalesCategoryResponse.setMasterCategories(masterCategories);
    return originalSalesCategoryResponse;
  }

  public static MatrixAttributeExtractionRequest toMatrixAttributeExtractionRequest(Product product,
      ProductAttributeExtractionModel productAttributeExtractionModel) {
    return MatrixAttributeExtractionRequest.builder().product_code(product.getProductCode())
        .cn_code(productAttributeExtractionModel.getCnCategoryCode()).title(product.getName()).brand(product.getBrand())
        .product_usp(product.getUniqueSellingPoint()).description(new String(product.getDescription())).build();
  }

  public static ProductMasterDataResponse toProductMasterDataResponse(Product product, ProductItem productItem) {
    ProductMasterDataResponse response = new ProductMasterDataResponse();
    BeanUtils.copyProperties(product, response);

    Set<ProductItemResponse> productItemResponses = new HashSet<>();
    List<ProductCategoryResponse> productCategoryResponses = new ArrayList<>();
    List<Image> productImageResponses = new ArrayList<>();

    productItemResponses.add(toProductItemResponse(productItem));
    boolean activeCategoryMappingFound = false;
    for (ProductCategory productCategory : product.getProductCategories()) {
      if (!productCategory.isMarkForDelete()) {
        if (!activeCategoryMappingFound) {
          activeCategoryMappingFound = true;
          productCategoryResponses.clear();
        }
        productCategoryResponses.add(toProductCategoryResponse(productCategory));
      } else  {
        if (!activeCategoryMappingFound) {
          productCategoryResponses.add(toProductCategoryResponse(productCategory));
        }
      }
    }
    productImageResponses.addAll(
        product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete())
            .filter(productImage -> filterProcessedProductImages(productImage))
            .map(productImage -> convertProductImageToResponse(productImage)).collect(Collectors.toList()));

    response.setProductCategoryResponses(productCategoryResponses);
    response.setImages(productImageResponses);
    response.setProductItemResponses(productItemResponses);
    response.setSellerCode(product.getCreatedMerchant());
    response.setDistributionInfo(CommonUtil.getDistributionInfoResponse(product));
    return response;
  }

  private static ProductItemResponse toProductItemResponse(ProductItem productItem) {
    ProductItemResponse response = new ProductItemResponse();
    BeanUtils.copyProperties(productItem, response, "product", "productItemAttributeValues", "productItemImages");
    response.setProductItemAttributeValueResponses(new ArrayList<>());
    response.setImages(new ArrayList<>());
    response.getImages().addAll(
        productItem.getProductItemImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
            .filter(productItemImage -> filterProcessedItemImages(productItemImage))
            .map(productItemImage -> toImageResponse(productItemImage)).collect(Collectors.toList()));
    productItem.getProductItemAttributeValues().forEach(
        productItemAttributeValue -> response.getProductItemAttributeValueResponses()
            .add(toProductItemAttributeValueResponse(productItemAttributeValue)));
    Map<String, ProductItem> skuCodeAndProductItemMap = new HashMap<>();
    skuCodeAndProductItemMap.put(productItem.getSkuCode(), productItem);
    if (Objects.nonNull(productItem.getProductItemUomInfo())) {
      DistributionInfoPerSkuResponse distributionInfoPerSkuResponse =
          CommonUtil.mapToDistributionInfoPerSkuResponse(productItem.getProductItemUomInfo(), new Product(),
              skuCodeAndProductItemMap);
      response.setDistributionItemInfo(distributionInfoPerSkuResponse.getDistributionItemInfoResponse());
      response.setDimensionsAndUOM(distributionInfoPerSkuResponse.getDimensionsAndUomResponse());
    }
    return response;
  }

  private static Image toImageResponse(ProductItemImage productItemImage) {
    Image imageResponse = new Image();
    BeanUtils.copyProperties(productItemImage, imageResponse);
    return imageResponse;
  }

  private static ProductItemAttributeValueResponse toProductItemAttributeValueResponse(
      ProductItemAttributeValue productItemAttributeValue) {
    ProductItemAttributeValueResponse response = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse, "attributeType",
        "allowedAttributeValues", "predefinedAllowedAttributeValues", "categoryAttributes");
    attributeResponse.setAttributeType(productItemAttributeValue.getAttribute().getAttributeType().name());
    BeanUtils.copyProperties(productItemAttributeValue, response, "productItem", "attribute");
    response.setAttributeResponse(attributeResponse);
    return response;
  }

  private static ProductCategoryResponse toProductCategoryResponse(ProductCategory productCategory) {
    ProductCategoryResponse response = new ProductCategoryResponse();
    BeanUtils.copyProperties(productCategory, response, "product", "category");
    CategoryResponse category = new CategoryResponse();
    BeanUtils.copyProperties(productCategory.getCategory(), category);
    category.setCatalog(toCatalogResponse(productCategory.getCategory()));
    response.setCategory(category);
    return response;
  }

  private static CatalogResponse toCatalogResponse(Category category) {
    CatalogResponse catalogResponse = new CatalogResponse();
    if (Objects.nonNull(category.getCatalog())) {
      BeanUtils.copyProperties(category.getCatalog(), catalogResponse);
      if (Objects.nonNull(category.getCatalog().getCatalogType())) {
        catalogResponse.setCatalogType(category.getCatalog().getCatalogType().name());
      } else {
        catalogResponse.setCatalogType(StringUtils.EMPTY);
      }
    }
    return catalogResponse;
  }

  public static void updateItemImages(ProductItem productItem, CopyImageEditRequest copyImageEditRequest, String locationPath,
      Map<String, String> errorMap, boolean needRevision, boolean commonImage,
      ProductPublishUpdateDTO productPublishUpdateDTO, Set<String> mainImagesAdded, boolean containsCommonMainImage) {
    log.info("Updating images for item : {}, request : {}, locationPath : {}", productItem.getSkuCode(),
        copyImageEditRequest, locationPath);
    String requestImageName = imageNameFromLocationPath(locationPath);
    Map<String, List<ProductItemImage>> imageNameToImageListNewItem =
        ConverterUtil.imageNameToListOfItemImageMap(productItem.getProductItemImages());
    boolean itemImagesUpdated = false;
    if (copyImageEditRequest.isAdd()) {
      if (imageNameToImageListNewItem.containsKey(requestImageName)) {
        log.error("Cannot copy image as image already exists : {}, location path : {}", productItem.getSkuCode(),
            locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.IMAGE_ALREADY_EXISTS.getMessage());
        return;
      } else if (imageNameToImageListNewItem.size() >= Constants.MAX_IMAGE_PER_VARIANT) {
        log.error("Cannot add new image as max number of images present for item : {}, location path : {}",
            productItem.getSkuCode(), locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.MAX_IMAGE_REACHED.getMessage());
        return;
      } else {
        ProductItemImage productItemImage = new ProductItemImage();
        if (needRevision) {
          productItemImage.setRevised(true);
        } else {
          productItemImage.setEdited(true);
          productItemImage.setActive(true);
        }
        productItemImage.setOriginalImage(true);
        productItemImage.setStoreId(productItem.getStoreId());
        productItemImage.setSequence(
            productItem.getProductItemImages().stream().max(Comparator.comparingInt(ProductItemImage::getSequence))
                .orElse(new ProductItemImage(0)).getSequence() + 1);
        productItemImage.setCreatedDate(new Date());
        productItemImage.setUpdatedDate(new Date());
        productItemImage.setMarkForDelete(Boolean.FALSE);
        productItemImage.setLocationPath(locationPath);
        productItemImage.setHashCode(copyImageEditRequest.getHashCode());
        productItemImage.setCommonImage(commonImage);
        productItemImage.setProductItem(productItem);
        itemImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemImagesUpdated);
        List<ProductItemImage> imageList = new ArrayList<>();
        imageList.add(productItemImage);
        imageNameToImageListNewItem.put(requestImageName, imageList);
      }
    }

    if (copyImageEditRequest.isCopy()) {
      if (imageNameToImageListNewItem.containsKey(requestImageName)) {
        log.error("Cannot copy image as image already exists : {}, location path : {}", productItem.getSkuCode(),
            locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.IMAGE_ALREADY_EXISTS.getMessage());
        return;
      } else if (imageNameToImageListNewItem.size() >= Constants.MAX_IMAGE_PER_VARIANT) {
        log.error("Cannot copy image as max image already reached : {}, location path : {}", productItem.getSkuCode(),
            locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.MAX_IMAGE_REACHED.getMessage());
        return;
      } else {
        ProductItemImage productItemImage = new ProductItemImage();
        productItemImage.setOriginalImage(false);
        productItemImage.setActive(true);
        productItemImage.setLocationPath(locationPath);
        productItemImage.setHashCode(copyImageEditRequest.getHashCode());
        productItemImage.setProductItem(productItem);
        productItemImage.setStoreId(productItem.getStoreId());
        productItemImage.setSequence(
            productItem.getProductItemImages().stream().max(Comparator.comparingInt(ProductItemImage::getSequence))
                .orElse(new ProductItemImage(0)).getSequence() + 1);
        productItemImage.setCreatedDate(new Date());
        productItemImage.setUpdatedDate(new Date());
        productItemImage.setCommonImage(commonImage);
        productItemImage.setMarkForDelete(Boolean.FALSE);
        itemImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemImagesUpdated);
        List<ProductItemImage> imageList = new ArrayList<>();
        imageList.add(productItemImage);
        imageNameToImageListNewItem.put(requestImageName, imageList);
      }
    }

    if (copyImageEditRequest.isMarkForDelete()) {
      if (!imageNameToImageListNewItem.containsKey(requestImageName)) {
        log.error("Image not present for deletion in item : {}, location path : {}", productItem.getSkuCode(),
            locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.IMAGE_NOT_PRESENT.getMessage());
        return;
      } else {
        List<ProductItemImage> imagesToDelete = imageNameToImageListNewItem.get(requestImageName);
        for (ProductItemImage productItemImage : imagesToDelete) {
          if (productItemImage.isMainImages() && isMainImageDeleteRestricted(productItem.getSkuCode(), mainImagesAdded,
              containsCommonMainImage)) {
            log.error("Cannot delete image as image is main image : {}, location path : {}", productItem.getSkuCode(),
                locationPath);
            errorMap.put(productItem.getSkuCode(), ErrorMessage.CANNOT_DELETE_MAIN_IMAGE.getMessage());
            return;
          }
          productItemImage.setMarkForDelete(true);
          itemImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemImagesUpdated);
        }
        imageNameToImageListNewItem.put(requestImageName, imagesToDelete);
      }
    }

    if (copyImageEditRequest.isMainImage()) {
      if (!imageNameToImageListNewItem.containsKey(requestImageName)) {
        log.error("Cannot assign main image as image not present in item : {}, location path : {}",
            productItem.getSkuCode(), locationPath);
        errorMap.put(productItem.getSkuCode(), ErrorMessage.IMAGE_NOT_PRESENT.getMessage());
        return;
      } else {
        for (Map.Entry<String, List<ProductItemImage>> entry : imageNameToImageListNewItem.entrySet()) {
          boolean imageFound = StringUtils.equals(requestImageName, entry.getKey());
          List<ProductItemImage> images = imageNameToImageListNewItem.get(entry.getKey());
          for (ProductItemImage productItemImage : images) {
            if (imageFound) {
              if (!productItemImage.isMainImages()) {
                itemImagesUpdated =
                    CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemImagesUpdated);
              }
              productItemImage.setMainImages(true);
            } else {
              if (productItemImage.isMainImages()) {
                itemImagesUpdated =
                    CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemImagesUpdated);
              }
              productItemImage.setMainImages(false);
            }
            imageNameToImageListNewItem.put(entry.getKey(), images);
          }
        }
      }
    }
    if (itemImagesUpdated) {
      productPublishUpdateDTO.getUpdatedItemSkuCodes().add(productItem.getSkuCode());
    }
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    for (Map.Entry<String, List<ProductItemImage>> entry : imageNameToImageListNewItem.entrySet()) {
      productItemImageList.addAll(entry.getValue());
    }
    productItem.setProductItemImages(productItemImageList);
  }

  private static boolean isMainImageDeleteRestricted(String skuCode, Set<String> mainImagesAdded,
      boolean containsCommonMainImage) {
    return !mainImagesAdded.contains(skuCode) && !containsCommonMainImage;
  }

  public static BrandResponse generateBrandResponse(Brand brand) {
    BrandResponse brandResponse = null;
    if (Objects.nonNull(brand)) {
      brandResponse = new BrandResponse();
      BeanUtils.copyProperties(brand, brandResponse,
          new String[] {"brandDescription", "brandLogoPath", "profileBannerPath"});
      brandResponse
          .setBrandDescription(brand.getBrandDescription() == null ? null : new String(brand.getBrandDescription()));
      if (StringUtils.isNotEmpty(brand.getBrandLogoPath())) {
        brandResponse
            .setBrandLogoPath(PATH_SEPARATOR + brand.getBrandCode() + PATH_SEPARATOR + brand.getBrandLogoPath());
      }
      if (StringUtils.isNotEmpty(brand.getProfileBannerPath())) {
        brandResponse.setProfileBannerPath(
            PATH_SEPARATOR + brand.getBrandCode() + PATH_SEPARATOR + brand.getProfileBannerPath());
      }
    }
    return brandResponse;
  }

  public static NotificationKafka getNotificationKafka(
      String notificationDetail, String message, String notificationType, String destinationKey) {
    NotificationKafka notificationKafka = new NotificationKafka();
    notificationKafka.setTimestamp(System.currentTimeMillis());
    notificationKafka.setNotificationDetail(notificationDetail);
    notificationKafka.setNotificationMessage(message);
    notificationKafka.setNotificationType(notificationType);
    notificationKafka.setDestinationKey(destinationKey);
    Set<String> appNames = new HashSet<>();
    appNames.add(Constants.CLIENT_ID);
    notificationKafka.setAppNames(appNames);
    return notificationKafka;
  }

  public static AttributeResponse toAttributeValueResponse(Attribute attribute) {

    AttributeResponse response = new AttributeResponse();
    BeanUtils.copyProperties(attribute, response, "allowedAttributeValues", "predefinedAllowedAttributeValues");
    response.setAttributeType(attribute.getAttributeType().toString());
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    for (AllowedAttributeValue allowedAttributeValue : attribute.getAllowedAttributeValues()) {
      AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
      BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueResponse);
      allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    }
    response.setAllowedAttributeValues(allowedAttributeValueResponses);
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses = new ArrayList<>();
    for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : attribute.getPredefinedAllowedAttributeValues()) {
      PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
          new PredefinedAllowedAttributeValueResponse();
      BeanUtils.copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueResponse);
      predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
    }
    response.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueResponses);
    return response;
  }

  public static List<ItemImageResponse> convertProductItemToItemImageResponse(
    List<ProductItem> productItems, boolean removeOriginalImages) {
    return productItems.stream().map(
      productItem -> ItemImageResponse.builder().itemName(productItem.getGeneratedItemName())
        .upcCode(productItem.getUpcCode()).itemCode(productItem.getSkuCode()).imageResponses(
          convertProductItemImageToImageResponse(productItem.getProductItemImages(),
            removeOriginalImages)).build()).collect(Collectors.toList());
  }

  private static List<ImageResponse> convertProductItemImageToImageResponse(List<ProductItemImage> productItemImages,
      boolean removeOriginalImages) {
    List<ImageResponse> imageResponseList = new ArrayList<>();
    if (removeOriginalImages) {
      productItemImages = productItemImages.stream().filter(ConverterUtil::filterProcessedItemImages)
          .collect(Collectors.toList());
    }
    for (ProductItemImage productItemImage : productItemImages) {
      ImageResponse imageResponse = new ImageResponse();
      imageResponse.setMainImage(productItemImage.isMainImages());
      imageResponse.setActive(productItemImage.isActive());
      imageResponse.setMarkForDelete(productItemImage.isMarkForDelete());
      imageResponse.setRevised(productItemImage.isRevised());
      imageResponse.setEdited(productItemImage.isEdited());
      imageResponse.setCommonImage(productItemImage.isCommonImage());
      imageResponse.setLocationPath(productItemImage.getLocationPath());
      imageResponse.setOriginalImage(productItemImage.getOriginalImage());
      imageResponse.setSequence(productItemImage.getSequence());
      imageResponseList.add(imageResponse);
    }
    return imageResponseList;
  }

  public static List<ImageResponse> convertProductImageToImageResponse(List<ProductImage> productImages, boolean removeOriginalImages) {
    List<ImageResponse> imageResponseList = new ArrayList<>();
    if (removeOriginalImages) {
      productImages = productImages.stream().filter(ConverterUtil::filterProcessedProductImages)
          .collect(Collectors.toList());
    }
    for (ProductImage productImage : productImages) {
      ImageResponse imageResponse = new ImageResponse();
      imageResponse.setMainImage(productImage.isMainImages());
      imageResponse.setActive(productImage.isActive());
      imageResponse.setLocationPath(productImage.getLocationPath());
      imageResponse.setOriginalImage(productImage.getOriginalImage());
      imageResponse.setSequence(productImage.getSequence());
      imageResponseList.add(imageResponse);
    }
    return imageResponseList;
  }


  private static Boolean validateAuthorisationStatus(BrandAuthorisation brandAuthorisation) {
    Date now = new Date();
    if (StringUtils.equals(brandAuthorisation.getAuthorisationStatus().name(),
      BrandAuthorisationStatus.ACTIVE.name())) {
      return (brandAuthorisation.getAuthStartDate().before(now)
        && brandAuthorisation.getAuthExpireDate().after(brandAuthorisation.getAuthStartDate()));
    } return false;
  }
  public static BrandAuthorisationDetailResponse convertToBrandAuthDetailResponse(
    BrandAuthorisation brandAuthorisation) {
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      new BrandAuthorisationDetailResponse();
    BeanUtils.copyProperties(brandAuthorisation, brandAuthorisationDetailResponse, "sellerCode",
      "authorisationStatus");
    if (Boolean.TRUE.equals(validateAuthorisationStatus(brandAuthorisation))) {
      brandAuthorisationDetailResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    } else {
      brandAuthorisationDetailResponse.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    }
    brandAuthorisationDetailResponse.setSellerId(brandAuthorisation.getSellerCode());
    return brandAuthorisationDetailResponse;
  }

  public static List<BrandAuthHistoryResponse> generateBrandAuthHistoryResponses(
    Page<BrandAuthorisationHistory> authorisationHistory,
    BrandAuthHistoryRequest brandAuthHistoryRequest) {

    Predicate<BrandAuthorisationHistory> userNameFilter =
      authorisation -> StringUtils.isBlank(brandAuthHistoryRequest.getUserName())
        || StringUtils.equals(brandAuthHistoryRequest.getUserName(), authorisation.getUpdatedBy());

    return Optional.ofNullable(authorisationHistory).map(Page::getContent)
      .orElse(Collections.emptyList()).stream().filter(userNameFilter)
      .map(ConverterUtil::generateBrandAuthHistoryResponse).collect(Collectors.toList());
  }

  private static BrandAuthHistoryResponse generateBrandAuthHistoryResponse(
    BrandAuthorisationHistory brandAuthorisationHistory) {
    BrandAuthHistoryResponse brandAuthHistoryResponse = new BrandAuthHistoryResponse();
    BeanUtils.copyProperties(brandAuthorisationHistory, brandAuthHistoryResponse);
    brandAuthHistoryResponse.setUpdatedBy(
      StringUtils.defaultString(brandAuthHistoryResponse.getUpdatedBy(),
        brandAuthHistoryResponse.getCreatedBy()));
    return brandAuthHistoryResponse;
  }

  public static List<BrandAuthBulkDownloadResponse> convertToBrandAuthBulkResponse(
    List<BrandAuthorisation> brandAuthList) {
    List<BrandAuthBulkDownloadResponse> bulkDownloadResponseList = new ArrayList<>();
    brandAuthList.forEach(brandAuth -> {
      BrandAuthBulkDownloadResponse authBulkDownloadResponse = new BrandAuthBulkDownloadResponse();
      BeanUtils.copyProperties(brandAuth, authBulkDownloadResponse);
      bulkDownloadResponseList.add(authBulkDownloadResponse);
    });
    return bulkDownloadResponseList;
  }

  public static List<BrandInReviewResponse> generateBrandInReviewResponses(List<BrandWip> brandWips) {
    return Optional.ofNullable(brandWips).orElseGet(Collections::emptyList).stream()
      .map(brandWip -> generateBrandInReview(brandWip)).collect(Collectors.toList());
  }

  public static BrandInReviewResponse generateBrandInReview(BrandWip brandWip) {
    BrandInReviewResponse brandInReviewResponse = new BrandInReviewResponse();
    BeanUtils.copyProperties(brandWip, brandInReviewResponse,  "state");
    brandInReviewResponse.setState(brandWip.getState());
    return brandInReviewResponse;
  }

  public static BrandAuthorisationHistory toBrandAuthHistoryEventModel(BrandAuthorisation finalAuthData,
    BrandAuthorisation savedAuthData) {
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(finalAuthData.getBrandCode());
    brandAuthorisationHistory.setSellerCode(finalAuthData.getSellerCode());
    brandAuthorisationHistory.setOldStatus(savedAuthData.getAuthorisationStatus().name());
    brandAuthorisationHistory.setNewStatus(finalAuthData.getAuthorisationStatus().name());
    brandAuthorisationHistory.setUpdatedDate(new Date());
    brandAuthorisationHistory.setUpdatedBy(finalAuthData.getUpdatedBy());
    brandAuthorisationHistory.setStoreId(finalAuthData.getStoreId());
    return brandAuthorisationHistory;
  }

  public static void setProductCategoryArchiveList(Collection<ProductCategory> productCategoryList,
      Collection<ProductCategoryArchive> productCategoryArchiveList) {
    for (ProductCategory productCategory : productCategoryList) {
      ProductCategoryArchive productCategoryArchive = new ProductCategoryArchive();
      BeanUtils.copyProperties(productCategory, productCategoryArchive);
      productCategoryArchiveList.add(productCategoryArchive);
    }
  }

  public static void setProductImageArchiveList(Collection<ProductImage> productImageList,
      Collection<ProductImageArchive> productImageArchiveList) {
    for (ProductImage productImage : productImageList) {
      ProductImageArchive productImageArchive = new ProductImageArchive();
      BeanUtils.copyProperties(productImage, productImageArchive);
      productImageArchiveList.add(productImageArchive);
    }
  }

  public static void setProductAttributeArchiveList(Collection<ProductAttribute> productAttributeList,
      Collection<ProductAttributeArchive> productAttributeArchiveList,
      Collection<ProductAttributeValueArchive> productAttributeValueArchiveList) {
    for (ProductAttribute productAttribute : productAttributeList) {
      ProductAttributeArchive productAttributeArchive = new ProductAttributeArchive();
      BeanUtils.copyProperties(productAttribute, productAttributeArchive);
      productAttributeArchiveList.add(productAttributeArchive);

      //copy pcc_product_attribute_value
      setProductAttributeValueArchiveList(productAttribute, productAttributeValueArchiveList);
    }
  }

  public static void setProductAttributeValueArchiveList(ProductAttribute productAttribute,
      Collection<ProductAttributeValueArchive> productAttributeValueArchiveList) {
    if (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())) {
      for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
        ProductAttributeValueArchive productAttributeValueArchive = new ProductAttributeValueArchive();
        BeanUtils.copyProperties(productAttributeValue, productAttributeValueArchive);
        productAttributeValueArchiveList.add(productAttributeValueArchive);
      }
    }
  }

  public static void setProductItemArchiveList(Collection<ProductItem> productItems,
      Collection<ProductItemArchive> productItemArchiveList,
      Collection<ProductItemAttributeValueArchive> productItemAttributeValueArchiveList,
      Collection<ProductItemImageArchive> productItemImageArchiveList) {
    for (ProductItem productItem : productItems) {
      ProductItemArchive productItemArchive = new ProductItemArchive();
      BeanUtils.copyProperties(productItem, productItemArchive);
      productItemArchiveList.add(productItemArchive);

      //copy pcc_product_item_attribute_value
      setProductItemAttributeValueArchiveList(productItem, productItemAttributeValueArchiveList);

      //copy pcc_product_item_images
      setProductItemImagesArchiveList(productItem, productItemImageArchiveList);
    }
  }

  public static void setProductItemAttributeValueArchiveList(ProductItem productItem,
      Collection<ProductItemAttributeValueArchive> productItemAttributeValueArchiveList) {
    if (CollectionUtils.isNotEmpty(productItem.getProductItemAttributeValues())) {
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        ProductItemAttributeValueArchive productItemAttributeValueArchive = new ProductItemAttributeValueArchive();
        BeanUtils.copyProperties(productItemAttributeValue, productItemAttributeValueArchive);
        productItemAttributeValueArchiveList.add(productItemAttributeValueArchive);
      }
    }
  }

  public static void setProductItemImagesArchiveList(ProductItem productItem,
      Collection<ProductItemImageArchive> productItemImageArchiveList) {
    if (CollectionUtils.isNotEmpty(productItem.getProductItemImages())) {
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        ProductItemImageArchive productItemImageArchive = new ProductItemImageArchive();
        BeanUtils.copyProperties(productItemImage, productItemImageArchive);
        productItemImageArchiveList.add(productItemImageArchive);
      }
    }
  }

  public static ImagePathUpdateDomainEventModel toImagePathUpdateDomainEventModel(String storeId, String productCode,
      Set<Pair<String, String>> imagePaths) {
    return new ImagePathUpdateDomainEventModel(storeId, productCode, toOldAndNewPathDomainEventModelSet(imagePaths));
  }

  private static Set<OldAndNewPathDomainEventModel> toOldAndNewPathDomainEventModelSet(
      Set<Pair<String, String>> imagePaths) {
    return imagePaths.stream()
        .map(imagePath -> new OldAndNewPathDomainEventModel(imagePath.getLeft(), imagePath.getRight()))
        .collect(Collectors.toSet());
  }

  public static boolean isCategoryRestrictedKeywordDataUpdated(CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO,
      CategoryRestrictedKeyword categoryRestrictedKeyword) {
    return (!StringUtils.equals(categoryRestrictedKeyword.getType(), categoryKeywordsUpdateDTO.getType()) || !StringUtils
        .equals(categoryRestrictedKeyword.getMessage(), categoryKeywordsUpdateDTO.getMessage()) || !StringUtils
        .equals(categoryRestrictedKeyword.getDestinationCategory(), categoryKeywordsUpdateDTO.getDestinationCategory())
        || categoryRestrictedKeyword.getAction() != categoryKeywordsUpdateDTO.getAction() || categoryRestrictedKeyword
        .isMarkForDelete());
  }

  public static CategoryRestrictedKeywordResponse toCategoryRestrictedKeywordResponse(
      CategoryRestrictedKeyword categoryRestrictedKeyword) {
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
    BeanUtils.copyProperties(categoryRestrictedKeyword, categoryRestrictedKeywordResponse);
    return categoryRestrictedKeywordResponse;
  }

  public static CategoryRestrictedKeywordResponse toCategoryRestrictedKeywordResponseWithKeyword(
      CategoryRestrictedKeyword categoryRestrictedKeyword) {
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
    BeanUtils.copyProperties(categoryRestrictedKeyword, categoryRestrictedKeywordResponse);
    if (Objects.nonNull(categoryRestrictedKeyword.getRestrictedKeyword())) {
      categoryRestrictedKeywordResponse.setKeyword(categoryRestrictedKeyword.getRestrictedKeyword().getKeyword());
      categoryRestrictedKeywordResponse.setValidateByDs(
          categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs());
    }
    return categoryRestrictedKeywordResponse;
  }

  public static MatrixAttributeExtractionRequest toMatrixAttributeExtractionRequest(Product product) {
    return MatrixAttributeExtractionRequest.builder().product_code(product.getProductCode())
        .cn_code(product.getProductCategories().get(0).getCategory().getCategoryCode()).title(product.getName())
        .brand(product.getBrand()).product_usp(product.getUniqueSellingPoint())
        .description(new String(product.getDescription()) + StringUtils.SPACE + product.getUniqueSellingPoint())
        .build();
  }

  public static ProductAttribute generateNewProductAttributeForAutoFill(Product product, Attribute attribute) {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductId(attribute.getId());
    productAttribute.setProduct(product);
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeName(attribute.getName());
    productAttribute.setSequence(0);
    productAttribute.setStoreId(product.getStoreId());

    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.DescriptiveAttributeValueType.SINGLE);
    productAttributeValue.setVersion(0L);
    productAttributeValue.setStoreId(product.getStoreId());

    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));

    return productAttribute;
  }

  public static boolean validateDimension(Double length, Double width, Double weight, Double height) {
    return Arrays.stream(new Double[] {length, width, weight, height})
        .anyMatch(dimension -> Objects.isNull(dimension) || dimension <= MINIMUM_DIMENSION_ALLOWED);
  }

  public static Product convertProductDTOToProduct(ProductDTO productDTO) {
    Product product = new Product();
    BeanUtils.copyProperties(productDTO, product, "productCategories", "productAttributes",
      "productImages", "productItems");
    product.setProductAttributes(productDTO.getProductAttributes());
    product.setProductCategories(productDTO.getProductCategories());
    product.getProductCategories().forEach(productCategory -> {
      productCategory.setProductId(productDTO.getId());
      productCategory.setProduct(product);
    });
    product.setProductImages(productDTO.getProductImages());
    product.getProductItems().forEach(productItem -> productItem.setProduct(product));
    product.setProductItems(productDTO.getProductItems());
    product.getProductItems().forEach(productItem -> productItem.setProduct(product));
    product.getProductImages().forEach(productImage -> productImage.setProduct(product));
    return product;
  }

  public static ProductDTO convertProductToDTO(Product product) {
    ProductDTO productDTO = getProductDTO(product);

    List<ProductCategory> productCategoryDTOs =
      Optional.ofNullable(product.getProductCategories()).orElse(Collections.emptyList()).stream()
        .filter(Objects::nonNull).map(ConverterUtil::convertProductCategoryToDTO)
        .collect(Collectors.toList());
    productCategoryDTOs.forEach(productCategory -> productCategory.setProductId(product.getId()));
    productCategoryDTOs.forEach(productCategory -> productCategory.setProduct(product));
    productDTO.setProductCategories(productCategoryDTOs);
    productDTO.setProductAttributes(product.getProductAttributes());
    product.getProductImages().stream()
      .map(productImage -> {
        ProductImage image = new ProductImage();
        BeanUtils.copyProperties(productImage, image, "product");
        image.setProduct(product);
        return image;
      })
      .forEach(productDTO.getProductImages()::add);
    productDTO.setProductItems(product.getProductItems());
    return productDTO;
  }

  private static ProductCategory convertProductCategoryToDTO(ProductCategory productCategory) {
    ProductCategory productCategoryDTO = new ProductCategory();
      BeanUtils.copyProperties(productCategory, productCategoryDTO, "category", "product");
    Category categoryDataTransferObject =
      convertProductCategoryToProductCategoryDTO(productCategory.getCategory());
    productCategoryDTO.setCategory(categoryDataTransferObject);
    productCategoryDTO.setProduct(productCategory.getProduct());
    return productCategoryDTO;
  }

  private static Category convertProductCategoryToProductCategoryDTO(Category category) {
    Category categoryDataTransferObject = new Category();
    if (Objects.nonNull(category)) {
      BeanUtils.copyProperties(category, categoryDataTransferObject, "categoryAttributes",
        "productCategories", "masterCategoryReferences", "salesCategoryReferences",
        "categoryRestrictedKeywords", "originalSalesCategory", "extractionType");
      categoryDataTransferObject.setCategoryAttributes(category.getCategoryAttributes());
      categoryDataTransferObject.setProductCategories(category.getProductCategories());
      categoryDataTransferObject.setCatalog(category.getCatalog());
      categoryDataTransferObject.setCategoryRestrictedKeywords(
        category.getCategoryRestrictedKeywords());
      categoryDataTransferObject.setExtractionType(category.getExtractionType());
      categoryDataTransferObject.setOriginalSalesCategory(category.getOriginalSalesCategory());
      categoryDataTransferObject.setMasterCategoryReferences(category.getMasterCategoryReferences());
    }
    return categoryDataTransferObject;
  }

  private static ProductDTO getProductDTO(Product product) {
    ProductDTO productDTO = new ProductDTO();
      BeanUtils.copyProperties(product, productDTO, "productCategories", "productAttributes",
        "productImages", "productItems");
    return productDTO;
  }


  public static String getProductAttributeValuesFromProductAttributeList(
      List<ProductAttributeValue> productAttributeValuesList) {
    String response = StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(productAttributeValuesList)) {
      ProductAttributeValue productAttributeValue =
          productAttributeValuesList.stream().findFirst().orElse(new ProductAttributeValue());
      if (StringUtils.isNotBlank(productAttributeValue.getDescriptiveAttributeValue())) {
        response = productAttributeValue.getDescriptiveAttributeValue();
      } else if (Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValueId()) && Objects.nonNull(
          productAttributeValue.getPredefinedAllowedAttributeValue()) && StringUtils.isNotBlank(
          productAttributeValue.getPredefinedAllowedAttributeValue().getValue())) {
        response = productAttributeValue.getPredefinedAllowedAttributeValue().getValue();
      } else if (Objects.nonNull(productAttributeValue.getAllowedAttributeValueId()) && Objects.nonNull(
          productAttributeValue.getAllowedAttributeValue()) && StringUtils.isNotBlank(
          productAttributeValue.getAllowedAttributeValue().getValue())) {
        response = productAttributeValue.getAllowedAttributeValue().getValue();
      }
    }
    return response;
  }

  public static CategoryHistoryEventModel constructCategoryHistoryEventModel(String previousValue,
    String currentValue, String categoryCode, String storeId, String userName, String activity) {
    return CategoryHistoryEventModel.builder().categoryCode(categoryCode).activity(activity)
      .oldStatus(previousValue).newStatus(currentValue).userName(userName).storeId(storeId).build();
  }

  public static boolean doCacheClearCategoryTree(boolean genericTemplateEligibleChanged,
    boolean nameChanged, boolean isParentCategoryChanged, boolean statusChangeFlag) {
    return genericTemplateEligibleChanged || nameChanged || isParentCategoryChanged
      || statusChangeFlag;
  }

  public static CategoryAttributeMappingUpdateHistoryEventDTO convertToCategoryAttributeMappingUpdateEventModel(
      CategoryAttribute categoryAttribute) {
    return CategoryAttributeMappingUpdateHistoryEventDTO.builder()
        .attributeName(categoryAttribute.getAttribute().getName())
        .attributeCode(categoryAttribute.getAttribute().getAttributeCode())
        .isUSP(categoryAttribute.isUSP()).build();
  }

  public static CategoryHistoryResponse mapCategoryHistoryToCategoryHistoryResponse(
      CategoryHistory categoryHistory) {
    CategoryHistoryResponse response = new CategoryHistoryResponse();
    BeanUtils.copyProperties(categoryHistory, response);
    return response;
  }

  public static WholesaleMappingDTO convertToWholesaleConfigDTO(String configurationType,
      List<WholesaleConfigDTO> wholesaleConfigDTOS) {
    return WholesaleMappingDTO.builder().configurationType(configurationType)
        .wholesaleConfig(wholesaleConfigDTOS).build();
  }

  public static Page<DimensionMappingResponse> toDimensionMappingResponse(
      Page<DimensionMapping> dimensionMappingPage) {
    List<DimensionMappingResponse> dimensionMappingResponses = new ArrayList<>();
    ValidationUtil.checkParameter(Objects.nonNull(dimensionMappingPage.getContent()),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE_ERROR_CODE.getMessage(),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE.getMessage());
    for (DimensionMapping dimensionMapping : dimensionMappingPage.getContent()) {
      DimensionMappingResponse response = new DimensionMappingResponse();
      BeanUtils.copyProperties(dimensionMapping, response, "attributeId", "dimension");
      Dimension dimension =
          Optional.ofNullable(dimensionMapping.getDimension()).orElseGet(Dimension::new);
      response.setName(dimension.getName());
      response.setNameEnglish(dimension.getNameEnglish());
      response.setDimensionId(dimension.getId());
      response.setDimensionCode(dimension.getDimensionCode());
      dimensionMappingResponses.add(response);
    }
    return new PageImpl<>(dimensionMappingResponses, dimensionMappingPage.getPageable(),
        dimensionMappingPage.getTotalElements());
  }

  public static DimensionMapping getDimensionMapping(String attributeId, Attribute attribute,
      DimensionMappingRequest dimensionMappingRequest) {
    DimensionMapping dimensionMapping = new DimensionMapping();
    dimensionMapping.setAttributeId(attributeId);
    dimensionMapping.setAttributeCode(attribute.getAttributeCode());
    dimensionMapping.setStoreId(attribute.getStoreId());
    dimensionMapping.setMandatory(dimensionMappingRequest.isMandatory());
    return dimensionMapping;
  }

  public static void constructSizeChart(SizeChartRequest sizeChartRequest, SizeChart sizeChart,
    String storeId)
    throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    sizeChart.setStoreId(storeId);
    sizeChart.setSizeAttributeValueTypes(
      mapper.writeValueAsString(sizeChartRequest.getSelectedValueTypes()));
    sizeChart.setDimensions(
      mapper.writeValueAsString(sizeChartRequest.getSelectedDimensionCodes()));
    sizeChart.setSizeChartRows(mapper.writeValueAsString(sizeChartRequest.getSizeChartRows()));
  }

  public static SizeChartDetailResponse convertToSizeChartDetailResponse(SizeChart sizeChart)
    throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    SizeChartDetailResponse sizeChartDetailResponse = new SizeChartDetailResponse();
    BeanUtils.copyProperties(sizeChart, sizeChartDetailResponse, "sizeChartRows");
    sizeChartDetailResponse.setSelectedValueTypes(
      mapper.readValue(sizeChart.getSizeAttributeValueTypes(), new TypeReference<List<String>>() {
      }));
    sizeChartDetailResponse.setSelectedDimensionCodes(
      mapper.readValue(sizeChart.getDimensions(), new TypeReference<List<String>>() {
      }));
    sizeChartDetailResponse.setSizeChartRows(
      mapper.readValue(sizeChart.getSizeChartRows(), new TypeReference<List<SizeChartDataRow>>() {
      }));
    return sizeChartDetailResponse;
  }

  public static SizeChartResponse convertToResponse(SizeChart sizeChart,
    Map<String, Attribute> attributeCodeDetailMap) {
    if (Objects.isNull(sizeChart)) {
      return null;
    }
    SizeChartResponse sizeChartResponse =  SizeChartResponse.builder()
        .sizeChartName(sizeChart.getName())
        .sizeChartCode(sizeChart.getSizeChartCode())
        .sizeAttributeName(sizeChart.getSizeAttributeName())
        .sizeAttributeCode(sizeChart.getSizeAttributeCode())
        .brandName(sizeChart.getBrand())
        .brandCode(sizeChart.getBrandCode())
        .businessPartnerCode(sizeChart.getBusinessPartnerCode())
        .waitingDeletion(sizeChart.isWaitingDeletion())
        .build();
    sizeChartResponse.setUpdatedBy(sizeChart.getUpdatedBy());
    sizeChartResponse.setUpdatedDate(sizeChart.getUpdatedDate());
    if (Objects.nonNull(attributeCodeDetailMap) && !attributeCodeDetailMap.isEmpty()) {
      Optional.ofNullable(attributeCodeDetailMap.get(sizeChart.getSizeAttributeCode()))
        .map(Attribute::getDescription).ifPresentOrElse(sizeChartResponse::setDescription,
          () -> sizeChartResponse.setDescription(new byte[0]));
      Optional.ofNullable(attributeCodeDetailMap.get(sizeChart.getSizeAttributeCode()))
        .map(Attribute::getDescriptionEnglish)
        .ifPresentOrElse(sizeChartResponse::setDescriptionEnglish,
          () -> sizeChartResponse.setDescriptionEnglish(new byte[0]));
    }
    return sizeChartResponse;
  }

  public static void groupByAttributeType(List<AllowedAttributeValueDtoRequest> request, List<String> predefinedValues,
      List<String> predefinedCodes, List<String> definingValues, List<String> definingCodes,
      List<AllowedAttributeValueDtoRequest> definingAttributeRequest) {
    for (AllowedAttributeValueDtoRequest allowedReq : request) {
      if (AttributeType.PREDEFINED_ATTRIBUTE.toString().equals(allowedReq.getAttributeType())) {
        predefinedValues.addAll(allowedReq.getValues());
        predefinedCodes.add(allowedReq.getAttributeCode());
      } else if (AttributeType.DEFINING_ATTRIBUTE.toString().equals(allowedReq.getAttributeType())) {
        definingValues.addAll(allowedReq.getValues());
        definingCodes.add(allowedReq.getAttributeCode());
        definingAttributeRequest.add(allowedReq);
      }
    }
  }

  public static Map<String, Map<String, Map<String, AllowedAttributeValue>>> getAttributeCodeAndValueAndValueTypeMapFromDb(
      List<AllowedAttributeValue> definingResponse) {
    Map<String, Map<String, Map<String, AllowedAttributeValue>>> attributeCodeAndValueAndValueTypeMap = new HashMap<>();
    for (AllowedAttributeValue allowedAttributeValue : definingResponse) {
      Map<String, Map<String, AllowedAttributeValue>> valueAndValueTypeMap =
          attributeCodeAndValueAndValueTypeMap.getOrDefault(allowedAttributeValue.getAttribute().getAttributeCode(),
              new HashMap<>());
      Map<String, AllowedAttributeValue> valueTypeMap =
          valueAndValueTypeMap.getOrDefault(allowedAttributeValue.getValue(), new HashMap<>());
      valueTypeMap.put(Optional.ofNullable(allowedAttributeValue.getValueType()).orElse(StringUtils.EMPTY),
          allowedAttributeValue);
      valueAndValueTypeMap.put(allowedAttributeValue.getValue(), valueTypeMap);
      attributeCodeAndValueAndValueTypeMap.put(allowedAttributeValue.getAttribute().getAttributeCode(),
          valueAndValueTypeMap);
    }
    return attributeCodeAndValueAndValueTypeMap;
  }

  public static List<AllowedAttributeValueDtoResponse> populatePredefinedAttribute(
      List<PredefinedAllowedAttributeValue> response) {
    Map<String, AllowedAttributeValueDtoResponse> mapResponse = new HashMap<>();
    for (PredefinedAllowedAttributeValue value : response) {
      Hibernate.initialize(value.getAttribute());
      AllowedValueDto allowedValueDto =
          new AllowedValueDto(value.getId(), value.getPredefinedAllowedAttributeCode(), value.getValue());
      populateAllowedValuesToMap(mapResponse, allowedValueDto, AttributeType.PREDEFINED_ATTRIBUTE.toString(),
          value.getAttribute().getAttributeCode());
    }
    return new ArrayList<>(mapResponse.values());
  }

  public static List<AllowedAttributeValueDtoResponse> populateAllowedAttributeNonSizeChartValue(
      List<AllowedAttributeValue> response) {
    Map<String, AllowedAttributeValueDtoResponse> mapResponse = new HashMap<>();
    for (AllowedAttributeValue value : response) {
      Hibernate.initialize(value.getAttribute());
      AllowedValueDto allowedValueDto =
          new AllowedValueDto(value.getId(), value.getAllowedAttributeCode(), value.getValue());
      populateAllowedValuesToMap(mapResponse, allowedValueDto, AttributeType.DEFINING_ATTRIBUTE.toString(),
          value.getAttribute().getAttributeCode());
    }
    return new ArrayList<>(mapResponse.values());
  }

  public static List<AllowedAttributeValueDtoResponse> populateAllowedAttributeSizeChartValue(
      List<AllowedAttributeValueDtoRequest> definingAttributeRequest,
      Map<String, Map<String, Map<String, AllowedAttributeValue>>> sanitizedAttributeCodeAndValueAndValueTypeMapFromDb,
      String sizeChartValueTypeDelimiter) {
    Map<String, AllowedAttributeValueDtoResponse> mapResponse = new HashMap<>();
    for (AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest : definingAttributeRequest) {
      if (sanitizedAttributeCodeAndValueAndValueTypeMapFromDb.containsKey(
          allowedAttributeValueDtoRequest.getAttributeCode())) {
        for (String value : allowedAttributeValueDtoRequest.getValues()) {
          String requestSanitizedValue =
              ConverterUtil.getAttributeValueFromValueAndValueType(value, sizeChartValueTypeDelimiter);
          String requestSanitizedValueType =
              ConverterUtil.getAttributeValueTypeFromValueAndValueType(value, sizeChartValueTypeDelimiter);
          Map<String, Map<String, AllowedAttributeValue>> valueAndValueTypeMap =
              sanitizedAttributeCodeAndValueAndValueTypeMapFromDb.get(
                  allowedAttributeValueDtoRequest.getAttributeCode());
          toAllowedAttributeValueResponseBasedOnValueAndValueType(value, valueAndValueTypeMap, requestSanitizedValue,
              requestSanitizedValueType, mapResponse);
        }
      }
    }
    return new ArrayList<>(mapResponse.values());
  }

  private static void toAllowedAttributeValueResponseBasedOnValueAndValueType(String value,
      Map<String, Map<String, AllowedAttributeValue>> valueAndValueTypeMap, String requestSanitizedValue,
      String requestSanitizedValueType, Map<String, AllowedAttributeValueDtoResponse> mapResponse) {
    if (valueAndValueTypeMap.containsKey(requestSanitizedValue)) {
      Map<String, AllowedAttributeValue> valueTypeAndAllowedAttributeValueMap =
          valueAndValueTypeMap.get(requestSanitizedValue);
      if (valueTypeAndAllowedAttributeValueMap.containsKey(requestSanitizedValueType)) {
        AllowedAttributeValue allowedAttributeValue =
            valueTypeAndAllowedAttributeValueMap.get(requestSanitizedValueType);
        toAllowedAttributeValueResponse(value, allowedAttributeValue, mapResponse);
      } else if (StringUtils.isBlank(requestSanitizedValueType) && valueTypeAndAllowedAttributeValueMap.size() == 1) {
        AllowedAttributeValue allowedAttributeValue =
            valueTypeAndAllowedAttributeValueMap.values().stream().findFirst().get();
        toAllowedAttributeValueResponse(value, allowedAttributeValue, mapResponse);
      }
    }
  }

  private static void toAllowedAttributeValueResponse(String value, AllowedAttributeValue allowedAttributeValue,
      Map<String, AllowedAttributeValueDtoResponse> mapResponse) {
    AllowedValueDto allowedValueDto =
        new AllowedValueDto(allowedAttributeValue.getId(), allowedAttributeValue.getAllowedAttributeCode(), value);
    allowedValueDto.setValueType(allowedAttributeValue.getValueType());
    populateAllowedValuesToMap(mapResponse, allowedValueDto, AttributeType.DEFINING_ATTRIBUTE.toString(),
        allowedAttributeValue.getAttribute().getAttributeCode());
  }

  private static void populateAllowedValuesToMap(Map<String, AllowedAttributeValueDtoResponse> mapResponse,
      AllowedValueDto allowedValueDto, String attributeType, String attributeCode) {
    if (!mapResponse.containsKey(attributeCode)) {
      mapResponse.put(attributeCode,
          new AllowedAttributeValueDtoResponse(attributeCode, attributeType, Arrays.asList(allowedValueDto)));
    } else {
      List<AllowedValueDto> allowedValueList = new ArrayList<>();
      allowedValueList.addAll(mapResponse.get(attributeCode).getAllowedValue());
      allowedValueList.add(allowedValueDto);
      mapResponse.get(attributeCode).setAllowedValue(allowedValueList);
    }
  }

  public static List<String> getAttributeValueFromValueAndValueType(Collection<String> definingValues,
      String sizeChartValueTypeDelimiter) {
    return definingValues.stream().filter(StringUtils::isNotBlank)
        .map(definingValue -> getAttributeValueFromValueAndValueType(definingValue, sizeChartValueTypeDelimiter))
        .collect(Collectors.toList());
  }

  private static String getAttributeValueFromValueAndValueType(String value, String sizeChartValueTypeDelimiter) {
    String[] tokens = value.split(sizeChartValueTypeDelimiter);
    return tokens[tokens.length - 1];
  }

  private static String getAttributeValueTypeFromValueAndValueType(String value, String sizeChartValueTypeDelimiter) {
    String[] tokens = value.split(sizeChartValueTypeDelimiter);
    return tokens.length == 2 ? tokens[0] : StringUtils.EMPTY;
  }

  public static List<ItemImageResponse> convertProductItemToBasicItemDetailResponse(
    List<ProductItem> productItems) {
    return productItems.stream().map(
        productItem -> ItemImageResponse.builder().itemName(productItem.getGeneratedItemName())
          .upcCode(productItem.getUpcCode()).itemCode(productItem.getSkuCode()).build())
      .collect(Collectors.toList());
  }

  public static void groupBySizeChartAttribute(Collection<AllowedAttributeValue> sanitizedDefiningResponse,
      List<AllowedAttributeValue> sizeChartAllowedAttributeValue,
      List<AllowedAttributeValue> nonSizeChartAllowedAttributeValue) {
    for (AllowedAttributeValue allowedAttributeValue : sanitizedDefiningResponse) {
      if (allowedAttributeValue.getAttribute().isSizeAttribute()) {
        sizeChartAllowedAttributeValue.add(allowedAttributeValue);
      } else {
        nonSizeChartAllowedAttributeValue.add(allowedAttributeValue);
      }
    }
  }

  public static BrandHistoryEventModel toBrandWipHistoryModel(BrandWip brandWip, String username) {
    BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
    BeanUtils.copyProperties(brandWip, brandHistoryEventModel);
    brandHistoryEventModel.setUsername(username);
    brandHistoryEventModel.setDescription(brandWip.getState().getDescription().getBytes());
    return brandHistoryEventModel;
  }

  public static BrandHistoryEventModel toBrandWipHistoryModel(BrandWip brandWip, String description,
      String username) {
    BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
    BeanUtils.copyProperties(brandWip, brandHistoryEventModel);
    brandHistoryEventModel.setUsername(username);
    brandHistoryEventModel.setDescription(description.getBytes());
    return brandHistoryEventModel;
  }

  public static BrandAuthorisationWip toUpdatedBrandAuthorisationWipEntity(BrandAuthUpdateRequest request,
    BrandAuthorisationWip brandAuthorisationWipDb) {
    if (Objects.isNull(brandAuthorisationWipDb)) {
      brandAuthorisationWipDb = new BrandAuthorisationWip();
      ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getIprRegistrationNumber()),
        ErrorMessage.IPR_REGISTRATION_NUMBER_CANNOT_BE_EMPTY.getMessage());
    }
    BeanUtils.copyProperties(request, brandAuthorisationWipDb);
    brandAuthorisationWipDb.setMarkForDelete(false);
    brandAuthorisationWipDb.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthorisationWipDb.setDocumentLink(String.join(Constants.COMMA, request.getDocumentLinks()));
    return brandAuthorisationWipDb;
  }


  public static BrandAuthWipDetailResponse convertToBrandAuthWipDetailResponse(
    BrandAuthorisationWip brandAuthorisationWip, Date currentAuthStartDate, Date currentAuthEndDate) {
    BrandAuthWipDetailResponse brandAuthWipDetailResponse = new BrandAuthWipDetailResponse();
    Date date = new Date();
    BeanUtils.copyProperties(brandAuthorisationWip, brandAuthWipDetailResponse, DOCUMENT_LINK);
    brandAuthWipDetailResponse.setStatus(brandAuthorisationWip.getAuthorisationStatus().name());
    if (StringUtils.isNotEmpty(brandAuthorisationWip.getDocumentLink())) {
      brandAuthWipDetailResponse.setDocumentList(
        Arrays.stream(brandAuthorisationWip.getDocumentLink().split(Constants.COMMA)).toList());
    }
    //Mark status as Expired if end date has crossed
    if (BrandAuthorizationWipStatus.ACTIVE.equals(brandAuthorisationWip.getAuthorisationStatus())
      && date.after(brandAuthorisationWip.getAuthExpireDate())) {
      brandAuthWipDetailResponse.setStatus(BrandAuthorisationStatus.EXPIRED.name());
    }
    brandAuthWipDetailResponse.setCurrentAuthStartDate(currentAuthStartDate);
    brandAuthWipDetailResponse.setCurrentAuthExpireDate(currentAuthEndDate);
    return brandAuthWipDetailResponse;
  }

  public static void validateApprovalDates(Date date, BrandAuthorisationWipActionRequest request) {
    GdnPreconditions.checkArgument(Objects.nonNull(request.getAuthStartDate()),
        ErrorMessage.AUTH_START_OR_END_DATE_MUST_NOT_BE_NULL.getMessage());
    GdnPreconditions.checkArgument(Objects.nonNull(request.getAuthExpireDate()),
        ErrorMessage.AUTH_START_OR_END_DATE_MUST_NOT_BE_NULL.getMessage());
    if (request.getAuthStartDate().after(request.getAuthExpireDate())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "Authorization start date " + request.getAuthStartDate()
              + " should not be after authorization expire date " + request.getAuthExpireDate());
    }
    if (date.after(request.getAuthExpireDate())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "Authorization end date " + request.getAuthExpireDate()
              + " cannot be before present date ");
    }
  }

  public static Date fetchDateFromThreshold(int day) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, day);
    calendar.set(Calendar.HOUR_OF_DAY, Constants.TWENTY_THIRD_HOUR);
    calendar.set(Calendar.MINUTE, Constants.FIFTY_NINE);
    calendar.set(Calendar.SECOND, Constants.FIFTY_NINE);
    return calendar.getTime();
  }

  public static BrandAuthActivateEventModel toBrandAuthActivateEventModel(
    BrandAuthorisationWip brandAuthorisationWip) {
    return BrandAuthActivateEventModel.builder().id(brandAuthorisationWip.getId())
      .brandCode(brandAuthorisationWip.getBrandCode())
      .sellerCode(brandAuthorisationWip.getSellerCode()).storeId(brandAuthorisationWip.getStoreId())
      .build();
  }

  public static BrandAuthorisationWip convertBrandAuthCreateRequestToBrandAuthWip(String storeId,
      BrandAuthCreateRequest brandAuthCreateRequest) {
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    brandAuthorisationWip.setStoreId(storeId);
    if (CollectionUtils.isNotEmpty(brandAuthCreateRequest.getDocumentLinks())) {
      brandAuthorisationWip.setDocumentLink(
          String.join(Constants.COMMA, brandAuthCreateRequest.getDocumentLinks()));
    }
    return brandAuthorisationWip;
  }

  public static String generateDateForHistory(Date startDate, Date endDate, DateFormat dateFormat) {
    return Constants.OPEN_BRACKET.concat(dateFormat.format(new Date(startDate.getTime())))
      .concat(Constants.HYPHEN).concat(dateFormat.format(new Date(endDate.getTime())))
      .concat(Constants.CLOSING_BRACKET);
  }

  public static BrandAuthorisationHistory convertToBrandAuthRequestedHistory(
    BrandAuthUpdateRequest brandAuthUpdateRequest, DateFormat dateFormat) {
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    String activity = BrandAuthorisationActivity.REQUESTED.getDescription().concat(
      generateDateForHistory(brandAuthUpdateRequest.getAuthStartDate(),
        brandAuthUpdateRequest.getAuthExpireDate(), dateFormat));
    brandAuthorisationHistory.setActivity(activity);
    brandAuthorisationHistory.setBrandCode(brandAuthUpdateRequest.getBrandCode());
    brandAuthorisationHistory.setSellerCode(brandAuthUpdateRequest.getSellerCode());
    return brandAuthorisationHistory;
  }

  public static List<ProductSuitabilityAttributeModel> removeNullAttribute(
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList) {

    List<ProductSuitabilityAttributeModel> filteredList =
      productSuitabilityAttributeModelList.stream().filter(attribute -> {
        String value = attribute.getValue();
        return Objects.nonNull(value) && StringUtils.isNotBlank(value);
      }).toList();

    return filteredList.isEmpty() ? List.of() : filteredList;
  }

  public static BasicInfoProductResponse toBasicInfoProductResponse(List<Image> imageList, Product product) {
    BasicInfoProductResponse basicInfoProductResponse = new BasicInfoProductResponse();
    basicInfoProductResponse.setCommonImageList(imageList);
    basicInfoProductResponse.setProductCode(product.getProductCode());
    basicInfoProductResponse.setProductName(product.getName());
    basicInfoProductResponse.setBrand(product.getBrand());
    basicInfoProductResponse.setDescription(new String(product.getDescription()));
    basicInfoProductResponse.setLength(product.getLength());
    basicInfoProductResponse.setWeight(product.getWeight());
    basicInfoProductResponse.setWidth(product.getWidth());
    basicInfoProductResponse.setHeight(product.getHeight());
    basicInfoProductResponse.setShippingWeight(product.getShippingWeight());
    basicInfoProductResponse.setVideo(product.getVideo());
    return basicInfoProductResponse;
  }

  public static String convertVideoAddEditRequestToDTO(VideoAddEditRequest videoAddEditRequest) {
    try {
      ObjectMapper mapper = new ObjectMapper();
      VideoDTO videoDTO = new VideoDTO();
      BeanUtils.copyProperties(videoAddEditRequest, videoDTO);
      videoDTO.setSourceUrl(videoAddEditRequest.getVideoUrl());
      return mapper.writeValueAsString(videoDTO);
    } catch (Exception e) {
      log.error("Failed to convert VideoAddEditRequest {} to JSON DTO", videoAddEditRequest);
      return StringUtils.EMPTY;
    }
  }

  public static ProductItemUomInfo convertToProductItemUomInfo(
      ProductItemUomInfoDTO productItemUomInfoDTO, String productCode, String sellerCode,
      ProductItem productItem, ObjectMapper objectMapper) throws JsonProcessingException {
    ProductItemUomInfo productItemUomInfo = new ProductItemUomInfo();
    productItemUomInfo.setProductCode(productCode);
    productItemUomInfo.setProductItem(productItem);
    productItemUomInfo.setSkuCode(productItemUomInfoDTO.getSkuCode());
    productItem.setOmniChannelSku(
        productItemUomInfoDTO.getDistributionItemInfoRequest().getOmniChannelSku());
    productItemUomInfo.setSellerCode(sellerCode);
    productItemUomInfo.setUom(
        objectMapper.writeValueAsString(productItemUomInfoDTO.getDimensionAndUomDTOList()));
    productItemUomInfo.setOrigin(
        Origin.valueOf(productItemUomInfoDTO.getDistributionItemInfoRequest().getOrigin()));
    productItemUomInfo.setExpiry(productItemUomInfoDTO.getDistributionItemInfoRequest().isExpiry());
    return productItemUomInfo;
  }
}

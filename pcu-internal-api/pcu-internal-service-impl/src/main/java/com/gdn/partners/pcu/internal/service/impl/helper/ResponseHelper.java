package com.gdn.partners.pcu.internal.service.impl.helper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.request.Margin;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductDetailCompleteResponse;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.web.model.enums.SellerStatus;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthFilterResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryModelFeedbackResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.AiGeneratedFieldsResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryCodeAndCategoryNameResponse;
import com.gda.mta.product.dto.ItemNeedRevisionNotes;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHData;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.ImageQcConstants;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.web.model.enums.WorkflowWebState;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.DescriptiveAttributeValueTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.UserFeedbackRequest;
import com.gdn.partners.pcu.internal.web.model.request.UserImageFeedbackRequest;
import com.gdn.partners.pcu.internal.web.model.response.AllowedAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.AttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandModelFeedBackResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CatalogTreeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryTreeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFeedbackResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFeedbackWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageQcPredictionResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageQcResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ItemNotesWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MasterCategoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;
import com.gdn.partners.pcu.internal.web.model.response.NeedRevisionNotesWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.PendingDownloadProcessWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.PreOrderResponse;
import com.gdn.partners.pcu.internal.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.PredictionCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCategoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductModelFeedback;
import com.gdn.partners.pcu.internal.web.model.response.ProductRevisionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSkuUpdateHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductUpdateHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RestrictedKeywordsDetectedWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RestrictionModelFeedBackResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.clientsdk.shade.com.google.common.collect.ImmutableSet;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductAttributeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductItemResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

import lombok.extern.slf4j.Slf4j;

import static com.gdn.partners.pcu.internal.model.Constants.NEAR_EXPIRY;

/**
 * @author Pradeep Reddy
 */
@Slf4j
public class ResponseHelper {

  private static final String OLD_VALUE = "oldValue: ";
  private static final String OLD_VALUE_REPLACE = "oldValue: '";
  private static final String NEW_VALUE = ", newValue: ";
  private static final String NEW_VALUE_REPLACE = "', newValue: '";
  private static final String FLOWER_BRACKET = "}";
  private static final String FLOWER_BRACKET_REPLACE = "'}";
  private static final String REMOVING_NEWLINE_REGEX = "[\\t\\n\\r]+";
  private static final Pattern NOTES_REGEX = Pattern.compile(Constants.PRODUCT_HISTORY_NOTES_REGEX);
  private static final Pattern FIELD_REGEX = Pattern.compile(Constants.FIELD_HISTORY_REGEX);
  private static final Pattern OLD_VALUE_REGEX = Pattern.compile(Constants.OLD_VALUE_REGEX);
  private static final Pattern NEW_VALUE_REGEX = Pattern.compile(Constants.NEW_VALUE_REGEX);
  private static final String WARNA_ATTRIBUTE_NAME = "warna";
  private static final String DAYS_ADDED = "daysAdded";
  private static final String TODAY = "today";
  private static final String YESTERDAY = "yesterday";
  private static final String TWO_DAYS_AGO = "twoDaysAgo";
  private static final String BETWEEN_3_UNTIL_5_DAYS_OLD = "threeToFiveDays";
  private static final String MORE_THAN_5_DAYS = "moreThanFiveDaysAgo";
  private static final String CURLEY_BRACES = "\\{|\\}";
  private static final String START_FIELD = "field:";
  private static final String START_FIELD_REPLACE = "{field:";
  private static final String END_FIELD = ", \\{field:";
  private static final String END_FIELD_REPLACE = "}, {field:";
  private static final String CLOSING_BRACKETS = "}]";
  private static final String OPENING_BRACKETS = "[{";
  private static final String AUTO_NEED_REVISION = "Auto need revision";
  private static final String FORCE_REVIEW = "Force review";
  private static final String INTERNAL = "INTERNAL";
  private static final String EXTERNAL = "EXTERNAL";
  private static final String ACTIVE = "Active";
  private static final String SUSPENDED = "Suspended";
  private static final String DEFAULT_LABEL_COLOUR = "BliLabelSuccess";

  public static boolean validateResponse(GdnRestListResponse clientResponse) {

    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateBrandResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }

    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(clientResponse.getErrorCode())) {
      throw new ClientException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE), clientResponse.getErrorCode());
    }
    return true;
  }

  public static boolean validateMasterSkuResponse(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }

    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(clientResponse.getErrorCode())) {
      throw new ConstraintViolationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
    return true;
  }

  public static boolean validateMasterSkuResponse(GdnRestListResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }

    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(clientResponse.getErrorCode())) {
      throw new ConstraintViolationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
    return true;
  }

  public static boolean validateErrorCodeResponse(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode()
        .equals(clientResponse.getErrorCode())) {
      throw new ConstraintViolationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess() && (StringUtils.isNotEmpty(clientResponse.getErrorCode())
        && !ErrorCategory.UNSPECIFIED.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(),
          clientResponse.getErrorCode());
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateMasterSkuResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }

    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(clientResponse.getErrorCode())) {
      throw new ConstraintViolationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
    return true;
  }

  public static boolean validateResponse(GdnRestSimpleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()||Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(ListBaseResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getContent())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(BPJPHListResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (clientResponse.isError() || Objects.isNull(clientResponse.getData())) {
      throw new ClientException(ErrorMessages.BPJPH_RESPONSE_FAILED);
    }
    return true;
  }

  public static List<Image> toImages(List<Image> imageResponseList){
    return imageResponseList.stream().map(ResponseHelper::toImage).collect(Collectors.toList());
  }

  public static Image toImage(Image imageResponse){
    Image itemImageRequest = new Image();
    BeanUtils.copyProperties(imageResponse, itemImageRequest);
    return itemImageRequest;
  }

  public static ProductDetailWebResponse toProductDetailWebResponse(ProductDetailResponse productDetailResponse,
     ProfileResponse profileResponse) {
    ProductDetailWebResponse productDetailWebResponse = new ProductDetailWebResponse();
    BeanUtils.copyProperties(productDetailResponse, productDetailWebResponse, "productItemResponses",
        "productAttributeResponses", "productCategoryResponses", "categories", "images", "restrictedKeywordsDetected");
    LinkedHashSet<ProductItemWebResponse> productItemResponses =
        Optional.ofNullable(productDetailResponse.getProductItemResponses()).orElse(new LinkedHashSet<>()).stream()
            .sorted(Comparator.comparing(ProductItemResponse::getGeneratedItemName))
            .map(ResponseHelper::toProductItemResponse).collect(Collectors.toCollection(LinkedHashSet::new));
    productDetailWebResponse.setProductItemResponses(groupItemsByWarna(productItemResponses));
    List<ProductAttributeWebResponse> productAttributeResponses =
        Optional.ofNullable(productDetailResponse.getProductAttributeResponses()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductAttributeWebResponse).collect(Collectors.toList());
    productDetailWebResponse.setProductAttributeResponses(productAttributeResponses);
    List<ImageWebResponse> productImageResponses =
        Optional.ofNullable(productDetailResponse.getImages()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toImageWebResponse).collect(Collectors.toList());
    productDetailWebResponse.setImages(productImageResponses);
    List<ProductCategoryWebResponse> productCategoryResponses =
        Optional.ofNullable(productDetailResponse.getProductCategoryResponses()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductCategoryWebResponse).collect(Collectors.toList());
    productDetailWebResponse.setProductCategoryResponses(productCategoryResponses);
    if(CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses())) {
      productDetailWebResponse.setDocumentType(productDetailResponse.getProductCategoryResponses()
          .get(0).getCategory().getDocumentType());
    }
    if (CollectionUtils.isNotEmpty(productDetailResponse.getCategories())) {
      productDetailWebResponse.setCategories(productDetailResponse.getCategories());
    }
    if (Objects.nonNull(profileResponse)) {
      productDetailWebResponse.setInternationalFlag(profileResponse.getCompany().isInternationalFlag());
      productDetailWebResponse.setCommissionType(profileResponse.getCompany().getMerchantType());
      productDetailWebResponse.setSellerStatus(populateSellerStatus(profileResponse));
      productDetailWebResponse.setBusinessPartnerName(profileResponse.getCompany().getBusinessPartnerName());
    }
    productDetailWebResponse.setCategoriesEnglish(productDetailResponse.getCategoriesEnglish());
    return productDetailWebResponse;
  }

  public static Set<ProductItemWebResponse> groupItemsByWarna(LinkedHashSet<ProductItemWebResponse> itemWebResponses) {
    TreeMap<String, List<ProductItemWebResponse>> treeMap = new TreeMap<>();
    if (CollectionUtils.isNotEmpty(itemWebResponses)) {
      for (ProductItemWebResponse productItemWebResponse : itemWebResponses) {
        if (CollectionUtils.isNotEmpty(productItemWebResponse.getProductItemAttributeValueResponses())) {
          for (ProductItemAttributeValueWebResponse productItemAttributeValueWebResponse : productItemWebResponse
              .getProductItemAttributeValueResponses()) {
            if (WARNA_ATTRIBUTE_NAME.equalsIgnoreCase(productItemAttributeValueWebResponse.getAttribute().getName())
                && isDefiningOrVariantCreation(productItemAttributeValueWebResponse)) {
              if (!treeMap.containsKey(productItemAttributeValueWebResponse.getValue())) {
                List<ProductItemWebResponse> list = new ArrayList<>();
                list.add(productItemWebResponse);
                treeMap.put(productItemAttributeValueWebResponse.getValue(), list);
              } else {
                treeMap.get(productItemAttributeValueWebResponse.getValue()).add(productItemWebResponse);
              }
            }
          }
        }
      }
    }

    if (treeMap.isEmpty()) {
      return itemWebResponses;
    }

    Iterator it = treeMap.entrySet().iterator();
    Set<ProductItemWebResponse> productItemWebResponseSet = new LinkedHashSet<>();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      List<ProductItemWebResponse> productItemWebResponseList = (List<ProductItemWebResponse>) pair.getValue();
      Collections.sort(productItemWebResponseList, Comparator.comparing(ProductItemWebResponse::getGeneratedItemName));
      for (ProductItemWebResponse productItemWebResponse : productItemWebResponseList) {
        productItemWebResponseSet.add(productItemWebResponse);
      }
      it.remove(); // avoids a ConcurrentModificationException
    }
    return productItemWebResponseSet;
  }

  private static boolean isDefiningOrVariantCreation(
      ProductItemAttributeValueWebResponse productItemAttributeValueWebResponse) {
    return AttributeType.DEFINING_ATTRIBUTE.toString()
        .equals(productItemAttributeValueWebResponse.getAttribute().getAttributeType())
        || productItemAttributeValueWebResponse.getAttribute().isVariantCreation();
  }

  private static ProductItemWebResponse toProductItemResponse(ProductItemResponse productItemResponse) {
    ProductItemWebResponse productItemWebResponse = new ProductItemWebResponse();
    BeanUtils
        .copyProperties(productItemResponse, productItemWebResponse, "productItemAttributeValues", "productItemImages");
    List<ImageWebResponse> imageWebResponses =
        Optional.ofNullable(productItemResponse.getImages()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toImageWebResponse).collect(Collectors.toList());
    List<ProductItemAttributeValueWebResponse> attributeValueWebResponses =
        Optional.ofNullable(productItemResponse.getProductItemAttributeValueResponses()).orElse(new ArrayList<>())
            .stream().map(ResponseHelper::toProductItemAttributeValueWebResponse).collect(Collectors.toList());
    productItemWebResponse.setImages(imageWebResponses);
    productItemWebResponse.setProductItemAttributeValueResponses(attributeValueWebResponses);
    productItemWebResponse.setNewlyAdded(productItemResponse.isNewlyAddedItem());
    return productItemWebResponse;
  }

  public static String createPdpRedirectionUrl(String pdpLinkPrefix, String itemSku) {
    StringBuilder stringBuilder = new StringBuilder();
    return stringBuilder.append(pdpLinkPrefix).append(itemSku).toString();
  }

  private static ProductAttributeWebResponse toProductAttributeWebResponse(
      ProductAttributeResponse productAttributeResponse) {
    ProductAttributeWebResponse productAttributeWebResponse = new ProductAttributeWebResponse();
    AttributeWebResponse attributeWebResponse = new AttributeWebResponse();
    BeanUtils
        .copyProperties(productAttributeResponse, productAttributeWebResponse, "productAttributeValues", "attribute");
    if (Objects.nonNull(productAttributeResponse.getAttribute())) {
      BeanUtils.copyProperties(productAttributeResponse.getAttribute(), attributeWebResponse);
      productAttributeWebResponse.setAttribute(attributeWebResponse);
    }
    List<ProductAttributeValueWebResponse> productAttributeValueWebResponses =
        Optional.ofNullable(productAttributeResponse.getProductAttributeValues()).orElse(new ArrayList<>()).stream().
            map(ResponseHelper::toProductAttributeValueWebResponse).collect(Collectors.toList());
    productAttributeWebResponse.setProductAttributeValues(productAttributeValueWebResponses);
    return productAttributeWebResponse;
  }

  private static ImageWebResponse toImageWebResponse(Image image) {
    ImageWebResponse imageWebResponse = new ImageWebResponse();
    BeanUtils.copyProperties(image, imageWebResponse, "originalImage");
    imageWebResponse.setOriginalImage(image.getOriginalImage());
    return imageWebResponse;
  }

  private static ProductCategoryWebResponse toProductCategoryWebResponse(
      ProductCategoryResponse productCategoryResponse) {
    ProductCategoryWebResponse productCategoryWebResponse = new ProductCategoryWebResponse();
    BeanUtils.copyProperties(productCategoryResponse, productCategoryWebResponse, "category");
    if (Objects.nonNull(productCategoryResponse.getCategory())) {
      BeanUtils.copyProperties(productCategoryResponse.getCategory(), productCategoryWebResponse);
    }
    return productCategoryWebResponse;
  }

  private static ProductItemAttributeValueWebResponse toProductItemAttributeValueWebResponse(
      ProductItemAttributeValueResponse response) {
    ProductItemAttributeValueWebResponse attributeValueWebResponse = new ProductItemAttributeValueWebResponse();
    attributeValueWebResponse.setId(response.getId());
    AttributeWebResponse attributeWebResponse = new AttributeWebResponse();
    if (Objects.nonNull(response.getAttributeResponse())) {
      BeanUtils.copyProperties(response.getAttributeResponse(), attributeWebResponse);
      attributeValueWebResponse.setAttribute(attributeWebResponse);
    }
    attributeValueWebResponse.setValue(response.getValue());
    return attributeValueWebResponse;
  }

  private static ProductAttributeValueWebResponse toProductAttributeValueWebResponse(
      ProductAttributeValueResponse productAttributeValueResponse) {
    ProductAttributeValueWebResponse productAttributeValueWebResponse = new ProductAttributeValueWebResponse();
    BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueWebResponse, "allowedAttributeValue",
        "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
    AllowedAttributeValueWebResponse allowedAttributeValueWebResponse = new AllowedAttributeValueWebResponse();
    PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse = new PredefinedAttributeValueWebResponse();
    if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
      BeanUtils
          .copyProperties(productAttributeValueResponse.getAllowedAttributeValue(), allowedAttributeValueWebResponse);
      productAttributeValueWebResponse.setAllowedAttributeValue(allowedAttributeValueWebResponse);
    }
    if(Objects.nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
      BeanUtils.copyProperties(productAttributeValueResponse.getPredefinedAllowedAttributeValue(),
          predefinedAttributeValueWebResponse);
      productAttributeValueWebResponse.setPredefinedAllowedAttributeValue(predefinedAttributeValueWebResponse);
    }
    if(Objects.nonNull(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
      productAttributeValueWebResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb
          .valueOf(productAttributeValueResponse.getDescriptiveAttributeValueType().name()));
    }
    return productAttributeValueWebResponse;
  }

  public static FilterCountWebResponse toFilterCountWebResponse(FilterCountResponse filterCountResponse) {
    FilterCountWebResponse filterCountWebResponse = new FilterCountWebResponse();
    BeanUtils.copyProperties(filterCountResponse, filterCountWebResponse);
    return filterCountWebResponse;
  }

  public static ProductImagePredictionWebResponse toProductImagePredictionWebResponse(
      ProductImagePredictionResponse productImagePredictionResponse) {
    ProductImagePredictionWebResponse productImagePredictionWebResponse = new ProductImagePredictionWebResponse();
    BeanUtils.copyProperties(productImagePredictionResponse, productImagePredictionWebResponse);
    productImagePredictionWebResponse.setRuleEnabled(true);
    if (!productImagePredictionResponse.isNeedRevisionEnabled() && !productImagePredictionResponse.isForceReview()) {
      productImagePredictionWebResponse.setRuleType(null);
      productImagePredictionWebResponse.setRuleEnabled(false);
    } else if (productImagePredictionResponse.isNeedRevisionEnabled()) {
      productImagePredictionWebResponse.setRuleType(AUTO_NEED_REVISION);
      productImagePredictionWebResponse.setRuleThreshold(productImagePredictionResponse.getNeedRevisionConfidenceThreshold());
    } else {
      productImagePredictionWebResponse.setRuleType(FORCE_REVIEW);
      productImagePredictionWebResponse.setRuleThreshold(productImagePredictionResponse.getConfidenceThreshold());
    }
    return productImagePredictionWebResponse;
  }

  public static List<ProductImagePredictionWebResponse> toListOfProductImagePredictionWebResponse(
      List<ProductImagePredictionResponse> productImagePredictionResponseList) {
    return productImagePredictionResponseList.stream().map(ResponseHelper::toProductImagePredictionWebResponse)
        .collect(Collectors.toList());
  }

  public static List<ProductCollectionWebResponse> toProductCollectionWebResponseList(
      List<ProductCollectionResponse> responses) {
    return responses.stream().map(ResponseHelper::toProductCollectionWebResponse).collect(Collectors.toList());
  }

  private static ProductCollectionWebResponse toProductCollectionWebResponse(
      ProductCollectionResponse productCollectionResponse) {
    ProductCollectionWebResponse productCollectionWebResponse = new ProductCollectionWebResponse();
    BeanUtils.copyProperties(productCollectionResponse, productCollectionWebResponse);
    return productCollectionWebResponse;
  }

  public static List<BusinessPartnerWebResponse> toBusinessPartnerWebResponseList(List<ProductBusinessPartnerMapperResponse> responses) {
    return responses.stream().map(ResponseHelper::toBusinessPartnerWebResponse).collect(Collectors.toList());
  }

  public static List<BusinessPartnerWebResponse> toBusinessPartnerWebResponseListFromProfileResponse(List<ProfileResponse> responses) {
    return responses.stream().map(ResponseHelper::toBusinessPartnerWebResponseFromProfileResponse).collect(Collectors.toList());
  }

  private static BusinessPartnerWebResponse toBusinessPartnerWebResponse(ProductBusinessPartnerMapperResponse mapperResponse) {
    return BusinessPartnerWebResponse.builder().businessPartnerCode(mapperResponse.getBusinessPartnerCode())
        .businessPartnerName(mapperResponse.getBusinessPartnerName()).build();
  }

  private static BusinessPartnerWebResponse toBusinessPartnerWebResponseFromProfileResponse(ProfileResponse mapperResponse) {
    String businessPartnerName = StringUtils.EMPTY;
    if (Objects.nonNull(mapperResponse.getCompany().getBusinessPartnerName())) {
      businessPartnerName = mapperResponse.getCompany().getBusinessPartnerName();
    }
    return BusinessPartnerWebResponse.builder().businessPartnerCode(mapperResponse.getBusinessPartnerCode())
        .businessPartnerName(businessPartnerName).build();
  }

  public static List<ProductHistoryWebResponse> toProductHistoryWebResponseList(
      List<ProductHistoryResponse> responses) {
    return responses.stream().map(ResponseHelper::toProductHistoryWebResponse).collect(Collectors.toList());
  }

  private static ProductHistoryWebResponse toProductHistoryWebResponse(ProductHistoryResponse productHistoryResponse) {
    ProductHistoryWebResponse productHistoryWebResponse = new ProductHistoryWebResponse();
    BeanUtils.copyProperties(productHistoryResponse, productHistoryWebResponse, "notes");
    if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE
        .equals(productHistoryWebResponse.getDescription())) {
      String notes = productHistoryResponse.getNotes().replaceAll(REMOVING_NEWLINE_REGEX, StringUtils.SPACE)
          .replaceAll(CURLEY_BRACES, StringUtils.EMPTY).replaceAll(START_FIELD, START_FIELD_REPLACE)
          .replaceAll(END_FIELD, END_FIELD_REPLACE);
      if (notes.contains(OPENING_BRACKETS)) {
        notes = notes.substring(0, notes.length() - 1) + CLOSING_BRACKETS;
      }
      Matcher matcher = NOTES_REGEX.matcher(notes);
      List<String> notesList = new ArrayList<>();
      while (matcher.find()) {
        notesList.add(matcher.group().replace(OLD_VALUE, OLD_VALUE_REPLACE).
            replace(NEW_VALUE, NEW_VALUE_REPLACE).replace(FLOWER_BRACKET, FLOWER_BRACKET_REPLACE));
      }
      if (CollectionUtils.isEmpty(notesList)) {
        productHistoryWebResponse.setNotes(Collections.singletonList(notes));
      } else {
        getProductUpdateHistoryResponse(productHistoryWebResponse, notesList);
      }
    }
    return productHistoryWebResponse;
  }

  private static void getProductUpdateHistoryResponse(ProductHistoryWebResponse productHistoryWebResponse,
      List<String> notesList) {
    Matcher matcher;
    List<ProductUpdateHistoryWebResponse> productUpdateHistoryWebResponses = new ArrayList<>();
    for (String note : notesList) {
      ProductUpdateHistoryWebResponse productUpdateHistoryWebResponse = new ProductUpdateHistoryWebResponse();
      matcher = FIELD_REGEX.matcher(note);
      if (matcher.find()) {
        productUpdateHistoryWebResponse.setField(matcher.group());
      }
      matcher = OLD_VALUE_REGEX.matcher(note);
      if (matcher.find()) {
        productUpdateHistoryWebResponse.setOldValue(matcher.group());
      }
      matcher = NEW_VALUE_REGEX.matcher(note);
      if (matcher.find()) {
        productUpdateHistoryWebResponse.setNewValue(matcher.group());
      }
      productUpdateHistoryWebResponses.add(productUpdateHistoryWebResponse);
    }
    productHistoryWebResponse.setProductUpdateHistoryWebResponseList(productUpdateHistoryWebResponses);
    productHistoryWebResponse.setNotes(notesList);
  }

  public static List<String> toAssigneeResponse(List<AssigneeResponse> assigneeResponses) {
    return assigneeResponses.stream().map(AssigneeResponse::getAssignee).collect(Collectors.toList());
  }

  public static String validateAndGetPristineSupportedCategory(String categoryId,
      Map<String, Set<String>> supportedCategories) {
    String supportedCategory = StringUtils.EMPTY;
    if (MapUtils.isNotEmpty(supportedCategories)) {
      for (Map.Entry<String, Set<String>> entry : supportedCategories.entrySet()) {
        if (CollectionUtils.isNotEmpty(entry.getValue()) && entry.getValue().contains(categoryId)) {
          supportedCategory = entry.getKey();
          break;
        }
      }
    }
    return supportedCategory;
  }

  public static List<ProductSuggestionWebResponse> toProductSuggestionWebResponseList(
      List<ProductCodeResponse> productCodeResponses) {
    return Optional.ofNullable(productCodeResponses)
        .orElseGet(Collections::emptyList).stream()
        .map(ResponseHelper::toProductSuggestionWebResponse)
        .collect(Collectors.toList());
  }

  public static ProductSuggestionWebResponse toProductSuggestionWebResponse(ProductCodeResponse productCodeResponse) {
    ProductSuggestionWebResponse productSuggestionWebResponse = new ProductSuggestionWebResponse();
    BeanUtils.copyProperties(productCodeResponse, productSuggestionWebResponse);
    return productSuggestionWebResponse;
  }

  public static List<ProductRevisionHistoryWebResponse> toProductRevisionHistoryWebResponseList(
      List<ProductRevisionInfoResponse> productRevisionInfoResponses) {
    return productRevisionInfoResponses.stream().map(ResponseHelper::toProductRevisionHistoryWebResponse)
        .collect(Collectors.toList());
  }

  private static ProductRevisionHistoryWebResponse toProductRevisionHistoryWebResponse(
      ProductRevisionInfoResponse productRevisionInfoResponse) {
    List<ItemNotesWebResponse> itemResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productRevisionInfoResponse.getItemNotes())) {
      for (ItemNeedRevisionNotes itemNeedRevisionNotes : productRevisionInfoResponse.getItemNotes()) {
        ItemNotesWebResponse itemNotesWebResponse = new ItemNotesWebResponse();
        BeanUtils.copyProperties(itemNeedRevisionNotes, itemNotesWebResponse);
        itemResponseList.add(itemNotesWebResponse);
      }
    }
    return ProductRevisionHistoryWebResponse.builder().id(productRevisionInfoResponse.getId())
        .storeId(productRevisionInfoResponse.getStoreId())
        .correctionReason(productRevisionInfoResponse.getCorrectionReason())
        .additionalNotes(productRevisionInfoResponse.getAdditionalNotes())
        .createdBy(productRevisionInfoResponse.getCreatedBy()).createdDate(productRevisionInfoResponse.getCreatedDate())
        .updatedBy(productRevisionInfoResponse.getUpdatedBy()).updatedDate(productRevisionInfoResponse.getUpdatedDate())
        .allVariants(productRevisionInfoResponse.isAllVariants())
        .contentAdditionalNotes(productRevisionInfoResponse.getContentAdditionalNotes())
        .imageReason(productRevisionInfoResponse.getImageReason())
        .imagesAdditionalNotes(productRevisionInfoResponse.getImagesAdditionalNotes())
        .vendorErrorFields(productRevisionInfoResponse.getVendorErrorFields())
        .vendorNotes(productRevisionInfoResponse.getVendorNotes()).itemNotes(itemResponseList).build();
  }

  public static String categoryChangeCheck(List<CategoryAttributeResponse> presentAttribute,
      List<CategoryAttributeResponse> targetAttribute, List<ProductAttributeResponse> productAttributeResponses) {
    Set<String> presentDefiningAttribute = presentAttribute.stream().filter(ResponseHelper::isDefiningOrVariantCreation)
        .map(p -> p.getAttribute().getAttributeCode()).collect(Collectors.toSet());
    Set<String> targetDefiningAttribute = targetAttribute.stream().filter(ResponseHelper::isDefiningOrVariantCreation)
        .map(p -> p.getAttribute().getAttributeCode()).collect(Collectors.toSet());
    Set<String> productDefiningAttribute =
        productAttributeResponses.stream().filter(ResponseHelper::isDefiningOrVariantCreationProductAttribute)
            .map(productAttributeResponse -> productAttributeResponse.getAttribute().getAttributeCode())
            .collect(Collectors.toSet());
    if (presentDefiningAttribute.size() != targetDefiningAttribute.size()
        || productDefiningAttribute.size() != targetDefiningAttribute.size()) {
      return ErrorMessages.DEFINING_ATTRIBUTE_MISMATCH;
    }
    return presentDefiningAttribute.containsAll(targetDefiningAttribute) && productDefiningAttribute
        .containsAll(targetDefiningAttribute) ? StringUtils.EMPTY : ErrorMessages.DEFINING_ATTRIBUTE_MISMATCH;
  }

  private static boolean isDefiningOrVariantCreationProductAttribute(ProductAttributeResponse productAttributeResponse) {
    return AttributeTypeWeb.DEFINING_ATTRIBUTE.name().equals(productAttributeResponse.getAttribute()
        .getAttributeType()) || productAttributeResponse.getAttribute().isVariantCreation();
  }

  private static boolean isDefiningOrVariantCreation(CategoryAttributeResponse categoryAttributeResponse) {
    return
        AttributeTypeWeb.DEFINING_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType())
            || categoryAttributeResponse.getAttribute().isVariantCreation();
  }

  public static BrandRejectionWebResponse toBrandRejectionWebResponse(BrandRejectionInfoResponse response) {
    return BrandRejectionWebResponse.builder().id(response.getId()).brandName(response.getBrandName())
        .brandRequestCode(response.getBrandRequestCode()).rejectionReason(response.getRejectionReason()).build();
  }

  public static BrandWebResponse toBrandWebResponse(BrandResponse brandResponse) {
    return BrandWebResponse.builder().id(brandResponse.getId())
      .brandCode(brandResponse.getBrandCode()).brandDescription(brandResponse.getBrandDescription())
      .brandLogoPath(brandResponse.getBrandLogoPath()).brandName(brandResponse.getBrandName())
      .profileBannerPath(brandResponse.getProfileBannerPath())
      .validBrand(brandResponse.isValidBrand()).protectedBrand(brandResponse.isProtectedBrand())
      .skuCreationAllowedForAllSellers(brandResponse.isSkuCreationAllowedForAllSellers()).build();
  }

  public static List<ProductBusinessPartnerMapperWebResponse>
  convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponse(
      GdnRestListResponse<ProductBusinessPartnerMapperResponse> list) {
    ProductBusinessPartnerMapperWebResponse productBusinessPartnerMapperWebResponse;
    List<ProductBusinessPartnerMapperWebResponse> response = new ArrayList<>();
    for (ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse : list.getContent()) {
      if (!productBusinessPartnerMapperResponse.getBusinessPartnerCode().equals(INTERNAL)
          && !productBusinessPartnerMapperResponse.getBusinessPartnerCode().equals(EXTERNAL)) {
        productBusinessPartnerMapperWebResponse = new ProductBusinessPartnerMapperWebResponse();
        productBusinessPartnerMapperWebResponse
            .setBusinessPartnerName(productBusinessPartnerMapperResponse.getBusinessPartnerName());
        productBusinessPartnerMapperWebResponse
            .setBusinessPartnerCode(productBusinessPartnerMapperResponse.getBusinessPartnerCode());
        response.add(productBusinessPartnerMapperWebResponse);
      }
    }
    return response;
  }

  public static List<AssigneeWebResponse> convertAssigneeResponseToAssigneeWebResponse(
      GdnRestListResponse<VendorAssigneeResponse> list) {
    AssigneeWebResponse assigneeWebResponse;
    List<AssigneeWebResponse> response = new ArrayList<>();
    for (VendorAssigneeResponse assigneeResponse : list.getContent()) {
      assigneeWebResponse = new AssigneeWebResponse();
      assigneeWebResponse.setAssigneeEmailId(assigneeResponse.getAssigneeEmailId());
      response.add(assigneeWebResponse);
    }
    return response;
  }

  public static List<ProductHistoryResponse> fromTaskHistoryResponseToProductHistoryResponse(
      List<TaskHistoryResponse> taskHistoryResponsesList){
    List<ProductHistoryResponse> productHistoryResponseList = new ArrayList<>();
    for(TaskHistoryResponse taskHistoryResponse : taskHistoryResponsesList){
      ProductHistoryResponse productHistoryResponse = new ProductHistoryResponse();
      productHistoryResponse.setProductId(taskHistoryResponse.getProductCode());
      productHistoryResponse.setDescription(taskHistoryResponse.getState());
      productHistoryResponse.setNotes(taskHistoryResponse.getReason());
      productHistoryResponse.setUpdatedBy(taskHistoryResponse.getUpdatedBy());
      productHistoryResponse.setCreatedBy(taskHistoryResponse.getCreatedBy());
      productHistoryResponse.setUpdatedDate(taskHistoryResponse.getUpdatedDate());
      productHistoryResponse.setCreatedDate(taskHistoryResponse.getCreatedDate());
      productHistoryResponseList.add(productHistoryResponse);
    }
    return productHistoryResponseList;
  }

  public static List<DistributionProductWebResponse> fromDistributionProductResponseToDistributionProductWebResponse(
      List<DistributionProductResponse> distributionProductResponseList, Map<String, ProfileResponse> profileResponseMap) {
    List<DistributionProductWebResponse> response = new ArrayList<>();
    for (DistributionProductResponse distributionProductResponse : distributionProductResponseList) {
      DistributionProductWebResponse distributionProductWebResponse = new DistributionProductWebResponse();
      BeanUtils.copyProperties(distributionProductResponse, distributionProductWebResponse,  "state");
      distributionProductWebResponse.setProductCreationType(
          distributionProductResponse.getProductCreationType());
      distributionProductWebResponse.setAiGeneratedBrand(
          Optional.ofNullable(distributionProductResponse.getAiGeneratedFieldsResponse()).map(
                  com.gdn.partners.pcu.internal.client.model.response.AiGeneratedFieldsResponse::isAiGeneratedBrand)
              .orElse(false));
      distributionProductWebResponse.setAiGeneratedCategory(
          Optional.ofNullable(distributionProductResponse.getAiGeneratedFieldsResponse()).map(
                  com.gdn.partners.pcu.internal.client.model.response.AiGeneratedFieldsResponse::isAiGeneratedCategory)
              .orElse(false));
      if (Objects.nonNull(distributionProductResponse.getState())) {
        distributionProductWebResponse
            .setState(WorkflowWebState.valueOf(distributionProductResponse.getState().name()));
      }
      if (Objects.nonNull(distributionProductResponse.getCurrentVendor())) {
        VendorDetailWebResponse vendorDetailWebResponse = new VendorDetailWebResponse();
        BeanUtils.copyProperties(distributionProductResponse.getCurrentVendor(), vendorDetailWebResponse);
        vendorDetailWebResponse.setIsAbleToReject(distributionProductResponse.getCurrentVendor().getAbleToReject());
        vendorDetailWebResponse.setIsQcRequired(distributionProductResponse.getCurrentVendor().getQcRequired());
        distributionProductWebResponse.setCurrentVendor(vendorDetailWebResponse);
      }
      if (profileResponseMap.containsKey(distributionProductResponse.getBusinessPartnerCode())) {
        distributionProductWebResponse.setInternationalFlag(
            profileResponseMap.get(distributionProductResponse.getBusinessPartnerCode()).getCompany().isInternationalFlag());
        distributionProductWebResponse.setCommissionType(
            profileResponseMap.get(distributionProductResponse.getBusinessPartnerCode()).getCompany().getMerchantType());
        distributionProductWebResponse.setOfficialSeller(
            profileResponseMap.get(distributionProductResponse.getBusinessPartnerCode()).isOfficial());
        distributionProductWebResponse.setSellerStatus(populateSellerStatus(
          profileResponseMap.get(distributionProductResponse.getBusinessPartnerCode())));
      }
      response.add(distributionProductWebResponse);
    }
    return response;
  }

  public static void mapToIPRProductWebResponse(
      List<IPRProductListResponse> iprProductListResponseList,
      Map<String, ProfileResponse> profileResponseMap, String pdpRedirectionPrefix) {
    for (IPRProductListResponse iprProductListResponse : iprProductListResponseList) {
      if (profileResponseMap.containsKey(iprProductListResponse.getBusinessPartnerCode())) {
        if (Objects.nonNull(
            profileResponseMap.get(iprProductListResponse.getBusinessPartnerCode()).getCompany())) {
          iprProductListResponse.setInternationalFlag(
              profileResponseMap.get(iprProductListResponse.getBusinessPartnerCode()).getCompany()
                  .isInternationalFlag());
          iprProductListResponse.setCommissionType(
              profileResponseMap.get(iprProductListResponse.getBusinessPartnerCode()).getCompany()
                  .getMerchantType());
          iprProductListResponse.setBusinessPartnerName(
              profileResponseMap.get(iprProductListResponse.getBusinessPartnerCode()).getCompany()
                  .getBusinessPartnerName());
        }
        iprProductListResponse.setOfficialSeller(
            profileResponseMap.get(iprProductListResponse.getBusinessPartnerCode()).isOfficial());
      }
      StringBuilder stringBuilder = new StringBuilder();
      iprProductListResponse.setPdpRedirectionUrl(
          stringBuilder.append(pdpRedirectionPrefix).append(iprProductListResponse.getProductSku()).toString());
    }
  }

  public static ProductDetailWebResponse convertDistributionDetailResponseToProductResponse(
      DistributionProductDetailResponse distributionProductDetailResponse,
      ProfileResponse profileResponse, String pdpLinkItemSkuPrefix,
      String pdpLinkProductCodePrefix) {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(distributionProductDetailResponse, productDetailResponse, "productImages", "productItems",
        "productAttributes");
    productDetailResponse.setName(distributionProductDetailResponse.getProductName());
    productDetailResponse.setActivated(true);
    productDetailResponse.setViewable(false);
    productDetailResponse.setUrl(distributionProductDetailResponse.getVideoUrl());
    productDetailResponse.setProductCategoryResponses(new ArrayList<>());
    productDetailResponse.setCategories(Arrays.asList(distributionProductDetailResponse.getCategoryName()));
    setProductDetailImages(distributionProductDetailResponse, productDetailResponse);
    setProductDetailAttributeResponse(distributionProductDetailResponse, productDetailResponse);
    setProductDetailItems(distributionProductDetailResponse, productDetailResponse);
    ProductDetailWebResponse productDetailWebResponse =
        ResponseHelper.toProductDetailWebResponse(productDetailResponse, profileResponse);
    setNeedRevisionNotes(distributionProductDetailResponse, productDetailWebResponse);
    productDetailWebResponse.setRestrictedKeywordsDetected(
      Optional.ofNullable(distributionProductDetailResponse.getRestrictedKeywordsDetected()).orElse(new ArrayList<>())
        .stream().map(restrictedKeywordsByFieldVendor -> RestrictedKeywordsDetectedWebResponse.builder()
          .fieldIdentifier(restrictedKeywordsByFieldVendor.getFieldIdentifier())
          .keywords(restrictedKeywordsByFieldVendor.getKeywords()).build()).collect(Collectors.toList()));
    productDetailWebResponse.setEditedFields(distributionProductDetailResponse.getEditedFields());
    productDetailWebResponse.setRevisedFields(distributionProductDetailResponse.getRevisedFields());
    productDetailWebResponse.setReviewType(distributionProductDetailResponse.getReviewType());
    productDetailWebResponse.setProductApproved(distributionProductDetailResponse.isProductApproved());
    productDetailWebResponse.setProductApprovedDate(distributionProductDetailResponse.getProductApprovedDate());
    productDetailWebResponse.setProductApproverAssignee(distributionProductDetailResponse.getProductApproverAssignee());
    productDetailWebResponse.setProductAssignedDate(distributionProductDetailResponse.getProductAssignedDate());
    productDetailWebResponse.setProductType(distributionProductDetailResponse.getProductType());
    productDetailWebResponse.setPredictedBrand(distributionProductDetailResponse.getPredictedBrand());
    productDetailWebResponse.setImageViolations(distributionProductDetailResponse.getImageViolations());
    productDetailWebResponse.setAppealedProduct(distributionProductDetailResponse.isAppealedProduct());
    productDetailWebResponse.setShowProductUrl(
      distributionProductDetailResponse.isShowProductUrl());
    populatePriceToResponse(distributionProductDetailResponse.getProductItems(),
      productDetailWebResponse);
    populatePdpRedirectionLink(productDetailWebResponse, pdpLinkItemSkuPrefix,
        pdpLinkProductCodePrefix);
    productDetailWebResponse.setDistributionMappingStatus(
      distributionProductDetailResponse.getDistributionMappingStatus());
    productDetailWebResponse.setProductCreationType(
        distributionProductDetailResponse.getProductCreationType());
    productDetailWebResponse.setAiGeneratedBrand(
        Optional.ofNullable(distributionProductDetailResponse.getAiGeneratedFieldsResponse())
            .map(AiGeneratedFieldsResponse::isAiGeneratedBrand).orElse(false));
    productDetailWebResponse.setAiGeneratedCategory(
        Optional.ofNullable(distributionProductDetailResponse.getAiGeneratedFieldsResponse())
            .map(AiGeneratedFieldsResponse::isAiGeneratedCategory).orElse(false));
    return productDetailWebResponse;
  }

  private static void populatePdpRedirectionLink(ProductDetailWebResponse productDetailWebResponse,
      String pdpLinkItemSkuPrefix, String pdpLinkProductCodePrefix) {

    productDetailWebResponse.getProductItemResponses().forEach(itemResponse -> {
      if (StringUtils.isNotBlank(itemResponse.getItemSku())) {
        itemResponse.setPdpRedirectionLink(
            createPdpRedirectionUrl(pdpLinkItemSkuPrefix, itemResponse.getItemSku()));
      } else {
        itemResponse.setPdpRedirectionLink(createPdpRedirectionUrl(pdpLinkProductCodePrefix,
            productDetailWebResponse.getProductCode()));
      }
    });
  }

  public static void populatePriceToResponse(List<DistributionProductItemResponse> productItems,
    ProductDetailWebResponse productDetailWebResponse) {
    Map<String, DistributionProductItemResponse> skuCodePriceMap =
      productItems.stream().filter(Objects::nonNull).collect(
        Collectors.toMap(DistributionProductItemResponse::getSkuCode, Function.identity(),
          (a, b) -> a));

    productDetailWebResponse.getProductItemResponses().forEach(itemResponse -> {
      DistributionProductItemResponse response = skuCodePriceMap.get(itemResponse.getSkuCode());
      if (Objects.nonNull(response)) {
        itemResponse.setMaxPrice(response.getMaxPrice());
        itemResponse.setMinPrice(response.getMinPrice());
        itemResponse.setItemSku(response.getItemSku());
      }
    });
  }

  private static void setNeedRevisionNotes(DistributionProductDetailResponse distributionProductDetailResponse,
      ProductDetailWebResponse productDetailWebResponse) {
    List<ItemNotesWebResponse> itemNotes = distributionProductDetailResponse.getProductItems().stream()
        .filter(item -> Objects.nonNull(item.getItemNotes())).map(item -> toItemNotesWebResponse(item))
        .collect(Collectors.toList());
    if (Objects.nonNull(distributionProductDetailResponse.getProductNotes())) {
      NeedRevisionNotesWebResponse needRevisionNotesWebResponse = NeedRevisionNotesWebResponse.builder()
          .vendorNotes(distributionProductDetailResponse.getProductNotes().getVendorNotes())
          .contentAdditionalNotes(distributionProductDetailResponse.getProductNotes().getContentAdditionalNotes())
          .imagesAdditionalNotes(distributionProductDetailResponse.getProductNotes().getImagesAdditionalNotes())
          .allVariants(distributionProductDetailResponse.getProductNotes().isAllVariants())
          .imageReason(distributionProductDetailResponse.getProductNotes().getImageReason())
          .itemNotes(itemNotes)
          .vendorErrorFields(distributionProductDetailResponse.getProductNotes().getVendorErrorFields()).build();
      productDetailWebResponse.setNeedRevisionNotes(needRevisionNotesWebResponse);
    }
  }

  private static ItemNotesWebResponse toItemNotesWebResponse(DistributionProductItemResponse item) {
    return ItemNotesWebResponse.builder().itemName(item.getItemNotes().getItemName())
        .itemNumber(item.getItemNotes().getItemNumber()).itemSku(item.getItemNotes().getItemSku())
        .vendorNotes(item.getItemNotes().getVendorNotes()).vendorErrorFields(item.getItemNotes().getVendorErrorFields())
        .skuCode(item.getItemNotes().getSkuCode()).build();
  }

  public static ProductDetailWebResponse convertDistributionDetailResponseToProductDetailWebResponse(
      DistributionProductDetailResponse distributionProductDetailResponse,
      ProfileResponse profileResponse, String pdpLinkItemSkuPrefix, String pdpLinkProductCodePrefix)
      throws Exception {
    ProductDetailWebResponse productDetailWebResponse = ResponseHelper.convertDistributionDetailResponseToProductResponse(
        distributionProductDetailResponse, profileResponse, pdpLinkItemSkuPrefix,
        pdpLinkProductCodePrefix);
    setVendorDetailResponse(distributionProductDetailResponse.getCurrentVendor(), productDetailWebResponse);
    setVendorProductDetailWebResponseFromDistributionProductDetailResponse(distributionProductDetailResponse,
        productDetailWebResponse);
    if (Objects.nonNull(distributionProductDetailResponse.getState())) {
      productDetailWebResponse.setState(distributionProductDetailResponse.getState().name());
    }
    return productDetailWebResponse;
  }

  private static void setVendorDetailResponse(VendorDetailResponse currentVendor,
      ProductDetailWebResponse productDetailWebResponse) {
    if (Objects.nonNull(currentVendor)) {
      VendorDetailWebResponse vendorDetailWebResponse =
          VendorDetailWebResponse.builder().name(currentVendor.getName()).vendorCode(currentVendor.getVendorCode())
              .slaInDays(currentVendor.getSlaInDays()).isAbleToReject(currentVendor.getAbleToReject())
              .isQcRequired(currentVendor.getQcRequired()).startHolidayDate(currentVendor.getStartHolidayDate())
              .endHolidayDate(currentVendor.getEndHolidayDate()).quota(currentVendor.getQuota())
              .description(currentVendor.getDescription()).build();
      productDetailWebResponse.setCurrentVendor(vendorDetailWebResponse);
    }
  }

  private static void setVendorProductDetailWebResponseFromDistributionProductDetailResponse(
      DistributionProductDetailResponse distributionProductDetailResponse,
      ProductDetailWebResponse productDetailWebResponse) {
    productDetailWebResponse.setStoreId(distributionProductDetailResponse.getStoreId());
    productDetailWebResponse.setProductId(distributionProductDetailResponse.getProductId());
    productDetailWebResponse.setBusinessPartnerName(distributionProductDetailResponse.getBusinessPartnerName());
    productDetailWebResponse.setVendorId(distributionProductDetailResponse.getVendorId());
    productDetailWebResponse.setVendorName(distributionProductDetailResponse.getVendorName());
    productDetailWebResponse.setVendorCode(distributionProductDetailResponse.getVendorCode());
    //TODO Change to correct variables on detail API jira
//    productDetailWebResponse.setImageApproved(distributionProductDetailResponse.isImageApproved());
//    productDetailWebResponse.setImageApproverAssignee(distributionProductDetailResponse.getImageApproverAssignee());
//    productDetailWebResponse.setImageAssignedDate(distributionProductDetailResponse.getImageAssignedDate());
//    productDetailWebResponse.setImageApprovedDate(distributionProductDetailResponse.getImageApprovedDate());
//    productDetailWebResponse.setContentApproved(distributionProductDetailResponse.isContentApproved());
//    productDetailWebResponse.setContentApproverAssignee(distributionProductDetailResponse.getContentApproverAssignee());
//    productDetailWebResponse.setContentAssignedDate(distributionProductDetailResponse.getContentAssignedDate());
//    productDetailWebResponse.setContentApprovedDate(distributionProductDetailResponse.getContentApprovedDate());
  }

  private static void setProductDetailImages(
      DistributionProductDetailResponse distributionProductDetailResponse, ProductDetailResponse productDetailResponse) {
    List<Image> imageList = new ArrayList<>();
    for (DistributionProductImageResponse distributionProductImageResponse :
         distributionProductDetailResponse.getProductImages()) {
      Image image = new Image();
      BeanUtils.copyProperties(distributionProductImageResponse, image, "originalImage");
      image.setOriginalImage(distributionProductImageResponse.getOriginalImage());
      image.setMainImages(distributionProductImageResponse.isMainImage());
      imageList.add(image);
    }
    productDetailResponse.setImages(imageList);
  }

  private static void setProductDetailAttributeResponse(
      DistributionProductDetailResponse distributionProductDetailResponse, ProductDetailResponse productDetailResponse) {
    List<ProductAttributeResponse> productAttributeResponseList = new ArrayList<>();
    for (DistributionProductAttributeResponse distributionProductAttributeResponse :
         distributionProductDetailResponse.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      productAttributeResponse
          .setProductAttributeName(distributionProductAttributeResponse.getName());
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(distributionProductAttributeResponse, attributeResponse);
      productAttributeResponse.setAttribute(attributeResponse);
      List<ProductAttributeValueResponse> productAttributeValueResponseList = new ArrayList<>();
      ProductAttributeValueResponse productAttributeValueResponse =
              new ProductAttributeValueResponse();
      BeanUtils.copyProperties(distributionProductAttributeResponse, productAttributeValueResponse);
      if (distributionProductAttributeResponse.getAttributeType()
              .equals(AttributeType.DEFINING_ATTRIBUTE.toString())) {
        AllowedAttributeValueResponse allowedAttributeValueResponse =
                new AllowedAttributeValueResponse();
        allowedAttributeValueResponse.setValue(distributionProductAttributeResponse.getValue());
        allowedAttributeValueResponse
                .setAllowedAttributeCode(distributionProductAttributeResponse.getAttributeCode());
        productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
      } else if (distributionProductAttributeResponse.getAttributeType()
              .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
        PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
                new PredefinedAllowedAttributeValueResponse();
        predefinedAllowedAttributeValueResponse
                .setValue(distributionProductAttributeResponse.getValue());
        predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(
                distributionProductAttributeResponse.getAttributeCode());
        productAttributeValueResponse
                .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
      } else {
        productAttributeValueResponse
                .setDescriptiveAttributeValue(distributionProductAttributeResponse.getValue());
      }
      productAttributeValueResponseList.add(productAttributeValueResponse);
      productAttributeResponse.setProductAttributeValues(productAttributeValueResponseList);
      productAttributeResponseList.add(productAttributeResponse);
    }
    productDetailResponse.setProductAttributeResponses(productAttributeResponseList);
  }

  private static void setProductDetailItems(
      DistributionProductDetailResponse distributionProductDetailResponse,
      ProductDetailResponse productDetailResponse) {
    List<ProductItemResponse> productItemResponseList = new ArrayList<>();
    for (DistributionProductItemResponse distributionProductItemResponse :
         distributionProductDetailResponse.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils
          .copyProperties(distributionProductItemResponse, productItemResponse, "productItemImages",
                      "productItemAttributes");
      productItemResponse.setNewlyAddedItem(distributionProductItemResponse.isNewlyAdded());
      setProductDetailsItemImages(distributionProductItemResponse, productItemResponse);
      setProductDetailsItemAttributes(distributionProductItemResponse, productItemResponse);
      productItemResponseList.add(productItemResponse);
    }
    productDetailResponse.setProductItemResponses(new HashSet<>(productItemResponseList));
  }

  private static void setProductDetailsItemImages(
      DistributionProductItemResponse distributionProductItemResponse, ProductItemResponse productItemResponse) {
    List<Image> itemImageList = new ArrayList<>();
    for (DistributionProductImageResponse distributionProductImageResponse :
            distributionProductItemResponse.getProductItemImages()) {
      Image image = new Image();
      BeanUtils.copyProperties(distributionProductImageResponse, image);
      image.setMainImages(distributionProductImageResponse.isMainImage());
      itemImageList.add(image);
    }
    productItemResponse.setImages(itemImageList);
  }

  private static void setProductDetailsItemAttributes(
      DistributionProductItemResponse distributionProductItemResponse,
      ProductItemResponse productItemResponse) {
    List<ProductItemAttributeValueResponse> productItemAttributeValueResponseList =
          new ArrayList<>();
    for (DistributionProductAttributeResponse distributionProductAttributeResponse :
         distributionProductItemResponse.getProductItemAttributes()) {
      ProductItemAttributeValueResponse productItemAttributeValueResponse =
          new ProductItemAttributeValueResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      attributeResponse.setName(distributionProductAttributeResponse.getName());
      attributeResponse.setAttributeCode(distributionProductAttributeResponse.getAttributeCode());
      attributeResponse.setAttributeType(distributionProductAttributeResponse.getAttributeType());
      attributeResponse.setVariantCreation(distributionProductAttributeResponse.isVariantCreation());
      BeanUtils.copyProperties(distributionProductAttributeResponse,
          productItemAttributeValueResponse);
      productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
      productItemAttributeValueResponseList.add(productItemAttributeValueResponse);
    }
    productItemResponse
        .setProductItemAttributeValueResponses(productItemAttributeValueResponseList);
  }

  public static List<ProductHistoryWebResponse> toProductHistoryWebResponseListFromVendorHistoryResponse(
      List<ProductHistoryResponse> responses) {
    return responses.stream().map(ResponseHelper::toProductHistoryWebResponseFromVendorResponse)
        .collect(Collectors.toList());
  }

  private static ProductHistoryWebResponse toProductHistoryWebResponseFromVendorResponse(
      ProductHistoryResponse productHistoryResponse) {
    ProductHistoryWebResponse productHistoryWebResponse = new ProductHistoryWebResponse();
    BeanUtils.copyProperties(productHistoryResponse, productHistoryWebResponse, "notes");
    if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && productHistoryResponse.getNotes()
        .startsWith(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE)) {
      productHistoryWebResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE);
      getSkuProductUpdateHistoryResponse(productHistoryWebResponse, productHistoryResponse.getNotes().substring(9));
    } else if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && productHistoryResponse.getNotes()
        .contains(Constants.PRODUCT_ASSIGNMENT)) {
      productHistoryWebResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT);
      productHistoryWebResponse.setNotes(Collections.singletonList(productHistoryResponse.getNotes()));
    } else if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && productHistoryResponse.getNotes()
        .contains(Constants.PRODUCT_UN_ASSIGNMENT)) {
      productHistoryWebResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT);
      productHistoryWebResponse.setNotes(Collections.singletonList(productHistoryResponse.getNotes()));
    } else if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && productHistoryResponse.getNotes()
        .contains(Constants.ASSIGNED_HISTORY_FORMAT)) {
      productHistoryWebResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT);
      getSkuProductUpdateHistoryResponse(productHistoryWebResponse, productHistoryResponse.getNotes().substring(13));
    } else if (StringUtils.isNotBlank(productHistoryResponse.getNotes()) && productHistoryResponse.getNotes()
        .contains(Constants.UNASSIGNED_HISTORY_FORMAT)) {
      productHistoryWebResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT);
      getSkuProductUpdateHistoryResponse(productHistoryWebResponse, productHistoryResponse.getNotes().substring(15));
    } else {
      productHistoryWebResponse.setNotes(Collections.singletonList(productHistoryResponse.getNotes()));
    }
    return productHistoryWebResponse;
  }

  private static void getSkuProductUpdateHistoryResponse(ProductHistoryWebResponse productHistoryWebResponse,
      String notes) {
    ObjectMapper objectMapper = new ObjectMapper();
    List<ProductUpdateHistoryWebResponse> productSkuUpdateHistoryWebResponses = null;
    try {
      productSkuUpdateHistoryWebResponses =
          Arrays.asList(objectMapper.readValue(notes, ProductSkuUpdateHistoryWebResponse[].class));
    } catch (IOException e) {
      log.error("Error while converting the data from PDT to web respnse ", e);
    }
    productHistoryWebResponse.setProductUpdateHistoryWebResponseList(productSkuUpdateHistoryWebResponses);
  }

  public static List<ProductSuspensionWebResponse> toProductSuspensionWebResponseList(
      List<SuspensionProductResponse> suspensionProductResponses, Map<String, ProfileResponse> profileResponseMap) {
    return suspensionProductResponses.stream()
        .map(suspensionProductResponse -> toProductSuspensionWebResponse(suspensionProductResponse, profileResponseMap))
        .collect(Collectors.toList());
  }

  public static ProductSuspensionWebResponse toProductSuspensionWebResponse(
      SuspensionProductResponse suspensionProductResponse, Map<String, ProfileResponse> profileResponseMap) {
    ProductSuspensionWebResponse productSuspensionWebResponse = new ProductSuspensionWebResponse();
    BeanUtils.copyProperties(suspensionProductResponse, productSuspensionWebResponse, "State");
    productSuspensionWebResponse.setStatus(suspensionProductResponse.getState());
    if (profileResponseMap.containsKey(suspensionProductResponse.getBusinessPartnerCode())) {
      productSuspensionWebResponse.setInternationalFlag(
          profileResponseMap.get(suspensionProductResponse.getBusinessPartnerCode()).getCompany()
              .isInternationalFlag());
      productSuspensionWebResponse.setCommissionType(
          profileResponseMap.get(suspensionProductResponse.getBusinessPartnerCode()).getCompany().getMerchantType());
    }
    return productSuspensionWebResponse;
  }

  public static Map<String, String> toBusinessPartnerNameMap(List<MerchantNameResponse> merchantNameResponses) {
    Map<String, String> map = new HashMap();
    for (MerchantNameResponse response : merchantNameResponses) {
      map.put(response.getMerchantCode(), response.getMerchantName());
    }
    return map;
  }

  public static List<ProductSuspensionHistoryWebResponse> toProductSuspensionHistoryWebResponseList(
      List<ProductSuspensionHistoryResponse> responses) {
    return responses.stream().map(ResponseHelper::toProductSuspensionHistoryWebResponse).collect(Collectors.toList());
  }

  private static ProductSuspensionHistoryWebResponse toProductSuspensionHistoryWebResponse(
      ProductSuspensionHistoryResponse productSuspensionHistoryResponse) {
    ProductSuspensionHistoryWebResponse productSuspensionHistoryWebResponse = new ProductSuspensionHistoryWebResponse();
    BeanUtils.copyProperties(productSuspensionHistoryResponse, productSuspensionHistoryWebResponse);
    return productSuspensionHistoryWebResponse;
  }

  public static List<MerchantWebSearchResponse> toMerchantWebSearchResponse(
      List<MerchantSearchResponse> merchantSearchResponseList) {
    List<MerchantWebSearchResponse> merchantWebSearchResponseList = new ArrayList<>();
    for(MerchantSearchResponse merchantSearchResponse: merchantSearchResponseList) {
      MerchantWebSearchResponse merchantWebSearchResponse =
          new MerchantWebSearchResponse(merchantSearchResponse.getBusinessPartnerCode(),
              merchantSearchResponse.getBusinessPartnerName(), merchantSearchResponse.getReviewConfig());
      merchantWebSearchResponseList.add(merchantWebSearchResponse);
    }
    return merchantWebSearchResponseList;
  }

  public static List<ConfigurationsStatusWebResponse> toConfigurationsStatusWebResponse(
      List<ConfigurationStatusResponse> responseGdnRestListResponse) {
    List<ConfigurationsStatusWebResponse> configurationsStatusWebResponseList = new ArrayList<>();
    for (ConfigurationStatusResponse configurationStatusResponse : responseGdnRestListResponse) {
      ConfigurationsStatusWebResponse configurationsStatusWebResponse =
          new ConfigurationsStatusWebResponse(configurationStatusResponse.getMerchantCode(),
              configurationStatusResponse.getCategoryCode(), configurationStatusResponse.getReviewConfig());
      configurationsStatusWebResponseList.add(configurationsStatusWebResponse);
    }
    return configurationsStatusWebResponseList;
  }
  public static ConfigurationCountWebResponse toConfigurationCountWebResponse(
      ConfigurationCountResponse configurationCountResponse) {
    return new ConfigurationCountWebResponse(configurationCountResponse.getCategoryConfigurationCount(),
        configurationCountResponse.getMerchantConfigurationCount(),
        (configurationCountResponse.getCategoryConfigurationCount() + configurationCountResponse
            .getMerchantConfigurationCount()));
  }

  public static List<CategoryConfigurationFilterWebResponse> toCategoryConfigurationFilterWebResponseList(
      List<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponseList) {
    return categoryConfigurationFilterResponseList.stream()
        .map(ResponseHelper::toCategoryConfigurationFilterWebResponse).collect(Collectors.toList());
  }

  private static CategoryConfigurationFilterWebResponse toCategoryConfigurationFilterWebResponse(
      CategoryConfigurationFilterResponse categoryConfigurationFilterResponse) {
    CategoryConfigurationFilterWebResponse categoryConfigurationFilterWebResponse =
        new CategoryConfigurationFilterWebResponse();
    BeanUtils.copyProperties(categoryConfigurationFilterResponse, categoryConfigurationFilterWebResponse);
    return categoryConfigurationFilterWebResponse;
  }

  public static List<MerchantConfigurationFilterWebResponse> toMerchantConfigurationFilterWebResponseList(
      List<MerchantConfigurationFilterResponse> merchantConfigurationFilterResponseList) {
    return merchantConfigurationFilterResponseList.stream()
        .map(ResponseHelper::toMerchantConfigurationFilterWebResponse).collect(Collectors.toList());
  }

  private static MerchantConfigurationFilterWebResponse toMerchantConfigurationFilterWebResponse(
      MerchantConfigurationFilterResponse merchantConfigurationFilterResponse) {
    MerchantConfigurationFilterWebResponse merchantConfigurationFilterWebResponse =
        new MerchantConfigurationFilterWebResponse();
    BeanUtils.copyProperties(merchantConfigurationFilterResponse, merchantConfigurationFilterWebResponse);
    return merchantConfigurationFilterWebResponse;
  }

  public static List<CategoryConfigurationHistoryWebResponse> toCategoryConfigurationHistoryWebResponseList(
      List<CategoryConfigurationHistoryResponse> categoryConfigurationHistoryResponseList) {
    return categoryConfigurationHistoryResponseList.stream()
        .map(ResponseHelper::toCategoryConfigurationHistoryWebResponse).collect(Collectors.toList());
  }

  private static CategoryConfigurationHistoryWebResponse toCategoryConfigurationHistoryWebResponse(
      CategoryConfigurationHistoryResponse categoryConfigurationHistoryResponse) {
    CategoryConfigurationHistoryWebResponse categoryConfigurationHistoryWebResponse =
        new CategoryConfigurationHistoryWebResponse();
    BeanUtils
        .copyProperties(categoryConfigurationHistoryResponse, categoryConfigurationHistoryWebResponse, "activityIn");
    if(Constants.ACTIVITY_UPDATE_EN.equals(categoryConfigurationHistoryResponse.getActivity())) {
      categoryConfigurationHistoryWebResponse.setActivityIn(Constants.ACTIVITY_UPDATE_IN);
    } else if(Constants.ACTIVITY_REGISTERED_EN.equals(categoryConfigurationHistoryResponse.getActivity())){
      categoryConfigurationHistoryWebResponse.setActivityIn(Constants.ACTIVITY_REGISTERED_IN);
    }
    return categoryConfigurationHistoryWebResponse;
  }

  public static MerchantConfigurationHistoryWebResponse toMerchantConfigurationHistoryWebResponse(
      MerchantConfigurationHistoryResponse merchantConfigurationHistoryResponse) {
    MerchantConfigurationHistoryWebResponse merchantConfigurationHistoryWebResponse =
        new MerchantConfigurationHistoryWebResponse();
    BeanUtils
        .copyProperties(merchantConfigurationHistoryResponse, merchantConfigurationHistoryWebResponse, "activityIn");
    if(Constants.ACTIVITY_UPDATE_EN.equals(merchantConfigurationHistoryResponse.getActivity())) {
      merchantConfigurationHistoryWebResponse.setActivityIn(Constants.ACTIVITY_UPDATE_IN);
    } else if(Constants.ACTIVITY_REGISTERED_EN.equals(merchantConfigurationHistoryResponse.getActivity())){
      merchantConfigurationHistoryWebResponse.setActivityIn(Constants.ACTIVITY_REGISTERED_IN);
    }
    return merchantConfigurationHistoryWebResponse;
  }

  public static MapWebResponse toMapWebResponse(ProductCollectionCountRestResponse productCollectionCountRestResponse) {
    MapWebResponse mapResponse = new MapWebResponse();
    Map<String, Object> productCollectionAges = new LinkedHashMap<>();
    productCollectionAges.put(TODAY, productCollectionCountRestResponse.getToday());
    productCollectionAges.put(YESTERDAY, productCollectionCountRestResponse.getYesterday());
    productCollectionAges.put(TWO_DAYS_AGO, productCollectionCountRestResponse.getTwoDaysAgo());
    productCollectionAges
        .put(BETWEEN_3_UNTIL_5_DAYS_OLD, productCollectionCountRestResponse.getThreeUntilFiveDaysAgo());
    productCollectionAges.put(MORE_THAN_5_DAYS, productCollectionCountRestResponse.getMoreThan5Days());
    Map<String, Object> response = new HashMap<>();
    response.put(DAYS_ADDED, productCollectionAges);
    mapResponse.setMap(response);
    return mapResponse;
  }

  public static MapWebResponse toMapWebResponse(com.gdn.x.mta.distributiontask.rest.model.response.MapResponse mapResponse) {
    MapWebResponse mapWebResponse = new MapWebResponse();
    if (Objects.nonNull(mapResponse.getMap())) {
      mapWebResponse.setMap(mapResponse.getMap());
    }
    return mapWebResponse;
  }

  public static List<LookupWebResponse> toLookupWebResponseList(List<LookupResponse> lookupList) {
    List<LookupWebResponse> lookupResponseList = new ArrayList<>();
    lookupList.forEach(lookup -> lookupResponseList.add(toLookupWebResponse(lookup)));
    return lookupResponseList;
  }

  private static LookupWebResponse toLookupWebResponse(LookupResponse lookup) {
    LookupWebResponse lookupWebResponse = new LookupWebResponse();
    BeanUtils.copyProperties(lookup, lookupWebResponse);
    return lookupWebResponse;
  }

  public static List<ReviewProductWebResponse> toReviewProductWebResponseList(List<ReviewProductResponse> responses,
      Map<String, ProfileResponse> profileResponseMap) {
    return responses.stream()
        .map(reviewProductResponse -> toReviewProductWebResponse(reviewProductResponse, profileResponseMap))
        .collect(Collectors.toList());
  }

  private static ReviewProductWebResponse toReviewProductWebResponse(ReviewProductResponse reviewProductResponse,
      Map<String, ProfileResponse> profileResponseMap) {
    ReviewProductWebResponse reviewProductWebResponse = new ReviewProductWebResponse();
    BeanUtils.copyProperties(reviewProductResponse, reviewProductWebResponse);
    if (profileResponseMap.containsKey(reviewProductResponse.getBusinessPartnerCode())) {
      reviewProductWebResponse.setInternationalFlag(
          profileResponseMap.get(reviewProductResponse.getBusinessPartnerCode()).getCompany().isInternationalFlag());
      reviewProductWebResponse.setCommissionType(
          profileResponseMap.get(reviewProductResponse.getBusinessPartnerCode()).getCompany().getMerchantType());
    }
    return reviewProductWebResponse;
  }

  public static List<DistributionProductWebResponse> toDistributionProductWebResponseList(
      List<DistributionProductResponse> responses) {
    return responses.stream().map(ResponseHelper::toDistributionProductWebResponse).collect(Collectors.toList());
  }

  private static DistributionProductWebResponse toDistributionProductWebResponse(
      DistributionProductResponse distributionProductResponse) {
    DistributionProductWebResponse distributionProductWebResponse = new DistributionProductWebResponse();
    BeanUtils.copyProperties(distributionProductResponse, distributionProductWebResponse);
    if (Objects.nonNull(distributionProductResponse.getState())) {
      distributionProductWebResponse.setState(WorkflowWebState.valueOf(distributionProductResponse.getState().name()));
    }
    return distributionProductWebResponse;
  }

  public static void toSetHalalResponse(List<HalalProductResponse> halalProductResponseList,
      HalalProductWebResponse halalProductWebResponse, String productSku,
      String prefix) {
    if (CollectionUtils.isNotEmpty(halalProductResponseList)) {
      BeanUtils.copyProperties(halalProductResponseList.stream().findFirst().get(), halalProductWebResponse);
      StringBuilder stringBuilder = new StringBuilder();
      halalProductWebResponse.setProductLink(stringBuilder.append(prefix).append(productSku).toString());
    }
  }

  public static List<VendorDetailWebResponse> toVendorDetailWebResponseList(
      List<VendorCapacityDTO> vendorCapacityDTOS) {
    return vendorCapacityDTOS.stream().map(ResponseHelper::toVendorDetailWebResponse).collect(Collectors.toList());
  }

  private static VendorDetailWebResponse toVendorDetailWebResponse(VendorCapacityDTO vendorCapacityDTO) {
    VendorDetailWebResponse vendorDetailWebResponse = new VendorDetailWebResponse();
    BeanUtils.copyProperties(vendorCapacityDTO, vendorDetailWebResponse);
    return vendorDetailWebResponse;
  }

  public static ProductImageQcWebResponse toProductImageQcWebResponse(
      ProductImageQcFeedbackResponse productImageQcFeedbackResponse, Map<String, String> predictionTypeToDisplayName,
      Map<String, Double> predictionTypeToConfidence, String gcsPrefixUrl, boolean categoryPredictionEnabled) throws IOException {
    ProductImageQcWebResponse productImageQcWebResponse = new ProductImageQcWebResponse();
    List<ImageFeedbackWebResponse> imageFeedbackWebResponseList = new ArrayList<>();
    ObjectMapper mapper = new ObjectMapper();
    UserFeedbackRequest userFeedbackRequest = new UserFeedbackRequest();
    Map<String, List<String>> userFeedbackMap = new HashMap<>();
    ImageQcResponse imageQcResponse =
        mapper.readValue(productImageQcFeedbackResponse.getSystemFeedback(), ImageQcResponse.class);
    productImageQcWebResponse.setProductCode(productImageQcFeedbackResponse.getProductCode());
    if (Objects.nonNull(productImageQcFeedbackResponse.getUserFeedback())) {
      userFeedbackRequest =
          mapper.readValue(productImageQcFeedbackResponse.getUserFeedback(), UserFeedbackRequest.class);
    }
    if (Objects.nonNull(userFeedbackRequest.getUserFeedback())) {
      userFeedbackMap = userFeedbackRequest.getUserFeedback().stream().collect(
          Collectors.toMap(UserImageFeedbackRequest::getLocationPath, UserImageFeedbackRequest::getUserPrediction));
    }
    for (ImageFeedbackResponse imageFeedbackResponse : imageQcResponse.getImages()) {
      if (imageFeedbackResponse.isMarkForDelete()) {
        continue;
      }
      ImageFeedbackWebResponse imageFeedbackWebResponse = new ImageFeedbackWebResponse();
      for (ImageQcPredictionResponse imageQcPredictionResponse : imageFeedbackResponse.getPredictions()) {
        if (imageQcPredictionResponse.isPresent()) {
          imageFeedbackWebResponse.getSystemFeedback().add(imageQcPredictionResponse.getDisplayName());
        }
      }
      if (CollectionUtils.isEmpty(imageFeedbackWebResponse.getSystemFeedback())) {
        imageFeedbackWebResponse.getSystemFeedback().add(ImageQcConstants.GOOD_EN);
      }
      if (CollectionUtils.isNotEmpty(userFeedbackMap.get(imageFeedbackResponse.getLocationPath()))) {
        imageFeedbackWebResponse.setUserFeedback(userFeedbackMap.get(imageFeedbackResponse.getLocationPath()));
      }
      imageFeedbackWebResponse.setLocationPath(
          imageFeedbackResponse.getLocationPath().replace(ImageQcConstants.IMAGE_BASE_PATH, StringUtils.EMPTY)
              .replace(gcsPrefixUrl, StringUtils.EMPTY));
      imageFeedbackWebResponse.setEdited(imageFeedbackResponse.isEdited());
      imageFeedbackWebResponseList.add(imageFeedbackWebResponse);
    }
    productImageQcWebResponse.setImageFeedback(imageFeedbackWebResponseList);

    Pair<Set<String>, Set<String>> restrictiveModelFeedback =
        getRestrictiveModelSystemAndUserFeedback(imageQcResponse, userFeedbackRequest);
    Pair<Set<String>, Set<String>> brandModelFeedback =
        getBrandModelSystemAndUserFeedback(imageQcResponse, userFeedbackRequest, predictionTypeToDisplayName, predictionTypeToConfidence);

    Pair<Set<String>, Set<String>> categoryModelFeedback = null;

    if (categoryPredictionEnabled) {
      categoryModelFeedback =
          getCategoryModelSystemAndUserFeedback(imageQcResponse, userFeedbackRequest,
              predictionTypeToDisplayName);
      productImageQcWebResponse.setCategoryModels(
          new ProductModelFeedback(categoryModelFeedback.getLeft(),
              categoryModelFeedback.getRight()));
    }
    if (CollectionUtils.isEmpty(restrictiveModelFeedback.getLeft()) && CollectionUtils.isEmpty(
        brandModelFeedback.getLeft()) &&
        (categoryModelFeedback == null || CollectionUtils.isEmpty(categoryModelFeedback.getLeft()))) {
      restrictiveModelFeedback.getLeft().add(ImageQcConstants.GOOD_EN);
    }
    productImageQcWebResponse.setRestrictiveModelFeedback(
        new ProductModelFeedback(restrictiveModelFeedback.getLeft(), restrictiveModelFeedback.getRight()));
    productImageQcWebResponse.setBrandModels(
        new ProductModelFeedback(brandModelFeedback.getLeft(), brandModelFeedback.getRight()));
    return productImageQcWebResponse;
  }

  private static Pair<Set<String>, Set<String>> getRestrictiveModelSystemAndUserFeedback(
      ImageQcResponse imageQcResponse, UserFeedbackRequest userFeedbackRequest) {
    Set<String> allSystemFeedback = new HashSet<>(ImmutableSet.of(ImageQcConstants.GOOD_EN));
    Set<String> restrictiveModelSystemFeedBack = new HashSet<>();
    Set<String> restrictiveModelUserFeedBack = new HashSet<>();

    if (CollectionUtils.isNotEmpty(imageQcResponse.getRestrictionModels())) {
      for (RestrictionModelFeedBackResponse restrictionModelFeedBackResponse : imageQcResponse.getRestrictionModels()) {
        if (CollectionUtils.isNotEmpty(restrictionModelFeedBackResponse.getPredictions())) {
          restrictiveModelSystemFeedBack.addAll(
              restrictionModelFeedBackResponse.getPredictions().stream().filter(ImageQcPredictionResponse::isPresent)
                  .map(ImageQcPredictionResponse::getDisplayName).collect(Collectors.toSet()));
          allSystemFeedback.addAll(
              restrictionModelFeedBackResponse.getPredictions().stream().map(ImageQcPredictionResponse::getDisplayName)
                  .collect(Collectors.toSet()));
        }
      }
    }

    if (CollectionUtils.isNotEmpty(userFeedbackRequest.getOtherModelFeedBack())) {
      restrictiveModelUserFeedBack =
          userFeedbackRequest.getOtherModelFeedBack().stream().filter(allSystemFeedback::contains)
              .collect(Collectors.toSet());
    }

    return Pair.of(restrictiveModelSystemFeedBack, restrictiveModelUserFeedBack);
  }

  private static Pair<Set<String>, Set<String>> getBrandModelSystemAndUserFeedback(ImageQcResponse imageQcResponse,
      UserFeedbackRequest userFeedbackRequest, Map<String, String> predictionTypeToDisplayName,
      Map<String, Double> predictionTypeToConfidence) {
    Set<String> allSystemFeedback = new HashSet<>(ImmutableSet.of(ImageQcConstants.GOOD_EN));
    Set<String> brandModelSystemFeedBack = new HashSet<>();
    Set<String> brandModelUserFeedBack = new HashSet<>();

    if (CollectionUtils.isNotEmpty(imageQcResponse.getBrandModels())) {
      for (BrandModelFeedBackResponse brandModelFeedBackResponse : imageQcResponse.getBrandModels()) {
        if (predictionTypeToDisplayName.keySet().contains(brandModelFeedBackResponse.getPredictionType())
            && CollectionUtils.isNotEmpty(brandModelFeedBackResponse.getPredictions())) {
          if (brandModelFeedBackResponse.getPredictions().stream().anyMatch(
              brandPredictionResponse -> brandPredictionResponse.getConfidence() > predictionTypeToConfidence.get(
                  brandModelFeedBackResponse.getPredictionType()))) {
            brandModelSystemFeedBack.add(
                predictionTypeToDisplayName.get(brandModelFeedBackResponse.getPredictionType()));
          }
        }
        allSystemFeedback.add(
          predictionTypeToDisplayName.get(brandModelFeedBackResponse.getPredictionType()));
      }
    }

    if (CollectionUtils.isNotEmpty(userFeedbackRequest.getOtherModelFeedBack())) {
      brandModelUserFeedBack = userFeedbackRequest.getOtherModelFeedBack().stream().filter(allSystemFeedback::contains)
          .collect(Collectors.toSet());
    }

    return Pair.of(brandModelSystemFeedBack, brandModelUserFeedBack);
  }

  private static Pair<Set<String>, Set<String>> getCategoryModelSystemAndUserFeedback(
      ImageQcResponse imageQcResponse, UserFeedbackRequest userFeedbackRequest,
      Map<String, String> predictionTypeToDisplayName) {
    Set<String> allSystemFeedback = new HashSet<>(ImmutableSet.of(ImageQcConstants.GOOD_EN));
    Set<String> categoryModelSystemFeedBack = new HashSet<>();
    Set<String> categoryModelUserFeedBack = new HashSet<>();

    if (CollectionUtils.isNotEmpty(imageQcResponse.getCategoryModels())) {
      for (CategoryModelFeedbackResponse categoryModelFeedbackResponse :
          imageQcResponse.getCategoryModels()) {
        if (categoryModelFeedbackResponse.isMismatchCombined()
            && predictionTypeToDisplayName.containsKey(
            categoryModelFeedbackResponse.getPredictionType())) {
          categoryModelSystemFeedBack.add(
              predictionTypeToDisplayName.get(categoryModelFeedbackResponse.getPredictionType()));
        }
        allSystemFeedback.add(
            predictionTypeToDisplayName.get(categoryModelFeedbackResponse.getPredictionType()));
      }
    }

    if (CollectionUtils.isNotEmpty(userFeedbackRequest.getOtherModelFeedBack())) {
      categoryModelUserFeedBack =
          userFeedbackRequest.getOtherModelFeedBack().stream().filter(allSystemFeedback::contains)
              .collect(Collectors.toSet());
    }

    return Pair.of(categoryModelSystemFeedBack, categoryModelUserFeedBack);
  }


  public static List<ImageFaultyTypeWebResponse> toFaultyTypeListWebResponse(
      List<PredictionTypeResponse> predictionTypeListResponse, String ignoreForImageQc, String notIgnoreImageQc,
      Map<String, String> labelColourMap, boolean categoryPredictionEnabled) {
    List<String> listOfPredictions = Arrays.asList(ignoreForImageQc.split(Constants.COMMA_SEPARATOR));
    List<String> notIgnoreImageQcPredictions = Arrays.asList(notIgnoreImageQc.split(Constants.COMMA_SEPARATOR));
    List<ImageFaultyTypeWebResponse> faultyTypeListWebResponse = new ArrayList<>();
    predictionTypeListResponse.forEach(
        fault -> faultyTypeListWebResponse.add(new ImageFaultyTypeWebResponse(fault.getEnName(), fault.getInName())));
    faultyTypeListWebResponse
        .add(new ImageFaultyTypeWebResponse(ImageQcConstants.BRAND_EN, ImageQcConstants.BRAND_IN));
    if (categoryPredictionEnabled) {
      faultyTypeListWebResponse.add(
        new ImageFaultyTypeWebResponse(ImageQcConstants.CATEGORY_EN, ImageQcConstants.CATEGORY_IN));
    }
    faultyTypeListWebResponse.add(new ImageFaultyTypeWebResponse(ImageQcConstants.GOOD_EN, ImageQcConstants.GOOD_IN));
    for (ImageFaultyTypeWebResponse imageFaultyTypeWebResponse : faultyTypeListWebResponse) {
      if (listOfPredictions.contains(imageFaultyTypeWebResponse.getEnName())) {
        imageFaultyTypeWebResponse.setIgnoreForImageQc(true);
      }
      else if (notIgnoreImageQcPredictions.contains(imageFaultyTypeWebResponse.getEnName())) {
        imageFaultyTypeWebResponse.setIgnoreForImageQc(false);
      }
      imageFaultyTypeWebResponse.setLabelColour(
          labelColourMap.getOrDefault(imageFaultyTypeWebResponse.getEnName(), DEFAULT_LABEL_COLOUR));
    }
    return faultyTypeListWebResponse;
  }

  public static void validateProductState(GdnRestSingleResponse<ProductDetailCompleteResponse> productDetail) {
    if (productDetail.getValue().isActivated()) {
      log.error("Product state invalid : productCode {}", productDetail.getValue().getProductCode());
      throw new InvalidStateException(ErrorMessages.PRODUCT_STATE_INVALID);
    }
  }

  public static List<HistoryWebResponse> toHistoryWebResponseListFromProductHistoryResponse(
      List<ProductHistoryResponse> responses) {
    return responses.stream().map(ResponseHelper::toHistoryWebResponse).collect(Collectors.toList());
  }

  private static HistoryWebResponse toHistoryWebResponse(ProductHistoryResponse productHistoryResponse) {
    HistoryWebResponse historyWebResponse = new HistoryWebResponse();
    BeanUtils.copyProperties(productHistoryResponse, historyWebResponse);
    historyWebResponse.setActivity(productHistoryResponse.getDescription());
    historyWebResponse.setDescription(productHistoryResponse.getNotes());
    return historyWebResponse;
  }

  public static List<HistoryWebResponse> toHistoryWebResponseListFromTaskHistoryResponse(
      List<TaskHistoryResponse> responses) {
    return responses.stream().map(ResponseHelper::toTaskHistoryResponse).collect(Collectors.toList());
  }

  private static HistoryWebResponse toTaskHistoryResponse(TaskHistoryResponse taskHistoryResponse) {
    HistoryWebResponse historyWebResponse = new HistoryWebResponse();
    BeanUtils.copyProperties(taskHistoryResponse, historyWebResponse);
    historyWebResponse.setActivity(taskHistoryResponse.getState());
    historyWebResponse.setDescription(taskHistoryResponse.getReason());
    return historyWebResponse;
  }

  //TODO validate on the equals functionality
  public static boolean categoryWholesaleConfigCheck(WholesaleMappingResponse presentWholesaleMappingResponse,
      WholesaleMappingResponse targetWholesaleMappingResponse) {
    if (StringUtils.compare(presentWholesaleMappingResponse.getConfigurationType(),
        targetWholesaleMappingResponse.getConfigurationType()) != 0) {
      return false;
    }
    return Objects.equals(presentWholesaleMappingResponse.getWholesaleConfig(),
        targetWholesaleMappingResponse.getWholesaleConfig());
  }

  public static List<ProductCenterSummaryWebResponse> toListProductCenterSummaryWebResponse(
      List<ProductCenterSummaryResponse> productCenterSummaryResponses) {
    return productCenterSummaryResponses.stream()
        .map(productCenterSummaryResponse -> toProductCenterSummaryWebResponse(productCenterSummaryResponse))
        .collect(Collectors.toList());
  }

  private static ProductCenterSummaryWebResponse toProductCenterSummaryWebResponse(
      ProductCenterSummaryResponse productCenterSummaryResponse) {
    ProductCenterSummaryWebResponse productCenterSummaryWebResponse = new ProductCenterSummaryWebResponse();
    BeanUtils.copyProperties(productCenterSummaryResponse, productCenterSummaryWebResponse, "masterCategory");
    productCenterSummaryWebResponse.setMasterCategory(
        new MasterCategoryWebResponse(productCenterSummaryResponse.getMasterCategory().getCategoryCode(),
            productCenterSummaryResponse.getMasterCategory().getCategoryName()));
    return productCenterSummaryWebResponse;
  }

  public static ProductCenterDetailWebResponse toProductCenterDetail(ProductL3Response productL3Response,
      CategoryNamesResponse categoryNamesResponse, String masterCategoryCode) {
    String mainImage = productL3Response.getMasterDataProduct().getMasterDataProductImages().stream()
        .filter(MasterDataProductImageDTO::isMainImage).findFirst().map(MasterDataProductImageDTO::getLocationPath)
        .orElse(StringUtils.EMPTY);
    ProductCenterDetailWebResponse productCenterDetailWebResponse =
        ProductCenterDetailWebResponse.builder().productCode(productL3Response.getProductCode())
            .productName(productL3Response.getMasterDataProduct().getProductName())
            .productSku(productL3Response.getProductSku())
            .sellerCode(productL3Response.getMerchantCode())
            .lastUpdated(productL3Response.getProductCenterUpdatedDate())
            .imagePath(mainImage).status(productL3Response.isSuspended() ? SUSPENDED : ACTIVE).build();
    List<CatalogTreeWebResponse> catalogTreeWebResponseList = new ArrayList<>();
    for (ItemCatalogDTO itemCatalogDTO : productL3Response.getItemCatalogs()) {
      CatalogTreeWebResponse catalogTreeWebResponse = new CatalogTreeWebResponse();
      catalogTreeWebResponse.setCatalogCode(itemCatalogDTO.getCatalogId());
      catalogTreeWebResponse.setCategories(new ArrayList<>());
      for (ItemCategoryDTO itemCategoryDTO : itemCatalogDTO.getItemCategories()) {
        CategoryTreeWebResponse categoryTreeWebResponse =
            CategoryTreeWebResponse.builder().categoryName(itemCategoryDTO.getCategory())
                .categoryCode(itemCategoryDTO.getProductCategoryCode()).level(itemCategoryDTO.getLevel()).build();
        catalogTreeWebResponse.getCategories().add(categoryTreeWebResponse);
      }
      MasterCatalogDTO masterCatalogDTO = productL3Response.getMasterCatalog();
      if (Objects.isNull(masterCatalogDTO) || !catalogTreeWebResponse.getCatalogCode()
          .equals(masterCatalogDTO.getCatalogCode())) {
        catalogTreeWebResponseList.add(catalogTreeWebResponse);
      }
    }
    CategoryTreeWebResponse masterCategory = new CategoryTreeWebResponse();
    masterCategory.setCategoryCode(masterCategoryCode);
    masterCategory.setCategoryName(categoryNamesResponse.getCategoryMap().get(masterCategoryCode));
    productCenterDetailWebResponse.setMasterCategory(masterCategory);
    productCenterDetailWebResponse.setSalesCategory(catalogTreeWebResponseList);
    return productCenterDetailWebResponse;
  }

  public static ProductCenterHistoryWebResponse toProductCenterHistoryWebResponse(
      ProductCenterHistoryResponse productCenterHistoryResponse) {
    ProductCenterHistoryWebResponse productCenterHistoryWebResponse =
        ProductCenterHistoryWebResponse.builder().productSku(productCenterHistoryResponse.getProductSku())
            .activity(productCenterHistoryResponse.getActivity())
            .description(productCenterHistoryResponse.getDescription())
            .date(productCenterHistoryResponse.getUpdatedDate().getTime())
            .user(productCenterHistoryResponse.getUpdatedBy()).build();
    return productCenterHistoryWebResponse;
  }

  public static ProductDetailWebResponse toProductDetailWebResponseFromProductDetailCompleteResponse(
      ProductDetailCompleteResponse productDetailCompleteResponse, ProfileResponse profileResponse) {
    ProductDetailWebResponse response = toProductDetailWebResponse(productDetailCompleteResponse, profileResponse);
    if (Objects.nonNull(productDetailCompleteResponse.getPreOrder())) {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      BeanUtils.copyProperties(productDetailCompleteResponse.getPreOrder(), preOrderResponse);
      response.setPreOrder(preOrderResponse);
    } else {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      preOrderResponse.setIsPreOrder(false);
      response.setPreOrder(preOrderResponse);
    }
    response.setRestrictedKeywordsDetected(
        toRestrictedKeywordsByFieldWebResponse(productDetailCompleteResponse.getRestrictedKeywordsDetected()));
    return response;
  }

  public static ProductDetailWebResponse setValueConcatenatedWithValueType(
      ProductDetailWebResponse productDetailWebResponse, String sizeChartDelimiter) {
    if (Objects.nonNull(productDetailWebResponse)) {
      for (ProductAttributeWebResponse productAttributeWebResponse :
          productDetailWebResponse.getProductAttributeResponses()) {
        setProductAttributeWebResponseValueConcatenatedWithDelimiter(sizeChartDelimiter,
            productAttributeWebResponse);
      }
    }
    return productDetailWebResponse;
  }

  private static void setProductAttributeWebResponseValueConcatenatedWithDelimiter(
      String sizeChartDelimiter, ProductAttributeWebResponse productAttributeWebResponse) {
    for (ProductAttributeValueWebResponse productAttributeValueWebResponse :
 productAttributeWebResponse.getProductAttributeValues()) {
      if (Objects.nonNull(productAttributeValueWebResponse.getAllowedAttributeValue())) {
        setProductAttributeValueConcatenatedWithDelimiter(sizeChartDelimiter,
            productAttributeValueWebResponse);
      }
    }
  }

  private static void setProductAttributeValueConcatenatedWithDelimiter(String sizeChartDelimiter,
      ProductAttributeValueWebResponse productAttributeValueWebResponse) {
    AllowedAttributeValueWebResponse allowedAttributeValueWebResponse =
        productAttributeValueWebResponse.getAllowedAttributeValue();
    if (Objects.nonNull(allowedAttributeValueWebResponse.getValueType())) {
      StringBuilder valueConcatenatedWithValueType = new StringBuilder();
      valueConcatenatedWithValueType.append(allowedAttributeValueWebResponse.getValueType())
          .append(sizeChartDelimiter).append(allowedAttributeValueWebResponse.getValue());
      allowedAttributeValueWebResponse.setValue(valueConcatenatedWithValueType.toString());
    }
  }

  private static List<RestrictedKeywordsDetectedWebResponse> toRestrictedKeywordsByFieldWebResponse(
      List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList) {
    return Optional.ofNullable(restrictedKeywordsByFieldResponseList).orElse(new ArrayList<>()).stream().map(
        restrictedKeywordsByFieldResponse -> new RestrictedKeywordsDetectedWebResponse(
            restrictedKeywordsByFieldResponse.getFieldIdentifier(), restrictedKeywordsByFieldResponse.getKeywords()))
        .collect(Collectors.toList());
  }

  public static Page<RecatProcessSummaryWebResponse> toRecatProcessSummaryWebResponsePage(
      Page<RecatProcessSummaryResponse> recatProcessSummaryResponses) {
    return new PageImpl<>(recatProcessSummaryResponses.getContent().stream().map(
        recatProcessSummaryResponse -> ResponseHelper
            .toRecatProcessSummaryWebResponse(recatProcessSummaryResponse))
        .collect(Collectors.toList()), PageRequest.of(recatProcessSummaryResponses.getNumber(),
        recatProcessSummaryResponses.getSize()), recatProcessSummaryResponses.getTotalElements());
  }

  private static RecatProcessSummaryWebResponse toRecatProcessSummaryWebResponse(
      RecatProcessSummaryResponse recatProcessSummaryResponse) {
    RecatProcessSummaryWebResponse recatProcessSummaryWebResponse =
        new RecatProcessSummaryWebResponse();
    BeanUtils.copyProperties(recatProcessSummaryResponse, recatProcessSummaryWebResponse);
    return recatProcessSummaryWebResponse;
  }

  public static List<RecatProductSummaryWebResponse> toRecatProductSummaryWebResponse(
      List<RecatProductSummaryResponse> recatProductSummaryResponseList) {
    List<RecatProductSummaryWebResponse> recatProductSummaryWebResponseList = new ArrayList<>();
    for (RecatProductSummaryResponse recatProductSummaryResponse : recatProductSummaryResponseList) {
      RecatProductSummaryWebResponse recatProductSummaryWebResponse = new RecatProductSummaryWebResponse();
      BeanUtils.copyProperties(recatProductSummaryResponse, recatProductSummaryWebResponse);
      if (Constants.BULK_PROCESS_STATE_PENDING.equals(recatProductSummaryResponse.getStatus())) {
        recatProductSummaryWebResponse.setStatus(Constants.BULK_PROCESS_STATE_IN_PROGRESS);
      }
      recatProductSummaryWebResponseList.add(recatProductSummaryWebResponse);
    }
    return recatProductSummaryWebResponseList;
  }

  public static List<BulkInternalProcessSummaryWebResponse> toBulkInternalProcessSummaryWebResponse(
      List<BulkInternalProcessSummaryResponse> bulkInternalProcessSummaryResponseList) {
    List<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummaryWebResponseList = new ArrayList<>();
    for (BulkInternalProcessSummaryResponse bulkInternalProcessSummaryResponse : bulkInternalProcessSummaryResponseList) {
      BulkInternalProcessSummaryWebResponse bulkInternalProcessSummaryWebResponse =
          new BulkInternalProcessSummaryWebResponse();
      BeanUtils.copyProperties(bulkInternalProcessSummaryResponse, bulkInternalProcessSummaryWebResponse);
        bulkInternalProcessSummaryWebResponse.setFilePath(bulkInternalProcessSummaryResponse.getFileName());
      bulkInternalProcessSummaryWebResponseList.add(bulkInternalProcessSummaryWebResponse);
    }
    return bulkInternalProcessSummaryWebResponseList;
  }

  public static RecatProductCountWebResponse toRecatProductCountWebResponse(
      RecatProductCountResponse recatProductCountResponse) {
    return RecatProductCountWebResponse.builder().totalCount(recatProductCountResponse.getTotalProductCount())
        .successCount(recatProductCountResponse.getSuccessCount())
        .failedCount(recatProductCountResponse.getFailedCount())
        .inProgressCount(recatProductCountResponse.getInProgressCount()).status(recatProductCountResponse.getStatus())
        .build();
  }

  public static PendingDownloadProcessWebResponse toPendingDownloadProcessWebResponse(
      BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse) {
    PendingDownloadProcessWebResponse pendingDownloadProcessWebResponse = new PendingDownloadProcessWebResponse();
    pendingDownloadProcessWebResponse
        .setBulkInternalProcessStatusFlag(bulkInternalPendingRequestResponse.isBulkInternalStatusFlag());
    pendingDownloadProcessWebResponse.setPendingCount(bulkInternalPendingRequestResponse.getPendingRequestsCount());
    return pendingDownloadProcessWebResponse;
  }

  public static List<BrandAuthFilterWebResponse> toBrandAuthFilterWebResponseList(
      List<BrandAuthFilterResponse> responses, Map<String, ProfileResponse> profileResponseMap,
      boolean brandAuthorise) {
    return responses.stream().map(
        brand -> ResponseHelper.toBrandAuthFilterWebResponse(brand, profileResponseMap,
            brandAuthorise)).collect(Collectors.toList());
  }

  public static List<BrandAuthorisationWipListResponse> toBrandAuthorisationWipResponseList(
      List<BrandAuthorisationWipListResponse> responses, int brandAuthWipNearExpiryDaysThreshold) {
    return responses.stream().map(response -> toBrandAuthorisationWipResponse(response, brandAuthWipNearExpiryDaysThreshold))
        .collect(Collectors.toList());
  }

  private static BrandAuthorisationWipListResponse toBrandAuthorisationWipResponse(
      BrandAuthorisationWipListResponse response, int brandAuthWipNearExpiryDaysThreshold) {
    BrandAuthorisationWipListResponse brandAuthorisationWipListResponse =
        new BrandAuthorisationWipListResponse();
    BeanUtils.copyProperties(response, brandAuthorisationWipListResponse);
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(response.getAuthExpireDate());
    calendar.add(Calendar.DAY_OF_MONTH, -brandAuthWipNearExpiryDaysThreshold);
    Date thresholdDate = calendar.getTime();
    if(BrandAuthorisationStatus.ACTIVE.name().equalsIgnoreCase(response.getAuthorisationStatus())) {
      if (response.getAuthStartDate().after(date) || response.getAuthExpireDate().before(date)) {
        brandAuthorisationWipListResponse.setAuthorisationStatus(
            BrandAuthorisationStatus.INACTIVE.name());
      } else if (thresholdDate.before(date)) {
        brandAuthorisationWipListResponse.setAuthorisationStatus(NEAR_EXPIRY);
      }
    }
    return brandAuthorisationWipListResponse;
  }

  private static BrandAuthFilterWebResponse toBrandAuthFilterWebResponse(BrandAuthFilterResponse response,
      Map<String, ProfileResponse> sellerCodeMap, boolean brandAuthorise) {
    BrandAuthFilterWebResponse brandAuthFilterWebResponse = new BrandAuthFilterWebResponse();
    BeanUtils.copyProperties(response, brandAuthFilterWebResponse);
    Date date = new Date();
    if (brandAuthorise && (
        BrandAuthorisationStatus.ACTIVE.name().equalsIgnoreCase(response.getStatus()) && (
            response.getAuthStartDate().after(date) || response.getAuthEndDate().before(date)))) {
      brandAuthFilterWebResponse.setStatus(BrandAuthorisationStatus.INACTIVE.name());
    }
    if (sellerCodeMap.containsKey(response.getSellerCode())) {
      brandAuthFilterWebResponse.setSellerName(
        sellerCodeMap.get(response.getSellerCode()).getCompany().getBusinessPartnerName());
    }
    return brandAuthFilterWebResponse;
  }

  public static List<ProductImagePredictionAndCategoryMappingWebResponse> toProductImagePredictionAndCategoryMappingResponse(
      List<ProductImagePredictionAndCategoryMappingResponse> productImagePredictionAndCategoryMappingResponseList) {
    List<ProductImagePredictionAndCategoryMappingWebResponse> productImagePredictionAndCategoryMappingWebResponseList =
        new ArrayList<>();
    for (ProductImagePredictionAndCategoryMappingResponse productImagePredictionAndCategoryMappingResponse : productImagePredictionAndCategoryMappingResponseList) {
      ProductImagePredictionAndCategoryMappingWebResponse productImagePredictionAndCategoryMappingWebResponse =
          new ProductImagePredictionAndCategoryMappingWebResponse();
      productImagePredictionAndCategoryMappingWebResponse.setPredictionType(
          productImagePredictionAndCategoryMappingResponse.getPredictionType());
      productImagePredictionAndCategoryMappingWebResponse.setRuleEnabled(
          productImagePredictionAndCategoryMappingResponse.isRuleEnabled());
      productImagePredictionAndCategoryMappingWebResponse.setConfidenceThreshold(
          productImagePredictionAndCategoryMappingResponse.getConfidenceThreshold());
      productImagePredictionAndCategoryMappingWebResponse.setTextConfidenceThreshold(
          productImagePredictionAndCategoryMappingResponse.getTextConfidenceThreshold());
      List<PredictionCategoryMappingWebResponse> predictionCategoryMappingWebResponseList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(
          productImagePredictionAndCategoryMappingResponse.getCategoryCodeAndCategoryNameResponseList())) {
        for (CategoryCodeAndCategoryNameResponse categoryCodeAndCategoryNameResponse : productImagePredictionAndCategoryMappingResponse.getCategoryCodeAndCategoryNameResponseList()) {
          PredictionCategoryMappingWebResponse predictionCategoryMappingWebResponse =
              new PredictionCategoryMappingWebResponse();
          predictionCategoryMappingWebResponse.setCategoryCode(categoryCodeAndCategoryNameResponse.getCategoryCode());
          predictionCategoryMappingWebResponse.setCategoryName(categoryCodeAndCategoryNameResponse.getCategoryName());
          predictionCategoryMappingWebResponseList.add(predictionCategoryMappingWebResponse);
        }
      }
      productImagePredictionAndCategoryMappingWebResponse.setPredictionCategoryMappingWebResponseList(
          predictionCategoryMappingWebResponseList);
      productImagePredictionAndCategoryMappingWebResponseList.add(productImagePredictionAndCategoryMappingWebResponse);
    }
    return productImagePredictionAndCategoryMappingWebResponseList;
  }

  public static List<HalalCertificationWebDetailsResponse> toHalalCertificationDetailResponse(
      BPJPHData halalCertificationDetailResponses) {
    List<HalalCertificationWebDetailsResponse> halalCertificationWebDetailsResponses = new ArrayList<>();
    for (HalalCertificationDetailResponse response : halalCertificationDetailResponses.getDatas()) {
      HalalCertificationWebDetailsResponse halalCertificationWebDetailsResponse =
          new HalalCertificationWebDetailsResponse();
      if (Objects.nonNull(response.getSertifikat())) {
        halalCertificationWebDetailsResponse.setCertificationNumber(response.getSertifikat().getNo_sert());
        halalCertificationWebDetailsResponse.setIssuedDate(response.getSertifikat().getTgl_sert());
        halalCertificationWebDetailsResponse.setExpirationDate(response.getSertifikat().getTgl_valid());
        halalCertificationWebDetailsResponse.setProductName(response.getReg_prod_name());
        halalCertificationWebDetailsResponses.add(halalCertificationWebDetailsResponse);
      }
    }
    return halalCertificationWebDetailsResponses;
  }

  public static HalalProductHistoryWebResponse getHalalProductHistoryWebResponse(
      HalalProductHistoryResponse halalProductHistoryResponse) {
    HalalProductHistoryWebResponse response = new HalalProductHistoryWebResponse();
    BeanUtils.copyProperties(halalProductHistoryResponse, response, "id", "storeId", "version");
    return response;
  }

  public static HalalDashboardProductsWebResponse getHalaDashboardProductsWebResponse(
      HalalDashboardProductsResponse halalDashboardProductsResponse, String halalProductLinkPrefix) {
    HalalDashboardProductsWebResponse response = new HalalDashboardProductsWebResponse();
    BeanUtils.copyProperties(halalDashboardProductsResponse, response, "id", "storeId", "version");
    response.setProductLink(halalProductLinkPrefix+halalDashboardProductsResponse.getProductSku());
    return response;
  }

  public static void setPdpRedirectionLink(String pdpLinkPrefix, List<ItemsListingResponse> itemsListingResponseList) {
    itemsListingResponseList.forEach(itemsListingResponse -> itemsListingResponse.setPdpRedirectionLink(
        getPdpRedirectionLinkWithItemSku(pdpLinkPrefix, itemsListingResponse.getItemSku())));
  }

  public static void setPdpRedirectionLinkForItemDetailResponses(String pdpLinkPrefix,
      List<ItemSkuDetailResponse> itemSkuDetailResponseList) {
    itemSkuDetailResponseList.forEach(itemSkuDetailResponse -> itemSkuDetailResponse.setPdpRedirectionLink(
        getPdpRedirectionLinkWithItemSku(pdpLinkPrefix, itemSkuDetailResponse.getItemSku())));
  }

  public static String getPdpRedirectionLinkWithItemSku(String pdpLinkPrefix, String itemSku) {
    return new StringBuilder().append(pdpLinkPrefix).append(itemSku).toString();
  }

  public static boolean checkMismatchForCategory(Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew,
      Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting) {
    Double marginValueForNew = getMarginValue(orderItemMarginsResponseForNew);
    Double marginValueForExisting = getMarginValue(orderItemMarginsResponseForExisting);
    if(Objects.isNull(marginValueForNew) || Objects.isNull(marginValueForExisting)){
      return false;
    }
    return marginValueForNew > marginValueForExisting;
  }

  public static Double getMarginValue(Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting) {
    if (Objects.nonNull(orderItemMarginsResponseForExisting)) {
      if (orderItemMarginsResponseForExisting.isPresent()) {
        Optional<Margin> marginForExisting =
            orderItemMarginsResponseForExisting.map(OrderItemMarginsResponse::getMargins).orElse(new ArrayList<>())
                .stream().filter(Objects::nonNull).findFirst();
        if (marginForExisting.isPresent()) {
          if (Objects.nonNull(marginForExisting.get().getMarginPercentage()))
            return marginForExisting.get().getMarginPercentage();
        }
      }
    }
    return null;
  }

  private static String populateSellerStatus(ProfileResponse profileResponse) {
    String sellerStatus = SellerStatus.populateSellerStatus(profileResponse.getMerchantStatus());
    if (SellerStatus.INACTIVE.getStatus().equals(sellerStatus)
      && !profileResponse.isSuspensionFlag()) {
      return SellerStatus.INACTIVE.name();
    } else if (SellerStatus.INACTIVE.getStatus().equals(sellerStatus)) {
      return SellerStatus.SUSPENDED.name();
    }
    return sellerStatus;
  }

  public static void populatePdpRedirectionUrl(List<IprSuspensionInProgressResponse> iprResponses,
      String pdpLinkProductSkuPrefix) {
    for (IprSuspensionInProgressResponse iprSuspensionInProgressResponse : iprResponses) {
      StringBuilder stringBuilder = new StringBuilder();
      iprSuspensionInProgressResponse.setPdpRedirectionUrl(
          stringBuilder.append(pdpLinkProductSkuPrefix)
              .append(iprSuspensionInProgressResponse.getProductSku()).toString());
    }
  }

  public static String toProductDetailPage(String productCode, String productDetailPageUrlPrefix) {
    return productDetailPageUrlPrefix.concat(productCode);
  }

  public static void populateAiGeneratedFields(ProductDetailWebResponse productDetailWebResponse,
      GdnRestSingleResponse<ProductDetailCompleteResponse> response) {
    Optional.ofNullable(response).map(GdnRestSingleResponse::getValue)
        .map(ProductDetailCompleteResponse::getAiGeneratedFieldsResponse).ifPresent(aiFields -> {
          productDetailWebResponse.setAiGeneratedBrand(aiFields.isAiGeneratedBrand());
          productDetailWebResponse.setAiGeneratedCategory(aiFields.isAiGeneratedCategory());
        });
  }
}

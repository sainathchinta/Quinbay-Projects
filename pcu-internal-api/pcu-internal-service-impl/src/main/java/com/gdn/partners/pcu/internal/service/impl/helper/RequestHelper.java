package com.gdn.partners.pcu.internal.service.impl.helper;

import static com.gdn.partners.pcu.internal.model.Constants.DOT;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.EMPTY_DOCUMENT_ERROR;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.EMPTY_IPR_REGISTRATION_NUMBER_EXCEEDED_MAX_LENGTH;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.PredictionCategoryMappingUpdateRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.model.request.BigQueryFetchRequest;
import com.gdn.partners.pcu.internal.client.model.request.VendorAutoAssignmentRequest;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.ImageQcConstants;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AnchorMappingRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AutoApprovedAllProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AutoApprovedSelectedProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkBrandAuthDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.IPRProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuInReviewDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuReviewAllItemsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuReviewSelectedItemsDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.AnchorMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovalEligibilityWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandRejectWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageFeedbackWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemViewConfigWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.OrderItem;
import com.gdn.partners.pcu.internal.web.model.request.PDTStateUpdateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredictionCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UserFeedbackRequest;
import com.gdn.partners.pcu.internal.web.model.request.UserImageFeedbackRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.Nullable;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Pradeep Reddy
 */
@Slf4j
public class RequestHelper {

  private static final String EMPTY_DROPDOWN_LIST_ELEMENT = "NA";
  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";
  private static final String CONFIGURATION = "Configuration";
  private static final String FLAG = "MERCHANT";
  private static final String STATUS = "ACTIVE";
  private static final String SORT_DIRECTION = "ASC";
  private static final DateFormat format = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
  private static final String START_OF_DAY = " 00:00:00";
  private static final String END_OF_DAY = " 23:59:59";
  private static final String AUTO_NEED_REVISION ="Auto need revision";
  private static final String FORCE_REVIEW = "Force review";
  private static final String STORE_COPY_PROCESS_TYPE = "STORE_COPY";
  private static final String DELETE_BRAND_AUTHORISATION = "DELETE_BRAND_AUTHORISATION";
  private static final String SALES_CATEGORY_UPDATE_PROCESS_TYPE = "SALES_CATEGORY_UPDATE";
  private static final String BULK_PRICE_UPDATE_PROCESS_TYPE = "BULK_PRICE_UPDATE";
  private static final String DATE_FORMAT = "dd/MM/yyyy";
  private static final String UPLOAD_DATE = "UPLOAD_DATE";
  private static final int MAX_FILE_ALLOWED = 5;
  public static final String ORDER_ITEM_ID = "-";

  public static String generateSpecificationDetail(ProductRequest request) throws Exception {
    StringBuilder specDetail = new StringBuilder("<ul>");
    for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
      if (!productAttributeRequest.getAttribute().isSkuValue()) {
        specDetail.append("<li>").append(productAttributeRequest.getAttribute().getName()).append("<ul>");
        if (productAttributeRequest.getAttribute().getAttributeType().equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
          specDetail.append("<li>")
              .append(productAttributeRequest.getProductAttributeValues().get(0).getDescriptiveAttributeValue()).append("</li>");
        } else if (productAttributeRequest.getAttribute().getAttributeType()
            .equals(AttributeType.PREDEFINED_ATTRIBUTE)) {
          specDetail.append("<li>").append(
              productAttributeRequest.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().getValue()).append("</li>");
        } else if (productAttributeRequest.getAttribute().getAttributeType()
            .equals( // Have to check on this use-case
                AttributeType.DEFINING_ATTRIBUTE)) { // Go inside else if AttributeType ==
          // AttributeType.DEFINING_ATTRIBUTE
          for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest
              .getProductAttributeValues()) {
            specDetail.append("<li>").append(productAttributeValueRequest.getAllowedAttributeValue().getValue())
                .append("</li>");
          }
        }
        specDetail.append("</ul></li>");
      }
    }
    specDetail.append("</ul>");
    return specDetail.toString();
  }

  public static SummaryFilterRequest getSummaryFilterRequest(ReviewProductsFilterRequest request) {
    String searchKeyword =
        StringUtils.isBlank(request.getSearchKeyword()) ? StringUtils.EMPTY : request.getSearchKeyword();
    return SummaryFilterRequest.builder().statusFilter(request.getStatusFilter()).timeFilter(request.getTimeFilter())
        .searchKeyword(searchKeyword).build();
  }

  public static BusinessPartnerFilterRequest getBusinessPartnerFilter(ProductSuspensionFilterRequest request) {
    String searchKeyword =
        StringUtils.isBlank(request.getSearchKeyword()) ? StringUtils.EMPTY : request.getSearchKeyword();
    return BusinessPartnerFilterRequest.builder().status(STATUS).category(request.getCategoryCode())
        .businessPartnerCodes(null).keywords(searchKeyword).flag(FLAG).merchantTypes(null).sortDirection(SORT_DIRECTION)
        .sortedBy(StringUtils.EMPTY).tags(null).type(StringUtils.EMPTY).build();
  }

  public static BusinessPartnerFilterRequest getBusinessPartnerFilterFromKeyword(String keyword) {
    return BusinessPartnerFilterRequest.builder().status(STATUS).category(null)
        .businessPartnerCodes(null).keywords(keyword).flag(FLAG).merchantTypes(null).sortDirection(SORT_DIRECTION)
        .sortedBy(StringUtils.EMPTY).tags(null).type(StringUtils.EMPTY).build();
  }

  public static ScreeningProductBulkActionsRequest toScreeningProductBulkActionsRequest(
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    return ScreeningProductBulkActionsRequest.builder()
        .productCodes(screeningProductBulkActionsWebRequest.getProductCodes())
        .assignTo(screeningProductBulkActionsWebRequest.getAssignTo())
        .assignedBy(screeningProductBulkActionsWebRequest.getAssignedBy())
        .correctionReason(screeningProductBulkActionsWebRequest.getCorrectionReason())
        .rejectionReason(screeningProductBulkActionsWebRequest.getRejectionReason())
        .additionalNotes(screeningProductBulkActionsWebRequest.getAdditionalNotes())
        .vendorNotes(screeningProductBulkActionsWebRequest.getVendorNotes())
        .contentAdditionalNotes(screeningProductBulkActionsWebRequest.getContentAdditionalNotes())
        .imageReason(screeningProductBulkActionsWebRequest.getImageReason())
        .imagesAdditionalNotes(screeningProductBulkActionsWebRequest.getImagesAdditionalNotes())
        .vendorErrorFields(screeningProductBulkActionsWebRequest.getVendorErrorFields())
        .allVariants(screeningProductBulkActionsWebRequest.isAllVariants()).build();
  }

  public static NeedRevisionRequest toNeedRevisionRequest(
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    NeedRevisionRequest needRevisionRequest =
        NeedRevisionRequest.builder().productCodes(screeningProductBulkActionsWebRequest.getProductCodes())
            .assignTo(screeningProductBulkActionsWebRequest.getAssignTo())
            .assignedBy(screeningProductBulkActionsWebRequest.getAssignedBy())
            .correctionReason(screeningProductBulkActionsWebRequest.getCorrectionReason())
            .rejectionReason(screeningProductBulkActionsWebRequest.getRejectionReason())
            .additionalNotes(screeningProductBulkActionsWebRequest.getAdditionalNotes()).build();
    needRevisionRequest.setProductNotesRequest(
        ProductNotesRequest.builder().allVariants(screeningProductBulkActionsWebRequest.isAllVariants())
            .contentAdditionalNotes(screeningProductBulkActionsWebRequest.getContentAdditionalNotes())
            .imageReason(screeningProductBulkActionsWebRequest.getImageReason())
            .commonImageReason(screeningProductBulkActionsWebRequest.getCommonImageReason())
            .imagesAdditionalNotes(screeningProductBulkActionsWebRequest.getImagesAdditionalNotes())
            .vendorErrorFields(screeningProductBulkActionsWebRequest.getVendorErrorFields())
            .vendorNotes(screeningProductBulkActionsWebRequest.getVendorNotes()).build());
    needRevisionRequest.setNeedRevisionType(screeningProductBulkActionsWebRequest.getNeedRevisionType());
    if (CollectionUtils.isNotEmpty(screeningProductBulkActionsWebRequest.getItemNotes())) {
      needRevisionRequest.setItemNotes(screeningProductBulkActionsWebRequest.getItemNotes().stream().map(
          itemNotesWebRequest -> ItemNotesRequest.builder().itemName(itemNotesWebRequest.getItemName())
              .itemNumber(itemNotesWebRequest.getItemNumber()).itemSku(itemNotesWebRequest.getItemSku())
              .vendorNotes(itemNotesWebRequest.getVendorNotes()).skuCode(itemNotesWebRequest.getSkuCode())
              .vendorErrorFields(itemNotesWebRequest.getVendorErrorFields()).build()).collect(Collectors.toList()));
    }
    return needRevisionRequest;
  }

  public static ScreeningProductBulkActionsRequest toScreeningProductBulkActionsRequestForNeedRevision(
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    BeanUtils.copyProperties(screeningProductBulkActionsWebRequest, screeningProductBulkActionsRequest);
    return screeningProductBulkActionsRequest;
  }

  public static SuspensionProductRequest toSuspensionProductRequest(
      SuspensionProductBulkActionsWebRequest suspensionProductBulkActionsWebRequest) {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    BeanUtils.copyProperties(suspensionProductBulkActionsWebRequest, suspensionProductRequest, "products");
    List<ProductLevel3Request> products = new ArrayList<>();
    for (ProductSuspensionWebRequest productSuspensionWebRequest : suspensionProductBulkActionsWebRequest
        .getProducts()) {
      ProductLevel3Request request = new ProductLevel3Request();
      request.setProductName(productSuspensionWebRequest.getProductName());
      request.setProductSku(productSuspensionWebRequest.getProductSku());
      request.setBusinessPartnerCode(productSuspensionWebRequest.getMerchantCode());
      products.add(request);
    }
    suspensionProductRequest.setProducts(products);
    return suspensionProductRequest;
  }

  public static List<AttributeReqModel> toAttributeReqModelList(
      List<ProductSuggestionWebRequest> productSuggestionWebRequests) {
    return productSuggestionWebRequests.stream().map(RequestHelper::toAttributeReqModel).collect(Collectors.toList());
  }

  private static AttributeReqModel toAttributeReqModel(ProductSuggestionWebRequest productSuggestionWebRequest) {
    return new AttributeReqModel(productSuggestionWebRequest.getName(), productSuggestionWebRequest.getValue());
  }

  public static BrandApproveRequest approveBrandWipWebRequestToBrandApproveRequest(
      ApproveBrandWipWebRequest approveBrandWipWebRequest, MultipartFile brandLogo, MultipartFile profileBanner) {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    BeanUtils.copyProperties(approveBrandWipWebRequest, brandApproveRequest, "brandLogoPath", "profileBannerPath");
    if (Objects.nonNull(brandLogo)) {
      brandApproveRequest.setBrandLogoPath(generatePath(approveBrandWipWebRequest.getBrandName(), brandLogo));
    } else {
      brandApproveRequest.setBrandLogoPath(null);
    }
    if (Objects.nonNull(profileBanner)) {
      brandApproveRequest.setProfileBannerPath(generatePath(approveBrandWipWebRequest.getBrandName(), profileBanner));
    } else {
      brandApproveRequest.setProfileBannerPath(null);
    }
    return brandApproveRequest;
  }

  public static BrandApproveRequest toBrandApproveRequest(ApproveBrandWipWebRequest approveBrandWipWebRequest,
      MultipartFile brandLogo, MultipartFile profileBanner) {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    BeanUtils.copyProperties(approveBrandWipWebRequest, brandApproveRequest, "brandLogoPath", "profileBannerPath");
    if (Objects.nonNull(brandLogo)) {
      brandApproveRequest
          .setBrandLogoPath(generatePath(approveBrandWipWebRequest.getBrandName(), brandLogo));
    }
    if (Objects.nonNull(profileBanner)) {
      brandApproveRequest.setProfileBannerPath(
          generatePath(approveBrandWipWebRequest.getBrandName(), profileBanner));
    }
    return brandApproveRequest;
  }

  public static String generatePath(String brandName, MultipartFile file, String fileExtension) {
    String fileType = file.getOriginalFilename().substring(file.getOriginalFilename().lastIndexOf(Constants.DOT) + 1);
    return brandName.replaceAll(Constants.NOT_ALPHA_NUMERIC_REGEX, Constants.HYPHEN).toLowerCase() + fileExtension
        + Constants.HYPHEN + RandomStringUtils.random(4, false, true) + fileType;
  }

  public static String generatePath(String brandName, MultipartFile file) {
    String fileType =
      file.getOriginalFilename().substring(file.getOriginalFilename().lastIndexOf(Constants.DOT) + 1);
    String lowercaseFilename = brandName.replaceAll(Constants.NOT_ALPHA_NUMERIC_REGEX, Constants.HYPHEN).toLowerCase();
    return lowercaseFilename
      .substring(0, Math.min(lowercaseFilename.length(), Constants.FILENAME_MAX_LENGTH))
      + Constants.HYPHEN + UUID.randomUUID().toString() + Constants.DOT + fileType;
  }

  public static BrandRejectRequest toBrandRejectRequest(BrandRejectWebRequest brandRejectWebRequest) {
    BrandRejectRequest brandRejectRequest = new BrandRejectRequest();
    BeanUtils.copyProperties(brandRejectWebRequest, brandRejectRequest);
    return brandRejectRequest;
  }

  public static FilterSummaryRequest toFilterSummaryRequest(VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest,
      String vendorCode) {
    FilterSummaryRequest summaryFilterRequest = new FilterSummaryRequest();
    BeanUtils.copyProperties(vendorSummaryFilterWebRequest, summaryFilterRequest, "timeFilterWebType", "faultyType");
    summaryFilterRequest.setTimeFilterType(
        TimeFilterType.getTimeFilterTypeByValue(vendorSummaryFilterWebRequest.getTimeFilterWebType()));
    summaryFilterRequest.setSortOrderByCreatedDate(vendorSummaryFilterWebRequest.getSortOrder());
    if (EMPTY_DROPDOWN_LIST_ELEMENT.equals(vendorSummaryFilterWebRequest.getAssigneeEmailId())) {
      summaryFilterRequest.setAssigneeEmailId(StringUtils.EMPTY);
    }
    if(StringUtils.isNotEmpty(vendorSummaryFilterWebRequest.getFaultyType())) {
      summaryFilterRequest.setFaultyImageType(vendorSummaryFilterWebRequest.getFaultyType());
    }
    summaryFilterRequest.setVendorCode(vendorCode);
    return summaryFilterRequest;
  }

  public static SummaryFilterRequest toSummaryFilterRequest(
      ProductSuspensionFilterRequest productSuspensionFilterRequest) {
    return SummaryFilterRequest.builder().businessPartnerCode(productSuspensionFilterRequest.getBusinessPartnerCode())
        .categoryCode(productSuspensionFilterRequest.getCategoryCode())
        .searchKeyword(productSuspensionFilterRequest.getSearchKeyword())
        .suspensionStatus(productSuspensionFilterRequest.getStatus()).build();
  }

  public static void createDirIfNotExists(String dirPath) {
    if (!new File(dirPath).exists()) {
      File f = new File(dirPath);
      try {
        f.mkdirs();
      } catch (Exception ex) {
        log.error("Error create dir", ex);
      }
    }
  }

  public static BulkVendorProductAssignRequest toBulkVendorProductAssignRequest(String storeId, String fileName,
      String vendorCode, String requestId, String userName, Map<String, List<String>> userList, String bipRequestCode) {
    return BulkVendorProductAssignRequest.builder().storeId(storeId).bulkProcessType(Constants.VENDOR_BULK_ASSIGN)
        .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).vendorCode(vendorCode).requestId(requestId)
        .updatedBy(userName).validUserRoleList(userList).internalProcessRequestCode(bipRequestCode).build();
  }

  public static BulkProductSuspensionRequest toBulkProductSuspensionRequest(String storeId, String fileName,
      String type, String requestId, String userName) {
    return BulkProductSuspensionRequest.builder().storeId(storeId).bulkProcessType(PRODUCT_LEVEL_3)
        .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).actionType(type).requestId(requestId)
        .updatedBy(userName).build();
  }

  public static List<MerchantConfigurationRequest> toMerchantConfigurationRequestList(List<ProfileResponse> profileResponseList) {
    List<MerchantConfigurationRequest> merchantConfigurationRequestList = new ArrayList<>();
    for(ProfileResponse profileResponse : profileResponseList) {
      merchantConfigurationRequestList.add(new MerchantConfigurationRequest(profileResponse.getBusinessPartnerCode(),
          profileResponse.getCompany().getBusinessPartnerName(), null));
    }
    return merchantConfigurationRequestList;
  }

  public static List<ConfigurationStatusRequest> toConfigurationStatusRequestList(
      List<ConfigurationWebRequest> configurationWebRequestList) {
    List<ConfigurationStatusRequest> configurationStatusRequestList = new ArrayList<>();
    for (ConfigurationWebRequest request : configurationWebRequestList) {
      configurationStatusRequestList
          .add(new ConfigurationStatusRequest(request.getMerchantCode(), request.getCategoryCode()));
    }
    return configurationStatusRequestList;
  }

  public static ConfigurationFilterRequest toConfigurationFilterRequest(
      ConfigurationFilterWebRequest configurationFilterWebRequest) {
    ConfigurationFilterRequest categoryConfigurationFilterRequest = new ConfigurationFilterRequest();
    BeanUtils.copyProperties(configurationFilterWebRequest, categoryConfigurationFilterRequest);
    return categoryConfigurationFilterRequest;
  }

  public static BulkConfigurationUpdateRequest toBulkConfigurationUpdateRequest(String storeId, String fileName,
      String type, String requestId, String username) {
    return BulkConfigurationUpdateRequest.builder().storeId(storeId).bulkProcessType(CONFIGURATION)
        .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).actionType(type).requestId(requestId)
        .updatedBy(username).build();
  }

  public static MasterDataBulkUpdateRequest toMasterDataBulkUpdateRequest(String requestId, String storeId,
      String filePath, String bulkUploadCode, String userName, String clientHost, String bipRequestCode) {
    MasterDataBulkUpdateRequest masterDataBulkUpdateRequest = new MasterDataBulkUpdateRequest();
    masterDataBulkUpdateRequest.setRequestId(requestId);
    masterDataBulkUpdateRequest.setTimestamp(System.currentTimeMillis());
    masterDataBulkUpdateRequest.setBulkProcessCode(bulkUploadCode);
    masterDataBulkUpdateRequest.setFilePath(filePath);
    masterDataBulkUpdateRequest.setStoreId(storeId);
    masterDataBulkUpdateRequest.setUpdatedBy(userName);
    masterDataBulkUpdateRequest.setClientHost(clientHost);
    masterDataBulkUpdateRequest.setEmailTo(userName);
    masterDataBulkUpdateRequest.setEmailCC(userName);
    masterDataBulkUpdateRequest.setBipRequestCode(bipRequestCode);
    return masterDataBulkUpdateRequest;
  }

  public static DistributionTaskMultipleFilterRequest toDistributionTaskMultipleFilterRequest(CountWebRequest countWebRequest) {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    if (Objects.nonNull(countWebRequest.getStatus())) {
      distributionTaskMultipleFilterRequest.setStatusList(Arrays.asList(WorkflowState.PASSED.name()));
    }
    return distributionTaskMultipleFilterRequest;
  }

  public static ProductListRequest toProductListRequest(SummaryFilterWebRequest summaryFilterWebRequest) {
    ProductListRequest productListRequest = new ProductListRequest();
    BeanUtils.copyProperties(summaryFilterWebRequest, productListRequest);
    productListRequest.setProductName(summaryFilterWebRequest.getKeyword());
    return productListRequest;
  }

  public static DistributionTaskMultipleFilterRequest toDistributionTaskMultipleFilterRequest(
      DistributionFilterWebRequest distributionFilterWebRequest) {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    BeanUtils.copyProperties(distributionFilterWebRequest, distributionTaskMultipleFilterRequest);
    distributionTaskMultipleFilterRequest.setProductName(distributionFilterWebRequest.getKeyword());
    return distributionTaskMultipleFilterRequest;
  }

  public static ProductDistributionTaskRequest toProductDistributionTaskRequest(
      ProductVendorWebRequest productVendorWebRequest) {
    ProductDistributionTaskRequest productDistributionTaskRequest = new ProductDistributionTaskRequest();
    BeanUtils.copyProperties(productVendorWebRequest, productDistributionTaskRequest);
    return productDistributionTaskRequest;
  }

  public static ProductImageQcFeedbackRequest toProductImageQcFeedbackRequest(
      ProductImageQcWebRequest productImageQcWebRequest, String pathPrefix, String gcsUrlPath) throws JsonProcessingException {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    UserFeedbackRequest userFeedbackRequest = new UserFeedbackRequest();
    ObjectMapper mapper = new ObjectMapper();
    productImageQcFeedbackRequest.setProductCode(productImageQcWebRequest.getProductCode());
    if (CollectionUtils.isNotEmpty(productImageQcWebRequest.getImageFeedback())) {
      for (ImageFeedbackWebRequest imageFeedbackWebRequest : productImageQcWebRequest.getImageFeedback()) {
        UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
        if (!CollectionUtils.isEqualCollection(imageFeedbackWebRequest.getSystemFeedback(),
            imageFeedbackWebRequest.getUserFeedback())) {
          productImageQcFeedbackRequest.setFeedbackUpdated(true);
        }
        if (imageFeedbackWebRequest.getLocationPath().contains(pathPrefix)) {
          userImageFeedbackRequest.setLocationPath(gcsUrlPath.concat(imageFeedbackWebRequest.getLocationPath()));
        } else {
          userImageFeedbackRequest.setLocationPath(
              ImageQcConstants.IMAGE_BASE_PATH.concat(imageFeedbackWebRequest.getLocationPath()));
        }
        userImageFeedbackRequest.setUserPrediction(imageFeedbackWebRequest.getUserFeedback());
        userFeedbackRequest.getUserFeedback().add(userImageFeedbackRequest);
      }
    } else {
      userFeedbackRequest.setUserFeedback(null);
    }
    if (Objects.nonNull(productImageQcWebRequest.getOtherModelFeedBack())) {
      userFeedbackRequest.setOtherModelFeedBack(productImageQcWebRequest.getOtherModelFeedBack().getUserFeedback());
    } else {
      userFeedbackRequest.setOtherModelFeedBack(null);
    }
    productImageQcFeedbackRequest.setUserFeedback(mapper.writeValueAsString(userFeedbackRequest));
    return productImageQcFeedbackRequest;
  }

  public static RejectProductRequest toRejectProductListRequest(RejectProductWebRequest rejectProductWebRequest) {
    RejectProductRequest rejectProductRequest = new RejectProductRequest();
    BeanUtils.copyProperties(rejectProductWebRequest, rejectProductRequest);
    return rejectProductRequest;
  }

  public static ProductCenterSummaryRequest toProductCenterSummaryRequest(
      ProductCenterSummaryWebRequest productCenterSummaryWebRequest) {
    ProductCenterSummaryRequest productCenterSummaryRequest = new ProductCenterSummaryRequest();
    BeanUtils.copyProperties(productCenterSummaryWebRequest, productCenterSummaryRequest);
    return productCenterSummaryRequest;
  }

  public static SalesCategoryMappingUpdateRequest toSalesCategoryMappingUpdateRequest (
      SalesCategoryMappingWebRequest request) {
    SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest = SalesCategoryMappingUpdateRequest.builder()
        .addedCategories(new ArrayList<>()).deletedCategories(new ArrayList<>()).build();
    for (SalesCategoryWebRequest addedCategory : request.getAddedCategories()){
      SalesCategoryUpdateRequest salesCategoryUpdateRequest = new SalesCategoryUpdateRequest();
      BeanUtils.copyProperties(addedCategory, salesCategoryUpdateRequest);
      salesCategoryMappingUpdateRequest.getAddedCategories().add(salesCategoryUpdateRequest);
    }
    for (SalesCategoryWebRequest deletedCategory : request.getDeletedCategories()){
      SalesCategoryUpdateRequest salesCategoryUpdateRequest = new SalesCategoryUpdateRequest();
      BeanUtils.copyProperties(deletedCategory, salesCategoryUpdateRequest);
      salesCategoryMappingUpdateRequest.getDeletedCategories().add(salesCategoryUpdateRequest);
    }
    return salesCategoryMappingUpdateRequest;
  }

  public static RecatProductSummaryRequest toRecatProductSummaryRequest(
      RecatProductSummaryWebRequest recatProductSummaryWebRequest) {
    RecatProductSummaryRequest recatProductSummaryRequest =
        new RecatProductSummaryRequest(recatProductSummaryWebRequest.getStatus(),
            recatProductSummaryWebRequest.getKeyword());
    if (Constants.BULK_PROCESS_STATE_IN_PROGRESS.equals(recatProductSummaryWebRequest.getStatus())) {
      recatProductSummaryRequest.setStatus(Constants.BULK_PROCESS_STATE_PENDING);
    }
    return recatProductSummaryRequest;
  }

  public static RecatProcessSummaryRequest toRecatProcessSummaryRequest(
      RecatProcessSummaryWebRequest recatProcessSummaryWebRequest) throws ParseException {
    RecatProcessSummaryRequest recatProcessSummaryRequest = new RecatProcessSummaryRequest();
    BeanUtils.copyProperties(recatProcessSummaryWebRequest, recatProcessSummaryRequest);
    if (StringUtils.isNotEmpty(recatProcessSummaryWebRequest.getRequestEndDate()) && StringUtils
        .isNotEmpty(recatProcessSummaryWebRequest.getRequestEndDate())) {
      recatProcessSummaryRequest.setRequestStartDate(
          format.parse(recatProcessSummaryWebRequest.getRequestStartDate() + START_OF_DAY));
      recatProcessSummaryRequest.setRequestEndDate(
          format.parse(recatProcessSummaryWebRequest.getRequestEndDate() + END_OF_DAY));
    }
    return recatProcessSummaryRequest;
  }

  public static void checkRecatAccessibility(String[] accessibilities) {
    if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_RECAT_ACCESSIBILITY)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public static void checkAccessibilityByProcessType(String processType) {
    switch (processType) {
      case STORE_COPY_PROCESS_TYPE: {
        if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_STORE_COPY_ACCESSIBILITY)) {
          throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
        }
        break;
      }
      case SALES_CATEGORY_UPDATE_PROCESS_TYPE: {
        if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY)) {
          throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
        }
        break;
      }
      case DELETE_BRAND_AUTHORISATION: {
        if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_UPDATE_BRAND_AUTHORISATION)) {
          throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
        }
        break;
      }
      case BULK_PRICE_UPDATE_PROCESS_TYPE: {
        if (!Arrays.asList(Credential.getAccessibilities())
            .contains(Constants.INTERNAL_BULK_PRICE_UPDATE_ACCESSIBILITY)) {
          throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
        }
      }
    }
  }

  public static void validateInternalUploadExcelType(String processType, String fileName) {
    switch (processType) {
      case STORE_COPY_PROCESS_TYPE:
      case SALES_CATEGORY_UPDATE_PROCESS_TYPE:
      case BULK_PRICE_UPDATE_PROCESS_TYPE:
      case DELETE_BRAND_AUTHORISATION: {
        validateUploadExcelTypeXLSX(fileName);
        break;
      }
      default:
        throw new IllegalStateException("Unexpected value: " + processType);
    }
  }

  public static BulkInternalProcessUploadRequest getUploadRequestByProcessType(String processType, String fileName,
      String sellerCode, String sellerName, String requestCode) {
    switch (processType) {
      case STORE_COPY_PROCESS_TYPE:
      case BULK_PRICE_UPDATE_PROCESS_TYPE: {
        return BulkInternalProcessUploadRequest.builder().internalProcessRequestCode(requestCode)
            .processType(processType).fileName(fileName).sellerCode(sellerCode)
            .sellerName(sellerName).build();
      }
      case SALES_CATEGORY_UPDATE_PROCESS_TYPE: {
        return BulkInternalProcessUploadRequest.builder().internalProcessRequestCode(requestCode)
            .processType(processType).fileName(fileName).sellerCode(Constants.USER_TYPE_INTERNAL)
            .sellerName(Constants.USER_TYPE_INTERNAL).build();
      }
      default:
        return null;
    }
  }

  private static void validateUploadExcelTypeXLSX(String fileName) {
    if (!FileType.XLSX.name().equalsIgnoreCase(fileName.substring(fileName.lastIndexOf(DOT) + 1))) {
      log.error("Input file is of invalid type, should be XLSX. File uploaded  - {}", fileName);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, ErrorMessages.XLSX_TYPE_ONLY);
    }
  }

  public static void checkUpdateImagePredictionAccessibility(String[] accessibilities) {
    if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public static ProductImagePredictionRequest toProductImagePredictionRequest(
      ProductImagePredictionWebRequest productImagePredictionWebRequest) {
    ProductImagePredictionRequest productImagePredictionRequest = new ProductImagePredictionRequest();
    BeanUtils.copyProperties(productImagePredictionWebRequest, productImagePredictionRequest);
    if (AUTO_NEED_REVISION.equals(productImagePredictionWebRequest.getRuleType())) {
      productImagePredictionRequest
          .setNeedRevisionConfidenceThreshold(productImagePredictionWebRequest.getRuleThreshold());
      productImagePredictionRequest.setNeedRevisionEnabled(productImagePredictionWebRequest.isRuleEnabled());
      productImagePredictionRequest.setForceReview(false);
    } else if (FORCE_REVIEW.equals(productImagePredictionWebRequest.getRuleType())) {
      productImagePredictionRequest.setConfidenceThreshold(productImagePredictionWebRequest.getRuleThreshold());
      productImagePredictionRequest.setForceReview(productImagePredictionWebRequest.isRuleEnabled());
      productImagePredictionRequest.setNeedRevisionEnabled(false);
    }
    return productImagePredictionRequest;
  }

  public static ProductImagePredictionAndCategoryMappingRequest toProductImagePredictionAndCategoryMappingRequest(
      ProductImagePredictionAndCategoryMappingWebRequest request) {
    ProductImagePredictionAndCategoryMappingRequest productImagePredictionAndCategoryMappingRequest =
        new ProductImagePredictionAndCategoryMappingRequest();
    BeanUtils.copyProperties(request, productImagePredictionAndCategoryMappingRequest, "categoryMappings");
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequestList = new ArrayList<>();
    GdnPreconditions.checkArgument(Objects.nonNull(request.getPredictionCategoryMappingWebRequestList()),
        ErrorMessages.PREDICTION_CATEGORY_MAPPING_WEB_REQUEST_LIST_NULL_ERROR);
    for (PredictionCategoryMappingWebRequest predictionCategoryMappingWebRequest : request.getPredictionCategoryMappingWebRequestList()) {
      PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
          new PredictionCategoryMappingUpdateRequest();
      predictionCategoryMappingUpdateRequest.setCategoryCode(predictionCategoryMappingWebRequest.getCategoryCode());
      predictionCategoryMappingUpdateRequest.setMarkForDelete(predictionCategoryMappingWebRequest.isMarkForDelete());
      predictionCategoryMappingUpdateRequestList.add(predictionCategoryMappingUpdateRequest);
    }
    productImagePredictionAndCategoryMappingRequest.setCategoryMappings(predictionCategoryMappingUpdateRequestList);
    return productImagePredictionAndCategoryMappingRequest;
  }

  public static BulkInternalProcessSummaryRequest toBulkInternalProcessSummaryRequest(
      BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest) throws Exception {
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest = new BulkInternalProcessSummaryRequest();
    BeanUtils.copyProperties(bulkInternalProcessSummaryWebRequest, bulkInternalProcessSummaryRequest);
    if (StringUtils.isEmpty(bulkInternalProcessSummaryRequest.getSortColumn())) {
      bulkInternalProcessSummaryRequest.setSortColumn(UPLOAD_DATE);
    }
    if (Objects.nonNull(bulkInternalProcessSummaryWebRequest.getStartDate()) && Objects
        .nonNull(bulkInternalProcessSummaryWebRequest.getEndDate())) {
      SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);
      String startDate = formatter.format(bulkInternalProcessSummaryWebRequest.getStartDate());
      bulkInternalProcessSummaryRequest.setStartDate(format.parse(startDate + START_OF_DAY));
      String endDate = formatter.format(bulkInternalProcessSummaryWebRequest.getEndDate());
      bulkInternalProcessSummaryRequest.setEndDate(format.parse(endDate + END_OF_DAY));
    }
    return bulkInternalProcessSummaryRequest;
  }

  public static void validateBrandAuthRequest(BrandAuthCreateWebRequest brandAuthCreateWebRequest, boolean isUpdate,
    int numberOfYears) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandAuthCreateWebRequest.getBrandCode()),
      ErrorMessages.EMPTY_BRAND_CODE_ERROR);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandAuthCreateWebRequest.getSellerCode()),
      ErrorMessages.EMPTY_SELLER_CODE_ERROR);
    if (!isUpdate) {
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(brandAuthCreateWebRequest.getBrandName()),
        ErrorMessages.EMPTY_BRAND_NAME_ERROR);
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(brandAuthCreateWebRequest.getAuthorisationStatus()),
        ErrorMessages.EMPTY_AUTH_STATUS_ERROR);
      if (Objects.isNull(brandAuthCreateWebRequest.getAuthStartDate())) {
        brandAuthCreateWebRequest.setAuthStartDate(new Date());
      }
      if (Objects.isNull(brandAuthCreateWebRequest.getAuthExpireDate())) {
        brandAuthCreateWebRequest.setAuthExpireDate(DateUtils.addYears(new Date(), numberOfYears));
      }
      if (brandAuthCreateWebRequest.getAuthExpireDate()
        .before(brandAuthCreateWebRequest.getAuthStartDate())) {
        throw new ConstraintViolationException(
          ErrorMessages.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE);
      }
      if (Optional.ofNullable(brandAuthCreateWebRequest.getDocumentLinks()).orElse(new ArrayList<>())
          .size() > MAX_FILE_ALLOWED) {
        throw new ConstraintViolationException(ErrorMessages.MAX_FILE_ALLOWED_CROSSED);
      }
    }
  }

  public static GenericStringListRequest toGenericStringListRequest(PredictionTypeListWebRequest request) {
    GenericStringListRequest genericStringListRequest = new GenericStringListRequest();
    genericStringListRequest.setStringList(request.getPredictionTypeList());
    return genericStringListRequest;
  }

  public static AutoApprovalTypeRequest toAutoApprovalTypeRequest(
      AutoApprovalEligibilityWebRequest autoApprovalEligibilityWebRequest) {
    return AutoApprovalTypeRequest.builder().storeId(autoApprovalEligibilityWebRequest.getStoreId())
        .productCode(autoApprovalEligibilityWebRequest.getProductCode())
        .categoryCode(autoApprovalEligibilityWebRequest.getCategoryCode())
        .reviewType(autoApprovalEligibilityWebRequest.getReviewType())
        .edited(autoApprovalEligibilityWebRequest.isEdited()).revised(autoApprovalEligibilityWebRequest.isRevised())
        .build();
  }

  public static FilterMarginsByOrderItemsRequest getMarginsByOrderItemRequest(String businessPartnercode,
      String categoryCode, boolean setDefaultOrderTypeForMargin, String defaultOrderTypeForMargin) {
    FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest = new FilterMarginsByOrderItemsRequest();
    OrderItem orderItemForExisting = new OrderItem();
    orderItemForExisting.setCategoryCode(categoryCode);
    orderItemForExisting.setOrderItemId(ORDER_ITEM_ID);
    orderItemForExisting.setTransactionDate(new Date());
    orderItemForExisting.setStoreCode(businessPartnercode);
    if (setDefaultOrderTypeForMargin) {
      orderItemForExisting.setOrderType(defaultOrderTypeForMargin);
    }
    filterMarginsByOrderItemsRequest.setMarginOrderItem(Collections.singletonList(orderItemForExisting));
    return filterMarginsByOrderItemsRequest;
  }

  public static ProductRetryStatusUpdate toProductRetryStatusUpdate(PDTStateUpdateWebRequest pdtStateUpdateWebRequest) {
    return ProductRetryStatusUpdate.builder().markForDelete(pdtStateUpdateWebRequest.getMarkForDelete())
        .edited(pdtStateUpdateWebRequest.getEdited()).reviewType(pdtStateUpdateWebRequest.getReviewType())
        .state(pdtStateUpdateWebRequest.getState()).revised(pdtStateUpdateWebRequest.getRevised()).build();
  }

  public static List<ItemViewConfigAndItemSkuRequest> toItemViewConfigAndItemSkuRequest(
      List<ItemViewConfigWebRequest> request) {
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    for (ItemViewConfigWebRequest itemViewConfigWebRequest : request) {
      ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
      itemViewConfigAndItemSkuRequest.setItemSku(itemViewConfigWebRequest.getItemSku());
      itemViewConfigAndItemSkuRequest.setPickupPointCode(itemViewConfigWebRequest.getPickupPointCode());
      itemViewConfigAndItemSkuRequest.setDiscoverable(itemViewConfigWebRequest.isDiscoverable());
      itemViewConfigAndItemSkuRequest.setBuyable(itemViewConfigWebRequest.isBuyable());
      itemViewConfigAndItemSkuRequest.setChannel(itemViewConfigWebRequest.getChannel());
      itemViewConfigAndItemSkuRequests.add(itemViewConfigAndItemSkuRequest);
    }
    return itemViewConfigAndItemSkuRequests;
  }

  public static VendorAutoAssignmentRequest toVendorAutoAssignmentRequest(
      VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest, String storeId,
      SequenceResponse sequenceResponse, String vendorCode) {
    VendorAutoAssignmentRequest vendorAutoAssignmentRequest = new VendorAutoAssignmentRequest();
    BeanUtils.copyProperties(vendorAutoConsignmentWebRequest, vendorAutoAssignmentRequest);
    vendorAutoAssignmentRequest.setAssigneeList(new ArrayList<>(vendorAutoConsignmentWebRequest.getAssigneeList()));
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest = new VendorAutoAssignmentFilterRequest();
    BeanUtils.copyProperties(vendorAutoConsignmentWebRequest.getVendorAutoAssignmentFilterWebRequest(),vendorAutoAssignmentFilterRequest);
    vendorAutoAssignmentRequest.setVendorAutoAssignmentFilterRequest(vendorAutoAssignmentFilterRequest);
    vendorAutoAssignmentRequest.setStoreId(storeId);
    vendorAutoAssignmentRequest.setVendorCode(vendorCode);
    vendorAutoAssignmentRequest.setInternalProcessRequestCode(new StringBuilder().append(Constants.BULK_INTERNAL_PROCESS_CODE_KEY).append(Constants.HYPHEN).append(
        StringUtils.leftPad(String.valueOf(sequenceResponse.getCounter()), Constants.PADDING_COUNT,
            Constants.PADDING_CONSTANT)).toString());
    return vendorAutoAssignmentRequest;
  }
  public static VendorDefaultFilterRequest toVendorDefaultFilterRequest(
      VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest) {
    VendorDefaultFilterRequest vendorDefaultFilterRequest = new VendorDefaultFilterRequest();
    BeanUtils.copyProperties(vendorAutoConsignmentWebRequest, vendorDefaultFilterRequest);
    vendorDefaultFilterRequest.setAssigneeList(new ArrayList<>(vendorDefaultFilterRequest.getAssigneeList()));
    return vendorDefaultFilterRequest;
  }

  public static BulkRestrictedKeywordUploadModel toBulkRestrictedKeywordUploadModel(String storeId, String fileName,
      String processType, String requestId, String username) {
    return BulkRestrictedKeywordUploadModel.builder().storeId(storeId).bulkProcessType(processType)
        .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).actionType(processType).requestId(requestId)
        .updatedBy(username).build();
  }

  public static DeleteProductRequest toDeleteProductRequest(String username,
    DeleteProductWebRequest deleteProductWebRequest) {
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    BeanUtils.copyProperties(deleteProductWebRequest,deleteProductRequest);
    deleteProductRequest.setUpdatedBy(username);
    return deleteProductRequest;
  }

  public static BigQueryFetchRequest toBigQueryFetchRequest(
      BigQueryFetchWebRequest bigQueryFetchWebRequest) {
    BigQueryFetchRequest bigQueryFetchRequest = new BigQueryFetchRequest();
    BeanUtils.copyProperties(bigQueryFetchWebRequest, bigQueryFetchRequest);
    return bigQueryFetchRequest;
  }

  public static BulkBrandAuthUploadModel toBulkBrandAuthUploadModel(String storeId, String fileName, String processType,
      String requestId, String username) {
    return BulkBrandAuthUploadModel.builder().storeId(storeId).bulkProcessType(processType)
        .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).requestId(requestId).createdBy(username)
        .build();
  }

  public static BulkReviewUploadModel toBulkReviewUploadModel(String storeId, String fileName,
    String processType, String requestId, String username, String vendorCode) {
    return BulkReviewUploadModel.builder().storeId(storeId).bulkProcessType(processType)
      .bulkProcessCode(UUID.randomUUID().toString()).filePath(fileName).requestId(requestId)
      .createdBy(username).vendorCode(vendorCode).build();
  }

  public static BulkBrandAuthDownloadRequest toBulkBrandAuthDownloadRequest(String username, BrandAuthWebRequest brandAuthWebRequest) {
    BulkBrandAuthDownloadRequest brandAuthDownloadRequest = new BulkBrandAuthDownloadRequest();
    BeanUtils.copyProperties(brandAuthWebRequest, brandAuthDownloadRequest);
    String requestId = UUID.randomUUID().toString();
    brandAuthDownloadRequest.setRequestId(requestId);
    brandAuthDownloadRequest.setDownloadType(DownloadType.ALL);
    brandAuthDownloadRequest.setFileType(FileType.XLSX);
    brandAuthDownloadRequest.setBulkProcessEntity(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD);
    brandAuthDownloadRequest.setEmailTo(username);
    brandAuthDownloadRequest.setFilename(requestId + DOT + FileType.XLSX.name());
    brandAuthDownloadRequest.setUsername(username);
    return brandAuthDownloadRequest;
  }

  public static MasterSkuReviewSelectedItemsDownloadRequest toMasterSkuReviewSelectedItemsDownloadRequest(
      String username, MasterSkuItemsDownloadWebRequest masterSkuItemsDownloadWebRequest, String fileName,
      String requestId) {
    MasterSkuReviewSelectedItemsDownloadRequest masterSkuReviewSelectedItemsDownloadRequest =
        new MasterSkuReviewSelectedItemsDownloadRequest();
    BeanUtils.copyProperties(masterSkuItemsDownloadWebRequest, masterSkuReviewSelectedItemsDownloadRequest);
    masterSkuReviewSelectedItemsDownloadRequest.setItemSkuList(masterSkuItemsDownloadWebRequest.getItemSkuList());
    masterSkuReviewSelectedItemsDownloadRequest.setRequestId(requestId);
    masterSkuReviewSelectedItemsDownloadRequest.setDownloadType(DownloadType.ALL);
    masterSkuReviewSelectedItemsDownloadRequest.setFileType(FileType.XLSX);
    masterSkuReviewSelectedItemsDownloadRequest.setBulkProcessEntity(
        BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD);
    masterSkuReviewSelectedItemsDownloadRequest.setEmailTo(username);
    masterSkuReviewSelectedItemsDownloadRequest.setFilename(fileName);
    masterSkuReviewSelectedItemsDownloadRequest.setUsername(username);
    return masterSkuReviewSelectedItemsDownloadRequest;
  }

  public static MasterSkuReviewAllItemsDownloadRequest toMasterSkuReviewAllItemsDownloadRequest(String username,
      MasterSkuItemsDownloadWebRequest masterSkuItemsDownloadWebRequest, String fileName, String requestId, int page,
      int size) {
    MasterSkuReviewAllItemsDownloadRequest masterSkuReviewAllItemsDownloadRequest =
        new MasterSkuReviewAllItemsDownloadRequest();
    BeanUtils.copyProperties(masterSkuItemsDownloadWebRequest, masterSkuReviewAllItemsDownloadRequest);
    masterSkuReviewAllItemsDownloadRequest.setPage(page);
    masterSkuReviewAllItemsDownloadRequest.setLimit(size);
    masterSkuReviewAllItemsDownloadRequest.setRequestId(requestId);
    masterSkuReviewAllItemsDownloadRequest.setDownloadType(DownloadType.ALL);
    masterSkuReviewAllItemsDownloadRequest.setFileType(FileType.XLSX);
    masterSkuReviewAllItemsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD);
    masterSkuReviewAllItemsDownloadRequest.setEmailTo(username);
    masterSkuReviewAllItemsDownloadRequest.setFilename(fileName);
    masterSkuReviewAllItemsDownloadRequest.setUsername(username);
    return masterSkuReviewAllItemsDownloadRequest;
  }

  public static HalalProductsFilterRequest toHalalProductsFilterRequest(
      HalalProductsFilterWebRequest halalProductsFilterWebRequest) {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    BeanUtils.copyProperties(halalProductsFilterWebRequest, halalProductsFilterRequest);
    return halalProductsFilterRequest;
  }

  public static MasterSkuInReviewDownloadRequest toMasterSkuInReviewDownloadRequest(
      MasterSkuInReviewDownloadWebRequest request, String username, String requestId, int page,
      int size) {
    String fileName = new StringBuilder().append(requestId).append(Constants.DOT)
        .append(FileType.XLSX.name().toLowerCase()).toString();
    return MasterSkuInReviewDownloadRequest.MasterSkuInReviewDownloadRequestBuilder().page(page)
        .size(size).assignedTo(request.getAssignedTo()).keyword(request.getKeyword())
        .clusterRequestList(toAnchorMappingRequest(request.getClusterRequestList()))
        .categoryCode(request.getCategoryCode()).endDate(request.getEndDate())
        .startDate(request.getStartDate()).downloadType(DownloadType.ALL).fileType(FileType.XLSX)
        .bulkProcessEntity(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD).directDownload(false)
        .filename(fileName).emailTo(username).username(username).language(Constants.LANGUAGE)
        .requestId(requestId).build();
  }

  public static List<AnchorMappingRequest> toAnchorMappingRequest(
      List<AnchorMappingWebRequest> requests) {
    return requests.stream().map(RequestHelper::toAnchorMappingModel).collect(Collectors.toList());
  }

  public static AnchorMappingRequest toAnchorMappingModel(AnchorMappingWebRequest request) {
    AnchorMappingRequest anchorMappingRequest = new AnchorMappingRequest();
    BeanUtils.copyProperties(request, anchorMappingRequest);
    return anchorMappingRequest;
  }

  public static void checkParameter(boolean expression, @Nullable Object errorMessage) {
    if (!expression) {
      throw new ConstraintViolationException(String.valueOf(errorMessage));
    }
  }

  public static SuspensionProductBulkActionsWebRequest toSuspensionProductBulkActionsWebRequest(String productSku,
      AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest) {
    SuspensionProductBulkActionsWebRequest suspensionProductBulkActionsWebRequest =
        new SuspensionProductBulkActionsWebRequest();
    suspensionProductBulkActionsWebRequest.setAction(autoApprovedProductsActionWebRequest.getAction());
    suspensionProductBulkActionsWebRequest.setNotes(autoApprovedProductsActionWebRequest.getNotes());
    suspensionProductBulkActionsWebRequest.setReason(autoApprovedProductsActionWebRequest.getReason());
    ProductSuspensionWebRequest productSuspensionWebRequest = new ProductSuspensionWebRequest();
    productSuspensionWebRequest.setProductSku(productSku);
    productSuspensionWebRequest.setMerchantCode(getMerchantCodeByProductSku(productSku));
    productSuspensionWebRequest.setProductName(autoApprovedProductsActionWebRequest.getProductName());
    suspensionProductBulkActionsWebRequest.setProducts(List.of(productSuspensionWebRequest));
    return suspensionProductBulkActionsWebRequest;
  }

  public static String getMerchantCodeByProductSku(String productSku) {
    return productSku.substring(0, productSku.indexOf(Constants.HYPHEN));
  }

  public static AutoApprovedAllProductsDownloadRequest toAutoApprovedProductsAllDownloadRequest(
      String username,
      AutoApprovedProductsDownloadWebRequest autoApprovedProductsDownloadWebRequest,
      String fileName, String requestId) {
    AutoApprovedAllProductsDownloadRequest autoApprovedAllProductsDownloadRequest =
        new AutoApprovedAllProductsDownloadRequest();
    BeanUtils.copyProperties(autoApprovedProductsDownloadWebRequest,
        autoApprovedAllProductsDownloadRequest);
    autoApprovedAllProductsDownloadRequest.setRequestId(requestId);
    autoApprovedAllProductsDownloadRequest.setDownloadType(DownloadType.ALL);
    autoApprovedAllProductsDownloadRequest.setFileType(FileType.XLSX);
    autoApprovedAllProductsDownloadRequest.setBulkProcessEntity(
        BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD);
    autoApprovedAllProductsDownloadRequest.setEmailTo(username);
    autoApprovedAllProductsDownloadRequest.setFilename(fileName);
    autoApprovedAllProductsDownloadRequest.setUsername(username);
    return autoApprovedAllProductsDownloadRequest;
  }

  public static AutoApprovedSelectedProductsDownloadRequest toAutoApprovedProductsSelectedDownloadRequest(
      String username,
      AutoApprovedProductsDownloadWebRequest autoApprovedProductsDownloadWebRequest,
      String fileName, String requestId) {
    AutoApprovedSelectedProductsDownloadRequest autoApprovedSelectedProductsDownloadRequest =
        new AutoApprovedSelectedProductsDownloadRequest();
    BeanUtils.copyProperties(autoApprovedProductsDownloadWebRequest,
        autoApprovedSelectedProductsDownloadRequest);
    autoApprovedSelectedProductsDownloadRequest.setRequestId(requestId);
    autoApprovedSelectedProductsDownloadRequest.setDownloadType(DownloadType.ALL);
    autoApprovedSelectedProductsDownloadRequest.setFileType(FileType.XLSX);
    autoApprovedSelectedProductsDownloadRequest.setBulkProcessEntity(
        BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD);
    autoApprovedSelectedProductsDownloadRequest.setEmailTo(username);
    autoApprovedSelectedProductsDownloadRequest.setFilename(fileName);
    autoApprovedSelectedProductsDownloadRequest.setUsername(username);
    return autoApprovedSelectedProductsDownloadRequest;
  }

  public static UploadAttributeImageRequest toUploadAttributeImageRequest(String imageFileName,
      byte[] bytes, String originalFileName) {
    String lowercaseFilename =
        imageFileName.replaceAll(Constants.SPECIAL_CHARS_REGEX, Constants.HYPHEN)
            .toLowerCase();
    return UploadAttributeImageRequest.builder().imageFileName(lowercaseFilename).bytes(bytes)
        .originalFileType(originalFileName.substring(
            originalFileName.lastIndexOf(Constants.DOT) + Constants.INDEX_COUNT)).build();
  }

  public static IPRProductsDownloadRequest toIPRProductsAllDownloadRequest(String username,
      IPRProductsDownloadWebRequest iprProductsDownloadWebRequest,
      String fileName, String requestId) {
    IPRProductsDownloadRequest iprProductsDownloadAllRequest =
        new IPRProductsDownloadRequest();
    BeanUtils.copyProperties(iprProductsDownloadWebRequest, iprProductsDownloadAllRequest);
    iprProductsDownloadAllRequest.setRequestId(requestId);
    iprProductsDownloadAllRequest.setDownloadType(DownloadType.ALL);
    iprProductsDownloadAllRequest.setFileType(FileType.XLSX);
    iprProductsDownloadAllRequest.setBulkProcessEntity(
        BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL);
    iprProductsDownloadAllRequest.setEmailTo(username);
    iprProductsDownloadAllRequest.setFilename(fileName);
    iprProductsDownloadAllRequest.setUsername(username);
    return iprProductsDownloadAllRequest;
  }

  public static IPRProductsDownloadRequest toIPRProductsSelectedDownloadRequest(String username,
      IPRProductsDownloadWebRequest iprProductsDownloadWebRequest,
      String fileName, String requestId) {
    IPRProductsDownloadRequest iprProductsDownloadSelectedRequest =
        new IPRProductsDownloadRequest();
    BeanUtils.copyProperties(iprProductsDownloadWebRequest, iprProductsDownloadSelectedRequest);
    iprProductsDownloadSelectedRequest.setRequestId(requestId);
    iprProductsDownloadSelectedRequest.setDownloadType(DownloadType.ALL);
    iprProductsDownloadSelectedRequest.setFileType(FileType.XLSX);
    iprProductsDownloadSelectedRequest.setBulkProcessEntity(
        BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_SELECTED);
    iprProductsDownloadSelectedRequest.setEmailTo(username);
    iprProductsDownloadSelectedRequest.setFilename(fileName);
    iprProductsDownloadSelectedRequest.setUsername(username);
    return iprProductsDownloadSelectedRequest;
  }

  public static void validateBrandAuthWipRequest(
      BrandAuthCreateWipRequest brandAuthCreateWipRequest, int numberOfYears) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandAuthCreateWipRequest.getBrandCode()),
        ErrorMessages.EMPTY_BRAND_CODE_ERROR);
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(brandAuthCreateWipRequest.getSellerCode()),
        ErrorMessages.EMPTY_SELLER_CODE_ERROR);
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(brandAuthCreateWipRequest.getIprRegistrationNumber()),
        ErrorMessages.EMPTY_IPR_REGISTRATION_NUMBER_ERROR);
    GdnPreconditions.checkArgument(brandAuthCreateWipRequest.getIprRegistrationNumber().length()
            <= Constants.MAX_LENGTH_IPR_REGISTRATION_NUMBER,
        EMPTY_IPR_REGISTRATION_NUMBER_EXCEEDED_MAX_LENGTH);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandAuthCreateWipRequest.getBrandName()),
        ErrorMessages.EMPTY_BRAND_NAME_ERROR);
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(brandAuthCreateWipRequest.getDocumentLinks()),
        EMPTY_DOCUMENT_ERROR);
    if (Objects.isNull(brandAuthCreateWipRequest.getAuthStartDate())) {
      brandAuthCreateWipRequest.setAuthStartDate(new Date());
    }
    if (Objects.isNull(brandAuthCreateWipRequest.getAuthExpireDate())) {
      brandAuthCreateWipRequest.setAuthExpireDate(DateUtils.addYears(new Date(), numberOfYears));
    }
    if (brandAuthCreateWipRequest.getAuthExpireDate()
        .before(brandAuthCreateWipRequest.getAuthStartDate())) {
      throw new ConstraintViolationException(
          ErrorMessages.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE);
    }
    if (Optional.ofNullable(brandAuthCreateWipRequest.getDocumentLinks()).orElse(new ArrayList<>())
        .size() > MAX_FILE_ALLOWED) {
      throw new ConstraintViolationException(ErrorMessages.MAX_FILE_ALLOWED_CROSSED);
    }
  }
}

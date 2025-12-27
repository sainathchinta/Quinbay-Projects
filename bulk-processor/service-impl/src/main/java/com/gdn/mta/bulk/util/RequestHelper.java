package com.gdn.mta.bulk.util;

import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2;
import static com.gdn.partners.bulk.util.BulkIPRProductsParameter.BRAND_REPORT;
import static com.gdn.partners.bulk.util.BulkIPRProductsParameter.BULK_IPR_ACTIONS_MAP;
import static com.gdn.partners.bulk.util.BulkIPRProductsParameter.BULK_IPR_SOURCE_MAP;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Month;

import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.request.InternalBrandUpdateNotes;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.BulkInternalProcessDataGenerationDTO;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.FbbFailedItems;
import com.gdn.mta.bulk.dto.GenericTemplateDataReadDTO;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.FbbStatusEventModel;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.models.AssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.AutoApprovedAssigneeRequest;
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BrandReport;
import com.gdn.mta.bulk.models.BulkInternalDataSplit;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.BundleRecipeRequest;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.SkuRebateUpdateRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.models.download.BrandAuthDownloadRequest;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.mta.bulk.models.download.BrandAuthorisationRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.DownloadItemsRequest;
import com.gdn.mta.bulk.models.download.MasterSkuReviewDownloadItemsRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssignAutoApprovedProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssigneeMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRebateRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkSkuLevelRebateRequestData;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.partners.bulk.util.BrandAuthorisationConstant;
import com.gdn.partners.bulk.util.BulkApprovalRejectionParameters;
import com.gdn.partners.bulk.util.BulkAssignAutoApprovedProductsParameters;
import com.gdn.partners.bulk.util.BulkIPRProductsParameter;
import com.gdn.partners.bulk.util.BulkMasterSkuUploadParameters;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import java.util.function.Function;
import java.util.stream.Stream;

import com.google.common.base.Splitter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.DormantSellerProductStatus;
import com.gdn.mta.bulk.DormantSellerStatus;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.models.BulkInternalProcessDataPayload;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.InstoreUpdateModel;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.models.InternalBulkUploadRequest;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.models.SalesCategoryUpdateRequest;
import com.gdn.mta.bulk.models.VendorBulkAssignmentRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.MasterDataBulkParameters;
import com.gdn.partners.bulk.util.SalesCategoryUpdateConstants;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.gdn.partners.bulk.util.VendorProductDataBulkParameters;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.google.common.collect.ImmutableMap;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;

@Slf4j
public class RequestHelper {

  private static final String REMOVE_TRAILING_QUOTATIONS = "^\"|\"$";
  private static final ObjectMapper objectMapper = new ObjectMapper();
  private static final int DEFAULT_YEAR = 1900;
  private static final int DEFAULT_MONTH = 01;
  private static final int DEFAULT_DAY = 01;
  private static final int NUMBER_OF_DAYS = 2;
  private static final String DEFAULT_COGS_VALUE = "0";
  private static final String DUPLICATE_ASSIGNEE_ERROR_MESSAGE = "Gagal melakukan tindakan ini "
    + "karena sudah ada data yang sama";
  private static final String REGULAR_PRICE_MUST_BE_DECIMAL = "Harga Normal harus desimal";
  private static final String CNC_STATUS_INVALID = "Cnc must be 0 or 1";
  private static final String INSTORE_INVALID = "Instore must be 0 or 1";
  private static final String STATUS_INVALID = "Invalid Status/pengiriman value";
  private static final String WAREHOUSE_STOCK_INVALID = "Invalid warehouse stock value";
  public static final String AND_SYMBOL = ", ";
  private static final Map<String, String> HEADER_BOOLEAN_VALUES =
      ImmutableMap.<String, String>builder().put(BulkParameters.CNC_STATUS_HEADER, CNC_STATUS_INVALID)
          .put(BulkParameters.IN_STORE_HEADER, INSTORE_INVALID).build();
  private static final List<Integer> POSSIBLE_STATUS_VALUES = Arrays.asList(0, 1, 2, 3);
  public static final String NO_AUTHORISATION_TO_UPDATE_THE_BRAND =
      "no authorisation to update the brand";

  private static FileStorageService fileStorageService;
  public static final Pattern ITEM_SKU_PATTERN = Pattern.compile("^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$");
  private static final String DATE_FORMAT = "dd/MM/yyyy";
  private static final int HOURS_IN_MILLIS = 1000 * 60 * 60 * 24;


  public static void setFileStorageService(FileStorageService fileStorageService) {
    RequestHelper.fileStorageService = fileStorageService;
  }

  RequestHelper() {
  }

  public static ProductCreationFailureDomainEventModel toProductCreationFailureDomainEventModel(
      ProductCreationRequest productRequest) {
    if(Objects.isNull(productRequest)) {
      return new ProductCreationFailureDomainEventModel();
    }
    ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel =
        ProductCreationFailureDomainEventModel.builder()
            .storeId(productRequest.getStoreId())
            .productCode(productRequest.getProductCode())
            .productName(productRequest.getName())
            .length(productRequest.getLength())
            .width(productRequest.getWidth())
            .height(productRequest.getHeight())
            .shippingWeight(productRequest.getShippingWeight())
            .brand(productRequest.getBrand())
            .brandCode(productRequest.getBrandCode())
            .brandApprovalStatus(productRequest.getBrandApprovalStatus())
            .uniqueSellingPoint(productRequest.getUniqueSellingPoint())
            .uom(productRequest.getUom())
            .url(productRequest.getUrl())
            .activated(productRequest.isActivated())
            .viewable(productRequest.isViewable())
            .promoSKU(productRequest.isPromoSKU())
            .isMarginExceed(productRequest.isMarginExceed())
            .forReview(productRequest.isForReview())
            .postLive(productRequest.isPostLive())
            .reviewPending(productRequest.isReviewPending())
            .createdMerchant(productRequest.getCreatedMerchant())
            .createdDate(productRequest.getCreatedDate())
            .createdBy(productRequest.getCreatedBy())
            .markForDelete(productRequest.isMarkForDelete())
            .productCategories(toProductCategoryDomainEventModel(productRequest.getProductCategories()))
            .productAttributes(toProductAttributeDomainEventModels(productRequest.getProductAttributes()))
            .images(toImageDomainEventModels(productRequest.getImages()))
            .productItems(toProductItemDomainEventModels(productRequest.getProductItems()))
            .build();
    if(Objects.nonNull(productRequest.getDescription())) {
      productCreationFailureDomainEventModel.setDescription(new String(productRequest.getDescription()));
    }
    return productCreationFailureDomainEventModel;
  }

  private static List<ProductCategoryDomainEventModel> toProductCategoryDomainEventModel(
      List<ProductCategoryRequest> productCategoryRequests) {
    List<ProductCategoryDomainEventModel> productCategories = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(productCategoryRequests)) {
      for (ProductCategoryRequest productCategoryRequest : productCategoryRequests) {
        ProductCategoryDomainEventModel
            productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
        if(Objects.nonNull(productCategoryRequest.getCategory())) {
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
    if(CollectionUtils.isNotEmpty(productAttributeRequests)) {
      for (ProductAttributeRequest productAttributeRequest : productAttributeRequests) {
        ProductAttributeDomainEventModel
            productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
        productAttributeDomainEventModel.setSequence(productAttributeRequest.getSequence());
        productAttributeDomainEventModel.setProductAttributeName(productAttributeRequest.getProductAttributeName());
        productAttributeDomainEventModel.setOwnByProductItem(productAttributeRequest.isOwnByProductItem());
        if(Objects.nonNull(productAttributeRequest.getAttribute())) {
          AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
          attributeDomainEventModel.setId(productAttributeRequest.getAttribute().getId());
          attributeDomainEventModel.setAttributeCode(productAttributeRequest.getAttribute().getAttributeCode());
          attributeDomainEventModel.setName(productAttributeRequest.getAttribute().getName());
          attributeDomainEventModel.setAttributeType(String.valueOf(productAttributeRequest.getAttribute()
              .getAttributeType()));
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
    if(CollectionUtils.isNotEmpty(productAttributeValueRequests)) {
      for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeValueRequests) {
        ProductAttributeValueDomainEventModel
            productAttributeValueDomainEventModel = new ProductAttributeValueDomainEventModel();
        productAttributeValueDomainEventModel.setDescriptiveAttributeValueType(String.valueOf(
            productAttributeValueRequest.getDescriptiveAttributeValueType()));
        productAttributeValueDomainEventModel.setDescriptiveAttributeValue(
            productAttributeValueRequest.getDescriptiveAttributeValue());
        if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
          AllowedAttributeValueDomainEventModel
              allowedAttributeValueDomainEventModel = new AllowedAttributeValueDomainEventModel();
          BeanUtils.copyProperties(productAttributeValueRequest.getAllowedAttributeValue(),
              allowedAttributeValueDomainEventModel);
          productAttributeValueDomainEventModel.setAllowedAttributeValue(
              allowedAttributeValueDomainEventModel);
        }
        if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
          PredefinedAllowedAttributeValueDomainEventModel
              predefinedAllowedAttributeValueDomainEventModel = new PredefinedAllowedAttributeValueDomainEventModel();
          BeanUtils.copyProperties(productAttributeValueRequest.getPredefinedAllowedAttributeValue(),
              predefinedAllowedAttributeValueDomainEventModel);
          productAttributeValueDomainEventModel.setPredefinedAllowedAttributeValue(
              predefinedAllowedAttributeValueDomainEventModel);
        }
        productAttributeValueDomainEventModels.add(productAttributeValueDomainEventModel);
      }
    }
    return productAttributeValueDomainEventModels;
  }

  private static List<ImageDomainEventModel> toImageDomainEventModels(List<Image> imageRequests) {
    List<ImageDomainEventModel> imageDomainEventModels = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(imageRequests)) {
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
    if(CollectionUtils.isNotEmpty(productItemRequests)) {
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
    if(CollectionUtils.isNotEmpty(productItemAttributeValueRequests)) {
      for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemAttributeValueRequests) {
        ProductItemAttributeValueDomainEventModel
            productItemAttributeValueDomainEventModel = new ProductItemAttributeValueDomainEventModel();
        productItemAttributeValueDomainEventModel.setValue(productItemAttributeValueRequest.getValue());
        if(Objects.nonNull(productItemAttributeValueRequest.getAttribute())) {
          AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
          attributeDomainEventModel.setId(productItemAttributeValueRequest.getAttribute().getId());
          attributeDomainEventModel.setAttributeCode(productItemAttributeValueRequest.getAttribute()
              .getAttributeCode());
          attributeDomainEventModel.setName(productItemAttributeValueRequest.getAttribute().getName());
          attributeDomainEventModel.setAttributeType(String.valueOf(
              productItemAttributeValueRequest.getAttribute().getAttributeType()));
          attributeDomainEventModel.setSkuValue(productItemAttributeValueRequest.getAttribute().isSkuValue());
          productItemAttributeValueDomainEventModel.setAttribute(attributeDomainEventModel);
        }
        productItemAttributeValueDomainEventModels.add(productItemAttributeValueDomainEventModel);
      }
    }
    return productItemAttributeValueDomainEventModels;
  }

  public static List<DormantSellerProduct> toDormantSellerActiveProductList(String storeId,
      List<ItemSummaryResponse> activeItemList, String dormantSellerEventId, String businessPartnerCode,
      String processType) {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    for (ItemSummaryResponse itemSummaryResponse : activeItemList) {
      DormantSellerProduct dormantSellerProduct = new DormantSellerProduct();
      dormantSellerProduct.setStoreId(storeId);
      dormantSellerProduct.setStatus(DormantSellerStatus.PENDING.name());
      dormantSellerProduct.setProductStatus(DormantSellerProductStatus.ACTIVE.name());
      dormantSellerProduct.setItemSku(itemSummaryResponse.getItemSku());
      dormantSellerProduct.setDormantSellerEventId(dormantSellerEventId);
      dormantSellerProduct.setBusinessPartnerCode(businessPartnerCode);
      dormantSellerProduct.setProcessType(processType);
      dormantSellerProductList.add(dormantSellerProduct);
    }
    return dormantSellerProductList;
  }

  public static List<DormantSellerProduct> toDormantSellerInProgressProductList(String storeId,
    List<InProgressProductResponse> inProgressProductResponses, String dormantSellerEventId,
    String businessPartnerCode, String processType) {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    for (InProgressProductResponse inProgressProductResponse : inProgressProductResponses) {
      DormantSellerProduct dormantSellerProduct = new DormantSellerProduct();
      dormantSellerProduct.setStoreId(storeId);
      dormantSellerProduct.setStatus(DormantSellerStatus.PENDING.name());
      dormantSellerProduct.setProductStatus(DormantSellerProductStatus.IN_PROGRESS.name());
      dormantSellerProduct.setItemSku(inProgressProductResponse.getProductSku());
      dormantSellerProduct.setDormantSellerEventId(dormantSellerEventId);
      dormantSellerProduct.setBusinessPartnerCode(businessPartnerCode);
      dormantSellerProduct.setProcessType(processType);
      dormantSellerProductList.add(dormantSellerProduct);
    }
    return dormantSellerProductList;
  }

  public static List<BulkInternalProcessData> generateBulkInternalProcessDataByProcessType(
      BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO) throws Exception {
    int brandAuthEndYear = bulkInternalProcessDataGenerationDTO.getBrandAuthEndYear();
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    Map<String, List<String>> reviewers = bulkInternalProcessDataGenerationDTO.getReviewers();
    BulkInternalProcess bulkInternalProcess = bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();
    List<Map<String, String>> internalProcessDataFromExcel =
        bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    if (BulkInternalProcessType.STORE_COPY.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          internalProcessDataFromExcel(internalProcessDataFromExcel, bulkInternalProcess, storeId, userName);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForSalesCategoryUpdate(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              userName);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForInternalBulkUpload(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              bulkInternalProcess.getUpdatedBy());
    } else if(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForVendorBulkAssignment(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              bulkInternalProcess.getUpdatedBy(), reviewers);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForRestrictedKeywordUpload(internalProcessDataFromExcel, bulkInternalProcess,
              storeId, userName);
    } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBrandAuthUpload(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              userName, brandAuthEndYear);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BULK_REJECTION.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBulkApproval(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              userName);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()
      .equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
        getBulkInternalProcessDataForBulkAssigneeMasterSkuReview(internalProcessDataFromExcel,
          bulkInternalProcess, storeId, userName);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBulkMasterSkuReview(internalProcessDataFromExcel, bulkInternalProcess, storeId,
              userName);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
        .equals(bulkInternalProcess.getProcessType())) {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkInternalProcess.getCreatedBy());
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBulkAssignAutoApprovedProducts(internalProcessDataFromExcel,
              bulkInternalProcess, storeId, bulkInternalProcess.getCreatedBy(), reviewers);
    }
    else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    }
    else if(BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType())){
      bulkInternalProcessDataList=  getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    }
    else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessDataList =
        getBulkInternalProcessDataForBulkPriceProductType(bulkInternalProcessDataGenerationDTO);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()
        .equals(bulkInternalProcess.getProcessType())) {
      Set<String> iprValidActions = bulkInternalProcessDataGenerationDTO.getIprValidActions();
      Set<String> iprSource = bulkInternalProcessDataGenerationDTO.getIprSource();
      Set<String> iprViolationTypes = bulkInternalProcessDataGenerationDTO.getIprViolationTypes();
      Set<String> iprReasons = bulkInternalProcessDataGenerationDTO.getIprReasons();
      Set<String> iprReviewers = new HashSet<>(bulkInternalProcessDataGenerationDTO.getReviewers()
          .get(BulkIPRProductsParameter.IPR_REVIEWERS));
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkInternalProcess.getCreatedBy());
      bulkInternalProcessDataList =
          getBulkInternalProcessDataForBulkAddReviewIPRProducts(internalProcessDataFromExcel,
              bulkInternalProcess, storeId, bulkInternalProcess.getCreatedBy(), iprValidActions,
              iprSource, iprViolationTypes, iprReasons, iprReviewers);
    }
    else if(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(bulkInternalProcess.getProcessType())){
      bulkInternalProcessDataList = getBulkInternalProcessDataForBulkSkuLevelRebate(bulkInternalProcessDataGenerationDTO);
    }
    else if(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())){
      bulkInternalProcessDataList = getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    }
    return bulkInternalProcessDataList;
  }

  static BulkInternalDataSplit separateUniqueAndDuplicate(List<Map<String, String>> datas) {
    List<Map<String, String>> uniqueList = new ArrayList<>();
    List<Map<String, String>> duplicateList = new ArrayList<>();
    Map<Map<String, String>, Integer> occurrences = new HashMap<>();
    for (Map<String, String> data : datas) {
      occurrences.put(removeRowNumber(data), occurrences.getOrDefault(removeRowNumber(data), 0) + 1);
    }

    for (Map<String, String> data : datas) {
      Map<String, String> dataWithoutRowNumber = new HashMap<>(removeRowNumber(data));
      if (occurrences.get(dataWithoutRowNumber) == 1) {
        uniqueList.add(data);
      } else {
        duplicateList.add(data);
      }
    }
    BulkInternalDataSplit bulkInternalDataSplit = new BulkInternalDataSplit();
    bulkInternalDataSplit.setDuplicateData(duplicateList);
    bulkInternalDataSplit.setUniqueData(uniqueList);
    return bulkInternalDataSplit;
  }

  private static Map<String,String> removeRowNumber(Map<String, String> data) {
    Map<String, String> dataWithoutRowNumber = new HashMap<>(data);
    dataWithoutRowNumber.remove(Constant.DATA_ROW_NUMBER);
    return dataWithoutRowNumber;
  }

  public static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkPriceRebate(
      BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO) throws JsonProcessingException {
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    BulkInternalProcess bulkInternalProcess = bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();

    List<Map<String, String>> internalProcessDataFromExcel =
        bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    if (internalProcessDataFromExcel.size() > bulkInternalProcessDataGenerationDTO.getBulkRebateMaxRows()) {
      updateInternalProcessStatusAndSize(bulkInternalProcess, new ArrayList<>(), userName);
      return new ArrayList<>();
    }
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    BulkInternalDataSplit dataSplit = separateUniqueAndDuplicate(internalProcessDataFromExcel);
    generateInternalProcessData(dataSplit.getDuplicateData(), storeId, userName, bulkInternalProcess,
        bulkInternalProcessDataList, Constant.DUPLICATE_ROW_REBATE_ERROR_MESSAGE);
    generateInternalProcessData(dataSplit.getUniqueData(), storeId, userName, bulkInternalProcess,
        bulkInternalProcessDataList, null);
    return bulkInternalProcessDataList;
  }

  public static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkSkuLevelRebate(
      BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO) throws JsonProcessingException {
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    BulkInternalProcess bulkInternalProcess = bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();
    List<Map<String, String>> internalProcessDataFromExcel =
        bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    if (internalProcessDataFromExcel.size() > bulkInternalProcessDataGenerationDTO.getBulkSkuLevelRebateMaxRows()) {
      updateInternalProcessStatusAndSize(bulkInternalProcess, new ArrayList<>(), userName);
      return new ArrayList<>();
    }
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Map<String, Integer> offlineItemIdToIndexMap = new HashMap<>();
    String offlineItemId;
    StringBuilder errorMessage;
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getSellerCode());
      errorMessage = new StringBuilder();
      if (!validateItemSkuWhenUpload(row.get(BulkParameters.SKU_LEVEL_REBATE_SKU_ID))) {
        errorMessage.append(BulkProcessValidationErrorMessages.INVALID_OR_BLANK_ITEM_SKU);
      }
      if (StringUtils.isEmpty(row.get(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE))) {
        errorMessage.append(BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN);
      }
      if (StringUtils.isEmpty(row.get(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE))) {
        errorMessage.append(BulkProcessValidationErrorMessages.BLANK_SKU_LEVEL_REBATE);
      }
      BulkSkuLevelRebateRequestData bulkSkuLevelRebateRequestData = BulkSkuLevelRebateRequestData.builder()
          .itemSku(row.get(BulkParameters.SKU_LEVEL_REBATE_SKU_ID))
          .pickupPointCode(row.get(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE))
          .rebate(row.get(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE))
          .excelRowNumber(Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)))
          .build();
      offlineItemId = bulkSkuLevelRebateRequestData.getItemSku() + Constant.HYPHEN
          + bulkSkuLevelRebateRequestData.getPickupPointCode();
      if (offlineItemIdToIndexMap.containsKey(offlineItemId)) {
        log.info("Duplicate offline item id found in sku level rebate bulk upload. OfflineItemId: {} ", offlineItemId);
        errorMessage.append(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR);
        BulkInternalProcessData duplicateBulkInternalProcessData =
            bulkInternalProcessDataList.get(offlineItemIdToIndexMap.get(offlineItemId));
        duplicateBulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        duplicateBulkInternalProcessData.setErrorMessage(duplicateBulkInternalProcessData.getErrorMessage()
            .concat(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR));
        duplicateBulkInternalProcessData.setNotes(duplicateBulkInternalProcessData.getNotes()
            .concat(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR));
      } else {
        offlineItemIdToIndexMap.put(offlineItemId, bulkInternalProcessDataList.size());
      }
      bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkSkuLevelRebateRequestData));
      if (StringUtils.isEmpty(errorMessage)) {
        bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      } else {
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      }
      bulkInternalProcessData.setNotes(errorMessage.toString());
      bulkInternalProcessData.setErrorMessage(errorMessage.toString());
      bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
      bulkInternalProcessDataList.add(bulkInternalProcessData);
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  public static List<BulkInternalProcessData> getBulkInternalProcessDataForNewBulkPriceUpdate(
      BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO) throws JsonProcessingException {
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    BulkInternalProcess bulkInternalProcess = bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();
    List<Map<String, String>> internalProcessDataFromExcel =
        bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    if (CollectionUtils.isEmpty(internalProcessDataFromExcel)) {
      bulkInternalProcessDataGenerationDTO.setNoRowsDetectedForPriceUpdate(true);
      updateInternalProcessStatusAndSize(bulkInternalProcess, new ArrayList<>(), userName);
      return new ArrayList<>();
    }
    else if (internalProcessDataFromExcel.size() > bulkInternalProcessDataGenerationDTO.getBulkPriceUpdateNewMaxRows()) {
      updateInternalProcessStatusAndSize(bulkInternalProcess, new ArrayList<>(), userName);
      return new ArrayList<>();
    }
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Map<String, Integer> offlineItemIdToIndexMap = new HashMap<>();
    String offlineItemId;
    StringBuilder errorMessage;
    String productSku;
    String merchantCode;
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getSellerCode());
      errorMessage = new StringBuilder();
      merchantCode = null;
      productSku = null;
      if (!validateItemSkuWhenUpload(row.get(BulkParameters.BULK_PRICE_UPDATE_SKU_ID))) {
        errorMessage.append(BulkProcessValidationErrorMessages.INVALID_OR_BLANK_ITEM_SKU);
      } else {
        productSku = getProductSkuFromItemSku(row.get(BulkParameters.BULK_PRICE_UPDATE_SKU_ID));
        merchantCode = getMerchantCodeFromItemSku(row.get(BulkParameters.BULK_PRICE_UPDATE_SKU_ID));
      }
      if (StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE))) {
        errorMessage.append(BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN);
      }
      if (StringUtils.isNotEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE))
          && StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID))) {
        errorMessage.append(BulkProcessValidationErrorMessages.BLANK_CAMPAIGN_ID);
      }
      BulkPriceUpdateRequestData bulkPriceUpdateRequestData = BulkPriceUpdateRequestData.builder()
          .itemSku(row.get(BulkParameters.BULK_PRICE_UPDATE_SKU_ID))
          .pickupPointCode(row.get(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE))
          .salesPrice(StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE)) ? null :
              row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE))
          .listPrice(StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE)) ? null :
              row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE))
          .campaignCode(StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID)) ? null :
              row.get(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID))
          .campaignPrice(StringUtils.isEmpty(row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE)) ? null :
              row.get(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE))
          .productSku(productSku)
          .sellerCode(merchantCode)
          .excelRowNumber(Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)))
          .build();
      offlineItemId = bulkPriceUpdateRequestData.getItemSku() + Constant.HYPHEN
          + bulkPriceUpdateRequestData.getPickupPointCode();
      if (offlineItemIdToIndexMap.containsKey(offlineItemId)) {
        log.info("Duplicate offline item id found in new price update. OfflineItemId: {} ", offlineItemId);
        errorMessage.append(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR);
        BulkInternalProcessData duplicateBulkInternalProcessData =
            bulkInternalProcessDataList.get(offlineItemIdToIndexMap.get(offlineItemId));
        duplicateBulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        duplicateBulkInternalProcessData.setErrorMessage(duplicateBulkInternalProcessData.getErrorMessage()
            .concat(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR));
        duplicateBulkInternalProcessData.setNotes(duplicateBulkInternalProcessData.getNotes()
            .concat(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR));
      } else {
        offlineItemIdToIndexMap.put(offlineItemId, bulkInternalProcessDataList.size());
      }
      bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkPriceUpdateRequestData));
      if (StringUtils.isEmpty(errorMessage)) {
        bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
        bulkInternalProcessData.setParentCode(bulkPriceUpdateRequestData.getProductSku());
        bulkInternalProcessData.setSellerCode(bulkPriceUpdateRequestData.getSellerCode());
      } else {
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      }
      bulkInternalProcessData.setNotes(errorMessage.toString());
      bulkInternalProcessData.setErrorMessage(errorMessage.toString());
      bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
      bulkInternalProcessDataList.add(bulkInternalProcessData);
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static String getProductSkuFromItemSku(String itemSku) {
    String[] str = itemSku.split(Constant.HYPHEN);
    return str[0] + Constant.HYPHEN + str[1] + Constant.HYPHEN + str[2];
  }

  private static String getMerchantCodeFromItemSku(String itemSku) {
    String[] str = itemSku.split(Constant.HYPHEN);
    return str[0] + Constant.HYPHEN + str[1];
  }

  private static void generateInternalProcessData(List<Map<String, String>> internalProcessDataFromExcel, String storeId, String userName,
      BulkInternalProcess bulkInternalProcess, List<BulkInternalProcessData> bulkInternalProcessDataList, String errorMessage)
      throws JsonProcessingException {
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStoreId(storeId);
      bulkInternalProcessData.setCreatedBy(userName);
      bulkInternalProcessData.setInternalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode());
      bulkInternalProcessData.setInternalProcessRequestId(bulkInternalProcess.getId());
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getSellerCode());
      if (Objects.isNull(errorMessage)) {
        validateAndGetBulkRebateInternalProcessData(row, Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)),
            bulkInternalProcessData);
      } else {
        validateAndGetBulkRebateInternalProcessData(row, Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)),
            bulkInternalProcessData);
        bulkInternalProcessData.setErrorMessage(errorMessage);
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      }
      bulkInternalProcessDataList.add(bulkInternalProcessData);
    }
  }

  private static void validateAndGetBulkRebateInternalProcessData(Map<String,String> row, int rowNumber, BulkInternalProcessData bulkInternalProcessData)
      throws JsonProcessingException {
    StringBuilder errorMessage = new StringBuilder();
    if (!isValidMonth(StringUtils.trimToEmpty(row.get(BulkParameters.MONTH)))) {
      errorMessage.append(BulkProcessValidationErrorMessages.INVALID_MONTH);
    }
    if (!isValidYear(StringUtils.trimToEmpty(row.get(BulkParameters.YEAR)))) {
      errorMessage.append(BulkProcessValidationErrorMessages.INVALID_YEAR);
    }

    if (!isValidProjectedRebate(StringUtils.trimToEmpty(row.get(BulkParameters.PROJECTED_REBATE)))) {
      errorMessage.append(BulkProcessValidationErrorMessages.INVALID_PROJECTED_REBATE);
    }

    BulkPriceRebateRequestData bulkPriceRebateRequestData = new BulkPriceRebateRequestData();
    bulkPriceRebateRequestData.setRowNumber(rowNumber);
    bulkPriceRebateRequestData.setStoreId(StringUtils.trimToEmpty(row.get(BulkParameters.STORE_ID)));
    bulkPriceRebateRequestData.setCategoryCode(StringUtils.isEmpty(row.get(BulkParameters.CATEGORY_CODE)) ?
        null :
        StringUtils.trimToEmpty(row.get(BulkParameters.CATEGORY_CODE)));
    bulkPriceRebateRequestData.setBrandName(StringUtils.isEmpty(row.get(BulkParameters.BRAND)) ?
        null :
        StringUtils.trimToEmpty(StringUtils.trimToEmpty(row.get(BulkParameters.BRAND))));
    bulkPriceRebateRequestData.setMonth(StringUtils.trimToEmpty(row.get(BulkParameters.MONTH)));
    bulkPriceRebateRequestData.setYear(StringUtils.trimToEmpty(row.get(BulkParameters.YEAR)));
    bulkPriceRebateRequestData.setMainCategoryCode(StringUtils.isEmpty(row.get(BulkParameters.MAIN_CATEGORY_CODE)) ?
        null :
        StringUtils.trimToEmpty(row.get(BulkParameters.MAIN_CATEGORY_CODE)));
    bulkPriceRebateRequestData.setProjectedRebate(StringUtils.trimToEmpty(row.get(BulkParameters.PROJECTED_REBATE)));

    bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkPriceRebateRequestData));
    if (StringUtils.isEmpty(errorMessage)) {
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    } else {
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
    bulkInternalProcessData.setNotes(errorMessage.toString());
    bulkInternalProcessData.setErrorMessage(errorMessage.toString());
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
  }

  private static boolean isValidMonth(String month) {
    try {
      Month.valueOf(month.toUpperCase());
    } catch (Exception e) {
      return false;
    }
    return true;
  }

  private static boolean isValidYear(String year) {
    try {
      Integer.parseInt(year);
    } catch (Exception exception) {
      return false;
    }
    return true;
  }

  private static boolean isValidProjectedRebate(String projectedRebate) {
    try {
      Double.parseDouble(projectedRebate);
    } catch (Exception exception) {
      return false;
    }
    return true;
  }


  public static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkPriceUpdate(
      BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO) throws JsonProcessingException {
    int excelRows = bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel().size();
    int bulkPriceUpdateMaxRows = bulkInternalProcessDataGenerationDTO.getBulkPriceUpdateMaxRows();
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    BulkInternalProcess bulkInternalProcess = bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,bulkInternalProcess.getCreatedBy());
    List<Map<String, String>> internalProcessDataFromExcel =
        bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    if (excelRows > bulkPriceUpdateMaxRows) {
      updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
      return new ArrayList<>();
    }
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(), bulkInternalProcess,
          bulkInternalProcessData);
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      try {
        BulkPriceUpdateRequestData bulkPriceUpdateRequestData =
            getDataValueAndValidateForBulkPriceUpdate(row, Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)));
        bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkPriceUpdateRequestData));
        bulkInternalProcessData.setParentCode(bulkPriceUpdateRequestData.getProductSku());
        validateDataForBulkPriceUpdate(bulkPriceUpdateRequestData.getSellerCode(),
            bulkPriceUpdateRequestData.getProductSku(), bulkPriceUpdateRequestData.getItemSku(),
            bulkPriceUpdateRequestData.getPickupPointCode(), bulkInternalProcessData);
        bulkInternalProcessData.setSellerCode(bulkPriceUpdateRequestData.getSellerCode());
      } catch (Exception e) {
        BulkPriceUpdateRequestData bulkPriceUpdateRequestData = new BulkPriceUpdateRequestData();
        bulkPriceUpdateRequestData.setExcelRowNumber(Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)));
        bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkPriceUpdateRequestData));
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        bulkInternalProcessData.setErrorMessage(
          e.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY));
      } finally {
        bulkInternalProcessDataList.add(bulkInternalProcessData);
      }
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkPriceProductType(
    BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO)
    throws JsonProcessingException {
    String storeId = bulkInternalProcessDataGenerationDTO.getStoreId();
    String userName = bulkInternalProcessDataGenerationDTO.getUserName();
    BulkInternalProcess bulkInternalProcess =
      bulkInternalProcessDataGenerationDTO.getBulkInternalProcess();
    List<Map<String, String>> internalProcessDataFromExcel =
      bulkInternalProcessDataGenerationDTO.getInternalProcessDataFromExcel();
    if (internalProcessDataFromExcel.size() > bulkInternalProcessDataGenerationDTO.getBulkProductTypeTaggingMaxRows()) {
      updateInternalProcessStatusAndSize(bulkInternalProcess, new ArrayList<>(), userName);
      return new ArrayList<>();
    }
    Map<String, Integer> l5XCountMap = new HashMap<>();
    for(Map<String, String> row : internalProcessDataFromExcel){
      String offlineItemId = getOfflineItemIDFromProductTaggingSheet(row);
      l5XCountMap.put(offlineItemId, l5XCountMap.getOrDefault(offlineItemId, 0) + 1);
    }
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      bulkInternalProcessData.setStoreId(storeId);
      bulkInternalProcessData.setCreatedBy(userName);
      bulkInternalProcessData.setInternalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode());
      bulkInternalProcessData.setInternalProcessRequestId(bulkInternalProcess.getId());
      validateAndGetBulkPRiceProductTypeTagging(row, Integer.parseInt(row.get(BulkParameters.ROW_NUMBER)), bulkInternalProcessData, l5XCountMap);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static String getOfflineItemIDFromProductTaggingSheet(Map<String, String> row) {
    String itemSku = row.get(BulkParameters.ITEM_SKU_PRODUCT_TAGGING);
    String pickupPointCode = row.get(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING);
    return itemSku.concat(Constant.HYPHEN).concat(pickupPointCode);
  }


  private static void validateAndGetBulkPRiceProductTypeTagging(Map<String, String> row, int i,
    BulkInternalProcessData bulkInternalProcessData, Map<String, Integer> l5XCountMap) throws JsonProcessingException {
    StringBuilder errorMessage = new StringBuilder();
    if (!validateItemSkuWhenUpload(row.get(BulkParameters.ITEM_SKU_PRODUCT_TAGGING))) {
      errorMessage.append(BulkProcessValidationErrorMessages.INVALID_OR_BLANK_ITEM_SKU);
    }
    if (!isValidDeleteTypeColumn(row.get(BulkParameters.DELETE_PRODUCT_TAGGING))) {
      errorMessage.append(BulkProcessValidationErrorMessages.INVALID_OR_BLANK_DELETE_TYPE_TAGGING);
    }
    if (StringUtils.isEmpty(row.get(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING))) {
      errorMessage.append(BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN);
    }
    if (StringUtils.isEmpty(row.get(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING))) {
      errorMessage.append(BulkProcessValidationErrorMessages.PRODUCT_TYPE_TAGGING_MUST_NOT_BE_BLANK_EN);
    }
    String offlineItemId = getOfflineItemIDFromProductTaggingSheet(row);
    if(!l5XCountMap.getOrDefault(offlineItemId, Constant.ZERO).equals(Constant.ONE)){
      log.info("Product tagging updated failed for duplicate L5 code : {} ", offlineItemId);
      errorMessage.append(BulkProcessValidationErrorMessages.DUPLICATE_ROW_PRODUCT_TAGGING_ERROR);
    }

    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequest = BulkPriceProductTypeTaggingRequest.builder()
      .productTypeTagging(row.get(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING))
      .deleteProductTypeTagging(row.get(BulkParameters.DELETE_PRODUCT_TAGGING)).id(bulkInternalProcessData.getId())
      .itemSku(row.get(BulkParameters.ITEM_SKU_PRODUCT_TAGGING))
      .pickupPointCode(row.get(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING)).excelRowNumber(i).build();

    bulkInternalProcessData.setData(objectMapper.writeValueAsString(priceProductTypeTaggingRequest));
    if (StringUtils.isEmpty(errorMessage)) {
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    } else {
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
    bulkInternalProcessData.setErrorMessage(errorMessage.toString());
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
  }

  private static boolean isValidDeleteTypeColumn(String deleteTagging) {
    if (StringUtils.isBlank(deleteTagging)) {
      return false;
    } else {
      return List.of(ExcelHeaderNames.YES.toUpperCase(), ExcelHeaderNames.NO.toUpperCase())
        .contains(deleteTagging.toUpperCase());
    }
  }

  public static boolean validateItemSkuWhenUpload(String itemSku) {
    if (StringUtils.isBlank(itemSku)) {
      return false;
    } else {
      return ITEM_SKU_PATTERN.matcher(itemSku).matches();
    }
  }

  private static BulkPriceUpdateRequestData getDataValueAndValidateForBulkPriceUpdate(Map<String, String> row, int i) {
    row = row.entrySet().stream()
        .collect(Collectors.toMap(Map.Entry::getKey, excelData -> StringUtils.trimToEmpty(excelData.getValue())));
    StringBuilder errorMessage = new StringBuilder();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    //Validate Stock
    if (StringUtils.isNotBlank(row.get(BulkParameters.STOCK_HEADER))) {
      BulkUpdateServiceUtil.validateDatasForStock(bulkUpdateErrorCounter, errorMessage, row, 0, true,
          row.get(BulkParameters.STOCK_HEADER));
    }

    //Validate PO Quota
    if (StringUtils.isNotBlank(row.get(BulkParameters.PO_QUOTA))) {
      BulkUpdateServiceUtil.validateDatasForPo_Quota(bulkUpdateErrorCounter, errorMessage, row, 0, true,
          row.get(BulkParameters.PO_QUOTA));
    }

    //Validate seller Sku
    if (BulkUpdateServiceUtil.isSellerSkuInvalid(row)) {
      BulkUpdateServiceUtil.validateDatasForSellerSku(bulkUpdateErrorCounter, errorMessage, row);
    }
    //Validate CNC header and Instore Header
    for (Map.Entry<String, String> booleanValuesMap : HEADER_BOOLEAN_VALUES.entrySet()) {
      String booleanHeader = booleanValuesMap.getKey();
      if (!row.containsKey(booleanHeader)) {
        continue;
      }
      boolean isValueInvalid = false;
      if (StringUtils.isNotBlank(row.get(booleanHeader)) && BulkUpdateServiceUtil.isBooleanHeaderInvalid(row,
          isValueInvalid, booleanHeader)) {
        errorMessage.append(booleanValuesMap.getValue()).append(AND_SYMBOL);
      }
    }
    //Validate Delivery Status
    Integer status = null;
    if (StringUtils.isNotBlank(row.get(BulkParameters.AMPHI_SKU_STATUS_NEW))) {
      try {
        status = (int) Double.parseDouble(row.get(BulkParameters.AMPHI_SKU_STATUS_NEW));
        if (!POSSIBLE_STATUS_VALUES.contains(status)) {
          errorMessage.append(STATUS_INVALID).append(AND_SYMBOL);
        }
      } catch (NumberFormatException e) {
        errorMessage.append(STATUS_INVALID).append(AND_SYMBOL);
      }
    }
    //Validate Warehouse Stock
    Integer warehouseStock = null;
    if (StringUtils.isNotBlank(row.get(BulkParameters.WAREHOUSE_STOCK_HEADER)) && !Constant.HYPHEN.equals(
        row.get(BulkParameters.WAREHOUSE_STOCK_HEADER))) {
      try {
        warehouseStock = Integer.parseInt(row.get(BulkParameters.WAREHOUSE_STOCK_HEADER));
      } catch (NumberFormatException e) {
        errorMessage.append(WAREHOUSE_STOCK_INVALID).append(AND_SYMBOL);
      }
    }
    if (StringUtils.isNotBlank(errorMessage)) {
      errorMessage.delete(errorMessage.length() - 2, errorMessage.length());
      errorMessage.append(Constant.PERIOD);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, errorMessage.toString());
    }
    return BulkPriceUpdateRequestData.builder().sellerCode(row.get(BulkParameters.SELLER_CODE))
        .productSku(row.get(BulkParameters.BLIBLI_PRODUCT_SKU))
        .parentProductName(row.get(BulkParameters.PARENT_PRODUCT_NAME)).itemSku(row.get(BulkParameters.BLIBLI_SKU))
        .pickupPointCode(row.get(BulkParameters.PICKUP_POINT_HEADER)).productName(row.get(BulkParameters.PRODUCT_NAME))
        .skuCode(row.get(BulkParameters.SKU_CODE_HEADER)).sellerSku(row.get(BulkParameters.SELLER_SKU)).listPrice(
            StringUtils.isEmpty(row.get(BulkParameters.PRICE_HEADER)) ? null : row.get(BulkParameters.PRICE_HEADER))
        .salesPrice(StringUtils.isEmpty(row.get(BulkParameters.SELLING_PRICE_HEADER)) ? null :
            row.get(BulkParameters.SELLING_PRICE_HEADER)).stock(StringUtils.isEmpty(
                row.get(BulkParameters.STOCK_HEADER)) ? null : Integer.parseInt(row.get(BulkParameters.STOCK_HEADER)))
        .status(status).delivery(StringUtils.isEmpty(row.get(BulkParameters.DELIVERY_STATUS_HEADER)) ?
            null :
            Integer.parseInt(row.get(BulkParameters.DELIVERY_STATUS_HEADER)))
        .cnc(StringUtils.isEmpty(row.get(BulkParameters.CNC_STATUS_HEADER)) ? null :
                Integer.parseInt(row.get(BulkParameters.CNC_STATUS_HEADER))).instore(
            StringUtils.isEmpty(row.get(BulkParameters.IN_STORE_HEADER)) ? null :
                Integer.parseInt(row.get(BulkParameters.IN_STORE_HEADER))).warehouseStock(warehouseStock)
        .excelRowNumber(i).build();
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBrandAuthUpload(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName, int brandAuthEndYear) throws ParseException, JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    int i = 1;
    Set<String> brandCodesAndSellerCodes = new HashSet<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      try {
        if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcess.getProcessType())) {
          BrandAuthAddRequestData brandAuthAddRequestData = getDataValueForBrandAuthAddUpload(row, i, brandAuthEndYear);
          bulkInternalProcessData.setData(objectMapper.writeValueAsString(brandAuthAddRequestData));
          validateDataForBrandAuth(bulkInternalProcessData, brandAuthAddRequestData.getBrandCode(),
              brandAuthAddRequestData.getSellerCode(), brandCodesAndSellerCodes);
        } else {
          BrandAuthDeleteRequestData brandAuthDeleteRequestData = getDataValueForBrandAuthDeleteUpload(row, i);
          bulkInternalProcessData.setData(objectMapper.writeValueAsString(brandAuthDeleteRequestData));
          validateDataForBrandAuth(bulkInternalProcessData, brandAuthDeleteRequestData.getBrandCode(),
              brandAuthDeleteRequestData.getSellerCode(), brandCodesAndSellerCodes);
        }
      } catch (Exception e) {
        BrandAuthAddRequestData brandAuthAddRequestData = new BrandAuthAddRequestData();
        brandAuthAddRequestData.setExcelRowNumber(i);
        bulkInternalProcessData.setData(objectMapper.writeValueAsString(brandAuthAddRequestData));
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.INVALID_DATE_FORMAT);
      } finally {
        bulkInternalProcessDataList.add(bulkInternalProcessData);
        i++;
      }
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkApproval(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    int i = 1;
    Set<String> productCodeSet = new HashSet<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      BulkApprovalRejectionRequestData bulkApprovalRejectionRequestData = new BulkApprovalRejectionRequestData();
      String productCode;
      if (BulkInternalProcessType.BULK_APPROVAL.name().equals(bulkInternalProcess.getProcessType())) {
        productCode = StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.PRODUCT_CODE));
        if (productCodeSet.contains(productCode)) {
          log.warn("Duplicate product code found!: {} ", productCode);
          continue;
        }
        productCodeSet.add(productCode);
        bulkApprovalRejectionRequestData = getDataValueForBulkApproval(row, i);
        bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkApprovalRejectionRequestData));
        bulkInternalProcessData.setNotes(productCode);
      } else {
        productCode =
          StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.PRODUCT_CODE));
        bulkApprovalRejectionRequestData = getDataValueForBulkRejection(row, i);
        bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulkApprovalRejectionRequestData));
        bulkInternalProcessData.setNotes(productCode);
      }
      validateDataForBulkApprovalAndRejection(bulkInternalProcessData, bulkApprovalRejectionRequestData);
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getCreatedBy());
      setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(), bulkInternalProcess,
          bulkInternalProcessData);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      i++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkMasterSkuReview(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Map<String, String> masterSkuAndItemSkuToActionMap = new HashMap<>();
    int rowNumber = 1;
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData = getBulKMasterSkuReviewRequestData(row, rowNumber);
      bulkInternalProcessData.setData(objectMapper.writeValueAsString(bulKMasterSkuReviewRequestData));
      validateDataForBulkMasterSkuReview(bulkInternalProcessData, bulKMasterSkuReviewRequestData,
          masterSkuAndItemSkuToActionMap);
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getCreatedBy());
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      rowNumber++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static void validateDataForBulkApprovalAndRejection(BulkInternalProcessData bulkInternalProcessData,
      BulkApprovalRejectionRequestData bulkApprovalRejectionRequestData) {
    if (StringUtils.isEmpty(bulkApprovalRejectionRequestData.getProductCode())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.PRODUCT_CODE_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
    if (bulkInternalProcessData.getProcessType().equals(BulkInternalProcessType.BULK_REJECTION.name())) {
      if (StringUtils.isEmpty(bulkApprovalRejectionRequestData.getReason())) {
        bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.REASON_EMPTY);
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      } else if (StringUtils.isEmpty(bulkApprovalRejectionRequestData.getComment())) {
        bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.COMMENT_EMPTY);
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      }
    }
  }

  private static BulkApprovalRejectionRequestData getDataValueForBulkApproval(Map<String, String> row,
      int rowNumber) {
    return BulkApprovalRejectionRequestData.builder()
        .productCode(StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.PRODUCT_CODE)))
        .reason(StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.APPROVAL_REASON))).excelRowNumber(rowNumber).build();
  }

  private static BulkMasterSkuReviewRequestData getBulKMasterSkuReviewRequestData(Map<String, String> row,
      int rowNumber) {
    return BulkMasterSkuReviewRequestData.builder()
        .firstMasterSku(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.ANCHOR_SKU)))
        .firstMasterSkuName(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.ANCHOR_SKU_NAME)))
        .secondMasterSku(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU)))
        .secondMasterSkuName(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU_NAME)))
        .action(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.REVIEW_ACTION))).excelRowNumber(rowNumber)
        .build();
  }

  private static BulkApprovalRejectionRequestData getDataValueForBulkRejection(Map<String, String> row,
      int rowNumber) {
    return BulkApprovalRejectionRequestData.builder()
        .productCode(StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.PRODUCT_CODE)))
        .reason(StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.REJECTION_REASON)))
        .comment(StringUtils.trimToEmpty(row.get(BulkApprovalRejectionParameters.COMMENTS_TO_SELLERS))).excelRowNumber(rowNumber).build();
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkAssigneeMasterSkuReview(
    List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess,
    String storeId, String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Set<String> anchorMappingPair = new HashSet<>();
    int rowNumber = 1;
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
        getDataValueForBulkAssignee(row, rowNumber);
      bulkInternalProcessData.setData(
        objectMapper.writeValueAsString(bulkAssigneeMasterSkuReviewRequestData));
      validateDataForBulkAssigneeMasterSkuReview(bulkInternalProcessData,
        bulkAssigneeMasterSkuReviewRequestData, anchorMappingPair);
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getCreatedBy());
      setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(),
        bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      rowNumber++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static BulkAssigneeMasterSkuReviewRequestData getDataValueForBulkAssignee(
    Map<String, String> row, int rowNumber) {
    return BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(
        StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.FIRST_MASTER_SKU)))
      .firstAnchorSkuName(
        StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.FIRST_MASTER_SKU_NAME)))
      .secondAnchorSku(
        StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU)))
      .secondAnchorSkuName(
        StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU_NAME)))
      .assignee(StringUtils.trimToEmpty(row.get(BulkMasterSkuUploadParameters.ASSIGNEE)))
      .rowNumber(rowNumber)
      .build();
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkAssignAutoApprovedProducts(
      List<Map<String, String>> internalProcessDataFromExcel,
      BulkInternalProcess bulkInternalProcess, String storeId, String userName,
      Map<String, List<String>> reviewers) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Set<String> productCodes = new HashSet<>();
    int rowNumber = 1;
    for (Map<String, String> rowEntry : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
          getDataForBulkAssignAutoApprovedProduct(rowEntry, rowNumber);
      bulkInternalProcessData.setData(
          objectMapper.writeValueAsString(bulkAssignAutoApprovedProductsRequestData));
      validateDataForBulkAssignAutoApprovedProducts(bulkInternalProcessData,
          bulkAssignAutoApprovedProductsRequestData, productCodes, new HashSet<>(
              reviewers.get(BulkAssignAutoApprovedProductsParameters.REVIEWERS)));
      setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(),
          bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      rowNumber++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForBulkAddReviewIPRProducts(
      List<Map<String, String>> internalProcessDataFromExcel,
      BulkInternalProcess bulkInternalProcess, String storeId, String userName,
      Set<String> iprValidActions, Set<String> iprSource, Set<String> iprViolations,
      Set<String> iprReasons, Set<String> iprReviewers)
      throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    Set<String> productSkus = new HashSet<>();
    int rowNumber = 1;
    for (Map<String, String> rowEntry : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
      BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
          getDataForBulkAddReviewIPRProduct(rowEntry, rowNumber);
      bulkInternalProcessData.setData(
          objectMapper.writeValueAsString(bulkAddReviewIPRProductsRequestData));
      validateDataForBulkAddReviewIPRProducts(bulkInternalProcessData,
          bulkAddReviewIPRProductsRequestData, productSkus, iprValidActions, iprSource,
          iprViolations, iprReasons, iprReviewers);
      setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(),
          bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      rowNumber++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    return bulkInternalProcessDataList;
  }

  private static BulkAddReviewIPRProductsRequestData getDataForBulkAddReviewIPRProduct(
      Map<String, String> rowEntry, int rowNumber) {
    return BulkAddReviewIPRProductsRequestData.builder()
        .productSku(StringUtils.trimToEmpty(rowEntry.get(BulkIPRProductsParameter.PRODUCT_SKU)))
        .productName(StringUtils.trimToEmpty(
            rowEntry.get(BulkIPRProductsParameter.PRODUCT_NAME)))
        .sellerNotes(rowEntry.get(BulkIPRProductsParameter.SELLER_NOTES))
        .reviewerNotes(rowEntry.get(BulkIPRProductsParameter.REVIEWER_NOTES))
        .action(rowEntry.get(BulkIPRProductsParameter.ACTION))
        .reasons(rowEntry.get(BulkIPRProductsParameter.REASON))
        .violationType(rowEntry.get(BulkIPRProductsParameter.VIOLATION_TYPE))
        .source(rowEntry.get(BulkIPRProductsParameter.SOURCE))
        .assignee(rowEntry.get(BulkIPRProductsParameter.ASSIGNEE))
        .reportDate(rowEntry.get(BulkIPRProductsParameter.REPORT_DATE))
        .reporter(rowEntry.get(BulkIPRProductsParameter.REPORTER))
        .reporterName(rowEntry.get(BulkIPRProductsParameter.REPORTER_NAME))
        .reporterPhone(rowEntry.get(BulkIPRProductsParameter.REPORTER_PHONE_NUMBER))
        .reporterEmail(rowEntry.get(BulkIPRProductsParameter.REPORTER_EMAIL))
        .reporterAddress(rowEntry.get(BulkIPRProductsParameter.REPORTER_ADDRESS))
        .reporterReason(rowEntry.get(BulkIPRProductsParameter.REPORTER_REASON))
        .excelRowNumber(rowNumber)
        .build();
  }

  private static BulkAssignAutoApprovedProductsRequestData getDataForBulkAssignAutoApprovedProduct(
      Map<String, String> rowEntry, int rowNumber) {
    return BulkAssignAutoApprovedProductsRequestData.builder()
        .productCode(StringUtils.trimToEmpty(rowEntry.get(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE)))
        .productName(StringUtils.trimToEmpty(
            rowEntry.containsKey(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME) ?
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME) :
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME_OPTIONAL)))
        .categoryName(StringUtils.trimToEmpty(
            rowEntry.containsKey(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME) ?
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME) :
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME_OPTIONAL)))
        .storeName(StringUtils.trimToEmpty(
            rowEntry.containsKey(BulkAssignAutoApprovedProductsParameters.STORE_NAME) ?
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.STORE_NAME) :
                rowEntry.get(BulkAssignAutoApprovedProductsParameters.STORE_NAME_OPTIONAL)))
        .assignee(StringUtils.trimToEmpty(rowEntry.get(BulkAssignAutoApprovedProductsParameters.ASSIGNEE)))
        .excelRowNumber(rowNumber)
        .build();
  }

  private static void validateDataForBulkAssignAutoApprovedProducts(
      BulkInternalProcessData bulkInternalProcessData,
      BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData,
      Set<String> productCodes, Set<String> reviewers) {
    if (StringUtils.isBlank(bulkAssignAutoApprovedProductsRequestData.getProductCode())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.PRODUCT_CODE_EMPTY);
      return;
    }
    if (productCodes.contains(bulkAssignAutoApprovedProductsRequestData.getProductCode())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.DUPLICATE_PRODUCT_CODE);
      return;
    }
    if (!reviewers.contains(bulkAssignAutoApprovedProductsRequestData.getAssignee())
        && StringUtils.isNotBlank(bulkAssignAutoApprovedProductsRequestData.getAssignee())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.INVALID_REVIEWER);
      return;
    }
    productCodes.add(bulkAssignAutoApprovedProductsRequestData.getProductCode());
  }

  private static void validateDataForBulkAddReviewIPRProducts(
      BulkInternalProcessData bulkInternalProcessData,
      BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData,
      Set<String> productSkus, Set<String> iprValidActions, Set<String> iprSource,
      Set<String> iprViolations, Set<String> iprReasons, Set<String> iprReviewers) {
    if (productSkus.contains(bulkAddReviewIPRProductsRequestData.getProductSku())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.DUPLICATE_PRODUCT_SKUS);
      return;
    }
    if (StringUtils.isBlank(bulkAddReviewIPRProductsRequestData.getAction())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.ACTION_MUST_NOT_BE_BLANK);
      return;
    }
    if (iprValidActions.stream()
        .noneMatch(bulkAddReviewIPRProductsRequestData.getAction()::equals)) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.INVALID_ACTION);
      return;
    }
    if (StringUtils.isNotBlank(bulkAddReviewIPRProductsRequestData.getSource())
        && iprSource.stream().noneMatch(bulkAddReviewIPRProductsRequestData.getSource()::equals)) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.UNKNOWN_SOURCE);
      return;
    }

    if (BRAND_REPORT.equals(bulkAddReviewIPRProductsRequestData.getSource()) && (
        StringUtils.isEmpty(bulkAddReviewIPRProductsRequestData.getReportDate()) || StringUtils.isEmpty(
            bulkAddReviewIPRProductsRequestData.getReporter()) || StringUtils.isEmpty(
            bulkAddReviewIPRProductsRequestData.getReporterName()) || StringUtils.isEmpty(
            bulkAddReviewIPRProductsRequestData.getReporterEmail()) || StringUtils.isEmpty(
            bulkAddReviewIPRProductsRequestData.getReporterReason()))) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(),
          BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK);
      return;
    }

    if (BRAND_REPORT.equals(bulkAddReviewIPRProductsRequestData.getSource())) {
      String reportDate = bulkAddReviewIPRProductsRequestData.getReportDate();
      if (isNotNumeric(reportDate)) {
        updateInternalProcessDataStatusAndErrorMessage(
            bulkInternalProcessData,
            ProcessStatus.FAILED.name(),
            BulkProcessValidationErrorMessages.DATE_NOT_IN_CORRECT_FORMAT
        );
        return;
      }
    }

    if (StringUtils.isNotBlank(bulkAddReviewIPRProductsRequestData.getViolationType())
        && iprViolations.stream()
        .noneMatch(bulkAddReviewIPRProductsRequestData.getViolationType()::equals)) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.UNKNOWN_VIOLATION_TYPE);
      return;
    }
    if (StringUtils.isNotBlank(bulkAddReviewIPRProductsRequestData.getReasons())
        && iprReasons.stream()
        .noneMatch(bulkAddReviewIPRProductsRequestData.getReasons()::equals)) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.UNKNOWN_REASONS);
      return;
    }
    if (StringUtils.isNotBlank(bulkAddReviewIPRProductsRequestData.getAssignee())
        && !iprReviewers.contains(bulkAddReviewIPRProductsRequestData.getAssignee())) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), BulkProcessValidationErrorMessages.INVALID_REVIEWER);
      return;
    }
    productSkus.add(bulkAddReviewIPRProductsRequestData.getProductSku());
  }

  public static Date convertExcelSerialToDate(String serial) {
    Calendar calendar = Calendar.getInstance();
    calendar.set(DEFAULT_YEAR, Calendar.JANUARY, DEFAULT_DAY);
    calendar.add(Calendar.DAY_OF_YEAR, Integer.parseInt(serial) - NUMBER_OF_DAYS);
    return calendar.getTime();
  }

  private static boolean isNotNumeric(String str) {
    try {
      Integer.parseInt(str);
      return false;
    } catch (NumberFormatException e) {
      return true;
    }
  }

  private static void validateDataForBulkAssigneeMasterSkuReview(
    BulkInternalProcessData bulkInternalProcessData,
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData,
    Set<String> anchorMappingPair) {
    if (StringUtils.isEmpty(bulkAssigneeMasterSkuReviewRequestData.getFirstAnchorSku())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.FIRST_MASTER_SKU_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    if (StringUtils.isEmpty(bulkAssigneeMasterSkuReviewRequestData.getSecondAnchorSku())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.SECOND_MASTER_SKU_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    String firstMasterSku = StringUtils.trimToEmpty(bulkAssigneeMasterSkuReviewRequestData.getFirstAnchorSku());
    String secondMasterSku =
      StringUtils.trimToEmpty(bulkAssigneeMasterSkuReviewRequestData.getSecondAnchorSku());
    String anchorMapping = firstMasterSku + GenericBulkParameters.SEPARATOR + secondMasterSku;
    if (anchorMappingPair.contains(anchorMapping)) {
      updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
        ProcessStatus.FAILED.name(), DUPLICATE_ASSIGNEE_ERROR_MESSAGE);
    }
    anchorMappingPair.add(anchorMapping);
  }

  private static void validateDataForBulkMasterSkuReview(BulkInternalProcessData bulkInternalProcessData,
      BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData,
      Map<String, String> masterSkuAndItemSkuToActionMap) {
    if (StringUtils.isBlank(bulKMasterSkuReviewRequestData.getAction())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.REVIEW_ACTION_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    if (!BulkMasterSkuUploadParameters.MASTER_SKU_REVIEW_ACTIONS_LIST.contains(
        bulKMasterSkuReviewRequestData.getAction())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.INVALID_REVIEW_ACTION);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    if (StringUtils.isBlank(bulKMasterSkuReviewRequestData.getFirstMasterSku())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.FIRST_MASTER_SKU_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    if (StringUtils.isBlank(bulKMasterSkuReviewRequestData.getSecondMasterSku())
        && !BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER.equals(bulKMasterSkuReviewRequestData.getAction())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.SECOND_MASTER_SKU_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    if (bulKMasterSkuReviewRequestData.getFirstMasterSku().equals(bulKMasterSkuReviewRequestData.getSecondMasterSku())
        && !BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER.equals(bulKMasterSkuReviewRequestData.getAction())) {
      bulkInternalProcessData.setErrorMessage(
          ProductUpdateErrorMessages.FIRST_ANCHOR_SKU_AND_SECOND_ANCHOR_SKU_IS_SAME);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    String key = new StringBuilder(bulKMasterSkuReviewRequestData.getFirstMasterSku()).append(Constant.HYPHEN)
        .append(bulKMasterSkuReviewRequestData.getSecondMasterSku()).toString();
    //To avoid duplicate entries having same action or same combination of master sku and item sku
    //having different action as we will consider the first action in Excel
    if (masterSkuAndItemSkuToActionMap.containsKey(key)) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.DUPLICATE_REVIEW_ACTION_FOUND);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else {
      if (bulKMasterSkuReviewRequestData.getAction().equals(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)) {
        bulkInternalProcessData.setParentCode(bulKMasterSkuReviewRequestData.getFirstMasterSku());
      } else {
        bulkInternalProcessData.setParentCode(bulKMasterSkuReviewRequestData.getSecondMasterSku());
      }
      masterSkuAndItemSkuToActionMap.put(key, bulKMasterSkuReviewRequestData.getAction());
    }
  }

  private static void validateDataForBrandAuth(BulkInternalProcessData bulkInternalProcessData, String brandCode,
      String sellerCode, Set<String> brandCodesAndSellerCodes) {
    if (StringUtils.isEmpty(sellerCode)) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.SELLER_CODE_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    } else if (StringUtils.isEmpty(brandCode)) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.BRAND_CODE_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      return;
    }
    String brandCodeAndSellerCode =
        new StringBuilder().append(brandCode).append(Constant.HYPHEN).append(sellerCode).toString();
    if (!brandCodesAndSellerCodes.add(brandCodeAndSellerCode)) {
      bulkInternalProcessData.setErrorMessage(
          ProductUpdateErrorMessages.DUPLICATE_BRAND_CODE_AND_SELLER_CODE);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
  }

  private static void validateDataForBulkPriceUpdate(String sellerCode, String productSku, String itemSku,
      String pickupPointCode, BulkInternalProcessData bulkInternalProcessData) {
    StringBuilder errorMessage = new StringBuilder();
    if (StringUtils.isEmpty(sellerCode)) {
      errorMessage.append(BulkProcessValidationErrorMessages.SELLER_CODE_MUST_NOT_BE_BLANK_EN);
    }
    if (StringUtils.isEmpty(productSku)) {
      errorMessage.append(BulkProcessValidationErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK_EN);
    }
    if (StringUtils.isEmpty(itemSku)) {
      errorMessage.append(BulkProcessValidationErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK_EN);
    }
    if (StringUtils.isEmpty(pickupPointCode)) {
      errorMessage.append(BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN);
    }
    if (StringUtils.isNotBlank(errorMessage)) {
      bulkInternalProcessData.setErrorMessage(errorMessage.toString());
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
  }

  private static BrandAuthDeleteRequestData getDataValueForBrandAuthDeleteUpload(Map<String, String> row,
      int excelRowNumber) {
    return BrandAuthDeleteRequestData.builder()
        .sellerCode(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE)))
        .brandCode(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.BRAND_AUTH_BRAND_CODE)))
        .excelRowNumber(excelRowNumber).build();
  }

  private static BrandAuthAddRequestData getDataValueForBrandAuthAddUpload(Map<String, String> row, int excelRowNumber,
      int brandAuthEndYear) throws ParseException {
    Date excelDefaultDate = new LocalDate(DEFAULT_YEAR, DEFAULT_MONTH, DEFAULT_DAY).toDate();
    String startDateValueTrimmed =
      StringUtils.trimToEmpty(row.get(ExcelHeaderNames.AUTH_START_DATE));
    String startDateValue = startDateValueTrimmed.contains(Constant.SLASH) ?
        parseDateInFormat(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.AUTH_START_DATE)),
          excelDefaultDate) : startDateValueTrimmed;

    String endDateValueTrimmed = StringUtils.trimToEmpty(row.get(ExcelHeaderNames.AUTH_END_DATE));
    String endDateValue = endDateValueTrimmed.contains(Constant.SLASH) ?
      parseDateInFormat(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.AUTH_END_DATE)),
        excelDefaultDate) : endDateValueTrimmed;
    Date startDate = StringUtils.isEmpty(startDateValue) ?
        new Date() :
        DateUtils.addDays(excelDefaultDate, Integer.parseInt(startDateValue) - NUMBER_OF_DAYS);
    //Subtracting 2 days - 01 Jan 1990 and Date mentioned in excel
    Date endDate = StringUtils.isEmpty(endDateValue) ?
        DateUtils.addYears(new Date(), brandAuthEndYear) :
        DateUtils.addDays(excelDefaultDate, Integer.parseInt(endDateValue) - NUMBER_OF_DAYS);
    if (StringUtils.isEmpty(endDateValue)) {
      log.warn("Authorization End date is empty, setting default end date as {}", endDate);
    }
    return BrandAuthAddRequestData.builder()
        .sellerCode(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE)))
        .brandCode(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.BRAND_AUTH_BRAND_CODE)))
        .brandName(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.BRAND_NAME))).authStartDate(startDate)
        .authExpireDate(endDate).documentLinks(null).excelRowNumber(excelRowNumber).build();
  }

  private static String parseDateInFormat(String date, Date excelDefaultDate)
    throws ParseException {
    //Calculating hour from Excel default date
    SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT);
    Date endDate = dateFormat.parse(date);
    long differenceInMillis = endDate.getTime() - excelDefaultDate.getTime();
    return Long.toString(differenceInMillis / HOURS_IN_MILLIS);
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForRestrictedKeywordUpload(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    int i = 1;
    for (Map<String, String> row : internalProcessDataFromExcel) {
      RestrictedKeywordRequestData restrictedKeywordRequestData =
          getDataValueForRestrictedKeywordUpload(row, i, bulkInternalProcess.getProcessType());
      BulkInternalProcessData bulkInternalProcessData = BulkInternalProcessData.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .status(ProcessStatus.PENDING.name()).data(objectMapper.writeValueAsString(restrictedKeywordRequestData))
          .internalProcessRequestId(bulkInternalProcess.getId()).processType(bulkInternalProcess.getProcessType())
          .build();
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(bulkInternalProcess.getProcessType())) {
        validateDataForRestrictedKeywordUpsert(row, restrictedKeywordRequestData, bulkInternalProcessData);
        bulkInternalProcessData.setNotes(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.APPLICABLE_FOR_ALL)));
      } else {
        validateDataForRestrictedKeywordDelete(row, restrictedKeywordRequestData, bulkInternalProcessData);
        bulkInternalProcessData.setNotes(
            StringUtils.trimToEmpty(row.get(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY)));
      }
      bulkInternalProcessDataList.add(bulkInternalProcessData);
      i++;
    }
    updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessDataList, userName);
    //Returning List of bulk internal process data based on number of rows of excel
    return bulkInternalProcessDataList;
  }

  public static void validateDataForRestrictedKeywordDelete(Map<String, String> row,
      RestrictedKeywordRequestData restrictedKeywordRequestData, BulkInternalProcessData bulkInternalProcessData) {
    if (StringUtils.isEmpty(restrictedKeywordRequestData.getKeyword())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.KEYWORD_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (StringUtils.isEmpty(row.get(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY))
        || !ExcelHeaderNames.APPLICABLE_FOR_ALL_LIST.contains(
        row.get(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY))) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.DELETE_FOR_ALL_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (ExcelHeaderNames.NO.equals(row.get(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY))
        && StringUtils.isBlank((row.get(ExcelHeaderNames.DELETE_KEYWORD_ACROSS_THESE_CATEGORY)))) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.DELETE_FOR_THESE_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
  }

  public static void validateDataForRestrictedKeywordUpsert(Map<String, String> rowData,
      RestrictedKeywordRequestData restrictedKeywordRequestData, BulkInternalProcessData bulkInternalProcessData) {
    if (StringUtils.isEmpty(restrictedKeywordRequestData.getKeyword())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.KEYWORD_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (StringUtils.isEmpty(restrictedKeywordRequestData.getKeywordType())
        || !ExcelHeaderNames.KEYWORD_TYPE_VALUES.contains(restrictedKeywordRequestData.getKeywordType())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.KEYWORD_TYPE_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (StringUtils.isEmpty(restrictedKeywordRequestData.getKeywordAction())
        || !ExcelHeaderNames.KEYWORD_ACTION_VALUES.contains(restrictedKeywordRequestData.getKeywordAction())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.KEYWORD_ACTION_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (ExcelHeaderNames.KEYWORD_REASON_ACTION_VALUES.contains(restrictedKeywordRequestData.getKeywordAction())
        && StringUtils.isBlank(restrictedKeywordRequestData.getMessage())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.REASON_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (ExcelHeaderNames.CHANGE_CATGEORY_TO.equals(restrictedKeywordRequestData.getKeywordAction())
        && StringUtils.isBlank(restrictedKeywordRequestData.getDestinationCategory())) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.DESTINATION_CATEGORY_EMPTY);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (StringUtils.isEmpty(rowData.get(ExcelHeaderNames.APPLICABLE_FOR_ALL)) || !ExcelHeaderNames.APPLICABLE_FOR_ALL_LIST.contains(rowData.get(ExcelHeaderNames.APPLICABLE_FOR_ALL))) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.APPLICABLE_FOR_ALL_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    } else if (ExcelHeaderNames.NO.equals(rowData.get(ExcelHeaderNames.APPLICABLE_FOR_ALL)) && StringUtils.isBlank(
        (rowData.get(ExcelHeaderNames.APPLICABLE_FOR_THESE)))) {
      bulkInternalProcessData.setErrorMessage(ProductUpdateErrorMessages.APPLICABLE_FOR_THESE_INVALID);
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
  }

  private static RestrictedKeywordRequestData getDataValueForRestrictedKeywordUpload(Map<String, String> row, int i,
      String processType) {
    Set<String> exclusionList = BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType) ?
        new HashSet<>(Arrays.asList(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.NOT_APPLICABLE)).split(",\\s*"))) :
        new HashSet<>(Arrays.asList(
            StringUtils.trimToEmpty(row.get(ExcelHeaderNames.DO_NOT_DELETE_ACROSS_THESE_CATEGORY)).split(",\\s*")));
    Set<String> applicableCategoryList = BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType) ?
        new HashSet<>(
            Arrays.asList(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.APPLICABLE_FOR_THESE)).split(",\\s*"))) :
        new HashSet<>(Arrays.asList(
            StringUtils.trimToEmpty(row.get(ExcelHeaderNames.DELETE_KEYWORD_ACROSS_THESE_CATEGORY)).split(",\\s*")));
    return RestrictedKeywordRequestData.builder().keyword(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.KEYWORD)))
        .keywordAction(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.KEYWORD_ACTION)))
        .keywordType(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.KEYWORD_TYPE)))
        .destinationCategory(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.CATEGORY_CHANGE_TO)))
        .message(StringUtils.trimToEmpty(row.get(ExcelHeaderNames.REASON))).exclusionList(exclusionList)
        .applicableCategoryList(applicableCategoryList)
        .excelRowNumber(i).build();
  }

  private static List<BulkInternalProcessData> internalProcessDataFromExcel(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws Exception {
    Set<String> failedBulkInternalProcessDatas = new HashSet<>();
    Map<String, List<BulkInternalProcessData>> bulkInternalProcessDataMap = new HashMap<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      BulkInternalProcessDataPayload bulkInternalProcessDataRequest = getDataValueForStoreCopy(row);
      if (StringUtils.isEmpty(bulkInternalProcessDataRequest.getPickupPointCode()) || StringUtils.isEmpty(
          bulkInternalProcessDataRequest.getShippingType())) {
        log.error("{} for internal-process-request-code {}", StoreCopyConstants.PICKUP_POINT_CODE_SHIPPING_TYPE_EMPTY,
            bulkInternalProcess.getInternalProcessRequestCode());
        bulkInternalProcessData.setErrorMessage(StoreCopyConstants.PICKUP_POINT_CODE_SHIPPING_TYPE_EMPTY);
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        failedBulkInternalProcessDatas.add(bulkInternalProcessDataRequest.getProductCode());
      } else {
        bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      }
      bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(bulkInternalProcessDataRequest));
      bulkInternalProcessData.setParentCode(bulkInternalProcessDataRequest.getProductCode());
      bulkInternalProcessData.setSellerCode(bulkInternalProcess.getSellerCode());
      bulkInternalProcessData.setProcessType(BulkInternalProcessType.STORE_COPY.name());
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
      if (bulkInternalProcessDataMap.containsKey(bulkInternalProcessDataRequest.getProductCode())) {
        bulkInternalProcessDataList = bulkInternalProcessDataMap.get(bulkInternalProcessDataRequest.getProductCode());
        bulkInternalProcessDataList.add(bulkInternalProcessData);
      } else {
        bulkInternalProcessDataList.add(bulkInternalProcessData);
      }
      bulkInternalProcessDataMap.put(bulkInternalProcessDataRequest.getProductCode(), bulkInternalProcessDataList);
    }
    if (CollectionUtils.isNotEmpty(failedBulkInternalProcessDatas)) {
      updateFailedStatusForMultiVaraint(failedBulkInternalProcessDatas, bulkInternalProcessDataMap);
    }
    return bulkInternalProcessDataMap.values().stream().flatMap(Collection::stream).collect(Collectors.toList());
  }

  private static void updateFailedStatusForMultiVaraint(Set<String> failedBulkInternalProcessDatas,
      Map<String, List<BulkInternalProcessData>> bulkInternalProcessDataMap) {
    for (String productCode : failedBulkInternalProcessDatas) {
      List<BulkInternalProcessData> failedBulkInternalProcessData = bulkInternalProcessDataMap.get(productCode);
      bulkInternalProcessDataMap.put(productCode, failedBulkInternalProcessData.stream()
          .map(bulkInternalProcessData -> setStatusFailedAndErrorMessage(bulkInternalProcessData))
          .collect(Collectors.toList()));
    }
  }

  private static BulkInternalProcessData setStatusFailedAndErrorMessage(
      BulkInternalProcessData bulkInternalProcessData) {
    if (StringUtils.isEmpty(bulkInternalProcessData.getErrorMessage())) {
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      bulkInternalProcessData.setErrorMessage(BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_MESSAGE_EN);
    }
    return bulkInternalProcessData;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForSalesCategoryUpdate(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDatas = new ArrayList<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
      String productSku = StringUtils.trimToEmpty(row.get(SalesCategoryUpdateConstants.PRODUCT_SKU));
      SalesCategoryUpdateRequest salesCategoryUpdateRequest = SalesCategoryUpdateRequest.builder()
          .operationType(StringUtils.trimToEmpty(row.get(SalesCategoryUpdateConstants.OPERATION_TYPE)))
          .cnCategoryCode(StringUtils.trimToEmpty(row.get(SalesCategoryUpdateConstants.CATEGORY_CODE))).build();
      if (StringUtils.isEmpty(productSku) || StringUtils.isEmpty(salesCategoryUpdateRequest.getOperationType())
          || StringUtils.isEmpty(salesCategoryUpdateRequest.getCnCategoryCode())) {
        log.error("{} for internal-process-request-code {}",
            SalesCategoryUpdateConstants.PRODUCT_SKU_OPERATION_TYPE_CATGEORY_CODE_EMPTY,
            bulkInternalProcess.getInternalProcessRequestCode());
        bulkInternalProcessData
            .setErrorMessage(SalesCategoryUpdateConstants.PRODUCT_SKU_OPERATION_TYPE_CATGEORY_CODE_EMPTY);
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      } else {
        bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      }
      bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(salesCategoryUpdateRequest));
      bulkInternalProcessData.setParentCode(productSku);
      bulkInternalProcessData.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDatas.add(bulkInternalProcessData);
    }
    return bulkInternalProcessDatas;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForInternalBulkUpload(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDatas = new ArrayList<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData = validateRowAndFormRequest(row);
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDatas.add(bulkInternalProcessData);
    }
    return bulkInternalProcessDatas;
  }

  private static List<BulkInternalProcessData> getBulkInternalProcessDataForVendorBulkAssignment(
      List<Map<String, String>> internalProcessDataFromExcel, BulkInternalProcess bulkInternalProcess, String storeId,
      String userName, Map<String, List<String>> reviewers) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDatas = new ArrayList<>();
    for (Map<String, String> row : internalProcessDataFromExcel) {
      BulkInternalProcessData bulkInternalProcessData =
          validateRowAndFormRequestForVendorBulkAssignment(row, reviewers, bulkInternalProcess, userName);
      setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess, bulkInternalProcessData);
      bulkInternalProcessDatas.add(bulkInternalProcessData);
    }
    return bulkInternalProcessDatas;
  }

  public static BulkInternalProcessData validateRowAndFormRequestForVendorBulkAssignment(Map<String, String> row,
      Map<String, List<String>> reviewers, BulkInternalProcess bulkInternalProcess, String assignedBy)
      throws JsonProcessingException {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    VendorBulkAssignmentRequest vendorBulkAssignmentRequest = new VendorBulkAssignmentRequest();
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    StringBuilder errorMessage = new StringBuilder();
    vendorBulkAssignmentRequest.setRowNumber(Integer.parseInt(row.get(VendorProductDataBulkParameters.ROW_NUMBER)));
    if (!validateProductCode(row)) {
      errorMessage.append(Constant.ERROR_IN_PRODUCT_CODE);
    } else {
      vendorBulkAssignmentRequest.setProductCode(row.get(VendorProductDataBulkParameters.PRODUCT_CODE));
    }
    if (!validateAssignee(row, reviewers.get(VendorProductDataBulkParameters.REVIEWERS))) {
      errorMessage.append(Constant.ERROR_IN_ASSIGNEE);
    } else {
      vendorBulkAssignmentRequest.setAssignedTo(row.get(VendorProductDataBulkParameters.ASSIGNEE));
    }
    if (StringUtils.isBlank(errorMessage.toString())) {
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
      vendorBulkAssignmentRequest.setAssignedBy(assignedBy);
    } else {
      bulkInternalProcessData.setErrorMessage(errorMessage.toString());
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
    bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(vendorBulkAssignmentRequest));
    return bulkInternalProcessData;
  }


  public static void setBulkInternalProcessDataBasicDetails(String storeId, String userName,
      BulkInternalProcess bulkInternalProcess, BulkInternalProcessData bulkInternalProcessData) {
    bulkInternalProcessData.setInternalProcessRequestId(bulkInternalProcess.getId());
    bulkInternalProcessData.setInternalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode());
    bulkInternalProcessData.setStoreId(storeId);
    bulkInternalProcessData.setCreatedBy(userName);
    bulkInternalProcessData.setCreatedDate(new Date());
    bulkInternalProcessData.setUpdatedBy(userName);
    bulkInternalProcessData.setUpdatedDate(new Date());
  }

  public static void updateInternalProcessStatusAndSize(BulkInternalProcess bulkInternalProcess,
      List<BulkInternalProcessData> bulkInternalProcessDataList, String userName) {
    bulkInternalProcess.setStartTime(new Date());
    bulkInternalProcess.setTotalCount(bulkInternalProcessDataList.size());
    bulkInternalProcess.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcess.setUpdatedDate(new Date());
    bulkInternalProcess.setUpdatedBy(userName);
  }

  public static void updateInternalProcessForVendorAutoAssignment(
    BulkInternalProcess bulkInternalProcess,
    List<BulkInternalProcessData> bulkInternalProcessDataList, String userName, int errorCount){
    bulkInternalProcess.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcess.setTotalCount(bulkInternalProcess.getTotalCount());
    bulkInternalProcess.setSuccessCount(bulkInternalProcessDataList.size());
    bulkInternalProcess.setErrorCount(errorCount);
    bulkInternalProcess.setUpdatedBy(userName);
    bulkInternalProcess.setUpdatedDate(new Date());
  }

  public static BulkInternalProcess getBulkInternalProcess(String storeId,
      BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest) {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
        .internalProcessRequestCode(bulkInternalProcessUploadRequest.getInternalProcessRequestCode())
        .processType(bulkInternalProcessUploadRequest.getProcessType())
        .fileName(bulkInternalProcessUploadRequest.getFileName())
        .sellerCode(bulkInternalProcessUploadRequest.getSellerCode())
        .sellerName(bulkInternalProcessUploadRequest.getSellerName())
        .errorCount(0).totalCount(0).successCount(0)
        .status(ProcessStatus.PENDING.name())
        .build();
    bulkInternalProcess.setStoreId(storeId);
    return bulkInternalProcess;
  }

  public static BulkInternalProcess getBulkInternalProcessFromMasterDataBulkUpdateRequest(String storeId,
      MasterDataBulkUpdateRequest masterDataBulkUpdateRequest) {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
        .internalProcessRequestCode(masterDataBulkUpdateRequest.getBipRequestCode())
        .processType(Constant.INTERNAL_BULK_UPLOAD)
        .fileName(masterDataBulkUpdateRequest.getFilePath())
        .sellerCode(Constant.INTERNAL)
        .sellerName(Constant.INTERNAL)
        .errorCount(0).totalCount(0).successCount(0)
        .status(ProcessStatus.PENDING.name())
        .build();
    bulkInternalProcess.setStoreId(storeId);
    return bulkInternalProcess;
  }

  public static BulkInternalProcess getBulkInternalProcessFromBulkVendorProductAssignRequest(String storeId,
      BulkVendorProductAssignRequest bulkVendorProductAssignRequest) {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
        .internalProcessRequestCode(bulkVendorProductAssignRequest.getInternalProcessRequestCode())
        .processType(VendorProductDataBulkParameters.VENDOR_BULK_ASSIGNMENT)
        .fileName(bulkVendorProductAssignRequest.getFilePath())
        .sellerCode(bulkVendorProductAssignRequest.getVendorCode())
        .sellerName(bulkVendorProductAssignRequest.getVendorCode())
        .errorCount(0)
        .totalCount(0)
        .successCount(0)
        .status(ProcessStatus.PENDING.name())
        .notes(VendorProductDataBulkParameters.ASSIGN).build();
    bulkInternalProcess.setStoreId(storeId);
    return bulkInternalProcess;
  }

  public static Map<String, Object> getEmailParams(BulkInternalProcess bulkInternalProcess) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(bulkInternalProcess.getCreatedBy()));
    emailParameters.put(EmailConstants.REQUEST_CODE, bulkInternalProcess.getInternalProcessRequestCode());
    emailParameters.put(EmailConstants.TOTAL_COUNT, bulkInternalProcess.getTotalCount());
    emailParameters.put(EmailConstants.SUCCESS_COUNT, bulkInternalProcess.getSuccessCount());
    emailParameters.put(EmailConstants.ERROR_FILE_PATH, bulkInternalProcess.getErrorFilePath());
    return emailParameters;
  }

  private static String getUserName(String email) {
    return email.split("@")[0];
  }

  private static BulkInternalProcessDataPayload getDataValueForStoreCopy(Map<String, String> internalProcessData) {
    return BulkInternalProcessDataPayload.builder()
        .productCode(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.PRODUCT_CODE)))
        .productName(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.PARENT_PRODUCT_NAME)))
        .productSku(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.BLIBLI_PRODUCT_SKU)))
        .itemSku(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.BLIBLI_SKU)))
        .itemCode(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.SKU_CODE)))
        .copyProductName(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.COPIED_PRODUCT_NAME)))
        .sellerSku(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.SELLER_SKU)))
        .listPrice(StringUtils.isEmpty(internalProcessData.get(StoreCopyConstants.HATGA)) ? null :
            Double.parseDouble(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.HATGA))))
        .offerPrice(StringUtils.isEmpty(internalProcessData.get(StoreCopyConstants.HARGA_PENJUALAN))? null :
            Double.parseDouble(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.HARGA_PENJUALAN))))
        .stock(StringUtils.isEmpty(internalProcessData.get(StoreCopyConstants.STOK)) ? null :
            Integer.parseInt(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.STOK))))
        .shippingType(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.SHIPPING_TYPE))).minimumStock(
            StringUtils.isEmpty(internalProcessData.get(StoreCopyConstants.MINIMUM_STOCK))? null:
            Integer.parseInt(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.MINIMUM_STOCK))))
        .pickupPointCode(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.TOKO_GUDANG)))
        .status(StringUtils.isEmpty(internalProcessData.get(StoreCopyConstants.SKU_STATUS))? null :
            Integer.parseInt(StringUtils.trimToEmpty(internalProcessData.get(StoreCopyConstants.SKU_STATUS))))
        .build();
  }

  public static boolean isExcelHeaderValid(List<Map<String, String>> internalProcessDataFromExcel,
      BulkInternalProcess bulkInternalProcess) {
    if (CollectionUtils.isEmpty(internalProcessDataFromExcel)) {
      bulkInternalProcess.setNotes(StoreCopyConstants.FILE_IS_EMPTY);
      return true;
    } else if (validateHeaderSizeByProcessType(internalProcessDataFromExcel, bulkInternalProcess)) {
      bulkInternalProcess.setNotes(StoreCopyConstants.HEADER_SIZE_VALIDATION_FAILED);
      log.error("Header Size Validation failed, bulkRequestCode : {} ", bulkInternalProcess.getInternalProcessRequestCode());
      return true;
    } else if (validateHeaderByProcessType(internalProcessDataFromExcel, bulkInternalProcess)) {
      bulkInternalProcess.setNotes(StoreCopyConstants.HEADER_VALIDATION_FAILED);
      log.error("Header Validation failed, bulkRequestCode : {} ", bulkInternalProcess.getInternalProcessRequestCode());
      return true;
    }
    return false;
  }

  private static boolean validateHeaderSizeByProcessType(List<Map<String, String>> internalProcessDataFromExcel,
      BulkInternalProcess bulkInternalProcess) {
    Map<String, String> headerRow = internalProcessDataFromExcel.get(0);
    if (BulkInternalProcessType.STORE_COPY.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size() != StoreCopyConstants.TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size() != SalesCategoryUpdateConstants.TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size() != BrandAuthorisationConstant.TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(bulkInternalProcess.getProcessType())) {
      return false;
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
      return false;
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != ExcelHeaderNames.RESTRICTED_KEYWORD_UPSERT_TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != ExcelHeaderNames.RESTRICTED_KEYWORD_DELETE_TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != ExcelHeaderNames.BRAND_AUTH_DELETE_TEMPLATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkApprovalRejectionParameters.BULK_APPROVAL.size() + 1;
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkApprovalRejectionParameters.BULK_REJECTION.size() + 1;
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()
      .equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
        != BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE.size() + 1;
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkMasterSkuUploadParameters.MASTER_SKU_BULK_REVIEW_HEADER_LIST.size() + 1;
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
        .equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size() !=
          BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_HEADERS.size() + 1;
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkParameters.BULK_INTERNAL_PRICE_UPDATE_HEADER.size() + 1;
    } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkParameters.BULK_REBATE_UPLOAD_HEADERS.size() + 1;
    }
    else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
        != BulkParameters.BULK_PRICING_PRODUCT_TYPE_UPLOAD_HEADERS.size() + 1;
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()
        .equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
          != BulkIPRProductsParameter.BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS.size() + 1;
    }
    else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
        != BulkParameters.BULK_SKU_LEVEL_REBATE_UPLOAD_HEADERS.size() + 1;
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())) {
      return internalProcessDataFromExcel.get(0).keySet().size()
        != BulkParameters.BULK_PRICE_UPDATE_NEW_UPLOAD_HEADERS.size() + 1;
    } else if (BulkInternalProcessType.SUSPEND.name()
      .equals(bulkInternalProcess.getProcessType())) {
      return headerRow.size() != POIUtil.SUSPENSION_HEADERS.size() + 1;
    }
    return true;
  }

  public static boolean validateHeaderByProcessType(List<Map<String, String>> internalProcessDataFromExcel,
      BulkInternalProcess bulkInternalProcess) {
    if (CollectionUtils.isEmpty(internalProcessDataFromExcel)) {
      return true;
    }
    Set<String> headersFromExcel = internalProcessDataFromExcel.get(0).keySet();
    if (BulkInternalProcessType.STORE_COPY.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(StoreCopyConstants.TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(SalesCategoryUpdateConstants.TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BrandAuthorisationConstant.TEMPLATE_HEADER);
    }  else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(bulkInternalProcess.getProcessType())) {
      return false;
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
      return false;
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(ExcelHeaderNames.RESTRICTED_KEYWORD_UPSERT_TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(ExcelHeaderNames.RESTRICTED_KEYWORD_DELETE_TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.SUSPEND.name()
      .equals(bulkInternalProcess.getProcessType())) {
      return !headersFromExcel.containsAll(POIUtil.SUSPENSION_HEADERS);
    }
    else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(ExcelHeaderNames.BRAND_AUTH_DELETE_TEMPLATE_HEADER);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkApprovalRejectionParameters.BULK_APPROVAL);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkApprovalRejectionParameters.BULK_REJECTION);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()
      .equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
        .containsAll(BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(BulkMasterSkuUploadParameters.MASTER_SKU_BULK_REVIEW_HEADER_LIST);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(bulkInternalProcess.getProcessType())) {
      return !(internalProcessDataFromExcel.get(0).keySet()
          .containsAll(BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_HEADERS) ||
          internalProcessDataFromExcel.get(0).keySet()
              .containsAll(BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_OPTIONAL_HEADERS));
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet()
          .containsAll(BulkParameters.BULK_INTERNAL_PRICE_UPDATE_HEADER);
    } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkParameters.BULK_REBATE_UPLOAD_HEADERS);
    }
    else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkParameters.BULK_PRICING_PRODUCT_TYPE_UPLOAD_HEADERS);
    }
    else if(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkIPRProductsParameter.BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS);
    }
    else if (BulkInternalProcessType. BULK_SKU_LEVEL_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkParameters.BULK_SKU_LEVEL_REBATE_UPLOAD_HEADERS);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())) {
      return !internalProcessDataFromExcel.get(0).keySet().containsAll(BulkParameters.BULK_PRICE_UPDATE_NEW_UPLOAD_HEADERS);
    }
    return true;
  }

  public static InternalProcessDataDomainEventModel toInternalProcessDataDomainEventModel(
      BulkInternalProcessPendingDataDTO bulkInternalProcessPendingDataDTO, String storeId) {
    return InternalProcessDataDomainEventModel.builder().storeId(storeId)
        .parentCode(bulkInternalProcessPendingDataDTO.getParentCode())
        .processType(bulkInternalProcessPendingDataDTO.getProcessType())
        .internalProcessRequestId(bulkInternalProcessPendingDataDTO.getInternalProcessRequestId()).build();
  }

  public static InternalProcessDataDomainEventModel toInternalProcessDataDomainEventModel(
      BulkInternalProcessData bulkInternalProcessData, String storeId) {
    return InternalProcessDataDomainEventModel.builder().storeId(storeId)
        .parentCode(bulkInternalProcessData.getParentCode())
        .processType(bulkInternalProcessData.getProcessType())
        .internalProcessRequestId(bulkInternalProcessData.getInternalProcessRequestId()).build();
  }

  public static InternalBulkUploadDataDomainEventModel toInternalBulkUploadDataDomainEventModel(
      BulkInternalProcessData bulkInternalProcessData, String storeId, String username) {
    return InternalBulkUploadDataDomainEventModel.builder().storeId(storeId)
        .updatedBy(username)
        .processType(bulkInternalProcessData.getProcessType())
        .internalProcessDataRequestId(bulkInternalProcessData.getId()).build();
  }

  public static InternalBrandUpdateEventModel toInternalBrandUpdateEventModel(
      BulkInternalProcessData bulkInternalProcessData, String storeId, String username)
      throws Exception {
    InternalBrandUpdateNotes internalBrandUpdateNotes =
        objectMapper.readValue(bulkInternalProcessData.getNotes(), InternalBrandUpdateNotes.class);
    return InternalBrandUpdateEventModel.builder().storeId(storeId).updatedBy(username)
        .processType(bulkInternalProcessData.getProcessType())
        .internalProcessDataRequestId(bulkInternalProcessData.getId())
        .brandNameUpdate(BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()
            .equals(bulkInternalProcessData.getData()))
        .oldBrandCode(internalBrandUpdateNotes.getSourceBrandCode())
        .oldBrandName(internalBrandUpdateNotes.getSourceBrandName())
        .newBrandCode(internalBrandUpdateNotes.getDestinationBrandCode())
        .newBrandName(internalBrandUpdateNotes.getDestinationBrandName()).build();
  }

  public static InternalBulkUploadDataDomainEventModel toInternalBulkUploadDataDomainEventModel(
      List<String> internalProcessDataRequestIdList, String bulkProcessType, String storeId,
      String username) {
    return InternalBulkUploadDataDomainEventModel.builder().storeId(storeId)
        .updatedBy(username)
        .processType(bulkProcessType)
        .internalProcessDataRequestIdList(internalProcessDataRequestIdList).build();
  }

  public static SalesCategoryUpdateRequest toSalesCategoryUpdateRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, SalesCategoryUpdateRequest.class);
  }

  public static InternalBulkUploadRequest toInternalBulkUploadRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, InternalBulkUploadRequest.class);
  }

  public static ProductBrandUpdateRequest toProductBrandUpdateRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, ProductBrandUpdateRequest.class);
  }

  public static VendorBulkAssignmentRequest toVendorBulkAssignmentRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, VendorBulkAssignmentRequest.class);
  }

  public static BrandAuthorisationRequest toBrandAuthorisationRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, BrandAuthorisationRequest.class);
  }

  public static FbbCreatePickupPointRequest toFbbCreateRequestFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, FbbCreatePickupPointRequest.class);
  }

  public static SimpleMasterProductUpdateRequest toSimpleMasterProductUpdateRequest(InternalBulkUploadRequest internalBulkUploadRequest) {
    return new SimpleMasterProductUpdateRequest.Builder()
        .productCode(internalBulkUploadRequest.getProductCode())
        .name(internalBulkUploadRequest.getProductName())
        .brand(internalBulkUploadRequest.getBrand())
        .length(Double.parseDouble(internalBulkUploadRequest.getLength()))
        .width(Double.parseDouble(internalBulkUploadRequest.getWidth()))
        .weight(Double.parseDouble(internalBulkUploadRequest.getWeight()))
        .height(Double.parseDouble(internalBulkUploadRequest.getHeight()))
        .dangerousGoodsLevel((int) Double.parseDouble(internalBulkUploadRequest.getDangerousGoodsLevel()))
        .build();
  }

  public static BulkVendorProductActionsRequest toBulkVendorProductActionsRequest(
      VendorBulkAssignmentRequest vendorBulkAssignmentRequest, BulkInternalProcessData bulkInternalProcessData) {
    BulkScreeningProductActionsRequest bulkScreeningProductActionsRequests =
        BulkScreeningProductActionsRequest.builder()
            .productCodes(Collections.singletonList(vendorBulkAssignmentRequest.getProductCode()))
            .assignedBy(vendorBulkAssignmentRequest.getAssignedBy().replaceAll(REMOVE_TRAILING_QUOTATIONS, ""))
            .assignTo(vendorBulkAssignmentRequest.getAssignedTo().replaceAll(REMOVE_TRAILING_QUOTATIONS, "")).build();
    return BulkVendorProductActionsRequest.builder().requestId(Constant.REQUEST_ID)
        .storeId(bulkInternalProcessData.getStoreId()).userName(vendorBulkAssignmentRequest.getAssignedBy())
        .actionType(VendorProductDataBulkParameters.ASSIGN)
        .bulkScreeningProductActionsRequests(Collections.singletonList(bulkScreeningProductActionsRequests)).build();
  }

  public static BulkMasterProductUpdateRequest toBulkMasterProductUpdateRequest(
      SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest, String storeId, String updatedBy) {
    BulkMasterProductUpdateRequest bulkMasterProductUpdateRequest = new BulkMasterProductUpdateRequest();
    bulkMasterProductUpdateRequest.setSimpleMasterProductUpdateRequests(Arrays.asList(simpleMasterProductUpdateRequest));
    bulkMasterProductUpdateRequest.setUpdatedBy(updatedBy);
    bulkMasterProductUpdateRequest.setStoreId(storeId);
    bulkMasterProductUpdateRequest.setUpdatedDate(Calendar.getInstance().getTime());
    return bulkMasterProductUpdateRequest;
  }

  public static BulkRebateUpdateRequest toBulkRebateUpdateRequest(
      BulkPriceRebateRequestData bulkPriceRebateRequestData) {
    BulkRebateUpdateRequest bulkRebateUpdateRequest = new BulkRebateUpdateRequest();
    bulkRebateUpdateRequest.setRebate(Double.parseDouble(bulkPriceRebateRequestData.getProjectedRebate()));
    bulkRebateUpdateRequest.setBusinessPartnerCode(bulkPriceRebateRequestData.getStoreId());
    bulkRebateUpdateRequest.setBrandName(bulkPriceRebateRequestData.getBrandName());
    bulkRebateUpdateRequest.setMonth(monthToInt(bulkPriceRebateRequestData.getMonth()));
    bulkRebateUpdateRequest.setYear(Integer.parseInt(bulkPriceRebateRequestData.getYear()));
    bulkRebateUpdateRequest.setC1CategoryCode(bulkPriceRebateRequestData.getMainCategoryCode());
    bulkRebateUpdateRequest.setC2CategoryCode(bulkPriceRebateRequestData.getCategoryCode());
    return bulkRebateUpdateRequest;
  }

  public static IprActionRequest toIprActionRequest(
      BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData, String updatedBy) {
    IprActionRequest iprActionRequest = new IprActionRequest();
    BeanUtils.copyProperties(bulkAddReviewIPRProductsRequestData, iprActionRequest);
    iprActionRequest.setAction(BULK_IPR_ACTIONS_MAP.get(iprActionRequest.getAction()));
    iprActionRequest.setSource(BULK_IPR_SOURCE_MAP.get(iprActionRequest.getSource()));
    iprActionRequest.setBulkAction(true);
    iprActionRequest.setUpdatedBy(updatedBy);
    if (BRAND_REPORT.equals(bulkAddReviewIPRProductsRequestData.getSource())) {
      BrandReport brandReport = new BrandReport();
      BeanUtils.copyProperties(bulkAddReviewIPRProductsRequestData, brandReport);
      Date date = convertExcelSerialToDate(bulkAddReviewIPRProductsRequestData.getReportDate());
      SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");
      String formattedDate = sdf.format(date);
      DateTimeFormatter formatter = DateTimeFormat.forPattern("MM/dd/yyyy");
      LocalDate localDate = LocalDate.parse(formattedDate, formatter);
      DateTime dateTime = localDate.toDateTimeAtStartOfDay();
      brandReport.setReportDate(new Date(dateTime.getMillis()));
      iprActionRequest.setBrandReport(brandReport);
    }
    return iprActionRequest;
  }

  private static int monthToInt(String monthName) {
    Month month = Month.valueOf(monthName.toUpperCase());
    return month.getValue();
  }

  public static void updateInternalProcessDataStatusAndErrorMessage(BulkInternalProcessData bulkInternalProcessData,
      String status, String errorMessage) {
    bulkInternalProcessData.setStatus(status);
    bulkInternalProcessData.setErrorMessage(errorMessage);
    bulkInternalProcessData.setUpdatedDate(new Date());
  }

  public static void updateInternalProcessDataStatusAndErrorMessageForFbb(
    BulkInternalProcessData bulkInternalProcessData, String status, String errorMessage,
    String errorCode) {
    bulkInternalProcessData.setStatus(status);
    bulkInternalProcessData.setErrorMessage(errorMessage);
    bulkInternalProcessData.setNotes(errorCode);
  }

  public static LinkedHashMap<String, Object> getInputRows(BulkInternalProcessData bulkInternalProcessData)
      throws IOException {
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    LinkedHashMap<String, Object> rowDataJson =
        new ObjectMapper().readValue(bulkInternalProcessData.getData(), typeRef);
    return rowDataJson;
  }

  public static List<DormantSellerProduct> toSuspendSellerActiveProductList(String storeId,
    List<ProductL3SummaryResponse> activeProductList, DormantSellerEvent dormantSellerEvent, String processType) {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    for (ProductL3SummaryResponse productL3SummaryResponse : activeProductList) {
      DormantSellerProduct dormantSellerProduct = new DormantSellerProduct();
      dormantSellerProduct.setStoreId(storeId);
      dormantSellerProduct.setStatus(DormantSellerStatus.PENDING.name());
      dormantSellerProduct.setProductStatus(DormantSellerProductStatus.ACTIVE.name());
      dormantSellerProduct.setItemSku(productL3SummaryResponse.getProductSku());
      dormantSellerProduct.setDormantSellerEventId(dormantSellerEvent.getId());
      dormantSellerProduct.setBusinessPartnerCode(dormantSellerEvent.getBusinessPartnerCode());
      dormantSellerProduct.setProcessType(processType);
      dormantSellerProductList.add(dormantSellerProduct);
    }
    return dormantSellerProductList;
  }

  public static List<BulkProcessData> generateBulkProcessDateForInstoreUpdate(BulkProcess bulkProcess,
      List<Map<String, String>> off2OnProductSkuRequest) throws JsonProcessingException {
    ObjectMapper objectMapper = new ObjectMapper();
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    Map<String, Long> productSkusCount =
        off2OnProductSkuRequest.stream().map(off2OnRequest -> off2OnRequest.get(BulkParameters.BLIBLI_PRODUCT_SKU))
            .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
    int rowNumber = 0;
    for (Map<String, String> row : off2OnProductSkuRequest) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      if (productSkusCount.get(row.get(BulkParameters.BLIBLI_PRODUCT_SKU)) > 1) {
        setBasicDetailsBulkProcessData(bulkProcess, bulkProcessData, BulkProcessData.STATUS_FAIL,
            BulkProcessValidationErrorMessages.DUPLICATE_PRODUCT_SKUS);
      } else {
        if (!BulkUpdateServiceUtil.validateProductSkuAndSellerCodeWhenUpload(row.get(BulkParameters.BLIBLI_PRODUCT_SKU),
            bulkProcess.getBusinessPartnerCode())) {
          setBasicDetailsBulkProcessData(bulkProcess, bulkProcessData, BulkProcessData.STATUS_FAIL,
              String.format(BulkProcessValidationErrorMessages.INVALID_PRODUCT_SKU, bulkProcess.getBulkProcessCode()));
        } else {
          setBasicDetailsBulkProcessData(bulkProcess, bulkProcessData, BulkProcessData.STATUS_PENDING,
              StringUtils.EMPTY);
        }
      }
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(
          InstoreUpdateModel.builder().productSku(row.get(BulkParameters.BLIBLI_PRODUCT_SKU))
              .productName(row.get(BulkParameters.PARENT_PRODUCT_NAME)).off2OnFlag(row.get(BulkParameters.OFF2ON_VALUE))
              .build()));
      bulkProcessData.setRowNumber(rowNumber++);
      bulkProcessDataList.add(bulkProcessData);
    }
    return bulkProcessDataList;
  }

  private static void setBasicDetailsBulkProcessData(BulkProcess bulkProcess, BulkProcessData bulkProcessData,
      String status, String errorMessage) {
    bulkProcessData.setBulkProcessId(bulkProcess.getId());
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setStoreId(bulkProcess.getStoreId());
    bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
    bulkProcessData.setStatus(status);
    bulkProcessData.setErrorMessage(errorMessage);
    bulkProcessData.setRequestId(bulkProcess.getRequestId());
    if (!StringUtils.isBlank(errorMessage)) {
      bulkProcessData.setEndDate(new Date());
    }
  }

  public static InstoreUpdateModel toInstoreUpdateModelFromJson(String json) throws IOException {
    return new ObjectMapper().readValue(json, InstoreUpdateModel.class);
  }

  private static BulkInternalProcessData validateRowAndFormRequest(Map<String, String> row)
      throws JsonProcessingException {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    InternalBulkUploadRequest internalBulkUploadRequest = new InternalBulkUploadRequest();
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    bulkInternalProcessData.setSellerCode(Constant.INTERNAL);
    StringBuilder errorMessage = new StringBuilder();
    if (!validateProductCode(row)) {
      errorMessage.append(Constant.ERROR_IN_PRODUCT_CODE).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setProductCode(row.get(MasterDataBulkParameters.PRODUCT_CODE));
    }
    if (!validateProductName(row)) {
      errorMessage.append(Constant.ERROR_IN_PRODUCT_NAME).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setProductName(row.get(MasterDataBulkParameters.PRODUCT_NAME));
    }
    if (!validateBrand(row)) {
      errorMessage.append(Constant.ERROR_IN_BRAND).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setBrand(row.get(MasterDataBulkParameters.BRAND));
    }
    if (!validateLength(row)) {
      errorMessage.append(Constant.ERROR_IN_LENGTH).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setLength(row.get(MasterDataBulkParameters.LENGTH));
    }
    if (!validateWidth(row)) {
      errorMessage.append(Constant.ERROR_IN_WIDTH).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setWidth(row.get(MasterDataBulkParameters.WIDTH));
    }
    if (!validateHeight(row)) {
      errorMessage.append(Constant.ERROR_IN_HEIGHT).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setHeight(row.get(MasterDataBulkParameters.HEIGHT));
    }
    if (!validateWeight(row)) {
      errorMessage.append(Constant.ERROR_IN_WEIGHT).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setWeight(row.get(MasterDataBulkParameters.WEIGHT));
    }
    if (!validateDangerousGoodsLevel(row)) {
      errorMessage.append(Constant.ERROR_IN_DG_LEVEL).append(StringUtils.SPACE);
    } else {
      internalBulkUploadRequest.setDangerousGoodsLevel(row.get(MasterDataBulkParameters.DANGEROUS_GOOD_LEVEL));
    }
    if (org.apache.commons.lang3.StringUtils.isBlank(errorMessage.toString())) {
      bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    } else {
      bulkInternalProcessData.setErrorMessage(errorMessage.toString());
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    }
    bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(internalBulkUploadRequest));
    return bulkInternalProcessData;
  }

  private static boolean validateProductCode(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.PRODUCT_CODE))){
      log.error(Constant.PRODUCT_CODE_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateAssignee(Map<String, String> row, List<String> validUserRoleList) {
    if (StringUtils.isBlank(row.get(VendorProductDataBulkParameters.ASSIGNEE))) {
      log.error(Constant.ASSIGNEE_EMPTY, row);
      return false;
    }
    if (!validUserRoleList.contains(row.get(VendorProductDataBulkParameters.ASSIGNEE))) {
      log.error(Constant.ASSIGNEE_INVALID, row);
      return false;
    }
    return true;
  }

  private static boolean validateProductName(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.PRODUCT_NAME))){
      log.error(Constant.PRODUCT_NAME_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateBrand(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.BRAND))){
      log.error(Constant.BRAND_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateLength(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.LENGTH))){
      log.error(Constant.LENGTH_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateWidth(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.WIDTH))){
      log.error(Constant.WIDTH_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateHeight(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.HEIGHT))){
      log.error(Constant.HEIGHT_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateWeight(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.WEIGHT))){
      log.error(Constant.WEIGHT_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private static boolean validateDangerousGoodsLevel(Map<String, String> row){
    if(org.apache.commons.lang3.StringUtils.isBlank(row.get(MasterDataBulkParameters.DANGEROUS_GOOD_LEVEL))){
      log.error(Constant.DANGEROUS_GOOD_LEVEL_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  public static InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO(
    ItemPickupPointListingResponse itemPickupPointListingResponse) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
      new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(itemPickupPointListingResponse.getItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(
      itemPickupPointListingResponse.getPickUpPointCode());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(
      itemPickupPointListingResponse.getMerchantCode());
    return inventoryDetailInfoRequestDTO;
  }

  public static String toL5Id(String itemSku, String pickupPointCode) {
    return new StringBuilder().append(itemSku).append(Constant.HYPHEN).append(pickupPointCode)
      .toString();
  }

  public static BulkInternalProcess getFbbBulkInternalProcess(String storeId,
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest) {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
      .internalProcessRequestCode(bulkInternalProcessUploadRequest.getInternalProcessRequestCode())
      .processType(bulkInternalProcessUploadRequest.getProcessType())
      .sellerCode(bulkInternalProcessUploadRequest.getSellerCode())
      .errorCount(0).totalCount(0).successCount(0)
      .status(ProcessStatus.PENDING.name())
      .notes(bulkInternalProcessUploadRequest.getNotes())
      .build();
    bulkInternalProcess.setStoreId(storeId);
    return bulkInternalProcess;
  }

  public static BulkInternalProcessData getFbbL5InternalDataProcess(String storeId,
    BulkInternalProcess bulkInternalProcess, FbbL5CreateDTO fbbL5CreateDTO) throws JsonProcessingException {

    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.FBB_L5_CREATE.name());
    bulkInternalProcessData.setSellerCode(fbbL5CreateDTO.getBusinessPartnerCode());
    bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(fbbL5CreateDTO));

    setBulkInternalProcessDataBasicDetails(storeId, fbbL5CreateDTO.getBusinessPartnerCode(),
      bulkInternalProcess, bulkInternalProcessData);

    return bulkInternalProcessData;
  }

  public static FbbStatusEventModel toFbbUpdateStatusEvent(BulkInternalProcess bulkInternalProcess ,
    List<FbbFailedItems> failedItems, String status) {
    List<FbbFailedItems> fbbFailedItemsList = new ArrayList<>();
    failedItems.forEach(item -> {
      FbbFailedItems fbbFailedItems1 = new FbbFailedItems();
      BeanUtils.copyProperties(item, fbbFailedItems1);
      fbbFailedItemsList.add(fbbFailedItems1);
    });
    return FbbStatusEventModel.builder().consignmentId(bulkInternalProcess.getNotes())
      .failedItems(fbbFailedItemsList).result(status).build();
  }

  public static BulkInternalProcess toBulkInternalProcessForAutoAssignment(
      VendorAutoAssignmentRequest vendorAutoAssignmentRequest) throws JsonProcessingException {
    int totalCount =
        vendorAutoAssignmentRequest.getRequestedSkuCount() * vendorAutoAssignmentRequest.getAssigneeList().size();
    ObjectMapper objectMapper = new ObjectMapper();
    String assignmentList = objectMapper.writeValueAsString(vendorAutoAssignmentRequest.getAssigneeList());
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
        .internalProcessRequestCode(vendorAutoAssignmentRequest.getInternalProcessRequestCode())
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString())
      .sellerCode(vendorAutoAssignmentRequest.getVendorCode())
      .sellerName(vendorAutoAssignmentRequest.getVendorCode())
      .fileName(objectMapper.writeValueAsString(vendorAutoAssignmentRequest.getVendorAutoAssignmentFilterRequest()))
        .errorCount(0).totalCount(totalCount).successCount(0)
        .status(ProcessStatus.PENDING.name()).notes(assignmentList).build();
    bulkInternalProcess.setStoreId(vendorAutoAssignmentRequest.getStoreId());
    bulkInternalProcess.setCreatedBy(vendorAutoAssignmentRequest.getVendorEmail());
    return bulkInternalProcess;
  }

  public static BoostedProductFilterRequest toBoostedProductFilterRequest(VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest) {
    BoostedProductFilterRequest boostedProductFilterRequest = new BoostedProductFilterRequest();
    if(Objects.nonNull(vendorAutoAssignmentFilterRequest)){
      BeanUtils.copyProperties(vendorAutoAssignmentFilterRequest, boostedProductFilterRequest);
    }
    return boostedProductFilterRequest;
  }

  public static BulkInternalProcess getBulkInternalProcessFromBulkRestrictedKeywordUploadRequest(String storeId,
      BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel) {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder()
        .internalProcessRequestCode(bulkRestrictedKeywordUploadModel.getBulkProcessCode())
        .processType(bulkRestrictedKeywordUploadModel.getBulkProcessType())
        .fileName(bulkRestrictedKeywordUploadModel.getFilePath()).startTime(Calendar.getInstance().getTime())
        .errorCount(0).totalCount(0).successCount(0).status(ProcessStatus.PENDING.name())
        .notes(bulkRestrictedKeywordUploadModel.getActionType()).build();
    bulkInternalProcess.setStoreId(bulkRestrictedKeywordUploadModel.getStoreId());
    return bulkInternalProcess;
  }

  public static BulkInternalProcess getBulkInternalProcessFromBulkBrandAuthUploadRequest(String storeId,
      BulkBrandAuthUploadModel bulkBrandAuthUploadModel) {
    BulkInternalProcess bulkInternalProcess =
        BulkInternalProcess.builder().internalProcessRequestCode(bulkBrandAuthUploadModel.getBulkProcessCode())
            .processType(bulkBrandAuthUploadModel.getBulkProcessType()).fileName(bulkBrandAuthUploadModel.getFilePath())
            .startTime(Calendar.getInstance().getTime()).errorCount(0).totalCount(0).successCount(0)
            .status(ProcessStatus.PENDING.name()).build();
    bulkInternalProcess.setStoreId(bulkBrandAuthUploadModel.getStoreId());
    return bulkInternalProcess;
  }

  public static BrandAuthFilterRequest toBrandAuthFilterRequest(BrandAuthDownloadRequest brandAuthDownloadRequest) {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BeanUtils.copyProperties(brandAuthDownloadRequest, brandAuthFilterRequest);
    brandAuthFilterRequest.setBrandAuthorise(Boolean.TRUE);
    return brandAuthFilterRequest;
  }

  public static List<BulkProcessStatusListingResponse> toBulkProcessStatusListingResponse(
    Page<BulkProcess> bulkProcessListing, Set<String> bulkProcessTypesToSuppressCountError) throws JsonProcessingException, ParseException {
    List<String> unfinishedProcessStatus =
      Arrays.asList(BulkProcess.STATUS_PENDING, BulkProcess.STATUS_IN_PROGRESS,
        STATUS_IMAGE_PROCESSING, STATUS_IMAGE_PROCESSING_PRIORITY_1,
        STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED,
        BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
        BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1,
        BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses = new ArrayList<>();
    if (Objects.nonNull(bulkProcessListing) && CollectionUtils.isNotEmpty(
      bulkProcessListing.getContent())) {
      for (BulkProcess bulkProcess : bulkProcessListing.getContent()) {
        BulkProcessStatusListingResponse listingResponse = new BulkProcessStatusListingResponse();
        listingResponse.setStoreId(bulkProcess.getStoreId());
        listingResponse.setCreatedDate(bulkProcess.getCreatedDate());
        listingResponse.setBulkProcessCode(bulkProcess.getBulkProcessCode());
        listingResponse.setStoreId(bulkProcess.getStoreId());
        listingResponse.setBulkProcessType(bulkProcess.getBulkProcessType());
        listingResponse.setBulkActivityStatus(
          BulkActivityStatus.getActivityStatusFromBulkStatus(bulkProcess.getStatus()));
        listingResponse.setProcessCompletionPercentage(
          calculateCompletionPercentage(Optional.ofNullable(bulkProcess.getTotalCount()).orElse(0),
            Optional.ofNullable(bulkProcess.getSuccessCount()).orElse(0), bulkProcess.getBulkProcessType(),
              bulkProcessTypesToSuppressCountError));
        listingResponse.setProcessCompleted(
          !unfinishedProcessStatus.contains(bulkProcess.getStatus()));
        listingResponse.setErrorFileLink(Stream.of(BulkProcess.STATUS_ABORTED, BulkProcess.STATUS_PARTIALLY_DONE)
            .anyMatch(status -> bulkProcess.getStatus().equalsIgnoreCase(status)) && !Objects.equals(
            bulkProcess.getTotalCount(), bulkProcess.getSuccessCount()) && !hasZeroErrorCount(bulkProcess) ?
          fileStorageService.getErrorFileLinkForListing(bulkProcess) : StringUtils.EMPTY);
        listingResponse.setSuccessRowCount(
          Optional.ofNullable(bulkProcess.getSuccessCount()).orElse(0));
        listingResponse.setTotalRowCountRequested(
          Optional.ofNullable(bulkProcess.getTotalCount()).orElse(0));
        listingResponse.setErrorRowCount(bulkProcess.getErrorCount());
        listingResponse.setBusinessPartnerCode(bulkProcess.getBusinessPartnerCode());
        //TODO : Estimate completion time based by Data analysis
        listingResponse.setEstimatedCompletionTime(null);
        listingResponse.setUploadDate(bulkProcess.getCreatedDate());
        listingResponse.setUser(bulkProcess.getCreatedBy());
        listingResponse.setUploadedFileName(
          Optional.ofNullable(bulkProcess.getUploadedFile()).orElse(StringUtils.EMPTY));
        listingResponse.setDescription(bulkProcess.getDescription());
        bulkProcessStatusListingResponses.add(listingResponse);
      }
    }
    return bulkProcessStatusListingResponses;
  }

  public static double calculateCompletionPercentage(int totalCount, int processedRowCount, String bulkProcessType,
      Set<String> suppressErrorForTotalCountListingBulkProcessTypes) {
    if (processedRowCount > totalCount) {
      if (StringUtils.isNotBlank(bulkProcessType) && Optional.ofNullable(
          suppressErrorForTotalCountListingBulkProcessTypes).orElse(new HashSet<>()).contains(bulkProcessType)) {
        return 0.0;
      }
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        ErrorCategory.INVALID_STATE.getCode());
    }
    if (totalCount == 0) {
      return 0.0;
    }
    double completionPercentage = (double) processedRowCount / totalCount * 100;
    return (int) Math.round(completionPercentage);
  }

  private static boolean hasZeroErrorCount(BulkProcess bulkProcess) {
    return Objects.requireNonNullElse(bulkProcess.getErrorCount(), 0) == 0
      && Objects.requireNonNullElse(bulkProcess.getInputErrorCount(), 0) == 0
      && Objects.requireNonNullElse(bulkProcess.getSystemErrorCount(), 0) == 0;
  }

  public static List<BulkProcessStatusListingResponse> downloadEntityToBulkProcessStatusListingResponse(
      Page<BulkDownloadEntity> bulkDownloadListing) throws Exception {
    List<String> unfinishedDownloadTypes = Arrays.asList(BulkProcess.STATUS_IN_PROGRESS, BulkProcess.STATUS_PENDING);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses = new ArrayList<>();
    if (Objects.nonNull(bulkDownloadListing) && CollectionUtils.isNotEmpty(bulkDownloadListing.getContent())) {
      for (BulkDownloadEntity bulkDownloadEntity : bulkDownloadListing.getContent()) {
        BulkProcessStatusListingResponse bulkProcessStatusListingResponse = new BulkProcessStatusListingResponse();
        bulkProcessStatusListingResponse.setBusinessPartnerCode(bulkDownloadEntity.getBusinessPartnerCode());
        bulkProcessStatusListingResponse.setCreatedDate(bulkDownloadEntity.getCreatedDate());
        bulkProcessStatusListingResponse.setCreatedBy(bulkDownloadEntity.getCreatedBy());
        bulkProcessStatusListingResponse.setUploadDate(bulkDownloadEntity.getCreatedDate());
        bulkProcessStatusListingResponse.setBulkProcessType(bulkDownloadEntity.getEntityType());
        bulkProcessStatusListingResponse.setUser(bulkDownloadEntity.getCreatedBy());
        bulkProcessStatusListingResponse.setUploadedFileName(bulkDownloadEntity.getFileName());
        bulkProcessStatusListingResponse.setSuccessRowCount(bulkDownloadEntity.getRecordsDownload());
        if (unfinishedDownloadTypes.contains(bulkDownloadEntity.getStatus())) {
          bulkProcessStatusListingResponse.setSuccessRowCount(bulkDownloadEntity.getRecordsDownload());
          bulkProcessStatusListingResponse.setProcessCompletionPercentage(0d);
        }
        if (bulkDownloadEntity.getStatus().equals(BulkProcess.STATUS_ABORTED) || bulkDownloadEntity.getStatus()
            .equals(BulkProcessConstant.DOWNLOAD_FAILED)) {
          bulkProcessStatusListingResponse.setProcessCompleted(true);
          bulkProcessStatusListingResponse.setSuccessRowCount(bulkDownloadEntity.getRecordsDownload());
        }
        bulkProcessStatusListingResponse
            .setBulkActivityStatus(BulkActivityStatus.getActivityStatusFromBulkStatus(bulkDownloadEntity.getStatus()));
        if (bulkDownloadEntity.getStatus().equals(BulkProcessConstant.DOWNLOAD_SUCCESS)) {
          BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
          bulkDownloadRequest.setRequestId(bulkDownloadEntity.getRequestId());
          bulkDownloadRequest
              .setBulkProcessEntity(Enum.valueOf(BulkProcessEntity.class, bulkDownloadEntity.getEntityType()));
          bulkDownloadRequest.setMerchantId(bulkDownloadEntity.getBusinessPartnerCode());
          bulkDownloadRequest.setFilename(bulkDownloadEntity.getFileName());
          bulkProcessStatusListingResponse
              .setDownloadFileLink(fileStorageService.getNotificationDetailPath(bulkDownloadRequest));
          bulkProcessStatusListingResponse.setProcessCompleted(true);
          bulkProcessStatusListingResponse.setProcessCompletionPercentage(100d);
        }
        bulkProcessStatusListingResponses.add(bulkProcessStatusListingResponse);
      }
    }
    return bulkProcessStatusListingResponses;
  }
  public static BrandAuthCreateRequest toBrandAuthCreateRequest(BrandAuthAddRequestData brandAuthAddRequestData) {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    BeanUtils.copyProperties(brandAuthAddRequestData, brandAuthCreateRequest);
    brandAuthCreateRequest.setAuthorisationStatus(Constant.ACTIVE);
    brandAuthCreateRequest.setBulkAction(true);
    return brandAuthCreateRequest;
  }

  public static BrandAuthDeleteRequest toBrandAuthDeleteRequest(BrandAuthDeleteRequestData brandAuthDeleteRequestData) {
    BrandAuthDeleteRequest brandAuthDeleteRequest = new BrandAuthDeleteRequest();
    BeanUtils.copyProperties(brandAuthDeleteRequestData, brandAuthDeleteRequest);
    return brandAuthDeleteRequest;
  }

  public static BulkInternalProcess getBulkInternalProcessFromBulkReviewUploadRequest(String storeId,
                                                                                      BulkReviewUploadModel bulkReviewUploadModel) {
    BulkInternalProcess bulkInternalProcess =
            BulkInternalProcess.builder().internalProcessRequestCode(bulkReviewUploadModel.getBulkProcessCode())
                    .processType(bulkReviewUploadModel.getBulkProcessType()).fileName(bulkReviewUploadModel.getFilePath())
                    .startTime(Calendar.getInstance().getTime()).errorCount(0).totalCount(0).successCount(0)
                    .status(ProcessStatus.PENDING.name()).build();
    bulkInternalProcess.setStoreId(bulkReviewUploadModel.getStoreId());
    bulkInternalProcess.setCreatedBy(bulkReviewUploadModel.getCreatedBy());
    bulkInternalProcess.setUpdatedBy(bulkReviewUploadModel.getCreatedBy());
    bulkInternalProcess.setSellerCode(bulkReviewUploadModel.getVendorCode());
    bulkInternalProcess.setSellerName(bulkReviewUploadModel.getVendorCode());
    return bulkInternalProcess;
  }

  public static RejectProductVendorRequest toVendorRejectRequest(
          BulkApprovalRejectionRequestData bulkVendorActionsModel) {
    RejectProductVendorRequest rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(bulkVendorActionsModel.getProductCode());
    rejectProductVendorRequest.setNotes(bulkVendorActionsModel.getComment());
    rejectProductVendorRequest.setBulkAction(true);

    Map<String, String> rejectionReasonMap =
      Splitter.on(Constant.COMMA).withKeyValueSeparator(Constant.COLON).split(bulkVendorActionsModel.getReason());

    if (rejectionReasonMap.containsKey(Constant.PRODUK)) {
      rejectProductVendorRequest.setRejectReasonRequest(RejectReasonRequest.builder()
        .product(Collections.singletonList(rejectionReasonMap.get(Constant.PRODUK))).build());
    } else if (rejectionReasonMap.containsKey(Constant.KONTEN)) {
      rejectProductVendorRequest.setRejectReasonRequest(RejectReasonRequest.builder()
        .content(Collections.singletonList(rejectionReasonMap.get(Constant.KONTEN))).build());
    } else {
      rejectProductVendorRequest.setRejectReasonRequest(RejectReasonRequest.builder()
        .image(Collections.singletonList(rejectionReasonMap.get(Constant.FOTO))).build());
    }
    return rejectProductVendorRequest;
  }

  public static VendorQuickApprovalRequest toVendorQuickApprovalRequest(
          BulkApprovalRejectionRequestData bulkVendorActionsModel, String vendorCode) {
    return VendorQuickApprovalRequest.builder().vendorCode(vendorCode)
      .productCode(bulkVendorActionsModel.getProductCode()).Notes(
        StringUtils.isNotEmpty(bulkVendorActionsModel.getReason()) ?
          bulkVendorActionsModel.getReason() :
          Constant.DASH).isBulkAction(true).build();
  }
  public static ProductLevel3ViewConfigResponse getDefaultChannelItemViewConfig(
    ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    return itemPickupPointListingL3Response.getViewConfigs().stream()
      .filter(response -> Constant.DEFAULT.equalsIgnoreCase(response.getChannelId())).findFirst()
      .orElse(null);
  }

  public static ProductLevel3ViewConfigResponse getCncChannelItemViewConfig(
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    return itemPickupPointListingL3Response.getViewConfigs().stream()
        .filter(response -> Constant.CNC.equalsIgnoreCase(response.getChannelId())).findFirst()
        .orElse(null);
  }

  public static ProductLevel3ViewConfigResponse getBfbChannelItemViewConfig(
    ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    return itemPickupPointListingL3Response.getViewConfigs().stream()
      .filter(response -> Constant.B2B_CHANNEL.equalsIgnoreCase(response.getChannelId()))
      .findFirst().orElse(null);
  }

  public static SimpleListAssemblyDisassemblyRequest getSimpleListAssemblyDisassemblyRequest(String productSku,
      int stock, String bundleType, String merchantCode, String merchantType, String warehouseCode,
      ItemBasicDetailV2Response itemBasicDetailV2Response, Map<String, String> itemSkuCogsMap) {
    SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest =
        new SimpleListAssemblyDisassemblyRequest();
    AssemblyDisassemblyRequest assemblyDisassemblyRequest = new AssemblyDisassemblyRequest();
    List<BundleRecipeRequest> bundleRecipeRequestList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemBasicDetailV2Response.getBundleRecipeList())) {
      for (BundleRecipeV2Response bundleRecipeV2Response : itemBasicDetailV2Response.getBundleRecipeList()) {
        BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
        bundleRecipeRequest.setItemSku(bundleRecipeV2Response.getItemSku());
        bundleRecipeRequest.setItemName(bundleRecipeV2Response.getGeneratedItemName());
        bundleRecipeRequest.setItemCode(bundleRecipeV2Response.getItemCode());
        bundleRecipeRequest.setQuantity(bundleRecipeV2Response.getQuantity());
        bundleRecipeRequest.setCogsPercent(
            Integer.parseInt(itemSkuCogsMap.getOrDefault(bundleRecipeV2Response.getItemSku(), DEFAULT_COGS_VALUE)));
        bundleRecipeRequestList.add(bundleRecipeRequest);
      }
    }
    assemblyDisassemblyRequest.setProductSku(productSku);
    assemblyDisassemblyRequest.setItemSku(itemBasicDetailV2Response.getItemSku());
    assemblyDisassemblyRequest.setItemCode(itemBasicDetailV2Response.getItemCode());
    assemblyDisassemblyRequest.setItemName(itemBasicDetailV2Response.getGeneratedItemName());
    assemblyDisassemblyRequest.setStockQuota(stock);
    assemblyDisassemblyRequest.setPhysicalBundle(BulkWorkOrderConstants.PHYSICAL_BUNDLING_TYPE.equals(bundleType));
    assemblyDisassemblyRequest.setMerchantCode(merchantCode);
    assemblyDisassemblyRequest.setMerchantType(merchantType);
    assemblyDisassemblyRequest.setWarehouseCode(warehouseCode);
    assemblyDisassemblyRequest.setBundleRecipe(bundleRecipeRequestList);
    simpleListAssemblyDisassemblyRequest.setValue(Collections.singletonList(assemblyDisassemblyRequest));
    return simpleListAssemblyDisassemblyRequest;
  }

  public static TransferRequest getTransferRequest(String sourceProductSku, String destinationProductSku,
      ItemBasicDetailV2Response sourceItemDetails, ItemBasicDetailV2Response destinationItemDetails,
      String merchantCode, String merchantType, String warehouseCode, WorkOrderDataModel workOrderDataModel) {
    return TransferRequest.builder().productSku(destinationProductSku).itemSku(destinationItemDetails.getItemSku())
        .itemCode(destinationItemDetails.getItemCode()).itemName(destinationItemDetails.getGeneratedItemName())
        .sourceItemCode(sourceItemDetails.getItemCode()).sourceItemName(sourceItemDetails.getGeneratedItemName())
        .sourceItemSku(sourceItemDetails.getItemSku()).sourceProductSku(sourceProductSku).merchantCode(merchantCode)
        .merchantType(merchantType).warehouseCode(warehouseCode)
        .stockQuota(Integer.parseInt(workOrderDataModel.getStock())).build();
  }
  public static DownloadItemsRequest toDownloadItemsRequest(MasterSkuReviewDownloadItemsRequest request) {
    DownloadItemsRequest downloadItemsRequest = new DownloadItemsRequest();
    BeanUtils.copyProperties(request, downloadItemsRequest);
    return downloadItemsRequest;
  }

  public static ChangeAssigneeRequest toChangeAssigneeRequest(
    BulkAssigneeMasterSkuReviewRequestData requestData, String createdBy) {
    return ChangeAssigneeRequest.builder().firstAnchorSku(requestData.getFirstAnchorSku())
      .secondAnchorSku(requestData.getSecondAnchorSku()).newAssignee(requestData.getAssignee())
      .assignedBy(createdBy).build();
  }

  public static AutoApprovedAssigneeRequest toAutoApprovedAssigneeRequest(
      BulkAssignAutoApprovedProductsRequestData requestData) {
    return AutoApprovedAssigneeRequest.builder()
        .productCode(Collections.singletonList(requestData.getProductCode()))
        .assigneeTo(requestData.getAssignee()).build();
  }

  public static BulkInternalProcessDataGenerationDTO getBulkInternalProcessDataGenerationDTO(
    int brandAuthEndYear, int bulkPriceUpdateMaxRows, int bulkRebateMaxRows, int bulkProductTypeTaggingMaxRows,
    int bulkSkuLevelRebateMaxRows, int bulkPriceUpdateNewMaxRows, String storeId, String userName,
    List<Map<String, String>> internalProcessDataFromExcel, Map<String, List<String>> reviewers,
    BulkInternalProcess bulkInternalProcess) {
    BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO = new BulkInternalProcessDataGenerationDTO();
    bulkInternalProcessDataGenerationDTO.setStoreId(storeId);
    bulkInternalProcessDataGenerationDTO.setUserName(userName);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewers);
    bulkInternalProcessDataGenerationDTO.setBrandAuthEndYear(brandAuthEndYear);
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateMaxRows(bulkPriceUpdateMaxRows);
    bulkInternalProcessDataGenerationDTO.setBulkRebateMaxRows(bulkRebateMaxRows);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(bulkProductTypeTaggingMaxRows);
    bulkInternalProcessDataGenerationDTO.setBulkSkuLevelRebateMaxRows(bulkSkuLevelRebateMaxRows);
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateNewMaxRows(bulkPriceUpdateNewMaxRows);
    return bulkInternalProcessDataGenerationDTO;
  }

  public static Map<String, String> convertFromDataToExcelHeaderMap(BulkPriceUpdateRequestData data,
      BasicProductResponse basicProductInfo, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ProfileResponse profileResponse, Set<String> cncPickupPointCodes) {
    Map<String, String> dataMap = new HashMap<>();
    dataMap.put(BulkParameters.SELLER_CODE, data.getSellerCode());
    dataMap.put(BulkParameters.BLIBLI_PRODUCT_SKU, data.getProductSku());
    dataMap.put(BulkParameters.BLIBLI_SKU, data.getItemSku());
    dataMap.put(BulkParameters.PICKUP_POINT_HEADER, data.getPickupPointCode());
    dataMap.put(BulkParameters.PRODUCT_NAME, basicProductInfo.getProductName());
    dataMap.put(BulkParameters.SKU_CODE_HEADER, data.getSkuCode());
    String merchantSku = StringUtils.EMPTY;
    if (Objects.nonNull(itemPickupPointListingL3Response)) {
      merchantSku = itemPickupPointListingL3Response.getMerchantSku();
    }
    setDataFromExcelOrExistingResponse(data.getSellerSku(), itemPickupPointListingL3Response, dataMap,
        BulkParameters.SELLER_SKU, merchantSku);
    String existingPrice = StringUtils.EMPTY;
    if (Objects.nonNull(itemPickupPointListingL3Response) && CollectionUtils.isNotEmpty(
        itemPickupPointListingL3Response.getPrices())) {
      existingPrice = String.valueOf(itemPickupPointListingL3Response.getPrices().get(0).getPrice());
    }
    setDataFromExcelOrExistingResponse(data.getListPrice(), itemPickupPointListingL3Response, dataMap,
        BulkParameters.PRICE_HEADER, existingPrice);
    String existingSellingPrice = StringUtils.EMPTY;
    if (Objects.nonNull(itemPickupPointListingL3Response) && CollectionUtils.isNotEmpty(
        itemPickupPointListingL3Response.getPrices())) {
      existingSellingPrice = String.valueOf(itemPickupPointListingL3Response.getPrices().get(0).getSalePrice());
    }
    setDataFromExcelOrExistingResponse(data.getSalesPrice(), itemPickupPointListingL3Response, dataMap,
        BulkParameters.SELLING_PRICE_HEADER, existingSellingPrice);
    setStock(data, itemPickupPointListingL3Response, dataMap);
    setCncFlag(data, itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes, dataMap);
    setDeliveryFlag(data, dataMap, itemPickupPointListingL3Response);
    setInStore(data, itemPickupPointListingL3Response, dataMap);
    setStatus(data, itemPickupPointListingL3Response, dataMap);
    setWarehouseStock(data, itemPickupPointListingL3Response, dataMap);
    return dataMap;
  }

  private static void setWarehouseStock(BulkPriceUpdateRequestData data,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, Map<String, String> dataMap) {
    if (Objects.nonNull(itemPickupPointListingL3Response) && itemPickupPointListingL3Response.isWebSyncStock()) {
      dataMap.put(BulkParameters.WAREHOUSE_STOCK_HEADER, String.valueOf(data.getWarehouseStock()));
    } else {
      dataMap.put(BulkParameters.WAREHOUSE_STOCK_HEADER, Constant.ZERO_DECIMAL_STRING);
    }
  }

  private static void setStock(BulkPriceUpdateRequestData data,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, Map<String, String> dataMap) {
    if (Objects.nonNull(data.getStock())) {
      dataMap.put(BulkParameters.STOCK_HEADER, String.valueOf(data.getStock()));
    } else {
      if (Objects.nonNull(itemPickupPointListingL3Response)) {
        dataMap.put(BulkParameters.STOCK_HEADER,
            String.valueOf(Optional.ofNullable(itemPickupPointListingL3Response.getAvailableStockLevel2()).orElse(0)));
      } else {
        dataMap.put(BulkParameters.STOCK_HEADER, Constant.ZERO_DECIMAL_STRING);
      }
    }
  }

  private static void setDataFromExcelOrExistingResponse(String data, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      Map<String, String> dataMap, String header, String existingValue) {
    if (StringUtils.isEmpty(data)) {
      if (Objects.nonNull(itemPickupPointListingL3Response)) {
        dataMap.put(header, existingValue);
      }
    } else {
      dataMap.put(header, data);
    }
  }

  private static void setDeliveryFlag(BulkPriceUpdateRequestData data, Map<String, String> dataMap,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    if (Objects.isNull(data.getDelivery())) {
      if (Objects.isNull(itemPickupPointListingL3Response)) {
        dataMap.put(BulkParameters.DELIVERY_STATUS_HEADER, Constant.ZERO_DECIMAL_STRING);
      } else {
        ProductLevel3ViewConfigResponse defaultViewConfig =
            getDefaultChannelItemViewConfig(itemPickupPointListingL3Response);
        if (defaultViewConfig.getBuyable() || defaultViewConfig.getDisplay()) {
          dataMap.put(BulkParameters.DELIVERY_STATUS_HEADER, Constant.ONE_DECIMAL_STRING);
        } else {
          dataMap.put(BulkParameters.DELIVERY_STATUS_HEADER, Constant.ZERO_DECIMAL_STRING);
        }
      }
    } else {
      dataMap.put(BulkParameters.DELIVERY_STATUS_HEADER, String.valueOf(Double.valueOf(data.getDelivery())));
    }
  }

  private static void setCncFlag(BulkPriceUpdateRequestData data,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, ProfileResponse profileResponse,
      Set<String> cncPickupPointCodes, Map<String, String> dataMap) {
    if (Objects.nonNull(profileResponse) && profileResponse.getCompany().isCncActivated()
        && cncPickupPointCodes.contains(data.getPickupPointCode()) && Objects.nonNull(data.getCnc())) {
      dataMap.put(BulkParameters.CNC_STATUS_HEADER, String.valueOf(Double.valueOf(data.getCnc())));
    } else {
      if (Objects.nonNull(itemPickupPointListingL3Response)) {
        ProductLevel3ViewConfigResponse cncViewConfig = getCncChannelItemViewConfig(itemPickupPointListingL3Response);
        if (Objects.isNull(cncViewConfig)) {
          cncViewConfig = new ProductLevel3ViewConfigResponse();
          cncViewConfig.setChannelId(Constant.CNC);
          cncViewConfig.setBuyable(false);
          cncViewConfig.setDisplay(false);
        }
        if (cncViewConfig.getBuyable() || cncViewConfig.getDisplay()) {
          dataMap.put(BulkParameters.CNC_STATUS_HEADER, Constant.ONE_DECIMAL_STRING);
        } else {
          dataMap.put(BulkParameters.CNC_STATUS_HEADER, Constant.ZERO_DECIMAL_STRING);
        }
      } else {
        dataMap.put(BulkParameters.CNC_STATUS_HEADER, Constant.ZERO_DECIMAL_STRING);
      }
    }
  }

  private static void setInStore(BulkPriceUpdateRequestData data,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, Map<String, String> dataMap) {
    if (Objects.nonNull(data.getInstore())) {
      dataMap.put(BulkParameters.IN_STORE_HEADER, String.valueOf(data.getInstore()));
    } else {
      if (Objects.nonNull(itemPickupPointListingL3Response)) {
        dataMap.put(BulkParameters.IN_STORE_HEADER, itemPickupPointListingL3Response.isOff2OnActiveFlag() ?
            Constant.ONE_DECIMAL_STRING :
            Constant.ZERO_DECIMAL_STRING);
      }
      dataMap.put(BulkParameters.IN_STORE_HEADER, Constant.ZERO_DECIMAL_STRING);
    }
  }

  private static void setStatus(BulkPriceUpdateRequestData data,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, Map<String, String> dataMap) {
    if (Objects.nonNull(data.getStatus())) {
      dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, String.valueOf(Double.valueOf(data.getStatus())));
    } else {
      if (Objects.nonNull(itemPickupPointListingL3Response)) {
        List<ProductLevel3ViewConfigResponse> viewConfigs = itemPickupPointListingL3Response.getViewConfigs();
        if (CollectionUtils.isNotEmpty(viewConfigs)) {
          ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
              getDefaultChannelItemViewConfig(itemPickupPointListingL3Response);
          ProductLevel3ViewConfigResponse productLevel3CncViewConfigResponse =
              getCncChannelItemViewConfig(itemPickupPointListingL3Response);
          if (Objects.isNull(productLevel3CncViewConfigResponse)) {
            productLevel3CncViewConfigResponse = new ProductLevel3ViewConfigResponse();
            productLevel3CncViewConfigResponse.setChannelId(Constant.CNC);
            productLevel3CncViewConfigResponse.setBuyable(false);
            productLevel3CncViewConfigResponse.setDisplay(false);
          }
          String defaultViewConfig = getViewConfigString(productLevel3ViewConfigResponse.getBuyable(),
              productLevel3ViewConfigResponse.getDisplay());
          String cncViewConfig = getViewConfigString(productLevel3CncViewConfigResponse.getBuyable(),
              productLevel3CncViewConfigResponse.getDisplay());

          if (!StringUtils.equals(defaultViewConfig, cncViewConfig)) {
            // If Default and Cnc are different, at least 1 should be offline. Otherwise CNC will be made offline
            if (!Constant.ZERO_DECIMAL_STRING.equals(defaultViewConfig) && !Constant.ZERO_DECIMAL_STRING.equals(
                cncViewConfig)) {
              cncViewConfig = Constant.ZERO_DECIMAL_STRING;
            }
            if (StringUtils.equals(cncViewConfig, Constant.ZERO_DECIMAL_STRING)) {
              dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, defaultViewConfig);
            } else {
              dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, cncViewConfig);
            }
          } else {
            // If Default and Cnc are same, column can have any one value
            dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, defaultViewConfig);
          }
        } else {
          dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, Constant.ZERO_DECIMAL_STRING);
        }
      } else {
        dataMap.put(BulkParameters.AMPHI_SKU_STATUS_NEW, Constant.ZERO_DECIMAL_STRING);
      }
    }
  }

  private static String getViewConfigString(boolean buyable, boolean display) {
    if (buyable && display) {
      return Constant.ONE_DECIMAL_STRING;
    } else if (buyable) {
      return Constant.THREE_DECIMAL_STRING;
    } else if (display) {
      return Constant.TWO_DECIMAL_STRING;
    } else {
      return Constant.ZERO_DECIMAL_STRING;
    }
  }

  public static Map<String, Boolean> getDefaultPrivilegeMap() {
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    return privilegedMap;
  }

  public static List<UpdateRemoveProductTaggingRequest> toUpdateRemoveProductTaggingRequest(
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests) {
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequests = new ArrayList<>();
    for(BulkPriceProductTypeTaggingRequest bulkPriceProductTypeTaggingRequest :
      bulkPriceProductTypeTaggingRequests){
      updateRemoveProductTaggingRequests.add(UpdateRemoveProductTaggingRequest.builder()
        .productType(bulkPriceProductTypeTaggingRequest.getProductTypeTagging()).itemPickUpPointId(
          bulkPriceProductTypeTaggingRequest.getItemSku().concat(Constant.HYPHEN)
            .concat(bulkPriceProductTypeTaggingRequest.getPickupPointCode())).build());
    }
    return updateRemoveProductTaggingRequests;
  }

  public static GenericTemplateDataReadDTO getGenericTemplateDataReadDTO(MerchantStatusType merchantStatusType,
      byte[] fileByteData, String merchantType, BulkProcess bulkProcess) {
    GenericTemplateDataReadDTO genericTemplateDataReadDTO = new GenericTemplateDataReadDTO();
    genericTemplateDataReadDTO.setFileByteData(fileByteData);
    genericTemplateDataReadDTO.setInternationalMerchant(bulkProcess.getInternationalMerchant());
    genericTemplateDataReadDTO.setMerchantType(merchantType);
    genericTemplateDataReadDTO.setMerchantStatusType(merchantStatusType);
    genericTemplateDataReadDTO.setExcelFileName(bulkProcess.getUploadedFile());
    return genericTemplateDataReadDTO;
  }

  public static Boolean getWaitingDeletion(boolean setWaitingDeletionForDeletePickupPoint) {
    return setWaitingDeletionForDeletePickupPoint ? false : null;
  }

  public static SkuRebateUpdateRequest toSkuRebateUpdateRequest(BulkSkuLevelRebateRequestData bulkSkuLevelRebateRequestData) {
    return SkuRebateUpdateRequest.builder()
        .itemSku(bulkSkuLevelRebateRequestData.getItemSku())
        .pickupPointCode(bulkSkuLevelRebateRequestData.getPickupPointCode())
        .rebate(Double.parseDouble(bulkSkuLevelRebateRequestData.getRebate()))
        .build();
  }

  public static void removeL5sWithStockToPreventDeletion(List<DeleteOfflineItemRequest> requests,
    List<DeleteOfflineItemDetailResponse> responses) {
    Set<String> l5IdsWithStock = responses.stream()
      .map(response -> RequestHelper.toL5Id(response.getItemSku(), response.getPickupPointCode()))
      .collect(Collectors.toSet());

    // Remove matching requests based on response from inventory for L5's with stock
    requests.removeIf(request -> l5IdsWithStock.contains(
      RequestHelper.toL5Id(request.getItemSku(), request.getPickupPointCode())));
  }

  public static void setBasicInternalProcessDataDetailsForInternalBrandUpdate(String storeId, BulkInternalProcess bulkInternalProcess,
      String userName, BulkInternalProcessData bulkInternalProcessData, String productCode,
      boolean brandAuthorisedForUpdate, List<BulkInternalProcessData> bulkInternalProcessDataList,
      String brandCode) throws JsonProcessingException {
    RequestHelper.setBulkInternalProcessDataBasicDetails(storeId, userName, bulkInternalProcess,
        bulkInternalProcessData);
    bulkInternalProcessData.setNotes(bulkInternalProcess.getNotes());
    bulkInternalProcessData.setParentCode(productCode);
    bulkInternalProcessData.setProcessType(bulkInternalProcess.getProcessType());
    bulkInternalProcessData.setStatus(
        brandAuthorisedForUpdate ? ProcessStatus.PENDING.name() : ProcessStatus.FAILED.name());
    bulkInternalProcessData.setErrorMessage(
        brandAuthorisedForUpdate ? null : NO_AUTHORISATION_TO_UPDATE_THE_BRAND);
    bulkInternalProcessDataList.add(bulkInternalProcessData);
  }
}

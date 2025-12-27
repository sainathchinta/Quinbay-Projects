package com.gdn.mta.product.util;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.MessageEmailRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gdn.mta.domain.event.modal.PriceInfoDTO;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.data.domain.Page;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.CategoryCodeAndCategoryNameResponse;
import com.gda.mta.product.dto.DimensionRefreshRequest;
import com.gda.mta.product.dto.DiscountPriceRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.ImageQcHashCodeAndLocationPathRequest;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.ImageQcProcessingDto;
import com.gda.mta.product.dto.ItemNeedRevisionNotes;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedOfflineProductResponse;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductCollectionDTO;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ImageQcConfidenceDto;
import com.gda.mta.product.dto.response.ImageQcPredictionResponse;
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.KeywordRestrictionModelsResponse;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gda.mta.product.dto.response.ProductFilterResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductStatusDomainEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.CategoryHierarchyDTO;
import com.gdn.mta.product.entity.HalalProductHistory;
import com.gdn.mta.product.entity.ItemSkuToItemIdMapping;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import com.gdn.mta.product.entity.NeedCorrectionNotesDto;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.valueobject.BasicItemDetail;
import com.gdn.mta.product.valueobject.BasicProductDetail;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.dto.productlevel3.ForceReviewImageViolationResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryBulkUpdatePickupPointCodeRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTItemNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductItemDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductNotesDomainEventModel;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.enums.ProductStatus;
import com.gdn.x.product.rest.web.model.request.B2bFields;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BundleItemResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
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
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.google.common.collect.ImmutableMap;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConverterUtil {

  private static final String UPDATE = "update";
  private static final String NEW = "new";
  private static final Integer LIMIT_SPLIT = 2;
  private static final String ROOT = "/";
  private static final String RESIZED_DATA_NOT_FOUND = "Resized Images data is not found for: ";

  public static final String INVALID_DETECTION = "invalid-detection";
  private static final String SUSPENDED = "Suspended";

  private static final ObjectMapper objectMapper = new ObjectMapper();

  public static SolrReviewProductCollectionAddEventFields toScreeningSolrProductCollectionAddEvent(
      ProductCollection productCollection) {
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEvent =
        SolrReviewProductCollectionAddEventFields.builder().id(productCollection.getId())
            .storeId(productCollection.getStoreId()).productCode(productCollection.getProductCode())
            .productId(productCollection.getProductId()).productName(productCollection.getProductName())
            .brand(productCollection.getBrand()).businessPartnerCode(productCollection.getBusinessPartnerCode())
            .businessPartnerName(productCollection.getBusinessPartnerName())
            .updatedStepDate(productCollection.getUpdatedStepDate())
            .updatedDate(productCollection.getUpdatedDate()).createdDate(productCollection.getCreatedDate())
            .createdBy(productCollection.getCreatedBy()).updatedBy(productCollection.getUpdatedBy())
            .assignedTo(productCollection.getAssignedTo()).activated(productCollection.isActivated())
            .viewable(productCollection.isViewable()).resubmitCount(productCollection.getResubmitCount())
            .categoryCodes(new ArrayList<>()).categoryNames(new ArrayList<>())
            .submittedDate(productCollection.getSubmittedDate())
            .state(productCollection.getState())
            .brandApproved(BrandApprovalStatus.APPROVED.equals(productCollection.getBrandApprovalStatus()))
            .build();
    return solrReviewProductCollectionAddEvent;
  }

  public static SolrInputDocument toSolrInputDocument(
      SolrReviewProductCollectionAddEventFields solrProductCollectionAddEvent) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, solrProductCollectionAddEvent.getId());
    solrInputDocument.setField(SolrFieldNames.STORE_ID,
        solrProductCollectionAddEvent.getStoreId());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_ID,
        solrProductCollectionAddEvent.getProductId());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE,
        solrProductCollectionAddEvent.getProductCode());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
        solrProductCollectionAddEvent.getProductName());
    solrInputDocument.setField(SolrFieldNames.BRAND,
        solrProductCollectionAddEvent.getBrand());
    solrInputDocument.setField(SolrFieldNames.CATEGORY_CODES,
        solrProductCollectionAddEvent.getCategoryCodes());
    solrInputDocument.setField(SolrFieldNames.CATEGORY_NAMES,
        solrProductCollectionAddEvent.getCategoryNames());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrProductCollectionAddEvent.getBusinessPartnerCode());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_NAME,
        solrProductCollectionAddEvent.getBusinessPartnerName());
    solrInputDocument.setField(SolrFieldNames.UPDATED_STEP_DATE,
        solrProductCollectionAddEvent.getUpdatedStepDate());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        solrProductCollectionAddEvent.getUpdatedDate());
    solrInputDocument.setField(SolrFieldNames.CREATED_DATE,
        solrProductCollectionAddEvent.getCreatedDate());
    solrInputDocument.setField(SolrFieldNames.CREATED_BY,
        solrProductCollectionAddEvent.getCreatedBy());
    solrInputDocument.setField(SolrFieldNames.UPDATED_BY,
        solrProductCollectionAddEvent.getUpdatedBy());
    solrInputDocument.setField(SolrFieldNames.ASSIGNED_TO,
        solrProductCollectionAddEvent.getAssignedTo());
    solrInputDocument.setField(SolrFieldNames.ACTIVATED,
        solrProductCollectionAddEvent.isActivated());
    solrInputDocument.setField(SolrFieldNames.VIEWABLE,
        solrProductCollectionAddEvent.isViewable());
    solrInputDocument.setField(SolrFieldNames.RESUBMIT_COUNT,
        solrProductCollectionAddEvent.getResubmitCount());
    solrInputDocument.setField(SolrFieldNames.SUBMITTED_DATE,
        solrProductCollectionAddEvent.getSubmittedDate());
    solrInputDocument.setField(SolrFieldNames.STATE,
        solrProductCollectionAddEvent.getState());
    solrInputDocument.setField(SolrFieldNames.BRAND_APPROVED,
        solrProductCollectionAddEvent.isBrandApproved());
    return solrInputDocument;
  }

  public static ProductRevisionInfoResponse toProductRevisionInfoResponse(ProductHistory productHistory) {
    String[] notesSplitByDash = productHistory.getNotes().split(Constants.DASH_DELIMITER, LIMIT_SPLIT);
    ObjectMapper objectMapper = new ObjectMapper();
    ProductRevisionInfoResponse productRevisionInfoResponse = new ProductRevisionInfoResponse();
    productRevisionInfoResponse.setId(productHistory.getId());
    productRevisionInfoResponse.setStoreId(productHistory.getStoreId());
    productRevisionInfoResponse.setCorrectionReason(notesSplitByDash[0]);
    if (notesSplitByDash.length > 1) {
      productRevisionInfoResponse.setAdditionalNotes(notesSplitByDash[1]);
    }
    productRevisionInfoResponse.setCreatedBy(productHistory.getCreatedBy());
    productRevisionInfoResponse.setCreatedDate(productHistory.getCreatedDate());
    productRevisionInfoResponse.setUpdatedBy(productHistory.getUpdatedBy());
    productRevisionInfoResponse.setUpdatedDate(productHistory.getUpdatedDate());
    NeedRevisionNotes revisionNotes;
    if (Objects.nonNull(productHistory.getNeedCorrectionNotes())) {
      try {
        revisionNotes = objectMapper.readValue(productHistory.getNeedCorrectionNotes(), NeedRevisionNotes.class);
        BeanUtils.copyProperties(revisionNotes, productRevisionInfoResponse);
        if(Objects.isNull(productRevisionInfoResponse.getVendorNotes())){
          productRevisionInfoResponse.setVendorNotes(new ArrayList<>());
        }
      } catch (IOException e) {
        log.error("Error when converting revisionNotes : {}", productHistory.getProductId(), e);
      }
    }
    return productRevisionInfoResponse;
  }

  public static List<RejectedSkuProductResponse> convertRejectedSkuProductCollectionToRejectedSkuProductResponse(
      List<RejectedSkuProductCollection> rejectedSkuProductCollection) {
    List<RejectedSkuProductResponse> response = new ArrayList<>();
    for (RejectedSkuProductCollection rejectedSku : rejectedSkuProductCollection) {
      RejectedSkuProductResponse wrapper = new RejectedSkuProductResponse();
      BeanUtils.copyProperties(rejectedSku, wrapper);
      response.add(wrapper);
    }
    return response;
  }

  public static BulkDownloadProductLevel3Response constructItemL5ListingResponse(List<ItemLevel5Response> productDatas,Map<String, ProductLevel3Inventory> inventoryDatas){
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    for (ItemLevel5Response itemLevel5Response : productDatas) {
      String itemSkuPpCode =
        itemLevel5Response.getItemSku().concat(Constants.HYPHEN).concat(itemLevel5Response.getPickupPointCode());
      ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
      productLevel3SummaryResponse.setProductName(itemLevel5Response.getProductName());
      productLevel3SummaryResponse.setItemSku(itemLevel5Response.getItemSku());
      productLevel3SummaryResponse.setProductSku(itemLevel5Response.getProductSku());
      productLevel3SummaryResponse.setItemName(itemLevel5Response.getGeneratedItemName());
      productLevel3SummaryResponse.setPickupPointCode(itemLevel5Response.getPickupPointCode());
      productLevel3SummaryResponse.setSkuCode(itemLevel5Response.getItemCode());
      productLevel3SummaryResponse.setMerchantSku(itemLevel5Response.getSellerSku());
      productLevel3SummaryResponse.setOff2OnActiveFlag(itemLevel5Response.getOff2OnChannelActive());
      productLevel3SummaryResponse.setCncActivated(itemLevel5Response.getCncActivated());
      productLevel3SummaryResponse.setCncActive(itemLevel5Response.getCncActive());
      if (Objects.nonNull(itemLevel5Response.getProductType())) {
        productLevel3SummaryResponse.setProductType(itemLevel5Response.getProductType().getCode());
      }
      productLevel3SummaryResponse.setSellerSku(itemLevel5Response.getSellerSku());
      productLevel3SummaryResponse.setProductCode(itemLevel5Response.getProductCode());
      if ((inventoryDatas.containsKey(itemSkuPpCode)) || inventoryDatas
        .containsKey(itemLevel5Response.getItemSku()))
     {
       ProductLevel3Inventory inventory = new ProductLevel3Inventory();
       if (inventoryDatas.containsKey(itemSkuPpCode)) {
         inventory = inventoryDatas.get(itemSkuPpCode);
       } else {
         inventory = inventoryDatas.get(itemLevel5Response.getItemSku());
       }
        productLevel3SummaryResponse.setAvailableStockLevel1(inventory.getWarehouseAvailable());
        productLevel3SummaryResponse.setReservedStockLevel1(inventory.getWarehouseReserved());
        productLevel3SummaryResponse.setAvailableStockLevel2(inventory.getWebAvailable());
        productLevel3SummaryResponse.setReservedStockLevel2(inventory.getWebReserved());
        productLevel3SummaryResponse.setMinimumStockLevel2(inventory.getWebMinAlert());
        productLevel3SummaryResponse.setSynchronizeStock(inventory.isWebSyncStock());
        productLevel3SummaryResponse.setNonDistributionAvailable(inventory.getNonDistributionAvailable());
        productLevel3SummaryResponse.setNonDistributionReserved(inventory.getNonDistributionReserved());
      }
      List<ProductLevel3PriceResponse> productLevel3PriceResponses = new ArrayList<>();
      for(PriceResponse priceResponse : itemLevel5Response.getPrices())
      {
        ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
        productLevel3PriceResponse.setPrice(priceResponse.getPrice());
        productLevel3PriceResponse.setSalePrice(priceResponse.getSalePrice());
        productLevel3PriceResponses.add(productLevel3PriceResponse);
      }
      productLevel3SummaryResponse.setPrices(productLevel3PriceResponses);
      productLevel3SummaryResponse.setB2BResponse(itemLevel5Response.getB2BResponse());
      List<ProductLevel3ViewConfigResponse> productLevel3ViewConfigResponses = new ArrayList<>();
      for(ViewConfigResponse viewConfigResponse : itemLevel5Response.getViewConfigs()){
        ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
        productLevel3ViewConfigResponse.setBuyable(viewConfigResponse.isBuyable());
        productLevel3ViewConfigResponse.setDisplay(viewConfigResponse.isDisplay());
        productLevel3ViewConfigResponse.setChannelId(viewConfigResponse.getChannelId());
        productLevel3ViewConfigResponses.add(productLevel3ViewConfigResponse);
      }
      productLevel3SummaryResponse.setViewConfigs(productLevel3ViewConfigResponses);
      if (Objects.nonNull(itemLevel5Response.getPreOrder())) {
        productLevel3SummaryResponse.setPreOrder(
            Optional.ofNullable(itemLevel5Response.getPreOrder().getIsPreOrder()).orElse(false));
        productLevel3SummaryResponse.setPreOrderDate(itemLevel5Response.getPreOrder().getPreOrderDate());
      }
      productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    }
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(productLevel3SummaryResponses);
    return bulkDownloadProductLevel3Response;
  }

  public static List<ItemSummaryDetailResponse> convertToItemSummaryDetailResponse(
      List<ItemLevel5Response> itemLevel5ResponseList, ProductType productType) {
    List<ItemSummaryDetailResponse> itemSummaryDetailResponseList = new ArrayList<>();
    for (ItemLevel5Response itemLevel5Response : itemLevel5ResponseList) {
      ItemSummaryDetailResponse itemSummaryDetailResponse = new ItemSummaryDetailResponse();
      itemSummaryDetailResponse.setProductSku(itemLevel5Response.getProductSku());
      itemSummaryDetailResponse.setItemSku(itemLevel5Response.getItemSku());
      itemSummaryDetailResponse.setPickupPointCode(itemLevel5Response.getPickupPointCode());
      itemSummaryDetailResponse.setMerchantSku(itemLevel5Response.getSellerSku());
      itemSummaryDetailResponse.setProductType(productType);
      PriceDTO priceDTO = new PriceDTO();
      priceDTO.setListPrice(itemLevel5Response.getPrices().get(0).getPrice());
      priceDTO.setOfferPrice(itemLevel5Response.getPrices().get(0).getSalePrice());
      Set<ItemViewConfigDTO> itemViewConfigDTOS = generateItemViewConfigDTOSet(itemLevel5Response.getViewConfigs());
      itemSummaryDetailResponse.setItemViewConfigs(itemViewConfigDTOS);
      itemSummaryDetailResponse.setPrice(Collections.singleton(priceDTO));
      itemSummaryDetailResponse.setOriginalPrice(itemLevel5Response.getOriginalSellingPrice());
      itemSummaryDetailResponse.setBundleRecipe(toBundleRecipeVoSet(itemLevel5Response.getBundleItemResponses()));
      itemSummaryDetailResponse.setDistribution(itemLevel5Response.isDistribution());
      itemSummaryDetailResponseList.add(itemSummaryDetailResponse);
    }
    return itemSummaryDetailResponseList;
  }

  private static Set<ItemViewConfigDTO> generateItemViewConfigDTOSet(List<ViewConfigResponse> viewConfigs) {
    Set<ItemViewConfigDTO> itemViewConfigDTOS = new HashSet<>();
    for (ViewConfigResponse viewConfig : viewConfigs) {
      ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
      itemViewConfigDTO.setChannel(viewConfig.getChannelId());
      itemViewConfigDTO.setBuyable(viewConfig.isBuyable());
      itemViewConfigDTO.setDiscoverable(viewConfig.isDisplay());
      itemViewConfigDTOS.add(itemViewConfigDTO);
    }
    return itemViewConfigDTOS;
  }

  private static Set<BundleRecipeVo> toBundleRecipeVoSet(List<BundleItemResponse> bundleItemResponses) {
    return Optional.ofNullable(bundleItemResponses).orElse(new ArrayList<>()).stream().map(ConverterUtil::toBundleRecipeVo)
        .collect(Collectors.toSet());
  }

  private static BundleRecipeVo toBundleRecipeVo(BundleItemResponse bundleItemResponse) {
    BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
    BeanUtils.copyProperties(bundleItemResponse, bundleRecipeVo);
    return bundleRecipeVo;
  }

  public static ProductItemAttributeValue convertProductItemAttributeValueRequestToProductItemAttributeValue(
      ProductItemAttributeValueRequest productItemAttributeValueRequest) {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue(productItemAttributeValueRequest.getValue());
    Attribute attribute = new Attribute();
    BeanUtils.copyProperties(productItemAttributeValueRequest.getAttribute(), attribute);
    productItemAttributeValue.setAttribute(attribute);
    return productItemAttributeValue;
  }

  public static BulkResizeImageRequest toBulkResizeImageRequest(String productCode, Map<String, CommonImagePathDto> uniqueImages,
      CustomGraphicsSettings customGraphicsSettings, String imageSourceDirectory) {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    bulkResizeImageRequest.setCustomGraphicsSettings(customGraphicsSettings);
    bulkResizeImageRequest.setGroupCode(productCode);
    bulkResizeImageRequest.setImageRequests(
        uniqueImages.entrySet().stream().map(image -> createImageRequest(image, imageSourceDirectory)).collect(Collectors.toList()));
    return bulkResizeImageRequest;
  }

  private static ImageRequest createImageRequest(Map.Entry<String, CommonImagePathDto> uniqueImage, String imageSourceDirectory) {
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode(uniqueImage.getKey());
    imageRequest.setAbsoluteImagePath(imageSourceDirectory + ROOT + uniqueImage.getValue().getLocationPath());
    String[] splitImageFilenameByDash = uniqueImage.getValue().getLocationPath().split(ROOT);
    imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
    imageRequest.setCommonImage(uniqueImage.getValue().isCommonImage());
    return imageRequest;
  }

  public static ImageQcRequestDomainEvent toImageQcRequest(BulkImageProcessResponse bulkImageProcessResponse,
      String imageSourceDirectory, ProductDetailResponse productDetailResponse, String gcsImagePathPrefix,
      String gcsSourceLocation, RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    imageQcRequestDomainEvent.setProductCode(bulkImageProcessResponse.getGroupCode());
    imageQcRequestDomainEvent.setImages(bulkImageProcessResponse.getImageResponses().stream().map(
        imageResponse -> new ImageQcHashCodeAndLocationPathRequest(imageResponse.getHashCode(),
            fullSourceLocation(imageSourceDirectory, imageResponse.getImagePathLocation(), gcsImagePathPrefix,
                gcsSourceLocation))).collect(Collectors.toList()));
    imageQcRequestDomainEvent.setBrand(productDetailResponse.getBrand());
    imageQcRequestDomainEvent.setProductName(productDetailResponse.getName());
    imageQcRequestDomainEvent.setDescription(new String(productDetailResponse.getDescription()));
    imageQcRequestDomainEvent.setUsp(productDetailResponse.getUniqueSellingPoint());
    updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent, productDetailResponse.getCategoryCodes(),
        productDetailResponse.getCategories());
    getRestrictedKeywordRequest(restrictedKeywordsByFieldAndActionType, imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  private static void getRestrictedKeywordRequest(RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      ImageQcRequestDomainEvent imageQcRequestDomainEvent) {
    List<KeywordRequestDTO> keywordRequestDTOList = new ArrayList<>();
    if (Objects.nonNull(restrictedKeywordsByFieldAndActionType)) {
      if (CollectionUtils.isNotEmpty(restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList())) {
        restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList().stream()
            .filter(restrictedKeywordsByField -> CollectionUtils.isNotEmpty(restrictedKeywordsByField.getKeywords()))
            .forEach(restrictedKeywordsByField -> {
              restrictedKeywordsByField.getKeywords().stream()
                  .map(keyword -> getKeywordRequestDTO(restrictedKeywordsByFieldAndActionType, keyword))
                  .filter(Objects::nonNull).forEach(keywordRequestDTOList::add);
            });
      }
    }
    imageQcRequestDomainEvent.setRestrictedKeywords(keywordRequestDTOList);
  }

  private static KeywordRequestDTO getKeywordRequestDTO(
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, String keyword) {
    KeywordRequestDTO keywordRequestDTO = null;
    if (
      MapUtils.isNotEmpty(restrictedKeywordsByFieldAndActionType.getKeywordToKeywordRequestDTOMap())
        && restrictedKeywordsByFieldAndActionType.getKeywordToKeywordRequestDTOMap()
        .containsKey(keyword.toLowerCase())) {
      KeywordRequestDTO value =
        restrictedKeywordsByFieldAndActionType.getKeywordToKeywordRequestDTOMap()
          .get(keyword.toLowerCase());
      keywordRequestDTO = new KeywordRequestDTO();
      keywordRequestDTO.setKeyword(keyword);
      keywordRequestDTO.setKeywordId(value.getKeywordId());
      keywordRequestDTO.setKeywordAction(value.getKeywordAction());
      keywordRequestDTO.setKeywordType(value.getKeywordType());
      keywordRequestDTO.setValidateByDs(value.isValidateByDs());
    }
    return keywordRequestDTO;
  }

  public static void updateCategoryInfoInImageQcRequest(ImageQcRequestDomainEvent imageQcRequestDomainEvent,
      List<String> categoryCodes, List<String> categoryNames) {
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      imageQcRequestDomainEvent.setCategoryCode(categoryCodes.get(categoryCodes.size() - 1));
    }
    List<CategoryHierarchyDTO> categoryHierarchyDTOs = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(categoryCodes) && CollectionUtils.isNotEmpty(categoryNames)
        && categoryCodes.size() == categoryNames.size()) {
      for (int categoryIndex = 0; categoryIndex < categoryCodes.size() ; categoryIndex++) {
        CategoryHierarchyDTO categoryHierarchyDTO = new CategoryHierarchyDTO();
        categoryHierarchyDTO.setCategoryCode(categoryCodes.get(categoryIndex));
        categoryHierarchyDTO.setCategoryName(categoryNames.get(categoryIndex));
        categoryHierarchyDTO.setHierarchy(categoryIndex +1);
        categoryHierarchyDTOs.add(categoryHierarchyDTO);
      }
    }
    imageQcRequestDomainEvent.setCategoryHierarchy(categoryHierarchyDTOs);
  }

  public static ImageQcProcessedResponseDomainEvent toImageQcProcessedResponseDomainEventFromProcessedResponse(
      ImageQcProcessedResponse imageQcProcessedResponse) {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    BeanUtils.copyProperties(imageQcProcessedResponse, imageQcProcessedResponseDomainEvent);
    imageQcProcessedResponseDomainEvent.setForceReview(imageQcProcessedResponse.isForceReview());
    return imageQcProcessedResponseDomainEvent;
  }

  public static ProductItemImageRequest toUpdatedProductItemImageRequestFromL5Request(
      List<ProductLevel3SummaryDetailsImageRequest> images, String skuCode) {
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemsImages = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : images) {
      if (UPDATE.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        Image image = new Image();
        image.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
        image.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
        image.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
        image.setSequence(productLevel3SummaryDetailsImageRequest.getSequence());
        itemsImages.add(image);
      }
    }
    productItemImageRequest.setItemImages(itemsImages);
    productItemImageRequest.setSkuCode(skuCode);
    return productItemImageRequest;
  }

  public static List<Image> copyToItemImagesL5(List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages,
      boolean needCorrection) {
    List<Image> itemsImages = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : copyToAllVariantImages) {
      Image image = new Image();
      image.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
      image.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
      image.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
      image.setSequence(productLevel3SummaryDetailsImageRequest.getSequence());
      image.setCreatedBy(Constants.SYSTEM);
      image.setStoreId(Constants.DEFAULT_STORE_ID);
      image.setCreatedDate(new Date());
      image.setCommonImage(true);
      image.setUpdatedBy(Constants.SYSTEM);
      if (NEW.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        image.setHashCode(ApproveProductUtils
            .generateHashcodeByLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath()));
        image.setOriginalImage(Boolean.TRUE);
        if (needCorrection) {
          image.setRevised(Boolean.TRUE);
          image.setEdited(Boolean.FALSE);
          image.setActive(Boolean.FALSE);
        } else {
          image.setEdited(Boolean.TRUE);
          image.setRevised(Boolean.FALSE);
          image.setActive(Boolean.TRUE);
        }
      } else {
        image.setHashCode(null);
        image.setOriginalImage(Boolean.FALSE);
        image.setActive(Boolean.TRUE);
      }
      itemsImages.add(image);
    }
    return itemsImages;
  }

  public static ProductAndItemImageRequest toProductAndItemImageRequest(
      BulkImageProcessResponse bulkImageProcessResponse, ProductDetailResponse productDetailResponse)
      throws NoSuchAlgorithmException {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    MessageDigest messageDigest = MessageDigest.getInstance("MD5");
    Map<String, CommonImagePathDto> hashCodeLocationPathMap =
        bulkImageProcessResponse.getImageResponses().stream().filter(imageResponse -> imageResponse.isSuccess())
            .collect(Collectors.toMap(ImageResponse::getHashCode,
                imageResponse -> new CommonImagePathDto(imageResponse.getImagePathLocation(),
                    imageResponse.isCommonImage()), (image1, image2) -> image1));
    productAndItemImageRequest.setProductCode(productDetailResponse.getProductCode());
    List<ProductItemImageRequest> productItemImageRequests = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
      productItemImageRequest.setSkuCode(productItemResponse.getSkuCode());
      productItemImageRequest.setItemImages(
          productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
              .map(image -> getResizedImageRequests(image, hashCodeLocationPathMap, messageDigest))
              .collect(Collectors.toList()));
      productItemImageRequests.add(productItemImageRequest);
    }
    productAndItemImageRequest.setProductItemImages(productItemImageRequests);
    productAndItemImageRequest.setProductImages(
        productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
            .map(image -> getResizedImageRequests(image, hashCodeLocationPathMap, messageDigest))
            .collect(Collectors.toList()));
    return productAndItemImageRequest;
  }

  public static ProductStatusDomainEvent toProductStatusDomainEvent(ProductDetailResponse productDetailResponse,
      ProductStatusDomainEvent productStatusDomainEvent, BasicProductDetail basicProductDetail,
      Map<String, ProductItemBusinessPartner> productItemIdToItemMap) {
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      BasicItemDetail basicItemDetail = new BasicItemDetail();
      ProductItemBusinessPartner productItemBusinessPartner = productItemIdToItemMap.get(productItemResponse.getId());
      if (Objects.nonNull(productItemBusinessPartner)) {
        basicItemDetail.setMerchantSku(productItemBusinessPartner.getMerchantSku());
        basicItemDetail.setProductItemSku(productItemBusinessPartner.getGdnProductItemSku());
      }
      basicItemDetail.setGeneratedItemName(productItemResponse.getGeneratedItemName());
      basicItemDetail.setProductItemCode(productItemResponse.getSkuCode());
      basicItemDetail.setUpcCode(productItemResponse.getUpcCode());
      basicProductDetail.getProductItems().add(basicItemDetail);
    }
    productStatusDomainEvent.setProduct(basicProductDetail);
    return productStatusDomainEvent;
  }

  private static Image getResizedImageRequests(Image image, Map<String, CommonImagePathDto> hashCodeLocationPathMap,
      MessageDigest messageDigest) {
    Image resizedImage = new Image();
    CommonImagePathDto commonImagePathDto = hashCodeLocationPathMap.get(image.getHashCode());
    resizedImage.setOriginalImage(Boolean.FALSE);
    resizedImage.setLocationPath(commonImagePathDto.getLocationPath());
    resizedImage.setMainImages(image.isMainImages());
    resizedImage.setStoreId(image.getStoreId());
    resizedImage.setSequence(image.getSequence());
    resizedImage.setCommonImage(image.isCommonImage());
    messageDigest.update(resizedImage.getLocationPath().getBytes());
    resizedImage.setHashCode(ApproveProductUtils.generateHashcode(messageDigest));
    return resizedImage;
  }

  private static Image getResizedImageRequestsEdited(Image image, Map<String, CommonImagePathDto> hashCodeLocationPathMap,
      MessageDigest messageDigest, boolean isRevised) {
    Image resizedImage = getResizedImageRequests(image, hashCodeLocationPathMap, messageDigest);
    if (isRevised) {
      resizedImage.setRevised(true);
    } else {
      resizedImage.setEdited(true);
    }
    return resizedImage;
  }

  public static ProductAndItemImageRequest toProductAndItemImageRequestOnImageResizingFailure(
      ProductDetailResponse productDetailResponse) {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(productDetailResponse.getProductCode());
    List<ProductItemImageRequest> productItemImageRequests = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
      productItemImageRequest.setSkuCode(productItemResponse.getSkuCode());
      productItemImageRequest.setItemImages(
          productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
              .map(image -> getResizedImageRequestsOnResizingFailure(image)).collect(Collectors.toList()));
      productItemImageRequests.add(productItemImageRequest);
    }
    productAndItemImageRequest.setProductItemImages(productItemImageRequests);
    productAndItemImageRequest.setProductImages(
        productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
            .map(image -> getResizedImageRequestsOnResizingFailure(image)).collect(Collectors.toList()));
    return productAndItemImageRequest;
  }

  private static Image getResizedImageRequestsOnResizingFailure(Image image) {
    Image resizedImage = new Image();
    resizedImage.setOriginalImage(Boolean.FALSE);
    resizedImage.setLocationPath(image.getLocationPath());
    resizedImage.setMainImages(image.isMainImages());
    resizedImage.setStoreId(image.getStoreId());
    resizedImage.setSequence(image.getSequence());
    resizedImage.setHashCode(image.getHashCode());
    return resizedImage;
  }

  public static AutoApprovalRulesListResponse toAutoApprovalRulesListResponse(List<AutoApprovalRules> autoApprovalRules,
      List<ProductImagePredictionResponse> productImagePredictionResponseList) throws JsonProcessingException {
    AutoApprovalRulesListResponse autoApprovalRulesListResponse = new AutoApprovalRulesListResponse();
    List<AutoApprovalRulesDto> autoApprovalRulesDtoList = toAutoApprovalRulesDtoList(autoApprovalRules,
        productImagePredictionResponseList);
    autoApprovalRulesListResponse.setAutoApprovalRulesDtoList(autoApprovalRulesDtoList);
    BeanUtils.copyProperties(autoApprovalRules.get(0), autoApprovalRulesListResponse, "autoApprovalRulesDtoList");
    return autoApprovalRulesListResponse;
  }

  private static List<AutoApprovalRulesDto> toAutoApprovalRulesDtoList(List<AutoApprovalRules> autoApprovalRules,
      List<ProductImagePredictionResponse> productImagePredictionResponseList) throws JsonProcessingException {
    Map<String, ProductImagePredictionResponse> predictionTypeToImagePredictionResponseMap = new HashMap<>();
    getProductImagePredictionMap(productImagePredictionResponseList, predictionTypeToImagePredictionResponseMap);
    List<AutoApprovalRulesDto> autoApprovalRulesDtoList = new ArrayList<>();
    ObjectMapper objectMapper = new ObjectMapper();
    for (AutoApprovalRules autoApprovalRule : autoApprovalRules) {
      AutoApprovalRulesDto autoApprovalRulesDto = new AutoApprovalRulesDto();
      autoApprovalRulesDto.setRuleName(autoApprovalRule.getRuleName());
      autoApprovalRulesDto.setAutoApprovalType(autoApprovalRule.getAutoApprovalType());
      autoApprovalRulesDto.setSequenceNumber(autoApprovalRule.getSequenceNumber());
      autoApprovalRulesDto.setMarkForDelete(autoApprovalRule.isMarkForDelete());
      autoApprovalRulesDto.setNeedRevisionConfigEnabled(autoApprovalRule.isNeedRevisionEnabled());
      List<AutoApprovalRuleDetailsDto> ruleConfig = objectMapper
          .readValue(autoApprovalRule.getRuleConfig(), new TypeReference<List<AutoApprovalRuleDetailsDto>>() {
          });
      List<AutoApprovalRuleDetailsDto> imageQcConfig = objectMapper
          .readValue(autoApprovalRule.getImageQcConfig(), new TypeReference<List<AutoApprovalRuleDetailsDto>>() {
          });
      filterImagePredictions(predictionTypeToImagePredictionResponseMap, imageQcConfig);
      List<AutoApprovalRuleDetailsDto> imageQcConfigNeedRevision = objectMapper
          .readValue(autoApprovalRule.getNeedRevisionConfig(), new TypeReference<List<AutoApprovalRuleDetailsDto>>() {
          });
      filterImagePredictions(predictionTypeToImagePredictionResponseMap, imageQcConfigNeedRevision);
      autoApprovalRulesDto.setRuleConfig(ruleConfig);
      autoApprovalRulesDto.setImageQcConfig(imageQcConfig);
      autoApprovalRulesDto.setNeedRevisionImageQcConfig(imageQcConfigNeedRevision);
      BeanUtils.copyProperties(autoApprovalRule, autoApprovalRulesDto, "ruleName", "ruleConfig", "imageQcConfig",
          "autoApprovalType");
      autoApprovalRulesDtoList.add(autoApprovalRulesDto);
    }
    return autoApprovalRulesDtoList;
  }

  private static void getProductImagePredictionMap(
      List<ProductImagePredictionResponse> productImagePredictionResponseList,
      Map<String, ProductImagePredictionResponse> productImagePredictionResponseMap) {
    for (ProductImagePredictionResponse productImagePredictionResponse : productImagePredictionResponseList) {
      productImagePredictionResponseMap.putIfAbsent(productImagePredictionResponse.getPredictionType(),
          productImagePredictionResponse);
    }
  }

  private static void filterImagePredictions(
      Map<String, ProductImagePredictionResponse> predictionTypeToImagePredictionResponseMap,
      List<AutoApprovalRuleDetailsDto> autoApprovalRuleDetailsDtoList) {
    Iterator<AutoApprovalRuleDetailsDto> iterator = autoApprovalRuleDetailsDtoList.iterator();
    while (iterator.hasNext()) {
      AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto = iterator.next();
      ProductImagePredictionResponse productImagePredictionResponse =
          predictionTypeToImagePredictionResponseMap.get(autoApprovalRuleDetailsDto.getKeyName());
      if (Objects.nonNull(productImagePredictionResponse) && !productImagePredictionResponse.isPredictionConsidered()) {
        iterator.remove();
      }
    }
  }

  public static List<ConfigurationStatusRequest> toConfigurationStatusRequestList(String bpCode, String categoryCode) {
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(bpCode).categoryCode(categoryCode).build();
    return Arrays.asList(configurationStatusRequest);
  }

  public static ProductFilterResponse toProductFilterResponse(SolrProductCollectionDTO solrProductCollectionDTO) {
    return ProductFilterResponse.builder().brandName(solrProductCollectionDTO.getBrand())
        .categoryName(solrProductCollectionDTO.getCategoryName()).productCode(solrProductCollectionDTO.getProductCode())
        .productName(solrProductCollectionDTO.getProductName()).build();
  }

  public static PriceRequest toPriceRequest(ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest) {
    PriceRequest priceRequest = new PriceRequest();
    BeanUtils.copyProperties(productPriceAndWholesaleRequest, priceRequest, "discountPrice");
    priceRequest.setDiscountPrice(
        Optional.ofNullable(productPriceAndWholesaleRequest.getDiscountPrice()).orElse(new ArrayList<>()).stream()
            .map(ConverterUtil::toDiscountPriceDTO).collect(Collectors.toList()));
    priceRequest.setWholesalePriceActivated(productPriceAndWholesaleRequest.getWholesalePriceActivated());
    return priceRequest;
  }

  private static DiscountPriceDTO toDiscountPriceDTO(DiscountPriceRequest discountPriceRequest) {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    BeanUtils.copyProperties(discountPriceRequest, discountPriceDTO);
    return discountPriceDTO;
  }

  public static WholesalePriceRequest toWholesalePriceRequest(
      ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest,
      ProductAndItemsResponse productAndItemsResponse) {
    WholesalePriceRequest wholesalePriceRequest = new WholesalePriceRequest();
    wholesalePriceRequest.setMerchantCode(productAndItemsResponse.getProduct().getMerchantCode());
    wholesalePriceRequest.setNewProduct(false);
    wholesalePriceRequest.setProductSku(productAndItemsResponse.getProduct().getProductSku());
    WholesalePriceSkuRequest wholesalePriceSkuRequest = new WholesalePriceSkuRequest();
    wholesalePriceSkuRequest.setItemCode(productAndItemsResponse.getItems().get(0).getItemCode());
    wholesalePriceSkuRequest.setItemSku(productAndItemsResponse.getItems().get(0).getItemSku());
    wholesalePriceSkuRequest.setPickUpPointCode(
      Optional.of(productAndItemsResponse).map(ProductAndItemsResponse::getItems)
        .map(itemResponses -> itemResponses.get(0).getPickupPointCode()).orElse(null));
    Map<Integer, Double> wholeSaleRulesMap =
        Optional.ofNullable(productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests())
            .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(ProductItemWholesalePriceRequest::getQuantity,
            ProductItemWholesalePriceRequest::getWholesaleDiscount));
    Map<Integer, Double> wholeSaleRulesMapSorted = new TreeMap<>(wholeSaleRulesMap);
    wholesalePriceSkuRequest.setWholesaleRules(wholeSaleRulesMapSorted);
    wholesalePriceRequest.setWholesalePriceSkuRequests(Collections.singletonList(wholesalePriceSkuRequest));
    return wholesalePriceRequest;
  }

  public static List<LogAuditTrailUpdatedOfflineProductResponse> toLogAuditTrailUpdatedOfflineProductResponses(
      Page<UpdatedProductHistory> updatedProductHistories) {
    DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
    return updatedProductHistories.getContent().stream().map(
        audit -> toLogAuditTrailUpdatedOfflineProductResponse(audit, dateFormat)).collect(
        Collectors.toList());
  }

  private static LogAuditTrailUpdatedOfflineProductResponse toLogAuditTrailUpdatedOfflineProductResponse(
      UpdatedProductHistory updatedProductHistory,
      DateFormat dateFormat) {
    LogAuditTrailUpdatedOfflineProductResponse response =
        new LogAuditTrailUpdatedOfflineProductResponse();
    response.setActivity(updatedProductHistory.getActivity());
    response.setCreatedByLog(updatedProductHistory.getChangedBy());
    response.setCreatedDateLog(
        dateFormat.format(updatedProductHistory.getAccessTime()));
    response.setOldValue(updatedProductHistory.getOldValues());
    response.setNewValue(updatedProductHistory.getNewValues());
    return response;
  }

  public static boolean getImageQcResponse(String productCode, ImageQcProcessedResponse imageQcProcessedResponse,
      ProductImageQcProcessingResponse response, List<String> listOfActivePredictionTypes) {
    ObjectMapper objectMapper = new ObjectMapper();
    if (Objects.nonNull(response)) {
      ImageQcResponseDomainEvent imageQcResponseDomainEvent = null;
      try {
        imageQcResponseDomainEvent =
            objectMapper.readValue(response.getImageQcResponse(), ImageQcResponseDomainEvent.class);
      } catch (IOException e) {
        log.error("Error when converting imageQc response : {} ", productCode, e);
        return false;
      }
      if (Objects.nonNull(imageQcResponseDomainEvent) && Objects.nonNull(imageQcResponseDomainEvent.getImages())) {
        for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
          List<ImageQcPredictionResponse> processedImageQcPredictionResponseList = new ArrayList<>();
          for (ImageQcPredictionResponse imageQcPredictionResponse : imageQcResponse.getPredictions()) {
            if (listOfActivePredictionTypes.contains(imageQcPredictionResponse.getPredictionType())) {
              ImageQcPredictionResponse processedImageQcPredictionResponse = new ImageQcPredictionResponse();
              BeanUtils.copyProperties(imageQcPredictionResponse, processedImageQcPredictionResponse);
              processedImageQcPredictionResponseList.add(processedImageQcPredictionResponse);
            }
          }
          imageQcResponse.setPredictions(processedImageQcPredictionResponseList);
        }
      }
      if (Objects.nonNull(imageQcResponseDomainEvent))
        try {
          response.setImageQcResponse(objectMapper.writeValueAsString(imageQcResponseDomainEvent));
        } catch (JsonProcessingException e) {
          log.error("Error when saving converted imageQc response : {}", productCode, e);
          return false;
        }
      BeanUtils.copyProperties(response, imageQcProcessedResponse);
      return true;
    }
    return false;
  }

  public static ProductRequest toProductRequest(ProductAndItemsResponse productAndItemsResponse) {
    ProductRequest productRequest = new ProductRequest();
    ProductResponse productResponse = productAndItemsResponse.getProduct();
    MasterDataProductDTO masterDataProductDTO = productResponse.getMasterDataProduct();
    productRequest.setStoreId(Constants.DEFAULT_STORE_ID);
    BeanUtils.copyProperties(masterDataProductDTO, productRequest);
    configureDefaultValues(productRequest);
    if (StringUtils.isNotBlank(masterDataProductDTO.getDescription())) {
      productRequest.setDescription(masterDataProductDTO.getDescription().getBytes());
    }
    if (StringUtils.isNotBlank(masterDataProductDTO.getLongDescription())) {
      productRequest.setLongDescription(masterDataProductDTO.getLongDescription().getBytes());
    }
    productRequest.setActivated(Boolean.TRUE);
    productRequest.setViewable(Boolean.TRUE);
    productRequest.setName(masterDataProductDTO.getProductName());
    copyProductAttributes(productRequest, masterDataProductDTO);
    copyProductItems(productAndItemsResponse, productRequest);
    copyProductCategory(productRequest, productResponse);
    copyProductImages(productRequest, masterDataProductDTO);
    return productRequest;
  }

  public static ProductDetailResponse toProductDetailResponse(ProductLevel3 request) {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(request.getProductName());
    productDetailResponse.setDescription(request.getDescription().getBytes(StandardCharsets.UTF_8));
    productDetailResponse.setUniqueSellingPoint(request.getUniqueSellingPoint());
    List<ProductLevel3Attribute> productLevel3AttributeList = request.getAttributes().stream()
        .filter(ConverterUtil::isDescriptiveAndNotVariantCreation)
        .collect(Collectors.toList());
    List<ProductAttributeResponse> productAttributeResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productLevel3AttributeList)) {
      for (ProductLevel3Attribute productLevel3Attribute : productLevel3AttributeList) {
        ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
        productAttributeResponse.setProductAttributeName(productLevel3Attribute.getAttributeName());
        AttributeResponse attributeResponse = new AttributeResponse();
        attributeResponse.setAttributeCode(productLevel3Attribute.getAttributeCode());
        attributeResponse.setAttributeType(productLevel3Attribute.getAttributeType());
        attributeResponse.setName(productLevel3Attribute.getAttributeName());
        attributeResponse.setSkuValue(productLevel3Attribute.getSkuValue());
        attributeResponse.setBasicView(productLevel3Attribute.isBasicView());
        productAttributeResponse.setAttribute(attributeResponse);
        List<ProductAttributeValueResponse> productAttributeValueResponseList = new ArrayList<>();
        for (String value : productLevel3Attribute.getValues()) {
          ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
          productAttributeValueResponse.setDescriptiveAttributeValue(value);
          productAttributeValueResponse
              .setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);
          productAttributeValueResponseList.add(productAttributeValueResponse);
        }
        productAttributeResponse.setProductAttributeValues(productAttributeValueResponseList);
        productAttributeResponseList.add(productAttributeResponse);
      }
    }
    productDetailResponse.setProductAttributeResponses(productAttributeResponseList);
    return productDetailResponse;
  }

  private static boolean isDescriptiveAndNotVariantCreation(ProductLevel3Attribute productLevel3Attribute) {
    return !productLevel3Attribute.isVariantCreation() && AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(productLevel3Attribute.getAttributeType());
  }

  private static void copyProductCategory(ProductRequest productRequest, ProductResponse productResponse) {
    if (Objects.nonNull(productResponse.getMasterCatalog())) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      CatalogRequest catalogRequest = new CatalogRequest();
      catalogRequest.setCatalogCode(productResponse.getMasterCatalog().getCatalogCode());
      categoryRequest.setCategoryCode(productResponse.getMasterCatalog().getCategory().getCategoryCode());
      categoryRequest.setCatalog(catalogRequest);
      productCategoryRequest.setCategory(categoryRequest);
      configureDefaultValues(productCategoryRequest);
      productRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    }
  }

  private static void copyProductItems(ProductAndItemsResponse productAndItemsResponse, ProductRequest productRequest) {
    if (CollectionUtils.isNotEmpty(productAndItemsResponse.getItems())) {
      List<ProductItemRequest> productItems = new ArrayList<>();
      for (ItemResponse itemResponse : productAndItemsResponse.getItems()) {
        ProductItemRequest productItemRequest = new ProductItemRequest();
        if (Objects.nonNull(itemResponse.getMasterDataItem())) {
          MasterDataItemDTO masterDataItem = itemResponse.getMasterDataItem();
          productItemRequest.setGeneratedItemName(masterDataItem.getGeneratedItemName());
          productItemRequest.setSkuCode(itemResponse.getItemCode());
          productItemRequest.setUpcCode(masterDataItem.getUpcCode());
          productItemRequest.setActivated(Boolean.TRUE);
          productItemRequest.setViewable(Boolean.TRUE);
          productRequest.setHeight(masterDataItem.getItemHeight());
          productRequest.setLength(masterDataItem.getItemLength());
          productRequest.setWeight(masterDataItem.getItemWeight());
          productRequest.setWidth(masterDataItem.getItemWidth());
          productRequest.setShippingWeight(masterDataItem.getItemDeliveryWeight());
          configureDefaultValues(productItemRequest);
          copyItemImages(productItemRequest, masterDataItem);
          copyItemAttributes(productItemRequest, masterDataItem);
        }
        productItems.add(productItemRequest);
      }
      productRequest.setProductItems(productItems);
    }
  }

  private static void copyItemAttributes(ProductItemRequest productItemRequest, MasterDataItemDTO masterDataItem) {
    List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValueDTOS =
        masterDataItem.getMasterDataItemAttributeValues();
    if (CollectionUtils.isNotEmpty(masterDataItemAttributeValueDTOS)) {
      List<ProductItemAttributeValueRequest> productItemAttributeValueRequests = new ArrayList<>();
      for (MasterDataItemAttributeValueDTO masterDataItemAttributeValueDTO : masterDataItemAttributeValueDTOS) {
        ProductItemAttributeValueRequest productItemAttributeValueRequest =
            new ProductItemAttributeValueRequest();
        configureDefaultValues(productItemAttributeValueRequest);
        productItemAttributeValueRequest.setValue(masterDataItemAttributeValueDTO.getAttributeValue());
        if (Objects.nonNull(masterDataItemAttributeValueDTO.getMasterDataAttribute())) {
          MasterDataAttributeDTO masterDataAttributeDTO =
              masterDataItemAttributeValueDTO.getMasterDataAttribute();
          AttributeRequest attribute = new AttributeRequest();
          BeanUtils.copyProperties(masterDataAttributeDTO, attribute);
          productItemAttributeValueRequest.setAttribute(attribute);
        }
        productItemAttributeValueRequests.add(productItemAttributeValueRequest);
      }
      productItemRequest.setProductItemAttributeValues(productItemAttributeValueRequests);
    }
  }

  private static void copyItemImages(ProductItemRequest productItemRequest, MasterDataItemDTO masterDataItem) {
    List<MasterDataItemImageDTO> masterDataItemImages = masterDataItem.getMasterDataItemImages();
    if (CollectionUtils.isNotEmpty(masterDataItemImages)) {
      List<Image> itemImages = new ArrayList<>();
      for (MasterDataItemImageDTO masterDataItemImageDTO : masterDataItemImages) {
        Image image = new Image();
        image.setLocationPath(masterDataItemImageDTO.getLocationPath());
        image.setSequence(masterDataItemImageDTO.getSequence());
        image.setMainImages(masterDataItemImageDTO.isMainImage());
        image.setActive(Boolean.TRUE);
        image.setOriginalImage(Boolean.FALSE);
        image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(masterDataItemImageDTO.getLocationPath()));
        image.setCreatedBy(Constants.SYSTEM);
        image.setStoreId(Constants.DEFAULT_STORE_ID);
        image.setCreatedDate(new Date());
        image.setUpdatedBy(Constants.SYSTEM);
        itemImages.add(image);
      }
      productItemRequest.setImages(itemImages);
    }
  }

  private static void copyProductAttributes(ProductRequest productRequest, MasterDataProductDTO masterDataProductDTO) {
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOS =
        masterDataProductDTO.getMasterDataProductAttributes();
    if (CollectionUtils.isNotEmpty(masterDataProductAttributeDTOS)) {
      List<ProductAttributeRequest> productAttributeRequests = new ArrayList<>();
      for (MasterDataProductAttributeDTO masterDataProductAttributeDTO : masterDataProductAttributeDTOS) {
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        productAttributeRequest.setStoreId(Constants.DEFAULT_STORE_ID);
        productAttributeRequest.setSequence(masterDataProductAttributeDTO.getSequence());
        productAttributeRequest.setOwnByProductItem(masterDataProductAttributeDTO.isOwnedByProductItem());
        configureDefaultValues(productAttributeRequest);
        MasterDataAttributeDTO masterDataAttributeDTO = masterDataProductAttributeDTO.getMasterDataAttribute();
        AttributeRequest attributeRequest = new AttributeRequest();
        if (Objects.nonNull(masterDataAttributeDTO)) {
          BeanUtils.copyProperties(masterDataAttributeDTO, attributeRequest);
          if ("PREDEFINED_ATTRIBUTE".equalsIgnoreCase(masterDataAttributeDTO.getAttributeType().name())) {
            attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
          } else if ("DESCRIPTIVE_ATTRIBUTE".equalsIgnoreCase(masterDataAttributeDTO.getAttributeType().name())) {
            attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
          } else {
            attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
          }
          productAttributeRequest.setAttribute(attributeRequest);
          copyProductAttributeValues(productAttributeRequest, masterDataProductAttributeDTO);
          productAttributeRequest.setProductAttributeName(masterDataAttributeDTO.getAttributeName());
        }
        productAttributeRequests.add(productAttributeRequest);
      }
      productRequest.setProductAttributes(productAttributeRequests);
    }
  }

  private static void copyProductAttributeValues(ProductAttributeRequest productAttributeRequest, MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS =
        masterDataProductAttributeDTO.getMasterDataProductAttributeValues();
    if (CollectionUtils.isNotEmpty(masterDataProductAttributeValueDTOS)) {
      List<ProductAttributeValueRequest> productAttributeValues = new ArrayList<>();
      for (MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO : masterDataProductAttributeValueDTOS) {
        ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
        productAttributeValueRequest
            .setDescriptiveAttributeValue(masterDataProductAttributeValueDTO.getDescriptiveAttributeValue());
        MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO =
            masterDataProductAttributeValueDTO.getAllowedAttributeValue();
        if (Objects.nonNull(masterDataAllowedAttributeValueDTO)) {
          AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
          BeanUtils.copyProperties(masterDataAllowedAttributeValueDTO, allowedAttributeValueRequest);
          allowedAttributeValueRequest.setAllowedAttributeCode(masterDataAllowedAttributeValueDTO.getAllowedAttributeValueCode());
          productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
        }

        PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValue =
            masterDataProductAttributeValueDTO.getPredefinedAllowedAttributeValue();
        if (Objects.nonNull(predefinedAllowedAttributeValue)) {
          PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
              new PredefinedAllowedAttributeValueRequest();
          BeanUtils.copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueRequest);
          predefinedAllowedAttributeValueRequest
              .setPredefinedAllowedAttributeCode(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode());
          productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
        }

        if (Objects.nonNull(masterDataProductAttributeValueDTO.getDescriptiveAttributeValueType())) {
          if (DescriptiveAttributeValueType.NONE
              .equals(masterDataProductAttributeValueDTO.getDescriptiveAttributeValueType())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(
                com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE);
          } else if (DescriptiveAttributeValueType.SINGLE
              .equals(masterDataProductAttributeValueDTO.getDescriptiveAttributeValueType())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(
                com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);
          } else if (DescriptiveAttributeValueType.MULTIPLE
              .equals(masterDataProductAttributeValueDTO.getDescriptiveAttributeValueType())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(
                com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.MULTIPLE);
          } else {
            productAttributeValueRequest.setDescriptiveAttributeValueType(
                com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
          }
        }
        configureDefaultValues(productAttributeValueRequest);
        productAttributeValues.add(productAttributeValueRequest);
      }
      productAttributeRequest.setProductAttributeValues(productAttributeValues);
    }
  }

  private static void copyProductImages(ProductRequest productRequest, MasterDataProductDTO masterDataProductDTO) {
    List<MasterDataProductImageDTO> masterDataProductImageDTOS = masterDataProductDTO.getMasterDataProductImages();
    if (CollectionUtils.isNotEmpty(masterDataProductImageDTOS)) {
      List<Image> images = new ArrayList<>();
      for (MasterDataProductImageDTO masterDataProductImageDTO : masterDataProductImageDTOS) {
        Image image = new Image();
        image.setLocationPath(masterDataProductImageDTO.getLocationPath());
        image.setSequence(masterDataProductImageDTO.getSequence());
        image.setMainImages(masterDataProductImageDTO.isMainImage());
        image.setActive(Boolean.TRUE);
        image.setOriginalImage(Boolean.FALSE);
        image.setHashCode(
            ApproveProductUtils.generateHashcodeByLocationPath(masterDataProductImageDTO.getLocationPath()));
        image.setCreatedBy(Constants.SYSTEM);
        image.setStoreId(Constants.DEFAULT_STORE_ID);
        image.setCreatedDate(new Date());
        image.setUpdatedBy(Constants.SYSTEM);
        images.add(image);
      }
      productRequest.setImages(images);
    } else {
      if (CollectionUtils.isNotEmpty(productRequest.getProductItems())) {
        Set<Image> images = new HashSet<>();
        int itemCount = 0;
        for (ProductItemRequest productItemRequest : productRequest.getProductItems()) {
          if (itemCount == 0) {
            images.addAll(productItemRequest.getImages().stream().collect(Collectors.toList()));
          } else {
            images.addAll(productItemRequest.getImages().stream().filter(image -> !image.isMainImages())
                .collect(Collectors.toList()));
          }
          itemCount++;
        }
        productRequest.setImages(new ArrayList<>(images));
      }
    }
  }

  public static List<ProductImagePredictionAndCategoryMappingResponse> getProductImagePredictionAndCategoryMappingResponses(
      List<ProductImagePrediction> productImagePredictionList,
      Map<String, List<CategoryCodeAndNameResponse>> predictionIdCategoryCodeMap) {
    List<ProductImagePredictionAndCategoryMappingResponse> responseList = new ArrayList<>();
    for (ProductImagePrediction productImagePrediction : productImagePredictionList) {
      ProductImagePredictionAndCategoryMappingResponse productImagePredictionAndCategoryMappingResponse =
          new ProductImagePredictionAndCategoryMappingResponse();
      productImagePredictionAndCategoryMappingResponse.setPredictionType(productImagePrediction.getPredictionType());
      productImagePredictionAndCategoryMappingResponse.setRuleEnabled(productImagePrediction.isForceReview());
      productImagePredictionAndCategoryMappingResponse.setConfidenceThreshold(
          productImagePrediction.getConfidenceThreshold());
      productImagePredictionAndCategoryMappingResponse.setTextConfidenceThreshold(
          productImagePrediction.getTextConfidenceThreshold());
      List<CategoryCodeAndCategoryNameResponse> categoryCodeAndCategoryNameResponses = new ArrayList<>();
      if (predictionIdCategoryCodeMap.containsKey(productImagePrediction.getId())) {
        for (CategoryCodeAndNameResponse categoryCodeAndNameResponse : predictionIdCategoryCodeMap.get(
            productImagePrediction.getId())) {
          CategoryCodeAndCategoryNameResponse categoryCodeAndCategoryNameResponse = new CategoryCodeAndCategoryNameResponse();
          categoryCodeAndCategoryNameResponse.setCategoryName(categoryCodeAndNameResponse.getCategoryName());
          categoryCodeAndCategoryNameResponse.setCategoryCode(categoryCodeAndNameResponse.getCategoryCode());
          categoryCodeAndCategoryNameResponses.add(categoryCodeAndCategoryNameResponse);
        }
      }
      productImagePredictionAndCategoryMappingResponse.setCategoryCodeAndCategoryNameResponseList(
          categoryCodeAndCategoryNameResponses);
      responseList.add(productImagePredictionAndCategoryMappingResponse);
    }
    return responseList;
  }

  public static Set<PriceRequest> setOfferPrice(List<ProductLevel3PriceRequest> productLevel3Prices) {
    Set<PriceRequest> priceRequests = new HashSet<>();
    for (ProductLevel3PriceRequest productLevel3Price : productLevel3Prices) {
      getPriceRequest(priceRequests, productLevel3Price.getSalePrice());
    }
    return priceRequests;
  }

  public static void getPriceRequest(Set<PriceRequest> priceRequests, Double salePrice) {
    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setOfferPrice(salePrice);
    priceRequests.add(priceRequest);
  }

  public static void setProductLevel3LogisticsRequestAndPreOrderRequest(ProductL3UpdateRequest productL3UpdateRequest,
      ProductLevel3UpdateRequest productLevel3UpdateRequest) {
    BeanUtils.copyProperties(productL3UpdateRequest, productLevel3UpdateRequest, "productLevel3LogisticsRequest",
        "preOrder");
    productLevel3UpdateRequest.setProductLevel3LogisticsRequest(
        productL3UpdateRequest.getProductLevel3LogisticsRequest());
    productLevel3UpdateRequest.setPreOrder(productL3UpdateRequest.getPreOrder());
  }



  public static ItemViewConfigAndItemSkuRequest getItemViewConfigRequest(String itemSku, String pickUpPointCode,
      boolean buyable, boolean discoverable) {
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(itemSku);
    itemViewConfigAndItemSkuRequest.setPickupPointCode(pickUpPointCode);
    itemViewConfigAndItemSkuRequest.setBuyable(buyable);
    itemViewConfigAndItemSkuRequest.setDiscoverable(discoverable);
    itemViewConfigAndItemSkuRequest.setChannel(Constants.DEFAULT);
    return itemViewConfigAndItemSkuRequest;
  }

  public static ItemViewConfigAndItemSkuRequest getItemViewConfigRequestB2b(String itemSku, String pickUpPointCode,
      boolean buyable, boolean discoverable) {
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(itemSku);
    itemViewConfigAndItemSkuRequest.setPickupPointCode(pickUpPointCode);
    itemViewConfigAndItemSkuRequest.setBuyable(buyable);
    itemViewConfigAndItemSkuRequest.setDiscoverable(discoverable);
    itemViewConfigAndItemSkuRequest.setChannel(Constants.B2B_CHANNEL);
    return itemViewConfigAndItemSkuRequest;
  }

  public static ItemViewConfigAndItemSkuRequest getItemViewConfigRequestCnc(String itemSku,
      String pickUpPointCode, boolean buyable, boolean discoverable) {
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest =
        new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(itemSku);
    itemViewConfigAndItemSkuRequest.setPickupPointCode(pickUpPointCode);
    itemViewConfigAndItemSkuRequest.setBuyable(buyable);
    itemViewConfigAndItemSkuRequest.setDiscoverable(discoverable);
    itemViewConfigAndItemSkuRequest.setChannel(Constants.CNC_CHANNEL);
    return itemViewConfigAndItemSkuRequest;
  }

  public static ItemViewConfigDTO getSingleItemViewConfigByChannel(String channel,
      Set<ItemViewConfigDTO> itemViewConfigs) {
    return itemViewConfigs.stream()
        .filter(itemViewConfig -> channel.equals(itemViewConfig.getChannel())).findFirst()
        .orElse(new ItemViewConfigDTO());
  }

  public static NeedCorrectionItemActivationRequest getNeedRevisionActivationRequest(
      ProductItemBusinessPartner itemBusinessPartner, Map<String, Boolean> productItemWholesalePriceMap,
      boolean cncForWarehouseFeatureSwitch) {
    NeedCorrectionItemActivationRequest itemActivationRequest = new NeedCorrectionItemActivationRequest();
    itemActivationRequest.setItemSku(itemBusinessPartner.getGdnProductItemSku());
    itemActivationRequest.setBuyable(itemBusinessPartner.isBuyable());
    itemActivationRequest.setDiscoverable(itemBusinessPartner.isDisplay());
    if (cncForWarehouseFeatureSwitch) {
      itemActivationRequest.setCncBuyable(itemBusinessPartner.isCncBuyable());
      itemActivationRequest.setCncDiscoverable(itemBusinessPartner.isCncDiscoverable());
    }
    itemActivationRequest.setCncActive(itemBusinessPartner.isCncActive());
    itemActivationRequest.setDistribution(itemBusinessPartner.isDistribution());
    itemActivationRequest.setFbbActivated(itemBusinessPartner.isFbbActive());
    if (Objects.nonNull(itemBusinessPartner.getPrice())) {
      itemActivationRequest.setListPrice(itemBusinessPartner.getPrice());
    }
    if (Objects.nonNull(itemBusinessPartner.getSalePrice())) {
      itemActivationRequest.setOfferPrice(itemBusinessPartner.getSalePrice());
    }
    itemActivationRequest.setMerchantSku(itemBusinessPartner.getMerchantSku());
    itemActivationRequest.setPickupPointCode(itemBusinessPartner.getPickupPointId());
    if (Objects.nonNull(itemBusinessPartner.getB2bPrice())) {
      B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
      b2bFieldsVo.setManaged(itemBusinessPartner.isB2bManaged());
      b2bFieldsVo.setBasePrice(itemBusinessPartner.getB2bPrice());
      Set<ItemViewConfig> b2bViewConfig = new HashSet<>();
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setChannel(Constants.B2B_CHANNEL);
      itemViewConfig.setBuyable(itemBusinessPartner.isB2bBuyable());
      itemViewConfig.setDiscoverable(itemBusinessPartner.isB2bDiscoverable());
      b2bViewConfig.add(itemViewConfig);
      b2bFieldsVo.setB2bItemViewConfigs(b2bViewConfig);
      itemActivationRequest.setB2bFields(b2bFieldsVo);
    }
    itemActivationRequest.setBundleRecipe(
      toBundleRecipeVoResponseFromJson(itemBusinessPartner.getBundleRecipe()));
      itemActivationRequest.setWholesalePriceActivated(productItemWholesalePriceMap
          .get(itemBusinessPartner.getGdnProductItemSku() + Constants.HYPHEN + itemBusinessPartner.getPickupPointId()));
    return itemActivationRequest;
  }

  private static void configureDefaultValues(BaseDTORequest baseDTORequest) {
    baseDTORequest.setCreatedBy(Constants.SYSTEM);
    baseDTORequest.setStoreId(Constants.DEFAULT_STORE_ID);
    baseDTORequest.setCreatedDate(new Date());
    baseDTORequest.setUpdatedBy(Constants.SYSTEM);
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(productCategoryRequests)) {
      for (ProductCategoryRequest productCategoryRequest : productCategoryRequests) {
        ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(productAttributeRequests)) {
      for (ProductAttributeRequest productAttributeRequest : productAttributeRequests) {
        ProductAttributeDomainEventModel productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(productAttributeValueRequests)) {
      for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeValueRequests) {
        ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel = new ProductAttributeValueDomainEventModel();
        productAttributeValueDomainEventModel.setDescriptiveAttributeValueType(String.valueOf(
            productAttributeValueRequest.getDescriptiveAttributeValueType()));
        productAttributeValueDomainEventModel.setDescriptiveAttributeValue(
            productAttributeValueRequest.getDescriptiveAttributeValue());
        if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
          AllowedAttributeValueDomainEventModel allowedAttributeValueDomainEventModel = new AllowedAttributeValueDomainEventModel();
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(imageRequests)) {
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(productItemRequests)) {
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
    if(org.apache.commons.collections.CollectionUtils.isNotEmpty(productItemAttributeValueRequests)) {
      for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemAttributeValueRequests) {
        ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel = new ProductItemAttributeValueDomainEventModel();
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

  public static WholesalePriceBulkUpdateRequest toWholesalePriceBulkUpdateRequest(
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest, ItemSummaryResponse itemSummaryResponse) {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    wholesalePriceBulkUpdateRequest.setNewProduct(false);
    wholesalePriceBulkUpdateRequest.setProductSku(itemSummaryResponse.getProductSku());
    WholesalePriceSkuRequest wholesalePriceSkuRequest = new WholesalePriceSkuRequest();
    wholesalePriceSkuRequest.setItemCode(itemSummaryResponse.getItemCode());
    wholesalePriceSkuRequest.setItemSku(itemSummaryResponse.getItemSku());
    wholesalePriceSkuRequest.setPickUpPointCode(itemSummaryResponse.getPickupPointCode());
    wholesalePriceSkuRequest.setWholesalePriceActivated(productPriceStockAndImagesRequest.getWholesalePriceActivated());
    Map<Integer, Double> wholeSaleRulesMap =
        Optional.ofNullable(productPriceStockAndImagesRequest.getProductItemWholesalePrices())
            .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(ProductItemWholesalePriceRequest::getQuantity,
            ProductItemWholesalePriceRequest::getWholesaleDiscount));
    Map<Integer, Double> wholeSaleRulesMapSorted = new TreeMap<>(wholeSaleRulesMap);
    wholesalePriceSkuRequest.setWholesaleRules(wholeSaleRulesMapSorted);
    wholesalePriceBulkUpdateRequest.setWholesalePriceSkuRequests(Collections.singletonList(wholesalePriceSkuRequest));
    return wholesalePriceBulkUpdateRequest;
  }

  public static WholesalePriceBulkUpdateRequest toWholesalePriceBulkUpdateRequestL5(String productSku, String itemCode,
      String itemSku, String pickupPointCode, Boolean wholesalePriceActivated,
      List<ProductItemWholesalePriceRequest> productItemWholesalePrices, boolean isWholesaleFlagChanged) {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    wholesalePriceBulkUpdateRequest.setNewProduct(false);
    wholesalePriceBulkUpdateRequest.setProductSku(productSku);
    WholesalePriceSkuRequest wholesalePriceSkuRequest = new WholesalePriceSkuRequest();
    wholesalePriceSkuRequest.setItemCode(itemCode);
    wholesalePriceSkuRequest.setItemSku(itemSku);
    wholesalePriceSkuRequest.setPickUpPointCode(pickupPointCode);
    if(isWholesaleFlagChanged) {
      wholesalePriceSkuRequest.setWholesalePriceActivated(wholesalePriceActivated);
    } else {
      wholesalePriceSkuRequest.setWholesalePriceActivated(null);
    }
    Map<Integer, Double> wholeSaleRulesMap =
        Optional.ofNullable(productItemWholesalePrices).orElse(new ArrayList<>()).stream().collect(Collectors
            .toMap(ProductItemWholesalePriceRequest::getQuantity,
                ProductItemWholesalePriceRequest::getWholesaleDiscount));
    Map<Integer, Double> wholeSaleRulesMapSorted = new TreeMap<>(wholeSaleRulesMap);
    wholesalePriceSkuRequest.setWholesaleRules(wholeSaleRulesMapSorted);
    wholesalePriceBulkUpdateRequest.setWholesalePriceSkuRequests(Collections.singletonList(wholesalePriceSkuRequest));
    return wholesalePriceBulkUpdateRequest;
  }

  public static SkuCodesRequest convertItemSkusToSkuCodesRequest(List<String> itemSkuCodes) {
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.getSkuCodes().addAll(itemSkuCodes);
    skuCodesRequest.setFetchArchived(true);
    return skuCodesRequest;
  }

  public static ProductItemUpcCodeUpdateRequest convertToProductItemUpcCodeUpdateRequest(String skuCode,
      String upcCode) {
    return ProductItemUpcCodeUpdateRequest.builder().skuCode(skuCode).upcCode(upcCode).build();
  }

  public static ProductItemImageRequest toUpdatedProductItemImageRequest(
      List<ProductLevel3SummaryDetailsImageRequest> images, String skuCode) {
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemsImages = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : images) {
      if (UPDATE.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        Image image = new Image();
        image.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
        image.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
        image.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
        image.setSequence(productLevel3SummaryDetailsImageRequest.getSequence());
        image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath()));
        itemsImages.add(image);
      }
    }
    productItemImageRequest.setItemImages(itemsImages);
    productItemImageRequest.setSkuCode(skuCode);
    return productItemImageRequest;
  }

  public static List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> toItemImageEditRequest(
    ImmutableMap<String, List<ProductItemImageRequest>> reviewTypeImageMap) {
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> itemImageEditRequestList =
      new ArrayList<>();
    for (Map.Entry<String, List<ProductItemImageRequest>> entry : reviewTypeImageMap.entrySet()) {
      String operation = entry.getKey();
      List<ProductItemImageRequest> itemImages = entry.getValue();
      for (ProductItemImageRequest productItemImageRequest : itemImages) {
        for (Image image : productItemImageRequest.getItemImages()) {
          com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest itemImageEditRequest =
            new com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest();
          itemImageEditRequest.setAdd(NEW.equals(operation));
          itemImageEditRequest.setItemCode(productItemImageRequest.getSkuCode());
          itemImageEditRequest.setMarkForDelete(image.isMarkForDelete());
          itemImageEditRequest.setMainImage(image.isMainImages());
          itemImageEditRequest.setHashCode(image.getHashCode());
          itemImageEditRequestList.add(itemImageEditRequest);
        }
      }
    }
    return itemImageEditRequestList;
  }


  public static ProductItemBusinessPartner getProductItemBusinessPartner(ProductVariantUpdateRequest request,
      ItemPickupPointRequest itemPickupPointRequest, ProductItemBusinessPartner productItemBusinessPartnerExistingData,
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerPickupPointCodeMap, Boolean b2bEnabled)  {
    ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerPickupPointCodeMap
        .getOrDefault(itemPickupPointRequest.getPickupPointId(), new ProductItemBusinessPartner());
    productItemBusinessPartner.setProductBusinessPartner(productItemBusinessPartnerExistingData.getProductBusinessPartner());
    productItemBusinessPartner.setProductItemId(productItemBusinessPartnerExistingData.getProductItemId());
    productItemBusinessPartner.setProductType(productItemBusinessPartnerExistingData.getProductType());
    productItemBusinessPartner.setInstallation(productItemBusinessPartnerExistingData.isInstallation());
    productItemBusinessPartner.setStoreId(productItemBusinessPartnerExistingData.getStoreId());
    productItemBusinessPartner.setBundleRecipe(productItemBusinessPartnerExistingData.getBundleRecipe());
    productItemBusinessPartner.setSaleStartDate(null);
    productItemBusinessPartner.setSaleEndDate(null);
    productItemBusinessPartner.setMerchantSku(itemPickupPointRequest.getSellerSku());
    productItemBusinessPartner.setGdnProductItemSku(itemPickupPointRequest.getItemSku());
    productItemBusinessPartner.setPrice(itemPickupPointRequest.getPrice());
    productItemBusinessPartner.setSalePrice(itemPickupPointRequest.getSalePrice());
    productItemBusinessPartner.setStock(itemPickupPointRequest.getStock());
    productItemBusinessPartner.setMinimumStock(itemPickupPointRequest.getMinimumStock());
    productItemBusinessPartner.setPickupPointId(itemPickupPointRequest.getPickupPointId());
    productItemBusinessPartner.setDisplay(itemPickupPointRequest.isDisplay());
    productItemBusinessPartner.setBuyable(itemPickupPointRequest.isBuyable());
    productItemBusinessPartner.setCncActivated(itemPickupPointRequest.isCncActive());
    productItemBusinessPartner.setCncBuyable(itemPickupPointRequest.isCncBuyable());
    productItemBusinessPartner.setCncDiscoverable(itemPickupPointRequest.isCncDisplay());
    productItemBusinessPartner.setFbbActive(Objects.nonNull(itemPickupPointRequest.getFbbActive()) ?
      itemPickupPointRequest.getFbbActive() : false);
    productItemBusinessPartner.setMarkForDelete(request.isMarkForDelete());
    if (Objects.nonNull(b2bEnabled)) {
      if (b2bEnabled && Objects.nonNull(itemPickupPointRequest.getB2bFields())) {
        B2BFields b2BFields = itemPickupPointRequest.getB2bFields();
        productItemBusinessPartner.setB2bPrice(b2BFields.getPrice());
        productItemBusinessPartner.setB2bBuyable(b2BFields.isBuyable());
        productItemBusinessPartner.setB2bDiscoverable(b2BFields.isDisplay());
        productItemBusinessPartner.setB2bManaged(b2BFields.isManaged());
      }
    }
    return productItemBusinessPartner;
  }

  public static boolean isPurchaseOrderPurchaseTerm(ProfileResponse businessPartner) {
    return GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equals(businessPartner.getCompany()
        .getPurchaseTerm());
  }

  public static ProductItemWholesalePrice getProductItemWholesalePrice(ProductVariantUpdateRequest request,
      ItemPickupPointRequest itemPickupPointRequest, ProductItemBusinessPartner productItemBusinessPartnerExistingData) {
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setProductItemId(productItemBusinessPartnerExistingData.getProductItemId());
    productItemWholesalePrice.setItemSku(productItemBusinessPartnerExistingData.getGdnProductItemSku());
    productItemWholesalePrice.setItemCode(request.getProductItems().get(0).getSkuCode());
    productItemWholesalePrice.setWholesalePriceActivated(Optional
        .ofNullable(itemPickupPointRequest.getWholesalePriceActivated()).orElse(false));
    productItemWholesalePrice.setUpdatePending(true);
    productItemWholesalePrice.setPickupPointCode(itemPickupPointRequest.getPickupPointId());
    productItemWholesalePrice.setCreatedBy(request.getCreatedBy());
    productItemWholesalePrice.setCreatedDate(request.getCreatedDate());
    productItemWholesalePrice.setMarkForDelete(request.isMarkForDelete());
    productItemWholesalePrice.setStoreId(request.getStoreId());
    productItemWholesalePrice.setUpdatedBy(request.getUpdatedBy());
    productItemWholesalePrice.setUpdatedDate(request.getUpdatedDate());
    return productItemWholesalePrice;
  }

  public static ProductItemImageRequest toNewProductItemImageRequest(
      List<ProductLevel3SummaryDetailsImageRequest> images, String skuCode, boolean needCorrection) {
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemsImages = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : images) {
      if (NEW.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        Image image = new Image();
        image.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
        image.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
        image.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
        image.setSequence(productLevel3SummaryDetailsImageRequest.getSequence());
        image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(
            productLevel3SummaryDetailsImageRequest.getLocationPath()));
        image.setOriginalImage(Boolean.TRUE);
        image.setCreatedBy(Constants.SYSTEM);
        image.setStoreId(Constants.DEFAULT_STORE_ID);
        image.setCreatedDate(new Date());
        image.setUpdatedBy(Constants.SYSTEM);
        image.setUpdatedDate(new Date());
        if (needCorrection) {
          image.setEdited(Boolean.FALSE);
          image.setRevised(Boolean.TRUE);
          image.setActive(Boolean.FALSE);
        } else {
          image.setEdited(Boolean.TRUE);
          image.setRevised(Boolean.FALSE);
          image.setActive(Boolean.TRUE);
        }
        itemsImages.add(image);
      }
    }
    productItemImageRequest.setItemImages(itemsImages);
    productItemImageRequest.setSkuCode(skuCode);
    return productItemImageRequest;
  }

  public static List<Image> copyToItemImages(List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages,
      boolean needCorrection) {
    List<Image> itemsImages = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : copyToAllVariantImages) {
      Image image = new Image();
      image.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
      image.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
      image.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
      image.setSequence(productLevel3SummaryDetailsImageRequest.getSequence());
      image.setCreatedBy(Constants.SYSTEM);
      image.setStoreId(Constants.DEFAULT_STORE_ID);
      image.setCreatedDate(new Date());
      image.setUpdatedBy(Constants.SYSTEM);
      if (NEW.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        image.setHashCode(ApproveProductUtils
            .generateHashcodeByLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath()));
        image.setOriginalImage(Boolean.TRUE);
        if (needCorrection) {
          image.setRevised(Boolean.TRUE);
          image.setEdited(Boolean.FALSE);
          image.setActive(Boolean.FALSE);
        } else {
          image.setEdited(Boolean.TRUE);
          image.setRevised(Boolean.FALSE);
          image.setActive(Boolean.TRUE);
        }
      } else {
        image.setHashCode(null);
        image.setOriginalImage(Boolean.FALSE);
        image.setActive(Boolean.TRUE);
      }
      itemsImages.add(image);
    }
    return itemsImages;
  }

  public static ProductItemImageUpdateRequest toProductItemImageUpdateRequest(
      List<ProductItemImageRequest> updatedProductImageRequest, List<ProductItemImageRequest> newProductImageRequest,
      List<Image> copyImageToAllProductItemsRequest, String productCode, boolean needCorrection) {
    return ProductItemImageUpdateRequest.builder().newProductItemImages(newProductImageRequest)
        .needCorrection(needCorrection).updateProductItemImages(updatedProductImageRequest)
        .copyToAllVariantImages(copyImageToAllProductItemsRequest).productCode(productCode).build();
  }

  public static ImageQcRequestDomainEvent toImageQcRequestForEditedImages(String productCode, List<Image> images,
      String imageSourceDirectory, ProductDetailResponse productDetailResponse, String gcsImagePathPrefix,
      String gcsSourceLocation, RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    imageQcRequestDomainEvent.setProductCode(productCode);
    imageQcRequestDomainEvent.setImages(images.stream().map(
        imageResponse -> new ImageQcHashCodeAndLocationPathRequest(imageResponse.getHashCode(),
            fullSourceLocation(imageSourceDirectory, imageResponse.getLocationPath(), gcsImagePathPrefix,
                gcsSourceLocation))).collect(Collectors.toList()));
    imageQcRequestDomainEvent.setBrand(productDetailResponse.getBrand());
    imageQcRequestDomainEvent.setProductName(productDetailResponse.getName());
    imageQcRequestDomainEvent.setDescription(new String(productDetailResponse.getDescription()));
    imageQcRequestDomainEvent.setUsp(productDetailResponse.getUniqueSellingPoint());
    updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent, productDetailResponse.getCategoryCodes(),
        productDetailResponse.getCategories());
    getRestrictedKeywordRequest(restrictedKeywordsByFieldAndActionType, imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  private static String fullSourceLocation(String imageSourceDirectory, String locationPath,
  String gcsImagePathPrefix, String gcsSourceLocation) {
    if (locationPath.contains(gcsImagePathPrefix)) {
      return gcsSourceLocation.concat(locationPath);
    }
    return imageSourceDirectory.concat(locationPath);
  }

  public static ImageQcRequestDomainEvent toImageQcRequestForContentEdit(List<CategoryResponse> categoryResponses,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, ProductLevel3 productLevel3) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent =
        ImageQcRequestDomainEvent.builder().productCode(productLevel3.getProductCode()).images(new ArrayList<>())
            .brand(productLevel3.getBrand()).productName(productLevel3.getProductName())
            .description(productLevel3.getDescription()).usp(productLevel3.getUniqueSellingPoint()).build();
    List<String> categoryCodes = categoryResponses.stream().map(CategoryResponse::getCategoryCode).collect(Collectors.toList());
    Collections.reverse(categoryCodes);
    List<String> categoryNames = categoryResponses.stream().map(CategoryResponse::getName).collect(Collectors.toList());
    Collections.reverse(categoryNames);
    updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent, categoryCodes, categoryNames);
    getRestrictedKeywordRequest(restrictedKeywordsByFieldAndActionType, imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  public static ProductAndItemImageRequest toProductAndItemImageRequestForEditedResizedImages(
      BulkImageProcessResponse bulkImageProcessResponse, ProductDetailResponse productDetailResponse, boolean isRevised)
      throws ApplicationException, NoSuchAlgorithmException {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    MessageDigest messageDigest = MessageDigest.getInstance(Constants.MESSAGE_DIGEST_ALGORITHM);
    Map<String, CommonImagePathDto> allHashCodeLocationPathMap =
        bulkImageProcessResponse.getImageResponses().stream().filter(imageResponse -> imageResponse.isSuccess())
            .collect(Collectors.toMap(ImageResponse::getHashCode,
                imageResponse -> new CommonImagePathDto(imageResponse.getImagePathLocation(),
                    imageResponse.isCommonImage()), (image1, image2) -> image1));
    Map<String, CommonImagePathDto> hashCodeLocationPathMap = new HashMap<>();
    List<String> setOfHashcodeInProductDetails = productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
        .map(image -> image.getHashCode()).collect(Collectors.toList());
    for (Map.Entry<String, CommonImagePathDto> allHashCodeLocationPathMapEntry : allHashCodeLocationPathMap.entrySet()) {
      messageDigest.update(allHashCodeLocationPathMapEntry.getValue().getLocationPath().getBytes());
      String updatedHashcode = ApproveProductUtils.generateHashcode(messageDigest);
      if (!setOfHashcodeInProductDetails.contains(updatedHashcode)) {
        hashCodeLocationPathMap.put(allHashCodeLocationPathMapEntry.getKey(),
            allHashCodeLocationPathMapEntry.getValue());
      }
    }
    if (hashCodeLocationPathMap.isEmpty()) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          RESIZED_DATA_NOT_FOUND + productDetailResponse.getProductCode());
    }
    productAndItemImageRequest.setProductCode(productDetailResponse.getProductCode());
    List<ProductItemImageRequest> productItemImageRequests = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
      productItemImageRequest.setSkuCode(productItemResponse.getSkuCode());
      productItemImageRequest.setItemImages(productItemResponse.getImages()
          .stream()
          .filter(image -> (!image.isMarkForDelete() && hashCodeLocationPathMap.keySet().contains(image.getHashCode())))
          .map(image -> getResizedImageRequestsEdited(image, hashCodeLocationPathMap, messageDigest, isRevised))
          .collect(Collectors.toList()));
      productItemImageRequests.add(productItemImageRequest);
    }
    productAndItemImageRequest.setProductItemImages(productItemImageRequests);
    productAndItemImageRequest.setProductImages(productDetailResponse.getImages()
        .stream()
        .filter(image -> !image.isMarkForDelete() && hashCodeLocationPathMap.keySet().contains(image.getHashCode()))
        .map(image -> getResizedImageRequestsEdited(image, hashCodeLocationPathMap, messageDigest, isRevised))
        .collect(Collectors.toList()));
    return productAndItemImageRequest;
  }

  public static AddEditedProductToPDTEvent toAddEditedProductToPDTEvent(String storeId, String reviewTypes,
      ProductCollection productCollection, List<String> allModifiedFields,
      ProfileResponse profileResponse, ProductBusinessPartner productBusinessPartner,
      boolean priceInfoVendorEditedEnabled, int priceInfoMaxVariantLimit) {

    List<PriceInfoDTO> priceInfoList = new ArrayList<>();
    if (priceInfoVendorEditedEnabled) {
      priceInfoList = getPriceInfoFromProductItemBusinessPartner(
          productBusinessPartner.getProductItemBusinessPartners(), priceInfoMaxVariantLimit);
    }
    AddEditedProductToPDTEvent addEditedProductToPDTEvent = new AddEditedProductToPDTEvent();
    addEditedProductToPDTEvent.setStoreId(storeId);
    addEditedProductToPDTEvent.setProductCode(productCollection.getProductCode());
    addEditedProductToPDTEvent.setReviewTypes(reviewTypes);
    addEditedProductToPDTEvent.setPostLive(productCollection.isPostLive());
    addEditedProductToPDTEvent.setPostLive(productCollection.isPostLive());
    addEditedProductToPDTEvent.setMerchantName(productCollection.getBusinessPartnerName());
    addEditedProductToPDTEvent.setMerchantCode(productCollection.getBusinessPartnerCode());
    addEditedProductToPDTEvent.setRestrictedKeywordsPresent(productCollection.isRestrictedKeywordsPresent());
    addEditedProductToPDTEvent.setUpdatedBy(productCollection.getUpdatedBy());
    addEditedProductToPDTEvent.setAllModifiedFields(allModifiedFields);
    addEditedProductToPDTEvent.setB2bActivated(productBusinessPartner.isB2bActivated());
    addEditedProductToPDTEvent.setB2cActivated(productBusinessPartner.isB2cActivated());
    addEditedProductToPDTEvent.setPriceInfo(priceInfoList);
    addEditedProductToPDTEvent.setProductCreationType(productCollection.getProductCreationType());
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(
        toRestrictedKeywordsByFieldResponseFromJson(productCollection.getRestrictedKeywordsDetected()));
    addEditedProductToPDTEvent.setTrustedSeller(Optional.ofNullable(profileResponse).filter(ProfileResponse::isTrustedSeller)
      .map(response -> Boolean.TRUE).orElse(false));
    return addEditedProductToPDTEvent;
  }

  public static AddEditedProductToPDTEvent toAddEditedProductToPDTEventForAddAndEdit(String storeId, String reviewTypes,
      ProductCollection productCollection, List<String> allModifiedFields, List<ProductItemBusinessPartner> productItemBusinessPartner,
      boolean priceInfoVendorEditedEnabled, int priceInfoMaxVariantLimit) {

    List<PriceInfoDTO> priceInfoList = new ArrayList<>();
    if (priceInfoVendorEditedEnabled) {
      priceInfoList = getPriceInfoFromProductItemBusinessPartner(
          productItemBusinessPartner, priceInfoMaxVariantLimit);
    }
    AddEditedProductToPDTEvent addEditedProductToPDTEvent = new AddEditedProductToPDTEvent();
    addEditedProductToPDTEvent.setStoreId(storeId);
    addEditedProductToPDTEvent.setProductCode(productCollection.getProductCode());
    addEditedProductToPDTEvent.setReviewTypes(reviewTypes);
    addEditedProductToPDTEvent.setPostLive(productCollection.isPostLive());
    addEditedProductToPDTEvent.setPostLive(productCollection.isPostLive());
    addEditedProductToPDTEvent.setMerchantName(productCollection.getBusinessPartnerName());
    addEditedProductToPDTEvent.setMerchantCode(productCollection.getBusinessPartnerCode());
    addEditedProductToPDTEvent.setRestrictedKeywordsPresent(productCollection.isRestrictedKeywordsPresent());
    addEditedProductToPDTEvent.setUpdatedBy(productCollection.getUpdatedBy());
    addEditedProductToPDTEvent.setAllModifiedFields(allModifiedFields);
    addEditedProductToPDTEvent.setB2bActivated(
        Objects.nonNull(productItemBusinessPartner.get(0).getProductBusinessPartner())
            && productItemBusinessPartner.get(0).getProductBusinessPartner().isB2bActivated());
    addEditedProductToPDTEvent.setB2cActivated(
        Objects.nonNull(productItemBusinessPartner.get(0).getProductBusinessPartner())
            && productItemBusinessPartner.get(0).getProductBusinessPartner().isB2cActivated());
    addEditedProductToPDTEvent.setPriceInfo(priceInfoList);
    addEditedProductToPDTEvent.setProductCreationType(productCollection.getProductCreationType());
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(
        toRestrictedKeywordsByFieldResponseFromJson(productCollection.getRestrictedKeywordsDetected()));
    return addEditedProductToPDTEvent;
  }

  public static List<HistoryResponse> toHistoryResponseFromLogAuditTrailUpdateProduct(
      List<UpdatedProductHistory> logAuditUpdateProducts) {
    List<HistoryResponse> historyResponseList = new ArrayList<>();
    for (UpdatedProductHistory updatedProductHistory : logAuditUpdateProducts) {
      HistoryResponse historyResponse = new HistoryResponse();
      BeanUtils.copyProperties(updatedProductHistory, historyResponse);
      if (Constants.DEFAULT.equals(updatedProductHistory.getGdnSku())) {
        historyResponse.setGdnSku(Constants.ALL_VARIANTS);
      }
      historyResponseList.add(historyResponse);
    }
    return historyResponseList;
  }

  public static List<HistoryUpdateResponse> toHistoryUpdateResponseList(
    List<UpdatedProductHistory> logAuditUpdateProducts) {
    List<HistoryUpdateResponse> historyUpdateResponseList = new ArrayList<>();
    for (UpdatedProductHistory updatedProductHistory : logAuditUpdateProducts) {
      HistoryUpdateResponse historyUpdateResponse = new HistoryUpdateResponse();
      BeanUtils.copyProperties(updatedProductHistory, historyUpdateResponse);
      if (Constants.DEFAULT.equals(updatedProductHistory.getGdnSku())) {
        historyUpdateResponse.setGdnSku(Constants.ALL_VARIANTS);
      }
      if(Constants.HYPHEN.equals(updatedProductHistory.getPickupPointCode())) {
        historyUpdateResponse.setPickupPointCode(Constants.ALL_PICKUP_POINTS);
        historyUpdateResponse.setPickupPointName(Constants.ALL_PICKUP_POINTS);
      }
      historyUpdateResponseList.add(historyUpdateResponse);
    }
    return historyUpdateResponseList;
  }

  public static void convertProductLevel3ToProduct(ProductLevel3 request, Product product) {
    String id = product.getId();
    product.setUpdatedBy(request.getUpdatedBy());
    product.setUpdatedDate(request.getUpdatedDate());
    Map<String, Attribute> savedProductAttribute = product.getProductAttributes().stream().collect(Collectors
        .toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(),
            productAttribute -> productAttribute.getAttribute()));

    BeanUtils.copyProperties(request, product, "productItems", "productAttributes", "version");
    product.setDescription(request.getDescription().getBytes());
    product.setName(request.getProductName());
    product.setId(id);

    for (ProductLevel3Attribute productLevel3Attribute : request.getAttributes()) {
      if (!savedProductAttribute.containsKey(productLevel3Attribute.getAttributeCode())) {
        ProductAttribute productAttribute = new ProductAttribute();
        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(productLevel3Attribute, attribute);
        productAttribute.setAttribute(attribute);
        product.getProductAttributes().add(productAttribute);
      }
    }
  }

  public static QuickEditUpdateRequest toQuickEditRequest(QuickEditRequest quickEditRequest) {
    QuickEditUpdateRequest request = new QuickEditUpdateRequest();
    request.setItemSku(quickEditRequest.getItemSku());
    request.setStatus(quickEditRequest.getStatus().name());
    request.setOff2OnActiveFlag(quickEditRequest.getOff2OnActiveFlag());
    request.setSellerSku(quickEditRequest.getSellerSku());
    request.setPickupPointCode(quickEditRequest.getPickupPointCode());
    request.setVersion(quickEditRequest.getVersion());
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(quickEditRequest.getPrice().getSalePrice());
    priceDTO.setListPrice(quickEditRequest.getPrice().getPrice());
    priceDTO.setChannel(quickEditRequest.getPrice().getChannelId());
    request.setPrice(priceDTO);
    return request;
  }

  public static ProductType getProductTypeFromCode(int code) {
    if (ProductType.REGULAR.getCode() == code) {
      return ProductType.REGULAR;
    } else if (ProductType.BIG_PRODUCT.getCode() == code) {
      return ProductType.BIG_PRODUCT;
    } else {
      return ProductType.BOPIS;
    }
  }

  public static Integer getCodeFromProductType(ProductType productType) {
    if (ProductType.REGULAR == productType) {
      return ProductType.REGULAR.getCode();
    } else if (ProductType.BIG_PRODUCT == productType) {
      return ProductType.BIG_PRODUCT.getCode();
    } else {
      return ProductType.BOPIS.getCode();
    }
  }

  public static String getProductStatus(boolean buyable, boolean discoverable) {
    if (buyable) {
      if (discoverable) {
        return ProductLevel3Status.ONLINE.name();
      } else {
        return ProductLevel3Status.B2B.name();
      }
    } else {
      if (discoverable) {
        return ProductLevel3Status.TEASER.name();
      } else {
        return ProductLevel3Status.OFFLINE.name();
      }
    }
  }

  public static boolean isProductStatusChange(boolean buyable, boolean discoverable, String status) {
    String itemStatus = getProductStatus(buyable, discoverable);
    List<String> requiredStatus = Arrays.asList(ProductLevel3Status.B2B.name(), ProductLevel3Status.TEASER.name());
    return (!StringUtils.equals(itemStatus, status) && requiredStatus.contains(status));
  }

  public static void trimLocationPaths(ScaleImageResponse imageResponse) {
    if (StringUtils.isNotEmpty(imageResponse.getImagePathLocation())) {
      imageResponse.setImagePathLocation(
          imageResponse.getImagePathLocation().replace(Constants.FULL_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
      imageResponse.setImagePathLocation(
          imageResponse.getImagePathLocation().replace(Constants.MEDIUM_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
      imageResponse.setImagePathLocation(
          imageResponse.getImagePathLocation().replace(Constants.THUMBNAIL_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
    }
  }

  public static ActivateImageRequest getActivateImageRequest(String productCode, ScaleImageResponse imageResponse) {
    ActivateImageRequest activateImageRequest =
        new ActivateImageRequest(productCode, imageResponse.getHashCode(), imageResponse.getImagePathLocation());
    activateImageRequest.setCommonImage(imageResponse.isCommonImage());
    return activateImageRequest;
  }

  public static String toNeedCorrectionNotesForScreening(NeedRevisionNotes needRevisionNotes)
      throws JsonProcessingException {
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().vendorNotes(needRevisionNotes.getVendorNotes())
            .imageReason(needRevisionNotes.getImageReason())
            .commonImageReason(needRevisionNotes.getCommonImageReason())
            .contentAdditionalNotes(needRevisionNotes.getContentAdditionalNotes())
            .imagesAdditionalNotes(needRevisionNotes.getImagesAdditionalNotes())
            .vendorErrorFields(needRevisionNotes.getVendorErrorFields()).allVariants(needRevisionNotes.isAllVariants())
            .merchantModifiedFields(new ArrayList<>())
            .itemNotes(new ArrayList<>())
            .build();
    return new ObjectMapper().writeValueAsString(needCorrectionNotesDto);
  }

  public static AddRevisedProductToPDTEvent toAddRevisedProductToPDTEvent(
      ProductCollection productCollection, String username, boolean trustedSeller,
      ProductBusinessPartner productBusinessPartner, boolean appealedProduct,
      String appealedProductNotes, boolean priceInfoVendorRevisedEnabled,
      int priceInfoMaxVariantLimit)
      throws JsonProcessingException {
    List<PriceInfoDTO> priceInfoList = new ArrayList<>();
    if (priceInfoVendorRevisedEnabled) {
      priceInfoList = getPriceInfoFromProductItemBusinessPartner(
          productBusinessPartner.getProductItemBusinessPartners(), priceInfoMaxVariantLimit);
    }
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        AddRevisedProductToPDTEvent.builder().storeId(productCollection.getStoreId())
            .productCode(productCollection.getProductCode()).postLive(productCollection.isPostLive())
            .merchantCode(productCollection.getBusinessPartnerCode())
            .merchantName(productCollection.getBusinessPartnerName()).updatedBy(username)
            .restrictedKeywordsPresent(productCollection.isRestrictedKeywordsPresent())
            .merchantModifiedFields(getMerchantModifiedFieldsFromJson(productCollection.getNeedCorrectionNotes()))
            .b2bActivated(productBusinessPartner.isB2bActivated()).b2cActivated(productBusinessPartner.isB2cActivated())
            .restrictedKeywordsDetected(
                toRestrictedKeywordsByFieldResponseFromJson(productCollection.getRestrictedKeywordsDetected()))
            .allModifiedFields(getAllModifiedFieldsFromJson(productCollection.getNeedCorrectionNotes())).trustedSeller(trustedSeller)
            .appealedProduct(appealedProduct)
            .appealedProductNotes(appealedProductNotes)
            .priceInfo(priceInfoList)
            .productCreationType(productCollection.getProductCreationType())
            .build();
    return addRevisedProductToPDTEvent;
  }

  public static List<PriceInfoDTO> getPriceInfoFromProductItemBusinessPartner(
      List<ProductItemBusinessPartner> productItemBusinessPartners, int priceInfoMaxVariantLimit) {
    Map<String, PriceInfoDTO> productItemIdToPriceInfoMap = new HashMap<>();
    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      PriceInfoDTO priceInfo =
          productItemIdToPriceInfoMap.getOrDefault(productItemBusinessPartner.getProductItemId(),
              PriceInfoDTO.builder().minPrice(productItemBusinessPartner.getSalePrice())
                  .maxPrice(productItemBusinessPartner.getSalePrice())
                  .itemId(productItemBusinessPartner.getProductItemId())
                  .itemSku(productItemBusinessPartner.getGdnProductItemSku()).build());
      priceInfo.setMinPrice(
          Math.min(priceInfo.getMinPrice(), productItemBusinessPartner.getSalePrice()));
      priceInfo.setMaxPrice(
          Math.max(priceInfo.getMaxPrice(), productItemBusinessPartner.getSalePrice()));
      productItemIdToPriceInfoMap.put(productItemBusinessPartner.getProductItemId(), priceInfo);
    });

    if (productItemIdToPriceInfoMap.size() > priceInfoMaxVariantLimit) {
      return new ArrayList<>();
    }
    return new ArrayList<>(productItemIdToPriceInfoMap.values());
  }

  private static List<String> getMerchantModifiedFieldsFromJson(String needCorrectionNotesJson)
      throws JsonProcessingException {
    List<String> merchantModifiedFields = new ArrayList<>();
    if (StringUtils.isNotEmpty(needCorrectionNotesJson)) {
      NeedCorrectionNotesDto needCorrectionNotesDto =
          new ObjectMapper().readValue(needCorrectionNotesJson, NeedCorrectionNotesDto.class);
      merchantModifiedFields =
          Optional.ofNullable(needCorrectionNotesDto.getMerchantModifiedFields()).orElse(new ArrayList<>());
    }
    return merchantModifiedFields;
  }

  private static List<String> getAllModifiedFieldsFromJson(String needCorrectionNotesJson)
      throws JsonProcessingException {
    List<String> allModifiedFields = new ArrayList<>();
    if (StringUtils.isNotEmpty(needCorrectionNotesJson)) {
      NeedCorrectionNotesDto needCorrectionNotesDto =
          new ObjectMapper().readValue(needCorrectionNotesJson, NeedCorrectionNotesDto.class);
      allModifiedFields =
          Optional.ofNullable(needCorrectionNotesDto.getAllModifiedFields()).orElse(new ArrayList<>());
      allModifiedFields
          .addAll(Optional.ofNullable(needCorrectionNotesDto.getMerchantModifiedFields()).orElse(new ArrayList<>()));
    }
    return allModifiedFields;
  }

  public static WholesalePriceRequest toWholesalePriceRequest(String productSku, String merchantCode,
      boolean newProduct, List<ProductItemWholesalePrice> productItemWholesalePrices) throws JsonProcessingException {
    List<WholesalePriceSkuRequest> wholesalePriceSkuRequests = new ArrayList<>();
    for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePrices) {
      WholesalePriceSkuRequest wholesalePriceSkuRequest =
          WholesalePriceSkuRequest.builder().itemSku(productItemWholesalePrice.getItemSku())
              .itemCode(productItemWholesalePrice.getItemCode())
              .wholesalePriceActivated(productItemWholesalePrice.isWholesalePriceActivated())
            .wholesaleRules(
              getWholesaleRulesFromJson(productItemWholesalePrice.getWholesaleRules()))
            .pickUpPointCode(productItemWholesalePrice.getPickupPointCode()).build();
      wholesalePriceSkuRequests.add(wholesalePriceSkuRequest);
    }
    return WholesalePriceRequest.builder().productSku(productSku).merchantCode(merchantCode).newProduct(newProduct)
        .wholesalePriceSkuRequests(wholesalePriceSkuRequests).build();
  }

  private static Map<Integer, Double> getWholesaleRulesFromJson(String jsonWholesaleRule)
      throws JsonProcessingException {
    Map<Integer, Double> wholesaleRules = new HashMap<>();
    if (StringUtils.isNotEmpty(jsonWholesaleRule)) {
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
          new ObjectMapper().readValue(jsonWholesaleRule, new TypeReference<List<ProductItemWholesalePriceRequest>>() {
          });
      productItemWholesalePriceRequests.forEach(productItemWholesalePriceRequest -> wholesaleRules
          .put(productItemWholesalePriceRequest.getQuantity(),
              productItemWholesalePriceRequest.getWholesaleDiscount()));
    }
    return wholesaleRules;
  }

  public static List<ImageRequest> getRevisedImageRequests(List<Image> images, String imageSourceDirectory) {
    List<ImageRequest> revisedImageList = new ArrayList<>();
    for (Image image : images) {
      if (!image.isMarkForDelete() && image.isRevised()) {
        ImageRequest imageRequest = new ImageRequest(generateImageName(image.getLocationPath()),
            generateImagePath(image.getLocationPath(), imageSourceDirectory), image.getHashCode());
        imageRequest.setCommonImage(image.isCommonImage());
        revisedImageList.add(imageRequest);
      }
    }
    return revisedImageList;
  }

  private static String generateImageName(String location) {
    String[] splitImageFilenameByDash = location.split(File.separator);
    return splitImageFilenameByDash[splitImageFilenameByDash.length - 1];
  }

  private static String generateImagePath(String location, String imageSourceDirectory) {
    return imageSourceDirectory + File.separator + location;
  }

  public static ProductRequest setProductBasicDetails(PDTProductDomainEventModel pdtProductDomainEventModel,
    ProductDetailResponse productDetailResponse) {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setBrandCode(pdtProductDomainEventModel.getBrandCode());
    productRequest.setBrandApprovalStatus(pdtProductDomainEventModel.getBrandApprovalStatus());
    productRequest.setBrand(pdtProductDomainEventModel.getBrand());
    BeanUtils.copyProperties(pdtProductDomainEventModel, productRequest, "markForDelete", "productItems",
        "productCategories", "productAttributes", "images");

    setProductRequestParams(productDetailResponse, productRequest);
    return productRequest;
  }

  public static ProductImagePredictionResponse toProductImagePredictionResponse(
      ProductImagePrediction productImagePrediction) {
    ProductImagePredictionResponse productImagePredictionResponse = new ProductImagePredictionResponse();
    BeanUtils.copyProperties(productImagePrediction, productImagePredictionResponse);
    return productImagePredictionResponse;
  }

  public static List<ProductImagePredictionResponse> toListOfProductImagePredictionResponse(
      List<ProductImagePrediction> productImagePredictionList) {
    return productImagePredictionList.stream().map(ConverterUtil::toProductImagePredictionResponse)
        .collect(Collectors.toList());
  }

  private static void setProductRequestParams(ProductDetailResponse productDetailResponse,
    ProductRequest productRequest) {
    productRequest.setId(productDetailResponse.getId());
    productRequest.setActivated(productDetailResponse.isActivated());
    productRequest.setViewable(productDetailResponse.isViewable());
    productRequest.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    productRequest.setLongDescription(productDetailResponse.getLongDescription());
    productRequest.setStoreId(productDetailResponse.getStoreId());
    productRequest.setVersion(productDetailResponse.getVersion());
    productRequest.setCreatedBy(productDetailResponse.getCreatedBy());
    productRequest.setCreatedDate(productDetailResponse.getCreatedDate());
    productRequest.setUpdatedBy(productRequest.getUpdatedBy());
    productRequest.setUpdatedDate(Calendar.getInstance().getTime());
  }

  public static void setProductRequestImages(PDTProductDomainEventModel pdtProductDomainEventModel,
    ProductRequest productRequest) {
    List<Image> imageList = new ArrayList<>();
    for (ImageDomainEventModel imageDomainEventModel : pdtProductDomainEventModel.getImages()) {
      Image image = new Image();
      image.setLocationPath(imageDomainEventModel.getLocationPath());
      image.setMainImages(imageDomainEventModel.isMainImage());
      image.setSequence(imageDomainEventModel.getSequence());
      image.setCommonImage(imageDomainEventModel.isCommonImage());
      image.setOriginalImage(Boolean.FALSE);
      image.setStoreId(productRequest.getStoreId());
      image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(imageDomainEventModel.getLocationPath()));
      imageList.add(image);
    }
    productRequest.setImages(imageList);
  }

  public static void setProductRequestCategories(ProductDetailResponse productDetailResponse,
    ProductRequest productRequest) {
    List<ProductCategoryRequest> productCategoryRequestList = new ArrayList<>();
    for (ProductCategoryResponse productCategoryResponse : productDetailResponse
      .getProductCategoryResponses()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      BeanUtils.copyProperties(productCategoryResponse, productCategoryRequest, "category");
      CategoryRequest categoryRequest = new CategoryRequest();
      CategoryResponse categoryResponse = productCategoryResponse.getCategory();
      if (Objects.nonNull(categoryResponse)) {
        BeanUtils.copyProperties(categoryResponse, categoryRequest, "catalog");
        CatalogRequest catalogRequest = new CatalogRequest();
        if (Objects.nonNull(categoryResponse.getCatalog())) {
          BeanUtils.copyProperties(categoryResponse.getCatalog(), catalogRequest);
          categoryRequest.setCatalog(catalogRequest);
        }
        productCategoryRequest.setCategory(categoryRequest);
      }
      productCategoryRequestList.add(productCategoryRequest);
    }
    productRequest.setProductCategories(productCategoryRequestList);
  }

  public static void setProductRequestCategoriesOnCategoryChange(ProductRequest productRequest,
    PDTProductDomainEventModel productDomainEventModel, Map<String, CategoryDetailResponse> categoryCodeToCategoryDetailMap) {
    productRequest.setProductCategories(new ArrayList<>());
    for (ProductCategoryDomainEventModel productCategoryDomainEventModel : productDomainEventModel
      .getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryResponse categoryResponse =
        categoryCodeToCategoryDetailMap.get(productCategoryDomainEventModel.getCategory().getCategoryCode());
      CategoryRequest category = new CategoryRequest();
      BeanUtils.copyProperties(categoryResponse, category, "catalog");
      if (Objects.nonNull(categoryResponse.getCatalog())) {
        CatalogRequest catalog = new CatalogRequest();
        BeanUtils.copyProperties(categoryResponse.getCatalog(), catalog);
        category.setCatalog(catalog);
      }
      productCategoryRequest.setCategory(category);
      productRequest.getProductCategories().add(productCategoryRequest);
    }
  }

  public static ProductItemRequest setProductItemBasicDetails(PDTProductDomainEventModel pdtProductDomainEventModel,
    ProductItemResponse productItemResponse) {
    ProductItemRequest productItemRequest = new ProductItemRequest();
    BeanUtils.copyProperties(productItemResponse, productItemRequest, "images",
      "productItemAttributeValueResponses");
    productItemRequest.setActivated(productItemResponse.isActivated());
    productItemRequest.setViewable(productItemResponse.isViewable());
    for (ProductItemDomainEventModel productItemDomainEventModel : pdtProductDomainEventModel
      .getProductItems()) {
      if (productItemDomainEventModel.getSkuCode().equals(productItemResponse.getSkuCode())) {
        productItemRequest
          .setGeneratedItemName(productItemDomainEventModel.getGeneratedItemName());
        productItemRequest.setDangerousGoodsLevel(productItemDomainEventModel.getDangerousGoodsLevel());
        productItemRequest.setUpcCode(productItemDomainEventModel.getUpcCode());
      }
    }
    return productItemRequest;
  }

  public static void setProductRequestItemImageList(PDTProductDomainEventModel pdtProductDomainEventModel,
    ProductItemResponse productItemResponse, ProductItemRequest productItemRequest) {
    for (PDTProductItemDomainEventModel productItemDomainEventModel : pdtProductDomainEventModel.getProductItems()) {
      if (productItemDomainEventModel.getSkuCode().equals(productItemResponse.getSkuCode())) {
        List<Image> itemImageList = new ArrayList<>();
        for (ImageDomainEventModel imageDomainEventModel : productItemDomainEventModel.getImages()) {
          Image image = new Image();
          image.setLocationPath(imageDomainEventModel.getLocationPath());
          image.setMainImages(imageDomainEventModel.isMainImage());
          image.setSequence(imageDomainEventModel.getSequence());
          image.setCommonImage(imageDomainEventModel.isCommonImage());
          image.setOriginalImage(Boolean.FALSE);
          image.setStoreId(productItemRequest.getStoreId());
          image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(imageDomainEventModel.getLocationPath()));
          itemImageList.add(image);
        }
        productItemRequest.setImages(itemImageList);
      }
    }
  }

  public static void setProductRequestDescriptiveAttributeValues(ProductAttributeResponse productAttributeResponse,
    ProductAttributeRequest productAttributeRequest, ProductAttributeDomainEventModel productAttributeDomainEventModel,
    AttributeRequest attributeRequest) {
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    String attributeValue = productAttributeDomainEventModel.getProductAttributeValues().get(0)
      .getDescriptiveAttributeValue();
    List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest =
      new ProductAttributeValueRequest();
    ProductAttributeValueResponse productAttributeValueResponse =
      new ProductAttributeValueResponse();
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(productAttributeResponse.getProductAttributeValues())) {
      productAttributeValueResponse = productAttributeResponse.getProductAttributeValues().get(0);
    }
    BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest,
      "allowedAttributeValue", "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
    productAttributeValueRequest.setDescriptiveAttributeValueType(
      com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);
    productAttributeValueRequest.setDescriptiveAttributeValue(attributeValue);
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  public static NeedRevisionNotes getNeedRevisionNotes(PDTProductNotesDomainEventModel pdtProductNotesDomainEventModel,
    List<PDTItemNotesDomainEventModel> pdtItemNotesDomainEventModelList, ProductDetailResponse productDetailResponse,
    List<ProductBusinessPartner> productBusinessPartner) {
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    if (Objects.nonNull(pdtProductNotesDomainEventModel)) {
      needRevisionNotes.setVendorNotes(pdtProductNotesDomainEventModel.getVendorNotes());
      needRevisionNotes.setAllVariants(Boolean.TRUE.equals(pdtProductNotesDomainEventModel.getAllVariants()));
      needRevisionNotes.setContentAdditionalNotes(pdtProductNotesDomainEventModel.getContentAdditionalNotes());
      needRevisionNotes.setImageReason(pdtProductNotesDomainEventModel.getImageReason());
      needRevisionNotes.setCommonImageReason(pdtProductNotesDomainEventModel.getCommonImageReason());
      needRevisionNotes.setImagesAdditionalNotes(pdtProductNotesDomainEventModel.getImagesAdditionalNotes());
      needRevisionNotes.setVendorErrorFields(pdtProductNotesDomainEventModel.getVendorErrorFields());
    }
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(productBusinessPartner)) {
      Map<String, ProductItemResponse> mapOfSkuCodeAndItem =
        productDetailResponse.getProductItemResponses().stream().collect(Collectors.toMap(ProductItemResponse::getSkuCode, Function
          .identity()));
      Map<String, ProductItemBusinessPartner> mapOfItemIdAndItem =
        productBusinessPartner.get(0).getProductItemBusinessPartners().stream()
          .collect(Collectors.toMap(ProductItemBusinessPartner::getProductItemId, Function.identity(), (a, b) -> a));
      List<ItemNeedRevisionNotes> itemNeedRevisionNotesList = new ArrayList<>();
      for (PDTItemNotesDomainEventModel pdtItemNotesDomainEventModel : pdtItemNotesDomainEventModelList) {
        ItemNeedRevisionNotes itemNeedRevisionNotes = new ItemNeedRevisionNotes();
        itemNeedRevisionNotes.setVendorNotes(pdtItemNotesDomainEventModel.getVendorNotes());
        itemNeedRevisionNotes.setItemName(pdtItemNotesDomainEventModel.getItemName());
        itemNeedRevisionNotes.setSkuCode(pdtItemNotesDomainEventModel.getSkuCode());
        itemNeedRevisionNotes.setVendorErrorFields(pdtItemNotesDomainEventModel.getVendorErrorFields());
        ProductItemResponse productItemResponse = mapOfSkuCodeAndItem.get(pdtItemNotesDomainEventModel.getSkuCode());
        ProductItemBusinessPartner productItemBusinessPartner = mapOfItemIdAndItem.get(productItemResponse.getId());
        itemNeedRevisionNotes.setItemSku(productItemBusinessPartner.getGdnProductItemSku());
        itemNeedRevisionNotes.setItemNumber(Integer.parseInt(
            StringUtils.substringAfterLast(productItemBusinessPartner.getGdnProductItemSku(), Constants.DASH_DELIMITER)
                .replaceFirst(Constants.LEADINGZERO, StringUtils.EMPTY)));
        itemNeedRevisionNotesList.add(itemNeedRevisionNotes);
      }
      needRevisionNotes.setItemNotes(itemNeedRevisionNotesList);
    }
    return needRevisionNotes;
  }

  public static void setProductCollectionDetailsForNeedRevision(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      ProductCollection productCollection, boolean autoNeedRevision, boolean overrideDataFromPDT) {
    productCollection.setActivated(false);
    productCollection.setViewable(false);
    if (overrideDataFromPDT) {
      productCollection.setBrand(pdtDomainModelResponseByCode.getBrand());
      productCollection.setBrandCode(pdtDomainModelResponseByCode.getBrandCode());
      productCollection
          .setCategoryCode(pdtDomainModelResponseByCode.getProductCategories().get(0).getCategory().getCategoryCode());
      productCollection
          .setCategoryName(pdtDomainModelResponseByCode.getProductCategories().get(0).getCategory().getName());
    }
    productCollection.setReviewPending(true);
    productCollection.setState(ProductLevel1State.NEED_CORRECTION);
    if(autoNeedRevision) {
      productCollection.setAutoNeedRevision(true);
      productCollection.setNeedRevision(true);
      productCollection.setAutoNeedRevisionCount(productCollection.getAutoNeedRevisionCount() + 1);
    }
  }

  public static void refreshProductAndItemDetails(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      ProductBusinessPartner productBusinessPartner, boolean overrideDataFromPDT) {
    productBusinessPartner.setActivated(false);
    productBusinessPartner.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productBusinessPartner.setMarkForDelete(false);
    if (overrideDataFromPDT) {
      productBusinessPartner.setBrand(pdtDomainModelResponseByCode.getBrand());
      productBusinessPartner.setProductName(pdtDomainModelResponseByCode.getName());
      productBusinessPartner
          .setCategoryCode(pdtDomainModelResponseByCode.getProductCategories().get(0).getCategory().getCategoryCode());
      productBusinessPartner
          .setCategoryName(pdtDomainModelResponseByCode.getProductCategories().get(0).getCategory().getName());
    }
  }

  private static List<ImageQcProcessingDto> toImageQcConfidenceDetails(List<Image> editFlagBasedImages,
    Map<String, ImageQcResponse> mapOfHashCodeAndImage) {
    List<ImageQcProcessingDto> imageQcProcessingDtoList = new ArrayList<>();
    for (Image image : editFlagBasedImages) {
      ImageQcProcessingDto imageQcProcessingDto = new ImageQcProcessingDto();
      ImageQcResponse imageQcResponse = mapOfHashCodeAndImage.get(image.getHashCode());
      if (Objects.nonNull(imageQcResponse)) {
        imageQcProcessingDto.setLocationPath(imageQcResponse.getLocationPath());
        imageQcProcessingDto.setImageQcConfidenceDtoList(imageQcResponse.getPredictions().stream()
          .map(ConverterUtil::toImageQcConfidenceDto)
          .collect(Collectors.toList()));
        imageQcProcessingDtoList.add(imageQcProcessingDto);
      }
    }
    return imageQcProcessingDtoList;
  }

  private static ImageQcConfidenceDto toImageQcConfidenceDto(ImageQcPredictionResponse imageQcPredictionResponse) {
    ImageQcConfidenceDto imageQcConfidenceDto = new ImageQcConfidenceDto();
    imageQcConfidenceDto.setConfidence(String.valueOf(imageQcPredictionResponse.getConfidence()));
    imageQcConfidenceDto.setPredictionType(imageQcPredictionResponse.getPredictionType());
    imageQcConfidenceDto.setDisplayName(imageQcPredictionResponse.getDisplayName());
    return imageQcConfidenceDto;
  }

  public static AutoApprovalsDetailDto setAutoApprovalDetailDTO(String storeId, ProductCollection productCollection,
    String c1CategoryCode, List<Image> editFlagBasedImages,
    Map<String, ImageQcResponse> mapOfHashCodeAndImage) {
    AutoApprovalsDetailDto autoApprovalsDetailDto = new AutoApprovalsDetailDto();
    autoApprovalsDetailDto.setProductCode(productCollection.getProductCode());
    autoApprovalsDetailDto.setStoreId(storeId);
    autoApprovalsDetailDto.setUpdatedBy(productCollection.getUpdatedBy());
    autoApprovalsDetailDto.setC1CategoryCode(c1CategoryCode);
    autoApprovalsDetailDto.setMerchantCode(productCollection.getBusinessPartnerCode());
    autoApprovalsDetailDto.setImageQcConfidenceDetails(
      ConverterUtil.toImageQcConfidenceDetails(editFlagBasedImages, mapOfHashCodeAndImage));
    autoApprovalsDetailDto.setPostLive(productCollection.isPostLive());
    autoApprovalsDetailDto.setContentEditOnly(false);
    autoApprovalsDetailDto.setAutoApprovalType(productCollection.getAutoApprovalType());
    return autoApprovalsDetailDto;
  }

  public static AutoApprovalsDetailDto setAutoApprovalDetailDTOForNoImages(String storeId,
    ProductCollection productCollection, String c1CategoryCode) {
    AutoApprovalsDetailDto autoApprovalsDetailDto = new AutoApprovalsDetailDto();
    autoApprovalsDetailDto.setProductCode(productCollection.getProductCode());
    autoApprovalsDetailDto.setStoreId(storeId);
    autoApprovalsDetailDto.setUpdatedBy(productCollection.getUpdatedBy());
    autoApprovalsDetailDto.setC1CategoryCode(c1CategoryCode);
    autoApprovalsDetailDto.setMerchantCode(productCollection.getBusinessPartnerCode());
    autoApprovalsDetailDto.setImageQcConfidenceDetails(new ArrayList<>());
    autoApprovalsDetailDto.setPostLive(productCollection.isPostLive());
    autoApprovalsDetailDto.setAutoApprovalType(productCollection.getAutoApprovalType());
    return autoApprovalsDetailDto;
  }

  public static ProductLevel3WipResponse generateProductLevel3WipDTOResponse(ProductLevel3WipDTO productLevel3Wip) {
    ProductLevel3WipResponse productLevel3WipResponse = null;
    productLevel3WipResponse = new ProductLevel3WipResponse();
    BeanUtils.copyProperties(productLevel3Wip, productLevel3WipResponse, "forceReviewImageViolations");
    productLevel3WipResponse.setForceReviewImageViolations(
        Optional.ofNullable(productLevel3Wip.getForceReviewImageViolations()).orElse(new ArrayList<>()).stream().map(
            forceReviewImageViolation -> new ForceReviewImageViolationResponse(forceReviewImageViolation.getEnName(),
                forceReviewImageViolation.getInName())).collect(Collectors.toList()));
    return productLevel3WipResponse;
  }

  public static List<RestrictedKeywordsByFieldResponse> toRestrictedKeywordsByFieldResponseFromJson(
      String restrictedKeywordsByFieldJson) {
    List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList = new ArrayList<>();
    if (StringUtils.isNotBlank(restrictedKeywordsByFieldJson)) {
      try {
        restrictedKeywordsByFieldResponseList = new ObjectMapper()
            .readValue(restrictedKeywordsByFieldJson, new TypeReference<List<RestrictedKeywordsByFieldResponse>>() {
            });
      } catch (IOException e) {
        log.error("Error wilhe parsing restricted keyword json. jsonData : {} ", restrictedKeywordsByFieldJson, e);
      }
    }
    return restrictedKeywordsByFieldResponseList;
  }

  public static ProductLevel3Attribute toProductLevel3Attribute(CategoryAttributeResponse categoryAttributeResponse) {
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(categoryAttributeResponse.getAttribute().getAttributeCode());
    productLevel3Attribute.setAttributeName(categoryAttributeResponse.getAttribute().getName());
    productLevel3Attribute.setAttributeType(categoryAttributeResponse.getAttribute().getAttributeType());
    productLevel3Attribute.setSkuValue(categoryAttributeResponse.getAttribute().isSkuValue());
    productLevel3Attribute.setBasicView(categoryAttributeResponse.getAttribute().isBasicView());
    productLevel3Attribute.setMandatory(categoryAttributeResponse.getAttribute().isMandatory());
    productLevel3Attribute.setVariantCreation(categoryAttributeResponse.getAttribute().isVariantCreation());
    return productLevel3Attribute;
  }

  public static SimpleListStringRequest toSimpleListStringRequest(
      List<UpdatedProductHistory> updatedProductHistoryList) {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> itemSkuList = new ArrayList<>();
    updatedProductHistoryList.stream()
        .forEach(updatedProductHistory -> itemSkuList.add(updatedProductHistory.getGdnSku()));
    simpleListStringRequest.setValue(itemSkuList);
    return simpleListStringRequest;
  }

  public static Map<String, String> toItemSkuAndPickupPointCodeMap(
      List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList) {
    Map<String, String> itemSkuAndPickupPointCodeMap = new HashMap<>();
    itemSkuPickupPointCodeResponseList.stream().forEach(itemSkuPickupPointCodeResponse -> itemSkuAndPickupPointCodeMap
        .put(itemSkuPickupPointCodeResponse.getItemSku(), itemSkuPickupPointCodeResponse.getPickupPointCode()));
    return itemSkuAndPickupPointCodeMap;
  }

  public static List<BulkActivateDeactivateRequest> toBulkActivateDeactivateRequest(
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<ProductItemWholesalePrice> productItemWholesalePrices) {
    List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests = new ArrayList<>();
    for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePrices) {
      WholesalePriceSkuResponse wholesalePriceSkuResponse =
          wholesalePriceSkuResponseMap.get(productItemWholesalePrice.getItemSku());
      if (Objects.nonNull(wholesalePriceSkuResponse) && checkIsStatusChanged(
          productItemWholesalePrice.isWholesalePriceActivated(), wholesalePriceSkuResponse.getSkuStatus())) {
        bulkActivateDeactivateRequests.add(
            BulkActivateDeactivateRequest.builder().itemSku(productItemWholesalePrice.getItemSku()).updateSkuStatus(
                Boolean.TRUE.equals(productItemWholesalePrice.isWholesalePriceActivated()) ?
                    Constants.ACTIVE_STATUS :
                    Constants.INACTIVE_STATUS).build());
      }
    }
    return bulkActivateDeactivateRequests;
  }

  private static boolean checkIsStatusChanged(Boolean flag, String status) {
    return (Constants.INACTIVE_STATUS.equals(status) && Boolean.TRUE.equals(flag)) || (
        Constants.ACTIVE_STATUS.equals(status) && Boolean.FALSE.equals(flag));
  }

  public static PDTDimensionRefreshEventModel toPDTDimensionRefreshEventModel(String storeId, String productCode,
      DimensionRefreshRequest dimensionRefreshRequest) {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel = new PDTDimensionRefreshEventModel();
    BeanUtils.copyProperties(dimensionRefreshRequest, pdtDimensionRefreshEventModel);
    pdtDimensionRefreshEventModel.setStoreId(storeId);
    pdtDimensionRefreshEventModel.setProductCode(productCode);
    return pdtDimensionRefreshEventModel;
  }

  public static DimensionRefreshRequest toDimensionRefreshRequest(Double length, Double width, Double height,
      Double weight, Double shippingWeight, Integer dangerousGoodsLevel, Integer productType) {
    return DimensionRefreshRequest.builder().length(length).width(width).height(height).weight(weight)
        .shippingWeight(shippingWeight).dangerousGoodsLevel(dangerousGoodsLevel).productType(productType).build();
  }

  public static void toProductImageEditRequest(
      ProductImageEditRequest request, String productCode, Map<String, String> itemSkuItemCodeMap,
      com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequestPCB) {
    productImageEditRequestPCB.setProductCode(productCode);
    request.setProductCode(productCode);
    productImageEditRequestPCB.setImagePath(request.getImagePath());
    String hashCode = ApproveProductUtils.generateHashcodeByLocationPath(request.getImagePath());
    request.setHashCode(hashCode);
    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> productItems = new ArrayList<>();
      for (com.gda.mta.product.dto.ItemImageEditRequest imageEditRequest : request.getProductItems()) {
        com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest imageEditRequestPCB =
            new com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest();
        imageEditRequestPCB.setItemCode(itemSkuItemCodeMap.get(imageEditRequest.getItemSku()));
        imageEditRequestPCB.setMainImage(imageEditRequest.isMainImage());
        imageEditRequestPCB.setAdd(imageEditRequest.isAdd());
        imageEditRequestPCB.setCopy(imageEditRequest.isCopy());
        imageEditRequestPCB.setMarkForDelete(imageEditRequest.isMarkForDelete());
        imageEditRequestPCB.setHashCode(hashCode);
        if (imageEditRequestPCB.isAdd()) {
          request.setImageAdded(true);
        }
        if (imageEditRequestPCB.isCopy()) {
          request.setImageUpdated(true);
        }
        productItems.add(imageEditRequestPCB);
      }
      productImageEditRequestPCB.setProductItems(productItems);
    }

    if (Objects.nonNull(request.getCopyToAllVariantImages())) {
      com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest copyImageEditRequest =
          new com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest();
      copyImageEditRequest.setCopy(request.getCopyToAllVariantImages().isCopy());
      copyImageEditRequest.setMainImage(request.getCopyToAllVariantImages().isMainImage());
      copyImageEditRequest.setMarkForDelete(request.getCopyToAllVariantImages().isMarkForDelete());
      copyImageEditRequest.setAdd(request.getCopyToAllVariantImages().isAdd());
      copyImageEditRequest.setHashCode(hashCode);
      if (copyImageEditRequest.isAdd()) {
        request.setImageAdded(true);
      }
      if (copyImageEditRequest.isCopy()) {
        request.setImageUpdated(true);
      }
      productImageEditRequestPCB.setCopyToAllVariantImages(copyImageEditRequest);
    }
  }

  public static ApiErrorCode getErrorCode(String description) {
    if (description.equals(ApiErrorCode.IMAGE_NOT_PRESENT.getDesc())) {
      return ApiErrorCode.IMAGE_NOT_PRESENT;
    }
    if (description.equals(ApiErrorCode.MAX_IMAGE_REACHED.getDesc())) {
      return ApiErrorCode.MAX_IMAGE_REACHED;
    }
    if (description.equals(ApiErrorCode.CANNOT_DELETE_MAIN_IMAGE.getDesc())) {
      return ApiErrorCode.CANNOT_DELETE_MAIN_IMAGE;
    }
    if (description.equals(ApiErrorCode.IMAGE_ALREADY_EXISTS.getDesc())) {
      return ApiErrorCode.IMAGE_ALREADY_EXISTS;
    }
    if (description.equals(ApiErrorCode.ONLY_IMAGE_IN_ITEM.getDesc())) {
      return ApiErrorCode.ONLY_IMAGE_IN_ITEM;
    }
    return null;
  }

  public static MessageEmailRequest getEmailRequest(String mailTemplateId, String sender, String subject,
      Map<String, Object> templateParams, String messageIdentifierKey,
      String messageIdentifierValue, String emailTo) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(mailTemplateId), ErrorMessages.TEMPLATE_ID_NOT_SPECIFIED);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(mailTemplateId);
    email.setMessageFrom(sender);
    email.setMessageSubject(subject);
    email.setMessageTo(emailTo);
    Map<String, Object> mailVariables = new HashMap<>();
    if (Objects.nonNull(templateParams)) {
      mailVariables.put("obj", templateParams);
    }
    email.setVariables(mailVariables);
    email.setMessageIdentifierKey(messageIdentifierKey);
    email.setMessageIdentifierValue(messageIdentifierValue);
    return email;
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
    appNames.add(Constants.PBP);
    notificationKafka.setAppNames(appNames);
    return notificationKafka;
  }

  public static MessageEmailRequest getMessageEmailRequest(
      String messageId, String messageTo, String messageCC, String messageSubject,
      String messageIdentifierKey, String messageIdentifierValue, Map<String, Object> variables, String messageFrom) {
    MessageEmailRequest emailRequest = new MessageEmailRequest();
    emailRequest.setMessageId(messageId);
    emailRequest.setMessageFrom(messageFrom);
    emailRequest.setMessageTo(messageTo);
    emailRequest.setMessageCc(messageCC);
    emailRequest.setMessageSubject(messageSubject);
    emailRequest.setMessageIdentifierKey(messageIdentifierKey);
    emailRequest.setMessageIdentifierValue(messageIdentifierValue);
    emailRequest.setVariables(variables);
    return emailRequest;
  }

  public static Page<HistoryUpdateResponse> setPickupPointName(
    Page<HistoryUpdateResponse> historyUpdateResponses,
    Map<String, BusinessPartnerPickupPointResponse> pickupPointCodeMap) {
    for (HistoryUpdateResponse historyUpdateResponse : historyUpdateResponses) {
      if(!Constants.ALL_PICKUP_POINTS.equals(historyUpdateResponse.getPickupPointCode())) {
      if (Objects.isNull(pickupPointCodeMap.get(historyUpdateResponse.getPickupPointCode()))) {
        historyUpdateResponse.setPickupPointName(Constants.DASH_DELIMITER);
      } else {
        historyUpdateResponse.setPickupPointName(
          pickupPointCodeMap.get(historyUpdateResponse.getPickupPointCode()).getName());
      }
      }
    }
    return historyUpdateResponses;
  }

  public static ProductLevel3SummaryResponse toProductLevel3SummaryResponse(
    ItemResponseV2 itemResponseV2, ProductLevel3Inventory productLevel3Inventory) {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setProductSku(itemResponseV2.getProductSku());
    productLevel3SummaryResponse.setProductName(itemResponseV2.getProductName());
    productLevel3SummaryResponse.setItemSku(itemResponseV2.getItemSku());
    productLevel3SummaryResponse.setPickupPointCode(itemResponseV2.getPickUpPointCode());
    productLevel3SummaryResponse.setPickupPointName(itemResponseV2.getPickUpPointName());
    productLevel3SummaryResponse.setItemName(itemResponseV2.getItemName());
    productLevel3SummaryResponse.setSkuCode(itemResponseV2.getSkuCode());
    productLevel3SummaryResponse.setMerchantSku(itemResponseV2.getMerchantSku());
    productLevel3SummaryResponse.setCncActive(itemResponseV2.isCncActive());
    productLevel3SummaryResponse.setOriginalSellingPrice(itemResponseV2.getOriginalSellingPrice());
    productLevel3SummaryResponse.setPrices(
      toProductLevel3PriceResponse(itemResponseV2.getPrices()));
    productLevel3SummaryResponse.setViewConfigs(
      toProductLevel3ViewConfigResponse(itemResponseV2.getViewConfigs()));
    productLevel3SummaryResponse.setOff2OnActiveFlag(itemResponseV2.isOff2OnActiveFlag());
    if (Objects.nonNull(productLevel3Inventory)) {
      productLevel3SummaryResponse.setSynchronizeStock(productLevel3Inventory.isWebSyncStock());
      productLevel3SummaryResponse.setAvailableStockLevel2(
        productLevel3Inventory.getWebAvailable());
      productLevel3SummaryResponse.setAvailableStockLevel1(
        productLevel3Inventory.getWarehouseAvailable());
      productLevel3SummaryResponse.setMinimumStockLevel2(
          Optional.ofNullable(productLevel3Inventory.getWebMinAlert()).orElse(0));
    }
    productLevel3SummaryResponse.setB2BResponse(
        Objects.nonNull(itemResponseV2.getB2BResponse()) ? itemResponseV2.getB2BResponse() : new B2BResponse());
    setPreOrderValues(productLevel3SummaryResponse, itemResponseV2.getPreOrder());
    return productLevel3SummaryResponse;
  }

  private static void setPreOrderValues(ProductLevel3SummaryResponse productLevel3SummaryResponse,
      PreOrderDTO preOrderDTO) {
    if (Objects.nonNull(preOrderDTO)) {
      productLevel3SummaryResponse.setPreOrder(Optional.ofNullable(preOrderDTO.getIsPreOrder()).orElse(false));
      productLevel3SummaryResponse.setPreOrderDate(preOrderDTO.getPreOrderDate());
    }
  }

  private static List<ProductLevel3ViewConfigResponse> toProductLevel3ViewConfigResponse(
    List<ViewConfigResponse> viewConfigs) {
    List<ProductLevel3ViewConfigResponse> productLevel3ViewConfigResponseList = new ArrayList<>();
    for (ViewConfigResponse viewConfig : viewConfigs) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
        new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(viewConfig, productLevel3ViewConfigResponse);
      productLevel3ViewConfigResponseList.add(productLevel3ViewConfigResponse);
    }
    return productLevel3ViewConfigResponseList;
  }

  private static List<ProductLevel3PriceResponse> toProductLevel3PriceResponse(
    List<PriceResponse> priceResponseList) {
    List<ProductLevel3PriceResponse> productLevel3PriceResponseList = new ArrayList<>();
    for(PriceResponse priceResponse : priceResponseList) {
      ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(priceResponse, productLevel3PriceResponse);
      productLevel3PriceResponseList.add(productLevel3PriceResponse);
    }
    return productLevel3PriceResponseList;
  }

  public static ItemPickupPointQuickEditRequest toItemPickupPointListingUpdateRequestVo(
    QuickEditV2Request quickEditV2Request) {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
      new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(quickEditV2Request.getPickupPointCode());
    itemPickupPointQuickEditRequest.setItemSku(quickEditV2Request.getItemSku());
    if (Objects.nonNull(quickEditV2Request.getCncActive())) {
      itemPickupPointQuickEditRequest.setCncActivated(quickEditV2Request.getCncActive());
    }
    itemPickupPointQuickEditRequest.setOff2OnActiveFlag(quickEditV2Request.getOff2OnActiveFlag());
    itemPickupPointQuickEditRequest.setMerchantSku(quickEditV2Request.getSellerSku());
    itemPickupPointQuickEditRequest.setVersion(quickEditV2Request.getVersion());
    itemPickupPointQuickEditRequest.setStatus(quickEditV2Request.getStatus().name());
    if (Objects.nonNull(quickEditV2Request.getCncStatus())) {
      itemPickupPointQuickEditRequest.setCncStatus(quickEditV2Request.getCncStatus().name());
    }
    itemPickupPointQuickEditRequest.setWholeSaleActivated(quickEditV2Request.getWholeSaleActivated());
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(quickEditV2Request.getPrice().getSalePrice());
    priceDTO.setListPrice(quickEditV2Request.getPrice().getPrice());
    priceDTO.setChannel(quickEditV2Request.getPrice().getChannelId());
    itemPickupPointQuickEditRequest.setPrice(priceDTO);
    itemPickupPointQuickEditRequest.setMerchantSku(quickEditV2Request.getSellerSku());
    itemPickupPointQuickEditRequest.setFbbActivated(quickEditV2Request.isFbbActivated());
    if (Objects.nonNull(quickEditV2Request.getB2bFieldsRequest())) {
      B2bFields b2bFields = new B2bFields();
      b2bFields.setStatus(quickEditV2Request.getB2bFieldsRequest().getStatus().name());
      b2bFields.setManaged(quickEditV2Request.getB2bFieldsRequest().isManaged());
      b2bFields.setBasePrice(quickEditV2Request.getB2bFieldsRequest().getBasePrice());
      itemPickupPointQuickEditRequest.setB2bFields(b2bFields);
    }
    itemPickupPointQuickEditRequest.setScheduleUpdate(quickEditV2Request.isScheduleUpdate());
    return itemPickupPointQuickEditRequest;
  }

  public static List<WebInventoryBulkUpdatePickupPointCodeRequest>
      toWebInventoryBulkUpdatePickupPointCodeRequest(PickupPointUpdateRequest pickupPointUpdateRequest) {
    List<WebInventoryBulkUpdatePickupPointCodeRequest> requests = new ArrayList<>();
    for (PickupPointRequest pickupPointRequest : pickupPointUpdateRequest.getItemsPickupPoint()) {
      WebInventoryBulkUpdatePickupPointCodeRequest webInventoryBulkUpdatePickupPointCodeRequest =
          new WebInventoryBulkUpdatePickupPointCodeRequest();
      webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(pickupPointRequest.getPickupPointCode());
      webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(pickupPointRequest.getItemSku());
      webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(pickupPointUpdateRequest.getBusinessPartnerCode());
      requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
    }
    return requests;
  }

  public static ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> toWebInventoryUpdatePickupPointRequestDTO(
      String merchantCode, Map<String, String> itemXNewPickUpPointMap, Map<String, String> itemXOldPickUpPoint) {
    List<WebInventoryUpdatePickupPointRequestDTO> requests = new ArrayList<>();
    for (String itemSku : itemXNewPickUpPointMap.keySet()) {
      WebInventoryUpdatePickupPointRequestDTO webInventoryBulkUpdatePickupPointCodeRequest =
          new WebInventoryUpdatePickupPointRequestDTO();
      webInventoryBulkUpdatePickupPointCodeRequest.setNewPickupPointCode(itemXNewPickUpPointMap.get(itemSku));
      webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(itemSku);
      webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(itemXOldPickUpPoint.get(itemSku));
      webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(merchantCode);
      requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
    }
    return new ListRequestDTO<>(requests);
  }

  public static List<WebInventoryBulkUpdatePickupPointCodeRequest>
  toWebInventoryBulkUpdatePickupPointCodeRequest(String merchantCode, Map<String, String> itemXNewPickUpPointMap) {
    List<WebInventoryBulkUpdatePickupPointCodeRequest> requests = new ArrayList<>();
    for (String itemSku : itemXNewPickUpPointMap.keySet()) {
      WebInventoryBulkUpdatePickupPointCodeRequest webInventoryBulkUpdatePickupPointCodeRequest =
          new WebInventoryBulkUpdatePickupPointCodeRequest();
      webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(itemXNewPickUpPointMap.get(itemSku));
      webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(itemSku);
      webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(merchantCode);
      requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
    }
    return requests;
  }

  public static ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> toWebInventoryUpdatePickupPointRequestDTO(
      PickupPointUpdateRequest pickupPointUpdateRequest, Map<String, Pair<String,String>> itemXOldPickUpPoint) {
    List<WebInventoryUpdatePickupPointRequestDTO> requests = new ArrayList<>();
    for (PickupPointRequest pickupPointRequest : pickupPointUpdateRequest.getItemsPickupPoint()) {
      WebInventoryUpdatePickupPointRequestDTO webInventoryBulkUpdatePickupPointCodeRequest =
          new WebInventoryUpdatePickupPointRequestDTO();
      webInventoryBulkUpdatePickupPointCodeRequest.setNewPickupPointCode(pickupPointRequest.getPickupPointCode());
      webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(pickupPointRequest.getItemSku());
      webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(
          Optional.ofNullable(itemXOldPickUpPoint.get(pickupPointRequest.getItemSku())).map(Pair::getLeft)
              .orElse(null));
      webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(
          pickupPointUpdateRequest.getBusinessPartnerCode());
      requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
    }
    return new ListRequestDTO<>(requests);
  }

  public static WebInventoryUpdatePickupPointResponseDTO toWebInventoryBulkUpdatePickupPointCodeResponseDTO(
      WebInventoryUpdatePickupPointResponseDTO updatePickupPointResponse) {
    WebInventoryUpdatePickupPointResponseDTO webInventoryBulkUpdatePickupPointCodeResponseDTO =
        new WebInventoryUpdatePickupPointResponseDTO();
    List<WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError> errors = new ArrayList<>();
    for (WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error : updatePickupPointResponse.getErrors()){
      WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError updatedError
          = new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
      updatedError.setPickupPointCode(error.getPickupPointCode());
      updatedError.setWebMerchantCode(error.getWebMerchantCode());
      updatedError.setWebItemSku(error.getWebItemSku());
      updatedError.setErrorCode(error.getErrorCode());
      updatedError.setErrorMessage(error.getErrorMessage());
      errors.add(updatedError);
    }
    webInventoryBulkUpdatePickupPointCodeResponseDTO.setErrors(errors);
    return webInventoryBulkUpdatePickupPointCodeResponseDTO;
  }

  public static List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> toProductImageEditRequestList(
      List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, boolean needCorrection, boolean activatedBefore, String productCode) {
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestList =
        new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : copyToAllVariantImages) {
      com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequestPCB =
          new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();
      productImageEditRequestPCB.setNeedRevision(needCorrection);
      productImageEditRequestPCB.setImagePath(productLevel3SummaryDetailsImageRequest.getLocationPath());
      productImageEditRequestPCB.setProductCode(productCode);
      CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
      if (NEW.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        copyImageEditRequest.setAdd(true);
        copyImageEditRequest.setMainImage(productLevel3SummaryDetailsImageRequest.getMainImage());
      } else if (UPDATE.equalsIgnoreCase(productLevel3SummaryDetailsImageRequest.getReviewType())) {
        if (Objects.nonNull(productLevel3SummaryDetailsImageRequest.getMarkForDelete())) {
          copyImageEditRequest.setMarkForDelete(productLevel3SummaryDetailsImageRequest.getMarkForDelete());
        }
        if (Objects.nonNull(productLevel3SummaryDetailsImageRequest.getMainImage())) {
          copyImageEditRequest.setMainImage(productLevel3SummaryDetailsImageRequest.getMainImage());
        }
      }

      if (!copyImageEditRequest.isAdd() && !copyImageEditRequest.isMainImage()
        && !copyImageEditRequest.isMarkForDelete()) {
        continue;
      }
      copyImageEditRequest.setHashCode(ApproveProductUtils
          .generateHashcodeByLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath()));
      productImageEditRequestPCB.setCopyToAllVariantImages(copyImageEditRequest);
      productImageEditRequestPCB.setActivatedBefore(activatedBefore);
      productImageEditRequestPCB.setMarkForDelete(copyImageEditRequest.isMarkForDelete());
      productImageEditRequestList.add(productImageEditRequestPCB);
    }
    return productImageEditRequestList;
  }

  public static void setOfferPriceForPartialUpdate(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse itemSummaryListResponse, ItemRequest itemRequest) {
    Set<PriceDTO> price = itemSummaryListResponse.getPrice();
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    for (PriceDTO productLevel3Price : price) {
      productLevel3PriceRequest.setSalePrice(
        Optional.ofNullable(productLevel3Price.getOfferPrice()).orElse(null));
      productLevel3PriceRequest.setPrice(
        Optional.ofNullable(productLevel3Price.getListPrice()).orElse(null));
      productLevel3PriceRequest.setChannelId(
        Optional.ofNullable(productLevel3Price.getChannel()).orElse(null));
      if (Optional.ofNullable(productLevel3Price.getMerchantPromoDiscountPrice()).isPresent()) {
        productLevel3PriceRequest.setDiscountAmount(
          Optional.ofNullable(productLevel3Price.getMerchantPromoDiscountPrice().getDiscountPrice())
            .orElse(null));
        productLevel3PriceRequest.setDiscountEndDate(
          Optional.ofNullable(productLevel3Price.getMerchantPromoDiscountPrice().getEndDateTime())
            .orElse(null));
        productLevel3PriceRequest.setDiscountStartDate(
          Optional.ofNullable(productLevel3Price.getMerchantPromoDiscountPrice().getStartDateTime())
            .orElse(null));
      }
    }
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemRequest.setPrice(ConverterUtil.setOfferPrice(Arrays.asList(quickEditV2Request.getPrice())));
  }

  public static List<ItemPickupPointDeleteRequest> getDeletePickupPointDeleteRequest(
    ProductVariantUpdateRequest productVariantUpdateRequest) {
    List<ItemPickupPointDeleteRequest> itemPickupPointDeleteRequests = new ArrayList<>();
    for (PickupPointDeleteRequest pickupPointDeleteRequest : productVariantUpdateRequest
      .getDeletePickupPoints()) {
      ItemPickupPointDeleteRequest itemPickupPointDeleteRequest =
        new ItemPickupPointDeleteRequest();
      BeanUtils
        .copyProperties(pickupPointDeleteRequest, itemPickupPointDeleteRequest, "pickupPointId");
      itemPickupPointDeleteRequest.setPickupPointCode(pickupPointDeleteRequest.getPickupPointId());
      itemPickupPointDeleteRequests.add(itemPickupPointDeleteRequest);
    }
    return itemPickupPointDeleteRequests;
  }

  public static ProductRequest getProductRequestForProductDiscard(String createdBy, Date createdDate, String productId) {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setCreatedBy(createdBy);
    productRequest.setCreatedDate(createdDate);
    productRequest.setId(productId);
    return productRequest;
  }

  public static void setDimensionDetails(Product product, ProductLevel3 productLevel3) {
    if (Objects.nonNull(productLevel3.getHeight())) {
      product.setHeight(productLevel3.getHeight());
    }

    if (Objects.nonNull(productLevel3.getLength())) {
      product.setLength(productLevel3.getLength());
    }

    if (Objects.nonNull(productLevel3.getWidth())) {
      product.setWidth(productLevel3.getWidth());
    }

    if (Objects.nonNull(productLevel3.getWeight())) {
      product.setWeight(productLevel3.getWeight());
    }

    if (Objects.nonNull(productLevel3.getShippingWeight())) {
      product.setShippingWeight(productLevel3.getShippingWeight());
    }
  }

  public static ProductDataAutoFixHistoryListRequest convertToProductDataAutoFixHistoryListRequest(String productCode,
      String type, String additionalInfo) {
    ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistoryDto.setType(type);
    productDataAutoFixHistoryDto.setProductCode(productCode);
    productDataAutoFixHistoryDto.setAdditionalInfo(additionalInfo);
    List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList = new ArrayList<>();
    productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
    return new ProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDtoList);
  }

  public static InternalProductHistoryEventModel generateInternalProductHistoryEventModel(String productCode,
      String activity, String userName, String notes) {
    InternalProductHistoryEventModel internalProductHistoryEventModel = new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setProductCode(productCode);
    internalProductHistoryEventModel.setUsername(userName);
    internalProductHistoryEventModel.setNotes(notes);
    internalProductHistoryEventModel.setActivity(activity);
    internalProductHistoryEventModel.setStoreId(Constants.DEFAULT_STORE_ID);
    return internalProductHistoryEventModel;
  }

  public static void setProductBusinessPartnerForCategoryChange(ProductCollection productCollection,
      ProductBusinessPartner productBusinessPartner) {
    productBusinessPartner.setCategoryName(productCollection.getCategoryName());
    productBusinessPartner.setCategoryCode(productCollection.getCategoryCode());
    productBusinessPartner.setUpdatedDate(Calendar.getInstance().getTime());
  }

  public static void setProductCollectionDetailsForCategoryChange(ProductCollection productCollection,
      CategorySummaryResponse response) {
    productCollection.setUpdatedDate(new Date());
    productCollection.setCategoryCode(response.getCategoryCode());
    productCollection.setCategoryName(response.getCategoryName());
  }

  public static void setProductCollectionDetailsForCategoryChangeNeedRevision(ProductCollection productCollection,
      CategoryRestrictedKeywordResponse messageById, CategorySummaryResponse response) {
    productCollection.setUpdatedDate(new Date());
    productCollection.setCategoryCode(messageById.getDestinationCategory());
    productCollection.setCategoryName(response.getCategoryName());
  }

  public static List<ItemSummaryL4Response> toItemSummaryL4Response(ProductBusinessPartner productBusinessPartner,
      List<ItemSkuToItemIdMapping> itemSkuToItemIdMappings,
      Map<String, ProductItemResponse> itemIdToProductItemResponseMap,
      Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap) throws JsonProcessingException {
    List<ItemSummaryL4Response> itemSummaryL4ResponseList = new ArrayList<>();
    for (ItemSkuToItemIdMapping itemSkuToItemIdMapping : itemSkuToItemIdMappings) {
      ProductItemResponse productItemResponse =
          itemIdToProductItemResponseMap.get(itemSkuToItemIdMapping.getProductItemId());
      ItemSummaryL4Response itemSummaryL4Response = new ItemSummaryL4Response();
      itemSummaryL4Response.setProductSku(productBusinessPartner.getGdnProductSku());
      itemSummaryL4Response.setCategoryCode(productBusinessPartner.getCategoryCode());
      itemSummaryL4Response.setBrand(productBusinessPartner.getBrand());
      itemSummaryL4Response.setMerchantCode(productBusinessPartner.getBusinessPartnerId());
      itemSummaryL4Response.setItemSku(itemSkuToItemIdMapping.getGdnProductItemSku());
      itemSummaryL4Response.setSellerSku(itemSkuToItemIdMapping.getMerchantSku());
      itemSummaryL4Response.setItemCode(productItemResponse.getSkuCode());
      itemSummaryL4Response.setGeneratedItemName(productItemResponse.getGeneratedItemName());
      itemSummaryL4Response.setUpcCode(productItemResponse.getUpcCode());
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap =
          productBusinessPartner.getProductItemBusinessPartners().stream().collect(Collectors
              .toMap(ProductItemBusinessPartner::getGdnProductItemSku, Function.identity(),
                  (existing, replacement) -> existing));
      itemSummaryL4Response.setBundleItemResponses(getBundleInfo(itemBasicDetailV2ResponseMap,
          productItemBusinessPartnerMap.get(itemSkuToItemIdMapping.getGdnProductItemSku())));
      Image mainImage =
          productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete()).filter(Image::isMainImages)
              .filter(ConverterUtil::filterProcessedProductImages).findFirst().orElse(new Image());
      itemSummaryL4Response.setMainImageUrl(mainImage.getLocationPath());
      itemSummaryL4Response.setActive(mainImage.isActive());
      ItemBasicDetailV2Response itemBasicDetailV2Response = itemBasicDetailV2ResponseMap.get(itemSkuToItemIdMapping.getGdnProductItemSku());
      if(Objects.nonNull(itemBasicDetailV2Response)) {
        itemSummaryL4Response.setSharedProduct(itemBasicDetailV2Response.isSharedProduct());
      }

      itemSummaryL4ResponseList.add(itemSummaryL4Response);
    }
    return itemSummaryL4ResponseList;
  }

  private static List<BundleItemResponse> getBundleInfo(Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap,
      ProductItemBusinessPartner productItemBusinessPartner) throws JsonProcessingException {
    List<ProductBundleRecipe> productBundleRecipes = new ArrayList<>();
    List<BundleItemResponse> bundleItemResponses = new ArrayList<>();
    if (StringUtils.isNotEmpty(productItemBusinessPartner.getBundleRecipe())) {
      productBundleRecipes =
          objectMapper.readValue(productItemBusinessPartner.getBundleRecipe(), new TypeReference<>() {
          });
    }
    for (ProductBundleRecipe productBundleRecipe : productBundleRecipes) {
      BundleItemResponse bundleItemResponse = new BundleItemResponse();
      ItemBasicDetailV2Response itemBasicDetailV2Response;
      if (Objects.nonNull(itemBasicDetailV2ResponseMap.get(productBundleRecipe.getItemSku()))) {
        itemBasicDetailV2Response = itemBasicDetailV2ResponseMap.get(productBundleRecipe.getItemSku());
        bundleItemResponse.setItemName(itemBasicDetailV2Response.getGeneratedItemName());
        bundleItemResponse.setItemCode(itemBasicDetailV2Response.getItemCode());
        bundleItemResponse.setMainImageUrl(itemBasicDetailV2Response.getMainImageUrl());
        bundleItemResponse.setProductStatus(getProductStatus(itemBasicDetailV2Response));
        bundleItemResponse.setProductSku(itemBasicDetailV2Response.getProductSku());
        bundleItemResponse.setSharedProduct(itemBasicDetailV2Response.isSharedProduct());
      }
      bundleItemResponse.setQuantity(productBundleRecipe.getQuantity());
      bundleItemResponse.setItemSku(productBundleRecipe.getItemSku());
      bundleItemResponses.add(bundleItemResponse);
    }
    return bundleItemResponses;
  }

  public static String getProductStatus(ItemBasicDetailV2Response itemBasicDetailV2Response) {
    if (itemBasicDetailV2Response.isArchived() || itemBasicDetailV2Response.isMarkForDelete()) {
      return ProductStatus.INACTIVE.name();
    }
    return ProductStatus.ACTIVE.name();
  }

  public static boolean filterProcessedProductImages(Image image) {
    if (image.isEdited()) {
      return image.isActive();
    }
    if (image.isRevised()) {
      return image.isActive();
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return !image.getOriginalImage();
    }
  }

  public static InternalProductHistoryEventModel createInternalProductHistoryEventModel(
      ProductCollection productCollection, String oldCategoryName, String keyword) {
    InternalProductHistoryEventModel internalProductHistoryEventModel = new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(productCollection.getStoreId());
    internalProductHistoryEventModel.setProductCode(productCollection.getProductCode());
    internalProductHistoryEventModel.setActivity(ProductWorkflowLookup.STATE_EDIT_DESCRIPTION);
    internalProductHistoryEventModel.setUsername(SaveHistoryConstants.AUTO_CATEGORY_CHANGE);
    List<ProductFieldHistory> productFieldHistoryList = new ArrayList<>();
    productFieldHistoryList.add(
        new ProductFieldHistory(SaveHistoryConstants.CATEGORY, oldCategoryName, productCollection.getCategoryName()));
    if (StringUtils.isNotEmpty(keyword)) {
      productFieldHistoryList.add(
          new ProductFieldHistory(SaveHistoryConstants.KEYWORD, keyword, StringUtils.EMPTY));
    }
    internalProductHistoryEventModel.setNotes(String.valueOf(productFieldHistoryList));
    return internalProductHistoryEventModel;
  }

  public static CreateFbbPickupPointRequest toCreateFbbPickupPointRequest(
      FbbCreatePickupPointRequest fbbCreatePickupPointRequest) {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setBusinessPartnerCode(fbbCreatePickupPointRequest.getBusinessPartnerCode());
    createFbbPickupPointRequest.setItemSku(fbbCreatePickupPointRequest.getItemSku());
    createFbbPickupPointRequest.setPickupPointCode(fbbCreatePickupPointRequest.getPickupPointId());
    createFbbPickupPointRequest.setDiscoverable(fbbCreatePickupPointRequest.isDiscoverable());
    createFbbPickupPointRequest.setBuyable(fbbCreatePickupPointRequest.isBuyable());
    return createFbbPickupPointRequest;
  }

  public static boolean isMPPEnabled(ProfileResponse profileResponse, String mppAllowedSellers) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && (
      (profileResponse.getCompany().isCncActivated()) || (
        Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) && mppAllowedSellers
          .contains(profileResponse.getCompany().getMerchantType())));
  }

  public static ProductLevel3 generateProductLevel3FromProductDetailResponse(ProductCollection productCollection,
      ProductDetailResponse productDetailResponse) {
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setStoreId(productCollection.getStoreId());
    productLevel3.setId(productDetailResponse.getId());
    productLevel3.setBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    productLevel3.setProductCode(productCollection.getProductCode());
    productLevel3.setBrand(productDetailResponse.getBrand());
    productLevel3.setProductName(productDetailResponse.getName());
    productLevel3.setDescription(new String(productDetailResponse.getDescription()));
    productLevel3.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    return productLevel3;
  }

  public static void overrideRestrictedKeywordPresentFlagBasedOnDsResponse(
      ImageQcResponseDomainEvent imageQcResponseDomainEvent, AutoApprovalsDetailDto autoApprovalsDetailDto,
      ProductCollection productCollection) {
    if (Objects.nonNull(imageQcResponseDomainEvent) && CollectionUtils.isNotEmpty(
        imageQcResponseDomainEvent.getKeywordRestrictionModels())) {
      KeywordRestrictionModelsResponse keywordRestrictionModelsResponse =
          imageQcResponseDomainEvent.getKeywordRestrictionModels().get(0);
      if (CollectionUtils.isNotEmpty(keywordRestrictionModelsResponse.getKeywordRecommendations())) {
        boolean allDetectionInvalid = keywordRestrictionModelsResponse.getKeywordRecommendations().stream().noneMatch(
            keywordRecommendationsResponse -> !keywordRecommendationsResponse.isSkipKeyword()
                && !INVALID_DETECTION.equals(keywordRecommendationsResponse.getRecommendation()));
        if (allDetectionInvalid) {
          log.info("Overriding restricted keyword present flag productCode: {} ",
              imageQcResponseDomainEvent.getProductCode());
          autoApprovalsDetailDto.setRestrictedKeywordPresent(false);
          if (Objects.nonNull(productCollection)) {
            productCollection.setRestrictedKeywordsPresent(false);
          }
        }
      }
    }
  }

  public static boolean skipAllActionsUnlessCategoryChange(boolean trustedSeller,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    return RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
        != restrictedKeywordsByFieldAndActionType.getAction() && trustedSeller;
  }

  public static boolean isSkipAllAction(
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails) {
    return restrictedKeywordsWithActionTypeInProductDetails.isSkipAllActions()
        || restrictedKeywordsWithActionTypeInProductDetails.getAction()
        == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType();
  }

  public static HalalProductHistoryResponse toHalalProductHistoryResponse(
      HalalProductHistory halalProductHistory) {
    HalalProductHistoryResponse halalProductHistoryResponse = new HalalProductHistoryResponse();
    BeanUtils.copyProperties(halalProductHistory, halalProductHistoryResponse);
    return halalProductHistoryResponse;
  }

  public static ProductCollection convertDtoToProductCollection(ProductCollectionDTO productCollectionDTO) {
    ProductCollection productCollection = new ProductCollection();
    if(Objects.nonNull(productCollectionDTO)) {
      BeanUtils.copyProperties(productCollectionDTO, productCollection, "brandApprovalStatus",
        "autoApprovalType");
      productCollection.setBrandApprovalStatus(productCollectionDTO.getBrandApprovalStatus());
      productCollection.setAutoApprovalType(productCollectionDTO.getAutoApprovalType());
    }
    return productCollection;
  }

  public static ProductCollectionDTO convertProductCollectionToDto(
    ProductCollection productCollection) {
    ProductCollectionDTO productCollectionDTO = new ProductCollectionDTO();
    if (Objects.nonNull(productCollection)) {
      BeanUtils.copyProperties(productCollection, productCollectionDTO, "brandApprovalStatus",
        "autoApprovalType");
      productCollectionDTO.setBrandApprovalStatus(productCollection.getBrandApprovalStatus());
      productCollectionDTO.setAutoApprovalType(productCollection.getAutoApprovalType());
      return productCollectionDTO;
    }
    return new ProductCollectionDTO();
  }

  public static Set<BundleRecipeVo> toBundleRecipeVoResponseFromJson(String bundleRecipe) {
    Set<BundleRecipeVo> bundleRecipeVoSet = new HashSet<>();
    if (StringUtils.isNotBlank(bundleRecipe)) {
      try {
        bundleRecipeVoSet = new ObjectMapper().readValue(bundleRecipe, new TypeReference<>() {
        });
      } catch (IOException e) {
        log.error("Error wilhe parsing restricted keyword json. jsonData : {} ", bundleRecipe, e);
      }
    }
    return bundleRecipeVoSet;
  }

  public static String getNotificationDetailForProductActivation(String productSku, boolean isBundleProduct) {
    return isBundleProduct ? String.format(Constants.BUNDLE_ACTIVE_PRODUCT_URL, productSku) : productSku;
  }

  public static Map<String, String> getItemIdToItemCodeMap(ProductDetailResponse productDetailResponse) {
    Map<String, String> itemIdToItemCodeMap = new HashMap<>();
    for (ProductItemResponse productItemResponse : Optional.ofNullable(productDetailResponse.getProductItemResponses())
        .orElse(new HashSet<>())) {
      itemIdToItemCodeMap.put(productItemResponse.getId(), productItemResponse.getSkuCode());
    }
    return itemIdToItemCodeMap;
  }

  public static List<String> getDistinctRecipeItemSkus(List<ProductBundleRecipeRequest> productBundleRecipe) {
    return Stream.concat(Optional.ofNullable(productBundleRecipe).orElseGet(() -> new ArrayList<>()).stream()
            .map(ProductBundleRecipeRequest::getItemSku),
        Optional.ofNullable(productBundleRecipe).orElseGet(() -> new ArrayList<>()).stream()
            .map(ProductBundleRecipeRequest::getBundleRecipe).flatMap(Collection::stream)
            .map(ProductBundleRecipe::getItemSku)).distinct().collect(Collectors.toList());
  }

  public static void addItemSkuAndItemCodeToMap(List<ItemBasicDetailV2Response> itemBasicDetailV2ResponseList,
      Map<String, String> itemSkuAndItemCodeMap) {
    for (ItemBasicDetailV2Response itemBasicDetailV2Response : itemBasicDetailV2ResponseList) {
      itemSkuAndItemCodeMap.put(itemBasicDetailV2Response.getItemSku(), itemBasicDetailV2Response.getItemCode());
    }
  }

  public static ProductItemImageRequest toNewProductItemImageRequestFromL5Request(
      List<ProductLevel3SummaryDetailsImageRequest> images, String skuCode, boolean needCorrection) {
    return toNewProductItemImageRequest(images, skuCode, needCorrection);
  }

  public static ProductBusinessPartnerCounter generateBusinessPartnerCounter(String storeId,
    String businessPartnerCode) {
    ProductBusinessPartnerCounter businessPartnerCounter = new ProductBusinessPartnerCounter();
    businessPartnerCounter.setStoreId(storeId);
    businessPartnerCounter.setBusinessPartnerCode(businessPartnerCode);
    businessPartnerCounter.setAppealedProductCount(0);
    businessPartnerCounter.setCreatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    businessPartnerCounter.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    return businessPartnerCounter;
  }

  public static void unifyProductAttributeAndProductAttributeValueTypeRequest(ProductCreationRequest productCreationRequest,
      boolean valueTypeAdditionForDefiningAttributes, String sizeChartValueTypeDelimiter) {
    if (valueTypeAdditionForDefiningAttributes) {
      Map<String, String> attributeCodeAndValueTypeMapping =
          unifyAttributeMapAndAttributeValueType(productCreationRequest, sizeChartValueTypeDelimiter);
      unifyProductAttributeAndAttributeValueType(productCreationRequest, attributeCodeAndValueTypeMapping,
          sizeChartValueTypeDelimiter);
    }
  }

  private static void unifyProductAttributeAndAttributeValueType(ProductCreationRequest productCreationRequest,
      Map<String, String> attributeCodeAndValueTypeMapping, String sizeChartValueTypeDelimiter) {
    List<ProductAttributeRequest> productAttributeRequests =
        Optional.ofNullable(productCreationRequest.getProductAttributes()).orElse(new ArrayList<>());
    for (ProductAttributeRequest productAttributeRequest : productAttributeRequests) {
      if (attributeCodeAndValueTypeMapping.containsKey(productAttributeRequest.getAttribute().getAttributeCode())
          && org.apache.commons.collections.CollectionUtils.isNotEmpty(
          productAttributeRequest.getProductAttributeValues())) {
        for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest.getProductAttributeValues()) {
          if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue()) && StringUtils.isNotEmpty(
              productAttributeValueRequest.getAllowedAttributeValue().getValue())) {
            productAttributeValueRequest.getAllowedAttributeValue().setValue(combineAttributeValueAndValueType(
                attributeCodeAndValueTypeMapping.get(productAttributeRequest.getAttribute().getAttributeCode()),
                productAttributeValueRequest.getAllowedAttributeValue().getValue(), sizeChartValueTypeDelimiter));
          }
        }
      }
    }
  }

  private static Map<String, String> unifyAttributeMapAndAttributeValueType(ProductCreationRequest productCreationRequest,
      String sizeChartValueTypeDelimiter) {
    Map<String, String> attributeCodeAndValueTypeMapping = new TreeMap<>();
    List<ProductItemCreationRequest> productItemCreationRequests =
        Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>());
    for (ProductItemCreationRequest productItemCreationRequest : productItemCreationRequests) {
      Map<String, String> attributesMap =
          Optional.ofNullable(productItemCreationRequest.getAttributesMap()).orElse(new TreeMap<>());
      Map<String, String> attributesValueTypeMap =
          Optional.ofNullable(productItemCreationRequest.getAttributesValueTypeMap()).orElse(new TreeMap<>());
      if (org.apache.commons.collections.MapUtils.isNotEmpty(attributesValueTypeMap)) {
        for (Map.Entry<String, String> attributeCodeAndValueTypeEntry : attributesValueTypeMap.entrySet()) {
          if (attributesMap.containsKey(attributeCodeAndValueTypeEntry.getKey())) {
            attributesMap.put(attributeCodeAndValueTypeEntry.getKey(),
                combineAttributeValueAndValueType(attributeCodeAndValueTypeEntry.getValue(),
                    attributesMap.get(attributeCodeAndValueTypeEntry.getKey()), sizeChartValueTypeDelimiter));
          }
        }
        attributeCodeAndValueTypeMapping.putAll(attributesValueTypeMap);
      }
    }
    return attributeCodeAndValueTypeMapping;
  }

  private static String combineAttributeValueAndValueType(String valueType, String value, String sizeChartValueTypeDelimiter) {
    return valueType + sizeChartValueTypeDelimiter + value;
  }

  public static ProductSkuDetailResponse getProductSkuDetailResponse(String productSku,
    ProductCollection productCollection, ProductCenterDetailResponse productCenterDetailResponse) {
    ProductSkuDetailResponse response = new ProductSkuDetailResponse();
    response.setProductCode(productCollection.getProductCode());
    response.setProductSku(productSku);
    response.setProductName(productCollection.getProductName());
    response.setBrandName(productCollection.getBrand());
    response.setBrandCode(productCollection.getBrandCode());
    response.setCategoryCode(productCollection.getCategoryCode());
    response.setCategoryName(productCollection.getCategoryName());
    response.setBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    response.setBusinessPartnerName(productCollection.getBusinessPartnerName());
    if (productCollection.isMarkForDelete()) {
      response.setRejected(true);
    }
    if (SUSPENDED.equals(productCenterDetailResponse.getStatus())) {
      response.setSuspended(true);
    }
    if (!productCollection.isPostLive() && !Constants.ACTIVE.equals(productCollection.getState())) {
      response.setPrelive(true);
    }
    response.setImageUrl(productCenterDetailResponse.getImagePath());
    if (!Constants.ACTIVE.equals(productCollection.getState())
      || productCenterDetailResponse.isMarkForDelete()) {
      response.setMarkForDelete(true);
    }
    return response;
  }

  public static boolean checkBrandCategoryTwentyOnePlusViolation(
    ProductImageQcProcessingResponse productImageQcProcessingResponse) {
    return StringUtils.isNotEmpty(productImageQcProcessingResponse.getPredictedBrand())
      || productImageQcProcessingResponse.getImageViolations().contains(Constants.CATEGORY_MISMATCH)
      || productImageQcProcessingResponse.getTextViolations()
      .contains(Constants.TWENTY_ONE_PRODUCT);
  }

  public static DistributionInfoUpdateRequest getDistributionInfoUpdateRequest(
      DistributionInfoRequest distributionInfoRequest) {
    DistributionInfoUpdateRequest distributionInfoUpdateRequest =
        new DistributionInfoUpdateRequest();
    distributionInfoUpdateRequest.setSellerCode(distributionInfoRequest.getSellerCode());
    distributionInfoUpdateRequest.setDistributionInfoRequest(
        distributionInfoRequest.getDistributionInfoRequest());
    distributionInfoUpdateRequest.setProductItemUomInfoDTOS(
        distributionInfoRequest.getProductItems().stream().map(
            productItemDistributionInfoRequest -> getProductItemUomInfoDTO(
                productItemDistributionInfoRequest)).collect(Collectors.toList()));
    return distributionInfoUpdateRequest;
  }

  public static ProductItemUomInfoDTO getProductItemUomInfoDTO(
      ProductItemDistributionInfoRequest productItemDistributionInfoRequest) {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setSkuCode(productItemDistributionInfoRequest.getSkuCode());
    productItemUomInfoDTO.setDistributionItemInfoRequest(DistributionItemInfoRequest.builder()
        .expiry(productItemDistributionInfoRequest.getDistributionItemInfoRequest().isExpiry())
        .omniChannelSku(
            productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku())
        .origin(productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOrigin())
        .build());
    productItemUomInfoDTO.setDimensionAndUomDTOList(
        productItemDistributionInfoRequest.getDimensionsAndUOMRequest().stream()
            .map(dimensionAndUomRequest -> getDimensionAndUomDTO(dimensionAndUomRequest))
            .collect(Collectors.toList()));
    return productItemUomInfoDTO;
  }

  public static DimensionAndUomDTO getDimensionAndUomDTO(
      DimensionAndUomRequest dimensionAndUomRequest) {
    DimensionAndUomDTO dimensionAndUomDTO = new DimensionAndUomDTO();
    BeanUtils.copyProperties(dimensionAndUomRequest, dimensionAndUomDTO);
    return dimensionAndUomDTO;
  }
}

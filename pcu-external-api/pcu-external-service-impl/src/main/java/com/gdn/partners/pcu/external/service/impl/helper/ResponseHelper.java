package com.gdn.partners.pcu.external.service.impl.helper;

import static java.util.stream.Collectors.toList;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.service.impl.exception.EditProductException;
import com.gdn.partners.pcu.external.service.impl.exception.ProductListingGenericException;
import com.gdn.partners.pcu.external.web.model.request.B2bFields;
import com.gdn.partners.pcu.external.web.model.response.BundleChildRecipeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BundleItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BuyableScheduleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentStatusWeb;
import com.gdn.partners.pcu.external.web.model.response.DiscoverableScheduleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.DistributionInfoWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventoryWarehouseStockWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemAndPickupPointBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemCodeBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryStockInfoDTO;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;

import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.PickupPointResponse;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductItemLevel3Response;
import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ImageResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InactiveSummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3AttributeWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.model.ItemImageWebResponse;
import com.gdn.partners.pcu.external.web.model.enums.ProductSyncWebStatus;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AllowedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.B2BResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CompanyWebResponse;
import com.gdn.partners.pcu.external.web.model.response.DescriptiveAttributeValue;
import com.gdn.partners.pcu.external.web.model.response.DistinctPickUpPoint;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.ForceReviewImageViolationWebResponse;
import com.gdn.partners.pcu.external.web.model.response.GeoLocationWebResponse;
import com.gdn.partners.pcu.external.web.model.response.GeolocationDTOResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.IgnoreAttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ImageWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemAttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.LogAuditTrailUpdatedProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PinPoint;
import com.gdn.partners.pcu.external.web.model.response.PreOrderResponse;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductAttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemLevel3LogisticsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemWholesalePriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CommonImageWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3AttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3AttributeWipWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ImageWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ItemWipWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3PriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsImageWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ViewConfigWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleDtoWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSettingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoMerchantWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.RuleConfigWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SellerLogisticsProductResponse;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesImageWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.VariantsErrorListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoV2Response;
import com.gdn.partners.product.pricing.web.model.response.PromoAdjustmentResponse;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuDetailResponse;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.enums.PriceUpdateCriteria;
import com.gdn.x.campaign.dto.ProductCampaignAvailabilityInfoDto;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummary;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.rest.web.model.ItemAndPickupPointBasicDetailResponse;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleItemResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.IgnoreAttributeSet;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.RuleConfigResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

/**
 * @author Pradeep Reddy
 */
public class ResponseHelper {

  public static final String DATE_PATTERN = "MM/dd/yyyy HH:mm:ss";
  private static final String IS = "is--";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUP_POINT_QUERY = "?pickupPointCode=%s";
  private static final String BRAND_NOT_FOUND = "DATA_NOT_FOUND";
  private static final String BRAND_NOT_FOUND_ERROR_MSG = "Brand yang Anda pilih ditolak. Pilih brand lainnya untuk melanjutkan.";
  public static final String SUCCESS = "SUCCESS";

  public static boolean validateResponse(GdnRestListResponse clientResponse) {

    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode())).anyMatch(
        code -> code.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getContent())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode()))
        .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateBrandWipResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (BRAND_NOT_FOUND.equals(clientResponse.getErrorCode())) {
      throw new ApiIncorrectInputDataException(BRAND_NOT_FOUND_ERROR_MSG, clientResponse.getErrorCode());
    }
    return true;
  }

  public static boolean validateResponse(BaseResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateUpdateSummaryResponse(GdnRestSingleResponse clientResponse, String gdnSku){
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.valueOf(clientResponse.getErrorCode()),
          clientResponse.getErrorMessage());
    }
    if (Objects.isNull(clientResponse.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "Product data is not found for: " + gdnSku);
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
    clientResponse.setErrorMessage(
        Optional.ofNullable(clientResponse.getErrorMessage()).orElse(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode()))
        .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
    }
    if (EnumSet.allOf(com.gdn.partners.pcu.external.web.model.response.ApiErrorCode.class).stream()
        .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
    }
    if (!clientResponse.isSuccess()) {
      if (ErrorCategory.VALIDATION.name().equals(clientResponse.getErrorCode())) {
        if (clientResponse.getErrorMessage().contains(ErrorCategory.VALIDATION.getMessage())) {
          throw new ValidationException(clientResponse.getErrorMessage());
        }
        throw new ValidationException(ErrorCategory.VALIDATION.getMessage() + clientResponse.getErrorMessage());
      }
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateProductCreationResponse(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode()))
      .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
      throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(),
        clientResponse.getErrorCode());
    }
    if (!clientResponse.isSuccess()) {
      if (ErrorCategory.VALIDATION.name().equals(clientResponse.getErrorCode())) {
        throw new ValidationException(
          ErrorCategory.VALIDATION.getMessage() + clientResponse.getErrorMessage());
      }
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponseForSyncUnsyncProduct(GdnBaseRestResponse response) {
    if (Objects.isNull(response)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!response.isSuccess()) {
      if (response.getErrorMessage().contains(ErrorMessages.UPDATING_ARCHIVED_ITEM)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.UPDATING_ARCHIVED_ITEM);
      } else if (response.getErrorMessage().contains(ErrorMessages.UPDATING_REJECTED_ITEM)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.UPDATING_REJECTED_ITEM);
      } else if (response.getErrorMessage().contains(ErrorMessages.UPDATING_SUSPENDED_ITEM)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.UPDATING_SUSPENDED_ITEM);
      } else {
        throw new ClientException(response.getErrorMessage());
      }
    }
    return true;
  }

  public static boolean validateBrandDetailResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    if (Objects.isNull(clientResponse.getValue())) {
      throw new InvalidStateException(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE);
    }
    return true;
  }

  public static boolean validateAgpQueryResponse(AgpSimpleQueryResponse response) {
    if (Objects.isNull(response) || Objects.isNull(response.getHits())) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    return true;
  }

  public static EstimateItemPriceWebResponse toEstimateItemPriceWebResponse(
      EstimateItemPriceResponse estimateItemPriceResponse) {
    EstimateItemPriceWebResponse estimateItemPriceWebResponse = new EstimateItemPriceWebResponse();
    BeanUtils.copyProperties(estimateItemPriceResponse, estimateItemPriceWebResponse);
    return estimateItemPriceWebResponse;
  }

  public static ProductDetailWebResponse toProductDetailWebResponse(ProductDetailResponse productDetailResponse) {
    Set<ProductItemWebResponse> productItemResponses = new HashSet<>();
    List<ProductCategoryWebResponse> productCategoryResponses = new ArrayList<>();
    List<ProductAttributeWebResponse> productAttributeResponses = new ArrayList<>();
    List<ImageWebResponse> productImageResponses = new ArrayList<>();
    ProductDetailWebResponse productDetailWebResponse = new ProductDetailWebResponse();
    BeanUtils.copyProperties(productDetailResponse, productDetailWebResponse, "productItemResponses",
        "productAttributeResponses", "productCategoryResponses", "categories", "images");
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses())) {
      productItemResponses =
          productDetailResponse.getProductItemResponses().stream().map(ResponseHelper::toProductItemResponse)
              .collect(Collectors.toSet());
    }
    productDetailWebResponse.setProductItemResponses(productItemResponses);
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      productAttributeResponses = productDetailResponse.getProductAttributeResponses().stream()
          .map(ResponseHelper::toProductAttributeWebResponse).collect(Collectors.toList());
    }
    productDetailWebResponse.setProductAttributeResponses(productAttributeResponses);
    if (CollectionUtils.isNotEmpty(productDetailResponse.getImages())) {
      productImageResponses = productDetailResponse.getImages().stream().map(ResponseHelper::toImageWebResponse)
          .collect(Collectors.toList());
    }
    productDetailWebResponse.setImages(productImageResponses);
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses())) {
      productCategoryResponses = productDetailResponse.getProductCategoryResponses().stream()
          .map(ResponseHelper::toProductCategoryWebResponse)
          .collect(Collectors.toList());
    }
    productDetailWebResponse.setProductCategoryResponses(productCategoryResponses);
    if (CollectionUtils.isNotEmpty(productDetailResponse.getCategories())) {
      productDetailWebResponse.setCategories(productDetailResponse.getCategories());
    }
    return productDetailWebResponse;
  }

  public static ProductLevel3WebResponse toProductLevel3WebResponse(ProductLevel3Response productLevel3Response) {
    List<ProductItemLevel3WebResponse> productItemLevel3WebResponses = new ArrayList<>();
    List<ProductLevel3AttributeWebResponse> productLevel3AttributeWebResponses = new ArrayList<>();
    List<ProductLevel3ImageWebResponse> productLevel3ImageWebResponses = new ArrayList<>();
    ProductLevel3WebResponse productLevel3WebResponse = new ProductLevel3WebResponse();
    BeanUtils.copyProperties(productLevel3Response, productLevel3WebResponse, "items", "attributes", "images");
    if (CollectionUtils.isNotEmpty(productLevel3Response.getItems())) {
      productItemLevel3WebResponses =
          productLevel3Response.getItems().stream().map(ResponseHelper::toProductItemLevel3WebResponse)
              .collect(Collectors.toList());
    }
    productLevel3WebResponse.setItems(productItemLevel3WebResponses);
    if (CollectionUtils.isNotEmpty(productLevel3Response.getAttributes())) {
      productLevel3AttributeWebResponses =
          productLevel3Response.getAttributes().stream().map(ResponseHelper::toProductLevel3AttributeWebResponse)
              .collect(Collectors.toList());
    }
    productLevel3WebResponse.setAttributes(productLevel3AttributeWebResponses);
    if (CollectionUtils.isNotEmpty(productLevel3Response.getImages())) {
      productLevel3ImageWebResponses =
          productLevel3Response.getImages().stream().map(ResponseHelper::toProductLevel3ImageWebResponse)
              .collect(Collectors.toList());
    }
    if (Objects.nonNull(productLevel3Response.getProductScore())) {
      com.gda.mta.product.dto.ProductScoreResponse response = productLevel3Response.getProductScore();
      ProductScoreResponse productScoreResponse =
          ProductScoreResponse.builder().MANDATORY_INFO_RULE(response.getMandatoryAttributeScore())
              .PRODUCT_TITLE_RULE(response.getProductTitleScore()).DESCRIPTION_RULE(response.getDescriptionScore())
              .EAN_UPC_RULE(response.getEanUpcScore()).IMAGE_RULE(response.getImageScore())
              .RECOMMENDED_ATTRIBUTE_RULE(response.getRecommendedAttributeScore())
              .REMAINING_ATTRIBUTE_RULE(response.getRemainingAttributeScore())
              .VARIANT_CREATING_RULE(response.getVariantCreatingScore()).VIDEO_URL_RULE(response.getVideoUrlScore())
              .USP_RULE(response.getUspScore()).TOTAL_SCORE(response.getTotalScore()).build();
      productLevel3WebResponse.setProductScore(productScoreResponse);
    }
    productLevel3WebResponse.setImages(productLevel3ImageWebResponses);
    productLevel3WebResponse.setEnableEdit(!productLevel3Response.isForceReview());
    return productLevel3WebResponse;
  }

  public static ProductItemLevel3WebResponse toProductItemLevel3WebResponse(
      ProductItemLevel3Response productItemLevel3Response) {
    List<ProductLevel3ViewConfigWebResponse> productLevel3ViewConfigWebResponses = new ArrayList<>();
    List<ProductLevel3PriceWebResponse> productLevel3PriceWebResponse = new ArrayList<>();
    List<ProductLevel3ImageWebResponse> imageWebResponses = new ArrayList<>();
    List<ProductItemWholesalePriceWebResponse> wholesalePriceWebResponses = new ArrayList<>();
    List<ProductItemLevel3LogisticsWebResponse> productItemLevel3LogisticsWebResponses =
        new ArrayList<>();
    ProductItemLevel3WebResponse productItemLevel3WebResponse = new ProductItemLevel3WebResponse();
    BeanUtils.copyProperties(productItemLevel3Response, productItemLevel3WebResponse, "prices", "viewConfigs", "images",
        "wholesalePriceActivated");
    productItemLevel3WebResponse.setRejected(productItemLevel3Response.isMarkForDelete());
    if (CollectionUtils.isNotEmpty(productItemLevel3Response.getPrices())) {
      productLevel3PriceWebResponse =
          productItemLevel3Response.getPrices().stream().map(ResponseHelper::toProductLevel3PriceWebResponse)
              .collect(Collectors.toList());
    }
    productItemLevel3WebResponse.setPrices(productLevel3PriceWebResponse);
    if (CollectionUtils.isNotEmpty(productItemLevel3Response.getViewConfigs())) {
      productLevel3ViewConfigWebResponses =
          productItemLevel3Response.getViewConfigs().stream().map(ResponseHelper::toProductLevel3ViewConfigWebResponse)
              .collect(Collectors.toList());
    }
    productItemLevel3WebResponse.setViewConfigs(productLevel3ViewConfigWebResponses);
    if (CollectionUtils.isNotEmpty(productItemLevel3Response.getImages())) {
      imageWebResponses =
          productItemLevel3Response.getImages().stream().map(ResponseHelper::toProductLevel3ImageWebResponse)
              .collect(Collectors.toList());
    }
    productItemLevel3WebResponse.setImages(imageWebResponses);

    if (CollectionUtils.isNotEmpty(productItemLevel3Response.getProductItemWholesalePriceResponses())) {
      wholesalePriceWebResponses = productItemLevel3Response.getProductItemWholesalePriceResponses().stream().map(
          productItemWholesalePriceResponse -> new ProductItemWholesalePriceWebResponse(
              productItemWholesalePriceResponse.getQuantity(),
              productItemWholesalePriceResponse.getWholesaleDiscount())).collect(Collectors.toList());
    }
    productItemLevel3WebResponse.setProductItemWholesalePriceResponses(wholesalePriceWebResponses);

    if (CollectionUtils
        .isNotEmpty(productItemLevel3Response.getProductItemLevel3LogisticResponse())) {
      productItemLevel3LogisticsWebResponses =
          productItemLevel3Response.getProductItemLevel3LogisticResponse().stream()
              .map(productItemLevel3LogisticResponse -> new ProductItemLevel3LogisticsWebResponse(
                  productItemLevel3LogisticResponse.getLogisticProductCode(),
                  productItemLevel3LogisticResponse.getLogisticProductName(),
                  productItemLevel3LogisticResponse.isSelected(),
                  productItemLevel3LogisticResponse.isRequiredLongLat(),
                  productItemLevel3LogisticResponse.getHighlightedInformation()))
              .collect(Collectors.toList());
    }
    productItemLevel3WebResponse
        .setProductItemLevel3LogisticsWebResponses(productItemLevel3LogisticsWebResponses);

    productItemLevel3WebResponse.setWholesalePriceActivated(productItemLevel3Response.getWholesalePriceActivated());
    return productItemLevel3WebResponse;
  }

  public static ProductLevel3AttributeWebResponse toProductLevel3AttributeWebResponse(
      ProductLevel3AttributeResponse productLevel3AttributeResponse) {
    ProductLevel3AttributeWebResponse productLevel3AttributeWebResponse = new ProductLevel3AttributeWebResponse();
    BeanUtils.copyProperties(productLevel3AttributeResponse, productLevel3AttributeWebResponse);
    return productLevel3AttributeWebResponse;
  }

  public static ProductLevel3AttributeWebResponse toProductLevel3AttributeWebResponse(
      com.gdn.partners.pcu.external.web.model.response.ProductLevel3AttributeResponse productLevel3AttributeResponse) {
    ProductLevel3AttributeWebResponse productLevel3AttributeWebResponse = new ProductLevel3AttributeWebResponse();
    BeanUtils.copyProperties(productLevel3AttributeResponse, productLevel3AttributeWebResponse);
    return productLevel3AttributeWebResponse;
  }


  public static ProductLevel3ViewConfigWebResponse toProductLevel3ViewConfigWebResponse(
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse) {
    ProductLevel3ViewConfigWebResponse productLevel3ViewConfigWebResponse = new ProductLevel3ViewConfigWebResponse();
    BeanUtils.copyProperties(productLevel3ViewConfigResponse, productLevel3ViewConfigWebResponse);
    return productLevel3ViewConfigWebResponse;
  }

  public static ProductLevel3PriceWebResponse toProductLevel3PriceWebResponse(
      ProductLevel3PriceResponse productLevel3PriceResponse) {
    ProductLevel3PriceWebResponse productLevel3PriceWebResponse = new ProductLevel3PriceWebResponse();
    BeanUtils.copyProperties(productLevel3PriceResponse, productLevel3PriceWebResponse, "discountPercentage");
    productLevel3PriceWebResponse.setDiscountPercentage(Math.ceil(
        100 * ((productLevel3PriceResponse.getPrice() - productLevel3PriceResponse.getSalePrice())
            / productLevel3PriceResponse.getPrice())));
    return productLevel3PriceWebResponse;
  }

  public static ProductLevel3ImageWebResponse toProductLevel3ImageWebResponse(
      ProductLevel3ImageResponse productLevel3ImageResponse) {
    ProductLevel3ImageWebResponse productLevel3ImageWebResponse = new ProductLevel3ImageWebResponse();
    BeanUtils.copyProperties(productLevel3ImageResponse, productLevel3ImageWebResponse);
    return productLevel3ImageWebResponse;
  }

  public static ProductL3CommonImageWebResponse toProductL3CommonImageWebResponse(
    ProductL3CommonImageResponse productLevel3ImageResponse) {
    ProductL3CommonImageWebResponse productL3CommonImageWebResponse = new ProductL3CommonImageWebResponse();
    BeanUtils.copyProperties(productLevel3ImageResponse, productL3CommonImageWebResponse, "mainImage");
    productL3CommonImageWebResponse.setMainImages(productLevel3ImageResponse.getMainImage());
    return productL3CommonImageWebResponse;
  }

  public static List<ActiveProductWebResponse> toActiveProductWebResponseList(
      List<ActiveProductResponse> responseList ) {
    return responseList.stream().map(ResponseHelper::toActiveProductWebResponse)
        .collect(Collectors.toList());
  }

  public static ActiveProductWebResponse toActiveProductWebResponse(ActiveProductResponse activeProductResponse) {
    List<ItemDetailWebResponse> itemDetailWebResponse = new ArrayList<>();
    ActiveProductWebResponse activeProductWebResponse = new ActiveProductWebResponse();
    BeanUtils.copyProperties(activeProductResponse, activeProductWebResponse, "itemDetailWebResponse");
    if (CollectionUtils.isNotEmpty(activeProductResponse.getItemDetailResponses())) {
      itemDetailWebResponse =
          activeProductResponse.getItemDetailResponses().stream().map(ResponseHelper::toItemDetailWebResponse)
              .collect(Collectors.toList());
    }
    activeProductWebResponse.setItemDetailWebResponse(itemDetailWebResponse);
    activeProductWebResponse.setMerchantCode(activeProductResponse.getMerchantCode());
    activeProductWebResponse.setProductName(activeProductResponse.getProductName());
    activeProductWebResponse.setProductCode(activeProductResponse.getProductCode());
    activeProductWebResponse.setProductSku(activeProductResponse.getProductSku());
    activeProductWebResponse.setImageUrl(activeProductResponse.getImageUrl());
    activeProductWebResponse.setVariantCount(activeProductResponse.getItemCount());
    return activeProductWebResponse;
  }

  public static ProductL3ListingWebResponse toActiveProductResponse(
      ActiveProductResponse activeProductResponse,
      Map<String, CategoryWebResponse> categoryWebResponseMap, String productDetailPageUrlPrefix) {
    return ProductL3ListingWebResponse.builder().productName(activeProductResponse.getProductName())
        .productSku(activeProductResponse.getProductSku())
        .variantCount(activeProductResponse.getItemCount())
        .l5Count(activeProductResponse.getL5Count())
        .bundleProduct(Boolean.TRUE.equals(activeProductResponse.getBundleProduct()))
        .imageUrl(activeProductResponse.getImageUrl()).categoryName(
            categoryWebResponseMap.getOrDefault(activeProductResponse.getMasterCatalog(),
                    CategoryWebResponse.builder().name(StringUtils.EMPTY).build()).getName())
        .pdpUrl(RequestHelper.toProductDetailPage(activeProductResponse.getProductSku(), productDetailPageUrlPrefix))
        .item(toItemAndPickupPointBasicDetailWebResponse(activeProductResponse.getItemAndPickupPointBasicDetailResponse()))
        .build();
  }

  private static ItemAndPickupPointBasicDetailWebResponse toItemAndPickupPointBasicDetailWebResponse(
      ItemAndPickupPointBasicDetailResponse itemAndPickupPointBasicDetailResponse) {
    if (Objects.nonNull(itemAndPickupPointBasicDetailResponse)) {
      ItemAndPickupPointBasicDetailWebResponse itemAndPickupPointBasicDetailWebResponse =
          new ItemAndPickupPointBasicDetailWebResponse();
      BeanUtils.copyProperties(itemAndPickupPointBasicDetailResponse, itemAndPickupPointBasicDetailWebResponse);
      return itemAndPickupPointBasicDetailWebResponse;
    } else {
      return null;
    }
  }

  public static List<SuspensionWebResponse> toSuspensionWebResponseList(
      List<SuspensionItemResponse> responseList ) {
    return responseList.stream().map(ResponseHelper::toSuspensionWebResponse)
        .collect(Collectors.toList());
  }

  public static SuspensionWebResponse toSuspensionWebResponse(SuspensionItemResponse suspensionItemResponse) {
    SuspensionWebResponse suspensionWebResponse = new SuspensionWebResponse();
    BeanUtils.copyProperties(suspensionItemResponse, suspensionWebResponse);
    return suspensionWebResponse;
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

  private static ProductAttributeWebResponse toProductAttributeWebResponse(
      ProductAttributeResponse productAttributeResponse) {
    ProductAttributeWebResponse productAttributeWebResponse = new ProductAttributeWebResponse();
    List<ProductAttributeValueWebResponse> productAttributeValueWebResponses = new ArrayList<>();
    AttributeWebResponse attributeWebResponse = new AttributeWebResponse();
    BeanUtils
        .copyProperties(productAttributeResponse, productAttributeWebResponse, "productAttributeValues", "attribute");
    if (Objects.nonNull(productAttributeResponse.getAttribute())) {
      BeanUtils.copyProperties(productAttributeResponse.getAttribute(), attributeWebResponse);
      productAttributeWebResponse.setAttribute(attributeWebResponse);
    }
    if (CollectionUtils.isNotEmpty(productAttributeResponse.getProductAttributeValues())) {
      productAttributeValueWebResponses = productAttributeResponse.getProductAttributeValues().stream().
          map(ResponseHelper::toProductAttributeValueWebResponse).collect(Collectors.toList());
    }
    productAttributeWebResponse.setProductAttributeValues(productAttributeValueWebResponses);
    return productAttributeWebResponse;
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
    if (Objects.nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
      BeanUtils.copyProperties(productAttributeValueResponse.getPredefinedAllowedAttributeValue(),
          predefinedAttributeValueWebResponse);
      productAttributeValueWebResponse.setPredefinedAllowedAttributeValue(predefinedAttributeValueWebResponse);
    }
    productAttributeValueWebResponse.setDescriptiveAttributeValueType(
        DescriptiveAttributeValue.valueOf(productAttributeValueResponse.getDescriptiveAttributeValueType().name()));
    return productAttributeValueWebResponse;
  }

  private static ProductItemWebResponse toProductItemResponse(ProductItemResponse productItemResponse) {
    ProductItemWebResponse productItemWebResponse = new ProductItemWebResponse();
    BeanUtils
        .copyProperties(productItemResponse, productItemWebResponse, "productItemAttributeValues", "productItemImages");
    List<ImageWebResponse> imageWebResponses = new ArrayList<>();
    List<ProductItemAttributeValueWebResponse> attributeValueWebResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemResponse.getImages())) {
      imageWebResponses =
          productItemResponse.getImages().stream().map(ResponseHelper::toImageWebResponse).collect(Collectors.toList());
    }
    if (CollectionUtils.isNotEmpty(productItemResponse.getProductItemAttributeValueResponses())) {
      attributeValueWebResponses = productItemResponse.getProductItemAttributeValueResponses().stream()
          .map(ResponseHelper::toProductItemAttributeValueWebResponse).collect(Collectors.toList());
    }
    productItemWebResponse.setImages(imageWebResponses);
    productItemWebResponse.setProductItemAttributeValueResponses(attributeValueWebResponses);
    return productItemWebResponse;
  }

  private static ItemDetailWebResponse toItemDetailWebResponse(ItemDetailResponse itemDetailResponse) {
    ItemDetailWebResponse itemDetailWebResponse = new ItemDetailWebResponse();
    BeanUtils
        .copyProperties(itemDetailResponse, itemDetailWebResponse);
    if (Objects.nonNull(itemDetailResponse)) {
      itemDetailWebResponse.setItemName(itemDetailResponse.getItemName());
      itemDetailWebResponse.setItemSku(itemDetailResponse.getItemSku());
    }
    return itemDetailWebResponse;
  }

  private static ImageWebResponse toImageWebResponse(Image image) {
    ImageWebResponse imageWebResponse = new ImageWebResponse();
    BeanUtils.copyProperties(image, imageWebResponse);
    return imageWebResponse;
  }

  private static ProductItemAttributeValueWebResponse toProductItemAttributeValueWebResponse(
      ProductItemAttributeValueResponse response) {
    ProductItemAttributeValueWebResponse attributeValueWebResponse = new ProductItemAttributeValueWebResponse();
    AttributeWebResponse attributeWebResponse = new AttributeWebResponse();
    if (Objects.nonNull(response.getAttributeResponse())) {
      BeanUtils.copyProperties(response.getAttributeResponse(), attributeWebResponse);
      attributeValueWebResponse.setAttribute(attributeWebResponse);
    }
    attributeValueWebResponse.setValue(response.getValue());
    return attributeValueWebResponse;
  }

  public static List<ProductItemDetailWebResponse> toProductItemDetailWebResponseList(List<ProductItemDetailResponse> responses) {
    return responses.stream().map(ResponseHelper::toProductItemDetailWebResponse).collect(Collectors.toList());
  }

  private static ProductItemDetailWebResponse toProductItemDetailWebResponse(
      ProductItemDetailResponse productItemDetailResponse) {
    ProductItemDetailWebResponse productItemDetailWebResponse = new ProductItemDetailWebResponse();
    ProductItemWebResponse productItemWebResponse = new ProductItemWebResponse();
    BeanUtils.copyProperties(productItemDetailResponse, productItemWebResponse);
    if (CollectionUtils.isNotEmpty(productItemDetailResponse.getImages())) {
      productItemWebResponse.setImages(
          productItemDetailResponse.getImages().stream().map(ResponseHelper::toImageWebResponse)
              .collect(Collectors.toList()));
    }
    if (CollectionUtils.isNotEmpty(productItemDetailResponse.getProductItemAttributeValueResponses())) {
      productItemWebResponse.setProductItemAttributeValueResponses(
          productItemDetailResponse.getProductItemAttributeValueResponses().stream()
              .map(ResponseHelper::toProductItemAttributeValueWebResponse).collect(Collectors.toList()));
    }
    productItemDetailWebResponse.setProductItemWebResponse(productItemWebResponse);
    if (Objects.nonNull(productItemDetailResponse.getProductResponse())) {
      productItemDetailWebResponse.setProductCode(productItemDetailResponse.getProductResponse().getProductCode());
      productItemDetailWebResponse.setProductId(productItemDetailResponse.getProductResponse().getId());
    }
    return productItemDetailWebResponse;
  }

  public static CategorySuggestionWebResponse toCategorySuggestionWebResponse(
      CategoryResponse categoryResponse, long productCount) {
    return CategorySuggestionWebResponse.categorySuggestionWebResponseBuilder()
        .id(categoryResponse.getId()).categoryCode(categoryResponse.getCategoryCode())
        .name(categoryResponse.getName()).nameEnglish(categoryResponse.getNameEnglish()).display(categoryResponse.isDisplay())
        .activated(categoryResponse.isActivated()).viewable(categoryResponse.isViewable())
        .productCount(productCount).parentCategoryId(categoryResponse.getParentCategoryId()).build();
  }

  public static SimpleCategoryWebResponse toSimpleCategoryWebResponse(CategoryResponse categoryResponse) {
    return SimpleCategoryWebResponse.builder().id(categoryResponse.getId())
        .categoryCode(categoryResponse.getCategoryCode()).name(categoryResponse.getName())
        .nameEnglish(categoryResponse.getNameEnglish()).display(categoryResponse.isDisplay())
        .activated(categoryResponse.isActivated()).viewable(categoryResponse.isViewable())
        .parentCategoryId(categoryResponse.getParentCategoryId()).build();
  }

  public static List<ProductItemWebResponse> toProductItemWebResponseList(
      List<ProductItemResponse> productItemResponses) {
    return productItemResponses.stream().map(ResponseHelper::toProductItemResponse).collect(Collectors.toList());
  }

  public static BusinessPartnerProfileWebResponse toBusinessPartnerProfileWebResponse(
          ProfileResponse profileResponse, boolean bpBopisRestrictionEnabled, String businessPartnerCode, Set<String> distributionSellerList, boolean ranchIntegrationEnabled) {
    BusinessPartnerProfileWebResponse businessPartnerProfileWebResponse = new BusinessPartnerProfileWebResponse();
    BeanUtils.copyProperties(profileResponse, businessPartnerProfileWebResponse, "pickupPoints",
      "bopisFlag","bigProductFlag");
    List<PickupPointWebResponse> pickupPointWebResponses =
        Optional.ofNullable(profileResponse.getPickupPoints()).orElseGet(Collections::emptyList).stream()
            .map(ResponseHelper::toPickupPointWebResponse).collect(Collectors.toList());
    businessPartnerProfileWebResponse.setPickupPoints(pickupPointWebResponses);
    businessPartnerProfileWebResponse.setId(profileResponse.getId());
    businessPartnerProfileWebResponse.setBusinessPartnerName(Objects.nonNull(profileResponse.getCompany()) ?
        profileResponse.getCompany().getBusinessPartnerName() :
        StringUtils.EMPTY);
    businessPartnerProfileWebResponse.setCncActivated(
        Objects.nonNull(profileResponse.getCompany()) ? profileResponse.getCompany().isCncActivated() : false);
    if (Objects.nonNull(profileResponse.getCompany())) {
      CompanyWebResponse company = new CompanyWebResponse();
      BeanUtils.copyProperties(profileResponse.getCompany(), company);
      businessPartnerProfileWebResponse.setCompany(company);
    }
    ProductSettingWebResponse productSettingWebResponse = new ProductSettingWebResponse();
    if (Objects.nonNull(profileResponse.getProductSettings())) {
      BeanUtils.copyProperties(profileResponse.getProductSettings(), productSettingWebResponse);
      businessPartnerProfileWebResponse.setProductSettings(productSettingWebResponse);
    }
    if (bpBopisRestrictionEnabled) {
      businessPartnerProfileWebResponse.setBopisFlag(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBopisFlag(), true));
      businessPartnerProfileWebResponse.setBigProductFlag(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBigProductFlag(), true));
    }
    if (MapUtils.isNotEmpty(profileResponse.getFlags())) {
      businessPartnerProfileWebResponse.setProductLimitEnabled(
          Boolean.TRUE.equals(profileResponse.getFlags().getOrDefault(Constants.PRODUCT_LIMIT, false)));
      businessPartnerProfileWebResponse.setFaasActivated(
          Boolean.TRUE.equals(profileResponse.getFlags().getOrDefault(Constants.FAAS_ACTIVATED, false)));
      businessPartnerProfileWebResponse.setProductConsequenceLimitation(Boolean.TRUE.equals(
        profileResponse.getFlags().getOrDefault(Constants.PRODUCT_CONSEQUENCE_LIMITATION, false)));
    }
    boolean distributionSeller = ranchIntegrationEnabled && distributionSellerList.contains(businessPartnerCode);
    businessPartnerProfileWebResponse.setDistributionSeller(distributionSeller);
    return businessPartnerProfileWebResponse;
  }

  private static PickupPointWebResponse toPickupPointWebResponse(PickupPointDTO pickupPointDTO) {
    PickupPointWebResponse pickupPointWebResponse = new PickupPointWebResponse();
    BeanUtils.copyProperties(pickupPointDTO, pickupPointWebResponse);
    GeolocationDTOResponse geolocationDTOResponse = null;
    if (Objects.nonNull(pickupPointDTO.getGeolocation())) {
      geolocationDTOResponse = new GeolocationDTOResponse();
      geolocationDTOResponse.setLatitude(pickupPointDTO.getGeolocation().getLatitude());
      geolocationDTOResponse.setLongitude(pickupPointDTO.getGeolocation().getLongitude());
      geolocationDTOResponse.setPlaceId(pickupPointDTO.getGeolocation().getPlaceId());
      geolocationDTOResponse.setStreetAddress(pickupPointDTO.getGeolocation().getStreetAddress());
    }
    pickupPointWebResponse.setGeolocation(geolocationDTOResponse);
    return pickupPointWebResponse;
  }

  public static List<PredefinedAttributeValueWebResponse> toPredefinedAttributeValueWebResponseList(
      List<PredefinedAllowedAttributeValueResponse> responseList ) {
    return responseList.stream().map(ResponseHelper::toPredefinedAttributeValueWebResponse)
        .collect(Collectors.toList());
  }

  private static PredefinedAttributeValueWebResponse toPredefinedAttributeValueWebResponse(PredefinedAllowedAttributeValueResponse response) {
    PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse = new PredefinedAttributeValueWebResponse();
    BeanUtils.copyProperties(response, predefinedAttributeValueWebResponse);
    return predefinedAttributeValueWebResponse;
  }

  public static List<PredefinedAllowedAttributeValueResponse> toPredefinedAllowedAttributeValueResponse(
      List<PredefinedAttributeValueWebResponse> responseList) {
    return responseList.stream().map(ResponseHelper::toPredefinedAllowedAttributeValueResponse)
        .collect(Collectors.toList());
  }

  private static PredefinedAllowedAttributeValueResponse toPredefinedAllowedAttributeValueResponse(
      PredefinedAttributeValueWebResponse response) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    BeanUtils.copyProperties(response, predefinedAllowedAttributeValueResponse);
    return predefinedAllowedAttributeValueResponse;
  }

  public static ProductLevel3WipDetailWebResponse toProductLevel3WipDetailWebResponse(
      ProductLevel3WipDetailResponse productLevel3WipDetailResponse) {
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse = ProductLevel3WipDetailWebResponse.builder()
        .businessPartnerCode(productLevel3WipDetailResponse.getBusinessPartnerCode())
        .businessPartnerName(productLevel3WipDetailResponse.getBusinessPartnerName())
        .productLevel1Id(productLevel3WipDetailResponse.getProductLevel1Id())
        .productSku(productLevel3WipDetailResponse.getProductSku())
        .productCode(productLevel3WipDetailResponse.getProductCode())
        .productName(productLevel3WipDetailResponse.getProductName())
        .categoryCode(productLevel3WipDetailResponse.getCategoryCode())
        .categoryName(productLevel3WipDetailResponse.getCategoryName())
        .brandName(productLevel3WipDetailResponse.getBrandName()).active(productLevel3WipDetailResponse.isActive())
        .build();
    List<ProductLevel3ItemWipWebResponse> productLevel3ItemWipWebResponses =
        productLevel3WipDetailResponse.getItems().stream().map(ResponseHelper::toproductLevel3ItemWipWebResponse)
            .collect(Collectors.toList());
    productLevel3WipDetailWebResponse.setItems(productLevel3ItemWipWebResponses);
    List<ProductLevel3AttributeWipWebResponse> productLevel3AttributeWipWebResponses =
        productLevel3WipDetailResponse.getAttributes().stream()
            .map(ResponseHelper::toProductLevel3AttributeWipWebResponses).collect(Collectors.toList());
    productLevel3WipDetailWebResponse.setAttributes(productLevel3AttributeWipWebResponses);
    productLevel3WipDetailWebResponse.setProductLevel3Logistics(
        ResponseHelper.toProductLevel3WebLogistics(productLevel3WipDetailResponse));
    return productLevel3WipDetailWebResponse;
  }

  private static List<ProductItemLevel3LogisticsWebResponse> toProductLevel3WebLogistics(
      ProductLevel3WipDetailResponse productLevel3WipDetailResponse) {
    List<ProductItemLevel3LogisticsWebResponse> productLevel3WebLogisticList = new ArrayList<>();
    for (ProductLevel3Logistics productLevel3Logistics : productLevel3WipDetailResponse
        .getProductLevel3Logistics()) {
      ProductItemLevel3LogisticsWebResponse productItemLevel3LogisticsWebResponse =
          new ProductItemLevel3LogisticsWebResponse();
      BeanUtils.copyProperties(productLevel3Logistics, productItemLevel3LogisticsWebResponse);
      productLevel3WebLogisticList.add(productItemLevel3LogisticsWebResponse);
    }
    return productLevel3WebLogisticList;
  }

  public static ProductCountWebResponse toProductCountWebResponse(
      ProductLevel3SummaryCountResponse productLevel3SummaryCountResponse) {
    ProductCountWebResponse productCountWebResponse = new ProductCountWebResponse();
    Map<ProductLevel3InventoryCriteria, Long> stockCounts = productLevel3SummaryCountResponse.getStockConditionCounts();
    productCountWebResponse.setAvailable(stockCounts.get(ProductLevel3InventoryCriteria.AVAILABLE));
    productCountWebResponse.setMinimumStock(stockCounts.get(ProductLevel3InventoryCriteria.STOCK_ALERT));
    productCountWebResponse.setOutOfStock(stockCounts.get(ProductLevel3InventoryCriteria.OOS));
    productCountWebResponse
        .setTotalCounts(productCountWebResponse.getAvailable() + productCountWebResponse.getOutOfStock());
    productCountWebResponse.setBusinessPartnerCode(productLevel3SummaryCountResponse.getBusinessPartnerCode());
    return productCountWebResponse;
  }

  public static ProductCountWebResponse toProductCountWebResponse(
      CountProductLevel3WipResponse countProductLevel3WipResponse) {
    ProductCountWebResponse productCountWebResponse = new ProductCountWebResponse();
    Map<ProductLevel3WipSummaryCriteria, Long> productCounts = countProductLevel3WipResponse.getTotalItemsByCriterias();
    productCountWebResponse.setNeedCorrection(productCounts.get(ProductLevel3WipSummaryCriteria.NEED_CORRECTION));
    productCountWebResponse.setWaitingForApproval(productCounts.get(ProductLevel3WipSummaryCriteria.IN_PROGRESS));
    productCountWebResponse.setNeedAction(productCounts.get(ProductLevel3WipSummaryCriteria.FAILED));
    productCountWebResponse.setTotalCounts(
        productCountWebResponse.getNeedCorrection() + productCountWebResponse.getWaitingForApproval()
            + productCountWebResponse.getNeedAction());
    return productCountWebResponse;
  }

  public static ProductCountWebResponse toProductCountWebResponse(
        CountProductLevel3InactiveResponse countProductLevel3InactiveResponse) {
      ProductCountWebResponse productCountWebResponse = new ProductCountWebResponse();
      Map<ProductLevel3InactiveSummaryCriteria, Long> productCounts =
          countProductLevel3InactiveResponse.getTotalItemsByCriterias();
      productCountWebResponse.setRejected(productCounts.get(ProductLevel3InactiveSummaryCriteria.REJECTED));
      productCountWebResponse.setSuspended(productCounts.get(ProductLevel3InactiveSummaryCriteria.SUSPENDED));
      productCountWebResponse.setArchived(productCounts.get(ProductLevel3InactiveSummaryCriteria.ARCHIVED));
      productCountWebResponse.setTotalCounts(
          productCountWebResponse.getRejected() + productCountWebResponse.getSuspended() + productCountWebResponse
              .getArchived());
      return productCountWebResponse;
    }

  private static ProductLevel3ItemWipWebResponse toproductLevel3ItemWipWebResponse(
      ProductLevel3ItemWipResponse productLevel3ItemWipResponse) {
    ProductLevel3ItemWipWebResponse productLevel3ItemWipWebResponse = ProductLevel3ItemWipWebResponse.builder()
        .productLevel1ItemId(productLevel3ItemWipResponse.getProductLevel1ItemId())
        .gdnSku(productLevel3ItemWipResponse.getGdnSku()).merchantSku(productLevel3ItemWipResponse.getMerchantSku())
        .pickupPointCode(productLevel3ItemWipResponse.getPickupPointCode())
        .productType(productLevel3ItemWipResponse.getProductType())
        .price(productLevel3ItemWipResponse.getRegularPrice()).salePrice(productLevel3ItemWipResponse.getSellingPrice())
        .stock(productLevel3ItemWipResponse.getStock()).minimumStock(productLevel3ItemWipResponse.getMinimumStock())
        .display(productLevel3ItemWipResponse.isDisplayable()).buyable(productLevel3ItemWipResponse.isBuyable())
        .needInstallation(productLevel3ItemWipResponse.isNeedInstallation()).productItemWholesalePriceResponses(new ArrayList<>())
        .wholesalePriceActivated(productLevel3ItemWipResponse.getWholesalePriceActivated())
        .build();
    if (!CollectionUtils.isEmpty(productLevel3ItemWipResponse.getProductItemWholesalePriceResponses())) {
      productLevel3ItemWipWebResponse.setProductItemWholesalePriceResponses(productLevel3ItemWipResponse.getProductItemWholesalePriceResponses()
        .stream().map(ResponseHelper::toProductItemWholesalePriceWebResponse).collect(Collectors.toList()));
    }
    return productLevel3ItemWipWebResponse;
  }

  private static ProductItemWholesalePriceWebResponse toProductItemWholesalePriceWebResponse(
          ProductItemWholesalePriceResponse productItemWholesalePriceResponse) {
    ProductItemWholesalePriceWebResponse productItemWholesalePriceWebResponse =
            ProductItemWholesalePriceWebResponse.builder().quantity(productItemWholesalePriceResponse.getQuantity())
            .wholesaleDiscount(productItemWholesalePriceResponse.getWholesaleDiscount()).build();
    return productItemWholesalePriceWebResponse;
  }

  private static ProductLevel3AttributeWipWebResponse toProductLevel3AttributeWipWebResponses(
      ProductLevel3AttributeWipResponse productLevel3AttributeWipResponse) {
    ProductLevel3AttributeWipWebResponse productLevel3AttributeWipWebResponse =
        ProductLevel3AttributeWipWebResponse.builder().attributeId(productLevel3AttributeWipResponse.getAttributeId())
            .value(productLevel3AttributeWipResponse.getValue()).build();
    return productLevel3AttributeWipWebResponse;
  }

  public static List<ProductLevel3SummaryResponse> toProductLevel3SummaryResponseFromRejectedSkuProductResponse(
      List<RejectedSkuProductResponse> rejectedSkuProductResponses) {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    for(RejectedSkuProductResponse response : rejectedSkuProductResponses){
      ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
      BeanUtils.copyProperties(response, productLevel3SummaryResponse);
      productLevel3SummaryResponse.setUpdatedDate(response.getRejectedDate());
      productLevel3SummaryResponse.setCreatedDate(response.getSubmitDate());
      productLevel3SummaryResponse.setReason(response.getRejectedReason());
      productLevel3SummaryResponse.setCreatedBy(response.getInitiator());
      productLevel3SummaryResponse.setItemName(response.getProductName());
      productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    }
    return productLevel3SummaryResponses;
  }

  public static List<ItemDetailWebResponse> toItemDetailWebResponse(List<ItemSummaryResponse> itemSummaryResponses) {
    List<ItemDetailWebResponse> itemDetailWebResponses = new ArrayList<>();
    for (ItemSummaryResponse response : itemSummaryResponses) {
      ItemDetailWebResponse itemDetailWebResponse =
          ItemDetailWebResponse.builder().itemName(response.getGeneratedItemName()).itemSku(response.getItemSku())
              .build();
      itemDetailWebResponses.add(itemDetailWebResponse);
    }
    return itemDetailWebResponses;
  }

  public static List<ItemDetailWebResponse> toProductNameWebResponse(
      List<ProductNameSuggestionResponse> productNameSuggestionResponses) {
    List<ItemDetailWebResponse> itemDetailWebResponses = new ArrayList<>();
    for (ProductNameSuggestionResponse response : productNameSuggestionResponses) {
      ItemDetailWebResponse itemDetailWebResponse =
          ItemDetailWebResponse.builder().productName(response.getProductName()).productSku(response.getProductSku())
              .build();
      itemDetailWebResponses.add(itemDetailWebResponse);
    }
    return itemDetailWebResponses;
  }

  public static List<ProductLevel3SummaryResponse> toProductLevel3SummaryResponseFromSuspensionItemResponse(
      List<SuspensionItemResponse> suspensionItemResponses) {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    for(SuspensionItemResponse response : suspensionItemResponses){
      ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
      BeanUtils.copyProperties(response, productLevel3SummaryResponse);
      productLevel3SummaryResponse.setMerchantCode(response.getBusinessPartnerCode());
      ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
      productLevel3ImageResponse.setLocationPath(response.getImageUrl());
      productLevel3ImageResponse.setSequence(0);
      productLevel3ImageResponse.setMainImage(true);
      productLevel3SummaryResponse.setImages(Collections.singletonList(productLevel3ImageResponse));
      productLevel3SummaryResponse.setUpdatedDate(response.getBannedDate());
      productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    }
    return productLevel3SummaryResponses;
  }

  public static List<InProcessWebResponse> toInProcessWebResponse(
      List<ProductLevel3WipResponse> productLevel3WipResponseList) {
    List<InProcessWebResponse> inProcessWebResponseList = new ArrayList<>();
    for (ProductLevel3WipResponse productLevel3WipResponse : productLevel3WipResponseList) {
      InProcessWebResponse inProcessWebResponse = new InProcessWebResponse();
      BeanUtils.copyProperties(productLevel3WipResponse, inProcessWebResponse);
      inProcessWebResponse.setBrand(productLevel3WipResponse.getBrandName());
      inProcessWebResponse.setItemName(productLevel3WipResponse.getProductName());
      inProcessWebResponse.setReason(productLevel3WipResponse.getNotes());
      inProcessWebResponse.setForceReviewImageViolations(
          Optional.ofNullable(productLevel3WipResponse.getForceReviewImageViolations()).orElse(new ArrayList<>())
              .stream().map(forceReviewImageViolationResponse -> new ForceReviewImageViolationWebResponse(
              forceReviewImageViolationResponse.getEnName(), forceReviewImageViolationResponse.getInName()))
              .collect(toList()));
      inProcessWebResponseList.add(inProcessWebResponse);
    }
    return inProcessWebResponseList;
  }

  public static PromoItemDetailWebResponse toPromoItemDetailWebResponse(
      PromoSkuDetailResponse promoSkuDetailResponse) {
    PromoItemDetailWebResponse promoItemDetailWebResponse = new PromoItemDetailWebResponse();
    PromoSkuResponse activePromoSkuDetail = promoSkuDetailResponse.getActivePromoSkuDetail();
    if (Objects.nonNull(activePromoSkuDetail)) {
      PromoItemWebResponse promoItemWebResponse = new PromoItemWebResponse();
      BeanUtils.copyProperties(activePromoSkuDetail, promoItemWebResponse, "promoAdjustment");
      PromoAdjustmentResponse merchantResponse = activePromoSkuDetail.getPromoAdjustment();
      if (Objects.nonNull(merchantResponse)) {
        PromoMerchantWebResponse promoMerchantWebResponse = new PromoMerchantWebResponse();
        BeanUtils.copyProperties(merchantResponse, promoMerchantWebResponse);
        promoItemWebResponse.setPromoAdjustment(promoMerchantWebResponse);
      }
      promoItemDetailWebResponse.setActivePromoSkuDetail(promoItemWebResponse);
    }
    List<PromoSkuResponse> pendingPromoSkuDetailList = promoSkuDetailResponse.getPendingPromoSkuDetails();
    if (CollectionUtils.isNotEmpty(pendingPromoSkuDetailList)) {
      PromoItemWebResponse promoItemWebResponse = new PromoItemWebResponse();
      BeanUtils.copyProperties(pendingPromoSkuDetailList.get(0), promoItemWebResponse, "promoAdjustment");
      PromoAdjustmentResponse merchantResponse = pendingPromoSkuDetailList.get(0).getPromoAdjustment();
      if (Objects.nonNull(merchantResponse)) {
        PromoMerchantWebResponse promoMerchantWebResponse = new PromoMerchantWebResponse();
        BeanUtils.copyProperties(merchantResponse, promoMerchantWebResponse);
        promoItemWebResponse.setPromoAdjustment(promoMerchantWebResponse);
      }
      promoItemDetailWebResponse.setPendingPromoSkuDetail(promoItemWebResponse);
    }
    return promoItemDetailWebResponse;
  }

  public static List<CategoryWebResponse> toCategoryWebResponseList(List<CategoryDTO> categoryDTOList) {
    List<CategoryWebResponse> categoryWebResponses = new ArrayList<>();
    categoryDTOList.stream().forEach(categoryDTO -> categoryWebResponses.add(toCategoryWebResponse(categoryDTO)));
    return categoryWebResponses;
  }

  private static CategoryWebResponse toCategoryWebResponse(CategoryDTO categoryDTO) {
    CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
    BeanUtils.copyProperties(categoryDTO, categoryWebResponse);
    return categoryWebResponse;
  }


  /**
   *
   * @param categoryHierarchyResponseList
   * @return
   */
  public static List<CategorySuggestionWebResponse> toCategorySuggestionWebResponseListBaseResponse(
      List<CategoryHierarchyResponse> categoryHierarchyResponseList) {
    Map<String, CategorySuggestionWebResponse> categoryIdCategorySuggestionWebResponseMap = new HashMap<>();
    Map<String, Set<String>> categoryIdChildrenCategoryIdMap = new HashMap<>();
    Set<String> topCategoryIds = new HashSet<>();
    buildMapResponsesAndChildCategoryMapAndTopCategories(categoryHierarchyResponseList,
        categoryIdCategorySuggestionWebResponseMap, categoryIdChildrenCategoryIdMap, topCategoryIds);
    return topCategoryIds.stream().map(
        topCategoryId -> buildCategorySuggestionTree(categoryIdCategorySuggestionWebResponseMap,
            categoryIdChildrenCategoryIdMap, topCategoryId))
        .sorted(ResponseHelper::compareCategorySuggestionWebResponseByProductCount).collect(Collectors.toList());
  }

  /**
   * Build data from response
   * @param categoryHierarchyProductCountResponseList
   * @param categoryIdCategorySuggestionWebResponseMap
   * @param categoryIdChildrenCategoryIdMap
   * @param topCategoryIds
   */
  private static void buildMapResponsesAndChildCategoryMapAndTopCategories(
      List<CategoryHierarchyResponse> categoryHierarchyProductCountResponseList,
      Map<String, CategorySuggestionWebResponse> categoryIdCategorySuggestionWebResponseMap,
      Map<String, Set<String>> categoryIdChildrenCategoryIdMap, Set<String> topCategoryIds) {
    for (CategoryHierarchyResponse categoryHierarchyResponse : categoryHierarchyProductCountResponseList) {
      for (CategoryResponse categoryResponse : categoryHierarchyResponse.getCategoryHierarchy()) {
        if (categoryResponse.isActivated()) {
          CategorySuggestionWebResponse categorySuggestionWebResponse =
              categoryIdCategorySuggestionWebResponseMap.get(categoryResponse.getId());
          if (Objects.isNull(categorySuggestionWebResponse)) {
            categorySuggestionWebResponse = ResponseHelper.toCategorySuggestionWebResponse(categoryResponse, 0L);
            categoryIdCategorySuggestionWebResponseMap.put(categoryResponse.getId(), categorySuggestionWebResponse);
          }
          categorySuggestionWebResponse.setProductCount(
              categorySuggestionWebResponse.getProductCount() + categoryHierarchyResponse.getProductCount());
          if (Objects.isNull(categoryResponse.getParentCategoryId())) {
            topCategoryIds.add(categoryResponse.getId());
          } else {
            if (Objects.isNull(categoryIdChildrenCategoryIdMap.get(categoryResponse.getParentCategoryId()))) {
              categoryIdChildrenCategoryIdMap.put(categoryResponse.getParentCategoryId(), new HashSet<>());
            }
            categoryIdChildrenCategoryIdMap.get(categoryResponse.getParentCategoryId()).add(categoryResponse.getId());
          }
        }
      }
    }
  }

  /**
   * Build Category Suggestion Tree
   * @param categoryIdCategorySuggestionWebResponseMap
   * @param categoryIdChildrenCategoryIdMap
   * @param topCategoryId
   * @return
   */
  public static CategorySuggestionWebResponse buildCategorySuggestionTree(
      Map<String, CategorySuggestionWebResponse> categoryIdCategorySuggestionWebResponseMap,
      Map<String, Set<String>> categoryIdChildrenCategoryIdMap, String topCategoryId) {
    CategorySuggestionWebResponse categorySuggestionWebResponse =
        categoryIdCategorySuggestionWebResponseMap.get(topCategoryId);
    List<CategorySuggestionWebResponse> childCategories = new ArrayList<>();
    if (Objects.nonNull(categoryIdChildrenCategoryIdMap.get(topCategoryId))) {
      for (String childCategoryId : categoryIdChildrenCategoryIdMap.get(topCategoryId)) {
        childCategories.add(categoryIdCategorySuggestionWebResponseMap.get(childCategoryId));
        buildCategorySuggestionTree(categoryIdCategorySuggestionWebResponseMap, categoryIdChildrenCategoryIdMap,
            childCategoryId);
      }
    }
    categorySuggestionWebResponse.setChildCategories(
        childCategories.stream().sorted(ResponseHelper::compareCategorySuggestionWebResponseByProductCount)
            .collect(Collectors.toList()));
    return categorySuggestionWebResponse;
  }

  /**
   * Comparator to order responses by descending order of product count
   * @param categorySuggestionWebResponse1
   * @param categorySuggestionWebResponse2
   * @return
   */
  public static int compareCategorySuggestionWebResponseByProductCount(
      CategorySuggestionWebResponse categorySuggestionWebResponse1,
      CategorySuggestionWebResponse categorySuggestionWebResponse2) {
    return Long
        .compare(categorySuggestionWebResponse2.getProductCount(), categorySuggestionWebResponse1.getProductCount());
  }

  public static String getMerchantTypeFromPurchaseTermAndInventoryFulfillment(String purchaseTerm,
      String inventoryFulfillment) {
    if (Constants.INVENTORY_FULFILLMENT_BLIBLI.equals(inventoryFulfillment)) {
      if (Constants.PURCHASE_TERM_COMISSION.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_CC;
      } else if (Constants.PURCHASE_TERM_REBATE.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_RC;
      } else if (Constants.PURCHASE_TERM_PURCHASE_ORDER.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_TD;
      } else if (Constants.PURCHASE_TERM_PURCHASE_CONSIGNMENT.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_TC;
      }
    } else if (Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER.equals(inventoryFulfillment)) {
      if (Constants.PURCHASE_TERM_COMISSION.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_CM;
      } else if (Constants.PURCHASE_TERM_REBATE.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_RB;
      } else if (Constants.PURCHASE_TERM_PURCHASE_ORDER.equals(purchaseTerm)
          || Constants.PURCHASE_TERM_PURCHASE_CONSIGNMENT.equals(purchaseTerm)) {
        return Constants.MERCHANT_TYPE_MP;
      }
    }
    return null;
  }

  public static ProductLevel3SummaryWebResponse toProductLevel3SummaryWebResponse(
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse = new ProductLevel3SummaryWebResponse();
    BeanUtils.copyProperties(productLevel3SummaryResponse, productLevel3SummaryWebResponse,
        "prices", "viewConfigs", "images", "productSyncStatus", "cncActivated");

    if (CollectionUtils.isNotEmpty(productLevel3SummaryWebResponse.getActivePromoBundlings())) {
      productLevel3SummaryWebResponse.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
    }
    productLevel3SummaryWebResponse.setCncActivated(Objects.nonNull(productLevel3SummaryResponse.getCncActivated()) ?
        productLevel3SummaryResponse.getCncActivated() :
        false);
    productLevel3SummaryWebResponse.setIsArchived(productLevel3SummaryResponse.getArchived());
    productLevel3SummaryWebResponse.setPrices(
        Optional.ofNullable(productLevel3SummaryResponse.getPrices()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3PriceWebResponse).collect(Collectors.toList()));
    productLevel3SummaryWebResponse.setImages(
        Optional.ofNullable(productLevel3SummaryResponse.getImages()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3ImageWebResponse).collect(Collectors.toList()));
    productLevel3SummaryWebResponse.setViewConfigs(
        Optional.ofNullable(productLevel3SummaryResponse.getViewConfigs()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3ViewConfigWebResponse).collect(Collectors.toList()));
    if (Objects.nonNull(productLevel3SummaryResponse.getProductSyncStatus())) {
      productLevel3SummaryWebResponse.setProductSyncStatus(
          ProductSyncWebStatus.valueOf(productLevel3SummaryResponse.getProductSyncStatus().name()));
    }
    return productLevel3SummaryWebResponse;
  }

  public static PromoUpdateProductResponse toPromoUpdateProductResponse(
      BulkProcessNotesResponse bulkProcessNotesResponse) {
    String[] itemNameAndSku = bulkProcessNotesResponse.getNotes().split(Constants.COMMA_DELIMITER);
    PromoUpdateProductResponse promoUpdateProductResponse =
        new PromoUpdateProductResponse(itemNameAndSku[0], itemNameAndSku[1]);
    return promoUpdateProductResponse;
  }

  public static List<BrandPredefinedAttributeValueWebResponse> toBrandPredefinedAttributeValueWebResponse(
      BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse) {
    BrandPredefinedAttributeValueWebResponse response = new BrandPredefinedAttributeValueWebResponse();
    BeanUtils.copyProperties(brandPredefinedAttributeValueResponse, response);
    return Collections.singletonList(response);
  }

  public static boolean getThreshold(Map<Integer, Double> itemQuantityDiscountMap,
      Map<Integer, Double> quantityDiscountMap) {
    TreeMap<Integer, Double> sortedItemMap = new TreeMap<>(Collections.reverseOrder());
    sortedItemMap.putAll(itemQuantityDiscountMap);
    TreeMap<Integer, Double> sortedCategoryMap = new TreeMap<>(Collections.reverseOrder());
    sortedCategoryMap.putAll(quantityDiscountMap);
    Map<Integer, Double> editedCatQuantityDiscountMap = new HashMap<>();
    for (Map.Entry<Integer, Double> itemQuantityPercentage : sortedItemMap.entrySet()) {
      sortedCategoryMap.keySet().stream().filter(categoryMapKey -> itemQuantityPercentage.getKey() >= categoryMapKey)
          .findFirst()
          .map(s -> editedCatQuantityDiscountMap.put(itemQuantityPercentage.getKey(), sortedCategoryMap.get(s)));
    }
    return editedCatQuantityDiscountMap.keySet().stream().filter(Objects::nonNull)
        .allMatch(itemMapKey -> sortedItemMap.get(itemMapKey) >= editedCatQuantityDiscountMap.get(itemMapKey));
  }

  public static Double getDiscountBracket(List<MinWholesaleDiscountResponse> minWholesaleDiscounts, double price) {
    minWholesaleDiscounts.sort(Comparator.comparing(MinWholesaleDiscountResponse::getPrice).reversed());
    return minWholesaleDiscounts.stream().filter(minWholesaleDiscount -> minWholesaleDiscount.getPrice() <= price)
        .findFirst().map(MinWholesaleDiscountResponse::getPercentage).get();
  }

  public static WholesaleCountWebResponse toWholeCountWebResponse(WholeSaleCountResponse wholeSaleCountResponse) {
    WholesaleCountWebResponse wholesaleCountWebResponse = new WholesaleCountWebResponse();
    BeanUtils.copyProperties(wholeSaleCountResponse, wholesaleCountWebResponse);
    return wholesaleCountWebResponse;
  }

  public static boolean validateBulkDownloadFileContentResponse(
      GdnRestSimpleResponse<UnifiedBulkDownloadResponse> clientResponse, String businessPartnerCode) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, clientResponse.getErrorMessage());
    }
    if (Objects.isNull(clientResponse.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessages.ERR_UNIFIED_TEMPLATE + businessPartnerCode);
    }
    return true;
  }

  public static ProductScoreRuleWebResponse toProductScoreRuleWebResponse(
      ProductScoreRuleResponse productScoreRuleResponse) {
    ProductScoreRuleWebResponse productScoreRuleWebResponse = new ProductScoreRuleWebResponse();
    productScoreRuleWebResponse.setCategoryCode(productScoreRuleResponse.getCategoryCode());
    productScoreRuleWebResponse.setProductScoreRules(productScoreRuleResponse.getProductScoreRules().keySet().stream()
        .collect(Collectors.toMap(ruleName -> ruleName, ruleName -> toProductScoreRuleDtoWebResponse(
            productScoreRuleResponse.getProductScoreRules().get(ruleName)))));
    productScoreRuleWebResponse
        .setIgnoreAttributes(toIgnoreAttributeWebResponse(productScoreRuleResponse.getIgnoreAttributes()));
    productScoreRuleWebResponse.setIgnoreSymbols(productScoreRuleResponse.getIgnoreSymbols());
    return productScoreRuleWebResponse;
  }

  private static ProductScoreRuleDtoWebResponse toProductScoreRuleDtoWebResponse(
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse) {
    ProductScoreRuleDtoWebResponse productScoreRuleDtoWebResponse = new ProductScoreRuleDtoWebResponse();
    productScoreRuleDtoWebResponse.setMaxScore(maxScoreAndRuleConfigResponse.getMaxScore());
    productScoreRuleDtoWebResponse.setRuleConfig(toRuleConfigWebResponse(maxScoreAndRuleConfigResponse.getRuleConfig()));
    return productScoreRuleDtoWebResponse;
  }

  private static List<RuleConfigWebResponse> toRuleConfigWebResponse(List<RuleConfigResponse> ruleConfig) {
    if (Objects.nonNull(ruleConfig)) {
      return ruleConfig.stream().map(rule -> new RuleConfigWebResponse(rule.getOperator(), rule.getValue(), rule.getScore()))
          .collect(Collectors.toList());
    }
    return null;
  }

  public static UpcCodeAndImagesWebResponse toUpcCodeAndImagesWebResponse(
      ProductAndItemsResponse productAndItemsResponse) {
    UpcCodeAndImagesWebResponse upcCodeAndImagesWebResponse = new UpcCodeAndImagesWebResponse();
    if (Objects.nonNull(productAndItemsResponse.getProduct()) && CollectionUtils
        .isNotEmpty(productAndItemsResponse.getItems())) {
      upcCodeAndImagesWebResponse =
          UpcCodeAndImagesWebResponse.builder().productSku(productAndItemsResponse.getProduct().getProductSku())
              .itemResponses(Optional.ofNullable(productAndItemsResponse.getItems()).orElse(new ArrayList<>()).stream()
                  .map(itemResponse -> new UpcCodeAndImagesItemWebResponse(itemResponse.getItemSku(),
                      itemResponse.getMasterDataItem().getUpcCode(), getImages(itemResponse)))
                  .collect(Collectors.toList())).build();
    }
    return upcCodeAndImagesWebResponse;
  }

  private static List<UpcCodeAndImagesImageWebResponse> getImages(ItemResponse itemResponse) {
    List<UpcCodeAndImagesImageWebResponse> imageWebResponses = new ArrayList<>();
    for (MasterDataItemImageDTO imageDTO : Optional
        .ofNullable(itemResponse.getMasterDataItem().getMasterDataItemImages()).orElse(new ArrayList<>())) {
      UpcCodeAndImagesImageWebResponse imageWebResponse =
          UpcCodeAndImagesImageWebResponse.builder().locationPath(imageDTO.getLocationPath())
              .isMainImage(imageDTO.isMainImage()).sequence(imageDTO.getSequence()).build();
      imageWebResponses.add(imageWebResponse);
    }
    return imageWebResponses;
  }

  private static List<IgnoreAttributeWebResponse> toIgnoreAttributeWebResponse(
      List<IgnoreAttributeSet> ignoreAttributes) {
    if (Objects.nonNull(ignoreAttributes)) {
      return ignoreAttributes.stream().map(
          ignoreAttribute -> new IgnoreAttributeWebResponse(ignoreAttribute.getAttributeName(),
              ignoreAttribute.getValue(), ignoreAttribute.getIgnoreAttributeNames())).collect(Collectors.toList());
    }
    return null;
  }


  public static void clearPIIConfidentialInformationForCategoryResponse(CategoryWebResponse categoryWebResponse) {
    categoryWebResponse.setUpdatedBy(StringUtils.EMPTY);
    categoryWebResponse.setUpdatedDate(null);
    categoryWebResponse.setCreatedBy(StringUtils.EMPTY);
    categoryWebResponse.setCreatedDate(null);
  }

  public static boolean validateResponse(Response<?> clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!(clientResponse.getCode() == HttpStatus.OK.value())
        || Objects.isNull(clientResponse.getData())) {
      throw new ClientException(getErrorMessage(clientResponse.getErrors()));
    }
    return true;
  }

  public static void validateResponseForReels(ReelsResponse<?> clientResponse,
      boolean validateData) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!(StringUtils.equals(clientResponse.getStatus(), SUCCESS)) || (
        Objects.isNull(clientResponse.getData()) && validateData)) {
      throw new ClientException(getErrorMessage(clientResponse.getErrors()));
    }
  }


  public static void validateVideoResponse(Response<?> clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!(StringUtils.equals(clientResponse.getStatus(), SUCCESS)) || Objects.isNull(
        clientResponse.getData())) {
      throw new ClientException(getErrorMessage(clientResponse.getErrors()));
    }
  }

  private static String getErrorMessage(Map<String, List<String>> errorMap) {
    if (MapUtils.isNotEmpty(errorMap)) {
      return errorMap.values().stream().findFirst().orElseGet(ArrayList::new).stream().findFirst()
          .orElseGet(() -> ErrorMessages.ERR_INVALID_RESPONSE);
    }
    return ErrorMessages.ERR_INVALID_RESPONSE;
  }

  public static List<SellerLogisticsProductResponse> toListSellerLogisticsProductResponse(
      List<GetSellerLogisticProductResponse> getSellerLogisticProductResponses) {
    List<SellerLogisticsProductResponse> sellerLogisticsProductResponses = new ArrayList<>();
    for (GetSellerLogisticProductResponse response : getSellerLogisticProductResponses) {
      SellerLogisticsProductResponse sellerLogisticsProductResponse = new SellerLogisticsProductResponse();
      BeanUtils.copyProperties(response, sellerLogisticsProductResponse);
      sellerLogisticsProductResponses.add(sellerLogisticsProductResponse);
    }
    return sellerLogisticsProductResponses;
  }

  public static LogisticsDownloadTemplateResponse toLogisticDownloadTemplateResponse(
      DownloadSkuTemplateResponse downloadSkuTemplateResponse) {
    LogisticsDownloadTemplateResponse logisticsDownloadTemplateResponse =
        new LogisticsDownloadTemplateResponse();
    logisticsDownloadTemplateResponse.setContent(downloadSkuTemplateResponse.getContent());
    logisticsDownloadTemplateResponse.setName(downloadSkuTemplateResponse.getName());
    return logisticsDownloadTemplateResponse;
  }

  public static LogisticsExcelSkuUploadResponse toLogisticsExcelSkuUploadResponse(
      UploadExcelSkuUpdateResponse response) {
    LogisticsExcelSkuUploadResponse logisticsExcelSkuUploadResponse =
        new LogisticsExcelSkuUploadResponse();
    logisticsExcelSkuUploadResponse.setInvalidItemSkus(response.getInvalidItemSkus());
    logisticsExcelSkuUploadResponse.setInvalidLogisticEntry(response.getInvalidLogisticEntry());
    logisticsExcelSkuUploadResponse
        .setInvalidLogisticProductHeader(response.getInvalidLogisticProductHeader());
    return logisticsExcelSkuUploadResponse;
  }

  public static ExcelSkuUpdateStatusResponse toLogisticsExcelSkuUploadStatusResponse(
      UploadExcelSkuUpdateStatusResponse response) {
    ExcelSkuUpdateStatusResponse excelSkuUpdateStatusResponse = new ExcelSkuUpdateStatusResponse();
    excelSkuUpdateStatusResponse.setInProgress(response.isInProgress());
    excelSkuUpdateStatusResponse.setStatus(response.getStatus());
    return excelSkuUpdateStatusResponse;
  }

  public static List<LogAuditTrailUpdatedProductWebResponse> toLogAuditTrailUpdatedProductWebResponse(
      List<LogAuditTrailUpdatedProductResponse> logAuditTrailUpdatedProductResponses) throws ParseException {
    DateFormat dateFormat = new SimpleDateFormat(DATE_PATTERN);
    List<LogAuditTrailUpdatedProductWebResponse> logAuditTrailUpdatedProductWebResponses = new ArrayList<>();
    for (LogAuditTrailUpdatedProductResponse logAuditTrailUpdatedProductResponse :
        logAuditTrailUpdatedProductResponses) {
      LogAuditTrailUpdatedProductWebResponse logAuditTrailUpdatedProductWebResponse =
          new LogAuditTrailUpdatedProductWebResponse();
      logAuditTrailUpdatedProductWebResponse.setActivity(logAuditTrailUpdatedProductResponse.getActivity());
      logAuditTrailUpdatedProductWebResponse.setCreatedByLog(logAuditTrailUpdatedProductResponse.getCreatedByLog());
      logAuditTrailUpdatedProductWebResponse
          .setCreatedDateLog(dateFormat.parse(logAuditTrailUpdatedProductResponse.getCreatedDateLog()));
      logAuditTrailUpdatedProductWebResponse.setNewValue(logAuditTrailUpdatedProductResponse.getNewValue());
      logAuditTrailUpdatedProductWebResponse.setOldValue(logAuditTrailUpdatedProductResponse.getOldValue());
      logAuditTrailUpdatedProductWebResponses.add(logAuditTrailUpdatedProductWebResponse);
    }
    return logAuditTrailUpdatedProductWebResponses;
  }

  public static ProductLevel3SummaryDetailsWebResponse toProductLevel3SummaryDetailsWebResponse(
      ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse) {
    ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse = new ProductLevel3SummaryDetailsWebResponse();
    List<ProductItemWholesalePriceWebResponse> wholesalePriceWebResponses = new ArrayList<>();
    BeanUtils.copyProperties(productLevel3SummaryDetailsResponse, productLevel3SummaryDetailsWebResponse,
        "prices", "viewConfigs", "images", "productSyncStatus", "productItemWholesalePrices");
    if (CollectionUtils.isNotEmpty(productLevel3SummaryDetailsWebResponse.getActivePromoBundlings())) {
      productLevel3SummaryDetailsWebResponse.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
    }
    productLevel3SummaryDetailsWebResponse.setPrices(
        Optional.ofNullable(productLevel3SummaryDetailsResponse.getPrices()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3PriceWebResponse).collect(Collectors.toList()));
    productLevel3SummaryDetailsWebResponse.setImages(
        Optional.ofNullable(productLevel3SummaryDetailsResponse.getImages()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3ImageDetailsWebResponse).collect(Collectors.toList()));
    productLevel3SummaryDetailsWebResponse.setViewConfigs(
        Optional.ofNullable(productLevel3SummaryDetailsResponse.getViewConfigs()).orElse(new ArrayList<>()).stream()
            .map(ResponseHelper::toProductLevel3ViewConfigWebResponse).collect(Collectors.toList()));
    if (Objects.nonNull(productLevel3SummaryDetailsResponse.getProductSyncStatus())) {
      productLevel3SummaryDetailsWebResponse.setProductSyncStatus(
          ProductSyncWebStatus.valueOf(productLevel3SummaryDetailsResponse.getProductSyncStatus().name()));
    }
    if (CollectionUtils.isNotEmpty(productLevel3SummaryDetailsResponse.getProductItemWholesalePrices())) {
      wholesalePriceWebResponses = productLevel3SummaryDetailsResponse.getProductItemWholesalePrices().stream().map(
          productItemWholesalePriceResponse -> new ProductItemWholesalePriceWebResponse(
              productItemWholesalePriceResponse.getQuantity(),
              productItemWholesalePriceResponse.getWholesaleDiscount())).collect(Collectors.toList());
    }
    productLevel3SummaryDetailsWebResponse.setProductItemWholesalePrices(wholesalePriceWebResponses);
    productLevel3SummaryDetailsWebResponse.setFreeSample(productLevel3SummaryDetailsResponse.isFreeSample());
    return productLevel3SummaryDetailsWebResponse;
  }

  private static ProductLevel3SummaryDetailsImageWebResponse toProductLevel3ImageDetailsWebResponse(
      ProductLevel3SummaryDetailsImageResponse productLevel3SummaryDetailsImageResponse) {
    ProductLevel3SummaryDetailsImageWebResponse productLevel3SummaryDetailsImageWebResponse =
        new ProductLevel3SummaryDetailsImageWebResponse();
    BeanUtils.copyProperties(productLevel3SummaryDetailsImageResponse, productLevel3SummaryDetailsImageWebResponse);
    return productLevel3SummaryDetailsImageWebResponse;
  }

  public static DistinctPickUpPoint toDistinctPickUpPoint(PickupPointDTO pickupPointDTO) {
    PinPoint pinPoint = null;
    if (Objects.nonNull(pickupPointDTO.getGeolocation())) {
      pinPoint = PinPoint.builder().latitude(pickupPointDTO.getGeolocation().getLatitude())
          .longitude(pickupPointDTO.getGeolocation().getLongitude()).build();
    }
    DistinctPickUpPoint distinctPickUpPoint = DistinctPickUpPoint.builder().pickupPointCode(pickupPointDTO.getCode())
        .pickupPointName(pickupPointDTO.getName()).pinPoint(pinPoint).build();
    return distinctPickUpPoint;
  }


  public static ItemsPriceStockImagesUpdateWebResponse toItemsPriceStockImagesUpdateWebResponse(
      ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse) {
    if (Objects.nonNull(itemsPriceStockImagesUpdateResponse.getApiErrorCode())) {
      throw new ApplicationException(HttpStatus.BAD_REQUEST,
          itemsPriceStockImagesUpdateResponse.getApiErrorCode().getCode(),
          itemsPriceStockImagesUpdateResponse.getApiErrorCode().getDesc());
    }
    return getItemsPriceStockImagesUpdateWebResponse(itemsPriceStockImagesUpdateResponse);
  }

  public static ItemsPriceStockImagesUpdateWebResponse getItemsPriceStockImagesUpdateWebResponse(
      ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse) {
    ItemsPriceStockImagesUpdateWebResponse itemsPriceStockImagesUpdateWebResponse =
        new ItemsPriceStockImagesUpdateWebResponse();
    BeanUtils.copyProperties(itemsPriceStockImagesUpdateResponse, itemsPriceStockImagesUpdateWebResponse);
    itemsPriceStockImagesUpdateWebResponse.setVariantsErrorList(
        itemsPriceStockImagesUpdateResponse.getVariantsErrorList().stream()
            .map(variantsErrorListResponse -> toVariantsErrorListWebResponse(variantsErrorListResponse))
            .collect(Collectors.toList()));
    return itemsPriceStockImagesUpdateWebResponse;
  }

  private static VariantsErrorListWebResponse toVariantsErrorListWebResponse(
      VariantsErrorListResponse variantsErrorListResponse) {
    VariantsErrorListWebResponse variantsErrorListWebResponse = new VariantsErrorListWebResponse();
    BeanUtils.copyProperties(variantsErrorListResponse, variantsErrorListWebResponse, "pickupPointCode");
    variantsErrorListWebResponse.setPickupPointId(variantsErrorListResponse.getPickupPointCode());
    return variantsErrorListWebResponse;
  }

  public static List<HistorySummaryWebResponse> toHistorySummaryWebResponse(
      List<HistoryResponse> historyResponses) {
    List<HistorySummaryWebResponse> historySummeryWebRespons = new ArrayList<>();
    for (HistoryResponse historyResponse : historyResponses) {
      HistorySummaryWebResponse historySummaryWebResponse = new HistorySummaryWebResponse();
      BeanUtils.copyProperties(historyResponse, historySummaryWebResponse);
      if (StringUtils.isEmpty(historySummaryWebResponse.getNewValues())) {
        historySummaryWebResponse.setNewValues(Constants.DASH_SEPARATOR);
      }
      if (StringUtils.isEmpty(historySummaryWebResponse.getOldValues())) {
        historySummaryWebResponse.setOldValues(Constants.DASH_SEPARATOR);
      }
      historySummeryWebRespons.add(historySummaryWebResponse);
    }
    return historySummeryWebRespons;
  }

  public static PickupPointUpdateWebResponse toPickupPointUpdateWebResponse(
      PickupPointUpdateResponse pickupPointUpdateResponse) {
    PickupPointUpdateWebResponse pickupPointUpdateWebResponse = new PickupPointUpdateWebResponse();
    pickupPointUpdateWebResponse.setL3version(pickupPointUpdateResponse.getL3version());
    if (CollectionUtils.isNotEmpty(pickupPointUpdateResponse.getResponses())) {
      pickupPointUpdateWebResponse.setVariantsErrorList(pickupPointUpdateResponse.getResponses().stream()
          .map(pickupPointResponse -> toPickupPointUpdateItemWebResponse(pickupPointResponse)).collect(toList()));
    }
    return pickupPointUpdateWebResponse;
  }

  public static PickupPointUpdateItemWebResponse toPickupPointUpdateItemWebResponse(
      PickupPointResponse pickupPointResponse) {
    PickupPointUpdateItemWebResponse pickupPointUpdateItemWebResponse = new PickupPointUpdateItemWebResponse();
    pickupPointUpdateItemWebResponse.setItemSku(pickupPointResponse.getItemSku());
    pickupPointUpdateItemWebResponse.setCode(pickupPointResponse.getErrorCode());
    pickupPointUpdateItemWebResponse.setMessage(pickupPointResponse.getErrorMessage());
    return pickupPointUpdateItemWebResponse;
  }


  public static List<ProductItemNameWebResponse> toProductItemNameWebResponse(
      List<ProductItemNameResponse> productItemNameResponses) {
    List<ProductItemNameWebResponse> productItemNameWebResponses = productItemNameResponses.stream().map(
        productItemNameResponse -> ProductItemNameWebResponse.builder().itemName(productItemNameResponse.getItemName())
            .itemSku(productItemNameResponse.getItemSku()).build()).collect(toList());
    return productItemNameWebResponses;
  }

  public static ProductLevel3DetailWebResponse toProductLevel3DetailWebResponse(
      ProductLevel3DetailResponse productLevel3DetailResponse) {
    ProductLevel3DetailWebResponse productLevel3DetailWebResponse = new ProductLevel3DetailWebResponse();
    BeanUtils.copyProperties(productLevel3DetailResponse, productLevel3DetailWebResponse);
    productLevel3DetailWebResponse.setProductEditable(productLevel3DetailResponse.isProductEditable());
    productLevel3DetailWebResponse.setResubmitCount(productLevel3DetailResponse.getResubmitCount());
    if (Objects.nonNull(productLevel3DetailResponse.getProductL3Response())) {
      productLevel3DetailWebResponse.setPromoLabels(productLevel3DetailResponse.getProductL3Response().getPromoLabels());
    }
    if (CollectionUtils.isNotEmpty(productLevel3DetailResponse.getAttributes())) {
      List<ProductLevel3AttributeWebResponse> attributes =
          productLevel3DetailResponse.getAttributes().stream().map(ResponseHelper::toProductLevel3AttributeWebResponse)
              .collect(Collectors.toList());
      productLevel3DetailWebResponse.setAttributes(attributes);
    }

    if (CollectionUtils.isNotEmpty(productLevel3DetailResponse.getImages())) {
      List<ProductLevel3ImageWebResponse> images =
          productLevel3DetailResponse.getImages().stream().map(ResponseHelper::toProductLevel3ImageWebResponse)
              .collect(Collectors.toList());
      productLevel3DetailWebResponse.setImages(images);
    }
    if (Objects.nonNull(productLevel3DetailResponse.getProductScore())) {
      com.gda.mta.product.dto.ProductScoreResponse response = productLevel3DetailResponse.getProductScore();
      ProductScoreResponse productScoreResponse =
          ProductScoreResponse.builder().MANDATORY_INFO_RULE(response.getMandatoryAttributeScore())
              .PRODUCT_TITLE_RULE(response.getProductTitleScore()).DESCRIPTION_RULE(response.getDescriptionScore())
              .EAN_UPC_RULE(response.getEanUpcScore()).IMAGE_RULE(response.getImageScore())
              .RECOMMENDED_ATTRIBUTE_RULE(response.getRecommendedAttributeScore())
              .REMAINING_ATTRIBUTE_RULE(response.getRemainingAttributeScore())
              .VARIANT_CREATING_RULE(response.getVariantCreatingScore()).VIDEO_URL_RULE(response.getVideoUrlScore())
              .USP_RULE(response.getUspScore()).TOTAL_SCORE(response.getTotalScore()).build();
      productLevel3DetailWebResponse.setProductScore(productScoreResponse);
    }

    if (CollectionUtils.isNotEmpty(productLevel3DetailResponse.getProductLevel3Logistics())) {
      List<ProductItemLevel3LogisticsWebResponse> productItemLevel3LogisticsWebResponses =
          productLevel3DetailResponse.getProductLevel3Logistics().stream().map(
              productItemLevel3LogisticResponse -> new ProductItemLevel3LogisticsWebResponse(
                  productItemLevel3LogisticResponse.getLogisticProductCode(),
                  productItemLevel3LogisticResponse.getLogisticProductName(),
                  productItemLevel3LogisticResponse.isSelected(), productItemLevel3LogisticResponse.isRequiredLongLat(),
                  productItemLevel3LogisticResponse.getHighlightedInformation())).collect(Collectors.toList());
      productLevel3DetailWebResponse.setProductLevel3Logistics(productItemLevel3LogisticsWebResponses);
    }
    if (Objects.nonNull(productLevel3DetailResponse.getPreOrder())) {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      BeanUtils.copyProperties(productLevel3DetailResponse.getPreOrder(), preOrderResponse);
      productLevel3DetailWebResponse.setPreOrder(preOrderResponse);
    }
    return productLevel3DetailWebResponse;
  }

  public static boolean validateResponseForErrorCode(GdnRestSingleResponse response) {
    if (Objects.isNull(response)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      if (Objects.nonNull(response.getValue()) && response.getValue().getClass()
          .equals(PickupPointUpdateResponse.class)) {
        PickupPointUpdateResponse pickupPointUpdateResponse = (PickupPointUpdateResponse) response.getValue();
        if (Objects.nonNull(pickupPointUpdateResponse.getApiErrorCode())) {
          throw new ApplicationException(HttpStatus.BAD_REQUEST, pickupPointUpdateResponse.getApiErrorCode().getCode(),
              pickupPointUpdateResponse.getApiErrorCode().getDesc());
        } else {
          throw new ClientException(response.getErrorMessage());
        }
      } else {
        throw new ClientException(response.getErrorMessage());
      }
    }
    return true;
  }

  public static EditProductWebResponse validateEditInfoResponse(GdnRestSingleResponse<EditProductV2Response> response) {
    if (Objects.isNull(response)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (ErrorCategory.INVALID_STATE.getCode().equals(response.getErrorCode())) {
      throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.INVALID_STATE.getCode(),
          response.getErrorMessage());
    }
    if ((!response.isSuccess() && StringUtils.isBlank(response.getErrorCode())) || Objects
        .isNull(response.getValue())) {
      throw new ClientException(response.getErrorMessage());
    }
    EditProductWebResponse editProductWebResponse = new EditProductWebResponse();
    BeanUtils.copyProperties(response.getValue(), editProductWebResponse);
    if (Objects.nonNull(response.getValue().getApiErrorCode())) {
      validateSpecificErrorCode(ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode(),
          response.getValue().getApiErrorCode().getCode(),
          response.getValue().getApiErrorCode().getDesc());
      throw new ApplicationException(HttpStatus.BAD_REQUEST, response.getErrorCode(),
          response.getErrorMessage());
    }
    return editProductWebResponse;
  }

  public static boolean validateAppealProductResponse(
      GdnRestSingleResponse<AppealProductResponse> response) {
    if (Objects.isNull(response)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (Objects.isNull(response.getValue())) {
      throw new ClientException(response.getErrorMessage());
    }
    AppealProductResponse appealProductResponse = response.getValue();
    validateSpecificErrorCode(ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode(),
        appealProductResponse.getErrorCode(), appealProductResponse.getErrorMessage());
    validateSpecificErrorCode(ApiErrorCode.PRODUCT_IN_INVALID_STATE.getCode(),
        appealProductResponse.getErrorCode(), appealProductResponse.getErrorMessage());
    validateSpecificErrorCode(ApiErrorCode.PRODUCT_NOT_FOUND.getCode(),
        appealProductResponse.getErrorCode(), appealProductResponse.getErrorMessage());
    if (!response.isSuccess()) {
      throw new ClientException(response.getErrorMessage());
    }
    return true;
  }

  public static void validateSpecificErrorCode(String expectedErrorCode,
      String actualErrorCodeFromResponse, String actualErrorMessageFromResponse) {
    if (StringUtils.equals(expectedErrorCode, actualErrorCodeFromResponse)) {
      throw new ApplicationException(HttpStatus.BAD_REQUEST, actualErrorCodeFromResponse,
          actualErrorMessageFromResponse);
    }
  }

  public static boolean validateResponseForErrorCode(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      if (StringUtils.isNotBlank(clientResponse.getErrorCode())) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, clientResponse.getErrorCode(),
            clientResponse.getErrorMessage());
      } else {
        throw new ClientException(clientResponse.getErrorMessage());
      }
    }
    return true;
  }

  public static ProductL3CountWebResponse toProductL3CountWebResponse(ProductCountResponse productCountResponse,
      CountProductLevel3WipResponse countProductLevel3WipResponse, String businessPartnerCode) {
    ProductL3CountWebResponse productL3CountWebResponse = new ProductL3CountWebResponse();
    productL3CountWebResponse.setActive(productCountResponse.getActive());
    productL3CountWebResponse.setOutOfStock(productCountResponse.getOutOfStock());
    productL3CountWebResponse.setBusinessPartnerCode(businessPartnerCode);
    Map<ProductLevel3WipSummaryCriteria, Long> productCounts = countProductLevel3WipResponse.getTotalItemsByCriterias();
    productL3CountWebResponse.setNeedCorrection(productCounts.get(ProductLevel3WipSummaryCriteria.NEED_CORRECTION));
    productL3CountWebResponse.setInReview(productCounts.get(ProductLevel3WipSummaryCriteria.IN_PROGRESS));
    return productL3CountWebResponse;
  }

  public static ProductL3CountWebResponse toProductL3CountWebResponseByType(ProductCountResponse productCountResponse,
      String businessPartnerCode, String type, ProductLevel3CountResponse countResponse) {
    ProductL3CountWebResponse productL3CountWebResponse = new ProductL3CountWebResponse();
    productL3CountWebResponse.setBusinessPartnerCode(businessPartnerCode);
    if (Constants.ACTIVE_STATUS.equalsIgnoreCase(type)) {
      productL3CountWebResponse.setActive(productCountResponse.getActive());
      productL3CountWebResponse.setOutOfStock(productCountResponse.getOutOfStock());
      productL3CountWebResponse.setAll(productCountResponse.getActive() + productCountResponse.getOutOfStock());
      return productL3CountWebResponse;
    } else {
      productL3CountWebResponse.setArchived(productCountResponse.getArchived());
      productL3CountWebResponse.setSuspended(productCountResponse.getSuspended());
      Map<ProductLevel3SummaryCriteria, Long> productCounts = countResponse.getTotalItemsByCriterias();
      productL3CountWebResponse.setRejected(productCounts.get(ProductLevel3SummaryCriteria.REJECTED));
      return productL3CountWebResponse;
    }
  }

  public static ProductL3CountWebResponse toProductL3CountWebResponse(ProductCountResponse productCountResponse,
      CountProductLevel3WipResponse countProductLevel3WipResponse,
      CountProductLevel3InactiveResponse countProductLevel3InactiveResponse, String businessPartnerCode) {
    ProductL3CountWebResponse productL3CountWebResponse =
        toProductL3CountWebResponse(productCountResponse, countProductLevel3WipResponse, businessPartnerCode);
    productL3CountWebResponse.setArchived(productCountResponse.getArchived());
    productL3CountWebResponse.setSuspended(productCountResponse.getSuspended());
    Map<ProductLevel3InactiveSummaryCriteria, Long> productCounts =
        countProductLevel3InactiveResponse.getTotalItemsByCriterias();
    productL3CountWebResponse.setRejected(productCounts.get(ProductLevel3InactiveSummaryCriteria.REJECTED));
    return productL3CountWebResponse;
  }

  public static ProductLevel3ListingWebResponse toProductLevel3ListingWebResponse(
      ProductL3SummaryResponse productResponse, Map<String, String> skuXCategoryName,
      Map<String, String> skuXSuspension, Map<String, InventoryStockInfoDTO> productSkuInventoryMap,
      Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL4,
      Map<String, CampaignPriceSkuResponse> itemCampaignMapL4, String productDetailLink) {
    String productSku = productResponse.getProductSku();
    com.gdn.x.product.rest.web.model.response.ProductScoreResponse scoreResponse = productResponse.getProductScore();
    ProductLevel3ListingWebResponse productLevel3ListingWebResponse = new ProductLevel3ListingWebResponse();
    BeanUtils.copyProperties(productResponse, productLevel3ListingWebResponse, "ItemL4SummaryResponse",
        "ProductScoreResponse");
    productLevel3ListingWebResponse.setOff2OnActiveFlag(productResponse.isOff2OnChannelActive());
    productLevel3ListingWebResponse.setCategoryName(skuXCategoryName.get(productSku));
    ProductType productType =
      Optional.of(productResponse).map(ProductL3SummaryResponse::getProductType).map(Enum::name)
        .map(name -> Enum.valueOf(ProductType.class, name)).orElse(null);
    productLevel3ListingWebResponse.setProductType(
      Objects.nonNull(productType) ? productType.getProductType() : null);
    productLevel3ListingWebResponse.setDimensionsMissing(productResponse.getDimensionsMissing());
    if (MapUtils.isNotEmpty(skuXSuspension)) {
      productLevel3ListingWebResponse.setSuspensionReason(skuXSuspension.get(productSku));
    } else {
      InventoryStockInfoDTO inventoryL3 = productSkuInventoryMap.get(productSku);
      if (Objects.nonNull(inventoryL3)) {
        productLevel3ListingWebResponse.setAvailableStockLevel1(inventoryL3.getWarehouseTotalAvailableStock());
        productLevel3ListingWebResponse.setAvailableStockLevel2(inventoryL3.getWebTotalAvailableStock());
        productLevel3ListingWebResponse.setReservedStockLevel1(
            inventoryL3.getWarehouseTotalOriginalStock() - inventoryL3.getWarehouseTotalAvailableStock());
        productLevel3ListingWebResponse
            .setReservedStockLevel2(inventoryL3.getWebTotalOriginalStock() - inventoryL3.getWebTotalAvailableStock());
      } else {
        productLevel3ListingWebResponse.setShowL3Stock(false);
      }
      productLevel3ListingWebResponse.setProductDetailPageLink(productDetailLink);
      ProductScoreResponse productScoreResponse = null;
      if (Objects.nonNull(scoreResponse)) {
        productScoreResponse =
            ProductScoreResponse.builder().MANDATORY_INFO_RULE(scoreResponse.getMandatoryAttributeScore())
                .PRODUCT_TITLE_RULE(scoreResponse.getProductTitleScore()).IMAGE_RULE(scoreResponse.getImageScore())
                .DESCRIPTION_RULE(scoreResponse.getDescriptionScore()).EAN_UPC_RULE(scoreResponse.getEanUpcScore())
                .RECOMMENDED_ATTRIBUTE_RULE(scoreResponse.getRecommendedAttributeScore())
                .REMAINING_ATTRIBUTE_RULE(scoreResponse.getRemainingAttributeScore())
                .USP_RULE(scoreResponse.getUspScore()).VARIANT_CREATING_RULE(scoreResponse.getVariantCreatingScore())
                .TOTAL_SCORE(scoreResponse.getTotalScore()).VIDEO_URL_RULE(scoreResponse.getVideoUrlScore()).build();
      }
      productLevel3ListingWebResponse.setProductScore(productScoreResponse);
      if ((productResponse.getVariantCount() == Constants.NO_VARIANTS_COUNT) && Objects
          .nonNull(productResponse.getItemL4SummaryResponse())) {
        String itemSku = productResponse.getItemL4SummaryResponse().getItemSku();
        ItemL3ListingWebResponse itemSummary = new ItemL3ListingWebResponse();
        InventoryDetailInfoResponseDTO inventoryL4 = itemInventoryMapL4.get(itemSku);
        CampaignPriceSkuResponse campaignL4 = itemCampaignMapL4.get(itemSku);
        BeanUtils.copyProperties(productResponse.getItemL4SummaryResponse(), itemSummary);
        if (CollectionUtils.isNotEmpty(itemSummary.getActivePromoBundlings())) {
          itemSummary.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        }
        if (Objects.nonNull(inventoryL4)) {
          toL4InventoryResponse(inventoryL4, itemSummary);
          productLevel3ListingWebResponse.setShowL3Stock(true);
        }
        if (Objects.nonNull(campaignL4)) {
          toL4CampaignResponse(campaignL4, itemSummary, productResponse.getItemL4SummaryResponse().getItemSku(),
              productResponse.getItemL4SummaryResponse().isPriceEditDisabled());
        }
        itemSummary.setPrices(toItemPrice(productResponse.getItemL4SummaryResponse().getPrice()));
        itemSummary.setItemName(productResponse.getItemL4SummaryResponse().getGeneratedItemName());
        itemSummary.setLateFulfillment(productResponse.getItemL4SummaryResponse().getIsLateFulfillment());
        itemSummary.setImages(
          toItemImageList(productResponse.getItemL4SummaryResponse().getMasterDataItemImages()));
        itemSummary.setPickupPointCode(
          productResponse.getItemL4SummaryResponse().getPickupPointCode());
        itemSummary.setPickupPointName(
          productResponse.getItemL4SummaryResponse().getPickupPointName());
        itemSummary
            .setViewConfigs(toItemViewConfigList(productResponse.getItemL4SummaryResponse().getItemViewConfigs()));
        productLevel3ListingWebResponse.setItemSummary(itemSummary);
      }
    }
    return productLevel3ListingWebResponse;
  }

  private static List<ProductLevel3PriceWebResponse> toItemPrice(Collection<PriceDTO> priceDTOS) {
    List<ProductLevel3PriceWebResponse> priceWebResponses = new ArrayList<>();
    for (PriceDTO priceDTO : priceDTOS) {
      ProductLevel3PriceWebResponse priceWebResponse = new ProductLevel3PriceWebResponse();
      priceWebResponse.setChannelId(priceDTO.getChannel());
      priceWebResponse.setPrice(priceDTO.getListPrice());
      priceWebResponse.setSalePrice(priceDTO.getOfferPrice());
      if (CollectionUtils.isNotEmpty(priceDTO.getListOfDiscountPrices())) {
        priceWebResponse.setDiscountAmount(priceDTO.getListOfDiscountPrices().get(0).getDiscountPrice());
        priceWebResponse.setDiscountStartDate(priceDTO.getListOfDiscountPrices().get(0).getStartDateTime());
        priceWebResponse.setDiscountEndDate(priceDTO.getListOfDiscountPrices().get(0).getEndDateTime());
        priceWebResponse.setPromotionName(priceDTO.getListOfDiscountPrices().get(0).getAdjustmentName());
      }
      priceWebResponses.add(priceWebResponse);
    }
    return priceWebResponses;
  }

  private static List<ProductLevel3ImageWebResponse> toItemImageList(List<MasterDataItemImageDTO> imageDTOList) {
    List<ProductLevel3ImageWebResponse> imageList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(imageDTOList)) {
      for (MasterDataItemImageDTO image : imageDTOList) {
        ProductLevel3ImageWebResponse imageWebResponse = new ProductLevel3ImageWebResponse();
        imageWebResponse.setMainImage(image.isMainImage());
        imageWebResponse.setLocationPath(image.getLocationPath());
        imageWebResponse.setSequence(image.getSequence());
        imageList.add(imageWebResponse);
      }
    }
    return imageList;
  }

  private static List<ProductLevel3ViewConfigWebResponse> toItemViewConfigList(
      List<ItemViewConfigDTO> viewConfigDTOList) {
    List<ProductLevel3ViewConfigWebResponse> configList = new ArrayList<>();
    for (ItemViewConfigDTO viewConfig : viewConfigDTOList) {
      ProductLevel3ViewConfigWebResponse viewConfigWebResponse = new ProductLevel3ViewConfigWebResponse();
      viewConfigWebResponse.setChannelId(viewConfig.getChannel());
      viewConfigWebResponse.setBuyable(viewConfig.isBuyable());
      viewConfigWebResponse.setDisplay(viewConfig.isDiscoverable());
      if (Objects.nonNull(viewConfig.getItemBuyableSchedules())) {
        BuyableScheduleWebResponse buyableSchedule = new BuyableScheduleWebResponse();
        buyableSchedule.setBuyable(viewConfig.getItemBuyableSchedules().isBuyable());
        buyableSchedule.setStartDateTime(viewConfig.getItemBuyableSchedules().getStartDateTime());
        buyableSchedule.setEndDateTime(viewConfig.getItemBuyableSchedules().getEndDateTime());
        viewConfigWebResponse.setBuyableSchedule(buyableSchedule);
      }
      if (Objects.nonNull(viewConfig.getItemDiscoverableSchedules())) {
        DiscoverableScheduleWebResponse discoverableSchedule = new DiscoverableScheduleWebResponse();
        discoverableSchedule.setDiscoverable(viewConfig.getItemDiscoverableSchedules().isDiscoverable());
        discoverableSchedule.setStartDateTime(viewConfig.getItemDiscoverableSchedules().getStartDateTime());
        discoverableSchedule.setEndDateTime(viewConfig.getItemDiscoverableSchedules().getEndDateTime());
        viewConfigWebResponse.setDiscoverableSchedule(discoverableSchedule);
      }
      configList.add(viewConfigWebResponse);
    }
    return configList;
  }

  private static B2bFields toB2bFields(B2bFieldsDTO b2bFieldsDTO) {
    B2bFields b2bFields = new B2bFields();
    if (Objects.nonNull(b2bFieldsDTO)) {
      BeanUtils.copyProperties(b2bFieldsDTO, b2bFields);
    }
    return b2bFields;
  }

  private static void toL4InventoryResponse(InventoryDetailInfoResponseDTO inventoryResponse,
      ItemL3ListingWebResponse itemResponse) {
    WebInventoryResponseDTO webInventory = inventoryResponse.getWebInventoryResponse();
    if (Objects.nonNull(webInventory)) {
      itemResponse.setAvailableStockLevel2(webInventory.getAvailableStock());
      itemResponse.setReservedStockLevel2(webInventory.getOriginalStock() - webInventory.getAvailableStock());
      itemResponse.setSynchronizeStock(webInventory.isSyncStock());
    }
    List<WarehouseInventoryResponseDTO> warehouseInventory = inventoryResponse.getWarehouseInventoryResponseList();
    if (Objects.nonNull(warehouseInventory)) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : warehouseInventory) {
        if (Objects.nonNull(warehouseInv)) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
      }
      itemResponse.setAvailableStockLevel1(warehouseAvailable);
      itemResponse.setReservedStockLevel1(warehouseOrigin - warehouseAvailable);
    }
  }

  private static void toL4CampaignResponse(CampaignPriceSkuResponse campaignPriceSkuResponse,
      ItemL3ListingWebResponse itemResponse, String itemSku, boolean priceEditDisabled) {
    if (campaignPriceSkuResponse.isLockPriceUpdate() || campaignPriceSkuResponse.isLive()) {
      itemResponse.setPriceEditDisabled(true);
    } else {
      itemResponse.setPriceEditDisabled(priceEditDisabled);
    }
    itemResponse.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
    itemResponse.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
    itemResponse.setMinAllowedPrice(campaignPriceSkuResponse.getMinAllowedPrice());
    itemResponse.setMaxAllowedPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
    itemResponse.setLockPriceUpdate(campaignPriceSkuResponse.isLockPriceUpdate());
    if (Objects.nonNull(campaignPriceSkuResponse.getCampaignPrice())) {
      itemResponse.setCampaignPrice(campaignPriceSkuResponse.getCampaignPrice());
    }
  }

  public static List<ProductLevel3SummaryWebResponse> getCampaignData(List<ProductLevel3SummaryWebResponse> productLevel3SummaryWebResponseList,
      Map<String, CampaignPriceSkuResponse> itemCampaignMap) {
    for (ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse : productLevel3SummaryWebResponseList) {
      CampaignPriceSkuResponse campaignL4 = itemCampaignMap.get(productLevel3SummaryWebResponse.getItemSku());
      if (Objects.nonNull(campaignL4)) {
        if (campaignL4.isLockPriceUpdate() || campaignL4.isLive()) {
          productLevel3SummaryWebResponse.setPriceEditDisabled(true);
        }
        productLevel3SummaryWebResponse.setItemCampaignMapped(campaignL4.isRegistered());
        productLevel3SummaryWebResponse.setItemCampaignActivated(campaignL4.isLive());
        productLevel3SummaryWebResponse.setMinAllowedPrice(campaignL4.getMinAllowedPrice());
        productLevel3SummaryWebResponse.setMaxAllowedPrice(campaignL4.getMaxAllowedPrice());
        productLevel3SummaryWebResponse.setLockPriceUpdate(campaignL4.isLockPriceUpdate());
        if (Objects.nonNull(campaignL4.getCampaignPrice())) {
          productLevel3SummaryWebResponse.setCampaignPrice(campaignL4.getCampaignPrice());
        }
      }
    }
    return productLevel3SummaryWebResponseList;
  }

  public static InventorySummaryWebResponse getInventorySummary(InventoryDetailInfoResponseDTO invInfo,
      ReservedStockSummaryResponse reservedStockResponse, boolean isWareHouse) {
    InventorySummaryWebResponse response;
    int pendingCartReserved = 0, pendingPaymentReserved = 0, waitingForPayment, reservedStock = 0;
    if (isWareHouse) {
      List<WarehouseInventoryResponseDTO> warehouseInventory = invInfo.getWarehouseInventoryResponseList();
      if (Objects.nonNull(warehouseInventory)) {
        int warehouseAvailable = 0;
        int warehouseOrigin = 0;
        for (WarehouseInventoryResponseDTO warehouseInv : warehouseInventory) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
        reservedStock = warehouseOrigin - warehouseAvailable;
      }
    } else {
      WebInventoryResponseDTO webInventory = invInfo.getWebInventoryResponse();
      if (Objects.nonNull(webInventory)) {
        reservedStock = (webInventory.getOriginalStock() - webInventory.getAvailableStock());
      }
    }
    if (CollectionUtils.isNotEmpty(reservedStockResponse.getPendingCartReserved())) {
      pendingCartReserved =
          reservedStockResponse.getPendingCartReserved().stream().mapToInt(ReservedStockSummary::getReservedQuantity)
              .sum();
    }
    if (CollectionUtils.isNotEmpty(reservedStockResponse.getPendingPaymentReserved())) {
      pendingPaymentReserved =
          reservedStockResponse.getPendingPaymentReserved().stream().mapToInt(ReservedStockSummary::getReservedQuantity)
              .sum();
    }
    waitingForPayment = pendingCartReserved + pendingPaymentReserved;
    response = InventorySummaryWebResponse.builder().waitingForPayment(waitingForPayment)
        .orderInProgress(reservedStock - waitingForPayment).build();
    return response;
  }

  public static List<PickupPointSummaryWebResponse> toPickupPointSummaryWebResponse(
      List<PickupPointOutboundResponse> pickupPointResponses) {
    List<PickupPointSummaryWebResponse> pickupPointSummaryWebResponseList = new ArrayList<>();
    for (PickupPointOutboundResponse pickupPointResponse : pickupPointResponses) {
        PickupPointSummaryWebResponse pickupPointSummaryWebResponse = new PickupPointSummaryWebResponse();
        BeanUtils.copyProperties(pickupPointResponse, pickupPointSummaryWebResponse);
        pickupPointSummaryWebResponse.setDelivery(pickupPointResponse.getFlags().isDeliveryFlag());
      pickupPointSummaryWebResponse.setDistribution(
          Boolean.TRUE.equals(pickupPointResponse.getFlags().getWarehouseDistribution()));
        if (Objects.nonNull(pickupPointResponse.getGeolocation())) {
          GeoLocationWebResponse geoLocationWebResponse = new GeoLocationWebResponse();
          BeanUtils.copyProperties(pickupPointResponse.getGeolocation(), geoLocationWebResponse);
          pickupPointSummaryWebResponse.setGeolocation(geoLocationWebResponse);
      }
      pickupPointSummaryWebResponseList.add(pickupPointSummaryWebResponse);
    }
    return pickupPointSummaryWebResponseList;
  }

  public static List<PickupPointSummaryWebResponse> toPickupPointSummaryWebResponseFromBusinessPartnerPickupPoint(
      List<BusinessPartnerPickupPointOutboundResponse> businessPartnerPickupPointResponseList) {
    List<PickupPointSummaryWebResponse> pickupPointSummaryWebResponseList = new ArrayList<>();
    for (BusinessPartnerPickupPointOutboundResponse pickupPointResponse : businessPartnerPickupPointResponseList) {
      PickupPointSummaryWebResponse pickupPointSummaryWebResponse = new PickupPointSummaryWebResponse();
      BeanUtils.copyProperties(pickupPointResponse, pickupPointSummaryWebResponse);
      if (Objects.nonNull(pickupPointResponse.getGeolocation())) {
        GeoLocationWebResponse geoLocationWebResponse = new GeoLocationWebResponse();
        BeanUtils.copyProperties(pickupPointResponse.getGeolocation(), geoLocationWebResponse);
        pickupPointSummaryWebResponse.setGeolocation(geoLocationWebResponse);
      }
      pickupPointSummaryWebResponseList.add(pickupPointSummaryWebResponse);
    }
    return pickupPointSummaryWebResponseList;
  }

  public static ProductLevel3ListingV2WebResponse toProductLevel3ListingV2WebResponse(
    ProductL3SummaryResponse productL3SummaryResponse, Map<String, String> skuXCategoryName,
    Map<String, String> skuXSuspensionReasonMap,
    Map<String, InventoryStockInfoDTO> productSkuInventoryMap,
    Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL5,
    Map<String, CampaignPriceSkuResponse> itemCampaignMap,
    Map<String, Integer> productSkuPendingConsignmentMap, String productDetailLink, boolean populateLabelForPwpPromo) {
    String productSku = productL3SummaryResponse.getProductSku();
    com.gdn.x.product.rest.web.model.response.ProductScoreResponse scoreResponse =
      productL3SummaryResponse.getProductScore();
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
      new ProductLevel3ListingV2WebResponse();
    BeanUtils.copyProperties(productL3SummaryResponse, productLevel3ListingV2WebResponse,
      "ItemL4SummaryResponse", "ProductScoreResponse");
    productLevel3ListingV2WebResponse.setOff2OnActiveFlag(
      productL3SummaryResponse.isOff2OnChannelActive());
    productLevel3ListingV2WebResponse.setFreeSample(productL3SummaryResponse.isFreeSample());
    productLevel3ListingV2WebResponse.setPickupPointCodes(productL3SummaryResponse.getPickupPointCodes());
    productLevel3ListingV2WebResponse.setBundleProduct(productL3SummaryResponse.getBundleProduct());
    productLevel3ListingV2WebResponse.setCategoryName(skuXCategoryName.get(productSku));
    productLevel3ListingV2WebResponse.setUpdatedDate(productL3SummaryResponse.getUpdatedDate());
    productLevel3ListingV2WebResponse.setSizeChartCode(productL3SummaryResponse.getSizeChartCode());
    ProductType productType =
      Optional.of(productL3SummaryResponse).map(ProductL3SummaryResponse::getProductType).map(Enum::name)
        .map(name -> Enum.valueOf(ProductType.class, name)).orElse(null);
    productLevel3ListingV2WebResponse.setProductType(
      Objects.nonNull(productType) ? productType.getProductType() : null);
    productLevel3ListingV2WebResponse.setDimensionsMissing(productL3SummaryResponse.getDimensionsMissing());
    productLevel3ListingV2WebResponse.setProductCategoryEligibleForSizeChart(
        productL3SummaryResponse.isProductCategoryEligibleForSizeChart());
    if (MapUtils.isNotEmpty(skuXSuspensionReasonMap)) {
      productLevel3ListingV2WebResponse.setSuspensionReason(
        skuXSuspensionReasonMap.get(productSku));
    } else {
      InventoryStockInfoDTO inventoryL3 = productSkuInventoryMap.get(productSku);
      if (Objects.nonNull(inventoryL3)) {
        productLevel3ListingV2WebResponse.setAvailableStockLevel1(
          inventoryL3.getWarehouseTotalAvailableStock());
        productLevel3ListingV2WebResponse.setAvailableStockLevel2(
          inventoryL3.getWebTotalAvailableStock());
        productLevel3ListingV2WebResponse.setReservedStockLevel1(
          inventoryL3.getWarehouseTotalOriginalStock()
            - inventoryL3.getWarehouseTotalAvailableStock());
        productLevel3ListingV2WebResponse.setReservedStockLevel2(
          inventoryL3.getWebTotalOriginalStock() - inventoryL3.getWebTotalAvailableStock());
        if (Objects.nonNull(inventoryL3.getTotalStock())) {
          productLevel3ListingV2WebResponse.setTotalStock(inventoryL3.getTotalStock());
          productLevel3ListingV2WebResponse.setAvailableStockLevel2(inventoryL3.getTotalStock());
        }
      } else {
        productLevel3ListingV2WebResponse.setShowL3Stock(false);
      }
      productLevel3ListingV2WebResponse.setProductDetailPageLink(productDetailLink);
      ProductScoreResponse productScoreResponse = null;
      if (Objects.nonNull(scoreResponse)) {
        productScoreResponse = ProductScoreResponse.builder()
          .MANDATORY_INFO_RULE(scoreResponse.getMandatoryAttributeScore())
          .PRODUCT_TITLE_RULE(scoreResponse.getProductTitleScore())
          .IMAGE_RULE(scoreResponse.getImageScore())
          .DESCRIPTION_RULE(scoreResponse.getDescriptionScore())
          .EAN_UPC_RULE(scoreResponse.getEanUpcScore())
          .RECOMMENDED_ATTRIBUTE_RULE(scoreResponse.getRecommendedAttributeScore())
          .REMAINING_ATTRIBUTE_RULE(scoreResponse.getRemainingAttributeScore())
          .USP_RULE(scoreResponse.getUspScore())
          .VARIANT_CREATING_RULE(scoreResponse.getVariantCreatingScore())
          .TOTAL_SCORE(scoreResponse.getTotalScore())
          .VIDEO_URL_RULE(scoreResponse.getVideoUrlScore()).build();
      }
      productLevel3ListingV2WebResponse.setProductScore(productScoreResponse);
      if ((productL3SummaryResponse.getVariantCount() == Constants.NO_VARIANTS_COUNT)
        && Objects.nonNull(productL3SummaryResponse.getItemL4SummaryResponse())) {
        ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse =
          new ItemPickupPointSummaryWebResponse();
        itemPickupPointSummaryWebResponse.setProductType(
          productL3SummaryResponse.getItemL4SummaryResponse().getProductType());
        itemPickupPointSummaryWebResponse.setCreatedDate(
          productL3SummaryResponse.getItemL4SummaryResponse().getCreatedDate());
        itemPickupPointSummaryWebResponse.setUpdatedDate(
          productL3SummaryResponse.getItemL4SummaryResponse().getUpdatedDate());
        itemPickupPointSummaryWebResponse.setMarkForDelete(
          productL3SummaryResponse.getItemL4SummaryResponse().isMarkForDelete());
        itemPickupPointSummaryWebResponse.setUpdatedDate(
          productL3SummaryResponse.getItemL4SummaryResponse().getUpdatedDate());
        String l5Id =
          RequestHelper.toL5Id(productL3SummaryResponse.getItemL4SummaryResponse().getItemSku(),
            productL3SummaryResponse.getItemL4SummaryResponse().getPickupPointCode());
        InventoryDetailInfoResponseDTO inventoryL5 = itemInventoryMapL5.get(l5Id);
        CampaignPriceSkuResponse campaignL4 = itemCampaignMap.get(l5Id);
        BeanUtils.copyProperties(productL3SummaryResponse.getItemL4SummaryResponse(),
          itemPickupPointSummaryWebResponse);
        if (CollectionUtils.isNotEmpty(
          itemPickupPointSummaryWebResponse.getActivePromoBundlings())) {
          itemPickupPointSummaryWebResponse.getActivePromoBundlings()
            .remove(Constants.WHOLESALE_PRICE);
        }
        if (Objects.nonNull(inventoryL5)) {
          toL5InventoryResponse(inventoryL5, itemPickupPointSummaryWebResponse);
          productLevel3ListingV2WebResponse.setShowL3Stock(true);
        }
        if (Objects.nonNull(campaignL4)) {
          toL5CampaignResponse(campaignL4, itemPickupPointSummaryWebResponse,
            productL3SummaryResponse.getItemL4SummaryResponse().isPriceEditDisabled());
        }
        itemPickupPointSummaryWebResponse.setPrices(
          toItemPrice(productL3SummaryResponse.getItemL4SummaryResponse().getPrice()));
        itemPickupPointSummaryWebResponse.setItemName(
          productL3SummaryResponse.getItemL4SummaryResponse().getGeneratedItemName());
        itemPickupPointSummaryWebResponse.setPickupPointName(
          productL3SummaryResponse.getItemL4SummaryResponse().getPickupPointName());
        itemPickupPointSummaryWebResponse.setPickupPointCncActive(
            productL3SummaryResponse.getItemL4SummaryResponse().isPickupPointCncActive());
        itemPickupPointSummaryWebResponse.setPickupPointDeliveryActive(
            productL3SummaryResponse.getItemL4SummaryResponse().isPickupPointDeliveryActive());
        itemPickupPointSummaryWebResponse.setLateFulfillment(
          productL3SummaryResponse.getItemL4SummaryResponse().getIsLateFulfillment());
        itemPickupPointSummaryWebResponse.setImages(toItemImageList(
          productL3SummaryResponse.getItemL4SummaryResponse().getMasterDataItemImages()));
        itemPickupPointSummaryWebResponse.setViewConfigs(
            toItemViewConfigList(productL3SummaryResponse.getItemL4SummaryResponse().getItemViewConfigs()));
        itemPickupPointSummaryWebResponse.setB2bFields(
            toB2bFields(productL3SummaryResponse.getItemL4SummaryResponse().getB2bFieldsDTO()));
        itemPickupPointSummaryWebResponse.setDistribution(
            productL3SummaryResponse.getItemL4SummaryResponse().isDistribution());
        if (MapUtils.isNotEmpty(productSkuPendingConsignmentMap)) {
          itemPickupPointSummaryWebResponse.setInProgressConsignmentCount(
            productSkuPendingConsignmentMap.getOrDefault(
              productL3SummaryResponse.getItemL4SummaryResponse().getItemSku(), null));
        }
        if (populateLabelForPwpPromo) {
          populatePwpPromo(itemPickupPointSummaryWebResponse);
        }
        productLevel3ListingV2WebResponse.setItemPickupPointSummary(itemPickupPointSummaryWebResponse);
        productLevel3ListingV2WebResponse.setCncActivated(
            productL3SummaryResponse.getItemL4SummaryResponse().isCncActivated());
      }
    }
    return productLevel3ListingV2WebResponse;
  }

  public static void populatePwpPromo(ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse) {
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryWebResponse.getActivePromoBundlings())
        && itemPickupPointSummaryWebResponse.getActivePromoBundlings().stream().anyMatch(
        promo -> Arrays.asList(Constants.PWP_ADDITIONAL_PENDING, Constants.PWP_MAIN_PENDING, Constants.PWP_MAIN_ACTIVE,
            Constants.PWP_ADDITIONAL_ACTIVE).contains(promo))) {
      itemPickupPointSummaryWebResponse.setPromoBundling(true);
    }
  }

  private static void toL5CampaignResponse(CampaignPriceSkuResponse campaignPriceSkuResponse,
    ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse,
    boolean priceEditDisabled) {
    if (campaignPriceSkuResponse.isLockPriceUpdate() || campaignPriceSkuResponse.isLive()) {
      itemPickupPointSummaryWebResponse.setPriceEditDisabled(true);
    } else {
      itemPickupPointSummaryWebResponse.setPriceEditDisabled(priceEditDisabled);
    }
    itemPickupPointSummaryWebResponse.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
    itemPickupPointSummaryWebResponse.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
    itemPickupPointSummaryWebResponse.setMinAllowedPrice(campaignPriceSkuResponse.getMinAllowedPrice());
    itemPickupPointSummaryWebResponse.setMaxAllowedPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
    itemPickupPointSummaryWebResponse.setLockPriceUpdate(campaignPriceSkuResponse.isLockPriceUpdate());
    itemPickupPointSummaryWebResponse.setPriceNeedRevision(campaignPriceSkuResponse.isNeedRevision());
    itemPickupPointSummaryWebResponse.setPriceUpdateCriteria(Optional.ofNullable(campaignPriceSkuResponse.getPriceUpdateCriteria())
        .orElse(new HashSet<>()).stream().map(PriceUpdateCriteria::toString).collect(Collectors.toList()));
    if (Objects.nonNull(campaignPriceSkuResponse.getCampaignPrice())) {
      itemPickupPointSummaryWebResponse.setCampaignPrice(campaignPriceSkuResponse.getCampaignPrice());
    }
  }

  private static void toL5InventoryResponse(InventoryDetailInfoResponseDTO inventoryL5,
    ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse) {
    WebInventoryResponseDTO webInventory = inventoryL5.getWebInventoryResponse();
    itemPickupPointSummaryWebResponse.setAvailableStockLevel2(webInventory.getAvailableStock());
    itemPickupPointSummaryWebResponse.setReservedStockLevel2(
      webInventory.getOriginalStock() - webInventory.getAvailableStock());
    itemPickupPointSummaryWebResponse.setWebSyncStock(webInventory.isSyncStock());
    List<WarehouseInventoryResponseDTO> warehouseInventory =
      inventoryL5.getWarehouseInventoryResponseList();
    if (CollectionUtils.isNotEmpty(warehouseInventory)) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : warehouseInventory) {
        warehouseAvailable += warehouseInv.getAvailableStock();
        warehouseOrigin += warehouseInv.getOriginalStock();
      }
      itemPickupPointSummaryWebResponse.setAvailableStockLevel1(warehouseAvailable);
      itemPickupPointSummaryWebResponse.setReservedStockLevel1(
        warehouseOrigin - warehouseAvailable);
    }
  }

  public static List<PickupPointDetailWebResponse> toPickupPointDetailWebResponse(
    List<PickupPointDetailResponse> pickupPointDetailResponseList) {
    List<PickupPointDetailWebResponse> pickupPointDetailWebResponseList = new ArrayList<>();
    for (PickupPointDetailResponse pickupPointDetailResponse : pickupPointDetailResponseList) {
      PickupPointDetailWebResponse pickupPointDetailWebResponse =
        new PickupPointDetailWebResponse();
      BeanUtils.copyProperties(pickupPointDetailResponse, pickupPointDetailWebResponse);
      pickupPointDetailWebResponseList.add(pickupPointDetailWebResponse);
    }
    return pickupPointDetailWebResponseList;
  }

  public static List<HistoryUpdateWebResponse> toHistoryUpdateWebResponseList(
    List<HistoryUpdateResponse> historyUpdateResponseList) {
    List<HistoryUpdateWebResponse> historyUpdateWebResponseList = new ArrayList<>();
    for (HistoryUpdateResponse historyUpdateResponse : historyUpdateResponseList) {
      HistoryUpdateWebResponse historyUpdateWebResponse = new HistoryUpdateWebResponse();
      BeanUtils.copyProperties(historyUpdateResponse, historyUpdateWebResponse);
      historyUpdateWebResponseList.add(historyUpdateWebResponse);
    }
    return historyUpdateWebResponseList;
  }

  public static List<WholesalePromoV2Response> toWholesalePromoV2ResponseList(
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponses) {
    List<WholesalePromoV2Response> wholesalePromoV2Responses = new ArrayList<>();
    for (WholesalePriceSkuResponse wholesaleResponse : wholesalePriceSkuResponses) {
      WholesalePromoV2Response wholesalePromoV2Response = new WholesalePromoV2Response();
      wholesalePromoV2Response.setItemSku(wholesaleResponse.getItemSku());
      wholesalePromoV2Response.setWholesalePromo(
        Objects.nonNull(wholesaleResponse.getPromoActive()) ? wholesaleResponse.getPromoActive() : false);
      wholesalePromoV2Response.setPickupPointCode(wholesaleResponse.getPickUpPointCode());
      wholesalePromoV2Responses.add(wholesalePromoV2Response);
    }
    return wholesalePromoV2Responses;
  }

  public static List<ItemPickupPointListingL3WebResponse> toItemPickupPointListingL3WebResponse(
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList,
    Map<String, Integer> itemSkuXinProgressCFMap, String baseUrlForPdpWithL5, boolean populateLabelForPwpPromo) {
    List<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponseList = new ArrayList<>();
    for (ItemPickupPointListingL3Response itemPickupPointListingL3Response : itemPickupPointListingL3ResponseList) {
      ItemPickupPointListingL3WebResponse itemPickupPointListingL3WebResponse =
          new ItemPickupPointListingL3WebResponse();
      BeanUtils.copyProperties(itemPickupPointListingL3Response, itemPickupPointListingL3WebResponse, "prices",
          "viewConfigs", "images", "productItemWholesalePrices");
      itemPickupPointListingL3WebResponse.setPrices(
          toProductLevel3PriceWebResponse(itemPickupPointListingL3Response.getPrices()));
      itemPickupPointListingL3WebResponse.setViewConfigs(
          toProductLevel3ViewConfigWebResponse(itemPickupPointListingL3Response.getViewConfigs()));
      itemPickupPointListingL3WebResponse.setImages(
          toProductLevel3ImageWebResponse(itemPickupPointListingL3Response.getImages()));
      itemPickupPointListingL3WebResponse.setProductItemWholesalePrices(
          toProductItemWholesalePriceWebResponse(itemPickupPointListingL3Response.getProductItemWholesalePrices()));
      itemPickupPointListingL3WebResponse.setB2bFields(toB2BResponse(itemPickupPointListingL3Response.getB2bFields()));
      if (MapUtils.isNotEmpty(itemSkuXinProgressCFMap)) {
        itemPickupPointListingL3WebResponse.setInProgressConsignmentCount(
          itemSkuXinProgressCFMap.getOrDefault(itemPickupPointListingL3Response.getItemSku(),
            null));
      }
      itemPickupPointListingL3WebResponse.setProductType(itemPickupPointListingL3Response.getProductType());
      itemPickupPointListingL3WebResponse.setDimensionsMissing(itemPickupPointListingL3Response.getDimensionsMissing());
      if(CollectionUtils.isNotEmpty(itemPickupPointListingL3WebResponse.getActivePromoBundlings())){
        itemPickupPointListingL3WebResponse.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
      itemPickupPointListingL3WebResponse.setPdpUrl(
          setPdpUrlWithPickupPointCode(itemPickupPointListingL3Response.getItemSku(),
              itemPickupPointListingL3Response.getPickupPointCode(), baseUrlForPdpWithL5));
      if (populateLabelForPwpPromo) {
        populatePwpPromo(itemPickupPointListingL3WebResponse);
      }
      itemPickupPointListingL3WebResponseList.add(itemPickupPointListingL3WebResponse);
    }
    return itemPickupPointListingL3WebResponseList;
  }

  public static void populatePwpPromo(ItemPickupPointListingL3WebResponse itemPickupPointListingL3WebResponse) {
    if (CollectionUtils.isNotEmpty(itemPickupPointListingL3WebResponse.getActivePromoBundlings())) {
      if (itemPickupPointListingL3WebResponse.getActivePromoBundlings().stream().anyMatch(
          promo -> Arrays.asList(Constants.PWP_ADDITIONAL_PENDING, Constants.PWP_MAIN_PENDING,
              Constants.PWP_MAIN_ACTIVE, Constants.PWP_ADDITIONAL_ACTIVE).contains(promo))) {
        itemPickupPointListingL3WebResponse.getActivePromoBundlings()
            .removeIf(promo -> StringUtils.equalsIgnoreCase(promo, Constants.PWP_ADDITIONAL_PENDING));
        itemPickupPointListingL3WebResponse.getActivePromoBundlings()
            .removeIf(promo -> StringUtils.equalsIgnoreCase(promo, Constants.PWP_MAIN_PENDING));
        itemPickupPointListingL3WebResponse.setPromoBundling(true);
      }
    }
  }

  private static String setPdpUrlWithPickupPointCode (String itemSku, String pickupPointCode, String baseUrlForPdpWithL5) {
    String urlProduct = baseUrlForPdpWithL5.replace(PRODUCT_SKU, IS + itemSku);
    urlProduct += String.format(PICKUP_POINT_QUERY, pickupPointCode);
    return urlProduct;
  }

  private static B2BResponse toB2BResponse(com.gda.mta.product.dto.response.B2BResponse b2BResponse) {
    if (Objects.nonNull(b2BResponse)) {
      B2BResponse response = new B2BResponse();
      BeanUtils.copyProperties(b2BResponse, response);
      return response;
    }
    return null;
  }

  private static List<ProductLevel3PriceWebResponse> toProductLevel3PriceWebResponse(
      List<ProductLevel3PriceResponse> priceResponseList) {
    List<ProductLevel3PriceWebResponse> productLevel3PriceWebResponseList = new ArrayList<>();
    for (ProductLevel3PriceResponse priceResponse : priceResponseList) {
      ProductLevel3PriceWebResponse productLevel3PriceWebResponse = new ProductLevel3PriceWebResponse();
      BeanUtils.copyProperties(priceResponse, productLevel3PriceWebResponse);
      productLevel3PriceWebResponseList.add(productLevel3PriceWebResponse);
    }
    return productLevel3PriceWebResponseList;
  }

  private static List<ProductLevel3ViewConfigWebResponse> toProductLevel3ViewConfigWebResponse(
      List<ProductLevel3ViewConfigResponse> viewConfigResponseList) {
    List<ProductLevel3ViewConfigWebResponse> productLevel3ViewConfigWebResponseList = new ArrayList<>();
    for (ProductLevel3ViewConfigResponse viewConfigResponse : viewConfigResponseList) {
      ProductLevel3ViewConfigWebResponse productLevel3PriceWebResponse = new ProductLevel3ViewConfigWebResponse();
      BeanUtils.copyProperties(viewConfigResponse, productLevel3PriceWebResponse);
      if (Objects.nonNull(viewConfigResponse.getBuyableScheduleResponse())) {
        productLevel3PriceWebResponse.setBuyableSchedule(new BuyableScheduleWebResponse());
        BeanUtils.copyProperties(viewConfigResponse.getBuyableScheduleResponse(),
            productLevel3PriceWebResponse.getBuyableSchedule());
      }
      if (Objects.nonNull(viewConfigResponse.getDiscoverableScheduleResponse())) {
        productLevel3PriceWebResponse.setDiscoverableSchedule(new DiscoverableScheduleWebResponse());
        BeanUtils.copyProperties(viewConfigResponse.getDiscoverableScheduleResponse(),
            productLevel3PriceWebResponse.getDiscoverableSchedule());
      }
      productLevel3ViewConfigWebResponseList.add(productLevel3PriceWebResponse);
    }
    return productLevel3ViewConfigWebResponseList;
  }

  private static List<ItemImageWebResponse> toProductLevel3ImageWebResponse(
      List<ImageResponse> imageResponseList) {
    List<ItemImageWebResponse> itemImageWebResponseList = new ArrayList<>();
    if (Objects.nonNull(imageResponseList)) {
      for (ImageResponse imageResponse : imageResponseList) {
        ItemImageWebResponse itemImageWebResponse = new ItemImageWebResponse();
        BeanUtils.copyProperties(imageResponse, itemImageWebResponse);
        itemImageWebResponseList.add(itemImageWebResponse);
      }
    }
    return itemImageWebResponseList;
  }

  private static List<ProductItemWholesalePriceWebResponse> toProductItemWholesalePriceWebResponse(
      List<ProductItemWholesalePriceResponse> productLevel3ImageWebResponseList) {
    List<ProductItemWholesalePriceWebResponse> productItemWholesalePriceWebResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productLevel3ImageWebResponseList)) {
      for (ProductItemWholesalePriceResponse productItemWholesalePriceResponse : productLevel3ImageWebResponseList) {
        ProductItemWholesalePriceWebResponse productItemWholesalePriceWebResponse =
            new ProductItemWholesalePriceWebResponse();
        BeanUtils.copyProperties(productItemWholesalePriceResponse, productItemWholesalePriceWebResponse);
        productItemWholesalePriceWebResponseList.add(productItemWholesalePriceWebResponse);
      }
    }
    return productItemWholesalePriceWebResponseList;
  }

  public static List<BusinessPartnerPickupPointWebResponse> toBusinessPartnerPickupPointWebResponse(
    List<BusinessPartnerPickupPointOutboundResponse> webResponseList) {
    List<BusinessPartnerPickupPointWebResponse> businessPartnerPickupPointWebResponses = new ArrayList<>();
    for (BusinessPartnerPickupPointOutboundResponse businessPartnerPickupPointResponse : webResponseList) {
      BusinessPartnerPickupPointWebResponse webResponse =
        new BusinessPartnerPickupPointWebResponse();
      BeanUtils.copyProperties(businessPartnerPickupPointResponse, webResponse);
      if (Objects.nonNull(businessPartnerPickupPointResponse.getGeolocation())) {
        GeoLocationWebResponse geoLocationWebResponse = new GeoLocationWebResponse();
        BeanUtils.copyProperties(businessPartnerPickupPointResponse.getGeolocation(),
          geoLocationWebResponse);
        webResponse.setGeoLocationWebResponse(geoLocationWebResponse);
      }
      businessPartnerPickupPointWebResponses.add(webResponse);
    }
    return businessPartnerPickupPointWebResponses;
  }

  public static ProductL3DetailWebResponse toProductL3DetailWebResponse(
      ProductL3DetailsResponse productL3DetailsResponse, String productDetailLink,
    boolean isFbbFetchRequired) {
    ProductL3DetailWebResponse productL3DetailWebResponse = new ProductL3DetailWebResponse();
    BeanUtils.copyProperties(productL3DetailsResponse, productL3DetailWebResponse, "attributes",
      "commonImages", "productLevel3Logistics", "preOrder");
    productL3DetailWebResponse.setProductEditable(productL3DetailsResponse.isProductEditable());
    productL3DetailWebResponse.setProductDetailPageLink(productDetailLink);
    productL3DetailWebResponse.setResubmitCount(productL3DetailsResponse.getResubmitCount());
    productL3DetailWebResponse.setDimensionsMissing(productL3DetailsResponse.getDimensionsMissing());
    if (Objects.nonNull(productL3DetailsResponse.getProductL3Response())) {
      productL3DetailWebResponse.setPromoLabels(productL3DetailsResponse.getProductL3Response().getPromoLabels());
    }
    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getAttributes())) {
      List<ProductLevel3AttributeWebResponse> attributes =
        productL3DetailsResponse.getAttributes().stream().map(ResponseHelper::toProductLevel3AttributeWebResponse)
          .collect(Collectors.toList());
      productL3DetailWebResponse.setAttributes(attributes);
    }

    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getCommonImages())) {
      List<ProductL3CommonImageWebResponse> images =
        productL3DetailsResponse.getCommonImages().stream().map(ResponseHelper::toProductL3CommonImageWebResponse)
          .collect(Collectors.toList());
      productL3DetailWebResponse.setCommonImages(images);
    }
    if (Objects.nonNull(productL3DetailsResponse.getProductScore())) {
      com.gda.mta.product.dto.ProductScoreResponse response = productL3DetailsResponse.getProductScore();
      ProductScoreResponse productScoreResponse =
        ProductScoreResponse.builder().MANDATORY_INFO_RULE(response.getMandatoryAttributeScore())
          .PRODUCT_TITLE_RULE(response.getProductTitleScore()).DESCRIPTION_RULE(response.getDescriptionScore())
          .EAN_UPC_RULE(response.getEanUpcScore()).IMAGE_RULE(response.getImageScore())
          .RECOMMENDED_ATTRIBUTE_RULE(response.getRecommendedAttributeScore())
          .REMAINING_ATTRIBUTE_RULE(response.getRemainingAttributeScore())
          .VARIANT_CREATING_RULE(response.getVariantCreatingScore()).VIDEO_URL_RULE(response.getVideoUrlScore())
          .USP_RULE(response.getUspScore()).TOTAL_SCORE(response.getTotalScore()).build();
      productL3DetailWebResponse.setProductScore(productScoreResponse);
    }

    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getProductLevel3Logistics())) {
      List<ProductItemLevel3LogisticsWebResponse> productItemLevel3LogisticsWebResponses =
        productL3DetailsResponse.getProductLevel3Logistics().stream().map(
          productItemLevel3LogisticResponse -> new ProductItemLevel3LogisticsWebResponse(
            productItemLevel3LogisticResponse.getLogisticProductCode(),
            productItemLevel3LogisticResponse.getLogisticProductName(),
            productItemLevel3LogisticResponse.isSelected(), productItemLevel3LogisticResponse.isRequiredLongLat(),
            productItemLevel3LogisticResponse.getHighlightedInformation())).collect(Collectors.toList());
      productL3DetailWebResponse.setProductLevel3Logistics(productItemLevel3LogisticsWebResponses);
    }
    if (Objects.nonNull(productL3DetailsResponse.getPreOrder())) {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      BeanUtils.copyProperties(productL3DetailsResponse.getPreOrder(), preOrderResponse);
      productL3DetailWebResponse.setPreOrder(preOrderResponse);
    }
    if (!isFbbFetchRequired) {
      //This is being done to add fbb pp code to pp code list to support backward compatibility
      setPickupPointListForOldApps(productL3DetailWebResponse);
    }
    if (Objects.nonNull(productL3DetailsResponse.getDistributionInfoResponse())) {
      DistributionInfoWebResponse distributionInfoResponse = new DistributionInfoWebResponse();
      BeanUtils.copyProperties(productL3DetailsResponse.getDistributionInfoResponse(), distributionInfoResponse);
      productL3DetailWebResponse.setDistributionInfoResponse(distributionInfoResponse);
    }
    productL3DetailWebResponse.setAiGeneratedBrand(
        Optional.ofNullable(productL3DetailsResponse.getAiGeneratedFieldsResponse())
            .map(AiGeneratedFieldsResponse::isAiGeneratedBrand).orElse(false));
    productL3DetailWebResponse.setAiGeneratedCategory(
        Optional.ofNullable(productL3DetailsResponse.getAiGeneratedFieldsResponse())
            .map(AiGeneratedFieldsResponse::isAiGeneratedCategory).orElse(false));
    return productL3DetailWebResponse;
  }

  private static void setPickupPointListForOldApps(
    ProductL3DetailWebResponse productL3DetailWebResponse) {
    if (CollectionUtils.isEmpty(productL3DetailWebResponse.getPickupPointCodes())) {
      productL3DetailWebResponse.setPickupPointCodes(
        productL3DetailWebResponse.getFbbPickupPointCodes());
    } else {
      List<String> pickupPointList =
        new ArrayList<>(productL3DetailWebResponse.getPickupPointCodes());
      pickupPointList.addAll(productL3DetailWebResponse.getFbbPickupPointCodes());
      productL3DetailWebResponse.setPickupPointCodes(pickupPointList);
    }
  }

  public static List<ItemLevel4ListingWebResponse> toItemLevel4ListingWebResponse(
      GdnRestListResponse<ItemLevel4ListingResponse> feignResponse,
      GdnRestListResponse<ProductBundleRecipeEditableResponse> bundleRecipeEditableInfoByItemCodes, String pdpPagePrefix) {
    List<ItemLevel4ListingWebResponse> webResponses = new ArrayList<>();
    Map<String, Boolean> itemSkuToBundleRecipeEditableMap =
        getBundleRecipeEditableInfoByItemCodeMap(bundleRecipeEditableInfoByItemCodes);
    for (ItemLevel4ListingResponse webResponse : feignResponse.getContent()) {
      ItemLevel4ListingWebResponse response = new ItemLevel4ListingWebResponse();

      response.setBundleRecipeEditable(itemSkuToBundleRecipeEditableMap.getOrDefault(webResponse.getItemCode(), true));
      BeanUtils.copyProperties(webResponse, response);
      response.setBundleItemResponses(toBundleItemWebResponse(webResponse.getBundleItemResponses(), pdpPagePrefix));
      response.setPdpUrl(RequestHelper.toProductDetailPage(response.getProductSku(), pdpPagePrefix));
      response.setActive(true);
      response.setSharedProduct(webResponse.isSharedProduct());
      webResponses.add(response);
    }
    return webResponses;
  }

  private static List<BundleItemWebResponse> toBundleItemWebResponse(List<BundleItemResponse> bundleItemResponseList, String pdpPagePrefix) {
    List<BundleItemWebResponse> bundleItemWebResponses = new ArrayList<>();
    for (BundleItemResponse bundleItemResponse : Optional.ofNullable(bundleItemResponseList).orElse(new ArrayList<>())) {
      BundleItemWebResponse bundleItemWebResponse = new BundleItemWebResponse();
      BeanUtils.copyProperties(bundleItemResponse, bundleItemWebResponse);
      bundleItemWebResponse.setPdpUrl(RequestHelper.toProductDetailPage(bundleItemWebResponse.getProductSku(), pdpPagePrefix));
      bundleItemWebResponse.setSharedProduct(bundleItemResponse.isSharedProduct());
      setBundleItemStatus(bundleItemWebResponse);
      bundleItemWebResponses.add(bundleItemWebResponse);
    }
    return bundleItemWebResponses;
  }

  private static void setBundleItemStatus(BundleItemWebResponse bundleItemWebResponse) {
    if(StringUtils.isBlank(bundleItemWebResponse.getProductStatus())) {
      bundleItemWebResponse.setProductStatus(Constants.INACTIVE_STATUS);
    }
  }

  public static List<ItemLevel4ListingWebResponse> fromItemSummaryL4ResponseToItemLevel4ListingWebResponse(
      GdnRestListResponse<ItemSummaryL4Response> itemSummaryL4ListResponse,
      GdnRestListResponse<ProductBundleRecipeEditableResponse> bundleRecipeEditableInfoByItemCodes) {
    List<ItemLevel4ListingWebResponse> webResponses = new ArrayList<>();
    Map<String, Boolean> itemSkuToBundleRecipeEditableMap =
        getBundleRecipeEditableInfoByItemCodeMap(bundleRecipeEditableInfoByItemCodes);
    for (ItemSummaryL4Response itemSummaryL4Response : itemSummaryL4ListResponse.getContent()) {
      ItemLevel4ListingWebResponse response = new ItemLevel4ListingWebResponse();
      response.setBundleRecipeEditable(itemSkuToBundleRecipeEditableMap.getOrDefault(itemSummaryL4Response.getItemCode(), true));
      BeanUtils.copyProperties(itemSummaryL4Response, response);
      webResponses.add(response);
    }
    return webResponses;
  }

  private static Map<String, Boolean> getBundleRecipeEditableInfoByItemCodeMap(
      GdnRestListResponse<ProductBundleRecipeEditableResponse> bundleRecipeEditableInfoByItemCodes) {
    return Optional.ofNullable(bundleRecipeEditableInfoByItemCodes.getContent()).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).collect(Collectors.toMap(ProductBundleRecipeEditableResponse::getItemCode,
            ProductBundleRecipeEditableResponse::isBundleRecipeEditable));
  }

  public static List<ProductLevel3ListingV2WebResponse> toProductLevel3ListingV2WebResponseList(
    List<ProductLevel3ListingWebResponse> productLevel3ListingWebResponseList) {
    return Optional.ofNullable(productLevel3ListingWebResponseList).orElse(new ArrayList<>())
      .stream().map(ResponseHelper::convertToProductLevel3ListingV2WebResponse)
      .collect(Collectors.toList());
  }

  private static ProductLevel3ListingV2WebResponse convertToProductLevel3ListingV2WebResponse(
    ProductLevel3ListingWebResponse productLevel3ListingWebResponse) {
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
      new ProductLevel3ListingV2WebResponse();
    BeanUtils.copyProperties(productLevel3ListingWebResponse, productLevel3ListingV2WebResponse);
    if (Objects.nonNull(productLevel3ListingWebResponse.getItemSummary())) {
      productLevel3ListingV2WebResponse.setItemPickupPointSummary(
        convertToItemPickupPointSummary(productLevel3ListingWebResponse.getItemSummary()));
    }
    return productLevel3ListingV2WebResponse;
  }

  private static ItemPickupPointSummaryWebResponse convertToItemPickupPointSummary(
    ItemL3ListingWebResponse itemL3ListingWebResponse) {
    ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse =
      new ItemPickupPointSummaryWebResponse();
    BeanUtils.copyProperties(itemL3ListingWebResponse, itemPickupPointSummaryWebResponse);
    itemPickupPointSummaryWebResponse.setPrices(itemL3ListingWebResponse.getPrices());
    itemPickupPointSummaryWebResponse.setWebSyncStock(
      itemL3ListingWebResponse.getSynchronizeStock());
    return itemPickupPointSummaryWebResponse;
  }

  public static ProductLevel3V2WebResponse toProductL3DetailWebResponse(
    ProductLevel3DetailsV2Response productL3DetailsResponse) {
    ProductLevel3V2WebResponse productL3DetailWebResponse = new ProductLevel3V2WebResponse();
    BeanUtils.copyProperties(productL3DetailsResponse, productL3DetailWebResponse);
    productL3DetailWebResponse.setProductEditable(productL3DetailsResponse.isProductEditable());
    productL3DetailWebResponse.setResubmitCount(productL3DetailsResponse.getResubmitCount());
    if (Objects.nonNull(productL3DetailsResponse.getProductL3Response())) {
      productL3DetailWebResponse.setPromoLabels(productL3DetailsResponse.getProductL3Response().getPromoLabels());
    }
    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getAttributes())) {
      List<ProductLevel3AttributeWebResponse> attributes =
        productL3DetailsResponse.getAttributes().stream().map(ResponseHelper::toProductLevel3AttributeWebResponse)
          .collect(Collectors.toList());
      productL3DetailWebResponse.setAttributes(attributes);
    }

    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getCommonImages())) {
      List<ProductL3CommonImageWebResponse> images =
        productL3DetailsResponse.getCommonImages().stream().map(ResponseHelper::toProductL3CommonImageWebResponse)
          .collect(Collectors.toList());
      productL3DetailWebResponse.setCommonImages(images);
    }
    if (Objects.nonNull(productL3DetailsResponse.getProductScore())) {
      com.gda.mta.product.dto.ProductScoreResponse response = productL3DetailsResponse.getProductScore();
      ProductScoreResponse productScoreResponse =
        ProductScoreResponse.builder().MANDATORY_INFO_RULE(response.getMandatoryAttributeScore())
          .PRODUCT_TITLE_RULE(response.getProductTitleScore()).DESCRIPTION_RULE(response.getDescriptionScore())
          .EAN_UPC_RULE(response.getEanUpcScore()).IMAGE_RULE(response.getImageScore())
          .RECOMMENDED_ATTRIBUTE_RULE(response.getRecommendedAttributeScore())
          .REMAINING_ATTRIBUTE_RULE(response.getRemainingAttributeScore())
          .VARIANT_CREATING_RULE(response.getVariantCreatingScore()).VIDEO_URL_RULE(response.getVideoUrlScore())
          .USP_RULE(response.getUspScore()).TOTAL_SCORE(response.getTotalScore()).build();
      productL3DetailWebResponse.setProductScore(productScoreResponse);
    }

    if (CollectionUtils.isNotEmpty(productL3DetailsResponse.getProductLevel3Logistics())) {
      List<ProductItemLevel3LogisticsWebResponse> productItemLevel3LogisticsWebResponses =
        productL3DetailsResponse.getProductLevel3Logistics().stream().map(
          productItemLevel3LogisticResponse -> new ProductItemLevel3LogisticsWebResponse(
            productItemLevel3LogisticResponse.getLogisticProductCode(),
            productItemLevel3LogisticResponse.getLogisticProductName(),
            productItemLevel3LogisticResponse.isSelected(), productItemLevel3LogisticResponse.isRequiredLongLat(),
            productItemLevel3LogisticResponse.getHighlightedInformation())).collect(Collectors.toList());
      productL3DetailWebResponse.setProductLevel3Logistics(productItemLevel3LogisticsWebResponses);
    }
    if (Objects.nonNull(productL3DetailsResponse.getPreOrder())) {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      BeanUtils.copyProperties(productL3DetailsResponse.getPreOrder(), preOrderResponse);
      productL3DetailWebResponse.setPreOrder(preOrderResponse);
    }
    return productL3DetailWebResponse;
  }

  public static EditProductWebResponse getEditProductWebResponseWebResponse(EditProductV2Response editProductResponse) {
    EditProductWebResponse editProductWebResponse = new EditProductWebResponse();
    BeanUtils.copyProperties(editProductResponse, editProductWebResponse);
    if (CollectionUtils.isNotEmpty(editProductResponse.getVariantsErrorList())) {
      editProductWebResponse.setVariantsErrorList(
          editProductResponse.getVariantsErrorList().stream().map(ResponseHelper::toVariantsErrorListWebResponse)
              .collect(Collectors.toList()));
      throw new EditProductException(editProductResponse.getVariantsErrorList().stream().findFirst().get().getCode(),
          editProductResponse.getVariantsErrorList().stream().findFirst().get().getMessage(), editProductWebResponse);
    }
    return editProductWebResponse;
  }

  public static boolean validateResponseForErrorCodeV2(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      if (ErrorCategory.VALIDATION.name().equals(clientResponse.getErrorCode())) {
        throw new ValidationException(clientResponse.getErrorMessage());
      }
      if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode()))
          .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
        throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
      }
      if (StringUtils.isNotBlank(clientResponse.getErrorCode())) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, clientResponse.getErrorCode(),
          clientResponse.getErrorMessage());
      } else {
        throw new ClientException(clientResponse.getErrorMessage());
      }
    }
    return true;
  }

  public static boolean validateResponseForGetProductL3(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse) || ErrorCategory.COMMUNICATION_FAILURE.getMessage()
        .equals(clientResponse.getErrorCode())) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      if (ErrorCategory.VALIDATION.name().equals(clientResponse.getErrorCode())) {
        throw new ValidationException(clientResponse.getErrorMessage());
      }
      if (EnumSet.allOf(ApiErrorCode.class).stream().filter(code -> StringUtils.isNotBlank(code.getCode()))
          .anyMatch(code -> code.getCode().equals(clientResponse.getErrorCode()))) {
        throw new ApiIncorrectInputDataException(clientResponse.getErrorMessage(), clientResponse.getErrorCode());
      }
      if (StringUtils.isNotBlank(clientResponse.getErrorCode())) {
        throw new ProductListingGenericException(clientResponse.getErrorCode(), clientResponse.getErrorMessage());
      } else {
        throw new ClientException(clientResponse.getErrorMessage());
      }
    }
    return true;
  }

  public static void modifyProductCampaignAvailabilityResponse(
      ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse) {
    if (Objects.nonNull(productCampaignAvailabilityResponse) && MapUtils
        .isEmpty(productCampaignAvailabilityResponse.getProductCampaignAvailabilityMap())) {
      Map<String, Boolean> productCampaignAvailabilityMap = new HashMap<>();
      if (CollectionUtils.isNotEmpty(productCampaignAvailabilityResponse.getProductCampaignAvailabilityInfo())) {
        productCampaignAvailabilityMap =
            productCampaignAvailabilityResponse.getProductCampaignAvailabilityInfo().stream().collect(Collectors
                .toMap(ProductCampaignAvailabilityInfoDto::getItemSku,
                    ProductCampaignAvailabilityInfoDto::isAvailability, (a, b) -> a));
      }
      productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(productCampaignAvailabilityMap);
    }
  }

  public static void modifyCampaignPriceResponse(CampaignPriceResponse campaignPriceResponse) {
    Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponseMap;
    if (Objects.nonNull(campaignPriceResponse) && MapUtils.isEmpty(campaignPriceResponse.getItemSkuToPriceResponse())) {
      if (CollectionUtils.isNotEmpty(campaignPriceResponse.getItemInfoToPriceResponse())) {
        itemSkuToPriceResponseMap = campaignPriceResponse.getItemInfoToPriceResponse().stream().collect(Collectors
            .toMap(CampaignPriceSkuResponse::getItemSku, campaignPriceSkuResponse -> campaignPriceSkuResponse,
                (a, b) -> a));
        campaignPriceResponse.setItemSkuToPriceResponse(itemSkuToPriceResponseMap);
      }
    }
  }

  public static void addPickupPointDetailsInProfileResponse(ProfileResponse profileResponse,
      List<PickupPointOutboundResponse> pickupPointResponseList) {
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    for (PickupPointOutboundResponse pickupPointResponse : pickupPointResponseList) {
      PickupPointDTO pickupPointDTO = new PickupPointDTO();
      BeanUtils.copyProperties(pickupPointResponse, pickupPointDTO);
      pickupPointDTOList.add(pickupPointDTO);
    }
    profileResponse.setPickupPoints(pickupPointDTOList);
  }

  public static PickupPointWebResponse toPickupPointWebResponse(
      PickupPointOutboundResponse pickupPointResponse) {
    PickupPointWebResponse pickupPointWebResponse = new PickupPointWebResponse();
    BeanUtils.copyProperties(pickupPointResponse, pickupPointWebResponse);

    GeolocationDTOResponse geolocationDTOResponse = new GeolocationDTOResponse();
    BeanUtils.copyProperties(pickupPointResponse.getGeolocation(), geolocationDTOResponse);
    pickupPointWebResponse.setGeolocation(geolocationDTOResponse);

    return pickupPointWebResponse;
  }

  public static List<ItemDetailWebResponse> toItemDetailWebResponseFromV2Response(
      List<ItemBasicDetailV2Response> itemBasicDetailV2ResponseList,
      Map<String, List<ItemAttributeWebResponse>> itemCodeToItemAttributeValuesMap,
      Map<String, CategoryWebResponse> categoryWebResponseMap, String productDetailPageUrlPrefix) {
    return itemBasicDetailV2ResponseList.stream().map(
        itemBasicDetailV2Response -> ItemDetailWebResponse.builder().itemSku(itemBasicDetailV2Response.getItemSku())
            .itemName(itemBasicDetailV2Response.getGeneratedItemName())
            .merchantCode(itemBasicDetailV2Response.getMerchantCode())
            .productSku(itemBasicDetailV2Response.getProductSku()).itemCode(itemBasicDetailV2Response.getItemCode())
            .mainImageUrl(itemBasicDetailV2Response.getMainImageUrl()).itemAttributes(
                itemCodeToItemAttributeValuesMap.getOrDefault(itemBasicDetailV2Response.getItemCode(),
                    Collections.emptyList())).categoryCode(itemBasicDetailV2Response.getCategoryCode()).categoryName(
                categoryWebResponseMap.getOrDefault(itemBasicDetailV2Response.getCategoryCode(),
                    CategoryWebResponse.builder().name(StringUtils.EMPTY).build()).getName()).pdpUrl(
                RequestHelper.toProductDetailPage(itemBasicDetailV2Response.getProductSku(),
                    productDetailPageUrlPrefix)).sharedProduct(itemBasicDetailV2Response.isSharedProduct())
            .bundleRecipeList(tobundleChildRecipeWebResponseList(itemBasicDetailV2Response)).build()).collect(toList());
  }

  private static List<BundleChildRecipeWebResponse> tobundleChildRecipeWebResponseList(
      ItemBasicDetailV2Response itemBasicDetailV2Response) {
    return itemBasicDetailV2Response.getBundleRecipeList().stream().map(ResponseHelper::tobundleChildRecipeWebResponse)
        .collect(toList());
  }

  private static BundleChildRecipeWebResponse tobundleChildRecipeWebResponse(
      BundleRecipeV2Response bundleRecipeV2Response) {
    BundleChildRecipeWebResponse bundleChildRecipeWebResponse = new BundleChildRecipeWebResponse();
    bundleChildRecipeWebResponse.setQuantity(bundleRecipeV2Response.getQuantity());
    bundleChildRecipeWebResponse.setProductStatus(bundleRecipeV2Response.getProductStatus());
    bundleChildRecipeWebResponse.setItemName(bundleRecipeV2Response.getGeneratedItemName());
    bundleChildRecipeWebResponse.setItemSku(bundleRecipeV2Response.getItemSku());
    bundleChildRecipeWebResponse.setItemCode(bundleRecipeV2Response.getItemCode());
    bundleChildRecipeWebResponse.setMainImageUrl(bundleRecipeV2Response.getMainImageUrl());
    bundleChildRecipeWebResponse.setMerchantCode(bundleRecipeV2Response.getMerchantCode());
    bundleChildRecipeWebResponse.setProductSku(bundleChildRecipeWebResponse.getProductSku());
    bundleChildRecipeWebResponse.setSharedProduct(bundleChildRecipeWebResponse.isSharedProduct());
    return bundleChildRecipeWebResponse;
  }

  public static List<ItemAttributeWebResponse> toItemAttributeValueWebResponses(
      List<ProductItemAttributeValueResponse> itemAttributeValueResponses) {
    return itemAttributeValueResponses.stream().map(ResponseHelper::toItemAttributeWebResponse).collect(toList());
  }

  public static ItemAttributeWebResponse toItemAttributeWebResponse(
      ProductItemAttributeValueResponse productItemAttributeValueResponse) {
    ItemAttributeWebResponse itemAttributeWebResponse = new ItemAttributeWebResponse();
    itemAttributeWebResponse.setAttributeCode(
        productItemAttributeValueResponse.getAttributeResponse().getAttributeCode());
    itemAttributeWebResponse.setAttributeType(
        productItemAttributeValueResponse.getAttributeResponse().getAttributeType());
    itemAttributeWebResponse.setAttributeName(productItemAttributeValueResponse.getAttributeResponse().getName());
    itemAttributeWebResponse.setAttributeNameEnglish(
        productItemAttributeValueResponse.getAttributeResponse().getNameEnglish());
    itemAttributeWebResponse.setVariantCreation(
        productItemAttributeValueResponse.getAttributeResponse().isVariantCreation());
    itemAttributeWebResponse.setValue(productItemAttributeValueResponse.getValue());
    return itemAttributeWebResponse;
  }


  private static String getErrorCodeForFbbResponse(Map<String, List<String>> errorMap) {
    return errorMap.values().stream().findFirst().orElseGet(ArrayList::new).stream().findFirst()
      .orElse(ErrorMessages.ERR_INVALID_RESPONSE);
  }

  public static boolean validateResponseForFbbConsignmentForm(
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>> consignmentFormsDetail) {
    if (Objects.isNull(consignmentFormsDetail)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
      if (MapUtils.isNotEmpty(consignmentFormsDetail.getErrors())) {
        throw new ClientException(getErrorCodeForFbbResponse(consignmentFormsDetail.getErrors()));
      } else if (Objects.isNull(consignmentFormsDetail.getData())) {
        throw new ClientException(ErrorCategory.VALIDATION.getMessage());
      }
    return true;
  }

  public static List<ConsignmentDetailWebResponse> toConsignmentDetailWebResponse(
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>> consignmentFormsDetail) {
    List<ConsignmentDetailWebResponse> consignmentDetailWebResponses = new ArrayList<>();
    consignmentFormsDetail.getData().forEach(consignmentStatusResponse -> {
      ConsignmentDetailWebResponse consignmentDetailWebResponse =
        new ConsignmentDetailWebResponse();
      consignmentDetailWebResponse.setConsignmentCode(
        consignmentStatusResponse.getDocumentNumber());
      consignmentDetailWebResponse.setStatus(
        ConsignmentStatusWeb.convertToConsignmentStatusWeb(consignmentStatusResponse.getStatus()));
      consignmentDetailWebResponse.setCreatedDate(consignmentStatusResponse.getCreatedDate());
      consignmentDetailWebResponses.add(consignmentDetailWebResponse);
    });
    // Compares the Status first and later compares the created Date
    consignmentDetailWebResponses.sort(Comparator.comparingInt(
        (ConsignmentDetailWebResponse response) -> ConsignmentStatusWeb.getStatusPriorityForLisitng(
          response.getStatus()))
      .thenComparing(ConsignmentDetailWebResponse::getCreatedDate, Comparator.reverseOrder()));
    return consignmentDetailWebResponses;
  }

  public static boolean validateResponseForConsignmentCount(
    com.blibli.oss.common.response.Response<List<CountConsignmentFormsByItemSkuResponse>> inProgressConsignmentForms) {
    return Optional.ofNullable(inProgressConsignmentForms)
      .filter(response -> Objects.isNull(response.getErrors()))
      .map(com.blibli.oss.common.response.Response::getData)
      .map(CollectionUtils::isNotEmpty)
      .orElse(false);
  }

  public static List<InventoryWarehouseStockWebResponse> getWarehouseStockDetails(
      List<WarehouseInventoryDetailResponseDTO> responseDTOList, List<String> itemSkus, String warehouseCode) {
    List<InventoryWarehouseStockWebResponse> warehouseStockWebResponses = new ArrayList<>();
    Map<String, InventoryWarehouseStockWebResponse> itemSkuToWarehouseStockWebResponseMap = responseDTOList.stream()
        .map(response -> new InventoryWarehouseStockWebResponse(response.getWebItemSku(),
            Optional.ofNullable(response.getWarehouseInventoryStockInfos()).orElse(new ArrayList<>()).stream()
                .filter(Objects::nonNull).findFirst().map(WarehouseInventoryStockInfoDTO::getAvailableStock).orElse(0),
            Optional.ofNullable(response.getWarehouseInventoryStockInfos()).orElse(new ArrayList<>()).stream()
                .filter(Objects::nonNull).findFirst().map(WarehouseInventoryStockInfoDTO::getWarehouseCode)
                .orElse(StringUtils.EMPTY)))
        .collect(Collectors.toMap(InventoryWarehouseStockWebResponse::getItemSku, Function.identity()));

    for (String itemSku : itemSkus) {
      warehouseStockWebResponses.add(itemSkuToWarehouseStockWebResponseMap.getOrDefault(itemSku,
          new InventoryWarehouseStockWebResponse(itemSku, 0, warehouseCode)));
    }

    return warehouseStockWebResponses;
  }

  public static Page<ItemCodeBasicDetailWebResponse> toBasicItemDetailsWebResponse(
    GdnRestListResponse<ItemCodeBasicDetailResponse> basicItemDetailsByItemCodes,
    String productDetailPageUrlPrefix, int page,
    int size) {
    List<ItemCodeBasicDetailResponse> itemCodeBaicDetailResponseList =
      basicItemDetailsByItemCodes.getContent();
    List<ItemCodeBasicDetailWebResponse> itemCodeBasicDetailWebResponses = new ArrayList<>();
    for (ItemCodeBasicDetailResponse basicDetailResponses : itemCodeBaicDetailResponseList) {
      ItemCodeBasicDetailWebResponse itemCodeBasicDetailWebResponse =
        new ItemCodeBasicDetailWebResponse();
      BeanUtils.copyProperties(basicDetailResponses, itemCodeBasicDetailWebResponse);
      itemCodeBasicDetailWebResponse.setPdpUrl(
        RequestHelper.toProductDetailPage(basicDetailResponses.getProductSku(),
          productDetailPageUrlPrefix));
      itemCodeBasicDetailWebResponse.setMainImageUrl(basicDetailResponses.getMainImageUrl());
      itemCodeBasicDetailWebResponses.add(itemCodeBasicDetailWebResponse);
    }
    return new PageImpl<>(itemCodeBasicDetailWebResponses, PageRequest.of(page, size),
      basicItemDetailsByItemCodes.getPageMetaData().getTotalRecords());
  }

  public static PickupPointStockAndInBoundStatusWebResponse toL3AndPickupPointStockAvailabilityResponse(
    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> stockAvailabilityByL3AndPickupPoint) {
    L3AndPickupPointStockAvailabilityResponse l3AndPickupPointStockAvailabilityResponse =
      stockAvailabilityByL3AndPickupPoint.getValue();
    PickupPointStockAndInBoundStatusWebResponse webResponse =
      new PickupPointStockAndInBoundStatusWebResponse();
    webResponse.setProductSku(l3AndPickupPointStockAvailabilityResponse.getWebProductSku());
    webResponse.setPickupPointCode(l3AndPickupPointStockAvailabilityResponse.getPickupPointCode());
    webResponse.setWarehouseStockAvailable(
      l3AndPickupPointStockAvailabilityResponse.isWarehouseAvailable());
    return webResponse;
  }

  public static VideoSignedUrlResponse toVideoSignedUrlResponse(VideoSignedUrlResponse data) {
    VideoSignedUrlResponse videoSignedUrlResponse = new VideoSignedUrlResponse();
    BeanUtils.copyProperties(data, videoSignedUrlResponse);
    return videoSignedUrlResponse;
  }

  public static VideoDetailWebResponse toVideoDetailWebResponse(
      List<ReelProductDetailWebResponse> reelProductDetailWebResponse,
      ReelsListingResponse reelsListingResponse) {
    VideoDetailWebResponse response = new VideoDetailWebResponse();
    BeanUtils.copyProperties(reelsListingResponse, response);
    response.setReelProductDetailWebResponses(reelProductDetailWebResponse);
    List<String> activeProductSkus =
        reelProductDetailWebResponse.stream().map(ReelProductDetailWebResponse::getProductSku)
            .collect(Collectors.toList());
    List<String> inActiveProducts = new ArrayList<>(reelsListingResponse.getProductSkuList());
    inActiveProducts.removeAll(activeProductSkus);
    response.setInActiveProductSkuList(inActiveProducts);
    return response;
  }

  public static ValidOmniChannelSkuWebResponse mapToValidOmniChannelSkuWebResponse(
      OmniChannelMapAndSkuResponse response) {
    if (Objects.isNull(response)) {
      return null;
    }
    ValidOmniChannelSkuWebResponse webResponse = new ValidOmniChannelSkuWebResponse();
    webResponse.setExistingOmniChannelSkusAndProductDetailsMap(response.getExistingSellerSkusAndProductDetailsMap());
    return webResponse;
  }
}

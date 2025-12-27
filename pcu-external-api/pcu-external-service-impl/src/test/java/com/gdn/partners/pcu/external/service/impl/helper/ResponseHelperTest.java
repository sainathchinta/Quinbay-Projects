package com.gdn.partners.pcu.external.service.impl.helper;

import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.PickupPointResponse;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
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
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.response.B2BResponse;
import com.gda.mta.product.dto.response.BuyableScheduleResponse;
import com.gda.mta.product.dto.response.DiscoverableScheduleResponse;
import com.gda.mta.product.dto.response.DistributionInfo;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ImageResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.fbb.core.constant.ProductConsignmentStatus;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InactiveSummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ForceReviewImageViolationResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3AttributeWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;
import com.gdn.partners.pcu.external.client.helper.HitsResponse;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.FlagDTO;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.EditProductException;
import com.gdn.partners.pcu.external.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.external.service.impl.exception.ProductListingGenericException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.model.enums.ProductSyncWebStatus;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentStatusWeb;
import com.gdn.partners.pcu.external.web.model.response.DistinctPickUpPoint;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemCodeBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import com.gdn.partners.pcu.external.web.model.response.LogAuditTrailUpdatedProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3AttributeResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.SellerLogisticsProductResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
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
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.dto.ProductCampaignAvailabilityInfoDto;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleItemResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.GeoLocationResponse;
import com.gdn.x.product.rest.web.model.response.IgnoreAttributeSet;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.RuleConfigResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
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
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import org.junit.jupiter.api.Assertions;
import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static com.gdn.mta.product.enums.ApiErrorCode.PREORDER_DATE_BEFORE_CURRENT_DATE;
import static com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_IS_TAKEN_DOWN;
import static com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_NAME_IS_EMPTY;
import static com.gdn.mta.product.enums.ApiErrorCode.SHIPPING_WEIGHT_EXCEEDED;
import static com.gdn.partners.pcu.external.model.Constants.SUCCESS;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;

public class ResponseHelperTest {

  private static final String ERROR_MESSAGE = "ERROR MESSAGE";
  private static final String ERROR_CODE = "ERROR_CODE";
  private static final String BRAND_NOT_FOUND = "DATA_NOT_FOUND";
  private static final String BRAND_NOT_FOUND_ERROR_MSG = "Brand yang Anda pilih ditolak. Pilih brand lainnya untuk melanjutkan.";
  private static final String VALIDATION = "VALIDATION";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final double OFFER_PRICE = 1.5;
  private static final BaseResponse RESPONSE = new BaseResponse();
  private static final ProductCampaignAvailabilityResponse RESPONSE1 = new ProductCampaignAvailabilityResponse();
  private static final String PRODUCT_ID = "product_id";
  private static final String NAME = "NAME";
  private static final String NAME_ENGLISH = "NAME_ENGLISH";
  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String ID = "ID";
  private static final String CODE = "CODE";
  private static final String BRAND = "brand";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String BRAND_APPROVAL_STATUS = "brandStatus";
  private static final String BRAND_PREDEFINED_CODE = "brandPredefinedCode";
  private static final String CREATED_BY = "createdBy";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String PICK_UP_POINT_NAME = "pickupPoint1";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_1 = "pickupPointCode1";
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String GDN_ITEM_SKU1 = "gdn-item-sku-1";
  private static final String GDN_ITEM_SKU2 = "gdn-item-sku-2";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_NAME_2 = "PRODUCT_NAME 2";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String UPC_CODE = "UPC_CODE";
  private static final Integer PRODUCT_TYPE = 1;
  private static final String IMAGE_URL = "IMAGE_URL";
  private static final String ITEM_NAME = "ITEM_NAME";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String CHANGED_BY = "changedBy";
  private static final double DISCOUNT = 5.0;
  private static final String ITEM_NAME_1 = "ITEM_NAME_1";
  private static final String DESCRIPTION = "DESCRIPTION";
  private static final String PRODUCT_TITLE = "PRODUCT_TITLE";
  private static final String REASON = "REASON";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String CATEGORY_HIERARCHY = "CATEGORY_HIERARCHY";
  private static final String CATEGORY_HIERARCHY_ENGLISH = "CATEGORY_HIERARCHY_ENGLISH";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String URL = "URL";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String DATA_LINK = "DATA_LINK";
  private static final Date BANNED_DATE = new Date();
  private static final String PROMO_ADJUSTMENT_CODE1 = "promoAdjustmentCode1";
  private static final String PROMO_ADJUSTMENT_CODE2 = "promoAdjustmentCode2";
  private static final String PROMO_ADJUSTMENT_NAME1 = "promoAdjustmentName1";
  private static final String PROMO_ADJUSTMENT_NAME2 = "promoAdjustmentName2";
  private static final String DETAIL_URL = "/discount/PROMO-101057";
  private static final long PRODUCT_COUNT_1 = 16;
  private static final long PRODUCT_COUNT_2 = 8;
  private static final long PRODUCT_COUNT_3 = 4;
  private static final long PRODUCT_COUNT_4 = 2;
  private static final long PRODUCT_COUNT_5 = 1;
  private static final long PRODUCT_COUNT_6 = 1;

  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE_3 = "categoryCode3";
  private static final String CATEGORY_CODE_4 = "categoryCode4";
  private static final String CATEGORY_CODE_5 = "categoryCode5";
  private static final String CATEGORY_CODE_6 = "categoryCode6";
  private static final String CATEGORY_CODE_7 = "categoryCode7";
  private static final String CATEGORY_CODE_8 = "categoryCode8";
  private static final String CATEGORY_CODE_9 = "categoryCode9";
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String CATEGORY_ID_2 = "categoryId2";
  private static final String CATEGORY_ID_3 = "categoryId3";
  private static final String CATEGORY_ID_4 = "categoryId4";
  private static final String CATEGORY_ID_5 = "categoryId5";
  private static final String CATEGORY_ID_6 = "categoryId6";
  private static final String CATEGORY_ID_7 = "categoryId7";
  private static final String CATEGORY_ID_8 = "categoryId8";
  private static final String CATEGORY_ID_9 = "categoryId9";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String CATEGORY_NAME_2 = "categoryName2";
  private static final String CATEGORY_NAME_3 = "categoryName3";
  private static final String CATEGORY_NAME_4 = "categoryName4";
  private static final String CATEGORY_NAME_5 = "categoryName5";
  private static final String CATEGORY_NAME_6 = "categoryName6";
  private static final String CATEGORY_NAME_7 = "categoryName7";
  private static final String CATEGORY_NAME_8 = "categoryName8";
  private static final String CATEGORY_NAME_9 = "categoryName9";
  private static final String CATEGORY_NAME_ENGLISH_1 = "categoryNameEnglish1";
  private static final String CATEGORY_NAME_ENGLISH_2 = "categoryNameEnglish2";
  private static final String CATEGORY_NAME_ENGLISH_3 = "categoryNameEnglish3";
  private static final String CATEGORY_NAME_ENGLISH_4 = "categoryNameEnglish4";
  private static final String COMMA_DELIMITER = ", ";
  private static final String DEFAULT_PROCESS_CODE = "defaultBulkProcessCode";
  private static final Long VERSION = Long.valueOf(2);
  private static final String FILE_NAME = "FILE-NAME";
  private static final String UPDATED_BY = "updatedBy";
  private static final Double LONGITUDE = 71.1;
  private static final Double LATITUDE = 62.2;
  private static final String ACTIVITY = "ACTIVITY";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String REVIEW_TYPE = "Pre-live";
  private static final String LOGISTIC_OPTION_NAME = "logisticOption";
  private static final String IMAGE_VIOLATION = "medicines";

  private static  final String ITEM_NOT_FOUND = "item not found";

  private static final String INVALID_STATE = "Invalid state";
  private static final String COMPLEMENTARY_COMBO_PROMO = "Complementary Combo promo";
  private static final String SALE_CHANNEL = "BLIBLI";
  private static final Date TEN_DAYS_AGO = new Date(System.currentTimeMillis() - (10 * 24 * 60 * 60 * 1000));
  private static final Date TEN_DAYS_AFTER = new Date(System.currentTimeMillis() + (10 * 24 * 60 * 60 * 1000));
  private static final String baseUrlForPdpWithL5 = "https://www.blibli.com/p/p/productSku";

  private ProductDetailResponse productDetailResponse;
  private ProductLevel3Response productLevel3Response;
  private ProductLevel3DetailResponse productLevel3DetailResponse;
  private List<ProductLevel3ImageResponse> productLevel3ImageResponseList;
  private ProductLevel3ImageResponse productLevel3ImageResponse;
  private List<ProductItemLevel3Response> productItemLevel3ResponseList;
  private ProductItemLevel3Response productItemLevel3Response;
  private List<com.gda.mta.product.dto.ProductLevel3AttributeResponse> productLevel3AttributeResponseList;
  private List<ProductLevel3AttributeResponse> productLevel3AttributeResponseListExternal;
  private com.gda.mta.product.dto.ProductLevel3AttributeResponse productLevel3AttributeResponse;
  private ProductLevel3AttributeResponse productLevel3AttributeResponsePBP;
  private ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse;
  private List<ProductLevel3ViewConfigResponse> productLevel3ViewConfigResponseList;
  private ProductLevel3PriceResponse productLevel3PriceResponse;
  private List<ProductLevel3PriceResponse> productLevel3PriceResponseList;
  private ActiveProductResponse activeProductResponse;
  private List<ProductAttributeResponse> productAttributeResponses;
  private ProductAttributeResponse productAttributeResponse;
  private Set<ProductItemResponse> productItemResponses;
  private ProductItemResponse productItemResponse;
  private List<Image> images;
  private Image image;
  private List<String> saleChannel = new ArrayList<>();
  private List<ProductItemAttributeValueResponse> attributeValueResponses;
  private ProductItemAttributeValueResponse attributeValueResponse;
  private List<ProductCategoryResponse> productCategoryResponses;
  private ProductCategoryResponse productCategoryResponse;
  private CategoryResponse categoryResponse;
  private ProfileResponse profileResponse;
  private ProfileResponse profileResponse2;
  private PickupPointDTO pickupPointDTO;
  private PickupPointUpdateResponse pickupPointUpdateResponse;
  private ProductItemDetailResponse productItemDetailResponse;
  private List<ProductItemDetailResponse> productItemDetailResponseList;
  private ProductResponse productResponse;
  private CompanyDTO companyDTO;
  private ProductLevel3WipDetailResponse productLevel3WipDetailResponse;
  private SuspensionItemResponse suspensionItemResponse;
  private List<SuspensionItemResponse>  suspensionItemResponseList;
  private ProductLevel3SummaryCountResponse productLevel3SummaryCountResponse;
  private CountProductLevel3WipResponse countProductLevel3WipResponse;
  private CountProductLevel3InactiveResponse countProductLevel3InactiveResponse;
  private RejectedSkuProductResponse rejectedSkuProductResponse;
  private ItemSummaryResponse itemSummaryResponse;
  private ItemSummaryResponse itemSummaryResponse1;
  private List<ItemSummaryResponse> itemSummaryResponseList = new ArrayList<>();
  private List<ProductNameSuggestionResponse> productNameSuggestionResponses = new ArrayList<>();
  private ProductLevel3WipResponse productLevel3WipResponse;
  private PromoSkuDetailResponse promoItemDetailResponse;
  private CategoryDTO categoryDTO;
  private List<CategoryHierarchyResponse> categoryHierarchyProductCountResponseList;
  MinWholesaleDiscountResponse minWholesaleDiscountResponse = new MinWholesaleDiscountResponse();
  MinWholesaleDiscountResponse minWholesaleDiscountResponse1 = new MinWholesaleDiscountResponse();
  private List<MinWholesaleDiscountResponse> minWholesaleDiscountResponses = new ArrayList<>();
  private ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
  private BulkProcessNotesResponse bulkProcessNotesResponse = new BulkProcessNotesResponse();
  private Map<Integer, Double> itemQuantityDiscountMap = new HashMap<>();
  private Map<Integer, Double> itemQuantityDiscountMap1 = new HashMap<>();
  private Map<Integer, Double> categoryQuantityDiscountMap = new HashMap<>();
  private BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse;
  private GdnRestSimpleResponse<UnifiedBulkDownloadResponse> clientResponse;
  private UnifiedBulkDownloadResponse unifiedBulkDownloadResponse;
  private ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse;
  private List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponses;
  private CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
  private PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse =
      new PredefinedAttributeValueWebResponse();
  private VariantsErrorListResponse variantsErrorListResponse;
  private PreOrderResponse preOrderResponse;
  private EditProductV2Response editProductResponse;
  private ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
  private ProductL3DetailsResponse productL3DetailsResponse;
  private com.gda.mta.product.dto.response.ProductL3DetailsResponse productL3DetailsResponse1;
  private PickupPointDetailResponse pickupPointDetailResponse;
  private HistoryUpdateResponse historyUpdateResponse;
  private WholesalePriceSkuResponse wholesalePriceSkuResponse;
  private ItemPickupPointListingL3Response itemPickupPointListingL3Response;
  private ProductLevel3DetailsV2Response productLevel3DetailsV2Response;
  private BuyableScheduleResponse buyableScheduleResponse;
  private DiscoverableScheduleResponse discoverableScheduleResponse;
  private String PRODUCT_DETAIL_PAGE_URL_PREFIX = "PDP_URL";

  @BeforeEach
  public void setUp() {
    productLevel3Response = new ProductLevel3Response();
    productLevel3Response.setProductCode(PRODUCT_CODE);
    productLevel3Response.setProductType(PRODUCT_TYPE);
    productLevel3Response.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3Response.setBrand(BRAND);
    productLevel3Response.setCategoryCode(CATEGORY_CODE);
    productLevel3Response.setDescription(DESCRIPTION);
    productLevel3Response.setCategoryId(CATEGORY_ID);
    productLevel3Response.setCategoryName(CATEGORY_NAME);
    productLevel3Response.setProductName(PRODUCT_NAME);
    productLevel3Response.setCategoryHierarchy(CATEGORY_HIERARCHY);
    productLevel3Response.setInstallationRequired(true);
    productLevel3Response.setSynchronize(true);
    productLevel3Response.setUrl(URL);
    productLevel3Response.setForceReview(true);
    productLevel3Response.setProductScore(new ProductScoreResponse(15, 10, 15, 5, 20, 9, 2, 18, 5, 1, 100));
    productLevel3ImageResponse = new ProductLevel3ImageResponse();
    productLevel3ImageResponse.setMainImage(true);
    productLevel3ImageResponseList = Arrays.asList(productLevel3ImageResponse);
    productItemLevel3Response = new ProductItemLevel3Response();
    productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponse.setBuyable(true);
    productLevel3ViewConfigResponse.setId(ID);
    productLevel3ViewConfigResponseList = Arrays.asList(productLevel3ViewConfigResponse);
    productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3PriceResponse.setId(ID);
    productLevel3PriceResponse.setPrice(10000.0);
    productLevel3PriceResponse.setSalePrice(9000.0);
    productItemLevel3LogisticResponse = ProductItemLevel3LogisticResponse.builder().build();
    productItemLevel3LogisticResponses = Arrays.asList(productItemLevel3LogisticResponse);
    productLevel3PriceResponseList = Arrays.asList(productLevel3PriceResponse);
    productItemLevel3Response.setImages(productLevel3ImageResponseList);
    productItemLevel3Response.setPrices(productLevel3PriceResponseList);
    productItemLevel3Response.setViewConfigs(productLevel3ViewConfigResponseList);
    productItemLevel3Response.setItemSku(ITEM_SKU);
    productItemLevel3Response.setProductItemLevel3LogisticResponse(productItemLevel3LogisticResponses);
    productItemLevel3ResponseList = Arrays.asList(productItemLevel3Response);
    productLevel3AttributeResponse = new com.gda.mta.product.dto.ProductLevel3AttributeResponse();
    productLevel3AttributeResponse.setAttributeCode(ATTRIBUTE_ID);
    productLevel3AttributeResponse.setVariantCreation(true);
    productLevel3AttributeResponseList = Arrays.asList(productLevel3AttributeResponse);
    productLevel3Response.setImages(productLevel3ImageResponseList);
    productLevel3Response.setAttributes(productLevel3AttributeResponseList);
    productLevel3Response.setItems(productItemLevel3ResponseList);

    productLevel3DetailResponse = new ProductLevel3DetailResponse();
    productLevel3DetailResponse.setImages(productLevel3ImageResponseList);
    productLevel3DetailResponse.setAttributes(productLevel3AttributeResponseList);
    productLevel3DetailResponse.setProductCode(PRODUCT_CODE);
    productLevel3DetailResponse.setProductType(PRODUCT_TYPE);
    productLevel3DetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3DetailResponse.setBrand(BRAND);
    productLevel3DetailResponse.setCategoryCode(CATEGORY_CODE);
    productLevel3DetailResponse.setDescription(DESCRIPTION);
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    productLevel3DetailResponse.setCategoryName(CATEGORY_NAME);
    productLevel3DetailResponse.setProductName(PRODUCT_NAME);
    productLevel3DetailResponse.setCategoryHierarchy(CATEGORY_HIERARCHY);
    productLevel3DetailResponse.setInstallationRequired(true);
    productLevel3DetailResponse.setSynchronize(true);
    productLevel3DetailResponse.setUrl(URL);
    productLevel3DetailResponse.setForceReview(true);
    productLevel3DetailResponse.setProductScore(new ProductScoreResponse(15, 10, 15, 5, 20, 9, 2, 18, 5, 1, 100));
    productLevel3DetailResponse.setDefaultItemSku(ITEM_SKU);
    productLevel3DetailResponse.setProductLevel3Logistics(productItemLevel3LogisticResponses);
    productItemLevel3ResponseList = Arrays.asList(productItemLevel3Response);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    productDetailResponse.setName(NAME);
    productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId(ATTRIBUTE_ID);
    attributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ID);
    attributeResponse.setName(NAME);
    attributeResponse.setVariantCreation(true);
    attributeValueResponse.setAttributeResponse(attributeResponse);
    attributeValueResponse.setId(ID);
    attributeValueResponses = Arrays.asList(attributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setId(ID);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.MULTIPLE);
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setId(ID);
    allowedAttributeValueResponse.setAllowedAttributeCode(CODE);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(CODE);
    predefinedAllowedAttributeValueResponse.setId(ID);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    List<ProductAttributeValueResponse> valueResponses = Arrays.asList(productAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(valueResponses);
    productAttributeResponses = Arrays.asList(productAttributeResponse);
    productDetailResponse.setProductAttributeResponses(productAttributeResponses);
    productItemResponse = new ProductItemResponse();
    productItemResponse.setId(ID);
    productItemResponse.setSkuCode(CODE);
    image = new Image();
    image.setId(ID);
    images = Arrays.asList(image);
    productItemResponse.setImages(images);
    productItemResponse.setGeneratedItemName(NAME);
    productItemResponse.setProductItemAttributeValueResponses(attributeValueResponses);
    productItemResponses = new HashSet<>();
    productItemResponses.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productItemResponses);
    productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId(ID);
    categoryResponse = new CategoryResponse();
    categoryResponse.setName(NAME);
    categoryResponse.setNameEnglish(NAME_ENGLISH);
    categoryResponse.setCategoryCode(CODE);
    categoryResponse.setId(CATEGORY_ID);
    productCategoryResponse.setCategory(categoryResponse);
    productCategoryResponses = Arrays.asList(productCategoryResponse);
    productDetailResponse.setProductCategoryResponses(productCategoryResponses);
    productDetailResponse.setCategories(Arrays.asList(CODE));
    productDetailResponse.setImages(images);
    pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setActivated(true);
    pickupPointDTO.setName(PICK_UP_POINT_NAME);
    pickupPointDTO.setGeolocation(new GeolocationDTO());
    profileResponse = new ProfileResponse();
    profileResponse2 = new ProfileResponse();
    profileResponse.setId(ID);
    profileResponse.setActivated(true);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setPickupPoints(Arrays.asList(pickupPointDTO));
    productItemDetailResponse = new ProductItemDetailResponse();
    productResponse = new ProductResponse();
    productResponse.setId(ID);
    productResponse.setProductCode(CODE);
    productItemDetailResponse.setId(ID);
    productItemDetailResponse.setImages(images);
    productItemDetailResponse.setProductItemAttributeValueResponses(attributeValueResponses);
    productItemDetailResponse.setProductResponse(productResponse);
    productItemDetailResponseList = Arrays.asList(productItemDetailResponse);

    companyDTO = new CompanyDTO();
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    companyDTO.setCncActivated(true);
    companyDTO.setSalesChannel(saleChannel);
    profileResponse.setCompany(companyDTO);

    productLevel3Logistics.setRequiredLongLat(true);
    productLevel3Logistics.setSelected(true);
    productLevel3Logistics.setLogisticProductName(LOGISTIC_OPTION_NAME);
    productLevel3WipDetailResponse = new ProductLevel3WipDetailResponse();
    productLevel3WipDetailResponse.setProductSku(GDN_PRODUCT_SKU);
    ProductLevel3ItemWipResponse productLevel3ItemWipResponse1 = new ProductLevel3ItemWipResponse();
    productLevel3ItemWipResponse1.setGdnSku(GDN_ITEM_SKU1);
    ProductLevel3ItemWipResponse productLevel3ItemWipResponse2 = new ProductLevel3ItemWipResponse();
    productLevel3ItemWipResponse2.setGdnSku(GDN_ITEM_SKU2);
    ProductLevel3AttributeWipResponse productLevel3AttributeWipResponse = new ProductLevel3AttributeWipResponse();
    productLevel3AttributeWipResponse.setAttributeId(ATTRIBUTE_ID);
    List<ProductLevel3ItemWipResponse> productLevel3ItemWipResponses =
        Arrays.asList(productLevel3ItemWipResponse1, productLevel3ItemWipResponse2);
    List<ProductLevel3AttributeWipResponse> productLevel3AttributeWipResponses =
        Arrays.asList(productLevel3AttributeWipResponse);
    productLevel3WipDetailResponse.setItems(productLevel3ItemWipResponses);
    productLevel3WipDetailResponse.setAttributes(productLevel3AttributeWipResponses);
    productLevel3WipDetailResponse.setProductLevel3Logistics(Arrays.asList(productLevel3Logistics));

    activeProductResponse = new ActiveProductResponse();
    activeProductResponse.setProductCode(PRODUCT_CODE);
    activeProductResponse.setProductName(PRODUCT_NAME);
    activeProductResponse.setProductSku(PRODUCT_SKU);
    activeProductResponse.setImageUrl(IMAGE_URL);

    suspensionItemResponse = new SuspensionItemResponse();
    suspensionItemResponse.setCategoryName(CATEGORY_NAME);
    suspensionItemResponse.setItemName(ITEM_NAME);
    suspensionItemResponse.setItemSku(ITEM_SKU);
    suspensionItemResponse.setProductCode(PRODUCT_CODE);
    suspensionItemResponse.setProductName(PRODUCT_NAME);
    suspensionItemResponse.setProductSku(PRODUCT_SKU);
    suspensionItemResponse.setReason(REASON);
    suspensionItemResponse.setBannedDate(BANNED_DATE);
    suspensionItemResponse.setProductDetailPageLink(DATA_LINK);

    suspensionItemResponseList = new ArrayList<>();
    suspensionItemResponseList.add(suspensionItemResponse);

    productLevel3SummaryCountResponse = new ProductLevel3SummaryCountResponse();
    Map<ProductLevel3InventoryCriteria, Long> stockConditionCounts = new HashMap<>();
    stockConditionCounts.put(ProductLevel3InventoryCriteria.AVAILABLE, 40L);
    stockConditionCounts.put(ProductLevel3InventoryCriteria.STOCK_ALERT, 30L);
    stockConditionCounts.put(ProductLevel3InventoryCriteria.OOS, 20L);
    productLevel3SummaryCountResponse.setStockConditionCounts(stockConditionCounts);
    productLevel3SummaryCountResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    countProductLevel3WipResponse = new CountProductLevel3WipResponse();
    Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias = new HashMap();
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.NEED_CORRECTION, 40L);
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 50L);
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.FAILED, 60L);
    countProductLevel3WipResponse.setTotalItemsByCriterias(totalItemsByCriterias);
    countProductLevel3WipResponse.setTotalItems(150L);

    countProductLevel3InactiveResponse = new CountProductLevel3InactiveResponse();
    Map<ProductLevel3InactiveSummaryCriteria, Long> totalInactiveItemsByCriteria = new HashMap();
    totalInactiveItemsByCriteria.put(ProductLevel3InactiveSummaryCriteria.SUSPENDED, 40L);
    totalInactiveItemsByCriteria.put(ProductLevel3InactiveSummaryCriteria.ARCHIVED, 50L);
    totalInactiveItemsByCriteria.put(ProductLevel3InactiveSummaryCriteria.REJECTED, 60L);
    countProductLevel3InactiveResponse.setTotalItemsByCriterias(totalInactiveItemsByCriteria);
    rejectedSkuProductResponse = new RejectedSkuProductResponse();
    rejectedSkuProductResponse.setInitiator(CREATED_BY);
    rejectedSkuProductResponse.setBrand(BRAND);
    rejectedSkuProductResponse.setRejectedDate(BANNED_DATE);

    itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(GDN_ITEM_SKU1);
    itemSummaryResponse.setGeneratedItemName(ITEM_NAME);
    itemSummaryResponse1 = new ItemSummaryResponse();
    itemSummaryResponse1.setItemSku(GDN_ITEM_SKU2);
    itemSummaryResponse1.setGeneratedItemName(ITEM_NAME_1);
    itemSummaryResponseList.add(itemSummaryResponse);
    itemSummaryResponseList.add(itemSummaryResponse1);

    productNameSuggestionResponses.add(new ProductNameSuggestionResponse(PRODUCT_NAME, PRODUCT_SKU));
    productNameSuggestionResponses.add(new ProductNameSuggestionResponse(PRODUCT_NAME_2, PRODUCT_CODE));
    productLevel3WipResponse = new ProductLevel3WipResponse();
    productLevel3WipResponse.setBrandName(BRAND);
    productLevel3WipResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3WipResponse.setCategoryName(CATEGORY_NAME);
    productLevel3WipResponse.setNotes(REASON);
    productLevel3WipResponse.setCreatedBy(CREATED_BY);
    productLevel3WipResponse.setProductName(ITEM_NAME);
    productLevel3WipResponse.setProductMainImage(IMAGE_URL);
    productLevel3WipResponse.setForceReview(true);
    productLevel3WipResponse.setProductCode(PRODUCT_CODE);
    productLevel3WipResponse.setActiveImage(true);
    productLevel3WipResponse.setForceReviewImageViolations(Arrays.asList(new ForceReviewImageViolationResponse(IMAGE_VIOLATION, IMAGE_VIOLATION)));

    promoItemDetailResponse = new PromoSkuDetailResponse();
    PromoSkuResponse activePromoSkuDetail = new PromoSkuResponse();
    PromoAdjustmentResponse promoAdjustment1 = new PromoAdjustmentResponse();
    promoAdjustment1.setPromoAdjustmentName(PROMO_ADJUSTMENT_NAME1);
    promoAdjustment1.setPromoAdjustmentCode(PROMO_ADJUSTMENT_CODE1);
    activePromoSkuDetail.setPromoAdjustment(promoAdjustment1);
    activePromoSkuDetail.setDetailUrl(DETAIL_URL);
    promoItemDetailResponse.setActivePromoSkuDetail(activePromoSkuDetail);
    PromoSkuResponse pendingPromoSkuDetail = new PromoSkuResponse();
    PromoAdjustmentResponse promoAdjustment2 = new PromoAdjustmentResponse();
    promoAdjustment2.setPromoAdjustmentName(PROMO_ADJUSTMENT_NAME2);
    promoAdjustment2.setPromoAdjustmentCode(PROMO_ADJUSTMENT_CODE2);
    pendingPromoSkuDetail.setPromoAdjustment(promoAdjustment2);
    promoItemDetailResponse.setPendingPromoSkuDetails(Arrays.asList(pendingPromoSkuDetail));

    categoryDTO = new CategoryDTO();
    categoryDTO.setChildCount(1);
    categoryDTO.setParentCategoryId(CATEGORY_ID);
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTO.setName(CATEGORY_NAME);

    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setId(CATEGORY_ID_1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse1.setName(CATEGORY_NAME_1);
    categoryResponse1.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse1.setNameEnglish(CATEGORY_NAME_ENGLISH_1);
    categoryResponse1.setActivated(true);

    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setId(CATEGORY_ID_2);
    categoryResponse2.setCategoryCode(CATEGORY_CODE_2);
    categoryResponse2.setName(CATEGORY_NAME_2);
    categoryResponse2.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse2.setNameEnglish(CATEGORY_NAME_ENGLISH_2);
    categoryResponse2.setActivated(true);

    CategoryResponse categoryResponse3 = new CategoryResponse();
    categoryResponse3.setId(CATEGORY_ID_3);
    categoryResponse3.setCategoryCode(CATEGORY_CODE_3);
    categoryResponse3.setName(CATEGORY_NAME_3);
    categoryResponse3.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse3.setNameEnglish(CATEGORY_NAME_ENGLISH_3);
    categoryResponse3.setActivated(true);

    CategoryResponse categoryResponse4 = new CategoryResponse();
    categoryResponse4.setId(CATEGORY_ID_4);
    categoryResponse4.setCategoryCode(CATEGORY_CODE_4);
    categoryResponse4.setName(CATEGORY_NAME_4);
    categoryResponse4.setParentCategoryId(null);
    categoryResponse4.setNameEnglish(CATEGORY_NAME_ENGLISH_4);
    categoryResponse4.setActivated(true);

    CategoryResponse categoryResponse5 = new CategoryResponse();
    categoryResponse5.setId(CATEGORY_ID_5);
    categoryResponse5.setCategoryCode(CATEGORY_CODE_5);
    categoryResponse5.setName(CATEGORY_NAME_5);
    categoryResponse5.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse5.setActivated(true);

    CategoryResponse categoryResponse6 = new CategoryResponse();
    categoryResponse6.setId(CATEGORY_ID_6);
    categoryResponse6.setCategoryCode(CATEGORY_CODE_6);
    categoryResponse6.setName(CATEGORY_NAME_6);
    categoryResponse6.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse6.setActivated(true);

    CategoryResponse categoryResponse7 = new CategoryResponse();
    categoryResponse7.setId(CATEGORY_ID_7);
    categoryResponse7.setCategoryCode(CATEGORY_CODE_7);
    categoryResponse7.setName(CATEGORY_NAME_7);
    categoryResponse7.setParentCategoryId(CATEGORY_ID_3);

    CategoryResponse categoryResponse8 = new CategoryResponse();
    categoryResponse8.setId(CATEGORY_ID_8);
    categoryResponse8.setCategoryCode(CATEGORY_CODE_8);
    categoryResponse8.setName(CATEGORY_NAME_8);
    categoryResponse8.setParentCategoryId(CATEGORY_ID_9);
    categoryResponse8.setActivated(true);

    CategoryResponse categoryResponse9 = new CategoryResponse();
    categoryResponse9.setId(CATEGORY_ID_9);
    categoryResponse9.setCategoryCode(CATEGORY_CODE_9);
    categoryResponse9.setName(CATEGORY_NAME_9);
    categoryResponse9.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse9.setActivated(true);

    CategoryHierarchyResponse categoryHierarchyResponse1 = new CategoryHierarchyResponse();
    categoryHierarchyResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryHierarchyResponse1.setCategoryId(CATEGORY_ID_1);
    categoryHierarchyResponse1.setProductCount(PRODUCT_COUNT_1);
    categoryHierarchyResponse1.setCategoryHierarchy(
        Arrays.asList(categoryResponse1, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse2 = new CategoryHierarchyResponse();
    categoryHierarchyResponse2.setCategoryCode(CATEGORY_CODE_5);
    categoryHierarchyResponse2.setCategoryId(CATEGORY_ID_5);
    categoryHierarchyResponse2.setProductCount(PRODUCT_COUNT_2);
    categoryHierarchyResponse2
        .setCategoryHierarchy(Arrays.asList(categoryResponse5, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse3 = new CategoryHierarchyResponse();
    categoryHierarchyResponse3.setCategoryCode(CATEGORY_CODE_6);
    categoryHierarchyResponse3.setCategoryId(CATEGORY_ID_6);
    categoryHierarchyResponse3.setProductCount(PRODUCT_COUNT_3);
    categoryHierarchyResponse3.setCategoryHierarchy(
        Arrays.asList(categoryResponse6, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse4 = new CategoryHierarchyResponse();
    categoryHierarchyResponse4.setCategoryCode(CATEGORY_CODE_7);
    categoryHierarchyResponse4.setCategoryId(CATEGORY_ID_7);
    categoryHierarchyResponse4.setProductCount(PRODUCT_COUNT_4);
    categoryHierarchyResponse4
        .setCategoryHierarchy(Arrays.asList(categoryResponse7, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse5 = new CategoryHierarchyResponse();
    categoryHierarchyResponse5.setCategoryCode(CATEGORY_CODE_2);
    categoryHierarchyResponse5.setCategoryId(CATEGORY_ID_2);
    categoryHierarchyResponse5.setProductCount(PRODUCT_COUNT_5);
    categoryHierarchyResponse5
        .setCategoryHierarchy(Arrays.asList(categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse6 = new CategoryHierarchyResponse();
    categoryHierarchyResponse6.setCategoryCode(CATEGORY_CODE_8);
    categoryHierarchyResponse6.setCategoryId(CATEGORY_ID_8);
    categoryHierarchyResponse6.setProductCount(PRODUCT_COUNT_6);
    categoryHierarchyResponse6
        .setCategoryHierarchy(Arrays.asList(categoryResponse8, categoryResponse9, categoryResponse4));

    categoryHierarchyProductCountResponseList = Arrays
        .asList(categoryHierarchyResponse2, categoryHierarchyResponse3, categoryHierarchyResponse1,
            categoryHierarchyResponse6, categoryHierarchyResponse5, categoryHierarchyResponse4);

    productLevel3SummaryResponse.setItemSku(ITEM_SKU);
    productLevel3SummaryResponse.setItemName(ITEM_NAME);
    productLevel3SummaryResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    productLevel3SummaryResponse.setProductSyncStatus(ProductSyncStatus.SUCCESS);
    productLevel3SummaryResponse.setEnableEdit(true);
    productLevel3SummaryResponse.setProductScore(90.0);


    bulkProcessNotesResponse.setPromoNote(true);
    bulkProcessNotesResponse.setNotes(ITEM_SKU + COMMA_DELIMITER + ITEM_NAME);
    bulkProcessNotesResponse.setBulkProcessCode(DEFAULT_PROCESS_CODE);

    brandPredefinedAttributeValueResponse = new BrandPredefinedAttributeValueResponse();
    brandPredefinedAttributeValueResponse.setId(ID);
    brandPredefinedAttributeValueResponse.setValue(BRAND);
    brandPredefinedAttributeValueResponse.setBrandRequestCode(BRAND_REQUEST_CODE);
    brandPredefinedAttributeValueResponse.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    brandPredefinedAttributeValueResponse.setPredefinedAllowedAttributeCode(BRAND_PREDEFINED_CODE);
    brandPredefinedAttributeValueResponse.setSequence(10);
    minWholesaleDiscountResponse = MinWholesaleDiscountResponse.builder().percentage(10.0).price(200000.0).build();
    minWholesaleDiscountResponse1 = MinWholesaleDiscountResponse.builder().percentage(5.0).price(9999.0).build();
    minWholesaleDiscountResponses.add(minWholesaleDiscountResponse1);
    minWholesaleDiscountResponses.add(minWholesaleDiscountResponse);

    itemQuantityDiscountMap.put(3, 5.0);
    itemQuantityDiscountMap.put(6, 15.0);
    itemQuantityDiscountMap1.put(3, 9.0);
    itemQuantityDiscountMap1.put(6, 18.0);
    categoryQuantityDiscountMap.put(2, 8.0);
    categoryQuantityDiscountMap.put(5, 16.0);
    unifiedBulkDownloadResponse = new UnifiedBulkDownloadResponse();
    unifiedBulkDownloadResponse.setFilePath("filePath");
    clientResponse = new GdnRestSimpleResponse<> ();
    clientResponse.setSuccess(true);
    clientResponse.setValue(unifiedBulkDownloadResponse);

    categoryWebResponse.setCreatedBy(CREATED_BY);
    categoryWebResponse.setUpdatedBy(UPDATED_BY);
    categoryWebResponse.setCreatedDate(new Date());
    categoryWebResponse.setUpdatedDate(new Date());
    predefinedAttributeValueWebResponse.setValue(Constants.OEM);

    variantsErrorListResponse =
        VariantsErrorListResponse.builder().itemSku(ITEM_SKU).itemName(ITEM_NAME).code(ERROR_CODE)
            .message(ERROR_MESSAGE).build();

    pickupPointUpdateResponse = new PickupPointUpdateResponse();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setItemSku(ITEM_SKU);
    pickupPointResponse.setErrorCode(ERROR_CODE);
    pickupPointResponse.setErrorMessage(ERROR_MESSAGE);
    pickupPointUpdateResponse.setResponses(Arrays.asList(pickupPointResponse));

    preOrderResponse =
        PreOrderResponse.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();

    editProductResponse = EditProductV2Response.builder().productReview(true).reviewType(REVIEW_TYPE).build();

    productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setImages(productLevel3ImageResponseList);
    productL3DetailsResponse.setAttributes(productLevel3AttributeResponseListExternal);
    productL3DetailsResponse.setProductCode(PRODUCT_CODE);
    productL3DetailsResponse.setProductType(PRODUCT_TYPE);
    productL3DetailsResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3DetailsResponse.setBrand(BRAND);
    productL3DetailsResponse.setCategoryCode(CATEGORY_CODE);
    productL3DetailsResponse.setDescription(DESCRIPTION);
    productL3DetailsResponse.setCategoryId(CATEGORY_ID);
    productL3DetailsResponse.setCategoryName(CATEGORY_NAME);
    productL3DetailsResponse.setProductName(PRODUCT_NAME);
    productL3DetailsResponse.setCategoryHierarchy(CATEGORY_HIERARCHY);
    productL3DetailsResponse.setInstallationRequired(true);
    productL3DetailsResponse.setSynchronize(true);
    productL3DetailsResponse.setUrl(URL);
    productL3DetailsResponse.setForceReview(true);
    productL3DetailsResponse.setProductScore(new ProductScoreResponse(15, 10, 15, 5, 20, 9, 2, 18, 5, 1, 100));
    productL3DetailsResponse.setDefaultItemSku(ITEM_SKU);
    productL3DetailsResponse.setProductLevel3Logistics(productItemLevel3LogisticResponses);
    pickupPointDetailResponse =
      PickupPointDetailResponse.builder().pickupPointName(PICK_UP_POINT_NAME)
        .pickupPointCode(PICKUP_POINT_CODE).build();

    historyUpdateResponse =
      HistoryUpdateResponse.builder().activity(ACTIVITY).changedBy(CHANGED_BY).gdnName(PRODUCT_NAME)
        .gdnSku(ITEM_SKU).pickupPointCode(CODE).pickupPointName(PICK_UP_POINT_NAME)
        .oldValues(PRODUCT_NAME).newValues(PRODUCT_NAME_2).build();

    wholesalePriceSkuResponse =
      WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_CODE)
        .promoActive(true).build();

    ProductLevel3PriceResponse productLevel3PriceResponse1 = new ProductLevel3PriceResponse();
    productLevel3PriceResponse1.setPrice(10.0);
    productLevel3PriceResponse1.setSalePrice(10.0);

    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse1 = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponse1.setBuyable(true);
    productLevel3ViewConfigResponse1.setDisplay(true);

    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setLocationPath(IMAGE_URL);

    ProductItemWholesalePriceResponse productItemWholesalePriceResponse =
        new ProductItemWholesalePriceResponse(10, 10.0);

    itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setProductCode(PRODUCT_CODE);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPrices(Arrays.asList(productLevel3PriceResponse1));
    itemPickupPointListingL3Response.setViewConfigs(Arrays.asList(productLevel3ViewConfigResponse1));
    itemPickupPointListingL3Response.setImages(Arrays.asList(imageResponse));
    itemPickupPointListingL3Response.setProductItemWholesalePrices(Arrays.asList(productItemWholesalePriceResponse));

    productLevel3DetailsV2Response = new ProductLevel3DetailsV2Response();
    productLevel3DetailsV2Response.setImages(productLevel3ImageResponseList);
    productLevel3DetailsV2Response.setAttributes(productLevel3AttributeResponseList);
    productLevel3DetailsV2Response.setProductCode(PRODUCT_CODE);
    productLevel3DetailsV2Response.setProductType(PRODUCT_TYPE);
    productLevel3DetailsV2Response.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3DetailsV2Response.setBrand(BRAND);
    productLevel3DetailsV2Response.setCategoryCode(CATEGORY_CODE);
    productLevel3DetailsV2Response.setDescription(DESCRIPTION);
    productLevel3DetailsV2Response.setCategoryId(CATEGORY_ID);
    productLevel3DetailsV2Response.setCategoryName(CATEGORY_NAME);
    productLevel3DetailsV2Response.setProductName(PRODUCT_NAME);
    productLevel3DetailsV2Response.setCategoryHierarchy(CATEGORY_HIERARCHY);
    productLevel3DetailsV2Response.setInstallationRequired(true);
    productLevel3DetailsV2Response.setSynchronize(true);
    productLevel3DetailsV2Response.setUrl(URL);
    productLevel3DetailsV2Response.setForceReview(true);
    productLevel3DetailsV2Response.setProductScore(new ProductScoreResponse(15, 10, 15, 5, 20, 9, 2, 18, 5, 1, 100));
    productLevel3DetailsV2Response.setDefaultItemSku(ITEM_SKU);
    productLevel3DetailsV2Response.setProductLevel3Logistics(productItemLevel3LogisticResponses);
    saleChannel.add(SALE_CHANNEL);

    buyableScheduleResponse = new BuyableScheduleResponse();
    discoverableScheduleResponse = new DiscoverableScheduleResponse();
    buyableScheduleResponse.setBuyable(true);
    buyableScheduleResponse.setStartDateTime(TEN_DAYS_AGO);
    buyableScheduleResponse.setEndDateTime(TEN_DAYS_AFTER);
    discoverableScheduleResponse.setDiscoverable(true);
    discoverableScheduleResponse.setStartDateTime(TEN_DAYS_AGO);
    discoverableScheduleResponse.setEndDateTime(TEN_DAYS_AFTER);
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestSingleResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_SuccessFalseTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_ValueNullTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_SuccessTrueTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, RESPONSE, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestSingleResponse));
  }

  @Test
  public void validateBrandWipResponse() {
    ApiIncorrectInputDataException exception = new ApiIncorrectInputDataException();
    try {
      ResponseHelper.validateBrandWipResponse(new GdnRestSingleResponse(null, BRAND_NOT_FOUND, false, null, null));
    } catch (ApiIncorrectInputDataException e) {
      exception = e;
    } finally {
      assertEquals(BRAND_NOT_FOUND_ERROR_MSG, exception.getErrorMessage());
    }
  }

  @Test
  public void validateBrandWipResponse_ResponseNullTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateBrandWipResponse((GdnRestSingleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateBrandWipResponse_DifferentErrorCode() {
    boolean response =
        ResponseHelper.validateBrandWipResponse(new GdnRestSingleResponse(null, ERROR_CODE, false, null, null));
    Assertions.assertTrue(response);
  }

  @Test
  public void validateResponse_BaseResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((com.gdn.partners.core.web.dto.BaseResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_BaseResponse_SuccessFalseTest (){
    com.gdn.partners.core.web.dto.BaseResponse baseResponse =
        new com.gdn.partners.core.web.dto.BaseResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(baseResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_BaseResponse_SuccessTrueTest (){
    com.gdn.partners.core.web.dto.BaseResponse baseResponse =
        new com.gdn.partners.core.web.dto.BaseResponse(null, null, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(baseResponse));
  }

  @Test
  public void validateResponse_GdnRestListResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestListResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestListResponse_SuccessFalseTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, false, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestListResponse_SuccessFalse_ApiErrorCodeExceptionTest() {
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, PRODUCT_NAME_IS_EMPTY.getCode(), false,
            Collections.singletonList(RESPONSE), new PageMetaData(0, 10, 100), REQUEST_ID);
    ApiIncorrectInputDataException exception = new ApiIncorrectInputDataException();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    } catch (ApiIncorrectInputDataException e) {
      exception = e;
    } finally {
      assertEquals(exception.getClass(), ApiIncorrectInputDataException.class);
      assertEquals(ERROR_MESSAGE, exception.getErrorMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestListResponse_ValueNullTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null,
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestListResponse_SuccessTrueTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestListResponse));
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnBaseRestResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessFalseTest (){
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_InvalidExcelFile(){
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, "INVALID_FORMAT", false, REQUEST_ID);
    ApiIncorrectInputDataException exception = new ApiIncorrectInputDataException();
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    }catch (ApiIncorrectInputDataException e){
      exception = e;
    }finally {
      assertEquals(ApiIncorrectInputDataException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getErrorMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessFalseValidationErrorTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.VALIDATION.name(), false, REQUEST_ID);
    ValidationException exception = null;
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (ValidationException e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ErrorCategory.VALIDATION.getMessage() + ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessFalseValidationErrorMessageTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ErrorCategory.VALIDATION.getMessage() + ERROR_MESSAGE,
            ErrorCategory.VALIDATION.name(), false, REQUEST_ID);
    ValidationException exception = null;
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (ValidationException e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ErrorCategory.VALIDATION.getMessage() + ERROR_MESSAGE, exception.getMessage());
    }
  }


  @Test
  public void validateResponseProductCreationSuccessFalseTest (){
    GdnBaseRestResponse gdnBaseRestResponse =
      new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseProductCreationResponseSuccessFalseValidationErrorTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
      new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.VALIDATION.name(), false, REQUEST_ID);
    ValidationException exception = null;
    try {
      ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    } catch (ValidationException e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ErrorCategory.VALIDATION.getMessage() + ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseProductCreationResponseNullTest() {
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateProductCreationResponse(null));
  }

  @Test
  public void validateResponseProductCreationResponseSuccessTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(null, null, true, REQUEST_ID);
    boolean creationResponse = ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    Assertions.assertTrue(creationResponse);
  }

  @Test
  public void validateResponseProductCreationResponseShippingValidationErrorTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
      new GdnBaseRestResponse(SHIPPING_WEIGHT_EXCEEDED.getDesc(),
        SHIPPING_WEIGHT_EXCEEDED.getCode(), false, REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = null;
    try {
      ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(ApiIncorrectInputDataException.class, apiIncorrectInputDataException.getClass());
      assertEquals(SHIPPING_WEIGHT_EXCEEDED.getDesc(), apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void validateResponseProductCreationResponsePreOrderValidationErrorTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
      new GdnBaseRestResponse(ApiErrorCode.PREORDER_DATE_BEFORE_CURRENT_DATE.getDesc(),
        PREORDER_DATE_BEFORE_CURRENT_DATE.getCode(), false, REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = null;
    try {
      ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(ApiIncorrectInputDataException.class, apiIncorrectInputDataException.getClass());
      assertEquals(PREORDER_DATE_BEFORE_CURRENT_DATE.getDesc(), apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void validateResponseProductCreationResponseWrongErrorTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
      new GdnBaseRestResponse(ERROR_MESSAGE,
        ERROR_CODE, false, REQUEST_ID);
    Exception exception = null;
    try {
      ResponseHelper.validateProductCreationResponse(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assert exception != null;
      assertNotEquals(ApiIncorrectInputDataException.class, exception.getClass());
    }
  }


  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessTrueTest (){
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnBaseRestResponse));
  }

  @Test
  public void validateResponse_GdnBaseRestResponseErrorCodeNullTest() {
    Exception exception = null;
    try {
      GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(null, VALIDATION, false, REQUEST_ID);
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertTrue(exception.getMessage().contains(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
  }

  @Test
  public void toEstimateItemPriceWebResponse_Test() {
    EstimateItemPriceResponse estimateItemPriceResponse = new EstimateItemPriceResponse();
    estimateItemPriceResponse.setOfferPrice(OFFER_PRICE);
    EstimateItemPriceWebResponse response = ResponseHelper.toEstimateItemPriceWebResponse(estimateItemPriceResponse);
    assertEquals(OFFER_PRICE, response.getOfferPrice(), 0);
  }

  @Test
  public void toProductDetailWebResponseTest() {
    ProductDetailWebResponse response = ResponseHelper.toProductDetailWebResponse(productDetailResponse);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    assertEquals(response.getProductAttributeResponses().get(0).getId(), ATTRIBUTE_ID);
    assertTrue(response.getProductAttributeResponses().get(0).getAttribute().isVariantCreation());
    assertEquals(response.getImages().size(), 1);
    assertEquals(response.getCategories().size(), 1);
    assertNotNull(response.getProductCategoryResponses());
    assertEquals(response.getProductCategoryResponses().get(0).getCategoryCode(), CODE);
    assertEquals(response.getProductItemResponses().size(), 1);
  }

  @Test
  public void toProductLevel3WebResponseTest() {
    productLevel3Response.setVersion(VERSION);
    ProductLevel3WebResponse response = ResponseHelper.toProductLevel3WebResponse(productLevel3Response);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getImages());
    assertEquals(response.getImages().get(0).getMainImage(), true);
    assertNotNull(response.getItems());
    assertNotNull(response.getItems().get(0).getImages());
    assertEquals(response.getItems().get(0).getImages().get(0).getMainImage(), true);
    assertNotNull(response.getItems().get(0).getViewConfigs());
    assertEquals(response.getItems().get(0).getViewConfigs().get(0).getId(), ID);
    assertNotNull(response.getItems().get(0).getPrices());
    assertEquals(response.getItems().get(0).getPrices().get(0).getId(), ID);
    assertEquals(response.getItems().get(0).getItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(VERSION, response.getVersion());
    assertFalse(response.isEnableEdit());
  }

  @Test
  public void toProductLevel3WebResponseWithWholesalePriceTest() {
    productLevel3SummaryResponse.setPrices(new ArrayList<>());
    productLevel3SummaryResponse.getPrices().add(new ProductLevel3PriceResponse());
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(9000.0);
    productLevel3SummaryResponse.getPrices().get(0).setPrice(10000.0);
    productLevel3Response.setWholesalePriceConfigEnabled(true);
    productLevel3Response.getItems().get(0).setProductItemWholesalePriceResponses(new ArrayList<>());
    productLevel3Response.getItems().get(0).getProductItemWholesalePriceResponses()
        .add(new ProductItemWholesalePriceResponse(2, 10));
    productLevel3Response.getItems().get(0).setMarkForDelete(true);
    productLevel3Response.getItems().get(0).setArchived(true);
    ProductLevel3WebResponse response = ResponseHelper.toProductLevel3WebResponse(productLevel3Response);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertNotNull(response.getImages());
    assertEquals(response.getImages().get(0).getMainImage(), true);
    assertNotNull(response.getItems());
    assertNotNull(response.getItems().get(0).getImages());
    assertEquals(response.getItems().get(0).getImages().get(0).getMainImage(), true);
    assertNotNull(response.getItems().get(0).getViewConfigs());
    assertEquals(response.getItems().get(0).getViewConfigs().get(0).getId(), ID);
    assertNotNull(response.getItems().get(0).getPrices());
    assertEquals(response.getItems().get(0).getPrices().get(0).getId(), ID);
    assertEquals(response.getItems().get(0).getItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertTrue(response.isWholesalePriceConfigEnabled());
    assertNull(response.getItems().get(0).getWholesalePriceActivated());
    assertEquals(2, response.getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
    assertTrue(response.getItems().get(0).getArchived());
    assertTrue(response.getItems().get(0).isRejected());
  }

  @Test
  public void toActiveProductWebResponseTest() {
    activeProductResponse.setItemCount(10);
    ActiveProductWebResponse response = ResponseHelper.toActiveProductWebResponse(activeProductResponse);
    assertNotNull(response);
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(PRODUCT_SKU, response.getProductSku());
    assertEquals(IMAGE_URL, response.getImageUrl());
    assertEquals(10, response.getVariantCount());
  }

  @Test
  public void toActiveProductWebResponseEmptyItemListTest() {
    activeProductResponse.setItemDetailResponses(new ArrayList<>());
    activeProductResponse.setItemCount(10);
    ActiveProductWebResponse response = ResponseHelper.toActiveProductWebResponse(activeProductResponse);
    assertTrue(response.getItemDetailWebResponse().isEmpty());
  }

  @Test
  public void toProductItemDetailWebResponseListTest() {
    List<ProductItemDetailWebResponse> responses =
        ResponseHelper.toProductItemDetailWebResponseList(productItemDetailResponseList);
    assertNotNull(responses);
    assertNotNull(responses.get(0).getProductItemWebResponse().getImages());
    assertEquals(responses.size(), 1);
  }

  @Test
  public void toProductItemWebResponseListTest()  {
    List<ProductItemWebResponse> response =
        ResponseHelper.toProductItemWebResponseList(Arrays.asList(productItemResponse));
    assertNotNull(response);
    assertEquals(response.get(0).getSkuCode(), CODE);
  }

  @Test
  public void toBusinessPartnerProfileWebResponseTest() {
    profileResponse.setOfficial(true);
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_LIMIT, true);
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    profileResponse.setFlags(flags);
    Set<String> sellerList = Set.of(BUSINESS_PARTNER_CODE);
    BusinessPartnerProfileWebResponse response =
        ResponseHelper.toBusinessPartnerProfileWebResponse(profileResponse, false, BUSINESS_PARTNER_CODE, sellerList, true);
    assertNotNull(response);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PICK_UP_POINT_NAME, response.getPickupPoints().get(0).getName());
    assertTrue(response.isCncActivated());
    assertEquals(BUSINESS_PARTNER_NAME, response.getCompany().getBusinessPartnerName());
    assertTrue(response.getCompany().isCncActivated());
    assertEquals(saleChannel, response.getCompany().getSalesChannel());
    assertTrue(response.isBopisFlag());
    assertTrue(response.isOfficial());
    assertTrue(response.isProductLimitEnabled());
    assertFalse(response.isFaasActivated());
    assertTrue(response.isDistributionSeller());
    assertTrue(response.isProductConsequenceLimitation());
  }

  @Test
  public void toBusinessPartnerProfileWebResponseWithEmptyResponseTest() {
    profileResponse2.setBopisFlag(false);
    profileResponse2.setBigProductFlag(false);
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse2.setFlags(flags);
    BusinessPartnerProfileWebResponse response =
        ResponseHelper.toBusinessPartnerProfileWebResponse(profileResponse2, true,"", Collections.emptySet(), false);
    assertFalse(response.isCncActivated());
    assertFalse(response.isBopisFlag());
    assertFalse(response.isBigProductFlag());
    assertFalse(response.isProductLimitEnabled());
    assertTrue(response.isFaasActivated());
    assertFalse(response.isDistributionSeller());
  }

  @Test
  public void toBusinessPartnerProfileWebResponseCompanyNonNullTest() {
    profileResponse.setCompany(companyDTO);
    profileResponse.setBopisFlag(true);
    profileResponse.setBigProductFlag(true);
    Set<String> sellerList = Set.of("OTHER_BP");
    BusinessPartnerProfileWebResponse response =
        ResponseHelper.toBusinessPartnerProfileWebResponse(profileResponse, true,BUSINESS_PARTNER_CODE ,sellerList, true);
    assertNotNull(response);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PICK_UP_POINT_NAME, response.getPickupPoints().get(0).getName());
    assertEquals(BUSINESS_PARTNER_NAME, response.getBusinessPartnerName());
    assertEquals(ID, response.getId());
    assertEquals(saleChannel, response.getCompany().getSalesChannel());
    assertTrue(response.isBopisFlag());
    assertTrue(response.isBigProductFlag());
    assertFalse(response.isDistributionSeller());
  }

  @Test
  public void toProductLevel3WipDetailWebResponseTest() {
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse =
        ResponseHelper.toProductLevel3WipDetailWebResponse(productLevel3WipDetailResponse);
    assertNotNull(productLevel3WipDetailWebResponse);
    assertEquals(GDN_PRODUCT_SKU, productLevel3WipDetailWebResponse.getProductSku());
    assertNotNull(productLevel3WipDetailWebResponse.getItems());
    assertEquals(2, productLevel3WipDetailWebResponse.getItems().size());
    assertEquals(GDN_ITEM_SKU1, productLevel3WipDetailWebResponse.getItems().get(0).getGdnSku());
    assertEquals(GDN_ITEM_SKU2, productLevel3WipDetailWebResponse.getItems().get(1).getGdnSku());
    assertNotNull(productLevel3WipDetailWebResponse.getAttributes());
    assertEquals(1, productLevel3WipDetailWebResponse.getAttributes().size());
    assertEquals(ATTRIBUTE_ID, productLevel3WipDetailWebResponse.getAttributes().get(0).getAttributeId());
    assertTrue(productLevel3WipDetailWebResponse.getProductLevel3Logistics().get(0).isRequiredLongLat());
    assertTrue(productLevel3WipDetailWebResponse.getProductLevel3Logistics().get(0).isSelected());
    assertTrue(productLevel3WipDetailWebResponse.getProductLevel3Logistics().get(0).isRequiredLongLat());
    assertEquals(LOGISTIC_OPTION_NAME,
        productLevel3WipDetailWebResponse.getProductLevel3Logistics().get(0)
            .getLogisticProductName());
  }

  @Test
  public void toProductLevel3WipDetailWebResponseWithWholesalePriceTest() {
    ProductItemWholesalePriceResponse productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();
    productItemWholesalePriceResponse.setQuantity(2);
    productItemWholesalePriceResponse.setWholesaleDiscount(20);
    productLevel3WipDetailResponse.getItems().get(0).setProductItemWholesalePriceResponses(Arrays.asList(productItemWholesalePriceResponse));
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse =
            ResponseHelper.toProductLevel3WipDetailWebResponse(productLevel3WipDetailResponse);
    assertNotNull(productLevel3WipDetailWebResponse);
    assertEquals(GDN_PRODUCT_SKU, productLevel3WipDetailWebResponse.getProductSku());
    assertNotNull(productLevel3WipDetailWebResponse.getItems());
    assertEquals(2, productLevel3WipDetailWebResponse.getItems().size());
    assertEquals(GDN_ITEM_SKU1, productLevel3WipDetailWebResponse.getItems().get(0).getGdnSku());
    assertEquals(GDN_ITEM_SKU2, productLevel3WipDetailWebResponse.getItems().get(1).getGdnSku());
    assertEquals(1, productLevel3WipDetailWebResponse.getItems().get(0).getProductItemWholesalePriceResponses().size());
    assertNotNull(productLevel3WipDetailWebResponse.getAttributes());
    assertEquals(1, productLevel3WipDetailWebResponse.getAttributes().size());
    assertEquals(ATTRIBUTE_ID, productLevel3WipDetailWebResponse.getAttributes().get(0).getAttributeId());
  }

  @Test
  public void toSuspensionWebResponseListTest() {
    List<SuspensionWebResponse> suspensionWebResponses =
        ResponseHelper.toSuspensionWebResponseList(suspensionItemResponseList);
    assertEquals(1, suspensionWebResponses.size());
    assertEquals(PRODUCT_SKU, suspensionWebResponses.get(0).getProductSku());
    assertEquals(ITEM_NAME, suspensionWebResponses.get(0).getItemName());
    assertEquals(ITEM_SKU, suspensionWebResponses.get(0).getItemSku());
    assertEquals(CATEGORY_NAME, suspensionWebResponses.get(0).getCategoryName());
    assertEquals(REASON, suspensionWebResponses.get(0).getReason());
    assertNotNull(suspensionWebResponses.get(0).getBannedDate());
  }

  @Test
  public void toProductCountWebResponseTest_forActiveProducts() {
    ProductCountWebResponse productCountWebResponse =
        ResponseHelper.toProductCountWebResponse(productLevel3SummaryCountResponse);
    assertNotNull(productCountWebResponse);
    assertEquals(40L, productCountWebResponse.getAvailable().longValue());
    assertEquals(30L, productCountWebResponse.getMinimumStock().longValue());
    assertEquals(20L, productCountWebResponse.getOutOfStock().longValue());
    assertEquals(60L, productCountWebResponse.getTotalCounts().longValue());
    assertEquals(BUSINESS_PARTNER_CODE, productCountWebResponse.getBusinessPartnerCode());
  }

  @Test
  public void toProductCountWebResponseTest_forInProgressProducts() {
    ProductCountWebResponse productCountWebResponse =
        ResponseHelper.toProductCountWebResponse(countProductLevel3WipResponse);
    assertNotNull(productCountWebResponse);
    assertEquals(40L, productCountWebResponse.getNeedCorrection().longValue());
    assertEquals(50L, productCountWebResponse.getWaitingForApproval().longValue());
    assertEquals(60L, productCountWebResponse.getNeedAction().longValue());
    assertEquals(150L, productCountWebResponse.getTotalCounts().longValue());
  }

  @Test
  public void toProductCountWebResponseTest_forInActiveProducts() {
    ProductCountWebResponse productCountWebResponse =
        ResponseHelper.toProductCountWebResponse(countProductLevel3InactiveResponse);
    assertNotNull(productCountWebResponse);
    assertEquals(40L, productCountWebResponse.getSuspended().longValue());
    assertEquals(50L, productCountWebResponse.getArchived().longValue());
    assertEquals(60L, productCountWebResponse.getRejected().longValue());
    assertEquals(150L, productCountWebResponse.getTotalCounts().longValue());
  }

  @Test
  public void validateBrandResponse_emptyClientResponseTest() {
    try{
      boolean result = ResponseHelper.validateBrandResponse(null);
    } catch (ClientException e) {
    }
  }

  @Test
  public void validateBrandResponse_successFalseTest() {
    GdnRestSingleResponse response = new GdnRestSingleResponse(null, DEFAULT_REQUEST_ID);
    response.setSuccess(false);
    try{
      boolean result = ResponseHelper.validateBrandResponse(response);
    } catch (ClientException e) {
    }
  }

  @Test
  public void toProductLevel3SummaryResponseFromRejectedSkuProductResponseTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = ResponseHelper
        .toProductLevel3SummaryResponseFromRejectedSkuProductResponse(Arrays.asList(rejectedSkuProductResponse));
    assertEquals(CREATED_BY, productLevel3SummaryResponses.get(0).getCreatedBy());
    assertEquals(BRAND, productLevel3SummaryResponses.get(0).getBrand());
    assertEquals(BANNED_DATE,productLevel3SummaryResponses.get(0).getUpdatedDate());
  }

  @Test
  public void toProductLevel3SummaryResponseFromSuspensionItemResponseTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
        ResponseHelper.toProductLevel3SummaryResponseFromSuspensionItemResponse(Arrays.asList(suspensionItemResponse));
    assertEquals(DATA_LINK, productLevel3SummaryResponses.get(0).getProductDetailPageLink());
    assertEquals(REASON, productLevel3SummaryResponses.get(0).getReason());
    assertEquals(ITEM_NAME, productLevel3SummaryResponses.get(0).getItemName());
    assertEquals(BANNED_DATE,productLevel3SummaryResponses.get(0).getUpdatedDate());
  }

  @Test
  public void toItemDetailWebResponseTest() {
    List<ItemDetailWebResponse> itemDetailWebResponseList =
        ResponseHelper.toItemDetailWebResponse(itemSummaryResponseList);
    assertEquals(2, itemDetailWebResponseList.size());
    assertEquals(GDN_ITEM_SKU1, itemDetailWebResponseList.get(0).getItemSku());
    assertEquals(ITEM_NAME, itemDetailWebResponseList.get(0).getItemName());
    assertEquals(GDN_ITEM_SKU2, itemDetailWebResponseList.get(1).getItemSku());
    assertEquals(ITEM_NAME_1, itemDetailWebResponseList.get(1).getItemName());
  }

  @Test
  public void toProductNameWebResponseTest() {
    List<ItemDetailWebResponse> itemDetailWebResponseList =
        ResponseHelper.toProductNameWebResponse(productNameSuggestionResponses);
    assertEquals(2, itemDetailWebResponseList.size());
    assertEquals(PRODUCT_NAME, itemDetailWebResponseList.get(0).getProductName());
    assertEquals(PRODUCT_SKU, itemDetailWebResponseList.get(0).getProductSku());
    assertEquals(PRODUCT_NAME_2, itemDetailWebResponseList.get(1).getProductName());
    assertEquals(PRODUCT_CODE, itemDetailWebResponseList.get(1).getProductSku());
    assertNull(itemDetailWebResponseList.get(0).getItemName());
  }

  @Test
  public void toInProcessWebResponseTest() {
    List<InProcessWebResponse> inProcessWebResponseList =
        ResponseHelper.toInProcessWebResponse(Collections.singletonList(productLevel3WipResponse));
    assertEquals(1, inProcessWebResponseList.size());
    assertEquals(BRAND, inProcessWebResponseList.get(0).getBrand());
    assertEquals(ITEM_NAME, inProcessWebResponseList.get(0).getItemName());
    assertEquals(REASON, inProcessWebResponseList.get(0).getReason());
    assertEquals(IMAGE_URL, inProcessWebResponseList.get(0).getProductMainImage());
    assertTrue(inProcessWebResponseList.get(0).isForceReview());
    assertEquals(IMAGE_VIOLATION, inProcessWebResponseList.get(0).getForceReviewImageViolations().get(0).getEnName());
    assertEquals(IMAGE_VIOLATION, inProcessWebResponseList.get(0).getForceReviewImageViolations().get(0).getInName());
  }

  @Test
  public void toPromoItemDetailWebResponseTest() {
    PromoItemDetailWebResponse promoItemDetailWebResponse =
        ResponseHelper.toPromoItemDetailWebResponse(promoItemDetailResponse);
    assertEquals(PROMO_ADJUSTMENT_CODE1,
        promoItemDetailWebResponse.getActivePromoSkuDetail().getPromoAdjustment().getPromoAdjustmentCode());
    assertEquals(PROMO_ADJUSTMENT_NAME1,
        promoItemDetailWebResponse.getActivePromoSkuDetail().getPromoAdjustment().getPromoAdjustmentName());
    assertEquals(PROMO_ADJUSTMENT_CODE2,
        promoItemDetailWebResponse.getPendingPromoSkuDetail().getPromoAdjustment().getPromoAdjustmentCode());
    assertEquals(PROMO_ADJUSTMENT_NAME2,
        promoItemDetailWebResponse.getPendingPromoSkuDetail().getPromoAdjustment().getPromoAdjustmentName());
    assertEquals(DETAIL_URL, promoItemDetailWebResponse.getActivePromoSkuDetail().getDetailUrl());
  }

  @Test
  public void toCategoryWebResponseListTest() {
    List<CategoryWebResponse> categoryWebResponses = ResponseHelper.toCategoryWebResponseList(Arrays.asList(categoryDTO));
    assertEquals(1, categoryWebResponses.size());
    assertEquals(CATEGORY_CODE, categoryWebResponses.get(0).getCategoryCode());
    assertEquals(CATEGORY_NAME, categoryWebResponses.get(0).getName());
    assertEquals(1, categoryWebResponses.get(0).getChildCount());
    assertEquals(CATEGORY_ID, categoryWebResponses.get(0).getParentCategoryId());
  }

  @Test
  public void toCategorySuggestionWebResponseListBaseResponseTest() {
    List<CategorySuggestionWebResponse> categorySuggestionWebResponses = ResponseHelper.toCategorySuggestionWebResponseListBaseResponse(categoryHierarchyProductCountResponseList);
    assertEquals(CATEGORY_CODE_4, categorySuggestionWebResponses.get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, categorySuggestionWebResponses.get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_9, categorySuggestionWebResponses.get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_2,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_5,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_1,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_6,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_8,
        categorySuggestionWebResponses.get(0).getChildCategories().get(1).getChildCategories().get(0).getCategoryCode());
    assertEquals(32, categorySuggestionWebResponses.get(0).getProductCount());
    assertEquals(31, categorySuggestionWebResponses.get(0).getChildCategories().get(0).getProductCount());
    assertEquals(1, categorySuggestionWebResponses.get(0).getChildCategories().get(1).getProductCount());
    assertEquals(21, categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getProductCount());
    assertEquals(8, categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(1).getProductCount());
    assertEquals(16,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getProductCount());
    assertEquals(4,
        categorySuggestionWebResponses.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getProductCount());
    assertEquals(1, categorySuggestionWebResponses.get(0).getChildCategories().get(1).getChildCategories().get(0).getProductCount());
  }

  @Test
  public void testGetMerchantTypeFromPurchaseTermAndInventoryFulfillment() {
    String result = "";
    result = ResponseHelper.getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_COMISSION,
        Constants.INVENTORY_FULFILLMENT_BLIBLI);
    assertEquals(Constants.MERCHANT_TYPE_CC, result);
    result = ResponseHelper.getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_REBATE,
        Constants.INVENTORY_FULFILLMENT_BLIBLI);
    assertEquals(Constants.MERCHANT_TYPE_RC, result);
    result = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_PURCHASE_ORDER,
            Constants.INVENTORY_FULFILLMENT_BLIBLI);
    assertEquals(Constants.MERCHANT_TYPE_TD, result);
    result = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_PURCHASE_CONSIGNMENT,
            Constants.INVENTORY_FULFILLMENT_BLIBLI);
    assertEquals(Constants.MERCHANT_TYPE_TC, result);
    result = ResponseHelper.getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_COMISSION,
        Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    assertEquals(Constants.MERCHANT_TYPE_CM, result);
    result = ResponseHelper.getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_REBATE,
        Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    assertEquals(Constants.MERCHANT_TYPE_RB, result);
    result = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_PURCHASE_ORDER,
            Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    assertEquals(Constants.MERCHANT_TYPE_MP, result);
    result = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(Constants.PURCHASE_TERM_PURCHASE_CONSIGNMENT,
            Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    assertEquals(Constants.MERCHANT_TYPE_MP, result);
  }

  @Test
  public void validateResponseForSyncUnsyncProduct_GdnBaseRestResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForSyncUnsyncProduct((GdnBaseRestResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForSyncUnsyncProduct_archivedItem_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ErrorMessages.UPDATING_ARCHIVED_ITEM, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForSyncUnsyncProduct(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      assertTrue(exception.getMessage().contains(ErrorMessages.UPDATING_ARCHIVED_ITEM));
    }
  }

  @Test
  public void validateResponseForSyncUnsyncProduct_rejectedProduct_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ErrorMessages.UPDATING_REJECTED_ITEM, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForSyncUnsyncProduct(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      assertTrue(exception.getMessage().contains(ErrorMessages.UPDATING_REJECTED_ITEM));
    }
  }

  @Test
  public void validateResponseForSyncUnsyncProduct_suspendedProduct_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ErrorMessages.UPDATING_SUSPENDED_ITEM, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForSyncUnsyncProduct(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      assertTrue(exception.getMessage().contains(ErrorMessages.UPDATING_SUSPENDED_ITEM));
    }
  }

  @Test
  public void validateResponseForSyncUnsyncProduct_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForSyncUnsyncProduct(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void toProductLevel3SummaryWebResponseTest() {
    productLevel3SummaryResponse.setPrices(new ArrayList<>());
    productLevel3SummaryResponse.getPrices().add(new ProductLevel3PriceResponse());
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(9000.0);
    productLevel3SummaryResponse.getPrices().get(0).setPrice(10000.0);
    productLevel3SummaryResponse.setArchived(true);
    ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse =
        ResponseHelper.toProductLevel3SummaryWebResponse(productLevel3SummaryResponse);
    assertEquals(ITEM_SKU, productLevel3SummaryWebResponse.getItemSku());
    assertEquals(ITEM_NAME, productLevel3SummaryWebResponse.getItemName());
    assertEquals(BUSINESS_PARTNER_CODE, productLevel3SummaryWebResponse.getMerchantCode());
    assertEquals(90.0, productLevel3SummaryWebResponse.getProductScore(), 0);
    assertEquals(ProductSyncWebStatus.SUCCESS, productLevel3SummaryWebResponse.getProductSyncStatus());
    assertTrue(productLevel3SummaryWebResponse.isEnableEdit());
    assertEquals(10.0, productLevel3SummaryWebResponse.getPrices().get(0).getDiscountPercentage(), 0);
    assertTrue(productLevel3SummaryWebResponse.getIsArchived());
  }

  @Test
  public void toProductLevel3SummaryWebResponseCncTest() {
    productLevel3SummaryResponse.setCncActivated(true);
    productLevel3SummaryResponse.setPrices(new ArrayList<>());
    productLevel3SummaryResponse.getPrices().add(new ProductLevel3PriceResponse());
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(9000.0);
    productLevel3SummaryResponse.getPrices().get(0).setPrice(10000.0);
    productLevel3SummaryResponse.setArchived(true);
    ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse =
        ResponseHelper.toProductLevel3SummaryWebResponse(productLevel3SummaryResponse);
    assertEquals(ITEM_SKU, productLevel3SummaryWebResponse.getItemSku());
    assertEquals(ITEM_NAME, productLevel3SummaryWebResponse.getItemName());
    assertEquals(BUSINESS_PARTNER_CODE, productLevel3SummaryWebResponse.getMerchantCode());
    assertEquals(90.0, productLevel3SummaryWebResponse.getProductScore(), 0);
    assertEquals(ProductSyncWebStatus.SUCCESS, productLevel3SummaryWebResponse.getProductSyncStatus());
    assertTrue(productLevel3SummaryWebResponse.isEnableEdit());
    assertEquals(10.0, productLevel3SummaryWebResponse.getPrices().get(0).getDiscountPercentage(), 0);
    assertTrue(productLevel3SummaryWebResponse.getIsArchived());
    assertTrue(productLevel3SummaryWebResponse.isCncActivated());
  }

  @Test
  public void toProductLevel3SummaryWebResponse_NullTest() {
    productLevel3SummaryResponse.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse =
        ResponseHelper.toProductLevel3SummaryWebResponse(productLevel3SummaryResponse);
    assertEquals(ITEM_SKU, productLevel3SummaryWebResponse.getItemSku());
    assertEquals(ITEM_NAME, productLevel3SummaryWebResponse.getItemName());
    assertEquals(BUSINESS_PARTNER_CODE, productLevel3SummaryWebResponse.getMerchantCode());
    assertEquals(90.0, productLevel3SummaryWebResponse.getProductScore(), 0);
    assertEquals(ProductSyncWebStatus.SUCCESS, productLevel3SummaryWebResponse.getProductSyncStatus());
    assertTrue(productLevel3SummaryWebResponse.isEnableEdit());
  }

  @Test
  public void toPromoUpdateProductResponseTest() {
    PromoUpdateProductResponse promoUpdateProductResponse =
        ResponseHelper.toPromoUpdateProductResponse(bulkProcessNotesResponse);
    assertEquals(ITEM_SKU, promoUpdateProductResponse.getItemSku());
    assertEquals(ITEM_NAME, promoUpdateProductResponse.getItemName());
  }

  @Test
  public void toBrandPredefinedAttributeValueWebResponseTest() {
    List<BrandPredefinedAttributeValueWebResponse> response =
        ResponseHelper.toBrandPredefinedAttributeValueWebResponse(brandPredefinedAttributeValueResponse);
    Assertions.assertEquals(ID, response.get(0).getId());
    Assertions.assertEquals(BRAND, response.get(0).getValue());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, response.get(0).getBrandApprovalStatus());
    Assertions.assertEquals(BRAND_PREDEFINED_CODE, response.get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(10, response.get(0).getSequence(), 0);
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.get(0).getBrandRequestCode());
  }

  @Test
  public void validateBrandDetailResponseNullTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateBrandDetailResponse(null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateBrandDetailResponseSucessFalseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateBrandDetailResponse(new GdnRestSingleResponse(ERROR_MESSAGE, null, false, null, null));
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateBrandDetailResponseContentNullTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateBrandDetailResponse(
          new GdnRestSingleResponse(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE, null, true, null, null));
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(InvalidStateException.class, exception.getClass());
      assertEquals(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE, exception.getMessage());

    }
  }
  @Test
  public void validateBulkDownloadFileContentResponseTest() {
    ResponseHelper.validateBulkDownloadFileContentResponse(clientResponse, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(true, clientResponse.isSuccess());
  }

  @Test
  public void validateBulkDownloadFileContentResponseClientExceptionTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateBulkDownloadFileContentResponse(null, BUSINESS_PARTNER_CODE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void getDiscountBracketTest(){
    Double discount = ResponseHelper.getDiscountBracket(minWholesaleDiscountResponses, 10000);
    assertEquals(DISCOUNT, discount, 0);
  }

  @Test
  public void getThresholdFalseTest(){
    boolean threshold = ResponseHelper.getThreshold(itemQuantityDiscountMap, categoryQuantityDiscountMap);
    assertFalse(threshold);
  }

  @Test
  public void getThresholdTrueTest(){
    boolean threshold = ResponseHelper.getThreshold(itemQuantityDiscountMap1, categoryQuantityDiscountMap);
    assertTrue(threshold);
  }

  @Test
  public void toWholeCountWebResponseTest() {
    WholesaleCountWebResponse response =
        ResponseHelper.toWholeCountWebResponse(new WholeSaleCountResponse(10, 10, 10, "path"));
    Assertions.assertEquals("path", response.getDownloadFilePath());
    Assertions.assertEquals(10, response.getWholeSaleTurnOffCount());
    Assertions.assertEquals(10, response.getWholeSaleFailedCount());
    Assertions.assertEquals(10, response.getWholeSaleUpdatedCount());
  }

  @Test
  public void validateBulkDownloadFileContentResponseApplicationRuntimeExceptionTest() {
    Exception exception = new Exception();
    clientResponse.setSuccess(false);
    clientResponse.setErrorCode(ErrorCategory.VALIDATION.getCode());
    clientResponse.setErrorMessage(ErrorCategory.VALIDATION.getMessage());
    try {
      ResponseHelper.validateBulkDownloadFileContentResponse(clientResponse, BUSINESS_PARTNER_CODE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
    }
  }

  @Test
  public void toProductScoreRuleWebResponseRuleConfigNullTest() {
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setCategoryCode(CATEGORY_CODE);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(20);
    productScoreRuleDtoMap.put(PRODUCT_TITLE, maxScoreAndRuleConfigResponse);
    productScoreRuleResponse.setProductScoreRules(productScoreRuleDtoMap);
    ProductScoreRuleWebResponse productScoreRuleWebResponse =
        ResponseHelper.toProductScoreRuleWebResponse(productScoreRuleResponse);
    Assertions.assertEquals(CATEGORY_CODE, productScoreRuleWebResponse.getCategoryCode());
    Assertions.assertEquals(20, productScoreRuleWebResponse.getProductScoreRules().get(PRODUCT_TITLE).getMaxScore());
  }

  @Test
  public void toProductScoreRuleWebResponseTest() {
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setCategoryCode(CATEGORY_CODE);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    RuleConfigResponse ruleConfig = new RuleConfigResponse();
    ruleConfig.setOperator("==");
    ruleConfig.setScore(20);
    ruleConfig.setValue(10);
    maxScoreAndRuleConfigResponse.setMaxScore(20);
    maxScoreAndRuleConfigResponse.setRuleConfig(Arrays.asList(ruleConfig));
    productScoreRuleDtoMap.put(PRODUCT_TITLE, maxScoreAndRuleConfigResponse);
    productScoreRuleResponse.setProductScoreRules(productScoreRuleDtoMap);
    productScoreRuleResponse.setIgnoreSymbols(Arrays.asList(" "));
    IgnoreAttributeSet ignoreAttributeSet = new IgnoreAttributeSet("Warranty", "No Warranty", Arrays.asList("Warranty Detail"));
    productScoreRuleResponse.setIgnoreAttributes(Arrays.asList(ignoreAttributeSet));
    ProductScoreRuleWebResponse productScoreRuleWebResponse =
        ResponseHelper.toProductScoreRuleWebResponse(productScoreRuleResponse);
    Assertions.assertEquals(CATEGORY_CODE, productScoreRuleWebResponse.getCategoryCode());
    Assertions.assertEquals(20, productScoreRuleWebResponse.getProductScoreRules().get(PRODUCT_TITLE).getMaxScore());
    Assertions.assertEquals("==",
        productScoreRuleWebResponse.getProductScoreRules().get(PRODUCT_TITLE).getRuleConfig().get(0).getOperator());
    Assertions.assertEquals(20.0,
        productScoreRuleWebResponse.getProductScoreRules().get(PRODUCT_TITLE).getRuleConfig().get(0).getScore(), 1);
    Assertions.assertEquals(10,
        productScoreRuleWebResponse.getProductScoreRules().get(PRODUCT_TITLE).getRuleConfig().get(0).getValue());
    Assertions.assertEquals(" ", productScoreRuleWebResponse.getIgnoreSymbols().get(0));
    Assertions.assertEquals("Warranty", productScoreRuleWebResponse.getIgnoreAttributes().get(0).getAttributeName());
  }

  @Test
  public void toUpcCodeAndImagesWebResponseTest() {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(new com.gdn.x.product.rest.web.model.response.ProductResponse());
    productAndItemsResponse.getProduct().setProductSku(PRODUCT_SKU);
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setItemSku(ITEM_SKU);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setUpcCode(UPC_CODE);
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO(true, "Path", 1);
    masterDataItemDTO.setMasterDataItemImages(Collections.singletonList(masterDataItemImageDTO));
    itemResponse.setMasterDataItem(masterDataItemDTO);
    productAndItemsResponse.setItems(Collections.singletonList(itemResponse));
    UpcCodeAndImagesWebResponse upcCodeAndImagesWebResponse = ResponseHelper.toUpcCodeAndImagesWebResponse(productAndItemsResponse);
    assertNotNull(upcCodeAndImagesWebResponse);
    assertEquals(ITEM_SKU, upcCodeAndImagesWebResponse.getItemResponses().get(0).getItemSku());
    assertTrue(upcCodeAndImagesWebResponse.getItemResponses().get(0).getImages().get(0).isMainImage());
  }

  @Test
  public void toUpcCodeAndImagesWebResponseTest_nullL3() {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(null);
    UpcCodeAndImagesWebResponse upcCodeAndImagesWebResponse = ResponseHelper.toUpcCodeAndImagesWebResponse(productAndItemsResponse);
    assertNotNull(upcCodeAndImagesWebResponse);
  }

  @Test
  public void toUpcCodeAndImagesWebResponseTest_emptyL4() {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(new com.gdn.x.product.rest.web.model.response.ProductResponse());
    productAndItemsResponse.setItems(Collections.EMPTY_LIST);
    UpcCodeAndImagesWebResponse upcCodeAndImagesWebResponse = ResponseHelper.toUpcCodeAndImagesWebResponse(productAndItemsResponse);
    assertNotNull(upcCodeAndImagesWebResponse);
  }

  @Test
  public void toProductLevel3WebResponseTest_nullProductScore() {
    productLevel3Response.setProductScore(null);
    productLevel3Response.setVersion(VERSION);
    ProductLevel3WebResponse response = ResponseHelper.toProductLevel3WebResponse(productLevel3Response);
    assertNotNull(response);
    assertNull(response.getProductScore());
  }


  @Test
  public void clearPIIConfidentialInformationForCategoryResponseTest() {
    ResponseHelper.clearPIIConfidentialInformationForCategoryResponse(categoryWebResponse);
  }

  @Test
  void validateResponse_nullResponse() throws Exception {
    Response response = null;
    Assertions.assertThrows(ClientException.class, () -> ResponseHelper.validateResponse(response));
  }

  @Test
  void validateResponse_failedResponse() throws Exception {
    Response response = new Response();
    response.setCode(400);
    Assertions.assertThrows(ClientException.class, () -> ResponseHelper.validateResponse(response));
  }

  @Test
  public void validateResponse_success() throws Exception {
    Response<LogisticsDownloadTemplateResponse> response = new Response<>();
    response.setCode(200);
    response.setData(new LogisticsDownloadTemplateResponse());
    boolean success = ResponseHelper.validateResponse(response);
    assertTrue(success);
  }

  @Test
  public void toListSellerLogisticsProductResponse() {
    List<GetSellerLogisticProductResponse> getSellerLogisticProductResponseList = new ArrayList<>();
    getSellerLogisticProductResponseList.add(new GetSellerLogisticProductResponse());
    List<SellerLogisticsProductResponse> sellerLogisticsProductResponseList =
        ResponseHelper.toListSellerLogisticsProductResponse(getSellerLogisticProductResponseList);
    assertEquals(sellerLogisticsProductResponseList.size(), 1);
  }

  @Test
  public void toLogisticDownloadTemplateResponse() {
    DownloadSkuTemplateResponse downloadSkuTemplateResponse = new DownloadSkuTemplateResponse();
    downloadSkuTemplateResponse.setName(FILE_NAME);
    LogisticsDownloadTemplateResponse logisticsDownloadTemplateResponse =
        ResponseHelper.toLogisticDownloadTemplateResponse(downloadSkuTemplateResponse);
    assertEquals(logisticsDownloadTemplateResponse.getName(), FILE_NAME);
  }

  @Test
  public void toLogisticsExcelSkuUploadResponse() {
    UploadExcelSkuUpdateResponse uploadExcelSkuUpdateResponse = new UploadExcelSkuUpdateResponse();
    uploadExcelSkuUpdateResponse.setInvalidItemSkus(Arrays.asList(ITEM_NAME));
    LogisticsExcelSkuUploadResponse logisticsExcelSkuUploadResponse =
        ResponseHelper.toLogisticsExcelSkuUploadResponse(uploadExcelSkuUpdateResponse);
    assertEquals(logisticsExcelSkuUploadResponse.getInvalidItemSkus().get(0), ITEM_NAME);
  }

  @Test
  public void toLogisticsExcelSkuUploadStatusResponse() {
    UploadExcelSkuUpdateStatusResponse uploadExcelSkuUpdateStatusResponse =
        new UploadExcelSkuUpdateStatusResponse();
    uploadExcelSkuUpdateStatusResponse.setInProgress(false);
    ExcelSkuUpdateStatusResponse excelSkuUpdateStatusResponse =
        ResponseHelper.toLogisticsExcelSkuUploadStatusResponse(uploadExcelSkuUpdateStatusResponse);
    assertFalse(excelSkuUpdateStatusResponse.isInProgress());
  }

  @Test
  public void toLogAuditTrailUpdatedProductWebResponseTest() throws ParseException {
    LogAuditTrailUpdatedProductResponse logAuditTrailUpdatedProductResponse = new LogAuditTrailUpdatedProductResponse();
    logAuditTrailUpdatedProductResponse.setCreatedDateLog("01/27/2021 18:25:41");
    List<LogAuditTrailUpdatedProductWebResponse> logAuditTrailUpdatedProductWebResponses =
        ResponseHelper.toLogAuditTrailUpdatedProductWebResponse(Arrays.asList(logAuditTrailUpdatedProductResponse));
    Assertions.assertEquals(1, logAuditTrailUpdatedProductWebResponses.size());
  }

  @Test
  public void toPredefinedAllowedAttributeValueResponseTest() {
    List<PredefinedAllowedAttributeValueResponse> result =
        ResponseHelper.toPredefinedAllowedAttributeValueResponse(Arrays.asList(predefinedAttributeValueWebResponse));
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(Constants.OEM, result.get(0).getValue());
  }

  @Test
  public void toProductLevel3SummaryDetailsWebResponseTest() {
    productLevel3SummaryResponse.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    BeanUtils.copyProperties(productLevel3SummaryResponse, productLevel3SummaryDetailsResponse);
    productLevel3SummaryDetailsResponse.setSuspended(true);
    productLevel3SummaryDetailsResponse.setRejected(true);
    ProductLevel3SummaryDetailsImageResponse productLevel3SummaryDetailsImageResponse =
        new ProductLevel3SummaryDetailsImageResponse();
    productLevel3SummaryDetailsImageResponse.setLocationPath(DETAIL_URL);
    productLevel3SummaryDetailsImageResponse.setMainImage(true);
    productLevel3SummaryDetailsImageResponse.setMarkForDelete(false);
    productLevel3SummaryDetailsImageResponse.setReviewType("IMAGE");
    productLevel3SummaryDetailsResponse.setImages(Arrays.asList(productLevel3SummaryDetailsImageResponse));
    productLevel3SummaryDetailsResponse.setItemNumber("1");
    ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse =
        ResponseHelper.toProductLevel3SummaryDetailsWebResponse(productLevel3SummaryDetailsResponse);
    assertEquals(DETAIL_URL, productLevel3SummaryDetailsWebResponse.getImages().get(0).getLocationPath());
    assertEquals("IMAGE", productLevel3SummaryDetailsWebResponse.getImages().get(0).getReviewType());
    assertTrue(productLevel3SummaryDetailsWebResponse.getImages().get(0).isMainImage());
    assertEquals(ITEM_NAME, productLevel3SummaryDetailsWebResponse.getItemName());
    assertEquals(ITEM_SKU, productLevel3SummaryDetailsWebResponse.getItemSku());
    assertTrue(productLevel3SummaryDetailsWebResponse.isSuspended());
    assertTrue(productLevel3SummaryDetailsWebResponse.isRejected());
    assertEquals("1", productLevel3SummaryDetailsWebResponse.getItemNumber());
  }

  @Test
  public void toProductLevel3SummaryDetailsWebResponsePriceAndWholesaleConfigTest() {
    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    BeanUtils.copyProperties(productLevel3SummaryResponse, productLevel3SummaryDetailsResponse);
    productLevel3SummaryDetailsResponse.setPrices(new ArrayList<>());
    productLevel3SummaryDetailsResponse.getPrices().add(new ProductLevel3PriceResponse());
    productLevel3SummaryDetailsResponse.getPrices().get(0).setSalePrice(9000.0);
    productLevel3SummaryDetailsResponse.getPrices().get(0).setPrice(10000.0);
    productLevel3SummaryDetailsResponse.setWholesalePriceActivated(true);
    productLevel3SummaryDetailsResponse.setProductItemWholesalePrices(new ArrayList<>());
    productLevel3SummaryDetailsResponse.getProductItemWholesalePrices()
        .add(new ProductItemWholesalePriceResponse(2, 10));
    ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse =
        ResponseHelper.toProductLevel3SummaryDetailsWebResponse(productLevel3SummaryDetailsResponse);
    assertEquals(9000.0, productLevel3SummaryDetailsResponse.getPrices().get(0).getSalePrice(), 0.0);
    assertEquals(10000.0, productLevel3SummaryDetailsResponse.getPrices().get(0).getPrice(), 0.0);
    assertTrue(productLevel3SummaryDetailsWebResponse.getWholesalePriceActivated());
    assertEquals(2, productLevel3SummaryDetailsWebResponse.getProductItemWholesalePrices().get(0).getQuantity());
    assertEquals(10.0,
        productLevel3SummaryDetailsWebResponse.getProductItemWholesalePrices().get(0).getWholesaleDiscount(), 0.0);
  }

  @Test
  public void toProductLevel3SummaryDetailsWebResponseRejectedSuspendedTest() {
    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    BeanUtils.copyProperties(productLevel3SummaryResponse, productLevel3SummaryDetailsResponse);
    ProductLevel3SummaryDetailsImageResponse productLevel3SummaryDetailsImageResponse =
        new ProductLevel3SummaryDetailsImageResponse();
    productLevel3SummaryDetailsImageResponse.setLocationPath(DETAIL_URL);
    productLevel3SummaryDetailsImageResponse.setMainImage(true);
    productLevel3SummaryDetailsImageResponse.setMarkForDelete(false);
    productLevel3SummaryDetailsImageResponse.setReviewType("IMAGE");
    productLevel3SummaryDetailsResponse.setImages(Arrays.asList(productLevel3SummaryDetailsImageResponse));
    productLevel3SummaryDetailsResponse.setRejected(true);
    productLevel3SummaryDetailsResponse.setSuspended(true);
    productLevel3SummaryDetailsResponse.setWholesalePriceConfigEnabled(true);
    ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse =
        ResponseHelper.toProductLevel3SummaryDetailsWebResponse(productLevel3SummaryDetailsResponse);
    assertEquals(DETAIL_URL, productLevel3SummaryDetailsWebResponse.getImages().get(0).getLocationPath());
    assertEquals("IMAGE", productLevel3SummaryDetailsWebResponse.getImages().get(0).getReviewType());
    assertTrue(productLevel3SummaryDetailsWebResponse.getImages().get(0).isMainImage());
    assertEquals(ITEM_NAME, productLevel3SummaryDetailsWebResponse.getItemName());
    assertEquals(ITEM_SKU, productLevel3SummaryDetailsWebResponse.getItemSku());
    assertTrue(productLevel3SummaryDetailsWebResponse.isRejected());
    assertTrue(productLevel3SummaryDetailsWebResponse.isSuspended());
    assertTrue(productLevel3SummaryDetailsWebResponse.isWholesalePriceConfigEnabled());
  }

  @Test
  public void toProductLevel3SummaryDetailsWebResponseCategoryDetailsTest() {
    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    BeanUtils.copyProperties(productLevel3SummaryResponse, productLevel3SummaryDetailsResponse);
    ProductLevel3SummaryDetailsImageResponse productLevel3SummaryDetailsImageResponse =
        new ProductLevel3SummaryDetailsImageResponse();
    productLevel3SummaryDetailsImageResponse.setLocationPath(DETAIL_URL);
    productLevel3SummaryDetailsImageResponse.setMainImage(true);
    productLevel3SummaryDetailsImageResponse.setMarkForDelete(false);
    productLevel3SummaryDetailsImageResponse.setReviewType("IMAGE");
    productLevel3SummaryDetailsResponse.setImages(Arrays.asList(productLevel3SummaryDetailsImageResponse));
    productLevel3SummaryDetailsResponse.setCategoryCode(CATEGORY_CODE);
    productLevel3SummaryDetailsResponse.setCategoryName(CATEGORY_NAME);
    productLevel3SummaryDetailsResponse.setCategoryHierarchy(CATEGORY_HIERARCHY);
    ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse =
        ResponseHelper.toProductLevel3SummaryDetailsWebResponse(productLevel3SummaryDetailsResponse);
    assertEquals(DETAIL_URL, productLevel3SummaryDetailsWebResponse.getImages().get(0).getLocationPath());
    assertEquals("IMAGE", productLevel3SummaryDetailsWebResponse.getImages().get(0).getReviewType());
    assertTrue(productLevel3SummaryDetailsWebResponse.getImages().get(0).isMainImage());
    assertEquals(CATEGORY_CODE, productLevel3SummaryDetailsWebResponse.getCategoryCode());
    assertEquals(CATEGORY_NAME, productLevel3SummaryDetailsWebResponse.getCategoryName());
    assertEquals(CATEGORY_HIERARCHY, productLevel3SummaryDetailsWebResponse.getCategoryHierarchy());
  }

  @Test
  public void validateAgpQueryResponseTest() {
    AgpSimpleQueryResponse agpSimpleQueryResponse = new AgpSimpleQueryResponse();
    agpSimpleQueryResponse.setHits(new HitsResponse());
    boolean response = ResponseHelper.validateAgpQueryResponse(agpSimpleQueryResponse);
    Assertions.assertTrue(response);
  }

  @Test
  void validateAgpQueryHitResponseNullTest() {
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateAgpQueryResponse(new AgpSimpleQueryResponse()));
  }

  @Test
  void validateAgpQueryNullResponseTest() {
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateAgpQueryResponse(null));
  }

  @Test
  public void toDistinctPickUpPointTest() {
    pickupPointDTO.setCode(CODE);
    pickupPointDTO.setName(NAME);
    GeolocationDTO geolocation = new GeolocationDTO();
    geolocation.setLongitude(LONGITUDE);
    geolocation.setLatitude(LATITUDE);
    pickupPointDTO.setGeolocation(geolocation);
    DistinctPickUpPoint distinctPickUpPoint = ResponseHelper.toDistinctPickUpPoint(pickupPointDTO);
    Assertions.assertEquals(CODE, distinctPickUpPoint.getPickupPointCode());
    Assertions.assertEquals(NAME, distinctPickUpPoint.getPickupPointName());
    Assertions.assertEquals(LATITUDE, distinctPickUpPoint.getPinPoint().getLatitude());
    Assertions.assertEquals(LONGITUDE, distinctPickUpPoint.getPinPoint().getLongitude());
  }

  @Test
  public void toDistinctPickUpPointWithNullGeoLocationTest() {
    pickupPointDTO.setCode(CODE);
    pickupPointDTO.setName(NAME);
    pickupPointDTO.setGeolocation(null);
    DistinctPickUpPoint distinctPickUpPoint = ResponseHelper.toDistinctPickUpPoint(pickupPointDTO);
    Assertions.assertEquals(CODE, distinctPickUpPoint.getPickupPointCode());
    Assertions.assertEquals(NAME, distinctPickUpPoint.getPickupPointName());
    assertNull(distinctPickUpPoint.getPinPoint());
  }

  @Test
  public void toItemsPriceStockImagesUpdateWebResponseTest() {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setPostLive(true);
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(Arrays.asList(variantsErrorListResponse));
    ItemsPriceStockImagesUpdateWebResponse response =
        ResponseHelper.toItemsPriceStockImagesUpdateWebResponse(itemsPriceStockImagesUpdateResponse);
    Assertions.assertTrue(response.isProductReview());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertEquals(1, response.getVariantsErrorList().size());
    Assertions.assertEquals(ITEM_SKU, response.getVariantsErrorList().get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, response.getVariantsErrorList().get(0).getItemName());
    Assertions.assertEquals(ERROR_MESSAGE, response.getVariantsErrorList().get(0).getMessage());
    Assertions.assertEquals(ERROR_CODE, response.getVariantsErrorList().get(0).getCode());
  }

  @Test
  public void toItemsPriceStockImagesUpdateWebResponse_ErrorTest() {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        ItemsPriceStockImagesUpdateResponse.builder().apiErrorCode(PRODUCT_IS_TAKEN_DOWN).build();
    Assertions.assertThrows(ApplicationException.class,
        () -> ResponseHelper.toItemsPriceStockImagesUpdateWebResponse(
            itemsPriceStockImagesUpdateResponse));
  }

  @Test
  public void toHistorySummaryWebResponseTest() {
    HistoryResponse historyResponse =
        new HistoryResponse(new Date(), ITEM_SKU, ITEM_NAME, CHANGED_BY, PRODUCT_NAME, PRODUCT_NAME_2, ACTIVITY);
    List<HistorySummaryWebResponse> responses =
        ResponseHelper.toHistorySummaryWebResponse(Arrays.asList(historyResponse));
    assertEquals(ITEM_SKU, responses.get(0).getGdnSku());
    assertEquals(ITEM_NAME, responses.get(0).getGdnName());
    assertEquals(CHANGED_BY, responses.get(0).getChangedBy());
    assertEquals(PRODUCT_NAME, responses.get(0).getOldValues());
    assertEquals(PRODUCT_NAME_2, responses.get(0).getNewValues());
    assertEquals(ACTIVITY, responses.get(0).getActivity());

  }

  @Test
  public void toHistorySummaryWebResponse_EmptyTest() {
    HistoryResponse historyResponse =
        new HistoryResponse(new Date(), ITEM_SKU, ITEM_NAME, CHANGED_BY, null, "", ACTIVITY);
    List<HistorySummaryWebResponse> responses =
        ResponseHelper.toHistorySummaryWebResponse(Arrays.asList(historyResponse));
    assertEquals(ITEM_SKU, responses.get(0).getGdnSku());
    assertEquals(ITEM_NAME, responses.get(0).getGdnName());
    assertEquals(CHANGED_BY, responses.get(0).getChangedBy());
    assertEquals(Constants.DASH_SEPARATOR, responses.get(0).getOldValues());
    assertEquals(Constants.DASH_SEPARATOR, responses.get(0).getNewValues());
    assertEquals(ACTIVITY, responses.get(0).getActivity());

  }

  @Test
  public void toPickupPointUpdateWebResponseTest() {
    PickupPointUpdateWebResponse pickupPointUpdateWebResponse =
        ResponseHelper.toPickupPointUpdateWebResponse(pickupPointUpdateResponse);
    Assertions.assertEquals(ITEM_SKU, pickupPointUpdateWebResponse.getVariantsErrorList().get(0).getItemSku());
    Assertions.assertEquals(ERROR_CODE, pickupPointUpdateWebResponse.getVariantsErrorList().get(0).getCode());
    Assertions.assertEquals(ERROR_MESSAGE, pickupPointUpdateWebResponse.getVariantsErrorList().get(0).getMessage());
  }

  @Test
  public void toPickupPointUpdateWebResponseEmptyListTest() {
    pickupPointUpdateResponse.setResponses(null);
    PickupPointUpdateWebResponse pickupPointUpdateWebResponse =
        ResponseHelper.toPickupPointUpdateWebResponse(pickupPointUpdateResponse);
    Assertions.assertEquals(0, pickupPointUpdateWebResponse.getVariantsErrorList().size());
  }

  @Test
  public void toProductItemNameWebResponseTest() {
    ProductItemNameResponse productItemNameResponse =
        ProductItemNameResponse.builder().itemSku(ITEM_SKU).itemName(ITEM_NAME).build();
    List<ProductItemNameWebResponse> productItemNameWebResponses =
        ResponseHelper.toProductItemNameWebResponse(Arrays.asList(productItemNameResponse));
    Assertions.assertEquals(ITEM_SKU, productItemNameWebResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, productItemNameWebResponses.get(0).getItemName());
  }

  @Test
  public void toProductLevel3DetailWebResponseTest() {
    productLevel3DetailResponse.setVersion(VERSION);
    productLevel3DetailResponse.setPreOrder(preOrderResponse);
    ProductLevel3DetailWebResponse response =
        ResponseHelper.toProductLevel3DetailWebResponse(productLevel3DetailResponse);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getImages());
    assertEquals(response.getImages().get(0).getMainImage(), true);
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
  }

  @Test
  public void toProductLevel3DetailWebResponseNonNullL3ResponseTest() {
    productLevel3DetailResponse.setVersion(VERSION);
    productLevel3DetailResponse.setProductL3Response(new ProductL3Response());
    productLevel3DetailResponse.setPreOrder(preOrderResponse);
    ProductLevel3DetailWebResponse response =
        ResponseHelper.toProductLevel3DetailWebResponse(productLevel3DetailResponse);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getImages());
    assertEquals(response.getImages().get(0).getMainImage(), true);
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
  }

  @Test
  public void toProductLevel3DetailWebResponseEmptyListTest() {
    productLevel3DetailResponse.setVersion(VERSION);
    productLevel3DetailResponse.setAttributes(null);
    productLevel3DetailResponse.setProductScore(null);
    productLevel3DetailResponse.setImages(null);
    productLevel3DetailResponse.setProductLevel3Logistics(null);
    ProductLevel3DetailWebResponse response =
        ResponseHelper.toProductLevel3DetailWebResponse(productLevel3DetailResponse);
    assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getAttributes()));
    assertTrue(CollectionUtils.isEmpty(response.getImages()));
    assertTrue(CollectionUtils.isEmpty(response.getProductLevel3Logistics()));
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(VERSION, response.getVersion());
  }

  @Test
  public void validateResponseForErrorCode_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode((GdnRestSingleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnRestSingleResponse_ValueNullTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnRestSingleResponse_SuccessTrueTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, RESPONSE, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponseForErrorCode(gdnRestSingleResponse));
  }

  @Test
  public void validateResponseForErrorCode_PickupPointResponse_GdnRestSingleResponse_SuccessFalseTest() {
    PickupPointUpdateResponse response = new PickupPointUpdateResponse();
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, response, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_PickupPointResponseErrorCode_GdnRestSingleResponse_SuccessFalseTest() {
    PickupPointUpdateResponse response = new PickupPointUpdateResponse();
    response.setApiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED);
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, response, REQUEST_ID);
    ApplicationException exception = new ApplicationException();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnRestSingleResponse);
    } catch (ApplicationException e) {
      exception = e;
    } finally {
      assertEquals(ApplicationException.class, exception.getClass());
      assertEquals(ApiErrorCode.ITEM_IS_SUSPENDED.getDesc(), exception.getMessage());
      assertEquals(ApiErrorCode.ITEM_IS_SUSPENDED.getCode(), exception.getCode());
      assertEquals(HttpStatus.BAD_REQUEST, exception.getHttpStatus());
    }
  }

  @Test
  public void vvalidateResponseForErrorCode_GdnBaseRestResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode((GdnBaseRestResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnBaseRestResponse_nullErrorrMessage_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(null, null, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCode(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertNull(exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCode_GdnBaseRestResponse_SuccessTrueTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponseForErrorCode(gdnBaseRestResponse));
  }

  @Test
  public void validateEditInfoResponseForErrorCode_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateEditInfoResponse(null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateEditInfoResponseForErrorCode_GdnRestListResponse_SuccessTrueTest() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, new EditProductV2Response(), REQUEST_ID);
    assertNotNull(ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }

  @Test
  public void validateEditInfoResponse_nullErrorrMessage_SuccessFalseTest() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, null, false, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateEditInfoResponse_validationError() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ErrorCategory.INVALID_STATE.getCode(), false, null,
            REQUEST_ID);
    Assertions.assertThrows(ApplicationException.class,
        () -> ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }

  @Test
  void validateEditInfoResponse_ErrorCode_SuccessTrueTest() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }

  @Test
  public void validateEditInfoResponse_GdnRestSingleResponse_SuccessTrueTest() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, new EditProductV2Response(), REQUEST_ID);
    assertNotNull(ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }

  @Test
  void validateEditInfoResponse_GdnRestSingleResponse_SuccessFalseExceptionTest() {
    EditProductV2Response editProductResponse = new EditProductV2Response();
    editProductResponse.setApiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED);
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, editProductResponse, REQUEST_ID);
    Assertions.assertThrows(ApplicationException.class,
        () -> ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }

  @Test
  public void toProductL3CountWebResponseTest_forActiveProducts() {
    ProductCountResponse productCountResponse = new ProductCountResponse(40L, 20L, 40L, 50L);
    ProductL3CountWebResponse productL3CountWebResponse = ResponseHelper
        .toProductL3CountWebResponse(productCountResponse, countProductLevel3WipResponse, BUSINESS_PARTNER_CODE);
    assertNotNull(productL3CountWebResponse);
    assertEquals(40L, productL3CountWebResponse.getActive().longValue());
    assertEquals(20L, productL3CountWebResponse.getOutOfStock().longValue());
    assertEquals(40L, productL3CountWebResponse.getNeedCorrection().longValue());
    assertEquals(50L, productL3CountWebResponse.getInReview().longValue());
    assertEquals(BUSINESS_PARTNER_CODE, productL3CountWebResponse.getBusinessPartnerCode());
  }

  @Test
  public void toProductL3CountWebResponseByTypeTest_forActiveProducts() {
    ProductCountResponse productCountResponse = new ProductCountResponse(40L, 20L, 40L, 50L);
    ProductL3CountWebResponse productL3CountWebResponse = ResponseHelper
        .toProductL3CountWebResponseByType(productCountResponse, BUSINESS_PARTNER_CODE, Constants.ACTIVE_STATUS, null);
    assertNotNull(productL3CountWebResponse);
    assertEquals(40L, productL3CountWebResponse.getActive().longValue());
    assertEquals(20L, productL3CountWebResponse.getOutOfStock().longValue());
    assertEquals(60L, productL3CountWebResponse.getAll().longValue());
    assertEquals(BUSINESS_PARTNER_CODE, productL3CountWebResponse.getBusinessPartnerCode());
  }

  @Test
  public void toProductL3CountWebResponseByTypeTest_forInActiveProducts() {
    ProductLevel3CountResponse countProductLevel3WipResponse = new ProductLevel3CountResponse();
    Map<ProductLevel3SummaryCriteria, Long> totalItemsByCriterias = new HashMap();
    totalItemsByCriterias.put(ProductLevel3SummaryCriteria.REJECTED, 60L);
    countProductLevel3WipResponse.setTotalItemsByCriterias(totalItemsByCriterias);
    countProductLevel3WipResponse.setTotalItems(150L);
    ProductCountResponse productCountResponse = new ProductCountResponse(40L, 20L, 40L, 50L);
    ProductL3CountWebResponse productL3CountWebResponse =
        ResponseHelper.toProductL3CountWebResponseByType(productCountResponse, BUSINESS_PARTNER_CODE,
            Constants.INACTIVE_STATUS, countProductLevel3WipResponse);
    assertNotNull(productL3CountWebResponse);
    assertEquals(40L, productL3CountWebResponse.getSuspended().longValue());
    assertEquals(50L, productL3CountWebResponse.getArchived().longValue());
    assertEquals(60L, productL3CountWebResponse.getRejected().longValue());
    assertEquals(BUSINESS_PARTNER_CODE, productL3CountWebResponse.getBusinessPartnerCode());
  }

  @Test
  public void toProductL3CountWebResponseTest_forInActiveProducts() {
    ProductCountResponse productCountResponse = new ProductCountResponse(40L, 20L, 40L, 50L);
    ProductL3CountWebResponse productL3CountWebResponse = ResponseHelper
        .toProductL3CountWebResponse(productCountResponse, countProductLevel3WipResponse,
            countProductLevel3InactiveResponse, BUSINESS_PARTNER_CODE);
    assertNotNull(productL3CountWebResponse);
    assertEquals(40L, productL3CountWebResponse.getActive().longValue());
    assertEquals(20L, productL3CountWebResponse.getOutOfStock().longValue());
    assertEquals(40L, productL3CountWebResponse.getNeedCorrection().longValue());
    assertEquals(50L, productL3CountWebResponse.getInReview().longValue());
    assertEquals(40L, productL3CountWebResponse.getSuspended().longValue());
    assertEquals(50L, productL3CountWebResponse.getArchived().longValue());
    assertEquals(60L, productL3CountWebResponse.getRejected().longValue());
    assertEquals(BUSINESS_PARTNER_CODE, productL3CountWebResponse.getBusinessPartnerCode());
  }

  @Test
  public void validateEditInfoResponse_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, new EditProductV2Response(), REQUEST_ID);
    assertNotNull(ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse));
  }


  @Test
  public void validateEditInfoResponse_GdnRestSingleResponse_appealProductLimitReached() {
    GdnRestSingleResponse<EditProductV2Response> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false,
            EditProductV2Response.builder().apiErrorCode(ApiErrorCode.APPEAL_LIMIT_CROSSED)
                .build(), REQUEST_ID);
    try {
      ResponseHelper.validateEditInfoResponse(gdnRestSingleResponse);
    }
    catch (ApplicationException e){
      Assertions.assertEquals(ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode(), e.getCode());
    }
  }

  @Test
  public void campaignCalidateResponse_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(
          (GdnRestSingleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void campaignValidateResponse_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> gdnRestSingleResponse =
      new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE1, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void campaignValidateResponse_GdnRestSingleResponse_SuccessFalseApiIncorrectExceptionTest() {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, PRODUCT_NAME_IS_EMPTY.getCode(), false, RESPONSE1, REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = new ApiIncorrectInputDataException();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(apiIncorrectInputDataException.getClass(), ApiIncorrectInputDataException.class);
      assertEquals(ERROR_MESSAGE, apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessFalseApiIncorrectExceptionTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, PRODUCT_NAME_IS_EMPTY.getCode(), false, REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = new ApiIncorrectInputDataException();
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(apiIncorrectInputDataException.getClass(), ApiIncorrectInputDataException.class);
      assertEquals(ERROR_MESSAGE, apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void campaignValidateResponse_GdnRestSingleResponse_ValueNullTest () {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> gdnRestSingleResponse =
      new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void campaignVvalidateResponse_GdnRestSingleResponse_SuccessTrueTest () {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> gdnRestSingleResponse =
      new GdnRestSingleResponse<ProductCampaignAvailabilityResponse>(ERROR_MESSAGE, ERROR_CODE,
        true, RESPONSE1, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestSingleResponse));
  }

  @Test
  public void validateL4ListingWebResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.toItemLevel4ListingWebResponse(
        (GdnRestListResponse<ItemLevel4ListingResponse>) null, null, "");
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(NullPointerException.class, exception.getClass());
    }
  }

  @Test
  public void fromItemSummaryL4ResponseToItemLevel4ListingWebResponseTest() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_SKU).bundleRecipeEditable(true).build();
    ItemSummaryL4Response itemSummaryL4Response = new ItemSummaryL4Response();
    itemSummaryL4Response.setItemSku(ITEM_SKU);
    itemSummaryL4Response.setItemCode(ITEM_SKU);
    itemSummaryL4Response.setProductSku(PRODUCT_SKU);
    itemSummaryL4Response.setSellerSku(PRODUCT_SKU);
    itemSummaryL4Response.setUpcCode(UPC_CODE);
    itemSummaryL4Response.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemSummaryL4Response.setCategoryCode(CATEGORY_CODE);
    itemSummaryL4Response.setGeneratedItemName(NAME);
    itemSummaryL4Response.setMainImageUrl(URL);
    itemSummaryL4Response.setBrand(BRAND);
    List<ItemLevel4ListingWebResponse> itemLevel4ListingWebResponses = ResponseHelper
        .fromItemSummaryL4ResponseToItemLevel4ListingWebResponse(
            new GdnRestListResponse<>(Collections.singletonList(itemSummaryL4Response), null, null),
            new GdnRestListResponse<>(Collections.singletonList(productBundleRecipeEditableResponseContent),
                null, null));
    Assertions.assertEquals(ITEM_SKU, itemLevel4ListingWebResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU, itemLevel4ListingWebResponses.get(0).getItemCode());
    Assertions.assertEquals(PRODUCT_SKU, itemLevel4ListingWebResponses.get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_SKU, itemLevel4ListingWebResponses.get(0).getSellerSku());
    Assertions.assertEquals(UPC_CODE, itemLevel4ListingWebResponses.get(0).getUpcCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, itemLevel4ListingWebResponses.get(0).getMerchantCode());
    Assertions.assertEquals(CATEGORY_CODE, itemLevel4ListingWebResponses.get(0).getCategoryCode());
    Assertions.assertEquals(NAME, itemLevel4ListingWebResponses.get(0).getGeneratedItemName());
    Assertions.assertEquals(URL, itemLevel4ListingWebResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(BRAND, itemLevel4ListingWebResponses.get(0).getBrand());
  }

  @Test
  public void validateL4ListingWebResponse_SuccessFalseTest() {
    ItemLevel4ListingResponse content =
      ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).brand(BRAND).itemCode(ITEM_SKU)
        .build();
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_SKU).bundleRecipeEditable(true).build();

    GdnRestListResponse<ItemLevel4ListingResponse> response =
      new GdnRestListResponse<ItemLevel4ListingResponse>(ERROR_MESSAGE, ERROR_CODE, false,
        Arrays.asList(content), new PageMetaData(0, 10, 100), REQUEST_ID);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>(ERROR_MESSAGE, ERROR_CODE, false,
            Arrays.asList(productBundleRecipeEditableResponseContent),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.toItemLevel4ListingWebResponse(response, productAssemblyResponse, "");
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(Exception.class, exception.getClass());
    }
  }

  @Test
  public void validateL4ListingWebResponse_ValueNullTest() {
    GdnRestListResponse<ItemLevel4ListingResponse> response =
      new GdnRestListResponse<ItemLevel4ListingResponse>(ERROR_MESSAGE, ERROR_CODE, true, null,
        new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(response);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
    }
  }

  @Test
  public void validateL4ListingWebResponse_SuccessTrueTest() {
    ItemLevel4ListingResponse content =
      ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).brand(BRAND).itemSku(ITEM_SKU)
        .build();
    GdnRestListResponse<ItemLevel4ListingResponse> response =
      new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true, Arrays.asList(content),
        new PageMetaData(0, 10, 100), REQUEST_ID);
    assertTrue(Objects.nonNull(response));
    assertTrue(CollectionUtils.isNotEmpty(response.getContent()));
  }

  @Test
  public void validateL4ListingWebResponse_SuccessAndContentTrueTest() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_SKU).bundleRecipeEditable(true).build();
    ItemLevel4ListingResponse content =
      ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).brand(BRAND).itemCode(ITEM_SKU).build();
    content.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    List<ItemLevel4ListingResponse> itemLevel4ListingResponse = new ArrayList<>();
    itemLevel4ListingResponse.add(content);
    GdnRestListResponse<ItemLevel4ListingResponse> response =
        new GdnRestListResponse<>(null, null, true, itemLevel4ListingResponse, new PageMetaData(0, 10, 100),
            REQUEST_ID);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>(ERROR_MESSAGE, ERROR_CODE, false,
            Arrays.asList(productBundleRecipeEditableResponseContent),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    assertNotNull(response);
    List<ItemLevel4ListingWebResponse> webResponses =
        ResponseHelper.toItemLevel4ListingWebResponse(response, productAssemblyResponse, "");
    assertTrue(CollectionUtils.isNotEmpty(response.getContent()));
    assertTrue(response.getContent().get(0).getClass().equals(ItemLevel4ListingResponse.class));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getItemSku(), webResponses.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.getContent().get(0).getPickupPointCodes().get(0));

  }

  @Test
  public void toItemLevel4ListingWebResponse_ContentTest() {
    GdnRestListResponse<ItemLevel4ListingResponse> response =
      new GdnRestListResponse<ItemLevel4ListingResponse>(ERROR_MESSAGE, ERROR_CODE, true,
        new ArrayList<>(), new PageMetaData(0, 10, 100), REQUEST_ID);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>(ERROR_MESSAGE, ERROR_CODE, false,
            new ArrayList<>(), new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    List<ItemLevel4ListingWebResponse> webResponses = null;
    try {
      webResponses = ResponseHelper.toItemLevel4ListingWebResponse(response, productAssemblyResponse, "");
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertTrue(CollectionUtils.isEmpty(response.getContent()));
      assertEquals(Exception.class, exception.getClass());
      assertNotNull(webResponses);
      assertTrue(webResponses.isEmpty());
    }
  }
  @Test
  public void toItemLevel4ListingWebResponse() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_SKU).bundleRecipeEditable(true).build();
    ItemLevel4ListingResponse content =
      ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).brand(BRAND).itemCode(ITEM_SKU)
        .build();
    BundleItemResponse bundleItemResponse1 = new BundleItemResponse();
    BundleItemResponse bundleItemResponse2 = new BundleItemResponse();
    bundleItemResponse2.setProductStatus("ACTIVE");
    content.setBundleItemResponses(Arrays.asList(bundleItemResponse1, bundleItemResponse2));
    List<ItemLevel4ListingResponse> itemLevel4ListingResponse = new ArrayList<>();
    itemLevel4ListingResponse.add(content);
    List<ItemLevel4ListingWebResponse> webResponses = new ArrayList<>();
    GdnRestListResponse<ItemLevel4ListingResponse> clientResponse =
      new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true,
        itemLevel4ListingResponse, new PageMetaData(0, 10, 100), REQUEST_ID);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>(ERROR_MESSAGE, ERROR_CODE, false,
            Arrays.asList(productBundleRecipeEditableResponseContent),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    try {
      ResponseHelper.validateResponse(clientResponse);
      webResponses = ResponseHelper. toItemLevel4ListingWebResponse(clientResponse, productAssemblyResponse, "");
    } finally {
      Assertions.assertTrue(CollectionUtils.isNotEmpty(itemLevel4ListingResponse));
      assertNotNull(webResponses);
      Assertions.assertTrue(CollectionUtils.isNotEmpty(webResponses));
    }
  }

  @Test
  public void toProductL3DetailWebResponseTest() {
    ProductL3CommonImageResponse commonImageResponse = new ProductL3CommonImageResponse();
    commonImageResponse.setActiveLocation(true);
    commonImageResponse.setLocationPath("/sample/path");
    commonImageResponse.setMainImage(true);
    ProductLevel3AttributeResponse productLevel3AttributeResponse1 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse1.setAttributeCode(ATTRIBUTE_ID);
    productLevel3AttributeResponse1.setVariantCreation(true);
    productL3DetailsResponse.setAttributes(Collections.singletonList(productLevel3AttributeResponse1));
    productL3DetailsResponse.setVersion(VERSION);
    productL3DetailsResponse.setPreOrder(preOrderResponse);
    productL3DetailsResponse.setCommonImages(Arrays.asList(commonImageResponse));
    productL3DetailsResponse.setActiveL5Mapped(true);
    productL3DetailsResponse.setCategoryNameEnglish(CATEGORY_NAME_ENGLISH_1);
    productL3DetailsResponse.setCategoryHierarchyEnglish(CATEGORY_HIERARCHY_ENGLISH);
    productL3DetailsResponse.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    productL3DetailsResponse.setFbbPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE_1));
    productL3DetailsResponse.setSizeChartCode(SIZE_CHART_CODE);
    productL3DetailsResponse.setVideoUrl("videoUrl");
    productL3DetailsResponse.setCoverImagePath("coverImagePath");
    DistributionInfo distributionInfo = new DistributionInfo();
    distributionInfo.setCategoryName(CATEGORY_NAME);
    distributionInfo.setProductName(PRODUCT_NAME);
    productL3DetailsResponse.setDistributionInfoResponse(distributionInfo);
    ProductL3DetailWebResponse response =
      ResponseHelper.toProductL3DetailWebResponse(productL3DetailsResponse, null, true);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getCommonImages());
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(PICKUP_POINT_CODE, response.getPickupPointCodes().get(0));
    assertEquals(PICKUP_POINT_CODE_1, response.getFbbPickupPointCodes().get(0));
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
    assertEquals(CATEGORY_NAME_ENGLISH_1, response.getCategoryNameEnglish());
    assertEquals(CATEGORY_HIERARCHY_ENGLISH, response.getCategoryHierarchyEnglish());
    Assertions.assertTrue(response.isActiveL5Mapped());
    Assertions.assertEquals(SIZE_CHART_CODE, response.getSizeChartCode());
    Assertions.assertEquals("videoUrl", response.getVideoUrl());
    Assertions.assertEquals("coverImagePath", response.getCoverImagePath());
  }

  @Test
  public void toPickupPointSummaryWebResponseTest() {
    PickupPointOutboundResponse pickupPointResponse =
        new PickupPointOutboundResponse();
    GeolocationDTO geolocationDTO = new GeolocationDTO();
    pickupPointResponse.setGeolocation(geolocationDTO);
    FlagDTO flags = new FlagDTO();
    pickupPointResponse.setFlags(flags);

    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponse(Arrays.asList(pickupPointResponse));

    assertNotNull(pickupPointUpdateWebResponseList.get(0).getGeolocation());
  }

  @Test
  public void toPickupPointSummaryWebResponseGeolocationNullTest() {
    PickupPointOutboundResponse pickupPointResponse =
        new PickupPointOutboundResponse();
    pickupPointResponse.setCncActivated(true);
    FlagDTO flags = new FlagDTO();
    pickupPointResponse.setFlags(flags);
    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponse(Arrays.asList(pickupPointResponse));

    assertNull(pickupPointUpdateWebResponseList.get(0).getGeolocation());
  }

  @Test
  public void toPickupPointSummaryWebResponseCncActivatedTrueTest() {
    PickupPointOutboundResponse pickupPointResponse =
        new PickupPointOutboundResponse();
    FlagDTO flags = new FlagDTO();
    pickupPointResponse.setFlags(flags);
    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponse(Arrays.asList(pickupPointResponse));
    Assertions.assertEquals(1, pickupPointUpdateWebResponseList.size());
  }

  @Test
  public void toPickupPointSummaryWebResponseCncActivatedTrueCncInResponseIsTrueTest() {
    PickupPointOutboundResponse pickupPointResponse =
        new PickupPointOutboundResponse();
    pickupPointResponse.setCncActivated(true);
    FlagDTO flags = new FlagDTO();
    pickupPointResponse.setFlags(flags);
    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponse(Arrays.asList(pickupPointResponse));
    assertNull(pickupPointUpdateWebResponseList.get(0).getGeolocation());
  }

  @Test
  public void toPickupPointSummaryWebResponseFromBusinessPartnerPickupPointTest() {
    BusinessPartnerPickupPointOutboundResponse businessPartnerPickupPointResponse =
        new BusinessPartnerPickupPointOutboundResponse();
    GeolocationDTO geoLocationResponse = new GeolocationDTO();
    businessPartnerPickupPointResponse.setGeolocation(geoLocationResponse);

    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponseFromBusinessPartnerPickupPoint(
            Arrays.asList(businessPartnerPickupPointResponse));

    assertNotNull(pickupPointUpdateWebResponseList.get(0).getGeolocation());
  }

  @Test
  public void toPickupPointSummaryWebResponseFromBusinessPartnerPickupPointGeolocationNullTest() {
    BusinessPartnerPickupPointOutboundResponse businessPartnerPickupPointResponse =
        new BusinessPartnerPickupPointOutboundResponse();

    List<PickupPointSummaryWebResponse> pickupPointUpdateWebResponseList =
        ResponseHelper.toPickupPointSummaryWebResponseFromBusinessPartnerPickupPoint(
            Arrays.asList(businessPartnerPickupPointResponse));

    assertNull(pickupPointUpdateWebResponseList.get(0).getGeolocation());
  }

 @Test
  public void toPickupPointDetailWebResponseTest() {
    List<PickupPointDetailWebResponse> response = ResponseHelper.toPickupPointDetailWebResponse(
      Collections.singletonList(pickupPointDetailResponse));
    Assertions.assertEquals(PICKUP_POINT_CODE, response.get(0).getPickupPointCode());
    Assertions.assertEquals(PICK_UP_POINT_NAME, response.get(0).getPickupPointName());
  }

  @Test
  public void toHistoryUpdateWebResponseListTest() {
    List<HistoryUpdateWebResponse> historyUpdateWebResponseList =
      ResponseHelper.toHistoryUpdateWebResponseList(
        Collections.singletonList(historyUpdateResponse));
    Assertions.assertEquals(PICK_UP_POINT_NAME,
      historyUpdateWebResponseList.get(0).getPickupPointName());
    Assertions.assertEquals(PRODUCT_NAME, historyUpdateWebResponseList.get(0).getOldValues());
    Assertions.assertEquals(PRODUCT_NAME_2, historyUpdateWebResponseList.get(0).getNewValues());
    Assertions.assertEquals(ITEM_SKU, historyUpdateWebResponseList.get(0).getGdnSku());
  }

  @Test
  public void toWholesalePromoV2ResponseListTest() {
    List<WholesalePromoV2Response> wholesalePromoV2Responses =
      ResponseHelper.toWholesalePromoV2ResponseList(
        Collections.singletonList(wholesalePriceSkuResponse));
    Assertions.assertEquals(ITEM_SKU, wholesalePromoV2Responses.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, wholesalePromoV2Responses.get(0).getPickupPointCode());
    Assertions.assertTrue(wholesalePromoV2Responses.get(0).isWholesalePromo());
  }

  @Test
  public void toWholesalePromoV2ResponseListT_promoNullTest() {
    wholesalePriceSkuResponse.setPromoActive(null);
    List<WholesalePromoV2Response> wholesalePromoV2Responses =
      ResponseHelper.toWholesalePromoV2ResponseList(
        Collections.singletonList(wholesalePriceSkuResponse));
    Assertions.assertEquals(ITEM_SKU, wholesalePromoV2Responses.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, wholesalePromoV2Responses.get(0).getPickupPointCode());
    assertFalse(wholesalePromoV2Responses.get(0).isWholesalePromo());
  }

  @Test
  public void toItemPickupPointListingL3WebResponseTest() {
    itemPickupPointListingL3Response.getImages().get(0).setMarkForDelete(true);
    itemPickupPointListingL3Response.setPickupPointCncActive(true);
    Set<String> activePromoBundling = new HashSet<>();
    activePromoBundling.add(Constants.WHOLESALE_PRICE);
    activePromoBundling.add(COMPLEMENTARY_COMBO_PROMO);
    itemPickupPointListingL3Response.setActivePromoBundlings(activePromoBundling);
    List<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponseList =
        ResponseHelper.toItemPickupPointListingL3WebResponse(Arrays.asList(itemPickupPointListingL3Response),
          new HashMap<>(), baseUrlForPdpWithL5, false);
    Assertions.assertEquals(PRODUCT_CODE, itemPickupPointListingL3WebResponseList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingL3WebResponseList.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingL3WebResponseList.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, itemPickupPointListingL3WebResponseList.get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getSalePrice(), 0);
    Assertions.assertEquals(10.0,
        itemPickupPointListingL3WebResponseList.get(0).getProductItemWholesalePrices().get(0).getWholesaleDiscount(),
        0);
    Assertions.assertEquals(10,
        itemPickupPointListingL3WebResponseList.get(0).getProductItemWholesalePrices().get(0).getQuantity());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getImages().get(0).isMarkForDelete());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).isPickupPointCncActive());
    assertFalse(itemPickupPointListingL3WebResponseList.stream()
      .map(ItemPickupPointListingL3WebResponse::getActivePromoBundlings).flatMap(Collection::stream)
      .anyMatch(i -> i.equals(Constants.WHOLESALE_PRICE)));
  }

  @Test
  public void toItemPickupPointListingL3WebResponseB2bFieldTest() {
    itemPickupPointListingL3Response.getImages().get(0).setMarkForDelete(true);
    itemPickupPointListingL3Response.setPickupPointCncActive(true);
    itemPickupPointListingL3Response.setB2bFields(new B2BResponse(true, 1000.0));
    itemPickupPointListingL3Response.getViewConfigs().stream()
        .forEach(viewconf -> viewconf.setChannelId(Constants.DEFAULT_CHANNEL));
    itemPickupPointListingL3Response.getViewConfigs().stream()
        .forEach(viewconf -> viewconf.setBuyableScheduleResponse(buyableScheduleResponse));
    itemPickupPointListingL3Response.getViewConfigs().stream()
        .forEach(viewconf -> viewconf.setDiscoverableScheduleResponse(discoverableScheduleResponse));
    Set<String> activePromoBundling = new HashSet<>();
    activePromoBundling.add(Constants.WHOLESALE_PRICE);
    activePromoBundling.add(COMPLEMENTARY_COMBO_PROMO);
    itemPickupPointListingL3Response.setActivePromoBundlings(activePromoBundling);
    List<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponseList =
        ResponseHelper.toItemPickupPointListingL3WebResponse(Arrays.asList(itemPickupPointListingL3Response),
          new HashMap<>(), baseUrlForPdpWithL5, true);
    Assertions.assertEquals(PRODUCT_CODE, itemPickupPointListingL3WebResponseList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingL3WebResponseList.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingL3WebResponseList.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, itemPickupPointListingL3WebResponseList.get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getSalePrice(), 0);
    Assertions.assertEquals(10.0,
        itemPickupPointListingL3WebResponseList.get(0).getProductItemWholesalePrices().get(0).getWholesaleDiscount(),
        0);
    Assertions.assertEquals(10,
        itemPickupPointListingL3WebResponseList.get(0).getProductItemWholesalePrices().get(0).getQuantity());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getImages().get(0).isMarkForDelete());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).isPickupPointCncActive());
    assertFalse(itemPickupPointListingL3WebResponseList.stream()
        .map(ItemPickupPointListingL3WebResponse::getActivePromoBundlings).flatMap(Collection::stream)
        .anyMatch(i -> i.equals(Constants.WHOLESALE_PRICE)));
    Assertions.assertEquals(1000, itemPickupPointListingL3WebResponseList.get(0).getB2bFields().getBasePrice(), 0);
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getB2bFields().isManaged());
    Assertions.assertEquals(TEN_DAYS_AGO, itemPickupPointListingL3WebResponseList
        .get(0).getViewConfigs().get(0).getBuyableSchedule().getStartDateTime());
    Assertions.assertEquals(TEN_DAYS_AFTER, itemPickupPointListingL3WebResponseList
        .get(0).getViewConfigs().get(0).getDiscoverableSchedule().getEndDateTime());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList
        .get(0).getViewConfigs().get(0).getBuyableSchedule().isBuyable());

  }

  @Test
  public void toItemPickupPointListingL3WebResponse_CFCountTest() {
    itemPickupPointListingL3Response.getImages().get(0).setMarkForDelete(true);
    itemPickupPointListingL3Response.setPickupPointCncActive(true);
    itemPickupPointListingL3Response.setB2bFields(new B2BResponse(true, 1000.0));
    Set<String> activePromoBundling = new HashSet<>();
    Map<String, Integer> itemSkuXCfCountMap = new HashMap<>();
    itemSkuXCfCountMap.put(ITEM_SKU,5);
    activePromoBundling.add(Constants.WHOLESALE_PRICE);
    activePromoBundling.add(COMPLEMENTARY_COMBO_PROMO);
    itemPickupPointListingL3Response.setActivePromoBundlings(activePromoBundling);
    List<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponseList =
      ResponseHelper.toItemPickupPointListingL3WebResponse(Arrays.asList(itemPickupPointListingL3Response),
        itemSkuXCfCountMap, baseUrlForPdpWithL5, false);
    Assertions.assertEquals(Optional.of(5).get(),
      itemPickupPointListingL3WebResponseList.stream().map(ItemPickupPointListingL3WebResponse::getInProgressConsignmentCount).findFirst().get());
  }


  @Test
  public void toItemPickupPointListingL3WebResponseWholesalePricesEmptyTest() {
    itemPickupPointListingL3Response.setProductItemWholesalePrices(new ArrayList<>());
    List<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponseList =
        ResponseHelper.toItemPickupPointListingL3WebResponse(Arrays.asList(itemPickupPointListingL3Response),
          new HashMap<>(), baseUrlForPdpWithL5, true);
    Assertions.assertEquals(PRODUCT_CODE, itemPickupPointListingL3WebResponseList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingL3WebResponseList.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingL3WebResponseList.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, itemPickupPointListingL3WebResponseList.get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0, itemPickupPointListingL3WebResponseList.get(0).getPrices().get(0).getSalePrice(), 0);
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertTrue(itemPickupPointListingL3WebResponseList.get(0).getProductItemWholesalePrices().isEmpty());
  }

  @Test
  public void toBusinessPartnerPickupPointWebResponseTest() {
    List<BusinessPartnerPickupPointOutboundResponse> responseList = Collections.singletonList(
        BusinessPartnerPickupPointOutboundResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .geolocation(new GeolocationDTO("placeId", LONGITUDE, LATITUDE, "streetAddress"))
        .build());
    List<BusinessPartnerPickupPointWebResponse> businessPartnerPickupPointWebResponses =
      new ArrayList<>();
    List<BusinessPartnerPickupPointWebResponse> webResponses =
      ResponseHelper.toBusinessPartnerPickupPointWebResponse(responseList);
    assertNotNull(webResponses);
    BusinessPartnerPickupPointOutboundResponse pickupPointResponse = responseList.get(0);
    BusinessPartnerPickupPointWebResponse webResponse = webResponses.get(0);
    Assertions.assertEquals(pickupPointResponse.getBusinessPartnerCode(), webResponse.getBusinessPartnerCode());
    Assertions.assertEquals(pickupPointResponse.getGeolocation().getLatitude(), webResponse.getGeoLocationWebResponse().getLatitude());
    Assertions.assertEquals(pickupPointResponse.getGeolocation().getStreetAddress(), webResponse.getGeoLocationWebResponse().getStreetAddress());
  }

  @Test
  public void toBusinessPartnerPickupPointWebResponseTest1() {
    List<BusinessPartnerPickupPointOutboundResponse> responseList = Collections.singletonList(
        BusinessPartnerPickupPointOutboundResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).fbbActivated(false)
            .build());
    List<BusinessPartnerPickupPointWebResponse> webResponses =
        ResponseHelper.toBusinessPartnerPickupPointWebResponse(responseList);
    assertNotNull(webResponses);
    BusinessPartnerPickupPointOutboundResponse pickupPointResponse = responseList.get(0);
    BusinessPartnerPickupPointWebResponse webResponse = webResponses.get(0);
    assertFalse(webResponse.isFbbActivated());
    Assertions.assertEquals(pickupPointResponse.isFbbActivated(), webResponse.isFbbActivated());
  }

  @Test
  public void toBusinessPartnerPickupPointWebResponseTestDelivery() {
    List<BusinessPartnerPickupPointOutboundResponse> responseList = Collections.singletonList(
        BusinessPartnerPickupPointOutboundResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).fbbActivated(false)
            .build());
    List<BusinessPartnerPickupPointWebResponse> webResponses =
        ResponseHelper.toBusinessPartnerPickupPointWebResponse(responseList);
    assertNotNull(webResponses);
    BusinessPartnerPickupPointOutboundResponse pickupPointResponse = responseList.get(0);
    BusinessPartnerPickupPointWebResponse webResponse = webResponses.get(0);
    Assertions.assertEquals(pickupPointResponse.isDelivery(), webResponse.isDelivery());
  }

  @Test
  public void toProductL3V2DetailWebResponseTest() {
    ProductL3CommonImageResponse commonImageResponse = new ProductL3CommonImageResponse();
    commonImageResponse.setLocationPath("/sample/data");
    productLevel3DetailsV2Response.setVersion(VERSION);
    productLevel3DetailsV2Response.setPreOrder(preOrderResponse);
    productLevel3DetailsV2Response.setCommonImages(Arrays.asList(commonImageResponse));
    ProductLevel3V2WebResponse response =
      ResponseHelper.toProductL3DetailWebResponse(productLevel3DetailsV2Response);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getCommonImages());
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
  }

  @Test
  public void toProductL3V2DetailWebResponseNullTest() {
    productLevel3DetailsV2Response.setVersion(VERSION);
    productLevel3DetailsV2Response.setPreOrder(preOrderResponse);
    productLevel3DetailsV2Response.setProductL3Response(new ProductL3Response());
    productLevel3DetailsV2Response.setAttributes(new ArrayList<>());
    productLevel3DetailsV2Response.setImages(new ArrayList<>());
    productLevel3DetailsV2Response.setProductScore(null);
    ProductLevel3V2WebResponse response =
      ResponseHelper.toProductL3DetailWebResponse(productLevel3DetailsV2Response);
    assertNotNull(response);
  }

  @Test
  void validateResponseTest() {
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponse((GdnRestSingleResponse) null));
  }

  @Test
  void validateResponse_successFalseTest() {
    Assertions.assertThrows(ClientException.class, () -> ResponseHelper.validateResponse(
        new GdnRestSingleResponse<>(null, null, false, null, null)));
  }

  @Test
  void validateResponse_valueNullTest() {
    Assertions.assertThrows(ClientException.class, () -> ResponseHelper.validateResponse(
        new GdnRestSingleResponse<>(null, null, true, null, null)));
  }

  @Test
  public void getEditProductWebResponseWebResponseTest() {
    ResponseHelper.getEditProductWebResponseWebResponse(editProductResponse);
  }

  @Test
  void getEditProductWebResponseWebResponseNotNullTest() {
    VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
    variantsErrorListResponse.setItemName(ITEM_NAME);
    editProductResponse.setVariantsErrorList(Collections.singletonList(variantsErrorListResponse));
    Assertions.assertThrows(EditProductException.class,
        () -> ResponseHelper.getEditProductWebResponseWebResponse(editProductResponse));
  }

  @Test
  public void vvalidateResponseForErrorCodeV2_GdnBaseRestResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCodeV2((GdnBaseRestResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCodeV2_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCodeV2(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCodeV2_GdnBaseRestResponse_SuccessFalseWithValidationErrorCodeTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, VALIDATION, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCodeV2(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCodeV2_GdnBaseRestResponse_SuccessFalseWithApiErrorCodeTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(SHIPPING_WEIGHT_EXCEEDED.getDesc(), SHIPPING_WEIGHT_EXCEEDED.getCode(), false,
            REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = null;
    try {
      ResponseHelper.validateResponseForErrorCodeV2(gdnBaseRestResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(ApiIncorrectInputDataException.class, apiIncorrectInputDataException.getClass());
      assertEquals(SHIPPING_WEIGHT_EXCEEDED.getDesc(), apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void validateResponseForErrorCodeV2_GdnBaseRestResponse_nullErrorrMessage_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(null, null, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForErrorCodeV2(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertNull(exception.getMessage());
    }
  }

  @Test
  public void validateResponseForErrorCodeV2_GdnBaseRestResponse_SuccessTrueTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponseForErrorCodeV2(gdnBaseRestResponse));
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForGetProductL3((GdnBaseRestResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assert exception instanceof ProductListingGenericException;
      assertEquals(ERROR_MESSAGE, ((ProductListingGenericException) exception).getErrorMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_SuccessFalseWithValidationErrorCodeTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, VALIDATION, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3CommunicationFailureTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_SuccessFalseWithApiErrorCodeTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(SHIPPING_WEIGHT_EXCEEDED.getDesc(), SHIPPING_WEIGHT_EXCEEDED.getCode(), false,
            REQUEST_ID);
    ApiIncorrectInputDataException apiIncorrectInputDataException = null;
    try {
      ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse);
    } catch (ApiIncorrectInputDataException e) {
      apiIncorrectInputDataException = e;
    } finally {
      assertEquals(ApiIncorrectInputDataException.class, apiIncorrectInputDataException.getClass());
      assertEquals(SHIPPING_WEIGHT_EXCEEDED.getDesc(), apiIncorrectInputDataException.getErrorMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_nullErrorrMessage_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(null, null, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertNull(exception.getMessage());
    }
  }

  @Test
  public void validateResponseForGetProductL3_GdnBaseRestResponse_SuccessTrueTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponseForGetProductL3(gdnBaseRestResponse));
  }

  @Test
  public void modifyProductCampaignAvailabilityResponseTest() {
    ResponseHelper.modifyProductCampaignAvailabilityResponse(new ProductCampaignAvailabilityResponse());
  }

  @Test
  public void modifyProductCampaignAvailabilityResponseNullTest() {
    ResponseHelper.modifyProductCampaignAvailabilityResponse(null);
  }

  @Test
  public void modifyProductCampaignAvailabilityResponseMapNotEmptyTest() {
    ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse = new ProductCampaignAvailabilityResponse();
    Map<String, Boolean> itemSkuAvailableMap = new HashMap<>();
    itemSkuAvailableMap.putIfAbsent(ITEM_SKU, true);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(itemSkuAvailableMap);
    ResponseHelper.modifyProductCampaignAvailabilityResponse(productCampaignAvailabilityResponse);
  }

  @Test
  public void modifyProductCampaignAvailabilityResponseMapEmptyTest() {
    ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse = new ProductCampaignAvailabilityResponse();
    ProductCampaignAvailabilityInfoDto productCampaignAvailabilityInfoDto = new ProductCampaignAvailabilityInfoDto();
    productCampaignAvailabilityInfoDto.setItemSku(ITEM_SKU);
    productCampaignAvailabilityInfoDto.setAvailability(true);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityInfo(
        Collections.singletonList(productCampaignAvailabilityInfoDto));
    ResponseHelper.modifyProductCampaignAvailabilityResponse(productCampaignAvailabilityResponse);
    Assertions.assertTrue(productCampaignAvailabilityResponse.getProductCampaignAvailabilityMap().get(ITEM_SKU));
  }

  @Test
  public void modifyProductCampaignAvailabilityResponseFalseTest() {
    ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse = new ProductCampaignAvailabilityResponse();
    ProductCampaignAvailabilityInfoDto productCampaignAvailabilityInfoDto = new ProductCampaignAvailabilityInfoDto();
    productCampaignAvailabilityInfoDto.setItemSku(ITEM_SKU);
    productCampaignAvailabilityInfoDto.setAvailability(false);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityInfo(
        Arrays.asList(productCampaignAvailabilityInfoDto, productCampaignAvailabilityInfoDto));
    ResponseHelper.modifyProductCampaignAvailabilityResponse(productCampaignAvailabilityResponse);
    assertFalse(productCampaignAvailabilityResponse.getProductCampaignAvailabilityMap().get(ITEM_SKU));
  }

  @Test
  public void modifyCampaignPriceResponseTest() {
    ResponseHelper.modifyCampaignPriceResponse(new CampaignPriceResponse());
  }

  @Test
  public void modifyCampaignPriceResponseNullTest() {
    ResponseHelper.modifyCampaignPriceResponse(null);
  }

  @Test
  public void modifyCampaignPriceResponseMapNotEmptyTest() {
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponse = new HashMap<>();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    itemSkuToPriceResponse.putIfAbsent(ITEM_SKU, campaignPriceSkuResponse);
    campaignPriceResponse.setItemSkuToPriceResponse(itemSkuToPriceResponse);
    ResponseHelper.modifyCampaignPriceResponse(campaignPriceResponse);
  }

  @Test
  public void modifyCampaignPriceResponseMapTest() {
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    campaignPriceResponse.setItemInfoToPriceResponse(Arrays.asList(campaignPriceSkuResponse, campaignPriceSkuResponse));
    ResponseHelper.modifyCampaignPriceResponse(campaignPriceResponse);
    Assertions.assertEquals(ITEM_SKU, campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).getItemSku());
  }

  @Test
  public void addPickupPointDetailsInProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    PickupPointOutboundResponse pickupPointResponse =
        PickupPointOutboundResponse.builder().code(PICKUP_POINT_CODE).build();
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, Arrays.asList(pickupPointResponse));
    Assertions.assertEquals(PICKUP_POINT_CODE, profileResponse.getPickupPoints().get(0).getCode());
  }

  @Test
  public void toProductL3DetailWebResponseNonWebTest() {
    ProductL3CommonImageResponse commonImageResponse = new ProductL3CommonImageResponse();
    commonImageResponse.setActiveLocation(true);
    commonImageResponse.setLocationPath("/sample/path");
    commonImageResponse.setMainImage(true);
    ProductLevel3AttributeResponse productLevel3AttributeResponse1 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse1.setAttributeCode(ATTRIBUTE_ID);
    productLevel3AttributeResponse1.setVariantCreation(true);
    productL3DetailsResponse.setAttributes(Collections.singletonList(productLevel3AttributeResponse1));
    productL3DetailsResponse.setVersion(VERSION);
    productL3DetailsResponse.setPreOrder(preOrderResponse);
    productL3DetailsResponse.setCommonImages(Arrays.asList(commonImageResponse));
    productL3DetailsResponse.setActiveL5Mapped(true);
    productL3DetailsResponse.setCategoryNameEnglish(CATEGORY_NAME_ENGLISH_1);
    productL3DetailsResponse.setCategoryHierarchyEnglish(CATEGORY_HIERARCHY_ENGLISH);
    productL3DetailsResponse.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    productL3DetailsResponse.setFbbPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE_1));
    productL3DetailsResponse.setAiGeneratedFieldsResponse(
        new AiGeneratedFieldsResponse(false, true));
    ProductL3DetailWebResponse response =
      ResponseHelper.toProductL3DetailWebResponse(productL3DetailsResponse, null, false);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getCommonImages());
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(Arrays.asList(PICKUP_POINT_CODE,PICKUP_POINT_CODE_1),
      response.getPickupPointCodes());
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
    assertEquals(CATEGORY_NAME_ENGLISH_1, response.getCategoryNameEnglish());
    assertEquals(CATEGORY_HIERARCHY_ENGLISH, response.getCategoryHierarchyEnglish());
    Assertions.assertTrue(response.isActiveL5Mapped());
    Assertions.assertTrue(response.isAiGeneratedBrand());
    Assertions.assertFalse(response.isAiGeneratedCategory());
  }

  @Test
  public void toProductL3DetailWebResponseEmptyPickupPointListTest() {
    ProductL3CommonImageResponse commonImageResponse = new ProductL3CommonImageResponse();
    commonImageResponse.setActiveLocation(true);
    commonImageResponse.setLocationPath("/sample/path");
    commonImageResponse.setMainImage(true);
    ProductLevel3AttributeResponse productLevel3AttributeResponse1 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse1.setAttributeCode(ATTRIBUTE_ID);
    productLevel3AttributeResponse1.setVariantCreation(true);
    productL3DetailsResponse.setAttributes(Collections.singletonList(productLevel3AttributeResponse1));
    productL3DetailsResponse.setVersion(VERSION);
    productL3DetailsResponse.setPreOrder(preOrderResponse);
    productL3DetailsResponse.setCommonImages(Arrays.asList(commonImageResponse));
    productL3DetailsResponse.setActiveL5Mapped(true);
    productL3DetailsResponse.setCategoryNameEnglish(CATEGORY_NAME_ENGLISH_1);
    productL3DetailsResponse.setCategoryHierarchyEnglish(CATEGORY_HIERARCHY_ENGLISH);
    productL3DetailsResponse.setFbbPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE_1));
    ProductL3DetailWebResponse response =
      ResponseHelper.toProductL3DetailWebResponse(productL3DetailsResponse, null, false);
    assertNotNull(response);
    assertNotNull(response.getAttributes());
    assertEquals(response.getAttributes().get(0).getAttributeCode(), ATTRIBUTE_ID);
    assertTrue(response.getAttributes().get(0).isVariantCreation());
    assertNotNull(response.getCommonImages());
    assertEquals(response.getDefaultItemSku(), ITEM_SKU);
    assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_TYPE, response.getProductType());
    assertEquals(BRAND, response.getBrand());
    assertEquals(CATEGORY_ID, response.getCategoryId());
    assertEquals(CATEGORY_NAME, response.getCategoryName());
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(CATEGORY_HIERARCHY, response.getCategoryHierarchy());
    assertEquals(URL, response.getUrl());
    assertEquals(PRODUCT_NAME, response.getProductName());
    assertEquals(true, response.getInstallationRequired());
    assertEquals(true, response.getSynchronize());
    assertEquals(100.0, response.getProductScore().getTOTAL_SCORE(), 0);
    assertEquals(15.0, response.getProductScore().getMANDATORY_INFO_RULE(), 0);
    assertEquals(15.0, response.getProductScore().getDESCRIPTION_RULE(), 0);
    assertEquals(10.0, response.getProductScore().getPRODUCT_TITLE_RULE(), 0);
    assertEquals(1.0, response.getProductScore().getEAN_UPC_RULE(), 0);
    assertEquals(20.0, response.getProductScore().getRECOMMENDED_ATTRIBUTE_RULE(), 0);
    assertEquals(9.0, response.getProductScore().getREMAINING_ATTRIBUTE_RULE(), 0);
    assertEquals(18.0, response.getProductScore().getIMAGE_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getUSP_RULE(), 0);
    assertEquals(5.0, response.getProductScore().getVARIANT_CREATING_RULE(), 0);
    assertEquals(2.0, response.getProductScore().getVIDEO_URL_RULE(), 0);
    assertEquals(PICKUP_POINT_CODE_1, response.getPickupPointCodes().get(0));
    assertEquals(VERSION, response.getVersion());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
    assertEquals(CATEGORY_NAME_ENGLISH_1, response.getCategoryNameEnglish());
    assertEquals(CATEGORY_HIERARCHY_ENGLISH, response.getCategoryHierarchyEnglish());
    Assertions.assertTrue(response.isActiveL5Mapped());
    Assertions.assertFalse(response.isAiGeneratedBrand());
  }

  @Test
  public void validateResponseForFbbConsignmentFormTest(){
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>> consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber(ITEM_NAME).build()));
    ResponseHelper.validateResponseForFbbConsignmentForm(consignmentFormsDetail);
  }

  @Test
  void validateResponseForFbbConsignmentFormNullTest() {
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForFbbConsignmentForm(null));
  }

  @Test
  void validateResponseForFbbConsignmentFormErrorTest(){
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>>
      consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setErrors(Map.of(ErrorMessages.ERR_INVALID_RESPONSE,
      Collections.singletonList(ErrorMessages.ERR_INVALID_RESPONSE)));
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForFbbConsignmentForm(consignmentFormsDetail));
  }

  @Test
  void validateResponseForFbbConsignmentFormNullData() {
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>>
        consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setData(null);
    Assertions.assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForFbbConsignmentForm(consignmentFormsDetail));
  }

  @Test
  public void toConsignmentDetailWebResponseTest(){
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>> consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber(ITEM_NAME).build()));
    List<ConsignmentDetailWebResponse> webResponse =
      ResponseHelper.toConsignmentDetailWebResponse(consignmentFormsDetail);
    assertNotNull(webResponse);
    Assertions.assertEquals(ITEM_NAME,webResponse.get(0).getConsignmentCode());
    Assertions.assertEquals(ConsignmentStatusWeb.IN_PROGRESS,webResponse.get(0).getStatus());
  }

  @Test
  public void toConsignmentDetailWebResponseWithComparatorTest(){
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>>
      consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setData(List.of(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber("CF-00001").build(),
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.DONE)
        .createdDate(new Date(System.currentTimeMillis() - 1000L)).documentNumber("CF-00002")
        .build(), ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.REJECTED)
        .createdDate(new Date(System.currentTimeMillis() - 2000L)).documentNumber("CF-00003")
        .build()));

    List<ConsignmentDetailWebResponse> webResponse =
      ResponseHelper.toConsignmentDetailWebResponse(consignmentFormsDetail);

    assertNotNull(webResponse);
    Assertions.assertEquals("CF-00001", webResponse.get(0).getConsignmentCode());
    Assertions.assertEquals(ConsignmentStatusWeb.IN_PROGRESS, webResponse.get(0).getStatus());
    Assertions.assertEquals("CF-00002", webResponse.get(1).getConsignmentCode());
    Assertions.assertEquals(ConsignmentStatusWeb.DONE, webResponse.get(1).getStatus());
    Assertions.assertEquals("CF-00003", webResponse.get(2).getConsignmentCode());
    Assertions.assertEquals(ConsignmentStatusWeb.REJECTED, webResponse.get(2).getStatus());
  }

  @Test
  public void toConsignmentDetailWebResponseTest2(){
    com.blibli.oss.common.response.Response<List<ConsignmentStatusResponse>> consignmentFormsDetail = new com.blibli.oss.common.response.Response<>();
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.DONE)
        .createdDate(new Date()).documentNumber(ITEM_NAME).build()));
    List<ConsignmentDetailWebResponse> webResponse =
      ResponseHelper.toConsignmentDetailWebResponse(consignmentFormsDetail);
    assertNotNull(webResponse);
    Assertions.assertEquals(ITEM_NAME,webResponse.get(0).getConsignmentCode());
    Assertions.assertEquals(ConsignmentStatusWeb.DONE,webResponse.get(0).getStatus());
  }

  @Test
  public void validateResponseForConsignmentCountTest(){
    com.blibli.oss.common.response.Response<List<CountConsignmentFormsByItemSkuResponse>>
      consignmentCount = new com.blibli.oss.common.response.Response<>();
    consignmentCount.setData(Collections.singletonList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build()));
    boolean validated = ResponseHelper.validateResponseForConsignmentCount(consignmentCount);
    assertTrue(validated);
  }

  @Test()
  public void validateResponseForConsignmentCountNullTest(){
    com.blibli.oss.common.response.Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentCount = null;
    boolean validated = ResponseHelper.validateResponseForConsignmentCount(consignmentCount);
    assertFalse(validated);
  }

  @Test
  public void validateResponseForConsignmentCountErrorTest(){
    com.blibli.oss.common.response.Response<List<CountConsignmentFormsByItemSkuResponse>>
      consignmentCount = new com.blibli.oss.common.response.Response<>();
    consignmentCount.setErrors(Map.of(ErrorMessages.ERR_INVALID_RESPONSE,
      Collections.singletonList(ErrorMessages.ERR_INVALID_RESPONSE)));
    boolean validated = ResponseHelper.validateResponseForConsignmentCount(consignmentCount);
    assertFalse(validated);
  }

  @Test
  public void validateResponseForConsignmentCountNullData(){
    com.blibli.oss.common.response.Response<List<CountConsignmentFormsByItemSkuResponse>>
      consignmentCount = new com.blibli.oss.common.response.Response<>();
    consignmentCount.setData(null);
    boolean validated = ResponseHelper.validateResponseForConsignmentCount(consignmentCount);
    assertFalse(validated);
  }

  @Test
  public void categorySuggestionWebResponseTest(){
    CategorySuggestionWebResponse webResponse =
      ResponseHelper.toCategorySuggestionWebResponse(categoryResponse
      , 10);
    assertEquals(webResponse.getNameEnglish(), categoryResponse.getNameEnglish());
    assertEquals(webResponse.getProductCount(), 10);
  }

  @Test
  public void toBasicItemDetailsWebResponse(){
    ItemCodeBasicDetailResponse itemCodeBasicDetailResponse =
      ItemCodeBasicDetailResponse.builder().itemName(ITEM_NAME).itemCode(GDN_ITEM_SKU1)
        .suspended(true).build();
    ItemCodeBasicDetailResponse itemCodeBasicDetailResponse1 =
      ItemCodeBasicDetailResponse.builder().itemName(ITEM_NAME_1).itemCode(GDN_ITEM_SKU2)
        .suspended(true).build();

    List<ItemCodeBasicDetailResponse> itemCodeBasicDetailResponses =
      List.of(itemCodeBasicDetailResponse1, itemCodeBasicDetailResponse);
    GdnRestListResponse<ItemCodeBasicDetailResponse> basicItemDetailsByItemCodes =
      new GdnRestListResponse<>();
    basicItemDetailsByItemCodes.setContent(itemCodeBasicDetailResponses);
    Page<ItemCodeBasicDetailWebResponse> basicItemDetailsWebResponse =
      ResponseHelper.toBasicItemDetailsWebResponse(basicItemDetailsByItemCodes,
        PRODUCT_DETAIL_PAGE_URL_PREFIX, 0, 10);
    assertEquals(2, basicItemDetailsWebResponse.getContent().size());

  }

  @Test
  public void toProductLevel3ListingWebResponseTest(){
    ProductL3SummaryResponse productL3SummaryResponse =
      ProductL3SummaryResponse.builder().productType(ProductType.BIG_PRODUCT).dimensionsMissing(true).build();
    ProductLevel3ListingWebResponse productLevel3ListingWebResponse =
      ResponseHelper.toProductLevel3ListingWebResponse(productL3SummaryResponse,
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        Collections.emptyMap(), Collections.emptyMap(), PRODUCT_DETAIL_PAGE_URL_PREFIX);
    Assertions.assertEquals(com.gdn.mta.product.enums.ProductType.BIG_PRODUCT.getProductType(),
      productLevel3ListingWebResponse.getProductType());
    Assertions.assertTrue(productLevel3ListingWebResponse.getDimensionsMissing());
  }

  @Test
  public void toProductLevel3ListingWebResponseNullProductTypeTest(){
    ProductL3SummaryResponse productL3SummaryResponse =
      ProductL3SummaryResponse.builder().dimensionsMissing(true).build();
    ProductLevel3ListingWebResponse productLevel3ListingWebResponse =
      ResponseHelper.toProductLevel3ListingWebResponse(productL3SummaryResponse,
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        Collections.emptyMap(), Collections.emptyMap(), PRODUCT_DETAIL_PAGE_URL_PREFIX);
    assertNull(productLevel3ListingWebResponse.getProductType());
    Assertions.assertTrue(productLevel3ListingWebResponse.getDimensionsMissing());
  }

  @Test
  public void toProductLevel3ListingV2WebResponseTest(){
    ProductL3SummaryResponse productL3SummaryResponse =
      ProductL3SummaryResponse.builder().productType(ProductType.BIG_PRODUCT).productSku(PRODUCT_SKU).dimensionsMissing(true).build();
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
      ResponseHelper.toProductLevel3ListingV2WebResponse(productL3SummaryResponse,
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        PRODUCT_DETAIL_PAGE_URL_PREFIX, false);
    Assertions.assertEquals(com.gdn.mta.product.enums.ProductType.BIG_PRODUCT.getProductType(),
      productLevel3ListingV2WebResponse.getProductType());
    Assertions.assertEquals(true, productLevel3ListingV2WebResponse.getDimensionsMissing());
  }

  @Test
  public void toProductLevel3ListingV2WebResponseSingleVariantTest() {
    ProductL3SummaryResponse productL3SummaryResponse =
        ProductL3SummaryResponse.builder().productType(ProductType.BIG_PRODUCT).productSku(PRODUCT_SKU)
            .dimensionsMissing(true).build();
    productL3SummaryResponse.setVariantCount(1);
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    itemL4SummaryResponse.setPrice(new HashSet<>());
    itemL4SummaryResponse.setItemViewConfigs(new ArrayList<>());
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
        ResponseHelper.toProductLevel3ListingV2WebResponse(productL3SummaryResponse, Collections.emptyMap(),
            Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
            Collections.emptyMap(), PRODUCT_DETAIL_PAGE_URL_PREFIX, false);
    Assertions.assertEquals(com.gdn.mta.product.enums.ProductType.BIG_PRODUCT.getProductType(),
        productLevel3ListingV2WebResponse.getProductType());
    Assertions.assertEquals(true, productLevel3ListingV2WebResponse.getDimensionsMissing());
  }

  @Test
  public void toProductLevel3ListingV2WebResponseSingleVariantTrueTest() {
    ProductL3SummaryResponse productL3SummaryResponse =
        ProductL3SummaryResponse.builder().productType(ProductType.BIG_PRODUCT).productSku(PRODUCT_SKU)
            .dimensionsMissing(true).build();
    productL3SummaryResponse.setVariantCount(1);
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    itemL4SummaryResponse.setPrice(new HashSet<>());
    itemL4SummaryResponse.setItemViewConfigs(new ArrayList<>());
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
        ResponseHelper.toProductLevel3ListingV2WebResponse(productL3SummaryResponse, Collections.emptyMap(),
            Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
            Collections.emptyMap(), PRODUCT_DETAIL_PAGE_URL_PREFIX, true);
    Assertions.assertEquals(com.gdn.mta.product.enums.ProductType.BIG_PRODUCT.getProductType(),
        productLevel3ListingV2WebResponse.getProductType());
    Assertions.assertEquals(true, productLevel3ListingV2WebResponse.getDimensionsMissing());
  }

  @Test
  public void toProductLevel3ListingV2WebResponseWithNullProductTypeTest() {
    ProductL3SummaryResponse productL3SummaryResponse =
      ProductL3SummaryResponse.builder().productSku(PRODUCT_SKU).dimensionsMissing(true).build();
    ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse =
      ResponseHelper.toProductLevel3ListingV2WebResponse(productL3SummaryResponse,
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap(),
        PRODUCT_DETAIL_PAGE_URL_PREFIX, true);
    assertNull(productLevel3ListingV2WebResponse.getProductType());
    Assertions.assertEquals(true, productLevel3ListingV2WebResponse.getDimensionsMissing());
  }

  @Test
  public void toL3AndPickupPointStockAvailabilityResponseTest() {
    L3AndPickupPointStockAvailabilityResponse stockAvailabilityResponse =
      L3AndPickupPointStockAvailabilityResponse.builder().webProductSku(PRODUCT_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).warehouseAvailable(true).build();

    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> stockAvailabilityByL3AndPickupPoint =
      new GdnRestSingleResponse<>();
    stockAvailabilityByL3AndPickupPoint.setValue(stockAvailabilityResponse);
    PickupPointStockAndInBoundStatusWebResponse webResponse =
      ResponseHelper.toL3AndPickupPointStockAvailabilityResponse(stockAvailabilityByL3AndPickupPoint);
    assertNotNull(webResponse);
    assertEquals(PRODUCT_SKU, webResponse.getProductSku());
    assertEquals(PICKUP_POINT_CODE, webResponse.getPickupPointCode());
    assertTrue(webResponse.isWarehouseStockAvailable());
  }

  @Test
  public void validateVideoResponse_Success() {
    VideoSignedUrlResponse signedUrlResponse = new VideoSignedUrlResponse();
    signedUrlResponse.setVideoSignedUrl("https://test-url.com");
    Response<VideoSignedUrlResponse> successResponse = new Response<>(
        200, "SUCCESS", signedUrlResponse, null, null
    );
    assertDoesNotThrow(() -> ResponseHelper.validateVideoResponse(successResponse));
  }

  @Test
  public void validateVideoResponse_WhenResponseIsNull() {
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(null));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateVideoResponse_WhenStatusIsNotSuccess() {
    Response<VideoSignedUrlResponse> errorResponse = new Response<>(
        400, "ERROR", null,
        Collections.singletonMap("error", Collections.singletonList("Invalid request")),
        null
    );
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(errorResponse));
    assertEquals("Invalid request", exception.getMessage());
  }

  @Test
  public void validateVideoResponse_WhenDataIsNull() {
    Response<VideoSignedUrlResponse> nullDataResponse = new Response<>(
        200, "SUCCESS", null, null, null
    );
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(nullDataResponse));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateVideoResponse_WhenResponseHasEmptyErrorList() {
    Response<VideoSignedUrlResponse> emptyErrorResponse = new Response<>(
        400, "ERROR", null,
        Collections.singletonMap("error", Collections.emptyList()),
        null
    );
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(emptyErrorResponse));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateVideoResponse_WhenResponseHasNullErrors() {
    Response<VideoSignedUrlResponse> nullErrorsResponse = new Response<>(
        400, "ERROR", null, null, null
    );
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(nullErrorsResponse));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateVideoResponse_WhenResponseHasEmptyErrorsMap() {
    Response<VideoSignedUrlResponse> emptyErrorsMapResponse = new Response<>(
        400, "ERROR", null, Collections.emptyMap(), null
    );
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateVideoResponse(emptyErrorsMapResponse));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateResponseForReels_NullResponse() {
    ClientException exception =
        assertThrows(ClientException.class, () -> ResponseHelper.validateResponseForReels(null,
            true));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void validateResponseForReels_NullData() {
    ReelsResponse<Object> response = ReelsResponse.builder().status(SUCCESS).data(null)
        .errors(Map.of("error", Collections.singletonList("Data not found"))).metadata(null)
        .paging(null).build();
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForReels(response, true));
    assertEquals("Data not found", exception.getMessage());
  }

  @Test
  public void validateResponseForReels_NullDataNoExpection() {
    ReelsResponse<Object> response = ReelsResponse.builder().status(SUCCESS).data(null)
        .errors(Map.of("error", Collections.singletonList("Data not found"))).metadata(null)
        .paging(null).build();
    ResponseHelper.validateResponseForReels(response, false);
  }

  @Test
  public void validateResponseForReels_NonSuccessStatusWithData() {
    ReelsResponse<String> response = ReelsResponse.<String>builder()
        .status(HttpStatus.BAD_REQUEST.name())
        .data("test data")
        .errors(Map.of("error", Collections.singletonList("Invalid request")))
        .metadata(null)
        .paging(null)
        .build();

    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForReels(response, true));
    assertEquals("Invalid request", exception.getMessage());
  }

  @Test
  public void validateResponseForReels_NonSuccessStatus() {
    ReelsResponse<String> response = ReelsResponse.<String>builder()
        .status(HttpStatus.BAD_REQUEST.name())
        .data("test data")  // Adding non-null data
        .errors(Map.of("error", Collections.singletonList("Invalid request")))
        .metadata(null)
        .paging(null)
        .build();
    ClientException exception = assertThrows(ClientException.class,
        () -> ResponseHelper.validateResponseForReels(response, true));
    assertEquals("Invalid request", exception.getMessage());
  }

  @Test
  public void validateResponseForReels_Success() {
    ReelsResponse<Object> response = ReelsResponse.builder()
        .status(SUCCESS)
        .data(new Object())
        .errors(null)
        .metadata(null)
        .paging(null)
        .build();
    assertDoesNotThrow(() -> ResponseHelper.validateResponseForReels(response, true));
  }

  @Test
  public void populatePwpPromoTest() {
    ItemPickupPointSummaryWebResponse itemPickupPointSummaryWebResponse = new ItemPickupPointSummaryWebResponse();
    ResponseHelper.populatePwpPromo(itemPickupPointSummaryWebResponse);
    itemPickupPointSummaryWebResponse.setActivePromoBundlings(new HashSet<>(Collections.singleton(Constants.OEM)));
    ResponseHelper.populatePwpPromo(itemPickupPointSummaryWebResponse);
    itemPickupPointSummaryWebResponse.setActivePromoBundlings(
        new HashSet<>(Collections.singleton(Constants.PWP_MAIN_PENDING)));
    ResponseHelper.populatePwpPromo(itemPickupPointSummaryWebResponse);
    Assertions.assertTrue(itemPickupPointSummaryWebResponse.isPromoBundling());
    Assertions.assertFalse(CollectionUtils.isEmpty(itemPickupPointSummaryWebResponse.getActivePromoBundlings()));
    Assertions.assertTrue(
        itemPickupPointSummaryWebResponse.getActivePromoBundlings().contains(Constants.PWP_MAIN_PENDING));
    itemPickupPointSummaryWebResponse.setActivePromoBundlings(
        new HashSet<>(Arrays.asList(Constants.PWP_ADDITIONAL_ACTIVE)));
    Assertions.assertTrue(itemPickupPointSummaryWebResponse.isPromoBundling());
    Assertions.assertFalse(CollectionUtils.isEmpty(itemPickupPointSummaryWebResponse.getActivePromoBundlings()));
  }

  @Test
  public void populatePwpPromoWebTest() {
    ItemPickupPointListingL3WebResponse itemPickupPointListingL3WebResponse = new ItemPickupPointListingL3WebResponse();
    ResponseHelper.populatePwpPromo(itemPickupPointListingL3WebResponse);
    itemPickupPointListingL3WebResponse.setActivePromoBundlings(new HashSet<>(Collections.singleton(Constants.OEM)));
    ResponseHelper.populatePwpPromo(itemPickupPointListingL3WebResponse);
    itemPickupPointListingL3WebResponse.setActivePromoBundlings(
        new HashSet<>(Collections.singleton(Constants.PWP_MAIN_PENDING)));
    ResponseHelper.populatePwpPromo(itemPickupPointListingL3WebResponse);
    Assertions.assertTrue(itemPickupPointListingL3WebResponse.isPromoBundling());
    Assertions.assertTrue(CollectionUtils.isEmpty(itemPickupPointListingL3WebResponse.getActivePromoBundlings()));
    itemPickupPointListingL3WebResponse.setActivePromoBundlings(
        new HashSet<>(Arrays.asList(Constants.PWP_ADDITIONAL_ACTIVE)));
    Assertions.assertTrue(itemPickupPointListingL3WebResponse.isPromoBundling());
    Assertions.assertFalse(CollectionUtils.isEmpty(itemPickupPointListingL3WebResponse.getActivePromoBundlings()));
  }

  @Test
  public void testMapToValidOmniChannelSkuWebResponse_NullInput() {
    ValidOmniChannelSkuWebResponse result = ResponseHelper.mapToValidOmniChannelSkuWebResponse(null);
    assertNull(result, "Expected result to be null when input is null");
  }

  @Test
  public void testMapToValidOmniChannelSkuWebResponse_ValidInput() {
    // Setup test input
    Map<String, OmniChannelSkuResponse> testMap = new HashMap<>();
    testMap.put("SKU123", new OmniChannelSkuResponse());
    OmniChannelMapAndSkuResponse input = new OmniChannelMapAndSkuResponse();
    input.setExistingSellerSkusAndProductDetailsMap(testMap);

    // Call the mapping method
    ValidOmniChannelSkuWebResponse result = ResponseHelper.mapToValidOmniChannelSkuWebResponse(input);

    // Assertions
    assertNotNull(result);
    assertEquals(testMap, result.getExistingOmniChannelSkusAndProductDetailsMap());
  }

}


package com.gdn.partners.pcu.external.service.impl.helper;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLevel3OrderResponse;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductL3CommonImageRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.BulkProcessExternalUploadRequest;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerAttributeServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.external.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductBasicInfoDownloadRequest;
import com.gdn.partners.pcu.external.web.BulkActivityStatusWeb;
import com.gdn.partners.pcu.external.web.model.B2BFields;
import com.gdn.partners.pcu.external.web.model.enums.MerchantType;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.B2bFieldsRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BuyableScheduleWebRequest;
import com.gdn.partners.pcu.external.web.model.request.CopyImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.request.DeletedProductItemsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DiscoverableScheduleWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointDeleteWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateItemsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PreOrderWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBundleRecipeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBundleWebRecipe;
import com.gdn.partners.pcu.external.web.model.request.ProductDetailsRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLevel3LogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWholesalePriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3AttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3CommonImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3ImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3LogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3PriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3SummaryDetailsImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3ViewConfigWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSettingsRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoAddEditRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;


public class RequestHelperTest {
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "category_name";
  private static final String SEARCH_KEY = "searchKey";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_TWO = "pickupPointCodeTwo";
  private static final String NAMEKEY = "name";
  private static final String SORTTYPE = "asc";
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String PRODUCT_NAME = "searchKey";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String IN_PROGRESS_CRITERIA = "IN_PROGRESS";
  private static final String FAILED_CRITERIA = "FAILED";
  private static final String NEED_CORRECTION_CRITERIA = "NEED_CORRECTION";
  private static final String USERNAME = "username";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";
  private static final String PRIVILEGED_EDIT_O2O = "isPrivilegedToEditO2O";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String DESCRIPTION = "test description";
  private static final String CHANNEL_ID = "Channel_Id";
  private static final String PROMOTION_NAME = "Promotion_Name";
  private static final double PRICE =  1000;
  private static final int SEQUENCE = 9080;
  private static final String LOCATION_PATH = "/sample/path";
  private static final String ATTRIBUTE_CODE = "attribute_code";
  private static final String ATTRIBUTE_NAME = "attribute_name";
  private static final String ATTRIBUTE_TYPE = "attribute_type";
  private static final String KEYWORD = "KEYWORD";
  private static final String ITEM_SKU = "item_sku";
  private static final String ITEM_NAME = "item_name";
  private static final String[] VALUES = {"value1","value2"};
  private static final String INVENTORY_FULFILLMENT = "BL";
  private static final String STORE_ID = "store_id";
  private static final String BRAND = "brand";
  private static final String PRODUCT_TAB = "STORE_PRODUCT_MANAGE-PRODUCT,STORE_PRODUCT";
  private static final String MANAGE_PRODUCT_TAB = "STORE_PRODUCT";
  private static final String MANAGE_PRODUCT = "STORE_PRODUCT_MANAGE-PRODUCT";
  private static final String INVALID_TEXT = "testing text & <script>wrong</script> HK 1 & HK < HK 1 > < > HK 2";
  private static final String VALID_TEXT = "testing text &  HK 1 & HK < HK 1 > < > HK 2";
  private static final String INVALID_TEXT1 = "testing text &amp; HK 1 &amp; HK &lt; HK 1 &gt; HK 2";
  private static final String INVALID_TEXT2 = "testing text; HK 1 ; HK; HK 1 &gt; HK 2";
  private static final String INVALID_TEXT3 = "testing text; HK 1 ; HK; HK 1 &lt; HK 2";
  private static final String INVALID_TEXT4 = "testing text; HK 1 ; HK; HK 1 &amp; HK 2";
  private static final String VALID_TEXT1 = "testing text & HK 1 & HK < HK 1 > HK 2";
  private static final String INVALID_URL_TEXT = "<h2 style=\"text-align: center;\"><iframe src=\"//www.youtube.com/embed/3qdSTNnfWec\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></h2>\n"
      + "<h2 style=\"text-align: center;\"><iframe src=\"//www.media.com/xyz\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></h2>\n"
      + "<h2 style=\"text-align: center;\"><iframe width=\"560\" height=\"314\" src=\"https://youtu.be/J3W4dz92WTI\"  allowfullscreen=\"allowfullscreen\"></iframe></h2>";
  private static final String VALID_URL_TEXT = "<h2 style=\"text-align: center;\"><iframe src=\"//www.youtube.com/embed/3qdSTNnfWec\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></h2>\n"
      + "<h2 style=\"text-align: center;\"></h2>\n"
      + "<h2 style=\"text-align: center;\"><iframe width=\"560\" height=\"314\" src=\"https://youtu.be/J3W4dz92WTI\"  allowfullscreen=\"allowfullscreen\"></iframe></h2>";
  private static final long NORMAL_PRICE = 1000;
  private static final long SALE_PRICE = 2000;
  private static final int QUANTITY = 10;
  private static final double DISCOUNT = 10.0;
  private static final String LOGISTICS_PRODUCT_CODE = "LOGISTICS_PRODUCT_CODE";
  private static final String STYLED_DESCRIPTION =
      "tstt<div style=\"text-align: center;\">aligmnet center</div><div style=\"text-align: left;\"><span style=\"text-align: right;\"><br></span></div><div style=\"text-align: left;\"><span style=\"text-align: right;\">aligment left</span></div><div style=\"text-align: right;\">aligment right</div><div style=\"text-align: justify;\">alligment justify</div><div style=\"text-align: justify;\"><br></div><div style=\"text-align: justify;\"><b>testttt</b></div>";
  private static final String VALID_STYLED_DESCRIPTION =
      "tstt\n" + "<div style=\"text-align: center;\">\n" + " aligmnet center\n" + "</div>\n"
          + "<div style=\"text-align: left;\">\n" + " <span style=\"text-align: right;\"><br></span>\n" + "</div>\n"
          + "<div style=\"text-align: left;\">\n" + " <span style=\"text-align: right;\">aligment left</span>\n"
          + "</div>\n" + "<div style=\"text-align: right;\">\n" + " aligment right\n" + "</div>\n"
          + "<div style=\"text-align: justify;\">\n" + " alligment justify\n" + "</div>\n"
          + "<div style=\"text-align: justify;\">\n" + " <br>\n" + "</div>\n" + "<div style=\"text-align: justify;\">\n"
          + " <b>testttt</b>\n" + "</div>";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String PREORDER_WEEK_TYPE = "WEEK";
  private static final String PREORDER_DATE_TYPE = "DATE";
  private static final String WRONG_PREORDER_TYPE = "PREORDER";
  private static final Integer PREORDER_MAXIMUM_DAYS = 90;
  private static final Integer PREORDER_MAXIMUM_WEEK = 13;
  private static final String DAYS_MORE_THAN_LIMIT_ERROR = "Maximum 90 days are allowed";
  private static final String ZERO_DAYS_ERROR = "Number of days should be more than 0";
  private static final String MAXIMUM_WEEK_ERROR = "Maximum 13 weeks are allowed";
  private static final String ZERO_WEEK_ERROR = "Number of week should be more than 0";
  private static final String DATE_ERROR = "PreOrder date must be greater than available date";
  private static final String WRONG_PREORDER_TYPE_ERROR = "PreOrder type must be DAYS, WEEK or DATE";
  private static final String PREORDER_DATE_EXCEEDED_LIMIT = "PreOrder date is exceeded 90 days";
  private static final String VARIANT_NAME = "VARIANT_NAME";
  private static final int PREORDER_FAILURE_STATUS_CODE = 400;
  private static final String MERCHANT_TYPE_CC = "CC";
  private static final String MERCHANT_TYPE_TD = "TD";
  private static final String MERCHANT_TYPE_TC = "TC";
  private static final String MERCHANT_TYPE_RC = "RC";
  private static final String PRODUCT_DETAIL_URL = "PRODUCT_SKU-PRODUCT_SKU.html";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";
  private static final String STATUS = "ONLINE";
  private static final String VALUE = "value";

  private static final String VALIDATION_ERROR =
      "Can not process invalid input data :You are not authorized";
  private static final String TEMPLATE_VALIDATION_ERROR =
      "Can not process invalid input data :selected template does not support number of QR per "
          + "page selected";
  private static final String INVALID_REQUEST_ERROR =
      "Can not process invalid input data :Not a valid parameter value";

  private static final String REQUEST_ID="Request_id";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "ABC-70000";
  private static final String TEST_USERNAME = "testUser";
  private static final String TEST_BUSINESS_PARTNER_CODE = "BP123";
  private static final String TEST_RANDOM_UUID = "123e4567-e89b-12d3-a456-426614174000";
  private static final String TEST_EMAIL_TO = "test@example.com";
  private static final String TEST_FILE_NAME = "testFile.xlsx";
  private static final String VALID_BP_CODE = "BP123";
  private static final String INVALID_BP_CODE = "BP999";
  private static final String ACTIVE_STATUS = "ACTIVE";
  private static final String INACTIVE_STATUS = "INACTIVE";
  private static final String EXPECTED_ERROR_MESSAGE =
      ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + INVALID_BP_CODE;


  private ActiveProductWebRequest activeProductWebRequest;
  private ProductL3ListingWebRequest productL3ListingWebRequest;
  private ActiveProductWebRequest activeProductWebRequest1;
  private SuspensionWebRequest suspensionWebRequest;
  private InActiveProductWebRequest inInActiveProductWebRequest;
  private InProcessProductWebRequest inProcessProductWebRequest;
  private ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest;
  private ProductLevel3OrderResponse productLevel3OrderResponse;
  private Map<String, Boolean> privilegeMap = new HashMap<>();
  private ProfileResponse profileResponse;
  private Map<String, String> args = new HashMap<>();
  private Map<String, String> files = new HashMap<>();
  private ProductLevel3PriceWebRequest productLevel3PriceWebRequest;
  private ProductLevel3ViewConfigWebRequest productLevel3ViewConfigWebRequest;
  private ProductLevel3ImageWebRequest productLevel3ImageWebRequest;
  private ProductLevel3AttributeWebRequest productLevel3AttributeWebRequest;
  private ProductItemLevel3WebRequest productItemLevel3WebRequest;
  private ProductLevel3WebRequest productLevel3WebRequest;
  private ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest;
  private ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest;
  private ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest;
  private ProductCreationRequest productCreationRequest;
  private ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest1;
  private CreateBrandWipRequest createBrandWipRequest;
  private ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest1;
  private ProductLevel3Request productLevel3Request;
  private ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest;
  private PriceUpdateWebRequest priceUpdateWebRequest;
  private ProductItemWholesalePriceWebRequest productItemWholesalePriceWebRequest;
  private ProductLevel3SummaryDetailsImageWebRequest productLevel3SummaryDetailsImageWebRequest;
  private ProductLevel3CommonImageWebRequest ProductLevel3CommonImageWebRequest;
  private ProductPriceStockAndImagesWebRequest productPriceStockAndImagesWebRequest;
  private ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest;
  private PickupPointUpdateWebRequest pickupPointUpdateWebRequest;
  private PreOrderWebRequest preOrderWebRequest;
  private PreOrderRequest preOrderRequest;
  private List<QuickEditWebRequest> quickEditWebRequests;
  private ProductSummaryV2WebRequest productSummaryV2WebRequest;
  private ItemPickupPointWebRequest itemPickupPointWebRequest =
    ItemPickupPointWebRequest.builder().itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_CODE).build();
  private ProductLevel3CommonImageWebRequest productLevel3CommonImageWebRequest;
  private ProductL3CommonImageRequest productL3CommonImageRequest;
  private ProductL3UpdateRequest productL3UpdateRequest;
  private ProductVariantUpdateRequest productVariantUpdateRequest;
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest;
  private ProductLevel3SummaryDetailsImageRequest imageRequest;
  private com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointV2Request;
  private PickupPointDeleteRequest itemPickupPointDeleteRequest;
  private DefaultConfigurationAndPickupPointRequest defaultConfigurationAndPickupPointRequest;
  private QuickEditV2WebRequest quickEditV2WebRequest = new QuickEditV2WebRequest();
  private ProductVariantPriceStockAndImagesWebRequest variantPriceStockAndImagesWebRequest;
  private ProductVariantUpdateWebRequest productVariantUpdateWebRequest;
  private ItemPickupPointDeleteWebRequest itemPickupPointDeleteWebRequest;
  private HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
  private ManualQRCodeRequest manualQRCodeRequest;
  private ProductDetailsRequest productDetailsRequest;

  private static final String DEFAULT_PRODUCT_SKU = "ABC-70000-12345";
  @BeforeEach
  public void setUp() {
    activeProductWebRequest = ActiveProductWebRequest.builder()
        .searchKey(SEARCH_KEY).discoverable(true).buyable(true)
        .page(PAGE).size(SIZE).build();

    productL3ListingWebRequest =
        ProductL3ListingWebRequest.builder().searchKey(SEARCH_KEY)
            .categoryCodes(Collections.singletonList(CATEGORY_CODE))
            .pickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)).build();

    activeProductWebRequest1 = ActiveProductWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .searchKey(SEARCH_KEY).buyable(true).build();

    suspensionWebRequest =
        SuspensionWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .nameKey(NAMEKEY).skuKey(SEARCH_KEY).sortType(SORTTYPE).build();
    inInActiveProductWebRequest = InActiveProductWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .searchKey(SEARCH_KEY).orderBy(ORDER_BY).sortBy(SORT_BY).build();
    inProcessProductWebRequest = InProcessProductWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).searchKey(PRODUCT_NAME).criteria(IN_PROGRESS_CRITERIA).orderBy(ORDER_BY)
        .sortBy(SORT_BY).build();

    productLevel3UpdateSummaryRequest = new ProductLevel3UpdateSummaryRequest();
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setSalePrice(1000.80);
    productLevel3PriceRequest.setPrice(2000.0);
    productLevel3UpdateSummaryRequest.setOff2OnActiveFlag(false);
    productLevel3UpdateSummaryRequest.setPrices(Arrays.asList(productLevel3PriceRequest));
    productLevel3UpdateSummaryRequest.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_LIST.getDesc());

    productLevel3OrderResponse = new ProductLevel3OrderResponse();
    productLevel3OrderResponse.setItems(Arrays.asList(new ProductItemLevel3OrderResponse()));

    profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment(INVENTORY_FULFILLMENT);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    privilegeMap.put(PRIVILEGED_EDIT_O2O, true);

    productItemBusinessPartnerServiceRequest = new ProductItemBusinessPartnerServiceRequest();
    productItemBusinessPartnerServiceRequest.setBuyable(true);

    productBusinessPartnerAttributeServiceRequest = new ProductBusinessPartnerAttributeServiceRequest();
    productBusinessPartnerAttributeServiceRequest.setAttributeId(ATTRIBUTE_CODE);
    productBusinessPartnerAttributeServiceRequest.setValue(ATTRIBUTE_NAME);

    productBusinessPartnerServiceRequest = new ProductBusinessPartnerServiceRequest();
    productBusinessPartnerServiceRequest.setStoreId(STORE_ID);
    productBusinessPartnerServiceRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productBusinessPartnerServiceRequest.setBrand(BRAND);
    productBusinessPartnerServiceRequest.setCategoryName(CATEGORY_NAME);
    productBusinessPartnerServiceRequest.setProductName(PRODUCT_NAME);
    productBusinessPartnerServiceRequest.setProductBusinessPartnerAttributes(Arrays.asList
        (productBusinessPartnerAttributeServiceRequest));
    productBusinessPartnerServiceRequest.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartnerServiceRequest));


    productLevel3PriceWebRequest = new ProductLevel3PriceWebRequest();
    productLevel3PriceWebRequest.setChannelId(CHANNEL_ID);
    productLevel3PriceWebRequest.setDiscountAmount(new Double(PRICE));
    productLevel3PriceWebRequest.setPrice(new Double(PRICE));
    productLevel3PriceWebRequest.setSalePrice(new Double(PRICE));
    productLevel3PriceWebRequest.setDiscountStartDate(new Date());
    productLevel3PriceWebRequest.setDiscountEndDate(new Date());
    productLevel3PriceWebRequest.setPromotionName(PROMOTION_NAME);

    productLevel3ViewConfigWebRequest = new ProductLevel3ViewConfigWebRequest();
    productLevel3ViewConfigWebRequest.setChannelId(CHANNEL_ID);
    productLevel3ViewConfigWebRequest.setDisplay(true);
    productLevel3ViewConfigWebRequest.setBuyable(true);

    productLevel3ImageWebRequest = new ProductLevel3ImageWebRequest();
    productLevel3ImageWebRequest.setMainImage(true);
    productLevel3ImageWebRequest.setSequence(new Integer(SEQUENCE));
    productLevel3ImageWebRequest.setLocationPath(LOCATION_PATH);

    productLevel3AttributeWebRequest = new ProductLevel3AttributeWebRequest();
    productLevel3AttributeWebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3AttributeWebRequest.setAttributeName(ATTRIBUTE_NAME);
    productLevel3AttributeWebRequest.setAttributeType(ATTRIBUTE_TYPE);
    productLevel3AttributeWebRequest.setItemSku(ITEM_SKU);
    productLevel3AttributeWebRequest.setSkuValue(true);
    productLevel3AttributeWebRequest.setValues(Arrays.asList(VALUES));

    productItemLevel3WebRequest = new ProductItemLevel3WebRequest();
    productItemLevel3WebRequest.setItemSku(ITEM_SKU);
    productItemLevel3WebRequest.setPrices(Arrays.asList(productLevel3PriceWebRequest));
    productItemLevel3WebRequest.setViewConfigs(Arrays.asList(productLevel3ViewConfigWebRequest));
    productItemLevel3WebRequest.setImages(Arrays.asList(productLevel3ImageWebRequest));

    productLevel3WebRequest = new ProductLevel3WebRequest();
    productLevel3WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3WebRequest.setItems(Arrays.asList(productItemLevel3WebRequest));
    productLevel3WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    productLevel3WebRequest.setImages(Arrays.asList(productLevel3ImageWebRequest));

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setDescription(INVALID_TEXT.getBytes());
    productCreationRequest.setLongDescription(INVALID_TEXT.getBytes());
    productCreationRequest.setUniqueSellingPoint(INVALID_TEXT);
    productCreationRequest.setSpecificationDetail(INVALID_TEXT);
    productCreationRequest.setForceReviewNotes(INVALID_TEXT);
    productCreationRequest.setNotes(INVALID_TEXT);
    productCreationRequest.setUom(INVALID_TEXT);
    productCreationRequest.setName(INVALID_TEXT);
    productCreationRequest.setProductStory(INVALID_TEXT);
    productCreationRequest.setProductCode(INVALID_TEXT);
    productCreationRequest.setUrl(INVALID_TEXT);
    productCreationRequest.setBrand(BRAND);
    productCreationRequest.setBrandApprovalStatus(INVALID_TEXT);
    productCreationRequest.setBrandCode(INVALID_TEXT);
    productCreationRequest.setBusinessPartnerCode(INVALID_TEXT);
    productCreationRequest.setBusinessPartnerId(INVALID_TEXT);
    productCreationRequest.setBusinessPartnerName(INVALID_TEXT);
    productCreationRequest.setGdnProductSku(INVALID_TEXT);
    productCreationRequest.setOldProductCode(INVALID_TEXT);
    productCreationRequest.setOldProductRejectionNote(INVALID_TEXT);
    productCreationRequest.setCategoryName(INVALID_TEXT);
    productCreationRequest.setCreatedBy(INVALID_TEXT);
    productCreationRequest.setUpdatedBy(INVALID_TEXT);
    productCreationRequest.setStoreId(INVALID_TEXT);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setUpcCode(INVALID_TEXT);
    productItemCreationRequest.setMerchantSku(INVALID_TEXT);
    productItemCreationRequest.setGdnProductItemSku(INVALID_TEXT);
    productItemCreationRequest.setPickupPointId(INVALID_TEXT);
    productItemCreationRequest.setProductItemId(INVALID_TEXT);
    productItemCreationRequest.setProductItemHashCode(INVALID_TEXT);
    Image image =  new Image();
    image.setLocationPath(INVALID_TEXT);
    image.setId(INVALID_TEXT);
    image.setHashCode(INVALID_TEXT);
    image.setCreatedBy(INVALID_TEXT);
    image.setUpdatedBy(INVALID_TEXT);
    image.setStoreId(INVALID_TEXT);
    productItemCreationRequest.setImages(Arrays.asList(image));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    AttributeRequest attributeRequest5 = new AttributeRequest();
    attributeRequest5.setName(Constants.FAMILY_COLOUR);
    productItemAttributeValueRequest.setAttribute(attributeRequest5);
    productItemCreationRequest.setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValueRequest));
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
        new ProductBusinessPartnerAttributeRequest();
    productBusinessPartnerAttributeRequest.setId(INVALID_TEXT);
    productBusinessPartnerAttributeRequest.setAttributeId(INVALID_TEXT);
    productBusinessPartnerAttributeRequest.setValue(INVALID_TEXT);
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));

    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setMandatory(true);
    attributeRequest.setName(Constants.WARNA);
    attributeRequest.setVariantCreation(true);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setProductAttributeName(INVALID_TEXT);
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    productCategoryRequest.setCategory(categoryRequest);

    categoryRequest.setCategoryCode(INVALID_TEXT);
    categoryRequest.setName(INVALID_TEXT);
    categoryRequest.setId(INVALID_TEXT);
    CategoryRequest productCategoryRequestCategory = new CategoryRequest();
    categoryRequest.setParentCategory(productCategoryRequestCategory);
    productCategoryRequestCategory.setId(INVALID_TEXT);

    CatalogRequest catalogRequest = new CatalogRequest();
    categoryRequest.setCatalog(catalogRequest);
    catalogRequest.setCatalogCode(INVALID_TEXT);
    catalogRequest.setCatalogType(INVALID_TEXT);
    catalogRequest.setName(INVALID_TEXT);
    catalogRequest.setId(INVALID_TEXT);
    productCreationRequest.setProductCategories(Arrays.asList(productCategoryRequest));

    productBusinessPartnerServiceRequest1 = new ProductBusinessPartnerServiceRequest();
    productBusinessPartnerServiceRequest1.setBrand(BRAND);
    productBusinessPartnerServiceRequest1.setBusinessPartnerCode(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setGdnProductSku(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setProductId(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setProductName(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setCategoryName(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setUsername(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setRequestId(INVALID_TEXT);
    productBusinessPartnerServiceRequest1.setStoreId(INVALID_TEXT);

    ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest =
        new ProductItemBusinessPartnerServiceRequest();
    productItemBusinessPartnerServiceRequest.setMerchantSku(INVALID_TEXT);
    productItemBusinessPartnerServiceRequest.setGdnProductItemSku(INVALID_TEXT);
    productItemBusinessPartnerServiceRequest.setPickupPointId(INVALID_TEXT);
    productItemBusinessPartnerServiceRequest.setProductItemId(INVALID_TEXT);
    productItemBusinessPartnerServiceRequest.setProductHashCode(INVALID_TEXT);
    productBusinessPartnerServiceRequest1
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartnerServiceRequest));

    ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest =
        new ProductBusinessPartnerAttributeServiceRequest();
    productBusinessPartnerAttributeServiceRequest.setAttributeId(INVALID_TEXT);
    productBusinessPartnerAttributeServiceRequest.setValue(INVALID_TEXT);
    productBusinessPartnerServiceRequest1
        .setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeServiceRequest));

    createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setBrandDescription(INVALID_TEXT1);
    createBrandWipRequest.setBrandLogoPath(INVALID_TEXT1);
    createBrandWipRequest.setBrandName(INVALID_TEXT1);
    createBrandWipRequest.setBusinessPartnerCode(INVALID_TEXT1);
    createBrandWipRequest.setBusinessPartnerName(INVALID_TEXT1);
    createBrandWipRequest.setProfileBannerPath(INVALID_TEXT1);

    productLevel3UpdateSummaryRequest1 = new ProductLevel3UpdateSummaryRequest();
    productLevel3UpdateSummaryRequest1.setAccessChannel(INVALID_TEXT);
    productLevel3UpdateSummaryRequest1.setMerchantSku(INVALID_TEXT);
    productLevel3UpdateSummaryRequest1.setPickupPointCode(INVALID_TEXT);
    productLevel3UpdateSummaryRequest1.setProductName(INVALID_TEXT);

    ProductLevel3PriceRequest productLevel3PriceRequest1 = new ProductLevel3PriceRequest();
    productLevel3PriceRequest1.setChannelId(INVALID_TEXT);
    productLevel3PriceRequest1.setPromotionName(INVALID_TEXT);
    productLevel3PriceRequest1.setId(INVALID_TEXT);
    productLevel3UpdateSummaryRequest1.setPrices(Arrays.asList(productLevel3PriceRequest1));

    ProductLevel3ViewConfigRequest productLevel3ViewConfigRequest = new ProductLevel3ViewConfigRequest();
    productLevel3ViewConfigRequest.setChannelId(INVALID_TEXT);
    productLevel3UpdateSummaryRequest1.setViewConfigs(Arrays.asList(productLevel3ViewConfigRequest));

    productLevel3Request = new ProductLevel3Request();
    productLevel3Request.setDescription(INVALID_TEXT);
    productLevel3Request.setUniqueSellingPoint(INVALID_TEXT);
    productLevel3Request.setSpecificationDetail(INVALID_TEXT);
    productLevel3Request.setProductName(INVALID_TEXT);
    productLevel3Request.setProductSku(INVALID_TEXT);
    productLevel3Request.setProductStory(INVALID_TEXT);
    productLevel3Request.setProductCode(INVALID_TEXT);
    productLevel3Request.setUrl(INVALID_TEXT);
    productLevel3Request.setAccessChannel(INVALID_TEXT);
    productLevel3Request.setBrand(BRAND);
    productLevel3Request.setBusinessPartnerCode(INVALID_TEXT);
    productLevel3Request.setCategoryCode(INVALID_TEXT);
    productLevel3Request.setCategoryHierarchy(INVALID_TEXT);
    productLevel3Request.setCategoryName(INVALID_TEXT);

    ProductItemLevel3Request productItemLevel3Request = new ProductItemLevel3Request();
    productItemLevel3Request.setItemName(INVALID_TEXT);
    productItemLevel3Request.setItemSku(INVALID_TEXT);
    productItemLevel3Request.setUpcCode(INVALID_TEXT);
    productItemLevel3Request.setMerchantSku(INVALID_TEXT);
    productItemLevel3Request.setPickupPointCode(INVALID_TEXT);
    productItemLevel3Request.setPickupPointName(INVALID_TEXT);
    productLevel3Request.setItems(Arrays.asList(productItemLevel3Request));

    ProductLevel3AttributeRequest productLevel3AttributeRequest = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest.setAttributeName(INVALID_TEXT);
    productLevel3AttributeRequest.setAttributeCode(INVALID_TEXT);
    productLevel3AttributeRequest.setAttributeType(INVALID_TEXT);
    productLevel3AttributeRequest.setId(INVALID_TEXT);
    productLevel3AttributeRequest.setItemSku(INVALID_TEXT);
    productLevel3Request.setAttributes(Arrays.asList(productLevel3AttributeRequest));

    ProductLevel3ImageRequest productLevel3ImageRequest = new ProductLevel3ImageRequest();
    productLevel3ImageRequest.setLocationPath(INVALID_TEXT);
    productLevel3Request.setImages(Arrays.asList(productLevel3ImageRequest));

    priceUpdateWebRequest =
        PriceUpdateWebRequest.builder().price(NORMAL_PRICE).salePrice(SALE_PRICE).channelId(CHANNEL_ID).build();

    productItemWholesalePriceWebRequest =
        ProductItemWholesalePriceWebRequest.builder().quantity(QUANTITY).wholesaleDiscount(DISCOUNT).build();

    productPriceAndStockUpdateWebRequest =
        ProductPriceAndStockUpdateWebRequest.builder().itemSku(ITEM_SKU).deltaStock(0).minimumStock(0)
            .synchronizeStock(false).categoryCode(CATEGORY_CODE).prices(Arrays.asList(priceUpdateWebRequest))
            .productItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceWebRequest)).build();

    productLevel3SummaryDetailsImageWebRequest =
        ProductLevel3SummaryDetailsImageWebRequest.builder().mainImage(true).locationPath(LOCATION_PATH)
            .markForDelete(false).reviewType("new").sequence(SEQUENCE).build();

    productLevel3CommonImageWebRequest =
      ProductLevel3CommonImageWebRequest.builder().mainImages(true).locationPath(LOCATION_PATH)
        .markForDelete(false).reviewType("new").sequence(SEQUENCE).build();

    productPriceStockAndImagesWebRequest = ProductPriceStockAndImagesWebRequest.builder().itemSku(ITEM_SKU)
        .prices(Arrays.asList(productLevel3PriceWebRequest))
        .viewConfigs(Arrays.asList(productLevel3ViewConfigWebRequest))
        .images(Arrays.asList(productLevel3SummaryDetailsImageWebRequest)).build();

    productLevel3UpdateWebRequest = new ProductLevel3UpdateWebRequest();
    productLevel3UpdateWebRequest.setProductSku(PRODUCT_SKU);
    productLevel3UpdateWebRequest.setSynchronize(true);
    productLevel3UpdateWebRequest.setProductEditable(true);

    pickupPointUpdateWebRequest = new PickupPointUpdateWebRequest();
    pickupPointUpdateWebRequest.setProductSku(PRODUCT_SKU);
    pickupPointUpdateWebRequest.setDifferentLocation(true);
    pickupPointUpdateWebRequest.setMarkDefaultAddress(true);
    pickupPointUpdateWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);
    pickupPointUpdateWebRequest.setNeedCorrection(false);
    PickupPointUpdateItemsWebRequest pickupPointUpdateItemsWebRequest = new PickupPointUpdateItemsWebRequest();
    pickupPointUpdateItemsWebRequest.setItemSku(ITEM_SKU);
    pickupPointUpdateItemsWebRequest.setItemName(ITEM_NAME);
    pickupPointUpdateItemsWebRequest.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointUpdateWebRequest.setItemsPickupPoint(Arrays.asList(pickupPointUpdateItemsWebRequest));

    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);

    productLevel3UpdateWebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));

    preOrderWebRequest =
        PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productLevel3LogisticsWebRequest.setSelected(true);

    preOrderRequest =
        PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();

    productLevel3UpdateWebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));

    QuickEditWebRequest quickEditWebRequest1 = new QuickEditWebRequest();
    quickEditWebRequest1.setItemSku(ITEM_SKU);
    ProductLevel3PriceWebRequest priceUpdateWebRequest = new ProductLevel3PriceWebRequest();
    priceUpdateWebRequest.setPrice(10000.0);
    priceUpdateWebRequest.setSalePrice(10000.0);
    quickEditWebRequest1.setPrices(Arrays.asList(priceUpdateWebRequest));
    quickEditWebRequest1.setStatus("ONLINE");
    QuickEditWebRequest quickEditWebRequest2 = new QuickEditWebRequest();
    quickEditWebRequest2.setPrices(Arrays.asList(new ProductLevel3PriceWebRequest()));
    quickEditWebRequest2.setStatus("OFFLINE");
    QuickEditWebRequest quickEditWebRequest3 = new QuickEditWebRequest();
    quickEditWebRequest3.setPrices(Arrays.asList(new ProductLevel3PriceWebRequest()));
    quickEditWebRequest3.setStatus("TEASER");
    QuickEditWebRequest quickEditWebRequest4 = new QuickEditWebRequest();
    quickEditWebRequest4.setPrices(Arrays.asList(new ProductLevel3PriceWebRequest()));
    quickEditWebRequest4.setStatus("B2B");
    quickEditWebRequests = Arrays.asList(quickEditWebRequest1, quickEditWebRequest2, quickEditWebRequest3, quickEditWebRequest4);

    productSummaryV2WebRequest = ProductSummaryV2WebRequest.builder().suspended(true).archived(true)
        .categoryCodes(Collections.singletonList(CATEGORY_CODE))
        .pickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)).merchantCode(BUSINESS_PARTNER_CODE)
        .suspended(true).keyword(KEYWORD).inStock(true).b2cActivated(true).b2bActivated(true).build();

    productLevel3CommonImageWebRequest = new ProductLevel3CommonImageWebRequest();
    productLevel3CommonImageWebRequest.setMainImages(true);
    productLevel3CommonImageWebRequest.setSequence(new Integer(SEQUENCE));
    productLevel3CommonImageWebRequest.setLocationPath(LOCATION_PATH);

    productL3CommonImageRequest = new ProductL3CommonImageRequest();
    productL3CommonImageRequest.setMainImage(true);
    productL3CommonImageRequest.setSequence(new Integer(SEQUENCE));
    productL3CommonImageRequest.setLocationPath(LOCATION_PATH);


    productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productVariantUpdateRequest.setProductEditable(true);
    productVariantUpdateRequest.setSynchronize(true);
    productVariantUpdateRequest.setNeedCorrection(true);
    productVariantUpdateRequest.setCopyToAllVariantImages(Arrays.asList(imageRequest));
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointV2Request));
    productVariantUpdateRequest.setDeletePickupPoints(Collections.singletonList(itemPickupPointDeleteRequest));

    imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageRequest.setLocationPath(LOCATION_PATH);

    productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setDescription(INVALID_TEXT);
    productL3UpdateRequest.setUniqueSellingPoint(INVALID_TEXT);
    productL3UpdateRequest.setSpecificationDetail(INVALID_TEXT);
    productL3UpdateRequest.setProductName(INVALID_TEXT);
    productL3UpdateRequest.setProductSku(INVALID_TEXT);
    productL3UpdateRequest.setProductStory(INVALID_TEXT);
    productL3UpdateRequest.setProductCode(INVALID_TEXT);
    productL3UpdateRequest.setUrl(INVALID_TEXT);
    productL3UpdateRequest.setAccessChannel(INVALID_TEXT);
    productL3UpdateRequest.setBrand(BRAND);
    productL3UpdateRequest.setBusinessPartnerCode(INVALID_TEXT);
    productL3UpdateRequest.setCategoryCode(INVALID_TEXT);
    productL3UpdateRequest.setCategoryHierarchy(INVALID_TEXT);
    productL3UpdateRequest.setCategoryName(INVALID_TEXT);
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest));
    productL3UpdateRequest.setCommonImages(Arrays.asList(imageRequest));

    defaultConfigurationAndPickupPointRequest = new DefaultConfigurationAndPickupPointRequest();
    defaultConfigurationAndPickupPointRequest.setProductSettings(new ProductSettingsRequest());

    quickEditV2WebRequest.setItemSku(ITEM_SKU);
    quickEditV2WebRequest.setCncActivated(true);
    quickEditV2WebRequest.setDeltaStock(2);
    quickEditV2WebRequest.setVersion(10L);
    quickEditV2WebRequest.setPrices(Collections.singleton(productLevel3PriceWebRequest));
    quickEditV2WebRequest.setStatus(STATUS);
    quickEditV2WebRequest.setSynchronizeStock(true);
    quickEditV2WebRequest.setWholesalePriceActivated(true);
    quickEditV2WebRequest.setOff2OnActiveFlag(true);

    variantPriceStockAndImagesWebRequest = new ProductVariantPriceStockAndImagesWebRequest();
    variantPriceStockAndImagesWebRequest.setItemSku(ITEM_SKU);
    variantPriceStockAndImagesWebRequest.setProductSku(PRODUCT_SKU);
    variantPriceStockAndImagesWebRequest.setImages(Arrays.asList(productLevel3CommonImageWebRequest));
    variantPriceStockAndImagesWebRequest.setModifiedItemPickupPoints(
      Collections.singletonList(itemPickupPointWebRequest));
    itemPickupPointDeleteWebRequest = new ItemPickupPointDeleteWebRequest();
    itemPickupPointDeleteWebRequest.setPickupPointId(ITEM_SKU);

    productVariantUpdateWebRequest = new ProductVariantUpdateWebRequest();
    productVariantUpdateWebRequest.setProductEditable(true);
    productVariantUpdateWebRequest.setSynchronize(true);
    productVariantUpdateWebRequest.setNeedCorrection(true);
    productVariantUpdateWebRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateWebRequest.setProductItems(Arrays.asList(variantPriceStockAndImagesWebRequest));
    productVariantUpdateWebRequest.setCommonImages(Arrays.asList(productLevel3SummaryDetailsImageWebRequest));
    productVariantUpdateWebRequest.setAddPickupPoints(Arrays.asList(itemPickupPointWebRequest));
    productVariantUpdateWebRequest.getAddPickupPoints().get(0).setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceWebRequest));
    productVariantUpdateWebRequest.setDeletePickupPoints(Arrays.asList(itemPickupPointDeleteWebRequest));

    productDetailsRequest = new ProductDetailsRequest();
    productDetailsRequest.setProductSku(PRODUCT_SKU);
    productDetailsRequest.setPickupPointCode(PICKUP_POINT_CODE);
    productDetailsRequest.setItemSku(ITEM_SKU);

    manualQRCodeRequest = new ManualQRCodeRequest();
    manualQRCodeRequest.setAllStores(false);
    manualQRCodeRequest.setIsDarkTheme(true);
    manualQRCodeRequest.setQrPerPage(1);
    manualQRCodeRequest.setTemplateSize(TemplateSize.A5);
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ITEM);
    List<ProductDetailsRequest> requests = new ArrayList<>();
    requests.add(productDetailsRequest);
    manualQRCodeRequest.setProductDetailsRequestList(requests);
    manualQRCodeRequest.setCncActivated(false);

  }

  @Test
  public void toProductBusinessPartnerRequestTest(){
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(
        ProductItemLogisticsWebRequest.builder().logisticProductCode(LOGISTICS_PRODUCT_CODE).isSelected(true).build());
    productItemBusinessPartnerServiceRequest
        .setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    ProductBusinessPartnerRequest productBusinessPartnerRequest = RequestHelper
        .toProductBusinessPartnerRequest(productBusinessPartnerServiceRequest);
    assertEquals(BUSINESS_PARTNER_CODE,productBusinessPartnerRequest.getBusinessPartnerId());
    assertEquals(ATTRIBUTE_CODE,productBusinessPartnerRequest.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(true,productBusinessPartnerRequest.getProductItemBusinessPartners().get(0).isBuyable());
  }

  @Test
  public void toProductBusinessPartnerRequestWithWholesalePriceTest() {
    productBusinessPartnerServiceRequest.getProductItemBusinessPartners().get(0)
        .setProductItemWholesalePriceRequests(new ArrayList<>());
    productBusinessPartnerServiceRequest.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests()
        .add(new ProductItemWholesalePriceWebRequest(2, 10));
    ProductBusinessPartnerRequest productBusinessPartnerRequest = RequestHelper
        .toProductBusinessPartnerRequest(productBusinessPartnerServiceRequest);
    assertEquals(BUSINESS_PARTNER_CODE,productBusinessPartnerRequest.getBusinessPartnerId());
    assertEquals(ATTRIBUTE_CODE,productBusinessPartnerRequest.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(true,productBusinessPartnerRequest.getProductItemBusinessPartners().get(0).isBuyable());
    assertEquals(1,
        productBusinessPartnerRequest.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests()
            .size());
  }

  @Test
  public void getReadAccessibilitiesTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesRead());
      Map<String, Boolean> map = RequestHelper.getReadAccessibilities(profileResponse, false);
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK));
    }
  }

  @Test
  public void getReadAccessibilitiesTestExternalUser(){
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesRead());
      profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_RC);
      Map<String, Boolean> map = RequestHelper.getReadAccessibilities(profileResponse, true);
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE));
      assertEquals(false, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK));
    }
  }

  @Test
  public void getReadAccessibilitiesTestExternalUserTC() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesRead());
      profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_TC);
      Map<String, Boolean> map = RequestHelper.getReadAccessibilities(profileResponse, true);
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK));
    }
  }

  @Test
  public void getReadAccessibilitiesTestExternalUserCC() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesRead());
      profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_CC);
      Map<String, Boolean> map = RequestHelper.getReadAccessibilities(profileResponse, true);
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK));
    }
  }

  @Test
  public void getReadAccessibilitiesTestExternalUserTD(){
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesRead());
      profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_TD);
      Map<String, Boolean> map = RequestHelper.getReadAccessibilities(profileResponse, true);
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK));
    }
  }

  @Test
  public void getEditAccessibilitiesTest(){
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilitiesEdit());
      Map<String, Boolean> map = RequestHelper.getEditAccessibilities();
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_O2O));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT));
      assertEquals(true, map.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE));
    }
  }

  @Test
  public void toActiveProductRequestTest() {
    ActiveProductRequest activeProductRequest = RequestHelper.toActiveProductRequest(activeProductWebRequest, Collections
        .singletonList(CATEGORY_CODE));
    assertNotNull(activeProductRequest);
    assertEquals(SEARCH_KEY, activeProductRequest.getSearchKey());
    assertEquals(Collections.singletonList(CATEGORY_CODE), activeProductRequest.getCategoryCodes());
  }

  @Test
  public void toActiveProductRequestTestFromProductL3ListingWebRequestForQR() {
    ActiveProductRequest activeProductRequest = RequestHelper.toActiveProductRequest(
        productL3ListingWebRequest);
    assertNotNull(activeProductRequest);
    assertEquals(SEARCH_KEY, activeProductRequest.getSearchKey());
    assertEquals(Collections.singletonList(CATEGORY_CODE), activeProductRequest.getCategoryCodes());
    assertEquals(Collections.singletonList(PICKUP_POINT_CODE), activeProductRequest.getPickupPointCodes());
  }

  @Test
  public void toSummaryFilterRequestTest() {
    SummaryFilterRequest summaryFilterRequest = RequestHelper.toSummaryFilterRequest(suspensionWebRequest);
    assertNotNull(summaryFilterRequest);
    assertEquals(SEARCH_KEY, summaryFilterRequest.getSearchKeyword());
    assertEquals(BUSINESS_PARTNER_CODE, summaryFilterRequest.getBusinessPartnerCode());
    assertEquals(CATEGORY_CODE, summaryFilterRequest.getCategoryCode());
    assertEquals(NAMEKEY, summaryFilterRequest.getNameKey());
    assertEquals(SORTTYPE, summaryFilterRequest.getSortType());
  }

  @Test
  public void toSummaryFilterRequestFromInActiveProductWebRequestTest() {
    SummaryFilterRequest summaryFilterRequest = RequestHelper.toSummaryFilterRequestFromInActiveProductWebRequest(
        InActiveProductWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
            .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
            .searchKey(SEARCH_KEY).build());
    assertNotNull(summaryFilterRequest);
    assertEquals(SEARCH_KEY, summaryFilterRequest.getSearchKeyword());
    assertEquals(BUSINESS_PARTNER_CODE, summaryFilterRequest.getBusinessPartnerCode());
    assertEquals(CATEGORY_CODE, summaryFilterRequest.getCategoryCodes().get(0));
  }

  @Test
  public void toProductLevel3SummaryRequestFromInActiveProductWebRequestTest() {
    ProductLevel3SummaryRequest summaryRequest =
        RequestHelper.toProductLevel3SummaryRequestFromInActiveProductWebRequestTest(inInActiveProductWebRequest);
    assertNotNull(summaryRequest);
    assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
    assertEquals(SEARCH_KEY, summaryRequest.getSearchKey());
    assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
  }

  @Test
  public void toProductLevel3SummaryRequestTest() {
    ProductLevel3SummaryRequest summaryRequest = RequestHelper.toProductLevel3SummaryRequest(activeProductWebRequest1);
    assertNotNull(summaryRequest);
    assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
    assertEquals(SEARCH_KEY, summaryRequest.getSearchKey());
    assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
    assertTrue(summaryRequest.getBuyable());
    assertNull(summaryRequest.getDisplayable());
    assertNull(summaryRequest.getInventoryFilter());
  }

  @Test
  public void toProductLevel3SummaryRequest_withAvailableInventoryFilterTest() {
    activeProductWebRequest1.setInventoryFilter(ProductLevel3InventoryCriteria.AVAILABLE.toString());
    ProductLevel3SummaryRequest summaryRequest = RequestHelper.toProductLevel3SummaryRequest(activeProductWebRequest1);
    assertNotNull(summaryRequest);
    assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
    assertEquals(SEARCH_KEY, summaryRequest.getSearchKey());
    assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
    assertTrue(summaryRequest.getBuyable());
    assertNull(summaryRequest.getDisplayable());
    assertEquals(ProductLevel3InventoryCriteria.AVAILABLE, summaryRequest.getInventoryFilter());
  }

  @Test
  public void toProductLevel3SummaryRequest_withStockAlertInventoryFilterTest() {
    activeProductWebRequest1.setInventoryFilter(ProductLevel3InventoryCriteria.STOCK_ALERT.toString());
    ProductLevel3SummaryRequest summaryRequest = RequestHelper.toProductLevel3SummaryRequest(activeProductWebRequest1);
    assertNotNull(summaryRequest);
    assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
    assertEquals(SEARCH_KEY, summaryRequest.getSearchKey());
    assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
    assertTrue(summaryRequest.getBuyable());
    assertNull(summaryRequest.getDisplayable());
    assertEquals(ProductLevel3InventoryCriteria.STOCK_ALERT, summaryRequest.getInventoryFilter());
  }

  @Test
  public void toProductLevel3SummaryRequest_withOOSInventoryFilterTest() {
    activeProductWebRequest1.setInventoryFilter(ProductLevel3InventoryCriteria.OOS.toString());
    activeProductWebRequest1.setDisplayable(false);
    ProductLevel3SummaryRequest summaryRequest = RequestHelper.toProductLevel3SummaryRequest(activeProductWebRequest1);
    assertNotNull(summaryRequest);
    assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
    assertEquals(SEARCH_KEY, summaryRequest.getSearchKey());
    assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
    assertTrue(summaryRequest.getBuyable());
    assertFalse(summaryRequest.getDisplayable());
    assertEquals(ProductLevel3InventoryCriteria.OOS, summaryRequest.getInventoryFilter());
  }

  @Test
  public void toProductLevel3WipSummaryRequestWithInprogressStateTest() {
    ProductLevel3WipSummaryRequest summaryRequest =
        RequestHelper.toProductLevel3WipSummaryRequest(inProcessProductWebRequest);
    assertNotNull(summaryRequest);
    assertEquals(PRODUCT_NAME, summaryRequest.getProductName());
    assertEquals(IN_PROGRESS_CRITERIA, summaryRequest.getCriteria().toString());
    assertEquals(BUSINESS_PARTNER_CODE, summaryRequest.getBusinessPartnerCode());
  }

  @Test
  public void toProductLevel3WipSummaryRequestWithFailedStateTest() {
    inProcessProductWebRequest.setCriteria(FAILED_CRITERIA);
    ProductLevel3WipSummaryRequest summaryRequest =
        RequestHelper.toProductLevel3WipSummaryRequest(inProcessProductWebRequest);
    assertNotNull(summaryRequest);
    assertEquals(PRODUCT_NAME, summaryRequest.getProductName());
    assertEquals(FAILED_CRITERIA, summaryRequest.getCriteria().toString());
    assertEquals(BUSINESS_PARTNER_CODE, summaryRequest.getBusinessPartnerCode());
  }

  @Test
  public void toProductLevel3WipSummaryRequestWithNeedCorrectionStateTest() {
    inProcessProductWebRequest.setCriteria(NEED_CORRECTION_CRITERIA);
    ProductLevel3WipSummaryRequest summaryRequest =
        RequestHelper.toProductLevel3WipSummaryRequest(inProcessProductWebRequest);
    assertNotNull(summaryRequest);
    assertEquals(PRODUCT_NAME, summaryRequest.getProductName());
    assertEquals(NEED_CORRECTION_CRITERIA, summaryRequest.getCriteria().toString());
    assertEquals(BUSINESS_PARTNER_CODE, summaryRequest.getBusinessPartnerCode());
    assertEquals(Arrays.asList(CATEGORY_CODE), summaryRequest.getCategoryCodes());
  }

  @Test
  public void toProductLevel3WipSummaryRequestWithInvalidStateTest() {
    inProcessProductWebRequest.setCriteria(PRODUCT_NAME);
    ProductLevel3WipSummaryRequest result = null;
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.toProductLevel3WipSummaryRequest(inProcessProductWebRequest));
  }

  public void setProductOff2OnFlagNullAndRoundOffSalePriceConditionTrueTest() {
    RequestHelper
        .setProductOff2OnFlagNullAndRoundOffSalePrice(productLevel3OrderResponse, productLevel3UpdateSummaryRequest);
    assertNull(productLevel3UpdateSummaryRequest.getOff2OnActiveFlag());
    assertEquals(1001, productLevel3UpdateSummaryRequest.getPrices().get(0).getSalePrice().longValue());
  }

  @Test
  public void setProductOff2OnFlagNullAndRoundOffSalePriceConditionFalseTest() {
    productLevel3OrderResponse.setItems(null);
    RequestHelper
        .setProductOff2OnFlagNullAndRoundOffSalePrice(productLevel3OrderResponse, productLevel3UpdateSummaryRequest);
    assertFalse(productLevel3UpdateSummaryRequest.getOff2OnActiveFlag());
    assertEquals(1001, productLevel3UpdateSummaryRequest.getPrices().get(0).getSalePrice().longValue());
  }

  @Test
  public void toBulkProcessUpdateRequestTest() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest = RequestHelper
        .toBulkProcessUpdateRequest(BUSINESS_PARTNER_CODE, USERNAME, privilegeMap, generateDummyExcelMultipartFile(),
            profileResponse, Constants.BULK_PROCESS_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpdateRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUpdateRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpdateRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpdateRequest.getUpdatedBy());
    assertEquals(privilegeMap, bulkProcessUpdateRequest.getPrivilegedMap());
    assertNotNull(bulkProcessUpdateRequest.getFileContent());
    assertTrue(privilegeMap.get(PRIVILEGED_EDIT_O2O));
  }

  @Test
  public void toBulkProcessV2RequestTest() throws Exception {
    BulkProcessV2Request bulkProcessUpdateRequest = RequestHelper
      .toBulkProcessV2Request(BUSINESS_PARTNER_CODE, USERNAME, privilegeMap, generateDummyExcelMultipartFile(),
        profileResponse, Collections.singleton(PICKUP_POINT_CODE), Constants.BULK_PROCESS_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpdateRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUpdateRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpdateRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpdateRequest.getUpdatedBy());
    assertEquals(privilegeMap, bulkProcessUpdateRequest.getPrivilegedMap());
    assertNotNull(bulkProcessUpdateRequest.getFileContent());
    assertTrue(privilegeMap.get(PRIVILEGED_EDIT_O2O));
    assertTrue(bulkProcessUpdateRequest.getAccessiblePickupPoints().contains(PICKUP_POINT_CODE));
  }

  @Test
  public void toBulkProcessV2Request_emptyPrivilegesTest() throws Exception {
    BulkProcessV2Request bulkProcessUpdateRequest = RequestHelper
      .toBulkProcessV2Request(BUSINESS_PARTNER_CODE, USERNAME, new HashMap<>(),
        generateDummyExcelMultipartFile(),
        profileResponse, Collections.singleton(PICKUP_POINT_CODE), Constants.BULK_PROCESS_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpdateRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUpdateRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpdateRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpdateRequest.getUpdatedBy());
    assertNotNull(bulkProcessUpdateRequest.getFileContent());
    assertTrue(privilegeMap.get(PRIVILEGED_EDIT_O2O));
    assertTrue(bulkProcessUpdateRequest.getAccessiblePickupPoints().contains(PICKUP_POINT_CODE));
  }

  @Test
  public void toBulkProcessV2Request_privilegeFalseTest() throws Exception {
    privilegeMap.put(PRIVILEGED_EDIT_O2O, false);
    BulkProcessV2Request bulkProcessUpdateRequest = RequestHelper
      .toBulkProcessV2Request(BUSINESS_PARTNER_CODE, USERNAME, privilegeMap,
        generateDummyExcelMultipartFile(),
        profileResponse, Collections.singleton(PICKUP_POINT_CODE), Constants.BULK_PROCESS_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpdateRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUpdateRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpdateRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpdateRequest.getUpdatedBy());
    assertEquals(privilegeMap, bulkProcessUpdateRequest.getPrivilegedMap());
    assertNotNull(bulkProcessUpdateRequest.getFileContent());
    assertFalse(privilegeMap.get(PRIVILEGED_EDIT_O2O));
    assertTrue(bulkProcessUpdateRequest.getAccessiblePickupPoints().contains(PICKUP_POINT_CODE));
  }

  @Test
  public void toBulkProcessV2Request_profileO2OFalseTest() throws Exception {
    profileResponse.getCompany().setOfflineToOnlineFlag(false);
    BulkProcessV2Request bulkProcessUpdateRequest = RequestHelper
      .toBulkProcessV2Request(BUSINESS_PARTNER_CODE, USERNAME, privilegeMap,
        generateDummyExcelMultipartFile(),
        profileResponse, Collections.singleton(PICKUP_POINT_CODE), Constants.BULK_PROCESS_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpdateRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUpdateRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpdateRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpdateRequest.getUpdatedBy());
    assertNotNull(bulkProcessUpdateRequest.getFileContent());
    assertFalse(bulkProcessUpdateRequest.getPrivilegedMap().get(PRIVILEGED_EDIT_O2O));
    assertTrue(bulkProcessUpdateRequest.getAccessiblePickupPoints().contains(PICKUP_POINT_CODE));
  }


  @Test
  public void toBulkProcessUpsertOfflineItemRequestTest() throws Exception {
    BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest = RequestHelper
        .toBulkProcessUpsertOfflineItemRequest(BUSINESS_PARTNER_CODE, USERNAME,
          generateDummyExcelMultipartFile(), Collections.singleton(PICKUP_POINT_CODE));
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpsertOfflineItemRequest.getBusinessPartnerCode());
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(), bulkProcessUpsertOfflineItemRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkProcessUpsertOfflineItemRequest.getFileName());
    assertEquals(USERNAME, bulkProcessUpsertOfflineItemRequest.getUpdatedBy());
    assertNotNull(bulkProcessUpsertOfflineItemRequest.getFileContent());
    assertEquals(PICKUP_POINT_CODE,
      bulkProcessUpsertOfflineItemRequest.getAccessiblePickupPoints().stream().findFirst().get());
  }

  @Test
  public void toBulkProcessUploadRequestTest() throws Exception {
    BulkProcessUploadRequest bulkProcessUploadRequest = RequestHelper
        .toBulkProcessUploadRequest(BUSINESS_PARTNER_CODE, args, files);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequest.getBulkProcessType());
    assertEquals(args, bulkProcessUploadRequest.getArgs());
    assertEquals(files, bulkProcessUploadRequest.getFiles());
  }

  @Test
  public void toBulkProcessExternalUploadRequestTest() throws Exception {
    BulkProcessExternalUploadRequest bulkProcessUploadRequest = RequestHelper.toBulkProcessExternalUploadRequest(
        "zip.zip", files, "externalUpload", PICKUP_POINT_CODE, "", BUSINESS_PARTNER_CODE);
    assertEquals("zip.zip", bulkProcessUploadRequest.getZipFileName());
    assertEquals(files, bulkProcessUploadRequest.getFiles());
    assertEquals("externalUpload",bulkProcessUploadRequest.getBulkProcessCode());
    assertEquals(PICKUP_POINT_CODE, bulkProcessUploadRequest.getPickupPointCode());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequest.getBusinessPartnerCode());
  }

  @Test
  public void toProductLevel3RequestFromProductLevel3WebRequestTest(){
    List<ProductItemLevel3LogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(ProductItemLevel3LogisticsWebRequest.builder()
        .logisticProductCode(LOGISTICS_PRODUCT_CODE).isSelected(true).build());
    productLevel3WebRequest.setLogistics(productItemLogisticsWebRequests);
    ProductLevel3Request productLevel3Request = RequestHelper
        .toProductLevel3RequestFromProductLevel3WebRequest(productLevel3WebRequest);
    assertEquals(BUSINESS_PARTNER_CODE,productLevel3Request.getBusinessPartnerCode());
    assertEquals(ITEM_SKU, productLevel3Request.getItems().get(0).getItemSku());
    assertEquals(ATTRIBUTE_CODE ,productLevel3Request.getAttributes().get(0).getAttributeCode());
    assertEquals(LOCATION_PATH,productLevel3Request.getImages().get(0).getLocationPath());
  }

  @Test
  public void toProductItemLevel3RequesFromProductItemLevel3RequestTest() {
    ProductItemLevel3Request productItemLevel3Request =
        RequestHelper.toProductItemLevel3RequesFromProductItemLevel3Request(productItemLevel3WebRequest);
    assertEquals(ITEM_SKU, productItemLevel3Request.getItemSku());
    assertEquals(new Double(PRICE), productItemLevel3Request.getPrices().get(0).getPrice());
    assertEquals(CHANNEL_ID, productItemLevel3Request.getViewConfigs().get(0).getChannelId());
    assertEquals(LOCATION_PATH, productItemLevel3Request.getImages().get(0).getLocationPath());
  }

  @Test
  public void validateRequestForProductCreationRequest() throws Exception {
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
    assertEquals(VALID_TEXT, productCreationRequest.getBusinessPartnerCode());
    assertEquals(VALID_TEXT, new String(productCreationRequest.getDescription(), "UTF-8"));
    assertEquals(VALID_TEXT, new String(productCreationRequest.getLongDescription(), "UTF-8"));
    assertEquals(VALID_TEXT, productCreationRequest.getUniqueSellingPoint());
    assertEquals(VALID_TEXT, productCreationRequest.getSpecificationDetail());
    assertEquals(VALID_TEXT, productCreationRequest.getForceReviewNotes());
    assertEquals(VALID_TEXT, productCreationRequest.getNotes());
    assertEquals(VALID_TEXT, productCreationRequest.getUom());
    assertEquals(VALID_TEXT, productCreationRequest.getName());
    assertEquals(VALID_TEXT, productCreationRequest.getProductStory());
    assertEquals(VALID_TEXT, productCreationRequest.getProductCode());
    assertEquals(VALID_TEXT, productCreationRequest.getUrl());
    assertEquals(BRAND, productCreationRequest.getBrand());
    assertEquals(VALID_TEXT, productCreationRequest.getBrandApprovalStatus());
    assertEquals(VALID_TEXT, productCreationRequest.getBrandCode());
    assertEquals(VALID_TEXT, productCreationRequest.getBusinessPartnerCode());
    assertEquals(VALID_TEXT, productCreationRequest.getBusinessPartnerId());
    assertEquals(VALID_TEXT, productCreationRequest.getBusinessPartnerName());
    assertEquals(VALID_TEXT, productCreationRequest.getGdnProductSku());
    assertEquals(VALID_TEXT, productCreationRequest.getOldProductCode());
    assertEquals(VALID_TEXT, productCreationRequest.getOldProductRejectionNote());
    assertEquals(VALID_TEXT, productCreationRequest.getCategoryName());
    assertEquals(VALID_TEXT, productCreationRequest.getCreatedBy());
    assertEquals(VALID_TEXT, productCreationRequest.getUpdatedBy());
    assertEquals(VALID_TEXT, productCreationRequest.getStoreId());
    assertEquals(VALID_TEXT, productCreationRequest.getProductItemRequests().get(0).getGdnProductItemSku());
    assertEquals(VALID_TEXT, productCreationRequest.getProductItemRequests().get(0).getMerchantSku());
    assertEquals(VALID_TEXT, productCreationRequest.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(VALID_TEXT, productCreationRequest.getProductCategories().get(0).getCategory().getCategoryCode());
  }

  @Test
  public void validateRequestForProductCreationRequestWithStyling() throws Exception {
    productCreationRequest.setDescription(STYLED_DESCRIPTION.getBytes());
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
    Assertions.assertEquals(VALID_STYLED_DESCRIPTION, new String(productCreationRequest.getDescription()));
  }

  @Test
  public void validateRequestForProductBusinessPartnerServiceRequest() throws Exception {
    RequestHelper.validateRequest(productBusinessPartnerServiceRequest1);
    assertEquals(BRAND, productBusinessPartnerServiceRequest1.getBrand());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getBusinessPartnerCode());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getGdnProductSku());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getProductId());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getProductName());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getCategoryName());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getUsername());
    assertEquals(VALID_TEXT, productBusinessPartnerServiceRequest1.getRequestId());
    assertEquals(VALID_TEXT,
        productBusinessPartnerServiceRequest1.getProductItemBusinessPartners().get(0).getMerchantSku());
    assertEquals(VALID_TEXT,
        productBusinessPartnerServiceRequest1.getProductItemBusinessPartners().get(0).getGdnProductItemSku());
    assertEquals(VALID_TEXT,
        productBusinessPartnerServiceRequest1.getProductItemBusinessPartners().get(0).getProductItemId());
    assertEquals(VALID_TEXT,
        productBusinessPartnerServiceRequest1.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(VALID_TEXT,
        productBusinessPartnerServiceRequest1.getProductBusinessPartnerAttributes().get(0).getValue());
  }

  @Test
  public void validateRequestForCreateBrandWipRequest() {
    RequestHelper.validateRequest(createBrandWipRequest);
    assertEquals(VALID_TEXT1, createBrandWipRequest.getBrandName());
    assertEquals(VALID_TEXT1, createBrandWipRequest.getBusinessPartnerCode());
    assertEquals(VALID_TEXT1, createBrandWipRequest.getBrandDescription());
    assertEquals(VALID_TEXT1, createBrandWipRequest.getProfileBannerPath());
    assertEquals(VALID_TEXT1, createBrandWipRequest.getBrandLogoPath());
  }

  @Test
  public void validateRequestForProductLevel3UpdateSummaryRequest() {
    RequestHelper.validateRequest(productLevel3UpdateSummaryRequest1);
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getAccessChannel());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getMerchantSku());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getPickupPointCode());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getProductName());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getPrices().get(0).getChannelId());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getPrices().get(0).getPromotionName());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getPrices().get(0).getId());
    assertEquals(VALID_TEXT, productLevel3UpdateSummaryRequest1.getViewConfigs().get(0).getChannelId());
  }

  @Test
  public void validateRequestForProductLevel3Request() {
    RequestHelper.validateRequest(productLevel3Request);
    assertEquals(VALID_TEXT, productLevel3Request.getDescription());
    assertEquals(VALID_TEXT, productLevel3Request.getUniqueSellingPoint());
    assertEquals(VALID_TEXT, productLevel3Request.getSpecificationDetail());
    assertEquals(VALID_TEXT, productLevel3Request.getProductName());
    assertEquals(VALID_TEXT, productLevel3Request.getProductSku());
    assertEquals(VALID_TEXT, productLevel3Request.getProductStory());
    assertEquals(VALID_TEXT, productLevel3Request.getProductCode());
    assertEquals(VALID_TEXT, productLevel3Request.getUrl());
    assertEquals(VALID_TEXT, productLevel3Request.getAccessChannel());
    assertEquals(BRAND, productLevel3Request.getBrand());
    assertEquals(VALID_TEXT, productLevel3Request.getItems().get(0).getItemSku());
    assertEquals(VALID_TEXT, productLevel3Request.getItems().get(0).getItemName());
    assertEquals(VALID_TEXT, productLevel3Request.getItems().get(0).getUpcCode());
    assertEquals(VALID_TEXT, productLevel3Request.getAttributes().get(0).getAttributeName());
    assertEquals(VALID_TEXT, productLevel3Request.getAttributes().get(0).getAttributeCode());
    assertEquals(VALID_TEXT, productLevel3Request.getAttributes().get(0).getAttributeType());
    assertEquals(VALID_TEXT, productLevel3Request.getImages().get(0).getLocationPath());
  }

  @Test
  public void validateRequestForProductLevel3RequestEmptyDescription() {
    productLevel3Request.setDescription(StringUtils.EMPTY);
    RequestHelper.validateRequest(productLevel3Request);
    assertEquals(StringUtils.EMPTY, productLevel3Request.getDescription());
  }

  @Test
  public void validateRequestForProductLevel3RequestContainVideoUrl() {
    productLevel3Request.setDescription(INVALID_URL_TEXT);
    RequestHelper.validateRequest(productLevel3Request);
  }

  @Test
  public void validateRequestForProductLevel3RequestInvalidTest1() {
    productLevel3Request.setDescription(INVALID_TEXT2);
    RequestHelper.validateRequest(productLevel3Request);
    assertEquals("testing text; HK 1 ; HK; HK 1 > HK 2", productLevel3Request.getDescription());
  }

  @Test
  public void validateRequestForProductLevel3RequestInvalidTest2() {
    productLevel3Request.setDescription(INVALID_TEXT2);
    RequestHelper.validateRequest(productLevel3Request);
    assertEquals("testing text; HK 1 ; HK; HK 1 > HK 2", productLevel3Request.getDescription());
  }

  @Test
  public void validateRequestForProductLevel3RequestInvalidTest3() {
    productLevel3Request.setDescription(INVALID_TEXT4);
    RequestHelper.validateRequest(productLevel3Request);
    assertEquals("testing text; HK 1 ; HK; HK 1 & HK 2", productLevel3Request.getDescription());
  }


  @Test
  public void validateVideoURLs() {
    String response = RequestHelper.validateVideoURLs(INVALID_URL_TEXT);
    assertEquals(VALID_URL_TEXT, response);
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  private String[] accessibilitiesEdit() {
    String[] accessibilities = {Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA};
    return accessibilities;
  }

  private String[] accessibilitiesRead() {
    String[] accessibilities = {Accessibilty.MAINTAIN_PRODUK_SKU_O2O_VIEW_ONLY, Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_VIEW_ONLY,
            Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_VIEW_ONLY, Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_VIEW_ONLY,
            Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_VIEW_ONLY};
    return accessibilities;
  }

  @Test
  public void toProductPriceAndWholesaleRequestTest() {
    ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest =
        RequestHelper.toProductPriceAndWholesaleRequest(productPriceAndStockUpdateWebRequest);
    assertEquals(CHANNEL_ID, productPriceAndWholesaleRequest.getChannel());
    assertEquals(SALE_PRICE, productPriceAndWholesaleRequest.getOfferPrice(), 0.0);
    assertEquals(NORMAL_PRICE, productPriceAndWholesaleRequest.getListPrice(), 0.0);
    assertEquals(QUANTITY, productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().get(0).getQuantity());
    assertEquals(DISCOUNT,
        productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().get(0).getWholesaleDiscount(), 0.0);
  }

  @Test
  public void toUpdateItemsPriceStockImagesRequestTest() {
    UpdateItemsPriceStockImagesWebRequest updateItemsPriceStockImagesWebRequest =
        new UpdateItemsPriceStockImagesWebRequest();
    updateItemsPriceStockImagesWebRequest
        .setCopyToAllVariantImages(Arrays.asList(productLevel3SummaryDetailsImageWebRequest));
    updateItemsPriceStockImagesWebRequest.setProductEditable(true);
    updateItemsPriceStockImagesWebRequest.setSynchronize(true);
    updateItemsPriceStockImagesWebRequest.setNeedCorrection(true);
    updateItemsPriceStockImagesWebRequest.setProductItems(Arrays.asList(productPriceStockAndImagesWebRequest));
    UpdateItemsPriceStockImagesRequest request =
        RequestHelper.toUpdateItemsPriceStockImagesRequest(updateItemsPriceStockImagesWebRequest);
    Assertions.assertTrue(request.isProductEditable());
    Assertions.assertTrue(request.isSynchronize());
    Assertions.assertEquals("new", request.getCopyToAllVariantImages().get(0).getReviewType());
    Assertions.assertTrue(request.getCopyToAllVariantImages().get(0).getMainImage());
    Assertions.assertFalse(request.getCopyToAllVariantImages().get(0).getMarkForDelete());
    Assertions.assertEquals(1, request.getProductItems().size());
    Assertions.assertEquals(ITEM_SKU, request.getProductItems().get(0).getItemSku());
    Assertions.assertEquals(PRICE, request.getProductItems().get(0).getPrices().get(0).getPrice(), 0);
    Assertions.assertEquals(LOCATION_PATH, request.getProductItems().get(0).getImages().get(0).getLocationPath());
    Assertions.assertTrue(request.getProductItems().get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(request.isNeedCorrection());
  }

  @Test
  public void toHistoryRequestTest() {
    HistorySummaryWebRequest historySummaryWebRequest =
        new HistorySummaryWebRequest(PRODUCT_SKU, VARIANT_NAME, KEYWORD, new Date(), new Date(), true);
    HistoryRequest variantHistoryRequest = RequestHelper.toHistoryRequest(historySummaryWebRequest);
    Assertions.assertEquals(PRODUCT_SKU, variantHistoryRequest.getProductSku());
    Assertions.assertEquals(KEYWORD, variantHistoryRequest.getKeyword());
    Assertions.assertEquals(VARIANT_NAME, variantHistoryRequest.getSearchField());
    assertNotNull(variantHistoryRequest.getStartDate());
    assertNotNull(variantHistoryRequest.getEndDate());
    Assertions.assertTrue(variantHistoryRequest.isBeforeThreeMonths());
  }

  @Test
  public void toProductLevel3UpdateRequestTest() {
    ProductLevel3UpdateRequest productLevel3UpdateRequest =
        RequestHelper.toProductLevel3UpdateRequest(productLevel3UpdateWebRequest);
    assertEquals(PRODUCT_SKU, productLevel3UpdateRequest.getProductSku());
    assertTrue(productLevel3UpdateRequest.isProductEditable());
    assertTrue(productLevel3UpdateRequest.isSynchronize());
    assertEquals(1, productLevel3UpdateRequest.getProductLevel3LogisticsRequest().size());
    assertEquals(LOGISTICS_PRODUCT_CODE,
        productLevel3UpdateRequest.getProductLevel3LogisticsRequest().get(0).getLogisticProductCode());
    assertTrue(productLevel3UpdateRequest.getProductLevel3LogisticsRequest().get(0).isSelected());
  }

  @Test
  public void toProductLevel3UpdateRequestEmptyLogisticsRequestTest() {
    productLevel3UpdateWebRequest.setLogistics(null);
    ProductLevel3UpdateRequest productLevel3UpdateRequest =
        RequestHelper.toProductLevel3UpdateRequest(productLevel3UpdateWebRequest);
    assertEquals(PRODUCT_SKU, productLevel3UpdateRequest.getProductSku());
    assertTrue(productLevel3UpdateRequest.isProductEditable());
    assertTrue(productLevel3UpdateRequest.isSynchronize());
  }

  @Test
  public void toProductLevel3UpdateRequestWithPreOrderTest() {
    productLevel3UpdateWebRequest.setPreOrder(preOrderWebRequest);
    ProductLevel3UpdateRequest productLevel3UpdateRequest =
        RequestHelper.toProductLevel3UpdateRequest(productLevel3UpdateWebRequest);
    assertEquals(PRODUCT_SKU, productLevel3UpdateRequest.getProductSku());
    assertTrue(productLevel3UpdateRequest.isProductEditable());
    assertTrue(productLevel3UpdateRequest.isSynchronize());
    assertEquals(1, productLevel3UpdateRequest.getProductLevel3LogisticsRequest().size());
    assertEquals(LOGISTICS_PRODUCT_CODE,
        productLevel3UpdateRequest.getProductLevel3LogisticsRequest().get(0).getLogisticProductCode());
    assertEquals(PREORDER_TYPE, productLevel3UpdateRequest.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, productLevel3UpdateRequest.getPreOrder().getPreOrderValue());
  }

  @Test
  public void toPickupPointUpdateRequestTest() {
    pickupPointUpdateWebRequest.setNeedCorrection(true);
    PickupPointUpdateRequest pickupPointUpdateRequest =
        RequestHelper.toPickupPointUpdateRequest(pickupPointUpdateWebRequest, BUSINESS_PARTNER_CODE);
    assertEquals(PRODUCT_SKU, pickupPointUpdateRequest.getProductSku());
    assertEquals(BUSINESS_PARTNER_CODE, pickupPointUpdateRequest.getBusinessPartnerCode());
    assertTrue(pickupPointUpdateRequest.isDifferentLocation());
    assertTrue(pickupPointUpdateRequest.isMarkDefaultAddress());
    assertEquals(1, pickupPointUpdateRequest.getItemsPickupPoint().size());
    assertEquals(ITEM_SKU, pickupPointUpdateRequest.getItemsPickupPoint().get(0).getItemSku());
    assertEquals(ITEM_NAME, pickupPointUpdateRequest.getItemsPickupPoint().get(0).getItemName());
    assertEquals(PICKUP_POINT_CODE, pickupPointUpdateRequest.getItemsPickupPoint().get(0).getPickupPointCode());
    assertTrue(pickupPointUpdateRequest.isNeedCorrection());
  }

  @Test
  public void toPickupPointUpdateRequestTestEmptyItems() {
    pickupPointUpdateWebRequest.setItemsPickupPoint(null);
    PickupPointUpdateRequest pickupPointUpdateRequest =
        RequestHelper.toPickupPointUpdateRequest(pickupPointUpdateWebRequest, BUSINESS_PARTNER_CODE);
    assertEquals(PRODUCT_SKU, pickupPointUpdateRequest.getProductSku());
    assertEquals(BUSINESS_PARTNER_CODE, pickupPointUpdateRequest.getBusinessPartnerCode());
    assertTrue(pickupPointUpdateRequest.isDifferentLocation());
    assertTrue(CollectionUtils.isEmpty(pickupPointUpdateRequest.getItemsPickupPoint()));
  }

  @Test
  public void toProductLevel3SummaryDetailsRequestTest() {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setProductSku(PRODUCT_SKU);
    productLevel3VariantsWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ProductLevel3SummaryDetailsRequest productLevel3SummaryDetailsRequest =
        RequestHelper.toProductLevel3SummaryDetailsRequest(productLevel3VariantsWebRequest);
    Assertions.assertEquals(PRODUCT_SKU, productLevel3SummaryDetailsRequest.getProductSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productLevel3SummaryDetailsRequest.getBusinessPartnerCode());
  }

  @Test
  public void checkPrivilegeEditO2O() {
    Assertions.assertTrue(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void checkPrivilegeEditO2O1() {
    this.privilegeMap.put(PRIVILEGED_EDIT_O2O, false);
    Assertions.assertFalse(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void checkPrivilegeEditO2O2() {
    this.profileResponse.getCompany().setOfflineToOnlineFlag(false);
    Assertions.assertFalse(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void checkPrivilegeEditO2O3() {
    this.privilegeMap.put(PRIVILEGED_EDIT_O2O, false);
    this.profileResponse.getCompany().setOfflineToOnlineFlag(false);
    Assertions.assertFalse(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void checkPrivilegeEditO2OWithprivilegeEditO2ONull() {
    this.privilegeMap.put(PRIVILEGED_EDIT_O2O, null);
    Assertions.assertFalse(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void checkPrivilegeEditO2OWithprivilegeEditO2ONullOmlineFlag() {
    this.privilegeMap.put(PRIVILEGED_EDIT_O2O, null);
    this.profileResponse.getCompany().setOfflineToOnlineFlag(false);
    Assertions.assertFalse(RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap));
  }

  @Test
  public void toQuickEditRequestListTest() {
    List<QuickEditRequest> quickEditRequests = RequestHelper.toQuickEditRequestList(quickEditWebRequests);
    Assertions.assertEquals(ITEM_SKU, quickEditRequests.get(0).getItemSku());
    Assertions.assertEquals(10000.0, quickEditRequests.get(0).getPrice().getPrice().doubleValue(), 0);
    Assertions.assertEquals(10000.0, quickEditRequests.get(0).getPrice().getSalePrice().doubleValue(), 0);
    Assertions.assertEquals(ProductLevel3Status.ONLINE, quickEditRequests.get(0).getStatus());
    Assertions.assertEquals(ProductLevel3Status.OFFLINE, quickEditRequests.get(1).getStatus());
    Assertions.assertEquals(ProductLevel3Status.TEASER, quickEditRequests.get(2).getStatus());
    Assertions.assertEquals(ProductLevel3Status.B2B, quickEditRequests.get(3).getStatus());
  }


  @Test
  public void toProductSummaryRequestTest() {
    ProductSummaryWebRequest productSummaryWebRequest =
        ProductSummaryWebRequest.builder().archived(true).categoryCodes(Arrays.asList(CATEGORY_CODE, CATEGORY_NAME))
            .inStock(true).keyword(PRODUCT_NAME).merchantCode(BUSINESS_PARTNER_CODE)
            .pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE)).promoTypes(Arrays.asList(PROMOTION_NAME))
            .sortField(SORT_BY).sortOrder(SORT_BY).build();
    ProductSummaryRequest productSummaryRequest =
        RequestHelper.toProductSummaryRequest(productSummaryWebRequest);
    Assertions.assertEquals(PRODUCT_NAME, productSummaryRequest.getKeyword());
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeDays() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
  }

  @Test
  public void validateWarnaAndFamilyColorAttributeTest() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
  }

  @Test
  public void validateWarnaAndFamilyColorAttributeFalseTest() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(ATTRIBUTE_NAME);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
            PREORDER_MAXIMUM_WEEK, false, true));
  }

  @Test
  public void validateWarnaAndFamilyColorAttributeFamilyColorFalseTest() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(ATTRIBUTE_NAME);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(ATTRIBUTE_NAME);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    try {
      RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, true);
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
  }

  @Test
  public void validateWarnaAndFamilyColorAttributeVariantCreationTest() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(false);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
            PREORDER_MAXIMUM_WEEK, false, true));
  }

  @Test
  public void validateWarnaAndFamilyColorAttributeVariantCreationFalseTest() throws Exception {
    productCreationRequest.setPreOrder(preOrderRequest);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(false);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeDaysAndValueZero() throws Exception {
    preOrderRequest.setPreOrderValue(0);
    productCreationRequest.setPreOrder(preOrderRequest);
    Assertions.assertThrows(ApplicationException.class,
        () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
            PREORDER_MAXIMUM_WEEK, false, false));
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeDaysAndValueMoreThanLimit() throws Exception {
    preOrderRequest.setPreOrderValue(91);
    productCreationRequest.setPreOrder(preOrderRequest);
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeWeek() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(PREORDER_MAXIMUM_WEEK);
    productCreationRequest.setPreOrder(preOrderRequest);
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeWeekAndValueZero() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(0);
    productCreationRequest.setPreOrder(preOrderRequest);
    String errorMessage = StringUtils.EMPTY;
    int statusCode = 0;
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
  }

  @Test
  void validateRequestForProductCreationRequestWithPreOrderTypeWeekAndValueMoreThanLimit() {
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(14);
    productCreationRequest.setPreOrder(preOrderRequest);
    int statusCode = 400;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
    } finally {
      Assertions.assertEquals(PREORDER_FAILURE_STATUS_CODE, statusCode);
    }
  }

  @Test
  public void validateRequestForProductCreationRequestWithPreOrderTypeDate() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 5);
    preOrderRequest.setPreOrderDate(cal.getTime());
    productCreationRequest.setPreOrder(preOrderRequest);
    RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS, PREORDER_MAXIMUM_WEEK, false, false);
  }

  @Test
  void validateRequestForProductCreationRequestWithPreOrderTypeDateEqualtoCurrentDate() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    preOrderRequest.setPreOrderDate(cal.getTime());
    productCreationRequest.setPreOrder(preOrderRequest);
    String errorMessage = StringUtils.EMPTY;
    int statusCode = 400;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
    } finally {
      Assertions.assertEquals(PREORDER_FAILURE_STATUS_CODE, statusCode);
    }
  }

  @Test
  void validateRequestForProductCreationRequestWithPreOrderTypeDateLessThanCurrentDate() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -1);
    preOrderRequest.setPreOrderDate(cal.getTime());
    productCreationRequest.setPreOrder(preOrderRequest);
    String errorMessage = StringUtils.EMPTY;
    int statusCode = 400;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
    } finally {
      Assertions.assertEquals(PREORDER_FAILURE_STATUS_CODE, statusCode);
    }
  }

  @Test
  void validateRequestForProductCreationRequestWithPreOrderTypeDateHasDaysMoreThanLimit() throws Exception {
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 91);
    preOrderRequest.setPreOrderDate(cal.getTime());
    productCreationRequest.setPreOrder(preOrderRequest);
    String errorMessage = StringUtils.EMPTY;
    int statusCode = 400;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
    } finally {
      Assertions.assertEquals(PREORDER_FAILURE_STATUS_CODE, statusCode);
    }
  }

  @Test
  void validateRequestForProductCreationRequestWithWrongPreOrderType() throws Exception {
    preOrderRequest.setPreOrderType(WRONG_PREORDER_TYPE);
    productCreationRequest.setPreOrder(preOrderRequest);
    String errorMessage = StringUtils.EMPTY;
    int statusCode = 400;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> RequestHelper.validateRequest(productCreationRequest, PREORDER_MAXIMUM_DAYS,
              PREORDER_MAXIMUM_WEEK, false, false));
    } finally {
      Assertions.assertEquals(PREORDER_FAILURE_STATUS_CODE, statusCode);
    }
  }

  @Test
  public void toNeedRevisionSubmitRequestTest() throws Exception {
    NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest =
        new NeedRevisionSubmitWebRequest(LOGISTICS_PRODUCT_CODE, PRODUCT_SKU);
    NeedRevisionSubmitRequest needRevisionSubmitRequest = RequestHelper.toNeedRevisionSubmitRequest(needRevisionSubmitWebRequest);
    Assertions.assertEquals(LOGISTICS_PRODUCT_CODE, needRevisionSubmitRequest.getProductCode());
    Assertions.assertEquals(PRODUCT_SKU, needRevisionSubmitRequest.getProductSku());
  }

  @Test
  public void toAppealProductRequestTest() {
    AppealProductWebRequest appealProductWebRequest = AppealProductWebRequest.builder()
        .productSku(PRODUCT_SKU)
        .productCode(PRODUCT_CODE).build();
    AppealProductRequest appealProductRequest =
        RequestHelper.toAppealProductRequest(appealProductWebRequest, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, appealProductRequest.getBusinessPartnerCode());
  }

  @Test
  public void toProductImageEditRequestTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImagePath(LOCATION_PATH);
    ItemImageEditWebRequest itemImageEditWebRequest = new ItemImageEditWebRequest();
    itemImageEditWebRequest.setCopy(true);
    productImageEditWebRequest.setProductItems(Arrays.asList(itemImageEditWebRequest));
    ProductImageEditRequest productImageEditRequest =
        RequestHelper.toProductImageEditRequest(productImageEditWebRequest, PRODUCT_SKU);
    Assertions.assertEquals(LOCATION_PATH, productImageEditRequest.getImagePath());
    Assertions.assertEquals(PRODUCT_SKU, productImageEditRequest.getProductSku());
    Assertions.assertEquals(1, productImageEditRequest.getProductItems().size());
    Assertions.assertTrue(productImageEditRequest.getProductItems().get(0).isCopy());
    assertNull(productImageEditRequest.getCopyToAllVariantImages());
  }

  @Test
  public void toProductImageEditRequestForAddTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(LOCATION_PATH);
    ItemImageEditWebRequest itemImageEditWebRequest = new ItemImageEditWebRequest();
    itemImageEditWebRequest.setCopy(true);
    productImageEditWebRequest.setProductItems(Arrays.asList(itemImageEditWebRequest));
    ProductImageEditRequest productImageEditRequest =
        RequestHelper.toProductImageEditRequest(productImageEditWebRequest, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_SKU, productImageEditRequest.getProductSku());
    Assertions.assertEquals(1, productImageEditRequest.getProductItems().size());
    Assertions.assertFalse(productImageEditRequest.getProductItems().get(0).isCopy());
    Assertions.assertTrue(productImageEditRequest.getProductItems().get(0).isAdd());
    assertNull(productImageEditRequest.getCopyToAllVariantImages());
  }

  @Test
  public void toProductImageEditRequestCopyTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImagePath(LOCATION_PATH);
    CopyImageEditWebRequest copyImageEditWebRequest = new CopyImageEditWebRequest();
    copyImageEditWebRequest.setCopy(true);
    productImageEditWebRequest.setCopyToAllVariantImages(copyImageEditWebRequest);
    ProductImageEditRequest productImageEditRequest =
        RequestHelper.toProductImageEditRequest(productImageEditWebRequest, PRODUCT_SKU);
    Assertions.assertEquals(LOCATION_PATH, productImageEditRequest.getImagePath());
    Assertions.assertEquals(PRODUCT_SKU, productImageEditRequest.getProductSku());
    Assertions.assertEquals(0, productImageEditRequest.getProductItems().size());
    Assertions.assertTrue(productImageEditRequest.getCopyToAllVariantImages().isCopy());
    Assertions.assertFalse(productImageEditRequest.getCopyToAllVariantImages().isAdd());
  }

  @Test
  public void toProductImageEditRequestCopyAddTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(LOCATION_PATH);
    CopyImageEditWebRequest copyImageEditWebRequest = new CopyImageEditWebRequest();
    copyImageEditWebRequest.setCopy(true);
    productImageEditWebRequest.setCopyToAllVariantImages(copyImageEditWebRequest);
    ProductImageEditRequest productImageEditRequest =
        RequestHelper.toProductImageEditRequest(productImageEditWebRequest, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_SKU, productImageEditRequest.getProductSku());
    Assertions.assertEquals(0, productImageEditRequest.getProductItems().size());
    Assertions.assertFalse(productImageEditRequest.getCopyToAllVariantImages().isCopy());
    Assertions.assertTrue(productImageEditRequest.getCopyToAllVariantImages().isAdd());
  }

  @Test
  public void toPickupPointFilterRequestTest() {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointSummaryWebRequest.setCncActivated(true);
    pickupPointSummaryWebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    PickupPointFilterRequest pickupPointFilterRequest =
        RequestHelper.toPickupPointFilterRequest(pickupPointSummaryWebRequest, new HashSet<>(), false);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, pickupPointFilterRequest.getBusinessPartnerCode());
    Assertions.assertTrue(pickupPointFilterRequest.getCncActivated());
    Assertions.assertEquals(1, pickupPointFilterRequest.getCodes().size());
    Assertions.assertEquals(PICKUP_POINT_CODE, pickupPointFilterRequest.getCodes().stream().findFirst().get());
  }

  @Test
  public void toPickupPointSummaryRequestTest() {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointSummaryRequest pickupPointSummaryRequest =
        RequestHelper.toPickupPointSummaryRequest(pickupPointSummaryWebRequest, new HashSet<>());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, pickupPointSummaryRequest.getBusinessPartnerCode());
  }

  @Test
  public void setWaitingDeletionFlagForDeletePickupPointTest() {
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setWaitingDeletion(true);
    RequestHelper.setWaitingDeletionFlagForDeletePickupPoint(false, pickupPointFilterRequest);
    RequestHelper.setWaitingDeletionFlagForDeletePickupPoint(true, pickupPointFilterRequest);
    Assertions.assertFalse(pickupPointFilterRequest.getWaitingDeletion());
  }

  @Test
  public void toProductSummaryRequestFromV2Test() {
    productSummaryV2WebRequest.setBundleProduct(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductSummaryRequest productSummaryRequest =
      RequestHelper.toProductSummaryRequest(productSummaryV2WebRequest);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productSummaryRequest.getMerchantCode());
    Assertions.assertEquals(KEYWORD, productSummaryRequest.getKeyword());
    Assertions.assertTrue(productSummaryRequest.getSuspended());
    Assertions.assertTrue(productSummaryRequest.getInStock());
    Assertions.assertTrue(productSummaryRequest.getB2bActivated());
    Assertions.assertTrue(productSummaryRequest.getB2cActivated());
    Assertions.assertTrue(productSummaryRequest.getBundleProduct());
    Assertions.assertEquals(ATTRIBUTE_CODE, productSummaryRequest.getSizeAttributeCode());
  }

  @Test
  public void toL5IdTest() {
    String l5Id = RequestHelper.toL5Id(ITEM_SKU, PICKUP_POINT_CODE);
    Assertions.assertEquals(ITEM_SKU + Constants.DASH_SEPARATOR + PICKUP_POINT_CODE, l5Id);
  }

  @Test
  public void toProductDetailPageTest() {
    String url = RequestHelper.toProductDetailPage(PRODUCT_SKU, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_DETAIL_URL, url);
  }

  @Test
  public void toItemDTOLIstFromItemPickupPointRequestTest() {
    List<ItemInfoDto> itemInfoDTOs = RequestHelper.toItemDTOLIstFromItemPickupPointRequest(
      Collections.singletonList(itemPickupPointWebRequest));
    Assertions.assertEquals(ITEM_SKU, itemInfoDTOs.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemInfoDTOs.get(0).getPickupPointCode());
  }

  @Test
  public void toItemPickupPointListingL3RequestTest() {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest =
        ItemPickupPointListingL3WebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).itemSku(ITEM_SKU).keyword(ITEM_NAME).pickupPointCodes(ImmutableSet.of(PICKUP_POINT_CODE)).build();
    ItemPickupPointListingL3Request itemPickupPointListingL3Request =
        RequestHelper.toItemPickupPointListingL3Request(PRODUCT_SKU, itemPickupPointListingL3WebRequest);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, itemPickupPointListingL3Request.getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingL3Request.getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingL3Request.getItemSku());
    Assertions.assertEquals(ITEM_NAME, itemPickupPointListingL3Request.getKeyword());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListingL3Request.getPickupPointCodes().iterator().next());
  }

  @Test
  public void toItemLevel4WebRequestTest() {
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest();
    Set<String> productSkus = new HashSet<>();
    productSkus.add(DEFAULT_PRODUCT_SKU);
    webRequest.setProductSkus(productSkus);
    ItemLevel4ListingWebRequest itemLevel4ListingWebRequest = new ItemLevel4ListingWebRequest();
    itemLevel4ListingWebRequest = RequestHelper.toItemLevel4WebRequest(webRequest);
    Assertions.assertEquals(webRequest.getProductSkus().size(),
      itemLevel4ListingWebRequest.getProductSkus().size());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(webRequest.getProductSkus()));
  }

  @Test
  public void toItemLevel4WebRequestTest_nullRequest() {
    ItemLevel4WebRequest webRequest = null;
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    Exception exception = new Exception();
    try {
      ItemLevel4ListingWebRequest itemLevel4ListingWebRequest =
        RequestHelper.toItemLevel4WebRequest(webRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ApplicationException.class);
    }
  }

  @Test
  public void toItemLevel4WebRequestTest_EmptyRequest() {
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest();
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    webRequest.setProductSkus(Collections.EMPTY_SET);
    Exception exception = new Exception();
    ItemLevel4ListingWebRequest itemLevel4ListingWebRequest = new ItemLevel4ListingWebRequest();
    try {
      itemLevel4ListingWebRequest = RequestHelper.toItemLevel4WebRequest(webRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertTrue(CollectionUtils.isEmpty(webRequest.getProductSkus()));
      Assertions.assertTrue(CollectionUtils.isEmpty(itemLevel4ListingWebRequest.getProductSkus()));
    }
  }

  @Test
  public void toBulkProcessDeleteOfflineItemRequestTest() throws Exception {
    BulkProcessDeleteOfflineItemRequest request;
    request = RequestHelper
      .toBulkProcessDeleteOfflineItemRequest(BUSINESS_PARTNER_CODE, USERNAME, CHANNEL_ID,
        generateDummyExcelMultipartFile(), new HashSet<>());
    Assertions.assertEquals(request.getBusinessPartnerCode(), BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(StringUtils.isNotBlank(request.getFileName()));
  }

  @Test
  public void toProductL3RequestFromProductEditInfoWebRequestTest() {
    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);
    ProductEditInfoV2WebRequest editInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    editInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    editInfoV2WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    editInfoV2WebRequest.setCommonImages(Arrays.asList(productLevel3CommonImageWebRequest));
    editInfoV2WebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));
    editInfoV2WebRequest.setPreOrder(preOrderWebRequest);
    editInfoV2WebRequest.setDeletedProductItems(Collections.singletonList(new DeletedProductItemsWebRequest()));
    editInfoV2WebRequest.setProductBundleRecipe(Arrays.asList(ProductBundleRecipeWebRequest.builder()
        .bundleRecipe(Set.of(ProductBundleWebRecipe.builder().itemSku(ITEM_SKU).build())).build()));
    ProductItemUomInfoWebRequest productItemUomInfoWebRequest = new ProductItemUomInfoWebRequest();
    editInfoV2WebRequest.setDistributionAndUOMRequest(Collections.singletonList(productItemUomInfoWebRequest));
    ProductL3UpdateRequest productL3UpdateRequest =
      RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(editInfoV2WebRequest, true);
    Assertions.assertEquals(productL3UpdateRequest.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productL3UpdateRequest.isSizeChartChanged());
  }

  @Test
  public void toProductL3RequestFromProductEditInfoWebRequestRanchTrueTest() {
    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);
    ProductEditInfoV2WebRequest editInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    editInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    editInfoV2WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    editInfoV2WebRequest.setCommonImages(Arrays.asList(productLevel3CommonImageWebRequest));
    editInfoV2WebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));
    editInfoV2WebRequest.setPreOrder(preOrderWebRequest);
    editInfoV2WebRequest.setDeletedProductItems(Collections.singletonList(new DeletedProductItemsWebRequest()));
    editInfoV2WebRequest.setProductBundleRecipe(Arrays.asList(ProductBundleRecipeWebRequest.builder()
        .bundleRecipe(Set.of(ProductBundleWebRecipe.builder().itemSku(ITEM_SKU).build())).build()));
    ProductL3UpdateRequest productL3UpdateRequest =
        RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(editInfoV2WebRequest, true);
    Assertions.assertEquals(productL3UpdateRequest.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productL3UpdateRequest.isSizeChartChanged());
  }

  @Test
  public void toProductL3RequestFromProductEditInfoWebRequest_withPreOrderTest() {
    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);
    ProductEditInfoV2WebRequest editInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    editInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    editInfoV2WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    editInfoV2WebRequest.setCommonImages(Arrays.asList(productLevel3CommonImageWebRequest));
    editInfoV2WebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));
    editInfoV2WebRequest.setPreOrder(preOrderWebRequest);
    editInfoV2WebRequest.setDeletedProductItems(Collections.singletonList(new DeletedProductItemsWebRequest()));
    editInfoV2WebRequest.setProductBundleRecipe(Arrays.asList(ProductBundleRecipeWebRequest.builder()
      .bundleRecipe(Set.of(ProductBundleWebRecipe.builder().itemSku(ITEM_SKU).build())).build()));
    ItemPickupPointWebRequest itemPickupPointWebRequest = new ItemPickupPointWebRequest();
    itemPickupPointWebRequest.setInitialPreOrderQuota(10);
    editInfoV2WebRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointWebRequest));
    ProductL3UpdateRequest productL3UpdateRequest =
      RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(editInfoV2WebRequest, false);
    Assertions.assertEquals(productL3UpdateRequest.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productL3UpdateRequest.isSizeChartChanged());
    Assertions.assertEquals(10,
      productL3UpdateRequest.getAddPickupPoints().getFirst().getInitialPreOrderQuota());
  }

  @Test
  public void toProductL3RequestFromProductEditInfoWebRequestTest_videoUpdated() {
    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);
    ProductEditInfoV2WebRequest editInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    editInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    editInfoV2WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    editInfoV2WebRequest.setCommonImages(Arrays.asList(productLevel3CommonImageWebRequest));
    editInfoV2WebRequest.setLogistics(Arrays.asList(productLevel3LogisticsWebRequest));
    editInfoV2WebRequest.setPreOrder(preOrderWebRequest);
    editInfoV2WebRequest.setDeletedProductItems(Collections.singletonList(new DeletedProductItemsWebRequest()));
    editInfoV2WebRequest.setProductBundleRecipe(Arrays.asList(ProductBundleRecipeWebRequest.builder()
        .bundleRecipe(Set.of(ProductBundleWebRecipe.builder().itemSku(ITEM_SKU).build())).build()));
    editInfoV2WebRequest.setVideoUpdated(true);
    editInfoV2WebRequest.setVideoAddEditRequest(
        VideoAddEditRequest.builder().videoName(VALUE).build());
    ProductL3UpdateRequest productL3UpdateRequest =
        RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(editInfoV2WebRequest, false);
    Assertions.assertEquals(productL3UpdateRequest.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productL3UpdateRequest.isSizeChartChanged());
    Assertions.assertTrue(productL3UpdateRequest.getVideoUpdated());
    Assertions.assertEquals(VALUE, productL3UpdateRequest.getVideoAddEditRequest().getVideoName());
  }

  @Test
  public void toProductL3RequestFromProductEditInfoWebRequest_NullTest() {
    ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest = new ProductLevel3LogisticsWebRequest();
    productLevel3LogisticsWebRequest.setLogisticProductCode(LOGISTICS_PRODUCT_CODE);
    ProductEditInfoV2WebRequest editInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    editInfoV2WebRequest.setSizeChartChanged(true);
    editInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    editInfoV2WebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    editInfoV2WebRequest.setCommonImages(Arrays.asList(productLevel3CommonImageWebRequest));
    editInfoV2WebRequest.setLogistics(new ArrayList<>());
    editInfoV2WebRequest.setPreOrder(null);
    editInfoV2WebRequest.setDeletedProductItems(null);
    ProductL3UpdateRequest productL3UpdateRequest =
      RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(editInfoV2WebRequest, false);
    Assertions.assertEquals(productL3UpdateRequest.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(productL3UpdateRequest.isSizeChartChanged());
  }

  @Test
  public void validateRequestForProductL3UpdateRequest() {
    RequestHelper.validateRequest(productL3UpdateRequest);
    assertEquals(VALID_TEXT, productL3UpdateRequest.getDescription());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getUniqueSellingPoint());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getSpecificationDetail());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getProductName());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getProductSku());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getProductStory());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getProductCode());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getUrl());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getAccessChannel());
    assertEquals(BRAND, productL3UpdateRequest.getBrand());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getAttributes().get(0).getAttributeName());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getAttributes().get(0).getAttributeCode());
    assertEquals(VALID_TEXT, productL3UpdateRequest.getAttributes().get(0).getAttributeType());
    assertEquals(LOCATION_PATH, productL3UpdateRequest.getCommonImages().get(0).getLocationPath());
  }

  @Test
  public void validateRequestForProductL3UpdateRequest_EmptyAttributesTest() {
    productL3UpdateRequest.setAttributes(null);
    RequestHelper.validateRequest(productL3UpdateRequest);
    assertNull(productL3UpdateRequest.getAttributes());
  }

  @Test
  public void validateRequestForProductL3UpdateRequest_EmptyCommonImagesTest() {
    productL3UpdateRequest.setCommonImages(null);
    RequestHelper.validateRequest(productL3UpdateRequest);
    assertNull(productL3UpdateRequest.getCommonImages());
  }

  @Test
  public void toMarkPickupPointAsDefaultRequestTest() throws Exception {
    MarkPickupPointAsDefaultRequest markPickupPointAsDefaultRequest =
        RequestHelper.toMarkPickupPointAsDefaultRequest(MERCHANT_CODE, defaultConfigurationAndPickupPointRequest);
    Assertions.assertEquals(markPickupPointAsDefaultRequest.getStoreCode(), MERCHANT_CODE);
  }

  @Test
  public void toProfileRequestTest() throws Exception {
    ProductSettingsRequest productSettings = new ProductSettingsRequest();
    productSettings.setCnc(true);
    productSettings.setFbb(false);
    defaultConfigurationAndPickupPointRequest.setProductSettings(productSettings);
    ProfileRequest profileRequest =
        RequestHelper.toProfileRequest(MERCHANT_CODE, defaultConfigurationAndPickupPointRequest);
    Assertions.assertEquals(profileRequest.getBusinessPartnerCode(), MERCHANT_CODE);
    Assertions.assertTrue(profileRequest.getProductSettings().getCnc());
    Assertions.assertFalse(profileRequest.getProductSettings().getFbb());
  }

  @Test
  public void toQuickEditWebRequestListTest() {
    List<QuickEditWebRequest> quickEditWebRequestList =
      RequestHelper.toQuickEditWebRequestList(Collections.singletonList(quickEditV2WebRequest));
    Assertions.assertEquals(ITEM_SKU, quickEditWebRequestList.get(0).getItemSku());
    Assertions.assertTrue(quickEditWebRequestList.get(0).getSynchronizeStock());
    Assertions.assertTrue(quickEditWebRequestList.get(0).getOff2OnActiveFlag());
    Assertions.assertTrue(quickEditWebRequestList.get(0).getWholesalePriceActivated());
    Assertions.assertEquals(STATUS, quickEditWebRequestList.get(0).getStatus());
    Assertions.assertEquals(2, quickEditWebRequestList.get(0).getDeltaStock(), 0);
    Assertions.assertEquals(10L, quickEditWebRequestList.get(0).getVersion(), 0);
    Assertions.assertEquals(productLevel3PriceWebRequest,
      quickEditWebRequestList.get(0).getPrices().get(0));
  }

  @Test
  public void toProductSummaryWebRequest() {
    ProductSummaryWebRequest productSummaryWebRequest =
      RequestHelper.toProductSummaryWebRequest(productSummaryV2WebRequest);
    Assertions.assertTrue(productSummaryWebRequest.getSuspended());
    Assertions.assertTrue(productSummaryWebRequest.getInStock());
    Assertions.assertTrue(productSummaryWebRequest.getArchived());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productSummaryWebRequest.getMerchantCode());
    Assertions.assertEquals(Collections.singletonList(CATEGORY_CODE),
      productSummaryWebRequest.getCategoryCodes());
    Assertions.assertEquals(Collections.singletonList(PICKUP_POINT_CODE),
      productSummaryWebRequest.getPickupPointCodes());
  }

  @Test
  public void profileResponseAndRequestValidationTest() {
    profileResponse.setMerchantStatus("ACTIVE");
    RequestHelper.profileResponseAndRequestValidation(profileResponse, productL3UpdateRequest);
  }

  @Test
  public void profileResponseAndRequestValidationNullTest() {
    profileResponse.setMerchantStatus("ACTIVE");
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.profileResponseAndRequestValidation(null, productL3UpdateRequest));
  }

  @Test
  public void profileResponseAndRequestValidation_EmptyBPNumberTest() {
    profileResponse.setMerchantStatus("INACTIVE");
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.profileResponseAndRequestValidation(profileResponse,
            productL3UpdateRequest));
  }

  @Test
  public void toProductVariantPriceStockAndImagesRequestTest() {
    ItemPickupPointWebRequest itemPickupPointWebRequest1 = new ItemPickupPointWebRequest();
    itemPickupPointWebRequest1.setB2bFields(new B2BFields());
    variantPriceStockAndImagesWebRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointWebRequest1));
    List<ProductVariantPriceStockAndImagesRequest> result = RequestHelper
      .toProductVariantPriceStockAndImagesRequest(
        Arrays.asList(variantPriceStockAndImagesWebRequest));
    Assertions.assertEquals(1, result.size());
  }


  @Test
  public void toProductVariantPriceStockAndImagesRequestForBuyableScheduleTest() {
    ItemPickupPointWebRequest itemPickupPointWebRequest1 = new ItemPickupPointWebRequest();
    itemPickupPointWebRequest1.setB2bFields(new B2BFields());
    itemPickupPointWebRequest1.setBuyableSchedule(BuyableScheduleWebRequest.builder().build());
    variantPriceStockAndImagesWebRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointWebRequest1));
    List<ProductVariantPriceStockAndImagesRequest> result = RequestHelper
      .toProductVariantPriceStockAndImagesRequest(
        Arrays.asList(variantPriceStockAndImagesWebRequest));
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void toProductVariantPriceStockAndImagesRequestForDiscoverableScheduleTest() {
    ItemPickupPointWebRequest itemPickupPointWebRequest1 = new ItemPickupPointWebRequest();
    itemPickupPointWebRequest1.setB2bFields(new B2BFields());
    itemPickupPointWebRequest1.setDiscoverableSchedule(DiscoverableScheduleWebRequest.builder().build());
    variantPriceStockAndImagesWebRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointWebRequest1));
    List<ProductVariantPriceStockAndImagesRequest> result = RequestHelper
      .toProductVariantPriceStockAndImagesRequest(
        Arrays.asList(variantPriceStockAndImagesWebRequest));
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void toProductVariantPriceStockAndImagesRequestEmptyTest() {
    List<ProductVariantPriceStockAndImagesRequest> result =
      RequestHelper.toProductVariantPriceStockAndImagesRequest(new ArrayList<>());
    Assertions.assertEquals(0, result.size());
  }

  @Test
  public void toProductLevel3RequestFromProductEditInfoWebRequestTest() {
    ProductEditInfoWebRequest productEditInfoWebRequest = new ProductEditInfoWebRequest();
    productEditInfoWebRequest.setAttributes(Arrays.asList(productLevel3AttributeWebRequest));
    productEditInfoWebRequest.setImages(Arrays.asList(productLevel3ImageWebRequest));
    ProductLevel3Request result =
      RequestHelper.toProductLevel3RequestFromProductEditInfoWebRequest(productEditInfoWebRequest);
    assertNotNull(result);
  }

  @Test
  public void toProductVariantUpdateRequestTest() {
    ProductVariantUpdateRequest result = RequestHelper
      .toProductVariantUpdateRequest(productVariantUpdateWebRequest, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(result.getProductItems().size(), 1);
  }

  @Test
  public void toProductVariantUpdateRequest_EmptyWholeSalePriceRequesrTest() {
    productVariantUpdateWebRequest.getAddPickupPoints().get(0).setProductItemWholesalePriceRequests(new ArrayList<>());
    ProductVariantUpdateRequest result = RequestHelper
      .toProductVariantUpdateRequest(productVariantUpdateWebRequest, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(result.getProductItems().size(), 1);
  }

  @Test
  public void setTimeFilterForHistoryRequestTest() {
    historyUpdateRequest.setBeforeOneMonths(true);
    RequestHelper.setTimeFilterForHistoryRequest(historyUpdateRequest);
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getEndDate()));
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getStartDate()));
  }

  @Test
  public void setTimeFilterForHistoryRequest_beforeOneMonthsTest() {
    historyUpdateRequest.setBeforeOneMonths(false);
    RequestHelper.setTimeFilterForHistoryRequest(historyUpdateRequest);
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getEndDate()));
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getStartDate()));
  }

  @Test
  public void setTimeFilterForHistoryRequest_nonNullDatesTest() {
    historyUpdateRequest.setBeforeOneMonths(true);
    historyUpdateRequest.setEndDate(new Date());
    historyUpdateRequest.setStartDate(new Date());
    RequestHelper.setTimeFilterForHistoryRequest(historyUpdateRequest);
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getEndDate()));
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getStartDate()));
  }

  @Test
  public void setTimeFilterForHistoryRequest_beforeOneMonths_nonNullDatesTest() {
    historyUpdateRequest.setBeforeOneMonths(false);
    historyUpdateRequest.setEndDate(new Date());
    historyUpdateRequest.setStartDate(new Date());
    RequestHelper.setTimeFilterForHistoryRequest(historyUpdateRequest);
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getEndDate()));
    Assertions.assertTrue(Objects.nonNull(historyUpdateRequest.getStartDate()));
  }

  @Test
  public void toQuickEditV2RequestsTest() {
    List<QuickEditV2Request> response =
      RequestHelper.toQuickEditV2Requests(Collections.singletonList(quickEditV2WebRequest));
    Assertions.assertEquals(ITEM_SKU, response.get(0).getItemSku());
    Assertions.assertTrue(response.get(0).getUseWarehouseStock());
  }

  @Test
  public void toQuickEditV2Requests_withCncStatusTest() {
    quickEditV2WebRequest.setCncStatus(STATUS);
    List<QuickEditV2Request> response =
      RequestHelper.toQuickEditV2Requests(Collections.singletonList(quickEditV2WebRequest));
    Assertions.assertEquals(ITEM_SKU, response.get(0).getItemSku());
    Assertions.assertTrue(response.get(0).getUseWarehouseStock());
    Assertions.assertEquals(ProductLevel3Status.ONLINE, response.get(0).getCncStatus());
  }

  @Test
  public void toQuickEditV2RequestsTest1() {
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setStatus("ONLINE");
    quickEditV2WebRequest.setB2bFields(b2bFieldsRequest);
    List<QuickEditV2Request> response =
        RequestHelper.toQuickEditV2Requests(Collections.singletonList(quickEditV2WebRequest));
    Assertions.assertEquals(ITEM_SKU, response.get(0).getItemSku());
    Assertions.assertTrue(response.get(0).getUseWarehouseStock());
  }

  @Test
  public void toMarkPickupPointDefaultRequestTest() throws Exception {
    MarkPickupPointAsDefaultRequest markPickupPointAsDefaultRequest =
        RequestHelper.toMarkPickupPointDefaultRequest(MERCHANT_CODE, PICKUP_POINT_CODE);
    Assertions.assertEquals(MERCHANT_CODE, markPickupPointAsDefaultRequest.getStoreCode());
  }

  @Test
  public void toItemInfoDtoList() {
    List<WholeSaleDetailListWebRequest> wholeSaleDetailListWebRequests = new ArrayList<>();
    WholeSaleDetailListWebRequest wholeSaleDetailListWebRequest = new WholeSaleDetailListWebRequest();
    wholeSaleDetailListWebRequest.setItemSku(ITEM_SKU);
    wholeSaleDetailListWebRequest.setPickupPointCode(PICKUP_POINT_CODE);
    wholeSaleDetailListWebRequests.add(wholeSaleDetailListWebRequest);
    List<ItemInfoDto> itemInfoDtos = RequestHelper.toItemInfoDtoList(wholeSaleDetailListWebRequests);
    Assertions.assertEquals(itemInfoDtos.get(0).getItemSku(), ITEM_SKU);
    Assertions.assertEquals(itemInfoDtos.get(0).getPickupPointCode(), PICKUP_POINT_CODE);
  }

  @Test
  public void getWholesalePriceSkuDetailListRequestTest() {
    RequestHelper.getWholesalePriceSkuDetailListRequest(productPriceAndStockUpdateWebRequest);
  }
  @Test
  public void toPickupPointFilterRequestFalseNullTest() {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointSummaryWebRequest.setCncActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest =
      RequestHelper.toPickupPointFilterRequest(pickupPointSummaryWebRequest, new HashSet<>(), false);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, pickupPointFilterRequest.getBusinessPartnerCode());
    Assertions.assertTrue(pickupPointFilterRequest.getCncActivated());
    assertNull(pickupPointFilterRequest.getFbbActivated());
  }

  @Test
  public void toPickupPointFilterRequestFalseTrueTest() {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointSummaryWebRequest.setCncActivated(true);
    pickupPointSummaryWebRequest.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest =
      RequestHelper.toPickupPointFilterRequest(pickupPointSummaryWebRequest, new HashSet<>(), false);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, pickupPointFilterRequest.getBusinessPartnerCode());
    Assertions.assertTrue(pickupPointFilterRequest.getCncActivated());
    Assertions.assertTrue(pickupPointFilterRequest.getFbbActivated());
  }

  @Test
  public void toBulkProcessStatusListingWebResponseTest() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    BulkProcessStatusListingResponse response1 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response2 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
      .bulkProcessType(BULK_PROCESS_TYPE).build();
    content.add(response1);
    content.add(response2);
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    BulkProcessStatusListingWebResponse expectedWebResponse1 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
      .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse2 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    expectedWebResponseList.add(expectedWebResponse1);
    expectedWebResponseList.add(expectedWebResponse2);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
    Assertions.assertEquals(expectedWebResponseList.size(), result.size());
  }

  @Test
  public void toBulkProcessStatusListingWebResponseTest2() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    BulkProcessStatusListingWebResponse expectedWebResponse1 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse2 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    expectedWebResponseList.add(expectedWebResponse1);
    expectedWebResponseList.add(expectedWebResponse2);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
  }

  @Test
  public void toBulkProcessStatusListingWebResponseTestWithBulkWithStatus() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    BulkProcessStatusListingResponse response1 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkActivityStatus(BulkActivityStatus.IN_PROGRESS).bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response2 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2")).bulkActivityStatus(
          BulkActivityStatus.FAILED)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    content.add(response1);
    content.add(response2);
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    BulkProcessStatusListingWebResponse expectedWebResponse1 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse2 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    expectedWebResponseList.add(expectedWebResponse1);
    expectedWebResponseList.add(expectedWebResponse2);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
    Assertions.assertEquals(expectedWebResponseList.size(), result.size());
    Assertions.assertEquals(result.get(0).getStatus(), BulkActivityStatusWeb.IN_PROGRESS);
  }

  @Test
  public void toBulkProcessStatusListingWebResponseTestWithBulkWithStatus2() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    BulkProcessStatusListingResponse response1 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkActivityStatus(BulkActivityStatus.SUCCESS).bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response2 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2")).bulkActivityStatus(
          BulkActivityStatus.PARTIAL_SUCCESS).uploadedFileName("File-123")
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response3 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2")).bulkActivityStatus(
          BulkActivityStatus.PENDING)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    content.add(response1);
    content.add(response2);
    content.add(response3);
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    BulkProcessStatusListingWebResponse expectedWebResponse1 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse2 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse3 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-3"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    expectedWebResponseList.add(expectedWebResponse1);
    expectedWebResponseList.add(expectedWebResponse2);
    expectedWebResponseList.add(expectedWebResponse3);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
    Assertions.assertEquals(expectedWebResponseList.size(), result.size());
    Assertions.assertEquals(result.get(0).getStatus(), BulkActivityStatusWeb.SUCCESS);
    Assertions.assertEquals(result.get(1).getStatus(), BulkActivityStatusWeb.PARTIAL_SUCCESS);
    Assertions.assertEquals(result.get(2).getStatus(), BulkActivityStatusWeb.PENDING);
    Assertions.assertEquals(result.get(1).getUploadedFileName() , "File-123");
    Assertions.assertEquals(result.get(2).getUploadedFileName() , "");

  }

  @Test
  public void toBulkProcessStatusListingWebResponseTestWithBulkWithUploadedFileName() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    BulkProcessStatusListingResponse response1 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkActivityStatus(BulkActivityStatus.SUCCESS).bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response2 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2")).bulkActivityStatus(
          BulkActivityStatus.NA)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingResponse response3 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    content.add(response1);
    content.add(response2);
    content.add(response3);
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    BulkProcessStatusListingWebResponse expectedWebResponse1 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse2 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    BulkProcessStatusListingWebResponse expectedWebResponse3 =
      BulkProcessStatusListingWebResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-3"))
        .bulkProcessType(BULK_PROCESS_TYPE).build();
    expectedWebResponseList.add(expectedWebResponse1);
    expectedWebResponseList.add(expectedWebResponse2);
    expectedWebResponseList.add(expectedWebResponse3);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
    Assertions.assertEquals(expectedWebResponseList.size(), result.size());
    Assertions.assertEquals(result.get(0).getStatus(), BulkActivityStatusWeb.SUCCESS);
    Assertions.assertEquals(result.get(1).getStatus(), BulkActivityStatusWeb.NA);
    Assertions.assertEquals(result.get(2).getStatus(), BulkActivityStatusWeb.NA);
    Assertions.assertEquals(result.get(1).getUploadedFileName() , "");
    Assertions.assertEquals(result.get(2).getUploadedFileName() , "");

  }

  @Test
  public void toBulkProcessStatusListingWebResponseTestWithDescription() {
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    List<BulkProcessStatusListingResponse> content = new ArrayList<>();
    BulkProcessStatusListingResponse response1 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .bulkActivityStatus(BulkActivityStatus.SUCCESS).bulkProcessType(BULK_PROCESS_TYPE)
        .description(DESCRIPTION).build();
    BulkProcessStatusListingResponse response2 =
      BulkProcessStatusListingResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE.concat("-2"))
        .bulkActivityStatus(BulkActivityStatus.IN_PROGRESS).bulkProcessType(BULK_PROCESS_TYPE).build();
    content.add(response1);
    content.add(response2);
    listingResponse.setContent(content);
    List<BulkProcessStatusListingWebResponse> result = RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    assertNotNull(result);
    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals(DESCRIPTION, result.get(0).getDescription());
    Assertions.assertNull(result.get(1).getDescription());
  }

  @Test
  void validateManualQRCodeRequest_A5templateInvalid_ExceptionTest() {
    manualQRCodeRequest.setQrPerPage(16);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_A5template_Test() {
    manualQRCodeRequest.setQrPerPage(2);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  void validateManualQRCodeRequest_A5template_QrPerPageNull_Test() {
    manualQRCodeRequest.setQrPerPage(null);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_BY46templateInvalid_ExceptionTest() throws Exception {
    manualQRCodeRequest.setQrPerPage(16);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY46);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_BY46template_18QrPerPage_Test() throws Exception {
    manualQRCodeRequest.setQrPerPage(18);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY46);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  void validateManualQRCodeRequest_BY46template_QrPerPageNull_Test() throws Exception {
    manualQRCodeRequest.setQrPerPage(null);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY46);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_BY46template_1QrPerPage_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY46);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_BY712templateInvalid_ExceptionTest() throws Exception {
    manualQRCodeRequest.setQrPerPage(16);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY712);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_BY712template_4QrPerPage_Test() throws Exception {
    manualQRCodeRequest.setQrPerPage(4);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY712);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_BY712template_QrPerPageNull_Test() throws Exception {
    manualQRCodeRequest.setQrPerPage(null);
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY712);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_BY712template_1QrPerPage_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.BY712);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_SingleQRTemplateInvalid_ExceptionTest() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
    manualQRCodeRequest.setQrPerPage(2);
    manualQRCodeRequest.setIsDarkTheme(null);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_SingleQRTemplate_16QRPerPage_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
    manualQRCodeRequest.setQrPerPage(16);
    manualQRCodeRequest.setIsDarkTheme(null);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_SingleQRTemplate_1QRPerPage_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
    manualQRCodeRequest.setIsDarkTheme(null);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_SingleQRTemplate_32QRPerPage_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
    manualQRCodeRequest.setQrPerPage(32);
    manualQRCodeRequest.setIsDarkTheme(null);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  void validateManualQRCodeRequest_SingleQRTemplate_darkTheme_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  void validateManualQRCodeRequest_SingleQRTemplate_QrPerPageNull_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.SINGLE_QR);
    manualQRCodeRequest.setIsDarkTheme(null);
    manualQRCodeRequest.setQrPerPage(null);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_A1TemplateInvalid_ExceptionTest() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.A1);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_A1Template_InvalidRequest_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.A1);
    manualQRCodeRequest.setQrPerPage(null);
    manualQRCodeRequest.setIsDarkTheme(null);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_A1Template_Test() throws Exception {
    manualQRCodeRequest.setTemplateSize(TemplateSize.A1);
    manualQRCodeRequest.setQrPerPage(null);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_AllStoresTrue_ExceptionTest() throws Exception {
    manualQRCodeRequest.setAllStores(true);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_AllStoresTrue_Test() throws Exception {
    manualQRCodeRequest.setAllStores(true);
    manualQRCodeRequest.setProductDetailsRequestList(new ArrayList<>());
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateManualQRCodeRequest_QrForStore_Test() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.STORE);
    manualQRCodeRequest.setPrintPrice(false);
    try {
      RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
    } catch (ApplicationRuntimeException e) {
      assertEquals(INVALID_REQUEST_ERROR, e.getErrorMessage());
      throw e;
    }
  }

  @Test
  public void validateManualQRCodeRequest_QrForStore_ExceptionTest() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.STORE);
    manualQRCodeRequest.setPrintPrice(true);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_QrForAllProducts_ExceptionTest() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ALL_PRODUCTS);
    manualQRCodeRequest.setPrintPrice(true);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void validateManualQRCodeRequest_QrForAllProducts_test() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ALL_PRODUCTS);
    RequestHelper.validateManualQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateExcelQRCodeRequest_storeRequest_exceptionTest() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.STORE);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateExcelQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void getMerchantTypePureDeliveryTest() {
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantType.PURE_DELIVERY_SELLER.getType());
  }
  @Test
  public void getMerchantTypeCncTest() {
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.BLIBLI));
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantType.CNC_SELLER.getType());
  }

  @Test
  public void getMerchantTypeBfbCncTest() {
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.B2B_SELLER_CHANNEL));
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantType.BFB_CNC_SELLER.getType());
  }

  @Test
  public void getMerchantTypeBfbTest() {
    profileResponse.getCompany().setCncActivated(false);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.B2B_SELLER_CHANNEL));
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantType.BFB_SELLER.getType());
  }

  @Test
  public void getMerchantTypeCompanyNullTest() {
    profileResponse.setCompany(null);
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantType.PURE_DELIVERY_SELLER.getType());
  }

  @Test
  public void validateValidateQRCodeRequest() throws Exception {
    RequestHelper.validateExcelQRCodeRequest(manualQRCodeRequest);
  }

  @Test
  public void validateValidateQRCodeRequest_allProducts() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ALL_PRODUCTS);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateExcelQRCodeRequest(manualQRCodeRequest));
  }

  @Test
  public void populateDownloadQrCodeRequest_Test() throws Exception {
    DownloadQRCodeRequest downloadQRCodeRequest =
        RequestHelper.populateDownloadQrCodeRequest(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE,
            BUSINESS_PARTNER_NAME, manualQRCodeRequest);

    Assertions.assertEquals(downloadQRCodeRequest.getMerchantCode(), BUSINESS_PARTNER_CODE);
  }

  @Test
  public void populateDownloadQrCodeRequest_qrPerPageNull_Test() throws Exception {
    manualQRCodeRequest.setQrPerPage(null);
    DownloadQRCodeRequest downloadQRCodeRequest =
        RequestHelper.populateDownloadQrCodeRequest(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE,
            BUSINESS_PARTNER_NAME, manualQRCodeRequest);

    Assertions.assertEquals(downloadQRCodeRequest.getMerchantCode(), BUSINESS_PARTNER_CODE);
  }

  @Test
  public void validateAuthorisationTest() {
    RequestHelper.validateAuthorisation(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void validateModifiedPickupPoints() {
    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    ProductEditInfoV2WebRequest productEditInfoWebRequest = new ProductEditInfoV2WebRequest();
    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    RequestHelper.validateModifiedPickupPoints(productVariantPriceStockAndImagesWebRequestList);
  }

  private static List<ItemPickupPointWebRequest> getItemPickupPointRequests() {
    ItemPickupPointWebRequest itemPickupPointRequest = new ItemPickupPointWebRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointNotUpdated(Boolean.TRUE);
    ItemPickupPointWebRequest itemPickupPointRequestTwo = new ItemPickupPointWebRequest();
    itemPickupPointRequestTwo.setPickupPointId(PICKUP_POINT_CODE_TWO);
    itemPickupPointRequestTwo.setItemSku(ITEM_SKU);
    itemPickupPointRequestTwo.setPickupPointNotUpdated(Boolean.FALSE);
    List<ItemPickupPointWebRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    itemPickupPointRequestList.add(itemPickupPointRequestTwo);
    return itemPickupPointRequestList;
  }

  @Test
  void validateAuthorisationExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.validateAuthorisation(BUSINESS_PARTNER_CODE, STORE_ID));
  }

  @Test
  void validateGenerateShippingWeightRequestTest(){
    Assertions.assertThrows(ValidationException.class,
        () -> RequestHelper.validateGenerateShippingWeightRequest(2, 3, 4, Double.NaN));
  }

  @Test
  void validateGenerateShippingWeightRequestTest2(){
    Assertions.assertThrows(ValidationException.class,
        () -> RequestHelper.validateGenerateShippingWeightRequest(2, 3, Double.NaN, Double.NaN));
  }

  @Test
  public void validateGenerateShippingWeightRequestTest3(){
    Assertions.assertThrows(ValidationException.class,
        () -> RequestHelper.validateGenerateShippingWeightRequest(2, Double.NaN, Double.NaN,
            Double.NaN));
  }

  @Test
  void validateGenerateShippingWeightRequestTest4(){
    Assertions.assertThrows(ValidationException.class,
        () -> RequestHelper.validateGenerateShippingWeightRequest(Double.NaN, Double.NaN,
            Double.NaN, Double.NaN));
  }

  @Test
  public void validateSizeForExcelUploadsTest() throws Exception {
    MultipartFile multipartFile = generateDummyExcelMultipartFile();
    RequestHelper.validateSizeForExcelUploads(multipartFile, 3L);
  }

  @Test
  public void validateSizeForExcelUploadsWithSizeExceedingLimitTest() throws Exception {
    File largeDummyExcelFile = generateDummyExcelFileWithSize(2L);
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(largeDummyExcelFile));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> RequestHelper.validateSizeForExcelUploads(multipartFile, 1L));
  }

  private File generateDummyExcelFileWithSize(long sizeInMB) throws IOException, IOException {
    long sizeInBytes = sizeInMB * 1024 * 1024;
    File file = File.createTempFile("dummy-excel", ".xlsx");
    try (FileOutputStream fos = new FileOutputStream(file)) {
      while (file.length() < sizeInBytes) {
        fos.write(new byte[1024]); // Write 1KB chunks
      }
    }
    return file;
  }

  @Test
  public void validateProductSkuByBusinessPartnerCodeValidTest() {
    RequestHelper.validateProductSkuByBusinessPartnerCode(true, DEFAULT_BUSINESS_PARTNER_CODE, Collections.EMPTY_SET);
    RequestHelper.validateProductSkuByBusinessPartnerCode(true, DEFAULT_BUSINESS_PARTNER_CODE,
        Set.of(DEFAULT_PRODUCT_SKU));
    RequestHelper.validateProductSkuByBusinessPartnerCode(false, DEFAULT_BUSINESS_PARTNER_CODE,
        Set.of(DEFAULT_PRODUCT_SKU));
    RequestHelper.validateProductSkuByBusinessPartnerCode(false, BUSINESS_PARTNER_CODE, Set.of(DEFAULT_PRODUCT_SKU));
  }

  @Test
  void validateProductSkuByBusinessPartnerCodeInvalidTest() {
    Assertions.assertThrows(ValidationException.class,
        () -> RequestHelper.validateProductSkuByBusinessPartnerCode(true, BUSINESS_PARTNER_CODE,
            Set.of(DEFAULT_PRODUCT_SKU)));
  }

  @Test
  public void validateAccessibilityForAssemblyTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.ASSEMBLY_REQUEST_LISTING});
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly("ASSEMBLY_REQUEST");
    }
  }

  @Test
  public void validateAccessibilityForDisAssemblyTest(){
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(
          new String[] {Accessibilty.ASSEMBLY_REQUEST_LISTING,
              Accessibilty.DISASSEMBLY_REQUEST_LISTING});
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly("DISASSEMBLY_REQUEST");
    }
  }

  @Test
  public void validateAccessibilityForDisAssemblyExceptionTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.DISASSEMBLY_REQUEST_LISTING});
      Assertions.assertThrows(Exception.class,
          () -> RequestHelper.validateAccessibilityForAssemblyAndDisassembly(
              "DISASSEMBLY_REQUEST1"));
    }
  }

  @Test
  public void validateAccessibilityForTransferTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(
          new String[] {Accessibilty.TRANSFER_PRODUCT_LISTING,
              Accessibilty.TRANSFER_PRODUCT_LISTING});
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly("TRANSFER_REQUEST");
    }
  }

  @Test
  public void validateAccessibilityForTransferUnauthorizedTest() {
    try {
      Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly("TRANSFER_REQUEST");
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      Assertions.assertEquals("Unauthorize access :You are not authorized",
          applicationRuntimeException.getErrorMessage());
    }
  }


  @Test
  public void validateAccessibilityForAssemblyUnauthorizedTest() {
    try {
      Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly("ASSEMBLY_REQUEST");
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      Assertions.assertEquals("Unauthorize access :You are not authorized",
          applicationRuntimeException.getErrorMessage());
    }
  }

  @Test
  public void testCheckProductSkuOrItemSkuStartsWithBPCode_AllSkusStartWithBPCode() {
    List<String> skus = Arrays.asList("BP12345", "BP67890", "BP11223");
    boolean validateEnabled = true;
    String bpCode = "BP";//t t
      RequestHelper.checkProductSkuOrItemSkuStartsWithBPCode(skus, validateEnabled, bpCode);
    }

  @Test
  public void testCheckProductSkuOrItemSkuStartsWithBPCode_SomeSkusDoNotStartWithBPCode() {
    List<String> skus = Arrays.asList("BP12345", "INVALID67890", "BP11223");
    boolean validateEnabled = true;
    String bpCode = "BP";
    try {
      RequestHelper.checkProductSkuOrItemSkuStartsWithBPCode(skus, validateEnabled, bpCode);
      Assertions.fail("Should have thrown ValidationException");
    } catch (ValidationException ve) {
      Assertions.assertEquals(ErrorMessages.INVALID_GDN_SKU, ve.getMessage());
    }
  }

  @Test
  public void testCheckProductSkuOrItemSkuStartsWithBPCode_ValidationDisabled() {
    List<String> skus = Arrays.asList("BP12345", "INVALID67890", "BP11223");
    boolean validateEnabled = false;
    String bpCode = "BP";
    try {
      RequestHelper.checkProductSkuOrItemSkuStartsWithBPCode(skus, validateEnabled, bpCode);
    } catch (ValidationException ve) {
      Assertions.assertEquals(ErrorMessages.INVALID_GDN_SKU, ve.getMessage());
    }
  }
  @Test
  public void isBopisEligibleTest() {
    Assertions.assertTrue(RequestHelper.isBopisEligible(MERCHANT_TYPE_CC, true, false , List.of(MERCHANT_TYPE_CC), false));
    Assertions.assertTrue(RequestHelper.isBopisEligible(MERCHANT_TYPE_CC, true, true , List.of(MERCHANT_TYPE_CC), false));
    Assertions.assertTrue(RequestHelper.isBopisEligible(MERCHANT_TYPE_CC, false, true , List.of(MERCHANT_TYPE_CC), false));
    Assertions.assertFalse(RequestHelper.isBopisEligible(MERCHANT_TYPE_CC, false, true , List.of(MERCHANT_TYPE_CC), true));
    Assertions.assertTrue(RequestHelper.isBopisEligible(MERCHANT_TYPE_TD, false, true , List.of(MERCHANT_TYPE_CC), true));
  }

  @Test
  public void getMerchantTypeFromProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    Assertions.assertEquals(StringUtils.EMPTY, RequestHelper.getMerchantTypeFromProfileResponse(null));
    Assertions.assertEquals(StringUtils.EMPTY, RequestHelper.getMerchantTypeFromProfileResponse(profileResponse));
    profileResponse.setCompany(new CompanyDTO());
    Assertions.assertEquals(StringUtils.EMPTY, RequestHelper.getMerchantTypeFromProfileResponse(profileResponse));
    profileResponse.getCompany().setMerchantType(MERCHANT_CODE);
    Assertions.assertEquals(MERCHANT_CODE, RequestHelper.getMerchantTypeFromProfileResponse(profileResponse));
  }

  @Test
  public void validateProductEditAccessibilityFoundTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT});
      RequestHelper.validateProductEditAccessibility(true, new HashSet<>(),
          RequestHelper.DEFAULT_CLIENT_HOST);
    }
  }

  @Test
  public void validateProductEditAccessibilityExclusionListTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(new String[] {});
      RequestHelper.validateProductEditAccessibility(true,
          Set.of(RequestHelper.DEFAULT_CLIENT_HOST), RequestHelper.DEFAULT_CLIENT_HOST);
    }
  }

  @Test
  public void validateProductEditAccessibilityNotFoundTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA});
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> RequestHelper.validateProductEditAccessibility(true, new HashSet<>(),
              RequestHelper.DEFAULT_CLIENT_HOST));
    }
  }

  @Test
  public void validateProductEditAccessibilitySwitchOffTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA});
      RequestHelper.validateProductEditAccessibility(false, new HashSet<>(),
          RequestHelper.DEFAULT_CLIENT_HOST);
    }
  }

  @Test
  public void isInstoreEligibleSellerTest() {
    Assertions.assertFalse(RequestHelper.isInstoreEligibleSeller(null));
    profileResponse.setCompany(null);
    Assertions.assertFalse(RequestHelper.isInstoreEligibleSeller(profileResponse));
    profileResponse.setCompany(new CompanyDTO());
    Assertions.assertFalse(RequestHelper.isInstoreEligibleSeller(profileResponse));
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Assertions.assertTrue(RequestHelper.isInstoreEligibleSeller(profileResponse));
  }

  @Test
  public void isInstoreEligibleSellerWithSwitch() {
    Assertions.assertFalse(RequestHelper.isInstoreEligibleSellerWithSwitch(false, null));
    Assertions.assertFalse(RequestHelper.isInstoreEligibleSellerWithSwitch(true, null));
    profileResponse.setCompany(CompanyDTO.builder().offlineToOnlineFlag(true).build());
    Assertions.assertTrue(RequestHelper.isInstoreEligibleSellerWithSwitch(true, profileResponse));
  }

  @Test
  public void checkHideFromSellerAttributeTest() {
    CategoryAttributeResponse categoryAttribute = new CategoryAttributeResponse();
    categoryAttribute.setAttribute(AttributeResponse.builder().hideForSeller(false).build());
    Assertions.assertFalse(RequestHelper.checkHideFromSellerAttribute(false, categoryAttribute));
    Assertions.assertFalse(RequestHelper.checkHideFromSellerAttribute(true, categoryAttribute));
    categoryAttribute.getAttribute().setHideForSeller(true);
    Assertions.assertTrue(RequestHelper.checkHideFromSellerAttribute(true, categoryAttribute));
  }

  @Test
  public void validateAccessibilityForProductTabTest() {
    RequestHelper.validateAccessibilityForProductTab(false, new ArrayList<>(), false, StringUtils.EMPTY,
        Constants.CLIENT_TYPE_SELLER_API);
    RequestHelper.validateAccessibilityForProductTab(true, new ArrayList<>(), false, StringUtils.EMPTY,
        Constants.CLIENT_TYPE_SELLER_API);
    RequestHelper.validateAccessibilityForProductTab(true, new ArrayList<>(), true, StringUtils.EMPTY,
        Constants.CLIENT_TYPE_SELLER_API);
    RequestHelper.validateAccessibilityForProductTab(true, List.of(MANAGE_PRODUCT, MANAGE_PRODUCT_TAB), true,
        PRODUCT_TAB, Constants.CLIENT_TYPE_SELLER_API);
  }

  @Test
  public void validateAccessibilityForProductTabUnauthorizedTest() {
    Assertions.assertThrows(UnauthorizedException.class,
        () -> RequestHelper.validateAccessibilityForProductTab(true, List.of(MANAGE_PRODUCT), true,
            PRODUCT_TAB, Constants.CLIENT_TYPE_WEB));
  }

  @Test
  public void toProductBasicInfoDownloadRequestTest() {
    final ProductSummaryRequest PRODUCT_SUMMARY_REQUEST = new ProductSummaryRequest();
    ProductBasicInfoDownloadRequest result =
        RequestHelper.toProductBasicInfoDownloadRequest(TEST_USERNAME, TEST_BUSINESS_PARTNER_CODE, new HashMap<>(),
            TEST_RANDOM_UUID, TEST_FILE_NAME, PRODUCT_SUMMARY_REQUEST);
    assertNotNull(result);
    assertEquals(PRODUCT_SUMMARY_REQUEST, result.getProductSummaryRequest());
    assertEquals(TEST_RANDOM_UUID, result.getRequestId());
    assertEquals(DownloadType.ALL, result.getDownloadType());
    assertEquals(FileType.XLSX, result.getFileType());
    assertEquals(BulkProcessEntity.PRODUCT_BASIC_INFO, result.getBulkProcessEntity());
    assertEquals(TEST_FILE_NAME, result.getFilename());
    assertEquals(TEST_BUSINESS_PARTNER_CODE, result.getMerchantId());
    assertEquals(TEST_USERNAME, result.getUsername());
    assertTrue(result.isDirectDownload());
  }

  @Test
  public void toBulkBasicInfoRequest() throws Exception {
    profileResponse.setCompany(CompanyDTO.builder().offlineToOnlineFlag(true).build());
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName(DUMMY_FILE_NAME);
    request.setFilePath(DUMMY_FILE_NAME);
    BulkBasicInfoRequest bulkBasicInfoRequest =
        RequestHelper.toBulkBasicInfoRequest(BUSINESS_PARTNER_CODE, USERNAME, request, profileResponse,
            Constants.BULK_PROCESS_TYPE, false);
    assertEquals(BUSINESS_PARTNER_CODE, bulkBasicInfoRequest.getBusinessPartnerCode());
    assertEquals(BULK_PROCESS_TYPE, bulkBasicInfoRequest.getBulkProcessType());
    assertEquals(DUMMY_FILE_NAME, bulkBasicInfoRequest.getFileName());
    assertEquals(USERNAME, bulkBasicInfoRequest.getUpdatedBy());
    assertTrue(bulkBasicInfoRequest.isInstoreSeller());
  }

  @Test
  public void testValidateBusinessPartnerCode_SecurityDisabled() {
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode("MERCHANT123");
    String businessPartnerCode = "DIFFERENT123";
    RequestHelper.validateBusinessPartnerCode(businessPartnerCode, request, false);
  }

  @Test
  public void testValidateBusinessPartnerCode_SecurityEnabled_ValidCode() {
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode("MERCHANT123");
    String businessPartnerCode = "MERCHANT123";
    RequestHelper.validateBusinessPartnerCode(businessPartnerCode, request, true);
  }

  @Test
  public void testValidateBusinessPartnerCode_SecurityEnabled_InvalidCode() {
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode("MERCHANT123");
    String businessPartnerCode = "INVALID123";
    Assertions.assertThrows(ValidationException.class, () ->
        RequestHelper.validateBusinessPartnerCode(businessPartnerCode, request, true));
  }

  @Test
  void validateActiveBusinessPartner_shouldPass_whenProfileIsActive() {
    ProfileResponse profile = new ProfileResponse();
    profile.setMerchantStatus(ACTIVE_STATUS);
    RequestHelper.validateActiveBusinessPartner(VALID_BP_CODE, profile);
  }

  @Test
  void validateActiveBusinessPartner_shouldFail_whenProfileIsNull() {
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () ->
        RequestHelper.validateActiveBusinessPartner(INVALID_BP_CODE, null));
    assertTrue(exception.getMessage().contains(EXPECTED_ERROR_MESSAGE));
  }

  @Test
  void validateActiveBusinessPartner_shouldFail_whenMerchantStatusIsNotActive() {
    ProfileResponse profile = new ProfileResponse();
    profile.setMerchantStatus(INACTIVE_STATUS);
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () ->
        RequestHelper.validateActiveBusinessPartner(INVALID_BP_CODE, profile));
    assertTrue(exception.getMessage().contains(EXPECTED_ERROR_MESSAGE));
  }


  @Test
  void mapToOmniChannelSkuRequestTest() {
    OmniChannelSkuWebRequest omniChannelSkuWebRequest = new OmniChannelSkuWebRequest();
    omniChannelSkuWebRequest.setSellerCode(INVALID_BP_CODE);
    OmniChannelExistsRequest omniChannelSkuRequest =
      RequestHelper.mapToOmniChannelSkuRequest(omniChannelSkuWebRequest);
    assertEquals(INVALID_BP_CODE, omniChannelSkuRequest.getSellerCode());
  }
}

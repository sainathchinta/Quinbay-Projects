package com.gdn.partners.pcu.external.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ProductApiPath;
import com.gdn.partners.pcu.external.properties.EstimatedPriceProperties;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.ProductL3Service;
import com.gdn.partners.pcu.external.service.ProductLevel3WipService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.external.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.B2bFieldsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkProductSkuRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkRequest;
import com.gdn.partners.pcu.external.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistoryUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ImageRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ListingUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointCreateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PreOrderWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceChangeCompatibleRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerAttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWholesalePriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SubmitEvidenceIPRWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpcStatusWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BulkPendingRequestsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderPlacedWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3MasterWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSystemParameterSwitchWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UnifiedBulkDownloadWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UniquePickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class ProductControllerTest extends TestHelper {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ProductService productService;

  @Mock
  private EstimatedPriceProperties estimatedPriceProperties;

  @InjectMocks
  private ProductController productController;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private ProductL3Service productL3Service;

  @Mock
  private FileStorageService fileStorageService;


  private static final String ITEM_CODE = "itemCode";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final double LOWEST_PRICE_COEFFICIENT = 1.5;
  private static final double OFFER_PRICE = 10000;
  private static final String KEYWORD = "keyword";
  private static final Pageable pageable = PageRequest.of(0, 100);
  private static final Metadata metaData = new Metadata(0, 100, 100L);
  private static final String PRODUCT_ID = "product_id";
  private static final String MATERIAL_CODE = "materialCode";
  private static final String PRODUCT_CODE = "product-code";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String BUSINESS_PARTNER_CODE_DIFFERENT = "business-partner-code-different";
  private static final String PRODUCT_SKU_CODE = BUSINESS_PARTNER_CODE+ "-00001";
  private static final String GDN_PRODUCT_ITEM_SKU = "gdn-product-item-sku";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String ATTRIBUTE_ID = "attribute-id";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String ALLOWED_ATTRIBUTE_ID = "allowed-attribute-id";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_ID = "predefined-allowed-attribute-id";
  private static final String USER_NAME = "username";
  private static final int PRODUCTS_COUNT = 1234;
  private ProductDetailWebResponse productDetailWebResponse;
  private List<EstimateItemPriceWebResponse> responseList;
  private EstimateItemPriceWebResponse estimateItemPriceWebResponse;
  private static final String CATEGORY_ID = "categoryId";
  private static final String PRODUCT_NAME = "productName";
  private static final String UPCCODE = "upcCode";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_NAME = "category_name";
  private static final String CATALOG_CODE = "catalog-code";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final Integer SIZE_1 = 25;
  private static final String VIEWABLE = "viewable";
  private static final String TYPE = "type";
  private static final long TOTAL_RECORDS = 100;
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String DEFAULT_PRODUCT_SKU = "ABC-70000-12345";
  private static final String MERCHANT_CODE = "businessPartnerCode";
  private static final String SKU_CODE = "sku-code";
  private static final String ITEM_NAME = "itemName";
  private static final String ITEM_SKU = "itemSku";
  private static final double PRICE = 100000;
  private static final String IMAGE_URL = "imageUrl";
  private static final String SEARCH_KEY = "searchKey";
  private static final String PRODUCT_TYPE = "Active";
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String DEFAULT_CRITERIA = "ALL";
  private static final String DOARCHIVE = "doArchive";
  private static final String GDN_SKU = "gdnSku";
  private static final double TEST_COGS_VALUE = 53.53;
  private byte[] fileContent;
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  private static final String FILE_NAME_1 = "file.xls";
  private static final String FILE_NAME_2 = "file.zip";
  private static final Integer PRODUCT_SIZE = 100;
  private static final String WEB_MERCHANT_CODE = "webMerchantCode";
  private static final String WEB_ITEM_SKU = "webItemSku";
  private static final String IS_UPC_CODE = "isUPCCode";
  private static final String UPC_CODE = "1234567";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  public static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  private static final String LOCATION_PATH = "locationPath";
  private static final String DEFAULT_BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String DESCRIPTION = "description";
  private static final String THUMBNAIL_IMAGE = "thumbnail_image";
  private static final String TITLE = "title";
  private static final String FILE_NAME_3 = "file.xlsx";
  private static final String DUMMY_FILE_NAME1 = "dummy-excel-1.xlsx";
  private static final String DUMMY_FILE_NAME2 = "dummy-excel-2.xlsx";
  private static final String DUMMY_FILE_NAME4 = "VAT_bulk_upload_template.xlsx";
  private static final String DUMMY_FILE_NAME3 = "dummy-excel-3.xlsm";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_STATUS = "brandStatus";
  private static final String VALUE = "value";
  private static final String PRODUCT_TITLE = "PRODUCT_TITLE";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String BRAND = "brand";
  private static final String PRE_LIVE_REVIEW_TYPE = "prelive";
  private static final String ERROR_CODE = "error-code";
  private static final String NOTES = "notes";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "ABC-70000";
  private static final String FILE_PATH = "filePath";
  private List<String> categoryCodes;
  private List<String> categoryIds;
  private ProductItemDetailWebResponse productItemDetailWebResponse;
  private List<ProductItemDetailWebResponse> productItemDetailWebResponseList;
  private List<PriceChangeCompatibleRequest> priceChangeCompatibleRequests;
  private PriceChangeCompatibleRequest priceChangeCompatibleRequest;
  private Page<ProductItemDetailWebResponse> productItemDetailWebResponses;
  private List<ProductItemWebResponse> productItemWebResponses;
  private ProductItemWebResponse productItemWebResponse;
  private ProductCreationWebRequest productCreationWebRequest;
  private ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse;
  private ProductLevel3WebResponse productLevel3Response;
  private ProductLevel3MasterWebResponse productLevel3MasterWebResponse;
  private ActiveProductWebResponse activeProductWebResponse;
  private List<ActiveProductWebResponse> activeProductWebResponseList;
  private Page<ActiveProductWebResponse> activeProductWebResponsePage;
  private ActiveProductWebRequest activeProductWebRequest;
  private ProductL3ListingWebRequest productL3ListingWebRequest;
  private ProductL3ListingWebResponse productL3ListingWebResponse;
  private Page<ProductL3ListingWebResponse> productL3ListingWebResponseForQRPage;
  private SuspensionWebRequest suspensionWebRequest;
  private List<SuspensionWebResponse> suspensionWebResponseList;
  private Page<SuspensionWebResponse> suspensionWebResponsePage;
  private InActiveProductWebRequest inActiveProductWebRequest;
  private List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList;
  private InProcessProductWebRequest inProcessProductWebRequest;
  private List<String> itemSkus = new ArrayList<>();
  private ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest;
  private ProductLevel3WebRequest productLevel3WebRequest;
  private ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest;
  private PickupPointUpdateWebRequest pickupPointUpdateWebRequest;
  private BulkDeleteProductWipRequest bulkDeleteProductWipRequest;
  private Map<String, ProductLevel3UpdateSummaryRequest> productLevel3UpdateSummaryRequestMap;
  private Map<String, ProductLevel3SummaryResponse> productLevel3SummaryResponseMap;
  private Map<String, String> externalFiles = Map.of("file","file.xlsx");
  private MockMultipartFile multipartFile;
  private BulkRequest bulkRequest;
  private ProductLevel3SummaryRequest productLevel3SummaryRequest;
  private List<String> productBusinessPartnerIds = new ArrayList<>();
  private BaseResponse baseResponse;
  private UpdateImageRequest updateImageRequest;
  private YouTubeAPIWebResponse youTubeAPIWebResponse;
  private UnifiedBulkDownloadWebResponse unifiedBulkDownloadWebResponse;
  private ProductEditInfoWebRequest productEditInfoWebRequest;
  private EditProductWebResponse editProductWebResponse;
  private BulkProductSkuRequest bulkProductSkuRequest;
  private ProductSummaryWebRequest productSummaryWebRequest;
  private B2bFieldsWebRequest b2bFieldsWebRequest;
  private static final String USP_WITH_MORE_THAN_400_CHARACTERS =
      "Case Premium Bagian Belakang Dove Transparan , Kelebihan :~Bagian belakang dove.\n"
          + "~Ada crack / Anti jatuh.\n" + "~Bagian belakang transparan tampil elegant\n"
          + "~Anti Pecah / Sobek karena pakai bahan lentur.\n" + "~Penuh warna glosy di List sekelilingnya\n"
          + "~Slim dan pas di gengam di tangan\n" + "~Anti lecet karena menggunakan Kualitas bahan terbaik\n"
          + "~Lepas pasang sangat mudah , tidak keras / kasar. Lepas pasang sangat mudah , tidak keras / kasar2.";

  private static final String USP_WITH_SPECIAL_CHARACTERS_AND_SPACES = "• Susu formula\n"
      + "• Susu bernutrisi yang     dapat memenuhi kebutuhan nutrisi anak usia 1 - 3 tahun sebesar 190 kalori setiap saji\n"
      + "• Mengandung campuran lemak nabati sebanyak 33% dari kandungan total energi\n"
      + "• Dengan alfa-laktalbumin, oligofruktosa, kalsium, kolin, fosfor, selenium, vitamin C, E, dan D, serta 52 jenis makro mikro nutrien yang penting untuk pertumbuhan anak\n"
      + "• Rasa : Vanila, Netto : 1600 gr\n" + "• Rasa : Vanilad";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private PreOrderWebRequest preOrderWebRequest;
  private HistoryUpdateWebRequest historyUpdateWebRequest = new HistoryUpdateWebRequest();
  private SubmitEvidenceIPRWebRequest submitEvidenceIPRWebRequest;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequest;

  @Captor
  private ArgumentCaptor<BulkDeleteProductWipRequest> bulkDeleteProductWipRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3UpdateSummaryRequest> productLevel3UpdateSummaryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, ProductLevel3UpdateSummaryRequest>> mapArgumentCaptor;

  @Captor
  private ArgumentCaptor<ActiveProductWebRequest> activeProductWebRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductSummaryWebRequest> productSummaryWebRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3WebRequest> productLevel3WebRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3UpdateWebRequest> productLevel3UpdateWebRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<UpdateImageRequest> updateImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductEditInfoWebRequest> productEditInfoWebRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<UpdateProductLevel3InfoRequest>
      updateProductLevel3InfoRequestArgumentCaptor;

  @Mock
  private GCSProperties gcsProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(productController).build();
    productDetailWebResponse = new ProductDetailWebResponse();
    productDetailWebResponse.setId(PRODUCT_ID);

    productLevel3WipDetailWebResponse = new ProductLevel3WipDetailWebResponse();
    productLevel3WipDetailWebResponse.setProductSku(GDN_PRODUCT_SKU);

    productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setName(PRODUCT_NAME);
    productCreationWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ProductItemCreationWebRequest productItemCreationWebRequest = new ProductItemCreationWebRequest();
    productItemCreationWebRequest.setGdnProductItemSku(GDN_PRODUCT_ITEM_SKU);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode("hash-code");
    imageRequest.setLocationPath("location-path");
    List<ImageRequest> imageRequests = new ArrayList<>();
    imageRequests.add(imageRequest);
    productItemCreationWebRequest.setImages(imageRequests);
    TreeMap<String, String> attributesMap = new TreeMap<String, String>();
    attributesMap.put("image1", "hash-code1");
    productItemCreationWebRequest.setAttributesMap(attributesMap);

    List<ProductItemCreationWebRequest> productItemCreationWebRequests = new ArrayList<>();
    productItemCreationWebRequests.add(productItemCreationWebRequest);
    productCreationWebRequest.setProductItemRequests(productItemCreationWebRequests);

    ProductBusinessPartnerAttributeWebRequest productBusinessPartnerAttributeWebRequest =
        new ProductBusinessPartnerAttributeWebRequest();
    productBusinessPartnerAttributeWebRequest.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttributeWebRequest.setValue(ATTR_VALUE);
    List<ProductBusinessPartnerAttributeWebRequest> productBusinessPartnerAttributeWebRequests = new ArrayList<>();
    productBusinessPartnerAttributeWebRequests.add(productBusinessPartnerAttributeWebRequest);
    productCreationWebRequest.setProductBusinessPartnerAttributes(productBusinessPartnerAttributeWebRequests);

    List<ProductCategoryWebRequest> productCategories = new ArrayList<>();
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    CatalogWebRequest catalogWebRequest = new CatalogWebRequest();
    catalogWebRequest.setCatalogCode(CATALOG_CODE);
    productCategoryWebRequest.setCatalog(catalogWebRequest);
    productCategories.add(productCategoryWebRequest);
    productCreationWebRequest.setProductCategories(productCategories);

    List<ProductAttributeWebRequest> productAttributes = new ArrayList<>();
    ProductAttributeWebRequest productAttributeWebRequest = new ProductAttributeWebRequest();
    AttributeWebRequest attributeWebRequest = new AttributeWebRequest();
    attributeWebRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeWebRequest.setAttributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE);
    List<AllowedAttributeValueWebRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueWebRequest allowedAttributeValueWebRequest = new AllowedAttributeValueWebRequest();
    allowedAttributeValueWebRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueWebRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueWebRequest);
    attributeWebRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest =
        new PredefinedAllowedAttributeValueWebRequest();
    predefinedAllowedAttributeValueWebRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueWebRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueWebRequest);
    attributeWebRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeWebRequest.setProductAttributeValues(Arrays.asList(new ProductAttributeValueWebRequest()));
    productAttributeWebRequest.setAttribute(attributeWebRequest);
    productAttributeWebRequest.setSequence(2);
    productAttributes.add(productAttributeWebRequest);
    productCreationWebRequest.setProductAttributes(productAttributes);

    List<ProductItemWebRequest> productItems = new ArrayList<>();
    ProductItemWebRequest productItemRequest = new ProductItemWebRequest();
    productItemRequest.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemRequest.setImages(imageRequests);
    productItemRequest.setAttributesMap(attributesMap);
    ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest = new ProductItemAttributeValueWebRequest();
    productItemAttributeValueWebRequest.setAttribute(attributeWebRequest);
    productItemAttributeValueWebRequest.setValue(ATTR_VALUE);
    productItemAttributeValueWebRequest.setId("ID");
    List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests = new ArrayList<>();
    productItemAttributeValueWebRequests.add(productItemAttributeValueWebRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueWebRequests);
    productItems.add(productItemRequest);
    productCreationWebRequest.setProductItems(productItems);

    responseList = new ArrayList<>();
    estimateItemPriceWebResponse = new EstimateItemPriceWebResponse();
    estimateItemPriceWebResponse.setOfferPrice(OFFER_PRICE);
    responseList.add(estimateItemPriceWebResponse);
    productDetailWebResponse = new ProductDetailWebResponse();
    productDetailWebResponse.setId(PRODUCT_ID);
    productItemDetailWebResponse = new ProductItemDetailWebResponse();
    productItemDetailWebResponse.setProductId(PRODUCT_ID);
    productItemDetailWebResponseList = new ArrayList<>();
    productItemDetailWebResponseList.add(productItemDetailWebResponse);
    categoryCodes = Arrays.asList(CATEGORY_CODE);
    categoryIds = Arrays.asList(CATEGORY_ID);
    productItemWebResponses = new ArrayList<>();
    productItemWebResponse = new ProductItemWebResponse();
    productItemWebResponse.setId(PRODUCT_ID);
    productItemWebResponse.setGeneratedItemName(PRODUCT_NAME);
    productItemWebResponses.add(productItemWebResponse);
    productItemDetailWebResponses = new PageImpl<>(productItemDetailWebResponseList, pageable, TOTAL_RECORDS);

    ItemDetailWebResponse itemDetailWebResponse = new ItemDetailWebResponse();
    itemDetailWebResponse.setItemName(ITEM_NAME);
    itemDetailWebResponse.setItemSku(ITEM_SKU);
    List<ItemDetailWebResponse> itemDetailWebResponseList = new ArrayList<>();
    itemDetailWebResponseList.add(itemDetailWebResponse);
    activeProductWebResponse =
        new ActiveProductWebResponse(SKU_CODE, PRODUCT_CODE, PRODUCT_NAME, IMAGE_URL, MERCHANT_CODE, 1,
            itemDetailWebResponseList, Boolean.FALSE);
    activeProductWebResponseList = new ArrayList<>();
    activeProductWebResponseList.add(activeProductWebResponse);
    activeProductWebResponsePage = new PageImpl<>(activeProductWebResponseList, pageable, TOTAL_RECORDS);

    activeProductWebRequest = new ActiveProductWebRequest();
    activeProductWebRequest.setCategoryCode(CATEGORY_CODE);
    activeProductWebRequest.setSearchKey(SEARCH_KEY);
    activeProductWebRequest.setDiscoverable(true);
    activeProductWebRequest.setBuyable(true);
    activeProductWebRequest.setPage(PAGE);
    activeProductWebRequest.setSize(SIZE);
    activeProductWebRequest.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);

    productL3ListingWebRequest = new ProductL3ListingWebRequest();
    productL3ListingWebRequest.setCncActivated(true);
    productL3ListingWebRequest.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    productL3ListingWebRequest.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    productL3ListingWebRequest.setSearchKey(SEARCH_KEY);

    productL3ListingWebResponse = new ProductL3ListingWebResponse();
    productL3ListingWebResponse.setImageUrl(IMAGE_URL);
    productL3ListingWebResponse.setProductName(PRODUCT_NAME);
    productL3ListingWebResponse.setProductSku(PRODUCT_SKU);
    productL3ListingWebResponse.setVariantCount(1);
    productL3ListingWebResponse.setCategoryName(CATEGORY_NAME);

    productL3ListingWebResponseForQRPage = new PageImpl<>(
        Collections.singletonList(productL3ListingWebResponse), pageable, TOTAL_RECORDS);

    suspensionWebResponseList = new ArrayList<>();
    suspensionWebResponseList.add(new SuspensionWebResponse());
    suspensionWebResponsePage = new PageImpl<>(suspensionWebResponseList, pageable, TOTAL_RECORDS);
    suspensionWebRequest = new SuspensionWebRequest();
    suspensionWebRequest.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    inActiveProductWebRequest = InActiveProductWebRequest.builder().businessPartnerCode(Constants.BUSINESS_PARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .searchKey(SEARCH_KEY).orderBy(ORDER_BY).sortBy(SORT_BY).build();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemName(ITEM_NAME);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3SummaryResponse.setCategoryCode(CATEGORY_CODE);
    productLevel3SummaryResponseList = new ArrayList<>();
    productLevel3SummaryResponseList.add(productLevel3SummaryResponse);

    inProcessProductWebRequest =
        InProcessProductWebRequest.builder().businessPartnerCode(Constants.BUSINESS_PARTNER_CODE)
            .categoryCodes(Arrays.asList(CATEGORY_CODE)).searchKey(SEARCH_KEY).criteria(DEFAULT_CRITERIA)
            .orderBy(ORDER_BY).sortBy(SORT_BY).build();
    itemSkus.add(ITEM_SKU);

    productLevel3UpdateSummaryRequest = new ProductLevel3UpdateSummaryRequest();
    ProductLevel3PriceRequest priceRequest = new ProductLevel3PriceRequest();
    priceRequest.setPrice(10000.0);
    priceRequest.setSalePrice(5000.0);
    productLevel3UpdateSummaryRequest.setPrices(Collections.singletonList(priceRequest));

    baseResponse = new BaseResponse(null, null, true, REQUEST_ID);

    bulkDeleteProductWipRequest = new BulkDeleteProductWipRequest();
    Set<String> productLevel1Ids = new HashSet<>();
    productLevel1Ids.add(PRODUCT_ID);
    bulkDeleteProductWipRequest.setProductLevel1Ids(productLevel1Ids);

    productLevel3UpdateSummaryRequestMap = new LinkedHashMap<>();
    productLevel3UpdateSummaryRequestMap.put(GDN_SKU, productLevel3UpdateSummaryRequest);

    productLevel3SummaryResponseMap = new HashMap<>();
    productLevel3SummaryResponseMap.put(GDN_SKU, new ProductLevel3SummaryResponse());

    fileContent = new byte[] {-1, -40, -20, -10};

    bulkRequest = new BulkRequest();
    bulkRequest.setGdnSkus(Arrays.asList(GDN_SKU));

    productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setCategoryCode(CATEGORY_CODE);
    productLevel3SummaryRequest.setItemCode(ITEM_CODE);

    productBusinessPartnerIds.add(PRODUCT_BUSINESS_PARTNER_ID);

    productLevel3WebRequest = new ProductLevel3WebRequest();
    productLevel3WebRequest.setProductSku(BUSINESS_PARTNER_CODE+ "-00001");

    productLevel3UpdateWebRequest = new ProductLevel3UpdateWebRequest();
    productLevel3UpdateWebRequest.setProductSku(BUSINESS_PARTNER_CODE+ "-00001");

    pickupPointUpdateWebRequest = new PickupPointUpdateWebRequest();
    pickupPointUpdateWebRequest.setProductSku(BUSINESS_PARTNER_CODE+ "-00001");

    productLevel3Response = new ProductLevel3WebResponse();
    productLevel3Response.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3Response.setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));

    productLevel3MasterWebResponse = new ProductLevel3MasterWebResponse();
    productLevel3MasterWebResponse.setProductLevel3(productLevel3Response);

    updateImageRequest = new UpdateImageRequest();
    ProductLevel3ImageRequest productLevel3ImageRequest = new ProductLevel3ImageRequest();
    productLevel3ImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3ImageRequest.setSequence(0);
    productLevel3ImageRequest.setMainImage(true);
    updateImageRequest.setItemSku(ITEM_SKU);
    updateImageRequest.setProductCode(PRODUCT_CODE);
    updateImageRequest.setProductSku(GDN_PRODUCT_SKU);
    updateImageRequest.setMasterDataItemImages(Arrays.asList(productLevel3ImageRequest));

    youTubeAPIWebResponse =
        YouTubeAPIWebResponse.builder().isValid(true).description(DESCRIPTION).thumbnailImageUrl(THUMBNAIL_IMAGE)
            .title(TITLE).build();

    priceChangeCompatibleRequest =
        PriceChangeCompatibleRequest.builder().categoryCode(CATEGORY_CODE).itemSku(ITEM_SKU).salePrice(PRICE).build();
    priceChangeCompatibleRequests = new ArrayList<>();
    priceChangeCompatibleRequests.add(priceChangeCompatibleRequest);

    unifiedBulkDownloadWebResponse = UnifiedBulkDownloadWebResponse.builder().filePath("filePath").build();

    preOrderWebRequest =
        PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productEditInfoWebRequest = new ProductEditInfoWebRequest();
    productEditInfoWebRequest.setProductCode(PRODUCT_CODE);
    productEditInfoWebRequest.setProductSku(BUSINESS_PARTNER_CODE);
    productEditInfoWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditInfoWebRequest.setBrand(BRAND);
    productEditInfoWebRequest.setCategoryCode(CATEGORY_CODE);
    productEditInfoWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditInfoWebRequest.setDescription(DESCRIPTION);

    b2bFieldsWebRequest = new B2bFieldsWebRequest();
    b2bFieldsWebRequest.setBuyable(true);
    b2bFieldsWebRequest.setDisplay(true);
    b2bFieldsWebRequest.setManaged(true);
    b2bFieldsWebRequest.setPrice(100.0);

    productCreationWebRequest.setB2bActivated(false);
    productCreationWebRequest.setB2cActivated(true);

    editProductWebResponse =
        EditProductWebResponse.builder().productReview(true).reviewType(PRE_LIVE_REVIEW_TYPE).build();

    bulkProductSkuRequest =
        BulkProductSkuRequest.builder().productSkus(Arrays.asList(PRODUCT_SKU)).build();
    productSummaryWebRequest =
        ProductSummaryWebRequest.builder().categoryCodes(Arrays.asList(CATEGORY_CODE)).build();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    submitEvidenceIPRWebRequest =
        SubmitEvidenceIPRWebRequest.builder().productSku(PRODUCT_SKU).evidenceFilePath(FILE_PATH)
            .notes(NOTES).build();
    MDC.clear();
  }

  @Test
  public void getCategorySuggestionsTest() throws Exception {
    ListBaseResponse<CategorySuggestionWebResponse> response =
        new ListBaseResponse<>(null, null, true, Constants.REQUEST_ID, new ArrayList<>(), metaData);
    when(productService.getCategorySuggestions(eq(KEYWORD), eq(pageable), anyString(), eq(false)))
        .thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_CATEGORY_SUGGESTIONS).param("keyword", KEYWORD)
            .param("page", "0").param("size", "100").param(IS_UPC_CODE, Boolean.FALSE.toString())
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).getCategorySuggestions(eq(KEYWORD), eq(pageable), anyString(), eq(false));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void getTopCategorySuggestionsTest() throws Exception {
    List<List<SimpleCategoryWebResponse>> response = new ArrayList<>();
    when(productService.getTopCategorySuggestions(eq(KEYWORD), eq(pageable), anyString())).thenReturn(response);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_TOP_CATEGORY_SUGGESTIONS).param("keyword", KEYWORD)
            .param("page", "0").param("size", "100").contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).getTopCategorySuggestions(eq(KEYWORD), eq(pageable), anyString());
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getEstimatedPrice_Test() throws Exception {
    when(estimatedPriceProperties.getLowestPriceCoefficient()).thenReturn(LOWEST_PRICE_COEFFICIENT);
    when(productService.getEstimatedPriceByItemCodes(Collections.singletonList(ITEM_CODE), LOWEST_PRICE_COEFFICIENT))
        .thenReturn(responseList);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ESTIMATED_PRICE_BY_ITEM_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(Collections.singletonList(ITEM_CODE))).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.content[0].offerPrice", is(OFFER_PRICE)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(estimatedPriceProperties).getLowestPriceCoefficient();
    verify(productService).getEstimatedPriceByItemCodes(Collections.singletonList(ITEM_CODE), LOWEST_PRICE_COEFFICIENT);
  }

  @Test
  public void getEstimatedPrice_EmptyItemCodeTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ESTIMATED_PRICE_BY_ITEM_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(CollectionUtils.EMPTY_COLLECTION)).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getProductDetailsByProductIdTest() throws Exception {
    when(this.productService.getProductDetailsByProductId(PRODUCT_ID)).thenReturn(productDetailWebResponse);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.INFO, PRODUCT_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.id", equalTo(PRODUCT_ID)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(this.productService).getProductDetailsByProductId(PRODUCT_ID);
  }

  @Test
  public void getCogsValueTest() throws Exception {
    when(this.productService.getCogsValue(Mockito.any(), Mockito.any()))
        .thenReturn(TEST_COGS_VALUE);

when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_COGS_VALUE, MATERIAL_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(this.productService).getCogsValue(Mockito.any(), Mockito.any());
  }

  @Test
  public void getCogsValueTestCogsValueNull() throws Exception {
    when(this.productService.getCogsValue(Mockito.any(), Mockito.any())).thenReturn(null);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_COGS_VALUE, MATERIAL_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(this.productService).getCogsValue(Mockito.any(), Mockito.any());
  }

  @Test
  public void getProductItemInfoBynameAndCategoryCodesTest() throws Exception {
    when(productService.getProductItemsByKeywordAndCategoryCodes(PRODUCT_NAME, categoryCodes, PAGE, SIZE, Boolean.TRUE))
        .thenReturn(productItemDetailWebResponses);

    when(mandatoryParameterHelper.isExternalOnly()).thenReturn("TRUE");

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BY_PRODUCT_NAME_AND_CATEGORY_CODES)
            .param(PRODUCT_NAME, PRODUCT_NAME).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(categoryCodes))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).getProductItemsByKeywordAndCategoryCodes(PRODUCT_NAME, categoryCodes, PAGE, SIZE, Boolean.TRUE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void getProductSuggestionsTest() throws Exception {
    when(this.productService.getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE))
        .thenReturn(productItemDetailWebResponseList);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BY_ITEM_NAME_AND_CATEGORY_ID)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE))
            .param(PRODUCT_NAME, PRODUCT_NAME).param(CATEGORY_ID, CATEGORY_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content[0].productId", equalTo(PRODUCT_ID)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(this.productService).getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE);
  }

  @Test
  public void createProductTest() throws Exception {
    when(productService.createProduct(eq(Constants.USER_NAME), Mockito.any(ProductCreationRequest.class),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null))).thenReturn(new CreateProductResponse());
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productCreationWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).createProduct(eq(Constants.USER_NAME), productCreationRequest.capture(),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null));
    verify(mandatoryParameterHelper, times(3)).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper).isExternalOnly();
    ProductCreationRequest productCreationRequestModel = productCreationRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productCreationRequestModel.getName());
    Assertions.assertEquals(Constants.BUSINESS_PARTNER_CODE, productCreationRequestModel.getBusinessPartnerCode());
    Assertions.assertEquals(ATTRIBUTE_ID,
        productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    Assertions.assertEquals(2l, productCreationRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues()
            .get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
        productCreationRequestModel.getProductItemRequests().get(0).getGdnProductItemSku());
    Assertions.assertEquals("location-path",
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals("hash-code1",
        productCreationRequestModel.getProductItemRequests().get(0).getAttributesMap().get("image1"));
    Assertions.assertEquals(Constants.STORE_ID, productCreationRequestModel.getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getStoreId());
  }

  @Test
  public void createProductV2Test() throws Exception {
    when(productService.createProduct(eq(Constants.USER_NAME), Mockito.any(ProductCreationRequest.class),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null))).thenReturn(new CreateProductResponse());
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(true));
    ProductItemLogisticsWebRequest productItemLogisticsWebRequest = new ProductItemLogisticsWebRequest();
    productItemLogisticsWebRequest.setLogisticProductCode(LOCATION_PATH);
    productItemLogisticsWebRequest.setSelected(true);
    productCreationWebRequest
        .setProductItemLogisticsWebRequests(Collections.singletonList(productItemLogisticsWebRequest));
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setBuyable(true);
    pickupPointCreateWebRequest.setDisplay(true);
    pickupPointCreateWebRequest.setCncActive(true);
    pickupPointCreateWebRequest.setMinimumStock(5);
    pickupPointCreateWebRequest.setStock(1);
    pickupPointCreateWebRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateWebRequest.setPrice(100.0);
    pickupPointCreateWebRequest.setSalePrice(99.0);
    pickupPointCreateWebRequest.setB2bFields(b2bFieldsWebRequest);
    pickupPointCreateWebRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceWebRequest productItemWholesalePriceWebRequest = new ProductItemWholesalePriceWebRequest();
    productItemWholesalePriceWebRequest.setQuantity(2);
    productItemWholesalePriceWebRequest.setWholesaleDiscount(10);
    pickupPointCreateWebRequest
        .setProductItemWholesalePriceRequests(Collections.singletonList(productItemWholesalePriceWebRequest));
    productCreationWebRequest.getProductItemRequests().get(0).setMinimumStock(6);
    productCreationWebRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateWebRequest));
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setMainImages(true);
    imageRequest.setLocationPath(LOCATION_PATH);
    productCreationWebRequest.setCommonImages(Collections.singletonList(imageRequest));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.ADD).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productCreationWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).createProductV2(eq(Constants.USER_NAME), productCreationRequest.capture(),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null));
    verify(mandatoryParameterHelper, times(3)).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper, times(1)).isExternalOnly();
    ProductCreationRequest productCreationRequestModel = productCreationRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productCreationRequestModel.getName());
    Assertions.assertEquals(Constants.BUSINESS_PARTNER_CODE, productCreationRequestModel.getBusinessPartnerCode());
    Assertions.assertEquals(ATTRIBUTE_ID,
        productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    Assertions.assertEquals(2l, productCreationRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues()
            .get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
        productCreationRequestModel.getProductItemRequests().get(0).getGdnProductItemSku());
    Assertions.assertEquals("location-path",
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals("hash-code1",
        productCreationRequestModel.getProductItemRequests().get(0).getAttributesMap().get("image1"));
    Assertions.assertEquals(Constants.STORE_ID, productCreationRequestModel.getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getStoreId());
    PickupPointCreateRequest pickupPointCreateRequest =
        productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertTrue(productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0).isBuyable());
    Assertions.assertTrue(productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0).isDisplay());
    Assertions.assertEquals(5, pickupPointCreateRequest.getMinimumStock(), 0);
    Assertions.assertEquals(1, pickupPointCreateRequest.getStock(), 0);
    Assertions.assertEquals(100.0, pickupPointCreateRequest.getPrice(), 0);
    Assertions.assertEquals(99.0, pickupPointCreateRequest.getSalePrice(), 0);
    Assertions.assertTrue(pickupPointCreateRequest.getWholesalePriceActivated());
    Assertions.assertEquals(2, pickupPointCreateRequest.getProductItemWholesalePriceRequests().get(0).getQuantity(), 0);
    Assertions
        .assertEquals(10, pickupPointCreateRequest.getProductItemWholesalePriceRequests().get(0).getWholesaleDiscount(),
            0);
    Assertions.assertEquals(LOCATION_PATH,
        productCreationRequestModel.getProductItemLogisticsRequests().get(0).getLogisticProductCode());
    Assertions.assertTrue(productCreationRequestModel.getProductItemLogisticsRequests().get(0).isSelected());
    Assertions.assertTrue(productCreationRequestModel.getCommonImages().get(0).isMainImages());
    Assertions.assertEquals(LOCATION_PATH, productCreationRequestModel.getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productCreationWebRequest.isB2cActivated());
    Assertions.assertFalse(productCreationWebRequest.isB2bActivated());
    Assertions.assertNotNull(pickupPointCreateWebRequest.getB2bFields());
  }

  @Test
  public void createProductWholesaleEmptyV2Test() throws Exception {
    when(productService.createProduct(eq(Constants.USER_NAME), Mockito.any(ProductCreationRequest.class),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null))).thenReturn(new CreateProductResponse());
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(true));
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setBuyable(true);
    pickupPointCreateWebRequest.setDisplay(true);
    pickupPointCreateWebRequest.setCncActive(true);
    pickupPointCreateWebRequest.setMinimumStock(5);
    pickupPointCreateWebRequest.setStock(1);
    pickupPointCreateWebRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateWebRequest.setPrice(100.0);
    pickupPointCreateWebRequest.setSalePrice(99.0);
    pickupPointCreateWebRequest.setWholesalePriceActivated(true);
    pickupPointCreateWebRequest.setB2bFields(null);
    productCreationWebRequest.getProductItemRequests().get(0).setMinimumStock(6);
    productCreationWebRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateWebRequest));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.ADD).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productCreationWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).createProductV2(eq(Constants.USER_NAME), productCreationRequest.capture(),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null));
    verify(mandatoryParameterHelper, times(3)).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper, times(1)).isExternalOnly();
    ProductCreationRequest productCreationRequestModel = productCreationRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productCreationRequestModel.getName());
    Assertions.assertEquals(Constants.BUSINESS_PARTNER_CODE, productCreationRequestModel.getBusinessPartnerCode());
    Assertions.assertEquals(ATTRIBUTE_ID,
        productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    Assertions.assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    Assertions.assertEquals(2l, productCreationRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues()
            .get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
        productCreationRequestModel.getProductItemRequests().get(0).getGdnProductItemSku());
    Assertions.assertEquals("location-path",
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals("hash-code1",
        productCreationRequestModel.getProductItemRequests().get(0).getAttributesMap().get("image1"));
    Assertions.assertEquals(Constants.STORE_ID, productCreationRequestModel.getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getStoreId());
    PickupPointCreateRequest pickupPointCreateRequest =
        productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertTrue(productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0).isBuyable());
    Assertions.assertTrue(productCreationRequestModel.getProductItemRequests().get(0).getPickupPoints().get(0).isDisplay());
    Assertions.assertEquals(5, pickupPointCreateRequest.getMinimumStock(), 0);
    Assertions.assertEquals(1, pickupPointCreateRequest.getStock(), 0);
    Assertions.assertEquals(100.0, pickupPointCreateRequest.getPrice(), 0);
    Assertions.assertEquals(99.0, pickupPointCreateRequest.getSalePrice(), 0);
    Assertions.assertTrue(pickupPointCreateRequest.getWholesalePriceActivated());
  }

  @Test
  public void createProductWithDescriptiveAttributeValueNullTest() throws Exception {

    productCreationWebRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(null);
    when(productService.createProduct(eq(Constants.USER_NAME), Mockito.any(ProductCreationRequest.class),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null))).thenReturn(new CreateProductResponse());
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productCreationWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).createProduct(eq(Constants.USER_NAME), productCreationRequest.capture(),
        eq(Constants.BUSINESS_PARTNER_CODE), eq(null));
    verify(mandatoryParameterHelper, times(3)).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    ProductCreationRequest productCreationRequestModel = productCreationRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productCreationRequestModel.getName());
    Assertions.assertEquals(Constants.BUSINESS_PARTNER_CODE, productCreationRequestModel.getBusinessPartnerCode());
    Assertions.assertEquals(ATTRIBUTE_ID,
        productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions
        .assertEquals(ATTR_VALUE, productCreationRequestModel.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    Assertions.assertEquals(2l, productCreationRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues()
            .get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
        productCreationRequestModel.getProductItemRequests().get(0).getGdnProductItemSku());
    Assertions.assertEquals("location-path",
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals("hash-code1",
        productCreationRequestModel.getProductItemRequests().get(0).getAttributesMap().get("image1"));
    Assertions.assertEquals(Constants.STORE_ID, productCreationRequestModel.getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductItemRequests().get(0).getImages().get(0).getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductAttributes().get(0).getAttribute().getStoreId());
    Assertions.assertEquals(Constants.STORE_ID,
        productCreationRequestModel.getProductCategories().get(0).getCategory().getStoreId());
    verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void getProductsCountTest() throws Exception {
    when(this.productService.getProductsCount(true)).thenReturn(PRODUCTS_COUNT);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCTS_COUNT).param(VIEWABLE, String.valueOf(Boolean.TRUE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(PRODUCTS_COUNT)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(this.productService).getProductsCount(true);
  }

  @Test
  public void getProductDetailByProductSkuTest() throws Exception {
    when(this.productLevel3WipService.findProductDetailByProductSku(PRODUCT_SKU_CODE, false))
        .thenReturn(productLevel3WipDetailWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL, PRODUCT_SKU_CODE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).
        andExpect(jsonPath("$.value.productSku", equalTo(GDN_PRODUCT_SKU)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(this.productLevel3WipService).findProductDetailByProductSku(PRODUCT_SKU_CODE, false);
  }

  @Test
  public void getProductDetailByProductSkuIsActiveTest() throws Exception {
    when(this.productLevel3WipService.findProductDetailByProductSku(PRODUCT_SKU_CODE, true))
        .thenReturn(productLevel3WipDetailWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL, PRODUCT_SKU_CODE).contentType(MediaType.APPLICATION_JSON)
            .param("isActive", Boolean.TRUE.toString()).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).
        andExpect(jsonPath("$.value.productSku", equalTo(GDN_PRODUCT_SKU)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(this.productLevel3WipService).findProductDetailByProductSku(PRODUCT_SKU_CODE, true);
  }

  @Test
  void getProductDetailByProductSkuExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    try{
      MockHttpServletRequestBuilder requestBuilder =
          get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL, GDN_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
              .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false))));
    }finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  void getProductDetailByProductSkuIsActivExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    try{
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL, GDN_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
            .param("isActive", Boolean.TRUE.toString()).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false))));
    }finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void filterDetailByGdnSkuTest() throws Exception {
    when(this.productService
        .findDetailByGdnSku(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_SKU))
        .thenReturn(productLevel3MasterWebResponse);

    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_SKU_DETAIL, Constants.BUSINESS_PARTNER_CODE + GDN_SKU)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService)
        .findDetailByGdnSku(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_SKU);
  }

  @Test
  public void unsynchronizeTest() throws Exception {
    doNothing().when(productService)
        .unsynchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder = put(ProductApiPath.BASE_PATH + ProductApiPath.UNSYNCHRONIZE_PRODUCT,
        Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(GDN_PRODUCT_SKU)).content(toJson(ITEM_SKU));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService)
        .unsynchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void unsynchronize_ExceptionTest() throws Exception {
    doNothing().when(productService)
        .unsynchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UNSYNCHRONIZE_PRODUCT,
              Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU)
              .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(GDN_PRODUCT_SKU)).content(toJson(ITEM_SKU));

      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.REQUIRED_PARAMETER));
    } finally {
      verify(this.productService)
          .unsynchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
              Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void synchronizeTest() throws Exception {
    doNothing().when(productService)
        .synchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder = put(ProductApiPath.BASE_PATH + ProductApiPath.SYNCHRONIZE_PRODUCT,
        Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(GDN_PRODUCT_SKU)).content(toJson(ITEM_SKU));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService)
        .synchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void synchronize_ExceptionTest() throws Exception {
    doNothing().when(productService)
        .synchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
            Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    try {
      MockHttpServletRequestBuilder requestBuilder = put(ProductApiPath.BASE_PATH + ProductApiPath.SYNCHRONIZE_PRODUCT,
          Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU)
          .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
          .content(toJson(GDN_PRODUCT_SKU)).content(toJson(ITEM_SKU));

      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.REQUIRED_PARAMETER));
    } finally {
      verify(this.productService)
          .synchronizeProduct(Constants.BUSINESS_PARTNER_CODE, Constants.BUSINESS_PARTNER_CODE + GDN_PRODUCT_SKU,
              Constants.BUSINESS_PARTNER_CODE + ITEM_SKU);
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductsByMerchantAndCategoryCodeTest() throws Exception {
    when(productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE))
        .thenReturn(activeProductWebResponsePage);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(activeProductWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductsByMerchantAndCategoryCodeV2Test() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        Constants.BUSINESS_PARTNER_CODE);
    when(productService.getProductListByMerchantAndCategoryCodeV2(productL3ListingWebRequest,
        MERCHANT_CODE, PAGE, SIZE)).thenReturn(productL3ListingWebResponseForQRPage);
    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH
        + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODE_V2).contentType(
            MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
        .content(toJson(productL3ListingWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(productService).getProductListByMerchantAndCategoryCodeV2(productL3ListingWebRequest,
        MERCHANT_CODE, PAGE, SIZE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }


  @Test
  public void getSuspendedItemsTest() throws Exception {
    when(productService
        .getSuspendedItemListByMerchantAndCategoryCode(suspensionWebRequest, PageRequest.of(PAGE, SIZE_1)))
        .thenReturn(suspensionWebResponsePage);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.GET_SUSPENDED_ITEMS)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(suspensionWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService)
        .getSuspendedItemListByMerchantAndCategoryCode(suspensionWebRequest, PageRequest.of(PAGE, SIZE_1));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductCountsTest() throws Exception {
    when(productService.getProductCounts(PRODUCT_TYPE, BUSINESS_PARTNER_CODE))
        .thenReturn(new ProductCountWebResponse());
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_COUNTS).param(TYPE, PRODUCT_TYPE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(productService).getProductCounts(PRODUCT_TYPE, BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getInActiveProductsTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.findSummaryByFilter(inActiveProductWebRequest, PageRequest.of(PAGE, SIZE),
        new SortOrder(inActiveProductWebRequest.getSortBy(), inActiveProductWebRequest.getOrderBy()), TYPE))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductLevel3SummaryResponse())));
    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.GET_INACTIVE_PRODUCTS)
        .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE)).param(TYPE, TYPE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(inActiveProductWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).findSummaryByFilter(inActiveProductWebRequest, PageRequest.of(PAGE, SIZE),
        new SortOrder(inActiveProductWebRequest.getSortBy(), inActiveProductWebRequest.getOrderBy()), TYPE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getActiveProductsTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getActiveProductList(activeProductWebRequest, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductLevel3SummaryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ACTIVE_PRODUCTS).param(Constants.PAGE, String.valueOf(PAGE))
            .param(Constants.SIZE, String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(activeProductWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).getActiveProductList(activeProductWebRequest, PageRequest.of(PAGE, SIZE));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getInprocessProductsTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getInprocessProductList(inProcessProductWebRequest, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new InProcessWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_INPROCESS_PRODUCTS)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE)).param(TYPE, TYPE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(inProcessProductWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).getInprocessProductList(inProcessProductWebRequest, PageRequest.of(PAGE, SIZE));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }


  @Test
  public void getActiveProductsNameListTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(productService.getActiveProductNameList(SEARCH_KEY, BUSINESS_PARTNER_CODE, false, PageRequest.of(PAGE, SIZE), true))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ItemDetailWebResponse())));

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ACTIVE_NAME_SUGGESTIONS).param(SEARCH_KEY, SEARCH_KEY)
            .param("page", "0").param("size", "10").contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService)
        .getActiveProductNameList(SEARCH_KEY, BUSINESS_PARTNER_CODE, false, PageRequest.of(PAGE, SIZE), true);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getActiveProductsNameListTest_isProductName() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(productService
        .getActiveProductNameList(SEARCH_KEY, BUSINESS_PARTNER_CODE, true, PageRequest.of(PAGE, SIZE), false))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ItemDetailWebResponse())));

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ACTIVE_NAME_SUGGESTIONS).param(SEARCH_KEY, SEARCH_KEY)
            .param("page", "0").param("size", "10").param("isProductName", Boolean.TRUE.toString())
            .param("inStock", Boolean.FALSE.toString()).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService)
        .getActiveProductNameList(SEARCH_KEY, BUSINESS_PARTNER_CODE, true, PageRequest.of(PAGE, SIZE), false);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void toggleArchiveItemsTest() throws Exception {
    doNothing().when(productService)
        .toggleArchiveItems(itemSkus, Boolean.TRUE, Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.TOGGLE_ARCHIVE_ITEMS)
        .param(DOARCHIVE, String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(itemSkus));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).toggleArchiveItems(itemSkus, Boolean.TRUE, Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductUpdateAuditLogsTest() throws Exception {
    LogAuditTrailUpdatedProductResponse logAuditTrailUpdatedProductResponse = new LogAuditTrailUpdatedProductResponse();
    logAuditTrailUpdatedProductResponse.setCreatedDateLog("01/27/2021 18:25:41");
    when(productService.getProductUpdateLogs(Constants.BUSINESS_PARTNER_CODE + ITEM_SKU, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(logAuditTrailUpdatedProductResponse)));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_UPDATE_AUDIT_LOGS).param("page", "0")
            .param("size", "10").param(GDN_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService)
        .getProductUpdateLogs(Constants.BUSINESS_PARTNER_CODE + ITEM_SKU, PageRequest.of(PAGE, SIZE));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductUpdateAuditLogsExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_UPDATE_AUDIT_LOGS).param("page", "0")
              .param("size", "10").param(GDN_SKU, ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
              .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .sessionAttr(Constants.SESSION, getDefaultSession());
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.INVALID_GDN_SKU));
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateSummaryTest() throws Exception {
    when(productService
        .updateSummary(Constants.BUSINESS_PARTNER_CODE + ITEM_SKU, Constants.BUSINESS_PARTNER_CODE,
            productLevel3UpdateSummaryRequest)).thenReturn(new ProductLevel3SummaryResponse());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SUMMARY)
        .param(GDN_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU).contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateSummaryRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).updateSummary(eq(Constants.BUSINESS_PARTNER_CODE + ITEM_SKU),
        eq(Constants.BUSINESS_PARTNER_CODE), productLevel3UpdateSummaryRequestArgumentCaptor.capture());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Assertions.assertNotNull(productLevel3UpdateSummaryRequestArgumentCaptor.getValue());
    Assertions.assertEquals(1, productLevel3UpdateSummaryRequestArgumentCaptor.getValue().getPrices().size());
    Assertions.assertEquals(10000,
        productLevel3UpdateSummaryRequestArgumentCaptor.getValue().getPrices().get(0).getPrice().longValue());
    Assertions.assertEquals(5000,
        productLevel3UpdateSummaryRequestArgumentCaptor.getValue().getPrices().get(0).getSalePrice().longValue());
  }

  @Test
  public void updateSummaryTestWrongGdnSkuExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SUMMARY).param(GDN_SKU, ITEM_SKU)
              .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(productLevel3UpdateSummaryRequest));
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.INVALID_GDN_SKU));
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateSummaryTestNullPriceExceptionTest() throws Exception {
    productLevel3UpdateSummaryRequest.setPrices(null);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    try {
      MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SUMMARY)
          .param(GDN_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU).contentType(MediaType.APPLICATION_JSON_VALUE)
          .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateSummaryRequest));
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.REQUIRED_PARAMETER));
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateSummaryTestSalePriceExceptionTest() throws Exception {
    productLevel3UpdateSummaryRequest.getPrices().get(0).setSalePrice(11000.0);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    try {
      MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SUMMARY)
          .param(GDN_SKU, Constants.BUSINESS_PARTNER_CODE + ITEM_SKU).contentType(MediaType.APPLICATION_JSON_VALUE)
          .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateSummaryRequest));
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.SELLING_PRICE_MUST_BE_LOWER_THAN_REGULAR_PRICE));
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void bulkDeleteProductWipTest() throws Exception {
    when(productService
        .bulkDeleteProductWip(Constants.BUSINESS_PARTNER_CODE, bulkDeleteProductWipRequest))
        .thenReturn(new PostLiveProductCountResponse(5, new HashMap<>()));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IN_PROGRESS_PRODUCTS)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(bulkDeleteProductWipRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success",
        is(true)));
    verify(productService).bulkDeleteProductWip(eq(Constants.BUSINESS_PARTNER_CODE),
        bulkDeleteProductWipRequestArgumentCaptor.capture());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Assertions.assertEquals(1, bulkDeleteProductWipRequestArgumentCaptor.getValue().getProductLevel1Ids().size());
    Assertions.assertTrue(bulkDeleteProductWipRequestArgumentCaptor.getValue().getProductLevel1Ids().contains(PRODUCT_ID));
  }

  @Test
  public void bulkDeleteProductWipExceptionTest() throws Exception {
    bulkDeleteProductWipRequest.setProductLevel1Ids(new HashSet<>());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IN_PROGRESS_PRODUCTS)
              .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(bulkDeleteProductWipRequest));
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", is(false)));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.ERR_EMPTY_PRODUCT_LIST));
    }
  }

  @Test
  public void updateBulkSummaryTest() throws Exception {
    when(productService.updateBulkSummary(Constants.BUSINESS_PARTNER_CODE, productLevel3UpdateSummaryRequestMap))
        .thenReturn(productLevel3SummaryResponseMap);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_BULK_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productLevel3UpdateSummaryRequestMap));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).updateBulkSummary(eq(Constants.BUSINESS_PARTNER_CODE), mapArgumentCaptor.capture());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Assertions.assertEquals(1, mapArgumentCaptor.getValue().size());
    Assertions.assertTrue(mapArgumentCaptor.getValue().containsKey(GDN_SKU));
  }

  @Test
  public void updateBulkSummaryExceptionTest() throws Exception {
    try {
      MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_BULK_SUMMARY)
          .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
          .content(toJson(new HashMap() {
          }));
      mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", is(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.ERR_EMPTY_PRODUCT_LIST));
    }
  }

  @Test
  public void updateTemplateBulkUpdateTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    doNothing().when(bulkProcessService)
        .uploadBulkUpdate(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, true, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("isOnlyExternalUser", "true")).andExpect(status().isOk());

    verify(bulkProcessService)
        .uploadBulkUpdate(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, true, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void updateTemplateBulkUpdateExceptionTest() throws Exception {
    multipartFile = new MockMultipartFile("request", new byte[10]);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE)
              .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.EXCEL_FILE_TYPE_INVALID));
    }
  }

  @Test
  public void retryCreateWIPTest() throws Exception {
    doNothing().when(productService).retryCreate(productBusinessPartnerIds);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.RETRY_CREATE_WIP).contentType(MediaType.APPLICATION_JSON)
            .content(toJson(productBusinessPartnerIds)).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).retryCreate(productBusinessPartnerIds);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void updateTemplateBulkUploadWithXLSFileTest() throws Exception {
    multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME1, null,
        generateDummyExcelMultipartFile1().getBytes());
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
      when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
          Constants.BUSINESS_PARTNER_CODE);
      when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD).file(multipartFile)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());

      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void updateTemplateBulkUploadWithXLSXTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
      multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME1, null,
          generateDummyExcelMultipartFile1().getBytes());
      when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
          Constants.BUSINESS_PARTNER_CODE);
      when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD).file(multipartFile)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  void updateTemplateBulkUploadWithXLSXFileWithOldTemplateTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME1, null, generateDummyExcelMultipartFile1().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD)
              .file(multipartFile).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()));
    }
    finally {
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void updateTemplateBulkUploadWithXLSXFileWithUnAuthorisedTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME1, null,
      generateDummyExcelMultipartFile1().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Exception exception = null;
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON));
    }
    catch (Exception e){
      exception = e;
    } finally{
      verify(mandatoryParameterHelper).getUsername();
      Assertions.assertTrue(exception.getLocalizedMessage().contains(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE));
    }
  }

  @Test
  public void updateTemplateBulkUploadWithXLSMTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
    multipartFile =
        new MockMultipartFile("request", DUMMY_FILE_NAME3, null, generateDummyExcelMultipartFile3().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void updateTemplateBulkUploadWithZipFileTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
      multipartFile = new MockMultipartFile("request", FILE_NAME_2, null,
          generateDummyExcelMultipartFile().getBytes());
      when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
          Constants.BUSINESS_PARTNER_CODE);
      when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD).file(multipartFile)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());

      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void uploadTemplateBulkUploadProductWipTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
    multipartFile = new MockMultipartFile("request", FILE_NAME_3, null, generateDummyExcelMultipartFile1().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(bulkProcessService.upload(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME,
        Collections.singletonList(FILE_NAME_1))).thenReturn(FILE_NAME_1);

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD_ALL).param(
          "processType", Constants.BULK_UPSERT).contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(Arrays.asList(FILE_NAME_1)));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(bulkProcessService).uploadExcelFile(eq(Constants.BUSINESS_PARTNER_CODE), eq(Constants.USER_NAME),
        eq(Collections.singletonList(FILE_NAME_1)), eq(Constants.BULK_UPSERT));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadExternalTemplate() throws Exception {
    Credential.setAccessibilities(
        new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(bulkProcessService)
        .uploadExternalFiles(anyString(), anyString(), anyString(), anyMap(), anyString(),
            anyString());

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.EXTERNAL_BULK_UPLOAD_ALL).param(
                "zipFileName", "zip.zip").param("pickupPointCode", PICKUP_POINT_CODE)
            .param("bulkProcessCode", DEFAULT_BULK_PROCESS_CODE)
            .content(new ObjectMapper().writeValueAsString(externalFiles))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(bulkProcessService).uploadExternalFiles(eq(Constants.BUSINESS_PARTNER_CODE),
        eq(Constants.USER_NAME), eq("zip.zip"), eq(externalFiles), eq(PICKUP_POINT_CODE),
        eq(DEFAULT_BULK_PROCESS_CODE));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadTemplateBulkUploadProductWipEmptyProcessTypeTest() throws Exception {
    Credential.setAccessibilities(
      new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
    multipartFile = new MockMultipartFile("request", FILE_NAME_3, null,
      generateDummyExcelMultipartFile1().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
      Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(bulkProcessService.upload(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME,
      Collections.singletonList(FILE_NAME_1))).thenReturn(FILE_NAME_1);

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD_ALL).contentType(
          MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(Arrays.asList(FILE_NAME_1)));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(bulkProcessService).uploadExcelFile(eq(Constants.BUSINESS_PARTNER_CODE),
      eq(Constants.USER_NAME), eq(Collections.singletonList(FILE_NAME_1)),
      eq(null));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadTemplateBulkUploadProductWipExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD_ALL).contentType(
            MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
          .param("categoryCode", CATEGORY_CODE).param("processType", Constants.BULK_UPSERT)
          .content(toJson(new ArrayList<>()));
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void uploadTemplateBulkUploadProductWithInvalidTypeTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
      Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD_ALL).param(
            "processType", Constants.BLIBLI).contentType(MediaType.APPLICATION_JSON_VALUE)
          .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(Arrays.asList(FILE_NAME_1)));
      mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void deleteBulkTemplateXLSFileTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_TEMPLATE_BULK_DELETE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .param("fileName", FILE_NAME_1);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void deleteBulkTemplateZipFileTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_TEMPLATE_BULK_DELETE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .param("fileName", FILE_NAME_2);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void deleteAllTemplateBulkProductFileTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_ALL_TEMPLATE_BULK_DELETE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void downloadTemplateBulkUpdateTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadTemplateBulkUpdate(eq(Constants.USER_NAME), eq(Constants.BUSINESS_PARTNER_CODE),
            eq(true), eq(bulkRequest), Mockito.any(HttpServletResponse.class));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_TEMPLATE_BULK_UPDATE)
            .param("isOnlyExternalUser", String.valueOf(Boolean.TRUE)).content(toJson(bulkRequest))
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService)
        .downloadTemplateBulkUpdate(eq(Constants.USER_NAME), eq(Constants.BUSINESS_PARTNER_CODE),
            eq(true), eq(bulkRequest), Mockito.any(HttpServletResponse.class));
  }

  @Test
  public void downloadAllBulkProductsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadAllProduct(eq(Constants.USER_NAME), eq(Boolean.TRUE),
            eq(Constants.BUSINESS_PARTNER_CODE), Mockito.any(ProductSummaryWebRequest.class));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS)
            .param("productSize", String.valueOf(PRODUCT_SIZE))
            .param("isOnlyExternalUser", String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productSummaryWebRequest)).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService).
        downloadAllProduct(eq(Constants.USER_NAME), eq(Boolean.TRUE),
            eq(Constants.BUSINESS_PARTNER_CODE), productSummaryWebRequestArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE,
        productSummaryWebRequestArgumentCaptor.getValue().getCategoryCodes().get(0));
  }

  @Test
  public void downloadAllBulkProductsEANTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadAllProduct(eq(Constants.USER_NAME), eq(Boolean.TRUE),
            eq(Constants.BUSINESS_PARTNER_CODE), Mockito.any(ProductSummaryWebRequest.class));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS_EAN)
            .param("productSize", String.valueOf(PRODUCT_SIZE))
            .param("isOnlyExternalUser", String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productSummaryWebRequest)).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService).
        downloadAllProductWithEAN(eq(Constants.USER_NAME), eq(Boolean.TRUE),
        eq(Constants.BUSINESS_PARTNER_CODE), productSummaryWebRequestArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE,
        productSummaryWebRequestArgumentCaptor.getValue().getCategoryCodes().get(0));
  }

  @Test
  public void downloadAllBulkBasicInfoProductsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadAllProductBasicInfo(eq(Constants.USER_NAME), eq(Constants.BUSINESS_PARTNER_CODE),
            Mockito.any(ProductSummaryWebRequest.class));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS_BASIC_INFO).contentType(
                MediaType.APPLICATION_JSON_VALUE).content(toJson(productSummaryWebRequest))
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService).downloadAllProductBasicInfo(eq(Constants.USER_NAME),
        eq(Constants.BUSINESS_PARTNER_CODE), productSummaryWebRequestArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE, productSummaryWebRequestArgumentCaptor.getValue().getCategoryCodes().get(0));
  }

  @Test
  public void getStockInfoWebSiteTest() throws Exception {
    when(productService.getStockInfoWebSite(MERCHANT_CODE, ITEM_SKU))
        .thenReturn(new ProductLevel3StockInfoWebSiteResponse());
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.STOCK_INFO_WEBSITE).param(WEB_MERCHANT_CODE, MERCHANT_CODE)
            .param(WEB_ITEM_SKU, ITEM_SKU).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).getStockInfoWebSite(MERCHANT_CODE, ITEM_SKU);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void downloadProductTemplateTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn("FALSE");
    doNothing().when(productService)
        .downloadProductTemplate(eq(Constants.USER_NAME), eq(Constants.BUSINESS_PARTNER_CODE),
            eq(CATEGORY_ID), Mockito.any(HttpServletResponse.class), eq(false));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_PRODUCT_TEMPLATE).param("categoryId", CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());

    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService)
        .downloadProductTemplate(eq(Constants.USER_NAME), eq(Constants.BUSINESS_PARTNER_CODE),
            eq(CATEGORY_ID), Mockito.any(HttpServletResponse.class), eq(false));
  }

  @Test
  public void checkSyncStockModeAndProductPermissionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(productService.checkSyncStockModeAndProductPermission(eq(Constants.BUSINESS_PARTNER_CODE)))
        .thenReturn(true);

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_SYNC_STOCK_MODE_AND_PRODUCT_PERMISSION)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).checkSyncStockModeAndProductPermission(eq(Constants.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getCategoriesByCategoryCodesTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(categoryService.getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(new CategoryWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_CATEGORIES_BY_CATEGORY_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(Collections.singletonList(CATEGORY_CODE)))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(categoryService).getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
  }

  @Test
  public void updateTest() throws Exception {
    productLevel3WebRequest.setDescription(DESCRIPTION);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    doNothing().when(productService)
        .updateProductDetails(eq(productLevel3WebRequest), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY));

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService).updateProductDetails(productLevel3WebRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE), eq(Constants.IS_EXTERNAL_ONLY));
  }

  @Test
  public void updateExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
              .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
              .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
              .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
      mockMvc.perform(requestBuilder);
    } catch (Exception e) {
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void bulkArchiveItemSkusTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    doNothing().when(bulkProcessService)
        .uploadBulkUpdateForBulkArchive(USER_NAME, MERCHANT_CODE, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.BULK_ARCHIVE_PRODUCTS_EXTERNAL)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    verify(bulkProcessService).uploadBulkUpdateForBulkArchive(USER_NAME, MERCHANT_CODE, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void bulkArchiveItemSkusExceptionTest() throws Exception {
    multipartFile = new MockMultipartFile("request", new byte[10]);
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK});
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.BULK_ARCHIVE_PRODUCTS_EXTERNAL)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadTemplateForBulkUpdateInStoreFlagTest() throws Exception{
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    doNothing().when(bulkProcessService)
        .uploadBulkUpdateForInStoreUpdate(USER_NAME, MERCHANT_CODE, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_IN_STORE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    verify(bulkProcessService).uploadBulkUpdateForInStoreUpdate(USER_NAME, MERCHANT_CODE, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadTemplateForBulkUpdateInStoreFlag_emptyRequestTest() throws Exception{
    multipartFile = new MockMultipartFile("request", new byte[0]);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    this.mockMvc.perform(MockMvcRequestBuilders
        .multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_IN_STORE)
        .file(multipartFile).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void uploadTemplateForBulkUpdateInStoreFlagExceptionTest() throws Exception{
    try {
      multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
      Mockito.doThrow(Exception.class)
          .when(bulkProcessService).uploadBulkUpdateForInStoreUpdate(USER_NAME, MERCHANT_CODE, multipartFile);
      when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
      when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_IN_STORE)
          .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(jsonPath("$.success", is(false)));
    }finally {
      verify(bulkProcessService).uploadBulkUpdateForInStoreUpdate(USER_NAME, MERCHANT_CODE, multipartFile);
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getUsername();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductItemByUPCCodeAndCategoryIdsTest() throws Exception {
    when(productService.getProductItemsByUPCCodeAndCategoryIds(UPC_CODE, categoryIds, PAGE, SIZE, Boolean.TRUE))
        .thenReturn(productItemDetailWebResponses);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(String.valueOf(Boolean.TRUE));
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(Boolean.TRUE));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BY_UPC_CODE_AND_CATEGORY_IDS).param(UPCCODE, UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryIds))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(productService).getProductItemsByUPCCodeAndCategoryIds(UPC_CODE, categoryIds, PAGE, SIZE, Boolean.TRUE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void updateImageTest() throws Exception {
    doNothing().when(this.productService)
        .updateProductImage(Mockito.any(UpdateImageRequest.class), Mockito.anyBoolean());
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Boolean.TRUE.toString());
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.IMAGE_UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(updateImageRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(this.productService).updateProductImage(updateImageRequestArgumentCaptor.capture(), eq(true));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternalOnly();
    Assertions.assertEquals(PRODUCT_CODE, updateImageRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(GDN_PRODUCT_SKU, updateImageRequestArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(ITEM_SKU, updateImageRequestArgumentCaptor.getValue().getItemSku());
    Assertions.assertEquals(LOCATION_PATH,
        updateImageRequestArgumentCaptor.getValue().getMasterDataItemImages().get(0).getLocationPath());
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile
        multipartFile = new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  private MultipartFile generateDummyExcelMultipartFile2() throws Exception {
    File file = generateDummyExcelFile2();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel-2", DUMMY_FILE_NAME2, null, fileData);
    return multipartFile;
  }


  private File generateDummyExcelFile2() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME2).getFile());
    return file;
  }

  private MultipartFile generateDummyExcelMultipartFile4() throws Exception {
    File file = generateDummyExcelFile2();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel-4", DUMMY_FILE_NAME4, null, fileData);
    return multipartFile;
  }


  private File generateDummyExcelFile4() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME4).getFile());
    return file;
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(productLevel3WipService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(estimatedPriceProperties);
    verifyNoMoreInteractions(systemParameterProperties, businessPartnerService);
    verifyNoMoreInteractions(bulkProcessService);
  }

  @Test
  public void fetchBulkPromoNotesTest() throws Exception {
    when(bulkProcessService.fetchPromoUpdatedProductNotes(DEFAULT_BULK_PROCESS_CODE)).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.FETCH_PROMO_BULK_NOTES, DEFAULT_BULK_PROCESS_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(bulkProcessService).fetchPromoUpdatedProductNotes(DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void fetchWholeSaleConfigCountTest() throws Exception {
    when(bulkProcessService.fetchWholeSaleConfigCount(DEFAULT_BULK_PROCESS_CODE)).thenReturn(new WholesaleCountWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.FETCH_WHOLESALE_CONFIG_COUNT, DEFAULT_BULK_PROCESS_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(bulkProcessService).fetchWholeSaleConfigCount(DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void checkPendingBulkProcessByMerchantCode() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(bulkProcessService.checkPendingBulkProcess(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE))
        .thenReturn(new BulkPendingRequestsWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE)
            .param("bulkProcessType", BULK_PROCESS_TYPE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(bulkProcessService).checkPendingBulkProcess(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE);
  }

  @Test
  public void checkYouTubeUrlStatusTest() throws Exception {
    when(this.productService.validateYouTubeUrl(YOUTUBE_URL)).thenReturn(youTubeAPIWebResponse);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_YOUTUBE_URL_STATUS).param("youTubeUrl", YOUTUBE_URL)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.productService).validateYouTubeUrl(YOUTUBE_URL);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void checkYouTubeUrlStatusExceptionTest() throws Exception {
    Mockito.doThrow(new Exception()).when(this.productService).validateYouTubeUrl(YOUTUBE_URL);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          get(ProductApiPath.BASE_PATH + ProductApiPath.GET_YOUTUBE_URL_STATUS).param("youTubeUrl",
                  YOUTUBE_URL).contentType(MediaType.APPLICATION_JSON)
              .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .sessionAttr(Constants.SESSION, getDefaultSession());
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(status().isOk())
              .andExpect(jsonPath("$.success", is(false))));

    } finally {
      verify(this.productService).validateYouTubeUrl(YOUTUBE_URL);
    }
  }

  @Test
  public void getMinimumPriceTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getMinimumPrice()).thenReturn(1);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_MINIMUM_PRICE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getMinimumPrice();
  }

  private MultipartFile generateDummyExcelMultipartFile1() throws Exception {
    File file = generateDummyExcelFile1();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel-1", DUMMY_FILE_NAME1, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile1() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME1).getFile());
    return file;
  }

  private MultipartFile generateDummyExcelMultipartFile3() throws Exception {
    File file = generateDummyExcelFile3();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel-3", DUMMY_FILE_NAME3, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile3() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME3).getFile());
    return file;
  }

  @Test
  public void getBrandPredefinedAllowedAttributeValueDetailTest() throws Exception {
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_BRAND_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_DETAIL,
            BRAND_CODE, BRAND_STATUS).param("value", VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getBrandPredefinedAllowedAttributeValueDetail(BRAND_CODE, BRAND_STATUS);
  }

  @Test
  public void checkPriceThresholdTest() throws Exception {
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(null);
    when(productService.getPriceChangeCompatibility(STORE_ID, REQUEST_ID, new ArrayList<>()))
        .thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRICE_CHANGE_COMPATIBILITY)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(priceChangeCompatibleRequests));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).getPriceChangeCompatibility(STORE_ID, null, priceChangeCompatibleRequests);
  }

  @Test
  public void getWholesalePromoStatusTest() throws Exception {
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(productService.getWholesalePromoStatus(STORE_ID, REQUEST_ID, new ArrayList<>())).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_WHOLESALE_PROMO_STATUS)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(new ArrayList<>()));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).getWholesalePromoStatus(anyString(), anyString(), Mockito.anyList());
  }

  @Test
  public void updatePriceAndStockTest() throws Exception {
    ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest = new ProductPriceAndStockUpdateWebRequest();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESS_PARTNER_CODE))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRICE_AND_STOCK).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productPriceAndStockUpdateWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void downloadUnifiedProductTemplateTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(bulkProcessService.downloadProductUnifiedTemplate(eq(Constants.BUSINESS_PARTNER_CODE)))
        .thenReturn(unifiedBulkDownloadWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_UNIFIED_PRODUCT_TEMPLATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(bulkProcessService).downloadProductUnifiedTemplate(eq(Constants.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void downloadUnifiedProductTemplateExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(bulkProcessService.downloadProductUnifiedTemplate(eq(Constants.BUSINESS_PARTNER_CODE)))
        .thenThrow(new Exception());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_UNIFIED_PRODUCT_TEMPLATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(bulkProcessService).downloadProductUnifiedTemplate(eq(Constants.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getProductScoreRules() throws Exception {
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setCategoryCode(CATEGORY_CODE);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap = new HashMap<>();
    MaxScoreAndRuleConfigResponse productScoreRuleDto = new MaxScoreAndRuleConfigResponse();
    productScoreRuleDto.setMaxScore(20);
    productScoreRuleDtoMap.put(PRODUCT_TITLE, productScoreRuleDto);
    productScoreRuleResponse.setProductScoreRules(productScoreRuleDtoMap);
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getProductScoreRule(CATEGORY_CODE))
        .thenReturn(ResponseHelper.toProductScoreRuleWebResponse(productScoreRuleResponse));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_SCORE_RULES).param("categoryCode", CATEGORY_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getProductScoreRule(CATEGORY_CODE);
  }

  @Test
  public void getSystemParamterSwitchesTest() throws Exception {
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getSystemParameterSwitches()).thenReturn(new ProductSystemParameterSwitchWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_SYSTEM_PARAMETER_SWITCH).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getSystemParameterSwitches();
  }

  @Test
  public void getUpcCodeAndImagesTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(productService.getUpcCodeAndImages(Constants.STORE_ID, Constants.REQUEST_ID, PRODUCT_CODE))
        .thenReturn(new UpcCodeAndImagesWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_UPC_CODE_IMAGES).param("productSku", PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).getUpcCodeAndImages(Constants.STORE_ID, Constants.REQUEST_ID, PRODUCT_CODE);
  }

  @Test
  public void getPickupPointsByProductSkuCodeTest() throws Exception {
    PickupPointCodeWebResponse pickupPointCodeWebResponse = new PickupPointCodeWebResponse();
    pickupPointCodeWebResponse.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointCodeWebResponse.setItemSku(ITEM_SKU);
    pickupPointCodeWebResponse.setItemName(ITEM_NAME);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getPickupPointCodesByProductSku(0, 1, GDN_PRODUCT_SKU, Boolean.FALSE,
      Constants.BUSINESS_PARTNER_CODE, Boolean.FALSE)).thenReturn(
      new PageImpl<>(Collections.singletonList(pickupPointCodeWebResponse), PageRequest.of(0, 1),
        1));
    MockHttpServletRequestBuilder requestBuilder =
      get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PICKUP_POINT_CODES).param("page", "0")
        .param("size", "1").param("productSku", GDN_PRODUCT_SKU).param("needCorrection", "false")
        .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getPickupPointCodesByProductSku(0, 1, GDN_PRODUCT_SKU, false,
      Constants.BUSINESS_PARTNER_CODE, false);
  }

  @Test
  public void getUniquePickupPointsByProductSkuCodeTest() throws Exception {
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getUniquePickupPointCodesByProductSku(GDN_PRODUCT_SKU))
        .thenReturn(new UniquePickupPointCodeWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_UNIQUE_PICKUP_POINT_CODES)
            .param("productSku", GDN_PRODUCT_SKU).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getUniquePickupPointCodesByProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  public void updateForUSPWithSpecialCharactersTest() throws Exception {
    productLevel3WebRequest.setUniqueSellingPoint(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES);
    productLevel3WebRequest.setDescription(DESCRIPTION);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    doNothing().when(productService)
        .updateProductDetails(eq(productLevel3WebRequest), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY));

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService)
        .updateProductDetails(productLevel3WebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY));
  }

  @Test
  void updateForUSPWithMoreThan400CharactersTest() throws Exception {
    productLevel3WebRequest.setDescription(DESCRIPTION);
    productLevel3WebRequest.setUniqueSellingPoint(USP_WITH_MORE_THAN_400_CHARACTERS);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
              .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
              .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
              .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void isNameEditableTest() throws Exception {
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.checkSuccessfullOrderPlacedForProductSku(GDN_PRODUCT_SKU))
        .thenReturn(new OrderPlacedWebResponse(GDN_PRODUCT_SKU, true));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.IS_NAME_EDITABLE, GDN_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).checkSuccessfullOrderPlacedForProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  void updateForDescriptionWithMoreThan5000CharactersTest() throws Exception {
    productLevel3WebRequest.setDescription(RandomStringUtils.random(5001));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
              .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
              .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
              .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void editProductInfoTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productService.editProductInfo(eq(productEditInfoWebRequest), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean()))
        .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productEditInfoWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService)
        .editProductInfo(productEditInfoWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void editProductInfoWithErrorCodeTest() throws Exception {
    editProductWebResponse.setApiErrorCode(ERROR_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productService.editProductInfo(eq(productEditInfoWebRequest), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean()))
        .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productEditInfoWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService)
        .editProductInfo(productEditInfoWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void editLogisticsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    doNothing().when(productService)
        .updateLogistics(eq(productLevel3UpdateWebRequest), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY), eq(false));

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_LOGISTICS).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateWebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService).updateLogistics(productLevel3UpdateWebRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE), eq(Constants.IS_EXTERNAL_ONLY), eq(false));
  }

  @Test
  public void editLogisticsWithPreOrderTest() throws Exception {
    productLevel3UpdateWebRequest.setPreOrder(preOrderWebRequest);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    doNothing().when(productService)
        .updateLogistics(eq(productLevel3UpdateWebRequest), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY), eq(false));

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_LOGISTICS).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateWebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService)
        .updateLogistics(productLevel3UpdateWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE),
            eq(Constants.IS_EXTERNAL_ONLY), eq(false));
    assertEquals(PREORDER_TYPE, productLevel3UpdateWebRequestArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE,
        productLevel3UpdateWebRequestArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
  }

  @Test
  public void editLogisticsExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON_VALUE)
              .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3UpdateWebRequest))
              .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
              .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
      mockMvc.perform(requestBuilder);
    } catch (Exception e) {
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductLevel3VariantsTest() throws Exception {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    productLevel3VariantsWebRequest.setProductSku(GDN_PRODUCT_SKU);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getProductLevel3VariantList(productLevel3VariantsWebRequest, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductLevel3SummaryDetailsWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_VARIANTS).param(Constants.PAGE, String.valueOf(PAGE))
            .param(Constants.SIZE, String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3VariantsWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).getProductLevel3VariantList(productLevel3VariantsWebRequest, PageRequest.of(PAGE, SIZE));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductLevel3VariantsNeedRevisionTest() throws Exception {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    productLevel3VariantsWebRequest.setProductSku(GDN_PRODUCT_SKU);
    productLevel3VariantsWebRequest.setNeedCorrection(true);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getProductLevel3VariantList(productLevel3VariantsWebRequest, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductLevel3SummaryDetailsWebResponse())));
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_VARIANTS).param(Constants.PAGE, String.valueOf(PAGE))
              .param(Constants.SIZE, String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON_VALUE)
              .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3VariantsWebRequest));
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    } catch (Exception e) {
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getPinpointStatusByL3Test() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(productService.getPinpointStatusByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE))
        .thenReturn(StringUtils.EMPTY);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PINPOINT_STATUS_BY_L3, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService)
        .getPinpointStatusByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void editItemsPriceStockImagesTest() throws Exception {
    ItemsPriceStockImagesUpdateWebResponse response = new ItemsPriceStockImagesUpdateWebResponse();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(productService.updateItemsPriceStockImages(anyString(), Mockito.any(UpdateItemsPriceStockImagesWebRequest.class)))
        .thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_PRICE_STOCK_INFO).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(productLevel3WebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).updateItemsPriceStockImages(anyString(), Mockito.any(
        UpdateItemsPriceStockImagesWebRequest.class));
  }

  @Test
  public void getVariantEditHistorySummaryTest() throws Exception {
    HistorySummaryWebRequest historySummaryWebRequest = new HistorySummaryWebRequest(BUSINESS_PARTNER_CODE + PRODUCT_SKU, "", "",  null, null, false);
    String request = new ObjectMapper().writeValueAsString(historySummaryWebRequest);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(productService.getProductEditHistorySummary(historySummaryWebRequest, 0, 1))
        .thenReturn(new PageImpl(new ArrayList()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_EDIT_HISTORY_SUMMARY).param("page", "0")
            .param("size", "1").content(request).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getProductEditHistorySummary(historySummaryWebRequest, 0, 1);
  }

  @Test
  void getVariantEditHistorySummaryExceptionTest() throws Exception {
    HistorySummaryWebRequest historySummaryWebRequest = new HistorySummaryWebRequest(GDN_PRODUCT_SKU, "", "",  null, null, false);
    String request = new ObjectMapper().writeValueAsString(historySummaryWebRequest);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    try {
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_EDIT_HISTORY_SUMMARY).param("page", "0")
            .param("size", "1").content(request).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON);
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", is(false))));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getOrderStatusTest() throws Exception {
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getOrderStatusByProductCode(PRODUCT_CODE)).thenReturn(new OrderStatusWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ORDER_STATUS, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getOrderStatusByProductCode(PRODUCT_CODE);
  }

  @Test
  public void updatePickupPointsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(
        productService.updatePickupPoints(eq(pickupPointUpdateWebRequest), eq(BUSINESS_PARTNER_CODE)))
        .thenReturn(new PickupPointUpdateWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PICKUP_POINTS).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(pickupPointUpdateWebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService)
        .updatePickupPoints(eq(pickupPointUpdateWebRequest), eq(BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getProductVariantsNameByProductSkuTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getProductVariantsNameByProductSku(GDN_PRODUCT_SKU, 0, 1))
        .thenReturn(new PageImpl(new ArrayList()));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_VARIANTS_NAME).param("page", "0").param("size", "1")
            .param("productSku", GDN_PRODUCT_SKU).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getProductVariantsNameByProductSku(GDN_PRODUCT_SKU, 0, 1);
  }

  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productService.getL3DetailByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE, false))
        .thenReturn(new ProductLevel3DetailWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_L3_DETAIL, PRODUCT_SKU)
            .param("isNeedCorrection", "false")
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getL3DetailByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE, false);
  }

  @Test
  public void getInventorySummaryTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productService.getInventorySummary(ITEM_SKU, true, BUSINESS_PARTNER_CODE))
        .thenReturn(new InventorySummaryWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_INVENTORY_SUMMARY, ITEM_SKU)
            .param("isWareHouse", String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getInventorySummary(ITEM_SKU, true, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getL3ProductCountsTest() throws Exception {
    when(productService.getL3ProductCounts(PRODUCT_TYPE, BUSINESS_PARTNER_CODE))
        .thenReturn(new ProductL3CountWebResponse());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_L3_PRODUCT_COUNTS).param(TYPE, PRODUCT_TYPE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(productService).getL3ProductCounts(PRODUCT_TYPE, BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void updateItemListingTest() throws Exception {
    QuickEditWebRequest quickEditWebRequest = new QuickEditWebRequest();
    ListingUpdateWebRequest listingUpdateWebRequest = new ListingUpdateWebRequest(Arrays.asList(quickEditWebRequest));
when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    doNothing().when(productService).updateItemListing(PRODUCT_SKU, listingUpdateWebRequest.getQuickEditRequests());
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_ITEM_LISTING, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(listingUpdateWebRequest)).param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).updateItemListing(PRODUCT_SKU, listingUpdateWebRequest.getQuickEditRequests());
  }

  @Test
  public void toggleArchiveProductsTest() throws Exception {
    doNothing().when(productService)
        .toggleArchiveProducts(itemSkus, Boolean.TRUE, Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.TOGGLE_ARCHIVE_PRODUCTS)
            .param(DOARCHIVE, String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(itemSkus));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).toggleArchiveProducts(itemSkus, Boolean.TRUE, Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getFilterSummaryL3Test() throws Exception {
    String request = new ObjectMapper().writeValueAsString(new ProductSummaryWebRequest());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.getProductL3List(Mockito.any(ProductSummaryWebRequest.class), eq(0), eq(1)))
        .thenReturn(new PageImpl(new ArrayList()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_L3_PRODUCT_LIST).param("page", "0").param("size", "1")
            .content(request).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getProductL3List(Mockito.any(ProductSummaryWebRequest.class), eq(0), eq(1));
  }
  @Test
  public void bulkArchiveProductSkusTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    doNothing().when(bulkProcessService)
        .uploadBulkUpdateForBulkArchiveProductSkus(USER_NAME, MERCHANT_CODE, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_ARCHIVE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(bulkProcessService).uploadBulkUpdateForBulkArchiveProductSkus(USER_NAME, MERCHANT_CODE, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void bulkArchiveProductSkus_emptyRequestTest() throws Exception {
    multipartFile = new MockMultipartFile("request", new byte[0]);
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_ARCHIVE)
          .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", is(false)));
    } catch (Exception e) {
    } finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadTemplateBulkUpdateByProductSku(eq(Constants.BUSINESS_PARTNER_CODE),
            eq(true), eq(Arrays.asList(PRODUCT_SKU)),
            Mockito.any(HttpServletResponse.class), eq(0), eq(50));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_TEMPLATE_BULK_UPDATE_BY_PRODUCT_SKU)
            .param("isOnlyExternalUser", String.valueOf(Boolean.TRUE))
            .content(toJson(bulkProductSkuRequest)).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService)
        .downloadTemplateBulkUpdateByProductSku(eq(Constants.BUSINESS_PARTNER_CODE),
            eq(true), eq(Arrays.asList(PRODUCT_SKU)),
            Mockito.any(HttpServletResponse.class), eq(0), eq(50));
  }

  @Test
  public void downloadTemplateMultiPickuppointTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productService)
        .downloadTemplateForMultiPickupPointTemplate(eq(Constants.BUSINESS_PARTNER_CODE),
            Mockito.any(HttpServletResponse.class));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_TEMPLATE_MULTIPICKUPPOINT).contentType(
            MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(productService).downloadTemplateForMultiPickupPointTemplate(eq(Constants.BUSINESS_PARTNER_CODE),
        Mockito.any(HttpServletResponse.class));
  }
  @Test
  public void getItemsByProductSkuTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productService.getItemsByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductLevel3SummaryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEMS_BY_PRODUCT_SKU, PRODUCT_SKU)
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getItemsByProductSku(PRODUCT_SKU, BUSINESS_PARTNER_CODE, PageRequest.of(PAGE, SIZE));
  }

  @Test
  public void getVendorNotesTest() throws Exception {
    when(productService.getVendorNotes(PRODUCT_CODE)).thenReturn(new VendorNotesResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_VENDOR_NOTES, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).getVendorNotes(PRODUCT_CODE);
  }

  @Test
  public void updateVendorNotesTest() throws Exception {
    VendorNotesRequest vendorNotesRequest = new VendorNotesRequest();
    vendorNotesRequest.setContentAdditionalNotes(DESCRIPTION);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_VENDOR_NOTES, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(vendorNotesRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).updateVendorNotes(PRODUCT_CODE, vendorNotesRequest);
  }

  @Test
  public void submitNeedRevisionProductTest() throws Exception {
    NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest = new NeedRevisionSubmitWebRequest();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.submitNeedForRevisionProduct(needRevisionSubmitWebRequest))
        .thenReturn(new EditProductWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.NEED_REVISION_SUBMIT).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(needRevisionSubmitWebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).submitNeedForRevisionProduct(needRevisionSubmitWebRequest);
  }

  @Test
  public void submitNeedRevisionProductApiErrorTest() throws Exception {
    NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest = new NeedRevisionSubmitWebRequest();
    EditProductWebResponse editProductWebResponse = new EditProductWebResponse();
    editProductWebResponse.setApiErrorCode(ERROR_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.submitNeedForRevisionProduct(needRevisionSubmitWebRequest)).thenReturn(editProductWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.NEED_REVISION_SUBMIT).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(toJson(needRevisionSubmitWebRequest))
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productService).submitNeedForRevisionProduct(needRevisionSubmitWebRequest);
  }

  @Test
  public void appealProductFromInProgressTest_success() throws Exception {
    AppealProductWebRequest appealProductWebRequest = AppealProductWebRequest.builder()
        .notes(NOTES)
        .productCode(PRODUCT_CODE)
        .productSku(PRODUCT_SKU).build();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(productService.appealProductsInProgress(appealProductWebRequest,
        BUSINESS_PARTNER_CODE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.APPEAL_PRODUCT).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(appealProductWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).appealProductsInProgress(appealProductWebRequest, BUSINESS_PARTNER_CODE);
  }

  @Test
  @Disabled
  public void appealProductFromInProgressTest_emptyFields() throws Exception {
    AppealProductWebRequest appealProductWebRequest = new AppealProductWebRequest();
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.APPEAL_PRODUCT).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(appealProductWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().is4xxClientError());
    verify(productService).appealProductsInProgress(appealProductWebRequest, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateSubjectToVatWithXLSX() throws Exception {
    multipartFile =
        new MockMultipartFile("request", DUMMY_FILE_NAME4, null, generateDummyExcelMultipartFile4().getBytes());
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    doNothing().when(bulkProcessService)
        .uploadBulkSubjectToVatSkus(eq(Constants.BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.any());
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_SUBJECT_TO_VAT)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(bulkProcessService)
        .uploadBulkSubjectToVatSkus(eq(Constants.BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.any());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  void updateSubjectToVatEmptyRequest() throws Exception {
   when(systemParameterProperties.getDirectoryVatFilepath()).thenReturn(Constants.DATA_BASE_DIR);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(
                    ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_SUBJECT_TO_VAT).file(
                    new MockMultipartFile("request", DUMMY_FILE_NAME, null,
                        StringUtils.EMPTY.getBytes()))
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()));
    doNothing().when(bulkProcessService)
        .uploadBulkSubjectToVatSkus(eq(BUSINESS_PARTNER_CODE), anyString(), anyString());
  }

  @Test
  void updateSubjectToVatWithXLSTest() throws Exception {
    multipartFile =
        new MockMultipartFile("request", DUMMY_FILE_NAME, null, generateDummyExcelMultipartFile1().getBytes());
    Mockito.doThrow(Exception.class)
      .when(fileStorageService).uploadSubjectToVatFile(Mockito.any(), anyString());
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(
                      ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_SUBJECT_TO_VAT).file(multipartFile)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk()));
    } finally {
      verify(fileStorageService).uploadSubjectToVatFile(Mockito.any(), anyString());
    }
  }

  @Test
  public void updateProductInfoTest() throws Exception {
    when(productService.updateProductInfo(Mockito.any(UpdateProductLevel3InfoRequest.class),
        eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean(), anyString())).thenReturn(editProductWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_INFO, BUSINESS_PARTNER_CODE).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService).updateProductInfo(updateProductLevel3InfoRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean(), anyString());
  }

  @Test
  public void updateProductInfoWithErrorCodeTest() throws Exception {
    editProductWebResponse.setApiErrorCode(ERROR_CODE);
    when(productService.updateProductInfo(Mockito.any(),
        eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean(), Mockito.any())).thenReturn(editProductWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_INFO, BUSINESS_PARTNER_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productService).updateProductInfo(updateProductLevel3InfoRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void updateProductInfoMerchantUpdatingOtherProductSkuTest() throws Exception {
    when(productService.updateProductInfo(Mockito.any(), eq(BUSINESS_PARTNER_CODE),
        Mockito.anyBoolean(), Mockito.any())).thenReturn(editProductWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("ABC-70000");
    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_INFO, PRODUCT_SKU).accept(
                  MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(UpdateProductLevel3InfoRequest.builder().description(StringUtils.EMPTY).build())).param("storeId", Constants.STORE_ID)
              .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
              .param("requestId", Constants.REQUEST_ID);
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(status().isOk()));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }


  @Test
  public void updateProductInfoDiscription5000CheckTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest = UpdateProductLevel3InfoRequest.builder().build();
    updateProductLevel3InfoRequest.setDescription(Base64.getEncoder().encodeToString(RandomStringUtils.random(5001).getBytes()));
    when(productService.updateProductInfo(Mockito.any(UpdateProductLevel3InfoRequest.class), eq(BUSINESS_PARTNER_CODE),
        Mockito.anyBoolean(), anyString())).thenReturn(editProductWebResponse);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_INFO, BUSINESS_PARTNER_CODE).accept(
                  MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(updateProductLevel3InfoRequest)).param("storeId", Constants.STORE_ID)
              .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
              .param("requestId", Constants.REQUEST_ID);
      mockMvc.perform(requestBuilder).andExpect(status().is4xxClientError());
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS));
    } finally {
     verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateProductInfoUSP5000CheckTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest = UpdateProductLevel3InfoRequest.builder().build();
    updateProductLevel3InfoRequest.setUniqueSellingPoint(
        Base64.getEncoder().encodeToString(RandomStringUtils.random(5001).getBytes()));
    updateProductLevel3InfoRequest.setDescription(DESCRIPTION);
    when(productService.updateProductInfo(Mockito.any(UpdateProductLevel3InfoRequest.class), eq(BUSINESS_PARTNER_CODE),
        Mockito.anyBoolean(), anyString())).thenReturn(editProductWebResponse);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_INFO, BUSINESS_PARTNER_CODE).accept(
                  MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(updateProductLevel3InfoRequest)).param("storeId", Constants.STORE_ID)
              .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
              .param("requestId", Constants.REQUEST_ID);
      mockMvc.perform(requestBuilder).andExpect(status().isOk());
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS));
      throw e;
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).isExternalOnly();
      verify(mandatoryParameterHelper).getRequestId();
      verify(productService).updateProductInfo(updateProductLevel3InfoRequestArgumentCaptor.capture(),
          eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean(), anyString());
    }
  }

  @Test
  public void updateImagesTest() throws Exception {
    when(productService.updateImages(anyString(), eq(DEFAULT_PRODUCT_SKU),
        Mockito.any())).thenReturn(new ItemsPriceStockImagesUpdateWebResponse());
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("ABC-70000");

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_IMAGES, DEFAULT_PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).updateImages(eq(DEFAULT_PRODUCT_SKU), anyString(),
        Mockito.any());
  }

  @Test
  void updateImagesWrongProductSkuPatternTest() throws Exception {
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_IMAGES, BUSINESS_PARTNER_CODE).accept(
                  MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
              .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
              .param("requestId", Constants.REQUEST_ID);
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  void updateImagesWrongMerchantProductSkuTest() throws Exception {
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_IMAGES, "ABC-70001-11111").accept(
                  MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
              .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
              .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
              .param("requestId", Constants.REQUEST_ID);
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getL4ItemListByProductSkuTest(){
    Set<String> productSkus = new HashSet<>();
    productSkus.add(DEFAULT_PRODUCT_SKU + "1");
    productSkus.add(DEFAULT_PRODUCT_SKU + "2");
    ItemLevel4WebRequest request =
      new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebResponse response1 =
      ItemLevel4ListingWebResponse.builder().productSku(DEFAULT_PRODUCT_SKU + "1").build();
    ItemLevel4ListingWebResponse response2 =
      ItemLevel4ListingWebResponse.builder().productSku(DEFAULT_PRODUCT_SKU + "2").build();
    List<ItemLevel4ListingWebResponse> responses= Arrays.asList(response1,response2);
    Page<ItemLevel4ListingWebResponse> responsePage = new PageImpl<>(responses, pageable, responses.size());
    when(productService.getL4ItemListByProductSku(STORE_ID, Constants.REQUEST_ID, PAGE, SIZE, request))
        .thenReturn(responsePage);
    try {
      MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
          .content(toJson(request)).param("storeId", Constants.STORE_ID)
          .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));
      mockMvc.perform(requestBuilder).andExpect(status().isOk());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getStoreId();
      verify(productService).getL4ItemListByProductSku(STORE_ID, Constants.REQUEST_ID, PAGE, SIZE, request);
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }

  }

  @Test
  public void getL4ItemListByProductSkuNullTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(DEFAULT_PRODUCT_SKU + "1");
    productSkus.add(DEFAULT_PRODUCT_SKU + "2");
    ItemLevel4WebRequest request = null;
    Pageable pageable = PageRequest.of(0, 10);
    when(productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, request))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ItemLevel4ListingWebResponse())));
    try {
      MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
          .content(toJson(request)).param("storeId", Constants.STORE_ID)
          .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));
      mockMvc.perform(requestBuilder);
    } catch (Exception e) {
      throw new RuntimeException(e);
    } finally {
      assertTrue(Objects.isNull(request));
    }
  }


  @Test
  public void getL3ProductDetailsByProductSkuTest() throws Exception {
    when(productL3Service
      .getL3DetailsByProductSku(STORE_ID, false,
        BUSINESS_PARTNER_CODE, false, PRODUCT_SKU, true))
      .thenReturn(new ProductL3DetailWebResponse());
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
      get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_DETAILS, PRODUCT_SKU)
        .param("isNeedCorrection", "false").accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productL3Service)
      .getL3DetailsByProductSku(STORE_ID, false,
        BUSINESS_PARTNER_CODE, false, PRODUCT_SKU, true);
  }

  @Test
  public void getProductUpdateHistoryTest() throws Exception {
    when(productService.getProductUpdateHistory(historyUpdateWebRequest, PAGE, SIZE)).thenReturn(
      new PageImpl<>(Collections.emptyList(), pageable, SIZE));
    historyUpdateWebRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(DEFAULT_BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
      post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_UPDATE_HISTORY).accept(
          MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(UpdateProductLevel3InfoRequest.builder().build())).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID).content(toJson(historyUpdateWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getProductUpdateHistory(historyUpdateWebRequest, PAGE, SIZE);
  }

  @Test
  public void getProductItemPickupPointsByProductSku() throws Exception {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest =
        ItemPickupPointListingL3WebRequest.builder().build();

    when(productService.getItemPickupPointListingByProductSku(eq(PAGE), eq(SIZE), eq(true),
        eq(PRODUCT_SKU), Mockito.any(ItemPickupPointListingL3WebRequest.class), eq(true))).thenReturn(
        new PageImpl<>(new ArrayList<>(), pageable, SIZE));

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_ITEM_PICKUP_POINT, PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(itemPickupPointListingL3WebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productService).getItemPickupPointListingByProductSku(eq(PAGE), eq(SIZE), eq(true),
        eq(PRODUCT_SKU), Mockito.any(ItemPickupPointListingL3WebRequest.class), eq(true));
  }

 @Test
 public void getPickupDetailByCodesTest() throws Exception {
   List<String> pickupPointCodes = Arrays.asList(PICKUP_POINT_CODE);
   when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(MERCHANT_CODE);
   when(productService.getPickupDetailByCode(pickupPointCodes, MERCHANT_CODE)).thenReturn(Collections.emptyList());
   MockHttpServletRequestBuilder requestBuilder =
     post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PICKUP_DETAIL_BY_CODE).accept(
         MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
       .content(toJson(pickupPointCodes)).param("storeId", Constants.STORE_ID)
       .param("requestId", Constants.REQUEST_ID);
   mockMvc.perform(requestBuilder).andExpect(status().isOk());
   verify(mandatoryParameterHelper).getRequestId();
   verify(mandatoryParameterHelper).getBusinessPartnerCode();
   verify(productService).getPickupDetailByCode(pickupPointCodes, MERCHANT_CODE);
 }

  @Test
  public void deleteBulkOfflineItemTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    multipartFile = new MockMultipartFile("file", generateDummyExcelMultipartFile().getBytes());
    doNothing().when(bulkProcessService)
      .uploadBulkDeleteOfflineItems(Constants.REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
        Constants.USER_NAME, Constants.CLIENT_ID, multipartFile);
    this.mockMvc.perform(MockMvcRequestBuilders
      .multipart(ProductApiPath.BASE_PATH + ProductApiPath.OFFLINE_ITEM_DELETE).file(multipartFile)
      .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk());
    verify(bulkProcessService)
      .uploadBulkDeleteOfflineItems(Constants.REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
        Constants.USER_NAME, Constants.CLIENT_ID, multipartFile);
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getClientId();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  void deleteBulkOfflineItem_ExceptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    multipartFile = new MockMultipartFile("file", new byte[0]);
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(
                      ProductApiPath.BASE_PATH + ProductApiPath.OFFLINE_ITEM_DELETE).file(multipartFile)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk()));
    }
    finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getClientId();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  void editL5PriceStockExceptionTest() throws Exception {
    ProductVariantUpdateWebRequest productVariantUpdateWebRequest =
        ProductVariantUpdateWebRequest.builder().productSku(PRODUCT_SKU).build();

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_L5_PRICE_STOCK).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productVariantUpdateWebRequest));
    try {
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(status().isOk()));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void editL5PriceStockTest() throws Exception {
    ProductVariantUpdateWebRequest productVariantUpdateWebRequest =
        ProductVariantUpdateWebRequest.builder().productSku(Constants.BUSINESS_PARTNER_CODE).build();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productService.editL5ProductStockAndPrice(productVariantUpdateWebRequest, Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(new ItemsPriceStockImagesUpdateWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.EDIT_L5_PRICE_STOCK).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productVariantUpdateWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(productService).editL5ProductStockAndPrice(productVariantUpdateWebRequest, Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void updateTemplateBulkUploadWithXLSFileGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(true);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
      multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME1, null,
          generateDummyExcelMultipartFile1().getBytes());
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD).file(multipartFile)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void getExternalDownloadTemplateFilePathsTest() throws Exception {
    when(productService.getExternalDownloadTemplateFilePaths()).thenReturn(new TemplateDownloadFilePathWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ALL_EXTERNAL_TEMPLATE_DOWNLOAD_PATHS).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getExternalDownloadTemplateFilePaths();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getItemBasicDetailsByProductSkuTest() throws Exception {
    when(productService.getItemBasicDetails(PRODUCT_SKU, true)).thenReturn(
        Collections.singletonList(new ItemDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("productSku", PRODUCT_SKU);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemBasicDetails(PRODUCT_SKU, true);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getItemBasicDetailsByProductSkuTest_fetchAllDetailsFalse() throws Exception {
    when(productService.getItemBasicDetails(PRODUCT_SKU, false)).thenReturn(
        Collections.singletonList(new ItemDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("productSku", PRODUCT_SKU)
            .param("fetchAllDetails", Boolean.FALSE.toString());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemBasicDetails(PRODUCT_SKU, false);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getBulkItemDetailByItemSkusTest() throws Exception {
    when(productService.getItemBasicDetails(Collections.singletonList(ITEM_SKU), true)).thenReturn(
        Collections.singletonList(new ItemDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("fetchBundleRecipe", Boolean.TRUE.toString()).content(toJson(Collections.singletonList(ITEM_SKU)));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemBasicDetails(Collections.singletonList(ITEM_SKU), true);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getBulkItemDetailByItemSkus_fetchBundleRecipeFalse() throws Exception {
    when(productService.getItemBasicDetails(Collections.singletonList(ITEM_SKU), false)).thenReturn(
        Collections.singletonList(new ItemDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("fetchBundleRecipe", Boolean.FALSE.toString()).content(toJson(Collections.singletonList(ITEM_SKU)));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemBasicDetails(Collections.singletonList(ITEM_SKU), false);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getItemL5DetailsByProductSkuTest() throws Exception {
    when(productService.getItemL5Details(PRODUCT_SKU, PAGE, SIZE, false, false)).thenReturn(
        new PageImpl<>(new ArrayList<>(), pageable, SIZE));
    MockHttpServletRequestBuilder requestBuilder = get(ProductApiPath.BASE_PATH
        + ProductApiPath.GET_ITEM_L5_DETAILS_BY_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("productSku", PRODUCT_SKU)
        .param("cncActivated", String.valueOf(false));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemL5Details(PRODUCT_SKU, PAGE, SIZE, false, false);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getItemL5DetailsByProductSku_PageSizeNullTest() throws Exception {
    when(productService.getItemL5Details(PRODUCT_SKU, null, null, false, false)).thenReturn(
        new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder = get(ProductApiPath.BASE_PATH
        + ProductApiPath.GET_ITEM_L5_DETAILS_BY_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("page", "")
        .param("size", "").param("productSku", PRODUCT_SKU)
        .param("cncActivated", String.valueOf(false));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getItemL5Details(PRODUCT_SKU, null, null, false, false);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getWarehouseStockByItemSkusTest() throws Exception {
    when(productService.getWarehouseStockByItemSkusAndWarehouseCode(anyString(), anyList())).thenReturn(
        new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_WAREHOUSE_STOCK_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).content(toJson(new ArrayList<>()))
            .param("warehouseCode", "");
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getWarehouseStockByItemSkusAndWarehouseCode(anyString(), anyList());
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getWarehouseStockByItemSkus_ExceptionTest() throws Exception {
    when(productService.getWarehouseStockByItemSkusAndWarehouseCode(anyString(), anyList())).thenThrow(
        ClientException.class);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_WAREHOUSE_STOCK_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).content(toJson(new ArrayList<>()))
            .param("warehouseCode", "");
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(productService).getWarehouseStockByItemSkusAndWarehouseCode(anyString(), anyList());
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void bulkWorkorderCreationTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    doNothing().when(bulkProcessService)
        .uploadBulkForWorkOrderCreation(USER_NAME, Constants.ASSEMBLY_REQUEST, MERCHANT_CODE, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_WORKORDER,
                Constants.ASSEMBLY_REQUEST).file(multipartFile).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
    verify(bulkProcessService).uploadBulkForWorkOrderCreation(USER_NAME, Constants.ASSEMBLY_REQUEST, MERCHANT_CODE,
        multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  void bulkWorkorderCreationEmptyRequestTest() throws Exception {
    multipartFile = new MockMultipartFile("request", new byte[0]);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_WORKORDER,
                  Constants.ASSEMBLY_REQUEST).file(multipartFile).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()));
    }
    finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  void bulkWorkorderCreationEmptyTypeTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(
                  ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_WORKORDER, " ")
              .file(multipartFile).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()));
    } finally {
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void downloadTemplatesForWorkLoadTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    mockMvc.perform(MockMvcRequestBuilders.get(
            ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_WORK_ORDER_TEMPLATES_BASED_ON_TYPE, "ASSEMBLY_REQUEST"))
        .andExpect(status().isOk());
    verify(productService).downloadWorkOrderTemplates(Mockito.any(), Mockito.any());
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void fetchAppealProductConfigTest() throws Exception {
    AppealProductConfigResponse appealProductConfigResponse = new AppealProductConfigResponse();
    when(
        productService.fetchAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(appealProductConfigResponse);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_APPEAL_PRODUCT_ELIGIBILITY,
          BUSINESS_PARTNER_CODE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("requestId", REQUEST_ID)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(true)).andReturn();
    verify(productService)
      .fetchAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchAppealProductConfigExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productService)
      .fetchAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_APPEAL_PRODUCT_ELIGIBILITY,
          BUSINESS_PARTNER_CODE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("requestId", REQUEST_ID)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(false)).andReturn();
    verify(productService)
      .fetchAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void submitEvidence_successTest() throws Exception {
    doNothing().when(productService).submitEvidenceForIPR(
        ConverterUtil.convertToSubmitEvidenceIPRRequest(submitEvidenceIPRWebRequest));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.SUBMIT_EVIDENCE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(submitEvidenceIPRWebRequest)).param("requestId", "");
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).submitEvidenceForIPR(
        ConverterUtil.convertToSubmitEvidenceIPRRequest(submitEvidenceIPRWebRequest));
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void submitEvidence_successURLPresentTest() throws Exception {
    submitEvidenceIPRWebRequest.setEvidenceUrl(IMAGE_URL);
    doNothing().when(productService).submitEvidenceForIPR(
        ConverterUtil.convertToSubmitEvidenceIPRRequest(submitEvidenceIPRWebRequest));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.SUBMIT_EVIDENCE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(submitEvidenceIPRWebRequest)).param("requestId", "");
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).submitEvidenceForIPR(
        ConverterUtil.convertToSubmitEvidenceIPRRequest(submitEvidenceIPRWebRequest));
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void submitEvidence_successFalseTest() throws Exception {
    submitEvidenceIPRWebRequest.setEvidenceUrl(StringUtils.EMPTY);
    submitEvidenceIPRWebRequest.setEvidenceFilePath(StringUtils.EMPTY);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.SUBMIT_EVIDENCE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(submitEvidenceIPRWebRequest)).param("requestId", "");
    Assertions.assertThrows(Exception.class,
        () -> mockMvc.perform(requestBuilder).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(false))));
  }

  @Test
  void submitEvidence_successFalseNullTest() throws Exception {
    submitEvidenceIPRWebRequest.setEvidenceUrl(null);
    submitEvidenceIPRWebRequest.setEvidenceFilePath(null);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.SUBMIT_EVIDENCE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(submitEvidenceIPRWebRequest)).param("requestId", "");
    Assertions.assertThrows(Exception.class,
        () -> mockMvc.perform(requestBuilder).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(false))));
  }

  @Test
  public void getUpcStatus() throws Exception {
    UpcStatusWebRequest upcStatusRequest = new UpcStatusWebRequest();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productService.getUpcStatus(upcStatusRequest)).thenReturn(
        Collections.singletonList(new UpcStatusWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_UPC_CODE_STATUS).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(upcStatusRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getUpcStatus(upcStatusRequest);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void updateTemplateBulkUpdateMasterInfoTest() throws Exception {
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName("test.xlsx");
    request.setFilePath("test.xlsx");
    Mockito.doNothing().when(bulkProcessService)
        .uploadBulkUpdateMasterInfo(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, request);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_MASTER_INFO).contentType(
            MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(request));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(bulkProcessService).uploadBulkUpdateMasterInfo(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, request);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void updateTemplateBulkUpdateMasterInfoExceptionTest() throws Exception {
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName("");
    request.setFilePath("test.xlsx");
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_MASTER_INFO).contentType(
              MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(request));
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void updateTemplateBulkUpdateMasterInfoInvalidExcelTypeTest() throws Exception {
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName("test.xlsm");
    request.setFilePath("test.xlsm");
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_MASTER_INFO).contentType(
              MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(request));
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void getProductsListForReelsTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_SELLER_VIDEO});

      ReelProductListingWebRequest reelProductListingWebRequest =
          new ReelProductListingWebRequest();
      reelProductListingWebRequest.setMerchantCode(MERCHANT_CODE);
      reelProductListingWebRequest.setCategoryCodes(List.of(CATEGORY_CODE));
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(MERCHANT_CODE);
      when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
      when(productService.getProductListForReels(reelProductListingWebRequest, PAGE,
          SIZE)).thenReturn(new PageImpl<>(Collections.emptyList(), pageable, SIZE));
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCTS_LIST_FOR_REELS).contentType(
                  MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .content(toJson(reelProductListingWebRequest));
      mockMvc.perform(requestBuilder).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
      verify(productService).getProductListForReels(reelProductListingWebRequest, PAGE, SIZE);
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getRequestId();
    }
  }

  @Test
  public void getProductsListForReelsDifferentMerchantCodeTest() throws Exception {
    try {
      ReelProductListingWebRequest reelProductListingWebRequest =
          new ReelProductListingWebRequest();
      reelProductListingWebRequest.setMerchantCode(MERCHANT_CODE);
      reelProductListingWebRequest.setCategoryCodes(List.of(CATEGORY_CODE));
      when(mandatoryParameterHelper.getBusinessPartnerCode())
          .thenReturn(BUSINESS_PARTNER_CODE_DIFFERENT);
      when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCTS_LIST_FOR_REELS).contentType(
                  MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .content(toJson(reelProductListingWebRequest));
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getRequestId();
    }
  }

  @Test
  public void getProductsListForReelsDifferentMerchantTypeTest() throws Exception {
    try {
      ReelProductListingWebRequest reelProductListingWebRequest =
          new ReelProductListingWebRequest();
      reelProductListingWebRequest.setTradingProduct(true);
      reelProductListingWebRequest.setCategoryCodes(List.of(CATEGORY_CODE));
      when(mandatoryParameterHelper.getBusinessPartnerCode())
          .thenReturn(BUSINESS_PARTNER_CODE);
      when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
      MockHttpServletRequestBuilder requestBuilder =
          post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCTS_LIST_FOR_REELS).contentType(
                  MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .content(toJson(reelProductListingWebRequest));
      Assertions.assertThrows(Exception.class, () -> mockMvc.perform(requestBuilder));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(mandatoryParameterHelper).getRequestId();
    }
  }

  @Test
  public void getProductBasicDetailsByProductSkusTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(List.of(PRODUCT_SKU));
    MockHttpServletRequestBuilder requestBuilder = post(ProductApiPath.BASE_PATH
        + ProductApiPath.PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKUS).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
        .content(toJson(simpleListStringRequest));
    when(productService.getProductBasicDetailsByProductSkus(List.of(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(new ProductBasicWebResponse()));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getProductBasicDetailsByProductSkus(List.of(PRODUCT_SKU));
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void updateTemplateBulkEANUpdateTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    doNothing().when(bulkProcessService)
        .uploadBulkUpdateEAN(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, multipartFile);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_EAN_UPDATE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

    verify(bulkProcessService)
        .uploadBulkUpdateEAN(Constants.BUSINESS_PARTNER_CODE, Constants.USER_NAME, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void updateTemplateBulkEANUpdateEmptyFileTest() throws Exception {
    multipartFile = new MockMultipartFile("request", DUMMY_FILE_NAME, null, new byte[0]);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(
                      ProductApiPath.BASE_PATH + ProductApiPath.UPLOAD_TEMPLATE_BULK_EAN_UPDATE).file(multipartFile)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.errorCode", equalTo("400")));
    } catch (jakarta.servlet.ServletException e) {
      // Exception is expected when file is empty - validation fails before service call
      // The exception handler should catch this, but in test context it may be wrapped
      Assertions.assertTrue(e.getCause() instanceof com.gdn.common.exception.ApplicationRuntimeException ||
          e.getMessage().contains("Anda tidak dapat meng-upload file kosong"));
    }

    // Verify interactions that occur before validation fails
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    // getRequestId() is called after validation, so it should not be called when validation fails
    verify(mandatoryParameterHelper, never()).getRequestId();
    // Service method should not be called when validation fails
    verify(bulkProcessService, never())
        .uploadBulkUpdateEAN(anyString(), anyString(), any());
  }
}

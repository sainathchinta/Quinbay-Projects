package com.gdn.mta.product.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import jakarta.servlet.ServletException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ItemBulkArchiveRequest;
import com.gda.mta.product.dto.ItemImageEditRequest;
import com.gda.mta.product.dto.ItemSkuListRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductL3SummaryRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3DTO;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3LogisticsRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.SuspensionProductRequestList;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.mta.product.converter.ProductLevel3RequestConverter;
import com.gdn.mta.product.converter.ProductLevel3ResponseConverter;
import com.gdn.mta.product.entity.BulkDownloadProductLevel3Summary;
import com.gdn.mta.product.entity.PreOrderDTO;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductItemLevel3Order;
import com.gdn.mta.product.entity.ProductItemWholesalePriceVo;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Dashboard;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Order;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.entity.ProductLevel3UpdateSummary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.entity.ProductScore;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.entity.UsageStatus;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductLevel3V2Service;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.mta.product.service.ProductLevel3Wrapper;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.mta.product.service.exception.InvalidDataStateException;
import com.gdn.mta.product.service.exception.ProductInNeedCorrectionException;
import com.gdn.mta.product.service.exception.ProductInReviewException;
import com.gdn.mta.product.service.exception.ProductRejectedException;
import com.gdn.mta.product.valueobject.EstimateItemPriceDTO;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.mta.product.web.model.ProductLevel3ControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductLevel3ControllerPath;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.Thumbnail;
import com.google.api.services.youtube.model.ThumbnailDetails;
import com.google.api.services.youtube.model.Video;
import com.google.api.services.youtube.model.VideoListResponse;
import com.google.api.services.youtube.model.VideoLocalization;
import com.google.api.services.youtube.model.VideoSnippet;

public class ProductLevel3ControllerTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_GDN_SKU = "BLI-00001-00001-00001";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001";
  private static final String DEFAULT_ITEM_SKU2 = "BLI-00001-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "api";
  private static final String DEFAULT_USERNAME = "com.gdn.mta";
  private static final String DEFAULT_CLIENT_ID = "mta";
  private static final String DEFAULT_CATEGORY_ID = "categoryId";
  private static final String ITEM_SKU = "TOT-15014-0001-0001";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final Pageable pageable = PageRequest.of(0, 10);
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final ObjectMapper DEFAULT_OBJECT_MAPPER = new ObjectMapper(new JsonFactory());
  private static final String DEFAULT_PRODUCT_CODE = "CODE-1000";
  private static final Boolean DEFAULT_SYNCHRONIZE = false;
  private static final String DEFAULT_PRODUCT_NAME = "CODE-1000";
  private static final Integer DEFAULT_PRODUCT_TYPE = 1;
  private static final String DEFAULT_CATEGORY_CODE = "CODE-1000";
  private static final String DEFAULT_CATEGORY_NAME = "CODE-1000";
  private static final String DEFAULT_CATEGORY_HIERARCHY = "CODE-1000";
  private static final String DEFAULT_BRAND = "CODE-1000";
  private static final String DEFAULT_DESCRIPTION = "CODE-1000";
  private static final String DEFAULT_SPECIFICATION = "CODE-1000";
  private static final String DEFAULT_UNIQUE_SELLING = "CODE-1000";
  private static final String DEFAULT_PRODUCT_STORY = "CODE-1000";
  private static final String DEFAULT_ITEM_SKU = "CODE-1000";
  private static final String DEFAULT_SKU_CODE = "CODE-1000";
  private static final String DEFAULT_UPC_CODE = "CODE-1000";
  private static final String DEFAULT_ITEM_NAME = "CODE-1000";
  private static final Double DEFAULT_LENGTH = 10.0;
  private static final Double DEFAULT_WIDTH = 10.0;
  private static final Double DEFAULT_HEIGHT = 10.0;
  private static final Double DEFAULT_WEIGHT = 10.0;
  private static final Double DEFAULT_SHIPPING_WEIGHT = 10.0;
  private static final Integer DEFAULT_LEVEL = 1;
  private static final Boolean DEFAULT_FULFILLMENT = false;
  private static final String DEFAULT_PICKUP_POINT_NAME = "CODE-1000";
  private static final Integer DEFAULT_DELTA_STOCK = 10;
  private static final Integer DEFAULT_MINIMUM_STOCK = 10;
  private static final Boolean DEFAULT_SYNCHRONIZE_STOCK = false;
  private static final Double DEFAULT_PRICE = 1000000.0;
  private static final Double DEFAULT_SALE_PRICE = 900000.0;
  private static final Double DEFAULT_DISCOUNT = 10.0;
  private static final String DEFAULT_PROMOTION_NAME = "PROMO";
  private static final String DEFAULT_LOCATION_PATH = "Location";
  private static final Boolean DEFAULT_SKU_VALUE = true;
  private static final String DEFAULT_MERCHANT_SKU = "MSKU-0000001";
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String DEFAULT_OFF_2_ON_ACTIVE_FLAG = "true";
  private static final String DEFAULT_INTERNATIONAL_FLAG = "IS";
  private static final String DEFAULT_CLIENT_HOST = "localhost";
  private static final Boolean DO_ARCHIVE_TRUE = Boolean.TRUE;
  private static final Boolean DO_ARCHIVE_FALSE = Boolean.FALSE;
  private static final String BLANK = "";
  private static final String ITEM_CODE= "itemCode";
  private static final double LOWEST_PRICE_COEFFICIENT = 1.5d;
  private static final int MAX_INVENTORY_REQUEST = 1;
  private static final double NORMAL_PRICE = 10000d;
  private static final double OFFER_PRICE = 5000d;
  private static final Long BULK_ARCHIVE_SIZE = 10L;
  private static final String BULK_ARCHIVE_SIZE_PARAM = "10";
  private static final int MONTHS_TO_ARCHIVE_FOR = 6;
  private static final Boolean ARCHIVE_WITHOUT_SENDING_MAIL = Boolean.FALSE;
  private static final Boolean IS_SUSPENDED = Boolean.FALSE;
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String ORDER_BY = "createdDate";
  private static final String SORT_BY = "desc";
  private static final String BRAND = "brand";
  private static final String CATEGORY = "category";
  private static final Long VERSION = Long.valueOf(2);
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String API_KEY = "apiKey";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String DESCRIPTION = "description";
  private static final String URL = "url";
  private static final String TITLE = "title";
  private static final String ID = "id";
  private static final String USP_WITH_SPECIAL_CHARACTERS_AND_SPACES =
      "•   Case Premium Bagian Belakang    Dove Transparan , Kelebihan :~Bagian belakang dove.\n"
          + "~Ada crack / Anti jatuh.\n" + "~Bagian belakang transparan tampil elegant\n"
          + "~Anti Pecah / Sobek karena pakai bahan lentur.\n" + "~Penuh warna glosy di List sekelilingnya\n"
          + "~Slim dan pas di gengam di tangan\n" + "~Anti lecet karena menggunakan Kualitas bahan terbaik\n"
          + "~Lepas pasang sangat mudah , tidak keras / kasar.";
  private static final String LARGE_PRODUCT_NAME =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut sit amet placerat erat. "
          + "Pellentesque mi turpis, suscipit at pellentesque sit amet tincidunt.";
  private static final String PREORDER_DAYS_TYPE = "DAYS";
  private static final String PREORDER_WEEK_TYPE = "WEEK";
  private static final String PREORDER_DATE_TYPE = "DATE";
  private static final Integer PREORDER_VALUE = 10;
  private static final SimpleDateFormat PREORDER_DATE_FORMAT = new SimpleDateFormat("dd/MM/yyyy");
  private static final String MAXIMUM_WEEK_ERROR = "Can not process invalid input data :Maximum 13 weeks are allowed";
  private static final String ZERO_WEEK_ERROR = "Can not process invalid input data :Number of week should be more than 0";
  private static final String DATE_ERROR = "Can not process invalid input data :PreOrder date must be greater than available date";
  private static final String WRONG_PREORDER_TYPE = "Can not process invalid input data :PreOrder type must be DAYS, WEEK or DATE";
  private static final String PREORDER_DATE_EXCEEDED_LIMIT = "Can not process invalid input data :PreOrder date must be greater than available date";
  private static final String ZERO_DAYS_ERROR = "Can not process invalid input data :Number of days should be more than 0";
  private static final String IMAGE_PATH = "imagePath";
  private static final String REVIEW_TYPE = "pre-live";

  @InjectMocks
  private ProductLevel3Controller productLevel3Controller;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductLevel3Wrapper productLevel3Wrapper;

  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Mock
  private ProductLevel3RequestConverter productLevel3RequestConverter;

  @Mock
  private ProductLevel3ResponseConverter productLevel3ResponseConverter;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3V2Service productLevel3V2Service;

  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private YouTube.Videos.List list;
  private VideoListResponse videoListResponse;

  @Captor
  private ArgumentCaptor<ProductLevel3> productLevel3ArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3UpdateSummary> productLevel3UpdateSummaryArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3SummaryDetailsRequest> productLevel3SummaryDetailsRequestCaptor;

  @Captor ArgumentCaptor<ProductLevel3UpdateRequest> productLevel3UpdateRequestArgumentCaptor;

  private ProfileResponse profileResponse;
  private SuspensionProductRequest suspensionProductRequest;
  private ProductResponse productResponse;
  private ListRequestDTO<SuspensionProductRequest> suspensionProductRequestListRequestDTO;
  private ProductSystemParameter productSystemParameter;
  private PreOrderRequest preOrderRequest;
  private EditProductResponse editProductResponse;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();

  public ProductLevel3Controller getProductLevel3Controller() {
    return productLevel3Controller;
  }

  public void setProductLevel3Controller(ProductLevel3Controller productLevel3Controller) {
    this.productLevel3Controller = productLevel3Controller;
  }

  public ProductLevel3Service getProductLevel3Service() {
    return productLevel3Service;
  }

  public void setProductLevel3Service(ProductLevel3Service productLevel3Service) {
    this.productLevel3Service = productLevel3Service;
  }

  public MockMvc getMockMvc() {
    return mockMvc;
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public ProductLevel3RequestConverter getProductLevel3RequestConverter() {
    return productLevel3RequestConverter;
  }

  public void setProductLevel3RequestConverter(
      ProductLevel3RequestConverter productLevel3RequestConverter) {
    this.productLevel3RequestConverter = productLevel3RequestConverter;
  }

  private ProductLevel3 generateProductLevel3() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setItems(new ArrayList<ProductItemLevel3>());
    productLevel3.setAttributes(new ArrayList<ProductLevel3Attribute>());
    productLevel3.setImages(new ArrayList<ProductLevel3Image>());
    productLevel3.getItems().add(new ProductItemLevel3());
    productLevel3.getItems().get(0).setPrices(new ArrayList<ProductLevel3Price>());
    productLevel3.getItems().get(0).setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productLevel3.getItems().get(0).setImages(new ArrayList<ProductLevel3Image>());
    productLevel3.getItems().get(0).getPrices().add(new ProductLevel3Price());
    productLevel3.getItems().get(0).getViewConfigs().add(new ProductLevel3ViewConfig());
    productLevel3.getItems().get(0).getImages().add(new ProductLevel3Image());
    productLevel3.getAttributes().add(new ProductLevel3Attribute());
    productLevel3.getImages().add(new ProductLevel3Image());
    productLevel3.setCategoryId(DEFAULT_CATEGORY_ID);
    productLevel3.setProductScore(new ProductScore(10, 20, 10, 10 ,10, 10, 10, 10 ,10, 1, 80));
    return productLevel3;
  }

  private Page<ProductLevel3SummaryMinified> generateProductLevel3SummaryMinified() {
    List<ProductLevel3SummaryMinified> productDatasList =
        new ArrayList<ProductLevel3SummaryMinified>();
    ProductLevel3SummaryMinified productData = new ProductLevel3SummaryMinified();
    productData.setImages(new ArrayList<ProductLevel3Image>());
    productData.setPrices(new ArrayList<ProductLevel3Price>());
    productData.setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productData.getImages().add(new ProductLevel3Image(true, 1, "localtionPath"));
    productData.getPrices().add(new ProductLevel3Price(DEFAULT_CHANNEL_ID, 1000.0, 1000.0));
    productData.getViewConfigs().add(new ProductLevel3ViewConfig(DEFAULT_CHANNEL_ID, true, true));
    productDatasList.add(productData);
    return new PageImpl<ProductLevel3SummaryMinified>(productDatasList, pageable, 10);
  }

  private Page<ProductLevel3Summary> generateProductLevel3Summary() {
    List<ProductLevel3Summary> productLevel3Summary = getProductLevel3Summaries();
    return new PageImpl<ProductLevel3Summary>(productLevel3Summary, DEFAULT_PAGEABLE, 1);
  }

  private ProductLevel3UpdateSummaryRequest generateProductLevel3UpdateSummaryRequest() {
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    productLevel3UpdateSummaryRequest.setMerchantSku(DEFAULT_MERCHANT_SKU);
    productLevel3UpdateSummaryRequest.setProductType(1);
    productLevel3UpdateSummaryRequest.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    productLevel3UpdateSummaryRequest.setLateFulfillment(false);
    productLevel3UpdateSummaryRequest.setDeltaStock(0);
    productLevel3UpdateSummaryRequest.setSynchronizeStock(false);
    productLevel3UpdateSummaryRequest.setPrices(new ArrayList<ProductLevel3PriceRequest>());
    productLevel3UpdateSummaryRequest
        .setViewConfigs(new ArrayList<ProductLevel3ViewConfigRequest>());
    productLevel3UpdateSummaryRequest.getPrices().add(
        new ProductLevel3PriceRequest(ChannelName.DEFAULT.name(), 10000.0, 10000.0, null, null,
            null, null));
    productLevel3UpdateSummaryRequest.getViewConfigs().add(
        new ProductLevel3ViewConfigRequest(ChannelName.DEFAULT.name(), true, true));
    return productLevel3UpdateSummaryRequest;
  }

  @Test
  public void updateItemPriceNoItemPriceTest() throws Exception {
    PriceRequest priceRequest = new PriceRequest();

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_PRICE)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("itemSku", "").build();

    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(
          post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(priceRequest))).andExpect(
          jsonPath("$.success", equalTo(false)));
    });
  }

  @Test
  public void updateItemPriceTest() throws Exception {
    PriceRequest priceRequest = new PriceRequest();

    Mockito.when(productLevel3Service
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any())).thenReturn(null);

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_PRICE)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("itemSku", ProductLevel3ControllerTest.ITEM_SKU).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(priceRequest))).andExpect(
        jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any());
  }

  @Test
  public void updateItemViewConfigNoItemTest() throws Exception {
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_VIEW_CONFIG)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("itemSku", "").build();

    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(
          post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(itemViewConfigRequest))).andExpect(
          jsonPath("$.success", equalTo(true)));
    });
  }

  @Test
  public void updateItemViewConfigTest() throws Exception {
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();

    Mockito.doNothing().when(productLevel3Service)
        .updateItemViewConfig(Mockito.any(ItemViewConfigRequest.class), Mockito.any(), Mockito.any());

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_VIEW_CONFIG)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("itemSku", ProductLevel3ControllerTest.ITEM_SKU).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemViewConfigRequest))).andExpect(
        jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .updateItemViewConfig(Mockito.any(ItemViewConfigRequest.class), Mockito.any(), Mockito.any());
  }

  @Test
  public void updateItemOff2OnTest() throws Exception {
    Mockito.doNothing().when(productLevel3Service)
        .updateItemOff2On(Mockito.anyBoolean(), Mockito.any());

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_OFF_2_ON)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("itemSku", ProductLevel3ControllerTest.ITEM_SKU)
            .addParameter("off2OnActiveFlag",
                ProductLevel3ControllerTest.DEFAULT_OFF_2_ON_ACTIVE_FLAG).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .updateItemOff2On(Mockito.anyBoolean(), Mockito.any());
  }

  private ProfileResponse getProfileResponse(boolean isInternational) {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(isInternational);
    profileResponse.setCompany(companyDTO);
    return profileResponse;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productLevel3Controller, "maximumCharactersWithoutFormattingDescription", 5000);
    ReflectionTestUtils.setField(productLevel3Controller, "maximumCharactersInDescription", 6000);
    setMockMvc(MockMvcBuilders
        .standaloneSetup(getProductLevel3Controller())
        .setMessageConverters(new ByteArrayHttpMessageConverter(),
            new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
            new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build());
    ProductLevel3 productLevel3Data = generateProductLevel3();
    ProductItemLevel3Order productItemLevel3Order =
        new ProductItemLevel3Order(DEFAULT_SKU_CODE, DEFAULT_LENGTH, DEFAULT_WIDTH, DEFAULT_HEIGHT);
    List<ProductItemLevel3Order> listProductItemLevel3Order =
        new ArrayList<ProductItemLevel3Order>();
    listProductItemLevel3Order.add(productItemLevel3Order);
    ProductLevel3Order productLevel3OrderData =
        new ProductLevel3Order(DEFAULT_DESCRIPTION, DEFAULT_SPECIFICATION, DEFAULT_PRODUCT_STORY,
            listProductItemLevel3Order, false);
    Page<ProductLevel3Summary> productLevel3Summary = generateProductLevel3Summary();
    ProductLevel3Dashboard productLevel3DashboardData = new ProductLevel3Dashboard();
    Mockito.when(
        getProductLevel3Service().findDetailByGdnSku(Mockito.any(), Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(productLevel3Data);
    Mockito.when(
        getProductLevel3Service().findDetailByGdnSkuForAllItemSkus(Mockito.any(), Mockito.any()))
        .thenReturn(productLevel3Data);
    Mockito.when(
        getProductLevel3Service().findDetailOrderByGdnSku(
            Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU))).thenReturn(
        productLevel3OrderData);
    Mockito.doNothing().when(getProductLevel3Service())
        .updateItemStock(Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.eq(null));
    Mockito.when(
        getProductLevel3Service().updateSummary(Mockito.any(), Mockito.any(),
            Mockito.any(ProductLevel3UpdateSummary.class))).thenReturn(
        productLevel3Summary.getContent().get(0));
    Mockito.when(
        getProductLevel3Service().findSummaryByGdnSku(Mockito.any(), Mockito.any()))
        .thenReturn(productLevel3Summary.getContent().get(0));
    Mockito.when(getProductLevel3Service().findDashboard(Mockito.any())).thenReturn(
        productLevel3DashboardData);
    Mockito.when(
        getProductLevel3Service().findSummaryByCategoryAndBrandFilter(
            Mockito.any(BrandAndCategoryItemSummaryRequest.class), Mockito.any(PageRequest.class), Mockito.any()))
        .thenReturn(productLevel3Summary);
    Mockito.when(getProductLevel3Service().getMinimumPrice(DEFAULT_STORE_ID)).thenReturn(1);
    suspensionProductRequest = new SuspensionProductRequest();
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    productLevel3Request.setProductCode(DEFAULT_PRODUCT_CODE);
    productLevel3Request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productLevel3Request.setProductSku(DEFAULT_PRODUCT_SKU);
    suspensionProductRequest.setProducts(Arrays.asList(productLevel3Request));

    suspensionProductRequestListRequestDTO = new ListRequestDTO<>();
    suspensionProductRequestListRequestDTO.setList(Arrays.asList(suspensionProductRequest));

    productResponse = new ProductResponse();
    productResponse.setProductSku(DEFAULT_PRODUCT_SKU);

    productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("false");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(productSystemParameter);
    ReflectionTestUtils.setField(productLevel3Controller, "youTubeDataApiKey", API_KEY);

    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
    Video video = new Video();
    video.setId(ID);
    VideoSnippet videoSnippet = new VideoSnippet();
    videoSnippet.setCategoryId(ID);
    videoSnippet.setDescription(DESCRIPTION);
    ThumbnailDetails thumbnailDetails = new ThumbnailDetails();
    Thumbnail thumbnail = new Thumbnail();
    thumbnail.setUrl(URL);
    thumbnailDetails.setDefault(thumbnail);
    videoSnippet.setThumbnails(thumbnailDetails);
    VideoLocalization videoLocalization = new VideoLocalization();
    videoLocalization.setDescription(DESCRIPTION);
    videoLocalization.setTitle(TITLE);
    videoListResponse.setItems(Arrays.asList(video));
    videoSnippet.setLocalized(videoLocalization);
    videoListResponse.getItems().get(0).setSnippet(videoSnippet);

    preOrderRequest =
        PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_DAYS_TYPE).preOrderValue(PREORDER_VALUE)
            .build();
    editProductResponse = EditProductResponse.builder().productReview(true).reviewType(REVIEW_TYPE).build();

    ReflectionTestUtils.setField(productLevel3Controller, "preOrderMaximumDays", 90);
    ReflectionTestUtils.setField(productLevel3Controller, "preOrderMaximumWeek", 13);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel3Service);
    Mockito.verifyNoMoreInteractions(this.productLevel3AggregatorService);
    Mockito.verifyNoMoreInteractions(this.productLevel3Wrapper);
    Mockito.verifyNoMoreInteractions(this.productLevel3RequestConverter);
    Mockito.verifyNoMoreInteractions(this.productSystemParameterService);
  }

  @Test
  public void testFilterDetailByGdnSkuForActiveItems() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service()
        .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean()))
        .thenReturn(productLevel3);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(new NullPointerException());
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsItemNotFoundExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_NOT_FOUND.getCode()));
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsSuspendedExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_IS_SUSPENDED.getCode()));
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsInReviewExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(new ProductInReviewException(ApiErrorCode.ITEM_IS_IN_REVIEW.getCode(),
        ApiErrorCode.ITEM_IS_IN_REVIEW.getDesc()));
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsInRejectedExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(new ProductRejectedException(ApiErrorCode.ITEM_IS_IN_REVIEW.getCode(),
        ApiErrorCode.ITEM_IS_IN_REVIEW.getDesc()));
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void filterDetailByGdnSkuForActiveItemsInNeedCorrectionExceptionTest() throws Exception {
    ProductLevel3 productLevel3 = new ProductLevel3();
    List<ProductItemLevel3> productItemLevel3List = new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setLogistics(Arrays.asList(ProductLevel3Logistics.builder().build()));
    productItemLevel3.setPrices(new ArrayList<>());
    productItemLevel3.setViewConfigs(new ArrayList<>());
    productItemLevel3.setImages(new ArrayList<>());
    productItemLevel3.setProductItemWholesalePrices(new ArrayList<>());
    productItemLevel3List.add(productItemLevel3);
    productLevel3.setItems(productItemLevel3List);
    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.setImages(new ArrayList<>());
    productLevel3.setPreOrder(new PreOrderDTO());
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenThrow(
        new ProductInNeedCorrectionException(ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getCode(),
            ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getDesc()));
    try {
      URI uri = new URIBuilder().setPath(
              ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID)
          .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
          .build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(getProductLevel3Service())
          .findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void testFilterDetailByGdnSkuForActiveItemsWithwholesalePrice() throws Exception {
    ProductLevel3 productLevel3 = generateProductLevel3();
    productLevel3.getItems().get(0).setWholesalePromoActivated(true);
    productLevel3.getItems().get(0).setProductItemWholesalePrices(new ArrayList<>());
    productLevel3.getItems().get(0).getProductItemWholesalePrices().add(new ProductItemWholesalePriceVo(2, 10));
    Mockito.when(getProductLevel3Service().findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenReturn(productLevel3);
    ObjectMapper objectMapper = new ObjectMapper();
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    MvcResult mvcResult = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    verify(getProductLevel3Service()).findDetailByGdnSku(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    GdnRestSingleResponse<ProductLevel3Response> response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(),
        new TypeReference<GdnRestSingleResponse<ProductLevel3Response>>() {
        });

    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getValue().getItems().get(0).getWholesalePriceActivated());
    Assertions.assertTrue(response.getValue().getItems().get(0).isWholesalePromoActivated());
    Assertions.assertEquals(2,
        response.getValue().getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
  }

  @Test
  public void testFilterDetailByGdnSkuForActiveItemsWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").addParameter("gdnSku", DEFAULT_GDN_SKU)
            .build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailByGdnSku(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void testFilterDetailByGdnSkuForActiveItemsWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailByGdnSku(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void testFilterDetailByGdnSkuForAllItems() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_FOR_ALL_ITEM_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findDetailByGdnSkuForAllItemSkus(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
        Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void testFilterDetailByGdnSkuForAllItemsWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_FOR_ALL_ITEM_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").addParameter("gdnSku", DEFAULT_GDN_SKU)
            .build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailByGdnSkuForAllItemSkus(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU));
    }
  }

  @Test
  public void testFilterDetailByGdnSkuForAllItemsWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_FOR_ALL_ITEM_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailByGdnSku(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    }
  }

  @Test
  public void testIsPristineCategory() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.IS_PRISTINE_CATEGORY + "/"
            + DEFAULT_CATEGORY_ID).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).isPristineCategory(Mockito.eq(DEFAULT_CATEGORY_ID));
  }

  @Test
  public void testIsPristineCategoryExceptionTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.IS_PRISTINE_CATEGORY + "/"
            + StringUtils.SPACE).addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.CATEGORY_ID_MUST_NOT_BE_NULL));
      verify(getProductLevel3Service(), NEVER_CALLED).isPristineCategory(Mockito.any());
    }
  }

  @Test
  public void testFilterDetailOrderByGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ORDER_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findDetailOrderByGdnSku(
        Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void testFilterDetailOrderByGdnSkuWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ORDER_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").addParameter("gdnSku", DEFAULT_GDN_SKU)
            .build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailOrderByGdnSku(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU));
    }
  }

  @Test
  public void testFilterDetailOrderByGdnSkuWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DETAIL_ORDER_GDN_SKU)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDetailOrderByGdnSku(
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU));
    }
  }

  @Test
  public void testSynchronizeProduct() throws Exception {
    Mockito.when(
        getProductLevel3Service().synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2)))
        .thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testSynchronizeWithApiErrorCodeProduct() throws Exception {
    Mockito.when(
        getProductLevel3Service().synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2)))
        .thenReturn(ApiErrorCode.ITEM_IS_REJECTED);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testSynchronizeWithException() throws Exception {
    Mockito.doThrow(Exception.class).when(getProductLevel3Service())
        .synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .synchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testSynchronizeProductWithInvalidProductSku() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).addParameter("productSku", "")
        .addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void testUnsynchronizeProduct() throws Exception {
    Mockito.when(
        getProductLevel3Service().unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2)))
        .thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UNSYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testUnsynchronizeProductWithErrorCode() throws Exception {
    Mockito.when(
        getProductLevel3Service().unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2)))
        .thenReturn(ApiErrorCode.ITEM_IS_REJECTED);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UNSYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testUnsynchronizeProductWithException() throws Exception {
    Mockito.doThrow(Exception.class).when(getProductLevel3Service())
        .unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UNSYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .unsynchronizeProduct(Mockito.eq(DEFAULT_PRODUCT_SKU), Mockito.eq(DEFAULT_ITEM_SKU2));
  }

  @Test
  public void testUnsynchronizeProductWithInvalidProductSku() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UNSYNCHRONIZE_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productSku", "")
        .addParameter("itemSku", DEFAULT_ITEM_SKU2).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void testUpdateItemStock() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_STOCK)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    ProductLevel3StockRequest request = new ProductLevel3StockRequest();
    request.setGdnSku(DEFAULT_GDN_SKU);
    request.setDeltaStock(0);
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).updateItemStock(Mockito.any(),
        Mockito.any(), Mockito.anyInt(), Mockito.eq(null));
  }

  @Test
  public void testUpdateItemStockWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_STOCK)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    ProductLevel3StockRequest request = new ProductLevel3StockRequest();
    request.setDeltaStock(0);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateItemStock(Mockito.any(),
          Mockito.any(), Mockito.anyInt(), Mockito.eq(null));
    }
  }

  @Test
  public void testUpdateItemStockWithInvalidDeltaStock() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_STOCK)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    ProductLevel3StockRequest request = new ProductLevel3StockRequest();
    request.setGdnSku(DEFAULT_GDN_SKU);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.DELTA_STOCK_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateItemStock(Mockito.any(),
          Mockito.any(), Mockito.anyInt(), Mockito.eq(null));
    }
  }

  @Test
  public void testUpdateItemStockWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_STOCK)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("businessPartnerCode", "").build();
    ProductLevel3StockRequest request = new ProductLevel3StockRequest();
    request.setGdnSku(DEFAULT_GDN_SKU);
    request.setDeltaStock(0);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateItemStock(Mockito.any(),
          Mockito.any(), Mockito.anyInt(), Mockito.eq(null));
    }
  }

  private ProductLevel3Request generateProductLevel3Request() {
    ProductLevel3Request product =
        new ProductLevel3Request(DEFAULT_PRODUCT_SKU, DEFAULT_PRODUCT_CODE,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_SYNCHRONIZE, DEFAULT_PRODUCT_NAME,
            DEFAULT_PRODUCT_TYPE, DEFAULT_CATEGORY_CODE, DEFAULT_CATEGORY_NAME,
            DEFAULT_CATEGORY_HIERARCHY, DEFAULT_BRAND, DEFAULT_DESCRIPTION, DEFAULT_SPECIFICATION,
            DEFAULT_UNIQUE_SELLING, DEFAULT_PRODUCT_STORY,
            new ArrayList<ProductItemLevel3Request>(),
            new ArrayList<ProductLevel3AttributeRequest>(),
            new ArrayList<ProductLevel3ImageRequest>(), new ArrayList<>());
    ProductItemLevel3Request productItemLevel3Request =
        new ProductItemLevel3Request(DEFAULT_ITEM_SKU, DEFAULT_SKU_CODE, DEFAULT_MERCHANT_SKU,
            DEFAULT_UPC_CODE, DEFAULT_ITEM_NAME, DEFAULT_LENGTH, DEFAULT_WIDTH, DEFAULT_HEIGHT,
            DEFAULT_WEIGHT, DEFAULT_SHIPPING_WEIGHT, DEFAULT_LEVEL, DEFAULT_FULFILLMENT,
            DEFAULT_PICKUP_POINT_CODE, DEFAULT_PICKUP_POINT_NAME, DEFAULT_DELTA_STOCK,
            DEFAULT_MINIMUM_STOCK, DEFAULT_SYNCHRONIZE_STOCK,
            new ArrayList<ProductLevel3PriceRequest>(),
            new ArrayList<ProductLevel3ViewConfigRequest>(),
            new ArrayList<ProductLevel3ImageRequest>());
    ProductLevel3PriceRequest priceItem =
        new ProductLevel3PriceRequest(DEFAULT_CHANNEL_ID, DEFAULT_PRICE, DEFAULT_SALE_PRICE,
            DEFAULT_DISCOUNT, new Date(), new Date(), DEFAULT_PROMOTION_NAME);
    productItemLevel3Request.getPrices().add(priceItem);
    ProductLevel3ViewConfigRequest viewConfigItem =
        new ProductLevel3ViewConfigRequest(DEFAULT_CHANNEL_ID, true, true);
    productItemLevel3Request.getViewConfigs().add(viewConfigItem);
    ProductLevel3ImageRequest imageItem =
        new ProductLevel3ImageRequest(true, 1, DEFAULT_LOCATION_PATH);
    productItemLevel3Request.getImages().add(imageItem);
    ProductLevel3AttributeRequest attribute =
        new ProductLevel3AttributeRequest(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BRAND,
            new ArrayList<String>(), DEFAULT_SKU_VALUE);
    String value = "value";
    attribute.getValues().add(value);
    ProductLevel3ImageRequest image =
        new ProductLevel3ImageRequest(DEFAULT_FULFILLMENT, DEFAULT_DELTA_STOCK, DEFAULT_BRAND);
    product.getItems().add(productItemLevel3Request);
    product.getAttributes().add(attribute);
    product.getImages().add(image);
    product.setAccessChannel(UpdateProductAccessChannel.MTA_API_UPDATE_SINGLE.getDesc());
    return product;
  }

  @Test
  public void testUpdate() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductLevel3LogisticsRequest(Arrays.asList(new ProductLevel3LogisticsRequest()));
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateAndReturn() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
      verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateAndReturn_withApiErrorCode() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false))).thenReturn(
        ProductLevel3DTO.builder().productLevel3(generateProductLevel3())
            .apiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE.getCode())))
        .andExpect(jsonPath("$.errorMessage", equalTo(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE.getDesc())));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateAndReturnValidationTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
  }

  @Test
  public void testUpdateAndReturnValidationExceptionTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getPrices().get(0).setSalePrice(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    });
  }

  @Test
  public void testUpdateAndReturnValidationMinimumPriceExceptionTest() throws Exception {
    try {
      ProductLevel3Request product = generateProductLevel3Request();
      product.getItems().get(0).getPrices().get(0).setPrice(0.2);
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID)
              .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
              .addParameter("isOnlyExternal", String.valueOf(false)).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
                MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                    .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
            .andExpect(MockMvcResultMatchers.status().isOk());
      });
    } finally {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateAndReturnValidationMinimumSellingPriceExceptionTest() throws Exception {
    try {
      ProductLevel3Request product = generateProductLevel3Request();
      product.getItems().get(0).getPrices().get(0).setSalePrice(0.2);
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID)
              .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
              .addParameter("isOnlyExternal", String.valueOf(false)).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
                MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                    .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
            .andExpect(MockMvcResultMatchers.status().isOk());
      });
    } finally {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateAndReturnValidationSellingPriceExceptionTest() throws Exception {
    try {
      ProductLevel3Request product = generateProductLevel3Request();
      product.getItems().get(0).getPrices().get(0).setSalePrice(20.0);
      product.getItems().get(0).getPrices().get(0).setPrice(10.0);
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID)
              .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
              .addParameter("isOnlyExternal", String.valueOf(false)).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
                MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                    .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
            .andExpect(MockMvcResultMatchers.status().isOk());
      });
    } finally {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateAndReturnEmptyChannelIdExceptionTest() throws Exception {
    try {
      ProductLevel3Request product = generateProductLevel3Request();
      product.getItems().get(0).getPrices().get(0).setChannelId("");
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID)
              .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
              .addParameter("isOnlyExternal", String.valueOf(false)).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
                MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                    .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
            .andExpect(MockMvcResultMatchers.status().isOk());
      });
    } finally {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }


  @Test
  public void testUpdateAndReturnProductValidationTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setPrices(new ArrayList<>());
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
  }

  @Test
  public void testUpdateAndReturn_expectException() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE));
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateAndReturnWithValidUrlTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3Controller, "youtubeRegex",
        "(?<=watch\\?v=|/videos/|embed\\/|youtu"
            + ".be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F"
            + "|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*");
    ProductLevel3Request product = generateProductLevel3Request();
    productSystemParameter.setValue("true");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(productSystemParameter);
    product.setUrl(YOUTUBE_URL);
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(true),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).addParameter("hasOrder", String.valueOf(true)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(youTube, Mockito.times(2)).videos();
    verify(list).execute();
    verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(true),  Mockito.eq(false));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateAndReturnIS() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setBusinessPartnerCode(DEFAULT_INTERNATIONAL_FLAG);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
      verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateWithSynchronize() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setSynchronize(true);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
        MockMvcResultMatchers.status().isOk());
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateWithInvalidProductSku() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductSku("");
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    }
  }

  @Test
  public void testUpdateWithEmptyItem() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().clear();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    }
  }

  @Test
  public void testUpdateWithEmptyViewConfig() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getViewConfigs().clear();
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.VIEW_CONFIG_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void testUpdateWithInvalidBuyable() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getViewConfigs().get(0).setBuyable(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUYABLE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidDisplay() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getViewConfigs().get(0).setDisplay(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.DISPLAY_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidProductType() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductType(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidItemSku() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setItemSku("");
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidProductName() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductName(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithLargeProductName() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductName(LARGE_PRODUCT_NAME);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRODUCT_NAME_LTE_LIMIT));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidBrand() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setBrand(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidDescription() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithNullUniqueSellingPoint() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setUniqueSellingPoint(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri = new URIBuilder().setPath(
         ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
         .addParameter("storeId", DEFAULT_STORE_ID)
         .addParameter("channelId", DEFAULT_CHANNEL_ID)
         .addParameter("requestId", DEFAULT_REQUEST_ID)
         .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
         .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
         .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
      verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateWithInvalidLength() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setLength(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_LENGTH_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidWidth() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setWidth(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_WIDTH_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidHeight() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setHeight(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_HEIGHT_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidWeight() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setWeight(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_WEIGHT_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidPickupPointCode() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setPickupPointCode(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void testUpdateWithInvalidPickupPointName() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setPickupPointName(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PICKUP_POINT_NAME_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void testUpdateWithInvalidBusinessPartnerCode() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setBusinessPartnerCode(null);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidShippingWeight() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setShippingWeight(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidLateFulfillment() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setLateFulfillment(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.LATE_FULFILLMENT_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithEmptyImage() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getImages().clear();
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.IMAGE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithEmptyAttribute() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().clear();
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithEmptyItemImage() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getImages().clear();
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_IMAGE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidSpecificationDetail() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setSpecificationDetail(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidLocationPath() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getImages().get(0).setLocationPath(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.LOCATION_PATH_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidSequence() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getImages().get(0).setSequence(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SEQUENCE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidMainImage() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getImages().get(0).setMainImage(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.MAIN_IMAGE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidAttributeCode() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setAttributeCode(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidAttributeType() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setAttributeType(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidAttributeSkuValue() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setSkuValue(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidItemName() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setItemName(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ITEM_NAME_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidDangerousGoodsLevel() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).setDangerousGoodsLevel(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.DANGEROUS_GOODS_LEVEL_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED).update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidImageItemLocationPath() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getImages().get(0).setLocationPath(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.LOCATION_PATH_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidImageItemSequence() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getImages().get(0).setSequence(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SEQUENCE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED). update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidImageItemMainImage() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getImages().get(0).setMainImage(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.MAIN_IMAGE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED).update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithInvalidViewConfigChannelId() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getItems().get(0).getViewConfigs().get(0).setChannelId(null);
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.CHANNELID_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateWithEmptyAttributeValue() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).getValues().clear();
    URI uri =
        new URIBuilder()
            .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED)
          . update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).updateSummary(Mockito.any(),
        Mockito.any(), Mockito.any());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateSummaryRequestWholesaleFlagFalseTest() throws Exception {
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(false);
    Mockito.when(productLevel3Service.updateSummary(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
        productLevel3UpdateSummaryArgumentCaptor.capture())).thenReturn(productLevel3Summary);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.setWholesalePriceActivated(false);
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        jsonPath("$.errorCode", equalTo(null)));
    verify(getProductLevel3Service()).updateSummary(stringArgumentCaptor.getAllValues().get(0), stringArgumentCaptor.getAllValues().get(1), productLevel3UpdateSummaryArgumentCaptor.getValue());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }
  @Test
  public void updateSummaryRequestWholesaleFlagTrueResponseFlagFalseTest() throws Exception {
    ApiErrorCode apiErrorCode=ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE;
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(false);
    Mockito.when(productLevel3Service.updateSummary(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
        productLevel3UpdateSummaryArgumentCaptor.capture())).thenReturn(productLevel3Summary);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.setWholesalePriceActivated(true);
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        jsonPath("$.errorCode", equalTo(apiErrorCode.getCode())));
    verify(getProductLevel3Service()).updateSummary(stringArgumentCaptor.getAllValues().get(0), stringArgumentCaptor.getAllValues().get(1), productLevel3UpdateSummaryArgumentCaptor.getValue());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateSummaryRequestWholesaleFlagTrueResponseFlagTrueTest() throws Exception {
    ApiErrorCode apiErrorCode=ApiErrorCode.SAME_THRESHOLD_ERROR_CODE;
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(true);
    Mockito.when(productLevel3Service.updateSummary(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
        productLevel3UpdateSummaryArgumentCaptor.capture())).thenReturn(productLevel3Summary);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.setWholesalePriceActivated(true);
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        jsonPath("$.errorCode", equalTo(apiErrorCode.getCode())));
    verify(getProductLevel3Service()).updateSummary(stringArgumentCaptor.getAllValues().get(0), stringArgumentCaptor.getAllValues().get(1), productLevel3UpdateSummaryArgumentCaptor.getValue());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateSummaryTestWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void updateSummaryTestWithInvalidDataStateException() throws Exception {
    Mockito.when(getProductLevel3Service()
        .updateSummary(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU),
            Mockito.any(ProductLevel3UpdateSummary.class)))
        .thenThrow(InvalidDataStateException.class);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("gdnSku", DEFAULT_GDN_SKU).addParameter("clientHost", DEFAULT_CLIENT_HOST)
        .build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    try {
      getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains("Can't update archived product :Tidak berhasil update  product yang telah diarsipkan , request"));
    }
    finally {
      verify(getProductLevel3Service())
          .updateSummary(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU),
              Mockito.any(ProductLevel3UpdateSummary.class));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithApplicationRuntimeException() throws Exception {
    Mockito.when(getProductLevel3Service()
        .updateSummary(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU),
            Mockito.any(ProductLevel3UpdateSummary.class)))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("gdnSku", DEFAULT_GDN_SKU).addParameter("clientHost", DEFAULT_CLIENT_HOST)
        .build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    try {
      getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    }
    finally {
      verify(getProductLevel3Service())
          .updateSummary(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_GDN_SKU),
              Mockito.any(ProductLevel3UpdateSummary.class));
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", "").addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void updateSummaryTestWithInvalidPrice() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setPrice(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRICE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void updateSummaryTestWithInvalidMinimumPrice() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setPrice(0.0);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PRICE_MINIMUM_VALUE_INVALID));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidSalePrice() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setSalePrice(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void updateSummaryTestWithInvalidMinimumSalePrice() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setSalePrice(0.0);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MINIMUM_VALUE_INVALID));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidValueSalePrice() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setSalePrice(11000.0);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.SALE_PRICE_VALUE_INVALID));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }


  @Test
  public void updateSummaryTestWithInvalidPriceChannelId() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getPrices().get(0).setChannelId(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.CHANNEL_ID_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidDisplay() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getViewConfigs().get(0).setDisplay(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.DISPLAY_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidBuyable() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getViewConfigs().get(0).setBuyable(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUYABLE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void updateSummaryTestWithInvalidViewConfigChannelId() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    request.getViewConfigs().get(0).setChannelId(null);
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.CHANNEL_ID_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).updateSummary(Mockito.any(),
          Mockito.any(), Mockito.any());
      verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void filterSummaryByGdnSkuTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_SINGLE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findSummaryByGdnSku(Mockito.any(),
        Mockito.any());
  }

  @Test
  public void filterSummaryByGdnSkuForceReviewTrueTest() throws Exception {
    Page<ProductLevel3Summary> productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.getContent().get(0).setForceReview(true);
    Mockito.when(
        getProductLevel3Service().findSummaryByGdnSku(Mockito.any(), Mockito.any()))
        .thenReturn(productLevel3Summary.getContent().get(0));
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_SINGLE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", DEFAULT_GDN_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findSummaryByGdnSku(Mockito.any(),
        Mockito.any());
  }

  @Test
  public void filterSummaryByGdnSkuTestWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_SINGLE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").addParameter("gdnSku", DEFAULT_GDN_SKU)
            .build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findSummaryByGdnSku(
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void filterSummaryByGdnSkuTestWithInvalidGdnSku() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_SINGLE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("gdnSku", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findSummaryByGdnSku(
          Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void filterDashboardTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DASHBOARD)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findDashboard(Mockito.any());

  }

  @Test
  public void filterDashboardTestWithInvalidBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_DASHBOARD)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).findDashboard(Mockito.any());
    }
  }


  @Test
  public void checkPickupPointCodeUsedTest() throws Exception {
    UsageStatus usageStatus = new UsageStatus(true);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.CHECK_PICKUP_POINT_USED)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("pickupPointCode", DEFAULT_PICKUP_POINT_CODE).build();
    Mockito.when(getProductLevel3Service().checkPickupPointCodeUsed(DEFAULT_PICKUP_POINT_CODE))
        .thenReturn(usageStatus);
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).checkPickupPointCodeUsed(DEFAULT_PICKUP_POINT_CODE);
  }

  @Test
  public void checkPickupPointCodeUsedTestWithInvalidPickupPointCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.CHECK_PICKUP_POINT_USED)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("pickupPointCode", "").build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK));
      verify(getProductLevel3Service(), NEVER_CALLED).checkPickupPointCodeUsed(
          Mockito.any());
    }
  }

  @Test
  public void filterSummaryForBulkDownloadTest() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        new PageImpl<ProductLevel3Summary>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    Mockito.when(
        productLevel3Service.findProductSummaryForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_PAGE_REQUEST, DEFAULT_REQUEST_ID, null, null, null, null, null, null, false))
        .thenReturn(bulkDownloadProductLevel3Summary);
    productLevel3Controller.filterSummaryForBulkDownload(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID,
        DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, 0,
        10);

    verify(productLevel3Service).findProductSummaryForBulkDownload(Mockito.any(),
        (PageRequest) Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyBoolean());

  }

  @Test
  public void filterSummaryForBulkDownload_ErrorTest() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        new PageImpl<ProductLevel3Summary>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    Mockito.when(
        productLevel3Service.findProductSummaryForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_PAGE_REQUEST, DEFAULT_REQUEST_ID, null, null, null, null, null, null, false))
        .thenThrow(new RuntimeException());
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productLevel3Controller.filterSummaryForBulkDownload(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID,
            DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
            0, 10);
      });
    } catch (Exception e) {
      throw e;
    }
    verify(productLevel3Service).findProductSummaryForBulkDownload(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void bulkDownloadSummaryTest() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        new PageImpl<ProductLevel3Summary>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    Mockito.when(productLevel3RequestConverter
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest))
        .thenReturn(new ProductLevel3SummaryFilter());
    Mockito.when(productLevel3Service
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class)))
        .thenReturn(bulkDownloadProductLevel3Summary);
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = productLevel3Controller
        .bulkDownloadSummary(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, 0, 10, productLevel3SummaryRequest);
    verify(productLevel3Service)
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class));
    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(0, response.getValue().getPageMetaData().getPageNumber());
    Assertions.assertEquals(10, response.getValue().getPageMetaData().getPageSize());
    Assertions.assertEquals(10, response.getValue().getPageMetaData().getTotalRecords());
  }

  @Test
  public void bulkDownloadSummaryFromDbTest() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    Mockito.when(productLevel3RequestConverter
            .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest))
        .thenReturn(new ProductLevel3SummaryFilter());
    Mockito.when(productLevel3Service
            .findProductSummaryForBulkDownloadByDb(Mockito.any(ProductLevel3SummaryFilter.class), Mockito.eq(false),
                Mockito.eq(null)))
        .thenReturn(new BulkDownloadProductLevel3Response());
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = productLevel3Controller
        .bulkDownloadSummaryFromDb(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,false, null, productLevel3SummaryRequest);
    verify(productLevel3Service)
        .findProductSummaryForBulkDownloadByDb(Mockito.any(ProductLevel3SummaryFilter.class), Mockito.eq(false),
            Mockito.eq(null));
    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkDownloadSummaryForceReviewTest() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    List<ProductLevel3Summary> forceReviewItem = getProductLevel3Summaries();
    forceReviewItem.get(0).setForceReview(true);
    level3SummaryList.add(forceReviewItem.get(0));
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    Page<ProductLevel3Summary> productLevel3SummaryPage = new PageImpl<>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    Mockito.when(productLevel3RequestConverter
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest))
        .thenReturn(new ProductLevel3SummaryFilter());
    Mockito.when(productLevel3Service
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class)))
        .thenReturn(bulkDownloadProductLevel3Summary);
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = productLevel3Controller
        .bulkDownloadSummary(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, 0, 10, productLevel3SummaryRequest);
    verify(productLevel3Service)
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class));
    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(1, response.getValue().getProductLevel3SummaryResponses().size());
  }

  private List<ProductLevel3Summary> getProductLevel3Summaries() {
    List<ProductLevel3Summary> level3SummaryList = new ArrayList<>();
    ProductLevel3Summary productLevel3SummaryItem = new ProductLevel3Summary();
    productLevel3SummaryItem.setPrices(new ArrayList<ProductLevel3Price>());
    productLevel3SummaryItem.setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productLevel3SummaryItem.setImages(new ArrayList<ProductLevel3Image>());
    productLevel3SummaryItem.getPrices().add(new ProductLevel3Price());
    productLevel3SummaryItem.getViewConfigs().add(new ProductLevel3ViewConfig());
    productLevel3SummaryItem.getImages().add(new ProductLevel3Image());
    productLevel3SummaryItem.setWholesalePriceActivated(null);
    productLevel3SummaryItem.setOriginalSellingPrice(100);
    productLevel3SummaryItem.setProductName(DEFAULT_PRODUCT_NAME);
    level3SummaryList.add(productLevel3SummaryItem);
    return level3SummaryList;
  }

  @Test
  public void bulkDownloadSummaryTestWithEmptyBPCode() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        new PageImpl<ProductLevel3Summary>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
          productLevel3Controller.bulkDownloadSummary(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID,
              DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, "", 0, 10,
              new ProductLevel3SummaryRequest());
      Assertions.assertFalse(response.isSuccess());
    });
    Mockito.verifyNoMoreInteractions(productLevel3Service);
  }

  @Test
  public void bulkDownloadSummaryTest_Error() throws Exception {
    List<ProductLevel3Summary> level3SummaryList = getProductLevel3Summaries();
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        new PageImpl<ProductLevel3Summary>(level3SummaryList, DEFAULT_PAGEABLE, 10);
    HashMap<String, String> exceptionMap = new HashMap<>();
    BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
        new BulkDownloadProductLevel3Summary(productLevel3SummaryPage, exceptionMap);
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    Mockito.when(productLevel3RequestConverter
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest))
        .thenReturn(new ProductLevel3SummaryFilter());
    Mockito.when(productLevel3Service
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class)))
        .thenThrow(new RuntimeException());


    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = productLevel3Controller
        .bulkDownloadSummary(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, 0, 10, productLevel3SummaryRequest);

    verify(productLevel3Service)
        .findProductSummaryForBulkDownload(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_PAGE_REQUEST),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductLevel3SummaryFilter.class));
    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkDownloadSummaryFromDb_ExceptionTest() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    Mockito.when(productLevel3RequestConverter
            .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest))
        .thenReturn(new ProductLevel3SummaryFilter());
    Mockito.when(productLevel3Service
            .findProductSummaryForBulkDownloadByDb(Mockito.any(ProductLevel3SummaryFilter.class), Mockito.eq(false),
                Mockito.eq(null)))
        .thenThrow(new RuntimeException());
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = productLevel3Controller
        .bulkDownloadSummaryFromDb(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME,DEFAULT_BUSINESS_PARTNER_CODE, false, null, productLevel3SummaryRequest);
    verify(productLevel3Service).findProductSummaryForBulkDownloadByDb(Mockito.any(ProductLevel3SummaryFilter.class),
        Mockito.eq(false), Mockito.eq(null));
    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequest);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductStockConditionCountTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COUNTS_SUMMARY
                    + "/" + DEFAULT_BUSINESS_PARTNER_CODE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
//  Mockito.when(productLevel3Service.countSummary(Mockito.any())).thenReturn(new ProductLevel3SummaryCount());
    Mockito.when(productLevel3AggregatorService.countSummary(Mockito.any())).thenReturn(
        new ProductLevel3SummaryCount());
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
//  Mockito.verify(productLevel3Service).countSummary(Mockito.any());
    verify(productLevel3AggregatorService).countSummary(Mockito.any());
  }

  @Test
  public void updateImageTest() throws Exception {
    UpdateImageRequest updateImageRequest = getUpdateImageRequest();
    ItemResponse itemResponse = getItemResponse(updateImageRequest);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemsResponse();

    Mockito.when(productLevel3Service.getItem(updateImageRequest.getItemSku())).thenReturn(
        itemResponse);
    Mockito.when(productLevel3Service.getProduct(updateImageRequest.getProductSku())).thenReturn(
        productAndItemsResponse);

    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGE)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(updateImageRequest))).andExpect(
        jsonPath("$.success", equalTo(true)));
    verify(getProductLevel3Service()).getItem(updateImageRequest.getItemSku());
    verify(getProductLevel3Service()).getProduct(updateImageRequest.getProductSku());
    verify(getProductLevel3Service()).updateItem(Mockito.any(), Mockito.anyBoolean());
    verify(getProductLevel3Service()).update((ProductRequest) Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void updateImageTestException() throws Exception {
    UpdateImageRequest updateImageRequest = getUpdateImageRequest();
    updateImageRequest.setItemSku(ITEM_SKU);
    Mockito.when(productLevel3Service.getItem(ITEM_SKU)).thenThrow(Exception.class);

    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGE)
        .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID).build();

    this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(updateImageRequest))).andExpect(jsonPath("$.success", equalTo(false)));
    verify(getProductLevel3Service()).getItem(ITEM_SKU);
  }

  private UpdateImageRequest getUpdateImageRequest() {
    List<ProductLevel3ImageRequest> productLevel3ImageRequests = new ArrayList<>();
    ProductLevel3ImageRequest productLevel3ImageRequest =
        new ProductLevel3ImageRequest(true, 1, "path");
    productLevel3ImageRequests.add(productLevel3ImageRequest);
    return new UpdateImageRequest("itemSku", "productSku", "productCode",
        productLevel3ImageRequests);
  }

  private ProductAndItemsResponse getProductAndItemsResponse() {
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProductDTO);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(productResponse);
    return productAndItemsResponse;
  }

  private ItemResponse getItemResponse(UpdateImageRequest updateImageRequest) {
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setItemCode("itemCode");
    itemResponse.setProductSku(updateImageRequest.getProductSku());
    itemResponse.setItemSku(updateImageRequest.getItemSku());
    itemResponse.setItemViewConfigs(new HashSet<>());
    itemResponse.setMasterDataItem(new MasterDataItemDTO());
    return itemResponse;
  }


  @Test
  public void toggleArchiveItemTest() throws Exception {
    Mockito.doNothing().when(getProductLevel3Service())
        .toggleArchiveItem(Mockito.any(), Mockito.anyBoolean());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ARCHIVE_ITEM)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST)
            .addParameter("itemSku", DEFAULT_ITEM_SKU)
            .addParameter("doArchive", DO_ARCHIVE_TRUE.toString()).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).toggleArchiveItem(Mockito.any(),
        Mockito.anyBoolean());
  }

  @Test
  public void bulkArchiveOosItemsTest() throws Exception {
    Mockito.doNothing().when(productLevel3Wrapper)
        .bulkArchiveOldOosProducts(DEFAULT_STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.BULK_ARCHIVE_OOS_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientHost", DEFAULT_CLIENT_HOST)
        .addParameter("bulkArchiveSize", BULK_ARCHIVE_SIZE_PARAM).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Wrapper)
        .bulkArchiveOldOosProducts(DEFAULT_STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
  }

  @Test
  public void toggleArchiveItemTest_checkArg() throws Exception {
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ARCHIVE_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientHost", DEFAULT_CLIENT_HOST)
        .addParameter("itemSku", BLANK).addParameter("doArchive", DO_ARCHIVE_TRUE.toString()).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void toggleArchiveItemTest_Exception() throws Exception {
    Mockito.doThrow(Exception.class).when(getProductLevel3Service())
        .toggleArchiveItem(Mockito.any(), Mockito.anyBoolean());
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ARCHIVE_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientHost", DEFAULT_CLIENT_HOST)
        .addParameter("itemSku", ITEM_SKU).addParameter("doArchive", DO_ARCHIVE_TRUE.toString()).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).toggleArchiveItem(Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void updateSummary_ErrorTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();
    Mockito.when(productLevel3Service
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any(ProductLevel3UpdateSummary.class)))
        .thenThrow(new RuntimeException());
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(getProductLevel3Service())
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void filterSummaryByGdnSkuListTest() throws Exception {
    ItemSkuListRequest request = new ItemSkuListRequest();
    List<String> gdnSkus = new ArrayList<>();
    gdnSkus.add(DEFAULT_GDN_SKU);
    request.setItemSkuList(gdnSkus);
    Page<ProductLevel3Summary> pageOfProductLevel3Summary = generateProductLevel3Summary();
    Mockito.when(
        productLevel3Service.findSummaryByGdnSkuList(Mockito.any(), Mockito.anyList(),
            Mockito.any(PageRequest.class))).thenReturn(pageOfProductLevel3Summary);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_GDN_SKU_LIST)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(jsonPath("$.content[0].originalSellingPrice", equalTo(100d))).
        andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findSummaryByGdnSkuList(Mockito.any(), Mockito.anyList(),
        Mockito.any(PageRequest.class));
  }

  @Test
  public void filterSummaryByGdnSkuListForceReviewTrueTest() throws Exception {
    ItemSkuListRequest request = new ItemSkuListRequest();
    List<String> gdnSkus = new ArrayList<>();
    gdnSkus.add(DEFAULT_GDN_SKU);
    request.setItemSkuList(gdnSkus);
    Page<ProductLevel3Summary> pageOfProductLevel3Summary = generateProductLevel3Summary();
    pageOfProductLevel3Summary.getContent().get(0).setForceReview(true);
    Mockito.when(
        productLevel3Service.findSummaryByGdnSkuList(Mockito.any(), Mockito.anyList(),
            Mockito.any(PageRequest.class))).thenReturn(pageOfProductLevel3Summary);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_GDN_SKU_LIST)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request))).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).findSummaryByGdnSkuList(Mockito.any(), Mockito.anyList(),
        Mockito.any(PageRequest.class));
  }

  @Test
  public void getProductsCountByBrandTest() throws Exception {
    Mockito.when(this.productLevel3Service.getProductsCountByBrand(Mockito.any()))
        .thenReturn(1L);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.COUNT_PRODUCT_BY_BRAND)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("brand", DEFAULT_BRAND).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3Service).getProductsCountByBrand(Mockito.any());
  }

  @Test
  public void getProductsCountByBrandTestWithEmptyBrand() throws Exception {
    Mockito.when(this.productLevel3Service.getProductsCountByBrand(Mockito.any()))
        .thenReturn(1L);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.COUNT_PRODUCT_BY_BRAND)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("brand", "").build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    });
  }

  @Test
  public void testFilterSummarybyCategoryAndBrand() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.FILTER_SUMMARY_CATEGORY_BRAND)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("page", PAGE.toString())
            .addParameter("size", SIZE.toString())
            .addParameter("orderBy", ORDER_BY)
            .addParameter("sortBy", SORT_BY)
            .build();
    BrandAndCategoryItemSummaryRequest request =
        new BrandAndCategoryItemSummaryRequest();
    request.setBrands(Collections.singletonList(BRAND));
    request.setCategories(Collections.singletonList(CATEGORY));
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service)
        .findSummaryByCategoryAndBrandFilter(Mockito.any(BrandAndCategoryItemSummaryRequest.class),
            Mockito.any(PageRequest.class), Mockito.any(SortOrder.class));
  }

  @Test
  public void testFilterSummarybyCategoryAndBrand_whenException() throws Exception {
    Mockito.when(productLevel3Service
        .findSummaryByCategoryAndBrandFilter(Mockito.any(BrandAndCategoryItemSummaryRequest.class),
            Mockito.any(PageRequest.class), Mockito.any(SortOrder.class))).thenThrow(new RuntimeException());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.FILTER_SUMMARY_CATEGORY_BRAND)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("page", PAGE.toString())
            .addParameter("size", SIZE.toString())
            .addParameter("orderBy", ORDER_BY)
            .addParameter("sortBy", SORT_BY)
            .build();
    BrandAndCategoryItemSummaryRequest request =
        new BrandAndCategoryItemSummaryRequest();
    request.setBrands(Collections.singletonList(BRAND));
    request.setCategories(Collections.singletonList(CATEGORY));
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service)
        .findSummaryByCategoryAndBrandFilter(Mockito.any(BrandAndCategoryItemSummaryRequest.class),
            Mockito.any(PageRequest.class), Mockito.any(SortOrder.class));
  }

  @Test
  public void getEstimatePriceDetailsTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3Controller, "maxInventoryRequest", MAX_INVENTORY_REQUEST);
    EstimateItemPriceDTO estimateItemPriceDTO = new EstimateItemPriceDTO();
    estimateItemPriceDTO.setNormalPrice(NORMAL_PRICE);
    estimateItemPriceDTO.setOfferPrice(OFFER_PRICE);
    Mockito.when(this.productLevel3Service
        .estimatePriceForFlow2ProductCreation(ITEM_CODE, LOWEST_PRICE_COEFFICIENT, MAX_INVENTORY_REQUEST))
        .thenReturn(estimateItemPriceDTO);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_ESTIMATE_PRICE_FOR_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("itemCode", ITEM_CODE)
        .addParameter("lowestPriceCoefficient", LOWEST_PRICE_COEFFICIENT + "").build();

    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .estimatePriceForFlow2ProductCreation(ITEM_CODE, LOWEST_PRICE_COEFFICIENT, MAX_INVENTORY_REQUEST);
  }

  @Test
  public void getEstimatePriceDetailsExceptionTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_ESTIMATE_PRICE_FOR_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("itemCode", "")
        .addParameter("lowestPriceCoefficient", LOWEST_PRICE_COEFFICIENT + "").build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    });
  }


  @Test
  public void bulkArchiveItemsTest() throws Exception {
    Mockito.when(productLevel3Service.bulkArchiveItems(Mockito.anyList(), Mockito.any()))
        .thenReturn(Collections.emptyList());
    final List<String> itemSkus = Arrays.asList(DEFAULT_ITEM_SKU);
    ItemBulkArchiveRequest request = ItemBulkArchiveRequest.builder()
        .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).itemSkus(itemSkus)
        .build();
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.ITEM_BULK_ARCHIVE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
    verify(productLevel3Service).bulkArchiveItems(Mockito.eq(itemSkus),
        Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
  }

  @Test
  public void bulkArchiveItemsTest_WithFailure() throws Exception {
    Mockito.when(productLevel3Service.bulkArchiveItems(Mockito.anyList(), Mockito.any()))
        .thenReturn(Collections.emptyList());
    ItemBulkArchiveRequest request = ItemBulkArchiveRequest.builder()
        .itemSkus(Arrays.asList(DEFAULT_ITEM_SKU)).build();
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.ITEM_BULK_ARCHIVE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
      getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void activeBrandByCategoryIdTest() throws Exception {
    Mockito.when(productLevel3Service.getAllActiveBrandsByCNCategoryId(DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_CATEGORY_ID,
        false))
        .thenReturn(Collections.emptyList());
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_ACTIVE_BRANDS_BY_CATEGORY_ID)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("categoryId", DEFAULT_CATEGORY_ID)
            .build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service)
        .getAllActiveBrandsByCNCategoryId(DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_CATEGORY_ID, false);
  }

  @Test
  public void activeBrandByCategoryIdFailureTest() throws Exception {
    Mockito.when(productLevel3Service.getAllActiveBrandsByCNCategoryId(DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_CATEGORY_ID,
        false))
        .thenThrow(Exception.class);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_ACTIVE_BRANDS_BY_CATEGORY_ID)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("categoryId", DEFAULT_CATEGORY_ID)
            .build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service)
        .getAllActiveBrandsByCNCategoryId(DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_CATEGORY_ID, false);
  }

  @Test
  public void activeBrandByCategoryIdEmptyCategoryTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_ACTIVE_BRANDS_BY_CATEGORY_ID)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("categoryId", StringUtils.EMPTY)
            .build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void getAllProductsTest() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    summaryFilterRequest.setSuspensionStatus("All");
    Page<SuspensionProductResponse> suspensionProductResponses = new PageImpl<>(Collections.emptyList(), DEFAULT_PAGE_REQUEST, 25);
    Mockito.when(productLevel3Service
        .getAllProducts(Mockito.any(SummaryFilterRequest.class), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(pageable.getClass()))).thenReturn(suspensionProductResponses );
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_ALL_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(summaryFilterRequest))).andExpect(status().isOk());
    verify(productLevel3Service)
        .getAllProducts(summaryFilterRequest, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_STORE_ID,
            PageRequest.of(0, 25));
  }

  @Test
  public void getAllProductsTest_withException() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    Mockito.doThrow(Exception.class).when(productLevel3Service)
        .getAllProducts(Mockito.any(SummaryFilterRequest.class), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(pageable.getClass()));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_ALL_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(summaryFilterRequest))).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(productLevel3Service)
        .getAllProducts(Mockito.eq(summaryFilterRequest), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(PageRequest.of(0, 25)));
  }

  @Test
  public void getSuspendedItemsTest() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    Page<SuspensionItemResponse> suspensionProductResponses = new PageImpl<>(Collections.emptyList(), DEFAULT_PAGE_REQUEST, 25);
    Mockito.when(productLevel3Service
        .getSuspendedItems(Mockito.any(SummaryFilterRequest.class), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(pageable.getClass()))).thenReturn(suspensionProductResponses);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_SUSPENDED_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(summaryFilterRequest))).andExpect(status().isOk());
    verify(productLevel3Service)
        .getSuspendedItems(summaryFilterRequest, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_STORE_ID,
            PageRequest.of(0, 25));
  }

  @Test
  public void getSuspendedItemsTest_withException() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    Mockito.doThrow(new RuntimeException()).when(productLevel3Service)
        .getSuspendedItems(Mockito.any(SummaryFilterRequest.class), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(pageable.getClass()));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_SUSPENDED_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(summaryFilterRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(productLevel3Service)
        .getSuspendedItems(Mockito.eq(summaryFilterRequest), Mockito.eq(DEFAULT_REQUEST_ID),
            Mockito.eq(DEFAULT_USERNAME), Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(PageRequest.of(0, 25)));
  }

  @Test
  public void doSuspensionProductsActionsTest() throws Exception {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_SUSPENSION)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(suspensionProductRequest))).andExpect(status().isOk());
    verify(productLevel3Service)
        .doSuspensionProductsActions(DEFAULT_STORE_ID, DEFAULT_USERNAME, suspensionProductRequest);
  }

  @Test
  public void doSuspensionProductsActions_withExceptionTest() throws Exception {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    Mockito.doThrow(new RuntimeException()).when(productLevel3Service)
        .doSuspensionProductsActions(DEFAULT_STORE_ID, DEFAULT_USERNAME, suspensionProductRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_SUSPENSION)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(suspensionProductRequest))).andExpect(status().isOk());
    verify(productLevel3Service)
        .doSuspensionProductsActions(DEFAULT_STORE_ID, DEFAULT_USERNAME, suspensionProductRequest);
  }

  @Test
  public void getSuspensionHistoryTest() throws Exception {
    ProductSuspensionHistory productSuspensionHistory = new ProductSuspensionHistory();
    productSuspensionHistory.setProductSku(DEFAULT_PRODUCT_SKU);
    productSuspensionHistory.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productSuspensionHistory.setStoreId(DEFAULT_STORE_ID);
    productSuspensionHistory.setDescription(DEFAULT_DESCRIPTION);
    List<ProductSuspensionHistory> historyList = Arrays.asList(productSuspensionHistory);
    Page<ProductSuspensionHistory> productSuspensionHistories = new PageImpl<>(historyList, DEFAULT_PAGE_REQUEST, 10);
    Mockito.when(
        this.productLevel3Service.getSuspensionHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, DEFAULT_PAGE_REQUEST))
        .thenReturn(productSuspensionHistories);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SUSPENSION_HISTORY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("page", "0").addParameter("size", "10")
            .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getSuspensionHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void getSuspensionHistoryExceptionTest() throws Exception {
    Mockito.when(
        this.productLevel3Service.getSuspensionHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, DEFAULT_PAGE_REQUEST))
        .thenThrow(Exception.class);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SUSPENSION_HISTORY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("page", "0").addParameter("size", "10")
            .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(productLevel3Service)
        .getSuspensionHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void getSuspensionHistorEmptyProductSkuExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SUSPENSION_HISTORY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME).addParameter("page", "0").addParameter("size", "10")
            .addParameter("productSku", "").build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    });
  }

  @Test
  public void doBulkSuspensionProductsActionsTest() throws Exception {
    Mockito.when(this.productLevel3Wrapper
        .doBulkProductSuspension(DEFAULT_STORE_ID, DEFAULT_USERNAME, Arrays.asList(suspensionProductRequest)))
        .thenReturn(Arrays.asList(new SuspensionProductResponse()));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.BULK_PRODUCT_SUSPENSION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).build();
    SuspensionProductRequestList list = new SuspensionProductRequestList();
    list.setSuspensionProductRequestList(Arrays.asList(suspensionProductRequest));
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(list))).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productLevel3Wrapper)
        .doBulkProductSuspension(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.anyList());
  }

  @Test
  public void countSummaryForInactiveProductTest() throws Exception {
    CountProductLevel3InactiveResponse countProductLevel3InactiveResponse = new CountProductLevel3InactiveResponse();
    Mockito.when(this.productLevel3Service
        .countSummaryForInactiveProduct(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(countProductLevel3InactiveResponse);
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COUNT_SUMMARY_FOR_INACTIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service)
        .countSummaryForInactiveProduct(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void countSummaryForInactiveProductEmptyBusinessPartnerCodeTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COUNT_SUMMARY_FOR_INACTIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("businessPartnerCode", "").build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    });
  }

  @Test
  public void countSummaryForInactiveProductExceptionTest() throws Exception {
    Mockito.when(this.productLevel3Service
        .countSummaryForInactiveProduct(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COUNT_SUMMARY_FOR_INACTIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(productLevel3Service)
          .countSummaryForInactiveProduct(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
              DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void testUpdateAndReturnWithVersion() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setVersion(VERSION);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class),  Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service())
        .update(productLevel3ArgumentCaptor.capture(), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
    Assertions.assertEquals(VERSION, productLevel3ArgumentCaptor.getValue().getVersion());
  }

  @Test
  public void checkCategoryProductWholesaleRulesTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setVersion(VERSION);
    Mockito.when(getProductLevel3Service()
        .checkCategoryProductWholesaleRules(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(true);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COMPARE_PRODUCT_CATEGORY_WHOLEALE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("productCode", DEFAULT_PRODUCT_CODE)
            .addParameter("categoryCode", DEFAULT_CATEGORY_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .checkCategoryProductWholesaleRules(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void checkCategoryProductWholesaleRules_exceptionTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setVersion(VERSION);
    Mockito.when(getProductLevel3Service()
        .checkCategoryProductWholesaleRules(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenThrow(Exception.class);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COMPARE_PRODUCT_CATEGORY_WHOLEALE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("productCode", DEFAULT_PRODUCT_CODE)
            .addParameter("categoryCode", DEFAULT_CATEGORY_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .checkCategoryProductWholesaleRules(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void checkCategoryProductWholesaleRulesEmptyProductCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setVersion(VERSION);
    product.setProductCode(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COMPARE_PRODUCT_CATEGORY_WHOLEALE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("productCode", StringUtils.EMPTY)
            .addParameter("categoryCode", DEFAULT_CATEGORY_CODE).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON));
    });
  }

  @Test
  public void checkCategoryProductWholesaleRulesEmptyCategoryCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setVersion(VERSION);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COMPARE_PRODUCT_CATEGORY_WHOLEALE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("productCode", DEFAULT_PRODUCT_CODE)
            .addParameter("categoryCode", StringUtils.EMPTY).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON));
    });
  }

  @Test
  public void updateItemPrice_rulesValidationErrorTest() throws Exception {
    Mockito.when(productLevel3Service
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any())).thenThrow(new ApiIncorrectInputDataException(ErrorCategory.UNSPECIFIED.getMessage(),
        ApiErrorCode.WHOLESALE_QUANTITY_DUPLICATE));
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_PRICE)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("itemSku", ProductLevel3ControllerTest.ITEM_SKU).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(new ProductPriceAndWholesaleRequest()))).andExpect(
        jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service)
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any());
  }

  @Test
  public void updateItemPrice_exceptionTest() throws Exception {
    Mockito.when(productLevel3Service
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any()))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorCategory.UNSPECIFIED.getMessage()));
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_PRICE)
            .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("itemSku", ProductLevel3ControllerTest.ITEM_SKU).build();

    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(new ProductPriceAndWholesaleRequest()))).andExpect(
        jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service)
        .updateItemPrice(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(ProductPriceAndWholesaleRequest.class),
            Mockito.any());
  }

  @Test
  public void updateSummaryTestWithWholesalePriceFalse() throws Exception {
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(false);
    Mockito.when(getProductLevel3Service()
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any(ProductLevel3UpdateSummary.class)))
        .thenReturn(productLevel3Summary);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();

    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(getProductLevel3Service())
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateSummaryTestWithWholesalePriceTrue() throws Exception {
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(true);
    Mockito.when(getProductLevel3Service()
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any(ProductLevel3UpdateSummary.class)))
        .thenReturn(productLevel3Summary);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_SUMMARY)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("gdnSku", DEFAULT_GDN_SKU)
            .addParameter("clientHost", DEFAULT_CLIENT_HOST).build();
    ProductLevel3UpdateSummaryRequest request = generateProductLevel3UpdateSummaryRequest();

    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(getProductLevel3Service())
        .updateSummary(Mockito.any(), Mockito.any(), Mockito.any());
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateAndReturnWithUSPMoreThan400CharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setUniqueSellingPoint(RandomStringUtils.randomAlphanumeric(401));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS));
    }
  }

  @Test
  public void updateAndReturnWithUSPWithSpecialCharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setUniqueSellingPoint(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES);
    Mockito.when(getProductLevel3Service()
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductLevel3DTO.builder().productLevel3(generateProductLevel3()).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(getProductLevel3Service())
        .update(Mockito.any(ProductLevel3.class), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq(false), Mockito.eq(false),  Mockito.eq(false));
    verify(getProductLevel3Service()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void updateAndReturnWithDescriptionMoreThan5000CharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription(RandomStringUtils.random(5001));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING));
    }
  }

  @Test
  public void updateAndReturnWithDescriptionWithoutTagsTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription("<p><P><P>somethingsomething");
    ReflectionTestUtils.setField(productLevel3Controller, "maximumCharactersInDescription", 9);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_RETURN)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
            .addParameter("isOnlyExternal", String.valueOf(false)).build();
    try {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(productLevel3Service).getMinimumPrice(DEFAULT_STORE_ID);
      Assertions.assertTrue(e.getMessage()
          .contains("Max. 9 characters for Description"));
    }
  }

  @Test
  public void updateItemsPriceStockImages() throws Exception {
    UpdateItemsPriceStockImagesRequest request = new UpdateItemsPriceStockImagesRequest();
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(DEFAULT_ITEM_SKU);
    request.setProductItems(Arrays.asList(productPriceStockAndImagesRequest));
    Mockito.when(productLevel3Service
        .editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
            Mockito.any())).thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_ITEMS_PRICE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.DEFAULT_BUSINESS_PARTNER_CODE).build();
    this.mockMvc.perform(
        post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request))).andExpect(
        jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
            Mockito.any());
  }

  @Test
  public void updateItemsPriceStockImagesNullRequestTest() throws Exception {
    UpdateItemsPriceStockImagesRequest request = new UpdateItemsPriceStockImagesRequest();
    Mockito.when(productLevel3Service
        .editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
            Mockito.any())).thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_ITEMS_PRICE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.DEFAULT_BUSINESS_PARTNER_CODE).build();
    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(
          post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(request))).andExpect(
          jsonPath("$.success", equalTo(true)));
    });
  }

  @Test
  public void updateItemsPriceStockProductItemListIsEmptyImages() throws Exception {
    UpdateItemsPriceStockImagesRequest request = new UpdateItemsPriceStockImagesRequest();
    request.setProductItems(new ArrayList<>());
    Mockito.when(productLevel3Service
        .editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
            Mockito.any())).thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_ITEMS_PRICE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.DEFAULT_BUSINESS_PARTNER_CODE).build();
    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(
          post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(request))).andExpect(
          jsonPath("$.success", equalTo(true)));
    });
  }

  @Test
  public void updateItemsPriceStockImagesExceptionTest() throws Exception {
    UpdateItemsPriceStockImagesRequest request = new UpdateItemsPriceStockImagesRequest();
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(DEFAULT_ITEM_SKU);
    request.setProductItems(Arrays.asList(productPriceStockAndImagesRequest));
    Mockito.when(productLevel3Service.editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
        Mockito.any())).thenThrow(Exception.class);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_ITEMS_PRICE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.DEFAULT_BUSINESS_PARTNER_CODE).build();
    this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))).andExpect(jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service).editProductItemsPriceStockImages(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(UpdateItemsPriceStockImagesRequest.class),
        Mockito.any());
  }

  @Test
  public void filterSummaryDetailsTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_SUMMARY_DETAILS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    ProductLevel3SummaryDetailsRequest request = new ProductLevel3SummaryDetailsRequest();
    request.setProductSku(DEFAULT_PRODUCT_SKU);
    ProductLevel3SummaryFilterDetails filter = new ProductLevel3SummaryFilterDetails();
    filter.setProductSku(DEFAULT_PRODUCT_SKU);
    Page<ProductLevel3SummaryDetails> pageOfProductLevel3Summary = generateProductLevel3SummaryDetails();
    Mockito.when(
        productLevel3RequestConverter.convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(Mockito
            .any(ProductLevel3SummaryDetailsRequest.class))).thenReturn(filter);
    for (ProductLevel3SummaryDetails productLevel3SummaryDetails : pageOfProductLevel3Summary.getContent()) {
      ProductLevel3SummaryDetailsResponse response = new ProductLevel3SummaryDetailsResponse();
      BeanUtils.copyProperties(productLevel3SummaryDetails, response);
      response.setCogs(10000.0);
      response.setCogsErrorCode("ERR-00000");
      Mockito.when(
          productLevel3ResponseConverter
              .convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(productLevel3SummaryDetails))
          .thenReturn(response);
    }
    Mockito.when(productLevel3Service
        .findSummaryDetailsByFilter(Mockito.any(), Mockito.eq(filter), Mockito.any(Pageable.class)))
        .thenReturn(pageOfProductLevel3Summary);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(productLevel3RequestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(productLevel3SummaryDetailsRequestCaptor.capture());
    verify(productLevel3Service).findSummaryDetailsByFilter(Mockito.any(), Mockito.eq(filter),
        Mockito.any(Pageable.class));
    verify(productLevel3ResponseConverter, Mockito.times(pageOfProductLevel3Summary.getContent().size()))
        .convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(
            Mockito.any(ProductLevel3SummaryDetails.class));

    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productLevel3SummaryDetailsRequestCaptor.getValue().getProductSku());
  }

  @Test
  public void filterSummaryDetailsEmptyBusinessPartnerCodeTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_SUMMARY_DETAILS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", "").build();
    Assertions.assertThrows(ServletException.class, () -> {
      this.mockMvc
          .perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                  .contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(new ProductLevel3SummaryDetailsRequest())));
    });
  }

  @Test
  public void filterSummaryDetailsEmptyProductSkuTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_SUMMARY_DETAILS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    Assertions.assertThrows(ServletException.class, () -> {
      this.mockMvc
          .perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                  .contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(new ProductLevel3SummaryDetailsRequest())));
    });
  }

  private Page<ProductLevel3SummaryDetails> generateProductLevel3SummaryDetails() {
      List<ProductLevel3SummaryDetails> productLevel3SummaryDetails = getProductLevel3SummariesDetails();
      return new PageImpl<ProductLevel3SummaryDetails>(productLevel3SummaryDetails, DEFAULT_PAGEABLE, 1);
  }

  private List<ProductLevel3SummaryDetails> getProductLevel3SummariesDetails() {
    List<ProductLevel3SummaryDetails> productLevel3SummaryDetailsArrayList = new ArrayList<>();
    ProductLevel3SummaryDetails productLevel3SummaryDetails = new ProductLevel3SummaryDetails();
    productLevel3SummaryDetails.setPrices(new ArrayList<ProductLevel3Price>());
    productLevel3SummaryDetails.setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productLevel3SummaryDetails.setImages(new ArrayList<ProductLevel3SummaryDetailsImage>());
    productLevel3SummaryDetails.getPrices().add(new ProductLevel3Price());
    productLevel3SummaryDetails.getViewConfigs().add(new ProductLevel3ViewConfig());
    productLevel3SummaryDetails.getImages().add(new ProductLevel3SummaryDetailsImage());
    productLevel3SummaryDetails.setWholesalePriceActivated(null);
    productLevel3SummaryDetailsArrayList.add(productLevel3SummaryDetails);
    return productLevel3SummaryDetailsArrayList;
  }

  @Test
  public void getPickupPointCodesTest() throws Exception {
    Mockito.when(productLevel3Service.getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, false)).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(0)).addParameter("size", String.valueOf(1))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("needCorrection", "false")
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("fbbActivated", "false").build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, false);
  }

  @Test
  public void getPickupPointCodesTestBusinessPartnerCode() throws Exception {
    Mockito.when(productLevel3Service.getPickupPointCodes(DEFAULT_PRODUCT_SKU, PAGE, SIZE, false, DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, false)).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(0)).addParameter("size", String.valueOf(1))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("needCorrection", "false")
        .addParameter("businessPartnerCode", "").addParameter("fbbActivated", "false").build();
    try {
      getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk());
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage()
        .contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
      verify(getProductLevel3Service(), NEVER_CALLED).getPickupPointCodes(Mockito.eq(DEFAULT_PRODUCT_SKU),
          Mockito.eq(PAGE), Mockito.eq(SIZE), Mockito.eq(Boolean.FALSE), Mockito.eq(DEFAULT_STORE_ID),
          Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(Boolean.FALSE));
    }
  }
  @Test
  public void getPickupPointCodesMppErrorTest() throws Exception {
    ApplicationException ae = new ApplicationException(ErrorCategory.INVALID_STATE,
      ApiErrorCode.MULTI_PICKUP_POINT_ENABLED_ERROR.getDesc());
    Mockito.when(productLevel3Service.getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, false)).thenThrow(ae);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(0)).addParameter("size", String.valueOf(1))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("needCorrection", "false")
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("fbbActivated", "false")
        .build();
    try {
        getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(status().isOk());
    } finally {
      verify(productLevel3Service).getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
          DEFAULT_BUSINESS_PARTNER_CODE, false);
    }
  }

  @Test
  public void getPickupPointCodesExceptionTest() throws Exception {
    Mockito.when(productLevel3Service.getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, false)).thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(0)).addParameter("size", String.valueOf(1))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).addParameter("needCorrection", "false")
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).addParameter("fbbActivated", "false")
        .build();
    try {
      getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      verify(productLevel3Service).getPickupPointCodes(DEFAULT_PRODUCT_SKU, 0, 1, false, DEFAULT_STORE_ID,
          DEFAULT_BUSINESS_PARTNER_CODE, false);
    }
  }

  @Test
  public void getUniquePickupPointCodesTest() throws Exception {
    Mockito.when(productLevel3Service.getUniquePickupPointCodes(DEFAULT_PRODUCT_SKU))
        .thenReturn(new UniquePickupPointCodeResponse());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_UNIQUE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getUniquePickupPointCodes(DEFAULT_PRODUCT_SKU);
  }

  @Test
  public void getUniquePickupPointCodesExceptionTest() throws Exception {
    Mockito.when(productLevel3Service.getUniquePickupPointCodes(DEFAULT_PRODUCT_SKU))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_UNIQUE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    try {
      getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      verify(productLevel3Service).getUniquePickupPointCodes(DEFAULT_PRODUCT_SKU);
    }
  }

  @Test
  public void updateLogisticsTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Mockito.when(getProductLevel3Service()
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsNeedRevisionTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Mockito.when(getProductLevel3Service()
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
            .addParameter("isNeedCorrection", String.valueOf(true)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateLogisticsForNeedRevision(Mockito.any(ProductLevel3UpdateRequest.class));
  }


  @Test
  public void updateLogisticsExceptionTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ApiErrorCode.ITEM_IS_REJECTED);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    verify(getProductLevel3Service())
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }


  @Test
  public void updateLogisticsException1Test() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Mockito.doThrow(Exception.class).when(getProductLevel3Service())
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    verify(getProductLevel3Service())
        .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsWithPreOrderTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
    Assertions.assertEquals(PREORDER_DAYS_TYPE,
        productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE,
        productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
  }

  @Test
  public void updateLogisticsException_emptyProductSkuTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setProductSku(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyProductCodeTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setProductCode(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.anyBoolean(), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyProductTypeTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setProductType(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.anyBoolean(), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyShippingWeightTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setShippingWeight(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyLengthTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setLength(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false),Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }


  @Test
  public void updateLogisticsException_emptyHeightTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setHeight(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.anyBoolean(), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyWidthTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setWidth(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyWeightTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setWeight(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void updateLogisticsException_emptyBusinessPartnerCodeTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    product.setBusinessPartnerCode(null);
    try {
      URI uri =
          new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
              .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
              .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
              .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false))
              .build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
              Mockito.eq(false), Mockito.eq(false));
    }
  }

  @Test
  public void editProductInfoTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductLevel3LogisticsRequest(null);
    product.setItems(null);
    product.setUrl(YOUTUBE_INVALID_URL);
    editProductResponse.setApiErrorCode(null);
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    productEditValidationDTO.getEditProductResponse().setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse1);
     Mockito.when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
            Mockito.any(ProfileResponse.class), Mockito.eq(false), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), (Mockito.eq(false)))).thenReturn(productEditValidationDTO);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(productEditValidationDTO);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
  }

  @Test
  public void editProductInfoTestForProductEditValidationNull() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductLevel3LogisticsRequest(null);
    product.setItems(null);
    product.setUrl(YOUTUBE_INVALID_URL);
    editProductResponse.setApiErrorCode(null);
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    productEditValidationDTO.setEditProductResponse(editProductResponse);

    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse1);
    Mockito.when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
        Mockito.any(ProfileResponse.class), Mockito.eq(false), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.eq(false))).thenReturn(productEditValidationDTO);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false))).thenReturn(productEditValidationDTO);
    Mockito.when(productLevel3Service.updateEditInfo(Mockito.any(ProductLevel3.class),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(ProfileResponse.class),Mockito.anyBoolean(),Mockito.any(ProductL3Response.class),
      Mockito.anyBoolean(),  eq(null), Mockito.any())).thenReturn(editProductResponse);

    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(), Mockito.anyBoolean(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void editProductInfoValidVideoUrlTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductLevel3LogisticsRequest(null);
    product.setItems(null);
    product.setUrl(YOUTUBE_URL);
    editProductResponse.setApiErrorCode(null);
    Mockito.when(
            productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(), Mockito.anyBoolean(), eq(null), Mockito.any());
  }

  @Test
  public void editProductInfoWithErrorCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductLevel3LogisticsRequest(null);
    product.setUniqueSellingPoint(null);
    product.setUrl(URL);
    editProductResponse.setApiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED);
    productSystemParameter.setValue("true");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(productSystemParameter);
    Mockito.when(
            productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(),
      Mockito.anyBoolean(),  eq(null), Mockito.any());
  }

  @Test
  public void editProductInfoWithUSPMoreThan400CharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setUniqueSellingPoint(RandomStringUtils.randomAlphanumeric(401));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS));
    }
  }

  @Test
  public void editProductInfoWithEmptyBusinessPartnerCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setBusinessPartnerCode(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyAttributesTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setAttributes(new ArrayList<>());
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.ATTRIBUTE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void editProductInfoWithEmptySpecialDetailsTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setSpecificationDetail(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyAttributeCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setAttributeCode(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyAttributeTypeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setAttributeType(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyAttributeValuesTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setValues(new ArrayList<>());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void editProductInfoWithEmptyAttributeSkuValuesTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.getAttributes().get(0).setSkuValue(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyProductNameTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductName(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyBrandTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setBrand(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithEmptyDescriptionTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription(StringUtils.EMPTY);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void editProductInfoWithDescriptionMoreThan5000CharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setProductName(LARGE_PRODUCT_NAME);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.PRODUCT_NAME_LTE_LIMIT));
    }
  }

  @Test
  public void editProductInfoWithProductNameMoreThan150CharactersTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription(RandomStringUtils.random(5001));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage()
          .contains(ProductLevel3ControllerErrorMessage.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING));
    }
  }

  @Test
  public void editProductInfoProductDescriptionForDescriptionWithoutTagTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    product.setDescription("<p><P><P>somethingsomething");
    ReflectionTestUtils.setField(productLevel3Controller, "maximumCharactersInDescription", 9);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains("Max. 9 characters for Description"));
    }
  }

  @Test
  public void editProductInfoWithException() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(
            productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(), Mockito.anyBoolean(), eq(null), Mockito.any());
  }

  @Test
  public void editProductInfo_applicationException() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(false), Mockito.eq(false),
            Mockito.eq(false))).thenThrow(
        new ApplicationException(ErrorCategory.INVALID_STATE,
            ErrorCategory.INVALID_STATE.getMessage()));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(), Mockito.anyBoolean(), eq(null), Mockito.any());
  }

  @Test
  public void editProductInfoApiErrorCodeTest() throws Exception {
    ProductLevel3Request product = generateProductLevel3Request();
    editProductResponse = new EditProductResponse();
    editProductResponse.setApiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED);
    Mockito.when(
            productLevel3V2Wrapper.editProductDetails(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    verify(productLevel3Service).updateEditInfo(Mockito.any(),Mockito.anyBoolean(),Mockito.anyBoolean(),
        Mockito.anyBoolean(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(), Mockito.anyBoolean(), eq(null), Mockito.any());
  }

  private ProductLevel3UpdateRequest generateProductLevel3UpdateRequest() {
    ProductLevel3UpdateRequest product = new ProductLevel3UpdateRequest();
    product.setLength(1.0);
    product.setWidth(1.0);
    product.setHeight(1.0);
    product.setWeight(1.0);
    product.setShippingWeight(1.0);
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    product.setProductType(1);
    product.setProductSku(DEFAULT_PRODUCT_SKU);
    product.setLateFulfillment(false);
    product.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setAccessChannel(UpdateProductAccessChannel.MTA_API_UPDATE_SINGLE.getDesc());
    return product;
  }

  @Test
  public void getProductSkuSummaryTest() throws Exception {
    Mockito.when(productLevel3Service
        .getProductL3Summary(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            PAGE, SIZE, new ProductL3SummaryRequest()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, SIZE), SIZE));
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
        + ProductLevel3ControllerPath.GET_PRODUCT_SKU_LIST)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("merchantCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("page", String.valueOf(PAGE)).addParameter("size", String.valueOf(SIZE))
        .build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(new ProductL3SummaryRequest())))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .getProductL3Summary(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            PAGE, SIZE, new ProductL3SummaryRequest());
  }

  @Test
  public void getProductSkuSummaryExceptionTest() throws Exception {
    Mockito.when(productLevel3Service
        .getProductL3Summary(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            PAGE, SIZE, new ProductL3SummaryRequest()))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
        + ProductLevel3ControllerPath.GET_PRODUCT_SKU_LIST)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("merchantCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("page", String.valueOf(PAGE)).addParameter("size", String.valueOf(SIZE))
        .build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(new ProductL3SummaryRequest())))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service)
        .getProductL3Summary(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            PAGE, SIZE, new ProductL3SummaryRequest());
  }

  @Test
  public void updatePickupPointCodesTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    Mockito.when(getProductLevel3Service().updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class)))
        .thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service()).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }

  @Test
  public void updatePickupPointCodesPickupPointEmptyTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.setItemsPickupPoint(new ArrayList<>());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Assertions.assertThrows(ServletException.class, () -> {
      getMockMvc().perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    });
  }

  @Test
  public void updatePickupPointCodesExceptionTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    Mockito.doThrow(Exception.class).when(getProductLevel3Service())
        .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    verify(getProductLevel3Service()).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }

  @Test
  public void updatePickupPointCodes_emptyProductSkuTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.setProductSku(null);
    try {
      URI uri = new URIBuilder()
          .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    }
  }

  @Test
  public void updatePickupPointCodes_emptyBusinessPartnerCodeTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.setBusinessPartnerCode(null);
    try {
      URI uri = new URIBuilder()
          .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    }
  }

  @Test
  public void updatePickupPointCodes_emptyItemsTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.setItemsPickupPoint(null);
    try {
      URI uri = new URIBuilder()
          .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    }
  }

  @Test
  public void updatePickupPointCodes_emptyPickupPointCodeTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.getItemsPickupPoint().get(0).setPickupPointCode(null);
    try {
      URI uri = new URIBuilder()
          .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    }
  }

  @Test
  public void updatePickupPointCodes_emptyItemSkuTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    product.getItemsPickupPoint().get(0).setItemSku(null);
    try {
      URI uri = new URIBuilder()
          .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
          .addParameter("clientId", DEFAULT_CLIENT_ID).build();
      Assertions.assertThrows(Exception.class, () -> {
        getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
      });
    } finally {
      verify(getProductLevel3Service(), Mockito.times(0))
          .updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
    }
  }

  @Test
  public void getProductVariantsNameTest() throws Exception {
    Mockito.when(productLevel3Service.getItemSummaryResponseByProductSku(DEFAULT_PRODUCT_SKU, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_VARIANT_NAME)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(PAGE)).addParameter("size", String.valueOf(SIZE))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getItemSummaryResponseByProductSku(DEFAULT_PRODUCT_SKU, PAGE, SIZE);
  }

  @Test
  public void getProductVariantsNameExceptionTest() throws Exception {
    Mockito.when(productLevel3Service.getItemSummaryResponseByProductSku(DEFAULT_PRODUCT_SKU, PAGE, SIZE))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_VARIANT_NAME)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(PAGE)).addParameter("size", String.valueOf(SIZE))
        .addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getItemSummaryResponseByProductSku(DEFAULT_PRODUCT_SKU, PAGE, SIZE);
  }

  @Test
  public void getProductVariantsNameProductSkuEmptyCheckTest() throws Exception {
    Mockito.when(productLevel3Service.getItemSummaryResponseByProductSku(DEFAULT_PRODUCT_SKU, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_VARIANT_NAME)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("clientHost", ProductLevel3ControllerTest.DEFAULT_CLIENT_HOST)
        .addParameter("page", String.valueOf(PAGE)).addParameter("size", String.valueOf(SIZE))
        .addParameter("productSku", StringUtils.EMPTY).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    });
  }

  private PickupPointUpdateRequest generatePickupPointUpdateRequest() {
    PickupPointUpdateRequest pickupPointUpdateRequest = new PickupPointUpdateRequest();
    pickupPointUpdateRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    pickupPointUpdateRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    PickupPointRequest pickupPointRequest = new PickupPointRequest();
    pickupPointRequest.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointRequest.setItemSku(ITEM_SKU);
    pickupPointUpdateRequest.setItemsPickupPoint(Arrays.asList(pickupPointRequest));
    return pickupPointUpdateRequest;
  }

  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    Mockito.when(getProductLevel3Service().getL3DetailByProductSku(Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyList(), Mockito.eq(false)))
        .thenReturn(new ProductLevel3DetailResponse());
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_DETAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productSku", DEFAULT_GDN_SKU)
            .addParameter("isNeedCorrection", Boolean.FALSE.toString()).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .getL3DetailByProductSku(Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyList(), Mockito.eq(false));
  }

  @Test
  public void getL3DetailByProductSkuExceptionTest() throws Exception {
    Mockito.when(productLevel3Service.getL3DetailByProductSku(DEFAULT_PRODUCT_SKU, false, true, new ArrayList<>(), false)).thenThrow(Exception.class);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_DETAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productSku", DEFAULT_PRODUCT_SKU).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    verify(productLevel3Service).getL3DetailByProductSku(DEFAULT_PRODUCT_SKU, false, true, new ArrayList<>(), false);
  }

  @Test
  public void getL3DetailByProductSkuProductSkuEmptyCheckTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.GET_PRODUCT_DETAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productSku", "" ).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(MockMvcRequestBuilders.get(uri));
    });
  }

  @Test
  public void updatePickupPointCodesApiErrorCodeTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
    pickupPointUpdateResponse.setApiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED);
    Mockito.when(getProductLevel3Service().updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class)))
        .thenReturn(pickupPointUpdateResponse);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    verify(getProductLevel3Service()).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }

  @Test
  public void updatePickupPointCodesWithNullApiErrorCodeTest() throws Exception {
    PickupPointUpdateRequest product = generatePickupPointUpdateRequest();
    PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
    pickupPointUpdateResponse.setApiErrorCode(null);
    Mockito.when(getProductLevel3Service().updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class)))
        .thenReturn(pickupPointUpdateResponse);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)));
    verify(getProductLevel3Service()).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }

  @Test
  public void archiveProductsTest() throws Exception {
    Mockito.when(productLevel3Service.toggleArchiveProducts(Mockito.anyList(), Mockito.eq(true)))
        .thenReturn(new ItemBulkArchiveResponse(Collections.emptyList(), null));
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(DEFAULT_ITEM_SKU));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_ARCHIVE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("doArchive", Boolean.TRUE.toString()).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service).toggleArchiveProducts(Mockito.eq(request.getValue()), Mockito.eq(true));
  }

  @Test
  public void archiveProductsSuccessFalseTest() throws Exception {
    Mockito.when(productLevel3Service.toggleArchiveProducts(Mockito.anyList(), Mockito.eq(true)))
        .thenReturn(new ItemBulkArchiveResponse(Collections.singletonList(ITEM_SKU), null));
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(DEFAULT_ITEM_SKU));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_ARCHIVE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("doArchive", Boolean.TRUE.toString()).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service).toggleArchiveProducts(Mockito.eq(request.getValue()), Mockito.eq(true));
  }

  @Test
  public void archiveProductsTest_WithFailure() throws Exception {
    Mockito.doThrow(Exception.class).when(productLevel3Service)
        .toggleArchiveProducts(Mockito.anyList(), Mockito.anyBoolean());
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(DEFAULT_ITEM_SKU));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.PRODUCT_ARCHIVE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("doArchive", Boolean.TRUE.toString()).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productLevel3Service).toggleArchiveProducts(Mockito.eq(request.getValue()), Mockito.eq(true));
  }

  @Test
  public void itemListingUpdateTest() throws Exception {
    ProductLevel3QuickEditRequest productLevel3QuickEditRequest =
        new ProductLevel3QuickEditRequest(Arrays.asList(new QuickEditRequest()));
    Mockito.when(
        productLevel3Service.productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>()))
        .thenReturn(null);
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ITEM_LISTING_UPDATE
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productLevel3QuickEditRequest)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(getProductLevel3Service()).productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>());
  }

  @Test
  public void itemListingUpdateTest_PriceError() throws Exception {
    ProductLevel3QuickEditRequest productLevel3QuickEditRequest =
        new ProductLevel3QuickEditRequest(Arrays.asList(new QuickEditRequest()));
    Mockito.when(
        productLevel3Service.productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>()))
        .thenReturn(ApiErrorCode.PRICE_UPDATE_FAILED);
    URI uri = new URIBuilder().setPath(
        ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ITEM_LISTING_UPDATE
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productLevel3QuickEditRequest)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(getProductLevel3Service())
        .productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>());
  }

  @Test
  public void itemListingUpdateExceptionTest() throws Exception {
    ProductLevel3QuickEditRequest productLevel3QuickEditRequest =
        new ProductLevel3QuickEditRequest(Arrays.asList(new QuickEditRequest()));
    Mockito.doThrow(ApplicationRuntimeException.class).when(productLevel3Service)
        .productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ITEM_LISTING_UPDATE.replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productLevel3QuickEditRequest)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(getProductLevel3Service()).productQuickEdit(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, productLevel3QuickEditRequest, new ArrayList<>());
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDaysTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, preOrderRequest.getPreOrderValue());
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
    Assertions.assertEquals(PREORDER_DAYS_TYPE,
        productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE,
        productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
    assertTrue(productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_DAYS_TYPE,
        productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
    assertEquals(PREORDER_DATE_FORMAT.format(cal.getTime()), PREORDER_DATE_FORMAT
        .format(productLevel3UpdateRequestArgumentCaptor.getValue().getPreOrder().getPreOrderDate()));
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderValueMoreThanLimitTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderValue(91);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderValueZeroTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderValue(0);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeWeekSuccessTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(13);
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeWeekAndMoreThanLimitTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(14);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(MAXIMUM_WEEK_ERROR))).andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeWeekWithZeroWeekTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(0);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ZERO_WEEK_ERROR))).andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeWeekWithNegativeWeekTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderRequest.setPreOrderValue(-1);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ZERO_WEEK_ERROR))).andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDAYSWithNegativeWeekTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_DAYS_TYPE);
    preOrderRequest.setPreOrderValue(-1);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ZERO_DAYS_ERROR))).andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDateSuccessTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 5);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDateWith90DaysSuccessTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = PREORDER_DATE_FORMAT.parse(PREORDER_DATE_FORMAT.format(new Date()));
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 90);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDateWithLessThanCurrentDateTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    preOrderRequest.setPreOrderDate(new Date("01/06/2021"));
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.errorMessage", equalTo(DATE_ERROR)))
        .andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDateWithMoreThan90DaysTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 91);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(PREORDER_DATE_EXCEEDED_LIMIT))).andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderTypeDateWithEqualToCurrentDateTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    Date currentDate = PREORDER_DATE_FORMAT.parse(PREORDER_DATE_FORMAT.format(new Date()));
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    preOrderRequest.setPreOrderDate(cal.getTime());
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.errorMessage", equalTo(DATE_ERROR)))
        .andReturn();
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderFalseTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setIsPreOrder(false);
    product.setPreOrder(preOrderRequest);
    Mockito.when(
        getProductLevel3Service().updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.eq(false), Mockito.eq(null),
            Mockito.eq(false), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(getProductLevel3Service())
        .updateLogistics(productLevel3UpdateRequestArgumentCaptor.capture(), Mockito.eq(false), Mockito.eq(null), Mockito.eq(false),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateLogisticsWithPreOrderAndPreOrderWrongTest() throws Exception {
    ProductLevel3UpdateRequest product = generateProductLevel3UpdateRequest();
    preOrderRequest.setPreOrderType(REVIEW_TYPE);
    preOrderRequest.setPreOrderDate(new Date("01/06/2021"));
    product.setPreOrder(preOrderRequest);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_LOGISTICS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isOnlyExternal", String.valueOf(false)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(WRONG_PREORDER_TYPE))).andReturn();
  }

  @Test
  public void fetchProductSuspensionHistoryTest() throws Exception {
    Mockito.when(productLevel3Service.getProductSuspensionHistoryByProductSkus(DEFAULT_STORE_ID,
        Arrays.asList(DEFAULT_PRODUCT_SKU))).thenReturn(Collections.EMPTY_LIST);
    ProductSkuListRequest productSkuListRequest =
        ProductSkuListRequest.builder().productSkuList(Arrays.asList(DEFAULT_PRODUCT_SKU)).build();
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
        + ProductLevel3ControllerPath.GET_SUSPENSION_NOTES_BY_PRODUCT_SKUS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productSkuListRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service).getProductSuspensionHistoryByProductSkus(DEFAULT_STORE_ID,
        Arrays.asList(DEFAULT_PRODUCT_SKU));
  }

  @Test
  public void fetchProductSuspensionHistory_emptyRequestTest() throws Exception {
    ProductSkuListRequest productSkuListRequest =
        ProductSkuListRequest.builder().productSkuList(Collections.EMPTY_LIST).build();
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
        + ProductLevel3ControllerPath.GET_SUSPENSION_NOTES_BY_PRODUCT_SKUS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productSkuListRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void retryActivationTest() throws Exception {
    Mockito.doNothing().when(productLevel3Service)
        .retrySkipReviewProductActivation(DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_SKU));
    ProductSkuListRequest productSkuListRequest =
        ProductSkuListRequest.builder().productSkuList(Arrays.asList(DEFAULT_PRODUCT_SKU)).build();
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.RETRY_FLOW2_ACTIVATION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productSkuListRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productLevel3Service)
        .retrySkipReviewProductActivation(DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_SKU));
  }

  @Test
  public void updateProductItemViewConfigTest() throws Exception {
    ProductLevel3ViewConfigRequest request = new ProductLevel3ViewConfigRequest();
    request.setBuyable(false);
    request.setDisplay(false);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_ITEM_VIEW_CONFIG.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productLevel3Service)
        .updateProductItemViewConfig(Mockito.any(ProductLevel3ViewConfigStockRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
        Mockito.any())).thenReturn(editProductResponse);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(UpdateProductLevel3InfoRequest.builder().build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoRejectedItemTest() throws Exception {
    editProductResponse.setApiErrorCode(ApiErrorCode.ITEM_IS_REJECTED);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
        Mockito.any())).thenReturn(editProductResponse);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(UpdateProductLevel3InfoRequest.builder().build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoApplicationExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
            Mockito.any()))
        .thenThrow(new ApplicationException(ErrorCategory.INVALID_STATE, ErrorCategory.INVALID_STATE.getMessage()));
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(UpdateProductLevel3InfoRequest.builder().build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoApplicationRuntimeExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
            Mockito.any()))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorCategory.INVALID_STATE.getMessage()));
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(UpdateProductLevel3InfoRequest.builder().build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    preOrderRequest.setIsPreOrder(false);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
        Mockito.any())).thenThrow(new Exception());
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(
                    UpdateProductLevel3InfoRequest.builder().preOrder(preOrderRequest).build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateProductLevel3InfoWithPreOrderTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    Mockito.when(getProductLevel3Service().updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class),
        Mockito.any())).thenReturn(editProductResponse);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(
                    UpdateProductLevel3InfoRequest.builder().preOrder(preOrderRequest).build())))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(getProductLevel3Service())
        .updateProductLevel3Info(Mockito.any(UpdateProductLevel3InfoRequest.class), Mockito.any());
  }

  @Test
  public void updateImagesEmptyBpCodeTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductItems(Arrays.asList(new ItemImageEditRequest()));
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
          .andExpect(MockMvcResultMatchers.status().is5xxServerError());
    });
  }

  @Test
  public void updateImagesEmptyImagePathTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductItems(Arrays.asList(new ItemImageEditRequest()));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
          .andExpect(MockMvcResultMatchers.status().is5xxServerError());
    });
  }

  @Test
  public void updateImagesEmptyProductSkuTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductItems(Arrays.asList(new ItemImageEditRequest()));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
          .andExpect(MockMvcResultMatchers.status().is5xxServerError());
    });
  }

  @Test
  public void updateImagesEmptyItemSkuTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductItems(Arrays.asList(new ItemImageEditRequest()));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    productImageEditRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Assertions.assertThrows(Exception.class, () -> {
      getMockMvc().perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
          .andExpect(MockMvcResultMatchers.status().is5xxServerError());
    });
  }

  @Test
  public void updateImagesEmptyApiErrorTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    ItemImageEditRequest itemImageEditRequest = new ItemImageEditRequest();
    itemImageEditRequest.setItemSku(ITEM_SKU);
    productImageEditRequest.setProductItems(Arrays.asList(itemImageEditRequest));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    productImageEditRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setApiErrorCode(ApiErrorCode.IMAGE_UPDATE_FAILED);

    Mockito.when(productLevel3Service.updateImages(DEFAULT_STORE_ID, productImageEditRequest))
        .thenReturn(itemsPriceStockImagesUpdateResponse);

    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(productLevel3Service).updateImages(DEFAULT_STORE_ID, productImageEditRequest);
  }

  @Test
  public void updateImagesTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    ItemImageEditRequest itemImageEditRequest = new ItemImageEditRequest();
    itemImageEditRequest.setItemSku(ITEM_SKU);
    productImageEditRequest.setProductItems(Arrays.asList(itemImageEditRequest));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    productImageEditRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();

    Mockito.when(productLevel3Service.updateImages(DEFAULT_STORE_ID, productImageEditRequest))
        .thenReturn(itemsPriceStockImagesUpdateResponse);

    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(productLevel3Service).updateImages(DEFAULT_STORE_ID, productImageEditRequest);
  }

  @Test
  public void updateImagesEmptyImagesTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    productImageEditRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();

    Mockito.when(productLevel3Service.updateImages(DEFAULT_STORE_ID, productImageEditRequest))
        .thenReturn(itemsPriceStockImagesUpdateResponse);

    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(productLevel3Service).updateImages(DEFAULT_STORE_ID, productImageEditRequest);
  }

  @Test
  public void updateImagesErrorTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    ItemImageEditRequest itemImageEditRequest = new ItemImageEditRequest();
    itemImageEditRequest.setItemSku(ITEM_SKU);
    productImageEditRequest.setProductItems(Arrays.asList(itemImageEditRequest));
    productImageEditRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productImageEditRequest.setImagePath(IMAGE_PATH);
    productImageEditRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();

    Mockito.when(productLevel3Service.updateImages(DEFAULT_STORE_ID, productImageEditRequest))
        .thenThrow(ApplicationRuntimeException.class);

    URI uri =
        new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.UPDATE_IMAGES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(productImageEditRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());

    verify(productLevel3Service).updateImages(DEFAULT_STORE_ID, productImageEditRequest);
  }

  @Test
  public void testGetInProgressProductsByPickupPointCodeAndBusinessPartnerCode() throws Exception {
    List<InProgressProductsByPickupPointCodeResponse> products = new ArrayList<>();
    InProgressProductsByPickupPointCodeResponse inProgressProductResponsePageResponseByPickuppoint =
        new InProgressProductsByPickupPointCodeResponse();
    inProgressProductResponsePageResponseByPickuppoint.setProductSku(DEFAULT_PRODUCT_SKU);
    inProgressProductResponsePageResponseByPickuppoint.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    inProgressProductResponsePageResponseByPickuppoint.setItemSku(DEFAULT_ITEM_SKU);
    Page<InProgressProductsByPickupPointCodeResponse> page = new PageImpl<>(products);
    Mockito.when(productLevel3Service.getInProgressProductsByBusinessPartnerAndPickupPointCode(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT_CODE, PAGE, 50)).thenReturn(page);
    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L4_BY_PICKUP_POINT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("pickupPointCode", DEFAULT_PICKUP_POINT_CODE).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(DEFAULT_REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", Matchers.equalTo(50)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    verify(productLevel3Service).getInProgressProductsByBusinessPartnerAndPickupPointCode(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT_CODE, PAGE, 50);
  }

  @Test
  public void testGetInProgressProductsByPickupPointCodeAndBusinessPartnerCodeException() throws Exception {
    Mockito.when(productLevel3Service.getInProgressProductsByBusinessPartnerAndPickupPointCode(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT_CODE, PAGE, 50)).thenThrow(RuntimeException.class);

    URI uri = new URIBuilder().setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L4_BY_PICKUP_POINT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("pickupPointCode", DEFAULT_PICKUP_POINT_CODE).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(productLevel3Service).getInProgressProductsByBusinessPartnerAndPickupPointCode(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT_CODE, PAGE, 50);
  }

  @Test
  public void fetchInProgressL3BySizeChartCodeTest() throws Exception {
    List<InProgressProductsBySizeChartCodeResponse> products = new ArrayList<>();
    Page<InProgressProductsBySizeChartCodeResponse> page = new PageImpl<>(products);
    Mockito.when(
        productLevel3Service.getInProgressProductsBySizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE,
            PAGE, 50)).thenReturn(page);
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L3_BY_SIZE_CHART_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("sizeChartCode", SIZE_CHART_CODE).addParameter("page", "0")
        .addParameter("size", "50").build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(productLevel3Service).getInProgressProductsBySizeChartCode(DEFAULT_STORE_ID,
        SIZE_CHART_CODE, PAGE, 50);
  }

  @Test
  public void fetchInProgressL3BySizeChartCodeExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(
        productLevel3Service).getInProgressProductsBySizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE,
            PAGE, 50);
    URI uri = new URIBuilder().setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L3_BY_SIZE_CHART_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("sizeChartCode", SIZE_CHART_CODE).addParameter("page", "0")
        .addParameter("size", "50").build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    verify(productLevel3Service).getInProgressProductsBySizeChartCode(DEFAULT_STORE_ID,
        SIZE_CHART_CODE, PAGE, 50);
  }


}

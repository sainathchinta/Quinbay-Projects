package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;
import static org.hamcrest.Matchers.hasSize;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.verification.VerificationMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ProductApiPath;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.GeneratedProductImagesPathDto;
import com.gdn.x.productcategorybase.dto.GeneratedProductItemImagesPathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImagePathDTO;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductImagesDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemMultipleUpcCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductItemImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleStringListRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.ActivateImage;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.solr.AttributeModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductResponse;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ImageServiceWrapper;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.TrackerService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.service.solr.SolrProductFilterService;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

public class ProductControllerTest {

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String CREATED_BY = "CREATED_BY";
  private static final String UPDATED_BY = "updated-by";
  private static final String PRODUCT_ITEM_NAME = "PRODUCT_ITEM_NAME";
  private static final String PRODUCT_ITEM_NAME_2 = "PRODUCT_ITEM_NAME_2";
  private static final String PRODUCT_ITEM_NAME_SEARCH = "item_name";
  private static final String SKU_CODE_2 = "SKU_CODE_2";
  private static final String UPC_CODE_2 = "UPC_CODE_2";
  private static final String SKU_CODE_1 = "SKU_CODE_1";
  private static final String SOURCE_ITEM_CODE1 = "SOURCE_SKU_CODE_1";
  private static final String UPC_CODE_1 = "UPC_CODE_1";
  private static final String WRONG_SKU_CODE = "WRONG_SKU_CODE";
  private static final String UPC_CODE_SEARCH = "upc_code";
  private static final String ID2 = "ID";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final Double WIDTH = Double.valueOf(98.42);
  private static final Double WEIGHT = Double.valueOf(1.1);
  private static final Double HEIGHT = Double.valueOf(2.2);
  private static final String UOM = "uom";
  private static final String UNIQUE_SELLING_POINT = "unique-selling-point";
  private static final Double SHIPPING_WEIGHT = Double.valueOf(8.45);
  private static final Double LENGTH = Double.valueOf(5.12);
  private static final String BRAND_NAME = "brand-name";
  private static final String CATEGORY_ID = "category-id";
  private static final String CATEGORY_NAME = "category-name";
  private static final String PRODUCT_CODE = "product-code";
  private static final String WRONG_PRODUCT_CODE = "wrong-product-code";
  private static final String PRODUCT_CODE_SEARCH = "-code";
  private static final String ID = "id";
  private static final String NAME = "product-name";
  private static final String HIDDEN_ID = "hidden-id";
  private static final String HIDDEN_NAME = "hidden-product-name";
  private static final String NAME_SEARCH = "-name";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DETAIL_BY_PRODUCT_CODE = "/productCode/" + ProductControllerTest.PRODUCT_CODE;
  private static final String CATEGORY_CODE = "CA-1000010";
  private static final String PRODUCT_CODE_1 = "MTA-0011708";
  private static final String LOCATION_PATH = "LOCATION_PATH";
  private static final String HASH_CODE = "HASH_CODE";
  private static final String DAYS = "10";
  private static final String DAYSPAN = "20";
  private static final String BATCH_SIZE = "1000";
  private static final String BASE_FOLDER = "./src/test/resources/testFolders";
  private static final String NEW_PRODUCT_CODE = "NEW_PRODUCT_CODE";
  private static final String CREATED_MERCHANT = "CREATED_MERCHANT";
  private static final String BRAND_CODE = "brand-code";
  private static final String NEW_BRAND_CODE = "new-brand-code";
  private static final String BUSINESS_PARTNER_CODE = "BP001";
  private static final String DUPLICATE_PRODUCT_CODE_CONSTRAINT = "uk_b89txf3dqb2vuwju5l1nlnf61";

  private static final String DETAIL_BY_SKU_CODE = "/skuCode/" + ProductControllerTest.SKU_CODE_1;
  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final int TOTAL_RECORDS = 1;
  private static final boolean VIEWABLE = false;
  private static final String PRODUCT_ID = "id";
  private static final boolean PROMO_SKU = false;
  private static final Integer PAGE = 10;
  private static final List<AttributeModel> ATTRIBUTES = new ArrayList<>();
  private static final String DEFAULT_DATE = "01-01-2016";
  private final Logger LOG = LoggerFactory.getLogger(ProductController.class);
  private static final VerificationMode NEVER_CALLED = times(0);
  private static final List<String> PRODUCT_CODE_LISTS = ImmutableList.of(PRODUCT_CODE_1);
  private static final String DEFAULT_CHANNEL_ID = "channelId";
  private static final String DEFAULT_CLIENT_ID = "clientId";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final Integer DANGEROUS_GOODS_LEVEL = 1;
  private static final String VALUE = "value";
  private static final int PRODUCT_SCORE_BATCH_SIZE = 1000;
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "PREDEFINED_ALLOWED_ATTRIBUTE_CODE";
  private static final String VIDEO_NAME = "VIDEO_NAME_1";

  @Mock
  private ProductService service;

  @Mock
  private ProductItemService productItemService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private Page<Product> page;

  @Mock
  private Page<String> page1;

  @Mock
  private Page<Object[]> pageOfObject;

  @Mock
  private Page<Product> pageHidden;

  @Mock
  private Page<ProductItem> productItemPage;

  @InjectMocks
  private ProductController controller;

  @Mock
  private AllowedAttributeValueService allowedAttributeValueService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private SolrProductFilterService solrProductFilterService;

  @Mock
  private ImageService imageService;

  @Mock
  private ImageServiceWrapper imageServiceWrapper;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private Product productObject;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Mock
  private TrackerService trackerService;

  @Captor
  private ArgumentCaptor<SimpleMasterProductUpdateDTO> simpleMasterProductUpdateDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemImageRequest> productAndItemImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItem>> productItemsListArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductItemImageUpdateRequest> productItemImageUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemUpcCodeUpdateRequest>> productItemUpcCodeUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductBrandUpdateDTO> productBrandUpdateDTOArgumentCaptor;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ObjectMapper objectMapperMock;

  private MockMvc mockMvc;
  private Product product, sentProduct, hiddenProduct;
  private List<Product> products, hiddenProducts;
  private ProductItem productItem;
  private List<ProductItem> productItems;
  private Pageable pageable;
  private ProductRequest productRequest;
  private String jsonReq;
  private Category category;
  private UPCCodeSearchRequest upcCodeSearchRequest;
  private ActivateImageRequest activateImageRequest;
  private ProductAndItemImageRequest productAndItemImageRequest;
  private ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest;
  private ActivateImage activateImage;
  private List<String> filenames;
  private List<ProductImageDTO> imagesDto;
  private ProductItem originalImageProductItem;
  private ObjectMapper objectMapper = new ObjectMapper();
  private SimpleItemDetailResponse simpleItemDetailResponse;
  private List<String> productCodes;
  private List<Product> productList;
  private List<ProductAttributeRequest> productAttributes;
  private List<ProductItemRequest> productItemsRequest;
  private ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest;
  private Product product1;

  @Test
  public void getDuplicateProductItemsOrByUpcCodeTest_lessResultsFromSolr_getDataFromDB() throws
      Exception {
    when(this.solrProductFilterService.filterDuplicateProducts(any(SolrProductModel.class), eq(10)))
        .thenReturn(new HashSet<SolrProductResponse>());
    List<AttributeModel> modelList = new ArrayList<>();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("upcCode", ProductControllerTest.UPC_CODE_1)
        .param("productName", ProductControllerTest.PRODUCT_NAME)
        .param("finalCategoryId", ProductControllerTest.CATEGORY_ID)
    ).andExpect(status().isOk());
    verify(this.solrProductFilterService).filterDuplicateProducts(any(SolrProductModel.class), eq(10));
  }

  @Test
  public void initTest() {
    controller.init();
  }

  @Test
  public void getDuplicateProductItemsOrByUpcCodeTest_lessResultsFromSolr_getDataFromDB_SizeSmallerThan5() throws
          Exception {
    Set<SolrProductResponse> responses=getMockResponses(4);
    when(this.solrProductFilterService.filterDuplicateProducts(any(SolrProductModel.class), eq(10)))
            .thenReturn(responses);
    List<AttributeModel> modelList = new ArrayList<>();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", ProductControllerTest.UPC_CODE_1)
            .param("productName", ProductControllerTest.PRODUCT_NAME)
            .param("size", "10")
            .param("finalCategoryId", ProductControllerTest.CATEGORY_ID)
    ).andExpect(status().isOk());
    verify(this.solrProductFilterService).filterDuplicateProducts(any(SolrProductModel.class), eq(10));
  }

  @Test
  public void getDuplicateProductItemsOrByUpcCode_solrResultsExist_return() throws Exception {
    Set<SolrProductResponse> responses=getMockResponses(6);
    when(this.solrProductFilterService.filterDuplicateProducts(any(SolrProductModel.class), eq(10)))
        .thenReturn(responses);
    List<AttributeModel> modelList = new ArrayList<>();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("upcCode", ProductControllerTest.UPC_CODE_1)
        .param("productName", ProductControllerTest.PRODUCT_NAME)
        .param("finalCategoryId", ProductControllerTest.CATEGORY_ID)
    ).andExpect(status().isOk());
    verify(this.solrProductFilterService).filterDuplicateProducts(any(SolrProductModel.class), eq(10));
  }

  @Test
  public void getDuplicateProductItemsOrByUpcCode_solrResultsExist_return_sizeLessThen5_sizeNotSame() throws Exception {
    Set<SolrProductResponse> responses=getMockResponses(4);
    when(this.solrProductFilterService.filterDuplicateProducts(any(SolrProductModel.class), eq(10)))
        .thenReturn(responses);
    List<AttributeModel> modelList = new ArrayList<>();
    modelList.add(new AttributeModel());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("upcCode", ProductControllerTest.UPC_CODE_1)
        .param("productName", ProductControllerTest.PRODUCT_NAME)
        .param("finalCategoryId", ProductControllerTest.CATEGORY_ID)
        .param("size", "10")
        .content(requestString)
    ).andExpect(status().isOk());
    verify(this.solrProductFilterService).filterDuplicateProducts(any(SolrProductModel.class), eq(10));
  }

  @Test
  public void getDuplicateProductItemsOrByUpcCodeTest_noFinalCategory() throws
          Exception {

    when(this.solrProductFilterService.filterDuplicateProducts(any(SolrProductModel.class), eq(10)))
            .thenReturn(new HashSet<SolrProductResponse>());
    List<AttributeModel> modelList = new ArrayList<>();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", ProductControllerTest.UPC_CODE_1)
            .param("productName", ProductControllerTest.PRODUCT_NAME)
            .param("finalCategoryId", "")
    ).andExpect(status().isOk());
  }

  @Test
  public void getDuplicateProductItemsOrByUpcCodeTest_Exception() throws
          Exception {
    List<AttributeModel> modelList = new ArrayList<>();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(modelList);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", ProductControllerTest.UPC_CODE_1)
            .param("productName", ProductControllerTest.PRODUCT_NAME)
            .param("finalCategoryId", "")
    ).andExpect(status().isOk());
  }

  private Set<SolrProductResponse> getMockResponses(int size) {
    Set<SolrProductResponse> responseSet = new HashSet<>();
    for (int i = 0; i < size; i++) {
      SolrProductResponse response=new SolrProductResponse();
      response.setProductCode(String.valueOf(i));
      response.setProductName(String.valueOf(i));
      responseSet.add(response);
    }
    return responseSet;
  }


  @Test
  public void activateProductTest() throws Exception {
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.ACTIVATE).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(productServiceWrapper, Mockito.times(1)).activateProduct(ProductControllerTest.STORE_ID, this.sentProduct.getId());
  }

  @Test
  public void activateProductWithEmptyCreatedByTest() throws Exception {
    this.productRequest.setCreatedBy(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.ACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void activateProductWithEmptyCreatedDateTest() throws Exception {
    this.productRequest.setCreatedDate(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.ACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void activateProductWithEmptyIdTest() throws Exception {
    this.productRequest.setId(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.ACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void checkProductByProductCodeTest() throws Exception {
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_BY_PRODUCT_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("productCode", ProductControllerTest.PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_BY_PRODUCT_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("productCode", ProductControllerTest.WRONG_PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service, Mockito.times(1)).countByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE);
    verify(this.service, Mockito.times(1)).countByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.WRONG_PRODUCT_CODE);
  }

  @Test
  public void checkProductItemBySkuCodeTest() throws Exception {
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_ITEM_BY_SKU_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("skuCode", ProductControllerTest.SKU_CODE_1)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_ITEM_BY_SKU_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("skuCode", ProductControllerTest.WRONG_SKU_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.productItemService, Mockito.times(1)).countBySkuCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.SKU_CODE_1);
    verify(this.productItemService, Mockito.times(1)).countBySkuCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.WRONG_SKU_CODE);
  }

  @Test
  public void createProductTest() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc
        .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).save(productArgumentCaptor.capture());
    Mockito.verify(this.categoryService, Mockito.times(1))
        .findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertNotEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getProductAttributes().size(),
        productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void createProductForDifferentImageLocationTest() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(PRODUCT_CODE_1 + File.separator + "image.jpg");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service, Mockito.times(1)).save(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductWithNoMainImagesTest() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    image.setSequence(0);
    image.setMainImages(false);
    image.setId(STORE_ID);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).save(productArgumentCaptor.capture());
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertNotEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getProductAttributes().size(),
        productArgumentCaptor.getValue().getProductAttributes().size());
    assertTrue(productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
    FileUtils.deleteDirectory(mainDirectory);
  }

  private void setImagesAndDefiningAttributes(Product product) {
    for (int i=0; i< product.getProductItems().size(); i++) {
      product.getProductItems().get(i).getProductItemImages().add(new ProductItemImage(productItem, Boolean.TRUE, product.getProductCode() + File.separator + "image.jpg", 1));
      product.getProductItems().get(i).getProductItemImages().add(new ProductItemImage(productItem, Boolean.FALSE, product.getProductCode() + File.separator + "image1.jpg", 2));
      product.getProductItems().get(i).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
      product.getProductItems().get(i).getProductItemAttributeValues().add(new ProductItemAttributeValue());
      product.getProductItems().get(i).getProductItemAttributeValues().get(0).setMarkForDelete(false);
      product.getProductItems().get(i).getProductItemAttributeValues().get(0).setValue(ATTRIBUTE_VALUE+i);
      Attribute attribute = new Attribute("Attribute-name",
          com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE, true, ProductControllerTest.STORE_ID);
      attribute.setAttributeCode(ATTRIBUTE_CODE+i);
      product.getProductItems().get(i).getProductItemAttributeValues().get(0).setAttribute(attribute);
    }
  }

  private void setImagesAndDefiningAttributes(ProductRequest productRequest) {

    for (int i=0; i< product.getProductItems().size(); i++) {
      TreeMap<String, String> attributesMap = new TreeMap<>();
      attributesMap.put(ATTRIBUTE_CODE+i, ATTRIBUTE_VALUE+i);
      productRequest.getProductItems().get(i).getImages().add(new Image(Boolean.TRUE, productRequest.getProductCode() + File.separator + "image.jpg", 1));
      productRequest.getProductItems().get(i).getImages().add(new Image(Boolean.FALSE, productRequest.getProductCode() + File.separator + "image1.jpg", 2));
      productRequest.getProductItems().get(i).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValueRequest>());
      productRequest.getProductItems().get(i).setAttributesMap(attributesMap);
    }
  }

  @Test
  public void createProductTestWithProductImages() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image3.jpg");
    file1.createNewFile();
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(productRequest.getProductCode() + File.separator + "image3.jpg");
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    when(this.service.saveProduct(any(Product.class), eq(new ArrayList<>()), eq(true))).thenReturn(product);
    Mockito.when(fileStorageService.updateLocationForImages(any(), any(), any(),
        any(), Mockito.anySet(), any())).thenThrow(new RuntimeException());
    doNothing().when(this.service).publishVatUpdateEvent(productItemsListArgumentCaptor.capture());
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductTestWithProductImages_aiGeneratedFields() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image3.jpg");
    file1.createNewFile();
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.productRequest.setAiGeneratedFieldsResponse(new AiGeneratedFieldsResponse(true, true));
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(productRequest.getProductCode() + File.separator + "image3.jpg");
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    when(this.service.saveProduct(any(Product.class), eq(new ArrayList<>()), eq(true))).thenReturn(product);
    Mockito.when(fileStorageService.updateLocationForImages(any(), any(), any(),
        any(), Mockito.anySet(), any())).thenThrow(new RuntimeException());
    doNothing().when(this.service).publishVatUpdateEvent(productItemsListArgumentCaptor.capture());
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
        , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
        anyList(), anyBoolean(), anyString());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(objectMapperMock).writeValueAsString(any());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductTestWithProductImages_Exception() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image3.jpg");
    file1.createNewFile();
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(productRequest.getProductCode() + File.separator + "image3.jpg");
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    when(this.service.saveProduct(any(), any(), any(Boolean.class))).thenThrow(RuntimeException.class);
    Mockito.when(fileStorageService.updateLocationForImages(any(), anyString(), anyString(),
        anyString(), Mockito.anySet(), any())).thenThrow(new RuntimeException());
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
    verify(this.service).saveProduct(any(), any(), any(Boolean.class));
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductTestWithProductImages_ApplicationException() throws Exception {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image3.jpg");
    file1.createNewFile();
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(productRequest.getProductCode() + File.separator + "image3.jpg");
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    doThrow(RuntimeException.class).when(this.service).saveProduct(any(Product.class), eq(new ArrayList<>()),  eq(true));
    Mockito.when(fileStorageService.updateLocationForImages(any(), anyString(), anyString(),
        anyString(), Mockito.anySet(), any())).thenThrow(new ApplicationRuntimeException());
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
    verify(this.service).saveProduct(any(Product.class), eq(new ArrayList<>()),  eq(true));
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductTestWithDuplicateProductCode() throws Exception {
    ReflectionTestUtils.setField(controller, "ranchIntegrationEnabled", true);
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image3.jpg");
    file1.createNewFile();
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(productRequest.getProductCode() + File.separator + "image3.jpg");
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    Exception exception = new Exception(DUPLICATE_PRODUCT_CODE_CONSTRAINT);
    doThrow(exception).when(this.service).saveProduct(any(Product.class), eq(new ArrayList<>()),  eq(true));
    Mockito.when(fileStorageService.updateLocationForImages(any(), anyString(), anyString(),
        anyString(), Mockito.anySet(), any())).thenThrow(new ApplicationRuntimeException());
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    HashMap<String, String> hashMap = new HashMap<>();
    hashMap.put(PRODUCT_CODE, PRODUCT_CODE);
    productRequest.setDistributionInfoRequest(hashMap);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(objectMapperMock).writeValueAsString(productRequest.getDistributionInfoRequest());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
    verify(this.service).saveProduct(any(Product.class), eq(new ArrayList<>()),  eq(true));
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void createProductTestWithProductImagesWithoutProductCode() throws Exception {
    ReflectionTestUtils.setField(controller, "fullImageSourceDirectory", BASE_FOLDER);
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(LOCATION_PATH + 1);
    productImage.setMainImages(Boolean.TRUE);
    productImage.setActive(true);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    productRequest.setProductCode(StringUtils.EMPTY);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    Mockito.when(fileStorageService.updateLocationForImages(any(), anyString(), anyString(),
        anyString(), Mockito.anySet(), any())).thenThrow(new RuntimeException());
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
              .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
              .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
      Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
      verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
        , anyList());
      verify(attributeService, times(1)).getAttributeAndValueMap(anyString(),anySet(), anySet(),
        anySet(),
        anyList(), anyBoolean(), anyString());
      verify(attributeService, Mockito.times(6))
          .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
      verify(trackerService).trackProductCreationFailure(eq(REQUEST_ID), any(ProductRequest.class), any());
    }
  }

  @Test
  public void createProductTestWithoutProductImages() throws Exception {
    this.productRequest.getProductItems().get(0).setStoreId("");
    setImagesAndDefiningAttributes(this.productRequest);
    this.product.setStoreId(null);
    setImagesAndDefiningAttributes(this.product);
    Attribute attribute = new Attribute();
    attribute.setVariantCreation(true);
    when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(product);
    when(this.service.saveProduct(any(Product.class), eq(new ArrayList<>()), eq(true))).thenReturn(product);
    when(this.attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(attribute);
    Mockito.when(fileStorageService.updateLocationForImages(any(), any(), any(),
        any(), Mockito.anySet(), any())).thenThrow(new RuntimeException());
    doNothing().when(this.service).publishVatUpdateEvent(productItemsListArgumentCaptor.capture());
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.service).sortAndGenerateProductItem(any(Product.class));
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(attributeService, Mockito.times(6))
        .getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(trackerService).trackProductCreationFailure(any(), any(ProductRequest.class), any());
  }

  @Test
  public void createProductTestWithEmptyStoreId() throws Exception {
    this.LOG.debug("Perform CreateProductTest with json = " + this.jsonReq);
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.getImages().add(image);
    this.productRequest.setStoreId("");
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).save(productArgumentCaptor.capture());
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());

  }

  @Test
  public void createProductWithEmptyCreatedByTest() throws Exception {
    this.productRequest.setCreatedBy(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createProductWithEmptyCreatedDateTest() throws Exception {
    this.productRequest.setCreatedDate(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createProductWithEmptyProductCategoriesTest() throws Exception {
    this.productRequest.setProductCategories(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_PRODUCT_CATEGORY_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createProductWithSpecificationDetailGeneratedTest() throws Exception {
    this.LOG.debug("Perform CreateProductTest with json = " + this.jsonReq);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM)
        .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).saveProductWithSpecificationDetailGenaratedBySystem(productArgumentCaptor.capture());
    Mockito.verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getProductAttributes().size(),
        productArgumentCaptor.getValue().getProductAttributes().size());
  }

  @Test
  public void createProductWithSpecificationDetailGeneratedWithEmptyCreatedByTest() throws Exception {
    this.productRequest.setCreatedBy(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM)
          .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createProductWithSpecificationDetailGeneratedWithEmptyCreatedDateTest() throws Exception {
    this.productRequest.setCreatedDate(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM)
          .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createProductWithSpecificationDetailGeneratedWithEmptyProductCategoriesTest() throws Exception {
    this.productRequest.setProductCategories(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM)
          .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_PRODUCT_CATEGORY_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void deactivateProductTest() throws Exception {
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.DEACTIVATE).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(productServiceWrapper, Mockito.times(1)).deactivateProduct(ProductControllerTest.STORE_ID, this.sentProduct.getId());
  }

  @Test
  public void deactivateProductWithEmptyCreatedByTest() throws Exception {
    this.productRequest.setCreatedBy(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DEACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }


  @Test
  public void deactivateProductWithEmptyCreatedDateTest() throws Exception {
    this.productRequest.setCreatedDate(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DEACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void deactivateProductWithEmptyIdTest() throws Exception {
    this.productRequest.setId(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DEACTIVATE).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void discardProductTest() throws Exception {
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.DISCARD).contentType(MediaType.APPLICATION_JSON)
        .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.productServiceWrapper, Mockito.times(1)).markForDeleteProductAndEvictCache(
        ProductControllerTest.STORE_ID, this.sentProduct.getId());
  }

  @Test
  public void discardProductWithEmptyCreatedByTest() throws Exception {
    this.productRequest.setCreatedBy(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DISCARD).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void discardProductWithEmptyCreatedDateTest() throws Exception {
    this.productRequest.setCreatedDate(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DISCARD).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void discardProductWithEmptyIdTest() throws Exception {
    this.productRequest.setId(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.DISCARD).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void getProductByBrandTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BRAND)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("brandName", ProductControllerTest.BRAND_NAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByBrandLike(ProductControllerTest.STORE_ID, ProductControllerTest.BRAND_NAME,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  void getProductByBrandNameTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_CODE_BY_BRAND)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("brandName", ProductControllerTest.BRAND_NAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].id",equalTo(ProductControllerTest.ID)));
    verify(this.service).findByStoreIdAndBrandName(ProductControllerTest.STORE_ID,
        ProductControllerTest.BRAND_NAME, this.pageable);
  }

  @Test
  public void getProductByCategoriesTest() throws Exception {
    List<String> categoriesCode = new ArrayList<String>();
    categoriesCode.add(CATEGORY_CODE);
    CategoryMultipleIdRequest request = new CategoryMultipleIdRequest();
    request.setCategoryCode(categoriesCode);

    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    when(this.service.findByStoreIdAndCategoriesCode(STORE_ID, categoriesCode, this.pageable)).thenReturn(this.page);
    this.mockMvc
        .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_CATEGORIES)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
            .andExpect(status().isOk());

    verify(this.service).findByStoreIdAndCategoriesCode(STORE_ID, categoriesCode, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }



  @Test
  public void getProductByCategoryTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_CATEGORY)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("categoryId", ProductControllerTest.CATEGORY_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByCategoryId(ProductControllerTest.STORE_ID, ProductControllerTest.CATEGORY_ID,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByNameAndCreatedByTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_CREATED_BY)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("name", ProductControllerTest.NAME_SEARCH).param("createdBy", ProductControllerTest.CREATED_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)))
        .andExpect(jsonPath("$.content[0].createdBy", equalTo(ProductControllerTest.CREATED_BY)));
    verify(this.service).findByNameAndCreatedBy(ProductControllerTest.STORE_ID, ProductControllerTest.NAME_SEARCH,
        ProductControllerTest.CREATED_BY, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByNameAndViewableAndActivatedAndUpdatedByTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED_UPDATED_BY)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("name", ProductControllerTest.PRODUCT_NAME).param("viewable", Boolean.TRUE.toString())
        .param("activated", Boolean.TRUE.toString()).param("updatedBy", ProductControllerTest.UPDATED_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)));
    verify(this.service).findByNameAndViewableAndActivatedAndUpdatedBy(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_NAME, true, true, ProductControllerTest.UPDATED_BY, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByNameAndViewableAndActivatedTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("name", ProductControllerTest.PRODUCT_NAME).param("viewable", Boolean.TRUE.toString())
        .param("activated", Boolean.TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByNameAndViewableAndActivated(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_NAME, true, true, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByNameTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("name", ProductControllerTest.NAME_SEARCH)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByName(ProductControllerTest.STORE_ID, ProductControllerTest.NAME_SEARCH, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByProductCodeTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("productCode", ProductControllerTest.PRODUCT_CODE_SEARCH)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_SEARCH,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByShippingWeightBiggerOrEqualTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_SHIPPING_WEIGHT_BIGGER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("shippingWeight", ProductControllerTest.SHIPPING_WEIGHT.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByShippingWeightBiggerOrEqualThan(ProductControllerTest.STORE_ID,
        ProductControllerTest.SHIPPING_WEIGHT, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByShippingWeightLesserOrEqualTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_SHIPPING_WEIGHT_LESSER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("shippingWeight", ProductControllerTest.SHIPPING_WEIGHT.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByShippingWeightLesserOrEqualThan(ProductControllerTest.STORE_ID,
        ProductControllerTest.SHIPPING_WEIGHT, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByUniqueSellingPointTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_UNIQUE_SELLING_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("uniqueSellingCode", ProductControllerTest.UNIQUE_SELLING_POINT)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByUniqueSellingCodeLike(ProductControllerTest.STORE_ID,
        ProductControllerTest.UNIQUE_SELLING_POINT, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByViewableAndActivatedTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_VIEWABLE_ACTIVATED)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("viewable", Boolean.TRUE.toString())
        .param("activated", Boolean.TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].id", equalTo(ProductControllerTest.ID)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)));
    verify(this.service).findByViewableAndActivated(ProductControllerTest.STORE_ID, true, true, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByViewableFalseTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_VIEWABLE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("viewable", Boolean.FALSE.toString()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].id", equalTo(ProductControllerTest.HIDDEN_ID)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.HIDDEN_NAME)));
    verify(this.service).findByViewable(ProductControllerTest.STORE_ID, false, this.pageable);
    verify(this.pageHidden).getContent();
    verify(this.pageHidden).getTotalElements();
  }

  @Test
  public void getProductByViewableTrueTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_VIEWABLE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("viewable", Boolean.TRUE.toString()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByViewable(ProductControllerTest.STORE_ID, true, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByWeightBiggerOrEqualTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_WEIGHT_BIGGER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("weight", ProductControllerTest.WEIGHT.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByWeightBiggerOrEqualThan(ProductControllerTest.STORE_ID, ProductControllerTest.WEIGHT,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductByWeightLesserOrEqualTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_WEIGHT_LESSER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("weight", ProductControllerTest.WEIGHT.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByWeightLesserOrEqualThan(ProductControllerTest.STORE_ID, ProductControllerTest.WEIGHT,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductDetailByProductCodeTest() throws Exception {
    ProductImage productImage = new ProductImage(this.product, false, "location", 1, ProductControllerTest.STORE_ID);
    productImage.setOriginalImage(false);
    ProductImage productImage1 = new ProductImage(this.product, false, "location", 0, ProductControllerTest.STORE_ID);
    productImage1.setOriginalImage(false);
    this.product.getProductImages().add(productImage);
    this.product.getProductImages().add(productImage1);
    this.product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setOriginalImage(true);
    image1.setSequence(0);
    image1.setOriginalImage(false);
    this.product.getProductItems().get(0).getProductItemImages().add(image);
    this.product.getProductItems().get(0).getProductItemImages().add(image1);

    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(category));
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_PRODUCT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)))
        .andExpect(jsonPath("$.value.categoryCodes", contains(ProductControllerTest.CATEGORY_CODE)));
    verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_CODE);
    assertEquals(this.product.getProductImages().get(0).getSequence(),Integer.valueOf(0));
    assertEquals(this.product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),Integer.valueOf(0));
  }

  @Test
  public void getProductDetailByProductCodeWithNullCategory() throws Exception {
    ProductImage productImage = new ProductImage(this.product, false, "location", 1, ProductControllerTest.STORE_ID);
    productImage.setOriginalImage(false);
    ProductImage productImage1 = new ProductImage(this.product, false, "location", 0, ProductControllerTest.STORE_ID);
    productImage1.setOriginalImage(false);
    this.product.getProductImages().add(productImage);
    this.product.getProductImages().add(productImage1);
    this.product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setOriginalImage(true);
    image1.setSequence(0);
    image1.setOriginalImage(false);
    this.product.getProductItems().get(0).getProductItemImages().add(image);
    this.product.getProductItems().get(0).getProductItemImages().add(image1);

    ProductCategory prdCat = new ProductCategory();
    prdCat.setMarkForDelete(true);
    this.product.setProductCategories(Arrays.asList(prdCat));
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_PRODUCT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

        verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
                ProductControllerTest.PRODUCT_CODE);
        assertEquals(this.product.getProductImages().get(0).getSequence(),Integer.valueOf(0));
        assertEquals(this.product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),Integer.valueOf(0));
  }

  @Test
  public void getProductDetailByProductCodeInAllProductTest() throws Exception {

    ProductImage productImage = new ProductImage(this.product, false, "location", 1, ProductControllerTest.STORE_ID);
    productImage.setOriginalImage(false);
    ProductImage productImage1 = new ProductImage(this.product, false, "location", 0, ProductControllerTest.STORE_ID);
    productImage1.setOriginalImage(false);
    this.product.getProductImages().add(productImage);
    this.product.getProductImages().add(productImage1);
    this.product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setOriginalImage(true);
    image1.setSequence(0);
    image1.setOriginalImage(false);
    this.product.getProductItems().get(0).getProductItemImages().add(image);
    this.product.getProductItems().get(0).getProductItemImages().add(image1);

    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_PRODUCT_CODE)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("inAllProducts", "true")
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));

    verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_CODE);
    assertEquals(this.product.getProductImages().get(0).getSequence(),Integer.valueOf(0));
    assertEquals(this.product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),Integer.valueOf(0));
  }

  @Test
  public void getProductDetailByProductCode_ProductImageNullTest() throws Exception {

    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    this.mockMvc
            .perform(
                    get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_PRODUCT_CODE)
                            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                            .param("clientId", ProductControllerTest.CLIENT_ID)
                            .param("requestId", ProductControllerTest.REQUEST_ID)
                            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(true)))
            .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
            .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
            .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
            .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
            .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
            .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
            .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
            .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
            .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));

    verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void getProductDetailByProductCodeWithCatrgorieEmptyTest() throws Exception {
    this.product.getProductCategories().get(0).setCategory(this.category);
    this.product.getProductCategories().get(0).getCategory().setCategoryCode(CATEGORY_CODE);
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    List<Category> value = new ArrayList<Category>();
    value.add(new Category());
    when(this.categoryService.findCategoryHierarchyByCategoryCode(STORE_ID,
        CATEGORY_CODE)).thenReturn(value );
        this.mockMvc
  .perform(
      get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_PRODUCT_CODE)
      .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
      .param("clientId", ProductControllerTest.CLIENT_ID)
      .param("requestId", ProductControllerTest.REQUEST_ID)
      .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
      .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
      .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
      .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
      .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
      .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
      .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
      .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
      .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));
      verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
          ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
      verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void getProductDetailByProductCodeReplaceCategoryTest() throws Exception {
    ProductImage productImage = new ProductImage(this.product, false, "location", 1, ProductControllerTest.STORE_ID);
    productImage.setOriginalImage(false);
    ProductImage productImage1 = new ProductImage(this.product, false, "location", 0, ProductControllerTest.STORE_ID);
    productImage1.setOriginalImage(false);
    this.product.getProductImages().add(productImage);
    this.product.getProductImages().add(productImage1);
    this.product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setOriginalImage(true);
    image1.setSequence(0);
    image1.setOriginalImage(false);
    this.product.getProductItems().get(0).getProductItemImages().add(image);
    this.product.getProductItems().get(0).getProductItemImages().add(image1);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(category);
    product.setProductCategories(Arrays.asList(productCategory));
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    when(this.categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(Arrays.asList(category));
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL_BY_PRODUCT_CODE_REPLACE_CATEGORY_INFO, PRODUCT_CODE,
                CATEGORY_CODE)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));

    verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_CODE);
    assertEquals(this.product.getProductImages().get(0).getSequence(),Integer.valueOf(0));
    assertEquals(this.product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),Integer.valueOf(0));
  }

  @Test
  public void getProductDetailByProductCodeReplaceCategoryWithDifferentCategoryCodeTest() throws Exception {
    ProductImage productImage = new ProductImage(this.product, false, "location", 1, ProductControllerTest.STORE_ID);
    productImage.setOriginalImage(false);
    ProductImage productImage1 = new ProductImage(this.product, false, "location", 0, ProductControllerTest.STORE_ID);
    productImage1.setOriginalImage(false);
    this.product.getProductImages().add(productImage);
    this.product.getProductImages().add(productImage1);
    this.product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setOriginalImage(true);
    image1.setSequence(0);
    image1.setOriginalImage(false);
    this.product.getProductItems().get(0).getProductItemImages().add(image);
    this.product.getProductItems().get(0).getProductItemImages().add(image1);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(category);
    product.setProductCategories(Arrays.asList(productCategory));
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    when(this.categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_NAME)).thenReturn(Arrays.asList(category));
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL_BY_PRODUCT_CODE_REPLACE_CATEGORY_INFO, PRODUCT_CODE,
                CATEGORY_NAME)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));

    verify(this.productServiceWrapper).getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(ProductControllerTest.STORE_ID, CATEGORY_NAME);
    assertEquals(this.product.getProductImages().get(0).getSequence(),Integer.valueOf(0));
    assertEquals(this.product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),Integer.valueOf(0));
  }

  @Test
  public void getProductItemAttrValuesTest() throws Exception {
    this.productItems.get(0).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(new Attribute(BRAND_NAME, null, true, STORE_ID));;
    when(this.productItemService.findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE))
        .thenReturn(this.productItems);
    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_ITEM_ATTR_VALUES)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("productId", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productItemService).findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductItemAttrValueDetailsTest() throws Exception {
    this.productItems.get(0).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(new Attribute(BRAND_NAME, null, true, STORE_ID));;
    when(this.productItemService.findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE))
        .thenReturn(this.productItems);
    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_ITEM_ATTR_VALUE_DETAIL)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("productId", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productItemService).findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE);
  }


  @Test
  public void getProductItemByMultipleUpcCodeTest() throws Exception {
    ProductItemMultipleUpcCodesRequest request = new ProductItemMultipleUpcCodesRequest();
    request.setUpcCodes(new ArrayList<String>());
    request.getUpcCodes().add(UPC_CODE_1);
//    request.setSkuCode(SKU_CODE_1);

    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    when(this.productItemService.findByMultipleUpcCode(eq(STORE_ID), any(List.class), any(Pageable.class))).thenReturn(this.productItemPage);
    this.productItems.get(0).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(new Attribute(BRAND_NAME, null, true, STORE_ID));;
    when(this.productItemService.findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE))
        .thenReturn(this.productItems);
    this.mockMvc
        .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_MULTIPLE_UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productItemService).findByMultipleUpcCode(eq(STORE_ID), any(List.class), any(Pageable.class));
  }

  @Test
  public void getProductItemByMultipleUpcCodeWithNonEmptySkuCodeTest() throws Exception {
    ProductItemMultipleUpcCodesRequest request = new ProductItemMultipleUpcCodesRequest();
    request.setUpcCodes(new ArrayList<String>());
    request.getUpcCodes().add(UPC_CODE_1);
    request.setSkuCode(SKU_CODE_1);

    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    when(this.productItemService.findByMultipleUpcCodeExcludeOneItem(eq(STORE_ID), any(List.class), eq(SKU_CODE_1), any(Pageable.class))).thenReturn(this.productItemPage);
    this.productItems.get(0).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(new Attribute(BRAND_NAME, null, true, STORE_ID));;
    when(this.productItemService.findProductItemByProductIdInitAttrValue(STORE_ID, PRODUCT_CODE))
        .thenReturn(this.productItems);
    this.mockMvc
        .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_MULTIPLE_UPC_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productItemService).findByMultipleUpcCodeExcludeOneItem(eq(STORE_ID), any(List.class), eq(SKU_CODE_1), any(Pageable.class));
  }

  @Test
  public void getProductDetailTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + "/" + ProductControllerTest.ID)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.value.length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.value.width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.value.weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.value.shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.value.brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.value.uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.value.uom", equalTo(ProductControllerTest.UOM)));
    verify(this.productServiceWrapper).getCompleteProductDetailByProductIdAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
  }

  @Test
  @Disabled
  public void getProductItemByProductItetrueNameOrUpcCodeUsingProductItemNameTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_OR_UPC_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("itemNameOrUpcCode", ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH)
        .param("viewable", Boolean.FALSE.toString()).param("isOnlyExternal", Boolean.TRUE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(
            jsonPath("$.content[0].generatedItemName",
                containsString(ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH.toUpperCase())))
                .andExpect(jsonPath("$.content[0].viewable", equalTo(Boolean.FALSE)));

    verify(this.productItemService).findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
        ProductControllerTest.STORE_ID, false, true, ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH,
        this.pageable);

    verify(this.productItemPage).getContent();
    verify(this.productItemPage).getTotalElements();
  }


  @Disabled
  @Test
  public void getProductItemByProductItemNameOrUpcCodeUsingUpcCodeTest() throws Exception {
    this.productItemPage.getContent().get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setMainImages(false);
    image1.setMainImages(true);
    this.productItemPage.getContent().get(0).getProductItemImages().add(image);
    this.productItemPage.getContent().get(0).getProductItemImages().add(image1);
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_OR_UPC_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("itemNameOrUpcCode", ProductControllerTest.UPC_CODE_SEARCH)
        .param("viewable", Boolean.FALSE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(
            jsonPath("$.content[0].upcCode", containsString(ProductControllerTest.UPC_CODE_SEARCH.toUpperCase())))
            .andExpect(jsonPath("$.content[0].viewable", equalTo(Boolean.FALSE)));

    verify(this.productItemService).findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
        ProductControllerTest.STORE_ID, false, true, ProductControllerTest.UPC_CODE_SEARCH, this
            .pageable);

    verify(this.productItemPage, times(4)).getContent();
    verify(this.productItemPage).getTotalElements();

  }

  @Test
  public void getProductItemByProductItemNameTest() throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("productItemName", ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].generatedItemName", equalTo(ProductControllerTest.PRODUCT_ITEM_NAME)));
    verify(this.productItemService).findByStoreIdAndGeneratedItemName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH, this.pageable);
    verify(this.productItemPage).getContent();
    verify(this.productItemPage).getTotalElements();
  }

  @Test
  public void getProductItemByProductItemNameAndCategoryIdTest() throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    when(
        this.productItemService.findByStoreIdAndGeneratedItemNameAndCategoryId(anyString(),
            anyString(), anyString(), (Pageable) Mockito.any())).thenReturn(
        this.productItemPage);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_CATEGORY_ID)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID)
            .param("categoryId", ProductControllerTest.CATEGORY_ID)
            .param("productItemName", ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH)).andExpect(
        status().isOk());
    verify(this.productItemService).findByStoreIdAndGeneratedItemNameAndCategoryId(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH,
        ProductControllerTest.CATEGORY_ID, this.pageable);
  }

  @Test
  public void getProductItemByNameAndCategoryIdTest() throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    when(this.productItemService
        .findByStoreIdAndGeneratedItemNameAndCategoryId(anyString(), anyString(), anyString(),
            (Pageable) Mockito.any())).thenReturn(this.productItemPage);
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BY_NAME_AND_CATEGORY_ID)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID)
        .param("categoryId", ProductControllerTest.CATEGORY_ID)
        .param("productItemName", ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH)).andExpect(status().isOk());
    verify(this.productItemService).findByStoreIdAndGeneratedItemNameAndCategoryId(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH, ProductControllerTest.CATEGORY_ID, this.pageable);
  }

  @Test
  public void getProductItemByUpcCodeTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_UPC_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("upcCode", ProductControllerTest.UPC_CODE_1)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].generatedItemName", equalTo(ProductControllerTest.PRODUCT_ITEM_NAME)))
        .andExpect(jsonPath("$.content[0].upcCode", equalTo(ProductControllerTest.UPC_CODE_1)))
        .andExpect(jsonPath("$.content[0].skuCode", equalTo(ProductControllerTest.SKU_CODE_1)))
        .andExpect(jsonPath("$.content[0].activated", equalTo(Boolean.FALSE)));
    verify(this.productItemService).findByUpcCode(ProductControllerTest.STORE_ID, ProductControllerTest.UPC_CODE_1,
        this.pageable);
    verify(this.productItemPage).getContent();
    verify(this.productItemPage).getTotalElements();
  }

  @Test
  public void getProductItemDetailBySkuCodeTest() throws Exception {
    this.productItem.setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItem.getProductItemImages().add(new ProductItemImage());
    this.productItem.setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItem.getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItem.getProductItemAttributeValues().get(0).setMarkForDelete(false);
    this.productItem.getProductItemAttributeValues().get(0)
    .setAttribute(new Attribute("Attribute-name",
        com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE, true, ProductControllerTest.STORE_ID));

    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_SKU_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.generatedItemName", equalTo(ProductControllerTest.PRODUCT_ITEM_NAME)))
        .andExpect(jsonPath("$.value.skuCode", equalTo(ProductControllerTest.SKU_CODE_1)))
        .andExpect(jsonPath("$.value.productResponse.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)));

    verify(this.productItemService).findByStoreIdAndSkuCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.SKU_CODE_1);
  }

  @Test
  public void getProductItemDetailBySkuCodeOriginalImagesTest() throws Exception {
    this.originalImageProductItem.setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.originalImageProductItem.getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.originalImageProductItem.getProductItemAttributeValues().get(0).setMarkForDelete(false);
    this.originalImageProductItem.getProductItemAttributeValues().get(0).setAttribute(
        new Attribute("Attribute-name", com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE, true,
            ProductControllerTest.STORE_ID));
    when(this.productItemService
        .findByStoreIdAndSkuCode(ProductControllerTest.STORE_ID, ProductControllerTest.SKU_CODE_1))
        .thenReturn(originalImageProductItem);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_SKU_CODE)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME).param("originalImages", String.valueOf(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.generatedItemName", equalTo(ProductControllerTest.PRODUCT_ITEM_NAME)))
        .andExpect(jsonPath("$.value.skuCode", equalTo(ProductControllerTest.SKU_CODE_1)))
        .andExpect(jsonPath("$.value.productResponse.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)));
    verify(this.productItemService)
        .findByStoreIdAndSkuCode(ProductControllerTest.STORE_ID, ProductControllerTest.SKU_CODE_1);
  }

  @Test
  public void getProductItemDetailBySkuCodeAttributeMarkFordeleteTest() throws Exception {
    this.productItem.setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItem.getProductItemImages().add(new ProductItemImage());
    this.productItem.setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItem.getProductItemAttributeValues().add(new ProductItemAttributeValue());
    this.productItem.getProductItemAttributeValues().get(0).setMarkForDelete(true);

    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductControllerTest.DETAIL_BY_SKU_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.generatedItemName", equalTo(ProductControllerTest.PRODUCT_ITEM_NAME)))
        .andExpect(jsonPath("$.value.skuCode", equalTo(ProductControllerTest.SKU_CODE_1)))
        .andExpect(jsonPath("$.value.productResponse.productCode", equalTo(ProductControllerTest.PRODUCT_CODE)));

    verify(this.productItemService).findByStoreIdAndSkuCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.SKU_CODE_1);
  }

  @Test
  public void getProductSummary() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("name", ProductControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByStoreId(ProductControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductSummaryByMarkForDeleteFalseTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_MARK_FOR_DELETE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("markForDelete", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByMarkForDelete(ProductControllerTest.STORE_ID, false, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getProductSummaryWithCategoriesTest() throws Exception {
    this.mockMvc
    .perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.SUMMARY_WITH_CATEGORIES)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest.SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest.UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)))
        .andExpect(
            jsonPath("$.content[0].productCategoryResponses[0].category.name",
                equalTo(ProductControllerTest.CATEGORY_NAME)));
    verify(this.service).findByStoreIdInitProductCategories(ProductControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void saveProductTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy("Joni");
    productRequest.setUpdatedDate(new Date());
    for (ProductItemRequest productItemRequest : productRequest.getProductItems()) {
      productItemRequest.setDangerousGoodsLevel(0);
    }
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito
    .when(
        this.productItemService.isUpcAvailable(ProductControllerTest.STORE_ID, ProductControllerTest.UPC_CODE_1,
            false)).thenReturn(true);
    Mockito
    .when(
        this.productItemService.isUpcAvailable(ProductControllerTest.STORE_ID, ProductControllerTest.UPC_CODE_2,
            false)).thenReturn(true);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.SAVE).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.productServiceWrapper, Mockito.times(1)).saveProductItemDetails(
        eq(ProductControllerTest.STORE_ID), productItemsListArgumentCaptor.capture(),
        eq(this.product.getProductCode()));
    Assertions.assertEquals(PRODUCT_ITEM_NAME, productItemsListArgumentCaptor.getValue().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE_1, productItemsListArgumentCaptor.getValue().get(0).getUpcCode());
    Assertions.assertEquals(SKU_CODE_1, productItemsListArgumentCaptor.getValue().get(0).getSkuCode());
    Assertions.assertFalse(productItemsListArgumentCaptor.getValue().get(0).getProductItemImages().get(0).isMainImages());
    Assertions.assertEquals(Integer.valueOf(0), productItemsListArgumentCaptor.getValue().get(0).getDangerousGoodsLevel());
  }

  @Test
  public void saveProductWithEmptyProductItemsTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductItems(null);
    productRequest.setUpdatedBy("Joni");
    productRequest.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.SAVE).contentType(MediaType.APPLICATION_JSON)
          .content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_ITEMS_MUST_DEFINE.getMessage()));
    }
  }

  @Test
  public void saveProductWithEmptyUpdatedByTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(null);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.SAVE).contentType(MediaType.APPLICATION_JSON)
          .content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveProductWithEmptyUpdatedDateTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedDate(null);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.SAVE).contentType(MediaType.APPLICATION_JSON)
          .content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveProductWithInvalidDangerousGoodsLevelProductItemTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy("Joni");
    productRequest.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    try {
      this.mockMvc
      .perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.SAVE).contentType(MediaType.APPLICATION_JSON)
          .content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ProductController.DANGEROUS_GOODS_LEVEL_MUST_NOT_BE_BLANK));
    }
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    ReflectionTestUtils.setField(controller, "imageSourceDirectory", BASE_FOLDER);
    ReflectionTestUtils.setField(controller, "duplicateProductCodeConstraint", DUPLICATE_PRODUCT_CODE_CONSTRAINT);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(new Category(ProductControllerTest.STORE_ID, ProductControllerTest.CATEGORY_NAME, 1));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(new Attribute("Attribute-name",
        com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE, true, ProductControllerTest.STORE_ID));
    this.mockMvc = standaloneSetup(this.controller).build();
    this.product = new Product();
    this.product.setId(ProductControllerTest.ID);
    this.product.setBrand(ProductControllerTest.BRAND_NAME);
    this.product.setLength(ProductControllerTest.LENGTH);
    this.product.setName(ProductControllerTest.NAME);
    this.product.setProductCode(ProductControllerTest.PRODUCT_CODE);
    this.product.setShippingWeight(ProductControllerTest.SHIPPING_WEIGHT);
    this.product.setUniqueSellingPoint(ProductControllerTest.UNIQUE_SELLING_POINT);
    this.product.setViewable(true);
    this.product.setActivated(true);
    this.product.setUom(ProductControllerTest.UOM);
    this.product.setWeight(ProductControllerTest.WEIGHT);
    this.product.setWidth(ProductControllerTest.WIDTH);
    this.product.setCreatedBy(ProductControllerTest.CREATED_BY);
    this.product.getProductCategories().add(productCategory);
    this.product.getProductAttributes().add(productAttribute);
    this.products = new ArrayList<>();
    this.products.add(this.product);
    this.productRequest =
        new ProductRequest.Builder().productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH)
        .width(WIDTH).height(HEIGHT).weight(WEIGHT).shippingWeight(SHIPPING_WEIGHT)
        .description(DESCRIPTION).longDescription(DESCRIPTION).brand(BRAND_NAME)
        .uniqueSellingPoint(UNIQUE_SELLING_POINT).uom(UOM).storeId(STORE_ID).promoSKU(false).publishProductEvent(true)
        .build();
    this.imagesDto =
        Arrays.asList(ProductImageDTO.builder().productId(PRODUCT_ID).productCode(PRODUCT_CODE_1)
            .locationPath(LOCATION_PATH).active(Boolean.TRUE).sequence(1).isMainImage(Boolean.TRUE)
            .build());
    productItemsRequest = new ArrayList<ProductItemRequest>();
    ProductItemRequest itemRequest1 =
        new ProductItemRequest(ProductControllerTest.PRODUCT_ITEM_NAME, ProductControllerTest.UPC_CODE_1,
            ProductControllerTest.SKU_CODE_1, false, false, ProductControllerTest.STORE_ID);
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests = new ArrayList<>();
    itemAttributeValueRequests.add(new ProductItemAttributeValueRequest(new AttributeRequest(), VALUE, STORE_ID));
    itemRequest1.setId("1");
    itemRequest1.setProductItemAttributeValues(itemAttributeValueRequests);
    itemRequest1.setSourceItemCode(SOURCE_ITEM_CODE1);
    itemRequest1.setContentChanged(true);
    List<Image> imageList1 = new ArrayList<>();
    Image image1_1 = new Image();
    image1_1.setLocationPath(LOCATION_PATH);
    imageList1.add(image1_1);
    itemRequest1.setImages(imageList1);
    ProductItemRequest itemRequest2 =
        new ProductItemRequest(ProductControllerTest.PRODUCT_ITEM_NAME_2, ProductControllerTest.UPC_CODE_2,
            ProductControllerTest.SKU_CODE_2, false, false, ProductControllerTest.STORE_ID);
    itemRequest2.setId("2");
    itemRequest2.setProductItemAttributeValues(itemAttributeValueRequests);
    productItemsRequest.add(itemRequest1);
    productItemsRequest.add(itemRequest2);

    Attribute attribute1 = new Attribute();
    attribute1.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setAttribute(attribute1);
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    for (ProductItemRequest request : productItemsRequest) {
      ProductItem item =
          new ProductItem(this.product, request.getUpcCode(), request.getSkuCode(), request.getGeneratedItemName(),
              request.getHash(), ProductControllerTest.STORE_ID);
      item.setId(request.getId());
      item.getProductItemAttributeValues().add(productItemAttributeValue);
      productItems.add(item);
    }
    this.product.setProductItems(productItems);
    originalImageProductItem = new ProductItem(this.product, productItemsRequest.get(0).getUpcCode(), productItemsRequest.get(0).getSkuCode(),
        productItemsRequest.get(0).getGeneratedItemName(), productItemsRequest.get(0).getHash(),
        ProductControllerTest.STORE_ID);
    originalImageProductItem.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(false);
    productItemImage.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setOriginalImage(true);
    productItemImage2.setLocationPath(LOCATION_PATH);
    originalImageProductItem
        .setProductItemImages(Arrays.asList(productItemImage, productItemImage1, productItemImage2));
    this.hiddenProduct = new Product();
    this.hiddenProduct.setId(ProductControllerTest.HIDDEN_ID);
    this.hiddenProduct.setName(ProductControllerTest.HIDDEN_NAME);
    this.hiddenProduct.setViewable(false);
    this.hiddenProducts = new ArrayList<>();
    this.hiddenProducts.add(this.hiddenProduct);

    this.productItem =
        new ProductItem(this.product, productItemsRequest.get(0).getUpcCode(), productItemsRequest.get(0).getSkuCode(),
            productItemsRequest.get(0).getGeneratedItemName(), productItemsRequest.get(0).getHash(),
            ProductControllerTest.STORE_ID);
    this.productItems = new ArrayList<>();
    this.productItems.add(this.productItem);

    List<AllowedAttributeValueRequest> allowedAttributeWarna = new ArrayList<AllowedAttributeValueRequest>();
    allowedAttributeWarna
    .add(new AllowedAttributeValueRequest("Merah", Integer.valueOf(1), ProductControllerTest.STORE_ID));
    allowedAttributeWarna.add(new AllowedAttributeValueRequest("Biru", Integer.valueOf(2), ProductControllerTest.STORE_ID));
    allowedAttributeWarna
    .add(new AllowedAttributeValueRequest("Kuning", Integer.valueOf(3), ProductControllerTest.STORE_ID));

    List<AllowedAttributeValueRequest> allowedAttributeUkuran = new ArrayList<AllowedAttributeValueRequest>();
    allowedAttributeUkuran
    .add(new AllowedAttributeValueRequest("16 GB", Integer.valueOf(1), ProductControllerTest.STORE_ID));
    allowedAttributeUkuran
    .add(new AllowedAttributeValueRequest("32 GB", Integer.valueOf(1), ProductControllerTest.STORE_ID));

    AttributeRequest warna =
        new AttributeRequest("Warna", "Kode Warna", AttributeType.DEFINING_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);
    warna.setAllowedAttributeValues(allowedAttributeWarna);

    AttributeRequest ukuran =
        new AttributeRequest("Ukuran", "Kode Ukuran", AttributeType.DEFINING_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);
    ukuran.setAllowedAttributeValues(allowedAttributeUkuran);

    AttributeRequest brand =
        new AttributeRequest("Brand", "Kode Ukuran", AttributeType.PREDEFINED_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValue.setValue(VALUE);
    brand.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValue));

    productAttributes = new ArrayList<ProductAttributeRequest>();
    ProductAttributeRequest productAttributeWarna =
        new ProductAttributeRequest(warna, "warna", false, Integer.valueOf(1), ProductControllerTest.STORE_ID);
    productAttributeWarna.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeWarna.get(0), null, DescriptiveAttributeValueType.NONE,
            ProductControllerTest.STORE_ID));
    productAttributes.add(productAttributeWarna);
    ProductAttributeRequest productAttributeUkuran =
        new ProductAttributeRequest(ukuran, "ukuran", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeUkuran.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.NONE,
            ProductControllerTest.STORE_ID));
    productAttributes.add(productAttributeUkuran);
    ProductAttributeRequest productAttributeBrand =
            new ProductAttributeRequest(ukuran, "brand", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeBrand.getProductAttributeValues().add(
            new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.PREDEFINED,
                ProductControllerTest.STORE_ID));
    productAttributeBrand.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeBrand.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(ID);
    productAttributes.add(productAttributeBrand);
    ProductAttributeRequest productAttributeBrand2 =
            new ProductAttributeRequest(ukuran, "ukuran", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeBrand2.getProductAttributeValues().add(
            new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.PREDEFINED,
                ProductControllerTest.STORE_ID));
    productAttributeBrand2.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeBrand2.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(null);
    productAttributeBrand2.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setPredefinedAllowedAttributeCode(CATEGORY_ID);
    productAttributes.add(productAttributeBrand2);
    ProductAttributeRequest productAttributeBrand3 =
            new ProductAttributeRequest(ukuran, "ukuran", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeBrand3.getProductAttributeValues().add(
            new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.PREDEFINED,
                ProductControllerTest.STORE_ID));
    productAttributes.add(productAttributeBrand3);
    ProductAttributeRequest productAttributeBrand4 =
            new ProductAttributeRequest(ukuran, "brand", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeBrand4.getProductAttributeValues().add(
            new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.PREDEFINED,
                ProductControllerTest.STORE_ID));
    productAttributeBrand4.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeBrand4.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(null);
    productAttributes.add(productAttributeBrand4);

    List<ProductAttribute> productAttributeList = new ArrayList<ProductAttribute>();
    for (ProductAttributeRequest request : productAttributes) {
      Attribute attribute =
          new Attribute(request.getAttribute().getName(), com.gdn.x.productcategorybase.AttributeType.valueOf(request
              .getAttribute().getAttributeType().toString()), request.getAttribute().isSearchAble(),
              ProductControllerTest.STORE_ID);
      productAttributeList.add(new ProductAttribute(attribute, this.product, request.getProductAttributeName(), request
          .isOwnByProductItem(), request.getSequence(), ProductControllerTest.STORE_ID));
    }

    this.productRequest.setProductAttributes(productAttributes);
    this.productRequest.setProductItems(productItemsRequest);
    this.productRequest.setId(ProductControllerTest.ID2);
    this.productRequest.setCreatedBy(ProductControllerTest.CREATED_BY);
    this.productRequest.setCreatedDate(new Date());

    String categoryId = UUID.randomUUID().toString();

    List<ProductCategoryRequest> productCategoryRequests = new ArrayList<ProductCategoryRequest>();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(categoryId);
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequests.add(new ProductCategoryRequest(categoryRequest, ProductControllerTest.STORE_ID));
    this.productRequest.setProductCategories(productCategoryRequests);

    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);

    ProductRequest productRequestTest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductControllerTest.OBJECT_MAPPER
            .getTypeFactory().constructType(ProductRequest.class));
    Assertions.assertNotNull(productRequestTest);

    this.category = new Category(ProductControllerTest.STORE_ID, "category_name", 1);
    this.category.setName(CATEGORY_NAME);
    this.category.setCategoryCode(CATEGORY_CODE);
    this.category.setId(categoryId);
    Mockito.when(this.categoryService.findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId()))
    .thenReturn(this.category);
    this.sentProduct =
        new Product.Builder().productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH)
        .width(WIDTH).height(HEIGHT).weight(WEIGHT).shippingWeight(SHIPPING_WEIGHT)
        .description(DESCRIPTION).brand(BRAND_NAME).uniqueSellingPoint(UNIQUE_SELLING_POINT)
        .uom(UOM).storeId(STORE_ID).promoSKU(false).uom(UOM).build();
    this.sentProduct.setId(ProductControllerTest.ID2);
    this.sentProduct.setCreatedBy(ProductControllerTest.CREATED_BY);
    this.sentProduct.setCreatedDate(new Date());

    this.pageable = PageRequest.of(ProductControllerTest.PAGE_NUMBER, ProductControllerTest.PAGE_SIZE);

    this.activateImageRequest =
        new ActivateImageRequest(ProductControllerTest.PRODUCT_CODE,
            ProductControllerTest.HASH_CODE, ProductControllerTest.LOCATION_PATH);
    this.filenames = new ArrayList<String>();
    this.filenames.add(ProductControllerTest.LOCATION_PATH);
    this.activateImage = new ActivateImage(ProductControllerTest.PRODUCT_CODE, true, this.filenames);

    this.upcCodeSearchRequest = new UPCCodeSearchRequest();
    upcCodeSearchRequest.setUpcCode("1234567");
    upcCodeSearchRequest.setCategoryIds(Arrays.asList(CATEGORY_ID));
    upcCodeSearchRequest.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    ProductCodeResponse productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductCode(product.getProductCode());
    productCodeResponse.setId(product.getId());
    when(this.service.countByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE))
    .thenReturn(1L);
    when(this.service.findByBrandLike(ProductControllerTest.STORE_ID, ProductControllerTest.BRAND_NAME, this.pageable))
    .thenReturn(this.page);
    when(this.service.findByStoreIdAndBrandName(ProductControllerTest.STORE_ID,
        ProductControllerTest.BRAND_NAME, this.pageable)).thenReturn(
        new PageImpl<>(new ArrayList<>(List.of(productCodeResponse))));
    when(
        this.service.findByCategoryId(ProductControllerTest.STORE_ID, ProductControllerTest.CATEGORY_ID, this.pageable))
        .thenReturn(this.page);
    when(this.service.findById(ProductControllerTest.ID)).thenReturn(this.product);
    when(this.service.findByMarkForDelete(ProductControllerTest.STORE_ID, false, this.pageable)).thenReturn(this.page);
    when(this.service.findByName(ProductControllerTest.STORE_ID, ProductControllerTest.NAME_SEARCH, this.pageable))
    .thenReturn(this.page);
    when(
        this.service.findByNameAndCreatedBy(ProductControllerTest.STORE_ID, ProductControllerTest.NAME_SEARCH,
            ProductControllerTest.CREATED_BY, this.pageable)).thenReturn(this.page);
    when(
        this.service.findByNameAndViewableAndActivated(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_NAME, true, true, this.pageable)).thenReturn(this.page);
    when(
        this.service.findByNameAndViewableAndActivatedAndUpdatedBy(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_NAME, true, true, ProductControllerTest.UPDATED_BY, this.pageable))
            .thenReturn(this.page);
    when(
        this.service.findByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_SEARCH,
            this.pageable)).thenReturn(this.page);
    when(
        this.service.findByShippingWeightBiggerOrEqualThan(ProductControllerTest.STORE_ID,
            ProductControllerTest.SHIPPING_WEIGHT, this.pageable)).thenReturn(this.page);
    when(
        this.service.findByShippingWeightLesserOrEqualThan(ProductControllerTest.STORE_ID,
            ProductControllerTest.SHIPPING_WEIGHT, this.pageable)).thenReturn(this.page);
    when(this.service.findByStoreId(ProductControllerTest.STORE_ID, this.pageable)).thenReturn(this.page);
    when(
        this.service.getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(
            ProductControllerTest.STORE_ID, ProductControllerTest.ID)).thenReturn(this.product);
    when(this.service.findByStoreIdInitProductCategories(ProductControllerTest.STORE_ID, this.pageable)).thenReturn(
        this.page);
    when(
        this.service.findByUniqueSellingCodeLike(ProductControllerTest.STORE_ID,
            ProductControllerTest.UNIQUE_SELLING_POINT, this.pageable)).thenReturn(this.page);
    when(this.service.findByViewable(ProductControllerTest.STORE_ID, false, this.pageable)).thenReturn(this.pageHidden);
    when(this.service.findByViewable(ProductControllerTest.STORE_ID, true, this.pageable)).thenReturn(this.page);
    when(this.service.findByViewableAndActivated(ProductControllerTest.STORE_ID, true, true, this.pageable))
    .thenReturn(this.page);
    when(
        this.service.findByWeightBiggerOrEqualThan(ProductControllerTest.STORE_ID, ProductControllerTest.WEIGHT,
            this.pageable)).thenReturn(this.page);
    when(
        this.service.findByWeightLesserOrEqualThan(ProductControllerTest.STORE_ID, ProductControllerTest.WEIGHT,
            this.pageable)).thenReturn(this.page);
    when(this.productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID)).thenReturn(this.product);
    when(
        this.productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            ProductControllerTest.ID)).thenReturn(this.product);
    when(
        this.productServiceWrapper.getCompleteProductDetailByProductId(ProductControllerTest.STORE_ID,
            ProductControllerTest.ID)).thenReturn(this.product);
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);

    when(this.productItemService
        .countBySkuCode(ProductControllerTest.STORE_ID, ProductControllerTest.SKU_CODE_1))
        .thenReturn(1L);
    when(this.productItemService.findByStoreIdAndGeneratedItemName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH, this.pageable))
        .thenReturn(this.productItemPage);
    when(this.productItemService
        .findByStoreIdAndSkuCode(ProductControllerTest.STORE_ID, ProductControllerTest.SKU_CODE_1))
        .thenReturn(this.productItem);
    when(this.productItemService
        .findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(ProductControllerTest.STORE_ID,
            false, false, ProductControllerTest.UPC_CODE_SEARCH, this.pageable))
        .thenReturn(this.productItemPage);
    when(this.productItemService
        .findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(ProductControllerTest.STORE_ID,
            false, false, ProductControllerTest.PRODUCT_ITEM_NAME_SEARCH, this.pageable))
        .thenReturn(this.productItemPage);
    when(this.productItemService
        .findByUpcCode(ProductControllerTest.STORE_ID, ProductControllerTest.UPC_CODE_1,
            this.pageable)).thenReturn(this.productItemPage);
    when(this.service.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(ProductControllerTest.STORE_ID, PRODUCT_CODE))
        .thenReturn(this.product);
    when(this.service.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID, PRODUCT_CODE))
        .thenReturn(this.product);
    when(this.service.updateProductActivated(anyString(), anyString(), Mockito.anyBoolean())).thenReturn(product);
    when(this.page.getContent()).thenReturn(this.products);
    when(this.page.getTotalElements()).thenReturn((long) ProductControllerTest.TOTAL_RECORDS);
    when(this.pageHidden.getContent()).thenReturn(this.hiddenProducts);
    when(this.pageHidden.getTotalElements()).thenReturn((long) ProductControllerTest.TOTAL_RECORDS);
    when(this.productItemPage.getContent()).thenReturn(this.productItems);
    when(this.productItemPage.getTotalElements()).thenReturn((long) ProductControllerTest.TOTAL_RECORDS);
    doNothing().when(this.productServiceWrapper).saveProductItemDetails(ProductControllerTest.STORE_ID, productItems, this.product.getProductCode());
    doNothing().when(this.service)
        .updateProductViewable(anyString(), anyString(), Mockito.anyBoolean());
    doNothing().when(this.service).republishProductByProductCodes(anyString(), anyList(),
            anyString());
    when(this.service.findByProductCodeExactMatch(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_SEARCH, this.pageable)).thenReturn(this.page);
    Mockito.doNothing().when(this.service).updateProductContent(any(Product.class),
      Mockito.anyBoolean());

    when(this.predefinedAllowedAttributeValueService.findById(anyString()))
	.thenReturn(new PredefinedAllowedAttributeValue());
    when(this.service.getProductCountForViewable(anyString(), Mockito.anyBoolean())).thenReturn((long) ProductControllerTest.TOTAL_RECORDS);
    when(this.productItemService.findByUpcCodeExactMatch(anyString(), anyString(), (Pageable) Mockito.any()))
    	.thenReturn(this.productItemPage);
    List<Attribute> attributes = new ArrayList<>();
    attributes.add(new Attribute());
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(new AllowedAttributeValue());
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue());
    Mockito.when(attributeService.findByAttributeIds(eq(STORE_ID), anyList())).thenReturn(attributes);
    Mockito.when(predefinedAllowedAttributeValueService.findByStoreIdAndIds(eq(STORE_ID), Mockito.anySet()))
        .thenReturn(predefinedAllowedAttributeValues);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCodes(eq(STORE_ID), Mockito.anySet()))
        .thenReturn(predefinedAllowedAttributeValues);
    Mockito.when(allowedAttributeValueService.findByStoreIdAndIds(eq(STORE_ID), Mockito.anySet()))
        .thenReturn(allowedAttributeValues);

    simpleItemDetailResponse = SimpleItemDetailResponse.builder().productCode(PRODUCT_CODE).itemName(NAME).itemCode(SKU_CODE_1).build();

    productCategory = new ProductCategory();
    productCategory.setCategory(category);
    productCategory.setProduct(product);
    product.setProductCategories(Collections.singletonList(productCategory));

    productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);

    productList = new ArrayList<>();
    productList.add(new Product());

    productItemUpcCodeUpdateRequest =
        ProductItemUpcCodeUpdateRequest.builder().skuCode(SKU_CODE_1).upcCode(UPC_CODE_1).build();

    ConverterUtil.setFileStorageService(fileStorageService);
    productItemUpcCodesSkuCodesRequest =
        new ProductItemUpcCodesSkuCodesRequest(Arrays.asList(UPC_CODE_1), Arrays.asList(SKU_CODE_1));

    ReflectionTestUtils.setField(controller, "sizeChartValueTypeDelimiter", "-");
  }

  @AfterEach
  public void tearDown() throws IOException {
    verifyNoMoreInteractions(this.service);
    verifyNoMoreInteractions(this.page);
    verifyNoMoreInteractions(this.categoryService);
    verifyNoMoreInteractions(this.productItemService);
    verifyNoMoreInteractions(this.allowedAttributeValueService);
    verifyNoMoreInteractions(this.attributeService);
    verifyNoMoreInteractions(this.predefinedAllowedAttributeValueService);
    verifyNoMoreInteractions(this.imageService);
    verifyNoMoreInteractions(this.applicationCacheServiceBean);
    verifyNoMoreInteractions(this.solrProductFilterService);
    verifyNoMoreInteractions(this.productServiceWrapper);
    verifyNoMoreInteractions(this.productItemServiceWrapper);
    verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(imageServiceWrapper);
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
  public void testFilterProductItemBySkuCodes() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(this.productItem.getSkuCode());
    SkuCodesRequest request = new SkuCodesRequest(skuCodes);
    request.setFetchArchived(true);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE);
    productItemAttributeValue.setAttribute(attribute);
    this.productItems.get(0)
        .setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(productItemAttributeValue);
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(
        new Attribute(BRAND_NAME, com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE,
            true, STORE_ID));

    ProductItemImage productItemImage = new ProductItemImage();
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    when(this.productItemService.findBySkuCodes(STORE_ID, skuCodes, true)).thenReturn(this.productItems);
    this.mockMvc
        .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_SKU_CODES)
            .contentType(MediaType.APPLICATION_JSON)
            .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.productItemService, Mockito.times(1)).findBySkuCodes(STORE_ID, skuCodes, true);
  }

  @Test
  public void testFilterProductItemBySkuCodes_whenFetchArchivedEmpty() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(this.productItem.getSkuCode());
    SkuCodesRequest request = new SkuCodesRequest();
    request.setSkuCodes(skuCodes);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE);
    productItemAttributeValue.setAttribute(attribute);
    this.productItems.get(0)
      .setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(productItemAttributeValue);
    this.productItems.get(0).getProductItemAttributeValues().get(0).setAttribute(
        new Attribute(BRAND_NAME, com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE,
            true, STORE_ID));

    ProductItemImage productItemImage = new ProductItemImage();
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    when(this.productItemService.findBySkuCodes(STORE_ID, skuCodes, false)).thenReturn(this.productItems);
    this.mockMvc
      .perform(post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_SKU_CODES)
        .contentType(MediaType.APPLICATION_JSON)
        .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request))
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME))
      .andExpect(status().isOk());
    verify(this.productItemService, Mockito.times(1)).findBySkuCodes(STORE_ID, skuCodes, false);
  }

  @Test
  public void updateProductTestWithMergeFalse() throws Exception {
    sentProduct.setStoreId(null);
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(sentProduct);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
    		this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, sentProduct, true, false, true, false,
        false, false, false,
        new ProductAndItemLevelUpdatesDTO(false, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
      this.product, sentProduct, true, false, true, false,
        false, false, false, new ProductAndItemLevelUpdatesDTO(true,
        new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))),
        new HashSet<>());
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getValue().getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void updateProductTestWithMergeFalse_videoUpdated() throws Exception {
    sentProduct.setStoreId(null);
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(sentProduct);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    Mockito.when(this.objectMapperMock.writeValueAsString(any(VideoAddEditRequest.class))).thenReturn(
        "{\"videoUrl\":null,\"videoId\":null,\"videoName\":\"VIDEO_NAME_1\","
            + "\"coverImagePath\":null}");
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    productRequest.setVideoUpdated(true);
    productRequest.setVideoAddEditRequest(
        VideoAddEditRequest.builder().videoName(VIDEO_NAME).build());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, sentProduct, true, false, true, false,
        false, false, false,
        new ProductAndItemLevelUpdatesDTO(false, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, sentProduct, true, false, true, false,
        false, false, false, new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))),
        new HashSet<>());
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
        anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getValue().getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getValue().getCreatedBy());
    Assertions.assertTrue(productArgumentCaptor.getValue().getVideo().contains(VIDEO_NAME));
  }

  @Test
  public void updateProductException1Test() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(null);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductExceptionIdNullTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(null);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductExceptionTest() throws Exception {
    Mockito.when(productServiceWrapper.getCompleteProductDetailByProductId(ProductControllerTest.STORE_ID,
        ProductControllerTest.ID)).thenThrow(new RuntimeException());
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductException2Test() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(null);
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  public void updateProductException3Test() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(null);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductTestWithMergeDifferentUSpTest() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint("usp");
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, this.product, true, false, true, true, false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
      this.product, this.product, true, false, true, true,
        false, false, false, new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))),
        new HashSet<>());
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getValue().getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void updateProductTestWithMergeFalseDetailDifferentBrandTest() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand("Brand2");
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, this.product, true, false, true, true, false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
      this.product, this.product, true, false, true, true,
        false, false, false, new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))),
        Collections.emptySet());
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
  }

  @Test
  public void updateProductTestWithMergeFalseDifferentDescription() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription("description".getBytes());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, this.product, true, false, true, true,
        false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
      this.product, this.product, true, false, true, true, false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), Collections.singleton("DESCRIPTION_UPDATE"));
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getValue().getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void updateProductTestWithMergeFalseDifferentCatgeory() throws Exception {
    ReflectionTestUtils.setField(this.controller, "validateItemName", true);
    Category category = new Category();
    category.setCategoryCode("CategoryCode2");
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(categoryService.findByStoreIdAndId(STORE_ID, this.category.getId())).thenReturn(category);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
        this.product, this.product, true, false, true, true,
        false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), new HashSet<>())).thenReturn(product);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.FALSE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(productArgumentCaptor.capture());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(ProductControllerTest.STORE_ID,
      this.product, this.product, true, false, true, true, false, false, false,
        new ProductAndItemLevelUpdatesDTO(true, new HashSet<>(Arrays.asList(SKU_CODE_1, SKU_CODE_2))), Collections.emptySet());

    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getValue().getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void updateProductTestWithValidateItemNameSwitchOnAndItemNameNull() throws Exception {
    ReflectionTestUtils.setField(this.controller, "validateItemName", true);
    Category category = new Category();
    category.setCategoryCode("CategoryCode2");
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    product.setDescription(new byte[] {});
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setActivated(true);
    productRequest.setNewlyAddedProductItems(Arrays.asList(productItemRequest));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("isMergeRequest", Boolean.FALSE.toString())
            .param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductTestWithMergeTrue() throws Exception {
    ReflectionTestUtils.setField(controller, "ranchIntegrationEnabled", true);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(
        this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    Map<String, String> map = new HashMap<>();
    map.put(PRODUCT_CODE, PRODUCT_CODE);
    productRequest.setDistributionInfoRequest(map);
    when(objectMapperMock.writeValueAsString(map)).thenReturn(PRODUCT_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(eq(ProductControllerTest.STORE_ID), productArgumentCaptor.capture(), productArgumentCaptor.capture(), eq(null),
        eq(false), eq(true), eq(true), eq(false), eq(false), eq(false)
    , eq(new ProductAndItemLevelUpdatesDTO()), eq(new HashSet<>()))).thenReturn(product);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("isMergeRequest", Boolean.TRUE.toString()).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).regenerateProductAndEvictCache(eq(ProductControllerTest.STORE_ID), productArgumentCaptor.capture(), productArgumentCaptor.capture(), eq(null),
        eq(false), eq(true), eq(true), eq(false), eq(false), eq(false)
        , eq(new ProductAndItemLevelUpdatesDTO(true, ImmutableSet.of(SKU_CODE_1))),
        eq(new HashSet<>(Arrays.asList("DESCRIPTION_UPDATE"))));
    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(objectMapperMock).writeValueAsString(map);
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.service, Mockito.times(1)).checkBrandChanges(productArgumentCaptor.capture());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getAllValues().get(0).getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getAllValues().get(0).getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getAllValues().get(0).getCreatedBy());
    Assertions.assertEquals(productRequest.getName(), productArgumentCaptor.getAllValues().get(1).getName());
    Assertions.assertEquals(productRequest.getBrand(), productArgumentCaptor.getAllValues().get(1).getBrand());
    Assertions.assertEquals(productRequest.getCreatedBy(), productArgumentCaptor.getAllValues().get(1).getCreatedBy());
    Assertions.assertEquals(product.getName(), productArgumentCaptor.getAllValues().get(2).getName());
    Assertions.assertEquals(product.getBrand(), productArgumentCaptor.getAllValues().get(2).getBrand());
    Assertions.assertEquals(product.getCreatedBy(), productArgumentCaptor.getAllValues().get(2).getCreatedBy());
  }

  @Test
  public void updateProductTestWithMergeTrue_ranchIntegrationEnabledTrue() throws Exception {
    ReflectionTestUtils.setField(controller, "ranchIntegrationEnabled", true);
    Mockito.when(this.service.checkBrandChanges(any(Product.class))).thenReturn(this.product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setSkuCode(SKU_CODE_1);
    productItemUomInfoDTO.setDistributionItemInfoRequest(
        DistributionItemInfoRequest.builder().build());
    productItemUomInfoDTO.setDimensionAndUomDTOList(List.of(DimensionAndUomDTO.builder().build()));
    productRequest.getProductItems().get(0).setProductItemUomInfoDTO(productItemUomInfoDTO);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    when(this.productServiceWrapper.regenerateProductAndEvictCache(
        eq(ProductControllerTest.STORE_ID), productArgumentCaptor.capture(),
        productArgumentCaptor.capture(), eq(null), eq(false), eq(true), eq(true), eq(false),
        eq(false), eq(false), eq(new ProductAndItemLevelUpdatesDTO()),
        eq(new HashSet<>()))).thenReturn(product);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("isMergeRequest", Boolean.TRUE.toString())
            .param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).regenerateProductAndEvictCache(
        eq(ProductControllerTest.STORE_ID), productArgumentCaptor.capture(),
        productArgumentCaptor.capture(), eq(null), eq(false), eq(true), eq(true), eq(false),
        eq(false), eq(false),
        eq(new ProductAndItemLevelUpdatesDTO(true, ImmutableSet.of(SKU_CODE_1))),
        eq(new HashSet<>(Arrays.asList("DESCRIPTION_UPDATE"))));
    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(
        ProductControllerTest.STORE_ID, this.category.getId());
    verify(this.productServiceWrapper).getCompleteProductDetailByProductId(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.service, Mockito.times(1)).checkBrandChanges(productArgumentCaptor.capture());
    verify(attributeService).getAttributeAndValueMap(anyString(), anySet(), anySet(), anySet(),
        anyList(), anyBoolean(), anyString());
    verify(this.service).setProductItemIdForNewlyAddedItems(anyList(), any());
    Assertions.assertEquals(product.getName(),
        productArgumentCaptor.getAllValues().get(0).getName());
    Assertions.assertEquals(product.getBrand(),
        productArgumentCaptor.getAllValues().get(0).getBrand());
    Assertions.assertEquals(product.getCreatedBy(),
        productArgumentCaptor.getAllValues().get(0).getCreatedBy());
    Assertions.assertEquals(productRequest.getName(),
        productArgumentCaptor.getAllValues().get(1).getName());
    Assertions.assertEquals(productRequest.getBrand(),
        productArgumentCaptor.getAllValues().get(1).getBrand());
    Assertions.assertEquals(productRequest.getCreatedBy(),
        productArgumentCaptor.getAllValues().get(1).getCreatedBy());
    Assertions.assertEquals(product.getName(),
        productArgumentCaptor.getAllValues().get(2).getName());
    Assertions.assertEquals(product.getBrand(),
        productArgumentCaptor.getAllValues().get(2).getBrand());
    Assertions.assertEquals(product.getCreatedBy(),
        productArgumentCaptor.getAllValues().get(2).getCreatedBy());
  }

  @Test
  public void updateProductForMergeTest() throws Exception {
    //    Mockito.when(this.service.sortAndGenerateProductItem(this.product)).thenReturn(this
    // .product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.getProductItems().get(0).setImages(new ArrayList<Image>());
    Image image = new Image(true, "loation", 1);
    productRequest.getProductItems().get(0).getImages().add(image);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FOR_MERGE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productItemService, times(2))
        .activateProductItem(eq(STORE_ID), any(ProductItem.class));
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, ID);
  }

  @Test
  public void updateProductForMergeUpdatedByEmptyTest() throws Exception {
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(StringUtils.EMPTY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.getProductItems().get(0).setImages(new ArrayList<Image>());
    Image image = new Image(true, "loation", 1);
    productRequest.getProductItems().get(0).getImages().add(image);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Exception exception = new Exception();
    try {
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FOR_MERGE).contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
              .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
              .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
              .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      exception = e;
    } finally {
      assertTrue(exception.getMessage().contains("Can not process invalid input data "));
    }
  }

  @Test
  public void updateProductForMergeUpdatedDateNullTest() throws Exception {
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(null);
    productRequest.setId(ProductControllerTest.ID);
    productRequest.getProductItems().get(0).setImages(new ArrayList<Image>());
    Image image = new Image(true, "loation", 1);
    productRequest.getProductItems().get(0).getImages().add(image);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Exception exception = new Exception();
    try {
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FOR_MERGE).contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
              .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
              .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
              .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      exception = e;
    } finally {
      assertTrue(exception.getMessage().contains("Can not process invalid input data "));
    }
  }

  @Test
  public void updateProductForMergeIdEmptyTest() throws Exception {
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(StringUtils.EMPTY);
    productRequest.getProductItems().get(0).setImages(new ArrayList<Image>());
    Image image = new Image(true, "loation", 1);
    productRequest.getProductItems().get(0).getImages().add(image);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Exception exception = new Exception();
    try {
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FOR_MERGE).contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
              .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
              .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
              .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      exception = e;
    } finally {
      assertTrue(exception.getMessage().contains("Can not process invalid input data "));
    }
  }

  @Test
  public void updateProductWithSpecificationDetailGeneratedBySystemTest() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setCreatedMerchant("INTERNAL");
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM)
        .contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service, Mockito.times(1)).sortAndGenerateProductItem(
        productArgumentCaptor.capture());
    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.service, Mockito.times(1)).getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, ProductControllerTest.ID);
    verify(this.service, Mockito.times(1)).updateProductWithSpecificationDetailGeneratedBySystem(
        ProductControllerTest.STORE_ID, this.product, this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertEquals(productRequest.getProductAttributes().size(),
        productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(productRequest.getProductAttributes().get(0).getProductAttributeName(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getProductAttributeName());
    Assertions.assertNotEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    assertEquals("INTERNAL", productArgumentCaptor.getValue().getCreatedMerchant());
  }

  @Test
  public void updateProductWithSpecificationDetailGeneratedBySystemExceptionTest() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setCreatedMerchant("INTERNAL");
    productRequest.setId(null);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(
            ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateProductWithSpecificationDetailGeneratedBySystemException2Test() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(null);
    productRequest.setUpdatedDate(new Date());
    productRequest.setCreatedMerchant("INTERNAL");
    productRequest.setId(ID);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(
            ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateProductWithSpecificationDetailGeneratedBySystemException3Test() throws Exception {
    Mockito.when(this.service.sortAndGenerateProductItem(any(Product.class))).thenReturn(this.product);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(null);
    productRequest.setCreatedMerchant("INTERNAL");
    productRequest.setId(ID);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(
            ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void uploadProductItemTest() throws Exception {
    String productId = "PRO1";
    Product product = new Product();
    product.setId(productId);
    String id[] = new String[5];
    String itemName[] = {"MI5 S Merah", "MI5 S Kuning", "MI5 M Merah", "MI5 M Kuning"};
    String upc[] = new String[5];
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    for (int i = 0; i < 4; i++) {
      id[i] = "PI" + (i + 1);
      ProductItem productItem = new ProductItem(product, null, null, itemName[i], null, ProductControllerTest.STORE_ID);
      productItem.setId(id[i]);
      productItems.add(productItem);
      Mockito.when(this.productItemService.findByStoreIdAndId(ProductControllerTest.STORE_ID, id[i])).thenReturn(
          productItems.get(i));
      upc[i] = "UPCBARU" + (i + 1);
      Mockito.when(this.productItemService.isUpcAvailable(ProductControllerTest.STORE_ID, upc[i], false)).thenReturn(
          true);
    }

    InputStream in = this.getClass().getResourceAsStream("/ProductItem.csv");
    MockMultipartFile file = new MockMultipartFile("productItemCsvFile", in);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + "/upload-product-item").file(file)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    for (int i = 0; i < 4; i++) {
      Assertions.assertEquals(productItems.get(i).getSkuCode(), ("PC1230000" + (i + 1)));
      Assertions.assertEquals(productItems.get(i).getUpcCode(), ("UPCBARU" + (i + 1)));
      Mockito.verify(this.productItemService).findByStoreIdAndId(ProductControllerTest.STORE_ID, id[i]);
      // Mockito.verify(productItemService).isUpcAvailable(STORE_ID, upc[i], false);
    }
    Mockito.verify(this.productItemService).update(productItems.get(0));
    Mockito.verify(this.productItemService).update(productItems.get(1));
    Mockito.verify(this.productItemService).update(productItems.get(2));
    Mockito.verify(this.productItemService).update(productItems.get(3));
  }

  @Test
  public void uploadProductItemNullTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
      Mockito.when(this.productItemService.findByStoreIdAndId(ProductControllerTest.STORE_ID, "PI1")).thenReturn(null);
    InputStream in = this.getClass().getResourceAsStream("/ProductItem.csv");
    MockMultipartFile file = new MockMultipartFile("productItemCsvFile", in);
    try {
      Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + "/upload-product-item").file(file).param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk()));
    }
    finally {
      Mockito.verify(this.productItemService).findByStoreIdAndId(ProductControllerTest.STORE_ID, "PI1");
    }
  }

  @Test
  public void uploadProductTest() throws Exception {
    String categoryId1 = "CATE123";
    String categoryId2 = "CATE456";
    String productId = UUID.randomUUID().toString();
    Product product =
        new Product.Builder().productCode("PC123").name("MI5").length(4D)
        .width(1D).height(3D).weight(2D).shippingWeight(5D).description("Description".getBytes())
        .brand("Xiaomi").uniqueSellingPoint("Unique").uom("UOM").storeId(STORE_ID)
        .promoSKU(false).uom(UOM).build();
    // product.setId(productId);
    Product savedProduct =
        new Product.Builder().productCode("PC123").name("MI5").length(4D)
        .width(1D).height(3D).weight(2D).shippingWeight(5D).description("Description".getBytes())
        .brand("Xiaomi").uniqueSellingPoint("Unique").uom("UOM").storeId(STORE_ID)
        .promoSKU(false).uom(UOM).build();
    savedProduct.setId(productId);

    ProductItem productItem1 =
        new ProductItem(product, null, null, "generatedItemName1", "hash".getBytes(), ProductControllerTest.STORE_ID);
    ProductItem productItem2 =
        new ProductItem(product, null, null, "generatedItemName2", "hash".getBytes(), ProductControllerTest.STORE_ID);
    productItem1.setId(UUID.randomUUID().toString());
    productItem2.setId(UUID.randomUUID().toString());
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    savedProduct.setProductItems(new ArrayList<ProductItem>());
    savedProduct.getProductItems().add(productItem1);
    savedProduct.getProductItems().get(0).setSkuCode(SKU_CODE_1);

    String attributeId[] = new String[5];
    String attributeName[] = {"Warna", "Size", "Layar", "Memori"};
    List<Attribute> attributes = new ArrayList<Attribute>();
    for (int i = 0; i < 4; i++) {
      attributeId[i] = "AT" + (i + 1);
      attributes.add(new Attribute(attributeName[i], com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE,
          true, ProductControllerTest.STORE_ID));
      Mockito.when(this.attributeService.findById(ProductControllerTest.STORE_ID, attributeId[i])).thenReturn(
          attributes.get(i));
    }

    String allowedAttributeValueId[] = new String[5];
    List<AllowedAttributeValue> productAllowedAttributeValues = new ArrayList<AllowedAttributeValue>();
    for (int i = 0; i < 4; i++) {
      allowedAttributeValueId[i] = "AAV" + (i + 1);
      productAllowedAttributeValues.add(new AllowedAttributeValue(attributes.get(i / 2), "value",
          ProductControllerTest.STORE_ID, 1));
      Mockito.when(
          this.allowedAttributeValueService.findByStoreIdAndId(ProductControllerTest.STORE_ID,
              allowedAttributeValueId[i])).thenReturn(productAllowedAttributeValues.get(i));
    }
    Mockito.when(this.productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, productId)).thenReturn(savedProduct);
    Mockito.when(this.service.save(any(Product.class))).thenReturn(productId);
    InputStream in = this.getClass().getResourceAsStream("/Product.csv");
    MockMultipartFile file = new MockMultipartFile("productCsvFile", in);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + "/upload-product").file(file)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());

    for (int i = 0; i < 4; i++) {
      Mockito.verify(this.allowedAttributeValueService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
          allowedAttributeValueId[i]);
    }
    Mockito.verify(this.attributeService, times(4)).findById(eq(STORE_ID), anyString());
    Mockito.verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID, categoryId1);
    Mockito.verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID, categoryId2);
    Mockito.verify(this.productServiceWrapper).getCompleteProductDetailByProductIdAndMarkForDeleteFalse(
        ProductControllerTest.STORE_ID, productId);
    Mockito.verify(this.service).save(productArgumentCaptor.capture());
  }

  @Test
  public void testUpdateImageNameSuccessfully() throws Exception {
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Mockito.doNothing().when(
        this.imageService).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
            ProductControllerTest.HASH_CODE);

    Mockito.when(this.imageService.isProductImagesActivated(STORE_ID, PRODUCT_CODE)).thenReturn(this.activateImage);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.imageService, Mockito.times(1)).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
        ProductControllerTest.HASH_CODE);
    verify(this.imageService, Mockito.times(1)).isProductImagesActivated(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void testActivateAndUpdateImageNameSuccessfully() throws Exception {
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Mockito.doNothing().when(
        this.imageService).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
            ProductControllerTest.HASH_CODE);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.imageService, Mockito.times(1)).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
        ProductControllerTest.HASH_CODE);
  }

  @Test
  public void testActivateAndUpdateImageNameInvalidProductCode() throws Exception {
    this.activateImageRequest.setProductCode("");
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Mockito.doNothing().when(
        this.imageService).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
            ProductControllerTest.HASH_CODE);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());

    verify(this.imageService, Mockito.times(0)).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
        ProductControllerTest.HASH_CODE);
  }

  @Test
  public void testActivateAndUpdateImageNameInvalidFileNames() throws Exception {
    this.activateImageRequest.setFilenames("");
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Mockito.doNothing().when(
        this.imageService).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
            ProductControllerTest.HASH_CODE);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());

    verify(this.imageService, Mockito.times(0)).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
        ProductControllerTest.HASH_CODE);
  }

  @Test
  public void testActivateAndUpdateImageNameInvalidHashCode() throws Exception {
    this.activateImageRequest.setHashCode("");
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Mockito.doNothing().when(
        this.imageService).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
            ProductControllerTest.HASH_CODE);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());

    verify(this.imageService, Mockito.times(0)).activateAndUpdateImageName(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, ProductControllerTest.LOCATION_PATH,
        ProductControllerTest.HASH_CODE);
  }

  @Test
  public void isProductImagesActivatedSuccessfully() throws Exception {
    when(this.imageService.isProductImagesActivated(STORE_ID, PRODUCT_CODE)).thenReturn(
        this.activateImage);

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.IS_PRODUCT_IMAGES_ACTIVATED)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.imageService).isProductImagesActivated(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void isProductImagesActivatedInvalidProductCode() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.IS_PRODUCT_IMAGES_ACTIVATED)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("productCode", ""))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))));
  }

  @Test
  public void testUpdateWithInvalidProductCode() throws Exception {
    this.activateImageRequest.setProductCode("");
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void testUpdateWithInvalidFilenames() throws Exception {
    this.activateImageRequest.setFilenames("");
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Assertions.assertThrows(Exception.class, () ->  this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void testUpdateWithInvalidHashCode() throws Exception {
    this.activateImageRequest.setHashCode("");
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.activateImageRequest);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateProductViewableTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_VIEWABLE)
            .addParameter("storeId", ProductControllerTest.STORE_ID)
            .addParameter("channelId", ProductControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductControllerTest.REQUEST_ID)
            .addParameter("username", ProductControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductControllerTest.CLIENT_ID)
            .addParameter("productCode", ProductControllerTest.PRODUCT_CODE)
            .addParameter("viewable", String.valueOf(true)).build();
    this.mockMvc.perform(get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(productServiceWrapper).updateProductViewable(anyString(), anyString(), Mockito.anyBoolean());
  }

  @Test
  public void updateProductViewableWithInvalidProductCodeTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_VIEWABLE)
            .addParameter("storeId", ProductControllerTest.STORE_ID)
            .addParameter("channelId", ProductControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductControllerTest.REQUEST_ID)
            .addParameter("username", ProductControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductControllerTest.CLIENT_ID).addParameter("productCode", "")
            .addParameter("viewable", String.valueOf(true)).build();
    try {
      this.mockMvc.perform(get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK));
      verify(this.service, ProductControllerTest.NEVER_CALLED).updateProductViewable(anyString(),
          anyString(), Mockito.anyBoolean());
    }
  }

  @Test
  public void updateProductReviewPendingTest() throws Exception {
    this.mockMvc
        .perform(put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REVIEW_PENDING, ProductControllerTest.PRODUCT_CODE)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("reviewPending", String.valueOf(false)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productServiceWrapper).updateProductReviewPendingAndEvictCache(ProductControllerTest.STORE_ID,
      ProductControllerTest.PRODUCT_CODE, false);
  }

  @Test
  public void updateFlagsOnNeedCorrectionTest() throws Exception {
    NeedRevisionConfigRequest needRevisionConfigRequest = new NeedRevisionConfigRequest(true, true, true);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(needRevisionConfigRequest);
    this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FLAGS_FOR_NEED_REVISION,
        ProductControllerTest.PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).content(request)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("clientId", ProductControllerTest.CLIENT_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productServiceWrapper).updateFlagsOnNeedCorrectionAndEvictCache(ProductControllerTest.STORE_ID,
      ProductControllerTest.PRODUCT_CODE,
        needRevisionConfigRequest);
  }

  @Test
  public void updateProductActivatedTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_ACTIVATED)
            .addParameter("storeId", ProductControllerTest.STORE_ID)
            .addParameter("channelId", ProductControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductControllerTest.REQUEST_ID)
            .addParameter("username", ProductControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductControllerTest.CLIENT_ID)
            .addParameter("productCode", ProductControllerTest.PRODUCT_CODE)
            .addParameter("activated", String.valueOf(true)).build();
    this.mockMvc.perform(get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
    .andExpect(status().isOk());
    verify(productServiceWrapper).updateProductActivated(STORE_ID, PRODUCT_CODE, true);
  }

  @Test
  public void updateProductActivatedWithInvalidProductCodeTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_ACTIVATED)
            .addParameter("storeId", ProductControllerTest.STORE_ID)
            .addParameter("channelId", ProductControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductControllerTest.REQUEST_ID)
            .addParameter("username", ProductControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductControllerTest.CLIENT_ID).addParameter("productCode", "")
            .addParameter("activated", String.valueOf(true)).build();
    try {
      this.mockMvc.perform(get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK));
      verify(this.service, ProductControllerTest.NEVER_CALLED).updateProductActivated(anyString(),
          anyString(), Mockito.anyBoolean());
    }
  }


  @Test
  public void getProductlByProductCodeExactMatchTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_CODE_EXACT_MATCH)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId",
                ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE_SEARCH)).andExpect
        (status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest
            .PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest
            .TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductControllerTest
            .PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].length", equalTo(ProductControllerTest.LENGTH)))
        .andExpect(jsonPath("$.content[0].width", equalTo(ProductControllerTest.WIDTH)))
        .andExpect(jsonPath("$.content[0].weight", equalTo(ProductControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.content[0].shippingWeight", equalTo(ProductControllerTest
            .SHIPPING_WEIGHT)))
        .andExpect(jsonPath("$.content[0].brand", equalTo(ProductControllerTest.BRAND_NAME)))
        .andExpect(jsonPath("$.content[0].uniqueSellingPoint", equalTo(ProductControllerTest
            .UNIQUE_SELLING_POINT)))
        .andExpect(jsonPath("$.content[0].uom", equalTo(ProductControllerTest.UOM)));
    verify(this.service).findByProductCodeExactMatch(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_SEARCH,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getActiveProductIdsFromCategoryTestWithoutDate() throws Exception {
    when(this.solrProductFilterService.getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10)))
        .thenReturn(new PageImpl<>(new ArrayList<>(getMockResponses(5))));

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("categoryId", ProductControllerTest.CATEGORY_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrProductFilterService).getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10));
  }

  @Test
  public void getActiveProductIdsFromCategoryTestEmptyResponse() throws Exception {
    when(this.solrProductFilterService.getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("categoryId", ProductControllerTest.CATEGORY_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrProductFilterService).getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10));
  }

  @Test
  public void getActiveProductIdsFromCategoryTestException() throws Exception {
	  List<SolrProductResponse> listResponse = new ArrayList<SolrProductResponse>();
	  listResponse.add(new SolrProductResponse());
	    when(this.solrProductFilterService.getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10)))
        .thenThrow(RuntimeException.class);

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("categoryId", ProductControllerTest.CATEGORY_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.solrProductFilterService).getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10));
  }

  @Test
  public void getActiveProductIdsFromCategoryTestWithDate() throws Exception {
    when(this.solrProductFilterService.getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10)))
        .thenReturn(new PageImpl<>(new ArrayList<>(getMockResponses(5))));

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("categoryId", ProductControllerTest.CATEGORY_ID)
            .param("updatedAfter", ProductControllerTest.DEFAULT_DATE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrProductFilterService).getActiveProductIds(any(SolrProductModel.class), eq(0), eq(10));
  }

  @Test
  public void getActiveProductIdsFromCategoryTestWithPageAndSize() throws Exception {
    when(this.solrProductFilterService.getActiveProductIds(any(SolrProductModel.class), eq(2), eq(100)))
      .thenReturn(new PageImpl<>(new ArrayList<>(getMockResponses(5))));

    this.mockMvc
      .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("categoryId", ProductControllerTest.CATEGORY_ID)
        .param("updatedAfter", ProductControllerTest.DEFAULT_DATE)
        .param("page", "2")
        .param("size", "100"))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.solrProductFilterService).getActiveProductIds(any(SolrProductModel.class), eq(2), eq(100));
  }

  @Test
  public void updateProductContentTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoId("video-Id");
    productRequest.setVideoAddEditRequest(videoAddEditRequest);
    when(objectMapperMock.writeValueAsString(videoAddEditRequest)).
        thenThrow(new RuntimeException("Error serializing videoAddEditRequest"));
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
        .content(request).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnAndNewlyAddedItemsTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    productItemsRequest.get(0).getImages().get(0).setOriginalImage(false);
    productItemsRequest.get(0).getImages().get(0).setActive(true);
    productRequest.setNewlyAddedProductItems(productItemsRequest);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito.when(this.productItemService.getSequenceTransaction(product.getProductCode())).thenReturn(STORE_ID);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(productArgumentCaptor.capture(), Mockito.anyBoolean(),
        eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(productItemService, times(2)).getSequenceTransaction(product.getProductCode());
    assertTrue(productArgumentCaptor.getValue().getProductItems().get(0).getVatApplicable());
    assertTrue(productArgumentCaptor.getValue().getProductItems().get(0).isNewlyAddedItem());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnAndNewlyAddedItemsMainImageTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    productItemsRequest.get(0).getImages().get(0).setOriginalImage(false);
    productItemsRequest.get(0).getImages().get(0).setActive(true);
    productItemsRequest.get(0).getImages().get(0).setMainImages(true);
    productRequest.setNewlyAddedProductItems(productItemsRequest);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito.when(this.productItemService.getSequenceTransaction(product.getProductCode())).thenReturn(STORE_ID);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService, times(1)).getAttributeAndValueMap(anyString(),anySet(), anySet(),
      anySet(),
      anyList(), anyBoolean(), anyString());
    verify(productItemService, times(2)).getSequenceTransaction(product.getProductCode());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnAndNewlyAddedItemsActiveFalseTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    productItemsRequest.get(0).getImages().get(0).setOriginalImage(false);
    productItemsRequest.get(0).getImages().get(0).setActive(false);
    productItemsRequest.get(0).getImages().get(0).setMainImages(true);
    productRequest.setNewlyAddedProductItems(productItemsRequest);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito.when(this.productItemService.getSequenceTransaction(product.getProductCode())).thenReturn(STORE_ID);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(productItemService, times(2)).getSequenceTransaction(product.getProductCode());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnAndNewlyAddedItemsOriginalImageNullTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    productItemsRequest.get(0).getImages().get(0).setOriginalImage(null);
    productItemsRequest.get(0).getImages().get(0).setActive(true);
    productItemsRequest.get(0).getImages().get(0).setMainImages(true);
    productRequest.setNewlyAddedProductItems(productItemsRequest);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito.when(this.productItemService.getSequenceTransaction(product.getProductCode())).thenReturn(STORE_ID);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(productItemService, times(2)).getSequenceTransaction(product.getProductCode());
  }

  @Test
  public void updateProductContentAddVariantsSwitchOnAndNewlyAddedItemsOriginalImageTrueTest() throws Exception {
    ReflectionTestUtils.setField(controller, "addDeleteVariantsSwitch", true);
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    productItemsRequest.get(0).getImages().get(0).setOriginalImage(true);
    productItemsRequest.get(0).getImages().get(0).setActive(true);
    productItemsRequest.get(0).getImages().get(0).setMainImages(true);
    productRequest.setNewlyAddedProductItems(productItemsRequest);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Mockito.when(this.productItemService.getSequenceTransaction(product.getProductCode())).thenReturn(STORE_ID);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(productItemService, times(2)).getSequenceTransaction(product.getProductCode());
  }

  @Test
  public void updateProductContentExceptionTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.setBrand("Brand2");
    Mockito.doThrow(Exception.class).when(this.productServiceWrapper)
        .updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean() , eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
  }

  @Test
  public void updateProductContentNoDetailChangeTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent(any(Product.class), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductContentTestWithNullStoreIdInAttribute() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    productRequest.getProductAttributes().get(0).setStoreId(null);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT).contentType(MediaType.APPLICATION_JSON)
        .content(request).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).updateProductContent((Product) Mockito.any(), Mockito.anyBoolean(), eq(false), eq(false));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductImageTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
    .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE).contentType(MediaType.APPLICATION_JSON)
        .content(request).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateProductImage((Product) Mockito.any());
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateProductImageExceptionTest() throws Exception {
    ProductRequest productRequest = ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setProductCode(ProductControllerTest.PRODUCT_CODE);
    Mockito.doThrow(Exception.class).when(productServiceWrapper).updateProductImage(Mockito.any());
    String request = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE).contentType(MediaType.APPLICATION_JSON)
                .content(request).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    verify(productServiceWrapper).updateProductImage(Mockito.any());
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void getProductDetailListByProductCodesTest() throws Exception {
    this.controller.getProductDetailListByProductCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, PRODUCT_CODE_LISTS,false, false);
    Mockito.verify(this.productServiceWrapper)
        .getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(anyString(), anyString());
  }

  @Test
  public void getProductDetailListByProductCodesForBulkRequestTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.when(this.service.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID, PRODUCT_CODE_LISTS)).thenReturn(products);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES_FOR_BULK_DOWNLOAD)
        .content(objectMapper.writeValueAsString(PRODUCT_CODE_LISTS)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    Mockito.verify(this.service).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID, PRODUCT_CODE_LISTS);
  }

  @Test
  public void getProductDetailListByProductCodesForBulkRequest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.when(this.service.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID, PRODUCT_CODE_LISTS))
        .thenThrow(Exception.class);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES_FOR_BULK_DOWNLOAD)
        .content(objectMapper.writeValueAsString(PRODUCT_CODE_LISTS)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    Mockito.verify(this.service).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID, PRODUCT_CODE_LISTS);
  }

  @Test
  public void getProductDetailListByProductCodesTest_Success() throws Exception {
    Mockito.when(this.productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_CODE_1)).thenReturn(this.productObject);
    this.controller.getProductDetailListByProductCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, PRODUCT_CODE_LISTS, Boolean.FALSE, Boolean.FALSE);
    Mockito.verify(this.productServiceWrapper)
        .getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(anyString(), anyString());
    Mockito.verify(this.productObject).getProductItems();
    Mockito.verify(this.productObject, times(2)).getProductImages();
    Mockito.verify(this.productObject).getProductAttributes();
    Mockito.verify(this.productObject).getProductCategories();
  }

  @Test
  public void getProductDetailListByProductCodesTest_inAllProductsTrue() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    Mockito
        .when(this.productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(STORE_ID, PRODUCT_CODE_1))
        .thenReturn(productObject);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES)
        .content(objectMapper.writeValueAsString(PRODUCT_CODE_LISTS)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("inAllProducts", String.valueOf(true))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    Mockito.verify(this.productServiceWrapper)
        .getCompleteProductDetailByProductCodeInAllProducts(STORE_ID, PRODUCT_CODE_1);
  }

  @Test
  public void getProductBasicDetailListByProductCodesTest_Success() throws Exception {
    GdnRestSingleResponse<ProductResponse> gdnRestSingleResponse = this.controller
        .getProductBasicDetailByProductCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, DEFAULT_USERNAME,
            PRODUCT_CODE);
    Mockito.verify(this.service).getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(
        eq(STORE_ID), eq(PRODUCT_CODE));
    Assertions.assertEquals(PRODUCT_CODE, gdnRestSingleResponse.getValue().getProductCode());
  }

  @Test
  public void getProductDetailListByProductCodesExceptionTest() throws Exception {
    this.controller.getProductDetailListByProductCodes(STORE_ID, CHANNEL_ID, null, REQUEST_ID,
        DEFAULT_USERNAME, null, Boolean.FALSE, Boolean.FALSE);
  }

  @Test
  public void getProductItemByProductItemNameOrUpcCodeTest() throws Exception {
	this.productItemPage.getContent().get(0).setProductItemImages(new ArrayList<ProductItemImage>());
	ProductItemImage image = new ProductItemImage();
	ProductItemImage image1 = new ProductItemImage();
	ProductItemImage image2 = new ProductItemImage();
	ProductItemImage image3 = new ProductItemImage();
	image.setMainImages(false);
	image1.setMainImages(true);
	image1.setMarkForDelete(true);
	image2.setMainImages(true);
	image2.setMarkForDelete(false);
	this.productItemPage.getContent().get(0).getProductItemImages().add(image);
	this.productItemPage.getContent().get(0).getProductItemImages().add(image1);
	this.productItemPage.getContent().get(0).getProductItemImages().add(image2);
	when(this.productItemPage.getContent()).thenReturn(this.productItems);
    when(this.productItemService
            .findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
					anyString(), Mockito.anyBoolean(),
					Mockito.anyBoolean(), anyString(),
					(Pageable) Mockito.any()))
            .thenReturn(this.productItemPage);
    this.controller.getProductItemByProductItemNameOrUpcCode(STORE_ID, REQUEST_ID,
        DEFAULT_USERNAME, PAGE, PAGE, UPC_CODE_SEARCH, VIEWABLE, VIEWABLE);
    Mockito.verify(this.productItemService)
				.findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
						anyString(), Mockito.anyBoolean(),
						Mockito.anyBoolean(), anyString(),
						(Pageable) Mockito.any());
  }

  @Test
  public void getProductItemByProductItemNameOrUpcCodeImageNullTest() throws Exception {
	when(this.productItemPage.getContent()).thenReturn(this.productItems);
    when(this.productItemService
            .findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
					anyString(), Mockito.anyBoolean(),
					Mockito.anyBoolean(), anyString(),
					(Pageable) Mockito.any()))
            .thenReturn(this.productItemPage);
    this.controller.getProductItemByProductItemNameOrUpcCode(STORE_ID, REQUEST_ID,
        DEFAULT_USERNAME, PAGE, PAGE, UPC_CODE_SEARCH, VIEWABLE, VIEWABLE);
    Mockito.verify(this.productItemService)
				.findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(
						anyString(), Mockito.anyBoolean(),
						Mockito.anyBoolean(), anyString(),
						(Pageable) Mockito.any());
  }

  @Test
  public void republishProductTest() throws Exception {
    this.controller.republishProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, PRODUCT_CODE, PRODUCT_CODE_LISTS);
    Mockito.verify(this.service)
        .republishProductByProductCodes(anyString(), anyList(), anyString());
  }

  @Test
  public void publishProductsByProductCodesTest() throws Exception {
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.REPUBLISH_PRODUCT_TO_AGP)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .content(objectMapper.writeValueAsString(PRODUCT_CODE_LISTS))
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productServiceWrapper).republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODE_LISTS,true);
  }

  @Test
  public void clearProductCacheTest() throws Exception {
    Mockito.when(this.service.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(this.productObject);
    GdnBaseRestResponse gdnBaseRestResponse = this.controller
        .clearProductCache(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, DEFAULT_USERNAME, PRODUCT_CODE, PRODUCT_CODE);
    Mockito.verify(service).evictAllProductDetailCacheByProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertNotNull(gdnBaseRestResponse);
    assertTrue(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void clearProductCacheSyncTest() {
    Mockito.when(this.productItemService.findByListOfProductCode(Collections.singletonList(PRODUCT_ID), false, true))
        .thenReturn(new ArrayList<>());
    GdnBaseRestResponse gdnBaseRestResponse = this.controller
        .clearProductCacheSync(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, DEFAULT_USERNAME, PRODUCT_CODE,
            PRODUCT_ID);
    Mockito.verify(this.service).evictAllProductDetailCacheByProductCode(STORE_ID, PRODUCT_ID);
    Assertions.assertNotNull(gdnBaseRestResponse);
    assertTrue(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void getProductCountByViewableTest() throws Exception {
    this.controller.getProductCountByViewable(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, VIEWABLE);
    Mockito.verify(this.service)
        .getProductCountForViewable(anyString(), Mockito.anyBoolean());
  }

  @Test
  public void getProductItemByUpcCodeExactMatchTest() throws Exception {
    this.controller.getProductItemByUpcCodeExactMatch(STORE_ID, REQUEST_ID,
        DEFAULT_USERNAME, PAGE, PAGE, UPC_CODE_1);
    Mockito.verify(this.productItemService)
        .findByUpcCodeExactMatch(anyString(), anyString(), (Pageable) Mockito.any());
  }

//  @Test
//  public void convertToImageUpdateTest() throws Exception {
//	  List<UpdateImageDTO> request = new ArrayList<UpdateImageDTO>();
//	  request.add(new UpdateImageDTO());
//    this.controller.convertToImageUpdate(request);
//  }

  @Test
  public void getProductCountByBrandNameTest() throws Exception {
    this.controller.getProductCountByBrandName(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, BRAND_NAME);
    Mockito.verify(this.service).getProductCountByBrandName(STORE_ID, BRAND_NAME);
  }

  @Test
  public void getProductCountByBrandNameWithNullBrandNameTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.controller.getProductCountByBrandName(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, null));
  }

  @Test
  public void getProductItemByListOfProductCodeTest() throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(new ArrayList<String>());
    request.getProductCodes().add("12345");
    ObjectMapper objectMapper = new ObjectMapper();
    when(this.productItemService
        .findByListOfProductCode(anyList(), Mockito.anyBoolean(), eq(true)))
        .thenReturn(this.productItems);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_PRODUCT_CODES)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request))).andExpect(
        status().isOk());
    verify(this.productItemService)
        .findByListOfProductCode(anyList(), Mockito.anyBoolean(), eq(true));
  }

  @Test
  public void getProductItemByListOfProductCodeTestException() throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(new ArrayList<String>());
    request.getProductCodes().add("12345");
    ObjectMapper objectMapper = new ObjectMapper();
    doThrow(RuntimeException.class).when(this.productItemService)
        .findByListOfProductCode(anyList(), Mockito.anyBoolean(), eq(true));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_PRODUCT_CODES)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request))).andExpect(
        status().isOk());
    verify(this.productItemService)
        .findByListOfProductCode(anyList(), Mockito.anyBoolean(), eq(true));
  }

  @Test
  public void updateRejectedProductTest() throws Exception {
    Mockito.when(this.service.checkBrandChanges(this.product)).thenReturn(this.product);
    when(this.productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE)).thenReturn(this.product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REJECTED_PRODUCT)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME).param("resetExtractedAttributeValue", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper)
        .getCompleteProductDetailByProductCodeInAllProducts(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_CODE);
    verify(this.categoryService, Mockito.times(1))
        .findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
    verify(this.productServiceWrapper, Mockito.times(1)).regenerateProductAndEvictCache(eq(ProductControllerTest.STORE_ID),
        productArgumentCaptor.capture(), productArgumentCaptor.capture(), eq(false), eq(false), eq(true), eq(false),  eq(false), eq(false), eq(true),
        eq(new ProductAndItemLevelUpdatesDTO()), eq(new HashSet<>()));
    verify(this.service, Mockito.times(1)).checkBrandChanges(this.product);
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(0).isActivated());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(1).isActivated());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(0).isMarkForDelete());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(1).isMarkForDelete());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(0).isViewable());
    Assertions.assertFalse(productArgumentCaptor.getAllValues().get(1).isViewable());
  }

  @Test
  public void updateRejectedProductException1Test() throws Exception {
    Mockito.when(this.service.checkBrandChanges(this.product)).thenReturn(this.product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(null);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REJECTED_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateRejectedProductException2Test() throws Exception {
    Mockito.when(this.service.checkBrandChanges(this.product)).thenReturn(this.product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(null);
    productRequest.setId(ProductControllerTest.ID);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REJECTED_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateRejectedProductException3Test() throws Exception {
    Mockito.when(this.service.checkBrandChanges(this.product)).thenReturn(this.product);
    ProductRequest productRequest =
        ProductControllerTest.OBJECT_MAPPER.readValue(this.jsonReq, ProductRequest.class);
    productRequest.setUpdatedBy(UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(null);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REJECTED_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void replaceProductImagesWithEmptyList() throws Exception {
    GeneratedProductImagesPathDto generatedProduct = new GeneratedProductImagesPathDto();
    Mockito.when(this.service.replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any()))
      .thenReturn(generatedProduct);

    ReplaceProductImagesRequest replaceProductImagesReq = new ReplaceProductImagesRequest();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(replaceProductImagesReq);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.REPLACE_PRODUCT_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(this.service).replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any());
  }

  @Test
  public void replaceProductImagesTest() throws Exception {
    GeneratedProductImagesPathDto generatedProductImage = new GeneratedProductImagesPathDto();
    generatedProductImage.setImages(new ArrayList<ImagePathDTO>());
    generatedProductImage.getImages().add(new ImagePathDTO("", true));
    generatedProductImage.setProductItems(new ArrayList<GeneratedProductItemImagesPathDto>());

    GeneratedProductItemImagesPathDto generatedItemDto = new GeneratedProductItemImagesPathDto();
    generatedItemDto.setSkuCode(SKU_CODE_1);
    generatedItemDto.setImages(new ArrayList<ImagePathDTO>());
    generatedItemDto.getImages().add(new ImagePathDTO("", true));
    generatedProductImage.getProductItems().add(generatedItemDto);

    Mockito.when(this.service.replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any()))
      .thenReturn(generatedProductImage);

    ReplaceProductImagesRequest replaceProductImagesReq = new ReplaceProductImagesRequest();
    replaceProductImagesReq.setProductItem(new ArrayList<ReplaceProductItemImagesRequest>());
    replaceProductImagesReq.getProductItem().add(new ReplaceProductItemImagesRequest(SKU_CODE_1, 3));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(replaceProductImagesReq);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.REPLACE_PRODUCT_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(this.service).replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any());
  }

  @Test
  public void replaceProductNullImagesTest() throws Exception {
    GeneratedProductImagesPathDto generatedProductImage = new GeneratedProductImagesPathDto();
    generatedProductImage.setImages(new ArrayList<ImagePathDTO>());
    generatedProductImage.getImages().add(new ImagePathDTO("", true));
    generatedProductImage.setProductItems(new ArrayList<GeneratedProductItemImagesPathDto>());

    GeneratedProductItemImagesPathDto generatedItemDto = new GeneratedProductItemImagesPathDto();
    generatedItemDto.setSkuCode(SKU_CODE_1);
    generatedItemDto.setImages(new ArrayList<ImagePathDTO>());
    generatedProductImage.getProductItems().add(generatedItemDto);

    Mockito.when(this.service.replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any()))
      .thenReturn(generatedProductImage);

    ReplaceProductImagesRequest replaceProductImagesReq = new ReplaceProductImagesRequest();
    replaceProductImagesReq.setProductItem(new ArrayList<ReplaceProductItemImagesRequest>());
    replaceProductImagesReq.getProductItem().add(new ReplaceProductItemImagesRequest(SKU_CODE_1, 3));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(replaceProductImagesReq);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.REPLACE_PRODUCT_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(this.service).replaceProductImages(anyString(), (ReplaceProductImagesDTO) Mockito.any());
  }

  @Test
  public void validateProductPromoSkuWithParamFalse() throws Exception {
    GdnRestSingleResponse<SingleObjectResponse<Boolean>> response =
        this.controller.validateProductPromoSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, PRODUCT_ID, PROMO_SKU);
    Mockito.verify(this.service).validateProductPromoSku(PRODUCT_ID, STORE_ID, PROMO_SKU);
    assertNotNull(response);
  }

  @Test
  public void validateProductPromoSkuWithParamTrue() throws Exception {
    GdnRestSingleResponse<SingleObjectResponse<Boolean>> response =
        this.controller.validateProductPromoSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        DEFAULT_USERNAME, PRODUCT_ID, false);
    Mockito.verify(this.service).validateProductPromoSku(PRODUCT_ID, STORE_ID, false);
    assertNotNull(response);
  }

  @Test
  public void testUpdateImagesName_success() throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(PRODUCT_CODE);
    Set<ActivateImageRequest> imageRequests = new HashSet<>();
    imageRequests.add(this.activateImageRequest);
    productActivateImageRequest.setImageRequests(imageRequests);
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productActivateImageRequest);

    Mockito.doNothing().when(this.imageServiceWrapper)
        .activateAndUpdateImagesName(eq(ProductControllerTest.STORE_ID),
            any(ProductActivateImageDTO.class),  eq(true));

    Mockito.when(this.imageService.isProductImagesActivated(STORE_ID, PRODUCT_CODE))
        .thenReturn(this.activateImage);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGES_NAME)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("skipReview", Boolean.TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.imageServiceWrapper)
        .activateAndUpdateImagesName(eq(ProductControllerTest.STORE_ID),
            any(ProductActivateImageDTO.class), eq(true));
    verify(this.imageService).isProductImagesActivated(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void testUpdateImagesNameWithEmptyProductCode() throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    Set<ActivateImageRequest> imageRequests = new HashSet<>();
    imageRequests.add(this.activateImageRequest);
    productActivateImageRequest.setImageRequests(imageRequests);
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productActivateImageRequest);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGES_NAME)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void testUpdateImagesNameWithoutImageActivationRequest() throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(PRODUCT_CODE);
    String requestString =
        ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productActivateImageRequest);

    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateSimpleMasterDataTest() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    request.setUpdatedBy(UPDATED_BY);
    request.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    SimpleMasterProductUpdateResponseDTO responseDTO =
        new SimpleMasterProductUpdateResponseDTO(PRODUCT_CODE, Boolean.TRUE);
    when(productServiceWrapper.updateMasterProductData(anyString(), any(SimpleMasterProductUpdateDTO.class)))
        .thenReturn(responseDTO);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(productServiceWrapper).updateMasterProductData(eq(STORE_ID),
        simpleMasterProductUpdateDTOArgumentCaptor.capture());
    assertEquals(PRODUCT_CODE, simpleMasterProductUpdateDTOArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void updateSimpleMasterDataException1Test() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    request.setUpdatedBy(null);
    request.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    SimpleMasterProductUpdateResponseDTO responseDTO =
        new SimpleMasterProductUpdateResponseDTO(PRODUCT_CODE, Boolean.TRUE);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

  }

  @Test
  public void updateSimpleMasterDataException2Test() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    request.setUpdatedBy(UPDATED_BY);
    request.setUpdatedDate(null);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    SimpleMasterProductUpdateResponseDTO responseDTO =
        new SimpleMasterProductUpdateResponseDTO(PRODUCT_CODE, Boolean.TRUE);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateSimpleMasterDataTestWithoutUpdatedByAndUpdatedDate() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateSimpleMasterDataTestWithoutProductCode() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    request.setUpdatedBy(UPDATED_BY);
    request.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateSimpleMasterDataExceptionTest() throws Exception {
    SimpleMasterProductUpdateRequest request = new SimpleMasterProductUpdateRequest.Builder()
        .productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(LENGTH).width(WIDTH).height(HEIGHT)
        .weight(WEIGHT).dangerousGoodsLevel(DANGEROUS_GOODS_LEVEL).brand(BRAND_NAME).build();
    request.setUpdatedBy(UPDATED_BY);
    request.setUpdatedDate(new Date());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    SimpleMasterProductUpdateResponseDTO responseDTO =
        new SimpleMasterProductUpdateResponseDTO(PRODUCT_CODE, Boolean.TRUE);
    when(productServiceWrapper.updateMasterProductData(anyString(), any(SimpleMasterProductUpdateDTO.class)))
        .thenThrow(NullPointerException.class);

      this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA).contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

      verify(productServiceWrapper).updateMasterProductData(anyString(), any(SimpleMasterProductUpdateDTO.class));
  }

  @Test
  public void filterProductImagesByProductIds_HappyFlow_Success() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(Arrays.asList(PRODUCT_ID));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    Mockito.when(this.imageService.filterProductImagesByProductIds(request.getLists()))
      .thenReturn(this.generateProductImageResp());

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(this.imageService).filterProductImagesByProductIds(request.getLists());
  }

  @Test
  public void filterProductImagesByProductIds_HappyFlow_Success_withFilters() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(Arrays.asList(PRODUCT_ID));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    List<ProductImageSingle> productImages = new ArrayList<>(this.generateProductImageResp());
    ProductImageSingle img = new ProductImageSingle();
    img.setLocationPath(LOCATION_PATH);
    img.setOriginalImage(null);
    img.setSequence(2);
    img.setMainImages(true);
    productImages.add(img);
    Mockito.when(this.imageService.filterProductImagesByProductIds(request.getLists())).thenReturn(productImages);

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_IMAGES).contentType(MediaType.APPLICATION_JSON)
            .content(requestString).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("mainImage", String.valueOf(Boolean.TRUE)).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.imageService).filterProductImagesByProductIds(request.getLists());
  }

  @Test
  public void filterProductImagesByProductIds_emptyProductIdsList() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(new ArrayList<>());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.content", equalTo(new ArrayList<>()))).andReturn();
  }
  @Test
  public void filterProductImagesByProductProductCodes_HappyFlow_Success() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(Arrays.asList(PRODUCT_CODE_1));
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    Mockito.when(this.imageService.filterProductImagesByProductCodes(STORE_ID, request.getLists()))
        .thenReturn(imagesDto);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_IMAGES_BY_PRODUCT_CODES)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME).param("page", "0")
                .param("size", "10")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.imageService).filterProductImagesByProductCodes(STORE_ID,
        request.getLists());
  }

  private List<ProductImageSingle> generateProductImageResp() {
    ProductImageSingle img = new ProductImageSingle();
    img.setLocationPath(LOCATION_PATH);
    img.setActive(true);
    img.setMainImages(true);
    img.setProductId(PRODUCT_ID);
    img.setOriginalImage(true);
    img.setSequence(0);
    return Collections.singletonList(img);
  }

  @Test
  public void sendProductPublishEventUsingUpdatedByTest() throws Exception {
    doNothing().when(service).publishProductByStoreIdAndUpdatedBy(anyString(), anyString());
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.PUBLISH_PRODUCT_BY_UPDATED_BY)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("updatedBy", ProductControllerTest.UPDATED_BY))
        .andExpect(status().isOk());
    verify(service).publishProductByStoreIdAndUpdatedBy(eq(STORE_ID), eq(UPDATED_BY));
  }

  @Test
  public void updateProductAndItemImagesByProductCodeWithoutSetDgParamTest() throws Exception {
    productAndItemImageRequest = new ProductAndItemImageRequest();
    Mockito.when(productServiceWrapper
        .updateProductAndItemImagesByProductCode(any(ProductAndItemImageRequest.class), anyString(),eq(true)))
        .thenReturn(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productAndItemImageRequest);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_AND_ITEM_IMAGE_DETAILS_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductAndItemImagesByProductCode(productAndItemImageRequestArgumentCaptor.capture(),
            anyString(),eq(true));
  }

  @Test
  public void updateProductAndItemImagesByProductCodeTest() throws Exception {
    productAndItemImageRequest = new ProductAndItemImageRequest();
    Mockito.when(productServiceWrapper
        .updateProductAndItemImagesByProductCode(any(ProductAndItemImageRequest.class), anyString(),eq(false)))
        .thenReturn(true);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productAndItemImageRequest);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_AND_ITEM_IMAGE_DETAILS_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON).content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("setDgLevel","0"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductAndItemImagesByProductCode(productAndItemImageRequestArgumentCaptor.capture(),
            anyString(),eq(false));
  }

  @Test
  public void deleteOriginalImagesByProductCode() throws Exception {
    Mockito.when(productServiceWrapper.deleteOriginalImagesByProductCode(anyString(), anyString()))
        .thenReturn(true);
    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_ORIGINAL_IMAGES, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .deleteOriginalImagesByProductCode(anyString(), anyString());
  }

  @Test
  public void getItemCodesListTest() throws Exception {
    Mockito.when(productServiceWrapper.getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE))
        .thenReturn(new PageImpl<>(Arrays.asList(simpleItemDetailResponse), PageRequest.of(PAGE_NUMBER, PAGE_SIZE),
            TOTAL_RECORDS));
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_LISTING_BY_PRODUCT_CODE, PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content[0].itemCode", equalTo(SKU_CODE_1)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(PRODUCT_CODE)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE);
  }

  @Test
  public void getItemCodesListTest_expectException() throws Exception {
    Mockito.when(productServiceWrapper.getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE))
        .thenThrow(Exception.class);
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_LISTING_BY_PRODUCT_CODE, PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE);
  }

  @Test
  public void getItemCodesListTest_expectApplicationRuntimeException() throws Exception {
    Mockito.when(productServiceWrapper.getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_LISTING_BY_PRODUCT_CODE, PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE_NUMBER, PAGE_SIZE);
  }
  @Test
  public void getProductItemsByUPCCodeAndCategoryIdsTest() throws Exception {
    Page<ProductItem> result = new PageImpl<>(productItems);
    when(productItemServiceWrapper
        .findByUPCCodeAndCategoryIds(eq(STORE_ID), any(UPCCodeSearchRequest.class),
            Mockito.anyBoolean(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(result);
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_UPC_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(upcCodeSearchRequest))).andExpect(status().isOk());
    verify(productItemServiceWrapper)
        .findByUPCCodeAndCategoryIds(eq(STORE_ID), eq(upcCodeSearchRequest), Mockito.anyBoolean(),
            Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  public void getProductItemsByUPCCodeAndCategoryIdsExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(this.productItemServiceWrapper).
        findByUPCCodeAndCategoryIds(eq(STORE_ID), any(UPCCodeSearchRequest.class), Mockito.anyBoolean(),
            Mockito.anyInt(), Mockito.anyInt());
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_UPC_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(upcCodeSearchRequest))).andExpect(status().isOk());
    verify(this.productItemServiceWrapper)
        .findByUPCCodeAndCategoryIds(eq(STORE_ID), eq(upcCodeSearchRequest), Mockito.anyBoolean(),
            Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  public void getCategoryHierarchyByUPCCodeWithProductCountTest() throws Exception {
    when(service.getCategoryHierarchyByUPCCode(ProductControllerTest.STORE_ID, UPC_CODE_1, true))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_CATEGORY_HIERARCHY_BY_UPCCODE_PRODUCT_COUNT)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("updatedBy", ProductControllerTest.UPDATED_BY).param("upcCode", ProductControllerTest.UPC_CODE_1)
        .param("isOnlyExternal", Boolean.TRUE.toString())).andExpect(status().isOk());
    verify(service).getCategoryHierarchyByUPCCode(ProductControllerTest.STORE_ID, UPC_CODE_1, true);
  }

  @Test
  public void updateProductCategoryTest() throws Exception {
    Mockito
        .when(productServiceWrapper.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true))
        .thenReturn(new CategorySummaryResponse(CATEGORY_CODE, CATEGORY_NAME, CATEGORY_ID, CATEGORY_ID, false));
    this.mockMvc
        .perform(
            put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CATEGORY, PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("categoryCode", ProductControllerTest.CATEGORY_CODE))
            .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true);
  }

  @Test
  public void updateProductCategoryApplicationExceptionTest() throws Exception {
    ApplicationRuntimeException applicationRuntimeException = new ApplicationRuntimeException();
    applicationRuntimeException.setErrorCodes(ErrorCategory.VALIDATION);
    Mockito.doThrow(applicationRuntimeException).when(productServiceWrapper)
        .updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true);
    this.mockMvc
        .perform(
            put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CATEGORY, PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("categoryCode", ProductControllerTest.CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true);
  }

  @Test
  public void updateProductCategoryExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productServiceWrapper)
        .updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true);
    this.mockMvc
        .perform(
            put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CATEGORY, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("categoryCode", ProductControllerTest.CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true,true);
  }

  @Test
  public void getProductItemDetailByItemCodeTest() throws Exception {
    Mockito.when(this.productServiceWrapper.getItemResponseByItemCode(STORE_ID, SKU_CODE_1))
        .thenReturn(new ProductItemCompleteResponse());
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_DETAIL_BY_ITEM_CODE, SKU_CODE_1)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("updatedBy", ProductControllerTest.UPDATED_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    Mockito.verify(this.productServiceWrapper).getItemResponseByItemCode(STORE_ID, SKU_CODE_1);
  }

  @Test
  public void getProductItemDetailByItemCode_expectExceptionTest() throws Exception {
    Mockito.when(this.productServiceWrapper.getItemResponseByItemCode(STORE_ID, SKU_CODE_1))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_DETAIL_BY_ITEM_CODE, SKU_CODE_1)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("updatedBy", ProductControllerTest.UPDATED_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    Mockito.verify(this.productServiceWrapper).getItemResponseByItemCode(STORE_ID, SKU_CODE_1);
  }

  @Test
  public void getProductDetailsWithoutItemsTest() throws Exception {
    product.getProductCategories().get(0).getCategory().setCategoryCode(CATEGORY_CODE);
    when(this.categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Arrays.asList(category));
    when(this.productServiceWrapper.getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_CODE)).thenReturn(product);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL_BY_PRODUCT_CODE_WITHOUT_ITEM, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_CODE);
    verify(this.categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void getProductDetailsWithoutItemsTest_expectException() throws Exception {
    when(this.productServiceWrapper.getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_CODE)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL_BY_PRODUCT_CODE_WITHOUT_ITEM, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.productServiceWrapper).getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void deleteImagesForDeletedProductTest() throws Exception {
    Mockito.doNothing().when(this.service)
        .deleteImagesForDeletedProduct(STORE_ID, Integer.parseInt(DAYS), Integer.parseInt(DAYSPAN),
            Integer.parseInt(BATCH_SIZE));
    this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IMAGES_FOR_DELETED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("days", ProductControllerTest.DAYS).param("daySpan", ProductControllerTest.DAYSPAN)
        .param("batchSize", ProductControllerTest.BATCH_SIZE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.service).deleteImagesForDeletedProduct(STORE_ID, Integer.parseInt(DAYS), Integer.parseInt(DAYSPAN),
        Integer.parseInt(BATCH_SIZE));
  }

  @Test
  public void deleteImagesForDeletedProductExceptionTest() {
    try {
      this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IMAGES_FOR_DELETED_PRODUCT)
          .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME).param("days", ProductControllerTest.DAYS)
          .param("daySpan", ProductControllerTest.DAYSPAN).param("batchSize", "-10")).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ErrorMessage.NOT_VALID_PARAMETERS.getMessage()));
    }
  }

  @Test
  public void deleteImagesForUpdatedProductTest() throws Exception {
    Mockito.doNothing().when(this.service)
        .deleteImagesForUpdatedProduct(Integer.parseInt(DAYS), Integer.parseInt(BATCH_SIZE));
    this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IMAGES_FOR_UPDATED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("days", ProductControllerTest.DAYS).param("batchSize", ProductControllerTest.BATCH_SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.service).deleteImagesForUpdatedProduct(Integer.parseInt(DAYS), Integer.parseInt(BATCH_SIZE));
  }

  @Test
  public void deleteImagesForUpdatedProductExceptionTest() {
    try {
      this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_IMAGES_FOR_UPDATED_PRODUCT)
          .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME).param("days", ProductControllerTest.DAYS)
          .param("daySpan", ProductControllerTest.DAYSPAN).param("batchSize", "-10")).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ErrorMessage.NOT_VALID_PARAMETERS.getMessage()));
    }
  }

  @Test
  public void getSalesCategoryMappingTest() throws Exception {
    Mockito.when(this.service.getProductSalesCategoryMapping(STORE_ID, PRODUCT_CODE, true))
        .thenReturn(new ProductSalesCategoryMappingResponse());
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_SALES_CATEGORY_BY_PRODUCT_CODE, PRODUCT_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.DEFAULT_USERNAME)
    .param("ignoreHalalCategories", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).getProductSalesCategoryMapping(STORE_ID, PRODUCT_CODE, true);
  }

  @Test
  public void getSalesCategoryMapping_exceptionTest() throws Exception {
    Mockito.when(this.service.getProductSalesCategoryMapping(STORE_ID, PRODUCT_CODE, false))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_SALES_CATEGORY_BY_PRODUCT_CODE, PRODUCT_CODE).accept(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).getProductSalesCategoryMapping(STORE_ID, PRODUCT_CODE, false);
  }

  @Test
  public void clearProductCacheByProductCodesTest() throws Exception {
    this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.CLEAR_PRODUCT_CACHE_BY_PRODUCT_CODES)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Arrays.asList(PRODUCT_CODE)))).andExpect(status().isOk());
    verify(service).evictProductCacheByProductCodes(STORE_ID, productCodes);
  }

  @Test
  public void clearProductCacheByProductCodesExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(service).evictProductCacheByProductCodes(STORE_ID, productCodes);
    this.mockMvc.perform(put(ProductApiPath.BASE_PATH + ProductApiPath.CLEAR_PRODUCT_CACHE_BY_PRODUCT_CODES)
        .param("storeId", ProductControllerTest.STORE_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)
        .param("channelId", ProductControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductControllerTest.DEFAULT_CLIENT_ID).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Arrays.asList(PRODUCT_CODE)))).andExpect(status().isOk());
    verify(service).evictProductCacheByProductCodes(STORE_ID, productCodes);
  }

  @Test
  public void updateProductScoreTest() throws Exception {
    Mockito.doNothing().when(this.productServiceWrapper).updateProductScore(STORE_ID, this.pageable, true, 1000);
    URI uri = new URIBuilder().setPath(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_SCORE)
        .addParameter("storeId", STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("productScoreUpdateSwitch", Boolean.TRUE.toString()).addParameter("productScoreBatchSize", "1000")
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(status().isOk());
    verify(this.productServiceWrapper).updateProductScore(STORE_ID, this.pageable, true, PRODUCT_SCORE_BATCH_SIZE);
  }

  @Test
  public void migrateProductTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    attribute.setId(ID);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString()))
        .thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductWithProductRequestNullTest() throws Exception {
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("oldProductCode", ProductControllerTest.PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service)
        .copyProduct(eq(PRODUCT_CODE), eq(NEW_PRODUCT_CODE), eq(STORE_ID), eq(null),
            eq(CREATED_MERCHANT));
  }

  @Test
  public void migrateProductExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(service)
        .copyProduct(eq(PRODUCT_CODE), eq(NEW_PRODUCT_CODE), eq(STORE_ID), eq(null),
            eq(CREATED_MERCHANT));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("oldProductCode", ProductControllerTest.PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    verify(this.service)
        .copyProduct(eq(PRODUCT_CODE), eq(NEW_PRODUCT_CODE), eq(STORE_ID), eq(null),
            eq(CREATED_MERCHANT));
  }

  @Test
  public void migrateProductWithProductAttributesEmptyTest() throws Exception {
    productRequest.setProductAttributes(new ArrayList<>());
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andReturn();
    verify(this.attributeService)
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(),
            any(), any());
  }

  @Test
  public void migrateProductWithEmptyProductAttributesValuesTest() throws Exception {
    productAttributes.get(0).setProductAttributeValues(null);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.allowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(3))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithProductDescriptiveAttributesTest() throws Exception {
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(DescriptiveAttributeValueType.PREDEFINED.name());
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.allowedAttributeValueService, times(4))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithProductDescriptiveAttributesAndNullAttributeResponseTest() throws Exception {
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(DescriptiveAttributeValueType.PREDEFINED.name());
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithProductPredefinedAttributesAndNullAttributeResponseTest() throws Exception {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithPredefinedAllowedAttributesValueNullTest() throws Exception {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(null);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    List<Attribute> attributeList = new ArrayList<>();
    attributeList.add(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(attributeList);
    Mockito.when(attributeService
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), anyString()))
        .thenReturn(new Attribute());

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
  }

  @Test
  public void migrateProductWithPredefinedAllowedAttributeCodeNullTest() throws Exception {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(null);
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithAllowedAttributeValueNullTest() throws Exception {
    productAttributes.get(0).getProductAttributeValues().get(0).setAllowedAttributeValue(null);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(3))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithAllowedAttributeCodeTest() throws Exception {
    productAttributes.get(0).getProductAttributeValues().get(0).getAllowedAttributeValue()
        .setAllowedAttributeCode(ATTRIBUTE_CODE);
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(3)).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(
        any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAllowedAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.allowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductWithNullAttributeResponseTest() throws Exception {
    productRequest.setProductAttributes(productAttributes);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductAttributeNullTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString()))
        .thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    productRequest.getProductAttributes().get(0).getProductAttributeValues().get(0).getAllowedAttributeValue()
        .setAllowedAttributeCode("ALLOWED_ATTRIBUTE_CODE");
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.allowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAllowedAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.attributeService, times(4)).findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
        anyString(), anyString());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  private List<ProductAttributeRequest> getProductAttributes() {
    List<ProductAttributeRequest> productAttributeRequests =  new ArrayList<>();

    List<AllowedAttributeValueRequest> allowedAttributeWarna = new ArrayList<AllowedAttributeValueRequest>();
    allowedAttributeWarna
        .add(new AllowedAttributeValueRequest("Merah", Integer.valueOf(1), ProductControllerTest.STORE_ID));
    allowedAttributeWarna.add(new AllowedAttributeValueRequest("Biru", Integer.valueOf(2), ProductControllerTest.STORE_ID));
    allowedAttributeWarna
        .add(new AllowedAttributeValueRequest("Kuning", Integer.valueOf(3), ProductControllerTest.STORE_ID));
    AttributeRequest warna =
        new AttributeRequest("Warna", "Kode Warna", AttributeType.DEFINING_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);
    warna.setAllowedAttributeValues(allowedAttributeWarna);

    AttributeRequest ukuran =
        new AttributeRequest("Ukuran", "Kode Ukuran", AttributeType.DESCRIPTIVE_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);

    AttributeRequest ukuran1 =
        new AttributeRequest("Ukuran", "Kode Ukuran", AttributeType.DESCRIPTIVE_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);

    AttributeRequest brand =
        new AttributeRequest("Brand", "Kode Ukuran", AttributeType.PREDEFINED_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);

    AttributeRequest garansi =
        new AttributeRequest("garansi", "garansi", AttributeType.PREDEFINED_ATTRIBUTE, true, true,
            ProductControllerTest.STORE_ID);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValue.setValue(VALUE);
    brand.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValue));

    ProductAttributeRequest productAttributeWarna =
        new ProductAttributeRequest(warna, "warna", false, Integer.valueOf(1), ProductControllerTest.STORE_ID);
    productAttributeWarna.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeWarna.get(0), null, DescriptiveAttributeValueType.NONE,
            ProductControllerTest.STORE_ID));
    productAttributeRequests.add(productAttributeWarna);

    ProductAttributeRequest productAttributeUkuran =
        new ProductAttributeRequest(ukuran, "ukuran", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeUkuran.getProductAttributeValues().add(
        new ProductAttributeValueRequest(null, "XL", DescriptiveAttributeValueType.NONE,
            ProductControllerTest.STORE_ID));
    productAttributeRequests.add(productAttributeUkuran);

    ProductAttributeRequest productAttributeUkuran1 =
        new ProductAttributeRequest(ukuran1, "ukuran1", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeUkuran1.getProductAttributeValues().add(
        new ProductAttributeValueRequest(null, null, DescriptiveAttributeValueType.NONE,
            ProductControllerTest.STORE_ID));
    productAttributeRequests.add(productAttributeUkuran1);

    ProductAttributeRequest productAttributeBrand =
        new ProductAttributeRequest(brand, "brand", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeBrand.setProductAttributeValues(Arrays.asList(new ProductAttributeValueRequest()));
    productAttributeBrand.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeBrand.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(ID);
    productAttributeBrand.getProductAttributeValues().get(0).setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productAttributeRequests.add(productAttributeBrand);

    ProductAttributeRequest productAttributeGaransi =
        new ProductAttributeRequest(garansi, "garansi", false, Integer.valueOf(2), ProductControllerTest.STORE_ID);
    productAttributeGaransi.setProductAttributeValues(Arrays.asList(new ProductAttributeValueRequest()));
    productAttributeGaransi.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeGaransi.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(ID);
    productAttributeGaransi.getProductAttributeValues().get(0).setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productAttributeRequests.add(productAttributeGaransi);

    return  productAttributeRequests;
  }

  @Test
  public void migrateProductNullAttributeTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductMandatoryAttributeTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setMandatory(true);
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductDescriptiveBrandAttributeTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    File mainDirectory = new File(BASE_FOLDER);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0).setDescriptiveAttributeValue("Brand");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(1))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductCreatingBrandTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));

    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().remove(3);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(1))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(1)).findDetailByStoreIdAndAttributeCode(anyString(), anyString());
    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
  }

  @Test
  public void migrateProductMandatoryAttributePredefinedTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setMandatory(true);
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode("CODE");
    this.productRequest.getProductAttributes().get(4).getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductMandatoryAttributePredefinedEmptyTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setMandatory(true);
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().get(4).getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(null);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(1))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(1))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void migrateProductMandatoryAttributePredefinedEmptyNonMandatoryTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().get(4).getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(null);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(any(), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(2))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(),
            any(), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void getProductItemBySkuCodesTest() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(this.productItem.getSkuCode());
    SkuCodesRequest request = new SkuCodesRequest(skuCodes);
    request.setFetchArchived(true);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    this.productItems.get(0).setProductItemAttributeValues(new ArrayList<ProductItemAttributeValue>());
    this.productItems.get(0).getProductItemAttributeValues().add(productItemAttributeValue);
    this.productItems.get(0).getProductItemAttributeValues().get(0)
        .setAttribute(new Attribute(BRAND_NAME, null, true, STORE_ID));

    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath("xyz");
    this.productItems.get(0).setProductItemImages(new ArrayList<ProductItemImage>());
    this.productItems.get(0).getProductItemImages().add(productItemImage);
    when(this.productItemService.findBySkuCodes(STORE_ID, skuCodes, true)).thenReturn(this.productItems);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_BY_SKU_CODES).contentType(MediaType.APPLICATION_JSON)
            .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(this.productItemService, Mockito.times(1)).findBySkuCodes(STORE_ID, skuCodes, true);
  }

  @Test
  public void migrateFinalImageFromGfsToGcsTest() throws Exception {
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add(PRODUCT_CODE);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_FINAL_IMAGE_FROM_GFS_TO_GCS).contentType(
                MediaType.APPLICATION_JSON).content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productCodeList))
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(productServiceWrapper).migrateFinalImageFromGfsToGcs(productCodeList, ProductControllerTest.STORE_ID);
  }

  @Test
  public void migrateFinalImageFromGfsToGcsExceptionTest() throws Exception {
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add(PRODUCT_CODE);
    Mockito.doThrow(new NullPointerException()).when(productServiceWrapper)
        .migrateFinalImageFromGfsToGcs(productCodeList, ProductControllerTest.STORE_ID);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_FINAL_IMAGE_FROM_GFS_TO_GCS).contentType(
                    MediaType.APPLICATION_JSON).content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productCodeList))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).migrateFinalImageFromGfsToGcs(productCodeList, ProductControllerTest.STORE_ID);
  }

  @Test
  public void editItemUpcCodeTest() throws Exception {
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_UPDATE_UPCCODE).contentType(MediaType.APPLICATION_JSON)
            .content(
                ProductControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(productItemUpcCodeUpdateRequest)))
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.productItemServiceWrapper, Mockito.times(1))
        .updateProductItemUpcCodeAndEvictCache(eq(ProductControllerTest.STORE_ID),
            productItemUpcCodeUpdateRequestArgumentCaptor.capture(), eq(this.product.getProductCode()));
    Assertions.assertEquals(UPC_CODE_1, productItemUpcCodeUpdateRequestArgumentCaptor.getValue().get(0).getUpcCode());
    Assertions.assertEquals(SKU_CODE_1, productItemUpcCodeUpdateRequestArgumentCaptor.getValue().get(0).getSkuCode());
  }

  @Test
  public void editItemUpcCodeExceptionTest() {
    try {
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_UPDATE_UPCCODE).contentType(MediaType.APPLICATION_JSON)
              .content(
                  ProductControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(productItemUpcCodeUpdateRequest)))
              .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
              .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
              .param("requestId", ProductControllerTest.REQUEST_ID)
              .param("username", ProductControllerTest.DEFAULT_USERNAME)
              .param("productCode", StringUtils.EMPTY)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void updateProductItemImagesByProductCodeTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    GdnRestListResponse<LocationPathAndCommonImage> gdnRestListResponse = new GdnRestListResponse<>();
    gdnRestListResponse.setSuccess(true);
    gdnRestListResponse.setRequestId(REQUEST_ID);
    Mockito.when(productServiceWrapper
        .updateProductItemImagesByProductCode(any(ProductItemImageUpdateRequest.class), anyString(), anyString()))
        .thenReturn(gdnRestListResponse);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productItemImageUpdateRequest);

    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_ITEM_IMAGES)
        .contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    Mockito.verify(productServiceWrapper)
        .updateProductItemImagesByProductCode(productItemImageUpdateRequestArgumentCaptor.capture(),
            anyString(), eq(REQUEST_ID));
  }

  @Test
  public void migrateProductCreatingBrandNotFoundTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any())).thenReturn(null);
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString()))
        .thenReturn(new Attribute());

    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().remove(3);
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCode(anyString(), anyString());
  }

  @Test
  public void migrateProductCreatingBrandNotFound1Test() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(null);
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(anyString(), anyString())).thenReturn(null);
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString()))
        .thenReturn(new Attribute());

    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode("ABCD");
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("ABCD");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(any(), any());
    verify(this.attributeService, times(2)).findDetailByStoreIdAndAttributeCode(anyString(), anyString());
  }

  @Test
  public void migrateProductCreatingBrandNotFound2Test() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(null);
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(anyString(), anyString())).thenReturn(new PredefinedAllowedAttributeValue());
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString()))
        .thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode("ABCD");
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("ABCD");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);

    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(1))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(any(), any());
    verify(this.attributeService, times(1)).findDetailByStoreIdAndAttributeCode(anyString(), anyString());
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void migrateProductDescriptiveBrandSizeAttributeTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    File mainDirectory = new File(BASE_FOLDER);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString())).thenReturn(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(Arrays.asList(new Attribute()));
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getProductAttributes().get(1).getProductAttributeValues().get(0).setDescriptiveAttributeValue(
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
    this.productRequest.getProductAttributes().get(3).getProductAttributeValues().get(0).setDescriptiveAttributeValue("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(1))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), anyString());
    verify(this.predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
  }

  @Test
  public void getItemNameByProductItemIdTest() throws Exception {
    when(productItemService.findByStoreIdAndId(STORE_ID, ID2)).thenReturn(productItem);

    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_NAME_BY_PRODUCT_ITEM_ID, ID2)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username",ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(productItem.getGeneratedItemName())));
    verify(productItemService).findByStoreIdAndId(STORE_ID, ID2);
  }

  @Test
  public void getItemNameByProductItemIdExceptionTest() throws Exception {
    when(productItemService.findByStoreIdAndId(STORE_ID, ID2)).thenThrow(RuntimeException.class);
    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_NAME_BY_PRODUCT_ITEM_ID, ID2)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username",ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)));
    verify(productItemService).findByStoreIdAndId(STORE_ID, ID2);
  }

  @Test
  public void getItemNameByUpcCodeAndProductCodeTest() throws Exception {
    when(productItemService.getItemNameByUPCCodeAndProductCode(any(), any(), any()))
        .thenReturn(Arrays.asList(NAME));

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_NAME_BY_UPC_CODE_PRODUCT_CODE)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username",ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", UPC_CODE_1)
            .param("productCode", PRODUCT_CODE)
            .param("skuCode", SKU_CODE_1))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productItemService).getItemNameByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void getItemNameByUpcCodeAndProductCodeExceptionTest() throws Exception {
    when(productItemService.getItemNameByUPCCodeAndProductCode(any(), any(), any()))
        .thenThrow(RuntimeException.class);

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_NAME_BY_UPC_CODE_PRODUCT_CODE)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username",ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", UPC_CODE_1)
            .param("productCode", PRODUCT_CODE)
            .param("skuCode", SKU_CODE_1))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(productItemService).getItemNameByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void getItemNameByUpcCodeAndProductCodeEmptyResponseTest() throws Exception {
    when(productItemService.getItemNameByUPCCodeAndProductCode(any(), any(), any()))
        .thenReturn(new ArrayList<>());

    this.mockMvc
        .perform(get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_NAME_BY_UPC_CODE_PRODUCT_CODE)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username",ProductControllerTest.DEFAULT_USERNAME)
            .param("upcCode", UPC_CODE_1)
            .param("productCode", PRODUCT_CODE)
            .param("skuCode", SKU_CODE_1))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productItemService).getItemNameByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void migrateProductItemAttributeNullTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(),
            any(), any()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(Attribute.COLUMN_ATTRIBUTE_CODE);
    attribute.setId(ID);
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCode(anyString(), anyString()))
        .thenReturn(attribute);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(new ArrayList());
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, productRequest.getProductCode());
    directory1.mkdir();
    File file1 = new File(directory1, "image.jpg");
    file1.createNewFile();
    this.productRequest.getImages().addAll(new ArrayList<Image>());
    Image image = new Image();
    image.setId(STORE_ID);
    image.setLocationPath(productRequest.getProductCode() + File.separator + "image.jpg");
    this.productRequest.setProductAttributes(null);
    this.productRequest.setProductAttributes(getProductAttributes());
    this.productRequest.getImages().add(image);
    this.productRequest.getProductItems().get(0).setStoreId("");
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
            .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(this.service).copyProduct(eq(null), eq(NEW_PRODUCT_CODE), eq(STORE_ID),
        productArgumentCaptor.capture(), eq(CREATED_MERCHANT));
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(ProductControllerTest.STORE_ID,
            this.category.getCategoryCode());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(eq(ProductControllerTest.STORE_ID), any(),
            any(), any(), any());
    verify(this.allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString());
    verify(this.attributeService, times(1))
        .findDetailByStoreIdAndAttributeCode(eq(Constants.DEFAULT_STORE_ID), any());
    verify(this.predefinedAllowedAttributeValueService, times(1))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(),
            any(), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    Assertions.assertEquals(productRequest.getProductItems().size(),
        productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(productRequest.getImages().get(0).isMainImages(),
        productArgumentCaptor.getValue().getProductImages().get(0).isMainImages());
  }

  @Test
  public void getImagesForScalingByProductCodeTest() throws Exception {
    when(this.service.getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE))
        .thenReturn(this.product);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FETCH_IMAGES_FOR_SCALING, ProductControllerTest.PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("clientId", ProductControllerTest.CLIENT_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getImagesForScalingByProductCodeExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.service)
        .getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FETCH_IMAGES_FOR_SCALING, ProductControllerTest.PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("clientId", ProductControllerTest.CLIENT_ID))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getImagesForScalingByProductCodeException1Test() throws Exception {
    Mockito.doThrow(new Exception()).when(this.service)
        .getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.FETCH_IMAGES_FOR_SCALING, ProductControllerTest.PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)
            .param("clientId", ProductControllerTest.CLIENT_ID))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).getImagesByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getProductItemIdsBySkuCodeTest() throws Exception {
    when(productItemService.getProductItemIdsBySkuCode(any(), any()))
        .thenReturn(new HashMap<>());
      this.mockMvc.perform(
          post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_ITEM_IDS_BY_SKU_CODES).contentType(MediaType.APPLICATION_JSON)
              .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(new SkuCodesRequest(new ArrayList<>())))
              .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
              .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
              .param("requestId", ProductControllerTest.REQUEST_ID)
              .param("username", ProductControllerTest.DEFAULT_USERNAME)
              .param("productCode", StringUtils.EMPTY)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productItemService).getProductItemIdsBySkuCode(any(), any());
  }

  @Test
  public void updateAndMarkProductForNeedCorrectionTest() throws Exception {
    doNothing().when(productServiceWrapper).updateAndMarkForNeedRevision(product);
    this.mockMvc.perform(
            put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_NEED_REVISION, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateAndMarkForNeedRevision(any(Product.class));
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
    verify(this.attributeService, times(1)).findByAttributeIds(eq(ProductControllerTest.STORE_ID)
      , anyList());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateAndMarkProductForNeedCorrection_expectExceptionTest() throws Exception {
    when(this.categoryService.findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId())).thenThrow(
        RuntimeException.class);
    this.mockMvc.perform(
            put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_NEED_REVISION, PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
  }

  @Test
  public void updateVatFlagBySkuCodesTest() throws Exception {
    BatchVatUpdateRequest batchVatUpdateRequest = new BatchVatUpdateRequest();
    Mockito.when(this.productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, DEFAULT_USERNAME,
        batchVatUpdateRequest)).thenReturn(Collections.emptyList());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.BATCH_VAT_UPDATE_BY_SKU_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(batchVatUpdateRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    Mockito.verify(this.productServiceWrapper).updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, DEFAULT_USERNAME,
        batchVatUpdateRequest);
  }

  @Test
  public void updateVatFlagBySkuCodes_exceptionTest() throws Exception {
    BatchVatUpdateRequest batchVatUpdateRequest = new BatchVatUpdateRequest();
    Mockito.when(this.productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, DEFAULT_USERNAME,
        batchVatUpdateRequest)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.BATCH_VAT_UPDATE_BY_SKU_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(batchVatUpdateRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    Mockito.verify(this.productServiceWrapper).updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, DEFAULT_USERNAME,
        batchVatUpdateRequest);
  }

  @Test
  public void getMasterProductDetailsByItemCodeTest() throws Exception {
    when(productServiceWrapper.getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE_1))
        .thenReturn(new ProductMasterDataResponse());
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.MASTER_DETAIL_BY_ITEM_CODE, SKU_CODE_1)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE_1);
  }

  @Test
  public void getMasterProductDetailsByItemCodeExceptionTest() throws Exception {
    when(productServiceWrapper.getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE_1))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(ProductApiPath.BASE_PATH + ProductApiPath.MASTER_DETAIL_BY_ITEM_CODE, SKU_CODE_1)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE_1);
  }

  @Test
  public void updateImagesTest() throws Exception {
    when(productServiceWrapper.updateImages(any(), any())).thenReturn(new HashMap<>());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_IMAGES).contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(new ProductImageEditRequest()))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateImages(any(), any());
  }

  @Test
  public void updateImagesExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productServiceWrapper).updateImages(any(), any());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_IMAGES).contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(new ProductImageEditRequest()))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateImages(any(), any());
  }

  @Test
  public void updateCommonImagesTest() throws Exception {
    when(productServiceWrapper.updateCommonImages(any(), any())).thenReturn(new HashMap<>());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_COMMON_IMAGES).contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(new ProductImageEditRequest())))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateCommonImages(any(), any());
  }

  @Test
  public void updateCommonImagesExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productServiceWrapper).updateCommonImages(any(), any());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_COMMON_IMAGES).contentType(MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(new ProductImageEditRequest())))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateCommonImages(any(), any());
  }

  @Test
  public void getProductImagesByProductCodeTest() throws Exception {
    ImageResponse imageResponse =
      ImageResponse.builder().mainImage(Boolean.TRUE).originalImage(Boolean.TRUE)
        .active(Boolean.TRUE).locationPath(LOCATION_PATH).sequence(1).build();
    when(
      this.productServiceWrapper.findProductImagesByProductCode(STORE_ID, PRODUCT_CODE, true)).thenReturn(
      Arrays.asList(imageResponse));
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_IMAGES_BY_PRODUCT_CODE,
          PRODUCT_CODE).param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).findProductImagesByProductCode(STORE_ID, PRODUCT_CODE, true);
  }

  @Test
  public void getProductImagesByProductCodeExceptionTest() throws Exception {
    ImageResponse imageResponse =
      ImageResponse.builder().mainImage(Boolean.TRUE).originalImage(Boolean.TRUE)
        .active(Boolean.TRUE).locationPath(LOCATION_PATH).sequence(1).build();
    Mockito.doThrow(RuntimeException.class).when(productServiceWrapper).findProductImagesByProductCode(STORE_ID, PRODUCT_CODE, true);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_IMAGES_BY_PRODUCT_CODE,
          PRODUCT_CODE).param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).findProductImagesByProductCode(STORE_ID, PRODUCT_CODE, true);
  }

  @Test
  public void getProductItemImagesByItemCodeTest() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(SOURCE_ITEM_CODE1);
    SkuCodesRequest request = new SkuCodesRequest(skuCodes);
    ImageResponse imageResponse =
      ImageResponse.builder().mainImage(Boolean.TRUE).originalImage(Boolean.TRUE)
        .active(Boolean.TRUE).locationPath(LOCATION_PATH).sequence(1).build();
    ItemImageResponse itemImageResponse =
      ItemImageResponse.builder().imageResponses(Arrays.asList(imageResponse))
        .itemCode(SOURCE_ITEM_CODE1).build();
    when(
      this.productItemServiceWrapper.findProductItemImagesByItemCodes(STORE_ID,
        Collections.singletonList(SOURCE_ITEM_CODE1), true, null)).thenReturn(
      Arrays.asList(itemImageResponse));
    this.mockMvc.perform(
      post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_IMAGES_BY_ITEM_CODES).contentType(MediaType.APPLICATION_JSON)
        .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request))
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(productItemServiceWrapper).findProductItemImagesByItemCodes(STORE_ID, Collections.singletonList(SOURCE_ITEM_CODE1), true,
      null);
  }

  @Test
  public void getProductItemImagesByItemCodeExceptionTest() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(SOURCE_ITEM_CODE1);
    SkuCodesRequest request = new SkuCodesRequest(skuCodes);
    ImageResponse imageResponse =
      ImageResponse.builder().mainImage(Boolean.TRUE).originalImage(Boolean.TRUE)
        .active(Boolean.TRUE).locationPath(LOCATION_PATH).sequence(1).build();
    ItemImageResponse itemImageResponse =
      ItemImageResponse.builder().imageResponses(Arrays.asList(imageResponse))
        .itemCode(SOURCE_ITEM_CODE1).build();
    doThrow(RuntimeException.class).when(
      this.productItemServiceWrapper).findProductItemImagesByItemCodes(STORE_ID,
        Collections.singletonList(SOURCE_ITEM_CODE1), true, null);
    this.mockMvc.perform(
      post(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_IMAGES_BY_ITEM_CODES).contentType(MediaType.APPLICATION_JSON)
        .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request))
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(jsonPath("$.success", equalTo(false)));
    verify(productItemServiceWrapper).findProductItemImagesByItemCodes(STORE_ID, Collections.singletonList(SOURCE_ITEM_CODE1), true,
      null);
  }

  @Test
  public void getProductAndAttributeDetailsByProductCodeTest() throws Exception {
    when(
        this.productServiceWrapper.getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE, false)).thenReturn(product);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_AND_ATTRIBUTE_DETAIL_BY_PRODUCT_CODE,
            PRODUCT_CODE).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE, false);
  }

  @Test
  public void getProductAndAttributeDetailsByProductCodeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productServiceWrapper).getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE,
        false);
    this.mockMvc.perform(
        get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_AND_ATTRIBUTE_DETAIL_BY_PRODUCT_CODE,
            PRODUCT_CODE).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE, false);
  }

  @Test
  public void getProductAttributeDetailsByProductCodeTest() throws Exception {
    when(this.productServiceWrapper.getProductAttributeDetailsByProductId(STORE_ID, PRODUCT_ID)).thenReturn(
        new ArrayList<>());
    this.mockMvc.perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_ATTRIBUTE_DETAIL_BY_PRODUCT_ID, PRODUCT_ID).param(
                    "storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).getProductAttributeDetailsByProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void getProductAttributeDetailsByProductCodeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productServiceWrapper)
        .getProductAttributeDetailsByProductId(STORE_ID, PRODUCT_ID);
    this.mockMvc.perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_ATTRIBUTE_DETAIL_BY_PRODUCT_ID, PRODUCT_ID).param(
                    "storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).getProductAttributeDetailsByProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void autoFillProductAttributeTest() throws Exception {
    when(this.productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE)).thenReturn(
        Arrays.asList(new AttributeHistoryResponse(ATTRIBUTE_CODE, ATTRIBUTE_VALUE)));
    this.mockMvc.perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.AUTO_FILL_PRODUCT_ATTRIBUTES, PRODUCT_CODE).param("storeId",
                    ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).autoFillAttributes(STORE_ID, PRODUCT_CODE);
  }


  @Test
  public void autoFillProductAttributeErrorTest() throws Exception {
    when(this.productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
            get(ProductApiPath.BASE_PATH + ProductApiPath.AUTO_FILL_PRODUCT_ATTRIBUTES, PRODUCT_CODE).param("storeId",
                    ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productServiceWrapper).autoFillAttributes(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getItemCodeByUpcCodeAndProductCodeTest() throws Exception {
    when(productItemService.getItemCodeByUPCCodeAndProductCode(any(), any(), any())).thenReturn(Arrays.asList(NAME));
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_CODE_BY_UPC_CODE_PRODUCT_CODE).contentType(
                    MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productItemUpcCodesSkuCodesRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME).param("upcCode", UPC_CODE_1)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productItemService).getItemCodeByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void getSkuCodesByProductItemIdsTest() throws Exception {
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_CODES_BY_IDS).contentType(MediaType.APPLICATION_JSON)
                .content(
                    ProductControllerTest.OBJECT_MAPPER.writeValueAsString(new SimpleStringListRequest(new ArrayList<>())))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME).param("upcCode", UPC_CODE_1)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productItemService).getBySkuCodeByProductItemIds(STORE_ID, new ArrayList<>());
  }

  @Test
  public void getItemCodeByUpcCodeAndProductCodeExceptionTest() throws Exception {
    when(productItemService.getItemCodeByUPCCodeAndProductCode(any(), any(), any())).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_CODE_BY_UPC_CODE_PRODUCT_CODE).contentType(
                    MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productItemUpcCodesSkuCodesRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME).param("productCode", PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(productItemService).getItemCodeByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void getItemCodeByUpcCodeAndProductCodeEmptyResponseTest() throws Exception {
    when(productItemService.getItemCodeByUPCCodeAndProductCode(any(), any(), any())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ITEM_CODE_BY_UPC_CODE_PRODUCT_CODE).contentType(
                    MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(productItemUpcCodesSkuCodesRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME).param("upcCode", UPC_CODE_1)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productItemService).getItemCodeByUPCCodeAndProductCode(any(), any(), any());
  }

  @Test
  public void updateProductMasterDataAndImagesAndUpcCodeTest() throws Exception {
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
        .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    editProductDetailRequest.setProductImageEditRequests(new ArrayList<>());
    editProductDetailRequest.setResetExtractedAttributeValue(true);
    editProductDetailRequest.setProductRequest(productRequest);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(editProductDetailRequest);
    Mockito.when(productServiceWrapper.updateProductMasterDataAndImagesAndUpcCode(any(Product.class),
            any(EditProductDetailRequest.class), anyString(), anyString(), Mockito.anyList()))
        .thenReturn(new EditProductItemAndImageResponse());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA_IMAGES_UPC_CODE, PRODUCT_CODE).contentType(
                    MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)
                .param("ignoreSalesCategoryPublish", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(this.categoryService, Mockito.times(1)).findByStoreIdAndId(ProductControllerTest.STORE_ID,
        this.category.getId());
    verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
      anyList(), anyBoolean(), anyString());
    verify(this.productServiceWrapper).updateProductMasterDataAndImagesAndUpcCode(any(), any(), anyString(),
        anyString(), anyList());
  }

  @Test
  public void updateProductMasterDataAndImagesAndUpcCode_UpdatedDateUpdatedByExcveptionTest() throws Exception {
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
      .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setUpdatedDate(null);
    productRequest.setUpdatedBy(StringUtils.EMPTY);
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    editProductDetailRequest.setProductImageEditRequests(new ArrayList<>());
    editProductDetailRequest.setResetExtractedAttributeValue(true);
    editProductDetailRequest.setProductRequest(productRequest);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(editProductDetailRequest);
    Mockito.when(
        productServiceWrapper.updateProductMasterDataAndImagesAndUpcCode(any(Product.class),
          any(EditProductDetailRequest.class), anyString(), anyString(),
          Mockito.anyList()))
      .thenReturn(new EditProductItemAndImageResponse());
    Assertions.assertThrows(Exception.class, () -> this.mockMvc
      .perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA_IMAGES_UPC_CODE, PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON)
          .content(requestString).accept(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME)
          .param("ignoreSalesCategoryPublish", Boolean.FALSE.toString())).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn());
  }

  @Test
  public void updateProductMasterDataAndImagesAndUpcCode_applicationRuntimeExceptionTest() throws Exception {
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    product.setDescription(new byte[] {});
    productRequest.getProductCategories().get(0).getCategory()
      .setCategoryCode(product.getProductCategories().get(0).getCategory().getCategoryCode());
    productRequest.setName(product.getName());
    productRequest.setBrand(product.getBrand());
    productRequest.setUniqueSellingPoint(product.getUniqueSellingPoint());
    productRequest.setDescription(product.getDescription());
    productRequest.setUpdatedBy(ProductControllerTest.UPDATED_BY);
    productRequest.setUpdatedDate(new Date());
    productRequest.setId(ProductControllerTest.ID);
    productRequest.setPristineCategory(true);
    editProductDetailRequest.setProductImageEditRequests(new ArrayList<>());
    editProductDetailRequest.setResetExtractedAttributeValue(true);
    editProductDetailRequest.setProductRequest(productRequest);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(editProductDetailRequest);
    doThrow(ApplicationRuntimeException.class).when(productServiceWrapper)
      .updateProductMasterDataAndImagesAndUpcCode(any(), any(), anyString(), anyString(), anyList());
    try {
      this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA_IMAGES_UPC_CODE,
          PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.DEFAULT_USERNAME).param("ignoreSalesCategoryPublish", Boolean.FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId",
          equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    } finally {
      verify(this.categoryService).findByStoreIdAndId(ProductControllerTest.STORE_ID, this.category.getId());
      verify(attributeService).getAttributeAndValueMap(anyString(),anySet(), anySet(), anySet(),
        anyList(), anyBoolean(), anyString());
      verify(this.productServiceWrapper).updateProductMasterDataAndImagesAndUpcCode(any(), any(),
        anyString(), anyString(), anyList());
    }
  }

  @Test
  public void updateProductBrandDataTest() throws Exception {
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandCode(NEW_BRAND_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    ProductBrandUpdateResponseDTO responseDTO = new ProductBrandUpdateResponseDTO();
    responseDTO.setProductCode(PRODUCT_CODE);
    responseDTO.setBrandCode(NEW_BRAND_CODE);
    responseDTO.setBrandName(BRAND_NAME);
    when(productServiceWrapper.updateProductBrandData(anyString(), any(ProductBrandUpdateDTO.class))).thenReturn(
        responseDTO);

    this.mockMvc.perform(post(StringUtils.join(ProductApiPath.BASE_PATH, ProductApiPath.UPDATE_BRAND_DATA)).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();

    verify(productServiceWrapper).updateProductBrandData(eq(STORE_ID), productBrandUpdateDTOArgumentCaptor.capture());
    assertEquals(PRODUCT_CODE, productBrandUpdateDTOArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void updateProductBrandDataTestWhenProductCodeNull() throws Exception {
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    request.setProductCode(null);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandCode(NEW_BRAND_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);


    this.mockMvc.perform(post(StringUtils.join(ProductApiPath.BASE_PATH, ProductApiPath.UPDATE_BRAND_DATA)).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductBrandDataExceptionTestWhenMissingBrandCodes() throws Exception {
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);


    this.mockMvc.perform(post(StringUtils.join(ProductApiPath.BASE_PATH, ProductApiPath.UPDATE_BRAND_DATA)).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductBrandDataTestWhenOldAndNewBrandCodesSame() throws Exception {
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandCode(BRAND_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    this.mockMvc.perform(post(StringUtils.join(ProductApiPath.BASE_PATH, ProductApiPath.UPDATE_BRAND_DATA)).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
  }

  @Test
  public void updateProductBrandDataExceptionTest() throws Exception {
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandCode(NEW_BRAND_CODE);
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);

    when(productServiceWrapper.updateProductBrandData(anyString(), any(ProductBrandUpdateDTO.class))).thenThrow(
        NullPointerException.class);

    this.mockMvc.perform(post(StringUtils.join(ProductApiPath.BASE_PATH, ProductApiPath.UPDATE_BRAND_DATA)).contentType(
                MediaType.APPLICATION_JSON).content(requestString).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    verify(productServiceWrapper).updateProductBrandData(anyString(), any(ProductBrandUpdateDTO.class));
  }

  @Test
  public void migrateProductWithAttributesValueNullTest() throws Exception {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(null);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    productAttributes.get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productRequest.setProductAttributes(productAttributes);
    productRequest.getProductItems().get(0).getProductItemAttributeValues().add(null);
    this.jsonReq = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(this.productRequest);
    Mockito.when(predefinedAllowedAttributeValueService
            .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
                any(Attribute.class), anyString()))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Mockito.when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), anyString())).thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Mockito.when(attributeService
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString())).thenReturn(null);
    List<Attribute> attributeList = new ArrayList<>();
    attributeList.add(new Attribute());
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(anyString(), anyList()))
        .thenReturn(attributeList);
    Mockito.when(attributeService
            .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), anyString()))
        .thenReturn(new Attribute());
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.MIGRATE_PRODUCT).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonReq).accept(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("newProductCode", ProductControllerTest.NEW_PRODUCT_CODE)
                .param("createdMerchant", ProductControllerTest.CREATED_MERCHANT)
                .param("username", ProductControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.categoryService, Mockito.times(1))
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.any(),Mockito.any());
    verify(this.attributeService, times(3))
        .findDetailByStoreIdAndAttributeCode(eq(ProductControllerTest.STORE_ID), any());
    verify(this.attributeService).findDetailByStoreIdAndAttributeCodeList(anyString(), anyList());
    verify(this.predefinedAllowedAttributeValueService, times(3))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(Constants.DEFAULT_STORE_ID),
            any(Attribute.class), any());
    verify(this.attributeService, times(4))
        .findAttributeByCodeOrNameAndValue(anyString(), anyString(), anyString(),
            anyString(), anyString());
  }

  @Test
  public void mapDsAttributesToProductTest() throws Exception {
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_SUITABILITY_ATTRIBUTE_MAPPING).contentType(
                    MediaType.APPLICATION_JSON)
                .content(ProductControllerTest.OBJECT_MAPPER.writeValueAsString(new ProductSuitabilityEventModel()))
                .accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper).validateAndProcessProductDsAttributeMapping(any());
  }

  @Test
  public void testGetBasicInfoProductDetailsListByProductCodes_Success() throws Exception {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    List<BasicInfoProductResponse> expectedResponse =
        Arrays.asList(new BasicInfoProductResponse(), new BasicInfoProductResponse());
    when(service.getProductBasicInfoByProductCodes(anyString(), anyList())).thenReturn(expectedResponse);
    mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_BASIC_INFO_DETAILS_BY_PRODUCT_CODES).contentType(
                    MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(productCodeList))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.content", hasSize(2)));
    verify(service).getProductBasicInfoByProductCodes(anyString(), anyList());
  }

  @Test
  public void testGetBasicInfoProductDetailsListByProductCodes_EmptyProductCodeList() throws Exception {
    List<String> productCodeList = Collections.emptyList();
    mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_BASIC_INFO_DETAILS_BY_PRODUCT_CODES).contentType(
                    MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(productCodeList))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void testGetBasicInfoProductDetailsListByProductCodes_ServiceException() throws Exception {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    when(service.getProductBasicInfoByProductCodes(anyString(), anyList())).thenThrow(
        new RuntimeException("Service error"));
    mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_BASIC_INFO_DETAILS_BY_PRODUCT_CODES).contentType(
                    MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(productCodeList))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(service).getProductBasicInfoByProductCodes(anyString(), anyList());
  }

  @Test
  public void updateMasterDataTest_Success() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    
    // Add common images to trigger updateCommonImages call
    List<ProductImageEditRequest> commonImages = new ArrayList<>();
    ProductImageEditRequest imageRequest = new ProductImageEditRequest();
    imageRequest.setImagePath("test/image.jpg");
    commonImages.add(imageRequest);
    request.setCommonImages(commonImages);
    
    Map<String, Map<String, String>> productImagesErrorMap = new HashMap<>();
    productImagesErrorMap.put("image1", new HashMap<>());
    
    doNothing().when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), anyString(), any(ProductMasterDataUpdateRequest.class));
    when(productServiceWrapper.updateCommonImagesAndPublishHistory(anyString(), anyString(), any()))
        .thenReturn(productImagesErrorMap);
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.value.productImagesErrorMap", notNullValue()));
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), eq(DEFAULT_USERNAME), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper).updateCommonImagesAndPublishHistory(eq(STORE_ID), anyString(), any());
  }

  @Test
  public void updateMasterDataTest_ValidationException() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    
    ValidationException exception = new ValidationException("Validation failed", ErrorMessage.INVALID_SIZE_CHART_CODE.name());
    doThrow(exception).when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), anyString(), any(ProductMasterDataUpdateRequest.class));
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo("Validation failed")))
        .andExpect(jsonPath("$.value.errorMessage", equalTo(ErrorMessage.INVALID_SIZE_CHART_CODE.name())));
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), eq(DEFAULT_USERNAME), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper, times(0)).updateCommonImages(anyString(), anyList());
  }

  @Test
  public void updateMasterDataTest_GenericException() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    
    RuntimeException exception = new RuntimeException("Generic error");
    doThrow(exception).when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), anyString(), any(ProductMasterDataUpdateRequest.class));
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.errorMessage", equalTo("Generic error")))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), eq(DEFAULT_USERNAME), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper, times(0)).updateCommonImages(anyString(), anyList());
  }

  @Test
  public void updateMasterDataTest_WithoutCommonImages_Success() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    request.setCommonImages(null); // No common images
    
    doNothing().when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), anyString(), any(ProductMasterDataUpdateRequest.class));
    // No need to mock updateCommonImages as it won't be called
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.value.productImagesErrorMap").doesNotExist());
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), eq(DEFAULT_USERNAME), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper, times(0)).updateCommonImages(anyString(), any()); // Should not be called
  }

  @Test
  public void updateMasterDataTest_WithoutUsername_Success() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    // No common images set, so updateCommonImages won't be called
    
    doNothing().when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), any(), any(ProductMasterDataUpdateRequest.class));
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), isNull(), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper, times(0)).updateCommonImages(anyString(), anyList()); // Should not be called
  }

  @Test
  public void updateMasterDataTest_WithNullDescription_Success() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku(PRODUCT_CODE);
    request.setName("Updated Product Name");
    request.setDescription(null); // Null description to test handling
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    
    doNothing().when(productServiceWrapper).updateMasterDataAndEvictCache(anyString(), anyString(), any(ProductMasterDataUpdateRequest.class));
    
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    
    // When & Then
    this.mockMvc.perform(
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_MASTER_DATA)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    
    // Verify
    verify(productServiceWrapper).updateMasterDataAndEvictCache(eq(STORE_ID), eq(DEFAULT_USERNAME), any(ProductMasterDataUpdateRequest.class));
    verify(productServiceWrapper, times(0)).updateCommonImages(anyString(), anyList()); // Should not be called since no common images
  }

  @Test
  public void getProductMasterDataByProductCode_Success() throws Exception {
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(PRODUCT_CODE_LISTS);
    when(this.productServiceWrapper.getProductMasterDataByProductCode(STORE_ID, request)).thenReturn(new ArrayList<>());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_MASTER_DATA_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestString)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productServiceWrapper).getProductMasterDataByProductCode(eq(STORE_ID), any(ProductCodesRequest.class));
  }

  @Test
  public void getProductMasterDataByProductCode_EmptyRequest() throws Exception {
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(new ArrayList<>());
    String requestString = ProductControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(
              post(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_MASTER_DATA_BY_PRODUCT_CODE)
                  .contentType(MediaType.APPLICATION_JSON)
                  .content(requestString)
                  .accept(MediaType.APPLICATION_JSON)
                  .param("storeId", ProductControllerTest.STORE_ID)
                  .param("channelId", ProductControllerTest.CHANNEL_ID)
                  .param("clientId", ProductControllerTest.CLIENT_ID)
                  .param("requestId", ProductControllerTest.REQUEST_ID)
                  .param("username", ProductControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.PRODUCT_CODE_LIST_MUST_NOT_BE_EMPTY.getMessage()));
    }
  }

  @Test
  public void updateBrandDataForProductTest() throws Exception {
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName("New Brand Name");
    request.setNewBrandCode("BRD-12345");
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(false);
    request.setBusinessPartnerCodes(Set.of("BP001", "BP002"));

    String requestJson = OBJECT_MAPPER.writeValueAsString(request);

    when(productServiceWrapper.updateProductBrandDataWithBrandInfo(STORE_ID, request)).thenReturn(
        new ProductBrandUpdateResponse());

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATED_BRAND)
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestJson)
                .param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID)
                .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    verify(productServiceWrapper).updateProductBrandDataWithBrandInfo(STORE_ID, request);
  }

  @Test
  public void updateBrandDataForProductWithApplicationRuntimeExceptionTest() throws Exception {
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName("New Brand Name");
    request.setNewBrandCode("BRD-12345");
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(false);
    request.setBusinessPartnerCodes(Set.of("BP001", "BP002"));

    String requestJson = OBJECT_MAPPER.writeValueAsString(request);

    ApplicationRuntimeException exception = new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Brand not found");
    doThrow(exception).when(productServiceWrapper).updateProductBrandDataWithBrandInfo(STORE_ID, request);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATED_BRAND)
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestJson)
                .param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID)
                .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.errorMessage", equalTo("Can not process invalid input data :Brand not found")))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.VALIDATION.getCode())));

    verify(productServiceWrapper).updateProductBrandDataWithBrandInfo(STORE_ID, request);
  }

  @Test
  public void updateBrandDataForProductWithGenericExceptionTest() throws Exception {
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName("New Brand Name");
    request.setNewBrandCode("BRD-12345");
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(false);
    request.setBusinessPartnerCodes(Set.of("BP001", "BP002"));

    String requestJson = OBJECT_MAPPER.writeValueAsString(request);

    Exception exception = new RuntimeException("Unexpected error");
    doThrow(exception).when(productServiceWrapper).updateProductBrandDataWithBrandInfo(STORE_ID, request);

    this.mockMvc
        .perform(
            post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATED_BRAND)
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestJson)
                .param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID)
                .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));

    verify(productServiceWrapper).updateProductBrandDataWithBrandInfo(STORE_ID, request);
  }
}
